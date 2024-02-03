
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!


! Used to calculate biogenic emissions. First available in CMAQ 5.4

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE MEGAN_DEFN

C-----------------------------------------------------------------------
C Function: biogenics emissions interface to the chemistry-transport model
      
      USE RUNTIME_VARS
      USE MEGAN_FX
      IMPLICIT NONE

      REAL,    ALLOCATABLE, SAVE :: VDEMIS_ME( :,:,: ) ! megan emis

      CONTAINS

C=======================================================================
         FUNCTION MEGAN_INIT( JDATE, JTIME, TSTEP ) RESULT ( SUCCESS )

         USE HGRD_DEFN
         USE CGRID_SPCS          ! CGRID mechanism species
         USE BIOG_EMIS           ! from beis
         USE centralized_io_module, only : soilinp_setup
         USE MEGAN_GSPRO
#ifdef mpas
         USE utilio_defn
#endif

         IMPLICIT NONE

         INTEGER JDATE, JTIME, TSTEP
         LOGICAL SUCCESS

         CHARACTER( 16 )       :: PNAME = 'MEGAN_INIT'
         CHARACTER( 80 )       :: VARDESC   ! env variable description
         CHARACTER( 120 )      :: XMSG = ' '            
         INTEGER V, K, STATUS

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

C In-line biogenic emissions?
         CALL LOG_SUBHEADING( LOGDEV, 'Initialize Online Biogenic VOC Emissions Module ' )

C biogenics to gas-phase species map
         IF ( BIOGEMIS_MEGAN ) THEN

            CALL MEGAN_MAP

            XMSG = 'Using in-line biogenic emissions option (MEGAN)'

            DESID_EMVAR( IMIOGSRM )%LEN = NMGNSPC
            ALLOCATE( DESID_EMVAR( IMIOGSRM )%ARRY ( NMGNSPC ) )
            ALLOCATE( DESID_EMVAR( IMIOGSRM )%UNITS( NMGNSPC ) )
            ALLOCATE( DESID_EMVAR( IMIOGSRM )%MW   ( NMGNSPC ) )
            ALLOCATE( DESID_EMVAR( IMIOGSRM )%USED ( NMGNSPC ) )
            ALLOCATE( DESID_EMVAR( IMIOGSRM )%CONV ( NMGNSPC ) )
            ALLOCATE( DESID_EMVAR( IMIOGSRM )%BASIS( NMGNSPC ) )
            ALLOCATE( DESID_EMVAR( IMIOGSRM )%LAREA( NMGNSPC ) )
            ALLOCATE( DESID_EMVAR( IMIOGSRM )%LAREAADJ( NMGNSPC ) )
            
            DESID_EMVAR( IMIOGSRM )%ARRY  = MEGAN_NAMES
            DESID_EMVAR( IMIOGSRM )%UNITS = 'MOLES/S'
            DESID_EMVAR( IMIOGSRM )%MW    = 1.0
            DESID_EMVAR( IMIOGSRM )%USED  = .FALSE.
            DESID_EMVAR( IMIOGSRM )%CONV  = 1.0
            DESID_EMVAR( IMIOGSRM )%BASIS = 'UNIT' 
            DESID_EMVAR( IMIOGSRM )%LAREA = .FALSE.
            DESID_EMVAR( IMIOGSRM )%LAREAADJ = .FALSE.

            ALLOCATE( VDEMIS_ME( NMGNSPC,NCOLS,NROWS ), STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
              XMSG = 'VDEMIS_ME memory allocation failed'
              SUCCESS = .FALSE.; RETURN
            END IF

#ifdef mpas
            call mio_setfile ('GR_EMIS_001')

             call mio_fcreate ('MEGAN_OUTPUT',512 ) ! 512 means clobber+netcdf64
          if (BDSNP_MEGAN) then
             call mio_fcreate ('BDSNPOUT',512 ) ! 512 means clobber+netcdf64
             call mio_fcreate ('BDSNP_DIAG',512 ) ! 512 means clobber+netcdf64
          end if
#endif
         END IF

         SUCCESS = .TRUE.; RETURN

         END FUNCTION MEGAN_INIT
C=======================================================================

         SUBROUTINE GET_MEGAN ( JDATE, JTIME, TSTEP, L_DESID_DIAG )
       
         USE centralized_io_module, only : ldf,ctf,t24y,sw24y,lai_m,lai_y
         USE centralized_io_util_module
         USE ASX_DATA_MOD 
         USE hgrd_defn, only : ncols, nrows
         USE RUNTIME_VARS, only : NEW_START,IGNORE_SOILINP,logdev
         USE UTILIO_DEFN
         USE MEGAN_HRNO_MOD
         USE DESID_VARS, ONLY : DESID_N_ISTR, IMIOGSRM,
     &                         MAP_ISTRtoEMVAR, MAP_ISTRtoDIFF
         USE GRID_CONF, ONLY: GDTYP_GD, XCELL_GD, YCELL_GD, YORIG_GD, GL_NROWS
         USE MEGAN_GSPRO

         IMPLICIT NONE

         LOGICAL, INTENT( IN ) :: L_DESID_DIAG

         CHARACTER (20) :: TIME_STAMP
         INTEGER JDATE, JTIME, TSTEP( 3 ), ISTR, io_mode, LAYERS
         INTEGER I, J,MXLAI, LAIp_I,LAIc_i
         REAL JDAY, JYEAR, ZTIME
         REAL            :: CURRHR                          ! current GMT hour

         REAL shadeleafTK(ncols,nrows,5)
         REAL sunleafTK(ncols,nrows,5) 
         REAL sunfrac(ncols,nrows,5)
         REAL sunppfd(ncols,nrows,5)
         REAL shadeppfd(ncols,nrows,5)

         REAL er(ncols,nrows)  
         REAL non_dimgarma(19,ncols,nrows)
         REAL out_to_cmaq(NMGNSPC,ncols,nrows) ! 
         REAL CFNO(ncols,nrows)         ! emission activity for crop
         REAL CFNOG(ncols,nrows)        ! "     " for grass
         REAL GAMSM(ncols,nrows)        ! soil moisture activity for isop
         REAL GAMNO(ncols,nrows)        ! final NO emission activity
         REAL BDSNP_NO_out(ncols,nrows) ! final NO emission activity from BDSNP

         REAL PRECADJ(ncols,nrows)
         REAL LAIP ( NCOLS, NROWS )    ! previous LAI 
         REAL LAIC ( NCOLS, NROWS )    ! current LAI 

         CHARACTER( 16 ) :: PNAME = 'MEGAN_DEFN'
         CHARACTER( 120 )      :: XMSG = ' '

         INTEGER, SAVE :: MGN_IHR       ! current simulation hour
         INTEGER          MGN_NDX       ! RAINFALL array timestep index
!        Calculate cell area. 
!        Could be removed in favor of DESID changes
         REAL  DX1, DX2                            ! grid-cell width and length
         REAL, ALLOCATABLE :: LOC_CELL_AREA(:,:)   ! grid-cell area [m2]
         REAL  :: LOC_MSFX2(NCOLS,NROWS)
         INTEGER :: STAT

         if (.not. allocated(loc_cell_area)) then
            allocate (loc_cell_area(ncols, nrows), stat=stat)
         end if

#ifdef mpas 
         loc_msfx2 = 1.0
         loc_cell_area = cell_area
         !output_step   = time2sec(tstep(1))
         !half_syn_step = time2sec(tstep(2)) / 2
         JDAY   = FLOAT( MOD( JDATE, 1000 ) )
         JYEAR  = FLOAT( JDATE / 1000  )       
         CURRHR = REAL ( TIME2SEC( JTIME ) ) / 3600.0
#else
         JDAY   = FLOAT( MOD( JDATE, 1000 ) )
         JYEAR  = FLOAT( JDATE / 1000  )       
         CURRHR = REAL ( TIME2SEC( JTIME ) ) / 3600.0

         ! *** Get length and width of each grid cell
         IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
            DX1 = DG2M * XCELL_GD ! in m
            DX2 = DG2M * YCELL_GD
     &          * COS( PI180*( YORIG_GD + YCELL_GD
     &          * FLOAT( GL_NROWS/2 ) ) ) ! in m
         ELSE
            DX1 = XCELL_GD        ! in m
            DX2 = YCELL_GD        ! in m
         END IF

         LOC_CELL_AREA = DX1 * DX2
         loc_msfx2 = msfx2
#endif
         non_dimgarma = 0.
         LAYERS = 5  ! canopy layers


         ! This option is only included for users who wish to confirm
         ! that their results match with offline MEGAN 3.2
         IF (USE_MEGAN_LAI) THEN ! Read from the preprocessed file
           MXLAI = 46 ! 8-daily
                      ! hard coded to avoid excessive environmental
                      ! variables. MEGAN3.2 offers monthly and 10-day options.
           CALL FINDLAI( JDATE, MXLAI, MXLAI, LAIp_I, LAIc_I)
           laip(:,:) = lai_m(:,:,laip_i)
           laic(:,:) = lai_m(:,:,laic_i)
         ELSE ! Take daily values from WRF/MCI
           laip = LAI_Y
           laic = Met_data%LAI 
         END IF

! Canopy light calculation
         CALL MEGCANOPY( JYEAR, LAYERS, JDAY,CURRHR,    
     &            grid_data%lat, grid_data%lon, laic,          
     &            met_data%TEMP2, met_data%rgrnd, met_data%wspd10,
     &            met_data%prsfc, met_data%Q2, ctf(1:6,:,:),
     &            ShadeleafTK, SunleafTK, SunFrac, SunPPFD, ShadePPFD)

! MEGAN_HRNO is here so we have rainfall/pulse/PRECADJ, temp and radiation 
! for prev 24 hrs. Also used to get save daily LAI.
          precadj=0. 
          CALL MEGAN_HRNO( JDATE, JTIME, TSTEP, L_DESID_DIAG,PRECADJ)

! SOIL NITROGEN CALCULATION
! We call MEGVSA for GAMSM if using BDSNP or 
! turning on drought stress. Used for
! GAMNO if using MEGAN YL95 for soil NO
          CFNO = 0 
          CFNOG = 0 
          GAMNO = 0
          bdsnp_no_out = 0. 

          CALL MEGVSA (JDATE,JTIME,TSTEP,JYEAR,JDAY,
     &    L_DESID_DIAG,Grid_Data%SLTYP, 
     &    ctf(1:6,:,:),laic, grid_data%lat, 
     &    met_data%TEMP2,Met_Data%SOIM1, Met_Data%SOIM2,
     &    Met_Data%SOIT1,PRECADJ,
     &    CFNO, CFNOG, GAMSM, GAMNO,BDSNP_NO_out)

         IF ( NEW_START .or. IGNORE_SOILINP) THEN
          t24y  = met_data%temp2 ! use instantaneous values if no 24h avg
          sw24y = met_data%rgrnd
          laip  = laic  ! no veg growth for first day of simulation
         END IF

! Emission activity calculation
         CALL MEGVEA( LAYERS, JDATE, CURRHR, 
     &   laip,laic,ldf,
     &   GAMSM, met_data%TEMP2,met_data%TEMP2,
     &   met_data%wspd10, 
     &   t24y, sw24y, sunleaftk, shadeleaftk, 
     &   sunfrac, sunppfd, shadeppfd,ER, NON_DIMGARMA )

! Speciate for mechanism
        if(bdsnp_megan) then
          CALL convert2mech(bdsnp_no_out,non_dimgarma,out_to_cmaq)
        else
          CALL convert2mech(gamno,non_dimgarma,out_to_cmaq)
        end if
! Units for use by EMIS_DEFN.F via vdemis_me: [moles/s/m2]
         do i=1,NMGNSPC
           vdemis_me(i,:,:) = out_to_cmaq(i,:,:)*loc_cell_area/loc_msfx2
         end do

#ifdef mpas
             call mio_time_format_conversion (jdate, jtime, time_stamp)
           IF (L_DESID_DIAG) THEN
                do i=1,NMGNSPC
                   call mio_fwrite('MEGAN_OUTPUT',trim(MEGAN_NAMES(i)), pname,
     &                             out_to_cmaq(i,:,1),time_stamp)
                end do
             END IF
#endif

         RETURN
         END SUBROUTINE GET_MEGAN

      END MODULE MEGAN_DEFN

