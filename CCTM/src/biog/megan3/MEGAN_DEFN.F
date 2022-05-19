
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

            EM_FILE_SURR( IMIOGSRM )%LEN = NMGNSPC
            ALLOCATE( EM_FILE_SURR( IMIOGSRM )%ARRY ( NMGNSPC ) )
            ALLOCATE( EM_FILE_SURR( IMIOGSRM )%UNITS( NMGNSPC ) )
            ALLOCATE( EM_FILE_SURR( IMIOGSRM )%MW   ( NMGNSPC ) )
            ALLOCATE( EM_FILE_SURR( IMIOGSRM )%USED ( NMGNSPC ) )
            ALLOCATE( EM_FILE_SURR( IMIOGSRM )%CONV ( NMGNSPC ) )
            ALLOCATE( EM_FILE_SURR( IMIOGSRM )%BASIS( NMGNSPC ) )
            ALLOCATE( EM_FILE_SURR( IMIOGSRM )%LAREA( NMGNSPC ) )
            ALLOCATE( EM_FILE_SURR( IMIOGSRM )%LAREAADJ( NMGNSPC ) ) 

            EM_FILE_SURR( IMIOGSRM )%ARRY  = MEGAN_NAMES
            EM_FILE_SURR( IMIOGSRM )%UNITS = 'MOLES/S'
            EM_FILE_SURR( IMIOGSRM )%MW    = 1.0
            EM_FILE_SURR( IMIOGSRM )%USED  = .FALSE.
            EM_FILE_SURR( IMIOGSRM )%CONV  = 1.0
            EM_FILE_SURR( IMIOGSRM )%BASIS = 'UNIT' ! not MASS, right?
            EM_FILE_SURR( IMIOGSRM )%LAREA = .false. ! not MASS, right?
            EM_FILE_SURR( IMIOGSRM )%LAREAADJ = .false. ! not MASS, right?


            ALLOCATE( VDEMIS_ME( NMGNSPC,NCOLS,NROWS ), STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
              XMSG = 'VDEMIS_ME memory allocation failed'
              SUCCESS = .FALSE.; RETURN
            END IF
#ifdef mpas
          call soilinp_setup
#endif

         END IF

         SUCCESS = .TRUE.; RETURN

         END FUNCTION MEGAN_INIT
C=======================================================================

         SUBROUTINE GET_MEGAN ( JDATE, JTIME, TSTEP )
       
         USE centralized_io_module, only : ldf,ctf,t24y,sw24y,lai_m,lai_y
         USE centralized_io_util_module
         USE ASX_DATA_MOD 
         USE hgrd_defn, only : ncols, nrows
         USE RUNTIME_VARS, only : NEW_START,IGNORE_SOILINP,logdev
         USE UTILIO_DEFN
         USE MEGAN_HRNO_MOD
         USE EMIS_VARS, ONLY : N_EMIS_ISTR, IMIOGSRM,
     &                         MAP_EMtoSURR, MAP_EMtoDIFF
         USE GRID_CONF, ONLY: GDTYP_GD, XCELL_GD, YCELL_GD, YORIG_GD, GL_NROWS
!         USE BDSNP_MOD
         USE MEGAN_GSPRO

         IMPLICIT NONE

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
         REAL non_dimgarma(20,ncols,nrows)
         REAL out_to_cmaq(NMGNSPC,ncols,nrows) ! 

         REAL CFNO(ncols,nrows) ! emission activity for crop
         REAL CFNOG(ncols,nrows)! "     " for grass
         REAL GAMSM(ncols,nrows)! soil moisture activity for isop
         REAL GAMNO(ncols,nrows)! final NO emission activity
         REAL PRECADJ(ncols,nrows)
         REAL LAIP ( NCOLS, NROWS )    ! previous LAI 
         REAL LAIC ( NCOLS, NROWS )    ! current LAI 

         CHARACTER( 16 ) :: PNAME = 'MEGAN_DEFN'
         CHARACTER( 120 )      :: XMSG = ' '

         INTEGER, SAVE :: MGN_IHR       ! current simulation hour
         INTEGER          MGN_NDX       ! RAINFALL array timestep index
#ifndef mpas 
!        Calculate cell area. This is not ideal but is how it's done elsewhere.
         REAL  DX1, DX2                            ! grid-cell width and length
         REAL  CELL_AREA                            ! grid-cell area [m2]

         JDAY   = FLOAT( MOD( JDATE, 1000 ) )
         JYEAR  = FLOAT( JDATE / 1000  )       
         CURRHR = REAL ( TIME2SEC( JTIME ) ) / 3600.0
         LAYERS = 5  ! canopy layers

! *** Get length and width of each grid cell
!     note: crude estimate is made for LAT/LONG coordinate systems
         IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
            DX1 = DG2M * XCELL_GD ! in m
            DX2 = DG2M * YCELL_GD
     &          * COS( PI180*( YORIG_GD + YCELL_GD
     &          * FLOAT( GL_NROWS/2 ) ) ) ! in m
         ELSE
            DX1 = XCELL_GD        ! in m
            DX2 = YCELL_GD        ! in m
         END IF

         CELL_AREA = DX1 * DX2
#endif

         IF (USE_MEGAN_LAI) THEN ! Read from the preprocessed file
           MXLAI = 46 ! 8-daily
           CALL FINDLAI( JDATE, MXLAI, LAIp_I, LAIc_I)
           laip(:,:) = lai_m(:,:,laip_i)
           laic(:,:) = lai_m(:,:,laic_i)
         ELSE ! Use same values as WRF/MCIP
           laip = LAI_Y
           laic = Met_data%LAI 
         END IF

! Canopy light calculation
         CALL MEGCANOPY( NCOLS, NROWS, JYEAR, LAYERS, JDAY,CURRHR,    
     &            grid_data%lat, grid_data%lon, laic,          
     &            met_data%TEMP2, met_data%rgrnd*0.45, met_data%wspd10,
     &            met_data%prsfc, met_data%Q2, ctf(1:6,:,:),
     &            ShadeleafTK, SunleafTK, SunFrac, SunPPFD, ShadePPFD)

! MEGAN_HRNO is here so we have rainfall/pulse/PRECADJ, and SW for prev 24 hrs. Also LAI.
          precadj=0 
          CALL MEGAN_HRNO( JDATE, JTIME, TSTEP, PRECADJ)


! SOIL NITROGEN CALCULATION
! Need MEGVSA for GAMSM if using BDSNP or 
! turning on wilting.
! Need GAMNO if using MEGAN YL95 for soil NO
          CFNO = 0 
          CFNOG = 0 
          GAMNO = 0 

          CALL MEGVSA (JDATE,JTIME,TSTEP,NCOLS,NROWS,Grid_Data%SLTYP, 
     &    ctf(1:6,:,:),laic, grid_data%lat, 
     &    met_data%TEMP2,Met_Data%SOIM1, Met_Data%SOIM2,
     &    Met_Data%SOIT1,PRECADJ,
     &    CFNO, CFNOG, GAMSM, GAMNO )

         IF ( NEW_START .or. IGNORE_SOILINP) THEN
          t24y  = met_data%temp2 ! use instantaneous values if no 24h 
          sw24y = met_data%rgrnd
          laip  = laic  ! no growth for first day of simulation
         END IF

! Emission activity calculation
         CALL MEGVEA(NCOLS,NROWS, LAYERS, JDATE, CURRHR, 
     &   laip,laic,ldf,
     &   GAMSM, met_data%TEMP2,met_data%TEMP2,
     &   met_data%wspd10, 
     &   t24y, sw24y, sunleaftk, shadeleaftk, 
     &   sunfrac, sunppfd, shadeppfd,ER, NON_DIMGARMA )


! Speciate for mechanism
         CALL convert2mech(gamno,non_dimgarma,out_to_cmaq)

! Convert units for use by EMIS_DEFN.F via vdemis_me
         vdemis_me(:,:,:) = out_to_cmaq*cell_area


         RETURN
         END SUBROUTINE GET_MEGAN

      END MODULE MEGAN_DEFN

