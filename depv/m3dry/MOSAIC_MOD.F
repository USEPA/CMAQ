
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


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/depv/m3dry/MOSAIC_MOD.F,v 1.3 2011/10/21 16:10:35 yoj Exp $

c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module MOSAIC_MOD
      
C Contains the shared variables and subrountes needed estimated the resistances
C from natural and agricultrual lands for NH3 bidirectional flux
C
C 
C
C Revision History: J. Bash June 16 11: Created


      Implicit None
      
C shared variables 
!      Integer, Save :: n_lufrac
      Real, Save, Allocatable :: hcanj   (:)   ! Canopy height for LU j
      Real, Save, Allocatable :: lai0    (:)   ! Met model LAI for LU j
      Real, Save, Allocatable :: laimn0  (:)   ! Met model min LAI for LU j
      Real, Save, Allocatable :: lufraccr(:)   ! Column Row LU fraction
      Real, Save, Allocatable :: laijcr  (:)   ! Column Row LAI for LU j      
      Real, Save, Allocatable :: rsmin   (:)   ! Min Rs for LU j
      Real, Save, Allocatable :: veg0    (:)   !
      Real, Save, Allocatable :: vegmn0  (:)   !
      Real, Save, Allocatable :: vegjcr  (:)   !
      Real, Save, Allocatable :: z00     (:)   ! Met model z0 for LU j
      Real, Save, Allocatable :: z0jcr   (:)   ! Column Row z0 for LU j      
      Real, Save, Allocatable :: rbcj    (:)   ! Colum Row boundary layer resist for LU j      
      Real, Save, Allocatable :: luf_fac (:)   ! Leaf emission pot. for NH3 bidi

      Real, Save, Allocatable :: rgndcj     ( :,:,: )  ! Rgnd for LU j
      Real, Save, Allocatable :: rincj      ( :,:,: )  ! Rinc for LU j      
      Real, Save, Allocatable :: deltaj     ( :,:,: )  ! Surface wetness for LU j 
      Real, Save, Allocatable :: ustarj     ( :,:,: )  ! u* for LU j  
      Real, Save, Allocatable :: raj        ( :,:,: )  ! aerodynamic resistance for LU j  
      Real, Save, Allocatable :: laij       ( :,:,: )  ! LAI for LU j 
      Real, Save, Allocatable :: vegj       ( :,:,: )  ! veg for LU j 
      Real, Save, Allocatable :: z0j        ( :,:,: )  ! z0 for LU j 
      REAL, SAVE, ALLOCATABLE :: rstomj     ( :,:,:,: )
      REAL, SAVE, ALLOCATABLE :: rcanj      ( :,:,:,: )! stomatal, mesiphyll and cuticular resistances only
      REAL, SAVE, ALLOCATABLE :: rsurfj     ( :,:,:,: )! Rsurf for LU j        
      Real, Save, Allocatable :: depvel_gasj( :,:,:,: ) ! deposition velocity by land use type
      Real, Save, Allocatable :: vd_fst_gasj( :,:,:,: ) ! deposition velocity for stomatal and
      Real, Save, Allocatable :: adepvj     ( :,:,:,: ) ! deposition velocity for stomatal and
      
!      Character( 80 ), Save   :: LAND_SCHEME 
      
         

C Private variables used in this module and subroutines       
      Integer, Save, Private      :: logdev 
      
      Contains
       Subroutine Init_Mosaic(jdate, jtime, ltotg, MOSAIC)
       
       Use HGRD_DEFN
       Use LSM_Mod
       USE UTILIO_DEFN
       
       Implicit None    

C...include files

       INCLUDE SUBST_FILES_ID   ! file name parameters                 
       
       Integer, Intent( In )  :: jdate
       Integer, Intent( In )  :: jtime  
       Integer, Intent( In )  :: ltotg 
       Logical, Intent( In )  :: MOSAIC         
       CHARACTER( 240 )       :: XMSG = ' '
       CHARACTER(  16 ), SAVE :: PNAME = 'Init_Mosaic'
       Integer l
       INTEGER      GXOFF, GYOFF            ! global origin offset from file
       INTEGER      :: STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2
                                                                                   
       
!       LAND_SCHEME = 'UNKNOWN'
!       CALL SUBHFILE ( GRID_CRO_2D, GXOFF, GYOFF,
!     &                 STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2 )
       
!       IF ( .NOT. DESC3( GRID_CRO_2D ) ) THEN
!          XMSG = 'COULD NOT GET GRID_CRO_2D FILE DESCRIPTION '
!          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
!       END IF        
         
!       DO l = 1, NVARS3D ! determine land character
!          IF ( VNAME3D( l ) .EQ. 'DLUSE' ) THEN
!             IF ( INDEX( VDESC3D( l ), 'USGS24' ) .NE. 0 ) LAND_SCHEME = 'USGS24'
!             IF ( INDEX( VDESC3D( l ), 'MODIS' ) .NE. 0 )  LAND_SCHEME = 'MODIS'
C...overwrite if NLCD-MODIS scheme
!             IF ( INDEX( VDESC3D( l ), 'NLCD-MODIS' ) .NE. 0 ) LAND_SCHEME = 'NLCD-MODIS'
!          END IF
!       END DO         
!       Write(Logdev,*) 'Land use scheme is ', TRIM(LAND_SCHEME) 
       
       SELECT CASE( LAND_SCHEME )
         CASE( 'USGS24' )
!            n_lufrac = n_usgs            
            ALLOCATE (  lai0( n_lufrac ),
     &                laimn0( n_lufrac ),
     &                 rsmin( n_lufrac ),
     &                  veg0( n_lufrac ),
     &                vegmn0( n_lufrac ),
     &                   z00( n_lufrac ),
     &               luf_fac( n_lufrac ) )
            rsmin    = RSMIN_USGS
            lai0     = LAI0_USGS
            laimn0   = LAIMN0_USGS
            veg0     = VEG0_USGS
            vegmn0   = VEGMN0_USGS
            z00      = Z00_USGS/100. !convert to meters
            luf_fac  = LUF_FAC_USGS
         CASE( 'MODIS' )
!            n_lufrac = n_modis
            ALLOCATE (  lai0( n_lufrac ),
     &                laimn0( n_lufrac ),
     &                 rsmin( n_lufrac ),
     &                  veg0( n_lufrac ),
     &                vegmn0( n_lufrac ),
     &                   z00( n_lufrac ),
     &               luf_fac( n_lufrac ) )
            rsmin    = RSMIN_MODIS
            lai0     = LAI0_MODIS
            laimn0   = LAIMN0_MODIS
            veg0     = VEG0_MODIS
            vegmn0   = VEGMN0_MODIS
            z00      = Z00_MODIS/100. !convert to meters
            luf_fac  = LUF_FAC_MODIS
         CASE( 'NLCD-MODIS' )
!            n_lufrac = n_nlcd
            ALLOCATE (  lai0( n_lufrac ),
     &                laimn0( n_lufrac ),
     &                 rsmin( n_lufrac ),
     &                  veg0( n_lufrac ),
     &                vegmn0( n_lufrac ),
     &                   z00( n_lufrac ),
     &               luf_fac( n_lufrac ) )
            rsmin    = RSMIN_NLCD
            lai0     = LAI0_NLCD
            laimn0   = LAIMN0_NLCD
            veg0     = VEG0_NLCD
            vegmn0   = VEGMN0_NLCD
            z00      = Z00_NLCD/100. !convert to meters
            luf_fac  = LUF_FAC_NLCD
         CASE DEFAULT
!            Write(Logdev,*) 'Land use scheme is ', LAND_SCHEME
            xmsg = 'Land use scheme not supported'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
       END SELECT                                       
       
       IF ( .NOT. ALLOCATED ( hcanj ) ) THEN
          ALLOCATE ( hcanj( n_lufrac ) )
       END IF
       
       IF ( .NOT. ALLOCATED ( lufraccr ) ) THEN
          ALLOCATE ( lufraccr( n_lufrac ) )
       END IF
       
       IF ( .NOT. ALLOCATED ( laijcr ) ) THEN
          ALLOCATE ( laijcr( n_lufrac ) )
       END IF  
       
        IF ( .NOT. ALLOCATED ( vegjcr ) ) THEN
          ALLOCATE ( vegjcr( n_lufrac ) )
       END IF           
       
       IF ( .NOT. ALLOCATED ( z0jcr ) ) THEN
          ALLOCATE ( z0jcr( n_lufrac ) )
       END IF
       
        IF ( .NOT. ALLOCATED ( rbcj ) ) THEN
          ALLOCATE ( rbcj( n_lufrac ) )
       END IF               
       
       IF ( .NOT. ALLOCATED ( rgndcj ) ) THEN
          ALLOCATE ( rgndcj( n_lufrac,my_ncols,my_nrows ) )
       END IF

       IF ( .NOT. ALLOCATED ( rincj ) ) THEN
          ALLOCATE ( rincj( n_lufrac,my_ncols,my_nrows ) )
       END IF

       IF ( .NOT. ALLOCATED ( rcanj ) ) THEN
          ALLOCATE ( rcanj( n_lufrac,ltotg,my_ncols,my_nrows ) )
       END IF
       
       IF ( .NOT. ALLOCATED ( rstomj ) ) THEN
          ALLOCATE ( rstomj( n_lufrac,ltotg,my_ncols,my_nrows ) )
       END IF
       
       IF ( .NOT. ALLOCATED ( rsurfj ) ) THEN
          ALLOCATE ( rsurfj( n_lufrac,ltotg,my_ncols,my_nrows ) )
       END IF
       
       IF ( .NOT. ALLOCATED ( deltaj ) ) THEN
          ALLOCATE ( deltaj( n_lufrac,my_ncols,my_nrows ) )
       END IF          
       
       IF ( .NOT. ALLOCATED ( ustarj ) ) THEN
          ALLOCATE ( ustarj( n_lufrac,my_ncols,my_nrows ) )
       END IF
       
       IF ( .NOT. ALLOCATED ( raj ) ) THEN
          ALLOCATE ( raj( n_lufrac,my_ncols,my_nrows ) )
       END IF  
       
       IF ( .NOT. ALLOCATED ( laij ) ) THEN
          ALLOCATE ( laij( n_lufrac,my_ncols,my_nrows ) )
       END IF
       
       IF ( .NOT. ALLOCATED ( vegj ) ) THEN
          ALLOCATE ( vegj( n_lufrac,my_ncols,my_nrows ) )
       END IF
       
       IF ( .NOT. ALLOCATED ( z0j ) ) THEN
          ALLOCATE ( z0j( n_lufrac,my_ncols,my_nrows ) )
       END IF  
       
       If ( MOSAIC ) Then
          IF ( .NOT. ALLOCATED ( depvel_gasj ) ) THEN
             ALLOCATE ( depvel_gasj( n_lufrac,ltotg,my_ncols,my_nrows ) )
          END IF 
          IF ( .NOT. ALLOCATED ( vd_fst_gasj ) ) THEN
             ALLOCATE ( vd_fst_gasj( n_lufrac,ltotg,my_ncols,my_nrows ) )
          END IF 
       
       End IF                         
       
       Return   
          
       End Subroutine Init_Mosaic
       
       SUBROUTINE calc_lai( jday, jtime, soit2, lufrac,
     &                     laig, zruf, laicrj, z0crj, vegcrj, logdev, c, r )

C***********************************************************************
C  Function:
C     Calculate the lai for each LUC in the gridcell
 
C  Preconditions:  none
 
C  Subroutines and Functions Called:  none
 
C  Revision History:
C***********************************************************************

      USE LSM_MOD, Only: n_lufrac, Land_Scheme

      IMPLICIT NONE

C Arguments:
      INTEGER, INTENT( IN )   :: jday
      INTEGER, INTENT( IN )   :: jtime     
      REAL,    INTENT( IN )   :: soit2              ! temeperature in 1 m of soil
      REAL,    INTENT( IN )   :: lufrac( n_lufrac ) ! fractional land use
      REAL,    INTENT( IN )   :: laig               ! grid value for LAI
      REAL,    INTENT( IN )   :: zruf               ! grid value for roughness
      REAL,    INTENT( OUT )  :: laicrj( n_lufrac )
      REAL,    INTENT( OUT )  :: vegcrj( n_lufrac )
      REAL,    INTENT( OUT )  :: z0crj ( n_lufrac )
      INTEGER, INTENT( IN )   :: logdev
      INTEGER, INTENT( IN )   :: c, r               ! column, row for reporting errors

C Local variables:
      REAL    :: arg, arg1, argw
      REAL    :: correction
      INTEGER :: idaype
      REAL    :: fseas
      REAL    :: vseas
      INTEGER :: j
      REAL    :: lai_crop
      REAL    :: lai_mos5, lai_mos6, lai_mos45, lai_mos14
      REAL    :: new_sum_nat_lai
      REAL    :: root1
      REAL    :: root2
      REAL    :: sig
      REAL    :: sum_nat_lai
      REAL    :: sum_mos_lai
      REAL    :: sum_crop_frac
      REAL    :: sum_mos_frac
      REAL    :: sum_lai
      REAL    :: total_frac
      REAL    :: znotc


C initialize
      idaype = 0
      lai_crop = 0.0
      root1 = 0.0
      root2 = 0.0
      sum_nat_lai = 0.0
      sum_mos_lai = 0.0
      sum_crop_frac = 0.0
      sum_mos_frac = 0.0
      total_frac = 0.0
      znotc = 0.0

C calculate fseas based on deep soil temperature
      IF ( soit2 .LE. 290.0 ) THEN
         fseas = MAX( 1.0 - 0.015625 * ( 290.0 - soit2 ) ** 2, 0.0)
      ELSE
         fseas = 1.0
      END IF

C get individual LAIs for LUCs for this date - lu_type: 1 = USGS; lu_type: 2 = NLCD
      DO j = 1, n_lufrac
         total_frac = total_frac + lufrac( j )
         vegcrj( j ) = ( ( 1. - fseas ) * vegmn0( j ) + fseas * veg0( j ) )/100.
         IF ( ( Land_Scheme .EQ. 'USGS24' .AND. ( j .LE. 2 .AND. j .LE. 4 ) ) .OR.
     &        ( Land_Scheme .EQ. 'NLCD-MODIS' .AND. ( j .EQ. 20 .OR. j .EQ. 43  ) ) .OR.
     &        ( Land_Scheme .EQ. 'MODIS' .AND. ( j .EQ. 12 ) ) ) THEN
            sum_crop_frac = sum_crop_frac + lufrac( j )
         ELSE IF ( ( Land_Scheme .EQ. 'USGS24' .AND. ( j .EQ. 5 .OR. j .EQ. 6 ) ) .OR.
     &             ( Land_Scheme .EQ. 'NLCD-MODIS' .AND. j .EQ. 45 ).OR.
     &             ( Land_Scheme .EQ. 'MODIS' .AND. ( j .EQ. 14 ) ) ) THEN
            sum_mos_frac = sum_mos_frac + lufrac( j )
            sum_mos_lai = sum_mos_lai + lufrac( j ) * ( laimn0( j )
     &                  + fseas * ( lai0( j ) - laimn0( j ) ) )
         ELSE
            laicrj( j ) = laimn0( j ) + fseas * ( lai0( j ) - laimn0( j ) )
            sum_nat_lai = sum_nat_lai + lufrac( j ) * laicrj( j )
            z0crj( j ) = z00( j )
         END IF
      END DO

C total frac needs to be non-water
      Select Case(Land_Scheme)
         Case('USGS24')
            IF ( lufrac( 16 ) .GT. 0.10 ) THEN
               total_frac = total_frac-lufrac( 16 )
            ELSE
               total_frac = 1.0
            END IF
         Case('NLCD-MODIS')
            argw = lufrac( 1 ) + lufrac( 31 ) + lufrac( 48 ) + lufrac( 50 )
            IF ( argw .GT. 0.10 ) THEN
               total_frac = total_frac-argw
            ELSE
               total_frac = 1.0
            END IF
         Case('MODIS')
            IF ( lufrac( 17 ) .GT. 0.10 ) THEN
               total_frac = total_frac-lufrac( 16 )
            ELSE
               total_frac = 1.0
            END IF
      End Select

      IF ( ( sum_crop_frac + 0.5 * sum_mos_frac ) .LE. 0.0 ) THEN
         lai_crop = 0.5
      ELSE
         arg1 = total_frac * laig - sum_nat_lai - 0.5 * sum_mos_lai

#ifdef Verbose
         IF ( arg1 .LE. 0.0 ) THEN
            WRITE( logdev,* ) ' '
            WRITE( logdev,'(a, 2i4)' ) 'Sum Natural+Mosaic LAI too big at c,r: ', c, r
            WRITE( logdev,* ) 'arg1,total_frac,laig             : ', arg1, total_frac, laig
            WRITE( logdev,* ) 'soit2,fseas                      : ', soit2, fseas
            WRITE( logdev,* ) 'sum_nat_lai,sum_mos_lai          : ', sum_nat_lai, sum_mos_lai
            WRITE( logdev,* ) 'sum_crop_frac, sum_mos_frac      : ', sum_crop_frac, sum_mos_frac
            WRITE( logdev,* ) ' '
         END IF
#endif

         lai_crop = arg1 / ( sum_crop_frac + 0.5 * sum_mos_frac )

#ifdef Verbose
         IF ( lai_crop .GT. 3.16 ) THEN
            WRITE( logdev,* ) ' '
            WRITE( logdev,'(a, 2i4, a, f12.5)' ) 'CROP LAI too big at c,r: ', c, r, ' lai crop = ', lai_crop 
            WRITE( logdev,* ) 'arg1,total_frac,laig             : ', arg1, total_frac, laig
            WRITE( logdev,* ) 'soit2,fseas                      : ', soit2, fseas
            WRITE( logdev,* ) 'sum_nat_lai,sum_mos_lai          : ', sum_nat_lai, sum_mos_lai
            WRITE( logdev,* ) 'sum_crop_frac, sum_mos_frac      : ', sum_crop_frac, sum_mos_frac
            WRITE( logdev,* ) ' '
         END IF
#endif

C check to see if correction needed
         IF ( lai_crop .LT. 0.0 .OR. lai_crop .GT. 3.16 ) THEN   ! method didn't work - numerical error - use approximation based on fseas

#ifdef Verbose
            WRITE( logdev,'(a, 2i4, a, f12.5)' ) 'Numerical error in Calc_LAI at c,r: ', c, r, ' - bad lai_crop = ', lai_crop
#endif
            Select Case(Land_Scheme)
               Case('USGS24')
                  lai_crop = MAX( laimn0( 2 ) + fseas * ( 3.16 - laimn0( 2 ) ), 0.5 )   ! max lai-crop is really 3.16 not 3.0 as in table
               Case('NLCD-MODIS')
                  lai_crop = MAX( laimn0( 20 ) + fseas * ( 3.16 -laimn0( 20 ) ), 0.5 )
               Case('MODIS')
                  lai_crop = MAX( laimn0( 12 ) + fseas * ( 3.16 -laimn0( 12 ) ), 0.5 )
            End Select
            
#ifdef Verbose
            WRITE( logdev,'(47x, a, f12.5)' ) 'New lai_crop = ', lai_crop
#endif

            Select Case(Land_Scheme)
               Case('USGS24')
                  lai_mos5 = 0.5 * ( laimn0( 5 ) + fseas * ( lai0( 5 ) - laimn0( 5 ) ) ) + 0.5 * lai_crop
                  lai_mos6 = 0.5 * ( laimn0( 6 ) + fseas * ( lai0( 6 ) - laimn0( 6 ) ) ) + 0.5 * lai_crop
                  new_sum_nat_lai = total_frac * laig - ( lai_crop * sum_crop_frac + lai_mos5 * lufrac( 5 ) + 
     &                              lai_mos6 * lufrac( 6 ) )
               Case('NLCD-MODIS')
                  lai_mos45 = 0.5 * ( laimn0( 45 ) + fseas * ( lai0( 45 ) - laimn0( 45 ) ) ) + 0.5 * lai_crop
                  new_sum_nat_lai = total_frac * laig - ( lai_crop * sum_crop_frac + lai_mos45* lufrac( 45 ) )
               Case('MODIS')
                  lai_mos14 = 0.5 * ( laimn0( 14 ) + fseas * ( lai0( 14 ) - laimn0( 14 ) ) ) + 0.5 * lai_crop
                  new_sum_nat_lai = total_frac * laig - ( lai_crop * sum_crop_frac + lai_mos14* lufrac( 14 ) )
            End Select
            
            IF ( sum_nat_lai .LT. 0.1 .OR. new_sum_nat_lai .LT. 0.1 ) THEN   ! not worth doing a correction
               correction = 1.0
            ELSE
               correction = new_sum_nat_lai / sum_nat_lai
            END IF

#ifdef Verbose
            Select Case(Land_Scheme)
               Case('USGS24')
                  WRITE( logdev,* ) 'Old lai: ', laicrj( 1 ), laicrj( 7 ), laicrj( 11 ), laicrj( 15 )   
               Case('NLCD-MODIS')
                  WRITE( logdev,* ) 'Old lai: ', laicrj( 1 ), laicrj( 14 ), laicrj( 9 ), laicrj( 11 )
               Case('MODIS')
                  WRITE( logdev,* ) 'Old lai: ', laicrj( 17 ), laicrj( 10 ), laicrj( 4 ), laicrj( 5 )
            End Select           
            WRITE( logdev,* ) 'correction = ', correction
#endif

            Select Case(Land_Scheme)
               Case('USGS24')
                  laijcr( 1 ) = laicrj( 1 ) * correction
                  DO j = 7, 15
                     laijcr( j ) = laicrj( j ) * correction
                  END do
#ifdef Verbose
                  WRITE( logdev,* ) 'New lai: ', laicrj( 1 ), laicrj( 7 ), laicrj( 11 ), laicrj( 15 )
#endif                  
               Case('NLCD-MODIS')
                  DO j = 2, 19
                     laicrj( j ) = laicrj( j ) * correction
                  END DO
                  DO j = 21, 30
                     laicrj( j ) = laicrj( j ) * correction
                  END DO
                  DO j = 32, 42
                     laicrj( j ) = laicrj( j ) * correction
                  END DO
                  laicrj( 44 ) = laicrj( 44 ) * correction
                  laicrj( 46 ) = laicrj( 46 ) * correction
                  laicrj( 47 ) = laicrj( 47 ) * correction
               Case('MODIS')
                  DO j = 1, 11
                     laicrj( j ) = laicrj( j ) * correction
                  END DO
                  laicrj( 13 ) = laicrj( 13 ) * correction
                  DO j = 15, 16
                     laicrj( j ) = laicrj( j ) * correction
                  END DO
                  DO j = 18, 20
                    laicrj( j ) = laicrj( j ) * correction
                  END DO                  
            End Select            
         END IF

      END IF

c estimate vseas
      vseas = (lai_crop-0.5)/2.5
      vseas = min(max(vseas,0.0),1.0)      

C find z0_crop by finding days past emergence
      sig = ( ( lai_crop ) ** ( 1.0 / 1.923 ) ) / 2.273
      arg = 0.0184 * 0.0184 - 4.0 * 1.057e-4 * sig
      root1 = ( 0.0184 + SQRT( arg ) ) / ( 2.0 * 1.057E-4 )
      root2 = ( 0.0184 - SQRT( arg ) ) / ( 2.0 * 1.057E-4 )
C figure out which root to use - PX assumes maturity 87 days after emergence
C which should have been caught by testing lai
      IF ( root1 .GT. 87.0 ) root1 = 0.0
      IF ( root2 .GT. 87.0 ) root2 = 0.0
      idaype = NINT( MAX( root1, root2 ) )
      IF ( idaype .LE. 0 .OR. jday .GT. 288 ) THEN
         znotc = 0.05
      ELSE IF ( idaype .GT. 87 ) THEN
         znotc = 0.15
      ELSE
         znotc = 5.00 + 0.23 * REAL( idaype ) - 1.32E-3 * REAL( idaype * idaype )
         znotc = znotc / 100.0  ! convert to meters
      END IF
      
      Select Case(Land_Scheme)
         Case('USGS24')
            DO j = 2, 4
               laicrj( j ) = lai_crop
               z0crj( j )  = znotc
               vegcrj( j ) = ((1.-vseas)*vegmn0(j) + vseas*veg0(j))/100.
            END DO

            DO j = 5, 6
               laicrj( j ) = 0.5 * ( laimn0( j ) + fseas * ( lai0( j ) - laimn0( j ) ) )
     &                     + 0.5 * lai_crop
               z0crj( j )  = 0.5 * ( znotc + z00( j ) )
               vegcrj( j ) = ((1.-vseas)*vegmn0(j) + vseas*veg0(j))/100.
            END DO
         Case('NLCD-MODIS')
            laicrj( 20 ) = lai_crop
            z0crj ( 20 ) = znotc
            vegcrj( 20 ) = ((1.-vseas)*vegmn0(20) + vseas*veg0(20))/100.
            laicrj( 43 ) = lai_crop
            z0crj ( 43 ) = znotc
            vegcrj( 43 ) = ((1.-vseas)*vegmn0(43) + vseas*veg0(43))/100.
            laicrj( 45 ) = 0.5 * ( laimn0( 45 ) + fseas * ( lai0( 45 ) - laimn0( 45 ) ) )
     &                   + 0.5 * lai_crop
            z0crj ( 45 ) = 0.5 * ( znotc + z00( 45 ) )
            vegcrj( 45 ) = ((1.-vseas)*vegmn0(45) + vseas*veg0(45))/100.
         Case('MODIS')
            laicrj( 12 ) = lai_crop
            z0crj ( 12 ) = znotc
            vegcrj( 12 ) = ((1.-vseas)*vegmn0(12) + vseas*veg0(12))/100.
            laicrj( 14 ) = 0.5 * ( laimn0( 14 ) + fseas * ( lai0( 14 ) - laimn0( 14 ) ) )
     &                   + 0.5 * lai_crop
            z0crj ( 14 ) = 0.5 * ( znotc + z00( 14 ) )
            vegcrj( 12 ) = ((1.-vseas)*vegmn0(12) + vseas*veg0(12))/100.
      End Select      

      sum_lai = SUM( laicrj * lufrac )

#ifdef Verbose
      IF ( ( sum_lai - laig ) / laig .GT. 0.05 ) THEN
         WRITE( logdev,* ) ' '
         WRITE( logdev,* ) 'Error in total LAI'
         WRITE( logdev,* ) 'LAI grid = ', laig, ' LAI mosaic = ', sum_lai
         WRITE( logdev,* ) ' '
      END IF
#endif

      RETURN
      END Subroutine Calc_LAI      

      SUBROUTINE RA_MM5(WS10,US,VS,THETA,THETAV,TG,QVS,QSS,SNOW,PSFPA,LWMASK,WG,WFC,Z,ZNT,UST,RA,rich,psih,betap,hu,dthvdz)

      IMPLICIT NONE

      REAL US,VS,THETA, THETAV, TG, QVS, QSS, SNOW, PSFPA, LWMASK, WG, WFC, Z, ZNT, UST, RA
      REAL BETAH, GAMAH, PR0, BH, RICRIT, CZO, OZO, CP, R, ROVCP, EP1, G, ROVG, KARMAN, PI
      REAL HU, BETAP, QFC, CVIRT, TV0, CPOT, TH0, THETAG, CPAIR
      REAL RICRITI, WS, GZ1OZ0, DTHVDZ, DTHVM, VCONV, WSPD, GOVRTH, RICH, RICUT
      REAL ZOL, PSIH, AH, SQLNZZ0
      REAL PSURF
      REAL WS10
      INTEGER ISNOW, ISTI

C Calculate Ra via the richardson number as done for MM5

      DATA BETAH/8.21/,GAMAH/11.6/,PR0/0.95/
      DATA BH/15.7/

      DATA RICRIT/0.25/
      DATA CZO/0.032/, OZO/0.0001/
      DATA KARMAN/0.4/

      SAVE BETAH,GAMAH, PR0,BH,RICRIT,CZO,OZO,KARMAN

C from param.F
      CP=1004.E0
      R=287.04
      ROVCP=R/CP
      EP1 = 0.608
      G=9.81
      ROVG=R/G
      PI=4.*ATAN(1.)

      ISNOW = NINT(SNOW)
      ISNOW = MAX( 0, ISNOW )

      ISTI  = NINT(LWMASK)
      PSURF = PSFPA/100.  ! CONVERT TO MB

C
C-- compute sfc rh from soil moisture
C*  SATURATED FOR WATER SURFACE AND SNOW (ISNOW=1)
      HU = 1.0
      BETAP = 1.0
C*
C-- FOR SURFACE OVER LAND

C..... beta method, Lee & Pielke (JAM,May1992)
      IF(ISTI .NE. 0 .AND. ISNOW .NE. 1 .AND. WFC .GT. 0. .AND. WG .LE. WFC) THEN
        BETAP = 0.25*(1.0 - COS(WG / WFC * PI))**2
        HU = BETAP+ (1.0 - BETAP) * QVS / QSS
      ENDIF

      QFC = HU * QSS

C.......... compute the thetav at ground
      CVIRT=(1.0 +EP1*QFC)
      TV0 = TG *CVIRT
      CPOT= (1000.0 /PSURF)**ROVCP   ! psurf needs to be in mb
      TH0 = TV0*CPOT
c      THETAG = CPOT * TG
c      CPAIR = 1004.67 * (1.0 + 0.84 * QVS)   ! J/(K KG)

C
C-----CALCULATE BULK RICHARDSON NO. OF SURFACE LAYER, ACCORDING TO
C     AKB(1976), EQ(12).
C
      RICRITI = 1./RICRIT
      GZ1OZ0 = ALOG(Z / ZNT)
      WS = SQRT(US*US + VS*VS)
c      WS=WS10
      DTHVDZ = THETAV - TH0
      DTHVM = AMAX1(-DTHVDZ,0.0)
      VCONV = 2.0 * SQRT(DTHVM)
      WSPD  = SQRT(WS*WS + VCONV*VCONV)
      WSPD = AMAX1(WSPD,1.0)
      GOVRTH = G / THETA
      RICH = GOVRTH * Z * DTHVDZ / (WSPD * WSPD)
      RICUT = 1/(RICRITI+GZ1OZ0)

C------------------------------------------------------------------
C-- NOTE THAT THE REGIMES USED IN HIRPBL HAVE BEEN CHANGED:
C-- USE PLEIM(2006) TO CALCULATE PSI
      ZOL = 0.0
      IF (RICH .GE. RICUT) THEN
C
C-----CLASS 1; VERY STABLE CONDITIONS:  Z/L > 1
C
            ZOL = RICH*GZ1OZ0/(1-RICRITI*RICUT)
            PSIH    =  1.-BETAH-ZOL

      ELSE IF(RICH .GE. 0.0) THEN

C-----CLASS 2; STABLE: for 1 > Z/L >0
C
            ZOL = RICH*GZ1OZ0/(1-RICRITI*RICH)
            PSIH = -BETAH * ZOL

      ELSE
C
C-----CLASS 3 or 4; UNSTABLE:
C        CLASS 4 IS FOR ACM NON-LOCAL CONVECTION (H/L < -3)
C
          AH=0.04+0.355*ALOG(GZ1OZ0)
          SQLNZZ0=SQRT(GZ1OZ0)
          PSIH = AH*ALOG(1.-BH*SQLNZZ0*RICH)
C
      ENDIF

      RA = PR0*(GZ1OZ0-PSIH)/(KARMAN*UST)

      RETURN
      END Subroutine RA_MM5

      SUBROUTINE RA_WRF (moli,zol,zntol,zh,ustar,z0crj,j,c,r)

      IMPLICIT NONE

      REAL,    INTENT( IN ) :: moli, zol, zntol,zh, ustar, z0crj 
      INTEGER, INTENT( IN ) :: j,c,r

      REAL    y, ynt, psih, alogz1z0

      REAL,    PARAMETER :: betah = 5.0  ! WRF uses Dyer
      REAL,    PARAMETER :: gamah = 16.0 ! WRF uses Dyer
      REAL,    PARAMETER :: pr0   = 0.95 ! Prandtl number for neutral stability
      REAL,    PARAMETER :: karman = 0.4 ! von Karman constant

      IF ( moli .LT. 0.0 ) THEN
         y    = SQRT( 1.0 - gamah * zol )
         ynt  = SQRT( 1.0 - gamah * zntol )
         psih =  2.0 * LOG( ( y + 1.0 ) / ( ynt + 1.0 ) )
      ELSE
         IF ( ( zol - zntol ) .LE. 1.0 ) THEN
            psih = -betah * ( zol - zntol )
         ELSE
            psih = 1.0 - betah - ( zol - zntol )
         END IF
      END IF
      alogz1z0   = LOG( zh / z0crj )
      raj(j,c,r) = pr0 * ( alogz1z0 - psih ) / ( karman * ustar ) ! update the shared array

      RETURN
      END Subroutine RA_WRF
      
      End Module Mosaic_Mod
   

  
