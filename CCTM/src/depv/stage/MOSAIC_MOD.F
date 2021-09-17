
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

c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module MOSAIC_MOD
      
C Contains the shared variables and subrountes needed estimated the resistances
C from natural and agricultrual lands for NH3 bidirectional flux
 
C Revision History: J. Bash June 16 2011:    Created
C                   J. Young Oct 31 2011:    changed lai0, laimn0, rsmin, VEG0, vegmn0,
C                                             z00, & luf_fac to pointers to save memory
C                   D. Schwede Mar 12 2012:  fixed errors in crop lai
C                   D. Schwede Sept 07 2012: updated code for NLCD40 land use classification
C                   J. Bash:   Nov 07  2014: Modified for the restructuring of vidff. Most 
C                                            mosaic variables were moved to ASX_DATA_MOD. 
C                                            Algorithms were restuctured using fortran 95 
C                                            array construcs for readability.
C                   D. Wong:   Feb 10  2019: removed all MY_N clauses
C                   D. Wong:   Apr 24  2019: removed unused BUFF2D_2 array
C-------------------------------------------------------------------------------

      Implicit None
      
C Private variables used in this module and subroutines       
      Real, Save, Allocatable, Private :: fseas          ( :,: )
      Real, Save, Allocatable, Private :: f_land         ( :,: )
      Real, Save, Allocatable, Private :: sum_mos_lai    ( :,: )
      Real, Save, Allocatable, Private :: sum_mos_veg    ( :,: )
      Real, Save, Allocatable, Private :: vseas          ( :,: )
      Real, Save, Allocatable, Private :: znotc          ( :,: )

      Integer,         PRIVATE :: ALLOCSTAT
      Integer, Save, PRIVATE :: l_ag, l_agmos
      Logical, Save, Allocatable,  PRIVATE :: is_ag( : )
      Logical, Save, Allocatable,  PRIVATE :: is_agmos( : )
      Logical, Save, Allocatable,  PRIVATE :: is_water( : )

C Buffer variables  
      Real, Pointer, Private :: Buff2D_1       ( :,: )

      Type :: Tile_Type                
         Integer                      :: n_gas ! number of gas species for tiled output
         Integer                      :: n_aero! number of aerosol species for tiled output
         Integer                      :: n_lu  ! number of land use for tiled output
         Character( 16 ), Allocatable :: lu_NAME    ( : ) ! Tiled LU name
         Character( 16 ), Allocatable :: gas_NAME   ( : ) ! Gas species output name
         Logical,         Allocatable :: gas_out    ( : ) ! vector of length N_SPC_DIFF with T for output
         Character( 16 ), Allocatable :: aero_NAME  ( : ) ! Gas species output name
         Logical,         Allocatable :: aero_out   ( : ) ! vector of length N_SPC_DIFF with T for output
         real,            Allocatable :: lu2tile    ( : ) ! vector of length n_lufrac with lu index 
!> Aggrigated fractional land use       
         Real,            Allocatable :: Tile       ( :,:,: ) ! aggrigated land use 
!> Sub grid cell output:
         Real,            Allocatable :: depvel_gasj( :,:,:,: ) ! deposition velocity by land use type
         Real,            Allocatable :: vd_fst_gasj( :,:,:,: ) ! deposition velocity for stomatal
         Real,            Allocatable :: adepvj     ( :,:,:,: ) ! aerosol deposition
      End Type Tile_Type

      Type( Tile_Type ),     Save :: Tile_Data 
      
      Contains

         Subroutine Init_Mosaic( jdate, jtime, lufrac ) 
       
         Use HGRD_DEFN
         Use LSM_Mod
         Use UTILIO_DEFN
         USE STAGE_DATA, Only:dep_gas_all ! needs to be n_gas_asx to save memory but will require remapping
         USE CGRID_SPCS          ! CGRID mechanism species
         USE RUNTIME_VARS
       
         Implicit None    

C...include files

         Include SUBST_FILES_ID   ! file name parameters                 
       
         Integer, Intent( In )  :: jdate
         Integer, Intent( In )  :: jtime
         Real, Intent( In )  :: lufrac( :,:,: )    
         Character( 240 )       :: xmsg = ' '
         Character(  16 ), save :: pname = 'Init_Mosaic'
         Integer l
         Integer gxoff, gyoff            ! global origin offset from file
         Integer :: strtcolgc2, endcolgc2, strtrowgc2, endrowgc2
                                                                                                  
! Allocate buffers
         ALLOCATE ( BUFF2D_1( ncols,nrows ), STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating 2D Buffers'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If
         
         ALLOCATE ( fseas          ( ncols,nrows ),
     &              f_land         ( ncols,nrows ),
     &              sum_mos_lai    ( ncols,nrows ),
     &              sum_mos_veg    ( ncols,nrows ),  
     &              vseas          ( ncols,nrows ),
     &              znotc          ( ncols,nrows ), STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating MOSAIC variables'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If
         f_land = 0.0

!---------------------------------------------------------------------------------------------------
! placeholder to read namelist for lu agrigation and dep species output
!---------------------------------------------------------------------------------------------------

         Allocate ( Tile_Data%depvel_gasj( n_lufrac,N_GC_DEPV + N_NR_DEPV + N_TR_DEPV,ncols,nrows ),
     &              Tile_Data%adepvj     ( n_lufrac,N_AE_DEPV,ncols,nrows   ),  STAT = ALLOCSTAT )   
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating MOSAIC deposition velocities'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         Allocate ( is_ag    ( n_lufrac ),
     &              is_agmos ( n_lufrac ), 
     &              is_water ( n_lufrac ), STAT = ALLOCSTAT )   
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating is_ag, is_agmos, is_water'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If
         is_ag    = .FALSE.
         is_agmos = .FALSE.
         is_water = .FALSE.

         If( FST ) Then
            Allocate ( Tile_Data%vd_fst_gasj( n_lufrac,dep_gas_all,ncols,nrows ), STAT = ALLOCSTAT )   
            If ( ALLOCSTAT .Ne. 0 ) Then
               XMSG = 'Failure allocating FST deposition velocities'
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End If
         End If
! Get the location of ag and water in the land use fractions
         Do l = 1, n_lufrac
            If(cat_lu(l) .Eq. 'AG'   ) Then
               is_ag( l )    = .TRUE.
               l_ag          = l
            End If
            If(cat_lu(l) .Eq. 'AGMOS') Then
               is_agmos( l ) = .TRUE.
               l_agmos       = l
            End If
            If(cat_lu(l) .Eq. 'WATER') Then
               is_water( l ) = .TRUE.
            Else
               f_land = f_land + lufrac(:,:,l)
            End If
         End Do

         Return   
          
         End Subroutine Init_Mosaic
       
         Subroutine calc_lai( jday, jtime, SOIT2, LUFRAC, LAI, VEG,
     &                        MOS_VEG, MOS_LAI, MOS_Z0  )

C***********************************************************************
C  Function:
C     Calculate the lai for each LUC in the gridcell
C  Preconditions:  none
C  Subroutines and Functions Called:  none
C  Revision History:
C***********************************************************************

         Use LSM_Mod

         Implicit None

C Arguments:
         Integer, Intent( In )  :: jday
         Integer, Intent( In )  :: jtime     
         Real,    Intent( In )  :: SOIT2( :,: )
         Real,    Intent( In )  :: LAI( :,: )
         Real,    Intent( In )  :: VEG( :,: )
         Real,    Intent( In )  :: LUFRAC( :,:,: )
         Real,    Intent( Out ) :: MOS_VEG( :,:,: )
         Real,    Intent( Out ) :: MOS_LAI( :,:,: )
         Real,    Intent( Out ) :: MOS_Z0( :,:,: )

C Local variables:
         Integer :: c,r,j

C Local volatile variables:
         Real, Pointer :: d_past_emer ( :,: )

C initialize
         vseas           = 0.0
         fseas           = 0.0
         znotc           = 0.0
         Buff2D_1        = 0.0
         MOS_VEG         = 0.0
         MOS_LAI         = 0.0
         MOS_Z0          = 0.0
         sum_mos_lai     = 0.0
         sum_mos_veg     = 0.0

C calculate fseas based on deep soil temperature
         Where( SOIT2 .Lt. 290.0 .And. SOIT2 .Gt. 282.0 )
            fseas = 1.0 - 0.015625 * ( 290.0 - SOIT2 ) ** 2
         Elsewhere( SOIT2 .Ge. 290.0 )
            fseas = 1.0
         Elsewhere
            fseas = 0.0
         End where
C based on a 10 C germination temperature for 5 cm soil depth reported multiple agricultural extension offices, 
C e.g. https://www.agry.purdue.edu/ext/corn/news/timeless/Emergence.html. 
         Where( SOIT2 .Lt. 290.0 .And. SOIT2 .Gt. 283.0 )
            vseas = 1.0 - ( 290.0 - SOIT2 ) ** 2 / 7.0 ** 2
         Elsewhere( SOIT2 .Ge. 290.0 )
            vseas = 1.0
         Elsewhere
            vseas = 0.0
         End where
C find z0_crop by finding days past emergence
         d_past_emer => Buff2D_1
         d_past_emer =  0.0
         d_past_emer = ( ( LAIMN0( l_ag ) + vseas * ( LAI0( l_ag ) - LAIMN0( l_ag ) ) ) ** ( 1.0 / 1.923 ) ) / 2.273
         d_past_emer = max(0.0184 * 0.0184 - 4.0 * 1.057e-4 * d_past_emer,0.0)
         d_past_emer = ( 0.0184 - SQRT( d_past_emer ) ) / ( 2.0 * 1.057E-4 )
         znotc = 0.05
         Where ( d_past_emer .Gt. 87.0 )
            znotc = 0.15
         Elsewhere( d_past_emer .Gt. 0.0 )
            znotc = 5.00 + 0.23 * d_past_emer - 1.32E-3 * d_past_emer**2
            znotc = znotc / 100.0  ! convert to meters
         End Where
         Nullify( d_past_emer )
C get individual LAIs for LUCs for this date 

         Do j = 1, n_lufrac
            If( is_water( j ) ) Then
               Where( LUFRAC( :,:,j ) .Gt. 0.0 )
                  MOS_Z0 ( :,:,j )  = Z00( j )
               End Where
            Else
               If ( .NOT. is_ag( j ) .And. .NOT. is_agmos( j ) ) Then
                  Where( LUFRAC( :,:,j ) .Gt. 0.0 )
                     MOS_VEG( :,:,j ) = ( VEGMN0( j ) + fseas * ( VEG0( j ) -VEGMN0( j ) )  )/100.
                     MOS_LAI( :,:,j ) = LAIMN0( j ) + fseas * ( LAI0( j ) - LAIMN0( j ) )
                     MOS_Z0 ( :,:,j )  = Z00( j )
                  End Where
               Else If( is_ag( j ) ) Then
                  Where( LUFRAC( :,:,j ) .Gt. 0.0 )
                     MOS_VEG( :,:,j ) = ( VEGMN0( j ) + vseas * ( VEG0( j ) -VEGMN0( j ) )  )/100.
                     MOS_LAI( :,:,j ) = LAIMN0( j ) + vseas * ( LAI0( j ) - LAIMN0( j ) )
                     MOS_Z0 ( :,:,j )  = znotc
                  End Where
               Else If( is_agmos( j ) ) Then ! assume 50% natural and 50% crop
                     MOS_VEG( :,:,j ) = ( VEGMN0( j ) + (vseas+fseas)/2.0 * ( VEG0( j ) -VEGMN0( j ) )  )/100.
                     MOS_LAI( :,:,j ) = LAIMN0( j ) + (vseas+fseas)/2.0 * ( LAI0( j ) - LAIMN0( j ) )
                     MOS_Z0 ( :,:,j )  = 0.5 * ( znotc + Z00( j ) )
               End If
               sum_mos_lai = sum_mos_lai + MOS_LAI( :,:,j ) *  LUFRAC( :,:,j )
               sum_mos_veg = sum_mos_veg + MOS_VEG( :,:,j ) *  LUFRAC( :,:,j )
            End If
         End Do
C Now normalize the data to the meteorological LAI and VEG
         Do j = 1, n_lufrac 
            If( .NOT. is_water( j ) .AND. maxval(LUFRAC( :,:,j )) .Gt. 0.0 ) Then
               Where( sum_mos_lai .Gt. 0.0 .And. LAI .Gt. 0.0 .And. sum_mos_veg .Gt. 0.0 .And. VEG .Gt. 0.0 )
                  MOS_LAI( :,:,j ) = MOS_LAI( :,:,j ) * LAI / sum_mos_lai
                  MOS_VEG( :,:,j ) = MOS_VEG( :,:,j ) * VEG / sum_mos_veg
               End Where
               Where( MOS_LAI( :,:,j ) .Gt. 6.0 )
                  MOS_LAI( :,:,j ) = 6.0
               End Where
               Where( MOS_VEG( :,:,j ) .Gt.  0.999 ) ! not VEG0(j) to support earlier versions of WRF and satellite Veg 
                  MOS_VEG( :,:,j ) =  0.999
               End Where
               Where( MOS_VEG( :,:,j ) .Eq.  0.0 .Or. MOS_LAI( :,:,j ) .Eq.  0.0 )
                  MOS_LAI( :,:,j ) = 0.0
                  MOS_VEG( :,:,j ) = 0.0
               End Where
            End If
         End Do         

         Return

         End Subroutine Calc_LAI      

C*********************************************************************************************
C                    RA_WRF
C*********************************************************************************************

         Subroutine RA_WRF( MOLI, ZH, LUFRAC, RA, MOS_Z0, MOS_USTAR, MOS_RA, gamah, 
     &                      betah, karman )   

         Use LSM_Mod

         Implicit None

         Real, Intent( In )  :: gamah
         Real, Intent( In )  :: betah
         Real, Intent( In )  :: karman
         Real, Intent( In )  :: MOLI( :,: )
         Real, Intent( In )  :: ZH( :,:,: )
         Real, Intent( In )  :: LUFRAC( :,:,: )
         Real, Intent( In )  :: RA( :,: )
         Real, Intent( In )  :: MOS_Z0( :,:,: )
         Real, Intent( In )  :: MOS_USTAR( :,:,: )
         Real, Intent( Out ) :: MOS_RA( :,:,: )

         Integer            :: j
         Real, Parameter :: pr0        = 0.95

C local volatile variable
         Real, Pointer :: PSIH   ( :,: )

         PSIH => Buff2D_1
         PSIH = 0.0
         Do j = 1,n_lufrac
            Where( MOLI .Lt. 0.0 ) ! checked against PX
               PSIH = 2.0 * Log( ( Sqrt( 1.0 - gamah * ZH( :,:,1 ) * MOLI ) + 1.0 ) / 
     &                              ( Sqrt( 1.0 - gamah * MOS_Z0( :,:,j ) * MOLI ) + 1.0 ) )
            Else Where ( ( ZH( :,:,1 ) - MOS_Z0( :,:,j ) ) * MOLI .Le. 1.0 )
               PSIH = -betah * ( ZH( :,:,1 ) - MOS_Z0( :,:,j ) ) * MOLI
            Else Where
               PSIH = 1.0 - betah - ( ZH( :,:,1 ) - MOS_Z0( :,:,j ) ) * MOLI
            End Where
            Where ( LUFRAC( :,:,j ) .Eq. 1.0 ) 
               MOS_RA( :,:,j ) = RA
            Elsewhere( LUFRAC( :,:,j ) .Gt. 0.0 )
               MOS_RA( :,:,j ) = pr0 * ( Log( ZH( :,:,1 ) / MOS_Z0( :,:,j ) ) - PSIH ) / 
     &                                 ( karman * MOS_USTAR( :,:,j ) )
            End Where
         End Do
         Nullify( PSIH )
         Return
         End Subroutine RA_WRF

C*********************************************************************************************
C                    MOS_Rst
C*********************************************************************************************

         Subroutine MOS_RSTW(LUFRAC, MOS_LAI, RGRND, SOIM2, WWLT, WFC, TEMP2, MOS_RA, MOS_USTAR, 
     &                       QSS_GRND, QV, RST, MOS_RST)

         Use LSM_Mod
         Use GRID_CONF           ! horizontal & vertical domain specifications

         Implicit None

         Real, Intent( In )  :: LUFRAC( :,:,: )
         Real, Intent( In )  :: MOS_LAI( :,:,: )
         Real, Intent( In )  :: RGRND( :,: )
         Real, Intent( In )  :: SOIM2( :,: )
         Real, Intent( In )  :: WWLT( :,: )
         Real, Intent( In )  :: WFC( :,: )
         Real, Intent( In )  :: TEMP2( :,: )
         Real, Intent( In )  :: MOS_RA( :,:,: )
         Real, Intent( In )  :: MOS_USTAR( :,:,: )
         Real, Intent( In )  :: QSS_GRND( :,: )
         Real, Intent( In )  :: QV( :,:,: )
         Real, Intent( In )  :: RST( :,: )
         Real, Intent( Out ) :: MOS_RST( :,:,: )

         Real :: f1, f1max, par      ! radiation variables
         Real :: f2, w2avail, w2mxav ! soil moisture variables
         Real :: f3, gs, ga, raw     ! humidity variables
         Real :: f4                  ! temperature variables
         Real :: ftot, fshelt        ! combined Jarvis variables
         Real :: lu_tot              ! total land use where Rst is estiamted
         Real :: cor_fact            ! correction factor to match met model RST
         Real, Parameter :: f3min      = 0.25
         Real, Parameter :: ftmin      = 0.0000001  ! m/s
         Real, Parameter :: rsmax      = 5000.0     ! s/m
         Real            :: mean_mos_gst            ! area weighted mean Rst
         Integer         :: c, r, j                 ! loop induction variables
        
         DO c = 1, NCOLS
            DO r = 1, NROWS
              mean_mos_gst = 0.0
              lu_tot       = 0.0             
              If( f_land( c,r ) .Gt. 0.0 ) Then
!-SOIL MOISTURE
                  w2avail = SOIM2( c,r ) - WWLT( c,r )
                  w2mxav  = WFC ( c,r ) - WWLT( c,r )
                  f2      = 1.0 / ( 1.0 + EXP( -5.0 * ( w2avail / w2mxav -
     &                    ( w2mxav / 3.0 + WWLT( c,r ) ) ) ) )    ! according JP, 9/94
!-AIR TEMP
!... according to Avissar (1985) and AX 7/95
                  IF ( TEMP2( c,r ) .LE. 302.15 ) THEN
                     f4 = 1.0 / ( 1.0 + EXP( -0.41 * (TEMP2( c,r ) - 282.05 ) ) )
                  ELSE
                     f4 = 1.0 / ( 1.0 + EXP( 0.5 * (TEMP2( c,r ) - 314.0 ) ) )
                  END IF
!-RADIATION
                  par = 0.45 * RGRND( c,r ) * 4.566
                  DO j = 1, n_lufrac
                     IF ( LUFRAC( c,r,j ) .GT. 0.0 .AND. MOS_LAI( c,r,j ) .LT. 0.00001 ) THEN
                           MOS_RST( c,r,j ) = rsmax
                     ELSE IF ( LUFRAC( c,r,j ) .GT. 0.0 ) THEN
                        IF ( rsmin( j ) .GT. 130.0 ) THEN
                           f1max = 1.0-0.02*MOS_LAI( c,r,j )
                        ELSE
                           f1max = 1.0-0.07*MOS_LAI( c,r,j )
                        END IF
                        f1  = f1max * ( 1.0 - exp( -0.0017 * par ) )
                        f1  = amax1( f1, rsmin( j ) / rsmax )
                        ftot = MOS_LAI( c,r,j ) * f1 * f2 * f4
                        ftot = MAX( ftot,ftmin )
                        fshelt = 1.0   ! go back to NP89
                        gs     = ftot / ( rsmin( j ) * fshelt )
                        raw    = MOS_RA( c,r,j ) + 4.503 / MOS_USTAR( c,r,j )
                        ga     = 1.0 / raw
!-- Compute humidity effect according to RH at leaf surf
                        f3 = 0.5 * ( gs - ga + SQRT( ga * ga + ga * gs
     &                   * ( 4.0 * QV( c,r,1 ) / QSS_GRND( c,r ) - 2.0 ) + gs * gs ) ) / gs
                        f3 = MIN ( MAX( f3, f3min ), 1.0 )
                        MOS_RST( c,r,j ) = 1.0 / ( gs * f3 )
                     END IF
                  END DO ! lufrac
               END IF ! LWMASK
            END DO ! rows
         END DO ! cols
         Return
         End Subroutine MOS_RSTW

C*********************************************************************************************
C                    MOS_CanWat
C*********************************************************************************************

         Subroutine MOS_CanWat(LUFRAC, MOS_VEG, MOS_LAI, WR, MOS_DELTA)

         Use LSM_Mod
         Use GRID_CONF           ! horizontal & vertical domain specifications

         Implicit None

         Real, Intent( In )  :: LUFRAC( :,:,: )
         Real, Intent( In )  :: MOS_VEG( :,:,: )
         Real, Intent( In )  :: MOS_LAI( :,:,: )
         Real, Intent( In )  :: WR( :,: )
         Real, Intent( Out ) :: MOS_DELTA( :,:,: )

         Integer         :: j                 ! loop induction variables

         DO j = 1, n_lufrac
            Where ( ( WR .LE. 0.0 ) .or. ( MOS_LAI(:,:,j) .LE. 0.0 ) )
               MOS_DELTA( :,:,j ) = 0.0
            Elsewhere( LUFRAC( :,:,j ) .Gt. 0.0 )
               MOS_DELTA( :,:,j ) = WR / ( 0.2e-3 * MOS_VEG(:,:,j) * MOS_LAI(:,:,j) )   ! refer to SiB model
            End Where
         End Do
         Where( MOS_DELTA .GT. 1.0 ) 
            MOS_DELTA = 1.0
         End Where               

         Return
         End Subroutine MOS_CanWat
      
      End Module Mosaic_Mod
