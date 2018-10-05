
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
C-------------------------------------------------------------------------------

      Implicit None

!      INCLUDE SUBST_CONST     ! constants
      
C shared variables 
!      Real, Save, Allocatable :: rcanj      ( :,:,:,: )! stomatal, mesiphyll and cuticular resistances only
!      Real, Save, Allocatable :: depvel_gasj( :,:,:,: ) ! deposition velocity by land use type
!      Real, Save, Allocatable :: vd_fst_gasj( :,:,:,: ) ! deposition velocity for stomatal and
!      Real, Save, Allocatable :: adepvj     ( :,:,:,: ) ! deposition velocity for stomatal and
      
!     Character( 80 ), Save   :: LAND_SCHEME 

C Private variables used in this module and subroutines       
      Real, Save, Allocatable, Private :: fseas          ( :,: )
      Real, Save, Allocatable, Private :: total_frac     ( :,: )
      Real, Save, Allocatable, Private :: sum_mos_frac   ( :,: )
      Real, Save, Allocatable, Private :: sum_crop_frac  ( :,: )
      Real, Save, Allocatable, Private :: sum_mos_lai    ( :,: )
      Real, Save, Allocatable, Private :: sum_nat_lai    ( :,: )
      Real, Save, Allocatable, Private :: new_sum_nat_lai( :,: )
      Real, Save, Allocatable, Private :: lai_crop       ( :,: )
      Real, Save, Allocatable, Private :: znotc          ( :,: )
      Real, Save, Allocatable, Private :: WSPD2          ( :,: )
      Real, Save, Allocatable, Private :: US             ( :,: )
      Real, Save, Allocatable, Private :: VS             ( :,: )
      Real, Save, Allocatable, Private :: RICH           ( :,: )

      Integer,         PRIVATE :: ALLOCSTAT

C Buffer variables  
      Real, Pointer, Private :: Buff2D_1       ( :,: )
      Real, Pointer, Private :: Buff2D_2       ( :,: )

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
         Real,            Allocatable :: pol        ( :,:,:,: ) ! Bidi production over loss rate
         Real,            Allocatable :: adepvj     ( :,:,:,: ) ! aerosol deposition
      End Type Tile_Type

      Type( Tile_Type ),     Save :: Tile_Data 
      
      Contains

         Subroutine Init_Mosaic( jdate, jtime ) 
       
         Use HGRD_DEFN
         Use LSM_Mod
         Use UTILIO_DEFN
!         USE DEPVVARS, Only: dep_gas_all
         USE STAGE_DATA, Only:dep_gas_all ! needs to be n_gas_asx to save memory but will require remapping
         USE CGRID_SPCS          ! CGRID mechanism species
         USE RUNTIME_VARS
       
         Implicit None    

C...include files

         Include SUBST_FILES_ID   ! file name parameters                 
       
         Integer, Intent( In )  :: jdate
         Integer, Intent( In )  :: jtime  
         Character( 240 )       :: xmsg = ' '
         Character(  16 ), save :: pname = 'Init_Mosaic'
         Integer l
         Integer gxoff, gyoff            ! global origin offset from file
         Integer :: strtcolgc2, endcolgc2, strtrowgc2, endrowgc2
                                                                                                  
! Allocate buffers
         ALLOCATE ( BUFF2D_1( my_ncols,my_nrows ),
     &              BUFF2D_2( my_ncols,my_nrows ), STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating 2D Buffers'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If
         
         ALLOCATE ( fseas          ( my_ncols,my_nrows ),
     &              total_frac     ( my_ncols,my_nrows ),
     &              sum_mos_frac   ( my_ncols,my_nrows ),
     &              sum_crop_frac  ( my_ncols,my_nrows ), 
     &              sum_mos_lai    ( my_ncols,my_nrows ), 
     &              sum_nat_lai    ( my_ncols,my_nrows ), 
     &              new_sum_nat_lai( my_ncols,my_nrows ),
     &              lai_crop       ( my_ncols,my_nrows ),
     &              znotc          ( my_ncols,my_nrows ),
     &              US             ( my_ncols,my_nrows ),
     &              VS             ( my_ncols,my_nrows ),
     &              WSPD2          ( my_ncols,my_nrows ), 
     &              RICH           ( my_ncols,my_nrows ), STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating MOSAIC variables'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

!---------------------------------------------------------------------------------------------------
! placeholder to read namelist for lu agrigation and dep species output
!---------------------------------------------------------------------------------------------------

         Allocate ( Tile_Data%depvel_gasj( n_lufrac,dep_gas_all,my_ncols,my_nrows ),
     &              Tile_Data%adepvj     ( n_lufrac,N_AE_DEPV,my_ncols,my_nrows   ), 
     &              Tile_Data%pol        ( n_lufrac,dep_gas_all,my_ncols,my_nrows ), STAT = ALLOCSTAT )   
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating MOSAIC deposition velocities'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         If( FST ) Then
            Allocate ( Tile_Data%vd_fst_gasj( n_lufrac,dep_gas_all,my_ncols,my_nrows ), STAT = ALLOCSTAT )   
            If ( ALLOCSTAT .Ne. 0 ) Then
               XMSG = 'Failure allocating FST deposition velocities'
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End If
         End If

         Return   
          
         End Subroutine Init_Mosaic
       
         Subroutine calc_lai( jday, jtime, SOIT2, LUFRAC, LAI,
     &                        MOS_VEG, MOS_LAI, MOS_Z0  )

C***********************************************************************
C  Function:
C     Calculate the lai for each LUC in the gridcell
C  Preconditions:  none
C  Subroutines and Functions Called:  none
C  Revision History:
C***********************************************************************

         Use LSM_Mod
!         Use ASX_DATA_MOD

         Implicit None

C Arguments:
         Integer, Intent( In )  :: jday
         Integer, Intent( In )  :: jtime     
         Real, Intent( In )     :: SOIT2( :,: )
         Real, Intent( In )     :: LAI( :,: )
         Real, Intent( In )     :: LUFRAC( :,:,: )
         Real, Intent( Out )    :: MOS_VEG( :,:,: )
         Real, Intent( Out )    :: MOS_LAI( :,:,: )
         Real, Intent( Out )    :: MOS_Z0( :,:,: )

C Local variables:
         Integer :: c,r,j
         Real    :: lai_err

C Local volatile variables:
         Real, Pointer :: argw        ( :,: )
         Real, Pointer :: correction  ( :,: )
         Real, Pointer :: d_past_emer ( :,: )
         Real, Pointer :: vseas       ( :,: )
#ifdef Verbose
         Real, Pointer :: mos_lai     ( :,: )
#endif

C initialize
         lai_crop        = 0.0
         sum_nat_lai     = 0.0
         new_sum_nat_lai = 0.0
         sum_mos_lai     = 0.0
         sum_crop_frac   = 0.0
         sum_mos_frac    = 0.0
         total_frac      = 0.0
         znotc           = 0.0
         Buff2D_1        = 0.0

C calculate fseas based on deep soil temperature
         Where( SOIT2 .Lt. 290.0 .And. SOIT2 .Gt. 282.0 )
            fseas = 1.0 - 0.015625 * ( 290.0 - SOIT2 ) ** 2
         Elsewhere( SOIT2 .Ge. 290.0 )
            fseas = 1.0
         Elsewhere
            fseas = 0.0
         End where
C get individual LAIs for LUCs for this date 
         Do j = 1, n_lufrac
            total_frac = total_frac + LUFRAC( :,:,j )
         End Do

         argw => Buff2D_1
         argw =  0.0
         Do j = 1, n_lufrac
            Where( LUFRAC( :,:,j ) .Gt. 0.0 )
               MOS_VEG( :,:,j ) = ( ( 1.0 - fseas ) * VEGMN0( j ) + fseas * VEG0( j ) )/100.
            End Where
            If ( cat_lu( j ) .NE. 'AG' .And. cat_lu( j ) .NE. 'AGMOSW'
     &                       .And. cat_lu( j ) .NE. 'AGMOS' ) Then
               Where( LUFRAC( :,:,j ) .Gt. 0.0 .And. LAI .Gt. 0.0 )
                  MOS_LAI( :,:,j ) = LAIMN0( j ) + fseas * ( LAI0( j ) - LAIMN0( j ) )
                  MOS_Z0 ( :,:,j )  = Z00( j )
               Elsewhere( LUFRAC( :,:,j ) .Gt. 0.0 )
                  MOS_LAI( :,:,j ) = 0.0
                  MOS_Z0 ( :,:,j ) = Z00( j )
               End Where
               sum_nat_lai = sum_nat_lai + LUFRAC( :,:,j ) * MOS_LAI( :,:,j )
            End If
            If( cat_lu(j) .Eq. 'AG' ) Then
               sum_crop_frac = sum_crop_frac + LUFRAC( :,:,j )
            End If
            If( cat_lu(j) .EQ. 'AGMOS' .Or. cat_lu(j) .EQ. 'AGMOSW' ) Then
               sum_mos_frac = sum_mos_frac + LUFRAC( :,:,j )
               sum_mos_lai  = sum_mos_lai  + LUFRAC( :,:,j ) * ( LAIMN0(j) + fseas * ( LAI0(j) - LAIMN0(j) ) )
               sum_nat_lai = sum_nat_lai + 0.5 * LUFRAC( :,:,j ) * MOS_LAI( :,:,j )
            End If
            If( cat_lu(j) .Eq. 'WATER') Then
               argw = argw + LUFRAC( :,:,j )
            End If
         End Do

C total frac needs to be non-water
         Where( argw .GT. 0.10 )
            total_frac = total_frac - argw
         Elsewhere
            total_frac = 1.0
         End Where
         Nullify( argw )

         Where( sum_crop_frac + 0.5 * sum_mos_frac .Gt. 0.0 .And.
     &          total_frac * LAI .Gt. sum_nat_lai ) ! only estimate crop lai for ag
            lai_crop = ( total_frac * LAI - sum_nat_lai ) / ( sum_crop_frac + 0.5 * sum_mos_frac )
         Elsewhere( sum_crop_frac + sum_mos_frac .Gt. 0.0 ) ! small crop or mosaic fraction can lead to rounding errors
            lai_crop = 0.5
         End Where

#ifdef Verbose
         If(Maxval(lai_crop) .Gt. 3.16 .Or. Minval(lai_crop, Mask = sum_mos_frac + sum_crop_frac .gt. 0.0 ) .Lt. 0.5 ) Then
            Write(Logdev,*) 'sum_mos_frac  Max:', Maxval(sum_mos_frac, Mask = lai_crop .Eq. Maxval(lai_crop)), 
     &                                    'Min:', Minval(sum_mos_frac, Mask = lai_crop .Eq. Minval(lai_crop, 
     &                                            Mask = sum_mos_frac + sum_crop_frac .gt. 0.0))
            Write(Logdev,*) 'sum_crop_frac Max:', Maxval(sum_crop_frac, Mask = lai_crop .Eq. Maxval(lai_crop)), 
     &                                    'Min:', Minval(sum_crop_frac, Mask = lai_crop .Eq. Minval(lai_crop, 
     &                                            Mask = sum_mos_frac + sum_crop_frac .gt. 0.0))
            Write(Logdev,*) 'Total frac    Max:', Maxval(total_frac, Mask = lai_crop .Eq. Maxval(lai_crop)), 
     &                                    'Min:', Minval(total_frac, Mask = lai_crop .Eq. Minval(lai_crop, 
     &                                            Mask = sum_mos_frac + sum_crop_frac .gt. 0.0))
            Write(Logdev,*) 'lai_crop      Max:', Maxval(lai_crop),
     &                                    'Min:', Minval(lai_crop, Mask = sum_mos_frac + sum_crop_frac .gt. 0.0)
            Write(Logdev,*) 'sum_mos_lai   Max:', Maxval(sum_mos_lai, Mask = lai_crop .Eq. Maxval(lai_crop)), 
     &                                    'Min:', Minval(sum_mos_lai, Mask = lai_crop .Eq. Minval(lai_crop, 
     &                                            Mask = sum_mos_frac + sum_crop_frac .gt. 0.0))
            Write(Logdev,*) 'sum_nat_lai   Max:', Maxval(sum_nat_lai, Mask = lai_crop .Eq. Maxval(lai_crop)), 
     &                                    'Min:', Minval(sum_nat_lai, Mask = lai_crop .Eq. Minval(lai_crop, 
     &                                            Mask = sum_mos_frac + sum_crop_frac .gt. 0.0))
            Write(Logdev,*) 'WRF LAI       Max:', Maxval(total_frac * LAI, Mask = lai_crop .Eq. Maxval(lai_crop)), 'Min:',
     &                                            Minval(total_frac * LAI, Mask = lai_crop .Eq. Minval(lai_crop, 
     &                                            Mask = sum_mos_frac + sum_crop_frac .gt. 0.0))
            Write(Logdev,*) 'Mos-WRF LAI   Max:', Maxval(total_frac * LAI -sum_nat_lai,Mask = lai_crop .Eq. Maxval(lai_crop)), 
     &                                    'Min:', Minval(total_frac * LAI -sum_nat_lai,Mask = lai_crop .Eq. Minval(lai_crop, 
     &                                            Mask = sum_mos_frac + sum_crop_frac .gt. 0.0))
         End If
#endif
C check to see if correction needed
         correction => Buff2D_1
         correction =  0.0
         If( Minval(lai_crop) .Lt. 0.5 .Or. Maxval(lai_crop) .Gt. MAXVAL( LAI0, MASK = cat_lu .EQ. 'AG' ) ) Then         
            correction = 1.0
            Where( lai_crop .Lt. 0.5 ) ! method didn't work - numerical error - use approximation based on fseas
               lai_crop = 0.5
               new_sum_nat_lai = total_frac * LAI -
     &                      ( lai_crop * ( sum_crop_frac + 0.5* sum_mos_frac ) )
            elsewhere( lai_crop .Gt. 3.16 )
               lai_crop = MAXVAL( LAIMN0, MASK = cat_lu .EQ. 'AG' ) + fseas * ( MAXVAL( LAI0 - LAIMN0, MASK =  cat_lu .EQ. 'AG' ) )
               new_sum_nat_lai = total_frac * LAI -
     &                      ( lai_crop * ( sum_crop_frac + 0.5* sum_mos_frac ) )
            elsewhere
               new_sum_nat_lai = 0.0
            End Where
               
#ifdef Verbose
            Write( logdev,'(47x, a, f12.5)' ) 'New lai_crop = ', lai_crop(c,r)
#endif
            

            Where ( sum_nat_lai .Ge. 0.1 .And. new_sum_nat_lai .Ge. 0.1 )   ! worth doing a correction
               correction = new_sum_nat_lai / sum_nat_lai
            End Where

#ifdef Verbose
            Write( logdev,* ) 'Old lai: ', Sum(LAI,Dim=3,
     &                                     mask = correction .Eq. Maxval(correction) )
            Write( logdev,* ) 'correction = ', Maxval(correction)
#endif

            Forall( j = 1:n_lufrac, (cat_lu(j) .NE. 'AG' .AND. cat_lu(j) .NE. 'AGMOS' .AND. cat_lu(j) .NE. 'AGMOSW') )
               MOS_LAI( :,:,j ) = min( MOS_LAI( :,:,j ) * correction, LAI0(j) )
            End Forall
         End If   ! correction needed
         Nullify(correction)

C find z0_crop by finding days past emergence
         d_past_emer => Buff2D_1
         d_past_emer =  0.0
         d_past_emer = ( ( lai_crop ) ** ( 1.0 / 1.923 ) ) / 2.273
         d_past_emer = max(0.0184 * 0.0184 - 4.0 * 1.057e-4 * d_past_emer,0.0)
         d_past_emer = ( 0.0184 - SQRT( d_past_emer ) ) / ( 2.0 * 1.057E-4 )
         If ( jday .Gt. 288 ) Then
            znotc = 0.05
         Else
            Where ( d_past_emer .Le. 0.0 )
               znotc = 0.05
            Elsewhere ( d_past_emer .Gt. 87.0 )
               znotc = 0.15
            Elsewhere
               znotc = 5.00 + 0.23 * d_past_emer - 1.32E-3 * d_past_emer**2
               znotc = znotc / 100.0  ! convert to meters
            End Where
         End If
         Nullify( d_past_emer )

c estimate vseas
         vseas => Buff2D_1
         vseas =  0.0
         vseas = (lai_crop-0.5)/2.5
         Where( vseas .Lt. 0.0 )
            vseas = 0.0
         Elsewhere( vseas .Gt. 1.0 )
            vseas = 1.0
         End Where

         Forall(j = 1:n_lufrac, cat_lu(j) .Eq. 'AG' .And. Maxval( LUFRAC( :,:,j ) ) .Gt. 0.0 )
            MOS_LAI( :,:,j ) = min( lai_crop, LAI0(j) )
            MOS_Z0 ( :,:,j ) = znotc
            MOS_VEG( :,:,j ) = (( 1.0 - vseas ) * VEGMN0(j) + vseas * VEG0(j) ) / 100.0
         End Forall

         Forall(j = 1:n_lufrac, cat_lu(j) .Eq. 'AGMOS' .OR. cat_lu(j) .Eq. 'AGMOSW'  .And. 
     &                          Maxval( LUFRAC( :,:,j )) .Gt. 0.0 )
            MOS_LAI( :,:,j ) = min( 0.5 * ( LAIMN0(j) + fseas * ( LAI0(j) - LAIMN0(j) ) ) + 
     &                              0.5 * lai_crop, LAI0(j) )
            MOS_Z0 ( :,:,j )  = 0.5 * ( znotc + Z00(j) )
            MOS_VEG( :,:,j ) = (( 1.0 - vseas ) * VEGMN0(j) + vseas * VEG0(j) ) / 100.0
         End Forall
         sum_mos_lai = 0.0
         Do j = 1, n_lufrac 
            sum_mos_lai = sum_mos_lai + MOS_LAI( :,:,j ) *  LUFRAC( :,:,j )
         End Do
         Do j = 1, n_lufrac 
            Where( sum_mos_lai > 0.0 )
               MOS_LAI( :,:,j ) = MOS_LAI( :,:,j ) * LAI / sum_mos_lai
            End Where
            Where( MOS_LAI( :,:,j ) .Gt. LAI0( j ) )
               MOS_LAI( :,:,j ) = LAI0( j )
            End Where
         End Do         
#ifdef Verbose
         mos_lai => Buff2D_1
         mos_lai =  0.0
         Do j = 1,n_lufrac
            mos_lai = mos_lai + MOS_LAI( :,:,j ) * LUFRAC( :,:,j )
         End Do
         lai_err = MaxVal( mos_lai - LAI ) / LAI )
         If ( lai_err .Gt. 0.05 ) Then
            Write( logdev,* ) ' '
            Write( logdev,* ) 'Error in total LAI'
            Write( logdev,* ) 'LAI grid = ', MaxVal(LAI, 
     &         Mask = lai_err .Eq. mos_lai - LAI ) / LAI ), 
     &       ' LAI mosaic = ', MaxVal(mos_lai,
     &         Mask = lai_err .Eq. mos_lai - LAI ) / LAI )
            Write( logdev,* ) ' '
         End If
         Nullify( mos_lai )
#endif

         Return

         End Subroutine Calc_LAI      

C*********************************************************************************************
C                    RA_MM5
C*********************************************************************************************

         Subroutine RA_MM5(UWIND, VWIND, LWMASK, SNOCOV, SOIM1, WFC, QV, Q2, QSS_GRND, 
     &                     TEMPG, PRSFC, THETAV, ZH, LUFRAC, VEG, RA, MOS_Z0, MOS_USTAR, 
     &                     MOS_RA, karman )

         Use LSM_Mod
         Use GRID_CONF           ! horizontal & vertical domain specifications
!         Use ASX_DATA_MOD

         Implicit None

         Include SUBST_CONST     ! constants

         Real, Intent( In )     :: karman
         Real, Intent( In )     :: VWIND( :,:,: )
         Real, Intent( In )     :: UWIND( :,:,: )
         Real, Intent( In )     :: LWMASK( :,: )
         Real, Intent( In )     :: SNOCOV( :,: )
         Real, Intent( In )     :: SOIM1( :,: )
         Real, Intent( In )     :: WFC( :,: )
         Real, Intent( In )     :: Q2( :,: )
         Real, Intent( In )     :: QV( :,:,: )
         Real, Intent( In )     :: QSS_GRND( :,: )
         Real, Intent( In )     :: TEMPG( :,: )
         Real, Intent( In )     :: PRSFC( :,: )
         Real, Intent( In )     :: THETAV( :,:,: )
         Real, Intent( In )     :: ZH( :,:,: )
         Real, Intent( In )     :: LUFRAC( :,:,: )
         Real, Intent( In )     :: VEG( :,: )
         Real, Intent( In )     :: RA( :,: )
         Real, Intent( In )     :: MOS_Z0( :,:,: )
         Real, Intent( In )     :: MOS_USTAR( :,:,: )
         Real, Intent( Out )    :: MOS_RA( :,:,: )

         Real BETAH_MM5, BH, RICRIT, ROVCP, EP1, ROVG
         Real RICRITI
         Integer c,r,j
         Real, Parameter :: pr0        = 0.95
C Volatile variables
         Real, Pointer :: BETAP  ( :,: )
         Real, Pointer :: HU     ( :,: )
         Real, Pointer :: QFC    ( :,: )
         Real, Pointer :: CVIRT  ( :,: )
         Real, Pointer :: TV0    ( :,: )
         Real, Pointer :: TH0    ( :,: )
         Real, Pointer :: DTHVM  ( :,: )
         Real, Pointer :: DTHVDZ ( :,: )
         Real, Pointer :: VCONV  ( :,: )
         Real, Pointer :: RICUT  ( :,: )
         Real, Pointer :: PSIH   ( :,: )

C Calculate Ra via the richardson number as done for MM5

         Data BETAH_MM5/ 8.21 /
         Data BH/ 15.7 /

         Data RICRIT/ 0.25 /

         Save BETAH_MM5, BH, RICRIT

C from param.F
         ROVCP = RDGAS / CPD
         EP1   = 0.608
         ROVG  = RDGAS / GRAV

         US = 0.25 * (UWIND(1:my_ncols,1:my_nrows,    1) + UWIND(2:(my_ncols+1),1:my_nrows,    1) +
     &                UWIND(1:my_ncols,2:(my_nrows+1),1) + UWIND(2:(my_ncols+1),2:(my_nrows+1),1))
         VS = 0.25 * (VWIND(1:my_ncols,1:my_nrows,    1) + VWIND(2:(my_ncols+1),1:my_nrows,    1) +
     &                VWIND(1:my_ncols,2:(my_nrows+1),1) + VWIND(2:(my_ncols+1),2:(my_nrows+1),1))

C..... beta method, Lee & Pielke (JAM,May1992)
C-- compute sfc rh from soil moisture
C*     SATURATED FOR WATER SURFACE AND SNOW (ISNOW=1)
         BETAP => Buff2D_1
         HU    => Buff2D_2
         HU    =  1.0
         BETAP =  1.0
         Where( NINT( LWMASK ) .Ne. 0 .And. SNOCOV .Lt. 0.5 .And. 
     &            SOIM1 .Le. WFC )
           BETAP = 0.25 * ( 1.0 - COS( SOIM1 / WFC * PI) )**2 ! BETAP          
           HU    = BETAP + (1.0 - BETAP) * Q2 / QSS_GRND          ! HU
         End Where
         Nullify( BETAP ) ! HU is still pointing to Buff2D
         
         QFC   => Buff2D_1

         QFC =  HU * QSS_GRND 

         Nullify( HU )
         CVIRT => Buff2D_2
C.......... compute the thetav at ground
         CVIRT = ( 1.0 + EP1 * QFC )
         Nullify( QFC )
         TV0 => Buff2D_1
         TV0 = TEMPG * CVIRT
         Nullify( CVIRT )
         TH0 => Buff2D_2
         TH0 = TV0 * ( 1000.0 / PRSFC/100 )**ROVCP
         Nullify( TV0 )
C
C-----CALCULATE BULK RICHARDSON NO. OF SURFACE LAYER, ACCORDING TO
C        AKB(1976), EQ(12).
C
         DTHVDZ => Buff2D_2
         DTHVDZ  = THETAV( :,:,1 ) - TH0
         Nullify( TH0 )
         DTHVM => Buff2D_1
         Where( -DTHVDZ .Lt. 0.0 )
            DTHVM =  0.0  ! DTHVM
         Elsewhere
            DTHVM = -DTHVDZ
         End Where
         RICRITI = 1.0 / RICRIT
         VCONV => Buff2D_1
         VCONV = 2.0 * SQRT( DTHVM )
         Nullify( DTHVM )
         WSPD2   =  US**2 + VS**2 + VCONV**2
         Nullify( VCONV )
         Where( WSPD2 .Lt. 1.0 )
            WSPD2 = 1.0
         End Where

         Do j = 1,n_lufrac
            RICH  = GRAV / THETAV( :,:,1 ) / ( 1.0 + 0.61 * QV( :,:,1 ) ) *
     &               ZH( :,:,1 ) * DTHVDZ / WSPD2
            RICUT => Buff2D_1
            RICUT = 1.0 / ( RICRITI + LOG( ZH( :,:,1 ) / MOS_Z0( :,:,j ) ) ) 

C------------------------------------------------------------------
C-- NOTE THAT THE REGIMES USED IN HIRPBL HAVE BEEN CHANGED:
C-- USE PLEIM(2006) TO CALCULATE PSI
            PSIH => Buff2D_1
            Where ( RICH .Ge. RICUT  )
C-----CLASS 1; VERY STABLE CONDITIONS:     Z/L > 1
               PSIH = RICH*LOG( ZH( :,:,1 ) / MOS_Z0( :,:,j ) )/(1-RICRITI* RICUT ) ! ZOL
               PSIH =  1.-BETAH_MM5-PSIH
            Elsewhere (RICH .Ge. 0.0 )
C-----CLASS 2; STABLE: for 1 > Z/L >0
               PSIH = RICH*LOG( ZH( :,:,1 ) / MOS_Z0( :,:,j ) )/(1-RICRITI*RICH)
               PSIH = -BETAH_MM5 * PSIH ! PSIH
            Elsewhere
C-----CLASS 3 or 4; UNSTABLE:
C           CLASS 4 IS FOR ACM NON-LOCAL CONVECTION (H/L < -3) 
               PSIH = 0.04+0.355*ALOG(LOG( ZH( :,:,1 ) / MOS_Z0( :,:,j ))) ! AH
               PSIH = PSIH*ALOG(1.-BH*SQRT(LOG( ZH( :,:,1 ) / MOS_Z0( :,:,j )))*RICH) ! PSIH
            End where
            Nullify( RICUT )

            Where ( ( ( NINT(LWMASK) .Eq. 0 ) .AND. ( VEG .Eq. 0.0 ) ) .Or. LUFRAC( :,:,j ) .Eq. 1.0 ) 
               MOS_RA( :,:,j ) = RA
            Elsewhere( LUFRAC( :,:,j ) .Gt. 0.0 )
               MOS_RA( :,:,j ) = PR0*(LOG( ZH( :,:,1 ) / MOS_Z0( :,:,j ))-PSIH)/
     &                           (KARMAN*MOS_USTAR( :,:,j ))
            End Where
            Nullify(PSIH)
         End Do
         Nullify( DTHVDZ )

         Return
         End Subroutine RA_MM5

C*********************************************************************************************
C                    RA_WRF
C*********************************************************************************************

         Subroutine RA_WRF( MOLI, ZH, Z0, LWMASK, VEG, LUFRAC, RA, MOS_Z0, MOS_USTAR, MOS_RA, gamah, 
     &                      betah, karman )   

         Use LSM_Mod
!         Use ASX_DATA_MOD

         Implicit None

         Real, Intent( In )  :: gamah
         Real, Intent( In )  :: betah
         Real, Intent( In )  :: karman
         Real, Intent( In )  :: MOLI( :,: )
         Real, Intent( In )  :: ZH( :,:,: )
         Real, Intent( In )  :: Z0( :,: )
         Real, Intent( In )  :: LWMASK( :,: )
         Real, Intent( In )  :: VEG( :,: )
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

         Subroutine MOS_RSTW(LUFRAC, LWMASK, MOS_LAI, RGRND, SOIM2, WWLT, WFC, TEMP2, MOS_RA, MOS_USTAR, 
     &                       QSS_GRND, QV, RST, MOS_RST)

         Use LSM_Mod
         Use GRID_CONF           ! horizontal & vertical domain specifications

         Implicit None

         Real, Intent( In )  :: LUFRAC( :,:,: )
         Real, Intent( In )  :: LWMASK( :,: )
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
         Real, Parameter :: f3min      = 0.25
         Real, Parameter :: ftmin      = 0.0000001  ! m/s
         Real, Parameter :: rsmax      = 5000.0     ! s/m
         Real            :: mean_mos_rst            ! area weighted mean Rst
         Integer         :: c, r, j                 ! loop induction variables
        
         DO c = 1, MY_NCOLS
            DO r = 1, MY_NROWS
              mean_mos_rst = 0.0
              If(LWMASK( c,r ) .EQ. 0.0 ) Cycle
!-SOIL MOISTURE
               w2avail = SOIM2( c,r ) - WWLT( c,r )
               w2mxav  = WFC ( c,r ) - WWLT( c,r )
               f2      = 1.0 / ( 1.0 + EXP( -5.0 * ( w2avail / w2mxav -
     &                 ( w2mxav / 3.0 + WWLT( c,r ) ) ) ) )    ! according JP, 9/94
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
     &                * ( 4.0 * QV( c,r,1 ) / QSS_GRND( c,r ) - 2.0 ) + gs * gs ) ) / gs
                     f3 = MIN ( MAX( f3, f3min ), 1.0 )
                     MOS_RST( c,r,j ) = 1.0 / ( gs * f3 )
!                     mean_mos_rst = mean_mos_rst + MOS_RST( c,r,j ) * LUFRAC( c,r,j )
                  END IF
               END DO ! lufrac
 !              DO j = 1, n_lufrac
 !                 If( mean_mos_rst .Gt. 0.0 .And. MOS_LAI( c,r,j ) .Gt. 0.0 ) Then
!                     MOS_RST( c,r,j ) = MOS_RST( c,r,j ) * RST( c,r ) / mean_mos_rst
!                     MOS_RST( c,r,j ) = max(MOS_RST( c,r,j ),  rsmin( j ) /MOS_LAI( c,r,j ) )
!                  End If
!               End Do
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
