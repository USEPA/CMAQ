
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
      
C Contains the shared variables and subrountes needed estimate the resistances
C from natural and agricultural lands
 
C Revision History: J. Bash June 16 2011:    Created
C                   J. Young Oct 31 2011:    changed lai0, laimn0, rsmin, VEG0, vegmn0,
C                                             z00, & luf_fac to pointers to save memory
C                   D. Schwede Mar 12 2012:  fixed errors in crop lai
C                   D. Schwede Sept 07 2012: updated code for NLCD40 land use classification
C                   J. Bash:   Nov 07  2014: Modified for the restructuring of vidff. Most 
C                                            mosaic variables were moved to ASX_DATA_MOD. 
C                                            Algorithms were restructured using fortran 90 
C                                            array constructs for readability.
C                   D. Wong:   Feb 10  2019: removed all MY_N clauses
C                   D. Wong:   Apr 24  2019: removed unused BUFF2D_2 array
C-------------------------------------------------------------------------------

      Implicit None
      
C Private variables used in this module and subroutines       
      Real, Save, Allocatable, Private :: fseas          ( :,: ) ! Non-agricultural seasonal leaf on/off 0.0-1.0 where 1.0 is leaf on condutions
      Real, Save, Allocatable, Private :: f_land         ( :,: ) ! fraction of the grid cell that is land
      Real, Save, Allocatable, Private :: sum_mos_lai    ( :,: ) ! sum of the land use LAI used for normalization
      Real, Save, Allocatable, Private :: sum_mos_veg    ( :,: ) ! sum of the land use VEG used for normalization
      Real, Save, Allocatable, Private :: vseas          ( :,: ) ! Agricultural seasonal leaf on/off 0.0-1.0 where 1.0 is leaf on condutions
      Real, Save, Allocatable, Private :: znotc          ( :,: ) ! land use surface roughness lenth for momentum (m)
      Real, Save, Allocatable, Private :: lu_mean_ga     ( :,: ) ! mean land use aerodynamic conductance used for normalization (m/s)
      Real, Save, Allocatable, Private :: lu_mean_ustar  ( :,: ) ! mean land use ustar used for normalization (m/s)

      Integer, Save, Allocatable, Private :: lstwetdate( :,: ) ! last wet date
      Integer, Save, Allocatable, Private :: lstwettime( :,: ) ! last wet time 

      Integer,         PRIVATE :: ALLOCSTAT
      Integer, Save, PRIVATE :: l_ag, l_agmos
      Logical, Save, Allocatable,  PRIVATE :: is_ag( : )     ! Agricultural flag
      Logical, Save, Allocatable,  PRIVATE :: is_agmos( : )  ! Agricultural mosaic flag
      Logical, Save, Allocatable,  PRIVATE :: is_water( : )  ! Water flag

C Buffer variables  
      Real, Pointer, Private :: BUFF2D       ( :,: )

      Type :: Tile_Type                
         Integer                      :: n_vd ! number of gas species for tiled output
         Integer                      :: n_lufrac  ! number of land use for tiled output
         Real,            Allocatable :: RSMIN        ( : ) ! minimum stomatal resistance (s/m)
         Real,            Allocatable :: Z00          ( : ) ! momentum roughness length (m)
         Real,            Allocatable :: VEG0         ( : ) ! maximum vegetation coverage (%)
         Real,            Allocatable :: VEGMN0       ( : ) ! minimum vegetation coverage (%)
         Real,            Allocatable :: LAI0         ( : ) ! maximum lai (m2/m2)
         Real,            Allocatable :: LAIMN0       ( : ) ! minimum lai (m2/m2)
         Real,            Allocatable :: NH3_gam_grnd ( : ) ! soil/litter surface NH3 emission potential ([NH4]/[H])
         Real,            Allocatable :: NH3_gam_st   ( : ) ! vegetation NH3 emission potential ([NH4]/[H])
         Real,            Allocatable :: Hg_grnd      ( : ) ! soil Hg concentration (umol/g )
         Real,            Allocatable :: l_width      ( : ) ! aerodynamic leaf width (m)
         Real,            Allocatable :: Alpha        ( : ) ! Emerson et al 2020 PNAS empirical land use factor (unitless)
         Real,            Allocatable :: BAI          ( : ) ! building area index (m2/m2)
         Real,            Allocatable :: Ahair        ( : ) ! leaf hair width (m)
         Real,            Allocatable :: Fhair        ( : ) ! ratio of leaf covered in hairs
         Real,            Allocatable :: Aleaf        ( : ) ! aerodynamic leaf width 
!> Mapping for diagnostic ouputs
         Character( 16 ), Allocatable :: cat_lu       ( : ) ! Tiled LU name
         Character( 16 ), Allocatable :: name_lu      ( : ) ! Land use output name
         Logical,         Allocatable :: gas_out      ( : ) ! vector of length N_SPC_DIFF with TRUE for output
         Character( 16 ), Allocatable :: Vd_Name      ( : ) ! Deposition species output name
         Logical,         Allocatable :: Vd_out       ( : ) ! vector of length N_SPC_DEPV with TRUE for output
         Integer,         Allocatable :: dep2vdiff    ( : ) ! Vdiff location for dep species
         Real,            Allocatable :: Vd_fac       ( : ) ! Vd factor from the GC, NR, and TR namelists
!> Mean grid cell output 
         Real,            Allocatable :: Grd_Vd     ( :,:,: ) ! Grid mean gaseous and aerosol deposition velocity
         Real,            Allocatable :: Bidi_Emis  ( :,:,: ) ! Grid bidirectional Emissions
!> Aggrigated fractional land use       
         Real,            Allocatable :: LUfrac     ( :,:,: ) ! land use fraction (ratio)
!> Sub grid cell output:
         Real,            Allocatable :: Lu_Vd      ( :,:,:,: ) ! gaseous and aerosol deposition velocity
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
         USE CENTRALIZED_IO_MODULE, Only: WR_AVAIL
       
         Implicit None    

C...include files

         Include SUBST_FILES_ID   ! file name parameters                 
       
         Integer, Intent( In )  :: jdate
         Integer, Intent( In )  :: jtime
         Real, Intent( In )  :: lufrac( :,:,: )    
         Character( 240 )       :: xmsg = ' '
         Character(  16 ), save :: pname = 'Init_Mosaic'
         Integer n,l,s
         Integer N_GAS_DEPV
         Integer gxoff, gyoff            ! global origin offset from file
         Integer :: strtcolgc2, endcolgc2, strtrowgc2, endrowgc2

! Allocate Tile Data
         ALLOCATE ( Tile_Data%cat_lu       ( n_stage_lu ),
     &              Tile_Data%name_lu      ( n_stage_lu ), 
     &              Tile_Data%RSMIN        ( n_stage_lu ),
     &              Tile_Data%LAI0         ( n_stage_lu ),
     &              Tile_Data%LAIMN0       ( n_stage_lu ),
     &              Tile_Data%VEG0         ( n_stage_lu ),
     &              Tile_Data%VEGMN0       ( n_stage_lu ),
     &              Tile_Data%Z00          ( n_stage_lu ),
     &              Tile_Data%NH3_gam_st   ( n_stage_lu ),
     &              Tile_Data%NH3_gam_grnd ( n_stage_lu ),
     &              Tile_Data%Hg_grnd      ( n_stage_lu ),
     &              Tile_Data%l_width      ( n_stage_lu ),
     &              Tile_Data%Alpha        ( n_stage_lu ),
     &              Tile_Data%BAI          ( n_stage_lu ),
     &              Tile_Data%Ahair        ( n_stage_lu ),
     &              Tile_Data%Fhair        ( n_stage_lu ),
     &              Tile_Data%Aleaf        ( n_stage_lu ),STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating tile land use specific data'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

! STAGE land use 
         Tile_Data%n_lufrac = n_stage_lu
         Tile_Data%cat_lu       = STAGE_LU_Data( 1:n_stage_lu )%LU_Cat
         Tile_Data%name_lu      = STAGE_LU_Data( 1:n_stage_lu )%LU_Name
         Tile_Data%RSMIN        = STAGE_LU_Data( 1:n_stage_lu )%RSMIN
         Tile_Data%LAI0         = STAGE_LU_Data( 1:n_stage_lu )%LAI0
         Tile_Data%LAIMN0       = STAGE_LU_Data( 1:n_stage_lu )%LAIMN0
         Tile_Data%VEG0         = STAGE_LU_Data( 1:n_stage_lu )%VEG0
         Tile_Data%VEGMN0       = STAGE_LU_Data( 1:n_stage_lu )%VEGMN0
         Tile_Data%Z00          = STAGE_LU_Data( 1:n_stage_lu )%Z00
         Tile_Data%NH3_gam_st   = STAGE_LU_Data( 1:n_stage_lu )%Gamma_NH3_st
         Tile_Data%NH3_gam_grnd = STAGE_LU_Data( 1:n_stage_lu )%Gamma_NH3_grnd
         Tile_Data%Hg_grnd      = STAGE_LU_Data( 1:n_stage_lu )%Hg_grnd
         Tile_Data%l_width      = STAGE_LU_Data( 1:n_stage_lu )%l_width
         Tile_Data%Alpha        = STAGE_LU_Data( 1:n_stage_lu )%Alpha
         Tile_Data%BAI          = STAGE_LU_Data( 1:n_stage_lu )%BAI
         Tile_Data%Ahair        = STAGE_LU_Data( 1:n_stage_lu )%Ahair
         Tile_Data%Fhair        = STAGE_LU_Data( 1:n_stage_lu )%Fhair
         Tile_Data%Aleaf        = STAGE_LU_Data( 1:n_stage_lu )%Aleaf
                                                                                                  
! Allocate buffers
         ALLOCATE ( BUFF2D( ncols,nrows ), STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating 2D Buffers'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If
         
         ALLOCATE ( fseas          ( ncols,nrows ),
     &              f_land         ( ncols,nrows ),
     &              sum_mos_lai    ( ncols,nrows ),
     &              sum_mos_veg    ( ncols,nrows ),  
     &              vseas          ( ncols,nrows ),
     &              znotc          ( ncols,nrows ), 
     &              lu_mean_ga     ( ncols,nrows ), 
     &              lu_mean_ustar  ( ncols,nrows ),  STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating MOSAIC 2D variables'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If
         f_land        = 0.0

         IF( .not. WR_AVAIL ) Then
            ALLOCATE ( lstwetdate     ( ncols,nrows ),
     &                 lstwettime     ( ncols,nrows ), STAT = ALLOCSTAT )
            If ( ALLOCSTAT .Ne. 0 ) Then
               XMSG = 'Failure allocating MOSAIC lstwetdate and lstwettime'
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End If
            lstwetdate  = 0
            lstwettime  = 0
         End If

         N_GAS_DEPV = N_GC_DEPV + N_NR_DEPV + N_TR_DEPV
         Allocate ( Tile_Data%Lu_Vd     ( ncols,nrows,N_SPC_DEPV,Tile_Data%n_lufrac ), 
     &              Tile_Data%Grd_Vd    ( ncols,nrows,N_SPC_DEPV ),  
     &              Tile_Data%Bidi_Emis ( ncols,nrows,N_SPC_DEPV ),
     &              Tile_Data%LUfrac    ( ncols,nrows,Tile_Data%n_lufrac ),
     &              Tile_Data%Vd_Name   ( N_SPC_DEPV ),
     &              Tile_Data%Vd_Fac    ( N_SPC_DEPV ),
     &              Tile_Data%Vd_Out    ( N_SPC_DEPV ),        
     &              Tile_Data%dep2vdiff ( N_SPC_DEPV ),  
     &              STAT = ALLOCSTAT ) 
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating MOSAIC deposition velocities'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If
         Tile_Data%Lu_Vd     = 0.0
         Tile_Data%Grd_Vd    = 0.0
         Tile_Data%Bidi_Emis = 0.0
         Tile_Data%Vd_Fac    = 0.0  
         Tile_Data%dep2vdiff = 0        

! Map Met land use to STAGE land use
         Tile_Data%LUfrac = 0.0
         Do n = 1, n_xref_lu
            s = MET_TO_STAGE_LU( n )%Dep_Index 
            l = MET_TO_STAGE_LU( n )%Met_Index
            Tile_Data%LUfrac(:,:,s) = Tile_Data%LUfrac(:,:,s) +
     &            MET_TO_STAGE_LU( n )%Factor * lufrac(:,:,l)
         End Do

         Allocate ( is_ag    ( Tile_Data%n_lufrac ),
     &              is_agmos ( Tile_Data%n_lufrac ), 
     &              is_water ( Tile_Data%n_lufrac ), STAT = ALLOCSTAT )   
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating is_ag, is_agmos, is_water'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If
         is_ag    = .FALSE.
         is_agmos = .FALSE.
         is_water = .FALSE.

! Get the location of ag and water in the land use fractions
         Do l = 1, Tile_Data%n_lufrac
            If(Tile_Data%cat_lu(l) .Eq. 'AG'   ) Then
               is_ag( l )    = .TRUE.
               l_ag          = l
            End If
            If(Tile_Data%cat_lu(l) .Eq. 'AGMOS') Then
               is_agmos( l ) = .TRUE.
               l_agmos       = l
            End If
            If(Tile_Data%cat_lu(l) .Eq. 'WATER') Then
               is_water( l ) = .TRUE.
            Else
               f_land = f_land + Tile_Data%lufrac(:,:,l)
            End If
         End Do

         Return   
          
         End Subroutine Init_Mosaic
       
         Subroutine calc_lai( jday, jtime, SOIT2, LAI, VEG,
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
         BUFF2D        = 0.0
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
            vseas = 1.0 - ( 290.0 - SOIT2 ) ** 2 / 49.0 
         Elsewhere( SOIT2 .Ge. 290.0 )
            vseas = 1.0
         Elsewhere
            vseas = 0.0
         End where
C find z0_crop by finding days past emergence
         d_past_emer => BUFF2D
         d_past_emer =  0.0
         d_past_emer = ( ( Tile_Data%LAIMN0( l_ag ) + vseas * ( Tile_Data%LAI0( l_ag )
     &                -    Tile_Data%LAIMN0( l_ag ) ) ) ** ( 1.0 / 1.923 ) ) / 2.273
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

         Do j = 1, Tile_Data%n_lufrac
            If( is_water( j ) ) Then
               Where( Tile_Data%LUFRAC( :,:,j ) .Gt. 0.0 )
                  MOS_Z0 ( :,:,j )  = Tile_Data%Z00( j )
               End Where
            Else
               If ( .NOT. is_ag( j ) .And. .NOT. is_agmos( j ) ) Then
                  Where( Tile_Data%LUFRAC( :,:,j ) .Gt. 0.0 )
                     MOS_VEG( :,:,j ) = ( Tile_Data%VEGMN0( j ) + 
     &                                  fseas * ( Tile_Data%VEG0( j ) - Tile_Data%VEGMN0( j ) )  )/100.
                     MOS_LAI( :,:,j ) = Tile_Data%LAIMN0( j ) + 
     &                                  fseas * ( Tile_Data%LAI0( j ) - Tile_Data%LAIMN0( j ) )
                     MOS_Z0 ( :,:,j )  = Tile_Data%Z00( j )
                  End Where
               Else If( is_ag( j ) ) Then
                  Where( Tile_Data%LUFRAC( :,:,j ) .Gt. 0.0 )
                     MOS_VEG( :,:,j ) = ( Tile_Data%VEGMN0( j ) + 
     &                                  vseas * ( Tile_Data%VEG0( j ) - Tile_Data%VEGMN0( j ) )  )/100.
                     MOS_LAI( :,:,j ) = Tile_Data%LAIMN0( j ) + 
     &                                  vseas * ( Tile_Data%LAI0( j ) - Tile_Data%LAIMN0( j ) )
                     MOS_Z0 ( :,:,j )  = znotc
                  End Where
               Else If( is_agmos( j ) ) Then ! assume 50% natural and 50% crop
                     MOS_VEG( :,:,j ) = ( Tile_Data%VEGMN0( j ) + 
     &                                  ( vseas + fseas ) / 2.0 * ( Tile_Data%VEG0( j ) - Tile_Data%VEGMN0( j ) )  )/100.
                     MOS_LAI( :,:,j ) = Tile_Data%LAIMN0( j ) + 
     &                                  ( vseas + fseas ) / 2.0 * ( Tile_Data%LAI0( j ) - Tile_Data%LAIMN0( j ) )
                     MOS_Z0 ( :,:,j )  = 0.5 * ( znotc + Tile_Data%Z00( j ) )
               End If
               sum_mos_lai = sum_mos_lai + MOS_LAI( :,:,j ) *  Tile_Data%LUFRAC( :,:,j )
               sum_mos_veg = sum_mos_veg + MOS_VEG( :,:,j ) *  Tile_Data%LUFRAC( :,:,j )
            End If
         End Do
C Now normalize the data to the meteorological LAI and VEG
         Do j = 1, Tile_Data%n_lufrac 
            If( .NOT. is_water( j ) .AND. maxval( Tile_Data%LUFRAC( :,:,j ) ) .Gt. 0.0 ) Then
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
C                    RA_WRF based on PX LSM
C*********************************************************************************************
         Subroutine RA_WRF( MOLI, ZH, RA, Z0, MOS_Z0, USTAR, MOS_USTAR,
     &                      MOS_RA, gamah, betah, karman )   

         Use LSM_Mod

         Implicit None

         Real, Intent( In )     :: gamah              ! MOST coefficient for stability correction for unstable conditions [unitless]
         Real, Intent( In )     :: betah              ! MOST coefficient for stability correction for stable conditions [unitless]
         Real, Intent( In )     :: karman             ! von Karman constant [unitless]
         Real, Intent( In )     :: MOLI( :,: )        ! 1 over the Obukhov length [1/m]
         Real, Intent( In )     :: ZH( :,:,: )        ! mid-layer height above ground [m]
         Real, Intent( In )     :: RA( :,: )          ! grid aerodynamic resistance [s/m]
         Real, Intent( In )     :: Z0( :,: )          ! grid momentum roughness length [m]
         Real, Intent( In )     :: USTAR( :,: )       ! grid friction velocity [m/s]
         Real, Intent( InOut )  :: MOS_Z0( :,:,: )    ! land use momentum roughness length [m]
         Real, Intent( InOut )  :: MOS_USTAR( :,:,: ) ! land use friction velocity [m/s]
         Real, Intent( InOut )  :: MOS_RA( :,:,: )    ! land use aerodynamic resistance [s/m]

         Integer            :: j
         Real, Parameter    :: pr0        = 0.95      ! turbulent Prandtl number

C local volatile variable
         Real, Pointer :: PSIH   ( :,: ) ! Stability correction to the vertical wind profile

         lu_mean_ga    = 0.0
         lu_mean_ustar = 0.0
C Get surface flux variables
         Do j = 1, Tile_Data%n_lufrac
            Where( Tile_Data%LUFRAC( :,:,j ) .EQ. 1.0 .OR. ZH( :,:,1 ) .EQ. MOS_Z0( :,:,j ) )
                   MOS_USTAR( :,:,j ) = USTAR
                   MOS_Z0( :,:,j )    = Z0
            Elsewhere( Tile_Data%LUFRAC( :,:,j ) .Gt. 0.0 )
                   MOS_USTAR( :,:,j ) = USTAR * SQRT( LOG( ZH( :,:,1 ) / Z0 )
     &                                / LOG( ZH( :,:,1 ) / MOS_Z0( :,:,j ) ) )
            End Where
         End Do  

         PSIH => BUFF2D
         PSIH = 0.0
         Do j = 1,Tile_Data%n_lufrac
            Where( MOLI .Lt. 0.0 ) ! checked against PX
               PSIH = 2.0 * Log( ( Sqrt( 1.0 - gamah * ZH( :,:,1 ) * MOLI ) + 1.0 ) / 
     &                              ( Sqrt( 1.0 - gamah * MOS_Z0( :,:,j ) * MOLI ) + 1.0 ) )
            Else Where ( ( ZH( :,:,1 ) - MOS_Z0( :,:,j ) ) * MOLI .Le. 1.0 )
               PSIH = -betah * ( ZH( :,:,1 ) - MOS_Z0( :,:,j ) ) * MOLI
            Else Where
               PSIH = 1.0 - betah - ( ZH( :,:,1 ) - MOS_Z0( :,:,j ) ) * MOLI
            End Where
            Where ( Tile_Data%LUFRAC( :,:,j ) .Eq. 1.0 ) 
               MOS_RA( :,:,j ) = RA
            Elsewhere( Tile_Data%LUFRAC( :,:,j ) .Gt. 0.0 )
               MOS_RA( :,:,j ) = pr0 * ( Log( ZH( :,:,1 ) / MOS_Z0( :,:,j ) ) - PSIH ) / 
     &                                 ( karman * MOS_USTAR( :,:,j ) )
            End Where
         End Do
         Nullify( PSIH )
! Normalization loops
         Do j = 1,Tile_Data%n_lufrac
            Where( Tile_Data%LUFRAC( :,:,j ) .Gt. 0.0 )
               lu_mean_ga    = lu_mean_ga    + Tile_Data%LUFRAC( :,:,j ) / MOS_RA( :,:,j )
               lu_mean_ustar = lu_mean_ustar + Tile_Data%LUFRAC( :,:,j ) * MOS_USTAR( :,:,j )
            End Where
         End Do
         Do j = 1,Tile_Data%n_lufrac
            Where( Tile_Data%LUFRAC( :,:,j ) .Gt. 0.0 )
               MOS_RA( :,:,j )    = RA    * MOS_RA( :,:,j )    * lu_mean_ga
               MOS_USTAR( :,:,j ) = USTAR * MOS_USTAR( :,:,j ) / lu_mean_ustar
            End Where
         End Do         
         
         Return
         End Subroutine RA_WRF

C*********************************************************************************************
C                    MOS_Rst
C*********************************************************************************************
         Subroutine MOS_RSTW(MOS_LAI, RGRND, SOIM2, WWLT, WFC, TEMP2, MOS_RA, MOS_USTAR, 
     &                       QSS_GRND, QV, RST, MOS_RST)

         Use LSM_Mod
         Use GRID_CONF           ! horizontal & vertical domain specifications

         Implicit None

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
         Real            :: max_mos_gst, mos_gst   ! area weighted conductance
         Integer         :: c, r, j                 ! loop induction variables
         
         DO c = 1, NCOLS
            DO r = 1, NROWS
              lu_tot       = 0.0             
              If( f_land( c,r ) .Gt. 0.0 .And. RST( c,r ) .Ge. 1.0e30 ) Then
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
                  DO j = 1, Tile_Data%n_lufrac
                     IF ( Tile_Data%LUFRAC( c,r,j ) .GT. 0.0 .AND. MOS_LAI( c,r,j ) .LT. 0.00001 ) THEN
                           MOS_RST( c,r,j ) = rsmax
                     ELSE IF ( Tile_Data%LUFRAC( c,r,j ) .GT. 0.0 ) THEN
                        IF ( Tile_Data%rsmin( j ) .GT. 130.0 ) THEN
                           f1max = 1.0-0.02*MOS_LAI( c,r,j )
                        ELSE
                           f1max = 1.0-0.07*MOS_LAI( c,r,j )
                        END IF
                        f1  = f1max * ( 1.0 - exp( -0.0017 * par ) )
                        f1  = amax1( f1, Tile_Data%rsmin( j ) / rsmax )
                        ftot = MOS_LAI( c,r,j ) * f1 * f2 * f4
                        ftot = MAX( ftot,ftmin )
                        fshelt = 1.0   ! go back to NP89
                        gs     = ftot / ( Tile_Data%rsmin( j ) * fshelt )
                        raw    = MOS_RA( c,r,j ) + 4.503 / MOS_USTAR( c,r,j )
                        ga     = 1.0 / raw
!-- Compute humidity effect according to RH at leaf surf
                        f3 = 0.5 * ( gs - ga + SQRT( ga * ga + ga * gs
     &                   * ( 4.0 * QV( c,r,1 ) / QSS_GRND( c,r ) - 2.0 ) + gs * gs ) ) / gs
                        f3 = MIN ( MAX( f3, f3min ), 1.0 )
                        MOS_RST( c,r,j ) = 1.0 / ( gs * f3 )
                     END IF
                  END DO ! lufrac
               Else If( f_land( c,r ) .Gt. 0.0 ) Then
! Simply weight Rst by land use Rst_min. The weighting must be done as a conductance following Ohm's law
! as the deposition pathways are in parallel
                  max_mos_gst = Sum( Tile_Data%LUFRAC( c,r,: ) / Tile_Data%rsmin)
! Normalize to the mean meterological model grid cell value               
                  DO j = 1, Tile_Data%n_lufrac
                     mos_gst          = 1.0 / RST( c,r ) / ( max_mos_gst * Tile_Data%rsmin( j ) )
                     MOS_RST( c,r,j ) = 1.0 / mos_gst 
                     If( MOS_RST( c,r,j ) * MOS_LAI( c,r,j ) .Lt. Tile_Data%rsmin( j )  .And. 
     &                   MOS_LAI( c,r,j ) .Gt. 0.0 ) Then
                        MOS_RST( c,r,j ) = Tile_Data%rsmin( j ) / MOS_LAI( c,r,j )
                     Else If ( MOS_LAI( c,r,j ) .Eq. 0.0 ) Then
! this does not impact results but reset the RST to the maximum value if there is no veg
                        MOS_RST( c,r,j ) = 1.0e6
                     End If
                  End Do
               END IF ! LWMASK
            END DO ! rows
         END DO ! cols
         Return
         End Subroutine MOS_RSTW

C*********************************************************************************************
C                    MOS_CanWat
C*********************************************************************************************
         Subroutine MOS_CanWat(MOS_VEG, MOS_LAI, WR, MOS_DELTA, WR_AVAIL, Q2, QSS_Grnd,
     &                         Rn, Rc, Wspd, Rgrnd, jdate, jtime)

         Use LSM_Mod
         Use GRID_CONF           ! horizontal & vertical domain specifications
         Use UTILIO_DEFN

         Implicit None

         Real,    Intent( In )  :: MOS_VEG( :,:,: )
         Real,    Intent( In )  :: MOS_LAI( :,:,: )
         Real,    Intent( In )  :: WR( :,: )
         Real,    Intent( In )  :: Q2( :,: )
         Real,    Intent( In )  :: QSS_Grnd( :,: )
         Real,    Intent( In )  :: Rn( :,: )
         Real,    Intent( In )  :: Rc( :,: )
         Real,    Intent( In )  :: Wspd( :,: )
         Real,    Intent( In )  :: Rgrnd( :,: )
         Logical, Intent( In )  :: WR_AVAIL
         Integer, Intent( In )  :: jdate
         Integer, Intent( In )  :: jtime
         Real,    Intent( Out ) :: MOS_DELTA( :,:,: )

         Real            :: rh_grnd
         Integer         :: c,r,j                 ! loop induction variables
         Integer         :: elapsedsec            ! seconds from last precip

         IF ( .NOT. WR_AVAIL ) THEN  ! approx canopy wetness - dew from Wesely
            ! canopy is wet if > trace precip. or moist with light winds
            DO r = 1, nrows
            DO c = 1, ncols
               rh_grnd  = 100.0 * Q2( c,r ) / QSS_Grnd( c,r )
               rh_grnd  = MIN( 100.0, rh_grnd )
               IF ( ( Rn( c,r ) + Rc( c,r ) .GT. 0.025 ) .OR.
     &              ( (0.6 + Wspd( c,r ))*(100.0-rh_grnd) .LE. 19.0 ) ) THEN

                  MOS_DELTA( c,r,: ) = 1.0
                  lstwetdate( c,r )  = jdate
                  lstwettime( c,r )  = jtime

               ELSE

                  IF ( Rgrnd( c,r ) .GT. 5.0 ) THEN  ! day (if at night, persist delta)

                  ! Determine if canopy was recently wet.

                     IF ( ( lstwetdate( c,r ) .GT. 0 ) .AND.
     &                    ( lstwettime( c,r ) .GT. 0 ) ) THEN  ! canopy recently wet

                        elapsedsec = secsdiff ( lstwetdate( c,r ),
     &                                          lstwettime( c,r ),
     &                                          jdate, jtime )

                        IF ( ( elapsedsec .GT.     0 ) .AND.    ! assume canopy stays
     &                       ( elapsedsec .LE. 7200 ) ) THEN    ! wet for 2 h
                           MOS_DELTA( c,r,: ) = 1.0
                        ELSE IF ( ( elapsedsec .GT.  7200 ) .AND.     ! ramp down DELTA
     &                            ( elapsedsec .LT. 10800 ) ) THEN    ! between 2 & 3 h
                           MOS_DELTA( c,r,: ) = ( 10800.0 - FLOAT( elapsedsec ) ) / 3600.0
                        ELSE
                           MOS_DELTA( c,r,: ) = 0.0
                           lstwetdate( c,r )  = 0
                           lstwettime( c,r )  = 0
                        END IF
                     END IF
                  END IF
               END IF
            End Do ! col
            End Do ! row
            Where ( MOS_LAI .LE. 0.0 )
               MOS_DELTA = 0.0
            End Where
         Else
            DO j = 1, Tile_Data%n_lufrac
               Where ( ( WR .LE. 0.0 ) .or. ( MOS_LAI(:,:,j) .LE. 0.0 ) )
                  MOS_DELTA( :,:,j ) = 0.0
               Elsewhere( Tile_Data%LUFRAC( :,:,j ) .Gt. 0.0 )
                  MOS_DELTA( :,:,j ) = WR / ( 0.2e-3 * MOS_VEG(:,:,j) * MOS_LAI(:,:,j) )   ! refer to SiB model
               End Where
            End Do
         End If
         Where( MOS_DELTA .GT. 1.0 ) 
            MOS_DELTA = 1.0
         ElseWhere( MOS_DELTA .LT. 0.0 ) 
            MOS_DELTA = 0.0
         End Where               

         Return
         End Subroutine MOS_CanWat
      
      End Module Mosaic_Mod
