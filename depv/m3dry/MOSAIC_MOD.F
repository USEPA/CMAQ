
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
C $Header: /project/work/rep/arc/CCTM/src/depv/m3dry/MOSAIC_MOD.F,v 1.6 2012/03/28 16:11:09 yoj Exp $

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
      Real, Save, Allocatable :: rcanj      ( :,:,:,: )! stomatal, mesiphyll and cuticular resistances only
      Real, Save, Allocatable :: rsurfj     ( :,:,:,: )! Rsurf for LU j        
      Real, Save, Allocatable :: depvel_gasj( :,:,:,: ) ! deposition velocity by land use type
      Real, Save, Allocatable :: vd_fst_gasj( :,:,:,: ) ! deposition velocity for stomatal and
      Real, Save, Allocatable :: adepvj     ( :,:,:,: ) ! deposition velocity for stomatal and
      
!     Character( 80 ), Save   :: LAND_SCHEME 

C Private variables used in this module and subroutines       
      Integer, Save, Private  :: logdev 
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
C Volatile variables 

      
      Contains

         Subroutine Init_Mosaic( jdate, jtime, ltotg, MOSAIC, ABFLUX )
       
         Use HGRD_DEFN
         Use LSM_Mod
         Use UTILIO_DEFN
       
         Implicit None    

C...include files

         Include SUBST_FILES_ID   ! file name parameters                 
       
         Integer, Intent( In )  :: jdate
         Integer, Intent( In )  :: jtime  
         Integer, Intent( In )  :: ltotg 
         Logical, Intent( In )  :: MOSAIC         
         Logical, Intent( In )  :: ABFLUX         
         Character( 240 )       :: xmsg = ' '
         Character(  16 ), save :: pname = 'Init_Mosaic'
         Integer l
         Integer gxoff, gyoff            ! global origin offset from file
         Integer :: strtcolgc2, endcolgc2, strtrowgc2, endrowgc2
                                                                                                  
! Allocate buffers
         ALLOCATE ( BUFF2D_1( my_ncols,my_nrows ),
     &              BUFF2D_2( my_ncols,my_nrows ), STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating Buffers'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If
         
         If ( .Not. ALLOCATED ( fseas ) ) Then
            ALLOCATE ( fseas( my_ncols,my_nrows ) )
         End If

         If ( .Not. ALLOCATED ( total_frac ) ) Then
            ALLOCATE ( total_frac( my_ncols,my_nrows ) )
         End If

         If ( .Not. ALLOCATED ( sum_mos_frac ) ) Then
            ALLOCATE ( sum_mos_frac( my_ncols,my_nrows ) )
         End If

         If ( .Not. ALLOCATED ( sum_crop_frac ) ) Then
            ALLOCATE ( sum_crop_frac( my_ncols,my_nrows ) )
         End If

         If ( .Not. ALLOCATED ( sum_mos_lai ) ) Then
            ALLOCATE ( sum_mos_lai( my_ncols,my_nrows ) )
         End If

         If ( .Not. ALLOCATED ( sum_nat_lai ) ) Then
            ALLOCATE ( sum_nat_lai( my_ncols,my_nrows ) )
         End If

         If ( .Not. ALLOCATED ( new_sum_nat_lai ) ) Then
            ALLOCATE ( new_sum_nat_lai( my_ncols,my_nrows ) )
         End If

         If ( .Not. ALLOCATED ( lai_crop ) ) Then
            ALLOCATE ( lai_crop( my_ncols,my_nrows ) )
         End If

         If ( .Not. ALLOCATED ( znotc ) ) Then
            ALLOCATE ( znotc( my_ncols,my_nrows ) )
         End If

         If ( .Not. ALLOCATED ( US ) ) Then
            ALLOCATE ( US( my_ncols,my_nrows ) )
         End If

         If ( .Not. ALLOCATED ( VS ) ) Then
            ALLOCATE ( VS( my_ncols,my_nrows ) )
         End If

         If ( .Not. ALLOCATED ( WSPD2 ) ) Then
            ALLOCATE ( WSPD2( my_ncols,my_nrows ) )
         End If

         If ( .Not. ALLOCATED ( RICH ) ) Then
            ALLOCATE ( RICH( my_ncols,my_nrows ) )
         End If

         If ( .Not. ALLOCATED ( rcanj ) ) Then
            ALLOCATE ( rcanj( n_lufrac,ltotg,my_ncols,my_nrows ) )
         End If

         If ( .Not. ALLOCATED ( rsurfj ) ) Then
            ALLOCATE ( rsurfj( n_lufrac,ltotg,my_ncols,my_nrows ) )
         End If

         If ( .Not. ALLOCATED ( depvel_gasj ) ) THEN
            Allocate ( depvel_gasj( n_lufrac,ltotg,my_ncols,my_nrows ) )
         End If                         
         If ( mosaic ) Then
            If ( .Not. ALLOCATED ( vd_fst_gasj ) )
     &         Allocate ( vd_fst_gasj( n_lufrac,ltotg,my_ncols,my_nrows ) )
         End If                         
       
         Return   
          
         End Subroutine Init_Mosaic
       
         Subroutine calc_lai( jday, jtime, logdev )

C***********************************************************************
C  Function:
C     Calculate the lai for each LUC in the gridcell
C  Preconditions:  none
C  Subroutines and Functions Called:  none
C  Revision History:
C***********************************************************************

         Use LSM_Mod
         Use ASX_DATA_MOD

         Implicit None

C Arguments:
         Integer, Intent( In )  :: jday
         Integer, Intent( In )  :: jtime     
         Integer, Intent( In )  :: logdev

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
         Where( MET_DATA%SOIT2 .Lt. 290.0 .And. MET_DATA%SOIT2 .Gt. 282.0 )
            fseas = 1.0 - 0.015625 * ( 290.0 - MET_DATA%SOIT2 ) ** 2
         Elsewhere( MET_DATA%SOIT2 .Ge. 290.0 )
            fseas = 1.0
         Elsewhere
            fseas = 0.0
         End where
C get individual LAIs for LUCs for this date 
         Do j = 1, n_lufrac
            total_frac = total_frac + GRID_DATA%LUFRAC( :,:,j )
         End Do

         argw => Buff2D_1
         argw =  0.0
         Do j = 1, n_lufrac
            Where( GRID_DATA%LUFRAC( :,:,j ) .Gt. 0.0 )
               MOSAIC_DATA%VEG( :,:,j ) = ( ( 1.0 - fseas ) * VEGMN0( j ) + fseas * VEG0( j ) )/100.
            End Where
            If ( cat_lu( j ) .NE. 'AG' .And. cat_lu( j ) .NE. 'AGMOSW'
     &                       .And. cat_lu( j ) .NE. 'AGMOS' ) Then
               Where( GRID_DATA%LUFRAC( :,:,j ) .Gt. 0.0 .And. MET_DATA%LAI .Gt. 0.0 )
                  MOSAIC_DATA%LAI( :,:,j ) = LAIMN0( j ) + fseas * ( LAI0( j ) - LAIMN0( j ) )
                  MOSAIC_DATA%Z0 ( :,:,j )  = Z00( j )
               Elsewhere( GRID_DATA%LUFRAC( :,:,j ) .Gt. 0.0 )
                  MOSAIC_DATA%LAI( :,:,j ) = 0.0
                  MOSAIC_DATA%Z0 ( :,:,j ) = Z00( j )
               End Where
               sum_nat_lai = sum_nat_lai + GRID_DATA%LUFRAC( :,:,j ) * MOSAIC_DATA%LAI( :,:,j )
            End If
            If( cat_lu(j) .Eq. 'AG' ) Then
               sum_crop_frac = sum_crop_frac + GRID_DATA%LUFRAC( :,:,j )
            End If
            If( cat_lu(j) .EQ. 'AGMOS' .Or. cat_lu(j) .EQ. 'AGMOSW' ) Then
               sum_mos_frac = sum_mos_frac + GRID_DATA%LUFRAC( :,:,j )
               sum_mos_lai  = sum_mos_lai  + GRID_DATA%LUFRAC( :,:,j ) * ( LAIMN0(j) + fseas * ( LAI0(j) - LAIMN0(j) ) )
               sum_nat_lai = sum_nat_lai + 0.5 * GRID_DATA%LUFRAC( :,:,j ) * MOSAIC_DATA%LAI( :,:,j )
            End If
            If( cat_lu(j) .Eq. 'WATER') Then
               argw = argw + GRID_DATA%LUFRAC( :,:,j )
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
     &          total_frac * MET_DATA%LAI .Gt. sum_nat_lai ) ! only estimate crop lai for ag
            lai_crop = ( total_frac * MET_DATA%LAI - sum_nat_lai ) / ( sum_crop_frac + 0.5 * sum_mos_frac )
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
            Write(Logdev,*) 'WRF LAI       Max:', Maxval(total_frac * MET_DATA%LAI, Mask = lai_crop .Eq. Maxval(lai_crop)), 'Min:',
     &                                            Minval(total_frac * MET_DATA%LAI, Mask = lai_crop .Eq. Minval(lai_crop, 
     &                                            Mask = sum_mos_frac + sum_crop_frac .gt. 0.0))
            Write(Logdev,*) 'Mos-WRF LAI   Max:', Maxval(total_frac * MET_DATA%LAI -sum_nat_lai,Mask = lai_crop .Eq. Maxval(lai_crop)), 
     &                                    'Min:', Minval(total_frac * MET_DATA%LAI -sum_nat_lai,Mask = lai_crop .Eq. Minval(lai_crop, 
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
               new_sum_nat_lai = total_frac * MET_DATA%LAI -
     &                      ( lai_crop * ( sum_crop_frac + 0.5* sum_mos_frac ) )
            elsewhere( lai_crop .Gt. 3.16 )
               lai_crop = MAXVAL( LAIMN0, MASK = cat_lu .EQ. 'AG' ) + fseas * ( MAXVAL( LAI0 - LAIMN0, MASK =  cat_lu .EQ. 'AG' ) )
               new_sum_nat_lai = total_frac * MET_DATA%LAI -
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
            Write( logdev,* ) 'Old lai: ', Sum(MET_DATA%LAI,Dim=3,
     &                                     mask = correction .Eq. Maxval(correction) )
            Write( logdev,* ) 'correction = ', Maxval(correction)
#endif

            Forall( j = 1:n_lufrac, (cat_lu(j) .NE. 'AG' .AND. cat_lu(j) .NE. 'AGMOS' .AND. cat_lu(j) .NE. 'AGMOSW') )
               MOSAIC_DATA%LAI( :,:,j ) = MOSAIC_DATA%LAI( :,:,j ) * correction
            End Forall
         End If   ! correction needed
         Nullify(correction)

C find z0_crop by finding days past emergence
         d_past_emer => Buff2D_1
         d_past_emer =  0.0
         d_past_emer = ( ( lai_crop ) ** ( 1.0 / 1.923 ) ) / 2.273
         d_past_emer = 0.0184 * 0.0184 - 4.0 * 1.057e-4 * d_past_emer
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

         Forall(j = 1:n_lufrac, cat_lu(j) .Eq. 'AG' .And. Maxval( GRID_DATA%LUFRAC( :,:,j ) ) .Gt. 0.0 )
            MOSAIC_DATA%LAI( :,:,j ) = lai_crop
            MOSAIC_DATA%Z0 ( :,:,j ) = znotc
            MOSAIC_DATA%VEG( :,:,j ) = (( 1.0 - vseas ) * VEGMN0(j) + vseas * VEG0(j) ) / 100.0
         End Forall

         Forall(j = 1:n_lufrac, cat_lu(j) .Eq. 'AGMOS' .OR. cat_lu(j) .Eq. 'AGMOSW'  .And. 
     &                          Maxval( GRID_DATA%LUFRAC( :,:,j )) .Gt. 0.0 )
            MOSAIC_DATA%LAI( :,:,j ) = 0.5 * ( LAIMN0(j) + fseas * ( LAI0(j) - LAIMN0(j) ) ) + 0.5 * lai_crop
            MOSAIC_DATA%Z0 ( :,:,j )  = 0.5 * ( znotc + Z00(j) )
            MOSAIC_DATA%VEG( :,:,j ) = (( 1.0 - vseas ) * VEGMN0(j) + vseas * VEG0(j) ) / 100.0
         End Forall
                  
#ifdef Verbose
         mos_lai => Buff2D_1
         mos_lai =  0.0
         Do j = 1,n_lufrac
            mos_lai = mos_lai + MOSAIC_DATA%LAI( :,:,j ) * GRID_DATA%LUFRAC( :,:,j )
         End Do
         lai_err = MaxVal( mos_lai - MET_DATA%LAI ) / MET_DATA%LAI )
         If ( lai_err .Gt. 0.05 ) Then
            Write( logdev,* ) ' '
            Write( logdev,* ) 'Error in total LAI'
            Write( logdev,* ) 'LAI grid = ', MaxVal(MET_DATA%LAI, 
     &         Mask = lai_err .Eq. mos_lai - MET_DATA%LAI ) / MET_DATA%LAI ), 
     &       ' LAI mosaic = ', MaxVal(mos_lai,
     &         Mask = lai_err .Eq. mos_lai - MET_DATA%LAI ) / MET_DATA%LAI )
            Write( logdev,* ) ' '
         End If
         Nullify( mos_lai )
#endif

         Return

         End Subroutine Calc_LAI      

C*********************************************************************************************
C                    RA_MM5
C*********************************************************************************************

         Subroutine RA_MM5

         Use LSM_Mod
         Use ASX_DATA_MOD

         Implicit None

         Real BETAH_MM5, BH, RICRIT, ROVCP, EP1, ROVG
         Real RICRITI
         Integer c,r,j
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

         US = 0.25 * (MET_DATA%UWIND(1:my_ncols,1:my_nrows,    1) + MET_DATA%UWIND(2:(my_ncols+1),1:my_nrows,    1) +
     &                MET_DATA%UWIND(1:my_ncols,2:(my_nrows+1),1) + MET_DATA%UWIND(2:(my_ncols+1),2:(my_nrows+1),1))
         VS = 0.25 * (MET_DATA%VWIND(1:my_ncols,1:my_nrows,    1) + MET_DATA%VWIND(2:(my_ncols+1),1:my_nrows,    1) +
     &                MET_DATA%VWIND(1:my_ncols,2:(my_nrows+1),1) + MET_DATA%VWIND(2:(my_ncols+1),2:(my_nrows+1),1))

C..... beta method, Lee & Pielke (JAM,May1992)
C-- compute sfc rh from soil moisture
C*     SATURATED FOR WATER SURFACE AND SNOW (ISNOW=1)
         BETAP => Buff2D_1
         HU    => Buff2D_2
         HU    =  1.0
         BETAP =  1.0
         Where( NINT( GRID_DATA%LWMASK ) .Ne. 0 .And. MET_DATA%SNOCOV .Lt. 0.5 .And. 
     &            MET_DATA%SOIM1 .Le. GRID_DATA%WFC )
           BETAP = 0.25 * ( 1.0 - COS( MET_DATA%SOIM1 / GRID_DATA%WFC * PI) )**2 ! BETAP          
           HU    = BETAP + (1.0 - BETAP) * MET_DATA%Q2 / MET_DATA%QSS_GRND          ! HU
         End Where
         Nullify( BETAP ) ! HU is still pointing to Buff2D
         
         QFC   => Buff2D_1

         QFC =  HU * MET_DATA%QSS_GRND 

         Nullify( HU )
         CVIRT => Buff2D_2
C.......... compute the thetav at ground
         CVIRT = ( 1.0 + EP1 * QFC )
         Nullify( QFC )
         TV0 => Buff2D_1
         TV0 = MET_DATA%TEMPG * CVIRT
         Nullify( CVIRT )
         TH0 => Buff2D_2
         TH0 = TV0 * ( 1000.0 / MET_DATA%PRSFC/100 )**ROVCP
         Nullify( TV0 )
C
C-----CALCULATE BULK RICHARDSON NO. OF SURFACE LAYER, ACCORDING TO
C        AKB(1976), EQ(12).
C
         DTHVDZ => Buff2D_2
         DTHVDZ  = Met_Data%THETAV( :,:,1 ) - TH0
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
            RICH  = GRAV / MET_DATA%THETAV( :,:,1 ) / ( 1.0 + 0.61 * Met_Data%QV( :,:,1 ) ) *
     &               MET_DATA%ZH( :,:,1 ) * DTHVDZ / WSPD2
            RICUT => Buff2D_1
            RICUT = 1.0 / ( RICRITI + LOG( MET_DATA%ZH( :,:,1 ) / MOSAIC_DATA%Z0( :,:,j ) ) ) 

C------------------------------------------------------------------
C-- NOTE THAT THE REGIMES USED IN HIRPBL HAVE BEEN CHANGED:
C-- USE PLEIM(2006) TO CALCULATE PSI
            PSIH => Buff2D_1
            Where ( RICH .Ge. RICUT  )
C-----CLASS 1; VERY STABLE CONDITIONS:     Z/L > 1
               PSIH = RICH*LOG( MET_DATA%ZH( :,:,1 ) / MOSAIC_DATA%Z0( :,:,j ) )/(1-RICRITI* RICUT ) ! ZOL
               PSIH =  1.-BETAH_MM5-PSIH
            Elsewhere (RICH .Ge. 0.0 )
C-----CLASS 2; STABLE: for 1 > Z/L >0
               PSIH = RICH*LOG( MET_DATA%ZH( :,:,1 ) / MOSAIC_DATA%Z0( :,:,j ) )/(1-RICRITI*RICH)
               PSIH = -BETAH_MM5 * PSIH ! PSIH
            Elsewhere
C-----CLASS 3 or 4; UNSTABLE:
C           CLASS 4 IS FOR ACM NON-LOCAL CONVECTION (H/L < -3) 
               PSIH = 0.04+0.355*ALOG(LOG( MET_DATA%ZH( :,:,1 ) / MOSAIC_DATA%Z0( :,:,j ))) ! AH
               PSIH = PSIH*ALOG(1.-BH*SQRT(LOG( MET_DATA%ZH( :,:,1 ) / MOSAIC_DATA%Z0( :,:,j )))*RICH) ! PSIH
            End where
            Nullify( RICUT )

            Where ( ( ( NINT(GRID_DATA%LWMASK) .Eq. 0 ) .AND. ( MET_DATA%VEG .Eq. 0.0 ) ) .Or. GRID_DATA%LUFRAC( :,:,j ) .Eq. 1.0 ) 
               MOSAIC_DATA%RA( :,:,j ) = MET_DATA%RA
            Elsewhere( GRID_DATA%LUFRAC( :,:,j ) .Gt. 0.0 )
               MOSAIC_DATA%RA( :,:,j ) = PR0*(LOG( MET_DATA%ZH( :,:,1 ) / MOSAIC_DATA%Z0( :,:,j ))-PSIH)/
     &                           (KARMAN*MOSAIC_DATA%USTAR( :,:,j ))
            End Where
            Nullify(PSIH)
         End Do
         Nullify( DTHVDZ )

         Return
         End Subroutine RA_MM5

C*********************************************************************************************
C                    RA_WRF
C*********************************************************************************************

         Subroutine RA_WRF

         Use ASX_DATA_MOD

         Implicit None

         Integer            :: j

C local volatile variable
         Real, Pointer :: PSIH   ( :,: )

         PSIH => Buff2D_1
         PSIH = 0.0
         Do j = 1,n_lufrac
            Where( MET_DATA%MOLI .Lt. 0.0 ) ! checked against PX
               PSIH = 2.0 * Log( ( Sqrt( 1.0 - gamah * MET_DATA%ZH( :,:,1 ) * MET_DATA%MOLI ) + 1.0 ) / 
     &                              ( Sqrt( 1.0 - gamah * MOSAIC_DATA%Z0( :,:,j ) * MET_DATA%MOLI ) + 1.0 ) )
            Else Where ( ( MET_DATA%ZH( :,:,1 ) - MOSAIC_DATA%Z0( :,:,j ) ) * MET_DATA%MOLI .Le. 1.0 )
               PSIH = -betah * ( MET_DATA%ZH( :,:,1 ) - MOSAIC_DATA%Z0( :,:,j ) ) * MET_DATA%MOLI
            Else Where
               PSIH = 1.0 - betah - ( MET_DATA%ZH( :,:,1 ) - MOSAIC_DATA%Z0( :,:,j ) ) * MET_DATA%MOLI
            End Where
            Where ( ( ( NINT(GRID_DATA%LWMASK) .Eq. 0 ) .AND. ( MET_DATA%VEG .Eq. 0.0 ) ) .Or. GRID_DATA%LUFRAC( :,:,j ) .Eq. 1.0 ) 
               MOSAIC_DATA%RA( :,:,j ) = MET_DATA%RA
            Elsewhere!( GRID_DATA%LUFRAC( :,:,j ) .Gt. 0.0 )
               MOSAIC_DATA%RA( :,:,j ) = pr0 * ( Log( MET_DATA%ZH( :,:,1 ) / MOSAIC_DATA%Z0( :,:,j ) ) - PSIH ) / 
     &                                 ( karman * MOSAIC_DATA%USTAR( :,:,j ) )
            End Where
         End Do
         Nullify( PSIH )
         Return
         End Subroutine RA_WRF
      
      End Module Mosaic_Mod
