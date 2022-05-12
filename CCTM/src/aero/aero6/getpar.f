
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
      Subroutine getpar( fixed_sg  )

C  Calculates the 3rd moments (M3), masses, aerosol densities, and
C  geometric mean diameters (Dg) of all 3 modes, and the natural logs of
C  geometric standard deviations (Sg) of the Aitken and accumulation modes.

C  The logical variable, WET_MOMENTS_FLAG, dictates whether the
C  calculations in GETPAR are to assume that the aerosol is "wet" or
C  "dry."  In the present context, a "wet" aerosol consists of all
C  chemical components of the aerosol.  A "dry" aerosol excludes
C  particle-bound water and also excludes semivol secondary organic aerosol.

C  NOTE! 2nd moment concentrations (M2) are passed into GETPAR in the
C  CBLK array and are modified within GETPAR only in the event that
C  the Sg value of a given mode has gone outside of the acceptable
C  range (1.05 to 2.50).  The GETPAR calculations implicitly assume
C  that the input value of M2 is consistent with the input value of
C  WET_MOMENTS_FLAG.  If, for example, the input M2 value was calculated
C  for a "dry" aerosol and the WET_MOMENTS_FLAG is .TRUE., GETPAR would
C  incorrectly adjust the M2 concentrations!
C  
C  Outputs: 
C    moment3_conc  third moment, proportional to volume [ m3/m3 ]
C    moment2_conc  second moment, prop. to surface area [ m2/m3 ] 
C                     (adjusted if standard dev. hits limit)
C    aeromode_dens [ kg/m3 ]
C    aeromode_lnsg log of geometric standard deviation
C    aeromode_diam geometric mean diameter [ m ]
C    aeromode_mass mass concentration: [ ug / m**3 ]
C
C SH  03/10/11 Renamed met_data to aeromet_data
C HP and BM 4/2016: Updated use of wet_moments_flag which is now
C    available through AERO_DATA consistent with the moments it refers to
C-----------------------------------------------------------------------

      Use aero_data, only : wet_moments_flag, moment3_conc, moment2_conc, moment0_conc,
     &                       aeromode_dens, aeromode_lnsg, aeromode_diam, aeromode_mass,
     &                       min_dg_dry, min_dg_wet, min_sigma_g, max_sigma_g, n_mode, 
     &                       aerospc, aero_missing, aerospc_conc, aeromode, n_aerospc
      Use aeromet_data, only : f6pi   ! Includes CONST.EXT

      Implicit None

C Arguments:
      Logical, Intent( In ) :: fixed_sg  ! If TRUE, then the second moment is modified 
                                         ! during each call in order to preserve the 
                                         ! standard deviation at the current value.
                                         !
                                         ! If FALSE, then the standard deviation is 
                                         ! recalculated to be consistent with the current 
                                         ! combination of the 0th, 2nd and 3rd moments.
                                         ! During this calculation, standard deviation is 
                                         ! limited by parameters in AERO_DATA (min_sigma_g 
                                         ! and max_sigma_g)

C Local Variables:
      Real( 8 ) :: xxm0        ! temporary storage of moment 0 conc's
      Real( 8 ) :: xxm2        ! temporary storage of moment 2 conc's
      Real( 8 ) :: xxm3        ! temporary storage of moment 3 conc's
      Real( 8 ) :: xfsum       ! (ln(M0)+2ln(M3))/3; used in Sg calcs
      Real( 8 ) :: lxfm2       ! ln(M2); used in Sg calcs
      Real( 8 ) :: l2sg        ! square of ln(Sg); used in diameter calcs
      Real      :: es36        ! exp(4.5*l2sg); used in diameter calcs

      Real( 8 ), Parameter :: one3d = 1.0D0 / 3.0D0
      Real( 8 ), Parameter :: two3d = 2.0D0 / 3.0D0

      Real,      Parameter :: one3  = 1.0 / 3.0
      Real,      Parameter :: densmin = 1.0E03  ! minimum particle density [ kg/m**3 ]

      Real( 8 ), Save :: minl2sg( n_mode )   ! min value of ln(sg)**2 for each mode
      Real( 8 ), Save :: maxl2sg( n_mode )   ! max value of ln(sg)**2 for each mode

      Real( 8 ) :: factor
      Real( 8 ) :: species_mass
      Real( 8 ) :: sumM3
      Real( 8 ) :: sumMass
      Integer   :: n, spc   ! loop counters

      Logical,   Save :: FirsTime = .True.

C-----------------------------------------------------------------------

      If ( FirsTime ) Then
          ! Set bounds for ln(Sg)**2
          minl2sg = Real( Log( min_sigma_g ) ** 2, 8 )
          maxl2sg = Real( Log( max_sigma_g ) ** 2, 8 )
          FirsTime = .False.
      End If

C *** Calculate aerosol 3rd moment concentrations [ m**3 / m**3 ]

      Do n = 1, n_mode
         sumM3   = 0.0d0
         sumMass = 0.0d0

         Do spc = 1, n_aerospc
            If ( aerospc( spc )%tracer .Or. aero_missing(spc,n) .Or. 
     &         ( aerospc( spc )%no_M2Wet .AND. .Not. wet_moments_flag ) ) Cycle

            factor       = Real( 1.0E-9 * f6pi / aerospc( spc )%density, 8 )
            species_mass = Real( aerospc_conc( spc,n ), 8 )
            sumM3        = sumM3   + factor * species_mass
            sumMass      = sumMass + species_mass
         End Do

         moment3_conc( n )  = Max ( Real( sumM3 ), aeromode( n )%min_m3conc )
         aeromode_mass( n ) = Real( sumMass )
      End Do

C *** Calculate modal average particle densities [ kg/m**3 ]
      aeromode_dens = 1.0E-9 * f6pi * aeromode_mass / moment3_conc
      Where( aeromode_dens .Lt. densmin )
         aeromode_dens = densmin
      End Where

C *** Calculate geometric standard deviations as follows:
c        ln^2(Sg) = 1/3*ln(M0) + 2/3*ln(M3) - ln(M2)
c     NOTES:
c      1. Equation 10-5a of [Binkowski:1999] and Equation 5a of
c         Binkowski&Roselle(2003) contain typographical errors.
c      2. If the square of the logarithm of the geometric standard
c         deviation is out of an acceptable range, reset this value and
c         adjust the second moments to be consistent with this value.
c         In this manner, M2 is artificially increased when Sg exceeds
c         the maximum limit.  M2 is artificially decreased when Sg falls
c         below the minimum limit.

      Do n = 1, n_mode
         xxm0 = Real( moment0_conc( n ), 8 )
         xxm3 = Real( moment3_conc( n ), 8 )
         xfsum = one3d * Log( xxm0 ) + two3d * Log( xxm3 )

         if ( fixed_sg ) then
            l2sg  = Real( aeromode_lnsg( n ) ** 2, 8)

         else
            xxm2  = Real( moment2_conc( n ), 8 )

            lxfm2 = Log( xxm2 )
            l2sg  = xfsum - lxfm2

            l2sg  = Max( l2sg, minl2sg( n ) )
            l2sg  = Min( l2sg, maxl2sg( n ) )

         end if

         lxfm2 = xfsum - l2sg
         moment2_conc( n )  = Real( Exp ( lxfm2 ) )
         aeromode_lnsg( n ) = Real( Sqrt( l2sg ) )

         ES36 = Real( Exp( 4.5d0 * l2sg ) )

         ! Implement a lower-bound on the number concentration that
         ! constrains the mean diameter of each mode to less than 100 um
         moment0_conc( n ) = Max(moment0_conc( n ),
     &                           moment3_conc( n )/(1.0e-12 * es36) )

         aeromode_diam( n ) = ( moment3_conc( n ) / 
     &                          ( moment0_conc( n ) * es36 ) ) ** one3 

      End Do

      Return
      End Subroutine getpar

