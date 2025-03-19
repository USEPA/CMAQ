
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
      REAL FUNCTION ZFDBC (C1, C2, V1, V2)

c Zero Flux Divergence Boundary Condition (See Jon Pleim's JGR (1991) paper)
c To eliminate reflections and other boundary anomalies
C Problem if V1 is outflow, but V2 is inflow

      IMPLICIT NONE
      REAL SMALL
      PARAMETER (SMALL = 1.0E-03 )   ! for small wind speed (m/s)
      REAL C1, C2, V1, V2
 
      IF ( ABS( V1 ) .GE. SMALL ) THEN
         IF ( V1 * V2 .GT. 0.0 ) THEN
            ZFDBC = MAX (0.0, C1 - V2 / V1 * (C2 - C1)) 
         ELSE
            ZFDBC = C1         ! nothing changes for wind divergence at edge
         END IF
      ELSE
         ZFDBC = C1            ! nothing changes for small wind speed
      END IF

      RETURN
      END
