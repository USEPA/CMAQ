
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

C......................................................................
      MODULE PA_IPRDEF
 
C CONTAINS: Defines science processes contained in the CMAQ and the 
C           IPR operators that can be used
 
C DEPENDENT UPON: Nothing, but if the process names and/or operator
C                 names are changed, the definitions of the 
C                 operators must be re-programmed in pa_setup_ipr.F.
 
C REVISION HISTORY:
C  Prototype created by Jerry Gipson, Sept, 1997 for the targeted CTM
C  Modified June 3, 1998 by Jerry Gipson to add PING to the CTM science processes
C  Modified June 4, 2005 by Jeff Young to deal with Yamo (Yamartino-type)
C  advection scheme 
C  27 Jan 2006, J.Young: change all operator names to be 4 chars - they prepend
C  species names, which must remain less that 12 chars, i.e. OPNM_<__SPECIES__>
C                                                            12345 67890123456
C  27 Jun 2016, J.Young: moved PA_IPRDEF.EXT to PA_IPRDEF.F module
C......................................................................

      USE PA_DEFN, Only: NPRCS, PROCNAME

      IMPLICIT NONE

      INTEGER, PARAMETER :: NOPNAMES = 16  ! Number of IPR_OUTPUT operators

      CHARACTER( 16 ) :: OPNAME( NOPNAMES ) = (/  ! IPR_OUTPUT Operator names
     &                   'ZADV         ',  !  1
     &                   'HADV         ',  !  2
     &                   'HDIF         ',  !  3
     &                   'VDIF         ',  !  4
     &                   'EMIS         ',  !  5
     &                   'DDEP         ',  !  6
     &                   'CLDS         ',  !  7
     &                   'CHEM         ',  !  8
     &                   'COND         ',  !  9
     &                   'COAG         ',  ! 10
     &                   'NPF          ',  ! 11
     &                   'GROW         ',  ! 12
     &                   'AERO         ',  ! 13
     &                   'MADV         ',  ! 14
     &                   'TDIF         ',  ! 15
     &                   'TRNM         ' /)! 16

      END MODULE PA_IPRDEF
