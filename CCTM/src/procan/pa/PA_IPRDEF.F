
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
C $Header: /project/yoj/arc/PROCAN/src/driver/pa/PA_IPRDEF.EXT,v 1.6 2011/11/02 14:33:55 yoj Exp $

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

!     INTEGER, PARAMETER :: NPROCS = 13    ! Number of CTM science processes

!     CHARACTER( 16 ) :: PROCNAME( NPROCS ) = (/  ! Names of CTM science processes
!    &                   'XADV ',  !  1
!    &                   'YADV ',  !  2
!    &                   'ZADV ',  !  3
!    &                   'ADJC ',  !  4
!    &                   'HADV ',  !  5
!    &                   'HDIF ',  !  6
!    &                   'VDIF ',  !  7
!    &                   'EMIS ',  !  8
!    &                   'DDEP ',  !  9
!    &                   'CLDS ',  ! 10
!    &                   'CHEM ',  ! 11
!    &                   'AERO ',  ! 12
!    &                   'PING '/) ! 13

      INTEGER, PARAMETER :: NOPNAMES = 20  ! Number of IPR_OUTPUT operators

      CHARACTER( 16 ) :: OPNAME( NOPNAMES ) = (/  ! IPR_OUTPUT Operator names
     &                   'XADV         ',  !  1
     &                   'YADV         ',  !  2
     &                   'ZADV         ',  !  3
     &                   'ADJC         ',  !  4
     &                   'HADV         ',  !  5
     &                   'HDIF         ',  !  6
     &                   'VDIF         ',  !  7
     &                   'EMIS         ',  !  8
     &                   'DDEP         ',  !  9
     &                   'CLDS         ',  ! 10
     &                   'CHEM         ',  ! 11
     &                   'AERO         ',  ! 12
!    &                   'XYADV        ',  ! 13
     &                   'ADV2         ',  ! 13
!    &                   'XYZADV       ',  ! 14
     &                   'ADV3         ',  ! 14
!    &                   'HZADV        ',  ! 15
     &                   'MADV         ',  ! 15
!    &                   'TOTADV       ',  ! 16
     &                   'TADV         ',  ! 16
!    &                   'TOTDIF       ',  ! 17
     &                   'TDIF         ',  ! 17
!    &                   'TOTTRAN      ',  ! 18
     &                   'TRAN         ',  ! 18
!    &                   'TOTTRAN_YAMO ',  ! 19
     &                   'TRNM         ',  ! 19
     &                   'PING         '/) ! 20

C  OLD Operator 13-19 definitions:
C      XYADV   = XADV + YADV
C      XYZADV  = XADV + YADV + ZADV
C      HZADV   = HADV + ZADV  = TOTADV for YAMO
C      TOTADV  = XADV + YADV + ZADV + ADJC
C      TOTDIF  = HDIF + VDIF
C      TOTTRAN = XADV + YADV + ZADV + ADJC + HDIF + VDIF
C      TOTTRAN_YAMO = HADV + ZADV + HDIF + VDIF

C  NEW Operator 13-19 definitions:
C      ADV2 = XADV + YADV
C      ADV3 = XADV + YADV + ZADV
C      MADV = HADV + ZADV  = TOTADV for YAMO, "Mass-conserving ADV"
C      TADV = XADV + YADV + ZADV + ADJC
C      TDIF = HDIF + VDIF
C      TRAN = XADV + YADV + ZADV + ADJC + HDIF + VDIF
C      TRNM = HADV + ZADV + HDIF + VDIF = total "TRaN for Mass-conserving adv"

      END MODULE PA_IPRDEF
