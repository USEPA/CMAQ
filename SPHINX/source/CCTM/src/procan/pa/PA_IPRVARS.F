
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
      MODULE PA_IPRVARS

C Contains: data used in the Process Analysis Control Program

C Dependent Upon: None

C Revision History:
C......................................................................

      USE PA_GLOBAL, Only: MXSPEC
      USE PA_VARS, Only: MAXTERMS

      INTEGER, PARAMETER :: MAXIPRSPC = 600    ! Max no. of IPR species

      INTEGER, PARAMETER :: MAXIPROUT = MAXIPRSPC * 10   ! Max no. IPR outputs


      CHARACTER(  4 ) :: IPR_SPTYPE( MAXIPRSPC )   ! Type of mass balance species
      CHARACTER( 16 ) :: IPR_SPNAM ( MAXIPRSPC )   ! Name of mass balance species
      CHARACTER( 16 ) :: IPR_OPNAME( MAXIPRSPC,MAXTERMS )  ! Input IPR op names

      INTEGER :: N_IPR_OPS ( MAXIPRSPC )   ! No. of input IPR operation names
      INTEGER :: IPR_SPNUM ( MAXIPRSPC )   ! Species or family-name number

      END MODULE PA_IPRVARS
