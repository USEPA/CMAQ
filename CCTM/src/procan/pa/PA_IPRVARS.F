
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

      CHARACTER( 16 ) :: IPRNAME_TMP( MAXIPROUT )  ! Initial IPR variable names
      CHARACTER( 60 ) :: IPRDESC_TMP( MAXIPROUT )  ! Initial IPR variable descriptions

      INTEGER :: N_IPR_OPS ( MAXIPRSPC )   ! No. of input IPR operation names
      INTEGER :: IPR_SPNUM ( MAXIPRSPC )   ! Species or family-name number

      INTEGER :: SAV2GRD_TMP( MXSPEC )              ! Initial spcs pointer for saved concs
      INTEGER :: IPR2GRD_TMP( MAXIPRSPC,MAXTERMS )  ! Initial pointer to model spcs
      INTEGER :: IPR2SAV_TMP( MAXIPRSPC,MAXTERMS )  ! Initial spcs pointer to saved concs
      REAL    :: SPCOEF_TMP ( MAXIPRSPC,MAXTERMS )  ! Initial spcs coefficient

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
 

      END MODULE PA_IPRVARS
