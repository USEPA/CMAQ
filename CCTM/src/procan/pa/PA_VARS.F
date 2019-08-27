
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
      MODULE PA_VARS
 
C Contains: IRR data used in the Process Analysis Control Program
 
C Dependent Upon: None
 
C Revision History:
C  Prototype created by Jerry Gipson, July, 1996
C  Revised by Jerry Gipson April, 1997 to add the ADJC process and replace
C  NSPCSD with NSPCS for dimensioning
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Modified May 2002 by Jerry Gipson to increase number of reaction lines (for saprc99)
C  Aug 2011 Jeff Young: Removed domain data
C  Jun 2016 Jeff Young: made PA_VARS.EXT int this module
 
C......................................................................
      USE PA_GLOBAL, Only: MXSPEC, MXRXNS
      USE CGRID_SPCS, Only:
     &          CGRID_SPCS_INIT,
     &          N_GC_SPC, GC_SPC,
     &          N_AE_SPC, AE_SPC,
     &          N_NR_SPC, NR_SPC,
     &          N_TR_SPC, TR_SPC

      IMPLICIT NONE

C Parameters:

      INTEGER, PARAMETER :: MAXFAMLYS = 150     ! Max no. of families

      INTEGER, PARAMETER :: MAXRXNSUM = 150     ! Max no. of reaction sums

      INTEGER, PARAMETER :: MAXCYCLES = 150     ! Max no. of cycles

      INTEGER, PARAMETER :: MAXIRROUT = 900     ! Max number of IRR outputs

      INTEGER, PARAMETER :: MAXDEFNAMES =       ! Max no. of defined names
     &                                    MAXFAMLYS
     &                                  + MAXRXNSUM
     &                                  + MAXCYCLES
     &                                  + MAXIRROUT

      INTEGER, PARAMETER :: MAXRXTERMS = 300    ! Max no. of terms in a reaction sum
      
      INTEGER, PARAMETER :: MAXCYTERMS = 300    ! Max no. of rxns in a cycle

      INTEGER, PARAMETER :: MAXOUTTERMS = 300   ! Max no. of terms in an IRR output

      INTEGER, PARAMETER :: MAXTERMS = 900      ! Generic max no. of terms

      INTEGER, PARAMETER :: MAXFAMMEM = MXSPEC  ! Max no of members in any family

      INTEGER, PARAMETER :: MXMECHLINE = 30

      INTEGER, PARAMETER :: MXFAMLINE = 15

C Common Variables:
      CHARACTER(  4 ) :: CYSPTYP( MAXCYCLES )              ! Type of name for cycle
      CHARACTER(  4 ) :: OUTTYPE ( MAXIRROUT,MAXOUTTERMS ) ! Output term type
      CHARACTER(  4 ) :: OUTOP2  ( MAXIRROUT,MAXOUTTERMS ) ! Output AND/OR keyword
      CHARACTER(  4 ) :: OUTS1TYP( MAXIRROUT,MAXOUTTERMS ) ! Name type for species 1 
      CHARACTER(  4 ) :: OUTS2TYP( MAXIRROUT,MAXOUTTERMS ) ! Name type for species 2
      CHARACTER(  4 ) :: OUTS3TYP( MAXIRROUT,MAXOUTTERMS ) ! Name type for species 3

      CHARACTER(  7 ) :: OUTPNFLAG( MAXIRROUT,MAXOUTTERMS ) ! Flag to include
                                                            ! only if sum is + or -

      CHARACTER( 16 ) :: FAMNAME  ( MAXFAMLYS )  ! Name of Family
      CHARACTER( 16 ) :: RXSUMNAME( MAXRXNSUM )  ! Name assigned to reaction sum
      CHARACTER( 16 ) :: CYCLNAME ( MAXCYCLES )  ! Name assigned to reaction sum
      CHARACTER( 16 ) :: CYCLSPEC ( MAXCYCLES )  ! Species to cycle if so defined

      CHARACTER( 16 ) :: FAMMEMNAM( MAXFAMLYS,MAXFAMMEM )  ! Name of family member
      CHARACTER( 16 ) :: RXSUMLBL ( MAXRXNSUM,MAXRXTERMS ) ! Rx no. of terms in sum
      CHARACTER( 16 ) :: CYRXLBL  ( MAXCYCLES,MAXCYTERMS ) ! Rx no. of cycle terms
      CHARACTER( 16 ) :: OUTRXLBL ( MAXIRROUT,MAXOUTTERMS ) ! Reaction labels of 
                                                            ! terms in command
      CHARACTER( 16 ) :: OUTSPEC1 ( MAXIRROUT,MAXOUTTERMS ) ! Species 1 name for  
                                                            ! output operator
      CHARACTER( 16 ) :: OUTSPEC2 ( MAXIRROUT,MAXOUTTERMS ) ! Species 2 name for  
                                                            ! output operator
      CHARACTER( 16 ) :: OUTSPEC3 ( MAXIRROUT,MAXOUTTERMS ) ! Species 3 name for  
                                                            ! output operator
      CHARACTER( 16 ), SAVE :: IRRNAME_TMP( MAXIRROUT ) ! for IRRNAME

      CHARACTER( 28 ) :: TDATOUT    ! Output clock time and date

      CHARACTER( 60 ), SAVE :: IRRDESC_TMP( MAXIRROUT ) ! for IRRDESC

      CHARACTER( 80 ) :: MECLINE( MXRXNS,MXMECHLINE )    ! Compressed reaction lines
      CHARACTER( 80 ) :: FAMLINE( MAXFAMLYS,MXFAMLINE )  ! Compressed family lines

      INTEGER :: NFAMLYS                 ! Number of families pdefined
      INTEGER :: NRXSUMS                 ! Number of reaction sums defined
      INTEGER :: NCYCLES                 ! Number of cycles defined
      INTEGER :: NIRROUT                 ! Number of outputs IRR fields requested
      INTEGER :: FAMNUMLNS ( MAXFAMLYS ) ! Number of lines for family
      INTEGER :: MECNUMLNS (    MXRXNS ) ! Number of lines for reaction
      INTEGER :: NUMFAMMEM ( MAXFAMLYS ) ! Number of members of each family
      INTEGER :: NRXTERMS  ( MAXRXNSUM ) ! Number of terms in reaction sum
      INTEGER :: NCYTERMS  ( MAXCYCLES ) ! Number of reaction terms in each cycle
      INTEGER :: NIRRTERMS ( MAXIRROUT ) ! Number of terms in each output request
      INTEGER :: NOUTCYCS  ( MAXIRROUT ) ! No. of cycles used in output
      INTEGER :: NOUTFAMS  ( MAXIRROUT ) ! No. of families used in output
      INTEGER :: NOUTRXSUMS( MAXIRROUT ) ! No. of reaction sums used in output
      INTEGER :: NOUTOPS   ( MAXIRROUT ) ! No. of output operators
      INTEGER :: NOUTRXN   ( MAXIRROUT ) ! No. of reaction used in output
      INTEGER :: CYSPNUM   ( MAXCYCLES ) ! Species or family-name number

      INTEGER :: TEMPOUTIND_TMP( MAXTERMS )  ! Init no. of temp IRR terms to add
      INTEGER :: INDXOUTIND_TMP( MAXTERMS )  ! Init IRR output index of temp terms
      INTEGER :: TEMPOUTNEG_TMP( MAXTERMS )  ! Init no. of temp IRR terms to add if neg
      INTEGER :: INDXOUTNEG_TMP( MAXTERMS )  ! Init IRR output index of neg only temp terms
      INTEGER :: TEMPOUTPOS_TMP( MAXTERMS )  ! Init no. of temp IRR terms to add if pos
      INTEGER :: INDXOUTPOS_TMP( MAXTERMS )  ! Init IRR output index of pos only temp terms
      INTEGER :: TEMPTERMS_TMP ( MAXTERMS )  ! Init no of temp IRRs to compute
      INTEGER :: TEMPRXN_TMP   ( MAXTERMS,MAXTERMS )   ! Init Rxn nos. for each temp IRR

      INTEGER :: FAMSPIDS  ( MAXFAMLYS,MAXFAMMEM )   ! Mech spec IDs for fam. mem.
      INTEGER :: RXSUMRN   ( MAXRXNSUM,MAXRXTERMS )  ! Rx numbers of rxsum terms 
      INTEGER :: CYRXNUM   ( MAXCYCLES,MAXCYTERMS )  ! Rx numbers of cycle terms
      INTEGER :: OUTCYCS   ( MAXIRROUT,MAXOUTTERMS ) ! Cycle number of output terms
      INTEGER :: OUTFAMS   ( MAXIRROUT,MAXOUTTERMS )
      INTEGER :: OUTRXSUMS ( MAXIRROUT,MAXOUTTERMS ) ! Rxsum number of output terms 
      INTEGER :: OUTS1NUM  ( MAXIRROUT,MAXOUTTERMS ) ! Species/family name number
      INTEGER :: OUTS2NUM  ( MAXIRROUT,MAXOUTTERMS ) ! Species/family name number
      INTEGER :: OUTS3NUM  ( MAXIRROUT,MAXOUTTERMS ) ! Species/family name number
      INTEGER :: OUTRN     ( MAXIRROUT,MAXOUTTERMS ) ! Input IRR Rxn nos.
      INTEGER :: IRRRXN_TMP( MAXIRROUT,MAXTERMS )  ! Initial IRR Rx nos. excluding
                                                   ! CYCLES and RXNSUMS -> IRRRXN

      REAL    :: COEFOUTIND_TMP( MAXTERMS )        ! Init coefficients for pos or neg init
      REAL    :: COEFOUTNEG_TMP( MAXTERMS )        ! Init coefficients for neg init
      REAL    :: COEFOUTPOS_TMP( MAXTERMS )        ! Init coefficients for pos init


      REAL    :: FAMSC( MAXFAMLYS,MAXFAMMEM )    ! Coefficients for a family member
      REAL    :: RXNSC( MAXRXNSUM,MAXRXTERMS )   ! Coefficients for a sum rxn term
      REAL    :: CYSC ( MAXCYCLES,MAXCYTERMS )   ! Coefficients for cycle terms
      REAL    :: OUTSC( MAXIRROUT,MAXOUTTERMS )  ! Coefficients for output terms
      REAL    :: IRRCOEF_TMP( MAXIRROUT,MAXTERMS ) ! Initial coefficients for IRR Rxns
      REAL    :: TEMPCOEF_TMP( MAXTERMS,MAXTERMS ) ! Initial coefficients for temp IRRs

      END MODULE PA_VARS
