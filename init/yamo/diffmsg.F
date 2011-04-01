
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/init/yamo/diffmsg.F,v 1.2 2011/04/01 15:41:50 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

       SUBROUTINE DIFFMSG ( VNAME, INT1, INT2 )

C----------------------------------------------------------------------
C Function: To write message for inconsistency between two values
 
C Preconditions: None
 
C Key Subroutines/ unctions Called:
 
C Revision History: Prototype created by Jerry Gipson, September, 1997
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                        removed deprecated TRIMLEN
 
C----------------------------------------------------------------------

      USE UTILIO_DEFN

      IMPLICIT NONE 

C..INCLUDES: None
     
C..ARGUMENTS:
      CHARACTER*(*) VNAME        ! Variable NameName
      CHARACTER*(*) STR1         ! Value of character variable 1
      CHARACTER*(*) STR2         ! Value of character variable 2
      
      INTEGER  INT1         ! Value of integer variable 1
      INTEGER  INT2         ! Value of integer variable 2

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None
      
C..SCRATCH LOCAL VARIABLES:
      CHARACTER*80 MSG
      CHARACTER*20 CHR1      ! Value of variable 1 in character data
      CHARACTER*20 CHR2      ! Value of variable 1 in character data

      INTEGER SV, EV    ! Starting, ending position for variable name
      INTEGER S1, E1    ! Starting, ending position for variable 1
      INTEGER S2, E2    ! Starting, ending position for variable 2

C----------------------------------------------------------------------

         ENTRY INTDIFF ( VNAME, INT1, INT2 )
         SV = LBLANK( VNAME ) + 1
         EV = LEN_TRIM( VNAME ) 
         WRITE( CHR1, 94020 ) INT1
         S1 = LBLANK( CHR1 ) + 1
         E1 = LEN_TRIM( CHR1 )
         WRITE( CHR2, 94020 ) INT2
         S2 = LBLANK( CHR2 ) + 1
         E2 = LEN_TRIM( CHR2 )
         SV = LBLANK( VNAME ) + 1
         EV = LEN_TRIM( VNAME ) 
         MSG = '    Inconsistent values for '// VNAME( SV:EV) //': '
     &         // CHR1( S1:E1 ) // ' versus ' // CHR2( S2:E2 )
         CALL M3MESG( MSG )
         RETURN

         ENTRY CHRDIFF ( VNAME, STR1, STR2 )
         SV = LBLANK( VNAME ) + 1
         EV = LEN_TRIM( VNAME ) 
         S1 = LBLANK( STR1 ) + 1
         E1 = LEN_TRIM( STR1 )
         S2 = LBLANK( STR2 ) + 1
         E2 = LEN_TRIM( STR2 )
         SV = LBLANK( VNAME ) + 1
         EV = LEN_TRIM( VNAME ) 
         MSG = '    Inconsistent values for '// VNAME( SV:EV) //': '
     &         // STR1( S1:E1 ) // ' versus ' // STR2( S2:E2 )
         CALL M3MESG( MSG )
         RETURN


94020    FORMAT(I20)
         END
