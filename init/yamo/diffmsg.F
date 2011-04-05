
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
C $Header: /project/yoj/arc/CCTM/src/init/yamo/diffmsg.F,v 1.3 2011/04/05 20:59:34 sjr Exp $ 

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
C   04 Apr 11 S.Roselle: updated string blank space removal to use
C                        fortran intrinsic functions
 
C----------------------------------------------------------------------

      USE UTILIO_DEFN

      IMPLICIT NONE 

C..INCLUDES: None
     
C..ARGUMENTS:
      CHARACTER(*) VNAME        ! Variable Name
      CHARACTER(*) STR1         ! Value of character variable 1
      CHARACTER(*) STR2         ! Value of character variable 2
      
      INTEGER  INT1         ! Value of integer variable 1
      INTEGER  INT2         ! Value of integer variable 2

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None
      
C..SCRATCH LOCAL VARIABLES:
      CHARACTER( 80 ) MSG
      CHARACTER( 20 ) CHR1      ! Value of variable 1 in character data
      CHARACTER( 20 ) CHR2      ! Value of variable 1 in character data

C----------------------------------------------------------------------

         ENTRY INTDIFF ( VNAME, INT1, INT2 )
         WRITE( CHR1, 94020 ) INT1
         WRITE( CHR2, 94020 ) INT2
         MSG = '    Inconsistent values for '
     &       // TRIM( ADJUSTL( VNAME ) ) // ': '
     &       // TRIM( ADJUSTL( CHR1  ) ) // ' versus '
     &       // TRIM( ADJUSTL( CHR2  ) )
         CALL M3MESG( MSG )
         RETURN

         ENTRY CHRDIFF ( VNAME, STR1, STR2 )
         MSG = '    Inconsistent values for '
     &       // TRIM( ADJUSTL( VNAME ) ) // ': '
     &       // TRIM( ADJUSTL( STR1  ) ) // ' versus '
     &       // TRIM( ADJUSTL( STR2  ) )
         CALL M3MESG( MSG )
         RETURN

94020    FORMAT(I20)
         END
