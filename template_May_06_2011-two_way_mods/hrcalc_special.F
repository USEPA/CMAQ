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
C $Header$

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE HRCALC_SPECIAL

C***********************************************************************
C
C  FUNCTION: To calculate special concentration dependent rate constants
C
R1  PRECONDITIONS: For SAPRC99 family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
R2  REVISION HISTORY: Prototype created by Jerry Gipson, July, 2009
C
C***********************************************************************
      USE HRDATA

      IMPLICIT NONE

C.....INCLUDES: NONE
 
C.....ARGUMENTS: NONE 

C.....PARAMETERS: NONE

C.....EXTERNAL FUNCTIONS: NONE

C.....SAVED VARIABLES:

      CHARACTER( 16 ), SAVE  ::  PNAME = 'HRCALC_SPECIAL'   ! Program name

      REAL, SAVE ::  MNLOSS = 1.0E-30   ! Min value of loss freq
      REAL, SAVE ::  MNCONC = 1.0E-30   ! Min concentration value


C.....LOCAL VARIABLES:

c..Names assigned to special rate constants
R3

c***********************************************************************

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  This subroutine calculates special rate constants that may depend on
c  species concentrations.  The special rate constants are computed first,
c  and then the mechanism specific rate constants are set to the value
c  of the appropriate rate constant
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

S1

  