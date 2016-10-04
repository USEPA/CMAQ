
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

  
