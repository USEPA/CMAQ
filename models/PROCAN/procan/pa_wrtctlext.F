
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
C $Header: /project/yoj/arc/PROCAN/src/driver/pa/pa_wrtctlext.F,v 1.7 2011/11/02 14:34:00 yoj Exp $

      SUBROUTINE PA_WRTCTLEXT

C***********************************************************************
 
C Function: To write the Models-3 Process Analysis Control Include File 
 
C Preconditions: Requires mechanism to have been processed
  
C Key Subroutines/Functions Called: None
 
C Revision History:
C  Prototype created by Jerry Gipson, August, 1996
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Modified Jan, 2006 by J. Young I couldn't resist the temptation to F90-ize the output
C  Aug 2011 Jeff Young: Replaced I/O API include files with IOAPI's M3UTILIO
 
C***********************************************************************
      USE M3UTILIO      ! IOAPI module

      IMPLICIT NONE
      
C Includes:
      INCLUDE 'PA_IPRDEF.EXT'
      INCLUDE 'PA_GLOBAL.EXT'
      INCLUDE 'PA_PARSE.EXT'
      INCLUDE 'PA_VARS.EXT'
      
C Arguments: None
                                        
C Parameters: None

C External Functions: None

C Local Variables:

      CHARACTER(  1 ) :: DOT = '.'  ! Dot character
      CHARACTER( 80 ) :: DELIM      ! Delimiter line output
      CHARACTER( 80 ) :: CEES       ! Line of cees
      CHARACTER( 78 ) :: LINOUT     ! Output line

      INTEGER IOUT                  ! Unit number for include file
      INTEGER :: LINLEN = 78        ! Length of output line
      INTEGER N                     ! Loop index
         
C***********************************************************************

      DO N = 1, 78
         DELIM( N:N ) = DOT
         CEES( N:N ) = 'c'
      END DO     
      DELIM( 1:1 ) = 'C'

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Open and Control Include file and write the header data
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IOUT = JUNIT(  )
      OPEN( UNIT = IOUT, FILE = 'PA_CTL.EXT' )
      WRITE( IOUT, 94000 ) DELIM( 1:72 )
      LINOUT = 'C    INCLUDE FILE   PA_CTL.EXT'
      WRITE( IOUT, 94000 ) LINOUT
      WRITE( IOUT, 94000 ) LINOUT( 1:1 )
      LINOUT = 'C    CONTAINS: Control parameters for Process Analysis'
      WRITE( IOUT, 94000 ) LINOUT
      WRITE( IOUT, 94000 ) LINOUT( 1:1 )
      LINOUT = 'C    DEPENDENT UPON: Mechanism Input file'
      WRITE( IOUT, 94000 ) LINOUT
      WRITE( IOUT, 94000 ) LINOUT( 1:1 )
      LINOUT = 'C    CREATED BY: Process Analysis Control Program'
      WRITE( IOUT, 94000 ) LINOUT
      LINOUT = 'C                ' // TDATOUT
      WRITE( IOUT, 94000 ) LINOUT
      WRITE( IOUT, 94000 ) LINOUT( 1:1 )
      WRITE( IOUT, 94000 ) DELIM( 1:72 )
      LINOUT = ''
      WRITE( IOUT, 94000 ) LINOUT

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write the control parameters
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!     WRITE( IOUT, 94000 )
      WRITE( IOUT, 94000 ) CEES( 1:72 )
      WRITE( IOUT, 94020 )
!     WRITE( IOUT, 94000 ) CEES( 1:72 )
      WRITE( IOUT, 94040 )      
      WRITE( IOUT, 94000 ) CEES( 1:72 )
      WRITE( IOUT, 94000 ) LINOUT

c..LIPR -- for integrated process rates
!     WRITE( IOUT, 94060 )
      IF ( N_IPR_SPC .GT. 0 ) THEN
         WRITE( IOUT, 94080 )
      ELSE
         WRITE( IOUT, 94100 )
      END IF

c..LIRR -- for integrated reaction rates     
!     WRITE( IOUT, 94120 )
      IF ( LFULLIRR .OR. LPARTIRR ) THEN
         WRITE( IOUT, 94140 )
      ELSE
         WRITE( IOUT, 94160 )
      END IF

      LINOUT = DELIM
      LINOUT( 14:58 ) = ' End of Process Analysis CONTROL' //
     &                    ' Include File '
      WRITE( IOUT, 94000 ) LINOUT
      CLOSE( IOUT )

C*********************** FORMAT Statements *****************************

94000 FORMAT( A )     
94020 FORMAT( 
     & 'c  The following variables are used to control calls to', 
     &     ' the Integrated' /
     & 'c  Process Rate and Integrated Reaction Rate routines:' )
94040 FORMAT( 
     & 'c     LIPR = Flag for calculating Integrated Process Rates' /
     & 'c     LIRR = Flag for calculating Integrated Reaction Rates' )
!4060 FORMAT( //6X, 'LOGICAL LIPR' )
94080 FORMAT(   6X, 'LOGICAL, PARAMETER :: LIPR = .TRUE.' / )
94100 FORMAT(   6X, 'LOGICAL, PARAMETER :: LIPR = .FALSE.' / )
!4120 FORMAT(   6X, 'LOGICAL LIRR' )
94140 FORMAT(   6X, 'LOGICAL, PARAMETER :: LIRR = .TRUE.' / )
94160 FORMAT(   6X, 'LOGICAL, PARAMETER :: LIRR = .FALSE.' / )

      END
