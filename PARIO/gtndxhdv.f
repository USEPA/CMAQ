
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
C $Header: /project/work/rep/PARIO/src/gtndxhdv.f,v 1.6 2011/03/30 18:13:00 sjr Exp $

      LOGICAL FUNCTION GTNDXHDV ( FILE, VAR, JDATE, JTIME, VARSIZE,
     &                            NBVS, ENDBUF, VX, NEWVAR ) 
C ....................................................................
 
C  PURPOSE:  This function serves the Models-3 parallel interpolation
C            routine PINTERP3. It finds the index of the file variable
C            VAR in file FILE from the list of file variables with
C            existing read buffers. If VAR is not in the list, the
C            list is extended, dimensions are checked and stored, and
C            buffer pointers are set. Otherwise all that is done is a
C            dimension check against the original stored values.
              
C  RETURN VALUE:  Fails if the buffered variable dimensions are not
C                 equal to their originally stored values).
 
C  REVISION HISTORY: 
C       Original version  6/96 by Al Bourgeois for parallel implementation.
C       Modified 12/15/1997 by Al Bourgeois to set NEWVAR (i.e., fix bug).
C       Modified 04/14/1998 by Al Bourgeois to avoid private M3IO references.
C       Modified 07/14/1998 by AJB for some comments regarding COLDIM, ROWDIM.
C       Modified 10/06/1998 by AJB to replace NCOLS, NROWS, NLAYS with VARSIZE.
C       Modified 12/07/1998 by Al Bourgeois to add EXTERNAL declarations.
C       Modified 07/14/1999 by Al Bourgeois to add TYPE argument.
C       Modified 08/06/1999 by Al Bourgeois to call PM3EXIT if NBVS exceeds
C                           maximum value MXNVARHD.
C       15 May 06 J.Young
C       Modified 02/23/2011 by Shawn Roselle
C          -- Replaced I/O API include files with M3UTILIO; removed
C             deprecated TRIMLEN

C  ARGUMENT LIST DESCRIPTION:
C  M1 = PINTERB_MODULE
C  IN:
C     CHARACTER*16   FILE               ! Name of file to be read
C     CHARACTER*16   VAR                ! File variable name
C     INTEGER        JDATE              ! Current Julian date (YYYYDDD)
C     INTEGER        JTIME              ! Current time (HHMMSS)
C     INTEGER        VARSIZE            ! Local processor size of variable
C     INTEGER        MXNVARHD     M1    ! Max possible buffered file variables
 
C  IN/OUT:
C     INTEGER        NBVS               ! Number of variables in list
C     INTEGER        ENDBUF             ! Offset for end of buffer plus one
C     CHARACTER*33   VLISTHD( MXNVARHD ) M1 ! Dynamic array of variable ids
C     INTEGER        SIZEHD  ( 3*NBVS )  M1 ! Dimensions of buffered variables
C     INTEGER        PTR_COUNT           M1 !
 
C  OUT:
C     INTEGER        BUFPOSHD( 2*NBVS )  M1 ! Offsets into buffer for each variable
C     INTEGER        VX                 ! Index of file variable in VLISTHD
C     LOGICAL        NEWVAR             ! Flag to indicate new variable
 
C  LOCAL VARIABLE DESCRIPTION:  see below
 
C  CALLS:  M3WARN
 
C .......................................................................

      USE PINTERPB_MODULE
      USE M3UTILIO              ! i/o api

      IMPLICIT  NONE

C ARGUMENTS:

      CHARACTER( 16 ) :: FILE       ! Name of file to be read
      CHARACTER( 16 ) :: VAR        ! File variable name
      INTEGER JDATE                 ! Current Julian date (YYYYDDD)
      INTEGER JTIME                 ! Current time (HHMMSS)
      INTEGER VARSIZE               ! Local processor size of variable
      INTEGER NBVS                  ! Number of variables in list
      INTEGER ENDBUF                ! Offset for end of buffer plus one
      INTEGER VX                    ! Index of file variable in VLISTHD
      LOGICAL NEWVAR                ! Flag to indicate new variable

C LOCAL VARIABLES: 

      CHARACTER( 33 ) :: FVNAME     ! Concatenated file_variable name
      CHARACTER( 80 ) :: MSG        ! Buffer for building error messages

C .......................................................................

C Concatenate file and variable names to make a unique string

      FVNAME = TRIM( FILE ) // '.' // TRIM( VAR )

C Get index for file variable

      VX = INDEX1( FVNAME, NBVS, VLISTHD )

      IF ( VX .EQ. 0 ) THEN   ! new variable, extend list, etc.  

         NEWVAR = .TRUE.
         NBVS = NBVS + 1

         IF ( NBVS .GT. MXNVARHD ) THEN
            MSG = 'Max number of buffered file variables exceeded.'
            CALL M3WARN ( 'GTNDXHDV', JDATE, JTIME, MSG )           
            GTNDXHDV = .FALSE.; RETURN
         END IF

         VLISTHD(NBVS) = FVNAME
         VX = NBVS

C Set buffer positions for start:end buffers
!        BUFPOSHD( 0, VX ) = ENDBUF 
!        BUFPOSHD( 1, VX ) = ENDBUF + VARSIZE
         PTR_COUNT = PTR_COUNT + 1
!        BUFPOSHD( 2, VX ) = PTR_COUNT
         BUFPOSHD( VX ) = PTR_COUNT
         ENDBUF = ENDBUF + 2*VARSIZE

C Store variable size
        
         SIZEHD( VX ) = VARSIZE

      ELSE    ! old variable: check dimensions against original values

         NEWVAR = .FALSE.

         IF ( VARSIZE .NE. SIZEHD( VX ) ) THEN
            MSG = 'Variable size is inconsistent with previous values'
            CALL M3WARN ( 'GTNDXHDV', JDATE, JTIME, MSG )           
            GTNDXHDV = .FALSE.; RETURN 
         END IF

      END IF    ! End if new variable

      GTNDXHDV = .TRUE.

      RETURN
      END
