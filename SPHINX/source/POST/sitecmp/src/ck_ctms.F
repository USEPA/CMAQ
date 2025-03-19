
!-----------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in    !
!  continuous development by various groups and is based on information !
!  from these groups: Federal Government employees, contractors working !
!  within a United States Government contract, and non-Federal sources  !
!  including research institutions.  These groups give the Government   !
!  permission to use, prepare derivative works of, and distribute copies!
!  of their work in the CMAQ system to the public and to permit others  !
!  to do so.  The United States Environmental Protection Agency         !
!  therefore grants similar permission to use the CMAQ system software, !
!  but users are requested to provide copies of derivative works or     !
!  products designed to operate in the CMAQ system to the United States !
!  Government without restrictions as to use by others.  Software       !
!  that is used with the CMAQ system but distributed under the GNU      !
!  General Public License or the GNU Lesser General Public License is   !
!  subject to their copyright restrictions.                             !
!-----------------------------------------------------------------------!

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /home/sjr/cvs2git/TOOLS/src/sitecmp/ck_ctms.F,v 1.4 2011/10/21 14:41:31 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C*************************************************************************
C
C  FUNCTION: To check multiple input CTM conc files for consistent
C            header data
C             
C                   
C*************************************************************************
      SUBROUTINE CK_M3FLS()

      USE M3FILES
      USE M3UTILIO

      IMPLICIT NONE     


C..ARGUMENTS:

C..PARAMETERS: None

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      CHARACTER*80  MSG               ! Log message
      CHARACTER*16  PNAME             ! Program Name
      CHARACTER*15  VAL1              ! Character value of real
      CHARACTER*15  VAL2              ! Character value of real

      INTEGER L, N      ! Loop indices
      INTEGER FTYPE1    ! File 1 file type
      INTEGER NCOLS1    ! File 1 number of columns
      INTEGER NROWS1    ! File 1 number of rows
      INTEGER NLAYS1    ! File 1 number of levels
      INTEGER SDATE1    ! File 1 start date
      INTEGER STIME1    ! File 1 start time
      INTEGER TSTEP1    ! File 1 time step
      INTEGER MXREC1    ! File 1 number of time steps
      INTEGER NTHIK1    ! File 1 boundary thickness
      INTEGER GDTYP1    ! File 1 horizontal grid type
      INTEGER VGTYP1    ! File 1 Vertical coordinate type

      LOGICAL LERROR    ! Error Flag
      LOGICAL LSTOP     ! Exit Flag

      REAL P_ALP1       ! File 1 map projection parameter      
      REAL P_BET1       ! File 1 map projection parameter   
      REAL P_GAM1       ! File 1 map projection parameter
      REAL XORIG1       ! File 1 X-origin
      REAL YORIG1       ! File 1 Y-origin
      REAL XCELL1       ! File 1 X-dimension of cells
      REAL YCELL1       ! File 1 Y-dimension of cells
      REAL XCENT1       ! File 1 X-center of coordinate system
      REAL YCENT1       ! File 1 Y-center of coordinate system
      REAL VGTOP1       ! File 1 model top

      REAL VGLVS1( MXLAYS3 + 1 )     ! File vertical layer heights
   
C**********************************************************************
      DATA PNAME / 'CK_M3FLS' /

      LSTOP = .FALSE.

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get header data for CTM file 1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( .NOT. DESC3 ( M3_FLNAME( 1 ) ) ) THEN
         MSG = 'Could not read DESC of  ' // M3_FLNAME( 1 ) 
     &         // ' file'
         CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )
      ENDIF

      FTYPE1 = FTYPE3D
      NCOLS1 = NCOLS3D
      NROWS1 = NROWS3D
      NLAYS1 = NLAYS3D
      SDATE1 = SDATE3D
      STIME1 = STIME3D
      TSTEP1 = TSTEP3D
      MXREC1 = MXREC3D
      NTHIK1 = NTHIK3D
      GDTYP1 = GDTYP3D
      P_ALP1 = P_ALP3D
      P_BET1 = P_BET3D
      P_GAM1 = P_GAM3D
      XORIG1 = XORIG3D
      YORIG1 = YORIG3D
      XCELL1 = XCELL3D
      YCELL1 = YCELL3D
      XCENT1 = XCENT3D
      YCENT1 = YCENT3D
      VGTYP1 = VGTYP3D
      VGTOP1 = VGTOP3D
      DO N = 1 , NLAYS3D + 1
         VGLVS1( N ) = VGLVS3D( N )
      ENDDO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check to see that requested variable is on all files
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      DO N = 1, N_M3FILES

         IF( .NOT. DESC3 ( M3_FLNAME( N ) ) ) THEN
            MSG = 'Could not read DESC of  ' // M3_FLNAME( N ) 
     &         // ' file'
            CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )
         ENDIF

      ENDDO


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write out report data
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( N_M3FILES .GT. 1 ) THEN 
         MSG = 'Multiple Models-3 files being used' 
         CALL M3MESG( MSG )
         MSG = 'Files being checked for consistent header data' 
         CALL M3MESG( MSG )
      ENDIF

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Compare header data on file 1 with header data on other CTM files
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO N = 2, N_M3FILES

         WRITE( MSG, '( ''Differences found between files '', A, 
     &                  '' and '', A )' ) 
     &                 TRIM( M3_FLNAME( 1 ) ), 
     &                 TRIM( M3_FLNAME( N ) )

         CALL M3MESG( MSG )

         LERROR = .FALSE.


         IF( .NOT. DESC3 ( M3_FLNAME( N ) ) ) THEN
            MSG = 'Could not read DESC of  ' // M3_FLNAME( N ) 
     &         // ' file'
            CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )
         ENDIF
         
         IF( FTYPE1 .NE. FTYPE3D ) THEN
            WRITE( LOGUNIT, 94020 ) FTYPE1, N, FTYPE3D
            LERROR = .TRUE.
         ENDIF

         IF( NCOLS1 .NE. NCOLS3D ) THEN
            WRITE( LOGUNIT, 94040 ) NCOLS1, N, NCOLS3D
            LERROR = .TRUE.
         ENDIF

         IF( NROWS1 .NE. NROWS3D ) THEN
            WRITE( LOGUNIT, 94060 ) NROWS1, N, NROWS3D
            LERROR = .TRUE.
         ENDIF

         IF( NLAYS1 .NE. NLAYS3D ) THEN
            WRITE( LOGUNIT, 94080 ) NLAYS1, N, NLAYS3D
            LERROR = .TRUE.
         ENDIF

         IF( TSTEP1 .NE. TSTEP3D ) THEN
            WRITE( LOGUNIT, 94140 ) TSTEP1, N, TSTEP3D
            LERROR = .TRUE.
         ENDIF

         IF( NTHIK1 .NE. NTHIK3D ) THEN
            WRITE( LOGUNIT, 94180 ) NTHIK1, N, NTHIK3D
            LERROR = .TRUE.
         ENDIF

         IF( GDTYP1 .NE. GDTYP3D ) THEN
            WRITE( LOGUNIT, 94200 ) GDTYP1, N, GDTYP3D
            LERROR = .TRUE.
         ENDIF

         WRITE( VAL1, 94000 ) P_ALP1
         WRITE( VAL2, 94000 ) P_ALP3D
         IF( VAL1 .NE. VAL2 ) THEN
            WRITE( LOGUNIT, 94220 ) P_ALP1, N, P_ALP3D
            LERROR = .TRUE.
         ENDIF

         WRITE( VAL1, 94000 ) P_BET1
         WRITE( VAL2, 94000 ) P_BET3D
         IF( VAL1 .NE. VAL2 ) THEN
            WRITE( LOGUNIT, 94240 ) P_BET1, N, P_BET3D
            LERROR = .TRUE.
         ENDIF

         WRITE( VAL1, 94000 ) P_GAM1
         WRITE( VAL2, 94000 ) P_GAM3D
         IF( VAL1 .NE. VAL2 ) THEN
            WRITE( LOGUNIT, 94260 ) P_GAM1, N, P_GAM3D
            LERROR = .TRUE.
         ENDIF

         WRITE( VAL1, 94000 ) XORIG1
         WRITE( VAL2, 94000 ) XORIG3D
         IF( VAL1 .NE. VAL2 ) THEN
            WRITE( LOGUNIT, 94280 ) XORIG1, N, XORIG3D
            LERROR = .TRUE.
         ENDIF

         WRITE( VAL1, 94000 ) YORIG1
         WRITE( VAL2, 94000 ) YORIG3D
         IF( VAL1 .NE. VAL2 ) THEN
            WRITE( LOGUNIT, 94300 ) YORIG1, N, YORIG3D
            LERROR = .TRUE.
         ENDIF

         WRITE( VAL1, 94000 ) XCELL1
         WRITE( VAL2, 94000 ) XCELL3D
         IF( VAL1 .NE. VAL2 ) THEN
            WRITE( LOGUNIT, 94320 ) XCELL1, N, XCELL3D
            LERROR = .TRUE.
         ENDIF

         WRITE( VAL1, 94000 ) YCELL1
         WRITE( VAL2, 94000 ) YCELL3D
         IF( VAL1 .NE. VAL2 ) THEN
            WRITE( LOGUNIT, 94340 ) YCELL1, N, YCELL3D
            LERROR = .TRUE.
         ENDIF

         WRITE( VAL1, 94000 ) XCENT1
         WRITE( VAL2, 94000 ) XCENT3D
         IF( VAL1 .NE. VAL2 ) THEN
            WRITE( LOGUNIT, 94360 ) XCENT1, N, XCENT3D
            LERROR = .TRUE.
         ENDIF

         WRITE( VAL1, 94000 ) YCENT1
         WRITE( VAL2, 94000 ) YCENT3D
         IF( VAL1 .NE. VAL2 ) THEN
            WRITE( LOGUNIT, 94380 ) YCENT1, N, YCENT3D
            LERROR = .TRUE.
         ENDIF

         WRITE( VAL1, 94000 ) VGTOP1
         WRITE( VAL2, 94000 ) VGTOP3D
         IF( VAL1 .NE. VAL2 ) THEN
            WRITE( LOGUNIT, 94420 ) VGTOP1, N, VGTOP3D
            LERROR = .TRUE.
         ENDIF

         DO L = 1 , NLAYS1 + 1
            WRITE( VAL1, 94000 ) VGLVS1(  L )
            WRITE( VAL2, 94000 ) VGLVS3D( L )
            IF( VAL1 .NE. VAL2 ) THEN
               WRITE( LOGUNIT, 94440 ) L, VGLVS1( L ), N, L,
     &                                 VGLVS3D( L )      
               LERROR = .TRUE.
            ENDIF
         ENDDO
 
         IF( .NOT. LERROR ) THEN
            WRITE( LOGUNIT, 94460 )
         ELSE
            LSTOP = .TRUE.
         ENDIF


      ENDDO

      IF( LSTOP ) THEN
          MSG = 'Missing variable or file inconsistencies detected: stopping' 
          CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )
      ENDIF
     
      RETURN

C************************* FORMAT STATEMENTS ***************************

94000 FORMAT( E15.5 )
94020 FORMAT(10X, 'FTYPE1 = ', I3, '   FTYPE', I1, ' = ', I3 )
94040 FORMAT(10X, 'NCOLS1 = ', I3, '   NCOLS', I1, ' = ', I3 )
94060 FORMAT(10X, 'NROWS1 = ', I3, '   NROWS', I1, ' = ', I3 )
94080 FORMAT(10X, 'NLAYS1 = ', I3, '   NLAYS', I1, ' = ', I3 )
94100 FORMAT(10X, 'SDATE1 = ', I3, '   SDATE', I1, ' = ', I3 )
94120 FORMAT(10X, 'STIME1 = ', I3, '   STIME', I1, ' = ', I3 )
94140 FORMAT(10X, 'TSTEP1 = ', I3, '   TSTEP', I1, ' = ', I3 )
94160 FORMAT(10X, 'MXREC1 = ', I3, '   MXREC', I1, ' = ', I3 )
94180 FORMAT(10X, 'NTHIK1 = ', I3, '   NTHIK', I1, ' = ', I3 )
94200 FORMAT(10X, 'GDTYP1 = ', I3, '   GDTYP', I1, ' = ', I3 )
94220 FORMAT(10X, 'P_ALP1 = ', 1PE12.5, '   P_ALP', I1, ' = ', 1PE12.5 )
94240 FORMAT(10X, 'P_BET1 = ', 1PE12.5, '   P_BET', I1, ' = ', 1PE12.5 )
94260 FORMAT(10X, 'P_GAM1 = ', 1PE12.5, '   P_GAM', I1, ' = ', 1PE12.5 )
94280 FORMAT(10X, 'XORIG1 = ', 1PE12.5, '   XORIG', I1, ' = ', 1PE12.5 )
94300 FORMAT(10X, 'YORIG1 = ', 1PE12.5, '   YORIG', I1, ' = ', 1PE12.5 )
94320 FORMAT(10X, 'XCELL1 = ', 1PE12.5, '   XCELL', I1, ' = ', 1PE12.5 )
94340 FORMAT(10X, 'YCELL1 = ', 1PE12.5, '   YCELL', I1, ' = ', 1PE12.5 )
94360 FORMAT(10X, 'XCENT1 = ', 1PE12.5, '   XCENT', I1, ' = ', 1PE12.5 )
94380 FORMAT(10X, 'YCENT1 = ', 1PE12.5, '   YCENT', I1, ' = ', 1PE12.5 )
94420 FORMAT(10X, 'VGTOP1 = ', 1PE12.5, '   VGTOP', I1, ' = ', 1PE12.5 )
94440 FORMAT(10X, 'VGLVS1(', I2, ' ) = ', 1PE12.5, '   VGLVS', I1,
     &                  '(', I2, ' ) = ', 1PE12.5 )
94445 FORMAT(10X, 'Variable ', A, ' not on file ', A )
94460 FORMAT(10X, 'NONE' )

      END
