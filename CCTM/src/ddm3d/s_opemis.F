
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
C $Header: /project/yoj/arc/CCTM/src/emis/emis/opemis.F,v 1.3 2011/10/21 16:10:47 yoj Exp $
 
C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%
 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE S_OPEMIS ( JDATE, JTIME, NEMIS, EM_TRAC, CONVEM, 
     &                    LAYSNUM, EMISFILE, FDATE )

C  7 Mar 02 - J.Young: add units string variations
C 29 Oct 05 - J.Young: dyn. layers
C 19 Feb 08 - David Wong: add EM_TRAC = .TRUE. when EMIS_TRAC_1 exists
C 21 Jun 10 - J.Young: convert for Namelist redesign
C 16 Feb 11 - S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                        removed deprecated TRIMLEN
C 21 Jun 13 S.Napelenok: ddm-3d implementation for cmaq5.0.1

      USE VGRD_DEFN           ! vertical layer specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:

      INTEGER      JDATE      ! current model date, coded YYYYDDD
      INTEGER      JTIME      ! current model time, coded HHMMSS
      INTEGER      NEMIS      ! no. of gas chem emissions species for vdiff
      LOGICAL      EM_TRAC    ! are there tracer emissions?
      REAL         CONVEM     ! conversion for emissions rates
c     INTEGER      EMLAYS     ! emissions layers from file
      INTEGER      LAYSNUM     ! emissions layers from file
      CHARACTER( 16 ) :: EMISFILE ! emissions file to read

C External Functions:

      INTEGER, EXTERNAL :: SETUP_LOGDEV

C Local variables:

      CHARACTER( 16 ) :: PNAME = 'S_OPEMIS'
      CHARACTER( 96 ) :: XMSG
      CHARACTER( 16 ) :: UNITSCK

      LOGICAL ::   WRFLG = .FALSE.
      INTEGER      LOGDEV
      INTEGER      V, N, S        ! induction variables

      INTEGER      FDATE


C-----------------------------------------------------------------------
 
!     LOGDEV = INIT3()
      LOGDEV = SETUP_LOGDEV()

      LAYSNUM = 0

C Open the tracer emissions file

c     IF ( N_TR_EMIS .GT. 0 ) THEN

c        IF ( .NOT. OPEN3( EMIS_TRAC_1, FSREAD3, PNAME ) ) THEN

c           XMSG = 'Could not open '// EMIS_TRAC_1 // ' file'
c           CALL M3MESG( XMSG )
c           EM_TRAC = .FALSE.

c        ELSE

c           EM_TRAC = .TRUE.
c           IF ( .NOT. DESC3( EMIS_TRAC_1 ) ) THEN
c              XMSG = 'Could not get '// EMIS_TRAC_1 // ' file description'
c              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
c           END IF

c           EMLAYS = NLAYS3D
c
c           V = INDEX1( TR_EMIS( 1 ), NVARS3D, VNAME3D )
c           IF ( V .NE. 0 ) THEN
c              UNITSCK = UNITS3D( V )
c           ELSE
c              XMSG = 'Emissions species '
c    &              // TRIM( TR_EMIS( 1 ) )
c    &              // ' not found on ' // EMIS_1
c              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
c           END IF

c           DO N = 2, N_TR_EMIS
c              V = INDEX1( TR_EMIS( N ), NVARS3D, VNAME3D )
c              IF ( V .NE. 0 ) THEN
c                 IF ( UNITS3D( V ) .NE. UNITSCK ) THEN
c                    XMSG = 'Units not uniform on ' // EMIS_TRAC_1
c                    CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
c                 END IF
c              ELSE
c                 XMSG = 'Emissions species '
c    &                 // TRIM( TR_EMIS( N ) )
c    &                 // ' not found on ' // EMIS_TRAC_1
c                 CALL M3MESG( XMSG )
c              END IF
c           END DO

c        END IF

c     END IF   ! tracer emissions

!     IF ( NEMIS + N_AE_EMIS + N_NR_EMIS + N_TR_EMIS .GT. 0 ) THEN
      IF ( NEMIS + N_NR_EMIS .GT. 0 ) THEN

C Open the emissions file (for gas chem, aerosols and non-reactive species)

         IF ( .NOT. OPEN3( EMISFILE, FSREAD3, PNAME ) ) THEN
            XMSG = 'Could not open '// EMISFILE // ' file'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. DESC3( EMISFILE ) ) THEN
            XMSG = 'Could not get '// EMISFILE // ' file description'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

         FDATE = SDATE3D

         IF ( LAYSNUM .EQ. 0 ) THEN
            LAYSNUM = NLAYS3D
         ELSE
            IF ( LAYSNUM .NE. NLAYS3D ) THEN
               WRITE( XMSG,'(A, I3, A, I3)' ) 'Tracer No. of emis layers ', LAYSNUM,
     &         ' Different than Gas No. of emis layers: ', NLAYS3D 
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF
         END IF

         DO N = 1, N_GC_EMIS
            V = INDEX1( GC_EMIS( N ), NVARS3D, VNAME3D )
            IF ( V .NE. 0 ) THEN
               UNITSCK = UNITS3D( V )
               S = N + 1
               GO TO 101
            ELSE
               XMSG = 'Emissions species '
     &              // TRIM( GC_EMIS( 1 ) )
     &              // ' not found on ' // EMISFILE
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            END IF
         END DO
         XMSG = ' No emissions species ' // ' found on ' // EMISFILE
         CALL M3WARN( PNAME, JDATE, JTIME, XMSG )

101      CONTINUE

         DO N = S, N_GC_EMIS
            V = INDEX1( GC_EMIS( N ), NVARS3D, VNAME3D )
            IF ( V .NE. 0 ) THEN
               UNITSCK = UNITS3D( V )
               CALL UPCASE ( UNITSCK )
               IF ( UNITSCK .NE. 'MOLES/S'   .AND.
     &              UNITSCK .NE. 'MOLE/S'    .AND.
     &              UNITSCK .NE. 'MOL/S'     .AND.
     &              UNITSCK .NE. 'MOLES/SEC' .AND.
     &              UNITSCK .NE. 'MOLE/SEC'  .AND.
     &              UNITSCK .NE. 'MOL/SEC' ) THEN
                  XMSG = 'GC units incorrect on ' // EMISFILE
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF
            ELSE
               XMSG = 'Emissions species '
     &              // TRIM( GC_EMIS( N ) )
     &              // ' not found on ' // EMISFILE
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            END IF
         END DO

         DO N = 1, N_NR_EMIS
            V = INDEX1( NR_EMIS( N ), NVARS3D, VNAME3D )
            IF ( V .NE. 0 ) THEN
               UNITSCK = UNITS3D( V )
               CALL UPCASE ( UNITSCK )
               IF ( UNITSCK .NE. 'MOLES/S'   .AND.
     &              UNITSCK .NE. 'MOLE/S'    .AND.
     &              UNITSCK .NE. 'MOL/S'     .AND.
     &              UNITSCK .NE. 'MOLES/SEC' .AND.
     &              UNITSCK .NE. 'MOLE/SEC'  .AND.
     &              UNITSCK .NE. 'MOL/SEC' ) THEN
                  XMSG = 'NR units incorrect on ' // EMISFILE
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF
            ELSE
               XMSG = 'Emissions species '
     &              // TRIM( NR_EMIS( N ) )
     &              // ' not found on ' // EMISFILE
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            END IF
         END DO

C Assume units = mol/s across gas and non-reactive species classes

         CONVEM = 1.0E-03  ! assuming gram-moles

      END IF  ! (gas chem or non-react emissions)

      IF ( NEMIS .GT. 0 ) THEN
         WRFLG = .TRUE.
         WRITE( LOGDEV,1001 )
1001     FORMAT( / 10X, 'Gas Chemistry Emissions Processing in',
     &              1X, 'Vertical diffusion ...' )
      END IF

!     IF ( N_AE_EMIS .GT. 0 ) THEN
!        WRFLG = .TRUE.
!        WRITE( LOGDEV,1003 )
1003     FORMAT( / 10X, 'Aerosol Emissions Processing in',
     &              1X, 'Vertical diffusion ...' )
!     END IF

      IF ( N_NR_EMIS .GT. 0 ) THEN
         WRFLG = .TRUE.
         WRITE( LOGDEV,1005 )
1005     FORMAT( / 10X, 'Non-reactives Emissions Processing in',
     &              1X, 'Vertical diffusion ...' )
      END IF

      IF ( N_TR_EMIS .GT. 0 .AND. EM_TRAC ) THEN
         WRFLG = .TRUE.
         WRITE( LOGDEV,1007 )
1007     FORMAT( / 10X, 'Tracer Emissions Processing in',
     &              1X, 'Vertical diffusion ...' )
      END IF

      RETURN
      END
