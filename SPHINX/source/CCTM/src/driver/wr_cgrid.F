
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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WR_CGRID ( CGRID, JDATE, JTIME, TSTEP )

C Save the instantaneous state of CGRID in an I/O-API "circular buffer"
C file to use for restart/continuation for subsequent simulation. This
C file will replace the CONC file for such use, allowing users to reduce
C the number of variables and/or layers saved to the CONC file.

C Revision History:
C      May 06 J.Young: initial
C      Feb 08 J.Young: fix VDESC3D bug
C   21 Jun 10 J.Young: convert for Namelist redesign
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                        removed deprecated TRIMLEN
C   12 Aug 15 D.Wong:  Replaced MYPE with IO_PE_INCLUSIVE and added code
C                        to handle parallel I/O implementation
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
#ifndef mpas
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif
#endif

      IMPLICIT NONE

C Include Files:

      INCLUDE SUBST_FILES_ID    ! file name parameters

      REAL, POINTER :: CGRID( :,:,:,: )
      INTEGER      JDATE        ! current model date, coded YYYYDDD
      INTEGER      JTIME        ! current model time, coded HHMMSS
      INTEGER      TSTEP        ! output timestep (HHMMSS)

C Local variables:

      CHARACTER( 16 ) :: PNAME = 'WR_CGRID'
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER K, MXK, SPC, VAR
      INTEGER ALLOCSTAT

      INTEGER TSTEP_RF, NTHIK_RF, NCOLS_RF, NROWS_RF, GDTYP_RF
      REAL( 8 ) :: P_ALP_RF, P_BET_RF, P_GAM_RF
      REAL( 8 ) :: XCENT_RF, YCENT_RF
      REAL( 8 ) :: XORIG_RF, YORIG_RF
      REAL( 8 ) :: XCELL_RF, YCELL_RF
      INTEGER VGTYP_RF
      REAL VGTOP_RF

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL OK

      LOGICAL, EXTERNAL :: FLUSH3

C-----------------------------------------------------------------------

#ifndef mpas
      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.

C Try to open existing file for update

         CALL SUBST_BARRIER

         OK = OPEN3( S_CGRID, FSRDWR3, PNAME )
         CALL SUBST_GLOBAL_LOGICAL( OK, 'AND' )
         IF ( .NOT. OK ) THEN

            XMSG = 'Could not open ' // TRIM( S_CGRID )
     &           // ' file for update - try to open new'
            CALL M3MESG( XMSG )

            IF ( IO_PE_INCLUSIVE ) THEN

C Get default file header attibutes from CONC file (assumes file already open)

               IF ( .NOT. DESC3( CTM_CONC_1 ) ) THEN
                  XMSG = 'Could not get '
     &                 // TRIM( CTM_CONC_1 )
     &                 // ' file description'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

C Set tstep negative to create "circular buffer" type file

!              TSTEP3D = -TSTEP
               SDATE3D = JDATE
               STIME3D = JTIME

C Get CGRID nvars

               NVARS3D = NSPCSD
               NLAYS3D = NLAYS

C Set file header attributes that differ from CONC and open the file

               FDESC3D = ' '
               FDESC3D( 1 ) = 'Computational grid instantaneous concentrations'
               FDESC3D( 2 ) = '- for scenario continuation.'

               WRITE( LOGDEV,* ) ' '
               WRITE( LOGDEV,* ) '       State CGRID File Header Description:'
               DO K = 1, 2
                  WRITE( LOGDEV,* ) '    => ',
     &            TRIM( FDESC3D( K ) )
               END DO

               VAR = 0

               DO SPC = 1, N_GC_SPC
                  VAR = VAR + 1
                  VTYPE3D( VAR ) = M3REAL
                  VNAME3D( VAR ) = GC_SPC( SPC )
                  UNITS3D( VAR ) = 'ppmV'
                  VDESC3D( VAR ) = 'Variable ' // VNAME3D( VAR )
               END DO

C pick up transported RHOJ

               VAR = VAR + 1
               VTYPE3D( VAR ) = M3REAL
               VNAME3D( VAR ) = 'RHOJ'
               UNITS3D( VAR ) = 'm kg m-3'
               VDESC3D( VAR ) = 'Variable ' // VNAME3D( VAR )

               DO SPC = 1, N_AE_SPC
                  VAR = VAR + 1
                  VTYPE3D( VAR ) = M3REAL
                  VNAME3D( VAR ) = AE_SPC( SPC )
                  IF ( VNAME3D( VAR )(1:3) .EQ. 'NUM' ) THEN
                     UNITS3D( VAR ) = 'm-3'
                  ELSE IF ( VNAME3D( VAR )(1:3) .EQ. 'SRF' ) THEN
                     UNITS3D( VAR ) = 'm2 m-3'
                  ELSE
                     UNITS3D( VAR ) = 'ug m-3'
                  END IF
                  VDESC3D( VAR ) = 'Variable ' // VNAME3D( VAR )
               END DO

               DO SPC = 1, N_NR_SPC
                  VAR = VAR + 1
                  VTYPE3D( VAR ) = M3REAL
                  VNAME3D( VAR ) = NR_SPC( SPC )
                  UNITS3D( VAR ) = 'ppmV'
                  VDESC3D( VAR ) = 'Variable ' // VNAME3D( VAR )
               END DO

               DO SPC = 1, N_TR_SPC
                  VAR = VAR + 1
                  VTYPE3D( VAR ) = M3REAL
                  VNAME3D( VAR ) = TR_SPC( SPC )
                  UNITS3D( VAR ) = 'ppmV'
                  VDESC3D( VAR ) = 'Variable ' // VNAME3D( VAR )
               END DO

               WRITE( LOGDEV,* ) ' '
               WRITE( LOGDEV,* ) '       State CGRID File Variable List:'
               DO SPC = 1, VAR
                  WRITE( LOGDEV,'( 5X, "=> VNAME3D(", I3, " ): ", A )' )
     &                   SPC, VNAME3D( SPC )
               END DO

               IF ( .NOT. OPEN3( S_CGRID, FSNEW3, PNAME ) ) THEN
                  XMSG = 'Could not open '
     &                 // TRIM( S_CGRID ) // ' file'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

            END IF   ! IO_PE_INCLUSIVE

         ELSE

C File exists. Check header data with CONC file as reference. Currently only
C proc 0 has CTM_CONC_1 open

            IF ( IO_PE_INCLUSIVE ) THEN

               IF ( .NOT. DESC3( CTM_CONC_1 ) ) THEN
                  XMSG = 'Could not get '
     &                 // TRIM( CTM_CONC_1 )
     &                 // ' file description'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               TSTEP_RF = TSTEP3D
               NTHIK_RF = NTHIK3D
               NCOLS_RF = NCOLS3D
               NROWS_RF = NROWS3D
               GDTYP_RF = GDTYP3D
               P_ALP_RF = P_ALP3D
               P_BET_RF = P_BET3D
               P_GAM_RF = P_GAM3D
               XCENT_RF = XCENT3D
               YCENT_RF = YCENT3D
               XORIG_RF = XORIG3D
               YORIG_RF = YORIG3D
               XCELL_RF = XCELL3D
               YCELL_RF = YCELL3D
               VGTYP_RF = VGTYP3D
               VGTOP_RF = VGTOP3D

               IF ( .NOT. DESC3( S_CGRID ) ) THEN
                  XMSG = 'Could not get '
     &                 // TRIM( S_CGRID )
     &                 // ' file description'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               IF ( TSTEP_RF .NE. ABS( TSTEP3D ) .OR.
     &              NTHIK_RF .NE. NTHIK3D .OR.
     &              NCOLS_RF .NE. NCOLS3D .OR.
     &              NROWS_RF .NE. NROWS3D .OR.
     &              GDTYP_RF .NE. GDTYP3D ) THEN
                    XMSG = 'Header inconsistent on existing S_CGRID'
                    CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF
               IF ( P_ALP_RF .NE. P_ALP3D .OR.
     &              P_BET_RF .NE. P_BET3D .OR.
     &              P_GAM_RF .NE. P_GAM3D ) THEN
                    XMSG = 'Header inconsistent on existing S_CGRID'
                    CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF
               IF ( XCENT_RF .NE. XCENT3D .OR.
     &              YCENT_RF .NE. YCENT3D ) THEN
                    XMSG = 'Header inconsistent on existing S_CGRID'
                    CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF
               IF ( XORIG_RF .NE. XORIG3D .OR.
     &              YORIG_RF .NE. YORIG3D ) THEN
                    XMSG = 'Header inconsistent on existing S_CGRID'
                    CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF
               IF ( XCELL_RF .NE. XCELL3D .OR.
     &              YCELL_RF .NE. YCELL3D ) THEN
                    XMSG = 'Header inconsistent on existing S_CGRID'
                    CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF
               IF ( VGTYP_RF .NE. VGTYP3D ) THEN
                    XMSG = 'Header inconsistent on existing S_CGRID'
                    CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF
               IF ( VGTOP_RF .NE. VGTOP3D ) THEN
                    XMSG = 'Header inconsistent on existing S_CGRID'
                    CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF

            END IF   ! IO_PE_INCLUSIVE

         END IF   ! .NOT. OPEN S_CGRID

      END IF   ! FIRSTIME

#ifdef parallel_io
      IF ( IO_PE_INCLUSIVE ) THEN
         IF ( .NOT. FLUSH3 ( S_CGRID ) ) THEN
            XMSG = 'Could not sync to disk ' // TRIM( S_CGRID )
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END IF
      CALL SUBST_BARRIER
      IF ( .NOT. IO_PE_INCLUSIVE ) THEN
         IF ( .NOT. OPEN3( S_CGRID, FSREAD3, PNAME ) ) THEN
            XMSG = 'Could not open ' // TRIM( S_CGRID )
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END IF

      IF ( .NOT. WRITE3( S_CGRID, ALLVAR3, JDATE, JTIME, CGRID ) ) THEN
         XMSG = 'Could not write S_CGRID'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
#else

#ifdef parallel
      IF ( .NOT. PTRWRITE3( S_CGRID, ALLVAR3, JDATE, JTIME, CGRID ) ) THEN
         XMSG = 'Could not write S_CGRID'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
#else
      IF ( .NOT. WRITE3( S_CGRID, ALLVAR3, JDATE, JTIME, CGRID ) ) THEN
         XMSG = 'Could not write S_CGRID'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
#endif

#endif

      WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &      'Timestep written to', S_CGRID,
     &      'for date and time', JDATE, JTIME
#endif

      RETURN 
      END
