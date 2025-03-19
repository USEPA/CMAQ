
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
C $Header: /amber/home/krt/cmq471/models/CCTM/src/vdiff/acm2_inline/opddep.F,v 1.1.1.1 2010/06/14 16:03:07 sjr Exp $
 
C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%
 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SA_OPWDDEP ( SADEPFILE, JDATE, JTIME, TSTEP )

C20140428  Open dry or wet deposition file (SA_DD_1 or SA_WD_1 as SADEPFILE)
C          for ISAM tracers.
C
C          Called by vdiffproc.F (dry) or initscen.F (wet)

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE UTILIO_DEFN         ! 20120712 replaces ioparms, iofdesc, iodecl
      USE SA_DEFN             ! Mc06
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif


      IMPLICIT NONE
 
      INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:

      CHARACTER( 16 )  SADEPFILE ! Either SA_DD_1 or SA_WD_1
      INTEGER      JDATE      ! current model date, coded YYYYDDD
      INTEGER      JTIME      ! current model time, coded HHMMSS
      INTEGER      TSTEP      ! output time step

C Local variables:

      CHARACTER( 16 ) :: PNAME = 'OPWDDEP'
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER      V, L     ! loop induction variables
      INTEGER      JSPCTAG  ! loop induction for the Tags

C-----------------------------------------------------------------------

C Try to open existing file for update

      IF ( .NOT. OPEN3( SADEPFILE, FSRDWR3, PNAME ) ) THEN

         XMSG = 'Could not open ' // SADEPFILE // ' file for update - '
     &        // 'try to open new'
         CALL M3MESG( XMSG )

C Set output file characteristics based on COORD.EXT and open the dry dep file

         FTYPE3D = GRDDED3
         SDATE3D = JDATE
         STIME3D = 10000 * ( JTIME / 10000 )   ! Clear MMSS fields
         TSTEP3D = TSTEP
         CALL NEXTIME( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour

         NVARS3D = N_SPCTAG  
         NCOLS3D = GL_NCOLS
         NROWS3D = GL_NROWS
         NLAYS3D =     1
         NTHIK3D =     1
         GDTYP3D = GDTYP_GD
         P_ALP3D = P_ALP_GD
         P_BET3D = P_BET_GD 
         P_GAM3D = P_GAM_GD
         XORIG3D = XORIG_GD
         YORIG3D = YORIG_GD
         XCENT3D = XCENT_GD
         YCENT3D = YCENT_GD
         XCELL3D = XCELL_GD
         YCELL3D = YCELL_GD
         VGTYP3D = VGTYP_GD
         VGTOP3D = VGTOP_GD
!        VGTPUN3D = VGTPUN_GD ! currently, not defined
         DO L = 1, NLAYS3D + 1
            VGLVS3D( L ) = VGLVS_GD( L )
         END DO
!        GDNAM3D = GDNAME_GD
         GDNAM3D = GRID_NAME  ! from HGRD_DEFN

         IF ( SADEPFILE .EQ. SA_DD_1 ) THEN
           XMSG = ' RADM dry '
         ELSEIF ( SADEPFILE .EQ. SA_WD_1 ) THEN
           XMSG = ' wet '
         ENDIF

         DO JSPCTAG = 1, N_SPCTAG
           VTYPE3D( JSPCTAG ) = M3REAL
           VNAME3D( JSPCTAG ) = VNAM_SPCTAG( JSPCTAG )
           UNITS3D( JSPCTAG ) = 'kg/hectare'
           VDESC3D( JSPCTAG ) = 'hourly' // XMSG( 1:LEN_TRIM(XMSG) )
     &                  // 'deposition values'
           !write(*,*) VNAM_SPCTAG( JSPCTAG ),VNAME3D(JSPCTAG), UNITS3D( JSPCTAG ),VDESC3D( JSPCTAG )
         END DO

         FDESC3D( 1 ) = 
     &      'hourly 1-layer X-point' // XMSG( 1:LEN_TRIM(XMSG) ) 
     &      // 'dep data for Tags'
         DO L = 2, MXDESC3
           FDESC3D( L ) = ' '
         END DO

C    Open dry deposition file

         IF ( .NOT. OPEN3( SADEPFILE, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not create '// SADEPFILE // ' file'
            CALL M3EXIT( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
         END IF

      END IF

      RETURN
      END
