
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
C $Header: /project/yoj/arc/CCTM/src/vdiff/acm2/opddep.F,v 1.6 2012/01/19 14:40:52 yoj Exp $
 
C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%
 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPDDEP ( JDATE, JTIME, TSTEP, N_SPC_DDEP )

C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN
C   03 Sep 01 David Wong: for new pario
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   21 Jun 10 J.Young: convert for Namelist redesign
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   11 May 11 J.Bash: Modified to output NH3 bidi component fluxes
C   04 Jan 12 J.Young: Initalize char strings with blank padding
C   08 Jun 12 J.Young: remove full character blank padding for GNU Fortran (GCC) 4.1.2
C   12 Dec 19 S.L.Napelenok: ddm-3d implementation for version 5.3.1

      USE RUNTIME_VARS, ONLY : ABFLUX
      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN

#ifdef sens
      USE DDM3D_DEFN, ONLY: NP, NPMAX, SENNUM, SPCNAME, SEN_PAR
#endif

      IMPLICIT NONE
 
      INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:

      INTEGER, INTENT( IN ) :: JDATE  ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME  ! current model time, coded HHMMSS
      INTEGER, INTENT( IN ) :: TSTEP  ! output time step
      INTEGER, INTENT( IN ) :: N_SPC_DDEP ! no. of dry dep species

C Local variables:

      CHARACTER( 16 ) :: PNAME = 'OPDDEP'
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER      V, N, L     ! loop induction variables
      INTEGER      STRT, FINI  ! loop induction variables

C-----------------------------------------------------------------------

#ifndef mpas
C Try to open existing file for update

      IF ( .NOT. OPEN3( CTM_DRY_DEP_1, FSRDWR3, PNAME ) ) THEN
      
         XMSG = 'Could not open ' // CTM_DRY_DEP_1 // ' file for update - '
     &        // 'try to open new'
         CALL M3MESG( XMSG )

C Set output file characteristics based on COORD.EXT and open the dry dep file

         FTYPE3D = GRDDED3
         SDATE3D = JDATE
         STIME3D = JTIME
         TSTEP3D = TSTEP
         CALL NEXTIME( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour
         IF ( ABFLUX ) THEN
            NVARS3D = N_SPC_DDEP + 2
         ELSE 
            NVARS3D = N_SPC_DDEP 
         END IF
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

         N = 0
         STRT = 1
         FINI = N_GC_DDEP
         DO V = STRT, FINI
            N = N + 1
            VTYPE3D( V ) = M3REAL
            VNAME3D( V ) = GC_DDEP( N )
            UNITS3D( V ) = 'kg ha-1'
            VDESC3D( V ) = 'hourly dry deposition values'
         END DO

         N = 0
         STRT = N_GC_DDEP + 1
         FINI = N_GC_DDEP + N_AE_DDEP
         DO V = STRT, FINI
            N = N + 1
            VTYPE3D( V ) = M3REAL
            VNAME3D( V ) = AE_DDEP( N )
            IF ( AE_DDEP( N )( 1:3 ) .EQ. 'NUM' ) THEN
               UNITS3D( V ) = 'ha-1'
            ELSE IF ( AE_DDEP( N )( 1:3 ) .EQ. 'SRF' ) THEN
               UNITS3D( V ) = 'm2 ha-1'
            ELSE
               UNITS3D( V ) = 'kg ha-1'
            END IF
            VDESC3D( V ) = 'hourly dry deposition values'
         END DO

         N = 0
         STRT = N_GC_DDEP + N_AE_DDEP + 1
         FINI = N_GC_DDEP + N_AE_DDEP + N_NR_DDEP
         DO V = STRT, FINI
            N = N + 1
            VTYPE3D( V ) = M3REAL
            VNAME3D( V ) = NR_DDEP( N )
            UNITS3D( V ) = 'kg ha-1'
            VDESC3D( V ) = 'hourly dry deposition values'
         END DO

         N = 0
         STRT = N_GC_DDEP + N_AE_DDEP + N_NR_DDEP + 1
         FINI = N_GC_DDEP + N_AE_DDEP + N_NR_DDEP + N_TR_DDEP
         DO V = STRT, FINI
            N = N + 1
            VTYPE3D( V ) = M3REAL
            VNAME3D( V ) = TR_DDEP( N )
            UNITS3D( V ) = 'ha-1'
            VDESC3D( V ) = 'hourly dry deposition values'
         END DO

         IF ( ABFLUX ) THEN
            VTYPE3D( V ) = M3REAL
            VNAME3D( V ) = 'NH3_Flux'
            UNITS3D( V ) = 'kg/hectare'
            VDESC3D( V ) = 'hourly bidi flux values'
  
            VTYPE3D( V+1 ) = M3REAL
            VNAME3D( V+1 ) = 'NH3_Emis'
            UNITS3D( V+1 ) = 'kg/hectare'
            VDESC3D( V+1 ) = 'hourly NH3_Emis values'
         END IF

         FDESC3D( 1 ) = 'hourly 1-layer cross-point M3Dry dry deposition data'
         DO L = 2, MXDESC3
            FDESC3D( L ) = ' '
         END DO

C    Open dry deposition file

         IF ( .NOT. OPEN3( CTM_DRY_DEP_1, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not create '// CTM_DRY_DEP_1 // ' file'
            CALL M3EXIT( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
         END IF

      END IF
#endif

#ifdef sens
CC..Create sensitivity dry deposition file

C...check for existing file
      IF ( .NOT. OPEN3( CTM_SDRYDEP_1, FSRDWR3, PNAME ) ) THEN
         XMSG = 'Could not open ' // CTM_SDRYDEP_1 // ' file for update - '
     &        // 'try to open new'
         CALL M3MESG( XMSG )

C...set output file variables 
         NVARS3D = N_SPC_DDEP * NPMAX
         FDESC3D( 1 ) = 'sensitivity of hourly dry deposition'

C...define sens dry deposition output descriptors for gaseous species
         N = 0
         STRT = 1
         FINI = N_GC_DDEP
         DO V = STRT, FINI
            N = N + 1
            SPCNAME = GC_DDEP( N )( 1:12 )
            DO NP = 1, NPMAX
               SENNUM = ( V - 1 ) * NPMAX + NP
               VNAME3D ( SENNUM ) = TRIM( SPCNAME ) //'_' // SEN_PAR( NP )
               VTYPE3D( SENNUM ) = M3REAL
               UNITS3D( SENNUM ) = 'kg/hectare'
               VDESC3D( SENNUM ) = 'sensitivity of hourly dry deposition values'
            END DO
         ENDDO

C...define dry deposition output descriptors for aerosol species
         N = 0
         STRT = N_GC_DDEP + 1
         FINI = N_GC_DDEP + N_AE_DDEP
         DO V = STRT, FINI
            N = N + 1
            SPCNAME = AE_DDEP( N )( 1:12 )
            DO NP = 1, NPMAX
               SENNUM = ( V - 1 ) * NPMAX + NP
               VNAME3D ( SENNUM ) = TRIM( SPCNAME ) //'_' // SEN_PAR( NP )
               VTYPE3D( SENNUM ) = M3REAL
               IF ( AE_DDEP( N )( 1:3 ) .EQ. 'NUM' ) THEN
                  UNITS3D( SENNUM ) = 'number/hectare'
               ELSE IF ( AE_DDEP( N )( 1:3 ) .EQ. 'SRF' ) THEN
                  UNITS3D( SENNUM ) = 'm**2/hectare'
               ELSE
                  UNITS3D( SENNUM ) = 'kg/hectare'
               END IF
               VDESC3D( SENNUM ) = 'sensitivity of hourly dry deposition values'
            END DO
         ENDDO

C...define dry deposition output descriptors for non-reactive species
         N = 0
         STRT = N_GC_DDEP + N_AE_DDEP + 1
         FINI = N_GC_DDEP + N_AE_DDEP + N_NR_DDEP
         DO V = STRT, FINI
            N = N + 1
            SPCNAME = NR_DDEP( N )( 1:12 )
            DO NP = 1, NPMAX
               SENNUM = ( V - 1 ) * NPMAX + NP
               VNAME3D ( SENNUM ) = TRIM( SPCNAME ) //'_' // SEN_PAR( NP )
               VTYPE3D( SENNUM ) = M3REAL
               UNITS3D( SENNUM ) = 'kg/hectare'
               VDESC3D( SENNUM ) = 'sensitivity of hourly dry deposition values'
            END DO
         END DO

C...define dry deposition output descriptors for tracer species
         N = 0
         STRT = N_GC_DDEP + N_AE_DDEP + N_NR_DDEP + 1
         FINI = N_GC_DDEP + N_AE_DDEP + N_NR_DDEP + N_TR_DDEP
         DO V = STRT, FINI
            N = N + 1
            SPCNAME = TR_DDEP( N )( 1:12 )
            DO NP = 1, NPMAX
               SENNUM = ( V - 1 ) * NPMAX + NP
               VNAME3D ( SENNUM ) = TRIM( SPCNAME ) //'_' // SEN_PAR( NP )
               VTYPE3D( SENNUM ) = M3REAL
               UNITS3D( SENNUM ) = '----'
               VDESC3D( SENNUM ) = 'sensitivity of hourly dry deposition values'
            END DO
         END DO

C...create a new file
         IF ( .NOT. OPEN3( CTM_SDRYDEP_1, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not create '// CTM_DRY_DEP_1 // ' file'
            CALL M3EXIT( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
         END IF

      END IF
#endif

      RETURN
      END
