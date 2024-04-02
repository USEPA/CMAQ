
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

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN

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

! component flux varialbes      
      INTEGER,         SAVE :: N_BD_EMIS 
      CHARACTER( 16 ), SAVE :: BD_EMIS_SPC( 3 )
      INTEGER,         SAVE :: N_BD_FLUX 
      CHARACTER( 16 ), SAVE :: BD_FLUX_SPC( 1 )

      INTEGER      V, N, L     ! loop induction variables
      INTEGER      STRT, FINI  ! loop induction variables

C-----------------------------------------------------------------------

C Try to open existing file for update

      IF ( .NOT. OPEN3( CTM_DRY_DEP_1, FSRDWR3, PNAME ) ) THEN

         N_BD_EMIS = 0
         N_BD_FLUX = 0
         If( ABFLUX ) Then
            N_BD_EMIS = N_BD_EMIS + 1
            BD_EMIS_SPC( N_BD_EMIS ) = 'NH3_Emis'
            N_BD_FLUX = 1
            BD_FLUX_SPC( 1 ) = 'NH3_Flux'
         End If 
         If( HGBIDI ) Then
            N_BD_EMIS = N_BD_EMIS + 1
            BD_EMIS_SPC( N_BD_EMIS ) = 'HG_Emis'
         End If 
         If( SFC_HONO ) Then 
            N_BD_EMIS = N_BD_EMIS + 1      
            BD_EMIS_SPC( N_BD_EMIS ) = 'HONO_Het '
         End If      

         XMSG = 'Could not open ' // CTM_DRY_DEP_1 // ' file for update - '
     &        // 'try to open new'
         CALL M3MESG( XMSG )

C Set output file characteristics based on COORD.EXT and open the dry dep file

         FTYPE3D = GRDDED3
         SDATE3D = JDATE
         STIME3D = JTIME
         TSTEP3D = TSTEP
         CALL NEXTIME( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour
         NVARS3D = N_SPC_DDEP + N_BD_EMIS + N_BD_FLUX 
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
         DO L = 1, NLAYS3D + 1
            VGLVS3D( L ) = VGLVS_GD( L )
         END DO
         GDNAM3D = GRID_NAME  ! from HGRD_DEFN

         N = 0
         STRT = 1
         FINI = N_GC_DDEP
         DO V = STRT, FINI
            N = N + 1
            VTYPE3D( V ) = M3REAL
            VNAME3D( V ) = GC_DDEP( N )
            UNITS3D( V ) = 'kg/hectare'
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
               UNITS3D( V ) = 'number/hectare'
            ELSE IF ( AE_DDEP( N )( 1:3 ) .EQ. 'SRF' ) THEN
               UNITS3D( V ) = 'm**2/hectare'
            ELSE
               UNITS3D( V ) = 'kg/hectare'
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
            UNITS3D( V ) = 'kg/hectare'
            VDESC3D( V ) = 'hourly dry deposition values'
         END DO

         N = 0
         STRT = N_GC_DDEP + N_AE_DDEP + N_NR_DDEP + 1
         FINI = N_GC_DDEP + N_AE_DDEP + N_NR_DDEP + N_TR_DDEP
         DO V = STRT, FINI
            N = N + 1
            VTYPE3D( V ) = M3REAL
            VNAME3D( V ) = TR_DDEP( N )
            UNITS3D( V ) = '----'
            VDESC3D( V ) = 'hourly dry deposition values'
         END DO

         IF ( ABFLUX ) THEN
            N = 0   
            STRT = N_GC_DDEP + N_AE_DDEP + N_NR_DDEP + N_TR_DDEP + 1
            FINI = N_GC_DDEP + N_AE_DDEP + N_NR_DDEP + N_TR_DDEP + 1
            DO V = STRT, FINI
               N = N + 1
               VTYPE3D( V ) = M3REAL
               VNAME3D( V ) = BD_EMIS_SPC( N )
               UNITS3D( V ) = 'kg/hectare'
               VDESC3D( V ) = 'hourly emission values'
            END DO
            N = 0   
            STRT = FINI + 1
            FINI = FINI + N_BD_FLUX
            DO V = STRT, FINI
               N = N + 1
               VTYPE3D( V ) = M3REAL
               VNAME3D( V ) = BD_FLUX_SPC( N )
               UNITS3D( V ) = 'kg/hectare'
               VDESC3D( V ) = 'hourly flux values'
            END DO
         END IF
         IF ( HGBIDI ) THEN
            STRT = FINI + 1
            FINI = FINI + 1
            DO V = STRT, FINI
               VTYPE3D( V ) = M3REAL
               VNAME3D( V ) = 'HG_Emis'
               UNITS3D( V ) = 'kg/hectare'
               VDESC3D( V ) = 'hourly emission values'
            END DO
         END IF
         IF ( SFC_HONO ) THEN
            STRT = FINI + 1
            FINI = FINI + 1
            DO V = STRT, FINI
               VTYPE3D( V ) = M3REAL
               VNAME3D( V ) = 'HONO_Het '
               UNITS3D( V ) = 'kg/hectare'
               VDESC3D( V ) = 'hourly surface heterogenous production values'
            END DO
         END IF

         FDESC3D( 1 ) = 'hourly 1-layer cross-point STAGE dry deposition data'
         DO L = 2, MXDESC3
            FDESC3D( L ) = ' '
         END DO

C    Open dry deposition file

         IF ( .NOT. OPEN3( CTM_DRY_DEP_1, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not create '// CTM_DRY_DEP_1 // ' file'
            CALL M3EXIT( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
         END IF

      END IF

      RETURN
      END
