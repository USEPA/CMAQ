
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
      SUBROUTINE LOAD_CGRID ( FNAME, SPC_CAT, CMIN, CGRID )

C-----------------------------------------------------------------------
C Function:
C   Initialize the model CGRID array from file data

C Revision history:
C   Jeff - Dec 00 - split out from initscen.F
C                 - move CGRID_MAP into f90 module
C   Jeff - Feb 01 - module LOCALFILE to set CCTM IOAPI file defn's
C                 - add BUFSIZE for dfio option
C                 - assumed shape arrays
C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN; replace INTERP3 with INTERPX;
C   30 Oct 01 J.Young: fix ICBC_FAC
C    4 Sep 03 J.Young: fix SPC/NDX bug if ASO4J IC's are unavailable
C   20 Nov 03 J.Young: enable loading RHOJ
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   21 Jun 10 J.Young: convert for Namelist redesign
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                      removed deprecated TRIMLEN
C    2 Sep 11 J.Young: change ICBC_FAC policy to always assigning factor,
C                      if specified, not just if a surrogate is also specified 
C   11 Sep 15 B.Murphy: add condition for no surrogate name
C
C   08 Mar 19 F. Sidi  Split up ICBC_FAC into IC_FAC and BC_FAC for 
C                      tracer namelist only
C   01 Feb 19 D.Wong: Implemented centralized I/O approach, removed all
C                     MY_N clauses
C   01 Feb 19 D.Wong: Implemented centralized I/O approach, removed all
C                     MY_N clauses
C   08 Mar 19 F. Sidi  Split up ICBC_FAC into IC_FAC and BC_FAC for 
C                      tracer namelist only
C   13 Mar 19 D. Wong: Implemented centralized I/O approach
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
      USE AERO_DATA, ONLY : CHECK_AERO_ICBC, N_MODE
      Use CENTRALIZED_IO_MODULE, only : interpolate_var
      USE RUNTIME_VARS, ONLY : IC_AERO_M2WET, IC_AERO_M2USE
#ifdef mpas
      USE coupler_module
#endif

      IMPLICIT NONE

      INCLUDE SUBST_CONST       ! constants
      INCLUDE SUBST_FILES_ID    ! file name parameters

C Arguments:

      CHARACTER( 16 ) :: FNAME
      INTEGER      JDATE
      INTEGER      JTIME
      CHARACTER( 2 ) :: SPC_CAT
      REAL         CMIN
!     REAL      :: CGRID( :,:,:,: )  ! for initial CONC
      REAL, POINTER :: CGRID( :,:,:,: )  ! for initial CONC

C Parameters:

C minimum aerosol sulfate concentration [ ug/m**3 ]
      REAL, PARAMETER :: AEROCONCMIN = 0.001

C The following two factors assume that sulfate density is 1.8e3 [ kg/m**3 ]
C and that the geometric mean diameter and geometric standard deviations
C for the Aitken mode are 0.01e-6 [ m ] and 1.7 respectively
C and are 0.07e-6 and 2.0 respectively for the accumulation mode.

C factor to calculate aerosol number concentration from aerosol sulfate mass
C concentration in the Aitken mode [ ug ].
      REAL, PARAMETER :: NUMFACT_I = 2.988524 E11

C factor to calculate aerosol number concentration from aerosol sulfate mass
C concentration in the Accumulation mode [ ug ].
      REAL, PARAMETER :: NUMFACT_J = 3.560191 E08

C fraction of sulfuric acid vapor taken as aerosol for first time step
      REAL, PARAMETER :: SO4VAPTOAER = 0.999
C initial fraction of total aerosol sulfate in the Aitken mode
      REAL, PARAMETER :: IFRACATKN = 0.04

      INTEGER, SAVE :: MXSPC
      INTEGER ASTAT

C File variables:

      REAL      :: DENS( NCOLS,NROWS,NLAYS )       ! air density (kg/m^3)
      REAL      :: RHOJ( NCOLS,NROWS,NLAYS ) ! air density X Jacobian (kg/m^2)

C External Functions:

      INTEGER, EXTERNAL :: FINDEX       !  looks up number in table.

C Local Variables

      REAL         MWH2SO4                           ! H2SO4 molec. wt.
      REAL         H2SO4CONV                         ! ppm -> ug/m**3
      INTEGER      LSULF                             ! Gas chem CGRID index
      INTEGER      ISO4AJ, ISO4AI, INUMATKN, INUMACC ! CGRID aerosol indices

      INTEGER      GXOFF, GYOFF               ! global origin offset from file

C for XTRACT3
      INTEGER       :: STRTCOLINI, ENDCOLINI, STRTROWINI, ENDROWINI
      REAL      :: DBUFF( NCOLS,NROWS,NLAYS )
      REAL      :: DBUFF_TMP( NCOLS,NROWS,NLAYS )

      INTEGER      SPC_STRT, SPC_FINI         ! Species Indices
      INTEGER      N_SPCS                     ! no. of species for this call
      INTEGER      NDX                        ! loop copy of INDX
      INTEGER      ISUR                       ! surrogate index
      INTEGER      ISPCS                      ! model species index
      INTEGER, ALLOCATABLE, SAVE :: INDX( : ) ! Variable indices for all IC species
      REAL,    ALLOCATABLE, SAVE :: ICBC_FAC( : ) ! Factor to be applied to ICs
      INTEGER      C, R, L, SPC, V, J         ! loop counters
      INTEGER      ASPC                       ! CGRID RHOJ pointer
      INTEGER      STAT( N_MODE )             ! Status reported by Aerosol Dist Checker

      INTEGER       :: IMODE    
      REAL          :: AER_PAR( 2, N_MODE,6 )  !Modal parameter after the BC 
                                               !check (N, dg, sg)
                                               !      (N, M2, M3) - Before
                                               !      (N, M2, M3) - After
      REAL          :: AECON( N_AE_SPC )

      CHARACTER( 16 ) :: PNAME = 'LOAD_CGRID'
      CHARACTER( 16 ) :: VNAME
      CHARACTER( 16 ) :: POC_CHK
      CHARACTER( 16 ) :: ICNAME_TMP
      CHARACTER( 16 ) :: CONCMIN
      CHARACTER( 96 ) :: XMSG = ' '
      CHARACTER(199 ) :: XMSG2 = ' '
      CHARACTER( 40 ) :: CHWARN = 'Domain extents different from model for '
      CHARACTER( 24 ) :: ESTR1 = 'No IC found for species '
      CHARACTER( 34 ) :: ESTR2 = ' '
      CHARACTER( 34 ) :: ESTR3 = ' '

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL       :: L_WRITE_WARNING

C-----------------------------------------------------------------------

      JDATE = STDATE
      JTIME = STTIME

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

         CALL LOG_HEADING( LOGDEV, 'Load Initial Conditions' ) 

         MXSPC = N_GC_SPC + N_AE_SPC + N_NR_SPC + N_TR_SPC
         ALLOCATE ( INDX( MXSPC ), ICBC_FAC( MXSPC ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'ERROR allocating INDX or ICBC_FAC'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF
#ifdef mpas
         ALLOCATE ( INDX_MPAS( MXSPC ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'ERROR allocating INDX_MPAS '
            call M3EXIT (PNAME, JDATE, JTIME, XMSG, 1)
         END IF
         INDX_MPAS = 0
#endif
      END IF

      WRITE( CONCMIN,'(1PE9.2)' ) CMIN

#ifndef mpas
      IF ( .NOT. OPEN3( FNAME, FSREAD3, PNAME ) ) THEN
         XMSG = 'Could not open ' // FNAME // ' file'
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
 
      IF ( .NOT. DESC3( FNAME ) ) THEN
         XMSG = 'Could not get ' // FNAME // ' file description'
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
 
      IF ( GL_NCOLS .NE. NCOLS3D ) THEN
         WRITE( LOGDEV,* ) ' '
         WRITE( LOGDEV,* ) '    WARNING: ' // CHWARN // FNAME
         WRITE( LOGDEV,* ) '>>  GL_NCOLS: ', GL_NCOLS, '  NCOLS3D: ', NCOLS3D
      END IF
 
      IF ( GL_NROWS .NE. NROWS3D ) THEN
         WRITE( LOGDEV,* ) ' '
         WRITE( LOGDEV,* ) '    WARNING: ' // CHWARN // FNAME
         WRITE( LOGDEV,* ) '>>  GL_NROWS: ', GL_NROWS, '  NROWS3D: ', NROWS3D
      END IF
 
      IF ( NLAYS .NE. NLAYS3D ) THEN
         XMSG = 'Wrong number of layers in ' // FNAME // ' file'
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
#endif

      ESTR2 = ' in ' // TRIM( FNAME ) // '; Look for '
      ESTR3 = ' in ' // TRIM( FNAME ) // '; set to ' // TRIM( CONCMIN )

C The original policy for using surrogate names is first, check if the Namelist
C species is on the IC file; if so ignore any surrogate. If the Namelist species
C is not on the IC file, then check if the surrogate name is; if so also use the
C scale factor (default = 1.0).
C Note: parsing in CGRID_SPCS follows this policy for all the Namelist surrogate
C types (EMIS, DEPV, ICBC, and SCAV).
C => Change this for ICBC:
C First check if there's a surrogate name in the Namelist and use it (and the
C corresponding scale factor) if it exists. If it's not on the IC file, which it
C wouldn`t be if it were blank, e.g., then look for the Namelist species name. If
C that name is found on the IC file, then the default scale factor is applied
C (default = 1.0). To use a scale factor other that 1.0, there must be a name in
C the surrogate slot; it could be the same as the Namelist main species name.

C Get INDX
      DO SPC = 1, MXSPC
         INDX( SPC ) = 0
      END DO

      SPC_STRT = 0
      IF ( SPC_CAT .EQ. 'GC' ) THEN
         WRITE( XMSG,1009 ) 'transported gas-phase (reactive) species'
         WRITE( LOGDEV, * )
         CALL LOG_MESSAGE( LOGDEV, XMSG )
         SPC_STRT = GC_STRT
         N_SPCS = N_GC_SPC
         DO SPC = 1, N_SPCS
C is there a surrogate name?
            ISUR = FINDEX ( SPC, N_GC_IC, GC_IC_MAP )
            NDX = 0
            IF ( ISUR .NE. 0 ) THEN
C is it on the IC file?
#ifdef mpas
               NDX = INDEX1( GC_IC( ISUR ), n_all_spc, all_spc )
#else
               NDX = INDEX1( GC_IC( ISUR ), NVARS3D, VNAME3D )
#endif
               IF ( NDX .NE. 0 ) THEN
C if there`s a surrogate name, CGRID_SPCS handles setting GC_ICBC_FAC
                  INDX( SPC ) = NDX   ! index in the IC file
                  ICBC_FAC( SPC ) = GC_IC_FAC( ISUR )
               ELSE
                  XMSG = ESTR1 // TRIM( GC_IC( ISUR ) ) // ESTR2 // 
     &                   TRIM(GC_SPC( SPC ))
                  CALL M3MESG( XMSG )
               END IF
            END IF
C If there is no surrogate or it cannot be found, look for the (main) species name on the IC file
            If ( ISUR .EQ. 0 .OR. NDX .EQ. 0 .OR. (.NOT. NEW_START ) ) THEN
#ifdef mpas
               NDX = INDEX1( GC_SPC( SPC ), n_all_spc, all_spc )
#else
               NDX = INDEX1( GC_SPC( SPC ), NVARS3D, VNAME3D )
#endif
               IF ( NDX .NE. 0 ) THEN
                  INDX( SPC ) = NDX   ! index in the IC file
                  ICBC_FAC( SPC ) = 1.0
               ELSE
                  XMSG = ESTR1 // TRIM( GC_SPC( SPC ) ) // ESTR3
                  CALL M3MESG( XMSG )
               END IF
            END IF

            IF ( INDX( SPC ) .GT. 0 )
     &         WRITE( LOGDEV,1013 ) INDX( SPC ), GC_SPC( SPC ), ICBC_FAC( SPC )

         END DO

      ELSE IF ( SPC_CAT .EQ. 'AE' ) THEN
         WRITE( XMSG,1009 ) 'transported aerosol species'
         WRITE( LOGDEV, * )
         CALL LOG_MESSAGE( LOGDEV, XMSG )
         SPC_STRT = AE_STRT
         N_SPCS = N_AE_SPC
         DO SPC = 1, N_SPCS
C is there a surrogate name?
            ISUR = FINDEX ( SPC, N_AE_IC, AE_IC_MAP )
            NDX = 0
            IF ( ISUR .NE. 0 ) THEN
C is it on the IC file?
#ifdef mpas
               NDX = INDEX1( AE_IC( ISUR ), n_all_spc, all_spc )
#else
               NDX = INDEX1( AE_IC( ISUR ), NVARS3D, VNAME3D )
#endif
               IF ( NDX .NE. 0 ) THEN
                  INDX( SPC ) = NDX   ! index in the IC file
                  ICBC_FAC( SPC ) = AE_IC_FAC( ISUR )
               ELSE
                  XMSG = ESTR1 // TRIM( AE_IC( ISUR ) ) // ESTR2 // 
     &                   TRIM(AE_SPC( SPC ))
                  CALL M3MESG( XMSG )
               END IF
            END IF
C If there is no surrogate or it cant be found, look for the (main) species name on the IC file
            If ( ISUR .EQ. 0 .OR. NDX .EQ. 0 .OR. (.NOT. NEW_START ) ) THEN
#ifdef mpas
               NDX = INDEX1( AE_SPC( SPC ), n_all_spc, all_spc )
#else
               NDX = INDEX1( AE_SPC( SPC ), NVARS3D, VNAME3D )
#endif
               IF ( NDX .NE. 0 ) THEN
                  INDX( SPC ) = NDX
                  ICBC_FAC( SPC ) = 1.0
               ELSE
                  XMSG = ESTR1 // TRIM( AE_SPC( SPC ) ) // ESTR3
                  CALL M3MESG( XMSG )
               END IF
            END IF

            IF ( INDX( SPC ) .GT. 0 )
     &         WRITE( LOGDEV,1013 ) INDX( SPC ), AE_SPC( SPC ), ICBC_FAC( SPC )
 
         END DO

      ELSE IF ( SPC_CAT .EQ. 'NR' ) THEN
         WRITE( XMSG,1009 ) 'transported non-reactive gas species'
         WRITE( LOGDEV, * )
         CALL LOG_MESSAGE( LOGDEV, XMSG )
         SPC_STRT = NR_STRT
         N_SPCS = N_NR_SPC
         DO SPC = 1, N_SPCS
C is there a surrogate name?
            ISUR = FINDEX ( SPC, N_NR_IC, NR_IC_MAP )
            NDX = 0
            IF ( ISUR .NE. 0 ) THEN
C is it on the IC file?
#ifdef mpas
               NDX = INDEX1( NR_IC( ISUR ), n_all_spc, all_spc )
#else
               NDX = INDEX1( NR_IC( ISUR ), NVARS3D, VNAME3D )
#endif
               IF ( NDX .NE. 0 ) THEN
                  INDX( SPC ) = NDX   ! index in the IC file
                  ICBC_FAC( SPC ) = NR_IC_FAC( ISUR )
               ELSE
                  XMSG = ESTR1 // TRIM( NR_IC( ISUR ) ) // ESTR2 // 
     &                   TRIM(NR_SPC( SPC ))
                  CALL M3MESG( XMSG )
               END IF
            END IF
C If there is no surrogate or it cant be found, look for the (main) species name on the IC file
            If ( ISUR .EQ. 0 .OR. NDX .EQ. 0 .OR. (.NOT. NEW_START ) ) THEN
#ifdef mpas
               NDX = INDEX1( NR_SPC( SPC ), n_all_spc, all_spc )
#else
               NDX = INDEX1( NR_SPC( SPC ), NVARS3D, VNAME3D )
#endif
               IF ( NDX .NE. 0 ) THEN
                  INDX( SPC ) = NDX
                  ICBC_FAC( SPC ) = 1.0
               ELSE
                  XMSG = ESTR1 // TRIM( NR_SPC( SPC ) ) // ESTR3
                  CALL M3MESG( XMSG )
               END IF
            END IF

            IF ( INDX( SPC ) .GT. 0 )
     &         WRITE( LOGDEV,1013 ) INDX( SPC ), NR_SPC( SPC ), ICBC_FAC( SPC )

         END DO

      ELSE IF ( SPC_CAT .EQ. 'TR' ) THEN
         WRITE( XMSG, 1009 ) 'transported inert tracer gas species'
         WRITE( LOGDEV, * )
         CALL LOG_MESSAGE( LOGDEV, XMSG )
         SPC_STRT = TR_STRT
         N_SPCS = N_TR_SPC
         DO SPC = 1, N_SPCS
C is there a surrogate name?
            ISUR = FINDEX ( SPC, N_TR_IC, TR_IC_MAP )
            NDX = 0
            IF ( ISUR .NE. 0 ) THEN
C is it on the IC file?
#ifdef mpas
               NDX = INDEX1( TR_IC( ISUR ), n_all_spc, all_spc )
#else
               NDX = INDEX1( TR_IC( ISUR ), NVARS3D, VNAME3D )
#endif
               IF ( NDX .NE. 0 ) THEN
                  INDX( SPC ) = NDX   ! index in the IC file
                  ICBC_FAC( SPC ) = TR_IC_FAC( ISUR )
               ELSE
                  XMSG = ESTR1 // TRIM( TR_IC( ISUR ) ) // ESTR2 // 
     &                   TRIM(TR_SPC( SPC ))
                  CALL M3MESG( XMSG )
               END IF
            END IF
C If there is no surrogate or it cant be found, look for the (main) species name on the IC file
            If ( ISUR .EQ. 0 .OR. NDX .EQ. 0 .OR. (.NOT. NEW_START ) ) THEN
#ifdef mpas
               NDX = INDEX1( TR_SPC( SPC ), n_all_spc, all_spc )
#else
               NDX = INDEX1( TR_SPC( SPC ), NVARS3D, VNAME3D )
#endif
               IF ( NDX .NE. 0 ) THEN
                  INDX( SPC ) = NDX
                  ICBC_FAC( SPC ) = 1.0
               ELSE
                  XMSG = ESTR1 // TRIM( TR_SPC( SPC ) ) // ESTR3
                  CALL M3MESG( XMSG )
               END IF
            END IF

            IF ( INDX( SPC ) .GT. 0 )
     &         WRITE( LOGDEV,1013 ) INDX( SPC ), TR_SPC( SPC ), ICBC_FAC( SPC )

         END DO

      ELSE IF ( SPC_CAT .EQ. 'RJ' ) THEN
         N_SPCS = 0
      ELSE
         XMSG = 'Species categories incorrect for CGRID '
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
      END IF
        
C Read into CGRID

!     CALL SUBHFILE ( FNAME, GXOFF, GYOFF,
!    &                STRTCOLINI, ENDCOLINI, STRTROWINI, ENDROWINI )
C IOFDESC common now loaded with FNAME header

      DO SPC = 1, N_SPCS
         V = SPC_STRT - 1 + SPC
         NDX = INDX( SPC )

         IF ( NDX .GT. 0 ) THEN
#ifndef mpas
            call interpolate_var (VNAME3D ( NDX ), jdate, jtime, DBUFF)

            ! Add Non-Carbon Mass to Carbon Mass if any tracer is
            ! pointing to POC as a surrogate. It is very likely that
            ! these tracers are seeking the entire POA mass (e.g.
            ! semivolatile POA tracers).
            POC_CHK = 'NOTPOC'
            IF ( SPC_CAT .EQ. 'AE' ) POC_CHK = AE_SPC( SPC )

            IF ( VNAME3D( NDX )( 1:4 ) .EQ. 'APOC' .AND.
     &           POC_CHK(1:4) .NE. 'APOC' ) THEN 
               ICNAME_TMP = 'APNCOM' // VNAME3D( NDX )( 5:5 )
               call interpolate_var (ICNAME_TMP, jdate, jtime, DBUFF_TMP)
               DBUFF = DBUFF + DBUFF_TMP
            END IF

            ! Load ICs into CGRID
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CGRID( C,R,L,V ) = ICBC_FAC( SPC ) * DBUFF( C,R,L )
                  END DO
               END DO
            END DO
#endif

         END IF   ! INDX .GT. 0

      END DO
 
      IF ( SPC_CAT .EQ. 'RJ' ) THEN

C Load RHOJ for transport and mixing ratio advection adjustment

         call interpolate_var ('DENSA_J', jdate, jtime, RHOJ)

         ASPC = GC_STRT - 1 + N_GC_SPCD
         DO L = 1, NLAYS
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  CGRID( C,R,L,ASPC ) = RHOJ( C,R,L )
               END DO
            END DO
         END DO

      END IF

      IF ( SPC_CAT .EQ. 'AE' ) THEN
         CALL LOG_SUBHEADING( LOGDEV, 'Check Aerosol IC Size Distributions' )

         ! Check Aerosol Size Distributions and Warn the User if They Are Not Robust.
         ! Surface area is output from this routine consistent with the
         ! specification in IC_AERO_M2WET.
         IF ( NEW_START ) THEN
           SPC_STRT = AE_STRT
           SPC_FINI = AE_STRT + N_AE_SPC - 1
           L_WRITE_WARNING = .TRUE.
           DO L = 1, NLAYS
           DO R = 1, NROWS
           DO C = 1, NCOLS
              AECON( 1:N_AE_SPC ) = CGRID( C,R,L,SPC_STRT:SPC_FINI )
              CALL CHECK_AERO_ICBC( AECON, IC_AERO_M2WET, IC_AERO_M2USE, 
     &                              .FALSE., L_WRITE_WARNING, C,R,L )
              CGRID( C,R,L,SPC_STRT:SPC_FINI ) = AECON( 1:N_AE_SPC )
           END DO
           END DO
           END DO
         END IF

C are ASO4J ICs available on the file?

         VNAME = 'ASO4J'
#ifdef mpas
         NDX = INDEX1( VNAME, n_ae_spc, ae_spc )
#else
         NDX = INDEX1( VNAME, NVARS3D, VNAME3D )
#endif
     
         IF ( NDX .EQ. 0 ) THEN  ! ASO4J not on file

C Set pointers for gas (vapor) phase sulfur species

            NDX = INDEX1( VNAME, N_AE_SPC, AE_SPC )
            IF ( NDX .NE. 0 ) THEN
               ISO4AJ = AE_STRT - 1 + NDX
            ELSE
               XMSG = 'Could not find ' // VNAME // 'in aerosol table'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
            END IF

            VNAME = 'SULF'
            NDX = INDEX1( VNAME, N_GC_G2AE, GC_G2AE )
            IF ( NDX .NE. 0 ) THEN
               LSULF   = GC_STRT - 1 + GC_G2AE_MAP( NDX )
               MWH2SO4 = GC_MOLWT( GC_G2AE_MAP( NDX ) )
            ELSE
               XMSG = 'Could not find ' // VNAME // 'in gas chem aerosol table'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
            END IF

            VNAME = 'ASO4I'
            NDX = INDEX1( VNAME, N_AE_SPC, AE_SPC )
            IF ( NDX .NE. 0 ) THEN
               ISO4AI = AE_STRT - 1 + NDX
            ELSE
               XMSG = 'Could not find ' // VNAME // 'in aerosol table'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
            END IF
            VNAME = 'NUMATKN'
            NDX = INDEX1( VNAME, N_AE_SPC, AE_SPC )
            IF ( NDX .NE. 0 ) THEN
               INUMATKN = AE_STRT - 1 + NDX
            ELSE
               XMSG = 'Could not find ' // VNAME // 'in aerosol table'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
            END IF
            VNAME = 'NUMACC'
            NDX = INDEX1( VNAME, N_AE_SPC, AE_SPC )
            IF ( NDX .NE. 0 ) THEN
               INUMACC = AE_STRT - 1 + NDX
            ELSE
               XMSG = 'Could not find ' // VNAME // 'in aerosol table'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
            END IF

            call interpolate_var ('DENS', jdate, jtime, DENS)

C Partition the aerosol sulfate arrays with a fraction of the initial SO4 

            H2SO4CONV = 1.0E3 * MWH2SO4 / MWAIR * SO4VAPTOAER

            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS

C total accumulation mode sulfate:

                     CGRID( C,R,L,ISO4AJ )   = MAX ( AEROCONCMIN,
     &                                         ( 1.0 - IFRACATKN )
     &                                       * H2SO4CONV
     &                                       * DENS ( C,R,L )
     &                                       * CGRID( C,R,L,LSULF ) )

C Accumulation mode number:
    
                     CGRID( C,R,L,INUMACC )  = NUMFACT_J
     &                                       * CGRID( C,R,L,ISO4AJ )

C Aitken mode sulfate:
    
                     CGRID( C,R,L,ISO4AI )   = MAX ( AEROCONCMIN,
     &                                         IFRACATKN
     &                                       * H2SO4CONV
     &                                       * DENS ( C,R,L )
     &                                       * CGRID( C,R,L,LSULF ) )
    
C Aitken mode number:
    
                     CGRID( C,R,L,INUMATKN ) = NUMFACT_I
     &                                       * CGRID( C,R,L,ISO4AI )
    
C correct sulfate vapor concentration for part removed:
    
                     CGRID( C,R,L,LSULF )    = ( 1.0 - SO4VAPTOAER )
     &                                       * CGRID( C,R,L,LSULF)
    
                  END DO
               END DO
            END DO

            XMSG = 'No IC''s found for aerosol sulfate. ' //
     &             'Gas Chem sulfate used for partitioning.'
            CALL M3MESG( XMSG )

         END IF  ! NDX .EQ. 0

      END IF  !  SPC_CAT .EQ. 'AE'

      RETURN

1009  FORMAT( 'Initial Condition Factors used for ', A )
1013  FORMAT( 5X, I3, 2X, A, 1PG13.5 )
      END
