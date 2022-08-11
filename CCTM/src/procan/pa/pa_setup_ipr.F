
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

      SUBROUTINE PA_SETUP_IPR

C-----------------------------------------------------------------------
C Function: To store IPR data that will be needed to 
C           generate the PA report and output INCLUDE files
 
C Preconditions: None
  
C Key Subroutines/Functions Called: None
 
C Revision History:
C  Prototype created by Jerry Gipson, August, 1996
C  Modified by Jerry Gipson April, 1997, to add ADJC process
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Modified Jun, 1998 by Jerry Gipson to add PING to the CTM science processes
C  Modified Feb, 2002 by Jerry Gipson to correct IO/API variable names for
C  TOTDIF & TOTTRAN
C  Jun, 2005 Jeff Young to add HADV for yamo
C  Jan, 2006 Jeff Young: change operator names to be 4 chars - they prepend
C species names, which must remain less that 12 chars, i.e. OPNM_<__SPECIES__>
C                                                           12345 67890123456
C  Aug 2011 Jeff Young: Replaced I/O API include files with IOAPI`s M3UTILIO
C  Sep 2018 C. Nolte, S. Roselle: replace M3UTILIO with UTILIO_DEFN
C-----------------------------------------------------------------------
      USE UTILIO_DEFN
      USE PA_GLOBAL     ! Mech data used 
      USE PA_VARS, Only: NFAMLYS, FAMNAME, NUMFAMMEM, FAMMEMNAM, FAMSC
      USE PA_IPRVARS
      USE PA_DEFN
      USE PA_PARSE, Only: IZERO
      USE CGRID_SPCS, Only: N_CGRID_SPC, CGRID_NAME

      IMPLICIT NONE
      
C Includes: None
      
C Arguments: None
                                        
C Parameters: None

C External Functions: None

C Local Variables:
      INTEGER IFM    ! Family pointer 
      INTEGER ISPC   ! Species pointer
      INTEGER N      ! Loop counter
      INTEGER NIPR   ! Loop counter on IPR output commands
      INTEGER IPRSPC ! Loop counter on IPR output commands
      INTEGER ICG    ! Loop index for species
      INTEGER ISV    ! Loop index for species
      INTEGER ASTAT  ! Memory allocation status

      LOGICAL :: LSAVSP( N_CGRID_SPC )  ! Flag to save species conc for PA

      CHARACTER( 16 )  :: IPRNAME_TMP( MAXIPROUT )
      CHARACTER( 60 )  :: IPRDESC_TMP( MAXIPROUT )
      INTEGER          :: MAP_IPRVARtoSPC_TMP( MAXIPROUT )
      LOGICAL          :: MASK_IPR_PROC_TMP( MAXIPROUT,NPRCS )
      INTEGER          :: MAP_IPRtoCGRID_TMP( MAXIPRSPC,MXSPEC )
      REAL             :: SPCOEF_TMP( MAXIPRSPC,MXSPEC )
      CHARACTER( 200 ) :: MSG = ' '
         
C-----------------------------------------------------------------------

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set up the Process analysis output pointers 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      NIPRVAR = 0
      DO NIPR = 1, N_IPR_SPC
         IF ( LEN_TRIM( IPR_SPNAM( NIPR ) ) .GT. 11 ) THEN
            WRITE( *,* ) ' '
            WRITE( MSG, 94030 ) TRIM( IPR_SPNAM( NIPR ) )
            CALL M3MESG( MSG )
            WRITE( MSG, 94031 )
            CALL M3MESG( MSG )
         END IF

         DO N = 1, N_IPR_OPS( NIPR )

            NIPRVAR = NIPRVAR + 1

            IF ( NIPRVAR .GT. MAXIPROUT ) THEN
               WRITE( MSG, 94000 ) 
               CALL M3MESG( MSG )
               WRITE( MSG, 94020 ) MAXIPROUT
               CALL M3MESG( MSG )
               CALL M3EXIT( 'GETEXTDAT', IZERO, IZERO, ' ', XSTAT2 )
            END IF

            IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'ZADV' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'ZADV_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Z-Advection of ' // 
     &                                   IPR_SPNAM( NIPR )
               MAP_IPRVARtoSPC_TMP( NIPRVAR )= NIPR
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_ZADV ) = .TRUE.

            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'XADV' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'XADV_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Horizontal X-Advection of ' //
     &                                   IPR_SPNAM( NIPR )
               MAP_IPRVARtoSPC_TMP( NIPRVAR )= NIPR
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_XADV ) = .TRUE.

            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'YADV' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'YADV_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Horizontal Y-Advection of ' //
     &                                   IPR_SPNAM( NIPR )
               MAP_IPRVARtoSPC_TMP( NIPRVAR )= NIPR
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_YADV ) = .TRUE.

            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'HDIF' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'HDIF_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Horizontal diffusion for ' // 
     &                                   IPR_SPNAM( NIPR )
               MAP_IPRVARtoSPC_TMP( NIPRVAR )= NIPR
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_HDIF ) = .TRUE.

            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'VDIF' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'VDIF_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Vertical diffusion for ' // 
     &                                   IPR_SPNAM( NIPR )
               MAP_IPRVARtoSPC_TMP( NIPRVAR )= NIPR
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_VDIF ) = .TRUE.

            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'EMIS' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'EMIS_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Emissions of ' // 
     &                                   IPR_SPNAM( NIPR )
               MAP_IPRVARtoSPC_TMP( NIPRVAR )= NIPR
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_EMIS ) = .TRUE.

            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'DDEP' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'DDEP_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Dry deposition for ' // 
     &                                   IPR_SPNAM( NIPR )
               MAP_IPRVARtoSPC_TMP( NIPRVAR )= NIPR
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_DDEP ) = .TRUE.

            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'CLDS' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'CLDS_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Clouds for ' // 
     &                                   IPR_SPNAM( NIPR )
               MAP_IPRVARtoSPC_TMP( NIPRVAR )= NIPR
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_CLDS ) = .TRUE.

            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'PVO3' ) THEN
               IPRNAME_TMP( NIPRVAR )  = 'PVO3_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR )  = 'Potential Vorticity for ' // 
     &                                    IPR_SPNAM( NIPR )
               MAP_IPRVARtoSPC_TMP( NIPRVAR )= NIPR
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_PVO3 ) = .TRUE.

            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'CHEM' ) THEN
               IPRNAME_TMP( NIPRVAR )  = 'CHEM_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR )  = 'Chemistry of ' // 
     &                                   IPR_SPNAM( NIPR )
               MAP_IPRVARtoSPC_TMP( NIPRVAR )= NIPR
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_CHEM ) = .TRUE.

            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'COND' ) THEN
               IPRNAME_TMP( NIPRVAR )  = 'COND_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR )  = 'Condensation for ' // 
     &                                    IPR_SPNAM( NIPR )
               MAP_IPRVARtoSPC_TMP( NIPRVAR )= NIPR
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_COND ) = .TRUE.

            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'COAG' ) THEN
               IPRNAME_TMP( NIPRVAR )  = 'COAG_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR )  = 'Coagulation for ' // 
     &                                    IPR_SPNAM( NIPR )
               MAP_IPRVARtoSPC_TMP( NIPRVAR )= NIPR
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_COAG ) = .TRUE.

            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:5 ) .EQ. 'GROW' ) THEN
               IPRNAME_TMP( NIPRVAR )  = 'GROW_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR )  = 'Aerosol Growth (Renaming) for ' // 
     &                                    IPR_SPNAM( NIPR )
               MAP_IPRVARtoSPC_TMP( NIPRVAR )= NIPR
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_GROW ) = .TRUE.

            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:3 ) .EQ. 'NPF' ) THEN
               IPRNAME_TMP( NIPRVAR )  = 'NPF_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR )  = 'New Particle Formation for ' // 
     &                                    IPR_SPNAM( NIPR )
               MAP_IPRVARtoSPC_TMP( NIPRVAR )= NIPR
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_NPF ) = .TRUE.

            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'AERO' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'AERO_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Aerosol Impact on ' // 
     &                                   IPR_SPNAM( NIPR )
               MAP_IPRVARtoSPC_TMP( NIPRVAR )= NIPR
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_COND ) = .TRUE.
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_COAG ) = .TRUE.
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_GROW ) = .TRUE.
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_NPF ) = .TRUE.

            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'HADV' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'HADV_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Horizontal Advection of ' //
     &                                   IPR_SPNAM( NIPR )
               MAP_IPRVARtoSPC_TMP( NIPRVAR )= NIPR
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_XADV ) = .TRUE.
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_YADV ) = .TRUE.

            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'MADV' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'MADV_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Horizontal and Vertical Advection of ' //
     &                                   IPR_SPNAM( NIPR )
               MAP_IPRVARtoSPC_TMP( NIPRVAR )= NIPR
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_ZADV ) = .TRUE.
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_XADV ) = .TRUE.
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_YADV ) = .TRUE.

            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'TDIF' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'TDIF_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Total Diffusion of ' //
     &                                   IPR_SPNAM( NIPR )
               MAP_IPRVARtoSPC_TMP( NIPRVAR )= NIPR
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_HDIF ) = .TRUE.
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_VDIF ) = .TRUE.

            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'TRNM' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'TRNM_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Total Mass-Conserving Transport of ' //
     &                                   IPR_SPNAM( NIPR )
               MAP_IPRVARtoSPC_TMP( NIPRVAR )= NIPR
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_HDIF ) = .TRUE.
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_VDIF ) = .TRUE.
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_ZADV ) = .TRUE.
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_XADV ) = .TRUE.
               MASK_IPR_PROC_TMP( NIPRVAR,IPR_YADV ) = .TRUE.

            ELSE
               MSG = 'Unknown Process ' // TRIM(IPR_OPNAME(NIPR,N )) //
     &               ' has been selected in Process Analysis.'
               CALL M3EXIT( 'PA_SETUP_IPR',0,0,MSG,XSTAT2 )
            END IF
         END DO
      END DO

      ! Save Name and Description Vectors for Processes
      ALLOCATE( IPRNAME( NIPRVAR ),
     &          IPRDESC( NIPRVAR ), 
     &          MAP_IPRVARtoSPC( NIPRVAR ),
     &          MASK_IPR_PROC( NIPRVAR,NPRCS ),
     &          STAT = ASTAT )
      IF ( ASTAT .NE. 0 ) THEN
         MSG = 'ERROR 2 allocating IPR variables'
         CALL M3EXIT ( 'PA_SETUP_IPR', 0, 0, MSG, XSTAT2 )
      END IF
      IPRNAME = IPRNAME_TMP( 1:NIPRVAR )
      IPRDESC = IPRDESC_TMP( 1:NIPRVAR )
      MAP_IPRVARtoSPC = MAP_IPRVARtoSPC_TMP( 1:NIPRVAR )
      MASK_IPR_PROC = MASK_IPR_PROC_TMP( 1:NIPRVAR,: )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set up the species pointers for the IPR Outputs
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LSAVSP = .FALSE.

      ALLOCATE( NCGRID ( N_IPR_SPC ), 
     &           STAT = ASTAT )
      IF ( ASTAT .NE. 0 ) THEN
         MSG = 'ERROR 3 allocating IPR variables'
         CALL M3EXIT ( 'PA_SETUP_IPR', 0, 0, MSG, XSTAT2 )
      END IF
      
      ! Map IPR Species Names to CGRID
      MAP_IPRtoCGRID_TMP = 0
      SPCOEF_TMP = 0

      DO IPRSPC = 1, N_IPR_SPC
         ICG = INDEX1( IPR_SPNAM( IPRSPC ), N_CGRID_SPC, CGRID_NAME )
         IF ( ICG .NE. 0 ) THEN
            NCGRID( IPRSPC ) = 1
            MAP_IPRtoCGRID_TMP( IPRSPC,1 ) = ICG
            SPCOEF_TMP( IPRSPC,1 ) = 1.0
         ELSE
            ! This IPR Species is a Family. Find its members on CGRID
            IFM = INDEX1( IPR_SPNAM( IPRSPC ), NFAMLYS, FAMNAME )
            NCGRID( IPRSPC ) = NUMFAMMEM( IFM )
            DO N = 1, NUMFAMMEM( IFM )
               ICG = INDEX1( FAMMEMNAM( IFM,N ), N_CGRID_SPC, CGRID_NAME )
               MAP_IPRtoCGRID_TMP( IPRSPC,N ) = ICG
               SPCOEF_TMP( IPRSPC,N ) = FAMSC( IFM,N )
            END DO
         END IF
      END DO
      MXCGRID = MAX( 1, MAXVAL( NCGRID( : ) ) )

      ALLOCATE( MAP_IPRtoCGRID( N_IPR_SPC,MXCGRID ),
     &          SPCOEF ( N_IPR_SPC,MXCGRID ), 
     &          STAT = ASTAT )
      IF ( ASTAT .NE. 0 ) THEN
         MSG = 'ERROR 4 allocating IPR variables'
         CALL M3EXIT ( 'PA_SETUP_IPR', 0, 0, MSG, XSTAT2 )
      END IF
      MAP_IPRtoCGRID  = MAP_IPRtoCGRID_TMP( 1:N_IPR_SPC,1:MXCGRID )
      SPCOEF          = SPCOEF_TMP( 1:N_IPR_SPC,1:MXCGRID )

      RETURN

C----------------------- FORMAT Statements -----------------------------

94000 FORMAT( 'ERROR: Maximum number of IPR_OUTPUTs exceeded' )
94020 FORMAT( '       Modify PARAMETER ( MAXIPROUT =', I3,' ) or',
     &              ' decrease the number of IPR_OUTPUTs' )
94030 FORMAT( 'WARNING: Species name ', A, ' too long.' )
94031 FORMAT( '     Combined variable name will ',
     &        'exceed the I/O-API''s 16 character limit.' )
            
      END SUBROUTINE PA_SETUP_IPR

