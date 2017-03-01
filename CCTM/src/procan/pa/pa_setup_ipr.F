
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
C $Header: /project/yoj/arc/PROCAN/src/driver/pa/pa_setup_ipr.F,v 1.9 2011/11/02 14:34:00 yoj Exp $

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
C-----------------------------------------------------------------------
      USE M3UTILIO      ! IOAPI module
      USE PA_GLOBAL     ! Mech data used 
      USE PA_VARS, Only: NFAMLYS, FAMNAME, NUMFAMMEM, FAMMEMNAM, FAMSC
      USE PA_IPRVARS
      USE PA_IPRDEF
      USE PA_DEFN
      USE PA_PARSE, Only: IZERO

      IMPLICIT NONE
      
C Includes: None
      
C Arguments: None
                                        
C Parameters: None

C External Functions: None

C Local Variables:
      INTEGER IFM    ! Family pointer 
      INTEGER ISP    ! Species pointer
      INTEGER N      ! Loop counter
      INTEGER NIPR   ! Loop counter on IPR output commands
      INTEGER NS     ! Loop index for species
      INTEGER ASTAT  ! Memory allocation status

      LOGICAL :: LSAVSP( MXSPEC )  ! Flag to save species conc for PA

      CHARACTER( 120 ) :: MSG = ' '
         
C-----------------------------------------------------------------------

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set up the Process analysis output pointers 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ALLOCATE( LPROCOUT( NPRCS ),
     &          IPROUT( N_IPR_SPC,NPRCS ), STAT = ASTAT )
      IF ( ASTAT .NE. 0 ) THEN
         MSG = 'ERROR 1 allocating IPR variables'
         CALL M3EXIT ( 'PA_SETUP_IPR', 0, 0, MSG, XSTAT2 )
      END IF
      LPROCOUT = .FALSE.   ! array assignment
      IPROUT = 0           !   "       "

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

            IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'XADV' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'XADV_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'X-Advection of ' //
     &                                   IPR_SPNAM( NIPR )
               IPROUT( NIPR,1 )   =  NIPRVAR
               LPROCOUT( 1 )      = .TRUE.
            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'YADV' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'YADV_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Y-Advection of ' //
     &                                   IPR_SPNAM( NIPR )
               IPROUT( NIPR,2 )   =  NIPRVAR
               LPROCOUT( 2 )      = .TRUE.
            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'ZADV' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'ZADV_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Z-Advection of ' // 
     &                                   IPR_SPNAM( NIPR )
               IPROUT( NIPR,3 )   = NIPRVAR
               LPROCOUT( 3 )      = .TRUE.
            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'ADJC' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'ADJC_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Advection mass adjustment for ' // 
     &                                   IPR_SPNAM( NIPR )
               IPROUT( NIPR,4 )   = NIPRVAR
               LPROCOUT( 4 )      = .TRUE.
            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'HADV' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'HADV_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Horizontal Advection of ' //
     &                                   IPR_SPNAM( NIPR )
               IPROUT( NIPR,5 )   =  NIPRVAR
               LPROCOUT( 5 )      = .TRUE.
            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'HDIF' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'HDIF_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Horizontal diffusion for ' // 
     &                                   IPR_SPNAM( NIPR )
               IPROUT( NIPR,6 )   = NIPRVAR
               LPROCOUT( 6 )      = .TRUE.
            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'VDIF' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'VDIF_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Vertical diffusion for ' // 
     &                                   IPR_SPNAM( NIPR )
               IPROUT( NIPR,7 )   = NIPRVAR
               LPROCOUT( 7 )      = .TRUE.
            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'EMIS' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'EMIS_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Emissions of ' // 
     &                                   IPR_SPNAM( NIPR )
               IPROUT( NIPR,8 )   = NIPRVAR
               LPROCOUT( 8 )      = .TRUE.
            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'DDEP' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'DDEP_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Dry deposition for ' // 
     &                                   IPR_SPNAM( NIPR )
               IPROUT( NIPR,9 )   = NIPRVAR
               LPROCOUT( 9 )      = .TRUE.
            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'CLDS' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'CLDS_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Clouds for ' // 
     &                                   IPR_SPNAM( NIPR )
               IPROUT( NIPR,10 )   = NIPRVAR
               LPROCOUT( 10 )      = .TRUE.
            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'CHEM' ) THEN
               IPRNAME_TMP( NIPRVAR )  = 'CHEM_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR )  = 'Chemistry of ' // 
     &                                   IPR_SPNAM( NIPR )
               IPROUT( NIPR,11 )   = NIPRVAR
               LPROCOUT( 11 )      = .TRUE.
            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'AERO' ) THEN
               IPRNAME_TMP( NIPRVAR )  = 'AERO_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR )  = 'Aerosols for ' // 
     &                                    IPR_SPNAM( NIPR )
               IPROUT( NIPR,12 )   = NIPRVAR
               LPROCOUT( 12 )      = .TRUE.
            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'ADV2' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'ADV2_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'XY-Advection of ' //
     &                                   IPR_SPNAM( NIPR )
               IPROUT( NIPR,1 )  = NIPRVAR
               IPROUT( NIPR,2 )  = NIPRVAR
               LPROCOUT( 1 )     = .TRUE.
               LPROCOUT( 2 )     = .TRUE.
            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'ADV3' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'ADV3_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'XYZ-Advection of ' //
     &                                   IPR_SPNAM( NIPR )
               IPROUT( NIPR,1 )  = NIPRVAR
               IPROUT( NIPR,2 )  = NIPRVAR
               IPROUT( NIPR,3 )  = NIPRVAR
               LPROCOUT( 1 )     = .TRUE.
               LPROCOUT( 2 )     = .TRUE.
               LPROCOUT( 3 )     = .TRUE.
            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'MADV' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'MADV_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Horizontal and Vertical Advection of ' //
     &                                   IPR_SPNAM( NIPR )
               IPROUT( NIPR,5 )  = NIPRVAR
               IPROUT( NIPR,3 )  = NIPRVAR
               LPROCOUT( 5 )     = .TRUE.
               LPROCOUT( 3 )     = .TRUE.
            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'TADV' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'TADV_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Total Advection of ' //
     &                                   IPR_SPNAM( NIPR )
               IPROUT( NIPR,1 )  = NIPRVAR
               IPROUT( NIPR,2 )  = NIPRVAR
               IPROUT( NIPR,3 )  = NIPRVAR
               IPROUT( NIPR,4 )  = NIPRVAR
               LPROCOUT( 1 )     = .TRUE.
               LPROCOUT( 2 )     = .TRUE.
               LPROCOUT( 3 )     = .TRUE.
               LPROCOUT( 4 )     = .TRUE.
            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'TDIF' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'TDIF_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Total Diffusion of ' //
     &                                   IPR_SPNAM( NIPR )
               IPROUT( NIPR,6 )  = NIPRVAR
               IPROUT( NIPR,7 )  = NIPRVAR
               LPROCOUT( 6 )     = .TRUE.
               LPROCOUT( 7 )     = .TRUE.
            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'TRAN' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'TRAN_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Total Transport of ' //
     &                                   IPR_SPNAM( NIPR )
               IPROUT( NIPR,1 )  = NIPRVAR
               IPROUT( NIPR,2 )  = NIPRVAR
               IPROUT( NIPR,3 )  = NIPRVAR
               IPROUT( NIPR,4 )  = NIPRVAR
               IPROUT( NIPR,6 )  = NIPRVAR
               IPROUT( NIPR,7 )  = NIPRVAR
               LPROCOUT( 1 )     = .TRUE.
               LPROCOUT( 2 )     = .TRUE.
               LPROCOUT( 3 )     = .TRUE.
               LPROCOUT( 4 )     = .TRUE.
               LPROCOUT( 6 )     = .TRUE.
               LPROCOUT( 7 )     = .TRUE.
            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'TRNM' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'TRNM_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'Total Mass-Conserving Transport of ' //
     &                                   IPR_SPNAM( NIPR )
               IPROUT( NIPR,5 )  = NIPRVAR
               IPROUT( NIPR,3 )  = NIPRVAR
               IPROUT( NIPR,6 )  = NIPRVAR
               IPROUT( NIPR,7 )  = NIPRVAR
               LPROCOUT( 5 )     = .TRUE.
               LPROCOUT( 3 )     = .TRUE.
               LPROCOUT( 6 )     = .TRUE.
               LPROCOUT( 7 )     = .TRUE.
            ELSE IF ( IPR_OPNAME( NIPR,N )( 1:4 ) .EQ. 'PING' ) THEN
               IPRNAME_TMP( NIPRVAR ) = 'PING_' // IPR_SPNAM( NIPR )
               IPRDESC_TMP( NIPRVAR ) = 'PING Impact on ' // 
     &                                   IPR_SPNAM( NIPR )
               IPROUT( NIPR,13 )  = NIPRVAR
               LPROCOUT( 13 )     = .TRUE.
            END IF
         END DO
      END DO

      ALLOCATE( IPRNAME( NIPRVAR ),
     &          IPRDESC( NIPRVAR ), STAT = ASTAT )
      IF ( ASTAT .NE. 0 ) THEN
         MSG = 'ERROR 2 allocating IPR variables'
         CALL M3EXIT ( 'PA_SETUP_IPR', 0, 0, MSG, XSTAT2 )
      END IF

      IPRNAME = IPRNAME_TMP( 1:NIPRVAR )
      IPRDESC = IPRDESC_TMP( 1:NIPRVAR )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set up the species pointers for the IPR Outputs
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO NS = 1, N_SPC
         LSAVSP( NS ) = .FALSE.
      END DO

      ALLOCATE( NCGRID ( N_IPR_SPC ), STAT = ASTAT )
      IF ( ASTAT .NE. 0 ) THEN
         MSG = 'ERROR 3 allocating IPR variables'
         CALL M3EXIT ( 'PA_SETUP_IPR', 0, 0, MSG, XSTAT2 )
      END IF

      MXCGRID = 1
      DO NIPR = 1, N_IPR_SPC
         ISP = INDEX1( IPR_SPNAM( NIPR ), N_SPC, SPC_NAME )
         IF ( ISP .NE. 0 ) THEN
            LSAVSP( ISP ) = .TRUE.
            NCGRID( NIPR ) = 1
            IPR2GRD_TMP( NIPR,1 ) = ISP
            SPCOEF_TMP( NIPR,1 ) = 1.0
         ELSE
            IFM = INDEX1( IPR_SPNAM( NIPR ), NFAMLYS, FAMNAME )
            NCGRID( NIPR ) = NUMFAMMEM( IFM )
            MXCGRID = MAX( MXCGRID, NCGRID( NIPR ) )
            DO N = 1, NUMFAMMEM( IFM )
               ISP = INDEX1( FAMMEMNAM( IFM,N ), N_SPC, SPC_NAME )
               LSAVSP( ISP ) = .TRUE.               
               IPR2GRD_TMP( NIPR,N ) = ISP 
               SPCOEF_TMP( NIPR,N ) = FAMSC( IFM,N )
            END DO
         END IF
      END DO

      NCSAVE = 0
      DO NS = 1, N_SPC
         IF ( LSAVSP( NS ) ) THEN
            NCSAVE = NCSAVE + 1
            SV2GRD_TMP( NCSAVE ) = NS
         END IF
      END DO

      DO NIPR = 1, N_IPR_SPC
         DO N = 1, NCGRID( NIPR )
            DO NS = 1, NCSAVE
               IF ( SV2GRD_TMP( NS ) .EQ. IPR2GRD_TMP( NIPR,N ) )
     &              IPR2SAV_TMP( NIPR,N ) = NS
            END DO
         END DO
      END DO

      NCSAVE    = MAX( NCSAVE, 1 )
      ALLOCATE( SV2GRD ( NCSAVE ),
     &          IPR2GRD( N_IPR_SPC,MXCGRID ),
     &          IPR2SAV( N_IPR_SPC,MXCGRID ),
     &          SPCOEF ( N_IPR_SPC,MXCGRID ), STAT = ASTAT )
      IF ( ASTAT .NE. 0 ) THEN
         MSG = 'ERROR 4 allocating IPR variables'
         CALL M3EXIT ( 'PA_SETUP_IPR', 0, 0, MSG, XSTAT2 )
      END IF

      SV2GRD  = SV2GRD_TMP( 1:NCSAVE )
      IPR2GRD = IPR2GRD_TMP( 1:N_IPR_SPC,1:MXCGRID )
      IPR2SAV = IPR2SAV_TMP( 1:N_IPR_SPC,1:MXCGRID )
      SPCOEF  = SPCOEF_TMP ( 1:N_IPR_SPC,1:MXCGRID )

      RETURN

C----------------------- FORMAT Statements -----------------------------

94000 FORMAT( 'ERROR: Maximum number of IPR_OUTPUTs exceeded' )
94020 FORMAT( '       Modify PARAMETER ( MAXIPROUT =', I3,' ) or',
     &              ' decrease the number of IPR_OUTPUTs' )
94030 FORMAT( 'WARNING: Species name ', A, ' too long.' )
94031 FORMAT( '     Combined variable name will ',
     &        'exceed the I/O-API''s 16 character limit.' )
            
      END SUBROUTINE PA_SETUP_IPR

