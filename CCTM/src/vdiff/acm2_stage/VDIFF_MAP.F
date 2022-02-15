
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
      MODULE VDIFF_MAP

C-----------------------------------------------------------------------
C Function: index mapping between CGRID, diffusion, dry dep, and emissions
C           species

C Module depends on prior call to DEPV_INIT to get NDEPV_SPC

C Revision History:
C   10 Apr 2008 J.Young: initial implementation using subroutine vdiff_map.F
C   21 Jun 10 J.Young: convert for Namelist redesign
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   11 May 11 J.Bash: Modified for NH3 bidi component fluxes
C   04 Jan 12 J.Young: Initalize char strings with blank padding

C-----------------------------------------------------------------------
      USE VDIFF_DATA

C global diffusion species
      !INTEGER, SAVE :: N_SPC_DIFF 
     
C global dry dep species
      INTEGER, SAVE :: N_SPC_DDEP 

      INTEGER, ALLOCATABLE, SAVE :: DIFF_MAP( : ) ! global diff map to CGRID
      INTEGER, ALLOCATABLE, SAVE :: DF2DV   ( : ) ! map from diff spc to depv spc
      INTEGER, ALLOCATABLE, SAVE :: DF2EM   ( : ) ! map from diff spc to emis spc
      INTEGER, ALLOCATABLE, SAVE :: DD2DV   ( : ) ! map from ddep spc to depv spc
      INTEGER, ALLOCATABLE, SAVE :: DEPV_MAP( : ) ! global depv map to CGRID
      INTEGER, ALLOCATABLE, SAVE :: DV2DF   ( : )  ! map from depv spc to diff spc

      ! These Masks Identify the Phase/Nature of Species in The Diffused
      ! Species Vector
      LOGICAL, ALLOCATABLE, SAVE :: DIFF_MASK_GAS ( : ) !Gases
      LOGICAL, ALLOCATABLE, SAVE :: DIFF_MASK_AERO( : ) !Particles
      LOGICAL, ALLOCATABLE, SAVE :: DIFF_MASK_NUM ( : ) !Particles
      LOGICAL, ALLOCATABLE, SAVE :: DIFF_MASK_SRF ( : ) !Particles
      LOGICAL, ALLOCATABLE, SAVE :: DIFF_MASK_NR  ( : ) !Non-Reactives
      LOGICAL, ALLOCATABLE, SAVE :: DIFF_MASK_TRAC( : ) !Tracers

      CHARACTER( 16 ), ALLOCATABLE, SAVE :: DDEP_SPC( : )
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: DEPV_SPC( : )
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: DV2DF_SPC( : )
      REAL,            ALLOCATABLE, SAVE :: DIFF_MW  ( : )

C component flux output variables 
      INTEGER,         SAVE :: N_BD_EMIS, NH3_E, HG_E, HONO_E
      CHARACTER( 16 ), SAVE :: BD_EMIS_SPC( 4 ) = ' '

      CONTAINS

C-----------------------------------------------------------------------
         FUNCTION VDIFF_MAP_INIT( ) RESULT ( SUCCESS )

         USE CGRID_SPCS   ! CGRID species number and offsets
         USE UTILIO_DEFN

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_EMISPRM   ! Emissions processing control parameters

C Arguments:
         LOGICAL SUCCESS

C External Functions:
         INTEGER, EXTERNAL :: FINDEX

C Local variables:
         
         CHARACTER( 80 )  :: XMSG       ! Message text
         CHARACTER( 16 )  :: PNAME = 'VDIFF_MAP'

         INTEGER STRT, FINI
         INTEGER S, V, N, I, INDX
         INTEGER ALLOCSTAT

         LOGICAL LERROR                    ! Error flag

!-----------------------------------------------------------------------
         SUCCESS = .TRUE.

         N_SPC_DIFF = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_DIFF
         
         ALLOCATE ( DIFF_MAP( N_SPC_DIFF+1 ),
     &              DF2DV( N_SPC_DIFF+1 ),
     &              DIFF_MW( N_SPC_DIFF+1 ),
     &              DIFF_SPC( N_SPC_DIFF+1 ), stat = allocstat )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DIFF_MAP, DF2DV or DIFF_SPC'
            CALL M3WARN( PNAME, 0, 0, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         N_SPC_DDEP = N_GC_DDEP + N_AE_DDEP + N_NR_DDEP + N_TR_DDEP

         ALLOCATE ( DD2DV( N_SPC_DDEP+1 ),
     &              DDEP_SPC( N_SPC_DDEP+1 ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DD2DV or DDEP_SPC'
            CALL M3WARN( PNAME, 0, 0, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         ALLOCATE ( DEPV_MAP( N_SPC_DEPV+1 ),
     &              DEPV_SPC( N_SPC_DEPV ),
     &              DV2DF( N_SPC_DEPV ),
     &              dv2df_spc( n_spc_depv ), stat = allocstat )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DEPV_MAP, DEPV_SPC, DV2DF or dv2df_spc'
            CALL M3WARN( PNAME, 0, 0, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

! Populate global maps to CGRID

! Map Diffusion Module Species to Deposition Velocity Species
         I = 0; S = 0; N = 0
         DO V = 1, N_GC_TRNS ! assumes dep.vel species subset of vdif species
            N = N + 1
            INDX = FINDEX( GC_TRNS_MAP( V ), N_GC_DEPV, GC_DEPV_MAP )
            IF ( INDX .GT. 0 ) THEN
               I = I + 1
               DF2DV( N ) = S + INDX
            ELSE
               DF2DV( N ) = N_SPC_DEPV + 1
            END IF
         END DO
         S = S + I     ! S should now be = N_GC_DEPV

         I = 0
         DO V = 1, N_AE_TRNS
            N = N + 1
            INDX = FINDEX( AE_TRNS_MAP( V ), N_AE_DEPV, AE_DEPV_MAP )
            IF ( INDX .GT. 0 ) THEN
               I = I + 1
               DF2DV( N ) = S + INDX
            ELSE
               DF2DV( N ) = N_SPC_DEPV + 1
            END IF
         END DO
         S = S + I     ! S should now be = N_GC_DEPV + N_AE_DEPV

         I = 0
         DO V = 1, N_NR_TRNS
            N = N + 1
            INDX = FINDEX( NR_TRNS_MAP( V ), N_NR_DEPV, NR_DEPV_MAP )
            IF ( INDX .GT. 0 ) THEN
               I = I + 1
               DF2DV( N ) = S + INDX
            ELSE
               DF2DV( N ) = N_SPC_DEPV + 1
            END IF
         END DO
         S = S + I     ! S should now be = N_GC_DEPV + N_AE_DEPV + N_NR_DEPV

         DO V = 1, N_TR_DIFF
            N = N + 1
            INDX = FINDEX( TR_DIFF_MAP( V ), N_TR_DEPV, TR_DEPV_MAP )
            IF ( INDX .GT. 0 ) THEN
               DF2DV( N ) = S + INDX
            ELSE
               DF2DV( N ) = N_SPC_DEPV + 1
            END IF
         END DO

! Map Deposition Velocity Species to Dry Deposition Module Species
         LERROR = .FALSE.
         S = 0; V = 0
         DO N = 1, N_GC_DDEP
            V = V + 1
            INDX = FINDEX ( GC_DDEP_MAP( N ), N_GC_DEPV, GC_DEPV_MAP )
            IF ( INDX .GT. 0 ) THEN
               DD2DV ( V ) = S + INDX
               DDEP_SPC( V ) = GC_DDEP( N )
            ELSE
               XMSG = 'ERROR: No deposition velocity for DDEP species ' //
     &               GC_DDEP( N )
               CALL M3MESG( XMSG )
               LERROR = .TRUE.
            END IF
         END DO

         S = N_GC_DEPV
         DO N = 1, N_AE_DDEP
            V = V + 1
            INDX = FINDEX ( AE_DDEP_MAP( N ), N_AE_DEPV, AE_DEPV_MAP )
            IF ( INDX .GT. 0 ) THEN
               DD2DV ( V ) = S + INDX
               DDEP_SPC( V ) = AE_DDEP( N )
            ELSE
               XMSG = 'ERROR: No deposition velocity for DDEP species ' //
     &               AE_DDEP( N )
               CALL M3MESG( XMSG )
               LERROR = .TRUE.
            END IF
         END DO

         S = N_GC_DEPV + N_AE_DEPV
         DO N = 1, N_NR_DDEP
            V = V + 1
            INDX = FINDEX ( NR_DDEP_MAP( N ), N_NR_DEPV, NR_DEPV_MAP )
            IF ( INDX .GT. 0 ) THEN
               DD2DV ( V ) = S + INDX
               DDEP_SPC( V ) = NR_DDEP( N )
            ELSE
               XMSG = 'ERROR: No deposition velocity for DDEP species ' //
     &               NR_DDEP( N )
               CALL M3MESG( XMSG )
               LERROR = .TRUE.
            END IF
         END DO

         S = N_GC_DEPV + N_AE_DEPV + N_NR_DEPV
         DO N = 1, N_TR_DEPV
            V = V + 1
            INDX = FINDEX ( TR_DDEP_MAP( N ), N_TR_DEPV, TR_DEPV_MAP )
            IF ( INDX .GT. 0 ) THEN
               DD2DV ( V ) = S + INDX
               DDEP_SPC( V ) = TR_DDEP( N )
            ELSE
               XMSG = 'ERROR: No deposition velocity for DDEP species ' //
     &               TR_DDEP( N )
               CALL M3MESG( XMSG )
               LERROR = .TRUE.
            END IF
         END DO
         NH3_E  = 0
         HG_ E  = 0
         HONO_E = 0
         IF ( ABFLUX ) THEN          
            NH3_E = 1
            N_BD_EMIS = NH3_E
            BD_EMIS_SPC( NH3_E ) = 'NH3_Emis'
         END IF
         IF ( HGBIDI ) THEN
            HG_E = NH3_E + 1 
            N_BD_EMIS = HG_E
            BD_EMIS_SPC( HG_E ) = 'HG_Emis'             
         END IF   
         IF ( SFC_HONO ) THEN
            HONO_E = max(NH3_E,HG_E) + 1   
            N_BD_EMIS = HONO_E
            BD_EMIS_SPC( HONO_E ) = 'HONO_Het '                      
         END IF    

! Populate Deposition Velocity Map and Species 
         S = 0
         DO V = 1, N_GC_DEPV
            S = S + 1
            DEPV_MAP( S ) = GC_STRT - 1 + GC_DEPV_MAP( V )
            DEPV_SPC( S ) = GC_SPC( GC_DEPV_MAP( V ) )
         END DO
         DO V = 1, N_AE_DEPV
            S = S + 1
            DEPV_MAP( S ) = AE_STRT - 1 + AE_DEPV_MAP( V )
            DEPV_SPC( S ) = AE_SPC( AE_DEPV_MAP( V ) )
         END DO
         DO V = 1, N_NR_DEPV
            S = S + 1
            DEPV_MAP( S ) = NR_STRT - 1 + NR_DEPV_MAP( V )
            DEPV_SPC( S ) = NR_SPC( NR_DEPV_MAP( V ) )
         END DO
         DO V = 1, N_TR_DEPV
            S = S + 1
            DEPV_MAP( S ) = TR_STRT - 1 + TR_DEPV_MAP( V )
            DEPV_SPC( S ) = TR_SPC( TR_DEPV_MAP( V ) )
         END DO

! Populate Diffusion Module Map and Species
         ALLOCATE( DIFF_MASK_GAS ( N_SPC_DIFF ) )
         ALLOCATE( DIFF_MASK_AERO( N_SPC_DIFF ) )
         ALLOCATE( DIFF_MASK_NUM( N_SPC_DIFF ) )
         ALLOCATE( DIFF_MASK_SRF( N_SPC_DIFF ) )
         ALLOCATE( DIFF_MASK_NR  ( N_SPC_DIFF ) )
         ALLOCATE( DIFF_MASK_TRAC( N_SPC_DIFF ) )
         DIFF_MASK_GAS  = .FALSE.
         DIFF_MASK_AERO = .FALSE.
         DIFF_MASK_NUM  = .FALSE.
         DIFF_MASK_SRF  = .FALSE.
         DIFF_MASK_NR   = .FALSE.
         DIFF_MASK_TRAC = .FALSE.
         DIFF_MAP = 0
         DIFF_SPC = ""
         DIFF_MW  = 0.

         S = 0
         DO V = 1, N_GC_TRNS
            S = S + 1
            DIFF_MAP( S ) = GC_STRT - 1 + GC_TRNS_MAP( V )
            DIFF_SPC( S ) = GC_SPC( GC_TRNS_MAP( V ) )
            DIFF_MW ( S ) = GC_MOLWT( GC_TRNS_MAP( V ) )
            DIFF_MASK_GAS( S ) = .TRUE.
         END DO
         DO V = 1, N_AE_TRNS
            S = S + 1
            DIFF_MAP( S ) = AE_STRT - 1 + AE_TRNS_MAP( V )
            DIFF_SPC( S ) = AE_SPC( AE_TRNS_MAP( V ) )
            DIFF_MW ( S ) = AE_MOLWT( AE_TRNS_MAP( V ) )
            DIFF_MASK_AERO( S ) = .TRUE.
            IF ( INDEX( DIFF_SPC( S ), 'NUM' ) .NE. 0 ) THEN
                DIFF_MASK_NUM( S ) = .TRUE.
            END IF
            IF ( INDEX( DIFF_SPC( S ), 'SRF' ) .NE. 0 ) THEN
                DIFF_MASK_SRF( S ) = .TRUE.
            END IF
         END DO
         DO V = 1, N_NR_TRNS
            S = S + 1
            DIFF_MAP( S ) = NR_STRT - 1 + NR_TRNS_MAP( V )
            DIFF_SPC( S ) = NR_SPC( NR_TRNS_MAP( V ) )
            DIFF_MW ( S ) = NR_MOLWT( NR_TRNS_MAP( V ) )
            DIFF_MASK_NR( S ) = .TRUE.
         END DO
         DO V = 1, N_TR_DIFF
            S = S + 1
            DIFF_MAP( S ) = TR_STRT - 1 + TR_DIFF_MAP( V )
            DIFF_SPC( S ) = TR_SPC( TR_DIFF_MAP( V ) )
            DIFF_MW ( S ) = TR_MOLWT( TR_DIFF_MAP( V ) )
            DIFF_MASK_TRAC( S ) = .TRUE.
         END DO

! Map Deposition Velocity Species to Diffusion Module Species
         S = 0; V = 0
         DO N = 1, N_GC_DEPV
            V = V + 1
            INDX = FINDEX ( GC_DEPV_MAP( N ), N_GC_TRNS, GC_TRNS_MAP )
            IF ( INDX .GT. 0 ) THEN
               DV2DF ( V ) = S + INDX
               dv2df_spc( v ) = gc_trns( indx )
            ELSE
               XMSG = 'ERROR: deposition velocity specified for non-diff species '
     &               // GC_SPC( GC_DEPV_MAP( N ) )
               CALL M3MESG( XMSG )
               LERROR = .TRUE.
            END IF
         END DO

         S = N_GC_TRNS
         DO N = 1, N_AE_DEPV
            V = V + 1
            INDX = FINDEX ( AE_DEPV_MAP( N ), N_AE_TRNS, AE_TRNS_MAP )
            IF ( INDX .GT. 0 ) THEN
               DV2DF ( V ) = S + INDX
               dv2df_spc( v ) = ae_trns( indx )
            ELSE
               XMSG = 'ERROR: deposition velocity specified for non-diff species '
     &               // AE_SPC( AE_DEPV_MAP( N ) )
               CALL M3MESG( XMSG )
               LERROR = .TRUE.
            END IF
         END DO

         S = N_GC_TRNS + N_AE_TRNS
         DO N = 1, N_NR_DEPV
            V = V + 1
            INDX = FINDEX ( NR_DEPV_MAP( N ), N_NR_TRNS, NR_TRNS_MAP )
            IF ( INDX .GT. 0 ) THEN
               DV2DF ( V ) = S + INDX
               dv2df_spc( v ) = nr_trns( indx )
            ELSE
               XMSG = 'ERROR: deposition velocity specified for non-diff species '
     &               // NR_SPC( NR_DEPV_MAP( N ) )
               CALL M3MESG( XMSG )
               LERROR = .TRUE.
            END IF
         END DO

         S = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS
         DO N = 1, N_TR_DEPV
            V = V + 1
            INDX = FINDEX ( TR_DEPV_MAP( N ), N_TR_DIFF, TR_DIFF_MAP )
            IF ( INDX .GT. 0 ) THEN
               DV2DF ( V ) = S + INDX
               dv2df_spc( v ) = tr_diff( indx )
            ELSE
               XMSG = 'ERROR: deposition velocity specified for non-diff species '
     &               // TR_SPC( TR_DEPV_MAP( N ) )
               CALL M3MESG( XMSG )
               LERROR = .TRUE.
            END IF
         END DO

! Check for Errors and Exit
         IF ( LERROR ) THEN
            XMSG = '*** Species mapping error(s) in VDIFF_MAP'
            CALL M3WARN( PNAME, 0, 0, XMSG )
            SUCCESS = .FALSE.
         END IF

         RETURN

         END FUNCTION VDIFF_MAP_INIT

      END MODULE VDIFF_MAP
