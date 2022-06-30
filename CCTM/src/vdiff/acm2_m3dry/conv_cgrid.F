
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
      SUBROUTINE CONV_CGRID ( CGRID, JDATE, JTIME, CNGRD )

C-----------------------------------------------------------------------
C Function:
C   Convert decoupled aerosol species to molar units (ppm and m**2/mol)
C   and reorder dimensions

C Revision History:
C   Written by: J.Young 21 Aug 03
C   J.Young 31 Jan 05: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   01 Feb 19 D.Wong: Implemented centralized I/O approach, removed all
C                     MY_N clauses
C-----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN
      USE VDIFF_MAP
      use CENTRALIZED_IO_MODULE, only : interpolate_var
      
#ifdef isam
      USE SA_DEFN
#endif

      IMPLICIT NONE

      INCLUDE SUBST_CONST     ! constants
      INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:

      REAL, POINTER :: CGRID( :,:,:,: )            ! concentrations
      INTEGER, INTENT( IN )    :: JDATE            ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN )    :: JTIME            ! current model time, coded HHMMSS
      REAL,    INTENT( INOUT ) :: CNGRD( :,:,:,: ) ! cgrid replacement
#ifdef isam
      INTEGER IBGN, JSPCTAG
      LOGICAL, SAVE :: ISAM_FIRSTIME = .TRUE.
      LOGICAL, ALLOCATABLE, SAVE :: CONVERT_TAG_SPC ( : )
      REAL,    ALLOCATABLE, SAVE :: CFAC_TAG_SPC   ( : )
#endif

C Parameters:
      REAL, PARAMETER :: MWAIR_SI = MWAIR / 1.0E+03 ! kg mol-1

C Local Variables:

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      CHARACTER( 16 ), SAVE :: PNAME = 'CONV_CGRID'
      CHARACTER( 96 ) :: XMSG = ' '

      REAL      DENS( NCOLS,NROWS,NLAYS )  ! air density (kg m-3)
      REAL      FAC1( NCOLS,NROWS,NLAYS )  ! conversion factor

      INTEGER   NSPCS, OFF
      INTEGER   C, R, L, S, V, I           ! loop induction variables
      INTEGER IOS

      REAL    CONV            ! temp var

C-----------------------------------------------------------------------

      call interpolate_var ('DENS', jdate, jtime, DENS) ! kg m-3
      FAC1 = MWAIR_SI / DENS  ! m3 mol-1

C Convert non-molar mixing ratio species and re-order CGRID
      DO L = 1, NLAYS

        DO I = 1,N_CGRID_SPC
            ! Gas - no conversion
            IF ( CGRID_MASK_GAS( I ) .OR.
     &           CGRID_MASK_NR( I )  .OR.
     &           CGRID_MASK_TRAC( I ) )
     &         CNGRD( I,L,:,: ) = CGRID( :,:,L,I )

            ! Aerosol Mass: ug/m3 -> ppmv
            IF ( CGRID_MASK_AERO( I ) .AND.
     &           .NOT. CGRID_MASK_NUM( I ) .AND.
     &           .NOT. CGRID_MASK_SRF( I )  )
     &         CNGRD( I,L,:,: ) = CGRID( :,:,L,I ) 
     &                          * FAC1( :,:,L ) / CGRID_MW( I )

            ! Aerosol Number: N m-3 -> N mol-1
            ! Aerosol Surface Area: m2/m3 -> m2 / mol air
            IF ( CGRID_MASK_NUM( I ) .OR.
     &           CGRID_MASK_SRF( I )      )
     &         CNGRD( I,L,:,: ) = CGRID( :,:,L,I ) * FAC1( :,:,L )
        END DO
      
      END DO

#ifdef isam
      ! Convert PM Mass Only
      IF( ISAM_FIRSTIME )THEN
         ALLOCATE( CONVERT_TAG_SPC( N_SPCTAG ) )
         ALLOCATE( CFAC_TAG_SPC   ( N_SPCTAG ) )
         SET_CONV_SPCTAG: DO JSPCTAG = 1, N_SPCTAG
            IBGN = INDEX( VNAM_SPCTAG( JSPCTAG ), '_', BACK = .TRUE. ) - 1
            S = INDEX1( VNAM_SPCTAG( JSPCTAG )( 1:IBGN ), N_AE_SPC, AE_SPC )
            IF ( S .GT. 0 ) THEN
              CFAC_TAG_SPC( JSPCTAG )    = AE_MOLWT( S )
              CONVERT_TAG_SPC( JSPCTAG ) = .TRUE.  
            ELSE
              CFAC_TAG_SPC( JSPCTAG )    = 1.0
              CONVERT_TAG_SPC( JSPCTAG ) = .FALSE.
            END IF
         END DO SET_CONV_SPCTAG
         ISAM_FIRSTIME = .FALSE.
      END IF
      CONV_SPCTAG: DO JSPCTAG = 1, N_SPCTAG
         IF ( CONVERT_TAG_SPC( JSPCTAG ) ) THEN
            DO L = 1, NLAYS
               ISAM( :,:,L,S_SPCTAG( JSPCTAG ),T_SPCTAG( JSPCTAG ) ) =
     &            ISAM( :,:,L,S_SPCTAG( JSPCTAG ),T_SPCTAG( JSPCTAG ) )
     &              * FAC1( :,:,L ) / CFAC_TAG_SPC( JSPCTAG ) 
            END DO 
         END IF
      END DO CONV_SPCTAG
#endif

      RETURN

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      ENTRY REV_CGRID ( CNGRD, JDATE, JTIME, CGRID )

C Revert non-molar mixing ratio species and re-order CGRID

      call interpolate_var ('DENS', jdate, jtime, DENS) ! kg m-3
      FAC1 = DENS / MWAIR_SI  ! mol m-3

       DO L = 1, NLAYS
         DO I = 1,N_CGRID_SPC
             ! Gas - no conversion
             IF ( CGRID_MASK_GAS( I ) .OR.
     &            CGRID_MASK_NR( I )  .OR.
     &            CGRID_MASK_TRAC( I ) )
     &          CGRID( :,:,L,I ) = CNGRD( I,L,:,: )

             ! Aerosol Mass:  ppmv -> ug m-3
             IF ( CGRID_MASK_AERO( I ) .AND.
     &            .NOT. CGRID_MASK_NUM( I ) .AND.
     &            .NOT. CGRID_MASK_SRF( I )  )
     &          CGRID( :,:,L,I ) = CNGRD( I,L,:,: ) 
     &                           * FAC1( :,:,L ) * CGRID_MW( I )

             ! Aerosol Number: N mol-1 -> N m-3
             ! Aerosol Surface Area: m2 mol-1 -> m2 m-3
             IF ( CGRID_MASK_NUM( I ) .OR.
     &            CGRID_MASK_SRF( I )      )
     &          CGRID( :,:,L,I ) = CNGRD( I,L,:,: ) * FAC1( :,:,L )
         END DO
      END DO
#ifdef isam
Ckrt....JSPCTAG loop on PM mass only.....
      REV_SPCTAG: DO JSPCTAG = 1, N_SPCTAG
         IF ( CONVERT_TAG_SPC( JSPCTAG ) ) THEN
            DO L = 1, NLAYS
                ISAM( :,:,L,S_SPCTAG( JSPCTAG ),T_SPCTAG( JSPCTAG ) ) =
     &             ISAM( :,:,L,S_SPCTAG( JSPCTAG ),T_SPCTAG( JSPCTAG ) )
     &              * FAC1( :,:,L ) * CFAC_TAG_SPC( JSPCTAG ) 
            END DO
         END IF
      END DO REV_SPCTAG
#endif

      RETURN
      END

