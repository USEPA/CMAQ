
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
#endif

C Parameters:

      REAL, PARAMETER :: GPKG = 1.0E+03        ! g/Kg
      REAL, PARAMETER :: MAOGPKG = MWAIR / GPKG
      REAL, PARAMETER :: GPKGOMA = 1.0 / MAOGPKG
      REAL, PARAMETER :: MAOAVO1000 = 1.0E+03 * MWAIR / AVO
      REAL, PARAMETER :: AVOOMA_001 = 1.0 / MAOAVO1000

C Local Variables:

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      CHARACTER( 16 ), SAVE :: PNAME = 'CONV_CGRID'
      CHARACTER( 96 ) :: XMSG = ' '

      REAL      DENS( NCOLS,NROWS,NLAYS )  ! air density

      INTEGER   NSPCS, OFF
      INTEGER   C, R, L, S, V            ! loop induction variables
      INTEGER, SAVE :: NQAE              ! number of micro-grams/m**3 species
      INTEGER, SAVE :: NNAE              ! number of #/m**3 species
      INTEGER, SAVE :: NSAE              ! number of m**2/m**3 species
      INTEGER, ALLOCATABLE, SAVE :: QAE( : ) ! CGRID pointer to micro-grams/m**3 species
      INTEGER, ALLOCATABLE, SAVE :: NAE( : ) ! CGRID pointer to #/m**3 species
      INTEGER, ALLOCATABLE, SAVE :: SAE( : ) ! CGRID pointer to m**2/m**3 species
      REAL,    ALLOCATABLE, SAVE :: MOLWT( : ) ! only for "QAE" species
      INTEGER IOS

      REAL    CONV, FAC            ! temp var

C-----------------------------------------------------------------------

      call interpolate_var ('DENS', jdate, jtime, DENS)

C Convert non-molar mixing ratio species and re-order CGRID

C Gas - no conversion
       FORALL ( I = 1:N_SPC_DIFF, DIFF_MASK_GAS( I ) .OR.
     &                            DIFF_MASK_NR( I )  .OR.
     &                            DIFF_MASK_TRAC( I ) )
         DO L = 1, NLAYS
            CNGRD( I,L,:,: ) = CGRID( :,:,L,I )
         END DO
       END FORALL

C micro-grams/m**3 aerosol -> mol/mol air <- no
C micro-grams/m**3 aerosol -> ppmv
C (Don't divide by MGPG, then multiply by 1.0E+6: 1/MGPG = 10**-6 cancels out
C ppm = 10**6)
       FORALL ( I = 1:N_SPC_DIFF, DIFF_MASK_AERO( I ) .AND.
     &                           .NOT. DIFF_MASK_NUM( I ) .AND.
     &                           .NOT. DIFF_MASK_SRF( I )  )
     &    VDEMIS( I,1:NL,:,: ) = CNVTC( 1:NL,:,: ) * VDEMIS0( I,1:NL,:,:) * DIFF_MW( I )

      FORALL ( I = 1:N_SPC_DIFF, DIFF_MASK_SRF( I ) )
     &    VDEMIS( I,1:NL,:,: ) = CONVM( 1:NL,:,: ) * VDEMIS0( I,1:NL,:,:)

      FORALL ( I = 1:N_SPC_DIFF, DIFF_MASK_NUM( I ) )
     &    VDEMIS( I,1:NL,:,: ) = CONVN( 1:NL,:,: ) * VDEMIS0( I,1:NL,:,:)
 

      NSPCS = NQAE
      IF ( NSPCS .GT. 0 ) THEN
         DO R = 1, NROWS
            DO C = 1, NCOLS
               DO L = 1, NLAYS
                  FAC = MAOGPKG / DENS( C,R,L )
                  DO V = 1, NSPCS
                     CONV = FAC / MOLWT( V )
                     CNGRD( QAE( V ),L,C,R ) = CONV * CGRID( C,R,L,QAE( V ) )
                  END DO
               END DO
            END DO
         END DO
      END IF

C number/m**3 aerosol -> ppmv
C (Don't divide by MGPG, etc. See note above)

      NSPCS = NNAE
      IF ( NSPCS .GT. 0 ) THEN
         DO R = 1, NROWS
            DO C = 1, NCOLS
               DO L = 1, NLAYS
                  CONV = MAOAVO1000 / DENS( C,R,L )
                  DO V = 1, NSPCS
                     CNGRD( NAE( V ),L,C,R ) = CONV * CGRID( C,R,L,NAE( V ) )
                  END DO
               END DO
            END DO
         END DO
      END IF

C m**2/m**3 aerosol -> m**2/mol air

      NSPCS = NSAE
      IF ( NSPCS .GT. 0 ) THEN
         DO R = 1, NROWS
            DO C = 1, NCOLS
               DO L = 1, NLAYS
                  CONV = MAOGPKG / DENS( C,R,L )
                  DO V = 1, NSPCS
                     CNGRD( SAE( V ),L,C,R ) = CONV * CGRID( C,R,L,SAE( V ) )
                  END DO
               END DO
            END DO
         END DO
      END IF

C Non-reactives - no conversion

      NSPCS = N_NR_SPC
      IF ( NSPCS .GT. 0 ) THEN
         OFF = NR_STRT - 1
         DO R = 1, NROWS
            DO C = 1, NCOLS
               DO L = 1, NLAYS
                  DO V = 1, NSPCS
                     CNGRD( OFF+V,L,C,R ) = CGRID( C,R,L,OFF+V )
                  END DO
               END DO
            END DO
         END DO
      END IF

C Tracers - no conversion

      NSPCS = N_TR_SPC
      IF ( NSPCS .GT. 0 ) THEN
         OFF = TR_STRT - 1
         DO R = 1, NROWS
            DO C = 1, NCOLS
               DO L = 1, NLAYS
                  DO V = 1, NSPCS
                     CNGRD( OFF+V,L,C,R ) = CGRID( C,R,L,OFF+V )
                  END DO
               END DO
            END DO
         END DO
      END IF

#ifdef isam
Ckrt....JSPCTAG loop on PM mass only.....
!krt  write( *,* ) 'JSPCTAG, VNAM_SPCTAG'
      CONV_SPCTAG: DO JSPCTAG = 1, N_SPCTAG
         IBGN = INDEX( VNAM_SPCTAG( JSPCTAG ), '_', .FALSE. ) - 1
         S = INDEX1( VNAM_SPCTAG( JSPCTAG )( 1:IBGN ), N_AE_SPC, AE_SPC )
         IF ( S .GT. 0 ) THEN
!krt        write( *,* )  JSPCTAG , VNAM_SPCTAG( JSPCTAG )
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  DO L = 1, NLAYS
                     FAC = MAOGPKG / DENS( C,R,L )
                     CONV = FAC / AE_MOLWT( S )
                     ISAM( C,R,L,S_SPCTAG( JSPCTAG ),T_SPCTAG( JSPCTAG ) ) =
     &                  CONV * ISAM( C,R,L,S_SPCTAG( JSPCTAG ),T_SPCTAG( JSPCTAG ) )
                  END DO 
               END DO
            END DO
         END IF
      END DO CONV_SPCTAG
#endif

      RETURN

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      ENTRY REV_CGRID ( CNGRD, JDATE, JTIME, CGRID )

C Revert non-molar mixing ratio species and re-order CGRID

      call interpolate_var ('DENS', jdate, jtime, DENS)

C Gas - no conversion

      NSPCS = N_GC_SPC
      IF ( NSPCS .GT. 0 ) THEN
         OFF = GC_STRT - 1
         DO V = 1, NSPCS
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CGRID( C,R,L,OFF+V ) = CNGRD( OFF+V,L,C,R )
                  END DO
               END DO
            END DO
         END DO
      END IF

C aerosol ppmv -> micro-grams/m**3
C (Don't multiply by MGPG, then divide by 1.0E+6: 1/MGPG = 10**-6 cancels out
C ppm = 10**6)

      NSPCS = NQAE
      IF ( NSPCS .GT. 0 ) THEN
         DO V = 1, NSPCS
            FAC = GPKGOMA * MOLWT( V )
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CONV = FAC * DENS( C,R,L )
                     CGRID( C,R,L,QAE( V ) ) = CONV * CNGRD( QAE( V ),L,C,R )
                  END DO
               END DO
            END DO
         END DO
      END IF

C aerosol ppmv -> number/m**3
C (Don't multiply by MGPG, etc. See note above)

      NSPCS = NNAE
      IF ( NSPCS .GT. 0 ) THEN
         DO V = 1, NSPCS
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CONV = AVOOMA_001 * DENS( C,R,L )
                     CGRID( C,R,L,NAE( V ) ) = CONV * CNGRD( NAE( V ),L,C,R )
                  END DO
               END DO
            END DO
         END DO
      END IF

C m**2/m**3 aerosol -> m**2/mol air

      NSPCS = NSAE
      IF ( NSPCS .GT. 0 ) THEN
         DO V = 1, NSPCS
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CONV = GPKGOMA * DENS( C,R,L )
                     CGRID( C,R,L,SAE( V ) ) = CONV * CNGRD( SAE( V ),L,C,R )
                  END DO
               END DO
            END DO
         END DO
      END IF

C Non-reactives - no conversion

      NSPCS = N_NR_SPC
      IF ( NSPCS .GT. 0 ) THEN
         OFF = NR_STRT - 1
         DO V = 1, NSPCS
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CGRID( C,R,L,OFF+V ) = CNGRD( OFF+V,L,C,R )
                  END DO
               END DO
            END DO
         END DO
      END IF

C Tracers - no conversion

      NSPCS = N_TR_SPC
      IF ( NSPCS .GT. 0 ) THEN
         OFF = TR_STRT - 1
         DO V = 1, NSPCS
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CGRID( C,R,L,OFF+V ) = CNGRD( OFF+V,L,C,R )
                  END DO
               END DO
            END DO
         END DO
      END IF

#ifdef isam
Ckrt....JSPCTAG loop on PM mass only.....
      REV_SPCTAG: DO JSPCTAG = 1, N_SPCTAG
         IBGN = INDEX( VNAM_SPCTAG( JSPCTAG ), '_', .FALSE. ) - 1
         S = INDEX1( VNAM_SPCTAG( JSPCTAG )( 1:IBGN ), N_AE_SPC, AE_SPC )
         IF ( S .GT. 0 ) THEN
            FAC = GPKGOMA * AE_MOLWT( S )
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CONV = FAC * DENS( C,R,L )
                     ISAM( C,R,L,S_SPCTAG( JSPCTAG ),T_SPCTAG( JSPCTAG ) ) =
     &                  CONV * ISAM( C,R,L,S_SPCTAG( JSPCTAG ),T_SPCTAG( JSPCTAG ) )
                  END DO
               END DO
            END DO
         END IF
      END DO REV_SPCTAG
#endif

      RETURN
      END

