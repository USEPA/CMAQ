      SUBROUTINE WRSPECIAL_EXT( NSPECIAL, NSPECIAL_RXN, SPECIAL,
     &                          ISPECIAL, INDEX_KTERM, INDEX_CTERM,
     &                          KC_COEFFS, OPERATORS, OPERATOR_COEFFS)

C   Suboutine adds pointer and coefficient values used to calculate the
C   special rate coefficients

c   Modified 7/09 by J. Gipson to close files in no special reation coeffs

      IMPLICIT NONE

      INCLUDE 'PARMS.e'

C INPUTS

      INTEGER         :: NSPECIAL
      INTEGER         :: NSPECIAL_RXN
      CHARACTER( 16 ) :: SPECIAL( MAXSPECRXNS )
      INTEGER         :: ISPECIAL( MAXSPECRXNS,2 )
      INTEGER         :: NKC_TERMS(  MAXSPECRXNS )
      CHARACTER( 16 ) :: KC_TERMS(   MAXSPECRXNS,  MAXSPECTERMS, 2)
      INTEGER         :: INDEX_KTERM( MAXSPECRXNS, MAXSPECTERMS)
      INTEGER         :: INDEX_CTERM( MAXSPECRXNS, MAXSPECTERMS)
      REAL( 8 )       :: KC_COEFFS(  MAXSPECRXNS,  MAXSPECTERMS)
      INTEGER         :: N_OPERATORS( MAXSPECRXNS )
      INTEGER         :: OPERATORS( MAXSPECRXNS, MAXSPECTERMS )
      REAL( 8 )       :: OPERATOR_COEFFS( MAXSPECRXNS, MAXSPECTERMS)

C LOCAL

      REAL( 8 )       :: WREXT_COEFFS( MAXSPECTERMS)
      INTEGER         :: WREXT_INDEX(  MAXSPECTERMS)
      LOGICAL         :: READ_FILE

      CHARACTER(   1 ) :: XXX

      INTEGER, EXTERNAL :: JUNIT
      EXTERNAL NAMEVAL
 
      CHARACTER( 120 ) :: EQNAME_RXDT
      CHARACTER( 120 ) :: EQNAME_RXCM

      CHARACTER(  50 ) :: EXHEAD_RXDT
      CHARACTER(  52 ) :: EXHEAD_RXCM

      CHARACTER(  12 ) :: EXFLNM_RXDT = 'RXNSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXCM = 'RXNSCOMX'

      CHARACTER( 20 ) :: BUFF20( MAXRXNUM )

      INTEGER EXUNIT_SPCS
      INTEGER EXUNIT_RXDT
      INTEGER EXUNIT_RXCM

      INTEGER LUNOUT
      INTEGER I, ISPC, IRX, IRXXN, IFLD0, IFLD1, IFLD2

      EXUNIT_RXDT = JUNIT()
      EXUNIT_RXCM = JUNIT()
c symbolic link locates "EXFLNM_..."; setenv requires INQUIRE (NAMEVAL):
      CALL NAMEVAL ( EXFLNM_RXDT, EQNAME_RXDT )
      CALL NAMEVAL ( EXFLNM_RXCM, EQNAME_RXCM )

      OPEN ( UNIT = EXUNIT_RXDT, FILE = EQNAME_RXDT, STATUS = 'OLD' )
      OPEN ( UNIT = EXUNIT_RXCM, FILE = EQNAME_RXCM, STATUS = 'OLD' )

      READ_FILE = .TRUE.

      DO WHILE(READ_FILE)
         READ( EXUNIT_RXCM, '( A )', END = 4200)XXX
      ENDDO

4200  DO WHILE(READ_FILE)
         READ( EXUNIT_RXDT, '( A )', END = 3300)XXX
      ENDDO

3300  WRITE( EXUNIT_RXCM, 1049 )
1049  FORMAT( /'C', 4X, 'NSPECIAL     = Number of special rate coefficients', 
     &        /'C', 4X, 'SPECIAL      = Names of special rate coefficients', 
     &        /'C', 4X, 'NSPECIAL_RXN = Number of reactions with special rates'
     &        /'C', 4X, 'ISPECIAL     = Pointers to reactions using special rates'
     &              1X, 'and their special rate coefficients'/'C', 4X, 'MAXSPECTERMS = Max Number of each term type in ',
     &              1X, 'special rate coefficients', 
     &        /'C', 4X, 'KC_COEFFS    = Coefficients of standard rate coefficients ', 
     &              1X, 'times concentration terms '
     &        /'C', 4X, 'INDEX_KTERMS  = Pointers to standard rate coefficients in '
     &              1X, 'special rate coefficients',
     &        /'C', 4X, 'INDEX_CTERMS  = Pointers to species concentrations in '
     &              1X, 'special rate coefficients',
     &        /'C', 4X, 'OPERATOR_COEFFS = Coefficients of preceeding special ', 
     &              1X, 'rate coefficients used in special coefficient ',
     &        /'C', 4X, 'OPERATORS       = Pointers to preceeding special ', 
     &              1X, 'rate coefficients used in special coefficient ')


c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     NSPECIAL_RXN, ISPECIAL
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      IF ( NSPECIAL_RXN .NE. 0 ) THEN

         WRITE( EXUNIT_RXCM, 2701 ) NSPECIAL_RXN
2701     FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NSPECIAL_RXN =', I4 )

         WRITE( EXUNIT_RXCM, 2703 )
2703     FORMAT(  6X, 'INTEGER', 12X, ':: ISPECIAL( NSPECIAL_RXN,2 )' )

         WRITE( EXUNIT_RXDT, 2705 ) '1'
2705     FORMAT( /6X, 'DATA ( ISPECIAL( IRXXN,', A, ' ), IRXXN = 1, NSPECIAL_RXN ) /' )

         CALL WRBF6 ( EXUNIT_RXDT, 10, NSPECIAL_RXN, ISPECIAL( 1,1 ) )

         WRITE( EXUNIT_RXDT, 2705 ) '2'

         CALL WRBF6 ( EXUNIT_RXDT, 10, NSPECIAL_RXN, ISPECIAL( 1,2 ) )


      ELSE

         WRITE( EXUNIT_RXCM, 2707 )
2707     FORMAT( /'C Special Rate information not available ..'
     &           /6X, 'INTEGER, PARAMETER', 1X, ':: NSPECIAL_RXN = 0' )

         WRITE( EXUNIT_RXCM, 2708 )
2708     FORMAT(  6X, 'INTEGER', 12X, ':: ISPECIAL( 1, 2 )' )

      END IF

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     4th Common Block (character)
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c        SPECIAL
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      IF ( NSPECIAL .NE. 0 ) THEN

         WRITE( EXUNIT_RXCM, 2711 ) NSPECIAL
2711     FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NSPECIAL =', I4 )

         WRITE( EXUNIT_RXCM, 2713 )
2713     FORMAT(  6X, 'CHARACTER( 16 )', 4X, ':: SPECIAL( NSPECIAL )' )

         WRITE( EXUNIT_RXDT, 2715 )
2715     FORMAT( /6X, 'DATA ( SPECIAL( IRXXN ), IRXXN = 1, NSPECIAL ) /' )

         DO IRX = 1, NSPECIAL - 1
            WRITE( BUFF20( IRX ), '(1X, "''", A16, "''", ",") )' ) SPECIAL( IRX )
         END DO
         WRITE( BUFF20( NSPECIAL ), '(1X, "''", A16, "''", "/") )' ) SPECIAL( NSPECIAL )

         IFLD1 = 1
         DO IFLD0 = 1, NSPECIAL / 3
            IFLD2 = IFLD1 + 2
            WRITE( EXUNIT_RXDT, 2759 ) ( BUFF20( IRX ), IRX = IFLD1, IFLD2 )
2759        FORMAT(5X, '&', 2X, 4A20 )
            IFLD1 = IFLD2 + 1
         END DO
         IF ( IFLD1 .LE. NSPECIAL )
     &      WRITE( EXUNIT_RXDT, 2759 ) ( BUFF20( IRX ), IRX = IFLD1, NSPECIAL )

      ELSE

         WRITE( EXUNIT_RXCM, 2717 )
2717     FORMAT( /'C Special Rate information not available ...'
     &           /6X, 'INTEGER, PARAMETER', 1X, ':: NSPECIAL = 0' )
                                                
         WRITE( EXUNIT_RXCM, 2719 )
2719     FORMAT( /'C Special Rate information not available ...'
     &           /6X, 'CHARACTER( 16 )', 4X, ':: SPECIAL( 1 )' )

C        RETURN

      END IF

      LUNOUT = EXUNIT_RXCM

      WRITE( LUNOUT, 4401 ) MAXSPECTERMS
4401  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: MAXSPECTERMS =', I4 )

      WRITE( LUNOUT, 4402 )
4402  FORMAT(  6X, 'REAL( 8 )', 10X, ':: KC_COEFFS( NSPECIAL + 1, MAXSPECTERMS)' )

      WRITE( LUNOUT, 4403 )
4403  FORMAT(  6X, 'INTEGER', 12X, ':: INDEX_KTERMS( NSPECIAL + 1, MAXSPECTERMS)' )

      WRITE( LUNOUT, 4404 )
4404  FORMAT(  6X, 'INTEGER', 12X, ':: INDEX_CTERMS( NSPECIAL + 1, MAXSPECTERMS)' )

      WRITE( LUNOUT, 4406 )
4406  FORMAT(  6X, 'REAL( 8 )', 10X, ':: OPERATOR_COEFFS( NSPECIAL + 1, MAXSPECTERMS)' )

      WRITE( LUNOUT, 4407 )
4407  FORMAT(  6X, 'INTEGER', 12X, ':: OPERATORS( NSPECIAL + 1, MAXSPECTERMS)' )

         WRITE( EXUNIT_RXCM, 2807 )
2807     FORMAT( /5X, ' COMMON     / MECHRX4 /'
     &           /5X, '&             SPECIAL,',
     &           /5X, '&             ISPECIAL,',
     &           /5X, '&             KC_COEFFS,',           
     &           /5X, '&             INDEX_KTERMS,',
     &           /5X, '&             INDEX_CTERMS,',
     &           /5X, '&             OPERATOR_COEFFS,',
     &           /5X, '&             OPERATORS')

      IF ( NSPECIAL .EQ. 0 ) THEN
         CLOSE( EXUNIT_RXDT )
         CLOSE( EXUNIT_RXCM )
         RETURN
      ENDIF

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     write to end of RXDT.EXT
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-



3350  LUNOUT = EXUNIT_RXDT
      
      DO 3701 ISPC = 1, NSPECIAL

         WRITE( LUNOUT, 3407 )ISPC
3407     FORMAT( /6X, 'DATA ( KC_COEFFS( ', I3, ',IRXXN ), IRXXN = 1, MAXSPECTERMS ) /' )

         DO I = 1, MAXSPECTERMS
            WREXT_COEFFS( I ) = KC_COEFFS( ISPC, I )
         END DO
         CALL WRBF12D ( LUNOUT, 5, MAXSPECTERMS, WREXT_COEFFS, 'D' )

         WRITE( LUNOUT, 3408 ) ISPC
3408     FORMAT( /6X, 'DATA ( INDEX_KTERMS(' I3, ',  IRXXN), IRXXN = 1, MAXSPECTERMS ) /' )
        
         DO I = 1, MAXSPECTERMS
            WREXT_INDEX( I ) = INDEX_KTERM( ISPC, I)
         ENDDO
       
         CALL WRBF6 ( LUNOUT, 10, MAXSPECTERMS, WREXT_INDEX )

         WRITE( LUNOUT, 3409 ) ISPC
3409     FORMAT( /6X, 'DATA ( INDEX_CTERMS(' I3, ',  IRXXN), IRXXN = 1, MAXSPECTERMS ) /' )
        
         DO I = 1, MAXSPECTERMS
            WREXT_INDEX( I ) = INDEX_CTERM( ISPC, I)
         ENDDO
       
         CALL WRBF6 ( LUNOUT, 10, MAXSPECTERMS, WREXT_INDEX )

3701   CONTINUE

      DO 3702 ISPC = 1, NSPECIAL

         WRITE( LUNOUT, 3410 )ISPC
3410     FORMAT( /6X, 'DATA ( OPERATOR_COEFFS( ', I3, ',IRXXN ), IRXXN = 1, MAXSPECTERMS ) /' )

         DO I = 1, MAXSPECTERMS
            WREXT_COEFFS( I ) = OPERATOR_COEFFS( ISPC, I )
         END DO
         CALL WRBF12D ( LUNOUT, 5, MAXSPECTERMS, WREXT_COEFFS, 'D' )

         WRITE( LUNOUT, 3411 ) ISPC
3411     FORMAT( /6X, 'DATA ( OPERATORS(' I3, ',  IRXXN), IRXXN = 1, MAXSPECTERMS ) /' )
        
         DO I = 1, MAXSPECTERMS
            WREXT_INDEX( I ) = OPERATORS( ISPC, I)
         ENDDO
       
         CALL WRBF6 ( LUNOUT, 10, MAXSPECTERMS, WREXT_INDEX )

3702   CONTINUE

       CLOSE( EXUNIT_RXDT )
       CLOSE( EXUNIT_RXCM )

       RETURN
       END
