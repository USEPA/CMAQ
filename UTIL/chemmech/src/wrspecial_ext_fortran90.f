      SUBROUTINE WRSPECIAL_EXT_FORTRAN90( WRUNIT )

C   Suboutine adds pointer and coefficient values used to calculate the
C   special rate coefficients

c   Modified 7/09 by J. Gipson to close files in no special reation coeffs

      USE MECHANISM_DATA
      
      IMPLICIT NONE
!      INCLUDE 'PARMS.e'

C INPUTS
      INTEGER,           INTENT( IN ) ::  WRUNIT     ! logical write unit no.


C LOCAL

      REAL( 8 )       :: WREXT_COEFFS( MSPECTERMS)
      INTEGER         :: WREXT_INDEX(  MSPECTERMS)

      INTEGER, EXTERNAL :: JUNIT
 
      CHARACTER( 20 ) :: BUFF20( MAXRXNUM )

      INTEGER LUNOUT
      INTEGER I, ISPC, IRX, IFLD0, IFLD1, IFLD2


      INTERFACE
        SUBROUTINE WRBF6( WRUNIT, AWPL, NEL, IVAR )
         INTEGER, INTENT( IN ) ::  WRUNIT     ! logical write unit no.
         INTEGER, INTENT( IN ) ::  AWPL       ! words per line (max at 10)
         INTEGER, INTENT( IN ) ::  NEL        ! number of list elements
         INTEGER, INTENT( IN ) ::  IVAR( : )  ! integer variable to write
         END SUBROUTINE WRBF6
        SUBROUTINE WRBF6_FORTRAN90( WRUNIT, AWPL, NEL, IVAR )
         INTEGER, INTENT( IN ) ::  WRUNIT     ! logical write unit no.
         INTEGER, INTENT( IN ) ::  AWPL       ! words per line (max at 10)
         INTEGER, INTENT( IN ) ::  NEL        ! number of list elements
         INTEGER, INTENT( IN ) ::  IVAR( : )  ! integer variable to write
        END SUBROUTINE WRBF6_FORTRAN90      
        SUBROUTINE WRBF12S ( WRUNIT, AWPL, NEL, VAR, AFMT )
           INTEGER, INTENT( IN )         :: WRUNIT   ! logical write unit no.
           INTEGER, INTENT( IN )         :: AWPL     ! words per line (max at 5)
           INTEGER, INTENT( IN )         :: NEL                       ! number of list elements
           REAL,    INTENT( IN )         :: VAR( : )   ! real variable to write
           CHARACTER(  1 ), INTENT( IN ) :: AFMT   ! write format: E -> 1PE11.4, F -> F11.5
        END SUBROUTINE WRBF12S
        SUBROUTINE WRBF12S_FORTRAN90 ( WRUNIT, AWPL, NEL, VAR, AFMT )
           INTEGER, INTENT( IN )         :: WRUNIT   ! logical write unit no.
           INTEGER, INTENT( IN )         :: AWPL     ! words per line (max at 5)
           INTEGER, INTENT( IN )         :: NEL                       ! number of list elements
           REAL,    INTENT( IN )         :: VAR( : )   ! real variable to write
           CHARACTER(  1 ), INTENT( IN ) :: AFMT   ! write format: E -> 1PE11.4, F -> F11.5
        END SUBROUTINE WRBF12S_FORTRAN90
        SUBROUTINE WRBF16C_FORTRAN90 ( WRUNIT, AWPL, NEL, VAR )
          INTEGER,         INTENT( IN ) :: WRUNIT      ! logical write unit no.
          INTEGER,         INTENT( IN ) :: AWPL        ! words per line (max at 5)
          INTEGER,         INTENT( IN ) :: NEL         ! number of list elements
          CHARACTER( 16 ), INTENT( IN ) :: VAR( : )  ! character variable to write
        END SUBROUTINE WRBF16C_FORTRAN90 
        SUBROUTINE WRBF16C ( WRUNIT, AWPL, NEL, VAR )
          INTEGER,         INTENT( IN ) :: WRUNIT      ! logical write unit no.
          INTEGER,         INTENT( IN ) :: AWPL        ! words per line (max at 5)
          INTEGER,         INTENT( IN ) :: NEL         ! number of list elements
          CHARACTER( 16 ), INTENT( IN ) :: VAR( : )  ! character variable to write
        END SUBROUTINE WRBF16C
      END INTERFACE


3300  WRITE( WRUNIT, 1049 )
1049  FORMAT( /'!', 4X, 'NSPECIAL     = Number of special rate coefficients', 
     &        /'!', 4X, 'SPECIAL      = Names of special rate coefficients', 
     &        /'!', 4X, 'NSPECIAL_RXN = Number of reactions with special rates'
     &        /'!', 4X, 'ISPECIAL     = Pointers to reactions using special rates'
     &              1X, 'and their special rate coefficients'
     &        /'!', 4X, 'MAXSPECTERMS = Max Number of terms type used by',
     &              1X, 'special rate coefficients', 
     &        /'!', 4X, 'KC_COEFFS    = Coefficients of standard rate coefficients ', 
     &              1X, 'times concentration terms '
     &        /'!', 4X, 'INDEX_KTERMS  = Pointers to standard rate coefficients in '
     &              1X, 'special rate coefficients',
     &        /'!', 4X, 'INDEX_CTERMS  = Pointers to species concentrations in '
     &              1X, 'special rate coefficients',
     &        /'!', 4X, 'OPERATOR_COEFFS = Coefficients of preceeding special ', 
     &              1X, 'rate coefficients used in special coefficient ',
     &        /'!', 4X, 'OPERATORS       = Pointers to preceeding special ', 
     &              1X, 'rate coefficients used in special coefficient ')



c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     NSPECIAL_RXN, ISPECIAL
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      IF ( NSPECIAL_RXN .NE. 0 ) THEN

         WRITE( WRUNIT, 2701 ) NSPECIAL_RXN
2701     FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NSPECIAL_RXN =', I4 )

         WRITE( WRUNIT, 2703 )
2703     FORMAT(  6X, 'INTEGER', 12X, ':: ISPECIAL( NSPECIAL_RXN,2 )' )

         WRITE( WRUNIT, 2705 ) '1'
2705     FORMAT( /6X, 'DATA ( ISPECIAL( IRXXN,', A, ' ), IRXXN = 1, NSPECIAL_RXN ) / & ' )

         CALL WRBF6_FORTRAN90( WRUNIT, 10, NSPECIAL_RXN, ISPECIAL( :,1 ) )

         WRITE( WRUNIT, 2705 ) '2'

         CALL WRBF6_FORTRAN90( WRUNIT, 10, NSPECIAL_RXN, ISPECIAL( :,2 ) )


      ELSE


         WRITE( WRUNIT, 2707 )
2707     FORMAT( /'! Special Rate information not available ..'
     &           /6X, 'INTEGER, PARAMETER', 1X, ':: NSPECIAL_RXN = 0' )


         WRITE( WRUNIT, 2708 )
2708     FORMAT(  6X, 'INTEGER', 12X, ':: ISPECIAL( 1, 2 )' )

      END IF


c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     4th Common Block (character)
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c        SPECIAL
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      IF ( NSPECIAL .NE. 0 ) THEN

         WRITE( WRUNIT, 2711 ) NSPECIAL
2711     FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NSPECIAL =', I4 )

         WRITE( WRUNIT, 2713 )
2713     FORMAT(  6X, 'CHARACTER( 16 )', 4X, ':: SPECIAL( NSPECIAL )' )

         WRITE( WRUNIT, 2715 )
2715     FORMAT( /6X, 'DATA ( SPECIAL( IRXXN ), IRXXN = 1, NSPECIAL ) / & ' )

         DO IRX = 1, NSPECIAL - 1
            WRITE( BUFF20( IRX ), '(1X, "''", A16, "''", ",")' ) SPECIAL( IRX )
         END DO
         WRITE( BUFF20( NSPECIAL ), '(1X, "''", A16, "''", "/")' ) SPECIAL( NSPECIAL )

         IFLD1 = 1
         DO IFLD0 = 1, (NSPECIAL / 3 - 1)
            IFLD2 = IFLD1 + 2
            IF( IFLD2 .EQ. NSPECIAL )EXIT
            WRITE( WRUNIT, 2759 ) ( BUFF20( IRX ), IRX = IFLD1, IFLD2 )
2759        FORMAT(5X, '&', 2X, 3A20, ' & ' )
            IFLD1 = IFLD2 + 1
         END DO
         IF ( IFLD1 .LE. NSPECIAL )
     &      WRITE( WRUNIT, 2769 ) ( BUFF20( IRX ), IRX = IFLD1, NSPECIAL )
2769        FORMAT(5X, '&', 2X, 3A20 )

      ELSE

         WRITE( WRUNIT, 2717 )
2717     FORMAT( /'! Special Rate information not available ...'
     &           /6X, 'INTEGER, PARAMETER', 1X, ':: NSPECIAL = 0' )

                                                
         WRITE( WRUNIT, 2719 )
2719     FORMAT( /'! Special Rate information not available ...'
     &           /6X, 'CHARACTER( 16 )', 4X, ':: SPECIAL( 1 )' )

C        RETURN

      END IF

      LUNOUT = WRUNIT
 
      WRITE( LUNOUT, 4401 ) MSPECTERMS
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



      IF ( NSPECIAL .EQ. 0 ) THEN
          RETURN
      ENDIF


c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     write to end of RXDT.EXT
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-



3350  LUNOUT = WRUNIT
 

      
      DO 3701 ISPC = 1, NSPECIAL

         WRITE( LUNOUT, 3407 )ISPC
3407     FORMAT( /6X, 'DATA ( KC_COEFFS( ', I3, ',IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & ' )

         DO I = 1, MSPECTERMS
            WREXT_COEFFS( I ) = KC_COEFFS( ISPC, I )
         END DO
         CALL WRBF12D_FORTRAN90( LUNOUT, 5, MSPECTERMS, WREXT_COEFFS, 'D' )

         WRITE( LUNOUT, 3408 ) ISPC
3408     FORMAT( /6X, 'DATA ( INDEX_KTERMS(' I3, ',  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & ' )
        
         DO I = 1, MSPECTERMS
            WREXT_INDEX( I ) = INDEX_KTERM( ISPC, I)
         ENDDO
       
         CALL WRBF6_FORTRAN90( LUNOUT, 10, MSPECTERMS, WREXT_INDEX )

         WRITE( LUNOUT, 3409 ) ISPC
3409     FORMAT( /6X, 'DATA ( INDEX_CTERMS(' I3, ',  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & ' )
        
         DO I = 1, MSPECTERMS
            IF( INDEX_CTERM( ISPC, I) .GT. 0 )THEN
               IF( REORDER_SPECIES )THEN
                   WREXT_INDEX( I ) = IOLD2NEW( INDEX_CTERM( ISPC, I) )
               ELSE
                   WREXT_INDEX( I ) = INDEX_CTERM( ISPC, I)
               END IF
            ELSE
               WREXT_INDEX( I ) = 0 
            END IF
         ENDDO
       
         CALL WRBF6_FORTRAN90( LUNOUT, 10, MSPECTERMS, WREXT_INDEX )

3701   CONTINUE

      DO 3702 ISPC = 1, NSPECIAL

         WRITE( LUNOUT, 3410 )ISPC
3410     FORMAT( /6X, 'DATA ( OPERATOR_COEFFS( ', I3, ',IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & ' )

         DO I = 1, MSPECTERMS
            WREXT_COEFFS( I ) = OPERATOR_COEFFS( ISPC, I )
         END DO
         CALL WRBF12D_FORTRAN90( LUNOUT, 5, MSPECTERMS, WREXT_COEFFS, 'D' )

         WRITE( LUNOUT, 3411 ) ISPC
3411     FORMAT( /6X, 'DATA ( OPERATORS(' I3, ',  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & ' )
        
         DO I = 1, MSPECTERMS
            WREXT_INDEX( I ) = OPERATORS( ISPC, I)
         ENDDO
       
         CALL WRBF6_FORTRAN90( LUNOUT, 10, MSPECTERMS, WREXT_INDEX )

3702   CONTINUE


       RETURN
       END
