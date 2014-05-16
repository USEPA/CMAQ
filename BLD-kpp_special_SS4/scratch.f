       DO NXX = 1, NR
         DO IREACT = 1, NREACT( NXX )
            ISPC = IRR( NXX, IREACT )
               IF( IREACT .LT. 2 )THEN
                  REACTION_STR( NXX ) =  TRIM(SPCLIS( ISPC )) // ' '
               ELSE
                  REACTION_STR( NXX ) = REACTION_STR( NXX ) //  ' + ' // TRIM(SPCLIS( ISPC )) // ' '
               END IF
         END DO
         IF( INDEX_FIXED_SPECIES( NXX ) .GT. 0 .AND. INDEX_FIXED_SPECIES( NXX ) .LT. 7 )THEN
              ISPC = INDEX_FIXED_SPECIES( NXX )
              REACTION_STR( NXX ) = REACTION_STR( NXX ) //  ' + '  TRIM(FIXED_SPECIES( ISPC )) // ' '
         ELSE 
              IF( INDEX_FIXED_SPECIES( NXX ) .GT. 0 )THEN
                  WRITE(*,*)'WARNING: INDEX_FIXED_SPECIES( ', NXX,') = ',INDEX_FIXED_SPECIES( NXX )
              END IF
         END IF         
         REACTION_STR( NXX ) = REACTION_STR( NXX ) //  ' = '
! write out products
         DO IPRODUCT = 1, NPRDCT( NXX )
            ISPC = IRR( NXX, IPRODUCT + 3 )
            IF ( ABS( SC( NXX,IPRODUCT ) ) .NE. 1.0 ) THEN
               IF ( SC( NXX,IPRODUCT ) .LT. 0 ) THEN
                  WRITE(COEFF_STR,'(A,F8.5)')' - ',ABS(SC( NXX,IPRODUCT ))
                  REACTION_STR( NXX ) = REACTION_STR( NXX ) // TRIM(COEFF_STR) 
     &                        // ' * '  // TRIM(SPCLIS( ISPC ))
               ELSE
                  WRITE(COEFF_STR,'(F8.5)')SC( NXX,IPRODUCT )
                  IF( IPRODUCT .EQ. 1 )THEN
                     REACTION_STR( NXX ) = REACTION_STR( NXX ) // TRIM(COEFF_STR) 
     &                           // ' * '  // TRIM(SPCLIS( ISPC ))
                  ELSE
                     REACTION_STR( NXX ) = REACTION_STR( NXX ) // ' + ' // TRIM(COEFF_STR) 
     &                           // ' * '  // TRIM(SPCLIS( ISPC ))
                  END IF
               END IF
            ELSE IF ( SC( NXX,IPRODUCT ) .LT. 0.0 ) THEN
               REACTION_STR( NXX ) = REACTION_STR( NXX ) //  ' - ' // TRIM(SPCLIS( ISPC ))
            ELSE
               IF( IPRODUCT .EQ. 1 )THEN
                  REACTION_STR( NXX ) = REACTION_STR( NXX ) // TRIM(SPCLIS( ISPC ))
               ELSE
                  REACTION_STR( NXX ) = REACTION_STR( NXX ) //  ' + ' // TRIM(SPCLIS( ISPC ))
               END IF
            END IF
         END DO
       END DO
