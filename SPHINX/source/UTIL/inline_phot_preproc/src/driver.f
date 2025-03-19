      PROGRAM DRIVER
      
      USE GET_ENV_VARS
     
      USE CSQY_PARAMETERS
      USE CSQY_REFER_DATA

      IMPLICIT NONE

      CHARACTER(  5 )    :: SPLIT_OUTPUT
      CHARACTER( 13 )    :: TYPE_OUTPUTS  = 'SPLIT_OUTPUT'

      INTEGER  :: STATUS
      INTEGER  :: SYSTEM

      INTERFACE
       SUBROUTINE CONVERT_CASE ( BUFFER, UPPER )
           CHARACTER(LEN= *), INTENT( INOUT ) :: BUFFER
           LOGICAL,           INTENT( IN    ) :: UPPER
       END SUBROUTINE CONVERT_CASE
      END INTERFACE

!Bin 01 177.5-202.5 nm
!Bin 02 177.5-202.5 nm 
!Bin 03 177.5-202.5 nm 
!Bin 04 177.5-202.5 nm 
!Bin 05 202.5-206.5 nm
!Bin 06 206.5-209.5 nm
!Bin 07 209.5-212.5 nm
!Bin 08 212.5-215.5 nm
!Bin 09 233.0-275.5 nm
!Bin 10 275.5-286.5 nm
!Bin 11 286.5-291.0 nm
!Bin 12 291.0-298.3 nm
!Bin 13 298.3-307.5 nm
!Bin 14 307.5-312.5 nm
!Bin 15 312.5-320.3 nm
!Bin 16 320.3-345.0 nm
!Bin 17 345.0-412.5 nm
!Bin 18 412.5-850.0 nm

         CALL VALUE_NAME ( TYPE_OUTPUTS,  SPLIT_OUTPUT)

         CALL CONVERT_CASE( SPLIT_OUTPUT, .TRUE.)

         IF( SPLIT_OUTPUT(1:1) .EQ. 'T' .OR. SPLIT_OUTPUT(1:1) .EQ. 'Y' )THEN
             SPLIT_OUTPUTS = .TRUE.
             WRITE(6,'(A)')'Environment Variable  SPLIT_OUTPUT set to '
     &       // TRIM( SPLIT_OUTPUT ) // '. CSQY and Optics Data in two'
     &      //  ' separate files'
         ELSE IF(  SPLIT_OUTPUT(1:1) .EQ. 'F' .OR. SPLIT_OUTPUT(1:1) .EQ. 'N' )THEN
             SPLIT_OUTPUTS = .FALSE.
             WRITE(6,'(A)')'Environment Variable  SPLIT_OUTPUT set to '
     &      // TRIM( SPLIT_OUTPUT ) // '. CSQY and Optics Data in one file'
         ELSE
             WRITE(6,' (A)')'Environment Variable  SPLIT_OUTPUT set to '
     &       // TRIM( SPLIT_OUTPUT ) // ' and must equal T, Y, F, or N.'
     &       // ' Using default value of T'
             SPLIT_OUTPUTS = .TRUE.
         END IF

          
         CALL PROCESS_CSQYS ( )

         IF(SPLIT_OUTPUTS)CALL WRT_OPTICS_DATA( )
        
         CLOSE( JTABLE_UNIT )
        
        WRITE(6, *)'NORMAL_STOP'
        STATUS = 0
        CALL EXIT(STATUS)
      STOP
      END 

