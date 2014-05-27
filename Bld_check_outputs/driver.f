      PROGRAM DRIVER
      
      USE CHECK_CSQY_DATA

      IMPLICIT NONE

        CALL LOAD_CSQY_DATA()

        CALL LOAD_OPTICS_DATA()
        
        WRITE(6, *)'NORMAL STOP'
      STOP
      END 

