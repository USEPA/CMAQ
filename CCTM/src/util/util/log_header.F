
      module log_header

      contains

      subroutine LOAD_HEADER( HEADER_TXT )

      IMPLICIT NONE

      CHARACTER( 90 ) :: HEADER_TXT( : )

      HEADER_TXT( : ) = ''

      HEADER_TXT( 1:21 ) = (/   
     &   '================================================================================', 
     &   '|                                                                              |', 
     &   '|               The Community Multiscale Air Quality (CMAQ) Model              |', 
     &   '|                                   Version 5.3.3                              |', 
     &   '|                                                                              |', 
     &   '|                          Built and Maintained by the                         |', 
     &   '|                        Office of Research and Development                    |', 
     &   '|                   United States Environmental Protection Agency              |', 
     &   '|                                                                              |', 
     &   '|                            https://www.epa.gov/cmaq                          |', 
     &   '|                                                                              |', 
     &   '|       Source Code:   https://www.github.com/USEPA/cmaq/tree/main             |', 
     &   '|       Documentation: https://www.github.com/USEPA/cmaq/tree/main/DOCS        |', 
     &   '|                                                                              |', 
     &   '|         The CMAQ Model is tested and released with cooperation from          |', 
     &   '|         the Community Modeling and Analysis System (CMAS) Center via         |', 
     &   '|         contract support. CMAS is managed by the Institute for the           |', 
     &   '|         Environment, University of North Carolina at Chapel Hill.            |', 
     &   '|         CMAS URL: (https://www.cmascenter.org)                               |', 
     &   '|                                                                              |', 
     &   '================================================================================'  
     &      /)

       end subroutine LOAD_HEADER 

      end module log_header
