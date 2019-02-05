
      module log_header

      contains

      subroutine LOAD_HEADER( HEADER_TXT )

      IMPLICIT NONE

      CHARACTER( 90 ) :: HEADER_TXT( : )

      HEADER_TXT( : ) = ''

      HEADER_TXT( 1:23 ) = (/   
     &   '================================================================================', 
     &   '|                                                                              |', 
     &   '|               The Community Multiscale Air Quality (CMAQ) Model              |', 
     &   '|                                   Version 5.3 Beta                           |', 
     &   '|                            Interim Freeze August 31, 2018                    |', 
     &   '|                                                                              |', 
     &   '|                          Built and Maintained by the                         |', 
     &   '|                      National Exposure Research Laboratory                   |', 
     &   '|                        Office of Research and Development                    |', 
     &   '|                   United States Environmental Protection Agency              |', 
     &   '|                                                                              |', 
     &   '|                            https://www.epa.gov/cmaq                          |', 
     &   '|                                                                              |', 
     &   '|       Source Code:   https://www.github.com/USEPA/cmaq/tree/5.2.1            |', 
     &   '|       Documentation: https://www.github.com/USEPA/cmaq/tree/5.2.1/DOCS       |', 
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
