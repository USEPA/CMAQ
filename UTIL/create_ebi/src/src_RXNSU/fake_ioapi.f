c...fake_ioapi.f

     
      subroutine m3exit ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      
      implicit none
      character*16 pname
      character*80 xmsg
      integer jdate, jtime, xstat1
      
      print *, ' *******program ',pname,' terminated normally********'
      print *, ' at date/time ', jdate, jtime
      print *, xmsg
      return
      end
      
      subroutine m3warn ( PNAME, JDATE, JTIME, XMSG )
      
      implicit none
      character*16 pname
      character*80 xmsg
      integer jdate, jtime
      
      print *, ' !!!warning in program ',pname,' !!!!!!!!!!'
      print *, ' at date/time ', jdate, jtime
      print *, xmsg
      return
      end
      

      subroutine pm3warn ( PNAME, JDATE, JTIME, XMSG )
      
      implicit none
      character*16 pname
      character*80 xmsg
      integer jdate, jtime
      
      print *, ' !!!warning in program ',pname,' !!!!!!!!!!'
      print *, ' at date/time ', jdate, jtime
      print *, xmsg
      return
      end


      subroutine m3mesg ( MSG )
      
      implicit none
      character*80 msg
      
      print *, msg
      return
      end


      integer function init3 ( )
      
      implicit none
     
      init3 = 6
      return
      end

C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/trimlen.f,v 1.2 2000/11/28 21:23:07 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        INTEGER  FUNCTION  TRIMLEN ( STRING )

C***********************************************************************
C  function body starts at line 43
C
C  FUNCTION:  return the effective length of argument CHARACTER*(*) STRING,
C             after trailing blanks have been trimmed.
C
C  PRECONDITIONS REQUIRED:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:  
C             Prototype 8/91 by CJC
C             Version 2/93 for CRAY by CJC
C
C***********************************************************************

      IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*) STRING


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER       L, K


C***********************************************************************
C   begin body of function  TRIMLEN

        L = LEN( STRING )
        DO  11  K = L, 1, -1
            IF ( STRING( K:K ) .NE. ' ' ) THEN
                GO TO  12
            END IF
11      CONTINUE

        K = 1

12      CONTINUE

        TRIMLEN = K

        RETURN

        END


C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/index1.f,v 1.2 2000/11/28 21:22:49 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

      INTEGER FUNCTION INDEX1 (NAME, N, NLIST)

C***********************************************************************
C  subroutine body starts at line 46
C
C  FUNCTION:
C
C    Searches for NAME in list NLIST and returns the subscript
C    (1...N) at which it is found, or returns 0 when NAME not
C    found in NLIST
C
C  PRECONDITIONS REQUIRED:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION HISTORY:
C
C    5/88   Modified for ROMNET
C    9/94   Modified for Models-3 by CJC
C
C***********************************************************************

      IMPLICIT NONE
 
C.......   Arguments and their descriptions:

      CHARACTER*(*) NAME        !  Character string being searched for
      INTEGER       N           !  Length of array to be searched
      CHARACTER*(*) NLIST(*)    !  array to be searched

C.......   Local variable:

      INTEGER       I   !  loop counter

C.....................................................................
C.......   begin body of INDEX1()

      DO 100 I = 1, N

          IF ( NAME .EQ. NLIST( I ) ) THEN    ! Found NAME in NLIST
              INDEX1 = I
              RETURN
          END IF

100   CONTINUE

      INDEX1 = 0        !  not found
      RETURN

      END

