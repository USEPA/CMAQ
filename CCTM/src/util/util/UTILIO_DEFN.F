
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

C....................................................................
C  CONTAINS:  This module provides the interface to the Models3 IOAPI
C     using the M3UTILIO module.  For parallel implementation of CMAQ, 
C     several IOAPI routines are replaced with PARIO versions.  For
C     example, the IOAPI function WRITE3 is renamed to an unused 
C     function name "EXCLUDE_WRITE3", and the PARIO function PWRITE3
C     is aliased to WRITE3.  So when WRITE3 is used in CMAQ, then the
C     PARIO function PWRITE3 is accessed.  This approach allows
C     flexibility in naming within CMAQ and eliminated the usage of
C     "cpp" flags to achieve the function overloading.
C              
C  REVISION HISTORY:
C       Original version 03/2011 by Shawn Roselle
C                        08/2011 by David Wong
C                          -- extended to handle buffer file
C                        08/2015 by David Wong
C                          -- defined WRITE3 under parallel I/O
C                             implementation
C                        02/2019 by David Wong
C                          -- removed INTERPB declaration which is
C                             not needed with centralized I/O
C                             implementation
C....................................................................

      MODULE UTILIO_DEFN

#ifdef mpas
       use util_module
       use mio_module
       
       use prog_interrupt_module, M3WARN => prog_interrupt_warning,
     &                             M3EXIT => prog_interrupt_exit,
     &                             M3MESG => prog_interrupt_msg,
     &                             M3MSG2 => prog_interrupt_msg,
     &                             m3parag => prog_interrupt_nmsgs


       INTEGER, PARAMETER :: XSTAT1 = 1
       INTEGER, PARAMETER :: XSTAT2 = 1
       INTEGER, PARAMETER :: XSTAT3 = 1

#else

#ifdef parallel

         USE M3UTILIO, EXCLUDE_INTERPB => INTERP3,
     &                 EXCLUDE_M3EXIT  => M3EXIT,
     &                 EXCLUDE_M3WARN  => M3WARN,
     &                 EXCLUDE_SHUT3   => SHUT3,
     &                 EXCLUDE_WRITE3  => WRITE3,
     &                 BUF_WRITE3      => WRITE3

         USE PARUTILIO, 
     &                  M3ERR   => PM3ERR,
     &                  M3EXIT  => PM3EXIT,
     &                  M3WARN  => PM3WARN,
#ifdef parallel_io
     &                  SHUT3   => PSHUT3

         LOGICAL, EXTERNAL :: WRITE3
#else
     &                  SHUT3   => PSHUT3,
     &                  WRITE3  => PWRITE3
#endif
#else
         USE M3UTILIO, INTERPB => INTERP3
#endif

#endif

      END MODULE UTILIO_DEFN
