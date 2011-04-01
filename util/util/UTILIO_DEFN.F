
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/util/util/UTILIO_DEFN.F,v 1.1 2011/04/01 15:29:48 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C....................................................................
C  CONTAINS:  This module provides the interface to the Models3 IOAPI
C     using the M3UTILIO module.  For parallel implementation of CMAQ, 
C     several IOAPI routines are replaced with PARIO versions.  For
C     example, the IOAPI function WRITE3 is renamed to an unused 
C     function name "EXCLUDE_WRITE3", and the PARIO function PWRITE3
C     is aliased to WRITE3.  So when WRITE3 is used in CMAQ, then the
C     the PARIO function PWRITE3 is accessed.  This approach allows
C     flexibility in naming within CMAQ and eliminated the usage of
C     "cpp" flags to achieve the function overloading.
C              
C  REVISION HISTORY:
C       Original version 03/2011 by Shawn Roselle
C....................................................................

      MODULE UTILIO_DEFN

#ifdef parallel

         USE M3UTILIO, EXCLUDE_INTERPB => INTERP3,
     &                 EXCLUDE_M3EXIT  => M3EXIT,
     &                 EXCLUDE_M3WARN  => M3WARN,
     &                 EXCLUDE_SHUT3   => SHUT3,
     &                 EXCLUDE_WRITE3  => WRITE3

         USE PARUTILIO, INTERPB => PINTERPB,
     &                  M3ERR   => PM3ERR,
     &                  M3EXIT  => PM3EXIT,
     &                  M3WARN  => PM3WARN,
     &                  SHUT3   => PSHUT3,
     &                  WRITE3  => PWRITE3
#else
         USE M3UTILIO, INTERPB => INTERP3
#endif

      END MODULE UTILIO_DEFN
