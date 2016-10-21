
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

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/PARIO/src/alloc_data_mod.f,v 1.2 2006/06/05 17:36:42 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C....................................................................
C  CONTAINS:  Allocated variables used by pwrgrdd
              
C  REVISION HISTORY:
C       Original version 01/10/05 by David Wong
C....................................................................

      MODULE ALLOC_DATA_MODULE

      REAL, ALLOCATABLE, SAVE :: WRITBUF( :,:,: )
      REAL, ALLOCATABLE, SAVE :: RECVBUF( : )

      END MODULE ALLOC_DATA_MODULE
