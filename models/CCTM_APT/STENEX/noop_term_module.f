
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/STENEX/src/noop_f90/noop_term_module.f,v 1.1 2002/02/28 16:27:28 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Purpose:
C
C   to terminate stenex library
C
C Revision history:
C
C   Orginal version: 11/30/00 by David Wong
C --------------------------------------------------------------------------

        module noop_term_module

          implicit none

          contains

	  subroutine noop_term 

  	  implicit none

          return
          end subroutine noop_term 

        end module noop_term_module
