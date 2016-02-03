C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/STENEX/src/se_snl/se_term_module.f,v 1.2 2006/02/15 14:45:31 yoj Exp $

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
C
C                    12/04/02 by David Wong
C                       -- made the routine more robust by checking 
C                          allocation first before any deallocate call
C --------------------------------------------------------------------------

        module se_term_module

          implicit none

          contains

	  subroutine se_term 

	  use se_pe_info_ext
	  use se_domain_info_ext
          use se_reconfig_grid_info_ext
          use se_subgrid_info_ext

  	  implicit none

          if (allocated(ranks)) then
             deallocate(ranks)
          end if

          if (allocated(se_gl_ind)) then
             deallocate (se_gl_ind)
          end if

          if (allocated(se_reconfig_grid_send_ind)) then
             deallocate (se_reconfig_grid_send_ind)
             deallocate (se_reconfig_grid_recv_ind)
          end if

          if (allocated(se_subgrid_send_ind)) then
             deallocate (se_subgrid_send_ind)
             deallocate (se_subgrid_recv_ind)
             deallocate (se_subgrid_send)
             deallocate (se_subgrid_recv)
             deallocate (se_subgrid_ind)
          end if

          return
          end subroutine se_term 

        end module se_term_module
