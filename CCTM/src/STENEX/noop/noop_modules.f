
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
C $Header: /project/work/rep/STENEX/src/noop_f90/noop_modules.f,v 1.3 2002/02/28 15:20:36 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Purpose:
C
C   to provide an interface between the noop stencil exchange library and the
C application code
C
C Revision history:
C
C   Orginal version: 11/05/99 by David Wong
C                    02/27/01 by David Wong
C                      -- to include a new statement: use noop_term_module
C                    11/27/01 by David Wong
C                      -- include a new module: noop_bndy_copy_module
C --------------------------------------------------------------------------

        module noop_modules

          use noop_init_module
          use noop_term_module

          use noop_util_module

          use noop_comm_module
          use noop_slice_module
          use noop_data_copy_module
          use noop_gather_module

          use noop_global_max_module
          use noop_global_min_module
          use noop_global_sum_module

        end module noop_modules
