
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
C $Header: /project/work/rep/STENEX/src/se_snl/se_modules.f,v 1.1 2004/03/26 16:16:47 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Purpose:
C
C   to provide an interface between the stencil exchange library and the
C application code
C
C Revision history:
C
C   Orginal version: 11/05/99 by David Wong
C                    02/27/01 by David Wong
C                      -- include two use statements: use se_term_module and
C                         use se_reconfig_grid_module
C                    11/27/01 by David Wong
C                      -- include a new module: se_bndy_copy_module
C                    08/24/11 by David Wong
C                      -- include a new module: se_twoway_comm_module for
C                         the WRF-CMAQ twoway model
C --------------------------------------------------------------------------

        module se_modules

          use se_init_module
          use se_term_module

          use se_util_module

          use se_comm_module
          use se_slice_module
          use se_data_copy_module
          use se_gather_module

          use se_reconfig_grid_module

          use se_bndy_copy_module

          use se_global_max_module
          use se_global_min_module
          use se_global_sum_module

          use se_global_gather_module
          use se_global_bcast_module

          use se_twoway_comm_module

        end module se_modules
