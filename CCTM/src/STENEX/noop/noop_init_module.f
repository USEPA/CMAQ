
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
C $Header: /project/work/rep/STENEX/src/noop_f90/noop_init_module.f,v 1.1.1.1 2000/04/12 17:40:55 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Purpose:
C
C   use F90 module feature to capture noop_init routine
C
C Revision history:
C
C   Orginal version: 11/05/99 by David Wong
C --------------------------------------------------------------------------

        module noop_init_module

          implicit none

          contains

C -----------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op noop_init.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 10/6/98 by David Wong
C                    11/05/99 by David Wong
C                      -- recode using F90 syntax
C -----------------------------------------------------------------------------

        subroutine noop_init (numprocs, nprow, npcol, gl_nrows, 
     $                        gl_ncols, gl_nlays, gl_nspcs, my_pe,
     $                        mndis, medis, msdis, mwdis, data_ori, geo_ori)

        implicit none

        integer, intent(in) :: numprocs, nprow, npcol
        integer, intent(in) :: gl_nrows, gl_ncols, gl_nlays, gl_nspcs
        integer, intent(in) :: my_pe
        integer, intent(in) :: mndis, medis, msdis, mwdis
        character (len = 2), optional, intent(in) :: data_ori
        integer, optional, intent(in) :: geo_ori

        end subroutine noop_init 

        end module noop_init_module
