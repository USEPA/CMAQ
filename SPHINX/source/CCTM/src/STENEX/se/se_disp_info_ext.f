
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
C $Header: /project/work/rep/STENEX/src/se_snl/se_disp_info_ext.f,v 1.1 2004/03/26 16:16:47 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Note: all these variables with prefix se_ are for stencil exchange library
C       only
C
C to define maximum displacement variables:
C
C   se_mndis -- maximun displacement in the north direction
C   se_medis -- maximun displacement in the east direction
C   se_msdis -- maximun displacement in the south direction
C   se_mwdis -- maximun displacement in the west direction
C --------------------------------------------------------------------------

        module se_disp_info_ext

          integer :: se_mndis, se_medis, se_msdis, se_mwdis

        end module se_disp_info_ext
