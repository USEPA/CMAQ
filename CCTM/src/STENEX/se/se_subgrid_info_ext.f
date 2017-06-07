
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
C $Header: /project/work/rep/STENEX/src/se_snl/se_subgrid_info_ext.f,v 1.1 2004/03/26 16:16:47 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Note: -- all these variables with prefix se_ are for stencil exchange library 
C          only
C
C to define sub-grid common variables:
C
C se_my_subgrid_beglev -- beginning level number of sub-grid
C se_my_subgrid_endlev -- ending level number of sub-grid
C se_subgrid_ind       -- same as se_gl_ind but on fine sub grid
C se_subgrid_send      -- holding processor number info where data is sending 
C                         to
C se_subgrid_recv      -- holding processor number info where data is receiving 
C                         from
C se_subgrid_send_ind  -- holding row (first two entries) and column (second 
C                         two entries) dimensions of data for sending
C se_subgrid_recv_ind  -- holding row (first two entries) and column (second 
C                         two entries) dimensions of data for receiving
C --------------------------------------------------------------------------

        module se_subgrid_info_ext

          integer :: se_my_subgrid_beglev
          integer :: se_my_subgrid_endlev

          integer, allocatable, save, target :: se_subgrid_ind (:, :, :)
          integer, pointer :: se_subgrid_ind_ptr (:, :, :)

          integer, allocatable, save, target :: se_subgrid_send_ind (:, :, :)
          integer, allocatable, save, target :: se_subgrid_recv_ind (:, :, :)
          integer, pointer :: se_subgrid_send_ind_ptr (:, :, :)
          integer, pointer :: se_subgrid_recv_ind_ptr (:, :, :)

          integer, allocatable, save, target :: se_subgrid_send(:)
          integer, allocatable, save, target :: se_subgrid_recv(:)
          integer, pointer :: se_subgrid_send_ptr(:)
          integer, pointer :: se_subgrid_recv_ptr(:)

        end module se_subgrid_info_ext
