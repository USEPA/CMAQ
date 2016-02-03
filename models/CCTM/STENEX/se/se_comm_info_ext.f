
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
C $Header: /project/work/rep/STENEX/src/se_snl/se_comm_info_ext.f,v 1.1 2004/03/26 16:16:47 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Note: all these variables with prefix se_ are for stencil exchange library
C       only
C
C to define communication info variables:
C   
C   se_ngb_pe    -- an array to indicate a communication with a certain 
C                   processor is required base upon near-neighbour 
C                   communication pattern: -1 denotes no communication is 
C                   needed, and a non -1 number denotes processor number with 
C                   which communication is formed
C   se_numdim    -- dimensionality of a data structure which requires 
C                   communication
C   se_decompstr -- indicator of which dimenion(s) of data is/are decomposed, 
C                   0 (not decomposed), 1 (decomposed)
C --------------------------------------------------------------------------

        module se_comm_info_ext

          integer :: se_ngb_pe(8)
          integer :: se_numdim
          character (len=10) :: se_decompstr

          integer :: se_twoway_npcol, se_twoway_nprow

        end module se_comm_info_ext
