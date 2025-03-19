
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
C $Header: /project/work/rep/STENEX/src/se_snl/se_domain_info_ext.f,v 1.1 2004/03/26 16:16:47 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Note: all these variables with prefix se_ are for stencil exchange library
C       only
C
C to define domain decomposition info variables:
C
C -- global values
C
C   se_gl_ncols  -- number of columns in the physical domain
C   se_gl_nrows  -- number of rows in the physical domain
C   se_gl_nlays  -- number of layers in the physical domain
C   se_gl_nspcs  -- number of species in the physical domain
C   se_gl_ind    -- global fine domain to processor map: the first two entries
C                   are the beginning and ending row number, the second two 
C                   entries are the beginning and ending column number, second 
C                   dimension corresponding to the processor number
C
C -- local dimension values
C
C   se_ncols -- max. number of columns in a processor for data declaration
C   se_nrows -- max. number of rows in a processor for data declaration
C   se_nlays -- max. number of layers in a processor for data declaration
C   se_nspcs -- max. number of species in a processor for data declaration
C
C -- local processor data dimension values
C
C   se_my_ncols -- number of columns a processor is responsible for
C   se_my_nrows -- number of rows a processor is responsible for
C   se_my_nlays -- number of layers a processor is responsible for
C   se_my_nspcs -- number of species a processor is responsible for
C
C -- local processor data dimension values plus one
C
C   se_my_ncolsp1 -- number of columns a processor is responsible for plus one
C   se_my_nrowsp1 -- number of rows a processor is responsible for plus one
C   se_my_nlaysp1 -- number of layers a processor is responsible for plus one
C   se_my_nspcsp1 -- number of species a processor is responsible for plus one
C
C eg. given eight processors, four processors are assigned to column
C     dimension and 2 are assigned to row dimension (note: vertical axis
C     denotes the column dimension and horizontal axis denotes row dimension),
C     and a 2-D domain of size 17 by 11
C
C     ie. numprocs = 8               PE map:    0   1
C         npcol    = 4                          2   3
C         nprow    = 2                          4   5
C         gl_ncols = 17                         6   7
C         gl_nrows = 11
C
C     in each PE, ncols by nrows = 5 x 6
C     se_my_ncols by se_my_nrows = 5 x 6 for PE 0
C     se_my_ncols by se_my_nrows = 5 x 5 for PE 1
C     se_my_ncols by se_my_nrows = 4 x 6 for PE 2, 4, and 6
C     se_my_ncols by se_my_nrows = 4 x 5 for PE 3, 5, and 7
C --------------------------------------------------------------------------

        module se_domain_info_ext

          integer :: se_gl_ncols
          integer :: se_gl_nrows
          integer :: se_gl_nlays
          integer :: se_gl_nspcs

          integer, allocatable, save, target :: se_gl_ind(:, :, :)
          integer, pointer :: se_gl_ind_ptr (:, :, :)

          integer :: se_ncols
          integer :: se_nrows
          integer :: se_nlays
          integer :: se_nspcs

          integer :: se_my_ncols
          integer :: se_my_nrows
          integer :: se_my_nlays
          integer :: se_my_nspcs
          integer :: se_my_ncolsp1
          integer :: se_my_nrowsp1
          integer :: se_my_nlaysp1
          integer :: se_my_nspcsp1
 
        end module se_domain_info_ext
