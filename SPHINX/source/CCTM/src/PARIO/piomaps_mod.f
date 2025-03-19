
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
C $Header: /project/work/rep/PARIO/src/piomaps_mod.f,v 1.2 2006/06/05 17:36:43 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C....................................................................
 
C  CONTAINS:  Processor-to-subdomain maps for parallel Models-3
C             I/O routines.
 
C  DEPENDENT UPON:  none 
 
C  REVISION HISTORY:
C       Original version 02/04 by David Wong
C       Modified 12/05/2015 by David Wong
C          -- Added two new varaible PIO_GL_NCOLS and PIO_GL_NROWS
C       Modified 12/09/2015 by David Wong
C          -- Introduced a new variable PARIO_IO_PE_INCLUSIVE to determine
C             which procesor involves in file closing process
 
C  NOTES:  The mapping assumes 2-dimensional subdomain decomposition,
C          over grid rows and columns.
C....................................................................

C  ----------------------------------------------------
C  The arrays below are allocated by function PIO_INIT.
C  ----------------------------------------------------

C    INTEGER  NUMPROCS              Number of processors.

C    INTEGER  NCOLS_PE( NUMPROCS )      Number grid columns in the processor
C    INTEGER  NROWS_PE( NUMPROCS )      Number grid rows in the processor
C    INTEGER  COLSX_PE( 2,NUMPROCS )    Processor column range:
C                                       COLSX_PE(1,*) = start column index
C                                       COLSX_PE(2,*) = end column index
C    INTEGER  ROWSX_PE( 2,NUMPROCS )    Processor row range:
C                                       ROWSX_PE(1,*) = start row index
C                                       ROWSX_PE(2,*) = end row index
C    INTEGER  WR_NCOLS_PE( NUMPROCS )   Number of columns in the processor
C                                       subgrid to write
C    INTEGER  WR_NROWS_PE( NUMPROCS )   Number of rows in the processor
C                                       subgrid to write
C                                       ROWSX_PE(2,*) = end row index
C    INTEGER  WR_COLSX_PE( 2,NUMPROCS ) Column range of the processor subgrid
C                                       to write
C                                       COLSX_PE(1,*) = start column index
C                                       COLSX_PE(2,*) = end column index
C    INTEGER  WR_ROWSX_PE( 2,NUMPROCS ) Row range of the processor subgrid
C                                       to write
C                                       ROWSX_PE(1,*) = start row index

C....................................................................

      MODULE PIOMAPS_MODULE

      INTEGER  NUMPROCS
  
      INTEGER, ALLOCATABLE :: NCOLS_PE( : )
      INTEGER, ALLOCATABLE :: COLSX_PE( :,: )

      INTEGER, ALLOCATABLE :: NROWS_PE( : )
      INTEGER, ALLOCATABLE :: ROWSX_PE( :,: )

      INTEGER, ALLOCATABLE :: WR_NCOLS_PE( : )
      INTEGER, ALLOCATABLE :: WR_COLSX_PE( :,: )

      INTEGER, ALLOCATABLE :: WR_NROWS_PE( : )
      INTEGER, ALLOCATABLE :: WR_ROWSX_PE( :,: )

      integer :: pio_gl_ncols
      integer :: pio_gl_nrows

      LOGICAL :: PARIO_IO_PE_INCLUSIVE

      END MODULE PIOMAPS_MODULE
