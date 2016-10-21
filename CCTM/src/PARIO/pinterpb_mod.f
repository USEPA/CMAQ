
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
C $Header: /project/work/rep/PARIO/src/pinterpb_mod.f,v 1.3 2006/06/05 17:36:43 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C....................................................................
C  CONTAINS:  Variables used by parallel file reading routines
C             PINTERPB, which are based on a domain 
C             decomposition over the horizontal grid coordinates. 
C             These I/O routines are part of the parallel Models-3 
C             I/O library.
              
C  REVISION HISTORY:
C       Original version 02/04 by David Wong
C       26Sep2013 J.Young increase MXNVARHD from 200 to 2048 - needed for DDM3D
C....................................................................

      MODULE PINTERPB_MODULE

C         Parameters         Description
C         ----------         -----------

      INTEGER, PARAMETER :: MXNVARHD = 2048   ! Max number of buffered file variables.

C  -----------------------------------------------------------------
C  The following (dynamic) variables are used to determine when
C  file I/O buffers need to be updated.  All are allocated to size
C  MXNVARHD. Times and dates are initialized to -9999 and buffer 
C  switches are initialized to 0 in function PINTERPB.
C
C  NOTE: These are the parallel versions of the I/O API state 
C        variables LDATE3 and NDATE3.  (see I/O API file STATE3.EXT )
C  -----------------------------------------------------------------

C          Variables                         Description
C          ---------                         -----------

      INTEGER, SAVE :: LDATHD( MXNVARHD ) ! Start date for current buffered 2D data

      INTEGER, SAVE :: LTIMHD( MXNVARHD ) ! Start time for current buffered 2D data

      INTEGER, SAVE :: NDATHD( MXNVARHD ) ! End date for current buffered 2D data

      INTEGER, SAVE :: NTIMHD( MXNVARHD ) ! End time for current buffered 2D data

      INTEGER, SAVE :: SWBUFHD( MXNVARHD )! Buffer switches (0 means normal, 1 means reversed)

C  -----------------------------------------------------------------
C  The following (dynamic) array holds offsets for positioning into
C  the file buffers.
C  -----------------------------------------------------------------

      INTEGER, SAVE :: BUFPOSHD( MXNVARHD ) ! Buffer positions for file variable buffers

C  -----------------------------------------------------------------
C  The following (dynamic) array holds dimensions and boundary 
C  thicknesses for file variables.
C  -----------------------------------------------------------------

      INTEGER, SAVE :: SIZEHD( MXNVARHD )    ! Local processor variable size

C  -----------------------------------------------------------------
C  The following (static) array is used to identify file variables 
C  uniquely.
C  -----------------------------------------------------------------

      CHARACTER( 33 ), SAVE :: VLISTHD( MXNVARHD )     ! Variable-name table

C  -----------------------------------------------------------------
C  The following (dynamic) array, BUFFERHD is used for holding start
C  and end buffers for file variables which need to be interpolated.
C  It is allocated by extension, as each new variable is called for 
C  interpolation.
C  -----------------------------------------------------------------

      TYPE DATA_PTR_TYPE 
         REAL, POINTER :: DATA_PTR( : )
      END TYPE DATA_PTR_TYPE 

      TYPE MEM_TYPE 
         INTEGER :: SIZE
         TYPE( DATA_PTR_TYPE ) :: MEM( 0:1 )
      END TYPE MEM_TYPE 

      TYPE( MEM_TYPE ), SAVE :: BUFFERHD( MXNVARHD )

      INTEGER, SAVE :: PTR_COUNT = 0
      INTEGER, SAVE :: BUFFERHD_SIZE = 0

C  ------------------------------------------------------------
C  The dynamic array below is used for reading and communicating
C  file variables. It is allocated in PINTERPB.
C  ------------------------------------------------------------

      REAL, ALLOCATABLE, SAVE :: MSGBUFHD( : )         ! Message buffer

      END MODULE PINTERPB_MODULE
