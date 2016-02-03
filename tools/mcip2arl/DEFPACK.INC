
! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /nas01/depts/ie/cempd/apps/CMAQ/v5.0.1/CMAQv5.0.1/models/TOOLS/src/mcip2arl/DEFPACK.INC,v 1.1.1.1 2012/04/19 19:48:37 sjr Exp $

!-------------------------------------------------------------------------------
! DEFPACK.INC - Structure for packing routines
!-------------------------------------------------------------------------------
! Last Revision: 13 Jul 1999 (RRD) 
!                20 Sep 2000 (RRD) - fortran90 upgrade
!                18 Oct 2001 (RRD) - support for large grids
!                03 Oct 2002 (RRD) - more vertical levels

!-------------------------------------------------------------------------------
! packed data set array limits set here           

  INTEGER,  PARAMETER :: mlvl  =   75      ! number of vertical levels
  INTEGER,  PARAMETER :: mvar  =   35      ! number of different variables 
  INTEGER,  PARAMETER :: mlen  = 3072      ! maximum length of header record
  INTEGER,  PARAMETER :: mgrd  =    3      ! number of simultaneous grids

  TYPE MFILES
     SEQUENCE

     REAL             :: height    (mlvl)      ! height of each level
     REAL             :: grids       (12)      ! grid definition array
     INTEGER          :: nvar      (mlvl)      ! variables each level
     INTEGER          :: nrec      (mlvl)      ! record offset from index
     INTEGER          :: chks (mvar,mlvl)      ! rotating checksum
     CHARACTER(4)     :: varb (mvar,mlvl)      ! variable identification
     CHARACTER(4)     :: model                 ! model identification
     LOGICAL          :: newt                  ! new time flag
     LOGICAL          :: xgpt                  ! xtended number of grid points
     INTEGER          :: ig                    ! unique grid number
     CHARACTER(2)     :: igc,igd               ! character equivalent of ig
     INTEGER          :: ksys                  ! vertical coordinate system
     INTEGER          :: nxg,nyg               ! horizontal grid points
     INTEGER          :: nlvl                  ! number of vertical levels
     INTEGER          :: mrec                  ! record number of index record
     INTEGER          :: nrpt                  ! records per time period
     INTEGER          :: iy0,im0,id0           ! initial date for record
     INTEGER          :: ih0,mn0               ! initial hour minute
     INTEGER          :: ic0,icx               ! fcst hour, extended fcst hour
     INTEGER          :: kunit                 ! output unit number
     INTEGER          :: lrec                  ! bytes in data part of record
     INTEGER          :: lenh                  ! bytes in extended header
                                               ! (108 + NLVL * ( 8 + NVAR*8 ))
     INTEGER          :: nhrec                 ! number of extended header recs
     INTEGER          :: nhbyt                 ! bytes last extend header rec

  END TYPE

!-------------------------------------------------------------------------------
! define meteorological grid and file structure

  TYPE(mfiles)  :: gv(mgrd)    ! define for each meteorological grid
