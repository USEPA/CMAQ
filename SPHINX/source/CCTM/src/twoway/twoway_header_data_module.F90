!===============================================================================
! Name:     HEADER_DATA_MODULE
! Purpose:  Define Met and Grid file constant header information
! Revised:  10 Apr 2007  Original version.  (David Wong)
!===============================================================================

module twoway_header_data_module

  integer, parameter :: max_nlays = 100

  type header_constant_data_record
    integer :: sdate
    integer :: stime
    integer :: nthik
    integer :: ncols
    integer :: nrows
    integer :: nlays
    integer :: gdtyp
    integer :: vtype
    real    :: p_alp
    real    :: p_bet
    real    :: p_gam
    real    :: xcent
    real    :: ycent
    real    :: xorig
    real    :: yorig
    real    :: xcell
    real    :: ycell
    integer :: vgtyp
    real    :: vgtop
    real    :: vglvs (max_nlays + 1)
    character (len = 16) :: gdnam
    character (len = 16) :: grid_name
  end type header_constant_data_record

  TYPE(header_constant_data_record) :: ioapi_header

end module twoway_header_data_module
