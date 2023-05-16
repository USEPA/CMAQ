SUBROUTINE feedback_setup ( jdate, jtime, tstep )

!===============================================================================
! Purpose:  Setup feedback buffer file
!
! Revised:  April 2007  Original version.  David Wong
!           Oct.  2015  -- put in error checking and updated water insoluble
!                          calculation
!                       -- commented out indirect code
!                       -- used loop structure rather than explicit list
!           Jan.  2016  -- resized the first dimension of cmaq_wrf_c_send_to,
!                          cmaq_wrf_c_recv_from, cmaq_wrf_c_send_index_g,
!                          cmaq_wrf_c_send_index_l, cmaq_wrf_c_recv_index_g,
!                          and cmaq_wrf_c_recv_index_l
!           22 Nov 2016  Constructed water soluble and insoluble list dynamically
!                        based on a given chemical mechanism and AE scheme
!           17 Jan 2017  Replace 3 with n_mode for robustness
!           31 Jan 2019  (David Wong)
!              -- adopted the idea to process all twoway related environment
!                 variables in one place
!           01 Aug 2019  (David Wong)
!              -- updated abort message
!           26 Jul 2022  (David Wong)
!              -- Added a prefix tw_ for these variables: sc, ec, sr, er sc_d, ec_d, 
!                 sr_d, and er_d to avoid naming conflicts
!===============================================================================

  USE twoway_header_data_module
  USE twoway_met_param_module
  USE twoway_data_module
  USE twoway_util_module
  USE twoway_cgrid_aerosol_spc_map_module
  USE aero_data

  use cgrid_spcs

  use utilio_defn

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: jdate, jtime, tstep

  CHARACTER (LEN = 16), PARAMETER :: pname = 'feedback_setup  '

  CHARACTER (LEN = 16) :: feedback_fname

    integer :: i, j, k, n, stat, slen
    logical :: found

    character (len = 4), save :: pe_str

       allocate (cmaq_wrf_c_send_to(0:9, 0:twoway_nprocs-1),              &
                 cmaq_wrf_c_recv_from(0:9, 0:twoway_nprocs-1),            &
                 cmaq_wrf_c_send_index_g(9*3, 2, 0:twoway_nprocs-1),      &   ! starting and ending dimension, dimenionality
                 cmaq_wrf_c_send_index_l(9*3, 2, 0:twoway_nprocs-1),      &   ! starting and ending dimension, dimenionality
                 cmaq_wrf_c_recv_index_g(9*3, 2, 0:twoway_nprocs-1),      &   ! starting and ending dimension, dimenionality
                 cmaq_wrf_c_recv_index_l(9*3, 2, 0:twoway_nprocs-1),      &   ! starting and ending dimension, dimenionality
                 stat=stat) 
       if (stat .ne. 0) then
          print *, ' Error: Allocating communication indices arrays'
          stop
       end if

       cmaq_wrf_c_send_to = wrf_cmaq_c_recv_from
       cmaq_wrf_c_recv_from = wrf_cmaq_c_send_to
       cmaq_wrf_c_send_index_l = wrf_cmaq_c_recv_index_l
       cmaq_wrf_c_recv_index_l = wrf_cmaq_c_send_index_l

       write (pe_str, 11) '_', twoway_mype
 11    format (a1, i3.3)

       feedback_fname = 'feed_back' // pe_str

       call aq_set_ioapi_header ('C', ioapi_header%ncols, ioapi_header%nrows)

       xorig3d = ioapi_header%xorig - ioapi_header%xcell
       yorig3d = ioapi_header%yorig - ioapi_header%ycell
       nlays3d = ioapi_header%nlays
       nvars3d = n_feedback_var
       vname3d(1:nvars3d) = feedback_vlist
       units3d(1:nvars3d) = ''
       tstep3d = tstep
       vtype3d(1:nvars3d) = ioapi_header%vtype

       sdate3d = jdate
       stime3d = jtime

       if ( .not. open3 (feedback_fname, FSRDWR3, pname) ) then
          print *, ' Error: Could not open file ', trim(feedback_fname), 'for update'
          if ( .not. open3 (feedback_fname, FSNEW3, pname) ) then
             print *, ' Error: Could not open file ', trim(feedback_fname)
          end if
       end if

! The water soluble and insoluble lists are actually used to differentiate between two
! refractive index values. They do not necessarily align completely with water soluble
! and insoluble species. The detemrination for what goes into each list is from the 
! AERO_DATA table, column "OptSurr". Species with "solute" in this column will be in the
! ws_spc_index list and species with "dust" will be in the wi_spc_index list.
       allocate (ws_spc_index(n_ae_spc, n_mode),     &
                 wi_spc_index(n_ae_spc, n_mode),     &
                 stat=stat)

! to create water soluble and insoluble list
       num_ws_spc = 0
       num_wi_spc = 0
       ws_spc_index = 0
       wi_spc_index = 0
       do i = 1, n_ae_spc
          slen = len(trim(ae_spc(i)))
          if ((ae_spc(i) .ne. 'AECI') .and.       &   ! skip species that will be
              (ae_spc(i) .ne. 'AECJ') .and.       &   ! considered later in EC,
              (ae_spc(i) .ne. 'ANAJ') .and.       &   ! sea salt and H2O catergories
              (ae_spc(i) .ne. 'ACLJ') .and.       &
              (ae_spc(i) .ne. 'ACLK') .and.       &
              (ae_spc(i) .ne. 'ASO4K') .and.      &
              (ae_spc(i) .ne. 'ASEACATK') .and.    &
              (ae_spc(i) .ne. 'AH2OI') .and.      &
              (ae_spc(i) .ne. 'AH2OJ') .and.      &
              (ae_spc(i) .ne. 'AH2OK') .and.      &
              (ae_spc(i)(slen:slen) .ne. 'K')) then   ! not consider K mode ANH4K and ANO3K
             found = .false.
             k = 0
             do while ((.not. found) .and. (k .lt. n_aerospc))
               k = k + 1
               n = 0
               do while ((.not. found) .and. (n .lt. n_mode))
                  n = n + 1
                  if (aerospc(k)%name(n) .eq. ae_spc(i)) then
                     found = .true.
                  end if
               end do
             end do
             if (found) then
                if (aerospc(k)%optic_surr .eq. 'SOLUTE') then
                   num_ws_spc(n) = num_ws_spc(n) + 1
                   ws_spc_index(num_ws_spc(n), n) = i
                else if (aerospc(k)%optic_surr .eq. 'DUST') then
                   num_wi_spc(n) = num_wi_spc(n) + 1
                   wi_spc_index(num_wi_spc(n), n) = i
                end if 
             end if
          end if
       end do

END SUBROUTINE feedback_setup

! ------------------------------------------------------------------------------------
SUBROUTINE feedback_write ( c, r, l, cgrid, o3_value, jdate, jtime )

!===============================================================================
! Purpose:  Processes CMAQ data and write it to the feedback buffer file
!
! Revised:  April 2007  Original version.  David Wong
!           22 Nov 2016  Constructed water soluble and insoluble list dynamically
!                        based on a given chemical mechanism and AE scheme
!           12 Mar 2019  Implemented centralized I/O approach
!===============================================================================

! SUBROUTINE feedback_write ( c, r, l, cgrid, o3_value, aeromode_lnsg, &
!                             aeromode_diam, jdate, jtime )

  USE HGRD_DEFN
  USE aero_data
  USE UTILIO_DEFN
  USE twoway_header_data_module
  USE twoway_met_param_module
  USE twoway_data_module
  USE twoway_util_module
  USE twoway_cgrid_aerosol_spc_map_module
  Use CENTRALIZED_IO_MODULE, only : interpolate_var

  use utilio_defn
  use cgrid_spcs
  use aero_data

  IMPLICIT NONE

  real, intent(in) :: cgrid(:), o3_value
  INTEGER, INTENT(IN) :: r, c, l, jdate, jtime

  REAL,    PARAMETER :: DGMIN = 1.0E-09
  REAL(8), PARAMETER :: ONE3D = 1.0 / 3.0 
  REAL(8), PARAMETER :: TWO3D = 2.0 * ONE3D
  REAL(8), PARAMETER :: MINL2SG = 2.380480480d-03   ! minimum value of ln(Sg)**2
                                                    ! minimum sigma_g = 1.05
  REAL(8), PARAMETER :: MAXL2SG = 8.39588705d-1     ! maximum value of ln(Sg)**2
                                                    ! maximum sigma_g = 2.5

  REAL :: L2SGAT, L2SGAC

  logical, save :: firstime = .true.

  CHARACTER (LEN = 16), PARAMETER :: pname = 'feedback_write  '
  CHARACTER (LEN = 16), save :: feedback_fname
  CHARACTER (LEN = 16) :: vname
  CHARACTER (LEN = 16), PARAMETER :: MET_CRO_3D = 'MET_CRO_3D      '

  integer :: i, j, s, e, stat, rr, cc, k
  integer, save :: nlays, inumatkn, inumacc, inumcor

  real, allocatable, save :: feedback_data_cmaq (:,:,:,:)

  character (len = 4), save :: pe_str

  real, allocatable, save :: dens( :,:,: )  ! dry air density

  INTEGER   GXOFF, GYOFF      ! global origin offset from file
  integer, save :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

  CHARACTER( 96 ) :: XMSG = ' '

  IF ( firstime ) THEN

     write (pe_str, 11) '_', twoway_mype
 11  format (a1, i3.3)

     feedback_fname = 'feed_back' // pe_str

     nlays = ioapi_header%nlays

! feedback_vlist defines the feedback variable list (twoway_cgrid_aerosol_spc_map_module.F90)
! the first 22 variables are for direct aerosol effect.
     allocate ( feedback_data_cmaq (cmaq_c_ncols, cmaq_c_nrows, nlays, n_feedback_var), stat=stat)

     allocate (dens( NCOLS, NROWS, nlays ), stat=stat)

! begin: this is for indirect effect only, temporary blocked
     if (indirect_effect) then
        inumatkn = index1('NUMATKN', n_ae_spc, ae_spc) + n_gc_spcd
        inumacc  = index1('NUMACC', n_ae_spc, ae_spc) + n_gc_spcd
        inumcor  = index1('NUMCOR', n_ae_spc, ae_spc) + n_gc_spcd

        do i = 1, num_twoway_ae_cmaq_spc
           twoway_ae_cmaq_spc_name_index(i)  = index1 (twoway_ae_cmaq_spc_name(i), n_ae_spc, ae_spc) + n_gc_spcd
           if (twoway_ae_cmaq_spc_name_index(i) == n_gc_spcd) then   ! species not found
              print *, ' Warning: AE species ', trim(twoway_ae_cmaq_spc_name(i)), ' is not on the list'
           end if
        end do

        do i = 1, num_twoway_ae_cmaq_spc_other
           twoway_ae_cmaq_spc_name_other_index(i)  = index1 (twoway_ae_cmaq_spc_name_other(i), n_ae_spc, ae_spc) + n_gc_spcd
           if (twoway_ae_cmaq_spc_name_other_index(i) == n_gc_spcd) then   ! species not found
              print *, ' Warning: AE species ', trim(twoway_ae_cmaq_spc_name_other(i)), ' is not on the list'
           end if
        end do
     end if
! end: this is for indirect effect only, temporary blocked

     do j = 1, n_mode
        do i = 1, num_ws_spc(j)
           if (ws_spc_index(i,j) .gt. 0) then
              ws_spc_index(i,j) = ws_spc_index(i,j) + n_gc_spcd
           end if
        end do
     end do

     do j = 1, n_mode
        do i = 1, num_wi_spc(j)
           if (wi_spc_index(i,j) .gt. 0) then
              wi_spc_index(i,j) = wi_spc_index(i,j) + n_gc_spcd
           end if
        end do
     end do

     do i = 1, num_ec_spc
        ec_spc_index(i) = index1 (ec_spc(i), n_ae_spc, ae_spc)
        if (ec_spc_index(i) == 0) then
           write (logdev, *) ' ABORT: in aero_driver ec species ', &
                 trim(ec_spc(i)), ' is not found '
           stop
        else
           ec_spc_index(i) = ec_spc_index(i) + n_gc_spcd
        end if
     end do

     do i = 1, num_ss_spc
        ss_spc_index(i) = index1 (ss_spc(i), n_ae_spc, ae_spc)
        if (ss_spc_index(i) == 0) then
           write (logdev, *) ' ABORT: aero_driver ss species ', &
                 trim(ss_spc(i)), ' is not found '
           stop
        else
           ss_spc_index(i) = ss_spc_index(i) + n_gc_spcd
        end if
     end do

     do i = 1, num_h2o_spc
        h2o_spc_index(i) = index1 (h2o_spc(i), n_ae_spc, ae_spc)
        if (h2o_spc_index(i) == 0) then
           write (logdev, *) ' ABORT: in aero_driver h2o species ', &
                 trim(h2o_spc(i)), ' is not found '
           stop
        else
           h2o_spc_index(i) = h2o_spc_index(i) + n_gc_spcd
        end if
     end do

     CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF, STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

     firstime = .false.

  ENDIF  ! first time

! water soluble
! i mode
     feedback_data_cmaq(c,r,l, 1) = 0.0
     do i = 1, num_ws_spc(1)
        feedback_data_cmaq(c,r,l, 1) = feedback_data_cmaq(c,r,l, 1) + cgrid(ws_spc_index(i,1))
     end do

! j mode
     feedback_data_cmaq(c,r,l, 2) = 0.0
     do i = 1, num_ws_spc(2)
        feedback_data_cmaq(c,r,l, 2) = feedback_data_cmaq(c,r,l, 2) + cgrid(ws_spc_index(i,2))
     end do

! k mode
     feedback_data_cmaq(c,r,l, n_mode) = 0.0

! insoluble
! i mode
     feedback_data_cmaq(c,r,l, 4) = 0.0
     do i = 1, num_wi_spc(1)
        feedback_data_cmaq(c,r,l, 4) = feedback_data_cmaq(c,r,l, 4) + cgrid(wi_spc_index(i,1))
     end do

! j mode
     feedback_data_cmaq(c,r,l, 5) =   0.0
     do i = 1, num_wi_spc(2)
        feedback_data_cmaq(c,r,l, 5) = feedback_data_cmaq(c,r,l, 5) + cgrid(wi_spc_index(i,2))
     end do

! k mode
     feedback_data_cmaq(c,r,l, 6) =   0.0
     do i = 1, num_wi_spc(3)
        feedback_data_cmaq(c,r,l, 6) = feedback_data_cmaq(c,r,l, 6) + cgrid(wi_spc_index(i,3))
     end do

! elemental carbon
     feedback_data_cmaq(c,r,l, 7) = cgrid(ec_spc_index(1))
     feedback_data_cmaq(c,r,l, 8) = cgrid(ec_spc_index(2))
     feedback_data_cmaq(c,r,l, 9) = 0.0

! seasalt
     feedback_data_cmaq(c,r,l,10) = 0.0
     feedback_data_cmaq(c,r,l,11) =   cgrid(ss_spc_index(1))   &
                                    + cgrid(ss_spc_index(2))
     feedback_data_cmaq(c,r,l,12) =   cgrid(ss_spc_index(3))   &
                                    + cgrid(ss_spc_index(4))   &
                                    + cgrid(ss_spc_index(5))

! water
     feedback_data_cmaq(c,r,l,13) = cgrid(h2o_spc_index(1))
     feedback_data_cmaq(c,r,l,14) = cgrid(h2o_spc_index(2))
     feedback_data_cmaq(c,r,l,15) = cgrid(h2o_spc_index(3))

! diameters
     feedback_data_cmaq(c,r,l,16) = aeromode_diam(1)
     feedback_data_cmaq(c,r,l,17) = aeromode_diam(2)
     feedback_data_cmaq(c,r,l,18) = aeromode_diam(3)   ! min(cblk(VDGCO), 6.8e-6)       ! temporarily fix

! standard deviations
     feedback_data_cmaq(c,r,l,19) = EXP(aeromode_lnsg(1))
     feedback_data_cmaq(c,r,l,20) = EXP(aeromode_lnsg(2))
     feedback_data_cmaq(c,r,l,21) = 2.2

! O3
     feedback_data_cmaq(c,r,l,22) = o3_value

! AE mass  ( this is for future indirect effect)

! begin: this is for indirect effect only, temporary blocked
!   if (indirect_effect) then
!      s = 23
!      e = n_feedback_var-3
!      j = 0
!      do i = s, e
!         j = j + 1
!         if (j == 29) then
!            feedback_data_cmaq(c,r,l,i) = cgrid(twoway_ae_cmaq_spc_name_other_index(1)) +      &
!                                          cgrid(twoway_ae_cmaq_spc_name_other_index(2))
!         else if (j == 30) then
!            feedback_data_cmaq(c,r,l,i) = cgrid(twoway_ae_cmaq_spc_name_other_index(3)) +      &
!                                          cgrid(twoway_ae_cmaq_spc_name_other_index(4))
!         else if (j == 37) then
!            feedback_data_cmaq(c,r,l,i) = 0.8373 * cgrid(twoway_ae_cmaq_spc_name_other_index(5)) +  &
!                                          0.0626 * cgrid(twoway_ae_cmaq_spc_name_other_index(6)) +  &
!                                          0.0023 * cgrid(twoway_ae_cmaq_spc_name_other_index(7))
!         else if (j == 42) then
!            feedback_data_cmaq(c,r,l,i) = 2.20 * cgrid(twoway_ae_cmaq_spc_name_other_index(8))  +  &
!                                          2.49 * cgrid(twoway_ae_cmaq_spc_name_other_index(9))  +  &
!                                          1.63 * cgrid(twoway_ae_cmaq_spc_name_other_index(10)) +  &
!                                          2.42 * cgrid(twoway_ae_cmaq_spc_name_other_index(11)) +  &
!                                          1.94 * cgrid(twoway_ae_cmaq_spc_name_other_index(12))
!         else
!            feedback_data_cmaq(c,r,l,i) = cgrid(twoway_ae_cmaq_spc_name_index(j))
!         end if
!      end do
!      feedback_data_cmaq(c,r,l,n_feedback_var-2) = cgrid(inumatkn)
!      feedback_data_cmaq(c,r,l,n_feedback_var-1) = cgrid(inumacc)
!      feedback_data_cmaq(c,r,l,n_feedback_var)   = cgrid(inumcor)
!   end if
! end: this is for indirect effect only, temporary blocked

     if ((c .eq. cmaq_c_ncols) .and. (r .eq. cmaq_c_nrows) .and. (l .eq. nlays)) then
 
        call interpolate_var ('DENS', jdate, jtime, dens)
 
        if ( .not. open3 (feedback_fname, FSRDWR3, pname) ) then
           print *, ' Error: Could not open file ', feedback_fname, 'for update'
        end if

! begin: this is for indirect effect only, temporary blocked
!       if (indirect_effect) then
!          do k = 1, size(feedback_data_cmaq,3)
!             do rr = 1, size(feedback_data_cmaq,2)
!                do cc = 1, size(feedback_data_cmaq,1)
!                   do s = 23, n_feedback_var
!                      feedback_data_cmaq(cc,rr,k,s) = feedback_data_cmaq(cc,rr,k,s) / dens(cc,rr,k)
!                   end do
!                end do
!             end do
!          end do
!       end if
! end: this is for indirect effect only, temporary blocked

        if ( .not. buf_write3 (feedback_fname, allvar3, jdate, jtime, feedback_data_cmaq) ) then
           print *, ' Error: Could not write to file ', trim(feedback_fname), jdate, jtime
           stop
        end if

     end if

END SUBROUTINE feedback_write

! ------------------------------------------------------------------------------------
SUBROUTINE feedback_read (grid, jdate, jtime)

!===============================================================================
! Purpose:  Read in information from feedback buffer file and make it available
!           to WRF
!
! Revised:  April 2007  Original version.  David Wong
!           25 Sep 2015  (David Wong)
!             -- replace SUBST_MODULES with SE_MODULES
!             -- removed ae_mass access
!           08 Sep 2022  (David Wong)
!              -- fixed a bug that caused by changed value of NLAYS3D in the
!                 subsequent time step
!===============================================================================

  USE module_domain           ! WRF module
  USE module_state_description

  USE twoway_data_module
  USE twoway_met_param_module
  USE twoway_cgrid_aerosol_spc_map_module
  USE SE_MODULES
  USE HGRD_DEFN

  use utilio_defn

  IMPLICIT NONE

  TYPE(domain), INTENT(OUT) :: grid
  INTEGER, INTENT(IN)       :: jdate, jtime

  CHARACTER (LEN = 16), PARAMETER :: pname = 'feedback_read   '

  CHARACTER (LEN = 16), save :: feedback_fname

  LOGICAL, SAVE :: firstime = .TRUE.

  integer :: stat, l, c, r, s, d, e

  integer, save :: tstep = 0
  integer, save :: o3

  real, allocatable, save :: feedback_data_wrf (:,:,:,:)
  real, allocatable, save :: feedback_data_cmaq (:,:,:,:)

  logical, save :: north_bndy_pe = .false.
  logical, save :: east_bndy_pe  = .false.
  logical, save :: south_bndy_pe = .false.
  logical, save :: west_bndy_pe  = .false.

  character (len = 4), save :: pe_str

  integer, save :: loc_nlays

  tstep = tstep + 1

  if (firstime) then

     write (pe_str, 11) '_', twoway_mype
 11  format (a1, i3.3)

     feedback_fname = 'feed_back' // pe_str

     if ( .not. open3 (feedback_fname, FSREAD3, pname) ) then
        print *, ' Error: Could not open file ', trim(feedback_fname), 'for reading'
     end if

     if ( .not. desc3 (feedback_fname) ) then
        print *, ' Error: Could not get file descript of file ', trim(feedback_fname)
     end if

     o3 = 41

     allocate ( feedback_data_wrf (wrf_c_ncols, wrf_c_nrows, nlays3d, nvars3d), stat=stat)
     allocate ( feedback_data_cmaq (cmaq_c_ncols, cmaq_c_nrows, nlays3d, nvars3d), stat=stat)

     if ((twoway_nprocs - mype) .le. npcol) then
        north_bndy_pe = .true.
     end if

     if (mod(mype, npcol) .eq. npcol - 1) then
        east_bndy_pe = .true.
     end if

     if (mype .lt. npcol) then
        south_bndy_pe = .true.
     end if

     if (mod(mype, npcol) .eq. 0) then
        west_bndy_pe = .true.
     end if

     loc_nlays = nlays3d

     firstime = .false.

  end if

  if ( .not. read3(feedback_fname, allvar3, allays3, jdate, jtime, feedback_data_cmaq) ) then
     print *, ' Error: Could not read data from file ', trim(feedback_fname)
     stop
  end if

  feedback_data_wrf = 0.0

  call se_cmaq_wrf_comm4 (twoway_mype, feedback_data_cmaq,                             &
                         feedback_data_wrf, cmaq_wrf_c_send_to, cmaq_wrf_c_recv_from, &
                         cmaq_wrf_c_send_index_l, cmaq_wrf_c_recv_index_l, 6)

 if (north_bndy_pe) then
    s = cmaq_c_domain_map(2,2,mype) - tw_sr + 1
    do r = cmaq_c_domain_map(2,2,mype)+1, wrf_c_domain_map(2,2,mype)
       feedback_data_wrf(:,r-tw_sr+1,:,:) = feedback_data_wrf(:,s,:,:)
    end do
 end if

 if (east_bndy_pe) then
    s = cmaq_c_domain_map(2,1,mype) - tw_sc + 1
    d = wrf_c_domain_map(2,1,mype) - cmaq_c_domain_map(2,1,mype)
    do r = lbound(feedback_data_wrf,2), ubound(feedback_data_wrf,2)
       do c = s+1, s+d
          feedback_data_wrf(c,r,:,:) = feedback_data_wrf(s,r,:,:)
       end do
    end do
 end if

 if (south_bndy_pe) then
    do r = 1, delta_y
       feedback_data_wrf(:,r,:,:) = feedback_data_wrf(:,delta_y+1,:,:)
    end do
 end if

 if (west_bndy_pe) then
    do r = lbound(feedback_data_wrf,2), ubound(feedback_data_wrf,2)
       do c = 1, delta_x
          feedback_data_wrf(c,r,:,:) = feedback_data_wrf(delta_x+1,r,:,:)
       end do
    end do
 end if

  do l = 1, loc_nlays
     do r = tw_sr, tw_er
        do c = tw_sc, tw_ec
           grid%mass_ws_i(c, l, r)  = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,1)
           grid%mass_ws_j(c, l, r)  = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,2)
           grid%mass_ws_k(c, l, r)  = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,3)
           grid%mass_in_i(c, l, r)  = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,4)
           grid%mass_in_j(c, l, r)  = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,5)
           grid%mass_in_k(c, l, r)  = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,6)
           grid%mass_ec_i(c, l, r)  = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,7)
           grid%mass_ec_j(c, l, r)  = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,8)
           grid%mass_ec_k(c, l, r)  = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,9)
           grid%mass_ss_i(c, l, r)  = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,10)
           grid%mass_ss_j(c, l, r)  = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,11)
           grid%mass_ss_k(c, l, r)  = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,12)
           grid%mass_h2o_i(c, l, r) = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,13)
           grid%mass_h2o_j(c, l, r) = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,14)
           grid%mass_h2o_k(c, l, r) = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,15)
           grid%dgn_i(c, l, r)      = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,16)
           grid%dgn_j(c, l, r)      = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,17)
           grid%dgn_k(c, l, r)      = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,18)
           grid%sig_i(c, l, r)      = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,19)
           grid%sig_j(c, l, r)      = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,20)
           grid%sig_k(c, l, r)      = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,21)
           grid%ozone(c, l, r)      = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,22)
! begin: this is for indirect effect only, temporary blocked
!          if (indirect_effect) then
!             s = 0
!             do d = 23, N_FEEDBACK_VAR-3
!                s = s + 1
!                grid%ae_mass(c, l, r, s) = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,d)
!             end do
!             grid%ae_num(c, l, r, 1:3) = feedback_data_wrf(c-tw_sc+1,r-tw_sr+1,l,N_FEEDBACK_VAR-2:N_FEEDBACK_VAR)
!          end if
! end: this is for indirect effect only, temporary blocked
        end do
     end do
  end do

  grid%mass_ws_i(:,loc_nlays+1,:) = grid%mass_ws_i(:,loc_nlays,:)
  grid%mass_ws_j(:,loc_nlays+1,:) = grid%mass_ws_j(:,loc_nlays,:)
  grid%mass_ws_k(:,loc_nlays+1,:) = grid%mass_ws_k(:,loc_nlays,:)
  grid%mass_in_i(:,loc_nlays+1,:) = grid%mass_in_i(:,loc_nlays,:)
  grid%mass_in_j(:,loc_nlays+1,:) = grid%mass_in_j(:,loc_nlays,:)
  grid%mass_in_k(:,loc_nlays+1,:) = grid%mass_in_k(:,loc_nlays,:)
  grid%mass_ec_i(:,loc_nlays+1,:) = grid%mass_ec_i(:,loc_nlays,:)
  grid%mass_ec_j(:,loc_nlays+1,:) = grid%mass_ec_j(:,loc_nlays,:)
  grid%mass_ec_k(:,loc_nlays+1,:) = grid%mass_ec_k(:,loc_nlays,:)
  grid%mass_ss_i(:,loc_nlays+1,:) = grid%mass_ss_i(:,loc_nlays,:)
  grid%mass_ss_j(:,loc_nlays+1,:) = grid%mass_ss_j(:,loc_nlays,:)
  grid%mass_ss_k(:,loc_nlays+1,:) = grid%mass_ss_k(:,loc_nlays,:)
  grid%mass_h2o_i(:,loc_nlays+1,:) = grid%mass_h2o_i(:,loc_nlays,:)
  grid%mass_h2o_j(:,loc_nlays+1,:) = grid%mass_h2o_j(:,loc_nlays,:)
  grid%mass_h2o_k(:,loc_nlays+1,:) = grid%mass_h2o_k(:,loc_nlays,:)
  grid%dgn_i(:,loc_nlays+1,:) = grid%dgn_i(:,loc_nlays,:)
  grid%dgn_j(:,loc_nlays+1,:) = grid%dgn_j(:,loc_nlays,:)
  grid%dgn_k(:,loc_nlays+1,:) = grid%dgn_k(:,loc_nlays,:)
  grid%sig_i(:,loc_nlays+1,:) = grid%sig_i(:,loc_nlays,:)
  grid%sig_j(:,loc_nlays+1,:) = grid%sig_j(:,loc_nlays,:)
  grid%sig_k(:,loc_nlays+1,:) = grid%sig_k(:,loc_nlays,:)

! begin: this is for indirect effect only, temporary blocked
! if (indirect_effect) then
!    grid%ae_mass(:,loc_nlays+1,:,:) = grid%ae_mass(:,loc_nlays,:,:)
!    grid%ae_num(:,loc_nlays+1,:,:)  = grid%ae_num(:,loc_nlays,:,:)
! end if
! end: this is for indirect effect only, temporary blocked

END SUBROUTINE feedback_read
