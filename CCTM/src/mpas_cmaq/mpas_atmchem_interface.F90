 module mpas_atmchem_interface

   use mpas_kind_types
   use mpas_derived_types
   use mpas_pool_routines
 
   use mpas_atmphys_vars

   use get_env_module
   use RUNTIME_VARS, only : logdev

   use mpi

   implicit none

   private :: convert_time, find_n_items_in_namelist

   contains

   subroutine mpas_cmaq_coupler (config_dt, domain, mesh, state,        &
                                 diag, diag_physics, sfc_input, gfso3)

      use mpas_timekeeping
      use coupler_module
      use util_module, only : time2sec
      use RXNS_DATA, only : MECHNAME

! ppmv = MR * 1e6 * 28.97 / 47.9982 = MR * 603564.300327929
     real, parameter :: o3_convert_factor = 603564.300327929
     real, parameter :: rad_to_deg_factor = 57.295779513

     real(kind=RKIND), pointer           :: config_dt
     type(domain_type),    intent(in)    :: domain
     type(mpas_pool_type), intent(in)    :: mesh
     type(mpas_pool_type), intent(inout) :: state
     type(mpas_pool_type), intent(in)    :: diag
     type(mpas_pool_type), intent(in)    :: diag_physics
     type(mpas_pool_type), intent(in)    :: sfc_input
     type(mpas_pool_type), intent(in)    :: gfso3

! local pointers:
     integer, parameter :: n_so4 = 3

     integer, pointer :: index_qv, index_qc, index_qr, index_qi, index_qs, index_qg

     real(kind=RKIND), dimension(:),   pointer :: latCell, lonCell, xland, xice, snowc, &
                                                  t2m, skintemp, ter, prsfc, rs, ra,    &
                                                  rmol, hpbl, ustar, hfx, lh, canwat,   &
                                                  lai, vegpx, znt, q2, rainc, rainnc,   &
                                                  cldfract, cldfracwcut, areaCell,      &
                                                  gfs_ptrop, u10, v10, rgrnd &
                                                  ,laim, laip
                                                  
     real(kind=RKIND), dimension(:,:), pointer :: zgrid, cldfrac, rho, theta, &
                                                  landusef, smois, qv, qc, qr, qi, qs,  &
                                                  qg, eddy, pressure_p, pressure_base,  &
                                                  cldfracwcu, qc_cu, qi_cu, gfs_ozone,  &
                                                  uReconstructMeridional,               &
                                                  uReconstructZonal,tslb &
                                                  ,lai12

     real(kind=RKIND), allocatable, save :: pressure(:,:)

     real(kind=RKIND), dimension(:,:,:), pointer :: scalars

     real(kind=RKIND), allocatable :: ta(:,:)

     integer, dimension(:), pointer :: ivgtyp, lw, sltyp

     character(len=StrKIND), pointer :: mminlu

     integer, pointer :: num_scalars
 
     character(len=StrKIND)          :: timeStamp
     type (MPAS_Time_Type)           :: currTime
     type (MPAS_TimeInterval_Type)   :: xtimeTime
     type (MPAS_Clock_type), pointer :: clock

     integer :: ierr, i, v, k 
     integer, save :: mpas_date, mpas_time, model_tstep, nsteps, jdate, jtime, runlen, couple_tstep
     logical, save :: firstime = .true.
     logical, save :: initialized = .false.
     logical, save :: called_convert_time = .false.
     integer, save :: counter = -1
     integer, save :: mpas_cmaq_freq
     logical, save :: run_cmaq_driver, mpas_restart
     integer, save :: total_step = 0
     logical       :: cmaq_step, all_zeros
     character(96) :: MSG = ' '
     character(1000) :: namelist

     integer :: my_n_gc
     integer :: my_n_ae
     integer :: my_n_nr

     integer :: min_loc(3), lwater, lice, so4_ind(n_so4)
     integer, save :: lmype

     counter = counter + 1

     if (firstime) then
        call mpi_comm_rank (mpi_comm_world, lmype, ierr)

        call get_env (mpas_cmaq_freq, 'mpas_cmaq_freq', 1)

        call get_env (mpas_restart, 'mpas_restart', .false.)

        call get_env (run_cmaq_driver, 'run_cmaq_driver', .false.)

        call get_env (model_tstep, 'CTM_TSTEP', 10000)
        call get_env (runlen, 'CTM_RUNLEN', 240000)

        call get_env (namelist, 'gc_matrix_nml', ' ')
        call find_n_items_in_namelist (namelist, my_n_gc)
        call get_env (namelist, 'ae_matrix_nml', ' ')
        call find_n_items_in_namelist (namelist, my_n_ae, so4_ind)
        call get_env (namelist, 'nr_matrix_nml', ' ')
        call find_n_items_in_namelist (namelist, my_n_nr)

        num_cmaq_species = my_n_gc + my_n_ae + my_n_nr


        so4_ind = so4_ind + my_n_gc


        allocate (g2ddata(ite, 1, n2d_data),                          &
                  g3ddata(ite, 1, kte, n3d_data),                     &
                  cell_area(ite, 1),                                  &
                  cell_vol(ite, 1, kte),                              &
                  inv_cell_vol(ite, 1, kte),                          &
                  cell_thickness(ite, 1, kte),                        &
                  inv_cell_thickness(ite, 1, kte),                    &
                  inv_mlvl_cell_thickness(ite, 1, kte),               &
                  smois_data(ite, 1, 2),                              &
                  pressure(kte, ite),                                 &
                  cmaq_species(ite, 1, kte, num_cmaq_species),        &
                  stat=ierr)
        if (ierr .ne. 0) then
           print *, ' Error: Cannot allocate memory in mpas_to_chem_var_mapping'
           stop
        end if 

        couple_tstep = int(config_dt * real(mpas_cmaq_freq))

        total_step = (time2sec( runlen ) / int(config_dt * mpas_cmaq_freq) - 1) * mpas_cmaq_freq
     end if


     call mpas_pool_get_dimension (state, 'num_scalars', num_scalars)

     if ( .not. mpas_restart .and. .not. initialized) then
        cmaq_species = 1.0e-20

        ! Initialize Ozone
        cmaq_species(:, 1, 1:18  , 1 ) = 20.0    * 1.0e-3
        cmaq_species(:, 1, 19:40 , 1)  = 50.0    * 1.0e-3
        cmaq_species(:, 1, 41    , 1)  = 70.0    * 1.0e-3
        cmaq_species(:, 1, 42    , 1)  = 100.0   * 1.0e-3
        cmaq_species(:, 1, 43    , 1)  = 200.0   * 1.0e-3
        cmaq_species(:, 1, 44    , 1)  = 500.0   * 1.0e-3
        cmaq_species(:, 1, 45    , 1)  = 1000.0  * 1.0e-3
        cmaq_species(:, 1, 46    , 1)  = 2000.0  * 1.0e-3
        cmaq_species(:, 1, 47    , 1)  = 3000.0  * 1.0e-3
        cmaq_species(:, 1, 48    , 1)  = 4000.0  * 1.0e-3
        cmaq_species(:, 1, 49    , 1)  = 6000.0  * 1.0e-3
        cmaq_species(:, 1, 50    , 1)  = 8000.0  * 1.0e-3

        ! Initialize Aerosols Modes with a negligible amount of SO4
        do v = 1, n_so4
           cmaq_species(:, 1, :, so4_ind(v) ) = 1.0e-5
        end do

     end if


     if (run_cmaq_driver) then

        cmaq_step = (mod(counter, mpas_cmaq_freq) .eq. 0)

        if (cmaq_step) then

           call mpas_pool_get_array (mesh, 'latCell', latCell)
           call mpas_pool_get_array (mesh, 'lonCell', lonCell)
           call mpas_pool_get_array (mesh, 'zgrid', zgrid)
           call mpas_pool_get_array (mesh, 'areaCell', areaCell)

           call mpas_pool_get_array (diag, 'uReconstructMeridional', uReconstructMeridional)
           call mpas_pool_get_array (diag, 'uReconstructZonal', uReconstructZonal)

! compute wind speed using orthogonal wind components, uReconstructMeridional and uReconstructZonal
           do k = 1, kte
              do i = 1, ite
                 g3ddata(i, 1, k, wspd_ind) = sqrt(  uReconstructMeridional(k,i) * uReconstructMeridional(k,i)   &
                                                   + uReconstructZonal(k,i) * uReconstructZonal(k,i))
              end do
           end do

           call mpas_pool_get_array (sfc_input, 'xland', xland)
           call mpas_pool_get_array (sfc_input, 'landmask', lw)
           call mpas_pool_get_array (sfc_input, 'xice', xice)
           call mpas_pool_get_array (sfc_input, 'snowc', snowc)
           call mpas_pool_get_array (sfc_input, 'skintemp', skintemp)
           call mpas_pool_get_array (sfc_input, 'ter', ter)
           call mpas_pool_get_array (sfc_input, 'isltyp', sltyp)
           call mpas_pool_get_array (sfc_input, 'ivgtyp', ivgtyp)
           call mpas_pool_get_array (sfc_input, 'mminlu', mminlu)
           call mpas_pool_get_array (sfc_input, 'landusef', landusef)
           call mpas_pool_get_array (sfc_input, 'smois', smois)
           call mpas_pool_get_array (sfc_input, 'tslb', tslb)


           call mpas_pool_get_array (gfso3, 'ozone', gfs_ozone)
           call mpas_pool_get_array (gfso3, 'ptrop', gfs_ptrop)

           call mpas_pool_get_array (diag_physics, 't2m', t2m)
           call mpas_pool_get_array (diag_physics, 'rs', rs)
           call mpas_pool_get_array (diag_physics, 'ra', ra)
           call mpas_pool_get_array (diag_physics, 'rmol', rmol)
           call mpas_pool_get_array (diag_physics, 'hpbl', hpbl)
           call mpas_pool_get_array (diag_physics, 'ust', ustar)
           call mpas_pool_get_array (diag_physics, 'hfx', hfx)
           call mpas_pool_get_array (diag_physics, 'lai_px', laip)
           call mpas_pool_get_array (diag_physics, 'u10', u10)
           call mpas_pool_get_array (diag_physics, 'v10', v10)
           call mpas_pool_get_array (diag_physics, 'swdnb', rgrnd)

! compute 10m wind speed using orthogonal wind components, u10 and v10
           do i = 1, ite
              g2ddata(i, 1, wspd10_ind) = sqrt(  u10(i) * u10(i)   &
                                               + v10(i) * v10(i))
           end do


           call mpas_pool_get_array (diag_physics, 'vegpx', vegpx)
           call mpas_pool_get_array (diag_physics, 'znt', znt)
           call mpas_pool_get_array (diag_physics, 'q2', q2)
           call mpas_pool_get_array (diag_physics, 'lh', lh)
           call mpas_pool_get_array (diag_physics, 'canwat', canwat)
           call mpas_pool_get_array (diag_physics, 'raincv', rainc)
           call mpas_pool_get_array (diag_physics, 'rainncv', rainnc)
           call mpas_pool_get_array (diag_physics, 'cldfract', cldfract)
           call mpas_pool_get_array (diag_physics, 'cldfracwcut', cldfracwcut)

           call mpas_pool_get_array (diag_physics, 'cldfrac', cldfrac)
           call mpas_pool_get_array (diag_physics, 'cldfracwcu', cldfracwcu)
           call mpas_pool_get_array (diag_physics, 'qc_cu', qc_cu)
           call mpas_pool_get_array (diag_physics, 'qi_cu', qi_cu)
           call mpas_pool_get_array (diag, 'pressure_p', pressure_p)
           call mpas_pool_get_array (diag, 'pressure_base', pressure_base)
           call mpas_pool_get_array (diag, 'rho', rho)
           call mpas_pool_get_array (diag, 'theta', theta)
           call mpas_pool_get_array (diag, 'surface_pressure', prsfc)
           call mpas_pool_get_array (diag_physics, 'exch_h', eddy)

!          pressure = pressure_p + pressure_base
           pressure = pressure_p(:,1:ite) + pressure_base(:,1:ite) + 1.0

           allocate (ta(kte, ite), stat=ierr)


           call mpas_pool_get_array (state, 'scalars', scalars, 2)
           call mpas_pool_get_dimension (state, 'index_qv', index_qv )
           call mpas_pool_get_dimension (state, 'index_qc', index_qc )
           call mpas_pool_get_dimension (state, 'index_qr', index_qr )
           call mpas_pool_get_dimension (state, 'index_qi', index_qi )
           call mpas_pool_get_dimension (state, 'index_qs', index_qs )
           call mpas_pool_get_dimension (state, 'index_qg', index_qg )


           qv => scalars(index_qv , :,: )
           qc => scalars(index_qc , :,: )
           qr => scalars(index_qr , :,: )
           qi => scalars(index_qi , :,: )
           qs => scalars(index_qs , :,: )
           qg => scalars(index_qg , :,: )

           ! convert potential temperature to temperature which CMAQ requires
           ta = theta(:,1:ite) / ( ( 1.0 + 0.608 * qv(:,1:ite) ) * ( ( 100000.0 / pressure ) ** 0.286 ))


           clock => domain % clock

           currTime = mpas_get_clock_time(clock, MPAS_NOW, ierr)

           call mpas_get_time(curr_time=currTime, dateTimeString=timeStamp, ierr=ierr)

           call convert_time (timeStamp, mpas_date, mpas_time)

           mminlu_mpas = mminlu

           my_gc_adj = num_scalars - num_cmaq_species


           if (.not. allocated(lufrac_data)) then
              allocate (lufrac_data(size(landusef,1), size(landusef,2)), stat=ierr)
              if (ierr .ne. 0) then
                 print *, ' Error: Cannot allocate lufrac_data'
                 stop
              end if

              lufrac_data = landusef
           end if


           smois_data(:,1,1) = smois(1,1:ite)
           smois_data(:,1,2) = smois(2,1:ite) ! gamsm deep soil for MEGAN


           g2ddata(:, 1, lon_ind) = lonCell(1:ite) * rad_to_deg_factor       ! convert to degree which CMAQ requires
           g2ddata(:, 1, lat_ind) = latCell(1:ite) * rad_to_deg_factor       ! convert to degree which CMAQ requires
           ! in MPAS lonCell is 0 - 360 counter clockwise, in CMAQ longitude is 0 - 180 counter clockwise 
           ! and 0 - -180 clockwise
           where (g2ddata(:, 1, lon_ind) > 180.0) g2ddata(:, 1, lon_ind) = g2ddata(:, 1, lon_ind) - 360.0

           g2ddata(:, 1, lwmask_ind) = xland(1:ite)

! in case OCEAN file does not exist, setup ocean bases on ground level height and xland paramenter,
! and surf zone is set to 0 for now
           do i = 1, ite
              if ((zgrid(1, i) .le. 20.0) .and. (xland(i) .ge. 1.5)) then
                 g2ddata(i, 1, open_ind) = 1     ! ocean
              else
                 g2ddata(i, 1, open_ind) = 0
              end if
           end do
           g2ddata(:, 1, surf_ind)   = 0

           g2ddata(:, 1, seaice_ind) = xice(1:ite)
           g2ddata(:, 1, prsfc_ind)  = prsfc(1:ite)
           g2ddata(:, 1, snocov_ind) = snowc(1:ite)
           g2ddata(:, 1, temp2_ind)  = t2m(1:ite)
           g2ddata(:, 1, tempg_ind)  = skintemp(1:ite)
           g2ddata(:, 1, ht_ind)     = ter(1:ite)
           g2ddata(:, 1, rmol_ind)   = rmol(1:ite)
           g2ddata(:, 1, rs_ind)     = rs(1:ite)
           g2ddata(:, 1, ra_ind)     = ra(1:ite)
           g2ddata(:, 1, pbl_ind)    = hpbl(1:ite)
           g2ddata(:, 1, ustar_ind)  = ustar(1:ite)
           g2ddata(:, 1, hfx_ind)    = hfx(1:ite)
           g2ddata(:, 1, lh_ind)     = lh(1:ite)
           g2ddata(:, 1, canwat_ind) = canwat(1:ite)*0.001 ! kg/m2 -> m
           g2ddata(:, 1, lai_ind)    = laip(1:ite)
           g2ddata(:, 1, vegpx_ind)  = vegpx(1:ite)
           g2ddata(:, 1, sltyp_ind)  = real(sltyp(1:ite))
           g2ddata(:, 1, rgrnd_ind)  = rgrnd(1:ite)
           g2ddata(:, 1, soit1_ind)  = tslb(1,1:ite)

           g2ddata(:, 1, znt_ind)    = znt(1:ite)
           g2ddata(:, 1, q2_ind)     = q2(1:ite)

           if ((mminlu == 'MODIS') .or. (mminlu == 'MODIFIED_IGBP_MODIS_NOAH')) then
              lwater = 17
              lice   = 15
           else if (mminlu == 'NLCD40') then
              lwater = 17
              lice   = 15
           end if

! to compute PURB
           do i = 1, ite
              if (landusef(lwater,i) == 1.0) then
                 g2ddata(i,1,purb_ind) = 0.0
              else 
                 if ((mminlu == 'MODIS') .or. (mminlu == 'MODIFIED_IGBP_MODIS_NOAH')) then
                    g2ddata(i,1,purb_ind) = ( landusef(13,i) /  (1.0 - landusef(lwater,i)) ) * 100.0
                 else if (mminlu == 'NLCD40') then
                    g2ddata(i,1,purb_ind) = ( ( landusef(23,i) * 0.10 +    &
                                                landusef(24,i) * 0.35 +    &
                                                landusef(25,i) * 0.65 +    &
                                                landusef(26,i) * 0.90 +    &
                                                landusef(13,i)        ) /  &
                                              (1.0 - landusef(lwater,i)) ) * 100.0
                 else
                    print *, ' Warning:: Unknow Land Use type'
                    g2ddata(:, 1, purb_ind)     = 0.0
                 end if
              end if
           
              if (rainc(i) >= 0.0) then
                 g2ddata(i, 1, rainc_ind)    = rainc(i) / 10.0 ! convert mm->cm
              else
                 g2ddata(i, 1, rainc_ind)    = 0.0
              end if
           end do

           g2ddata(:, 1, rainnc_ind)   = rainnc(1:ite)/10.0! convert mm->cm

           g2ddata(:, 1, cfrac2dr_ind) = cldfract(1:ite)
           g2ddata(:, 1, cfrac2dt_ind) = cldfracwcut(1:ite)

           if (firstime) then
              cell_area(:,1) = areaCell(1:ite)
              g2ddata(:, 1, cellArea_ind) = areaCell(1:ite)
              do k = 1, kte
                 do i = 1, ite
                    g3ddata(i, 1, k, zf_ind)    = (zgrid(k+1, i) - zgrid(1, i))

                    if (k .eq. 1) then
                       g3ddata(i, 1, k, zh_ind) = g3ddata(i, 1, 1, zf_ind) * 0.5
                       cell_thickness(i, 1, k)  = g3ddata(i, 1, k, zf_ind)
                    else
                       g3ddata(i, 1, k, zh_ind) = (g3ddata(i, 1, k, zf_ind) + g3ddata(i, 1, k-1, zf_ind)) * 0.5
                       cell_thickness(i, 1, k)  = g3ddata(i, 1, k, zf_ind) - g3ddata(i, 1, k-1, zf_ind)
                       inv_mlvl_cell_thickness(i, 1, k-1) = 1.0 / (g3ddata(i, 1, k, zh_ind)  - g3ddata(i, 1, k-1, zh_ind))
                    end if

                    cell_vol(i, 1, k)            = cell_thickness(i, 1, k) * cell_area(i,1)
                    inv_cell_thickness(i, 1, k)  = 1.0 / cell_thickness(i, 1, k)
                    inv_cell_vol(i, 1, k)        = inv_cell_thickness(i, 1, k) / cell_area(i,1)

                 end do
              end do
              inv_mlvl_cell_thickness(:, 1, kte) = inv_mlvl_cell_thickness(:, 1, kte-1)
           end if

           do k = 1, kte
              do i = 1, ite
                 g3ddata(i, 1, k, cfrac3d_ind)    = cldfrac(k, i)
                 g3ddata(i, 1, k, cldfracwcu_ind) = cldfracwcu(k, i)
                 g3ddata(i, 1, k, pres_ind)       = pressure(k, i)
                 g3ddata(i, 1, k, dens_ind)       = rho(k, i)
                 g3ddata(i, 1, k, temp_ind)       = ta(k, i)
                 g3ddata(i, 1, k, qv_ind)         = qv(k, i)
                 g3ddata(i, 1, k, qc_ind)         = qc(k, i)
                 g3ddata(i, 1, k, qr_ind)         = qr(k, i)
                 g3ddata(i, 1, k, qg_ind)         = qg(k, i)
                 g3ddata(i, 1, k, qi_ind)         = qi(k, i)
                 g3ddata(i, 1, k, qs_ind)         = qs(k, i)
                 g3ddata(i, 1, k, qc_cu_ind)      = qc_cu(k, i)
                 g3ddata(i, 1, k, qi_cu_ind)      = qi_cu(k, i)
!                g3ddata(i, 1, k, densa_j_ind)    = 1.0 / rho(k, i)
                 g3ddata(i, 1, k, densa_j_ind)    = rho(k, i)
                 g3ddata(i, 1, k, eddy_ind)       = eddy(k, i)
              end do
           end do

           deallocate (ta)

           if (mpas_restart .or. initialized) then
              do k = 1, kte
                 do i = 1, ite
                    do v = 1, num_cmaq_species
                       cmaq_species(i, 1, k, v) = scalars(v+my_gc_adj, k, i)
                    end do
                 end do
              end do
           end if


           if ( .not. mpas_restart .and. .not. initialized) then
              initialized = .true.
! use initial value of ozone from another global model which has been incorporated in the first time step of the GFS file
              do k = 1, kte
                 do i = 1, ite
                    cmaq_species(i, 1, k, 1) = gfs_ozone(k,i) * o3_convert_factor
                 end do
              end do
           else
! alternative to nudging; overwrite values
                do k = 1, kte
                    do i = 1, ite
                      if ((pressure(k,i) .le. 30000.0) .and. (gfs_ozone(k,i)*o3_convert_factor .gt. 0.2)) then
                         ! In CB6 ozone is 4
                         !cmaq_species(i, 1, k, 4) = gfs_ozone(k,i) * o3_convert_factor
                         ! In CRACMM ozone is 1
                         cmaq_species(i, 1, k, 1) = gfs_ozone(k,i) * o3_convert_factor
                      end if
                    end do
                 end do
           end if

           if (counter >= 1) then


              call cmaq_driver (mpas_date, mpas_time, model_tstep, jdate, jtime, &
                                (counter .eq. total_step), couple_tstep, ite, kte)

              ! transfer species information back to MPAS
              do k = 1, kte
                 do i = 1, ite
                    do v = 1, num_cmaq_species
                       scalars(v+my_gc_adj, k, i) = cmaq_species(i, 1, k, v)
                    end do
                 end do
              end do
           end if


        end if  ! end of cmaq_step
     end if  ! end of run_cmaq_driver

     if (firstime) then
        firstime = .false.

        if ( .not. mpas_restart ) then
           do k = 1, kte
              do i = 1, ite
                 do v = 1, num_cmaq_species
                    scalars(v+my_gc_adj, k, i) = 1e-10
                 end do
              end do
           end do
        end if

        ! this initialize CMAQ data in scalar array at the very first step
        if ( run_cmaq_driver ) then
           if ( .not. mpas_restart ) then
              do k = 1, kte
                 do i = 1, ite
                    do v = 1, num_cmaq_species
                       scalars(v+my_gc_adj, k, i) = cmaq_species(i, 1, k, v)
                    end do
                 end do
              end do
           end if
        else
           ! do nothing
        end if
     end if

   end subroutine mpas_cmaq_coupler

! -------------------------------------------------------------------

   subroutine convert_time (timeStamp, mpas_date, mpas_time)

     use util_module, only : leap_year

     character(len=StrKIND), intent(in) :: timeStamp
     integer, intent(out) :: mpas_date, mpas_time

     integer, parameter :: ly_days(12) = (/0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335 /)
     integer, parameter :: ry_days(12) = (/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 /)

     integer :: year, month, day, hour, min, sec, tday

     read (timeStamp, '(i4, 5(1x, i2))') year, month, day, hour, min, sec

     if (leap_year(year)) then
        tday = ly_days(month) + day
     else
        tday = ry_days(month) + day
     end if

     mpas_date = year * 1000 + tday
     mpas_time = hour * 10000 + min * 100 + sec

   end subroutine convert_time

! -------------------------------------------------------------------
   subroutine find_n_items_in_namelist (file, n, so4_ind)

     character (*), intent(in)      :: file
     integer, intent(out)           :: n
     integer, intent(out), optional :: so4_ind(3)

     integer, parameter :: iunit = 7
     integer :: loc_n, stat, n_so4, mloc(3)
     logical :: eof
     character (200) :: line

     open (unit = iunit, file=file, status='old')

     mloc = 0
     n_so4 = 0
     loc_n = 0
     eof = .false.
     do while (.not. eof)
        read (iunit, '(a200)', iostat=stat) line
        if (stat .ne. 0) then
           eof = .true.
        else
           if (line(1:1) == "'") then
              loc_n = loc_n + 1
              if (present(so4_ind)) then
                 if (line(22:22) == 'T') then
                    mloc(1) = mloc(1) + 1
                 end if
                 if (line(30:30) == 'T') then
                    mloc(2) = mloc(2) + 1
                 end if
                 if (line(37:37) == 'T') then
                    mloc(3) = mloc(3) + 1
                 end if
                 if (line(2:5) == 'ASO4') then
!                if ((line(2:6) == 'ASO4J') .or.  &
!                    (line(2:6) == 'ASO4I') .or.  &
!                    (line(2:6) == 'ASO4K')) then
                    n_so4 = n_so4 + 1
!                   so4_ind(n_so4) = loc_n
                    so4_ind(1) = loc_n
                    so4_ind(2) = loc_n + 1
                    so4_ind(3) = loc_n + 2
                 end if
              end if
           end if
        end if
     end do

     if (present(so4_ind)) then
        n = sum(mloc)
     else
        n = loc_n
     end if

     close (iunit)

   end subroutine find_n_items_in_namelist

 end module mpas_atmchem_interface
