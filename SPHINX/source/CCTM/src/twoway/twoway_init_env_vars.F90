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

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

        subroutine twoway_init_env_vars

!===============================================================================
! Name:     twoway_init_env_vars Prep
! Purpose:  process all twoway related environment variables
! Revised:  31 Jan 2019  Original version.  (D. Wong)
!           01 Aug 2019  (D. Wong)
!              -- removed usage of status in calling get_env
!===============================================================================

          use twoway_data_module
          use sd_time_series_module, only : sd_scol, sd_ecol, sd_srow, sd_erow
          use get_env_module

!         call get_env (wrf_restart, "WRF_RSTFLAG", .false.)

          call get_env (sd_time_series, 'SD_TIME_SERIES', .false.)

!         call get_env (create_physical_file, 'CREATE_PHYSICAL_FILE', .false.)

!         call get_env (wrf_cmaq_freq, 'WRF_CMAQ_FREQ', 1)

!         call get_env (run_cmaq_driver, 'RUN_CMAQ_DRIVER', .false.)

!         call get_env (cmaq_wrf_feedback, 'CMAQ_WRF_FEEDBACK', .false.)

!         call get_env (indirect_effect, 'INDIRECT_EFFECT', .false.)

          call get_env (cmaq_sdate, 'CTM_STDATE', 0)

          call get_env (cmaq_stime, 'CTM_STTIME', 0)

          call get_env (turn_on_pv, 'CTM_PVO3', .false.)

          call get_env (wrf_lc_ref_lat, 'WRF_LC_REF_LAT', 0.0)

          call get_env (delta_x, 'TWOWAY_DELTA_X', 5)

          call get_env (delta_y, 'TWOWAY_DELTA_Y', 5)

          call get_env (file_time_step, 'FILE_TIME_STEP', 10000)

          if (sd_time_series) then
             call get_env (sd_scol, 'SD_SCOL', 1)
             call get_env (sd_ecol, 'SD_ECOL', 1)
             call get_env (sd_srow, 'SD_SROW', 1)
             call get_env (sd_erow, 'SD_EROW', 1)
          end if

          call get_env (griddesc_fname, 'GRIDDESC', ' ')
          call get_env (grid_name_str, 'GRID_NAME', ' ')

        end subroutine twoway_init_env_vars
