
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

C-----------------------------------------------------------------------
      Subroutine process_aero_nml_modes
          
C         This function expands the AE namelist rows to account for 
C         mode-specific variables. If just one mode is requested for an 
C         aerosol species, then no rows are added. If more than one mode
C         is requested, then rows are added to accomodate those. 
C
C         Ben Murphy 2022-03-25
C-----------------------------------------------------------------------
          Use cgrid_spcs_types

          Implicit None

          Type(species_data_reg_2) :: aedat( size( ae_species_data,1 ) )
          Type(species_data_reg_2) :: aedat_mode( size( ae_species_data,1 ) )

          integer nsp2, naero

          Integer isp, im, iaer, iost
          Character(300) xmsg
          Character(20) :: pname = 'process_aero_species'
                    
          aedat = ae_species_data
          aedat_mode = aedat
          nsp2 = 0


          do isp = 1,size( aedat,1 )
              if ( aedat(isp)%species_name .EQ. '' ) EXIT

              ! For Aitken-Mode Particles
              if ( aedat(isp)%lait ) then
                 if ( aedat(isp)%species_name .eq. 'NUM' .OR. 
     &                aedat(isp)%species_name .eq. 'SRF' ) THEN
                    ! Add Aitken Mode Number or Surface Area
                    nsp2 = nsp2 + 1
                    aedat_mode(nsp2)%species_name = 
     &                    trim(aedat(isp)%species_name)//'ATKN'
                    aedat_mode(nsp2)%depv_surr = 
     &                    trim(aedat(isp)%depv_surr)//'ATKN'
                    aedat_mode(nsp2)%scav_surr = 
     &                    trim(aedat(isp)%scav_surr)//'_AITKEN'
                    aedat_mode(nsp2)%cloud_surr = 
     &                    trim(aedat(isp)%cloud_surr)//'_AITKEN'
                 else
                    ! Add Aitken Mode Mass
                    nsp2 = nsp2 + 1
                    aedat_mode(nsp2)%species_name = 
     &                    trim(aedat(isp)%species_name)//'I'
                    aedat_mode(nsp2)%depv_surr = 
     &                    trim(aedat(isp)%depv_surr)//'I'
                    aedat_mode(nsp2)%scav_surr = 
     &                    trim(aedat(isp)%scav_surr)//'_AITKEN'
                    aedat_mode(nsp2)%cloud_surr = 
     &                    trim(aedat(isp)%cloud_surr)//'_AITKEN'
                 end if
 
                 aedat_mode(nsp2)%molwt = aedat(isp)%molwt
                 aedat_mode(nsp2)%lait = .True.
                 aedat_mode(nsp2)%lacc = .False.
                 aedat_mode(nsp2)%lcor = .False.
                 aedat_mode(nsp2)%optics_surr = aedat(isp)%optics_surr
                 aedat_mode(nsp2)%ic_surr     = aedat(isp)%ic_surr
                 aedat_mode(nsp2)%ic_fac      = aedat(isp)%ic_fac
                 aedat_mode(nsp2)%bc_surr     = aedat(isp)%bc_surr
                 aedat_mode(nsp2)%bc_fac      = aedat(isp)%bc_fac
                 aedat_mode(nsp2)%depv_fac    = aedat(isp)%depv_fac
                 aedat_mode(nsp2)%scav_fac    = aedat(isp)%scav_fac
                 aedat_mode(nsp2)%trns_flag   = aedat(isp)%trns_flag
                 aedat_mode(nsp2)%ddep_flag   = aedat(isp)%ddep_flag
                 aedat_mode(nsp2)%wdep_flag   = aedat(isp)%wdep_flag
                 aedat_mode(nsp2)%conc_flag   = aedat(isp)%conc_flag

              end if

              ! For Accumulation-Mode Particles
              if ( aedat(isp)%lacc ) then
                 if ( aedat(isp)%species_name .eq. 'NUM' .OR. 
     &                aedat(isp)%species_name .eq. 'SRF' ) THEN
                    ! Add Accumulation Mode Number or Surface Area
                    nsp2 = nsp2 + 1
                    aedat_mode(nsp2)%species_name = 
     &                    trim(aedat(isp)%species_name)//'ACC'
                    aedat_mode(nsp2)%depv_surr = 
     &                    trim(aedat(isp)%depv_surr)//'ACC'
                    aedat_mode(nsp2)%scav_surr = 
     &                    trim(aedat(isp)%scav_surr)//'_ACCUM'
                    aedat_mode(nsp2)%cloud_surr = 
     &                    trim(aedat(isp)%cloud_surr)//'_ACCUM'
                 else
                    ! Add Accumulation Mode Mass
                    nsp2 = nsp2 + 1
                    aedat_mode(nsp2)%species_name = 
     &                    trim(aedat(isp)%species_name)//'J'
                    aedat_mode(nsp2)%depv_surr = 
     &                    trim(aedat(isp)%depv_surr)//'J'
                    aedat_mode(nsp2)%scav_surr = 
     &                    trim(aedat(isp)%scav_surr)//'_ACCUM'
                    aedat_mode(nsp2)%cloud_surr = 
     &                    trim(aedat(isp)%cloud_surr)//'_ACCUM'
                 end if
 
                 aedat_mode(nsp2)%molwt = aedat(isp)%molwt
                 aedat_mode(nsp2)%lait = .False.
                 aedat_mode(nsp2)%lacc = .True.
                 aedat_mode(nsp2)%lcor = .False.
                 aedat_mode(nsp2)%optics_surr = aedat(isp)%optics_surr
                 aedat_mode(nsp2)%ic_surr     = aedat(isp)%ic_surr
                 aedat_mode(nsp2)%ic_fac      = aedat(isp)%ic_fac
                 aedat_mode(nsp2)%bc_surr     = aedat(isp)%bc_surr
                 aedat_mode(nsp2)%bc_fac      = aedat(isp)%bc_fac
                 aedat_mode(nsp2)%depv_fac    = aedat(isp)%depv_fac
                 aedat_mode(nsp2)%scav_fac    = aedat(isp)%scav_fac
                 aedat_mode(nsp2)%trns_flag   = aedat(isp)%trns_flag
                 aedat_mode(nsp2)%ddep_flag   = aedat(isp)%ddep_flag
                 aedat_mode(nsp2)%wdep_flag   = aedat(isp)%wdep_flag
                 aedat_mode(nsp2)%conc_flag   = aedat(isp)%conc_flag
                 
              end if

              ! For Coarse-Mode Particles
              if ( aedat(isp)%lcor ) then
                 if ( aedat(isp)%species_name .eq. 'NUM' .OR. 
     &                aedat(isp)%species_name .eq. 'SRF' ) THEN
                    ! Add Coarse Mode Number or Surface Area
                    nsp2 = nsp2 + 1
                    aedat_mode(nsp2)%species_name = 
     &                    trim(aedat(isp)%species_name)//'COR'
                    aedat_mode(nsp2)%depv_surr = 
     &                    trim(aedat(isp)%depv_surr)//'COR'
                    aedat_mode(nsp2)%scav_surr = 
     &                    trim(aedat(isp)%scav_surr)//'_COARSE'
                    aedat_mode(nsp2)%cloud_surr = 
     &                    trim(aedat(isp)%cloud_surr)//'_COARSE'

                 else
                    ! Add Coarse Mode Mass
                    nsp2 = nsp2 + 1
                    if ( aedat(isp)%species_name .eq. 'ACORS' .OR.
     &                    aedat(isp)%species_name .eq. 'ASOIL' .OR.
     &                    aedat(isp)%species_name .eq. 'ASEACAT' .OR.
     &                    aedat(isp)%species_name .eq. 'ADE_CORS' ) THEN
                       aedat_mode(nsp2)%species_name = 
     &                       trim(aedat(isp)%species_name)
                       aedat_mode(nsp2)%depv_surr = 
     &                       trim(aedat(isp)%depv_surr)//'C'
                       aedat_mode(nsp2)%scav_surr = 
     &                       trim(aedat(isp)%scav_surr)//'_COARSE'
                       aedat_mode(nsp2)%cloud_surr = 
     &                       trim(aedat(isp)%cloud_surr)//'_COARSE' 
                    else  
                       aedat_mode(nsp2)%species_name = 
     &                       trim(aedat(isp)%species_name)//'K'
                       aedat_mode(nsp2)%depv_surr = 
     &                       trim(aedat(isp)%depv_surr)//'C'
                       aedat_mode(nsp2)%scav_surr = 
     &                       trim(aedat(isp)%scav_surr)//'_COARSE'
                       aedat_mode(nsp2)%cloud_surr = 
     &                       trim(aedat(isp)%cloud_surr)//'_COARSE'
                    end if
                 end if

                 aedat_mode(nsp2)%molwt = aedat(isp)%molwt
                 aedat_mode(nsp2)%lait = .False.
                 aedat_mode(nsp2)%lacc = .False.
                 aedat_mode(nsp2)%lcor = .True.
                 aedat_mode(nsp2)%optics_surr = aedat(isp)%optics_surr
                 aedat_mode(nsp2)%ic_surr     = aedat(isp)%ic_surr
                 aedat_mode(nsp2)%ic_fac      = aedat(isp)%ic_fac
                 aedat_mode(nsp2)%bc_surr     = aedat(isp)%bc_surr
                 aedat_mode(nsp2)%bc_fac      = aedat(isp)%bc_fac
                 aedat_mode(nsp2)%depv_fac    = aedat(isp)%depv_fac
                 aedat_mode(nsp2)%scav_fac    = aedat(isp)%scav_fac
                 aedat_mode(nsp2)%trns_flag   = aedat(isp)%trns_flag
                 aedat_mode(nsp2)%ddep_flag   = aedat(isp)%ddep_flag
                 aedat_mode(nsp2)%wdep_flag   = aedat(isp)%wdep_flag
                 aedat_mode(nsp2)%conc_flag   = aedat(isp)%conc_flag
                 
              end if

          end do

          ae_species_data(1:nsp2) = aedat_mode(1:nsp2)

      End Subroutine process_aero_nml_modes
C----------------------------------------------------------------------- 
