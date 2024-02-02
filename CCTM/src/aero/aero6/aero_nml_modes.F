
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
      Subroutine aero_nml_modes
          
C         This function expands the AE namelist rows to account for 
C         mode-specific variables. If just one mode is requested for an 
C         aerosol species, then no rows are added. If more than one mode
C         is requested, then rows are added to accomodate those. 
C
C         Ben Murphy 2022-03-25
C-----------------------------------------------------------------------
          Use aero_data, only : n_aerolist, aerolist, modesuff,
     &                          iait, iacc, icor, reqd_modes
          Use runtime_vars, only : log_message, log_subheading, logdev,
     &                             outdev, cmype
          Use cgrid_spcs_types
          Use utilio_defn, only : index1, xstat2, m3exit, m3warn, m3mesg

          Implicit None

          Type(species_data_reg_2) :: aedat( size( ae_species_data,1 ) )
          Type(species_data_reg_2) :: aedat_mode( size( ae_species_data,1 ) )

          integer nsp2, naero

          Integer isp, im, iaer, iost
          Character(300) xmsg
          Character(20) :: pname = 'aero_nml_modes'
          Logical eflag
                    
          aedat = ae_species_data
          aedat_mode = aedat
          nsp2 = 0

          call log_message( LOGDEV,"")
          Call log_subheading( LOGDEV, "Add Modal Dimension to CMAQ Aerosol Species" )
          call log_message( LOGDEV,"")

          eflag = .False.
          do isp = 1,size( aedat,1 )
              ! Stop after the last aerosol name
              if ( aedat(isp)%species_name .EQ. '' ) EXIT

              ! Get Index on Aerosol Table
              iaer = index1( aedat(isp)%species_name, 
     &                       n_aerolist, aerolist(:)%bulkname )

              ! Confirm species is known to CMAQ either in the Aerosol
              ! Table or Particle Number or Surface Area
              if ( iaer .eq. 0 ) then
                 if ( trim(aedat(isp)%species_name) .eq. 'NUM' .or.
     &                trim(aedat(isp)%species_name) .eq. 'SRF'     ) then
                   ! Species is a Number or Surface Area Concentration
                   if ( aedat(isp)%lait .neqv. reqd_modes(iait) ) then
                      xmsg = 'Number or Surface Area must have an Aitken-Mode ' //
     &                       'component. Please adjust the aerosol namelist.'
                      Call log_message( OUTDEV, 'Error: '//xmsg )
                      eflag = .True.
                   end if

                   if ( aedat(isp)%lacc .neqv. reqd_modes(iacc) ) then
                      xmsg = 'Number or Surface Area must have an Accumulation-Mode ' //
     &                       'component. Please adjust the aerosol namelist.'
                      Call log_message( OUTDEV, 'Error: '//xmsg )
                      eflag = .True.
                   end if

                   if ( aedat(isp)%lcor .neqv. reqd_modes(icor) ) then
                      xmsg = 'Number or Surface Area must have a Coarse-Mode ' //
     &                       'component. Please adjust the aerosol namelist.'
                      Call log_message( OUTDEV, 'Error: '//xmsg )
                      eflag = .True.
                   end if

                 else
                   ! Species is not a recognized aerosol chemical species or aerosol
                   ! parameter
                   xmsg = 'An aerosol chemical species appears on the AE '//
     &                     'namelist that is not available in AERO_DATA. Please '//
     &                     'see the aerolist table.'
                   Call log_message( OUTDEV, 'Error: '//xmsg )
                   eflag = .True.
                 end if
              else

                  ! Species is a recognized aerosol chemical species
                   if ( aedat(isp)%lait .neqv. aerolist(iaer)%lait ) then
                      xmsg = 'Aitken Flag for '//
     &                       TRIM(aedat(isp)%species_name)//' set incorrectly in the AE namelist.'
                      xmsg = Trim( pname ) // ': ' // xmsg
                      call m3mesg( xmsg )
                      eflag = .True.
                   end if

                   if ( aedat(isp)%lacc .neqv. aerolist(iaer)%lacc ) then
                      xmsg = 'Accumulation Flag for '//
     &                       TRIM(aedat(isp)%species_name)//' set incorrectly in the AE namelist.'
                      xmsg = Trim( pname ) // ': ' // xmsg
                      call m3mesg( xmsg )
                      eflag = .True.
                   end if

                   if ( aedat(isp)%lcor .neqv. aerolist(iaer)%lcor ) then
                      xmsg = 'Coarse Flag for '//
     &                       TRIM(aedat(isp)%species_name)//' set incorrectly in the AE namelist.'
                      xmsg = Trim( pname ) // ': ' // xmsg
                      call m3mesg( xmsg )
                      eflag = .True.
                   end if
              end if

              if( eflag )then
                  xmsg = 'AE Namelist has fatal errors. Check Processor log '
     &                 // Trim( cmype )
                  Call log_message( outdev, 'Error: '//xmsg )
                  xmsg = Trim( pname ) // ': ' // xmsg
                  call m3mesg( xmsg )
                  call m3exit( pname, 0, 0, 'FATAL ERROR', xstat2 )
              end if

              ! If no modes are selected, warn and move on
              if ( .not.aedat(isp)%lait .and. 
     %             .not.aedat(isp)%lacc .and. 
     &             .not.aedat(isp)%lcor ) then
                    xmsg = 'No aerosol modes have been requested for '//
     &                     TRIM(aedat(isp)%species_name)//' on the AE namelist.' 
                    call m3warn( pname, 0, 0, xmsg ) 
              end if

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
                 elseif ( aerolist(iaer)%lait ) then
                    ! Add Aitken Mode Mass
                    nsp2 = nsp2 + 1
                    aedat_mode(nsp2)%species_name = 
     &                    trim(aedat(isp)%species_name)//modesuff(iait)
                    aedat_mode(nsp2)%depv_surr = aedat(isp)%depv_surr
                    if ( aedat(isp)%depv_surr .ne. '' )
     &                   aedat_mode(nsp2)%depv_surr = 
     &                         trim(aedat(isp)%depv_surr)//'I'
                    aedat_mode(nsp2)%scav_surr = aedat(isp)%scav_surr
                    if ( aedat(isp)%scav_surr .ne. '' )
     &                   aedat_mode(nsp2)%scav_surr = 
     &                         trim(aedat(isp)%scav_surr)//'_AITKEN'
                    aedat_mode(nsp2)%cloud_surr = aedat(isp)%cloud_surr
                    if ( aedat(isp)%cloud_surr .ne. '' )
     &                   aedat_mode(nsp2)%cloud_surr = 
     &                         trim(aedat(isp)%cloud_surr)//'_AITKEN'
                 else
                    ! Mode not allowed for this species. Exit CMAQ
                    xmsg = 'Aitken-mode aerosols have been requested for '//
     &                     TRIM(aedat(isp)%species_name)//
     &                     ' but they are not available for this species. See '//
     &                     'aerolist table in AERO_DATA.'
                    Call log_message( outdev, 'Error: '//xmsg )
                    call m3exit( pname, 0, 0, xmsg, xstat2 ) 
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

                 xmsg = 'Added an Aitken Mode for Aerosol Species '//
     &                  trim(aedat_mode(nsp2)%species_name)
                 Call log_message( LOGDEV, xmsg )
 
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
                 elseif ( aerolist(iaer)%lacc ) then
                    ! Add Accumulation Mode Mass
                    nsp2 = nsp2 + 1
                    aedat_mode(nsp2)%species_name = 
     &                    trim(aedat(isp)%species_name)//modesuff(iacc)
                    aedat_mode(nsp2)%depv_surr = aedat(isp)%depv_surr
                    if ( aedat(isp)%depv_surr .ne. '' )
     &                   aedat_mode(nsp2)%depv_surr = 
     &                         trim(aedat(isp)%depv_surr)//'J'
                    aedat_mode(nsp2)%scav_surr = aedat(isp)%scav_surr
                    if ( aedat(isp)%scav_surr .ne. '' )
     &                   aedat_mode(nsp2)%scav_surr = 
     &                         trim(aedat(isp)%scav_surr)//'_ACCUM'
                    aedat_mode(nsp2)%cloud_surr = aedat(isp)%cloud_surr
                    if ( aedat(isp)%cloud_surr .ne. '' )
     &                   aedat_mode(nsp2)%cloud_surr = 
     &                         trim(aedat(isp)%cloud_surr)//'_ACCUM'
                 else
                    ! Mode not allowed for this species. Exit CMAQ
                    xmsg = 'Accumulation-mode aerosols have been requested for '//
     &                     TRIM(aedat(isp)%species_name)//
     &                     ' but they are not available for this species. See '//
     &                     'aerolist table in AERO_DATA.'
                    Call log_message( outdev, 'Error: '//xmsg )
                    call m3exit( pname, 0, 0, xmsg, xstat2 ) 
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
                 
                 xmsg = 'Added an Accumulation Mode for Aerosol Species '//
     &                  trim(aedat_mode(nsp2)%species_name)
                 Call log_message( LOGDEV, xmsg )
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

                 elseif ( aerolist(iaer)%lcor ) then
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
     &                       trim(aedat(isp)%species_name)//modesuff(icor)
                       aedat_mode(nsp2)%depv_surr = aedat(isp)%depv_surr
                       if ( aedat(isp)%depv_surr .ne. '' )
     &                      aedat_mode(nsp2)%depv_surr = 
     &                            trim(aedat(isp)%depv_surr)//'C'
                       aedat_mode(nsp2)%scav_surr = aedat(isp)%scav_surr
                       if ( aedat(isp)%scav_surr .ne. '' )
     &                      aedat_mode(nsp2)%scav_surr = 
     &                            trim(aedat(isp)%scav_surr)//'_COARSE'
                       aedat_mode(nsp2)%cloud_surr = aedat(isp)%cloud_surr
                       if ( aedat(isp)%cloud_surr .ne. '' )
     &                      aedat_mode(nsp2)%cloud_surr = 
     &                            trim(aedat(isp)%cloud_surr)//'_COARSE'
                    end if
                 else
                    ! Mode not allowed for this species. Exit CMAQ
                    xmsg = 'Coarse-mode aerosols have been requested for '//
     &                     TRIM(aedat(isp)%species_name)//
     &                     ' but they are not available for this species. See '//
     &                     'aerolist table in AERO_DATA.'
                    Call log_message( outdev, 'Error: '//xmsg )
                    call m3exit( pname, 0, 0, xmsg, xstat2 ) 
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
                 
                 xmsg = 'Added a Coarse Mode for Aerosol Species '//
     &                  trim(aedat_mode(nsp2)%species_name)
                 Call log_message( LOGDEV, xmsg )
 
              end if

          end do

          write( xmsg, '(A,I3)' ), 'Total number of aerosol species: ',nsp2
          Call log_message( LOGDEV, '' )
          Call log_message( LOGDEV, xmsg )
          Call log_message( LOGDEV, '' )

          ae_species_data(1:nsp2) = aedat_mode(1:nsp2)

      End Subroutine aero_nml_modes
C----------------------------------------------------------------------- 
