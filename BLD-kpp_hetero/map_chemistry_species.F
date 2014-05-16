         function map_chemistry_species() result ( success )

            USE UTILIO_DEFN
            USE CGRID_SPCS
            USE RXNS_DATA
            
            implicit none

            integer, save :: logdev
            
            logical success
            integer i, ios, j
            integer i1, i2, i3, i4      ! surrogate type 1 counters
            integer j1, j2              ! surrogate type 2 counters
            integer k1, k2, k3, k4, k5  ! control type counters
            integer icall
            character(  1 ), parameter :: bl = ' '
            integer, parameter :: spc_dim = 200
            logical :: order = .true., found = .true.
            character( 120 ) :: xmsg

            character( 16 ), allocatable     :: nml_spc  ( : )
            character(  2 ), allocatable     :: nml_type ( : )
            integer,         allocatable     :: nml_index( : )

            logdev = junit ()
            success = .true.
            
            nspcsd = n_gc_spcd + n_ae_spc + n_nr_spc + n_tr_spc
            
            allocate ( cgrid_spc( nspcsd - 1 ), nml_spc( nspcsd - 1 ), 
     &                 nml_index( nspcsd - 1 ), nml_type( nspcsd - 1 ), 
     &                 type_index( nspcsd - 1 ), stat = ios )
           
            allocate ( species_molwt( nspcsd - 1 ), stat = ios )

            j = 0
                       
            cgrid_spc     = ' '
            nml_index     = -1
            type_index    = -1
            nml_type      = ' '
            species_molwt = -1.0
            
            do i = 1, n_gc_spc ! load GC names and indices 
               j = j + 1
               cgrid_spc( i )   = gc_spc( i )
               nml_index( j )  = i + gc_strt -1
               type_index( j ) = i
               nml_type( j )   = 'GC'
               species_molwt( j ) = gc_molwt( i )
            end do  

            do i = 1, n_ae_spc ! load AE names and indices 
               j = j + 1
               cgrid_spc( j )    = ae_spc( i )
               nml_index( j )    = i + ae_strt - 1
               nml_type( j )     = 'AE'
               type_index( j )   = i
               species_molwt( j ) = ae_molwt( i )
            end do  

            do i = 1, n_nr_spc ! load NR names and indices 
               j = j + 1
               cgrid_spc( j )   = nr_spc( i )
               nml_index( j )   = i + nr_strt - 1
               nml_type( j )    = 'NR'
               type_index( j )  = i
               species_molwt( j ) = nr_molwt( i )
            end do  

            do i = 1, n_tr_spc ! load TR names and indices 
               j = j + 1
               cgrid_spc( j )   = tr_spc( i )
               nml_index( j )   = i + tr_strt - 1
               nml_type( j )    = 'TR'
               type_index( j )   = i
               species_molwt( j ) = tr_molwt( i )
            end do  

            nml_spc( 1:(nspcsd-1) ) = cgrid_spc( 1:(nspcsd-1) )
            
            
! determine if mechanism species are in cgrid species            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
            do i = 1, numb_mech_spcs
               i1 = index1a( mechanism_spc( i ), nspcsd, cgrid_spc )
               if ( i1 .lt. 1 ) then
                  if ( found ) then
                     found = .false.
                     xmsg = '*** Fatal error(s) found not in species namelist -'
     &                    // ' missing:'
                     write( logdev,'( /5x, a )' ) trim( xmsg )
                  end if
                  xmsg = trim( mechanism_spc( i ) )
                  write( logdev,'( 9x, i4, 2x, a )' ) i, trim( xmsg )
               else
                  cgrid_index( i )  = nml_index( i1 )
                  species_type( i ) = nml_type( i1 )
               end if
               if( index( mechanism_spc( i ), 'SRF') .gt. 0 )then
                   found = .false.
                   xmsg = '*** reactions cannot use modal aerosol surface area as species'
                   write( logdev,'( /5x, a )' ) trim( xmsg )
                   xmsg = trim( mechanism_spc( i ) )
                   write( logdev,'( 9x, i4, 2x, a )' ) i, trim( xmsg )
               end if
               if( index( mechanism_spc( i ), 'NUM') .gt. 0 )then
                   found = .false.
                   xmsg = '*** reactions cannot use modal aerosol number density as species'
                   write( logdev,'( /5x, a )' ) trim( xmsg )
                   xmsg = trim( mechanism_spc( i ) )
                   write( logdev,'( 9x, i4, 2x, a )' ) i, trim( xmsg )
               end if
            end do
            success = found 

         return

         end function map_chemistry_species
!----------------------------------------------------------------------------------------
         integer function index1a ( name, n, nlist )
            implicit none
            character( * ) name        ! character string being searched for
            integer n                  ! length of array to be searched
            character( * ) nlist( : )  ! array to be searched

            integer i

            do i = 1, n
               if ( name .eq. nlist( i ) ) then
                  index1a = i
                  return
               end if
           end do
           index1a = 0
           return

          end function index1a
