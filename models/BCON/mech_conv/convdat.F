
      program convdat
C generate profile.dat files for various gas chem mechanisms
C from the RADM2 mechanism .dat files
C purpose: eliminate the 3 or 4 gc convert routines from icon and bcon

      include './mech_table.ext'

      implicit none

      integer :: n_mcnv_sp( nsptab )          ! no. of mech input species for each mech output species
      real    :: mcnv_coeff( nsptab,mxcvsp )  ! mech conv species coefficients
      character( 16 ) :: mcnv_spcs( nsptab,mxcvsp ) 

      real,    allocatable :: inprof( :,:,: )  ! input profile data
      real,    allocatable :: outprof( :,:,: ) ! output profile data for GC-converted species
      real,    allocatable :: siglev( : )     ! number of sigma levels for input & output profile
      logical, allocatable :: lconvsp( : )    ! gas species convert flag
      integer, allocatable :: spmap( : )      ! gas species convert index
      integer, allocatable :: line_found( : ) ! profile line found to convert for table species
      logical :: found( nsptab )

      integer idx                           ! List index
      integer i, i1, i2, i3, j, k, n, nr, q ! Loop indices
      integer nspcs_in, nspcs_out
      integer n_gc_out
      integer n_gc_radm2              ! number of profile lines (species) found to convert for table species
      integer nlays_in
      integer ios
      integer :: in0 = 21
      integer :: out0 = 31

      character( 128 ) :: line2 = " bcon profile data converted from RADM2 gas mechanism species"
      character( 128 ) :: prof_fl_name = "/home/yoj/src/bcon/mech_conv/bc_profile_RADM2_AERO5.dat"
      character( 128 ) :: linein
      character(  16 ), allocatable :: mechfl_sp_nf( : )   ! radm2 species not found in the input file
      character(  16 ), allocatable :: prof_sp_name( : )
      character(  16 ), allocatable :: spname_out( : )
      character(  16 ) :: blank = '                '
      character(  10 ) :: date = '          '
      character(   6 ) :: bndy( 4 ) = (/ '      ', '      ', '      ', '      '/)

C Open input profile file
      open( unit = in0, file = prof_fl_name , status = "old", iostat = ios )
      if ( ios .ne. 0 ) then
         write( *,* ) '*** error opening ', trim( prof_fl_name )
         stop
      end if

C Open output profile file
      open( unit = out0, file = out_fl_name , status = "new", iostat = ios )
      if ( ios .ne. 0 ) then
         write( *,* ) '*** error opening ', trim( out_fl_name )
         stop
      end if

      do n = 1, 3
         read( in0,'(a)' ) linein
         write( *,'(5x, a)' ) trim( linein )
      end do
      write( out0,'(a)' ) trim( line1 )
      write( out0,'(a)' ) trim( line2 )
      write( out0,'(a)' ) trim( linein )   ! last line from input header

      read( in0,* ) nlays_in
      allocate( siglev( nlays_in+1 ), stat = ios )
      if ( ios .ne. 0 ) then
         write( *,* ) '*** error allocating siglev'
         stop
      end if
      backspace in0
      read( in0,* ) nlays_in, nspcs_in, ( siglev( i ), i = 1, nlays_in+1 )
      write( *,'(2i7, 1x, 7f6.2)' ) nlays_in, nspcs_in, ( siglev( i ), i = 1, nlays_in+1 )

      read( in0, '(a)' ) linein
      write( *,'(5x, a)' ) trim( linein )

      allocate( prof_sp_name( nspcs_in ),
     &          inprof( nlays_in,nspcs_in,4 ), stat = ios )
      if ( ios .ne. 0 ) then
         write( *,* ) '*** error allocating prof_sp_name or inprof'
         stop
      end if

      do q = 1, 4   ! boundary
         read( in0,'(a)' ) linein
         write( bndy( q ),'(a)' ) trim( linein )
         write( *,'(4x, i3, 2x, a)' ) q, bndy( q )
         do i = 1, nspcs_in
            read( in0,'( a )' ) linein
            write( *,'(2x, i3, 2x, a)' ) i, trim( linein )
            backspace in0
            read( in0,* ) prof_sp_name( i ), ( inprof( k,i,q ), k = 1, nlays_in )
         end do
      end do

      write( *,'(5x, a)' ) 'End of file contents'

C count the number of repeated RADM2 species
      nr = 0
      do i = 1, nsptab
         if ( spcmap( i )%n_radm2_spc .eq. 1 ) then
            do j = i+1, nsptab
               if ( spcmap( j )%radm2_spc( 1 ) .eq. spcmap( i )%radm2_spc( 1 ) ) then
                  nr = nr + 1
!                 write( *,'(3i4,2x,a,2x,a)' ) i, j, n, spcmap( i )%radm2_spc( 1 ), spcmap( j )%radm2_spc( 1 )
               end if
            end do
         end if
      end do
      write( *,'(/3x, a, i3)' ) 'number of repeated RADM2 species required:', nr

      allocate( mechfl_sp_nf( nspcs_in ),
     &          line_found  ( nspcs_in ),
     &          lconvsp     ( nspcs_in+nr ),
     &          spname_out  ( nspcs_in+nr ),
     &          spmap       ( nspcs_in+nr ), stat = ios )
      if ( ios .ne. 0 ) then
         write( *,* ) 'error allocating mechfl_sp_nf, line_found, lconvsp, spname_out, or spmap'
         stop
      end if
      line_found = 0     ! array assignment
      lconvsp = .false.  ! array assignment
      spname_out = blank ! array assignment
      spmap = 0          ! array assignment

C RADM2 species not in the input file
      found = .false.   ! array assignment
      n = 0
      do i = 1, nsptab
         found( i ) = .true.
         do j = 1, spcmap( i )%n_radm2_spc
            idx = index1( spcmap( i )%radm2_spc( j ), nspcs_in, prof_sp_name )
            if ( idx .eq. 0 ) then
               found( i ) = .false.
               n = n + 1
               mechfl_sp_nf( n ) = spcmap( i )%radm2_spc( j )
            end if
         end do
      end do
      if ( n .ne. 0 ) then
         write( *,92060 ) n
92060    format( /3x, 'WARNING: The following', i4, ' table species were ',
     &                'not found in the input profile file.'
     &           /3X, '         No conversion was generated for the ',
     &                'output profile file.' )
         write( *,92040 ) ( mechfl_sp_nf( j ), j = 1, n )
92040    format( ( 15x, 4( a, 2x ) ) )
      end if

      n = 0; k = 0; i1 = 0; i2 = 0; i3 = 0
      do i = 1, nsptab
         if ( found( i ) ) then
            n = n + 1
            spname_out( n ) = spcmap( i )%tab_spc
            n_mcnv_sp( n )  = spcmap( i )%n_radm2_spc
            do j = 1, n_mcnv_sp( n )
               mcnv_spcs( n,j )  = spcmap( i )%radm2_spc( j )
               mcnv_coeff( n,j ) = spcmap( i )%coeff( j )
            end do
!           if ( n_mcnv_sp( n ) .eq. 1 .and. mcnv_coeff( n,1 ) .eq. 1.0 ) then
            if ( n_mcnv_sp( n ) .eq. 1 ) then
               if ( mcnv_coeff( n,1 ) .eq. 1.0 ) then
                  idx = index1( mcnv_spcs( n,1 ), nspcs_in, prof_sp_name )
                  i1 = i1 + 1
                  line_found( i1+i2+i3 ) = idx
#ifdef debug
                  write( *,'(i7, 2i5, i3, 2x, 2a)' ) i, n, i1+i2+i3, line_found( i1+i2 ), 'a ', prof_sp_name( idx )
#endif
                  spmap( n )   = idx
                  lconvsp( n ) = .false.   ! requires just a 1-to-1 mapping
               else
                  idx = index1( mcnv_spcs( n,1 ), nspcs_in, prof_sp_name )
                  i2 = i2 + 1
                  line_found( i1+i2+i3 ) = idx
#ifdef debug
                  write( *,'(i7, 2i5, i3, 2x, 2a)' ) i, n, i1+i2+i3, line_found( i1+i2 ), 'b ', prof_sp_name( idx )
#endif
                  spmap( n )   = 0
                  lconvsp( n ) = .true.    ! requires the mcnv_coeff; 1 radm2 species to mutltple table species
               end if
            else
               do j = 1, n_mcnv_sp( n )
                  idx = index1( mcnv_spcs( n,j ), nspcs_in, prof_sp_name )
                  i3 = i3 + 1
                  line_found( i1+i2+i3 ) = idx
#ifdef debug
                  write( *,'(i7, 2i5, i3, 2x, 2a)' ) i, n, i1+i2+i3, line_found( i1+i2 ), 'c ', prof_sp_name( idx )
#endif
               end do
               spmap( n )   = 0
               lconvsp( n ) = .true.    ! requires the mcnv_coeff; multiple radm2 species to 1 table species
            end if
         else
         k = k + 1   ! table species not found
         end if
!        write( *,'(i7, i5, i3)' ) i, i1+i2, line_found( i1+i2 )
      end do
      n_gc_radm2 = i1 + i2 + i3
      n_gc_out = n

!     do i = 1, nspcs_in
!        write( *,'(i7, i3)' ) i, line_found( i )
!     end do

      n = n_gc_out
      outer: do i = 1, nspcs_in
         do j = 1, n_gc_radm2
            if ( i .eq. line_found( j ) ) cycle outer
         end do
         n = n + 1
         spname_out( n ) = prof_sp_name( i )
#ifdef debug
         write( *,'(i7, i5, 3x, a)' ) i, n, spname_out( n )
#endif
      end do outer
      nspcs_out = n

      allocate ( outprof( nlays_in,nspcs_out,4 ), stat = ios )
      if ( ios .ne. 0 ) then
         write( *,* ) 'error allocating outprof'
         stop
      end if

      write( *,'(/3x, a, 5i5/)' ) 'nsptab, n_gc_out, n_gc_radm2, nspcs_in, nspcs_out:',
     &                             nsptab, n_gc_out, n_gc_radm2, nspcs_in, nspcs_out 

      write( out0,'(2i5, 1x, 7f6.2)' ) nlays_in, nspcs_out, ( siglev( i ), i = 1, nlays_in+1 )
      call wrdate( date )
!     write( out0, '(a)' ) ' 2011215 00'
      write( out0, '(1x, a)' ) date

!     do i = n + 1, nspcs_in
!        lconvsp( i ) = .false.  ! don't convert the non GC species
!     end do

      do q = 1, 4
         write( *,'(3x, a)' ) bndy( q ) 
         write( out0,'(1x, a)' ) bndy( q ) 
         do i = 1, nspcs_out
!           write( *,* ) 'spmap,lconvsp:', i, spmap( i ), lconvsp( i )
            if ( lconvsp( i ) ) then
               idx = index1( mcnv_spcs( i,1 ), nspcs_in, prof_sp_name )
#ifdef debug
               write( *,* ) spname_out( i ), prof_sp_name( idx ), idx
#endif
               do k = 1, nlays_in
                  outprof( k,i,q ) = mcnv_coeff( i,1 ) * inprof( k,idx,q )
               end do
               if ( n_mcnv_sp( i ) .gt. 1 ) then
                  do j = 2, n_mcnv_sp( i )
                     idx = index1( mcnv_spcs( i,j ), nspcs_in, prof_sp_name )
#ifdef debug
                     write( *,* ) spname_out( i ), prof_sp_name( idx ), idx
#endif
                     do k = 1, nlays_in
                        outprof( k,i,q ) = outprof( k,i,q ) + mcnv_coeff( i,j ) * inprof( k,idx,q )
                        end do
                  end do
               end if
            else
               if ( spmap( i ) .ne. 0 ) then
                  idx = spmap( i )
               else
                  idx = index1( spname_out( i ), nspcs_in, prof_sp_name )
               end if
            
               do k = 1, nlays_in
                  outprof( k,i,q ) = inprof( k,idx,q )
               end do
            end if

            write( *,'(i3, 2x, 3a, 6(1pe10.3))' ) i, '"', spname_out( i ), '"', ( outprof( k,i,q ), k = 1, nlays_in )
            write( out0,'(3a, 6(1pe10.3))' ) '"', spname_out( i ), '"', ( outprof( k,i,q ), k = 1, nlays_in )

         end do

      end do

      stop

C-----------------------------------------------------------------------
      contains

         integer function index1( item, nlist, list )
         implicit none
         character( * ) item
         integer nlist
         character( * ) :: list( nlist )
         integer i

         do i = 1, nlist
            if ( item .eq. list( i ) ) then
               index1 = i
               return
            end if
         end do

         index1 = 0
         return

         end function index1

      end program convdat
