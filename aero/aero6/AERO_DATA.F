
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/aero/aero6/AERO_DATA.F,v 1.1 2011/02/25 15:33:49 sjr Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module aero_data

C  Defines aerosol species arrays and the parameters required in aerosol
C  processing.
 
C  Contains:
C     Subroutine map_aero
C     Subroutine extract_aero
C     Subroutine update_aero
C     Function findAero
 
C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C     24 Jan 2011 Heather Simon: updates to initial AERO6; PMOTHR -> 9 species
C----------------------------------------------------------------------

      Implicit None

C Number of aerosol species and modes

      Integer, Parameter :: n_aerospc = 39      ! number of aero species
      Integer, Parameter :: n_mode = 3          ! number of modes:
                                                ! 1 = Aitken 
                                                ! 2 = accumulation 
                                                ! 3 = coarse 

C Default minimum concentration
      Real,    Parameter :: conmin = 1.0E-30    ! [ ug/m^3 ]
      Real,    Parameter :: conmin_set( n_mode )  = (/conmin, conmin, conmin/)
      Real,    Parameter :: conmin_so4( n_mode )  = (/1.0E-12,1.0E-6, conmin/)
      Real,    Parameter :: conmin_cor( n_mode )  = (/conmin, conmin, 1.889544E-05/)


C Flag to obtain coagulation coefficients
C by analytical approximation (True) or by Gauss-Hermite quadrature (False)
      Logical, Parameter :: fastcoag_flag = .True.

C-------------------------------------------------------------------------------------------------------

      Type spcs_type
         Character( 16 ) :: name( n_mode )     ! names of aerosol species for each mode
         Real            :: min_conc( n_mode ) ! minimum concentration values for each mode
         Real            :: density            ! density [ kg/m^3 ]
         Logical         :: no_M2Wet           ! flag to exclude from 2nd moment during transport
         Logical         :: nonVol_soa         ! non-volatile SOA flag
         Logical         :: tracer             ! tracer flag; does have not mass
         Integer         :: charge             ! electroneutrality charge
         Real            :: visual_idx         ! visual index factor
         Character( 16 ) :: optic_surr         ! optical surrogate name
         Character( 16 ) :: emis               ! emissions names
      End Type spcs_type

      Type( spcs_type ), Parameter :: aerospc( n_aerospc ) = (/
C                  -----------Name-------------- 
C                    Aitken    Accum    Coarse      Min_Concs   Density no_M2Wet NonVolSOA  Tracer Chrg Visual Opt_Surr  Emis
C                  --------- --------- ---------    ---------- -------- -------- -------- -------- ---- ----- --------- --------
     & spcs_type((/'ASO4I  ','ASO4J  ','ASO4K  '/), conmin_so4,  1800.0, .false., .false., .false.,  -2,  3.0, 'SOLUTE','PSO4'),
     & spcs_type((/'ANO3I  ','ANO3J  ','ANO3K  '/), conmin_set,  1800.0, .false., .false., .false.,  -1,  3.0, 'SOLUTE','PNO3'),
     & spcs_type((/'ACLI   ','ACLJ   ','ACLK   '/), conmin_set,  2200.0, .false., .false., .false.,  -1,  0.0, 'SOLUTE','PCL'),
     & spcs_type((/'ANH4I  ','ANH4J  ','ANH4K  '/), conmin_set,  1800.0, .false., .false., .false.,   1,  3.0, 'SOLUTE','PNH4'),
     & spcs_type((/'ANAI   ','ANAJ   ','ANAK   '/), conmin_set,  2200.0, .false., .false., .false.,   1,  0.0, 'SOLUTE','PNA'),
     & spcs_type((/'       ','ACAJ   ','       '/), conmin_set,  2200.0, .false., .false., .false.,   0,  1.0, 'DUST',  'PCA'),
     & spcs_type((/'       ','AMGJ   ','       '/), conmin_set,  2200.0, .false., .false., .false.,   0,  1.0, 'DUST',  'PMG'),
     & spcs_type((/'       ','AKJ    ','       '/), conmin_set,  2200.0, .false., .false., .false.,   0,  1.0, 'DUST',  'PK'),
     & spcs_type((/'APOCI  ','APOCJ  ','       '/), conmin_set,  2000.0, .false., .false., .false.,   0,  4.0, 'DUST',  'POC'),
     & spcs_type((/'APNCOMI','APNCOMJ','       '/), conmin_set,  2000.0, .false., .false., .false.,   0,  4.0, 'DUST',  'PNCOM'),
     & spcs_type((/'AECI   ','AECJ   ','       '/), conmin_set,  2200.0, .false., .false., .false.,   0, 10.0, 'SOOT',  'PEC'),
     & spcs_type((/'       ','AFEJ   ','       '/), conmin_set,  2200.0, .false., .false., .false.,   0,  1.0, 'DUST',  'PFE'),
     & spcs_type((/'       ','AALJ   ','       '/), conmin_set,  2200.0, .false., .false., .false.,   0,  1.0, 'DUST',  'PAL'),
     & spcs_type((/'       ','ASIJ   ','       '/), conmin_set,  2200.0, .false., .false., .false.,   0,  1.0, 'DUST',  'PSI'),
     & spcs_type((/'       ','ATIJ   ','       '/), conmin_set,  2200.0, .false., .false., .false.,   0,  1.0, 'DUST',  'PTI'),
     & spcs_type((/'       ','AMNJ   ','       '/), conmin_set,  2200.0, .false., .false., .false.,   0,  1.0, 'DUST',  'PMN'),
     & spcs_type((/'AH2OI  ','AH2OJ  ','AH2OK  '/), conmin_set,  1000.0,  .true., .false., .false.,   0,  0.0, 'WATER', 'PH2O'),
     & spcs_type((/'AOTHRI ','AOTHRJ ','       '/), conmin_set,  2200.0, .false., .false., .false.,   0,  1.0, 'DUST',  'PMOTHR'),
     & spcs_type((/'       ','AALKJ  ','       '/), conmin_set,  2000.0,  .true., .false., .false.,   0,  4.0, 'DUST',  ' '),
     & spcs_type((/'       ','AXYL1J ','       '/), conmin_set,  2000.0,  .true., .false., .false.,   0,  4.0, 'DUST',  ' '),
     & spcs_type((/'       ','AXYL2J ','       '/), conmin_set,  2000.0,  .true., .false., .false.,   0,  4.0, 'DUST',  ' '),
     & spcs_type((/'       ','AXYL3J ','       '/), conmin_set,  2000.0, .false.,  .true., .false.,   0,  4.0, 'DUST',  ' '),
     & spcs_type((/'       ','ATOL1J ','       '/), conmin_set,  2000.0,  .true., .false., .false.,   0,  4.0, 'DUST',  ' '),
     & spcs_type((/'       ','ATOL2J ','       '/), conmin_set,  2000.0,  .true., .false., .false.,   0,  4.0, 'DUST',  ' '),
     & spcs_type((/'       ','ATOL3J ','       '/), conmin_set,  2000.0, .false.,  .true., .false.,   0,  4.0, 'DUST',  ' '),
     & spcs_type((/'       ','ABNZ1J ','       '/), conmin_set,  2000.0,  .true., .false., .false.,   0,  4.0, 'DUST',  ' '),
     & spcs_type((/'       ','ABNZ2J ','       '/), conmin_set,  2000.0,  .true., .false., .false.,   0,  4.0, 'DUST',  ' '),
     & spcs_type((/'       ','ABNZ3J ','       '/), conmin_set,  2000.0, .false.,  .true., .false.,   0,  4.0, 'DUST',  ' '),
     & spcs_type((/'       ','ATRP1J ','       '/), conmin_set,  2000.0,  .true., .false., .false.,   0,  4.0, 'DUST',  ' '),
     & spcs_type((/'       ','ATRP2J ','       '/), conmin_set,  2000.0,  .true., .false., .false.,   0,  4.0, 'DUST',  ' '),
     & spcs_type((/'       ','AISO1J ','       '/), conmin_set,  2000.0,  .true., .false., .false.,   0,  4.0, 'DUST',  ' '),
     & spcs_type((/'       ','AISO2J ','       '/), conmin_set,  2000.0,  .true., .false., .false.,   0,  4.0, 'DUST',  ' '),
     & spcs_type((/'       ','AISO3J ','       '/), conmin_set,  2000.0, .false.,  .true., .false.,   0,  4.0, 'DUST',  ' '),
     & spcs_type((/'       ','ASQTJ  ','       '/), conmin_set,  2000.0,  .true., .false., .false.,   0,  4.0, 'DUST',  ' '),
     & spcs_type((/'       ','AOLGAJ ','       '/), conmin_set,  2000.0, .false.,  .true., .false.,   0,  4.0, 'DUST',  ' '),
     & spcs_type((/'       ','AOLGBJ ','       '/), conmin_set,  2000.0, .false.,  .true., .false.,   0,  4.0, 'DUST',  ' '),
     & spcs_type((/'       ','AORGCJ ','       '/), conmin_set,  2000.0, .false.,  .true., .false.,   0,  4.0, 'DUST',  ' '),
     & spcs_type((/'       ','       ','ASOIL  '/), conmin_set,  2600.0, .false., .false., .false.,   0,  0.0, 'DUST',  ' '),
     & spcs_type((/'       ','       ','ACORS  '/), conmin_cor,  2200.0, .false., .false., .false.,   0,  0.0, 'DUST',  'PMC')/)
     
C Required species
      Character( 16 ), Private, Parameter :: req_so4   = 'ASO4'  
      Character( 16 ), Private, Parameter :: req_nh4   = 'ANH4'  
      Character( 16 ), Private, Parameter :: req_no3   = 'ANO3'  
      Character( 16 ), Private, Parameter :: req_poc   = 'APOC'  
      Character( 16 ), Private, Parameter :: req_na    = 'ANA'   
      Character( 16 ), Private, Parameter :: req_cl    = 'ACL'   
      Character( 16 ), Private, Parameter :: req_h2o   = 'AH2O'  
      Character( 16 ), Private, Parameter :: req_soil  = 'ASOIL'
      Character( 16 ), Private, Parameter :: req_cors  = 'ACORS'  

C Indexes of required species
      Integer :: aso4_idx
      Integer :: anh4_idx
      Integer :: ano3_idx
      Integer :: apoc_idx
      Integer :: ana_idx
      Integer :: acl_idx
      Integer :: ah2o_idx 
      Integer :: asoil_idx
      Integer :: acors_idx

      Real :: aerospc_mw( n_aerospc )          ! aero species M.W. (from AE_SPC.EXT) [ g/mol ]
      Real :: aerospc_conc( n_aerospc,n_mode ) ! aero species concentration [ ug/m^3 ]

C Common factors
      Real( 8 ) :: h2ofac                      ! converts mass concentrations [ug/m3] to 3rd moment concentrations [m3/m3]

C-------------------------------------------------------------------------------------------------------

      Type mode_type
         Character( 16 ) :: num_name     ! name of aerosol number variable
         Character( 16 ) :: srf_name     ! name of aerosol surface area variable
         Real            :: min_numconc  ! minimum number concentration
         Real            :: min_m2conc   ! minimum 2nd moment concentration
         Real            :: min_m3conc   ! minimum 3rd moment concentration
      End Type mode_type

      Type ( mode_type ), Parameter  :: aeromode( n_mode ) = (/
C                   number     surface   minimum minimum minimum
C                    name       name     numconc  m2conc  m3conc
C                  ----------  -------  -------- -------  ------
     &   mode_type('NUMATKN', 'SRFATKN', conmin,  conmin, conmin),
     &   mode_type('NUMACC ', 'SRFACC ', conmin,  conmin, conmin),
     &   mode_type('NUMCOR ', 'SRFCOR ', conmin,  conmin, conmin)/)

      Real, Parameter :: min_sigma_g = 1.05
      Real, Parameter :: max_sigma_g = 2.50
      Real, Parameter :: def_sigma_g( n_mode ) = (/ 1.70, 2.0, 2.2 /)       ! default background sigma-g for each mode
      Real, Parameter :: def_diam( n_mode ) = (/ 1.0E-8, 7.0E-8, 1.0E-6 /)  ! default background mean diameter for each mode

      Real            :: moment0_conc( n_mode )     ! 0th moment concentration
      Real            :: moment2_conc( n_mode )     ! 2nd moment concentration
      Real            :: moment3_conc( n_mode )     ! 3rd moment concentration

C Mass concentration (calculated by GETPAR)
      Real :: aeromode_mass( n_mode )   ! [ ug/m^3 ]

C Particle density (calculated by GETPAR)
      Real :: aeromode_dens( n_mode )   ! [ kg/m^3 ]

C Geometric mean diameter (calculated by GETPAR)
      Real :: aeromode_diam( n_mode )   ! [ m ]

C Log of geometric standard deviation (calculated by GETPAR )
      Real :: aeromode_sdev( n_mode )

C Minimum number (calculated in map_aero routine)
      Real :: aeromode_minNum( n_mode )

C Minimum 2nd moment (calculated in map_aero routine)
      Real :: aeromode_minM2( n_mode )

C Mapping for loading from and unloading to CGRID array
      Integer :: aerospc_map( n_aerospc,n_mode )  ! indices of aero species to CGRID
      Integer :: aeronum_map( n_mode )      ! indices of aero number variable to CGRID
      Integer :: aerosrf_map( n_mode )      ! indices of aero surf area variable to CGRID

C Private variables for loading from and unloading to CGRID array
      Logical, Private, Save :: mapped = .False.
      Character( 16 ), Private, Save :: pname = 'Aero_Data'

      Contains

C-----------------------------------------------------------------------
      Subroutine map_aero()

C  Defines aerosol mapping from CGRID for species concentration and moments.
 
C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C-----------------------------------------------------------------------

      Use cgrid_spcs            ! CGRID mechanism species
      Use met_data

      Implicit None

C Includes:
      Include SUBST_IOPARMS   ! I/O parameters definitions

C External Functions:
      Integer index1

C Local Variables:
      Character( 80 ) :: xmsg
      Integer m, n, spc
      Real  so4fac
      Real  anthfac

      if ( .Not. mapped ) Then
         aerospc_mw = 0.0
         aerospc_map = 0

C build mapping to CGRID for each aero spc
         Do m = 1, n_mode
            Do spc = 1, n_aerospc
               If ( aerospc( spc )%name( m ) .Ne. ' ' ) Then
                  n = index1( aerospc( spc )%name( m ), n_ae_spc, ae_spc )
                  If ( n .Eq. 0 ) Then
                     xmsg = 'Species '// aerospc(spc)%name( m )
     &                    // ' in aerospc name is not in the AE_SPC table.'
                     Call m3exit( pname, 0, 0, xmsg, xstat3 )
                  End If
                  aerospc_map( spc,m ) = ae_strt - 1 + n
                  If ( aerospc_mw( spc ) .Lt. 0.5 ) Then  ! mw=0 means a new species
                     aerospc_mw( spc ) = ae_molwt( n )
                  Else If ( aerospc_mw( spc ) .Ne. ae_molwt( n ) ) Then
                     xmsg = 'molecular weight of ' // Trim( aerospc( spc )%name( m ) )
     &                    // ' is different from that of the same species'
     &                    // ' in the same or another mode.'
                   Call m3exit( pname, 0, 0, xmsg, xstat3 )
                   End If
               End If
            End Do
         End Do

C Build mapping to CGRID for aero # and surf area variables
         aeronum_map = 0
         aerosrf_map = 0

         Do m = 1, n_mode
            n = index1( aeromode( m )%num_name, n_ae_spc, ae_spc )
            If ( n .Eq. 0 ) Then
               xmsg = 'Species ' // Trim( aeromode( m )%num_name )
     &              //' in aeronum name is not in AE_SPC'
               Call m3exit( pname, 0, 0, xmsg, xstat3 )
            Else
               aeronum_map( m ) = ae_strt - 1 + n
            End If

            n = index1( aeromode( m )%srf_name, n_ae_spc, ae_spc )
            If ( n .Eq. 0 ) Then
               xmsg = 'species ' // Trim( aeromode( m )%srf_name )
     &              // ' in aerosrf name is not in AE_SPC'
               Call m3exit( pname, 0, 0, xmsg, xstat3 )
            Else
               aerosrf_map( m ) = ae_strt - 1 + n
            End If
         End Do

C Find indices of required species
         aso4_idx   = findAero( req_so4,   .True. )
         anh4_idx   = findAero( req_nh4,   .True. )
         ano3_idx   = findAero( req_no3,   .True. )
         apoc_idx   = findAero( req_poc,   .True. )
         ana_idx    = findAero( req_na,    .True. )
         acl_idx    = findAero( req_cl,    .True. )
         ah2o_idx   = findAero( req_h2o,   .True. )
         asoil_idx  = findAero( req_soil,  .True. )
         acors_idx  = findAero( req_cors,  .True. )
 
C Compute common factors
         h2ofac = 1.0D-9 * f6dpi / Real( aerospc( ah2o_idx )%density, 8 )

C compute aeromode_minNum and aeromode_minM2
         so4fac  = 1.0E-9 * f6dpi / aerospc( aso4_idx )%density
         anthfac = 1.0E-9 * f6dpi / aerospc( acors_idx )%density

         Do m = 1, n_mode
            if ( m .lt. n_mode ) Then
               aeromode_minNum( m ) = so4fac * aerospc( aso4_idx )%min_conc( m ) /
     &           ( def_diam( m )**3 * Exp( 4.5 * Log( def_sigma_g( m ) )**2 ) )
            else
               aeromode_minNum( m ) = anthfac * aerospc( acors_idx )%min_conc( m ) /
     &           ( def_diam( m )**3 * Exp( 4.5 * Log( def_sigma_g( m ) )**2 ) )
            endif
 
            aeromode_minM2( m ) = aeromode_minNum( m ) * def_diam( m )**2 *
     &            Exp( 2.0 * Log( def_sigma_g( m ) )**2 )
         End do
 
         mapped = .True.

      End If   ! Not mapped

      Return
      End Subroutine map_aero

C-----------------------------------------------------------------------
      Subroutine extract_aero( conc, minchk )

C  Extracts the required aerosol data from CGRID into conc.
 
C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C-----------------------------------------------------------------------

      Implicit None

C Arguments:
      Real, Intent( In ) :: conc( : )
      Logical, Intent( In ) :: minchk

C Local Variables:
      Character( 80 ) :: xmsg
      Integer m, n, spc 

      If ( .Not. mapped ) Then
         Call map_aero()
      End If

C copy grid cell concentrations of aero species to aerospc_conc
      aerospc_conc = 0.0
      If ( minchk ) Then
         Do m = 1, n_mode
            Do spc = 1, n_aerospc
               n = aerospc_map( spc,m )
               If ( n .ne. 0 ) Then
                  aerospc_conc( spc,m ) = Max( conc( n ), aerospc( spc )%min_conc( m ) ) ! ug/m^3
               End If
            End Do
         End Do
      Else
         Do m = 1, n_mode
            Do spc = 1, n_aerospc
               n = aerospc_map( spc,m )
               If ( n .ne. 0 ) Then
                  aerospc_conc( spc,m ) = conc( n )   ! ug/m^3
               End If
            End Do
         End Do
      End If

C copy grid cell concentrations of aero # and surf area
C convert and assign to moment0_conc and moment2_conc

      moment0_conc = 0.0
      moment2_conc = 0.0

      If ( minchk ) Then
         Do m = 1, n_mode
            n = aeronum_map( m )
            moment0_conc( m ) = Max( conc( n ), aeromode( m )%min_numconc )
            n = aerosrf_map( m )
            moment2_conc( m ) = Max( conc( n ), aeromode( m )%min_m2conc )
         End Do
      Else
         Do m = 1, n_mode
            n = aeronum_map( m )
            moment0_conc( m ) = conc( n )
            n = aerosrf_map( m )
            moment2_conc( m ) = conc( n )
         End Do
      End If

      Return
      End Subroutine extract_aero

C-----------------------------------------------------------------------
      Subroutine update_aero( conc, minchk )

C  Updates aerosol values in CGRID from the conc array.
 
C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C-----------------------------------------------------------------------

      Use met_data      !fundamental constants, data type definitions, etc.

      Implicit None

C arguments:
      Real, Intent( Out ) :: conc( : )
      Logical, Intent( In ) :: minchk

C includes:
      Include SUBST_IOPARMS   ! I/O parameters definitions

C local variables:
      Character( 80 ) :: xmsg
      Integer m, n, spc 

      If ( .Not. mapped ) Then
         xmsg = 'CGRID Species has not been mapped'
         Call m3exit( pname, 0, 0, xmsg, xstat3 )
      End If

C Copy aerospc_conc back to grid cell concentrations

      If ( minchk ) Then
         Do m = 1, n_mode
            Do spc = 1, n_aerospc
               n = aerospc_map( spc,m )
               If ( n .Ne. 0 ) Then
                    conc( n ) = Max( aerospc_conc( spc,m ), aerospc( spc )%min_conc( m ) )
               End If
            End Do
         End Do
      Else 
         Do m = 1, n_mode
            Do spc = 1, n_aerospc
               n = aerospc_map( spc,m )
               If ( n .Ne. 0 ) Then
                    conc( n ) = aerospc_conc( spc,m )
               End If
            End Do
         End Do
      End If

C Copy aero number and surface area back to grid cell concentrations

      If ( minchk ) Then
         Do m = 1, n_mode
            n = aeronum_map( m )
            conc( n ) = Max( moment0_conc( m ), aeromode( m )%min_numconc )
         End Do
      Else
         Do m = 1, n_mode
            n = aeronum_map( m )
            conc( n ) = moment0_conc( m )
         End Do
      End If

      Do m = 1, n_mode
         n = aerosrf_map( m )
         conc( n ) = pi * moment2_conc( m ) 
      End Do

      Return
      End Subroutine update_aero   

C-----------------------------------------------------------------------
      Integer Function findAero( vname, required ) Result ( idx )

C  Finds the index of 'required' aerosol species in the aerospc list
 
C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C-----------------------------------------------------------------------

      Implicit None

C Arguments:
      Character( * ) :: vname
      Logical :: required

C Includes:
      Include SUBST_IOPARMS   ! I/O parameters definitions

C Local Variables:
      Character( 80 ) :: xmsg
      Integer spc, n

      Do n = 1, n_mode
         Do spc = 1, n_aerospc
            If ( Index( aerospc( spc )%name( n ), Trim( vname ) ) .Gt. 0 ) Then
               idx = spc
               Return
            End If
         End Do
      End Do

      If ( .Not. required ) Then
         idx = 0
         Return
      End If

      xmsg = 'Required Species ' // Trim( vname ) // ' Not found in aerospc names array'
      Call m3exit( pname, 0, 0, xmsg, xstat3 )

      Return
      End Function findAero

      End Module aero_data

