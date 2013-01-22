
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

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/aero/aero6/AERO_DATA.F,v 1.10 2012/01/19 13:18:37 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module aero_data

C  Defines aerosol species arrays and the parameters required in aerosol
C  processing.
 
C  Contains:
C     Subroutine map_aero
C     Subroutine map_pmemis
C     Subroutine extract_aero
C     Subroutine update_aero
C     Function findAero
 
C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.

C HS  01/24/11 Updated to initial AERO6; PMOTHR -> 9 species
C GS  03/02/11 Revised parameters for Mg, K, and Ca
C SH  03/10/11 Inserted functionality of PMEM_DEFN
C    -new subroutine map_pmemis
C HS  03/10/11 Made PNCOM a required species
C SR  03/25/11 Replaced I/O API include files with UTILIO_DEFN
C SH  04/04/11 Added sea-salt speciation factors
C GS  04/09/11 Updated sea-salt speciation; replaced ANAK with ASEACAT;
C              made MG, K, CA, SEACAT required species;
C JY  04/21/11 Added optional log messages (verbose)
C JY  05/02/11 Added Reshape to aerospc_ssf initialization for pgf90 compiler
C JY  06/08/12 remove full character blank padding put in for GNU Fortran (GCC) 4.1.2
C HOTP 01/16/13 removed isoprene acid enhanced aerosol
C HP  08/26/12 Added isoprene derived products
C----------------------------------------------------------------------

      Implicit None

C Number of aerosol species and modes

      Integer, Parameter :: n_aerospc = 41      ! number of aero species
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

C Define Logical values as T and F for the aerospc table
      Logical, Parameter, Private :: T = .true.
      Logical, Parameter, Private :: F = .false.

      Integer, Private, Save :: logdev
      Integer, Private, External :: setup_logdev

C-------------------------------------------------------------------------------------------------------

      Type spcs_type
         Character( 16 ) :: name( n_mode )       ! names of aerosol species for each mode
         Real            :: min_conc( n_mode )   ! minimum concentration values for each mode
         Real            :: density              ! density [ kg/m^3 ]
         Logical         :: no_M2Wet             ! flag to exclude from 2nd moment during transport
         Logical         :: nonVol_soa           ! non-volatile SOA flag
         Logical         :: tracer               ! tracer flag; does have not mass
         Integer         :: charge               ! electroneutrality charge
         Real            :: visual_idx           ! visual index factor
         Character( 16 ) :: optic_surr           ! optical surrogate name
         Character( 16 ) :: emis                 ! file emissions names
         Real            :: emis_split( n_mode ) ! minimum concentration values for each mode
      End Type spcs_type

      Type( spcs_type ), Parameter :: aerospc( n_aerospc ) = (/
C                                                                     nonVolSOA
C                                                                          |   Charge                            Emissions
C                  -----------Name--------------                   NoM2Wet |Tracer|                         ----Split Factor----
C                   Aitken    Accum     Coarse     Min_Concs   Density  |  |  |   | Visidx OptSurr  Emis_1   Aitken Accum Coarse
C                  --------- --------- ---------   ---------- -------   +  +  +   + ----- -------- -------- ------ ------ ------
     & spcs_type((/'ASO4I  ','ASO4J  ','ASO4K  '/), conmin_so4, 1800.0, F, F, F, -2,  3.0,'SOLUTE','PSO4  ',(/0.000,1.000,0.000/)),
     & spcs_type((/'ANO3I  ','ANO3J  ','ANO3K  '/), conmin_set, 1800.0, F, F, F, -1,  3.0,'SOLUTE','PNO3  ',(/0.000,1.000,0.000/)),
     & spcs_type((/'ACLI   ','ACLJ   ','ACLK   '/), conmin_set, 2200.0, F, F, F, -1,  0.0,'SOLUTE','PCL   ',(/0.000,1.000,0.000/)),
     & spcs_type((/'ANH4I  ','ANH4J  ','ANH4K  '/), conmin_set, 1800.0, F, F, F,  1,  3.0,'SOLUTE','PNH4  ',(/0.000,1.000,0.000/)),
     & spcs_type((/'ANAI   ','ANAJ   ','       '/), conmin_set, 2200.0, F, F, F,  1,  0.0,'SOLUTE','PNA   ',(/0.000,1.000,0.000/)),
     & spcs_type((/'       ','AMGJ   ','       '/), conmin_set, 2200.0, F, F, F,  2,  1.0,'DUST  ','PMG   ',(/0.000,1.000,0.000/)),
     & spcs_type((/'       ','AKJ    ','       '/), conmin_set, 2200.0, F, F, F,  1,  1.0,'DUST  ','PK    ',(/0.000,1.000,0.000/)),
     & spcs_type((/'       ','ACAJ   ','       '/), conmin_set, 2200.0, F, F, F,  2,  1.0,'DUST  ','PCA   ',(/0.000,1.000,0.000/)),
     & spcs_type((/'APOCI  ','APOCJ  ','       '/), conmin_set, 2000.0, F, F, F,  0,  4.0,'DUST  ','POC   ',(/0.001,0.999,0.000/)),
     & spcs_type((/'APNCOMI','APNCOMJ','       '/), conmin_set, 2000.0, F, F, F,  0,  4.0,'DUST  ','PNCOM ',(/0.001,0.999,0.000/)),
     & spcs_type((/'AECI   ','AECJ   ','       '/), conmin_set, 2200.0, F, F, F,  0, 10.0,'SOOT  ','PEC   ',(/0.001,0.999,0.000/)),
     & spcs_type((/'       ','AFEJ   ','       '/), conmin_set, 2200.0, F, F, F,  0,  1.0,'DUST  ','PFE   ',(/0.000,1.000,0.000/)),
     & spcs_type((/'       ','AALJ   ','       '/), conmin_set, 2200.0, F, F, F,  0,  1.0,'DUST  ','PAL   ',(/0.000,1.000,0.000/)),
     & spcs_type((/'       ','ASIJ   ','       '/), conmin_set, 2200.0, F, F, F,  0,  1.0,'DUST  ','PSI   ',(/0.000,1.000,0.000/)),
     & spcs_type((/'       ','ATIJ   ','       '/), conmin_set, 2200.0, F, F, F,  0,  1.0,'DUST  ','PTI   ',(/0.000,1.000,0.000/)),
     & spcs_type((/'       ','AMNJ   ','       '/), conmin_set, 2200.0, F, F, F,  0,  1.0,'DUST  ','PMN   ',(/0.000,1.000,0.000/)),
     & spcs_type((/'AH2OI  ','AH2OJ  ','AH2OK  '/), conmin_set, 1000.0, T, F, F,  0,  0.0,'WATER ','PH2O  ',(/0.000,1.000,0.000/)),
     & spcs_type((/'AOTHRI ','AOTHRJ ','       '/), conmin_set, 2200.0, F, F, F,  0,  1.0,'DUST  ','PMOTHR',(/0.000,1.000,0.000/)),
     & spcs_type((/'       ','AALKJ  ','       '/), conmin_set, 2000.0, T, F, F,  0,  4.0,'DUST  ','      ',(/0.000,0.000,0.000/)),
     & spcs_type((/'       ','AXYL1J ','       '/), conmin_set, 2000.0, T, F, F,  0,  4.0,'DUST  ','      ',(/0.000,0.000,0.000/)),
     & spcs_type((/'       ','AXYL2J ','       '/), conmin_set, 2000.0, T, F, F,  0,  4.0,'DUST  ','      ',(/0.000,0.000,0.000/)),
     & spcs_type((/'       ','AXYL3J ','       '/), conmin_set, 2000.0, F, T, F,  0,  4.0,'DUST  ','      ',(/0.000,0.000,0.000/)),
     & spcs_type((/'       ','ATOL1J ','       '/), conmin_set, 2000.0, T, F, F,  0,  4.0,'DUST  ','      ',(/0.000,0.000,0.000/)),
     & spcs_type((/'       ','ATOL2J ','       '/), conmin_set, 2000.0, T, F, F,  0,  4.0,'DUST  ','      ',(/0.000,0.000,0.000/)),
     & spcs_type((/'       ','ATOL3J ','       '/), conmin_set, 2000.0, F, T, F,  0,  4.0,'DUST  ','      ',(/0.000,0.000,0.000/)),
     & spcs_type((/'       ','ABNZ1J ','       '/), conmin_set, 2000.0, T, F, F,  0,  4.0,'DUST  ','      ',(/0.000,0.000,0.000/)),
     & spcs_type((/'       ','ABNZ2J ','       '/), conmin_set, 2000.0, T, F, F,  0,  4.0,'DUST  ','      ',(/0.000,0.000,0.000/)),
     & spcs_type((/'       ','ABNZ3J ','       '/), conmin_set, 2000.0, F, T, F,  0,  4.0,'DUST  ','      ',(/0.000,0.000,0.000/)),
     & spcs_type((/'       ','ATRP1J ','       '/), conmin_set, 2000.0, T, F, F,  0,  4.0,'DUST  ','      ',(/0.000,0.000,0.000/)),
     & spcs_type((/'       ','ATRP2J ','       '/), conmin_set, 2000.0, T, F, F,  0,  4.0,'DUST  ','      ',(/0.000,0.000,0.000/)),
     & spcs_type((/'       ','AISO1J ','       '/), conmin_set, 2000.0, T, F, F,  0,  4.0,'DUST  ','      ',(/0.000,0.000,0.000/)),
     & spcs_type((/'       ','AISO2J ','       '/), conmin_set, 2000.0, T, F, F,  0,  4.0,'DUST  ','      ',(/0.000,0.000,0.000/)),
     & spcs_type((/'       ','ASQTJ  ','       '/), conmin_set, 2000.0, T, F, F,  0,  4.0,'DUST  ','      ',(/0.000,0.000,0.000/)),
     & spcs_type((/'       ','AOLGAJ ','       '/), conmin_set, 2000.0, F, T, F,  0,  4.0,'DUST  ','      ',(/0.000,0.000,0.000/)),
     & spcs_type((/'       ','AOLGBJ ','       '/), conmin_set, 2000.0, F, T, F,  0,  4.0,'DUST  ','      ',(/0.000,0.000,0.000/)),
     & spcs_type((/'       ','AORGCJ ','       '/), conmin_set, 2000.0, F, T, F,  0,  4.0,'DUST  ','      ',(/0.000,0.000,0.000/)),
     & spcs_type((/'       ','       ','ASOIL  '/), conmin_set, 2600.0, F, F, F,  0,  0.0,'DUST  ','      ',(/0.000,0.000,1.000/)),
     & spcs_type((/'       ','       ','ACORS  '/), conmin_cor, 2200.0, F, F, F,  0,  0.0,'DUST  ','PMC   ',(/0.000,0.000,1.000/)),
     & spcs_type((/'       ','       ','ASEACAT'/), conmin_cor, 2200.0, F, F, F,  1,  0.0,'SOLUTE','      ',(/0.000,0.000,1.000/)),
     & spcs_type((/'       ','AIEPXJ ','       '/), conmin_set, 2000.0, F, F, F,  0,  4.0,'DUST  ','      ',(/0.000,0.000,0.000/)),
     & spcs_type((/'       ','AIPANJ ','       '/), conmin_set, 2000.0, F, F, F,  0,  4.0,'DUST  ','      ',(/0.000,0.000,0.000/))/)

C Sea-Salt Speciation factors
      Real, Parameter :: aerospc_ssf( n_mode, n_aerospc ) = Reshape (
C            Aitken    Accum   Coarse
C           -------  -------  -------
     &   (/  0.0,     0.0776,  0.0776,      ! ASO4
     &       0.0,     0.0,     0.0,         ! ANO3
     &       0.0,     0.5538,  0.5538,      ! ACL
     &       0.0,     0.0,     0.0,         ! ANH4
     &       0.0,     0.3086,  0.0,         ! ANA
     &       0.0,     0.0368,  0.0,         ! AMG
     &       0.0,     0.0114,  0.0,         ! AK
     &       0.0,     0.0118,  0.0,         ! ACA
     &       0.0,     0.0,     0.0,         ! APOC
     &       0.0,     0.0,     0.0,         ! APNCOM
     &       0.0,     0.0,     0.0,         ! AEC
     &       0.0,     0.0,     0.0,         ! AFE
     &       0.0,     0.0,     0.0,         ! AAL
     &       0.0,     0.0,     0.0,         ! ASI
     &       0.0,     0.0,     0.0,         ! ATI
     &       0.0,     0.0,     0.0,         ! AMN
     &       0.0,     0.0,     0.0,         ! AH2O
     &       0.0,     0.0,     0.0,         ! AOTHR
     &       0.0,     0.0,     0.0,         ! AALK
     &       0.0,     0.0,     0.0,         ! AXYL1
     &       0.0,     0.0,     0.0,         ! AXYL2
     &       0.0,     0.0,     0.0,         ! AXYL3
     &       0.0,     0.0,     0.0,         ! ATOL1
     &       0.0,     0.0,     0.0,         ! ATOL2
     &       0.0,     0.0,     0.0,         ! ATOL3
     &       0.0,     0.0,     0.0,         ! ABNZ1
     &       0.0,     0.0,     0.0,         ! ABNZ2
     &       0.0,     0.0,     0.0,         ! ABNZ3
     &       0.0,     0.0,     0.0,         ! ATRP1
     &       0.0,     0.0,     0.0,         ! ATRP2
     &       0.0,     0.0,     0.0,         ! AISO1
     &       0.0,     0.0,     0.0,         ! AISO2
     &       0.0,     0.0,     0.0,         ! ASQT
     &       0.0,     0.0,     0.0,         ! AOLGA
     &       0.0,     0.0,     0.0,         ! AOLGB
     &       0.0,     0.0,     0.0,         ! AORGC
     &       0.0,     0.0,     0.0,         ! ASOIL
     &       0.0,     0.0,     0.0,         ! ACORS
     &       0.0,     0.0,     0.3686,      ! ASEACAT
     &       0.0,     0.0,     0.0,         ! AIEPX
     &       0.0,     0.0,     0.0 /),      ! AIPAN
     &   (/ n_mode,n_aerospc /), Order = (/ 1,2 /) )

C Required species
      Character( 16 ), Private, Parameter :: req_so4    = 'ASO4'  
      Character( 16 ), Private, Parameter :: req_no3    = 'ANO3'  
      Character( 16 ), Private, Parameter :: req_cl     = 'ACL'   
      Character( 16 ), Private, Parameter :: req_nh4    = 'ANH4'  
      Character( 16 ), Private, Parameter :: req_na     = 'ANA'   
      Character( 16 ), Private, Parameter :: req_mg     = 'AMG'           
      Character( 16 ), Private, Parameter :: req_k      = 'AK' 
      Character( 16 ), Private, Parameter :: req_ca     = 'ACA' 
      Character( 16 ), Private, Parameter :: req_poc    = 'APOC'  
      Character( 16 ), Private, Parameter :: req_h2o    = 'AH2O'  
      Character( 16 ), Private, Parameter :: req_soil   = 'ASOIL'
      Character( 16 ), Private, Parameter :: req_cors   = 'ACORS' 
      Character( 16 ), Private, Parameter :: req_ncom   = 'APNCOM'  
      Character( 16 ), Private, Parameter :: req_seacat = 'ASEACAT' 
      Character( 16 ), Private, Parameter :: req_aiepx  = 'AIEPX' 
      Character( 16 ), Private, Parameter :: req_aipan  = 'AIPAN' 
     
C Indices of required species
      Integer :: aso4_idx
      Integer :: ano3_idx
      Integer :: acl_idx
      Integer :: anh4_idx
      Integer :: ana_idx
      Integer :: amg_idx
      Integer :: ak_idx
      Integer :: aca_idx
      Integer :: apoc_idx
      Integer :: ah2o_idx 
      Integer :: asoil_idx
      Integer :: acors_idx
      Integer :: apncom_idx
      Integer :: aseacat_idx
      Integer :: aiepx_idx
      Integer :: aipan_idx

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

C Emissions mapping
      Integer         :: n_emis_pm               ! number of aerospc with emissions
      Integer         :: pmem_map( n_aerospc )   ! mapping to aerospc array for PM emissions
      Character( 16 ) :: pmem_units              ! units for PM emissions for all species

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

C HS  01/24/11 Renamed AORGPA as POC for AERO6
C GS  03/02/11 Find new req`d species for AERO6 (Mg, K, Ca)
C HS  03/10/11 Get index for new required species, PNCOM
C-----------------------------------------------------------------------

      Use cgrid_spcs            ! CGRID mechanism species
      Use aeromet_data
      Use utilio_defn

      Implicit None

C Local Variables:
      Character( 80 ) :: xmsg
      Integer m, n, spc
      Real  so4fac
      Real  anthfac

      logdev = setup_logdev()

      if ( mapped ) Return

      aerospc_mw = 0.0
      aerospc_map = 0

C build mapping to CGRID for each aero spc
      Do m = 1, n_mode
         Do spc = 1, n_aerospc

!           write( logdev,* ) '==1 ', aerospc( spc )%name( m ), ' ', ae_spc( 1 ), ' ', ae_spc( 15 )
!           write( logdev,* ) '==2 ', trim( aerospc( spc )%name( m ) ), ' ', ae_spc( 1 ), ' ', ae_spc( 15 )

            If ( aerospc( spc )%name( m ) .Ne. ' ' ) Then
               n = index1( aerospc( spc )%name( m ), n_ae_spc, ae_spc )
               If ( n .Eq. 0 ) Then
                  xmsg = 'Species '// aerospc(spc)%name( m )
     &                 // ' in aerospc name is not in the AE_SPC table.'
                  Call m3exit( pname, 0, 0, xmsg, xstat3 )
               End If
               aerospc_map( spc,m ) = ae_strt - 1 + n
               If ( aerospc_mw( spc ) .Lt. 0.5 ) Then  ! mw=0 means a new species
                  aerospc_mw( spc ) = ae_molwt( n )
               Else If ( aerospc_mw( spc ) .Ne. ae_molwt( n ) ) Then
                  xmsg = 'molecular weight of ' // Trim( aerospc( spc )%name( m ) )
     &                 // ' is different from that of the same species'
     &                 // ' in the same or another mode.'
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
     &           //' in aeronum name is not in AE_SPC'
            Call m3exit( pname, 0, 0, xmsg, xstat3 )
         Else
            aeronum_map( m ) = ae_strt - 1 + n
         End If

         n = index1( aeromode( m )%srf_name, n_ae_spc, ae_spc )
         If ( n .Eq. 0 ) Then
            xmsg = 'species ' // Trim( aeromode( m )%srf_name )
     &           // ' in aerosrf name is not in AE_SPC'
            Call m3exit( pname, 0, 0, xmsg, xstat3 )
         Else
            aerosrf_map( m ) = ae_strt - 1 + n
         End If
      End Do

C Find indices of required species
      aso4_idx    = findAero( req_so4,    .True. )
      ano3_idx    = findAero( req_no3,    .True. )
      acl_idx     = findAero( req_cl,     .True. )
      anh4_idx    = findAero( req_nh4,    .True. )
      ana_idx     = findAero( req_na,     .True. )
      amg_idx     = findAero( req_mg,     .True. )
      ak_idx      = findAero( req_k,      .True. )
      aca_idx     = findAero( req_ca,     .True. )
      apoc_idx    = findAero( req_poc,    .True. )
      ah2o_idx    = findAero( req_h2o,    .True. )
      asoil_idx   = findAero( req_soil,   .True. )
      acors_idx   = findAero( req_cors,   .True. )
      apncom_idx  = findAero( req_ncom,   .True. )
      aseacat_idx = findAero( req_seacat, .True. )
      aiepx_idx   = findAero( req_aiepx, .True. )
      aipan_idx   = findAero( req_aipan, .True. )

#ifdef verbose
      Write( logdev,'( /5x, a )' ) 'map_aero required species' 
      Write( logdev,'( 5x, a, i4 )' ) 'aso4_idx:   ', aso4_idx
      Write( logdev,'( 5x, a, i4 )' ) 'ano3_idx:   ', ano3_idx
      Write( logdev,'( 5x, a, i4 )' ) 'acl_idx:    ', acl_idx
      Write( logdev,'( 5x, a, i4 )' ) 'anh4_idx:   ', anh4_idx
      Write( logdev,'( 5x, a, i4 )' ) 'ana_idx:    ', ana_idx
      Write( logdev,'( 5x, a, i4 )' ) 'amg_idx:    ', amg_idx
      Write( logdev,'( 5x, a, i4 )' ) 'ak_idx:     ', ak_idx
      Write( logdev,'( 5x, a, i4 )' ) 'aca_idx:    ', aca_idx
      Write( logdev,'( 5x, a, i4 )' ) 'apoc_idx:   ', apoc_idx
      Write( logdev,'( 5x, a, i4 )' ) 'ah2o_idx:   ', ah2o_idx
      Write( logdev,'( 5x, a, i4 )' ) 'asoil_idx:  ', asoil_idx
      Write( logdev,'( 5x, a, i4 )' ) 'acors_idx:  ', acors_idx
      Write( logdev,'( 5x, a, i4 )' ) 'apncom_idx: ', apncom_idx
      Write( logdev,'( 5x, a, i4 )' ) 'aseacat_idx:', aseacat_idx
#endif
      Write( logdev,'( 5x, a, i4 )' ) 'aiepx_idx:', aiepx_idx
      Write( logdev,'( 5x, a, i4 )' ) 'aipan_idx:', aipan_idx
 
C Compute common factors
      h2ofac = 1.0D-9 * f6dpi / Real( aerospc( ah2o_idx )%density, 8 )

C compute aeromode_minNum and aeromode_minM2
      so4fac  = 1.0E-9 * Real( f6dpi, 4 ) / aerospc( aso4_idx )%density
      anthfac = 1.0E-9 * Real( f6dpi, 4 ) / aerospc( acors_idx )%density

      Do m = 1, n_mode
         If ( m .lt. n_mode ) Then
            aeromode_minNum( m ) = so4fac * aerospc( aso4_idx )%min_conc( m ) /
     &           ( def_diam( m )**3 * Exp( 4.5 * Log( def_sigma_g( m ) )**2 ) )
         Else
            aeromode_minNum( m ) = anthfac * aerospc( acors_idx )%min_conc( m ) /
     &           ( def_diam( m )**3 * Exp( 4.5 * Log( def_sigma_g( m ) )**2 ) )
         End If
         aeromode_minM2( m ) = aeromode_minNum( m ) * def_diam( m )**2 *
     &            Exp( 2.0 * Log( def_sigma_g( m ) )**2 )
      End do
 
      mapped = .True.

      Write( logdev,'( 5x, a )' ) ' --- Aero Species Mapped ---' 

      Return
      End Subroutine map_aero

C-----------------------------------------------------------------------
      Subroutine map_pmemis ( )
C
C  Set the emissions units from the header of EMIS_1 file
C  Verify that all the units on the file are consistent

      Use utilio_defn

      Implicit None

      Include SUBST_FILES_ID  ! file name parameters

C Parameters:
      Character( 10 ), Parameter :: blank10 = ' '

C Local Variables:
      Character( 16 ), Save :: pname = 'map_pmemis'
      Character( 512 )      :: xmsg1
      Character( 1024 )     :: xmsg2
      Character( 10 )       :: units

      Integer :: indx
      Integer :: v
      Logical :: found, match

      Logical, Save :: pm_mapped = .False.

      logdev = setup_logdev()

C Create mapping only if first call
      If ( pm_mapped ) Return

C Call routine to map aerosol species array
      If ( .Not. mapped ) Call map_aero ( )

C Open the gridded emissions file, which contains gas, aerosol, and non-reactive
C species

      If ( .Not. open3( emis_1, fsread3, pname ) ) Then
         xmsg1 = 'Could not open '// emis_1 // ' file'
         Call m3exit( pname, 0, 0, Trim( xmsg1 ), xstat1 )
      End If

      If ( .Not. desc3( emis_1 ) ) Then
         xmsg1 = 'Could not get '// 'EMIS_1' // ' file description'
         Call m3exit( pname, 0, 0, Trim( xmsg1 ), xstat2 )
      End If

C Search emissions file for emission species names. Verify that their units
C are the same and set pmem_units

      n_emis_pm = 0
      pmem_units = 'null'
      found = .True.
      match = .True.
      xmsg1 = 'Could not find the following species in emissions file'
      xmsg2 = 'PM Units not uniform in EMIS_1 file.'

      Do v = 1, n_aerospc
         If ( aerospc( v )%emis .Ne. ' ' ) Then
            indx = index1( aerospc( v )%emis, nvars3d, vname3d )
            If ( indx .Le. 0 ) Then
               xmsg1 = Trim( xmsg1 ) // crlf() // blank10
     &               // Trim( aerospc( v )%emis )
               found = .False.
               Cycle
            End If

            n_emis_pm = n_emis_pm + 1
            pmem_map( n_emis_pm ) = v

C Change UNITS to upper case
            units = units3d( indx )
            Call upcase( units )

C Save units on first emissions
            If ( pmem_units .Eq. 'null' ) pmem_units = units

C Check that all emissions units match
            If ( pmem_units .Ne. units ) Then
               xmsg2 = Trim( xmsg2 ) // crlf() // blank10
     &               // Trim( aerospc( v )%emis )
     &               // '  [' // Trim( units3d( indx ) ) // ']'
               match = .False.
            End If
         End If
      End Do

      If ( .Not. found ) Then
         Call m3exit( pname, 0, 0, Trim( xmsg1 ), xstat2 )
      End If

      If ( .Not. match ) Then
         Call m3exit( pname, 0, 0, Trim( xmsg2 ), xstat2 )
      End If

#ifdef verbose
      Write( logdev,'( /5x, a )' ) 'pmem_map to aerospc' 
         Do v = 1, n_emis_pm
         Write( logdev,'( 5x, a, 2i4, 2x, a )' ) 'pmem_map:', v, pmem_map( v ), aerospc( pmem_map( v ) )%emis
      End Do
#endif

      pm_mapped = .True.

      Write( logdev,'( 5x, a )' ) ' --- PM Emis Species Mapped ---' 

      Return

      End Subroutine map_pmemis

C-----------------------------------------------------------------------
      Subroutine extract_aero( conc, minchk )

C  Extracts the required aerosol data from CGRID into conc.
 
C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C-----------------------------------------------------------------------

      Implicit None

C Arguments:
      Real,    Intent( In ) :: conc( : )
      Logical, Intent( In ) :: minchk

C Local Variables:
      Integer m, n, spc 

      If ( .Not. mapped ) Then
         Call map_aero()
      End If

C Copy grid cell concentrations of aero species to aerospc_conc
      aerospc_conc = 0.0
      If ( minchk ) Then
         Do m = 1, n_mode
            Do spc = 1, n_aerospc
               n = aerospc_map( spc,m )
               If ( n .ne. 0 ) Then
                  aerospc_conc( spc,m ) = Max( conc( n ), aerospc( spc )%min_conc( m ) ) ! [ug/m^3]
               End If
            End Do
         End Do
      Else
         Do m = 1, n_mode
            Do spc = 1, n_aerospc
               n = aerospc_map( spc,m )
               If ( n .ne. 0 ) Then
                  aerospc_conc( spc,m ) = conc( n )   ! [ug/m^3]
               End If
            End Do
         End Do
      End If

C Copy grid cell concentrations of aero # and surf area
C Convert and assign to moment0_conc and moment2_conc

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

      Use aeromet_data      ! fundamental constants, data type definitions, etc.
      Use utilio_defn

      Implicit None

C Arguments:
      Real, Intent( Out ) :: conc( : )
      Logical, Intent( In ) :: minchk

C Local variables:
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
         conc( n ) = Real( pi, 4 ) * moment2_conc( m ) 
      End Do

      Return
      End Subroutine update_aero   

C-----------------------------------------------------------------------
!     Integer Function findAero( vname, required ) Result ( idx )
      Function findAero( vname, required ) Result ( idx )

C  Finds the index of 'required' aerosol species in the aerospc list
 
C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C-----------------------------------------------------------------------

      Use utilio_defn

      Implicit None

C Arguments:
      Character( 16 ) :: vname
      Logical :: required
      Integer :: idx

C Local Variables:
      Character( 80 ) :: xmsg
      Integer spc, n

C Find the substring vname in aerospc( spc )%name( n )
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

