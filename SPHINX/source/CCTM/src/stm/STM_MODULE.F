
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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module stm_module

C  Contains:
C     Subroutines stm_wrap_ae
C                 stm_wrap_gas

C  Revision History:
C SR 12/13/2018 Initial version

C----------------------------------------------------------------------

      Implicit None

      Logical, Private, Save :: stm_mapped = .False.

      Real, Allocatable :: aero_conc( :,: ) ! aero species concentration [ ug/m^3 ]

      Contains

C-----------------------------------------------------------------------
      Subroutine stm_wrap_ae ( cgrid, jdate, jtime )

C  capture changes from aero proc (changes in aso4 from new particle
C     formation and condensation)

C  Revision History:
C     Initial version - 6/5/19 - Shawn Roselle
C
C-----------------------------------------------------------------------
      Use grid_conf, Only: ncols, nrows, nlays
      Use runtime_vars, Only: adj_stmspc
      Use aero_budget, Only: aero_cond, aero_npf
      Use aero_data, Only : aso4_idx, aso4aqh2o2_idx, aso4aqo3_idx,
     &                      aso4aqfemn_idx, aso4aqmhp_idx, aso4aqpaa_idx,
     &                      aso4gas_idx, aso4emis_idx, aso4icbc_idx,
     &                      oso4aqh2o2_idx, oso4aqo3_idx, oso4aqfemn_idx,
     &                      oso4aqmhp_idx, oso4aqpaa_idx, oso4_idx,
     &                      oso4gas_idx, oso4emis_idx, oso4icbc_idx,
     &                      aerospc_map, aerospc_mw, aerospc,
     &                      n_mode, ae6isoa,
     &                      findAero
      Use stm_vars, Only: organosulf

      Implicit None

C Arguments

      Real, Pointer :: cgrid( :,:,:,: )

      Integer, Intent( In ) :: jdate
      Integer, Intent( In ) :: jtime

C Local variables

      Integer c, r, l, m

      Real aso4tot, oso4tot
      Real mscor

      Do c = 1, ncols
         Do r = 1, nrows
            Do l = 1, nlays

C extract aerosol species from CGRID

               Call stm_extract_aero ( cgrid( c,r,l,: ) )

               Do m = 1, n_mode
                  aero_conc( aso4gas_idx, m ) =
     &                     Max( ( aero_conc( aso4gas_idx, m )
     &                          + aero_cond( c,r,l,aerospc_map( aso4_idx,m ) )
     &                          + aero_npf ( c,r,l,aerospc_map( aso4_idx,m ) ) ),
     &                          aerospc( aso4gas_idx )%min_conc( m ) )
               End Do

C normalize tracked sulfate species to total modeled sulfate

               If ( adj_stmspc ) Then

                  Do m = 1, n_mode

                     aso4tot = aero_conc( aso4gas_idx, m )
     &                       + aero_conc( aso4emis_idx,m )
     &                       + aero_conc( aso4icbc_idx,m )

C...  for the accumulation mode, add aqueous tracked species

                     If ( m .Eq. 2 ) Then

                        aso4tot = aso4tot
     &                          + aero_conc( aso4aqh2o2_idx,m )
     &                          + aero_conc( aso4aqo3_idx,  m )
     &                          + aero_conc( aso4aqfemn_idx,m )
     &                          + aero_conc( aso4aqmhp_idx, m )
     &                          + aero_conc( aso4aqpaa_idx, m )

                     End If

C...  normalize sulfur tracking species

                     If ( aso4tot .Gt. 0.0 ) Then

                        aso4tot = Max( aso4tot,
     &                                 aerospc( aso4_idx )%min_conc( m ) )
                        mscor = Max( aero_conc( aso4_idx, m ),
     &                               aerospc( aso4_idx )%min_conc( m ) )
     &                        / aso4tot

                        aero_conc( aso4gas_idx, m ) =
     &                     Max( aero_conc( aso4gas_idx, m ) * mscor,
     &                          aerospc( aso4gas_idx )%min_conc( m ) )
                        aero_conc( aso4emis_idx, m ) =
     &                     Max( aero_conc( aso4emis_idx, m ) * mscor,
     &                          aerospc( aso4emis_idx )%min_conc( m ) )
                        aero_conc( aso4icbc_idx, m ) =
     &                     Max( aero_conc( aso4icbc_idx, m ) * mscor,
     &                          aerospc( aso4icbc_idx )%min_conc( m ) )

C...  for the accumulation mode, adjust aqueous tracked species

                        If ( m .Eq. 2 ) Then
                           aero_conc( aso4aqh2o2_idx, m ) =
     &                        Max( aero_conc( aso4aqh2o2_idx, m ) * mscor,
     &                             aerospc( aso4aqh2o2_idx )%min_conc( m ) )
                           aero_conc( aso4aqo3_idx, m ) =
     &                        Max( aero_conc( aso4aqo3_idx, m ) * mscor,
     &                             aerospc( aso4aqo3_idx )%min_conc( m ) )
                           aero_conc( aso4aqfemn_idx, m ) =
     &                        Max( aero_conc( aso4aqfemn_idx, m ) * mscor,
     &                             aerospc( aso4aqfemn_idx )%min_conc( m ) )
                           aero_conc( aso4aqmhp_idx, m ) =
     &                        Max( aero_conc( aso4aqmhp_idx, m ) * mscor,
     &                             aerospc( aso4aqmhp_idx )%min_conc( m ) )
                           aero_conc( aso4aqpaa_idx, m ) =
     &                        Max( aero_conc( aso4aqpaa_idx, m ) * mscor,
     &                             aerospc( aso4aqpaa_idx )%min_conc( m ) )
                        End If

                     End If

                     If ( organosulf ) Then

C...  in the current implementation of heterogeneous chemistry,
C...    only the accumulation mode sulfate can be converted to organosulfate

                        If ( m .Eq. 2 ) Then
                           oso4tot = aero_conc( oso4gas_idx, m )
     &                             + aero_conc( oso4emis_idx,m )
     &                             + aero_conc( oso4icbc_idx,m )
     &                             + aero_conc( oso4aqh2o2_idx,m )
     &                             + aero_conc( oso4aqo3_idx,  m )
     &                             + aero_conc( oso4aqfemn_idx,m )
     &                             + aero_conc( oso4aqmhp_idx, m )
     &                             + aero_conc( oso4aqpaa_idx, m )

                           If ( oso4tot .Gt. 0.0 ) Then

                              oso4tot = Max( oso4tot,
     &                                       aerospc( oso4_idx )%min_conc( m ) )
                              mscor = Max( aero_conc( oso4_idx, m ),
     &                                     aerospc( oso4_idx )%min_conc( m ) )
     &                              / oso4tot

                              aero_conc( oso4gas_idx, m ) =
     &                           Max( aero_conc( oso4gas_idx, m ) * mscor,
     &                                aerospc( oso4gas_idx )%min_conc( m ) )
                              aero_conc( oso4emis_idx, m ) =
     &                           Max( aero_conc( oso4emis_idx, m ) * mscor,
     &                                aerospc( oso4emis_idx )%min_conc( m ) )
                              aero_conc( oso4icbc_idx, m ) =
     &                           Max( aero_conc( oso4icbc_idx, m ) * mscor,
     &                                aerospc( oso4icbc_idx )%min_conc( m ) )
                              aero_conc( oso4aqh2o2_idx, m ) =
     &                           Max( aero_conc( oso4aqh2o2_idx, m ) * mscor,
     &                                aerospc( oso4aqh2o2_idx )%min_conc( m ) )
                              aero_conc( oso4aqo3_idx, m ) =
     &                           Max( aero_conc( oso4aqo3_idx, m ) * mscor,
     &                                aerospc( oso4aqo3_idx )%min_conc( m ) )
                              aero_conc( oso4aqfemn_idx, m ) =
     &                           Max( aero_conc( oso4aqfemn_idx, m ) * mscor,
     &                                aerospc( oso4aqfemn_idx )%min_conc( m ) )
                              aero_conc( oso4aqmhp_idx, m ) =
     &                           Max( aero_conc( oso4aqmhp_idx, m ) * mscor,
     &                                aerospc( oso4aqmhp_idx )%min_conc( m ) )
                              aero_conc( oso4aqpaa_idx, m ) =
     &                           Max( aero_conc( oso4aqpaa_idx, m ) * mscor,
     &                                aerospc( oso4aqpaa_idx )%min_conc( m ) )

                           End If
                        End If
                     End If

                  End Do

               End If

C update aerosol species in CGRID

               Call stm_update_aero ( cgrid( c,r,l,: ) )

            End Do
         End Do
      End Do

      Return

      End Subroutine stm_wrap_ae


C-----------------------------------------------------------------------
      Subroutine stm_wrap_gas ( cgrid, jdate, jtime, b4chem )

C  capture changes in aso4 from gas phase chemistry
C  in the current release, ASO4 can be lost in gas chem to form organo-sulfate
C
C     Initial version - 6/5/19 - Shawn Roselle
C-----------------------------------------------------------------------

      Use grid_conf, Only: ncols, nrows, nlays
      Use utilio_defn
      Use aero_data, Only : aso4_idx, aso4aqh2o2_idx, aso4aqo3_idx,
     &                      aso4aqfemn_idx, aso4aqmhp_idx, aso4aqpaa_idx,
     &                      aso4gas_idx, aso4emis_idx, aso4icbc_idx,
     &                      oso4aqh2o2_idx, oso4aqo3_idx, oso4aqfemn_idx,
     &                      oso4aqmhp_idx, oso4aqpaa_idx, oso4_idx,
     &                      oso4gas_idx, oso4emis_idx, oso4icbc_idx,
     &                      aerospc, n_mode, ae6isoa
      Use stm_vars, Only: organosulf

      Implicit None

C Arguments

      Real, Pointer :: cgrid( :,:,:,: )

      Integer, Intent( In ) :: jdate
      Integer, Intent( In ) :: jtime
      Logical, Intent( In ) :: b4chem

C local variables

      Character( 16 ) :: pname = 'STM_WRAP_GAS'
      Character( 96 ) :: xmsg = ' '

      Logical, Save :: firstime = .True.

      Integer c, r, l, m
      Integer allocstat

      Real fso4, omfso4
      Real, Save, Allocatable :: so4b4( :,:,:,: )

C-----------------------------------------------------------------------
C  begin body of subroutine stm_wrap_gas

      If ( firstime ) Then

         Allocate ( so4b4( ncols, nrows, nlays, n_mode ),
     &              Stat = allocstat )
         If ( allocstat .Ne. 0 ) Then
            xmsg = 'Failure allocating so4_b4 '
            Call m3exit( pname, jdate, jtime, xmsg, xstat1 )
         End If

         firstime = .False.

      End If

      if ( .not. organosulf ) Return

C capture the loss of ASO4J to organosulfate
      m = 2  ! in the current implementation of heterogeneous chemistry,
             ! only accumulation mode sulfate can be converted to organosulfate

C save ASO4 concentrations before call to CHEM
      If ( b4chem ) Then

         Do c = 1, ncols
            Do r = 1, nrows
               Do l = 1, nlays

C extract aerosol species from CGRID
                  Call stm_extract_aero ( cgrid( c,r,l,: ) )

                  so4b4( c,r,l,m ) = Max( aero_conc( aso4_idx, m ),
     &                                    aerospc( aso4_idx )%min_conc( m ) )

               End Do
            End Do
         End Do

C scale sulfur tracking species by change in ASO4J following call to CHEM
      Else

         Do c = 1, ncols
            Do r = 1, nrows
               Do l = 1, nlays

C extract aerosol species from CGRID
                  Call stm_extract_aero ( cgrid( c,r,l,: ) )

                  If ( ( aero_conc( aso4_idx, m ) .Ne. so4b4( c,r,l,m ) ) .And.
     &                 ( so4b4( c,r,l,m ) .Gt. 0.0 ) ) Then

C fractional change in ASO4J
                     fso4 = Max( aero_conc( aso4_idx, m ),
     &                           aerospc( aso4_idx )%min_conc( m ) )
     &                    / so4b4( c,r,l,m )
                     omfso4 = 1.0 - fso4

                     aero_conc( oso4_idx, m ) =
     &                  Max( ( aero_conc( oso4_idx, m )
     &                       + ( so4b4( c,r,l,m ) - aero_conc( aso4_idx, m ) ) ),
     &                       aerospc( oso4_idx )%min_conc( m ) )

                     aero_conc( oso4gas_idx, m ) =
     &                  Max( ( aero_conc( oso4gas_idx, m )
     &                       + aero_conc( aso4gas_idx, m ) * omfso4 ),
     &                       aerospc( oso4gas_idx )%min_conc( m ) )
                     aero_conc( oso4emis_idx, m ) =
     &                  Max( ( aero_conc( oso4emis_idx, m )
     &                       + aero_conc( aso4emis_idx, m ) * omfso4 ),
     &                       aerospc( oso4emis_idx )%min_conc( m ) )
                     aero_conc( oso4icbc_idx, m ) =
     &                  Max( ( aero_conc( oso4icbc_idx, m )
     &                       + aero_conc( aso4icbc_idx, m ) * omfso4 ),
     &                       aerospc( oso4icbc_idx )%min_conc( m ) )
                     aero_conc( oso4aqh2o2_idx, m ) =
     &                  Max( ( aero_conc( oso4aqh2o2_idx, m )
     &                       + aero_conc( aso4aqh2o2_idx, m ) * omfso4 ),
     &                       aerospc( oso4aqh2o2_idx )%min_conc( m ) )
                     aero_conc( oso4aqo3_idx, m ) =
     &                  Max( ( aero_conc( oso4aqo3_idx, m )
     &                       + aero_conc( aso4aqo3_idx, m ) * omfso4 ),
     &                       aerospc( oso4aqo3_idx )%min_conc( m ) )
                     aero_conc( oso4aqfemn_idx, m ) =
     &                  Max( ( aero_conc( oso4aqfemn_idx, m )
     &                       + aero_conc( aso4aqfemn_idx, m ) * omfso4 ),
     &                       aerospc( oso4aqfemn_idx )%min_conc( m ) )
                     aero_conc( oso4aqmhp_idx, m ) =
     &                  Max( ( aero_conc( oso4aqmhp_idx, m )
     &                       + aero_conc( aso4aqmhp_idx, m ) * omfso4 ),
     &                       aerospc( oso4aqmhp_idx )%min_conc( m ) )
                     aero_conc( oso4aqpaa_idx, m ) =
     &                  Max( ( aero_conc( oso4aqpaa_idx, m )
     &                       + aero_conc( aso4aqpaa_idx, m ) * omfso4 ),
     &                       aerospc( oso4aqpaa_idx )%min_conc( m ) )

c  reduce tracked inorganic species by amount converted to organosulfate
                     aero_conc( aso4gas_idx, m ) =
     &                  Max( aero_conc( aso4gas_idx, m ) * fso4,
     &                       aerospc( aso4gas_idx )%min_conc( m ) )
                     aero_conc( aso4emis_idx, m ) =
     &                  Max( aero_conc( aso4emis_idx, m ) * fso4,
     &                       aerospc( aso4emis_idx )%min_conc( m ) )
                     aero_conc( aso4icbc_idx, m ) =
     &                  Max( aero_conc( aso4icbc_idx, m ) * fso4,
     &                       aerospc( aso4icbc_idx )%min_conc( m ) )
                     aero_conc( aso4aqh2o2_idx, m ) =
     &                  Max( aero_conc( aso4aqh2o2_idx, m ) * fso4,
     &                       aerospc( aso4aqh2o2_idx )%min_conc( m ) )
                     aero_conc( aso4aqo3_idx, m ) =
     &                  Max( aero_conc( aso4aqo3_idx, m ) * fso4,
     &                       aerospc( aso4aqo3_idx )%min_conc( m ) )
                     aero_conc( aso4aqfemn_idx, m ) =
     &                  Max( aero_conc( aso4aqfemn_idx, m ) * fso4,
     &                       aerospc( aso4aqfemn_idx )%min_conc( m ) )
                     aero_conc( aso4aqmhp_idx, m ) =
     &                  Max( aero_conc( aso4aqmhp_idx, m ) * fso4,
     &                       aerospc( aso4aqmhp_idx )%min_conc( m ) )
                     aero_conc( aso4aqpaa_idx, m ) =
     &                  Max( aero_conc( aso4aqpaa_idx, m ) * fso4,
     &                       aerospc( aso4aqpaa_idx )%min_conc( m ) )
                  End If

C update aerosol species in CGRID

                  Call stm_update_aero ( cgrid( c,r,l,: ) )

               End Do
            End Do
         End Do

      End If
      Return

      End Subroutine stm_wrap_gas

C-----------------------------------------------------------------------
      Subroutine stm_extract_aero( conc )

C  Extracts aerosol data into the aero_conc array
C  The original idea is that the data for conc comes from CGRID

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C     6/25/19 streamlined version of extract_aero borrowed from AERO_DATA
C-----------------------------------------------------------------------

      Use aero_data, Only : n_aerospc, aerospc_map, n_mode

      Implicit None

C Arguments:
      Real,    Intent( In ) :: conc( : )

C Local Variables:
      Logical, Save :: firstime = .True.

      Integer m, n, spc

      If ( .Not. stm_mapped ) Then
         Call stm_map_aero()
      End If

C Copy grid cell concentrations of aero species to aero_conc
      aero_conc = 0.0
      Do m = 1, n_mode
         Do spc = 1, n_aerospc
            n = aerospc_map( spc,m )
            If ( n .Ne. 0 ) Then
               aero_conc( spc,m ) = conc( n )   ! [ug/m^3]
            End If
         End Do
      End Do

      Return
      End Subroutine stm_extract_aero

C-----------------------------------------------------------------------
      Subroutine stm_update_aero( conc )

C  Updates conc from the aero_conc array.
C  The original idea is that the data in conc updates CGRID
C  stm_update_aero now also saves the updated surface area back to CGRID as
C  well.

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C     6/25/19 streamlined version of update_aero borrowed from AERO_DATA
C-----------------------------------------------------------------------

      Use aero_data, Only : n_aerospc, aerospc_map, n_mode

      Use utilio_defn

      Implicit None

C Arguments:
      Real, Intent( Out ) :: conc( : )

C Local variables:

      Character( 16 ) :: pname = 'STM_WRAP_GAS'
      Character( 80 ) :: xmsg

      Integer m, n, spc

      If ( .Not. stm_mapped ) Then
         xmsg = 'CGRID Species has not been mapped in stm_update_aero'
         Call m3exit( pname, 0, 0, xmsg, xstat3 )
      End If

C Copy aero_conc back to grid cell concentrations

      Do m = 1, n_mode
         Do spc = 1, n_aerospc
            n = aerospc_map( spc,m )
            If ( n .Ne. 0 ) Then
                 conc( n ) = aero_conc( spc,m )
            End If
         End Do
      End Do

      Return
      End Subroutine stm_update_aero

C-----------------------------------------------------------------------
      Subroutine stm_map_aero()

C  Defines aerosol mapping from CGRID for species

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C     6/25/19 streamlined version of map_aero borrowed from AERO_DATA
C-----------------------------------------------------------------------

      Use aero_data, Only : n_aerospc, map_aero, n_mode

      Implicit None

      If ( stm_mapped ) Return

C...map the aerosol species using map_aero in the AERO_DATA module
      Call map_aero()

      Allocate ( aero_conc  ( n_aerospc, n_mode ) )

      stm_mapped = .True.

      Return
      End Subroutine stm_map_aero

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      End Module stm_module
