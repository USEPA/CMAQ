
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
      Use rxns_data, only: mechname       ! chemical mechanism data
      Use aero_budget, Only: aero_cond, aero_npf
      Use aero_data, Only : aso4_idx, aso4aqh2o2_idx, aso4aqo3_idx,
     &                      aso4aqfemn_idx, aso4aqmhp_idx, aso4aqpaa_idx,
     &                      aso4gas_idx, aso4emis_idx, aso4icbc_idx,
     &                      oso4aqh2o2_idx, oso4aqo3_idx, oso4aqfemn_idx,
     &                      oso4aqmhp_idx, oso4aqpaa_idx, oso4_idx, 
     &                      oso4gas_idx, oso4emis_idx, oso4icbc_idx,
!     &                      aieos_idx, aimos_idx, 
     &                      aerospc_conc, aerospc_map, aerospc_mw, n_mode, 
     &                      ae6isoa,
     &                      extract_aero, update_aero, findAero

      Implicit None

C Arguments

      Real, Pointer :: cgrid( :,:,:,: )

      Integer, Intent( In ) :: jdate
      Integer, Intent( In ) :: jtime

C Local variables

!      Character( 16 ), Parameter :: req_aiso3 = 'AISO3'

!      Logical, Save :: firstime = .True.

      Integer c, r, l, m
!      Integer, Save :: aiso3_idx

      Real aso4tot, oso4tot
      Real mscor

!      If ( firstime ) Then
!         aiso3_idx   = findaero( req_aiso3, .True. )
!         firstime = .False.
!      End If

      Do c = 1, ncols
         Do r = 1, nrows
            Do l = 1, nlays

C extract aerosol species from CGRID

               Call extract_aero ( cgrid( c,r,l,: ), .True. )

               Do m = 1, n_mode
                  aerospc_conc( aso4gas_idx, m ) = aerospc_conc( aso4gas_idx,m )
     &                                           + aero_cond( c,r,l,aerospc_map( aso4_idx,m ) )
     &                                           + aero_npf ( c,r,l,aerospc_map( aso4_idx,m ) )
               End Do

C normalize tracked sulfate species to total modeled sulfate

               If ( adj_stmspc ) Then

                  Do m = 1, n_mode
         
                     aso4tot = aerospc_conc( aso4gas_idx, m )
     &                       + aerospc_conc( aso4emis_idx,m )
     &                       + aerospc_conc( aso4icbc_idx,m )

C...  for the accumulation mode, add aqueous tracked species

                     If ( m .Eq. 2 ) Then

                        aso4tot = aso4tot
     &                          + aerospc_conc( aso4aqh2o2_idx,m )
     &                          + aerospc_conc( aso4aqo3_idx,  m )
     &                          + aerospc_conc( aso4aqfemn_idx,m ) 
     &                          + aerospc_conc( aso4aqmhp_idx, m )
     &                          + aerospc_conc( aso4aqpaa_idx, m )

                     End If

C...  normalize sulfur tracking species

                     If ( aso4tot .Gt. 0.0 ) Then
                        mscor = aerospc_conc( aso4_idx, m ) / aso4tot
                        aerospc_conc( aso4gas_idx, m ) = aerospc_conc( aso4gas_idx, m )
     &                                                 * mscor
                        aerospc_conc( aso4emis_idx,m ) = aerospc_conc( aso4emis_idx,m )
     &                                                 * mscor
                        aerospc_conc( aso4icbc_idx,m ) = aerospc_conc( aso4icbc_idx,m )
     &                                                 * mscor
                        If ( m .Eq. 2 ) Then
                           aerospc_conc( aso4aqh2o2_idx,m ) = aerospc_conc( aso4aqh2o2_idx,m )
     &                                                      * mscor
                           aerospc_conc( aso4aqo3_idx,  m ) = aerospc_conc( aso4aqo3_idx,  m )
     &                                                      * mscor
                           aerospc_conc( aso4aqfemn_idx,m ) = aerospc_conc( aso4aqfemn_idx,m )
     &                                                      * mscor
                           aerospc_conc( aso4aqmhp_idx, m ) = aerospc_conc( aso4aqmhp_idx, m )
     &                                                      * mscor
                           aerospc_conc( aso4aqpaa_idx, m ) = aerospc_conc( aso4aqpaa_idx, m )
     &                                                      * mscor
                        End If
                     End If
         
                     If ( ( ae6isoa ) .Or.
     &                    ( Index( mechname, 'CB6R3_AE7'  ) .Gt. 0 ) .Or.
     &                    ( Index( mechname, 'CB6R3M_AE7' ) .Gt. 0 ) ) Then
    
C...  for the accumulation mode, add aqueous tracked species

                        If ( m .Eq. 2 ) Then
                           oso4tot = aerospc_conc( oso4gas_idx, m )
     &                             + aerospc_conc( oso4emis_idx,m )
     &                             + aerospc_conc( oso4icbc_idx,m )
     &                             + aerospc_conc( oso4aqh2o2_idx,m )
     &                             + aerospc_conc( oso4aqo3_idx,  m )
     &                             + aerospc_conc( oso4aqfemn_idx,m ) 
     &                             + aerospc_conc( oso4aqmhp_idx, m )
     &                             + aerospc_conc( oso4aqpaa_idx, m )

                           If ( oso4tot .Gt. 0.0 ) Then

!                              If ( ae6isoa ) Then
!                                 mscor = ( aerospc_conc( aieos_idx, m ) / aerospc_mw( aieos_idx )
!     &                                   + aerospc_conc( aimos_idx, m ) / aerospc_mw( aimos_idx ) ) * aerospc_mw( aso4_idx )
!     &                                 / oso4tot
!                              Else
!                                 mscor = 0.4 * aerospc_conc( aiso3_idx, m ) / 216.0
!     &                                 * aerospc_mw( aso4_idx )
!     &                                 / oso4tot
!                              End If
                              mscor = aerospc_conc( oso4_idx, m ) / oso4tot

                              aerospc_conc( oso4gas_idx, m ) = aerospc_conc( oso4gas_idx, m )
     &                                                       * mscor
                              aerospc_conc( oso4emis_idx,m ) = aerospc_conc( oso4emis_idx,m )
     &                                                       * mscor
                              aerospc_conc( oso4icbc_idx,m ) = aerospc_conc( oso4icbc_idx,m )
     &                                                       * mscor   
                              aerospc_conc( oso4aqh2o2_idx,m ) = aerospc_conc( oso4aqh2o2_idx,m )
     &                                                         * mscor
                              aerospc_conc( oso4aqo3_idx,  m ) = aerospc_conc( oso4aqo3_idx,  m )
     &                                                         * mscor
                              aerospc_conc( oso4aqfemn_idx,m ) = aerospc_conc( oso4aqfemn_idx,m )
     &                                                         * mscor
                              aerospc_conc( oso4aqmhp_idx, m ) = aerospc_conc( oso4aqmhp_idx, m )
     &                                                         * mscor
                              aerospc_conc( oso4aqpaa_idx, m ) = aerospc_conc( oso4aqpaa_idx, m )
     &                                                         * mscor
                        
                           End If
                        End If
                     End If

                  End Do

               End If

C update aerosol species in CGRID

               Call update_aero ( cgrid( c,r,l,: ), .True. )

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
      Use utilio_defn, Only: xstat1
      Use rxns_data, only: mechname       ! chemical mechanism data
      Use aero_data, Only : aso4_idx, aso4aqh2o2_idx, aso4aqo3_idx,
     &                      aso4aqfemn_idx, aso4aqmhp_idx, aso4aqpaa_idx,
     &                      aso4gas_idx, aso4emis_idx, aso4icbc_idx,
     &                      oso4aqh2o2_idx, oso4aqo3_idx, oso4aqfemn_idx,
     &                      oso4aqmhp_idx, oso4aqpaa_idx, oso4_idx,
     &                      oso4gas_idx, oso4emis_idx, oso4icbc_idx,
     &                      aerospc_conc, n_mode, ae6isoa,
     &                      extract_aero, update_aero

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

      Real fso4
      Real, Save, Allocatable :: so4b4( :,:,:,: )

C-----------------------------------------------------------------------
C  begin body of subroutine stm_wrap_gas

      If ( firstime ) Then

         Allocate ( so4b4( ncols, nrows, nlays, n_mode ),
     &              Stat = allocstat )
         If ( allocstat .Ne. 0 ) Then
            xmsg = 'Failure allocating so4_b4 or fso4'
            Call m3exit( pname, jdate, jtime, xmsg, xstat1 )
         End If

         firstime = .False.

      End If

      Do c = 1, ncols
         Do r = 1, nrows
            Do l = 1, nlays
  
C save ASO4 concentrations before call to CHEM
               If ( b4chem ) Then
  
C extract aerosol species from CGRID
                  Call extract_aero ( cgrid( c,r,l,: ), .True. )

                  Do m = 1, n_mode
                     so4b4( c,r,l,m ) = aerospc_conc( aso4_idx, m )
                  End Do

C scale sulfur tracking species by change in ASO4
               Else
  
C extract aerosol species from CGRID
                  Call extract_aero ( cgrid( c,r,l,: ), .True. )

                  Do m = 1, n_mode
                     If ( ( aerospc_conc( aso4_idx, m ) .Ne. so4b4( c,r,l,m ) ) .And.
     &                    ( so4b4( c,r,l,m ) .Gt. 0.0 ) ) Then

                        fso4 = aerospc_conc( aso4_idx, m ) / so4b4( c,r,l,m )

c  capture the loss of SO4 to organosulfate
                        If ( ( ae6isoa ) .Or.
     &                       ( Index( mechname, 'CB6R3_AE7'  ) .Gt. 0 ) .Or.
     &                       ( Index( mechname, 'CB6R3M_AE7' ) .Gt. 0 ) ) Then
                           If ( m .Eq. 2 ) Then
                              aerospc_conc( oso4_idx, m ) = aerospc_conc( oso4_idx, m )
     &                            + ( so4b4( c,r,l,m ) - aerospc_conc( aso4_idx, m ) )
                              aerospc_conc( oso4gas_idx, m ) = aerospc_conc( oso4gas_idx, m )
     &                            + ( aerospc_conc( aso4gas_idx,  m ) * ( 1.0 - fso4 ) )
                              aerospc_conc( oso4emis_idx,m ) = aerospc_conc( oso4emis_idx,m )
     &                            + ( aerospc_conc( aso4emis_idx, m ) * ( 1.0 - fso4 ) )
                              aerospc_conc( oso4icbc_idx,m ) = aerospc_conc( oso4icbc_idx,m )
     &                            + ( aerospc_conc( aso4icbc_idx, m ) * ( 1.0 - fso4 ) )
                              aerospc_conc( oso4aqh2o2_idx,m ) = aerospc_conc( oso4aqh2o2_idx,m )
     &                            + ( aerospc_conc( aso4aqh2o2_idx,  m ) * ( 1.0 - fso4 ) )
                              aerospc_conc( oso4aqo3_idx,  m ) = aerospc_conc( oso4aqo3_idx,  m )
     &                            + ( aerospc_conc( aso4aqo3_idx,  m ) * ( 1.0 - fso4 ) )
                              aerospc_conc( oso4aqfemn_idx,m ) = aerospc_conc( oso4aqfemn_idx,m )
     &                            + ( aerospc_conc( aso4aqfemn_idx,  m ) * ( 1.0 - fso4 ) )
                              aerospc_conc( oso4aqmhp_idx, m ) = aerospc_conc( oso4aqmhp_idx, m )
     &                            + ( aerospc_conc( aso4aqmhp_idx,  m ) * ( 1.0 - fso4 ) )
                              aerospc_conc( oso4aqpaa_idx, m ) = aerospc_conc( oso4aqpaa_idx, m )
     &                            + ( aerospc_conc( aso4aqpaa_idx,  m ) * ( 1.0 - fso4 ) )

                           End If
                        End If

c  reduce tracked species by amount converted to organosulfate
                        aerospc_conc( aso4gas_idx, m ) = aerospc_conc( aso4gas_idx, m )
     &                                                 * fso4
                        aerospc_conc( aso4emis_idx,m ) = aerospc_conc( aso4emis_idx,m )
     &                                                 * fso4
                        aerospc_conc( aso4icbc_idx,m ) = aerospc_conc( aso4icbc_idx,m )
     &                                                 * fso4
                        If ( m .Eq. 2 ) Then

                           aerospc_conc( aso4aqh2o2_idx,m ) = aerospc_conc( aso4aqh2o2_idx,m )
     &                                                      * fso4
                           aerospc_conc( aso4aqo3_idx,  m ) = aerospc_conc( aso4aqo3_idx,  m )
     &                                                      * fso4
                           aerospc_conc( aso4aqfemn_idx,m ) = aerospc_conc( aso4aqfemn_idx,m )
     &                                                      * fso4
                           aerospc_conc( aso4aqmhp_idx, m ) = aerospc_conc( aso4aqmhp_idx, m )
     &                                                      * fso4
                           aerospc_conc( aso4aqpaa_idx, m ) = aerospc_conc( aso4aqpaa_idx, m )
     &                                                      * fso4
                        End If
                     End If
                  End Do
  
C update aerosol species in CGRID

                  Call update_aero ( cgrid( c,r,l,: ), .True. )

               End If
            End Do
         End Do

      End Do
      Return

      End Subroutine stm_wrap_gas

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      End Module stm_module
