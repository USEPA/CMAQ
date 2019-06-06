
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
C                 init_stm_pntrs

C  Revision History:
C SR 12/13/2018 Initial version

C----------------------------------------------------------------------
      Use grid_conf, Only: ncols, nrows, nlays
      Use aero_data, Only : aso4_idx, aso4aqh2o2_idx, aso4aqo3_idx,
     &                      aso4aqfemn_idx, aso4aqmhp_idx, aso4aqpaa_idx,
     &                      aso4gas_idx, aso4emis_idx, aso4icbc_idx,
     &                      aerospc_map, n_mode, map_aero


      Logical, Private, Save :: initialized = .False.

      Integer, Private, Save :: vso4 ( n_mode )
      Integer, Private, Save :: vso4g( n_mode )
      Integer, Private, Save :: vso4e( n_mode )
      Integer, Private, Save :: vso4b( n_mode )
      Integer, Private, Save :: vso4ah2o2
      Integer, Private, Save :: vso4ao3
      Integer, Private, Save :: vso4afemn
      Integer, Private, Save :: vso4amhp 
      Integer, Private, Save :: vso4apaa

      Contains

C-----------------------------------------------------------------------
      Subroutine stm_wrap_ae ( cgrid, jdate, jtime )

C  capture changes from aero proc (changes in aso4 from new particle
C     formation and condensation)

C  Revision History:
C     Initial version - 6/5/19 - Shawn Roselle
C     
C-----------------------------------------------------------------------
      Use runtime_vars, Only: adj_stmspc
      Use aero_budget, Only: aero_cond, aero_npf

      Implicit None

C Arguments

      Real, Pointer :: cgrid( :,:,:,: )

      Integer, Intent( In ) :: jdate
      Integer, Intent( In ) :: jtime

C Local variables

      Logical, Save :: firstime = .True.

      Integer c, r, l, m

      Real aso4tot
      Real mscor

      If ( firstime ) Then
         firstime = .False.
         Call init_stm_pntrs()
      End If

      Do m = 1, n_mode
         cgrid( :,:,:,vso4g( m ) ) = cgrid( :,:,:,vso4g( m ) )
     &                             + aero_cond( :,:,:,vso4( m ) )
     &                             + aero_npf ( :,:,:,vso4( m ) )
      End Do

C normalize tracked sulfate species to total modeled sulfate

      If ( adj_stmspc ) Then

         Do c = 1, ncols
            Do r = 1, nrows
               Do l = 1, nlays
                  Do m = 1, n_mode
         
                     aso4tot = cgrid( c,r,l,vso4g( m ) )
     &                       + cgrid( c,r,l,vso4e( m ) )
     &                       + cgrid( c,r,l,vso4b( m ) )

C...  for the accumulation mode, add aqueous tracked species

                     if ( m .eq. 2 ) then
                        aso4tot = aso4tot
     &                          + cgrid( c,r,l,vso4ah2o2 )
     &                          + cgrid( c,r,l,vso4ao3 )
     &                          + cgrid( c,r,l,vso4afemn ) 
     &                          + cgrid( c,r,l,vso4amhp )
     &                          + cgrid( c,r,l,vso4apaa )
                     End If

C...  normalize sulfur tracking species

                     If ( aso4tot .Gt. 0.0 ) Then
                        mscor = cgrid( c,r,l,vso4( m ) ) / aso4tot
                        cgrid( c,r,l,vso4g( m ) ) = cgrid( c,r,l,vso4g( m ) )
     &                                            * mscor
                        cgrid( c,r,l,vso4e( m ) ) = cgrid( c,r,l,vso4e( m ) )
     &                                            * mscor
                        cgrid( c,r,l,vso4b( m ) ) = cgrid( c,r,l,vso4b( m ) )
     &                                            * mscor
                        if ( m .eq. 2 ) then
                           cgrid( c,r,l,vso4ah2o2 ) = cgrid( c,r,l,vso4ah2o2 )
     &                                              * mscor
                           cgrid( c,r,l,vso4ao3 )   = cgrid( c,r,l,vso4ao3 )
     &                                              * mscor
                           cgrid( c,r,l,vso4afemn ) = cgrid( c,r,l,vso4afemn )
     &                                              * mscor
                           cgrid( c,r,l,vso4amhp )  = cgrid( c,r,l,vso4amhp )
     &                                              * mscor
                           cgrid( c,r,l,vso4apaa )  = cgrid( c,r,l,vso4apaa )
     &                                              * mscor
                        End If
                     End If
         
                  End Do
               End Do
            End Do
         End Do

      End If
      Return
      
      End Subroutine stm_wrap_ae


C-----------------------------------------------------------------------
      Subroutine stm_wrap_gas ( cgrid, jdate, jtime, after )

C  capture changes in aso4 from gas phase chemistry
C  in the current release, ASO4 can be lost in gas chem to form organo-sulfate
C
C     Initial version - 6/5/19 - Shawn Roselle
C-----------------------------------------------------------------------

      Use utilio_defn, Only: xstat1

      Implicit None

C Arguments

      Real, Pointer :: cgrid( :,:,:,: )

      Integer, Intent( In ) :: jdate
      Integer, Intent( In ) :: jtime
      Logical, Intent( In ) :: after

C local variables

      Character( 16 ) :: pname = 'stm_wrap_gas'
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

         Call init_stm_pntrs()

      End If

C save ASO4 concentrations before call to CHEM

      If ( .not. after ) Then
  
         Do m = 1, n_mode
            so4b4( :,:,:,m ) = cgrid( :,:,:,vso4( m ) )
         End Do

C scale sulfur tracking species by change in ASO4

      Else
  
         Do c = 1, ncols
            Do r = 1, nrows
               Do l = 1, nlays
  
                  Do m = 1, n_mode
                     fso4 = cgrid( c,r,l,vso4( m ) ) / so4b4( c,r,l,m )
                     If ( so4b4( c,r,l,m ) .Gt. 0.0 ) Then
                        cgrid( c,r,l,vso4g( m ) ) = cgrid( c,r,l,vso4g( m ) )
     &                                            * fso4
                        cgrid( c,r,l,vso4e( m ) ) = cgrid( c,r,l,vso4e( m ) )
     &                                            * fso4
                        cgrid( c,r,l,vso4b( m ) ) = cgrid( c,r,l,vso4b( m ) )
     &                                            * fso4
                        if ( m .eq. 2 ) then
                           cgrid( c,r,l,vso4ah2o2 ) = cgrid( c,r,l,vso4ah2o2 )
     &                                              * fso4
                           cgrid( c,r,l,vso4ao3 )   = cgrid( c,r,l,vso4ao3 )
     &                                              * fso4
                           cgrid( c,r,l,vso4afemn ) = cgrid( c,r,l,vso4afemn )
     &                                              * fso4
                           cgrid( c,r,l,vso4amhp )  = cgrid( c,r,l,vso4amhp )
     &                                              * fso4
                           cgrid( c,r,l,vso4apaa )  = cgrid( c,r,l,vso4apaa )
     &                                              * fso4
                        End If
                     End If
                  End Do
  
               End Do
            End Do
         End Do

      End If
      Return

      End Subroutine stm_wrap_gas


C-----------------------------------------------------------------------
      Subroutine init_stm_pntrs()

C  initialize stm species pointers
 
C  Revision History:
C     Initial version 6/5/19 Shawn Roselle
C     
C-----------------------------------------------------------------------

      Implicit None

C Local variables:

      Character( 16 ) :: pname = 'init_stm_pntrs'

      Integer m


C-----------------------------------------------------------------------
C  begin body of subroutine init_stm_pntrs

      If ( initialized ) Return

      Call map_aero()

      Do m = 1, n_mode
         vso4 ( m ) = aerospc_map( aso4_idx,m )
         vso4g( m ) = aerospc_map( aso4gas_idx,m )
         vso4e( m ) = aerospc_map( aso4emis_idx,m )
         vso4b( m ) = aerospc_map( aso4icbc_idx,m )

C...  set pointers to species that only appear in the accumulation mode

         If ( m .eq. 2 ) Then
            vso4ah2o2 = aerospc_map( aso4aqh2o2_idx,m )
            vso4ao3   = aerospc_map( aso4aqo3_idx,m )
            vso4afemn = aerospc_map( aso4aqfemn_idx,m ) 
            vso4amhp  = aerospc_map( aso4aqmhp_idx,m )
            vso4apaa  = aerospc_map( aso4aqpaa_idx,m )
         End If

      End Do

      initialized = .True.
      Return

      End Subroutine init_stm_pntrs

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      End Module stm_module
