
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

C  

C  Contains:
C     Subroutine adj_sulftrk

C  Revision History:
C SR 12/13/2018 Initial version

C References:
C----------------------------------------------------------------------
      USE AERO_DATA, only : coag_budget, cond_budget, growth_budget, npf_budget,
     &                      aerospc_conc, aso4aqh2o2_idx, aso4aqo3_idx,
     &                      aso4aqfemn_idx, aso4aqmhp_idx, aso4aqpaa_idx,
     &                      aso4gas_idx, aso4emis_idx, aso4icbc_idx,
     &                      aso4_idx, aerospc_map, n_mode

      Contains

C-----------------------------------------------------------------------
      Subroutine adj_sulftrk()

C  Adjust sulfur tracking species to total sulfur prediction
 
C  Revision History:
C     Initial version coded in July 2013 by Shawn Roselle
C     
C-----------------------------------------------------------------------

      USE RUNTIME_VARS, ONLY: ADJ_STMSPC

      Implicit None

C Local variables:
      Integer m
      Real aso4trk( n_mode ), mscor( n_mode )

      If ( ADJ_STMSPC ) Then
      
C Normalize tracked sulfate species to total modeled sulfate

         m = 1
         aso4trk( m ) = aerospc_conc( aso4gas_idx,m )
     &                + aerospc_conc( aso4emis_idx,m )
     &                + aerospc_conc( aso4icbc_idx,m )
         
         If ( aso4trk( m ) .GT. 0.0 ) Then
            mscor( m ) = aerospc_conc( aso4_idx,m ) / aso4trk( m )
            aerospc_conc( aso4gas_idx,m  ) = aerospc_conc( aso4gas_idx,m  ) * mscor( m )
            aerospc_conc( aso4emis_idx,m ) = aerospc_conc( aso4emis_idx,m ) * mscor( m )
            aerospc_conc( aso4icbc_idx,m ) = aerospc_conc( aso4icbc_idx,m ) * mscor( m )
         End If
         
         m = 2
         aso4trk( m ) = aerospc_conc( aso4aqh2o2_idx,m )
     &                + aerospc_conc( aso4aqo3_idx,m )
     &                + aerospc_conc( aso4aqfemn_idx,m ) 
     &                + aerospc_conc( aso4aqmhp_idx,m )
     &                + aerospc_conc( aso4aqpaa_idx,m )
     &                + aerospc_conc( aso4gas_idx,m )
     &                + aerospc_conc( aso4emis_idx,m )
     &                + aerospc_conc( aso4icbc_idx,m )
         
         If ( aso4trk( m ) .GT. 0.0 ) Then
            mscor( m ) = aerospc_conc( aso4_idx,m ) / aso4trk( m )
            aerospc_conc( aso4aqh2o2_idx,m ) = aerospc_conc( aso4aqh2o2_idx,m ) * mscor( m )
            aerospc_conc( aso4aqo3_idx,m   ) = aerospc_conc( aso4aqo3_idx,m ) * mscor( m )
            aerospc_conc( aso4aqfemn_idx,m ) = aerospc_conc( aso4aqfemn_idx,m ) * mscor( m )
            aerospc_conc( aso4aqmhp_idx,m  ) = aerospc_conc( aso4aqmhp_idx,m  ) * mscor( m )
            aerospc_conc( aso4aqpaa_idx,m  ) = aerospc_conc( aso4aqpaa_idx,m  ) * mscor( m )
            aerospc_conc( aso4gas_idx,m    ) = aerospc_conc( aso4gas_idx,m ) * mscor( m )
            aerospc_conc( aso4emis_idx,m   ) = aerospc_conc( aso4emis_idx,m ) * mscor( m )
            aerospc_conc( aso4icbc_idx,m   ) = aerospc_conc( aso4icbc_idx,m ) * mscor( m )
         End If
         
         m = 3
         aso4trk( m ) = aerospc_conc( aso4gas_idx,m )
     &                + aerospc_conc( aso4emis_idx,m )
     &                + aerospc_conc( aso4icbc_idx,m )
         
         If ( aso4trk( m ) .GT. 0.0 ) Then
            mscor( m ) = aerospc_conc( aso4_idx,m ) / aso4trk( m )
            aerospc_conc( aso4gas_idx,m  ) = aerospc_conc( aso4gas_idx,m  ) * mscor( m )
            aerospc_conc( aso4emis_idx,m ) = aerospc_conc( aso4emis_idx,m ) * mscor( m )
            aerospc_conc( aso4icbc_idx,m ) = aerospc_conc( aso4icbc_idx,m ) * mscor( m )
         End If
         
      End If ! ADJ_STMSPC = .TRUE.

      End Subroutine adj_sulftrk


C-----------------------------------------------------------------------
      Subroutine trk_so4_npf_cond()

C  track change in aso4 from new particle formation and condensation
 
C  Revision History:
C     Initial version - 12/18/18 - Shawn Roselle
C     
C-----------------------------------------------------------------------

      Implicit None

C Local variables:
      Integer m

      do m=1,n_mode
        aerospc_conc( aso4gas_idx,m  ) = aerospc_conc( aso4gas_idx,m  )
     &                                 + cond_budget( aerospc_map( aso4_idx,m ) )
     &                                 + npf_budget( aerospc_map( aso4_idx,m ) )
      end do

      End Subroutine trk_so4_npf_cond

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      End Module stm_module
