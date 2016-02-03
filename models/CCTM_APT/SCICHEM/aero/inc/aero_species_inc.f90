!----------------------------------------------------------------------
! CMAQ 5.0 Compatible version
! PK, ENVIRON, August 2011
!----------------------------------------------------------------------

module aero_species_inc

use param_inc, only: MAX_MC
use cgrid_spcs, only: n_gc_spc, n_ae_spc, n_nr_spc, n_tr_spc, &
                      gc_spc, ae_spc, nr_spc, tr_spc, & 
                      gc_molwt, ae_molwt, nr_molwt, tr_molwt, &
                      n_gc_g2ae, n_nr_n2ae, n_tr_t2ae, & 
                      gc_g2ae, nr_n2ae, tr_t2ae, & 
                      gc_g2ae_map, nr_n2ae_map, tr_t2ae_map
use aero_data, only: N_AEROSPC, N_MODE, CONMIN, &
                     aerospc, aerospc_mw, aerospc_conc, &
                     aeromode, moment0_conc, moment2_conc, &
                     moment3_conc, apoc_idx, ah2o_idx, &
                     aeromode_minNum, aeromode_minM2
use precursor_data, only: N_PRECURSOR, &
                          precursor, precursor_mw, precursor_conc, &
!                         precursor_mw, precursor_conc, &
                          so4rate, sulprd_idx
use soa_defn, only: N_VAPOR, N_ORGPROD, &
                    vaporspc, vapor_mw, orgprod, orgprod_mw, &
                    vapor_conc, orgprod_conc, soa_aeroMap

implicit none
save

! Mapping for loading from and unloading to CONC array
! Note: these are different from mapping in CMAQ since species order
!       may be different. So declared separately here and not imported
!       from AERO_DATA module
Integer :: aerospc_map( N_AEROSPC,N_MODE )  ! indices of aero species to CONC
Integer :: aeronum_map( N_MODE )   ! indices of aero number variable to CONC
Integer :: aerosrf_map( N_MODE )   ! indices of aero surf area variable to CONC
Integer :: precursor_map( N_PRECURSOR )  ! indices of precursors to CONC
Integer :: vapor_map( N_VAPOR )     ! pointers of vapor species to CONC
Integer :: orgprod_map( N_ORGPROD ) ! pointers of orgprod species to CONC

! Below copied from PRECURSOR_DATA.F, since precursor is a PRIVATE variable there
! this version assumes precursor is public in precursor_data.F
!      Type precursor_type
!         Character( 16 ) :: name               ! species name
!         Logical         :: rxncounter         ! flag to reset counter species
!         Logical         :: update             ! update precursor concentration
                                               ! in cgrid after aeroproc
!      End Type precursor_type

!      Type ( precursor_type ), Save :: precursor( n_precursor ) = (/ &
!                       Name      RxnCounter  Update
!                    ----------   ----------  ------
!       precursor_type( 'NO2             ', .False. , .True.  ), &
!       precursor_type( 'N2O5            ', .False. , .True.  ), &
!       precursor_type( 'HNO3            ', .False. , .True.  ), &
!       precursor_type( 'HONO            ', .False. , .True.  ), &
!       precursor_type( 'ISOPRXN         ', .True.  , .True.  ), &
!       precursor_type( 'TRPRXN          ', .True.  , .True.  ), &
!       precursor_type( 'SULF            ', .False. , .True.  ), &
!       precursor_type( 'SULPRD          ', .True.  , .True.  ), &
!       precursor_type( 'HCL             ', .False. , .True.  ), &
!       precursor_type( 'ALK5RXN         ', .True.  , .True.  ), &
!       precursor_type( 'TOLNRXN         ', .True.  , .True.  ), &
!       precursor_type( 'TOLHRXN         ', .True.  , .True.  ), &
!       precursor_type( 'XYLNRXN         ', .True.  , .True.  ), &
!       precursor_type( 'XYLHRXN         ', .True.  , .True.  ), &
!       precursor_type( 'BNZNRXN         ', .True.  , .True.  ), &
!       precursor_type( 'BNZHRXN         ', .True.  , .True.  ), &
!       precursor_type( 'SESQRXN         ', .True.  , .True.  ), &
!       precursor_type( 'NH3             ', .False. , .True.  ), &
!       precursor_type( 'OH              ', .False. , .False. ) /)

end module aero_species_inc
