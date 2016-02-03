!----------------------------------------------------------------------
! Version 1, February 2005, PK, AER, Inc
!
! Updated Feb 2008,  PK, AER, Inc.
! Updated for CMAQ 4.7.1 AE5 version, PK, ENVIRON, Nov 2010
! Updated for CMAQ 5, PK, ENVIRON, Aug 2011
!----------------------------------------------------------------------

module aqueous_species_inc

use param_inc, only: MAX_MC
use cgrid_spcs, only: n_gc_spc, n_ae_spc, n_nr_spc, n_tr_spc, &
                      gc_spc, ae_spc, nr_spc, tr_spc, & 
                      n_gc_g2aq, n_ae_a2aq, n_nr_n2aq, n_tr_t2aq, & 
                      gc_g2aq_map, ae_a2aq_map, nr_n2aq_map, tr_t2aq_map, &
                      n_gc_scav, n_ae_scav, n_nr_scav, n_tr_scav, &
                      gc_scav, ae_scav, nr_scav, tr_scav, &
                      gc_scav_map, ae_scav_map, nr_scav_map, tr_scav_map, &
                      gc_scav_fac, ae_scav_fac, nr_scav_fac, tr_scav_fac
use aq_data, cgrid2aq_map_host => cgrid2aq_map
use aero_data, only:  N_MODE, aerospc, aeromode

implicit none

end module aqueous_species_inc
