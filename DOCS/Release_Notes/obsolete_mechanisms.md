# Removal of obsolete mechanisms in CMAQv5.3

[Deborah Luecken](mailto:luecken.deborah@epa.gov), U.S. Environmental Protection Agency

## Brief Description

CMAQv5.3 does not include several mechanisms and variants of mechanisms that were available in CMAQv5.2.1; see Table 1. As older mechanisms are replaced with updated versions in CMAQ, we recommend that users migrate to mechanisms that are more consistent with our current understanding of atmospheric chemistry and the types of applications for which air quality models are applied. 

In most cases, users who still want to use mechanisms that have not been retained in CMAQv5.3 can use the released mechanisms as a guide to implement those earlier mechanisms into CMAQv5.3. The mechanisms then can be recreated using the EBI solver, or CMAQ can be run with Rosenbrock.  


Table 1. Mechanisms removed in CMAQv5.3.

|Mechanism Name| Notes |                                  
| ----------------------- | ------------------------------------------------------- |
|cb05e51_ae6_aq | superceded by CB6r3_ae6_aq and CB6r3_ae7_aq |
|cb05e51_ae6nvPOA_aq | superceded by CB6r3_ae6_aq and CB6r3_ae7_aq |
|cb05he51_ae6_aq | superceded by CB6r3m_ae7_kmtbr |
|cb05mp51_ae6_aq | superceded by CB6mp_ae6_aq |
|cb05tucl_ae6_aq | superceded by CB6r3_ae6_aq and CB6r3_ae7_aq|
|cb6r3_ae6nvPOA_aq | non-volatile option available in CB6r3_ae6_aq |
|saprc07tb_ae6_aq | functionality available in saprc07tc_ae6_aq|
|saprc07tc_ae6nvPOA_aq | non-volatile option available  in saprc07tc_ae6_aq|
|saprc07tic_ae6invPOA_aq | non-volatile option available in saprc07tic_ae6i_aq|


## Files Affected
None

## References
None

## Internal Records
None


