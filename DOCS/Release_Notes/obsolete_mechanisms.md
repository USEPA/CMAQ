# Removal of obsolete mechanisms in CMAQv5.3

**Author/P.O.C.:** [Deborah Luecken](mailto:luecken.deborah@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

Several mechanisms and variations of mechanisms previously available in CMAQv5.2, are not included in CMAQv5.3. 
As older mechanisms are replaced with updated versions, we recommend that users migrate to mechanisms that are more consistent with our current understanding of atmospheric chemistry and the types of applications for which AQMs are applied.  

Table 1. Mechanisms obsoleted in CMAQv5.3 

|Mechanism Name| Notes |                                  
| ----------------------- | ------------------------------------------------------- |
|cb05e51_ae6_aq | superceded by CB6r3_ae6_aq and CB6r3_ae7_aq |
|cb05e51_ae6nvPOA_aq | superceded by CB6r3_ae6_aq and CB6r3_ae7_aq |
|cb05he51_ae6_aq | superceded by CB6r3m_ae7_kmtbraq |
|cb05mp51_ae6_aq | superceded by CB6mp_ae6_aq |
|cb05tucl_ae6_aq | superceded by CB6r3_ae6_aq and CB6r3_ae7_aq|
|cb6r3_ae6nvPOA_aq | non-volatile option available in CB6r3_ae6_aq |
|saprc07tb_ae6_aq | functionality available in saprc07tc_ae6_aq|
|saprc07tc_ae6nvPOA_aq | non-volatile option available  in saprc07tc_ae6_aq|
|saprc07tic_ae6invPOA_aq | non-volatile option available in saprc07tic_ae6i_aq|


Users that want earlier mechanisms can, in many cases, extend the earlier mechanism for compatibility with CMAQv5.3 by using released mechanisms as a guide for modifying their mechanisms, and either recreating the ebi solver or running CMAQ with Rosenbrock.  

## Files Affected
None

## References

## Internal Records:
None


