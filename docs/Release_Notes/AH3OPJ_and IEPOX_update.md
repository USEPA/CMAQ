# Changes to acidity for epoxide uptake

**Author/P.O.C.:**, [Havala Pye](mailto:pye.havala@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

Formation of SOA from epoxides is acid catalyzed. The molecular weight of AH3OP (the hydronium ion) is needed to convert the concentration of H+ predicted by ISORROPIA in mol/m3 to micrograms/m3. The molecular weight of AH2OP was incorrectly being obtained from PRECURSOR_MW array instead of AEROSPC_MW in aero_subs.F.
As a result, the molecular weight (species 18 in aerospc) is being set to that of species 18 in precursor_mw (SESQRXN). This is a factor of 10 higher than it should be. Correcting this reference should result in a factor of 10 decrease in acidity (AH3OP) which degrades prediction of IEPOX SOA for the SOAS 2013 campaign.

Considerable uncertainty exists in the Henry's law coefficient for IEPOX

|H (M/atm)       |Reference      |              How it was determined|
|---|---|---|
|2.7e6              | Pye et al. 2013      |      HenryWin|
|3.0e7               |Nguyen et al. 2014    | Laboratory, on NaCl aqueous particles|
|3.3e7              | Marais et al. 2016    |   Fitted value to reproduce SEAC4RS and SOAS data|
|1.7e8 | Gaston et al. 2014 |  Laboratory |

To improve model performance, we recommend a Henry's law coefficient for IEPOX of 3.0e7 M/atm based on Nguyen et al. 2014 (laboratory experiments on NaCl particles). This improves the model predictions of 2-methyltetrols, a known IEPOX-derived organic aerosol species. The plot below shows particle-phase 2-methyltetrols measured by Gabriel Isaacman-VanWertz (formerly Goldstein Group, Berkeley) in black, CMAQ predictions before the H-law increase in red, and CMAQ predictions with the larger 3e7 M/atm H-law for IEPOX in green. Simulations/observations are for June 2013 SOAS (eastern US domain) at the Centreville, AL site.
See Pye et al., 2016 for performance of IEPOX SOA in CMAQ with this update.

## Significance and Impact

Minor changes in IEPOX-derived SOA as a result of compensating corrections.

## Affected Files

aero/aero6/aero_subs.F
cloud/acm_ae6/hlconst.F

## References

Pye, H. O. T., Murphy, B. N., Xu, L., Ng, N. L., Carlton, A. G., Guo, H., Weber, R., Vasilakos, P., Appel, K. W., Budisulistiorini, S. H., Surratt, J. D., Nenes, A., Hu, W., Jimenez, J. L., Isaacman-VanWertz, G., Misztal, P. K., and Goldstein, A. H.: On the implications of aerosol liquid water and phase separation for organic aerosol mass, Atmos. Chem. Phys. Discuss., doi:10.5194/acp-2016-719, in review, 2016. 

-----
## Internal Records:
#### Relevant Pull Requests:



#### Commit IDs:


-----
