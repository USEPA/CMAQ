# Glyoxal and methylglyoxal SOA updates

**Author/P.O.C.:**, [Havala Pye](mailto:pye.havala@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

In CMAQ v5.1, only saprc07tic_aero6i allowed for the formation of glyoxal (GLY) and methylglyoxal (MGLY) SOA in aqueous accumulation mode particles (AGLYJ).
 SOA from GLY+MGLY uptake onto particles was added to CB6. In addition, the uptake
coefficient of MGLY was updated by scaling the uptake coefficient for GLY using the Henry's law coefficients to reflect the lower solubility of MGLY than GLY. This was implemented in Pye et al. (2017).

## Significance and Impact

AGLYJ available in the new CB6 mechanism and updated in saprc07tic mechanisms. SOA due to MGLY uptake onto J mode particles is 9% of its former magnitude in saprc07tic.

## Affected Files

aero/aero6/AEROSOL_CHEMISTRY.F  
aero/aero6/AERO_DATA.F  
MECHS/cb6r3_ae6_aq/mech\*.def  
MECHS/cb6r3_ae6_aq/RXNS_DATA_MODULE.F90  
MECHS/cb6r3_ae6_aq/RXNS_FUNC_MODULE.F90  
gas/ebi_cb6r3_ae6_aq/\*.F  
MECHS/saprc07tic_ae6_aq/mech\*.def  
MECHS/saprc07tic_ae6i_aq/RXNS_DATA_MODULE.F90  
MECHS/saprc07tic_ae6i_aq/RXNS_FUNC_MODULE.F90  
gas/ebi_saprc07tic_ae6i_aq/\*.F  


## References

Pye, H. O. T., Murphy, B. N., Xu, L., Ng, N. L., Carlton, A. G., Guo, H., Weber, R., Vasilakos, P., Appel, K. W., Budisulistiorini, S. H., Surratt, J. D., Nenes, A., Hu, W., Jimenez, J. L., Isaacman-VanWertz, G., Misztal, P. K., and Goldstein, A. H.: On the implications of aerosol liquid water and phase separation for organic aerosol mass, Atmos. Chem. Phys., 17, 343-369, doi:10.5194/acp-17-343-2017, 2017. [link](http://www.atmos-chem-phys.net/17/343/2017/)

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #78](https://github.com/usepa/cmaq_dev/pull/78)  


#### Commit IDs:
1dfd9e3b10034dde2f43add22aed1d73cb128711  
60099f8a4014cd889b6f2af9ca2e97283895dafc  


-----
