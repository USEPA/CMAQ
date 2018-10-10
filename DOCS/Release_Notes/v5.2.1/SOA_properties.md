# Update to SOA properties

**Author/P.O.C.:**, [Havala Pye](mailto:pye.havala@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

Several properties of semivolatile SOA species (aerosol and nonreactives) were updated: number of carbon atoms per molecule, density, organic-mass-to-organic-carbon (OM/OC) ratio (now in species definition file), and molecular weights. In addition, unique dry and wet deposition surrogates were implemented for each semivolatile species. See Pye et al. (2017) for how updated properties were determined.

## Significance and Impact

+ Up to 7% decrease in OM and 10% decrease in OC in the Eastern US for June 2013 conditions.
+ The largest impact was due to the updated OM/OC. 
+ Deposition updates play a relatively minor role.

## Affected Files

MECHS/\*/AE\*.nml  
MECHS/\*/NR\*.nml  
aero/aero6/AERO_DATA.F  
aero/aero6/SOA_DEFN.F  
cloud/acm_ae6/hlconst.F  
depv/m3dry/DEPVVARS.F  
vdiff/acm2/ASX_DATA_MOD.F  

For saprc07tic:  
gas/ebi\*/hrprodloss.F  
MECHS/\*/mech.def  


## References

Pye, H. O. T., Murphy, B. N., Xu, L., Ng, N. L., Carlton, A. G., Guo, H., Weber, R., Vasilakos, P., Appel, K. W., Budisulistiorini, S. H., Surratt, J. D., Nenes, A., Hu, W., Jimenez, J. L., Isaacman-VanWertz, G., Misztal, P. K., and Goldstein, A. H.: On the implications of aerosol liquid water and phase separation for organic aerosol mass, Atmos. Chem. Phys., 17, 343-369, doi:10.5194/acp-17-343-2017, 2017. [link](http://www.atmos-chem-phys.net/17/343/2017/)

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #13](https://github.com/usepa/cmaq_dev/pull/13)   
[PR #11](https://github.com/usepa/cmaq_dev/pull/11)  
[PR #75](https://github.com/usepa/cmaq_dev/pull/75)  
[PR #64](https://github.com/usepa/cmaq_dev/pull/64)  
[PR #36](https://github.com/usepa/cmaq_dev/pull/36)  

#### Commit IDs:

8f3bd301099c354350a3d152088c9f2fb961c720  
dfe134ae37240de92fc19292e2ce98306742b984  
dc9a180de6d3452db7852dab7007d1a2a38d8ad5  
77df3e978480eecdea4081599fee89a2a98c597f  
5db1fa3af2f49c9dc382f45593a7657fbb964abf  
e246de9d80a9076575a3d4a83071ed4edeca4a56  
879f22428b8932cd0113c12f42acead44a9bdf1c  
86e391e47dc1954bab5e270c0d475967734d8571  
156df955d4cdd68fec10444f39c7b7a53c834a32  
ac7593063a5227670667786ec0d8bdcacd10175e  
a95165c0d4b0c4d4dbbc5df051a081f217c2910c  

-----
