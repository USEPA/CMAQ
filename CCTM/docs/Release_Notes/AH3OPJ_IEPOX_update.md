# Changes to acidity and epoxide uptake properties

**Author/P.O.C.:**, [Havala Pye](mailto:pye.havala@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

Formation of SOA from epoxides is acid catalyzed. The molecular weight of AH3OP (the hydronium ion) is needed to convert the concentration of H+ predicted by ISORROPIA in mol/m3 to micrograms/m3. The molecular weight of AH3OP was incorrectly being obtained from PRECURSOR_MW array instead of AEROSPC_MW in aero_subs.F.
As a result, the molecular weight was a factor of 10 higher than it should be. Correcting this array reference resulted in decrease in acidity (AH3OPJ+AH3OPI) which degraded predictions of IEPOX SOA for the SOAS 2013 campaign. AH3OPK was not affected.

To improve model performance, we implemented a Henry's law coefficient for IEPOX of 3.0e<sup>7</sup> M/atm (instead of the former 2.7e<sup>6</sup> M/atm) based on Nguyen et al. (2014) (laboratory experiments on NaCl particles). This improved the model predictions of 2-methyltetrols, a known IEPOX-derived organic aerosol species.


## Significance and Impact

IEPOX-derived SOA is estimated to increase by only 3% compared to before this update as a result of compensating modifications. See Pye et al. (2017) for performance of IEPOX SOA and 2-methyltetrols in CMAQ with this update.

## Affected Files

aero/aero6/aero_subs.F  
cloud/acm_ae6/hlconst.F  

## References

Nguyen, T. B., Coggon, M. M., Bates, K. H., Zhang, X., Schwantes, R. H., Schilling, K. A., Loza, C. L., Flagan, R. C., Wennberg, P. O., and Seinfeld, J. H.: Organic aerosol formation from the reactive uptake of isoprene epoxydiols (IEPOX) onto non-acidified inorganic seeds, Atmos. Chem. Phys., 14, 3497-3510, doi:10.5194/acp-14-3497-2014, 2014. [link](http://www.atmos-chem-phys.net/14/3497/2014/)

Pye, H. O. T., Murphy, B. N., Xu, L., Ng, N. L., Carlton, A. G., Guo, H., Weber, R., Vasilakos, P., Appel, K. W., Budisulistiorini, S. H., Surratt, J. D., Nenes, A., Hu, W., Jimenez, J. L., Isaacman-VanWertz, G., Misztal, P. K., and Goldstein, A. H.: On the implications of aerosol liquid water and phase separation for organic aerosol mass, Atmos. Chem. Phys., 17, 343-369, doi:10.5194/acp-17-343-2017, 2017. [link](http://www.atmos-chem-phys.net/17/343/2017/)

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #105](https://github.com/usepa/cmaq_dev/pull/105)


#### Commit IDs:
ce109c9bd1a4b1d3c730b002f489c9a907ab9c71  
7e0c1d8c0645559c0773c7c222d70acb240a2896  


-----
