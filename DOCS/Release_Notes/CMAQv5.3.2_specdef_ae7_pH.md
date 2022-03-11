# Optional Diagnostic Information on Fine Aerosol Acidity

[Havala O. T. Pye](mailto:pye.havala@epa.gov), U.S. Environmental Protection Agency

## Brief Description
Fine aerosol pH is an important property of particles containing liquid water. Lines have been added to the ae7/7i species definition files to calculate fine aerosol acidity. The value reported corresponds to free H+ on the molality scale (pH<sub>F</sub>, see Pye et al., 2020). When aerosol liquid water is less than 0.01 &mu;g m<sup>-3</sup>, pH<sub>F</sub> should not be trusted and is replaced by a large negative number. The lines must be uncommented to active.

## Significance and Impact
Allows for additional diagnostic information useful for model evaluation, understanding chemical regimes, and analysis.

## Affected Files
CCTM/src/MECHS/cb6r3_ae7_aq/SpecDef_cb6r3_ae7_aq.txt                  
CCTM/src/MECHS/saprc07tic_ae7i_aq/SpecDef_saprc07tic_ae7_aq.txt                  
CCTM/src/MECHS/cb6r3m_ae7_kmtbr/SpecDef_cb6r3m_ae7_kmtbr.txt                  

## References
Pye, H. O. T., Nenes, A., Alexander, B., Ault, A. P., Barth, M. C., Clegg, S. L., Collett Jr., J. L., Fahey, K. M., Hennigan, C. J., Herrmann, H., Kanakidou, M., Kelly, J. T., Ku, I.-T., McNeill, V. F., Riemer, N., Schaefer, T., Shi, G., Tilgner, A., Walker, J. T., Wang, T., Weber, R., Xing, J., Zaveri, R. A., and Zuend, A.: The acidity of atmospheric particles and clouds, *Atmos. Chem. Phys. Discuss.*, https://doi.org/10.5194/acp-2019-889, accepted, 2020.

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #628](https://github.com/USEPA/CMAQ_Dev/pull/628)

#### Commit 
IDs:                        


-----

