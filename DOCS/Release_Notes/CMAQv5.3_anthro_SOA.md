# Anthropogenic SOA reorganization in *aero7*

Momei Qin and [Havala Pye](mailto:pye.havala@epa.gov), U.S. Environmental Protection Agency

## Brief Description
This update, available in *aero7/7i*, reduces the number of species required
to represent SOA from alkanes, benzene, toluene, xylene, and PAHs (naphthalene)
without changing any of the major model dependencies or magnitude of mass. High- and low-NO<sub>x</sub> SOA yields 
are retained, as well as previous oligomerization pathways. In general, the same underlying experimental datasets are
used, but yields are parameterized using a volatility basis set (VBS) instead of an Odum 2-product fit.
BTX and PAH yields are the same as those in Pye et al. (2010) and long-chain alkanes follow
the dodecane data of Presto et al. (2010). Properties (OM/OC, nC, molecular weights, 
solubilities, diffusivities, enthalpies of vaporization, LeBas molar volumes) of the VBS bins
were set to be consistent with what had been used in the Odum 2-product parameterizations
of *aero6*. Formation of the semivolatiles is performed in the gas-phase mechanism, but note
that molar yields are implemented. Documentation appears in Qin et al., *in prep*. 

New aerosol species (also see [aero7 overview](CMAQv5.3_aero7_overview.md)):  
AAVB1-4J  
SVAVB1-4  

Table 1: Mass-based SOA yields by C* (&#956;g&nbsp;m<sup>&#8209;3</sup>)

|	Precursor		|	0.01	|	1	|	10	|	100	|
|	------	|		------	|	------	|	------	|	------	|
|	BENZ (high NO<sub>x</sub>)	|		0	|	0.078	|	0	|	0.793	|
|	BENZ (low NO<sub>x</sub>)	|		0.37	|	0	|	0	|	0	|
|	TOLU (high NO<sub>x</sub>)	|		0	|	0.032	|	0.094	|	0.08	|
|	TOLU (low NO<sub>x</sub>)	|		0.3	|	0	|	0	|	0	|
|	XYLE (high NO<sub>x</sub>)	|		0	|	0.025	|	0.036	|	0.09	|
|	XYLE (low NO<sub>x</sub>)	|		0.36	|	0	|	0	|	0	|
|	PAH (high NO<sub>x</sub>)	|		0	|	0.039	|	0.296	|	0.235	|
|	PAH (low NO<sub>x</sub>)	|		0.73	|	0	|	0	|	0	|
|	ALK (high NO<sub>x</sub>) 	|		0	|	0.014	|	0.11	|	0.16	|
            

## Significance and Impact
 Approximately the same OC mass with faster run-time.                      

## Affected Files
CCTM/src/aero/aero7 (linked to *aero6*)  
CCTM/src/MECHS/\*aero7\*                        
Wet and dry deposition  

## References

Presto, A. A., Miracolo, M. A., Donahue, N. M., and Robinson, A. L.: Secondary organic aerosol formation from high-NO<sub>x</sub> photo-oxidation of low volatility precursors: n-Alkanes, 
*Environmental Science & Technology*, **44**, 2029-2034, https://doi.org/10.1021/es903712r, 2010.

Pye, H. O. T., Chan, A. W. H., Barkley, M. P., and Seinfeld, J. H.: Global modeling of organic aerosol: the importance of reactive nitrogen (NO<sub>x</sub> and NO<sub>3</sub>), *Atmospheric Chemistry and Physics*, **10**, 11261-11276, https://doi.org/10.5194/acp-10-11261-2010, 2010.    

Qin, M., Murphy, B. N., McDonald, B. C., McKeen, S. A., Koval, L., Isaacs, K., Lu, Q., Robinson, A. L., Strum, M., Snyder, J., Efstathious, C., Allen, C., and Pye, H. O. T.: The estimated impacts of volatile chemical products on particulate matter and ozone criteria pollutants in an urban atmosphere, in preparation.

-----
## Internal Records
#### Relevant Pull Requests:
[PR #341]

#### Commit 
IDs:                        


-----

