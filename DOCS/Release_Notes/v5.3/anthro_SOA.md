# Anthropogenic SOA reorganization in aero7

**Author/P.O.C.:**, [Momei Qin](mailto:qin.momei@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description
This update, available in aero7/7i, reduces the number of species required
to represent SOA from alkanes, benzene, toluene, xylene, and PAHs (naphthalene)
without changing any of the major model dependencies or magnitude of mass. High and low-NOx SOA yields 
are retained as well as previous oligomerization pathways. In general, the same underlying experimental datasets are
used, but yields are parameterized using a volatility basis set (VBS) instead of an Odum 2-product fit.
BTX and PAH yields are the same as those in Pye et al. (2010) and long-chain alkanes follow
the dodecane data of Presto et al. (2010). Properties (OM/OC, nC, molecular weights, 
solubilities, diffusivities, enthalpies of vaporization, LeBas molar volumes) of the VBS bins
were set to be consistent with what had been used in the Odum 2-product parameterizations
of aero6. Formation of the semivolatiles is performed in the gas-phase mechanism (note
that molar yields are implemented).  

New aerosol species (see [aero7 overview](aero7_overview.md) as well):  
AAVB1-4J  
SVAVB1-4  

Table 1: Mass-based SOA yields by C* (ug/m3)

|	Precursor		|	0.01	|	1	|	10	|	100	|
|	------	|		------	|	------	|	------	|	------	|
|	BENZ (high NOx)	|		0	|	0.078	|	0	|	0.793	|
|	BENZ (low NOx)	|		0.37	|	0	|	0	|	0	|
|	TOLU (high NOx)	|		0	|	0.032	|	0.094	|	0.08	|
|	TOLU (low NOx)	|		0.3	|	0	|	0	|	0	|
|	XYLE (high NOx)	|		0	|	0.025	|	0.036	|	0.09	|
|	XYLE (low NOx)	|		0.36	|	0	|	0	|	0	|
|	PAH (high NOx)	|		0	|	0.039	|	0.296	|	0.235	|
|	PAH (low NOx)	|		0.73	|	0	|	0	|	0	|
|	ALK (high NOx) 	|		0	|	0.014	|	0.11	|	0.16	|
            

## Significance and Impact
 Approximately the same OC mass with faster run-time.                      

## Affected Files
CCTM/src/aero/aero7 (linked to aero6)  
CCTM/src/MECHS/\*aero7\*                        
Wet and dry deposition  

## References

Albert A. Presto, Marissa A. Miracolo, Neil M. Donahue, and Allen L. Robinson, Secondary Organic Aerosol Formation from High-NOx Photo-Oxidation of Low Volatility Precursors: n-Alkanes, 
Environmental Science & Technology 2010 44 (6), 2029-2034, DOI: 10.1021/es903712r

Pye, H. O. T., Chan, A. W. H., Barkley, M. P., and Seinfeld, J. H.: Global modeling of organic aerosol: the importance of reactive nitrogen (NOx and NO3), Atmos. Chem. Phys., 10, 11261-11276, https://doi.org/10.5194/acp-10-11261-2010, 2010.                 

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #341]

#### Commit 
IDs:                        


-----

