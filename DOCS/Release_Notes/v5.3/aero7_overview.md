# AERO7/7i

**Author/P.O.C.:**, [Havala O. T. Pye](mailto:pye.havala@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description
                       
Aero7/7i builds on the aero6 module. Updates in aero7/7i include: 
improved consistency in terms of secondary organic aerosol (SOA)
between carbon bond and saprc-based mechanisms, 
updated monoterpene SOA from photooxidation (OH and ozone), uptake
of water onto hydrophillic organics, and reorganization of anthropogenic SOA
species. All of the science updates are accomplished with a reduction
in the number of species needed and thus accomplishes more
robust predictions in less time than aero6. 

### Species updates

![Schematic](aero7.jpg) 
Figure 1: Organic aerosol treatment in CMAQv5.3-aero7. Note that primary organic aerosol system is the same as v5.2 (Murphy et al. 2017) and abbreviated in this schematic. Species in red are new to aero7.

Species introduced in aero7 (new compared to aero6):

|					|	Species			|	Phase			|	Description		|	Scientific Basis	|	Model Implementation	|
|	-------------	|	-------------	|	-------------	|	-------------	|	-------------		|	-------------	|
|	1	|	AMT1J	|	particle	|	low volatility particulate matter from monoterpene photoxidation (OH and O3 reaction), C*=0.01 ug/m3	|	dark a-pinene ozonolysis (Saha and Grieshop 2016 ES&T)	|	Xu et al. 2018 ACP	|
|	2	|	AMT2J	|	particle	|	low volatility particulate matter from monoterpene photoxidation (OH and O3 reaction), C*=0.1 ug/m3	|	dark a-pinene ozonolysis (Saha and Grieshop 2016 ES&T)	|	Xu et al. 2018 ACP	|
|	3	|	AMT3J	|	particle	|	semivolailte particulate matter from monoterpene photoxidation (OH and O3 reaction), C*=1 ug/m3	|	dark a-pinene ozonolysis (Saha and Grieshop 2016 ES&T)	|	Xu et al. 2018 ACP	|
|	4	|	AMT4J	|	particle	|	semivolatile particulate matter from monoterpene photoxidation (OH and O3 reaction), C*=10 ug/m3	|	dark a-pinene ozonolysis (Saha and Grieshop 2016 ES&T)	|	Xu et al. 2018 ACP	|
|	5	|	AMT5J	|	particle	|	semivolatile particulate matter from monoterpene photoxidation (OH and O3 reaction), C*=100 ug/m3	|	dark a-pinene ozonolysis (Saha and Grieshop 2016 ES&T)	|	Xu et al. 2018 ACP	|
|	6	|	AMT6J	|	particle	|	semivolatile particulate matter from monoterpene photoxidation (OH and O3 reaction), C*=1000 ug/m3	|	dark a-pinene ozonolysis (Saha and Grieshop 2016 ES&T)	|	Xu et al. 2018 ACP	|
|	7	|	SVMT1	|	gas	|	low volatility gas from monoterpene photoxidation (OH and O3 reaction), C*=0.01 ug/m3	|	dark a-pinene ozonolysis (Saha and Grieshop 2016 ES&T)	|	Xu et al. 2018 ACP	|
|	8	|	SVMT2	|	gas	|	low volatility gas from monoterpene photoxidation (OH and O3 reaction), C*=0.1 ug/m3	|	dark a-pinene ozonolysis (Saha and Grieshop 2016 ES&T)	|	Xu et al. 2018 ACP	|
|	9	|	SVMT3	|	gas	|	semivolailte gas from monoterpene photoxidation (OH and O3 reaction), C*=1 ug/m3	|	dark a-pinene ozonolysis (Saha and Grieshop 2016 ES&T)	|	Xu et al. 2018 ACP	|
|	10	|	SVMT4	|	gas	|	semivolatile gas from monoterpene photoxidation (OH and O3 reaction), C*=10 ug/m3	|	dark a-pinene ozonolysis (Saha and Grieshop 2016 ES&T)	|	Xu et al. 2018 ACP	|
|	11	|	SVMT5	|	gas	|	semivolatile gas from monoterpene photoxidation (OH and O3 reaction), C*=100 ug/m3	|	dark a-pinene ozonolysis (Saha and Grieshop 2016 ES&T)	|	Xu et al. 2018 ACP	|
|	12	|	SVMT6	|	gas	|	semivolatile gas from monoterpene photoxidation (OH and O3 reaction), C*=1000 ug/m3	|	dark a-pinene ozonolysis (Saha and Grieshop 2016 ES&T)	|	Xu et al. 2018 ACP	|
|	13	|	AORGH2OJ	|	particle	|	water associated with organic species of particulate matter	|	Hygroscopicity parameters (Petters and Kreidenweis 2007 ACP) as a function of degree of oxygenation (Lambe et al. 2011 ACP)	|	Pye et al. 2017 ACP	|
|	14	|	AAVB1J	|	particle	|	low volatility organic particulate matter from oxidation of anthropogenic VOCs (benzene, toluene, xylene, PAHs, alkanes)	|	GEOS-Chem VBS parameterization (Pye et al. 2010 ACP) for aromatics and PAHs with long-chain alkanes following Pye and Pouliot 2012 ES&T but with Presto et al. 2010 ES&T VBS fits; all underlying experimental datasets are the same as in aero6	|	M. Qin, EPA/CED/AMDB	|
|	15	|	AAVB2J	|	particle	|	semivolailte organic particulate matter from oxidation of anthropogenic VOCs (benzene, toluene, xylene, PAHs, alkanes)	|	see AAVB1J	|	M. Qin, EPA/CED/AMDB	|
|	16	|	AAVB3J	|	particle	|	semivolailte organic particulate matter from oxidation of anthropogenic VOCs (benzene, toluene, xylene, PAHs, alkanes)	|	see AAVB1J	|	M. Qin, EPA/CED/AMDB	|
|	17	|	AAVB4J	|	particle	|	semivolailte organic particulate matter from oxidation of anthropogenic VOCs (benzene, toluene, xylene, PAHs, alkanes)	|	see AAVB1J	|	M. Qin, EPA/CED/AMDB	|
|	18	|	SVAVB1	|	gas	|	low volatility organic gas from oxidation of anthropogenic VOCs (benzene, toluene, xylene, PAHs, alkanes)	|	see AAVB1J	|	M. Qin, EPA/CED/AMDB	|
|	19	|	SVAVB2	|	gas	|	semivolailte organic gas from oxidation of anthropogenic VOCs (benzene, toluene, xylene, PAHs, alkanes)	|	see AAVB1J	|	M. Qin, EPA/CED/AMDB	|
|	20	|	SVAVB3	|	gas	|	semivolailte organic gas from oxidation of anthropogenic VOCs (benzene, toluene, xylene, PAHs, alkanes)	|	see AAVB1J	|	M. Qin, EPA/CED/AMDB	|
|	21	|	SVAVB4	|	gas	|	semivolailte organic gas from oxidation of anthropogenic VOCs (benzene, toluene, xylene, PAHs, alkanes)	|	see AAVB1J	|	M. Qin, EPA/CED/AMDB	|
|	22	|	MTNO3	|	gas	|	organic nitrates from monoterpene oxidation	|	gas-phase SAPRC yields (should not be counted as gas-phase organic nitrate for evaluation purposes in cb6r3 mechanisms)	|	Pye et al. 2015 ES&T	|
|	23	|	AMTNO3J	|	particle	|	semivolatile organic nitrates from monoterpene oxidation	|	Fry et al. 2009 ACP for vapor pressure of monoterpene organic nitrates	|	Pye et al. 2015 ES&T	|
|	24	|	AMTHYDJ	|	particle	|	organic pseudo-hydrolysis accretion product from monoterpene organic nitrates (AMTNO3J)	|	Boyd et al. 2015 ACP for hydrolysis timescale for tertiary nitrates, but applied to all MTNO3 following Pye et al. 2015 ES&T	|	Pye et al. 2015 ES&T	|


Species new to aero7i: species 1-21 of the above table. Other species in 
aero7i (including species 22-24 from above) previously existed in aero6i. 

Species in aero6/6i that are deprecated in aero7/7i (these species should NOT appear in an ae7/7i namelist): 
ATRP1J, ATRP2J, SV_TRP1, SV_TRP2, ABNZ1J, ABNZ2J, ABNZ3J, SV_BNZ1, SV_BNZ2, AXYL1J, AXYL2J, AXYL3J, SV_XYL1, SV_XYL2,
ATOL1J, ATOL2J, ATOL3J, SV_TOL1, SV_TOL2, APAH1J, APAH2J, APAH3J, SV_PAH1, SV_PAH2, AALK1J, AALK2J, SV_ALK1, SV_ALK2

All gas-phase semivolatiles use species-specific wet and dry deposition surrogates.

Note that underscores are no longer used in species names in any aerosol or nonreactives namelist
(e.g. SV_ISO1 is now SVISO1 in the non-reactives namelist (NR*.nml)).  

### Improvements in consistency
The explicit monoterpene organic nitrate SOA from Pye et al. (2015) originally
implemented in saprc07tic, has been ported to cb6r3-aero7
using the same assumptions as the saprc07tic-aero6i and aero7i implementation. 

### Differences in aero7 and aero7i
Aero7 and aero7i differ in terms of the degree of speciation of isoprene
SOA. Specifically, isoprene epoxydiol (IEPOX) SOA (Pye et al. 2013, 2017) is represented using species 
AISO3J in aero7. In aero7i, AISO3J is approximately zero and IEPOX SOA is 
represented explicitly as organosulfates (AIEOSJ), 2-methyltetrols (AIETETJ), 
and dimers (AIDIMJ). In addition, aero7i includes explicit 
methylglyceric acid (AIMGAJ) and its analogous 
organosulfate (AIMOSJ), both of which are minor. Aero7i also includes
a particle-phase isoprene dinitrate (Pye et al. 2015) that was not ported to aero7.

### Required emission updates
Aero7/7i requires that a-pinene (usually denoted APIN)
is separate from all other monoterpenes (TERP) in the model. This is to avoid
making SOA from a-pinene + nitrate radical reactions as that pathway has been shown to
produce negligible SOA. Saprc07-based mechanisms, including the aero6 ones, already
treat APIN as separate and mutally exclusive of TERP (saprc07tc aero6 emissions
will work with aero7 without any adjustment). CB6r3 with aero6 continues 
to include APIN in TERP as it did in CMAQ v5.2.1.

If you have cb6r3-aero6 emissions and want to run cb6r3-aero7, options are:  
1. Reprocess your emissions to separate APIN from all other monoterpenes.  
2. Use the emission control file and assign 30% of emitted TERP to APIN and 70% of emitted TERP to TERP.  
3. If you are using inline biogenic emissions, do nothing and let CMAQ determine
the correct biogenic speciation for your simulation.  

Approach 1 is the most thorough and the only way to properly map anthropogenic
monoterpene emissions (which are currently relatively minor in the inventory). It may 
not be necessary if your biogenic emissions are calculated inline within CMAQ.
Approach 2 is an approxmimation based on assuming 30% of global monoterpene
emissions are a-pinene (Pye et al. 2010) and could be used if your biogenic
emissions are preprocessed in input files. Approach 3 makes use of separate biogenic emission mapping
profiles for cb6r3-aero6 and cb6r3-aero7 available within CMAQ (see [biogenic emission update](biogenic_apinene.md)). 
Approach 3 is not an option if your biogenic emissions were pre-processed.

### More information on science updates in aero7

  * [Monoterpene SOA](monoterpene_SOA.md)  
  * [Reorganization of anthropogenic SOA species](anthro_SOA.md)  
  * [Uptake of water onto hydrophilic organic aerosol](organic_water.md)  

## Significance and Impact
Increased dry PM2.5 mass primarily in summer in vegetation-rich locations
such as the southeast U.S. (Xu et al. 2018).
Ambient PM2.5 further increased due to water uptake with implications
for metrics such as AOD that represent in situ (vs dry) conditions.                       

## Affected Files
CCTM/src/aero/aero7 (links to aero6)

hlconst.f

dry deposition files                       

## References

Lambe, A. T., Onasch, T. B., Massoli, P., Croasdale, D. R., Wright, J. P., Ahern, A. T., Williams, L. R., Worsnop, D. R., Brune, W. H., and Davidovits, P.: Laboratory studies of the chemical composition and cloud condensation nuclei (CCN) activity of secondary organic aerosol (SOA) and oxidized primary organic aerosol (OPOA), Atmos. Chem. Phys., 11, 8913-8928, https://doi.org/10.5194/acp-11-8913-2011, 2011.               

Murphy, B. N., Woody, M. C., Jimenez, J. L., Carlton, A. M. G., Hayes, P. L., Liu, S., Ng, N. L., Russell, L. M., Setyan, A., Xu, L., Young, J., Zaveri, R. A., Zhang, Q., and Pye, H. O. T.: Semivolatile POA and parameterized total combustion SOA in CMAQv5.2: impacts on source strength and partitioning, Atmos. Chem. Phys., 17, 11107-11133, https://doi.org/10.5194/acp-17-11107-2017, 2017.

Petters, M. D. and Kreidenweis, S. M.: A single parameter representation of hygroscopic growth and cloud condensation nucleus activity, Atmos. Chem. Phys., 7, 1961-1971, https://doi.org/10.5194/acp-7-1961-2007, 2007.

Pye, H. O. T., Murphy, B. N., Xu, L., Ng, N. L., Carlton, A. G., Guo, H., Weber, R., Vasilakos, P., Appel, K. W., Budisulistiorini, S. H., Surratt, J. D., Nenes, A., Hu, W., Jimenez, J. L., Isaacman-VanWertz, G., Misztal, P. K., and Goldstein, A. H.: On the implications of aerosol liquid water and phase separation for organic aerosol mass, Atmos. Chem. Phys., 17, 343-369, https://doi.org/10.5194/acp-17-343-2017, 2017.

Pye, H. O. T., D. J. Luecken, L. Xu, C. M. Boyd, N. L. Ng, K. Baker, B. A. Ayres, J. O. Bash, K. Baumann, W. P. L. Carter, E. Edgerton, J. L. Fry, W. T. Hutzell, D. Schwede, P. B. Shepson, Modeling the current and future roles of particulate organic nitrates in the southeastern United States, Environ. Sci. Technol., 2015. 

Pye, H. O. T., R. W. Pinder, I. Piletic, Y. Xie, S. L. Capps, Y.-H. Lin, J. D. Surratt, Z. Zhang, A. Gold, D. J. Luecken, W. T. Hutzell, M. Jaoui, J. H. Offenberg, T. E. Kleindienst, M. Lewandowski, and E. O. Edney, Epoxide pathways improve model predictions of isoprene markers and reveal key role of acidity in aerosol formation, Environ. Sci.  Technol., doi:10.1021/es402106h, 2013.

Pye, H. O. T., Chan, A. W. H., Barkley, M. P., and Seinfeld, J. H.: Global modeling of organic aerosol: the importance of reactive nitrogen (NOx and NO3), Atmos. Chem. Phys., 10, 11261-11276, https://doi.org/10.5194/acp-10-11261-2010, 2010.

Xu, L., Pye, H. O. T., He, J., Chen, Y., Murphy, B. N., and Ng, N. L.: Experimental and model estimates of the contributions from biogenic monoterpenes and sesquiterpenes to secondary organic aerosol in the southeastern United States, Atmos. Chem. Phys., 18, 12613-12637, https://doi.org/10.5194/acp-18-12613-2018, 2018.        

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #346, 341, 353, 335]

-----
