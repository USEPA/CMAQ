# Updated Gas-Phase Deposition of H2O2, HACET, Organic Nitrates and s07tic Species

**Author/P.O.C.:**, Havala Pye, pye.havala@epa.gov, Computational Exposure Division, U.S. EPA

## Brief Description 

Based largely on the work of Nguyen et al. (2015) for the southeast United States, deposition of several gas-phase species has been updated. Most species only exist in saprc07tic. Howver, IEPOX and H2O2 are common to several mechanisms.

New deposition surrogates

|Species|	Henry’s Law Coefficient (M/atm)	|Diffusivity (cm2/s)|	Relative reactivity	|f0 (meso parameter from Wesely)|	Lebas molar volume (cm3/mol)|
| --- | --- | --- | --- | --- | --- |
|IEPOX|	IEPOX (3x10<sup>7</sup>)	|0.0579|	8|	0	|110.8|
|HACET|	HACET (2.93x10<sup>3</sup>)|	0.106 (Nguyen et al., 2015)|	8|	0|	72.6|

Revised deposition species (saprc07tic species only)

|Species	|Old dry deposition surrogate|	Old wet deposition surrogate|	New dry deposition surrogate|	New wet deposition surrogate|
|---|---|---|---|---|
|NISOPOOH|	H2O2|	H2O2|	NTRM|	HYDROXY_NITRATES (1.7×10<sup>4</sup> M/atm)|
|HPALD|	none|	none|	OP	|hydroxy_peroxide (8.3×10<sup>4</sup> M/atm)|
|ISOPOOH	|OP	|HYDROXY_PEROXIDE (H=7.4x10<sup>3</sup> M/atm)|	IEPOX|	no change|

Revised surrogate properties

|Species|	Property	|Old Value	|New Value	|Reference
|---|---|---|---|---|
|PROPNN	|Henry’s Law Coefficient	|1×10<sup>3</sup> M/atm	|1×10<sup>4</sup> M/atm |	Nguyen et al. (2015)
|H2O2	|Henry’s Law|	8.3×10<sup>4</sup> M/atm	|1.1×10<sup>5</sup> M/atm	|Sander (1999)
|H2O2	|Relative reactivity	|30	|34,000	|Nguyen et al. (2015)
|HYDROXY_NITRATES, PROPNN, ORG_NTR	|f0 (meso parameter)	|0.1|	0	|Nguyen et al. (2015)
|OP	|f0 (meso parameter)	|0.1	|0.3	|Wolfe and Thornton (2011)


## Significance and Impact

(significance and impact on modeled results and runtime)

## Affected Files:
MECHS/saprc07tic_ae6i_aq/AE_saprc07tic_ae6i_aq.nml  
MECHS/saprc07tic_ae6i_aq/GC_saprc07tic_ae6i_aq.nml  
MECHS/saprc07tic_ae6i_aq/NR_saprc07tic_ae6i_aq.nml  
MECHS/saprc07tic_ae6i_aq/RXNS_DATA_MODULE.F90  
MECHS/saprc07tic_ae6i_aq/mech_saprc07tic_ae6i_aq.def  
aero/aero6/AEROSOL_CHEMISTRY.F  
aero/aero6i/AEROSOL_CHEMISTRY.F  
aero/aero6i/SOA_DEFN.F  
cloud/acm_ae6/hlconst.F  
depv/m3dry/DEPVVARS.F  
gas/ebi_saprc07tic_ae6i_aq/hrdata_mod.F  
gas/ebi_saprc07tic_ae6i_aq/hrdriver.F  
gas/ebi_saprc07tic_ae6i_aq/hrg1.F  
gas/ebi_saprc07tic_ae6i_aq/hrg2.F  
gas/ebi_saprc07tic_ae6i_aq/hrg3.F  
gas/ebi_saprc07tic_ae6i_aq/hrg4.F  
gas/ebi_saprc07tic_ae6i_aq/hrinit.F  
gas/ebi_saprc07tic_ae6i_aq/hrprodloss.F  
gas/ebi_saprc07tic_ae6i_aq/hrrates.F  
gas/ebi_saprc07tic_ae6i_aq/hrsolver.F  
vdiff/acm2/ASX_DATA_MOD.F  

## References: 

Nguyen et al. 2015 PNAS

-----
## Internal Records:

### Relevant Pull Requests: 
[PR #11](/usepa/cmaq/pull/11)

### Commit IDs:
8f3bd301099c354350a3d152088c9f2fb961c720  
dfe134ae37240de92fc19292e2ce98306742b984  
dc9a180de6d3452db7852dab7007d1a2a38d8ad5  
77df3e978480eecdea4081599fee89a2a98c597f  
5db1fa3af2f49c9dc382f45593a7657fbb964abf  
e246de9d80a9076575a3d4a83071ed4edeca4a56  
879f22428b8932cd0113c12f42acead44a9bdf1c  

