# Updated Gas-Phase Deposition of H2O2, HACET, Organic Nitrates and s07tic Species

**Author/P.O.C.:**, [Havala Pye](mailto:pye.havala@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

Based largely on the work of Nguyen et al. (2015) for the southeast United States, deposition of several gas-phase species has been updated. Most species only exist in saprc07tic. However, IEPOX and H2O2 are common to several mechanisms.

New deposition surrogates

|Species| Henry’s law Coefficient (M/atm) | Diffusivity (cm2/s)        | Relative reactivity| f0 (meso parameter from Wesely) | Lebas molar volume (cm3/mol) |
|-------|--------------------------       |--------------              |--------------      |---------------                  |--------------                |
|IEPOX  | IEPOX (3x10<sup>7</sup>)        | 0.0579                     | 8                  | 0                               | 110.8 |
|HACET  | HACET (2.93x10<sup>3</sup>)     | 0.106 (Nguyen et al., 2015)| 8                  | 0                               | 72.6  |

Revised deposition species (saprc07tic species only)

|Species    |Old dry deposition surrogate|	Old wet deposition surrogate| New dry deposition surrogate| New wet deposition surrogate|
|-----------|---------                   |-------------------          |-------------                |----------------- |
|NISOPOOH   | H2O2                       |H2O2                         |	NTRM                       |HYDROXY_ NITRATES (1.7×10<sup>4</sup> M/atm)|
|HPALD      | none                       |none                         |	OP                         |hydroxy_peroxide (8.3×10<sup>4</sup> M/atm)|
|ISOPOOH    |  OP                        |HYDROXY_ PEROXIDE (H=8.3×10<sup>4</sup> M/atm)|IEPOX       |no change    |

Revised surrogate properties

|Species          |Property                             |Old Value                |New Value                |Reference
|-----------------|------------------                   |-------------            |------------               |----------------
|PROPNN           |Henry’s law Coefficient              |1×10<sup>3</sup> M/atm   |1×10<sup>4</sup> M/atm   |Nguyen et al. (2015)
|H2O2             |Henry’s law                          |8.3×10<sup>4</sup> M/atm |1.1×10<sup>5</sup> M/atm |Sander (1999)
|H2O2             |Relative reactivity                  |30                       |34,000	            |Nguyen et al. (2015)
|HYDROXY_ NITRATES, PROPNN, ORG_NTR |f0 (meso parameter) |0.1                      |0                        |Nguyen et al. (2015)
|OP               |f0 (meso parameter)                  |0.1                      |0.3                      |Wolfe and Thornton (2011)


## Significance and Impact

Deposition rates were compared to observations of concentrations and deposition velocities measured during SOAS by Nguyen et al. (2015). The largest changes occured for H2O2 (HO2H in SAPRC) in which dry deposition velocities at midday increased from approximately 3 cm/s to 5 cm/s (consistent with measurements). The bias in H2O2 concentration was reduced from 238% to 113%.

HACET deposition velocities at midday increased from 0.6 cm/s to 2 cm/s. The bias in HACET decreased from 26% to 15%.

HPALD was not deposited in v5.1. Deposition velocities predicted for HPALD in CMAQ v5.2 are low (1 cm/s) compared to measurements (>2 cm/s) at midday. The overestimate in HPALD at night improved signifcantly with the added deposition.

The deposition velocity of NISOPOOH decreased to just under 2 cm/s at midday (consistent with observation of INP), slightly increasing the bias in concentration.

The deposition velocity of PROPNN increased to approximately 2 cm/s at midday (consistent with observations) with little effect on gas-phase predicted PROPNN concentrations.


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

Nguyen, T. B., Crounse, J. D., Teng, A. P., St. Clair, J. M., Paulot, F., Wolfe, G. M., and Wennberg, P. O.: Rapid deposition of oxidized biogenic compounds to a temperate forest, Proc. Natl. Acad. Sci. U. S. A., 112, E392–E401, doi:10.1073/pnas.1418702112, 2015.

Sander R.: Compilation of Henry’s Law Constants for Inorganic and Organic Species of Potential Importance in Environmental Chemistry (Max Planck Inst Chem, Mainz, Germany). http://www.henrys-law.org/henry-3.0.pdf, 1999.

Wolfe, G. M., and Thornton, J. A.: The Chemistry of Atmosphere-Forest Exchange (CAFE) Model – Part 1: Model description and characterization. Atmos. Chem. Phys. 11, 1, 77-101, 2011.

-----
## Internal Records:

### Relevant Pull Requests:
[PR #11](https://github.com/usepa/cmaq_dev/pull/11)

### Commit IDs:
8f3bd301099c354350a3d152088c9f2fb961c720  
dfe134ae37240de92fc19292e2ce98306742b984  
dc9a180de6d3452db7852dab7007d1a2a38d8ad5  
77df3e978480eecdea4081599fee89a2a98c597f  
5db1fa3af2f49c9dc382f45593a7657fbb964abf  
e246de9d80a9076575a3d4a83071ed4edeca4a56  
879f22428b8932cd0113c12f42acead44a9bdf1c  
