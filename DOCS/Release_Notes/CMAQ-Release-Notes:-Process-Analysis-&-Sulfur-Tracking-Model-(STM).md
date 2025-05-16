# Process Analysis and Sulfur Tracking Model (STM)

## Fix to option for Sulfur Tracking Model
[William T. Hutzell](mailto:Hutzell.Bill@epa.gov), U.S. Environmental Protection Agency  
**Type of Update**: Bug Fix   
**Release Version**: CMAQv5.5  

**Description**: The Sulfur Tracking Model (STM) does not accurately attribute concentrations of ASO4J from gas chemistry. Code changes were made to remove the problem by implementing the below fixes.

1. CGRID_SPCS.F and AERO_DATA.F are updated for new chemical mechanisms that have Organosulfate reactions in CGRID_SPCS.F and AERO_DATA.F, i.e., the cb6r5_ae7 and cracmm1 based mechanisms. Note that the current files already account for the cb6r5m_ae7 mechanism.  

2. In sciproc.F, PA_UPDATE_AERO and STM_WRAP_AE calls are reversed. The incorrect calling order causes errors in ASO4JGAS_ because it depends on aero_cond and aero_npf, and they will have wrong units from a COUPLE_PA call in PA_UPDATE_AERO.  

**Significance and Impact**: The code fix allows to accurately predict the inorganic and organic components of aerosol sulfate with CMAQv54. 
   
|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#968](https://github.com/USEPA/CMAQ/commit/19592006add22a559e275e98e71193f697d7b064) | [PR#968](https://github.com/USEPA/CMAQ_Dev/pull/968)  |

## Use Budget Filename set in Runscript
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency  
**Type of Update**: Minor Feature Improvement  
**Release Version**: CMAQv5.5  
 
**Description**: The budget file initialization algorithm creates the output filename manually using the simulation scenario character string and the output directory defined in the runscript. However, the standard runscript already defines the budget output filename in CTM_BUDGET and this variable isn't being used. This update improves the algorithm in the source code to first check for this variable and if it is not present, then build the filename manually as it currently does.  

**Significance and Impact**: Code change only impacts filenames and prevents error crashing. No impact on data values.  
   
|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#967](https://github.com/USEPA/CMAQ/commit/b3968964c6f8adfbb5fda9f3677b3ee1586b53e2) | [PR#967](https://github.com/USEPA/CMAQ_Dev/pull/967)  |

## Introduction of Budget Reporting Tool
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency  
**Type of Update**: New Feature  
**Release Version**: CMAQv5.4  

**Description**: 
The existing Process Analysis module outputs process rates for variables or families of variables on a gridded domain. The Budget Reporting Tool added here outputs as a text file (CCTM_BUDGET_xxx.txt), the domain-wide process rates and total abundance change for every variable or family requested by the user in the CMAQ_Control_Misc.nml file.

The output produced by the Budget Tool is quite powerful when applying or developing CMAQ. It can be used to better understand the large-scale source and loss pathways of individual trace species or families of species. For example, the fraction of a species that is transported out of the domain versus lost by dry or wet deposition is immediately accessible as a function of output time step. Additionally, one can use this output to diagnose potential errors if, for example, the Budget Tool reports emissions or chemical production of a species and the developer knows it should not be possible.

Output is produced for every output time step (generally hourly). Units for both gases and particles are in kilograms per output time step.
  
**Significance and Impact**: No impact on results for the cases tested.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#810](https://github.com/USEPA/CMAQ/commit/37c09c0a6617b1595f769c9d12f8911da2a12098) | [PR#810](https://github.com/USEPA/CMAQ_Dev/pull/810)  |
|[Merge for PR#634](https://github.com/USEPA/CMAQ/commit/e040ca874d6cd468a31363dca92edc3500cab46b) | [PR#634](https://github.com/USEPA/CMAQ_Dev/pull/634)  |
|[Merge for PR#669](https://github.com/USEPA/CMAQ/commit/22b870e33495c9e9f45b4a794f586bdb61d47333) | [PR#669](https://github.com/USEPA/CMAQ_Dev/pull/669)  |
 

## Resolve omission of organic condensable vapors from aerosol process analysis
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version**: CMAQv5.4  

**Description**: 
The flux of every condensable inorganic and organic vapor should be aggregated and saved in the process analysis arrays for aerosol condensation (i.e. COND). This procedure was overlooked for the organic species, although it was implemented for the inorganics.
  
**Significance and Impact**: This is particularly important for ISAM and the Budget Tool which will use these net changes in aerosol processing to perform source apportionment and some domain-wide totals.
 
 
|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#810](https://github.com/USEPA/CMAQ/commit/37c09c0a6617b1595f769c9d12f8911da2a12098) | [PR#810](https://github.com/USEPA/CMAQ_Dev/pull/810)  |
|[Merge for PR#634](https://github.com/USEPA/CMAQ/commit/e040ca874d6cd468a31363dca92edc3500cab46b) | [PR#634](https://github.com/USEPA/CMAQ_Dev/pull/634)  |
|[Merge for PR#669](https://github.com/USEPA/CMAQ/commit/22b870e33495c9e9f45b4a794f586bdb61d47333) | [PR#669](https://github.com/USEPA/CMAQ_Dev/pull/669)  | 