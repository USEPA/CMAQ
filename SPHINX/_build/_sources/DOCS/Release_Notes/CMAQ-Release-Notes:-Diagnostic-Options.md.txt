# Diagnostic Options

## Remove Uninitialized Variable in Column Model
[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5  

**Description**: When the GRID_NAME sets the model domain to a column model, _i.e., NCOLS and NROWS equal one_. The model unpredictably crashes with the below error message.

     NCVGT: : NetCDF: Index exceeds dimension bound
        *** ERROR ABORT in subroutine retrieve_time_de on PE 000
        Could not extract MET_CRO_3D       file
     Abort(0) on node 0 (rank 0 in comm 0): application called MPI_Abort(MPI_COMM_WORLD, 0) - process 0

The error occurs because SDATE and STIME are uninitialized in sciproc.F and later used to interpolate the model grid's Jacobian. Uninitialized SDATE and STIME may have values outside the period covered by the METCRO3D file so the time interpolation fails. The code fix moves setting SDATE and STIME above the IF ( .NOT. COLUMN_MODEL ) block in sciproc.F.  

**Significance and Impact**: Allows running CCTM for a column domain without unpredictable crashes. Using a column model can speed up developing code or debugging species name-lists and emissions control files.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1072](https://github.com/USEPA/CMAQ/commit/277c01c4b82b86a021949a72b3e387449cf68eda) | [PR#1072](https://github.com/USEPA/CMAQ_Dev/pull/1072)  |   

## Fix bug in ELMO calculation of PMF_OC, PMF_NCOM 
[Chris Nolte](mailto:nolte.chris@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5  

**Description**: This fixes a bug in the calculation of the ELMO parameter PMF_OC, which inadvertently subtracts off non-carbonaceous mass that should instead be skipped. Because PMF_NCOM is calculated using the value of PMF_OC, this bug affects PMF_NCOM as well.  

**Significance and Impact**: This change affects only PMF_OC and PMF_NCOM in the ELMO/AELMO outputs. There is no change in CONC/ACONC, CGRID, or deposition outputs. If using the default treatment of semivolatile organics, in which POC and PNCOM are mapped to VSVPO1, VSVPO2, VSVPO3, and VIVPO1 (see CMAQ_Control_DESID_<MECH>.nml), then the impact is minimal. However, if using the older method of treating POA as nonvolatile, with POC mapped to APOC and PNCOM mapped to APNCOM, then this bug has a significant impact on PMF_OC and PMF_NCOM.  
  
|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1104](https://github.com/USEPA/CMAQ/commit/3857d63aa00c2fbdd8278ccedc2c4986acc24f0e) | [PR#1104](https://github.com/USEPA/CMAQ_Dev/pull/1104)  | 

## Correct calculation of PM1, PM2.5, and PM25to10 as well as speciated NA, K, CA, and MG in ELMO
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5  
 
**Description**: The summations within ELMO for nonexplicit coarse-mode cations (NA, K, CA, and MG) into output aerosol variables with hard size cutoffs (PM1, PM25, and PM25to10) were incomplete.  This issue was identified on the CMAS User Forum [https://forum.cmascenter.org/t/incorrect-pm25-na-calculation-in-the-elmo-module/4615](https://forum.cmascenter.org/t/incorrect-pm25-na-calculation-in-the-elmo-module/4615)  

A second correction also fixes a bug in the calculation of the ELMO parameter PM2.5 and PM2.5 species. To calculate the mass of particulate up to a hard size cutoff, the subroutine GET_AERO_INLET is called for each mode, with the second input parameter specifying the mode to be calculated. In CMAQv5.4, the variable IM was used, but the correct variable should be IMODE.

**Significance and Impact**: This change is important for a complete representation of PM25 cations. Significant impacts on PM25_NA (~33%), PM25_CA (~16%), PM25_MG (~27%), and PM25_K (9%) in tests on the southeast US benchmark domain.  

**PM<sub>2.5</sub> Sodium**
![Murphy_PM_Sodium](https://github.com/user-attachments/assets/3281f30b-e914-4c5f-a8cc-da6ebb9b2282)

**PM<sub>2.5</sub> Calcium**
![Murphy_PM_Calcium](https://github.com/user-attachments/assets/02609bbb-d56c-4c2e-bda3-93da53e32fe3)

**PM<sub>2.5</sub> Magnesium**
![Murphy_PM_Magnesium](https://github.com/user-attachments/assets/a459a040-a2de-43b7-a981-41d8c314f2ea)

**PM<sub>2.5</sub> Potassium**
![Murphy_PM_K](https://github.com/user-attachments/assets/8f546043-686d-46ee-be8b-c023e2729d4e)

**PM<sub>2.5-10</sub> Sodium**
![Murphy_PM_Sodium_II](https://github.com/user-attachments/assets/5881c212-1206-4a41-ba0f-999bcaafaa93)

The mode parameter fix for the calculation of mass with a hard diameter cut-off has important impacts on PM2.5 predictions across the model domain, and the specific impact depends on fraction of the total mass, or speciated mass, that is present in each of the Aitken, Accumulation and Coarse modes. The predictions for fine-mode particulate (PMF) are unaffected as the GET_AERO_INLET routine is not called for these cases. 

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1078](https://github.com/USEPA/CMAQ/commit/52a12a0ac08237a871998996873dd57486833891) | [PR#1078](https://github.com/USEPA/CMAQ_Dev/pull/1078)  | 

## Accurate output of gas-phase species in mass units
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5  
  
**Description**: Although ELMO was correctly outputting gas species mixing ratios, ELMO had an error outputting these variables from the GC or NR lists if mass units were requested. This PR resolves the issue and adds the ability to output NH3 and HNO3 in mass units via ELMO.  

**Significance and Impact**: Gas-phase Benzo-a-pyrene was yielding garbage output from ELMO.   

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1008](https://github.com/USEPA/CMAQ/commit/f1d6b57dc10c2a02dffc60bd3ace522c40e677b6) | [PR#1008](https://github.com/USEPA/CMAQ_Dev/pull/1008)  | 


## Restore accurate timestepping when running with temporally finer MET inputs 
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5  

**Description**: ELMO gave erroneous results when run using met inputs not aligned with hour time steps such as seen when running WRF-CMAQ. See Issue [179](https://github.com/USEPA/CMAQ/issues/179) in public repo.  
  
**Significance and Impact**: This model bug fix should be adopted as soon as possible for any user running WRF-CMAQ or using met inputs with time steps not aligned with hourly structure. Simulations for offline CMAQ with hourly met inputs are unaffected.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#962](https://github.com/USEPA/CMAQ/commit/a6e2bf20b774467fed85fc82eacd93ff57ef3434) | [PR#962](https://github.com/USEPA/CMAQ_Dev/pull/962)  | 

## Introduction of the Explicit and Lumped air quality Model Output module (ELMO)
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: New Feature  
**Release Version/Date**: CMAQv5.4  

**Description**: 
The ELMO Synthesizer streamlines the definition, calculation, and maintenance of over 170 aggregate particulate matter parameters online in CMAQ. Previously, aggregate parameters like PM2.5 and Fine-mode Sulfate (ASO4I + ASO4J) were calculated through the COMBINE post-processing utility and documented via the SpecDef input file for that program. With ELMO, these variables are defined in ELMO_DATA.F and their calculations are prescribed in ELMO_PROC.F. Thus, CMAQ can now output higher-level parameters without needing to run any particular post-processing tool.  

The interface for using ELMO is on the CMAQ Miscellaneous Control File. Please see that file for a description of how to use the options relevant for ELMO.  

There are several distinct advantages to using ELMO over post-processing raw output with COMBINE:  
  - Definitions of parameters like PM2.5, PMF (Fine PM), and PM10.0 automatically adjust as PM species are activated or deactivated in CMAQ. There is no need to modify a SpecDef file to account for a new species.  
  - Complex properties like O:C, OM:OC, particle acidity, etc. can be calculated using species properties available within CMAQ. This resolves a potential vulnerability where, for example, the OM:OC of organic species may become out of sync between the SpecDef and the SOA_DEFN table within the model. This could potentially lead to errors in the calculation of OC (organic carbon).  
  - If a user is only interested in aggregate parameters like PM25 mass, they can avoid the I/O time and storage required saving the raw output of every PM variable and then processing with COMBINE. This can be particularly important when processing 3D data.  
  - This functionality will be critical for applications like ISAM where there is a huge runtime and storage penalty for outputting raw species concentrations for every emission source.  
  - New parameters are available that were not before like N10, N20, N40 and N100, the number of particles above 10, 20, 40 and 100 nm in diameter. AOD and extinction have also been supported as options.  
  - Some time may be saved in the input/output of so many variables via COMBINE.  

Keywords are available (see the interface in the EmissionControlFile) to use a short cut for identifying the parameters of interest. Parameters may be added to the ELMO_LIST table in ELMO_DATA.F and then prescribed in ELMO_PROC.F with greater ease. 

For developers: the new module works by putting all of the diagnostic parameters first on the ELMO_LIST table in ELMO_DATA.F. There is also a list of ID numbers (integers) above that table which allow for a master order to be preserved while the order of the parameters in the table and their calculations in ELMO_PROC.F are allowed to move in sequence. The subroutine LOAD_ELMO (ELMO_PROC.F) is called from AERO_DRIVER and cycles through the list of used parameters as defined by the interface in the EmissionControlFile.  

For each parameter, the subroutine CALC_ELMO is called to lookup the calculation. For many of the calculations, all that is required is an assignment from an already existing diagnostic variable. For the parameters which are linear combinations of CMAQ species or other parameters (e.g. fine-mode nitrate PMF_NO3 = ANO3I + ANO3J, PM2.5 mass PM25 = PM25_SO4 + PM25_NO3 + PM25_NH4 + PM25_CL + PM25_NA + PM25_EC + PM25_POA + ...), they may be defined in the subroutine MAP_ELMO_COEFF (in ELMO_PROC.F). Follow the guide of existing parameters to prescribe the species to be added, the inlet type to assume for collection, etc. If you need to add a new inlet description, you can do that in the ELMO_INLET table in ELMO_DATA.F. For more complicated variables like fine-mode acidity (PMF_PH) or the PM25 mass collected by a Federal Reference Method sampler (PM25_FRM), calculations appear directly in CALC_ELMO_DATA.
  
**Significance and Impact**: There is no quantitative impact on results, but there will be a noticeable positive impact on the time invested in post-processing aggregate PM components and storage volumes required for standard runs. There can be some slight differences realized when one compares quantities averaged online vs. offline. For example, total PM2.5 have some small deviations when it is calculated as the hourly average of the sum of species (online) versus the sum of hourly averaged species (offline).  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#639](https://github.com/USEPA/CMAQ/commit/3dc2bb6e3d3041bbbf0729643cc38bb2c52b2e11) | [PR#639](https://github.com/USEPA/CMAQ_Dev/pull/639)  | 
|[Merge for PR#637](https://github.com/USEPA/CMAQ/commit/6bf6a3c367cb5fae088396c879e1c9609766a5dd) | [PR#637](https://github.com/USEPA/CMAQ_Dev/pull/637)  | 
|[Merge for PR#769](https://github.com/USEPA/CMAQ/commit/c5bce3ef77dc54b29bf66046d07f766afc2d9f61) | [PR#769](https://github.com/USEPA/CMAQ_Dev/pull/769)  | 