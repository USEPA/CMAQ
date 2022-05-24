# Explicit and Lumped air quality Model Output (ELMO) Synthesizer
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency

## Brief Description
The ELMO Synthesizer streamlines the definition, calculation, and maintenance of over 170 aggregate particulate matter parameters online in CMAQ. Previously, aggregate parameters like PM2.5 and Fine-mode Sulfate (ASO4I + ASO4J) were calculated through the COMBINE post-processing utility and documented via the SpecDef input file for that program. With ELMO, these variables are defined in PMDIAG_DATA and their calculations are prescribed in PMDIAG_PROC. Thus, CMAQ can now output higher-level parameters without needing to run any particular post-processing tool.  

The interface for using ELMO is on the CMAQ Miscellaneous Control File. Please see that file for a description of how to use the options relevant for ELMO.  

There are several distinct advantages to using ELMO over post-processing raw output with COMBINE:  
  - Definitions of parameters like PM2.5, PMF (Fine PM), and PM10.0 automatically adjust as PM species are activated or deactivated in CMAQ. There is no need to modify a SpecDef file to account for a new species.  
  - Complex properties like O:C, OM:OC, particle acidity, etc. can be calculated using species properties available within CMAQ. This resolves a potential vulnerability where, for example, the OM:OC of organic species may become out of sync between the SpecDef and the SOA_DEFN table within the model. This could potentially lead to errors in the calculation of OC (organic carbon).  
  - If a user is only interested in aggregate parameters like PM25 mass, they can avoid the I/O time and storage required saving the raw output of every PM variable and then processing with COMBINE. This can be particularly important when processing 3D data.  
  - This functionality will be critical for applications like ISAM where there is a huge runtime and storage penalty for outputting raw species concentrations for every emission source.  
  - New parameters are available that were not before like N10, N20, N40 and N100, the number of particles above 10, 20, 40 and 100 nm in diameter. AOD and extinction have also been supported as options.  
  - Some time may be saved in the input/output of so many variables via COMBINE.  

Keywords are available (see the interface in the EmissionControlFile) to use a short cut for identifying the parameters of interest. Parameters may be added to the PMDIAG_LIST table in PMDIAG_DATA.F and then prescribed in PMDIAG_PROC.F with greater ease. 

For developers: the new module works by putting all of the diagnostic parameters first on the PMDIAG_LIST table in PMDIAG_DATA.F. There is also a list of ID numbers (integers) above that table which allow for a master order to be preserved while the order of the parameters in the table and their calculations in PMDIAG_PROC.F are allowed to move in sequence. The subroutine LOAD_PMDIAG_DATA (PMDIAG_PROC.F) is called from AERO_DRIVER and cycles through the list of used parameters as defined by the interface in the EmissionControlFile.  

For each parameter, the subroutine CALC_PMDIAG_DATA is called to lookup the calculation. For many of the calculations, all that is required is an assignment from an already existing diagnostic variable. For the parameters which are linear combinations of CMAQ species or other parameters (e.g. fine-mode nitrate PMF_NO3 = ANO3I + ANO3J, PM2.5 mass PM25 = PM25_SO4 + PM25_NO3 + PM25_NH4 + PM25_CL + PM25_NA + PM25_EC + PM25_POA + ...), they may be defined in the subroutine MAP_PMDIAG_COEFF (in PMDIAG_PROC.F). Follow the guide of existing parameters to prescribe the species to be added, the inlet type to assume for collection, etc. If you need to add a new inlet description, you can do that in the PMDIAG_INLET table in PMDIAG_DATA.F. For more complicated variables like fine-mode acidity (PMF_PH) or the PM25 mass collected by a Federal Reference Method sampler (PM25_FRM), calculations appear directly in CALC_PMDIAG_DATA.

## Significance and Impact  
There is no quantitative impact on results, but there will be a noticeable positive impact on the time invested in post-processing aggregate PM components and storage volumes required for standard runs. There can be some slight differences realized when one compares quantities averaged online vs. offline. For exmaple, total PM2.5 have some small deviations when it is calculated as the hourly average of the sum of species (online) versus the sum of hourly averaged species (offline).  

## Affected Files
CCTM/src/aero/aero7/PMDIAG_DATA.F  
CCTM/src/aero/aero7/PMDIAG_PROC.F  
CCTM/src/aer/aero7/aero_driver.F  
CCTM/src/aer/aero7/AERO_DATA.F  

-----
## Internal Records:
#### Relevant Pull Requests:

#### Commit 
IDs:                        

-----
