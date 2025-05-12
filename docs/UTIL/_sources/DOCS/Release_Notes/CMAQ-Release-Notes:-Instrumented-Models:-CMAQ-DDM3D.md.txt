### CMAQ-HDDM-3D Second Order Sensitivity Fix  
[Sergey L. Napelenok](mailto:contact.email@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5    
**Description**: A bug made its way into the chemistry routines that calculated 2nd order sensitivities (HDDM-3D) resulting in erroneous output for these parameters. Additionally, there are some runtime errors in the log files associated with writing out deposition sensitivity output. Finally, dry deposition sensitivities were not written out correctly.  
**Significance and Impact**: Second order sensitivities will now provide correct output and the log files will no longer display error messages about deposition sensitivities. Sensitivity of dry depositions will now output correctly past the 1st parameter.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#971](https://github.com/USEPA/CMAQ/commit/b39c23827b1d68a661d4b45815229e22d8691c03) | [PR#971](https://github.com/USEPA/CMAQ_Dev/pull/971)  |


### CMAQ-DDM-3D potential vorticity sensitivity  
[Sergey L. Napelenok](mailto:contact.email@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Science Update  
**Release Version/Date**: CMAQv5.5  
**Description**: It is possible to compute sensitivity to ozone incursions at the top of the simulated volume if the base model is compiled with potential vorticity module enabled. For CMAQ-DDM-3D, the sensitivity is to the total domain-wide incursion.   
**Significance and Impact**: The sensitivity parameter is defined in the control file as follows:

    EPV   
    PVO3   
    SPECIES   
    O3     

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#957](https://github.com/USEPA/CMAQ/commit/bb4ebd757ebbb70f0f5e7ce32db52c716d08fdc8) | [PR#957](https://github.com/USEPA/CMAQ_Dev/pull/957)  |


### CMAQ-DDM-3D Version 5.4
[Sergey L. Napelenok](mailto:contact.email@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Science Update  
**Release Version/Date**: CMAQv5.4  
**Description**:  CMAQ-DDM-3D has been fully integrated into the base model. There is no longer a separate repository, and the sensitivity calculations are accessed through compiler flags set in the build script and by completing the appropriate sections of the run script. These options are demonstrated in the sample scripts provided with the release of the base model.

Additionally, some code structure changes were made to the DDM-3D mainly in the gas phase chemistry routines.  Of note is the ability of the model to now calculate gas phase Jacobians inline making the code more flexible to accept new chemical mechanisms as well as changes to existing ones. 

**Significance and Impact**: No changes to sensitivity outputs should be expected in this release outside the ones caused by modifications of the base CMAQ science routines. 

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#609](https://github.com/USEPA/CMAQ/commit/2195617d6a2ff682c8089bcf82674a3862d73efb) | [PR#609](https://github.com/USEPA/CMAQ_Dev/pull/609)  |
|[Merge for PR#630](https://github.com/USEPA/CMAQ/commit/b78c26bccdba7cafcadb2a6ca58ec10317c31b5f) | [PR#630](https://github.com/USEPA/CMAQ_Dev/pull/630)  |
|[Merge for PR#922](https://github.com/USEPA/CMAQ/commit/df82fd1c8381f873d9b56ab74a2af6b9ed4ee61e) | [PR#922](https://github.com/USEPA/CMAQ_Dev/pull/922)  |
|[Merge for PR#929](https://github.com/USEPA/CMAQ/pull/929#issuecomment-1169352469) | [PR#929](https://github.com/USEPA/CMAQ_Dev/pull/929)  |
|[Merge for PR#930](https://github.com/USEPA/CMAQ/commit/72765f335188d9b32a24dc8281fa248b139b3b2a) | [PR#930](https://github.com/USEPA/CMAQ_Dev/pull/930)  |
|[Merge for PR#935](https://github.com/USEPA/CMAQ/commit/7ae4c28e8a50c56f9b0dd4aa3319abdcfcbc36c4) | [PR#935](https://github.com/USEPA/CMAQ_Dev/pull/935)  |
|[Merge for PR#946](https://github.com/USEPA/CMAQ/commit/dddfab60e47bdf9b9782252dc5d85bf0d962ebe1) | [PR#946](https://github.com/USEPA/CMAQ_Dev/pull/946)  |


### Speed up DDM3 solution of Gas Chemistry Sensitivities
[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Science/Numerical Update  
**Release Version/Date**:  CMAQ version 5.4

**Description**: In DDM3D's method for solving sensitivities from gas chemistry, code changes shortened CMAQ_DDM3D runtimes by 15% to 35% dependent on the chemical mechanism used. The changes also implemented DDM3D's approach for gas chemistry in the Rosenbrock and SMVGear solvers so CMAQ_DDM3D can use any of the available gas chemistry solvers. The option better supports using CMAQ_DDM3D built with the cb6r5m_ae7_aq mechanism because the Rosenbrock solver is recommended for the cb6r5m_ae7_aq mechanism.

Changes reduced runtimes by modifying setting and calculating the gas chemistry Jacobian. The modification reorders chemistry species defining the Jacobian's rows and columns based on the number of nonzero values in rows for each chemistry species. The number measures the coupling of a species to other chemistry species. The reordering seeks to put the Jacobian closer to a lower triangular matrix and simplify the LU decomposition of the chemistry Jacobian used by CMAQ_DDM3D. It may also make the process more accurate by reducing the number of floating operations.

**Significance and Impact**:  

Changes have no impact of predicted concentrations and deposition of the base model species but they do altered sensitivity predictions from the DDM3D algorithm by a few percent or less. The exception is for sulfur dioxide and aerosol sulfate in the accumulation mode. Tests that turned off cloud chemistry removed this exception and may indicate that DDM3D has feedback between gas and cloud chemistry regarding predicted sensitivities for sulfate production. Overall the code change reduce DDM3D runtime and allow the CMAQ_DDM3D to used an EBI solver for the chemical mechanism, or a generalized  chemistry solver such Rosenbrock and SMVGear

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#935](https://github.com/USEPA/CMAQ/commit/7ae4c28e8a50c56f9b0dd4aa3319abdcfcbc36c4) | [PR#935](https://github.com/USEPA/CMAQ_Dev/pull/935)  |
|[Merge for PR#630](https://github.com/USEPA/CMAQ/commit/b78c26bccdba7cafcadb2a6ca58ec10317c31b5f) | [PR#630](https://github.com/USEPA/CMAQ_Dev/pull/630)  |