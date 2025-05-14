## CMAQ Version X+ Overview
New CMAQ versions are released on the 'main' branch.  After a CMAQ release an additional bugfix or 'X+' branch is created.  The X+ branch is a continually evolving branch and is intended for users who wish to adopt the most recent bugfixes or minor updates that improve model stability, efficiency, etc. The X+ code has not been thoroughly evaluated or characterized so users must assume the risk of any unforeseen and undocumented impacts of code changes that have been incorporated since the most recent documented public release. The thoroughly vetted version of these bugfixes and updates are then included in the next public release of CMAQ.  For example, after CMAQv5.4 was released, a 5.4+ branch was added.  All of the tagged updates in the 5.4+ branch were then released with CMAQv5.5. 

Below is the record of tagged bugfix versions for each CMAQ release. 

### Record of changes to CMAQv5.5+  
To clone code with all of the CMAQv5.5 bug fixes issue the following command from within a working directory on your server:
```
 git clone -b 5.5+ https://github.com/USEPA/CMAQ.git CMAQ_REPO
```
Note, each tagged version of CMAQv5.5+ contains all updates from previous versions.
|      Tag        |   PR Number       |         PR Name          |   Merge Date     | Brief Description |
| -------------- | ------------------ | ------------------------ | ---------------- | -----------------------|
| |[#249](https://github.com/USEPA/CMAQ/pull/249)|Increase string size for DDM-3D control file | 2025-05-14 | Updates allows for longer strings of emissions stream names and region names for DDM-3D simulations.|
| [CMAQv5.5.0.2_13May2025](https://github.com/USEPA/CMAQ/releases/tag/CMAQv5.5.0.2_13May2025)  |[#246](https://github.com/USEPA/CMAQ/pull/246)|5.5 Tutorial Updates from CMAS  | 2025-04-29 | Improvements to the WRF-CMAQ Tutorial and supporting documentation for preparing your compute environment prior to running CMAQ.|
| |[#245](https://github.com/USEPA/CMAQ/pull/245)|Fix bugs in shp2cmaq | 2025-04-29 | Bugfix to shp2cmaq tool to avoid errors when CMAQ Emission Control reads the time information from the CMAQ-ready grid mask file.|
| |[#242](https://github.com/USEPA/CMAQ/pull/242)|Updated Post-Processing support files and code  | 2025-04-29 | Bug fixes and minor enhancements.|
| |[#241](https://github.com/USEPA/CMAQ/pull/241)|Adds pcVOC and NOy species to ISAM species lists  | 2025-04-29 | This change supports comprehensive accounting of ISAM source attribution of PM, ozone, and other pollutants.|
| |[#240](https://github.com/USEPA/CMAQ/pull/240)|Update conversion factor for area-based denominator  | 2025-04-29 | Bugfix will affect predictions for any species that includes emissions normalized to a specific area that is not in units of meters.|
| |[#239](https://github.com/USEPA/CMAQ/pull/239)| DDM3D Correction to ebi solvers | 2025-05-13 | DDM3D sensitivity calculations were updated for all ebi solvers to resolve some instabilities.|
| |[#238](https://github.com/USEPA/CMAQ/pull/238)|Corrections to ISAM aerosol and cloud processing  | 2025-04-29 | Resolve some spurious unreasonable results reported by CMAQ-ISAM users.|
|[CMAQv5.5.0.1_19Mar2025](https://github.com/USEPA/CMAQ/releases/tag/CMAQv5.5.0.1_19Mar2025) |[#231](https://github.com/USEPA/CMAQ/pull/231)|CRACMM2 fixes for ISAM and post processing  | 2025-03-19 | Small changes in post-processed concentrations due to missing species. Resolves issue [#221](https://github.com/USEPA/CMAQ/issues/221). |
| |[#226](https://github.com/USEPA/CMAQ/pull/226)|Fix bug preventing CMAQ from running using SAPRC mechanisms  | 2024-12-06 | The fix is to specify that CLNO2 does undergo dry deposition to avoid a model crash when using any of the SAPRC07 mechanisms.|
| |[#222](https://github.com/USEPA/CMAQ/pull/222)|Corrected NLCD to BELD3 mapping for shrub/scrub and dwarf scrub| 2024-12-06 | Corrects an error in NLCD40 land use mapping for inline windblown dust calculations. Addresses issues [#220](https://github.com/USEPA/CMAQ/issues/220).|



### Record of changes to CMAQv5.4+  
To clone code with all of the CMAQv5.4 bug fixes issue the following command from within a working directory on your server:
```
 git clone -b CMAQv5.4.0.5_5Sept2024 https://github.com/USEPA/CMAQ.git CMAQ_5.4.0.5
```

Note, each tagged version of CMAQv5.4+ contains all updates from previous versions.
|      Tag        |   PR Number       |         PR Name          |   Merge Date     | Brief Description |
| -------------- | ------------------ | ------------------------ | ---------------- | -----------------------|
| [CMAQv5.4.0.5_5Sept2024](https://github.com/USEPA/CMAQ/releases/tag/CMAQv5.4.0.5_5Sept2024)|[#218](https://github.com/USEPA/CMAQ/pull/218)|Minor fixes to ELMO output algorithms| 2024-09-05 | This fixes several coding errors in the ELMO algorithm used to calculates post-processed variables online like total PM mass and mass of PM species in discrete size ranges. Addresses issues [#210](https://github.com/USEPA/CMAQ/issues/210) and [#212](https://github.com/USEPA/CMAQ/issues/212).|
| [CMAQv5.4.0.4_9April2024](https://github.com/USEPA/CMAQ/releases/tag/CMAQv5.4.0.4_9April2024)|[#215](https://github.com/USEPA/CMAQ/pull/215)|fix bug in calculation of PMF_OC | 2024-04-09 | This fixes a bug in the calculation of the ELMO parameter PMF_OC, which inadvertently subtracts off non-carbonaceous mass that should instead be skipped. Addresses issue [#213](https://github.com/USEPA/CMAQ/issues/213).|
|  |[#201](https://github.com/USEPA/CMAQ/pull/201) | Update config_cmaq.csh  | 2023-07-06 | _`Community Contribution`_ Fixes a typo in the config_cmaq.csh to address issue [#199](https://github.com/USEPA/CMAQ/issues/199).|
|  |[#198](https://github.com/USEPA/CMAQ/pull/198) | Fix functionality of RBSTATS when using ROS3  | 2023-07-06 | _`Community Contribution`_ This fixes a bug in CCTM/src/gas/ros3/rbdriver.F that causes the model not to compile when using the conditional rbstats option.|
|[CMAQv5.4.0.3_9June2023](https://github.com/USEPA/CMAQ/releases/tag/CMAQv5.4.0.3_9June2023)|||||
|  |[#194](https://github.com/USEPA/CMAQ/pull/194) | DDM3D fix May2023  | 2023-06-09 | This fixes bugs impacting 2nd order DDM sensitivities and dry deposition sensitivity output.|
|[CMAQv5.4.0.2_4May2023](https://github.com/USEPA/CMAQ/releases/tag/CMAQv5.4.0.2_4May2023)|||||
|  |[#189](https://github.com/USEPA/CMAQ/pull/189) | WRF-CMAQ Bugfix for UWIND and VWIND at Grid Cell Centers (mass points)  | 2023-05-04 | This fixes a bug that impacts all processes that use the u and v wind components at mass points in the WRF-CMAQ coupled model.|
|  |[#186](https://github.com/USEPA/CMAQ/pull/186) | Isam update 21april2023 | 2023-04-28 |  This fixes several issues with CMAQ-ISAM as described in the PR documentation.|
|[CMAQv5.4.0.1_7Dec2022](https://github.com/USEPA/CMAQ/releases/tag/CMAQv5.4.0.1_7Dec2022)|||||
|  |[#183](https://github.com/USEPA/CMAQ/pull/183) | Enable DDM-3D and ISAM calculations and output for Potential Vorticity option| 2022-12-07 | Inadvertently, DDM-3D and ISAM code were not functioning with the CMAQ 5.4 release when the potential vorticity option was enabled during compilation. This pull request corrects both models.|
|  |[#182](https://github.com/USEPA/CMAQ/pull/182) | Add precision to logfile process timing| 2022-12-02 | At high computational efficiency, the default precision provided for the timing metrics in the logfile was yielding 0.0. This PR adds 3 decimal places of precision to the timing output.|
|  |  [#181](https://github.com/USEPA/CMAQ/pull/181)|Resolve time-stepping issue in ELMO when used within WRF-CMAQ and MPAS-CMAQ | 2022-12-02 | ELMO gives erroneous results in WRF-CMAQ and when using met inputs not aligned with hour time steps. This PR resolves the problem by adjusting the algorithm for identifying when the initial time step is hit for the simulation and synchronization cycles.|
| | [#180](https://github.com/USEPA/CMAQ/pull/180)     | BDSNP fix  | 2022-12-02 | Online nitrogen deposition (NDEP) is erroneously enabled when using BDSNP in the released version of CMAQ 5.4. This PR corrects an IF statement that was meant to disable online nitrogen deposition. |


### Record of changes to CMAQv5.3.3+
To clone code with all of the CMAQv5.3.3 bug fixes issue the following command from within a working directory on your server:
```
 git clone -b CMAQv5.3.3.3_10May2022 https://github.com/USEPA/CMAQ.git CMAQv5.3.3.3_10May2022
```

Note, each tagged version of CMAQv5.3.3+ contains all updates from previous versions.

|      Tag        |   PR Number       |         PR Name          |   Merge Date     | Brief Description |
| -------------- | ------------------ | ------------------------ | ---------------- | -----------------------|
| [CMAQv5.3.3.3_10May2022](https://github.com/USEPA/CMAQ/releases/tag/CMAQv5.3.3.3_10May2022) |[#168](https://github.com/USEPA/CMAQ/pull/168) | WRF-CMAQv533+| 2022-05-10 | Script and documentation update to ensure streamlined building of the WRF-CMAQ model intended for users who would like to run CMAQv5.3.3+ with WRFv4.4+.|
| [CMAQv5.3.3.2_7May2022](https://github.com/USEPA/CMAQ/releases/tag/CMAQv5.3.3.2_7May2022)     |  [#171](https://github.com/USEPA/CMAQ/pull/171)|Remove files from chemistry utilities causing problems on MAC and WINDOWS systems | 2022-05-07 | Both the create_ebi and inline_phot_prerproc utilities have files in their source code directories that only differ in letter case in filenames. The property does not cause problems on Linux systems but can cause problems on Window and Macintosh systems. The pull request removes the unneeded files causing the problems.|
|[CMAQv5.3.3.1_11Feb2022](https://github.com/USEPA/CMAQ/releases/tag/CMAQv5.3.3.1_11Feb2022) | | | | |
| | [#163](https://github.com/USEPA/CMAQ/pull/163)     | Resolve Bug for reading Emission Stream Families        | 2022-02-09 | Bug fix to avoid errors when processing matches for emission stream families related to capitalization. |
|                | [#164](https://github.com/USEPA/CMAQ/pull/164)    | Resolve bug in Low-NOx formation of SOA  | 2022-02-09| A typo in the SOA_DEFN data table resulted in high NOx reactions contributing to low-NOx species, which are quite low in volatility. This bug fix has a considerable impact on SOA if using AE6. No impact for AE7.  |
|                | [#165](https://github.com/USEPA/CMAQ/pull/165)    | Bugfix Speed up emissions processing in PTMET subroutine        | 2022-02-09 | The PTMET subroutine is streamlined so that it is not run for every point source and every stream every time each stream is called. This update drastically reduces runtimes in areas where there happen to be lots of point sources on a particular processor. No impact on results. |










