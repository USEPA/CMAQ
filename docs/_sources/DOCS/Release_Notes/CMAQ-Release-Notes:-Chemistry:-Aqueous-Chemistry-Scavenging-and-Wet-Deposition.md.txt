###  Cleanup of unit conversions and wetdep output mapping  
[Ben Murphy](mailto:murphy.benjamin@epa.gov), U.S. Environmental Protection Agency      
**Type of update**: Science Update    
**Release Version/Date**: CMAQv5.5  

**Description**:  
In CMAQv5.4 and prior, there are at least 3 or 4 different mapped vectors through which data is passed when translating from CGRID to the local arrays within scavwdep and aqchem. Each of these steps constrains the flexibility to expand gas and aerosol components and necessitates the differentiation of codes to maintain separate modules. For example, 'mp', and 'kmt' flavors require special versions of AQ_DATA and/or aq_map to handle mapping. Unfortunately, when the various optional modules for cloud chemistry diverge, it becomes much less likely that instrumented codes like STM and ISAM will be applied across all options. It is also more difficult to keep cloud process codes like CONVCLD and RESCLD consistent across all options.

With refactored mapping procedures, these can all be merged and all the cloud chemical schemes can take advantage of STM, ISAM, and other improvements made to the base code. These improvements will also reduce maintenance needed.

This code update simplifies the unit conversions in cldproc and the mapping of wet deposition rates. Vectors supporting these maps are now available in CGRID_SPCS and don't need to be recalculated in cldproc. Scavwdep was updated to take advantage of aerosol masks. Further updating of scavwdep will occur in subsequent PR's. DDM and ISAM codes were modified for consistency with the new changes.

**Significance and Impact**: This code update is designed to improve the transparency and flexibility of mapping, speciation, and instrumentation within the cloud chemistry/removal processor.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#651](https://github.com/USEPA/CMAQ/commit/cd7b0e8939552ca8056e348fc3758513280cc095) | [PR#651](https://github.com/USEPA/CMAQ_Dev/pull/651)  |
  

### Bug fix for AE2AQ surrogates and redistribution of aerosol species after cloud processing 
[Kathleen Fahey](mailto:fahey.kathleen@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5  

**Description**: 
This is a bug fix updating the treatment of aerosol-to-aqueous surrogates (AE2AQ) for those species in the aerosol namelists that had a mismatch in number of modes of a chosen surrogate and the CMAQ aerosol species, leading to incomplete or incorrect treatment for the affected aerosol species during cloud processing (e.g., if an aerosol species with an I and J mode was assigned to an AE2AQ surrogate with only a J-mode, Aitken scavenging was not calculated for that aerosol species). This update also corrects the redistribution of an aqueous surrogate to "CEND" aerosol species after cloud processing if (1) the surrogate has an I and J mode and (2) the I and J mode fractional compositions of the surrogate are different.

**Significance and Impact**:  
These updates correct the in-cloud treatment and post-cloud redistribution of inert aerosol species. PM2.5 impacts are minor. 

![Fahey_AE2AQ_BugFix](https://github.com/user-attachments/assets/4a16562f-faa3-4c0c-a59b-817d30cda615)

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#976](https://github.com/USEPA/CMAQ/commit/53d0884fc138ab2cb48cf733de961be448b4395d) | [PR#976](https://github.com/USEPA/CMAQ_Dev/pull/976)  | 