### Updates needed for MPAS-CMAQ implementation  
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Bug Fix  
**Release Version/Date**: v5.5  

**Description**:  Two issues were found related to emissions processing algorithms in DESID.  

(1) In desid_module.F, when the emission scale factor is applied, there are conditionals that govern whether the scale factor (FAC) should be adjusted by the map scale factor or grid cell area. These potential adjustments are applied within a loop over vertical levels and so their impact will accumulate as the algorithm treats higher model layers. This issue was not resolved earlier since area adjustments are seldom needed in the current CMAQ workflow.

(2) A variable in AERO_EMIS (EMISM3) was allocated every time the subroutine was called, and a better approach is to define EMISM3 as a saved variable and allocate it once. It is a large variable so avoiding repeated allocations may have an impact on memory management.

**Significance and Impact**: In addition to correcting potential vulnerabilities, these changes are needed to harmonize the code between offline CMAQ and MPAS-CMAQ.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1007](https://github.com/USEPA/CMAQ/commit/109ac7ef5b972dee37fd4e1f66ec865b277d77c2) | [PR#1007](https://github.com/USEPA/CMAQ_Dev/pull/1007)  |

### Reconcile Emission Molecular Weight Table with CRACMM Speciation  
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Mechanism Support  
**Release Version/Date**: CMAQv5.5  

**Description**:  DESID uses molecular weight to convert between moles and mass for gas and aerosol emission variables. In general, these MW's are not needed because gases and aerosols are provided in terms of moles and mass, respectively. However, if emissions for a CMAQ species are provided in a unit that requires this conversion, the MW must be provided or a value of 1.0 will be assumed.   

**Significance and Impact**: To fully support CRACMM development and implementation.  In most cases, there is no impact on results. If MW is needed for a unit conversion, then the impact can be very significant (1-2 orders of magnitude for the affected species).   

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1006](https://github.com/USEPA/CMAQ/commit/bea8e25dccc0c416e42924cda3890ab098470b58) | [PR#1006](https://github.com/USEPA/CMAQ_Dev/pull/1006)  |

### Chemical Family Support
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5   

**Description**: Allows for aerosol bulk names to be used on emission input files (e.g. APOC or ASO4). These variable names do not include the mode suffixes. DESID should be able to map them to an internal CMAQ species using a rule like:
```
Region             Stream      Emission    CMAQ     Phase       Scale      Basis    Operator
                               Variable    Species            Factor
'EVERYWHERE', 'ALL'          ,'APOC',     'APOC'      ,'FINE',    1.0,        'UNIT',   'a',
```
but it currently cannot because it automatically stores all aerosol bulk names as families with members equivalent to the list of aerosol species matching that chemical (i.e. APOC contains APOCI and APOCJ). If an input file has APOC on it, and the user tries to map to it, DESID looks for APOCI and APOCJ, can't find them, and reports a problem (but moves on and runs). Previous CMAQ versions (5.3.3) had this capability and the introduction of aerosol bulk name families compromised it.

In the new approach, if the algorithm detects that a variable name is an aerosol bulk name, it looks for an emission variable matching that name, instead of breaking it apart into its members. On the other hand, if the name is used in the CMAQ species column (i.e. the second occurrence of APOC in the example above), then DESID will match the components and scale them each as desired.

Other improvements in this update include a check when families are defined to make sure that if a user defines a family that has the same name as an active aerosol bulk name, then the user definition is preferred and the number of chemical families is reduced.

**Significance and Impact**: No impact on model results. Restoration of features available in previous model versions and better error checking and handling.    

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#963](https://github.com/USEPA/CMAQ/commit/f7182ebf94524be13a6db804a26a148f863dc3f2) | [PR#963](https://github.com/USEPA/CMAQ_Dev/pull/963)  | 


### Restructuring and Miscellaneous updates to the DESID Interface and Processing Features
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Interface Update  
**Release Version/Date**: CMAQv5.4  

**Description**: 
The DESID Emission Control file has been restructured and several minor updates have been made to the functionality and features available in the DESID interface. Specific details as follows:
- The Emission control file has been streamlined and split to improve maintainability. There are now three control files including CMAQ_Control_Misc.nml which contains input parameters for non-DESID modules like ELMO and the Budget tool, CMAQ_Control_DESID.nml which contains DESID parameters that are independent of chemical mechanism (e.g. region and are size distribution parameters), and CMAQ_Control_DESID_${MECH}.nml which contains mechanism-dependent scaling rules.
- Area normalization - if offline streams are provided in units of area-normalized flux, DESID can now be told to multiply the flux inputs by the area of each grid cell projected to the real area on the Earth's surface, if appropriate.
- A computational inefficiency was discovered that has been corrected for cases when there are more than 200,000 point sources on a single sub-domain block.
- Diagnostic options have been expanded so that users can choose which variables and sources will populate individual output files.
- A bug in the processing of region-based scaling parameters was discovered and revised.
- Variables and comments have been revised for clarity.
- Chemical families have been revised to so they are accessed globally by other modules like ELMO.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#817](https://github.com/USEPA/CMAQ/commit/d3a8d13f63746a83854a4babde3a2a5d9747e15d) | [PR#817](https://github.com/USEPA/CMAQ_Dev/pull/817)  | 
|[Merge for PR#809](https://github.com/USEPA/CMAQ/commit/c6f906165301f28ad45a0118acec9d9a6666db98) | [PR#809](https://github.com/USEPA/CMAQ_Dev/pull/809)  | 
|[Merge for PR#766](https://github.com/USEPA/CMAQ/commit/9b90649c8fb9316c5abc163f79e3a1245644880f) | [PR#766](https://github.com/USEPA/CMAQ_Dev/pull/766)  | 
|[Merge for PR#714](https://github.com/USEPA/CMAQ/commit/804498d39c73e648b1aa72fcb807697fd1dc67b1) | [PR#714](https://github.com/USEPA/CMAQ_Dev/pull/714)  | 
|[Merge for PR#709](https://github.com/USEPA/CMAQ/commit/c510f9b3be031cd799d1f7dd5a106674c29e58b6) | [PR#709](https://github.com/USEPA/CMAQ_Dev/pull/709)  | 
|[Merge for PR#648](https://github.com/USEPA/CMAQ/commit/22d519fdac9c8fcfec9aeb2c186b2f0e3f77ad8b) | [PR#648](https://github.com/USEPA/CMAQ_Dev/pull/648)  | 
|[Merge for PR#894](https://github.com/USEPA/CMAQ/commit/baced51d9047a7814846cb54533e6a0dfe14832c) | [PR#894](https://github.com/USEPA/CMAQ_Dev/pull/894)  | 
|[Merge for PR#638](https://github.com/USEPA/CMAQ/commit/cac8f6cbd55558c330549278d396a1d3920d2f80) | [PR#638](https://github.com/USEPA/CMAQ_Dev/pull/638)  | 
|[Merge for PR#772](https://github.com/USEPA/CMAQ/commit/60af2e025519ccaed3a51feebda622d48e9f020a) | [PR#772](https://github.com/USEPA/CMAQ_Dev/pull/772)  | 
