# Structural Improvements

### GNU build flag update to enable compilation with GNU versions 10+
[Fahim Sidi](mailto:sidi.fahim@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Bug Fix  
**Release Version/Date**:  v5.5  

**Description**:  Starting GNU version 10+, GNU no longer allows rank mismatches between the callee and the calling function. The exact verbiage from the GNU change logs:"Mismatches between actual and dummy argument lists in a single file are now rejected with an error. Use the new option -fallow-argument-mismatch to turn these errors into warnings; this option is implied with -std=legacy. -Wargument-mismatch has been removed.” (https://gcc.gnu.org/gcc-10/changes.html)

“GCC 10 now rejects argument mismatches occurring in the same source file. Those are not permitted by the Fortran standard and in general have the potential to generate invalid code. However, the Fortran standard does permit passing an array element or a scalar string (of default character kind or of c_char kind) as actual argument to an array dummy argument. (For the exact wording, see the Fortran standard on argument association; in particular, Fortran 2018, Sect. 15.5.2.4, Para. 4.)

Depending on their nature, argument mismatches have the potential to cause the generation of invalid code and, hence, should be investigated. The most common reason that code fails due to newly enforced check is the following: instead of using an array element as actual argument, a scalar is used; one solution is to replace the scalar by a size-one array. (This should be passed as a whole as there is no point in passing it as array element.) Additionally, check that the code indeed only accesses this single element. — Other mismatches occur more rarely but usually indicate more serious bugs where a wrong result is likely (at least for some target-platform and optimization combination).”

The non-FORTRAN explanation boils down to the ability to pass 1-D arrays, 2-D arrays, 3-D arrays, etc., into a subroutine or function and have the called routine set up so that that it "does the right thing". This is in fact a common occurrence, where the callee "single-indexes" multi-dimensional arrays.

**References**:  n/a
|Merge Commit | Internal record|
|:------:|:-------:|
| [Merge for PR#1154](https://github.com/USEPA/CMAQ_Dev/commit/c31983b72a3049d708138da3f57227875333eb39) |  [PR#1154](https://github.com/USEPA/CMAQ_Dev/pull/1154) |

### Emissions Diagnostics and Log Output
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Diagnostic and Log Updates  
**Release Version**:  v5.5 
 
**Description**:  
Several issues with emissions diagnostics were identified by internal developers and external users. These have been resolved. Issues include:

- Process analysis errors when PA_BLEV > 1 and emissions are restricted to layer 1 only.  This issue was first identified on the CMAS User Forum: https://forum.cmascenter.org/t/really-large-ipr-emis-results-for-upper-layers/
- Inconsistent time vector for B3GTS. On the CONUS domain, it was observed to equal 25 or 26 hours on random days. It should be 24 hours.
- Timing on lightning diagnostic files starting at 00000 instead of 10000.
- Formatting of DESID scale factors in log file has always been F6.3. Users have complained for some time. This is updated to ES9.2.
- The EMVAR molecular weight table defined in desid_vars.F is now assigned with individual operational lines instead of one continuous parameter statement in the module specification section. This update will avoid Fortran continuation line limit issues in the future if the number of emission species continues to expand.
- Adding space for environment variables like the symbolic date labels to be printed completely in the log files

**Significance and Impact**: These updates improve consistency among diagnostic output files and improve readability of the log files. 
 
|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1077](https://github.com/USEPA/CMAQ/commit/1eef012a93faf0f7f9b523fede916fb5cd890fef) | [PR#1077](https://github.com/USEPA/CMAQ_Dev/pull/1077)  |  

## Add precision to timing metrics in logfiles
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Improvement (Minor log formatting change)  
**Release Version/Date**: v5.5  

**Description**: This PR adds three decimal places of precision to the process-level timing metrics in the ascii logfile.

**Significance and Impact**: At high computational efficiency, the default precision provided for the timing metrics in the logfile was yielding 0.0 for some processes. When aggregated, this underestimates the time taken by these processes.  
 
|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#961](https://github.com/USEPA/CMAQ/commit/cf37d49e144b3aed1380c6a74f404063f5e047bf) | [PR#961](https://github.com/USEPA/CMAQ_Dev/pull/961)  | 

## Improvement of Logfile output and error reporting
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix and Log File Improvements  
**Release Version**: CMAQv5.4  

**Description**: 
- Propagated SHA ID from git repository to configuration file and execution ID to support versioning and matching code state to results.
- Propagated (mostly documentation) improvements to v5.4 branch from existing v5.3 release branch. 
- Added M3EXIT output to Main logfile to improve discoverability.
  
**Significance and Impact**: No impact on results for the cases tested.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#639](https://github.com/USEPA/CMAQ/commit/3dc2bb6e3d3041bbbf0729643cc38bb2c52b2e11) | [PR#639](https://github.com/USEPA/CMAQ_Dev/pull/639)  | 
|[Merge for PR#637](https://github.com/USEPA/CMAQ/commit/6bf6a3c367cb5fae088396c879e1c9609766a5dd) | [PR#637](https://github.com/USEPA/CMAQ_Dev/pull/637)  | 
|[Merge for PR#769](https://github.com/USEPA/CMAQ/commit/c5bce3ef77dc54b29bf66046d07f766afc2d9f61) | [PR#769](https://github.com/USEPA/CMAQ_Dev/pull/769)  | 
