# WRF-CMAQ Coupled Model

## Compatibility issues with WRF versions 4.5.2 and later
[David Wong](mailto:wong.david-C@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5  
**Description**: An update in WRFv4.5.2 (issued in [PR 1953](https://github.com/wrf-model/WRF/pull/1953) to the wrf-model GitHub repository) causes a run time error when using the WRF-CMAQ coupled model with WRF versions 4.5.2 and later.

A workaround for this incompatibility issue is to revert this WRF update when using the WRF-CMAQ system.  To do this first download or clone the code for WRFv4.5.1. Next copy the following two files from the WRFv4.5.1 code base to your folder containing the more recent version WRF (e.g., 4.6.1). For example, if your WRF code for the two versions are saved in folders 'WRFv451' and 'WRFv4.*', execute the following commands:   
 
```
cp WRFv451/share/wrf_tsin.F  WRFv4.*/share/wrf_tsin.F
cp  WRFv451/frame/module_domain_type.F WRFv4.*/frame/module_domain_type.F
```
where * can be 4.5.2, 4.5.3, 4.6, and 4.6.1.

**Significance and Impact:**  
This solution will avoid the run time error in WRF-CMAQ, however it also means you will not be working with the latest WRF updates to these two files.  

**Additional Notes:**
EPA's testing of WRFv4.5.1-CMAQv5.5 has included chemical mechanisms CB6r5 and CRACMMv1 with the M3DRY dry deposition scheme. Other model options can be used with the WRF-CMAQ model but will have limited user support for issues that are encountered. 
 
## Remove gcc bug for CORE_SHELL_OPTICS option
[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5  
**Description**: When the runtime option CORE_SHELL_OPTICS is set to True and CCTM is compiled with the gcc 6.1 compiler, the model crashes. The cause is a write to the mystr character variable in the BHCOAT subroutine because the value has an insufficient length.  

**Significance and Impact:**  The change allows running CCTM with gcc 6.1 compiler where CORE_SHELL_OPTICS is set to True. Using the intel 18.0, tests showed the change does not alter model predictions over the 2018 12NE3 and 2015 HEMI domains.  
 
|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1021](https://github.com/USEPA/CMAQ/commit/ef4565bc785ccd5367311e429a49b7b3ccd54ded) | [PR#1021](https://github.com/USEPA/CMAQ_Dev/pull/1021)  | 
    

## New WRF-CMAQ model using WRFv4.4 and CMAQv5.4
[David Wong](mailto:wong.david-c@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: New Feature  
**Release Version/Date**: CMAQv5.4  
**Description**: The new WRF-CMAQ model is based on WRFv4.4 and CMAQv5.4. It supports only RRTMG radiation scheme for short wave aerosol direct effect. It uses core-shell model to perform aerosol optics calculations rather than volume mixing technique as in the previous version of the WRF-CMAQ model.

The code used to couple the WRFv4.4-CMAQv5.4 models is now released as part of the CMAQ Github Repository.

Starting from WRF V4.4 and CMAQ v5.4, users can construct the coupled model with any version of WRF (v4.4 or later) and any version of CMAQ (v5.4 or later) with the following steps:

* download the desirable version of WRF
* download the desirable version of CMAQ
* build CMAQ by executing bldit_cctm.csh with build_twoway turns on
* move the built CMAQ code, BLD* into WRF direction with the name cmaq
* setenv WRF_CMAQ 1
* setenv IOAPI the_path_IOAPI     (for example: /home/wdx/lib/x86_64/gcc-9.1/ioapi_3.2)
* setenv WRFIO_NCD_LARGE_FILE_SUPPORT 1
* configure
* compile em_real

The "setenv WRF_CMAQ 1" must be there when you compile or recompile code and the "setenv IOAPI the_path_IOAPI" must be there before typing configure.

A complete step by step build process and run instructions are provided in the [WRF-CMAQ Tutorial](../Users_Guide/Tutorials/CMAQ_UG_tutorial_WRF-CMAQ_Benchmark.md).

**Significance and Impact**  This simplifies the WRF-CMAQ coupled model construction process and lets users have choices to build the coupled model with a desirable version of WRF and CMAQ.
 
**Internal PRs**: N/A

## WRF-CMAQ model Aerosol Feedback Bugfix
[David Wong](mailto:wong.david-c@epa.gov@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.4  
**Description**: A bug was identified within the CMAQ to WRF coupling routine (twoway_feedback.F90) where aerosol feedback information is transferred from CMAQ to WRF. In doing so, it was found that WRF was not receiving the correct aerosol feedback information due to a looping error relating to the number of layers set to 1 in some cases. 

Specifically, The 3-way nested loop in the subroutine feedback_read (twoway_feedback.F90) assigns aerosol feedback information to the WRF grid structure. The outer loop runs from 1 to NLAYS3D which it is an IOAPI internal variable, and the value of NLAYS3D is establish at the end of calling IOAPI subroutine DESC3. Hence its value might differ from the value obtained in the FIRTIME block and subsequent time step. 

Similarly the last block of code in the feedback_read subroutine suffers a similar problem, i.e. the top layer + 1 does not get updated properly.

**Additional information on the bug and the code fix is posted on the [CMAS User Forum](https://forum.cmascenter.org/t/important-note-wrf-cmaq-coupled-model-bug/3617).**

**Significance and Impact**: The bug impacts the WRF-CMAQ coupled system in the CMAQv5.3 release series (v5.3, v5.3.1, v5.3.2, v5.3.3) when running with short wave radiative feedback. The bug was not present in prior WRF-CMAQ versions.  The bugfix in CMAQv5.4 now correctly captures the variations in the aerosol optical properties and consequently the direct feedback effects through all layers.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#951](https://github.com/USEPA/CMAQ_Dev/commit/50cf578877c377fb00c74619e60ae511ab14dd3e) | [PR#951](https://github.com/USEPA/CMAQ_Dev/pull/951)  |   
