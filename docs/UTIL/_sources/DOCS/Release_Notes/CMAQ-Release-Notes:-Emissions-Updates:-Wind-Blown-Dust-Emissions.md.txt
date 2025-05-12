### Windblown Dust Emissions 
[Jeff Willison](mailto:willison.jeffrey@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Science Update and Bug Fix  
**Release Version/Date**: CMAQv5.4  
**Description**: 
Several updates have been made to the windblown dust routine for CMAQ 5.4. The module has been updated to include additional PX soil texture information from WRF when available. The fugitive dust subroutines tfabove.F and tfbelow.F have been removed, since these were not originally intended to be used with windblown dust sources. 

The option of using BELD3 as a windblown dust input has been removed. BELD3 is outdated and in CMAQ 5.3 the windblown dust module did not support BELD4 or BELD5. Beginning in CMAQ 5.4 the necessary land use information for windblown dust is taken from MCIP input files or WRF. For CMAQ 5.4 we strongly recommend the use of WRFv4.1+ and the PX LSM when enabling windblown dust emissions. 

Lastly, a bug was corrected that was causing low erodibility values and significantly lower dust emissions when using WRFv4 inputs. 

**Significance and Impact**:

A consequence of removing BELD as an option from windblown dust is that the DUST_LU* files are no longer needed. They have been removed from the CCTM code and the run scripts.

The following plot summarizes the impact of the remaining changes above: 

![image](https://user-images.githubusercontent.com/47453034/192348532-00cc147a-df4f-47dc-a5d5-b57ff315a9ec.png)

Again, note, in the image above, the impacts of changing windblown dust input data from BELD to MCIP/WRF is not shown, but was not found to be a large contributor to the changes seen. As can be seen the largest change on modeled windblown dust is a result of the bug fix that was causing low erodibility values. 

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#877](https://github.com/USEPA/CMAQ/commit/632673aa0abc81a4f88223e67744a3744174708d) | [PR#877](https://github.com/USEPA/CMAQ_Dev/pull/877)  |