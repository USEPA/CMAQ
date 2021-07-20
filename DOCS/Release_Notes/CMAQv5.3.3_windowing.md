# Windowing setting update

[David Wong](mailto:wong.david-c@epa.gov), U.S. Environmental Protection Agency

## Brief Description
Previous versions of CCTM (before CMAQv5.3) allowed users to specify gridded inputs (horizontal only) which were larger than the simulated domain. The model would then 
check if the simulated domain was contained in the larger domain (checking for matching grid resolution and projection parameters). If so, the model would then have extracted data corresponding to the simulated domain from the larger horizontal domain by identifying the corresponding subrectangle of the horizontal grid, online. This feature called "Windowing", is more often done offline, using I/O API tools such as [m3wndw](https://www.cmascenter.org/ioapi/documentation/all_versions/html/M3WNDW.html) and [bcwndw](https://www.cmascenter.org/ioapi/documentation/all_versions/html/BCWNDW.html) before running CMAQ.

Recent versions of the model, CMAQv5.3+, broke this functionality, allowing users to only window gridded inputs offline using I/O API. CMAQv5.3.3 reintroduces this functionality, allowing users to window gridded inputs online. Users must note, the windowing functionality does not apply to Chemical Boundary files. Users are responsible for generating their own domain specific Chemical boundary files!

More details about this new feature can be found in the [Users Guide](../Users_Guide/CMAQ_UG_ch04_model_inputs.md#431-windowing-capability). 

## Significance and Impact  
N/A - no impact on simulated results. 

## Affected Files
CCTM/src/cio/centralized_io_module.F

CCTM/src/util/util/subhfile.F

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #736](https://github.com/USEPA/CMAQ_Dev/pull/736)

-----
