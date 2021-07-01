# Windowing setting update

[David Wong](mailto:wong.david-c@epa.gov), U.S. Environmental Protection Agency

## Brief Description
To provide a flexibility to allow each input can have different XORIG and YORIG 
settings than the simulation domain if it can be overlapped with the simulation 
domain perfectly w.r.t. domain resolution. Details about this new feature can
be found in the User Guide section 4.3.1.

## Significance and Impact  
Does not impact any simulation result.

## Affected Files
CCTM/src/cio/centralized_io_module.F

CCTM/src/util/util/subhfile.F

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #736](https://github.com/USEPA/CMAQ_Dev/pull/736)

-----
