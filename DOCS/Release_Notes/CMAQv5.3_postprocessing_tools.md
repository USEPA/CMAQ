# Updates to Post-processing Utilities

[Christian Hogrefe](mailto:hogrefe.christian@epa.gov), U.S. Environmental Protection Agency

## Brief Description
*hr2day* was updated to handle multiple model input files and
(optional) processing start and end days.

*sitecmp* and *sitecmp_dailyo3* were updated to enable reading of both
csv-formatted and tab-delimited SITE_FILEs, which, in
turn, enables the passing-through of (optional) state, county,
and elevation information from the csv SITE_FILEs to the
output file.

The new *calc_tmetric* program creates gridded I/O API files with
temporally averaged or summed values that are calculated from
one or more gridded time-dependent I/O API files.

Updated the maximum allowable number of I/O API input files set in
*module_file* used by *block_extract*, *calc_tmetric*, *combine*,
*hr2day*, *sitecmp*, and *sitecmp_dailyo3* to comply
with the global I/O API maximum value of MXFILE3. For the public
I/O API release, MXFILE3 is defined as 64. To allow the
opening of an output file, the maximum number of input files was
set to MXFILE3 - 1 in module_file.                   

## Significance and Impact
Allows for additional functionality in *hr2day*, *sitecmp*,
*sitecmp_dailyo3*, as summarized above and described in more
detail in the updated README files for each of these tools.
The new *calc_tmetric* program provides a quick way to compute
monthly or annual averages or sums from one or more gridded
time-dependent I/O API files.
                       

## Affected Files
calc_tmetric/scripts/bldit_calc_tmetric.csh  
calc_tmetric/scripts/run_calc_tmetric.csh  
calc_tmetric/src/calc_tmetric.F  
calc_tmetric/src/ck_ctms.F  
calc_tmetric/src/module_grid.F  
calc_tmetric/src/module_spec.F  
calc_tmetric/src/module_tstep.F  
calc_tmetric/src/parser.F  
block_extract/README.md  
block_extract/src/module_file.f  
calc_tmetric/README.md  
calc_tmetric/src/module_file.F  
combine/README.md  
combine/src/module_file.F  
hr2day/README.md  
hr2day/src/module_file.F  
hr2day/scripts/bldit_hr2day.csh  
hr2day/scripts/run_hr2day.csh  
hr2day/src/hr2day.F  
hr2day/src/module_evaluator.F  
sitecmp/README.md  
sitecmp/src/module_file.F  
sitecmp/scripts/bldit_sitecmp.csh  
sitecmp/scripts/run_sitecmp_AQS_Hourly.csh  
sitecmp/src/module_sites.F  
sitecmp/src/process.F  
sitecmp_dailyo3/README.md  
sitecmp_dailyo3/src/module_file.F  
sitecmp_dailyo3/scripts/bldit_sitecmp_dailyo3.csh  
sitecmp_dailyo3/scripts/run_sitecmp_dailyo3_AQS.csh  
sitecmp_dailyo3/scripts/run_sitecmp_dailyo3_CASTNET.csh  
sitecmp_dailyo3/src/module_sites.F  
sitecmp_dailyo3/src/process.F  


-----
## Internal Records:
#### Relevant Pull Requests:
[PR #343](https://github.com/USEPA/CMAQ_Dev/pull/343)  

#### Commit 
IDs:  2819c7c0e4597ff8b4767e902df64c2498c3bb43 (https://github.com/USEPA/CMAQ_Dev/commit/2819c7c0e4597ff8b4767e902df64c2498c3bb43#diff-bc1fef7602eb41d9b4879f71d0c6029b)  
      552bb703137c29a3ba6dc9a7b73380d0dca51801 (https://github.com/USEPA/CMAQ_Dev/commit/552bb703137c29a3ba6dc9a7b73380d0dca51801#diff-bc1fef7602eb41d9b4879f71d0c6029b)  


-----

