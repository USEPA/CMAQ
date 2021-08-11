# Enable column modeling

[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency


## Brief Description

The changes allow running CCTM with ncols and nrows to equal one, i.e., one vertical column.

Using a column model is accomplished by changing the GRID_NAME in the cctm run-script.
Given below is an example for the 12SE1 Benchmark domain

         setenv GRID_NAME CELLBENCH 

where the GRIDDESC file defines CELLBENCH as below

      'CELLBENCH'
      'LamCon_40N_97W'    792000.000  -1080000.000     12000.000     12000.000   1   1   1

The example places the column at origin of the Benchmark domain. **If cctm windowing function worked, a model
column can be located at any of the vertical columns within the domain.** Running a column model only
requires one processor (_it stops with a run time error if multiple processors are used_) and the
cctm run-script can be executed at the prompt or interactively. Typical run times for a 24 hour simulation vary 
between 30 to 60 seconds for the base model and the Intel compiler dependent on whether the model was compiled using standard or debug flags.

## Significance and Impact

The code changes intent to support  developing new cloud, aerosol or chemistry processes in _sciproc_ or diagnosing existing ones. 

## Affected Files

* CCTM/src/driver/advstep.F
* CCTM/src/driver/sciproc.F
* CCTM/src/grid/cartesian/HGRD_DEFN.F
* CCTM/src/util/util/RUNTIME_VARS.F
* CCTM/src/vdiff/acm2_m3dry/VDIFF_MAP.F
* CCTM/src/vdiff/acm2_stage/opddep.F

# Internal Records
#### Relevant Pull Requests:
[PR #605](https://github.com/usepa/cmaq_dev/pull/605)

#### Commit IDs:
57a9c5161e609badc69a31c0475a7e5dc08227e2 
100f4269d796ceeeab489935ab2344bf6c6aa15b 
0c7bbf7a24ecfdc1066ee51fb201d92b3c1b41a1
0c94012c6f916b1740f555dd2b941a21a7cda662 
1a490da1c7b6148a82411380fdb38fd45f2846c5 
742393cfd47d900b597dedc27f8a34647ca63f29 


-----------------------
