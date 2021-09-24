# Running with temporally fine scale MET (Enhancement)
 
**Author/P.O.C.:** [D. Wong](mailto:wong.david-c@epa.gov), Center for Environmental Measurement and Modeling (CEMM), U.S. EPA
 
## Brief Description
The time stepping algorithm as of CMAQv4.7+, would incorrectly run if given temporally finer meteorology inputs than expected by the output time step. If the user set the output time step to be equal to the temporal time step of the incoming meteorology file, the model would correctly run but would also output data at the temporal frequency of the incoming meteorology file. This is often undesirable as it can be burdensome for the I/O of the model and or the underlying disk space needed to store this data. To decouple these two processes a new environmental variable allowing users to control the [MET_TSTEP](../Users_Guide/Appendix/CMAQ_UG_appendixA_model_options.md#timestep-configuration) is introduced. The general algorithm is discussed below: 

```
      CMAQ_Main
      Computes the number of steps the DRIVER is called (NSTEPS)
      NSTEPS = RUNLEN/min(output step, input metstep)
      e.g., for a 24 hr simulation with input met data every 10min and output every hour, DRIVER will be called 144 times (86400/600). This informs the driver how often the model synchronization step (TSPEP(2)) needs to be recalculated. The synchronization step defines a model step for which all processes are integrated within the time/process split paradigm where users can specify bounds for the synchronization step via specifying the environment variables: MAXSYNC and MINSYNC .
      
      In the call to DRIVER, MODEL_STEP (or the output step or TSTEP(1)) is passed.
      
      DRIVER
      Functions: initializes model, computes synchronization step (TSPEP(2)), calls SCIPROC for the duration of the model step (NREPS x TSTEP(2))
      DRIVER calls ADVSTEP to computes the synchronization step, NREPS, and the advection step (ASTEP) that satisfies the specified CFL condition
```

 
## Significance and Impact
The model now can run with temporally finer or coarser meteorology inputs without having to compromise model I/O time or system disk space.
 
## Affected Files
#### Files modified:
 modified: CCTM/src/driver/advstep.F
 modified: CCTM/src/driver/cmaq_main.F
 modified: CCTM/src/cio/centralized_io_module.F
 modified: CCTM/src/util/util/RUNTIME_VARS.F
 modified: CCTM/scripts/run_cctm_2010_4CALIF1.csh 
 modified: CCTM/scripts/run_cctm_2011_12US1.csh 
 modified: CCTM/scripts/run_cctm_2014_12US1.csh 
 modified: CCTM/scripts/run_cctm_2015_HEMI.csh 
 modified: CCTM/scripts/run_cctm_2016_12US1.csh 
 modified: CCTM/scripts/run_cctm_Bench_2011_12SE1.csh 
 modified: CCTM/scripts/run_cctm_Bench_2016_12SE1.csh 

#### Files added:
 None.
 
#### Files deleted:
 None.  
 
## References:
 None.      
-----
## Internal Records:
#### Relevant Pull Requests:
[PR #616](https://github.com/USEPA/CMAQ_Dev/pull/566)  
 
#### Commit IDs:
[f1a9cf0378a4615d6582d9e77c8781312a0734a0] (https://github.com/USEPA/CMAQ_Dev/pull/616/f1a9cf0378a4615d6582d9e77c8781312a0734a0)  
[b094cfce2f62f36f97465258bddd966b5a273f10] (https://github.com/USEPA/CMAQ_Dev/pull/616/b094cfce2f62f36f97465258bddd966b5a273f10)  
[23cbf108d3097e71e8e684193737e0ab9b800578] (https://github.com/USEPA/CMAQ_Dev/pull/616/23cbf108d3097e71e8e684193737e0ab9b800578)  
[670a21251d9fe66a95f9046616895538ff8d0798] (https://github.com/USEPA/CMAQ_Dev/pull/616/670a21251d9fe66a95f9046616895538ff8d0798)
[744940656deeeca3a5e49c03b131801bc49fc83d] (https://github.com/USEPA/CMAQ_Dev/pull/616/744940656deeeca3a5e49c03b131801bc49fc83d)
[d3ad4023c304f12e9d5139fc2fb9617f59ddc377] (https://github.com/USEPA/CMAQ_Dev/pull/616/d3ad4023c304f12e9d5139fc2fb9617f59ddc377)
