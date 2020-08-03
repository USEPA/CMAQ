## CMAQ Tutorial 
### Strategies to Improve CMAQ Model Runtime 
Purpose: This tutorial shares common options and strategies that CMAQ developers recommend for improving model runtimes on common systems.


------------
#### Linux Environment Settings

```
limit stacksize unlimited 
```
#### Run time Process Configuration Settings

For these two variable, n should be >= m and m and n should be as close as SQRT(m*n) as possible.

```
@ NPCOL  = m; @ NPROW =  n
```

#### True Parallel IO Operation in CMAQ

To use this option, please go through this check list:

    * To build CMAQ, you need to uncomment parallel_io line in the bldit_cctm.csh script
    * Have PnetCDF installed and the underlying computing system should have parallel I/O file system, e.g. Lustre or Beefs
    * In the run script, all outfile definition should have MPI: prefix, e.g. setenv S_CGRID "MPI: $OUTDIR/CCTM_CGRID_${CTM_APPL}.nc"

Here are some example of CMAQ Parallel IO performance on various systems:


    
#### HPC Queue Manager Options

One consideration is to reserve the entire node that CMAQ is running on so that the simulation can make use of maximum resources. In the SLURM queue manager, you can use the following option.
```
#SBATCH --exclusive
```



