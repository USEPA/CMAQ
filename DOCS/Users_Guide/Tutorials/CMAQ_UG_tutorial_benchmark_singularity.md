# CMAQ Benchmarking Tutorial using Singularity

Purpose: This guide describes how to install and run the CMAQ test case using Singularity. 

Why use a container, and why use Singularity over other Containers such as Docker?

Containers are stand-alone software environments that contain all the software required to run a package in different environments. Because containers are designed to be self-contained and allow execution as if a native program or script on a host system it comes with several advantages including mobility, reproducibility, configurability and performance.

However, containers such as Docker often required elevated (i.e. root access) security privileges to start and run a in an environment, making this feature a security risk in large HPC clusters.

To combat this, Lawrence Berkeley National Laboratory (LBNL) developed and popularized an open source container system aimed at HPC users and academic sites called Singularity. 

Singularity allows users to run the container <b>without root privileges</b> as well as seamless interaction with cluster schedulers such as SLURM, Torque, etc.

To achieve this, Singularity takes advantage of the split between the user space and kernal in UNIX operating systems. Singularity containers take the user space and make it a swappable component independent of the kernal, unlike traditional operating systems. This means the that the root path ('/') will be different than that of your host.

When you build a Singularity container, by default the software will try to resolve which parts are the user space and the kernal and will automatically mount some directories. By default the swappable components (i.e. the user space) is defined as the following directories:
```
· /home
· /sys
· /proc
· /tmp
· /var/tmp
· /etc/resolv.conf
· /etc/password
· $PWD
```


However, you can mount additional directories by specifying them as a 'bind' when you BUILD THE container. 

For example, the /work directory on ATMOS is not a default directory that is mounted, but additionally specified by the user building the container. Look at /home/local-rhel7/apps/singularity/singularity-3.1.1/etc/singularity/singularity.conf for an example of Singularity system configuration file, user who installed singularity can modify.

Question 1: Is it the person who builds the container that does this modification, or the IT staff who installed Singularity on the Compute Server? (Because it is on /home, I think it is the container builder who modifies this file.)

On UNC systems the work directory is /proj/ie/proj/ or /21dayscratch/scr

Question 2: Is it possible to build a container on a system that does not have  /proj or /21dayscratch to bind that directory to make it available as user space on a different compute server?

Answer from Mike Waldron:
You should be able to map local directories when you run the containers.

 

Here is an example I use for running a tensorflow container on longleaf.
$ cat run_interactive.slurm
#!/bin/bash
## This is an example of a script to run  tensorflow interactively
## using Singularity to run the tensorflow image.
##
## You will be dropped into an interactive shell with the tensorflow environment.
##
## Make a copy of this script and change reserved resources on the srun command as needed for your job.
##
## Just execute this script on the command line.

unset OMP_NUM_THREADS

# Set SIMG path
SIMG_PATH=/nas/longleaf/apps/tensorflow/1.14.0/simg

# Set SIMG name
SIMG_NAME=tensorflow1.14.0-cuda10.0-ubuntu16.04.simg

# Run interactive job to GPU node using Singularity

srun --ntasks=1 --cpus-per-task=1 --mem=4G --time=4:00:00 --partition=volta-gpu --gres=gpu:1 --qos=gpu_access --pty singularity shell --nv -B /pine -B /proj $SIMG_PATH/$SIMG_NAME  

Note the '-B' arguments to the singularity shell command. In this case, it is providing access to /pine and /proj to the container.

Question 2: Would that work for CMAQ, would the OUTPUT directory be written to the /proj directory rather than the /home directory?

Question 3: How do we create a generic work directory for use on other systems?

To build a Singularity Container, YOU MUST HAVE ROOT Privileges. If you have those privileges you should follow the following step in the admin documentation below.

## System Checks

The following support software are required for compiling and running CMAQ using Singularity.
 
1. Singularity
   - Must have root permission to install singularity on compute server (requires assistance from your IT department)
   - Help guides for IT staff
   	- https://sylabs.io/guides/3.5/admin-guide.pdf
   	- https://singularity.lbl.gov/install-linux
   - Singularity Version
        - Singularity used by Carlie to build container:  3.0.2
        - Singularity version used on Atmos: singularity-3.1.1
        - Encountered a bug (https://github.com/hpcng/singularity/issues/2744) 
           when tried to run his container on Dogwood when the singularity version was 


To determine the version of singularity on your machine, use the command:
```
singularity --version
2.5.1-dist
```
 
   -  Uniqueness in how Singularity and SLURM is set up between Atmos and Dogwood 
Dogwood - Can only access singularity commands from within a SLURM queue
Atmos - Can access singularity commands from the login node

2. OpenMPI
3. Slurm


## Configure the CMAQ build environment

The user has two options for building an environment. She or he may build and run CMAQ components directly in the repository structure (object files and executables will be ignored with .gitignore), or they may extract the build and run scripts out of the repository and work in a separate location. If you would like to build directly in the repository, skip to "Install the CMAQ Libraries" below.

### Build and run in a user-specified directory outside of the repository
In the top level of CMAQ_REPO, the bldit_project.csh script will automatically replicate the CMAQ folder structure and copy every build and run script out of the repository so that you may modify them freely without version control.

In bldit_project.csh, modify the variable $CMAQ_HOME to identify the folder that you would like to install the CMAQ package under. For example:
```
set CMAQ_HOME = /home/username/CMAQ_v5.3.2
```
Now execute the script.
```
./bldit_project.csh
```


#### Configure CMAQ benchmark Science Modules:

The build directory parameters for the benchmark test case include the following:

-   Multiprocessor simulation 
-   3-D Advection Scheme: wrf_cons
-   Horizontal diffusion: Multiscale
-   Vertical diffusion: ACM2_M3Dry
-   Deposition: M3Dry
-   Chemistry solver: EBI
-   Aerosol module: AERO7
-   Cloud module: ACM_AE7
-   Mechanism: cb6r3_ae7_aq
-   Online biogenic emissions
-   Inline plume rise

To configure these parameters, the CCTM Science Modules within the bldit_cctm.csh need to be set. The comments within the script itself should help guide the user on the options for each variable and how to set them. Further information on variable names can be found in 
[Appendix A](../Appendix/CMAQ_UG_appendixA_model_options.md).

Following the requisite changes to the CCTM build script, use the following command to create the CCTM executable: 

```
cd $CMAQ_HOME/CCTM/scripts
./bldit_cctm.csh [compiler] [version] |& tee bldit.cctm.log
```

## Configure the CCTM script 

For an MPI configuration with 8 processors,

```
cd $CMAQ_HOME/CCTM/scripts
```

Edit the CCTM run script (run_cctm_Bench_2016_12SE1.csh) for the MPI configuration that you will use:

```
@ NPCOL 4 ; @ NPROW = 2
```

Most clustered multiprocessor systems require a command to start the MPI run-time environment. The default CCTM run script uses the *mpirun* command. Consult your system administrator to find out how to invoke MPI when running multiprocessor applications.

For single-processor computing, set PROC to serial:

```
set PROC     = serial
```

CCTM Science Configuration Options set to **Y** in the RunScript for the benchmark case include the following: 

-  ```CTM_OCEAN_CHEM``` - use ocean halgoen chemistry and sea spray aerosol emissions
-  ```KZMIN``` - minimum eddy diffusivity in each grid cell determined by land use fraction
-  ```PX_VERSION``` - WRF PX land surface model 
-  ```CTM_ABFLUX``` - bidirectional ammonia flux for online deposition velocities
-  ```CTM_BIDI_FERT_NH3``` - subtract fertilizer NH3 from emissions because it will be handled by the BiDi calculation
-  ```CTM_SFC_HONO``` - surface HONO interaction
-  ```CTM_GRAV_SETL``` - vdiff aerosol gravitational sedmentation
-  ```CTM_BIOGEMIS``` - online biogenic emissions

To configure these parameters, the Science Options within the $CMAQ_HOME/CCTM/scripts/run_cctm_Bench_2016_12SE1.csh need to be set. The comments within the script itself should help guide the user on the options for each variable and how to set them. Further information on variable names can be found in 
[Appendix A](../Appendix/CMAQ_UG_appendixA_model_options.md).

The Emissions Control Namelist is located in the BLD directory.

After configuring the MPI settings for your Linux system, check the rest of the script to ensure the correct path, date and names are used for the input data files. Per the note above, different Linux systems have different requirements for submitting MPI jobs.  The command below is an example of how to submit the CCTM run script and may differ depending on the MPI requirements of your Linux system. 

```
./run_cctm_Bench_2016_12SE1.csh |& tee cctm.log
```

## Confirm that the Benchmark Simulation Completed

To confirm that the benchmark case ran to completion view the run.benchmark.log file. For MPI runs, check each of the CTM_LOG_[ProcessorID]*.log files. A successful run will contain the following line at the bottom of the log(s):

``>>---->  Program completed successfully  <----<<``

Note: If you are running on multiple processors the log file for each processor is also moved from the $CMAQ_HOME/CCTM/scripts directory to the benchmark output directory: 

```
$CMAQ_DATA/output_CCTM_v532_[compiler]_Bench_2016_12SE1
```
and these log files have the name convention: 

```
CTM_LOG_[ProcessorID].v532_[compiler]_[APPL]_[YYYYMMDD]
CTM_LOG_[ProcessorID].v532_gcc_Bench_2016_12SE1_20160701
```

The benchmark output results will have been placed in the directory: 

```
$CMAQ_DATA/output_CCTM_v532_[compiler]_Bench_2016_12SE1
```

and can include upto 23 netCDF-type files: ACONC, AOD_DIAG, APMDIAG, APMVIS, B3GTS_S, CGRID, CONC, DEPV, DRYDEP, DUSTEMIS, LTNGCOL, LTNGHRLY, MEDIA_CONC, PHOTDIAG1, PHOTDIAG2, PMDIAG, PMVIS, SOILOUT, SSEMIS, VDIFF, VSED, WETDEP1, and WETDEP2.


Common errors in a CCTM simulation include the following:
- Incorrect paths to input files. Look in the CCTM screen output (capture in your log file) for an Error message about an input file not being found.  
- Incorrect MPI implementation. A series of MPI errors at the end of the log file often indicate that the MPI job was not submitted correctly.   

Check the last few lines of the CCTM output log for messages to help diagnose why the simulation did not complete.

## Check the CMAQ Benchmark Results

To determine if CMAQ is correctly installed on your Linux system compare the results from your benchmark simulation to the reference output data downloaded from the CMAS Center. This data was generated on a Linux system with the following specifications:
- Red Hat Enterprise Linux Server release 7.8 (Maipo)
- Linux Kernel 3.10.0-1062.12.1.el7.x86_64
- GNU GCC compiler version 9.1.0, 8 processors with OpenMPIv4.0.1 and I/O APIv3.2
- Debug mode turned off (```set Debug_CCTM``` commented out in $CMAQ_HOME/CCTM/scripts/bldit_cctm.csh)
- CMAQv5.3.2

The CMAQv5.3.2 reference output data includes a set of CCTM_ACONC_\*.nc files with layer 1 average model species concentrations for each model hour for 226 variables and a set of CCTM_WETDEP1_\*.nc files with cumulative hourly wet deposition fluxes for an additional 136 variables.

Use your netCDF evaluation tool of choice to evaluate your benchmark results. For example, [VERDI](https://www.verdi-tool.org/) is a visualization tool to view CCTM results as tile plots. Statistical comparison of the results can be made with the I/O API Tools or R. 

Note, even with a successful installation and run of the benchmark case, some differences between your simulation and the reference data can occur due to differences in domain decomposition for multi-processor simulations as well as differences in compiler optimization.  These differences tend to manifest in upper layers of the model and are mostly found in predicting aerosol water (AH2O) and aerosol acidity (AH3OP), while differences are smaller for other key species like ASO4, ANO3, ACL, ALOO1, etc. These species have short atmospheric lifetimes with large changes in time and space derivatives or have model physics sensitive to small changes in concentration. Predicting these species is more sensitivity to small changes in machine precision and accuracy.

