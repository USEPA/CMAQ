## CMAQ Tutorial ##
### Add Chemically Inert Tracer Species to CMAQ ###
Purpose: This tutorial will step you through the process of adding chemically inert tracers to the CMAQ model. Additional details are provided in the CMAQ Operational Guidance Document (OGD) than can be found online (https://github.com/USEPA/CMAQ/blob/5.2/DOCS/User_Manual).


------------


### STEP 1: Create tracer namelist</strong>

Create namelist according to Table 8.4 in the CMAQ OGD (https://github.com/USEPA/CMAQ/blob/5.2/DOCS/User_Manual/CMAQ_OGD_ch08_input_files.md#Table8-4). Include one line for each tracer species with the following format (refer to the table below for more information on the abbreviations):

<a id=Table8-4></a>

<center> **Amended from Table 8-4. in CMAQ OGD** </center>

| **Column** |**Name** | **Type**| **Description** |
|---|---|---|---|
| 1 | SPC | String |Tracer Name|
| 2 | MOLWT | Integer |Tracer molecular weight|
| 3 | EMIS_SUR | String |Emissions surrogate species name for the tracer|
| 4 | EMIS_FAC | Real |Scaling factor for input emissions|
| 5 | DEPV_SUR | String |Deposition velocity surrogate species name for the tracer|
| 6 | DEPV_FAC | Real |Scaling factor for the deposition velocity|
| 7 | SCAV_SUR | String |Scavenging surrogate species name for the tracer|
| 8 | SCAV_FAC | Real |Scaling factor for scavenging|
| 9 | ICBC_SUR | String |IC/BC surrogate species name for the tracer|
| 10 | ICBC_FAC | Real |Scaling factor for the IC/BC concentration|
| 11 | G2AE_SUR | String |Gas-to-aerosol transformation species|
| 12 | G2AQ_SUR | String |Gas-to-aqueous transformation species|
| 13 | ADV | Yes/No |Advection switch|
| 14 | DIFF | Yes/No |Diffusion switch|
| 15 | DDEP | Yes/No |Dry deposition output file switch|
| 16 | WDEP | Yes/No |Wet deposition output file switch|
| 17 | CONC | Yes/No |Concentration output file switch|

The example namelist file shown below defines five tracer species.


```
&TR_nml

n_surr1 = 4,
n_surr2 = 2,
n_ctrl = 5,

TYPE_HEADER =
'SPC:MOLWT:EMIS_SUR:EMIS_FAC:DEPV_SUR:DEPV_FAC:SCAV_SUR:SCAV_FAC:ICBC_SUR:ICBC_FAC:T2AE_SUR:T2AQ_SUR:ADV:DIFF:DDEP:WDEP:CONC',
TYPE_MATRIX =
'O3_BC:48.0:::VD_O3:1.0:O3:1.0:O3_BC:1.0:::Yes:Yes:Yes:Yes:Yes',
'O3_BC_50PC:48.0:::VD_O3:1.0:O3:1.0:O3_BC_50PC:1.0:::Yes:Yes:Yes:Yes:Yes',
'CO_BC:28.0:::VD_CO:1.0:CO:1.0:CO_BC:1.0:::Yes:Yes:Yes:Yes:Yes',
'O3_IC :48.0:::VD_O3:1.0:O3:1.0:O3_IC:1.0:::Yes:Yes:Yes:Yes:Yes',
'ICT_50PPB:1.0:::::::ICT_50PPB:1.0:::Yes:Yes:::Yes',
'CO_EMIS:28.0:CO:1.0:VD_CO:1.0:CO:1.0:::::Yes:Yes:Yes:Yes:Yes'
/

```

**The first tracer species O3_BC**
  * is defined to have the same molecular weight as ozone
  * is not mapped to any emissions species
  * uses the ozone dry deposition velocity (VD_O3) scaled with a factor of 1 as its surrogate for dry deposition
  * uses ozone as its scavenging surrogate scaled with a factor of 1
  * will try to obtain its initial and boundary conditions from a species named O3_BC in the initial and boundary condition files
  * does not participate in gas-to-aerosol or gas-to-aqueous transformations
  * will undergo advection and diffusion
  * will be written to the DDEP, WDEP, and CONC output files

**The second tracer species O3_BC_50PC**
  * is defined to have the same molecular weight as ozone
  * is not mapped to any emissions species
  * uses the ozone dry deposition velocity (VD_O3) scaled with a factor of 1 as its surrogate for dry deposition
  * uses ozone as its scavenging surrogate scaled with a factor of 1
  * will try to obtain its initial and boundary conditions from a species named O3_BC_50PC in the initial and boundary condition files
  * does not participate in gas-to-aerosol or gas-to-aqueous transformations
  * will undergo advection and diffusion
  * will be written to the DDEP, WDEP, and CONC output files

**The third tracer species CO_BC**
  * is defined to have the same molecular weight as CO
  * is not mapped to any emissions species
  * uses the CO dry deposition velocity (VD_CO) scaled with a factor of 1 as its surrogate for dry deposition
  * uses CO as its scavenging surrogate scaled with a factor of 1
  * will try to obtain its initial and boundary conditions from a species named CO_BC in the initial and boundary condition files
  * does not participate in gas-to-aerosol or gas-to-aqueous transformations
  * will undergo advection and diffusion
  * will be written to the DDEP, WDEP, and CONC output files  

**The fourth tracer species O3_IC**
  * is defined to have the same molecular weight as ozone
  * is not mapped to any emissions species
  * uses the ozone dry deposition velocity (VD_O3) scaled with a factor of 1 as its surrogate for dry deposition
  * uses ozone as its scavenging surrogate scaled with a factor of 1
  * will try to obtain its initial and boundary conditions from a species named O3_IC in the initial and boundary condition files
  * does not participate in gas-to-aerosol or gas-to-aqueous transformations
  * will undergo advection and diffusion
  * will be written to the DDEP, WDEP, and CONC output files  

**The fifth tracer species ICT_50PPB**
  * is defined to have a molecular weight of 1 g/mole
  * is not mapped to any emissions species
  * is not mapped to any dry deposition velocity surrogate, i.e. does not undergo dry deposition
  * is not mapped to any scavenging surrogate, i.e. does not undergo scavenging
  * will try to obtain its initial and boundary conditions from a species named ICT_50PPB in the initial and boundary condition files
  * does not participate in gas-to-aerosol or gas-to-aqueous transformations
  * will undergo advection and diffusion
  * will not be written to the DDEP and WDEP output files
  * will be written to the CONC output file  

**The last tracer species CO_EMIS**
  * is defined to have the same molecular weight as CO
  * uses CO emissions scaled with a factor of 1 as its emission surrogate
  * uses the CO dry deposition velocity (VD_CO) scaled with a factor of 1 as its surrogate for dry deposition
  * uses CO as its scavenging surrogate scaled with a factor of 1
  * is not mapped to any initial/boundary condition surrogate
  * does not participate in gas-to-aerosol or gas-to-aqueous transformations
  * will undergo advection and diffusion
  * will be written to the DDEP, WDEP, and CONC output files  

### STEP 2: Add tracers to initial condition, boundary condition, and/or emission files

Depending on the desired application, the emission surrogate and IC/BC surrogate defined in the tracer namelist for each tracer need to be added to the corresponding CMAQ input files, i.e. the emissions, initial condition, and/or boundary condition files.

None of the example tracers defined in STEP1 have any emission surrogate that is not already contained in a standard emission file (the first five tracers to not have any emission surrogate while the sixth tracer uses CO as its emission surrogate). The first five tracers have a IC/BC surrogate that is not contained in standard initial or boundary condition files. This section provides two sample scripts that add the necessary species to an existing boundary condition file and an existing initial condition file.

Note that adding species (if any) to the initial condition file is only necessary for the first day of the simulation while boundary condition and/or emission species (if any) need to be added to the boundary condition and/or emission files for all days of the simulation. Since all tracers defined above will be written to the CGRID file using their names as defined in column 1 of the namelist file, and since the CGRID file will be used to provide initial conditions to CMAQ after the first day of simulation, the tracer species names defined in column 1 were also used as the names of the IC/BC surrogate in column 9 in the sample tracer namelist file for those tracers that use an IC/BC surrogate.   

#### Script to add O3_BC, O3_BC_50BC, and CO_BC to an existing boundary condition file ####

The script below uses the `combine` program to add species O3_BC, O3_BC_50BC, and CO_BC to an existing boundary condition file. O3_BC is set to be identical to the ozone concentration in the existing boundary condition file while 03_BC_50PC is set to be indentical to 50% of the ozone concentration in the existing boundary condition file. CO_BC is set to be identical to the CO concentration in the existing boundary condition file. O3_IC and ICT_50PPB are not added to the boundary condition file since these two tracers are designed to track the fate of initial conditions without any inflow from the boundaries. As is the case with any species defined in a namelist file, CMAQ uses a boundary condition of zero if the name of the IC/BC surrogate specified in column 9 is not available in the boundary condition file. The CO_EMIS tracer is designed to track the fate of CO emissions without any influence from initial or boundary conditions and therefore no IC/BC surrogate was specified and no additional species needs to be added to the boundary condition file for this tracer.
```
#!/bin/csh

#> Location of CMAQv5.2 benchmark case
 set CMAQ_DATA = $CMAQ_HOME/data
 set OUTDIR = $REPO_HOME/POST/combine/scripts/spec_def_files/

#> Set the working directory
 set BASE  = $cwd      

 cd $BASE; date; set timestamp; echo " "; set echo

#> Timestep run parameters.
 set YEAR     = 2011
 set MONTH    = 07
 set DAY      = 01
 set MET_YEAR = 11

#> Use GENSPEC switch to generate a new specdef file (does not generate output file).

 setenv GENSPEC Y

#> Define name of new species definition file to be created

 setenv SPECIES_DEF ${OUTDIR}/SpecDef_BCON_tracer.txt

 if (-e ${SPECIES_DEF}) 'rm' ${SPECIES_DEF}

#> Set the build directory if this was not set above 
#> (this is where the CMAQ executable is located by default).
 if ( ! -e $BINDIR ) then
  setenv BINDIR $CMAQ_HOME/POST/combine/scripts/BLD_combine_${VRSN}_${compiler}        
 endif                   


#> Define name of input and output files needed for combine program.

   setenv INFILE1 ${M3DATA}/SE52BENCH/icbc/BCON_$YEAR$MONTH$DAY_bench.nc

   setenv OUTFILE ${OUTDIR}/SE52BENCH/icbc/BCON_$YEAR$MONTH$DAY_added_tracer.nc


#> Executable call:
#>
#> In this first call, we only generate the specdef file that contains all the
#> species contained in the existing boundary condition file. OUTFILE is not
#> created

   /usr/bin/time $BINDIR/combine.${VRSN}.exe

#>
#> define the tracer species to be added to the boundary condition file using the
#> "combine" specdef syntax
#>

   echo "O3_BC            ,ppmV            ,O3[1], Variable O3_BC"            >! ${OUTDIR}/SpecDef_tracer.txt
   echo "O3_BC_50PC       ,ppmV            ,0.5 * O3[1], Variable O3_BC_50PC" >> ${OUTDIR}/SpecDef_tracer.txt
   echo "CO_BC            ,ppmV            ,CO[1], Variable CO_BC"            >> ${OUTDIR}/SpecDef_tracer.txt

#>
#> concatenate the specdep file containing the existing species and the file
#> containing the additional tracer species
#>

cat ${SPECIES_DEF} ${OUTDIR}/SpecDef_tracer.txt >! ${OUTDIR}/SpecDef_BCON_added_tracer.txt

#> Redefine the name of specdef file
 setenv SPECIES_DEF ${OUTDIR}/SpecDef_BCON_added_tracer.txt

#> Reset the GENSPEC switch to not generate a new specdef file but to generate an output file
 setenv GENSPEC N

#> Executable call:
#>
#> In this second call, the modified specdef file is used
#> and an output file containing all the original species
#> as well as the added tracer species is created

   /usr/bin/time $BINDIR/combine.${VRSN}.exe

#>
#> Remove the temporary file with the tracer definitions
#>

'rm' ${OUTDIR}/species_def_tracer.txt

 date
 exit()
```

#### Script to add O3_IC and ICT_50PPB to an existing initial condition file ####

The script below uses the `combine` program to add species O3_IC and ICT_50PB to an existing initial condition file. O3_IC is set to be identical to the ozone concentration in the existing initial condition file. ICT_50PPB is set to a constant mixing ratio of 0.05 ppm for all grid cells. O3_BC, O3_BC_50PC and CO_BC are not added to the initial condition file since these three tracers are designed to track their inflow from the boundaries without any initial mixing ratios within the modeling domain. As is the case with any species defined in a namelist file, CMAQ uses an initial condition of zero if the name of the IC/BC surrogate specified in column 9 is not available in the initial condition file. The CO_EMIS tracer is designed to track the fate of CO emissions without any influence from initial or boundary conditions and therefore no IC/BC surrogate was specified and no additional species needs to be added to the initial condition file for this tracer.

```
#!/bin/csh

#> Location of CMAQv5.2 benchmark case
 set CMAQ_DATA = $CMAQ_HOME/data
 set OUTDIR = $CMAQ_DATA/SE52BENCH

#> Set the working directory
 set BASE  = $cwd      

 cd $BASE; date; set timestamp; echo " "; set echo

#> Timestep run parameters.
 set YEAR     = 2011
 set MONTH    = 07
 set DAY      = 01 
 set MET_YEAR = 11

#> Use GENSPEC switch to generate a new specdef file (does not generate output file).

 setenv GENSPEC Y

#> Define name of new species definition file to be created

 setenv SPECIES_DEF ${OUTDIR}/SpecDef_CGRID_SE52BENCH.txt

 if (-e ${SPECIES_DEF}) 'rm' ${SPECIES_DEF}


#> Define name of input and output files needed for combine program.

   setenv INFILE1 ${CMAQ_DATA}/SE52BENCH/ref_output/cctm/CCTM_CGRID_v52_intel_SE52BENCH_$YEAR$MONTH$DAY.nc

   setenv OUTFILE ${OUTDIR}/SE52BENCH/CCTM_CGRID_v52_intel_SE52BENCH_added_tracer_$YEAR$MONTH$DAY.nc


#> Executable call:
#>
#> In this first call, we only generate the specdef file that contains all the
#> species contained in the existing boundary condition file. OUTFILE is not
#> created

   /usr/bin/time $BINDIR/combine.${VRSN}.exe

#>
#> define the tracer species to be added to the boundary condition file using the
#> "combine" specdef syntax
#>

   echo "O3_IC            ,ppmV            ,O3[1], Variable O3_IC"            >! ${OUTDIR}/species_def_tracer.txt
   echo "ICT_50PPB        ,ppmV            ,0.05, Variable ICT_50PPB"         >> ${OUTDIR}/species_def_tracer.txt

#>
#> concatenate the specdep file containing the existing species and the file
#> containing the additional tracer species
#>

cat ${SPECIES_DEF} ${OUTDIR}/species_def_tracer.txt >! ${OUTDIR}/species_CGRID_D51a_12CalnexBench_added_tracer.txt

#> Redefine the name of specdef file
 setenv SPECIES_DEF ${OUTDIR}/species_CGRID_D51a_12CalnexBench_added_tracer.txt

#> Reset the GENSPEC switch to not generate a new specdef file but to generate an output file
 setenv GENSPEC N

#> Executable call:
#>
#> In this second call, the modified specdef file is used
#> and an output file containing all the original species
#> as well as the added tracer species is created

   /usr/bin/time $BINDIR/combine.${VRSN}.exe

#>
#> Remove the temporary file with the tracer definitions
#>

'rm' ${OUTDIR}/SpecDef_tracer.txt

 date
 exit()

```

### STEP 3: Modify CMAQ run script</strong>

In the CMAQ run script, replace the default tracer namelist file `Species_Table_TR_0.nml` with the custom tracer namelist file created in STEP 1. Also replace the original input files (initial conditions, boundary conditions, and/or emissions) with the modified input files created in Step 2.
