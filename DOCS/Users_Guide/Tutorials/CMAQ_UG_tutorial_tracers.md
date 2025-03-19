## CMAQ Tutorial ##
### Add Chemically Inert Gas Phase Tracer Species to CMAQ ###
Purpose: This tutorial will step you through the process of adding chemically inert gas phase tracers to the CMAQ model.  Inert tracers can be used as a development tool to test mass conservation when modifying processes in CMAQ.  They can also be used for exploratory analysis such as studying the persistence or impact of initial and/or boundary conditions on air quality model estimates, e.g., [Hogrefe et al. (2017)](https://doi.org/10.1016/j.atmosenv.2017.04.009), [Liu et al. (2018)](https://doi.org/10.5194/acp-18-17157-2018). Note that inert tracers are not a replacement for source contribution analysis (e.g., [CMAQ ISAM](../CMAQ_UG_ch11_ISAM.md)) that provides an estimate of how much of a chemically reactive modeled pollutant came from specific sources or processes ([Baker et al. (2015)](https://doi.org/10.1016/j.atmosenv.2015.10.055)).

------------
This tutorial will cover 5 different examples to illustrate how adding tracers to the ICBC and emissions input files are similar and highlight important differences to consider when developing a new application.

**The first tracer species O3_BC**
  * is defined to have the same molecular weight as ozone
  * uses the ozone dry deposition velocity (VD_O3) scaled with a factor of 1 as its surrogate for dry deposition
  * uses ozone as its scavenging surrogate scaled with a factor of 1
  * will try to obtain its boundary conditions from a CMAQ species named 'O3' in the boundary condition files with a scaling factor of 1 
  * is not mapped to any initial condition, therefore uses default scaling factor of -1
  * does not participate in gas-to-aerosol or gas-to-aqueous transformations
  * will undergo advection and diffusion
  * will be written to the DDEP, WDEP, and CONC output files
  
**The second tracer species CO_BC**
  * is defined to have the same molecular weight as CO
  * uses the CO dry deposition velocity (VD_CO) scaled with a factor of 1 as its surrogate for dry deposition
  * uses CO as its scavenging surrogate scaled with a factor of 1
  * will try to obtain its boundary conditions from a species named 'CO' in the boundary condition files with a scaling factor of 1
  * is not mapped to any initial condition, therefore uses default scaling factor of -1
  * does not participate in gas-to-aerosol or gas-to-aqueous transformations
  * will undergo advection and diffusion
  * will be written to the DDEP, WDEP, and CONC output files 
  
  **The third tracer species O3_IC**
  * is defined to have the same molecular weight as ozone
  * uses the ozone dry deposition velocity (VD_O3) scaled with a factor of 1 as its surrogate for dry deposition
  * uses ozone as its scavenging surrogate scaled with a factor of 1
  * will try to obtain its initial conditions from a species named 'O3' in the initial condition files with scaling factor of -1
  * has no boundary condition specified, therefore uses default scaling factor of -1
  * does not participate in gas-to-aerosol or gas-to-aqueous transformations
  * will undergo advection and diffusion
  * will be written to the DDEP, WDEP, and CONC output files  
  
**The fourth tracer species O3_BC_50PC**
  * is defined to have the same molecular weight as ozone
  * uses the ozone dry deposition velocity (VD_O3) scaled with a factor of 1 as its surrogate for dry deposition
  * uses ozone as its scavenging surrogate scaled with a factor of 1
  * will try to obtain its  boundary conditions from a species named 'O3' in the boundary condition files with a scaling factor of 0.5
  * is not mapped to any boundary condition, therefore uses default scaling factor of -1
  * does not participate in gas-to-aerosol or gas-to-aqueous transformations
  * will undergo advection and diffusion
  * will be written to the DDEP, WDEP, and CONC output files
  
**The fifth tracer species CO_EMIS**
  * is defined to have the same molecular weight as CO
  * uses the CO dry deposition velocity (VD_CO) scaled with a factor of 1 as its surrogate for dry deposition
  * uses CO as its scavenging surrogate scaled with a factor of 1
  * is not mapped to any initial/boundary condition surrogate, therefore uses default scaling factor of -1
  * does not participate in gas-to-aerosol or gas-to-aqueous transformations
  * will undergo advection and diffusion
  * will be written to the DDEP, WDEP, and CONC output files  
  * see the instructions below for how to map emissions to this tracer species using DESID.
  
**The last tracer species ICT_50PPB**
  * is defined to have a molecular weight of 1 g/mole
  * is not mapped to any dry deposition velocity surrogate, i.e. does not undergo dry deposition, therefore uses default scaling factor of -1
  * is not mapped to any scavenging surrogate, i.e. does not undergo scavenging , therfore uses default scaling factor of -1
  * will try to obtain its initial from a species named ICT_50PPB in the initial condition files
  * is not mapped to any boundary condition therefore uses default scaling factor of -1
  * does not participate in gas-to-aerosol or gas-to-aqueous transformations
  * will undergo advection and diffusion
  * will not be written to the DDEP and WDEP output files
  * will be written to the CONC output file 

------------

### STEP 1: Create tracer namelist</strong>

Create namelist according to Table 4-2 in the [CMAQ User's Guide](../CMAQ_UG_ch04_model_inputs.md#Table4-2). Include one line for each tracer species with the following format (refer to the table below for more information on the abbreviations):

<a id=Table3-4></a>

**Amended from Table 4-2 in the CMAQ User's Guide** 

| **Line**| **Column** |**Name** | **Type**| **Description** |**Comments/Options for Syntax**:|
|-----|-----|----------------------|----------|--------------------------------------------|----------------------------|
| 1 || File Type |String|String to delineate Gas Phase (GC), Aerosol (AE), Non-reactive (NR) and Tracer (TR) species namelist. This section is only applicable for "TR" format files.|{TR_nml}|
| 3 || Header ID | String |String to define data structure relating to namelist|{TR_SPECIES_DATA = }|
| 5 |1| SPECIES | String |CMAQ Species name, i.e. NO, HNO<sub>3</sub>, PAR; dependent on chemical mechanism|-|
||2| MOLWT| Integer |Species Molecular Weight|-|
|  |3| IC | String |Initial conditions surrogate species name|{'Species name', ' '}|
|  |4| FAC | Integer |Scaling factor for the inital conditions concentration|{Any integer: default = -1 if IC is not specified}|
|  |5| BC | String |Boundary conditions surrogate species name|{'Species name', ' '}|
|  |6| FAC | Integer |Scaling factor for the boundary concentration|{Any integer: default = -1 if BC is not specified}|
| |7| DRYDEP SURR | String |Surrogate specie name for dry deposition|-|
| |8| FAC | Integer |Scaling factor for dry deposition velocity|{Any integer: default = -1 if SURR is not specified}|
| |9| WET-SCAV SURR | String |Surrograte specie name for wet deposition|-|
| | 10 | FAC | Integer |Scaling factor for wet scavenging|{Any integer: default = -1 if SURR is not specified}|
|| 11 | TR2AE SURR | String |Surrogate species name for gas-to-aerosol transformation species|Not currently functional in CMAQ|
|| 12 | TR2AQ SURR | String |Surrogate species name for aqueous phase reactions|Allows the tracer to participate in aqueous phase chemical reactions|
|| 13 | TRNS | String |Transport Switch. *NOTE: Instead of using one column labeled "TRNS" to turn/off both advection and diffusion for a pollutant, two separate columns labeled "ADV" and "DIFF" can be used to switch on/off advection and diffusion separately.|{YES/NO}|
|| 14 | DDEP | String |Dry deposition output file switch|{YES/NO}|
|| 15 | WDEP | Real |Wet deposition output file switch|{YES/NO}|
|| 16 | CONC | String |Concentration output file switch|{YES/NO}|

The example namelist file shown below defines six tracer species.


```
&TR_nml

TR_SPECIES_DATA =

!SPECIES     ,MOLWT   ,IC           ,IC_FAC         ,BC        ,BC_FAC      ,DEPV       ,DEPV_FAC  ,SCAV     ,SCAV_FAC ,TR2AE      ,TR2AQ ,ADVC  ,DIFF  ,DDEP  ,WDEP  ,CONC 
'O3_BC'      ,48.0    ,''          ,-1              ,'O3'      , 1          ,'VD_O3'    ,1         ,'O3'     , 1        ,''        ,''   ,'YES' ,'YES' ,'YES' ,'YES' ,'YES',
'CO_BC'      ,28.0    ,''          ,-1              ,'CO'      , 1          ,'VD_CO'    ,1         ,'CO'     , 1        ,''        ,''   ,'YES' ,'YES' ,'YES' ,'YES' ,'YES', 
'O3_IC'      ,48.0    ,'O3'        , 1              ,''        ,-1          ,'VD_O3'    ,1         ,'O3'     , 1        ,''        ,''   ,'YES' ,'YES' ,'YES' ,'YES' ,'YES', 
'O3_BC_50PC' ,48.0    ,''          ,-1              ,'O3'      ,0.5         ,'VD_O3'    ,1         ,'O3'     , 1        ,''        ,''   ,'YES' ,'YES' ,'YES' ,'YES' ,'YES',
'CO_EMIS'    ,28.0    ,''          ,-1              ,''        ,-1          ,'VD_CO'    ,1         ,'CO'     , 1        ,''        ,''   ,'YES' ,'YES' ,'YES' ,'YES' ,'YES', 
'ICT_50PPB'  , 1.0    ,'ICT_50PPB' , 1              ,''        ,-1          ,''         ,-1        ,         ,-1        ,''        ,''   ,'YES' ,'YES' ,''    ,''    ,'YES'
/

``` 

### STEP 2: Add tracers to DESID configuration files

If the tracer has emissions then it must be included as part of the DESID configuration for CMAQ. If the tracers are only being added to the initial or boundary conditions file then this step is not necessary. 

The CO_EMIS tracer is designed to track the fate of CO emissions without any influence from initial or boundary conditions and therefore no IC/BC surrogate was specified and no additional species needs to be added to the initial condition file for this tracer. However, it must be specified in the mechanism-specific CMAQ_Control_DESID file found under CCTM/src/MECHS/*{mechanism name}* in the CMAQ repository. For example if running the cb6r3_ae7_aq mechanism, edit the CMAQ_Control_DESID_cb6r3_ae7_aq.nml  file to include the following line after the Custom Mapping Examples in the Emissions Scaling Rules section: 

```
   ! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op
   !  Label      |               |Species  | Species      |Mode  |Factor|      |
   !Tracer
   'EVERYWHERE'  , 'ALL'         ,'CO'     ,'CO_EMIS'      ,'GAS',1.    ,'UNIT','a',
```

### STEP 3: Add tracers to initial condition, boundary condition, and/or emission files

Depending on the desired application, the emission surrogate and IC/BC surrogate defined in the tracer namelist for each tracer need to be added to the corresponding CMAQ input files, i.e. the emissions, initial condition, and/or boundary condition files.

The CO_EMIS tracer defined in STEP1 has an emission surrogate that needs to be mapped in a standard emission file (the first four tracers do not have any emission surrogate while the fifth tracer uses CO as its emission surrogate). The first four tracers have a IC/BC surrogates that are contained in standard initial or boundary condition files, while the last tracer, ICT_50PPB, does not have an IC that is contained in a standard IC file. This section provides the necessary steps to add an emissions surrogate to the emissions file and a sample scripts that add the necessary species to an existing initial condition file.

Note that adding species (if any) to the initial condition file is only necessary for the first day of the simulation while boundary condition and/or emission species (if any) need to be added to the boundary condition and/or emission files for all days of the simulation. Since all tracers defined above will be written to the CGRID file using their names as defined in column 1 of the namelist file, and since the CGRID file will be used to provide initial conditions to CMAQ after the first day of simulation, the tracer species names defined in column 1 were also used as the names of the IC/BC surrogate in column 9 in the sample tracer namelist file for those tracers that use an IC/BC surrogate.   


#### Script to add O3_IC and ICT_50PPB to an existing initial condition file ####

The run script below uses the [`combine`][link_1] program to add species ICT_50PB to an existing initial condition file. The ICT_50PPB specie is set to a constant mixing ratio of 0.05 ppm for all grid cells. From the CMAQ Home directory run the following commands to build the combine executable: 

```
cd $CMAQ_HOME/POST/combine/scripts
./bldit_combine.csh [compiler] [version] |& tee build_combine.log
```

After the combine executable is successfully built, create the following run script in the same folder: 

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

Once the script is made, execute the run script with the following commands:

```
./run.{script_name}.csh |& tee run.combine.log
```

Further details on how to change and customize the emissions control file to the users specification outside the scope of this tutorial can be found in the [emissions tutorial](CMAQ_UG_tutorial_emissions.md). 


### STEP 4: Modify CMAQ run script</strong>

In the CMAQ run script, replace the default tracer namelist file `Species_Table_TR_0.nml` with the custom tracer namelist file created in STEP 1 and new DESID namelist file created in STEP 2 (if needed). Also replace the original input files (initial conditions, boundary conditions, and/or emissions) with the modified input files created in Step 3.

<!-- START_OF_COMMENT -->

[link_1]: ../../../POST/combine/

<!-- END_OF_COMMENT -->

[link_1]: https://github.com/USEPA/CMAQ/blob/main/POST/combine/ 