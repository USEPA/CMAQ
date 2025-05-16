Combine
========

This Fortran program combines fields from a set of IOAPI or wrfout input files to an output file.  The file assigned to environmental variable SPECIES_DEF defines the new species variables and how they are constructed.  This means that all the species listed in the SPECIES_DEF files need to be output when CMAQ is being run. One option is  to set the ACONC (or CONC) output to be all species.  

## Run Time Environment variables used:

```
 GENSPEC      Indicates to generate a new SPECIES_DEF file (does not generate OUTFILE)
              Choices are: Y, N. (e.g. setenv GENSPEC N)
 SPECIES_DEF  Set location of species definition files.
 INFILE1      input file number 1
              The maximum number of IOAPI files is set to be one less than the global IOAPI parameter MXFILE3.
	      Since this parameter is currently set to 64 (https://www.cmascenter.org/ioapi/documentation/all_versions/html/TUTORIAL.html),
	      the maximum number of IOAPI input files is 63.
 OUTFILE      IOAPI output file name, opened as read/write if it does not exist and 
              read/write/update if it already exists
```

## Run Time Environment Variables (not required):
```
 IOAPI_ISPH  projection sphere type (use type #20 to match WRF/CMAQ)
             (the default for this program is 20, overriding the ioapi default of 8) 
```
Record type descriptions in SPECIES_DEF file

```
 / records are comment lines
 ! records are comment lines
 # records can be used to define parameters
 #start   YYYYDDD  HHMMSS
 #end     YYYYDDD  HHMMSS
 #layer      KLAY     (default is all layers)
```
All other records are read as variable definition records
```
 format of variable definition records (comma seperated fields)
 field 1: variable name (maximum of 16 characters)
 field 2: units (maximum of 10 characters)
 field 3: formular expression (maximum of 512 characters)
 ```

Formular expressions support operators ^+-\*/ and are evaluated based on mathematical order of operations, i.e., powers first, multiplication/division next and addition/subtraction last. Order of evaluation can be forced by use of parentheses. When part of an expression is enclosed in parentheses, that part is evaluated first.   Table 1. lists supported functions evalutated for each grid cell per each file time step. In addition, formular expressions can use conditional statements of the form "expression_for_condition ? expresssion_if_true :  expression_if_false". Conditional statements have the highest rank in the order of formular operations.

Table 1. Grid Cell Functions per Output Time Step 

| Name         |Result                       |    
|:-------------|:---------------------------:|    
| LOG(X[n])    |Natural Logarithm of X[n]    |    
| LOG10(X[n])  |Logarithm Base 10 of X[n]    |    
| EXP(X[n])    |Exponential of X[n]          |    
| SIN(X[n])    |Sine of X[n]                 |    
| COS(X[n])    |Cosine of X[n]               |     
| TAN(X[n])    |Tangent of X[n]              |     
| ASIN(X[n])   |Arcsine of X[n]              |     
| ACOS(X[n])   |Arccosine of X[n]            |     
| ATAN(X[n])   |Acrtangent of X[n]           |    
| ABS(X[n])    |Absolute Value of X[n]       |    
| SINH(X[n])   |Hyperbolic Sine of X[n]      |    
| COSH(X[n])   |Hyperbolic Cosine of X[n]    |    
| TANH(X[n])   |Hyperbolic Tangent of X[n]   |   
| INT(X[n])    |Integer Truncation of X[n]   |  
| SQRT(X[n])   |Square Root of X[n]          | 
| SIGN(X[n])   |Sign of X[n]                 | 
| ERF(X[n])    |Gauss Error Function of X[n] | 
 
 <sup> 0 </sup> Value of n equals an input file number or zero if X is a derived variable.

Formular expresssions can also use functions (Table 2.) computed for a variable *common* to all input files so each input file *must* contain a variable named X at the timestep.

Table 2. Functions over all Input Files per Output Time Step    

| Name       |Result                             |    
|:-----------|:---------------------------------:|    
| FMAX[X]    |Maximum of X over files            |    
| FMIN[X]    |Minumum of X over files            |     
| FAVG[X]    |Average of X over files            |     
| FSDEV[X]   |Standard Deviation of X over files |    

Variables from input files are defined by their name followed by its file number enclosed in brackets. Once defined in a species definition file, variables can subsequently be referred to by their name and the number zero enclosed in brackets. Adding a + or - sign before the file number within the bracket instructs combine to use the variable value for the next or previous timestep instead of the current time step when evaluating the expression. This can be used to define variables that are computed as difference between the current and previous time step, for example to compute hourly precipitation as the difference in WRF cumulative precipitation values between successive timesteps.

Examples of possible post-processing expressions are shown in the sample SPECIES_DEF files under the scripts/spec_def_files folder in this directory. Species definitions are used to specify how the concentrations of raw output species from CMAQ should be aggregated or transformed into variables of interest. For example, the concentrations of NO and NO<sub>2</sub> from CMAQ can be added together to yield the concentration of NO<sub>x</sub>. Because each chemical mechanism being used in CMAQ differs in the number and kind of species it treats, each example file is specific to a certain chemical mechanism. The sample SpecDef files provided have been labeled according to the mechanism each corresponds to. In version 5.5, we have added SpecDef_Conc files which explicitly map output from the CCTM_CONC and CCTM_ACONC variables to post-processed variables consistent with output from the ELMO module. See [User Guide section 8.2](../../DOCS/Users_Guide/CMAQ_UG_ch08_analysis_tools.md).

In addition to each SpecDef file, a SpecDef_Dep file exists for each chemical mechanism that demonstrates how deposition rates can be aggregated to useful quantities (these are indicated with the "Dep" label in the file name). Users wanting to apply `combine` to other post-processing tasks (e.g. aggregating raw DDM3D or ISAM output species, aggregating individual aerosol species from DESID diagnostic emission files into total PM emissions) can use these sample files as a starting point for creating their own SPECIES_DEF files supporting such applications.

**If you have questions about the sample species definition files or find something that does not seem correct please discuss it with us by creating an "Issue" through GitHub or contacting the CMAS Help Desk: http://www.cmascenter.org.**

## Compile combine source code:

Execute the build script to compile combine:

```
cd $CMAQ_HOME/POST/combine/scripts
./bldit_combine.csh [compiler] [version] |& tee build_combine.log
```

## Run combine:
Edit the sample run script (run.combine.aconc), then run:
```
 ./run.combine |& tee combine.aconc.log
```
A sample run script for creating combine files for evaluating deposition and hourly average concentrations is also provided (run_combine.csh). In addition to aggregating CCTM output variables, this script also illustrates the use of looping to combine day-specific CCTM files with hourly values into a single output file with hourly values that spans the entire analysis period. Note that this sample run script contains two time loops over days, one for creating the combine file for hourly average concentrations (using inputs from CCTM_ACONC, CCTM_AELMO, METCRO2D, and METCRO3D) and a second one for creating the combine file for deposition (using inputs from CCTM_DRYDEP, CCTM_WETDEP1, and METCRO2D). Within each time loop, specific definitions of SPECIES_DEF, INFILEx and OUTFILE are being set up before calling the `combine` executable for each day. If users wish to adapt this run script for their own application (e.g. for aggregating diagnostic emission files), only a single time loop may be required.

Check the log file to ensure complete and correct execution without errors.

## Note on the use of wrfout files as input to combine:
Meteorological variables used as part of the SPECIES_DEF file typically are obtained from wrfout files that have been processed through MCIP and follow IOAPI-netCDF formatting guidelines. However, `combine` also allows reading variables directly from wrfout files to support post-processing of outputs from the two-way model when MCIP files may not be available.  To make use of this capability, several requirements need to be met:
  * When using a combination of IOAPI and (netcdf) wrfout files as input files, the first input file (i.e. INFILE1) **must** be an IOAPI file and its grid description information will be used to define the grid for OUTFILE.
  * Only wrfout variables defined with dimensions "west_east", "south_north", and optionally "bottom_top" can be utilized by `combine` and referenced in the SPECIES_DEF file.
  * The projection used in the WRF simulation that generated the wrfout files must be the same as the projection defined in the IOAPI files, specifically INFILE1.  

If necessary, combine will window the variables from the wrfout file to the domain specified in INFILE1, this often is the case when the CMAQ domain was a subset of the WRF domain. To support such windowing, the following conditions need to be met [(also see the MCIP release notes for CMAQv5.5)](https://github.com/USEPA/CMAQ/wiki/CMAQ-Release-Notes:-Preprocessors#mcip):
  * The domains defined in the wrfout and INFILE1 files need to be concentric.
  * The horizontal grid spacing defined in the wrfout and INFILE1 files needs to be greater than 400 meters because the rigor of the windowing algorithm has not been tested below that scale. 

## Note on time steps:
Unless "start" and "end" are defined in the SPECIES_DEF file, combine will determine the longest time period that is common to all input files and will produce outputs for that time period.

