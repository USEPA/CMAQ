combine
========

This Fortran program combines fields from a set of IOAPI or wrfout input files to an output file.  The file assigned to environmental variable SPECIES_DEF defines the new species variables and how they are constructed.  This means that all the species listed in the SPECIES_DEF files need to be output when CMAQ is being run. One option is  to set the ACONC (or CONC) output to be all species.  

## Environment variables used:

```
 GENSPEC      Indicates to generate a new SPECIES_DEF file (does not generate OUTFILE)
              Choices are: Y, N. (e.g. setenv GENSPEC N)
 SPECIES_DEF  Species definition file defining the new variables of the output file
 INFILE1      input file number 1
              The maximum number of IOAPI files is set to be one less than the global IOAPI parameter MXFILE3.
	      Since this parameter is currently set to 64 (https://www.cmascenter.org/ioapi/documentation/all_versions/html/TUTORIAL.html),
	      the maximum number of IOAPI input files is 63.
 OUTFILE      IOAPI output file name, opened as read/write if it does not exist and 
              read/write/update if it already exists
```

## Environment Variables (not required):
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

| Name         |Result                     |    
|:-------------|:-------------------------:|    
| LOG(X[n])    |Natural Logarithm of X[n]   |    
| LOG10(X[n])  |Logarithm Base 10 of X[n]  |    
| EXP(X[n])    |Exponential of X[n]        |    
| SIN(X[n])    |Sine of X[n]               |    
| COS(X[n])    |Cosine of X[n]             |     
| TAN(X[n])    |Tangent of X[n]            |     
| ASIN(X[n])   |Arcsine of X[n]            |     
| ACOS(X[n])   |Arccosine of X[n]          |     
| ATAN(X[n])   |Acrtangent of X[n]         |    
| ABS(X[n])    |Absolute Value of X[n]     |    
| SINH(X[n])   |Hyperbolic Sine of X[n]    |    
| COSH(X[n])   |Hyperbolic Cosine of X[n]  |    
| TANH(X[n])   |Hyperbolic Tangent of X[n] |   
| INT(X[n])    |Integer Truncation of X[n] |  
| SQRT(X[n])   |Square Root of X[n]        | 
 
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

Examples of possible post-processing expressions are shown in the sample SPECIES_DEF files under the scripts/spec_def_files folder in this directory. Species definitions are used to specify how the concentrations of raw output species from CMAQ should be aggregated or transformed into variables of interest. For example, the concentrations of NO and NO<sub>2</sub> from CMAQ can be added together to yield the concentration of NO<sub>x</sub>. Because each chemical mechanism being used in CMAQ differs in the number and kind of species it treats, each example file is specific to a certain chemical mechanism. The sample spec_def files provided have been labeled according to the mechanism each corresponds to. In addition to each spec_def file, a spec_dep file exists for each chemical mechanism that demonstrates how deposition rates can be aggregated to useful quantities (these are indicated with the "Dep" label in the file name).

**If you have questions about the sample species definition files or find something that does not seem correct please discuss it with us by creating an "Issue" through GitHub or contacting the CMAS Help Desk: http://www.cmascenter.org.**

## To run:
Edit the sample run script (run.combine.aconc), then run:
```
 run.combine.aconc |& tee combine.aconc.log
```
A sample run script for creating a combine file for evaluating deposition is also provided (run.combine.dep).  
Check the log file to ensure complete and correct execution without errors.

## Note on the use of wrfout files as input to combine:
In previous releases of combine, meteorological variables used as part of the SPECIES_DEF file needed to be in IOAPI files, typically these files would have been generated by MCIP. The ability to use wrfout files as input to combine was added in this release to support post-processing of outputs from the two-way model when MCIP files may not be available.  While combine allows a combination of IOAPI and (netcdf) wrfout files as input files, the first input file (i.e. INFILE1) must be an IOAPI file and its grid description information will be used to define the grid for OUTFILE. Only wrfout variables defined with dimensions "west_east", "south_north", and optionally "bottom_top" can be utilized by combine and referenced in the SPECIES_DEF file. It is assumed that the projection used in the WRF simulation that generated the wrfout files is the same as the projection defined in the IOAPI files, specifically INFILE1. If necessary, combine will window the variables from the wrfout file to the domain specified in INFILE1, this often is the case when the CMAQ domain was a subset of the WRF domain. 

## Note on time steps:
Unless "start" and "end" are defined in the SPECIES_DEF file, combine will determine the longest time period that is common to all input files and will produce outputs for that time period.

