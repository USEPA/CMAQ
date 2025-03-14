CMAQ Utilities 
========

## Overview
The CMAQ release includes several optional utilities for model developers. Chemical reaction data is processed by the Chemical Mechanism Compiler (*chemmech*) for all chemical reaction solver approaches. This tool needs chemical namelists (e.g. GC_NAMELIST, AE_NAMELIST, etc) in order to run, and these namelists can be modified directly with a text editor or converted to CSV with the namelist converter *nml*. After running chemmech, to then generate files specifically for the Euler Backward Iterative (EBI) solver approach, the *create_ebi* is provided. Finally the Inline Photolysis Preprocessor (*inline_phot_preproc*) provides support for generating photylisis rate input to custom chemical mechanisms.  In addition, the CMAQ repository includes software for generating Makefiles necessary for compiling the CCTM and other components. This bldmake utility is designed to account for user options, diagnose dependencies in source code and produce a Makefile ready to build executable files.  Documentaiton for each utilitiy programs is provided in the README files within each folder.  

## Utility Programs

* **[bldmake](bldmake/README.md)**: CMAQ Makefile generator and model builder
* **[chemmech](chemmech/README.md)**: generates chemical mechanism input files for the CMAQ programs from a mechanism definition file
* **[create_ebi](create_ebi/README.md)**: creates mechanisms-dependent EBI chemistry solver source code 
* **[inline_phot_preproc](inline_phot_preproc/README.md)**: creates photolysis reaction parameter tables for the CCTM inline photlysis module
* **[nml](nml/README.md)**: converts chemical mechanism csv output files from chemmech to the namelist files required by the CMAQ programs
* **[jproc](jproc/README.md)**: calculates daily look-up tables containing clear-sky photolysis rates that are used by the tabular method for calculate photolysis rates in CMAQ CCTM.
