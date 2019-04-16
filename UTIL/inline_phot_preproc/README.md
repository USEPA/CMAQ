# Inline photolysis preprocessor (inline_phot_preproc)

##  Quick Start

The utility creates two input files used by the in-line method for calculating photolysis rates. The CSQY_DATA\_*mechanism* file contains the 
cross-section and quantum yields for the photolysis rates used by the specified photochemical *mechanism*. The *mechanism* is determined the RXNS_DATA_MODULE.F90 from building and running the
 **chemmech** utility. The PHOT_OPTICS.dat file gives the optical properties for cloud water and ice plus the refractive indice for aerosol species. The file does not change between photochemical *mechanisms*.
When using the files for CCTM executions, the number of wavebands defined in the CSQY_DATA\_*mechanism* and PHOT_OPTICS.dat files need to be the same. The buildrun script sets this number.


###  Using the Utility.

The utility is built and executed for each application because the RXNS_DATA_MODULE.F90 file can change between applications. It is a FORTRAN program and the bldrun script specifies what compiler to use.

To use the utility follow the below instructions.

1) Copy and edit bldrun.inline_phot_preproc.csh (see Table 1.) for your compiler and Mechanism. Save and run to build the software.

2)  IF NEEDED, modify src/inline_phot_preproc.makefile based on the compilers and their flags on your computer platform.

3) If the application uses photolysis rates whose cross-section and quantum yields are not listed under photolysis_CSQY_data, create the data files and add them to the directory.

4) Execute the script. Check the bldrun.log file if the executable does not produce CSQY_DATA table in the output directory.  
<center> Table 1. inline_phot_preproc bldrun script run time or environment settings </center>

 |  Names | Definition | Notes or Recommeded Value |      
 |:-----|:-----|:------|     
 |  COMPILER        | FORTRAN compiler to building create_ebi | the utility's makefile, _src/inline_phot_preproc.makefile_, is step up for the Intel (INTEL), Portland Group (PGF90) and GCC gfortran (GFORT) compilers. If a separate compiler is to be used, the user has to modify the makefile to define the compiler and its compile flags, recommend including debugging flags| 
 |   GC_INC  | Full path to mechanism's RXNS_DATA_MODULE.F90 or mechanism include files | Produced by CHEMMECH utility | 
 |   USE_RXNS_MODULES  | whether FORTRAN 90 describe the photochemical mechanism | T keep if CMAQ v5.1 or higher but comment out if CMAQ v5.02 and lower |   
 |   WVL_AE_REFRAC  | Whether to include spectral values of refractive indices for aerosol species | set T if CMAQ v5.1 or higher and  if CMAQ lower than version 5.1 |   
 |   SPLIT_OUTPUT   | whether optical and CSQY data written to two separate files | set T if CMAQ v5.1 or higher and  if CMAQ lower than version 5.1 |   
 |   N_WAVEBANDS_OUT   | Number of Wavebands to write to output files starting from the band with the longest | range 1 to 18; use 7 for CMAQ and 11 from MPAS-CMAQ |      
 |   APPL   | Name of the photochemical mechanism for application | values equals MECHNAME in RXNS_DATA_MODULE.F90  |  
 |   WVBIN_FILE | Full path to file defining the wavelength bins | ${CMAQ_REPO}/UTIL/flux_data/wavel-bins.dat |
 |   FLUX_FILE  | Full path to solar flux file used for averaging over the wavelength bins  |  ${CMAQ_REPO}/UTIL/flux_data/solar-p05nm-UCI.dat | 
 | CSQY_DATA_RAW | Path to directory with ASCII files containing cross-section and quantum yield versus wavelength for photolysis rates in the photochemical mechanism | individual file names correspond to values in the PHOTAB array in the RXNS_DATA_MODULE.F90 file |  
 | MAX_NUMB_REFRACT | maximum number of aerosol refractive indices to be use | 6, the value can be greater than the actual number used  |
 | AE_REFRAC_LIST | Lists names of environment variable defining file paths to each aerosol refractive index | **"WATER SOLUTE DUST SEASALT SOOT"**<sup>1</sup>; in general "index<sub>1</sub> index<sub>2</sub> ... index<sub>n</sub>" where index<sub>i</sub> defines from 1 to a maxmim of MAX_NUMB_REFRACT file paths | 
 | WATER | Full path to file containing the refractive index for water | ${CMAQ_REPO}/water_clouds/water_refractive_index.dat, index<sub>1</sub> |
 | DUST | Full path to file containing the refractive index for insoluble mineral and unidentified aerosol material | ${CMAQ_REPO}/water_clouds/inso00, index<sub>2</sub> |
 | SOLUTE | Full path to file containing the refractive index for soluble inorganic aerosol material | ${CMAQ_REPO}/water_clouds/waso00, index<sub>3</sub> |
 | SOOT | Full path to file containing the refractive index for elemental carbon aerosol material | ${CMAQ_REPO}/water_clouds/soot00-two_way-Oct_21_2012, index<sub>4</sub> |
 | SEASALT | Full path to file containing the refractive index for sea spray material | ${CMAQ_REPO}/water_clouds/ssam00, index<sub>5</sub> |
 |   OUT_DIR    | Full path to output directory | Value is the user's preference | 
 
 1. CMAQ version 5.3 and lower are hardwired to use only these refractive indices based on information set in AERO_DATA.F. The model will read additional refractive indices but not use them.


<center> Table 2. INLINE_PHOT_PREPROC output files </center>

|File Name|Format|Description|
|----------------|------------|------------------------------------------------------------|
|CSQY_DATA|ASCII|Tabulated CSQY data as a function of temperature and wavelength bin|
|PHOT_OPTICS|ASCII|Wavelength, Optical and Surface Albedo Parameters for CMAQ In-Line Photolysis calculation.|


To report potential program errors or failures, contact Bill Hutzell/USEPA at hutzell.bill@epa.gov

### Files, configuration, and environment variables

To implement new CSQY data in CMAQ, start with individual CSQY data files for each photolysis reaction in an applicable photochemical mechanism. Add to or modify these data to create the CCTM inline CSQY data table.


#### INLINE_PHOT_PREPROC output files



The location of the INLINE_PHOT_PREPROC output files is set in the run script by the variable OUTDIR. To compile a version of the CMAQ programs that use the files created by INLINE_PHO_PREPROC, copy the output files to a new directory under the `$CMAQ_HOME/CCTM/src/MECHS/$Mechanism` directory. Point the CMAQ build scripts to this new directory with the “Mechanism” variable.
