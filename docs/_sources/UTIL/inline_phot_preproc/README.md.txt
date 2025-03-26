# Inline photolysis preprocessor (inline_phot_preproc)

##  Background

The Inline photolysis preprocessor utility creates two input files used by the in-line method for calculating photolysis rates. The CSQY_DATA\_*mechanism* file contains the 
cross-section and quantum yields for the photolysis rates used by the specified photochemical *mechanism*. The *mechanism* is determined the RXNS_DATA_MODULE.F90 from building and running the
 **chemmech** utility. The PHOT_OPTICS.dat file gives the optical properties for cloud water and ice plus the refractive indice for aerosol species. The file does not change between photochemical *mechanisms*.
When using the files for CCTM executions, the number of wavebands defined in the CSQY_DATA\_*mechanism* and PHOT_OPTICS.dat files need to be the same. 

The utility's method is based on how FAST-JX version 6.8 processes cross-section and quantum yield data to a condensed waveband structure for calculating photolysis rates. The process has two steps that allocate the data over wavelength bins and average the data over the bins based on the solar spectrum. The below table lists the maximum number of wavelength bins. At the shortest wavelength, bins overlap because the O<sub>2</sub> absorption cross-section and the solar flux are correlated in the Schumman-Runge bands.  The utility includes an option that subsets the bins starting from longest to shortest wavelenghts.

<center> Table 1. Wavelength Bins Intervals </center>

|Bin|      start(nm)   |    effective(nm)  |       stop(nm) |   
|:----:|:-------------------:|:--------------------:|:---------------------:|    
|  1|               177.500|               186.839|               202.500 |  
|  2|               177.500|               191.209|               202.500 |  
|  3|               202.500|               193.620|               206.500 |  
|  4|               206.500|               196.244|               209.500 |  
|  5|               209.500|               202.392|               212.500 |  
|  6|               212.500|               208.183|               215.500 |  
|  7|               215.500|               211.125|               221.500 |  
|  8|               221.500|               213.817|               233.000 |  
|  9|               233.000|               261.412|               275.500 |  
| 10|               275.500|               270.511|               286.500 |  
| 11|               286.500|               281.149|               291.000 |  
| 12|               291.000|               294.590|               298.300 |  
| 13|               298.300|               303.151|               307.500 |  
| 14|               307.500|               310.007|               312.500 |  
| 15|               312.500|               316.434|               320.300 |  
| 16|               320.300|               333.076|               345.000 |  
| 17|               345.000|               381.997|               412.500 |  
| 18|               412.500|               607.723|               850.000 |  
 
###  Using the Utility.

The utility is built and executed for each application because the RXNS_DATA_MODULE.F90 file can change between applications. It is a FORTRAN program and the bldrun script specifies what compiler to use.

To use the utility follow the below instructions.

1) Copy and edit bldrun.inline_phot_preproc.csh (see Table 2.) for your compiler and Mechanism. Save and run to build the software.

2)  IF NEEDED, modify src/inline_phot_preproc.makefile based on the compilers and their flags on your computer platform.

3) If the application uses photolysis rates whose cross-section and quantum yields are not listed under the subdirectory, _photolysis_CSQY_data_, create the data files for each rate and add them to the directory.

4) Execute the script. Check the bldrun.log file if the executable does not produce CSQY_DATA table in the output directory.  
<center> Table 2. inline_phot_preproc bldrun script run time or environment settings </center>

 |  Names | Definition | Notes or Recommeded Value |      
 |:-----|:-----|:------|     
 |  COMPILER        | FORTRAN compiler to building create_ebi | the utility's makefile, _src/inline_phot_preproc.makefile_, is step up for the Intel (INTEL), Portland Group (PGF90) and GCC gfortran (GFORT) compilers. If a separate compiler is to be used, the user has to modify the makefile to define the compiler and its compile flags, recommend including debugging flags| 
 |   RXNS_DATA_SRC  | Full path to mechanism's RXNS_DATA_MODULE.F90 or mechanism include files | Produced by CHEMMECH utility | 
 |   USE_RXNS_MODULES  | whether FORTRAN 90 describe the photochemical mechanism | T if CMAQ v5.1 or higher but comment out if CMAQ v5.02 and lower |   
 |   WVL_AE_REFRAC  | Whether to include spectral values of refractive indices for aerosol species | T if CMAQ v5.1 or higher and  if CMAQ lower than version 5.1 |   
 |   SPLIT_OUTPUT   | whether optical and CSQY data written to two separate files | T if CMAQ v5.1 or higher and  if CMAQ lower than version 5.1 |   
 |   N_WAVEBANDS_OUT   | Number of Wavebands to write to output files starting from the band with the longest | range 1 to 18; use 7 for CMAQ and 11 from MPAS-CMAQ |      
 |   APPL   | Name of the photochemical mechanism for application | values equals MECHNAME in RXNS_DATA_MODULE.F90  |  
 |   WVBIN_FILE | Full path to file defining the wavelength bins | ${CMAQ_REPO}/UTIL/inline_phot_preproc/flux_data/wavel-bins.dat |
 |   FLUX_FILE  | Full path to solar flux file used for averaging over the wavelength bins  |  ${CMAQ_REPO}/UTIL/inline_phot_preproc/flux_data/solar-p05nm-UCI.dat | 
 | CSQY_DATA_RAW | Path to directory with ASCII files containing cross-section and quantum yield versus wavelength for photolysis rates in the photochemical mechanism | ${CMAQ_REPO}/inline_phot_preproc/photolysis_CSQY_data; individual file names correspond to values of the PHOTAB array in the RXNS_DATA_MODULE.F90 file |  
 | MAX_NUMB_REFRACT | maximum number of aerosol refractive indices to be use | 16, the value can be greater than the actual number used  |
 | AE_REFRAC_LIST | Lists names of environment variable defining file paths to each aerosol refractive index | "index<sub>1</sub> index<sub>2</sub> ... index<sub>n</sub>" in general where index<sub>i</sub> defines from 1 to MAX_NUMB_REFRACT file paths. The current build-runscript use **"WATER SOLUTE DUST SEASALT SOOT ISOP_NOX ISOP_SOX LIMONENE_SOA APINENE_SOA NAPTH_SOA MXYL_HIGH_NOX MXYL_LOW_NOX TOLU_HIGH_NOX TOLU_LOW_NOX ORGCARB BIOMASS"** but can be changed. Read the section on aerosol refractive indices. | 
 | index<sub>1</sub> | Full path to file containing the refractive index 1 | ${CMAQ_REPO}/inline_phot_preproc/refractive_indices/water_refractive_index.dat, WATER |
 | index<sub>2</sub> | Full path to file containing the refractive index 2 | ${CMAQ_REPO}/inline_phot_preproc/inline_phot_preproc/refractive_indices/OPAC_water_clouds/inso00, DUST |
 | index<sub>3</sub> | Full path to file containing the refractive index 3 | ${CMAQ_REPO}/inline_phot_preproc/inline_phot_preproc/refractive_indices/OPAC_water_clouds/waso00, SOLUTE |
 | index<sub>n</sub> | Full path to file containing the refractive index n | ${CMAQ_REPO}/inline_phot_preproc/inline_phot_preproc/refractive_indicesadient_aerosol_refrac_indx/refract_biomass_new.txt, BIOMASS |
 |   OUT_DIR    | Full path to output directory | Value is the user's preference | 
 
 1. CMAQ version 5.32 and lower are hardwired to use only these refractive indices based on information set in AERO_DATA.F. Models higher than versions allows change default
 value by change the _OPTICS_ entries in the aerosol species name list.

<center> Table 2. INLINE_PHOT_PREPROC output files </center>

|File Name|Format|Description|
|----------------|------------|------------------------------------------------------------|
|CSQY_DATA_**mechanism**|ASCII|Processed cross-sections and quantum yields as a function of temperature and wavelength bin; **mechanism** equals the value of MECHNAME in RXNS_DATA_MODULE.F90 |
|PHOT_OPTICS.dat |ASCII|Wavelength, Optical and Surface Albedo Parameters for CMAQ In-Line Photolysis calculation.|


To report potential program errors or failures, contact Bill Hutzell/USEPA at hutzell.bill@epa.gov.

### Files, configuration, and environment variables

In general, applications of inline_phot_preproc vary based on the photochemical mechanism's reaction data module, RXNS_DATA_MODULE.F90, and ASCII files containing cross-section and quantum yield data for photolysis rates used by the photochemical mechanism. The chemmech utility produces the RXNS_DATA_MODULE.F90 file. The latter files are created by the user if they do not exist under the ${CMAQ_REPO}/inline_phot_preproc/photolysis_CSQY_data directory. Each of these files have names listed in the PHOTAB array defined by the mechanism's RXNS_DATA_MODULE.F90 file (Figure 1.). The array is constucted based on reactions of the mechanism definitions file (check the chemmech README for more information). These files follow simple formatting rules (Table 3.). Check files under CSQY_DATA_RAW for examples.

Table 3. General Format Rules for cross-section and quantum yield data files.

*   First line read gives the name of the photolysis rate used by the photochemical mechanism. The PHOTAB array in RXNS_DATA_MODULE.F90 lists the names.  
*   Comment lines begin with a "!". Comments should give the reactions reactants and products as well as cite the source of the file's data.
*   Second line read begins with a "B", "E", "C", or "P". The first three symbols state whether the values are the Beginning, End or Center of the wavelength intervals of the data. The last symbol, "P", states that data should interperted as irradance value. The case determines how the data is interpolated across the wavelength bands in Table 1.
     -  Interpolation scheme gives zero results for wavelengths not covered by the data file.       
*   Third line read begins with "FAC=" followed by a real number. Its value gives a factor to convert the cross-section data into cm<sup>-2</sup>.
*    Remaining lines read give the data values for the wavelengths (nm), cross-sections and quantum yields of the photolysis rate in free format. 


<center> Figure 1. The PHOTAB array extracted from RXNS_DATA_MODULE.F90 for the saprc07tc_ae6_aq mechanism </center>

         INTEGER, PARAMETER :: NPHOTAB =  38
         CHARACTER( 16 )    :: PHOTAB( NPHOTAB )
   
         DATA ( PHOTAB( IRXXN ), IRXXN = 1, NPHOTAB ) / & 
        &   'NO2_06          ', 'NO3NO_06        ', 'NO3NO2_6        ', & 
        &   'O3O1D_06        ', 'O3O3P_06        ', 'HONO_06         ', & 
        &   'HNO3            ', 'HNO4_06         ', 'H2O2            ', & 
        &   'PAN             ', 'HCHOR_06        ', 'HCHOM_06        ', & 
        &   'CCHO_R          ', 'C2CHO           ', 'ACET_06         ', & 
        &   'MEK_06          ', 'COOH            ', 'GLY_07R         ', & 
        &   'GLY_07M         ', 'MGLY_06         ', 'BACL_07         ', & 
        &   'BALD_06         ', 'AFG1            ', 'MACR_06         ', & 
        &   'MVK_06          ', 'IC3ONO2         ', 'HOCCHO_IUPAC    ', & 
        &   'ACRO_09         ', 'PAA             ', 'CL2             ', & 
        &   'CLNO_06         ', 'CLONO           ', 'CLNO2           ', & 
        &   'CLONO2_1        ', 'CLONO2_2        ', 'HOCL_06         ', & 
        &   'CLCCHO          ', 'CLACET          '/


To make the CMAQ CCTM use a new CSQY_DATA_**mechanism**, modify the value of CSQY_DATA in the CCTM run-script to equal the new file. CCTM needs to be compiled with RXNS_DATA_MODULE.F90 used to create the new file. Compiling CCTM also needs to use the RXNS_FUNC_MODULE.F90 that the chemmech utility produced along with the RXNS_DATA_MODULE.F90. 

If an applications of inline_phot_preproc changes the N_WAVEBANDS_OUT from the standard value, 7, the CCTM run-script has use the new CSQY_DATA_**mechanism** and PHOT_OPTICS.dat files. 

#### Aerosol Refractive Indices

<center> Table 3. Aerosol Refractive defined in build-run script </center>

The current build-run script uses Table 3's refractive indices. Their raw data files are located
under the _refractive indices_ subdirectory. After processing this data, inline_phot_preproc
writes results to the PHOT_OPTICS.dat file read by the CCTM model. A user may
change refractive indices written to PHOT_OPTICS.dat by changing **AE_REFRAC_LIST** then setting
new environment variables in the new list and/or by changing values of environment variables in
the existing list. The _refractive indices_ subdirectory also contain data files that can be used.
If a user wants to use their own data, the files have to use the same format as the current repository's files. 
Note that wavelengths have to use nanometers as their units.

 |  Names        | Definition or aerosol material | Source |      
 |:--------------|:---------------------------------------------|:---------------------|     
 | WATER         | water in the aqueous aerosol component       | Segelstein, D., 1981 |
 | DUST          | insoluble mineral and unidentified material  | OPAC software package (M. Hess et. al, 1998) |     
 | SOLUTE        | inorganic solutes in aqueous aerosol component such as SO<sub>4</sub>, NH<sub>4</sub>, NO<sub>3</sub> and CL ions | OPAC software package (M. Hess et. al, 1998) |
 | SOOT          | insoluble elemental carbon | Chang,H & T.T. Charalmpopoulos (1990), Bond, T.C. & R.W. Bergstrom (2006), Personal Communication from Tami Bond.  |
 | SEASALT       | material from sea spray    | OPAC software package (M. Hess et. al, 1998)        |
 | ISOP_NOX      | organic material from isoprene oxidation and later reactions with NO<sub>x</sub> | Nakayama et. al (2018) |
 | ISOP_SOX      | organic material from isoprene oxidation and later reactions with SO<sub>x</sub> | Nakayama et. al (2018) |
 | LIMONENE_SOA  | organic material from limonene oxidation similar biogenic gases     | Lui et. al (2013)   |
 | APINENE_SOA   | organic material from alpha-pinene oxidation similar biogenic gases | Lui et. al (2013)   |
 | NAPTH_SOA     | organic material from polycyclic aromatic hydrocarbon oxidation     | Lambe et. al (2013) |
 | MXYL_HIGH_NOX | organic material from xylenes oxidation under high NO<sub>x</sub>   | Lui et. al (2015)   |
 | MXYL_LOW_NOX  | organic material from xylenes oxidation under low NO<sub>x</sub>    | Lui et. al (2015)   |
 | TOLU_HIGH_NOX | organic material from toluene oxidation under high NO<sub>x</sub>   | Lui et. al (2015)   |
 | TOLU_LOW_LOW  | organic material from toluene oxidation under low NO<sub>x</sub>    | Lui et. al (2015)   |
 | ORGCARB       | general organic material                                            | Assembled from numerous sources by Adient review (McMeeking et. al, 2010) |
 | BIOMASS       | organic material from biomass combustion                            | Assembled from numerous sources by Adient review (McMeeking et. al, 2010) |

### References 

ADIENT database: http://www.met.rdg.ac.uk/~adient/refractiveindices.html, last accessed on January 22, 2021.

Bian H. and Prather M. J. (2002). Fast-J2: Accurate Simulation of Stratospheric Photolysis in Global Chemical Models, J. Atmos. Chem., 41, 281-296. (Table I & II corrected, June 2008).

Bond, T.C. (2012), personal communication.

Bond, T.C. & R.W. Bergstrom (2006) Light absorption by
Carbonaceous Particles: An investigative review,
Aerosol Science and Technology. Vol. 40. pp 27-67

Chang,H and T.T. Charalmpopoulos (1990) Determination of the
wavelength dependence of refractive indices of flame soot,
Proceeding of the Royal Society of London A, Vol. 430, pp 577-591.

Hess M., Koepke P., and I. (1998): Optical Properties of Aerosols and clouds: The software package OPAC, Bull. Am. Met. Soc., 79, 831-844.

Andrew T. Lambe, Christopher D. Cappa, Paola Massoli, Timothy B. Onasch, Sara D. Forestieri, Alexander T. Martin, Molly J. Cummings,
David R. Croasdale, William H. Brune, Douglas R. Worsnop, and Paul Davidovits (2013).
Environmental Science & Technology, 47(12), 6349-6357, DOI: 10.1021/es401043j.

Liu P.F., Yue Zhang, and Scot T. Martin (2013).
Complex Refractive Indices of Thin Films of Secondary Organic Materials by Spectroscopic Ellipsometry from 220 to 1200 nm
Environmental Science & Technology 47(23), 13594-13601, DOI: 10.1021/es403411e.

Liu P.F., N. Abdelmalki, H.-M. Hung, Y. Wang, W. H. Brune and S. T. Martin (2015).
Ultraviolet and visible complex refractive indices of secondary organic 
material produced by photooxidation of the aromatic compounds toluene and m-xylene
Atmos. Chem. Phys., 15, 1435-1446, doi:10.5194/acp-15-1435-2015.

McMeeking, G. R., Hamburger, T., Liu, D., Flynn, M., Morgan, W. T., Northway, M., Highwood,
5 E. J., Krejci, R., Allan, J. D., Minikin, A., and Coe, H. (2010). Black carbon measurements in the
boundary layer over western and northern Europe, Atmos. Chem. Phys., 10, 93939414,
doi:10.5194/acp-10-9393-2010.

Nakayama T., Kei Sato, Takashi Imamura, and Yutaka Matsumi (2018).
Effect of Oxidation Process on Complex Refractive Index of Secondary Organic Aerosol Generated from Isoprene.
Environmental Science & Technology 52(5), 2566-2574. DOI: 10.1021/acs.est.7b05852

Segelstein, D., (1981), The Complex Refractive Index of Water, M.S. Thesis, University of Missouri--Kansas City, MO.

/
