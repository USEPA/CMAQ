<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch07_programs_libraries.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch09_grid_defn.md)

<!-- END COMMENT -->



<a id=griddesc></a>


<a id=matrix_nml></a>



<a id=ic_profile></a>
### IC_PROFILE: Initial conditions vertical profiles

Used by: ICON

ICON can generate initial conditions from two different input file types. The first file type is an ASCII vertical profile file that lists species concentrations at various model layers that are fixed in space and time. To configure ICON to generate initial conditions from ASCII vertical profiles, the “prof” input module is chosen when compiling the program (see [Chapter 7](CMAQ_OGD_ch07_programs_libraries.md#ICON)). These ASCII-formatted vertical profile files are IC_PROFILE files, and are described in this section. IC_PROFILE files must be developed by the user and can be generated from climatologically averaged observational data or as a prior estimate from previous modeling studies of the region being modeled. The second file type that ICON can use to generate initial conditions is a concentration file from a previous CMAQ run. These are CTM_CONC_1 files, and are described later in the [CTM_CONC_1 section](#ctm_conc_1).

IC_PROFILE begins with a header that contains a comment section that describes the data, and a file description section that defines the number of vertical levels in the file, the number of pollutants in the file, and the distribution of the vertical levels. The next entries in IC_PROFILE are the Julian start date and the start time of the data; they are not used by ICON.

Each line in IC_PROFILE corresponds to a different pollutant and begins with the name of the pollutant. The subsequent columns on each line list the chemical concentration at each layer contained in the file. Gas-phase species are in ppmV, and aerosol species are in µg m<sup>-3</sup>. The layer structure of the IC_PROFILE vertical profiles does not need to correspond exactly to the layer structure that will be modeled; the ICON program will interpolate the data to the correct vertical format for the simulation.

Initial conditions are provided for only the first hour of a model simulation. The initial conditions that are based on an ASCII vertical profile include a gridded file for input to CCTM that has horizontally uniform species concentrations at each model layer. For spatially resolved initial conditions in the horizontal direction, it is necessary to use the other input file type to ICON, an existing CCTM concentration file (CTM_CONC_1).

A detailed description of the vertical profile file format for initial conditions is provided in [Table 8-5](#Table8-5). The header of the profiles file is list-formatted, while the data section of the file is fixed format.

<a id=Table8-5></a>

 **Table 8-5. IC_PROFILE format description**

|**Line**|**Column**|**Name**|**Type**|**Description**|
|---------|-------------|--------------------------|---------|------------------------------------------------------|
| 1-3 || Text Header | String |Text description of the contents and source of the initial conditions file (optional)|
| 4 | A | NUM_SIGMA_LVL | Int |Number of sigma levels contained in the file (required)|
|| B | NUM_POLL | Int |Number of pollutants contained in the file (required)|
|| C | SIGMA_LVL | Real |Vertical coordinate values of sigma-p levels; number of values (n+1) is one more than the NUM_SIGMA_LVL (n) (required)|
| 4 | n | ...  | ...  |...|
| 5 | A | STDATE | String |Julian start date of the file, YYYYDDD (optional)|
|| B | STTIME | String |Start time of the file, HH (optional)|
| 6 | 1-10 | SPECIES1 | String |Pollutant name, enclosed in double quotes (required)|
|| 12-20 | LAYER1_IC | Exp |IC concentration for species 1 in lowest sigma layer (required)|
|| 23-31 | LAYER2_IC | Exp |IC concentration for species 1 in 2nd sigma layer (required)|
|| 34-42 | LAYER3_IC | Exp |IC concentration for species 1 in 3rd sigma layer (required)|
|| 45-53 | LAYER4_IC | Exp |IC concentration for species 1 in 4th sigma layer (required)|
|| ...  | LAYERX_IC | Exp |IC concentration for species 1 in Xth sigma layer (required)|
 | 7 | 1-10 | SPECIES2 | String |Pollutant name, enclosed in double quotes (required)|
|| 12-20 | LAYER1_IC | Exp |IC concentration for species 2 in lowest sigma layer (required)|
|| 23-31 | LAYER2_IC | Exp |IC concentration for species 2 in 2nd sigma layer (required)|
|| 34-42 | LAYER3_IC | Exp |IC concentration for species 2 in 3rd sigma layer (required)|
|| 45-53 | LAYER4_IC | Exp |IC concentration for species 2 in 4th sigma layer (required)|
|| ...  | LAYERX_IC | Exp |IC concentration for species 2 in Xth sigma layer (required)|
| ...  | ...  | ...  | ...  |...|
| Z | 1-10 | SPECIESZ | String |Pollutant name, enclosed in double quotes (required)|
| ...  | 12-20 | LAYER1_IC | Exp |IC concentration for species Z in lowest sigma layer (required)|
| ...  | 23-31 | LAYER2_IC | Exp |IC concentration for species Z in 2nd sigma layer (required)|
| ...  | 34-42 | LAYER3_IC | Exp |IC concentration for species Z in 3rd sigma layer (required)|
| ...  | 45-53 | LAYER4_IC | Exp |IC concentration for species Z in 4th sigma layer (required)|
| ...  | ...  | LAYERX_IC | Exp |IC concentration for species Z in Xth sigma layer (required)|

A sample of the four sections of an IC_PROFILE file is shown below.

```
Example initial condition: The vertical coordinate of the model to generate
these i.c. is the terrain-following sigma coordinate.
The number of sigma layers and defined sigma levels are listed below.
 6 55 1.00 0.98 0.93 0.84 0.60 0.30 0.00
1988180 00
"SO2 " 0.300E-03 0.200E-03 0.100E-03 0.100E-03 0.200E-04 0.100E-04
```

<a id=bc_profile></a>
### BC_PROFILE: Boundary conditions vertical profiles

Used by: BCON

As with the ICON program, BCON can generate boundary conditions from two different input file types. The first file type is an ASCII vertical profile file that list species concentrations at various model layers that are fixed in space in time. To configure BCON to generate boundary conditions from ASCII vertical profiles, the “prof” input module is chosen when compiling the program (see Section 7.2 on BCON). These ASCII-formatted vertical profile files are BC_PROFILE files, and are described in this section. BC_PROFILE files must be developed by the user and can be generated from climatologically averaged observational data or as a prior estimate from previous modeling studies of the region being modeled. The second file type that BCON can use to generate initial conditions is a concentration file from a previous CMAQ run. These are CTM_CONC_1 files, and are described later in the [CTM_CONC_1 section](#ctm_conc_1).

BC_PROFILE begins with a header that contains a comment section that describes the data, and a file description section that defines the number of vertical levels in the file, the number of pollutants in the file, and the distribution of the vertical levels. The next entries in BC_PROFILE are the Julian start date and the start time of the data; they are not used by BCON. The BCON input consists of four data sections that correspond to the lateral boundaries (i.e., north, south, east, and west) of the model grid. The BCON input profiles contain a field that precedes each data section to indicate which horizontal boundary the data section describes.

The format of the data sections in BC_PROFILE files is the same as in IC_PROFILE files. Each line corresponds to a different pollutant and begins with the name of the pollutant. The subsequent columns on each line list the chemical concentration at each layer contained in the file. Gas-phase species are in ppmV, and aerosol species are in ?g m<sup>-3</sup>. The layer structure of the BC_PROFILE vertical profiles does not need to correspond exactly to the layer structure that will be modeled; the BCON program will interpolate the data to the correct vertical format for the simulation.

Boundary conditions can either be time-independent (static) or time-dependent (dynamic). Boundary conditions generated with BC_PROFILE’s ASCII vertical profiles are both static and spatially uniform along each of the four horizontal boundaries at each model layer. For spatially resolved (in the horizontal direction) and temporally resolved boundary conditions, it is necessary to use the other input file type to BCON, an existing CCTM concentration file (CTM_CONC_1).

A detailed description of the vertical profile file format for boundary conditions is provided in [Table 8-6](#Table8-6). The header of the profiles file is list-formatted, while the data section of the file is fixed format.

<a id=Table8-6></a>

**Table 8-6. BC_PROFILE format description**

|**Line**|**Column**|**Name**|**Type**|**Description**|
|--------|------------|--------------------|----------|---------------------------------------------|
| 1-3 || Text Header | String |Text description of the contents and source of the initial conditions file (optional) |
| 4 | A | NUM_SIGMA_LVL | Int |Number of sigma levels contained in the file (required)|
|| B | NUM_POLL | Int |Number of pollutants contained in the file (required)|
|| C | SIGMA_LVL | Real |Vertical coordinate values of sigma-p levels; number of values (n+1) is one more than the NUM_SIGMA_LVL (n) (required)|
| 4 | n | ...  | ...  |...|
| 5 | A | STDATE | String |Julian start date of the file, YYYYDDD (optional)|
|| B | STTIME | String |Start time of the file, HH (optional)|
| 6 | A | Direction | String |North, South, East, West; indicates the boundary described by the subsequent data section (required)|
| 7 | 1-10 | SPECIES1 | String |Pollutant name, enclosed in double quotes (required)|
|| 12-20 | LAYER1_BC | Exp |BC concentration for species 1 in lowest sigma layer (required)|
|| 23-31 | LAYER2_BC | Exp |BC concentration for species 1 in 2nd sigma layer (required)|
|| 34-42 | LAYER3_BC | Exp |BC concentration for species 1 in 3rd sigma layer (required)|
|| 45-53 | LAYER4_BC | Exp |BC concentration for species 1 in 4th sigma layer (required)|
|| ...  | LAYERX_BC | Exp |BC concentration for species 1 in Xth sigma layer (required)|
| 8 | 1-10 | SPECIES2 | String |Pollutant name, enclosed in double quotes (required)|
|| 12-20 | LAYER1_BC | Exp |BC concentration for species 2 in lowest sigma layer (required)|
|| 23-31 | LAYER2_BC | Exp |BC concentration for species 2 in 2nd sigma layer (required)|
|| 34-42 | LAYER3_BC | Exp |BC concentration for species 2 in 3rd sigma layer (required)|
|| 45-53 | LAYER4_BC | Exp |BC concentration for species 2 in 4th sigma layer (required)|
|| ...  | LAYERX_BC | Exp |BC concentration for species 2 in Xth sigma layer (required)|
| ...  | ...  | ...  | ...  |...|
| Y | A | Direction | String |North, South, East, West; indicates the horizontal boundary described by the subsequent data section (required)|
| Z+1 | 1-10 | SPECIESZ | String |Pollutant name, enclosed in double quotes (required)|
| ...  | 12-20 | LAYER1_BC | Exp |BC concentration for species Z in lowest sigma layer (required)|
| ...  | 23-31 | LAYER2_BC | Exp |BC concentration for species Z in 2nd sigma layer (required)|
| ...  | 34-42 | LAYER3_BC | Exp |BC concentration for species Z in 3rd sigma layer (required)|
| ...  | 45-53 | LAYER4_BC | Exp |BC concentration for species Z in 4th sigma layer (required)|
| ...  | ...  | LAYERX_BC | Exp |BC concentration for species Z in Xth sigma layer (required)|

A sample of the important sections of a BC_PROFILE file is shown below.

```
6 55 1.00 0.98 0.93 0.84 0.60 0.30 0.00
1988180 00
 North
 "SO2 " 0.300E-03 0.200E-03 0.100E-03 0.100E-03 0.200E-04 0.100E-04
 West
 "SO2 " 0.300E-03 0.200E-03 0.100E-03 0.100E-03 0.200E-04 0.100E-04
```

<a name="ctm_conc_1"></a>
### CTM_CONC_1: CCTM concentration files

Used by: ICON, BCON

An I/O API GRDDED3-formatted CCTM output concentration file, CTM_CONC_1, can be used to create spatially and temporally varying initial and boundary conditions. To configure ICON and BCON to generate initial and boundary conditions from a CCTM concentration file, the “m3conc” input module is chosen when compiling the programs (see Section 7.7 on ICON and Section 7.2 on BCON). The input concentration file must cover a temporal and spatial extent that is consistent with the time period and domain that are being modeled, respectively. Both ICON and BCON require a Julian start date to be specified at execution that identifies the first time step to extract from the input concentration file; BCON also requires a run-length specification to indicate the number of time steps of boundary conditions to extract from the input file. For nested runs, the nested domain for which initial and boundary conditions are being extracted must be on the same projection and fall within the domain contained in the input concentration file.

<a name="CSQY"></a>
### CSQY: Absorption cross section and quantum yields

Used by: JPROC

CSQY is the logical name for the ASCII data file containing absorption cross section and quantum yield data for unique photolysis reactions. The data in these files are listed as a function of wavelength and correspond to the standard photolysis reactions in each of the chemical mechanisms implemented in CMAQ. A flexible format allows users to deviate from these standards and test new data with minimal effort.

The ASCII-formatted CSQY files begin with the number and a list of the applicable photolysis reactions. The data section of the CSQY file includes (1) the reaction name/ID; (2) The temperature bands (3) the number of wavelength bins (4) the start, effect and end of the wavelength bin and the photon flux for each inline band. The absorption cross section averaged over UCI Solar Flux(cm), and the quantum yield averaged over UCI Solar Flux as columns, for each wavelength bin, for each temperature, and for each reaction. The CSQY file uses a space-delimited, free-form format for the data section of the file. A detailed description of the CSQY file format is provided in [Table 8-7](#Table8-7).

<a id=Table8-7></a>

**Table 8-7. CSQY format description**

|**Line** | **Column** | **Name** | **Type** | **Description** |
|--------------------------|----|--------------------------|---------|-----------------------------------------|
| 1 | A | CSQY Mechanism Table Name | String |Text name indicating this is the CSQY for the Mechanism Specified.  This name is cross-referenced in the chemical mechanism description files (required)|
| 1 | A | NPHOTAB | String |Number of Photolysis Reactions|
| 2 | A | Comments | String |Preceded by "!", Individual Reaction Rates Listed below|
| 3 | A  | Name | String| Reaction Name  |
| repeat for all (NPHOTAB) reaction names|
| 3+NPHOTAB | A | Name | String |Reaction Name|
| 3+NPHOTAB+1 | A | NTEMP| Int or Real |Number of Temperature Bands|
| 3+NPHOTAB+2 | A | Number| Integer | Index of temperature |
|| B | TEMP | Real | Temperature in Kelvin|
| repeat for all NTEMP values|
| 3+NPHOTAB+NTEMP | A | Comments | String |Preceded by "!", column header: I,      START_WL_BIN(nm),    EFFECT_WL_BIN_(nm),       END_WL_BIN_(nm), photon_flux(cm-2\*s-1), |
| 3+NPHOTAB+NTEMP+1 | A | Number of Inline Photolysis Bands| Integer | |
|| B | Start Wavelength | Real |Start Wavelength for Bin |
|| C | Effect Wavelength | Real |Effect Wavelength for Bin |
|| D | End Wavelength | Real |End Wavelength for Bin |
|| E | Photon Flux| Real |Photon Flux |
| 3+NPHOTAB+NTEMP+2 | A | Comments | String |Preceded by "!", CS  = absorption cross sections averaged over UCI Solar Flux|
| 3+NPHOTAB+NTEMP+3 | A | Comments | String |Preceded by "!", QY  = quantum yields averaged over UCI Solar Flux|
| 3+NPHOTAB+NTEMP+4 | A | Comments | String |Preceded by "!", EQY = eCS\*eQY/CS averaged over Solar Flux and 77 bins in UCI Model|
| 3+NPHOTAB+NTEMP+5 | A | Comments | String |Preceded by "!", header for reactions, CS or EQY, and Wavelength Bins|
| 3+NPHOTAB+NTEMP+6| A |Reaction Name | String |Reaction Name|
|| B | Quantity | String |CS |
|| C | TEMP| Real | Temperature|
|| D | WBIN (1) | Real or EXP | Absorption Cross Section (CS)  Value for BIN 1|
|| E | WBIN (2)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 2|
|| F | WBIN (3)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 3|
|| G | WBIN (4)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 4|
|| H | WBIN (5)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 5|
|| I | WBIN (6)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 6|
|| J | WBIN (7)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 7|
| 3+NPHOTAB+NTEMP+7 | A |Reaction Name | String |Reaction Name|
|| B | Quantity | String |EQY |
|| C | TEMP| Real | Temperature|
|| D | WBIN (1) | Real or EXP | Absorption Cross Section (EQY)  Value for BIN 1|
|| E | WBIN (2)  | Real or EXP  |Absorption Cross Section (EQY) Value for BIN 2|
|| F | WBIN (3)  | Real or EXP  |Absorption Cross Section (EQY) Value for BIN 3|
|| G | WBIN (4)  | Real or EXP  |Absorption Cross Section (EQY) Value for BIN 4|
|| H | WBIN (5)  | Real or EXP  |Absorption Cross Section (EQY) Value for BIN 5|
|| I | WBIN (6)  | Real or EXP  |Absorption Cross Section (EQY) Value for BIN 6|
|| J | WBIN (7)  | Real or EXP  |Absorption Cross Section (EQY) Value for BIN 7|
|<repeat for 6 temperature bands and for each of the 26 reactions)|
| n-11 | A | Comments | String |Preceded by "!", Ozone Cross-Section for Stratospheric Opacity based on Table 4-5 in|
| n-10 | A | Comments | String |Chemical Kinetics and Photochemical Data for Use in Atmospheric|
| n-9 | A | Comments | String |Studies Evaluation Number 15. Sander et. al: NASA-JPL 2006|
| n-8 | A |NTEMP_STRAT | Int |Number of Temperature Bins in Stratosphere|
| n-7 | A | Comments | String |Preceded by "!", Header|
| n-6 | A |Reaction Name | String |Stratospheric Reaction Name|
|| B | Quantity | String |CS |
|| C | TEMP| Real | Temperature|
|| D | WBIN (1) | Real or EXP | Absorption Cross Section (CS)  Value for BIN 1|
|| E | WBIN (2)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 2|
|| F | WBIN (3)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 3|
|| G | WBIN (4)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 4|
|| H | WBIN (5)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 5|
|| I | WBIN (6)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 6|
|| J | WBIN (7)  | Real or EXP  |Absorption Cross Section (CS) Value for BIN 7|

An example of the CSQY table is shown below.

```Tcsh
Table for Mechanism = CB05E51_AE6_AQ
NPHOTAB =   26
!Individual rates listed below:
NO2_IUPAC10
O3_O3P_IUPAC10
repeat for each photolysis reaction
...
...
NTEMP =    6
! I   TEMP( I ) K
  1  210.00
  2  230.00
  3  250.00
  4  270.00
  5  290.00
  6  310.00
! Wavelength and Photolysis Rate Parameters for CMAQ In-Line Photolysis
! calculation . The values are determined by averaging technique over the below
! number of wavelength bins. The technique employs an approach used by the FASTJX
! versions 6 and 7.
!References:
! 1) Bian, H. S. and Prather, M. J.: Fast-J2: accurate simulation of stratospheric
!     photolysis in global chemical models, J. Atmos. Chem., 41, 281-296
     doi:10.1023/A:1014980619462, 2002.
! 2) Hsu, J. and Prather, M. J.: Stratospheric variability and tropospheric ozone,
!    J. Geophys. Res., 114, D06102, doi:10.1029/2008JD010942, 2009.
! 3) Prather, M. J.: Fast-JX versions and utilities, available at:
!    http://www.ess.uci.edu/researchgrp/prather/scholar_software/fast-jx
!    (last accessed: 24 January 2014), 2014.
! Wave bands and Solar Flux
!
N_INLINE_BAND =    7
! I,      START_WL_BIN(nm),    EFFECT_WL_BIN_(nm),       END_WL_BIN_(nm), photon_flux(cm-2*s-1),
  1,               291.000,               294.590,               298.300,            5.8828E+14,
  2,               298.300,               303.151,               307.500,            7.6354E+14,
  3,               307.500,               310.007,               312.500,            5.0412E+14,
  4,               312.500,               316.434,               320.300,            8.9628E+14,
  5,               320.300,               333.076,               345.000,            3.8537E+15,
  6,               345.000,               381.997,               412.500,            1.5457E+16,
  7,               412.500,               607.723,               850.000,            2.1307E+17,
!...  CS  = absorption cross sections averaged over UCI Solar Flux
!...  QY  = quantum yields averaged over UCI Solar Flux
!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model
!PHOTAB       QUANTITY   TEMP   WBIN(  1)     WBIN(  2)     WBIN(  3)     WBIN(  4)     WBIN(  5)     WBIN(  6)     WBIN(  7)
NO2_IUPAC10         CS  210.000 9.420958E-20  1.354622E-19  1.774391E-19  2.174503E-19  3.547179E-19  5.767915E-19  3.786790E-21
NO2_IUPAC10        EQY  210.000 1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  7.963594E-01  4.393946E-02
NO2_IUPAC10         CS  230.000 9.434513E-20  1.353973E-19  1.770686E-19  2.162921E-19  3.512878E-19  5.752929E-19  3.777024E-21
NO2_IUPAC10        EQY  230.000 1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  7.963594E-01  4.393946E-02
NO2_IUPAC10         CS  250.000 9.461621E-20  1.352675E-19  1.763276E-19  2.139758E-19  3.444275E-19  5.722957E-19  3.757493E-21
NO2_IUPAC10        EQY  250.000 1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  7.971660E-01  4.545939E-02
NO2_IUPAC10         CS  270.000 9.488729E-20  1.351377E-19  1.755867E-19  2.116595E-19  3.375671E-19  5.692984E-19  3.737961E-21
NO2_IUPAC10        EQY  270.000 1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  8.052323E-01  6.065865E-02
NO2_IUPAC10         CS  290.000 9.515838E-20  1.350079E-19  1.748457E-19  2.093432E-19  3.307068E-19  5.663012E-19  3.718430E-21
NO2_IUPAC10        EQY  290.000 1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  8.132986E-01  7.585791E-02
NO2_IUPAC10         CS  310.000 9.521259E-20  1.349819E-19  1.746975E-19  2.088799E-19  3.293347E-19  5.657017E-19  3.714523E-21
NO2_IUPAC10        EQY  310.000 1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  1.000000E+00  8.165251E-01  8.193760E-02
repeat (6 temperature bands  x 2 (CS or EQY) lines for each of the 26 reactions:
...
...
after last reaction:
! Ozone Cross-Section for Stratospheric Opacity based on Table 4-5 in
! Chemical Kinetics and Photochemical Data for Use in Atmospheric
! Studies Evaluation Number 15. Sander et. al: NASA-JPL 2006
NTEMP_STRAT =    6
!             QUANTITY   TEMP   WBIN(  1)     WBIN(  2)     WBIN(  3)     WBIN(  4)     WBIN(  5)     WBIN(  6)     WBIN(  7)
O3_STRAT            CS  180.000 6.113743E-19  2.263004E-19  8.354117E-20  3.417408E-20  3.688351E-21  4.086261E-23  1.663175E-21
O3_STRAT            CS  260.000 6.434133E-19  2.434104E-19  9.254548E-20  3.928807E-20  4.583227E-21  4.086261E-23  1.663175E-21
O3_STRAT            CS  300.000 6.685868E-19  2.568540E-19  9.962031E-20  4.330621E-20  5.286344E-21  4.086261E-23  1.663175E-21
O3_STRAT            CS  340.000 6.685868E-19  2.568540E-19  9.962031E-20  4.330621E-20  5.286344E-21  4.086261E-23  1.663175E-21
O3_STRAT            CS  380.000 6.685868E-19  2.568540E-19  9.962031E-20  4.330621E-20  5.286344E-21  4.086261E-23  1.663175E-21
O3_STRAT            CS  420.000 6.685868E-19  2.568540E-19  9.962031E-20  4.330621E-20  5.286344E-21  4.086261E-23  1.663175E-21
```

See this [link](../../src/MECHS/cb05e51_ae6_aq/CSQY_DATA_cb05e51_ae6_aq) for an complete CSQY file for the cb05e51_ae6_aq mechanism.

<a id=inmetfiles></a>
### InMetFiles: List of WRF-ARW output files

Used by: MCIP

MCIP can read WRF-ARW netCDF-based files to generate I/O API-formatted netCDF files for input to CMAQ and emissions modeling. For information about the format of the WRF output files, visit the [WRF-ARW homepage](http://www.mmm.ucar.edu/wrf/users).

<a id=interfile></a>
### InTerFile: Terrain file

Used by: MCIP

WRF output file containing fractional land use data. This file is generated by the WRF program GEOGRID.

<a id=insatfiles></a>
### InSatFiles: GOES cloud data file

Used by: MCIP

[EPA: need a description of this file]

<a id=bndy_conc_1></a>


<a id=init_conc_1></a>

<Image:>

**Figure 8-3. Graphical example of a CMAQ gridded initial conditions file**

<a id=jtable></a>


<a id=emis_1></a>





<a id=ocean_1></a>



<a id=dust_lu_1></a>

<a id=ltngno></a>

<a id=ltngparm_file></a>


<a id=nldn_strikes></a>

<a id=beld4_lu></a>


<a id="init_medc_1"></a>
### INIT_MEDC_1 – Soil initial conditions file

Used by: CCTM – bidirectional NH<sub>3</sub> flux version only

The ASXfile output for the previous day from the bidirectional NH<sub>3</sub> and/or Hg model is used to initialize the soil conditions for each simulation day. This file contains soil NH4 concentrations, soil pH, and Soil, vegetation and water Hg.

<a id="grid_cro_2d"></a>


<a id=grid_dot_2d></a>


<a id=outputs></a>

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch07_programs_libraries.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch09_grid_defn.md)  
CMAQ Operational Guidance Document (c) 2016  

<!-- END COMMENT -->
