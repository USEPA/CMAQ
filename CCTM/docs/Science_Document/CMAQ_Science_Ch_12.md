<!-- BEGIN COMMENT --> 

[<< Previous Chapter](CMAQ_Science_Ch_11.md) - [Home](README.md) - [Next Chapter >>](CMAQ_Science_Ch_13.md)

<!-- END COMMENT -->

Chapter 12 
================

METEOROLOGY-CHEMISTRY  INTERFACE  PROCESSOR  (MCIP)  FOR 

MODELS-3  COMMUNITY  MULTISCALE  AIR  QUALITY  (CMAQ)  MODELING 

SYSTEM 

Daewoo  W.  Byun·  and  Jonathan  E.  Pleim .. 

Atmospheric Modeling Division 

National Exposure Research Laboratory 
U.S. Environmental Protection Agency 

Research Triangle Park, NC 27711 

Ruen  Tai  Tang 
Dyntel Corporation 

Research Triangle Park, NC 27711 

Al  Bourgeois 

Lockheed Martin/U.S. EPA Scientific Visualization Center 

Research Triangle Park, NC 27711 

ABSTRACT 

The Meteorology-Chemistry Interface Processor (MCIP) links meteorological models such as 
l\.1M5 with the Chemical Transport Model (CTM) of the Models-3 Community Multiscale Air 
Quality (CMAQ) modeling system to provide a complete set of meteorological data needed for air 
quality simulations.  Because most meteorological models are not built ·for air quality modeling 
purpose, MCIP deals with issues related to data format translation, conversion of units of 
parameters, diagnostic estimations of parameters not provided, extraction of data for appropriate 
window domains, and reconstruction of meteorological data on different grid and layer structures. 
To support the multiscale generalized coordinate implementation of the CMAQ CTM, MCIP 
provides appropriate dynamic meteorological parameters to allow mass-consistent air quality 
computations. The current implementation of MCIP links l\.1M5 meteorological data to CMAQ 
CTM.  Because its code has a streamlined modular computational structure, adapting the system to 
other inputs only require inclusion of a reader module and coordinate related routines specific for 
the meteorological model. 

On assignment from the National Oceanic and Atmospheric Administration, U.S. Department of Commerce. 

Corresponding author address: Daewon W.  Byun, MD-80, Research Triangle Park, NC 27711.  E-mail: 
bdx@hpcc.epagov 

••On assignment from the National Oceanic and Atmospheric Administration, U.S. Department of Commerce. 


12.0  METEOROLOGY-CHEMISTRY  INTERFACE  PROCESSOR  (MCIP)  FOR 
~~~DE;,LS-~.,,,CQMMUNITY MULTISCALE  AIR  QUALITY  (CMAQ)  MODELING 
SYSTEM 

12 .1  Introduction 

" 

'"" 

, 

1 

:11111111 

" 

, 

"'' 

"' 

:w,111·.11

1111111'"11':1'' 

Models-3 Community Multiscale Air Quality (CMAQ) modeling system is a ~ext generation 
modeling system designed to handle research and application issues for multiscale (urban and 
regional) and multi-pollutant (oxidants, acid deposition, and particulates) air quality problems.  Its 
Chemical Transport Model (CTM) uses a generalized coordinate system.  To accommodate 
meteorological inputs from a variety of meteorological models using different coordinate systems, 
CMAQ CTM (CCTM) requires information about the coordinates and grid as well as the 
meteorological data.  The Meteorology-Chemistry Interface Processor (MCIP) links a 
meteorological model with CCTM to provide a complete set of meteorological data needed for air 
quality simulation.  Because most meteorological models are not built for air quality modeling 
purposes, MCIP cikes care of many issues related to data format translation, conversion of units of 
parameters, diagnostic estimations of parameters not provided, extraction of data for appropriate 
window domains, and reconstruction of meteorological data on different horizontal and vertical 
grid resolutions through interpolations as needed.  Considering these functions, it is not difficult to 
see that MCIP is a key processor in the Models-3 CMAQ system. 

In the Models-3 CMAQ system, the role of MCIP is further expanded to enforce consistency 
among the meteorological variables.  The consistency among meteorological parameters and the 
way they are utilized in a CTM greatly influence the success of air quality simulations.  This issue 
becomes a dominant concern for the CCTM, which uses a generalized coordinate system, because 
it should be able to deal with data from different meteorological models that may or may not use 
fully compressible formulations (or assumptions on the atmospheric dynamics such as hydrostatic 
or nonhydrostatic approximation).  Chapters 5 and 6 of this science document provide detailed 
descriptions on the generalized coordinate system. 

CMAQ's MCIP provides similar functions as the meteorological preprocessor for Regional Acid 
Deposition Model (RADM) (Chang et al., 1987,  1990).  MCIP' s code has a streamlined 
computational stri.icture, incorporating many of the physical and dynamical algorithms necessary to 
prepare meteorological inputs used by CMAQ.  Some of the planetary boundary layer (PBL) 
parameterizations are extensively updated subroutines of the RADM's meteorological preprocessor 
which was described in Byun and Dennis (1995).  MCIP is highly modularized to accommodate 
data from different meteorological models.  This versatility is accomplished by allowing 
incorporation of a different set of input modules for a specific meteorological model.  At present, 
two sets of input modules are available.  One links to the output of Pensylvania State 
'Oniversity/National Center for Atmospheric Research (PSU/NCAR) Mesoscale Modeling System 
Generation 5 (MM5) with CCTM and the other links to meteorological data already in the Models-3 
input/output applications programming interface (1/0 API) format.  MM5 can be run either in 
hydrostatic mode using a time dependent terrain-following hydrostatic pressure coordinate, or in 
nonhydrostatic mode using a time independent terrain-following reference pressure coordinate. 
For the details on how MM5 simulations are conducted and how the reference state is determined, 
refer to Seaman et al. (1995), Seaman and Stauffer (1993), Dudhia (1993), Grell et al. (1994), 
Stauffer and Seaman (1993), Tesche and McNally (1993), and Haagenson et al. ( 1994).  To 
characterize past atmospheric conditions properly, MM5 is usually run with Four Dimensional Data 
Assimilation (FDDA) for air quality simulations.  It incorporates the results of intensive 
meteorological observations into the model simulations so that the uncertainties associated with 
meteorological input for a CTM are minimized (Stauffer and Seaman, 1993; Seaman et al.,  1995). 
Different output data generated by the different options, such as dynamics cloud parameterization 
and surface-PBL algorithms used for the operation of MM5 operation, can be handled by MCIP 
accordingly. 

This chapter provides a detailed description of the functions and data flow of MCIP and 
formulations used for estimating parameters diagnostically.  Although this chapter mainly describes 
the current implementation of MCIP written for MM5, it also provides key information necessary 
to build different MCIP versions for other meteorological models.  It is hoped that developers of 
different MCIP versions can concentrate their efforts to read in data files from different 
meteorological models with minimal modifications for diagnostic routines and output processes. 
Section 12-1  describes basic functions and data dependency of MCIP.  Section 12-2 deals with 
meteorological data types, coordinates, and grids.  Section  12-3 contains descriptions of diagnostic 
algorithms used for estimating parameters necessary for air quality simulation and Section 12-4 
describes additional parameters needed for the generalized coordinate system.  Section 12-5 
provides key operational operational details, such as building and executing MCIP including how 
to set up grids/domains and environmental variables for different runtime options.  Refer to Figure 
12-1  for the structure of the contents of the present chapter. 

L 


FUNCI10NS 1 - - - - . i  

MCIP 

## 12.1 Overview

OPERA TIO 
(Sudan 12.5) 

MCIPModJlcs 
(Sution 12.5. /) 

Bulldl~ MCI' 
(Sudon 12.5.2) 

Providi'fl boundary 

layer paran<t<rs 
mb.tinxfrom or 
incunJlstt,. wllh 
mdeorolo&ical 

mal<I t>Jtput ( e.i;. 

PBLlwlsht, 

dtposilion  vtlocilia, 
cloud paramttrrs ) 

Dc6ni~ Grid& 

Dnm.iin 

(Sudan 12.5.4) 


Figure 12-1.  Contents and Structure of Chapter 12 

• 

### 12.1.1 MCIP  Functions 

One of MCIP' s functions is to translate meteorological parameters from the output of a mesoscale 
model (e.g., MMS) to the Models-3 1/0 API format (CoatS,  1996), which is required for 
operations of Models-3 CMAQ processors.  Some other necessary parameters not available from 
tfie meteorologicaf model are estimated with appropriate diagnostic algorithms in the program.  The 
~ey functions ofMCIP are summarized below. 

Readim:  in  meteorolo2ical  model  output  files: 

:li11'1:: 

.. 

. . . .   ,,,:,1111111111111 

'"11111111. 

. 

.... 

,•,, 

.. 

.. 

... 

... 

. 

. 

. 

The EPA-enhanced MM5 version (P\eim et al.,  1997) generates not only the standard MMS output 
but also several additional files that contain detailed PBL, cloud, and surface parameters.  MCIP 
reads these files and stores the information in the memory for further processing.  Essential header 
information is passed to the Models-31/0 API file header. 

111 

'

J:;f'traction  of meteorolo2ical  data  for  CTM  window  domain: 

In general, the CCTM uses a smaller computational domain than MMS.  Also, MMS predictions in 
th.~ cell.~ neru: .. ,,,,~e. ~,oun~~ may not be adequate for use in air quality simulation.  Therefore, MCIP 
extracts only the portion of the MMS output data which falls within the CCTM's main domain and 
boundary cells. 

Interpolation  of coarse  meteorological  model  output  for  finer  grid: 

When user requests meteorological data on a finer resolution grid than that simulated in the 
meteorological model, MCIP interpolates profile data using simple bilinear interpolation. 

Collapsing  of  meteorolot:ical  profile  data  if  coarse  vertical  resolution  data  is 
requested: 

MCIP performs a mass-weighted averaging of data in the vertical direction.  For example, 30-layer 
meteorological data may be lumped into  15 layers, or 6 layers for the CCTM. 

Computation  or  passing  through  surface  and  PBL  parameters: 

Depending on the user options, MCIP either passes through surface and PBL parameters simulated 
by the meteorology model directly or diagnoses them using the mean wind, temperature, and 
humidity profiles, surface data, and detailed landuse information available. 

Diagnosing  of  cloud  parameters: 

When important parameters needed for processing cloud effects in the CCTM are not provided by 
the meteorological model, MCIP diagnoses cloud information (i.e., cloud top, base, liquid water 
content, and coverage) using a simple convective parameterization.  The information can be used in 
the CCTM to process aqueous-phase chemistry and cloud mixing as well as to modulate photolysis 
rates that reflect the effects of cloud. 

-

Computation  of  species-specific  dry  deposition  velocities: 

MCIP computes dry deposition velocities for important gaseous species using either diagnosed 
PBL parameters or the surf ace/PBL information passed through from the meteorological model. 

Generation  of  coordinate  dependent  meteorolot:ical  data  for  the  generalized 
coordinate  CCTM  simulation: 

Many of the coordinate-related functions traditionally treated in a CTM have been incorporated as a 
part of the MCIP functions.  This change was necessary to maintain modularity of the CCTM 
regardless of the coordinates used and to eliminate many coordinate-dependent processor modules 
in the CCTM.  By incorporating dynamically consistent interpolation methods and associated 
subroutines in the CCTM, the dynamic and thermodynamic consistencies among the 
meteorological data can be maintained even after the temporal interpolations. 

Output  meteorological  data  in  Models-3  1/0  API  format: 

MCIP writes the bulk of its two- and three-dimensional meteorological and geophysical output data 
in a transportable binary format using the Models-3 input/output applications program interface 
(110 API) library. 

### 12.1.2 MCIP's  Data  Dependency 

MCIP processes meteorological model output files in order to provide environmental data needed 
for the other computational subsystems in Models-3 CMAQ.  Landuse data is also required to 
generate additional meteorological information needed for air quality simulations.  MCIP utilizes 
this and profiles of temperature, moisture, and wind components to estimate parameters for the 
turbulence and surface exchange characteristics.  When the meteorological model computes all the 
riecessafy information, it can be passed through the MCIP as well.  The inputs for MCIP consist of 
operational inputs and meteorological model output files.  These inputs are described below. 

#### 12.1.2.1 Environmental  Variable  Inputs 

The user can select a computational path among the internal process options and define parameters 
in MCIP output files by specifying several UNIX environmental variables.  These settings allow 
~CIP to be c~pfigured to fit a particular meteorological model simulation and to follow process 
sfeps requested by the user.  First, it defines the mode of meteorological model run to check if the 
meteorology data linked have been generated using options compatible with the MCIP.  Then it 
assigns filenames of meteorology model output files, defines CTM model window domain offset 
coordinates in terms of the meteorological model grid definition, determines simulation time and 
duration of the MCIP, and assigns appropriate landuse data file typ~.  Refer to Section 12-5 and 
Table  l2~10 for the details of the UNIX environmental variables used in MCIP for these settings. 

#### 12.1.2.2 Meteorology Model  Outputs 

It is anticipated that MCIP will include a unique set of reader modules for a variety of 
tjjeteorological models.  Most of the idiosyncrasies of meteorological data from a specific 
meteorological model should be resolved in this module.  They include the number of files, 
foformation on the data such as where they are stored and in what format, etc.  In the following, 
we provide a description of the output files from MM5 as an example. 

Standard  MM5  output 

~e s~d,~~,,,~,;? time-st~pped grid-domain output contains most of the key meteorological data 
written by the MM5 subroutine OUTI AP.  Each time-step includes a header record.  This header is 
read by MCIP at each time-step, but only the header from the first time-step is used and the 
subsequent headers are ignored.  Data following each header consist of two- and three-dimensional 
arrays.  The time-step interval for the meteorological data is taken from the header record on the 
f!~st time-step. 

EPA-added  MMS  output 

EPA and MCNC added two more output files for additional information to facilitate air quality 
modeling.  One includes additional two-dimensional boundary layer parameters and flux  values 
and the other contains the detailed Kain-Fritsch (Kain and Fritsch, 1993) cloud data file,  which 
describes locations and cloud lifetimes of convective clouds.  Currently, only the first file is used 
to process CMAQ dry deposition module options and the second file is not actively used because 
the CCTM does not yet support a corresponding aqueous-phase Kain-Fritsch cloud mixing 
module. 

### 12.1.2.3 Landuse  Data 

MCIP requires landuse data that define surface characteristics in order to compute dry deposition 
velocities and other PBL parameters.  Depending on the PBL and dry deposition modules desired, 
the needs for landuse data vary.  The RADM dry deposition algorithm (Wesely,  1989) needs 11-
category fractional landuse data.  On the other hand, the new CMAQ deposition algorithm requires 
cell-averaged parameters defining surface exchange characteristics (i.e., landuse dependent 
parameters).  The latter algorithm assumes that the landuse dependent subgrid effects are processed 
in the meteorology modeling system.  To avoid incorrect averaging of the land-surface parameters, 
it distinguishes a dominant water cell from other landuse types. 

Table 12-1.  Relations Among MMS, CMAQ/RADM, and USGS Landuse Categories 

MMS  MMS  Category  MCIP 

CMAQ/RADM 

USGS 

USGS  Category 

I 
2 

3 

4 

5 

6 

7 
8 

Urban Land 
Agriculture Land 

Range-Grassland 

Deciduous Forest 

Coniferous  Forest 

Mixed  Forest/Wet 
Land 

Water 
Marsh or Wet Land 

I 
2 

3 

4 

5 

6 

7 
9 

Cate2ory 

Urban Land 
Agriculture 

Range 

Deciduous Forest 

Coniferous  Forest 

Mixed Forest and 
Wet Land 

Water 
Nonforest Wet 
Land 

9 
10 

Desert 
Tundra 

11 
8 

Rock,  Open Shrub 
Barren Land 

Permanent Ice 
Tropical  Forest 
Savannah 

11 
12 
13 

None 

7 
3* 
3 
10 

Water 
Range* 
Range 
Mixed 
Agriculture/Range 
Land 

1 
2 
3 
4 
7 
8 
9 
10 
12 
16 
13 
14 
17 
6 

15 
18 
19 

20 
21 
22 
23 
24 
25 
26 
27 

l· l 
5 

Urban/built  up 
Drv cropland &  pasture 
Irrigated cropland &  pasture 
Mixed  drvland/irrigated  pasture 
Grassland 
Shrubland 
Mixed  shrubland/grassland 
Chaparral 
Broadleaf deciduous forest 
Deciduous coniferous  forest 
Evergreen  coniferous  forest 
Sub alpine  forest 
Evergreen broadleaf 
Woodland/cropland  mosaic 

Mixed  forest 
Water 
Herbaceous 

Forested  wetlands 
Barren  or sparsely  vegetated 
Shrub &  brush tundra 
Herbaceous tundra 
Bare ground tundra 
Wet tundra 
mixed tundra 
Perennial  snowfields  or glaciers 

Savannah 
Grassland/cropland  mosaic 

* Mappmg ''Tropical Forest" to "Range" 1s not appropnate m most lo_cations. 

Currently, MCIP accepts four different types of landuse data.  Two types of landuse input (one 
from MM5 directly, or the other from TERRAIN, a preprocessor for MMS system) contain 13-
category data.  The third one is a preprocessed landuse data set in an ASCII file.  Recently, we 
added a landuse processor (LUPROC) for the USGS vegetation information (described below) to 
i11:1prove landuse data quality.  For the case of MMS, the percentages of MMS's 13  landuse 
category are transformed into the CMAQ/RADM's 11-category fractions in the landuse reader 
module. The USGS vegetation category is also transformed into the 11-category fractions.  Table 
12-1  provides the conversion rules for landuse types from MMS  13-category, and USGS 27 
(~ttp://edcwww.cr.usgs.gov/landdaac/glcc/ nadocl_l.html) category to CMAQ/RADM's 11-
category.  The USGS North America land cover characteristics data base has 1-km nominal spatial 
resolution and is based on 1-km A VHRR data spanning April  1992 through March 1993.  This 
data base has been adapted to the CMAQ' s base map projection which uses Lambert conformal 
projection with origins at latitude 40° N and longitude 90° W. 

""""'""'"'""" 

' 

' 

" 

'" 

"'""' 

" 

L,~nduse  P£pc~~sor CLUPRQC) 

'',11,, 

,11111' 

111! 

,, 

CMAQ' s Landuse Processor (LUPROC) is a special processor that provides a high-resolution 
landuse data base for the system.  MCIP requires landuse data that define surface characteristics to 
dompute dry deposition and other PBL parameters.  Depending on the PBL and dry deposition 
modules desired, the needs for the landuse data are somewhat different. 

LUPROC windows out the landuse data for the user-defined domain and converts percentages of 
27 vegetation categories in the database into the fractional landuse data in RADM's 11-category. 
The output of the LUPROC should have the same resolution as the CCTM domain and the 
LUPROC domain should include the boundary cells in addition to the CCTM's computational 
domain.  In the near future, MMS will be upgraded to allow use of the USGS land cover 
characteristic data as an option.  When the CMAQ dry deposition algorithm is used and necessary 
J?BL parameters are provided by the MMS directly, LUPROC will not be needed. 

### 12.1.3 Computational  Structure 

'the MClp d~Ja flpw diagram, Figure 12-2, shows the key processing sequences.  First, MCIP 
executes the one-time processes such as:  reading the header, processing other operational 
information for the meteorological output, reading appropriate landuse data, and generating time 
il'ldependent MCIP output files.  Then, MCIP loops through the time stepped (hourly or sub(cid:173)
hourly depending on the meteorology data) input data from a meteorology model performing the 
functions described above.  The processing sequence of MCIP is summarized below: 

• 

• 
• 
• 
• 

1: 

I 

•'Ill 
Ill 

, 

"11 
1:,J:J 

.:!iii:. 

,, 

GETMET: reads and extracts meteorology data from standard MMS output for the CCTM 
window domain, converts variables into SI units and process special files (e.g., two(cid:173)
dimensional surface/PBL data and Kain-Fritsch cloud files); 
PBLPKG/PBLSUB: computes PBL parameters using diagnostic method if desired; 
BCLQPR~_AK: computes diagnostic convective cloud parameters if needed; 
SOLAR: computes solar radiation parameters; 
RADMDRY/M3DDEP: computes dry deposition velocities; and 
METCRO_OUT &  METDOT_OUT: computes additional meteorology data required for the 
generalized CTM, interpolates mean profile data into finer grid resolution if needed, and 
output Models-3 1/0 API meteorology files. 

START 

EPA/600/R-991030 

• read met. data 
• reconcile coordinate 

....---------. • unit adjustment 
.__ __ ___  __,  • process special files 

• horizontal inteq>olation 

(e.g., 20 met. &  cloud files) 

•  compute Jacobian, entropy, density 

GETLUSE 

GRID_OUT 

YES 

PBLPKG 

compute all PBL parameters 

PB LS UP 

supplement PBL parameters 

MET3DSUP 

'-----......-----'•complete met data 

NO 

RADMDRY 

M3DRY 

END 

Figure 12-2.  Flow Chart of MCIP Showing Key Data Processing Sequence. 


i ..... i. 2  Data  Types,  Coordinates,  and  Grids 
MCIP's essen'tial role is to provide consistent meteorological data f~r the CMAQ .modeling system. 
Therefore, it is very important to understand the vertical coordinate used in the meteorological 
modeling system.  For example, MM5 can be used either in hydrostatic mode, in which the 
hydrostatic pressure (time dependent) is used to define the terrain-influenced vertical coordinate, or 
in nonhydrostatic mode, where a reference hydrostatic pressure (time independent) in a normalized 
height form is used as the vertical coordinate.  Many parts of current MCIP code deal with these 
differences in the vertical coordinate specifically.  In the following we provide technical 
information related with coordinates and grids used in MCIP. 

### 12.2.1 Meteorological Data Types 

Many different combinations of approximations are used for describing the atmosphere in 
meteorological models.  Therefore, classification of MCIP output parameters based on detailed 
classification of vertical coordinate types (such as geometric height, pressure, terrain-following 
coordinates, etc.), and their application approximations (such as hydrostatic and nonhydrostatic), 
can be exceedingly complex for the CTM with generalized coordinate implementation.  Unlike 
previous use of detailed classification of meteorological coordinates for determining meteorological 
data type, we classify the coordinate types mainly based on the temporal dependency of the 
~H~obians (explained later).  The benefit of this distinction is obvious.  For example, a height 
coordinate which is time independent may require only a time constant file for describing the 
vertical coordinate while a dynamic pressure coordinate requires several parameters related with the 
coordinate description that needs to be stored in a time dependent file. 

1'"1, 

,11'111111. 

111111111111111111 

11"111111111" 

111111111,,::1' 

"11111111, 

"' 

'" 

,' 

' 

""' 

"' 

,, 

A similar distinction is made based on the need for describing data in different horizontal positions, 
such as flux-point data for horizontal wind components and cross-point for most other scalar 
parameters.  For MM5, two horizontal wind components are defined at so-called 'dot' points while 
all other scalar values are at 'cross' points, following Arakawa-£ grid definition.  Refer to Figure 
12-3 for the definitions of Arakawa C-grid and E-grid (Mesinger and Arakawa,  1976).  For the 
CCTM, certain flux data are defined on the Arakawa C-grid in which the flux points are not 
collocated in  £ 1 
- and £ 2 -directions.  Therefore, MCIP interpolates MM5' s dot-point wind 
components linearly and multiplies the result with the two-point averaged density to provide the 
flux-point momentum component data.  Both the flux-point and dot point data require array sizes 
larger than the cross-point data by one cell in each horizontal direction. 

111,:1111:,,,,, 

:lh 

llll' 

'II 

'' 

CMAQ utilizes contravariant wind components, instead of the regular wind components, to advect 
tracer species.  Figure 12-4 shows that the east-west component of the contravariant wind field,  u , . 
is placed at the x1 -direction flux points (marked with the square symbol) and the north-south ( .x2-
direction) component of the contra variant wind field,  v, is placed at the  .X2 -direction flux points 
(marked with the triangle symbol).  The vertical wind component is defined at the full-layer height. 

The Models-3 I/O API requires individual data components to have exactly the same temporal and 
spatial dimensions in a file.  Because the flux-point data and dot-point data both need additional 
column or row positions, we can combine them together in a so-called DOT file, which is larger 
than the CRO file by one cell for each horizontal direction.  It is important to note that because the 
flux points are different from the dot-points, they must be shifted by a half cell east or north 
(depending on whether they are square-point or triangle-point flux values) for graphical 
visualization of the flux-point parameters. 

Figure 12-3.  Two Different Grid Point Definitions, Arakawa C- and B-grids, Used in MCIP. 
u and v are horizontal wind components, s represents a scalar quantity, and pis density of air.  For 
MM5,  p •  is used instead of p. 

( 
/$· ....  , .....  1  ...... 

,..., 
I 

I 

. / 

/ 

¥-- --:::._  -1 _-L·~ 

·-1 

" 

....... .-

........... 

····~·  ........ . 
,,.· 

/ 

:  / 
/ 

l-112 

- - - - - - - - - - - - - - - -

l+l/2 

k+l/2 

k 

A2 x 
~ 

k-112// 

m+l/2 
/ 

/ 

/ 

/ 

/ 

/ 
Al 
•  x 

Figure 12-4.  Computational Grid Points and Corresponding Indices Used in MCIP. 
Markers are for dot points (e), cross points ( X), x-direction flux points (0), y-direction flux 
points (.0.), and vertical cross point at layer interface ( ®). 


'':,, 

~p A/600/R-99fP30 

:111,,, 

:I. 

,,  ' I ~ 
,, ~ 
illl'"' 

, 

11 I ' 

Furthermore, when these two flux components are assigned to form vectors ·in a visualization 
program like PA VE (Thorpe,  1996), the starting points of the vectors in the visualization are 
relocated at the dot-points although the two components are actually not collocated there. 

' 

• 

• 

11:1' 

' 

.::::,,1111:: 

.. ,,,111111111''·11• 

Data for the boundary cells are defined with a special 1/0 API boundary data type.  Depending on 
tpe need to d~scril:>e boundary mass flux accurately, one may want to have a boundary domain with 
NTHIK cells (see Figure 12-5).  The boundary grid is  represented as the external perimeter to the 
main grid. This perimeter is NTHIK cells wide (where you may use a negative NTHIK to indicate 
an internal perimeter as used by such air quality modeling systems ROM and RADM).  The 
boundary array is dimensioned in terms of the sizes of the arrays surrounding the main domain. 
Current Models-3CMAQ system uses NTHIK=l  throughout ttie system components. 

""""' 

"'"' 

' 

"' 

"""'' 

,, 

' 

"'" 

•" 

• 

11111' 

' 

).  Vertical wind component and flux values are examples of the full-layer 

Finally, the dimensionality of the parameters (i.e., whether they contain three-dimensional or two(cid:173)
dimensional information) is used to distinguish the data type of meteorological parameters.  In 
vertical direction, we define half and full layer positions based on the values of the generalized 
vertical coordinate, ( .£3 
parameters.  Although full-layer data require one more data point vertically, often the flux  values at 
the bottom boundary (i.e., at the lowest full-layer) are zero.  Therefore, we do not need to use 
additional data types for these, as lorig as we save the non-zero lowest full-layer data in a 
corresponding two-dimensional data type file separately. (Refer to Table 12-2 for dimensions used 
for each grid points.)  Because a file represents a set of data with the same data type (with only a 
few exceptions) in the Models-3 system, locating meteorological parameters from an appropriate 
r7b API file fs relatively easy.  Table 12-3 summarizes the possible data types for the  ····· 
meteorological parameters.  Depending on the choice of coordinates and grids, some of data types 
may not be relevant.  For example, the current version of MCIP does not use GRID _DOT _3D and 
MET_DOT_2D data types.  Appendix 12A provides the list of MCIP output parameters in each 
data file. 

· 

i" 

I 

"I,' 


NTHIK 

-

~ 

-
-

61 

62 

63 

64 

49 

50 

51 

52 

95 

96 

65166167168169170  I 11 

~oundary Component-N 
53 
57 

54 

56 

55 

58 

59 

h 

MAIN GRID (NCOLS, NROWS) = (10,10) 

)X 

& 

~ 

-

~·coLs 

~ 
0 
~ 

-
-

EPA/600/R-991030 

k~  I • ~ 

.;; 
f 

0 

y(l4) 

~ 
~ 
0 

f 

r(l2) 

r(l l) 

y(l3) 

r(lO) 

y(l2) 

48 

46 

44 

42 

r(9) 

y(l l) 

72 

60 

47 

45 

43 

41 

39 

40 
t:=-
= 
37  §  38 
c..-
35  ~ 36 
n -= 
33  a  34 
~ =-
= 
31  ~  32 
..... 
~-
30 
-

29 

27 

28 

26 

r(8) 

y(IO) 

r(7) 

y(9) 

r(6) 

y(8) 

r(5) 

y(7) 

r(4) 

y(6) 

r(3) 

y(5) 

r(2) 

y(4) 

r(l) 

y(3) 

... 

~ 

(/) 

~ * <:;! 
~ 
~ 
~ 
>< 
u:i, 
(/) 

~ 

94 

93 

-91  ~ 92 
.... 
-5 
89  !  90 
-E' 
87  8  88 
7s~ 86 
-§  
83  ~ 84 

81 

79 

77 

75 

73 

82 

80 

78 

76 

74 

,, 

x(l) 

1~ 

(XORIG, YORIG) 

,  r 

25 

13 

I 

14 

2 

15 

16 

17 

18 

19 

20 

21 

uOUD lary Component-S 

3  I  4  I  5  I  6  I  1  I  8  I 9 

22 

23 

24 

r(O) 

y(2) 

10 

II 

12 

r(-1) 

y(l) 

c(-1) 

c(O) 

c(l) 

c(2) 

c(3) 

c(4) 

c(5) 

c(6) 

c(7) 

c(8) 

c(9) 

c(lO)  c(l l)  c(l2)  =NCOLS+NTHlK 

x(2) 

x(3) 

x(4) 

x(5) 

x(6) 

x(7) 

x(8) 

x(9) 

x(lO)  x(l l)  x(l2)  x(13)  x(l4) =NCOLS+2*NTHlK 

NCOLS_EXT =NCOLS+2*NTHIK 

~1 

Note: Individual boundary component grids are treated as a 1/0 API 2-D file 
S(l:NCOLS+NTffiK, 1-NTHIK:O) 
E(NCOLS+ 1 :NCOLS+NTHIK, 1 :NROWS+NTHIK) 
N(l-NTHIK:NCOLS, NROWS+ I :NROWS+NTHIK) 
W(l-NTHIK:O, 1-NTHIK:NROWS) 

Figure 12-5.  Grid Data Structure for Models-3/CMAQ Showing Main Grid and Boundary 
Components.  Here NTHIK=2, NCOLS=IO, NROWS=10 are used for the illustration. 


WA1600IR-99t()30  . 

Table 12-2.  Dimensions and Indices for Grid Points used in MCIP and CCTM 

Dot-Point 

Cross-Point 

(start: end) 

(start:  end) 

dimension 

dimension 

index 

index 

,... 
x  -
direction 
Flux Point 

x2 -direction  Full-Layer  Half-Layer 
Flux Point 

(start:  end) 

(start:  end) 

dimension 

dimension 

index 

NIA 

index 

NIA 

(l:NCOLS+l) 

(l:NCOLS) 

(l:NCOLS+l) 

(l:NCOLS) 

NCOLS+l 

NCOl..S 

NCOLS+I 

NCO LS 

/±112 

I 

/±112 

l 

(l:NROWS+I) 

(l:NROWS) 

(I: !\'ROWS) 

(I: NROWS + 1) 

NIA 

NIA 

NROWS+l 

NROWS 

NROWS 

NROWS+l 

m±ll2 

NIA 

m 

NIA 

m 
NIA 

m±ll2 

NIA 

(O:NLAYS) 

(l:NLAYS) 

NLAYS+I 

NLAYS 

k± 112 

k 

xi 

.x2 

.x3 

11 

''I' 

1111111 

' 

II' 

Table 12-3.  Classification of Meteorological Data Types in the CMAQ System. 
Q.ata types GRID_DOT_3D and MET_DOT_2D (in gray color) are not used in CMAQ currently . 

1111111111,,,,:11, 

" 1 

•" 

"11 

:111;,,,, 

:111111111 

'1' 

11

,.11

•1 

II 

" 

'" 

"' 

" 

'Ill' 

1111 
111 "!1

' 

1 

11,,:I 

1
""" 

' 

'ii:::::: 

i'I"  I 

:·•  111111!111,J 

.. '' ..• i111lii 

''i' .... 
Time Independent 

,,  ,,,,''·,,,• 

''" 

', 

11 ~ : 

" 

l'h 

,, 

'I 

millll .. 

, 

Time Dependent 

GRID_CR0_2D 
GRID_DOT_2D 
GRID_BDY_2D 

GRID_CR0_3D 
GRID_ DOT_ 3D 
GRID_BDY _3D 

MET_CR0_2D 
MET_DOT_2D 
MET_BDY_2D 

MET_CR0_3D 
MEf_DOT_3D 
MET_BDY_3D 

### 12.2.2 

#### 12.2.2.1 Horizontal  Coordinates  and  Grid 

MCIP can be configured with horizontal coordinates based on conformal map projections, such as 
Lambert Conformal, Polar Sterographic, and Mercator.  Table 12-4 summarizes the necessary 
information for the description of the conformal map projections as defined in Models-3 1/0 APL 
To generate 1/0 API files, which require exact definitions of the grid and coordinate system, users 
need to provide all the necessary map projection information in addition to ve1tical coordinate and 
layering definitions.  The Models-3 CMAQ system uses square cells for the horizontal grid 
representation.  A modeling domain is defined with the integer multiples of the square cells in E-W 
CS?lu~-:--Vi~,~) ~.? ~-S ~,row-w~~e) dir~.~tions.  Although a rectangular cell shape can be handled 
by the 1/0 API, the fractional time splitting approach used for the modularization of CTM 
processes requires use of square grid to maintain the accuracy of the finite differencing algorithms 
consistently.  The effect of different horizontal coordinate is reflected in the values of the map scale 
factor.  Because of the need to couple the map scale factor (m) with other state variables and wind 
components, MCIP provides map scale factor for both the dot- and cross-points. 

Table 12-4.  Map Scale Factors and Parameters Defining Horizontal Coordinates in Models-3 I/O 
API (Coats,  1996) 

Map Parameters 

Map Scale (m) 

Note 

NIA 
m=l 

m= sin(ir/2-¢,)[ l'ilil(tr/4-¢/2} J (X 1cent,X2cent) = (A0 ,</J,,) 

sin(tr/2-¢)  tan(tr/4-¢1/2) 

Coordinate 

lat.-long. 

ID in 
Models-3 
1/0 API 
LATGRD3=1 

Lambert 

LAMGRD3=2 

Mercator 

MERGRD3=3 

Stereo-
graphic 

STEGRD3=4 

UfM 

UTMGRD3=5 

NIA 

0

Pa  = "'· ~ IP  =  l/J2 
two  latitudes  that 
determine the 
projection  cone. 
P,,  =  A,
,  central 
meridian 
P,.  = tPo•  Pp= ./to: 
latitude  longitude of 
coordinate  origin 
within  the  tangent 
circle. 
P,, : angle  between 
cylinder axis  and  the 
North  polar  axis 
Pa=l/Jo•  Pp=Jlo;_ 
latitude and  longitude 
of the  point  of 
tangency. 
P,.:  angle from  true 
north  to  .X2-axis 
Pa  is the UTM  zone 

PP' pr  not used 

n =In[ sin(tr/2-¢,)] 

sin(tr/2-¢1) 

·[in( tan(ir/4-¢,/2) JT 

tan(tr/4-1/1112) 

cos¢. 
m=--
costfl 

I +sin¢. 
m=--
l+sin¢ 

m=l 

(i1 ,i2 )=(long,lat)  are  in 
degrees 

for the center of 
coordinate  system. 
(i1 ,i2 )are in meters 

) 

(x 1cen1,X2cen1) =  (A0 ,¢0
for the center of 
coordinate  system. 
(.i 1 ,.i2 )are in  meters 

(X 1cen1,X2cen1) = (A0 ,</J0
for the center of 
coordinate  system. 
(.i 1 ,i2 )are in  meters 

) 

cl 

.... 2 

) 

X  cent, X  cent  are offset 
from the UTM coordinate 
origin. 
(.i 1,i2 )are in meters 

#### 12.2.2.2 Vertical  Coordinates  and  Grid 

The Models-3 CMAQ system allows many popular vertical coordinates used in meteorological 
models.  We expect that the Models-3 I/O API will be extended to include several other vertical 
coordinates.  The I/O API follows the definitions of vertical coordinates as used in meteorological 
models.  However, in CMAQ the vertical coordinates are redefined to increase monotonically with 
height.  This precaution is needed because the CMAQ code is expected to handle generalized 
coordinate system.  We found that this constraint in the vertical coordinate is extremely useful for 
the implementation of CTM in the generalized coordinate system by removing possible sign errors 
wherever vertical differentiations are involved.  For example, the Jacobian information for a 
c:oordinate can be directly used to replace some of the derivatives involved with the generalized 
vertical coordinate.  This internal change does not require redefinition of the vertical coordinates in 
the MCIP output files.  The impact of this constraint is limited to an include file that defines the 
coordinates <llld vertical layers.  In a typical Models-3 operation, this include file is automatically 
generated by the system framework through the use of the coordinate/grid manager.  In the 
{gllowing two typical application examples are presented; one for the time dependent coordinate 
and the other for time independent coordinate. 

Time  dependent  hydrostatic  sii:ma-pressure  coordinate 

For the MMS system, this coordinate is used when the hydrostatic option is chosen.  In general, 
qiis ~09rqin~!~ C:.!!!l  be U$ed not only for the hydrostatic atmosphere simulations but also for 
n()nhydrostat~c cases.  For example, when a regional scale hydrostatic meteorological model 
~:tovides hydrostatic values, the same coordinate can be used in a nonhydrostatic nested model as 
described by Jaung (1992).  In MCIP implementation, we should view this coordinate as an 
example of processing meteorological data with a dynamic vertical coordinate definition rather than 
an example of a hydrostatic coordinate. 

The CCTM utilizes a generalized coordinate system that allows construction of the vertical layering 
consistent with the meteorological coordinate used.  However, to maintain the vertical coordinate 
definition monotonically increasing with height, the coordinate definition for the CCTM has been 
modified as: 

_x::i  = ~ = l _ Ci_  ;  Ci_  =  P - Pr  _  P - Pr 
Ps - Pr  - P0 

P 

P 

(12-1) 

, x3

, .X2

where  p(x1
, t)  is the hydrostatic pressure,  Pr  is the pressure at the model top which is  held 
~onstant, Ps(x1 ,x2 ,t) is the surface pressure, and  'j/ = Ps - PT.  Be~~use th~ pressure used in the 
definition of the vertical coordinate is in hydrostatic balance, we have: 

i,,,1 

- ' 
ap 
-=-pg 
(k 

The Jacobian is then defined as: 

,11 

1'::11

1 

(12-2) 

(12-3) 

and it is time dependent because the surface pressure and air density are time dependent. 

Time  independent  reference  hydrostatic  siema-pressure  coordinate 

In MMS this coordinate is specifically used for the simulation of the nonhydrostatic atmosphere. 
J'he layer structure is entirely different from the case when hydrostatic option is used.  The 
coordinates defined with this option are time independent and have very similar characteristic to 
those of the normalized geometric height coordinates.  The only difference is that the vertical layer 
thickness is defined with a nonlinear function of the geometric height.  Here, we should view this 
coordinate as an example of processing meteorological data with time independent vertical 
coordinate. 

The terrain-following reference hydrostatic pressure ( <J' Po  )  coordinate, again in a monotonically 
increasing form,  is given as: 

(12-4) 

where  p 0 (x 1 ,x2 ,x3
the model top,  p 0,,(x1 ,x2
height  z .. , and  p; =Pus - PT' 

)  is the hydrostatic pressure of the reference atmosphere,  PT  is the pressure at 
)  is the reference surface pressure which is detennined by the topographic 

Because the reference atmosphere is in hydrostatic balance, we have: 

The Jacobian is then defined as: 

(12-5) 

(12-6) 

and it is time independent because it is a function of the time-independent surface pressure and the 
density of the reference atmosphere.  Here, the vertical coordinate z represents a height above the 
lowest point in a modeling domain, or the mean sea level (MSL) height if there is no place lower 
than the MSL.  Usually, this type of vertical coordinate accompanies with a simple description of 
reference temperature profile of the base state.  For example, in MM5 the base temperature profile 
is defined with a simple expression: 

T,,(z) = T,,,,  + Aln(p0 (z)J 

Poo 

(12-7) 

where the reference values are chosen such that T,,,,  is a sea level reference temperature in K,  p 00 = 
1000 mb =  105 Pascal, and A, which is set to be 50 K, is a measure of atmospheric lapse rate 
represented in temperature difference for the e-folding depth.  With Equations  12-5 and  12-7 and 
the Equation of State, we could relate the reference pressure in terms of height z in a differential 
equation: 

dlnp,(z) = _..[ 

1 

dz 

R  [7;,s-Aln(p00 )]+Aln(p0 (z)) 

(12-8) 

which can be readily solved for  p0 (z)  by the separation-of-variables technique.  The solution is a 
q~adratic equation in terms of In p 0 •  For typical tropospheric conditions, the base pressure can be 
related with the geometric height by taking the positive square root term: 

p 0  z  =exp 
[

() 
. 

-b+"1b2 -2Ac] 

A 

(12-9) 

Equation  12-9 shows thatp0 (z)  is a monotonic, but nonlinear, function of z. 

### 12.2.3 Modification  of  Grid  Structure 

In CMAQ, the horizontal and vertical coordinate information (such as the map projection 
parameters) for CTM simulation domains are required to be exactly the same as (or a derivative of) 
the master coordinates used in the meteorological model simulation.  However, the horizontal grid 
structures of the CTM domains can be redefined depending on the need for air quality simulations. 
Here, the concept of grid family is introduced.  It is as a set of grid domain specifications with the 
following properties: 

1. 
2. 
3. 

the same coordinate origin and map projection; 
a window domain for the parent domain; or 
a nest (multi-stage) domain from a window or the parent domain. 

The Models-3 I/O API file header helps to describe the horizontal grid and coordinates 
unambiguously to position the background map correctly and to provide relations among the 
members (multi-stage nesting domains) of the grid family.  Only with a limited manner, the user is 
allowed to modify the grids in the vertical directions because of the concern that ad hoc 
interpolation may destroy the integrity of the meteorological data. 

#### 12.2.3.1  Windowing 

The windowing functions in MCIP extract MM5 output for a CCTM window domain.  As a rule of 
thumb, a CCTM uses a smaller computational domain than the domain used by meteorological 
models, because predictions in the cells near the boundaries may not be suitable for use in air 
quality simulation.  Therefore, MCIP extracts only the portion of the MM5 output data which falls 
within the CTM' s main domain and boundary cells.  The CTM domain should be located at least 
four or five cells inside the MM5 domain to minimize the boundary effects.  MCIP can, however, 
generate output for the CMAQ domain as large as 2 cells smaller than the meteorology domain, 
because it generates files for the boundary cells with NTHIK=l.  (Note in MM5 terminology, it 
may seem like the CMAQ domain is smaller by three because a DOT point concept is used in MM5 
system for the grid definition.) 

Limitation on the number of horizontal cells (to 120 cells) of the meteorology grid is coming from 
the MCIP's parameters defined in MCIPPARM.EXT: 

PARAMETER  (  MAXI 
& 

=  120, 
MAXJ  = 120  ) 

MET  domain  size  in  N-S  dir. 
MET  domain  size  in  E-W  dir. 

When the meteorology data has larger dimensions, MAXI and MAXJ should be modified 
accordingly.  Also, there coulcJ be some limitations with reader routines specific for meteorology 
models.  Because these reader modules are essentially foreign codes, there is no easy way to 
generalize the input process of the meteorological data.  Refer to Figure 12-6, which shows the 
relation among the meteorology domain, the window domain for meteorological data extraction, 
and the CTM main and boundary domains. 

12-19 

Relations am0ng MM grid, extented-CTM~ grid, ~d CTM grid for NDX::J 

-~""'==~-~=---~ 

,!g 
§ 
~ I 
\0 
\0 
,Cs 
,1.>J 
0 

l=MAXLMM 

x 

! 
l 
!X 
I 
Ii_  L. 
--!1- -1---~- -,- .. 
x I 

x 

I 

--

x 

1----

-

--~-x_.__ 

I 

! 

I 

! 
i 
! X 

i 

• 
; 
X 

I 
.  I 

I 
I 

I 
I 
I xi 

I 

I 
x: 
! 

1 
x: 
! 
I 

I 

! 

I 

x 

x:  I  Xi 
I 
i 
i --1-·11  : 

-··.-

--·-

I 
t 

' 

-

)( 

DXL.._ 

X  IX 

X: 
I 

-~-1'- x 

~trri~ 
x i x 
)(  ! x 

l 
I" 
-1---- ~ 
x I 

'x. I I 
i.-~x~ 
I 
-~  i'~ ;. "_ _ ~ ~X ~-x- --~-\-?<t_x _ ~~~- - --1 ~-~+~+-. 
-~--r.  . 
I 
~I  I 
_j_x_, ___ ~e-.~-!--~1.  ·_1xJ·_  :J:< __  ._  -·~x~_x ___ x __ \_x_\ _ _:__~_x _____ 
i_x-'--. _ ~-\--
1
I 
, 
I 
' 
: 
: 
I x 
J-~-- -- ----'----· 
_J  x; 

xx   x  xlx\x  x;x!x  ><;x 
x1x1x  xix  x  X!><  xfx  x  X 1 x  ><  x:xlx  xx 

•Jxlx  x!x;x  xjx;x  >e;x1x  x;x 
)(  x  x  x  x  x 
-- -
·  x 
x 

_J_ __  ----!:._.  - ~~-'----
x : •  • : x  •  • ; x:  •  •  x 

• 
x  x~x  xx   x 

•(XCENTYCEN'l)=(OO)'  • 

xx   xjx 

'  . 

><jx:x 

---
___ 

j 
t '  
I 

-...;-.-----

---

I 

I 

><I 

i 
I 
-

i 

I 
I 

x 

I 
• 

I 

I 

: 

' 

• 

>< 

I 

• 

! 

I 

j 

T 

x 

x 

' 
I 

1 

l 

! 

X 

>e 

I 

1 

I 
• 

>C 

I 

' 

. 
I 
j 

' 

1 

: 

I 

I 

1 

I 
~  I 
' 
I 
! x 1 __ 
I  1 

-·- - -+- ----~- --·----- ---~-
! x ,_ 
; 
I 

- x 

I 

• 

I 

. 

-·--x-- --- -· >c-:---

I 

x 

,._ ____ .__....__1 

--- --

:E 
:E, 
0 
§ 
;.-. 

I=l 

S::-'--1-----'--~-'-~--i------i--EXTENDED CTM DOMA 

(XORIG_.X. YORIG_X) 

I 

--

\-
' 

x  : 

x 

·X 

J=I 

J 

-- - - ·- x 

XORIG_MM 

\ 

~L --.J~-

' 
I 
I 

x  ~,pox  x 

x 

'.  x 

-;;- !-x ·--.- -~-;-x- -• ·I  .-:-x-~--;;  ·-;'.·-x- --- ---~-;,+-·--

>< 

x 

x 

• 

( 

I 
I 

: 

WINDOW DOMAIN 

I 

··  -

- ----

---- • -
I 
-1- -·-~----
;_x_  t -· ____ '. _~.;.  -

x. 

' 

·-

! 
1_x t 

I 
I 

I 

I 

\ 

--·r·-x-'.·----
' 
-- · ··  ;-
I 
-
' 
----1-
---r' .?< __ ,_ 

MM DOMAJN----1 
J=MAXJ_.MM 

' 

(XORIG_MM. YORIG_MM) 

Figure 6.  Relations among the meteorology domain, window domain for data extraction, 

and CTM's main and boundary domains. 

,_ 
N 
I 
~ 

~ 

#### 12.2.3.2  Horizontal  Interpolation 

A horizontal interpolation function is provided to help users generate higher resolution data than the 
input meteorological data.  This function is used when higher resolution emissions data is available 
but the detailed variations in meteorological fields can be neglected.  This option is useful only 
when the surface and PBL parameters are diagnosed in MCIP.  If interpolation is desired, NDX 
(defined in MCIPPARM.EXT) must be modified before the MCIP is compiled so that data arrays 
are dimensioned properly.  When interpolation is desired, MCIP copies contiguous grid cell values 
from the coarse grid to the fine grid and then performs a two-dimensional bilinear interpolation on 
the three-dimensional data.  The interpolated temperature profile is updated using fine resolution 
landuse data to reflect land-sea boundaries in the profile appropriately.  The interpolated 
temperature, moisture, and wind components profiles, together with the detailed landuse data, are 
used to estimate surface and PBL parameters for the finer resolution.  The user should be reminded 
that this procedure generates higher resolution meteorological data without enhanced physics, thus 
the newly generated data may have consistency problems.  Therefore, such interpolation should be 
used sparingly for cases such as testing higher resolution emissions data with a finer resolution 
CTM.  The procedure is never meant to replace or minimize the need for higher resolution 
meteorological model runs.  It should be noted that the interpolated temperature and moisture 
profiles result in different estimations of cloud parameters (such as cloud bottom and top heights, 
fractions, liquid water contents) as determined by the diagnostic Anthes-Kuo cloud routine. 

#### 12.2.3.3 Vertical  Layer  Collapsing 

A vertical collapsing function is supplied to generate a smaller data set for testing CTM in a smaller 
computer system.  If desired, MCIP collapses MMS profile data for the coarse vertical resolution 
data as defined by the user.  MCIP performs a mass-weighted averaging of data in the vertical 
direction.  For example, 30-layer MMS data may be averaged into 15 or 6 CTM layers.  During the 
collapsing procedure, the layer description is modified accordingly.  The resulting profile may have 
consistency problems.  This option is usually used to generate meteorology data for a system test(cid:173)
run for code debugging and development purposes.  It is also appropriate to study the effects of the 
vertical resolution in air quality simulation such as presented in Byun and Dennis ( 1995).  Refer to 
Table 12-5 as an example for the layer collapsing. 

Defining the vertical layering structure for the CMAQ system requires consideration of several 
factors.  Depending on the layer definitions used, the model results will be affected considerably. 
The implications of the layer definition are pervading across the entire system.  For example, to 
determine mass exchange between the boundary layer and free troposphere, a good resolution near 
the boundary layer top is preferable.  Also, different cloud parameterizations may perform 
differently, depending on the layering structure.  Aerodynamic resistance, which influences dry 
deposition velocities, is a function of layer thickness and the boundary layer stability.  For 
emissions processing, the layer thickness affects the plume rises from major stacks.  Also, the 
vertical extent of the area emission effects is limited by the thickness of the lowest model layer for 
the CCTM.  Although 6-layer vertical grid definition is provided with the tutorial simulation 
ex~ples, we do not recommend it to be used for regulatory applications because of the difficulties 
in simulating certain processes, such as dry deposition under stable atmospheric conditions. 

Current limitation on the number of vertical layers to 30 comes from one of the MCIP' s parameters 
defined in MCIPP ARM.EXT: 

PARAMETER  (  MAXK 

=  30 

!  MET  number  of  layers 

When a meteorological data has more number of layers, the parameter MAXK should be increased 
ac'cordlngly.  Collapsfr1g is done automatically when the COORD.EXT file (see Models-3 CMAQ 
User's Guide for the details) for the output grid has smaller number of layers than the input grid. 

'11111"''" 

'I' 

I 

'' 

'" 

'I, 

'I 

, 

"I" 

•I 

''l'I 

'I 

Table 12-5.  An example of layer collapsing from  15 to 6 er-layers.  Full and half er-levels and standard 
heights are provided to show relations between the two vertical grid systems. (adapted from Byun and 
D~nnis, 1995) 

Level 

Index(15) 

Level 

Index(6) 

er-Level 

Full (15)  Half (15) 

15 

14 

13 

12 

11 

10 

9 

8 

7 

6 

5 

4 

3 

2 

1 

0 

6 

5 

4 

3 

2 

1 

0 

......... 

0.0 

0.1 

0.2 

0.3 

0.4 

0.5 

0.6 

0.7 

0.78 

0.84 

0.89 

0.93 

0.96 

0.98 

0.99 

1. 0 

0.05 

0.15 

0.25 

0.35 

0.45 

0.55 

0.65 

0.74 

0.81 

0.865 

0.91 

0.945 

0.97 

0.985 

0.995 

·" 12-22 

Standard  Height  (ml 
Full (15)  Half(15) 
16069 

11998 

9512 

7621 

6073 

4754 

3600 

2570 

1818 

1289 

868 

544 

307 

152 

76 

0 

13712 

10649 

8513 

6813 

5390 

4159 

3071 

2187 

1550 

1077 

705 

425 

230 

114 

38 


12. 3  Estimation  of  Physical  Parameters 

MCIP's essential role is to provide consistent meteorological data for the CMAQ modeling system. 
However, the meteorological models used for air quality study may not provide important 
boundary layer parameters at all, or may predict those at very coarse temporal resolution, or may 
only compute a subset of the needed parameters.  In such a case, it becomes necessary to estimate 
remaining meteorological parameters using certain diagnostic methods.  MCIP allows either the 
direct pass through of the PBL parameters provided by MM5, or they can be computed from the 
mean profiles of temperature, humidity and momentum together with the surface landuse data.  In 
the following, we explain the diagnostic methods used in MCIP.  Note that the approaches 
introduced here may not be consistent with MM5 directly and therefore may produce somewhat 
different spatial distribution patterns for certain parameters.  Basically, the diagnostic routines treat 
meteorological model outputs as the pseudo radiosonde observations. 

When desired, MCIP estimates key parameters for cloud distributions based on Anthes-Kuo 
parameterization.  They include precipitation rate, cloud fraction, and cloud base and top heights. 
MCIP also provides estimated dry deposition velocities for various chemical species in the RADM 
and carbon bond 4 (CB-4) mechanisms. 

### 12.3.1 PBL  Parameters 

Depending on the user option, MCIP either passes through MM5 predicted surface and PBL 
parameters or estimates them us the MM5 profile data and detailed landuse information.  The 
algorithms used for the diagnostic computation of PBL parameters are provided below. 

#### 12.3.1.1 Surface  Flux  Related  Parameters 

We utilize a diagnostic method based on similarity theory to estimate the turbulence flux related 
parameters.  When the meteorological model uses very high vertical resolution and the thickness of 
lowest model layer is less than 20 m, we can utilize the surface similarity theory to determine 
turbulence parameters for both stable and unstable atmosphere. 

For the computation of the surface layer parameters, MCIP utilizes analytical solutions suggested 
by Byun (1990) to minimize the needs for numerical iterations in solving the flux-profile relations. 
The method has been reviewed and successfully applied in several surface layer studies (e.g., 
Hess,  1992; Lo,  1993 and  1995).  Also, a weather research model, the Advanced Regional 
Prediction System (ARPS) (Xue et al.,  1995) utilizes it in the description of the surface layer.  The 
algorithm is summarized below. 

The nondimensional surface layer profile functions for momentum (<Pm)  and potential temperature 
( </J1.) are defined as: 

(12-lOa) 

kz ae =,,.  (~) 
e.  ik .H 
'l'h  L 

• 

(12-lOb) 

1"' 

, 

"" 

,,1 

"' 

1111111'"1"' 

where  U  and e are the horizontal wind speed and potential temperature in the boundary layer, 
respectively,14  is the friction velocity, and  fJ.  is the temperature scale representing the surface heat 
···flux (covariance of potential temperature and wind fluctuations) divided by the friction velocity.  L 
is the Monin-Obukhov length defined as: 

"!'Ill 

"11'"1111 

, 

,,11111111,,, 

•"" 

I 

1,,

11 

1 

1,, 

, 

1 

, 

L= eoU: 
kg(). 

(12-11) 

The similarity functional forms proposed by Bu singer et al. ( 1971) are used in MCIP as follows: 
For moderately stable conditions ( 1;;:: z/L;;;:: 0) we have: 

where  Pr0  is the Prandtl.number for neutral stability and  /3m  and  f3h  are the coefficients determined 
th{ough field experiments.  Refer to Table 12-6 for the values of these coefficients used in MCIP. 
Fqr unstable c9nclitions  {z/L < 0 ), we have: 

(12-12a) 

(12-12b) 

where rm  and r h  are coefficients of the profile functions.  In addition, we added a function for the 
very stable condition ( zlL ;;::1  ) to extend the applicability of the surface layer similarity following 
Holtslag et al. ( 1990): 

(12-13a) 

(12-13b) 

To estimate surface turbulence fluxes with the layered data from a meteorological model, we utilize 
the integrated flux-profile relationships:  For ( z/L <1  ) we have profile functions represented as a 
mqdified logarithmic functions, when integrated from the height of the roughness length (z), 

( 12-14a) 

(12-14b) 

12-24 

z  z,,  ] 
U(z)=- ln--lJI  (--) 

u. [ 
k 

z 
z,, 

m  L' L 

EPA/600/R-991030 

(12-15a) 

(12-15b) 

and for strongly stable conditions, ( dL ~I ), we use direct integration of the profile functions: 

U(z) = u. [/3m ln3....+ z-z,,J 

k 

Z0 

L 

(12-16a) 

(12-16b) 

The  1JI  functions are given by (e.g., Paulson  1970), for moderately stable conditions (1~ dL ~ 0): 

and for unstable conditions: 

where  x=(l-ym ~)1'·,  x 0  =(I-rm 2)"•, and 

(12-17a) 

.  (12-17b) 

(12-18a) 

(12-18b) 

One important fact to note here is that although the similarity functions for stable conditions, 
Equations  l 2-12a-b and 12-14a-b, are continuous at dL = 1, their integrated profiles are not 
continuous. 

The flux profile relations described above are used to relate the Monin-Obukhov stability 
parameters with the readily computable bulk Richardson number  Rib: 

R

.  g(z-z0
e 
l  -""'----~ 
b -

)  AB 
u2 

0 

For moderately stable conditions (I> z/L;;::: 0 ), we have: 

1111111111
' 

(12-19) 

(12-20) 

and for strongly stable conditions, (z/L ;;:::J  ), we can derive a similar solution following the same 
procedure as described in Byun (1990) to give: 

(12-21) 

Given a positive bulk Richardson number, one should use both Equations 12-20 and 12-21  to 
compute t; and choose the value in the correct range between the moderately stable and strongly 
stable conditions. 

For unstable conditions, we use an efficient analytical approximation by Byun (1990), for 
(~ - P,,2) ~ 0, 

s ~(-z )1n(3-l-2.JQ; cos(eh)+-
1 
3ym 

z-z0 

z0 

3 

] 

(12-22) 

(12-23) 

~ =.!.[_1 +3I!!...s2]; 
rm  b 

9  r! 

=- --+-(--+3)s  ; 

P. 
b 

1  [ 
54 

2 
r3 
m 

9 
r 
m 

rh 
r 

m 

2] 

h 

e = arccos(__!J_J · and 

w' 
-yQi, 

b 

T,,  = ~ P,,2  - Q~ + P,, 

[ 

]

1/3 
. 

The above equations are used to estimate friction velocity, u., by estimating the bulk Richardson 
number using the layered meteorological data: 

Heat flux can be found by using the temperature scale estimated with: 

where 

(12-24) 

(12-25) 

(12-26) 

There have been several suggestions for the coefficients of the surface layer profile functions.  For 
example, Businger et al. ( 1971) suggested: 

/3m=4.7,  /3h=6.35,  Ym=l5.0,  yh=9.0,  Pr0 =0.74 

with the von Karman constant value of 0.35.  On the other hand, Hagstrom (1988) reanalyzed the 
experimental data used for the determination of the similarity functions and suggested use of: 

with the von Karman constant value of 0.40.  In the current version of MCIP, the latter set of 
coefficient values are used (refer to Table 12-6). 

Moisture flux is found by using similar method as the potential temperature: 

E=-pu.q., 

(12-27) 

where mixing ratio scale,  q., is determined with an equation similar to Equation 12-26 for given 
Liq= q1 - qs  (q1 and qs are the mixing ratios at the lowest model layer and at the surface, 
respectively). 

12-27 

",II' 

:1111111·.·111, 

11111111 

,,111111 

#### 12.3.1.2 Estimation  of  Surface  Fluxes  Using  PBL  Profile  Functions 

Occasionally meteorological models use the lowest model layer thicker than 40 m or so.  Under 
this condition, it becomes difficult to believe that the lowest layer always belongs to the smface 
layer all the fune, espedaliy for very stable conditions.  To avoid thisfiffiitation, MCIP estimates 
s~.tf ~ce h.eat~ ,~o~~n~.~ ,,~d m<:>i,sture flux tr~nsfer param~ters from~5 smface wind and 
temperature by using the boundary layer similarity profiles proposed by Byun (1991).  In this 
section, we descnbe the methods used in MCIP for estimating surface fluxes when the thickness of 
the lowest model layer is too thick to rely on the surface layer similarity.  The intention is to 
provide a diagnostic method that can estimate the surface fluxes even when the lowest model layer 
extends above the surface layer.  It is difficult to expect that a layer with 40 m thick, for example, 
continuously belongs to the surface layer though out a day. 

Table  12-6.  Parametric Constants Used to Describe the PBL in MCIP 
Parameter 
von Karman constant 
Coefficient in stable profile function for momentum 

Coefficient in stable profile function for heat (scalar) 

Coefficient in unstable profile function for momentum 

Coefficient in unstable profile function for heat (scalar) 

Prandtl number for neutral stability 

Critical Richardson Number 

Maximum bulk Richardson number 

Minimum bulk Richardson number 

Minimum magnitude of Monin-Obukhov length 
Scale height ratio for neutral stability 

Symbol 
K 
{3m 
{3h 
Ym 
rh 
Pr0 
Ricr 
max( Ri8 ) 
min( Ri8 ) 
abs(L) 
Ao 

Zilitinkevich ch 
Neutral value of similarity function for wind component parallel to surface stress 
Neutral value of similarity function for wind component normal to surface stress 
Reference height 
,,, 

,, 

ch 

A(O) 

B(O) 

Z, 

" 

Value 
0.4 
6.0 

8.21 

19.30 

11.60 

0.95 

0.25 

0.70 

-4.75 

4.0 
0.07 

0.80 
1.70 
4.50 
10 m 

The wind and temperature predicted by a meteorological grid model represent layer averaged 
values.  In order to simplify computation of the surface fluxes, we apply the assumption that the 
predicted wind for the lowest model layer has the same direction as the surface stress (i.e., 
u! + v! = U!,  where  um  and v m  are latitudinal and longitudinal components of the first layer mean 
wind on the map and Um  is the layer mean wind speed in the direction of surface stress).  Applying 
the PBL momentum profile functions of Byun (1991) and integrating them vertically  from Zo 
(roughness length) to the top of the lowest CTM layer ( zFI  ~ h ), one can obtain wind and potential 
temperature profiles in the form: 

(12-28) 

(12-29) 

where  T10  = h , TIF  =  ~ 1 
z 

z 

,  and  µ = L.  For a detailed description of the notation used, refer to 

h 

Byun ( 1991) and Byun and Dennis (1995).  Initially, the atmospheric stability (µ) is approximated 

by the analytical solutions of flux-profile relationships described earlier.  Then, we computeµ, u*, 
and(}*  using Equations  12-28 and 12-29, with the Newton-Raphson iteration.  Equation  12-29 is 
then used to estimate temperatures at heights  1.5 m and 10 min MCIP. 

#### 12.3.1.3 Utilization  of  Sub-grid  Scale  Landuse  Information 

Accurate description of atmospheric turbulence is one of the important elements in modeling the 
deposition of pollutants.  For Eulerian air quality models, grid-average surface roughness based on 
sub-grid scale landuse information has played an important role in the characterization of the 
surface condition, which in tum determines intensity of turbulence in the atmosphere.  To represent 
the atmospheric deposition process utilizing the available sub-grid landuse information, Walcek et 
al. (1986) introduced a method to estimate friction velocity for each landuse patch, which is used in 
the calculation of the subgrid-scale aerodynamic resistance.  Also, several estimation methods have 
been proposed for the effective roughness length for use in meteorological grid models.  Compared 
with the latter methods that estimate the representative grid values for the given sub-grid 
information, Walcek et al. ( 1986) emphasized the description of the sub-grid flux estimation by 
introducing a somewhat ad hoc, but useful assumption.  They assumed that the quantity  Uu.  is 
constant both for the cell averaged parameters and for the individual landuse patches, i.e.: 

(12-30) 

where the non-subscripted variables refer to grid-averaged values while the j-subscripted values 
refer to the corresponding quantities over individual landuse types.  Equation 12-30 is an intuitive 
expression of the often observed condition that where wind speed is high, the turbulence is low, 
and vice versa, under similar pressure gradient forcing.  Strict validity of Equation 12-30 could be 
controversial, however, this approach is more realistic than those that assume constant wind or 
constant friction velocity.  In the dimensional analysis point of view, the condition is a statement 
about the conservation of kinematic energy in the presence of surface friction.  A derivation leading 
to Equation 12-30 is provided below using a combination of the surface layer similarity and the 
mixing length theories. 

The momentum flux is related with mean wind gradient as: 

-(u"w")  =U:=K  -

s 

au 
m  (k 

(12-31) 


111

'" 

:1!!

:' 

,1.11111::: 

''Ill 

,',,l"I 

i' 

,,,lli::!!li.ii 

. 

:1111 

atjd the edpy diffusivity for momentum is given by: 

,1;,,, 

.. ,,,, 

,,:illl!'!I 

K  = 

ku.z 

m  </Jm(z/ L) 

(12-32) 

I ' 
:1;;:,,, 
I 

11111':'1 

' '  I , 
;;i111!!!!i1ill!. 

11 

~~en we app~~ ass~mptions such as steady state flow and horizontal ...  homogeneity of the each 
landuse patch, the momentum conservation equation can be simplified to give: 

::· 

11,1:',, 

" " 
·illj:'. 

' I ' 
·I!:::. 

1 

, 

" 

1111 I ~ ~ ! I 
•11,1,,, 

U-+ 

au  du"w" 
ik 
ik 

=--V p 

z 

1 
p 

(12-33) 

Coriolis forcing is neglected in deriving Equation 12-33 because we are dealing with only the sub(cid:173)
grid scale variation of momentum field. 

The right hand side of Equation 12-33 represents the pressure gradient forcing imposed over the 
grid and is therefore not dependent on sub-grid representation.  Within the surface layer, which is 
treated as a constant flux layer, Equation 12-33 is further simplified to give: 

au 
ik 

1 
p 

z 

U-=--V p=const. 

Combining Equations 12-31,  12-32 and 12-34, one obtains 

Uu.  </Jm(zl L) =canst. 

,kz 

(12-34) 

(12-35) 

For the atmosphere at neutral stability,  <Pm  = 1, and the same expression should be applicable for 
ea.ch landuse patch.  Then, Equation 12-35 becomes identical to Equation 12-30 at a given 
reference height z = Zr. 

In,,,,addition to J;quation  12-30, Walcek et al. (1986) assumed that the surface roughness length can 
be averaged as 

(12-36) 

where  ~ 's are fractions of different landuse types in a grid cell.  W alcek et al. ( 1986) stated that 
Equation  12-36 conforms to a logarithmic wind profile.  In reality, however, Equation  12-36 can 
be derived from a simple geometric averaging and it does not produce a logarithmic wind profile. 
On the other hand, Mason (1987) suggest an averaging method based on the logarithmic wind 
profile by introducing a blending height (lb  ) concept: 

In~  =  L 
( 

2  [ 
)

Z0 

~ 
ln(lb I Z0i )

]-1 

2 

(12-37) 

Although this approach is a practical averaging method of the roughness length, this by itself 
cannot be used to estimate landuse-dependent friction velocities.  Therefore, here we compare 
several other roughness length averaging schemes that satisfy both Equation  12-30 and the 
logarithmic wind profile function for the neutral condition: 

EP A/600/R-99/030 

and 

(12-38a) 

(12-38b) 

where  Zr  is the reference height.  The objective is to compute landuse specific friction velocities 
from the cell average wind and friction velocity values while providing a consistent averaging 
scheme for the roughness length.  The simplest method for estimating the grid average values from 
the sub-grid scale information is using the linear summation with the fractional weight: 

(12-39) 

where  xi is a physical quantity and LJJ = 1.  Because Up.
12-30 with Equation 12-39 as follows: 

=constant, one can rewrite Equation 
1 

Uu. = U .u.  = ""f,.U .u.  =< Uu. > 

, ,   £ . . i11 ,  

j 

(12-40) 

If the surface wind is a quantity that follows Equation 12-39, a relation between the grid average 
and sub-grid landuse dependent roughness lengths can be found as: 

(12-41) 

In order for the expression to be useful,  u.i  in Equation 12-41  should be eliminated.  From 
Equations 12-38a-b and  12-40, a simple relation for  u. i  is found in terms of known quantities: 

112 

U..  = u.[-ln--'(__..zr_l-"z0'-'--) ]
ln(zr I Znj) 

1 

(12-42) 

Substituting Equation 12-42 into Equation 12-41, we obtain a simple relation for the effective 
roughness length that satisfies both the logarithmic wind profile and the linearly-additive wind 
speed assumptions under the approximation, Equation  12-30: 


(12-43) 

,  but still within the surface layer.  In the 

'IJ:le referenc~,,,,height Zr  needs to be within the regime where the log-linear wind profile can be 
s~tisfied.  Thi,~"me~ns that it sQ.ould be far away from  z0
cilrrent MCIP~ we are using Zr=10m.  The new method conserves wind speed and  Uju.,  under the 
Ii.near su;mrn~!,ioq(Equation 12-39).  With this assumption the turbulence momentum flux (  u3  ) 
can be summed linearly when it is scaled with the factor  [ln(zr I z0 )/ln(zr I z0j) ].  Depending on the 
degree of inhomogeneity, this factor can be substantially different from unity, making less 
consistent with the expectation that fluxes from different patches can be summed up.  One may 
expect that Equation 12-43 is sensitive to the choice of reference height  Zr;  however, the average 
roughness length is not strongly sensitive to the reasonable value of zr  between  1 to  10 m. 

II" 

An alternative approach to the linearly-additive wind speed assumption is to assume that the sub(cid:173)
grid scale momentum flux can be summed linearly: 

u.  = £.JJi~i =< ~ > 
2 

.,..  2 

2 

" 

J 

Q'sing Equation  12-39 and the logarithmic wind profiles, one can find: 

1111::: 

11:: . . .  

[1nczr I Z0 ) ]2  - i 

. - L.r. ~) ---;;-
) r 
(U./U) 2 
i  [ ln(zr I z01

.... 
1 

11

" 

'1111

(12-44) 

(12-45) 

In order that Equation 12-45 to be useful,  ( Ui I U) should be expressed in terms of z0 ,  z0j  ,  and  Zr. 
This is accomplished by dividing Equation 12-38a with Equation  12-38b and substituting  (u. I u.j) 
with Equation 12-30 to get: 

~· 

'I 

, 

I:,, 

111, 

• 

'I· 

""" 

,, 

This leads to an averaging method for the sub-grid scale roughness length: 

(12-46) 

(12-47) 

I 

Another popular assumption is that at the reference height (or blending height) the drag coefficients 
representing the individual landuse patches in a cell can be summed linearly, i.e.: 

(12-48) 

12-32 

Unlike the two other assumptions introduced, this assumption does not depend on Equation  12-30. 
The resulting equation for average roughness length is then identical to Equation 12-37. 

EP AJ600/R-99/030 

z0  = Z, exp  -

[ ( 

~  f.. 
1 
,£.J 
2 
j  [ln(z,lz0j)] 

-112] 
J

(12-37') 

Although the assumption in Equation 12-48 has been used often in the literature, it is  not intuitive 
that the sub-grid drag coefficients can be added linearly.  The drag coefficient simply quantifies the 
turbulence exchange characteristic of a landuse patch.  By using this assumption together with the 
Equation 12-30, one can readily show that: 

(12-49) 

It is difficult to expect that U:  is a physical quantity that can be added linearly.  In MCIP, we have 
implemented both Equations 12-43 and  12-47 as a user option.  Results of the comparison of the 
two recommended methods should be available as the model evaluation project progresses. 

#### 12.3.1.4 Boundary  Layer  Heights 

Boundary layer height is a key parameter that determines the domain of atmospheric turbulence in 
which pollutants disperse.  It is used as a fundamental scaling parameter for the similarity theory in 
the description of atmospheric diffusion characteristics.  Estimating PBL height has been one of the 
key functions of meteorological pre-processors for air quality models.  Below, we summarize PBL 
height estimation algorithms used in MCIP. 

Unstable  conditions 

MCIP estimates the PBL height using the vertical profiles of potential temperature and the bulk 
Richardson number with an algorithm similar to the one reported in Holtslag et al. (1995).  The 
bulk Richardson number of each model layer with respect to the surface is given as: 

(12-50) 

where subscript H represents values at the layer middle (i.e., half sigma level). 

First, the index of the PBL top (kp8J is determined at the layer when  Ri8  first becomes larger than 
max( Ri8 ).  Then the proration factor of the bulk Richardson number relative to max( Ri8 )  is 
computed with: 

(12-51) 

where  max(Ri~)=0.7 (refer to Table 12-6).  Depending on the value of fR;s,  the index kPBL  and 
!Ri,  are modified as follows: 

'1111 

for  fR;, < 0.5, f~11  = fR;,  + 0.5; k~BL = kPBL  -1. 

Once the fraction and index for the PBL top are determined, we estimate the initial PBL height 
with: 

(12-52) 

The above proration procedure ensures gradual increase of PBL height.  Without the procedure, 
th.~ resolution of PBL height is limited by the layer thickness of the model.  In the RADM 
preprocessor, the PBL height is determined simply at the layer where the potential temperature first 
becomes warmer than the surface temperature for convective conditions.  Compared with the 
RADM method, the present method takes into account effects of the wind shear as well. 

,, 

Ill 

Stable  conditions 

For stable conditions, the PBL height is determined by the maximum of the PBL height computed 
with above method and the stable boundary layer height given by the Zilitinkevich's (1989) 
formula: 

(12-53) 

Limitine  PBL  heiehts 

Unlike the PBL height estimation algorithms based on temporal integration of surface heat flux 
(e.g., Carson, T973; Betts, 1973; Driedonks, 1981), the above diagnostic algorithm could predict 
temporally disconnected PBL heights when the hourly meteorological data change abruptly.  To 
~nimize this ~ff~t, following limits on the PBL height are imposed. 

1. 

Compare with PBL height for neutral conditions, and take maximum except for the tropical 
areas: 

(12-54) 

2. 

Compare with the urban boundary layer height, which is approximated with: 

(12-55) 

where  furban  is the fraction of urban landuse in a cell.  In MCIP, the minimum PBL height 
for urban area and other land use types are set to be  huBI.min =300 m and  hp8 u,,;n =50 m. 
Then, take a maximum of the two to reflect the effect of urban landuse: 

h' PBL = max{hPBL'  huBd 

(12-56) 

This step is introduced to apply the urban landuse classification, which is consistent with 
the one used for the emission processing, for the determination of the PBL height. 

3. 

Limit the PBL height with a maximum value (hPBulUU=3000 m) in case the temperature 
profile does not have a capping inversion: 

(12-57) 

12.3.1.5 

Aerodynamic  Resistances 

Aerodynamic resistance describes the ability of the atmospheric turbulence to transport pollutant to 
the surface for the deposition.  In this regard, it may well be described with the probability 
concept.  If 100 particles, say, are at height 20m, how many of them can reach the surface to be 
available for deposition during a given time interval?  Or, what is the transport rate of particles in 
the air to the surface?  The ratio represents the maximum potential deposition rate of particles 
subjected to atmospheric turbulence.  However, even those particles which arrive at the surface 
may not all be deposited because of other resistances, which usually are parameterized using· 
characteristics of surface and gaseous elements. 

There have been many efforts to compare different formulations for dry deposition velocities. 
While there are a lot of uncertainties in describing atmospheric processes, the different 
aerodynamic resistance formulas in the literature are mainly originated from the differences in the 
applications and approximations of the same PBL theories and formulas.  Among the components 
involved in atmospheric resistance computation, formulations for aerodynamic resistance have the 
least controversy.  To compute the aerodynamic resistance, the parameterization of the eddy 
diffusivity for the PBL should be known.  The eddy-diffusivity formulations used in the derivation 
of aerodynamic resistance are discussed below. 

Eddy  diffusivity  formulations 

The similarity theory suggests that eddy diffusivity in the surface layer for heat flux is given by: 

(12-58) 

The profile function  </>his  defined in Equations 12-12b and 12-13b.  Eddy diffusivity in the PBL 
(above the surface layer) (Brost and Wyngaard 1978) is given as: 

_  ku.z(l-z/hpp1.)312 

K 

h-

</Jh(zl L) 

for stable atmosphere ( z IL> 0 ), and as: 

Kh  = kw.z(l - - )  

z 
hPBL 

..... 

,, 

I 

,,11111111111, 

•'""' 

(12-59a) 

(12-59b) 

for unstable atmosphere ( z IL ::;; 0 ), where the convective velocity scale  w.  is defined as 

resistance  formulations 

(12-60) 

We often characterize the ability of turbulent atmosphere to carry pollutants to the vegetation or 
other surface elements using the aerodynamic resistance concept.  Certain meteorological models 
may use the resistance representation of surface exchange processes in lieu of the conventional 
bulk aerodynamic methods and the aerodynamic resistance is available as a part of meteorological 
datawith other PBL parameters.  Conversely, there are models that do not predict resistances and 
refated parameters needed.  In such cases, we need to estimate it using PBL parameters and 
profiles of state variables. 
The aer()dynamic resistance ( R0
)  and sub-layer resistance (Rb) are parameterized in terms of 
friction velocity and surface roughness (W alcek,  1987; Chang et al.,  1987 and  1990; Wesely, 
1989; Wesely and Hicks, 1977).  Although some consider the estimation of aerodynamic 
resistance as an integra.J. part of the dry deposition velocity computation, we treat the aerodynamic 
~~istance !15 !l:11  iJ!Qependent element that characterizes the effects of atmospheric turbulence.  Here, 
W,,~ provide a set of integrated equations, which allow robust estimation of aerodynamic resistance 
compared with the method based on the nonintegrated form, which sometimes provides negative 
fluxes for unstable conditions.  Refer to Byun and Dennis ( 1995) for details. 

"""""'"""""""" 

' '   """"""'" 

1111,,,1

1 

1
" 

"' 

1111

'' 

' 

' 

"' 

" 

" 

' 

' 

' 

,, 

""' 

" 

General formulation for the aerodynamic resistance at the deposition height  Zt1ep  (where 
concentration is represented) is divided into two components; the resistance in the surface layer 
(whose top is at  Zsi, height of the surface layer) and the resistance in the PBL above the surface 
layer: 

'"'"" 

"' 

'""' 

" 

' 

" 

" 

'"' 

,, 

11111111 
Ill' 

' 

'""''" 
"'"" 
z  dz 

zsL  dz 

z 
dtp  dz 

Ra = J K  (z) = J K  (z) + J K  (z) = RaSL + RaPBL 

z.z 

Zo 

z 

ZsLZ 

(12-61) 

111

1· 

,, 

1111"'11111111111111 

I: 

1111111' 

II 

111111! 

::: 

,11111" 

12-36 

(a) For stable conditions: 

R 

aPBL  -

_  Pr0 

[  2(/3+1)  _  2(/3+1) 
.JI - Tlsi 

ku.  ~1- Tldep 

+  n 

l  (-1 + ~l -T]dep )(1 + ~l -TJsi)] 
, 
(1 + ~1- Tldep  )(-1 +.JI - Tlsi) 

where  =  h -L '  Tldep  = - - an  Tlsi  = --. 

/3  hPBL 

/3 

d 

Zdep 
hPBL 

ZsL 
hPBL 

(b) For unstable conditions: 

R  __ 1_1 [Tldep (1-TJsi)] 
• 
aPBL  -
TJsi (1- TJdep) 

kw. 

n 

EP A/600/R-99/030 

(12-62a) 

(12-62b) 

(12-63a) 

(12-63b) 

### 12.3.2 Dry  Deposition  Velocities 

The term dry deposition represents a complex sequence of atmospheric phenomena resulting in the 
removal of pollutants from the atmosphere to the surface of the earth.  The rate of transfer of 
pollutants between the air and exposed surfaces is controlled by a wide range of chemical, 
physical, and biological factors, which vary in their relative importance according to the nature and 
state of the surface, characteristics of the pollutant, and the strength of turbulence in the 
atmosphere.  The complexity of the individual processes involved and the variety of possible 
interactions among them prohibits simple generalization of the process.  Nevertheless, a 
"deposition velocity," analogous to a gravitational falling speed is of considerable use.  In practice, 
the knowledge of dry deposition velocities enables fluxes to be estimated from airborne 
concentrations.  Two dry deposition estimation methods, that from Wesely (1989) currently as 
implemented in CMAQ, and the new Models-3/CMAQ approach are presented. 

#### 12.3.2.1 RADM  Method 

The RADM dry deposition module in MCIP calculates deposition velocities of sixteen chemical 
species (Table 12-7).  It requires various ancillary 2-D meteorology fields such as the PBL height, 
mixing scale velocities, Monin-Obukov length, etc.  They are usually estimated from horizontal 
wind components, temperature and humidity profiles.  The dry deposition flux of each chemical 
species from the atmosphere to surface is calculated in the CCTM by multiplying the concentration 
in the lowest model layer with the dry deposition velocity.  The dry deposition velocity ( Vd) is 
computed from the resistance-in-series method; 

where Rtf  is the aerodynamic resistance, Rb is the quasi-laminar boundary layer resistance, and Re 
is the  canopy (surface) resistance.  Refer to Figure 12-7.  Vd  is usually estimated from a series of 
resistances to vertical transfer and surface uptake.  Aerodynamic resistance (Ra  ) is a function of 
turbulent transfer in the atmospheric surface layer and can be estimated in several ways depending 
on the instrumentation used or parameterizations provided. 

lll111::r' 

'' 

,,1111111111,.'1', 

:'"·,,,;::1111' 

11' 

1;::111111 

(12-64) 

The canopy resistances (R) for S02 and 03 are estimated from available measurements as a 
function of season, insolation, surface wetness, and land type (W alcek et al.,  1986; Shieh et al., 
1979; Fowler,  1978).  The surface resistances for all other gaseous species, due to the lack of 
extensive measurements, are qualitatively scaled to the S02 and 03 surface resistances according to 
their reactivity and solubility.  The surface resistance for particulate sulfate is parameterized in 
terms of stability and friction velocity in the surface layer (Wesely,  1989), based on limited studies 
t11at do not inc'iUde water surfaces.  In CMAQ this method only applies to the treatment of surrogate 
gas-phase representation of sulfate species.  Size dependent deposition velocities for particle 
depositions are estimated inside the CCTM's aerosol module.  See Chapter 10 of this document 
and Binkwoski and Shankar (1995) for details. 

11r"~llll'  :!' 

"1111" 

• "'" 

II'  1111!! 

•I.,  " 

I" 

'  ' 

" 

, ,,,!, 

!111111111111111111 

11, 

" 

1111 

,!1

"

'  ,! 

' 

,,11111:. 

'II, 

I, 

"'' 

, 

I' 

'II' 

" 'I, 

'Ill'" 

,  ,• 

, 

"" 

I,, 

, 

,,II 

11111 

The laminar sub-layer resistance (Rb)  depends on the landuse specific friction velocity and 
molecular characteristics of gases.  Using the landuse dependent friction velocity or the cell average 
friction  velocity, R,,'s for heat (Rbh)  and trace gases (R,,) are estimated with the Schmidt number 
(Sc): 

(for heat) 

(for trace gas species) 

(12-65a) 

(12-65b) 

R  =-Sc 213 
bx 

x 

2 
k 
l4 

where Schmidt number is defined as the kinematic viscosity of air (v = 0.146  cm2s-1
molecular diffusivity (i.e.,  Sc= v I De  for heat and  Sc"= v I Dex  for trace gases).  Here,  v 
kinematic viscosity of air;  De  is molecular diffusivity of air (heat); and Dex  is molecular diffusivity 
of trace species.  For heat, the molecular thermal diffusivity ( D8
vapor, molecular diffusivity (Dew)  is 0.244  cm 2s-1
0.159  cm 2 s-1  (molecular diffusivities of other chemical species are included in the model). 

;  and, for ozone molecular diffusivity ( Dg03 )  is 

)  is 0.206  cm 2s- 1

)  divided by 

;  for water 

is 

,: 

111 


Ambient Concentration 

EP A/600/R-99/030 

.. ................... , 
• 
• 
• 
• 
• 
• 
RC: 
• 
• 
• 

,...... 
• 
• 
• 
• 
• 
• 
i 
• 
• 
• 
1-4=-

~ 

• 
• 
• 
• 
~-·--·-----·--~ 

Figure 12-7. Schematic Diagram of Pathway Resistances Used in RADM Dry Deposition Model 
Resistances with subscript x are for different chemical species. 

Re represents the canopy resistance defined as: 

R  = 
( 
c 

I 

+-1-+ 

1 

r sx  + r m.t 

'iux 

rdc  + r;,lx 

+ 

1 

r <lC  + rgsx 

-1 

J

(12-66) 

where  rsx  = stomatal resistance; 

r=  = mesophyl resistance; 
'iu:c=  resistance of the outer surface of leaves in the upper canopy; 
rdc  = resistance for the gas transfer affected by buoyant convection in canopy; 
r;,tx  = lower canopy resistance (uptake pathways at the leaves, twig, etc.); 
rac  = resistance that depends on the canopy height; and 
rcsx=  resistance of soil, leaf litter, and other ground materials. 

Stomatal resistance for water is obtained using following equation: 

rsw  =~min  +  O.l+G.nv 

1 

(  [ 

200 

]2J( 

400 

'.f,,(40-T,) 

J 

(12-67) 

where  r sw  = stomatal resistance for water; 

1:.min= minimum stomatal resistance for water, specified in Table 12-7; · 

7;  = surface air temperature in the canopy in Celsius temperature; and 
GS'IY  =-solar radiation reaching at the canopy in W/m2 unit. 

Then stomatal resistance for trace gas species is obtained with: 

r,g:  - r .sw 

-"'"  ~ 

D 

Dex: 

(12-68) 

w is the molecular diffusivity of water.  Ratio of molecular diffusivity of water to that of 

where D1
each trace species is provided in Table 12-7. 

The mesophyl resistance is parameterized as: 

1
' 

111'

1111 

r mx:  =  __..!.&_ + 100 fox 

H 

)-

( 
····•••··  3000 

1 

(12-69) 

where  H.x  is Henry's gas constant (in mole atm-1)for the species, and.fox:is the reaction intensity 
factor. 

,1:::11:::11" 

I 

,11111· 

'11, 

,, 

'

1 ',111111, 

Both the Henry's gas constants and reaction intensity factors are provided in Table 12-7 as well. 
r1vx  is related with the upper canopy resistance for water ( 'iu)  which is provided in Table 2 of 
\Vesely ( 1989) as: 

r.1c  is parameterized in terms of the available solar radiation and the slope of local terrain: 

rtlc  = 100[1+lOOO(GSIV+10r1J(l + 10008)-1 

(12-71) 

(12-70) 

where e is the slope of local terrain in radians.  t;;1x and  ~.u are estimated based on the respective 
resistance V~!Jesfor S02 and 03 as follows: 

' 

,1111111:.. 

'' 

11111111111'' 

"'I 

"( 
H. 
r.  = 
1 as '"c-rs 
clx 

f, 
x  + _.!!L 
T;;ro 

)-1 

(12-72a) 

(12-72b) 

where subsci,:ipt Sand 0  are for S02 and 0 3, respectively.  All of these values, and  rue  (the 
resistance that depends on the canopy height) are landuse dependent and listed in Table 2 of 
Wesely (1989). 

Table 12-7.  Gaseous Species Treated in RADM Dry Deposition Module and Their Properties 
Relevant to Estimating Resistance Components as Implemented in MCIP 
(Modified from Wesely,  1989.) 

EP A/600/R-99/030 

Gaseous  species 

Symbol 

Dew/ Dgx 

H.x (mole atm- 1
) 

Sulfur dioxide 
Sulfate 
Nitrogen dioxide 
Nitric oxide 
Ozone 
Nitric acid vapor 
Hydrogen peroxide 
Acetaldehyde 
Formaldehyde 
Methyl hydroperoxide 
Peroxyacetic acid 
Formic acid 
Ammonia 
Peroxyacetyl nitrate 
Nitrous acid 
Carbon monoxide 

S02 
S04 
N02 
NO 

03 

HN03 
H202 
ALD 
HCHO 

OP 

PAA 
ORA 
NH3 
PAN 
HONO 
co 

1.9 

1.6 
1.3 
1.6 
1.9 
1.4 
1.6 
1.3 
1.6 
2.0 
1.6 
1.0 
2.6 
1.6 
1.2 

l.OX105 

0.01 
0.002 
0.01 

1.ox1014 
I.OX105 

15.0 

6.0XI03 
240.0 
540.0 
4.0X106 
2.0X104 

3.6 

I.OX105 
0.001 

fnx 

0.0 

0.1 
0.0 
1.0 
0.0 
1.0 
0.0 
0.0 
0.1 
0.1 
0.0 
0.0 
0.1 
0.1 
0.0 

#### 12.3.2.2  Models-3/CMAQ  Dry  Deposition  Model 

The Models-3/CMAQ dry deposition (M3DDEP) module estimates dry deposition velocities 
according to the same electrical resistance analog represented by Equation  12-64.  M3DDEP uses 
common components with the new land-surface model that has recently been added to MM5. 
Specifically, the aerodynamic resistance and the canopy or bulk stomata} resistance are the same as 
those used in the modified MM5 (MM5PX) for computing evapotranspiration.  Since the land 
surface scheme includes soil moisture and has an indirect nudging scheme for improving soil 
moisture estimates, the resulting stomata! resistance estimates should be better than those achieved 
with a stand alone dry deposition model.  Pleim and Xiu ( 1995) give a description of an early 
prototype of the land surface model which is now coupled to MM5.  Pleim et al ( 1996) and Pleim 
et al. (1997) briefly describe the dry deposition model and some studies comparing model results 
to field measurements of surface fluxes and PBL heights.  The description which follows here is 
partially drawn from these sources. 

. 

When using the M3DDEP option in the CMAQ system the aerodynamic resistance, as well as the 
bulk stomata! resistance (~ibw; discussed below) is provided to MCIP from the MM5PX.  Note that 
these parameters are computed as part of the land surf ace model in the MM5PX and are not 
available from the standard MM5.  In MM5PX the aerodynamic resistance  Ra  is computed 
assuming similarity with heat flux such that: 

'I)  = pC (e  - e ) I H - R 

.. 'II 

p 

g 

I 

bh 

(12-73) 

where Rbh is the quasi-laminar resistance for heat (defined in Equation 12-65a), ec  and e1  are 
potentiafiemperafure of the ground surface and the air, respectively, in the lowest model layer, and 
His the sensible heat flux defined in Equation 12-25.  If the sensible heat flux is very small (as 
during transition periods) the surface layer theory for neutral conditions is used as follows: 

(12-74) 

~here. z1 is the height of model layer 1 and z0  is the roughness length.  The heat and momentum 
fluxes are denvedfrom flux-profile relationships as in the MM5 (see Grell et al.,  1995). The quasi-
1 .. &ID: .. nar bouqgary layer resistance accounts for diffusive transfer across a thin laminar layer 
adjacent to surfaces.  Because of the no-slip condition, turbulent eddies cannot penetrate to a 
surface.  Therefore, there exists a thin layer of non-turbulent air where molecular diffusion is the 
primary mechanism for transfer.  While this concept is not relevant for momentum, it is relevant 
for any quantity that directly interacts with the surface such as heat, moisture, and chemical 
deposition.  Therefore, for these quantities, the addition of a resistance based on molecular 
diffusion is necessary.  Deposition layer resistance varies by the transported quantity because of 
differences in molecular diffusivity, which is defined earlier in Equations 12-65a and b. 
The total surface ~esistance to dry deposition (Rs)  has several components including bulk stomatal 
resistance (rsrb),  dry cuticle resistance (rcu1), wet cuticle resistance (rcw),  ground resistance (rg),  and 
in-canopy aerodynamic resistance (r1c): 

,Ill" 

1111,,, 

'11111 

1 

:111' 

, 

• 

(12-75) 

Bulk stomatai res!Stance (rsrb),  vegetation fractional coverage ifv), leaf area index (LAI) and 
(i:actiomtl leafar~ wetness ifw) are all provided by MM5PX since the same parameters are used in 
the lancfsurface scheme.  Figure 12-8 shows a schematic representation of Equation 12-75.  Note 
that r Sib•  as output from MM5PX, is already a combination of stomatal resistance on a leaf area 
basis r..,,  mesophyl resistance rm,  and LAI as described below. A  key component of the land 
surf ace model in the MM5PX is the parameterization of the bulk stomata! resistance that is used to 
c9mpute evapotranspiration.  The bulk stomatal resistance for water vapor is read in to MCIP from 
MM5PX and adjusted for chemical dry deposition in the M3DDEP model by weighting with the 
J1ltiO of molecular diffusivities for water vapor and the chemical species: 

1111''' 

(12-76) 

Ambient Concentration 

Leaf 

~c 

ground, water, ground, water, 

snow 

snow 

Figure 12-8.  Schematic Diagram of Pathway Resistances Used in Models-3/CMAQ Dry 
Deposition Model. 

In general, the bulk stomata} resistance is related to leaf based stomatal resistance as: 

where Psis a shelter factor to account for shading in denser canopies.  For water vapor and many 
chemical species, such as 0 3  and S027  rm  is assumed to be zero, however, for many less soluble 


(12-77) 

'" 

1111111,, 

EP A/600/R-99/030 

1111111 

!1111!! 

species a non-zero value should be used.  The shelter factor is given by: Ps = 0.3 LAI+ 0.7 with a 
iwnimum of Ps = 1.0.  Leaf scale stomata! resistance, computed as: 

'11111111111111, 

'11111111~ 

•,,1111' 

• 

I 

, 

, 

, 

1 

1111

111 

•••• 

•• 

., 

r.  _ 
st  -

11111'"1111"11' 

r. 
.</mm 

. 

F,_(PAR)F;_(w2 )f;(RHs)~(T,,) 

(12-78) 

depends on four functions of environmental factors which influence stomata! function, and the 
raj11iip~,~ stqmcit2:!,,, resi~~!illce (,-~,min) Vlh!.ch ~epends on ve~etative s12ecies.  The minimum stomatal 
resistance is a bulk parameter which reflects the maximum conductance of a leaf per unit area under 
unstressed conditions (well watered, full  sunlight, and optimal temperature and humidity).  This 
parameter is specified in the model according to vegetation type. The key to the model's ability to 
simulate stoJl1atal conductance in real world conditions is the four environmel}tal stress functions, 
f;-4 in Equati~~ iZ-78 ...  This kind of stomata! model, with independent empirical stress functions, 
is often called a Jarvis-type model after Jarvis (1976).  Specifically, the land surface model in 
MM5PX is based on Noilhan and Plan ton ( 1989) (hereafter referred to as NP89) with many 
svpsequent modifications. The radiation stress function is: 

Iii,,,,; 

with 

Fi= 

l+f 

f  + rs1m1n I ~1max 

(12-79) 

f  = 0.55 

2Ra  where rs, max is maximum stomata! resistance which is an arbitrarily large 
,,~L 

""'' 

" 

' 

"" 

'" 

I 

,,, 

" 

,,111111'1'" 

number (5000 s/m), Ro is solar radiation at the surface and the 0.55 factor is an approximation for 
the photosynthetically active portion,  and RoL is a limit value of 30 W/m2 for forest and 100 W/m2 
for crops according to NP89.  The only difference from the F1 in NP89 is that the dependence on 
LAI has been removed since the effects of leaf shading within the canopy are now accounted for by 
the shelter factor in Equation 12-77.  Therefore, the F1 as defined here represents the effects of 
sunlight on an individual leaf rather than the integrated effect on a canopy. The functions of root 
zone soil moisture and air temperature (F2 and F4)  were modified to follow the form of logistic 
curves as suggested by A vissar eial. (1985).  Logistic curves are "S" -shaped and therefore good 
fo'r representing a smooth transition from one state to another.  Also, logistic curves can be defined 
With varying degrees of abruptness, from an almost linear transition to an almost threshold 
behavior, and can be altered while maintaining differentiability.  The function of root zone soil 
moisture (w2)  is: 

"""""'"""""""' 

11111111111111111111,, 

""'"" 

'''"' 

" 

,,. 

1

' 

' 

.1

F;  = 11(1+exP(-5.o(waJ +b._)]) 

where the available soil moisture fraction is: 

(12-80) 

wef = 

W2  -Ww11 

wfc  -wwlt 

and the half point of the function (where F2 = 0.5) is: 

where Wwtt is the wilting point and w1cis the field capacity.  All soil moisture values (w) are in 
volumetric fraction. In many previous models, including NP89, Jacquemin and Noilhan (1990), 
Wetzel and Chang (1988), Mihailocic et al. (1993), Sellers et al. (1986), and Avissar et al (1985), 
the function of air humidity (F3)  is expressed in terms of vapor pressure deficit between the inside 
of the leaf, assumed to be saturated at leaf temperature, and ambient air humidity (vpd = elTsJ-ea). 
However, recent advances in plant physiology research have led to a new generation of stomatal 
models based on leaf photosynthesis (Sellers et al.,  1997) in  which stomatal conductance (g., = 
1/rs,) is a linear function of relative humidity at the leaf surface (Collatz et al.,  1991): 

(12-81) 

where g:,  is the stomatal conductance at RHs = 1 and b is the minimum stomatal conductance at 
RHs = 0.  Clearly, it makes more sense that stomata react to the humidity at the surface of the leaf 
rather than the ambient air humidity at some height above the canopy. Although, a physical 
mechanism for this linear relationship to leaf surf ace RH has not been determined, experimental 
data shows it to be a very good fit (Ball et al.,  1987). Since leaf surface relative humidity is neither 
an easily measured nor a modeled quantity, it must be computed from other parameters.  According 
to the electrical analog, the humidity at the leaf surface (qt) is an intermediate potential between the 
ambient air humidity (qa)  and the leaf interior humidity, qs{T5 ) ,  where Ts is surface temperature and 
qs  is saturation humidity (see Figure 12-9 for the schematics of the derivation).  Assuming 
constant flux from the ambient air to the leaf interior, the relative humidity at the leaf surface (RHs 
= q/qlTs)) can be represented as: 

RH  =  qaga + q,.g,, 
(g_,,  + ga)q, 

s 

(12-82) 

where Ka  is the air conductance (Jl(Ra+Rbw))  and qs is short hand for qlTs).  If we assume that bin 
Equation 12-81  is small compared to gsr.  which will be true in all but the driest conditions, and g 'st 
is the result of Equation  12-78 without the effects of humidity ( g 'st  = F /rs1), then RHs is the 
solution of a quadratic equation which can be computed, once all the other components of Equation 
12-78 have been determined.  F3  is equal to RHs but with a minimum imposed at 0.25. 


= 1/ga 

Leaf Surface 
rn  Humidity 

Leaf Surface 

~t  1 /Qst 

.. 

' 

Figure 12-9.  Derivation of Leaf Surface Relative Humidity 

11111111111"' 

" 

•I, 

• 

"""' 

'•11'111 

•I, 

'•'II'  ""'"'II•• 

•''Ill 

'll•'lh,,I• 

'1111'"11 

11"""11111111111111111111" 

The fourth environmental stress function (F4  )  is related to ambient temperature.  Again, we deviate 
from the NP89 formulation which used a quadratic function peaking at the optimal temperature of 
298 K.  Instead, we followed the method of A vissar et al. ·c 1985) which results in a function  with a 
pfateau over a range of ''optimal temperatures; the idea being that temperature ( I;,) inhibits stomatal 
(yncfioq Qnly at extremes of heat or cold.  F 4  is defined as: 

1 

''"'' 

11• 

,!I 

'''11"11111 

11" 

1 

1 

'" 

,11111 

··•II!' 

1111111111

, 
1

1
11111·:.. 

1
11111 

1::.

11:,1 

·I·'' 

···· 

•.  F. .. ~.  ="'l''t(l +. exp[ti.r(T;, - br )]) 

· 

·  · 

(12-83) 

' ! 111111111 i i II 

, , 

: 

11 ' ' 

' ' 

I I i i ' ' 11 : 

' 1,, 

' ~ 

' ' 

' " " ' : " " ' : : : I 

I ., 

" 

I , 

: 

' , , 

' 

' ' " ' 

" 

"  ' 

' 

' ' 

where ar= -0.41  and br= 282.05 K for Ta:::;;  302.15 Kand ar=l.18 and br= 314 K for Ta> 
3'oi;~ lg·:K.  No'te'"ihat the hlgh side or"the function extends.into higher temperatures than suggested 
by A vi~'~ar ~1'"~.· ("'1985) ~ho used a b~ = 307. 95. The current function is very similar to the 
function used by Rochette et al. ( 1991) and is close to the high side of the NP89 F4  function. 
Resistan~es fo; alI ~f the no~-stomataJdry d~position patfi:~ays in the M3DdiiP module of MCIP 
are siffiilar to""'the"dri deposition modei'developed for the A.cid Deposition and Oxidant Model 

',, 

"' 

"' 

'" 

" 

' 

'I 

11 ! ~ : I ' 11 Ii " 

' 

, .... 

(ADOM) by Pleim et al. (1984) and later evaluated and modified by Padro et al. (1991).  As in 
ADOM, surface resistances to ground and leaf cuticles are scaled relative to the most well measured 
chemical species such as S02  and 0 3•  The term used for the scaling factor:  relative reactivity  has 
caused considerable confusion since many users try to find a chemical definition for this parameter. 
This factor has no chemical definition but is simply meant to be a relative scaling factor for the 
removal rate of different species at the ground or cuticle surfaces.  It is assumed that the relative 
propensity to deposit to different surfaces is similar, so that the same scaling factor can be used for 
ground and cuticles: 

~ 

rcut = rcuto A/A 

(12-84a) 

(1Z-84b) 

where rg0  and rcuro  are the ground and cuticle resistances, respectively, for the reference chemic&} 
species (usually S02  or 0 3 )  and A0  and A are the relative reactivities for the reference species and 
the modeled species, respectively. 

One improvement from the original ADOM model is the inclusion of an in-canopy aerodynamic 
resistance which is added in series to the ground resistance for the vegetated portion of the modeled 
area (fv)  according to Erissman et al. (1994): 

(12-85) 

where he is the height of the canopy.  For wet or partially wet canopies ifw), the surface resistance 
to wet cuticle is estimated as: 

(12-86) 

where KH  is the nondimensional Henry's law constant and a* is an aqueous dissociation factpf. 
The empirical factor (rcwo)  is set to 2.4x 108 s m· 1
,  which is similar to Wesely (1989).  For 0 3,  rcw 
is set to 1250 s m·1 based on field measurements.  The pH of the canopy water can be specified to 
compute a* for species such as S02  and NH3  which readily dissociate.  The canopy wetness 
fraction ifw) is provided by the surface model in MMSPX.  For deposition to open water, the 
ground resistance is replaced by a water surface resistance according to Slinn et al. (1978): 

(12-87) 

The parameters needed for the non-stomata! part of this model are quite uncertain for many 
chemical species.  Our latest estimation of these parameters for the species in the RADM2 chemic&] 
mechanism for which dry deposition is considered to be an important process are presented in 
Table 12-8.  The Henry's law coefficients in Table 12-8 are in non-dimensional form as the ratio of 
gas- to aqueous-phase. 

Molecular diffusivity and Henry's law have the least amount of uncertainty since these have 
definite chemical definitions and can be experimentally determined.  Relative reactivity, however, 

1 : " I ~ " : ' " 
i 1
l,,ll,1 
11"11 

' I  

" ' ' : 1111 " 

' " ' 

' 

' 

' 

is less well defined and is estimated according to experimental and modeling studies in the 
published literature .  The only organic species for which dry deposition is considered significant 
are the aldehydes, peroxides, and acids, all of which are soluble in water.  For these organic 
l~mped species the numbers for reactivity are merely educated guesses. 

Table 12-8.  Cheml.cal Dependent Parameters for M3DDEP 

,11111111'' 

,,'1111111111111111, 

·'"1"1111111 

" 

lh11lll111 

,, 

'l,,h 

1
'

1111111111 

,' 

111:::111,,, 

11111111 

'11111!11!11' 

Species 

111111":11111111 

biff 
(cm2/s) 

0.1509 
0.1656 
0.1594 
0.2402 
0.1628 
0.2626 
0.0938# 
0.1877# 
0.1877# 

• a 
(dimensionless) 
1000.0 
l.O 
10.0 
1.0 
1.0e9 
l.Oe5 
1.0 
l.O 
1.0 
1.0 
l.O 
1.0 
l.O 
1.0 
l.O 

sg2 
N02 
OJ 
Hz(}z 
HNOJ 
NHJ 
PAN 
NO 
HCHO 
ALD 
OP 
~aA 
ORA 
11111:· HONO 
co 
•  Updates based on literature review 
'Diff and H based on Wesely (1989) same as RADM 
+Estimated 
• gaseous phase  value/aqueous  phase value 

0.1525' 
0.1525# 
0.1220# 
0.1525# 
0.1525# 
0.2033# 

:11111:.1, 

11!1•'!1· 

I 

11111111111, 

1111111111, 

,I, 

Reactivity 
(dimensionless) 

8.00 
2.00® 
16@ 

301# 
800@ 

10.0 
4.0 
2.0+ 

10.0+ 
10.0+ 
10.0+ 
20.0+ 
20.0+ 
20.0+ 

5.0 

Henry's laws.  ( KH) 
(dimensionless) 
0.04 
3.5 
2.0 
4.e-7 
2.7e-7 
6.6e-4 

0.01 
23. 
4.E-6 
2.7e-3• 
l.7e-4# 

7.6e-5' 
1.0e-8• 
4. le-7• 

40. 

'fhe dissociation factor (a*) is simply defined as the ratio of the effective Henry's law coefficient 
to the actual Henry's law coefficient.  In this case, the effective coefficient is meant to i~clude both 
aqueous dissociation and aqueous reactions.  The a*s for S02  and NH3  are particularly important 
since their solubilities are greatiy effected by dissociation, which is a strong function of pH for 
both species.  Therefore, the model enables the user to specify the pH of the rainwater wetting the 
c~opy and let the model compute a* rather than use the default values in Table  12-7.  The a* for 
0 3 is also important since 0 3 is only slightly soluble so any enhancement can have a large effect. 
The value of 10 for 0 3  is meant to account for,  very approximately, aqueous reactions such as the 
oxidation of S(IV) to S(Vl).  Clearly, all of these parameters are quite uncertain and subject to 
r~y1s1on: 

"'llll,,,llll• 

''"· 

I  ' 

111111111:: 

" 

e 

' 

111

'

' 

' 

,,, 

:11111:;1;1:.,,,,, 

The main advantage of the M3DDEP model over the many similar models previously used in air 
quhlity modeling is the coupling to the land surface model for description of the stomata! pathway. 
This is a veryimportant t'eatiire for certain chemical species, particularly 0 3  and SO~, which have 

,,.,,1111!'!11111111111111 

'"""'"""""' 

" 

"' 

, 

, 

, 

· 

II• 

.. 

, ,, 

been shown experimentally to have a strong stomata! pathway components.  Several studies with 
this model have demonstrated its ability to realistically simulate both latent heat flux, with a large 
fraction from evapotranspiration, and ozone dry deposition, over com and soybeans (Pleim et al., 
1996; Pleim et al.,  1997).  These studies are continuing and are being extended to other chemical 
species (S02), and land-use types (deciduous and coniferous forests). 

### 12.3.3 Cloud  Parameters  and  Solar  Radiation 

Cloud information is developed and used in  many different ways throughout the Models-3 CMAQ 
system.  For example, MM5 includes parameterizations related with subgrid convective clouds, 
grid resolved cloud water and microphysics, and cloud effects on radiation.  The CCTM needs 
cloud information for photolysis, convective transport, and aqueous chemistry.  Therefore, MCIP 
has an impo1tant role in providing cloud information to CTMs by either propagating information 
from upstream processors (MM5) or by parameterization. 

MCIP has multiple cloud parameterizations and functions depending on the options selected. 
There are currently two main options related to MM5 runs, either the standard MM5 version 2.6 
which does not include cloud cover and radiation parameters in its output, and MM5PX which 
does.  Later versions of MM5 (2.7, 2.8, and 2.9) also output these additional parameters. 
However, when MM5v2.6 or earlier versions are used, it is necessary to compute these parameters 
in MCIP.  Incident solar radiation at the surface is an important output from MCIP since it is used 
to estimate biogenic emissions in the emissions processor. 

Another cloud function  in MCIP is to diagnose cloud infonnation (such as cloud top, cloud base, 
liquid water content, and cloud coverage) which is passed to the CCTM to adjust actinic fluxes for 
computation of photolysis rates.  This function is executed regardless of the choice of the MM5 
version used.  In addition, grid resolved cloud water and rain water are propagated through MCIP 
from the MM5 output to CMAQ for use in aqueous chemistry and for photolysis calculations at the 
4 km grid resolution. 

#### 12.3.3.1 Cloud  Coverage 

The fractional cloud coverage scheme currently used in MCIP is the same as used in MM5PX and 
similar to the scheme used in the standard MM5.  Cloud cover fraction ( fck)  above the boundary 
layer is computed at each vertical model level k , according to Geleyn et al.  ( 1982), as: 

where RHc is the critical relative humidity defined as a function of a  as: 

( 12-88) 

We have modified the Geleyn scheme to avoid the overprediction of clouds in well-mixed 
boundary layers. Within the convective boundary layer (CBL) when RH> RHc  the cloud cover is 
es!lmate by: 

~Ir. = 0.34 RHk - RHC 
1-RH 
c 

Jc 

(12-89) 

where the critical relative humidity (RHc)  within the CBL is set to be 0.98.  The factor 0.34 is from 
the suggestion that convective mixing induced clouds within the CBL should not exceed the 
fq1~tional are3:,,,9f tJ!.e updrafts at top of CBL, which large eddy simulations estimate to be about 
34% (Schumann,  1989; Wyngaard and Brost, 1984) when inactive clouds are disregarded.  The 
re5ulting layered cloud fractions, from Equations 12-88 and 12-89, are used for both functions; 
mimely, to be used lo the surface radiation calculation when these parameters are not read from 
~5, and to derive the cloud parameters needed for the photolysis calculations. 

11 

''II 

., 

For the surface radiation calculation, the layered cloud cover fractions are aggregated into the same 
three broad vertical cloud layers (low, middle, and high) as in MM5, assuming maximum overlap. 
&ch layer is defined by pressure such that the low layer is between 97 and 80 kPa, the middle 
layer is between 80 and 45 kPa and the high layer is above 45 kPa.  These three cloud layer 
fractions are then used in the radiation calculation described in this section (12.3.3.2). 

~e photolysis model requires cloud information in a different form.  For the sake of consistency, 
these parameters are estimated from the same layer resolved fractional coverage described above 
(Equations  12-88 and 12-89).  The photolysis model assumes a single uniform, vertically mixed 
cloud layer.  Therefore, the required parameters are; cloud top, cloud base, cloud fractional 
coverag~. an cf average liquid water content.  Cloud top and base layers ( k 10p  and  kbu•e)  are 
d~~ermined by looking up and down from level of maximum coverage to where the fractional 
cqverage first becomes 50% of the maximum.  The layer average cloud cover is then computed 
from the volume averaging of layer cloud fractions between  k10P  and  k,,use : 

•'lll•1111i"llll1••I 

""11111'1111'"'',1 

''lh 

11

1111::,1!

.1., 

111111 

I 

111111• 

I 

"1 

' 

• 

'I.,, 

l:,.. 

k,.. 

!/"' = Lfc"L1z~ILL1z~ 

(12-90) 

" -

k,_ 

Once the base, top, and fraction of the cloud layer are determined, the average liquid water content 
of the cloud is computed assuming convective characteristics as described by Walcek and Taylor 
(1986), Chang et al. (1987), and Chang et al. (1990).  The lifted condensation level is assumed to 
be at the cloud base, defined as the bottom boundary of cloud layer kbuse , where the air is saturated 
at the model's ambient temperature.  The in-cloud liquid water profile (q) is then computed as a 
fraction of the adiabatic liquid water profile (qad): 

(12-91) 

where a = 0. 7 exp[ (p - Pic1) I 8] + 0.2  and  Pic1  is the pressure (in KPa) at the lifting condensation 
level according to Warner (1970).  The layer liquid cloud water values are then vertically averaged 
in the same way as fractional coverage (Equation  12-90). 

#### 12.3.3.2 Computation  of  Solar  Radiation  Components 

To meet air quality modeling needs, MCIP outputs several parameters related with radiation at the 
surface including: incident surface shortwave radiation (Rcmd),  absorbed surface shortwave 
radiation (G.,.,),  net long wave radiation at the ground (G1w),  total net radiation at the ground (Rnez), 
and surface albedo (A).  These radiation components are used in several CMAQ processors.  For 
example, photosynthetically active radiation  (PAR), which is needed for the biogenic emissions 
processing, is estimated with  PAR= 0.55Rcmd.  Depending on which version of MM5 is used, 
surface radiation parameters are either passed through from MM5 or computed in MCIP.  The 
computation of surface radiation parameters in MCIP is identical to the surface radiation option in 
MM5, as described in Grell et al.  (1995), except that the dependence on zenith angle is added to the 
land use specified albedo (A1u)  such that: 

A= A1u + O.l[exp(0.00386Z312

) - t.O] 

(12-92) 

where Z is zenith angle in degrees. 

Note that MCIP computations of surface radiation parameters, which essentially replicate MM5, 
are a stop gap measure for use with MM5v2.6 which does not output these values.  All later 
versions of MM5 do output these parameters which will be read and passed through by MCIP as is 
now done for the MM5PX option.  It will be preferable in future applications of Models-3 to 
directly propagate the radiation parameters from MM5 to CMAQ so that the more sophisticated 
radiation models now available in MM5 can be used.  The currently used surface radiation option 
includes effects of clouds, aerosols, and water vapor on radiation but neglects radiation effects on 
the atmosphere.  Therefore, many future applications may utilize either the Dudhia radiation 
scheme (Dudhia,  1989) or the CCM2 scheme (Hack et al.,  1993) which include atmospheric 
radiation effects.  See Chapter 3, section 3.3.4.1, for description of MM5 radiation options. 

1 ~.4  l\fete~ro~ogical Data  for  CCTM  with  Generalized  Coordinate  System 

"' 

'" 

"'" 

'" 

' 

'

I 

1 

'Ii 

,, 

1111,,,1

"""' 

11111111,," 

""''""' 

""""'':. 

, ,   " "  

1
1111:::11111111111111111 

','""  ,,,,1

ci'ne feature of"''ihe C:rvfAQ system"t:hat is distinct from other Eulerian air quality modeling systems 
is its ability to incorporate meteorological models with various different coordinates and dynamics. 
This functionahty is achieved by recasting meteorological parameters in terms of the variables used 
in the governing set of equations for the fully compressible atmosphere in generalized coordinate 
system (Byun,  1999a).  Key dynamic and thermodynamic parameters are estimated for the given 
coordinates in such a way to ensure consistencies among the meteorological data.  The consistency 
ccm be maintajnec:!: µiroughout CCTM simulations when appropriate temporal interpolation methods 
'  d 
~'~use ·· 

### 12.4.1 Thermodynamic  Variables:  Pressure,  Density  and  Entropy 

To facilitate mass-consistent interpolation among the them1odynamic variables, the governing set 
of equations for the fully compressible atmosphere is used in the CMAQ system.  The system 
iq9Iu9es prognostic equations for entropy and air density as suggested by Ooyama (1990). The 
algorithms used for estimating the thermodynamic parameters are presented below.  For a detailed 
cliscussfon on the governing set of equations and mass-consistent interpolation algorithm, refer to 
Byun (1999a and b). 
1"'2. 4 .1.1 

Pressure,  Density  of  Air  and  Density  of  Water  Vapor 

lli'I' 
' Ii I : ' ' : : : , ' ' ' : II ~ ~ ! i '  h 

11111111 
' 111 ! I ' 

!,' .. 1:1'.l.:.'•'.,'':I 

I 

' , , " 

'''I 

,,,, 

I,'' 

' ' 

• 

• 

' 

' 

Like most of the other meteorological models, MM5 does not use a predictive equation for air 
d~nsity.  Instead, air density is  estimated with the equation of state using predicted pressure and 
temperature.  For a terrain-influenced pressure coordinate, when it is applied for hydrostatic 
atmosphere, the hydrostatic pressure ( p) can be computed with  µ•  available from MM5. 

(12-93) 

1J:ie total pressure (p) in the terrain-influenced reference pressure coordinate (applied for a 
nonhydrostatic atmosphere in MM5) is the sum of the reference pressure and perturbation pressure: 

Once p  is known, virtual temperature ( T., ) and density of the (moist) air are computed with: 

(12-94) 

(12-95) 

~here mw  and  md  are the molecular weights of water vapor and dry air, respectively , and 

,1111,.':; 
I 
~ 

;! 

p = _!!_. 
RdJ: 

'''I 
,,11 

1
' ,   " 

~ " 11 
''.111!"
, 11
~ 

1 

" 

(12-96) 

The water vapor partial pressure ( Pv) can be found from the vapor mixing ratio (r) supplied by 
MMS using the following equation: 

EPA/600/R-991030 

Then, the density of water vapor is simply given as: 

r 

Pv  =  l+rp. 

#### 12.4.1.2 Entropy 

(12-97) 

(12-98) 

In the CMAQ system, entropy is treated as one of the key thermodynamic parameters.  Because its 
conservation equation follows similar continuity equation for air density, entropy can be 
interpolated using the same interpolation scheme as air density.  Then, we can reconstruct 
temperature and pressure for the intermediate time steps from the interpolated densities (for air and 
water) and entropy.  This type of interpolation will maintain mass consistency among the 
thermodynamic variables for air quality applications.  Using the density of moist air and density of 
water vapor obtained above, the entropy for ~oist air can be computed with: 

(12-99) 

where  pd= p - Pv•  p00 =105  Pascal, A 00  is the specific entropy of saturated vapor,  pd 
density of reference dry air, and Pv•o  is the density of reference state water vapor saturated over 
water at reference temperature T,, 0
Ooyama (1990): 

,  273.15 K.  The integral constants are computed following 

is the 

00 

Poo 
Pdoo  =RT 
d  00 

A  = A(T  ) = 1) T  dlnE(i::) 

dT 
c 

Too 

oo 

oo 

.. "'  oo 

-

- E(i::) 
Pv•o  - Pv•(T,,J - - -
~T r. 

00 

(12-100) 

(12-101) 

(12-102). 

where  i:: = T-273.15. 

For the basic formulation of the saturation vapor pressure approximation, we use the AERK 
formula recommended by Alduchov and Eskridge ( 1996): 

12-53 

1, . .  :!' 

111
1

111111 

111111 

,,·:11111!111" 

EPA1600IR-99to3o 

,,,,111111.11 

,, 

111 

,,;"'[' 

11· 

:,','1'" 
'I, 

E(T,) = aebT,l<c+T,) 

111,.i' 

II 

dlnE(1~) _ 
-

dr:: 

be 

(c + r::) 2

' 

'II' 

I 

•111111111 

.11' 

:1; 

, 

'.':,jlllljll'. 

111

, ,

1iii11I! 

"', 

'11111111 

1:,"' 

(12-103a) 

(12-103b) 

where a=6 l0.94, h=l 7.625, c=243.04 and E(r::)  is in Pascal.  With this information we can 
evaluate the integration constants, and thus the entropy using Equation 12-99. 

### 12.4.2 Vertical  Jacobian  and  Layer  Height 

In  the CCTM, Jacobian is used for the definition of vertical and horizontal coordinates/grid system 
at ,svery model synchronization time step.  The Jacobian characterizes the coordinate transformation 
and is tre:ited as one of the fundamental parameters definin~ grid structure of the CTM.  Depending 
on'll'll'ifie  honzop~~ m~p projections and vertical coordinates, the physical characteristics of the 
JaGobian change. 

, 

'1111, 

#### 12.4.2.1 Jacobian  for  Coordinate  Transformation 

The vertical Jacobian defines the coordinate transformation rules.  For a vertical coordinate  £ 3  = ~ , 
where <;,  is a monotonically increasing function of height, the Jacobian is related with the 
geopotential height,  <P = gz, as: 

J  _ .!..  a<P  = _!. _a<P_ 
g  a;;, 
~ - g  a;;, 

(12-104) 

1111111, 

1111111• 

111111 

Si~ma- p.  time  dependent  coordinate 

11111 

11!~1"  " 

'I 

' 

"11'"1111' 

:1· 

' 

·::111· 

' 

' 

'  '!!I!:" 

FOr hydrostatic application of MMS, the vertical coordinate is the same as the terrain-influenced 
sigma- p coordinate, whose vertical Jacobian is defined as  .,, 

J  = f/(x,y,t) 
a,. 

-pg 

(12-105) 

N?te thatEquations  12-3 and 12-105 are identical although CMAQ uses monotonically increasing 
coordinate <;,  and MM5 uses monotonically decreasing sigma- p coordinate. 

U~,ing the Equation of State and the definition of the sigma- p coordinate, the vertical Jacobian can 
be obtamed usmg the temperature and surface pressure from MM5. 

II  ~II,, 

•' 

'" 

111111 

"' 

''" 

I' 

'I, 

I, 

' 

,, 

,1, .. 1 

'"'""' 

lh,lh 

Jacobian at half sigma-level is estimated for  1 ~ k ~K with: 

(12-106) 

12-54 

where K is the number of model layer (i.e., NLAYS in Models-3 I/O API), and Jacobian at full 
sigma-level is computed for  1::;; k $K-l with: 

EPN600/R-99/030 

k+112  R. p-·. (Tk+I + Tk  ) 

=  arr 

vH 
2g(a;+112p"+pr) 

vH 

J 
u,,  F 

)

( 

and, at the surface (k= 1/2) with: 

k=112 

J 
)
( 
u"  F 

= 

R. p-"  • T 
vs 

air 

g( a~=112 p*  +Pr)' 

(12-107) 

(12-108) 

where we have used  I;, ..  to represent the virtual air temperature at the surface. 

For the thirty layer MM5 sigma- p coordinate, whose sigma value for the top of first layer is set 
0.995 (i.e., about 38 min the standard atmosphere), the reference temperature is estimated from 
the average between the skin temperature ( ~ ) and air temperature at the lowest model layer.  The 
virtual air temperature at the surface is computed with the assumption that the relative humidity at 
the surface is the same as the relative humidity at the middle of first layer. 

Time  independent  coordinate 

For the nonhydrostatic application of MM5, we use the ten-ain-following sigma- p 0  coordinate 
whose vertical Jacobian is defined as: 

(12-109) 

Again, Equations 12-6 and 12-109 are identical although CMAQ uses monotonically increasing 
coordinate; and MM5 uses monotonically decreasing coordinate.  Using the Equation of State for 
the reference hydrostatic atmosphere and the definition of the sigma- p 0  coordinate, the vertical 
Jacobian can be obtained using the temperature and surface pressure from MM5.  Jacobian at half 
a-height is estimated for  1::;; k $K with: 

k 
)  = 

(J 

CTPo  H 

] 
R. p* •  T  +Aln(  HPo  Pr) 
air  o 

ak  • + 

os 

[ 

g(a~p= +Pr) 

Poo 

and Jacobian at full  a-height is computed for  0::;; k::;; K  with: 

R. p 0  •[T  +Aln(a~+ 112p; +Pr)] 

air  o 

os 

Poo 
g(a;+112p; + Por) 

)k+112  = 

J 
ur.  F 

(

12-55 

(12-110) 

(12-111) 

"'' 

EP A/600/R-99/030 

' 

'"""" 

The Jacobian does not depend on :MMS data except for the topographic height and sigma-layer 
d~flnitio'n. ·  " 

· 

' 

Total  .Jacobian 

1 

)  is used in the governing equations for the CCTM.  The trace 

The total Jacobian (.Jr== J~ I m 2
species concentrations are coupled with the total Jacobian.  Because of the need to couple with both 
•  ~~1  traC~ ~pecies concentrations (defined at the layer middle) and vertical flux variables (defined at 
~;·1~ye~'1foterfiice); total Jacobians at the half and full level values are stored in the MET_CR0_3D 
file.  The total .. fac~?i~ at the surface, (lap ):=112 
divided by the map scale factor squared, is stored 
in the :MET_CR0_2D file.  For hydrostatic applications of MM5 the Jacobian varies with time and 
fQX: nonhydrostatic applications it is constant with time.  Although we could have saved some file 
space by distinguishing this feature, the Jacobians are stored in MET_CR0_3D and 
.MET_CR0_2D files to maintain the compatibility of data structure for both time-dependent and 
time-independent coordinate systems. 

"'1,11"" 

"'1111'1''11' 

I 

1'' 1[11' 

For use in the mass-conserving temporal interpolation, Jacobian weighted densities at the half(cid:173)
levels defined ''.for' fs; k s;K  as: 

" 

1
11111111" 

(12-112a) 

(12-112b) 

are stored in tlie MET_CR0_3D file.  Like density, the Jacobian weighted entropy at the layer 
middle is obtained for 1 s; k -5.K with: 

111111111' 

'llllli 

''"'I 

(12-113) 

They are stored in the MET_CR0_3D file as the Jacobian-weighted total air density and water 
vapor density, and entropy, respectively. 

#### 12.4.2.2 Layer  Heights 

~ .. CMAQ, la~:r ~~ights are computed using the basic definition of the geopotential height in terms 
0~11,~aco~i3:11 ~~11~1~~~2 of re~xing .on the coordinate-specific analytic equations, such as a hypsometric 
equation for a hydrostatic coordinate.  The height above mean sea level (MSL) is defined in terms 
of the vertical 1 acobian as: 

' 

! 

111· 

Ii 

"I 

,1111:. 
"';II 

1111. 

111 

....  ':111

I' 

'l .. illi 

Z = cf>/ g = Zs + 

., 
'I 

' 

' "  

•1,·1111:' 
Ill 
111 
::.illl,,:: 

... 

'' 

,, 

' 

J  d~ 

I~ 
~.  ~ 
''  :: 

' 

. 

' 

The layer heights at the interface and middle of the layers above the ground level (AGL) are 
cgmputed with: 

12-56 

(12-114) 

for  1 s; k S:K+l  where 

' 
= 0 

zk+112I 

F 

k=O 

and for 2s; k S:K with: 

ZH-z  Z-

,..k  -

k  -
. 

,.. 
s 

~. 

- f ~~ J d~' 

" 
.. 

where 

EP A/600/R-991030 

(12-115a) 

(12-115b) 

. (12-116a) 

(12-116b) 

Note that bothz~+ 1 ' 2  and  z~ are stored either in the GRID_CR0_3D file for the time independent 
vertical coordinate, or in the file MET_CR0_3D for time dependent one.  Because  z~+ 1 ' 2 lk=O is 
always zero for the terrain-influenced coordinates, it is not stored in any of MCIP files. 

### 12.4.3 Contravariant  Velocity  Components 

The generalized CCTM requires a set of contravariant velocity components to simulate numerical 
advection.  Contravariant velocity components are scaled components of the wind vectors for the 
transformed coordinates.  When the true velocity components (i.e., wind components for earth(cid:173)
tangential coordinates) are predicted in a meteorological model, they need to be transformed 
accordingly. 

#### 12.4.3.1 Horizontal  Wind  Components 

MM5 predicts horizontal wind velocity components defined on the spherical earth in the coupled 
form with the surface pressure scale  p • .  First, we need to de-couple the surface pressure scale as 
follows: 

vt+112,m+li2  = (p ·v)/+112,m+l/2 l(p. )/+112,m+112 

(12-117a) 

(12-117b) 

for OS l SL and OS m <5:.M,  where Lis the number of columns and Mis the number of rows for the 
rectangular modeling domain.  In the expression,  p •  represents either  p • or  p;  depending on the 
hydrostatic/nonhydrostatic option in MM5.  The value  p 0  at the dot-point is approximated with: 

(12-118) 

The next step i~ to compute the contravariant components  u~ = ~U and  v~ = mV at 1the dot-point: 

I 

,,111111,1i 

11111!: 

(u~)1+112,m+112 = ni1+112,m+112u1+112,m+112 

:II' 

(v~)1+112,m+112 = m/+112,m+112yl+112,m+112 

(12-119a) 

(12-119b) 

Because the Jacobian-mass weighted contravariant wind components are needed for the mass 
consistent interpolation, we estimate these quantities at the square-point (flux point) for x1 
direction and at the triangle-point (flux point) for  .X2 -direction, respectively: 

-

(12-120a) 

(12-120b) 

Both of these parameters, together with the contravariant wind components  u~, and  v~ at the DOT 
points, are stored in the MET_DOT_3D file. 

,:':I''" 

' I l l  

"I 

'" 

.I" 

I. 

I" 

I 

" 

#### 12.4.3.2 Vertical  Wind  Component 

In most meteorological models with terrain-influenced coordinates, the contravariant vertical 
v~Iocity components are not computed directly because of the complex representation of the 
di:yergence term in the generalized coordinate system.  In the generalized coordinate system, the 
prognostic equation for the vertical component contains Christoffel-symbols-of-the-second-type 
terms that make the equation not only complicated but also make conservation of momentum or 
kinetic energy difficult (Byun,  1999a).  To circumvent this difficulty, some meteorological models 
use the prognostic equations of motion for the true vertical wind component. 

I 

''1

11111111 

Ill 

, 

1' 

1 

!lllllll 

llp'' 

'Ill:,,,,,,,,,. 

For the MM5 hydrostatic application, the terrain-influenced hydrostatic pressure coordinate allows 
diagnostic computation of the contra variant vertical velocity.  The continuity equation for air 
dcwsity becomes a diagnostic equation for this application.  ·Here, the expressions are written with 
the normalized, vertical coordinate, which increases with geometric height,  ~ = 1- <J';.  By 
se~ecting the i~tegration limits as  ~ = 0 to  1, (i.e.,  <J' f>s  = 1 to  <J' f>T  = 0) and using the boundary 
cqnditions at tq~ tO.IJ  and bottom  ~ = v3  = 0 , the tendency term can be computed with: 

.. ,.... 
,, 

'"··""' 
:11111lli,1I 

',1: 1li1l1:::!!! 

I 
I, 

I 

,,,'"11111, 

1111111 

, 

1
:"' 

" 

,,, 

,, 

'II 

'" 

'" 

I 

I" 

'" 

1, 

I 

1
'

1,'I• 

• 

(12-121) 

This equation predicts time rate change of the hydrostatic pressure component whose vertical 
gradient is in balance with gravitational acceleration.  In the geometric height coordinate, this 
equation does not exist.  However, the height coordinate does not require surf ace pressure 
tendency to close the system because the coordinate is time independent.  Because the hydrostatic 
pressure coordinate  i; = 1 - c;;;  is a material coordinate, the mass continuity equation can also be 
used to estimate contravariant vertical velocity component by integrating the wind divergence term 
either from the bottom to a level i; or from the top to i;. 

(12-122a) 

(12-122b) 

where  i;r  =  1. 

For other coordinates that do not have diagnostic relations, the contravariant vertical velocity 
component should be estimated using standard coordinate transformation 

~ = v3  = ~ +(-mV~ • V~hr +w{~) 

(12-123) 

where hr is the height of the coordinate surface and w is the regular vertical component of wind. 
For nonhydrostatic MM5 applications,  i; = 1 - a Po  is the corresponding vertical coordinate in 
CMAQ.  Equation 12-122a can be rewritten as: 
a.  • 
j; = -d"  = [-1!!!..._vp_,, ](mU) + [-1!!!... 
'Po  ](mV) + Pog W 
. ax2 
• 
'::> 
Po 
Po 

(12-124) 

Po 

•  a~i 

tx 

po 

Ci 

;),..,* 

Ci 

Like the horizontal momentum components, we need the Jacobian-mass weighted contravariant 
vertical wind for the mass consistent interpolation. The quantity is computed with the thickness(cid:173)
weighted density as: 

(12-125) 

where .f/+112  = cz;+l/2 - z~) /(z~•I - z~).  Note that  (pl; ~) is defined at the layer interfaces.  It is 
zero at the earth's surface (no-penetration boundary condition) and therefore not included in the 

m 

12-59 

''1 
,1111 

EP A/600/R-99/030 

i,11!,,, 
1:11 

output.  It also vanishes at the top of the modeled atmosphere.  The quantity  ( pJf ~) is stored in 
theMET_CR0_3D file from k=l to K (i.e., NIAYS) with the variable name (WHAT_JD). 
. 

I  m 

,,,,1''l·11illl111ll:' 

lllllllllrh 

''''""11111111 

1111111111: 

11" 

"' 

'"·' 

'"',,, 

,.," 

"'' 

I 

·II 

', 

,, 

,, 

"" 

' 

I 

""" 

I, 

llll'"lll' 

""II 

11

' ' 1lli11illlll"lllh• 

I 

II 

" 

,' 

I 

1' 

### 12.4.4 Parameters 

Mass  Consistent  Temporal  Interpolation  of  Meteorological 

In the CCTM, the temporal interpolations of meteorological data are needed beeause the output 
frequency of the meteorological data is usually much coarser than the synchronization time step of 
the chemistry-transport model.  Consistency in meteorological parameters such as, Jacobian, 
density of air (total), density of water vapor, entropy (or temperature), and pressure are important 
for the science modules in the CCTM. 

Here, we recommend interpolation schemes for density and velocity fields that maintain mass 
consistency.  First, the Jacobian and density at a time  ta= (1- a)tn + atn+t  between the two 
c~,!:!-secutive o~tput time steps,  tn  and  tn+t , are expected to be interpolated with: 

..  (plt;)a = (1-a)(plt;)n + a(plt)n+t 

::· 

1
1111 

'

(12-126) 

(12-127) 

where  0 ~ a ~ i . The Models-3 I/O API provides a convenient function call (INTERP3) for the 
linear interpolation of grid variables. The premise used here is that the Jacobian is a fundamental 
qu'antit}t that 'detemliries""tlle coordinate system.  Den'sity ofwater vapor (pv) and entropy should 
follow the same interpolation rule (Equation 12-127).  Then densities and entropy at  tn  are 
cqµiputed with 

It is obvious that the functional form of the Jacobian (which depends on a vertical coordinate) 
changes the characteristic of density interpolation.  For nonhydrostatic MM5 runs, where the 
Jacobian is constant with time, the density is simply interpolated linearly with time. 

(12-128) 

1
'' 

11 

When the den~ .. !ty tendency term is available from the meteorological model, or can be computed 
from diagnostic relations for certain choice of vertical coordinate system, one can use a piecewise 
cu.bic spline interpolation method for  (pls) : 

111'"1'"""' 

..  •, 

11111,11111 

I• 

,' 

11111111111, 

I 

,II' 

11: 

"' 

' 

(pJ:s)a  "= aa3 + ba2 +ca+ d 

(12-129) 

12-60 

:::1111: 

"lliillk 

I" 

' 

b = 3[(pJJn+I -(pJs)n]-2L1{ d(~s) )n  - L1{ d(~s) l+l 
c=L1{ d(~..)l 

EP AJ600/R-99J030 

The density of water vapor ( Pv) and entropy should be interpolated with the same method.  The 
density of dry air from the total air density can be obtained by subtracting Pv  from p.  The 
interpolated temperature is computed with: 

(12-130) 

and entropy can be computed with Equation 12-99.  The interpolated pressure is computed with the 
ideal gas law, 

(12-131) 

Next, wind components multiplied with the Jacobian-weighted density are interpolated linearly and 
the interpolated wind components are obtained by dividing the results with  (pJ .. )a: 

(12-132a) 

(12-132b) 

(12-133a) 

(12-133b) 

When density and wind data are not collocated, the above procedure may incur spatial interpolation 
errors. 

### 12.4.S Meteorological  Data  for  MM5

Optional  Conversion  of  Nonhydrostatic  Data  to  Hydrostatic 

There may be a special situation when a hydrostatic CTM run is desired while the MM5 run is 
made with the nonhydrostatic option.  In such a case one may want to redefine the nonhydrostatic 
coordinate into hydrostatic one while keeping the profile data as provided.  In spite of many 
theoretical problems with this kind of data conversion, MCIP provides an option to convert the 
nonhydrostatic coordinate into the hydrostatic pressure coordinate.  Use of this option is not 
r~omm~nrj~ fo~ ~ci~ntifi!?ally rigorous computations.  However, this is a useful option for certain 
purposes such as code development and test.mg of a hydrostatic CTM. 

' , , , ,  

11111,111111111111111, 

'"""""""" 

,111111111111,11, 

"'"'' 

' 

• 

' 

' 

' 

,., 

• 

' 

' 

,,,,,,,,1111 

"" 

Nonhydrostatic and hydrostatic coordinates can be compared in terms of the atmospheric pressure. 
Pressure in a hydrostatic MMS run is defined in Equation 12-93 while the same in a hydrostatic 
MMS is defined in Equation 12-94.  Then, the difference in the nonhydrostatic and hydrostatic 
pressures is: 

p· = p;-a-1(p- p)+a-•pP 

111" 

(12-134) 

If we force the hydrostatic pressure to be that of nonhydrostatic coordinate, i.e.,  p  z  p at the 
surf ace, the time dependent surf ace pressure,  p; , can be estimated with: 

(12-134') 

where  a~ 2  =as= 1 is used.  Here, the perturbation pressure at the surface is assumed to be that of 
layer 1 hecau'se the real slliface perturbation pressure is not provided by the nonhydrostatic MM5. 
Q!}e can r~Ci~[f,pe,,,~be verti~!tl COO~Hin~te to be hydrostatiC with the time dependent surface pressure. 
'!]:le difficlllty of converting nonhydrostatic meteorology data into hydrostatic data is not in the 
e~~imation of pressure surfaces, but rather in the representativeness of profile data because of the 
change in the layer definitions. 

11111111111111 

' 

"""'" 

""''"" 

'" 

,111:''lli1 

,111111 
111:1" 

llh 

1111 

1111111 

11111111111 

12-62 

! I i 1111 ~ : : 

11111, 

111111, 

## 12.5  Operation  of MCIP 

Because MCIP is a Models-3 conformant processor, it needs to be compiled and executed using a 
Models-3 build command (m3bld) with a configuration file.  It requires the grid-domain object 
include files to define output domain and resolution.  In most cases, the user does not need to 
define input data domains and resolutions because reader modules can extract the information from 
file headers.  MCIP code structure, compilation and execution procedures are discussed below. 
Additional operational information for preparing MCIP through Models-3 system framework can 
be found in Chapter 7 of the EPA Third-Generation Air Quality Modeling System User Manual. 
Also, the User Manual Tutorial for the initial public release of Models-3 provides step-:by-step 
instructions for running MCIP with a set of sample examples. 

### 12.5.1 MCIP  Modules 

MCIP code is archived with CVS (Concurrent Version System) (Cederqvist,  1993) in the Models-
3/CMAQ system.  Currently, eleven module classes are defined for MCIP.  The classification 
makes it convenient to customize MCIP code for special situations.  For example, when different 
meteorological inputs and process options are needed to link with the CCTM, appropriate modules 
from different classes can be used.  Refer to Table 12-9 for the details of module descriptions and 
associated source code. 

### 12.5.2 Building  MCIP 

The MCIP code conforms to the Models-3 coding standard.  It is designed to be compiled using 
m3bld with a configuration file.  Refer to Appendix  12C for a sample MCIP configuration file. 
The user needs to retrieve the source code (main program, associated subroutines for selected 
modules, and include files) appropriate to the user's choice of optional modules through the CVS 
system.  There are four kinds of include files for this processor.  Three include file types, Models-
3 1/0 AP! include files, MCIP' s global include files, and MCIP module specific include files, are 
fixed.  The fourth include file type describes dimensional information of the input meteorology data 
~nd, as an exception, must be edited to match the number of vertical layers in the input. 

The parameter MAXK in MCIPP ARM.EXT may need to be changed large enough to accomodate 
the dimensions of the number of layers for the meteorological data.  Horizontal domain information 
is obtained from the header of MMS files automatically.  MCIP expects that include files for the 
CTM grid/domain domain (HORD.EXT, VGRD.EXT, COORD.EXT), for which the data will be 
extracted, are provide by the user.  Vertical collapsing is done automatically when the number of 
layers in VGRD.EXT is less than the number of layers in input data and the coordinate interface 
values match as shown in Table 12-5.  When a few lowest input model layers are collapsed into 
one for the output, only diagnostic option can be used because the surface flux  values and 
aerodynamic resistance passed through are no longer valid due to the change in the thickness of the 
lowest layer.  The M3DDEP option must not be used when collapsing the lowest model layers and 
doing so will be violating the parameteric values passed through from the meteorological model. 

Table 12-9.  MCIP Module Definitions and Associated Source Code 
Class 
driver 
input 

Description 
controls main computational flow 
reads in MM5 output files 

Module 
mcip 
mrn5 

,111::11 

11111,,, 

,, 

'", 

.:1•.,1, 

1111"''111·111, 

fakemet 

rams 
m3radm 
pblpkg 

rOOmcld 

1
:111111111111111 

·11111:

,, 

'I',  ':1!11'" 

cmaqdd 
radinkiio 

land use 
phi 

drydep 

111111111111 

cloud 

s~Jar 

solnr_px 

···.n'et3d 

m3sup 

11111111111· 

output 

util 

,":.11111,' 

stnd 

util 

reads in MCIP output files 
generates meteorology fields for idealized flow  field 
stud 
reads in RAMS J/O API output files 
reads in landuse data in RADM dry deposition category 
computes PBL parameters 

computes dry deposition velocities using Wescly's 
RADMmethod 
computes dry deposition velocities using Pieim's 
CMAQmethod 
computes cloud parameters using convective column 
assumption 
computes solar radiation using the algorithm 
implemented in MM5-PX version 
computes supplemental three-dimensional variables 
needed for CMAQ 

generates MCIP output for standard variable lists 

collection of utility subroutines 

icl 

1111111:::1, 

ic{ 

MCIP specific include files 

11 

11'"1

',,i,1'' 

12-64 

Source  Code 
riicip.F,  initx.F 
getmet_mm5.F, 
rea:dmml.F, 
readmm2.F,  getgist.F, 
MM5INPUT.EXT, 
MM5HEADER.EXT 
getmet  m3.F 
getmet_m3fake.F 

not available 
getluse.F 
pblpkg.F,  pblpwr.F, 
sfcflux.F,  slflux.F 
radmdry.F, 
DDEPSPC.EXT 
not available 

bcldprc_ak.F 

solar.F,  transm.F 

layht.F,  verthat.F, 
vertnhy.F,  verthyd.F, 
jacobi.F,  met3dsup.F 
comheader.F, gridout.F, 
metcro.F, metdot.F 
bilin2d.F,  bilin3d.F, 
cvbdx.F, collapx.F, 
cvmgp.F,  cvmgz.F, 
ratint.F,  errmsg.F, 
sanit  .F 
CONST_mete.EXT, 
C9NST_pbl.EXT, 
FILES3_MCIP.EXT, 
GROUTCOM.EXT, 
INFILES_MCIP.EXT, 
LlµDMDAT.EXT, 
MCIPCOM.EXT, 
MCIPPARM'.EXT, 
MCOUTCOM.EXT, 
MDciUTCOM.EXT 

Executing  MCIP 

#### 12.5.3.1 Run  Script  Command  File 

As with other interfaces, an execution run script is used to define key environmental variables 
prescribing run-time characteristics and linkage between logical file names specified in the codes 
and actual files for input and output.  A sample script for MCIP execution can be retrieved from the 
CVS archive and edited for the user's particular application.  The user may modify environmental 
variables for choosing different processing options and for defining input and output files.  Refer 
to the APPENDIX D for a sample MCIP run script.  For processing MM5 output, MCIP may not 
need to be co-resident with MM5 if its output files are in IEEE binary format.  MCIP can be run on 
any UNIX computer for processing meteorological data in Models-3 I/O API format. 

Input parameters METROW, METCOL, and METLA Y are defined through the UNIX 

. environmental variable list.  These are used to check if the input meteorology data have correct 

dimensions as expected.  Within MCIP, the actual values will be picked up from header 
information of MM5 data file.  When offset values (IO, JO) are inconsistent with the information in 
MM5 header and in COORD.EXT, MCIP suggests a new set of (IO, JO) or (XORIG, YORIO) 
values.  Make sure to set NDEP to correspond to the number of lowest input layers being collapsed 
into the lowest output model layer, so aerodynamic resistances are computed accordingly. 

#### 12.5.3.2 Input  Files  for  MM5  Data  Processing 

Logical names of input files are described below: 

• 

• 

• 

MMOUT_DOMAIN#:  This standard MM5 file contains gridded hourly (or sub-hourly) 
two- and three-dimensional meteorological data covering the entire MM5 domain. 

MDL3_DOMAIN#:  This is optional EPA added MM5 output that contains file contains 
PBL parameters needed when the pass-through option is chosen. 

LAND_CR0_2D_##KM:  This is LUPROC output that contains  I I-category fractional 
land-use data covering the entire MM5 domain.  If USGS North American land/vegetation 
characteristic data base used as input to LUPROC is not enough to cover the desired 
domain, dominant landuse file LANDUSE_DOMAIN# from TERRAIN, a preprocessor 
for MM5 system, can be used. 

#### 12.5.3.3 Input  Files  for  1/0  API  Meteorological  Data  Processing 

MCIP can be used to extract meteorology data for a smaller window domain, to collapse number of 
layers, or to process meteorological data already in I/O API format further.  It treats gridded input 
meteorological data as pseudo profiles for temperature, moisture and wind components.  Required 
I/O API meteorological inputs for this option are; GRID_CR0_2D, GRID_DOT_2D, 
MET_CR0_2D, MET_CR0_3D, and MET_DOT_3D files.  Because it does not use boundary 
files as its input, the output domain can be as large as (NCOLS-2)x(NROWS-2) for windowing or 
layer collapsing process.  This option can be used to generate interpolated meteorology data for 
higher resolution grid than the original input as well. 

12-65 

Description 

Note 

number of layers in input meteorology 
data 
column (east-west) direction cell 
dimension In input meteorology data 
row (south-north) direction cell dimension 
.. 
in input meteorology data 
location of CTM domain origin offset in 
row direction= ROW  OFFSET 
location of CTM domain origin offset in 
column direction= COL=OFFSET 
file type for landuse data 

should be larger than or equal to 
CTMNLAYS 
should be larger than CTM 
NCOLS  ... 

.. 

. 

. .. 

should be larger than CTM 
NROWS 
offset should be counted based on 
the input grid definition 
offset should be counted based on 
the input grid definition 
1: TERRAIN binary  13 category 
2: preprocessed ASCII 11 
category 
3: MM5 dominant landuse 
category 
4: use landuse in GRIDCR02D 
5:  use USGS 1/0 API landuse 
file 
offset should be counted based on 
the input grid definition. For 
LUTYPE 4 & 5, IOLUSE is  not 
used 
offset should be counted based on 
the input grid definition. For 
LUTYPE 4 &  5, JOLUSE  is  not 
used 
TRUE: re-compute PBL 
parameters 
FALSE: pass through from 
MM5 
TRUE: use surface layer 
similarity 
FALSE:  use  PBL similarity 
TRUE: use CMAQ dry 
deposition 
FALSE: use RADM dry 
de  osition 

EP A/600iR-99l030 

lllllllllh' 

1)1ble 12-10~ "" MQJP Environmental Variables 

Environmental 
Variables 
METI.AY 

METCOL 

METROW 

IO 

JO 

LUI'YPE 

IOLUSE 

JOLUSE 

location of CTM domain origin offset in 
row direction w.r.t. landuse data origin 

location of CTM domain origin offset in 
column direction w.r.t. landuse data origin 

LCALCPBL 

flag for estimating PBL parameters of not 

r;§LFLUX 

flag for similanty algorithms 

LM3DDEP 

flag for dry deposition algorithm 

12-66 

Table 12-10.  MCIP Environmental Variables (continued) 

EP A/600/R-99/030 

Description 

Note 

flag for wind field correction 

0 (default): no correction 
+/- 1,2,3 wind field correction 
options (TBD) 
TRUE: compute cloud 
parameters 
FALSE: pass through from 
MM5 
TRUE: forces hydrostatic data 
output with approximations 
FALSE: pass through from 
MM5 
I  (default): time dependent 
0 (special): time independent 
TRUE: check 
FALSE: do not check 
2 (default):  100% increase when 
urban and water areas are 
coexistin 
(yyyyddd) 
(hh) 
(hh) 
(hhh) 
1 (default) 

FALSE (default) 

Environmental 
Variables 
LWIND 

LCALCCLD 

flag for cloud algorithm 

LHYDOUT 

flag  for hydrostatic output 

CRO_FfYPE 
DOT_FIYPE 

LSANTIY 

BMAX 

flag for 110 API file type 

flag for checking ranges of output 
parameters 
Maximum boost rate for urban area 

JUDA TE 
HSTRT 
HTOSKIP 
HTORUN 
NDEP 
GRDNAM 
EXECUTION  ID 
SCENFILE 
IOAPI_CHECK_HEAD 
ERS 

Julian date for the start time 
start hour 
number of hours to skip MCIP process 
number of hours to run 
number of deposition layers 
grid/domain name for MCIP output files 
user defined execution ID 
user defined file path 
flag for checking 1/0 API file headers 

12-67 

" ' ! ~ '  i I I I I ::' 

,,. 

"II' 
i!Jii: 

11111 

111, 

'II 

11111111111' 

,, 

EPN600/R-99/030 

!llll!llllli 

I 

i I ' 

1111 : : ~ II : I ! i ; : : ' 

' 

' I 

: ' I : 1111 : ; ; : ! ! ~ I 

,,  1:!!ble  1,7-1 L,,,,!YIG!P file ~inkage 

' " '  : : : : 1111 : ,, 

I ' 

I 

I 

' " 

'"',, 
' 
' 

11

1' 

:1!!

Environmental 
Variables 
MMSl 
MM52 
LU13 

11

11111111

::,, 

::1111111111':"" 

LUSE 

GRID.,DOT_2D 
M'E'r_cR.oP1t20·········· 

MET0 DOT.3D 
GRID,,,,BDY _2D=G l 
GRID.BOY  3D_G l 

,.  MET_CR0_2D_Gl 

,,  MET_DOT.3D_Gl 

Description 

Note 

for mm5  input module 
for mm5 input module 
.,  applicable for LtJrYPE =  1 
applicable for LUTYPE = 4 or 5 
for II13  input rrioCiufo 
for m3  input module 
for m3 input mocfofo 
for m3  input module 
for m3  input module 

Input filename for MM5 l  data file 
Input filename for MM52 data file 
~nput filename for TERRAIN binary 
land use 

, .. 

,. 

,. 

input filename for I/O API landuse file 
input fiieilaffie  for GRID  cR.o  2D 
input filename for GRID__,DOT_2D 
lllput filename for M:Et  cR.o  2D  · 
input filename  for MET  CR0_3D 
input filename for MET  DOT _3D 
output filename for GRID  BDY _2D 
output filename for GRID_BDY =3D 
output filename for GRID  CR0_2D 
output filename for GRID  CRO. 36 . 
output filename for GRID  DOT  2D 
output filename for MET  CRO  2D 
output filename for MET  CRO  3D 
output filename for MET  DOT  3D 

### 12.5.4 Defining  Grid  and  Domain  for  MCIP 

, .,.  Ml\.15  Grid  and  Domain  Definitions 

f2.5.4. l 
MMS may ou'tput as many as nII1e grid domains (one'coarse grid and up to eight nestedgrid 
domains) per MMS execution.  The MM5 developers took the approach of compiling.MM5 with 
the appropriate dimensions for each study being performed.  On the other hand, MCIP processes 
::,  o!;!tput meteorological data one grid domain at a time.  It needs to be compiled explicitly for each 
grid domain.  For multiple scale MM5 model runs, usually MM5 users have to define all the multi(cid:173)
l~vel Qest. dorpaim;Jogether with the coarse "mother" grid.  The complication in defining MM5 
domains as the Models-3 domain objects is caused by somewhat inauthentic use of the left-hand 
c§:ord~,n~te system and dot-point grid/domain definitions in MM?.  For the details on how to define 
MMS q9main§. users are recommended to read MM5 User's Guide.,  In the following, we uses a 
set of multi-level MM5 domains listed in Table 12-12 as the exampfos of the discussion. 

111111111111 

"' 

' 

I 

''' 

"' 

"II 

I 

'"'Ill'• 

''II;', 

'" 

,111 

Table 12-12.  An Example Set of Multi-level Nesting MM5 Domains 
{Note that  C(]) inMM5 corresponds to (ROW, COL) in CMAQ system.) 

lllhmll 

"'11'"111111''11111 

" 

'1111' 

'" 

' 

Ill"' 

II 

MMS 
Domain 
No.(N) 

1 
2 
:llllllllli,,, 3 

1111111:::::,4 

,,, 

Resolution 

(L1t=L1y) 

Dimensions 
(IX,JX) = 

(ROWS+J,COLS+l) 

Origin Relative to Base 

Origin Relative to 

Grid 

ImmCdiate Parent grid 

(l_ll,]_11) 

( ROlV,,ffut + 1, COL,iff,., + 1) 

l08krn 
36km 
!i,,;?~11111111, 
.:'Q~kn:i 

11111111111111'' 

'l;,,:,:111::::1, 

'"'

(41,61) 
(67,82) 
(~2.190) 
(100;82) 

(1,1) 
(9,24) 

' ( 19.333333,35.3~3333) 
'(21.444444,41.555555) 

(1,1) 
(9,24) 
<~'~,35) 
(2b,57) 

a) 

Definitions for MMS  108 km mother grid (N=l) 

In MMS, the grid is defined with the number of dot points.  Therefore, to use MMS's grid 
definitions, for Models-3/CMAQ's cross-point grid definitions, we need to use following 

NCOLS =JX-1 =61-1 =60 

NROWS =IX - 1=41-1=40. 

(12-138a) 

(12-138b) 

For this grid, no "parent grid" is defined. Origins should be defined with the formula that is 
specific to MMS' s method of grid/domain definition.  Note that we require the MMS mother 
domain's number of cells in horizontal directions (i.e., NCOLS and NROWS) to be even 
numbers, so that the dimensions of the four quadrants relative to the center of the coordinates are 
identical (Refer to Figure 12-9).  The origin of the coarse MM5 grid is defined with 

<x  .  y  . ) = (-__sElL * L1x  _ ____!!!!!::!:.. * L1y) 

orig '  orig 

' 

N 
2 

N 
2 

Because it is the mother grid of the grid family, offsets should be set to zero: 

(12-139) 

(12-140) 

Currently this domain is defined with resolution of 108km (see Table 12-12). 

b) 

Definitions for MMS 36 km first-level nest grid/domain (N=2) 

For this grid, the MMS  108km domain defined above is used as the "parent grid."  For the nested 
domains, NCOLS and NROWS can be either even or odd numbers: 

NCOLS=JX-1 =67-1=66 

NROWS =IX - 1=82-1=81. 

(12-14la) 

(12-14lb) 

Because the left-most and bottom comer of the nest 36km domain is defined with  (/1,1, 11,1)  in 
MMS, we need to convert this information into  (COL0.ff.rei•R0""1ffsei)  using following equation: 

(12-142) 

Note that MMS uses left-hand coordinate system, so that the index positions for  (/1,1, 11,1)  are 
reversed with those for the Models-3/CMAQ' s  ( COL0ffsel' R0""1ffsei).  In the following case, the 
general equation for computing the offset numbers from  (/1,1, 11,1)  of the MM5 domains will be 
presented.  The coordinates of the origin is computed with: 

( 
xoric•Yoric 

) _  (  parent  COL 
-

xorig  + 

offset 

L1xparent 

parent  ROW 

•Yoric  + 

offse1L.J.Y 

A  parent) 

(12-143) 


c) 

Definitions for MMS  12 km and 4 km nest grid/domains (N=3, 4) 

For the third~"""12kin domain cN=3), we have: 

: I ! ! ii i i 11 I ! ' 

I, 

,II 

:ll:"llllllli,,,,, 

I" 

"lll!!i 

,,I, 

NCOLS = JX - I= 100-1=99 

'"II 

,, 

(12-144a) 

''! 

"'' 

,illllll' 

NRO\VS =IX- 1=82-1  = 81 

(12-144b) 

a11d  MMS 3cikm domain is used as the parent grid.  Also, for the fourth 4 km domain (N=4 ), we 
have: 

II"' 11 .. ,: 

NCOJ'....S:;:; JX - 1=82-1  = 81 

NROWS =IX - I= 100-1=99 

(12-145a) 

(12-145b) 

~d Mf\/1,,~  I~km,,~orpain is,,,~,~~~ a~,tqe parent grid.  Usually, the offset values will be known to the 
~od~Is73/C~1~911111users. 

,,,.,  . 

;,,

11 

, 

MM5 Mother Grid/Domain and Coordinate Center 

, .. 
.. .. 

...  rr 
....  LI. 

llllli, 

(Xon11•  ~,) 

..  ~7 
..  " 

(Xr,YJ 

NCO LS 

A • 

;1 r 

Figure 12-10.  A Schematic of the MMS Mother Grid Definitions. 

Because ~\15's mother grid is defined from the coordinate center and grows outward, the 
numbers of columns and rows for each quadrants relative to the coordinate center should be the 
same, i.e., NCOLS and NROWS for the MMS mother grid should be even numbers.  In case the 
Qffset iqf ()nn,~t-ie>n)s not ,,known ~priori, one needs to compute the offset values using the header 

12-70 

1
:
11111 

information in the MM5 output files.  To use the information correctly, we need to know how the 
MM5 nest domain's left-most and bottom comer (/1,1,11,1)  is defined.  In MM5, all  the nest 
domains are defined relative to the mother grid, and  U1.1,J1.1)  provides the offset information of 
the nest domains in terms of the mother grid's resolution.  The formula linking a nest MM5's 
domain  (/1,1,11,1)  and Models-3/CMAQ's offset values  (COL0/fm'ROW,,ffut)  are given for  N  ~ 2 as: 

EP A/600/R-99/030 

where  (L1xpurenr I L1x) = 3  for the current MM5 application example.  Because our objective is to 
help users to find out  (COL01!,..,,ROW,,ff.w)  for the nest grid, we need to rewrite Equation  12-146 
for  (COL0ff.,et'ROW,,ff.,er).  Assuming we know all the coarser grid/domains offset values, 
(COL0ff.w•ROW,,ffser)  for the current nesting level  N  ~ 2 can be found as: 

(12-146) 

(COL 

)  = (L1xparenr / L1x)N-2  (J  ) 
1,1  N 

[ 

offw  N 

- l - ~ 

N-I 

(COL 

] 
6 (L1xparenr / L1x)k-2 

) 
off.w  k 

(ROW 

offset  N 

~ 

)  = (,1  parent/ ,1  )N-2  (/  )  _  1- ~ 

~ 

I.I  N 

[ 

(ROW 

] 
N-I 
£..i (,1  parent/ ,1  )k-2 
k=2  ~ 

) 
offset  k 
~ 

(12-147a) 

(12-147b) 

Although Equations  12-147 a and b look somewhat complicated, one can readily compute offset 
numbers for Models-3 CMAQ's grid/domain definitions for the example set of domains defined in 
Table 12-11  as follows: 

[CCOLoffset),(ROW,,ffw) ]N=3 = [3[(35i- -1)-3° * 23],3[(19i- -1)-3° * 8]] 

= [34,31] 

= [56,19] 

(12-148a) 

(12-148b) 

(l2-148c) 

Most of the time, the above MM5 domain definitions are provided by the MM5 modelers and 
Models-3/CMAQ users do not have to re-compute them. 

#### 12.5.4.2 CMAQ  Grid  Definitions 

The horizontal grid structure used for the MM5 serves as the parent grid for the CCTM domains. 
To define a CCTM grid, number of cells to be excluded from the parent domain must be 
detennined.  It is recommended that the CCTM grid be smaller than its parent MM5 grid by at least 
4 grid cells (preferably by 6 grids) to avoid the possible numerical reflection problems at 
11,,fvll'15  ~om~n. One~ the offset values are known,  (xqrig•Yorig)  of the nest domain can 
~,~mndari~,~ ot
be computed using Equation  12-143.  Refer to Appendix 12B for other examples of other CCTM 
nest domain definitions. 

12.S .5 

Extension  of MCIP  for  Other  Meteorological  Models 

CMAQ 

·1· 

al'  d 

ty mo  e mg, 

d  1··· 

uti 1zes gener  1ze  coor  mates. 

The current release version of MCIP can only process MM5 output in binary format or previous 
¥CIP outI?utin 1/0 API format.  To realize the one-atmosphere concept for meteorological and air 
111111111 .• ali 
qu 
coordinate system is that a single CTM can adapt to any of the coordinate systems and dynamics 
commonly used in meteorological modeling.  Because most meteorological models are not 
originally designed for air quality studies, they lack characteristics that are required for air quality 
modeling.  Consistencies in dynamic descriptions of atmosphere, physical parameterizations, and 
numerical algorithms, where applicable, in meteorological and chemistry-transport models are 
cHtical in detennining the quality of air pollutant simulations.  This issue becomes more critical 
with high resolution air quality study where nonhydrostatic meteorological models must be used. 

al'  d 
e gener  1ze 

ne a  vantage o 

f th 

d' 

0 

d 

MCIP is the key processor allowing the consistent linkage between meteorological models and 
CMAQ.  Currently, different meteorological models are used by different atmospheric modeling 
groups forming their own respective user communities.  It is because these meteorological models 
ru-e  applicable for a limited range of spatial and temporal scales.  To expand the user base of the 
~9~els-3 C~11~Q system and to promote the one-atmosphere community modeling paradigm, it is 
e5sential to continuously develop MCIP modules for several popular mesoscale meteorological 
models such as RAMS, ARPS, HOTMAC, and others. 

1111111111111 

,, 

'"'" 

'"'"""'' 

To build a version of MCIP for processing a set of meteorological model output, several issues 
'"'' 
involved with different dynamics and coordinates must be considered.  They are; ( 1) compatibility 
of governing set of equations and state variables used, (2) scale limitations in subgrid scale 
parameterizations such as cloud, turbulence, and surface exchange processes, and (3) consistency 
in numerical algorithms and discretization methods.  Before designing MCIP modules for other 
meteorological model, developers should identify the structure, format, and frequency of data as 
well as their impact on the processing structure of MCIP.  MCIP expects time dependent 
meteorological data in a structured grid system as the input.  As this moment, there is no provision 
for handling irregularly spaced data structure either from observations or from a meteorological 
modeling system with unstructured adaptive (fixed or dynamic) grid structure, such as the one 
used in OMEGA system (Bacon et al.,  1996).  If there is need for including these data, object 
analysis tools must be used to prepare data in a structured grid system. 

The essential information related to the coordinates and dynamic assumptions used in 
meteorological models is captured in MCIP.  Dynamic and thermodynamic state variables are recast 
into the fully compressible system as described in Chapter 5 of this volume. 

It is important to know the processing sequence and associated data structures used in the current 
MCIP code.  Major processing steps in MCIP can be summarized as follows:  ( 1) Grid, coordinate, 
and information on atmospheric dynamics are used to compute Jacobian and layer heights; (2) State 
variables in meteorological models are recast into density and entropy; (3) Contravariant wind 
components are computed; (4) necessary two-dimensional parameters are diagnosed; and (5) Some 
of two- and three-dimensional parameters are passed through.  Figure  12-11  shows corresponding 
data structures used at different phases.  Input phase uses arrays with dimensions covering the full 
meteorology domain ('F-arrays'), processing phase uses arrays for extended domain covering both 
the CMAQ and boundary domains ('X-arrays').  During the output phase, the information in X(cid:173)
arrays are separated into the CMAQ and boundary domains for respective data types described in 
Table 12-3. 

Input phase  Processing 

Output phase 

Met. Domain 

'F'-arrays 
Figure 12-11. 
Processing 

Extended CMAQ Domain 

CMAQ Domain 

'X'-arrays 

Dot &  Cross 

Boundary 
Domain 

Data Structures Corresponding to Input, Processing, and Output Phases of MCIP 

12. 6  Concluding  Remarks 

MCIP is a key processor linking meteorological models to the CMAQ modeling system.  Its major 
roles are: 

1. 

2. 

To read meteorological data from a meteorological model and converts them in Models-3 
IIO API format. 

To provide all the necessary meteorological parameters for air quality simulations. When 
necessary, PBL parameters and other derived quantities are computed using gridded 
meteorology data and high-resolution fractional land use information. 

3. 

To support multiscale generalized coordinate implementation of the CCTM. 

Current implementation of MCIP is mainly for linking MM5 output to CCTM.  Reader modules to 
Regional Atmospheric Modeling System (RAMS) (Pielke et al.,  1992), will be added.  Users who 
wish to link other meteorological models may need to modify and introduce appropriate input 
modules. 


## 12.7  References 

Alduchov, 0. A. and R. E. Eskridge,  1996: Improved Magnus form approximation of saturation 

pressure. J.  Appl. Meteorol., 35, 601-609. 

Anthes, R. A., E. Y. Hsie and Y.H Kuo,  1987: Description of the Penn State/NCAR Mesoscale 
:.  Model version 4 (MM4).  NCAR Technical Note, NCAR!fN-282+STR, National Center for 

Atmosph~rlc 'R:esearch, Boulder, CO. 66pp. 

Avissar, R., P. Avissar, Y. Hahrer, and B. A.  Bravdo,  1985:  A model to simulate response of 

plant stomata to environmental conditions.  Ag. and Forest Meteorol., 34, 21-29. 

Bacon, D. P., Z.  Boybeyi, P.R. Boris, T.  J.  Dunn, M. Hall,  R.  A.  Sarma, S.  Young, and J. 

Zack,  1996: Ail overview of the operational Multi-sclae Environmental model with Grid 
Adaptivity. Proc.  of the 15th Conj.  on Wea.  Anal. and Forecasting, Norfolk, VA, August  19-
23,  1996, American Meteorological Soc., Boston, MA. 

Ball, J. T., I.E., Woodrow, and J.  A. Berry,  1987: A model predicting stomatal conductance and 

its contribution to the control of photosythesis under different environmental conditions.  In 
Progress in photosynthesis Research,  Vol.  IV (ed. J.  Biggins), pp. 221-234.  Martinus Nijhof 
Publishers, Dordrecht. 

Betts, A. K.,  1973: Non-precipitating cumulus convection and its parameterization. Quart. J.  Roy. 

Meteorol.  Soc. 99,  178-196. 

Binkowski, F.S. and U. Shankar,  1995: The Regional Particulate Model.  1.  Model description 

and preliminary results. J.  Geophys.  Res.,  100 Dl2, 26191-26209. 

Blackadar, A. K.,  1978: Modeling pollutant transfer during daytime convection, Preprints,  Fourth 

'.,  Symposium on Atmospheric Turbulence,  Diffusion, and Air Quality, Reno, Am. Meteor. 

Soc., 443-447. 

i3'~ost R.  A.  ~di ;t. Wyngaard,  1978: A model study of the stably stratified: planetary boundary 

"'I 

layer. J.  Atmos.  Sci., 35,  1427-1440. 

· 

Businger J. A., J.C. Wyngaard, Y. Izumi, and E. F. Bradley,  1971: Flux profile relationships in 

the atmospheric surface layer, J.  Atmos.  Sci., 28,  181-189. 

Byun, D. W.,  1990: On the analytical solutions of flux-profile relationships for the atmospheric 

surface layer, J.  Appl.  meteorol., 29, 652-657. 

Byun, D. W.,  1991: Determination of similarity functions of the resistance laws for the planetary 

boundary layer using surface-layer similarity functions, Boundary Layer Meteorol., 57,  17-
48. 

Byun, D.W., 'i999a: Dynamically consistent formulations in  meteorological and air quality models 
""""'  for multiscate atmospheric applications:  I. Governing equations in generalized coordinate 

System. J.  Atmos.  Sci.,  (in print) 

Byun, D.W.,  1999b: Dynamically consistent formulations in meteorological and air quality models 

for multiscale atmospheric applications:  II. Mass conservation issues. J.  Atmos. Sci., (in 
print) 

Byun, D. W., and R.L. Dennis,  1995: Design artifacts in Eulerian air quality models: Evaluation 

of the effects of layer thickness and vertical profile correction on surface ozone concentrations. 
Atmospheric Environment,  29, No.  1,  105-126. 

Carson, D. 1.,  1973: The development of a dry inversion-capped convectively unstable boundary 

layer.  Quart.  J.  Roy.  Meteorol.  Soc. 99, 450-467. 

Cederqvist, P.,  1993:  Version Management with CVS,  Signum Support AB, Linkoping, Sweden. 

168pp. 

Chang, J.S.,  R.A.  Brost,  LS.A.  Isaksen, S.  Madronich,  P.  Middleton, W.R.  Stockwell, and C.J. 

WaJcek,  1987: A three-dimensional Eulerian acid deposition model: Physical concepts and 
formulation,  J.  of Geo phys.  Res., 92,  14,681-700. 

Chang, J.S.,  P.B.  Middleton, W.R.  Stockwell, C.J.  Walcek, J.E.  Pleim,  H.H.  Lansford, S. 

Madronich,  N.L.  Seaman, D.R.  Stauffer, F.S.  Binkwski, D.  Byun, J.N.  McHenry,  H.  Hass, 
and  P.J. Samson,  1990: The Regional Acid Deposition Model and Engineering Model, 
NAPAP report 4. 

Coats, C. J., cited  1996: The EDSS/Models-3 I/O Applications Programming Interface. MCNC 

Environmental Programs, Research Triangle Park, NC.  [Available on-line from 
http://www.iceis.mcnc.org/EDSS/ioapi/H.AA.html.] 

Collatz, G. J., C. Grivet, J. T. Ball, and J. A. Berry,  1991:  Physiological and environmental 

regulation of stomata] conductance, photosynthesis and transpiration: a model that includes a 
laminar boundary layer.  Ag.  and Forest Meteorol., 54,  107-136. 

Driedonks, A. G. M.,  1981: Dynamics of the well-mixed atmospheric boundary layer. De Bilt 

K.NMI Sci. Rep. WR 81-2,  189pp. 

Dudhia, J.,  1989: Numerical study of convection observed during the Winter Monsoon 

Experiment using a mesoscale two-dimensional  model. J.  Atmos.  Sci., 46, 3077-3107. 

Dudhia, J.,  1993: A nonhydrostatic version of the Penn State-NCAR mesoscaJe model: Validation 
tests and simulation of an Atlantic cyclone and cold front, Mon.  Wea.  Rev.,  121,  1493-1513. 

Erisman, J. W., A. van Pul, and P. Wyers,  1994: Parameterization of surface resistance for the 

quantification of atmospheric deposition of acidifying pollutants and ozone.  Atmos. Environ. 
28,  2595-2607. 

Fowler, D.,  1978: Dry deposition of S02 on agricultural crops, Atmos.  Environ.,  12, 369-373. 

Fritsch, J.M., and C.F. Chappell, 1980: Numerical prediction of convectively driven mesoscale 

pressure systems,  1,  Convective parameterization, J.  Atmos. Sci., 37,  1722-1733. 

Geleyn, J.-F., A. Hense, and H.-J. Preuss,  1982: A comparison of model-generated radiation 

fields with satellite measurements.  Beitr.  Phys. Atmos., 55, 253-286. 

Grell, G.A., J. Dudhia, and D.R. Stauffer,  1994:  A Description of the Fifth-Generation PENN 

STATE/NCAR Mesoscale Model (MM5), NCAR Technical Note, NCAR!fN-398+STR, 
National Center for Atmospheric Research, Boulder, Colorado, 138pp 

111111

1

111111;;1 

Haagenson, P.  L., J. Dudhia, D. R.  Stauffer, and G.  A.  Grell,  1994: The Penn State-NCAR 
;;;;;;;;;,,  mesgscaj~ m.2 .. ?el (MM5) source code document, NCAR/TN-392+STR, National Center for 
""'  Atmospheric Research, Boulder, Colorado. 
1. 
Hack, J. J., B. A. Boville, B. P. Briegleb, J. T. Kiehl, P.  J.  Rasch, and D. L. Williamson,  1993: 
Description of the NCAR community climate model (CCM2).  NCAR Tech. Note, NCAR!fN-
382+STR, 120 pp. [Available from the National Center for Atmospheric Research, P.O. Box 
3000, Boulder, CO 80307.] 

f',  :i•·:· 
,,lit, 

llii:'!!! 
,,,,,,,Jlll11111 

i':
1llll!!' 
II 
, 

, 

' 
' 

,, 
'" 

,,,, 

;,,, 

I';, 

' 
' 

111

•

·:,;, 

, 

, 

"' 

1

,, 

Hess, G.D., 1992: Observations and scaling of the atmospheric boundary layer. Aus. Meteorol. 
,,,,,,.,,  M 

ag. 

41  79 99 
. 

, 

-

1111111111. 

I 

''"'II 

'1''"11 

~ogstrom, U.,  1988: Non-dimensional wind and temperature profiles in the ~tmosphe~ic surface 

layers: a re-evaluation. Boundary-Layer Meteor., 42, 55-78. 

Holtslag, A. A.,  E. I.  F. de Bruin, and H.-L. Pan,  1990:  A high resolution air mass 

transformation model for short-range weather forecasting. Mon.  Weather Rev., 118,  1561-
1575. 

Holtslag, A.AM., E.V. Meijgaard, and W.C. DeRooy, 1995:  A comparison of boundary layer 

diffµsionschemes in unstable conditions over land, Boundary-Layer Meteor., 76, 69-95. 

""' 

' 

"" 

ili111::':' 

1 

,,'·1;;;1 11

"

,, 

1ll!: 

,11111111: 111:111: 

,p 

,, 

Jarvis, P. G.,  1976: The interpretation of the variation of leaf water potential and stomata! 

I 

" 

" 

" 

I• 

I 

conductance found in canopies in the field.  Phil.  Trans.  Royal Meteorol.  Soc.  of London, 
Series IJ,  273,  593-61 o. 

Jacquemin B. and J. Noilhan, 1990: Sensitivity study and validation of a land surface 

parameterization using the HAPEX-MOBILHY data set.  Bound.-Layer Meteorol.,  52, 93-
134. 

Jaung, H.-M. H.,  1992: A spectral fully compressible nonhydrostatic mesoscale model in 

hydrostatic sigma coordinates: Formulations and preliminary results. Meteor.  Atmos.  Phys., 
SO,  75-88. 

Kain, J. S., and J.M. Fritsch,  1993: Convective paramterization for mesoscale models: The Kain(cid:173)
Fritsch scheme. Jthe representation of cumulus in mesoscale models., K.A. Emanuel and D.J. 
Raymond, Eds., Amer. Meteor. Soc., 246pp. 

Lo, A. K.,  1993: The direct calculation of fluxes and profiles in the marine surface layer using 

measurements from a single atmospheric level. J.  Appl. Meteorol.,  32,  1890-1990. 

Lo, A. K.,  1995: Determination of zero-plane displacement and roughness length of a forest 

canopy using profiles of limited height. Boundary-Layer Meteorol., 75, 381-402. 

Madronich, S.,  1987: Intercomparison of N02 photodissociation and UV radiometer 

measurements, Atmos.Environ., 21,  569-578. 

Mason, P.J.,  1987: The formation of areally-averaged roughness lengths.  Q.J.R.  Meteorol.  Soc., 

106,  351-366. 

Mesinger, F. and A. Arakawa,  1976: Numerical methods used in atmospheric models. In:  Global 

Atmospheric Research Programme (GARP), Joint Organization Committee, GARP Publication 
Series, No.  17, Aug.,  1976:  [Available from WMO Secretariat, Geneva]. 

Mihailovic, D. T., R.  A. Pielke, B. Rajkovic, L. J. Tsengdar, and M. Jeftic,  1993:  A resistance 
representation of schemes for evaporation from bare and partly plant-covered surfaces for use 
in atmospheric models.  J.  Appl. Meteorol., 32,  1038-1054. 

Noilhan, J.  and S. Planton,  1989: A simple parameterization of land surface processes for 

meteorological  models.  Mon.  Wea.  Rev.,  117, 536-549. 

Ooyama, K. V.,  1990: A thermodynamic foundation for modeling the moist atmosphere. J. 

Atmos.  Sci.,  47,  2580-2593. 

Padro, J., G.  den Hartog, and H.  H. Neumann,  1991: An  investigation of the ADOM dry 

deposition module using summertime 0 3  measurements above a deciduous forest, Atmos. 
Environ.,  25A,  1689-1704. 

Paulson, C. A.,  1970: The mathematical representation of wind speed and temperature profile in 

the unstable atmospheric surface layer. J.  Appl. Meteor., 9, 857-861. 

Pielke, R.  A., W.R. Cotton, R.  L.  Walko, C.  J. Tremback, M.  E.  Nicholls, M.  D.  Moran, D.  A. 

Wesley., T. J. Lee, and J.  H.  Copeland,  1992: A comprehensive meteorological modeling 
system-RAMS.  Meteor.  Atmos.  Phys., 49, 69-91. 

Pleim, J., A.  Venkatram, and R.  Yamartino,  1984:  ADOM!f ADAP Model Development 

Program, V?lume 4, The Dry Deposition Module.  ERT P-B980-520, Prepared for OME, AES 
of Canada and the Umweltbundesamt, Germany. 

Pleim, J.E. and A. Xiu,  1995:  Development and testing of a surface flux and planetary boundary 

layer model for application in  mesoscale models, J.  Appl. Meteor.,  34,  16-32. 

Pleim, J.  E.  , A.  Xiu, P.  L.  Finkelstein, and J.  F. Clarke,  1997:  Evaluation of a coupled land(cid:173)

surface and dry deposition model through comparison to field measurements  of surface heat, 
moisture, and ozone fluxes, Proceedings of the  12th Symposium on Boundary Layers and 
Turbulence, July  28-August 1,  1997, Vancouver, BC. 

Pleim, J.E., J. F. Clarke, P.  L.  Finkelstein, E. J. Cooter, T.  G.  Ellestad, A.  Xiu, and W.  M. 

Angevine,  1996:  Comparison of measured and modeled surface fluxes of heat, moisture and 
chemical dry deposition.  In: Air Pollution Modeling and its Application XI, Ed: Gryning and 
Schiermeier, Plenum Press, New York. 

1111!""" 

,, 

'""lll:,,,1111111111111111 

'''" 

"'' 

" 

11111"' 

'''11111, 

, R~hette, P.1, 1111DesJ~dins, R.  L., Dwyer, L. M., Stewart, D. W., Pattey, E.,  and Dube, P.A., 
1991: Estimation of maize canopy conductance by scaling up leaf stomatal conductance. Ag. 
and Forest Meteorol., 54, 241-262. 

,,  Schumann, U.,  1989: Large-eddy simulation of turbulent diffusion with chemical reactions in  the 

convecti":,~"" po~ndary layer. Atmos.  Environ., 23,  1713-1727. 

' 

,,,,,,1111,,,,,, 

,1111111 
::1111111' 

" 

,,1, 

,,,, 
'" 

,, 

' 

Seaman, N. L:; D.R. Stauffer, and A. M. Lario-Gibbs,  1995: A multiscale four-dimensional data 

',,,1111111111111 

I'°'" 

,.,1 

''"" 

'" 

" 

' 

assimilation system applied in the San Joaquin Valley during SARMAP. Part I:  Modeling 
design and basic performance characteristics, J.  Appl. Meteor., 34,  1739-1761. 

!, 
I,, 

, 

" "  
, 

,!!111111' 
:1111111 

'Ill' 

I 

Seaman, N. L., and D.R. Stauffer,  1995: Simulation of the thermal structure of the San Joaquin 

Valley during SJV AQS/AUSPEX/SARMAP using MM5 with four-dimensional data 

, " assimilation, paper presented at the A WMA conference, Regional Photochemical Measurement 

'•,,. 

""'',,111111 

111 

,:11""""'111·11111 

""""'""' 

, and Mode;,,!ing Studies, November 8-12, San Diego, Calif.,  1993. 

Sellers, P. J., Y. Mintz, Y.  C. Sud, and A.  Dalcher,  1986: A simple biosphere model (SiB) for 

" 

llllllli, 

, 

11' 

',111 

11iiilllli 

1
' 
!!"!!!Ill,,  '

1::r 

use within general circulation models.  J.  Atmos. Sci., 43, 505-531. 

Sellers, P.  J., R.  E.  Dickinson, D.  A.  Randall,  A.  K.  Betts, F.  G.  Hall, J.  A.  Berry, G.  J. 

Collatz, A. S. Denning, H.  A.  Mooney, C.  A. Nobre, N.  Sato, C.  B.  Field, and A, 
Henderson-Sellers,  1997: Modeling the exchanges of energy, water, and carbon between 
continents and the atmosphere.  Science, 275, 502-509. 

Sheih, C.M., M.L. Wesely, and B. B. Hicks,  1979: Estimated dry deposition velocities of sulfur 

over the eastern United States and surrounding regions, Atmos.  Environ., 13,  1361-1368. 

slrois, A., an~'L.A. Barrie,  1988: An estimate of the importance ~f dry deposition as a pathway of 
acidic substances from the atmosphere to the biosphere in eastern Canada, Tellus, 40B, 59-80. 

Slinn, W. G.  N., L.  Hasse, B.  B.  Hicks,  A.  W.  Hogan,  D.  Lal,  P.  S. Liss, K.  0. Munnice, G. 
""""'"  A. ~ehm~,!,~, and o.,,, Vi~iori, ,,! ~78: Some aspects of the transfer of atmospheric trace constituents 
past the ,~ir-sea i,~terface. Atmos.  Environ. 12, 2055-2087. 

'1'"111 

:111111111 

,1 

" 

111 

111 

I 

Ill 

.. 

1111' 

I' 

Stauffer, D.R., and N. L. Seaman, 1993: Evaluation of multiscale four-dimensional data 

assimilation techniques developed and tested in MM5 during SJV AQS/ AUSPEX/ SARMAP, 
paper presented at the A WMA conference, Regional Photochemical Measurement and 
Modeling Studies, November 8-12, San Diego, Calif.,  1993. 

Tesche, T. W., and D. McNally, 1993: Operational evaluation of the SARMAP meteorological 
, model (SMMI) for episode 1:  3-6 August,  1990, Alpine Geophysics, Crested Butte, CO. 

Thorpe, S., cited 1996: PA VE User Guide. MCNC Environmental Programs, Research Triangle 

Park, NC, [Available on-line from http://www.iceis.mcnc.org/ 
EDSS/pav~_doc/SCCS/s.Pave.html.] 

Voldner, E.C., L.A. Barrie, and A.  Sirois,  1986: A literature survey of dry deposition of oxides of 

sulfur and nitrogen with emphasis on long-range transport modelling in North America, 
Atmos.Environ., 20,  2101-2123. 

Walcek, C.J.,  1987: A theoretical estimate of 03 and H202 dry deposition over the northeast 

United States, Atmos.Environ., 21,  2649-2659. 

Walcek, C.J., R.  A.  Brost, J.  S.  Chang, and M. L.  Wesely,  1986: S02, sulfate and HN03 

deposition velocities computed using regional landuse and meteorological data, 
Atmos.Environ., 20,  949-964. 

Walcek, C.J., and G.R. Taylor,  1986: A theoretical method for computing vertical distributions of 

acidity and sulfate production within cumulus clouds, I.Atmos.Sci., 43, 339-355. 

Warner, J.,  1970: On steady-state one-dimensional models of cumulus convection, I. Al!Jlos.  Sci., 

27,  1035-1040. 

Wesely, M. L.,  1989: Parameterization of surface resistances to gaseous dry deposition in 

regional-scale numerical models, Atmos.Environ., 23,  1293-1304. 

Wesely, M. L., and B. B. Hicks,  1977: Some factors that affect the deposition rates of sulfur 

dioxide and similar gases on vegetation, I. Air Pollut.  Control. Assoc., 27,  1110-1116. 

Wesely, M.  L., D.R. Cook, R.  L.  Hart, and R.  E.  Speer,  1985: Measurements and 

parameterization of particulate sulfur dry deposition over grass, J.  Geophys. Res., 90, 2131-
2143. 

Wetzel, P. J.,  and J.-T. Chang,  1988: Evapotranspiration from nonuniform surfaces: a first 
approach for short-term numerical weather prediction.  Mon.  Wea.  Rev.,  116, 600-621. 

Wojcik, G.S., and J.S. Chang,  1996: An Eulerian modeling study of anthropogenic sulfur 

budgets and lifetimes for Eastern North America, submitted to I.Atmos.Chem. 

Wyngaard, J.  and Brost, J.C. and R.  A. Brost,  1984: Top-down and bottom-up diffusion of a 

scalar in the convective boundary layer. I.  Atmos.  Sci., 41,  102-112. 

Xue, M., K.  Droegemeier, V.  Vong, A.  Shapiro, and K.  Brewster,  1995:  ARPS Version 4.0 

User's Guide. Center for the Analysis and Prediction of Storms, Univ. of Oklahoma, 380 pp. 
[Available from Center for the Analysis and Prediction of Storms, Univ. of Oklahoma, 
Norman, OK 73019]. 

Zilitinkevich S. S., 1989: Velocity profiles, the resistance law and the dissipation rate of mean 

flow kinetic energy in a neutrally and stably stratified planetary boundary layer. Bound. Layer 
Met., 46,  367-387. 

This chapter is taken from Science Algorithms of the EPA Models-3 Community 
Multiscale Air Quality (CMAQ) Modeling System, edited by D. W. Byun and J. K. S. 
Ching, 1999. 


## Appendix  12A 

MCIP Output Data 

MCIP ytfites gie f?ulk of its two- and three-dimensional meteorology and geophysical output data 
in a transportable binary format using a tailored Models-3 input/output applications program 
interface (I/O API) library.  Depending on whether the meteorological vertical coordinate is time 
dependent or not, the temporal characteristics of certain variables that belong to GRID data types 
and MET data types may differ.  For example, when the meteorological coordinate is crP,  which is 
time dependent, Jacobian is also time dependent.  On the other hand for  a po  coordinate, Jacobian 
i"s time independent.  However we are using a consistent list of meteorological variables for both 
hydrostatic and nonhydrostatic coordinates for as the standard output. Tables 12A-1  through  12A-
6 provide lists of variables in each data type. 

1

' 

' '

11111' 

" 

"""I'  . .  "'1111111 
11 :1, 

.... ::1,1"''1111' 

' 

Table  l 2A 1. Variables in GRIDCR02D 

"111

' 

' 

Variable Name 

LAT 
Lal 
~FX2 
Hl' 
iZERo 
PRSFCO 

!11111111 

111111:"" 

,, 

JACOBSO 
WSE;_URBA 
LUSEJGRI 
LU~ 
LUSJLDECI 
~COOI 
LUSlUfiXF 
LUSE;_WATE 
~SE_BARR 
LUSEJ'QIB 
LUSE_.MIXA 
LUSJLDECI 
WSE_ROC!< 

Unit 
DEGREES 
DEGREES 
(M/M)**2 
M 
M 
Pascal 

M 
fraction 
fraction 
fraction 
fraction 
fraction 
fraction 
fraction 
fraction 
fraction 
fraction 
fraction 
fraction 

(0-1) 
(0-1) 
(0-1) 
(0-1) 
(0-1) 
(0-1) 
(0-1) 
(0-1) 
(0-1) 
(0-1) 
(0-1) 
(0-1) 

Description 
latitude  (south  negative) 
longitude  (west  negative) 
squared  map-scale  factor  (CROSS) 
terrain  elevation 
roughness  length 
surface  pressure  difference  of  reference 
atmosphere 
total  Jacobian  at  surface 
landuse  for  URBAN_LAND 
landuse  for  AGRICILTURE 
landuse  for  RANGE 
landuse  for  DECIDUOUS_FOREST 
landuse  for  CONIFEROUS_FOREST 
landuse  for  MIXFOREST_WETLND 
landuse  for  WATER 
landuse  for  BARREN_LAND 
landuse  for  NONFOREST_WETLND 
landuse  for  MIXAGRI_RANGE 
landuse  for  URBAN__LAND 
landuse  for  ROCKY  OPENSHRUB 

Table l 2A2.  Variables in GRIDDOT2D 

1111111;;: 

,, 

,,::.;,, 

""Variable Name 

111" 

11 

111111111 

LAT 
Lal 
MSFD2 

Unit 
DEGREES 
DEGREES 
(M/M) **2 

Description 
latitude  (south  negative) 
longitude  (west  negative) 
squared  map-scale  factor  (DOI') 

Table  12A3.  Variables in GRIDCR03D 

Variable Name 

DENSO 
ENT RPO 
JACOBOF 
JACOB OM 
TEMPO 
PRESO 
X3HTOF 

X3HTOM 

Unit 
KG/M**3 
J/K/M**3 
M 
M 
K 
Pascal 
M 

M 

Table  12A4. Variables in METCR03D 

Variable Name 

JACO BF 
JACO BM 
DENSA.....J 
DENSW_J 
ENTRP_J 
WHAT_JD 
r:;i:; 
QR 
<;N 
TA 
PRES 
DENS 
WWIND 
ZH 
ZF 
JDRATE 

Unit 
M 
M 
KG/M**2 
KG/M**2 
J/K/M**2 
KG/ (M"S) 
KG/KG 
KG/KG 
KG/KG 
K 
Pascal 
KG/M*"3 
MIS 
M 
M 
KG/M**2/S 

Table 12A5. Variables in METDOT3D 

Variable Name 

UWIND 
VWIND 
UHAT_JD 

VHAT_JD 

Unit 
MIS 
MIS 
KG/ (M*S) 

KG/(M*S) 

EPr\/600/R-991030 

Description 
density  of  reference  atmosphere 
entropy  density  of  reference  atmosphere 
total  Jacobian  at  layer  face 
total  Jacobian  at  layer  middle 
te:rrperature  of  reference  atmosphere 
pressure  of  reference  atmosphere 
height  of  layer  face  (top)  of  reference 
atmosphere 
height  of  layer  middle  of  reference 
atmosphere 

Description 
total  Jacobian  at  layer  face 
total  Jacobian  at  layer  middle 
Jacobian  weighted  total  air  density 
Jacobian  weighted  density  of  vapor 
Jacobian  weighted  entropy  of  moist  air 
J  & Density weighted  vertical  contra-W 
cloud  water  mixing  ratio 
rain  water  mixing  ratio 
water  vapor  mixing  ratio 
air  te:rrperature 
pressure 
total  density  of  air 
true  W corcponent  of  wind 
mid-layer  height  above  ground 
full-layer  height  above  ground 
time  rate  change  of  Jacob*Density 

Description 
u-comp.  of  true  wind  at  dot  point 
V-comp.  of  true  wind  at  dot  point 
(contra_U*Jacobian*Density)  at  square 
point 
(contra_V*Jacobian*Density)  at  triangle 
oint 

Table 12A6. Variables in METCR02D 

Variable Name 

11111111111 

'11111,, 

PRSFC 
JJl.COBS 
IENSAS 
WSTAR 
RIB 
PBL 
ZRlJF 
?«LI 
HFX 
~ 
l«lLI 
RADYNI 
RBNm'I 
RS'l'QII 
TEMPG 
TEMPlO 
TEMPlPS 
SURF2 
ALBEDO 
FSOIL 
Gal 
GSW 
~ 
RNET 
RN 
~· 
CFMCH 
CFRAQl 
CFRJ\CL 
CFRAC 
CI.Dr 
cum 
wBAR 
VD,,XXXX 

1111111111: 

Unit 
Pascal 
M 
KG/M**3 
M/S 
NOD IM 
M 
M 
1/M 
WA'I'I'S/M**2 
WA'I'I'S/M**2 
l/M 
M/S 
M/S 
M/S 
K 
K 
K 
EMPTY 
NO DIM 
WA'I'I'S/M**2 
WA'I'I'S/M**2 
WA'I'I'S/M**2 
WA'I'I'S/M**2 
WA'I'I'S/M**2 
CM 
CM 
Fraction 
Fraction 
Fraction 
Fraction 
M 
M 
G/M**3 
M/S 

Description 
surf ace  pressure 
total  Jacobian  at  surface 
air  density  at  surface 
convective  velocity  scale 
bulk  Richardson  number 
PBL  height 
surface  roughness  length 
inverse  of  Monin-Obukhov  length 
sensibie  heat  flux 
latent  heat  flux 
inverse  of  Monin-Obukhov  length 
inverse  of  aerodynamic  resistance 
inverse  of  laminar  bnd  layer  resistance 
bulk  stomata!  resistance  for  water 
skin  t~erature at  ground 
air  t~erature at  10  m 
air  t~erature at  1.5  m 
surface  parameter  1 
surface  albedo 
heat  flux  in  soil  layers 
longwave  radiation  at  ground 
solar  radiation  absorbed  at  ground 
solar  radiation  reaching  surface 
net  radiation  . 
accumulated  nonconvective  hourly  precip. 
accumulated  convective  hourly  precip. 
fraction  of high  cloud 
fraction  of  middle  cloud 
fraction  of  low  cloud 
total  cloud  fraction 
cloud  top  layer  height 
cloud  bottom  layer  height 
liquid  water  content  of  cloud 
arY  deposition  velocity  for  species  xxxx 

Note: List of deposition velocities 
VD....S02 
VD...,SULF 
VJ)JK>2 
VDJK) 
VD_03 
VDJ-!003 
VDJl202 
VD__,ALD 
VDJCHO 
VD_OP 
VD_ORA 
VD_NH3 
VD_l?AN 
VDJI(H> 
VD_CO 

deposition  velocity 
deposition  velocity 
deposition  velocity 
deposition  velocity 
deposition  velocity 
deposition  velocity 
deposition velocity 
deposition  velocity 
deposition velocity 
deposition  velocity 
deposition  velocity 
deposition  velocity 
deposition velocity 
deposition  velocity 
deposition  velocity 

: "M/S" 
:"MIS" 
: "M/S" 
: "M/S" 
: •M1s• 
: "MIS" 
: ·M1s• 
: •Mis• 
: "M/S" 
: "MIS" 
: "MIS" 
:"M/S" 
: •M1s· 
: "M/S" 
: "M/S" 

for 
for 
for 
for 
for 
for 
for 
for 
for 
for 
for 
for 
for 
for 
for 

species  S02 
species  SULF 
species  N02 
species  NO 
species  03 
species  HN03 
species  H202 
species  ALD 
species  HCHO 
species  OP 
species  ORA 
species  NH3 
species  PAN 
species  HONO 
species  co 

Appendix  12B 

Examples  of  Nest  Domain  Definitions  for  CMAQ  system 

12B.1 

Grid  Domain  Definitions  for  the  Models-3  CMAQ  Tutorial  Study 

Table 12B- l.  Origins for the TUTORIAL MMS domain, MCIP landuse domain, and CTM 
domains.  Note that (x,y) corresponds to (COL.ROW) in CMAQ.  MCIP landuse domain is 
smaller by one cell around than the original MMS domain.  Also, the CTM domain removes three 
cells around from the original MM5 domain. Origin is measured from the center of  Base 108 km 
domain located at 90W and 40N. 

Resolution 
(L1.t=L1y) 

108km 
36km 
12km 
04km 

Origin for original MM5 

Origin for MCIP Landuse 

Origin for CTM Domain 

Domain 

(xorig•Yorig) 

(-3996xl03
(432x103
(972x 103
(1200xl03

,  -2808xl03
,  -432xl03
,  -36xl03
) 
,  120xl03
) 

) 

) 

Domain 

(Xorig•Yorig) 

NIA 
,  -396xl03
) 
,  -24x 103
) 
,  124xl03
) 

(468xl03
(984x 103
(1204xl03

(xorig•Yorig) 

NIA 
,  -324x 103
,  Oxl03
) 
,  132x103
) 

(540x 103
(1008xl03
(1212x103

) 

Table 12B-2.  Offset Definitions for the Tutorial MM5 Nest Domains 

MM5 
Domain 

Resolution  NCOlS 
(km) 

NRCM/S  Col.  Offset  Row  Offset  Col.  Offset  Row  Offset 

01 
D2 
03 
04 

108 
36 
12 
4 

74 
36 
39 
36 

for  nest 

from  Parent from  Parent for  nest 
Domain,  D1  Domain,  D1 
0 
0 
52 
22 
36 
41 
36 
47  26.666666 
36  48.666666  28.111111 

0 
41 
15 
19 

0 
22 
11 
13 

Table 12B-3.  Dimensions of Set of MMS, MCIP, and CTM Domains for Each Grid Resolution 
for the Tutorial Study 

Resolution  MM5NCOLS  MM5 NROWS  MCIP NCOLS  MCIP NROWS  CTM NCOLS  CTMNROWS 

108 
36 
12 
4 

74 
36 
39 
36 

52  NI A-
36 
36 
36 

NIA 

N/A 

N/A 

34 
37 
34 

34 
34 
34 

30 
33 
30 

30 
30 
30 

12B.2Grid  Domain  Definitions  for  the  Models-3  CMAQ  Demonstration  Study 

Table 12B-4. Origins for the CMAQ demonstration MM5 domain, MCIP landuse domain, and 
CTM domains.  Note that (x,y) corresponds to (COL,ROW) in CMAQ.  MCIP landuse domain is 
smaller by one cell around than the original MM5 domain.  Also, the CTM domain removes three 
cells around from the original MM5 domain. Origin is measured from the center of Base  I 08 km 
domain located at 90W and 40N. 

Resolution 

(.1x=L\y) 

108km 
36km 
12km 
1111111111,,.  04km 

",111,,,,' 

llli111111111· 

:11 

Origin for original MMS 

Origin for MCIP Landuse 

Origin for CTM Domain 

Domain 

· (xorig•Yorig) 

(-3996xl03
3
....... ~~ .. 1080x 1,0

,  -2808x103
) 
3
,  -1728.:X: .. 10
) 
(486x103, -216x103
) 
(1 i4ox103
,  24xi03

) 

:::,1111,'1!1111, 

1111111:

1
111111 

11,,11111111 

Domain 

(xorig• Yorig) 

NIA 
,  -1692xl03
,  -Z04x103
) 
,  28x103
) 

(-1044xl03
(480xl03
(1144xl03

) 

(xnric• Ynrig) 

NIA 
(-864x103
,  -1512xl03
) 
,  -144xI03
c.54oxto3
) 
(l 164x103, 48x103
) 

Table  liB-5.  Offset Definitions for the CMAQ Demonstration MM5 Nest Domains 

I 

I 

II' 

" 

11 

.f; 

MMS 
Domain 

Resolution  NCOLS 
(km) 

NRONS  Col.  Offset  Row  Offset  Col.  Offset  Row  Offset 

from  Parent from  Parent for  nest 
Domain,  D1  Domain,  D1 

for  nest 

01 
D2 
03 
04 

108 
36 
12 
4 

74 
87 
102 
87 

0 
52 
81 
27 
84  42.333333 

0 
22 
25 
105  48.555556  27.222222 

0 
27 
43 
56 

0 
10 
42 
20 

1111111111, 

1 , 

11111111111',, 

·Ill 

, 

""" 

, 

, 

" 

, 

,,,, 

-

Tabl~ B6.  Dimensions of Set of MM5, MCIP, and CTM'. Domains for Each Grid Resolution for 
tfie tMAQ Demonstration Study 

· 

Resolution 

MM5NCOLS 

MM5NROWS  MCIPNCOLS 

108 
36 
12 
4 

111111111 

74 
87 
102 
87 

52  NIA 
81 
84 
105 

MCIP NROWS  CTM NCOLS 
NIA 

NIA 

CTMNROWS 
NIA 

85 
100 
85 

79 
82 
103 

75 
90 
75 

69 
72 
93 

1111,1' 

'II' 

Appendix  12C 

Sample  MCIP  Configuration  File 

II  RCS  file,  release,  date  & time  of  last  delta,  author,  state,  [and  locker] 
II  $Header:  lprojectlworklreplMCIPlsrclsunOS51sun_;n3_12.cfg,v  1.1.1.1  1997110113  17:38:23  yoj 
Exp$ 

II  what(l)  key,  module  and  SID;  SCCS  file;  date  and  time  of  last  delta: 
II  @(#)sun_rn3_12.cfg  1.1  lprojectlrnod31MCIPldoclbldrunlsunOSSISCCSls.sun._rn3_12.cfg  14  Jun 
1997  15:41:54 

II  This  is  a  configuration  file  for 

model  mcip_rn3_12; 

11  f77_flags  "-e  -Nl200  -fast  -04"; 

f77_flags 

•-e  -g  -C"; 

11  link_flags  "-fast  -04"; 
link_flags  "-e  -g  -C"; 

libraries 

"-L${M3TOOLS}IIOAPilsrc_liblSunOS5  -lrn3io  \ 
-L${M3TOOLS}lnetCDFISunOSS  -lnetcdf"; 

II  global  {verbose  I  parse_only  I  cornpile_all  I  one_step  I  clean._up}  ... 

global  verbose; 

II  shared  include  files  llllllllllllllllllllllllllllllllllllllllllllllll 

II  Models-3  IIO  API  permanent  includes 

include  SUBST_IOPARMS 
include  SUBST_IOFDESC 
include  SUBsr_IODECL 

$M3TOOLSIIOAPilincludeslPARMS3.EXT; 
$M3TOOLSIIOAPilincludeslFDESC3.EXT; 
$M3TOOLSIIOAPilincludeslIODECL3.EXT; 

II  Models-3  global  constants  for  this  processor 

include  SUBST_CONST 

$M3MODELIICLlsrclfixedlconstlCONsr.EXT; 

II  Models-3  grid  definitions 

include  SUBST_HGRD_ID 
include  SUBST_VGRD_ID 
include  SUBST_COORD_ID 

$M3MODELIICLlsrclgridlD_l2_1SIHGRD_21X21.EXT; 
$M3MODELIICLlsrclgridlD_l2_151VGRD.EXT; 
$M3MODELIICLlsrclgridlD_l2_1SICOORD_21X21.EXT; 

II  Models-3  IIO  API  files  for  this  processor 

include  SUBsr_INFILES 
include  SUBST_FILES 

$M3MODELIMCIPlsrclicllicllINFILES_MCIP.EXT; 
$M3MODELIMCIPlsrclicllicllFILES3_MCIP.EXT; 

include  SUBST_MET_CONST  $M3MODELIMCIPlsrclicllicllCONSTJ11ete.EXT; 
include  SUBsr_PBL_CONST  ~M3MODELIMCIPlsrclicllicllCONST_pbl.EXT; 
include  SUBST.....MPARM 
include  SUBST..J1(l-lMN 
include  SUBST...}l3001' 
include  SUBST...,M:X)UT 
include  SUBSTJIDOUT 
include  SUBST__..LRAI:ff 

$M3MODEL/MCIP/src/icl/icl/MCIPPARM.EXT; 
$M3MODEL/MCIP/src/icl/icl/MCIPCOM.EXT; 
$M3MODEL/MCIP/src/icl/icl/GROUTCOM.EXT; 
$M3MODEL/MCIP/src/icl/icl/MCOUTCOM.EXT; 
$M3MODEL/MCIP/src/icl/icl/MDOUTCOM.EXT; 
$M3MODEL/MCIP/src/icl/icl/LRALMDAT.EXT; 

llllllllllllll///l/l///////l/ll////////l///l/ll//ll/////ll/ll//l/////I// 

:1111111111111,, 

:i1':,, 
1111::111 

I I  the  rncip  driver  class 

module  mcip  developnent; 

II  the  landuse  class 

module  m3radm  develoi;:rnent; 

II  the  input  class 

module  m3  development; 

II  the  met3d  class 

module  m3sup  development; 

,;111111: 

II 

1111111:; 

.11111! 

' '  

illllll: 
~ 111111 : 

' 

II  the  pbl  class 
;1111;,  module  pb~1,J;>,kg  ~evelopnent; 
I l,  the  drydep  ,cl~,,s 

1!1111111, 

:::, 

11

11

',,, 

1  module  radmdd  development; 

1111111

111

II  the  cloud  class 

module  radmkuo  development; 

11  the  util  class 

module  util  development; 

II  the  output  class 

module  stnd  developnent; 

Appendix  12D 

Sample  MCIP  Run  Script 

#!  /bin/csh  -f 

#  RCS  file,  release,  date  & time  of  last  delta,  author,  state,  [and  locker] 
#  $Header:  /project/work/rep/MCI~/src/sunOSS/sun..;n3_12.q,v 1.1.1.1  1997/10/13  17:38:23  yoj 
E>Cp  $ 

#  what(l)  key,  module  and  SID1  secs  file;  date  and  time  of  last  delta: 
#  @(#)sun..;n3_12.q 
08:26:34 

1.2  /project/mod3/MCIP/doc/bldrun/sunOSS/SCCS/s.sun..;n3_12.q  19  Jun  1997 

#  for  Sun  $pare  20,  Ultra$parc  2 
#method:  sun..;n3_12.q  >&I  sun..;n3_12.log  & 

#  QSUB  -r sun..;n3_12 
#  QSUB  -eo  -o  /work/you/your_directory/sun..;n3.log 
#  QSUB  -lM  8MAF 
#  QSUB  -lT  1800 

date;  set  timestanp;  cat  stU\.)113_12.cfg;  echo  • 

•;  set echo 

setenv M3HOME  /home/models3 
setenv M3DATA  $M3HOME/datasets 

#------------------------------------------------------------
#  set  executable 
1------------------------------------------------------------
set  EXF.C  =  mcip_Jl\3_12 

#------------------------------------------------------------
#  set base  directory 
#------------------------------------------------------------
#set  BASE  = /work/you/your_directory 
set BASE  =  $cwd 

#------------------------------------------------------------
#  set working  directories 
#------------------------------------------------------------
set MCIPOIR  =  $BASE 
set  OUTDIR  =  $cwd 

#------------------------------------------------------------
#  set  input  data  directories 
#------------------------------------------------------------
set  DATADIR  =  $M3HOME/datasets/nostudies 

#-------------------------------------------------------------


I  , ..  (METRCM,METCoL,METLAY)  is  (ROWS,  COLS,  LAYS)  for  MM5  data 
1-------------------------------------------------------------
setenv METROW  25 
setenv METCOL  25 
setenv METLAY  15 

·-------------------------------------------------------------
(IO,  JO):  The  left bottom  loc.  of  CI'M  domain  in MET  terms 
• 
In C'IM  terms,  IO  = ROW_OFFSET,  JO  =  COL_ OFFSET 
I 
1-------------------------------------------------------------
&'etenv  IO  1 
setenv JO  l 

'liiii::' 

1-------------------------------------------------------------
1  LUTYPE:  Filetype  for  Landuse  data: 
I 
I 
I 
I 
1-: ... ------------:------------------------------------------------
setenv LUTYPB  4 

1:  BINARY  13  category  directly  from  TERRAIN 
2:  Preprocessed ASCII  11  category 
3 :  Use  MM5  internal  dominant  landuse  category 
4:  Use  M3  landuse  fractions  available  in  GRIDCR02D 

(IOLUSE,  JOLuSE):  The  left bottom  loc.  of  CI'M  domain  in 

1-------------------------------------------------------------
1 
I  ··· 
1-------------------------------------------------------------
setenv  IOLUsEl 
aei:env  JOLUSE  1 

····.LAND ..  USE  DATA  GRID  terms 

If LCALCPBL  is:  TRUE,  Recorrpute  PBL  values 

1---------------------------------------------------------~---
I 
I 
1-------------------------------------------------------------
setenv  LCALCPBL  T 

FALSE,  Read  PBL  values  from  MM52 

If LSLFLUX  is: 

·-------------------------------------------------------------
TRUE,  Use  only  surface  layer  similarity 
I 
t 
FALSE,  Use  PBL  similarity 
1-------------------------------------------------------------
setenv  LSLFLUX  F 

If  LM3DDEP  is: 

·-------------------------------------------------------------
• 
I 
1-------------------------------------------------------------
setenv  I.M3DDEP  F 

TRUE,  Use  Models-3  D.  Dep  routtine 
FALSE,  Use  RADM  D.  Dep.  routine 

·-------------------------------------------------------------
• 

If I.CALCCLD  is:  TRUE:  Recorrpute  CLOUD  values 

12-88 

'1111111 

FALSE:  Read  CLOUD  values  from  MM52 

#-------------------------------------------------------------
setenv  LCALCCLD  T 

EP A/600/R-991030 

If  LSANITY  is: 

#-------------------------------------------------------------
• 
# 
#-------------------------------------------------------------
setenv  LSANITY  T 

TRUE: 
check  range  of  output  parameters 
FALSE:  do  not  check  range  of  paramters 

INITIAL  JULIAN  STUDY  DATE  (YYDDD) 
STARTING  HOUR  OF  .JUDATE  FOR  STUDY 
NUMBER  OF  HOURS  TO  USE  FROM  MM5 

#-------------------------------------------------------------
JUDATE 
# 
#  HSTRT 
#  HTORUN 
#  HTOSKIP:  NUMBER  OF  HOURS  TO  SKIP  FIRST 
#-------------------------------------------------------------
#  88218  00  0  2 
John's  data 
#  88216  12  0  49  I  Dave's  data 
#  88214  12  10  108  !  Daewon's  data 
setenv  .JUDATE  88209 
setenv  HSTRT  00 
setenv  HTOSKIP  00 
setenv  HTORUN  25 
#setenv  HTORUN  108 

input  file  linkage 

#-------------------------------------------------------------
# 
#  MMSl:  Filename  for  MMSl  datafile 
#  MM52:  Filename  for  MM52  datafile  (NA  if none) 
#  LU13:  Filename  for  LU13  datafile  (binary  from  TERRAIN) 
#-------------------------------------------------------------
#setenv MMSl  $INDIR/MM5l_es80 
#setenv MM52  $INDIR/MM52_es80 
#setenv  LU13  $INDIR/LU80 
#setenv  LUSE  $DATADIR/LU11_80 
#setenv  LUSE  /work/bdx/metpp/newmcip/input/LU11_80 

number  of  iterations  for  vertical  wind  correction 

#-------------------------------------------------------------
# 
# 
#-------------------------------------------------------------
setenv  IWIND  0 

(Use  0,  +/- 1,2,3, .. ) 

number  of  deposition  layers  (default  = 1) 

#-------------------------------------------------------------
# 
# 
#---~---------------------------------------------------------
setenv  NDEP  1 

(for  layer  collapsing,  it could  be  2,  or  3) 

·-------------------------------------------------------------
•  GRIW\M:  Grid  Name  (User  Defined) 
1-------------------------------------------------------------
setenv GROOAM  TUTOR12 

111111'""" 

1-------------------------------------------------------------
,:.,, ___________ :..'.,. ___ ,,:.. __________________________________________ _ 
I  EKECU'I'ICN_ID 

(User  Defined) 

setenv EKECU'I'ION_ID  MCIPJlETA 

SCENFILE:  file  path(User  Defined) 

·-------------------------------------------------------------
' 
·-------------------------------------------------------------
lsetenv  SCENFILE 

$BASE/ 

1-------------------------------------------------------------
1  If  IOAPI_CHECKJiEADERS  is:  TRUE,  Check  the  headers 
f 
1-------------------------------------------------------------
setenv  IOAPI.....CHECKJiEADERS  F 
lsetenv LOGFILE  m::ip.log 
, ____________ _: ___ _:,:_ __________________________________________ _ 

FALSE,  Do  NOT  check  headers 

I  Remove  any  previously  generated  output  files 
·-------------------------------------------------------------
1/bin/rm GRID_*  MET_* 
I/bin/rm  •• /output/GRID_*  •• /output/MET_* 

. _________________ :_ _______________________________ _: __________ _ 

f  Set  up  input  files 
·-----------------~-------------------------------------------

11111111,, 

11111 

I 

11111 

illlii1:,,, 

1111 

'1!!

illl' 

~,,tenv . GRID_CR0_2D  $DATADIR' /GRIDCR02D_TUT12 ' 
~:.~tenv GRID.....DOT_2o  $DATADIR • /GRIDOOT2D_TUT12 • 
setenv MET_CR0_2D 
setenv :MET_coo_3ri 
setenv MET_DOT_3D 

$DATADIR' /METCR02D_TUT12 ' 
$DATADIR I  /METCR03D_TUT12  I 
$DATADIR' /METDOT3D_TUT12 ' 

, ____________ _: ____ :_ __________________________________________ _ 

"II', 

f  Set  up  output  files 
·-------------------------------------------------------------

111111111,,' 

111,, 

aetenv  GRIDJIDY_2D_Gl  $Ol1l'DIR' /GRIDBDY2D_tut12 ' 
setenv  GRID_IIDY_3D_Gl  $Ol1l'DIR' /GRIDBDY3D_tutl2 ' 
setenv GRID_CR0_2D_Gl  $0UI'DIR' /GRIOCR02D_tutl2 ' 
setenv GRID_CR0_3D_Gl  $0l1l'DIR'/GRIOCR03D_tutl2' 
aetenv  GRID_DOT_2D_Gl  $0l1l'DIR'/GRIDD0'1'2D_tutl2' 
aetenv MET_IIDY_2D_Gl 
setenv METJlDY_3D_Gl 

$0UTDIR'/METBDY2D_tut12' 
$OUTDIR'/METBDY3D_tut12' 

setenv MET_CR0_2D_Gl 
setenv MET_CR0_3D_Gl 
setenv MET_DOT_3D_Gl 

$0UTDIR'/METCR02D_tutl2' 
$OUTDIR'/METCR03D_tutl2' 
$OUTDIR'/METDCYl'3D_tut12' 

#-------------------------------------------------------------
#  Execute  MCIP 
#-------------------------------------------------------------
#cdbx  "$EXEC  -g  -x";  exit 
#  ja 
#/opt/Su:t-Mspro/bin/debugger  "$MCIPDIR/$EXEC";  exit 
$MCIPDIR/$EXEC 
#  ja  -cshlt 

exit() 
#-------------------------------------------------------------
#  Move  output  to  proper  directory 
#-------------------------------------------------------------
# /bin/rnv  GRID*  .. /outputl/ 
#/bin/rnv  MET* 
.. /outputl/ 

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_Science_Ch_11.md) - [Home](README.md) - [Next Chapter >>](CMAQ_Science_Ch_13.md)<br>
CMAQ Science Document (c) 1999<br>

<!-- END COMMENT -->

