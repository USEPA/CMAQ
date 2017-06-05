<!-- BEGIN COMMENT --> 

[<< Previous Chapter](CMAQ_Science_Ch_03.md) - [Home](README.md) - [Next Chapter >>](CMAQ_Science_Ch_05.md)

<!-- END COMMENT -->

Chapter 4 
=============

EMISSION SUBSYSTEM 

William G. Benjey and James i\'1.  Godowitch

Atmospheric Modeling Division 

National Exposure Research Laboratory 
U.S. Environmental Protection Agency 

Research Triangle Park, NC 27711 

Human Exposure and Atmospheric Sciences Division 

Gerald L. Gipson 

National Exposure Research Laboratory 
U.S. Environmental Protection Agency 

Research Triangle Park, NC 27711 

ABSTRACT 

Chapter 4 provides a description of the Models-3 Emission Processing and Projection System 
(MEPPS) structure, scientific approach and the assumptions used in modeling and processing 
emission data in the Models-3 framework.  The description includes emission data entry through 
the Inventory Data Analyzer (IDA) import and quality control checks; and the data flow and 
quality control used in loading emission inventory and meteorology data in the MEPPS Input 
Processor.  The description of the main Emission Processor addresses the basis of spatial and 
temporal allocation procedures.  The scientific models and assumptions used in modeling hourly 
mobile source and biogenic emissions are explained (Biogenic Emission Inventory System 2 and 
Mobile Sa, respectively).  The rationale and assumptions are described which are used in the 
allocation and grouping of individual chemical species into "lumped species" in preparation for the 
lumped species chemical transformation mechanisms contained in the Community Multiscale Air 
Quality (CMAQ) model.  The chapter also describes the procedures used by the Models-3 
Emission Projection Processor to estimate emission data for use in modeling future air quality 
scenarios.  Finally, the quality control report and output file options contained in the Output 
Processor are described. 

\*on assignment from the National Oceanic and Atmospheric Administration, U.S. Department of Commerce. 
Corresponding author address: Bill Benjey, MD-80. Research Triangle Park, NC 27711.  E-mail: 
benjey@hpcc.epa.gov 

••on assignment from the National Oceanic and Atmospheric Administration, U.S. Department of Commerce. 


## 4.0 EMISSION SUBSYSTEM 

The rationale for the Models-3 air quality emission processor is rooted in the need to estimate, 
organize, and process emission inventory data for regulatory and scientific analysis and modeling. 
Historically, air quality emission processing methods have been developed. in an ad hoc manner, 
with each procedure specifically tuned to meet the need at hand, reflecting the various data 
information sources and estimation methodologies, as well as the uses of the data.  The Models-3 
air quality emission processor consolidates and simplifies the estimation, data handling, and 
linkage of the resulting data to air quality models.  This section describes the purpose, origins, and 
scientific bases of the Models-3 Emission Projection and Processing System (MEPPS).  Material 
is presented in the general sequence of emission processing.  System requirements, system design 
,and detailed user information for emission data processing are not addressed in this Volume. 
These items are described in Volume 7, Volume 8, and Volume 9B, respectively of the Models-3 
documentation set.  For example, processor names, environment variables, and flow diagrams are 
given in Chapter 6 of the Models-3  Volume 9B:  User Manual. 

## 4.1 Emission Inventory Processors 

In the Models-3 framework, MEPPS imports, quality controls, and processes emission inventory 
data for either direct regulatory analysis or input to a chemical transport model.  The MEPPS also 
models  hourly biogenic emissions and mobile source emissions.  The MEPPS is non-conforming 
within the framework because it is not an object-oriented component, and because it does not use 
the NetCDF Input/Output Applications Program Interface (I/O API) format internally. 
Processing is accomplished using a combination of FORTRAN, SAS®, and ARC/INFO® 
programs. The emission processor results are translated into the NetCDF I/O API format by the 
MEPPS Output Processor. 

### 4.1.1  Discussion 

The MEPPS builds on lessons from and functionalities of previous software developed for 
processing emission inventories.  The capabilities and design decisions in MEPPS are placed in the 
context of emission inventory developments. 

#### 4.1.1.1 The Role of Emission Inventory Processing for Chemical Transport Modeling 

Historically, there have been many air quality emission data bases which were compiled and used 
for the purposes of regulatory or scientific assessment of emissions, including spatial and temporal 
patterns and trends.  As air quality chemical transport models (CTMs) were developed, each had 
its own specific  format for input data, based either on the data structure used in the model and/or 
the format of the available data.  Initially, CTMs were relatively simple and applied to one or a 
few point sources, such as emissions from an industrial stack.  As CTMs for urban and regional 
scales - such as the Urban Airshed Model (Morris et al., 1992) - became more sophisticated, the 
level of detail needed for input emission data became increasingly more comprehensive and 
detailed.  It became necessary to include as complete an inventory as possible for the area of 
interest, including emissions from all diffuse as well as point sources at as great a degree of spatial 
and temporal resolution as possible.  As the spatial and temporal resolution of the models has 
increased, so has the demand for detailed emission input data.  Because of resource limitations on 
data gathering, the emission inventory information needed often substantially exceed the reported 
or observed emission data available.  The result is development of a wide range of emission 
estimation and modeling techniques. 

Emission inventory data are available from various sources, often state and local air pollution 
control agencies.  The data are commonly compiled into annual emission inventories for specific 
areas to be analyzed and/or modeled.  The spatial extent of an emission inventory may vary from 
plant-specific emission data to data for an entire country or more.  Recent regional exampl~s 
include the National Acid Precipitation Assessment Program 1985 National inventory (Saeger et 
al.,1989), the 1990 EPA Interim National Emission Inventory (U.S. EPA, 1993; U.S. EPA, 
1994), and the 1990 Ozone Transport Assessment Group inventory (Ozone Transport 
Assessment Group, 1997). Most emission inventories are organized into four traditional general 
groups of emission types: 

Point sources, which are emission sources attributable to discrete emission points, usually 
a stack.  The data include pollutant, source category code, stack parameters (height, 
diameter, exit velocity, temperature, flow rate), emissions, location coordinates, fuel, etc. 

Area sources, which are emission sources attributable to diffuse sources or areas, such as 
agricultural fields, large open mining operations, forests, or a combination of many point 
sources which are too small and numerous to account for individually (e.g., .residences). 
Area somce inventories are typically by county and include pollutant, source category 
code, emissions, location, coordinates, etc. 

Biogenic sources, which are often natural emissions from vegetation, soils, and lightning. 
Biogenic emissions are dependent on temperature, solar radiation, and land cover type. 
They are usually modeled hourly for specific days and locations. 

Mobile sources, which are emissions from vehicular traffic on roadways, aircraft, trains, 
shipping, and off-road mobile equipment.  Mobile source emissions are dependent on the 
ambient temperature, road type, vehicle type and age, miles traveled, etc.  These emissions 
are generally modeled for specific day and meteorology scenarios either by county or road 
segment (link-node data). 

The techniques used to estimate emissions in compiling an emission inventory are based on 
extrapolation of limited direct measurements for point sources, and application of limited 
measurements or estimates to spatial surrogate data for area sources.  Emissions which are 
dependent on environmental conditions (e.g., biogenic and mobile source emissions) are modeled 
to generate either portions of emission inventories or hourly data for direct use in air quality 
modeling.  The procedures used to process emission inventory data and to model and process 
mobile and biogenic source emission data are described in more detail in the following sections. 

Point source and area source emission inventory data are usually included in annual emission 
inventories.  Annual mobile and biogenic emissions are included ifthe inventory was intended for 
assessing annual totals or trends of emissions.  If the inventory was prepared for modeling, only 
the annual "Vehicle Miles Traveled" by county or road segment and vegetation land cover may be 
provided.  In order to accomplish episodic air quality modeling, it is necessary to model the hourly 
mobile and biogenic emissions for the episode-specific meteorological conditions. 

The typical procedures used to prepare annual emission inventories for use in a CTM require 
temporal, spatial, and pollutant species allocation of the data.  This is accomplished in a sequential 
manner.  For regional modeling, initial "raw" emission inventory data files are very large, often 
several megabytes in size.  The data files are subjected to a variety of data quality checks, 
depending upon the methodological sophistication and computing and time resources available. 
Typically, visual inspections of mapped locations, value range checks, cross-checks of sums, and 
routine computer checks for blank fields and valid data types are performed. 

In order to reduce the size of emission data files, the data are often speciated first, depending 
upon the pollutants involved, then temporally allocated to hourly data, and then spatially allocated 
or "gridded" to a spatial domain with gridded cells of the resolution required by the CTM. At each 
step some of the detailed information, such as source category code and geographic coordinates, 
are dropped to reduce the file size.  Hourly mobile source and biogenic emission data are modeled 
using the appropriate hourly meteorological data, and merged with point and area source data 
prior to speciation. 

It is necessary to rerun the processing sequence in the event of an error, or for each new 
meteorological or day-specific scenario.  Projections to future years require application of source(cid:173)
category-specific economic growth  factors to a base year inventory to produce a projected 
annual inventory.  The projected inventory is then processed through the entire emission 
processing sequence.  Preparation of detailed regional emission inventory data for regional 
modeling using this traditional approach may take weeks or month. 

#### 4.1.l.2 

Function and Place of the Emission Processor in the Models-3 Framework 

The Models-3 framework is designed to contain conforming object-oriented modules that pass 
data in the NetCDF I/O API format, although non-conforming modules are accommodated at the 
cost ofreduced functionality.  The emission processing system is intended to  ultimately be 
conforming.  The initial version ofMEPPS was derived from existing software and consequently 
is not a conforming object-oriented program in Models-3.  However, it is integrated into the 
Models-3 framework and does take instructions and information from the system and provides 
emission data output in NetCDF IO/API format.  Figure 4-1  provides a functional view of the 
Models-3 system.  Processing of emission data is often a part of iterative air quality strategy 
building, directly or indirectly, though the effect of emissions on air quality modeling results. 
Figure 4-2 is a simplified illustration of the location ofMEPPS and other non-conforming 
components within the Models-3 framework.  The main aspects of a study (including the principal 
emission processing options) are graphically defined in the high-level Models-3  Study Planner, 
and the resulting information is automatically passed to MEPPS as well as other portions of the 
system.  Specifically, a study name is defined by the user by copying, modifying, and renaming an 
existing "template" emission study.  For example, a tutorial emission study could be used as a 
template to define a new study.  The new study is associated with input data sources, MEPPS 
processing and modeling modules and their primary options by annotating links and nodes in the 
specified emission study and plans.  The pollutants of interest, the geographic domain, map 
projection, grid spatial resolution, chemical speciation mechanism, temporal resolution, and dates 
of interest (referred to as the "case") are defined in the Models-3 Science Manager and passed to 
the rest of the system including MEPPS.  This approach allows the user to make primary 
specifications once, rather than separately to each of the system components and thus ensures 
consistency.  Study Planner and Science Manager also aid in using the system to run multiple 
emission, projected emission, and emission control scenarios without frequent respecification of 
parameters.  From a functional view, MEPPS generates emission data that reflect user-defined 
studies and cases, whether for regulatory analysis of emission data and/or for input to CTM runs 
and comparison with CTM results. 

Main Control Subsystem 

(Study Planner) 

Control 

Model 
Building 

I 
~Litalysis 
I 

Functionality 

(  Models 

] 

~ 

Building 
Blocks 

Figure 4-1  Functional View of the Models-3 System 

I 

Strategy 
Building 

I 

I 

(  Data  ] 

Meteorology 

(MMS) 

E~$.i~~~~b.1~1: 
~:l?f~essbt!:1: 
:::™·:·:··EE·: 
.. :·p·:S 
... ) .. :::: 
.  .... . 
. . . 
... 
.... . 
. 
. .  . . . . . . .  . . 
························ 

Data Flow 

Models-3 Interfaces 

System Instructions 

Analysis ToD 

Figure 4-2  The location ofMEPPS within the Models-3 Framework 

The emission processing function ofMEPPS requires that it be linked to the Models-3 
framework, anci to other processing, modeling, and analysis tools through the framework. 
MEPPS outputs data in the NetCDF I/O API format in order to link MEPPS to the Models-3 
framework and other processing, modeling, and analysis modules.  Figure 4-3 illustrates that 
information (grid, chemical mechanism, and case) user-specified in Models-3  Science Manager for 
a study is passed to other modules, including MEPPS.  However, it is necessary to access the 
Parameter Window ofMEPPS through the Tools Manager, to specific study name, grid, source 
(data source name), and case, and then return to the Study Planner to run the emission study. 
Direct access to MEPPS is through the Tools Manager and the emission projection and control 
functions are in Strategy Manager.  New emission inventory data are imported and quality 
controlled through the Models-3 File Converter and its subsidiary Inventory Data Analyzer 
(IDA).  The data are then imported directly into MEPPS through its internal Input Processor 
(INPRO). Meteorological information needed to estimate biogenic and mobile source emissions 
are provided to MEPPS from a meteorological model or dataset in the Models-3 system through 
the Meteorology-Chemistry Interface Processor (MCIP).  For regional-scale modeling, the 
current default meteorological model is Mesoscale Model 5 (MM5).  The MEPPS processes 
emission data into speciated, spatially and temporally allocated hourly emission data for use in a 
Models-3 conforming CTM.  The data are output as NetCDF I/O API files to the Emission(cid:173)
Chemistry Interface Processor (ECIP) which adjusts the format to that needed by the CTM. 
Emission data for large elevated point sources (large stack emissions) are also provided as I/O 
API files to the Plume Dynamics Module, and the plume rise algorithm. 

Tools Manager 

·Study Planner 

MMS 

CM:AQ 

MCIP  1--------~ 

Science 
Manager 

.I 

(1~A\*) 

ECIP 

Where: 

:Ml.v.1:5  is Mesoscale Model 5 
C11AQ is  Community Multi scale Air Quality Model 
l:v.IBPPS is Models-3 Emission Processing and Projection System 
MCIP is  Meteorology-Chemistry Interface Processor 
ECIP is Emissions-Chemistry Interface Process or 
\*ID Ai s Inventory Data Analyzer 
\*File Converter is  generic to the Models-3 system 
\*IDA and File Converter are optional  (used for new data) 

Figure 4-3 Relationship of Principal Models-3 Framework Components 

#### 4.1.1.3 

Rationale for the Basis of the Initial Version of MEPPS 

In order to assure that an acceptable and functional emission inventory processor would be 
present in the initial release of Models-3  given time and resources available, it was decided that 
the emission processing module for the initial version would be based on software and 
methodologies available as of 1994.  More advanced approaches being researched and developed 
then and now will be incorporated as soon as possible.  The decision factors used in selecting 
from existing emission pro.cessors were availability, cost, features (capabilities), hardware and 
software requirements, and operating characteristics.  Table 4-1  lists the emission processing 
systems considered with respect to the principal evaluation factors. 

Based on the evaluation, the Geocoded Emission Modeling and Projection. System (GEMAP; 
Wilkinson et al.,1994), now known as the Emission Modeling System-95 r(EMS-95), was selected 
as the basis of the initial emission processing system, although it is non-conforming (not in I/O 
API Net CDF) in the Models-3 system.  The GEMAP was selected because it was readily 
available in the public domain; it was state-of-the-art at the time; there wa'3 no licensing cost 
(exclusive ofGEMAP's internal use of SAS® and Arc/Info®); its code was modular and flexible; 
and it contained a geographic information system to perform spatial allocation of emission 
inventory data. 

Other systems considered included the Emission Processing System (EPS), The Flexible Regional 
Emission Data System (FREDS), and the Sparse Matrix Operator Kernel Emission System 
(SMOKE).  The EPS is used to process emission inventory data for the Urban Airshed Model 
(UAM; U.S. EPA, 1992).  A recent adaptation ofEPS by Environment Canada for regional use is 
called the Canadian Emission Processing System Version 1 (CEPS 1.0) (Moran et al.,  1998).  Like 
GEMAP, EPS would be non-conforming code in the Models-3 framework, and would require 
design and development of interfaces with the Models-3 system and the addition of many features. 
However, EPS does not include flexible GIS-based gridding capability of GEMAP and was 
originally  designed for urban scale air quality modeling as opposed to the multiscale (local to 
regional and national) air quality modeling capabilities ofModels-3. 

The FREDS has been used for regional air quality modeling during the past ten years in 
conjunction with the Regional Oxidant Model and Regional Acid Deposition Model (Modica et 
al.,  1989).  The FREDS code is run on a main-frame computer, and is "hard-wired" and difficult 
to change for different scenarios or grids. Consequently it lacks the modulmity and flexibility 
needed to operate in the Model-3 framework. 

Design and prototyping of SMOKE was just beginning when the design of the initial version of 
Models-3 was determined (Coats,  1995).  The SMOKE, which is now being used in conjunction 
with UAM, could be modified to be fully compliant with the object-oriented Models-3 system. 
Additional analysis and quality control tools are being designed for SMOKE, and initial work has 
begun to adapt it to the Models-3 framework. 

Table 4-1.  Emission Processor Selection Factors for the Initial Public Release ofModels-3 


Facfors···· 

E@$$ioll J>ro~~ssh1g.. 
Systeni ~P~) •' •·. ·· ·,:· ····  ·  \ 

Acquisition Cost of Source 
Code1 

II  None 
·I NIA 

I None 

I None 

Availability 

Public domain 

Degree of Development 

Completed and in use 

Public domain 
I ~ompleted and in use 

Public qomain 
I Initial version being tested 

I NIA 
I Design and prototype· 
beginning 

Relative Flexibility for 
Modification to Models-3 

Hardware Requirements 

History (Operating 
Experience) 

II  Changes required recoding of 
modules to allow revised 
interfac(!s and external control 
- spatial allocation would be 
added. 

Changes required substantial 
recoding - many patches -
relatively inflexible -spatial 
allocation would be added 

II  Work station with UNIX 

operating system 
II  Established system used by
EPA, state, and local agencies·  U.S. EPA m regional 

I IBM Mainframe system 
I Establishe~ syst~m used by 
modeling program·s 

Changes required recoding of  NIA 
modules to allow revised 
interfaces and external control  · 
- spatial allocation already 
present 

Work station with UNIX 
operation system 

I New system, used only with 

test data in California 

Unix-based m~chines, 
specifics not established 

I NIA 

Software Requirements 

II  FORTRAN and SAS® 

. I FORTRAN and SAS® 

Spatial Allocation Capability 

II  Spatial allocation grids must 
be manually coded for each 
allocation scenario. 

Spatial allocation grids must 
be manually coded for each 
allocation scenario. 

SAS® and ARC/INFO® 
licenses. 

Unix-based, specifics.not well 
established 

ARC/INFO® allows flexible 
user-defined grid domains, 
spatial resolution and data 
overlays 

Unknown 

Since its selection, GEMAP/EMS-95 has been substantially modified and incorporated into 
MEPPS.  Because of the changes, the GEMAP portion ofMEPPS has been renamed the 
Emission Processor (EMPRO) module.  Revisions were made to allow the software to be more 
generic and efficient in its operation, and new features were added.  In general, new capabilities 
·were written in FORTRAN and placed in modules outside ofGEMAP.  This was done to 
minimize rewriting of code in future when the GEMAP-based portion of the system is replaced. 

MEPPS must include several basic functions.  There are also many enhancements, detailed 
descriptions for which may be found in Volumes 7, 8, and 9 (System Requirements, System 
Design, and User Manual, respectively) of the Models-3 documentation set.  The experience and 
design suggestions of personnel familiar with processing large emission inventories were used as 
important guidance in deciding which features to include in MEPPS.  The design and 
implementation ofMEPPS emphasized ease of use and efficiency within the overall design of the 
Models-3 system.  The following paragraphs present a brief description o:fthe principal functions 
included in MEPPS. 

• 

• 

• 

• 

• 

• 

Annual point and area source emission data from inventories are subset to the spatial 
domain of interest, spatially allocated to a grid, and temporally allocated.  Area source 
emissions are spatially allocated using surrogate spatial coverages. 

Spatial allocation of each general type of air quality data (point, etc.) to grid cells is done 
by the GIS using a grid defined by the Science Manager module ofModels-3. 

Modeled estimates of mobile source emissions are prepared on an hourly basis for periods 
of interest, usually several days.  These estimates account for meteorological conditions 
using data from a meteorology model (such as MM5) that has been passed through the 
Models-3 meteorology-chemistry interface processor (MCIP).  Hourly, grid-cell specific 
emission factors for different vehicle and roadway classifications are prepared with MM5 
using vehicle miles traveled (VMT) data from an emission inventory. 

Modeled hourly estimates ofbiogenic source emissions are prepared on an hourly basis for 
periods of interest, using the Biogenic Emission Inventory System Version 2 (BEIS-2) and 
meteorology data from MCIP and land use coverages (see Section 4.2.4.1). 

Temporal allocation of emission data to hourly data for the period of interest is 
accomplished using source-type specific defaults or user-selected temporal allocation 
profiles. 

Disaggregation of gridded, temporally-allocated emission data of groups of chemical 
species to specific species (chemical speciation) is completed according to the users choice 
of chemical speciation mechanism (currently Carbon Bond 4 (CB-4) and RADM 2.0). 

Projection of emission inventory data to future years from a base-year inventory (1990) 
and application of controls is performed by the Models-3 Emission Projection Processor 
(MEPRO).  Projected emission inventories are used to iteratively evaluate different 
emission scenarios caused by economic or emission control changes. Projected emission 
inventories are each processed by MEPPS for air quality modeling in the same way as the 
original, or "base" inventory; 

Merging of spatially and temporally-allocated, speciated files for point, area, mobile, and 
bjogenic emission data into one emission data output file, and translation into NetCDF 
1/0 API format are performed for use in the Models-3 system, including the chemical(cid:173)
transport model.  Summary and quality control reports on the output data are also 
produced. 

User-defined point-source emission data are extracted and prepared for use in vertically(cid:173)
layered (three-dimensional) emission files to be used with the plume dynamics module of 
the Models-3 system. 

### 4.1.2  General MEPPS Structure 

The MEPPS is normally used as an integral part of the Models-3 framework from high-level 
menu-driven screens and pick-lists. 

MEPPS in the Models-3 Framework 

A user may define a study using the Study Planner within Models-3.  The Study Planner  specifies 
the name and description of a study, and defines data sources, models, and the relationship 
between them using a graphical interface.  If a study includes processing of emission data, the 
sequence of processing operations, source file addresses and many options for processing are 
defined by the Study Planner and passed to MEPPS annotated to the study name.  Specifications 
for spatial allocation grids, and definition of specific temporal cases are defined in the Science 
Manager.  Existing studies, grids, work space directories, speciation mechanisms, source 
directories, and computer hosts must be selected from the MEPPS parameter window located 
through the MEPPS icon in the Tools Manager prior to running under the Study Planner.  The 
MEPPS may also be directly accessed through the Tools Manager and run interactively via its 
SAS® interface windows.  During direct access interactive operation, some features may be run 
individually in the background using an· "interactive" batch mode selection, or a series of 
processes may be run together as a concatenated batch run. 

The MEPPS shares an import File Converter with the rest of the Models-3 framework.  The File 
Converter will import any ASCII, SAS®, or NetCDF 1/0 API file of format known to the user. 
SAS® data sets are used within EMPRO. The data are imported, converted, and subjected to basic 
quality control checks, including missing or out-of-range values.  The data are then put through 
the Inventory Data Analyzer (IDA) for quality control and analysis specific to emission files, 
including inventories (in one of four EPA formats), temporal allocation factors, control factors, 
vehicle miles traveled, and continuous emission monitoring (CEM) data.  The data then go to the 
MEPPS Input Processor (INPRO) for final quality contr.ol and loading into EMPRO. 

The Internal Components ofMEPPS 

The emission processor (MEPPS) includes several basic components (Figure 4-4):  an em1ss1on 
data Input Processor (INPRO), a main Emission Processor (EMPRO), an Output Processor 
(OUTPRO), and a Models-3 Emission Projection Processor (MEPRO). The File Converter and 
IDA are not intrinsic parts of MEPPS.  They are used to import, quality control, and convert the 
formats and units of data files of known format to the formats that are used in the Models-3 
system, including emission inventories and related emission data files.  The INPRO imports 
emission inventory data from IDA and meteorology files from MCIP, and prepares them for use in 
EMPRO. The EMPRO spatially and temporally allocates point and area source emission data to· 
hourly gridded data.  It is also used to model biogenic and mobile-source emission data using 
meteorological data generated by MMS and processed by MCIP, as well as spatially and 
temporally allocates the data.  The EMPRO then allocates (groups) the chemical species from the 
gridded temporally allocated emission files according to the user's selection of a chemical 
speciation mechanism.  The speciated files then go to OUTPRO where they are merged (a two(cid:173)
dimensional file).  Quality control is performed and summary reports are prepared in accordance 
with the user's choices.  The user also may divide point sources into categories to separate very 
large sources (major-elevated point sources or MEPSE) and large sources (major sources) into 
separate output files, which may be placed in a three-dimensional emission output files to be 
allocated to vertical layers for CMAQ.  The user defines what is MEPSE or major using a 
combination of pollutant-specific emission values and/or stack parameters (see Section 4.2.6). 
Remaining point sources are merged into the two-dimensional emission output file along with 
area, mobile, and biogenic source emission data.  Output files are in NetCDF I/O API format.  The 
MEPRO module projects emission inventory data to future years while applying controls.  The 
projected emission data can be read by EMPRO for further processing if modeling of a projected 
case is desired 

## 4.2 The MEPPS Emission ,Processing System 

Although MEPPS is a significant advance in emission inventory data processing, it is continually 
being improved.  The following sections describe the scientific rationale for the 1998 (first public 
release) version of the system. 

· 

This section describes the basis for emission processing procedures in MEPPS.  For models 
developed independently of the Models-3 framework and incorporated into MEPPS, 
documentation for those models is referenced.  MEPPS was developed to meet emission 
processing needs specified in Models-3  Volume  7:  Design Requirements of the Models-3 
docun1entation set, as amended.  The design requirements for emission inventory processing are 
based on the emission data input requirements of chemical transport models, and on the needs of 

MCIP 

I 

[ 

Session 

Jl  ( 

Session 

Speciate 

INPRO  ~I  Point Sources 

II 

II 

Visualize 

(  Ar01 s~ces  J ~ 

Mcx:lels 

(Mobile Source Da3 

( 

Grid 

INPRO 

I 

I  I 

)~ 

t-' 
I  n 

RADM-2 

CB-IV 

Generic 

Point 

Area. 

Bi(f;eniC 

) 
(  EpiscxieFile  J 
) 
( 
) 
I ho.I  OUTPRO  M  (  sec Reports  ) 
) 

Stack Files 

( 

Reports 

f 

......._ 

_..,,. 

~ 

I -w 

l(  Mobile  J 

I 

I 

EMPRO 

(remporalized Files) 

(  Speciated Files  ) 

OUTPRO 

(  Point Sources  JI  rl  Proiected 

EmimonData 

MEPRO  ~( Area Sources 

I  >I 

Reports 

)1 

(Mobile SourceDa31  ~ Graphs 

MEPRO 

Figure 4-4  Functional Components of MEPPS 

~ 0 

0 

\0 

~· 
~ w 

0 

those who will use MEPPS to process emission data for both regulatory and scientific emission 
analysis as well as CMAQ data input.  Two guiding principles were followed in the design and 
implementation of MEPPS.  First, substantive changes to the existing GEMAP system were 
minimized because of a long-term goal to replace GEMAP with a more flexible, less costly (in 
terms of software licenses), and faster processor.  Changes focused on improving the processing 
efficiency and correcting basic inadequacies of GEMAP.  The second principle was to place new 
features and new emission processing developments outside of GEMAP to the extent possible, but 
within MEPPS.  The resulting structure will make it easier to replace the GEMAP-based portion 
of MEPPS in the future. 

MEPPS is usually run from the Models-3  Study Planner, although it can be run independently in 
interactive mode.  Consequently, MEPPS is configured to run in a "batch" mode (from Study 
Planner for multi-day cases), and to accept Study Planner specifications for emission data 
processing and location of data sources.  Some MEPPS processors that may be run when 
accessing MEPPS through the Tools Manager (such as file,  grid, and case creation, deletion, or 
editing), will not function when MEPPS is accessed through the Study Planner.  This provision 
avoids conflicts when these processes are specified from the Models-3 framework under Science 
Manager.  The main components of MEPPS are each discussed in the following sections. 

4.2.1  The Inventory Data Analyzer (IDA) 

The IDA is an important enhancement to Models-3.  It operates as an adjunct to the generic File 
Converter, which is described in Section 10.8 of the Models-3 User Manual (Volume 9b of the 
Models-3 documentation set).  The File Converter is fully-compliant component ofModels-3, 
applicable to the entire system, and provides file format and unit conversion for files imported into 
or exported from Models-3, including emission related files.  The formats  currently used in 
Models-3 and supported by the File Converter are NetCDF I/O API (gridded data), ASCII, and 
SAS®. 

The IDA imports files from the File Converter and provides quality control ·and analysis tools 
specialized for reading, comparing, editing, and analyses of emission data .  Quality control reports 
are generated including the results of the checks.  Emission inventories that are in terms of annual, 
daily, or hourly emissions can be imported.  The functionality ofIDA is fully described in Section 
6.2 of the User Manual.  Specific quality control functions include the following: 

• 

Range checks.  Ranges accepted by IDA were taken from the range of values known to be 
correct in inventories currently in the system (1985,  1990, 1995).  English units are used 
because most U.S. inventories are submitted with those units.  Values outside of the 
validity ranges are considered incorrect.  The validity ranges currently used in IDA for 
point-source stacks are: 

4-14 

Diameter 
Diameter 
Height 
Temperature 
Velocity 
Flow rate 
Flow rate must not be more than 10 percent different from flow 

>0.5 and < 50 ft. 
< 0.2 \*Height 
>1.0 and <1,250 ft. 
>50 and <2,000°F 
> 1 and <1000 ft/sec 
> 1 and < 30,000,000 :ft3 /sec and/or 

rate calculated from Velocity and Diameter. 

• 

• 

• 

• 

• 

• 

Sign checks.  The system reports suspect signs based on the knowledge that particular 
parameters are always of a given sign (e.g., stack height are positive). 

Missing data checks. The system automatically fills missing data with Source Category 
Code-specific default data generated from the inventory being processed. 

Incorrect data checks.  Data are corrected automatically when they can be computed 
based on specific mathematical relations between data elements (such as exit velocity for 
stack parameters). 

Completeness cross-checks are performed between the plant, stack, process, and emission 
hierarchy levels of point source inventories.  Each level should be associated with data at 
the next lower and higher level.  For example process data should be subsidiary to 
corresponding stack information, and superior to associated emission data.  Specific 
quality control checks are provided for each level of point, area, and mobile source 
inventories. 

Location checks, which are applied to determine that the specified political unit (e.g., 
county) is valid in each case, and that latitude and longitude coordinates (for point  . 
sources) are not reversed or missing.  Political unit identifiers are checked against a list 
within IDA.  When point-source latitude and longitude coordinates are missing, they are 
currently assigned to the pseudo-center of the appropriate county.  The missing values are 
usually associated with smaller point sources.  This assignment procedure can result in 
over-representation at the county center.  Work is planned to better distribute point 
sources reported without geographic 'coordinates. 

Some emission inventories do not include sulfate (S04)  emission data, which are important 
in modeling of particulate matter.  The IDA can approximate S04  emission data by 
multiplying the source-category specific ratios of S04 to sulfur dioxide (S02)  taken from 
the 1985 National Acid Precipitation Assessment Program (NAPAP)  inventory, with S02 
values in another inventory.  The NAPAP inventory is considered to contain the best 
separate national modeling estimates of both 802  and S04  emissions at this time.  If an 
inventory contains 804  data, this approximation feature is not used.  The S04  estimator is 
optional in IDA.  However, it is applied automatically in INPRO if no S04  values are 
present.  With this method, S04  estimates were supplied forthe 1990 and 1995 U.S. 
inventories included with the initial Models-3 release. 

Occasionally an emission inventory contains an error in particulate matter emission values 
such that the fine fraction (PM25)  emissions exceed the value of the coarser fraction 
emissions (PM10)  of which they are a part.  In the absence of more specific data, or 
information that the coarser particulate emission (PM 10)  are in error,  IDA automatically 
computes a new PM2.5 value using the 1990 national inventory ratio of PM25 to PM 10 , 
which is expressed as PM2.5 = .2411  PM10• 

Point source stack emission parameters are essential to plume rise calculations in 
modeling.  However, the stack parameters often have erroneous or missing values in 
emission inventories.  In addition to range checks,  IDA addresses the problem in two 
ways: 

(1) IDA examines point source files for consistency between stack flow rate, 
velocity, and diameter.  The following relationship is used to correct erroneous 
velocity and flow  values: 

F =  V * 0.785398 * D2 

where: 
F is flow (cubic feet/second) 
Vis velocity (feet/second) 
D is stack diameter (feet) 

(4-1) 

(2) In the event that all or most stack parameters are missing for a given point 
source, IDA supplies default values which are computed by SCC from other point 
sources in the same emission inventory. 

4.2.2  The MEPPS Input Processor (INPRO) 

The INPRO includes the MEPPS processors used to establish an emission study, the grid 
directory structure, and time-specific cases.  These items are usually defined using the Models-3 
Science Manager, specified through the Study Planner in the Models-3 framework, and passed to 
INPRO, unless MEPPS is being operated through the Tools Manager. 

Import and Processing of Emission Inventories 

The INPRO serves as the principal data access point for MEPPS.  Two primary sets of data 
manipulations are accomplished in INPRO to prepare the data for further study-specific 
processing in MEPPS: 

The emission inventory data are subsetted and imported for the grid specified in the main 
MEPPS window and (if necessary) created in Science Manager.  The emission data are 
subset from the user-selected emission inventory for the kinds of emission sources (point, 
area, mobile) specified by the user.  The INPRO accepts inventories from IDA in the NET 
or IDA inventory formats (see Section 10.8 of the User Manual).  The user must be sure 
that any aggregated (not hourly) data in the inventory for biogenic and mobile source 
emissions (other than vehicle miles traveled) are deleted from the inventory before use. 
Otherwise, MEPPS will double-count these emissions because hourly biogenic and mobile 
source emissions are modeled during processing. 

The INPRO imports meteorological data specific to the spatial domain and temporal case 
defined in Science Manager and selected in the MEPPS main window, provided that the 
meteorological data have been previously generated.  The data come from the Mesoscale 
· Meteorology Model 5 (MM5), via MCIP.  The MCIP converts MM5 output to NetCDF 

IO/API files containing information needed by the rest of the Models-3 system (see 
Section 7.3  ofthis volume and Section 2.3.1  of the User Manual).  In MEPPS, 
meteorology data are needed to model hourly biogenic and mobile source emissions 
(Sections 4.2.4.1  and 4.2.4.2).  The MEPPS uses four of the files provided by MCIP, 
named MET\_CR0\_2D\_xx, MET\_CR0\_3D\_xx, GRID_DOT_2D_xx, and 
MET_DOT_3D_xx.  The terms "CRO" and "DOT" refer to the cross and dot points of 
grids, respectively, and the suffixes "xx" are study specific identifiers.  Refer to Chapter 12 
for the details of the grid system definitions. 

ROG-to-TOG Adiustment 

Emission inventories may reflect different assumptions or methodologies with respect to volatile 
organic compound (VOC) gases.  They may be reported as VOC, reactive organic gases (ROG) 
or total organic gases (TOG).  It is typical to require TOG for air quality modeling, whereas the 
VOCs comprising organic gases in many regional inventories are reported as ROG.  This occurs 
because some emission measurement techniques do not capture all of the discrete hydrocarbon 
compounds in the emission stream.  Since the hydrocarbon speciation profiles are based on total 
organic compounds, the measured value of the hydrocarbon must be adjusted to account for the 
missing hydrocarbon components for the emission measurements that fail to capture the total 
organic emission stream.  This adjustment is included as an option when importing an inventory 
using INPRO.  The default is "yes" - perform the adjustment.  More details are given in the 
speciation discussion in Section 4.2.5. 

### 4.2.3  Processing Procedure 

A detailed description of the processing procedure is given in Chapter 6 of the Models-3  Volume 
9B:  User Manual.  Briefly, EMPRO receives point- and area-source emission data from INPRO 
for use in biogenic- and mobile-source emission modeling.  The general sequence of processing 
for all of the emission data is (1) spatial allocation, (2) temporal allocation, and (3) chemical 
speciation.  There are some variations in processing between the general types (point, area, 
biogenic, mobile) of emission data because of the spatial and temporal resolution available and 
method of estimation.  The gridded, temporally allocated and speciated data are passed to 
OUTPRO where they are merged into output files in the NetCDF VO API format. 

#### 4.2.3.1 Spatial Allocation of Emission Data 

The emission inventory data are usually provided by discrete point sources or by political units 
(typically counties in the United States).  In order to prepare the  data for use in air quality models 
and for analysis by grid-oriented visualization tools such as PA VE, the data must be spatially 
allocated (gridded) on a map projection and rectangular grid.  The map coordinate system, grid 
position and spatial resolution are defined from windows in the Models-3 :framework Science 
Manager (Sections 7.1  and 7.2 of the Models-3  Volume 9B:  User Manual), although the actual 
grid creation is performed by the ARC/Info program.  The MEPPS is capable of using Lambert 
Conformal, Mercator, Albers, and Universal Transverse Mercator (UTM) map projections; and 
several datums including perfect sphere, NAD83, and Clarke.  However, in order to maintain 
consistency with gridded MM5 meteorology data, a spherical datum must be used.  The radius 
used for Models-3 applications is 6370.997 kilometers. 

Aside from the spherical datum restriction, the gridding processor allows development of a variety 
of emission grid systems which are used in different aspects of air quality modeling studies, 
including emission processing and in CMAQ.  The gridding processor is written in the ARC® 
Macro Language (AML®) of the Arc/Info® geographic information system (GIS).  Grids are used 
in MEPPS to spatially disaggregate or aggregate emission estimates, using land use/land cover 
data, and area source spatial surrogate data.  Surrogate data are needed to spatially distribute 
emission data because the exact location of emission sources totaled for a county, for example, 
are seldom known.  For example, census tract population data are of higher spatial resolution than 
county boundaries, and therefore population distribution could be assumed to be directly 
proportional to the spatial distribution of dry cleaning establishments.  Land use cover data, 
detailed road network maps, and many other kinds of spatial data may be used as spatial 
surrogates to locate different source types of emissions, if GIS coverages of these data are 
available.  At this time MEPPS is provided with GIS coverages for political boundaries and land 
cover for North America, and detailed population data and road networks for the United States. 

The primary input to the gridding processor consists of user-supplied data that are entered into 
windows or chosen from a pick-list in the Coordinate System and Grid windows of the Models-3 
Science Manager. These data define the grid projection system, the origin of the grid, cell size, 
etc., which is then generated by Arc/Info® through MEPPS.  The primary output of the gridding 
processor for MEPPS is an ARC® coverage which consists of the intersection of the political 
boundaries and the fixed grid. 

Use of the Arc/Info® GIS as the key component in the gridding processor allows substantial 
flexibility in the definition and use of grids and surrogate data. It also allows MEPPS to 

incorporate visualization methods for quality control and for managing and querying of the spatial 
emission data. 

To generate the emission modeling grid structure, a political boundaries (state and county 
boundaries) Arc/Info® coverage named "counties" must already exist in the exact map projection 
that is intended for the emission modeling grid structure.  Accordingly the counties  coverage 
provided with MEPPS is in the Lambert Conformal projection.  If inconsistent projections are 
used, the gridding processor produces unpredictable results. The processor produces three files 
that are used elsewhere in EMPRO: 

A running history of emission modeling grid structure changes. 
A list of the geographic location of each grid cell in the emissions modeling grid structure. 
The Arc/Info® coverage of the emission modeling grid structure. 

The gridding processor supports the following subset of Arc/Info® map projections: 

• 
• 
• 
• 
• 

UTM (Universal Transverse Mercator) 
Lambert (Lambert Conformal Conic) 
Albers (Albers Conic Equal Area) 
Geographic (actual latitude and longitude coordinates rather than a projection) 
State (State Plane Coordinate System) 

For more information about the Arc/Info® map projections, refer to the following: 

• 

• 

ARC/INFO~ User's Guide: Map Projections &  Coordinate Management,  Concepts and 
Procedures. 
ARC/INFO® Command References: ARCD Command References,  Commands J-Z 

The UTM map projection is a specialized application of the Transverse Mercator projection.  It is 
limited to use between the latitudes of 84 ° North and 80° South, and divides the earth into 60 
longitudinally-defined zones (UTM zones 1-60) with each zone spanning 6 ° of longitude.  UTM 
zone 1 starts at 180 ° West longitude and ends at 17 4 ° West longitude.  UTM zone 2 starts at 
17 4 ° West longitude .and ends at 168 ° Westlongitude, and so on around the globe.  Shape, area, 
direction, and distance errors are all minimized if a study area is within a single zone; however, 
error increases rapidly as study areas cross UTM zone boundaries. 

The Lambert map projection is a projection of the earth onto a cone intersecting the_ earth along 
two parallels called standard parallels (for example 30° North and 60° North).  The projection is 
good for large scale (continental or smaller), middle latitude (between 45 ° North and '45 ° South) 
study areas.  Shapes are maintained on a small scale (state or smaller), and large shapes 
(countries, continents) are minimally distorted.  Area and distance are maintained near the 
standard parallels, but area is reduced between the standard parallels and increased beyond the 
standard parallels; that is, the study area is accurately represented between the standard parallels. 

Direction is accurate within the entire study area.  CMAQ is currently tested and used with a 
Lambert conformal projection. 

The Albers map projection is suitable for study domains where the east-west extent is greater than 
the north-south extent.  Shape is minimally distorted near the standard parallels as long as the 
study domain's east-west extent is greater than the study domain's north-south extent.  Area is 
accurate within the study domain.  Direction is accurate only at the standard parallels and 
minimally distorted between the standard parallels.  If the study area is in the middle latitudes ( 45 ° 
North to 45 ° South), distances are minimally distorted at and between the standard parallels.  If 
the study area falls outside of the middle latitudes, distance error increases rapidly as the poles are 
approached. 

The Geographic system is not a true map projection; instead it is a global reference system.  It is 
supported as a map projection because geographic positioning (latitude and longitude) is the most 
widely used method of map location.  The origin of the geographic global reference system is at 
0° longitude (Meridian of Greenwich) and 0° latitude (equator).  Directions in the northeast 
quadrant are measured in positive longitude degrees by positive latitude degrees.  Directions in 
the northwest quadrant are measured in negative longitude degrees by positive latitude degrees. 
Directions in the southwest quadrant are measured in negative longitude degrees by negative 
latitude degrees.  Directions in the southeast quadrant are measured in positive longitude degrees 
by negative latitude degrees. 

The State system is also not a true map projection; instead it is a specialized coordinate system for 
the United States, Puerto Rico, and the United States Virgin Islands.  The State system divides 
the aforementioned areas into a total of 120 zones of varying sizes.  The map projection is 
inherent in each zone.  The three map projections that are used ill the State system are the 
Lambert Conic Conformal (for zones with where the east-west extent is greater than the north(cid:173)
south extent), Transverse Mercator (for zones where the north-south extent is greater than the 
east-west extent), and Oblique Mercator (used only for the Alaska Panhandle).  Direction, 
distance, area, and shape errors vary with the map projection inherent in each zone; however, the 
zones have been designed so that error is reduced or eliminated as long as the study area falls 
entirely within a state plan zone. 

The spatial allocation process grid description file ($EMS_ G RD/grd _ desc. in) references 
numerous data items that are used by the Grid Definition Model to prepar1~ the emissions 
modeling grid structure.  Table 4-2 lists the run description file data items that affect processing in 
the Grid Definition Model. 

Table 4-2.  Data Items That Affect the Gridding Processor 

EPA/600/R-991030 

rid  a 

Tutorial 36km 

Resolution 

descriptive text for the emissions 
modeling grid 

Grid 

SOUTHWEST 

10 

-108000 

location on/in the grid cell from 
which to make measurements 
(NORTH, NORTHEAST, EAST, 
SOUTHEAST, SOUTH, 
SOUTHWEST, WEST, 
NORTHWEST, CENTER) 

UTM zones for which the 
emissions modeling grid exists 
(1-60) 

southwest x comer origin 
(measured from the origin of the 
projection) of the emissions 
modelin  domain  meters) 

southwest y comer origin 
(measured from the origin of the 
projection) of the emissions 
modelin  domain (meters) 

.· ..... . 

<</  M~iiri.•·•·•i•·•·•·•······ 
··••··•···•··• id~riti.fi~t············· 

Grid Id: 

Description: 

Cell Location: 

(S,E) 

UTMzone: 

(S,E) 

Origin X 

direction: (S,E) 

c 
c 

c 

N 

N 

idid 

griddesc 

cell  loc 

utmzn 

utmxorig 

utmyorig 

Origin Y 

direction: (S,E) 

N 

-1080000 

cellsizx 

cellsizy 

xcells 

ycells 

zcells 

projectn 

Cell size X 

direction: (S,E) 

Cell size Y 

direction: (S,E) 

Number of 

cells X 

direction: (S,E) · 

Number of 

cells Y 

direction (S,E 

Number of 

cells Z 

direction: (S 

Name: (S,E) 

.N 

36000 

x grid cell size (meters) 

N 

N 

N 

N 

c 

36000 

y grid cell size (meters) 

21 

21 

10 

LAMBERT 

4-21 

number of grid cells in the x 
direction 

number of grid cells in the y 
direction 

number of grid cells in the z 
direction (currently not used by 
ARC/INFO) 

projection name (LAMBERT, 
UTM, ALBERS, STATE, 
GEOGRAPHIC 

Table 4-2.  Data Items That Affect the Gridding Processor 

Variable Name 

... ·.·.·.•,·.• .. ·.

.••. dMe····neti?fiu1 .... e\r .. ·.· 

1 

·.  ··.·.·. 
·.. 

·•  }~~~·!·  .,: 

~ial\1p1e  < •  (.•·.•·.·•·.·············· ..... ·.•·.•.·.•· .. ···.·.·.D····.· 

. .. -::..:.-.::::::::::~::=::~=r)==(~\:;:~:;:;:;:i 

.. ········.····.e··••.·.•.~.•ffi~tfo~  • ......... · 

·>  :-;-:::::::. .. .-- -

.·. 

-. 

....................... . 

.·.•.·.· .-.;.-

.:.:.:::::::;  ::::.: .. :;:.:.::  .. •.·.•,•,···:.· .. ·.·.··.·.:.:····· 

· .. · 

..... ·.· .. ;· 

.... <·  ··. 

: 

·. 

. ... ·.· .. ·· 

projunit 

Units: (S,E) 

xshift 

X-shift: (E) 

yshift 

Y-shift: (E) 

projzone 

UTMzone: 

(S,E) 

c 

N 

N 

N 

METERS 

0.0 

0.0 

16 

fipszone 

FIPS zone for 

state plane 

projection: (E) 

N 

3701 

datum 

p_lsp_dd 

p_lsp_mm 

p_lsp_ss 

Datum 

conversion 
name for 
projection: 

(S,E) 

1st standard 
parallel DD: 

(S,E) 

1st standard 
parallel MM: 

(S,E) 

1st standard 
parallel SS: 

(S.E) 

c 

N 

N 

N 

MM5 
(Perfect 
sphere) 

30 

0 

0.00 

units used to make measurements 
in the selected projection 
(METERS, FEET, DD [decimal 
degrees]) 

constant value to add to x input 
coordinates (value in projunit(cid:173)
typically 0.0) 

constant value to add to y input 
coordinates (value in projunit -
typically 0.0) 

State Plane or UTM zone (used 
only when projectn is  ST A TE or 
UTM -- see ARC™ Command 
References,  Commands J-Z; 
Table PROJECT-3 and Table 
PROJECT-4 for valid values) 

FIPS code for State Plane zone 
(used only when projectn is 
ST ATE -- see ARC™ Command 
References,  Commands J-Z; 
Table PROJECT-5 for valid 
values) 

datum upon which input 
coordinates are based (used only 
when projectn is ST A TE -(cid:173)
NAD27, NAD83, UNKNOWN) 

degree of first standard parallel 
(only for projectn ALBERS or 
LAMBERT) 

minutes of first standard parallel 
(only for projectn ALBERS or 
LAMBERT) 

seconds of first standard parallel 
(only for projectn ALBERS or 
LAMBERT) 

4-22 

Table 4-2.  Data Items That Affect the Gridding Processor 

···· 

1::::r  0:  . ·... 
,:•::·::  :\~i~!:~~~r·ame······ ... ······ 
. / .· 
\.)\:11·.·  .'. :\ 
········· 

:::.:·~ 

· · }\ <::>::(:·:·:::·· 

::::: ::::·~~~!\  ·•·  ""  •). :' '.}/ ······ 

::::.Fff 

:::: 

········· 

.·:-::::. 

.. · ....•. 

: .. :. 
:....=.:. 

.····:  (:  )· 

• .........  ·.·.· 
·:.n · jg.· ;jii:~~s00: .:.::. 

·::::·:·:;  ····~:/•••<:::. 

·.·~ 

• ·· :; .: 
······  •.  •/•/ 

: 

······ .·.  · .· . 

.f) 

p_2sp_dd 

p_2sp_mm 

p_2sp_ss 

p_cen_dd 

p_cen_mm 

p_cen_ss 

p_lpo_dd 

p_lpo_mm 

p_lpo_ss 

Menn 

: 1<l~litiri¢r : 
2nd standard 
parallel DD: 

(S,E) 

2nd standard 
parallel MM: 

(S,E) 

2nd standard 
parallel SS: 

(S,E) 

Central 

meridiart DD: 

(S,E) 

Central 

meridian MM: 

(S,E) 

Central 

meridian SS: 

{S,E) 

Latitude of 
projection 
origin DD: 

(S,E) 

Latitude of 
projection 
origin MM: 

(S,E) 

Latitude of 
projection 
origin SS: 

(S,E) 

N 

N 

N 

N 

N 

N 

N 

N 

N 

60 

0 

0.00 

-90 

0 

0.00 

40 

0 

0.00 

0.0 

0.0 

degrees of second standard 
parallel (only for projectn 
ALBERS or LAMBERT) 

minutes of second standard 
parallel (only for projectn 
ALBERS or LAMBERT) 

seconds of second standard 
parallel (only for projectn 
ALBERS or LAMBERT) 

degrees of central meridian of 
projection (only for projectn 
ALBERS or LAMBERT) 

minutes of central meridian of 
projection (only for projectn 
ALBERS or LAMBERT) 

seconds of central meridian of 
projection ( c:inly for projectn 
ALBERS or LAMBERT) 

degrees of latitude from 
projection origin (only for 
projectn ALBERS or LAMBERT) 

minutes of latitude from 
projection origin (only for 
projectn ALBERS or LAMBERT) 

seconds of latitude from 
projection origin (only for 
projectn ALBERS or LAMBERT) 

false easting (only for projectn 
ALBERS or LAMBERT --
meters) 

false northing (only for projectn 
ALBERS or LAMBERT --
meters) 

p_f_east 

False easting: 

N 

(E) 

p_fnorth 

False northing: 

N 

(E) 


Variable Name refers to the name as it is found in the SAS® data set run  desc.ssdOJ  which can be 
found in the directory referenced by the UNIX environment variable EMS_RUN (see Models-3 
Volume 9B:  User Manual, Chapter 6).  Menu Identifier refers to the descriptive text found on 
the grid definition window in Science Manager (S) or the EMPRO Grid Processor window (E) 
within MEPPS.  Type refers to the variable type -- N is a numeric variable, and C is a character 
variable.  Example gives an example of what might be entered for each data item.  Description 
provides descriptive text about the data item, supported units, and a list of valid values that each 
data item recognizes -- valid values and units are listed in parentheses. 

#### 4.2.3.2 Temporal Allocation of Emission Data 

Emission data that are based on annual, seasonal, weekly, or daily values must be temporally 
allocated (usually disaggregated) to hourly data for compatibility with the time scale of episodic 
air quality modeling.  Generally, this procedure applies to regional inventories of point- and area-· 
source emission data.  Modeled emission data, such as biogenic- and mobile-source emissions are 
generated as hourly data for the time period of interest.  Allocation of emission data from time 
periods greater than hourly (e.g., annual total, monthly total, weekly total), to hourly data is 
accomplished by use of seasonal, monthly, weekly, and daily diurnal temporal allocation factors to 
translate the data to daily total emission values.  The daily values are then transformed into 
emission values for each hour of a typical day by using user-supplied or default temporal 
allocation profiles.  The profiles assign proportions of the total daily emissions to each of the 24 
hours of a typical day.  Profiles are ideally source or source-category specific, but often are used 
for a range of similar source categories because of limited data on source category temporal 
variability.  The majority of diurnal temporal source profiles have been developed for point 
sources, since more detailed reporting and monitoring exist for point sources, particularly large 
point sources, than for area sources which are spatially diverse and variable.  Consequently are 
area source temporal allocation profiles tend to be less detailed and specific that point source 
temporal allocation profiles.  Most of the temporal profiles used as defaults in MEPPS, were 
developed for the National Acid Rain Precipitation Assessment Program (l'..f APAP) (Pratt et al., 
1990) and used in FREDS, with some more recent supplements (Moody et al.,  1995). 

. 

In the EMPRO module ofMEPPS, temporal allocation of point and area source category 
emission data is performed after spatial allocation of data in each main processor (point, area, 
biogenic, and mobile) and prior to speciation (see Chapter 6 of the Models·-3 User Manual).  This 
is transparent when running from Study Planner.  If running in MEPPS under Tools Manager, the 
user may enter source category or source-specific daily or diurnal hourly temporal allocation data, 
or elect to use profiles computed by EMPRO (procedure described in the following sections) or 
use temporal allocation profiles based on those developed for use in the FR.EDS processors as 
supplemented.  If a combination of emission inventory data at different temporal resolutions is 
used, EMPRO uses the most time-resolved (specific) data first, filling in with less time-specific 
data and applying temporal profiles as needed.  If hourly, daily, and annuall emission data are 
selected, EMPRO will provide hourly emission values taken from available data in the following 
order: 

Loading of hourly diurnal emission inventory (including Continuous Emission Monitoring 
(CEM) data); 
Loading and gridding of day-specific (daily) emission inventory data; 
Computing hourly emission data from annual or other longer-term emission inventory 
data. 

Further details concerning temporal allocatipn are discussed as part of the following descriptions 
of the basis for processing each basic type of emission source. 

#### 4.2.3.3 Point Source Emission Data Processing 

The point-source emission processor prepares gridded, temporally allocated point-source 
emission estimates suitable for speciation and reformatting for input to Models-3 framework for 
air quality modeling (e.g., CMAQ) (see Chapter 6 of the Models-3 User Manual).  As previously 
indicated, the point-source emission processor does not compute basic emission estimates using 
emission factors and source-specific information.  Instead, it reduces annual, point-source 
emission estimates to hourly, point source emissions estimates, unless the user is able to provide 
hourly emission data. 

The point-source emission processor begins by establishing study-specific foundation emission 
estimates taken by INPRO from an user-specified emission inventory database.  Foundation 
estimates (referred to as Foundation Files in MEPPS) are the basic annual emission inventory files 
imported to a processor (point, area, or mobile source) after quality control corrections have been 
applied and the data have been reduced to cover the study area only.  After a point-source 
foundation file has been created, the processor spatially allocates the emission data into the spatial 
allocation grid structure, temporally allocates the emission estimates, and updates the hourly 
emission estimates derived from the foundation annual inventory with day-specific hourly 
emission estimates or hourly CEM data (if available).  Usually, the point source-specific emission 
data are processed to files corresponding to the major hierarchical point-source data elements 
(i.e.,  Facility, Stack, Device, and  Process), and translated to SAS® data sets. 

Hourly emission estimates are computed using data from one of the following: 

• 

• 
• 

Gridded emission estimates derived from annual emission inventory data by applying 
factors :from source category-specific temporal allocation profiles 
Day and source-specific hourly emission data provided by the user 
Source-specific hourly CEM data. 

Typically, the primary inputs to the point-source processor are the foundation emission annual 
estimates which are supplemented by available day-specific emission estimates.  The primary 
output of the processor is the spatially and temporally allocated emission estimates. 

Computation of Point Source Emission Estimates 

The point-source emission processor treats emission estimates that have be:en prepared on one of 
the following bases: annual average; average day; or day-specific. 

Day-Specific Emissions Estimates 

Day-specific emission estimates are not computed by the MEPPS EMPRO point source 
processor.  The user must supply any available day-specific emission estimates to the processor. 
Because day-specific emission estimates are more representative of actual conditions, they replace 
the hourly emission estimates derived from disaggregated annual emission data, when available. 
Day-specific emission data formats are made consistent with the internal formats of the SAS® data 
sets derived from other (e.g., annual) point-source emission data when imported through IDA. 

CEM Hourly Emission Data 

The CEM data are a subset of hourly emission data derived from continuous air pollutant 
concentration monitors attached to components of specific facilities, usuaHy boilers or stacks of 
large point sources such as electric utilities.  The data elements included are hourly emission rates 
for Carbon Monoxide (CO), nitrogen oxide, (NOx), and sulfur dioxide (S02).  The CEM emission 
data are treated as hourly data imported to MEPPS via IDA.  Electric utility CEM data are 
identified by specific source using their ORIS (Office of Regulatory Information Systems) 
identification number and are mapped to the corresponding point source identification number 
from an emission inventory.  The CEM data are read in for a specified grid domain and day from a 
CEM dataset and substituted for hourly emission data derived from annual data.  The CEM 
emission data are used in the format of data available for electric utility emissions provided by the 
U.S. EPA Acid Rain Division.  The Acid Rain Division has electric utility CEM data for the 
United States for 1995 and 1996. 

Spatial Allocation 

Point sources are spatially allocated to an emission modeling grid by the geographic (latitude and 
longitude, UTM position) coordinates of a stack or by the geographic coordinates of the facility. 
The point-source emission data processor prepares an ASCII file of point-source identifiers and 
point-source geographic coordinate locations.  The processor reads the ASCII file that was 
generated by the location processor, generates the appropriate ARC/INFOO!> coverages, and 
prepares two ASCII files: 

• 
• 

A file which contains point source identifiers and grid cell location; and 
A file which contains point-source identifiers and latitude/longitude coordinates.  The 
process of assigning grid coordinates to point sources is an ARCffi..if 0® function; 
therefore, the underlying procedure follows the user documentation and proprietary code 
for the ARC/INFO® software. 

Temporal Allocation 

The EMPRO temporally allocates (produces hour-by-hour) foundation file emission estimates 
within each main emission processor (point, area, biogenic, ~d mobile emissions) based on 
operating schedule data that are provided in one of two ways. The operating schedule data may 
be passed into the MEPPS EMPRO module via an ASCII foundation file, which corresponds with 
the segment (process) level of point source inventory information.  For point sources, the 
temporal factors may be assigned by the user at the segment (process) level of the EMPRO point 
source hierarchy.  The hierarchy is closely aligned with tlhat of regional emission inventories. 
Temporal allocation factors may be also be applied at the device level in the facility, stack, device 
hierarchy.  Alternatively, default source-category-specific temporal allocation factors may be 
selected:  These factors were used in a MEPPS predecessor, FREDS, and are referred to as 
FREDS temporal allocation factors. The point source hierarchy for temporal allocation factors in 
FREDS is:  plant, point, temporal factors.  Source-specific temporal allocation factors are 
typically not available for large regional modeling domains.  Consequently, the default temporal 
allocation factors are commonly used in regional modeling, perhaps more than 90 percent of the 
time.  Note that EMPRO temporally allocates foundation file emission estimates, but not day(cid:173)
specific emissions estimates since the latter are entered on an hour-by-hour basis. 

EMPRO provides a variety of methods for identifying operating schedule data: 

• 

• 
• 
• 
• 
• 

Seasonal throughput fractions (winter [Dec, Jan, Feb], spring [Mar, Apr, May], summer  · 
[Jun, Jul, Aug], and fall [Sep, Oct,. Nov]) 
Hours per year in operation 
Days per year in operation; 
Weeks per year in operation; 
Days per week in operation; and 
Hours per day in operation. 

Any, none, or all of the operating schedule data can be supplied. If no operating schedule data are 
supplied, EMPRO uses a default continuous operating schedule of 24 hours per day, 7 days per 
week, and 52 weeks per year; otherwise, EMPRO filters 1through a hierarchy of the· operating 
schedule data to determine how to compute the temporal factors: 

• 
• 
• 

Weekly temporal factor; 
Daily temporal factor; and 
24 hourly temporal factors. 

To determine the weekly temporal factor, EMPRO determines the number of days in the year of 
interest and divides that value by 7 days per week.  For example, in a leap year, there are 29 days 
in February; therefore, the weekly temporal factor is 366 days/year+ 7 days/week). 

:  . 

EMPRO computes a daily temporal factor through a lookup table based on the value of average 
8-hour work days (days per week in operation) which is passed into MEPPS through the ASCII 
foundation files.  To determine the daily temporal factor, EMPRO examines the values for days, 
hours per year (houryear), days per year (dayyear), and weeks respecti:vely.  If days has a valid 
value, EMPRO takes no further action to determine the days per week in operation for the 
source.  If days does not have a valid value, EMPRO attempts to assign an operation code value 
to days by examining houryear, dayyear, and weeks respectively: 

• 

houryear (hours per year in operation) 

if houryear > 0 and houryear <= 850 then days = 2, 
ifhouryear > 850 and houryear <= 1250 then days= 3, 
ifhouryear >  1250 and houryear <= 1670 then days= 4, 
if houryear >  1670 and houryear <= 2100 then days= 5, 
ifhouryear > 2100 and houryear <= 2500 then days= 6, 
ifhouryear > 2500 then days= 7; 

• 

dayyear (days per year in operation) 

if dayyear > 0 and dayyear <= 110 then days = 2, 
if dayyear >  110 and dayyear <= 160 then days = 3, 
if dayyear > 160 and dayyear <= 210 then days = 4, 
if dayyear > 210 and dayyear <= 260 then days= 5, 
if dayyear > 260 and dayyear <= 315 then days = 6, 
if dayyear > 315 then days = 7; 

• 

weeks (weeks per year in operation) 

if weeks > 0 and weeks <= 7 then days = 1, 
if weeks > 7 and weeks <= 13 then days = 2, 
if weeks >  13  and weeks <= 19 then days = 3, 
if weeks >  19 and weeks <= 26 then days = 4, 
if weeks> 26 and weeks<= 33 then days= 5, 
if weeks> 33  and weeks<= 39 then d;:tys = 6, 
if weeks> 39 then days= 7. 

EMPRO computes an hourly temporal factor through a lookup table based on the value of hours 
(hours per day in operation) which is passed into EMPRO through the ASCII foundation files.  If 
hours has a valid value, the value is used and no further action is taken to determine the hours per 
day in operation for the source.  If hours does not have a valid value, EMPRO attempts to assign 
a value to hours by examining houryear, dayyear, and weeks respectively, using the above tables. 
If houryear, dayyear, or weeks has a valid value, then hours is assigned a value of 8, again based 
on an average work day of 8 hours.  If hours or days cannot be assigned through the method 
described above, operation is assumed to be continuous and hours is assigned a value of 24, and 
days is assigned a value of 7.  The following calculations are performed. 

4-28 

week_ fac = daysinyear I 7 
day_sumd =I, dy_act1, i =  1, ... 7 
day_faci = dy_act1Iday_sumd,i=1, ... 7 
hour_sumh =I, hr_act?, i =  1, ... 24 
hrpro~ = hr_act? I hour_sumh, i =  1, ... 24 

where  week_fac is the weekly temporal factor 

EPA/600/R-991030 

(4-2) 
(4-3) 
(4-4) 
(4-5) 
(4-6) 

daysinyear is the number of days in the year (365 or 366) 
day_ sum is the total relative activity for the week 
dy _act is an array that contains relative daily activities 
day _fac is the day of week temporal factor 
· hour_ sum is the total relative activity for the day 
hrprof  is an array of hourly temporal factors 
hr_ act  is an array that contains relative hourly activities 
d is an index indicating which days per week in operation code (days) to use 
h is an index indicating which hours per day in operation code (days) to use 
i is the integer count that applies (e.g., 7 days or 24 hours) 

EMPRO uses the temporal factors to allocate emission estimates to hourly values.  Application of 
the temporal factors depends on the temporal basis (annual average, day-specific, average day) 
that emissions estimates were input to EMPRO. 

• 

ifthe temporal basis code estt ="AA" (annual average) then 

hremisi = aceekg/( week_fac* day_fac * hrpro~), i =  1, ... 24 

(4-7) 

where  hremis is the array of gridded, hourly emissions estimates CNOx, TOG, CO, 

etc) (mass) 
aceekg is the emission estimates (mass) 
week_fac, day _fac, and hrprof are the weekly, daily and hourly temporal 

factors, respectively, computed by EMPRO 

if the temporal basis code estt ="AD" (average day) and (week_fac = 0 or day _fac 
= 0) then 

hremisi = 0.0, i =  1, ... 24 

(4-8) 

if the temporal basis code estt ="AD" (average day) and not (week_fac = 0 or 
day_fac = 0) then 

• 

• 

. hremisi = aceekg * hrpro~, i =  1, ... 24 

(4-9) 

• 

if the temporal basis code estt = "DS" (day-specific) then 

hremisi = aceekg * prot:, i = 1, ... 24 

(4-10) 

where  prof is an array of day-specific, hourly temporal factors 

#### 4.2.3.4 Area Source Emission Data Processing 

The area-source ·emission data processor prepares area-source emission estimates for speciation 
(lumped-model or discrete speciation) and reformatting for input to CMAQ or other Models-3 
framework components.  The area-source processor does not compute emission estimates from 
fundamental emission data.  It reduces annual, county-wide emission estimates to emissions on an 
hourly, grid cell-by-grid cell basis.  Most of the area source emission data processor is written in 
the SAS® programming language.  The area-source spatial surrogates allocation component of the 
processor is written in the ARC® Macro Language (AML ®).  The primary inputs to the processor 
are the area-source emission estimates for the spatial domain of interest, and day-specific emission 
estimates (if available).  The primary outputs are the spatially allocated, temporally resolved, 
emission estimates. 

The processor begins with an area-source emission data base, extracted by INPRO from an 
emission inventory as an ASCII file and automatically converted to a SAS<l!l file.  The area-source 
processor updates the estimates with any available day-specific estimates, spatially allocates the 
emissions into the emission modeling grid, and temporally allocates the criteria emissions. 

Emission Data Processing 

This section discusses how area-source emission estimates are read into, manipulated by, and 
quality-assured by the area-source emission data processor.  References are made to MEPPS 
source code where necessary to provide further details concerning area source emission 
allocation. 
The processor uses county-wide area-source emission estimates, that have been prepared 
externally as part of an emission inventory, to produce spatially allocated, temporally resolved, 
pollutant emission estimates.  To the extent that the inventory used includes small point sources in 
area source estimates, they are treated as area sources.  All data provided as point sources, 
regardless of magnitude, are processed as point sources in EMPRO.  The area-source emission 
data processor accommodates area-source emission estimates that have been prepared on the 
following bases: annual average; average day; or day-specific. 

Day-specific emission data enter the area-source emission data processor during the temporal 
allocation step.  Day-specific emission data  are not computed by EMPRO.  Like the foundation 
file emissions estimates, day-specific emission estimates are supplied from an external source to 
the emissions modeler.  The day-specific emissions estimates replace the area-source emission 
estimates derived from annual inventory data because day-specific emission data are more 
representative of actual conditions. 

Day-specific area-source emission estimates are summed to a daily total so that they are 
consistent with the internal formats of the SAS® data sets that maintain the other (e.g., derived 
from annual emission inventories) area-source emissions estimates,  which are passed to EMS-95 
on an hourly basis. 

Spatial Allocation 

Area-source emission estimates are allocated to a modeling domain through the application of 
spatial surrogates.  In general, surrogates approximate the value of unknown quantities.  For 
example, population can be used to estimate the number of gasoline service stations.  A spatial 
surrogate not only helps estimate the value of an unknown quantity, but a spatial surrogate also 
helps locate the unknown quantity. 

For each spatial surrogate, it is necessary to specify what data (categories ofland use/land cover, 
population counts, housing counts, etc.) contribute to the surrogate.  In EMPRO, county area, 
land cover, population (census), Federal Highway Administration major roadway, and 
TIGER/LINE roadway data for the United States are provided as ARC® coverages for use as 
surrogates. Others may be added by providing ARC® coverages generated external to MEPPS. 
County area, and less detailed population and land cover coverages are provided for Canada and 
Mexico.  Each area source category must be assigned a unique spatial surrogate value by grid cell. 
After the appropriate files have been populated, EMPRO grids the necessary data sets according 
to  the requirements of the user-defined spatial surrogates.  Each area source category (asct) 
which has been assigned to a spatial surrogate (k) can be allocated to grid cells (1,  m) through the 
application of Equation 4-11. 
* 
aceei,j,k',l,m = aceei,j,k' * ratloij,k,I,m  (4-11) 

where  i is the index on states 

j  is the index on counties within the states 
k is the spatial surrogate index 
k' is the area source category index (directly related to k) 
1 is the x cell index 
m is the y cell index 
acee is the county-wide area source emissions estimate (mass) 
ratio is the gridded surrogate ratio by state/county/surrogate/cell 

Each surrogate is computed through the application of Equations 4-12 and 4-13. 

surtotij,k = ~1  ~m attributeij,k,I,m  (4-12)

ratioij,k,I,m =  attributeij,k,I,m I surtotij,k  (4-13)

where: 

i is the index on states 
j is the index on counties within the states 
k is the spatial surrogate index 
1 is the x cell index 
m is the y cell index 
surtot is the total value of a surrogate within specified (inde:xed) states and 
counties attribute is the gridded value of the surrogate attribute by state/county/surrogate/cell 
ratio is the gridded surrogate ratio for specified (indexed) states/counties/surrogates/cells 

In the case of census data or other area-based or length-based surrogate information, it is 
necessary to area-apportion or length-apportion the surrogate (such as population or housing) 
information prior to aggregation to the cell level.  This is because the locale of interest crosses cell 
boundaries.  The assumption is that the surrogate information (such as population or housing) has 
a constant density across the locale (in the case of population or housing data, a constant density 
across the census tract).  Therefore, in some cases, it is necessary to apply Equations 4-14 
through 4-16 prior to the application of Equations 4-12 and 4-13. 

arlg_tot;j,k =  :E1:Em arealengij,k,l,m 

ap_ratio;j,k,J,m =  arealengij,k,l,m I arlg_totij,k 
attribute;j,l,m =  attribute;j,k * ap_ratioij,k,l,m 

where  i is the index on states 

(4-14) 

(4-15) 

(4-16) 

j  is the index on counties within the states 
k is the locale of interest index (such as census tract or roadway) 
1 is the x cell index 
m is the y cell index 
arlg_tot is the total area or length of a locale of interest within specified (indexed) 

states and counties 

arealeng is the gridded value of the specified (indexed) locale of interest by  state/ 
county /surrogate/cell 
ap ratio is the gridded ratio of the locale of interest specifa:d (by indices) states/ 

counties/locales/cell 

attribute is the surrogate data (population, road length, etc.) for the area specified 

by indices. 

As a check for Equations 4-14 through 4-16, the following must be true: 

(4-17) 

Temporal Allocation 

Temporal allocation of area source emission data is performed in an identical.manner as described 
for point source processing in Section 4.2.3.3, although it is almost always necessary to use 
default temporal allocation profiles.  Please refer to that section for a discussion of temporal 
allocation of area sources. 

### 4.2.4  Modeled Emission Data 

Emission data from available emission inventories are spatially and temporally allocated as 
described in the preceding paragraphs.  Many anthropogenic area and point sources of emissions 
vary little or predictably with time, and can reasonably be disaggregated for regional modeling. 
However, some emission data are highly variable, diurnally and.seasonally, because they are 
dependent on environmental variables, such as temperature, humidity, and solar insolation.  It is 
more accurate to model these kinds of emissions on an hourly basis for direct use in episodic air 
quality modeling.  The two principal kinds of sources for which hourly emission data are normally 
modeled are mobile sources and biogenic sources.  In both cases, MEPPS takes hourly 
meteorological data from  MCIP output derived from MM5. 

#### 4.2.4.1 Biogenic Emissions 

Hourly biogenic emission rates for biogenic VOC compounds (including  isoprenes, 
monoterpenes, and soil NO) for each grid cell are estimated using the Biogenic Emission 
Inventory System, Version 2 (BEIS-2) within MEPPS.  In order to estimate biogenic emissions 
for modeling, it is necessary to apply biogenic emission and biomass factors to a geographic 
distribution of land cover.  The BEIS-2 was developed to fill this need separately from Models-3 
as an improvement on BEIS-1  (Pierce et al.,  1990; Geron et al.,  1994), and has been used in 
conjunction with different air quality modeling systems, i.ncluding the Regional Acid Deposition 
Model (RADM) (Pierce et al., in press), the Regional Oxidant Model (ROM), and now Models-3 
CMAQ.  Additional information concerning BEIS-2 can be found in the Emission Inventory 
Improvement Program report on Biogenic Sources Preferred Methods (EIIP, 1996) and Pierce et 
al.  (1998).  The BEIS-2 model applies emission flux factors specific to tree genera and 
agricultural crop types by geographic area for biogenic emission species in accordance with 
equation 4-18. 

ER;= Lj [Aj * EF * Fu(S,T)] 

(4-18) 

where: ER is the emission rate (in grams/sec/model cell) 

i is the chemical species (e.g., isoprene, monoterpene) 
j is the vegetation type 
A is the vegetation area (meter) in a grid cell 
EF is the emission factor (micrograms/gram of leaf biomass/hour), and 
Fi/S,T) is an environment factor to account for solar radiation Sand leaf temperature T 

4-33 

Vegetation emission flux factors were adapted from those compiled by Geron et al. (1994) for 77 
tree genera.  Emission flux factors for 16 agricultural crops were taken.primarily from Lamb et al 
(1993), and the emission factors applied to 34 land cover types are from the work of Guenther et 
al. (1994).  Emission flux factors by land cover type are used principally for Canada and the 
western United States where genus-level forest cover and agricultural crop data were not 
available.  Biogenic emission flux factors for summer and winter conditions are given in Table 4-3 
and Table 4-4, respectively.  The vegetation classes are taken from the Biogenic Emission Land 
Use Database (BELD) (Kinnee et al., 1997), which in tum is drawn from other land cover data 
sets, with an emphasis on forest and agricultural land cover.  ·Spatial resolution of the raw 
(original) land cover data is at the county-level for the United States and sub-province level for 
Canada.  Emission flux factors are based on full leaf summer conditions normalized to leaf or soil 
temperatures of 30°C and photosynthetically active radiation (PAR) of 1000 micromoles/m2/sec. 
For use in regional modeling, the mass ofbiogenic emissions are converted to moles by dividing 
the mass (grams-compound) by the molecular weight of the compound.  The emission factors for 
tree genus and agricultural vegetation, and used in the Models-3 application ofBEIS-2 are 
compiled  by Pierce et al. (1998). 

Table 4-3.  Summer Biogenic Emission Flux Factors (µg-compound m·2hr- 1
Biogenic Compounds by Vegetation Category 

)  for Principal 

Vegetation or 
Land Cover 

lsoprene 

.  } 
.· .. Terpe,p~. 'fiii,i'i 

... · .. · .;::  }  ········· 

.. ·.·.·. 

Code 

Abie 

Acac 

Acer 

Aesc 

Aila 

Aleu 

Alfa 

Ainu 

Amel 

Asirn 

Avie 

Barl 

No. 
.• .. ··.· 

LeafAr.e~ ·• 
"  1riaex{m2 
. -: 
· 
:-·· ... ·· 
. .  Jri~2r 
. 

... 

:···· 

.. 
. ·. ·•·. 

:Qescription > · ' , ' ,  
::: -: : :.: ·::)?(~; :::·::::::::::'.:;·< ; . 

·.·. 

.. 
... 

........... 
.. 
.  .. · 
-:--:<·.·.;··· 
. ............... 

..... · .......... 

170.0 

79.3 

42.5 

42.5 

42.5 

42.5 

19.0 

42.5 

42.5 

42.5 

42.5 

7.6 

5100.0 

2380.0 

680.0 

42.5 

42.5 

42.5 

7.6 

42.5 

42.5 

42.5 

42.5 

19.0 

2775.0 

1295.0 

693.7 

693.7 

693.7 

693.7 

11.4 

693.7 

693.7 

693.7 

693.7 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

12.8 

4.5 

4.5 

4.5 

4.5 

11.4 

256.7 

4-34 

7 

5 

5 

5 

5 

5 

0 

5 

5 

5 

5 

0 

Abies (fir) 

Acacia 

Acer (maple) 

Aesculus (buckeye) 

Ailanthus 

Aleurites (tung-oil tree) 

Alfalfa 

Alnus (European alder) 

Amelanchier ( serviceberry) 

Asimina (pawpaw) 

A vicennia (black mangrove) 

Barley 

Table 4-3.  Summer Biogenic Emission Flux Factors (µg-compound m-2hr- 1
Biogenic Compounds by Vegetation Category 

)  for Principal 

EP Af 600/R-99/03Q 

Barr 

Betu 

Borf 

Burne 

Carp 

Cary 

Casp 

Cast 

Casu 

Cata 

Cedr 

Celt 

Cerc 

0.0 

42.5 

910.0 

42.5 

42.5 

42.5 

42.5 

42.5 

29750.0 

42.5 

79.3 

42.5 

42.5 

0.0 

85.0 

713.0 

42.5 

680.0 

680.0 

42.5 

42.5 

42.5 

42.5 

0.0 

693.7 

755.0 

693.7 

693.7 

693.7 

693.7 

693.7 

693.7 

693.7 

1269.3 

1295.0 

85.0 

42.5 

693.7 

693.7 

Cham 

170.0 

340.0 

2775.0 

42.5 

745.4 

1550.0 

0.5 

42.5 

42.5 

7.6 

42.5 

680.0 

1366.6 

1564.0 

0.0 

680.0 

42.5 

19.0 

42.5 

1050.0 

660.0 

Citr 

Cnif 

Conf 

Corn 

Coru 

Co ti 

Cott 

Crat 

Cs wt 

Desh 

65.0 

94.5 

56.7 

57.8 

4-35 

0.0 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

693.7 

993.9 

1036.0 

0.0 

577.6 

693.7 

693.7 

4.5 

4.5 

11.4 

256.7 

693.7 

770.0 

4.5 

0.2 

0 

5 

5 

5 

5 

5 

5 

5 

7 

5 

7 

5 

5 

7 

5 

9 

6 

0 

5 

5 

0 

5 

2 

0 

Barren 

Betula (birch) 

Boreal forest (Guenther*) 

Bumelia (gum bumelia) 

Carpinus (hornbean) 

Carya (hickory) 

Castanopsis ( chinkapin) 

Castanea (chestnut) 

Casuarina (Austl pine) 

Catalpa 

Cedrus (Deodar cedar) 

Celtis (hackberry) 

. Cercis (redbud) 

Chamaecyparis (prt-orford 

cedar) 

Citrus (orange) 

BEIS conifer forest 

Conifer forest (Guenther) 

Com 

Comus (dogwood)  . 

Cotinus (smoke tree) 

Cotton 

Crataegus (hawthorn) 

Herbaceous Wetlands 

(Guenther) 

Desert shrub (Guenther) 

Table 4-3.  Summer Biogenic Emission Flux Factors (µg-compound m-2hr-1
Biogenic Compounds by Vegetation Category 

)  for Principal 

Vegetation or 
Land Cover 

Isoprene 

Terpene•·• 

·.:···:····:·.·.·····.·  ··.·.· 

...... . .... 

••··•·.·•·•Other 
..........  voes  •t·? 

.•:···:·:·· 

..  . 

693.7 

·······:.··········~~············  · t~~t:x;:g~;: 

rri4~~ tiii:t  .... :::;::.:;:;;.(::>/(/.:·.:··. 
... .  irt~)\ .  . /: .. ::··:::-:-:···.·· 

............. : ....... :.: ... : ....... 
... 

:·:; . 

...· 

.. 

· .·. · ... · • .. ·.·.···.·. Descfrjptfon 

' , .  

···•>> 
. .  ····•· 
·.·<  > 

Diospyros (persimmon) 

Eucalyptus 

Fagus (american beech) 

Fraxinus (ash) 

Gleditsia (honey locust) 

Gordonia (loblolly-bay) 

Grass 

Gymnocladus (KY 

coffeetree) 

Halesia (silverbell) 

Hardwood forest (Guenther) 

Hay 

Ilex (holly) 

Juglans (black walnut) 

Juniperus (east. red cedar) 

Laguncularia (white 

mangrove) 

Larix (larch) 

Liquidambar (sweetgum) 

Liriodendron (yellow poplar) 

Maclura ( osage-orange) 

Magnolia 

Malus (apple) 

Melia (chinaberry) 

Mixed forest (Guenther) 

Morns (mulberry) 

Code 

Dios 

Eu ca 

Fagu 

Frax 

Gled 

Gord 

Gras 

Gymn 

Hale 

Harf 

Hay 

Hex 

Jugl 

Juni 

Lagu 

Lari 

Liqu 

Liri 

Macl 

Magn 

Malu 

Meli 

Mixf 

Moru 

42.5 

.. 

42.5 

29750.0 

1275.0 

42.5 

42.5 

42.5 

42.5 

56.2 

42.5 

42.5 

8730.0 

37.8 

42.5 

42.5 

79.3 

42.5 

255.0 

42.5 

42.5 

42.5 

140.5 

42.5 

42.5 

436.0 

94.5 

85.0 

1275.0 

476.0 

42.5 

693.7 

693.7 

693.7 

693.7 

693.7 

84.3 

693.7 

693.7 

882.0 

56.7 

693.7 

693.7 

1295.0 

693.7 

42.5 

42.5 

693.7 

29750.0 

1275.0 

42.5 

42.5 

42.5 

42.5 

42.5 

85.0 

42.5 

1275.0 

42.5 

42.5 

693.7 

693.7 

693.7 

693.7 

693.7 

693.7 

11450.0 

1134.0 

1140.0 

42.5 

85.0 

693.7 

4-36 

4.5 

5 

4.5 

4.5 

4.5 

4.5 

4.5 

57.8 

4.5 

4.5 

4.5 

12.8 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

A.5 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

5 

5 

5 

5 

5 

0 

5 

5 

5 

0 

5 

5 

7 

5 

5 

5 

5 

5 

5 

5 

5 

5 

5 

Table 4-3.  Summer Biogenic Emission Flux Factors (µg-compound m-2hr- 1
Biogenic Compounds by Vegetation Category 

)  for Principal 

EP A/600/R-99/030 

Mscp 

Nmxf 

Nyss 

Oak 

Oats 

Oded 

Ofor 

Oksv 

Ostr 

Othe 

Oxyd 

Pacp 

Past 

Paul 

Pean 

Pers 

Pice 

Pinu 

Plan 

Plat 

Popu 

Po ta 

Pros 

Prun 

7.6 

19.0 

10150.0 

1100.0 

5950.0 

3108.3 

7.6 

2112.4 

56.2 

7350.0 

42.5 

56.2 

42.5 

55.0 

56.2 

42.5 

102.0 

42.5 

255.0 

255.5 

19.0 

368.8 

140.5 

100.0 

42.5 

140.5 

255.0 

79.8 

140.5 

42.5 

255.0 

255.0 

I 1.4 

850.0 

693.7 

894.2 

12.3 

4.5 

4.5 

4.5 

I 1.4 

256.7 

871.8 

84.3 

600.0 

693.7 

4.5 

4.5 

4.5 

4.5 

84.3 

57.8 

693.7 

47.9 

84.3 

693.7 

153.0 

693.7 

4.5 

35.3 

57.8 

4.5 

12.8 

4.5 

23800.0 

5100.0 

2775.0 

79.3 

42.5 

14875.0 

29750.0 

9.6 

42.5 

42.5 

2380.0 

1295.0 

4.5 

42.5 

42.5 

42.5 

24.0 

42.5 

42.5 

693.7 

693.7 

693.7 

4 " • .J 
,. 
• .J 
,. 
• .J 

4 

4 

14.4 

192.5 

,. 
• .J 
,. 
.:> 

4 

4 

693.7 

693.7 

4-37 

0 

5 

5 

6 

0 

6 

0 

2 

5 

0 

5 

0 

0 

5 

0 

5 

7 

3 

5 

5 

5 

0 

5 

5 

Misc crops 

Northern Mixed Forest 

(Guenther) 

Nyssa (blackgum) 

BEIS oak forest 

Oats 

BEIS other deciduous forest 

Open forest 

Oak Savannah (Guenther) 

Ostrya(hophornbeam) 

Other (unknown, assume 

grass) 

Oxydendrum (sourwood) 

Pasture crop land (Guenther) 

Pasture 

Paulownia 

Peanuts 

Persea (redbay) 

Picea (spruce) 

Pinus (pine) 

Planera (water elm) 

Platanus (sycamore) 

Populus (aspen) 

Potato 

Prosopis (mesquite) 

Prunus (cherry) 

Table 4-3.  Summer Biogenic Emission Flux Factors (µg-compound m-2hr- 1
Biogenic Compounds by Vegetation Category 

)  for Principal 

Leaf Are.a••· 

NO. 
):.  i  .••  lli~~~<ili= . 

.·  fr('l) . •.. 

Vegetation or 
Land Cover 

Code 

Pseu 

Quer 

Rang 

Rhiz 

Rice 

Ro bi 

Rye 

Sabi 

Sali 

Sa pi 

Sass 

Scru 

Scwd 

Sere 

Shrf 

Smxf 

Snow 

Sor 

Sorg 

Soyb 

Spin 

Swie 

Taxo 

Thuj 

Isoprene 

Terpene 

.. 

•.·  .·.Other 
voes···· 

170.0 

2720.0 

2775.0 

29750.0 

37.8 

42.5 

102.0 

5950.0 

7.6 

5950.0 

14875.0 

42 .. 5 

42.5 

37.8 

2700.0 

14875.0 

10750.0 

85.0 

94.5 

42.5 

255.0 

85.0 

19.0 

42.5 

42.5 

42.5 

42.5 

94.5 

349.0 

42.5 

530.0 

693.7 

56.7 

693.7 

153.0 

693.7 

11.4 

693.7 

693.7 

693.7 

693.7 

56.7 

651.0 

693.7 

910.0 

17000.0 

1500.0 

1250.0 

0.0 

42.5 

7.8 

22.0 

0.0 

42.5 

19.5 

0.0 

1460.0 

1983.0 

42.5 

42.5 

42.5 

170.0 

1275.0 

1020.0 

0.0 

693.7 

11.7 

0.0 

1252.0 

693.7 

693.7 

2775.0 

4-38 

4.5 

4.5 

57.8 

4.5 

0.2 

4.5 

12.8 

4.5 

4.5 

4.5 

4.5 

57.8 

31.2 

4.5 

4.5 

4.5 

0.0 

4.5 

577.6 

12.8 

4.5 

4.5 

4.5 

4.5 

7 

5 

0 

5 

0 

5 

0 

5 

5 

5 

5 

0 

2 

5 

5 

4 

0 

5 

0 

0 

3 

5 

5 

7 

. 

Descriptfon 

· 

· . . .  · 

·. 

Pseudotsuga (douglas fir) 

Quercus (oak) 

Range 

Rhizophora (red mangrove) 

Rice 

Robinia (black locust) 

Rye 

Sabal (cabbage palmetto) 

Salix (willow) 

Sapium (chinese tallow tree) 

Sassafras 

Scrub 

Scrub woodland (Guenther) 

Serenoa (saw palmetto) 

Southeast/Western 
Deciduous Forest 

Southeast Mixed Forest 

Snow 

Sorbus (mountain ash) 

Sorghum 

Soybean 

Southern pine (Guenther) 

Swietenia (W.  Indies 

mahogany) 

Taxodium (cypress) 

Thuja (W. red cedar) 

Table 4-3.  Summer Biogenic Emission Flux Factors (µg-compound m·2hr- 1
Biogenic Compounds by Vegetation Category 

)  for Principal 

EPA/600/R-991030 

•··  V¢g~t~tf~ hf ·:.·::  .... ······:·· 
. Latj<l.•9?Y~i-•·.· 
•.".··•··· .. ··•Code• ........ 

.. 
~:;:;> 

:. 

.. 

. 

,:.~~:]'~''G ...•. · ••..  lt:r~\11(  .) . •· 
/ • ... ·  <·.·· > ..... 
42.5 

42.5 

'·. 

. ....  ...  .. •<.:.:::.;  ...... 
1/. 
l_./[~et":) 
:.:::"  ..... .;.·.•."·.· . 
. 
.·· 
··.:-::..  C•< 
•·.• 
... 

••• 

.··. 

·.· 

693.7 

4.5 

•<\ 
···:••: .. 
" 
.. .... 

: 

.·LeafArea··  .. 
rn4ei(iU~ 

·················tP":~)···· .. f. 

5 

. 

< 

·.· ::.• 
I 
.. .. "f':::,·· 

" 

./  <( 
: · .. 

: 

..

·.  \  :; 
. :::: 
.:" ... :.. 

:m  \ 

....... •.•.•.· ..• 
.-.·••:( 
·.·  c•>·  ··•···· 
.:.:·•··········· 

0 

7 

0 

0 

0 

5 

0 

0 

5 

5 

5 

0 

5 

3 

5 

0 

4 

3 

Tilia (basswood) 

Tobacco 

Tsuga (Eastern hemlock) 

Tundra 

BEIS urban forest 

BEIS urban grass 

Ulmus (American elm) 

BEIS other urban (barren) 

BEIS urban (.2 grass/.2 

forest) 

Urban trees (.5 Harf/.5 Cont) 

Vaccinium (blueberry) 

Washingtonia (fan palm) 

Water 

W Coniferous Forest 

(Guenther) 

Woodland/crop land 

(Guenther) 

Wetland forest (Guenther) 

Wheat 

Western Mixed Forest 

(Guenther) 

Western Woodlands 

(Guenther) 

Tili 

Toba 

Tsug 

Tund 

Ufor 

Ugra 

Ulmu 

Uoth 

Urba 

Utre 

Vacc 

Wash 

Wate 

Wcnf 

0.0 

79.3 

2411.7 

1988.7 

56.2 

42.5 

0.0 

58.8 

158.7 

120.6 

663.7 

140.5 

42.5 

0.0 

235.2 

256.7 

1295.0 

150.7 

920.0 

84.3 

693.7 

0.0 

4.5 

0.2 

4.5 

57.8 

4.5 

0.0 

408.6 

161.9 

200.5 

12.5 

5140.0 

1000.0 

42.5 

5950.0 

0.0 

42.5 

42.5 

0.0 

959.0 

693.7 

693.7 

0.0 

4270.0 

1120.0 

1320.0 

4.5 

4.5 

4.5 

0.0 

4.5 

Wdcp 

2550.0 

663.0 

2053.0 

8.7 

Wetf 

Whea 

Wmxf 

3820.0 

15.0 

5720.0 

923.0 

6.0 

620.0 

1232.0 

0.2 

9.0 

192.5 

530.0 

4.5 

Wwdl 

525.0 

250.0 

360.0 

4.5 

*Guenther references biogenic emission factors taken from  Guenther et al. (I 994) using A VHRR (Advanced Very 
High Resolution Radiometer) satellite imagery. 

4-39 

Table 4-4.  Winter Biogenic Emission Flux Factors  (µg-compound m-2hr"1
Biogenic Compounds by Vegetation Category 

)  for Principal 

Vegetation or 
Land Cover 

Code 

Abie 

Acac 

Acer 

Aesc 

Aila 

Aleu 

Alfa 

Ainu 

Amel 

Asim 

Avie 

Bari 

Barr 

Betu 

Borf 

Isoprerie 

' 

:·: .. ::T 

........ ,.,."'' 
"' lJ".ll". ·:·  . 

...... 
•  '"'~'  'V\.  .~S.::: 

1:-::.:  ..  _.::'.· 

..., ...  :., 
. 
....... ·.·: 

.....  : \  1:.: 

· ... ·. 

:  }· 

170.0 

5100.0 

2775.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

42.5 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

42.5 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

693.7 

0.0 

0.0 

0.0 

640.0 

706.0 

634.0 

Burne 

42.5 

42.5 

693.7 

Carp 

Cary 

Casp 

Cast 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

4-40 

•.. 

i~~·t~i~ :: ~¢.~{At~i~·  • .. ·.·.·.-···  :··:·:·:·"':···. 
::~~~1~~f-'•  .·:·:._.·.:.· ... ··:-::--· 

;:::'  .··  : 

.. ... -.::::.,, 
:.·":./··· 

···... 

:::-

;.· 

,. 

:· 

C\<>.,., :·, ... · . '  .... :::.: .. , 

,.,:,~.·,~· 

....... ·. 

....  -· 

.... ,:·:·:::.:·./ 
.· 
:  .. ·  ·· ... · 
::-
.. · :)  : ,:-.:.·: 

.· 

.... ·:-.:-· . .  · 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

12.8 

4.5 

4.5 

4.5 

4.5 

256.7 

0.0 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

7 

5 

5 

5 

5 

5 

0 

5 

5 

5 

5 

0 

0 

5 

6 

5 

5 

5 

5 

5 

Abies (fir) 

Acacia 

Acer (maple) 

Aesculus (buckeye) 

Ailanthus 

Aleurites (tung-oil 

tree) 

Alfalfa 

Alnus (European 

alder) 

Amelanchier 
(service berry) 

Asiminia (pawpaw) 

A vicennia (black 

mangrove) 

Barley 

Barren 

Betula (birch) 

Boreal forest 
(A VHRR/G. *) 

Bumelia (gum 

bumelia) 

Carpinus (hornbean) 

Carya (hickory) 

Castano psis 
(chinkapin) 

Castanea (chestnut) 

Table 4-4.  Winter Bicigenic Emission Flux Factors  (µg-compound m-2hr-1
Biogenic Compounds by Vegetation Category 

}  for Principal 

EP A/600/R-99/030 

Casu 

29750.0 

42.5 

693.7 

Cata 

Cedr 

Celt 

Cerc 

Cham 

Ci tr 

Cnif 

Conf 

Corn 

Coru 

Co ti 

Cott 

Crat 

0.0 

79.3 

0.0 

0.0 

0.0 

0.0 

1269.3 

1295.0 

0.0 

0.0 

0.0 

0.0 

170.0 

340.0 

2775.0 

42.5 

0.0 

680.0 

1353.0 

1400.0 

1548.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

693.7 

835.0 

870.0 

0.0 

0.0 

0.0 

0.0 

0.0 

Cs wt 

1050.0 

660.0 

770.0 

De sh 

Dios 

Eu ca 

Fagu 

Frax 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

29750.0 

1275.0 

693.7 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

4-41 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

577.6 

4.5 

4.5 

256.7 

4.5 

0.2 

57.8 

4.5 

4.5 

4.5 

4.5 

7 

5 

7 

5 

5 

7 

5 

9 

6 

0 

5 

5 

0 

5 

0 

5 

5 

5 

5 

Casuarina (Austl 

pine) 

Catalpa 

Cedrus (Deodar 

cedar) 

Celtis (hackberry) 

Cercis (redbud) 

Chamaecyparis 
(prt-orford cedar) 

Citrus (orange) 

BEIS conifer forest 

Conifer forest 
(AVHRR,G.) 

Corn 

Cornus (dogwood) 

Cotinus (smoke tree) 

Cotton 

Crataegus (hawthorn) 

Herbaceous Wetlands 

(AVHRR, G.) 

Desert shrub 
(AVHRR, G.) 

Diospyros 
(persimmon) 

Eucalyptus 

Fagus (american 

beech) 

Fraxinus (ash) 

Table 4-4.  Winter Biogenic Emission Flux Factors  (µg-compound m·2hr- 1
Biogenic Compounds by Vegetation Category 

)  for Principal 

Vegetation or 
Land Cover 

Code 

Gled 

Gord 

Gras 

Gymn 

Hale 

Harf 

Hay 

Ilex 

Jug! 

Juni 

Lagu 

Lari 

Liqu 

Liri 

Mac! 

Magn 

Malu 

Meli 

Isoprene •. 

.·. •. · 1'etp6tje  L ·' 

··.· 

.  "'''<::" 
"""  > 

. 

, .... ·:·  :: 

:,.:,,,o;:,.;:.:::;;.:;; 

,.:./'.. Ix•·, .. ,·:  ·:•<t··•  . :..'::· 

\•'(: .. : ,:,:.:; 

. ;;.;;;.,., ...  ,.,, 

... , .... 

~!AfQ liiiilii: t~~t'Al'ea  , ... 
i~'~i~~~~: 

··> 

,·.: •. 

·: 

:  <i  7 
:·>  : ... , 

· · .:,<:.:ri,;;, ',::·:·:" .. ,., ... ,; .. ,: .. , "";': :/'•/': :::: 
,  ..•  ,;.,.,;.,;.; 

.;:·:•::.:'::/ . 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

42.5 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

371.0 

185.0 

0.0 

85.0 

0.0 

0.0 

693.7 

0.0 

79.3 

476.0 

1295.0 

42.5 

42.5 

693.7 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

42.5 

1275.0 

693.7 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

4-42 

4.5 

4.5 

57.8 

4.5 

4.5 

4.5 

12.8 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

4.5 

5 

5 

0 

5 

5 

3 

0 

5 

5 

7 

5 

5 

5 

5 

5 

5 

5 

5 

Gleditsia 

(honey locust) 

Gordonia 

(lob lolly-bay) 

Grass 

Gymnocladus (KY 

coffeetree) 

Halesia (silverbell) 

Hardwood forest 

(AVHRR, G.) 

Hay 

Ilex (holly) 

Juglans (black 

walnut) 

Juniperus (east. red 

cedar) 

Laguncularia (white 

mangrove) 

Larix (larch) 

Liquidambar 
(sweetgum) 

Liriodendron (yellow 

poplar) 

Maclura 

( osage-orange) 

Magnolia 

Malus (apple) 

Melia (chinaberry) 

Table 4-4.  Winter Biogenk Emission Flux Factors  (µg-compound m·2br-1
Biogenic Compounds by Vegetation Category 

)  for Principal 

EP A/600/R-99/030 

·~····  X.,~(i~~je(::  ·:•• 

Mixf 

:<:.••········  ;::: 

·\··········<\:;.•?·••·•··•·1····· 

0.0 

1077.0 

·.. 
.... 
581.0 

·•  ··.•.:: :~~f~~~0~!.::: 

4 

.··. 

•··;:.•·::•. 

4.5 

Moru 

Mscp 

Nmxf 

Nyss 

Oak 

Oats 

Ocdf 

Ofor 

Oksv 

Os tr 

Othe 

Oxyd 

Pacp 

Past 

Paul 

Pean 

Pers 

Pice 

Pinu 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

175.0 

1100.0 

850.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

217.0 

0.0 

313.0 

0.0 

100.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

188.0 

0.0 

183.0 

0.0 

200.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

42.5 

255.0 

693.7 

23800.0 

5100.0 

2775.0 

79.3 

2380.0 

1295.0 

4-43 

4.5 

12.8 

4.5 

4.5 

4.5 

256.7 

4.5 

4.5  . 

4.5 

4.5 

57.8 

4.5 

35.3 

57.8 

4.5 

12.8 

4.5 

4.5 

4.5 

5 

0 

1 

5 

6 

0 

6 

0 

5 

0 

5 

0 

0 

5 

0 

5 

7 

3 

•···· 

.·······••·••/}}·····•······························ 

Mixed forest 
(AVHRR, G.) 

Morus (mulberry) 

Misc crops 

Northern Mixed 

Forest (AVHRR, G.) 

Nyssa (blackgum) 

BEIS oak forest 

Oats 

BEIS other deciduous 

forest 

Open forest 

Oak Savannah 
(A VHRR, Guen) 

Ostrya 

(hophombeam) 

·  Other (unknown, 

assume grass) 

Oxydendrum 
(sourwood) 

Pasture cropland 
(AVHRR, G.) 

Pasture 

Paulownia 

Peanuts 

Persea (redbay) 

Picea (spruce) 

Pinus (pine) 

Table 4-4.  Winter Biogenic Emission Flux Factors  (µg-compound m·2hr·1
Biogenic Compounds by Vegetation Category 

)  for Principal 

Vegetation or 
Land Cover 

Cade 

Isaprene 

··. 

. 

Terpene  •· 
.. : Lx 
·:\: 
.. 
.................  ······· 
... 

····· 

~~~~: 

·•·.  ·.·.····.·.·. 

.··. 
;<::::.:. 

;Nb 

· L:eafAi:ea 
i~ciexcrh• 

..;":'::--:  ... : . .•.• : t  trf2)  . 

: 

Plan 

Plat 

Popu 

Po ta 

Pros 

Prun 

Pseu 

Quer 

Rang 

Rhiz 

Rice 

Ro bi 

Rye 

Sabi 

Sali 

Sa pi 

Sass 

Scru 

Scwd 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

170.0 

2720.0 

2775.0 

0.0 

0.0 

42.5 

0.0 

0.0 

0.0 

0.0 

0.0 

42.5 

0.0 

0.0 

0.0 

0.0 

0.0 

693.7 

0.0 

0.0 

0.0 

5950.0 

42.5 

693.7 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

o.o 

0.0 

0.0 

0.0 

332.0 

332.0 

Sere 

14875.0 

42.5 

693.7 

Shrf 

0.0 

0.0 

0.0 

4-44 

4.5 

4.5 

4.5 

92.15 

4.5 

4.5 

4.5 

4.5 

57.8 

4.5 

0.2 

4.5 

12.8 

4.5 

4.5 

4.5 

4.5 

57.8 

31.2 

4.5 

4.5 

5 

5 

5 

0 

5 

5 

7 

5 

0 

5 

0 

5 

0 

5. 

5 

5 

5 

0 

2 

5 

0 

· · ·: ... ·  )Jes'?J,iption 

:.  :"": 

Planera (water elm) 

Platanus (sycamore) 

Populus (aspen) 

Potato 

Prosopis (mesquite) 

Prunus (cherry) 

Pseudotsuga (douglas 

fir) 

Quercus (oak) 

Range 

Rhizophora (red 

mangrove) 

Rice 

Robinia (black locust) 

Rye 

Sabal (cabbage 

palmetto) 

Salix (willow) 

Sapium (chinese 

tallow tree) 

Sassafras 

Scrub 

Scrub woodland 
(AVHRR,G.) 

Serenoa (saw 

palmetto) 

SE/W Deciduous 

Forest (A VHRR, G.) 

Table 4-4.  Winter Biogenic Emission Flux Factors  (µg-compound m·2hr- 1
Biogenic Compounds by Vegetation Category 

)  for Principal 

EPA/600/R-99/030 

••  >·yeg~~tip$ ~~ "){ t  f~hrii~I  •· 
···.· 

·.=:::.:.=::: 

':f,@4 .. ¢0.yer.•··  =:=::==:: 
(;Cid~>·  ('.I~~~~ 
Smxf 

:;: 

.............. : ........ 

,, 

:.••• 

:r>  ::•::::::.:  !  :::  ;:··:·.·:·: 

I  '':•'(':(.:  J\ 

1:: .. :.·.:: 

~:>\ /~L  ¥9£~;· 1:::1~i~-j~i!~~  ·:t~itrAi&~r I! ;}!~!,r ) ' "'·'' T L 

: ~ro~i~~f :1:: 

., .......  .:;:.\•·•.•:·.  ···"" 

1•·•··•·•)  · .. •··'········ >:: .. ···>·:••.\ .. :>  i• 

,,,.-::..;"',. ::.'···:.: .. 

: 

ii<y,;.::;: 

. 

.'•.' 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

1500.0 

500.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

1963.0 

1052.0 

Snow 

Sorb 

Sorg 

Soyb 

Spin 

Swie 

42.5 

42.5 

693.7 

Taxo 

Thuj 

Tili 

Toba 

Tsug 

Tund 

Ufor 

Ugra 

Ulmu 

Uoth 

Urba 

Utre 

42.5 

170.0 

0.0 

0.0 

79.3 

0.0 

0.0 

0.0 

0.0 

0.0 

0.0 

1275.0 

1020.0 

0.0 

0.0 

693.7 

2775.0 

0.0 

0.0 

158.7 

1295.0 

0.0 

631.0 

0.0 

0.0 

0.0 

0.0 

469.0 

0.0 

0.0 

0.0 

154.0 

102.0 

700.0 

960.0 

528.0 

4-45 

4.5 

0.0 

4.5 

577.6 

12.8 

4.5 

4.5 

4.5 

4.5 

4.5 

256.7 

4.5 

0.2 

4.5 

57.8 

4-.5 

0.0 

12.5 

4.5 

2 

0 

5 

0 

0 

3 

5 

5 

7 

5 

0 

7 

0 

0 

0 

5 

0 

0 

6 

SE Mixed Forest 

(AVHRR, G.) 

Snow 

Sorbus (mountain 

ash). 

Sorghum 

Soybean 

Southern pine 
(AVHRR, G.) 

Swietenia (W. Indies 

mahogany) 

Taxodium (cypress) 

Thuja (W. red cedar) 

Tilia (basswood) 

Tobacco 

Tsuga (Eastern 

hemlock) 

Tundra 

BEIS urban forest 

BEIS urban grass 

Ulmus (American 

elm) 

BEIS other urban 

(barren) 

BEIS urban (.2 
grass/.2 forest) 

Urban tree (.5 Harf/.5 

Cont) 

Table 4-4.  Winter Biogenic Emission Flux Factors  (µg-compound m-2hr- 1
Biogenic Compounds by Vegetation Category 

)  for Principal 

Vegetation or 
Land Cover 

Code 

Yacc 

Isoprene 

Terpene 

OtherVOCs 

0.0 

0.0 

0.0 

Wash 

5950.0 

42.5 

693.7 

Wate 

Wcnf 

Wdcp 

Wetf 

Whea 

Wmxf 

0.0 

0.0 

0.0 

3500.0 

1120.0 

1200.0 

0.0 

0.0 

0.0 

0.0 

630.0 

1047.0 

877.0 

628.0 

0.0 

620.0 

0.0 

330.0 

Wwdl 

0.0 

250.0 

360.0 

··No 

. . .  ·: ..........•... 

4.5 

4.5 

0.0 

4.5 

8.7 

0.2 

192.5 

4.5 

4.5 

· ...  ··.Leaf Area~ 
···•1ndex(mi··· 
m·~) ··•·· :·:  . 
5 

•·. 

5 

0 

5 

2 

3 

0 

3 

3 

Description 

......... 

Vaccinium 
(blueberry) 

Washingtonia (fan 

palm) 

Water 

Western Coniferous 
Forest (A VHRR, G.  ) 

Woodland/cropland 

(AVHRR, G.) 

Wetland forest 
(AVHRR, G.) 

Wheat 

Western Mixed Forest 

(AVHRR, G.) 

Western Woodlands 

* A VHRR/G. references biogenic emission factors from Guenther et al (1994) which used land use classifications 
from Advanced Very High Resolution Radiometer (AVHRR) satellite imagery. 

(AVHRR, G.) 

The BEIS-2 applies environmental correction factors to account for the effect of leaf temperature 
and visible solar radiation on isoprene (Pierce et al., 1998).  Specifically: 

(4-19) 

where: I is the adjusted isoprene emission flux, 

Is is the isoprene emission flux standardized to leaf temperature 30°C and PAR of 1000 

micromoles/m2/sec. 

The light adjustment factor CL  is estimated by: 

cL =  co:er.,PAR) /  Vo +0:2PAR2) 

(4-20) 

where: o: =  0.0027, and CLI= 1.066 are empirically derived coefficients.  The leaf temperature 
adjustment factor CT is derived from laboratory data and is computed using the following formula: 

(4-21) 

where:  Cn = 95,000 J mo1-1 

T5  is the standardized temperature (303°K), 
R is the ideal gas constant (8.314 °K mol- 1
), 
Cn = 230,000 J mo1- 1
Tm= 314°K. 

,  and 

PAR is computed as a function of height by: 

P ~ = P ~ ( exp[-0.42LAizD 

(4-22) 

During March 1998, the factor used in BEIS-2 within Models-3 to convert solar radiation values 
from watts per square meter (W/m2
)  to micromoles per square meter-second (µm/m 2-sec)  was 
changed from 2.2982 to 2, based on Alados et al. (1996).  This had the effect of reducing PAR 
by ~ 15 percent and isoprene emissions by ~ 5 percent based on limited simulation tests for the 
July 1995 ozone maximum period. 

Emissions for VOCs other that isoprene are assumed to vary only as a function of leaf 
temperature in accordance with the following: 

E =Es* exp[0.09 (T-Ts)] 

(4-23) 

where: Es is the standardized emission flux for monoterpenes and other VOCs, 

T is the leaf temperature in degrees Kelvin, and 
Ts is the standardized temperature (303°K). 

The soil NO emission flux factors used in BEIS-2 were adapted from Williams et al. (1992).  For 
temperatures greater than 0°C, soil NO temperature corrections follow the formulation of 
Williams et al. As follows: 

NO =N00  * exp[0.071(T-Ts)] 

(4-24) 

where  NO is the adjusted soil NO emission flux, 

N00  is the emission flux standardized to a soil temperature T5 of30°C, and 
T is the soil temperature. 

NO emissions at temperatures less than 0° C are set to zero.  During March 1998, it was decided 
to cap the exponential increase of soil NO emissions with tempe!atures above 30°C, based on the 
findings of Yienger and Levy (1995). 

The inclusion ofBEIS-2 into MEPPS biogenic emission modeling required creating an efficient 
connection such that BEIS-2 is easy to use from within MEPPS, and also so that the input and 
output data related to BEIS-2 can take advantage ofMEPPS and Models-3 data handling and 
analysis features.  The changes included: 

• 

• 

• 

• 

• 

BEIS-2 is now automatically invoked from MEPPS ·or the Study Planner when the user 
requests that biogenic emission factors be calculated.  It is not necessary to work directly 
with the BEIS-2 software. 

Solar radiation and temperature input files are not directly supplied by the user when 
BEIS-2 is run.  These meteorological data are supplied automaticall.y from MCIP files for 
the case (time period) specified, presuming that MMS and MCIP have been run for that 
period. 

Within MEPPS, BEIS-2 can now use ARC-INFO® generated grids and use gridded 
surrogate data to disaggregate county-level data to grid cells. 

BEIS-2 results are in terms of hourly emissions per grid cell within Models-3. 

Biogenic emission data are subject to the extended quality control and reporting features 
in MEPPS, and visualization using the ARC-INFO® based GIS-View feature. 

The MEPPS uses BEIS-2 by allowing the user to assume summer, winter seasons, or access frost 
data in order to allow for the seasonal ch~ge in vegetative biomass.  For summer and winter 
conditions, vegetation genus and emission factors for isoprene, monoterpene, other biogenic voe 
emissions, emission factors for NO, and the leaf area index (LAI) are provided based on the work 
of Geren et al. (1994).  Frost data include the federal identification protocol (FIPS) codes by state 
and county along with the biomass of the first and last day of summer.  The MEPPS  draws 
hourly temperature and solar radiation from MCIP for use in the biogenic processor.  Land cover 
and vegetation are provided in gridded ARC/INFO coverage.  The biogenic modeling input data 
are then provided to BEIS-2 and output as hourly gridded emission values of the VOCs isoprene, 
monoterpene, and "other" (unspecified); along with NO to the EMPRO speciation processor, 
where they are grouped with chemical species from other source types dep1::nding upon the 
conventions of either the CB4 or RADM2 speciation split factors (Section 4.2.5). 

#### 4.2.4.2 Mobile Source Emissions 

Emissions from mobile sources to the air are established as one of the primary contributors to 
pollution problems in many localities.  Unlike many anthropogenic emissions, mobile source 
emissions are strongly affected by the rapid variations of atmospheric temperature and 
anthropogenically-influenced, geographically-varying factors.  Consequently, it is necessary to 
model hourly mobile emissions, rather than to temporally disaggregate annual totals.  The 
capability to do this is in the MEPPS mobile-source emission processor.  For gaseous emissions 
the mobile source processor estimates hourly emissions ofVOC total organic gases (TOG), 
carbon monoxide (CO), and oxides of nitrogen (NOx) from on-road mobile sources (vehicles). 
The processor, which is located in the EMPRO module of MEPPS, uses a combination of air 
temperature data at l .S  m above the surface (provided by MMS through MCIP), mobile source 
emission factors (computed by the EPA regulatory model Mobile Sa for gaseous emissions from 
on-road mobile sources), fleet vehicle type composition, road type, and traffic data in the form of 
vehicle miles traveled (VMT) to generate emission estimates.  Vehicle emission controls, and 
Inspection and Maintenance programs may be accounted  for in the user-defined input settings for 
Mobile Sa (U.S. EPA,  1991; U.S. EPA, 1996).  The VMT data are usually available by county, or 
occasionally by specific road segments.  The resulting county or road segment-specific hourly 
emissions are spatially allocated to the cells of a user-defined study grid.  The gridded emission 
data are combined with corresponding species output from the mobile source particulate emission 
model, and allocated to different groups of lumped species for processing by the Speciation 
Processor (Section 4.2.S). 

Emissions of particulate species (particulate matter less that 10 micrometer and 2.S micrometers 
in diameter - PM10  and PM25,  respectively) from mobile sources are modeled analogously to 
gaseous emissions, using vehicle fleet composition, road ~ype (urban or rural), and VMT data. 
The mobile particulate emission factors are modeled with PARTS (U.S. EPA, 199S), the 
particulate companion of Mobile Sa.  Vehicle emission controls, Inspection and Maintenance 
programs, and use of reformulated fuel may be accounted for with the input settings of PARTS. 
In addition input information for road silt content and precipitation is required.  The particulate 
species modeled include lead, direct and indirect  particulate sulfate (any remaining sulfur in fuel is 
assumed to be exhausted as gaseous 802), total exhaust particulate (sum of lead, direct sulfate, 
and carbon including soluble organic material and other remaining carbon), and (separately) 
soluble organic fraction (SOF) and a remainder carbon portion (RCP), which are added to the 
mobile source emission output file. 

INPUT DATA 

Vehicle Fleet Information 

PARTICULATE  GASEOUS 
EMISSIONS 

EMISSIONS 
Silt Loa.ding 
Roe.d Type 

Controls 
Fuel Type 

v 

PARTS 
Mobile 
Emission 
Factors 
t 

v 

Mobile5a 
Mobile 
Emission 
Factors 
~ 

-
-

Compute Gridded Hourly 
Mobile Source Emissions 

[ Gridded VMT Data  ] 

" 

ME'IEOROLOGY] 

(MM5) 

MC;J 

Temperature 

. 
:  ~ SPECIA TION] 

OUIPUT 

- PROCESSOR 

Figure 4-5  General Flow of Mobile Source Emission Modeling 

The procedure by which mobile source emission data are estimated in MEPPS and flow diagrams 
for  the process are given in Section 6.5.3.4 of the Models-3  Volume  9B:  User Manual.  Briefly, 
the sequence is as follows: 

• 

• 

• 

VMT vehicle fleet data files are established for the study area (gridded spatial domain) of 
interest by either extracting county-level annual aggregate VMT data from the annual 
emission input files previously established by INPRO, or by loading "link-node" VMT data 
that are available for specific road segments (links) between nodes with geographic 
coordinates .  The link-node data are generally available only for selected urban areas and 
dates in conjunction with special studies.  The process is similar for VMT data which are 
occasionally available by quarter section (1/4 square mile) areas. 

ARC/INFO® spatial coverage files of surrogate data (Federal Highway Administration 
major highways, or United States Census Tiger Line road coverages are provided) are 
used  to spatially allocate county-level aggregate VMT data to the cells of the gridded 
domain being used with a particular study.  County area may also be used as a coarse 
surrogate coverage. 

Spatial allocation ratios are computed for use in assigning county-Kevel VMT data to the 
surrogate coverages.  The EMPRO calculates the proportion of roa.d links or county area 
in the spatial domain attributable to each grid cell overlain by the road link (FHA or 
TIGER/LINE) or county. 

Mobile source emission factors are computed using Mobile Sa and/or PART S- U.S. EPA 
regulatory models.  Mobile Sa requires user-defined information on driving operations, 
vehicle fleet and fuel use, and hourly ambient.temperature data.  PARTS requires user(cid:173)
defined information on vehicle fleet and fuel use, emission controls, non-attainment, road 
dust silt content, moisture, and percent of unpaved roads by geographic area (usually 
county).  Temperature and moisture data are from ithe MMS model as processed by 
MCIP. 

Hourly mobile emissions are computed for each grid cell, with adjustments applied to 
VMT to indirectly reflect the effect of temperature on gaseous emissions, and moisture on 
re-entrained- dust for particulate emissions. 

For mobile emissions ofVOC, hourly gridded mobile emissions are speciated using either 
RADM 2.0 or CB-4 emission profile splits. Speciation is not needed for particulate 
emission data. 

The hourly, gridded, speciated mobile emission data are merged with gridded, speciated 
hourly area-source emission data in OUTPRO, and converted to NetCDF 1/0 API format, 
to provide a two dimensional emission data set acceptable to the chemical-transport model 
and other Models-3 framework application_s. 

The Mobile 5a Model 

Detailed descriptions·of Mobile Sa are available from EPA publications (US EPA 1991, US EPA 
1996).  An optional Mobile 5b has also been adopted (US EPA 1996) to accommodate slight 
modifications in Inspection and Modification credits for hybrid fuels.  Both Mobile Sa and Sb are 
improvements in the Mobile 4.1  model (US EPA, July 1991).  Mobile 6 is in development and is 
tentatively expected in late 1999.  A new off-road mobile emission model is also due in late 1998. 
Portions of the  following description of Mobile are adapted from Wilkinson et al (1994).  The 
incorporation of Mobile 5a into MEPPS was extensively r•~vised from its treatment in GEMAP, 
including: 

• 

The application of Mobile Sa factors was revised to substantially reduce redundancy and 
computational space required.  The application of Mobile Sa in GEMAP computed 
specific emission factors for individual state/counties by area type/road type and hour. 
The combinations of hour and road type were derived by mapping operating modes 
(percent of hot and cold starts) to the hours and road types that used them, causing a 
duplication of Mobile Sa runs and factors.  It was necessary to recompute factors ifthere 
was a change in how the emission factors were used by hours or road type.  The computed 
mobile emission factors were merged into permanent (f~~the processing run) emission 
factor tables without regard to calendar year.  In MEPPS, Mobile 5a emission factors are 
computed and stored by calendar year, state/county, and operating mode.  Also, the 
system now has separate user-specified tables that allocate emission factors by hour and 
road type, using operating mode.  The tables are used directly in the emission estimate 
calculation.  In addition start and end times may now be specified for mobile emission runs 
rather than number of complete hours.  This has also shortened the processing time for 
county-level mobile emission processing. 

Calculations were separated into county-level, link-node level, and land-survey level 
options, depending upon the spatial detail ofVMT information available to the user. 

The ability to assign alias identification by state or county was added.  This allows the user 
to assign the values in a Mobile input file for a given area to a similar area which lacks 
specific input data. 

Geographic coverages of major highways (from the Federal Highway Administration) and 
all roads (TIGER-Line data) were made available as surrogate data to allow better spatial 
allocation of mobile emissions. 

Mobile emission data are now subject to the additional quality control and ARC-INFO® 
based visualization capabilities in MEPPS. 

• 

• 

• 

• 

Mobile Sa is an ANSI FORTRAN 77 computer program designed to estimate hydrocarbon (HC), 
carbon monoxide (CO), and oxides of nitrogen (NOx) emission factors for gasoline-fueled and 
diesel-fueled highway motor vehicles.  The computation methods that are 1~mbedded in Mobile Sa 
are based on the procedures that are presented in the Compilation of Air Pollutant Emission 
Factors -- Volume XX: Highway Mobile Sources (US EPA,  l 99S).  Mobile Sa computes 
emissions factors for eight vehicle categories in two regions of the country  (high-altitude and low(cid:173)
altitude ).  The eight vehicle categories include the following: 

• 
• 
• 
• 
• 
• 
• 
• 

LDGV - light-duty gasoline vehicles; 
LDGTl -- light-duty gasoline trucks (up to 6000 pounds); 
LDGT2 -- light-duty gasoline trucks (6001  to 8SOO pounds); 
HDGV -heavy-duty gasoline vehicles (over 8SOO  pounds); 
LDDV - light-duty diesel vehicles; 
LDDT-light-duty diesel trucks (0 to 8SOO  pounds); 
HDDV -heavy-duty diesel vehicles (over 8SOO pounds); and 
MC -- motorcycles. 

The Mobile Sa emission factors depend on various conditions including, but not limited to, 
ambient temperature, vehicle speed, operating modes, and vehicle mileagt:: accrual rates.  Much of 
the data required by Mobile Sa may be specified by the user through the Mobile input file.  Mobile 
computes emissions factors for any year from 1960 to 2020.  This date range is important for 
mobile-source emission estimated projections, and for the;: application of regulatory control 
factors.  Mobile version 5a.01  (which was modified for use in GEMAP/EMS-95 and later for 
MEPPS) is currently implemented in the EMPRO module ofMEPPS.  Consult the User's Guide 
to MOBILE 5,  EPA-AA-TEB-92-01 for further details on Mobile 5a.  Mobile model 
documentation may be obtained from the EPA Office of Mobile Sources World Wide Web home 
page located at:  http://www.epa.gov/OMSWWW/models.htm. 

A comprehensive discussion of the technical formulation of Mobile 5a is found in the above 
references.  This section contaiµs a description of modifications to Mobile 5a for use in GEMAP 
and later in MEPPS.  Mobile 5a was modified for use in EMS-95 to compute and report diurnal 
evaporative emissions factors (grams/mile) separately.  The unmodified Mobile 5a model 
computes and reports diurnal evaporative emission factors as part of a composite evaporative 
emission factor.  Also, Mobile 5a was modified to report a total non-diurnal evaporative emission 
factor.  The total non-diurnal evaporative emission factor includes hot soak, crankcase blow-by, 
running losses, and resting losses.  In the MEPPS EMPRO module, the Mobile SAS® table 
generators have been rewritten and segmented for greater computational efficiency, as have be~n 
the VMT and surrogate coverage grid processors and mobile source emission calculation 
procedures.  The technical procedure and rationale are the same, but processing time is shortened 
by at least a factor of two for regional modeling of spatial domains. 

The mobile-source emission processor relies on a SAS® lookup table to find appropriate emissions 
factors to compute the motor vehicle emission estimates.  The SAS® lookup table is generated 
through iterative runs of Mobile 5a.  The necessary number of runs of Mobile 5a is ,performed 
automatically, and is based on the standard Mobile ASCII input file supplied by the user.  For 
urban scale modeling, an input file for one state may suffice.  However, for regional modeling, 
multiple Mobile 5a ASCII input files can be concatenated to one file and run through the 
processor.  This capability is necessary to handle study domains where motor vehicle activity 
differs spatially and temporally.  For example, in a multiple-state study domain, it is unlikely that 
adjoining states have the same inspection and maintenance (I/M) program or the same vehicle 
fleet distribution (to name just two inputs to Mobile 5a).  Differences in regional inputs to Mobile 
5a such as JIM programs and vehicle fleet distributions result in different motor vehicle emissions 
factors.  A concatenated input file template (based on exarnple values used for the OTAG project) 
is provided which may be copied and edited.  In MEPPS, the Mobile input file (m5a.mv) is 
located in the directory structure at: 

$EMS_ HOME!project/$EMS _PROJECT/raw_ data/$EMS~ DOMAIN/common!. 

The only difference in the concatenated input file from the original input file is that the state title 
lines contain state (and where needed, county) FIPS identifier codes to identify the area for which 
each section of the Mobile 5a input file is applicable: 

• 
• 

FIPS state code -- Mobile input file applies to a state; or 
FIPS state code and FIPS county code -- Mobile input file applies to a particular county. 

The mobile-source emission processor varies the vehicle speed and ambient temperature records 
of the user-supplied input file and runs Mobile for each variation.  The Mobile source processor 
varies the vehicle speeds from 4 miles per hour to  64 miles per hour in increments of 2 miles per 
hour, although the user may adjust the increment (Mobile supports computation of emission 
factors for speeds between 2.S  MPH and 6S  MPH).  The mobile-source processor varies the 
minimum ambient temperature from S0° F to  110° Fin increments of 2 ° F, and the mobile source 
processor varies the maximum ambient temperature from 50 ° F to  110 ° F in increments of 2 ° F 
(Mobile Sa supports computation of exhaust emissions factors for temperatures between 0° F and 
110° F and evaporative emissions factors for temperatures between 40° F and 110° F).  The 
vehicle speed and ambient temperature values can be set interactively in EMPRO under the mobile 
source model, using the "Generate Mobile Emission Factors" screen. 

In MEPPS, the mobile-source processor generates two emission factor SAS® lookup tables. 
These are compact tables that contain the information held in seven generated SAS lookup tables 
in EMS-95.  They are: 

• 

• 

State/area type/facility type ..Q.iurnal emissions factors (I/M and non-I/M HC diurnal 
evaporative emissions factors for eight vehicle categories by state, minimum ambient 
temperature, maximum ambient temperature, and calendar year).  The  user specifies 
whether the operating mode varies by the hour of the day in the Temporal Allocation 
screen of the mobile emission model ofEMPRO); 

State/area-type/facility-type non-diurnal emissions factors (I/M and non-I/M HC 
[evaporative and exhaust], CO [exhaust only], and NOx [exhaust only] emissions factors 
for eight vehicle categories by state, area type [urban or rural], facility type [eg. interstate, 
collector, arterial, local], vehicle speed, ambient temperature, and calendar year.  The user 
can interactively specify whether the operating mode varies by hour of the day when 
establishing temporal profiles in the Temporal Allocation screen of the mobile emission 
model ofEMPRO. 

Operating mode is an important variable in the computation of motor vehicle emissions factors. 
Attention is specifically called to operating mode because the mobile source processor uses up to 
four different operating mode mixes to compute emissions factors for the Mobile Sa lookup tables 
(refer to the Mobile Sa user's guide for further discussion of operating modes). 

If area type/facility type records are to be used in the Mobile-generated lookup tables, the user 
must specify the mix of operating modes. The following example of operating mode mixes  was 
used for the Lake Michigan Ozone Study:  S% PCCN, 12.1 % PCHC, and 10.9% PCCC, where 
PCCN is percent VMT generated by non-catalyst vehicles in cold-start mode, PCHC is percent 
VMT generated by catalyst vehicles in hot-start mode, and PCCC is percent VMT generated by 
catalyst vehicles in cold-start mode.  If hourly records are used in the Mobile-generated lookup 
tables, the user must define the operating mode mixes by time of day, consistent with Mobile Sa 
guidance.  The following is an example of the use of four operating mode mixes: 

0000 to 0700 operating mode is 12.9% PCCN, 7% PCHC, and 17% PCCC; 
0700 to 0900 operating mode is 15% PCCN, 10% PCHC, and 15.4% PCCC; 
0900 to 1600 operating mode is 5% PCCN,  12.1 % PCHC, and 10.9% PCCC; 
1600 to  1800 operating mode is  10.1% PCCN, 9.1% PCHC, and 13.9% PCCC; 
1800 to 2200 operating mode is 5% PCCN, 12.1% PCHC, and 10.9% PCCC; 
2200 to 0000 operating mode is 12.9% PCCN, 7% PCHC, and 17% PCCC. 

As with the vehicle speeds and ambient temperatures, the operating mode mixes can be changed 
interactively on the Operating mode screen in THE MEPPS EMPRO module under Models, 
Mobile, Mobile Source. 

The mobile source processor generates emission factors for the following area type (urban or 
rural) and facility type (interstate, local, etc.) combinations: 

• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 
• 

rural/principal arterial - interstate 
urban/principal arterial - interstate 
rural/principal arterial - other 
urban/principal arterial - free/express-ways 
rural/minor arterial 
urban/principal arterial - other 
rural/major collector 
urban/collector 
rural/minor collector 
urban/minor arterial 
rural/local 
urban/local 

As with vehicle speeds, ambient temperatures, and operating modes; area type/facility type 
combinations can be changed interactively using the "Generate Mobile Emission Factors" screen 
under EMPRO, Models, Mobile, Mobile Source to edit the ASCII input file . When changes are 
made, the same changes are automatically made to the SAS® data sets. 

User Input Data 

The mobile source emission processor requires user-defined specifications using interactive 
screens and an input file.  While some of the specifications are required, some are not required.  In 
each case, there is provision to import the data file through IDA or in the mobile source processor 
ofMEPPS. 

The principal input data ASCII files to the mobile source model include the following 
(environment variables are defined in Chapter 6 of the Models 3 Volume 9B:  User Manual): 

• 

On-network link-specific area type/facility type ($EMS/onnet.mv) 

On-network link-specific percentage of vehicles that fall under an JIM program 
($EMS!onnetim. mv) 
On-network link-specific daily or hourly VMT ($EMS/onnetvmt.mv); 
Hourly or daily, link-specific or area type/facility type-specific vehicle mix 
Off-network, area type/ facility type average speed ($EMS_LOC/ojfnetspdmv) 
Off-network, ·area type/facility type VMT ($EMS/offnvmt.mv); 
Off-network, area type/facility type-specific vehicle mix profile ($EMS_LOC/offvmix.mv) 
Public land survey quarter sections area type/ facility type hourly or daily VMT, hourly 
speeds, and daily vehicle mix profile ($EMS/ofnvmtp.mv); 
Area type/facility type seasonal and daily adjustment factors ($EMS/adjstvmt.mv) 
Area type/facility type hourly average speed profile ($EMS_ CAT/spdadju. mv) 
On-network, link-specific hourly or daily average speed ($EMS_GRD!onnetspdmv) 
Area type/facility type hourly VMT fractional profile ($EMS/fracvmtu.mv) 

If off-network VMT is to be used to compute emissions estimates (typical when county VMT 
data are taken from regional emission inventories), the mobile emission processor can assign 
VMT to grid cells using ARC/INFO® coverage files for either county area, Federal Highway 
Administration (FHA) major highways, or TIGER/LINE road data.  These coverages for the 
contiguous United States are provided with MEPPS.  Subsets of the coverages  for the spatial 
domain of interest are extracted either when a grid is established or during mobile source 
processing. 

If Public Land Survey Quarter Section-based VMT is to be used to compute emissions estimates, 
the mobile source emission processor requires an ARC/Info® export data s1~t of the polygon(cid:173)
network system. 

Gridding VMT 

After the ASCII user input (foundation) data files have been read and checked, the user directs 
the mobile source emission processor to prepare the necessary network coverages (unless they 
were prepared when the grid was established).  The on-network system is prepared from the user(cid:173)
provided ASCII input files.  The off-network system is prepared via coun~v boundary, FHA, or 
TIGER/LINE data file extracts.  The polygon system is prepared from an ARC/INFO® export 
coverage.  The networks are gridded by overlaying the emission modeling grid on the network 
coverages in much the same manner that the area source spatial surrogates are gridded.  Each link 
in the networks (or area type/facility type polygon) are length (or area) apportioned to a grid cell 
in the emissions modeling domain.  Once the networks have been gridded, the corresponding 
VMT can also be gridded.  Equations 4-25 through 4-30 show how the on··network, off-network, 
and polygon VMT are gridded respectively. 

lenratioij,k,l,m = lengridij,k,l,m I lenparij,k 
VMTiJ,k,l,m =  vmtorigij,k * lenratioij,k,l,m 
cellpc~j.a,f,l,m = cellsumij,a,r,l,m I ctysumij,a,f 

4-56 

(4-25) 
(4-26) 
(4-27) 

VMTij,a,f,l,m =  vmtorigij,a,f * cellpctij,a,f,l,m 
areapctij,p,a.f.l,m = areagrdij,p,a.f,l,m I areaparij,p 
VMTij,p,a,r,i,m =  vmtorigij,p,a,r * areapctij,p,a,r,i,m 

where  lenratio is the percentage of a link in a given grid cell 

EP A/600/R-991030 

(4-28) 
(4-29) 
(4-30) 

lengrid is the length of a link in a given grid cell 
lenpar is the total length of the link 
VMT is the gridded VMT value 
vmtorig is the original VMT 
cellpct is the percent of the FHA or TIGER/Line~based area type/facility type 

roadway in a given cell 

cellsum is the total length of FHA or TIGER/Line-based area type/facility type 

roadway in a given cell 

ctysum is the total length of the FHA or TIGER/Line-based area type/facility type 

roadway in a county 

areapct is the percent of the polygon-based area type/facility type roadway in a 

given cell 

areagrd is the total length of the polygon-based area type/facility type roadway in a 

given cell 

areapar is the total length of the polygon-based area type/facility type roadway in 

a county 

i is the state index 
j  is the county index 
k is the link index for on-network systems 
1 is the east-west grid cell index 
m is the north-south grid cell index 
a is the area type index for off-network systems 
f is the facility type index for off-network systems 
p is the polygon index for Public Land Survey Quarter Sections network systems. 

For on-network and polygon VMT, the indices may also include an identifier for hour since 
polygon and on-network VMT can also be supplied on an hourly basis. 

Apply Default Information . 

If the user supplies limited data to the mobile-source emission processor, it can apply a variety of 
default values to compute motor vehicle emissions estimates.  These defaults include vehicle mix, 
speeds, VMT fractional profiles, and I/M vehicle percentages.  If the user supplies limited data, 
the mobile-source emission processor applies the following defaults: 

Default Vehicle Mix Profile 
Vehicle Class  Percentage 
LDGV 

0.618 

LDGTl 
LDGT2 
HDGV 

0.177 
0.077 
0.035 

LDDV 
LDDT 
HDDV 
MC 

0.008 
0.002 
0.077 
0.008 

Default Hourly VMT Fractional Profile 

Default Speed Profile 
Area Type· 

Facility~ 

0 
0 
0 
·o 
0 
0 
1 
1 
1 
1 
1 
1 

1 
4 
6 
7 
8 
9 
1 
2 
4 
5 
6 
9 

Speed 
60 
56 
56 
54 
40 
35 
60 
59 
55 
30 
55 
30 

Emission Hour  1 
Diurnal 
Other 

2 

3 

4 

5 

6 

7 

8 

9 

IO 

11 

12 

0.000  0.000 0.000  0.000  0.000  0.129 0.021  0.100  0.095  0.095 0.166 0.199 
0.016  0.010 0.003  0.006  0.010  0.026 0.053  0.064  0.055  0.048 0.050 0.052 

Diurnal 
Other 

13 

14 

15 

16 

17 

18 

19 

20 

21 

. 22 

23 

24 

0.079  0.116  0.000 0.000 0.000  0.000  0.000 0.000  0.000 0.000 0.000 0.000 
0.054  0.055  0.059 0.070 0.074  0.070  0.058 0.046  0.037 0.033 0.028 0.022 

If the user does not specify what the percentage of vehicles are under an I/M program, the mobile 
source emission processor assumes that no vehicles are under an I/M program.  The result is 
higher emission estimates. 

The mobile source emission processor does not override user-supplied data.  Default information 
are applied only when data are missing from the ASCII user input files. 

Add Temperatures and Adjust VMT 

Prior to computing the motor vehicle emission estimates, the mobile-source emission processor 
adds the gridded, hourly temperatures and adjusts the VMT to the specific modeling day.  The 
temperature data are used to obtain the correct emission factor from the Mobile emission factors 
lookup tables.  Through the application of Equations 4-31  through 4-34, the day-specific, diurnal 
and nondiurnal (other than full days), hourly VMT are computed, respectively. 

dvm~j.t,h,m,n =  ddayvmtij,t,m,n * adjday * adjmonth * dvmt_profij.l,h 
ovm~j.t,h,m,n = odayvmtij,l,m,n * adjday * adjmonth * ovmt_profij.l,h 
dvm~j.a.f.h,m,n = ddayvm~j.a.f.m.n * adjday * adjmonth * dvmt_profij,a,f,h 
ovm~j.a,f,h,m,n = odayvm~j,a,f,m,n * adjday * adjmonth * ovmt_profij.a,f,h 

(4-31) 
(4-32) 
(4-33) 
(4-34) 

where 

dvmt is the diurnal VMT; 
dayvmt is the total day diurnal VMT; 
adjday is the day-specific VMT adjustment factor supplied through an ASCII input 
· 
adjmonth is the month-specific VMT adjustment factor supplied through an ASCII 

file; 

input file; 

dvmt_prof is the hour-specific, diurnal VMT fractional profile factor; 
ovmt is the nondiurnal VMT; 
odayvmt is the total day nondiumal VMT; 
ovmt_prof is the hour-specific, nondiurnal VMT fractional profile factor; 
i is the state index; 
j  is the county index; 
h is the hour index; 
1 is the link identifier index; 
a is the area type index; 
f is the facility type index; 
m is the east-west grid cell index; and 
n is the north-south grid cell index. 

Compute Motor Vehicle Emissions Estimates 

The mobile source emission processor computes day-specific, gridded, hourly motor,vehicle 
emissions estimates ofVOC total organic gases (TOG), carbon monoxide (CO), and oxides of 
nitrogen (NOx)·  The actual emission estimate computations performed and maintained in the 
MEPPS data base are as follows: 

• 
• 
• 
• 
• 
• 
• 
• 

TOG for gasoline (noricatalyst/catalyst composit<:::) evaporative diurnal processes; 
TOG for gasoline (noncatalyst/catalyst composite) evaporative.nondiurn~ processes; •. 
TOG for gasoline (noncatalyst/catalyst composit<:::) exhaust operations; 
NOx for gasoline (noncatalyst/catalyst composite) exhaust operations; 
CO for gasoline (noncatalyst/catalyst composite) exhaust operations; 
TOG for diesel (noncatalyst/catalyst composite) exhaust operations; 
NOx for diesel (noncatalyst/catalyst composite) exhaust operations; and 
CO for diesel (noncaWyst/catalyst composite) exhaust operations. 

The mobile emission processor computes motor vehicle emissions estimates based on three types 
of VMT files: 

. 

. 

• 
• 
• 

On-network VMT; 
Off-network VMT; and 
Public land survey quarter sections VMT. 

The differences in the VMT types are: 

• 
• 
• 

On-network VMT are network link-specific; 
Off-network VMT are county/area type/facility type-specific; and 
Public land survey quarter sections VMT are county/polygon/area type/facility type(cid:173)
specific. 

The mobile-emission processor estimates motor vehicle emissions for all combinations ofVMT. 
Regardless of the VMT type that is used to generate motor vehicle emission estimates, all motor(cid:173)
vehicle emission estimates have some degree of uncertainty.  However, the most desirable motor(cid:173)
vehicle emissions estimates usually are generated from on-network, link-specific data, followed by 
public land survey quarter sections data, and finally off-network data.  Motor vehicle emission 
estimates that are generated from on-network data are not more certain, but that they are more 
spatially representative. 

Before the mobile-source emission processor computes the motor vehicle emission estimates, it 
computes a fleet composite emission factor.  Equations 4-44 through 4-67 show how the 
processor computes the fleet composite emission factors.  In all cases, the emission factors are 
extracted from the Mobile lookup tables based on the appropriate indices.  Please refer to the 
section on generating the Mobile emission factors lookup tables earlier in this section for 
additional information. 

If the user has provided non-I/M-specific VMT, the mobile source emission processor computes 
the fleet composite emission factors by vehicle classification for temperature and speeds 
combinations through the application of Equations 4-35 through 4-42. 

* 
* 

hcdifs.t  =  :Ejhcdij,s,t * vmixj 
hcexf;;,t =  :Ej hcexj,s,t * vmixj 
. 
xf  -~ 
no 
s.t  - .ujnOXXj,s,t  VIIllXj 
. 
f  -~ 
coex s,t  - ~j coexj,s,t  vm1xj 
hcevfs.1  =  :Ej hcevj,s,t * vmixj 
dhcexfs,t = :Ej  dhcexj,s,t * vmixj 
dno~.t =  :Ej  dnoxxj,s,t * vmixj 
dcoe~.t =  :Ej  dcoexj,s,t * vmixj 

(4-35) 
(4-36) 
(4-37) 
(4-38) 
(4-39) 
(4-40) 
(4-41) 
(4-42) 

where: 

j  is the vehicle classification index 
s is the speed index 
t is the temperature index 
hcdif is the hydrocarbon diurnal fleet composite emission factor for all vehicle types 

(grams/mile) 

hcdi is the non-I/M hydrocarbon diurnal emission factor (grams/mile) 
vmix is the vehicle mix profile 


hcexf is the hydrocarbon exhaust nondiurnal fleet composite emission factor for . 

gasoline vehicles (grams/mile) 

hcex is the non-1/M hydrocarbon exhaust nondiurnal emission factor for gasoline 

vehicles (grams/mile) 

noxf is the oxides of nitrogen exhaust nondiurnal fleet composite emission factor for 

gasoline vehicles (grams/mile) 

noxx is the non-1/M oxides of nitrogen exhaust nondiurnal emission factor for 

gasoline vehicles (grams/mile) 

coexf is the carbon monoxide exhaust nondiurnal fleet composite emission factor for 

gasoline vehicles (grams/mile) 

coex is the non-1/M carbon monoxide exhaust nondiurnal emission factor for gasoline 

vehicles (grams/mile) 

hcevf is the hydrocarbon evaporative nondiurnal fleet composite emission factor for 

gasoline vehicles (grams/mile) 

· 

hcev is the non-I/M hydrocarbon evaporative nondiurnal emission factor for gasoline 

vehicles (grams/mile) 

dhcexf is the hydrocarbon exhaust nondiurnal fleet composite emission factor for 

diesel vehicles (grams/mile) 

· 

dhcex is the non-1/M hydrocarbon exhaust nondiurnal emission factor for diesel 

vehicles (grams/mile) 

dnoxf is the oxides of nitrogen exhaust nondiurnal fleet composite emission factor for 

diesel vehicles (grams/mile) 

dnoxx is the non-1/M oxides of nitrogen exhaust nondiurnal emission factor for diesel 

vehicles (grams/mile) 

dcoexf is the carbon monoxide exhaust nondi.urnal fleet composite emission factor for 

diesel vehicles (grams/mile) 

dcoex is the non-1/M carbon monoxide exhaust nondiurnal emission factor for diesel 

vehicles (grams/mile) 

If the user has provided I/M-specific VMT data, the processor computes the fleet composite 
emission factors by vehicle category for temperature and speed combinations through the 
application of Equations 4-43 through 4-50. 

hcdifs,t  =  .I:j  ihcdij,s,t * vmixj 
hcexfs,t  =  I:j ihcexj,s,t * vmixj 
noxfs,t  =  .I:j  inoxxj,s,t * vmixj 
coexfs,t  = .I:j  icoexj,s,t * vmixj 
hcevfs,t  = .I:i  ihcevj,s,t * vmixi 
· dhcexfs,t =  .I:j  idhcexj,s,t * vmixj 
dnox~.t =  I:j idnoxxj,s,t * vmixj 
dcoexfs,t = .I:j  idcoexj,s,t * vmixi 

(4-43) 
(4-44) 
(4-45) 
(4-46) 
(4-47) 
(4-48) 
(4-49) 
(4-50) 

where: 

ihcdi is the I/M hydrocarbon diurnal emission factor (grams/mile) 

ihcex is the I/M hydrocarbon exhaust nondiurnal emission factor for gasoline vehicles 

(grams/mile) 

inoxx is the I/M oxides of nitrogen exhaust nondiurnal emission factor for gasoline 

vehicles (grams/mile) 

icoex is the I/M carbon monoxide exhaust nondiurnal emission factor for gasoline 

vehicles (grams/mile) 

ihcev is the I/M hydrocarbon evaporative nondiurnal emission factor for gasoline 

vehicles (grams/~ile) 

idhcex is the I/M hydrocarbon exhaust nondiurnal emission factor for diesel vehicles 

(grams/mile) 

idnoxx is the I/M oxides of nitrogen exhaust nondiurnal emission factor for diesel 

vehicles (grams/mile) 

idcoex is the I/M carbon monoxide exhaust nondiurnal emission factor for diesel 

vehicles (grams/mile) 

If the user has provided both I/Mand non-I/M VMT data, the mobile source emission processor 
computes the fleet composite emission factors by vehicle category for temperature and speed 
combinations through the application of Equations 4-51through4-58 This set of equations can be 
applied only to on-network VMT data because the I/M percentages are input to the mobile source 
emission processor on a link-specific basis. 

hcdifs,t  =  ~j ((ihcdij,s,t * imvmt1)  + (hcdij,s,t * (1  - imvmt1)))  *  vmixj 
hcexfs,t  =  ~j ((ihcexj,s,t * imvmt1)  + (hcexj,s,t * (1  - imvmt1))) * vmixj 
nox.fs,t  = ~j ((inoxxj,s,t * imvmt1)  + (noxxj,s,t * (1  - imvmt1))) * vmixj 
coex.fs.t  =  ~i ((icoexj,s,t * imvmt1)  + (coexj,s,t * (1  - imvmt1))) * vmixj 
hcevfs,t  =  ~i ((ihcevj,s,t * imvmt1)  + (hcevj,s,t * (1  - imvmt1))) ~ vmixj 
dhcex.fs.t  =  ~i ((idhcexj,s,t * imvmt1)  + (dhcexj,s,t * (1  - imvmt1))) * vmixj 
dnoxfs,t  = ~i ((idnoxxj,s,t * imvmt1)  + (dnoxxj,s,t * (1  - imvmt1))) * vmixj 
dcoex.fs,t  =  ~i ((idcoexj,s,t * imvmt1)  + (dcoexj,s,t * (1  - imvmt1))) * vmixj 

(4-51) 

(4-52) 
(4-53) 
(4-54) 
(4-55) 
(4-56) 
(4-57) 
(4-
58) 

where: 

imvmt is the fraction of vehicles that are under an I/M program; and 
I is the link identifier index. 

Many indices have been left out of Equations 4-35 through 4-58.  The composite emission factors 
that are generated are gridded, hourly values.  Because temperature is a gridded, hourly value, the 
temperature index tin Equations 4-31  through 4-54 implies that the composite emissions factor 
are gridded, hourly values. 

Once the fleet composite emission factors have been computed, the mobile source emission 
processor computes the motor vehicle emission estimates through the application of Equations 4-
59 through 4-66. 

4-62 

dhc  vee =  hcdif * divmt * 10**-3 
he  xee =  hcexf * ovmt * 10**-3 
noxxee =  noxf * ovmt * 10**-3 
co  xee =  coexf * ovmt * 10**-3 
he  vee = hcevf * ovmt * 10**-3 
dhc  xee = dhcexf * ovmt * 10**-3 
dnoxxee =  dnoxf * ovmt * 10**-3 
dco  xee = dcof * ovmt * 10**-3 

EPA/600/R-991030 

(4-59) 
(4-60) 
(4-61) 
(4-62) 
(4-63) 
(4-64) 
(4-65) 
(4-66) 

where: 

dhc _ vee is the gridded, hourly diurnal hydrocarbon emission estimate (kilograms) 
divmt is the gridded, hourly diurnal VMT (miles) 
he_ xee is the gridded, hourly exhaust hydrocarbon nondiurnal emission estimate for 

gasoline vehicles (kilograms) 

ovmt is the gridded, hourly nondiurnal VMT (miles) 
noxxee is the gridded, hourly exhaust oxides of nitrogen nondiurnal emission estimate 

for gasoline vehicles (kilograms) 

co_ xee is the gridded, hourly exhaust carbon monoxide nondiurnal emission estimate 

for gasoline vehicles (kilograms) 

he_ vee is the gridded, hourly evaporative hydrocarbon nondiurnal emission estimate 

for gasoline vehicles (kilograms) 

dhc _ xee is the gridded, hourly exhaust hydrocarbon nondiurnal emission estimate for 

diesel vehicles (kilograms) 

dnoxxee is the gridded, hourly exhaust oxides of nitrogen nondiurnal emission 

estimate for diesel vehicles (kilograms) 

dco_xee is (the gridded, hourly exhaust carbon monoxide nondiurnal emission 

estimate for diesel vehicles (kilograms) 

The mobile-source emission processor computes the final motor vehicle emission estimates file by 
summing the emission estimates computed by Equations 4-59 through 4-66 over each state 
identifier, county identifier, east-west grid cell identifier, north-south grid cell identifier, process 
type (EV for evaporative, EX for exhaust), technology type (1  for gasoline, 2 for diesel), and 
pollutant identifier (HC for hydrocarbon, CO for carbon monoxide, NOx for oxides of nitrogen). 

The PARTS Model 

The current version of PARTS was released in 199S by the U.S. EPA Office of Mobile Sources. 
The description given here is taken principally from the user's guide, A Draft User's Guide to 
PART5: A Program for Calculating Particle Emissions from Motor Vehicles  (U.S. EPA, 
199Sb ), which provides information in addition to that gi.ven in this section.  The guide is available 
from the Office of Mobile Sources Internet web site at hlttp:www.epa.gov/omsw/models.htm 
The PARTS is a FORTRAN program used to model emission factors needed to. estimate 
emissions from gasoline and diesel powered on-road vehicles.  It calculates particle emission 
factors in grams per mile for particle sizes 1 to 10 micrometers.  The emission factors include 
exhaust particulate matter and components, brake wear, tire wear, and re-entrained road dust. 
The program includes default data for most inputs, but allows user-supplied data for most items. 
The interactive aspects of running PARTS, as it is  provided by the Office of Mobile Sources, 
have been subsumed into MEPPS to allow the user to enter and edit data, specify program control 
flags, and run the model via a series of windows within the EMPRO mobile source processor. 

Controls specified within PARTS may vary spatially and temporally.  PARTS output report flags 
are not germane within MEPPS because the outputs are automatically processed with emissions 
from other emission sources in MEPPS internal format.  They flags used include the following 
items: 

• 

Vehicle fleet mix.  The user may specify of whether default or user··supplied VMT vehicle 
fleet mixes are used.  The VMT mix is the fraction of total VMT for all vehicles 
contributed to by each vehicle class.  The default VMT is based on national averages and 
trends over the years.  The trends reflect the sales shift from automobiles to light-duty 
trucks, and the increasing use of diesel engines in both light and heavy-duty trucks. VMT 
mix may vary spatially.  The following are the descriptions of the vehicle classes and 
corresponding Federal Highway Administration (FHA) and gross vehicle weight (GVW) 
used in PARTS. 

Vehicle Class 

FHA Class 

GVW (lbs) 

1 = LDGV (light-duty gasoline vehicle) 
2 =  LDGTl (light-duty gasoline truck, I) 
3 = LDGT2 (light-duty gasoline truck II) 
4 = HDGV (heavy-duty gasoline truck) 
5 = MC (motor cycle) 
6 = LDDV (light-duty diesel vehicle) 
7 = LDDT (light-duty diesel truck) 
8 = 2BHDDV (class 2B heavy duty diesel vehicle) 
9 = LHDDV (light heavy-duty diesel vehicle) 

10 =  MHDDV (medium heavy-duty diesel vehicle) 

1 

2B 

2A 

3,4,5 

6,7,8A 

1 
2A 
2B-8B 

<6000 
6001-8SOO 
>8500 

<6000 

6001-8500 

8501-10000 
10001-
19500 
19S01-
33000 
33000+ 

11  = IDIDDV (heavy heavy-duty diesel vehicle) 
12 =BUSES (buses) 

8B 

• 

Mileage accumulation rates.  The user may supply or use default mileage accumulation 
rates and vehicle registration distributions.  The mileage accumulation rate is the expected 
number of miles that a typical vehicle (for each class) is expected to travel in a year, 
divided by 100,000.  The rates must be provided for each vehicle type for each of 25 years 
(12 years for motor cycles). 

Inspection and maintenance.  The user specifies whether an inspection and maintenance 
program is assumed for gasoline vehicle vehicles only. 

Reformulated gasoline.  The specifies whether th1~ use and effects of reformulated gasoline 
on particulate emissions ~e assumed.  The effects are partially, based on the sulfur content 
of gasoline used.  An average sulfur weight percent of 0.034 is used for any gasoline used 
in all years prior to 2000.  If reformulated gasoline is used.for the year 2000 or later, the 
sulfur weight percent is assumed to be 0.0138. 

The purpose of MEPPS is to estimate temporally and spatially-varying emission data on a gridded 
basis.  This is consistent with some additional input data which PARTS requires be entered on 
spatial basis, which are defined as PARTS scenarios.  In EMPRO mobile source processing the 
user is prompted to edit existing or new scenario data.  The information includes: 

• 

• 

• 

• 

• 

• 

Region, calendar year, speed cycle (type of driving, ie. cruise), and average speed. 

Percent of unpaved road silt, paved road silt loading in gm /m2
number of wheels per vehicle. Modeled dust emission factor are highly sensitive to the 
unpaved road silt percentage which is extremely variable.  Consequently, measured data 
are advised when possible. 

,  and optionally, the average 

The number of days each year with greater than 0.01  inches of precipitation for use in 
modeling re-entrained dust.  This is  climatological average data.  In the future, the 
emission processing system may be modified to use  more spatially and temporally 
accurate modeled precipitation data by grid cell taken from MM5 and MCIP for the 
PARTS calculations. 

A scenario name may be applied.  In MEPPS a FIPS code a geographic identifier is 
entered which is used to apply PARTS emission factors computed for the comity level to 
the appropriate modeling grid cells. 

PARTS expects the user to define a maximum particle size cutoff (maximum allowed is 10 
micrometers).  In MEPPS the maximum size is defaulted to 10 micrometers to ensure that 
the full range of particle sizes are represented in the regional emission estimates. 

Average vehicle weight (lbs) 

PARTS Emission Factors 

The emission factors calculated by PARTS include the particulate pollutant compounds of lead, 
sulfate, soluble organic fraction particulate matter, remaining carbon portion particulate matter, 
and total exhausted particulate.  The lead and sulfate are formed from the lead and sulfur 
contained in the fuel.  The soluble organic fraction consists primarily of hydrocarbons coming 
from unburned or partially burned fuel and lubricating oil.  The remaining carbon portion consists 
of soot-like carbon (elemental carbon) and trace amounts of other compom~nts from the fuel and 
lubricating oil.  The total exhaust particulate is the sum of these four categories.  In addition to 
these categories of exhaust emissions, idle exhaust emissions (for heavy diesel vehicles only), 
brake wear, tire wear, fugitive dust, indirect sulfate, and gaseous sulfur dioxide are calculated by 
PARTS. 

The model calculates the emission factors for the 12 vehicle classes previously described and a 
fleet-wide average (estimated by VMT weighting of the emission factors for each of the 12 
vehicle classes.  The factors are composites of the emission factors for the 25+ years prior to the 
year of interest, in order to allow for the effects of older vehicles.  The composite emission factor 
for each vehicle class is calculated by weighting the emission factor calculated for each model year 
by the travel fraction for that model year, and then summing the 25 weighted factors.  The travel 
fraction of a model year is the fraction of VMT by a vehicle of that model year out of the total 
number of miles traveled by all model years' vehicles in that vehicle class. 

• 

. 
EFCOMPv =  EF mv  * TF mv 

. 

25 

m=l 

(4-67) 

where: 

EFCOMPv is the composite emission factor for vehicle class v~ 
is the emission factor for vehicle class v, model year m, 
EF m,v 
TF m,v 
is the travel fraction for vehicle class v, model year m. 

The overall travel fraction of a vehicle class represents the fraction of the total number of VMT of 
that class of the total highway VMT by all 12 classes.  The VMT fractions for each vehicle class 
are multiplied by the corresponding composite emission factors (EFCOMP v),  and the sum of the 
adjusted emission factors is reported as the emission factor for all vehicles. 

EF ALL =  :E EFCOMP v * TFCLASSv 

(4-68) 

where: 

EF ALL is the weighted emission factor for all vehicles, 
TFCLASSv is the VMT of vehicle class v. 

The emission factor for all vehicles, EF ALL, represents the grams/mile of ·~missions 

Lead Emission Factors for gasoline-fueled vehicles 

Lead particulate emission factors for. gasoline-fueled vehicles assume that almost all lead in the 
fuel is exhausted.  Therefore, the emission factors (grams/mile) depend on the lead content of fuel 
and fuel economy of the vehicle (miles/gallon).  The factors allow for the fact that the lead content 
of leaded fuel is much greater than for unleaded fuel.  The assumption is made that because diesel 
fuel has negligible lead content,  the lead emissions from diesel vehicles will be negligible.  The 
following formulae are used to compute lead emission factors for all gasoline-fueled automobiles 
and motorcycles, respectively. 

LEADm,Y =  PLNOCTm,Y * VLNOCTm.Y + PUNOCTm,Y * VUNOCTm,Y + 

PL YSCT m,Y  * VL YSCT m,Y  + PUYSCT m,Y,  * VUYSCT m,Y 

(4-69) 

where: 

m denotes a specified model year 
v denotes a specified vehicle class 
LEADm,Y is the lead particulate emissions for any given vehicle (grams/mile) 
PLNOCT m,Y  is the emissions for a non-catalyst, leaded fuel vehicle (grams/mile) 
PUN OCT m,Y  is the emissions for a non-catalyst, unleaded fuel vehicle (grams/mile) 
PL YSCT m,Y  is the emissions for a catalyst, leaded fuel vehicle (grams/mile) 
PUYSCTm,Y is the emissions for a catalyst, unleaded fuel vehicle (grams/mile) 
VLNOCTm,Y is the emissions for a non-catalyst, leaded fuel vehicle (grams/mile) 
VUNOCT m,Y  is the emissions for a non-catalyst, unleaded fuel vehicle (grams/mile) 
VL YSCT m,Y  is the fraction of catalyst leaded fuel vehicles 
VUYSCT my  is the fraction of catalyst, unleaded fuel vehicles 

The emission rate is adjusted for speed by the factor FEC such that: 

CLEADm,Y = FEC * LEADm,Y 

(4-70) 

where: 

CLEADmY is the lead emissions for a vehicle of model year m and vehicle class v, 
which has been adjusted for the effect of speed (grams/mile) 

FEC is 

l/SCF c 

(4-71) 

SCFc is the speed correction factor, based either transient driving cycle (c=l) or 
steady cruise driving cycle (c=2) 

SCF1 = 0.17930 + (0.038561  *SPEEDY)- (0.00041067 *SPEED/) 

SCF2 = 0.26929 + (0.054607 * SPEEDJ - (0.00069818 *SPEED/) 

(4-72) 

(4-73) 

SPEEDY is the average speed for vehicle class v (mph) - user input 

Motorcycles do not have catalytic emission controls, therefore organic emission factors are not 
calculated and sulfate emission factors are deemed negligible.  PAR TS  emission factors for 
motorcycles are almost entirely for lead particulate matter.  The lead particulate emission factor 
for 2-stroke engines is 0.33 g/mi and for 4-stoke engines 0.046 grams/mile.  For model years 
before 1978 there were 51percent4-stroke engines and 49 percent.2-stroke engines, computed as 
follows: 

LEADm,motorcycle =  [(0.49 * 0.33) + (0.51  * 0.046)] * PSL 

(4-74) 

For model years 1978 and later motorcycles are assumed to consist entirely of 4-stroke engines: 

LEADm,motorcycle = 0.046 * PSL 

(4-75) 

where: 

PSL is the fraction of all particles that are emitted based on a specified upper particle 
size cutoff (10 micrometers running in MEPPS) 

The specific derivations of the lead particulate emission formulae are given in Appendix 1 of the 
PART 5 user's guide (U.S. EPA, 1995b). 

Sulfate Emission Factors for Gasoline-Fueled Vehicles 

Particulate sulfate emission factors consist of direct and indirect sulfate material.  The direct 
sulfate is exhausted as sulfuric acid, and the indirect sulfate is formed later in the atmosphere from 
exhausted 802•  The indirect sulfate in the model is calculated based on the assumption that it 
consists entirely of ammonium sulfate and ammonium bisulfate.  The direct sulfate, indirect 
sulfate, and gaseous sulfate emission factors are computed in PARTS, and the emission factors 
reported as grams/mile traveled. 

The direct sulfate from non-catalyst vehicles using leaded fuel (includes ca.talyst-equipped vehicles 
which are misfueled, making the catalyst ineffective) is calculated as: 

DSULFN =  .002, for speeds equal to or less than 19.6 mph 

DSULFN =  .001, for speeds equal to or greater than 34.8 mph 

The direct sulfate from catalyst vehicles is calculated as : 

DSULFCm,v =  [FRACcat/noair (.005) + FRACcat/air (.016)] 
for speeds equal to or less than 19.6 mph 

DSULFCm,v = [FRACox/noair (.005) + FRAC3w/noair (.001) + FRACox/air (.020) 

+FRAC3w/air (.025)] 

for speeds equal to or greater than 34.8 mph 

(4-76) 

(4-77) 

(4-78) 

(4-79) 

For speeds between 19.6 and 34.8 mph, DSULFN and DSULFC are interpolated between 
Equations 4-76 and 4-77 and 4-78 and 4-79, respectively. 

where: 

m denotes a specified model year 
v denotes a specified vehicle class 
FRACcatlnoair is the fraction of vehicles which are catalyst equipped with no air pump 

FRACcat/air is the fraction of vehicles which are catalyst equipped with an air pump 
FRAC0x1noair  is the fraction of vehicles which are oxyg~m catalyst equipped with no air 

pump 

FRAC3wtnoair  is the fraction of vehicles which are 3-way catalyst equipped with no air 

pump 

FRAC0x1air  is the fraction of vehicles which are oxidation catalyst equipped with an air 

pump 

FRAC3wtair  is the fraction ofvehi~les which are 3-way catalyst equipped with air pump 

The direct sulfate from all gasoline-fueled vehicles is computed as: 

DSULF m,v  = CTLFRCm,v * DSULFCm,v + (1.0 -CTLFRCm) * DSULFN 

where: 

CTLFRCm v = CATFCT m v  (1-RMISm v) 
, 
the fraction of the vehicle class that has an effective catalyst 

,  . 

, 

(4-80) 

(4-81) 

The PAR T5 assumes that all sulfur in fuel is exhausted as either sulfate or gaseous sulfur dioxide 
(802).  Therefore when the direct sulfate emission factor is calculated, the remaining sulfur in the 
fuel is considered to be exhausted as 802•  The amount of sulfur remaining in the fuel after the 
direct sulfate emission factor has been determined must be calculated to find the amount of sulfur 
exhausted as 802 (grams/mile). 

The following equation is used to determine the fraction of sulfur in the fuel that has been directly 
converted to sulfate (DSULF m v  calculated in Equation 4-80 above).  The equation calculates 
direct sulfate as a function of the fuel sulfur content, DCNVRT (the fraction of sulfur in the fuel 
that is converted to direct sulfate), and the fuel economy. 

DSULFm,v = 13.6078 * (1.0 +WATER)* FDNSTY * SWGHT * DCNVRT 

I FEm,v 

(4-82) 

where: 

m denotes a given model year 
v denotes a given vehicle class 
DSULF m,v  is the direct sulfate emission factor (grams/mile) 
WATER is the weight ratio of seven water molecules to sulfate, 1.2857, based the 
estimate that at 50 percent humidity, seven water molecules bond with each 
sulfuric acid molecule 

FDNSTY is the fuel density in lb/gal (6.09 pounds/gallon) 
FEm v is the fuel economy 
SWGHT is the weight percent of sulfur content in fuel (.034, except for reformulated 

fuel phase II, for year 2000 and later . 013 8) 

DNVRT is the percent of sulfur in the fuel that is directly converted into sulfate (2 

percent) 

13.6078 is a unit conversion factor equal to (453.592 * 3.0)/100, where 453. 592 is 
equal to the number of gram in a pound, 3.0 is the weight ratio of sulfate to 
sulfur, and division by 100 corrects for the weight percent of sulfur, SWGHT 

IfDSULFCm,v and DSULFN (from Equations 4-76 through 4-79) are substituted in Equation 4-
82, one can solve for the :fractions of sulfur in the fuel that are converted to sulfate separately for 
catalyst and noncatalyst vehicles: 

FCNVRCm,.v = DSULFCm,v * FEm,v 

I (13.6078 * (1.0 +WATER)* 

FDNSTY* SWGHT) 

. 
FCNVRNmv = DSULFN * FEmv 

. 

FDNSTY * SWGHT) 

I (13.6078 * (1.0 +WATER)* 

(4-83) 

(4-84) 

where: 

FCNVRCm v is the fraction of the percent of fuel that is directly converted into sulfate 

FCNVRNmv is the fraction of the percent of fuel that is directly converted into sulfate 

for catalyst equipped vehicles 

. 

. 
for non-catalyst vehicles 

The gaseous sulfur emission factors, which are dependent on the above fractions, are calculated 
from the following equation: 

where: 

S02mv =  9.072 * FDNSTY * SWGHT * (1.0 -DCNVRT) I FEmv 
. 

. 
S02m,v is the gaseous ·sulfur emission factor of a vehicle 
9.072 is a unit conversion factor equal to (453.592 *2)/100, where 453.592 is the 
number of grams in a pound, 2 is the weight ratio of S02  to sulfur, and the 
division by 100 corrects for the weight percent of sulfur, SWGHT 

(4-85) 

Additional details concerning the calculation of gaseous sulfur emission factors are given in the 
PARTS user's guide (U.S. EPA,  1995b).  In addition to direct sulfate and gaseous sulfate 
emission factors, PAR T5 estimates an indirect sulfate emission factor by assuming that a fraction 
of the gaseous sulfur dioxide emissions is later converted in the atmosphere to sulfate material. 
Based on ambient sulfur and sulfate measurements in 11  cities, it is estimated that 12 percent of 
all gaseous sulfur is converted to sulfate.  Additional information on the cal~culation of indirect 
sulfate is given in the PAR T5 user's guide. 

· 

Sulfate Emission Factors for Diesel-Fueled Vehicles 

For diesel-fueled vehicles, PART5 calculates sulfate emission fact.ors again assuming that all 
sulfur in the fuel is exhausted as either sulfuric acid or gaseous sulfur dioxide.  The direct sulfate 
emission factor (grams/mile) is calculated using the following equation: 

DSULFm,v = 13.6078 * (1.0 +WATER)* FDNSTY * SWGHTD * 

DCNVRT I FEm,v 

(4-86) 

where: 

m is a specified model year 
y is a specified vehicle class 
DSULF m v is the direct sulfate emission factor for a vehicle (grams/mile) 
DCNVRT is the fraction of sulfur in the fuel that is converted directly to sulfate (2.0 

percent 

FDNSTY is the density of diesel fuel (7.11  pounds/gallon) 
FEm,v  is the fuel economy for a vehicle (miles/gallon) 
SWGHTD is the weight percent of sulfur in diesel fuel (0.25 for high sulfur fuel, 0.05 

for low sulfur fuel used in 1993  and later) 

WATER  is the weight ratio of seven water molecules to sulfate ( 1.2857) 
13.6078 

is a unit conversion factor equal to 493.592 * 3/100, where 493.592 is the 
number of grams in a pound, 3 is the weight ratio of sulfate to sulfur, and the 
division by 100 corrects for the weight percent of sulfur SWGHTD 

The gaseous sulfur emission factor is calculated by the following equation: 

S02m,v =  9.072 * FDNSTY * SWGHTD * (1.0 - DCNVRT) I FEm,v 

(Eq.4-

87) 

where: 

S02m v is the sulfur emission factor (grams/mile) of a vehicle 
9.072 is a unit conversion factor equal to (453.592 * 2/100) where 453.592 is the 
number of grams in a pound, 2 is the weight ratio of S02 to sulfur, and the 
division by 100 corrects for the weight percent of sulfur, SWGHTD 

The indirect sulfate emission factor, consisting mainly of ammonium sulfate and ammonium 
bisulfate is calculated using the following equation: 

ISULFmv = ICNVRT * S02mv * (3/2) * AMNWGT 

' 

' 

(4-88) 

where: 

ISULF m v is the indirect sulfate emission factor of a vehicle (grams/mile) 
ICNVRT is the fraction of S02  converted to sulfate in the atmosphere (12 percent) 
3/2  is the weight ratio of S02 to sulfate 
AMNWGT is the estimated weight ratio of the combination of ammonium bisulfate 

and ammonium sulfate in the atmosphere to sulfate (1.6) 

Total Exhaust Particulate Emission Factors for Gasoline-Fueled Vehicles 

The total exhaust particulate emission factors for light~duty gasoline fueled vehicles are calculated 
by summing lead, direct sulfate, and a carbon emission factor which includes soluble organic 
material and other remaining carbon.  Table 4-5 presents a summary of th1~ carbon emission 
factors by vehicle model year and type of technology. 

Table 4-5.  Carbon Emission Factors for Gasoline-Fueled Vehicles (grams/mile) 

Vehicle Type/ 
Model Year 

Light-Duty 

Gasoline Vehicles 

pre-1970 

1970-1974 

1975-1980 

1981  + 

Light-Duty 

Gasoline Trucks I 

pre-1970 

1970-1974 

1975-1986 

1987+ 

Light-Duty 

Gasoline Trucks II 

pre-1979 

1979-1986 

1987 + 

Heavy-Duty 

Gasoline Vehicles 

pre-1987 

1987 + 

Leaded Fuel··  · ... 

···  UQI#~9~9/.C.~~1y~t 
• • •.  · ·.• • ?CiP}~p~q Qi9 f:l}t  .••• 
..  ·/ :•p~~p)·  <•·.·· 

p~1¢~d,~d/.gat~l~·sf ·· 
. .  ·.·.·• . .  ••.<.•P.·'!'.•)•• ... ·.1.·· .•.. •.t .. h.·.•.•.• .. • .. •.~.-.·1·ir· 
····•.·.  E.··.•.·.·.•.q.•••.u.•.•.•.•.i.•.PP.:.•.•.•.•P·•.·.•.•.¢u•.• 

.pM~~4~a/No~S < 
•.'·.·· <  • ~i#alysteql11J>ped .•. ·•· 

.. • .. • .d . •m ·  

.193 

.068 

.030 

.017 

.193 

.068 

.030 

.017 

.370 

.068 

.030 

.370 

.163 

NA 

.0060 

.0060 

.0043 

NA 

.0060 

.0060 

.0043 

NA 

.0060 

.0043 

.054 

.054 

NA 

.0250 

.0250 

.0043 

NA 

.0250 

.0250 

.0043 

NA 

.0250 

.0043 

.054 

.054 

.030 

.030 

.030 

.017 

.030 

.030 

.030 

.017 

.054 

.030 

.017 

.054 

.054 

Total Exhaust Particulate Emission Factors for Diesel-Fueled Vehicles 

The total exhaust particulate emission factors are for diesel-fueled vehicle categories and model 
years (EFDPMmv)·  The emission factors· for heavy-duty vehicles are in units of grams/brake-
horsepower hour (g/BHP-hr), which are converted to grams/mile by PARTS.  The conversion 
factors and emission factors vary by model year.  The emission factors for light-duty diesel 
vehicles are in units of grams/mile.  The total exhaust emission factors given in Table 4-6 are 
based on high-sulfur fuel.  TI:ie sulfur content in diesel fuel was reduced in 1993 by EPA 
regulatory requirements.  Consequently, when a the specified calendar year is  1993 or later, 
PARTS will adjust the exhaust emission factors for lower sulfur fuel.  Particulate emission factors 
for diesel-fueled vehicles are not adjusted for speed. 

Table 4-6.  Exhaust Particulate Emission Factors for Diesel-Fueled Vehicles 

· ..  <: ,,,_,:.:::'  -<:::':'_.•·.· .. ·•• .. •.v.•·.•·.···-~-h. ·_fo_._1_._e_·.··.·•_'.T_•· 
...•.. Y ' .  

·  .. ·.·P'_e_._.f.· .•. M_.·_·_·. o_ 

:_  a_·_ .. _.e_'.'_·.t 

.... '_·•.·.Y.'.•··'.:.e_'_.••.a 

...•. r_.·.·_ .... •.•.o_·.• 

__ .. __ .. u_·.·_.P· 

.. ··. ·_.·_,.,_.·_:· '_:·.·········• 

:_·._-.r_._o 

Light-Duty Diesel Vehicles (grams/mile)-

pre-1981 

1981 

1982-1984 

1985-1986 

1987 

1988-1990 

1991-1993 

1994-1995 

1996 + 

Light-Duty Diesel Trucks (grams/mile) 

pre-1981 

..  1981 

1982-1984 

1985-1986 

1987 

1988-1990 

1991-1993 

1994-1996 

1997 + 

(t"E:'ib~J~£Pirii2m~f ~:Eiriis~fori:f'~@tcif, ·o \i .::.::::, ..•. 

·.· .. ·.·.· .. ·  ....... · .. ·.··.·.·:<·.··<······:-:..·· .. ···:····.· .. ·.· .. ·.·.··.·.· .. · .... ·.· .. ·.·.· .. ·.·.·.· .. · .. ·.·.· ......... 

. 

·.··.·. 

.700 

.259 

.256 

.255 

.134 

.132 

.131 

.128 

.100 

~700 

.309 

.354 

.358 

.334 

.291 

.294 

.130 

.109 

'  • ''  :,. ,,_: 

•': 

•• 

. 

Table 4-6.  Exhaust Particulate Emission Factors for Diesel-Fueled Vehicles 

Class 2B Heavy-Duty Diesel Vehicles (grams/brake 

horsepower-hour) 

pre-1988 

1988-1990 

1991-1993 

1994+ 

Light Heavy-Duty Diesel Vehicles (grams/brake 

horsepower-hour) 

pre-1988 

1988-1990 

1991-1993 

1994+ 

Medium Heavy-Duty Diesel Vehicles (grams/brake 

horsepower-hour) 

pre-1987 

1988-1990 

1991-1993 

1994 + 

Heavy Heavy-Duty Diesel Vehicles (grams/brake 

horsepower-hour) 

pre-1987 

1988-1990 

1991-1993 

1994+ 

Buses (grams/brake horsepower-hour) 

pre-1987 

1988-1990 

1991 

4-74 

.5156 

.5140 

.2873 

.1011 

.5156 

.5140 

.2873 

.1011 

.6946 

.4790 

.2747 

.0948 

.6444 

.4360 

.2709 

.0836 

.6931 

.4790 

.2772 

Table 4-6.  Exhaust Particulate Emission Factors for Diesel-Fueled Vehicles 

EP A/600/R-99/030 

1992 without traps 

1992 with traps 

1993 without traps 

1998 with traps 

1994 + 

.1716 

.0257 

.1457 

.0240 

.0591 

. Soluble Organic Fraction and Remaining Carbon Portion Emission Factors for Diesel-Fueled 
Vehicles 

The PARTS model calculates the Soluble Organic Fraction (SOF) emission factor as a fraction of 
the remaining ~ass, using the following equation: 

SOF m,v,  =  [EF m,v- DSULF m,v  ] * (fractionsoF,J 

The Remaining Carbon Portion (RCP) is defined the remainder (everything else): 

RCP m,v  = Efm,v - DSULF m:v  - SOF m,v 

(4-89) 

(4-

90) 

where: 

m is the model year of a selected vehicle 
v is the class of a selected vehicle 
SOF m vis the Soluble Organic Fraction of the exhaust particulate emission factor 

· 

· (grams/mile) 

· 

· · 

· 

, · 

RCP m,v  is the remaining carbon portion (elemental 'carbon) of the exhaust particulate 

emission factor (grams/mile) 

EF m,v  is EFDPMm,v * CF m,v  , the exhaust particulate emission factor for a vehicle 

(grams/mile) 

EFDPMm v is the exhaust particulate emission factor for a vehicle (grams/Brake 

Horsepower-hour) 

CF m v  is the conversion factor from grams/Brake Horsepower-hour to grams/mile 

(Brake Horsepower-hour/mile) 

fractionsoF.v is the fraction of the non-sulfate portion (ie. the carbon portion) of the 

diesel exhaust particulate emission factor which is organic carbon for a vehicle 
(Brake Horsepower-hours/mile) 

The Soluble Organic Fractions (fraction80F,v)  for different vehicle classes are as follows (U.S. 
EPA, 1990): 

0.18 for LDDVs (Light-Duty Diesel Vehicles) 
0.50 for LDDTls (Class 1 Light-Duty Diesel Trucks) 
0.48 for LDDT2s (Class 2 Light-Duty Diesel Trucks) 
0.51  for LHDDVs (Light Heavy-Duty Diesel Vehicles) and 2BHDDVs (Class 2B Heavy(cid:173)
Duty Diesel Vehicles) 
0.44 for MHDDVs (Medium Heavy-Duty Diesel Vehicles) and BUSES 
0.24 for HHDDVs (Heavy Heavy-Duty Diesel Vehicles) 

Idle Emission Factors for Heavy Diesel-Fueled Vehicles 

Idle emission factor data in grams/hour were collected from manufacturers for heavy-duty diesel 
vehicle classes only.  Consequently, the idle emission factors are not included into the "all 
vehicles'' emission category.  The vehicle class emission factors are calculated by averaging 
together model-year-specific emission data, where the model-year-specific: emission data are 
weighted by the estimated travel fraction of that model year within the vehicle class. 

The idle emission factors are model-year-specific but the model year emission rates do not vary by 
vehicle class.  Consequently, the same model year emission factors are used for all the heavy-duty 
diesel classes, and the differences between idle emission factors between classes reflects only the 
differing travel fractions between model years for a class.  As a result, the emission factors 
reported for the smaller of the heavy-duty vehicle classes, such as 2BHDDV and LHDDV, may 
be over estimated.  The base idle emission factor~ in PARTS for all heavy·'duty diesel vehicles are 
as follows: 

• 
• 
• 
• 

5.370 grams/hour for models prior to  1988 
3.174 grams/hour for model years 1988-1990 
1.860 grams/hour for model years  1991-1993 
1.004 grams/hour for model year 1994 + 

Re-entrained Dust from Unpaved Roads 

Re-entrained road dust emission factors for PM10  (particulate matter less than 10 micrometers in 
size)  in PARTS are estimated using an equation (4-91) based on ambient measurements.  Because 
these measurements include particulate matter from brake wear, tailpipe exhaust, tire wear, and 
ambient background particulate matter concentrations, these factors must be subtracted from the 
unpaved road particulate emission factors before the latter factors can be applied.  It is necessary 
to obtain a data base of unpaved and paved road types by county for regional modeling purposes. 
The MEPPS contains procedures and estimates for 1995 developed for EPA  by calculating re(cid:173)
entrained emissions by the month at the state road-type level for the average vehicle fleet, and 
then allocated to the county road type by population for unpaved roads and by total VMT for 
paved roads.  These procedures should be consulted for more details, such as the means of 
estimating unpaved and paved VMT data.  The equation used to calculate PM10  from unpaved 
roads is as follows: 

UNPVD = PSDUNP * 5.9 * (UNSILT/12.0) * (SPD/30.0) * (WEIGHT/3.0)0

(VWHEEL/4.0)0

·

5 * (365-IPDAYS)/365 * 453.592 

7 * 

·

(4-91) 

where: 

UNPVD is the fleet average unpaved road dust emission factor (grams/mile) 
PSDUNP is the fraction of particles less than or equal to the particle size cutoff (the 

cutoff is 10 micrometers in MEPPS) 

UNSILT is the percent silt content of the surface material (user input) 
SPD is the average vehicle speed in miles/hour (user input) 
WEIGHT is the fleet average vehicle weight (user input in pounds) 
VWHEEL is the fleet average number of wheels (user input, default is 4) 
IPDAYS is the average number of precipitation days per year with greater than 0.01 

inches of rain (user input - MEPPS contains climatological default data for 
1995) 

453.592 is the number of grams in a pound 

Emission factors for brake and tire wear (in addition to exhaust emissions) must be calculated so 
that they may be subtracted from the unpaved road emission factors.  The brake wear emission 
:factor is assumed to be the same for all vehicle classes.  It is set equal to: 

BRAKE= 0.0128 * PSBRK 

(4-92) 

where: 

PSBRK is the fraction of particles less than or equal to the particle size cutoff.  The 
emission factor 0.0128 grams/mile is taken from U.S. EPA, 1985c. 

The tire wear emission factor is calculated using the following equation: 

EFTIREV = 0.002 * PSTIRE * IVEHWLV 

(4-93) 

where: 

v is a selected vehicle class 
EFTIREv is the tire wear emission factor 
0.002 is the emission rate of airborne particulate matter from tire wear for light-duty 

vehicles (U.S. EPA, 1985) 

PS TIRE is the fraction of particles less than or equal to the particle size cutoff 
IVEHWLv is the average number of wheels on a vehicle of a given class, where 

LDGV=4, LDGTl,2=4, HDGV=6, MC=2, LDDV=4, LDDT=4, 2BHDDV=4, 
LHDDV=6, MHDDV=6,HHDDV=l8, BUSES=4 

Re-entrained Dust from Paved Roads 

The PM10  emission factor for paved roads is estimated similarly to that for unpaved roads.  The 
VMT data for paved roads may be developed following the procedures described in U.S. EPA, 
1998.  The equation used to estimated paved road re-entrained dust emission factors again is 
based on ambient measurements, and tailpipe, brake wear, and tire wear emission factors must be 
subtracted prior to use of the paved road dust emission.factor.  The paved road emission factors 
are calculated by: 

PAVED= PSDPVD * (PVSILT/2.0)0

65  * (WEIGHT/3.0)u 

·

(4-94) 

where: 

PAVED is the fleet average paved road dust emission factor (grams/mile) 
PSDPVD is the base emission factor for the particle size cutoff (10 micrometers in 

MEPPS) 

PVSILT is the road surface silt loading (grams/square meter) (user input) 
WEIGHT  is the fleet average vehicle weight (input by the user in pounds) 

Application in MEPPS 

The PARTS model is a companion ofMobileSa in MEPPS.  When running MEPPS interactively 
through the Tools Manager, the user is prompted to specify input data and options under 
EMPRO, Mobile Source Model.  Standard lookup data files can be found in the Models-3 
directory structure at /home/models3/datasets/nostudies/partS/.  Default/example input data sets 
are included for the 199S calendar year.  When a study is created, the study input data are located 
at SEMS_HOME/project/$EMS_PROJECT/raw_data/$EMS_DOMAIN/commonl. 
If the user is processing mobile emission data using the Study Planner, it is necessary to ensure 
that the appropriate PARTS input data sets are in place first.  This may be accomplished when 
using MEPPS interactively to establish MEPPS directory structure prior to  running through Study 
Planner.  If the data are in place, Study Planner will automatically process them. 

When processing mobile particulate emission data, the input and output data of PARTS are 
assigned to and summed within each grid cell in the same fashion given in detail under the 
Gridding VMT discussion of the implementation of the Mobile Sa model (above). 

### 4.2.5  Chemical Speciation of Emission Data 

Chemical transport models, such as CMAQ, require that emission data be provided for either 
individual species or specific species groups or "lumped" species.  This is necessary so that the 
atmospheric chemistry of pollutants may be more accurately modeled.  However, an initial 
processing step is required because emission data are often reported for pollutants that are 
aggregates of many species, such as VOC.  These aggregate pollutants must be split into their 
component species, or "speciated". 

The speciation takes two forms, discrete and lumped-model. 

In discrete speciation, a pollutant is split into the individual components which comprise the 
pollutant.  For the organic pollutant TOG (total organic gas), the individual components which 
comprise the pollutant are dependent on a variety of factors including the process, fuel type, and 
device from which the emissions occurred.  For example, TOG from the exhaust of automobile 
may contain approximately fifty discrete organic compounds (benzene, methane, toluene, hexane, 
etc.), while TOG from degreasers may contain approximately seventy discrete organic 
compounds.  NOx (nitrogen oxides), which is related to combustion processes, is speciated 
(discrete) into NO and N02  (and sometimes HONO). 

The discrete components in an emission stream are determined by a number of methods including 
source testing, surrogate application, and engineering kno~ledge of the process.  Many sources 
have been inventoried and a compendium of species profiles has been assembled by the US EPA 
in the Air Emissions Species Manual (US EPA, 1988).  The Air Emissions Species Manual 
contains a list of TOG and particulate matter (PM) species profiles to which a substantial number 
of emission sources have been assigned.  A compendium of species profiles currently available is 
compiled in the US EPA Speciate database.  The database is available on the Air Chief CD ROM, 
which is updated annually (US EPA, 1997).  Each of the species profiles identifies the mass 
percent of the discrete compounds that comprise TOG and PM.  Note that NOx, SOx(sulfur 
oxides), and CO (carbon monoxide) do not have source-specific speciation profiles.  For all 
sources, NOx  is discretely speciated into NO and N02 (and sometimes HONO), and CO is 
treated explicitly.  The speciation processor currently assumes that NOx is speciated to 95 percent 
NO and 5 percent N02,  based on average observed values. 

The MEPPS EMPRO speciation processor  provides chemical speciation of hydrocarbons,  oxides 
of nitrogen, and sulfur oxides.  For example, aggregations of hydrocarbon species such as TOG 
are disaggregated to their component individual chemical species.  The processor speciates the 
spatially and temporally allocated emission estimates that are prepared by the emission estimation 
processors and models (e.g., point source processor, mobile emission model).  The speciation is 
accomplished using look-up tables of profiles containing source-category specific and specie(cid:173)
specific chemical split factors (percent of mass of source emissions attributable to each specie for 
a given source category).  The general components of emission speciation processing are 
illustrated in Figure 4-6. 
It is computationally prohibitive to model the chemistry of all discrete voe compounds in the 
emissions stream in photochemical grid models.  Therefore, individual organic species comprising 
Total Organic Gas are assigned to one or more model species (groups of species) according to the 
chemical mechanism that is being used.  Thus, instead of modeling with a larger number of 
discrete compounds, the discrete compounds are lumped into a much smaller number of 
mechanism species.  The rules for assigning the discrete compounds to the mechanism species are 
mechanism dependent and typically involve lumping in one of two ways:  1) lumping compounds 
with similar reactivity characteristics into a single mechanism species (the lumped molecule 
approach) or 2) assigning molecular fragments of an individual compound to one or more 
mechanism species on the basis of molecular structure (lumped structure approach).  In Models-3, 
the lumping of discrete compounds to form mechanism species is carried out using tables of split 
factors that assign the discrete compounds to mechanism species.  Currently, two mechanisms, the 
Regional Acid Deposition Model 2.0 (RADM2) and Carbon Bond 4 (CB-4) are available in 
Models-3.  The user may also define  lumping procedures for an alternative mechanism ifthe 
assignment methods for that mechanism are known, or the user may create a modified version of 
the lumping procedure for one of the two mechanisms in Models-3. 

In the eB-4 mechanism, discrete compounds are assigned to mechanism species on the basis of 
the compound's carbon bond structure (Gery et al.,1989).  For instance, single carbon-carbon 
hydrocarbon bonds are assigned to a paraffin group (PAR), and carbon-carbon double bonds are 
assigned to an olefinic group (OLE).  Thus, an individual discrete voe could be dis-aggregated 
into more than one mechanism species depending on its structure.  Descriptions of the eB-4 
mechanism species are given in Table 4-8. 

The RADM2 mechanism lumps discrete compounds on the basis of their prevalence in the 
atmosphere, common reactivity, and/or molecular weight (Stockwell et al., 1990).  For RADM2, 
individual voe compounds are first assigned to one of 32 lumped groups.  The 32 lumped 
groups are then further reduced to  15 groups for increased computational efficiency.  The 
relationship between the 32 and 15 lumped groups is shown in Table 4-9.  The EMPRO does not 
include chemical or size fractionation of particulate matter, although size fractionation of 
particulate matter is planned in the future. 

Point 
Source 
Data 

Area 
Source 
Data 

Mobile 
Source 
Data 

Biogenic 
Source 
:Data 

Application of Speciation Split Factors 
o Chemical Mechanism Dependent 

(e .. g~ NOx~ SOx) 

(e.g.~ RADM2~ CB4) 

o ROG- to TOG- Adjustment 
o Non-VOC Split Factors 

, 

Output Processor 

Figure 4-6  General Components of Emission Speciation Processing 

Table 4-8.  Carbon Bond-4 Lumped Species 

CB4 Lumped Species 

Number 

CB4 Lu~d~sp.7:i; ;:.  ... 

1.; 
I·•·. 
·.·· .. 

.·  ·•· .·.. 
. . 
.·  .. :  .... ·.· / 

" /  •· 

Lum.,., 

. ... 

·.·.·····  ·~c .. 
'"  . . • Df f }'.'.~!tJ~s:i!;:)·. · 

.r 

.... 

•·•···  \  ... .<.: ........... ·. 

.:·:::. 

.:.;:\i;:;;t.i 
· 
. ···•· 
)'/.::;;;: 

1 

2 

3 

4 

5 

6 

7 

8 

9 

10 

11 

12 

13 

14 

15 

16 

OLE 

PAR 

TOL 

XYL 

FORM 

ALD2 

ETH 

MEOH 

ETOH 

ISOP 

NR 

NO 

N02 
co 

S02 

AERO 

Olefinic carbon bond (C=C) 

Paraffin carbon bond (C-C) 

. 

Toluene (C6H5-CH3) 

Xylene (C6H4-(CH3)2) 

Formaldehyde (CH2=0) 

High MW aldehydes (RCHO, R>H) 

Ethene (CH2=CH2) 

Methanol (CH30H) 

Ethanol (C2H50H) 

Isoprene 

Non Reactives as methane 

Nitric Oxide 

Nitrogen Dioxide 

Carbon Monoxide 

Sulfur Dioxide 

Aerosols (PM) 

4-82 

Table 4-9.  RADM 2.0 Lumped Species Descriptions 

EPJ\/600/R-991030 

.. ·•.•.·,·.•.z,'_.,· ..• ,o, .3.'2i ·•·  R.Ab-Miid .16. 

•. RA: 
.• : ... ·.·,•.·,b_.··••.M 
. ::A1f8¢iiiQiiC .,. 
•••.-.:?wni>¢.ct: ··>  ,~ti1ni>e4§p~¢1~s.Y  }  F.abt0f'·•:. 
·  · 
..•..• ,, •..•.. '.•,.: •. _.·,.· .. 00_•·,·_,.'~,-·.•.~.e.o_ 
> : §p:cws .·:·:.: ..•. ··•••  .••. > :,  : < •.. /_ ...••. _,_/.,' ... •·.•.•.••.•.•• ... •.-,•.•·, ... ·.,•, .• -,., .• 
·_:·,•·.•·., .• ·.•·.•.••.·.••·,r_fhi 

· · .... 

•.•.. i,'.,•·.9.•.•••',.r .• _; 

•..• ,_ .• )•·.n, 

1 

'·.•··. 
·::.  !.: 

""p·'":···.L·ec·.· 

.;:.;Ei;u; ··.  ,;;;:;~L::  , •. ,:· 11eseri~ti~··,···ofthe s2·1:i ·~oil,,i·•::·:.·., 
· ..•. '.',,·.:.,'.·C.2/\:zY'· .. q.~~:~J.  t 
<  ......  ,, 
.• '.':','g'.'.·.·,·.· •. ·.·.,•.',.t··,.·.•.,,'o·_·_·,· 
.. ·.,, ... ·_· •. ·.•.· .. ,·'_· 
,  .:  ':I; 

.. _'.·.~.-.. ~.··_·"'P 
.. ·.·.·.u·.·1~.bl'.e'·'.·.·s·.·.·.P 
"',,  _ 
.. ,· .. ·.'a.·.e·····de· 
, >  •.:.:::..-:·::::.:.-. 

:~~]tJoi_-'i,  ·,_.,, .. 
: 
(_.·  i' •?• , ·: 

/·•:·:::},_:.,:\  ................. : .. ::: .. :·:-.  :·.= .. ::: .. :.::::::  ................... ,.,., .. ,, .... ,, ....  ::;::::  :::::::{:::}:;::':::: .. . 

... ·.·.p'.·,·,··1._n 

.. 1·.~_.e'r''.·~·.'.·._u 
,,,,,,,  .... ,,~ ...  ,,,,,, < 

.... · ..• •_·p•· .. ·,.·e.'·.· .. d·: ... '··l'•s·.
6

, . , .· .... ·.· 

•,·L'' 
.. '.,.'•u··.·.·_.'.m' 

... ·e·.ec'·.'.•.'.·G·.· 

2 

3 

4 

5 

6 

7 

8 

8 

9 

10 

II 

12 

13 

13 

14 

15 

16 

17 

18 

18 

19 

20 

21 

22 

CH4 

ETH 

HC3 

HC3 

HC5 

HC8 

HC8 

HC8 

XYL 

OL2 

OLT 

OLT 

OLI 

OLT 

OLI 

TOL 

TOL 

XYL 

CSL 

OLT 

TOL 

HCHO 

ALD 

KET 

KET 

I 

0.91 

0.09 

0.5 

0.5 

0.5 

0.5 

0.519 

0.964 

0.956 

0.945 

1.141 

I.IOI 

Methane 

Ethane 

Propane 

Alkanes (0.25-0.50) 

Alkanes (0.50-1.00) 

Alkanes (1.00-2.00) 

Alkanes (>2.00) 

Alkane/ Aromatic Mix 

Alkane/ Aromatic Mix 

Ethene 

Propene 

Alkenes (Primary) 

Alkenes (Internal) 

Alkenes (Prim/Int Mix) 

Alkenes (Prim/Int Mix) 

0.293 

Benzene/Halobenzenes 

Aromatics ( <2 react) 

Aromatics (>2 react) 

Phenols and Cresols 

Styrenes 

Styrenes 

Formaldehyde 

Higher Aldehydes 

Acetone 

Ketones 

0.253 

Table 4-9.  RADM 2.0 Lumped Species Descriptions 

RADM2.032 

Lumped 
Species 

23 

24 

25 

26 

27 

28 

29 

30 

31 

32 

LRAumDpeMd 82p.Oe .. c1
.·e·6
1

0

5< · ········~··· lFloa· ccat····
oner. 

... ·.ti~.· 0 
•..•• ).~.a·c··· t •  o. r · '  .·.  b~scriptiorr of the· 32 RAbM  · ·.····•· 
t.ir·.o. n .. · •. · .. <  · .. ··.·.··.·.R .. ~a~ti.•(···rvi.•·.a· 
Lumped Specie$ 13etC>re Groupillgto 
·. ···.•<••··i,L,··  ····•··•••··••16l:~umpedSp~C.i~~  • . )}  ..•... 
. ...... ·:: ...  :,::··  ········ 

(fractionof 

. 

. .· 

.• 

·. 

ORA2 

HC3 

HC3 

HC3 

HC3 

HC5 

HC8 

1 

1 

1 

0.343 

0.078 

0.404 

1.215 

1.075 

1.011 

Organic Acids 

Acetylene 

Haloalkenes 

Unreactive 

Others (<0.25 react) 

Others (0.25-0.5 react) 

Others (0.5-1.0 react) 

Others (> 1.00 react) 

Unidentified 

Unassigned 

In Models-3, the user initiates speciation by selecting the chemical mechanism to be used  for a 
study  in the Study Manager under the Models-3 framework, and within the MEPPS main 
window under Tools Manager.  The procedure is explained in detail in Chapter 6 of Models-3 
Volume 9B:  User Man~al. The speCiation processor is written primarily in SAS®; however, it 
includes some FORTRAN programs.  ARC/INFO®  is not used in the speciation processor. 

Speciation Processing in MEPPS 

In MEPPS, speciated gaseous emissions can be calculated in either of two units - moles/hour or 
Kg/hour. (Particle emissions are always calculated as Kg/hr.)  Emissions in moles are required by 
the Models-3 CMAQ, but some analyses require that emissions be expressed in mass per unit 
time.  Emissions in mole and mass units are calculated using the following two generic equations: 

chemestij  = 'EP  'Em  hrimp  *  1000  *  rogtotogmp  *  factorjmp  I  divisorjmp 

(4-95) 

where: 

chemestij is the gridded emissions estimate for hour i and mechanism species j 

(moles/hour) 

hrimp is the gridded hourly emission estimate of pollutant p (NOx, SOx, CO, PM, 

NH3, or ROG) for houri and source category m (Kg /hr).  The factor of 1000 
is used to convert kilograms to grams. 

rogtotogmp  is· a ROG to TOG conversion factor (discussed below) 
factorjmp  is  a mole-based split factor to allocate total emissions of pollutant p from 

source category m to mechanism species j 

divisorjmp is a second conversion factor to allocate total emissions of pollutant p from 

source category m to mechanism species j 

and 

(4-96) 

where: 

estKgij is the gridded emissions estimate for houri and mechanism species j  (Kg/hour) 
hrimp is  the gridded hourly emission estimate of pollutant p (NOx, Sox, CO, PM, 

NH3, or ROG) for houri and source category m (Kg /hr) 

rogtotogmp is a ROG to TOG conversion factor (discussed below) 
xmassjmp is a mass-based split factor to allocate total emissions of pollutant p from 

source category m to mechanism species j 

These equations show that emissions for mechanism species are computed by summing the 
contributions of pollutants from the different source categories that emit that pollutant. As 
indicated previously, the assignment of a pollutant to a specific mechanism species usually 
mechanism dependent, although some assignments may be handled  the same way in different 
mechanisms.  In Equations 4-95 and 4-96, the terms factorjmp•  divisorjmp• and xmassjmp are 
mechanism specific apportioning factors that will be defined further below.  Note that, in some 
cases a pollutant will contribute to only one mechanism species, but in other cases a pollutant will 
contribute to more than on species.  For example, emissiollls of ammonia are always assigned to a 
model species named NH3, but NOx is allocated between 1wo mechanism species NO and N02. 
The details of the allocation procedure for both the RADM2 and CB4 mechanisms will be 
described further below.  First, however, the derivation and use of the rogtotogmp  conversion  . 
factor is described. 

ROG-to-TOG Adjustment 

The ROG (Reactive organic gas) to TOG conversion portion of the speciation process is 
selected when the emission inventory is loaded by INPRO (see section 4.2.2).  VOC substances 
deemed non-reactive (e.g., methane and ethane) are often excluded from inventories, and some 
emission measurement techniques do not capture all discre1te compounds in an emission stream 
( e.g, formaldehyde).  The ROG-to-TOG adjustment factor is used to account for those missing 
components.  It is calculated using Equations 4-97 and 4-98: 

(4-97) 

rogtotogm  = 1.0  I  rnistogm 

(4-98) 

where: 

mistogi is the sum of mass fractions of the discrete voe compounds deemed non(cid:173)

reactive or not included in an emission inventory for source category m (grams 
of missing compound per gram of TOG) 

ntsrdk,m is the mass fraction of the missing or non-reactive compound k in the 

emission stream for source category m 

rogtotogm is the ROG to TOG conversion factor 

Note that the rogtotogm,p adjustment factor applies only to anthropogenic emissions ofVOC, and 
thus the p subscript used in Equation 4-95 and 4-96 has been dropped here.  In equations 4-95 
and 4-96,  rogtotogm,p is set to  1.0 for all pollutants other than ROG and also set to  1.0 when 
voe emissions ofbiogenic origin are being computed. 

Carbon Bond 4 (CB4) Speciation Factors 

The CB-4 chemical mechanism is widely used in air quality model simulations of ozone 
concentrations at urban and regional spatial scales.  As described in the introductory part of this 
section, the basis of the CB-4 mechanism is that reactivity of organic compounds in the 
atmosphere can reasonably be simulated by mechanism species that represent different carbon 
bond types. A detailed description of the CB-4 mechanism is contained in Section 8.2.1.  The 
focus of the discussion here is on how the CB-4 apportioning factors used fin  Equations 4-95 and 
4-96 are calculated.  Since speciation of anthropogenic VOC emissions is done differently than for 
other pollutants, the discussion is divided into two subsections. 

Anthropogenic VOC speciation.  For the CB4 mechanism, the apportioning factor  divisorjmp in 
equation 4-95 is set to 1.0 for all anthropogenic VOC emission calculations(i.e., it is essentially 
not used).  The  factorjmp term for anthropogenic voe is set equal to a split factor term sjl  that is 
computed for a specified VOC emission profile l, which in turn is assigned to a source category m 
(i.e., an SCC or ASC). The split factor for each profile and mechanism species is computed as 
follows: 

sfJ. 1  =  Ek  xmfk 1 I  mwk  * xnum. k 

J, 

, 

, 

(4-99) 

where: 

s~.1  is a molar split factor for CB4 species j  and  VOC profile 1 (moles of CB4 

species/gram of TOG) 

xmfk,I  is a mass fraction Of discrete VOC compound kin VOC profile 1 (grams Of 

mwk 

discrete VOC/grams of TOG) 
is the molecular weight of discrete voe compound k (grams of discrete 
VOC/mole of discrete VOC) 

xnumj,k is the moles of discrete VOC compound k assigned to CB4 species j  (moles of 

CB4 species/mole of discrete VOC) 

In Equation 4-96, the  xmassjmp term apportions mass emissions to CB4 species for a given 
pollutant and  source category.  Analogous to the CB4 molar split factor defined above, the 
xmassjmp term is set equal to a mass split factor that appmtions the mass of a discrete VOC 
compound according to the fraction of carbon atoms that is assigned to a mechanism species: 

where: 

xmassj,l is the  mass_ split factor for CB4 species j  and VOC profile 1 (moles of CB4 

(4-100) 

species/gram of TOG) 

xmfk,I  is the mass fraction of discrete voe compound kin voe profile 1 (grams of 

discrete VOC/grams of TOG) 

cnj is the  number of carbon atoms in CB4 species j 
cnk is the number of carbon atoms in discrete voe compound k 

Again, one VOC profile is assigned to each source category, and thus split factors are generated 
for each source category/mechanism species combination. 

Other speciation.  For pollutants and source categories other than anthropogenic VOC, the terms 
factorjmp•  divisorjmp• and xmassjmp are simply assigned numeric values that determine the allocation 
of pollutant emissions.  These are summarized in Table 4-10. 

For all pollutants (except particles) in Table 4-10, the divisor  corresponds to the molecular 
weight of the pollutant and is used in equation 4-76 to convert mass emissions of the pollutant to 
moles of pollutant.  The two split factors factorjmp•  and xmassjmp are used to apportion the 
pollutant emissions to the mechanism species.  A factor of 1.0 indicates a  1: 1 correspondence. 
Factors different from one indicate a disaggregation or lumping of pollutant emissions into 
individual species. 

CO and NH3 are treated explicitly in many air quality models, and therefore emissions of these 
compounds are not split into other components.  Similarly, particle emissions are treated explicitly 
so no lumping or dis-aggregation is necessary for them either. 

When SOx emissions are speciated, 97% of the SOx mass is treated as S02.  The remaining 3% 
of the SOx mass corresponds to 804 (or SULF), but is dropped from further consideration.  S02 
and S04 emissions are treated as explicit species. 

Table 4-10.  CB4 Split Factors for Pollutants Other Than Anthropogenic VOC 

.... '" • .:-.,.~·'·'.·:;;::.:;~:~~:.;;::i~~;:(~:~:=::.::::;;~: 

'"·~~~~·: .-~"~:.·;~;:_::?~:~~~~~~~~rf:r~~~~~: ii[l~ll;~tll~~l;~; :;;;;;:;;;;:;;,£1il:'::::t 
::!:§Ufc~ G!¥.8#bi:07 

All 

All 

All 

All 

All 

All except BIO 

All except BIO 

All 

All 

All 

BIO 

BIO 

BIO 

BIO 

BIO 

BIO 

BIO 

BIO 

BIO 

co 

NH3 

AERO 

PMlO 

PM2  5 

NOX 

NOX 

802 

SOX 

804 

NO 

lsoprene 
ovoc 
ovoc 
ovoc 
TERP 

TERP 

TERP 

TERP 

co 

NH3 

AERO 

PMlO 

PM2  5 

-

NO 

N02 

802 

802 

SULF 

NO 

ISOP 

NR 

OLE 

PAR 

ALD2 

OLE 

PAR 

TERPB 

1.0 

1.0 

1.0 

1.0 

1.0 

0.62 

0.05 

1.0 

0.97 

1.0 

1.0 

1.0 

0.5 

0.5 

8.0 

1.5 

0.5 

6.0 

1.0 

28.0 

17.0 

1.0 

1.0 

1.0 

30.0 

46.0 

64.0 

64.0 

96.0 

30.0 

68.12 

148.0 

148.0 

148.0 

136.23 

136.23 

136.23 

136.23 

1.0 

1.0 

1.0 

1.0 

1.0 

0.62 

0.05 

1.0 

0.97 

1.0 

1.0 

1.0 

0.05 

0.10 

0.85 

0.3 

0.1 

0.6 

1.0 

In MEPPS, it is assumed that NOx is composed of95% NO and 5% N02 (by mass).  However, 
actual NOX composition can vary from 89%/11 % to 95%/5%.  In a few cases, a small percentage 
(<2%) ofNOX emissions is assumed to be nitrous acid (HONO).  Since NOx emissions are 
typically reported as N02 mass, it is necessary to normalize the NOx emissions by the molecular 
weight of N02.  Hence, the molar split factors (factorjmp) for NO and N02 are calculated as 
follows: 

(4-101) 

(4-102) 

With a NOx composition of 95% NO (as N02) and 5% N02 by mass, the split factors for NO 
and N02 become 0.62 and 0.05, respectively.  The mass split factors are calculated in the same 
manner, and thus have the same values as for the molar split factors. 

The speciation of biogenic emissions in CB4 is the same as used in the Regional Oxidant Model 
(EPA,  1989).  Isoprene is treated as an explicit species and terpene emissions are speciated into 
1.0 mole of OLE, 6.0 moles of PAR, and 3.0 moles of ALD2.  Terpenes are also assigned to the 
model species TERPB for special processing in the aerosol module of the CMAQ.  The apparent 
double counting ofterpene emissions is accounted for in the CMAQ processing however.  Finally, 
the category OVOC (other VOCs from biogenic sources) is apportioned to  1.0 mole of OLE, 8.5 
moles of PAR, and 0.5 mole ofNR.  Here it assumed that number of carbon atoms in a OVOC 
molecule is  10. 

RADM2 Speciation Factors 

The RADM2 chemical mechanism was developed for and has been used principally in regional air 
quality simulations of sulfur dioxide (S02) and oxides of nitrogen for ~cid rain assessment 
(Walters and Saegar, 1990).  A detailed description of the RADM2 mechanism is given in section 
8.2.2.  The speciation procedure used for RADM2 is fully described in Walters and Saeger 
(1990), and will only be summarized here.  This section focuses on how the RADM2 apportioning 
factors used in Equations 4-95 and 4-96 are calculated. Again, the discussion is divided into two 
subsections, one dealing with the speciation of anthropogenic voe emissions and one with all 
other speciation. 

Anthropogenic VOC speciation. The RADM2 mechanism requires that discrete organic VOC 
compounds be lumped into  15 mechanism species based on common reactivity and reaction 
products.  As with the CB4 mechanism, the apportioning factor  divisorjmp in Equation 4-95 is set 
to 1.0 for all anthropogenic VOC emission calculations, and thus is not used.  Also as with the 
CB4 mechanism, The factorjmp term for anthropogenic VOC is set equal to a split factor term sj1 
that is computed for a specified VOC emission profile 1,  which in turn is assigned to a source 
category m (i.e., an SCC or ASC).  However, the RADM2 split factors are calculated using the 
following equation: 

sf. 1  = :Ek  xmfk 1  * afac. k  * rfacJ. k  I  mw k 

J, 

, 

J, 

, 

(4-103) 

where: 

s~,I is the molar split factor for RADM2 species j  and  voe profile 1 (moles of 

RADM2 species/gram of TOG) 

4-89 

xmfk,l is the  mass fraction of discrete voe compound kin voe profile 1 (grams of 

discrete voe/grams of TOG) 

afacj,k is the allocation factor listed iri' Table 4-6 for RADM2 species j  and discrete 

voe compound k 

rfacj,k is the  reactivity factor listed in Table 4-6 for RADM2 species j  and discrete 

voe compound k 

. 

mwk is the molecular weight of discrete voe compound k (grams of discrete 

voe/mole of discrete voe) 

In the RADM2 speciation process, each discrete Voe compound is assigned to one of the 32 
lumped species categories, and that assignment is used to further lumped to one of the 15  species 
in the condensed group.  See Middleton et al. (1990) for information on how the allocation and 
reactivity factors are derived. 

In Equation 4-96, the term  xmassjmp apportions mass emissions to RADM2 species for a given 
pollutant and  source category.  Analogous to the RADM2 molar split factor defined above, the 
term xmassjmp is set equal to a mass split factor that apportions the mass of a discrete voe 
compound according to the mass fraction of the discrete voe compoiind in the emission stream 
and the mechanism specific allocation factor:  · 

(4-104) 

where: 

xmassj,l is the mass split factor for RADM2 species j  and and VOe profile 1 (moles of 

eB4 species/gram of TOG) 

xnifk,l is the mass fraction of discre~e voe compound kin voe profile l (grams of 

discrete voe/grams of TOG) 

afacj,k is the allocation factor listed in Table 4-6 for RADM2 species j  and discrete 

voe compound k 

Other speciation.  For pollutants and source categories other than anthropogenic voe, the terms 
factorjmp•  divisorjmp•  and xmassjmp are simply assigned numeric variables that control the allocation 
of emissions, similar to what is done for the eB4 mechanism.  The factors for RADM2  are 
summarized in Table 4-11. 

Except for the biogenic categories, the contents of Table 4-11  are identical to those in Table 4-10 
for the eB4 mechanism.  Thus, the reader is referred to the corresponding CB4 section for 
information on those source category/pollutant combinations.  The biogenic portions of the two 
tables are similar in that isoprene is treated as an explicit species in RADM2, and terpenes and 
other voes (OVOe) of biogenic origin are apportioned to mechanism species.  In RADM2, 
terpenes are apportioned entirely to OLI (Middleton et al.,  1990).  As with the eB4 mechanism 
,terpenes are also assigned to the model species TERPB for special processing in the aerosol 
module of the eMAQ, but the apparent double counting ofterpene ~missions is accounted for in 
the eMAQ processing however.  Finally, the assignment of other Vbe species to RADM2 
species is carried out in a manner analogous to that for CB4:  85% is ~ssumed to correspond to 
slow reacting alkanes, 15% to fast reacting olefins, and 5% is assumed to be non-reactive.  Thus, 
in RADM2 85% of the OVOe mass is assigned to the category containing the slowest reacting 
alkanes (category 27 in Table 4-6).  The split factor is obtained :by multiplying the mass fraction 
(.85) by the reactivity factor (0.404) to give a molar split factor of 0.343.  The 10% of the mass of 
OVOe assumed to be reactive olefins is assigned to the OLI category.  Both the mass and molar 
split factors for this compound are simply 0.1. 

lit Factors for Pollutants Other'fhan Anthro  ogenic VOe 

All 

All 

All 

All 

All 

All except BIO 

All except BIO 

All 

All 

All 

BIO 

BIO 

BIO 

BIO 

BIO 

BIO 

co 
NH3 

AERO 

PMIO 

co 
NH3 

AERO 

PMIO 

PM2  5 

PM2  5 

NOX 

NOX 

S02 

SOX 

S04 

NO 

lsoprene 
ovoc 
ovoc 
TERP 

NO 

N02 

S02 

S02 

SULF 

NO 

ISO 

HC3 

OLI 

OLI 

TERP 

TERPB 

l.O 

1.0 

1.0 

1.0 

1.0 

0.62 

0.05 

1.0 

0.97 

1.0 

1.0 

1.0 

0.343 

0.1 

1.0 

1.0 

28.0 

17.0 

1.0 

1.0 

1.0 

30.0 

46.0 

64.0 

64.0 

96.0 

30.0 

68.12 

148.0 

148.0 

136.23 

136.23 

1.0 

1.0 

1.0 

1.0 

1.0 

0.62 

0.05 

1.0 

0.97 

1.0 

1.0 

1.0 

0.85 

0.1 

1.0 

1.0 

### 4.2.6  Output Processor (OUTPRO) 

The results ofMEPPS processing must be in a form that is useful to the Models-3 framework and 
readily evaluated for the substantive content and quality of the data.  The OUTPRO proce~ses 
spatially and temporally allocated, speciated emission data files to prepare them for use by the 

Models-3 framework and its components, including CMAQ.  In addition, OUTPRO prepares 
many standard and user-defined emission summary reports.  The processing includes the following 
items: 

• 

• 

• 

• 

• 

• 

Merging of temporally allocated point, area, biogenic, and mobile source emission data 
files. 

Merging of speciated point, area, biogenic, and mobile source emission data files . 

Merging of point, area, biogenic, and mobile source files into a consolidated two(cid:173)
dimensional emission file.  Merging is optional; and data type files may be maintained and 
output separately. 

Preparation of three-dimensional point source emission files for use by the plume rise and 
Plume-in-Grid models available in CMAQ.  The user may define groups of similar stacks 
by specifying percentage tolerance differences in different physical stack properties.  In 
addition, the user may categorize minor, major, and major elevated point source emission 
(MEPSE) stacks by their emission rates and/or stack properties.  The classification 
criteria are not pre-specified, but are set by the user.  Specifically, any combination of 
emissions of specified pollutants in tons/day and/or physical stack parameters including 
height, diameter, flow rate, or exhaust velocity may be used to define a major or MEPSE 
point source.  Generally, MEPSE stacks are the largest of point sources, such as electric 
utility stacks.  Major point sources are usually somewhat smaller, but significant point 
sources.  Source-specific information is necessary to classify MEPSE or major point 
sources.  Smaller sources not classified as MEPSE or major point sources or without 
source-specific information are referred to as minor point sources, and are usually 
included with the surface area sources by OUTPRO, unless specifa:d otherwise by the 
user. 

Conversion of output file format to  NetCDF I/O API format.  This allows other 
components of the Models-3 framework,  including the CMAQ and. visualization tools, to 
use the emission files.  The CMAQ accepts the emission files and processes them for input 
to CMAQ using the Emission Chemical Input Processor (ECIP).  The ECIP is described in 
Section 7. 

Preparation of summary reports of the processed emission files, including summaries by 
primary emission type (point, area, biogenic, mobile), by geographic area (grid area, grid 
cell, state, county, etc.), by source category code and groups of codes (tiers, or 
combinations of these items. In addition, the reports rank emission values by amount, type, 
geographic area, etc. 

4.3  Models-3 Emission Projection Processor (MEPRO) 

It is often necessary to iteratively project emissions and emission control combinations to future 
years in order to model future year ambient pollutant concentrations relative to regulatory 
standards.  Consequently the Models-3 system includes a projection and control system, the 
Models-3 Emission Projection (MEPRO) processor to perform the necessary work.  The MEPRO 
was developed from the Multiple Projection System (MPS).  The MPS was originally designed as 
a stand-alone software tool to assist the EPA in projecting and tracking the "reasonable further 
progress" of regulatory emission reduction programs for criteria pollutants (Monroe et al.,  1994). 
It may be invoked from Strategy Manager from the Moclels-3  framework or run separately. 
Because MPS was designed as a PC application using the Superbase® programming language, 
MEPRO must be run under an emulator (SoftWindows®) ifit used in an UNIX operating system. 
Currently, MEPRO is more efficiently run on a PC workstation with a Windows NT® operating 
system.  Figure 4-7 illustrates the relationship ofMEPRO with the rest ofMEPPS. 

Using MEPRO, the user may edit regulatory control factors, control efficiency, rule penetration, 
rule effectiveness,  as well as "across-the-board" emission adjustments and source 9ategory code(cid:173)
specific emission projections .  Projected emission inventory data, and/or inventory data with 
revised emission controls and efficiencies are passed to EMPRO (in the 
!raw_ data/$EMS _DOMAIN!common/ directory) for processing to the NetCDF I/O API format 
required for air quality modeling by CMAQ.  Mobile source emissions are not projected in 
MEPRO. Instead, VMT is projected, and the projected mobile sources emissions are computed in 
the mobile source model ofEMPRO. 

The MEPRO will project emissions ofNOx, VOC, and CO for each year from  1991  through 
2010, using the 1990 EPA inventory for the base year.  The capability to project S02 and PM 
emission data may be provided in the future.  Projections are by source category code of 
emissions for any area in the United States.  The base year and the annual projection factors will 
be updated as new data become available.  The annual growth factors for voe, NOX,  and co are 
taken from look-up tables containing economic growth factors by county for the United States. 
The growth  factors are based on economic  forecasts applied to specific source category codes 
by the Economic Growth Analysis System (EGAS) (US  EPA,  1995a). 

The EGAS system is a PC-based tool which uses a hierarchical three-tiered approach to generate 
growth factors.  Tier 1 is the National Economic Tier.  It includes an economic  model by 
Regional Economic Models, Inc (REM!) (Treyz et al.,  1994) which is primarily based on the 
Bureau of Labor Statistics (BLS) American Workforce  J.992-2005 projections.  After 2005, the 
BLS moderate growth labor force participation rates and the Census Bureau's middle population 
projections for the United States are used to forecast the labor force.  The second tier is the 
Regional Economic Tier, the results of which are overlain on the Tier 1 results.  The Regional 
Economic Tier contains separate economic models developed by REMI for each non-attainment 
area and attainment area of each state.  The largest area addressed by one model is a state.  The 
third tier is the Growth Factor Tier, which contains commercial, residential, industrial, and electric 
utility models, and a VMT growth module.  The commercial, residential, and industrial energy 
models were developed by the Argonne National Laboratory and were used in the National Acid 
Precipitation Assessment Program (Boyd et al., 1990).  Electrical Utilities projection was 
accomplished using the Neural Network Electric Utility Model (EUMOD).  The default economic 
projection tables from EGAS are based on U.S. Bureau of Labor Statistics forecasts.  Projection 
factor tables based on forecasts by the Wharton School of Economics are provided as an option. 

Unprocessed emission 

invent 

da1a 

MEPR 0  Output Files  ) 

Gridded, temporally 
1....e1:-----t allomted, speciated 
emission data files 

Figure 4-7 MEPRO Relationship to Other MEPPS Components 

The method of applying regulatory and growth projection factors to point and area source 
emission data to obtain future year daily controlled emission data is summarized by Equation 4-
105.  Because MEPRO projects only the VMT data for mobile source emissions, the projection 
method is slightly different (Equation 4-106).  The projected VMT data arc~ passed to the 
EMPRO mobile-source emission model to be converted into projection mobile source emission 
data. 

PCONE = DCONE * PGF * (1  + AF/100) * 

[1-(PCE *PRE* PRP)]/[1-(CE *RE* RP)] 

(4-105) 

where: 

PCONE is the point or area source future-year daily controlled emissions 
DCONE is the base year daily-controlled emissions 
PGF is the projected year growth factor (percent) 
AF is the projected year emission adjustment factor (percent) 
PCE is the projected year control efficiency 
PRE is the projected year rule effectiveness 
PRP is the projected year rule penetration 
CE is the (adjusted) base year control efficiency 
RE is the (adjusted) base year rule effectiveness 
RP is the (adjusted) base year rule penetration 

For mobile source data: 

FPE = BVMT * EF * PGF * (1  + AF/100) 

(4-106) 

where: 

FPE is the future projected emissions (on-road mobile) 
BVMT is the base VMT by vehicle class by facility class by year 
EF is the emission factor 
PGF is the projected year growth factor 
AF is the projected year emission adjustment factor 

Additional details for EGAS and MPS are given the references cited above. 

4.4 

Emission Processing Interface 

An accurate characterization of the spatial and temporal variability of emissions at the surface and 
aloft is vital for realistic air quality grid modeling.  The ivIBPPS creates separate emission files for 
surface area and elevated point sources for a particular domain and time period to be modeled. 
Consequently, an interface processor program was needed to efficiently consolidate these various 
emissions types into a single, hourly gridded data file for use in grid model simulations. 

4.4.1  Overview of Key Features of ECIP 

The Emission-Chemistry· Interface Processor (ECIP) serves as the key link between the MEPPS 
system and the CMAQ Chemistry Transport Model (CCTM).  The primary function ofECIP is to 
generate an hourly 3-dimensional (3-D) emission data file for the CCTM from the individual  . 
emission file types produced by the MEPPS. 

The schematic diagram in Figure 4-8 shows the principal input files used to drive ECIP. 
The notable elevated point sources were likely separated into major and MEPSE (Major Elevated 
Point Source Emissions) source groups, based on a user-specified emission rate criterion during 
MEPPS processing.  The MEPSE group contains the largest point source emissions and is 
intended to be specially simulated by the CCTM Plume-in-Grid (PING) treatment.  However, the 
PING treatment is an optional capability of the CCTM and when it is not exercised, the MEPSE 
emission file must be processed by ECIP for the modeling scenario so that the significant 
emissions from these point sources are included in the 3-D emission data file with the other major 
point source emissions.  Thus, this capability to include or omit the MEPSE emissions in ECIP 
allows for CCTM runs to be performed without PING or with the PING tn:atment, respectively, 
while not requiring separate runs of the MEPPS emissions system.  A companion stack parameter 
file for each point source emission file is needed by ECIP for plume rise calculations.  In addition, 
meteorological data files generated by MCIP are also required to run ECIP in order to simulate 
point source plume processes.  The ECIP 3-D emission file displayed in Figure 4.6 contains the 
surface area anthropogenic and biogenic emissions, and the elevated emissions from major point 
sources and the MEPSEs, if appropriate.  The 3-D emissions output file from ECIP is ready for 
direct input into the CCTM. 

ARE 

A 

EM IS 

MEPSE 
PS EMIS 

(opt) 

MAJOR PS 

EMIS 

STACK 
PARMS 

MCIP 
MET 
DATA 

ECIP 

3-D EMIS 

CCTM 

Figure 4-8  Schematic Flow Diagram of the Input Files and 3-D Emission Data File for 

CCTM generated by ECIP 

A specific set of tasks is performed by ECIP for preparation of the 3-D emission file.  The 2-D 
gridded area emissions are incorporated into the first model layer since th(;:y represent near(cid:173)
surface releases.  In contrast, each elevated point source plume must be subjected to plume rise 
and initial vertical spread 'processes prior to the allocation of the plume emissions  into the proper 
grid cells.aloft.  Another capability ofECIP can be applied in case the CCTM domain is smaller 
than the emission domain.  ECIP can perform "spatial windowing" of the emissions needed for a 
particular CCTM domain from a larger MEPPS emissions domain. 

The methods used in ECIP to treat the processes impacting point source plumes are described in 
subsequent sections.  While these approaches employ existing scientific techniques, it is 
recognized that other formulations exist.  However, the modular design of the Models-3 coding 
structure allows for the implementation and application of alternative algorithms to treat a 
particular process. 

4.4.2  Plume Rise of Point Source Emissions 

The rise of a buoyant plume above stack height is strongly dependent upon the initial stack 
parameters and atmospheric vertical structure at the time of release.  A realistic determination of 
the height of final plume rise is important to incorporating the plume emissions into the proper 
vertical layer(s) of the model.  The initial buoyancy flux (Fh), which is a key parameter in plume 
rise formulas, is given by 

g(Ts -Ta) (Vsd 2) 
Fb  = - - - - - -

4Ts 

(4-107) 

where T5  and Ta  are the stack exit temperature and ambient temperature at stack top, 
respectively.  Other notable stack parameters in Equation 4-107 include the plume exit velocity 
(V5)  and stack diameter (d), while g is gravity.  Clearly, the magnitude of Fb is greatly influenced 
by these stack exit parameters.  For buoyant plumes, which exist for the vast majority of point 
sources, Fb  is greater than zero since Ts> Ta.  However, if Ts< Ta, then Fb is set to zero. 

The vertical profile of wind and temperature also greatly impact plume rise.  In particular, 
advances in the accuracy of plume rise estimates have resulted from taking into consideration the 
vertical variations in the thermal stability and wind structure which frequently display a strong 
height dependency in the atmosphere.  A key atmospheric stability parameter (s) used to 
distinguish the different stability regimes aloft is defined by 

S  = (g/Ta)(d8/dz) 

(4-108) 

where d8/dz is the vertical potential temperature gradient.  The value of s is employed as a 
criterion to apply the appropriate stability-dependent plume rise formula.  However, for the initial 
calculation of plume rise, the convective velocity scale (H.) is used as an indicator variable to 
identify the particular stability regime and it is defined by 

(4-109) 

where Ts  is the l .Sm air temperature, and wT is the surface heat flux covar:iance.  Unstable 
conditions are defined for H.  > 0.03H•min, while stable conditions exit when H. < -0.03H.min. 
Neutral conditions occur for the range of values between these criteria.  The default value of 
H•mln  has been set to 104  m2/s3

• 

The layer-by-layer approach described by Turner (1985), as originally suggested by Briggs 
(1975), has been applied in ECIP for the determination of final plume rise (Ah) based on the 
stability of each vertical layer.  This practical scheme takes advantage of the vertical resolution of 
the hourly, temperature and wind profiles and other 2-D meteorological parameters provided by 
the MMS dynamic mesoscale meteorological model outputs as postprocessed through MCIP. 
The method, as outlined by Turner (1985), is an iterative approach which computes plume rise 
through each layer.  An initial plume rise calculation is performed using meteorological variables 
derived at the stack top height with a stability-dependent plume rise formula at this level.  Two 
methods, linear interpolation or a surface similarity scheme by Byun (1990), are available for 
deriving temperature and wind at stack height from the modeled profile values.  For tall stacks, 
negligible differences were found in the derived values between these methods.  If the projected 
effective plume rise height Che = 1\ + Ah) exceeds the top of the layer containing the stack top, the 
amount of rise is limited to the height of the current layer top.  Then residual buoyancy flux (FJ 
is determined with an inverted form of the plume rise equation just applied.  Using FR, the 
procedure is repeated to determine the plume rise using the profile parameters for the next higher 
layer.  This method is applied over successive layers until the buoyancy flux is completely 
exhausted.  The plume rise ceases at the level where Fb = 0. 

A  set of analytical plume rise equations presented by Briggs (1984) for different atmospheric 
stabilities have been utilized in ECIP for all point sources.  The various plume rise equations are 
provided below.  The final effective plume centerline height Che)  is found by adding the computed 
plume rise (Ah) to 1\. 

4.4.2.1 

Plume Rise Treatment for Stable Conditions 

For stable atmospheric conditions, plume rise is taken from Briggs (1984) equation for bent-over 
plumes. 

113 

. 
[ F  ]
Ah  =  2.6  u: 

(4-110) 

where u is the wind speed for the layer.  Equation 4-88 is applied when H. < -0.03H. for the 
initial plume rise computation at the stack, and when s > 10·5  for subsequent layers in the current 
approach. 

4.4.2.2 

Plume Rise Treatment for Unstable Conditions 

The plume rise formula proposed by Briggs (1984) during unstable conditions is given by 

Ah  =  3[Fb/u]315H.-215 

(4-111) 

However, Briggs (1983) suggested a reasonable approximation for H.  which permits Equation 4-
111  to be applied in the following form. 

Ah  = 30 [F/u]315 

(4-112) 

The rationale for the simplification is due to the lack of data for evaluation to justify a more 
complicated form.  Equation 4-112 is applied with H.  > 0.03Hmin for the first plume rise 
computation at the stack and for higher layers for s < 10·5 

• 

#### 4.4.2.3 Plume Rise Treatment for Neutral Conditions 

.. , 

The neutral formula developed by Briggs (1984) plume rise equation has been modified into the 
following expression. 

Ah 

=  1.2[F/(uu.)]  [h5  + 1.3F/(uu.)] 

2  315 

'2  215 

(4-11~) 

where u.  is the surface friction velocity.  Equation 4'." 113 introduces minor differences from the 
other form.  Equation 4-113  is a more computationally efficient form suggested by Briggs (1983, 
communication) since his original neutral formula requires iteration to solve.  In the plume rise 
algorithm of ECIP,  the neutral plume rise equation is also solved during other stability conditions 
in the layers aloft.  A comparison between the two plume rise estimates is made and the lower 
value is selected before proceeding. 

#### 4.4.2.4 Special Conditions 

For the cases when Fb =  0,  plume rise can occur due to momentum provided by the exit velocity 
of the plume out of the stack.  Therefore, a momentum rise formula (Turner, 1985) has also been 
implemented to consider these situations and is given by 

Ahm  =  3d  v/u 

(4-114) 

If Equation 4-114 is selected, no further plume rise computations are performed. 

Another situation occurs when the stack is below the PBL height (zJ under unstable conditions 
and the condition Chs - zi)  < 200 m applies.  A treatment for limited plume penetration above the 
PBL  is determined when this situation is triggered.  This condition most often occurs during the 
morning period.  Since zi is generally growing rapidly, this period is generally brief.  To consider 
plume penetration of the overlying stable layer, a practical algorithm employed previously by 
Byun and Binkowski (1991) has been implemented based on Briggs (1984).  Ifh5  is less than 200 
m below Zj, then the following equation is solved. 

~ = 3.9[F/us] 113 

(4-115) 

If Zj  is greater than zb,  , then the plume top height (zj is set to zi  and he is defined to be 2/3Zi_. 
However, plume penetration is permitted when zi  < zb in Equation  4-115.  For this case, the 
plume top height is defined to be the height of the top of the next higher layer and the effective 
plume height is again computed as 2/3hi. 

4.4.3  Method for the Treatment of Initial Vertical Plume Spread 

Buoyancy-induced turbulence promotes plume expansion during the rise phase.  A widely-used 
method from Briggs (1975) designates the vertical thickness of a plume to be equivalent to the 
amount of plume rise.  With this method, the heights of the top and bottom  of the plume are 
determined by 

ht  =  h5  +  1.5.!lh 
hb  = h5 
- 0.5.!lh 

(4-116) 

where hi  and hb are the heights of the plume top and bottom, respectively.  Since the plume 
thickness is directly related to the amount of plume rise, this approach leads to rather thick plumes 
during the nocturnal period.  Experimental plume dimension data suggest more limited vertical 
thickness for plumes during the nighttime hours.  As an alternative,  an empirical form has also 
been included.  It is based on analyses of observed plume dimensions and vertical temperature 
gradients (Gillani, 1996 communication).  He found the best-fit empirical result is given by 

a  =  Ae (-BdT/dz) 
z 

(4-117) 

where the standard deviation of plume depth ( az ) is a function of the vertical temperature 
gradient (dT/dz) at he.  Values for A and Bare given by 10 and 117, respectively.  A minimum 
value specified for az is 3 m.  With this method,  hi  and hb are determined to be ±2. l 5az above 
and below I\ , respectively.  This approach provides for smaller plume thicknesses during the 
nocturnal period. 

4.4.4  Vertical Allocation of Plume Emissions 

Rather than dumping the entire emissions of a plume into a single layer, an approach has been 
developed to allow for the allocation of plume emissions into multiple layers since a plume can 
often span more than one layer.  This situation occurs often as more vertical layers are used in the 
model since model layers are thinner. 

· 

Once h1  and hh have been computed for each plume, these values, along with the heights of the 
model layer interfaces (Zz) are employed to determine the fractional amount of plume overlap 
across each layer.  The method uses the fractional amount of the plume depth residing within a 
layer in order to weight the amount of plume emissions incorporated into a particular layer.  If 
both hi  and hh  are contained within a particular layer, all the plume's emissions are allocated into 
one layer.  As noted above, the number oflayers receiving plume emissions is also dependent on 
the number of vertical layers in the model.  A model configuration with fewer vertical layers 
generally implies greater layer thicknesses .. 

4.4.5  Generation of 3-D Emissions 

Once the plume rise and plume partitioning functions have been performed, the emissions from 
each point source plume are transferred to the 3-D emission array which also contains the surface 
area emissions in the first layer.  The 3-D emission array is written at an hourly interval for the 
entire simulation period to a data file in a format compatible for use in CCTM simulations. 

4.5 

Data Requirements 

The following items are the data input requirements for operation ofMEPPS.  Those items that 
must be supplied by the user are marked (U).  Those items that are fixed internal lookup tables, 
provided with Models-3, or that may be generated in MEPPS are marked (I). 

• 

Complete annual (point, area, and mobile-source data by source category code) Regional, 
National, or international emission inventories are necessary for regional modeling.  The 
inventories preferably should be in the ASCII format of the EPA National Emission 
Trends (NET) inventories.  However, IDA allows import and conversion of any inventory 
with known fields formats in ASCII, SAS, or NetCDF format.  Currently, the 1985 
NAPAP, 1988 National Inventory, 1990 National Interim Inventory, and 1990 National 
Emission Trends (NET) inventories for criteria pollutants are included.  Limited data for 
southern Canada· are available as a part of these inventories (I).  Emission inventories for 
other areas, years, or pollutants must be supplied by the user (U). 

Existing hourly emission data, such as CEM data, may be used directly or substituted for 
hourly emission data derived by temporal disaggregation from annual emission inventories. 
The hourly data may be imported through the File Converter and IDA.  The Models-3 
system is includes 1995 CEM data in the SAS data set format provided by the US EPA 
Office of Acid Rain (I).  Other hourly data must be supplied by the user if desired (U). 

Hourly meteorological data for surface temperature and solar radiation, in  NetCDF  I/O 
API  format, (from MM5 processed via MCIP) must be available for modeling biogenic or 
mobile source emission data.  If data files for the appropriate case area available, the user 
may select them in MEPPS or when running Study Planner.  Otherwise, it is necessary to 
first run MM5 and output the meteorology files through MCIP (I). 

Chemical speciation profiles matched to source category codes are necessary for the 
speciation processor (I).  These may be updated with new information. 

Temporal allocation profiles, by source category code, must be available to accomplish 
temporal allocation of emission data to hourly emission data (I).  These may be updated 
with new information 

Geographic coverages for surrogate spatial allocation (gridding) of emission data.  The 
user may supply additional coverages:  Those supplied with MEPPS include (I): 

Political boundaries at the county level for North America 
Land-water boundaries and features for North America 
County-level population information for North 

America (with a gridded surrogate for Canada) 

Federal Highway Administration major highway 

coverage for the United States 

TIGER-LINE detail road coverage for the United 

States 

Land cover for North America.  Currently  is at 

county-scale for the United States and gridded 
at coarser resolution for Canada and Mexico. 
Land cover for the United States at one 
kilometer resolution is anticipated in 1999 

Species emission factors for biogenic emission modeling (I) 

Standard Mobile 5a model input information by political area (usually state and/or 
county), including vehicle fleet composition data, fuel type use by geographic area, 
inspection and maintenance program information, etc, (U).  A template with examples is 
provided for the user to edit. 

Road silt loading, geographic distribution of paved and unpaved roads, vehicle fleet 
composition data, fuel use data, and inspection and maintenance data are necessary as 
input to the PARTS mobile particulate model. 

Geographic, source category code-specific growth projection factors.  These are provided 
for use with MEPRO (I). 

Source category and/or geographically specific emission control data, and regulatory 
factors including control efficiency, rule effectiveness, and rule penetration for use in 
MEPRO.  These must be supplied by the user because controls are not standard, and in 
fact are a key variable in examining different emission scenarios (U). 

4.6 

Plans for Improvement 

Plans for improvement ofMEPPS may be divided into long and short-term improvements.  The 
short-term improvements are those that are anticipated to be in the Models-3 release scheduled 
for the summer of 1999. 

Short-term Improvements 

• 

• 

The IDA  will be improved to consolidate and further automate much of the quality 
control processing, format and unit conversion, and data file manipulation. In particular, 
format templates of internal formats will be added to assist  importing of emission data 
files into the system, and quality control for the CEM data will be enhanced. 

Although SAS® has proved to be a useful tool to date, increasing data handling 
requirements will likely overwhelm the data handling capabilities of SAS®.  Therefore, it is 
necessary to convert MEPPS to a fully Models-3 framework compliant system in order to 
take full advantage of the or object-oriented data base architecture of the system (eg., 
Orb ix®) to more efficiently manage very large amounts of data.  The use of the Sparse 
Matrix Operator Kernel Emission (SMOKE) system,  in conjunction with the Models-3 
object-oriented architecture and expansion of existing functionality should substantially 
improve performance because relatively inefficient processing sequences will not be 
necessary, and it will not be necessary to manipulate all elements of large files for each 
operation. More information about SMOKE can be found in Coats et al. (1995).  An 
initial (but not complete) version of SMOKE is planned for installation by summer 1999. 

Long-term Improvements 

New mobile source emission estimation models are being developed by the U.S. EPA 
Office of Mobile Sources.  They are scheduled for completion late summer of 1999. 
Mobile. 6 will replace Mobile Sa and Mobile Sb, and an Off-road Mobile Source Model 
will be intr.oduced.  These models will be installed in MEPPS when they are available and 
resources allow. 

The MEPPS contains the split factor assignments for two common chemical speciation 
mechanisms, CB-4 and RADM 2, that may be selected by users.  These speciation 
mechanisms will also be in SMOKE.  There are plans to also install the split factor 
information and computational mechanisms necessary to use for the Statewide Air 
Pollution Research Center (SAPRC) (Carter,  1988), and eventually, code necessary to 
support the more complex Morphecule mechanism being developed at the University of 
North Carolina. 

Additional quality control and reporting capabilities will be added to SMOKE, equaling or 
surpassing those capabilities in currently in MEPPS. 

The emission data processing system could be enhanced to support nested grid structures. 
Currently, there is only limited support for nested grid structures and no support for 
multiscale grid structures.  Under the current formulation, EMPRO (including the gridding 
processor) must be run consecutively for each grid structure that exists within the nested 
grid structure.  Repetitive runs are inefficient, and computer resour1~es are poorly utilized 
because certain areas in the modeling domain will be processed more than once.  In 
addition, the gridding processor cannot generate rotated grids, which may be a limitation 
for some applications.  If an air quality modeling study requires a rotated emission 
modeling grid, a knowledgeable ARC/INFO® user must prepare the~ grid independently 
from menu options provided in Model-3 and MEPPS. 

Tools such as NetCDF and the 1/0 API will evolve to directly accommodate geographic 
data references in their structure.  This will allow manipulation of geographic data 
(gridding, for example) to be accomplished without the use of non-conforming 
commercial software tools now in Models-3 system. 

4. 7 

References 

Alados, I., I.  Foyo-Moreno, and L. Alados-Arboledas, 1996: Photosynthetically Active 
Radiation: Measurements and Modeling, Agri.  and Forest Meteor., 78, pp.  121-131. 

Beck, L., L.A .. Bravo, B.L. Peer, M.L. Saeger, and Y.  Yan,  1994:  A Data Attributes Rating 
System, Presented at the International Conference on the Emission Inventory:  Applications and 
Improvement, Raleigh, NC., November 1-3, 1994, 12 pp. 

Boyd, G.A., E.C. Kokkelenberg, and M.H. Ross,  1990:·Sectoral Electricity and Fossil Fuel 
Demand in U.S. Manufacturing:.Developnient of the Industrial Regional Activity and Energy 
Demand (INRAD) Model; Argonne National Laboratory, Argonne, IL. 

Briggs, G.A., 1975: Plume Rise Predictions. In: Lectures on Air Pollution and Environmental 
Impact Analyses, Workshop, Proceedings, Boston, MA, 1975, pp 59-111. 

Briggs, G.A., 1983 communication: Plume rise equations used in EPA models.  Meteorology 
Division, Environmental Sciences Research Lab.; Research Triangle Park, NC. 

Briggs, G.A., 1984:. Plume Rise and Buoyancy Effects. In: Atmospheric Science and Power 
Production, D.R. Anderson, Ed., DOEITIC-27601  (DE84005177),Technical Information 
Center, U.S. DOE, Oak Ridge, TN, 850 pp. 

Byun, D.W., 1990: On the.Analytical Solutions of Flux-profile Relationships for the 
Atmospheric Surface Layer.  J.  of Appl. Meteorol., 29, No. 7, 652-657 

Byun, D.W. and F.S. Binkowski, 1991: Sensitivity ofRA.DM to Point Source Emissions 
Processing. Seventh AMS/A WMA Joint Conf. on Applications of Air Poll. Meteorol., New 
Orleans, LA, Jan.  14-18, 1991, Preprints, Amer. Meteorol. Soc., Boston, MA., 1991, pp. 70-73 . 

. :: 

Carter, W.P.L., 1988:  Documentation.for the SAPRC Atmospheric Photochemical Mechanism 
Preparation and Emissions Processing Programs for Implementation in Airshed Models, Prepared 
for the California Air Resources Board, Report AS-122-32. 

Coats, C.J., Jr.,  1995:  High Performance Algorithms in the Sparse Matrix Operator Kernel 
Emissions (SMOKE) Modeling System, Microelectronics Center of North Carolina, 
Environmental Systems Division, Research Triangle Park, NC,  6 pp. 

E.H. Pechan and Asso.ciates,  1994:  Industrial SO~ andNO,c Tracking System, EPA Contract No. 
68-Dl-0146, 38 pp. 

Emission Inventory Improvement Program (EIIP), 1996: Biogenic Sources Preferred Methods, 
Volume V, prepared for the EIIP Area Sources Committ<::e by the Radian Corporation, Research 
Triangle Park, NC, 104 pp. 

Fratt, D.B., D.F. Mudgett, and R.A. Walters, 1990: The 1985 NAPAP Emissions Inventory: 
Development of Temporal Allocation Factors, EP A-60017-89-01 Od,  U.S. Environmental 
Protection Agency, Office of Research and Development, Washington, D.C., 209 pp. 

Geron, C.D., A.B. Guenther, and T.E. Pierce,  1994: An Improved Model for Estimating 
Emissions of Volatile Organic Compounds from Forests in the Eastern United States, J.  Geophys. 
Res., 99, D6, pp.  12773-12791. 

Gery, M.W., G.Z. Whitt~n, J.P. Killus, and M.C. Dodge, M.C., 1989:  A Photochemical Kinetics 
Mechanism for Urban and Regional Scale Computer Models, J.  Geophys.  Res., 94, DlO, 12295-
12956. 

Gillani, N.V., 1996:  personal commUnication: Analyses results of plume data. 

Guenther, A.B., P. Zimmermann, and M.  Wildemuth, 1994: Natural Volatile Organic Compound 
Emission Rate Estimates for U.S. Woodland Laridscapes, Atmos.  Environ., 28, 1197-1210. 

Kinnee, E., C. Geron, and T. Pierce, 1997: United States Land Use Inventory for Estimating 
Ozone Precursor Emissions? Ecol.  Appl., 7, 46-58. 

Middleton, P., W.R. Stockwell, and W.P.L. Carter, 1990: Aggregation and. Analysis of Volatile 
Organic Compound Emissions for Regional Modeling, Atmos. Environ., 24A, pp.  1107-1133. 

Modica, L.G., D.R. Dulleba, R.A. Walters, and J.E. Langstaff, 1989:  Flexible Regional Emissions 
Data System (FREDS) Documentation for the 1985 NAP AP Emissions Inventory, EP A-60019-
89-047, U.S. Environmental Protection Agency, Research Triangle Park, NC, 574 pp. 

Monroe, C.C., T.A. Dean, and W.R. Barnard, 1994:  Multiple Projections System:  Version 1.0 
User's Manual, EPA-600/R-94-085, 70 pp. 

Moody, T., J.D. Winkler, T. Wilson, and S. Kersteter, 1995:  The Development and Improvement 
of Temporal Allocation Factor Files, EPA-600/R-95-004, 457 pp. 

Moran, M.D., M.T. Scholtz, C.F. Slama, A. Dorkalam, A. Taylor, N.S. Ting, D. Davies, C. 
Sobkowicz, P.A. Makar, and S.  Venkatesh,  1998: An Overview of CEPSl.O: Version 1.0 of the 
Canadian Emissions Processing System for Regional-Scale Air Quality Models, in the Proceedings 
of a Specialty Conference on Emission Inventory: Planning for the Future, Air and Waste 
Management Association, Pittsburgh, VIP-77, Volume I, 95-106. 

Morris R.E., M.A. Yocke, T.C. Myers and V. Mirabella, 1992:  Overview of the Variable-Grid 
Urban Airshed Model (UAM-V), Paper presented to the Air and Waste Mangement Association, 
85th Annual Meeting, Kansas City, MO, June 21-26,  1992,  13 pp. 

Ozone Transport Assessment Group,  1997:  Emission Inventory Development Report (Draft), 3 
Volumes. 

· 

Pierce, T.E., B.K. Lamb, and A.R. V anMeter, 1990: Development of a Biogenic Emissions 
Inventory System for Regional Scale Air Pollution Models,  Paper presented to the Air and 
Waste Management Association, 83rd Annual Meeting, Pittsburgh, PA, June 24-29, 1990, 16 pp. 

Pierce, T., C. Heron, L. Bender, R.  Dennis, G. Tennyson, and A.  Guenther, 1998: The Influence 
of Increased Isoprene Emissions on Regional Ozone Modeling, J.  Geo phys.  Res., in press. 

Saeger, M., J.  Langstaff, R.  Walters, L. Modica, D. Zimmerman, D. Pratt, D. Dulleba, R. Ryan, J. 
Demmy, W.  Tax., D. Sprague, D. Mudgett, and A.S. Werner,  1989:  The 1985 NAPAP 
Emissions Inventory (Version 2):  Development of the Annual Data and Modelers' Tapes, U.S. 
Environmental Protection Agency, Office of Research and Development, EPA-600/7-89-012a,' 
692 pp. 

Stockwell, W.R., P. Middleton, and J.S.  Chang, 1990: The Second Generation Regional Acid 
Deposition Model Chemical Mechanism for Regional Air Quality Modeling, J.  Geophys.  Res., 95 
(DlO), pp.  16,343-16367. 

Treyz, G. et al.:  1994: Model Documentation for the REMI EDFS-14 Forecasting and Simulation 
Model, Regional Economic Models, Inc., Amherst, MA. 

Turner, D.B.,  1985: Proposed Pragmatic Methods for Estimating Plume Rise and Plume 
Penetration through Atmospheric Layers. Atmos.  Environ., 19, 1215-1218. 

U.S. EPA, Office of Mobile Sources, 1985: Size Specific Total Particulate Emission Factors for 
Mobile Sources, EPA-46013-85-005, 294 pp. 

U.S. EPA, Office of Air Quality Planning and Standards, 1988:  Air Species Manual:  Volume 1, 
Volatile Organic Compound Species Profiles, EPA-450/2-88-003a, 492 pp. 

U.S. EPA, Office of Research and Development, 1989: Development of the Regional Oxidant 
Model Version 2.1, U.S.  EPA  Technical Report. 

U.S. EPA, Office of Mobile Sources, 1991:  User's Guide to MOBILE 4.1  (Mobile Source 
Emission Factor Model), EPA-AA-TEB-91-01, 354 pp. 

· 

U.S. EPA, Office of Air Quality Planning and Standards, 1992:  User's Guide forthe Urban 
Airshed Model, Volume IV:  User's Manual for the Emissions Preprocessor System 2.0, EPA-
450/4-90-007D(R), 497 pp. 

U.S. EPA, Office of Air Quality Planning and Standards,  J.993:  Regional Interim Emission 
Inventories 1987-1991), EPA-454/R-93-0212, 54 pp. 

U.S. EPA, Office of Air Qaulity Planning and Standards, 1994:  Emissions Inventory for the 
National Particulate Matter Study, Final Draft, Prepared by E.H. Pechan, Inc., EPA Contract No. 
68-D3005,  WA  No.  0-10, 83  pp. 

U.S. EPA, Office of Air Quality Planrung and Standards, 199S:  Compilation of Air Pollutant 
Emission Factors:  Volume II:  Mobile Solirces, AP-42, Fifth Edition, 2931)p.  · 

• 

• 

•  ) 

, 

• 

• t  • ' 

• 

' 

. 

• 

• 

• 

• 

• 

·, 

~. 

U.S. EPA, Office of Research and Development, 199Sa:  Econo,mic ~owth Analysis System: 
User's Guide Version 3.0, EPA-600/R-95-132b, 89 pp. 

U.S. EPA, Office of Mobile Sources, 199Sb: Draft User's Guide to PARTS: A Program for 
Calculating Particle Emissions from Motor Vehicles, EPA-AA-AQAB-94-2, 67 pp. 

U.S. EPA, Office of Mobile Sources, 1996:  User's Guide for MOBILE Sa (Mobile Source 
Emission Factor Model), EPA-AA-TEB-92-01,  176 pp. 

U.S. EPA, Office of Air Quality Planning and Standards, 1997: Air CHIEF, Version S (Compact 
Disk containing air quality speciation profiles, emission factors, and other ~~mission-related data), 
Research Triangle Park, NC. 

U.S. EPA, Office of Air Quality Planning and Standards, 1998: National Air Pollutant Emission 
Trends Procedures Document, 1900-1996 Projections 1999-2010,  EPA-454/R-98-008, 712 pp. 

U.S. EPA, Office of Research and Development,  199S:  Models-3:  Volume 7,  System 
Requirements (Draft) . 

U.S. EPA, Office of Research and Development, 199S:  Models-3:  System Design (Draft). 

Walters, R.A. and M.L. Saeger, 1990:  The 1985 NAP AP Emissions Inventory:  Development of 
Species Allocation Factors, EPA-60017-89-0JOJ, 470 pp. 

Wilkinson, J.G., C.F. Loomis, D.E. McNally, R.A. Emigh, and T.W. Tesche, 1994:  Technical 
Formulation Document:  SARMAP/LMOS Emissions Modeling System (EMS-95), Final Report, 
Lake Michigan Air Directors Consortium and the California Air Resources Board, AG-90/TS26 
and AG-90!TS27, 120 pp. 

Williams, E., A. Guenther, and F. Fehsenfeld, 1992: An Inventory of Nitric: Oxide Emissions from 
Soils in the United States, J.  Gaffes.  Res., 97, 7511-7519. 

Yienger, J.J. and  H. Levy II, 1995: Empirical Model of Global Soil-Biogenic Nox Emissions, J. 
Gaffes.  Res., 100, No. D6, 11447-11464. 

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_Science_Ch_03.md) - [Home](README.md) - [Next Chapter >>](CMAQ_Science_Ch_05.md)<br>
CMAQ Science Document (c) 1999<br>

<!-- END COMMENT -->

