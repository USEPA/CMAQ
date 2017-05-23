&EPA 

""'"""" ... ,.cm:::<> 
Environmental Protection 
Agency 

umce ot Hesearch and 
Development 
Washington DC 20460 

EPA/600/R-99/030 
March 1999 

Science Alg<>rithms of the 
EPA Models-3 Community 
Multiscale Aiir Quality 
(CMAQ) Modeling System 

United States 
Environmental Protection 
Agency 

Office of Research and Development 

Washington, DC 20460 

EP N600/R .. 99/030 
March 1999 

SCIENCE ALGORITHMS OF THE EPA MODELS-3 

COMMUNITY MULTISCALE AIR QUALITY (CMAQ) 

MODELING SYSTEM 

Edited by: 

D. W. BYUN• and J. K. S. CHING• 

Atmospheric Modeling Division 

National Exposure Research Laboratory 
U.S. Environmental Protection Agency 

Research Triangle Park, NC 27711 

*On assignment from the National Oceanic and Atmospheric Administration, U.S. Department of 
Commerce 

@  Printed on Recycled Paper 

DISCLAIMER 

BP A/600/R-99/030 

The information in this document has been funded wholly. or in part by the United States 
Environmental Protection Agency.  It has been subjected to the Agency's peer and administrative 
review, and has been approved for publication as an EPA document.  Mention of trade names or 
commercial products does not constitute endorsement nor recommendation for use. 

FOREWORD 

The Models-3 Community Multiscale Air Quality (CMAQ) modeling system has been developed 
under the leadership of the Atmospheric Modeling Division of the EPA National Exposure 
Research Laboratory in Research Triangle Park, NC.  This new generation of modeling software 
was under development for seven years and was made available in June 1998 without charge for 
use by air quality regulators, policy makers, industry, and scientists to address multiscale, 
multi-pollutant air quality concerns. 

Models-3/CMAQ has a unique framework and science design that enables scientists and 
regulators to build their own modeling system to suit their needs.  Users can access pre-installed 
modeling systems provided by the EPA or can incorporate their own modeling systems to work 
within the existing framework software. 

This direct user involvement is key to the concept of a community modeling and analysis 
system.  This approach to model development, application, and analysis leverages the 
community's complementary talents and resources to set new standards for rapid incorporation of 
better science into air quality model applications.  The resulting comprehensive system forms the 
foundation upon which the community, including governments, industry, academia, and other 
stakeholders, can collaborate in the examination of issues and the subsequent development of 
strategies that meet society challenges of environmental protection. 

The release of Models-3/CMAQ is one of the many steps which we hope will unite the 
community under the common goal of advancing our knowledge and abilities to tackle critical 
problems of the future in far more effective ways than have been attempted in the past. 
Scientifically sound modeling systems, developed and supported by the community, are one 
method of achieving this goal. 

The June 1998 release of the Models-3/CMAQ computer code was accompanied by a User 
Manual [EP A-600/R-98/069(b)] to serve as a reference on how to use the software system.  This 
Science Document is the counterpart to the User Manual in that it presents the peer reviewed 
scientific bases for the Models-3/CMAQ modeling systems.  This document also includes 
components such as interface processors, process analysis routines, and the present and planned 
evaluation program. 

F. A.  Schiermeier 
March 1999 

v 

CONTENTS 

FOREWORD  ................................. · ............................... v 

ACRONYMS  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  xvn 

ACKNOWLEDGMENTS  .................................................... xxi 

EXECUTIVE SUMMARY  .................................................. ES-1 

1. 

2. 

1.1 
1.2 
1.3 
1.4 

INTRODUCTION TO THE MODELS-3 FRAMEWORK AND THE 
COMMUNITY MULTISCALE AIR QUALITY MODEL (CMAQ) 
(J.  Ching and D.  Byun) 
Abstract  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  1-1 
1.0 

Introduction to the Models-3 Framework and the Community Multiscale Air 
Quality Model (CMAQ)  .......................................... 1-2 
The Models-3 Emissions, Meteorology, and the CMAQ Modeling Systems  . .  1-4 
CMAQ.Interface Processors  ....................................... 1-6 
The CMAQ Chemical Transport Model (CCTM)  . . . . . . . . . . . . . . . . . . . . . . .  1-8 
Analysis of CMAQ Output  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  1-10 
1.4.1  Process Analysis (Chapter 16)  ... : . . . . . . . . . . . . . . . . . . . . . . . . . . .  1-10 
1.4.2  Aggregation (Chapter 17)  .................................. 1-11 
1.5  Management of CMAQ Science Information Objects and Codes in Models-3  1-11 
1.5.1  Program Control Processors (Chapter 15)  ...................... 1-11 
1.5.2  CMAQ Code Integration (Chapter 18)  ........................ 1-12 
Post Release Studies and Near-Future Plans  .... · ...................... 1-12 
1.6.1  CMAQ Evaluation Study ................................... 1-12 
1.6.2  Testing Operational Configurations . . . . . . . . . . . . . . . . . . . . . . . . . . .  1-12 
1.6.3  Extensions and Science Additions  . . . . . . . . . . . . . . . . . . . . . . . . . . . .  1-13 
Opportunities and Encouragement for Long Term Extensions and Science 
Community Involvement  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  1-13 
References  .................................................... 1-15 

1.6 

1. 7 

1.8 

MODELS-3 ARCHITECTURE: A UNIFYING FRAMEWORK FOR 
ENVIRONMENTAL MODELING AND ASSESSMENT 
(J.  Novak and S.  Leduc) 
Abstract  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  2-1 
2.0  Models-3 Architecture: A Unifying Framework for Environmental Modeling and 

Assessment ..................................................... 2-2 
Introduction  .................................................... 2-2 
Overview of the Models-3 Frame~ork  ..................... ."  ......... 2-2 

2.1 
2.2 
2.2.1  Dataset  Manager  : . · ................... : ........ , ........... 2-2 
2.2.2  Program Manager ................ , ......................... 2-3 
2.2.3  Study Planner ...................... , · ................ , ...... 2-3 
2.2.4  Strategy Manager  ................... : ............... , ...... ,  2-4 
2.2.5  Tool Manager ............... · •.... · ....... · .......... , , ......... 2-4 
2.2.6  Science Manager  . . . . .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  2-6 
2.2. 7  Model Builder  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  2-6 
2.2.8  Source Code Manager  .......................... , ........... 2-7 
2.2.9  Framework Administrator  ................................... 2-7 
2.3  Models-3  System Architecture  ......... , .... , . ·,  , . · .................. 2-7 
Schedule and Future Plans  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  2-10 
2.4. 
References  ..................................... · . , . . . . . . . . . . . . . .  2-10 
2.5 

DEVELOPING METEOROLOGICAL FIELDS 
(I'.  Otte) 
Abstract  ....................................... , , . . . . . . . . . . . . . . . . . . . .  3-1 
3.0 
Developing Meteorological Fields ... , .................... ; .......... 3-2 
3.1 
Credits and Disclaimers for Use ofMM5  ..... ; ........... , ........... 3-2 
3.2  Meteorology Model Pre-Processing  ....................... , ......... 3-2 
3.2.1  Defining the Simulation Domain (TERRAIN)  .................... 3-2 
3.2.2  Processing the Meteorological Background Fields (DATAGRID)  .... 3-3 
3.2.3  Objective Analysis (RAW/NS)  ................ , ............... 3-3 
3.2.4  Setting the Initial and Boundary Conditions (INTERP)  ............ 3-4 
The Meteorology Model (MM5)  ........... , ......................... 3-4 
3.3.1  Brief History  .............................................. 3-4 
3.3.2  Horizontal and Vertical Grid  .................................. 3-5 
3.3.3  Prognostic Equations  .................. · ..... _ ................ 3-5 
3.3.4  Model Physics  ....................... ; .... ~  ............... 3-6 
3.3.5  Nesting  ........... ; .................. : .•................ 3-10 
3.3.6  Four-Dimensional Data Assimilation  .... ~  .... , ............ ; ..  3-10 
3.4  Meteorology Model Post-Processing ................................ 3-11 
3.5 
Changes to the MM5  System's Software for Models-3  .................. 3-11 
References  ............. ; .... · ................. , ................. 3-12 
3.6 

3.3 

EMISSION SUBSYSTEM 
(B.  Benjey, J.  Godowitch,  and G.  Gipson) 
Abstract  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ...  . . . . . . . . . . . . . . . . . .  4-1 
Emission Subsystem  ............................................. 4-2 
4.0 
4.1 
Emission Inventory Processors  ......................... , ........... 4-2 
4.1.1  Discussion  ............................................... 4-2 
4.1.2  General MEPPS Structure  .................................. 4-11 
The MEPPS Emission Processing System ............................ 4-12 
4.2.1  The Inventory Data Analyzer (IDA)  .......................... 4-14 

4.2 

4.2.2 
the MEPPS Input Processor (INPRO)  ........................ 4-16 
4.2.3  Processing Procedure  .......................... : . . . . . . . . . . .  4-17 
4.2.4  Modeled Emission Data ..................................... 4-33 
4.2.5  Chemical Speciation of Emission Data  .............. ·.  . . . . . . . . .  4-78 
4.2.6  Output Processor (OUTPRO)  ... · .... ; ....... · ...... · ........... 4-90 
4.3  Models-3 Emission Projection Processor (MEPRO)  .................... 4-91 
4.4 
Emission Processing Interface  ................... : .. : . . . . . . . . . . . . . .  4-94 
4.4.1  Overview of Key Features ofECIP  .· ..... : ........ :·.--.  ......... 4-94 
4.4.2  Plume Rise of Point Source Emissions  .... : ~·  ... · . . . . . . . . . . . . . . .  4-96 
4.4.3  Method for the Treatment of Initial Vertical Plunie Spread  . . . . . . . .  4-99 
4.4.4  Vertical Allocation of Plume Emissions  .. ; ......... -....... ~.  .. .  4-100 
4.4.5  Generation of 3-D Emissions .................. · .......... · ... 4-100 
Data Requirements  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  4-100 
Plans forlmprovement  ....... .' .......... : ...... ; ... .'. · .... ·.: .. ·.: .. 4-102 
References  ............. ~  ................................. · .... 4-103 

4.5 
4.6 
4. 7 

5 .1 

5.2 
5.3 

FUNDAMENTALS OF ONE-ATMOSPHERE DYNAMICS FOR MULTISCALE 
AlR QUALITY MODELING 
(D.  Byun) 
Abstract  ........................................................ · ......................................... : ........................  5-1 
5.0 

Fundamentals of One-Atmosphere Dynamics for Multiscale Air Quality 
Modeling  ............................... : ....... : ........................................... : ........................  5-2 
· Governillg Equations and Approximations for the Atmosphere .. .. .. .. .. .. .... .. .. . . . .  5-2 
5.1.1 ·  Governing Equations in a Generalized Curvilinear Coordinate System  5-3 
5.1.2  Assumptions of Atmospheric Dynrunics  ................ : ..............................  5-6 
Choice of Vertical Coordinate System for Air. Quality Modeling  ...................  5-11 
Coupling of Meteorology and Air Quality ..................................... ." .................  5-18 
5.3.1  Meteorological Data for Air Quality Modeling  .. ~ ................................  5-18 
5.3.2  Off-line and On·line Modeling Paradigms  ............. · .............................  5-18 
5.4  Mass Conservation ............................................................................................  5-22 
5.4.1  Mass Consistency in Meteorological Data ........................ : ..................  5-22 
5.4.2  Techniques for Mass Conservation in Air Quality Models  .................  5-2'.3 
5.4.3  Temporal Interpolation of Meteorological Data  ..................................  5-25 
5.5  ·  Conclusion  .......................................................................................................  5-26 
5.6 
References  .............................................................. , ..... : ............. ~ .. : ... ;; .. : .. : .......  5-27 
Appendix SA.  Tensor Primer and Derivation of the Continuity Equation in a 

Generalized Curvilinear Coordinate System .................................. ; .....  5-30 

Governing Equations And Computational Structure of The  Community 

GOVERNING EQUATIONS AND COMPUTATIONAL STRUCTURE OF THE 
COMMUNITY MULTISCALE AIR QUALITY (CMAQ) CHEMICAL 
TRANSPORT MODEL 
(D.  Byun, J.  Young,  and MT.  Odman) 
Abstract . .. .. . .. .. .. . .. .. .. . . . . .. .. .. . . .. .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .. .  6-1 
6.0 
Multiscale Air Quality (CMAQ) Chemical Transport Model  .......................................  6-2 
Derivation of the Atmospheric Diffusion Equation  ...........................................  6-3 
6.1 
Representation of Science Processes in CMAQ Modeling System .. ..... ..... .......  6-9 
6.2 
6.2.1  Supporting Models and Interface Processors  ...................... ...................  6-9 
6.2.2  Modularity Concept of CMAQ  ... .. ........... .. .. .. .. ....... ..... .................. .... ..  6-10  . 
6.2.3  Description of Science Processes ...... .. . .. .. ............ .. .. .. ....... ... ........ .. ......  6-15 
Equivalent Model Formulations for Different Vertical Coordinates  ...............  6-26 
6.3 
Nesting Techniques ...................................................................................... : ...  6-28 
6.4 
Summary  ..........................................................................................................  6-31 
6.5 
6.6 
References  .........................................................................................................  6-31 
Appendix 6A.  Concentration Units Used for Air Quality Studies  ..............................  6-34 

NUMERICAL TRANSPORT ALGORITHMS FOR THE COMMUNITY 
MUL TISCALE AIR QUALITY (CMAQ) CHEMICAL TRANSPORT MODEL 
IN GENERALIZED COORDINATES 
(D.  Byun, J.  Young,  J.  Pleim,  MT.  Odman,  and K  Alapaty) 
Abstract  . . .. . .. .. . . . .. . . . .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  7-1 
7 .0 

Numerical Transport Algorithms for the Community Multiscale Air Quality 
(CMAQ) Chemical Transport Model in Generalized Coordinates  ....................  7-2 
Numerical Advection Algorithms  ....................................... , ..............................  7-3 
7 .1.1  Conservation Form Equation for Advection .. .. .. .. . . .. . . . . .. .. . .. . . . . . .. . . . .. . . . . . . .  7-4 
7 .1.2  Classification of Advection Schemes  . ........ ............ .. .............. ... ............  7-6 
7.1.3  Description of Advection Schemes in CCTM  .......................................  7-6 
7 .1.4  Treatment of Boundary Conditions  .. .... ................ .. .. ............... ..... ... ....  7-11 
7.1.5  Test of Algorithms with Idealized Linear Horizontal Flow Fields  ......  7-12 
7.1.6  Vertical Advection  ...............................................................................  7-15 
7 .1. 7  Adjustment of Mass Conservation Error  . ..... ............. ........ .. .............. ..  7-16 
Vertical Mixing Algorithms . . . . .. . . .. . .. . . . . . . . . . . . .. . . . . .. . .. . . . . . . . . . . . . . .. .. .. . . .. . . . . . . . . .. . . . . . . . ..  7-18 
7.2.1  Closure Problem ................................................... ." ...............................  7-19 
7.2.2  Computing Vertical Mixing with the Eddy Diffusion Formulation: 

7.1 

7 .2 

K-Theory  ............................................................................. · .................  7-21 
7.2.3  Flux Form Representation of Vertical Mixing .....................................  7-31 
7 .3 
Horizontal Mixing Algorithms  . . . . . . .. . .. .. .. .. .. . . . . . .. .. .. . . .. . . . . . . . . . . . . . .. . . .. .. . .. . . . . . . . . . . . . . .  7-4 3 
Conclusions ...................... .... .. .. .... .............. .... .. .. .... .. ...... ...... .... ..... .. ... . .... ... ..... .  7-45 
7 .4 
7 .5 
References  . . . . .. ... . ... .. . . . .. . . . . .. . .. .. . . .. . . . . . . . . . . . .. .. .. .. . . . . . . . . . . . . . . . . .. .. .. . . . . . . . . . . . . . . . . . .. . . . . . . . . .  7-46 
Appendix 7A  Numerical Solvers for Diffusion Equations .........................................  7-51 

GAS-PHASE CHEMISTRY 
(G.  Gipson and J.  Young) 
Abstract  ........... .............. ........................ ..................... ....... ... ..... ........ .... ... ...... ... ... . .. . ... ...  8-1 
Gas-Phase Chemistry  ....................... : .................................................................  8-2 
8.0 
Background  ........................................................................................................  8-3 
8.1 
Chemical Mechanisms in the CMAQ System  ...................................................  8-4 
8.2 
8.2.1  CB4 Mechanism .....................................................................................  8-5 
8.2.2 .  RADM2 Mechanism .......................................... ~ ...................................  8-6 
8.2.3  SAPRC-97 Mechanism  ..........................................................................  8-7 
8.2.4  Extended Mechanisms  ...........................................................................  8-8 
8.2.5  Changing or Adding Mechanisms in CMAQ  .. ........................ ... . .. . .....  8-11 
Reaction Kinetics  ................. · .............................................................................  8-12 
8.3.1  Reaction Rates  .....................................................................................  8-12 
8.3.2  Rate Constant Expressions ...................................................................  8-13 
8.4  Mathematical Modeling  ............... ~ ...................................................................  8-15 
8.4.1  Governing Equations  ...........................................................................  8-16 
8.4.2  SMVGEAR ..........................................................................................  8-18 
8.4.3  QSSA Solver ........................................................................................  8-22 
8.4.4  Summary  ............................................................. , ................................  8-25 
References  .............................................................................. ; .........................  8-25 
8.5 
Appendix 8A  Chemical Mechanisms Included in the CMAQ System  ; .....................  8-29 

8.3 

PLUME-IN-GRID TREATMENT OF MAJOR POINT SOURCE 
EMISSIONS 
(N.  Gillani and J.  Godowitch) 
Abstract  . . . . . . . . . . . . . . . . . . . . . . . . . .. . . . .. . . . . . .. .. . . . . . . . . . . . . . . . . .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  9-1 
9.0 
Plume-in-Grid Treatment of Major Point Source Emi~sions ..... : .......................  9-2 
Introduction ........................................................................................................  9-2 
9.1 
Overview of the Conceptual  Framework  of the  Plume-in-Grid  Treatment  ..  9-4 
9.2 
Formulation of the Plume-in-Grid Modeling Components  ...............................  9-5 
9.3 
9.3.1  Description of the Plume Dynamics Model  ...........................................  9-5 
9.3.2  Formulation of the Plume-in-Grid Module ............. ~ ............................  9-12 
Summary  ........ ................ .................. ........ ........ .. . ....... ...... .. ... ... ... ... ... ... ....... ... ..  9-28 
References  ........................................................................................................  9-28 

9 .4 
9.5 

10.  AEROSOLS IN MODELS-3  CMAQ 

11. 

12. 

(F.  Binkowski) 
Abstract  . . . . .. .. . . . .. .. . . . . . . . . . . . .. . . .. . .. .. . .. .. .... .. .. .. .. .. .. .. .. .. . . . .. .. . . .. .. . .. .. ..... . .. .. .. . .. .. .. . .. . .. . . . . . . .. .. .. ..  10-1 
10.0  Aerosols in Models-3  CMAQ 
....................................................  10-2 
10.1  Aerosol Dynamics .............. ........................ .... ............................. ...... .. .... .... .. .. ..  10-4 
10.1.1  Modal Definitions  ........................... ........... .................. ............. ...... .. .. .  10-4 
10.1.2  New Particle Production by Nucleation ...............................................  10-5 
10.1.3  Primary Emissions  ........ · .................................. : ....................................  10-6 
10.1.4  Numerical Solvers ..... · ...........................................................................  10-7 
10.1.5  Mode Merging by Renaming  .............................................................  10-10 
10.2  Aerosol Dry Deposition  ........................................................ ......... ..... ...........  10-11 
10.3  Cloud Processing of Aerosols  .. .. .......................... ............ ...... ............ ............  10-11 
10.4  Aerosol Chemistry  ............................................ ...... .............. ... .. .. ..... ...... .... .. .  10-14 
10.5  Visibility  ........................................................................................................  10-15 
10.6  Summary  ........................................................................................................  10-17 
I 0. 7  References  ................................................... : ......... ;.. .. .. .. .. .. .. .. .. .. .... . .. .. .. .. .. .. .. .  1 0-17 

CLOUD DYNAMICS AND CHEMISTRY 
(S.  Roselle and F.  Binkowski) 
Abstract  .................................................... :...................................................................  11-1. 
11.0  Cloud Dynamics and Chemistry  ......................................................................  11-2 
11.1  Background  ......................... ~ ............................................................................  11-2 
11.2  Model Description  ...........................................................................................  11-2 
11.2.1  Subgrid Convective Cloud Scheme  ...................... ; ..............................  11-3 
11.2.2  Resolved Cloud Scheme  . .. .. . .. .. .. .. .. . .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . ..  11-7 
11.3  Conclusions ......................................... ~ ............................................................  11-8 
11.4  References  ................ .... .. .. .......... ...... .. .. .... .. .. .. .... .. . . .. . .. .. .. .. .. .. . . .. . .. .. .. ... .. . . . . . . .. . . .  11-9 

METEOROLOGY~CHEMISTRY INTERFACE PROCESSOR (MCIP) FOR 
MODELS-3 COMMUNITY MULTISCALE AIR QUALITY (CMAQ) 
MODELING SYSTEM 
(D.  Byun, J.  Pleim,  R.  Tang,  and A.  Bourgeois) 
Abstract  ........................................................................................................................  12-1 
12.0  Meteorology-Chemistry Interface Processor (MCIP) For Models-3  Community 

12.1 

Multiscale Air Quality (CMAQ) Modeling System  ........................................  12-2 
Introduction ......................................................................................................  12-2 
12.1.l  MCIP Functions  ...................................................................................  12-4 
12.1.2  MCIP's Data Dependency  ...................................................................  12-6 
12.1.3  Computational Structure  .. .. .. .. .. . ... .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  12-8 
12.2  Data Types, Coordinates, and Grids  .. .. .. . .. .. .. .. . .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  12-10 
12.2.1  Meteorological Data Types  . .. .. ... . . . .. .. .. .. .. . .. .. .. .. .. .. . .. .. .. .. .. .. .. .. .. .. .. .. .. .. .  12-10 
12.2.2  Coordinates  ........................................................................................  12-14 
12.2.3  Modification of Grid Structure  ..........................................................  12-18 

xii 

12.3  Estimation of Physical Parameters ....... , .........................................................  12-23 
12.3.1  PBL Parameters  .................................................................................  12-23 
12.3.2  Dry Deposition Velocities ..................................................................  12-37 
12.3.3  Cloud Parameters and Solar Radiation  ..............................................  12-49 
12.4  Meteorological Data for CCTM with Generalized Coordinate System .........  12-52 
12.4.1  Thermodynamic Variables: Pressure, Density and Entropy  ..............  12-52 
12.4.2  Vertical Jacobian and Layer Height ...................................................  12-54 
12.4.3  Contravariant Velocity Components ..................................................  12-57 
12.4.4  Mass Consistent Temporal Interpolation of Meteorological 

Parameters ..........................................................................................  12-60 

12.4.5  Optional Conversion ofNonhydrostatic Data to Hydrostatic 

Meteorological Data for MM5  ...........................................................  12-61 
12.5  Operation ofMCIP  ............................................... ; ..............•.........................  12-63 
12.5.1  MCIP Modules  .......................................................... ; ........................  12-63 
12.5.2  Building MCIP ...................................................................................  12-63 
12.5.3  Executing MCIP ................................................................. ; ...............  12-65 
12.5.4  Defining Grid and Domain for MCIP  ................................................  12-68 
12.5.5  Extension ofMCIP for Other Meteorological Models  ................ : .....  12-72 
12.6  Concluding Remarks ......................................................................................  12-73 
12.7  References  ......................................................................................................  12-74 
Appendix 12A MCIP Output Data  ............................................................................  12-80 
Appendix 12B  Examples of Nest Domain Definitions for CMAQ system  ..............  12-83 
Appendix 12C  Sample MCIP Configuration File  .....................................................  12-85 
Appendix 12D  Sample MCIP Run Script  .................................................................  12-87 

THE INITIAL CONCENTRATION AND BOUNDARY CONDITION 
PROCESSORS 
(G.  Gipson) 
Abstract  ........ ; ..................................................................................... .-.: .......................  13-1 
13.0  The Initial Concentration and Boundary Condition Processors  ......................  13-2 
13.1 
Introduction .. ; ...................................................................................................  13-2 
13.2  Overview of the ICON and BCON Processors  ................................................  13-2 
Input Sources  ····································································································' 13-3 
13.3 
13.3.1  Time Invariant Concentration Profiles .................................................  13-3 
13.3.2  CCTM Concentration files  ....................................................................  13-5 
13.3.3  Tracer Species  ........................................................... ~ ...... ~ ...................  13-5 
13.4  Spatial Interpolation ..................................................... ; ...................................  13-7 
13.4.1  Horizontal Interpolation .......................................................................  13-7 
13.4.2  Vertical Interpolation  ...........................................................................  13-8 
13.5 
ICON/BCON Species Processing  ...................................................................  13-9 
13.6  Mechanism Conversions  ................................................................................  13-11 
13.7  .  ICON/BCON Applications  ...................................................................... ~: ....  13-12 
13.8  References  ......................................................................................................  13-13 

PHOTOLYSIS RATES FOR CMAQ 
(S.  Roselle,  K  Schere,  J.  Pleim, A.  Hanna) 
Abstract  . .. .. .. . . . .. . . . .. . . .. .. .. . . . . .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  14-1 
14.0  Photolysis Rates for CMAQ  ............................................................................  14-2 
14.1  Background  ......................................................................................................  14-2 
14.2  Preprocessor JPROC: Calculate Clear-sky Photolysis Rate Table  ..................  14-3 
14.3 
Subroutine PHOT: Table Interpolation and Cloud Attenuation  . .. . .. . . . . .. .. . .. . . . . .  14-4 
14.4  Conclusions .................... .. .......... .. .. .. .. .. .. .. .. .. .. .. .. . .. . . .. .. .. .. .. . . .. ... .. ... .. ... .. . . . . .. ... .. .  14-5 
14.5  References  ........................................................................................................  14-6 

PROGRAM CONTROL PROCESSING IN MODELS-3 
(J.  Young) 
Abstract  ........................................................................................................................  15-1 
15.0  Program Control Processing in Models-3  ........................................................  15-2 
15.1  Domain Configuration  .....................................................................................  15-3 
15.2 
Input/Output Applications Programming Interface  .........................................  15-4 
15.3  Other CCTM Configuration Control  ...............................................................  15-4 
15.3.l  CCTM Process Analysis  ......................................................................  15-4 
15.3.2  CCTM Fixed Data ................................................................................  15-5 
15.4  Generalized Chemistry .....................................................................................  15-6 
15.4.1  Design  ..................................................................................................  15-6 
15.4.2  Operation ..............................................................................................  15-7 
15.4.3  Supported Reaction Types  ...................................................................  15-9 
15.4.4  Mechanism Parsing Rules  ..................................................................  15-10 
15.4.5  Chemical Species Include Files  .........................................................  15-10 
15.6  Conclusion  .....................................................................................................  15-16 
15.7  References  ......................................................................................................  15-16 

PROCESSANALYSIS 
(G.  Gipson) 
Abstract  . . .. . . .. . .. . .. .. .. .. . . . .. .. .. . . .. .. .. .. . . . . . . .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  16-1 
16.0  Process Analysis  ..............................................................................................  16-2 
Integrated Process Rate Analysis 
..................................................................  16-3 
16.1 
16.1.1  Computation oflntegrated Process Rates  ............................................  16-3 
16.1.2  Example IPR Analyses  ........................................................................  16-5 
16.1.3  Implementation ofIPR Analysis in the CMAQ System ... .. .. . . . ... ... .. .. . .  16-6 
16.1.4  Use of the PACP to set up an IPR Analysis .........................................  16-7 
Integrated Reaction Rate Analysis  .. ... .... ..... .. .... .. .. .. .. .. .. .. ...... .. .. . .. . . .. . .. .. . .. . .. . .. ..  16-9 
16.2.1  Computation oflntegrated Reaction Rates  . .. .. . .. . . .. .. ... .. .. ... .... ... . .. . .. . . .  16-10 
16.2.2  Example IRR Analyses  .... .. .. .... ........ .. .. .. .. .. .. .. .. .. .. .. .... .. . .. .. .. . .. . . . ..... ... .  16-11 
16.2.3  Implementation ofIRR Analysis the CMAQ system  . .. ... .. .. .. . . . . .. . .. ..  16-13 
16.2.4  Use of the PACP to set up an IRR Analysis  ......................................  16-14 
16.3  Conclusion  ..................................................................... ; ..... ~ .........................  16-19 
16.4  References  .....................................................................................................  16-20 

AN AGGREGATION AND EPISODE SELECTION SCHEME DESIGNED TO 
SUPPORT MODELS-3 CMAQ 
(R.  Cohn,  B.  Eder,  and S.  Leduc) 
Abstract  . .. . . . .. . .. . . . . . .. .. .. . .. . . . .. .. . .. . . .. .. . .. . . . .. . . .. . . . . . . . .. . . . .. . . .. . .. . . . . . . . . . . . .. .. . .. . . . . . . . . . . . . . . . . . .. .. . . . . . . .  17-1 
17.0  An Aggregation and Episode Selection Scheme Designed to Support Models-3 

1 7 .1 

CMAQ .................................................................................... : .........................  17-2 
Introduction . . . .. . . . . . . . .. .. . .. . . . . . . . . . . . . . . . . . . .. . . .. . . . .. . . . . . .. .. . . . . . .. . . . . . . . . . . . . .. . .. .. . . . . . . . . . . . . .. . . . . .  17-2 
17.1.l  Background  .................... , .....................................................................  17-2 
17.1.2  Objectives  ........................................................................... ~ ................  17-3 
17.2  Summary of the Approach  ...............................................................................  17-4 
17.2.1  Basic Elements of the Methodology  ....................................................  17-4 
17.2.2  Rationale, Scope, and Limitations  .......................................................  17-5 
17.2.3  Strategy  ...................................................................................... ~ .........  17-6 
17.3  Cluster Analysis of Wind Fields  ......................................................................  17-8 
17.3.1  Description of Wind Data  ....................................................................  17-8 
17.3.2  Basic Cluster Analysis Technique  ........................................................  17-8 
17.3.3  Illustration of Cluster Analysis Results  ...............................................  17-9 
17.4  Evaluation of Alternative Aggregation Approaches  ......................................  17-11 
17.4.1  Description of Alternative Approaches ..............................................  17-11 
17.4.2  Description of Meteorological Data ...................................................  17-12 
17.4.3  Analysis Methods .................. : .......................................... : .................  17-12 
17.4.4  Results  ....................... : ........................................................................  17-13 
17.5  Refinement of the Sampling Approach ..........................................................  17-21 
17.5.1  Determination of Appropriate Numbers of Strata and Events  ...........  17-22 
17.5.2  Selection of Stratified Sample of Events  .......................................... ;  17-27 
17.6  Application and Evaluation ............................................. : .. ···"····~~·····'.··· ........  17-29 
17 .6.1  Application of the Aggregation Procedure  ... ............. ... ............... ... ...  17-29 
17.6.2  Evaluation  ..........................................................................................  17-31 
17.7  Summary and Discussion ............................................................. ; .................  17-32 
17.8  References  ......................................................................................................  17-33 

18. 

INTEGRATION OF SCIENCE CODES INTO MODELS-3 
(J.  Young) 
Abstract  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ... . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  18-1 
Integration of Science Codes into Models-3  ............ ~ ........................................  18-2 
18.0 
18.l 
Introduction ......................................................................................................  18-2 
18.2  Classes and Modules  .................... ; ...................................................................  18-4 
18.2.1  Operational Design  ..............................................................................  18-4 
18.2.2  CGRID  .................................................................................................  18-5 
18.2.3  Class Driver  .........................................................................................  18-6 
18.2.4  Synchronization Time Step  ............................................... , ..................  18-7 
Input/Output Applications Programming Interface  .. .. .. .............. ....... ...... .. .. ....  18-8 
18.3 
18.4  Code Configuration Management .................. .... .... .. .... .. ............ ............ ........  18-10 
18.4.1  The Need  ............................................................................................  18-10 
18.4.2  The Tool .............. ....................... ........... .... .. .. .. .. .. . .. .. .. . .............. .........  18-11 
18.4.3  The Repository ...................................................................................  18~12 
18.5  How a Model is Constructed ..........................................................................  18-14 
18.5.1  Object Oriented Concepts  ...................................... : ...........................  18-14 
18.5.2  Global Name Table Data ....................................................................  18-14 
18.5.3  Build Template ..................................................... · ..............................  18-15 
18.6  How a Model is Executed  ................................................ ........ ............ .... .. ....  18-18 
18.7  Using the Models-3 Framework  ....................................................................  18-18 
18.8  Conformant Code .............. ....................................... ............ ... .. ..... .. . .. .. . ... .. . ..  18-19 
18.8.1  Thin Interface .....................................................................................  18-19 
18.8.2  Coding Rules .................................................... ~ .................................  18-20 
18.8.3  Science Process Code Template  ........................................................  18-21 
18.8.4  Robustness and Computational Efficiency  ........................................  18-24 
18.9  Conclusion  ............................. , .......................................................................  18-25 
18. l 0  References  ......................................................................................................  18-25 

ADEOS 
ADOM 
AQM 
ASD 
AVS 
BCs 
BCON 
BDF 
BEIS 
BEIS2 
BELD 
BOT 
CAAA-90 
CB-IV (or CB-4) 
CBWQM 
CCM2 
CCTM 
CEM 
CFL 
CG 
CMAQ 
CO RB A 
CPU 
CTM 
CVS 
ECIP 
EMPRO 
EPA 
FAST 
FDDA 
FG 
FIPS 
FSL 
GCIP 
GEMAP 
HSPF 
I/O API 
ICs  -
ICON 
IDA 
IEEE 

ACRONYMS 

· 

Advanced Earth Observing Satellite 
Acid Deposition and Oxidant Model 
Air Quality Model 
Accurate Space Derivative advectlon scheme 
Advanced Visualization System 
Boundary Conditions 
Boundary Conditions processor 
Backward Differentiation Formulae 
Biogenic Emissions Inventory System 
Biogenic Emissions Invenfory "System ~ version 2  · 
Biogenic Emissions Lailduse Database 
Bott Scheme for advection 
Clean Air Act Amendments of 1990 
Carbon Bond-IV chemical mechanism 
Chesapeake Bay Water Quality Model 
Community Climate Model Version 2 
CMAQ Chemical Transport Model processor 
Continuous Emission Monitor 
Courant-Friedrich-Levy condition 
Coarser Grid 
Community Multiscale Air Quality model 
Common Object Request Broker Architecture 
Central Processing Unit of a computer 
Chemical Transport Model 
Concurrent Versions System 
Emissions-Chemistry Interface Processor 
Emissions Processor 
Environmental Protection Agency 
Flow Analysis Software Toolkit 
Four-Dimensional Data-Assimilation 
Finer Grid  . 
Federal Identification Protocol System 
Forecast Systems Laboratory 
GEW AX Continental Scale International Project 
Geocoded Emission Modeling and Projection 
Hydrologic Simulation Program FORTRAN 
Input/Output Applications Programming Interface 
Initial Concentration 
Initial Conditions processor 
Inventory Data Analyzer 
Institute of Electrical and Electronics Engineers, Inc. 


INPRO 
IPR 
IRR 
JPROC 
LRPM 
LUPROC 
MAPS/RUC 
MCIP 
MEPPS 
MEPRO 
MEPSE 
MBPS Es 
MM5 
MPS 
MRF 
NAAQS 
NARSTO-NE 
NASA 
NCAR 
NCEP 
NOAA 
NO" 
NQS 
NSSDC 
03 
ODE 
OODBMS 
OUTPRO 
PACP 
PAN 
PARTS 
PAVE 
PBL 
PCP 
PDM 
PinG 
PM 
PMIO 
PM2.5 
PPM 
PSU 
QSSA 
RADM 

MEPPS Input Processor 
Integrated Process Rates 
Integrated Reaction Rates 
Photolysis Rate Processor 
LaGrangian Reaction Plume Module 
Landuse processor for MCIP 
Mesoscale Analysis and Prediction System/Rapid Update Cycle 
Meteorology-Chemistry Interface processor 
Models-3 Emissions Processing and Projection System 
Models-3 Emissions Projection Processor 
Major Elevated Point Source Emitter 
Major Elevated Point Source Emitters 
Fifth Generation Mesoscale Model 
Multiple Projection System 
Medium Range Forecast 
National Ambient Air Quality Standards 
North American Research Strategy for Tropospheric Ozone - Northeast 
National Aeronautics and Space Administration 
National Center for Atmospheric Research 
National Centers for Environmental Prediction 
National Oceanic and Atmospheric Administration 
Oxides of Nitrogen 
Network Queuing Service 
National Satellite Service Data Center 
Ozone 
Ordinary Differential Equations 
Object Oriented Data Base Management System 
MEPPS Output Processor 
Process Anaylsis Control Program 
Peroxy Acetyl Nitrate 
Mobile Source Particulate Model 5 
Package for Analysis and Visualization of Environmental Data 
Planetary Boundary Layer 
Program Control Processor 
Plume Dynamics Model 
Plume-in-Grid 
Particulate Matter 
Particulate Matter less than 10 µm in diameter 
Particulate Matter less than 2.5 µm in diameter 
Piecewise-Parabolic Method advective scheme 
Pennsylvania State University 
Quasi-Steady State Approximation method 
Regional Acid Deposition Model 

xvm 


RADM2 
RCS 
REL MAP 
RMSE 
ROG 
ROM 
RPM 
SAPRC 
SAQM 
SAS 
sec 
SCCS 
SIP 
SMO 
SMOKE 
SMVGEAR 
TOG 
SOX 
STEM 
svoc 
TKE 
TOMS 
UAM 
UCAR 
UTC 
VMT 
voe 
WRF 
XDR 
YAM 

Regional Acid Deposition Model Version 2 
Revision Control System 
Regional Lagrangian Modeling of Air Pollution 
Root Mean Square Error 
Reactive Organic Gases 
Regional Oxidant Model 
Regional Particulate Model 
State Air Pollution Research Center chemical mechanisms 
SARMAP Air Quality Model 
Statistical Analysis System 
Source Classification Code 
Source Code Control System 
State Implementation Plan 
Smolarkiewicz advection scheme 
Sparse Matrix Operator Kernel Emissions system 
Sparse Matrix Vectorized Gear algorithm 
Total Organic Gases 
Oxides of Sulfur 
Sulfur Transport and Emissions Model 
Semi-Volatile Organic Compounds 
Turbulent Kinetic Energy 
Total Ozone Mapping Spectrometer 
Urban Airshed Model 
University Corporation for Atmospheric Research 
Universal Time Coordinate 
Vehicle Miles Traveled 
Volatile Organic Compound 
Weather Research and Forecasting 
eXternal Data Representation 
Yamartino-Blackman Cubic Scheme for advection 

ACKNOWLEDGMENTS 

The development of the science components of the Models-3 Community Multiscale Air Quality 
(CMAQ) system represents a major undertaking by a large team of dedicated atmospheric 
scientists and a relatively long term effort beginning in the early 1990s, and covers a wide area of 
subject material as embodied in each of the contributing chapters.  Most of the EPA Models-3 
CMAQ Science Team are from the Atmospheric Modeling Division (AMD) in the EPA Office of 
Research And Development's National Exposure Research Laboratory (NERL). With several 
exceptions the contributing authors from this Division are on detail from the Air Resources 
Laboratory of the National Oceanic and Atmospheric Administration (NOAA).  The NOAA(cid:173)
AMD effort is supported through Interagency Agreement (DW13937252) with the EPA.  The 
authors of each of the chapters of this document would like to collectively express their gratitude 
to the other members of the NOAA Division.  In particular we acknowledge the encouragement 
and support of the Director of the  Atmospheric Modeling Division, Francis Schiermeier.  We are 
indebted to Dr. Robin Dennis for his vision and role in ensuring the implementation ofholistic(cid:173)
one-atmosphere approach into CMAQ.  We also recognize the technical help from our colleagues 
Tom Pierce, Steven Howard, Alfreida Torian, and Gary 'Walter. 

We also want to recognize the contributions, helpful discussions, and assistance of many members 
of the science community, either through formal agreements or through peer interest.  For 
example, the work was supported by several cooperative agreements including: 
• 

Atmospheric Modeling Research-Scientific and Computational (CR-822066: MCNC), 
Principal Investigator (PI): Kenneth Galluppi Ed Bilicki, Steve Fine, Alison Eyth, and 
Rohit Mathur; 
Research on Computational Framework in Generic Grids, Adaptive Grids, and Subgrid 
Treatment of Air Quality Simulation (CR-822053: MCNC), PI: Talat Odman; with R.K. 
Srivastava and D.S. McRae, North Carolina State University); 
Transport Algorithms for Air Quality Simulation Models  (CR-822059, MCNC), PI: Talat 
Odman; 
Emissions Modeling Research with High-Performance Computing (CR-822074: MCNC), 
PI: Carlie Coats; 
Advanced Modeling of Meteorology in Support of Air Quality Models (CR-822628: 
(MCNC), PI: Aijun Xiu, Kiran Alapaty, John N. McHenry, and Adel F.  Hanna; with 
Nelson L. Seaman and Aijun Deng, Pennsylvania State University; and John S. Kain, 
National Severe Storms Laboratory); 
A Flexible and Efficient Methodology for Modeling Aerosol for Air Quality Models 
(CR-823634: MCNC), PI: Uma Shankar, Mark Read and Atanas Trayanov; with Anthony 
Wexler, University of Delaware; and John H.  Seinfeld, California Institute of Technology; 
Develop Methods for Technology Transfer of Advanced Regional/Urban Air Quality 
Models (CR-822080: NCSU),  PI: Alan Schula; and 
Plume-in-Grid development for a Multiscale Air Quality Modeling System (IAG 
DW64937190, Tennessee Valley Authority (TV A)), PI's: Robert E. Imhoff;  with Noor 
Gillani, Arastoo Biazar (now at Monash University, Australia), and Yu-Ling Lu, 

• 

• 

• 

• 

• 

• 

• 

XXl 

University of Alabama, Huntsville). 

The following scientists also contributed under the formal contracts listed: Ruen Tang, Chris 
Maxwell, and Hao Jin of the Technical Support Group, Dyntel Corp.  (General Services 
Administration JAG  DW47937823); Tod Plessel, and Yan Ching Zhang of the Visualization 
Laboratory, Lockheed Martin (EPA Contract 68-W7-0055); and Nick Moghari, Joe Susick, and 
Dave Tivel of Science Applications International Corporation (SAIC) (EPA Contract 68-Wl-
0055). 

Significant contributions were made by: visiting scientists, including Sang-Mi Lee of the Seoul 
National University, Korea, Chong Bum Lee of the KangWeon University, Korea, and Seiji 
Sugata of the National Institute for Environmental Studies, Japan; UCAR's post doctoral fellows 
Yonghong Li, Qingyuan Song and Shoba Kondragunta; and Dr. Ingmar Akermann of Ford 
Research in Aachen, Germany. 

It is also a pleasure to thank the colleagues who engaged us in many seminal technical discussions 
on various aspects of the CMAQ system. We recognize Professors Harvey Jeffries, University of 
North Carolina at Chapel Hill; Ted Russell, Georgia Institute of Technology; Nelson Seaman and 
Jack Kain, Pennsylvania State University; Dick McNider, University of Alabama, Sonia 
Kreidenweis, Colorado State University, Panos Georgeopolus of Rutgers University, and Itsushi 
Uno of Kyushu University, Japan.  Many of our EPA colleagues provided stimulating discussion 
or reviews of our effort including Gail Tonnesen, Deborah Luecken, Carey Jang, John S. Irwin 
(NOAA) and Ed Edney. 

We are indebted to the following peer reviewers, who gave unselfishly of their time and whose 
comments and suggestions were extremely valuable in improving the scientific aspects of the 
report.  Listed in alphabetical order, the reviewers are followed by the chapter(s) they reviewed: 
Jeff Brook, Atmospheric Environment Service, Canada (Ch.  17);  David Chock, Ford Research 
Laboratory (Ch. 2 and 7);  Henry Hogo, Southern California Air Quality Modeling Division (Ch. 
8 and 16);  Sasha Madronich, National Center for Atmospheric Research (Ch.  14);  Paulette 
Middleton, RAND Environmental Science &  Policy Center (Ch.  10);  Ted Russell, Georgia 
Institute of Technology (Ch.  18);  Nelson Seaman, Pennsylvania State University (Ch. 3); 
Christian Seigneur, Atmospheric and Environmental Research, Inc. (Ch.  10 and 11 );  Trevor 
Sholtz, ORTECH Corporation (Ch. 4); Saffet Tanrikulu, California Environmental Protection 
Agency (Ch. 9 and 12);  Robert Yarmartino, Earth Tech (Ch. 5 and 6); and Zion Wang (Ch.  13, 
15, and 16). 

Finally, the editors note with great appreciation, the efforts of Brian Eder, who facilitated many of 
the activities and tasks involved in the preparation and completion of this document.  We thank 
the Raleigh, North Carolina, staff of SAIC, especially Alice Gilliland (now with NOAA-AMD) 
and Andrea Verykoukis for their critical contributions in the area of technical editing, assuring a 
high level of consistency and quality in all the chapters of this manuscript.  We are also grateful to 
Alice Gilliland for her invaluable work on the Executive Summary. 

xxii 

EXECUTIVE SUMMARY 

EP A/600fR-99{030 

THIS SCIENCE DOCUMENT PRESENTS THE PROCESSORS AND ALGORITHMS THAT 
EMBODY THE INITIAL RELEASE OF THE MODELS-3 COMMUNITY MULTISCALE 
AIR QUALITY (CMAQ) MODELING SYSTEM.  ClVIAQ IS A MULTIPLE POLLUTANT 
MODEL THAT CONTAINS NEW SCIENTIFIC APPROACHES TO AIR QUALITY 
MODELING, WIDCH REPRESENT THE CURRENT STATE OF SCIENCE.  This CMAQ 
Science Document is a living document that will be updated as the state of the science progresses. 
The CMAQ Science Document provides a basis and point of reference for the state of the science 
captured in the June1998 initial :release of Models-3.  Current and future efforts to improve the 
Models-3 modeling system(s) will depart from the scientific reference points presented in this 
document. 

Models-3 is a flexible software system that provides a user-interface framework for CMAQ air 
quality modeling applications and tools for analysis, management of model input/output, and 
visualization of data.  The Models:..3  framework relies on two  modeling systems to provide the 
meteorological and emissions data needed for air quality modeling.  With this data, the Models-3 
CMAQ modeling system can be used for urban and regional scale air quality simulation of 
tropospheric ozone, acid deposition, visibility; and particulate matter (PM2.5  and PM10).  The 
meteorological and emissions modeling systems that are provided with the current release of 
Models-3 will be described in this document.  However, CMAQ is designed as an open system 
where alternative models can be used to generate the data. 

This CMAQ Science Document contains chapters that address specific scientific and technical 
issues involved in the development and application of the Models-3 CMAQ modeling system. 
The principal researchers for each model component or fimction authored the· coinciding chapter 
in this document.  They serve as the points of contact for scientific questions regarding their 
CMAQ air quality model components.  For instructions on using the Models-3 framework and 
using the MMS, MEPP·s, and CMAQ modeling systems, refer to the Models-3  User Manual 
(EPA/600/R-98/069b, National Exposure Research Laboratory, Research Triangle Park, NC) and 
Tutorial (EP A/600/R-98/069c, National Exposure Research Laboratory, Research Triangle Park, 
NC). 

. 

An overview of the MEPPS emissions, MMS meteorology, and CMAQ air quality modeling 
systems is provided in Chapter 1.  Chapter 2 then introduces the Models-3 framework and 
structure and explains how the framework's user-interface is used with the MEPPS and CMAQ 
modeling systems.  More detailed discussions ·on the modeling systems are then discussed 
separately in the following chapters.  The amount of detail and the length of these discussions 
vary depending on whether this information has already been provided elsewhere.  Some chapters 
provide a synopsis of the scientific components and refer to· previously published material on the 
subject, while other chapters provide extensive detail on new scientific techniques that are not 
currently described in other publications. 

MM5  Meteorological data are essential for many processes simulated in the CMAQ chemical 
transport model including transport, chemistry, and cloud processes.  The Fifth-Generation Penn 
State/NCAR Mesoscale Model (MMS) is the only meteorology model compatible with the initial 
release ofModels-3.  MMS is a complex, state-of-the-science community model, and it is 
maintained by NCAR..  MMS is well documented by its primary developers in technical notes and 
refereed journal articles.  Chapter 3 briefly describes the scientific aspects ofMMS, including grid 
definitions, model physics, nesting, and four dimensional data assimilation.  These descriptions 
generally direct the user to more complete documentation about particular aspects ofMMS.  To 
promote the flexibility of CMAQ, additional meteorology models will be compatible with Models-
3 future releases. 

MEPPS 
Chapter 4 provides a description of the Models-3 Emission Processing and 
Projection System (MEPPS) structure, its scientific approach, and the assumptions used in 
modeling and processing emission data in the Models-3 framework.  The chapter also discusses 
data flow and quality control used with emission inventory and meteorological input data for 
MEPPS.  The description of the main Emission Processor addresses the basis of spatial and 
temporal allocation procedures, and the methods and assumptions used in modeling mobile and 
biogenic emissions and in the "lumping" of individual chemical species are also presented.  This 
chapter also explains the procedures used by the Models-3 Emission Projection Processor to 
estimate emission data for use in modeling future air quality scenarios. 

THE CMAQ CHEMICAL TRANSPORT MODEL (CCTM) 

Fundamentals of One-Atmosphere Dynamics for Multiscale Air Quality Modeling 
Chapter 5 provides information essential to the proper use of meteorological data in air 
quality modeling systems.  The chapter introduces a robust and fully compressible set of 
governing equations for the atmosphere, which provides an integral view of atmospheric 
modeling.  The limitations of several simplifying assumptions on atmospheric dynamics are 
presented, as are concepts of on-line and off-line coupling of meteorological and air 
quality models.  In addition, this chapter describes a procedure for conserving the mixing 
ratio of trace species even in the case of meteorological data that are not mass consistent. 
In summary, Chapter 5 attempts to bridge the information gap between dynamic 
meteorologists and air quality modelers by highlighting the implication of using different 
meteorological coordinates and dynamic assumptions for air quality simulations. 

Governing Equations and Computational Structure 
diffusion equation is derived in a generalized coordinate system, which is suitable for 
multiscale atmospheric applications.  CMAQ's use of generalized coordinates for its 
governing equations provides the flexibility to span multiple scales and to incorporate 
meteorological data on different coordinates.  The CMAQ system's modularity concepts 
and fractional time-step formulation, and CCTM' s key science processes are described. 
Chapter 6 also presents the dynamic formulations of several popular Eulerian air quality 

In Chapter 6, the governing models as emulated by the governing diffusion equations in the generalized coordinate system. 

Numerical Transport Algorithms  The transport processes in the atmosphere primarily 
consist of advection and diffusion.  In Chapter 7, CMAQ's numerical algorithms for 
advection and vertical and horizontal diffusion are discussed.  To provide the CMAQ 
system with multiscale capability, the transport processes, both advection and diffusion, 
are formulated in conservation (i.e., flux) forms for the generalized coordinate system. 
Therefore, CMAQ's numerical transport algorithms will function under a wide variety of 
dynamical situations and concentration distribution characteristics.  Users are encouraged 
to experiment with their own algorithms to test different numerical schemes for air quality 
simulations. 

Chapter 8 examines the way gas-phase chemistry is treated 

Gas Phase Chemistry 
in CMAQ.  The CMAQ system currently includes two base chemical mechanisms, 
RADM2 and CB4, while the incorporation of a third, the SAPRC97 mechanism, is 
planned for the future.  Chapter 8 describes each of these chemical mechanisms as well as 
the manner in which the first two are linked to the aqueous chemistry and aerosol 
formation processes.  The chapter also discusses procedures for entering new chemical 
mechanisms in the CMAQ system, the representation of reaction kinetics, the numerical 
modeling of gas-phase chemistry, and the two nurnerical solvers included in CMAQ, 
· 
SMVGEAR and a variant of the QSSA method. 

Chapter 9 introduces the plume-in-grid (PinG) technique developed 

Plume-in-Grid 
for CMA.Q.  PinG is designed to treat more realistically the dynamic and chemical 
processes impacting selected major point source pollutant plumes in CMAQ.  The Plume 
Dynamics Model (PDM) simulates plume rise, horizontal and vertical plume growth, and 
transport of each plume section during the subgricl scale phase.  The PinG module 
simulates the relevant physical and chemical processes during a subgrid scale phase.  This 
technique is in contrast to the traditional Eulerian grid modeling method of instantly 
mixing the point source emissions into an entire grid cell volume.  Chapter 9 describes the 
technical approach and model formulation of the relevant processes, and discusses the 
capabilities and limitations of the initial version of the PinG approach. 

The Aerosol Module One of CMAQ's key strengths is that it is a multi-pollutant model 
that fully addresses the criteria pollutants P.M and ozone.  Chapter 10 discusses the 
aerosol module of CMAQ, which is designed to be an efficient and economical depiction 
of aerosol dynamics in the atmosphere.  This chapter discusses the techniques for 
distributing particulates in three modes: coagulation, particle growth by the addition of 
new mass, and particle formation.  The aerosol module considers both PM2.5 and PM10  and 
includes estimates of the primary emissions of elemental and organic carbon, dust, and 
other species not fi.Irther specified.  Secondary species considered are sulfate, nitrate, 
ammonium, water and organic from precursors of anthropogenic and biogenic origin. 

Cloud Chemistry and Dynamics  Chapter 11  discusses the role and functions of 
clouds in CMAQ.  Clouds are involved in aqueous chemical reactions, vertical mixing of 
pollutants, and removal of aerosols by wet deposition, all of which affect the concentration 
of air pollutants.  CMAQ's cloud module performs several functions related to cloud 
physics and chemistry, and it models three types of clouds: sub-grid convective 
precipitating clouds, sub-grid non-precipitating clouds, and grid-resolved clouds.  The 
cloud module vertically redistributes pollutants for the sub-grid clouds, calculates in-cloud 
and precipitation scavenging, performs aqueous chemistry, and accumulates wet 
deposition amounts. 

CMAQ INTERFACE PROCESSORS PREPARE INPUT DATA FROM SOURCES 
INCLUDING THE EMISSION AND METEOROLOGICAL MODELING SYSTEMS FOR USE 
IN THE CMAQ CTM.  EACH OF THESE PROCESSORS, EACH OF WHICH HA VE 
SPECIFIC FUNCTIONS, ARE DESCRIBED IN THE CHAPTERS MENTIONED BELOW.  The 
interface processors that handle input data from the emissions and meteorological models are 
essential because CMAQ is an open system in which meteorological and emissions data are 
calculated separately (i.e., "off-line"), rather than during the chemical transport model simulation. 
These interface processors also add extra quality control, so that inconsistencies between input 
data and the CCTM are minimized. 

ECIP  In addition to describing the Models-3 MEPPS emission modeling system, Chapter 
4 discusses the Emission-Chemistry Interface Processor (ECIP).  ECIP serves as the key 
link between the MEPPS system and CCTM.  ECIP's primary function is to generate 
hourly 3-D emission data files for CCTM from the individual emission file types produced 
by the MEPPS.  The key inputs for ECIP are the area emissions file, the stack parameter 
and emission files for the point sources generated in MEPPS, and a set of meteorological 
data files generated by the Meteorology-Chemistry Interface Processor (MCIP) for the 
CCTM domain.  All major point sources are subject to plume-rise and initial vertical 
dispersion processes before being allocated to a particular vertical model layer. 

MCIP Chapter 12 describes MCIP, which links meteorological models, such as MM5, 
with the CCTM system to provide the complete set of meteorological data needed for air 
quality simulation.  To support CCTM's multiscale generalized coordinate 
implementation, MCIP provides appropriate dynamic meteorological parameters to allow 
mass-consistent air quality computations.  MCIP deals with issues related to data format 
translation, conversion of parameter units, diagnostic estimations of parameters not 
provided, extraction of data for appropriate window domains, and reconstruction of 
meteorological data on different grid and layer structures.  MCIP also relies on the 
Landuse Processor (LUPROC) to provide landuse and vegetation information to define 
surface characteristics to compute dry deposition and other PBL parameters.  LUPROC 
extracts information about landuse in the CMAQ domain from a landuse database and 
converts it into the fractional landuse data used in MCIP. 

Initial conditions provide a simulation's starting 

Initial and Boundary Conditions 
point, while boundary conditions define influences from outside the domain. 
Chapter 13  describes the two interface processors that generate the concentration fields 
for the initial and boundary conditions for CCTM.  The chapter describes how the initial 
condition (ICON) and boundary condition (BCON) processors can be used to generate 
the concentration fields from either predefined default vertical profiles or from other 
CMAQ simulation results when model nesting is being performed.  This chapter also 
discusses generating initial and boundary concentrations for special tracer species and 
procedures for horizontal and vertical interpolation and conversions between chemical 
mechanisms. 

Photolysis Rate Processor  Many chemical reactions in the atmosphere are initiated by 
the photodissociation of numerous trace gases, including N02,  0 3,  and HCHO.  Chapter 
14 describes the photolysis rate processor (JPROC) that produces the photolysis rates 
used in the CMAQ chemical transport simulation.  JPROC predicts photolysis rates for 
various altitudes, latitudes, and zenith angles. Currently, the radiative transfer algorithm 
assumes clear-sky conditions (i.e., no clouds present), and CCTM then attenuates for 
cloudiness. 

As described above, each of the CMAQ interface processors incorporate raw data for CMAQ and 
perform functions such as calculating parameters and inte1polating or converting data.  The 
functions of the interface processors also include capabilities to handle raw data with various 
resolutions or measurement units.  Raw input data is currently specified in the source code for 
JPROC, LUPROC, ICON, and BCON; however, the interface processors in future releases of 
CMAQ will be modified to handle a more generalized set of input data. 

PROGRAM CONTROL PROCESSORS, A SET OF PROGRAMS EMBEDDED IN THE 
MODELS-3 FRAMEWORK, HANDLE SCIENCE INFOJRMATION OBJECTS SUCH AS 
GRID AND LAYER SPECIFICATIONS, CHEMICAL MECHANISMS, AND MODEL 
CONFIGURATIONS FOR REPEATED USE ACROSS SEVERAL PROCESS COMPONENTS 
OFCMAQ. 

Program Control Processing (PCP)Chapter 15 explains how PCP is used within 
Models-3 to set up internal arrays, map species names, define global parameters, and 
establish linkages among processors in the Models-3 CMAQ system.  Specifications 
needed for the CCTM simulation (e.g., grid and coordinate conditions and chemical 
species names) are entered into the Models-3 system once by the graphical user interfaces, 
and an object-oriented database accessible by all model components is established.  PCP 
utilizes this information in the object database and automatically generates the required 
global FORTRAN include files.  As a part of PCP, Models-3 CMAQ system employs a 
generalized chemistry mechanism processor (MP), also called the "mechanism reader."  It 
greatly simplifies the task of implementing or altering gas-phase chemistry mechanisms 
and provides the capability of easily and safely using different mechanisms in the CMAQ 
system. 

Integration of Science Codes into Models-3 
One of the major objectives of the 
Models-3 project was to develop a flexible, comprehensive air quality modeling system 
with a modular coding structure that allows easy replacement of science process 
components.  Chapter 18 describes the modularity concepts, code management method, 
and integration schemes of CMAQ science code with the Models-3 framework.  The 
CMAQ FORTRAN code was integrated into the Models-3 framework with the following 
set of design, coding, and implementation standards: (1) modularity to allow easy 
exchange of science process solvers, (2) a standard subroutine interface at the module 
level, (3) restriction of coding practices, ( 4) the Models-3 I/O API 
(http://www.iceis.mcnc.org/EDSS/ioapi/index.html/), which contains standardized file I/O 
functions and a modeler-friendly interface built on top of self-describing netCDF 
(http://www.unidata.ucar.edu/packages/netcdf/) files that are portable across most Unix 
platforms. 

MODELS-3 ALSO PROVIDES ANALYSIS ROUTINES FOR USE WITH CMAQ OUTPUT, 
WHICH CAN BE USED TO PROVIDE PROCESS ANALYSIS RESULTS AND STATISTICAL 
AGGREGATION TECHNIQUES. 

Chapter 16 describes the implementation of process analysis 

Process Analysis 
techniques in the CMAQ modeling system.  These techniques can be used in CMAQ to 
provide insights into how model predictions are obtained, which is particularly useful 
when modeling nonlinear systems like atmospheric photochemistry.  Two techniques are 
available in the CMAQ system, integrated process rate (IPR) analysis and integrated 
reaction rate (IRR) analysis.  IPR analysis can be used to determine the relative 
contributions of individual physical and chemical processes, and IRR analysis can help 
elucidate important chemical pathways and identify key chemical characteristics. 

Aggregation  Chapter 17 discusses a statistical procedure called aggregation that is 
applied to CMAQ's outputs in order to derive the seasonal and annual estimates required 
by assessment studies.  Assessment studies require CMAQ-based distributional estimates 
of ozone, acidic deposition, and PM2.5,  as well as visibility, on seasonal and annual time 
frames.  Unfortunately, it is not financially feasible to execute CMAQ over such extended 
time periods.  Therefore, in practice CMAQ must be executed for a finite number of 
episodes or "events," which are selected to represent a variety of meteorological classes. 
The aggregation technique is used to incorporate these episode simulations into annual 
and seasonal estimates. 

THE MODEL-3 CMAQ MODELING SYSTEM IS BEING FORMALLY EVALUATED TO 
ASSESS THE PERFORMANCE OF CMAQ'S NEW DEVELOPMENTS IN AIR QUALITY 
MODELING.  THE EVALUATION WILL PROVIDE THE BASIS FOR UNDERSTANDING 
THE STRENGms OR WEAKNESSES OF THE CURRENT STATE-OF-SCIENCE IN 
CMAQ.  With an evaluation ofCMAQ simulations of36, 12, and 4 km grid resolution, 
CMAQ's performance can be evaluated on both the regional and urban scales.  This evaluation 
will include an initial comparison of relative performance against the RADM model and diagnostic 
evaluation against databases from regional studies such as the .1995  Southern Oxidant Study 
conducted in the vicinity of Nashville, TN and the 1995 NARSTO-NE study. 

CMAQ can be configured for a wide range of applications, from scientific studies to regulatory 
applications.  While the scientific community can take advantage of CMAQ's ability to create 
alternative applications for research and development purposes, regulatory applications depend 
upon a standardized, evaluated form of CMAQ for regulatory applications.  The CMAQ 
evaluation program will provide the scientific benchmark needed for this. 

FUTURE EXTENSIONS OF CMAQ INCLUDE NEAR-TERM EFFORTS TO PROVIDE A 
NEW CHEMICAL MECHANISM AND EMISSION MODELING SYSTEM. 

The SAPRC-97 gas phase mechanism will soon be incorporated into CMAQ, in 
addition to the current CB-IV and RADM2 mechanisms available.  The SAPRC 
mechanism will be incorporated with a fixed subset of the approximately 100 organic 
species contained in the semi-explicit version of the SAPRC mechanism. 

The Sparse Matrix Operator Kernel Emissions modeling system (SMOKE) 
(http://envpro.ncsc.org/products/) will also be incorporated into CMAQ in the near-term. 
The SMOKE model formulates emissions modeling in terms of sparse matrix operations 
which require considerably less time to perform than current systems. 

WE ENCOURAGE THE FULL PARTICIPATION AND INVOLVEMENT OF  THE 
SCIENTIFIC AND MODELING COMMUNITIES IN THE GROWTH AND USE OF MODELS-
3 CMAQ.  THERE ARE MANY IDEAS AND PLANS FOR FUTURE DEVELOPMENTS OF 
CMAQ, INCLUDING TOXIC POLLUTANT MODELING AND LINKAGES TO OTHER 
MODELS. 

Modeling atmospheric toxic pollutants  A key opportunity for CMAQ is developing the 
capability to model toxic pollutants.  Models of airborne toxic pollutants are essential for 
human exposure and risk assessments.  They can also be used to assess the exchange of 
toxic compounds between the atmosphere and sensitive ecosystems.  With the ability to 
simulate toxic pollutant processes in addition to the current photochemical oxidants and 
particulates, it is planned to transport the CMAQ model to a finer than urban scale to link 
with human exposure models. 

New linkages with global models  It is hoped that information from the urban and 
regional CMAQ applications and from global modeling applications can be bridged. 
CMAQ output, produced using state of the science techniques, can be used to benchmark 
or examine the parametric basis of process formulations in global models.  In addition, 
global model output can be used to improve or enhance the initial and boundary conditions 
for regional and urban scale CMAQ simulations. 

Modeling ecosystems  Efforts to combine environmental modeling techniques to 
encompass an entire ecosystem is needed to address issues including  (a) nutrient cycling 
through the atmosphere, water bodies, and soil and (b) acidic wet and dry deposition into 
sensitive ecosystems, including critical load analyses.  With this ecosystem modeling 
approach, air quality issues can be studied in combination with other aspects of 
environmental health. 

This is the Executive Summary of Science Algorithms of the EPA Models-3 Community 
Multiscale Air Quality (CMAQ) Modeling System, edited by D. W. Byun and J. K. S. 
Ching, 1999. 
