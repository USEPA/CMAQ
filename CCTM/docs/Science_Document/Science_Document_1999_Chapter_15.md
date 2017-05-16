Chapter 15 
===================

PROGRAM CONTROL PROCESSING IN MODELS-3 

EPA/600/R-991030 

Jeffrey Young' 

Atmospheric Modeling Division 

National Exposure Research Laboratory 
U.S.  Environmental Protection Agency 

Research Triangle Park, NC 2771  l 

ABSTRACT 

Based on user choices and input, the Models-3  framework creates objects and include files that 
control the complete problem domain under which a CMAQ model is built and executed.  The 
general concepts underlying the definition of these objects and the creation of the include files 
are discussed. 

'On assignment from  the National Oceanic and Atmospheric Administration, U.S.  Department of Commerce. 
Corresponding author address: Jeffrey Young, MD-80, Research Triangle Park, NC 27711.  E-mail: 
yoj@hpcc.epa.gov 

1
'111 
1

'1111111 

EPA/600/R-99/030 

'llllllluill, 

...  is':o 

i>11oGR.AM coN'TRoL PRocEssING IN Mon'ELs-3 , 

,;:1111111111 

"I 

"111 

Program control processing (PCP) refers to setting up internal arrays and mappings, global 
parameters, and data linkages to establish the complete problem domain in which the programs, 
of "models," in the Community Multiscale Air Quality (CMAQ) system are built and executed. 
Controlled by user choices, the Models-3 framework carries out this processing automatically 
ai;!,~ ~rovides the ~~y to modu!~ity and ensures uniform, consistent internal dimensioning, 
looping parameters and solver data in the compiled codes for all the CMAQ system models. 
Thus, modularity and consistency in establishing the computational domain and chemical 
mechanism for any particular application of the CMAQ system are assured by PCP. Modelers 
can easily and relatively safely carry out many different applications at different scales, or with 
different chemistry, or different solvers.  Model developers can readily study the effects of 
different schemes and determine optimal implementations of the science and codes.  PCP helps 
to establish a true "one-atmosphere" approach to modeling. 

"""""""""'" 

1111111111,,," 

'"' 

PCP involves both the Models-3 framework's graphical user interface and specialized processors 
that are launched from the framework.  In the Models-3  framework, a user selects the 
computational grid and domain characteristics, the vertical layer structure, and the chemical 
mechanism to be used in building (compiling and linking) a model (an executable).  How these 
choices get transformed automatically into compiled code is discussed in the following sections. 
This chapter focuses on the CMAQ Chemical Transport Model (CCTM) since the CCTM 
requires the most extensive use of PCP.  However, some or all of the user choices made to set up 
a particular CCTM determine the particular application characteristics for all of the supporting 
processors (or "models").  In addition to the CCTM, CMAQ is comprised of eight additional 
processors as follows: 

• 

• 

• 

• 

• 

• 

• 

• 

The Models-3 emissions processing and projection system (MEPPS), described in 
Chapter (  ···· 

· 

The emissigns-ch~mistry interface processor (ECIP), described in Chapter 4. 

The pfiJme:in-gricfdynarriics processor (PDM), described in Chapter 9. 

the meteorology-chemistry input processor (MCIP), described in Chapter 12. 

The land-use processor (LUPROC), described in Chapter 12. 

The initial and boundary conditions processors (ICON and BCON), described in Chapter 
13. 

The photolytic J-value processor (JPROC), described in Chapter 14. 

The process analysis processor (PROCAN), described in Chapter 16.  PROCAN is 
launched by the Models-3 framework and its execution is transparent to the user.  The 
outputs from PROCAN are used to control CCTM diagnostic outputs (see Section 15.3.1 
below). 

Building a particular model in the Models-3/CMAQ system requires the creation·of Fortran 
include files that populate the model's codes to set the specific computational grid and domain 
characteristics, the vertical layer structure, the chemical mechanism, and (for the CCTM) the 
process analysis to be used for the particular model.  Once these model configuration objects are 
set by the user in the Models-3  Science Manager, the Models-3  framework automatically 
generates the required global Fortran include files.  The use of global Fortran include files,  the 
notion of "Global Name Table Data," and the design concepts that govern _this  implementation 
are discussed in Chapter 18, Sections 18.5 and  18.7. 

15.1  Domain Configuration 

The computational domain configuration data are contained in three include files: 

• 

• 

• 

HG RD.EXT - The horizontal grid dimensions in terms of the number of grid cell columns 
and rows and the number of grid cells for which the computational domain boundary is 
extended for boundary data.  The number of these perimeter boundary cells is also 
specified. 
VGRD.EXT - The vertical layer dimensions as the number of vertical layers.  This 
include file also contains declarations for the layer surface and layer center arrays. 

COORD.EXT-The domain coordinate data: 

The map projection type (Lambert, Mercator, Stereographic, UTM, or latitude(cid:173)
longitudc ). 
The map projection parameters. 
The center of the grid's coordinate system with respect to the "mother grid" or 
parent grid (usually the main grid from the meteorology pre-processing). 
The horizontal grid cell sizes in meters. 
The vertical layer type.  There are 7 currently defined: 

crpco>  coordinates. 

1.  crP  coordinates. 
2. 
3.  crz coordinates. 
4. 
llp coordinates. 
5.  Pressure coordinates. 
6.  Z coordinates (meters above ground). 
7.  H coordinates (meters above mean sea level). 

The units of the top vertical layer value. 
The value of the top vertical layer variable. 
The units of the vertical layer surface values. 
The list of vertical layer surface values. 
The list of vertical layer surface values converted to  values monotonically. 
increasing with altitude. 
The grid name used for header description data in I/O API files (discussed in 
Section 15 .2). 


15.2 

Input/Output Applications Programming Interface 

Input and output data access for the CMAQ system is accomplished mainly through the 
Input/Output Applications Programmer's Interface (I/O API) sub-system [1].  The CMAQ system 
implementation of the I/O API is described in Chapter 18, Section 18.3.  In order to access 1/0 
A~rfilcs arid l!.~.~ otherI/O ~I ~ctions, iti~ necessary to specify the required declarations and 
parameters.  Some or all of the following include files are needed in each subroutine that uses the 
I/O API: 

~ 

:;, ... 11111ii1 

.••• 

' 

' 

' '  

•'' 

' 

11r 

1,, 

• 

• 

• 

• 

PARMS3.EXT - I/O API dimensioning parameters, file type and file characteristic 
parameters, coordinate system and map projection parameters, and vertical layer 
parameters. 

' 

.. 

,,, 

' 
'!iiill 

FDESC3.EXT- Fortran common blocks that contain a complete 1/0 API file description. 
The d~ .. !.~. in these comrn<?n. blocks are loaded from an I/OApl file whenopened and read. 
These data must be supplied from the user subroutine when an I/O API  file is opened for 
writing. 

IODECL3.EXT- Declal,'.ations and usage comments for I/O API functions and routines. 

II' 

11111111'11 

'II 

11111" 

• 

'II 

I• 

I 

XSTAT3.EXT- Exit codes for the 1/0 API M3EXIT function.  Generally, M3EXIT is 
called from any subroutine that produces a fatal error during 1/0 API  access. 

15.3  Other CCTM Configuration Control 

15.3.1  CCTM Process Analysis 

Th~ Models-3(~MA.Q system provides a diagnostic tool that allows a user to probe into the way 
the science processing is actually being executed in the CCTM.  The process analysis tool 
optionally provides two types of information, which are termed Integrated Process Rates (IPR) 
and Integrated Reaction Rates (IRR).  Note that these diagnostic tools apply only for the CCTM 
and not for the other models.  The reader is  referred to Chapter 16 for the details, but briefly the 
IPR processing captures the changes to the concentration field  for different species, or groupings 
of species, for each individual science process modeled.  The IRR processing focuses on the gas(cid:173)
phase chemistry and allows a detailed examination of various characteristics of the chemical 
mechanism implemented in the CCTM for the chosen scenario.  The data produced by these tools 
arc collected in 1/0 API output files, which can be further processed or examined using the 
visualization tools. 

. . .   ' 

"""""""' 

""'" 

""'"""" 

1
:::1111 

1111

1111111.,,' 

, 

I 

:11111111111111!" 

" "  

111111,·11 

' '  

""' 

' 

•"' 

" 

. 

, 

The CCTM requires three include files, even if neither IPR nor IRR processing is selected. 1 
These include files are automatically generated from PROCAN, and the Models-3 framework 
then incorporates them in the CCTM build process.  The three IPR/IRR include files are: 

'II 

·11111 

1 If process analysis is not requested while building a CCTM model executable, the  Models-3  framework 
automatic:illy supplies the include files. 

1111111'":111 

::;1 

' 

PA_CTL.EXT-The declarations and values of the two flags  that determine ifIPR or IRR 
(or both) processing is to be performed. 

PA_ CTM.EXT - Declarations and parameters that define the domain, output file 
descriptions, arrays and dimensions, IPR and IRR loop variables, and named common 
blocks for computational data. 

PA_ DAT.EXT - Data statements containing values for the variables needed to fill  in the 
common blocks in PA  CTM.EXT. 

15.3.2  CCTM Fixed Data 

The Models-3/CMAQ system can deal with other types of global include files and reference 
them from a user-supplied full  path to build a model.  The following three categories of global 
include files apply to the CCTM.  The other processor models (MCIP, ECIP, etc.) also have 
include files in this category, but not as extensively. 

• 

• 

• 

• 

• 

FILES_ CTM.EXT - The set of logical file names used in the current CCTM 
implementation.  The include file contains data statements for the file names as variables 
used in the codes and values for the variables.  The values are character strings that are 
UNIX environment variable values set in the scripts launched by the Models-3 
framework to run the models. 

CONST.EXT, CONST3_RADM.EXT-These include files contain parameter statements 
to define basic and frequently used air quality and meteorological modeling physical and 
mathematical constants.  Inclusion of these files in the model codes helps to assure 
consistency in the science calculations across all the processes and modules. 

BLKPRM_500.EXT - Computational blocking: For computational efficiency and 
reduced memory requirements, operations may be performed on groups of grid cells (a 
block of cells) at a time.  This include file provides convenient definitions to use 500-cell 
blocking. 

GRID_DECL.EXT- Declarations for the dimensions and species classes offsets in the 
main concentration array.  See Chapter 18, Section 18.2.2 for details. 

EMISPRM.chem.EXT, EMISPRM.vdif.EXT-These two include files contain the 
declarations that control in which process the emissions sources are injected; either the 
vertical diffusion process of the gas-phase chemistry solver process.  By using a C pre(cid:173)
processor (cpp) #ifdef directive, one or the other include file is actually included when the 
code is compiled.  The cpp directive is set by the user during the model building phase. 
The reader is referred to Chapter 18, Section 18.5 for more details. 

15.4  Generalized Chemistry 


15.4.1  Design 

In the past, a particular chemical mechanism was generally "hardwired" into a chemical transport 
model with mechanism parameters and variables embedded in the solver codes.  Implementing 
even minor changes incurred a high potential for code and consistency errors.  Additionally, 
some mechanism changes necessitated extensive coding changes, requiring careful tracing 
through all the mechanism dependencies.  Worse, not finding these dependencies led to errors 
th~,!. ~er.7 ~ot ?;?,,~c~'~"~arily detected ...  The Models-3/CMAQ system employs a generalized 
c~1~1niJ~aj aje~,b~~,,!11 processor(MP), also called the "mechanism reader," which greatly 
simp ifies the task of implementing or altering gas-phase chemical mechanisms and provides the 
capability of easily and safely using different mechanisms in the CMAQ system. 

MP reads an ASCII file that contains a symbolic description of a gas-phase chemical mechanism, 
and generates three ASCII Fortran source files.  The input ASCII file is called a mechanism 
description, or simply, "mechanism." 

I 

111111111111. 

,.:, 

" 

llll111llli1"'lllll1 

The ASCII mechanism description file is formatted according to a simple set of rules in a free-
fom1  format similar to the approaches used by Jeffries et al.  [2]  and Gery and Crouse [3].  The 
types of reactions that are supported are described below in Section  15.4.3, and the specific 
formats are described in the Models-3 User Manual, Appendix M [4]. 

111""111111111111 

111

',, 

I 

, 

' 

• 

""' 

"' 

• , •  

" 

Tht: Fortran files that are generated are incorporated as include files  in the CMAQ Fortran codes. 
They consist of: 

• 

• 

Fortran named common blocks that declare parameters and explicitly dimensioned arrays 
associated with gas chemistry' kinetic and .photolytic reactions. 

Fortran data statements that fill in the common block arrays with actual values 
determined from the input ASCII mechanism file. 

Th'~ common block~ and the data statements files are global and are used in ali the codes that 
require gas chemistry information .. 

Another file that is generated from the mechanism description file by MP, which is transparent to 
the user, is a species list found in the mechanism description.  This file  is used by the framework 
to generate all the gas chemistry species global include files that are required in the model codes. 

In order to  implement the mechanism reader's capabilities, the CMAQ system requires 
generalized gas-phase chemistry solvers.  At present, there are two such solvers available, 
SMVGEAR and QSSA, which are described in detail in Chapter 8.  The use of generalized 
solvers precludes some code optimizations that can increase performance, but significantly 
facilitates implementing new or altered chemical mechanisms. 

15.4.2  Operation 

The CMAQ system accounts for chemistry in three forms:  gas phase, aerosols (liquid and solid 
phase), and the aqueous phase.  Therefore, the Models-3 framework controls certain aspects of 
how the model species are treated in a model simulation and provides linkages between species 
in different phases.  Setting up the linkages is accomplished by means of a series of four species 
tables in a spread sheet format that are filled out by the user.  These tables also allow the user to 
control some aspects of the simulation and generated outputs and provide links to data such as 
emissions, initial and boundary concentrations, and dry deposition velocities that are generated 
by the CMAQ input processors.  The mechanism definition and species tables become objects 
that the Models-3 framework stores in its database.  Therefore if a required chemical mechanism 
already exists in the database as a predefined mechanism, the user does not have to enter any 
mechanism data into the framework. 

Since different gas-phase chemical mechanisms can be used in the CMAQ system and these 
mechanisms may employ different species names, it is necessary to supply the linkages among 
the gas-phase species, the fixed aerosol species, and the species that participate in aqueous-phase 
chemistry.  Similarly, it is also necessary to link the gas-phase species to emissions, deposition 
velocities, and to aqueous-phase scavenging.  In addition, the species tables allow the user to 
select which species concentrations are written to the output files and whether or not they are to 
be advected or diffused.  In the same manner, most of these linkages must be established for 
aerosol, non-reactive, and possibly tracer species. 

The methodology used to establish links between species names involves the concept of 
surrogate names.  Surrogate names are used to provide linkages between model species within 
the CCTM and to link the model species to those that represent data provided by other CMAQ 
processors.  For example the emissions-chemistry interface processor, ECIP, that links the 
Models-3 emissions processing and projection system (MEPPS) with the CCTM could write a 
species named XXX that represents xxx emissions rates.  However, the CCTM that uses xxx rnay 
have the corresponding model species named YYY.  Using the surrogate name concept, 
emissions species XX:X gets mapped to model species YYY.  The reader is referred to the 
Models-3 User Manual [4] for more details on the implementation of the set of predefined 
surrogate names used in the current version. 

An additional feature that can be applied to any of the surrogate species linkages is the 
application of a multiplicative, or scale factor.  The user can set the factor to be other than unity 
(the default) to easily modify some model input data related to the selected species and process 
linkage.  For example, this could be used to test the effect of changing the deposition velocity for 
a certain species, or to modify initial or boundary conditions data.  Also, the factors can be set to 
modify certain data between model species groups within the C~TM, such as in the gas-phase to 
aerosol linkage. 

These linkages are set by the user and are distributed among the four tables that establish the 
linkages among gas-phase, aerosols, non-reactive, and (possibly) tracer species.  Non-reactive 
species are gas-phase, but do not participate in gas-phase reactions.  Optional tracer species are 
inert and may have special, user-defined initial and boundary concentrations as well as emissions 
sources.  The reader is referred to the Models-3 User Manual [4]  for more details and a complete 
description of the species tables. 

The following is a list of the linkages set up in the current implementation of the Models-
3/CMAQ system.  The linkages take place with respect to the CCTM or model species name; the 
surrogate names and flags point to this name. 

1111 

111111

1,,:, 
2. 
3. 
4. 
5. 

6. 

7. 

8. 

9. 

,,111111111' 

10. 
ti. 
:iiiiii!I: 
12. 
13. 
14. 
15. 

16. 

17. 

18. 

1"11111111" 

,,II' 

,Mo4~l,,,species molecular weight. 

Emissions species surrogate name. 

Emissions species scale factor. 

Initiaf and boundary concentrations species surrogate name. 

Initial and boundary concentrations species scale factor. 

Deposition velocity species surrogate name. 

Deposition velocity species scale factor. 

Gas-phase to aerosol linkage species surrogate name. 

Gas-phase to aqueous-phase linkage species surrogate name. 

11111• 

11111°':111111111111111111 

I 

' 

"'ii' 

I 

"'II 

Gas-phase aqueous scavenging linkage species surrogate name. 
(}as-ph1as~ 1111~queous scavenging linkage species scale factor.' 
AerQ~Ql to aqueous-phase linkage species surrogate name. 

111111111111111·:11 

111111"111111,, 

'Ill 

1111 

I' 

11, 

1,,,,,.,, 

1111' 

Aerosol aqueous scavenging linkage species surrogate name. 

Aerosol aqueous scavenging linkage species scale factor. 

Non-reactive to aerosol linkage species surrogate name. 

Non-reactive to aqueous-phase linkage species surrogate name. 

Non-reactive aqueous scavenging linkage species surrogate name. 

Non-reactive aqueous scavenging linkage species scale factor. 

19.  Model species flag for participation in the advection processes. 

20.  Model species flag for participation in the diffusion processes. 

2!.~ 

:tv;fodet,,species flag for inclusion in the concentration output file. 

22.  Model species flag for inclusion in the dry deposition output file. 
23.  Model species flag f~r inclusion in the wet deposition output file. 
24. 
It should be noted that items 1 - 7 and  19 - 23 apply to all of the four groups of species (gas 
phase, aqueous phase, solid and liquid aerosols). 

Tracer spe:ies - si~lar linkages to any of the above. 

15.4.3  Supported Reaction Types 

In this section we describe the types of gas-phase chemical reactions supported by the 
generalized mechanism processor, MP, in the current version of the Models-3/CMAQ system. 
The reader is referred to Chapter 8 for a description of the available mechanisms. 

The gas-phase mechanism description file is a list of mechanism reactions.  The mechanism 
reactions list consists of lines of symbolic descriptions of elementary reactions among modeled 
chemical species, followed by a rule-based symbolic description of the reaction rate expression 
for that reaction.  The reader is referred to Chapter 8 for mechanism description examples. 

These mechanism reaction lists are generally "free-form," in that spacing of the symbols on the 
page is not important (except, perhaps for readability), and reaction descriptions may span many 
lines if necessary.  Liberal use of comments is facilitated by allowing for comment lines and 
comments embedded within the reaction lines.  In addition, each reaction may be labeled for 
subsequent reference.  The photolysis reactions are given labels for a specific photolytic table 
reference.  These tables are generated by the J-value processor and are used in the CCTM for the 
gas-phase chemistry processing.  Currently, there are 10 different rate constant expressions 
available for thermal reactions, including Arrhenius-type expressions and the class of so-called 
fall-off reactions (Type f,  below).  New expressions can be added, as necessary. 

The type of reaction rate expressions that can be calculated are: 

Type 1 : 
Type 2: 
Type 3: 

k=A 
k =  A(T/300)8
k =  Aec-c!T), 

, 

Type 4: 
Type 5: 
Type 6: 
Type 7: 

Type 8: 

Type 9: 

Type f: 

k = A(T/300)8 ec-c!T) 
k = ~ = kr/AeC·CIT)' 
k = Ak,,, 
k = A(l.O + 0.6P), 
k3 (M] 
k -k  + 
klMJ 
-o  
1+-3-
kz 
k =  k1 + k2[M], 
k =  k = 

ko[ M] 

l+ k 0 (M] I k; 

where  T =  temperature in deg K. 
where  C =  Ea I R, 

Ea  is the activation energy, and 
R is the gas constant. 

where kr is any previously defined forward reaction. 
where ~ refers to the n1
where P =  pressure in atmospheres. 

1t  reaction. 

,  where i,., k2,  and k3  are Type 3. 

~ 

where k1 and k2 are Type 3. 
pG 

where ko  and k;  may be Type 1, 2, 3, or 4, 
G =  { I + [In( ko  [M] I k;) j n ]2 r', 
F =  0.6 (usually), and 
n =  1.0 (usually). 

15-9 


1JM~1,,,11mS1~!J:,~1i,~,m,21~s~ripti()n fil,,~ that MP reads must foll<::>w  a set of format rules in .order to 
"::  describe the reactions listed above.  Generally, reserved ASCII characters and keywords on one 
line (which may wrap around tile page) represent parts of a reaction, includi~g species reactants 
and products, stoichiometric coefficients and rate expressions.  In addition, labels are supported, 
which function to allow one reaction to refer to another and to fix  photolytic reaction table names 
for photolytic reactions.  As mentioned above, the reader is referred to Chapter 8 for mechanism 
description examples.  Also, Appendix M of the Models-3 User Manual contains a complete 
description of these formats. 

"" 

" 

·iilllllllllll11 

·1 

II. 

111111111, 

111',,!lll 

.. 

·':::1111111111111111111

"

1 

·ll":;,,,,,,1111111: 

111111"' 

, "  

,,  15.4.5  Chemical Species Include Files 

,·.,, 

'l!,,1 

'" 

1111111· 

111 

;1!!!1!!!!111,1111 

111 

,,,11 

,,.:I',, 

. ' 

,,,,,,"'II,  ,,11111, 

,,, 

,111111,, 

As described above, MP reads and processes the mechanism description file, and one of the 
"'  0'111""""tpuislt generates is a file that contains the list of chemical species found in the 'mechanism. 
The Models-3 framework reads this species file to initialize the ·gas~phase species table in a 
format similar to a spreadsheet, which the user completes by inserting data such as molecular 
..  : weights, silrrogate names and factors, and processing flags.  The initial set-up presented to the 

user consists of all blank entries in each spreadsheet cell, but with the first column filled in with 
the species found in the MP-generated species file.  Thus all the species in the mechanism are 
li~~ed in the order in which they were found in the mechanism description file.  The top row is 
also filled in with category headers that are related to the linkages described above and that 
determine the framework-generated Fortran include files to be used in the codes. 

11," 

11111 .. 1 

1111,::1,1li;1il:: 

1111111111111 

,11111111111111111111111' 

11111111111111111111

11 

:11 

I· 

·1111 

"1111111111 

Tb~re ru;~,, th!:!::,~,,, ag,,git!9q~l !,a~J~s that fl[~ l,,!li!!,,f;ll,iz~g fqr aer{)sqJ~, non-reactive and tracer species. 
They do not have the first column set up, but the first row is set as in the gas-phase table.  It is up 
tcfthe user to fill in the first column, as with all the other spreadsheet cells. Appendix M of the 
Models-3 User Manual describes the details of how to enter data into these tables. 

"''II 

:111111111111""111" 

• 

''Ill:' 

11111111' 

'"1111 

• 

'111111 111111 

The following sections describe all the standard include files that are generated from the species 
tB;~l~,,~ .ar.d th~ Jiel!!::lvior o(Jhe i:nodels wh,en ?Cces~ing the data declared in the include files. 

15.4.5.1 

1111111!!!'•',, 

111::11111 

!1111111!!!1111' 

',, 111

:111111!!!!!!' 

llrr:!!!ll!1111!'' 

: .. '!"11111111 

Gas-Phase Reactions 

' 

111!,, 

111

, 

,,,,,,, 

' 

' 

'''••''• 

'', 

1111111:!,'', 

Th~ gas-phase reactions data are required in the CCTM by the gas-phase chemistry solvers and 
the subroutine PHOT, which calculates the photolysis rate constants.  These data are also 
required in JPROC, the processor model that calculates the table-based photolysis rates (see 
Chapter 14).  There are two include files associated with this data: 

"1111111111111" 

. """ 

:11111'111,I!,' 
111111" 

1111'' 
111
11111111!

'! 

• 

, 

,llll"''' 

',,,;:ll""lllllll' 

,  llllll"ll'r,,,:: .. , 

\,,::·,Jlllll!llllllllllllll' 

RXCM.EXT - contains all the declarations, parameter statements and array definitions 
I  associ'atec:f Wiih the gas-phase chemistry reactions.  This file also contains three named 
, common blocks for memory allocation of the data from  RXDT.EXT. 
R.XnT':1EXT- cont~ns data stat~ments'assodated with the p1~~eters and arrays declared''  II 
in RXCM.EXT.  These data fill the arrays in the named common blo~ks in RXCM.EXT. 

1'i" 

'  !  l!,ll'!lll1'11 

)''1lll1111illlllllll111r 

:,;,rrrr,,,,;:;,, 

,,,,,,,,,,,,,,,, 

,,llllllllll 

1' 

rrllll::,!I 

11
1111111

,,,,, 

,, 

,, 

,,,,,, 

. 

'' 

; 

' ' '  

' 

,, 

,, 

'

, 

I 

"!l1111il1: 

ll!lllir 

,' 

lllllr 


The CCTM driver program must include both of these files  in its declaration section.  The 
subroutines in the gas-phase chemistry solver process module must include only the common 
block file, RX CM.EXT.  That goes for the subroutine PHOT, as well.  PHOT is called from the 
gas-phase chemistry  "class-driver," the top-level subroutine (see Section 18.2.2 in Chapter 18). 
The CCTM driver, by including both files, causes the common blocks to be loaded with the 
necessary data used by all the modules that need the chemistry data, including the process 
analysis module, PROCAN. 

The remaining sections describe the global include files associated with the four species tables. 
As mentioned above, these files are automatically generated by the Models-3 framework, 
according to the user-specified linkages and flags, and are targeted for inclusion in all model 
routines that require the specific chemistry data in them.  For additional details the reader is 
referred to the Models-3  User Manual [4].  Each of the include files contains declarations and 
parameter statements for the loop index and array dimensioning for each particular species 
group.  If a particular species group is not used (e.g., aerosols or tracers), the include file contains 
declarations and parameter statements that set the species loop counter to zero and its array 
dimensioning parameter to one, thus maintaining complete generality and modularity within the 
codes.  In addition, by setting the dimensioning value to one (Fortran does not allow zero), the 
compiled codes do not waste memory, a typical problem when coding in Fortran with some pre(cid:173)
determined maximum dimension to hopefully account for all cases. 

15.4.5.2 

Model Species 

The framework generates four model species include files that together contain the names and 
molecular weights of all the chemical species available globally to the model. 

The model species include files are: 

• 

• 

• 

• 

GC _ SPC.EXT - gas-phase model species names and molecular weights. 

AE _ SPC.EXT - aerosol species names and molecular weights. 

NR_SPC.EXT- non-reactive species names and molecular weights. 

TR_SPC.EXT- tracer species names and molecular weights. 

The next three sections describe include files that are associated with data access by the CCTM 
from external files.  The data are referenced from 1/0 API files  by means of file variable names 
that are contained in headers in each of the files. 

15.4.5.3 

Emissions 

The CCTM reads the emissions data from an 1/0 API file produced by ECIP using the surrogate 
name concept discussed above.  The data are read by file variable names (surrogates) and stored 
in arrays that are linked to the corresponding model species names. 

The CCT!vf has the option of emissions sources injected as part of the vertical diffusion process 
or11111as part of the chemistry solver process, where the injected sources are treated as part of the 
chemical production terms.  To read source emissions data into the CCTM for processing either 
in the vertical diffusion or in the gas-phase chemistry modules, the following include files are 
required: 

,,·,',',',',11111,•ll' 

llllllllllllf,,I 

llllll,111111'1111111 

" "  

l11i1,.Jlllll'1' 

,.',1111• 

" 

, 

I 

" 

'I, 

"' 

"' 

' 

• 

• 

11111111· 

I 

GC_EMIS.EXT- gas-phase emissions surrogate species names with scale factors and 
indices that point to the positions of the species in the model species name table (in 
GC_SPC.EXT). 

AE_EMIS.EXT - aerosol emissions surrogate species names with scale factors and 
indices that point to the positions of the species in the model species name table (in 
Ap_SPC.EXT). 

'1''1111111111111111111111" 

·11111,,:i,,,,, 

Ill·, 

"' 

il1"'1!·111··;,',· 

'''':11lliillll', 

1
111
:.'.
1'1'
,'1' 
·11!!!111111111111" 

' 

"' 

''111 
11111111 

"' 

1
::1 
,,,,,1::: 

NR_EMIS.EXT - non-reactive emissions surrogate species names with scale factors and 
indices that point to the positions of the species in the model species name table (in 
NR_SPC.EXT). 

"" 

', 

11111· 
,,  ' '   : 

,,11 

'' 

I 

11' 

!,,,,,,'' 

.,, 

.. 

' 

: 

"I' 
' 

' '  '"':1111""1' 
,I. 
11111111 

• 

TR_EMIS.EXT - tracer emissions surrogate species names with scale factors and indices 
that point to the positions of the species in the model species name table (in 
TR_SPC.EXT). 

1111111111 

15.4.5.4 

'"'II 

Initial and Boundary Conditions 

For both the initial and boundary concentrations the CCTM reads I/O API input data by variable 
n3Jpe reference .. ~  The processing first checks to see if the model species name is on the file and 
reads the data associated with that name.  If the model species name is not on the file,  it checks 
for the surrogate name.  If that, too is not available, the input value is set to a minimum value 
("!l}Odel zero").  Additional information on initial and boundary conditions and how they are 
i~Elemented in th~··· Models-3/CM~Q system can be ~ound in  Chap!~r 14. 

• 

• 

• 

• 

GC_ICBC.EXT - gas-phase initial and boundary conditions surrogate species names with 
scale l~(.!tq~~ and in.dic~s th!:!t point to the positions of the species in the model species 
name table (in GC_SPC.EXT). 

AE_ICBC.EXT- aerosol initial arid boundary conditions surrogate species names with 
scale factors and indices that point to the positions of the species in the model species 
name table (in AE_SPC.EXT). 

NR_ICBC.EXT - non-reactive initial and boundary conditions surrogate species names 
""'.,ith  s~,~eJ~ctors and ir1:9k.~s that point to the positions of the species in the model 
species name table (in NR_SPC.EXT). 

· 

TR_ICBC.EXT- tracer initial and boundary conditions surrogate species names with 
scale,,,(~£~~~~ and iIJ-?ic~~ th~t point to the positions of the species in the model species 
name table (in TR_SPC.EXT) 

1111:11::1, 

·111 ::11,,, 

111"''1'' 
',11111111, 
,111"1 

15.4.5.5 

Dry Deposition 

The following include files are used to read in dry deposition velocities from an VO  API file 
produced by the meteorology-chemistry input processor, MCIP.  Using the surrogate name 
concept discussed above, the data are read by file variable names (surrogates) and stored in 
arrays that are linked to the corresponding model species names. 

• 

• 

• 

• 

GC_DEPV.EXT - gas-phase deposition velocity surrogate species names with scale 
factors and indices that point to the positions of the species in the model species name 
table (in GC_SPC.EXT). 

AE_DEPV.EXT - aerosol deposition velocity surrogate species names with scale factors 
and indices that point to the positions of the species in the model species name table (in 
AE_SPC.EXT). 

NR_DEPV.EXT - non-reactive deposition velocity surrogate species names with scale 
factors and indices that point to the positions of the species in the model species name 
table (in NR_SPC.EXT). 

TR_DEPV.EXT- tracer deposition velocity surrogate species names with scale factors 
and indices that point to the positions of the species in the model species name table (in 
TR_SPC.EXT). 

15.4.5.6 

Wet Scavenging 

In both the scavenging and the cross-phase linkages it is necessary to set up a mapping from the 
gas-phase species to the generic species names that are associated with aerosols and aqueous(cid:173)
phase chemistry and to the names of species that are absorbed by cloud and rain water.  Because 
of the generality with respect to the gas-phase species names, these generic names are set in the 
subroutines associated with aerosols, aqueous chemistry, and removal by in-cloud and 
precipitation scavenging.  They are linked to the model species names by employing the 
surrogate name concept described above. 

• 

• 

• 

GC_SCAV.EXT - gas-phase scavenging surrogate species names with scale factors and 
indices that point to the positions of the species in the model species name table (in 
GC_SPC.EXT). 

AE_SCAV.EXT- aerosol scavenging surrogate species names with scale factors and 
indices that point to the positions of the species in the model species name table (in 
AE_SPC.EXT). 

NR_SCAV.EXT - non-reactive scavenging surrogate species names with scale factors 
and indices that point to the positions of the species in the model species name table (in 
NR_SPC.EXT). 

TR_SCAV.EXT- tracer scavenging surrogate species names with scale factors and 
indices that point to the positions of the species in the model species name table (in 
TR_SPC.EXT). 

Cross-Phase Linkage 

15.4.5.7 
• 

1111,,,," 

,1,,,,,, 

• 

• 

• 

• 

• 

• 

GC_GiAE.EXT- surrogate names for gas-phase model species that participate in aerosol 
chemistry with scale factors and indices that point to the positions of the species in the 
m~detspecies name table (in GC_SPC.EXT). 

,1111111111''11,, 

GC_d2AQ.EXT- surrogate names for gas-phase model species that participate in 
aqueous-phase chemistry with scale factors and indices that point to the positions of the 
species in the model species name table (in GC_SPC.EXT). 

AE_A2AQ.EXT- surrogate names for aerosol species that participate in aqueous-phase 
chemistry with scale factors and indices that point to the positions of the species in the 
model species name table (in AE_SPC.EXT). 

NR_N2AE.EXT - surrogate names for non-reactive species that participate in aerosol 
chemistry with scale factors and indices that point to the positions of the species in the 
model species name table (in NR_SPC.EXT). 

NR_N2AQ.EXT- surrogate names for non-reactive species that participate in aqueous(cid:173)
phase chemistry with scale factors and indices that point to the positions of the species in 
the model species name table (in NR_SPC.EXT). 

TR_ T2AE.EXT - surrogate names for tracer species that participate in aerosol chemistry 
with scale factors and indices that point to the positions of the species in the model 
species name table (in TR_SPC.EXT). 

TR_T2AQ.EXT- surrogate names for tracer species that participate in aqueous-phase 
chemistry with scale factors and indices that point to the positions of the species in the 
modetspecies name table (in TR_SPC.EXT). 

15.4.5.8 

Operational Choices 

The Models-3 framework provides the capability of allowing the user to control how some of the 
processing is carried out with respect to the chemical species in the CCTM.  The following lists 
describe the include files that the framework generates for the various processes: 

1. 

Advected species: In order to save memory storage or to minimize computation, the user 
may choose not to advect certain modeled species such as some radicals.  In general, the 
special "counter species,, used in a mechanism should not be advected. 

• 

GC_ADV.EXT - names of gas-phase model species that are advected and indices 
that point to the positions of the species in the model species name table (in 
,,  GC_SPC.EXT). 

AE _ADV .EXT - names of aerosol species that are advected and indices that point 
to the positions of the species in the model species name table (in AE_SPC.EXT). 
NR_ADV.EXT - names of non-reactive species that are advected and indices that 
point to the positions of the species in the model species name table (in 
NR_SPC.EXT). 
TR_ADV.EXT- names of tracer species that are advected and indices that point 
to the positions of the spedes in the model species name table (in TR_SPC.EXT). 

2. 

Species that undergo diffusive processes: The comment above, for advected species, 
applies also to diffused species. 

• 

• 

• 

• 

GC_DIFF.EXT- names of gas-phase model species that are diffused and indices 
that point to the positions of the species in the model species name table (in 
GC _SPC.EXT). 
AE_DIFF.EXT - names of aerosol species that are diffused and indices that point 
to the positions of the species in the model species name table (in AE_SPC.EXT). 
NR_DIFF.EXT - names of non-reactive species that are diffused and indices that 
point to the positions of the species in the model species name table (in 
NR _ SPC.EXT). 
TR  DIFF.EXT- names of tracer species that are diffused and indices that point to 
the positions of the species in the model species name table (in .TR_SPC.EXT). 

3. 

Species that are saved to the dry deposition output file:  Generally, all the dry 
deposition species that are modeled would be saved to the output file, but a user might 
elect to reduce the number and save only a subset. 

• 

• 

• 

• 

-

.• 

GC_DDEP.EXT- names of gas-phase model species that are written to the dry 
deposition output file and indices that point to the positions of the species in the 
model species name table (in GC  SPC.EXT). 
AE_DDEP.EXT- names of aerosol species that are written to the dry deposition 
output file and indices that point to the positions of the species in the model 
species name table (in AE_SPC.EXT). 
NR_DDEP.EXT - names of non-reactive species that are written to the dry 
deposition output file and indices that point to the positions of the species in the 
model species name table (in NR_SPC.EXT). 
TR_DDEP.EXT- names of tracer species that are written to the dry deposition 
output file and indices that point to the positions of the species in the model 
species name table (in TR_SPC.EXT). 

. 

4. 

Species that are saved to the wet deposition output file:  See the comment in item 3. 

• 

• 

GC_ WDEP.EXT- names of gas-phase model species that are written to the wet 
deposition output file and indices that point to the positions of the species in the 
model species name table (in GC_SPC.EXT). 
AE_ WDEP.EXT- names of aerosol species that are written to the wet deposition 
output file and indices that point to the positions of the species in the model 
species name table (in AE_SPC.EXT). 

· 

• 

• 

NR_ WDEP.EXT- names of non-reactive species that are written to the wet 
deposition output file and indices that point to the positions of the species in the 
model species name table (in NR_SPC.EXT). 
TR_ WDEP.EXT- names of tracer species that are written to the wet deposition 
output file and indices that point to the positions of the species in the model 

..........  species name table (iri TR_ SPC.EXT). 

... 

.. 

. .. 

Sp~~i"~s th1~t are saved to the concentration output rile: See the comment in item 3. 
• 

GC_CONC.EXT- names of gas-phase model species that are written to the 
concentration output file and indices that point to the positions of the species in 
the model species name table (in GC _ SPC.EXT). 
AE_CONC.EXT- names of aerosol species that are written to the concentration 
output file and indices that point to the positions of the species in the model 
species name table (in AE_SPC.EXT). 

....  NR_CONC.EXT - names of non-reactive species that are written to the 
I:  concentration output file and indices that point to the positions of the species in 

the ....... model species name table (in NR _ SPC.EXT). 
TR_CONC.EXT - names of tracer species that are written to the concentration 
'output file and indices that point to the positions of the species in the model 
species name table (in TR_SPC.EXT). 

• 

• 

• 

111111111 

15.4.5.9 

Tracer Species 

The use of tracer species is purely user-determined.  An application with tracers can provide the 
modeler withinsights into how the model is simulating various physical processes, like 
advection or diffusion.  If tracer species are to be modeled, the user must have created the special 
table entries appropriate to the application.  In addition special data, such as emissions or initial 
and boundary tracer concentrations, must be created in the corresponding 1/0 API files.  The 
specialized data creation is outside the scope of the Models-3  framework and must be carried out 
by the user in conjunction with the application that is being modeled. 

15.6  Conclusion 

In this chapter we have described how PCP helps to establish modularity and consistency for 
particular applications of models in the CMAQ system.  The Models-3 framework processing, 
through PCP, enables model developers and model users to study the effects of different 
irpplementations of the science and codes or to execute different applications at various scales 
"'{,!~h difieren!,,,,,,,~h~,~ica!,, n;iec~~SQ1S pr with ,,~ifferent nur1~r~cal so!yer~.  'J'h~ d~sign and 
implementation of the concepts in the program control processing helps to establish a true "one(cid:173)
atrnosphere" approach to modeling. 

15.7  References 

': 

,;iu, 

[1) 

''"11

' 

I 

llHllli1111li111 

11!llliiiilll~:: 

,, 

'I' 

I, 

::1 

,, 

'1 

'Ill 

" 

,,, 

1" 

"The EDSS/Models-3 I/O API"  http://sage.mcnc.org/products/l/O APV 

:' 

,, 

,, 

Ii 

Jeffries H.  E: (1990), "User's Guide to Photochemical Kinetics Simulation System PC-

[2] 
PKSS Software Version 3," Chapel Hill, N.C.  27514 

Gery M.  W.  and R.R. Crouse (1990), "User's Guide for Executing OZIPR," EPN600/8-

[3] 
90/069, U.S. Environmental protection Agency, Research Triangle Park, NC 27711 

EPA Third-Generation Air Quality Modeling System Models-3 Volume 9b, User 

[4] 
Manual, Appendices, June 1998, EPA-600/R-98/069(b) 

This chapter is taken from Science Algorithms of the EPA  Models-3 Community 
Multiscale Air Quality (CMAQ) Modeling System, edited by D.  W. Byun and J. K. S. 
Ching, 1999. 
