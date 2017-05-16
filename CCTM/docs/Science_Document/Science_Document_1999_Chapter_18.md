Chapter 18 
===========

INTEGRATION OF SCIENCE CODES INTO MODELS-3 

Jeffrey Young • 

Atmospheric Modeling Division 

National Exposure Research Laboratory 
U.S. Environmental Protection Agency 

Research Triangle Park, NC 27711 

ABSTRACT 

A complete group of CMAQ science codes has been integrated into the Models-3 system 
following a set of rules to ensure compatibility and compliance with design principles that enable 
modularity and flexibility and that allow easy modification and replacement of science process 
components.  This chapter describes the concept of classes and modules, the Models-3 
input/output application programming interface, science code configuration management, and 
model building and execution concepts 

·on assignment from the National Oceanic and Atmospheric Administration, U.S. Department of Commerce. 
Corresponding author address: Jeffrey Young, MD-80, Research Triangle Park, NC 2771 l.  E-mail: 
yoj@hpcc.epa.gov 


18.0 

INTEGRATION OF SCIENCE CODES INTO MODELS-3 

18.l 

Introduction 

Integration of the Community Multiscale Air Quality (CMAQ) science into the Models-3 design 
~aradigm takes place at the level of transforming the science, described by systems of partial 
differential equations, into a discretized and parameterized numerical model.  The integral parts 
that arc involved in the process are the coding language (Fortran 77, currently), an operational 
design, a set of coding rules, computer platforms, model data access methods and storage 
mailagemcnt, and a public code repository.  Models-3  is a user interface system designed to 
fnci11!J,~q.~7 .~S, 11$!~1~,~l~rment and use of simulation models by scientists, model developers and 
regulatory users.  Tne Models-3  framework provides a high-performance computational structure 
for a community modeling system that cun-ently contains distinct emissions, meteorological, and 
chemical transport air quality models.  In this chapter we define a model as a single, complete 
executable built from a set of compiled subroutines that define either a core environmental 
simulation mod~l ora l?roces~ingsystem that provides data to  a simulation model.  The 
framework also links the models to data management tools and analysis/visualization software. 

A complete set ofCMAQ science codes has been integrated into the Models-3 system.  The 
development and integration of these codes has followed a set of principles relating to design and 
implementation concepts that are discussed in this chapter. 

We have promulgated a small set of software requirements and integration rules related to these 
concepts so that science codes can be developed that conform to Models-3 requirements.  With 
this approach, the general user community may readily develop and execute models that use the 
rich and growing base of science codes and data that reside in Models-3.  A user may contribute 
to that base and allow others to  integrate his developments by  applying these integration rules. 

"'111111111111111111111111111111111111 

11111111111111111 

With access to all the released, or publicly available source files,  a developer can supplement his 
O\\TI codes and readily develop and test model versions within a highly modularized building 
environment.  On the other hand, people whose primary interest is studying the effects of control 
strategies or regulatory applications can easily build and execute standard model versions for 
different emissions scenarios and modeling domains. 

The Models-3 and CMAQ systems have been developed to  foster community access, and to 
facilitate the improvement of the CMAQ system over time by  the easy inclusion of new 
scientific models and modules developed by researchers in the larger scientific community. 

The CMAQ system contains a number of models needed to cany out an air quality model 
application.  Defining a "model" to be an executable built for a specific application, the codes 

1

' 

,;;;;Ii 

:'iii  ,,U~q,1111111,!1211~<;;.11,~J? ~~~model are relatively general.  To build a specific model, the user selects a set of 

·  fundamental criteria that determine the application, such as the modeling domain, the chemical 
1111  mechanism, and physical process solvers.  The reader is referred to Chapter 15  for details. 
Defining a generic model as the set of codes from which a particular application can be built, the 
following generic CMAQ models are currently available in the system: 


ICON provides the required initial conditions for a model simulation as concentrations of 
specified individual chemical species for the complete modeling domain. 

BCON provides the needed simulation boundary conditions as concentrations of 
individual chemical species for the grid cells surrounding the modeling domain. 

Emissions-Chemistry Interface Processor (ECIP) transforms emission files produced 
by the Models-3  Emissions Processing and Projection System (MEPPS) into an hourly, 
3-D emissions data file for the CMAQ Chemical Transport Model (CCTM).  For a 
detailed discussion of the MEPPS operations, refer to Chapter 6 of the Models-3 User 
Manual [6]. 

The Meteorology-Chemistry Interface Processor (MCIP) interprets the output from a 
meteorological model, such as the Penn State/NCAR fifth-generation Mesoscale Model 
(MMS) [8], and prepares the data for use in the CCTM. 

The Landuse Processor (LUPROC) provides a high-resolution landuse database for the 
CMAQ system.  For example, MCIP uses the output from LUPROC to obtain surface 
characteristics for computing dry deposition and other planetary boundary layer 
parameters. 

The photolysis model (JPROC) computes photolytic rate constants G-values) for the 
gas-phase chemistry used in the CCTM. 

The Plume Dynamics Model (PDM) generates plume dimensions and positions along 
with other related data for use in applying the plume-in-grid module in a CCTM 
simulation. 

The CMAQ Chemical Transport Model (CCTM) simulates the chemical and physical 
processes affecting tropospheric pollutants and estimates pollutant concentrations (e.g., 
ozone, particulate matter (PM2.5  and PM 10), and carbon monoxide) and acid deposition. 

The reader is referred to  the Models-3  User Manual  [6]  for more details and descriptions of the 
usage of these models. 

Each generic model consists of a complete set of codes that can be compiled and linked into 
different model executables by  means of specific user-selected options with regard to groups of 
code called modules, the horizontal grid and vertical layer domain, the chemical mechanism, the 
computer platform, and compiling options. 

One of the key issues in modeling is the manner in which developers and users must deal  with 
reading, writing and using model data.  Models-3  provides a user-friendly Input/Output 
Applications Programming Interface (1/0 API) library [4] that enables a universal approach to 
managing data across subroutines, models, platforms, and networks. 

The organization of the remainder of this chapter is as follows:  We discuss the concepts that lead 
to and the implementation of classes and modules in the next section ( 18.2).  We describe I/O 
API  usage and function in Section  18.3.  Code management, an important consideration for 
complex models, is discussed in  Section 18.4.  In  Section 18.5 we discuss how a model is 
organized and constructed through Models-3.  Issues in regards to executing a particular model 
are discussed in Section 18.6.  Using the Models-3  framework to  construct and execute a model 
is summarized in Section 18.7.  Finally in Section 18.8, we discuss concepts and issues that 
address Models-3 compliant coding practice and how to ensure code development that is 

11:  confonnant to the Models-3 system. 

18.2  Classes and Modules 

18.2.1  Operational Design 

'Ql~"""'g~sign concept of code classes is used to facilitate the plug&play capability in the Models-
3/CMAQ system.  Science process modules are grouped into classes that are primarily based on 
,!Pe,,,,!!,,r!,~,:,~I?littiI'}g paradigmused in a CMAQ Chemical Transport Model (CCTM).  Generally 
, each class is associated with a particular science process.  A module consists of a complete set of 
subroutines capable of modifying the concentration field related to the science process associated 
with the module's class.  The CCTM is designed so that each module computes the changes in 
the concentration ~ield (C9l.lID) SJ:>ecif!.c to that particular science process.  A science module 
'o,,,,, perates ontheentireihree:climensional gn"dded concentration field for a period of time called 
the synchronization time step interval.  The module performs whatever looping over that grid 
and over whatever internal time steps that are necessary to complete its function. 

,,,,,,111111, 

'11111111:''1111111111111 

111111111, 

.1111111111111'"'11,, 

·1111111111111 

'Ill 

'• 

'i,,,,1111111111111111111111111· 

'1111:"'"111111111111111111111111111111 

,  '11111111111111111111111'"'""""'111111111111:" 

'I 

'",,11 1
' 

'Ill'  1111'"""111' 

Ill' 

,,1111; 

:ii 

,,· 

11  iii 

"""1111'11"':1111111111111111,,,:1111"'111111111111""111""":11'"111111:' 

.111111111111111111111 

''I' 

111' 

'lllll''h 

1111111 

Table 18-1  lists the classes and associated processes and modules currently available for the 
GGIM,:,,,,, !nJ?4U!;!ing a particular CCTM, one module is selected from each class. 

The other CMAQ models (i.e., those other than the CCTM) do not generally follow the time(cid:173)
splitting science process paradigm.  However, for convenience and consistency in model 
building, and for code organization, we have extended the concept of classes and modules to 
apply to the other CMAQ models.  Class and module organizations similar to those in Table 18-1 
exist for the other models but are less extensive. 

Table 18-1.  The Classes, Processes, and Modules Available in CCTM 

CLASS 
driver 
init 
hadv 

(PROCESS) 
(Control  Model  Execution) 
(Initialize  Model) 
(Horizontal  Advection) 

vadv 

(Vertical  Advection) 

hdiff 

(Horizontal  Diffusion) 

vdif f 

(Vertical  Diffusion) 

adj con 

(Advected  Mass  Adjustment) 

MODULES 
ctm 
init 
hbot  (Bott's  scheme) 
hppm  (Piecewise  Parabolic  Method) 
hadv_noop  (no  operation) 
vbot  (Bott' s  scheme) 
vppm  (Piecewise  Parabolic  Method) 
vadv_noop  (no  operation) 
unif  (Uniform  eddy  diffusion) 
hdiff_noop  (no  operation) 
eddy  (eddy  diffusion) 
vdiff _noop  (no  operation) 
denrate 
adjcon_noop  (no  operation) 

(Photolytic  Rate  Constants)  phot 

phot 

chem 

aero 

cloud 

(Aerosol  Solver) 

(Gas  Chemistry  Solver) 

phot_noop  (no  operation) 
smvgear  (SMVGEAR  solver) 
qssa  (Vectorized  QSSA  solver) 
chem_noop  (no  operation) 
aero 
aero_noop  (no  operation) 
aero_depv 
aero_depv_noop  (no  operation) 
(Cloud  and  Aqueous  Chemistry)cloud_radm  (RADM  cloud  scheme) 

aero_depv  (Aerosol  Dry  Deposition) 

ping 

(Plume-in-Grid) 

pro can 
couple 
util 

(Process  Analysis) 
(Couple  for  Transport) 
(Utility  Processing) 

cloud_noop  (no  operation) 
ping_smvgear  (uses  SMVGEAR  sol~er) 
ping_qssa  (uses  QSSA  solver) 
ping_noop  (no  operation) 
pa 
gencoor 
util 

18.2.2  CGRID 

To facilitate the science processing for the different chemistries involved in CMAQ, the 
concentration field array CORID, is partitioned into four species classes: gas chemistry, aerosols, 
non-reactive, and tracer species.  This order is mandatory and maintained throughout the 
processing.  A subroutine, CORID _MAP provides the means to correctly index into the CORID 
array for each science process that deals with individual classes of species.  The tracer species 
group is user-determined and may be empty in CORID.  Aqueous chemistry is dealt with solely 
in the cloud processing, and currently aqueous species are not transported outside of the cloud 
processing, therefore do not appear explicitly in CORID.  A subroutine call to CORID_MAP 
provides pointers to the locations of the species classes within CORID.  A process that deals only 
with one of the classes (e.g. aerosols) can thus determine which part of CORID it needs to 
access. 

18 .. ~ .. 3 ....  CI~ss Driver 

Generally, each module within a class requires a "class-driver," which is the top level subroutine 
within the module - it is at the top of the call chain for that module.  Exceptions for this paradigm 
exist for the util, procan, phot, and aero_depv classes.  The class-driver presents a fixed name 
and calling interface to the driver class subroutine SCIPROC that calls all the science process 
modules.  The calling interface consists of a subroutine argument list containing CGR1D, the 
cuq,rnt s~~p~n .. ~11111111g~~ ~~ ... ~ .. !11~· an~ a t1~.~ ..  ~!ep vector ...  Each class also contains a "no-operation" 
module, if appropriate.  The no-op module has the fixed name and call interface for that class, 
but performs no calculations.  When called, it merely returns control to SCIPROC. 

"'"~"""""""""""'" 

"""""""" 

111' 

' 

,,1111111 

111111111:,, 

1111 

1,,,,1::,,, 

• 

" 

• 

• 

• 

The following algorithm illustrates the processing and call sequence used in a CMAQ model for 
fractional time steps processed non-symmetrically with respect to the chemistry processes: 

Load  CGRID  from  initial  data;  set  initial  Date/Time 
Get  the  TimeStep  vector:  TimeStep[l] 
TimeStep[2] 
TimeStep[3] 

output  time  interval 
synchronization  time  interval 

c  horizontal  advection  time  interval 

Foreach  output_time_step 

Call  Couple  (  CGRID,  Date,  Time,  TimeStep  )* 

Foreach  sync_step 

[in  output  time  interval] 

Call  Horizontal_Advection  (  CGRID,  Date,  Time,  TimeStep 
(  CGRID,  Date,  Time,  TimeStep 
Call  Vertical  Advection 
(  CGRID,  Date,  Time,  TimeStep 
Call  Mass_ Adjustment 
Call  Horizontal - diffusion  (  CGRID,  Date,  Time,  T.imeStep 
Call  De-Couple 
Call  Vertical  diffusion 
Call  Plume  in  Grid 
Call  Gas_Chemistry 
Call  Aerosol 
Call  Cloud 

(  CGRID,  Date,  Time,  TimeStep  ) * 
(  CGRID,  Date,  Time,  TimeStep  ) 
CGRID,  Date,  Time,  TimeStep  ) 
( 
(  CGRID,  Date,  Time,  TimeStep  ) 
(  CGRID,  Date,  Time,  TimeStep  ) 
(  CGRID,  Date,  Time,  TimeStep  ) 

-

11111111111•• 

• ••• 1111111111  Advance  Date/Time  by  synchronization  time  interval 

(  CGRID,  Date,  Time,  TimeStep  ) * 

111111111::·  •••.• 

····:111111; 

"'Ill 

,,,1,, 

End  Foreach  sync_step 

Call  Couple 

.... , 

:111: 

II' 

,1 

•• 

······:111111111· 

1111111!!

11

1:: 

Call  De-Couple  (  CGRID,  Date,  Time,  TimeStep  ) * 
Write  Concentration  File 

End  Foreach  output_time_step 

•  Couple  CGRID  concentrations  with,  or  de-couple  CGRID  from  the  air  density  x  the 
Jacobian  of  the  computational  grid.  See  Chapter  6  for  a  complete  discussion  on  the 
concept. 

'I 

1111, 

"''1111 

I, 

"':11111111111 

No.~ shown in tl!e algorithm are processes that deal with source emissions (in either 
Vexticnl_diffusion or Gas_chemistry), dry deposition (in Vertical_ diffusion), and wet deposition 
(in Cloud).  Also not shown are calls to process analysis routines after each science process, 
which compute the integrated process rates for that particular process.  Such calls are also 
embedded in science process modules that affect CGR1D, such as emissions injection in either 
the vertical diffusion process or gas chemistry.  The Science Process Code Template in Section 
18.8.3 below illustrates how these calls can be integrated within a module. 

In order to  maintain the Models-3/CMAQ modularity and data-independence, the science 
process class-drivers must adhere to coding standards with regard to  the calling interface, file 
data 1/0, and standardized, uniform domain and mechanism data within global include files.  See 
Section 18.5, below, and Chapter  I 5 for a more complete discussion on the use of global include 
files.  These coding standards have not been propagated to the level below the class-driver.  At 
the sub-module level there are no strict 1/0 or coding standards, however we offer suggestions to 
facilitate the potential incorporation of a module into the Models-3 system in Section 18.8 below. 

18.2.4  Synchronization Time Step 

There are many different time scales that are important in modeling.  The CCTM deals with four 
levels of time stepping: 

1. 

2. 

3. 

4. 

Output time step - the time interval for which output data is written to disk. 

Synchronization time step - the time interval for which the science processes, 
represented by science modules, are considered to  run independently of the other 
processes.  This is the time interval during which no interaction with the other time-split 
science processes needs to take place. 

Advection time step - the time interval over which horizontal advection occurs.  It may 
be less than or the same as the synchronization time interval. 

Local or internal time looping - a possibly variable time interval that subdivides the 
synchronization time step and is dependent on the user's implementation of a particular 
algorithm. 

There may also be some science processes that don't fit  into this type of time scale hierarchy. 
The sub-grid cloud time step in CCTM for example, has a fixed  lifetime that may span the 
synchronization time interval. 

In the CCTM the synchronization time step is essentially the time interval over which the 
chemistry processes are considered to  be time-split and independent of the other processes. 

The proper relationship between the first three time steps listed above is determined by the 
"ADVSTEP" algorithm.  Based on a user-supplied output time step interval, and optionally an 
upper and lower limit for the synchronization time step interval, 1 the algorithm: 

• 

• 

• 

Ensures the synchronization time interval evenly divides the output time interval. 

Ensures the synchronization interval to not be greater than the upper limit. 

Determines a horizontal advection time interval that ensures all  horizontal advection 
calculations satisfy a Courant condition with respect to  the horizontal winds for each 
output time step.  See Chapter 7 for details about the Courant condition requirement. 

11fthese limits are not specified, the algorithm uses defaults of 900 and 300 seconds, 
respectively. 

Attempts to establish a horizontal advection time interval as close to the synchronization 
! iriteivaf as possible, but evenly divides the synchronization interval. 

1111 

1

''"'!

ii

• 

In case the Courant condition restriction forces an advection time interval to  be less than 
the lower synchronization interval limit, sets advection time intervals to be as dose to the 
lower limit as possible but still evenly divide2 the lower limit. 

If none of these criteria can be satisfied, reports the issue and aborts the execution. 

18.3 

Input/Output Applications Programming Interface 

··  · :" 

"''''"" 

A model needs access to external data.  Data access, including output to external files,  is done at 
the modulelevel.  The Models-3  IIO API [4] provides a standardized interface to external data 
that enables a user to follow the object oriented design concept of encapsulation.  For example, 
all basic and some derived meteorological data are calculated outside the CCTM (see MCIP 
documentation in Chapter 12, e.g.) and available to a module through calls to the Models-3 1/0 
A~,,! l~,J~rnry [~J:Thus the user can avoid the re-calculation of various meteorological variables 
,wi1.~I~. ~l!~r.!?l!'.~ .. !,!!:e~ ..  ~s!~& different algorithms and parameterizations to re-compute essentially the 
same quantities, which may give rise to errors and modeling inconsistencies.  It is expected that 
the modules treat these I/O API data completely independently, not knowing or caring if files 
haye beeq opened or read by any other module.  In particular, any subroutine within a module 
can open an I/O API file.  It is recommended that at least one routine within a module, preferably 
the class-driver, should open any I/O API files that are required in the module. 

In general, the 1/0 API provides the Models-3  user with a library containing both Fortran and C 
routines, which manage all the necessary file manipulations for data storage and access.  The 
main requirement is that the user follows the I/O API conventions for data structures, naming 
conventions, and representations of scenario date and time.  The 1/0 API routines manage access 
to the files in such a manner that data can be read or written flexibly, virtually in any order, and 
freeing the user from being concerned with low level file manipulation details, such as the order 
or format of file variables. 

In addition, the I/O API routines can be used for both file storage and cross-media model 
coupling using Parallel Virtual Machine (PVM) mailboxes. 

For file storage and access, the 1/0 API, which is the standard data access library for Models-3, 
is built on top ofNCAR's netCDF files [5].  NetCDF (network Common Data Form) is a library 
that provides an interface for array-oriented data access.  The netCDF library also defines a 
mt;lchine-independent format for representing scientific data.  Together, the interface, library, and 
form~t support the creation, access, and sharing of scientific data.  The netCDF software was 
developed at the Unidata Program Center in Boulder, Colorado.  The freely available source can 
be obtained by anonymous FTP [5]. 

The I/O API provides a variety of data structure types for organizing the data, and a set of access 
routines that offer selective direct access to the data in terms meaningful to the user. 

Jll  ,!!!: 

.illlllllllllll: 

2S!!ictly speaking, this means divides without remainder, not by a factor of two. 

Since they are netCDF files, 1/0 API files share the following characteristics: 

1. 

2. 

3. 

4. 

5. 

They are machine-independent and network-transparent.  Files created on a Cray 
Supercomputer can be read on a desktop workstation (or vice versa) either via NFS 
mounting or FTP, with no data format translation necessary; 

They are self-describing.  That is, they contain headers that provide a complete set of 
information necessary to use and interpret the data they contain; 

They are direct access.  A small subset of a large dataset may be accessed efficiently 
without first reading through all the preceding data.  This feature is mandatory in 
visualization requiring rapid access of large datasets; 

They are appendable.  Data can be appended to a netCDF dataset along one dimension 
without copying the dataset or redefining its structure.  For example, the I/O API can add 
more time step data to a previously created file; and 

They are sharable.  One writer and multiple readers may simultaneously access the same 
netCDF file.  This means, for example, that visualization tools can access the file and 
display data while the file is being created by the model. 

The 1/0 API has been designed to support a variety of data types used in the environmental 
sciences, among them: 

• 

• 

• 

• 

Gridded data (e.g., concentration or meteorological fields); 

Grid-boundary data (for model boundary conditions); 

Scattered data (e.g., meteorology observations or source level emissions); and 

Sparse matrix data (a specialized data type used in an emissions model). 

1/0 API files support three different time step structures: 

1. 

2. 

3. 

Time-stepped with regular time steps (e.g.  hourly model output concentration fields or 
twice-daily upper air meteorology observation profiles); 

Time-independent (e.g.  terrain height); and 

Restart, which always maintains the last two time step records of output from a running 
process as "even" and "odd." The "odd" is available for restart in case of a crash while 
the "even" step is being written, and vice versa. Restart data does not consume an 
inordinate amount of disk space with only two time steps worth of data at all times. 

The 1/0 API provides automated built-in mechanisms to support production and application 
requirements for dataset histories and audit trails: 

• 

Identifiers of the program execution that produced the file. 

Description of the study scenario in which the file was generated. 

The UO API also contains an extensive set of utility routines for manipulating dates and times, 
performing coordinate conversions, storing and recalling grid definitions, sparse matrix 
arithmetic, etc.  There are a variety of related programs that perform various analysis or data(cid:173)
manipulation tasks, including statistical analysis, file comparison, and data extraction.  Section 
18.8.3 below illustrates the use of some of these utilities in coding practice, and the web site [5] 
provides background materials, a user manual, and a tutorial. 

18.4  Code Configuration Management 

1111" 

,1111111111'·'1" 

1111111111111111, 

''1111111, 

•111111, 

'Ill'  ,111111'" 

llllllllli, 

'I"" 

,lllllllllllllllr"' 

"'1111111111111111111111111 

1  ::1111111 

18.4.1  The Need 

1 

Ill 

'I 

"I 

II 

~~~151~,,,~~!!,!,,,~ lfi!l~e ~d growing community that uses and develops a wide variety of programs, 

"""""  nfo  ules and codes, it is imperative to systematically manage the cross-community access to this 

software.  Typically, successful management of software involves: 

,, 

"Ill 

11111 ! ~ 

• 

• 

• 

A repository - a place where all of the public code resides; 

11111111111 

The concept of archived code - codes that have been deposited into the repository in such 
'a manne'r''that anyone can extract the exact code at a later time.  This involves some kind 
of transformation program to maintain master copies of the codes with embedded change 
tables; 

The concept of revision control - archiving codes results in modifying the tags or unique 
revision identifiers in the change tables in the master copies in order to  recover the exact 
code at a later date; and 

11111

1111111111111 

1
:::111 

1i1" 

11

• •  

' " " ' '   The concept of released code - codes that have reached some state of maturity and have 
I' b~,,~11~ d~1~!~gnated ~i~ ~ome kind of "released" status.  They can be used with reasonable 

::1111 

11

, 

111

:
::::

illl" .. t,  , expectation,,,,cf reha~1ht:y.,, 

1

, 

.,. 

.. 

The paradigm used employs the following scenario. 

1. 

2. 

3.~ 

A user modifies or develops code.  The code may be one subroutine or many, possibly 
comprising whole science models.  The code may originate from "scratch," or be 
extri;1:ct~g fr9gi !]J.e repository and modified. 

After testing or reaching a point of being satisfied with his/her results, he/she decides to 
save it in the repository so that others can have access to  it. 

Some ru;chiyed codes may still be in an experimental, or development, state while others 
may be reasonably stable and more completely tested.  The latter may be designated as 
"released." There is no enforceable means to control access based on an experimental or 
released state.  The community will have and should have access indiscriminately, well 
aware that using development state code is risky. 

As the user continues to work with the codes, he/she may make enhancements or 
discover and fix errors.  The upgrades are then installed in the repository, which 
automatically assigns unique revision identifiers. 

The repository is  located where it is conveniently accessible to all pertinent users, and it 
is maintained by an administrator who sets and enforces general access rules. 

18.4.2  The Tool 

There are many configuration management tools both free and commercially available.  We 
chose The Concurrent Versions System (CVS) [1]  mainly because of its versatility.  CVS 
controls the concurrent editing of sources by several users working on releases built from a 
hierarchical set of directories.  CVS uses the Revision Control System (RCS) [2]  as the base 
system.  Other reasons that CVS was an attractive choice include: 

• 

• 

• 

• 

It works on virtually all UNIX platforms and many PCs; 

It is publicly available and free; 

CVS is a state-of-the-art system, constantly being improved; and 

MM5 codes are managed by CVS, and MM5  is a primary meteorology model for the 
CMAQ system. 

From the UNIX man pages (online manual): 

CVS is a front end to the Revision Control System (RCS) that extends the notion 
of revision control from a collection of files  in a single directory to a hierarchical 
collection of directories consisting of revision controlled files.  These directories 
and files can be combined together to  form a software release.  CVS provides the 
functions necessary to manage these software releases and to control the 
concurrent editing of source files among multiple software developers [emphasis 
added]. 

The Revision Control System (RCS) manages multiple revisions of files.  RCS 
automates the storing, retrieval, logging, identification, and merging of revisions. 

Another widely used source code management system is the Source Code Control System 
(SCCS), which is usually distributed with UNIX systems.  The main note is that RCS and SCCS 
act on files only, while CVS operates on projects.  Working with entire projects works better and 
easier with CVS.  Multi-developer project development works better with CVS as well.  CVS 
supports a client/server mode of operation that can be very useful.  CVS also allows 
customization by adding hooks so that local scripts or programs can be called when executing 
various CVS commands.  This can be useful to force naming conventions of tags, or reference to 
bug-tracking software, etc.  Thus CVS adds power and features that are attractive for the 
ModelsS-3/CMAQ system. 

18.4.3  The Repository 

The repository structure, that is, the UNIX directory hierarchy, follows the class/module 

111111  grganization discussed above in Section 18.2.  The repository is actually divided into many 

repositories, one for each generic model.  This division makes it easier to  maintain the 
class/module organization that is important for the model building operation described below in 
Section 18.5. 

CVS allows for the use of a "modules" file3
,  which enables a user to easily check out or extract a 
complete CMAQ module.  For example, a module might be checked out by a user to  make code 
modifications, and complete modules are checked out during the model building operation. 

''11111, 

'11111111111'' 

,11 

'1111111111111 

''11111111 1..i: 

,;, 

''1111111' 

1111111111111111111111111!""' 

.""111111111111111111111 

'lllilllllllh 

,,llllllllllll"!I 

'1:..111111111 

11111111' 

'111111111, 

1111 

II' 

ill" 

.111:'':111111• 

1111111111111111111111111 

'1111::':111111111:· 

" 

II 

"llllllllllr"'lllll"' 

11° 

, 

·:,,,11 

''1 

11111111111111' 

The following shows a symbolic CVS UNIX directory tree that represents the current structure 
for the CCTM: 

111111111 

'111111111111' 

111 

,,,,,::1111111111111 

,1111111 

:1• 

I 

1111 

'I 

1
':11111111 

111

111111::,,,,,'''. 

ii111lllllllllllll 

'111111111,,' 

:1111111111, 

"111111111 

,lllll',:11111
• 
1
1111111 "' ~ : : 

,, 

I I ' 

, I ' 

"'',111111111111111111111" 

111'''111111111111 

,,"'''::1111111111111111111: 

"" 

,1111111111 

'!Iii, 

II 

,111111[ 

,, 

"' 

111111111111'''11' 

llll"',,,;,,111111: 

,;11: 

'" 

11111111""11, 

3The terminology is unfortunate.  The CVS modules file  has no  intrinsic relationship with the 
CMAQ classes/module design implementation. 

18-12 

... CCTM  -+->  CVSROOT  ---+->  CVS  administrative  files 

I 
\->  src  -------+->  driver  --+-------> ctm  ------+->  RCS  files 

I 

EPA/600/R-99/03 0 

I 
+-> 
I 
I 
I 
+-> 

hadv 

---+---> hadv _noop  ---+-> 
+--->  hbot  --------+-> 
+--->  hppm  --------+-> 

RCS  files 
RCS  files 
RCS  files 

vadv 

---+---> vadv _noop  ---+-> 
+--->  vbot  --------+-> 
+--->  vppm  --------+-> 

RCS  files 
RCS  files 
RCS  files 

+-> 

vdiff 

--+---> vdiff  no op  --+-> 
+--->eddy  --------+-> 

-

RCS  files 
RCS  files 

+-> 

chem 

---+---> chem_noop  ---+-> 
+--->  smvgear  -----+-> 
+--->  qssa  --------+-> 

RCS  files 
RCS  files 
RCS  files 

+-> 

phot 

---+---> phot  no op  ---+-> 
+--->  phot  --------+-> 

-

RCS  files 
RCS  files 

+-> 

aero 

---+--->  aero _noop  ---+-> 
+--->aero  --------+-> 

RCS  files 
RCS  files 

+->  couple  -+--->  gencoor  -----+->  RCS  files 

+->  cloud  --+---> cloud  noop  --+->  RCS  files 
+--->  cloud-radm  --+->RCS  files 

+->  procan  -+--->  pa  ----------+->  RCS  files 

+->  hdiff  --+--->  hdiff_noop  --+->  RCS  files 
+--->  unif  --------+->  RCS  files 

+->  init  ---+--->  init  --------+->  RCS  files 

+->  util  ---+--->  util  --------+->  RCS  files 

+->  aero_depv  -+->  RCS  files 

+->  aero_depv  +->  aero_depv_noop  +->  RCS  files 
I 
I 
+->  adjcon  -+--->  adjcon_noop  -+->  RCS  files 
I 
\--->  denrate  -----+->  RCS  files 
I 
\->  ping  ---+--->  ping_noop  ---+->  RCS  files 
+--->  ping_qssa  ---+->  RCS  files 
\--->  ping_smvgear  +->  RCS  files 

The symbolic tree is shown relative to the subdirectory in the repository named for the CCTM 
model.  Similar trees exist for each of the generic models.  The RCS  files are the revision control 
history files that contain the change tables to reconstruct the actual source code according to a 
specific revision identifier.  Also note that the tree closely follows the organization of classes and 
modules for the CCTM described in Table  18-1, and contains alternate modules within the 
classes.  In particular, most classes contain a "no-operation" (_noop) module that allows a user to 
essentially turn off that particular science process modeling.  This is useful, for example in 
debugging, where rapid turn-around is important, and a computationally demanding module that 
is not needed can be bypassed. 

18.5  How a Model is Constructed 

18.5.1  Object Oriented Concepts 

To make the Models-3/CMAQ system robust and flexible, object oriented concepts were 
incorporated into the design of the CMAQ system.  Incorporating these ideas into the design 
helps avoid introducing errors when code modifications are necessary.  Additionally, the system 
is capable of easy and efficient modification, allowing the user to quickly make models for 
different applications. 

The implementation language for CMAQ is Fortran 77, which imposes limits on how far one can 
go in terms of object oriented design.  In particular, since Fortran is a static language, objects 
cannot be instantiated dynamically; they must be declared explicitly in the source code to  be 
created at compile time.  However, to encourage a user community that will be contributing code 
for future enhancements, every attempt has been made to adhere to  the Fortran 77 standard.  In 
the future, the use of other implementation languages such as Fortran 90 will be considered. 

18.5.2  Global Name Table Data 

In order to implement modularity and data-independence, we have employed design ideas that 
draw heavily from the object-oriented concept of inheritance and code re-use.  The data 
structures in the codes that deal with the domain sizes, chemical mechanism, I/O API, logical file 
names, general constants, and CGRID pointers are determined by Fortran declarations in data 
and parameter statements that are created through the Models-3 system.  These data structures 
pertain to a particular application (domain, mechanism, etc.) and are meant to apply globally, not 
only to one particular CCTM through all its subroutines, but also to all the models that supply 
data to the CCTM for that application.  These data structures are contained in Fortran INCLUDE 
files, which are essentially header files,  included in the declaration sections near the top of the 
Fortran code source files.  The inclusion of these source files is made automatic by using a 
generic string that represents the include file and which is parsed and expanded to  the actual 
include file during a pre-processing stage in the compilation.  The Fortran global include files 
contain name tables that define: 

1. 

2. 

3. 

4. 

5. 

The computational grid domain; 

The chemical mechanism; 

The 110 A.PI  interface, including logical file names; 

The global modeling constants; and 

Other constants or parameters that apply across the model. 

q,,::,::1

;

111111

I  h 

,::,,  In q'~Cier to effect th~'impleme~t~tion of the i~clude files int~ the c~de) a special compiling 
system; ni3bld, has been developed (3], which reads a configuration file that, based on the 
application, completely determines the model executable to be built.  The ASCII configuration 
file can be generated either by the Models-3  system or by the user folloWing a few, simple 

, , ,  syntactical rules illustrated below.  For additional details, the reader is referred to Chapter 15. 

In addition to the global include files, the configuration file contains module commands that tell 
m3bld to extract the codes for that module from the model code repository for compilation. 

18.5.3  Build Template 

The following exhibit is an example of a configuration file that m3bld would use to  build a 
model executable: (The numerals at the left-hand margin are labels for the legend and are not 
part of the configuration file.  The"//" represents a non-parsed, comment line.) 

Example Configuration File 
(1)  model  TUTO_r4y; 

(2) 
(2) 
(2) 

cpp_flags 
f77_flags 
link_flags 

"  -Demis  vdif 
"  -e  -fast  -04  -xtarget=ultra2  -xcache=16/32/1:1024/64/1" 
"  -e  -fast  -04  -xtarget=ultra2"  ; 

"  ; 

(3) 

libraries 

"-L/home/models3/tools/IOAPI/release/m3io/lib/SunOS5  -lm3io. 
-L/home/models3/tools/netCDF/SunOS5  -lnetcdf"  ; 

(4)  global  verbose; 

(5) 
(5) 
(5) 
(5) 
(5) 
(5) 
(5) 
(5) 
(5) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 

include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 

-

-
-

SUBST  BLKPRM 
SUBST  CONST 
SUBST  FILES  ID 
SUBST  EMPR  VD 
SUBST  EMPR  CH 
SUBST  IOPARMS 
SUBST  IOFDESC 
SUBST  IODECL 
SUBST  XSTAT 
SUBST  COORD  ID 
-
SUBST  HGRD  ID 
SUBST  VGRD  ID 
SUBST  RXCMMN 
SUBST  RXDATA 
SUBST  GC  SPC 
SUBST  GC  EMIS 
SUBST  GC  ICBC 
SUBST  GC  DIFF 
SUBST  GC  DDEP 
SUBST  GC  DEPV 
SUBST  GC  ADV 
SUBST  GC  CONC 
SUBST  GC  G2AE 
SUBST_GC_G2AQ 
SUBST  GC  SCAV 
SUBST  GC  WDEP 
SUBST  AE  SPC 
SUBST  AE  EMIS 
SUBST  AE  ICBC 
SUBST  AE  DIFF 
SUBST  AE  DDEP 

/work/rep/include/release/BLKPRM_500.EXT; 
/work/rep/include/release/CONST3_RADM.EXT; 
/work/rep/include/release/FILES_CTM.EXT; 
. 
/work/rep/include/release/EMISPRM.vdif.EXT; 
/work/rep/include/release/EMISPRM.chem.EXT; 
/work/rep/include/release/PARMS3.EXT; 
/work/rep/include/release/FDESC3.EXT; 
/work/rep/include/release/IODECL3.EXT; 
/work/rep/include/release/XSTAT3.EXT; 
/work/yoj/tgt/BLD/COORD.EXT; 
/work/yoj/tgt/BLD/HGRD.EXT; 
/work/yoj/tgt/BLD/VGRD.EXT; 
/work/yoj/tgt/BLD/RXCM.EXT; 
/work/yoj/tgt/BLD/RXDT.EXT; 
/work/yoj/tgt/BLD/GC_SPC.EXT; 
/work/yoj/tgt/BLD/GC_EMIS.EXT; 
/work/yoj/tgt/BLD/GC_ICBC.EXT; 
/work/yoj/tgt/BLD/GC_DIFF.EXT; 
/work/yoj/tgt/BLD/GC_DDEP.EXT; 
/work/yoj/tgt/BLD/GC_DEPV.EXT; 
/work/yoj/tgt/BLD/GC_ADV.EXT; 
/work/yoj/tgt/BLD/GC_CONC.EXT; 
/work/yoj/tgt/BLD/GC_G2AE.EXT; 
/work/yoj/tgt/BLD/GC_G2AQ.EXT; 
/work/yoj/tgt/BLD/GC_SCAV.EXT; 
/work/yoj/tgt/BLD/GC_WDEP.EXT; 
/work/yoj/tgt/BLD/AE_SPC.EXT; 
/work/yoj/tgt/BLD/AE_EMIS.EXT; 
/work/yoj/tgt/BLD/AE_ICBC.EXT; 
/work/yoj/tgt/BLD/AE_DIFF.EXT; 
/work/yoj/tgt/BLD/AE_DDEP.EXT; 

Simple Configuration File 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
( 6) 
(6) 
(6) 
(6) 
(6) 
<6) 
<6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 
(6) 

SUBST  AE  DEPV 
SUBST_AE_ADV 
SUBST_AE_CONC 
SUBST_AE_A2AQ 
SUBST  AE  SCAV 
SUBST  AE  WDEP 
SUBST_NR_SPC 
SUBST_NR_EMIS 
SUBST  NR  ICBC 
SUBST  NR  DIFF 
SUBST  NR  DDEP 
SUBST  NR  DEPV 
SUBST  NR  ADV 
SUBST  NR  N2AE 
SUBST_NR_N2AQ 
SUBST  NR  SCAV 
SUBST_NR_WDEP 
SUBST  TR  SPC 
SUBST  TR  EMIS 
SUBST  TR  ICBC 
SUBST_TR_DIFF 
SUBST  TR  DDEP 
SUBST_TR_DEPV 
SUBST  TR  ADV 
SUBST_TR_T2AQ 
SUBST_TR_SCAV 
SUBST  TR  WDEP 

lworklyojltgtlBLD/AE_DEPV.EXT; 
lworklyojltgtlBLD/AE_ADV.EXT; 
lworklyojltgtlBLDIAE_CONC.EXT; 
lworklyojltgt/BLD/AE_A2AQ.EXT; 
lworklyojltgtlBLD/AE_SCAV.EXT; 
/worklyojltgtlBLD/AE_WDEP.EXT; 
lworklyojltgtlBLD/NR_SPC.EXT; 
lworklyojltgtlBLDINR_EMIS.EXT; 
lworklyojltgtlBLDINR_ICBC.EXT; 
lworklyojltgt/BLDINR_DIFF.EXT; 
lworklyojltgt/BLDINR_DDEP.EXT; 
lworklyojltgtlBLDINR_DEPV.EXT; 
lworklyojltgt/BLD/NR_ADV.EXT; 
lworklyojltgt1BLD/NR_N2AE.EXT; 
lworklyojltgtlBLD/NR_N2AQ.EXT; 
lworklyoj/tgtlBLD/NR_SCAV.EXT; 
lworklyojltgt/BLDINR_WDEP.EXT; 
lworklyojltgtlBLD/TR_SPC.EXT; 
lworklyojltgtlBLD/TR_EMIS.EXT; 
lworklyojltgtlBLDITR_ICBC.EXT; 
lworklyojltgtlBLD/TR_DIFF.EXT; 
lworklyoj/tgt/BLDITR_DDEP.EXT; 
lworklyojltgtlBLDITR_DEPV.EXT; 
lworklyojltgtlBLD/TR_ADV.EXT; 
lwork/yojltgtlBLD/TR_T2AQ.EXT; 
lworklyojltgtlBLD/TR_SCAV.EXT; 
lworklyojltgtlBLDITR_WDEP.EXT; 

include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
include 
II  Process  Analysis  I  Integrated  Reaction  Rates  processing 
include 
include 
include 

lworklyojltgtlBLD/PA_CTL.EXT; 
lworklyojltgtlBLDIPA_CMN.EXT; 
lworklyojltgtlBLDIPA_DAT.EXT; 

(6) 
(6) 
(6) 

,,1, 

-

SUBST  PACTL  ID 
SUBST_PACMN_ID 
SUBST  PADAT  ID 

-

-

-

(7)  module  ctm  release  ; 

(7)  module  init  release  ; 

II  options  are  denrate  and  adjcon_noop 

(7)  w.odule  denrate  release 

(7)  module  gencoor  release 

II  options  are  hbot  and  hadv_noop 
module  hppm  release  ; 

Iii'' 

(7) 

II options  are  vbot  and  vadv_noop 

(7) 

module  vppm  release  ; 

II  options  are  phot  and  phot_noop 

(7) 

module  phot  release  ; 

:, 

I'' 

II  options  are  ping_qssa,  ping_smvgear  and  ping_noop 

1111111111111 

18-16 

Example Corifiguration File 

(7)  module  ping_qssa  release 

II  options  are  qssa,  smvgear  and  chem_noop 

(7)  module  qssa  release 

EPA/600/R-99/030 

II  aerosols 

(7)  module  aero  release  ; 

II  aerosol  dep  vel 

(7)  module  aero  depv  release 

II  options  are  eddy  and  vdiff_noop 

(7)  module  eddy  release 

II  options  are  canst  and  hdiff_noop 

(7)  module  unif  release 

II  options  are  cloud_radm  and  cloud_noop 

(7)  module  cloud_radm  release  ; 

II  options  are  pa  and  pa_noop,  which  requires  the  replacment  of  the  three 
II  global  include  files  with  their  pa_noop  counterparts 

(7)  module  pa  release  i 

(7)  module  util  release 

-

IIO  AP!  and  netCDF  object  libraries  to  be  linked.  The  format  is 

- The  C  pre-processor,  Fortran  compiler  and  link  flags.  The  flags 

- Model  name  definition.  This  string  is  what  the  model  executable  will 

Legend: 
(1) 
be  named. 
(2) 
specified  indicate  that  the  model  is  to  be  compiled  with  the  option  to  input 
emissions  in  the  vertical  diffusion  processing,  and  to  compile  and  link  on  a 
Sun  Spare,  Ultra-30  workstation. 
(3) 
virtually  identical  to  that  of  a  UNIX  make  command. 
(4) 
Other  options  are: 
compile_all  -
clean_up 
no_compile 
no  link 

force  compile,  even  if  object  files  are  current 
remove  all  source  files  upon  successful  completion 
do  everything  except  compile 
do  everything  except  link 
compile  and  link  in  one  step 
checks  config  file  syntax 
show  requested  commands  but  doesn't  execute  them 

- m3bld  flag:  one  of  many  options.  verbose  indicates  report  all  actions. 

• 
• 
• 
• 
•  one_step 
•  parse_only 
show_only 
• 
(5), 
(6) 
include 

- global  include  files.  The  syntax  is: 
internal-string 

full-path-name. 

Every  routine  that  requires  a  specific  include  file  must  contain  a  Fortran 
include  statement  using  the  internal-string  (see  Section  18.3  for  examples) 

(5) 

- "fixed"  global  include  files.  The  fixed  include  files  have  been 
constructed  outside  of  the  Models-3  framework  and  contain  global  data  not 
directly  related  to  the  domain  or  chemical  mechanism  such  as  modeling 

- The  Models-3  framework  automatically  generates  all  the  include  files 

Example Configuration File 
cC:~stants  and  file  logical  names. 
(6) 
labeled  (6)  .  The  user  supplies 
Manager  that  determines  the  data  in  these  include  files.  These  data  define 
the  complete  problem  domain  for  a  particular  CMAQ  application.  See  Chapter 
15  for  a  complete  discussion. 
(7) 
optional  revision  flag  to  select  a  particular  module  version.  The  syntax  is: 
module  module  name 

information  through  the  Models-3  Science 

- The  module  name  to  extract  from  the  model  code  repository  and  an 

revision  flag. 

For additional information on this implementation, the reader is referred to the Model Building 
Tool described in Fine et al.  [3]. 

18.6  How a Model is Executed 

In order to run a model executable, various UNIX environment variables must be set in the shell 
that invokes the execute command.  Generally, these involve the modeling scenario start date and 
time, the run duration, the output time step interval, various internal code flags that differ among 
the models, and all the input and output logical, or symbolic file names (see Section 15.4.3  in 
Chapter 15).  There are various ways by which external file  names can be referenced in the code, 
but the most general across all  UNIX platforms is to link them by means of environment 
variables.  There are I/O API utility functions that allow a user easy access to  these variables 
within the code, making such accesses generic and portable. 

A:11nt!!li~i9I'),a~[~flt~!~e ~J:!at i .. ~ provided through the J/O  API is to declare a file "volatile" by 
appending a -v flag in the shell's declaration for the environment variable.  By doing this, the 1/0 
A.Pl will  cause the netCDF file to  update (sync) its disk copy after every write and thereby 
upcfat~ il1e riciCbFhcader:·  Otlierwise, netCDF file headers are not updated until  the files are 
closed.  This is useful, for example, to allow a user to  analyze an open netCDF file  using 
visualization tools while the model is executing.  It is also useful  in case of a system crash.  A 
CCTi\.1  model can be restarted at the scenario time step after the last successful write using the 
aborted output file as the input initial data. 

18.7  Using the Models-3 Framework 

The Modcls-3  framework simplifies the model building and model execution tasks, especially if 
key objects have been pre-defined.  Thus if a user is conducting a study that uses the same 
domain or chemical mechanism, but uses different code options, e.g., then rebuilding and re(cid:173)
executing using the framework makes conducting the study very straightforward.  Other 
examples where the fran1ework greatly simplifies operational tasks would be where a model is to 
be applied on different domains, or for control strategies in which the only thing that changes are 
thi: ..  ~1!1issic:ms input files.  A knowledgeable user is still able to  build and execute model 
?PPlications outside the framework.  In fact, one can easily run  model applications using 
executables and UNIX scripts generated by the framework. 

In general, in order to build a model within the framework, the coordinate system, horizontal grid 
and vertical layer definitions, and chemical mechanism must be specified.  This is done within 
the Models-3  Science Manager component.  If objects required for a particular application, such 
as a coordinate system specification, are already defined, then the user needs only to select those 
objects.  The Configuration File Manager, an additional subcomponent within the Science 
Manager, allows a user to  control the module definitions and fixed  4  include files that go into the 
configuration file used to  build the model.  Also the user can specify compiling and linking flags 
related to a specific platform on which the model is to  be executed. 

With all  required Models-3  objects defined, the task of building a model  is made simple using 
the Models-3 component, Model Builder.  It's important to note that as users develop models, the 
set of objects will grow to become a rich pool from  which to select for a particular application. 
The Models-3 framework was designed to  allow extensive re-use of objects.  Model Builder 
generates all the "non-fixed" include files,  then builds a model, linking in the include files 
including the user-specified fixed ones.  See the section  above for the include file  definitions. 
Perhaps one of the most useful features of the Models-3  framework  is the ability to graphically 
set up and execute single or multiple model executions with automatic data registration.  This 
allows a user to carefully control the order of model executions and the management of all the 
input and output data.  The Models-3  component that performs this function  is the Study Planner. 
Study Planner allows a user to specify input data sets, executables, and UNIX environment 
variables, and to initiate (possibly multiple) model runs. 

18.8  Conformant Code 

18.8.1  Thin Interface 

As mentioned above in section  18.5.1  , the Models-3/CMAQ system was designed to  be robust 
and flexible with respect to the interchange of modules and the elimination of cross-module data 
dependencies.  Consequently, the concept of a "thin interface" has been employed in the design, 
which applies principally to the class-drivers.  At the least, the thin interface implementation 
implies the following requirements: 

• 

• 

• 

Eliminate global memory references (across modules).  This implies no common blocks 
across modules, no hidden data paths, no "back doors"; 

Each module reads and interpolates its required data independently. The I/O API helps to 
ensure this kind of data independence; and 

Standardized argument list (CGRJD, Date, Time, TimeStep) for the class-driver, as 
described in the section above. 

These requirements attempt to  incorporate the object-oriented idea of encapsulation in the 
Models-3/CMAQ design.  The following quotation is  from  Rumbaugh et al.  [7]: 

Encapsulation (also information hiding) consists of separating the external aspects 
of an object, which are accessible to  other objects, from  the internal 

4not generated by the framework 

implementation details of the object, which are hidden from other objects. 
Encapsulation prevents a program from becoming so interdependent that a small 
change has massive ripple effects.  The implementation of an object can be 
changed without affecting the applications that use it  [emphasis added]. 

The encapsulation design makes the CMAQ system safer and enables the transaction processing, 
plug&play capability.  This design also makes it easier for a user to  trace data and usage within a 
module, particularly at the class-driver level. 

18.8.2  Coding Rules 

In order to maintain the object oriented concepts implemented in the CMAQ system design, we 
have established a small set of coding rules that apply for those that develop CMAQ science and 
affect the low-level design of the models.  We have developed standards to control data 
...  dependencies at the class-driver level, but we have not propagated these coding standards to the 
sub-module level. 

1. 

2. 

3. 

4. 

S. 

6. 

7. 

The models are generally coded in Fortran (Fortran-77 conventions are used).  It is 
possible to link in subroutines written in the C language, although this has not been done 
within the current CMAQ implementation. 

The modules must be controlled ~y a top-level class-driver routine, whose calling 
arguments must be the computational concentration grid array, CORID, the current 
scenario data and time, and the controlling time step vector.  See the section above. 

The class-driver is also responsible for any temporal integration required within the 
module.  (The time steps for process integration at the module level are usually shorter 
than those of the CCTM synchronization time step.) 

Any reads and writes for the module should be done at the level of the class-driver 
routine.  Although not absolutely necessary, this is strongly suggested because it is 
usually much easier to  control the timing of the data accesses at the highest level of the 
module where the current scenario date and time are known. 

Use the IMPLICIT NONE Fortran declaration to maintain some control on typographic 
errors and undefined variables.  Although not standard Fortran-77, the use ofIMPLICIT 
NONE forces the developer to declare all  internal variables. 

Use the global include files for domain definitions, chemical mechanism data, and other 
data where available. 

Use the 110 API for external data references where appropriate.  For an illustration of 
these rules, the reader is referred to  the code template below. 

At the sub-module level there are no strict 1/0 or coding standards.  Here it is envisioned that 
individual researchers/programmers use their own coding styles for their algorithms.  However 
U1e  following suggestions are offered to facilitate the potential incorporation of a module into the 
CMAQ system: 

It is expected that MKS units are used for input and output variables, as these units have 
been standardized throughout the CMAQ system. Within a sub-module subroutine 
whatever units are most convenient can be used.  However, the developer must be 
responsible for any unit conversions to MKS for input and output, and thus avoid any 
potential errors. 

For efficiency and performance considerations, operations may need to be done on 
groups of grid cells (a block of cells) at a time.  If there are N cells in the block and the 
entire domain contains M cells, then the entire domain can be decomposed into MIN 
blocks.  We have used N=500.  For operations in the horizontal (x,y), the cell constraint 
becomes Xx Y ~ N, where X=  number of cells in the x-direction, and  Y=  number of cells 
in they-direction. For operations in both the horizontal and vertical, the constraint 
becomes Xx Y x Z ~ N, where Z=  number of cells in the z-direction. There may be some 
operations, such as for some horizontal advection schemes, where this decomposition 
into blocks becomes more difficult or impossible. 

18.8.3  Science Process Code Template 

The following demonstrates what a science process class-driver Fortran 77 subroutine might look 
like.  We recommend that a code developer follows this template, where appropriate, to get 
maximum benefit from the design concepts implemented in the Models-3/CMAQ system.  This 
template is generic and attempts to show most, if not all  the features available.  Some class(cid:173)
drivers and most other sub-programs within a module may not have, nor require, most or any of 
these features.  (The numerals at the left-hand margin are for the legend and are not part of the 
text, and the text within"<>" indicates code not included.) 

CGRID,  JDATE,  JTIME,  TSTEP  ) 

Example of Science Process Class-driver 
SUBROUTINE  VDIFF  SUBST_GRID_ID 
1) 
2)  c------------------------------------
2)  C  Function: 
2) 
2)  c  Preconditions: 
2) 
2)  C  Subroutines  and  Functions  Called: 
2) 
2)  C  Revision  History: 
2)  c------------------------------------
3) 

IMPLICIT  NONE 

4) 
4) 

5) 
5) 
5) 
5) 
5) 

5) 
5) 
5) 
5) 

5) 
5) 
5) 

INCLUDE 
INCLUDE 

SUBST_HGRD_ID 
SUBST_VGRD_ID 

horizontal  dimensioning  parameters 
vertical  dimensioning  parameters 

INCLUDE 
INCLUDE 
INCLUDE 
INCLUDE 
INCLUDE 

SUBST_GC_SPC 
SUBST_GC_EMIS 
SUBST_GC_DEPV 
SUBST_GC_DDEP 
SUBST_GC_DIFF 

gas  chemistry  species  table 
gas  chem  emis  surrogate  names  and  map  table 

I  gas  chem  dep  vel  surrogate  names  and  map  table 
I  gas  chem  dry  dep  species  and  map  table 

gas  chem  diffusion  species  and  map  table 

INCLUDE 
INCLUDE 
INCLUDE 
INCLUDE 

SUBST  AE  SPC 
SUBST  AE  DEPV 
SUBST  AE  DDEP 
SUBST  AE  DIFF 

aerosol  species  table 
aerosol  dep  vel  surrogate  names  and  map  table 
aerosol  dry  dep  species  and  map  table 
aerosol  diffusion  species  and  map  table 

INCLUDE 
INCLUDE 
INCLUDE 

SUBST_NR_SPC 
SUBST_NR_EMIS 
SUBST_NR_DEPV 

non-reactive  species  table 
non-react  emis  surrogate  names  and  map  table 

I  non-react  dep  vel  surrogate  names  and  map  table 

18-21 

Example of Science Process Class-driver 
(  5) 
(  SI 

INCLUDE  SUBST_NR_DDEP 
INCLUDE  SUBST_NR_DIPF 

non-react  dry  dep  species  and  map  table 
non-react  diffusion  species  and  map  table 

"""""' 

''" 

111111111, 

' '

5) 
SI 
(  5) 
(  5) 
I  SI 

6) 
(  6) 
(  61 
(  6) 
(  61 
(  7) 
(  71 
(  71 
(  71 
(  ?l 
(  71 
(  7) 
(  4) 

(  8) 
(  •> 
(  81 
(  •I 
(  al 
(  8) 
(  81 
(  81 
iJ> 
( 

9) 
(  !I) 
(101 
(10) 
(10) 
(10) 

(11) 
(lll 
(1:21 
(1:2) 
(13) 
(13) 
(13) 
(13) 
(13) 
(13) 
(13) 
(13) 
(13) 
(lJ) 
(13) 
(13) 
(13) 
(13) 
(13) 
(13) 
(13) 
(13) 
(13) 
(13) 

INCLUDE  SUBST_TR_SPC 
INCLUDE  SUBST_TR_EMIS 
INCLUDE  SUBST_TR_DEPV 
INCLUDE  SUBST_TR_DDEP 
INCLUDE  SUBST_TR_DIFF 

tracer  species  table 
tracer  emis  surrogate  names  and  map  table 
tracer  dep  vel  surrogate  names  and  map  table 
tracer  dry  dep  species  and  map  table 
tracer  diffusion  species  and  map  table 

lli!def  emis_vdif 

INCLUDE  SUBST  EMPR  VD 

emissions  processing  in  vdif 

llel•e 

INCLUDE  SUBST_EMPR_CH 

emissions  processing  in  chem 

Hendif 

INCLUDE  SUBST_PACTL_ID 
IN!=LUDE  SUBST_CONST 
INCLUDE  SUBST_FILES_ID 
INCL.UDE  SUBST_IOPARMS 
INCLUDE  SUBST_IOFDESC 
INCLUDE  SUBST_IODECL 
INCLUDE  SUBST_XSTAT 
INCLUDE  SUBST_COORD_ID 

PA  control  parameters 
constants 
file  name  parameters 
I/O  parameters  definitions 
file  header  data  structure 
I/O  definitions  and  declarations 
M3EXIT  status  codes 
coordinate  and  domain  definitions  (req  IOPARMS) 

C  Arguments: 

REAL 
INTEGER 
INTEGER 
INTEGER 

C  Parameters: 

CGRID(  NCOLS,NROWS,NLAYS,*  ) 
JDATE 
JTIME 
TSTEP(  3  ) 

concentrations 
I  current  model  date,  coded  YYYYDDD 

current  model  time,  coded  HHMMSS 
time  step  vector  (HHMMSS) 
TSTEP(l) 
TSTEP(2) 
TSTEP(3) 

local  output  step 
sciproc  sync. 

z  advection  time  step 

step  (chem) 

< 

> 

C  External  Functions  not  previously  declared  in  IODECL3.EXT: 

INTEGER 
EXTERNAL 

SECSDIFF,  SEC2TIME,  TIME2SEC 
SECSDIFF,  SEC2TIME,  TIME2SEC 

C  File  variables: 

< 

> 

C  Local  variables: 

> 

< 
IF  (  FIRSTIME  )  THEN 

FIRSTIME  z 

.FALSE. 

LOGDE:V  •  INIT3 () 

C  Open  the  met  files: 

IF  ( 

.NOT.  OPEN3(  MET_CR0_3D,  FSREAD3,  PNAME) 

)  THEN 

XMSG  •  •could  not  open  '// MET_CR0_3D  //  •  file' 
CALL  M3EXIT(  PNAME,  JDATE,  JTIME,  XMSG,  XSTATl  ) 
END  IF 

<  open  other  met  files  > 

C  Open  Emissions  files: 

<  do  other  intialization  or  operations  that  need  be  done  only  once  > 

END  IF 

if  f irstime 

(14)  C  set  file  interpolation  to  middle  of  time  step 

1
':,,111111111,11111,,::111 

11111111111111 

111111111111111111111, 

11111111 

11111111111:' 


Example of Science Process Class-driver 
(14) 
(14) 
(14) 
(14) 
(14) 

JD ATE 
JTIME 
TIME2SEC(  TSTEP(  2  ) 

MDATE 
MTIME 
MSTEP 
CALL  NEXTIME 

(  MDATE,  MTIME,  SEC2TIME(  MSTEP  I  2  )  ) 

EP A/600/R-991030 

INTERP3(  MET_CR0_3D,  VNAME,  PNAME, 

MDATE,  MTIME,  NCOLS*NROWS*NLAYS, 
RRHOJ  )  )  THEN 

'Could  not  'interpolate  DENSA_J  from  •  II  MET_CR0_3D 

XMSG  = 
CALL  M3EXIT(  PNAME,  MDATE,  MTIME,  XMSG,  XSTATl  ) 
END  IF 

& 
& 

.NOT. 

'DENSA_J' 

VNAME  = 
IF  ( 

(15)  C  read&interpolate  met  data 
(15) 
(15) 
(15) 
(15) 
(15) 
(15) 
(15) 
(15) 
(15) 
(15) 
(15) 
(15) 
(15) 
(15) 
(15) 
(15)  C  read&interpolate  emissions 
(15) 
(15) 
(15) 
(15) 
(16) 

<  do  other  reads  > 

<  do  operations  > 

CALL  RDEMIS 

VDEMIS  ) 

& 

C  read&interpolate  deposition  velocities 

(  MDATE,  MTIME,  NCOLS,  NROWS,  EMISLYRS,  NEMIS,  EM_TRAC, 

IF  (LIPR)  CALL  PA_UPDATE_EMIS 

( 

'VDIF',  VDEMIS,  JDATE,  JTIME,  TSTEP  ) 

(17) 

(18) 
(18) 

(19) 

<  do  other  operations  > 

CALL  EDYINTB  SUBST_GRID  ID  (  EDDYV,  OT,  JDATE,  JTIME,  TSTEP(  2  )  ) 

DO  345  R  = 1,  NROWS 

DO  344  C  =  1,  NCOLS 

<  do  operations  > 

DO  301  N  = 1,  NSTEPS(  C,R  ) 

<  do  operations  > 

(19)  301 
(18)  344 
(18)  345 

CONTINUE 

CONTINUE 

CONTINUE 

end  time  steps  loop 
end  loop  on  col  C 
end  loop  on  row  R 

) 

)  THEN 

.GE.  TIME2SEC(  TSTEP(  1 

WSTEP  = WSTEP  +  TIME2SEC(  TSTEP(  2  )  ) 
IF  (  WSTEP 
) 

MDATE  = JDATE 
MTIME  = JTIME 
CALL  NEXTIME(  MDATE,  MTIME,  TSTEP(  2  )  ) 
WSTEP  = 0 

(20)  C  If  last  call  this  hour:  write  accumulated  depositions: 
(20) 
(20) 
(20) 
(20) 
(20) 
(20) 
(20) 
(20) 
(20) 
(20) 
(20) 
(20) 
(20) 
(20) 
(20) 
(20) 
(20) 
(20) 
(16) 

'Timestep  written  to',  CTM_DRY_DEP_l, 
'for  date  and  time',  MDATE,  MTIME 

XMSG  =  •could  not  write  •  II  CTM_DRY_DEP_l  II  •  file' 
CALL  M3EXIT(  PNAME,  MDATE,  MTIME,  XMSG,  XSTATl  ) 
END  IF 

IF  (LIPR)  CALL  PA  UPDATE  DDEP 

.NOT.  WRITE3(  CTM_DRY_DEP_l, 

WRITE(  LOGDEV, 

IF  ( 

& 
& 

( 

& 

ALLVAR3,  MDATE,  MTIME,  DDEP  )  )  THEN 

'(  /5X,  3(  A,:,  lX  ),  IS,":",  I6.6  )') 

'VDIF',  DDEP,  JDATE,  JTIME,  TSTEP  ) 

Example of Science Process Class-driver 
(20) 
(20) 
(20) 
c2o> 

<  do  other  operations  > 

END  IF 

( 21) 
(21) 

RETURN 
END 

l'..egend: 

is  used  for  developing  run-time  nesting  applications, 

- Domain  array  dimensioning  and  looping  global  variables,  e.g.  NCOLS,  NROWS,  NLAYS. 

IMPLICIT  NONE  will  catch  typo's  and  other  undeclared 

(  1)  - Class-driver  subroutine  internal  name.  Note  the  calling  argument  list.  It  is  fixed  for 
claaa-drivers.  The  string  ~  SUBST_GRID_IDN 
which  has  not  been  fully  implemented  in  the  current  version.  By  specifying  subroutine  names 
aaaociated  with  a  particular  nest  and  using  corresponding  domain  include  files  for  that 
aubroutine,  it  is  possible  to  set  up  a  multi-nest  application.  The  details  have  not  been 
diacuaaed  because  this  kind  of  nesting  has  not  been  implemented  in  the  current  release  of  the 
Hodela-3/o-tAQ  system. 
(  ·2)  - Header  com:nents.  Highly  recommended  for  internal  documentation. 
(  3)  - Highly  recommended  for  Fortran. 
variable  errors  at  compile  time. 
(  4) 
(  5)  - Chemical  mechanism  array  dimensioning  and  looping  global  variables. 
(  6)  - C  Preprocessor  flags  that  determine  which  emissions  control  dimensioning  and  looping 
variables  are  compiled. 
(  7) 
- Other  global  array  dimensioning  and  looping  global  variables  including  those  for  the  I/O 
API.  The  logical  variable  LIPR  is  defined  in  the  SUBST_PACTL_ID  include  file  for  use  at  lines 
labeled  (16)  . 
(  8)  - Declarations  for  the  argument  list  (standardized) . 
(  9) 
(10) 
(11) 
(12)  - Declarations  for  local  variables. 
(13)  - Code  section  for  subroutine  initialization  and  for  any  local  data  that  need  not  be  set  at 
every  entry  into  the  subroutine.  Such  data  would  require  a  SAVE  statement  in  the  declarations. 
For  example  FIRSTIME  is  initialized  to  .TRUE.  in  the  local  variables  section  (12). 
(14) 
(15) 
(16) 
function. 
(17) 
(18) 
(19) 
(20)  - Illustrates  writing  to  an  I/O  API  file  within  a  module. 
(21)  - Subroutine  end 

- Illustrates  using  an  I/O  API  function  to  set  file  interpolation  time. 
- Read  accesses  from  I/O  API  files  using  the  I/O  API  time-interpolation  function. 
- Call  to  process  analysis  routine  to  obtain  data  for  the  optional  integrated  process  rates 

- Declarations  and  PARAMETER  statements  for  local  Fortran  parameters. 
- Declarations  for  external  functions  not  previously  declared. 
- Declarations  for  arrays  to  hold  external  file  data. 

- Illustrates  call  to  another  science  process  within  the  module. 
- Main  computational  loop  over  the  horizontal  grid. 
- Time  step  loop  over  sub-synchronization  time  step  intervals. 

18.8.4  Robustness and Computational Efficiency 

Scientists, while working in their discipline, are generally not interested in the physical situations 
that don't significantly affect the phenomena they are studying and modeling.  Typically, the 
scientist works with box models, and uses specialized data to test hypotheses, parameterizations 
and numerical schemes.  The focus is to generate new results based on output from these 
relatively simple models.  In trying to integrate these codes into a more general grid model, 
,.  prbblems may arise when modeling simulations carry their formulations into regimes not 
explored in initial studies, therefore not properly accounted for in the grid model.  The scientist 
who is developing codes that may go into a grid model should be cognizant of these issues and 
should make every effort to make the codes robust. 

Another issue is code efficiency.  In order to handle the pathological situations, codes like these 
may become inefficient, spending many cycles dealing with the "off-problem" cases.  A good 
example is solving for the roots of a cubic polynomial whose coefficients, depending closely on 
the locally modeled physics, may range over broad values.  So, in addition to robustness, 
attention should be paid to code performance.  As the science improves in environmental 
modeling, this usually translates into increased computational complexity.  Although hardware 
performance is continually improving, it cannot keep pace with the computational demands of 
new science developments.  Not typically the main interest of the science developer, software 
performance is nevertheless a critical issue and must be addressed.  Otherwise scientifically 
sound models will not be of much general use if they take an inordinate amount of time to 
execute. 

18.9  Conclusion 

The CMAQ system has been designed and implemented in such a way that its integration into 
Models-3 allows access to the extensive functionality of the Models-3 framework.  Users can 
easily select important model options and can readily develop and execute models that apply to 
their requirements. 

We have integrated the CMAQ codes into the Models-3 system by following the basic set of 
guidelines and rules presented in this chapter. 

18.'10  References 

[ 1]  http://interactivate.com/public/cvswebsites/cvs _toc.html, http://www.cyclic.com/cvs/, and 
the UNIX man pages 

[2]  http://www.cs.cmu.edu/People/vaschelp/ Archiving/Res/, and the UNIX man pages 

[3]  Fine, S.  S., W.  T.  Smith, D.  Hwang, T.  L.  Turner,  1998: Improving model development 
with configuration management, IEEE Computational Science and Engineering, 5(1, Ja-Mr), 56-
65.  and http://envpro.ncsc.org/pub _files/fine l 998b.html 

[ 4]  http://sage.mcnc.org/products/ioapi/ 

[ 5)  ftp:! /ftp. uni data. ucar.edu/pub/netcdf/ 

[6]  EPA Third-Generation Air Quality Modeling System, Models-3 Volume 9b, User Manual, 
Appendices, June 1998, EPA-600/R-98/069(b) 

[7]  J.  Rumbaugh, M. Blaha, W.  Premerlani, F. Eddy, and W. Lorensen,  1991: Object-Oriented 
Modeling and Design, Prentice Hall 

[8]  Grell, G.  A., J.  Dudhia and D.R. Stauffer, 1994: A description of the fifth-generation Penn 
State/NCAR mesoscale model (MM5).  NCAR Technical Note,  NCAR/TN-398+STR, 122 pp. 
