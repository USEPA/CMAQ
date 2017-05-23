Chapter 2 
==============

MODELS-3 ARCHITECTURE: A UNIFYING FRAMEWORK FOR 

ENVIRONMENTAL MODELING AND ASSESSMENT 

EPAJ600JR-99J030 

Joan Novak• and Sharon. Leduc·· 
Atmospheric Modeling Division 

National Exposure Research Laboratory 
U.S. Environmental Protection Agency 

Research Triangle Park, NC 27711 

ABSTRACT 

The Models-3 framework and Community Multiscale Air Quality (CMAQ) model were designed 
and built to function together.  The CMAQ science is documented in the other chapters and this 
chapter is intended to provide science and technical details of the Models-3 framework which . 
integrates the CMAQ.  The Models-3 User Manual is available and useful for those who want 
operational details beyond the "big picture" provided in Chapter 2. 

This chapter present the various components of the Models-3 framework (Dataset Manager, 
Program Manager, Science Manager, Study Planner, Strategy Manager, Model Builder, Source 
Code Manager, Framework Administrator, and Tool Manager).  The Tool Manager provides 
access to third party applications for emissions processing, visualization, and analysis.  The 
Framework Administrator provides the system administrator access to system lists and 
administrative functions in Models-3. ·Each of the other components is provided for developing, 
executing, and managing CMAQ applications. 

. 

The computer architecture, the client-server design, the object-oriented data base management 
system, and the graphical user interface are also addressed in this chapter, along with the design 
features ofModels-3. 

·on assignment from the National Oceanic and Atmospheric Administration, U.S. Department of Commerce. 
Corresponding author address: Joan Novak, MD-80, Research Triangle Park, NC 27711.  Email: 
novak.joan@epamail.epa.gov 

.. On assignment from the National Oceanic and Atmospheric Administration, U.S. Department of Commerce. 

## 2.0  MODELS-3 ARCHITECTURE: A UNIFYING FRAMEWORK FOR ENVIRONMENTAL MODELING AND ASSESSMENT 

## 2.1 Introduction 

Models-3 is a flexible software system designed to simplify the development and use of 
environmental assessment and decision support tools for a wide range of applications from 
regulatory and policy analysis to understanding the interactions of atmospheric chemistry and 
physics. The initfal version ofModels-3 contains a Community Multiscale Air Quality (CMAQ) 
(Byun, et al.,  1997, and other chapters of this document) modeling system with capabilities for 
urban to regional scale air quality simulation of tropospheric ozone, acid deposition, visibility and 
fine particulate.  The Models-3 framework provides an interface between the user and operational 
models, between the scientist and developing models, and between ever changing hardware and 
software platforms. 

The concept of an integrated modeling and analysis framework (Novak, et al.,  1995; Dennis, et 
al., 1996) was formulated in response to  1) problems and duplication of effort involved  in 
maintaining the separate data pre-processing and post-processing software systems required for 
each model used to analyze different air quallty pollutants or scales, 2) the difficulties States and 
industry encountered using more complex modeling systems, and 3) the large. expense required to 
modify existing models to incorporate scientific advancements or to adapt the models to new 
domains.  Based on technology advances in the National High Performance Computing and 
Communications program, Models-3 was designed to overcome these limitations and to serve as a 
community framework for continual advancement and use of environmental assessment tools. 
Capabilities and design features of the Models-3 framework provide more flexibility in data 
handling, modeling, visualization and analysis. 

## 2.2 Overview of the Models-3 Framework 

The Models-3 framework contains components that assist I) the model user with air quality 
modeling studies and analysis of results, and  2)  the model developer with creating, testing, and 
performing comparative analysis of new versions of air quality models.  The major design goal is 
to simplify and integrate the development and use of complex environmental models, beginning 
with air quality and deposition models.  The function of each framework component is described 
below. 

### 2.2.1  Dataset  Manager 

Dataset Manager provides the user with the capability to register files for use with the modeling 
and analysis programs within the Models-3 framework. The registration process involves entering 
the location of the dataset (full path name) and metadata (information about the data such as 
spatial - temporal extent and resolution, source of data, time convention, units, etc.) into the 
Models-3 data base.  Models-3 follows the Federal Geospatial Metadata Standard for metadata 
content. The datasets may be located on any network-connected computer system  known to the  · 
Models-3 system installed at the user's site. Once a dataset is registered, the user can search for 
the dataset based on its metadata information, file type, etc. 

Dataset Manager allows the user to view the details of the selected dataset to ensure the correct 
one has been selected for use with an application. Dataset registration  eliminates the need for the 
user to type the entire path name each time the dataset is used.  Instead the user can highlight  the 
dataset  from a list of candidates that satisfy  the search criteria specified by the user. Models-3 
will automatically move selected data to the host where it is needed for a model execution. 
Dataset Manager also provides standard  capabilities such as deleting, copying, archiving, and 
restoring files and metadata. 

### 2.2.2  Program Manager 

Program Manager allows the user to register, update, and  search for executable programs and/or 
scripts to make them available for use in defining studies within the Study Planner component. 
During program registration, the user  enters characteristics of the program into the framework 
including descriptive information on program function, input requirements, output specifications, 
runtime environment variables, target architecture and operating system. Once the program or 
script is registered, this executable can be used in Study Planner to sequence a series of 
executions which may depend on previous executions for input data. The user can access and 
execute programs that are not registered.  However, registering programs ensures that all 
mandatory inputs have been specified and automatic naming and registration of output files is 
activated, to facilitate tracking output from numerous program executions.  Recommended model 
configurations for standard domains are preregistered in the system, eliminating the need for the 
typical user to deal with the details of program registration. 

### 2.2.3  Study Planner 

Study Planner allows the user to define a study and control the execution of its associated 
processes. A study is a collection of plans and properties that are necessary to describe and 
perform one or more environmental modeling analyses. A plan is a collection of information 
defining dataset and program interdependencies and the sequence of execution of one or more 
programs. Study Planner gathers much of its information from the Program Manager and Dataset 
Manager registration data. The relationship between a program (node) and its required and 
optional datasets (links) is user-defined through the process of constructing and annotating a 
graphical diagram with simple drag-arid-click mouse operations. Once a plan is constructed and its 
graphical diagram fully annotated with desired input datasets and options, the plan can be 
executed.  User specified program options are entered by editing program environment variables. 

Studies and associated plans are named entities that are saved in the system data base. Therefore, 
a typical user can start with an existing study plan provided by the model developer and simply 
change the dataset annotations by selecting, through a file browser, appropriate datasets needed 
for execution. The Study Planner provides capabilities to create new  studies, copy and modify 
existing studies, and delete existing studies. 

### 2.2.4  Strategy Manager 

Strategy Manager provides the capability to estimate point-, area-, and mobile-source emissions 
for future years and to determine the relative effectiveness of specified control scenarios. The user 
may adjust pollutant growth factors and emissions control factors to perform "what if' analyses 
for entire EPA regions, states, counties or for user-defined study areas.  By applying estimated 
yearly emission growth factors from the Emissions Growth and Assessment System, control 
efficiency, rule effectiveness, and rule penetration factors  to the EPA 1990 base year emissions 
inventory, the Strategy Manager provides estimates of future year (1991  - 2010) emissions for 
carbon monoxide, nitrogen oxide, particulate matter up to 10 microns, sulfur dioxide, and volatile 
organic compounds.  Strategy Manager is based on EPA's Multiple Projection System.  An input 
data processor is included to process the National Emissions Trend (NET) inventory (URL 
http://www.epa.gov/oar/oagpsD data format. 

### 2.2.5  Tool Manager 

Tool Manager provides access to third-party applications (tools) that are registered with the 
Models-3 framework. Third-party applications that are currently available include Models-3 
Emission Processing and Projection System (MEPPS), Vis5D, Text Editor, Package for Analysis 
and Visualization of Environmental Data (PA VE), SAS®, Arc/Info®, IBM Visualization Data 
Explorer™, DXDriver, and VisDriver. Tool Manager allows the user to add tools to the system 
that will help with the user's  work.  A file converter is included to convert between ASCII, SAS, 
and 1/0 API files. 

#### 2.2.5.1 Emissions Processing 

The majority of the emissions processing needed to support air quality model applications can be 
prepared through minor modifications to pre-specified emissions plans in the Study Planner. 
However, for those users who need total flexibility in specifying details for emissions preparation, 
the Models-3 Emissions and Projection System (MEPPS) (Benjey and Moghari, 1996) is available 
for interactive processing of emissions. MEPPS provides capabilities to  input and perform quality 
control on emission inventory data, and  reformat  and subset data for the user-specified modeling 
domain. The main emissions processing is performed by a significantly revised version of the 
Geocoded Emission Modeling and Projection (GEMAP) (Wilkinson and Emigh,  1994)  system 
which requires the user to have Arc/Info® and SAS® licenses for operation. This Emissions 
Processor (EMPRO)  transforms the annual county based emissions inventories into spatial and 
temporal resolutions that are  consistent with the target  model application, i.e. typically hourly 
gridded emissions for a selected domain. The processor also estimates mobile and biogenic 
emissions and performs chemical speciation consistent with the chemical mechanism in the target 
model.  Mobile emission factors are calculated using Mobile Sa; biogenic emissions are calculated 
using the Biogenic Emissions Inventory System (BEIS2).  Finally, according to a user-specified 
criteria, an output processor merges the emissions files to provide the 2-D gridded files and a 
point source stack emissions file required for the model execution. The user is offered numerous 
choices for interactively changing parameters and reviewing results in all of the emissions 
components. The global science specifications created by the user in Science Manager are 
accessible from within MEPPS to ensure consistency when desired. 

#### 2.2.5.2 Visualization and Analysis Tools 

Several freely available visualization tools are accessible from the Models-3 framework as well as 
commercial vis;ualization and statistics packages, such as IBM Visualization Data Explorer™ and 
the SAS®.  A graphical user interface component, VisDriver, provides an interface for finding and 
selecting Models-3 data files on local or remote hosts and launches  visualization applications with 
Vis5D or PA VE.  VisDriver performs requisite data conversions from the internal Models-3 data 
format to the data format required by these third party visualization packages. VisDriver also 
provides a data export capability  to such formats as Advanced Visualization System (A VS™), 
Flow Analysis Software Toolkit (FAST™), and an ASCII spreadsheet. On-line help is available to 
assist the user with these visualization tools 
(http ://www.epa.gov/asmdnerl/models3/vistutor/user/user-guide-OO .html). 

PAVE 

The Package for Analysis and Visualization for Environmental data (PA VE), developed at the 
North Carolina Supercomputing Center ofMCNC, allows the user to visualize 2-D multi-variate, 
gridded environmental data with smooth tile plots,3-D mesh plots, time series line and bar plots, 
vertical cross sections, wind vectors, and scatter plots.  More information can be found at 
http://www.iceis.mcnc.org/EDS Sf pave_doc/Pave.html. 

Vis 5D 

Vis5D, a public domain package developed at the University of Wisconsin Space Science and 
Engineering Center,  provides interactive visualization of large five-dimensional gridded datasets -
the data are real numbers at each point of a grid  that spans three space dimensions, one time 
dimension, and a dimension for enumerating multiple physical variables. Vis5D provides 
isosurfaces, contour line slices, colored slices, volume rendering of data in 3D, and rotation and 
animation of the 3D image in real time. There is also a feature for wind/trajectory tracing. More 
information can be found at http://www.ssec.wisc.edu/~billh/vis.html. 

IBM Visualization Data Explorer 

IBM Visualization Data Explorer™, a commercial visualization package, has been used to create 
custom visualizations to support diagnostic evaluation of the air quality models with visualization 
of multiple/nested grids, terrain following grids, grid cell time aggregate statistics such as 
maximum  ozone concentration and hours of non-compliance, comparison of measured aircraft 
data with model predictions along the path of flight, and detailed chemical analysis of the 
integrated reaction rates predicted over space and time in the air quality models. Other standard 
visualization capabilities for scalar and vector quantities, 2-D and 3-D graphs have also been 
integrated through the  DXDriver interface. 

### 2.2.6  Science Manager 

Critical model specifications typically found hardwired in air quality model codes, such as details 
of the horizontal coordinate system, specification of horizontal grid dimensions, vertical layers, 
chemical mechanism specification (reactions and rate constants), etc. are treated as globally 
shared information in the Models-3 framework.  Use of this global information enables more 
consistency throughout the system (i.e. emission, meteorological, and chemistry-transport 
components).  Detailed specifications for these key science components are entered by the user 
only once using Science Manager graphical user interfaces.  These specifications are saved as 
named entities in an object-oriented database accessible by all model components.  A typical user 
would access and modify previously defined Science Manager components to define a new 
modeling domain. 

The functionality of Science Manager, however, is targeted to the model developer to facilitate 
experimentation with new model components.  For example, extensive research is done on 
improving photochemical mechanism specifications.  One component of Science Manager enables 
the model developer to either edit an existing chemical mechanism  or import a new set of 
chemical reactions, specify chemical species, molecular weights, etc.  Both the Regional Acid 
Deposition Model, Version 2  (RADM-2)  and the Carbon Bond-4 mechanisms are available in 
the Models-3/CMAQ framework and the State Air Pollution Research Center (SAPRC) chemical 
mechanism is being modified to fit within this paradigm to facilitate comparative studies with 
these chemical mechanisms.  As long as the chemical species in any modified mechanisms are 
present in source emission profiles associated with Source Classification Codes (SCC), the 
chemical mechanism specification will propagate to the emission processing subsystem to enable 
generation of emission species consistent with the newly defined chemical mechanism. 

### 2.2.7  Model Builder 

The Models-3 framework facilitates the interchange of science components within a model, 
customization of the  chemistry mechanism, and changes in the modeling domain, horizontal grid 
resolution, and  vertical layering without the need for reprogramming. Modifying an existing 
model or building a new model is a two-step process.  First, the model developer must use 
Science Manager to specify the horizontal coordinate system, horizontal grid dimensions, vertical 
layers, chemical mechanism, and select science modules for each class (i.e. advection, diffusion , 
cloud process, etc.) of science process to be included in the new model.  Module selections are 
contained in a named c<;mfiguration file.  After the needed science components are specified, the 
model developer uses Model Builder to select from available science ·specifications and 
configuration files to create· a single custom model executable. 

### 2.2.8  Source Code Manager 

CMAQ source code is distributed. with the Models-3 framework  to provide the user flexibility to 
create a model for a user specified domain  or to allow a user to build a new ·executable with a 
selected chemistry mechanism. Model Builder uses Source Code Manager to access appropriate 
source code to transform the user's selections into· a working executable. Source Code Manager 
allows a user to  retrieve aversion ofa source code file; modify it, and return it to the code 
archive after the change has been tested. Source code should only be modified by knowledgeable 
model developers.  Source· Code Manager is based on Concurrent Versions System 
(CVS)/Revision Control System (RCS) public domain software for code configuration 
management and tracks the history and version numbers for all source code changes.  These 
version numbers become part of the history information associated with each output file; 

### 2.2.9  Framework Administrator 

The Framework Administrator subsystem menu allows the Models-3 system administrator to 
manipulate system lists and to access administrative functions of other Models-3 subsystems. 
System lists include users, hosts, device types, site IDs, file format types, compiler names, 
operating system names, platform names, and time zone names. Through this subsystem, only the 
authorized  system administrator can set access controls for users, add or delete entries from the 
system lists, and maintain the integrity of controlled files. 

2.3  Models-3 System Architecture 

-

The Models-3 framework is designed as a three-tier client-server architecture with an object-
oriented data base management system (OODBMS) ,ObjectStore™,  to store persistent data (i.e. 
studies, horizontal grid definitions, metadata containing pointers to physical dataset locations , 
etc.). When the user brings up the Models-3 graphical user interface on user's desktop system, the 
user becomes a "client" requesting services from a "server".  Several Models-3 servers act as 
"clients" requesting services from other Models-3 "servers".  The client and server can be on the 
same machine or on separate machines giving the system scalability by either adding more client 
workstations with only slight impact on performance, or moving to faster and larger server 
machines. Each of the subsystem functions described in Section 2.2 is implemented as a server 
and can be can run on different machines;  The Models-3  servers were initially developed on a 
Sun workstation using the Solaris 2.5.1  operating system (Models-3 Version 2.1).  Models-3  is 
being ported to a Sun· Solaris 2.6 operating system (Models-3 Version 2.2).  The next server to be 
released for Models-3 will be SGI (late 1998).  ·Models.:3 Version 3 (summer 1999) will allow 
server to be a PC with a Windows NT® operating system. 

This client-server architecture in conjunction with commercial cross-platform communications 
interface - ORBIXTM by Iona Technologies, Ltd. which is compliant with the Common Object 
Request Broker Architecture (CORBA) standard - and an OODBMS containing pointers to 
datasets distributed over the network enables transparent use of multiple computing platforms and 
access to data across the network.  Figure 2-1  illustrates a possible configuration for multiple 
installations for Models-3 software.  Each independent site must have at least one server to 
support the local OODBMS and Models-3 sessions.  Each site can run autonomously.  A two(cid:173)
server configuration is recommended at each installation:  one to support emissions processing 
activities, and the second to support other client-server and model computations.  Of course, 
additional servers will improve performance for larger applications and the current version of 
Models-3 also supports batch submissions of model executions to a supercomputer with job status 
reporting back to the framework through the standard Network Queuing Service (NQS).  In the 
future, we envision maintaining a master database to foster sharing of data and to minimize 
duplication of effort. 

State Offices 

Public Access 

Federal Agencies 

Figure 2-1.  Seamless Computing and Data Management 

from PC to Scalable Parallel Computer 

To adapt to ever changing hardware and software, ·the framework uses a layered  interface design 
that isolates critical system components, thus minimizing the impact of hardware and software 
upgrades. Each layer, as seen in Figure 2, is connected to its adjacent layer(s) by a well specified 
interface thus easing the task of replacement if a more advanced component becomes available. 
Another key design choice  is the use of an input/output application programming interface (I/O  . 
API) for the data access layer. This concept is implemented by using a standard FORTRAN or C 
callable library for all input/output in the air quality model codes. Thus 1/0 efficiencies can be 
improved by replacing  this centralized library or new data types can be handled by adding new 
routines to the library. 

The 1/0 API is layered on top of the netCDF standard·data file format developed at the National 
Center for Atmospheric Research. This data format uses the underlying eXtemal Data 
Representation (XDR),  which is IEEE (Institute of Electrical and Electronics Engineers, Inc.) 
compliant and, therefore, enables cross-platform transfer of data without conversion.  The 
Models-3 implementation conforms to the netCDF specification, but sets additional conventions 
for information stored in the file header. NetCDF is a self documenting file format that contains 
complete specifications (parameter names, units, etc) for the file contents. All netCDF records can 
be accessed via direct access methods to minimize data access times.  When  registering 
conforming  programs, the Models-3 framework automatically writes execution history 
information to the file header record  of each output file and to the OODBMS metadata 
associated with the output file.  The history information, which establishes traceability of output 
datasets to the model that generates them, contains the date/time of model execution, the version 
numbers of the science process modules contained in the model executable, Science Manager 
specification names, host computing environment, and other pertinent information.  Science 
Manager's global shared data objects and the netCDF format are the key aspects of building "plug 
compatible" science modules that conform to Models-3 guidelines. 

USER 

INTERFACE 

USER 

INTERFACE 

Management Layer: 

Data  Manager 
Program  Manager 
Study  Manager 
Strategy Manager 
Tool Manaqer 

Science Manager 
Model Builder 
Source Code  Manager 
Framework Administrator 

Environment Layer: 

OS,  System  "Personality" 

Com putatlona/ Layer:  Programs:  models, analysis, visualization, ••. 

Data  Access Layer: 

110  Application Programming Interface 

Data  Structure/Representation:  netCDF, XOR 

Data  Storage: 

File systems &  databases 

Physical Device Layer:  Disks, networks, printers, machines 

Figure 2-2.  Architectural Layering: Flexibility for Future Change 

Schedule and Future Plans 

The Models-3 framework provides a unifying foUn.dation for continual con:imunity evolvement of 
environmental modeling and assessment tools with possible extensions beyond the current air 
quality implementation.  The use of the Models-3 framework for multimedia exposure and risk 
assessment is possible.  A simple integration of the Hydrologic Simulation Program FORTRAN 
(HSPF) and the Chesapeake Bay Water Quality Model (CBWQM) in the Models-3 framework 
explored multimedia model linkage capabilities.  Maintaining Models-3 framework to use 
emerging computing capabilities and transferring that capability to science researchers and 
environmental decision makers will continue to be the focus of the Models-3 effort. 

2.5 

References 

Benjey, W.G., and N.M. Moghari. Functionality of an integrated emission preprocessing system 
for air quality modeling: the Models-3 emission  processor.  In The Emissions Inventory: 
Programs &  Progress. VIP-56.  Proceedings of a Specialty Conference, Research Triangle Park, 
NC, October 11-13, 1995. US Environmental Protection Agency, Research Triangle Park, NC, 
and Air &  Waste Management Association, Pittsburgh, 463-471  (1996). 

Byun, D., J.  Young, J. Gipson, J.  Godowitch, F. Binkowski, S. Roselle, B.  Benjey, J. Pleim, J. 
Ching, J. Novak, C. Coats, T. Odman, A. Hanna, K.  Alapaty, R.  Mathur, J. McHenry, U. 
Shankar, S. Fine, A. Xiu, and C. Jang.  Description of the Models-3 Community Multiscale Air 
Quality (CMAQ) model.  Proceedings of the American Meterological Society 78th Annual 
Meeting, January 11-16, 1998, Phoenix, AZ. 

Dennis, R.L., D.W. Byun, J.H. Novak, K.L. Galluppi, C.J. Coats, M.A. Vouk. The Next 
Generation oflntegrated Air Quality Modeling: EPA's Models-3. Atmospheric Environment, Vol. 
30, No.12, pp.  1925 - 1938, (1996). 

Emission Inventory Improvement Program.  URL http://www.epa.gov/oar/oaqps/eiip/ 

Novak, J.H., R.L. Dennis, D.W. Byun, J.E. Pleim, K.J. Galluppi, C.J. Coats, S.  Chall, M.A. 
Vouk. EPA Third-generation Air Quality Modeling System, Volume 1:  Concept. EPA 
600/R95/084, National Exposure Research Laboratory, Research Triangle Park, NC, 55 pp. 
(1995). 

Wilkinson, J.G., and Emigh, R.A.  The Geocoded Emissions Modeling and Projections System 
(GEMAP):  Advanced Training Workshop.  Prepared for Environmental Protection Agency, 
Office of Research and Development, by Alpine Geophysics, Boulder, CO (1994). 
