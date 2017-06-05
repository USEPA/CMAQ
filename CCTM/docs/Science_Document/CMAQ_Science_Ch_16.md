<!-- BEGIN COMMENT --> 

[<< Previous Chapter](CMAQ_Science_Ch_15.md) - [Home](README.md) - [Next Chapter >>](CMAQ_Science_Ch_17.md)

<!-- END COMMENT -->

Chapter 16 
==============

PROCESS ANALYSIS 

Human Exposure and Atmospheric Sciences Division 

Gerald L. Gipson

National Exposure Research Laboratory 
U.S. Environmental Protection Agency 

Research Triangle Park, NC 27711 

ABSTRACT 

The implementation of process analysis techniques in the Models-3 Community Multiscale Air 
Quality (CMAQ) modeling system is described in this chapter.  These techniques can be used in 
Eulerian photochemical models such as the CMAQ Chemical Transport Model (CCTM) to obtain 
information that provides insights into how model predictions are obtained.  This type of 
information is particularly useful when modeling nonlinear systems such as atmospheric 
photochemistry.  The two techniques available in the CMAQ system - integrated process rate 
(IPR) analysis and integrated reaction rate analysis (IRR) -- are each described.  The manner in 
which IPR analysis can be used to determine the relative contributions of individual physical and 
chemical processes is presented.  Descriptions of how to employ IRR analysis to elucidate 
important chemical pathways and to identify key chemical characteristics are included. Finally, the 
procedures used to apply each technique in the CMAQ system are also described. 

•Corresponding author address: Gerald L. Gipson, MD-80, Research Triangle Park, NC 27711.  E-mail: 
ggb@hpcc.epa.gov 

## 16.0  PROCESS ANALYSIS 

Major fur{~iion of 8.ir pollution models is to predict the spatial ru1'<l"'temporal distributions of 
ambient air pollutants and other species.  For complex Eulerian grid models, output concentration 
fields of these species are determined by solving systems of partial differential equations.  These 
equations define the time-rate of change in species concentrations due to a series of physical and 
'.  chemical processes (e.g., emissions, chemical reaction, horizontal advection, etc.).  Since most 
grid models are configured to output only the concentration fields that reflect the cumulative 
effect of all processes, information about the impact of individual processes is usually not 
available. Gnd models can be configured to provide quantitative information on the effects of the 
chemical reactions and other atmospheric processes that are being simulated, however (Pleim, 
1990; Jeffries and Tonnesen,  1994; Jang et al.,  1995a,b).  This type of information has been used 
to develop various process analyses that provide descriptions of how a model obtained its 
predictions.  This chapter provides background information on these methods and describes how 
process analysis is implemented in the Models-3 Community Multiscale Air Quality (CMAQ) 
modeling system. 

Although process analysis does not have to be included in a grid model application, it can provide 
5Upplemental information that can be quite useful in assessing a model's performance.  Quantifying 
the contributions of individual processes to model predictions provides a fundamental explanation 
of the reasons for a model's predictions and shows the relative importance of each process.  This 
information can be useful in identifying potential sources of error in the model formulation or its 
inputs.  It can also be useful in interpreting model outputs, particularly with respect to 
understanding differences in model predictions that occur from a change to the model itself or to 
its input.  Further, information provided from the chemical process analysis can be used to 
determine important characteristics of different chemical mechanisms. This is particularly useful 
for investigating mechanistic differences under different chemical regimes (e.g., voe versus NOX 
limiting conditions). 

The inclusion of process analysis in a model application is generally carried out in two steps. 
First, the model itself is "instrumented" (i.e., additional code or modules are added to the model) 
to produce supplemental outputs about the contributions of individual processes and different 
chemical reaction pathways to the model predictions.  These data are then used with the 
C,8~cen~liq2,,,Ji~1~11~s,,~~.P<?Stpro~:~sing operations to provide quantitative explanations of the 
factors affecting a model's predictions.  Although several specific postprocessing techniques have 
b'een develop'e'd to reveaf particular mod.et t'eatUres (e.g., Jeffries and Tonnesen,  1994; Jang et al., 
f995a,b ), ···process analysis data can be extracted and analyzed in many different ways.  The 
iiilp'l'e'mentatlon of process analysis in the CMAQ system has been structured to facilitate data 
~~'i;factiori, fg,r,,,,,,~uE,,~equent model analysis.  Although the main focus of this chapter is on data 
extraction techniques, some example process analyses are also presented to illustrate particular 
allplications. 

·· 

111111' 

I 

For purposes of discussion, it is convenient to separate process analysis into two parts: integrated 
process rate (IPR) analysis and integrated reaction rate (IRR) analysis.  The first deals with the 
effects of all the physical processes and the net effect of chemistry on model predictions. IRR 
analysis deals with the details of the chemical transformations that are described in the model's 
chemical mechanism. In general, IPR analyses are generally much easier to apply and understand 
than IRR analyses since the latter typically requires a fairly thorough understanding of 
atmospheric chemistry.  Thus, the discussion below describes each analysis method separately, 
starting with IPR analyses.  Users not familiar with atmospheric chemistry details may wish to 
omit the sections on IRR analysis.  It should be added that either analysis method can be applied 
independently of the other. 

The CMAQ implementation of process analysis includes a flexible user interface that allows the 
user to request only those particular outputs that are needed for model analysis. The information 
generated for both IPR analysis and IRR analysis is controlled by the Process Analysis Control 
Program (P ACP).  The P ACP processes user-specified commands to instrument the CMAQ 
Chemical Transport Model (hereafter referred to as the CCTM) to generate the specific outputs 
that are selected by the user.  As a consequence, the P ACP must be invoked before configuring a 
CCTM and running a simulation.  The details of how the P ACP works and the syntax for the 
commands are covered in the Models-3 User Manual (EPA,  1998), and they will not be repeated 
in their entirety here.  Nevertheless, some of the command syntax and some simple examples are 
presented in the discussions that follow to illustrate how the P ACP is used to set up a particular 
process analysis. 

## 16.1 Integrated Process Rate Analysis 

The governing equation for Eulerian models is the species continuity equation.  Application of 
the continuity equation to a group of chemically reactive species res~lts in a system of partial 
differential equations (PD Es) that gives the time-rate of change in species concentration as a 
function of the rates of change due to various chemical and physical processes that determine the 
ambient species concentrations.  As noted in the introduction, the concentration fields that are the 
numerical solutions to these PD Es reveal only the·net effects of all processes.  This section is 
concerned with how the contributions of individual processes are determined and used in process 
analyses.  The first two subsections deal with the calculation and use of IPRs in general. The last 
two subsections describe the CMAQ implementation of IPR analysis and the use of the PACP to 
set up an IPR analysis. 

### 16.1.1  Computation of Integrated Process Rates 

All Eulerian models utilize the technique of operator splitting.  As a result, it is relatively easy to 
obtain quantitative information about the contribution of individual processes to total 
concentrations.  In operator splitting, solutions to the system of PD Es are obtained by separating 
the continuity equation for each species into several simpler PDEs or ordinary differential 
equations (OD Es) that give the impact of only one or two processes.  These simpler PDEs or 

qp~ ~~,,,~!:,~ s2!,:ve~,~eparately to arrive at the final concentration.  To illustrate, consider the 
simple case of two-dimensional horizontal advection of a single species in the absence of any 
other processes, for which the governing equation can be expressed as follows: 

,. 

', 

' 

' 

' 

ac  +  a(uc)  +  a(vc)  =  0  ' 
at 

ax 

ay 

(16-1) 

where  c is the species concentration, and  u and v are the x- and y-components of the wind 
vefoCity vector, respectively.  With operator splitting, this 2-dimensional equation is split into two 
f:climeiisiOiiaI operators, one for each direction: 

,,11,, 1 

1:::1111' 

iIDd 

111::::111:: 

, 1: : : : : : ! ! : 111 · 
11111!!' 

, , . 
. 

11 

' • 

• ~ : : : : ! ! 111 : : , ! I : : 

"::111::, 

: i 11111 : 

I' 

ac  +  a(uc)  = 0 
at 

ax 

:111 

' '  

ac  +  a(vc)  =  0  . 
at 

ay 

'Ill:,:" 

(16-2a) 

(16-2b) 

l,,!!111111;; 

,;I 

11 

! 

'''''· 

, 

,,,11,,,Ill!i 

'' 

', 

, 

1 

, 

I 

'Ill 

',,,  ,,, 

'!1111111:, 

ii 

, 

These two equations are then solved sequentially, with the solution to the first being used as the 
initial condition for the second.  The solution to the second equation then represents the final 
solution and gives the net effect of 2-dimensional advection. 

The final solution for the example presented above can also be represented as follows: 

1

'""'

11111 

C(,t  + flt)  =  c(t)  +  (flc)x  +  (flc)Y  , 

I 

h 

(16-3) 

···  where c(t+At) is the final solution, c(t) is the initial condition for the 2-dimensional problem, and 
(Ac).r and (Ac)y are the changes in concentration produced by each of the  I-dimensional 
operators.  (Ac).r and (Ac)y  give the impact of each operator in moving from the initial to the final 
concentration and are equivalent to the results obtained by integrating the process rates 
individually. Hence the term integrated process rates is used to describe them.  Note that they can 
be computed with little additional work since they are simply equal to the difference between the 
final and initial concentrations for each operator. 

From the above example, it should be evident that a general mathematical representation of IPRs 
for individual processes can be expressed as follows: 

' , ,  

::: 

1111111 

,,,;11111111 

!1111: 

'!!II 

1
'""  1111,:::ll!  ,11 

.·

'" 

:. 

1111111· 

,::, 

,,111111;; 

111111 i i , , ; , 

' ; i 11 u 

" 

11 i 111111111111 i i I i ! ~ ' , , , , , 

· 11 11 I i I i i I !!111111 

<fl~)  =  r'. At  L  dt 

n 

J t 

n 

' 

(16-4) 

wh~re (Ac)n is the change in a species' concentration due to operator n, Ln is the differential 
d~nitor"iiss'o'ciated with a process, and At is the model synchronization time step (which is 

· , 

, 

, 

' · 

' 

· · · 

, 

' I : ' 

, . 

, , 

16-4 

'Ill 

llllh, 

1111111 
'lh,, 

11111111 

"'II!' 

equivalent to At sync in Chapter 6).  Refer to Chapter 6 for a discussion of the various time steps 
used in CMAQ.  The integration in equation (16-4) is performed by the model regardless of 
whether it has been instrumented for process analysis.  Thus, it is only necessary to save the (Ac)n 
to obtain process analysis capabilities.  In a few cases, however, models may be structured such 
that one operator deals with two or three processes simultaneously.· In those cases, the (Ac)n 
obtained after the integration would represent the compound effect of all of those processes and it 
would not be possible to discern the impacts of the individual processes.  Normally, the only way 
that information could be obtained would be to  integrate the process rates separately.  In some 
instances, however, it may still be possible to isolate the impacts of the individual processes 
without performing additional integrations.  For example; the CCTM treats vertical diffusion, 
emissions, and dry deposition simultaneously in one operator, but the amount of material 
deposited by dry deposition is tabulated and the amount of material that is emitted is known.  As a 
consequence, mass balance techniques can be used to compute the contribution of each process 
subsequent to the simultaneous integration of the process rates without separately integrating 
each process rate.  Thus, the IPRs that are available for process analysis are to some degree 
determined by the underlying structure of the photochemical model that is being used and by the 
effort that is invested in separating individual components when processes are coupled in a single 
operator. 

Analogous to equation ( 16-3), the concentration at the end of a time step can be expressed as 
follows: 

c(t+At)  =  c(t)  + L  (Ac)n  , 

N 

n=I 

(16-5) 

where the model is assumed to have N operators.  It should be noted that most IPRs can be either 
positive or negative since most processes can cause concentrations to either increase or decrease. 
Further, it should also be evident that the IPRs in the above expression are additive.  Thus, for 
example, the IPRs for horizontal advection and vertical advection could be summed to give one 
IPR that represents the net impact of the two advection processes.  The differential operators (Ln) 
themselves are most often nonlinear, however.  Because of these nonlinearities, the magnitude of 
the IPRs for most processes would change ifthe order of the model's operators was altered or 
even if only one of the operators was changed.  Thus, the additive property for IPRs holds only 
for a particular application of the model. 

### 16.1.2  Example IPR Analyses 

The tabulation and subsequent output of IP Rs provide the user with quantitative information on 
the  effects of individual processes, and these can be examined and depicted in a number of ways. 
Figures 16-1  and  16-2 contain two types of displays that were developed to depict process 
contribution data graphically (Jeffries,  1996).  Figure 16-1  is a time series plot showing both 
predicted concentrations and integrated process rates.  This type of plot shows the hourly 
variations at a cell (or group of cells if the data are aggregated) of a predicted species 
concentration and the change in concentration caused by each process (i.e., the IPRs).  Plots of 
this type illustrate the variations in process contributions during the simulation period.  Figure 16-
2 shows process contributions and total concentrations for different model formulations, in this 
case, different grid resolutions.  Here the data have been aggregated over several cells and hours, 
but could be developed for a single cell or time period just as well.  This figure highlights how 
cumulative process contributions are altered by the different model formulations.  For a more 
thorough discussion of how this type of data can be used to assist in the evaluation of a model's 
performance, the reader is referred to Jang et al.  (l 995a and  l 995b) and to Pleim (1990). 

1, 
: : I h " ~ : : 

111' 

·111111111

1
' ' '  

11 
" 

; I , 

I 

' "'  ' ' I " ' ' 

" ' 

" , , 

### 16.1.3  Implementation of IPR Analysis in the CMAQ System 
. 

.  ~ 

~ 

11 
' 
'  , , 

II 

" 

I 

The previous section illustrated that instrumenting a model for process analysis involves providing 
the capability to capture IPRs.  Since an IPR can be calculated for every combination of process 
and species, the amount of output data that can be generated is substantial.  As a consequence, 
the PACP has been designed to allow substantial flexibility in selecting the particular IPRs for 
output.  This is accomplished primarily by allowing the user to choose only those particular 
species/process combinations that are of interest.  Additional control and flexibility are provided 
by including the ability to produce lumped IPRs, by allowing special species families to be defined, 
and by pro\riding controls to limit the size of the modeling domain for which outputs are 
generated.  These will be illustrated in the examples presented below. 

The physical processes that are simulated in the CCTM and hence are available for IPR analysis 
are shown in Table 16-1.  As will be illustrated below, these processes are referenced in the PACP 
by the codes shown in the first column.  The procedure for selecting specific IPRs for output is 
species oriented.  Hence,  a user selects a species and then indicates which processes will be 
included in the IPR output.  The species are referenced by their model names.  In addition, the 
user may also define a family of species that is a linear combination of the model species (e.g., 
defining NOx as the sum of NO and N02)  and extract IPRs for the family.  This can be useful in 
saving disk space occupied by the output IPR files when information about individual members of 
a family is not needed. 

It should be apparent from Table 16-1  that IPRs for some species will always be zero (e.g., those 
species not emitted always have zero IPRs for that process).  Thus, the size of the output file can 
be minimized by not extracting those IPRs.  The P ACP also contains an option that allows the 
user to limit the amount of output data by extracting IPR outputs for only part of the modeling 
domain. Currently, the user is restricted to selecting a single, contiguous block of cells within the 
domain for the IPR outputs.  The block is defined relative to the modeling domain by  selecting a 
starting and ending column, row and level. A possible future enhancement would be to provide a 
graphical interface to allow the user to select any particular cell or group of cells within the 
domain. 

The CCTM model has been instrumented to write the IPRs to an output file at the same time as 
the output concentration files are written.  Thus, the IPR outputs represent the cumulative impact 
of the process integrations over the entire output time interval.  Process contributions over shorter 
time intervals can be obtained by increasing the frequency of writing outputs of both the 
concentration fields and the IPRs.  Finally, the IPR output files are standard Models-3 IO/API 
gridded files, and can be viewed with the Models-3 visualization tools described in Models-3 User 
Manual (EPA,  1998). 

### 16.1.4  Use of the PACP to set up an IPR Analysis 

This section illustrates how the PACP can be used to generate the IPR data for an analysis. 
Details on formatting inputs and using the PACP are contained in the Models-3 User Manual 
(EPA,  1998).  This section borrows from  that discussion to  illustrate how the P ACP is used.  The 
user selects and controls the form of the IPR output data by means of a PACP command file.  A 
few predefined command files are available to set up an analysis, or users can generate their own 
files to customize their analyses.  A command file consists of a series of commands and definitions 
that contain instructions for generating IPR outputs.  The commands are input in a free form 
format to facilitate encoding, and they contain special keywords that have specific meaning to the 
PACP.  The commands related to IPR analysis have been divided into two groups: Global 
commands and IPR output commands.  A description of the commands within each group will be 
presented first,  followed by an example illustrating how these commands are used.  In the 
description that follows, the syntax for each command is given first, with bold type used for 
PACP keywords and normal type used for user supplied input.  Alternative inputs are separated 
by  vertical bars and completely optional inputs are enclosed in curly braces. 

Global Commands: 

OUTPUT_DOMAIN = {LOCOL[ni] + HICOL[n2]  + LOROW[n3)  + HIROW[n4) 

+  LOLEV[n5]  + HILEV[n6]}; 

The OUTPUT _DOMAIN command provides the capability to limit 
the IPR output data to only one portion of the modeling domain. 
The ni  in brackets are numbers that define the bounds of the output 
domain relative to the number of columns, rows, and vertical levels 
in the modeling domain.  Thus, for example, the value for n1 must 
be greater than or equal to one and less than or equal to the number 
of columns in the domain.  If this command is included, at least one 
domain specifier must be present, and the end of the domain is used 
for any that are missing.  If the command is omitted entirely, output 
is generated for the entire domain. 

DEFINE FAMILY familyname = {c 1*}species 1 {+ {c2*}species2 + ... }; 

The DEFINE FAMILY command is used to define a group of 
species as members of a family.  The user specified "familyname" 

16-7 

"""'must be unique  and can be referenced in subsequent commands. 

" , ":'  ' ::::T4~,,,, ci are numerical coefficients that default to one if not specified; 

·:1111111111111111111111, 

.~11, 

"I'',, 

", 

i" 

··1111 

.1111111: 

'Ill" 

, 

, 

,, 

"speciest are the names of individual model species. 

ENDPA; 

'The ENDPA command signifies the end of the command input in 
the P ACP command file. 

", 

"I 

111  , 

·I, 

" 

. 

• 

i11iilllllll1::· 
"'""'1''11"' 

1111111111, 

,, 
11'  11111 

'  , 
IPR Output Command: 

""1 

' 

·" 

' 

IPR_OUTPUT specieslfamilynamelALL {=  pcode 1 + pcode2 + ... }; 

,::11:,,, 

' 

TQ~,, If',R _OUTPUT command defines specific IPR outputs to be 
generated during a CMAQ simulation.  A species name, family 
1ll111lllll11111nrune"1'"''orthe keyword ALL must follow the IRR  OUTPUT 
keyword.  The keyword ALL refers to all model species.  IPRs are 
generated for the selected species or family, and they are controlled 
H  by the specified values of pcodei, where pcodei corresponds to one 
"'"""of the  process codes listed in Table 16-1.  If no process codes are 
, ,  specified, IPRs will be generated for every process.  The output 
111111111:,,1,,.Variables  that are generated are named either species_pcodei or 
111j':  familyname _pcodei 
"",  ,11,J, 

,,,:,,,::11111 

\u111::
',

"', 

-

ilu'  '"' 

111!1!11: 
1

1111111111111, 

"' 

',, 

111111: 

" 

I 

' 

!Iii 
111111:111111," 
111 : ' " 

A  listing of an example PACP command file is contained in Exhibit 16-1.  To facilitate the 
discussion that follows, the commands have been numbered, although this is not required by the 
PACP.  (Note that all information enclosed by curly braces in a command file is treated as 
comments.)  Each numbered line represents a command, and the input for each command is 
terminated by a semicolon.  This particular set of commands causes the CCTM to generate 
several individual IPRs for a special user-defined sub-domain.  Each of the commands is described 
below. 

Command 1 is used to restrict the process analysis output to a subset of the modeling domain. As 
noted above, output would be generated for the entire computational domain if the 
OUTPUT_DOMAIN were omitted.  Since keywords for columns and rows are not present, the 
P ACP default is to include all rows and columns in the modeling domain.  The keywords 
"LOLEV" and "HILEV" restrict the output for the vertical level to layers 1 through 2.  Thus, the 
net effect of this command is to limit the IPR output to all cells within the first two vertical levels 
ofihe modeHng domain. 

111',11''1 

,111111111 

11111111 

Commands 2 and 3 are used to define families of species.  The species names to the right of the 
equal sign are model species.  The effect of the numerical coefficients in the definition of the VOC 
families is to convert the units of the IP Rs for individual organics from ppm to ppmC.  As will be 
seen, these "defined" family names are referenced in subsequent commands. 

Commands 4 through 8 actually cause IPR outputs to be generated.  As described above, IPRs 
are generated for the species and the processes that are referenced by means of the codes listed in 
Table 16-1.  Since no process codes are specified in commands 4 and 5, IPRs will be generated 
for each of the twelve processes in Table 16-1  for the families NOX and VOC (i.e., a total of24 
IPRs).  Command 6 causes three IPRs to be generated for species 03, one each for total 
transport, chemistry, and clouds.  Command 7 causes an IPR for vertical diffusion to be generated 
for every model species.  The last command signifies the end of the command inputs. 

The CCTM will generate the IPR output data in a form comparable to the output concentration 
files.  Hence, the data are written to standard Models-3 IO/ API gridded data files.  The units of 
the IPR data are normally the same as those for the species concentration data (i.e., ppm for gas(cid:173)
phase species and either µg/m 3 or number/m3  for aerosols).  As described in the example of 
creating the family VOC with units ofppmC, however, different output units can be created using 
special family definitions. 

The example just described was formulated primarily to illustrate how IPR commands are 
structured to collect IPR information during a model simulation.  In general, the particular data 
that are collected for process analysis would be determined by the needs of the study, and thus it 
is difficult to define a "default" process analysis.  Nevertheless, a minimal process analysis for 
studying ozone formation might involve collecting the process contributions for NOx, VOC, and 
for ozone.  The example in Exhibit 16-1  could be used as the starting point for such an analysis. 
Commands 4 and 5 capture all the IPRs for NOx and VOC.  Command 6 could be modified to 
capture all IPRs for ozone as well.  Since it would not normally be necessary to capture vertical 
diffusion IPRs for all species, command 7 could be dropped.  Of course, the user would still be 
required to define the domain for outputs (command 1) and to define the voe family for the 
mechanism that is being used (command 3). Thus, commands 1through5 with command 6 
modified to collect all IPRs for ozone would provide some very basic process analysis information 
on the formatio of ozone during a simulation. 

## 16.2 Integrated Reaction Rate Analysis 

The second major component of process analysis is IRR analysis.  It is applied to investigate gas(cid:173)
phase chemical transformations that are simulated in the model.  Its primary use to date has been 
to help explain how ambient ozone is formed in the chemical mechanisms that are used in 
photochemical models (e.g., Jeffries and Tonnesen, 1994; Tonnesen and Jeffries, 1994).  Thus, 
the CMAQ implementation oflRR analysis currently addresses only gas-phase reactions. 
Nevertheless, the concepts should be adaptable to the modules simulating aerosol formation and 
aqueous chemistry as well, and this is an area for future enhancement in the CCTM.  The 
remainder of this section describes how the IRRs are calculated and generated in the CMAQ 
system. 

### 16.2.l  Computation oflntegrated Reaction Rates 

As described in Chapter 8, the simulation of atmospheric chemistry is a key component of a 
photochemical air quality model.  The operator splitting techniques that are used in Eulerian 
photochemical models typically result in a set of nonlinear, coupled, ordinary differential 
equations (ODEs) that describe chemical interactions in the gas-phase.  Solutions to these ODEs 
are obtained using numerical solvers to compute species concentrations as a function of time. 
Again, these computed concentrations show only the net effect of chemical transformations.  As 
hns been done for physical processes, a technique has been developed to provide quantitative 
information on individual chemical transformations (Jeffries and Tonnesen,  1994).  Since the 
technique involves integrating the rates of the individual chemical reaction, the method is termed 
integrated reaction rate analysis. 

As was noted in section 8.3 .1, the mathematical expression for the rate of a chemical reaction 
tnkes one of the forms of equation set 8-3.  The reaction rate is used to compute the change in 
species concentration that is caused by the reactio.n.  Mathematically, this can be expressed as 
follows: 

· 

111'1! 

I 

, .1111111111  .M,1'(t+At)  =  ~ (t)  + r·t.i  r,  dt  , 

(16-6) 

where M1 refers to the integrated reaction rate (IRR) for reaction l,  At is the model 
synchronization time step used by the chemical solver, and r 1 is the rate of reaction l 
corresponding to one of the forms of equation set 8-3.  The value of Mi represents the total 
throughput of the reaction, and can be used with the appropriate stoichiometry to determine the 
amount of an individual species that is produced or consumed by the reaction.  For example, 
assume the IRR for the reaction A + B  -+  2C is 20 ppb for a given time period.  Then, the amount 
of A and B consumed in that time period by this reaction is 20 ppb, and correspondingly, the 
amount of C produced is 40 ppb.  Further, the net change in a species concentration due to all 
chemical reactions is equivalent to the sum of all its production terms less the sum of all its loss 
terms.  As a consequence, the contribution of each reaction to the change in concentration of any 
species is directly available from the IRRs.  With this information, it is possible to identify the 
important chemical pathways that affect species concentrations and thereby unravel the complex 
c~.~mical interactio1:1s that are being simulated. 

As described in Chapter 8-4, the chemistry solvers used in air quality models typically employ 
marching methods that compute species concentrations at the end of a time step given the 
c~,2~e~trat!~~'~"""'~t ,~,e ~ginning of the step.  Since the solvers adjust the time steps to maintain 
st~!?Hity and accuracy, the reaction rates should not vary too greatly over a given time step.  As a 
c~~~~qti~~ce, ··~·~···is  possible to use a fairly simple numerical integration technique to compute the 
IRRs.  The technique used in the CCTM is the same the one used by Jeffries and Tonnesen (1994) 
-- the trapezoid rule.  With this method, the IRRs are computed as follows: 

M, (t+/lt)  =  M, (t)  +  -[ r1(t)  +  rp+llt)]  . 

1 
2 

(16-7) 

Thus, the IRRs are simply computed from the values of the reaction rates at the beginning and the 
end of each chemistry integration time step.  As the chemistry solver marches through time, the 
variable Mt accumulates the IRRs over the simulation period.  Although an IRR could be 
accumulated for the entire simulation, more information can be gained on how the mechanistic 
processes vary with time ifthe accumulated IRRs are periodically output and the value of Mt is 
reset to zero.  In the CCTM, the IRR output is synchronized with the outputs for the 
concentration fields and the integrated process rates.  Thus, just like the IPRs, the IRRs that are 
output represent the integral of the reaction rates over the output time interval. 

### 16.2.2  Example IRR Analyses 

Most IRR analysis performed to date has been devoted to  studying mechanistic processes that 
affect tropospheric ozone formation.  One method that has been used involves analyzing two 
important, interacting cydes: the OH radical cycle and the NOx oxidation cycle.  This section 
contains a brief discussion of these cycles and other important chemical parameters to illustrate 
how integrated reaction rate analysis can be used to understand different chemical pathways.  For 
more comprehensive discussions, the reader is referred to Jeffries (1995), Jeffries and Tonnesen 
(1994), and Tonnesen and Jeffries (1994). 

Ozone is formed in the atmosphere via the photochemical cycle 

N02  +  hv 

....  NO+ 0(3P) 

0(3P)  +  0 2 

....  0 3 

(RI) 

(R2) 

(R3) 

If these were the only reactions taking place in the atmosphere, an equilibrium condition would be 
established that would determine the ozone concentration.  These levels are almost always lower 
than what is observed in the atmosphere because of other interactions (Seinfeld, 1998).  The 
presence of free radicals can alter the ozone production rate through the following reactions that 
are competitive with R3: 

H02  +  NO 

....  N02  +  OH 

R02  +  NO 

....  N02  +  RO . 

(R4) 

(RS) 

In these reactions, N02 is produced in reactions that do not consume ozone, thereby introducing a 
process by which ozone can accumulate.  Furthermore, these reactions are radical propagation 
reactions since a hydroxyl radical (OH) is produced from the hydroperoxy radical (H02)  and an 

16-11 

,11,,111 

'lh 

::·11ill11 

EP A/600/R-99/030 

illl!!!!I' 

,1111111111 

'illll''!'''I 

,' 

1
',,lll• 

1
!,,,,::::1!

1
!111!!:,, 

/,,'

,,,111,, 

'I 

all5:9xy radical (RO) is produced from an alkylperoxy radical (R02).  These product radicals are 
then available to participate in other reactions, some of which lead to the regeneration of the 
peroxy radicals.  Thus, the production of ozone can be viewed as an autocatalytic process since 
0 3  can be produced without the loss of its precursor N02•  In the atmosphere, however, ozone 
production is limited by termination reactions that remove either radicals or N02 from the system. 

'' 

"',  :"' 

' 

" 

' 

1111 

' 

' 

,, 

11111111111' 

""""' 

"111111111111111111"' 

The ozone formation process has been represented by means of a schematic diagram, shown in 
Figure 16-3a, 'th.at consists of two interacting cycles (Jeffries,  1995).  The top portion of the figure 
is the OH reaction cycle which consists of the three principal categories ofreactions involving 
radicals: initiation, propagation, and termination.  Radical initiation reactions are almost always 
photolytic reactions that generate ''new" radicals. Examples include the photolysis of 
formaldehyde, hydrogen peroxide and ozone . Radical propagation reactions include reactions 
such as R4 and RS  in which NO is converted to N02  but no radicals are lost.  Many reactions in 
which organic compounds are oxidized also propagate radicals.  The termination reactions 
remove radicaj~ through the formation of stable products.  The bottom portion of Figure l 6-3a 
represents the NOx oxidation cycle.  As with the radical cycle, processes in this cycle are classified 
as either initiation, propagation, or termination.  The initiation process for NOx corresponds to 
emissions of NOx, and the termination process corresponds to reactions in which NOx  is converted 
to""stable products.  The NOx cycle is connected to the radical cycle by means of the propagation 
steps that convert NO to N02• 

One form of an IRR analysis that can be conducted involves using the IRRs to develop 
quantitative information about the various initiation, propagation, and termination processes. 
Figure 16-3b shows a cycle diagram similar to  Figure 16-3a in which the IRRs have been used to 
compute the numerical values that are shown.  These values include net throughput for several 
parameters that serve to further characterize the state of the reacting system.  For example, the 
fraction of OH that is regenerated by the chemical reactions (0. 776 in Figure 16-3b) can be 
determined from the amount of OH that reacts (142.7 ppb) and the amount that is re-created 
(110.8 ppb).  This is directly related to the OH chain length parameter (4.46) which corresponds 
to the average number of times each new OH is cycled until it is removed from the system.  An 
analogous chain length parameter (5.13) can be calculated for the NOx oxidation cycle in the form 
of the NO chain length.  The longer these chain lengths, the greater the potential for 0 3  formation 
per unit ofNOx emissions.  Other parameters such as the "NO oxidations per VOe consumed" 
[(NO-NOi)NOe = 1.71]  and "03 produced per 0 3P generated by N02  photolysis" ([03]/[03P] 
= 0. 951)  further quantify the relative efficiency of ozone production, the former being particularly 
useful in providing a measure of the reactivity of the organic compounds. 

These types of analyses are particularly useful for comparing model results that are obtained using 
different chemical mechanisms or that are obtained at different locations with the same chemical 
mechanism.  Other types of IRR analyses can provide other information as well.  For example, 
IRRs have been used to allocate the total production of 0 3 to the individual voes, to examine 
individual characteristics of various chemical mechanisms such as yields of radicals from different 
organics, and to quantify other entities such as the production and loss of odd-oxygen (Ox),  which 
can serve as surrogate for tracking the formation of ozone (Jeffries and Tonnesen,  1994; 
Tonnesen and Jeffries,  1994).  It can be expected that many other types oflRR analyses will be 
developed, especially as newer and possibly more complex chemical mechanisms are developed 
and used in models. 

### 16.2.3  Implementation of IRR Analysis the CMAQ system 

As described in Chapter 8, the CMAQ system is designed to treat chemical mechanisms in a 
generalized manner.  Since a specific chemical mechanism is not embedded in the CCTM, a 
comparable generalized method is needed to link IRR analysis with the chemical mechanism.  The 
technique that has been incorporated in the CMAQ system is one that provides the user with the 
capability to formulate and then generate particular chemical parameters of interest.  Presumably, 
these parameters would be chosen to reveal special properties of the mechanism and/or to be used 
in generating photochemical cycle diagrams such as Figure l 6-3b.  To illustrate, a special P ACP 
command file has been prepared for the RADM2 mechanism.  For reference, a listing of the 
RADM2 mechanism is contained in Exhibit 16-2, and the reader is referred to the Models-3 User 
Manual for a detailed explanation of the mechanism format.  For purposes of the discussion that 
follows, it is sufficient to note that reaction labels are enclosed in "<" and ">" and precede each 
reaction, and that the reactants and products in each reaction are model species.  Both the 
reaction labels and the species names will be referenced in P ACP commands. 

Table 16-2 lists the chemical parameters that are produced in this example IRR analysis for the 
RADM2 mechanism. Most of these parameters could also be generated for other chemical 
mechanisms, although the specific calculations that would have to be performed would necessarily 
differ because of differences in the mechanisms.  The parameters in Table 16-2 are computed as 
simple, linear combinations of the IRRs that are calculated for each chemical reaction.  The 
domain controls that are specified for the integrated process rate outputs apply to IRR outputs as 
well.  Thus, both the IPR and IRR data will be generated for the same domain and written to 
output files at the same time intervals.  Similarly, the IRR output files are standard Models-3 
IO/API gridded files, and can be used with the standard Models-3 visualization tools.  In addition, 
a special visualization tool that can generate "default" cycle diagrams similar to Figure 16-3b is 
available and is described in Models-3 User Manual (EPA,  1998).  Note, however, that the IPR 
and IRR outputs are written to separate files. 

One of the advantages of generating IRR data in the form of chemical parameters is that the 
output file storage requirements can often be minimized.  Note that there are fewer IRR 
parameters in Table 16-2 than there are species in most mechanisms.  The major disadvantage to 
this approach is that new or different parameters cannot be computed without rerunning a model 
simulation.  As a consequence, the CMAQ implementation oflRR analysis also contains the 
option to capture the complete set of IRRs rather than  chemical parameters when a model is run. 
With this option, one IRR is generated for each chemical reaction, and these IRRs can then be 
manipulated in postprocessing routines to form any particular chemical parameter of interest. 
Thus, it would be anticipated that the chemical parameters such as those in Table 16-2 would be 
generated in fairly routine model applications, whereas IRRs for each reaction would be generated 
for  exploratory analyses.  The form of the outputs is controlled by the P ACP commands that are 
described next. 

### 16.2.4  Use of the PACP to set up an IRR Analysis 

As with IPR analysis, the IRR outputs that are generated by the CCTM are controlled by 
commands and special operators that are processed by the PACP.  The global commands that 
were described in the IPR section also apply to the IRR output.  Thus, family names created with 
the DEFINE FAMILY may also be used with several of the IRR commands and operators. 
Before describing how the commands are used to construct an IRR analysis, a brief description of 
each IRR command and operator is first presented.  These are divided into three groups: IRR 
global definitions, IRR operators and IRR output commands.  Again, the same syntax conventions 
ar~ used, i.e., P ACP keywords and symbols are in bold type, user supplied values are in normal 
type, alternative inputs are separated by vertical bars, and completely optional inputs are enclosed 
in braces. 

111111'111:... 

111111111111111111111,,,,1 

"111111111111111,,' 

,, 

' 

' 

1•,,:11111 

"""""""' 

""' 

' 

' '  

• 

111111 

" 

1111!'. 

' 

IRR Global Definitions: 

;1: 

I 

,,,1illl111ilh 

IRR_TYPE = FULLIP ARTIALINONE; 

,, 

The IRR_ TYPE command defines the type of IRR analysis.  With 
,,,,,the type set to FULL, IRRs for each reaction will be calculated and 
written to the IRR output file, and all other IRR commands will be 
..  ignored. IRR_TYPE set to PARTIAL indicates that the IRR 
commands following this command are to be processed to 
:"ii"produced user defined IRR outputs.  Type set to NONE causes all 
·:!!:9th~r IRR coll1Il1ands to be ignored and no IRR output to be 
generated.  If the command is omitted, type PARTIAL is assumed. 

111111

• 

DEFINE CYCLE cyclename = species 1; 

The DEFINE CYCLE command is used to compute the net of all 
chemical production and loss of a species involved in more than one 
cyclical reaction set.  Thus, this quantity is computed by summing 
,,,,,,  the IRRs for all reactions in which a species is consumed, and then 
, •• , sub!facting that sum from the sum of the IRRs for all reactions in 
which the species is produced.  The "cyclename" is a user defined 
name that must be unique, and can be referenced in subsequent 
' IRR  OUTPUT commands. 

DEFINE RxNSUM sumname = {±}{ci*}<rxlabl1>  { ±  {c2*}  <rxlabl2> :I: ... }; 

The RXSUM command is used to compute a linear combination of 
IRRs for individual reactions that can then be referenced in a 
subsequent IRR_ OUTPUT  command;  "sumname" is user defined 
and must be unique.  The linear combination of IRRs is defined 
according to the expressions following the equal signs that specify 
the reaction IRRs to sum.  The "rxlabl1"  is the reaction label that is 
used by the generalized mechanism to  identify each reaction and is 
enclosed in "<" and ">".  The "c.'' are optional numerical 
coefficients that default to one if not specified. 

IRR Output Operators: 

PROD(speciesi]  {FROM[species2]  {ANDIOR [species3]  } } 

The production operator (PROD) is  used to compute the total 
production of a species by summing the IRRs of all reactions in 
which species 1 appears as a product.  The optional qualifiers 
FROM, AND, and OR restrict th~ sum to  include only those 
reactions in which species2 and/or species3 are reactants; "species1" 
can be any gas-phase mechanism species or a family of gas-phase 
species that was defined using the DEFINE FAMILY command as 
described in the IPR section; "species2"  or species3"  may also be the 
keyword HV to restrict the selection to photolytic reactions. 

NETP[speciesiJ  {FROM[species2]  {ANDIOR [species3]  } } 

The net production operator (NETP) is very similar to the 
production operator PROD since it is used to compute the 
production of a species.  Whereas the PROD operator includes 
every reaction in which species occurs as a product, the NETP. 
operator includes only those reactions in which the net production 
of species 1 is greater than zero.  Thus, if species1 appears as both a 
reactant and a product with equal stoichiometry in a reaction, the 
PROD operator will include it but the NETP operator will not. 
This operator is useful for getting the net production of a family, 
for example, by eliminating those reactions in which the net effect 
of the reaction on the family concentration is zero. The qualifiers 
FROM, AND and OR restrict the inclusion of reactions to those in 
which species2  and/or species3  are reactants. 

LOSS[species 1]  {ANDIOR [species2]  } 

The loss operator (LOSS) is used to compute the total loss of a 
species by summing the IRRs of all reactions in which species1 
"""'appears as a reactant ...  The optional qualifier AND restricts the sum 
to include only those reactions in which both species 1 and species2 
arei:~act~ts. Similarly, the OR qualifier includes all reactions in 
which either "species1
"species1
mechanism, a family name that includes only gas-phase mechanism 
species, or the keyword HV to restrict the selection of reactions to 
those that are photolytic. 

11  or "species2"  can be any gas-phase species in the 

11  or "species2"  appears as a reactant. The 

NETL[species1]  {ANDIOR [species2]  } } 

111!•,llllllllll"I 

,'1111'" 

I 

""' 

'

''h 

,lllllll:",1111111 

'"""'"11111111 

The net loss operator (NETL) is very similar to the loss operator 
since it is used to compute the loss of a species.  However, it 
, includes only those reactions in which ther~ is a net loss of 
1:::::~'~sp'~'~ies1 11  and/or "species2"  •  Thus, if species1 appears as both a 
~eactanf and a product with equal stoichiometry in the reaction, the 
NETL operator will not include it in summing the loss of that 
species, whereas the LOSS operator will include the IRR for that 
reaction.  This operator is useful for getting the net loss of a family 
of species. 

,11, 

Ill 

,"'1111111111' 

,;1111 

NET[species1] 

I 

:111111:• 

,,1111 

The net operator (NET) is very similar to the CYCLE definition 
since it gives the net of the production and the loss of a species for 
all reactions in which "species1"  appears either as reactant or a 
product;  "species1
any family consisting wholly of gas-phase mechanism species. 

11  may be any gas-phase, mechanism species or 

1r111  I~ Output Co~~ds: 

IRR_ OUTPUT irrname =  {c1*}op1lcyclname{qual 1}lsumname{qual1}1<rxlabl1> 

{ ±  {c2*}op2lcyclname{qual2}lsumname{qual2}1<rxlabl2> + ... }; 

The IRR_ OUTPUT command defines a specific IRR output to be 
generated during a CCTM simulation.  It is constructed by 
specifying a linear combination of IRR operators, IRR global 
definitions, or IRRs for specified reactions.  Each individual term in 
the combination must include either one of the five IRR operators 
just described (i.e., opi), a cycle name, a reaction sum name, or a 
reaction label enclosed in "greater than" and "less than" signs.  The 
optional qualifiers (qual;) for cyclename or reaction sum name can· 
be either POSONL Y or NEGONL Y.  With these qualifiers, the 
defined quantity is included as a term only when it  is positive qr 
negative, respectively.  If the name is not qualified, the quantity is 
included regardless of sign.  The numerical coefficients for each 
term (c;) are assumed to  be one unless they are explicitly included. 
The irrname that is supplied by the user will be assigned as the 
variable name in the IO/API IRR output file. 

DESCRIPTION= 'description'; 

The description command is provided to allow the user to specify a 
long description of the output variable that will be included on the 
IO/ API IRR output name.  If a description is not specified for an 
IRR  OUTPUT variable, the irrname (or short name) will· be used in 
the output file.  If the description command is used, it should be 
located immediately following the IRR_ OUTPUT command to 
which it applies. 

Before describing how these commands are actually used, some additional comments are 
warranted.  First, the specification of any particular IRR output might be accomplished in several 
different ways.  For example, the net production of a species could be obtained using a CYCLE 
definition, a RXNSUM definition, a NET operator, or simply specifying the appropriate sum of 
IRRs directly in the IRR_ OUTPUT command (i.e., via reaction labels).  Although the user is free 
to choose any particular approach, some computational efficiencies may be achieved by using the 
CYCLE and RXNSUM definitions.  The CCTM has been constructed to compute these quantities 
just once, and then use them whenever they are referenced in an output command.  Conversely, 
operator quantities are recomputed every time they are referenced.  Thus, it is more efficient to 
use the RXNSUM and CYCLE commands when they can be referenced several different times in 
IRR_ OUTPUT commands. Second, the NETP and NETL operators are probably most useful for 
computing the production and loss of species families.  When these operators are used, a reaction 
is not included in the sum if there is no net loss or production of a family member in the reaction. 
Thus, all reactions are eliminated from  the computations when a member of a family  is formed 
from another member of the same family and there is no net impact on the family concentration. 
Finally, the sign conventions employed in the CMAQ process analysis need to be defined.  IRRs 
for individual chemical reactions are always positive.  Since IRRs can be subtracted when 
computing CYCLE, RXNSUM, and NET quantities, the result can be either positive or negative. 
The production and loss operators always produce positive values, however, since individual 
IRRs are always summed in their computation. 

To illustrate how these PACP commands are used to generate IRR output data, two examples are 
presented.  The first illustrates how to capture IRRs for each reaction.  The second demonstrates 
how PACP commands are used to compute the special chemical parameters in Table 16-2 for the 
RADM2 chemical mechanism. 

Ezjiibit 16~3 CQ:i:ttaJPs the ~A~}> ccmunands for the first example that corresponds to a full  IRR 
analysis since IRRs will be calculated for each and every reaction.  This option is invoked by the 
first command that specifies that the IRR analysis type is FULL.  Since no OUTPUT_DOMAIN 
command is present, the IRR outputs will be generated for every cell in the modeling domain. 
E,ilii6it 16-4 sho~~1111the p A.CP ~o~ands for the example i'R.R analy;fs that has be~~ set up  for 
the RADM2 chemical mechanism.  In a P ACP command file, all  lines that start with an 
exdamation point(!) in the first column are comments.  To facilitate the discussion below, 
eoffihient lines have been used to block the IRR commands into special groups.  The first four 
groups contain global commands or definitions.  All blocks after the first four contain the IRR 
coµl'rtjands  ~a,, igenerate the particular parameters listed in Table 16-2. 

111,,11111111' 

,,,111, 

II 

'1111111111111111 

'"'II 

I 

'11,,,, 

'llh' 

11"' 

1, 

1,, 

'I 

·"•"'I 

,111'1111111'" 

1111111 

lilil 

II'' 

, 

'" 

'""'"""' 

""' 

'11111111111111 .. ,1

1
: 

.':!.'"" 

"""""""""" 

"' 

""""' 

lllll1111lllllllll' 

'•,,,,''•' 

,' 

'""'  '" 

111111111111111111111111111:•, 

'·,11111111:":1: 

lll1'1llllllllllW:11 

',1,,,,1,,11111111111!:::11' 

'111111111111111 .11111,1111' 

The commands in the first group simply define the type of IRR analysis and the domain for which 
•  th<;1111111!l~Jl11111g,:P,!J>Uts are to be generated.  Th~ second group includes ~ru:1ily definitions.  These 
commands are of the same form as described for IPRs.  The remammg two groups of commands 
......  define cheniica.I cyCles ancfreaction sums that are subsequently referenced in IRR  OUTPUT 
commands. As noted above, the cycle commands give the net production or loss of a species by 
all chemical reactions.  Several of the reaction sums that are defined here are also cycles in that 
they generate the net effect of a few reactions on the production or loss of a few particular 
species.  Most of the others are used to define special quantities.  For example, the defined 
RXNSUM  newM02 in Exhibit 16-4 corresponds to the production of new M02, where new 
refers to an initiation reaction for the radical M02.  As is apparent from the IRR_OUTPUT 
commands in the subsequent blocks, the cycle and reaction-sum names are referenced fairly 
frequently.

All of the remaining blocks of commands in Exhibit 16-4 contain the commands for IRR outputs. 
Again, one IRR output is generated for each IRR_OUTPUT command, and the outputs that are 
produced correspond to the chemical parameters listed in Table 16-2.  As indicated above, each 
output is generated by the defined linear combination of predefined cycles, reaction sums, special 
IRR operators, and/or specified reaction IRRs referenced by reaction label.  It should be evident 
that these commands are mechanism specific and require analysis of the mechanism itself to 
formulate.  Thus, this particular P ACP command file would not  be applicable to any mechanism 
other than the RADM2.  It should also be apparent that other important chemical parameters 
could be formulated and generated in an analogous manner.  In fact, this command file can be 
used as the staning point for adding to or modifying some of the selected chemical parameters. 

11 

'11 11
·1::
""1111 

1,,,,,,111111111111 
1111111111111' 

1111111111111 
'1111111111111111;,, 

,II 

111111111 

:1 

11' 

1,ll' 

As11 ~vith II>R ()1!tputs, the CCTM Wi'n generate"the IRR output data in""'a  form comparable to the 
···output coricenfiation files.  That is, the data are contained in standard Models-3 IO/API gridded 
'11!i;11111111!~11110¥1Jmts are Iii:i:~ar combinations of individual reaction throughput, and thus 
dnt,~ fjJ~~· 
have the same units as the gas-phase species concentrations (i.e., ppms).  However, it should be 

111 

'111111' 

''I' 

11" 

'11111111, 

II•' 

11111 

"' 

:1111111111 

.111111: 
,11111 

remembered that these are throughputs calculated by integrating reaction rates over the output 
time interval, and not simply abundances at a particular time. 

## 16.3  Conclusion 

Process Analysis is a diagnostic method for evaluating the inner workings of a model.  Although 
specific types of techniques have been performed and used in the past, new ways of examining 
and analyzing process data are likely to be developed and used in the future.  As a consequence, 
the emphasis in the CMAQ implementation of process analysis has been placed on providing easy 
methods of extracting key process data from the CCTM simulations. The CMAQ implementation 
also provides tools to allow users to customize their analyses.  These tools are designed to be easy 
to use and not require coding changes to the model. 

As noted previously, a process analysis is set up by using the P ACP before constructing and 
running a CCTM simulation.  Both IPR and IRR data can be gathered during a simulation, but 
each are written to separate output files.  To collect both sets of output during a single simulation 
requires that the PACP command file contain both the IPR commands and the IRR commands. 
Although the IPR and IRR examples have been presented separately, both sets of outputs can be 
produced with a single file containing both sets of commands.  Recall, however, that the 
OUTPUT_DOMAIN applies to both the IPR outputs and the IRR outputs.  Thus, IPR and IRR 
data cannot be generated for different parts of the domain in the same simulation. 

The P ACP program performs a substantial amount of error checking.  The program will check 
for the proper syntax of the input commands and perform some logic checking.  For example, it 
checks to make sure that all species referenced in IRR commands are gas-phase mechanism 
species and that the members of defined families are either all gas-phase species or all aerosol 
species.  As is apparent from  Exhibit 16-4, however, the inputs for a comprehensive IRR analysis 
can be fairly extensive.  As a consequence, the PACP produces an output report that summarizes 
what IRR and IPR outputs are being requested.  One of its major functions is to report on the 
effects of the special IRR operators that are used in the P ACP command file.  A user may wish to 
review this report before proceeding to run the CCTM to  insure that the desired outputs will be 
generated.  The reader is referred to the Models-3 User Manual (EPA, 1998) for an example 
output report. 

Finally, the default configuration for the CCTM is to omit process analysis outputs entirely. Thus, 
no process analysis will be generated in this configuration.  Any  process analysis must be set up 
in the Science Manager of the Models-3 framework.  The reader is referred to the Models-3 User 
Manual (EPA, 1998) for details on how this is done. 

## 16.4  References 

J~g, J.  C., H. E. Jeffries, D. Byun, and J. E. Pleim,  l 995a. "Sensitivity of Ozone to Model Grid 
11111,, Resolution - I. Application of High-resolution Regional Acid Deposition Model", Atmospheric 
Environment, Volume 29, No. 21, 3085-3100. 

Jang, J.C., H. E. Jeffries, and S. Tonnesen, 1995b. "Sensitivity of Ozone to Model Grid 
Resolution - II. Detailed Process Analysis for Ozone Chemistry", Atmospheric Environment, 
Volwne 29, No. 21, 3101-3114 .. 

Jeffries, H. E.,  1995. "Photochemical Air Pollution," Chapter 9 in Composition,  Chemistry,  and 
Climate of the Atmosphere, Ed. H.B. Singh, Van Nostand-Reinhold, New York, N.Y. 

Jeffries, H.E., 1996. "Ozone Chemistry and Transport'', presentation to the F ACA subcommittee 
for Ozone, Particulate Matter and Regional Haze Implementation, March 21, Alexandria, Va. 

Jeffries, H. E. and S. Tonnesen, 1994.  "A Comparison of Two Photochemical Reaction 
Mechanisms Using Mass Balance and Process Analysis", Atmospheric Environment, Volume 28, 
No.  18, 2991-3003. 

. 

'II'"' 

1111111111111 

I'"' 

Pleim, J.E.,  1990. Development and Application of New Modeling Techniques for Mesoscale 
Atmospheric Chemistry, Ph.D. Thesis, State University of new York at Albany, Albany, New 
York. 

111111111' 

Seinfeld, J. H. and S. N. Pandis,  1998.  Atmospheric Chemistry and Physics,  From Air Pollution 
to Climate Change, John Wiley and Sons, New York, New York. 

Tonnesen, S. and H.  E. Jeffries, 1994.  "Inhibition of Odd Oxygen Production in the Carbon Bond 
Four and Generic Reaction Set Mechanisms", Atmospheric Environment, Volume 28, No. 7, 
1339-1349. 

This chapter is taken from Science Algorithms of the EPA Models-3 Community 
Mu/tiscale Air Quality (CMAQ) Modeling System, edited by D. W. Byun and J. K. S. 
Cbing, 1999. 

16-20 

Table 16-1. 

CCTM Processes and PACP Codes 

P ACP Code  Process Description 

EP A/600/R-99/030 

Vertical advection 
Mass adjustment for advection 
Horizontal diffusion 
Vertical diffusion 
Emissions 
Dry deposition 

X.ADV Advection in the E-W direction 
Y ADV Advection in the N-S direction 
ZADV 
ADJC 
HDIF 
VDIF 
EMIS 
DDEP 
CHEM Chemistry 
AERO 
CLDS 
PING 

Aerosols 
Cloud processes and aqueous chemistry 
Plume-in-grid 

Note:  The following process codes can also be used in the P ACP. 

XYADV  Sum ofX.ADV and YADV 
XYZADV  Sum ofXADV, YADV, and ZADV 
TOTADV  Sum ofXADV, YADV, ZADV, and ADJC 
TOTDIF  Sum of HDIF and VDIF 
TOTTRAN Sum of XADV, YADV, ZADV, ADJC, HDIF and VDIF 

Chemical Parameters Used in Default IRR Analysis 

Table 16-2. 

'11111;,,, 

'111111111 

{ 1}  Production of Odd Oxygen (Ox) 
{2}  Loss of Odd Oxygen (Ox) 
{3}  Production ofNOz from NOx 
{ 4}  Production of NOx from NOz 
{5}  Production of new OH from OlD 
{6}  Production of new OH other than from 010 
{7}  Production of new H02 
{8}  Total production ofH02 
{9}  Production of new R02 
{10}  Total Production of R02 
{ 11}  Loss of CO and CH4 by reaction with OH 
{12}  Production of OH from H02 
{13}  Production ofN02 from H02 
{14}  Production ofN02 from R02 
{15}  Production of PAN and TPAN 
{16}  Net Production of organic nitrates 
{17}  Loss ofVOCs by reaction with OH 
{18}  Loss of OH by reaction with organics 
{19}  Net Production or Loss ofHN03 
{20}  Loss ofHCHO by reaction with OH 
{21}  Loss of isoprene by reaction with OH 
{22}  Production of new H02 from HCHO 
{23}  Production ofH02 from PAN 
{24}  Production of H02 from R02 and NO 
{25}  Production ofH02 from R02 reacting with R02 
{26}  Production of R02 from OH 
{27}  Production of HN03 from N02 reacting with OH 
{28}  Production of new OH from H202 
{29}  Production of new OH from organic peroxides 
{30}  Production of OH from HONO 
{31}  Termination of OH 
{31}  Termination ofH02 
{31}  Termination of H02 by reaction with R02 
{34}  TerminationofR02 
{35}  Termination ofR02 by reaction with H02 
{36}  Termination ofR02 by reaction with R02 
{37}  Loss of OH by reaction with daughter VOCs 

Example P ACP Command File for IPR Analysis 

Exhibit 16-1. 

{1}  OUTPUT  DOMAIN=  LOLEV[l]  +  HILEV[2]; 

{2}  DEFINE  FAMILY  NOX 

NO  +  N02; 

{3}  DEFINE  FAMILY  voe 

{4}  IPR_OUTPUT  NOX; 

{s}  IPR_OUTPUT  VOC; 

2.0*ETH  +  2.9*HC3  +  4.8*HCS  +  7.9*HC8 
+  2.0*0L2  +  3.8*0LT  + 
4.8*0LI  +  S.O*ISO  +  7.l*TOL  +  8.9*XYL 
+  l.O*HCHO  +  2.4*ALD; 

{6}  IPR  OUTPUT  03  =  TOTTRAN  +  CHEM  +  CLDS; 

{-7}  IPR  OUTPUT  ALL  =  VDIF; 

{a}  ENDPA; 

16-23 

<  P2>  03 
<  P3>  03 
<  P4>  HONO 
<  PS>  HN03 
<  P6>  HN04 
<  P7>  N03 
<  PB>  N03 
<  P9>  H202 
.CP10>  liciio 
111  HIC1H'O 
1111111111111· 
~"~~~;  Aili 
<Pl3>  OPl 
<Pl4>  OP2 
<PlS>  PAA 
<Pl6>  KET 
<P17>  GLY 
<Pl9>  GLY 
<Pl9>  MGLY 
<P20>  DCB 
<P21>  ONIT 
<  1>  03P  +  02  +  M  • 
.C'"'  2>  03P  +  N02 
'!S,,,,H,,  3~  Q:!:D  +  !:!,~,, 
4>  OlD  +  02 
< 
5>  010  +  H20 
< 
6>  03  + 
NO 
< 
HO 
7>  03  + 
< 
<  8>  03  + 
H02 
9>  H02  + 
< 
NO 
<  10>  H02  + 
N02 
<  11>  HN04 
c;,,12>  H02  +  H02 
<  13>  H02  +  H02  +  H20  • 
<  14>  H202  +  HO 
.C"  15>  NO  +  HO 
<  16>  NO  + 
NO 
<  17>  03  + 
N02 
NO 
~111111,,,,lB>  Nq~  + 
''""N102 
"iillllllll" ~ 9 >  ~~II~  + 
'"""ito2 
'!\'.  20>  N03  + 
~;,,,,,  ~;> N03  + 
"'  ,~02 
<  22>  N2os 
J""" :23 >  ti2os  +  ii20 ,, 
Ho  + 
111111111 
'No2 
j11:':i  ~:: Hd  +  lllllllHNo:illllllllll 
Jlli1111111:2G'>  HO  +  1111111lii.zoi11111111111111 
~;;;1;::>2?>  HO  +  ,1111111H102 
1111111111 
<  28>  HO  +  "11111:~,',',,,,,',o,',, 2 
Jlllllll' 2"' 9">'  ' co 
i1111111"'":3 0 >  iio 
+  ""~o 
1 
"111'111' 
111111111111' 
~111111" 31 > " ETH  +  HO 
<  32>  HC3  +  HO 
lllllliiii!·:11 
1llh1111111111111111!111::;: 
~111111'.  33> 

,,, 

II 

11111134> 
~111  35> 
~1111""36> 
<11111111111·371> 

HCS  + 
''lio 
~~a  + 
OL2  + 1111  lllllllllllHO 
ot:i'  + 
'ii"o 
ol:i  +  "'iio 

•  2.0*  HO 

03 
NO 
03P 
03P 

N02 
H02 
HO 
N02 
HN04 
H02 
H202 
H202 
H02 
HONO 

N03 

+  02  "'  2.0*N02 

1!111 

+ 

+ 

NO 

03P 

+ 
+ 
+ 

o3P 
01D 
03P 
HO 
HO 
H02 
NO 
N02 

NO 
N02 
N02 

# '' 11i.o 
II  1.ci 
#  1.0 
#  1.0 
#  1.0 
#  1.0 
#  1.0 
#  1.0 
#  1.0 
#  1:0 
,,,,,  #  1. o 
f  1.o 
#  1.0 
#  1.0 
#  1.0 
#  1.0 
#  1.0 
#  1.0 
+ 
#  1.0 
#  1.0 
0.020*AC03 
0.800*KET  +  H02  +  N02  #  1.0 

co 
co 
HO 
HO 

•  2.0*  HO 
co 
H02 
M02 
HCHO 
ALO 
M02 
AC03 

HC)2 
H02 
H02 
H02 
HO 
ETHP 
•  0.13*HCHO  +  1.870*CO 
•  0.4S*HCHO  +  1.sso•co  +  0.800*H02 

+ 
+ 
+ 
+ 
+ 
+ 

•  0.98*H02  + 
•  0.20*ALD  + 

CO 
TC03 

+ 
+ 
+ 
+ 

AC03 

H02 

+ 

+ 

1:1111·1:"1;;:,,,1::1',,,, 

/<No~''""1:w:>M8B~; 
/<030lD_RADM88>; 
/<0303P_RADM88>; 
/<HONO_RADM88>; 
/<HN03_RADM88>; 
/<HN04_RADM88>; 
/<N03NO_RADM88>; 
/<N03N02_RADM88>; 
/<H202  RADM88>; 
/<Hc.Hcimo1  RADM88>; 
( <Hciiorad - RADM8 0 >; 
/<ALo:_:RAnM88>; 
/<MHP_RADM88>; 
/<HOP_RADM88>; 
/<PAA_RADM88>; 
/<KETONE_RADM88>; 
/<GLYform_RADM88>; 
/<GLYmol_RADM88>; 
/<MGLY_RADM88>; 
/<UDC_RADM88>; 
/<ORGNIT  RADM88>; 

#  6.0E-34A-2.3;-
#  6.SE-12 
@  -120.0; 
#  1.8E-ll  @  -110.0; 
#  3.2E-11 
#  2.20E-10; 
#  2.00E-12  ®1400.0; 
#  l.60E-12  ®  940.0; 
#  l.lOE-14  ®  500.0; 
#  3.70E-12  ®  -240.0; 

®  -70.0; 

+ 

+ 

HO 

N02 
If 
\-3 
\-3  # 

#  l.8E-31A-3.2  &4.7E-12A-l.4; 
#  2.lE-27  ®  -10900.0  *E<  10>; 
2.20E-13®-620.0  &  l.90E-33®-98~.0; 
3.08E-34@-2820.0  &  2.66E-54@-3180.0; 

#  7.0E-31A·2.6  &l.5E-11A·0.5; 

#  3.3008-12  @  200.0; 

#  3.300E-39@  -530.0; 
#  l.4000E-13  ~  2500.0; 
#  1.7000E-ll@  -150.0; 
~'''  2. SOOOE-14  ®  123§:. 0; 
#  2.sciooE-i2; 

2.0*N02 

NO 
HN03 
N205 
N02 

2'~ O*HNOJ 

+ 

+ 

HN03 
NOJ 
N02 

N02 

N03 

t2  #7. 2El'-15@-785&4. 1Ei"-i6®-i44o&1. 9E-33@-'72s; 

#  2.2E-30A-4.3  &l.5E-12A·0.5; 
#  l.lOE-27  ®  -l1200~0. *E<21;; 
#  2.00E-2l; 
. 
#  2 .6E-JOA -3. 2 .,,&2. 4E .. ~ 11A-1.3; 
lr 1.3ci6oi~1"2  @ -3BO .O; 
"#"  4.6000E-if  ®  -236.6; 

' 

#  3.0E-Jl:...·J.3  &l.SE-12AO.O; 

\-1.  #1.5000E,~13;  , .. , 

,,. 

' 

•  SULF  +  H02  +  SULAER 

H02 
M02 
ETHP 

(X  300  SQUARED) 
(X  300  SQ~ED) 

.. 

0. s'J*HCJP  +, O. ~ 7*H02  +,  O. 009*HCHO  "' 

0.025;.:KET 

HCSP  +  0.250*X02 
HC8P  +  o. 750*X02  +  HC8AER 
OL2P 
OLTP 
'oil:P  +  oLr.i>.ER 

""'""" 
®  1280.0; 

11'  2B.3"2 
#  l.23JE-12A2  ®  444.0; 

.f.  O. o7s•Af.o 

' 

#  1.S9E-ll  ,,,,,,,!i.  540.0; 
#  i.73E;:11  ®  3~0:0; 
#  3. 64'E:-11  "  ®  38,,o ~,  oi,, 
f, 2.15E-12  @  -411.0; 
#  s:32E:1:2  @  -so4:o; 
#  i.01E:~1:i.  @  -549.o, 
,111 
ii'' 
,.:II 

~?ch!h!,!,,,,J~-:~,;,,,,,,, Lfa!i11g of the RADM2 Chemical Mechanism 
11111111111'"1 

""1111,",1 

11111!1 

"1111111111 

'111111111:" 

'1:::1: 
11111111111, 

'".:,,·,',',,'1111.:··.:111' 
' ' ' I 111 : : ~ ! 11 ' II : ! ' 
I 
1
1
:ii,1
,,  "
:::!!!!:, 
1,.11111111"!1 

' "  

11

"'11"""11111'" 
'!1:! 1::1111
i I i i 11111 " ~ 1111 
111!!!iiil1111,,, 
11111.1111111: 

""'II 

1,6-24 

'lll,,111111111' 

1111111111'" 

'"11' 

':111111' 

11111"11 

Exhibit 16-2.  Listing of the RADM2 Chemical Mechanism 

EP A/600/R-99/030 

<  38>  TOL  +  HO 

<  39>  XYL  +  HO 

<  40>  CSL  +  HO 

<40a>  CSL  +  HO 
<  41>  HCHO  +  HO 
<  42>  ALO  +  HO 
<  43>  KET  +  HO 
<  44>  GLY  +  HO 
<  45>  MGLY  +  HO 
<  46>  DCB  +  HO 
<  47>  OPl  +  HO 
<  48>  OP2  +  HO 
<  49>  PAA  +  HO 
<  50>  PAN  +  HO 
<  51.>  ONIT  +  HO 
<  52>  ISO  +  HO 
<  53>  AC03  +  N02 
<  54>  PAN 
<  55>  TC03  +  N02 
<  56>  TPAN 
<  57>  M02  +  NO 
<  58>  HC3P  +  NO 

<  60>  HC5P  +  NO 

<  62>  HC8P  +  NO 

<  64>  OL2P  +  NO 
<  65>  OLTP  +  NO 
<  66>  OLIP  +  NO 

<  67>  AC03  +  NO 
<  68>  TC03  +  NO 

0.75*TOLP  +  0.250*CSL  +  0.250*H02 
+  TOLAER 
0.83*XYLP  +  O.l.70*CSL  +  0.170*H02 
+  XYLAER 
O.l.O*H02  +  0.900*X02  +  0.900*TC03 
+  CSLAER 

CSL 
H02 
AC03 
KETP 
H02  + 
AC03 
TC03 

+ 

co 

2.000*CO 
co 
+ 

0.5*M02  +  0.500*HCHO  +0.500*HO 
0.5*HC3P  +  0.500*ALD  +  0.500*HO 

+ 

N02 

AC03 
HCHO  +  N03  +  X02  (x  300  sq) 
HC3P 
OLTP 
PAN 
AC03 
TPAN 
TC03 
HCHO 

N02 
H02 

N02 

+ 
+ 

N02 

+ 

+ 

0.75*ALD  + 

0.25*KET  +  0.09*HCHO 

+  0.964*N02  +  0.964*H02 
= 0.38*ALD  +  0.69*KET  +  O.OS*ONIT 
+ 

0.92*N02  +  0.92*H02 
0.35*ALD  +  1.06*KET  +  0.04*HCHO 

+  0.24*0NIT  +  0.76*N02  + 

0.76*H02 

l..6*HCHO  +  H02  +  N02  +  0.20*ALD 

+  HCHO  +  H02  +  N02 

ALD 
H02  +  l..45*ALD  + 
N02 
N02 

+  O.l.*KET 
M02 
N02  +  0.920*H02  + 
+  0.050*AC03  +  0.950*CO 

+ 

+ 

0.28*HCHO 

#  2.lOE-12  ®  -322.0; 

It  l.89E-ll.  @  -116.0; 

#  4.00E-l.l.; 
#  0.9*K<40>; 
#  9.0000E-12; 
#  6.8700E-12@  -256.0; 
#  1.2000E-ll.  ®  745.0; 
#  l.1500E-ll; 
#  1. 7000E-ll.; 
II  2.8E-11; 
#  1.0000E-l.1; 
II  l..OOOOE-11; 
11  1. OOOOE-l.l; 
#  6.1650E-13A2 
II  1. 5500E-ll. 
#  2.5500E-11 
II  2.8000E-l.2 
#  l..9500E+l.6 
#  4.7000E-l.2; 
#  1.9500E+l.6 
II  4.2000E-l.2 
+  0.036*0NIT 
II  4.2000E-l.2  ®  -180.0; 

@  540.0; 
@  -409.0; 
@  -181..0; 
@  l.3543.0; 

@  1.3543.0; 
@  -180.0; 

@  444.0; 

#  4.2000E-12  ®  -l.80.0; 

#  4.2000E-l.2 
#  4.2000E-12 
11  4.2000E-12 

@  -180.0; 
@  -l.80.0; 
®  -l.80.0; 

#  4.2000E-12 
II  4.2000E-l.2 

@  -1.80.0; 
@  -180.0; 

0.890*GLY  +  O.ll.O*MGLY 
+ 

2.000*X02 

<  69>  TOLP  +  NO 

N02  +  H02  +  O.l.7*MGLY 

<  70>  XYLP  +  NO 
<  71.>  ETHP  +  NO 
<  72>  KETP  +  NO 
<  73>  OLN  +  NO 
<  74>  HCHO  +  N03 
<  75>  ALO  +  N03 
<  76>  GLY  +  N03 
<  77>  MGLY  +  N03 
<  78>  DCB  +  N03 
<  79>  CSL  +  N03 

<  80>  OL2  +  N03 
<  Bl.>  OLT  +  N03 
<  82>  OLI  +  N03 
<  83>  ISO  +  N03 
<  84>  OL2  +  03 

+ 

0.70*DCB 
.45*MGLY  + 

.806*DCB 

H02  + 

N02 

0.16*GLY  + 
N02  +  H02  + 
ALO 
+ 
MGLY  + 
HCHO 
H02 
AC03 
HN03 
HN03 
HN03 
HN03 

+ 
+ 
+ 

+ 
+ 
+ 

+ 

+ 

N02 
ALD  + 
HN03 
HN03 
H02  + 
AC03 
TC03 

H02 

2.0*N02 
co 

+ 

2.000*CO 
co 

+ 

XN02 

+  0.500*CSL 

+  O.SOO*CSLAER 

OLN 

OLN 
OLN  +  OLIAER 
OLN 
HCHO  +  0.400*0RAl.  +  0.420*CO 

ii  4.2000E-l.2 

@  -l.80.0; 

11  4.2000E-l.2 
#  4.2000E-l.2 
#  4.2000E-12 
11  4.2000E-l.2 
It  4.20008-12 
It  6.000E-13 
ll  l..400E-12 
It  6.000E-l.3 
#  l..400E-12 
#  l.400E-l.2 

fl  2.200E-11; 
#  2.000E-12 
It  1. OOOE-11. 
11  3.230E-l.l. 
#  5.BlOE-l.3; 

®  -1.80.0; 
@  -l.80.0; 
Cil  -l.80. O; 
@  -180,0; 
@  -l.80.0; 
@  2058.0; 
@  l.900.0; 
(jl  2058.0; 
@  l.900.0; 
@  1900.0; 

@  2923.0; 
@  l.895.0; 
@  975.0; 

<  85>  OLT  + 

03 

0.53*HCHO  +  O.SOO*ALD  + 

0.33*CO  +  0.20*0RA1 

+  0.20*0RA2  +  0.23*H02  + 

0.22*M02  +  O.lO*HO 

+  O.l.20*H02 

#  l.. 200E-14 

@  2633.0; 

#  1.3200E-l.4  @  2105.0; 

<  86>  OLI  + 

03 

O.l.B*HCHO  +  0.72*1>.LD  + 

O.l.O*KET  +  0.23*CO  +  0.06*0RAl. 

EXhibit  i6-2:  Listing of the RADM2 Chemical Mechanism 

< 87>  ISO  + 

C3 

+  0.29*CRA2  +  0.26*HC2  + 
+  CLIAER 
•  0.53*HCHC  +  O.SOO*ALD  + 
+  0.20*CRA2  +  0.23*HC2  + 

0.14*HC  +  0.3l*MC2 

#  7.2900E-15  @  1136.0; 

0.33*CC 

+ 

0.20*CRA1 

0.22*MC2  + 

0.10*HO 

li111llll 

<  88>  H02  + 
< 89>  H02  + 
<  90>  H02  + 
-< ...  91>  H02  + 
1\i,,,,,  941>  H02  + 
<93>  H02  + 
c  94>  H02  + 
<  95>  H02  + 
<  96>  H02  + 
<  97>  H02  + 
<  98>  H02  + 
~"""···99>  H02  + 
<100>  H02  + 
<l.01>  H02  + 
<l.02>  M02  + 
<103>  M02  + 
<104>  M02  + 

,  ""

CPl 
CP2 
CP2 
CP2 
CP2 
CP2 
CP2 
CP2 
CP2 
PAA 
CP2 
CP2 
CP2 
CNIT 

MC2 
ETHP  • 
HC3P  • 
HCSP  • 
HCSP  • 
OL2P  • 
CLTP  • 
CLIP  • 
J<ETP  " 
ACC3  • 
TOLP  • 
XYLP  • 
11Tco3 "'"· 
CLN 
M02 
ETHP  •  0.7S*HCHO 
HC3P  •  0.84*HCHC  +  0.770*ALD  + 

l.S*HCHO 

HC2  + 

HC2 

+ 

+ 

• 

0.75*ALD 

0. 26{J*KET 

+  1. OOO*HC2 

<10S>  M02  +  HCSP  •  0.77*HCHC  +  0.4l*ALD  + 

0.75*KET 

<106>  H02  +  HCSP  •  O.SO*HCHC  +  0.46*ALU  + 

l.39*KET 

+  1. OOO*HC2 

+  1.000*HC2 

HC2 
<l.07>  MC2  + 
<108>  M02  + 
HC2 
<l.09>  M02  +  CLIP  •  0.89*HCHC  +  0.725*ALD  +  HC2 

l..SS*HCHC  +  0.3SO*ALD  + 
l..2S*HCHC  +  0.750*ALD  + 

OL2P  • 
CLTP  • 

+  O.SS*KET 

<110>  H02  + 
<ll.l>  M02  + 

KETP  •  0.75*HCHC  +  0.750*MGLY  +  HC2 
ACO~  •  HCHC  +  O.S*HC2  +  O.S*MC2 

~"h2> 1111 Md2  + 

.111111"'' 
,,,1111111' 

111

,, 

;1'  .,''I 

<:n .. :1.13>  M02  + 

;1111:,,,1111 

1111 111111111111+ 

TOLP  "' 
.. ··.  + 
,,;igLP  • 

... ,,1111111•· 
11:"!111111 

''''11:::11111 
I 
':1111 
,,,,,,1111111. 

O.SO*CRA2 
HCHC  +  er. i 7~MGLY +  0. 16*GLY 
O. 70*DCB  .  +  2. O*HC2 
'HCHO  +  o".4s•MGLY  +  0.806*DCB 
" 

' " '  

,, 

+  2. OOO*HC2  . 

cl.l.4>  M02  + 

TCO~  • 

.SO*HCHC  +  0.445*GLY  +  O.OSS*MGLY 
+  O.SO*CRA2  +  0.025*ACC3  +  0.460*HC2 
+  0.475*CC  +  XC2 

<138>  H02  +  CLN 
<115>  ETHP  +  AC03  • 

1.7S*HCHO  +  O.SO*H02  +  ALD  +  N02 
ALD  +  O.S*HC2  + 
0.5*  CRA2 

O.S*MC2 

<l.16>  HC3P  +  AC03  • 

.77*ALD  +  0.26*  KET+  O.S*HC2 

fl  l.230E-14 
#  7.700E-14 
#  7.700E-14 
#  7.700E-14 
#  7.700E-14 
#  7.700E-14 
#  7. 700E-14 
7.700E-14 
# 
7.700E-14 
# 
# 
7.700E-14 
#  7. 700E-14 
#  7.700E-14 
7.700E-14 
# 
7.700E-14 
# 
7.700E-14 
# 
It 
1.90E-13 
It 
l.40E-13 

'"' 

'"' 

Ill' 

@  2013.0; 
@  -l.300.0; 
®  -1300.0; 
@  -1300.0; 
@  -1300.0; 
@  -1300.0; 
®  -1300.0; 
@  -1300.0; 
®  -1300.0; 
®  -1300.0; 
®  -1300.0; 
@  -1300.0; 
@  -1300.0; 
@  -1300.0; 
(j)  -1300.0; 
@  -220.0; 
(i)  -220.0; 

It  4.20E-14  <il  -220.0; 

It  3.40E-14  ®  -220.0; 

#  2.90E-14 
#  1.40E-13 
#  1. 40E-13 

®  -220.0; 

@  -220.0; 
@  -220.0; 

It  1. 70E-l4 
#  1.70E-14 

(i)  -220.0; 
@  -220.0; 

fl  9.60E-13 

@  -220.0; 

#  1.70E-14 

@  -220.0; 

#  l.70E-14 

@  -220.0; 

#  9.60E-13 
It  1. 70E-14 

@  -220.0; 
(j)  -220.0; 

If  3 .40E-13 

®  -220.0; 

<117>  HCSP  +  ACC3  - 0.4l*ALD  +  0.75*KET  + 

,,  ,,,+  O.SO*MC2  +  0.5*CRA2 

0.5*HC2 

#  8.40E-14  ®  -220.0; 

+  O.SO*M02  +  O.S*CRA2 

#  l.OOE-13 

<il  -220.0; 

'" 

' 

' 11 : '  " 11 " ' 

111 " ' 1111111 11 : : : ' ' 
:,,,,,'llliiiii:,,'" 

..  D  +,,,  l.39*KET  +  O.S*HC2 

"';,,,,,,+  O.SO*MC2  +  O.S*ORA2 

<118>  HCBP  +  AC03  •  0.4,, 6., *AL 
i '1 i 
' 
~1ll9> odP  +  ACC3  - o.ao.:"i-icHC  +  0.6*ALD  +  O.S*HC2 
~11· 
'I'""'.. 
c 1i20>  CLTP  +  ACC3  • 
i!1111'~!i:' 
<121>  OLIP  +  AC03  •  0.725*ALD  +  O.SS*KET  + 
''""'"' 
<122>  KETP  +  AC03  • 

"'  +  O.S*HC2  +  O.SO*M02  +  0.5*CRA2 

ALD  +  0. S*HCHC  +  0. 5*HC2 

·::· 1,,,,,.+  O • 5 *M02  +  O . 5 *oRA2 

MGLY  +  O.S*HC2  + 

,a,,,; ~f~.02, :r.· 

,,,  0'1111~,:,,~··a~2 

0.5*MC2 

::::;~!!11111111:' 

"11:1:·11111111"': 
"i:i  111111' 

'""''""""' 

!!!:::;!!!:::::::,t 

,"  "'""" 

'""' 

' 

,, 

1

0.14*HCHC 

<123>  ACOJ  +  ACC3  • 
<124>  AC03  +  TOLP  • 
+ 

+  O.S*ORA2 
2.0*M02 
MC2  +  0.170*MGLY  +  0.16*GLY 
0.70*DCB  + 

HC2 

16-26 

' 

#  7.20E-14  ®  -220.0; 

' 

#  3.40E-13  ®  -220.0; 

#  3.40E-13 

<il  -220.0;_ 

#  4.20E-14 

,. 

®  -220.0; 

#  4.20E-14 
#  1.l9E-12  @  -220.0; 

@  -220.0; 

#  4.20E-14 

<il  -220.0; 

Exhibit 16-2.  Listing of the RADM2 Chemical Mechanism 

<125>  AC03  +  XYLP 

M02  +  0.45*MGLY  +  0.806*DCB 

+ 

H02 

#  4.20E-14 

@  -220.0; 

EPAJ6DD/R-99/030 

#  l.19E-12  ®  -220.0; 

#  4.20E-14 
#  3.60E-16  @  -220.0; 

®  -220.0; 

@  -220.0; 

#  7.?0E-14  @  -1300.0; 
#  1.70E-14  @  -220.0; 
#  4.20E-14 
II  3.60E-16  ®  -220.0; 
#  4.2000E-12  ®  -180.0; 
#  4.2000E-12  @  -180.0; 
#  7.70E-14  ®  -1300.0; 
#  1. ?OE-14  ®  -220.0; 
#  4.20E-14 
II  3.60E-16  @  -220.0; 
II  1.0*K<37>; 
#  l.O*K<82>; 
#  1.0*K<86>; 

@  -220.0; 

<126>  AC03  +  TC03 

M02  +  0.92*H02  + 
0.11*MGLY  +  0.05*AC03  +  o.95•co 

0.89*GLY 

+ 
+  2.0*X02 

<139>  ACOJ  +  OLN 

HCHO  +  ALO  + 

0.5*0RA2 

+  N02  +  o.5*  M02 

<140>  OLN  +  OLN 

2.0*HCHO  +  2.0*ALD  +  2.0•N02 

<127>  X02  +  H02 
<128>  X02  +  M02 
<129>  X02  +  AC03 
<130>  X02  +  X02 
<131>  X02  +  NO 
<132>  XN02  +  N02 
<133>  XN02  +  H02 
<134>  XN02  +  M02 
<135>  XN02  +  AC03 
<136>  XN02  +  XN02 
<AE1>  TERP  +  HO 
<AE1>  TERP  +  N03 
<AEl>  TERP  +  03 

endmech 

OP2 
HCHO 
M02 

N02 
ONIT 
OP2 
HCHO 
M02 

+ 

H02 

+ 

H02 

TERPAER  +  HO 
TERPAER  +  N03 
TERPAER  +  03 

Exhibit 16-3:  Example PACI> Command File for a Full IRR Analysis · 

16-28 

Exhibit 16-4.  Example P ACP Commands for a.Partial  IRR Analysis 

!************••••······················································· 

Example  PACP  Command  File  illustrating  Partial  IRR  Analysis 

!************************************••····················~············ 

!  IRR  type  and  domain  commands 

IRRTYPE  = PARTIAL; 
OUTPUT_DOMAIN  = LOLEV[l]  +  HILEV[2]; 

!  Family  Definitions 

DEFINE  FAMILY  OX 

DEFINE  FAMILY  NOZ 

DEFINE  FAMILY  NOX 
DEFINE  FAMILY  VOCA 
DEFINE  FAMILY  R02 

DEFINE  FAMILY  voe 

DEFINE  FAMILY  dauHC 

03  +  N02  +  2*N03  +  03P  +OlD  +  PAN  +  HN04  +  3*N205  + 
TPAN  +  OLN  +  HN03  +  ONIT1 
PAN  +  TPAN  +  HONO  +  HN04  +  N03  +  N205  +  ONIT  + 
OLN  +  HN03; 
NO  +  N02; 
OL2  +  OLI  +  OLT  +  ISO; 
M02  +  ETHP  +  HC3P  +  HCSP  +  HC8P  +  0L2P  + 
OLTP  +  OLIP  +  TOLP  +  XYLP  +  AC03  +  KETP  + 
TC03  +  X02  +  XN02; 
{CH4  +}  CO  +  ETH  +  HC3  +  HCS  +  HC8  +  OL2  +  OLT  + 
OLI  +  ISO  +  TOL  +  CSL  +  XYL  +  HCHO  +  AU>  +  KET  + 
GLY  +  MGLY  +  DCB; 
CSL  +  KET  +  GLY  +  MGLY  +  DCB  +  OPl  +  OP2  + 
PAA  +  PAN  +  ONIT; 

I  Cycle  Definitions 

DEFINE  CYCLE  PANcyc 
DEFINE  CYCLE  TPANcyc 
DEFINE  CYCLE  HONOcyc 
DEFINE  CYCLE  HN04cyc 

PAN; 
TPAN; 
HONO; 
HN04; 

!  Reaction  Sum  Definitions 

DEFINE  RXNSUM  N03cyc 
DEFINE  RXNSUM  N205cyc 
DEFINE  RXNSUM  H202 _OHcyc 
DEFINE  RXNSUM  HN03_0Hcyc 
DEFINE  RXNSUM  OPl_OHcyc 
DEFINE  RXNSUM  OP2_0Hcyc 

., 

DEFINE  RXNSUM  PAA_OHcyc 
DEFINE  RXNSUM  HN04 _H02cyc 
DEFINE  RXNSUM  OP2_H02cyc 

P7>  - <  PS>  - <  18>  - <  19>; 
22>; 
12>  - <  13>; 
24>1 
88>; 

<  17>  - < 
<  2l.>  - < 
<  P9>  - < 
<  PS>  - < 
<Pl3>  - < 
<P14>  - < 
<  93>  - < 
<  98>  - < 
<PlS>  - < 
<  P6>  +  < 
<P14>  - < 
-
<  93>  - < 
-
<  98>  - <  99>  - <100>  {- <101>}  - <127> 

89>  - <  90>  - <  91>  - <  92> 
94>  - <  95>  - <  96>  - <  97> 
99>  - <100>{  - <101>}; 
97>; 
11>  - <  10>; 
89>  - <  90>  - <  91>  - <  92> 
94>  - <  95>  - <  96>  - <  97> 

--

-

Exhibit 16-4.  Example P ACP Commands for a Partial  IRR Analysis 

DEFINE  RXNSUM  HOXcyc 
DEFINE  RXNSUM  newM02 
DEFINE  RXNSUM  newAC03 
DEFINE  RXNSUM  newETHP 
DBPINE  RXNSUM  newTC03 
DKFINE  ~SUM .PAN  AC03cyc 
DEFINE  RXNSUMTPAN_TC03cyc 
!DEFINE  RXNSL'M  propR02_NO 

8>; 

7>  - < 

< 
<Pl2>  +  0.22*<  85>  +  0.31*<  86>  +  0.22*<  87>; 
<Pl6>  +  <Pl9>  +  0.02*<P20>  +  <  77>; 
<Pl6>; 
<P20>  +  <  78>; 
s  <  54>  - <  53>; 
<  56>  - <  55>; 

- ~67> +  4.05*<68>  +  1.5*<126>; 

ii,,'::1111: 

::111::::1111111: 

111111::::.. 
~·••••••••••••••••s•••==••a=~=2•=====•=ss====~==c===••======•=zz===••m=c 
I  IRR_OUTPUT  1:  OX  Production 
l••••••••••••••••••••=••••===2ssa::sc3mc:==~•&ac:=2•=•====23mc===2z•3a== 
IRR_OUTPUT  OXprod  ~  NETP[OX]; 

' 

.•• 

!!,gsCRIPTION  • 

'ox  Production' ; 

11111111111" 

'1111111111 

1111111: 

1111 

f••••••••••••••••••~•*&••=2•••==•••==~ssz====c======c3c======m=====s•=== 
I  IRR_OUTPUT  2:  ox  Loss 
l•••••••••••••••••••=•••••==c•az=•••n===~~•£====2:cc=:=mc~======a•c===== 
IRR_OUTPUT  OXloss 

NETL[OX]; 

DESCRIPTION.  'OX  Loss'; 

I  IRR_OUTPUT  3:  Production  of  NOZ  from  NOX 

:i;~_OUTPUT NOZfromNOX.  PANcyc[POSONLY)  + 
TPANcyc[POSONLY]  + 
HONOcyc[POSONLY]  + 
HN04cyc[POSONLY]  + 
N03cyc[POSONLY)  + 
N205cyc[POSONLY]  + 

0. 024*<  62>  + 
Ill:' 
DESCRIPTION•  'NOZ  produced  from  NOX'; 

<132>; 

<  24>  +  0.036*<  58>  +  0.08*<  60>  + 

I  IRR_OUTPUT  4:  Production  of  NOX  from  NOZ 
l•••••••••••••••••••=••ms•==c•~=====••a====•:=====~a~aac===mcac===s•m~== 
IRR_OUTPUT  NOXfromNOZ 

PANcyc[NEGONLY]  + 
TPANcyc[NEGONLY)  + 
HONOcyc[NEGONLY]  + 
HN04cyc[NEGONLY)  + 
N03cyc[NEGONLY)  + 
N20Scyc[NEGONLY]  + 

<  PS>  + 
<  73>  + 

2.0*<140>; 

<P21>  + 
<138>  + 

<  51>  + 
<139>  + 

DESCRIPTION 

'NOX  produced  from  NOZ'; 

'111 

"'lllllh 

IRR_OUTPUT  5:  Production  of  new  OH  from  010 

I 
(••••••••••••••••••••••=2••E•~==2••az=~=•ma=c====•mm~====~m•s=======cc:& 
IRR_OUTPUT  OHfromOlD.  PROD 

[HO]  FROM  [010]; 

16-30 

Exhibit 16-4.  Example PACP Commands for a Partial  IRR Analysis 

DESCRIPTION 

'OH  produced  from  OlD'; 

EPA/600/R-99/030 

!================2===================================c================== 
!  IRR_OUTPUT  6:  Production  of  new  OH  other  than  from  OlD 
!======c=========================================================~====== 
IRR  OUTPUT  newOH  = 

0.1*<  87>  + 

0.1*<  85>  +  0.14*<  86>  + 
2*H202_0Hcyc [POSONLY.] 
+ 
HN03_0Hcyc[POSONLY] 
HONOcyc[NEGONLY] 
OPl_OHcyc[POSONLY] 
OP2_0Hcyc[POSONLY] 
PAA_OHcyc[POSONLY] 

+ 
+ 
+ 
+ 

DESCRIPTION= 

'new  OH'; 

!==========~~~============£============================================= 
!  IRR_OUTPUT  7:  Production  of  new  H02 
!======================================================================= 
IRR  OUTPUT  newH02  = 

2.0*<Pll>  + 

<P12>  + 
<P19>  +  0.98*<P20>  + 
<  74>  + 

0.8*<P18>  + 
<P21>  + 
<  76>  +  0.12*<  84>  + 
0. 23*<  .85>  +  0.26*<  86>  +  0.23*<  87>  + 
OP1_0Hcyc[POSONLY]  + 
OP2_H02cyc[POSONLY]  + 
HN04_H02cyc[POSONLY]  i 

DESCRIPTION 

'new  H02'; 

IRR_OUTPUT  8:  Total  Production  of  H02 

!=============~============:================~===================:======= 
! 
!=========================:=&=====================~===================== 
IRR  OUTPUT  totalH02  = 

2.0*<Pll>  + 

<P12>  + 
<P19>  +  0.98*<P20>  + 
<  74>  + 

0.8*<P18>  + 
<P21>  + 
<  76>  +  0.12*<  84>  + 
0.23*<  85>  +  0.26*<  86>  +  0.23*<  87>  + 

{H02new} 

OPl_OHcyc[POSONLY]  + 
OP2_H02cyc[POSONLY]  + 
HN04_H02cyc[POSONLY]  + 

{H02propbyOH} 

PROD[H02]  FROM 

[HO]  AND 

[VOC]  + 

{H02viaR02_NO} 

PROD[H02]  FROM 

[NO]  AND 

[R0.2)  + 

{H02byR02_R02} 

PROD[H02]  FROM 

[R02]  AND  [R02)  + 

{otherOH} 

HOXcyc[POSONLY] 

DESCRIPTION 

'total  H02'; 

IRR_OUTPUT  9:  Production  of  new  R02 

lc=z=3z======================2==:======================c~=============== 
! 
l=======================2=========================c===================== 
IRR_OUTPUT  newR02  = 

newM02 
+ 
newAC03  + 
newETHP  + 
newTC03  + 

16-31 

Exhibit 16-4.  Example P ACP Commands for a Partial  IRR Analysis 

'1111111111'" 

111111111 

PAN_AC03cyc[POSONLY]  + 
TPAN_TC03cyc[POSONLY]; 

•new  R02'; 

I  IRR_OUTPUT  10:  Total  Production  of  R02 
11J••••••••••••••••••••••••z==••S••====cgm~c=:::cs~a=c==~•••~c===~c=c~a=== 
IRR_OOTPUT  TotalR02  •  newM02  + 
newAC03  + 
newETHP  + 
newTC03  + 
PAN_AC03cyc[POSONLY]  + 
TPAN_TC03cyc[POSONLY]  + 

{neWR02} 

(prcpR02_0H} 

PROD[R02]  FROM 
<  30>  + 
<  50>  + 

[HO]  AND 

[VOC] 

+ 

0.5*<48>  + 

,,l~ropR02_NO} 

PROD[R02]  FROM 

[NO] 

DESCRIPTION• 

'Total  R02'; 

11111111111111 

. , , , , , , -

I  IRR  OUTPUT  11:  Loss  of  co  &  CH4  by  reaction  with  OH 
•···················•a•===••••n••==z:•••=c===·-3·~===2aaaa=z===~c3ac:=== 
IRR_OUTPUT  Loss_CO_CH4 

<  30>  + 

DESCRIPTION.  'Loss  of  co  &  CH4'; 

LOSS 

[CO] 

l••••••••••••••••••••••••s~s••••z~====2msaz3c====z••~====ms~~z========:= 
'"!'  IRR._otfrpui'""l.2 :"""'"i?roducd.on  of  OH  from  H02 

IRR_OUTPUT  H02toOH  •  HOXcyc[NEGONLY]  + 

<9>  + 
,.  2. O*H202_0ltcyc [POSONLY]; 

DESCRIPTION• 

'H02  to  OH'; 

IRR_OUTPUT  13:  Production  of  N02  from  H02 

I 
f••••••••••••••••••••••••cx===•=czz=====2==c•=====•~•a=======u••~•~===== 
IRR_OUTPUT  N02fromH02  •  <  9>; 

DESCRIPTION• 

'N02  FROM  H02'; 

IRR_OUTPUT  14:  Production  of  N02  from  R02 

I 
l•••••••••••••••••••s•••••••a•axa=====••••c====~c~ca:=====2caazz======cm 

IRR_ OUTPUT  N02fromR02  = 

<  57>  +  0.964*<  58>  +  0.92*<  60>  + 
<  65>  + 
<  68>  + 
<  71>  + 
<131>; 

<  64>  + 
<  67>  + 
<  70>  + 
73>  + 
< 

0.76*<  62>  + 
<  66>  + 
<  69>  + 
< 72> 
+ 

Exhibit 16-4.  Example PACP Commands for a Partial  IRR Analysis 

DESCRIPTION 

'N02  FROM  H02'; 

Jcn=a=mc==============3================================================= 
!  IRR_OUTPUT  15:  Production  of  PAN  and  TPAN 
!~==========c===£===============~~=3=====c=====3===:===================3 
IRR_OUTPUT  prodPAN_TPAN  =  PANcyc  +  TPANcyc; 

DESCRIPTION= 

'Production  of  PAN  and  TPAN'; 

l•=c================:£================================================== 
!  IRR_OUTPUT  16:  Net  Production  of  organic  nitrates 
!==2:c:a:=u============================================================= 
IRR  OUTPUT  netONIT  =  NET[ONIT]; 

DESCRIPTION= 

'Net  production  of  ONIT'; 

!a:ca:c==================c==========:=================================== 
!  IRR_OUTPUT  17:  Loss  of  voes  by  reaction  with  OH 

!========c=====cc:3~=============================================c====== 
IRR_OUTPUT  lossOH  HC 

[HO] 

LOSS[VOC]  AND 
+ 
<  30>  +  <  47>  +  <  48>  + 
<  49>  +  <  50>  +  <  51>; 

DESCRIPTION= 

'Loss  of  HC  plus  OH'; 

!:::u=a~c=n===================================:===============~========= 
!  IRR_OUTPUT  18:  Loss  of  OH  by  reaction  with  inorganics 
!================c:==c================================================== 
IRR_OUTPUT  lossOH_INORG 

7>  +  <  14>  +  <  15>  + 
< 
<  24>  +  <  25>  +  <  26>  + 
<  27>; 

DESCRIPTION 

'Loss  of  OH  with  !organics'; 

!=================================c===================================== 
I  IRR_OUTPUT  19:  Net  production  or  loss  of  HN03 
!:========:============================================================= 
IRR  OUTPUT  netHN03  =  NET[HN03]; 

DESCRIPTION= 

'Net  change  in  HN03'; 

!E3=za~a=c==========================================================:=== 
I  IRR_OUTPUT  20:  Loss  of  HCHO  by  reaction  with  OH 
l===z=============================================================c====% 
IRR_OUTPUT  lossHCHO_OH  =  LOSS[HCHO]  AND 

[HO] 

DESCRIPTION= 

'Reaction  OH  HCHO  with  OH'; 

!======================================================================= 
I  IRR_OUTPUT  21:  Loss  of  isoprene  by  reaction  with  OH 
l===============~=:~==c~c==========================================2==== 

Exhibit 16-4.  Example P ACP Commands for a Partial  IRR Analysis 

IRR_OOTPOT  lossISO_OH.  LOSS[ISO]  AND  [HO]; 

DESCRIPTION• 

'Reaction  of  ISO  with  OH'; 

J•••••••••••••••••••~••••D•3==za•:=======mm••~•====cccaaKmcz:=====E=~m•= 
:l"""'  IRRj)tiT'ptiT"'22:''''''''''production  of  new  H02  from  HCHO 
l•••••••••••••••••••••••••c=c2••••======•a~•~mc=====c%a••=c====%=~•na=== 
IRR_OOTPOT  newH02fromHCHO  a  PROD[H02]  FROM 

[HCHO]  AND  [hv]; 

DESCRIPTION. 

'New  H02  from  HCHO'; 

I  IRR_OOTPUT  23:  Production  of  H02  from  PAN 
l••••••••••••••••s••s•a•••~z==c••~3=====c•~=c======3asamc========••:=c== 
IRR_OUTPOT  H02fromPAN  •  PAN_AC03cyc[POSONLY]; 

DESCRIPTION•  'H02  from  PAN'; 

J•••••••••••••••••••=~=•••••zz::cucc•~====E•caccc=c====••••a=~=====••a== 
"i 

IRR_OUTPUT  24 :  Production  of  H02  from  R02  and  NO 

IRR_OOTPUT  H02fro:nR02_NO  a  PROD[H02]  FROM 

[NO]  AND  [R02]; 

. 

1111!!'· 
DESCRIPTION•  'H02  from  R02  and  NO'; 

."'" 

""'1111,;::11111111: 

,11:·111111 

''" 

,, 

1
' 

11111

".. 

,111111111111111:, 

"II'"'" 
11111111·•••••:~,,~.,~~~11:11:•-:11:11~••••••••===.~E••,,•=~===c•.:.:m:a====c•:mc=c====:caam•:m======= 
I  IRR  OtrrPtrr  25:  Production  of  H02  from  R02  and  R02 
l•••••••••••••••••••~••••••~====E•c•::a======•cK•======~=a•:s========z•cc=c 
IRR_OUTPUT  H02fromR02_R02  a  PROD[H02]  FROM  [R02]  AND  [R02]; 

, , , , '   -

,111·, 

·111~ 

1111111111. 

. 

,, 

,. 

,,, 

• 

DESCRIPTION• 

'H02  from  R02  and  R02'; 

I" 

Ill 

'"!iii:ll!!:1111, 

,, 

,,, 

l •••••••••••••••••••••••••••••==-=•••m•====-=~a••••&s~==••••~cc=====••&m.:~ 
'r'  IRR  cUTl>tri"""'"26: 
'i?roduction  of  R.62  from  OH 
! a-~•••~•••-~ •••••••~•c:::1':11"2'a•a'..~"a:c~.===2:c •a ::ac=== ==::2aam:a:z=:i====u9"maa:::1:==== 
x"R.a_OOTP1UT  R:o2fromOH  •  PROD [R02]  FROM  [HO]; 

. -

II 

,, ..  , 
· 

> 

, 

"" 

,,I' 

::11111 

111:'"":1 

"" 

F!ESCRIPTION 

I 

11

:::1111111111 

1,::11111 

'R02  from  OH'; 

111

'

11111, 

:11111' 

l••••••••••••••••••••••=c=•••••=====:c=a::a&======acm~aa•=======•c=~m====== 
'·  IRR_OUTPuT  27:  Production  of  HN03  from  OH  +  N02 
l••••••••••••••••••m3•a=::cc••==z=====a•m======c•2~ccc======~=c~ca~====== 
IRR._OUTPUT  H:No3frornoH_No2  = <  24>; 

DESCRIPTION•  'HN03  from  OH+  N02'; 

'f  IRR_OUTPOT  28:  Production  of  new  OH  from  H202 
)••••••••••••••••••••••c=-=••••s==c=c=czz======a~·~~=====a•••-=z=======c== 
IRR_OOTPOT  newOH_H202  s  2*H202_0Hcyc[POSONLY] 

DESCRIPTION 

•new  OH  from  H202'; 


Exhibit 16-4.  Example PACP Commands for a Partial  IRR Analysis 

l===========~~-K~~-CEc==~============================================z~= 
!  IRR_OUTPUT  29:  Production  of  new  OH  from  organic  peroxides 
!==================scna•z~naz============c=c======~~==~=======a:caKaa•c• 
IRR_OUTPUT  newOH_OPl 

OPl_OHcyc[POSONLY]  + 
OP2_0Hcyc[POSONLY]  + 
PAA_OHcyc[POSONLY]; 

DESCRIPTION  z 

'new  OH  from  OP1  OP2  PAA'; 

!  IRR_OUTPUT  30:  Production  of  OH  from  HONO 
!================================================c====================== 
IRR_OUTPUT  newOHfromHONO  =  HONOcyc[NEGONLY]; 

DESCRIPTION= 

'new  OH  from  HONO'; 

!  IRR_OUTPUT  31:  OH  Termination 
!===============================================c======================= 
IRR_OUTPUT  OHterm  = <  25>  +  <26>  +  <27>  +  <  50>  + 

HN03_0Hcyc[NEGONLY]  + 

HONOcyc[POSONLY]; 

DESCRIPTION 

'OH  Termination'; 

!===============================================c=======c===========~=== 
I  IRR_OUTPUT  32:  H02  Termination 
l======================================c================================ 
IRR_OUTPUT  H02term  % 

<  20>  +  <  27>  +  <101>  + 
2  •  H202_0Hcyc[NEGONLYI  + 

HN04_H02cyc[NEGONLY]; 

DESCRIPTION  2 

1H02  Termination'; 

!==========c===============================~=======================c==== 
!  IRR_OUTPUT  33:  Termination  of  H02  by  reaction  with  R02 
!c=====================================================c=========~=~a~cc 
IRR_OUTPUT  termH02_R02 

OP1_0Hcyc[NEGONLY]  + 
OP2_H02cyc[NEGONLY]  + 

PAA_OHcyc[NEGONLY]1 

DESCRIPTION= 

'H02  term  with  R02'; 

!  IRR_OUTPUT  34:  R02  Termination 
!===============================~=====c====z=======~=====z=~====~aa•~••• 
IRR_OUTPUT  termR02  =  .036*<58>  + 

.08*<60>  +  .24*<62>  + 

.03*<68>  + 

PAN_AC03cyc[NEGONLY]  + 

TPANcyc[POSONLY]; 

Exhibit i6:.~[  Example PACP Commands for.a Partial  IRR Analysis 

DSSCRIPTION.  1 R02  Termination'; 

l•••••••••••••••••••••••••••••••••&c••••~2••sz:2••==~•s==••z=•~acc:2•~== 
i  IRR~OuTPUT3S;Tamination of  R02  by  reaction  with  with  H02 

l••••••••••••••••••••••••••••••••z••••2~••2=••m•==••~==•••==••z::2•==:c& 
IRR_OUTPOT  termR02_H02 

OPl  OHcyc[NEGONLY]  + 
OP2=0Hcyc[NEGONLY]  + 
PAA_OHcyc[NEGONLY] 

DSSCRIPTION.  1 R02  Termination  with  H02'; 

".11111 

"'"' 

:1 

J•••••••••••••••••••••••••••••••••••a••&~••••=•••c=•••==z•~==2••===~a=== 
I  IRR_OUTPOT  361  Termination  of  R02  by  reaction  with  R02 
l•••••••••••••••••••••••••••••••••••a•••zs•••s=s••==••&::aaa:cc•~==~•••~ 

termR02_R02  • 

<102>  +  <103>  +  <104>  +  <105>  +  <106>  + 
<107>  +  <108>  +  <109>  +  <110>  +  <111>  + 

1.515  •  <114>  +  <115>  +  <116>  +  <117>  + 

<118>  +  <119>  +  <120>  +  <121>  +  <122>  + 

1.030  *  <i26>  +  0.500  *  <138>  + 
0.500  *  <139>; 

11111: 

11111·.::"1 

1 

:11!

:1:·,,1;:' 

;;.',1111 

;11. 
'11! 
,,'11111 
~ :  i 111 i 

r 
I!,.. 
DS~CRIPTION 
'II' 

. ... 

,, 

"'1111111:·1 

1l1111'·1ill111 

'R02  Termination  with  R02'; 

·, 

'lllllllli;,:i 

'llii 

1

111:iiii 

l:,l ••••••••••!.::'·•-=!· ....... .la•••••••••••••••"'""a••s"':a••==•••"'="'••"'==•~':;.=.,=•'"" 
'  1,.  IRR .... citri'l?tiis'  3 7;  L011s  of  OH  by  reaction  with  daughter  voes 
l•••••••••••••••••••••••••••••••••sza••••=z•••s=~•a~s=~•a==~•••===2•~=== 
IRR_OUTPUT  dauHC_OH  •LOSS  [HO]  JUlD  [dauHC]; 

I 

DESCRIPTION•  'OH+  daughter  HC'; 

'111 
I 

"·END PA; 

II' 

.:illl" 

16-36 

1 

,,1111

1111111111 

150..---------------------------~ 

EPA/600/R-99/030 

fl) 

140  0 3  Processes 
130  Charlotte, 28718 O'Brien Off 
120  -Plt'l2!tt-Mon~i,.,.to"'r-i::ee;alf.ll--- - - -
... 
110 
-E.  100 
..0 8::  90 
~ 80 
-e  10 
fl)  60 
Q)  50 
(,) e  40 
CL 
.0  30 
0.. 
Q.  20 
.2  10 
~  0 
CJ) 
~ -10 
x E -20 
-30 
-40 
-50 
4 

10 

6 

8 

12 

14 

16 

18 

20 

0 3  Processes 
Charlotte. 287FC O'Brien On 

aza-Monftor-6eli-----------

'-Predicted 

Mixing Ratio 

150 
140 
1SO 
... 120 
~ 110 
!boo 
~ 90 
80 
UJ  70 
UJ 
CD 
e 0.  50 
0  60 
.a  40 
0. a.  30 
0 
~  20 
a  10 
c 
0 
>< "E  -10 

-20 
-30 
-40 
-50'---'--_.__..___._ _  _.__.___._ _  _.___. _  __.__.....___. _  _,_ _  _._____..__~ 
20 

16 

14 

12 

10 

18 

6 

Hours 

4 

8 

Figure 16-1.  Example process analyses showing contributions of individual processes to model 
predicted concentrations (Source: Jeffries,  1996). 

16-37 

Iii 

·:••iilllllllllll: 

1111

::::1: 

" 

·:··:.:,11111111, 

,,111111111··,,li 

New York area 
10·16  EDT 

80Ian 

New Jersey area 
10 ·16  EDT 

EP A/600/R-99/030 

111 

111111111

1]11"'" 

:::11111111:: 

::::1111 

1111111111 

'1111111111:'",,, 

1111 

111111111111" 

1ro 
,,,,,,  1..0 
130 
120 
110 

'8: 100 ;o 
.-a  60 
Q.  70 
80 
"'  <i -g  ro 
40 
CIS  30 
"(:J  20 
Q.  10 
0 
·10 
·20 
·30 
·40 
-&> 
.ft() 

"' 1111 

,11, 

111111:, 

11111 

11111 

!lllllllli 

100 
uo 
80 
70 
'"""''I' eo 
1111111111111  § 50 
40 
11111111111111  ~ 30 
0  20 
1  <l° 
11111111111::o11,  ,,-g 
'  '  """···10 
0 
...  " CIS  ·10 

111111111111111""' 

er ·20 

~ ·30 
·40 
-~ 
·90 
·70 
-ao 

11'

111111111111111, 

1

Figure 16-2.  Example Process Analyses showing model predicted concentrations and process 
c0htributions for different model configurations (Source: Jeffries,  1996). 

16-38 

Pbotochemic-ai. Air .Pollution 

NO 

Emission' 

Nri = n~mberof._C)'c.leut E/e m:.1  f (~~P~ 

Ill = organic proGesses 

. 

0  = ihbtgahlc processes 

~ 

.  . 

Figure 16-3a.  Schematic of OH reaction with NO oxidation cycles (Source: Jeffries, 1996). 

Atlanta. GA 6/611988, Base Case 

4.46 Cycles per new OH 

propagation and termination 

32.0 ppb new OH 

t 

-

---------1  A VOC= 122.5 ppbV 
142. 7 ppb OH  (NO ... N02) I voe - 1.71 
reacted 

P oH=O. 776 

"l10.8ppbOH 
re-created 

[03lp=170.6 ppb 

87.4 ppb hom emiss. 
46.0 ppb lost by met 

.4ppbnewNO 

(0sli/103P] .. 0.951 

222.Sppb NO 
reacted 

PN0 =0.805 

A NOz=- 27.5 ppbV  ~!~b NO 
NO~N02 = 220.4 ppb  1---..,.r-~ 13.6 ppb 
[0s1react 
propaga11011 

24.7ppb 
(OlJentraJn 

13.6 ppb 
[O:ilc1epo 

5.13 Cycles per new NO 

[O:i]i 0:161.2 ppb 

Figure 16-3b.  Example photochemical cycle throughputs derived from integrated reaction rates 
(Source: Jeffries,  1995). 

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_Science_Ch_15.md) - [Home](README.md) - [Next Chapter >>](CMAQ_Science_Ch_17.md)<br>
CMAQ Science Document (c) 1999<br>

<!-- END COMMENT -->

`
