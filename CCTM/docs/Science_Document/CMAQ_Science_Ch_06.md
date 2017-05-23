<!-- BEGIN COMMENT --> 

[<< Previous Chapter](CMAQ_Science_Ch_05.md) - [Home](README.md) - [Next Chapter >>](CMAQ_Science_Ch_07.md)

<!-- END COMMENT -->

Chapter 6 
==============

GOVERNING  EQUATIONS  AND  COMPUTATIONAL  STRUCTURE  OF  THE 

COMMUNITY  MULTISCALE  AIR  QUALITY  (CMAQ)  CHEMICAL  TRANSPORT 

MODEL 

Daewon  W.  Byun  and  Jeffrey  Young

Atmospheric Modeling Division, 

National Exposure Research Laboratory 
U.S. Environmental Protection Agency 

Research Triangle Park, NC 27711 

M. Talat Odman

MCNC-Environmental Programs 

P.O. Box 12889, 3021  Cornwallis Road 

Research Triangle Park, NC 27709-2889, USA 

ABSTRACT 

The chemical transport model (CTM) of the Models-3/CMAQ (Community Multiscale Air 
Quality) modeling system can be configured to follow the dynamics of the preprocessor 
meteorological model.  A science process module in the CMAQ CTM is not specific to a 
coordinate system.  The generality is accomplished through the use of the coordinate 
transformation Jacobian within the CMAQ CTM.  In this chapter, we derive the governing 
diffusion equation in a generalized coordinate system, which is suitable for multiscale 
atmospheric applications.  We describe the CMAQ system's modularity concepts, fractional 
time-step formulation, and key science processes implemented in the current version of the 
CMAQ CTM.  We examine dynamic formulations of several popular Eulerian air quality models 
as emulated by the governing diffusion equations in the g1eneralized coordinate system.  Also, a 
nesting technique for the CMAQ CTM is introduced.  Finally, because the amount of a 
substance in the atmosphere can be expressed in many different ways, we summarize the most 
popular expressions for concentration and their transfomiation relations. 

·on assignment from the National Oceanic and Atmospheric Administration, U.S. Department of Commerce. 
Corresponding author address: Daewon W. Byun, MD-80, Research Triangle Park, NC 27711. 
E-mail: bdx@hpcc.epa.gov 
** On assignment from the National Oceanic and Atmospheric Administration, U.S. Department of Commerce. 
*** Present Affiliation: Georgia Institute of Technology, Atlanta, GA. 

GOVERNING EQUATIONS AND COMPUTATIONAL STRUCTURE OF THE 

6.0 
COMMUNITY MULTISCALE AIR QUALITY (CMAQ) CHE1.\1ICAL TRANSPORT 
MODEL 

' 

. 

. 

' 

II 

• 

' 

• 

In Chapter 5, "Fundamentals of Atmospheric Modeling ... " we discussed the :fundamental set of 
equations for atmospheric dynamics and thermodynamics in a generalized coordinate system.  In 
this Chapter, we investigate the diffusion equation for the trace species in the atmosphere in the 
generalized coordinate system and the computational structure of the Community Multiscale Air 
Quality chemical transport model (CMAQ CTM or, hereafter, CCTM). 

One requirement of the CMAQ modeling system is to maintain a consistent description of the 
atmosphere for different meteorological and chemical transport mo~els.  This is a feature that is 
essential for spatial scalability.  Various coordinate systems are used in atmospheric models. 
Selection of a suitable coordinate system is an important step of model formulation.  There are 
numerous criteria to be considered in selecting a coordinate system, such as the dynamic 
characteristics it can handle and how well it can deal with curvature of the earth's surface and 
features of the terrain.  Formulation of the models may vary substantially for different coordinate 
systems.  If a CTM can be formulated and coded using a generalized coordinate system, it would 
be easy to switch from one coordinate to another depending on the application.  The generalized 
coordinate concept is useful becau~e a single CTM formulation can adapt to any of the 
coordinates commonly used in meteorological models.  It is also desirable to compare the benefits 
of various coordinate systems and to be able to link the CTMs to meteorological models and 
databases in different coordinates. 

Conformity of the coordinates to the physics of the problem is very important.  Unlike a model 
with a fixed coordinate system, a generalized coordinate system allows use of generic coordinates 
for the specific science processes within a model.  Although the model's overall structure is 
determined by the choice of a coordinate system, the individual science modules can still use their 
own generic coordinates that best suit the. physical processes they model.  This means that each 
science process can utilize the parameterizations based on the best coordinate to represent the 
problem.  For example, the planetary boundary layer (PBL) parameterizations can be expressed 
in terms of geometric height, or dimensionless height scaled with PBL height, while for cloud 
physics, they can be represented in terms of pressure.  The linkages between the generic 
cootdinate parameterizations in the science processes and the governing conser\ration equation in 
the generalized coordinates are established through the application of appropriate coordinate 
transformation rules. 

Here, we intend to provide a comprehensive and rational development of the governing 
conservation equation in generalized coordinates, which can be readily implemented in an Eulerian 
model.  The operating assumptions used for the derivations are listed below (see Srivastava et al., 
1995). 

• 

Assumption 1: Pollutant concentrations are sufficiently small, such that their presence 
would not affect the meteorology to any detectable extent.  Hence, the species 
conservation equations can be solved independently of the Navier-Stokes and energy 
equations.  The conditions which could invalidate this assumption are for cases where 
sufficient heat is generated by chemical reactions to influence the temperature of the 
medium or where an atmospheric layer become so concentrated with pollutants that 
absorption, reflection, and scattering of radiation alter the air flow (Seinfeld, 1986). 

Assumption 2: The velocities and concentratfons of the various species in atmospheric 
flow are turbulent quantities and undergo turbulent diffusion.  Because turbulent diffusion 
is much greater than molecular diffusion for most trace species, the latter can be ignored. 

Assumption 3: The metric tensor that defines the coordinate transformation rules is not a 
turbulent variable.  This means that we Call define the coordinates based on the Reynolds 
averaged quantities.  The vertical grids will be defined incrementally between time steps 
when a time-dependent vertical coordinate is used. 

Assumption 4:  The ergodic hypothesi's holds for the ensemble averaging process.  This 
means that the ensemble average of a property can be substituted with the time average of 
that property. 

Assumption 5: The turbulence is assumed stationary for the averaging time period of 
interest (say 30 minutes to one hour for atmospheic applications). 

Assumption 6:  The source function (i.e., emissions of pollutants) is deterministic for all 
practical purposes and there is no turbulent component. 

Assumption 7: The effect of concentration fluctuation on the rate of chemical reaction is 
negligible, i.e., contributions of covariance effects among tracer species are neglected. 

Assumption 8:  Because the large-scale motions of the atmosphere are quasi-horizontal 
with respect to the earth's surface, science processes can be separately represented in 
horizontal and vertical directions (i.e., quasi-orthogonal in transformed coordinates). 

• 

• 

• 

• 

• 

• 

• 

## 6.1 Derivation of the Atmospheric Diffusion Equation 

In Chapter 5, we derived the species continuity equation in generalized coordinates.  It is given 
as: 

(6-1) 

where <fJ;  is the trace species concentration in density units (e.g., kg m"3
Jacobian of the terrain-influenced coordinates, m is the map scale factor, \I:,  and 8  are horizontal 
and vertical wind components in the generalized coordinates, and Qcp, 
is the source or sink term. 

),  J.  is the vertical 

To make the instantaneous species continuity equation useful for air qtiality simulation, we need 
to derive the governing diffusion equation.  This is done by decomposing the variables in 
Equation 6-1, except for the Jacobian and map scale factor, in terms of mean and turbulent 
components.  The Reynolds decompositions of species concentration and mixing ratio are 
expressed as: 

<pi = <pi + <p, II 

<pf + <p, II= "iftp +qi II 75 + 7f;pll +qi II p" 

(6-2a) 

. (6-2b) 

(6-2c) 

p 

where  q1 = <p,  is the species mass mixing ratio and a stochastic quantity is decomposed into 
mean , O, and turbulent,  ("), components.  Stationary turbulence assumption 5 implies that a 
turbulent component has a zero mean for the averaging period.  Following Venkatram (1993), we 
can estimate the mean and turbulent components of species and air concentrations as 

q,llp" 
--=--<<1 
q,p 

p112 
(/5)2 

(6-3a) 

(6-3b) 

(6-3c) 

Without loss of generality, we redefine the terrain-influenced vertical coordinates with a 
coordinate  X3  , whose value is increasing monotonically with height, as: 

x3 = ~ = {  s 
1- s 

(if  s  increases  with  height) 
(if  s  decreases  with  height) 

(6-4) 

The choice of a generalized vertical coordinate which increases monotonically with height 
simplifies the derivation of the governing equation and thtls reduces the likelihood of making sign 
errors in the formulas and in computer codes.  The transformation does not change the horizontal 
wind components or the Jacobian, which is always defined to be a positive quantity.  Hereafter, 
the subscript s is replaced with ~ to reflect modification of the vertical coordinate.  Subsequently, 
the vertical v~locity is represented with v3 = d~ I dt = g, which is positive for upward motion. 
Application of decomposition of velocity components in Equation 6-1  and ensemble averaging 
produces Reynolds flux terms in the mass conservation equation as: 

(6-5) 

where we used  Jf.  = Jf. and  ~' = ~' based on Assumption 3 and Assumption 6, respectively. 
The Reynolds flux terms in Equation 6-5 can be approximated in terms of the mixing ratio. as: 

'lf;p"Vf," << 1 

(6-6a) 

.·  (6-6b) 

(6-6c) 

(6-6d) 

in which we have neglected the second order perturbation terms based on the scale analysis of the 
equations.  Equation 6-5 can be rewritten using Equations 6-3a-c and 6-6a-c to give: 

+mzv  •  p  ' 

.  .q."V "J  ]· 
(

f, 
m2 

f,  + 

c 

iJ(pq."v3"J) 

.  , 

~3 

c  = J  n 
. 
f.~1 

. . (6-7) 

The turbulence flux terms can be parameterized using a simple closure scheme such as the eddy 
diffusion concept (K-theory):. 

"'31  (}Q.. 
--,..-
q."u "=-K  _'2'._• • q."v "=-K  _'2'._•.  q."v  "=-K  _'2'._• 
!;  · 
~l 
' 

"21  (}a.  --;-3 

--,..-
!; 
·  1. 

"II (Ja.. 

~I ' 

~l ' 

I 

I 

(6-8) 

where  K.11 .denotes the eddy diffusivity tensor in the transformed coordinate.  The eddy 
diffusivity tensor for the generalized meteorological coordinates is related to the diffusivity 
tensor in Cartesian coordinates as: 

:'.\..~k 

Kkl  = _o;i;_. _o;i;_. Kii 

:i..~l 

. 

ax·  ax1 

(6-9)• 


If we postulate that the diffusivity tensor in Cartesian coordinates is diagonal (i.e., all the off(cid:173)
diagonal components vanish), then the eddy diffusivity tensor in the generalized meteorological 
coordinates becomes: 

m2K 
xx 

0 

K= 

0 

ax3 

m dx.Kxx  m-K 

yy 

m2K 
yy 
ax3 
dy 

m-K 
xx 

ax3 
ax 
ax3 

m dy  KYY 

(6-10) 

('!1 )2 Kxx  + (~ )2 KYY  + (~ )2 Ku. 

where  Kxx = K 11

,  KYY  = K22 , and  Ku.  = K33  are the diagonal components of eddy 

diffusivity tensor in the Cartesian coordinate.  To match with the computational grid, the gradient 
terms in Equation 6-10 must be rewritten in terms of the generalized coordinates  .£3  (defined 
based on height above ground  hAGL  = h- z.ifc, where  z.efc  represents the height of topography) 
using the appropriate chain rules, for example,  ~ ( ~) z = (:;; ) x
= x3

:  ( ;  ) ( : ;  )  x

•  When A 

,  we get 

-

3 

3

m2K xx 

0 

K= 

0 

m2K yy 

(6-11). 

Then the non-zero diffusion terms in Equation 6-7 can be parameterized with the eddy diffusion 
theory as follows: 

and 

(6-12) 

(6-13) 

Rewriting Equation 6-7 with Equations 6-12 and 6-13, and separating the diagonal and off(cid:173)
diagonal diffusion terms with an explicit description of the source terms, one can obtain the 
governing atmospheric diffusion equation in the generalized coordinates where the turbulent flux 
terms are expressed with the eddy diffusion theory: 

EP A/600/R-99/030 

(a) 

(b) 

(c) 

_  z~[P1~ cK.11  aqi)]- 2_}_[P1~ cftzz  aqi )J-~[-1 cK.33  ~ )] 
m axi  m2 

axi  m axz  m2 

ax3  p  ~ 

axz 

ax3 

(d) 

(e) 

_  2 ~[pl~ cK.13  aqi )]- 2 _}_[Pl~ ck23  aqi )] 
m axi  m2 

JX3  m axz  m2 

ax3 

(t) 

(g) 

-

-

=l~~;(<fJp••••<fJN)+l~(4,; +  dt 

d(qi;l~) 

d(<p;l~) 

+  dt 

d(<p;l~) 

+ 

dt 

(6-14) 

cltl 

aero 

ping 

(h) 

(i) 

(k) 

(1) 

The terms in Equation 6-14 are summarized as follows: 

(a) time rate of change of pollutant concentration; 
(b) horizontal advection; 
( c) vertical advection; 
( d) horizontal eddy diffusion (diagonal term); 
(e) vertical eddy diffusion (diagonal term); 
(t) off-diagonal horizontal diffusion; 
(g) off-diagonal vertical diffusion; 
(h) production or loss from chemical reactions; 
(i) emissions; 
G) cloud mixing and aqueous-phase chemical production or loss; 
(k) aerosol process; and 

(1) plume-in-grid process. 

Note that the dry deposition process can be included in the vertical diffusion process as a flux 
boundary condition at the bottom of the model layer: 

Alternatively, we can express the turbulent flux terms in Equation 6-7 using the Reynolds flux 
terms defined as: 

· 

q II u 11  = frt  q. 11 v 11  = fr2  q 11V311 = fr3 

i 

~ 

91  ' 

and the turbul~nt flux terms are related with the Cartesian counterpart using  ~~ = :i F j, : 

.  "-k 

91  ' 

~ 

q; 

I 

I 

(6-15) 

F" I  _ 
91  - m  q, ' 

F,;  p" 2  _ 

q;  - m  q;  ' 

FY  p" 3  _  (-OA-)F,; 

q,  -

'.l.~3 

ax 

'.l.~3 

'.l.~3 

(-OA-)FY 

q,  +  ()y 

q,  +  (}z 

(-OA-)Fz 
q, 

(6-16) 

In comparison with Equation 6-14, the Reynolds flux terms shown in Equation 6-15 include the 
off-diagonal components.  One can now rewrite the governing conservation equation for trace 
species equivalently to Equation 6-14 in terms of the Reynolds flux terms: 

2 a [pl~ "1 J  2 a [pl~ "2]  a [- "3] 
. +m  dXl  m2  ~I  + m  dX2  m2  ~I  + dX3  pJ~~i 

(6-17) 

The governing equation can be simplified for a domain with gentle topography for which one may 
ignore all the terms involved with the horizontal gradients of the surface normal to the vertical 
coordinate.  This forces the vertical diffusion terms in the curvilinear coordinate system to be 
identical to those of the orthogonal Cartesian coordinate system. Then the trace species 
conservation equation can be written in a simpler form: 

=  Gr" R  (<p 

"\/ "{ 

'Pt 

<p  ) +  Gr" s  + iJ(cp;)I  + iJ(cp;)I  + iJ(cp;)I 

1' ... ,  N 

"\/ f 

'P1 

at 

cld 

at 

. 

prng 

at 

aero 

(6-18) 

where cp; =..[Yep;= U1:, I m2 )<fJ;·  In writing Equation 6-18, we have explicitly identified terms to 
directly relate to science process modules implemented in the CMAQ.  Equation 6-18 is similar 
to the conservation equation in the generalized coordinates as suggested by Toon et al. (1988). 

## 6.2 Representation of Science Processes  in  CMAQ Modeling System 

This section describes how the CMAQ modeling system is structured to accommodate many 
different science process modules that provide a one-atmosphere, multiscale and multi-pollutant 
modeling capability to the CMAQ system.  First, we describe the modularity concepts and key 
science processes implemented in the current version of CMAQ.  Then the governing fractional 
time-step formulation for each science process is presented. 

### 6.2.1  Supporting Models and Interface Processors 

Key supporting models for the current version of the CMAQ modeling system are the 
Mesoscale Model Version 5 (MM5) (Seaman et al.,  1995)and Models-3 Emissions Processing 
and Projection System (MEPPS).  The CMAQ modeling system is comprised of the main 
CMAQ chemical transport model (CCTM) and several interface processors that link other model 
input data to the CCTM.  The Meteorology-Chemistry Interface Processor (MCIP) processes 
MM5 output to provide a complete set of meteorological data required for CCTM.  MCIP is 
designed in such a way that other meteorological models can be linked with minimal effort.  Initial 
and boundary conditions are generated with the ICON arid BCON processors, respectively, and 
the Emissions-Chemistry Interface Processor (ECIP) combines area and point source emissions 
to generate three-dimensional gridded emissions data for CCTM.  In addition, a plume dynamics 
model (PDM) is used to provide dimensions and positions of plumes from major elevated point(cid:173)
sources.  The PDM data are used for driving the plume-in-grid processing in CCTM.  A 
photolytic tate constant processor (JPROC) which is based on the RADM (Chang et al.,  1987) 
approach, computes species specific photolysis rates for a set of predefined zenith angles, 
latitude, and altitudes.  An alternative detailed-science version adopts state-of-the-science 
radiative transfer models that can take into account the total ozone column (TOMS data) and 
turbidity.  Refer to Table 6-1  for the list of the interface processors in CMAQ and Figure 6-1  for 
the data linkage among these interface processors. 

By assembling appropriate science modules available in the CMAQ system, users can build a 
specific CCTM' s that may include all or some of the critical science processes, such as 
atmospheric transport, deposition, cloud mixing, emissions, gas- and aqueous-phase chemical 
transformations, and aerosol dynamics and chemistry.  One of the features of CMAQ that 
distinguishes it from other air quality models is the hierarchical functional modularity of the 
science processor codes.  We define the levels of modularity in the science model based on the 
granularity of the modeling components.  The coarsest level of modularity is the distinction 
between the system framework and science models.  The second level is the division of science 
sub-models (MM5, MEPPS, and CMAQ).  The third level of modularity involves a driver 
module, processor modules, data provider modules, and a utility module (a collection of assisting 
subroutines) in a CTM.  While the emissions processing and the meteorology model are modular 
at the second level, the CCTM achieves the third-level of modularity by employing the operator 
splitting, or fractional time step, concept in the science processes.  The next level of modularity 
is based on th~ computational functionality in a processor module, e.g., science parameterization, 
numerical solver, processor analysis, and input/output routines.  The lowest meaningful 
modularization level is the isolation of sections of code that can benefit from machine dependent 
optimization. 

### 6.2.2  Modularity Concept of CMAQ 

To allow for both the continuous improvement of science and for the addition of new capabilities 
in. a unified fashion, it is critical to have efficient modular schemes in the CMAQ design. 
Currently, the modularity within CMAQ is based mostly on the fractional time-step 
implementation of the science processes.  This level of modularity involves the distinction of a 
driver, processor modules, data provider modules, and utility subroutines in CMAQ.  We have 
chosen this method because it provides a natural disciplinary distinction for different science 
processes through which developments in specific research areas can readily be incorporated 
(Refer to Figure 6-2). 

In some of the process modules, such as the aqueous-phase chemistry module, the science 
algorithms and numerical solvers are tightly linked.  For other types of modules, the science 
parameterization components and numerical solvers have a looser association.  In such cases the 
modularity can be defined either at the parameterization level, the numerical algorithm level, or 
both.  For example, the module definition for the advection process is based on the numerical 
advection algorithm used.  For the gas-phase chemistry process, the modularity is based on the 
ordinary differential equation numerical solvers.  The chemistry mechanism description is 
generalized aiid the Models-3/CMAQ framework provides a straightforward method to link 
model species surrogate names with the species names in the data set.  See Chapter 15 for details. 
The use of different chemical mechanisms is accommodated through the mechanism reader and 
generalized codes for setting up the production and loss terms of the chemistry reactions. 
Therefore, the CCTM does not require different gas-phase chemistry modules for different 
mechanisms. 

The vertical cliffusion process can be formulated using either local- or non-local-mixing 
parameterization schemes.  The current classification of vertical diffusion modules is based on the 
process parameterization methods.  The modularity of this process can be enhanced if we 
distinguish the method used for computing the vertical diffusivities for local-mixing.  In this case, 
the modularity is defined at the level of data provider modules.  The modularity level can be 
deepened further if we identify different numerical solution methods for the diffusion. 

With the current version of CMAQ, the level of science modularity is subordinated by the way 
the science process codes are archived in the system.  Here, we define a class as a collection of 
cllfferent mogllles for a given science process.  The science classes are identified with the grouping 
of the terms in the governing conservation equation, Equation 6-18. ·Currently, nine science 
process classes are defined in CCTM: 

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

DRIVER controls model data flows and synchr<;mizes fractional time steps; 

HADV computes the effects of horizontal advection; 

V ADV computes the effects of vertical advection; 

ADJ CON adjusts mixing ratio conservation property of advection processes; 

HDIFF computes the effects of horizontal diffusion; 

VDIFF computes the effects of vertical diffusion and deposition; 

CHEM computes the effects of gas-phase chemical reactions; 

CLOUD computes the effects of aqueous-phase reactions and cloud mixing; 

AERO computes aerosol dynamics and size distributions; and 

PING computes the effects of plume chemistry. 

CCTM does not have emissions as a separate science process because it can be either a part of 
the vertical diffusion or the gas-phase chemical reaction proces.s.  It is worthwhile to mention 
here that the current modular paradigm does not prevent establishment of combination of 
processes in a larger single module.  For example, one can develop a module describing the vertical 
transport, chemistry, and emissions simultaneously when time scales of those processes become 
comparable.  Users could experiment with the combination of modules to best fit to their 
problems at hand. 

In addition to nine science process modules, CCTM includes two .science process. classes.  The 
PHOT computes photolysis rates, and AERO_DEPV computes particle size-dependent dry 
deposition velocities.  These are typical "data-provider" science process classes, which do not 
involve updating concentrations directly.  There are some other classes that do not fall in any of 
the above definitions.  We have grouped these auxiliary routines as the UTIL class, which is a 
collection of utility subroutines.  As one can see, the current modularity of the CCTM is 
implemented more on a practical basis rather than by strictly following a design paradigm.  One 
can also see that the present modularity definition of CN.[AQ is somewhat subjective.  In the 
future we intend to allow definition of the modularity at the user-defined granularity level. 

Figure 6-1  describes the key science process modules in CCTM and their data linkage with 
CMAQ's preprocessors, whose descriptions are available in other chapters.  The only data 
dependencies among the CCTM science modules are the trace species concentration field as seen 
in the diagram and the model integration time step.  Figure 6-2 shows the distinct data 
dependencies within the CCTM.  To facilitate modularity and to minimize data dependency in 
CCTM, we store concentrations in global memory while the environmental input data are 
obtained from random-access files and interpolated to the appropriate computational 
(synchronization) time step.  This realizes the recommended "thin-interface" structure of the 
model: 

• 

• 

• 

• 

• 

Common timing data are managed through the science process main subroutine's call 
arguments; 

Conceµtrations are. the object of all process opera~ions; 

Environmental data are provided through a standard 1/0 interface; 

Model stru~ture data are provided through shared include files; and 

Standard physical constants are obtained from shared include files. 

See Chapter 18 for further details on how the science codes are integrated in the Models-3 
CMAQ  system. 

CMAQ  Science  Processors 

ECIP 

I 

.  I 

~Boundary 
~ I Conditions 

M Cl P 

I 
I 

r. Meteorology 
I 
I 

I 

G 

Figure 6-1.  Science Process Modules in CMAQ.  Interface processes are shown with rectangular 
boxes.  Typical science process modules are updating the concentration field directly and the data(cid:173)
provider modules include routines to feed appropriate environmental input data to the science 
process modules.  Driver module orchestrates the synchronization of numerical integration across 
the science processes. Concentrations are linked with solidi lines and other environmental data with 
broken lines. (From Byun et al.,  1998.) 

Table 6-1.  Interface Processors for the CMAQ Modeling System 

Interface 
Processor 
ICON 

B.CON 

ECIP 

MCIP 

JPROC 

PDM 

Description 

Reference 

Chapter 13 

Provides initial three-dimensional fields of trace species 
concentrations for modeling domain 
Prqvides concentrations of trace species for the boundary  Chapter 13 
cells 
Incorporates emissions from separate area and major 
point sources to generate hourly 3-D emissions input file 
Processes the output of a meteorological model to 
provide the necessary meteorological data for CMAQ 
models 
Computes photolysis rates for various altitudes, 
latitudes, and sun zenith angle 
Generates plume information needed to apply plume-in-
grid (PinG) processing in CCTM 

Chapter 9 

Chapter 3 

Chapter 12 

Chapter 14 


### 6.2.3  Description of Science Processes 

In this section we describe individual science processes, shown in Figure 6-1, associated with the 
groups of individual terms in the governing diffusion equation.  Note that different concentration 
units are used for different science processes in CMAQ CTMs.  Appendix 6A provides the 
relationships among the concentration units and their conversion factors from one unit to another. 

CMAQ CTM  's Data Flow 

Output 

IC 
Data 

Figure 6-2.  Data Dependencies Among Modules in CCTM.  P and Sk represent a science process 
module and the related subroutines for the module, respectively. (From Byun et al.,  1995.) 

#### 6.2.3.1 Driver Class for CCTM 

The key function of the driver class module is hosting the science processors.  It is responsible 
for coordinating model integration time (synchronizing :fractional-time steps of science process 
call) and some input/output sequences.  The driver struc1ure of the current CCTM is given in 
Figure 6-3.  A synchronization time step is used to ensure the global stability of the CCTM's 
numerical integration at the advection time step, which is based on a Courant number limit. 
Nesting requires finer synchronization time steps for the fine grid domain.  The CCTM's process 
synchronization time steps are represented as integer seconds because the Models-3 I/O API can 
only handle integer seconds for I/O data.  All the needed data are appropriately interpolated 
based on the synchronization time step.  For maintaining numerical stability and for other 
reasons, an individual process module may have its own internal time steps.  In general, each 
science process module uses the synchronization time step ( /),.t.rync)  as the input time step of 
required environmental data.  The global output time steps can be set differently from the 
synchronization time step.  Usually, the output time step (~t0"1 ) is set as one hour,  but sub(cid:173)
hourly output down to the synchronization time step is possible. 

Table 6-2.  List of Science Process Subroutines Called by the CMAQ Driver 

• 

Description 
Sets up pointers for different concentration species: gas 
chemistry, aerosol, non-reactive, and tracer species 
Initializes simulation time period, time stepping constants, and 
concentration arrays for the driver 
Computes the model synchronization time step and number of 
repetitions for the output time  step 
Converts units and couples or de-couples concentration values 
with the density and Jacobian for transport 
Controls all of the physical and chemical processes for a grid 
(currently, two versions are available: symmetric and 
asymmetric around the chemistry processes) 
Computes advection in horizontal plane (x- and y-directions) 

Subroutine 
CGRID  MAP 

Science Class 
UTIL 

INITSCEN 

INIT* 

ADV STEP 

DRIVER 

COUPLE/ 
DECOUPLE 
SCIPROC 

COUPLE* 

DRIVER 

XADV, 
YADV 
ZADV 

HADV 

VADV 

ADJ ADV 

ADJ CON 

HDIFF 
VDIFF 
CHEM 
P~NG 
AERO 

HDIFF 
VDIFF 
CHEM 
PING 
AERO 

Computes advection in the vertical direction in the generalized 
coordinate system 
Adjusts concentration fields to ensure mixing ratio 
conservation given mass consistency error in meteorology data 
Computes horizontal diffusion 
Computes vertical diffusion and depositio~ 
Solves gas-phase chemistry 
Computes effects of plume-in-grid process 
Computes aerosol dynamics, particle fo1mation, and 
deposition 
Computes cloud mixing and aqueous ~!;_emistry 

. 

CLDPRC 
•represents a process class that is part of DRIVER function. 

CLOUD 

6-16 

CMAQ-DRIVER 

Asymmetric SCIPROC 

Symmetric SCIPROC 

EP A/600/R-99/030 

CGRID_MAP 

} 
.-----_,Time Step} 

Output 

/),.tout 

• 
; 
' 

I 
} 

! 
i 
! , 

XADV* 

L1~<ync 

~ 
i 
I 
t 
' 
l 
l 
' l 

IiECOUPLE 
~ .. -:;;:;-=---;e::····::::: ... ~ Litsync 

QDIFF 

QING 

312::::::::, 

UHEM 

.----~-----.! 

Synchronization 

Time Step L1~<ync  i 

SCIPROC  I, 

QERO 

.......... 

.......... 

~.... 

* Alternating 
XADV and YADV calls 
for each asymmetric 

"· ....  SCIPROC call 

.......... 

'-.. 

.. .. ..._,,, 

;... . ., 

ADJ CON 

';&~~~~~~~~~___, 

Figure 6-3.  Driver Module and Its Science Process Call Sequence. 
Both asymmetric and symmetric call sequences in SCIPROC are presented. ·/),.tsync  and  /),.tout  are 
model synchronization and output time steps, respectively. Refer to Table 6-2 for the description 
of the subroutines. 

The DRIVER program calls initialization routines to set up CCTM runs.  It initializes the 
concentration field and checks ifthe input files, run time, and grid/coordinate information are 
consistent for a given scenario.  Subroutines used for the jnitialization process are grouped into 
the INIT class.  Usually, initial concentrations for gaseous species are in molar mixing ratio units 
(ppm) and aerosol species in density units (µg  m-3), the same as the output units of CMAQ. 
Also, DRIVER calls couple/decouple subroutines to convert concentration units for appropriate 
data processing.  The pair of couple/decouple calls, which are available in the class COUPLE, 
limit the interchange of process modules between two different concentration units, such as 
density versus mixing ratio.  The classes INIT and COUPLE are introduced just for the 
convenience of code management from the point of view of science process modularity, and they 
should be considered as part of the DRIVER class modules. 

#### 6.2.3.2 Advection Processes for CCTM: HADV, V ADV and ADJCON 

For convenience, the advection process is divided into horizontal and vertical components.  This 
distinction is possible because the mean atmospheric motion is mostly in horizontal planes. 
Usually the vertical motion is related with the interaction of dynamics and thermodynamics.  The 
advection process relies on the mass conservation characteristics of the continuity equation: 

Using the dynamically and thermodynamically consistent meteorology data from MCIP, we can 
maintain data' consistency for air quality simulations at the synchromzation time step.  In case 
. 
the meteorological data provided and the numerical advection algorithms are not exactly mass 
consistent, we need to solve a modified advection equation: 

(6-19) 

(6-20) 

where  QP  is mass' consistency error term (Byun, 1999).  Equation 6-20 ensmes conservation of 
mixing ratio, which is a necessary (though not sufficient) condition for preserving total tracer 
mass given significant fluctuations of density field in space and time.  The equation shows that 
the correction term has the same form as a first-order chen}ical reaction whose reaction rate is 
determined by the mass consistency error (normalized with air density) in the meteorology data. 

Modules in r.IADV class solve for the horizontal advection: 

a(jf"ip;) __ n 

(  E-y" ) 
v ~ •  "rep;  ~ 

at 

-

(6-21) 

and modules in V ADV class solve for the vertical advection with boundaiy conditions v3  = 0  at 
the bottom and top of the model. 

a<$q;;) =  ac$"fP;~) 

at 

a_x3 

(6-22) 

In simulating air quality, one of fundamental characteristic of the model application should be 
conservation of mass.  Therefore, the modules in the ADJ CON class solve for the mass 
correction term: 

(6-23) 

We wish to emphasize that the artificial distinction of advection modules between horizontal and 
vertical processes is not adequate and that all three modules (HADV, V ADV, and ADJCON) 
should be considered as an integral unit for solving the physical advection process of trace 
species.  The advection and mass adjustment algorithms are described in detail in Chapter 7. 

6.2.3.3 Diffusion Process Classes for CCTM: HDIFF ~md VDIFF 

For convenience the atmospheric diffusion process is divided into horizontal and vertical 
components.  This distinction is needed because the vertical diffusion mostly represents the. 
thermodynamic influence on the atmospheric turbulence by the air-surface energy exchange 
processes while the horizontal diffusion represents subgrid scale mixing due to the unresolved 
wind fluctuations.  To handle the atmospheric diffusion processes in the generalized coordinates, 
we need to carefully examine the governing equation to properly set up the diffusion solver. 

We start from the atmospheric diffusion equation in the same concentration units as used in 
advection: 

d<p; 
at  diff 

diff 

(6-24) 

where  <p;  = .fYcp;, and the term (Q~ Ip)  is the time rate change of mass mixing ratio due to 
emissions of species i.  Initially, it is assumed that we can. decompose the diffusion into the 
horizontal and vertical components with respect to the curvilinear coordinates: 

acp;I  =a( $i5?f;)  = _ ac.fYi5~) +  G-(Qrp, J 

'.:}. 
ai  vdiff 

'.:}. 
at 

i~3 
OA 

"\/ r P 

P--

(6-25) 

(6-26) 

vdiff 

Emissions can be included either in vertical diffusion or gas-phase chemistry module.  If we can 
parameterize the turbulent fluxes directly in the curvilinear coordinates, we can implement 
HDIFF and VDIFF modules following Equations 6-25 and 6-26.  When the turbulent fluxes are 
parameterized with eddy diffusion theory, the contributions of the off-diagonal (cross(cid:173)
directional) diffusion terms show up explicitly as shown in Equation 6-14: 

acp; I  = _}__[  G-cK.13  aqi )] 
at 

'VYP 

axi 

. 
cdiff 

_!____[  G-cK.23  dil )]. 

ax3  + ax2 

'VYP 

ax3 

6-19 

BP N600/R-99/030 

, 

, 

';1,,I' 

,, 

"32  aqi  J 
,,  a [ rz- "31  aqi 
+ ax3  -vrPCK  ax' +K  ax2) 

(6-27) 

For a domain with a significant topographic feature, the module CDIFF must be implemented. 
However, the current CMAQ version does not include CDIFF module as the off-diagonal terms 
are often neglected in operational air quality models.  In such a case, the HDIFF and VDIFF 
modules so1ve for diagonal terms (with respect to the curvilinear coordinates) as follows: 

~I  = ~[ rz-ct" (}(f; )]  ~[ rz-ct22 aqi )] 
ailhdiff 

ax'  + ax2  "'r P 

axi  "'r P 

ax2 

a<p; 
at  wli/f 

= ~[ rz-cf(33 aqi )] 

ax3  "'r P 

ax3 

(6-25') 

(6-26') 

Compared with above formulations, let's consider the case that we approximate the quantity 
.[-975, which defines the computational grid, to be constant for the duration of synchronization 
time step for integrating the diffusion process with the fractional time-step method.  Then, the 
problem becomes equivalent to solving for the diffusion equations In terms of the mass mixing 
ratio instead of density: 

aq,,  =-V  •[F  ]- acfr~) +({4,')-F  •V [inc  1Zy"p-)]-fr3 a[inc.Jri»] 
at  diff 

ax3 

-yr, 

75 

ax3 

~ 

q, 

q, 

~ 

q, 

(6-28) 

Ifwe rely on Equation 6-28 for representing the atmospheric diffusion process, the concentration 
must first be decoupled to obtain mass mixing ratio, q1•  Once the new mixing ratio is computed, it 
needs to be coupled with .[-975 to give the updated concentration in terms of <p;.  This means 
that the operator for the horizontal diffusion process should compute: 

and the vertical diffusion process should solve for: 

(6-29) 

(6-30) 

This approach is more convenient in numerically solving the flux-form turbulence mixing 
representation because most of the flux-based closure algorithms use parameterizations of 
turbulent fluxes in terms of conserving quantities, such as the mass mixing ratio, q1•  A 
considerable amount of meteorological and air quality literature on turbulence diffusion fails to 
clarify this important point.  Especially for the case of multiscale applications, the representation 
of diffusion in terms of a conserving quantity is critical as shown by Venkatram (1993). 

6-20 

' 

The effects of turbulence flux caused by the divergence of the grid boxes in the coordinate system 
need to be included in order to describe the turbulence exchange processes precisely.  One can 
readily show that the coordinate-divergence term in Equation 6-30 vanishes for a mass conserving 
vertical coordinate.  Similar Ly,  when topographical feature~s vary significantly and horizontal 
variations of the quantity .Jr75 are large, one cannot n~glect the last term in Equation 6-29. 
Chapter 7 of this document describes physical parameterization schemes and numerical 
algorithms for the horizontal and vertical diffusion processes· in the CCTM. 

One may wonder how deposition should be represented in the generalized coordinate system.  In 
Eulerian air quality models, the deposition process affects the concentration in the lowest layer 
as a boundary flux condition.  Considering the deposition process as the diffusion flux at the 
bottom of the model, we can relate the boundary condition in the generalized coordinate system 
to that of the Cartesian coordinate system as: 

(6-31) 

because  Fq~I 
concentration is accounted for by the following relationship: 

and  FqYI 

•  dep 

•  dep 

· 

do not exist.  Then, the effects of dry deposition on the species 

acp; I  _ a( .fY75<L) 
at  dep 

at 

= 

dep 

dep 

a ( rz-cax3)F7.) 

= - ax3  -vrP  az 

q; 

dep 

(6-32) 

where  hd•p  = (;,)  (Af 3

)dep  is the thickness of the lowest model layer in the geometric height 

dep 

coordinate.  In the derivation of Equation 6-32, we assume that the deposition flux is constant in 
the lower part of the surface layer (i.e., a constant flux layer).  Thus, the deposition velocities are 
computed at the middle (in terms of the generalized coordinate, g) of the lowest model layer at 
which the concentrations are represented.  For the case in which the mass mixing ratio is used as 
the concentration variable for solving the diffusion equation, the deposition should be 
implemented as a boundary condition for the vertical diffusion (VDIFF) in the following manner: 

;;qi I  _  ac~) 

'."\. 
at  dep 

-

-

:t.~3 
ax· 

dep 

a ( ax3 

= - :t.~3 
ax 

·  ) 
(  1. )F:i~ 

u~ 

. 

d 
ep 

(6-33) 

Therefore, the bottom boundary condition for the VDIFF module is given as: 

6-21 

~I  =-:X3 (c~ )~~)  = 

""' 

dtp 

(Fz) 

q;  dtp 

) 

(ax3
- -
(}z. 
(L1£3) 

bonnm 

, 

tfep 

z 
F 
h 

= ( q; t,p  = _ V d  -1 
h  qi  /aytrl 

. 

, 

dtp 

.. dtp 

(6-33') 

Equations 6-32 and 6-33 show that we do not need to estimate contravariant deposition 
velocities if the deposition process is implemented as a bottom boundary condition in the 
generalized coordinate formulation. 

In the current CCTM implementation, the concentration units for horizontal and vertical 
M 
diffusion processes are density (coupled with Jacobian) and niolar mixing ratio,  m; =  q;----1lli:.  , 
M; 

c:,,.. 

,,,, 

. 

. 

. 

. 

. 

,,,,, 

respectively.  We have chosen m;  as the generic concentration unit for the vertical diffusion to 
coordinate with the emissions units in the data .. Subsection 6.2.3.6 provides a detailed 
explanation for this.  Therefore, HDIFF i's placed.outside and VbIFF is placed in between the 
pair of couple/decouple calls.  Because the ratio of molecular weights are constant, equations for 
the vertical qiffusion in terms, of molar mixing ratio are equivalent ~() those in terms of mass 
mixing ratio,  q1 •  Refer to Chapter 7 for details of the computational algorithms for HDIFF and 
VDIFF. 

#### 6.2.3.4 Gas-phase Chemistry Process for CCTM 
Instead of directly computing the time rate of change of <p;, as is given by: 

(6-34), 

we need to decou~le the Jacobian and air density in .JY"li5;  before computing gas-phase chemistry. 
This is useful because we can approximate that the computational grid remains constant for the 
duration of a synchronization time step, which is set by the Courant conditions for the fractional 
time step numerical integration schemes.  Because the concentration unit required in the gas(cid:173)
phase chemistry is the volumetric mixing ratio, we rewrite the concentration <p;  as follows: 

" 

' 

<p;  = jrq,i = .fYpl]; ~'.r :'.  = 1f; ~'.r [-fYp :'. ] 
--[ rz- M;] 
-m; "'rp--

air 

arr 

t 

1 

Mair 

(6-35) 

where m; =ff; ~'.r  is used as the definition of the volumetric or molar mixing ratio.  The time rate 
of change of the volumetric mixing ratio due to the gas-phase cherilistry is evaluated with the 
following equation: 

t 

(6-36) 

where  ~; and  Qm; represent chemistry reactions and source terms in molar mixing ra~io units, 
respectively. 

CMAQ employs generalized chemistry solvers, such as QSSA (Young et al.,  1993) and 
SMVGEAR (Jacobson and Turco, 1994), which are designed to solve the nonlinear set of stiff 
ordinary equations presented in Equation 6-36.  They can be applied independent of the 
coordinate and grid descriptions.  To accommodate the need for modified or new chemical 
mechanisms, the CMAQ system is equipped with a generalized chemical mechanism processor. 
Refer to Chapter 8 for detailed description of numerical solvers used for gas-phase chemistry. 

The Models-3 framework provides a mapping table to link chemistry mechanism species with 
surrogate species names in the initial and boundary condition files and emissions files.  See 
Chapter 15 for details.  When a new mechanism is used, aippropriate emissions data must be 
supplied.  It is possible to include emissions either in the gas-phase chemistry or:in the vertical 
diffusion process.  It is preferable that the emissions are interpolated with the same temporal 
interpolation schemes used in the transport processes. 

#### 6.2.3.5 Aerosol Process Class for CCTM 

The fractional time step implementation solves for the effe:cts of aerosol chemistry and dynamics 
on trace gas and aerosol species concentrations with: 

(6-37) 

where  Raero, represents processes such as new particle fom1ation and growth and depletion of. 
existing particles.  Qaero,  stands for all the external sink and source terms, and vg  is the 
contravariant sedimentation velocity.  The generic concentration units for the aerosol process are 
[µg  m-3
Because the aerosol process is called within the pair of couple/decouple calls, the input 
concentration is already decoupled and the following set of governing equations are solved in the 
aerosol process module: 

]  (density) for aerosol mass and [number  m-3

]  for aerosol particle number density. 

The present implementation of the aerosol module in CCTM is derived from the Regional 
Particulate Model (Binkowski and Shankar, 1995).  Here, primary particles are divided into two 
groups: fine particles and coarse particles.  The fine particles result from combustion and 
secondary production processes and the coarse group is composed of materials such as wind-
blown dust and marine particles.  The key scientific algorithms simulating aerosol processes for 
the CCTM are: (1) aerosol removal by size-dependent dry deposition; (2) aerosol-cloud droplet 
interaction and removal by precipitation; (3) new particle formation by binary homogeneous 
nucleat,ion in i:i. sulfuric acid/water vapor system; ( 4) the production of an organic aerosol 
component from gas-phase precursors; and (5) particle coagulation and condensation growth. 
Refer to Chapter 10 for details on aerosol process implemented in CCTM. 

#### 6.2.3.6 Emissions Process for CCTM 

' 

1'•'111'

111::1 

,,'1

As mentioned earlier, the emissions process does not have its own science process class.  Instead, 
it is included .either in vertical diffusion or in the gas-phase chemistry process.  In the governing 
conservation equations for the trace gases, the emissions process is represented simply as source 
terms. 

I 

I• 

'I 

11 

,, 

, 

If emissions data cire given in the unit of time rate of change of mass, for example for particulate 
species, such as I'M2.5  and PMl 0 in [g s"1

], they are expressed as: 

'4. = ~ = _1 accp;ov)I  = _1 _acpqiov) 

emis 

(6-39) 

dt 

8V  OV 

dt 

emis  OV 

where 8V is vohu~e of the cell and E;  = d( ~OV) I . represents the emissions rate into the cell.  If 

the mass of air in the cell does not change for each time step (usually one hour), the concentration 
expression, either as the time rate of change of density or as the mass mixing ratio can be used. 
Otherwise, when the volume and density of a cell change substantially with time, the effect of 
change in air mass must be accounted for in determining the emissions rates. 

em1s 

For gaseous species, the time rate of change of  E1 for each hour and each grid cell are provided in 
the three-dimensional emissions data files in molar units, (i.e.,  mole  s-1 

): 

E. =(Mair  (4,, )ov 

M;  p 

I 

(6-40) 

Emissions for gaseous species in molar units are preferred to those in mass units because molar 
units are the natural units for chemistry arid mass units must be transfomied into  [mole  s-1
eventually for the gas-phase chemistry process.  For gaseous species the molar mixing ratio and 
mass mixing ratio differ only by a simple multiplication factor, the ratio of molecular weights. 
However, for lumped species, the molecular weights are variable depending''on the fractional 
compositions of the categorized hydrocarbon species in the emissions data.· Therefore, 
transformation of emissions in mass units into the molar units for the lumped species can 
introduce misrepresentation of emissions amount.  The data for the fractional compositions of 
the categorized hydrocarbon species are available in the emissions processor, Models-3 
Emissions Projection and Processing System (MEPPS) (See Chapter 4).  Thus, when emissions 
data are processed in [mole  s-1
emissions process is represented as: 

]  units, we do not have this conversion problem.  Then the 

dmi I  ""'  Mair  ~; 
at  emiss  M;  P 

(6-41) 

An additional benefit is that the same transformation rule can be applied when emissions are 
included either in the vertical diffusion or in the chemistry. 

#### 6.2.3.7 Cloud Mixing and Aqueous-phase Chemistry (CLOUD) for CCTM 

The rate of change in pollutant concentrations due to cloud processes is given by: 

am.., 
am.., 
-·  __ ,  +-' 
at  resc/d 
at  c/d 

am.., 
at  subc/d 

(6-42) 

where subscripts cld, subcld, and rescld represent cloud, subgrid scale cloud, and resolved cloud, 
respectively.  Although calls to the CLOUD module are made at every synchronization time 
step, the subgrid cloud effects are accounted for once an hour while the resolved cloud effects are 
impacted at each call.  This is equivalent to assuming that the cloud life time of all sub-grid clouds 
is one hour.  The effects of subgrid cloud processes, such as mixing (mix), scavenging (scav), 
aqueous-phase chemistry (aqchem), and wet deposition (wdep) on grid-average concentrations 
are parameterized with a "representative cloud" within the grid cell: 

am.., at' 

subc/d 

- f(mix,  scav,  aqchem,  wdep) 

(6-43) 

where/represents a function of its arguments.  We chose this expression because of the implicit 
nature of the algorithm representing the processes.  For the resolved cloud, no additional cloud 
dynamics are considered in CMAQ and only effects of the scavenging and aqueous-phase 
chemistry are considered: 
am., 
at  scav 

am.., 
am.., 
-·  __ ,  +-' 
at  aqchem 
at  resc/d 

(6-44) 

See Chapter 11  for details of the cloud process descriptions. 

#### 6.2.3.8 Plume-in-Grid Process (PING) for CCTM 

Anthropogenic precursors of the tropospheric loading of ozone, aerosols, and acidic species are 
largely emitted from major point sources, mobile sources, and urban-industrial area sources.  In 
particular, inadequate spatial resolution of the major point source emissions can cause inaccurate 
predictions of air quality in regional and urban Eulerian air quality models.  A plume-in-grid 

(PinG) approach in CCTM provides a more re~listic treatment of the subgrid scale physical and 
chemical processes for major elevated point source emitters (MEPSEs). 

The PING module solves for: 

(6-45) 

where  mp  is concentration of the subgrid plume (in molar mixing ratio) and the time-rate of 
change terms with subscripts disp, emis, chem, and dep represent effects of plume dispersion, 
point source emissions, plume chemistry, and dry deposition in the plume, respectively.  The 
location and shape of plumes are determined by the PDM and plume chemistry is computed in 
the CCTM within plume subsections.  When the subgrid scale phase of the plume simulation has 
been completed, the PING module updates grid scale concentrations with: 

anz;I 
at  ping 

= ov,,  ac~p -mibg) 

8V 

at 

(6-46) 

where  mib: is the back ground concentration and 8Vp  is the volume of plume in a grid cell with 
volume 8V.  Currently, only gaseous species are treated with the PING module. Readers are 
referred to Chapter 9 for the details.  The work for the inclusion of particulates in the PING 
process has been started. 

## 6.3 Equivalent Model Formulations for Different Vertical Coordinates 

Because the CCTM is based on a generalized coordinate system, it is possible to emulate the 
governing equations of other popular Eulerian air quality models.  For most urban and regional 
applications, the choice of horizontal map projection is handled with the map scale factors at the 
individual grid points.  Therefore, there are no real differences in formulations in horizontal 
directions.  One caveat is that the current CMAQ version is not tested with anholonomic 
coordinates, such as spherical coordinates.  A few implementation details must be taken into 
account to accommodate the spherical coordinates.  Most of the distinction of the dynamics is 
attributed to the choice of the vertical coordinate of the system. 

The generalized governing conservation equation for trace species, written in the Reynolds flux 
form, is given in Equation 6-18.  The same equation in eddy-diffusion form, in which the 
components of the eddy diffusivity tensor are represented in terms of those in Cartesian 
coordinates, is given below: 

(6-47) 

In most popular air quality models, including the present implementation of the CCTM, the 
cross-terms from the off-diagonal components of the diffusivity tensor are neglected.  Note that 
some models use wind components defined in Cartesian coordinates.  The mass conservation 
characteristic of Equation 6-4 7 is heavily dependent on the quality of the wind data provided.  In 
particular, depending on the dynamic assumptions used in atmospheric models, methods for 
estimating the contravari~t vertical velocity component vary considerably.  Refer to Chapter 5 
for the details. 

Formulations of other air quality models with popular meteorological vertical coordinates, such 
. as z, <J'z,  a po' and  av; and the step-mountain eta coordinate, 17,  can be obtained by substituting 
the appropriate vertical Jacobian in Equation 6-47.  Refer to Table 6-3 for the coordinate 
definitions, associated Jacobians and contravariant vertical velocity components.  Occasionally, 
one may find discrepancies in the governing equation between the one represented by Equation 6-
4 7 and the one presented in the documentation of a specific model with the same vertical 
coordinates.  Some of these can be attributed to the explicit representation of the dynamic 
characteristics and other idiosyncratic implementation practices used in those models.  In the 
CCTM, the vertical coordinate is defmed to increase with geometric height as given in Equation 
6-4.  This restriction simplifies interpretation of terms in the governing equations and eventually 
the computer coding of the algorithms.  For example, the sign of the contravariant vertical 
velocity component is kept the same (i.e., positive value represents upward motion) across the 
different coordinate systems. 

The terrain-influenced height coordinate  <J'z  has been used often for studying air quality 
especially with some simplifying conditions such as the Boussinesq approximation and anelastic 
assumption.  For urban air quality simulations, there are a few examples of applying the  az 
coordinate defmed with time and space dependent H  (thickness of model), which is often related 
with the boundary layer height.  The  <J'z  coordinate is used in air quality models, such as the 
Urban Airshed Model (UAM) (Scheffe and Morris, 1993), STEM-II (Carmichael et al.,  1991), 
CIT (Harley et al.,  1993), CALGRID (Yamartino et al.,  1992), and others.  The terrain-influenced 
reference pressure coordinate is used in SAQM (Chang et al.,  1997), which is designed to be 
consistent with the nonhydrostatic MM5 meteorological model.  The terrain-influenced time 
dependent hydrostatic pressure coordinate is used in RADM (Chang et al.,  1987).  It is the same 
coordinate used for MM4 or MM5 hydrostatic applications.  Step-mountain eta coordinate is 
used for NCJ3f' s Eta meteorological model (Black, 1994, and Mesinger et al., 1988), but no 
operational alr quality model using the.eta coordinate is available. 

.  .. 

Table 6-3.  Vertical Coordinates and Associated Characteristics 

Coordinate 

CMAQ 
_xJ  =c; 

z 

CJ".. 
% 

cr, 

1-apo 

1-<Y-
p 

l-77 

Definition 

Contravariant 

Vertical 
Velocity 

v3 
dz 
w=-dt 

daz 
dt 

da:i: 
dt 

~ dapo 
dt 
_ dafi 
dt 
_ d1J 
dt 

<Y--

- H  z-zsfc 
H-zsfc 

:i: 

a  =  z-z.<fc 
z  H-zsfc 

a  =Po-PT 
po 
Pos  -pT 
<Y- = p-pT 
-
p 
P .•  -Pr 
( ~-~ J 

re,.  -reT 

T  1J.<fc 

1J = 
. 

Vertical 
Jacobian 

Remarks 

JI; 

1 

H  -z.ifc 

H 

H-zsfc 

Pos  - Pr 

Pog 

P ..  -Pr 

pg 

res-,. reT 
pg1J.<fc 

Geometric height 

His the thickness of 
model and az  is the 
scaled height 
Nondimensional height, 
terrain-influenced 

Nondimensional 
reference pressure 
Nondimensional 
geostrophic pressure 
Step-mountain ETA 

71  = ( P.(z,1) - Pr J 

Po(O)- Pr 

.<Jc 

## 6.4 Nesting Techniques 

The nested grid CTM is needed to provide the required high resolution simulations.  At present, 
Models-3 CMAQ allows only static grid nesting.  In static grid nesting, finer grids (FGs) are 
placed (i.e., nested) inside coarser grids (CGs).  The resolution and the extent of each grid are 
determined a priori and remain fixed throughout the CTM simulation.  Static grid nesting 
conserves mass and preserves transport characteristics at the interfaces of grids with different 
resolutions (Odman et al.,  1995).  It allows for effective interaction between different scales with 
efficient use of computing resources. · On the other hand, the nest domain is redefined during a 
simulation with the dynamic nesting.  Both static and dynamic nesting techniques allow one-way 
or two-way exchange of information among FGs and the CG and periodically by independently 
simulating CTMs at each grid (coarse or nested) with its own time step.  The dynamic nesting 
procedure is not implemented in the CMAQ system. 

In' one-way nesting, the primary concern is the mass conservation at the grid, interface where 
boundary conditions are input to the FG using the CG solution.  The advective flux at the inflow 
boundaries of the FG is the flux as determined by the CG solution that passes through this 
interface.  We also allow for time variation of the flux during the CG time step.  This is 
performed by computing the departure point of the last particle passing through the interface for 
each FG time step.  In the Lagrangian description, the mass crossing the interface is equal to the 
spatial integral of the concentration distribution between the departure point and the interface. 
Most flux-conserving advection schemes use the same Lagrangian concept to calculate the mass 
transfer between grid cells (e.g., Bott, 1989).  During each FG time step, we meter into the FG 
the exact amount of mass that would have crossed the interface on the CG (Byun et al.,  1996). 

In two-way nesting, the concentrations are updated in each CG cell by averaging the 
concentrations of all the FG cells overlapping with the CG cell.  Special care is required to assure 
strict mass conservation at the grid interface.  The mass of some species (e.g., radicals) may no 
longer be conserved because, when advancing the FG solUltion, we perform nonlinear chemical 
transformations in addition to transport.  However, the mass of the basic chemical elements such 
as sulfur, nitrogen, and carbon must be conserved.  The FG solution is used to compute the flux 
of each element at the grid interface.  When conservation principles are applied to the grid 
interface, as described above, the CG concentrations near the interface must be corrected (when 
the Courant stability limit is applied, only the first row of CG cells immediately outside the 
interface need correction).  This is done by renormalizing the concentration of each species based 
on the assumption that the ratio of the species mass to element mass will remain the same before 
and after the correction.  This method is similar to the renormalization procedure used to make 
slightly non-conservative chemical solvers strictly conservative.  Alapaty et al. (19.98) compares 
different spatial interpolation schemes used for the two-way nesting. 

Cpa 

Figure 6-5.  Static Grid Nesting Used in CMAQ System 

The CCTM provides a static nesting (see Figure 6-5) capability while maintaining a high level of 
modularity by separately processing object codes for different grid domains and by enforcing the 
protocol that each module r~ads its required input data independently from others.  The scheme 
is also applicable for multiple and multi-level grid nesting.  Multi-level nesting is a natural 
extension of the single static grid nesting.  Figure 6-6 presents a schema for multi-level nesting, 
where three-levels are illustrated.  The feedback processes update coarser grid concentrations at 
each synchronization time steps of the nest grids.  The information about the grid is 
communicated to each process module through a set of FORTRAN include files specific to each 
grid domain dilling compilation time.  This allows use of the same process modules for different 
grids.  Customizmg a nested model is as simple as preparing include files with grid dimensions for 
each grid and a driver with the appropriate process calling sequence. 

As seen in Figure 6-6, the only difference between the one-way and two-way nesting is whether 
concentration.S in coarser grid simulations are updated with the finer grid simulations through the 
feedback processors or not.  Depending on the computer hardware and software configurations, 
one could build a nested CTM model with one executable collectively simulating all the FGs and 
CG, or with independent CTM executables for different grids that run simultaneously on 
m,ultiple CPUs accessing common data through appropriate 1/0 APL  The latter approach relies 
on the cooperating processors concept in a UNIX environment.  As mentioned before, the 
current CMAQ version lacks a feedback module, which is necessary for the two-way nesting. 

cGrkl 
Emls 

----~Q~Coarse_Conc 

' 

Coarse  '§P:ad< 

Interpolate 
cone 
.j, 

I:_:  ~§Grid_1 ~ 
tGfl((1 ~  l""  lt.LSync_fGrid1 

v  @"'--"'"'-""' 
_J_ 
~'G"d 1 ~--
Cc\ fGrkU '-
' 
v 
_J_ 

~ 

Driver 

...-------,..  "  - -""'"' 

int~l'!Xllate 
Cone 

Mil 

J  file 

!Grid  1 

Update 

..J-

~1Grid_2_Conc 

1Gtfd_1 

Legend 

c:G'ld:  coarse  grid  domaln 
!Grid:  fine  grid  domaln 
Emls:  emissions  file 
Met:  meteorology  flies 
J: 
C: 
EC: 
Cone:  conce ntr all on 
Sync:  synchronizat Ion 
Proc:  processor 

photolysis  rate 
Initial  conditions 
boundary conditions 

Figure 6-6.  Schematics for Multi-level Nesting (three levels illustrated) 

## 6.5 Summary 

The CMAQ system achieves multi-pollutant and multiscale capabilities by combining several 
distinct modeling techniques.  The generalized governing conservation equations of the CCTM 
allow transformation among various vertical coordinates (e.g., terrain-influenced geometric height, 
or pressure) and transformation among various horizontal coordinates, especially map 
projections (e.g., rectangular, Mercator, Lambert, and polar stereographic) by simple changes in a 
few scaling parameters defining the boundary domain, map origin, and orientation.  Therefore, 
CMAQ can be configured to match the dynamic characteristics of the preprocessor 
meteorological models.  The CMAQ system uses a nesting technique and a plume-in-grid 
approach to handle small scale air quality problems and subgrid scale plume dispersion and 
chemical reactions, respectively.  The multi-pollutant capability is provided by a generalized 
mechanism reader and generalized chemistry solvers, linked cloud mixing and aqueous reaction 
processes, and aerosol modules.  The CCTM code uses a modular structure that allows for the 
continuing improvement of the science and addition of new capabilities in a unified fashion.  It 
provides a natural disciplinary distinction among different science processes through which 
developments in specific research areas can be readily incorporated. 

As mentioned above, there remain a few implementation tasks, such as development of feed-back 
modules for two-way nesting and adaptation of anholonomic coordinates (e.g., latitude(cid:173)
longitude ), which will provide additional functionalities in CCTM. 

## 6.6 References 

Alapaty, K., R.  Mathur, and T.  Odman,  1998: Intercomparison of spatial interpolation schemes 
for use in nested grid models. Mon.  Wea.  Rev.  126, 243-249. 

Binkowski, F. and U.  Shankar, 1995: The regional particulate model; Part I: Model description 
and preliminary results. J.  Geophys.  Res., 100: 26191-26209. 

Black, T.,  1994:  The new NMC  mesoscale Eta model:  Description  and forecast examples.  Wea. 
Forecasting, 9, 265-278. 

Bott, A., 1989: A positive definite advection scheme obtained by nonlinear renormalization of 
the advective fluxes~ Mon.  Wea.  Rev.  117, 1006-1015. 

Byun D. W., A. Hanna, C. J.  Coats, and D.  Hwang,  1995: Models-3 Air Quality Model 
Prototype Science and Computational Concept Development. Trans.  TR-24  Regional 
P_hotochemical Measurement and Modeling Studies, 8-12 November 1993, San Diego, CA, Air & 
Waste Management Association. Pittsburgh, PA.,  197-212. 

Byun, D.W., D. Dabdub,  S.  Fine,  A. F.  Hanna, R.  Mathur,  M. T.  Odman, A. Russell, E. J. 
Segall, J. H.  Seinfeld, P.  Steenkiste, and J. Young,  1996:  Emerging Air Quality Modeling 
Technologies for High Performance Computing and Communication Environments, Air Pollution 
Modeling and Its Application XI, ed. S.E. Gryning and F. Schiermeier, 491-502. 

Byun, D.\V., J. Young., G.  Gipson., J.  Godowitch., F. Binkowsk, S. Roselle, B. Benjey, J. Pleim, 
J.  Ching., J. Novak, C. Coats, T. Odman, A.  Hanna, K. Alapaty, R. Mathur, J.  McHenry, U. 
Shankar, S. Fine, A. Xiu, and C. Jang,  1998: Description of the Models-3 Community Multiscale 
Air Quality (CMAQ) model.  Proceedings of the American Meteorological Society 78th Annual 
Meeting, Phoenix, AZ, Jan.  11-16, 1998. 264-268. 

Byun, D. W., 1999: Dynamically consistent formulations in meteorological and air quality 
models for multi-scale atmospheric applications: II. Mass conservation issues. J.  Atmos.  Sci., (in 
print). 

Carmichael, G.R., L.K. Peters, and R.D. Saylor, 1991: The STEM-II regional scale acid 
deposition and photochemical oxidant model-I. An overview of model development and 
applications. Atmos. Environ.  25, 2077-2090. 

Chang, J.S., R.A. Brost, LS.A. Isaksen, S. Madronich, P. Middleton, W.R. Stockwell, and C.J. 
Walcek, 1987:A three-dimensional Eulerian acid deposition model: Physical concepts and 
formulation, J.  of Geo phys. Res., 92, 14,681-700. 

Chang J.  S., S. Jin, Y. Li, M. Beauharnois, C.-H. Lu, H.-C. Huang, S.  Tanrikulu, and J. DaMassa, 
1997: The SARMAP air quality model. Final Report, SN AQS/AUSPEX Regional Modeling 
Adaptation Project, 53 pp. [Available from California Air Resources Board, 2020 L Street, 
Sacramento, California 95814.] 

Coats, C.J., cited 1996: The EDSS/Models-3 1/0 Applications Programming Interface. MCNC 
Environmental Programs, Research Triangle Park, NC. [Available on-line from 
http://www.iceis.mcnc.org/EDSS/ioapi/H.AA.html.] 

Harley, R.A., A.G. Russell, G.J. McRae, G.R. Cass, and J.H.  Seinfeld.  1993: Photochemical 
modeling of the Southern California air quality study. Envir.  Sci.  Technol. 27, 378-388. 

Jacobson, M.Z. and R.P. Turco,  1994: SMVGEAR: A sparse-matrix vectorized Gear code for 
atmospheric models. Atmos. Environ.  20, 3369-3385. 

Mesinger, F., Z. I. Janjic, S. Nickovic, D.  Gavrilov, and D.  G.  Deaven, 1988: The step-mountain 
coordinate: model description and performance for cases of Alpine lee cyclogenesis and for a case 
of Appalachian redevelopment. Mon.  Wea.  Rev.,  116, 1493-1518. 

Odman, M.T .. ~ R. Mathur, K. Alapaty, R.K. Srivastava, D.S. McRae, and R.J. Yamartino, 1995: 
Nested and adaptive grids for multiscale air quality modeling. U.S. EPA Workshop on Next 
Generation Models Computational Algorithms (NGEMCOM), Bay City, MI, June  1995. 

Seaman, N. L., D.R. Stauffer, and A. M. Lario-Gibbs, 1995: A multiscale four-dimensional data 
assimilation system applied in the San Joaquin Valley during SARMAP. Part I: Modeling design 
and basic performance characteristics. J.  Appl.  Meteor., 34, 173 9-17 61. 

Seinfeld, J.H.,  1986: Atmospheric Chemistry and Physics of Air Pollution. Wiley-Interscience, 
New York. 

Scheffe, R. D., and R. E. Morris, 1993: A review of the development and application of the 
Urban Airshed Model. Atmos.  Environ., 27B, 23-39. 

Srivastava, R.K., D.S. McRae, M.T.  Odman, 1995: Governing Equations of Atmospheric 
Diffusion. Technical Report, MCNC [Available from MCNC-North Carolina Supercomputing 
Center, P.O. Box 12889, 3021  Cornwallis Rd. Research Triangle Park, NC 27709-2889.] 

Schwartz, E.  S.  and P. Wameck, 1995: Units for use in atmospheric chemistry. Pure &  Appl. 
Chem. 67, Nos 8/9, 1377-1406. 

Toon, O.B., R.P. Turco, D. Westphal, R. Malone, and M.S. Liu, 1988: A multidimensional 
model for aerosols: Description of computational analogs. J.  Atmos. Sci., 45, 2123-2143. 

Venkatram,A., 1993: The parameterization of the vertical dispersion ofa scalar in the 
atmospheric boundary layer. Atmos. Environ.  27 A, 1963-1966. 

Yamartin~, R.J., J.S. Scire, G.R. Carmichael, and Y.S. Chang, 1992: The CALGRID mesoscale 
photochemical grid model - Part I.  Model formulation, Atmos. Environ.  26A, 1493-1512. 

Young, J.O., E. Sills, D. Jorge, 1993: Optimization of the Regional Oxidant Model/or the Cray Y(cid:173)
MP, EPA/600/R-941065.  [Available from U.S. Environmental Protection Agency, Research 
Triangl~ Park, NC 27711.] 

This chapter is taken from Science Algorithms of the EPA Models-3 Community 
Multiscale Air Quality (CMAQ) Modeling $ystem, edited by D. W. Byun and J. K. S. 
Ching, 1999. 


Appendix 6A. Concentration Units Used for Air Quality Studies 
============

As we have seen above, many different concentration units are used for air quality studies.  In 
this section we summarize the relations among the concentration units and their conversion 
factors from one unit to another.  For Models-3/CMAQ system we follow the International 
System of Units (Systeme International, SI) as a framework for units in the formulations.  The 
fundamental assumption used here is that air and trace gases follow the ideal gas law, i.e., 

(6A-1) 

where  Re  is the universal gas constant  = 8.314510 [J l(mol • K)], 

v is molar number, 
Vis volume of the gas [ m3 
pis pressure [Pa], and 
T is temperature [ K]. 

]  , 

There are many different ways to express the amount of substance in the atmosphere.  We 
introduce most popular quantities and transformation relations among them are presented below. 

Number Density, n 

One way to ·express trace gas quantities is to count number of molecules in the unit volume.  For 
example, number of molecules of air in the unit volume,  nair' is expressed as: 

where  v air  is number of moles of air, 

NA  is the Avogadro's number= 6.0221367x 1023

,  and 

Similarly, number of molecules of trace gas per volume, i.e., number density of species n;, is 
defined as: 

(6A-2) 

v.NA 

n1 =-'-= 

v.NA 
' 

V 

V1RgT IP; 

=-'-[mo ecu es 

l 

l 

P· 
k8T 

(6A-3) 

where  v,  is number of moles of trace gas i in a given volume, and 

k8  =Re I NA  is Bolzmann's constant 
p1  is the partial pressure of species i. 

Molar Density, c, 

The number of moles of air (ca1r) and trace gas (c1)  normalized for a unit volume of air are simply 
defined as: 

6-34 

v. 
c.  =......!EL 
V 
arr 

BP A/600/R-991030 

(6A-4a) 

(6A-4b) 

with the unit [mole  m-3
can be used conveniently for expressing chemical relationships. 

]. ·Because the SI unit for the ~mount qf substance is mol, this quantity 

Partial Pressure, Pt 

Although not used widely in atmospheric chemistry, the partial pressure has been playing an 
important role specifying thermodynamic properties of the atmosphere, especially for water 
vapor in the air.  The Dalton's law states that the total pressure exerted by a mixture of gases is 
equal to the sum of the partial pressure exerted by each constituent  at the given temperature and 
volume.  Because we assume that each trace gas follows the ideal gas law, the partial pressure 
can be used to express the trace gas quantity.  The partial pressure of atmospheric constituent 
gas is related to the number of moles per volume as: 

P.=--=c.RT 

I  g 

I 

viRgT 
v 

.(6A-5) 

and the standard SI unit for the partial pressure is Pascal [Pa]. 

Molar Mixing Ratio,  m; 

Often, the molar mixing ratio is used as a synonym for the volume mixing ratio, or the mole 
fraction of a substance in air.  Basically it is a unitless quantity.  However, it is customary to 
identify in terms of molar unit as [mole I mole].  Because the volume occupied by a mole of ideal 
gas at given pressure is the same regardless of the constituent, the mole fraction is essentially 
equal to the volume fraction.  However, mole fraction is preferred because it does not require the 
implicit assumption of the ideality of the gases, and more importantly because it is applicable 
also to condensed-phase species (Schwartz and Warneck, 1995).  For a given volume, the volume 
mixing ratio of a trace species is expressed in terms of concentration units defined above as: 

m.= 

I 

v. 

I 

Vair  +Vi 

=-C....;.;_=_Pi 
p 

where p  is the total pressure. 

(6A-6) 

Because  vi<< Vair  for trace gases, Equation 6A-6 can be approximated as: 

v. 

m; =-' =-' =-' 

c. 

p. 

Vair 

cair 

Pair 

When dealing with trace gases in the real atmosphere, the contribution of moisture can be used in 
the definition of mixing ratio.  Therefore,  Pair  represents total pressure of the atmosphere which 
includes vapor pressure of water while the contributio.n of other trace gases are neglected. 
Because the vwiation caused by the moisture can amount to several percent, some researchers 
prefer to use dry air when expressing trace gas mixing ratios.  However, in the Models-3/CMAQ 
system, we use the trace gas mixing ratio with respect to the moist air because we rely on the 
continuity equation for the total air density to represent atmospheric mass conservation. 

Mass Mixing Ratio, q, 

,1·:11!: 

Mass mixing ratio is used often for describing transport process and is simply related with the 
volume mixing ratio as: 

(6A-7) 

Density,  <p1 

Cpncentrations of l>Ubstances in air can be expressed in terms of amount of substance mass per 
volume of air, i.e. density.  Density is a most popular unit for dynamic representation of 
atmospheric concentration because it is the operating units of the mass continuity equation. 
Density can be expressed in terms of other concentration units as: 

_  v,M;  _ 

m1 ----c.M. - - c .  M. 
r 

air 

't'. 

r 

V 

_  c; 

1 

cair 

s= (~I:~ )(cairMair) = qiPair 

arr 

(6A-8) 

Table 6A-1 provides cross-reference relationships among the concentration variables described. 
However, the individual process science area prefers to use certain specific units of decimal 
multiples of the SI unit suggested.  We included the conversion factors among the concentration 
variables actually implemented in science process modules.  Because it is convenient to express 
the conversion factors in terms of the dry air quantities, the virtual temperature is used in Table 
6A-1. 

. 

. 

... 

6-36 

Table 6A.1.  Conversion formula among various trace gas concentration units used in the CMAQ system 
==========

number density,  N1 

[molecules!m3] 

molardensity,  c1 

[mol/m 3

] 

partial pressure, p1  molar mixing ratio,  mass mixing ratio, 

demity,  <p1 [µglm1 

[Pa.rcalj 

m1 fppmV] 

(/;  fppm] 

number density, 
N; 
[molecules/m 3

] 

molardensity, c1 
[mol!m3] 

1 

N;= (NA)C; 

N;  = (ksD-

1
P1 

C;  = (NAf1N; 

1 

C;  = (RgT)-l P; 

N,= Hr•  NA~ , 
. 

3 

( 

p  } 
M.,, 

Ni  =10-

C.  = 10~ -( pair  )m. 

M 
air 

I  C;= 

I 

. 

3 

· (  ~ ~ } /   ~ =10-6 ·(~} 
10~ (P·•) 

•  M  qi  c =10-6•(Mfl<p. 

I 

I 

I 

. 
I 

P;=  Q?.gT~ 

1 

-6 

. 

P1  =10  •(p)m; 

P1  = 10  •  ~;r P  q1 

-6  (M. 

) 

(RTJ 
P.  =10"6.  _, J~ 
~  I 
. .I 

°' I w 

-...J 

p1  [Pacal] 

P; =("f<aT)N; 

partial pressure,  I 
molarmixing  I  , ( M } 
mass  mixing  I q,= 1()1  -( 2L )N; 
I ~' =IO'•(*)N, 

density,  <p1 
[µglm1 

ratio,  fl;  fppm] 

Pa.NA 

NApal 

ratio,  m;  fppmV]  m;  =10  •  ~ ;  m; =  td •  -..!!!!:  C; 

·  ( M . )  m;  = 106 •(pf1P; 

Pa1r 

1 

m;=( ~:r}, 

•  -1 

m1 =IO ·•  ~ '1'1  · 

(  M.  ) 
P.1.M1 

( M) 
. 
q;=l03•  - :  lj 
Parr 

6 

'fJ;  = 10  • (M;)c; 

qi= Hf •(~Pi  q1=( Mi.  }1 
-IO'  (M')  ~-io'-(PwM}  'Pt  =10 3 

pMair 

Marr 

•  -

<p.  -

M 

· 

I 

I 

1 

I 

air 

RT 

g 

P.· 
I 
. 

q1  =Id ~ <P.,,f1 'P, 

•(pairMi 

1 

Note: Values for the Avogadro's constant (NA) is 6.0221367x1023 [number/mol], the universal gas constant (Rg)  is 8.314510 [J/mol-K], and the Boltzmann's 
constant (kB)  is defined with R/ Nk  Molar mixing ratio is equivalent to volume mixing ratio and their units [ppmV],  [ppb], and [ppt] are short hand notations for 
[µ  mol motl ], [n mol mo[-1], and fp mol mol-1], respectively.  The unit for  mass mixing ratio, [ppm], is a short hand for 110-6 kg kg-1].  Both  and  may 
represent values for dry air or moisture air depending on applications.  However, in one processing system, their usage should be consistent.  · Mair and  Mi 
(molecular weight of species i) are in [g moz-l] and Pair is in [kg m-31. 

ttl 

~ g 
~ I 
'O 
~ 
8 
" 

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_Science_Ch_05.md) - [Home](README.md) - [Next Chapter >>](CMAQ_Science_Ch_07.md)<br>
CMAQ Science Document (c) 1999<br>

<!-- END COMMENT -->

