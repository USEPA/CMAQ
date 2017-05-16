Chapter 9 
============

EP A/600/R-99/030 

N. V. Gillani 

Earth System Science Laboratory 
University of Alabama in Huntsville 

Huntsville,  Alabama 35899 

James M. Godowitch" 

Atmospheric Modeling Division 

National Exposure Research Laboratory 
U.S.  Environmental Protection Agency 

Research Triangle Park, North Carolina 27711 

ABSTRACT 

A plume-in-grid (PinG) technique has been developed to more realistically treat the dynamic and 
chemical processes impacting selected, major point source pollutant plumes in the Community 
Multiscale Air Quality (CMAQ) modeling system.  The principal science algorithms include a 
Plume Dynamics Model (PDM) and a Lagrangian reactive plume code.  The PDM processor 
simulates plume rise,  horizontal and vertical plume growth, and transport of each plume section 
during the subgrid scale phase.  It generates a data file  of this information for use by the PinG 
module.  In contrast to the traditional Eulerian grid modeling method of instantly mixing the 
emissions from each point source into an entire grid cell volume, the PinG module simulates the 
relevant physical and chemical processes during a subgrid scale phase which allows each plume 
section to expand in a realistic manner and to evolve chemically.  The PinG module is fully 
integrated into the CMAQ Chemical Transport Model (CCTM) in order to utilize the grid 
concentrations as boundary conditions and it provides a feedback of the plume pollutants to the 
grid model concentration field  at the proper time and grid location.  The technical approaches and 
the model formulation of the relevant processes treated by this plume-in-grid approach are 
described.  The capabilities and limitations of the initial version of PinG are also discussed and 
selected results from a test application for a single point source are briefly described. 

. 

. 
On assignment from the National Oceanic and Atmospheric Administration, U.S.  Department of Commerce. 
Corresponding author address: James M.  Godowitch, MD-80, Research Triangle Park, NC 27711.  E-mail: 
jug@hpcc.epa.gov 

PLUME-IN-GRID ~,;REATMENT OF MAJOR POINT SOURCE EMISSIONS 

9.1 

Introduction 

Significant emissions of anthropogenic nitrogen oxides (NO,J and sulfur oxides (SO,J are released 
from individual elevated point sources into the atmosphere at various levels.  These major point 
source emissions are distributed throughout the U.S.  The physical dimensions of their plumes are 
relatively small initially and expand at finite growth rates.  This diffusion-limited nature of plumes 
is in sharp contrast to the traditional method applied in Eulerian photochemical grid modeling, 
which has been to uniformly and instantly mix point source emissions into the entire volume of a 
model grid cell.  Depending on the meteorological conditions, however, pollutant plumes may 
require up to several hours to grow to the typical size of a regional model grid cell.  Since the 
horizontal grid resolution of regional scale domains for Eulerian models has generally been 20-~p 
km or greater, a real-world pollutant plume may remain a subgrid scale feature at current regional 
model grid resolutions for a substantial time period and considerable distance after release.  Thus, 
the widely-used grid modeling approach inadequately treats subgrid scale plume transport and 
diffusion. 

There are also important implications on the chemical processes in models due to the inability of 
large grid cells to adequately resolve a major point source plume.  As a real-world plume 
gradually grows, it concurrently evolves chemically as it entrains surrounding ambient air often 
richer in volatile organic compounds (VOCs).  However, these dynamic and chemical processes 
of plumes are neglected when large point source emissions are instantly diluted into large grid 
cells with other anthropogenic area emissions, which may actually be separated spatially from a  ;; 
plume.  The effect is that the simultaneous availability ofNOx and VOC's in a large grid cell 
prematurely initiates rapid photochemistry leading to distortion in the spatial and temporal 
features of secondary species concentrations.  This overdilution of point source emissions and the 
subsequent distortion of chemical processes contribute to model uncertainty.  Consequently, it has 
been recognized that a realistic,  subgrid scale modeling approach is needed which simulates the 
relevant physical and chemical processes impacting this notable class of large point source 
emissions,  and in particular, that has the capability of properly resolving the spatial '~cale of 
plumes and their growth. 

There are approaches which have been applied in attempts to resolve fine scale emissions in 
regional grid models.  These have included uniform, nested grid modeling (Odman and Russell, 
1991; Chang et al.,  1993), non-uniform grid modeling (Mathur et al.,  1992), and the plume-in- .. 
grid modeling (PinG) technique. The nested grid (telescoping) modeling approach employs the,,, 
same Eulerian model and simulations are performed with progressively finer-mesh grid sizes 
· ' 
within a domain to better resolve small scale emission features.  While an advantage of this 
approach is the use of a single model algorithm,  a disadvantage is the successively higher 
computational burden as the grid cell size is reduced in order to resolve major ~mi§~ion source~,,: 
Efforts to implement and apply the plume-in-grid technique, in particular, have been performed by 
Seigneur et al.  (1983) in a version of the Urban Airshed Model (UAM-P ARIS), by Morris et al. 
(1992) in the UAM-V model,  by Kumar and Russell (1996) in the Urban-Regional Model (URM), 
and by Myer et al.  (1996) in the SAQM (SARMAP Air Quality Model) model.  Briefly,  each of 
these PinG efforts simulate multiple plumes or puffs in a domain with no interaction between 
individual plumes.  Seigneur et al.  (1983) employed a rectangular plume cross-section divided into 
vertically well-mixed plume cells exhibiting varying widths to maintain equal pollutant mass 
among the cells.  It was applied to an urban domain with a 4 km grid cell size.  The other efforts 
employ an elliptical plume section divided into concentric rings,  except for the Kumar and Russell 
(1996) PinG.  Plume material in an outermost elliptic ring or shell is sequentially transferred to the 
grid model as it attains the grid size.  In the treatment by Kumar and Russell, the plume was 
returned to the grid model after one hour.  The general result of applications from these PinG 
implementations was that noticeable differences were found  primarily within or near the grid cells 
containing the major point sources and negligible impact was found far downwind.  This finding is 
to be expected since these formulations seemed to focus on the plume-core chemistry only in an 
early phase of plume chemical evolution with the feedback of plume pollutants occurring rather 
quickly to the grid model. 

A cooperative research and development effort was undertaken to design and implement a plume(cid:173)
in-grid (PinG) capability in the Models-3  CMAQ Chemical Transport Model (CCTM) in order to 
address the need for an improved approach to treat major point source emissions.  The current 
PinG approach has been designed to be suitable for the largest, isolated point source emissions 
with grid resolutions of regional modeling domains, where the sub grid scale error in the 
representation of these sources is  expected to be greatest without a PinG treatment.  An 
important consideration is to be able to spatially resolve a pollutant plume and to adequately 
simulate physical expansion so that the chemical evolution can occur gradually. 
Therefore, the objectives with this PinG technique are to provide an improved characterization of 
the near-source pollutant field  from the subgrid scale plume concentrations and to also generate 
better regional pollutant concentrations downwind due to the feedback of the PinG concentrations 
to the gridded domain.  Consequently, when the PinG approach is applied to the largest point 
source emissions in .a regional,  coarse-grid domain, improved initial and boundary conditions are 
anticipated for use with subsequent simulations for a subdomain of the same grid resolution or for 
fine-mesh nested domains that also need the regional gridded concentration fields. 

In this chapter, the conceptual design of the PinG approach is discussed.  The key modeling 
components are a Plume Dynamics Model (PDM) and a Lagrangian reactive plume model  ___  . 
designated as the PinG module since it is fully integrated and coupled with the CCTM Eulerian 
grid model.  The PDM has been developed to serve as a processor program to generate plume 
dimensions,  plume position information and related parameters needed by the PinG module.  The 
descriptions of the mathematical formulations and numerical techniques contained in these 
modeling components are also presented.  Their solutions form the basis of the physical-chemical 
simulations of sub grid scale plumes as implemented in the principal algorithms of the PinG 
modeling components. 

9.2 

Overview of the Conceptual Framework of the Plume-in-Grid Tre~.~ment 

The PinG technique is intended for the largest point sources, designated as major elevated point 
source emissions (!\1EPSEs),  which are isolated from notable area emission sources.  A complete 
description of the various criteria available to classify a point source as a MEPSE is provided in  .. 
the emission processing system in chapter 4.  In the context of photochemical modeling, the NOX 
emission rate is a useful criterion to classify a major point source as a MEPSE from the thousands 
of individual major point sources in an inventory.  Base~ on a NOx emission rate criterion of 50 
tons/day as a lower limit, Figure 9-1  depicts the group of fewer than 100 MEPSEs in a 36 km 
gridded domain covering the eastern US with emission rates greater than this criterion.  The 
!\1EPSE sources are situated away from major metropolitan emission source areas.  It reveals that 
numerous !\1EPSEs are distributed throughout the Ohio River and Tennessee River valley regions. 
Since the current PinG technique also relies on linear superposition of!\1EPSE plumes for 
determining the impact on grid concentration, the relatively isolated MEPSEs located in these 
remote rural environments also provide for the lowest likelihood of plume-plume interactions. 

An important feature of a PinG treatment is that it must be able to reproduce the various stages in 
the chemical evolution found in a large,  rural point source plume.  Based on daytime experimental 
field  study plume data, Gillani and Wilson (1980) documented three distinct stages, as displayed 
in Figure 9-2, in the chemical evolution of ozone (03) in a pollutant plume from a lllgh NO:c 
source.  The 0 3 data measured across a plume at different downwind distances in Figure 9-2 
illustrate ozone evolution in a large NOx point source plume.  Although NOx was not obtained, it 
would display a similar variation to S02  shown in Figure 9-2.  During the first stage, the relatively 
fresh plume is dominated by primary NOx  emissions and an 0 3 deficit exists in the plume due to 
titration by very high NO concentrations.  The chemistry in stage 1 is mostly inorganic and VOC(cid:173)
limited since !\1EPSE plumes generally ~xhibit low VOC emissions.  The second stage represents 
a transition in the chemistry with rapid production of 03 along the plume edges and some 03 
..... ,, 
recovery in the plume core.  Plume growth from dispersion processes allows for entrainment of 
background air which generally contains a richer supply of ambient voe conc~Qtr~!,ions. 
Consequently, this promotes rapid photochemical 0 3 production which leads to the 
characteristically higher 0 3 concentrations observed at the edges of a plume during this stage. 
Figure 9-2 reveals that stage 2 certainly exhibits considerable subgrid scale plume structure in 
species concentrations.  Therefore, the proper simulation of plume chemistry throughout stage 2 
is a key phase for a realistic,  overall characterization of plume chemical evolution.  In fact,  full 
completion of stage 2 is an important requirement for a chemical criterion during the PinG 
simulation before the plume pollutants are transferred to the grid system.  In stage 3, the plume is 
chemically mature and substantially diluted.  In this mature stage, the broad plume would also 
exhibit a high VOC/NOx  ratio, low NOx  concentrations, and 0 3 concentrations in excess of 
background levels.  In the modeling system, background (boundary) concentrations are provided 
from a CCTM grid cell containing the subgrid scale plume section. 

The chemical evolution,  as described above, for a large !\1EPSE plume in the eastern US generally 
may take up to several hours to reach full  maturity even during the daytime period.  The 
photochemical cycle is strongly influenced by the plume growth rate, particularly the rate of 
horizontal spread, with stage 3 achieved when a typical MEPSE plume attains a width of about 30 
km during the daytime period.  In addition, the time period to reach chemical maturity is also a 
function of the emission rate.  There is a range of emission rates even within a MEPSE group. 
For example, particular MEPSE plume with lower NOx emissions is expected to exhibit a faster 
chemical evolution than a MEPSE with a higher emission rate, assuming other factors being the 
same.  Nevertheless, during the subgrid scale simulation period, the plume may travel a 
·considerable distance downwind (i.e.  sever~ model grid cells) of its source location in the daytime 
planetary boundary layer (PBL) during the summer before reaching a width comparable to the 
grid cell size. 

The conceptual design of the Models-3  PinG is illustrated in Figure 9-3.  PinG components 
simulate the relevant processes impacting the hourly emission rate of all pollutant species during 
the subgrid scale phase and it provides a feedback,  or 11handover 11
,  of the modeled plume species 
concentrations at an appropriate handover time (tw0 )  to the CCTM grid framework.  The PinG 
module operates in a Lagrangian modeling framework to simulate the dynamics and kinetics in a 
moving plume section during the subgrid scale phase whose duration depends primarily upon 
source strength, plume growth rate, chemical composition of the background CCTM grid 
concentrations, and sunlight.  The PinG module simulates the plume processes concurrently 
during the simulation of the CCTM grid model. 

The PinG module contains a series of rectangular plume cross-sections, each of which are 
composed of a contiguous, crosswind array of plume cells with the depth of each cell extending 
vertically from plume bottom to a top height.  As displayed in Figure 9-3a,  the vertical depth of 
the single-layer plume model may be elevated initially, however, it eventually extends from the 
surface to the PBL height(~).  This one-layer plume structure in the initial PinG module also 
presents a limitation under high wind speed shear conditions since this situation cannot be 
modeled adequately.  While strong speed shears are more frequent during the nocturnal period, 
the vertical extent of a plume is also generally confined, which lessens the possible impact of 
speed shear.  Nevertheless, this condition is identified and appropriately handled by the PinG 
components. 

9.3 

Formulation of the Plume-in-Grid Modeling Components 

9.3.1  Description of the Plume Dynamics Model 

The PDM was developed to serve as a processor program in order to provide data needed for the 
CCTM/PinG simulation.  Therefore, the PDM and the PinG module are closely linked through 
the PDM data file.  The key processes simulated by the PDM include plume rise,  plume transport, 
and plume dispersion of each MEPSE plume cross-section.  The PDM also specifies when a 
particular plume section is to be transferred to the grid model based on an indicator flag which 
communicates to the PinG module when this process is to be performed.  The PDM is capable of 
simultaneously treating up to 100 MEPSEs in a single simulation with the impact on 
computational time a function of the number ofMEPSEs simulated.  In addition, the maximum 
simulation period is currently limited to 24 hours.  Therefore, a single simulation of the CCTM 
with PinG is also limited to a 24-hour time period.  ·· 

9.3.1.1 

!I· 

Aspects of Plume Rise of MEPSE Sources 

.,. 

The plume rise treatment implemented in PDM contains the same algorithms applie~ in the 
Emissions-Chemistry Interface Processor (ECIP).  A detailed discussion of the plume rise 
equations and treatment has been provided in section 4.4.2 of chapter 4 on the MEPPS system 
and therefore is not repeated herein. 

' 

( 

The plume rise algo;ithms handle the entire diurnal cycle since plume sections ~e r~leased hou;ly 
throughout the 24 hour cycle.  The temporal variation of the final  effective plume height for 
···· 
hourly MEPSE plume releases is illustrated in Figure 9-4. 
In this case,  a diurnal variation in the 
final  plume height is evident with lower plume rise occurring at night believed to be due in partto 
stronger wind speeds.  The plume height increases during the morning period as the PBL heigh[": 
grows and the relatively high values found in the afternoon display little variation until the end 01f 
the daytime period.  · 

A notable feature ofMEPSE plumes also revealed in Figure 9-4 is the rather significant height  , 
often attained by this class of point sources.  Statistics of the stack parameters .:ffo.mMEPSE an.9 
non-MEPSE groups of major point sources were determined.  A  comparison iggic~ies notable 'liiiilllll~ 
differences in certain stack parameters between these source groups.  The average values from a 
MEPSE group containing 84 stacks compared to the other major point source class containing 
6539 stacks show a stack height of207 m versus 45  m, a stack velocity of20ip/sversus 12.5 · 
mis, a stack diameter of7.5 m versus 2.3  m,  a stack exit temperature of 410° K versus 424° K, 
and a stack exit flow rate of 908 m3/s versus  105  m3/s, respectively.  Clearly,  these results indicate 
why MEPSE plume heights attain higher levels than most point sources.  In general, MEPSE 
stacks are considerably higher and their plumes exhibit greater buoyancy flux due primarily to 
greater physical dimensions which contribute to higher plume rise heights. 

"' 

1
1 
1

1 

11 

''

•• 

9.3.1.2 

Plume Dispersion Methods 

A key aspect of modeling a subgrid scale reactive plume, as noted earlier, is the importance of 
realistically specifying plume dimensions during travel time downwind.  Practical methods have 
been applied in the initial ver~.ion of PI>M to deterw.ine the horizontal and vert\,cal ~~mensions of 
each Lagrangian plume section throughout its life cycle. 

Each rectangular plume cross-section has a width (WP)  and an overall height from plume bottom 
to top denoted by }\ .  The traditional dispersion parameters in the horizontal (cry)  and vertical 
( crz)  have been employed in order to derive these plume dimensions.  In the current version, the 
plume width and plume height values are determined according to 

9-6 

w  =  aa 

p 

y 

Hp  =  hT  - hb 

EP N600/R-99/030 

(9-la) 

(9-lb) 

where a has been set to 3.545 which is obtained from 2(nY\ an adjustment parameter from an 
elliptical shape to the rectangular plume.  The top and bottom heights of each plume cross-section 
are denoted by hT  and hb  , respectively.  The methods used to determine values for the parameters 
in Equations 9-1 a and 9-1 b are discussed next. 

During the plume rise phase,  a plume experiences initial growth due to buoyancy-induced 
turbulence.  To account for this process,  practical methods have been used to determine initial 
values for the dispersion parameters.  The initial value of aY is computed according an expression 
suggested in Irwin (1979), as advanced by Pasquill (1976),  given by 

(J yo  = fl.h/3 . 5 

(9-2) 

In Equation 9-2, the initial value is a function of the plume rise (tl.h) above the stack top. 

Two methods have been installed to prescribe an initial vertical plume thickness.  The widely used 
approach advanced by Briggs (1975) is to define!\, to be equivalent to the amount of plume rise 
(tl.h).  This method can result in a rather thick plume when there is considerable plume rise, 
particularly, at night when experimental evidence suggests nocturnal plumes may be relatively 
thin.  Consequently, an optional approach provided as an experimental alternative has been 
included.  It consists of an empirical form developed by Gillani (1996) which is based on analyses 
of observed plume data taken during a field  study (Gillani et al.,  1981).  His regression analyses 
produced an expression given by 

a  = Ae (-BdT!dz) 
zo 

(9-3) 

where dT/dz is the vertical ambient temperature gradient at the plume centerline height and the 
best-fit values for A and B are 15  and  117, respectively.  A minimum azo value is set to 3 m. 
Then, the initial!\,  is determined from SZOFA times 0 20•  SZOFA is a user-specified input 
parameter for a PDM simulation and it is currently set to 3.545. 

Atmospheric turbulence and wind direction shear contribute to lateral plume growth during travel 
downwind.  During the daytime convective period, the turbulence component is often dominant, 
although directional shear across the PBL also has an important role in contributing to plume 
spread with travel distance.  In contrast, turbulence is generally weak at night, and horizontal 
plume expansion may be dominated by wind direction shear over a plume's depth.  Thus, the 
composite horizontal (lateral) dispersion parameter ( aY ) can be defined by 

'''!! 

1111

'!

2 
2 
ay  = aye  +  ays  +  ayo 

2 

2 

(9-4) 

where the turbulence ( ay1)  and direction shear (a.) terms may both contribute to horizontal plume 
growth after the initial plume spread. 

111111"" 

A general form for ayi can be expressed by 

'111111·11 

aye  = avTJ(TfT.J 

(9-5) 

ilii:lh 

'1111:':111"1" 

,, 

' 

where av is the standard deviation of the lateral wind (v) component, Tis travel time, and 'tL is the 
Lagrangian time scale.  Although various expressions for the function,  f(thJ, have emerged (e.g., 
Draxler,  1976; Irwin,  1983), the following form from Weil (1985; 1988) and applied by others 
(eg.,Venkatram,  1988) has been adopted. 

f  =  (1 

11;111 

(9-6) 

This form is advantageous since it fits dispersion at both the short (t < -i:J and also long travel 
times (t>>-i:J.  Since the Lagrangian time scale is small relative to the pJume sc;:~tio!!,,,travel tim~,,~, 
which are often several hours, the expressions above reveal the familiar relation that a>1  increase.s 
with the square root of travel time.  Therefore, the following form according to Weil (1988) at ..... ·· 
long travel times, which has been examined by Clarke et al.  (1983), is given by  ·· 
"'  · 

·· 

• 

1111, 

,, 

2 
aye  =  2av-r:LT 

2 

(9-7) 

The above equation also displays the square root of time increase for the lateral dispersion 
parameter.  Using modeled meteorological parameters from data files generated by MCIP, 
methods have been incorporated to determine the key parameters needed to solve the above 
expressions for ayi.  Values for av and  'CL  are computed with a set of formulas presented in Hanna 
et al.  (1982) and Hanna (1984), respectively, for various stability conditions based on Monin(cid:173)
Obhukov length (L) limits.  For unstable conditions with L < 0 

... , '""' 

9-8 

av  =  u.(12  - 0.5z/L)ll3 

'CL  = O. l5z/av 

EP N600/R-99/030 

(9-8) 

(9-9) 

where Zj  is the PBL height and u. is the surface friction velocity.  Under stable conditions defined 
by L> 0, 

av  = 1.3u.(1  - zlz) 

1:  = 0.07z la (z/z.)0
L 

v 

I 

5 

·

I 

(9-10) 

(9-11) 

and z is the height of the plume centerline.  For neutral conditions (L =  0),  values are determined 
by 

av  = 1.3e (-2ftlu.) 

'CL  = 0.5z/av(l  +  15/z/u.) 

(9-12) 

(9-13) 

where fis the Coriolis parameter.  Using av and 'CL values, the contribution from the turbulence 
component is computed incrementally from the derivative of Equation 9-7 and solved at each time 
step.  The turbulence term is accumulated with time.  Other semi-empirical methods available in 
PDM provide an alternative set of parameterizations for av as provided by Weil (1988), 
Nieuwstadt( 1984 ),  and Arya( 1984) for the different stability regimes, which may be selected from 
an input option when exercising PDM. 

Wind direction shear due to turning of the wind over the vertical extent of a plume can also 
provide an important contribution to horizontal plume growth.  While turbulence often dominates 
in the daytime PBL, direction shear during the nocturnal period is the principal mechanism for 
lateral plume expansion since turbulence is generally weak.  An expression by Pasquin (1979) has 
been applied for all  conditions to derive the direction shear term and is given by 

2 
ays  = a~8 2.x2 


(9-14) 

where« is 0.03, Xis distance traveled by a plume section over a particular time interval, and A6 
is the wind direction difference (radians) over the vertical extent of the plume.  The shear and 
turbulence terms are combined to solve for the total a/  in Equation 9-4. 

After the initial plume thickness has been determined, the treatment for Oz differs based on 
whether the plume height is above or below the PBL height.  When the plume centerline height is 
above Zj  ,  then the following expression from Gillani (1996) is applied to determine az. 

2 
2 
0 z  = 0 zo 

·

5

(1  +  bT0
) 
(1  +  bT~.s) 

::II, 

(9-15) 

,11111:::1' 

where b is 2.3  and T is travel time.  Equation 9-15 is applied for T < 4 hours.  At longer travel 
times,  a 2  is set to zero for plumes existing above the PBL in the free atmosphere. 

''Ill;,; 

When a plume section is inside the PBL, the parameterization for the vertical plume dispersion 
parameter can be expressed by the same general form as ay1. 

,1111," 

'w 

az  =  aw  T f(Tlt1) 

(9-16) 

As before,  T is the travel time,  and f has the same function~! form as used for ayt although it now 
contains the Lagrangian time scale in the vertical (tLJ·  The standard deviation of the vertical 
wind component (aw) is computed from the following expressions as given by Weil (1988) for 
unstable conditions and from Venkatram et al  (1984) for neutral/stable conditions, respectively. 

aw  = 0.6w. 

(convective  case) 

aw  = 1.3u.(l  - z!z)314 

neutral/stable 

(9-17) 

(9-18) 

The MCIP data files provide values for the fraction velocity (u.), the convective velocity scale 
(w.) and~·  The plume centerline height is represented by z.  The value of't'Lz is computed from 
the following expressions by Hanna et al.  (1982) for unstable and by Venkatram et al.  (1984) for 
neutral/stable conditions. 

't'Lz  =  0.15-'  [l  - e -5zlz,] 

z 
aw 

(unstable) 

(9-19) 


In the latter expression, the length scale (1)  is derived according to Venkatram et al.  (1984). 

An example application of these methods from a PDM simulation for a plume released at night 
from a MEPSE tall stack is shown in Figure 9-5.  It shows the plume remains rather narrow and it 

l 
'tLz  =  CJ 
w 

(neutral/ stable) 

(9-20) 

stays elevated during the night.  However,  once the PBL grows beyond the plume height during 
the morning period, the plume thickness expands to fill  the entire PBL with the plume top height 
matching~ until it reaches a maximum height.  The temporal variation of the plume width for the 
same example case is displayed in Figure 9-6.  It shows that since the plume has a limited vertical 
extent at night, the plume width increases at a relatively slow rate as sufficient direction shear 
existed over the plume depth to cause lateral spreading.  During the daytime, however, horizontal 
plume expansion occurs at a faster rate due to both greater turbulence and shear contributions. 
For a regional grid cell size of 36 km,  the plume width for this release reached the physical grid 
size during the mid-morning hour.  These results and other simulation results obtained from a 
daytime release versus observed values (Godowitch et al.,  1995) provide preliminary evidence 
that these methods give a realistic depiction of the temporal behavior of the magnitudes of plume 
width and depth.  Additional cases have also been exercised, although results are not shown 
herein, in order to assess the robustness and capability of the algorithms for different days.  An 
extensive evaluation of these dispersion methods in PDM is planned against field  study data 
obtained in the vicinity of major point sources in the Nashville, Tennessee area during the 
Southern Oxidant Study experimental period in the summer of 1995. 

9.3.1.3 

Plume Transport 

With the updated plume top and bottom heights,  the mean wind components are determined by 
averaging the winds over the layers spanned by the plume from hb  to hT.  This approach is 
currently applicable because of the single vertical layer structure of the current PinG module.  The 
mean plume transport speed is used to determine an updated plume centerline position over the 
time interval.  In addition, the positions of the plume edges at the bottom and top are also found 
in order to derive grid indices for later use in applying the proper grid cell concentrations for 
boundary conditions of each plume section. 

9.3.2  Formulation of the Plume-in-Grid Module 

9.3.2.1 

Overview of the Plume Conceptual Framework 

The Lagrangian plume model described originally by Gillani (1986) provided the conceptual basis 
for the Models-3  PinG module.  However, the computer algorit~s .in the C~!Mf,,,J:>inG have,, !ii, 
been rewritten to c~ntain up~,,ated methods for the key processes to be describ~.!i in,. a later sect!~n. 

,,1:::11 
''·ill1 
In a modeling framework,  a PinG plume cross-section can be described as a semi-infinite vertical 
slab moving along a Lagrangian trajectory with a mean wind flow.  A plume cross-section is 
considered rectangular with a vertical height (8i,) and a width (WP).  Temporally, plume spread 
in the vertical and horizontal is specified by growth rates dfl/dt and dW/dt, respectively.  A 
plume cross-section is discretized laterally into an array of attached plume cells or pillars with the 
same 8i,  at time t,  as depicted in Figure 9-7.  Each plume section consists of N pl~.me cells 
(currently, N =  I 0) with the width of each cell being equal.  With respect to the plume centerline, 
there are NL cells on the left side and NR  cells to the right side, such that 

,,, 

,II 

WP=  ~LI  WLi  +  ~~ WRi 
N  =NL+  NR 

(9-21) 

Currently, NL= NR  as Land R refer to the cells on the left and right sides,  respectively.  With 
respect to the plume centerline, the right side of the plume section expands to the right and the., 
left side expands to the left.  The width (w;) of each plume cell is given by 

(w)uR  = CY;.1  - Y)uR 

(9-22) 

where y values represent distances from the plume centerline position.  Normalization of the 
plume cell widths is performed such that as the plume expands laterally, each cell width increases 
in the same proportion as the overall plume width.  Thus,  since the individual cell width is 
normalized with respect to the total plume width, the result is a transformed moving crosswind 
gridded array which is invariant with respect to time.  The transformed crosswind 
coordinate ( 11)  of the model is given by 

Tl;  =  Y1 
WP 

(9-23) 

and d11Jdt is zero.  Therefore, although WP  and Y;  are time dependent, T};  remains constant during 
a simulation.  Additionally, although the fractional width of the cells across the plume section in 
the PinG has been prescribed to be equal in the current setup of the module, the algorithms have 
been generalized to allow for unequal plume cell widths and a different number of cells on the l~ft 
and right sides of the plume centerline as described in Gillani (1986). 

9.3.2.2 

Formulation of the Plume Mass Balance Equation 

The relevant processes, as noted in Figure 9-7,  have been incorporated into the plume equation 
for the mass balance of individual species.  They include dilution and entrainment due to vertical 
plume expansion, dilution and entrainment/detrainment due to horizontal plume expansion, lateral 
diffusion,  gas-phase chemistry,  surface removal, and surface emission. 

The subscript i denotes plume cell i (left or right of the plume centerline), subscript j  denotes 
species j, and superscript t denotes a particular time.  Thus,  Cji is the concentration of species j  in 
cell i at time t,  and mji is mass of species j  in cell i at time t.  In the following derivation of the 
terms of the mass balance equation for any species j  in cell i,  the subscript j  is omitted for 
convenience; also,  the equations and terms do not show "L"  or "R"  representing a left or right 
plume cell,  since the equation and terms apply similarly to both sides.  Consequently, consider the 
mass balance of (left or right) cell i (for species j) corresponding to the its expansion during a 
small time interval dt. 

ami  =  (am;)Disp  +  (am;)Em1s  +  (am;)Chem  +  (am;)Dep 

(9-24) 

Now, the following expression is also applicable for the plume cell mass. 

a  = 
m; 

m1 

.' ... d'  -

m1 

.'  =  c.' ... drt'H'.,.d'w'.,.dr  - c.(J'H'W' 
i  P  P 

P 

P 

i 

(9-25) 

Herein the alongwind dimension in the downwind direction (l =  Udt) is prescribed, which is 
determined from the initial mean transport speed (U) over the time interval dt. 

Mathematical expressions are presented in subsequent sections for the updated concentration of 
species j  in cell i due to the action of the individual processes during time interval dt.  In the 
current version of PinG,  only surface emissions after plume touchdown are included in the 
emission term since (non-MEPSE) point-source emissions into grid cells (above the lowest cell 
layer) neighboring the MEPSE plume are released uniformly into the entire emission grid cell and 
impact the MEPSE plume only through the boundary conditions.  Surface removal is due to dry 
deposition at the ground.  Gas-phase chemistry is included with the full  chemical mechanisms of 
the CCTM in the chemistry term.  In the future,  the PinG will be adapted to employ the same 
aerosol module used by the CCTM.  No wet processes are treated in the initial PinG version.  The 
current approach to address limitations in the initial version of PinG is to transfer the plume over 
to the grid solution whenever conditions exist (e.g., precipitation) which the PinG treatment 
cannot adequately accommodate.  This issue will be addressed in more detail in the section on 
plume handover. 

9.3.2.3 

Treatment of Plume Expansion and Diffusion Processes 

Dispersion affects the mass balance of a plume cell as a result of dilution and 
entrainment/detrainment processes related to the lateral and vertical expansion of a plume cros~:-
section during a time interval dt,  as well as mass diffusion which impacts cell c9µc~.ntrat.ions as ...... ,~ 
result of concentration gradients between adjacent cells.  The dispersion processes can be 
expressed by 

(am)n1sp  = (am;)n111E1n  +  (am)Dif 

(9-26) 

;  " 

where the first term on the right side of Equation 9-26 represents the dilution I entrainment I 
detrainment processes and the last term is the eddy diffusion process. 

The relationships developed herein apply to cells on either side of the plume centerline, since the 
only difference occurs in their application when different boundary conditions occur at each edge. 

(ami)DilJEID  = (mtdt  - m/)Dil/EID 

(9-27) 

refers to the change of mass in plume cell i (for any transported species j) during dt as a result of 
dilution and entrainment/detr.~inment.  The changes due to just the horizontal ~d 'X~rtical 
exchange processes can be expressed by 

mrdt  = m/  +  (aml)Latdil/EID +  (am;)Vertdi/IEID 

(9-28) 

iii·' 
where Latdil refers to horizontal (lateral) dilution.  In  practice1 ~he lateral dilu!!.?~'"···· •.. 
entrainment/detrainment term in Equation 9-28 is determined and then the last term is solved. 

::::;:.,,,,.,i 

11, 

The lateral dilution term in Equation 9-28 is given by 

''II 

(am)ia1d111E1n=  1H;cc/+1aY;+1- c/ay) 

However, the following relationship also exists. 

ay1 = 11,awp 

9-14 

11111 

'

(9-29) 

·;1! 

""''',,111 

(9-30) 

since 11  is invariant.  Therefore, Equation 9-29 can be revised to the following form. 

(am)Latdi/IEID=  1H;(11i .. 1c/+1  - 11P/)aWP 

(9-31) 

At this time, (i.e.,  after lateral dilution/entrainment/detrainment) the width of a plume cell is wtd'. 
With the cell widths remaining unchanged,  plume vertical expansion occurs (i.e., in the up and 
down direction for an elevated plume, and up only after plum~ touchdown).  Let Ctdt and Cbt+dt 
denote the concentrations of species j  above and below plume cell i,  respectively, for the elevated 
plume case.  Ca  and Cb  represent the CCTM grid concentrations at t+dt.  Once the plume becomes 
surface-based, only Ca is relevant.  The equation for the vertical dilution term is given by 

(a  ) 
_mj  VertdiYEID=  wi 

I 

t+dt(C t+dtaH 

a 

l+dt 

a  +  Cb  aHb) 

(9-32) 

where the changes in Ha and Hb  denote plume vertical expansion of the upper and lower plume 
boundaries, respectively.  An assumption is made that 

aHa  = aHb  = 0.5aHP 

for an elevated plume cross-section, and after a plume reaches the surface 

aHa  = aHP 
aH  = 0 

b 

are applicable.  Equation 9-32 can be rewritten accordingly as 

(am ) 

_  I 

r+drC r+dr 

i  VertdiYEID- W i 

*  aH p 

where 

cr .. dr=  (C  +  C) 
chdr=  cr,.dr 

* 

a 

b 

* 

a 

(elevated plume) 
(surface-based plume) 

9-15 

(9-33) 

(9-34) 

(9-35) 

(9-36) 

for an elevated plume and a plume bottom at the surface, respectively.  The combination of 
Equations 9-3 1 and 9-3 5 produces the following form. 

,;11, 

(am  1)Di11E1D  = 1H;awp(TJ1..ici~1  - riP/)  +  zaHpw/ .. d'c! .. d' 

(9-37) 

However, by substituting the next expression, 

(a 

).. 

mi Dil/EID 

I 

= cl ... dl!H' ... dt 

p  wi 

1.-d1  - CllH'  t 
pW; 

i 

..  (9-38~ 
1j 

111l

.Ill!" 

,'1111, 

1111111 

and rearranging, the concent~ation after the total dilution/entrainment/detrainment step is. 

• 

H 1  aw 
cl,.dl=  c  1-11 __ ,_w.1.-dt  +  __ P ___ P_( 
11-dl 
H 
p 

w.1 
t.-dt 
H 
p 

t" p 

w. 

I 

I 

I 

t•dl  T}l.,.l 

c.I  -

/1"! 

.C.')  +  __ P_Ct .. dt 

aH 
(1"d( 
H 
p 

··· 
• 
······ 

T}, 

I 

(9-39)  : 

The first term on the right-side is related to the dilution effect and the ne~ two terms are the 

lateral and vertical entrainment/detrainment effects, respectively.  The subsequent expressions are 
also applicable. 

w/=  Y1~1  - y/  = W/TJ;.1  - TJ) 
< 
,,.d, 
w, 
'lli ... l  -
H 1•d1= H 1 +  aH 

TJ; ... 1  - T}i  = 

=  WP 

1 ... d1< 

) 

p 

p 

p 

,,i  WP  +  WP 
)( 
) 

a 

By substituting the above expressions into Equation 9-39, and rearranging gives 

c' .. d' 

i 

= C.1  (HPWP)' 
I  (H  W )1.-d1 

p 

p 

+  --C  + 

aHP 
H 1 ... ddt 
p 

l1"dt 
• 

H;  awp (TJ, ... 1C1~1  - TJP/) 
H1 ... d1  W 
p 

- TJ) 
I 

(TJ 

/Tl 

p 

ii:,,, 

(9-40) 

(9-41) 

However, Equation 9-41  can be simplified and re-written as 

'111111! 

9-16 

=  1 +~ ( (} .. HC~Tdt + 1 +\ ..  [(C/ + . )..: 

W 

111·t-l 

H 

EP A/600/R-99/030 

(11;..iC;~I  - 11;C/)]l 
11 1 

(9-42) 

c.fTdf 

I 

where 

aHP 
).H  =  H 

p 

awp 
)..  = - (cid:173)
w  w 

p 

(9-43) 

When applying Equation 9-42,  certain boundary conditions must be specified.  In particular, if 

i  =  I 
·  N 
l  = 
L 

' 

, 
·  N 
l  = 
R 

Tl;  =  111  =  0 

' 

c 

i·rl  = 

c'Tdt 

bg 

(9-44) 

where Cbgt+dt  and c.i+dt  are the particular grid cell concentrations from the CCTM solution for the 
current time step. 

In the execution of the CCTM/PinG, both the grid-cell and plume-cell concentrations are updated 
by numerical integration of the corresponding mass balance equations from time t to t+dt, where 
dt is the CCTM advection time step (MSTEP).  The sequence is that CCTM performs this 
integration and updates the concentration field  first,  generating ci+di in each grid cell,  before PinG 
is called to do the same.  PinG performs the integration in a fractional step approach; first for 
plume dispersion (step  I) then for the surface emission and removal processes (step 2), and finally 
for plume chemistry (step 3).  The chemistry integration is actually performed in smaller chemistry 
time steps, internally determined for the plume cells by the chemical solver, in sequence, until the 
integration is completed for the time interval dt.  The emission/removal step is implemented in a 
single integration time step.  The dispersion step is performed sequentially in subtime steps until dt 
is reached.  The PinG dispersion time step is constrained by a restriction when applying Equation 
9-42 arising from the fact that, in a given time step, the location after expansion of the inner 
interface of a given plume cell (i.e.  yrd1 cannot pass beyond the location of the outer interface of 
that cell at the start of the ex expansion.  Therefore, the following constraint applies. 

t+dt 

t 

Y; 

:5:  YiTI 

2:5:i:5:NL,R 

(9-45) 

This criterion requires that the following limit for the PinG time step must be satisfied. 

atdisp  ~  (TJ,..i  - Tl)  1 
A.~ 

Tl; 

A.•  -
w- -

1  dW. 
p 
WP -;ft 

It should also be noted that A.w*  is related to Aw according to 

.t..  . = rd'A. ,.,1 
111"°' 
w 

t 

(9-46) 

(9-47) 

(9-48) 

During the model simulation, WP (t) and dWP/dt are provided to the PinG module from the PDM 
data file.  Thus, Aw·  is determined directly,  and Aw can also be computed using the plume widths 
from time t and t+dt. 

The dispersion step is not complete without inclusion of the diffusion term.  The diffusion 
equation of a species concentration for any plume cell i may be expressed by 

aci 
at 

ac, 
= -(K-) 
y  ay 

a 
ay 

where Ky  is the horizontal eddy diffusion coefficient.  It can be expressed by 

K  = _!_~(a2) 
y 
y 

2 dt 

(9-49) 

(9-50) 

By applying the relationship between ay  and WP  in Equation 9-1, the following form for Ky can 
be derived. 

K  = 
y 

w2 
P  ~ • 
--11. 
a2  w 

(9-51) 


Numerical integration of a form like Equation 9-49 is solved with the Crank-Nicholson method in 
which centered differencing is used for both time and space derivatives to solve the diffusion 
equation.  This technique provided stable numerical solutions compared to an explicit method. 
The resulting finite-difference equation is then solved by matrix decomposition of the coefficient(cid:173)
matrix into upper and lower (LU) triangular matrices.  For convenience in setting up the matrix 
form of the finite difference diffusion equation for all  plume cross-section cells, a set of 
simultaneous equations is developed which are written in a vector-matrix form for solving by LU 
decomposition. 

The derivation of the set of equations is provided.  By averaging Equation 9-49 in time to obtain 
ac/at at t+at,  and using center-differencing to solve for a2C/ay2

,  simplification leads to 

k-t-1 

Ci_i[-

Kyat 

(yi+1- y)(yi+I - Y,-1) 

+  c.k ... 1[ 

1+1 

Kat 

y 

) 
(yi+l- yi)(y/+2- Y; 

1  =  c.k 

1-I (y 

i+I- y)(yi+I- Y;-1) 

1 

k-t-1 

]  +Ci  [I+---"-----
(yi+I - y)(yi-t·2- Y;)] 

+ 

Kyat 

] 
(y/+I - yi)(yi+l - Yi-I) 

Kit 

Kat 

y 

+  C/[1  -

K& 

Y 

-

(yi+I - y;)(yi.,.2- Y) 

K& 

Y 

K& 

]  +  C;~1 [ 

] 
CY; ... 1 - Yi)(y;+2- Y) 

Y 

Yi+l - Y;)(y; ... 1- Y;-1) 

where small k is used as a time index for time t,  and k+ 1 represents t+at, i is the index to the left 
edge of plume cell i,  and y is the distance from the new reference position at the left 
background location of the plume cross-section.  By using the relationship that yi= TJiWp,  the 
following relations can be defined. 

(9-52) 

a.=  Kyat _______  _ 

w;  (TJ;d - TJ)(TJ/+l - 'Tl;-!) 

I 

Kat 

w;  CTJ; ... 1- TJ;)(TJ; ... 2- TJ;) 
Yi  =  - - - - - - -

(9-53) 

(9-54) 

P;  = <X;  +  Y; 

(9-55) 

By substituting the above expressions into Equation 9-52, the following formuJa is . .obtained. 

k+ I 

-a.Pi-I  +  (1 +P;)C; 

k+ I 

- yp, .. I  = a.pi-I  +  (1-P;)C,  +  YPi+l 

k-r I 

k 

k 

. 

k 

Equation 9-56 represents a system of simultaneous equations in the form, 

Ax=  b 

' 

(9-56) 

':1111 

(9-57) 

where, x =  (Cik+t  +  ····~+/+ 1  )'and N is the total number of plume cells.  Additionally, the 
expression, A =  I - G,  is defined where A is a square matrix of size (N+ I )x(N+2), a,nd b = 
(I+G)Ck.  In these expressions, I is the identity matrix, and G is a tridiagonal matrix of the form, 
G =  tridiag (a,  p,  y), and is of size (N+2)x(N+2).  Subsequently, the simultaneous system of 
equations given by Equation 9-57 is solved using LU decomposition. 

The boundary conditions are also included in the above system of equations since N+2 is used. 
Appropriate boundary conditions are provided by the CCTM grid concentrations for the right and 
left edges of each plume cross-section.  At left edge boundary,  al= P1 =  y l  and b1  = cl= c~L· '' 
At the right edge boundary,  a.N+2 ,  PN+2,  and  YN+2  are zero which leads to bn+i =  ~+2 =  ~t 

9.3.2.4 

Surface Area Emissions and Dry Deposition 

During the PinG simulation,  surface emissions are injected into individual surface-based plume 
cells.  The surface emissions, contained in layer I  values of the 3-D emissions file which is also 
employed by the CCTM, are used in  all plume cells of a plume cross-section.  For each grid cell, 
there is an emission rate for certain species ( qj).  In particular, a lVIEPSE plume cross-section 
passes over such gridded surface emissions.  Consequently, the change in concentration of plume 
cell i during a time step from surface emissions is· 

a1 
J_k  __ }  --
aci  - 1 t:ix!:iy HP 

q 

(9-58) 

where ~ herein is an appropriate species-specific factor for the conversion of concentration from a 
mass unit to the appropriate volume unit. 

Since the PinG formulation is composed of a single vertical layer,  it was not designed to  have the 
capability to properly ingest other elevated point-source emissions into the MEPSE plume cross(cid:173)
section.  As long as such an along-path, point-source emission remain at a relatively low value, 
the impact on the MEPSE concentration field  is assumed to be felt through background 
entrainment of the expanding MEPSE plume.  If the other point source emission was ingested,  of 
course, such entrained mass is currently dispersed instantaneously throughout the vertical extent 
of the plume.  For this reason, when point-source emissions larger than a critical value are 
encountered by the MEPSE plume, the current logic is to handover the MEPSE plume to the grid 
solution. 

Dry deposition is a sink term and occurs at the bottom of each plume cell based on the deposition 
velocity concept. 

acj I 
a1 

- - vj c/ 
-

H 
p 

(9-59) 

where Ct denotes the concentration of species j  and the species-specific deposition velocity (Vdj) 
values are available from a gridded data file.  The same deposition velocity is applied to all  cells of 
a plume section using the Vd values from the grid cell in which the plume centerline is located. 

9.3.2.5 

Gas-phase Chemistry of Plumes 

A gas-phase chemistry mechanism implemented in the CCTM is also invoked by the PinG module. 
The current mechanisms include the RADM2 and carbon bond (CB-4) chemical mechanisms. 
Details about these chemical mechanisms are provided in section 8.  In addition, there is a 
separate PinG module version for each chemical solver (i.e.,  SMVGEAR and QSSA).  Minor 
revisions were needed in the PinG versions of the solver codes to customize them to deal with the 
plume concentration array whose dimensionality differs from the CCTM gridded concentration 
array.  Nevertheless, the PinG gas photochemistry treatment is identical to that of the CCTM. 
Since only gas-phase plume chemistry has currently been implemented, when conditions 
conducive to extensive aqueous chemistry are encountered, a plume section is transferred to the 
CCTM grid system, as discussed in the handover section below.  Future plans include 
implementation of the existing CCTM aerosol module into PinG so that it also has the capability 
to treat aerosol and  particulate species. 

9.3.2.6 

Plume Initialization 

The initialization of each plume cross-section is the first key procedure performed at the start of 
its simulation.  The PinG initialization of a new MEPSE release occurs when a plume cross(cid:173)
section width reaches a minimum width.  The width criterion is a user-specified variable and it is 
specified for the PDM simulation.  A flag indicator variable in the PDM file communicates to 
PinG when a plume cross-section is ready to be initialized.  The current minimum width for 
initialization of a MEPSE plume section has tentativelybeen set at.2 km.  At initial~ation, it is 
assumed that the lateral concentration distribution across the plume section of the primaiy 
emission species exhibits a Gaussian shape.  For all other species, initial concentrations have been 
set to a machine minimum-value (ie.  1 o-30

· ·•,!I,\ 

). 

The lateral concentration distribution of the primary species in the plume cells (i.e.,''11initial 
condition"  of the plume concentration field)  is given by 

Cy(O)  = "J.-1---exp[ -0.5(yi la )2] 

1 

q. 
UHP  2na.Y  . 

• 
. 

Y. 

... 

I 
., 

(9-60) 

., 

'" 

·l,,,,111111 

where, y•  is (Yi - y0 ), the distance of the outer edge of cell i from the plume centerline, 
and U is the mean transport speed, qj is the MEPSE emission rate of species j, and  ·· 
kj  is the appropriate species specific mass-to-volume conversion factoL 

"I 

11i\' 

:11111 

i 

11 

In PinG,  it is assumed that the MEPSE emissions are at an hourly resolution,  and that we are 
performing plume simulations of hourly releases.  The assumption is made that the emission rate 
(q.;)  remains constant over the hour.  Consequently, at the handover time, the transformed mass 
impacted by the various plume dynamic and chemical processes corresponding to one hour of 
emission is released to the grid solution. 

9.3.2.7 

Methodolog)'.,,,. for Plume Feedback to the Eulerian Grid 

When the subgrid scale phase of a plume section simulation has been completed, the plume 
material is ready for transfer to the CCTM grid.  The total concentration of each species in any' 
plume cell is composed of a component equal to the background concentration, and an additional 
component consisting of the plume concentration, which differs from the background grid level. 
In performing the handover,  the conceptual basis is that only the plume component is related to 
the corresponding MEPSE emissions.  Thus, the feedback is restricted to the plume contributio"n. 
Additionally,  since hourly plume releases are simulated from each MEPSE source in PinG, and 
since the assumption has been made that each such hourly emission remains constant for the full 
hour, the current practice is to handover the contents of a plume section corresponding to a ful! 
hour release.  Thus, for each plume species, 

m  =V(C-C) 

p 

p 

bg 

p 

(9-61) 

where average values for the plume and background grid concentrations are employed.  The 
average plume concentration (CP  ) of each species is determined from all plum~ c~Us.  The 
background grid value (Cbg) represents the average of the CCTM grid-cell concentrations on 
each side of the plume for all appropriate layers over the vertical extent of the plume.  The 
handover plume mass is distributed into the column of cells up to I\ in which the plume centerline 
is ,ocated at the handover time.  The feedback of plume mass represents an adjustment to the 
CCTM grid cell concentrations from an average concentration (Cadj).  From a mass balance 
perspective, 

EP A/600/R-99/030 

C 0 d1V0  = (CP  - Cbg)VP 

where VG is the CCTM grid cell volume.  The plume volume (VP) is determined from 

VP  = WPH/UAt) 

(9-62) 

(9-63) 

where U is the mean horizontal wind  speed, At is a  I-hour interval and UAt corresponds to the 
alongwind dimension (A~) spanned by the plume section.  In a similar fashion,  the grid volume 
impacted by the plume is given by 

V0  = Ax0 Ay 0(Z~  - Z~) 

(9-64) 

where AXcr and AyG are the horizontal grid cell sizes, and ZG1  and Z Gb  are the heights of the top 
and bottom of the model layers spanned by the plume. 

Substituting the above expression into Equation 9-62 yields: 

C-(-C  _HWAx 
p 

·  -C)PP 
p 
G 

adJ-

bg 

17 

(9-65) 

In the initial release version of PinG when plume handover is triggered, an entire hour's worth of 
plume release in its transformed state is transferred to the appropriate column of grid cells during 
a CCTM advection time step.  In the future,  the intention is to incrementally  transfer the same 
plume excess to the impacted grid cells over a one hour period of model time steps.  In this 
manner, the continuous emissions after being treated in PinG would be transferred during a one 
hour period following the handover time. 

PinG simulations are primarily intended for the coarse-grid, regional modeling grid sizes with the 
horizontal resolution (A) of nominally 20-40 km.  Daytime l\ffiPSE plumes reach or are close to 
chemical maturity when the plume width attains these grid sizes.  However, PinG simulations at 
finer grid sizes are possible, however, test runs have not been performed but are needed in order 
to assess the benefits of this approach at urban grid cell sizes.  As the primary purpose of PinG Is 
to improve the subgrid-scale treatment of large point-source plumes, the key handover size ... 
criterion based on plume width relative to grid cell size has been prescribed according to 

w -2  ;::>; 
/::,,. 

Yer 

(9-66) 

The default value for y er is  1. 0.  Therefore, when a plume section width attains the grid cell size, a 
flag indicator variable generated by PDM triggers PinG to perform the handover process. 
Currently, the CCTM system is  operated in a  I-way nesting mode, as multiple nested grids ar~ .. ..not 
performed in a single simulation.  Consequently,  PinG is not equipped to handle ~J:ie movemen~ of 
a plume from a larger to a finer gridded domain within the same simulation. 

11::,.1' 

I 

• 

" 

'I~ 

Ideally,  plume handover also occurs when the plume has also reached chemical maturation when 
concentration differences within the plume become rather small.  A chemical criterion also 
incorporated into PinG involves a default condition recognizing chemical maturation.  It is based 
on an average plume concentration ratio of 0 3 /0x ,where Ox= 0 3+N02  denotes the principal 
oxidant species using concentrations from all cells in a plume section.  This ratio is employed as a 
surrogate indicator of plume chemical age.  The specific chemical maturation criterion is: 

• • •  ,,,,,,,... 

·••··· 

llW 

·,, 

. 

1:,,, 

03 
-==  ;::.;  r 
Q 

chem 

x 

(9-67) 

where rchcm  represents the critical value.  The default value has been set at 0.99.  Tests have 
indicated this value to be a good indicator corresponding to a chemically-mature stage 3 plume. 

The use of 0 3  I Ox  as a surrogate for plume age is not common compared to NOx I NOY  where 
NOY  includes all the reactive oxides of nitrogen.  However, NOjNOy has not been applied as a 
chemical criterion because injection of fresh emissions from large point sources has been observed 
to suddenly decrease the chemical age of the background which leads to "premature" activation of 
the chemical criterion.  Experience from testing with the first chemical criterion has proved it tp 
be more satisfactory. 

·· 

In the initial version of the Models-3  PinG, additional criteria are needed which can also trigger 
plume handover to the Eulerian grid cells.  As the PinG module is upgraded in the future,  the 
these criteria may be avoided. 

111111 

1111111111 

The next two criteria are related to non-l\.1EPSE emissions in the immediate vicinity of a l\.1EPSE 
plume cross-section. 

Urban Criterion 

PinG currently does not have the facility to injest different surface emissions into individual plume 
cells.  Such a condition is likely to be encountered by a l\.1EPSE plume after touch-down when it 
passes over an urban-industrial area with a very inhomogeneous spatial distribution of emissions. 
Gillani and Pleim (1996) have identified such "urban" areas based on the following criterion 
pertaining to the emissions of NOx. 

fq  (NOx)  >-10 12  molecules/cm 2/s 

(9-68) 

This condition denotes a fairly high NOx  surface emission flux  (fq ).  The critical value of 1012 
chosen by Gillani and Pleim (1996) was based on an emission inventory with a horizontal spatial 
resolution of about 20 km.  For such a grid size,  this condition corresponds to an emission rate of 
about 0.3  kg/s.  Thus, the selected "urban" emissions handover criterion is: 

qNOx  2:  0.3  kg/s 

(9-69) 

The above condition must be satisfied to trigger plume section handover, and the plume bottom 
must also be at the ground for this criterion to activate. 

Point-source Emissions Criterion 

The initial PinG version does not possess multiple vertical layer resolution of the plume.  Thus, if 
a plume section entrains the elevated concentration of a primary species in a background grid cell 
from a fresh NOx  emission from a non-l\.1EPSE major point source, such entrained mass is 
instantaneously mixed vertically throughout the l\.1EPSE plume.  If the non-l\.1EPSE point source 
emission is large enough, the related error of its treatment in the l\.1EPSE plume relative to its 
treatment in the gridded solution becomes large.  Thus, it is advantageous to handover  the 
l\.1EPSE plume under this circumstance.  A handover criterion of 3.33 x  10-6  ppm/s has been set 
which corresponds to about 2 ppb/I 0 minutes.  If the new non-MEPSE point-source emissions in 
the immediate vicinity of the l\.1EPSE plume are large enough to raise the average background 
concentration (left/right and vertically-averaged over MEPSE plume height) by more than 2 ppb 
in  I 0 minutes, then handover is also triggered. 

Precipitation Criterion 

PinG has no facility to handle wet removal of the plume.  Consequently, if the total(convective 
and non-convective) precipitation rate in the cell containing the !\.1EPSE centroid exceeds a 
critical value, a plume handover is also triggered.  Currently, the default value of this critical 
precipitation rate is 0.00008 cm/s or about 0.3  cm/hr. 

Excessive Wind-Shear Criterion 

Since the plume treatment is PinG is based on a Lagrangian simulation for a single vertical layer, 
there is no facility to properly treat the effect of wind shear.  When excessive speed wind shear 
over the vertical extent of the plume exceeds a critical value, plume section handover will occur: 
on the basis that the treatment of such wind shear will be better treated in the multi-layer grid 
model. 

' I  

Domain Boundary Criterion 

Finally, a MEPSE plume section is transferred to grid cell in which the. centroid of the MEPSE'is 
located if it is about to exit the gridded domain.  Otherwise the plume conce~tration co~tribution 
cannot be accounted for ifthe plume travels beyond the model domain boundary. 

In all handover cases considered above, the entire MEPSE plume section is transferred.  There is 
a particular condition in which only a top portion of the plume may be handed ov~r1 while the  ,
remaining part of the plume continues to be simulated.  This type of premature partial handover is 
forced because PinG does not possess adequate vertical resolution, and also because the plume 
simulation in PinG is based on a Lagrangian approach.  This condition arises when the plume is 
well-mixed throughout the daytime PBL, and the PBL height decreases significantly along the 
path of the plume, for whatever reason.  In reality,  in such a case, the plume would be split 
between an upper portion above the PBL and a lower  portion remaining within the PBL.  The 
two portions could then experience very diverse stability and flow conditions which cannot be 
accommodated in the current version of PinG. 

;, 

1

The approach is that once the PBL height begins to decrease,  and it decreases by more than  15% 
of the previous Zj, the plume section is handed over.  However, the lower portion continues to be 
simulated.  If the decrease in zi is temporary and  it later begins to incre!lse again, the remaining 
simulated plume would expand vertically just as if it was the full plume.  When Zj  continues to 
decrease, as during the evening transition period, then the partial handover would continue 
incrementally as the PBL decreases by each  15% segment until the nocturnal mixing height is 
reached, at which time the remaining plume is also handed over.  In this manner, l!!nG, does n~,~ 
continue to simulate the well-mixed daytime plume into the night.  This approach avoids the 
Lagrangian simulation of the deep night-time plume which frequently experiences substantial wj9d 
shear conditions related to nocturnal jets and inertial oscillations. 

The situation is different for a fresh "MEPSE release into the nocturnal stable layer above the 
shallow mixing layer.  Such a plume is likely to be thin in the vertical and,  therefore, not very 
likely  experience excessive shearing conditions.  In our Lagrangian approach, therefore, PinG will 
generally simulate fresh nighttime plume sections which do  not experience excessive shear 
because of their limited vertical extent. 

9.3.2.8 

Test Application of the Plume-in-Grid Modeling Approach 

The schematic diagram in Figure 9-8 shows how the PDM and PinG fit into the overall 
Models-3  CMAQ system of science programs.  Since PDM serves as a processor program, it is 
exercised in advance of the CCTM/PinG simulation.  PDM requires the MEPSE stack parameter 
file generated by the MEPPS emissions system,  and a set of meteorological data files prepared by 
MCIP from an l\llM5  simulation.  The details of these input files which are needed to perform a 
PDM simulation are described in the Models-3  user guide (EPA,  1998). 
parameters for the PDM processor are also defined in the user guide. 

Input and output 

The PinG module is an integral part of the CCTM, as depicted in Figure 9-8,  and it is invoked by 
the CCTM driver program along with the other science processes.  In the sequence of processes, 
PinG is called just before the grid model performs the gas-chemistry step.  Consequently, when 
PinG completes a time step, the CCTM driver calls the chemistry to undertake gas(cid:173)
photochemistry on the 3-D gridded array.  When PinG is being exercised, it needs data files 
generated by ECIP, MCIP, PDM and also the MEPSE emission file from l\1EPPS processing. 
The PinG module also generates a plume concentration file containing the species concentrations 
in each plume cell for each plume cross-section.  Since the PinG concentration file is a Models-3 
specific format,  it can be viewed in a visualization software package designed for such data files. 
In addition,  an effort is  planned to visualize both the plume and grid concentrations on the same 
display. 

The plume-in-grid algorithms were exercised for a single JV1EPSE within a 36 km gridded domain. 
The plume cell 0 3  concentrations in  Figure 9-9 are for various times from a selected plume 
section released during the early afternoon.  The PinG module simulated  10 plume cells in this 
plume section, with the concentrations on each edge of the cross-section at various times being 
the CCTM gridded values used as background conditions.  It is evident that the modeled 0 3 
concentrations displays the same chemical stages described earlier.  A significant ozone deficit 
exists in the narrow plume for  1-2 hours after release.  As the plume expanded, the 0 3 
concentration gradually recovers and  exhibits higher 0 3  in some cells outside the plume core of 
the cross-section at  15:30 than the grid value at each edge or in the plume core.  This represents 
stage 2 of plume chemical evolution.  Eventually,  stage 3 was attained as all  plume cell 0 3 
concentrations exceeded the CCTM concentrations on each side of the plume with the maximum 
excess of about  I 0 ppb in this case.  Shortly after  1730 LST,  the last time displayed in Figure 9-9 
for this plume section, it was transferred to the CCTM grid system.  Therefore, this plume section 
was active in the subgrid phase about 5 hours in this case.  Once the feedbeack of a plume section 
occurs, PinG no longer simulates it which reduces computational time. 

The results of this test case and others not described herein are encouraging since they compare 
rather favorably in a qualitative sense with observed concentrations (not shown) aci:oss the sam~. 
"MEPSE plume at about the same time.  Nevertheless, an extensive evaluation of the PinG is 
planned for several case study days with plume data for various species obtained during the 
Southern Oxidant Study 1995 experimental intensive program c:qnduc:ted in the Nashville, 
Tennessee area.  The evaluation will assess the capability of the PinG components to treat 
pollutant plumes and determine the impacts on the regional grid concentrations. 

"'""' 

""""'" 

' 

'!I: 

9.4 

Summary 

,,, 

111111 

A plume-in-grid technique has been developed for use in the Models-3  Community Multiscale Air 
Quality modeling system.  The key algorithms include a plume dynamics Model (PDM) processor 
designed to generate a data file for use in the PinG module simulation.  The PinG module has 
been fully integrated into the CCTM Eulerian grid model to provide a more refined, reali.stic 
treatment of the physical and chemical processes impacting selected major point source emissions 
during a subgrid scale plume phase in a regional model applicatipI1.  The ini.~~~,,,,,rel~,~e version i.~ 
limited to performing gas-phase photochemistry within the plumes.  A future PinG version is 
expected to include an aerosol and particulate modeling capability.  Test simulation results have 
been conducted and qualitative results are encouraging regarding the treatment of plume growth 
and plume concentrations.  However, a rigorous diagnostic evaluation of the various processes is 
planned using the 1995  Southern Oxidant Study-Nashville experimental plume dattj:,  Future 
. 
advancements and refinements of the initial  plume-in-grid algorithms are anticipated and upgraded 
algorithms will be reflected in upcoming releases. 

9.5 

References 

Arya, P.,  1984:  Parametric relations for the atmospheric boundary layer.  Boundary-Layer 
Meteorol.,30, 57-73. 

Briggs, G.A.,  1975: Plume rise predictions.  In:  Lectures on Air Pollution and Environmental 
Impact Analyses, Workshop Proceeding, Boston, MA,  1975, pp  59-111. 

Chang, J.S., K.H.  Chang, and S.  Jin,  1993:  Two-way and one-way nested SARMAP air 
quality model.  International Conf.  on Regional Photochemical Measurement &  Modeling 
Studies, November 8-12,  San Diego, CA,  A&\VMA, Pittsburgh, PA. 

''' 

·· 

Clarke, J.F., J.  Ching, J.M.  Godowitch,  1983: Lagrangian and eulerian time scales 
relationships and plume dipsersion from the Tennessee Plume Study.  Sixth Symp.  on 
Turbulence and Diffusion, March 22-25,  1983, Amer.  Meteorol.  Soc., Boston, MA. 

Draxler, RR,  1976: Determination of atmospheric diffusion parameters. Atmos. Environ.,  10, 
99-105. 

EPA,  1998: Models-3  Volume 9b:  User Manual, EPA-600/R-98/069b, U.S.  Environmental 
Protection Agency, Research Triangle Park, NC 277 I I. 

Gillani?  N.V. and W.E. Wilson,  1980: Formation and  transport of ozone and aerosols in 
power plant plumes.  Ann.  N. Y.  Acad.  Sci., 338, 276-296. 

Gillani,  N. V.,  S.  Kohli,  and W.E. Wilson,  1981:  Gas-to-particle conversion of sulfur in 
power plant plumes:  I: Parameterization of the conversion rate for moderately polluted 
ambient conditions.  Atmos.  Environ.,  15,  2293-2313. 

Gillani, N.V.  and J.  E.  Pleim,  1996:  Sub grid scale features of anthropogenic emissions of 
NOx and VOC in the context of regional eulerian models.  Atmos. Environ., 30, 2043-
2059. 

Gillani,  N.V.,  1986:  Ozone Formation in pollutant plumes:  A reactive plume model with 
arbitrary crosswind resolution.  U.S.  Environmental Protection Agency, EPA-600/3-86-051, 
Research Triangle Park, NC,  85  pp. 

Gillani,  N. V.,  1996:  Personal communication. 

Godowitch, J.M.,  J.  Ching,  and  N.V.  Gillani,  1995: A treatment for Lagrangian transport and 
diffusion of subgrid scale plumes in  an eulerian grid framework.  Eleventh Symp.  on 
Boundary Layers &  Turb., March 27-31, Charlotte, NC,  Amer. Meteorol.  Soc., Boston, 
MA, 86-89. 

Hanna,  S.R., G.A.  Briggs, and RP. Hasker,  1982:  Handbook on atmospheric diffusion,  U.S. 
DOE, DOE/TIC-11223, DE82002045, National Technical Info.  Center, Springfield, VA. 

Hanna,  S.R.,  1984: Applications in air pollution modeling,  Chap.  7,  Atmos.  Turb.  &  Air Poll. 
Modeling, Ed. F.T.M. Nieuwstadt and H.  van Dop, D.  Reidel Publishing Co., Kluwer 
Academic Publishers, Hingham, MA. 

Hicks, B.B.,  1985: Behavior of turbulence statistics in  the convective boundary layer.  J.  of 
Clim.  and Applied Meteorology.  24,  607-614. 

Irwin, J.S.,  1979:  Scheme for estimating dispersion parameters as a function of release height, 
EPA-600/4-79-062, Research Triangle Park, NC.,  56  pp. 

Irwin, J.S.,  1983: Estimating plume dispersion - a comparison of several sigma schemes.  1. 
Clim.  And Appl.  Meteorol., 22,  92-114. 

Kumar, N. and A.G. Russell,  1996: Development of a computationally efficient, reactive sub(cid:173)
grid scale plume model and the impact in the northeastern United State using increasing 
levels of chemical detail.  J.  ofGeophys. Res.,  101,  16737-16744. 
· 

Mathur, R., L.K. Peters, and R.D.  Saylor,  1992:  Sub-grid presentation ()f emission source 
clusters in regio~a~ air quality modeling. Atmos. Environ., 26A, 3219-3238. 

,,,,,,,, ..  , 

,,, 

I 

• :1111::" 

Morris, R.E. M.A. Yocke, T.C. Myers,  and V. Mirabella,  1992: Overview ofthevariable-grid  ., .... , 
Urban Airshed Mqd~l (UAM-V) 85th Annual Meeting of the .A&WMA,  June 21-26~ '11 
1992, Kansas City, MO., AWMA, Pittsburgh, PA 

1111111111 

"'

· 

'' 

' 

" 

•1:: 

"' 

,1,l,11 

'II 

,',Ill, 

Myer, T.C., P.D.  Guthrie and  S.Y. Wu,  1996: The implementation of a plume-in-grid module 
in the SARMAP air quality model (SAQM).  SYSAPP-96-06, Systems Appljc,~~o~ 
International, Inc.,  Available from Technical Support Div., California Air Resources 
Board, Sacramento CA. 

, 

I'" 

,11111,,,,"' 

Niewstadt, F.T.M.,  1984:  Some aspects of the turbulent stable boundary layer.  Boundary(cid:173)
Layer Meteorol., 30, 31-55 . 

1111: 

,

Odman, M.  T.  and A.G. Russell,  1991: Multiscale modeling of pollutant transp9rt!;!Jld ,,,, 
chemistry.  J.  ofGeophys. Res.,  96, D4, 7363-7370. 

,,,,,,11 

111111111111;  ... , 

Pasquill, F.,  1976: Atmospheric dispersion parameters in Gaussian plum modeling: Part IT. 
Possible requirements for change in the Turner workbook values.  EP A.-600/4:.76-030b, 
EPA, Research Triangle Park, NC, 53  pp. 

us 

'Ill 

Pasquill, F.,  1979: Atmospheric dispersion modeling.  J.  of the Air Pon:  Contrl. Assoc., 29, 
117-119. 

' I   Ill 

Seigneur, C., T.W. Tesche, P.M.  Roth, and M.K. Liu,  1983: On the treatment of point source 
emissions in urban air quality.  Atmos. Environ.,  17(9),  1655-1676. 

11111 

11 

Turner, D.B., T.  Chico and J.A.  Catalano,  1986: TUPOS - a multiple source gaussian 
dispersion algorithm using on-site turbulence data. U.S. Environmental Protection 
Agency, EPA/600/8-86-010, National Technical Information Center, Springfield, VA. 

Venkatram, A., D.  Strimaitis, D.  Cicristofaro,  1984: A semiempirjciµ  mqdel tQ  est!mate 
dispersion of evelated releases in the stable boundary layer.  Atmos.  Environ.,  18,  923-
928. 

Venkatram, A.  1988: Dispersion in the stable boundary layer.  Chapter 5,  In Lectures on Air 
Pollution Modeling, A  Venkatram abd J.  Wyngaard, Eds., Amer. Meteorol.  Soc., Boston, 
MA.,  1988. 

Weil, J.C.,  1988: Dispersion in the convective boundary layer. Chapter 4, In Lectures on Air 
Pollution Modeling, A  Venkatram and J.  Wyngaard, Eds., Amer. Meteorol.  Soc., Boston, 
MA.,  1988. 

This chapter is  taken from Science Algorithms oftlte EPA Models-3 Community 
Multiscale Air Quality (CMAQ) Modeling System, edited by D. W. Byun and J. K. S. 
Ching, 1999. 

Locations  of  MEPSE  Sources 

,111 

1.000 

79  .-~--.--r-------r-------,;,r-::~---, 

y=grid _mepse _36kmjul7 foapi 

0.000 

NUM 
fAYE 
by 

MCNC 

1 

1 

~~ 

<\  ",, 

85 

July 7,1995 0:00:00 

Min=0.000 at (1,11 Max:3.DOO at (48,45) 

Figure 9-1  Location of MEPSEs depicted in individual grid cells. 

stagef 

Ozone 

stage 2 

10(?°' 
ppb 

Su~ate 
201 
ppb t  ./ .. _...  Stage3  •· ... ,'.·! l  10x1 o"'' 

./ .. .'\ ·····.-.. 
'. 

..  Bscat 
::1  rri 1 

.... 

Bscat /""""'"~ 
.//" .,.,. . .,·--. 
---·--

SU/fat~· ...... 

/ 

'-': 

~~~ ~  /·~ 
/'-.Ozone 
,,;-,;'/".~~\~ 
:--·  " ....... 
. 
.... 
'· 
,Cr  ~ 

~,·"' 

1224 

o~ 

11 :31 

'---

11 :38 

o~ 

12:31 

15:57 

16:06 

nrne 

Figure 9-2.  Chemical stages of a plume are depicted by aircraft plume data of S02, ozone, 
sulfate,  and aerosol scattering coefficient (Bscat) from crosswind traverses through a large 

PSE plume. Measurements are from the daytime period of the 23  August  I 978 Tennessee 

lume Study experiment. (Adapted from_ Gillani et al,  1981) 

I (a) 

l 111"1!!-hegf~ V.fNI I 

~~tr-OJJ!f ll!l~1: 

J (b)  Top viewJ 

Plume  handover 

rr1(t wo) 

n,p 

Figure 9-3.  Schematics of the a) time-height view and b) top view of the modeling concept of the 
subgrid scale plume in the Models-3 PinG approach. 


.Zp 

••• 
• 
• 
• 
• 
• 
• 

1600 

1400 J 

1200 

~ 
1000 I 

-
'E' 
.... 
::c 
(!$ 
!! 
II.I 
:t 
:::> 
..... 
Q. 

~ 
i= 
0 
II.I 
LI. 
LL w 
..... 
;l 
i! 

600 

400 

200 

0 

900~  i!I  • •  

•• 

'  . 
.. 

• • 

i!I 

4 

6 

·I 
9 

I 

I 

I 
10 

I 

I 
12 

I 
14 

16 

19 

20 

22 

24 

RELEASE  TIME  (GMT) 

Figure 9-4.  Example of the final  plume he.ight after plume rise for hourly releases from a MEPSE 
300 m stack height. 


:uoo 

2200 

2000 

1900 

1600 

'E' - 1400 

~  1200 
CJ 
iii 
::c: 

1000 

900 

600 

400 

200 

0 

r 
0 

•  hT 
+hb 
+z1 

+-ff---T--'f'\ ----· 

j/ 

,, 

1: 

~ 

\ 
\ 

............... 
I 
u 

I 
19 

I 
1.6 

I 

I 

I 

20 

I 
/ 
·-·-·-·-----~/ 

~-·-·_.,..... /+ 

/+-+--... 

_.__.-r 

/+ 

2 

4 

6 

9 

10 

12 

TIME  (LST) 

Figure 9-5.  Time variation of the plume bottom (hb) and top (hT) heights and the PBL height 

(Zi) for a nocturnal release. 


•Wp 

_,.,..... 

/ I I 

./ 

./ 

./ 

./ 

/ 

/ 
.,,,.,,,·-·-·_.,.,.·-· 

.............. ......-· 

2 

4 

6 

B 

10 

12 

14 

16 

18 

20 

TIME  (l.ST) 

150000 

140000 

130000 

120000 

110000 

90000 

80000 

s 100000 
i!: 
0 
j 
UJ :e 
::::> 
-' Q. 

70000 

60000 

50000 

40000 

30000 

20000 

10000 

0 

I 
0 

Figure 9-6.  Time variation of plume width (Wp) from  a nocturnal plume release.  A plume width 
of about 30 km is achieved during the morning period about  10 hours after release. 

_);:::(cid:173)
\'"'-

I______ 

~ 
•j-••t '"'"'j-•"' 

j  .......... ., 

I 

--~-;----~----~ 
; 

Plume j L 
r- at (t +dt)  b 

cross-section 

I 
~ attimet 

u 
T 

D 

~Entrainment  N 
· 

from.skies 

~·-- -~·--: 

~ I 

' 

~ 

' ' ' ' 
' ~~,.. 

' 

.... ~?> 
Dilfusi:>n 
I[  I ~ 

igure 9-7. An illustration of the PinG module formulation depicting the relevant processes; 
a) time-height view and b) cross-sectional view. 

Emissions 

(:· .. ;li~~i,., 

::·,;  g¢1£i.O:'-
:fi~~~f)/:: ~:~~):~ti~~iif~ 

i: .. i.l ... 11~1i.l1;;: 

MQ~rolor;y 

I 

I 

::  ,.,,,,,,,.·f:;:;t:: 

PDM 

Plume Dynamics 

igure 9-8.  Flow diagram of the PDM processor and PinG module with.associated programs in 
e Models-3  CMAQ system. 


~ 
~~ 
°' 0 
~ 
'° 
~ (,,) 

0 

-

--

PinG  MODEL RESULTS  FOR  OPP  MEPSE 

NASl-MLI.E DOMAIN  (DAY =1995188) 

- - - -

70 

60 

13' 
g 50 
z 
0  40 
F: 
~  30 
I~  20 

0 
0 

10 

\0 
I 
.j::.. 
0 

I 

--------·------------------.... 

--

.._........_ 
_,..a------n...___ 
..... ..&----...., 

Ji' 

-----

~  ~-.-... 

~  ----

--,,. 

.... --.. -

--

__.--· 

.,,.,..~ .. -

,,.,., ....... ~ 
l!. 

.-·&----~ 
.. _ 
<>~....... 

~ 

-

-

..& .... -

~--- __ .,,,, .. -.. ' 
~-1 

\ --~/fl 

\  ~ 

/ 

<!"f 
I 

0  I  I 
-20 

I 

I 

I  I  I 
-15 

I 

I 

I 

I 

I  I  I 
-10 

I  I  I 
-5 

I 

I 

I  I  I 
0 

I 

I 

I  I  I 

I 

I 

5 

I  I  I 
10 

I 

I 

I  I  I 
15 

I 

I 

I  I 
20 

DISTANCE ACROSS  PLUME  (l<M) 

ooo OZONE  IN  Pt.J.M:  14 FROM  CIM_PING  C84 

14 

Fl.E•BNA-

'Figure 9-9 Plume ozone concentrations in an expanding cross-section in the subgrid scale plume 
phase at various times (o - 14:00, * - 14:30, 0 - 15:30,  o - 15:30, • - 17:30 ).  Plume section was 
released at 13:00.  Symbol at each edge is the grid boundary concentration. 
