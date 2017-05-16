Chapter 11 
===========

CLOUD DYNAMICS AND CHEMISTRY 

Shawn J. Roselle* and Francis S. Binkowski** 

Atmospheric Modeling Division 

National Exposure Research Laboratory 
U.S. Envirorunental Protection Agency 

Research Triangle Park, North Carolina 27711 

ABSTRACT 

Chapter 11  describes the cloud module that is currently incorporated into CMAQ.  This module 
simulates the physical and chemical processes of clouds that are important in air quality 
simulations.  Clouds affect pollutant concentrations by vertical-convective mixing, scavenging, 
aqueous chemistry, and removal by wet deposition.  The CMAQ cloud module includes 
parameterizations for sub-grid convective precipitating and non-precipitating clouds and grid(cid:173)
scale resolved clouds.  Cloud effects on both gas-phase species and aerosols are simulated by the 
cloud module. 

"on assignment from the National Oceanic and Atmospheric Administration, U.S. Department of Commerce. 
Corresponding author address:  Shawn J.  Roselle, MD-80, Research Triangle Park, NC 27711.  E-mail: 
sjr@hpcc.epa.gov 

.. On assignment from the National Oceanic and Atmospheric Administration, U.S. Department of Commerce. 

11.0  CLOUD DYNAMICS AND CHEMISTRY 

11.1  Background 

Clouds play an important role in boundary layer meteorology and air quality.  Convective clouds 
transport pollutants vertically, allowing an exchange of air between the boundary layer and the 
fr~e troposphere.  Cloud droplets formed by heterogeneous nucleation on aerosols grow into rain 
droplets through condensation, collision, and coalescence.  Clouds and precipitation scavenge 
pollutants from the air.  Once inside the cloud or rain water, some compounds dissociate into 
ions and/or react with one another through aqueous chemistry (i.e., cloud chemistry is an 
important process in the oxidation of sulfur dioxide to sulfate).  Another important role for 
clouds is the removal of pollutants trapped in rain water and its deposition onto the ground. 
Clouds can also affect gas-phase chemistry by attenuating solar radiation below the cloud base 
~hich has a si"gnifl~ant impact on the photolysis reactions. 

The Models-3/CMAQ cloud model incorporates many of these cloud processes.  The model 
includes parameterizations for several types of clouds, including sub-grid convective clouds 
(precipitating and non-precipitating) and grid-scale resolved clouds.  It includes an aqueous 
chemistry model for sulfur, and includes a simple mechanism for scavenging. 

,,,,,, 

,,1111 

,,. 

,, 

1111, 

''II, 

11.2  Model Description 

The cloud model can be divided into two main components, including the sub-grid cloud model 
(subcld)  and the resolved cloud model (rescld).  For large horizontal grid resolutions, the grid 
size will be Ia[ger than the size of a typical convective cloud, requiring a parameterization for 
t4~se sub-grici'clouds.  The sub-grid cloud scheme simulates convective precipitating and non(cid:173)
p~ecipitating Clouds.  The second component of the cloud model considers clouds which occupy 
the entire grid cell and have been "resolved" by the meteorological model.  The rate of change in 
pollutant concentrations ( m; ) due to cloud processes is given by: 

am; 
at  cld 

am; 
at 

subcld 

am; 
+-(cid:173)at 

rescld 

(11-1) 

'J]le terms 0.11 ... the right-hand side of Equation 11-1  are solved separately at different times.  The 
influence of sub-grid clouds are instituted once an hour while the resolved clouds impact 
concentrations every synchronization timestep.  Each subcomponent of the cloud model is . 
described in detail below. 

11-2 

11.2.1  Subgrid Convective Cloud Scheme 

om; 
Ot 

subc/d 

- f(mixing,scav,aqchem, wetdep) 

(11-2) 

The current sub-grid cloud scheme in CMAQ was derived from the diagnostic cloud model in 
RADM version 2.6 (Dennis et al.,  1993; Walcek and Taylor,  1986; Chang, et al.,  1987; Chang, et 
al.,  1990).  Seaman (1998) noted that most convective parameterizations are based on the 
assumption that "the area of an updraft is small compared to the area of the grid cell" and most 
parameterizations can be used at grid resolutions a small as  12 km (Wang and Seaman,  1997).  In 
CMAQ, subgrid clouds are considered only for horizontal grid resolutions on the order of 12 km 
or more.  Seaman (1998) also pointed to a study by Weisman et al.  (1997) that showed that 
explicit cloud models can resolve convection for resolution finer than 5 km.  Within CMAQ, for 
resolutions of 4 km or less, vertical convection is assumed to be resolved at the grid level; 
therefore, the resolved cloud model will be the only cloud scheme used at small grid scales. 

The effects of sub-grid clouds on grid-averaged concentrations are parameterized by modeling 
the mixing, scavenging, aqueous chemistry, and wet deposition of a "representative cloud" 
within the grid cell.  For all sub-grid clouds, a  I-hour lifetime (rc1d)  has been assumed.  Sub-grid 
clouds can be either precipitating or non-precipitating, and the non-precipitating subgrid clouds 
are further categorized as pure fair weather (PFW) clouds and non-precipitating clouds 
coexisting (CNP) with precipitating clouds. 

The subgrid cloud model determines if precipitating or nonprecipitating clouds exist over each 
grid cell.  Precipitating clouds are simulated when the meteorological preprocessor (currently the 
Mesoscale Model version 5 or  MM5, Grell et al.,  1994) indicates precipitation from its 
convective cloud model.  The CMAQ implementation differs from the RADM in that only the 
convective precipitation amounts from MM5 are used to drive the subgrid precipitating cloud. 
RADM used the total precipitation (convective and nonconvective precipitation) to drive the 
subgrid cloud model.  In CMAQ, the nonconvective precipitation is used in the resolved cloud 
model.  Nonprecipitating clouds are modeled if the moisture and temperature profiles support the 
development of a cloud (Dennis et al.,  1993).  Nonprecipitating clouds are modeled only when 
the relative humidity of the source level is above 70% and the calculated cloud base is below 
1500 m for PFW clouds or 3000 m for CNP clouds.  For both precipitating and nonprecipitating 
cloud types, the geometry of the cloud (base, top, and spatial extent) are determined next.  The 
cloud base is calculated by lifting a parcel of air from the cloud source level (the level between 
the surface and 650 mb with the highest equivalent potential temperature) to the lifting 
condensation level (LCL).  The cloud top calculation depends upon the cloud type and 
atmospheric stability.  For precipitating clouds in unstable conditions, the cloud top is found by 
following the moist adiabatic lapse rate from the cloud base up to the level where it becomes 
cooler than the surrounding environment.  For precipitating clouds under stable conditions, the 
cloud top is set as the first layer above the cloud base in which the relative humidity falls below 
65%, but limited to less than the 600 mb height.  For nonprecipitating clouds, further restrictions 
are placed on the cloud top.  The cloud top calculation for nonprecipitating clouds uses the same 
relative humidity criterion as the precipitating clouds, but the cloud top is allowed to extend up to 
500 mb for CNP clouds and only to  1500 m for PFW clouds.  If the atmosphere is unstable, the 
nonprecipitating cloud top may be reduced in height if the parcel method calculated a lower 
cloud top.  The fractional cloud coverage calculations depend on cloud type and have been 
described thoroughly by Dermis et al.  (1993).  For precipitating clouds, the model uses a 
parameterization similar to approach of Kuo (1974).  The fractional coverage parameterization 
for the nonprecipitating clouds is based on relative humidity. 

The convective cloud simulated by the sub-grid cloud model is considered to be composed of air 
transported vertically-from below the cloud, entrained from above the cloud (for precipitating 
clouds), and entrained from the sides of the cloud.  Concentrations of pollutants for each layer of 
th~ cloud are calculated by: 

-~ 
11'll 

(z) =  fent  (1 - fside )nu 

-~~ 

+  fside m; (z)  +  (1 - fent )m; 

-~ 

(11-3) 

[ 

-

J 

where f:ntle  is the fraction of entraining air originating from the side of the cloud.  For 
nonprecipitating clouds, no entrainment of air from above the cloud is allowed and therefore 
J;~=l.  The entraiii.ment,.feni• is calculated by iteratively solving conservation and 
thermodynamic equations (Dermis et al.,  1993; Chang et al,  1990,1987; and Walcek and Taylor, 

Ill'",, 

111111 

" 

~.  -·~ 

N 

represent the above and below cloud concentrations, 

1986).  The terms  m;  and  m; 
respectively.  Once the cloud volume has been determined, vertically-averaged cloud 
temperature, pressure, liquid water content, total water content, and pollutant concentrations, are 
computed with liquid water content (We)  as the weighting function (gives the most weight to the 
layers with the highest liquid water content).  Thus, the average pollutant concentrations within 
the cloud are calculated by: 

z<lDp J ;~Id (z)W,,(z)dz 

m:'d  = -'z'=""'~' - - - - (cid:173)
s~(z)dz 

Z~1op 

(11-4) 

With the averages over the cloud volume, the processes of scavenging, aqueous chemistry, and 
wet deposition are considered.  The final step in cloud mixing is the reapportioning of mass back 
into individual layers.  This is accomplished using cloud fractional coverage, initial in-cloud 
concentrations, final in-cloud concentration, and the initial vertical concentration profile.  For 
precipitating clouds, the average pollutant concentration for the grid cell within the cloud layers 
is computed by: 

(11-5) 

where cfrac is the fractional cloud coverage. There are variations on this equation for below 
cloud, above cloud, and for nonprecipitating clouds. 

11.2.1.1 

Scavenging and Wet Deposition 

Pollutant scavenging is calculated by two methods, depending upon whether the pollutant 
participates in the cloud water chemistry and on the liquid water content.  (1) For those pollutants 
that are absorbed into the cloud water and participate in the cloud chemistry (and provided that 
the liquid water content is> 0.01  g/m3), the amount of scavenging depends on Henry's law 
constants, dissociation constants, and cloud water pH.  (2) For pollutants which do not participate 
in aqueous chemistry (or for all water-soluble pollutants when the liquid water content is below 
0.01  g/m3), the model uses the Henry's Law equilibrium equation to calculate ending 
concentration and deposition amounts.  The rate of change for in-cloud concentrations (m/1
each pollutant (i) following the cloud timescale ('tcic0  is given by: 

") for 

-cld 
om; 

-cld( e-a,rcld  - lJ 

=m; 

'c1d 

scav 

(11-6) 

where Cl.;  is the scavenging coefficient for the pollutant.  For subgrid convective clouds, 'tcld is  1 
hour and for grid resolved clouds it is equal to the CMAQ's synchronization timestep.  For gases, 
the scavenging coefficient is given by: 

I 

(11-7) 

where H;  is the Henry's Law coefficient for the pollutant, TWF is the total water fraction given 
by: 

TWF=  PH20 

WTRT. 

11-5 

(11-8) 


11111111 

wtere PH2o  is the density of water,  w T  is the mean total water content (kg/m3
uipversal gas consfan. t, and Tis the in-cloud air temperature (K).  The WaShout tiine, ,;washout 
represents the amount of time required to remove all of the water from the cloud volume at the 
specified precipitation rate (Pr), 
and is given by: 

), R is the 

""" 

'"" 

(11-9) 

H~re, t:J.Zc:ra is the cloud thickness. 

1111111111

1
' 

The accumulation mode and coarse mode aerosols are assumed to be completely absorbed by the 
cloud and rain water.  Therefore, the scavenging coefficients for these two aerosol modes are 
simply a function of the washout time: 

1 
a,=--(cid:173)

"washou1 

(11-10) 

lh" 

The Aitken mode aerosols are treated as interstitial aerosol and are slowly absorbed into the 
cloud/rain water.  This process is discussed in detail in the aerosol chapter (Chapter 10). 

" 

' 

' 

' 

The wet deposition algorithms in CMAQ were taken from the RADM (Chang et al.,  1987).  In 
the current implementation, deposition is accumulated over 1-hour increments before being 
written to the output file.  The wet deposition amount of chemical species i (wdep,.)  depends upon 
th~ precipitation rate (Pr) and the cloud water concentration (mt11: 

11111 

·: 

:I" 

,,, 

wdep; =  f m;  P,.dt 

'rc1J-cld 

0 

(11-11) 

Deposition amounts are accumulated for each of the modeled species, but the user specified 
which species are written to the output file.  This is handled in the Program Control Processor 
(see Chapter fs).  ,,, 

11.2.1.2 

Aqueous Chemistry 

The aqueous chemistry model evolved from the original RADM inodel (Chang et al.,  1987; and 
\Va.leek and Taylor, 1986).  The model considers the absorption of chemical compounds into the 
c~.oud water; the amount that gas-phase species absorb into the cloud water depends on 
· 
thermodyrianlic equilibrium, while accumulation-mode aerosols are considered to have been the 
nucleation particles for cloud droplet forma,tion and are 100% absorbed into the cloud water. 
Then the model calculates the dissociation of compounds into ions, oxidation of S(IV) to S(VI), 
and wet deposition.  The species that participate in the aqueous chemistry are given in Table 11-

1"'1111· 

" 

111 

'II 

1.  This version of the aqueous chemistry model differs from  Walcek's scheme in that it tracks 
contributions from gases and aerosols separately.  It also considers the scavenging of interstitial 
aerosols, and it allows.for variable-length cloud time scales. 

· 

Table 11-1.  List of Species Considered in the Aqueous Chemistry Model 

EPA/600/R-99/030 

Gases 

S02 
HN03 

N10s 
C02 
NH3 
H202 
03 
formic acid 

methyl hydrogen peroxide 

peroxy acetic acid 

H2S04 

11.2.2  Resolved Cloud Scheme 

Aerosols 

S04 =  (Aitken &  accumulation) 
NH4 + (Aitken &  accumulation) 
NQ3- (Aitken, accumulation, &  coarse) 
Organics (Aitken &  accumulation) 

Primary (Aitken, accumulation, &  coarse) 

CaC03 
MgC03 
NaCl 

Fe3+ 

Mn2+ 

KCI 

Number (Aitken, accumulation, &  coarse) 

At any grid resolution, clouds may be resolved by the MM5, which could include stratus, 
cumulus, or cirrus type clouds.  The resolved clouds have been simulated by the MM5 to cover. 
the entire grid cell.  No additional cloud dynamics are considered in CMAQ for this cloud type 
since any convection and/or mixing would have been resolved and considered in the vertical 
wind fields provided by MM5.  A resolved cloud horizontally covers the entire grid cell and 

-cld 

. 

. 

and  m;  are equivalent in resolved 
vertically extends over the whole depth of the layer, thus  m; 
clouds.  These clouds are activated in MM5 when the humidity is high enough for water vapor to 
condense, and then MM5 computes cloud and tain water amounts according to any of several 
microphysical submodels.  Using the total of the condensed cloud water and rain water reported. 
. : 
by Ml\45, the CMAQ resolved cloud model then considers scavenging, aqueous chemistry, 'and 
wet deposition.  The average liquid water content  W c  in a model layer (z) for the resolved cloud 
is given by: 

. .  .  . 

.  ' 

.. 

.  . 

. 

. 

(11-12) 

where Qc(z) is the cloud water mixing ratio (kg/kg), Qn(z)  is the rain water mixing ratio (kg/kg), 
p(z)  is the air density (kg/m3).  Precipitation amounts for resolved cloud layers, P,(z), are derived 
using the MMS non-convective precipitation amounts (Rn),  apportioned into individual model 
layers with the vertical profile of condensed liquid water as follows: 

-

~(z)-Rn 

[  w:;(z)  ] 
() 
I~ z  dz 

(11:-13) 

Once quantities for precipitation rate, liquid water content, etc. have been calculated, then the 
scavenging, aqueous chemistry, and wet deposition are solved using the same procedures as in 
the subgrid clouds. 

Omi  ~  t3mi 
iJt 

rescld  = aLcav +a aqchem 

(11-14) 

Several assumptions have been made in the current implementation of the resolved cloud model. 
(1)  The lifetime of the resolved cloud computations varies based on the synchronization timestep 
0<1
,,(;MAQ.  (2~:,Fo!,!owing the method of operator splitting, the effect ~f the resolved clouds on 

pollutant concentrations occurs at the end of the cloud lifetime, thus no exchange between layers 
is permitted dming the cloud life-cycle.  (3) The pollutants, cloud water, and rain water are 
uniformly distributed within the grid cell.  Because of the separation of MM5 from CMAQ, we 
do not have the information to do precipitation fluxes.  Even if a complete cloud precipitation 
model was developed within CMAQ, there is no guarantee that it would be consistent with what 
was done in MMS. 

11.3  Conclusions 

One of the concepts for Models-3 was that multiple modules may exist for each physical process 
of the air quality model.  The implementation described here is the first module available for 
modeling cloud physics and chemistry.  Other subgrid cloud models (i.e., the Kain-Fritsch (1990, 
1993) and Betts-Miller (1986)) are under consideration and may be included as optional modules 
for CMAQ.  In addition, a more detailed resolved cloud model is under development which will 
include a microphysical submodel for following the evolution of the cloud (i.e., cloud droplet 
formation, growth ofrain droplets, and descent through model layers to the ground).  It will also 
cqpsider resoly~d~~oud lifetimes which extend beyond the C:MAQ synchronization timestep, 
thgs majntaiajpg the partition between gas and aqueous-phase pollutants during the gas-phase 
ch~mistry calculations.  The current implementation of the cloud model in CMAQ will be 
evaluated using available datasets and will be used as a reference for evaluating future cloud 
modules for CMAQ. 

11.4  References 

Betts, A.K. and M.J. Miller,  1986.  A new convective adjustment scheme.  Part II:  Single column 
tests using GA TE wave, BO MEX, A TEX, and arctic air- mass data sets.  Quarterly J.  Roy. 
Meteor.  Soc., 112, 693-709. 

Chang, J.S., R.A. Brost, LS.A. Isaksen, S.  Madronich, P.  Middleton, W.R.  Stockwell, and C.J. 
Walcek,  1987.  A three-dimensional Eulerian acid deposition model: Physical concepts and 
formation. J.  Geophys.  Res., 92,  14681-14700. 

Chang, J.S., P.B. Middleton, W.R. Stockwell, C.J. Walcek, J.E. Pleim, H.H. Lansford, F.S. 
Binkowski, S. Madronich, N.L.  Seaman, D.R.  Stauffer, D.  Byun, J.N. McHenry, P.J.  Samson, H. 
Hass.,  1990.  The regional acid deposition model and engineering model, Acidic Deposition: 
State of Science and Technology,  Report 4, National Acid Precipitation Assessment Program. 

Dennis, R.L., J.N. McHenry, W.R. Barchet, F.S. Binkowski, and D.W. Byun, 1993.  Correcting 
RAD M's sulfate underprediction: Discovery and correction of model errors and testing the 
corrections through comparisons against field data, Atmos.  Environ., 26A(6), 975-997. 

Grell, G.A., J.  Dudhia, and D.R. Stauffer,  1994.  A description of the fifth generation Penn 
State/NCAR mesoscale model (MM5).  NCAR Tech. Note NCAR/TN-398+STR,  138 pp. 

Kain, J.S. and J.M. Fritsch,  1990. A one-dimensional entraining/detraining plume model and its 
application in convective parameterization.  J.  Atmos. Sci, 47, 2784-2802. 

Kain, J.S. and J.M. Fritsch,  1993.  Convective parameterization for mesoscale models: The Kain(cid:173)
Fritsch scheme.  The  Representation of Cumulus Convection in Numerical Models,  Meteor. 
Monogr., 46, Amer. Meteor. Soc.,  165-170. 

Kuo, H.L.,  1974.  Further studies of the parameterization of the influence of cumulus convection 
on large-scale flow, J.  Atmos.  Sci., 31,  1232-1240. 

Seaman, N.L.,  1988. Meteorological Modeling for Air-Quality Assessments: A NARSTO 
Review Paper, Submitted to Atmos.  Environ. 

Walcek, C.J. and G.R. Taylor,  1986.  A theoretical method for computing vertical distributions 
of acidity and sulfate production within cumulus clouds, J.  Atmos.  Sci 43, 339-355. 

Wang, W.  and N.L.  Seaman,  1997.  A comparison study of convective par~eterization schemes 
in a mesoscale model. Mon.  Wea.  Rev., 125, 252-278. 

Weisman, L.M., W.C. Skamarock and J.B. Klemp,  1997.  The resolution dependence of 
explicitly modeled convective systems, Mon.  Wea.  Rev., 125, 527-548. 
