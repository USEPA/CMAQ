<!-- BEGIN COMMENT --> 

[<< Previous Chapter](CMAQ_Science_Ch_04.md) - [Home](README.md) - [Next Chapter >>](CMAQ_Science_Ch_06.md)

<!-- END COMMENT -->

Chapter 5
================

FUNDAMENTALS OF ONE-ATMOSPHERE DYNAMICS 

FOR MULTISCALE AIR QUALITY MODELING 

Daewon W. Byun

Atmospheric Modeling Division 

National Exposure Research Laboratory 
U.S. Envitonmental Protection Agency 

Research Triangle Park, NC 27711 

ABSTRACT 

This chapter provides essential information needed for the proper use of meteorological data in 
air quality modeling systems.  Sources of meteorological data are diverse and many difficulties 
can arise while linking these with air quality models.  To provide an integral view of atmospheric 
modeling, a robust and fully compressible governing set of equations for the atmosphere is 
introduced.  Limitations of several simplifying assumptions on atmospheric dynamics are 
presented.  Also, concepts of on-line and off-line coupling of meteorological and air quality 
models are discussed . 

. When the input meteorological data are recast with the proposed set of governing equations, 
chemical transport models can follow the dynamic and thermodynamic descriptions of the 
meteorological data closely.  In addition, this chapter introduces a procedure to conserve mixing 
ratio of trace species even in the case meteorological data. are not mass consistent.  In summary, 
it attempts to bridge the information gap between dynamic meteorologists and air quality 
modelers by highlighting the implication of using different meteorological coordinates and 
dynamic assumptions for air quality simulations. 

·On assignment from the National Oceanic and Atmospheric Administration, U.S. Department of Commerce. 
Corresponding author address: Daewon W. Byun, MD-80, Research Triangle Park, NC 27711. E-mail: 
bdx@hpcc.epa.gov 

FUNDAMENTALS FOR ONE-ATMOSPHERE MODELING I~OR MULTISCALE 

5.0 
AIR QUALITY MODELING 

To simulate weather and air quality phenomena realistically, adaptation of a one-atmosphere 
perspective based mainly on "first principles" description of the atmospheric system (Dennis, 
1998) is necessary.  The perspective emphasizes that the influence of interactions at different 
dynamic scales and among multi-pollutants cannot be ignored.  For example, descriptions of 
processes critical to producing oxidants, acid and nutrient depositions, and fine particles are too 
closely related to treat separately.  Proper modeling of these air pollutants requires that the broad 
range of temporal and spatial scales of multi-pollutant interactions be considered simultaneously. 
Several chapters (Chapters 4, 8, 9, 11and16) of this document present the one-atmosphere 
modeling perspective related with the multi-pollutant chemical interactions.  Another key aspect 
of the one-atmosphere perspective is the dynamic description of the atmosphere.  This is the 
focus of the present chapter. 

Air quality modeling should be viewed as an integral part of atmospheric modeling and the 
governing equations and computational algorithms should be consistent and compatible. 
Previously, many atmospheric models have been built with limited atmospheric dynamics 
assumptions.  To simplify the model development process, the governing equations were first 
simplified to match with the target problems, then computer codes were implemented.  This 
approach enabled rapid development of models.  However, we believe that dynamic assumptions 
and choice of coordinates should not precede the computational structure of the modeling 
system.  To provide the scalability in describing dynamics, a fully compressible governing set of 
equations in a generalized coordinate system is preferable.  Once the system is based on the fully 
compressible governing equations, simpler models can be built readily.  The characteristics of 
the vertical coordinates and other simplifying assumptions need to be considered as well.  For 
successful one-atmosphere simulations, it is imperative to have consistent algorithmic linkage 
between meteorological and chemical transport models (CTMs). 

The present chapter addresses the issue of consistent description of physical processes across 
scales in meteorological and air quality modeling systems.  It intends to provide appropriate 
background information to properly link air quality and meteorological models at a fundamental 
level.  It deals with dynamic scalability issues, such as hydrostatic and nonhydrostatic modeling 
covering wide range of both temporal and spatial scales.  Some of the contents are extracts from 
Byun (1999a and b) and others are complementary information to them.  It includes mass 
correction methods, mass conservative temporal interpolation method, and the coupling 
paradigm for meteorology and chemical transport models. 

## 5.1 Governing Equations and Approximations for the Atmosphere 

In most weather prediction models, temperature and pressure, as well as moisture variables, are 
used to represent thermodynamics of the system.  Often these thermodynamic parameters are 
represented with the advective form equations in meteorological models.  Most of tin1e, the 
density is diagnosed as a byproduct of the simulation, usually through the ideal gas law.  For 
multiscale air quality applications where the strict mass conservation is required, prognostic 
equations for the thermodynamic variables are preferably expressed in a conservative form 
similar to the continuity equation.  Recently, Ooyama (1990) has proposed the use of prognostic 
equations for entropy and air density in atmospheric simUtlations by highlighting the 
thermodynamic nature of pressure.  Entropy is a well-defined state function of the 
thermodynamic variables such as pressure, temperature, and density.  Therefore, entropy is a 
field variable that depends only on the state of the fluid.  The principle he uses is the separation 
of dynamic and thermodynamic parameters into their primary roles.  An inevitable interaction 
between dynamics and thermodynamics occurs in the fonn of the pressure gradient force. 

In this section, a set of governing equations for fully comipressible atmosphere is presented. 
Here, density and entropy are used as the primary thermodynamic variables.  For simplicity, a 
dry adiabatic atmosphere is considered.  Most of the discussions in this section should be 
extensible for moist atmosphere if Ooyama's approach is followed. 

### 5.1.1  Governing Equations in a Generalized CurviliEtear Coordinate System 

Using tensor notation, the governing set of equations for the dry atmosphere in a generalized 
curvilinear coordinate system can be written as: 

(5-1) 

(5-2) 

(5-3) 

(5-4) 

where vi  and v k  are contra variant and covariant wind components, respectively,  v;{  represents 
the covariant derivative of contravariant vector,  £ikl is the Levi-Cevita symbol,  Qk is the angular 
velocity of earth's rotation,  fr!  represents frictional forcing terms, .fY is the Jacobian of 
coordinate transformation,  cI>  is geopotential height, p is air density, and  gik  represents 
components of gravity vector in tensor form.  Refer to Appendix 5A for the tensor primer and the 
derivation of the continuity equation in a generalized curvilinear coordinate system.  <fJ; 
represents trace species concentration, ands is (dry air) entropy per unit volume (entropy 
density), given as 

s = pCwi In( I._) - pRd In( _E__) 
Poo 

T,,o 

(5-5) 

where T  is temperature,  T,,0  and p 00  are temperature and density of the reforence atmosphere, 
respectively, at pressure  p 00  = 1000 mb = 105 Pascal,  Cvd  is the specific he:at capacity  at 
constant volume, and Rd is the gas constant for dry air.  The Q-terms represent sources and sinks 
of each conservative property.  Although the source term for air density ( QP ) should be zero in 
an ideal case, it is retained here to capture the possible density error originating from numerical 
procedures in meteorological models.  It is important to linderstand how this error term 
influences computations of other parameters such as vertical velocity component.  Effects of the 
error term on trace gas simulation are discussed later. 

To close the system we need to utilize the ideal gas law and the thermodynamic relations for 
temperature, entropy, pressure gradients, and density.  Here, atmospheric pressure is treated as a 
thermodynamic variable that is fully defined by the density and entropy of the atmosphere. 
Then, pressure gradient terms can be computed using the thermodynamic relations with the 
density and entropy (e.g., Batchelor, 1967; Ooyama, 1990; DeMaria, 1995) in terms of the 
general vertical coordinate  s = .£3

,  as: 

p= pRJT 

V,p=(t )v,p+(~JVJ 
( ap J ap  ( ap J as 
ap 
as  =  ap  as  +  a,  as 
()p =(c  _fJRdT 
pd  p  cwl 
ap 

(5-6a) 

(5-6b) 

(5-6c) 

(5-6d) 

(5-6e) 

where  Cpd  is the specific heat capacity at constant pressure for dry air, andl 
v $  = ia I ax1 l..=const  + ja I ax2L=const.  For a conformal map projection, we can relate generalized 
meteorological curvilinear coordinates  (.£1 ,.£2 ,.£3 J) to the reference rotated earth-tangential 
coordinates  (x,y,z,t) as 

5-4 

x1 =mx 
x2 =my 
x3  =s 
t=t 

x=m-1x1 
y=m-1.x2 

h( "I  "2  "3  ")  h 

"I  "2) 
x  ,x  ,x ,t  =  AGL  x  ,x  ,x ,t  +z.ifc(x  ,x 

("I  "2  ,..3  ") 

z = 
t=t 

BP.A/600/R.-991030 

(5-7a, b) 

where mis the map scale factor,  z.ifc  is the topographic height, and his the geometric height, and 
hAGL represents height above the ground (AGL).  In the derivation of Equations 5-7a,b, we 
neglected the first-order variations of the map scale factor in x- and y-directions.  The 
approximation establishes a quasi-orthogonality of the vertical coordinate to the horizontal plane 
on the confomal map.  The covariant metric tensor, for example, and its determinant are given as 

(5-7c) 

(5-7d) 

With above relations, one can rewrite the governing momentum equation, Equation 5-1,into the 
horizontal and vertical components of the curvilinear cooirdinates as (Byun, 1999a): 

av 
_ s  +(V  •V )V +v3
at  SS S  

A .  

A 

av 
- s  + fi xV 
as 

3 

S 

A 

" 
+m  (-V p+V <P)-m  - (--+-)V h=F 
. 
s 

2(as)  1 tip 
dz  pas 

2  1 
p 

a<P 
as 

s 

s 

s 

-m  -(-V p+V  <P)•V  h+ 

s 

s 

2as  1 
()z  p 

~  12lttp 
pas 

" 
s  (--+-)= R 
z 
3 

a<P 
as 

s 

(5-8) 

(5-9) 

where V:,  = v1i  + 112 j, <P(x 1
forcing vector,  i 3  is the vertical tangential basis vector andfi;  is the forcing term in the 
momenhun equation for  x3  direction. 

, x 3 ,i) = gz represents the geopotential height,  :Fs  is the horizontal 

, .x2

An alternative equation for the Cartesian vertical velocity component is given as: 

d(plsw) 

dt  +m  vs 

2u  • (pf5wV4 ) 

d(pJ5wv3

) 

m 

+ 

as 
wQP J 
( 
3  p 
+pl  - -+ - - =pl  R + -

m ()p  d<PJ(as) 
spas 
dz 

as 

(

s 

(5-9') 

where Vz: = Vi + Vj = (v1 I m)i + (v2 
Cartesian coordinate system, w is the vertical velocity component,  J_.  is the Jacobian for vertical 

/  m)j is the horizontal wind vector represented in the 

coordinate transformation (ls= l~~I =  ~ 1::1 =  m 2 .fY ), and F;  is forcing term for thew(cid:173)

component.  Note that the contravariant vertical velocity component is related to the Cartesian 
vertical velocity as: 

... 3 
v  =-=-+V •V s+w  - =-+(--V •V <P+w)  -

(5-10) 

as 
dt 

ds 
dt 
' 

z; 

z; 

(as)  as 
dz 
dt 

1 ... 
s 
g 

s 

. 

(as) 
dz 

, 

where v z; =ia I ax1z;=C01L<t  + ja I dy\'z;=const. 

The conservation equations for air density, entropy density, and tracer concentrations are found 
to be: 

d(pls) 

dt  + m 

2v  • (pJ.,V:, J d(pJ.,v
ds 

m2  + 

" 

3

)  = 1 Q 

s  P 

(5-11) 

(5-12) 

(5-13) 

### 5.1.2  Assumptions of Atmospheric Dynamics 

In this subsection, several popular assumptions used in meteorological models are reviewed. 
Here, the dynamic and thermodynamic assumptions are discussed separately because they have 
been applied as independent approximations in many atmospheric models.  However, readers 
should be aware of the inseparable nature of the dynamics and thermodynamics of the 
atmosphere.  This study focuses on the impact of basic assumptions of the mass conservation 
issues and limits ()f applications in air quality applications. 

#### 5.1.2.1 Boussinesq Approximation 

The crux of the Boussinesq approximation is that variation in density is important only when it is 
combined as a factor with the acceleration of gravity. Originally, it was applied for studying 
shallow convection or boundary layer dynamics.  Descriptions of the Boussinesq approximation 
can be found in the literature (e.g., Arya, 1988; Pielke, 1984; Stull, 1988; and Thunis and 
Bornstein, 1996).  Although the Boussinesq approximation was originally developed for 
incompressible fluid, Dutton and Fichtl (1969) expanded the concept for anelastic deep 
convection applications.  The results of the approximation lead to the following simplifici:itions 
of the equations of motions in the planetary boundary layer (PBL): 

(1) 

(2) 

(3) 

Flows can be treated essentially as solenoidal either in velocity field (incompressible) or 
in momentum field (anelastic). 

The equation of state for the fluctuating component is simplified because the ratio of 
fluctuating density to total density can be approximated by the ratio of temperature 
fluctuation to the reference temperature. 

Molecular properties including diffusivity are constant.  These approximations are often 
used in air quality modeling to simplify the equations of motions and trace gas 
conservation equations.  The effect of the Boussinesq approximation on mass continuity 
is in the limitation of the flow characteristics, such as incompressible or anelastic.  For 
multiscale atmospheric studies, this approximation may be used only in the 
parameterization of the surface fluxes where the density can be treated essentially 
independent of height. 

#### 5.1.2.2 Nondivergent Flow Field Assumption 

Essentially, this is an assumption about flow characteristics.  The basis of this assumption is 
purely dynamic although an incompressible assumption leads to the nondivergent flow 
approximation.  A priori, there is no connection with atmospheric thermodynamics.  Therefore 
this assumption does not provide any information about the state variables, such as density, 
temperature, and pressure fields.  For atmospheric applications, this approximation should be 
viewed as a result of the incompressible atmosphere assumption linked through the continuity 
equation of air.  Because of the characteristics that the nondivergent velocity field can be 
expressed as the curl of a vector stream function, the field is also called solenoidal.  Implications 
of this assumption on mass conservation of trace species are presented below in the description 
of the incompressible atmosphere assumption.  In the generalized coordinate system, the 
nondivergent flow field is represented with following equation 

This is somewhat different from the meteorological nondivergent flow field assumption in the 
Cartesian coordinate system,  V • V = 0.  In the generalized meteorological coordinate system, 
Equation 14 can be rewritten as 

(5-14)  . 

(5-14') 

The two additional terms represent essentially the effects of the map projection and the gradient 
of the vertical Jacobian on the divergence of wind.  For a small domain and for a coordinate 
whose vertical Jacobian is constant with respect to height (e.g.,  <J',-coordinate), Equation 14' 
becomes identical to the nondivergent wind flow assumption used in a meteorological model. 
When the vertical Jacobian is a function of air density, the dependency of the wind on the density 
distribution cannot be ignored. 

#### 5.1.2.3 Incompressible Atmosphere Assumption 

This is an assumption about the thermodynamic characteristics of air.  The equation of state 
describes how density is affected by the changes in pressure and temperature fields. The 
incompressibility of air can be assumed (Batchelor, 1967) if: 

l_dp\<<u 
p  dt 
\

L' 

(5-15) 

where U and L are the velocity and length scales, respectively, of the atmospheric motion. 

As proposed in Byun (l 999a), one can choose the density and the entropy as the two independent 
parameters of state.  The total derivative of pressure with respect to time can be expressed as: 

Then, Equation 5-15 becomes the relation: 

(5-16) 

(5-17) 

where  csound  is the speed of sound in the atmosphere, i.e.,  c.Mund  =~lap I apj.  Batchelor (1967) 
stated that for Equation 5-17 to be satisfied, not only the difference between the two terms in the 
left hand side of Equation 5-17, but also the magnitude of each term should be small.  When the 
condition 

1  dp 
---<<(cid:173)
pc;_  dt 

u 
L 

(5-18) 

is satisfied, the change in the density of a material element due to pressure variations are 
negligible, that is, the fluid is behaving as if it were incompressible.  By expanding the term  dp 
dt 

in an Eulerian expression one can show that in order for the atmosphere to be treated as 
incompressible, the following conditions must be satisfied: 

u2 
-2-<<l; 
c,.und 

u2 
_P_<<l• 
' 
2 

csound 

gL. 
-2-<<l, 
c .. und 

(5-i 9) 

where UP  is the phase speed of dominant atmospheric waves.  The first condition states that the 
movement of air should have a Mach number much smaller than one, say 10%; the second 
condition states that energy-carrying waves should not propagate as fast as 10% of the speed of 
sound; and the last condition limits the vertical extent of motion to less than about one kilometer. 
Similarly, Dutton and Ficht! (1969) showed that the nondivergent wind relation is generally 
applicable up to half a kilometer above ground level through a scale analysis of the continuity 
equation.  Because of these limitations, a meteorological model with incompressible flow 
approximation may not be suitable for multiscale air quality simulations that' require descriptions 
of atmospheric motions over a wide range of temporal and spatial scales.  The second condition: 

1  (dpJd' 
u 
pc;und  ds  dt <<  L 

(5-20) 

means that variation of entropy due to internal heating or due to molecular conduction of heat 
into the element must be small.  For adiabatic or pseudo-adiabatic atmosphere, Equation 5-20 is 
usually satisfied. 

Basically, an incompressible atmosphere assumption is a shallow-water approximation for an 
adiabatic atmosphere.  With the incompressibility assumption, the distinction between the 
continuity equation and its advective form becomes blurred.  Consequently, concentrations in the 
form of either density or mixing ratio are often used indiscriminately in atmospheric diffusion 
equations.  As presented above, the incompressible atmosphere assumption is a very restrictive 
approximation that disassociates linkage between the thermodynamics and dynamics of 
atmospheric motions.  The incompressible atmosphere approximation simplifies the continuity 
equation of the air to the nondivergent wind component relation regardless of the type of vertical 
coordinates used. Compared with this, the atmosphere described with the hydrostatic pressure 
coordinate is not necessarily incompressible even for the hydrostatic atmosphere.  Because the 
vertical layer is defined by the pressure surface, the hydrostatic approximation applied with the 
hydrostatic pressure coordinate system limits only the vertical propagation of sound waves and 
the atmosphere is not totally incompressible. 

One might expect that as long as the wind field satisfies the nondivergent flow approximation, an 
air quality model would satisfy the pollutant species mass conservation.  In the following, it is 
shown that this expectation is correct only when the air density field is perfectly mass consistent. 
As will be shown later in Equation ·5-24, the trace species mass conservation is affected by the 
air density error term  QP  irrespective of whether or not the wind field is solenoidal.  The 
implication is that a nondivergent wind field does not guarantee the mass conservation of 
pollutant species as long as there is inconsistency in air density and wind fields.  It is not a 
surprising statement, but in general this fact has not been actively addressed in air quality 
modeling studies.  Because the nondivergent relation simply disassociates density and wind 
fields, it cannot be used to estimate the mass consistency error in the meteorological data set.  On 
the other hand, the diagnostic relations applicable for the famHy of hydrostatic pressure 
coordinates based on total air density maintain the consistency in wind and air density fields. 

#### 5.1.2.4 Anelastic Atmosphere Assumption 

Another popular limiting approximation applied in meteorological modeling is the anelastic 
assumption.  It simplifies the continuity equation as a diagnostic relation for the momentum 
(p0V, where  p0  is density ofreference atmosphere) components as follows: 

v S  e  ( .fY po VS) + ~ ( .fY p D V3

)  = 0 

(5-21) 

Ogura and Phillips (1962) and Dutton and Fitchl (1969) found that for deep atmospheric 
convection, if the characteristic vertical scale of motions is smaller than th~: atmospheric scale 
height, the anelastic assumption is satisfied.  For shallow convection, the Boussinesq 
approximation allows us to treat the fluid as incompressible; for deep convection, the 
approximate continuity equation requires the momentum field to be solenoidal, and the 
expansion or contraction of parcels moving in the vertical is taken into account.  Lipps and 
Hemler (1982) also performed a scale analysis to propose a set of approximate equations of 
motion which are anelastic when the time scale is larger than the inverse ofBrunt-Vaisala 
frequency.  The anelastic approximation leads to a divergent wind field, i.e.: 

(5-22) 

Usually, the right hand side of Equation 5-22 does not vanish.  Like the nondivergent wind field 
approximation, this assumption provides a diagnostic relation among wind components although 
it cannot be used to estimate the inconsistency in the total air density, p, and wind field data 
provided by a meteorological model.  However, unlike the incompressible atmosphere 
assumption, the pressure, temperature and wind fields are not completely independent with the 
anelastic assumption.  The distribution of pressure must be such that the wind fields predicted by 
the momentum equations continue to satisfy the anelastic relation (Gal-Chen and Somerville, 
1975).  For this reason, most anelastic meteorological models solve for the elliptic equation for 
the pressure that is derived from Equation 5-22.  Refer to Nance and Durran (1994) for a recent 
review on the accuracy of anelastic meteorological modeling systems. 

For air quality application, the anelastic approximation still requires use of a full continuity 
equation for the perturbation density component.  However, most anelastic meteorological 
models do not solve for the perturbation air density directly.  Therefore, one needs to infer it 
from other thermodynamic fields.  Also, because the trace gas concentration depends on the total 
density of air, not on just the reference density, it does not simplify the pollutant continuity 
equation and the concentration distribution represented in density units cannot be interchanged 
with trace species mixing ratio. 

#### 5.1.2.5 Hydrostatic Atmosphere Approximation 

Perhaps one of the most popular assumptions of atmospheric dynamics used in meteorological 
models is the hydrostatic approximation.  In the case of a hydrostatic atmosphere, the 
acceleration and the frictional force terms in the z-direction of the earth-tangential Cartesian 
coordinates are considered negligible.  In earlier days of atmospheric modeling, the hydrostatic 
approximation was usually applied with the pressure coordinate.  It is well known that the 
hydrostatic pressure coordinate applied to a hydrostatic atmosphere has a special property that 
simplifies the continuity equation into a solenoidal form and provides a diagnostic equation for 
the vertical velocity component.  On the other hand, the geometric height coordinate was not 
used extensively for studying a hydrostatic atmosphere.  Recently, Ooyama (1990) and DeMaria 
(1995) have presented a diagnostic vertical velocity equation.  Extending this, a general 
diagnostic equation for the vertical velocity component can be obtained with the hydrostatic 
approximation for a coordinate whose Jacobian is independent of time (Byun, 1999a): 

a  (- 2 
as  pcsound  as  = -sign  ()z  g  m 

iN

3 J 

.  (as)  [  zv  ( JsfJVs )  1 Q J 

s  •  ~ -

s  p 

(5-23) 

The diagnostic Equation 5-23 can be used to maintain mass consistency in meteorological data 
for air quality simulations. 

It is worthwhile to note that the hydrostatic or nonhydrostatic atmospheric description, which is a 
characterization of the vertical motion, is rather independent from either the 
compressible/incompressible atmosphere or the anelastic atmosphere assumption, which is an 
approximation of the mass continuity equation.  Choices of the assumptions from the two distinct 
groups have been used to simplify atmospheric motions, although some of the combinations, 
such as compressible but hydrostatic atmosphere, are rarely used in atmospheric studies. 

## 5.2 Choice of Vertical Coordinate System for Air Quality Modeling 

Figure 5-1  provides a pedigree of vertical coordinates used in many atmospheric models. 
Definitions of the coordinates are provided in Tables 5-1, 5-2 and 5-3.  The hierarchy of 
classification is: (1) temporal dependency of coordinates, (2) base physical characteristic of 
coordinate variables, and (3) method of topography treatments.  Application assumptions, such 
as hydrostatic or nonhydrostatic atmosphere approximations, are not part of the classification 
criteria.  Isentropic coordinates are not included here because they are not suitable for the 
regional and urban scale air quality simulation due to their inherent difficulties representing 
planetary boundary layer (PBL) structure.  For larger-scale simulations, an isentropic coordinate 
system can serve as an interesting alternative (Arakawa et al., 1992).  Also, there are new 
developments of hybrid coordinates that combine isentropic coordinates with other coordinates 
to mitigate the problem. 

Many different types of vertical coordinates have been used for various meteorological 
simulations.  !"or example, the geometric height is used to study boundary layer phenomenon 
because of its obvious advantage of relating near surface measurements with modeled results. 
Pressure coordinates are natural choices for atmospheric studies because many upper 
atmospheric measurements are made on pressure surfaces.  Because most radiosonde 
measurements are based on hydrostatic pressure, one may prefer use of the pressure coordinate 
to study cloud dynamics.  This idea of using the most appropriate vertical coordinate for 
describing a physical process is referred to as a generic coordinate concept (Byun et al.,  1995). 
Several different generic coordinates can be used in a CTM for describing different atmospheric 
processes while the underlying model structure should be based on a specific coordinate 
consistent with the preprocessor meteorological model.  The Models-3 Community Multiscale 
Air Quality (CMAQ) modeling system allows users to choose a specific coordinate without 
having to exchange science process modules (i.e., subroutines with physica1 parameterizations 
for describing atmospheric processes) which are written in their generic coordinates.  The 
coordinate transformation is performed implicitly through the use of Jacobian within CMAQ. 

Byun (1999a) discusses key science issues related to using a particular vertical coordinate for air 
quality simulations.  They include a governing set of equations for atmospheric dynamics and 
thermodynamics, the vertical component of the Jacobian, the form of continuity equation for air, 
the height of a model layer (expressed in terms of geopotential height), and other special 
characteristics of a vertical coordinate for either hydrostatic or nonhydrostatic atmosphere 
applications.  Tables 5-1, 5-2 and 5-3 summarize properties of the popular time-independent 
vertical coordinates (e.g., terrain-influenced height and the reference hydrostatic pressure 
coordinate systems) and the time-dependent terrain-influenced coordinate systems, respectively. 

Not only the assumptions on atmospheric dynamics, but also the choice of coordinate can affect 
the characteristics of atmospheric simulations.  For the time-independent vertical coordinates (z, 
p"' sigma-z, sigma-p0 ), the vertical Jacobians are also time-independent.  Especially with the 
hydrostatic assumption, one can obtain a diagnostic equation for the vertical velocity component 
, which includes soundwaves together with meteorological signals.  Further assumptions on flow 
characteristics, such as anelastic approximation, provide a simpler diagnostic equation for the 
nonsolenoidal air flow.  For such cases, with or without the anelastic approximation, one can 
maintain trace species mass conservation in a CTM by using the vertical velocity field estimated 
from the diagnostic relation.  The scheme works whether the horizontal wind components, 
temperature, and density field data are directly provided from a meteorological model or 
interpolated from hourly data at the transport time step.  This suggests that the mass error can be 
estimated with the diagnostic relations that originate from one of the goveming equations of the 
preprocessor meteorological models.  For a nonhydrostatic atmosphere, which does not have a 
special diagnostic relation for time ind~pendent coordinates, one should rely on the methods . 
described below to account for the mass consistency errors. 

For time dependent coordinates, the vertical Jacobians are also time dependent.  In general, this 
makes it more difficult to derive a diagnostic relation from the continuity equation.  However, 
for a coordinate with the Jacobian-weighted air density independent of.height, a diagnostic 
equation for the vertical velocity is available when appropriate top and bottom boundary 
conditions are used.  Vertical layers defined with this type of vertical ~oordinate are considered 
as material surfaces because mass continuity can be satisfied in a diagnostic fashion.  Air 
particles are not expected to cross material surfaces during the advection process.  An 
atmospheric model based on this type of coordinate may not h.ave a mass consistency problem 
except for numerical reasons.  The dynamic pressure coordinates based on true air density belong 
to this category, which includes such coordinates as  ~-coordinate, O'ir·coordinate, and the 1J  -
coordinate defined in conjunction with  air (See Table 5-3).  A meteorological model using one 
of these coordinates will conserve mass within the liinit'3 of numerical errors expected from finite 
differencing and computer precision.  For these coordinates, one can apply the same mass 
conservation procedure for both hydrostatic and nonhydrostatic cases.  Note that the diagnostic 
relations obtained by appropriate choices of coordinates and assumptions on atmospheric 
dynamics allow estimation of the density error term in the continuity equation.  This information 
can be used to reconstruct mass-consistent air density and wind fields that ensure mass 
conservation of pollutant species in air quality models. 


.. -.. 
, 
' 
,;;,  ~  ,' 
\ 
>-
, __ , 

I 

/ 

.......... 

.. -.. 
, 
' 
' 
' 
>-
:r 
I 
I 
\  z 
, 
./1'--" 

/ 

....... 

'\. 
.,., 

....... ...._).I .. - .. 
' 
C 
'?£:. 

\ 
I 
', 
,' 
jt.,.-' 
I 
I 

/ 

\ 

.. -.. 
. ~  ' 
, 

, 
7"--' 

C 

\ 

I 

\ 

.. ....__ 

.. -~ 
' 
\ 
'?i:_ 
, __ , 
, 
' 
z 

I 
l 
\ 

~, 

Figure 5-1.  Pedigree of meteorological vertical coordinates.  The encircled T symbol  represents 
that the associated coordinates are identical when temporal dependency is ignored.  Dashed(cid:173)
circles show that all the coordinates can be used for hydrostatic (HYD) and nonhydrostatic 
(NHY) application, regardless of the dynamic characteristics of the variables used to define 
vertical coordinates. 

Table 5-1.  Summary of Characteristics of the Geometric Height and Pressure Coordinate 
Systems. [Note: HYD and NHY stand for hydrostatic and nonhydrostatic applications, 
respectively.  D( ) and P( ) symbols are assigned for diagnostic and prognostic formulas with 
equation numbers.  p 0  and p are the reference and dynamic (time-dependent) hydrostatic 
pressure, respectively.] 

Coordinate 

geometric height 
(z) 

vertical velocitv 
NHY: w with P(S-9) or P(5-
9') 
HYD: D(S-23) 

) 

reference 
hydrostatic 
pressure  (p 0
~ =-p.(z)g 
dynamic 
hydrostatic 
pressure  (n), 
an 
a;= -p(x,y,z,t)g 

large-scale 
hydrostatic 
pressure  (p) , 
:  = -p(x,y,z,t)g 

NHY: P(S-9) or P(S-9') 
HYD: D(S-23) 

v3  =ii =-f [m2V  •( Vw )- QP]dn' 
for both NHY &  HYD 

"  m' 

p 

wr 

NHY: P(S-9) or P(S-9') for 
perturbation component 
HYD: 
v =.P=-f[mV  •  __e...  --::--ld.P 

c) 

V-
fi  m' 

Q,, 
p 

fi 
;, 

J 

• 

2 

Vertical Jacobian 
J.=J 
constant in 
(x1,.x2 . .x3,t) 
J  = (p gf1  constant 
in  c.x1 

.. ~2 ,t) 

Po 

o 

Geoootential hei!!ht 
<P=gz 

- t  dpo' 

Pu.ifr  p" 

cJJ=cJJ 

.ifc: 

11' = (pg)-1  but, 
pl i. = 1 I g  constant 
in  (x1,.x2,.x3,t) 

JP  =(pgfl 

sfc 

c.P=c.P 

"*  p 

- r dn' 
c.P=c.P  -r dfl 

i"r'  p 

.tfc 

Table 5-2.  Summary of Time Independent Terrain-influenced Height and Reference Hydrostatic 
Pressure Coordinate Systems.  [Note: D( ) and P( ) symbols are assigned for diagnostic and 
prognostic formulas with equation numbers, ~espectively, and '* f( )' represents that the 
parameter is not dependent on the argument.] 

Coordinate 

Application  Vertical Momentum 

Vertical 
Jacobian 

Geopotential height 

la,= H-zifc 
-:t:. f(x 3,t) 

<P=gz= 
g[z,1,. + a.(H - z.ifc)] 

. 
J  =  Po 
a,.  P.(x3)g 
¢  f (t), 
P; = P.<zs1,)- Pr 

f'"'  ~ ' 

apo 

.ifc- a 

<P-<P 

-

. 

'"''' P. 

nonnalized 
geometric 
height ( O" z) 
a  = z-z* 
'  H -z>fc 

terrain-
influenced 
reference 
pressure 
(O" p.) 
u  •  &-e.c 
,.  P.(z.,,)-p, 

hydrostatic 

0(5-23) 

generalized 
hydrostatic 

non-
hydrostatic 
hydrostatic 

P(5-9) or, 
P(5-9') with Eq.(5-10) 

0(5-23) 

non-
hydrostatic 

P(5-9) or, P(5-9') with Eq.(5-
10) for perturbation 
component 

Table 5-3.  Summary of Characteristics of the Time Dependent Terrain-influenced Coordinate 
Systems. 

EP A/600/R-99/030 

Geopotential height 

tP = <Plfr. -

tP = <Plfc -

t  jf 

"""'  p 

' 
!7der,, 

r  ir· 

"•,fe  p 

7der,.' 

tP = tP,1r - J."'  ~erJ>' 

a,,,,  p 

tP = <Plfc - f  _'f  dr]' 

q.,. P11src 

Vertical 
Jacobian 

t 

-· 
pJC1  =}!_ 
g 
-:;: J(x 3
n• = 1T:efc  -1T:T 
• 
1C 
pJ  = -
g 

CT" 

) 

) 

*f(x3
pJC1  = 'f/ 
g 
-:;: J(x3

) 

p 

-· 
J  =...:.}!__ 
"  -
pg1Jsfc 
pJ11-:;: 
J(x 3

) 

• 
'IC 

J  = - - <P = <Psfc  -
11 

pg1Jsfc 

r  n' 

q,,.,  PT/src 

~r]' 

pJ11-:;: 
J(x3

) 

-· 

J  =-p-
-
11 
pg1J.rfc 

tP = <Psfr - r _Ji'  dr]' 

q_,.  P11sfr 

Coordinate 

terrain-
influenced 
hydrostatic 
pressure 
(an") 
(J  = 
" 

1C-1T: 

T 
n:.ifc  -1T:r 

Applicatio  Vertical Momentum 
n 
hydrostatic 

n .. a,.j"• 
""' 

"•  Q 
p 

= f  [1r• =:.!!..-m2V  •~J do' 
" 

1rv 
m2 

"• 

non-
hydrostatic 

""' 

iltc" 
-(er  -er  ) -
iJt 

1(1' 

" 

non-
hydrostatic 

terrain-
influenced 
large-scale 
hydrostatic 
pressure( O' fi) 
step-mountain  hydrostatic 
eta  (77)with 

(j n"' 
11  = (j 1C11sfc' 
11.ifc  = 
Po(Zsfc)- Pr 
Pa(O)- Pr 

non-
hydrostatic 

P(5-9) or, P(5-9') with Eq.(5-10) for 
perturbation component when  p 
and  p  given 

iltc. 
-= - f  [ir"=:.!!..-m277  V  • - -q  J d1f 
dt 

1r·v 
sfc  q  m271sfc 

t  q..  Q 
p 
111/r.  o 

iltc" 
dt 

. T 
1r 71  =-11-+ 
q  Q 
f [1r' =:.!!..-m277  V  •~] dr/ 
p 
o 

·v 
sfc  q  m271sfc 

0 

step-mountain  non-
ETA ( 11u-) 

p 

hydrostatic 

.with <J p, 
11u;;  = (j p1Jsfc 

P(5-9) or, P(5-9') with Eq.(5-IO)for 
perturbation component when i/u  , 
p 
p  provided 

## 5.3 Coupling of Meteorology and Air Quality 

Characteristics of air quality model simulations are heavily dependent on the quality of the 
meteorological data.  Meteorological data for air quality can be provided either by diagnostic 
models, which.'analyze observations at surface sites and upper air soundings, or by dynamic 
models with or without four-dimensional data assimilation (FDDA).  Readers are referred to 
Seaman (1999) for a state-of-science review on this topic.  In the next section a dynamic 
modeling with FDDA approach, which is used in the Models-3 CMAQ system, is described. 

### 5.3.1  Meteorolo~cal Data for Air Quality Modeling 

Meteorological simulations are applied to drive a CTM for solving atmospheric diffusion 
equations for trace species.  For regional scale simulations, whose problem size is continental 
i;cale or somewhat smaller, hydrostatic meteorological models have been used, usually with 
FDDA.  For small scale simulations where topographic effects are important, nonhydrostatic or 
compressible atmospheric models are used.  These differences in the assumptions used for 
atmospheric characterization affect air quality simulations greatly. 

Meteorological data can be supplied by running dynamic models prognostically, or with the 
archived reanalysis data routinely available as a part of numerical weather forecasting for air 
quality simulations (Schulze and Turner, 1998).  Currently, GCIP (GEWAX Continental-scale 
International Project) provides an archive of the Eta model reanalysis of surface and upper air 
fields at 48 km resolution (Leese, 1993; Kalany et al., 1996).  Based on th~~ success of GCIP, the 
National Center for Environmental Prediction (NCEP), NOAA, is planning to archive regional 
reanalysis at a higher resolution.  Similarly, the Mesoscale Analysis and Prediction 
System/Rapid Update Cycle (MAPS/RUC) of the Forecast Systems Laboratory (FSL), NOAA, 
produces accurate and timely analyses and short-term forecasts at 40-60 km resolutions 
(Benjamin et al.,  1995, 1998).  The output data are archived at 1-3 hour intervals on 25-34 levels. 
These alternative data sources are promising because of the wealth of observation data used for 
the reanalysis and the availability of long-term meteorological characterization data suitable for 
seasonal or annual assessment studies. 

### 5.3.2  Off-line and On-line Modeling Paradigms 

Air quality models are run many times to understand the effects of emissions control strategies 
on the pollutant concentrations using the same meteorological data.  A non-coupled prognostic 
model with FDDA can provide adequate meteorological data needed for such operational use. 
This is the so-called off-line mode air quality simulation. However, a successful air quality 
simulation requires that the key parameters in meteorological data be consistent.  For example, to 
ensure the mass conservation of trace species, the density and velocity component should satisfy 
the continuity equation accurately.  Details of this issue will be discussed below. 

If air quality is solved as a part of the "meteorology modeling, this data consistency problem 
would be much less apparent.  Dynamic and thermodynamic descriptions of operational 
meteorological models should be self-consistent, and necessary meteorological parameters are 
readily available at the finite time steps needed for the air quality process modules during the 
numerical integration.  The ultimate goal within atmospheric community is the development of a 
fully integrated meteorological-chemical model (Seaman, 1995).  This is the so-called on-line 
mode air quality simulation.  There have been a few successful examples of integrating 
meteorology and atmospheric chemistry algorithms into a single computer program (e.g., Vogel 
et al.,  1995).  For certain research purposes, such as studying two-way interactions of radiation 
processes, the on-line modeling approach is needed.  However, the conventional on-line 
modeling approach, where chemistry-transport code is imbedded in one system, exhibits many 
operational difficulties.  For example, in addition to tremendously increasing the computer 
resource requirements, differences in model dynamics and code structures hinder development 
and maintenance of a fully coupled meteorological/chemical/emissions modeling system for use 
in routine air quality management. 

Figure 5-2 shows structures of the on-line and off-line air quality modeling systems, 
respectively, commonly used at present time.  Table 5-4 compares a few characteristics of on(cid:173)
line and off-line modeling paradigms.  Each method has associated pros and cons.  Therefore, in 
the future versions of the Models-3 CMAQ system, we intend to realize both on-line and off-line 
modes of operations through the use of an advanced input/output (I/O) applications programming 
interface (API) (Coats, 1996).  Figure 5-3 provides a schematic diagram of the implementation 
idea.  A proof-of-concept research effort using MM5 and a prototype version of CMAQ is  . 
underway (Xiu et al.,  1998).  However, to accomplish the goals of multiscale on-line/off-line 
modeling with one system, a full adaptation of the one-atmosphere concept is needed. 

Development of the fully coupled chemistry-transport model to a meteo~ological modeling 
system requires a fundamental rethinking of the atmospheric modeling approach in general. 
Some of the suggested requirements for a next generation mesoscale meteorological model that 
can be used as a host of the on-line/off-line modeling paradigms are: 

• 

• 

• 

Scaleable dynamics and thermodynamics: Use fully compressible form of governing 
set of equations and a flexible coordinate system that can deal with multiscale dynamics. 

Unified governing set of equations: Not only the weather forecasting, dynamics and 
thermodynamics research but also the air quality studies should rely on the same general 
governing set of equations describing the atmosphere. 

Cell-based mass conservation: As opposed to the simple conservation of domain total 
mass, cell-based conservation of the scalar (conserving) quantities is needed.  Use of 
proper state variables, such as density and entropy, instead of pressure and temperature, 
and representation of governing equations in the conservation form rather than in the 
advective form are recommended. 

State-of-the-art data assimilation method: Not only the surface measurements and 
upper air soUndings, but also other observation data obtafued thro~gh the remote sensing 
and other in situ means must be included for the data assimilation. 

Multiscale physics descriptions: It has been known that certain parameterizations of 
physical. processes, including clouds, used in present weather forecasting models are 
scale dependent.  General parameterization schemes capable of dealing with a wide 
spectruffi of spatial and temporal scales are needed. 

'fh.e Weather ~~secy:~h &  ~orecasting (WRF) Modeling System (Dudhia et al., 1998), which is 
under development by scientists at NCAR and NOAA, could meet most of the above 
requirements.  Therefore, the WRF modeling system has the potential to be: the future 
meteorologicat'model oftlie Models-3 CMAQ system to provide the multiscale on-line/off-line 
air quality modeling capability simultaneously. 

Table 5-4.  Comparison of On-line and Off-line Modeling Paradigms 

Off-line Modeling 

On-line Modeling 

Dynamic 
Consistency 

Process Interactions 

• Need sophisticated interface 
processors 
• Need careful treatment of 
meteorology data in AOM 
• No two-way interactions between 
meteorology and air quality 

System 
Characteristics 

• Systems maintained at different 
institutions 
• Modular at system level.  Different 
algorithms can be mixed and tested 
• Large and diverse user base 
• Community Involvement 

Appllcation 
Characteristics 

• Easy to test new science concept 
• Efficient for emissions control 
study 
• Good for independent air quality 
nrocess studv 

• Easier to accomplish, but must have 
proper governing equations. 
• Meteorology data available as 
computed 
• Two-way interaction 
•Small error in meteorological data 
will cause large problem in air 
quality simulation (positive feedback 
problem). 
• Proprietary ownership 
• Expensive in terms of computer 
resource need (memory and CPU) 
• Unnecessary repeat of 
computations for control strategy 
study 
• Low flexibility 
• Limited user base 
• Legacy complex code, which 
hinders new develooment 
• Difficult to isolate individual 
effects 
• Excellent for studying feedback of 
met. and air quality 

Off-line Modeling 

QJ.1-line Modeling 

EP A/600/R-991039 

Meteorology 

Modeling System 

Atmospheric 
Mod~ling System 

Environinental 
Data 

Met., Landuse, 
& Assimilation 
Data 

Surrogate & 
Emisssions 
Data 

• 

Chemistry 
Data 

~ 

Figure 5-2.  Current On-line and Off-line Air Quality Modeling Paradigms  · 

Met., Landuse, 
& Assimilation 
• 
Data 

Meteorology 

Modeling System 

------ - - "' 

1/0API 

CPU/Network 
Boundary 

Unified C3overning 
Set of  Ec~uations 

+ 

-

Chemistry 
Data 

• 

1/0API 

Consistent Numerics 

~~MIMW 

CPU/Network 
Boundary 

+ 

real-time 1/0 API 
feedback on 

: On-line model 

file-based 1/0 API  : Off-line model 
feedback off 

Figure 5-3.  Proposed One-atmosphere Air Quality Modeling Paradigms.  Double arrowhead 
lines represent possibility of two-way coupling. The coupling of independent modeling 
components is accomplished through the 1/0 API linking the cooperating executables. 

## 5.4  Mass Conservation 

For air quality simulations, mass conservation is the most important physical constraint.  This is 
because it is unreali.stic to have injection of primary pollutant mass through any other means than 
a real source emission process, and also because the little perturbations in the mass of both 
primary and secondary pollutants will jeopardize the correct simulation of reactions among trace 
species.  Therefore, conserving mass of a passive primary trace species is a necessary property of 
an air quality model. 

### 5.4.1  Mass Consistency in Meteorological Data 

The main objective of many meteorological models has been to predict synoptic or mesoscale 
weather phenomena.  Therefore, major design considerations are focused on such issues 
important for energy conservation, resolving a spectrum of different wavelengths, and energy 
cascade under nonlinear wave-wave interactions.  Conservation of mass is not usually 
emphasized as the other constraints listed.  Also, the predictive quantities are generally 
thermodynamic parameters, such as temperature and pressure.  The conservation equation for air 
density is rarely solved directly in meteorological models because of little operational use of air 
density for weather forecasting and no direct measurements to compare.  Usually it is estimated 
from the equation for the state of ideal gas or from a hydrostatic relation when hydrostatic 
assumptions are made.  Even the predictive equations for the moisture variables are often written 
in an advective form rather than a continuity equation form.  On the other hand, air quality 
simulation relies mostly on the continuity equation.  The success. of a simulation is heavily 
dependent on the consistency of density and wind data (i.e., how well they satisfy the continuity 
equation). 

The mass inconsistency in density and wind fields from a meteorological model is most likely 
caused by one or more of the following reasons: 

1. 

2. 

3. 

· 4. 

5. 

6. 

7. 

8. 

9. 

Many meteorological models do not use the proposed ideal set of governing equations.  A 
continuity equation for air is not used as one of the prognostic equations and air density is 
usually a diagnostic parameter in meteorological models. 

The prognostic equation for temperature is often used to represent thermodynamics of the 
atmosphere.  It is well known that temperature is not a good conserving parameter. 

Removal of hydrometeors due to condensation or sublimation may subtract and add mass 
and heat to the moist atmosphere making the system nonadiabatic (thermodynamically 
irreversible) and not mass-conserving. 

Numerical schemes used in meteorological models are designed to conserve energy, 
entropy, rather than the mass of air. 

The FDDA and overall assimilation process, including the effects of Newtonian forcing 
terms in the momentum and temperature equations, may cause inadvertent modification 
of the energy balance and subsequent perturbation of air density resulting in mass 
conservation problems. 

Heat, moisture and momentum flux exchanges at the surface-atmosphere interface may 
affect the air density distribution.  Usually this effect is not significant as it is often 
neglected with the Boussinesq approximation. 

Flux exchanges at the nesting boundaries for nested rups affect mass balance. 

Energy and mass balance characteristics of cloud modules used influence air and 
moisture density fields. 

Data output time steps are too large to capture the dynamic variations in the 
meteorological models.  If temporally averaged data are provided from the meteorology 
model this problem can be minimized (Scamarock, 1998). 

### 5.4.2  Techniques for Mass Conservation in Air Quality Models 

As presented in Byun (1999b ), species mixing ratios ( ci Ip) is a useful conserved quantity for 
photochemical Eulerian air quality modeling, in particular.  In limited area atmospheric modeling 
like an urban or a regional scale simulation, the total air mass in the simulation domain is subject 
to the inflow conditions determined by large synoptic scale weather systems.  In this situation, 
the conservation of pollutant mass in the modeling domain can be difficult unless the density and 
wind fields are perfectly mass consistent.  When the mass inconsistency in the meteorological 
fields is expected, the conservation equation for mixing ratio must be used as a necessary 
condition to ensure exact conservation of pollutant mass.  This is accomplished by replacing the 
right-hand-side term of Equation 5-11  with  Qc.  = c;  QP.  Then, the conservation equation for 

' 

p 

pollutant species is rewritten as: 

(5-24) 

This adjustment alone is not sufficient to conserve pollutant mass when tht: density error term is 
not small.  Equation 5-24 shows that the correction term has the same form as a first-order 
chemical reaction whose reaction rate is determined by the normalized air density error term. 
Table 7-5 in Chapter 7 in this document summarizes correction methods discussed in Byun 
(l 999b ).  Among these, the method based on the two-step procedure (i.e., solving the lhs of 
Equation 5-24 first followed by the mass correction step solving for rhs) is expected to be the 
most accurate: 

(c.J  )r:or  = (c;Js)T (pJ )int 

(pJ.,l 

I 

S 

S 

' 

(5-25) 

where superscripts cor, int, and Trepresent corrected, transported (advected), and interpolated 
quantities, respectively.  It should be noted that  ls in Equation 5-25 must not be canceled out 
even for a coordinate with time independent  J.  because the spatial variation of the Jacobian must 
be taken into account for the numerical advection.  In the event the total air mass in the 
computational domain fluctuates, this correction procedure would affect air quality predictions. 
In general, the air quality prediction can be as good as the density prediction of the 
meteorological model.  However, ~onsidering the nonlinear interactions of trace species in the 
chemical production/loss calculations, one could expect serious effects on air quality simulations 
when the quality of meteorology data is in doubt. 

Byun (1999b) also provides an alternative method to deal with the mass inconsistency in 
meteorological data through the modification of wind field, while keeping the density field 
intact, before solving the species conservation equation.  Assuming a modified wind field exists 
that eliminates the source term in the continuity equation for air, the relationship between the 
original and modified wind components is given as: 

" 

,...  M 

V:.  = v .. + 2a2 V)L 

1  . 

I 

(5-26a) 

(5-26b) 

where A, is the Lagrangian multiplier to be determined and a 1 and a3 are the weights for the 
horizontal and vertical wind components. A, must satisfy the Poisson equation 

(5-27) 

with the associated boundary conditions: 

A-=  0 for flow-through boundaries; and 
()A,/ as= Ofor impenetrable boundaries (i.e., at the topographic surface). 

The modified wind components are subject to the same top and bottom boundary conditions 
imposed by the given coordinate system and dynamic assumptions. 

The main difference in the two proposed correction methods, correction after advection versus 
correction of wind fields before advection, is practically.philosophical.  Should we process a 
CTM using meteorological data as supplied, then correct possible errors in the species 
concentrations, or should we modify the velocity field to be mass consistent before the 
computation of trace gas concentrations in the CTM?  The answer to this question lies in whether 
the air quality modeling need is satisfied with simple mixing ratio conservation with the 
adjustment process or not.  In case the source-receptor relation is important, it is preferable to 
maintain the linearity of transport process using the mass-consistent wind components, which 
have been modified at the expense of truthfulness of meteorological fields.  In practice, a 
combination of both methods is needed.  The mass consistency error in the meteorological data 
must be corrected before air quality simulations with the wind-field adjustment method and the 
mixing ratio correction method Equation 5-25 should be applied to compensate the numerical 
differences in advection processes between meteorological and air quality models. 

### 5.4.3  Temporal Interpolation of Meteorological Data 

Byun (1999b) discusses a mass-conservative temporal interpolation method to complement the 
mass inconsistency correction.  Temporal interpolations of density and velocity data are often 
required in a CTM because the meteorological model output has a coarser temporal resolution 
than the transport time step (which is usually the synchronization time step for a CTM using a 
fractional time-step method). 

The Jacobian and density at a time  ta  = (1- a)tn + atn+i  between the two consecutive output time 
steps,  tn  and  tn+P  are interpolated with linearity as~umed: 

(5-28a) 

where  0~a~1. It is obvious that the functional form of the Jacobian (which depends on a 
vertical coordinate) changes the characteristic of density interpolation.  The premise used here is 
that the Jacobjan is a fundamental quantity that determines the coordinate system.  When the 
Jacobian is interpolated to define the vertical layers through linear interpolation, all other 
components involved in the mass conservation equation need to be interpolated accordingly. 
Wind components multiplied with the Jacobian-weighted density are interpolated linearly: 

(5-28b) 

and interpolated wind components are derived with: 

(5-29a) 

(5-29b) 

(5-30a) 

(5-30b) 

However, the proposed scheme, Equation 5-28b, has a problem in such cases where the finite 
difference value of (pls) cannot approximate the linear interpolation of the time rate change of 
the quantity,  acr;;.), adequately.  Usually, this tendency term is not available with the 
meteorological data.  However, when the tendency is available or can be estimated with the 
diagnostic relations for certain meteorological coordinate systems, a different interpolation rule 
must be sought.  Because the tendency term, not  (pls)  itself, is a component of the continuity 
equation, linear interpolation of the tendency may be more appropriate.  Then,  (pls) at the 
interpolation time step must be estimated in such a way that satisfies the continuity as well as the 
tendency term (Byun 1999b ). 

## 5.5 Conclusion 

In this chapter I attempted to bridge the information gap between dynamic meteorologists and air 
quality modelers and to promote the proper use of meteorological information in air quality 
modeling studies.  It highlights the importance of dynamic consistency in meteorological and air 
quality modeling systems.  The effects of the common assumptions used for the atmospheric 
study on the mass conservation for trace species have been re"'.iewed.  Although meteorological 
data provided by operational meteorological models are usually self-consistent, air quality 
modelers need to evaluate the data for exact consistency before they can be used in air quality 
simulation.  Minor adjustment of the meteorological data may be needed to assure mass 
conservation of trace gas species in CTMs.  Also, characteristics of vertical coordinates have 
been discussed.  Certain coordinates provide diagnostic irelations that can be used to maintain 
mass consistency in meteorological fields.  When meteorological data are needed at sub-output 
time steps within CTM, the interpolation of the data should be done in such a way that the mass 
conservation and consistency in the thermodynamic variables are not compromised. 

In addition, the on-line and off-line.modeling concepts are discussed to provide design guidance 
for fully integrated meteorological-chemical models .. To realize the noble goal of implementing 
the one-atmosphere modeling system, both the multi-pollutant chemistry and multiscale physics 
capability in meteorology are needed.  The following are the features that make the CMAQ air 
quality model a suitable key component of an one-atmosphere modeling system: 

• 

• 

• 

• 

• 

• 

Flexible chemistry representations through a mechanism reader; 

Comprehensive list of atmospheric processes that are implemented; 

Modular coding structure and versatile data handling method; 

Capability to handle multiscale dynamics and thermodynamics; 

Fully compressible governing set of equations in generalized coordinates; and 

Robust mixing ratio conservation scheme, even with mass inconsistent meteorology data. 

At present, we are encouraged by the efforts of the WRF meteorology model development 
groups that focus on issues such as choice of coordinates, grid staggering method, state variables 
in the governing equations (e.g., fully compressible), conservation properties (mass and energy) 
both in the model equations and numerics, modulanty of code, data communication methods, 
and coding language.  This entails continuous exchange of ideas between the Models-3 CMAQ 
and WRF modeling groups. 

To achieve the true one:-atmosphere modeling system, we must address multi-pollutant and 
multiscale processes that are typically broader than any one group (or institution) has expertise to 
address.  The need is well summarized in Dennis (1998): 

· 

Considering additional needs for emerging environmental problems such as 
coastal eutrophication and ecological damage issues related with cross-media 
purview, encompassing the one-atmosphere scope is needed.  This means we have 
to work with a more complete one-atmosphere description to facilitate 
interactions within it as efficiently as broadly as possible.  One potential answer is 
to foster a community modeling perspective and model system framework that is 
supported and used by a critical fraction of the scientific community. 

5.6 

References 

Arakawa, A., C.R. Mechso, and C.  S. Konor, 1992: An isentropic vertical coordinate model: 
Design and application to atmospheric frontogenesis studies. Meteor.  Atmos. Phys. 50, 31-45. 

Arya, S. Pal, 1988i Introduction to Micrometeorology.  Academic Press, Inc., 307 pp. 

Batchelor, G.  K.,  1967: An Introduction  to  Fluid Mechanics.  Cambridge University Press,  615 
pp. 

:aenjamin, S.G., D.  Kim, and T.W.  Schlatter,  1995: The Rapid Update Cycle: A new mesoscale 
assimilation system in hybrid-theta-sigma coordinates  at the National  Meteorological Center. 
Second  International  Symposium  on  Assimilation  of Observations  in  Meteorology  and 
Oceanography, Tokyo, Japan, 13-17 March, 337-342. 

Benjamin, S.G., J.M. Brown, K.J. Brunge, B.E. Schwarts, T.G. Smirnova, and T.L. Smith, 1998: 
The operational RUC-2.  16th Conference  on  Weather Analysis and Forecasting,  Phoenix, AZ, 
Amer. Meteor. Soc., 249-252. 

Bishop, R. L. and S. I.  Goldberg, 1968: Tensor Analysis on Manifolds. Dover Publications, Inc. 
New York. 

Byun, D. W.,  1999a: Dynamically consistent formulations in meteorological and air quality 
models for multiscale atmospheric applications:  I. Governing equations in a generalized 
coordinate system. J.  Atmos. Sci., (in print) 

Byun, D. W., 1999b: Dynamically consistent formulations in meteorological and air quality 
models for multiscale atmospheric applications:  II. Mass conservation issues. J.  Atmos.  Sci., (in 
print) 

.. 

Byun D. W., A .. Hanna, C. J.  Coats, and D. Hwang, 1995a: Models-3 Air Quality Model 
Prototype Science and Computational Concept Development. Trans.  TR-24  Regional 
Photochemical Measurement and Modeling Studies, San Diego, CA, of Air &  Waste 
Management Association, 197-212. 

Coats, C. J., cited 1996: The EDSS/Models-3 VO Applications Programming Interface. MCNC 
Enviromnental Programs, Research Triangle Park, NC. [Available on-line from 
http://www.iceis.mcnc.org/EDSS/ioapi/H.AA.html.] 

Defrise, P., 1964: Tensor Calculus in Atmospheric Mechanics. Advances in Geophysics 10, 261-
315. 

. 

DeMaria, M., 1995: Evaluation of a hydrostatic, height-coordinate formulation of the primitive 
equations for atmospheric modeling. Mon.  Wea.  Rev.,  123, 3576-3589. 

Dennis, R.L.,  1998: The environmental protection agency's third generation air quality modeling 
system: an overall perspective. Proceedings of the Ameri.can Meteorological Society 78th 
Annual Meeting, Phoenix, AZ, Jan.  11-16, 1998. 255-258. 

Dudhia, J., D. Gill, J. Klemp, and W.  Skamarock, 1998: WRF: Cuurrent status of model 
development and plans for the future.  Preprints of the Eighth PSU/NCAR Mesoscale Model 
User's Workshop. Boulder, Colorado, 15-16 June,  1998. 

Dutton, J. A.,  1976: The Ceaseless Wind,  an Introduction to the Theory of Atmospheric Motion. 
McGraw-Hill, 579 pp. 

Dutton, J. A., and G. H. Fichtl, 1969: Approximate equations of motion for gases and liquids. J. 
Atmos. Sci., 26, 241-254. 

Gal-Chen, T., and R. C. J.  Somerville, 1975: On the use of coordinate transformations for the 
solution of the Navier-Stokes equations. J.  Comput.  Phys., 17, 209-228. 

Kalany, E., and Co-authors, 1996: The NCEP/NCAR 40-year Reanalysis Project.  Bull. Amer. 
Meteor.  Soc., 77, 437-471. 

Leese, J. A.,  1993: Implementation Plan for the GEWEX Continental-Scale International Project 
(GCIP). Int. GEWEX Project Office #6, 148 pp. [Available from IGPO, 1100 Wayne Ave., Suite 
1225, Silver Springs, MD 20910]. 

Lipps, F. B., and R.  S.  Hemler, 1982: A scale analysis of deep moist convection and some related 
numerical calculations. J.  Atmos. Sci., 39, 2192-2210. 

Nance, L.B., and D.R. Durran, 1994: A comparison of the accuracy of three anelastic systems 
and the pseudo-incompressible system. J.  Atmos. Sci., 51, 3549-3565. 

Ogura, Y., and N. W. Phillips, 1962: Scale analysis of deep and shallow convection in the 
atmosphere. J.  Atmos. Sci.,  19, 173-179. 

Ooyama, K. V.,  1990: A thermodynamic foundation for modeling the moist atmosphere. J. 
Atmos. Sci., 47, 2580-2593. 

Pielke, R. A.,  1984: Mesoscale Meteorological Modeling.  Academic Press, 612 pp. 

Scamarock, W.  1998: Personal communication. 

Schulze, R.H., and D. B. Turner, 1998: Potential use of NOAA-archived meteorological 
observations to improve air dispersion model performance.  EM, March 1998, 12-21. 

Seaman, N. L,  1995: Status of meteorological pre-processors for air-quality modeling. 
International Conf on Particulate Matter, Pittsburgh, PA, Air &  Waste Management 
Association, 639-650. 

Seaman, N.L., 1999: Meteorological modeling for air-quality assessments. (Submitted to Atmos. 
Environ., 32, 87pp.) 

Stull, R. B., 1988: An Introduction to Boundary Layer Meteorology.  Kluwer Academic 
Publishers. 666 pp. 

Thunis, P. and R. Bornstein, 1996: Hierarchy ofmesoscale flow assumptions and equations. J 
Atmos. Sci., 53, 380-397. 

V9gel, B., F. Fiedler, and H. Vogel, 1995: Influence of topography and biogenic volatile organic 
compounds emission in the state of Baden-Wurttemberg on ozone concentrations during 
episodes of high air temperatures. J  Geophys.  Res., 100, 22,907-22, 928. 

Xiu, A., R. Mathur, C. Coats, and K. Alapaty, 1998: On the development of an air quality 
modeling system with integrated meteorology, chemistry, and emissions. Proceedings of the 
International Symposium on Measurement a/Toxic and Related Air Pollutants, Research 
Triangle Park, North Carolina, 1-3 September, 1998.  144-152. 

This chapter is taken from Science Algorithms of the EPA Mode/s-3 Community 
Multiscale_ Air Quality (CMAQ) Modeling System, edited by D. W. Byun and J. K. S. 
Ching, 1999. 

Appendix SA. Tensor Primer and Derivation of the Continuity Equation in a Generalized 
Curvilinear Coordinate System 

The Appendix SA summarizes essential information needed for understanding the goyerning 
equations represented in tensor form.  It includes tensor primer and derivation of the continuity 
equation in a generalized coordinate system.  Readers are referred to classic references such as 
Dutton (1976), Defrise (1964), and Pielke (1984) for the details. 

5A.1  Tensor Analysis in a Curvilinear Coordinate System 

Cartesian coordinates are those curvilinear systems in which the positions of fluid elements are 
determined by their distance from intersecting planes.  Al1hough the Cartesian coordinates with 
orthogonal intersecting planes are specifically called rectangular, the adjective rectangular is 
often dropped.  To represent formulations governing atmospheric phenomena in a coordinate 
system other than a rectangular Cartesian one, a tensor representation is often used.  This 
generally involves determination of the unit vectors in the new system, determination of the 
components of a tensor with respect to theses unit vect?rs, and determination of the differential 
derivatives (e.g., divergence, curl, and gradient) of a tensor.  All these quantities depend 
explicitly on the form of the new coordinate system and it is always convenient to express these 
quantities in a rectangular Cartesian coordinate system for comparison purposes. 

In atmospheric modeling one is frequently led to adopt a curvilinear coordinate system other than 
the Cartesian coordinates depending on the problem under consideration.  A general curvilinear 
coordinate system can be defined relative to a Cartesian system x  = (x 1 .x2 ,x3
three families of curved surfaces 

)  represented by 

•  1 2 3 
x  = 1/f;  x  ,x  ,x· ,t , i  =  , , 

Aj 

2 

I 

( 

3 

) 

(SA-1). 

Here, the symbols with carat(") are used to denote a transformed curvilinear system.  In vector 
form, it is given as: 
x= 'fl(.x,t). 

(SA-2) 

), are three: independent, single-valued, and 

), then the system is called a Euclidean system.  Here we 

When the curvilinear system 1Jf is at rest relative to the rectangular Cartesian system, i.e., 
independent of time,  xi= 1/f;(x1 ,x2 ,x3
assume that components of vector i, (x1 ,x2 ,x3
differentiable scalar point functions such that to every point of some region 9t of three(cid:173)
dimensional Euclidean space, there is a corresponding unique triple of values  (x 1,x2 ,x3
Cartesian space  9t.  In other words, the function 1Jf prescribes one and only one value of x  and is 
such that the three coordinates are independent  of each other.  Also, we assume continuity of the 
function lfl.  Then the new coordinates  i  are called curviUnear and the surfaces  x1 =  1ff1 = 
constant,  x2=  1/f2  =constant,  x3=  1ff3  =constant are called coordinate surfaces.  The curvilinear 
coordinates  (x1 ,x2 ,x3
)  should be independent, single-valued, and differentiable.  As shown in 

)  in the 

Figure SA-1, the vector OP  pointing a parcel of air enclosed by the boundary ail can be 
represented either in Cartesian or Euclidean curvilinear coordinate systems. 

3-x  -z 

2 -x  -y 

I -
X  -x 

Figure SA-1.  Coordinates of the Cartesian and Curvilinear Coordinates.  9\ and 9't  represent 
Cartesian and Euclidean spaces, respectively. 

Note that the transformation involves with not only the spatial variables but also time as an 
independent variable.  We need a tensor calculus in the four variables of space-time with regard 
to the coordinate transformations.  Defrise (1964) used the term 'world tensor' to distinguish it 
from the time independent Euclidean tensor. 

5A.2  Basis Vectors 

In a rectangular coordinate system, directions of the basis vectors are constant in space. 
However, in a general curvilinear coordinates, directions of the basis vectors will vary from point 
to point and no one set of directions can be regarded as more natural than any other for the 
directions of base vectors to defme the local base vectors.  Usually, an upper index denotes 
contra variant, and a lower index denotes covariant tensors, respectively. 

With the coordinates defined by Equation SA-2, the chain rule provides the two expansions: 

d  - ax  J!',.j 

X - - .  U>I. 

ai' 

(5A-3) 

5-32 

,J~i 
UJL  =-.  = 

:L"!i 
OX  dxi 
ax1 

(V"i)  d 
•  x 

x 

EP A1600/R-99JD30 

(5A-4) 

Here, the Einstein summation convention (i.e., the repeated indices on two quantities that are 
multiplied by each other are summed over) has been implied.  The symbol(•) represents the 
inner product.  An inner product of two vectors yields a scalar that is invariant of the coordinate 
system.  An inner product of two tensors results in contraction of the rank in the resulting tensor. 

From Equations 5A-3 and 5A-4, we can form two distinct sets of basis vectors.  One is the 
tangential vectors: 

(5A-5) 

that reveals the variation of the position vector as it traces out a curve in which .Xi  varies and the 
other two coordinates are constant.  Hence  ~ is tangent to the curve along which only .Xi  varies. 
The other set of basis vectors is the normal vectors of the surfaces where  xi  =  constant: 

(5A-6) 

While there could be many choices, the tangential  ( 1i) and normal (ii) vectors are considered as 
a natural choice for the local basis vectors for the curvilinear coordinate system.  Using 
Equations 5A-5 and 5A-6, one can show that: 

,.. 
'f. ® 171  = 8! = 

,.. .  . {1  i = j 

0  i'#j 

I 

I 

(5A-7) 

where 8/ is the Kronecker delta and the symbol ®  represents the outer product.  Outer product 
of two tensors with rank r 1 and r2 yields a tensor with rank (r 1+r2  ). 
A curvilinear system is not orthogonal when not all the off-diagonal components of f]i  ® W and 
fi ®  fi  vanish.  The orthogonal curvilinear coordinate system is often used for interesting 
engineering problems that can be described with simple geometric orthogonal coordinates, such 
as spherical, cylindrical coordinate systems.  Usually, meteorological coordinates are not 
orthogonal and therefore, the vector calculus specific for the orthogonal curvilinear coordinates 
must not be used. 

SA.3  Distance and Metric Tensor in a Curvilinear Coordinate System 

The differential element of distance ds can be expressed in terms of the curvilinear coordinates 
as: 

(5A-8) 

Because of its obvious role in the measurement of distance, the quantity  yik  is called the metric 
tensor.  It is a symmetric tensor.  As such, it has an inverse matrix yik, which will satisfy 
following condition: 

(5A-9) 

"ik 

:L~i  :t..~k 
U,Ji,  U,Ji, 

r  = ax,  ax,  =  ., 

ni@ nk 

., 

(SA-10) 

(5A-11) 

The Levi-Cevita symbol e used in Equation 5-1  is an antisymmetric tensor defined as 

eijk = 

if  i = j,  j  = k,  or  i = k 

. 
0 
1  if  i, j, k  are  an  even  permutation  of  1, 2, 3 
-1  if  i,j,k  are  an  odd  permutation  of  1,2,3 

{ 

(5A-12) 

Using the Levi-Cevita symbol, the cross vector product  A =Bx C  can be written as 
At= e'ii: BJCk. 

One of the uses of the metric tensor and its inverse is for converting a covariant tensor to a 
contravariant tensor, and vice versa.  Another important usage of the metric tensor is the 
estimation ofthe Jacobian determinant of the transformation, which is defined as: 

(SA-13) 

where  J = 1{1:}1 =  ac:1

2
· :

3
· :
d(XpX2 ,X3 ) 

)  •  Note that the Jacobian matrix and the metric tensor are related 

as: 

(5A-14) 

A necessary and sufficient condition that (x1 ,£2 ,x3
components of the metric tensor vanish for i :t: j. 

)  be orthogonal at every point in  9\ is that the 

SA.4  Covariant Tensor and Contravariant Tensor 

In this section, the covariant and contravariant tensor concepts are presented using a vector, 
which is a simple form of a tensor (i.e., a tensor of rank one).  A distance in a Euclidean space 
can be represented in two corresponding sets of tangential basis vectors: 

5-34 

s =  x  'rj = 
d 

d "j" 

dxl 

-r, 

where 

EPA/600/R-99/030 

(5A-15) 

(5A-16) 

Any vector that transforms similarly to the tangential basis vector  ~ is called as a covariant 
vector.  On the other hand, when a vector transforms like the local normal basis vectors  ii, we 
call it a contravariant vector: 

"k 
j 
v  =-.v 
' 

::t.~k 
O.JC 
()x' 

(5A-17) 

where vi  is the components of V with respect to the normal base vectors. 

Since a vector A is invariant between coordinate systems, we can express it using either 
contravariant components (i.e., with the tangential basis vectors) or covariant components (i.e., 
with the normal basis vectors): 

l"' 
A=A/71  =A-r1 

,..  . 

(5A-18) 

Using Equation 5A-7, one can readily find the covariant and contravariant components with: 

(5A-19a) 

(5A-19b) 

SA.5  Derivatives, Total Derivative, and Divergence in Euclidean Coordinate 

Covariant derivative of a contravariant vector is defined as: 

"· 

V" i  _  av· 
;k  - axk  +  kj 

r." iv" j 

, 

Similarly, covariant derivative of a covariant vector is defined as: 

(5A-20) 

(5A-21) 

The Christoffel symbol I'icJ  is not a tensor but it is an important quantity relating the derivatives 
in the curvilinear coordinate system with those in the original Cartesian coordinate system.  Its 
relation with the metric tensor is: 

5-35 

Divergence of a contravariant vector W , wind for example, can be expressed as: 

A 

(5A-22) 

(SA-23) 

The total derivative of a covariant vector A  is represented in a Cartesian coordinate as 

(SA-24) 

dA = aA + v. v A  = a~ +vi a~ 
dt 
()Xi 

dt 
I  a  Ai  ax1  a  Ai  a 

dt 

V 

h 

= V  ()xi  = V  ()Xi  ()xi  = V  ()Xi  Was use  . 

W  ere  V • 
coordinate system where the covariant comp~nentA1, metric tensor, velocity, andx!' all refer to 
the same system whether or not the coordinate system is time dependent. 

. 
S expression lS correct lll any  0  onom1c 

d  Thi 

h  1 

. 

. 

. 

SA.6  Continuity Equation in Generalized Curvilinear Coordinate System 

Many practical coordinate systems used for atmospheric studies are time dependent.  Consider 
the case when a volume element that confines the fluid moves with the fluid.  Then, this is also 
the velocity of the fluid in the respective coordinate system.  A direct conversion from the 
continuity equation expressed in a Cartesian coordinate system does not work because the 
divergence term should take into account for the time rate change of volun1e element as well the 
same for the time dependent curvilinear coordinates.  In this situation, the Lie derivative concept 
(e.g., Bishop and Goldberg, 1968) becomes appropriate.  A Lie derivative is obtained by 
differe11ti.~1ing a function with respect to the parameters along the moving frame of reference. 
Following Defrise (1964), one can show that a Lie derivative of a mass volume integral along the 
moving frame vanishes: 

~(OM.) =0 

av~cp.jf) = sV<vµ a~f + p.ff ;: ) 

= av acpjf vµ)  = o 

axµ 

5-36 

(SA-25) 

(SA-26) 

where, indexµ= 1,4;  x4  = t  and v4 =1.  Using the same notation convention, the contravariant 
velocity is defined as for the coordinates that moves with fluid: 

EPA/600/R-99/030 

(SA-27) 

Note that one cannot derive the same result by directly replacing the divergence term in the 
continuity equation for a Cartesian coordinate system because the volume element is dependent 
on time as well. 

Alternatively, one can obtain Equation SA-26 by a method based on the finite derivative of a 
volume integral with the application of the Leibnitz rule.  This method of derivation helps to 
visualize the meaning of terms in the equation more clearly than the procedure based on the Lie 
derivative.  Volume integral in a Cartesian coordinates is defined as: 

F= JfJ1cx 1,x2,x3 )cW 

on 

(SA-28) 

where/is a conservative quantity, such as density or total kinetic energy.  Equation SA-28 can 
be rewritten in the cilrvilinear coordinate as: 

F= JfJ1cx1,.x2,x3)./Y8V 

on 

For example, if/=1: 

F = Jff  cW = von = fJJ  .fYD\I = .fYvon 

on 

on 

on 

(SA-29) 

(SA-30) 

(SA-31) 

The meaning of the metric becomes very clear-it is a measure of volume correction for the 
transformed coordinates. 

F = JJJ Jc.xi ,.x2 ,x3)./YW = JJJ h(x1 ,.x2 ,.x3)&1&2&3 

on 

where h(x1 ,x2 ,x3

)  = f(x 1 ,x2 ,x3).fY was used. 

Consider a time derivative following the control volume.  Applying a three-dimensional version 
of Leibnitz mle for the time differential of the integral, we get: 

Then, using the following relation with the aid of Figure SA-2: 

( 

..Jt-.2 

J +zh  _cu_ 
il  :i 
·lax 
"• 

dt  bd 

) 

..J!'..2 

8£2 = h(x;)-CM.-b - h(x;)-CM.-" 
dt 

dt 

..J!'..2 

we obtain an integral equation: 

=JII{ah +3-(hdi1I J+-4-(hdi2I J+-4-(hdx3I J}u1ae&e3 

dt 

bd 

c1X3 

dt 

bd 

oFI 
lit  fol/aw/nz 

W>lume 

at 

c1x1 

an 

dt 

bd 

ax2 

(SA-32) 

(SA-33) 

(SA-34) 

!"""-~:-.-_.,, -'--_..... .... di; ot 

dt 

Figure SA-2.  Volume Element in a Curvilinear Coordinate System 

When f  = p  (density), thenF = JJI p(x1 ,x2 ,x3)&1& 2& 3  = M  =mass of the volume element. 

en 

Therefore, the conservation law states: 

oFI 
ot  following 

volume 

= oMI 

Here, for example,  dX

dt  bd 

=O 

volume 

ot  following 
2 I is the velocity of the boundary of the volume element in the curvilinear 

(5A-35) 

coordinate  £2
velocity component of the fluid in the curvilinear coordinate x2

•  Because the volume element confines the fluid and moves with the fluid, this is 

•  Then, we have 

(5A-36) 

Since above integral should be satisfied for an arbitrarily infinitesimal volume element, we 
obtain the continuity equation in differential equation fonn for the time-dependent curvilinear 
coordinate as follows: 

- (5A-37) 

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_Science_Ch_04.md) - [Home](README.md) - [Next Chapter >>](CMAQ_Science_Ch_06.md)<br>
CMAQ Science Document (c) 1999<br>

<!-- END COMMENT -->

