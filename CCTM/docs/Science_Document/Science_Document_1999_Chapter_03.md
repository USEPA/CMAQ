Chapter 3 
=======================

DEVELOPING METEOROLOGICAL FIELDS 

Tanya L. Otte* 

Atmospheric Modeling Division 

National Expostlre Research Laboratory 
U. S. Environmental Protection Agency 

Research Triangle Park, NC 27711 

ABSTRACT 

Meteorological data are important in many of the processes simulated in the Community 
Multiscale Air Quality (CMAQ) model included in the first release of the Models-3 framework. 
The meteorology model that has been selected and evaluated with CMAQ is the Fifth-Generation 
Pennsylvania State University/National Center for Atmospheric Research (NCAR) Mesoscale 
Model (MM5).  All software in the Penn State/NCAR mesoscale modeling system has been 
dedicated to the public domain and is presently used for both research and operational purposes 
at many organizations worldwide.  Improvements to MMS within the meteorological community 
are ongoing, and these enhancements should be evaluated for their applicability to air quality 
modeling.  Other meteorological models are being considered for compatibility with CMAQ but 
are not provided with the initial release.  The application of MMS with CMAQ is described in 
Chapter 3. 

*on assignment from the National Oceanic and Atmospheric Administration, U.S. Department. of Commerce. 
Corresponding author address: Tanya L. Otte, MD-80, Research Triangle Park, NC 27711.  Email: 
tlotte@hpcc.epa.gov 

DEVELOPING METEOROLOGICAL FIELDS 

3.1 

Credits and Disclaimers for Use of MM5 

The Fifth-Generation Pennsylvania State University/National Center for Atmospheric Research 
(NCAR) Mesoscale Model (MM5) was developed in cooperation with Penn State and the 
University Corporation for Atmospheric Research (UCAR).  Penn State and UCAR make no 
proprietary claims, either statutory or otherwise, to this version and release of MM5, and they 
consider MM5 to be in the public domain for use by any person or entity without any fee or 
charge.  Penn State and UCAR shall be credited in any publications that result from the use of 
MM5.  The names "Penn State" and "UCAR" shall not be used or referenced in any advertising 
or publicity that endorses or promotes any products or commercial entity associated with or 
using MM5, or any derivative works thereof, without the written authorization ofUCAR and 
Penn State. 

MM5 is provided to the public by Penn State and UCAR on an "as is" basis, and any 
warranties, either express or implied, including but not limited to implied warranties of non(cid:173)
infringement, originality, merchantability, and fitness for a particular purpose, are disclaimed. 
Neither UCAR nor Penn State will be obligated to provide the user with any support, consulting, 
training, or assistance of any kind regarding the use, operation, and performance of MM5, nor 
will they be obligated to provide the user with any updates, revisions, new versions, error 
corrections, or "bug" fixes.  In no event will UCAR and Penn State be liable for any damages, 
whatsoever, whether direct, indirect, consequential, or special, which may result from an action in 
contract, negligence, or other claim that arises out of or about the access, use, or performance of 
MMS, including infringement actions. 

Data that are obtained from NCAR (e.g., global analyses and observations) are prepared and 
maintained by the Data Support Section, Scientific Computing Division, NCAR.  NCAR is 
operated by UCAR and is sponsored by the National Science Foundation.  Any published 
research that uses data from the NCAR archive shall credit the maintainers of the archive.  In 
addition, the original source(s) of data shall be acknowledged. 

3.2  Meteorology Model Pre-Processing 

Before the meteorology model can be run, several smaller ("pre-processing") programs must be 
run to set up the domain for the simulation and to generate a set of initial and boundary 
conditions for the meteorology model.  The pre-processing programs are briefly described in this 
section. 

3.2.1  Defining the Simulation Domain (TERRAIN) 

Domains for the meteorology simulations are defined by several primary parameters: number of. 
grid points in each horizontal dimension, grid spacing, center latitude, center longitude, map 
projection (Mercator, Lambert conformal, or polar stereographic), and number of "nested" 
domains and their horizontal dimensions.  (Some other user-specific parameters are also defined 
based on the primary parameters, as required.)  These parameters are processed by a program 
that is executed only when a new domain location is required.  This program, TERRAIN, makes 
use of high-resolution global terrain and land use data sets to create "static files" for the domain. 
The static files currently include values for each grid point for terrain height and land use 
specification (e.g., deciduous forest, desert, water).  Future versions of TERRAIN may include 
additional diurnal, seasonal, and location-specific information.  The TERRAIN program is 
thoroughly described by Guo and Chen (1994). 

3.2.2  Processing the Meteorological Background Fields (DATAGRID) 

After the simulation domain has been established, the program DATA GRID is run to process the 
meteorological background fields.  DATA GRID generates first-guess fields for the model 
simulation by horizontally interpolating a larger-scale data set (global or regional coverage) to the 
simulation domain.  DATA GRID interpolates the background fields to the simulation domain for 
times throughout the simulation period; these files are used ultimately to generate lateral 
boundary conditions for the coarse-domain simulations.  (Nested domains obtain lateral boundary 
conditions from the coarse domain.)  DATAGRID also processes the sea-surface tt'..mperature and 
snow files by interpolating the analyses to the simulation domain.  Lastly, DATA GRID calculates 
map-scale factors and Coriolis parameters at each grid point to be used by MM5.  The 
DATA GRID program is thoroughly described by Manning and Haagenson (1992). 

3.2.3  Objective Analysis (RA WINS) 

The program RA WINS performs an objective analysis by blending the first-guess fields generated 
by DATAGRID with upper-air and surface observations.  There are four objective analysis 
techniques available in RA WINS.  The following descriptions are taken largely from Dudhia et al. 
(1998). 

The Cressman scheme assigns a circular radius of influence to each observation.  The 
weights associated with the observations decrease radially from the observation to the 
radius of influence.  The first-guess (meteorological background) field at each grid point is 
adjusted by taking into account all observations that influence that grid point.  (See 
Cressman, 1959.) 

The Ellipse scheme alters the Cressman circles for analyses of wind and relative 
humidity by elongating the circles along the wind flow.  The eccentricity of the ellipses 
increases as wind speed increases.  This scheme reduces to the Cressman scheme under 
light wind conditions.  (See Benjamin and Seaman, 1985.) 

The Banana scheme alters the Cressman circles for analyses of ·wind and relative 
humidity by elongating the circles in the direction of the flow and curving the influence 
region along streamlines.  The resulting influence region is banana"'Shaped.  This scheme 
reduces to the ellipse scheme under straight-flow conditions,. and it further reduces to the 
Cressman scheme under light wind conditions.  (See Benjamin and Seaman, 1985.) 

The Multiquadric Interpolation scheme uses hyperboloid radial basis functions to 
perform objective analysis.  This scheme takes much more time and memory than the 
Cressman-based schemes.  This scheme must be used ·with caution in data-sparse regions 
and when more than 25% of the domain is water-covered.  (See Nuss and Titley, 1994.) 

In addition, RA WINS performs some data quality control checks and buddy checks with user(cid:173)
defined thresholds.  The objective analysis is performed for a user-defined number of pressure 
surfaces (generally mandatory levels plus some additional surfaces).  RAWINS is also used to 
prepare the analyses for the analysis nudging in the simulation model.  The RA WINS program is 
thoroughly described by Manning and Haagenson (1992). 

3.2.4  Setting the Initial and Boundary Conditions (INTERP) 

The "standard" INTERP program sets the initial and boundary conditions for the meteorology 
simulation.  The analyses from RA WINS are interpolated to MM5 's staggered grid configuration 
and from their native vertical coordinate (pressure) to MM5's vertical coordinate (a terrain(cid:173)
following, pressure-based "sigma" coordinate).  In addition, the state variables are converted as 
necessary, e.g., relative humidity to specific humidity.  The analyses from one time (generally the 
first time analyzed by RA WINS) are interpolated by INTERP to provide MM5 's initial 
conditions, while analyses from all times are interpolated to generate MM5's lateral boundary 
conditions. 

INTERP can also be used for "one-way nesting," where the MMS output from one model run is 
interpolated to provide initial and boundary conditions for the nested run.  This is a "non(cid:173)
standard" use of the INT ERP program, but it is commonly used to support air quality modeling. 

3.3 

The Meteorology Model (MMS) 

The following subsections include brief summaries of the science and options that are available in 
MM5.  Thorough descriptions of the standard MM5 options are found in Grell et al.  (1994) and 
Dudhia et al. (1998).  The source code for an early release of MM5 is documented in Haagenson 
et al. (1994).  The Models-3 package does not include NCAR's most current release ofMM5. 
MM5 user options have been tailored and expanded in the Models-3 release to support air 
quality modeling.  Some of these options have not yet been accepted into the official version that 
is maintained'by NCAR.  Caution should be exercised· when deviating.from the version ofMM5 
that is included with Models-3.  Refer to Section 3.5 for a summary of changes to MM5 for 
CMAQ. 

3.3.1  Brief History 

MM5 is the Fifth-Generation Penn State/NCAR Mesoscale Model.  It has evolved from the 
model used by Anthes in the early 1970s, described by Anthes and Warner (1978).  The 
improvements in MM5 over the previous version (MM4) include the option for non-hydrostatic 
physics, as well as more sophisticated explicit moisture, boundary layer processes, radiation, 
convective parameterization, among other improvements.  Dudhia (1993) documented the major 
changes from MM4 to MM5.  Starting with MM5 Version 2 (MM5v2), the software has been 
restructured to run on various hardware platforms in addition to the Cray, with emphasis placed 
on workstation-based MM5 simulations.  The Cray version of MM5 provided on the Models-3 
installation tape has been tested on the EPA's Cray C90.  A workstation-based version of MM5 
may be used with subsequent releases ofModels-3. 

3.3.2  Horizontal and Vertical Grid 

The coordinate system for MM5 is (x, y, sigma-p).  The x and y dimensions are a regular lattice 
of equally spaced points (delta-x = delta-y =horizontal gr.id spacing, in kilometers) forming rows 
and columns.  Sigma is a terrain-following vertical coordinate that is a :function of the pressure at 
the point on the grid (in hydrostatic runs) or the reference state pressure (in non-hydrostatic 
runs), the surface pressure at the grid point, and the pressure at the top of the model.  Sigma 
varies from  1 at the surface to 0 at the top of the model.  The influence of the terrain on the sigma 
structure diminishes with height, such that the sigma surfa.ces near the top of the model are nearly 
parallel. 

The horizontal grid in MM5· has an Arakawa..;B staggering of the velocity vectors with respect to 
the scalars (Arakawa and Lamb,  1977).  The momentum variables (u- and v-components of wind 
and the Coriolis force) are on "dot" points, while all other variables (e.g., mass and moisture 
variables) are on "cross" points.  The dot points form the regular lattice for the simulation 
domain, while the cross points are offset by 0.5 grid point in both the x and y directions.  Note 
that the interpolation of the variables to the staggered grid is done automatically within the 
INTERP program. 

3.3.3  Prognostic Equations 

MM5 is based on primitive physical equations of momentum, thermodynamics, and moisture. 
The state variables are temperature, specific humidity, grid-relative wind components, and 
pressure.  In the prognostic equations, the state variables are mass-weighted with a modified 
surface pressure.  MM5 can be run as either a hydrostatic or non-hydrostatic model.  In the 
hydrostatic model, the state variables are explicitly forecast.  In the non-hydrostatic model 
(Dudhia, 1993), pressure, temperature, and density are defined in terms of a reference state and 
perturbations from the reference state.  MM5 is not mass-conserving in the non-hydrostatic 
mode.  The vertical (sigma) coordinate is defined as a function of pressure.  The model's 
prognostic equations are thoroughly discussed in Grell et al. (1994) and Dudhia et al. (1998). 

3.3.3.1 Time  Differencing 

The hydrostatic and non~J:iydrostatic versions of MM5 use different time differencing schemes to 
filter the fast waves from the prognostic solutions in the model.  In the non-hydrostatic model, a 
semi-implicit scheme based on Klemp and Wilhelmson (1978) is used to control the acoustic 
waves in the model solution.  In the hydrostatic model, a split-explicit scheme based on Madala 
(1981) is used to control gravity waves in the model solution.  The time differencing in MMS is 
discussed at length in Grell et al. (1994) and Dudhia et al. (1998). 

3.3.3.2 Lateral Boundary Conditions 

There are five options for lateral boundary conditions in MMS: fixed, relaxation, time dependent, 
time and inflow/outflow dependent, and sponge.  The lateral boundaries in MMS  consist of 
either the outer five grid points (~elaxation and sponge options) or the outer grid point (all other 
options) on the horizontal perimeter of the sirilulation domain.  (The outer four grid points are 
used for boundary conditions for "cross" point variables for the relaxation and sponge options.) 
The lateral boundary conditions for the coarse domain are derived from the background fields 
processed in DATA GRID and INTERP.  When the one-way nest option is selected, the lateral 
boundary conditions for nested domains are interpolated from the simulation on the parent 
domain. 

3.3.4  Model Physics 

Several model physics options in MM5 are briefly noted below.  The model physics options are 
further discussed and compared in Dudhia et al. (1998).  The descriptions of the model physics 
options are largely taken from Dudhia et al (1998), and other pertinent references are noted. 

3.3.4.1 Radiation 

There are five atmospheric radiation cooling schemes available in MM5. 

• 

• 

• 

• 

• 

The "None" option applies no mean radiative tendency to the atmospheric temperature . 
This scheme is unrealistic for long-term simulations. 

The Simple Cooling scheme sets the atmospheric cooling rate strictly as a function of 
temperature.  There is no cloud interaction or diwnal cycle. 

The Surface Radiation scheme is used with the "none" and "simple cooling" schemes. 
This scheme includes a diurnally varying shortwave and longwave flux at the surface for 
use in the ground energy budget.  These fluxes are calculated based on atmospheric 
column-integrated water vapor and low/middle/high cloud fraction estimated from relative 
humidity. 

The Dudhia Longwave and Shortwave Radiation scheme is sophisticated enough to 
account for longwave and shortwave interactions with explicit cloud and clean air.  This 
scheme includes surface radiation fluxes and atmospheric temperature tendencies.  This 
scheme requires longer CPU time, but not much memory.  (See Dudhia, 1989.) 

The CCM2 Radiation scheme includes multiple spectral bands in shortwave and 
longwave, but the clouds are treated simply as functions of relative humidity.  This 
scheme is suitable for larger grid scales and probably more accurate for long time 
integration (e.g., climate modeling).  It also provid1es radiative fluxes at the surface.  (See 
Hack et al.,  1993.) 

3.3.4.2 Convective Parameterization 

There are currently six convective parameterization schemes in MM5.  There is also the option 
for no convective parameterization and an independent option for shallow convection.  The 
convective parameterization schemes have been designed for use at various simulation scales, and 
they are not entirely interchangeable.  For example, each scheme uses different assumptions for 
convective coverage on the sub-grid-scale and for the convective trigger function.  The convective 
parameterization schemes also differ greatly in CPU usage and memory requirements. 

3-7 

The Anthes-Kuo scheme is based on moisture convergence and is mostly applicable to 
larger grid scales (i.e., greater than 30 km).  This scheme tends to produce more convective 
rainfall and less resolved-scale precipitation.  This scheme uses a specified heating profile 
where moistening is dependent on relative humidity.  (See Anthes, 1977.) 

The Fritsch-Chappel scheme is based on relaxation to a profile due to updraft, 
downdraft, and subsidence region properties.  The convective mass flux removes 50% of 
the available buoyant energy in the relaxation time.  There is a fixed entrainment rate. 
This scheme is suitable for 20-30 km scales due to the single cloud assumption and local 
subsidence.  (See Fritsch and Chappel, 1980.) 

The Arakawa-Schubert scheme is a multi-cloud scheme that is otherwise similar to the 
Grell scheme (described below).  This scheme is based on a cloud population, and it 
allows for entrainment into updrafts and the existence of downdrafts.  This scheme is 
suitable for larger grid scales (i.e., greater than 30 km).  This scheme can be 
computationally expensive compared to the other available schemes.  (See Arakawa and 
Schubert,  1974.) 

The Kain-Fritsch scheme is similar to the Fritsch-Chappel scheme, but it uses a 
sophisticated cloud-mixing scheme to determine entrainment and detrainment.  This 
scheme also removes all available buoyant energy in the relaxation time.  (See Kain and 
Fritsch, 1990, 1993.) 

The Betts-Miller scheme is based on relaxation adjustment to a reference post(cid:173)
convective thermodynamic profile over a given period.  This scheme is suitable for scales 
larger than 30 km.  However, there is no explicit downdraft, so this scheme may not be 
suitable for severe convection.  (See Betts, 1986; Betts and Miller, 1986, 1993; and Janjic, 
1994.) 

The Grell scheme is based on rate of destabilization or quasi-equilibrium.  This is a 
single-cloud scheme with updraft and downdraft fluxes and compensating motion !Pat 
determines the heating and moistening profiles.  This scheme is useful for smaller grid 
scales (e.g.,  10-30 km), and it tends to allow a balance between the resolved scale rainfall 
and the convective rainfall.  (See Grell et al.,  1991; and Grell, 1993.) 

The "no convective parameterization" option (e.g., explicitly resolved convection on 
the grid scale) is also available.  This option is generally used for simulations on domains 
with horizontal grid spacing smaller than 10 km. 

3-8 

The Shallow Convection scheme is an independent option that handles non(cid:173)
precipitating clouds that are assumed to be uniform and to have strong entrainment, a 
small radius, and no downdrafts.  This scheme is based on the Grell and Arakawa(cid:173)
Schubert schemes.  There is also an equilibrium assumption between cloud strength and 
sub-grid boundary layer forcing. 

3.3.4.3 Planetary Boundary Layer Processes 

Four planetary boundary layer (PBL) parameterization schemes are available in MM5.  These 
parameterizations are most different in the turbulent closure assumptions that are used.  The 
PBL parameterization schemes also differ greatly in CPU usage. 

• 

• 

• 

• 

The Bulk Formula scheme is suitable for coarse vertical resolution in the PBL (i.e., 
greater than 250 m vertical grid sizes).  This scheme includes two stability regimes. 

The Blackadar scheme is suitable for "high-resolution" PBL (e.g., five layers in the 
lowest kilometer and a surface layer less than 100 m thick).  This scheme has four 
stability regimes; three stable and neutral regimes are handled with a first-order closure, 
while a free convective layer is treated with a non-local closure.  (See Blackadar, 1979.) 

The Burk-Thompson scheme is suitable for coarse and high-resolution PBL.  This 
scheme explicitly predicts turbulent kinetic energy for use in vertical mixing, based on a 
1.5-order closure derived from the Mellor-Yamada formulas.  (See Burk and Thompson, 
1989.) 

The Medium Range Forecast (MRF) model scheme is suitable for high-resolution 
PBL.  This scheme is computationally efficient.  It is based on a Troen-Mahrt 
representation of the counter-gradient term and a first-order eddy diffusivity (K) profile 
in the well-mixed PBL.  This scheme has been taken from the National Centers for 
Environmental Prediction's (NCEP's) MRF model.  (See Hong and Pan, 1996.) 

3.3.4.4 Surface Layer Processes 

The surface layer processes with the Blackadar and MRF PBL schemes have been parameterized 
with fluxes of momentum, sensible heat, and latent heat, following Zhang and Anthes (1982). 
The energy balance equation is used to predict the changes in ground temperature using a single 
slab and a fixed-temperature substrate.  The slab temperature is based on an energy budget, and 
the depth is assumed to represent the depth of the diurnal temperature variation ( ~ 10-20 cm). 
The 13  land use categories are used to seasonally define the physical properties at each grid point 
(e.g., albedo, available moisture, emissivity, roughness length, and thermal inertia). 

A five-layer soil temperature model (Dudhla,  1996) is also available as an option in MM5.  In 
this model, the soil temperature is predicted at layers of approximate depths of 1, 2, 4, 8, and 
16 cm, with a fixed substrate below using a vertical diffusion equation.  This scheme vertically 
resolves the diurnal temperature variation, allowing for more rapid response of surface 
temperature.  This model can only be used in conjunction with the Blackadar and MRF PBL 
schemes. 

In a subsequent release ofCMAQ, the Pleim-Xiu land-surface scheme (Pleim and Xiu, 1995) 
may be included in MM5."  The Pleim-Xiu scheme has been developed to address surface 
processes that can significantly impact air quality modeling, including evapotranspiration and soil 
moisture.  Notable in the Pleim-Xiu scheme is the more careful treatment of the surface 
characteristics (particularly vegetation parameters) that are currently assigned to grid points 
based on land use specification, as well as a more detailed land use and soil type classification 
database. 

3.3.4.5 Resolvable-Scale Microphysics Schemes 

There are six resolvable-scale (explicit grid-scale) microphysics schemes in MM5.  There is also 
an option for a "dry" model run.  The microphysics schemes have been designed with varying 
degrees of complexity for different applications of the model.  In addition, there are new 
prognostic output variables that are generated by the more sophisticated schemes.  These 
microphysics schemes also differ greatly in CPU usage and memory requirements. 

• 

• 

• 

• 

• 

The Dry scheme has no moisture prediction.  Water vapor is set to zero.  This scheme is 
generally used for sensitivity studies. 

The Stable Precipitation scheme generates non-convective precipitation.  Large-scale 
saturation is removed and rained out immediately.  There is no evaporation ofrain or 
explicit cloud prediction. 

The Warm Rain scheme uses microphysical processes for explicit predictions of cloud 
and rainwater fields.  This scheme does not consider ice phase processes.  (See Hsie and 
Anthes,  1984.) 

The Simple Ice scheme adds ice phase processes to the warm rain scheme without 
adding memory.  This scheme does not have supercooled water, and sriow is immediately 
melted below the freezing level.  (See Dudhla,  1989.) 

The Mixed-Phase scheme adds supercooled water to the simple ice scheme, and slow 
melting of snow is allowed.  Additional memory was added to accommodate the ice and 
snow.  This scheme does not include graupel or riming processes.  (See Reisner et al., 
1993, 1998.) 

The Mixed-Phase with Graupel scheme adds graupel and ice number concentration 
prediction equations to the mixed-phase scheme.  This scheme is suitable for cloud(cid:173)
resolving scales.  (See Reisner et al., 1998.) 

The NASA/Goddard Microphysics scheme explicitly predicts ice, snow, graupel, and 
hail.  This scheme is suitable for cloud-resolving scales.  (See Tao and Simpson, 1993.) 

• 

• 

3.3.5  Nesting 

MMS can simulate nested domains of finer resolutions ·within the primary simulation domain.  In 
MMS, the software is configured to enable up to nine nests (ten domains) within a particular run. 
However, due to current hardware resources, the state of the science, numerical stability, and 
practicality, the number of domains in a simulation is generally limited to four or fewer. 

Nesting can be accomplished by either a "one-way" or a "two-way" method.  In one-way 
nesting, the coarse-resolution domain simulation is run independentiy of the nest.  The coarse 
domain can then provide the initial and boundary conditions for its nest.  In one-way nesting, 
each domain can be defined with independent terrain fields, and there are no feedbacks to the 
coarse domain from its nest.  Note that the simulated meteorology at the same grid point in the 
coarse-resolution domain is likely to be different (if only slightly) from the nest in a one-way 
nest simulation. 

Two-way interactive nesting (Zhang et al., 1986; SmolarJrJewicz and Grell, 1992) allows.for 
feedback to occur between the. coarse-resolution domain and the nest throughout the simulation. 
The two domains are run simultaneously to enable this feedback, and terrain in the overlapping 
regions must be compatible to avoid mass inconsistencies and generation of numerical noise.  The 
TERRAIN program automatically defines the terrain compatibility when the user specifies the 
two-way nesting interaction.  When two-way nesting is used, the portion of the coarse-resolution 
domain that is simulated in the nest may reflect too much smaller-scale detail from the nest to be 
useful for the CMAQ simulations. 

The nesting ratio between domains in MMS is generally 3: 1.  (Some other mesoscale meteorology 
models allow for user-defined nest ratios.)  For example, if the coarse domain is a 36-km 
resolution domain, its nest will be a 12-km resolution domain.  This is strictly true for a two-way 
nest, but is largely held as a standard for the one-way nests in MMS.  The nest ratio restricts the 
number of grid points in each dimension of the nest domains to a multiple of 3, plus 1. 

3.3.6  Four-Dimensional Data Assimilation 

The four-dimensional data assimilation (FDDA) scheme included in MMS iis based on 
Newtonian relaxation or "nudging".  Nudging is a continuous form of FDDA where artificial 
(non-physical) forcing functions are added to the model's prognostic equations to nudge the 
solutions toward either a verifying analysis or toward observations.  The artificial forcing terms 
are scaled by a nudging coefficient that is selected so that the nudging term will not dominate the 
prognostic equations.  The nudging terms tend to be one order of magnitude smaller than the 
dominant terms in the prognostic equations and represent the inverse of thee-folding time of the 
phenomena captured by the observations. 

There are two types of nudging in MMS: analysis nudging and observation nudging ("obs 
nudging").  Analysis nudging gently forces the model solution toward gridded fields.  Analysis 
nudging can make use of three-dimensional analyses and some surface analyses.  Analysis 
nudging is generally used for scales where synoptic and mesoalpha forcing are dominant.  Obs 
nudging gently forces the model solution toward individual observations, with the influence of the 
observations spread in space and time.  Obs nudging is better suited for assimilating high 
frequency, asynoptic data that may not otherwise be included in an analysis. 

Nudging in MMS is extensively discussed in Stauffer and Seaman (1990, 1994) and Stauffer et al. 
(1991).  The data assimilation is generally used throughout the MMS simulation period for air 
quality simulations.  Three-dimensional analyses of wind, temperature, and moisture are 
assimilated, and only surface analyses of wind are assimilated, following Stauffer et al. (1991 ). 

3.4  Meteorology Model Post-Processing 

Since the output variables that are generated by an MMS simulation are not always useful in 
their raw form, those variables must be converted into fields that are required by the chemistry 
and emission models.  The conversion of MMS output to useful fields for the other Models-3 
programs is accomplished in the Meteorology Chemistry Interface Processor (MCIP), which is 
discussed in Chapter 12.  MCIP computes variables that are useful to the subsequent models in 
Models-3, and it creates an output file written with the 1/0 API libraries in netCDF format that 
is standard in the Models-3/CMAQ modeling system. 

3.5 

Changes to the MMS System's Software for Models-3 

The version ofMMS included with the initial release ofModels-3 is MMS Version 2, Release 6. 
The system is complete with "bug fixes" included through October 1997.  Only selected "bug 
fixes" and upgrades have been included beyond October 1997.  (Including all of the changes to 
MMS would have jeopardized testing and evaluation of CMAQ, which uses the MMS output.) 

The version of MMS in the Models-3 release includes most-of the science options in NCAR's· 
current version.  The omission of upgrades should not result in substandard or corrupted 
meteorological simulations. 

The following is a summary of EPA-initiated changes that were made to the NCARrelease of · 
MMS  and its supporting software to support air quality modeling with CMAQ: 

All Programs:  . 

• 

Standardized the radius of the earth as 6370.997 kill to be consistent with chemistry and 
emission models. 

TERRAIN: 

• 

• 

Set terrain height over ocean to zero when using the I -minute terrain database. 

Improved representation of urban areas along coasts. 

INT ERP: 

• 

Increased loop indices and parameter sizes to accommodate 120-hour simulation. 

MM5: 

• 

Modified script and source code to enable analysis nudging on a one-way nested 
simulation. 

3.6 

References 

Anthes, R. A.,  1977: A cumulus parameterization schem~ utilizing a one-dimensional cloud 
model.  Mon.  Wea.  Rev._,  106, 270-286. 

Anthes, R. A., and T. T.  Warner, 1978: Development of hydrodynamic models suitable-for air 
pollution and other mesometeorological studies.  Mon.  Wea.  Rev., 106, I 045-1078. 

Arakawa, A., and V.  R.  Lamb, 1977: Computational design of the basic dynamical process of the 
UCLA general circulation model.  Methods in Computational Physics, 17, 173-265. 

,. 

-

Arakawa, A., and W. H. Schubert, _1974:  Interaction of_a 1~~ulus cloud ensemble with the_large 
scale environment.  Part I.· J..Atmos.  Sci., 31, 674-701. 

Benjamin, S. G., and N. L. Seaman, 1985:-A simple scheme for objective analysis in curved flow. 
Mon.  Wea.  Rev., 113, 1184-1198. 

Betts, A. K.,  1986: A new convective adjustment scheme:  Part I:  Observational and theoretical 
basis.  Quart. J.  Roy.  Meteor.  Soc., 112, 677-692. 

Betts, A. K., and M. J. Miller, 1986: A new convective adjustment scheme.  Part II:  Single 
column tests using GATE wave, BOMEX, ATEX and Arctic air-mass data sets.  Quart.  J.  Roy. 
Meteor.  Soc., 112, 693-709. 

Betts, A. K., and M. J. Miller, 1993: The Betts-Miller scheme.  The representation of cumulus 
convection in numerical models.  K. A. Emanuel and D. J.  Raymond, Eds., Amer. Meteor. Soc., 
246 pp. 

Blackadar, A. K., 1979: High resolution models of the planetary boundary layer.  Advances in 
Environmental Science and Engineering, 1, No. 1, Pfafflin and Ziegler, Eds., Gordon and Briech 
Sci. Publ., New York, 50-85. 

Burk, S. D., and W. T. Thompson, 1989: A vertically nested regional numerical prediction model 
with second-order closure physics.  Mon.  Wea.  Rev., 117, 2305-2324. 

Cressman, G. P., 1959: An operational objective analysis system.  Mon.  Wea.  Rev., 87, 
367-374. 

Dudhia, J.,  1989: Numerical study of convection observed during the winter monsoon experiment 
using a mesoscale two-dimensional model.  J.  Atmos. Sci., 46, 3077-3107. 

Dudhia, J., 1993: A nonhydrostatic version of the Penn State/NCAR mesoscale model: 
Validation tests and simulation of an Atlantic cyclone and cold front.  Mon.  Wea.  Rev., 121, 
1493-1513. 

Dudhia, J.,  1996: A multi-layer soil temperature model for MM5.  Preprints, The Sixth 
PSUINCAR Mesoscale Model Users' Workshop, Boulder, CO, Natl. Ctr. Atmos. Res. 

Dudhia, J., D. Gill, Y.-R. Guo, D. Hansen, K. Manning, and W.  Wang,  1998: PSU/NCAR 
mesoscale modeling system tutorial class notes (MM5 modeling system version 2).  [Available 
from the National Center for Atmospheric Research, P. 0. Box 3000, Boulder, CO  80307.] 

Fritsch, J.M., and C. F. Chappel, 1980: Numerical prediction of convectively driven mesoscale 
pressure systems.  Part I: Convective parameterization.  J.  Atmos.  Sci., 37, 1722-1733. 

Grell, G. A., Y.-H. Kuo, and R. Pasch, 1991: Semi-prognostic tests of cumulus parameterization 
schemes in the middle latitudes.  Mon.  Wea.  Rev., 119, 5-31. 

Grell, G. A., 1993: Prognostic evaluation of assumptions used by cumulus parameterizations. 
Mon.  Wea.  Rev., 121, 764-787. 

Grell, G. A., J. Dudhia, and D.R. Stauffer, 1994: A description of the fifth-generation Penn 
State/NCAR mesoscale model (MM5).  NCAR Tech. Note NCAR/TN-398+STR,  117 pp. 
[Available from the National Center for Atmospheric Research, P. 0. Box 3000, Boulder, CO 
80307.] 

Guo, Y.-R., and S.  Chen, 1994: Terrain and land use for the fifth-generation Penn State/NCAR 
mesoscale modeling system (MM5):  Program TERRATI"l.  NCAR Tech. Note, NCAR/TN-
397+IA, 119 pp.  [Available from the National Center for Atmospheric Research, P. 0. Box 
3000, Boulder, CO  80307.] 

Haagenson, P., J. Dudhia, D. Stauffer, and G. Grell,  1994: The Penn State/NCAR mesoscale 
model (MM5) source code documentation.  NCAR Tech. Note, NCAR/TN-392+STR, 200 pp. 
[Available from the National Center for Atmospheric Research, P. 0. Box 3000, Boulder, CO 
80307.] 

Hack, J.  J., B. A. Boville, B. P. Briegleb, J. T. Kiehl, P. J.  Rasch, and D. L. Williamson, 1993: 
Description of the NCAR community climate model (CCM2).  NCAR Tech. Note, NCAR/TN-
382+STR, 120 pp.  [Available from the National Center for Atmospheric Research, P. 0. Box 
3000, Boulder, CO  80307.] 

Hong, S.-Y., and H.-L. Pan, 1996: Nonlocal boundary layer vertical diffusion in a medium-range 
forecast model.  Mon.  Wea.  Rev., 124, 2322-2339. 

Hsie, E.-Y., and R.  A. Anthes, 1984: Simulations offrontogenesis in a moist atmosphere using 
alternative parameterizations of condensation and precipitation.  J.  Atmos. Sci., 41, 2701-2716. 

Janjic, Z. I.,  1994: The step-mountain eta coordinate model:  Further development of the 
convection, viscous sublayer, and tl:rrbulent closure schemes.  Mon.  Wea.  Rev., 122, 927-945. 

Kain, J.  S., and J.M. Fritsch, 1990: A one-dimensional entraining/detraining plume model. 
J.  Atmos. Sci., 47, 2784-2802. 

Kain, J.  S., and J.M. Fritsch, 1993: Convective parameterization for mesoscale models:  The 
Kain-Fritsch scheme.  The representation of cumulus in mesoscale models.  K. A. Emanuel and 
D. J.  Raymond, Eds.,  Amer. Meteor. Soc., 246 pp. 

Klemp, J.B., and R. B. Wilhelmson, 1978: Simulations of three-dimensional convective storm 
dynamics.  J.  Atmos. Sci., 35, 1070-1096 

Madala, R. V.,  1981: Finite-difference techniques for vectorized fluid dynamics calculations. 
Edited by D. L. Book, Springer Verlag, New York. 

Manning, K., and P. Haagenson, 1992: Data ingest and objective analysis for the PSU/NCAR 
modeling system:  Programs DATAGRID and RA WINS.  NCAR Tech. Note, 
NCAR!IN-396+IA, 209 pp.  [Available from the National Center for Atmospheric Research, P. 
0. Box 3000, Boulder, CO  80307.] 

Nuss, W. A., and.D. W. Titley, 1994: Use ofmultiquadric interpolation in meteorological 
objective analysis.  Mon.  Wea.  Rev., 122, 1611-1631. 

Pleim, J.E., and A. Xiu, 1995: Development and testing of a surface flux and planetary boundary 
layer model for application in mesoscale models.  J.  Appl. Meteor., 34, 16-32. 

Reisner, J., R. T. Bruintjes, and R. J. Rasmussen,  1993: Preliminary comparisons between MM5 
NCAR/Penn State model generated icing forecasts and observations.  Preprints, Fifth Intl.  Conf 
on Aviation Weather Systems, Vienna, VA, Amer. Meteor. Soc., 65-69. 

Reisner, J., R. J. Rasmussen, and R. T. Bruintjes, 1998: Explicit forecasting of supercooled liquid 
water in winter storms using the MM5 mesoscale model.  Quart. J.  Roy. Meteor.  Soc., 124B, 
1071-1107. 

Smolarkiewicz, P. K., and G. A. Grell,  1992: A class of monotone interpolation schemes. 
J.  Comp.  Phys., 101, 431-440. 

Stauffer, D.R., and N. L. Seaman, 1990: Use of four-dimensional data assimilation in a limited(cid:173)
area mesoscale model.  Part I:  Experiments with synoptic-scale data.  Mon.  Wea.  Rev., 118, 
1250-1277. 

Stauffer, D.R., N. L. Seaman, and F. S. Binkowski, 1991: Use of four-dimensional data 
assimilation in a limited-area mesoscale model.  Part II: Effects of data assimilation within the 
planetary boundary layer.  Mon.  Wea.  Rev., 119, 734-754. 

Stauffer, D. R., and N. L. Seaman, 1994: Multiscale four-dimensional data assimilation.  J.  Appl. 
Meteor., 33, 416-434. 

Tao, W.-K., and J.  Simpson, 1993: Goddard cumulus ensemble model.  Part I:  Model 
description.  Terr.,  Atmos., and Oc.  Sci., 4, 35-72. 

Zhang, D.-L., and R. A. Anthes, 1982: A high-resolution model of the planetary boundary layer -
- Sensitivity tests and comparisons with SESAME-79 data.  J.  Appl. Meteor., 21,  1594-1609. 

Zhang, D.-L., H.-R. Chang, N. L.  Seaman, T. T. Warner, and J.  M; Fritsch, 1986: A two-way 
interactive nesting procedure with variable terrain resolution.  Mon.  Wea.  Rev., 114, 1330-1339. 
