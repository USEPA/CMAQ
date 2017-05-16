Chapter 14
================

PHOTOLYSIS RA TES FOR CMAQ 

Shawn J. Roselle," Kenneth L. Schere,"* and Jonathan E. Pleim ·· 

Atmospheric Modeling Division 

National Exposure Research Laboratory 
U.S. Environmental Protection Agency 

Research Triangle Park, North Carolina 27711 

Adel F. Hanna 

MCNC-North Carolina Supercomputing Center 
Research Triangle Park, North Carolina 27709 

ABSTRACT 

The method for calculating photodissociation reaction rates (photolysis rates) for CMAQ is 
described in this chapter.  The description includes the photolysis rate preprocessor (JPROC) and 
CMAQ subroutine PHOT.  JPROC produces a clear-sky photolysis rate look-up table.  The 
look-up table consists of photolysis rates at various altitudes, latitudes, and hour angles.  The 
look-up table is recalculated for each simulation day and is dependent upon the chemical 
mechanism.  Within CMAQ, photolysis rates for individual grid cells are interpolated from the 
look-up table in subroutine PHOT.  PHOT also uses a parameterization to correct the clear-sky 
photolysis rates for cloud cover. 

·on assignment from  the National Oceanic and Atmospheric Administration, U.S. Department of Commerce. 
Corresponding author address:  Shawn Roselle,  MD-80, Research Triangle Park, NC 27711. E-mail: 
sjr@hpcc.epa.gov  . 

.. On assignment from  the National Oceanic and Atmospheric Administration, U.S. Department of Commerce. 


14.0  PHOTOLYSIS RATES FOR CMAQ 

,, 

'lllllllllllllh 

14!1  Background 

Many chemical reactions in the atmosphere are initiated by the photodissociation of numerous 
tn,J,.~e gases.  These photodissociative reactions are responsible for most of the smog buildup 
detrimental to humans, animals, plant life and materials.  In order to accurately model and predict 
the effects of air pollution, good photodissociation reaction rate (or photolysis rate) estimates 
must be made. 

Photodissociation is the conversion of solar radiation into chemical energy to activate and 
dissociate chemical species.  Examples of species that photodissociate include many important 
trace constituentsof the troposphere such as N02,  0 3,  HCHO, CH3CHO, HONO, the N03 
radical, and I-1;02 (8.lso see Table 14.1).  The simulation accuracy of the entire chemical system is 
highly dependent upon the accuracy of photolysis rates, which are the primary sources of radicals 
i11111111~~ !f9posp~ere.  Photolysis rates (min-1), sometimes called J-values, are computed for 
photodissociation reaction (i) by 

Ai 

J;  = f F(A.)ap.)<f>,(A.)dA. 

A.1 

(14-1) 

11111111111111• 

,,11111'"'1111 

,., 

,, 

' 

,, 

"' 

"' 

), al.Ii.) the absorption cross section for the 
), <Ml..)  the quantum yield of the 

where, F(A.) is the actinic flux (photons cm·2  min· 1 nm· 1
molecule undergoing photodissociation (cm2  molecu1e- 1
), and I..  the wavelength (nm).  Absorption cross sections 
photolysis reaction (molecules photon- 1
and quantum yields are functions of wavelength, and may also be functions of temperature and 
pressure~ they are unique to species and reactions.  Laboratory experiments measuring the 
absorption cross sections and quantum yields. have been conducted for many species that 
photodissociate in the troposphere.  Actinic flux is a radiometric quantity that measures the 
spectral radiance integrated over all solid angles per unit area.  The spherical receiving surface 
distinguishes the actinic flux from the more commonly measured irradiance, which is the radiance 
falling on a horizontal surface.  Thus, the actinic flux can be called spherical spectral irradiance. 
The actinic flux changes with time of day, longitude, latitude, altitude, and season, and is 
governed by the astronomical and geometrical relationships between the sun and the earth.  It is 
greatly affected by the earth's surface albedo as well as by various atmospheric scatterers and 
absorbers.  Hence, correct model calculation of the temporal and spatial variation of the actinic 
flux is critical to obtaining accurate photolysis rates for regional and mesoscale episodic 
photochemical modeling. 

The current approach taken for setting photolysis rates in the Models-3/CMAQ framework 
follows that of the Regional Acid Deposition Model (RADM) (Chang et al., 1987).  It includes 
two stages of processing: (1) a table of clear-sky photolysis rates is calculated for specified 
heights, latitudes, and hours from  local noon; and (2) photolysis rates are interpolated from the 
table within the CMAQ Chemistry Transport Model (CCTM) based on grid cell location and the 
model time, and are corrected for cloud cover.· This approach is computationally efficient and has 
been shown by Madronich (1987) to give clear-sky photolysis rates within the uncertainty of the 
surface-based measurements. 

14.2  Preprocessor JPROC: Calculate Clear-sky Photolysis Rate Table 

Preprocessor JPROC calculates a table of clear-sky photolysis rates (or J-values) for a specific 
date.  The table is dimensioned by latitude, altitude, and time.  Currently, J-values are calculated 
for 6 latitudinal bands (l0°N, 20°N, 30°N, 40°N, 50°N, and 60°N), 7 altitudes (0 km,  1 km, 2 
km, 3 km, 4 km, 5 km, and  10 km), and ±9 hours from local noon (0 h,  1 h, 2 h, 3 h, 4 h, 5 h, 6 h, 
7 h,  and 8 h).  There is a separate table for each photolysis reaction.  In order to compute the 
photolysis rates using Equation 14.1, the actinic flux, absorption cross section, and quantum yield 
must be determined as a function of wavelength. 

The delta-Eddington two-stream radiative transfer model (Joseph et al.,  1976; Toon et al.,  1989) 
is used for computing the actinic flux.  The two-stream approximations are limited in application 
to cases where the scatter is  not highly anisotropic.  In computing the actinic flux, a description of 
the extraterrestrial radiation, aerosol, ozone absorption, oxygen absorption in the 
Schumann-Runge Bands, Rayleigh scattering (WMO,  1985) and surface albedo are provided to 
the radiation model. 

Extraterrestrial radiation is specified by a user input file.  JPROC is flexible enough to use any 
extraterrestrial radiation data distribution specified by the user.  However, the wavelength 
distribution of the extraterrestrial radiation data is important because this is also the distribution 
that will be used in the integration of Equation 14.1.  Therefore, the user should choose a 
wavelength distribution that resolves the features that are important to the photolysis reactions of 
interest.  A modified WMO extraterrestrial radiation data distribution (Chang, et al.,  1990) is used 
as input to JPROC, which has a variable wavelength resolution ranging from  1 nm to  10 nm. 

The 0 2 and 0 3  absorption cross section data are specified by user input files, but it is 
recommended that the most recent NASA data (DeMore et al.,  1994) be used in the calculations. 
Vertical ozone profiles are set by interpolating seasonal profiles from a user input file, and if total 
ozone column data are available (such as data measured by the Total Ozone Mapping 
Spectrometer (TOMS) instrument aboard the sun-synchronous polar orbiting Nimbus satellite), 
then the interpolated vertical profiles are uniformly rescaled so that the profiles integrated total 
ozone column value matches the measured total ozone column data.  TOMS data are archived 
and available at the National Satellite Service Data Center (NSSDC) in the form of digital daily 
maps with a resolution of 1 degree latitude by  1.25 degrees longitude.  The TOMS data are 
averaged over each latitudinal band in JPROC.  Nimbus-7 TOMS data are available for years 
1978 through 1993; Meteor-3 TOMS dataare available for  1991-1994; ADEOS TOMS dataare 
available for  1996-1997; and Earth Probe TOMS data are available for  1996-1998.  These data 
can be downloaded from site http://jwocky.gsfc.nasa.gov. 

The albedo data given by Demerjian et al.  ( 1980), which have been used extensively in radiative 
transfer models, are given as a function of wavelength and are used in the current version of 
JPROC.  Currently, a single vertical profile of aerosol attenuation coefficients (Elterman,  1968) is 
used in JPROC.  An effort is underway to  incorporate predicted aerosol parameters from CMAQ 
i~'i'o the photoiysi's'rate calculations. 

· 

···· 

···· 

111'"' 

l!::::,,11111. 

llllli111l1111 

·! 

"" 

" 

,, 

.,,  r  ·' 

";;;::111111;:!: 

·11111::1111111111:" 

,,, 

. 

.,. 

. 

... ,, 

,,111111, .. :111:" 

~::· 

1
·
;,,,:::::

1111111111

'' 

'" 

•1"::111 

::,,,t:11111: 

" 

' 

' 

'" 

.: 

" 

" 

' 

"' 

Several factors in JPROC depend on the vertical profiles of temperature and pressure, including 
ozone absorption and the absorption cross sections and quantum yields for individual photolysis 
reactions.  JPROC interpolates seasonal profiles of temperature and pressure.  We use the same 
vertical profile data that are used in the RADM (Chang et al.,  1987). 

,,. 

''"  · 

"· 

',,,1'' 

,,,,,. 

·, 

, 

11111 

1: 

. 

, 

1
"' 

1111 

' 

111111' 

"·111!!!1:::' 

11,,,11111 

''I 

,,I 

' 

Absorption cross section and quantum yield data are specified by the user through input files. 
Input files depend on the chemical mechanism selected by the user in the Program Control 

...  Prnc~ssor (see Chapter 15).  The original sets of cross section/quantum yield data published with 

the CarbQn Bond Mechanism IV (Gery et al.,  1989), the Regional Acid Deposition Model 
mechanism version 2 (RADM2) (Stockwell et al.,  1990), and the SAPRC mechanism (Carter, 
1990) are available for use in JPROC.  However, users can deviate from these standard sets to 
test other data, including the revisions suggested by NASA (DeMore et al.,  1994).  Table  14.1 
lists the photolysis reactions for the RADM2 mechanism. 

14.3  Subroutine PHOT: Table Interpolation and Cloud Attenuation 

shbroutine PHOT within the CCTM has two basic functions,  including intel1Jolation of the clear(cid:173)
sky photolysis rate table and application of a cloud correction factor to the clear-sky values.  The 
interpofation step is fairly simple.  For each grid cell, the latitude, height, and time from local noon 
are determined and used to interpolate clear-sky values from the photolysis rate table.  The cloud 
co'rrcction step is a little more complicated.  The method used to correct for cloud cover in PHOT 
was taken from RADM (Chang et al.,  1987; Madronich,  1987).  The correction of clear-sky 
values depends on whether the location is below, above, or within the cloud.  The below cloud 
photolysis rate ( J bdow)  is calculated as: 

where cfrac is the cloud coverage fraction (cloud fraction is interpolated from hourly data for 
each grid cell), 8  is the zenith angle, and tr is the cloud transmissivity.  Below cloud photolysis 
rates will be lower than the clear-sky values due to the reduced transmission of radiation through 
the cloud.  The cloud transmissivity is calculated by: 

(14-2) 

(14-3) 

where/is the scattering phase function asymmetry factor (assumed to be 0.86) and 'tc1d  is the 
cloud optical depth.  We have replaced the cloud optical depth equation in RADM with one taken 
from Stephens (1978).  The original RADM equation for  't'cid  required an estimate of the cloud 
droplet radius, which is not readily available.  In RADM, the cloud droplet radius was assumed to 
be I Oµm.  The empirical formula for 't'cid  from Stephens ( 1978): 

log('rc1)  = 0.2633 + l.7095 In[ log( W)] 

(14-4) 

is only a function of liquid water path (W), where W=L.6.z (g/m2
(g/m3

), and .6.z is the cloud thickness.  The above cloud top factor (F J is calculated as: 

), L is the liquid water content 

· 

(14-5) 

This equation allows for enhancement of photolysis rates above the cloud due to the reflected 
radiation from the cloud.  It also includes a reaction dependent coefficient ( aJ which allows for 
further above cloud enhancements (Chang et al., 1987).  Within the cloud, the cloud correction 
factor is a simple linear interpolation of the below cloud factor at cloud base to the above cloud 
factor at cloud top.  Once computed, the below, above, and within cloud factor are used to scale 
the clear sky photolysis rates to account for the presence of clouds.  In the current 
implementation, all cloud types (including clouds composed of ice crystals) are treated the same 
using the above outlined procedure. 

14.4  Summary 

The current method for calculating photolysis rates in CMAQ, which was derived from RADM, 
uses a preprocessor to compute a look-up table and a subroutine within the chemistry transport 
model to interpolate J-values and apply a cloud-cover correction.  Other approaches and 
enhancements are being developed and tested.  One enhancement to be added in the future is the 
dynamic link between aerosol predictions and photolysis rate calculations.  Another is to replace 
the two-stream model with a more comprehensive multi-stream radiative transfer model (Stamnes 
et al.,  1988).  Other absorption cross section and quantum yield data will be added or updated 
using the DeMore et al.  (1997) revisions. 

14.5  References 

Carter, W.P .L., 1990: A detailed mechanism for the gas-phase atmospheric reactions of organic 
compounds, Atmos.  Environ., 24(A), 481-518. 

Chang, J.S., R.A. Brost, LS.A. Isaksen, S.  Madronich, P.  Middleton, W.R. Stockwell, and C.J. 
Walcek, 1987: A three-dimensional eulerian acid deposition model:  physical concepts and 
formulation, J.. Geophys.  Res.  92 (Dl2):  14681-14 700. 

11

' 

1111!

,. 

"•11,:iiiif 

::111 

., 

' ' •  

111

!111!!

" 

1
1' 

'

w·:·;11,,,1llli:"1!ll 

"11111111111' 

"!11'1 

Chang, J.S., P.B. Middleton, W.R. Stockwell, C.J.  Walcek, J.E. Pleim, H.H. Lansford, F.S. 
Biiikowski, S. Madronich, N.L. Seaman, D.R. Stauffer, D.  Byun, J.N.  McHenry, P.J.  Samson, H. 
Hass,  1990: The regional acid deposition model and engineering model, Acidic Deposition: State 
o(Scicnce and ...  Tectmology,  Report 4, National Acid Precipitation Assessment Program. 

, 

• 

111111111 

, 

!iii1:1111lll 

.;;;1""1: 

'"il'''l;!!!i11, 

I 

Demerjian, K.L., K.L. Schere, J.T. Peterson,  1980: Theoretical estimates of actinic (spherically 
int'egrated) flux and photolytic rate constants of atmospheric species in the lower troposphere. 
Advances in Environmental Science and Technology, Vol.  10, by John Wiley &  Sons, Inc., 
369-459. 

. 

,' 
1

'' 

1111 

,.,: 

I 

DeMore, W.B., S.P. Sander, D.M. Golden, R.F.  Hampson, M.J.  Kurylo, C.J.  Howard, A.R. 
Ravishankara, C.E. Kolb, and M.J. Molina, 1994: Chemical Kinetics and Photochemical Data for 
Use in Stratospheric Modeling: Evaluation Number 11, JPL Pub. 94-26. Pasadena, CA:  National 
Aeronautics and Space Administration, Jet Propulsion Laboratory. 

:li1111111: 

1111, 
:: 
11

,, 

.,:·1':;,.,:;1. 
ll"'i, 
,,·:1;, 

·'' 
'.111, 
':1111 

. 

D~More, W.B:,,~, 87~. Sander, D.M. Golden, R.F. Hampson, M.J. Kurylo, C.J. Howard, A.R. 
Ravishankara, C.E. Kolb, and M.J. Molina,  1997: Chemical Kinetics and Photochemical Data for 
Use in Stratospheric Modeling: Evaluation Number 12, JPL Pub. 97-4. Pasadena, CA:  National 
Aeronautics and Space Administration, Jet Propulsion Laboratory. 

Elterman, L,  1968: UV, Visible, and IR Attenuation for Altitudes to 50 km; AFCRL-68-0153. 
Bedford, MA: Air Force Cambridge Res. Lab. 

Gery, M.W., G.Z. Whitten, J.P. Killus, M.C. Dodge, 1989:  A photochemical mechanism for 
urban and regional scale computer modeling, J.  Geophys.  Res., 94,  12925-12956. 

Joseph, J.H., W.J. Wiscombe, and J.A. Weinman,  1976: The delta-Eddington approximation for 
radiative flux transfer, J.  Atmos. Sci., 33, 2452-2459. 

Mndronich, S,  1987: Photodissociation in the Atmosphere:  1.  Actinic flux and the effects of 
ground reflections and clouds, J.  Geophys.  Res.  92 (08): 9740-9752. 

Stamnes, K., S. Tsay, W.  Wiscombe, and K.  Jayaweera,  1988: Nwnerically stable algorithm for 
discrete-ordinate-method radiative transfer in multiple scattering and emitting layered media, 
Appl.  Opt., 27, 2502-2509. 

Stephens, G.L.,  1978: Radiation profiles in extended water clouds. II.:  Parameterization schemes, 
J.  Atmos. Sci.  35:'2123-2132. 

Stockwell, W.R., P.  Middleton, and J.S. Chang,  1990: The second generation regional acid 
deposition model chemical mechanism for regional air quality modeling, J.  Geophys.  Res.  95 
(DlO):  16343-16367. 

Toon, O.B., C.P. Mckay, and T.P. Ackerman, 1989: Rapid calculation ofradiative heating rates 
and photodissociation rates in inhomogeneous multiple scattering atmospheres, J.  Geophys.  Res., 
94,  16287-16301. 

World Meteorological Organization,  1986:  "Atmospheric Ozone 1985:  Assessment of Our 
Understanding of the Processes Controlling its Present Distribution and Change"; WMO Rep. No. 
16; Global Ozone Research and Monitoring Project, Geneva, Switzerland. 

Table 14-1.  RADM2 Photolysis Reactions (Adapted from Stockwell et al.,  1990.) 

' 

" 

""'1'"11''11111'' 

Description 
Ozone Photolysis to 0 1D 
Ozone Photolysis to 0 3P 
Nitrogen Dioxide Photolysis 
Nitrate Photolysis to NO 
Nitrate Photolysis to N02 
Nitrous Acid Photolysis 
Nitric Acid Photolysis 
Pernitric Acid Photolysis 
Hydrogen Peroxide Photolysis 
Formaldehyde Photolysis to Radicals 
Formaldehyde Photolysis to Molecular 
Hydrogen 
Acetaidehyde Photolysis 
Acetone Photolysis 
Methyl Ethyl Ketone Photolysis 
Glyoxal Photolysis to Formaldehyde 
Glyox~ Ph~'tolysis to Molecular 
Hydrogen 
Methyl Glyoxal Photolysis 
Unsaturated Dicarbonyl Photolysis 

Methyl Hydrogen Peroxide Photolysis 
Organic Nitrate Photolysis 
Acrolein Photolysis 

Reaction 

-

t 

• 

........... 

0 3 +hv  - 0 2 +0 1D 
0 3 +hv  - 0 2 +03P 
3 
.... 
N02 +hv  -NO+O P 
N03 + hv  ....  NO + 0 2 
'Nbl + hv  - N02 + 0 3P 
HONO+hv  :.;.  OH+NO 
H;N03 + hv  - OH+ N02 
HN04  + hv  ....  H02 + N02 
H20 2 +hv  - OH+OH 
HCHO+hv  -H+HCO 

~~ 

r 

HCHO+hv  -H2 +CO 
CH3CHO + hv (+202)  - CH300 + H02 +CO 
CH3COCH3 +"hv  '...  CH3 + CH3CO 
CH3COC2Hs + hv  ....  AC03 + ETH 
I-icocHo + fiv  .:.  HcHo +co 
HCOCHO + hv  - 2CO + H2 

:·· 

..•. 

111.. 

"" 

. •. 

1': 

CH3COCHO + hv  - AC03 + H02 +CO 
HCOCH=CHCHO + hv  - 0.98H02 + TC03 + 
0.02AC03 
CH300H + hv  - CH20  + OH+ H02 
CH30N02 + hv  - 0.2ALD + 0.8KET + H02 + N02 
C,H,O + hv - products 
