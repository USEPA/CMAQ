

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch07_analysis_tools.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch09_new_simulation.md)

<!-- END COMMENT -->

# Model Forumulation

## Numerical Approach
** >>COMMENT<< ** ICON/BCON paragraph: sounds like it is referring to using the profile mode but we actually recommend using model output from a hemispheric run.

** >>COMMENT<< ** 2nd para: I think these should be PDEs instead of ODEs - the governing eqn is a PDE though the chemistry is a set of coupled ODEs.

** >>COMMENT<< ** Suggest reworking the equation as it appears to be incomplete (e.g., cloud processes) and likely incorrect (e.g., the term: EcSc)

 ** >>COMMENT<< ** Update or remove reference to chapter 2 from old User’s Document

As the chemistry-transport model (CTM) component of CMAQ, CCTM is the final program to be run in the CMAQ modeling sequence. There are three other main programs that prepare input data for CCTM (i.e., ICON, BCON, and MCIP). Before describing each of the CMAQ programs in the [summary description section](CMAQ_OGD_ch02_overview.md#summary-descriptions-of-the-major-cmaq-programs), we present a conceptual formulation of CMAQ and Eulerian air quality modeling to provide a framework for understanding the purposes of the various programs and their relationships to each other and to the overall system.

Eulerian CTMs use coupled ordinary differential equations (ODEs) to predict changes in pollutant concentrations throughout a three-dimensional grid that is fixed in space. The following processes affect changes in the predicted concentrations in each grid cell:

-   Emissions from sources
-   Horizontal and vertical transport
-   Horizontal and vertical diffusion
-   Chemical transformations
-   Loss processes (deposition)

The Eulerian representation of the area to be modeled is a series of contiguous grid cells that form a modeling domain on a subset of the globe. CMAQ currently accounts for transport into the domain only from the horizontal (i.e., lateral) boundaries, assuming there is no exchange through the top boundary of the domain (i.e., vertical exchange). These spatial lateral boundary conditions are estimated in CMAQ using the boundary conditions processor, BCON. Similarly, a temporal boundary condition is established with the initial conditions processor, ICON, which estimates the chemical conditions in the first time step of a CMAQ model simulation. Output from these two CMAQ programs is used with output files from the emissions and meteorological models and other CMAQ preprocessors to form the required input data for running CCTM. (Note: rather than using profile mode, model output from a hemispheric run is recommended.)

## Chemical
### Photolysis
CMAQ must consider two important qualities of photolysis, which are that 1) the interactions of light with airborne molecules is wavelength-dependent, and 2) the products that are formed from these interactions are dependent on ambient conditions and properties of the molecule.

* Wavelength-dependence of light intensity:
  * Sources of light extinction and scattering in the atmosphere must be known, and the propagation of light through the atmosphere must be accounted for.
  * Sources of wavelength-dependent extinction and scattering include clouds, gases, aerosols and surface albedo, which all depend on location and time.
  * The spread of light is determined by solving a differential equation called the radiative transfer equation.
* Factors determining the products that are formed from light interactions:
  * Molecular or atomic structure determines the absorption of photons.
  * The energy required to break the chemical bonds within the compounds determines whether an absorption of photons will cause a reaction.
  * Atmospheric temperature and pressure affect a given structure and determine whether an excited compound disperses the energy from absorbed light by colliding with the other compounds, or if the absorbed energy causes the compound to rearrange or break.

Historically, chemical transport models, including CMAQ, have used pre-calculated values of photolysis rates (informed by day-specific weather data) but a new method of calculating photolysis rates inside the model itself has become standard.

**>>COMMENT:<<** Do we want to keep this link?

[CMAQv5.1 In-line Calculation of Photolysis Rates](https://www.airqualitymodeling.org/index.php/CMAQv5.1_In-line_Calculation_of_Photolysis_Rates)

* CMAQ uses atmospheric and meteorological conditions predicted within the modeling system for the scattering and extinction of light.
  * The technique couples chemical and physical processes within the modeling system.
  * The approach better emulates feedbacks between atmospheric and geophysical processes.
* With photolysis calculated inside CMAQ itself, the optical properties of the predicted atmospheric state are applied directly to the radiative calculation.
  * Predicting reflection/absorption by clouds, aerosols, and the surface all have challenges.
    * In clouds, it is difficult to predict what condensed forms of water are present (e.g. cloud droplet, rain, snow, graupel, ice crystal, etc.), how they affect light, and how light-spread changes from the distribution of clouds within the atmosphere.
    * Difficulties from aerosols come from uncertainties of their internal structure and the contribution of individual compounds to the bulk behavior.
    * The albedo of Earth's surface is difficult to determine:
      * Over dry land there is much variability in land use category and season.
      * Over marine surfaces there is much variability in temperature and wind velocity that cause changes in roughness.
* Researchers also improve the methods for solving complex radiative transfer equations. By improving accuracy, the modeling systems can better account for the feedbacks listed above.

### Multiphase Chemistry

Heterogeneous reactions in CMAQ involve both the gas and aerosol phase. It is well-known that some reactions, which happen to be very slow or impossible in the gas-phase alone, are sped up by the presence of a surface that stabilizes the reacting molecules or by the chemical environment available in a liquid phase. In the atmosphere, this surface or liquid is provided by airborne particles. Thus, dust storms and high urban particulate matter events are compelling examples of when heterogeneous chemistry is important to consider. Concentrations of gaseous pollutants like ozone can be dramatically affected by these complex pathways.

CMAQ includes several descriptions of gas-phase atmospheric chemistry (i.e. mechanisms). Hundreds of chemical compounds and about 1000 chemical reactions are included, depending on which mechanism is chosen. These mechanisms describe important phenomena like catalytic cycling of NOx and volatile organic compound (VOC) gases to form and destroy O3 and VOC oxidation with hydroxyl radicals, O3, and nitrate radicals among other applications. Because of computing constraints, mechanisms rely on lumping chemical species together into “surrogates,” which approximately represent the mass and reactivity of many species, but without having to calculate each of them separately.

* The mechanisms used to represent gas-phase chemistry in CMAQ rely on different strategies to decide how this lumping should occur.
* They are all evaluated with highly detailed measurements of chemical reactions in smog chambers and in the ambient atmosphere.
* Even though they often give very similar predictions for ozone concentrations, important differences occur during certain times of the year, during individual case studies, or for specific chemical compounds.
* For some organic systems, like oxidation products of isoprene, CMAQ includes a semi-mechanistic description that treats many compounds individually and avoids the problems of excessively lumped techniques.

Heterogeneous chemistry is often represented using an uptake coefficient, gamma, which can be converted to a rate constant. The reaction rate of a species participating in a heterogeneous reaction may be influenced by diffusion, reaction, and solubility at the surface or in bulk. Because CMAQ explicitly predicts the amount of particle surface area and volume (see next section) in each grid cell, the model can use that information to compute the rate of heterogeneous reactions of various important compounds. Products of heterogeneous reactions may be gas- or aerosol- phase species.

* CMAQ is equipped with the [SAPRC07](https://www.engr.ucr.edu/~carter/SAPRC/), Carbon Bond 6 (CB6), and Regional Atmospheric Chemistry Mechanism (RACM) mechanisms.
* Heterogeneous chemistry on or within aerosols is considered and calculated simultaneously.
* Some particle-phase reactions of semivolatile organic creates nonvolatile oligomers.
* CMAQ considers several heterogeneous reactions that are solved simultaneously with the entire gas-phase chemistry.
  * Isoprene epoxydiols (IEPOX) secondary organic aerosol (SOA): IEPOX are gas-phase isoprene oxidation products that are highly soluble and undergo acid catalyzed reactions in liquid particles. Uptake of IEPOX to form SOA is parameterized in CMAQ using an uptake coefficient that is a function of the concentration of particle-phase constituents, like acids and nucleophiles, and solubility (Henry’s law coefficient) (Pye et al., 2013 with updates in Pye et al., 2017). IEPOX SOA in CMAQ has been evaluated against organic aerosol and tracer observations during the Southern Oxidant and Aerosol Study (SOAS) (Pye et al., 2017).
  * Glyoxal + methylglyoxal SOA: In some chemical mechanisms in CMAQ (CB6r3, SAPRC07tic), glyoxal and methylglyoxal participate in heterogeneous reactions on the surface of aerosol particles. Their uptake is parameterized using an uptake coefficient of 0.0029 for glyoxal and 0.00026 for methylglyoxal (Pye et al., 2015 with updates in Pye et al., 2017). Glyoxal and methylglyoxal can also form SOA in cloud droplets.
  * N2O5 hydrolysis occurs on the surfaces of particles. This pathway is a major sink of gas-phase nitrogen oxide compounds from the atmosphere (Sarwar et al., 2012).
  * Organic nitrates can react with particulate water (hydrolysis) to generate nitric acid. CB6r3 and SAPRC07tic both include this process, but for different types of organic nitrates due to different lumping approaches. Organic nitrate hydrolysis is one process involved in determining the speciation of atmospheric nitrogen and generally acts to decrease the abundance of atmospheric nitrogen.

### Airborne Particle Microphysics
The major goal of CMAQ’s aerosol module is to comprehensively account for particles' introduction to and removal from the atmosphere, as well as the impacts atmospheric processing has on their properties along the way. Understanding these phenomena is critical for quantifying the impacts of particles on human health and the environment.

The moment-based algorithm of Binkowski and Roselle (2003) is used to estimate the size-dependence of aerosol concentrations. Each population of particles is assumed to follow a log-normal shape. CMAQ tracks the 0th (number), 2nd (surface area), and 3rd (volume) moments of three distinct populations, which are named the Aitken, Accumulation and Coarse modes.

Mass transfer of compounds between the gas and particle phases as well as particle microphysical processes, like coagulation, and new particle formation are all treated at once, while holding the composition of the gas and particle phases constant for 1-5 mins.

The direct emissions of particles from sources like fires, vehicles, power plants, dust storms and sea spray, are classified as primary emissions and contribute particularly strongly near the source of the emission. Secondary particles come from the condensation of compounds that are either emitted as vapors or are formed in the atmosphere by the chemical reaction of a parent compound that was emitted by some process. For example, sulfur dioxide (SO2) can be emitted by power plants and then oxidized in the atmosphere to form sulfuric acid (H2SO4), which condenses quickly to the surface of any preexisting particles. Generally, secondary particulate mass contributes to pollution much farther from its original emission source than primary emissions.

The effects of each aerosol microphysical process are calculated independently and in sequence, using an operator-splitting approach:

* Condensation and evaporation are treated assuming equilibrium partitioning of semivolatile species.
  * Inorganic partitioning is solved using ISORROPIA II (Fountoukis and Nenes, 2007).
  * Organic particle mass fractions are obtained assuming equilibrium descirbed by absorptive partitioning theory. Primary organic aerosol mass is now (v5.2 and later) treated with the Volatility Basis Set framework (Donahue et al., 2006). For traditional precursors, both VBS and Odum 2-Product approaches (Griffin et al., 1999) are used depending on the availability of data to constrain the parameterization.
  * The flux to each aerosol population is calculated by normalizing the total flux to the condensational sink calculated for each mode. The flux from each population (during evaporation) is normalized to the mass fraction of each component present in each population.
* Particle coagulation is approximated using Gauss-Hermite techniques to minimize computation time when accounting for integral quantities of the log-normal distributions.
* New particle formation occurs through the binary sulfuric acid-water pathway (Vehkamaki et al., 2002). Research is currently underway to incorporate state-of-the-art algorithms for new particle formation from organic compounds, ammonia, amines, and atmospheric ions.

<a id=cloud-borne></a>
### Cloud-borne Chemical Processes

Clouds affect trace atmospheric species through a number of physical and chemical processes, including:

* vertical transport (convective updrafts and downdrafts)
* the scavenging of atmospheric aerosols and gases, and subsequent chemical reactions leading to the formation of secondary species
* wet deposition (removal from the atmosphere through rainout or washout)
* altering radiative transfer and optics

Here our focus is on the second and third bullets. Additional information on [vertical transport](#cloud processes) and the [radiation/optical impacts](#cloud processes) of clouds can also be found on this website.

In CMAQ, cloud chemistry and wet scavenging are treated for both resolved and sub-grid clouds, which are clouds smaller than the model grid resolution. For species that do not participate in cloud chemistry, or if the liquid water content is below 0.01 grams per cubic meter (g m-3), scavenging and wet deposition are calculated using a simple scavenging coefficient based on effective Henry’s law coefficients at a pH of four for gases, total water content, cloud thickness, and precipitation rate. J and K mode particles are assumed to be completely activated and subject to wet scavenging and deposition, and I mode particles are considered interstitial aerosol subject to gradual scavenging by cloud droplets (Binkowski and Roselle, 2003).

If the cell liquid water content, or vertically averaged liquid water content for sub-grid clouds, exceeds a threshold value of 0.01 g m-3, the cloud chemistry module (AQCHEM, AQCHEM-KMT, or AQCHEM-KMTI) is called and the scavenging and chemistry processes are calculated together (Fahey et al., 2017). For the duration of the CMAQ synchronization time step, AQCHEM and AQCHEM-KMT(I) treat the following processes:

* Activation of Accumulation and Coarse mode particles to droplets (instantaneous)
* Mass transfer between the gas and aqueous phases
  * Henry’s Law equilibrium (AQCHEM)
  * Kinetic mass transfer (AQCHEM-KMT, AQCHEM-KMTI)
* Ionic dissociation
  * In AQCHEM, [H+] or pH is calculated at each time step, assuming electroneutrality, using a bisection method to solve the system of nonlinear algebraic equations that result from the Henry’s Law and ionic equilibrium equations
  * In AQCHEM-KMTI, [H+] is estimated based on the initially activated aerosol and then allowed to evolve dynamically
* Scavenging of interstitial aerosol in the Aitken mode by droplets
* Chemical kinetics
  * S(IV) to S(VI) conversion due to oxidation by
    * O3
    * H2O2
    * O2 (Fe3+, Mn2+ catalyzed)
    * Methylhydroperoxide (MHP)
    * Peroxyacetic Acid (PAA)
  * Parameterized in-cloud SOA formation from glyoxal and methylglyoxal
  * Optional in-cloud formation of SOA from biogenic-derived epoxides (AQCHEM-KMTI)
* First-order removal of aqueous species by rainout based on the precipitation rate, amount of cloud water, and cloud thickness

At the end of the sync time step, cloud droplets are assumed to immediately evaporate, and aqueous species are redistributed back to the gas and aerosol phases. All new chemically-generated mass is added to the accumulation (J) mode aerosol, and wet deposition amounts are updated for the CMAQ species that are active in the cloud chemistry mechanism.

## Air-Surface Exchange
### Anthropogenic sources

The NEI is a comprehensive listing by sources of six common air pollutants over the United States for a specific time interval. The NEI is created on a triennial basis: 1996, 1999, 2002, 2005, 2008, 2011, 2014, 2017, etc.

The six air pollutants are:

* Carbon monoxide
* Nitrogen oxides, a ground-level ozone precursor
* Volatile Organic Compounds (VOCs), a ground-level ozone precursor
* Particulate matter
* Sulfur dioxide
* Ammonia

Anthropogenic Sources can be further divided into seven broad sectors:

* Agriculture
* Fugitive dust
* Fuel combustion
* Industrial process
* Mobile sources
* Solvents
* Miscellaneous

For additional technical information about the NEI, visit [EPA's National Emission Inventory website](https://www.epa.gov/air-emissions-inventories/national-emissions-inventory-nei).

The general equation for emissions estimation is:

E = A x EF x (1-ER/100)

where:

* E = emissions

* A = activity rate

* EF = emission factor

* ER =overall emission reduction efficiency, %

An emissions factor is a representative value that attempts to relate the quantity of a pollutant released to the atmosphere with an activity associated with the release of that pollutant. These factors are usually expressed as the weight of pollutant divided by a unit weight, volume, distance, or duration of the activity emitting the pollutant, such as kilograms of particulate emitted per megagram of coal burned. Such factors help researchers estimate emissions from various sources of air pollution. In most cases, these factors are simply averages of all available high-quality data, and are generally assumed to be representative of long-term averages for all facilities in the source category, like a population average.

An emissions processing system, such as [SMOKE](https://www.cmascenter.org/smoke/), is used to convert an emission inventory into a format usable in an air quality model, like CMAQ.

### Biogenic and natural sources

Either the [Biogenic Emission Inventory System (BEIS)](https://www.epa.gov/air-emissions-modeling/biogenic-emission-inventory-system-beis) or the [Model of Emissions of Gases and Aerosols from Nature (MEGAN )](http://lar.wsu.edu/megan/) can be used to provide offline biogenic emissions to CMAQ. To calculate inline biogenic emissions, CMAQ uses BEIS to estimate fluxes of VOCs that are:

* The result of biological activity from land-based vegetative species
* Nitric oxide emissions that are the result of microbial activity from certain soil types

The most recent version of BEIS is version 3.61. This version of BEIS uses the [Biogenic Emission Landuse Database (BELD)](https://www.epa.gov/air-emissions-modeling/biogenic-emission-sources) version 4, which incorporates the National Land Cover Database (NLCD). Additionally, leaf temperature is calculated using a canopy model instead of two-meter temperature. The BELD was revised to incorporate land use data from the Moderate Resolution Imaging Spectroradiometer (MODIS) land product and 2011 NLCD land coverage. Vegetation species data is based on the U.S. Forest Service (USFS) Forest Inventory and Analysis (FIA) version 5.1 for years from 2002 to 2013 and U.S. Department of Agriculture (USDA) 2007 census of agriculture data.

The basic emission rate equation for biogenic emissions can be expressed as:

ERi  =  ∑j[Aj * FFj * EFij * F(S,T,others)]

where:

* ER = Emission rate of each chemical species (i), (µg) (hour-1)
* Aj  = Area of vegetation for each vegetation type (j), meters
* FFj = Foliar density factor for each vegetation type (j), (g leaf biomass) (meters-2)
* EFij = Emission factor for each chemical species (i) and vegetation type (j), (µg)
* (g leaf biomass-1) (hour-1)
* F(S,T,others) = Environmental factor accounting for solar radiation (S) and leaf temperature (T) and other meteorological variables are unitless

**Major updates in BEIS version 3.61:**

* Two-layer vegetation canopy model:
  * Dynamic sunlight and shaded layers following the PAR attenuation of Weiss and Norman (1985)
  * Integrated the photosynthetic active radiation response function in the canopy model following Niinemets et al. (2010) for both canopy layers
* New leaf temperature algorithms:
  * The temperature functions for biogenic volatile organic compound (BVOC) emissions used in BEIS and the Model of Emissions Gases and Aerosols from Nature (MEGAN) were developed from leaf temperature observations taken at the same time as the BVOC flux measurements. The Weather Research and Forecasting (WRF) Model does not explicitly estimate leaf temperature observations. The two-meter temperature has been used in BEIS, while MEGAN uses a multi-layer canopy model that estimates leaf temperature by iteratively solving for their temperature using a leaf energy balance model.
  * A simple model following the BEIS radiation model was developed to solve for the leaf temperature for both sun and shade leaves individually (i.e. two big-leaf model), adapting the energy balance in the Pennman-Montieth approximation from Campbell and Norman (1998) to solve for leaf temperature given the WRF estimated latent heat flux (Bash et al. 2016).

**Development of updated vegetation landuse data:**

The Biogenic Emission Landuse Data (BELDv4) and emission factors for herbaceous wetlands were updated to address:

* Overestimates of BVOCs at coastal sites
* Land use and vegetation species data from the 1990s (This data was replaced with higher resolution satellite data, by a factor of approximately 1000, and survey observations from 2002-2012.)

The datasets contributing to BELDv4 are:

* 2002, 2006, and 2011 National Land Cover Database (NLCD) plant functional types (CONUS) and year specific MODIS plant functional types (global)
* 2001 and 2005 USDA Census of Agriculture data
* USFS Forest Inventory Analysis (FIA) v5.1 data from 2002-2013
* NLCD data was further refined by spatially kriging FIA tree species to the respective  NLCD plant functional type constrained by the NLCD canopy coverage (Bash et al. 2016).   

### Fire sources

The general equation for emissions estimation for fire sources is:

E = A x F x EF

where:

* E = emissions (tons);
* A = area burned (acre);
* EF = emission factor (lbs/ton); and
* F=Fuel Loading (tons/acre).


### Windblown dust sources
The actual amount of dust emitted from an arid surface depends on wind speed, surface roughness, moisture content of the soil, vegetation coverage, soil type and texture, and air density.

* The main mechanism behind strong dust storms is called “saltation bombardment” or “sandblasting.” The physics of saltation include the movement of sand particles due to wind, the impact of these particles to the surface that removes part of the soil volume, and the release of smaller dust particles.
* CMAQ first calculates friction velocity at the surface of the Earth. Once this friction velocity exceeds a threshold value, saltation, or horizontal movement, flux is obtained. Finally, the vertical flux of the dust is calculated based on a sandblasting efficiency formulation – a vertical-to-horizontal dust flux ratio.
* CMAQ uses satellite information from the Moderate Resolution Imaging Spectroradiometer or MODIS to obtain realistic time-varying vegetation coverage. The model obtains the values of soil moisture and wind speed from the meteorological model, WRF.
* Using the satellite data together with a newly developed relation for the surface roughness length, the effects of solid elements, such as pebbles, and vegetation non-erodible elements in local wind acceleration, drag partitioning, and protective coverage, is formulated in a consistent manner.

### Ocean and sea spray sources

* Because sea spray particles are emitted during wave breaking and bubble bursting at the ocean surface, the main factor affecting the emission rate is the wind speed.
* The temperature of the ocean also affects bubble bursting and subsequent emission rate of sea spray particles.
* Wave breaking is enhanced near the surf zone just offshore, and CMAQ accounts for this by increasing sea spray particle emission rates in the surf zone.
* The current open ocean sea spray particle emission rate in CMAQ as described in Gantt et al. (2015) is based on Gong (2003) with a temperature dependence derived from Jaeglé et al. (2011) and Ovadnevaite et al. (2014) and an adjustment of Θ from 30 to eight to account for higher accumulation mode emissions

**>>COMMENT<<** Do we want to include this equation?
* The equation for open ocean sea spray particle emissions in CMAQ is:

![](./images/outline8-equation.png)

* The current surf zone sea spray particle emission rate in CMAQ as described in Gantt et al. (2015) is based on Kelly et al. (2010) with a reduction of the assumed surf zone width from 50 to 25 meters.
### Dry deposition

Dry deposition is determined as the product of the atmospheric concentration and the deposition velocity. The deposition velocity is modeled in CMAQ using the electrical resistance paradigm where resistances are defined along pathways from the atmosphere to the vegetation or surface and act in series and parallel. Some literature refers to "conductances" which are simply the inverse of the resistance. The deposition pathways modeled in CMAQ are shown in  [Figure 8-1](#Figure 8-1) below from Pleim and Ran, 2011.

![Figure 8-1](./images/outline8-1.png)

**Figure 8-1.Resistance Diagram** **>>COMMENT<<** Is this the correct Figure title?

Following the resistance diagram above, the deposition velocity (vd) is modeled in CMAQ as
![](./images/outline8-2.png)

The aerodynamic resistance (Ra) represents the influence of the turbulence in the surface layer and is a function of the surface characteristics and the meteorology. In the CMAQ modeling system, Ra is derived in the land-surface module of the WRF meteorological model and is passed into CMAQ. The boundary layer resistance (Rb) characterizes the movement of the pollutant due to Brownian diffusion aross the thin quasi-laminar boundary layer which is adjacent to the surface. The surface resistance (Rs) includes several sub-resistances that act in series and parallel that determine the movement of pollutants to vegetation, soil, water, and snow. The surface resistance can be determined from

![](./images/outline8-3.png)

The components of the surface resistance include the stomatal resistance (Rst), mesophyll resistance (Rm), cuticular resistance (Rw), in-canopy aerodynamic resistance (Rac) and the ground resistance (Rg).  These resistances are calculated at each time step for each chemical.

### Wet deposition

In CMAQ, clouds can be grid-scale and sub-grid (convective). The precipitation rate is used directly from the meteorological model (typically WRF) as is the location of grid-scale clouds. Sub-grid clouds are rediagnosed in CMAQ based on the precipitation rate. Scavenging, wet deposition, and below-cloud rainout/washout are modeled for sub-grid clouds. The treatment for grid-based clouds does not currently include below-cloud rainout/washout, but this is a priority research area for model improvment.

Technical details on meteorologically-related cloud processes can be found on the [Meteorological Processes](#cloud processes) page. For additional information on cloud chemistry, please refer to the cloud pages under [Chemistry Processes](#cloud-borne).


### Bidirectional exchange processes
* Currently bidirectional exchange in CMAQ is parameterized for ammonia (Bash et al. 2013, Pleim et al. 2013) and mercury (Hg) (Bash 2010).
* Sub models, for Hg, and USDA Environmental Policy Integrated Climate (EPIC) model fertilization rates and soil pH, for NH3 (Cooter et al. 2012), are used to estimate the concentrations of Hg and NH3 in the soil, vegetation, and water surfaces.  The Fertilizer Emission Scenario Tool for CMAQ (FEST-C) system is used to simulate daily fertilizer application information using the EPIC model for a defined CMAQ domain
* Fluxes, or the sum of dry deposition and emissions, are calculated based on the air-surface concentration gradient and an estimated transfer velocity similar to the dry deposition velocity.

The bidirectional exchange flux is estimated using the following equation.

Fn=Vt(Cc-Ca)

Where Fn is the net flux with positive values indicating emissions, and negative values indicating deposition. Vt is the transfer velocity between the atmosphere and the surface, Cc is the surface compensation point concentration, and Ca is the atmospheric concentration. Cc is estimated from the transport and chemical and physical sinks and sources in the surface media.

## Meteorological
### Meteorology Modeling
Meteorology modeling is based on equations for the dynamics and physics of the atmosphere. These equations are translated into computer code and applied on 3-D gridsover any domain and time duration of interest from global to local scales. Because meteorology is so critical for accurately predicting the build-up, transport, and removal of pollution, substantial effort is put into reproducing weather metrics reliably using state-of-science model evaluation methods.

The approach used to inform CMAQ meteorological processes includes both explicit partial differential equations of mass and energy conservation, as well as parameterizations that reproduce the general impacts of extremely complex processes. The equations are integrated forward in time using initial and boundary conditions, which are usually provided by a model at the global or regional scale. The equations simulate weather conditions, such as temperature, relative humidity and wind speed/direction. In retrospective applications (looking at past events), which is how CMAQ is primarily used, weather observations and analyses can be used to “nudge” the model-predicted values toward reality. This process, called four-dimensional data assimilation (see below), improves the accuracy of the weather model.

#### Generation of Meteorological Data
CMAQ uses weather simulations from the [Weather Research and Forecasting (WRF) model](https://www.mmm.ucar.edu/weather-research-and-forecasting-model).There are two ways that CMAQ uses WRF data:

##### Offline CMAQ
*  WRF and CMAQ are run sequentially.
*  WRF data is input to CMAQ, and the chemistry does not affect the weather.
* The model is useful in many applications, for example, emissions-control studies and data sharing/collaboration.

##### Coupled WRF-CMAQ
* WRF and CMAQ run simultaneously and frequently exchange data on a set interval.
* Changes in atmospheric chemistry can impact weather.
* The results are important for studying chemistry-weather interactions. For example, aerosols may influence formation of the boundary layer due to altering the radiation reaching the surface.

#### Data Assimilation
Observations and analyses are used in WRF to improve the accuracy of historical weather simulations and subsequent CMAQ simulations.

* Four-dimensional data assimilation is used to “nudge” modeled values toward individual observations and/or analyses that area  blend of observations and short-term model forecasts.
* This technique directly affects temperature, moisture, and horizontal winds. It also affects cloud cover, rainfall, and other aspects of the weather that influence air quality.




### Transport Processes


Advection and diffusion are key processes in air quality (AQ) models for accurate representation of pollutant concentrations in the atmosphere. CMAQ uses the Piecewise Parabolic Method (PPM) advection scheme developed by Colella and Woodward (1984) for horizontal and vertical advection of gasses and aerosols. Computing pollutant mixing ratios involves dividing the concentrations by the air density. However, because the meteorology model (WRF) computes air density using different numerical techniques and time steps, this creates a mass inconsistency.

Therefore, the generalized coordinate form of the air density (ρJ) is also advected in CMAQ and then used to compute the mixing ratios. To minimize divergence between CMAQ’s density and WRF’s density, the vertical mass flux and vertical wind velocity are diagnosed from the mass continuity equation using the results of the horizontal density advection to represent the horizontal mass divergence.

Diffusive transport has both horizontal and vertical components that are modeled by different schemes in different modules. The horizontal diffusion uses simple first-order eddy diffusion, where the eddy diffusivity is a combination, or harmonic mean, of a grid resolution dependent constant and the horizontal wind field deformation. The atmospheric turbulence in the vertical direction is generally much stronger in the lowest layers of the atmosphere, which are commonly referred to as the Planetary Boundary Layer (PBLHelpPBLThe troposphere can be divided into two parts:  a planetary boundary layer, PBL, extending upward from the surface to a height that ranges anywhere from 100 to 3000 m, and above it, the free atmosphere.  The boundary layer is directly influenced by the presence of the Earth's surface, responding to such factors as frictional surface drag, solar heating, and evapotranspiration.  Each of these factors generates turbulence of various-sized eddies.).

It is important that the PBL model used in the AQ and meteorology models are as similar as possible so that there are not inconsistencies between meteorological parameters such as temperature, water vapor mixing ratio, and winds, and atmospheric chemical concentrations. Therefore, the WRF-CMAQ modeling system uses the same PBL scheme for both meteorology and air quality, namely the Asymmetric Convective Model version2 (ACM2).

The ACM2 is a combined local and non-local closure PBL scheme that provides a realistic model of:

* Vertical transport by convective plumes rising from the heated ground surface in the convective boundary layer
* Small scale turbulent eddies caused by wind shear both in the PBL and above in the free troposphere (Pleim 2007a, 2007b).

### Planetary Boundary Later (PBL) and Land-Surface Module (LSM)

#### ACM2
The ACM2 (Pleim, 2007) is a combination of the ACM, which is a simple transilient model that was originally a modification of the Blackadar convective model (Blackadar, 1978), and an eddy diffusion model. Therefore, in convective conditions, the ACM2 can simulate rapid upward transport in buoyant plumes and local shear induced turbulent diffusion. The partitioning between the local and non-local transport components is derived from the fraction of non-local heat flux according to the model of Holtslag and Boville (1993). The algorithm transitions smoothly from eddy diffusion in stable conditions to the combined local and non-local transport in unstable conditions. The ACM2 is particularly well suited for consistent PBLHelpPBLThe troposphere can be divided into two parts:  a planetary boundary layer, PBL, extending upward from the surface to a height that ranges anywhere from 100 to 3000 m, and above it, the free atmosphere.  The boundary layer is directly influenced by the presence of the Earth's surface, responding to such factors as frictional surface drag, solar heating, and evapotranspiration.  Each of these factors generates turbulence of various-sized eddies. transport of atmospheric quantities, including both meteorological and chemical trace species.

#### Pleim Surface Layer
The Pleim surface layer scheme (Pleim, 2006) was developed as part of the Pleim-Xiu (PX) LSM, but it can be used with any LSM or PBL model. This scheme is based on similarity theory and includes parameterizations of a viscous sub-layer in the form of a quasi-laminar boundary layer resistance accounting for differences in the diffusivity of heat, water vapor, and trace chemical species. The surface layer similarity functions are estimated by analytical approximations from state variables.

#### Pleim-Xiu Land Surface Model
The PX LSM (Pleim and Xiu, 2003; Xiu and Pleim, 2001), originally based on the ISBA model (Noilhan and Planton, 1989), includes a 2-layer force-restore soil temperature and moisture model. The top layer is taken to be one centimeter (cm) thick, and the lower layer is 99 cm. The PX LSM features three pathways for moisture fluxes:

* Evapotranspiration
* Soil evaporation
* Evaporation from wet canopies

Evapotranspiration is controlled by bulk stomatal resistance that is dependent on:

* Root zone soil moisture
* Photosynthetically active radiation
* Air temperature
* Relative humidity at the leaf surface

Grid aggregate vegetation and soil parameters are derived from fractional coverages of land use categories and soil texture types. There are two indirect nudging schemes that correct biases in two-meter air temperature and relative humidity by dynamic adjustment of soil moisture (Pleim and Xiu, 2003) and deep soil temperature (Pleim and Gilliam, 2009).  These physics options were put into WRF (Gilliam and Pleim, 2010) when it became the primary meteorological model platform for EPA air quality modeling. Note that a small utility program (IPXWRF) can be used to propagate soil moisture and temperature between consecutive WRF runs to create a continuous simulation of these quantities. Obsgrid pre-processor is used to generate soil nudging inputs for the PX LSM.

**>>COMMENT<<** Should we include these downloadable files?
* Download the IPXWRF utility  -  IPXWRF is a .tar.gz file (16KB) containing Fortran code and a README documentation file.
* Dowload the Obsgrid utility - Obsgrid is a tar.gz file (442KB) containing Fortran code and a README documentation file.
* [Section from WRF3.7 Technical Documentation related to air quality modeling](http://www2.mmm.ucar.edu/wrf/users/docs/PX-ACM.pdf) - Description and procedures for using the Pleim-Xiu LSM, ACM2 PBL and Pleim Surface Layer Scheme in WRF including best practices and namelist options.

<a id=cloud processes></a>
### Cloud Processes
Many cloud-related processes are accounted for in CMAQ. Grid-scale and sub-grid (convective) clouds are treated separately, but undergo many of the same processes. These processes include:

* Vertical transport
* Aqueous-phase chemistry
* Wet deposition (precipitating clouds)
* Aerosol activation (grid-resolved clouds)
* Lightning NOx production

#### Grid-resolved Clouds
* Vertical transport is treated explicitly in the advection scheme.
* Aqueous-phase chemistry uses hydrometeor mixing ratios directly from meteorological model (typically WRF or MPAS).
* Scavenging and wet deposition use the precipitation rate from the meteorological model. Below-cloud scavenging currently not treated.
* In the two-way WRF-CMAQ system, aerosol indirect effects are simulated using the activation scheme of Abdul-Razzak and Ghan (2002).

#### Sub-grid Clouds
* CMAQ uses a convective precipitation rate from the meteorological model to re-diagnose sub-grid clouds using a simple entraining convective plume cloud scheme.
* Properties from the CMAQ-generated cloud (e.g., liquid water content) are passed into the scavenging, aqueous chemistry, and wet deposition routines.
* Vertical transport occurs using a non-local convective mixing model that is similar to the Asymmetrical Convective Model (ACM) used in the boundary layer.

#### Lightning Generation of NOx Gases
In retrospective applications over the continental U.S., National Lightning Detection Network (NLDN) lightning data can be used directly to generate lightning NOx in CMAQ. For real-time forecasts where lightning data are not available, lightning NOx is produced based on statistical relationships with the simulated convective rainfall rate.

Cloud processes in CMAQ are continuously updated and improved as the science progresses. Check the CMAQ release notes for details of any cloud-related updates made in the model.

#### Related Information:
**>>COMMENT<<** Should we include these downloadable files?
* Lightning NOx Production in CMAQ Part I - Using hourly NLDN lightning strike data (PPT)
* [CMAQ Release Notes](https://www.epa.gov/cmaq/cmaq-documentation#release-notes)

### Radiation
Conceptually, atmospheric radiation is divided into two broad categories:
* Incoming solar radiation consists of shorter wavelengths (ultra-violet and visible).
* Outgoing terrestrial radiation, from the Earth’s surface, is made of longer wavelengths (infrared) and less energy.

The total radiation at any time and place must account for upward and downward radiative fluxes as well as any absorption that occurs. The earth’s surface, clouds, atmospheric gases and aerosols affect both  short- and long-wave radiation because they reflect scatter and absorb radiation. Determining the fluxes requires solving equations representing the transfer of the radiation within the atmosphere.

Solving these above equations can differ between the long- and short-wavelength types, but each requires describing how the atmosphere and surface scatter and absorb radiation. Most meteorological models solve for the fluxes for one vertical column at a time, an approach that assumes that horizontal radiation transfer is not as important as vertical transfer. The mathematical methods used make approximations to reduce the computational time needed to make predictions. Popular methods include the Two- or Four-stream solutions and the Discreet Ordinate Solution.

The WRF model provides several options to solve for atmospheric radiation. The options mostly differ in the following ways:

* How cloud droplets, ice, snow, etc. affect the propagation and spatial distribution of radiation.
* How well various land-types reflect or absorb radiation and how this information is interpreted by the meteorological model.
* What numerical methods are used to solve the equations of transfer.
  * Most CMAQ applications employ the RRTMG option in WRF and MPAS (Iacono et. al, 2008).
  * In coupled WRF-CMAQ applications, aerosol optical properties derived from simulated aerosol composition and size can be fed back to the RRTM to modify radiation.
  
<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch07_analysis_tools.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch09_new_simulation.md)<br>
CMAQ User's Guide (c) 2019<br>

<!-- END COMMENT -->

