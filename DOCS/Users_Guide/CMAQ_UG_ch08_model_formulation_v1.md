
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

### Cloud-borne Chemical Processes

Clouds affect trace atmospheric species through a number of physical and chemical processes, including:

* vertical transport (convective updrafts and downdrafts)
* the scavenging of atmospheric aerosols and gases, and subsequent chemical reactions leading to the formation of secondary species
* wet deposition (removal from the atmosphere through rainout or washout)
* altering radiative transfer and optics

Here our focus is on the second and third bullets. Additional information on vertical transport and the radiation/optical impacts of clouds can also be found on this website.

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
### Ocean and sea spray sources
### Dry deposition
### Wet deposition
### Bidirectional exchange processes


## Meteorological
### Meteorology Modeling
### Transport Processes
### Planetary Boundary Later (PBL) and Land-Surface Module (LSM)
### Cloud Processes
### Radiation

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch07_analysis_tools.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch09_new_simulation.md)<br>
CMAQ User's Guide (c) 2019<br>

<!-- END COMMENT -->
