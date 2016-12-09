Overview of the Science in the CMAQ Modeling System
===================================================

As discussed in [Section 3](#Introduction "wikilink"), CMAQ is a multipollutant, multiscale air quality modeling system that estimates the transport and chemistry of ozone, PM, toxic airborne pollutants (referred to as “air toxics”), visibility, and acidic and nutrient pollutant species. CMAQ uses state-of-the-science techniques and has many new and important features that are not available in previous modeling systems. It can model complex atmospheric processes affecting transformation, transport, and deposition of air pollutants using a system architecture that is designed for fast and efficient computing.

CMAQ has been developed to meet the needs of both the research and application communities. The CMAQ system allows users to easily construct model variations with different characteristics, such as different chemical mechanisms or alternative cloud treatments, in order to address a specific air quality issue (illustrated schematically in [Figure 2-1](./images/Figure2-1)). This modeling configuration allows CMAQ to retain its state-of-the-science status over time because it facilitates the implementation of new science modules as appropriate.

<span id=Figure2-1></span>

<center>
![](Figure2-1.png "Figure2-1.png")

</center>
<center>
**Figure 2‑1. CMAQ Modeling Framework**

</center>
CMAQ can be employed for regulatory applications by using approved standard configurations of the modeling platform that represent the best available modeling technology at a given time. At the same time, the CMAQ modeling system is also a useful tool for the model developer. It is unique in that its components are designed in a flexible, modular fashion with a user interface; model developers can use these design features to create complex modeling situations and scenarios, or to develop entirely new models using a standardized coding framework. Model developers can also perform sensitivity analyses on newly developed modules and perform comparisons with existing systems.

This section describes features of the modeling system that create its adaptability and flexibility ([Section 4.0](#Overview_of_the_Science_in_the_CMAQ_Modeling_System "wikilink")). [Section 4.2](#CMAQ_Input_Processors "wikilink") and [Section 4.3](#CMAQ_Chemistry-Transport_Model_Science_Modules "wikilink") then provides a brief descriptions of the key air quality modeling science features in various components of the CMAQ system, including MCIP, ICON, BCON, JPROC, CHEMMECH, PROCAN, and CCTM. More detailed discussions on these features can be found in Byun and Ching (1999) and Byun and Schere (2006). Finally, [Section 4.4](#The_CMAQ_User_Interface "wikilink") discusses the CMAQ user interface for building and running CMAQ.

Features Implemented to Achieve the Goals of CMAQ
-------------------------------------------------

As noted previously, early air quality model development resulted in separate air quality models that addressed single pollutants or issues, such as ozone or acid deposition. These models had little or no flexibility to be updated with advances in science or to accommodate new regulations. CMAQ was therefore designed to have more adaptability and flexibility for different applications and for changing or improving the modeling methodology. Within the context of the model’s science, the following subsections discuss CMAQ’s design in terms of (1) accommodating multiple pollutants and multiple scales, (2) providing flexibility through modularity, and (3) reducing the potential for model simulation error.

As a community model, CMAQ is able to leverage the expertise of model developers in many areas of atmospheric science. This facilitates improving and enhancing the CMAQ modeling system as the state-of-the-science evolves. For example, linkages of CMAQ with exposure modeling, hydrological modeling, climate modeling, and multimedia modeling are currently being explored.

### Multiple pollutants and multiple scales

With its “one-atmosphere” design, which allows modelers to address the complex interactions among multiple pollutants/air quality issues simultaneously, CMAQ is a dramatic improvement over the earlier, single-pollutant models. The CMAQ system provides state-of-the-science capabilities for modeling multiple air quality pollutants/issues in a single simulation, including tropospheric ozone, PM, air toxics, visibility, and acidic and nutrient pollutant species. The “one-atmosphere” approach is important because the various chemical species interact. For example, ozone and hydroxyl radicals react with emitted species such as anthropogenic and biogenic organics to generate secondary PM species. These PM species can interact with solar radiation to alter photolysis rates. The failure of the early, simplistic approach of trying to account for the chemistry of sulfur dioxide from power plants without also treating ozone and hydroxyl radicals demonstrated the need for the “one-atmosphere’ approach.

The multiple spatial scale (multiscale) capabilities of CMAQ enable applications from local to hemispheric scales. By combining this multiscale feature with the temporal flexibility of the model, users can perform simulations to evaluate annual and interannual pollutant climatology, as well as shorter-term transport from localized sources. To implement multiscale capabilities in CMAQ, several different issues have been addressed, such as scalable atmospheric dynamics and generalized coordinates that depend on the desired model resolution. Meteorological models may assume hydrostatic conditions for large regional scales, where the atmosphere is assumed to have a balance of vertical pressure and gravitational forces with no net vertical acceleration on larger scales. However, on smaller scales such as urban scales, the hydrostatic assumption cannot be made. A set of governing equations for compressible nonhydrostatic atmospheres is available to better resolve atmospheric dynamics at smaller scales; these are more appropriate for finer-regional-scale and urban-scale meteorology. CMAQ’s generalized coordinate system is used so that meteorological fields on different vertical coordinates can easily be accommodated and multiple scales can be simulated with the same CTM. The Jacobian matrix used by the generalized coordinate system controls the necessary grid and coordinate transformations (consult Byun, 1999).

### Modular flexibility

CMAQ’s current coding structure is based on a modularity level that distinguishes from each other CCTM’s main driver, science modules, data estimation modules, and control/utility subroutines. Also distinguished from each other are the science models (including submodels for meteorology, emissions, chemistry-transport modeling) and the analysis and visualization subsystems.

In CCTM, the process modules that affect the pollutant concentration fields are classified as listed below. Each bullet contains a description of the process followed by *module name* in parentheses. (These modules (except for gencoor) are discussed further later in this section.)

Science Modules:

-   Horizontal advection (*hadv*)

-   Vertical advection (*vadv*)

-   Mass conservation adjustments for advection processes (*adjc*)

-   Horizontal diffusion (*hdiff*)

-   Vertical diffusion (*vdiff*)

-   Gas-phase chemical reaction solver (*chem*)

-   Aqueous-phase reactions and cloud mixing (*cloud*)

-   Aerosol dynamics and size distributions (*aero*)

Control/Utility Modules:

-   Model data flow and synchronizing of fractional time steps (*driver*)

-   Model horizontal grid system (*grid*)

-   Unit conversion (*cpl*)

-   Initialization (*init*)

-   CGRID configuration (*cgrds*)

-   Process analysis (*pa*)

Data Estimation Modules:

-   Deposition velocity estimation (*depv*)

-   Photolytic rate computation (*phot*)

In-line Emissions Modules:

-   Calculate emissions (biogenics, dust, lightning, sea salt, plume rise) in-line (*emis*)
-   In-line BEIS3 biogenic emissions (*biog*)
-   In-line plume rise (*plmrs*)

This modularity makes it easier to modify or introduce a specific scientific process in CCTM. For example, the *chem* module contains several options for different gas-phase chemistry solvers that can be used to optimize model performance. Without the modular structure, changes to just one scientific process could entail having to modify source code throughout CCTM, thereby greatly increasing the risk of human error. In this situation, failing to make the correct modification at even a single location in the source code could lead to erroneous modeling results or program execution failure. CMAQ’s modularity makes designing or modifying a model simulation much more user friendly, requiring either less reprogramming or none at all.

### Major quality control features

The CMAQ system was designed to minimize the potential for model simulation error in several significant ways:

The formal CMAQ peer review process implemented by the EPA ensures that the model retains scientific credibility. Also, informal “review” of the modeling system occurs day-to-day as the broad international user community applies CMAQ for a wide variety of scientific questions and in locations other than North America.

The modularity of the scientific processes in CMAQ makes modifications and adaptations to the user’s needs more straightforward. The potential for error is minimized because the user is not required to change code or declare variables within program modules outside the one of immediate interest.

Another CMAQ feature that supports quality control is process analysis, which (as noted earlier) is a technique used to trace the source(s) of a chemical species within a simulation. Process analysis is discussed in [Section 4.2.5](#PROCAN:_Process-analysis_preprocessor "wikilink").

CMAQ Input Processors
---------------------

CCTM uses data from other models and CMAQ input processing programs as input for model simulations (Figure 2-2).

<span id=Figure2-2></span>

<center>
![](Figure2-2.png "Figure2-2.png")

</center>
<center>
**Figure 2-2. CMAQ Chemistry-Transport Model (CCTM) and input processors**

</center>
The input data for CCTM are developed using the four input processors shown in grey in [Figure 2-2](#Figure2-2 "wikilink"). All of the CMAQ programs shown in [Figure 2-2](#Figure2-2 "wikilink") (bordered by the broken line) require five basic configuration options:

-   Case – a unique character string that identifies the simulation
-   Grid (Domain and size) – a definition of the horizontal modeling grid that includes the location relative to a fixed map projection and the size of the domain
-   Projection –defines a horizontal plane on the spherical surface of the earth, used to specify the general location of the modeling grid on the globe
-   Vertical Structure – a definition of the layer boundaries for the vertical grid
-   Chemical Mechanism – the name of the photochemical mechanism, aerosol chemistry mechanism, and aqueous chemistry mechanism used for the CMAQ simulation

The choices for these options and how they are selected for each of the CMAQ programs are detailed in [Section 7.3](#CCTM "wikilink").

CMAQ uses the MCIP processor to prepare the meteorological fields for CCTM. The ICON and BCON processors generate the initial and boundary conditions for a CCTM simulation. JPROC computes the photolysis rates that are used when simulating photochemical reactions in CCTM. Emissions for CMAQ must be prepared with a modeling system that generates emissions for direct input to CCTM (currently, SMOKE or CONCEPT). Brief descriptions of the various CMAQ input processors are presented in this section. Also described are two processors (CHEMMECH and PROCAN) not shown in [Figure 2.1](#Figure2-1 "wikilink").

### MCIP: Meteorology-Chemistry Interface Processor

MCIP uses output files from a meteorological model (such as MM5) to create netCDF-formatted input meteorology files that are used by the emissions model that computes emissions inputs to CMAQ, and by CCTM within CMAQ ([Figure 2-3](#Figure2-3 "wikilink")). (The files created by MCIP are not listed individually here.)

<span id=Figure2-3></span>

<center>
![](Figure2-3.png "Figure2-3.png")

</center>
<center>
**Figure 2-3. Meteorology preprocessing for CMAQ with MCIP**

</center>
Using output fields from the meteorological model, MCIP performs the following functions:

-   Extracts meteorological model output that is specific to the CCTM horizontal grid domain. CCTM uses a smaller computational domain that is entirely contained within the computational domain of the meteorological model, and the lateral boundaries from the meteorological model are generally not used by CCTM.
-   Processes all required meteorological fields for CCTM and the emissions model. The meteorological information, such as atmospheric temperature, pressure, humidity, and winds produced by the meteorological model, is put into a form required by CMAQ.
    -   “Collapses” meteorological model fields, if coarser vertical resolution data are desired for a given CCTM run. (*This is not a recommended procedure*). To do this, MCIP uses mass-weighted averaging on higher-vertical-resolution meteorological model output.
    -   Computes cloud top, cloud base, liquid water content, and cloud coverage for cumuliform clouds using simple convective schemes. The cloud parameters influence CCTM aqueous-phase chemistry and cloud mixing, as discussed in [Section 4.3.5](#Clouds_and_aqueous-phase_chemistry "wikilink") (Walcek and Taylor, 1986; Chang et al., 1987). First, the cloud base is determined as the lifting condensation level computed from the highest saturated equivalent potential temperature below 700 mb. Then, the cloud top is computed by following a saturated adiabatic lapse rate from cloud base until it reaches a temperature five degrees cooler than the surrounding environment. Once the top and bottom of the cloud are determined, MCIP constructs a vertical profile of the adiabatic liquid water mixing ratio as the difference between the saturated mixing ratio at each level and the source-level mixing ratio. MCIP obtains the cloud coverage fraction by iteratively solving the equations governing the conservation of total water mass and energy conservation for cloud-top mixing, commensurate with the temperature profile.
    -   Outputs meteorological and geophysical files in the netCDF format, for input to SMOKE and CMAQ.

### ICON and BCON: The initial and boundary conditions processors

To perform air quality simulations, both initial and boundary conditions are required. Initial conditions (calculated in ICON) are needed to provide concentrations of individual chemical species for the first time step throughout the modeling domain. Boundary conditions (calculated in BCON) are needed to provide concentrations of individual chemical species at the lateral boundaries of the modeling domain. In a single run of each processor, ICON and BCON can generate these concentrations for all the chemical species required by CMAQ.. These processors require two inputs ([Figure 2‑4](#Figure2-4 "wikilink")): a concentration file for the chemical species to be simulated, and the chemical mechanism.

***Concentration file:*** The concentration file used in ICON and BCON can come from one of two sources:

-   A time-independent set of vertical concentration profiles that are dependent upon the chemical mechanism being used. This approach is usually taken when no other information about the initial and boundary concentrations is available. CMAQv5 is distributed with IC and BC profiles for the CB05, SAPRC-07T, and SAPRC-99 photochemical mechanisms and the CMAQ AERO6 aerosol module. These files are set at the four boundaries (north, east, south, west) of the computational grid and are thus fixed in space.

-   Existing CCTM 3-D concentration fields. Usually, this option is selected when performing a nested model simulation and modeling results from a previous CCTM simulation are available from a coarser-grid-resolution simulation. Existing CCTM concentration fields are also used when a CCTM simulation is extended in time in a separate run step. Unlike the profiles discussed in the previous bullet, these CCTM concentration files are spatially and temporally resolved.

<span id=Figure2-4></span>

<center>
![](Figure2-4.png "Figure2-4.png")

</center>
<center>
**Figure 2-4. Initial and boundary conditions preprocessing for CMAQ**

</center>
***Chemical mechanism:*** Both the vertical concentration profiles and the CCTM concentration fields have specific chemical mechanisms associated with them, which are a function of how the files were originally generated. [Figure 2‑4](#Figure2-4 "wikilink") specifies that either a generic ASCII input profile or an existing CCTM 3-D concentration file can be used to generate initial and boundary conditions for the CCTM. The user must consider the gas-phase chemical mechanism and aerosol module being used for the CCTM simulation when configuring ICON and BCON. CMAQv5 includes ASCII input profiles for the CB05, SAPRC-07T, and SAPRC-99 photochemical mechanisms and the CMAQ AERO6 aerosol module. Existing CCTM 3‑D concentration fields could have been generated using several different chemical mechanisms. The chemical mechanism used in CCTM and CMAQ input processors must be consistent with the mechanism used to generate the concentration fields input to ICON and BCON.

ICON and BCON can linearly interpolate input concentration profiles from the horizontal or vertical coordinate system used in the profiles to the one needed for the model simulation, if the input data are in the standard I/O API format. If the interpolation is between two different vertical coordinate systems, the mid-layer height of each vertical layer must also be available.

### JPROC: Clear-sky photolysis rate calculator

For CMAQ, the photolysis rate model, JPROC, is used to generate clear-sky photodissociation reaction rates. JPROC requires temperature profiles from the *U.S. Standard Atmosphere* document (NOAA, 1976), a profile of the aerosol extinction coefficients (Elterman, 1969), data on species cross sections and quantum yields (CSQY), extraterrestrial radiance (ET), and standard seasonal profiles of ozone. JPROC can optionally use ozone column totals from the NASA Total Ozone Mapping Spectrometer (TOMS) satellite to produce the photolysis rates for CCTM ([Figure 2‑5](#Figure2-5 "wikilink")).

<span id=Figure2-5></span>

<center>
![](Figure2-5.png "Figure2-5.png")

</center>
<center>
**Figure 2-5. Clear-sky photolysis rate preprocessing for CMAQ**

</center>
JPROC uses this information in a radiative transfer model to calculate the actinic flux (photons cm<sup>-2</sup> min<sup>-1</sup>) needed for calculating photolysis rates. Currently, JPROC calculates the actinic flux for clear-sky conditions (no clouds present), and CCTM then attenuates for cloudiness when clouds are present. JPROC computes the rate for each photolysis reaction at various latitudes, altitudes, and zenith angles. Within CCTM, the subroutine PHOT interpolates the data generated by JPROC to individual grid cells, and adjusts for the presence of clouds.

An in-line photolysis module is included with CMAQv5 that can be used an alternative to JPROC to calculate photolysis rates.

### CHEMMECH: Chemical mechanism compiler

As released, CMAQ includes all necessary chemical mechanism information for a series of pre-configured atmospheric chemistry parameterizations. All the beginning user has to do is choose which mechanism is desired, as explained in Section 9.4. Advanced users who wish to generate new chemical mechanism information for use in CMAQ can use the program CHEMMECH to convert a mechanism listing file into the files needed by CMAQ.

Gas-phase chemical mechanisms are implemented in CMAQ using Fortran INCLUDE files. These files are in a machine-readable ASCII format and include all of the mechanism parameters required, including gas-phase species, reaction stoichiometry, and kinetics information. To invoke chemical mechanisms in CMAQ, these files are included in the compilation of the various CMAQ programs to generate mechanism-specific executables. CHEMMECH takes a mechanism definition file, often named “mech.def”, and generates the mechanism and species INCLUDE files—RXDT.EXT, RXCM.EXT, and SPC.EXT—that define the chemistry parameters for the CMAQ programs. The file “mech.def” is an ASCII file that is easy to understand and modify. [Figure 2-6](#Figure2-6 "wikilink") shows the relationship between CHEMMECH and other parts of the CMAQ modeling system.

<span id=Figure2-6></span>

<center>
![](Figure2-6.png "Figure2-6.png")

</center>
<center>
**Figure 2-6. Invoking new/modified gas-phase chemical mechanisms in CMAQ**

</center>
CMAQv5 introduces an alternative implementation of chemical mechanisms for CCTM. A mechanism namelist file can be used instead of the mechanism INCLUDE files. The benefit of the namelist approach is that the mechanism definition becomes a run-time configuration option as opposed to a compiled configuration. With careful modification to the namelist file, the user can add or subtract species being saved to the output file and apply across-the-board scaling factors to input emissions species without having to recompile CCTM. The namelist approach for defining chemical mechanisms is applicable only to CCTM; the standard INCLUDE file approach is required for ICON, BCON, and JPROC.

### PROCAN: Process-analysis preprocessor

Process analysis (PA) is an accounting system that tracks the quantitative effects of the individual chemical and physical processes that combine to produce the predicted hourly species concentrations output from a CTM simulation. The CMAQ CTM and other Eulerian grid models output concentration fields that are solutions of systems of partial differential equations for the time-rate of change in species concentrations due to a series of individual physical and chemical processes; these results are then combined to obtain a cumulative hourly concentration. PA tracks the mass throughput of these individual processes and provides quantitative information about how each process affected the predicted hourly species concentrations. PA is an optional configuration in CMAQ that is implemented by compiling CCTM with Fortran INCLUDE files that define PA configuration options. A PA-instrumented version of CCTM outputs additional files that contain hourly concentrations by physical and/or chemical process for selected model species.

PROCAN is the PA preprocessor in the CMAQ modeling system. As shown in [Figure 2‑7](#Figure2-7 "wikilink"), PROCAN takes as input a configuration file (Pacp.inp) that is used to define the model species and processes to be tracked using PA, and outputs three INCLUDE files (PA\_CMN.EXT, PA\_CTL.EXT, and PA\_DAT.EXT) that are used when compiling CCTM.

<span id=Figure2-7></span>

<center>
![](Figure2-7.png "Figure2-7.png")

</center>
<center>
**Figure 2-7 Process analysis implementation in the CMAQ modeling system**

</center>
### LTNG\_2D\_DATA: Lightning flash count preprocessor

CMAQv5 is instrumented to estimate the impacts of NO emissions from lightning on air quality. Details of the CCTM lightning NO<sub>x</sub> capability are described in [Section 7.7](#LTNG_2D_DATA "wikilink") and in the [CMAQv5 Technical Documentation](http://www.cmaq-model.org/cmaqwiki/index.php?title=CMAQv5.0_Lightning_NO). One of the configurations of this capability estimates lightning NO<sub>x</sub> from flash count observations. CMAQv5 is distributed with a lightning flash count preprocessor that converts ASCII flash count data into a format for input to CCTM.

The preprocessor is a combination of R scripts and a Fortran program. As shown in [Figure 2-8](#Figure2-8 "wikilink"), the statistical package R is used to read in a METCRO2D file that includes hourly convective precipitation (RC) and cloud top heights (CLDT) and a text file of intercloud/cloud-to-ground ratios. The output from these scripts is read in by the Fortran program LTNG\_2D\_DATA, along with a text file of lightning flash counts, to produce a binary file for input to CCTM. The output binary netCDF file from this preprocessor includes (1) monthly flash totals per CMAQ grid cell, (2) grid-cell scaling factors for calculating flashes using the convective precipitation rate, (3) ratio of intercloud to cloud-to-ground flashes, and (4) moles of NO per flash.

<span id=Figure2-8></span>

<center>
![](Figure2-8.png "Figure2-8.png")

</center>
<center>
**Figure 2-8 Lightning data preprocessor for CCTM**

</center>
### CALMAP: Crop calendar map preprocessor

CMAQv5 has the capability to estimate windblown dust emissions in-line in the CCTM. The CMAQ dust emissions module uses land cover/land use data to identify dust source regions. The dust module includes a feature to estimate dust emissions from agricultural areas and the impacts of planting and harvesting cycles on dust emissions. Calmap is a preprocessor to the CCTM that uses crop calendar information to produce gridded crop planting and harvesting dates for input to the CMAQ dust module.

Figure 2-9 is a Calmap schematic showing the data flow through the software. The program reads grid information from the GRIDCRO2D file, land cover/land use data from BELD3, and crop calendar data to produce files of planting start dates, planting end dates, and harvesting end dates for different crop types interpolated to the modeling grid. These files are input to the CCTM when it is configured to estimate windblown dust and simulate the impacts of agricultural activity on the dust emissions. Additional details about Calmap are available in Section 5.3.

<span id=Figure2-9></span>

<center>
![](Figure2-9.png "Figure2-9.png")

</center>
<center>
**Figure 2‑9. Crop calendar data preprocessor for the CCTM**

</center>
CMAQ Chemistry-Transport Model Science Modules
----------------------------------------------

[Figure 2‑10](#Figure2-10 "wikilink") illustrates the CMAQ modeling system used to simulate the chemistry and transport of pollutants. This figure also shows how CMAQ relates to other parts of an air quality modeling system: the meteorological model, emissions model, and analysis package. To perform a model simulation, CMAQ needs input information, including meteorological and emissions data. Using this information, CCTM simulates each of the atmospheric processes that affect the transport, transformation, and removal of ozone, particulate matter, and other pollutants. CMAQ uses state-of-the-science techniques to simulate these processes, as described in the following subsections.

### Gas-phase chemistry solvers

Various modules for simulating tropospheric gas-phase chemistry within CMAQ have been developed, ranging from simple linear and nonlinear systems for engineering model prototypes to comprehensive chemistry representations for detailed chemical pathways related to chemical transformation of atmospheric pollutants. In CMAQv5, gas-phase chemistry can be simulated with the CB05, SAPRC-07, or SAPRC-99 photochemical mechanisms. Because of CCTM’s modularity, advanced users can modify the existing photochemical mechanisms, or even add new ones. To compute time-varying species concentrations and their rate of formation or depletion, differential equations governing chemical reaction kinetics and species conservation need to be solved for the entire species set. CCTM uses special techniques called numerical chemistry solvers to compute these concentrations and rates at each time step.

Regarding these solvers, various solution algorithms for the chemical kinetics have been investigated in terms of the optimal balance of accuracy, generalization, and computational efficiency that is required for this component of the atmospheric system. CCTM currently contains three options for solving gas-phase chemical transformations: the Rosenbrock (ROS3) solver (Sandu et al., 1997), the Euler Backward Iterative (EBI) solver (Hertel et al., 1993), and the Sparse Matrix Vectorized GEAR (SMVGEAR) solver (Jacobson and Turco, 1994).

CMAQv5 includes options for simulating the chemistry of chlorine, mercury, and other toxic compounds in a multipollutant (mp) version of the CB05. See the CMAQv5 release notes for addition details on the gas-phase chemistry options.

<span id=Figure2-10></span>

<center>
![](Figure2-10.png "Figure2-10.png")

</center>
<center>
**Figure 2-10. CMAQ chemistry-transport model and associated preprocessors**

</center>
### Photolysis

Photolysis (or photodissociation) of trace gases initiates most chemical reactions that take place in the atmosphere. Photolysis splits gas-phase chemical species using energy from sunlight. Photolysis is involved in the formation of smog, an air pollution problem that affects human, animal, and plant health. Simulating photochemical reactions accurately is therefore a key issue that strongly influences air quality model performance.

CCTM uses state-of-the-science techniques to simulate photolytic reactions in the PHOT module. Photolysis reactions and their rates of reaction are driven by sunlight. Similar to kinetic reaction rates for nonphotochemical reactions, the photolysis rate quantifies how much reactant is produced from a photolytic reaction in a given amount of time. The calculation of a photolysis rate must include multiple influences:

The rate of photolysis is a function of the amount of solar radiation (called actinic flux), which varies based on the time of day, season, latitude, and terrestrial features. The amount of solar radiation is also affected by the amount of cloudiness and by aerosol absorption and scattering in the atmosphere.

The photolysis rate also depends on species-specific molecular properties like the absorption cross section (the effective molecular area of a particular species when absorbing solar radiation, which results in a shadow region behind the particle) and quantum yield (the number of molecules that dissociate for each light photon incident on the atmosphere). These molecular properties depend on the wavelength of the incident radiation and the temperature (and hence, on the available photon energy). Thus, estimating the photolysis rate is further complicated by these temperature and wavelength dependencies.

As discussed in [Section 4.2.3](#JPROC:_Clear-sky_photolysis_rate_calculator "wikilink"), the CMAQ modeling system includes an advanced photolysis model (JPROC) to calculate temporally varying photolysis rates for use in simulating photolysis in CCTM.

An in-line photolysis module (Binkowski et al., 2007) was included in CMAQ beginning with version 4.7. The in-line photolysis calculations account for the presence of ambient PM and ozone predicted by CMAQ and use these estimates to adjust the actinic flux, rather than relying on a look-up table of static background PM and ozone values. The in-line method for calculating photolysis rates also uses newer values of the absorption cross sections and quantum yields than those in the table look-up version, and these new values are corrected for ambient temperature. The extinction coefficients for the ambient aerosols are explicitly calculated using a new, efficient parametric algorithm derived from explicit integration of Mie calculations over the lognormal size distributions. Refractive indices are calculated based upon the categories of chemical species present (i.e., water soluble, insoluble, soot-like, water, and sea salt).

The JPROC module does not need to be run if the in-line photolysis method is used. With the in-line configuration, photolysis rates are calculated internally by CCTM and there is no need for the clear-sky look-up tables produced by JPROC.

### Advection and diffusion

Pollutant transport includes both advection and sub-grid-scale diffusion. Advection has to do with pollutant transport that is due to the mean wind fields, while diffusion involves sub-grid-scale turbulent mixing of pollutants. If a pollutant plume is transported primarily by advection, then it may travel a long distance without much change in pollutant concentrations. On the other hand, if a plume is transported primarily by diffusion, then the pollutants will mix more quickly and nearer to the source, which will result in substantial changes to pollutant concentrations.

*'*Advection: *'*In CCTM, the advection process is divided into horizontal and vertical components. This distinction is possible because mean atmospheric motion is mostly horizontal. Often, the vertical motion is related to the interaction of dynamics and thermodynamics. The advection process relies on the mass conservation characteristics of the continuity equation. Data consis­tency is maintained for air quality simulations by using dynamically and thermodynamically consistent meteorology data from MCIP. When the meteorological data and the numerical advection algorithms are not exactly mass consistent, one needs to solve a modified advection equation (Byun, 1999).

The horizontal advection module for CMAQ is the piecewise parabolic method (PPM) (Colella and Woodward, 1984). This algorithm is based on the finite-volume subgrid definition of the advected scalar. In PPM, the subgrid distribution is described by a parabola in each grid interval. PPM is a monotonic and positive-definite scheme. Positive-definite schemes maintain the sign of input values, which in this case means that positive concentrations will remain positive and cannot become negative. These codes are implemented in a global mass-conserving scheme introduced in v4.6 that is similar to the one used in the air quality forecasting version of CMAQ. Inspired by discussions with Robert Yamartino of Cambridge Environmental, the method uses the PPM scheme for horizontal advection, deriving a vertical velocity component at each grid cell that satisfies the continuity equation using the driving meteorological model’s air density.

The vertical advection modules solve for the vertical advection with no mass-exchange boundary conditions at the bottom or top of the model. CMAQ also uses PPM as its vertical advection module. In CMAQv5, a new method for computing the vertical velocity is implemented that follows the omega calculation in WRF but uses PPM to compute horizontal mass divergence. The two-step process first integrates the continuity equation through the vertical column to get the change in column mass and then solves for omega layer by layer using the horizontal mass divergence (see equation 2.27 in [<http://www.mmm.ucar.edu/wrf/users/docs/arw_v3.pdf>](http://www.mmm.ucar.edu/wrf/users/docs/arw_v3.pdf)). In CCTM, the PPM algorithm with a steepening procedure is implemented for vertical advection as the default because of the strong gradients in the tracer species that are observed in photochemical air quality conditions.

*'*Diffusion: *'*In CCTM, vertical diffusion is represented by the Asymmetric Convective Method (ACM) of Pleim and Chang (1992). ACM2 (Pleim, 2007), an updated version of ACM, is used in CMAQv5. This method recognizes that under convective conditions (when the surface is warming), heated air is transported vertically by buoyancy and mixes with ambient air at each level above the surface until the temperature of the rising air equals the ambient temperature. This process results from fast-moving air in narrow updrafts and slower-moving air in broader downdrafts. Thus, under convective conditions, vertical diffusion is asymmetric. In CMAQv5, an in-line method for treating biogenic and point-source emissions uses ACM to vertically distribute these emissions during a CMAQ calculation.

Under non-convective conditions (when the surface is cooling), vertical diffusion is represented by an eddy diffusivity approach. Eddy diffusivity is a local mixing scheme and is estimated using the same planetary boundary layer (PBL) similarity-based algorithm as in the Regional Acid Deposition Model (Chang et al., 1987, 1990). In CCTM, the deposition process is simulated as a flux boundary condition that affects the concentration in the lowest vertical model layer. By treating the deposition process as the loss of mass due to the diffusion flux at the bottom of the model, one can relate the bottom boundary condition in the generalized coordinate system to that in the Cartesian coordinate system. CMAQv5 has an improved version of the minimum allowable vertical eddy diffusivity scheme. The new version interpolates between urban and nonurban land cover, allowing a larger minimum vertical diffusivity value for grid cells that are primarily urban.

Horizontal diffusion in v5 is implemented with a single eddy diffusion algorithm that is based on local wind deformation and is scaled to the grid cell size. The horizontal eddy diffusivity is assumed to be uniform but dependent on the grid resolution of the model. This diffusivity is larger for a higher-resolution run where the numerical diffusion due to the advection process is smaller.

### Particulate matter (aerosols)

Within the air quality community, atmospheric aerosol particles are referred to as particulate matter (PM). PM can be either primary or secondary. Primary PM is emitted directly into the atmosphere from natural or anthropogenic (man-made) sources. Secondary PM is formed in the atmosphere, either from precursors that react chemically to form new particles, or from vapor-phase species that condense or deposit onto primary particles that are already present in the atmosphere. Cloud processes also contribute to the formation of PM; for example, aqueous oxidation of sulfur dioxide in cloud droplets is a significant pathway for production of particulate sulfate.

CCTM represents PM using three lognormal subdistributions, or modes. Two interacting modes (Aitken and accumulation) represent PM<sub>2.5</sub> (particulate matter of diameter equal to or less than 2.5 microns). A coarse mode represents PM with diameters greater than 2.5 microns and equal to or less than 10 microns. Thus, PM<sub>10</sub> is modeled as the sum of the PM<sub>2.5</sub> and coarse-mode PM.

Particulate matter is deposited to the ground by wet or dry deposition, both of which are modeled by CMAQ. In wet deposition, PM is transferred by rainfall. Wet deposition is calculated within CMAQ’s cloud module. In dry deposition, the transfer is by turbulent air motion and by direct gravitational sedimentation of larger particles. The deposition velocity for particles must be calculated from the aerosol size distribution and from meteorological and land use information. CMAQ’s dry deposition module calculates the size distribution from the mass and number concentration for each of the three modes and then calculates the dry deposition velocity. Starting in CMAQv4.7, the dry deposition algorithm was modified to include an impaction term in the coarse and accumulation modes.

CMAQv5 builds on the secondary organic aerosol (SOA) formation pathways implemented in CMAQv4.7. As an update to the work and recommendations of Edney et al. (2007) and Carlton et al. (2008), SOA yield parameterizations for monoterpenes and sesquiterpenes are updated to the values from Carlton et al. (2010).

The new aerosol module (AERO6) in CMAQv5 expands the definition of the PM Other species in earlier versions of the model to include more detailed PM species. Nine new PM species are added to CMAQ in AERO6: noncarbon organic matter (NCOM), Al, Ca, Fe, Si, Ti, Mg, K, and Mn. Four species that were explicitly treated in previous versions of CMAQ but were not modeled can now be treated as primary anthropogenic species: H<sub>2</sub>O, Na, Cl, and NH<sub>4</sub>. The PM emissions mass that remains after speciation into the new components is now input to the model as PMOTHER. AERO6 requires 18 PM<sub>2.5</sub>emissions species: OC, EC, sulfate, nitrate, H2O, Na, Cl, NH4, NCOM, Al, Ca, Fe, Si, Ti, Mg, K, Mn, and Other (Reff et al., 2009).

The AERO5 module from CMAQv4.7 is still available in CMAQv5, and it supports backward compatibility with emissions inputs that include only 5 PM species.

CMAQv5 simulates primary organic aerosol (POA) aging as a second-order reaction between primary organic carbon (POCR) and OH radicals.

ISORROPIAv1.7 is replaced with ISORROPIAv2.1 (Fountoukis and Nenes, 2007) to support the addition of the crustal ion emissions introduced with AERO6. AERO6 uses ISORROPIA in the “reverse mode” to calculate the condensation/evaporation of volatile inorganic gases to/from the gas-phase concentrations of known coarse particle surfaces. It also uses ISORROPIA in the “forward mode” to calculate instantaneous thermodynamic equilibrium between the gas and fine-particle modes.

Another type of output available from CMAQ is the reduction in visual range caused by the presence of PM, perceived as haze. CCTM integrates Mie scattering (a generalized particulate light-scattering mechanism that follows from the laws of electromagnetism applied to particulate matter) over the entire range of particle sizes to obtain a single visibility value for each model grid cell at each time step. More detailed descriptions of the PM calculation techniques used in CCTM can be found in Binkowski and Shankar (1995), Binkowski and Roselle (2003), and Byun and Schere (2006).

For easier comparison of CMAQ’s output PM values with measurements, there are three new variables (PM25AT, PM25AC, and PM25CO) that are the fractional amounts of the Aitken, accumulation, and coarse modes, respectively, that are composed of particles less than 2.5 microns in aerodynamic diameter (Jiang et al., 2006).

In the near future, two PM diagnostic tools will be available in an update to CMAQv5: one for tracking the sulfate budget, and one for tracking the sources of elemental and primary organic carbon that were part of CMAQv4.6.

There is a new surface interaction module in the multipollutant version of CMAQ that calculates the flux of mercury to and from the surface (rather than just depositing mercury). Further discussion on the scientific improvements to the CMAQ PM treatment is available in the [CMAQv5 Technical Documentation](http://www.cmaq-model.org/cmaqwiki/index.php?title=CMAQ_version_5.0_%28January_2012_release%29_Technical_Documentation).

### Clouds and aqueous-phase chemistry

Clouds are an important component of air quality modeling and play a key role in aqueous chemical reactions, vertical mixing of pollutants, and removal of pollutants by wet deposition. Clouds also indirectly affect pollutant concentrations by altering the solar radiation, which in turn affects photochemical pollutants (such as ozone) and the flux of biogenic emissions. The cloud module in CMAQ performs several functions related to cloud physics and chemistry. Three types of clouds are modeled in CMAQ: sub-grid convective precipitating clouds, sub-grid nonprecipitating clouds, and grid-resolved clouds. The meteorological model provides information about grid-resolved clouds, with no additional cloud dynamics considered in CMAQ. For the two types of sub-grid clouds, the cloud module in CCTM vertically redistributes pollutants, calculates in-cloud and precipitation scavenging, performs aqueous chemistry calculations, and accumulates wet deposition amounts. An important improvement in the CMAQv5 convective cloud mixing algorithm corrects a tendency to predict excessive transport from upper layers in the cloud to sub-cloud layers. In CMAQv5, a version of ACM is used to model convective clouds.

### Deposition

Several new features are included in CMAQv5 that improve or enhance the simulation of dry deposition processes in the model. The new deposition features in CMAQv5 include the following:

-   Bidirectional modules for ammonia and mercury simulate two-way exchange between the atmosphere and the surface for these species (as opposed to only deposition). The mercury bidirectional module is part of the CMAQv5 multipollutant configuration.
-   CMAQ MOSAIC is a configuration that outputs land use specific deposition velocities and fluxes.
-   Mesophyll resistance was added to CMAQv5 to improve the deposition calculations for insoluble atmospheric gases.
-   The effects of HONO heterogeneous chemistry on deposition velocities were removed in CMAQv5.

### Emissions

CMAQv5 includes several in-line options for calculating and processing emissions in CCTM. The in-line emissions options in CMAQv5 include the following:

-   The BEIS3 biogenic emissions model can be used to calculate emissions from vegetation and soils
-   Plume rise can be calculated for large point sources
-   Windblown dust emissions can be estimated using meteorology and land-cover data
-   Updated sea salt emissions. In AERO6 sea salt emissions in the accumulation mode are speciated into Na, Cl, SO4, Ca, K, and Mg. All cations in the coarse-mode sea salt (i.e., Na, Ca, K, and Mg) are lumped into a species called ASEACAT.

### Process analysis

As discussed in [Section 4.2.5](#PROCAN:_Process-analysis_preprocessor "wikilink"), CCTM also includes a process analysis (PA) module. Process analysis is a technique for separating out and quantifying the contributions of various individual physical and chemical processes to the changes in the predicted concentrations of a pollutant. This makes PA useful for conducting quality assurance procedures on a model run. With the information PA provides, compensating or unresolvable errors in the model or input data can be identified even if they are not reflected in the total change in concentration. For example, if an error in the emissions input data causes the model to calculate negative concentration values in an intermediate step, this occurrence could be masked in the final predicted concentrations if the negative values are more than compensated for by larger positive values resulting from the chemistry calculations.

In addition to its role in the quality control of air quality modeling runs, PA has other important applications:

-   It is a very strong analysis tool for identifying the relative importance of processes (chemistry, advection, diffusion, etc.) that change pollutant concentrations.
-   As a tool for model development, PA can help evaluate the effect of modifications made to a model or process module.
-   As a tool for regulatory decision-making, PA can help determine whether a decision to control a specific type of emission would produce the desired results, and if the answer is no, it can help determine another type of control that would be more effective.

Note that CMAQ with process analysis will not work with the EBI chemical solver (discussed in [Section 4.3.1](#Gas-phase_chemistry_solvers "wikilink")). The user is required to use either the Rosenbrock or the SMVGEAR solver.

The CMAQ User Interface
-----------------------

The CMAQ user interface that is distributed with the model source code consists of a series of C-shell scripts for building and running the various CMAQ programs on Linux operating systems. These scripts function primarily to set a series of environment variables that are required by Bldmake (described in [Section 3.2.4](#Summary_descriptions_of_the_major_CMAQ_programs "wikilink")) or by the CMAQ program executables. The scripts can be adapted to work with any Linux shell scripting language (e.g., Bash, Bourne).

CMAQ tarballs can be downloaded from [the CMAS Center website](https://www.cmascenter.org/cmaq/index.cfm). The MODELS tar file contains all of the source code for CMAQ, and the SCRIPTS tar file contains all of the C–shell scripts for building and executing the individual programs. Each of CMAQ’s programs has separate build and run scripts. The build script is used to compile and link the program; in versions of CMAQ \< 5.0.2 it also checked copies of the source code out of the distributed CVS repository. The run script is used to set the required environment variables and run the program. The user can manipulate the CMAQ scripts using a text editor such as [emacs](https://en.wikipedia.org/wiki/Emacs), [gedit](https://en.wikipedia.org/wiki/Gedit), [nano](https://en.wikipedia.org/wiki/GNU_nano), or [vi](https://en.wikipedia.org/wiki/Vi). There are certain options that need to be set at compilation, and some that can be set before running a simulation. Details about using the scripts to build and run CMAQ are described in [Section 5](#CMAQ_System_Requirements_and_Installation "wikilink"), with further details in [Section 7](#CMAQ_Programs_and_Libraries "wikilink").

The CMAS Center (see Section 11.1) currently fully supports CMAQ on Linux systems using the Portland Group and Intel Fortran compilers. The CMAS Center no longer supports compiling and running CMAQ on other UNIX operating systems. In any event, **users are strongly urged** to use the *same* Fortran compiler for *all* components of the CMAQ system, *and* to compile the netCDF and I/O API libraries on which CMAQ depends.

References for Section: Overview of the Science
-----------------------------------------------

Binkowski, F.S., and U. Shankar, 1995: The Regional Particulate Model: Part I. Model description and preliminary results. *J. Geophys. Res*., **100**, 26 191–26 209.

Binkowski, F. S., and S. J. Roselle, 2003: Models-3 Community Multiscale Air Quality (CMAQ) model aerosol component. 1. Model description. ''J. Geophys. Res., '**'108,** 4183, <doi:10.1029/2001JD001409>.

Binkowski, F.S, , S. Arunachalam, Z. Adelman, and J. Pinto, Examining photolysis rates with a prototype on-line photolysis module in CMAQ, 2007, *J. Appl. Meteor. and Clim.*. 46, 1252-1256, doi: 10.1175/JAM2531.1

Byun, D. W., 1999: Dynamically consistent formulations in meteorological and air quality models for Multiscale atmospheric studies. Part I: Governing equations in a generalized coordinate system. *J. Atmos. Sci*., **56**, 3789–3807.

Byun, D. W., and J. K. S. Ching, 1999: Science Algorithms of the EPA Models-3 Community Multiscale Air Quality (CMAQ) Modeling System. U. S. Environmental Protection Agency Rep. EPA‑600/R‑99/030, 727 pp. [Available from Office of Research and Development, EPA, Washington, DC 20460.]

Byun, D., and K. L. Schere, 2006: Review of the governing equations, computational algorithms, and other components of the Models-3 Community Multiscale Air Quality (CMAQ) modeling system. *Appl. Mech. Rev.*, **59**, 51–77. <doi:10.1115/1.2128636>

Carlton, A.G., B. J. Turpin, K. Altieri, S. Seitzinger, R. Mathur, S. Roselle, R. J. Weber, 2008. CMAQ model performance enhanced when in-cloud SOA is included: comparisons of OC predictions with measurements, Environ. Sci. Technol. , 42, (23), 8799-8802,

Carlton, A.G., P.V. Bhave, S.L. Napelenok, E.O. Edney, G. Sarwar, R.W. Pinder, G.A. Pouliot, M. Houyoux, 2010: Model Representation of Secondary Organic Aerosol in CMAQv4.7. *Env. Sci. & Techno*. **44 (22)**, 8553-8560.

Chang, J. S., P. B. Middleton, W. R. Stockwell, C. J. Walcek, J. E. Pleim, H. H. Lansford, F. S. Binkowski, S. Madronich, N. L. Seaman, D. R. Stauffer, D. Byun, J. N. McHenry, P. J. Samson, and H. Hass, 1990: The regional acid deposition model and engineering model, *Acidic Deposition: State of Science and Technology*, Report 4, National Acid Precipitation Assessment Program.

Colella, P., and P. L. Woodward, 1984: The piecewise parabolic method (PPM) for gas-dynamical simulations. *J. Comput. Phys*.,'' '**'54**, 174–201.

Edney, E. O., T. E. Kleindienst, M. Lewandowski, and J. H. Offenberg, 2007. Updated SOA chemical mechanism for the Community Multi-Scale Air Quality model, EPA 600/X-07/025, U.S. EPA, Research Triangle Park, NC.

Elterman, L., R. Wexler, and D. T. Chang, 1969: Features of tropospheric and stratospheric dust. *Appl. Optics*, **8**, 893–903.

Fountoukis, C and A. Nenes, 2007: ISORROPIA II: A computational efficient thermodynamic equilibrium model for K+-Ca2+-Mg2+-NH4+-Na+-SO42—NO3—Cl—H2O aerosols, ''Atmos. Chem. And Phys., '**'7**, 4639-4659.

Hertel O., R. Berkowicz, J. Christensen, and O. Hov, 1993: Test of two numerical schemes for use in atmospheric transport-chemistry models. *Atmos. Environ.*, **27A**, 2591–2611

Jacobson, M., and R. P. Turco, 1994: SMVGEAR: A sparse-matrix, vectorized Gear code for atmospheric models. *Atmos. Environ.*, **28**, 2991–3003.

Jiang, W., S. Smyth, É. Giroux, H. Roth, and D. Yin, 2006: Differences between CMAQ fine mode particle and PM<sub>2.5</sub> concentrations and their impact on model performance evaluation in the lower Fraser valley. *Atmos. Environ*., **40**, 4973–4985.

Pleim, J. E., and J. S. Chang, 1992: A non‑local closure model in the convective boundary layer. *Atmos. Environ,*. **26A**, 965–981.

Pleim, J. E., A. Xiu, P. L. Finkelstein, and T. L. Otte, 2001: A coupled land-surface and dry deposition model and comparison to field measurements of surface heat, moisture, and ozone fluxes. *Water Air Soil Pollut. Focus*, **1**, 243–252.

Pleim, J, 2007: A combined local and nonlocal closure model for the atmospheric boundary layer. Part I: model description and testing, *J. of Appl Met. and Climatology*, 46, 1383-1395

Reff, A., P.V. Bhave, H. Simon, T.G. Pace, G.A. Pouliot, J.D. Mobley, M. Houyoux, 2009: Emissions inventory of PM2.5 trace elements across the United States, *Env. Sci. & Technol*. **43**, 5790-5796.

Sandu, A., J. G. Verwer, J. G., Blom, E. J. Spee, G. R. Carmichael, and F. A. Potra, 1997: Benchmarking stiff ODE solvers for atmospheric chemistry problems. II: Rosenbrock solvers. *Atmos. Environ.*, **31**, 3459–3472.

National Oceanic and Atmospheric Administration, 1976: *U.S. Standard Atmosphere*, U.S. Government Printing Office, Washington, DC, NOAA‑S/T76‑1562.

