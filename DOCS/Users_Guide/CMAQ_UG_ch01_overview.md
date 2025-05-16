
<!-- BEGIN COMMENT -->

[Home](README.md) - [Next Chapter >>](CMAQ_UG_ch02_program_structure.md)

<!-- END COMMENT -->

# 1. Overview

## Disclaimer

The information in this operational guidance document has been funded wholly or in part by the United States Environmental Protection Agency (EPA). The draft version of this document has not been subjected to the Agency’s peer and administrative review, nor has it been approved for publication as an EPA document. The draft document has been subjected to review by the Community Modeling and Analysis System Center only; this content has not yet been approved by the EPA. Mention of trade names or commercial products does not constitute endorsement or recommendation for use.

## 1.1 Introduction

Under the authority of the Clean Air Act, the U.S. Environmental Protection Agency (EPA) has established National Ambient Air Quality Standards (NAAQS). These standards are designed to protect human health and the environment from high levels of criteria pollutants, such as ozone and particulate matter. Meeting the NAAQS often requires the use of controls on sources of air pollutants. The complex nature of air pollution scenarios requires control strategies to be effective for a variety of air pollutants, geographic regions, and scales. The design of these control strategies is guided by comprehensive air pollution modeling systems which are applied to assess the ability of various control strategies to improve air quality in a cost-effective manner.

Because some emission sources contribute to the ambient levels of more than one pollutant and can affect an entire region on various time scales, an integrated modeling approach capable of handling multiple air pollutants and spatiotemporal scales is needed to identify cost-effective control strategies that improve overall air quality. The [EPA Community Multiscale Air Quality (CMAQ) modeling system](http://www.epa.gov/cmaq) was formulated and designed to facilitate extensions needed to examine emerging linked multi-pollutants air pollution issues. The source code for CMAQ is available through a publicly-accessible, version-controlled repository on [GitHub](www.github.com/usepa/cmaq) where interested parties may obtain the open-source software and contribute to enhancements of the model. CMAQ is designed for applications ranging from regulatory and policy analysis to probing and understanding the complex interactions of atmospheric chemistry and physics. It is a three\-dimensional Eulerian (i.e., gridded) atmospheric chemistry and transport modeling system that simulates ozone, particulate matter (PM), toxic airborne pollutants, visibility, and acidic and nutrient pollutant species throughout the troposphere. Designed as a “one-atmosphere” model, CMAQ can address the complex couplings among several air quality issues simultaneously across spatial scales ranging from urban to hemispheric.

Air quality models integrate our understandings of the complex processes that affect the concentrations of pollutants in the atmosphere. Establishing the relationships among meteorology, emissions of chemical species, chemical transformations, and removal processes in the context of atmospheric pollutants is the fundamental goal of an air quality model (Seinfeld and Pandis, 1998). CMAQ uses detailed mathematical representations of coupled atmospheric dynamical, chemical, and physical processes to describe the fate of airborne pollutants. The model is formulated to conserve mass in the 3-D atmosphere within the modeled domain. The resultant partial differential governing equations are numerically solved over a 3-D grid discretizing the geographic domain of interest. A model grid is an *x\-y\-z* array that is fixed in space and covers a prescribed domain (i.e., a geographic area of interest). CMAQ therefore belongs to the Eulerian class of mathematical models that calculate a mass balance over each discrete grid volume by accounting for transport across the boundaries of the grid volume and relevant source and sink terms within the grid volume over a given time period. As a mathematical framework for simulating the interactions of multiple complex atmospheric processes, CMAQ thus requires two primary types of inputs: meteorological information, and rates of emissions from a variety of anthropogenic and natural sources of primary pollutant species of interest or those that serve as precursors for formation of other pollutants of interest.

With weather conditions contributing the primary physical driving forces in the atmosphere (such as the changes in temperature, winds, cloud formation, and precipitation rates), representative gridded meteorology forms the basis of all 3\-D air quality model simulations. The Weather Research and Forecasting (WRF) model \- Advanced Research WRF (WRF\-ARW) (Skamarock et al., 2005) is compatible with CMAQ in that both systems can be configured to use identical horizontal and vertical coordinate and grid structures and is commonly used to drive CMAQ. The meteorology inputs dictate the following CMAQ configuration parameters:

-   Horizontal grid coordinate system (e.g., latitude-longitude, Lambert Conformal)
-   Horizontal grid resolution (i.e., the size of the cells composing the grid)
-   Maximum spatial coverage (horizontal geographic extent, i.e., *the domain*) of the grid
-   Maximum vertical extends (model top) and vertical grid resolution
-   Temporal extent (the starting and ending dates and times, and the meteorology update frequency)

To obtain inputs on emissions, CMAQ relies on an emissions processor to estimate the magnitude, location, and temporal variability of pollution sources. Open\-source processors such as the Sparse Matrix Operator Kernel Emissions ([SMOKE](https://www.cmascenter.org/smoke/)) processor (IE, 2008) are available for computing emissions inputs to CMAQ from emissions inventories. These emissions inputs must be specified on CMAQ's horizontal and vertical grid structure and cover at least the time period of the air quality model simulation. The emission inputs must also represent chemical species that conform with the gas and aerosol chemical mechanism employed in the CMAQ configuration; currently supported gas-phase mechanisms include recent versions of the Carbon Bond mechanism, the Statewide Air Pollution Research Center (SAPRC) mechanism, the Regional Atmospheric Chemistry Mechanism (RACM), and the Community Regional Atmospheric Chemistry Multiphase Mechanism (CRACMM). Additional details about the gas- and aerosol-phase chemistry in CMAQ are provided in [Chapter 6](CMAQ_UG_ch06_model_configuration_options.md).

## 1.2 Features

From inception, CMAQ was designed to facilitate community modeling. “Community modeling” is the concept that air quality model development should be a collective effort by a broad community of developers, thereby leveraging the cross-disciplinary expertise needed to keep the physical, numerical, and computational components of the modeling system at the state-of-the-science. By adopting a standardized modeling architecture, the air quality modeling community can focus its efforts on creating software enhancements and new science modules. CMAQ is designed to meet the needs of the multiple groups contained within the air quality modeling community: research and regulatory modelers, algorithm and science module developers, air quality forecasters, and planners and policy makers. While each of these groups has distinct individual requirements for CMAQ, they also share a common need for an efficient, transparent, and scientifically credible tool to simulate the air pollution phenomena. To address these individual and common needs, CMAQ development and maintenance have the following goals:

1.  *Scientific Integrity*. Ensure that the model remains state-of-the-science through subjecting it to [regular peer reviews](https://www.epa.gov/cmaq/cmaq-publications-and-peer-review)
2.  *Community Development*. Utilize a design that encourages innovations and enhancements by all members of the air quality modeling community
3.  *Multiscale Modeling*. Provide adequate technical formulations to address air quality issues on multiple spatial scales, from urban to hemispheric
4.  *Multi-pollutant Design*. Provide robust and integrated science for modeling multiple, coupled air quality issues in a single simulation
5.  *Modularity*. Maintain flexibility to add new, or select from existing, science modules to optimize model performance for specific applications
6.  *Transparency*. Utilize programming practices that promote understanding of the model formulation at the source-code level
7.  *Computational Efficiency*. Provide scientifically acceptable results without compromising the speed at which the results are generated
8.  *Open-Source Design*. Enable no-cost distribution and application by the modeling community

Designed under a community-modeling paradigm, CMAQ is distributed as open-source software engineered with a modular code design to facilitate decentralized development. Built around a layered [I/O API](https://www.cmascenter.org/ioapi) and [netCDF](http://www.unidata.ucar.edu/software/netcdf) code framework, CMAQ provides a flexible platform for testing new science algorithms, chemistry representations, and optimization techniques. CMAQ provides the following features to scientists interested in developing new algorithms or adding science to the model:

-   All CMAQ source code is available through [GitHub](https://github.com/USEPA/CMAQ).
-   Developed and distributed following open-source software conventions, CMAQ source code is easily accessible and free to obtain.
-   Designed for modularity, CMAQ uses standardized input/output (I/O) routines to facilitate extensibility.
-   The diverse and continually growing community of CMAQ developers provides an excellent forum for discussing development-related topics of all kinds.

The CMAQ modeling system is being developed and maintained under the leadership of the [EPA Office of Research and Development](https://www.epa.gov/aboutepa/about-office-research-and-development-ord) in Research Triangle Park, NC. CMAQ represents nearly three decades of research in atmospheric modeling and has been in active development since the early 1990s. The first public release of CMAQ was in 1998 to enable use by air quality scientists, policy makers, and stakeholder groups to address multiscale, multipollutant air quality concerns. Since then, through a series of phased development activities, new versions of the CMAQ modeling system are periodically released for use by the growing user community.

## 1.3 Diagnostics, Tools, and Instrumented Models
In addition to the air pollutant concentration and deposition fields output by CMAQ, the modeling system can also be instrumented to compute and output additional diagnostic information that can be used to probe the workings of the atmosphere as well as inform and guide policy inferences. These instrumented configurations include:

1.  *[Integrated Source Apportionment Method (ISAM)](CMAQ_UG_ch11_ISAM.md)*: Estimates source attribution information for user specified ozone and particulate matter precursors modeled in CMAQ. Such apportionment information could be used to gain insight on, for example, how much of the ozone in an urban area was formed due to nitrogen oxides emitted from motor vehicles in a neighboring state?
2.  *[Decoupled Direct Method in Three Dimensions (DDM-3D)](CMAQ_UG_ch10_HDDM-3D.md)*: A formal mathematical formulation that propagates sensitivity of CMAQ estimated concentrations and/or deposition to specified parameters (e.g., emissions) through the science modules in CMAQ. CMAQ-DDM-3D can be used for sensitivity to emission rates, boundary conditions, initial conditions, reaction rates, potential vorticity, or any combination of these parameters. Second order sensitivity calculations, or sensitivity of sensitivity, are also available.
3.  *[Sulfur Tracking Method (STM)](CMAQ_UG_ch12_sulfur_tracking.md)*: Tracks sulfate production from gas- and aqueous-phase chemical reactions, as well as contributions from emissions and initial and boundary conditions. The additional diagnostic information enables users to better understand the relative contribution of various pathways for airborne sulfate, a dominant contributor to fine particulate matter.
4.  *[Integrated Process Rates (IPR)](CMAQ_UG_ch09_process_analysis.md)*: CMAQ can be configured to output the process rates for each of the modeled processes impacting change in ambient concentrations of modeled species. This essentially provides a breakdown of the various terms contributing to the overall species mass-balance and thus helps with species mass-budget analysis.
5.  *[Integrated Reaction Rates (IRR)](CMAQ_UG_ch09_process_analysis.md)*: This technique involves integrating the rates of individual chemical reactions represented in the gas-phase chemical mechanism employed by CMAQ. As an example, this information can then be used to infer the relative importance of various precursor species contributing to ozone production in a grid cell or region.
6.  *[Budget Tool](CMAQ_UG_ch09_process_analysis.md)*: This tool outputs domain-wide burden and specific process rates for user-selected variables in a summarized ascii file. This data can be useful in understanding broad features of the lifecycle of trace pollutants and in diagnosing potential model inconsistencies.
7.  *[Explicit and Lumped CMAQ Model Output (ELMO) Module](Appendix/CMAQ_UG_appendixF_elmo_output.md)*: The ELMO Module allows users to request direct output of aggregate variables like PM<sub>2.5</sub> mass, total organic aerosol, and aerosol properties like mode-specific diameter and standard deviation. Users can also select scalar CMAQ variables and meteorological variables be added to ELMO output files.

## 1.4 New Features in CMAQv5.5
Building on previous versions of the modeling system, numerous updates to the process science and model structure have been implemented in CMAQv5.5 including:  

1. Gas and aerosol chemistry mechanisms have been updated to include more recent scientific understanding.  CMAQv5.5 introduces version 2 of the Community Regional Atmospheric Chemistry Multiphase Mechanism (CRACMM2). CRACMM2 includes several updates to CRACMM1. Many updates are intended to improve the representation of secondary formaldehyde (HCHO) in CRACMM. These include the incorporation of the AMORE v1.2 isoprene condensation into the primary CRACMM mechanism, updates to HCHO yields from monoterpenes, and the addition of styrene as a new explicit species. Additional updates include changes to monoterpene nitrates that affect SOA formation and NOx recycling, the inclusion of emitted methane (ECH4), heterogeneous uptake of HO2 and NO3 radicals, and changes in how emissions of certain aromatic species are mapped to CRACMM species.
   
2. CMAQv5.5 is the first public release that allows coupling of CMAQ with the Model for Prediction Across Scales (MPAS).  MPAS-CMAQ supports global domains with seamless regional resolution refinement over areas of interest.

3. The CMAQ-ISAM and CMAQ-DDM systems have been enhanced to support additional applications. CMAQ-ISAM now (1) includes the capability to quantify the contributions from anthropogenic and biogenic sources to total secondary organic aerosol (SOA) and individual species; (2) accounts for loss of aerosol mass from gravitational settling, thereby enabling the sum of K-mode tags to better match the bulk K-mode concentrations; (3) fixes a bug from v5.4 to allow for proper attribution of O3 from “stratospheric origin”. CMAQ-DDM was also corrected to properly estimate sensitivity of O3 to the PV-scaled O3 in the model top layers.

4. A  new option for estimation of photolysis rates was included.  This option includes a new approach to estimate the effects of aerosol optical properties on photolysis rates. The approach provides a better match to optical properties determined by solving Mie scattering theory for spherical particles than the default method (FastOptics) but at comparable runtimes.
   
See the [CMAQv5.5 Series FAQ](https://github.com/USEPA/CMAQ/wiki/CMAQv5.5-Series-FAQ) for more information on model updates in the new release. 

## 1.5 System Recommendations
CMAQ is a comprehensive air pollution modeling system whose source code is written mostly in Fortran. CMAQ execution is typically performed on Linux based systems. The hardware configuration of such a system depends on the domain size, grid resolution and simulation duration. Since typical input and output data sets for CMAQ entail three dimensional descriptions of the dynamical and chemical state of the simulated atmosphere, these data sets could require upwards of several gigabytes of disk storage per simulation day.

## 1.6 CMAQ Support Resources

Extensive information on the model's scientific basis, applications, publications, peer-review, and instructions to download the CMAQ modeling system are available at https://www.epa.gov/cmaq. To support the CMAQ user community, EPA currently funds the University of North Carolina at Chapel Hill to host the [Community Modeling and Analysis System (CMAS) Center](http://www.cmascenter.org/), which maintains a user help desk, provides new user training, and promotes the dissemination and use of the modeling system through exploration of new technologies and platforms (e.g., cloud-based). The CMAS Center offers an e-mail help desk and an [online forum](https://forum.cmascenter.org/) to allow users to connect with model developers and other model users around the world.


___

<!-- BEGIN COMMENT -->

[CMAQ User's Guide List of Tables and Figures](CMAQ_UG_tables_figures.md)

<!-- BEGIN COMMENT -->

___

<!-- BEGIN COMMENT -->

[Home](README.md) - [Next Chapter >>](CMAQ_UG_ch02_program_structure.md)<br>
CMAQv5.5 User's Guide <br>

<!-- END COMMENT -->
