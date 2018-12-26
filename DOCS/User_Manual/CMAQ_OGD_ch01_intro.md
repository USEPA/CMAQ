
<!-- BEGIN COMMENT -->

[Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch02_overview.md)

<!-- END COMMENT -->

<center>
**Version 5.3 (2019 Release)**

Prepared in cooperation with the Community Modeling and Analysis System
Institute for the Environment University of North Carolina at Chapel Hill.

</center>
Disclaimer
--
The information in this operational guidance document has been funded wholly or in part by the United States Environmental Protection Agency (EPA). The draft version of this document has not been subjected to the Agency’s peer and administrative review, nor has it been approved for publication as an EPA document. The draft document has been subjected to review by the Community Modeling and Analysis System Center only; this content has not yet been approved by the EPA. Mention of trade names or commercial products does not constitute endorsement or recommendation for use.

Foreword
--
The Community Multiscale Air Quality (CMAQ) modeling system is being developed and maintained under the leadership of the [EPA National Exposure Research Laboratory](https://www.epa.gov/aboutepa/about-national-exposure-research-laboratory-nerl) in Research Triangle Park, NC. CMAQ represents nearly three decades of research in atmospheric modeling and has been in active development since the early 1990s. The first public release of CMAQ was in 1998 to enable use by air quality scientists, policy makers, and stakeholder groups to address multiscale, multipollutant air quality concerns.

Introduction
--
Under the authority of the Clean Air Act, the U.S. Environmental Protection Agency (EPA) has established National Ambient Air Quality Standards (NAAQS) (EPA, 2008). These standards are designed to protect human health and the environment from high levels of criteria pollutants, such as ozone and particulate matter. Meeting the NAAQS often requires the use of controls on sources of air pollutants. The complex nature of air pollution scenarios requires control strategies to be effective for a variety of air pollutants, geographic regions, and scales. As part of the effort to decrease ambient concentrations of criteria pollutants, the EPA has approved air quality simulation models for use at regional, state, and local scales within the United States. The models have been applied to estimate the ability of various control strategies to improve air quality and ensure cost-effective results.

Because some emission sources contribute to the ambient levels of more than one pollutant and can affect an entire region on various time scales, an integrated modeling approach capable of handling multiple air pollutants and spatiotemporal scales was needed to isolate control strategies that improve overall air quality in a cost-effective manner. The [EPA Community Multiscale Air Quality (CMAQ) modeling system](http://www.epa.gov/cmaq) was formulated and designed to facilitate extensions needed to examine emerging linked multi-pollutants air pollution issues. The source code for CMAQ is available through a publicly-accessible, version-controlled git repository on [GitHub](www.github.com/usepa/cmaq) where interested parties may obtain the open-source software and contribute to enhancements of the model. CMAQ is designed for applications ranging from regulatory and policy analysis to probing and understanding the complex interactions of atmospheric chemistry and physics. It is a three\-dimensional Eulerian (i.e., gridded) atmospheric chemistry and transport modeling system that simulates ozone, particulate matter (PM), toxic airborne pollutants, visibility, and acidic and nutrient pollutant species throughout the troposphere. Designed as a “one-atmosphere” model, CMAQ can address the complex couplings among several air quality issues simultaneously across spatial scales ranging from local to hemispheric.

Model Background and Goals
--
Air quality models integrate our understandings of the complex processes that affect the concentrations of pollutants in the atmosphere. Establishing the relationships among meteorology, chemical transformations, emissions of chemical species, and removal processes in the context of atmospheric pollutants is the fundamental goal of an air quality model (Seinfeld and Pandis, 1998). CMAQ uses coupled mathematical representations of actual chemical and physical processes to simulate air quality. The model is formulated to conserve mass in the 3-D atmosphere within the modeled domain. The resultant partial differential governing equation is numerically solved over a 3-D grid discretizing the geographic domain of interest. A model grid is an *x\-y\-z* array that is fixed in space and covers a particular domain (i.e., a geographic area of interest). CMAQ therefore belongs to the Eulerian class of mathematical models that calculate a mass balance within each grid cell by solving the transport across each cell boundary and chemical transformations within each cell during a given time period. As a framework for simulating the interactions of multiple complex atmospheric processes, CMAQ thus requires two primary types of inputs: meteorological information, and emission rates from sources of emissions that affect the modeled air pollutant species.

With weather conditions contributing the primary physical driving forces in the atmosphere (such as the changes in temperature, winds, cloud formation, and precipitation rates), representative gridded meteorology forms the basis of all 3\-D air quality model simulations. The Weather Research and Forecasting (WRF) model \- Advanced Research WRF (WRF\-ARW) (Skamarock et al., 2005) is compatible with CMAQ and commonly used to drive the system. The meteorology inputs dictate the following CMAQ configuration parameters:

-   Horizontal grid coordinate system (e.g., latitude-longitude) and map projection (e.g., Lambert Conformal Conic)
-   Horizontal grid resolution (i.e., the size of the cells composing the grid)
-   Maximum spatial coverage (horizontal geographic extent, i.e., *the domain*) of the grid
-   Maximum vertical grid extent (model top)
-   Temporal extent (the starting and ending dates and times and the meteorology update frequency)

To obtain inputs on emissions, CMAQ relies on an emissions processor to estimate the magnitude, location, and temporal variability of pollution sources. Open\-source processors such as the Sparse Matrix Operator Kernel Emissions ([SMOKE](https://www.cmascenter.org/smoke/)) processor (IE, 2008) are available for computing emissions inputs to CMAQ from emissions inventories. These emissions inputs must be on the same horizontal and vertical spatial scales, as well as the same projection, and cover the same time period as are used in the air quality model simulation. The emission inputs must also represent VOC emissions speciated to conform with the chemical mechanism employed in the CMAQ configuration; currently supported photochemical mechanisms are recent versions of the Carbon Bond mechanism, the Statewide Air Pollution Research Center (SAPRC) mechanism, and the Regional Atmospheric Chemistry Mechanism (RACM). Additional details about the gas-phase chemistry in CMAQ are provided in [the Operational Guidance Document (OGD) section on CMAQ Chemistry and Transport Modules](CMAQ_OGD_ch02_overview.md#CMAQ_Chemistry-Transport_Model_Science_Modules). Those two sources also describe the primary aerosol emissions species that are supported by CMAQ.

CMAQ was designed from the start as a community model. “Community modeling” is the concept that air quality model development should be a collective effort by a broad community of developers. By adopting a standardized modeling architecture, the air quality modeling community can focus its efforts on creating software enhancements and new science modules. CMAQ is designed to meet the needs of the multiple groups contained within the air quality modeling community: research and regulatory modelers, algorithm and science module developers, air quality forecasters, and planners and policy makers. While each of these groups has distinct individual requirements for CMAQ, they also share a common need for an efficient, transparent, and scientifically credible tool to simulate air pollution formation and transport. To address these individual and common needs, CMAQ development and maintenance have the following goals:

1.  *Scientific Integrity*. Ensure that the model remains state-of-the-science through subjecting it to [regular peer reviews](https://www.epa.gov/cmaq/cmaq-publications-and-peer-review)
2.  *Community Development*. Utilize a design that encourages innovations and enhancements by all members of the air quality modeling community
3.  *Multiscale Modeling*. Provide adequate technical formulations to address air quality issues on multiple spatial scales, from urban to hemispheric
4.  *One-Atmosphere Design*. Provide robust and integrated science for modeling multiple, coupled air quality issues in a single simulation
5.  *Modularity*. Maintain flexibility to add new, or select from existing, science modules to optimize model performance for specific applications
6.  *Transparency*. Utilize programming practices that promote understanding of the model formulation at the source-code level
7.  *Computational Efficiency*. Provide scientifically acceptable results without compromising the speed at which the results are generated
8.  *Open-Source Design*. Enable no-cost distribution and application by the modeling community

[CMAQ OGD List Tables and Figures](CMAQ_OGD_tables_figures.md)
--

[CMAQ OGD Acronyms](CMAQ_OGD_acronyms.md)
--

<!-- BEGIN COMMENT -->

[Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch02_overview.md)<br>
CMAQ Operational Guidance Document (c) 2016<br>

<!-- END COMMENT -->
