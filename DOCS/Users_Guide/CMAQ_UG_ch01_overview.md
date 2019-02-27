
<!-- BEGIN COMMENT -->

[Home](README.md) - [Next Chapter >>](CMAQ_UG_ch02_modeling_system.md)

<!-- END COMMENT -->

# Overview

## Disclaimer

**>>COMMENT<< ** Ensure that the Disclaimer reflects current EPA/ORD language.  

The information in this operational guidance document has been funded wholly or in part by the United States Environmental Protection Agency (EPA). The draft version of this document has not been subjected to the Agency’s peer and administrative review, nor has it been approved for publication as an EPA document. The draft document has been subjected to review by the Community Modeling and Analysis System Center only; this content has not yet been approved by the EPA. Mention of trade names or commercial products does not constitute endorsement or recommendation for use.

## Introduction and Goals

**>>COMMENT<<** para 1: Add reference for CAA, and update reference for NAAQS.

**>>COMMENT<<** para 1: : Not sure "ambient" is the right word here.

**>>COMMENT<<** para 2: Explain why single-pollutant models are insufficient.

**>>COMMENT<<** para 2: "indirect exposure to air toxics" is vague


Under the authority of the Clean Air Act, the U.S. Environmental Protection Agency (EPA) has established National Ambient Air Quality Standards (NAAQS) (EPA, 2008). These standards are designed to protect human health and the environment from high levels of criteria pollutants, such as ozone and particulate matter. Meeting the NAAQS often requires the use of controls on sources of air pollutants. The complex nature of air pollution scenarios requires control strategies to be effective for a variety of air pollutants, geographic regions, and scales. As part of the effort to decrease ambient concentrations of criteria pollutants, the EPA has approved air quality simulation models for use at regional, state, and local scales within the United States. The models have been applied to estimate the ability of various control strategies to improve air quality and ensure cost-effective results.

Because some emission sources contribute to the ambient levels of more than one pollutant and can affect an entire region on various time scales, an integrated modeling approach capable of handling multiple air pollutants and spatiotemporal scales was needed to isolate control strategies that improve overall air quality in a cost-effective manner. The [EPA Community Multiscale Air Quality (CMAQ) modeling system](http://www.epa.gov/cmaq) was formulated and designed to facilitate extensions needed to examine emerging linked multi-pollutants air pollution issues. The source code for CMAQ is available through a publicly-accessible, version-controlled git repository on [GitHub](www.github.com/usepa/cmaq) where interested parties may obtain the open-source software and contribute to enhancements of the model. CMAQ is designed for applications ranging from regulatory and policy analysis to probing and understanding the complex interactions of atmospheric chemistry and physics. It is a three\-dimensional Eulerian (i.e., gridded) atmospheric chemistry and transport modeling system that simulates ozone, particulate matter (PM), toxic airborne pollutants, visibility, and acidic and nutrient pollutant species throughout the troposphere. Designed as a “one-atmosphere” model, CMAQ can address the complex couplings among several air quality issues simultaneously across spatial scales ranging from local to hemispheric.

**>>COMMENT<<** bulleted text:  Seems really out-of-place here to get to this level of detail.

**>>COMMENT<<** emissions para: Double-check that these chemical mechanisms are all still supported.

**>>COMMENT<<**  Should information be provided about where to go for information on advanced configurations? 

**>>COMMENT<<**   will the I/O discussion also apply to David's new I/O?

Air quality models integrate our understandings of the complex processes that affect the concentrations of pollutants in the atmosphere. Establishing the relationships among meteorology, chemical transformations, emissions of chemical species, and removal processes in the context of atmospheric pollutants is the fundamental goal of an air quality model (Seinfeld and Pandis, 1998). CMAQ uses coupled mathematical representations of actual chemical and physical processes to simulate air quality. The model is formulated to conserve mass in the 3-D atmosphere within the modeled domain. The resultant partial differential governing equation is numerically solved over a 3-D grid discretizing the geographic domain of interest. A model grid is an *x\-y\-z* array that is fixed in space and covers a particular domain (i.e., a geographic area of interest). CMAQ therefore belongs to the Eulerian class of mathematical models that calculate a mass balance within each grid cell by solving the transport across each cell boundary and chemical transformations within each cell during a given time period. As a framework for simulating the interactions of multiple complex atmospheric processes, CMAQ thus requires two primary types of inputs: meteorological information, and emission rates from sources of emissions that affect the modeled air pollutant species.

With weather conditions contributing the primary physical driving forces in the atmosphere (such as the changes in temperature, winds, cloud formation, and precipitation rates), representative gridded meteorology forms the basis of all 3\-D air quality model simulations. The Weather Research and Forecasting (WRF) model \- Advanced Research WRF (WRF\-ARW) (Skamarock et al., 2005) is compatible with CMAQ and commonly used to drive the system. The meteorology inputs dictate the following CMAQ configuration parameters:

-   Horizontal grid coordinate system (e.g., latitude-longitude) and map projection (e.g., Lambert Conformal Conic)
-   Horizontal grid resolution (i.e., the size of the cells composing the grid)
-   Maximum spatial coverage (horizontal geographic extent, i.e., *the domain*) of the grid
-   Maximum vertical grid extent (model top)
-   Temporal extent (the starting and ending dates and times and the meteorology update frequency)

**>>COMMENT<<** Update or remove reference to chapter 2 of old user's document

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


## Brief History

**>>COMMENT<<** Note: Additional "Brief History" text can be taken from Introduction section above.

The Community Multiscale Air Quality (CMAQ) modeling system is being developed and maintained under the leadership of the [EPA National Exposure Research Laboratory](https://www.epa.gov/aboutepa/about-national-exposure-research-laboratory-nerl) in Research Triangle Park, NC. CMAQ represents nearly three decades of research in atmospheric modeling and has been in active development since the early 1990s. The first public release of CMAQ was in 1998 to enable use by air quality scientists, policy makers, and stakeholder groups to address multiscale, multipollutant air quality concerns.

## Features

**>>COMMENT<<** This whole section does not fit with the concept of a "Science Overview".  Many of these features were special to CMAQ when first developed but are pretty standard in CTMs now and CMAQ is no longer unique.

**>>COMMENT<<** Needs to be dramatically shortened to remove history (*really* old) and flexibility/modularity stuff.

As noted previously, early air quality model development resulted in separate air quality models that addressed single pollutants and issues, such as ozone or acid deposition. These models had little or no flexibility to be updated with advances in science or to accommodate new regulations. CMAQ was therefore designed to have more adaptability and flexibility for different applications and for changing or improving the modeling methodology. Within the context of the model’s science, the following subsections discuss CMAQ’s design in terms of (1) accommodating multiple pollutants and multiple scales, (2) providing flexibility through modularity, and (3) reducing the potential for model simulation error.

As a community model, CMAQ is able to leverage the expertise of model developers in many areas of atmospheric science. This facilitates improving and enhancing the CMAQ modeling system as the state-of-the-science evolves.

**>>COMMENT<<**  This information does not need to be in a separate chapter.  Most of it is redundant with information found in Chapters 1 and 2.  The only new information seems to be links to release notes.

**>>COMMENT<<**  suggest deleting "independent but" in first bullet

**>>COMMENT<<** If this information is retained somewhere, need to explain regulatory applications and SIPs.  Be generic for international user community.

**>>COMMENT<<** bullet 2:  Delete.  All of the modularity/flexibility jargon drove the original design and is not necessarily applicable today.  We certainly do not optimize model performance for anyone.

**>>COMMENT<<** bullet 5:  The community aspect is not necessarily true (and it never really was).  In principle, the community really relies on a single, centralized development group.

**>>COMMENT<<** bullet 6:  The first sentence on training is misleading.  It implies that you can get online training from CMAS, which only appears to be true for SMOKE.  The vast majority of the training is in-residence for a fee.

**>>COMMENT<<** bullet 6:  The link for "support resource" appears to be broken.

**>>COMMENT<<** Add a bullet about the user forum for support and to exchange development ideas.

**>>COMMENT<<** Update or remove reference to chapter 13 from old user's document

The CMAQ modeling system provides a variety of important features to users who are interested in applying the model for investigating scientific research questions or for regulatory applications such as preparation of State Implementation Plans (SIPs).

-   CMAQ is designed to address the complex interactions among multiple air quality issues simultaneously. Using a one-atmosphere approach to air quality modeling by applying multiscale and multipollutant modeling techniques, CMAQ can provide independent but dynamically consistent predictions of several different pollutants at varying spatial scales.
-   The modularity of the CMAQ design provides flexibility in air quality model configuration for optimizing model performance for different applications and spatial resolutions.
-   Close interactions among the development communities for CMAQ and for the meteorology and emissions models provide for a tight integration of the three main components of the air quality modeling system.
-   Serial and multiprocessor execution options allow the application user to optimize the performance of CMAQ on various computer platforms.
-   Community development expedites the expansion of CMAQ’s capabilities through the pursuit of multiple research agendas by a variety of research groups. Application users thus avoid the limitations inherent in having to rely on a single, centralized development group.
-   A comprehensive training program is available through the Community Modeling and Analysis System (CMAS) Center [website](http://www.cmascenter.org). The CMAS Center is a [support resource](CMAQ_OGD_ch13_support.md) for users of CMAQ and other modeling systems.
-   Members of the large, international community of users connected through the CMAS Center help each other by sharing data and experiences and providing technical support.

**>>COMMENT<<** Remove opening clause "Designed under a community modeling paradigm", and delete "with a modular…"

**>>COMMENT<<**  Remove references to I/O API and netCDF.  This will get blurry really soon.

**>>COMMENT<<**  should rework the IO discussion to convey that the system employs flexible I/O and internal data structures that facilitate its modularity and extensibility

**>>COMMENT<<**  Actually, if this material is retained somewhere, consider shortening the opening material to simply say: "CMAQ is distributed as open-source software.  CMAQ provides the following features to scientists interested in developing new algorithms or adding science to the model:"

**>>COMMENT<<**  Combine bullets 1 and 2, and delete bullet 3.

Designed under a community-modeling paradigm, CMAQ is distributed as open-source software engineered with a modular code design to facilitate decentralized development. Built around a layered [I/O API](https://www.cmascenter.org/ioapi) and [netCDF](http://www.unidata.ucar.edu/software/netcdf) code framework, CMAQ provides a flexible platform for testing new science algorithms, chemistry representations, and optimization techniques. CMAQ provides the following features to scientists interested in developing new algorithms or adding science to the model:

-   All CMAQ source code is available through [GitHub](https://github.com/USEPA/CMAQ).
-   Developed and distributed following open-source software conventions, CMAQ source code is easily accessible and free to obtain.
-   Designed for modularity, CCTM uses standardized input/output (I/O) routines to facilitate extensibility.
-   The diverse and continually growing community of CMAQ developers provides an excellent forum for discussing development-related topics of all kinds.

## Instrumented Models

## New Features in CMAQv5.3

## System Recommendations
**>>COMMENT<<** Note: Add something about system recommendations so that people know they won't be running this on a PC.

## CMAQ Support Resources
**>>COMMENT<<** Condense these sections, and add information on resources on EPA website and GitHub

**>>COMMENT<<** This information should be greatly condensed and moved to one of the beginning sections so people realize these options are available for training and help on running CMAQ.  It does not need to be its own chapter.


To support the CMAQ user community, EPA and the University of North Carolina at Chapel Hill host the [Community Modeling and Analysis System (CMAS) Center](http://www.cmascenter.org/), which distributes CMAQ software, develops cloud-based platforms for running CMAQ and analyzing outputs, and provides new user training on the CMAQ modeling system. The CMAS Center offers an e-mail help desk and an online forum to allow users to connect with model developers and other model users around the world.


The following activities are available through the CMAS Center:

-   [Online help desk](https://www.cmascenter.org/help-desk.cfm) - Get help with the supported CMAS products
-   [Online user forum](https://forum.cmascenter.org) -  Post questions and search through previous posts for helpful tips
-   [Model clearinghouse](https://www.cmascenter.org/download.cfm) - Download the supported CMAS products





___

[CMAQ User's Guide List of Tables and Figures](CMAQ_UG_tables_figures.md)

[CMAQ User's Guide Acronyms](CMAQ_UG_acronyms.md)
___

<!-- BEGIN COMMENT -->

[Home](README.md) - [Next Chapter >>](CMAQ_UG_ch02_modeling_system.md)<br>
CMAQ User's Guide (c) 2019<br>

<!-- END COMMENT -->
