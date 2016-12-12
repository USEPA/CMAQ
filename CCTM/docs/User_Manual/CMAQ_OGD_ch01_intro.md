[[Back to Wiki Home|/Home.md]]
<center>
**Operational Guidance for the Community Multiscale Air Quality (CMAQ) Modeling System**

**Version 5.0 (February 2012 Release)**

Prepared in cooperation with the
Community Modeling and Analysis System
Institute for the Environment
University of North Carolina at Chapel Hill
Chapel Hill, NC

</center>
Disclaimer
==========

The information in this document has been funded wholly or in part by the United States Environmental Protection Agency. The draft version of this document has not been subjected to the Agency’s peer and administrative review, nor has it been approved for publication as an EPA document. The draft document has been subjected to review by the Community Modeling and Analysis System Center only; this content has not yet been approved by the EPA. Mention of trade names or commercial products does not constitute endorsement or recommendation for use.

Foreword
========

The Community Multiscale Air Quality (CMAQ) modeling system is being developed and maintained under the leadership of the Atmospheric Modeling and Analysis Division of the EPA National Exposure Research Laboratory in Research Triangle Park, NC. CMAQ represents over two decades of research in atmospheric modeling and has been in active development since the early 1990’s. The first release of CMAQ was made available in 1998 without charge for use by air quality scientists, policy makers, and stakeholder groups to address multiscale, multipollutant air quality concerns. All subsequent CMAQ releases, past, current, and future, will adhere to this same level of code accessibility and scope.

Figures, Tables, and Acronyms
-----------------------------

**List of Figures**

[Figure 2‑1. CMAQ Modeling Framework](CMAQ_OGD_ch04_science.md#Figure2-1)

[Figure 2-2. CMAQ Chemistry-Transport Model (CCTM) and input processors](CMAQ_OGD_ch04_science.md#Figure2-2)

[Figure 2-3. Meteorology preprocessing for CMAQ with MCIP](CMAQ_OGD_ch04_science.md#Figure2-3)

[Figure 2-4. Initial and boundary conditions preprocessing for CMAQ](CMAQ_OGD_ch04_science.md#Figure2-4)

[Figure 2-5. Clear-sky photolysis rate preprocessing for CMAQ](CMAQ_OGD_ch04_science.md#Figure2-5)

[Figure 2-6. Invoking new/modified gas-phase chemical mechanisms in CMAQ](CMAQ_OGD_ch04_science.md#Figure2-6)

[Figure 2-7 Process analysis implementation in the CMAQ modeling system](CMAQ_OGD_ch04_science.md#Figure2-7)

[Figure 2-8 Lightning data preprocessor for CCTM](CMAQ_OGD_ch04_science.md#Figure2-8)

[Figure 2-9 Crop calendar data preprocessor for the CCTM](CMAQ_OGD_ch04_science.md#Figure2-9)

[Figure 2-10. CMAQ chemistry-transport model and associated preprocessors](CMAQ_OGD_ch04_science.md#Figure2-10)

[Figure 7‑1.CMAQ core programs](CMAQ_OGD_ch07_programs_libraries.md#Figure7-1)

[Figure 7‑2. BCON input and output files](CMAQ_OGD_ch07_programs_libraries.md#Figure7-2)

[Figure 7‑3. Calmap input and output files](CMAQ_OGD_ch07_programs_libraries.md#Figure7-3)

[Figure 7‑4. CCTM input and output files](CMAQ_OGD_ch07_programs_libraries.md#Figure7-4)

[Figure 7‑5. CHEMMECH and CSV2NML input and output files](CMAQ_OGD_ch07_programs_libraries.md#Figure7-5)

[Figure 7‑6. ICON input and output files](CMAQ_OGD_ch07_programs_libraries.md#Figure7-6)

[Figure 7‑7. JPROC input and output files](CMAQ_OGD_ch07_programs_libraries.md#Figure7-7)

[Figure 7‑8. LTNG\_2D\_DATA input and output files](CMAQ_OGD_ch07_programs_libraries.md#Figure7-8)

[Figure 7‑9. MCIP input and output files](CMAQ_OGD_ch07_programs_libraries.md#Figure7-9)

[Figure 7‑10. PROCAN input and output files](CMAQ_OGD_ch07_programs_libraries.md#Figure7-10)

[Figure 9-1. CMAQ benchmark grid](CMAQ_OGD_ch09_grid_defn.md#Figure9-1)

[Figure 10-1. 36-km four-day modeling period IC/BC schematic](CMAQ_OGD_ch10_new_simulation.md#Figure10-1)

[Figure 10-2. 12-km nested two-day modeling period IC/BC schematic](CMAQ_OGD_ch10_new_simulation.md#Figure10-2)

**List of Tables**

[Table 5‑1. Software required for running CMAQ](#Table5-1 "wikilink")

[Table 5‑2. Optional support software for CMAQ](#Table5-2 "wikilink")

[Table 5‑3. NetCDF and I/O API compilation options for CMAQ](#Table5-3 "wikilink")

[Table 6-1. Possible Time Step Structures in I/O API Files](#Table6-1 "wikilink")

[Table 6-2. Possible Data Type Structures in I/O API Files](#Table6-2 "wikilink")

[Table 6-3. Possible values for OPEN(3) FSTATUS'](#Table6-3 "wikilink")

[Table 6-4. IO API data retrieval routines](#Table6-4 "wikilink")

[Table 6-5. I/O API data manipulation utilities](#Table6-5 "wikilink")

[Table 7‑1. BCON input files](#Table7-1 "wikilink")

[Table 7‑2. BCON output files](#Table7-2 "wikilink")

[Table 7‑3. Required Calmap input files](#Table7-3 "wikilink")

[Table 7‑4. Required Calmap input files](#Table7-4 "wikilink")

[Table 7‑5. Required CCTM input files](#Table7-5 "wikilink")

[Table 7‑6. Optional CCTM input files](#Table7-6 "wikilink")

[Table 7‑7. CCTM base output files](#Table7-7 "wikilink")

[Table 7‑8. CCTM optional output files](#Table7-8 "wikilink")

[Table 7‑9. CHEMMECH input files](#Table7-9 "wikilink")

[Table 7‑10. CHEMMECH output files](#Table7-10 "wikilink")

[Table 7‑11. CSV2NML input files](#Table7-11 "wikilink")

[Table 7‑12. CSV2NML output files](#Table7-12 "wikilink")

[Table 7‑13. ICON input files](#Table7-13 "wikilink")

[Table 7‑14. ICON output files](#Table7-14 "wikilink")

[Table 7‑15. JPROC input files](#Table7-15 "wikilink")

[Table 7‑16. JPROC output files](#Table7-16 "wikilink")

[Table 7‑17. LTNG\_2D\_DATA input files](#Table7-17 "wikilink")

[Table 7‑18. LTNG\_2D\_DATA output files](#Table7-18 "wikilink")

[Table 7‑19. MCIP input files](#Table7-19 "wikilink")

[Table 7‑20. MCIP output files](#Table7-20 "wikilink")

[Table 7‑21. PROCAN input files](#Table7-21 "wikilink")

[Table 7‑22. Process analysis global commands](#Table7-22 "wikilink")

[Table 7‑23. Integrated process rate output commands](#Table7-23 "wikilink")

[Table 7‑24. Integrated process rates process codes](#Table7-24 "wikilink")

[Table 7‑25. Integrated reaction rate output commands](#Table7-25 "wikilink")

[Table 7‑26. PROCAN output files](#Table7-26 "wikilink")

[Table 8‑1. CMAQ input files](#Table8-1 "wikilink")

[Table 8‑2. Coordinate sytem description segment of GRIDDESC](#Table8-2 "wikilink")

[Table 8‑3. Grid definition segment of GRIDDESC](#Table8-3 "wikilink")

[Table 8‑4. GC species namelist file format](#Table8-4 "wikilink")

[Table 8-5. IC\_PROFILE format description](#Table8-5 "wikilink")

[Table 8-6. BC\_PROFILE format description](#Table8-6 "wikilink")

[Table 8-7. CSQY format description](#Table8-7 "wikilink")

[Table 8-8 ET file format description](#Table8-8 "wikilink")

[Table 8-9. PROFILES file format description](#Table8-9 "wikilink")

[Table 8-10 TOMS Data Profile](#Table8-10 "wikilink")

[Table 8-11. JTABLE file format description](#Table8-11 "wikilink")

[Table 8‑12. OMI data format](#Table8-12 "wikilink")

**Acronyms**

|---|---|
|AE|Aerosol chemistry phase|
|AIRS|Aerometric Information Retrieval System|
|AQ|Aqueous Chemistry Phase|
|ASCII|American Standard Code for Information Interchange|
|BC|Boundary Condition|
|BCON|Boundary Conditions Processor|
|BEIS3|Biogenic Emissions Inventory System|
|CB-IV|Carbon Bond-IV|
|CB05|Carbon Bond 2005|
|CCTM|CMAQ Chemistry-Transport Model|
|CEMPD|Center for Environmental Modeling for Policy Development|
|CMAS|Community Modeling and Analysis System|
|CMAQ|Community Multi-Scale Air Quality model|
|CO|Carbon Monoxide|
|CPU|Central Processing Unit of a computer|
|CSQY|Cross Section and Quantum Yield|
|CTM|Chemistry-Transport Model|
|CVS|Concurrent Versions System|
|EBI|Euler Backward Iterative chemistry solver|
|EPA|Environmental Protection Agency|
|FAQ|Frequently Asked Question|
|FTP|File Transport Protocol|
|GB|Gigabyte|
|GIS|Geographic Information System|
|GMT|Greenwich Mean Time|
|GPL|Gnu General Public License|
|GUI|Graphical User Interface|
|HTML|HyperText Markup Language|
|HYSPLIT|Hybrid Single-Particle Lagrangian Integrated Trajectory model|
|I/O API|Input/Output Application Programming Interface|
|IC|Initial Concentration|
|ICON|Initial Conditions Processor|
|IDA|Inventory Data Analyzer|
|IE|Institute for the Environment|
|IPR|Integrated Process Rate|
|IRR|Integrated Reaction Rate|
|IRR/MB|Integrated Reaction Rates/Mass Balance Analysis|
|JPROC|Photolysis Rate Processor|
|MCIP|Meteorology-Chemistry Interface Processor|
|MECH|Chemical Mechanism Reader|
|MEPSE|Major Elevated Point Source Emission|
|MM5|Fifth-Generation Penn State/NCAR Mesoscale Model|
|MP|Mechanism Processor|
|NAAQS|National Ambient Air Quality Standards|
|NASA|National Aeronautics and Space Administration|
|NCAR|National Center for Atmospheric Research|
|NET|National Emission Trends|
|netCDF|network Common Data Form|
|NO<sub>x</sub>|Oxides of Nitrogen|
|NOAA|National Oceanic and Atmospheric Administration|
|NR|Non-Reactive|
|NWS|National Weather Service|
|PACP|Process Analysis Control Process|
|PAVE|Package for Analysis and Visualization of Environmental Data|
|PBL|Planetary Boundary Layer|
|PC|Personal Computer|
|PM2.5|Particulate matter up to 2.5 microns|
|PM10|Particulate matter up to 10 microns|
|PPM|Piecewise Parabolic Method|
|ppm|Parts per million (by weight)|
|ppmV|Parts per million (by volume)|
|PROCAN|Process Analysis Processor|
|QC|Quality Control|
|RADM|Regional Acid Deposition Model|
|SAPRC-99|State Air Pollution Research Center mechanism, version 1999|
|SAPRC-07|State Air Pollution Research Center mechanism, version 2007|
|SAS|Statistical Analysis System|
|SCC|Source Classification Code|
|SCCS|Source Code Control System|
|SMVGEAR|Sparse Matrix Vectorized Gear|
|SO<sub>2</sub>|Sulfur Dioxide|
|SPC|Species|
|TOMS|Total Ozone Mapping Spectrometer|
|TR|Tracer|
|UAM|Urban Airshed Model|
|UNC|University of North Carolina|
|UTC|Universal Time Coordinate|
|UTM|Universal Transverse Mercator|
|VOC|Volatile Organic Compounds|
|WRF|Weather Research and Forecasting model|
|WRF-ARW|Weather Research and Forecasting model – Advanced Research WRF|

Introduction
============

Under the authority of the Clean Air Act, the U.S. Environmental Protection Agency (EPA) has established National Ambient Air Quality Standards (NAAQS) (U.S. EPA, 2008). These standards are designed to protect human health and the environment from high levels of criteria pollutants, such as ozone and particulate matter. Meeting the NAAQS often requires the use of controls on sources of air pollutants. The complex nature of air pollution scenarios requires control strategies to be effective for a variety of air pollutants, geographic regions, and scales. As part of the effort to decrease ambient concentrations of criteria pollutants, the EPA has approved air quality simulation models for use at regional, state, and local scales within the United States. The models have been applied to estimate the ability of various control strategies to improve air quality and ensure cost-effective results.

So-called first-generation air quality models simulated air quality using simple chemistry at local scales, and Gaussian plume formulation was the basis for prediction. Second-generation models covered a broader range of scales (local, urban, regional) and pollutants, addressing each scale with a separate model that often focused on a single pollutant or issue (e.g., ozone, acid deposition). It became apparent, however, that single-pollutant models were not sufficient. Depending on the release characteristics, the pollutants in question, and the surrounding meteorological conditions at the time of pollutant release, modeling scenarios can range from a localized, short-term phenomenon to a long-term regional event. Because some emission sources contribute to the ambient levels of more than one pollutant and can affect an entire region on various time scales, an integrated modeling approach capable of handling multiple air pollutants and spatiotemporal scales was needed to isolate control strategies that improve overall air quality in a cost-effective manner. New air quality issues identified by the Clean Air Act Amendments of 1990 (such as visibility, fine and coarse particles, and indirect exposure to toxic pollutants) made an integrated modeling approach that could address multiple pollutants even more essential. Third-generation models were needed that could treat multiple pollutants simultaneously and at scales up to continental or larger.[1]

The EPA Community Multiscale Air Quality (CMAQ) modeling system is a third-generation air quality model. It is available online at [<http://www.cmaq-model.org>](http://www.cmaq-model.org/). CMAQ is designed for applications ranging from regulatory and policy analysis to understanding the complex interactions of atmospheric chemistry and physics. It is a three-dimensional Eulerian (i.e., gridded) atmospheric chemistry and transport modeling system that simulates ozone, particulate matter (PM), toxic airborne pollutants, visibility, and acidic and nutrient pollutant species throughout the troposphere. Designed as a “one-atmosphere” model, CMAQ can address the complex couplings among several air quality issues simultaneously across spatial scales ranging from local to hemispheric. The CMAQ source code is highly transparent and modular to facilitate the model’s extensibility through community development by all members of the air quality modeling community.

Model Background and Goals
--------------------------

Air quality models integrate our understandings of the complex processes that affect the concentrations of pollutants in the atmosphere. Establishing the relationships among meteorology, chemical transformations, emissions of chemical species, and removal processes in the context of atmospheric pollutants is the fundamental goal of an air quality model<sup></sup>(Seinfeld and Pandis, 1998). In contrast to statistical air quality models that use historical trends in observed atmospheric conditions to predict air pollution, CMAQ uses coupled mathematical representations of actual chemical and physical processes to simulate air quality. The model is based upon the underlying concept of preserving mass through a series of contiguous three-dimensional (3‑D) grid cells covering a fixed model grid (i.e., *x-y-z* array that is fixed in space and covers a particular domain, i.e., the geographic area of interest). CMAQ therefore belongs to the Eulerian class of mathematical models that calculate a mass balance within each grid cell by solving the transport across each cell boundary and chemical transformations within each cell during a given time period. As a framework for simulating the interactions of multiple complex atmospheric processes, CMAQ thus requires two primary types of inputs: meteorological information, and emission rates from sources of emissions that affect air quality.

With weather conditions contributing the primary physical driving forces in the atmosphere (such as the changes in temperature, winds, cloud formation, and precipitation rates), representative gridded meteorology forms the basis of all 3-D air quality model simulations. The Fifth-Generation Pennsylvania State University/National Center for Atmospheric Research (PSU/NCAR) Mesoscale Model (MM5) (Grell et al., 1994) and the Weather Research and Forecasting (WRF) model – Advanced Research WRF (WRF-ARW) (Skamarock et al., 2005) are two meteorological models that are compatible with CMAQ. The meteorology inputs dictate the following CMAQ configuration parameters:

-   Horizontal grid coordinate system (e.g., latitude-longitude) and map projection (e.g., Lambert Conformal Conic)
-   Horizontal grid resolution (i.e., the size of the cells composing the grid)
-   Maximum spatial coverage (horizontal geographic extent, i.e., *the domain*) of the grid
-   Maximum vertical grid extent (model top)
-   Temporal extent (the starting and ending dates and times and the meteorology update frequency)

To obtain inputs on emissions, CMAQ relies on an emissions model to estimate the magnitude, location, and temporal variability of pollution sources. Open-source models such as the Sparse Matrix Operator Kernel Emissions (SMOKE)<sup></sup>model (IE, 2008) ([<http://www.smoke-model.org/>](http://www.smoke-model.org/)) and the Consolidated Community Emissions Processing Tool (CONCEPT) ([<http://www.conceptmodel.org/>](http://www.conceptmodel.org/)) are available for computing emissions inputs to CMAQ from annual, county-level emissions inventories. These emissions inputs must be on the same horizontal and vertical spatial scales and cover the same time period as are used in the air quality model simulation. The emissions inputs to CMAQ must also represent volatile organic compound (VOC) emissions using a chemical parameterization supported by CMAQ; currently supported photochemical mechanisms are the 2005 update to the Carbon Bond mechanism (CB05) (Yarwood et al., 2005), and the Statewide Air Pollution Research Center, Version 1999 (SAPRC-99) mechanism (Carter, 1990, 2000). Additional details about the gas-phase chemistry in CMAQ are provided in [ the section on CMAQ Chemistry and Transport Modules](#CMAQ_Chemistry-Transport_Model_Science_Modules "wikilink") and in Byun and Ching (1999), Byun and Schere (2006). Those two sources also describe the primary aerosol emissions species that are supported by CMAQ (“aerosol” refers to particulate matter [tiny solid or liquid particles] suspended in the atmosphere.). It is possible to add new emissions species to CMAQ that are not supported in the distributed version of the software by using the chemical mechanism compiler (CHEMMECH) that is one of the CMAQ utility programs (see [ Section on CHEMMECH and CSV2NML](##CHEMMECH_and_CSV2NML "wikilink")).

**`A` `Note` `about` `the` `CMAQ` `Output` `File` `Format`**
`CMAQ uses a modified version of the netCDF file format.`
`Although CMAQ output is described as being in the netCDF format,`
`it is actually a hybrid format of the I/O API and the netCDF. `

CMAQ was designed from the start as a community model. “Community modeling” is the concept that air quality model development should be a collective effort by a broad community of developers. By adopting a standardized modeling architecture, the air quality modeling community can focus its efforts on creating software enhancements and new science modules. CMAQ’s modular structure facilitates customization and open-source development by the community. Using the Input/Output Applications Programming Interface (I/O API) library ([<https://www.cmascenter.org/ioapi/documentation/3.1/html/AA.html>](https://www.cmascenter.org/ioapi/documentation/3.1/html/AA.html)) to control the internal and external data flows to the model, and the network Common Data Form (netCDF) library ([<http://www.unidata.ucar.edu/software/netcdf>](http://www.unidata.ucar.edu/software/netcdf)) to control the input and output file formats, CMAQ is based around a transparent and platform-independent code infrastructure that promotes extensibility. The modularity of CMAQ also leads to multiple science configuration options (i.e., how various processes are represented) that model users can choose from when setting up new simulations. The trade-off for this flexibility is complexity in the model configuration; the model user is faced with hundreds of different configuration combinations when designing new simulations. To avoid confusing new CMAQ users, this document provides guidance on a basic configuration to use for getting started with the model. For experienced air quality model users, the multiple configuration combinations available provide an unprecedented level of flexibility for optimizing model performance for different air quality model applications.

CMAQ is designed to meet the needs of the multiple groups contained within the air quality modeling community: research and regulatory modelers, algorithm and science module developers, air quality forecasters, and planners and policy makers. While each of these groups has distinct individual requirements of CMAQ, they also share a common need for an efficient, transparent, and scientifically credible tool to simulate air pollution formation and transport. To address these individual and common needs, CMAQ development and maintenance have the following goals:

1.  *Scientific Integrity* – Ensure that the model remains state-of-the-science through use of regular peer reviews
2.  *Community Development* – Utilize a design that encourages innovations and enhancements by all members of the air quality modeling community
3.  *Multiscale Modeling* – Provide adequate technical formulations to address air quality issues on multiple spatial scales, from urban to hemispheric
4.  *One-Atmosphere Design* – Provide robust and integrated science for modeling multiple, coupled air quality issues in a single simulation
5.  ''Modularity ''– Maintain flexibility to add new or select from existing science modules to optimize model performance for specific applications
6.  *Transparency* – Utilize programming practices that promote understanding of the model formulation at the source-code level
7.  *Computational Efficiency* – Provide scientifically acceptable results without compro­mising the speed at which the results are generated
8.  ''Open-Source Design ''– Enable no-cost distribution and application by the modeling community

[[Next Chapter|/CMAQ_OGD_overview.md]]
