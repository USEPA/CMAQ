<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch03_features.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch05_sys_req.md)

<!-- END COMMENT -->


# Science Overview #
** >>COMMENT<< ** 2nd para. "application communities" is somewhat jargon.  I would think that the primary goal to state here is to serve the regulatory operations of the EPA, even if that's not how it works in practice.

** >>COMMENT<< ** If possible, add hyperlink for Byun and Schere.

As discussed in [Chapter 1](CMAQ_OGD_ch01_intro.md), CMAQ is a multipollutant, multiscale air quality modeling system that estimates the transport and chemistry of ozone, PM, toxic airborne pollutants (referred to as “air toxics”), and acidic and nutrient pollutant species, as well as visibility degradation and deposition totals. CMAQ includes state-of-the-art technical and computational techniques to simulate air quality from urban to global scales. It can model complex atmospheric processes affecting transformation, transport, and deposition of air pollutants using a system architecture that is designed for fast and efficient computing.

This chapter summarizes the CMAQ modeling system framework and science features in various components of the CMAQ system, including MCIP, ICON, BCON, CHEMMECH, and CCTM. More detailed discussions on these features can be found in [Byun and Ching (1999)](https://www.cmascenter.org/cmaq/science_documentation/) and Byun and Schere (2006).

## Features Implemented to Achieve the Goals of CMAQ


### Multiple pollutants and multiple scales
** >>COMMENT<< ** para 1:  "One-atmosphere" needs to be defined.

** >>COMMENT<< **  para 1:  Atmospheric chemistry talk got really intense, really fast.

** >>COMMENT<< **  para 2:  "Multiscale" is not limited to spatial scales.


CMAQ is a dramatic improvement over the earlier, single-pollutant models. The CMAQ system provides state-of-the-science capabilities for modeling multiple air quality pollutants/issues in a single simulation, including tropospheric ozone, PM, air toxics, visibility, and acidic and nutrient pollutant species. The “one-atmosphere” approach is important because the various chemical species interact. For example, ozone and hydroxyl radicals react with emitted species such as anthropogenic and biogenic organics to generate secondary PM species. These PM species can interact with solar radiation to alter photolysis rates, temperature, ventilation, winds, thermal reactions, and temperature- and windspeed-dependent emission rates.

The multiple spatial scale (multiscale) capabilities of CMAQ enable applications from local to hemispheric scales. By combining this multiscale feature with the temporal flexibility of the model, users can perform simulations to evaluate annual and interannual pollutant climatology, as well as shorter-term transport from localized sources. To implement multiscale capabilities in CMAQ, several different issues have been addressed, such as scalable atmospheric dynamics and generalized coordinates that depend on the desired model resolution. CMAQ’s generalized coordinate system is used so that meteorological fields on different horizontal and vertical coordinates can easily be accommodated and multiple scales can be simulated with the same CTM. The Jacobian used by the generalized coordinate system encapsulates the necessary grid and coordinate transformations between the physical and computational spaces (cf., Byun, 1999).

### Modular flexibility
** >>COMMENT<< ** Need to completely redo without emphasis on "modular" and made current.  

CMAQ’s current coding structure is based on a modularity level that distinguishes from each other CCTM’s main driver, science modules, data estimation modules, and control/utility subroutines. Also distinguished from each other are the science models (including submodels for meteorology, emissions, chemistry-transport modeling) and the analysis and visualization subsystems.

In CCTM, the process modules that affect the pollutant concentration fields are classified as listed below. Each bullet contains a description of the process followed by *module name* in parentheses. These modules, with the exception of gencoor, are discussed further later in this section.

Science Modules:

-   Horizontal advection (*hadv*)
-   Vertical advection (*vadv*)
-   Mass conservation adjustments for advection processes (*adjc*)
-   Horizontal diffusion (*hdiff*)
-   Vertical diffusion (*vdiff*)
-   Gas-phase chemical reaction solver (*gas*)
-   Aqueous-phase reactions and cloud mixing (*cloud*)
-   Aerosol dynamics and size distributions (*aero*)
-   Potential vorticity scaling for stratosphere/troposphere exchange (*pv_o3*)
-   Meteorology-chemistry coupling (*twoway*)

Control/Utility Modules:

-   Model data flow and synchronizing of fractional time steps (*driver*)
-   Model horizontal grid system (*grid*)
-   Unit conversion (*couple*)
-   Initialization (*init*)
-   MPI/parallelization (*par*)
-   CGRID configuration (*cgrds*)
-   Process analysis (*procan*)
-   Species namelist utilities (*spcs*)
-   Miscellaneous functions (*util*)

Data Estimation Modules:

-   Deposition velocity estimation (*depv*)
-   Photolytic rate computation (*phot*)

In-line Emissions Modules:

-   Calculate emissions (biogenics, dust, lightning, sea salt, plume rise) in-line (*emis*)
-   In-line BEIS3 biogenic emissions (*biog*)
-   In-line plume rise (*plrise*)

The CMAQ modularity makes it easy to modify or introduce a specific scientific process in CCTM. For example, the *gas* module contains several options for different gas-phase chemistry solvers that can be used to optimize model performance. Without the modular structure, changes to just one scientific process could entail having to modify source code throughout CCTM, thereby greatly increasing the risk of human error.

### Quality control features
** >>COMMENT<< ** Need to rewrite without emphasis on "modularity".  Also not sure how peer review really serves as QC.  There are better ways that we can tout QC, such as our internal reviews of the code in GitHub.  Maybe we also need a more formal process…but that's beyond the scope of this activity.

The CMAQ system was designed to minimize the potential for model simulation error in several significant ways:

- The [formal CMAQ peer review process](https://www.epa.gov/cmaq/cmaq-publications-and-peer-review) implemented by EPA ensures that the model retains scientific credibility. Also, informal “review” of the modeling system occurs day-to-day as the broad international user community applies CMAQ for a wide variety of scientific questions and in locations other than North America.
- The modularity of the scientific processes in CMAQ makes modifications and adaptations to the user’s needs more straightforward. The potential for error is minimized because the user is not required to change code or declare variables within program modules outside the one of immediate interest.

## CMAQ Input Processors
** >>COMMENT<< ** This section is totally redundant with the information in section 7. Since the preprocessors are not really core CTM processes I suggest deleting section 4.2 or merging with the information in section 7 on the same pre-processors.

** >>COMMENT<< ** Opening line needs to be updated, and full section needs to be made consistent with an updated Fig. 4-2.

CCTM uses data from other models and CMAQ input processing programs as input for model simulations [Figure 4-2](#Figure4-2).

** >>COMMENT<< ** Figure 4.2 Needs to be updated to be more complete.

<a id="Figure4-2"></a>

![](./images/Figure4-2.png "Figure4-2.png")

**Figure 4-2. CMAQ Chemistry-Transport Model (CCTM) and pre-processors**

The input data for CCTM are developed using the three pre-processors shown in grey in [Figure 4-2](#Figure4-2). All of the CMAQ programs shown in [Figure 4-2](#Figure4-2) (bordered by the broken line) require five basic configuration options:

-   Case – a unique character string that identifies the simulation
-   Grid (Domain and size) – a definition of the horizontal modeling grid that includes the location relative to a fixed map projection and the size of the domain
-   Projection – defines a horizontal plane on the spherical surface of the earth, used to specify the general location of the modeling grid on the globe
-   Vertical Structure – a definition of the layer boundaries for the vertical grid
-   Chemical Mechanism – the name of the photochemical mechanism, aerosol chemistry mechanism, and aqueous chemistry mechanism used for the CMAQ simulation

The choices for these options and how they are selected for each of the CMAQ programs are detailed in [Chapter 7](CMAQ_OGD_ch07_programs_libraries.md).

** >>COMMENT<< ** Need to clarify roles of ICON and BCON because they are not used every time or for every case.  SMOKE is not always used with CCTM, especially outside the US.

CMAQ uses MCIP software to process output from the meteorological model and prepare it for CCTM. The ICON and BCON processors generate the initial and boundary conditions for a CCTM simulation. Emissions for CMAQ must be prepared with an emissions data processing system (SMOKE) that generates emissions for direct input to CCTM. Brief descriptions of the various CMAQ pre-processors are presented in this section. Also described is the CHEMMECH processor, not shown in [Figure 4.2](#Figure4-2).

### MCIP: Meteorology-Chemistry Interface Processor

### ICON and BCON: The initial and boundary conditions processors


### CHEMMECH: Chemical mechanism compiler


### Lightning NO processing in CMAQ
** >>COMMENT<< ** Update sentence about the availability of the parameter file (should not say "with the NLDN hourly flash data")

** >>COMMENT<< ** Need to be sure this is still accurate (and recommended for use).

** >>COMMENT<< **  The reliance on K-F needs to be better explained here.  This means K-F in WRF; there is no provision to use LNO in CMAQ if K-F was not used in WRF.  How is this information communicated from WRF to MCIP to CCTM?

CMAQ is instrumented to estimate the impacts of NO emissions from lightning on air quality. Details of the CCTM lightning NO capability are described in [Chapter 7](CMAQ_OGD_ch07_programs_libraries.md) and in the [CMAQv52 Release Notes](../../CCTM/docs/Release_Notes/Lightning_NOx.md). Emissions of lightning NO can either be generated inline or read in as an external input file that contains 3-D NO data. There are two ways to estimate lightning NO emissions in the CCTM:
* Use observed hourly lightning flash count data from National Lightning Detection Network (NLDN). A lightning parameter file also contains the ocean mask and ICCG data (climatological data for the ratio between inter-cloud to cloud-to-ground flashes); the ocean mask and ICCG data are used in both inline production schemes. The parameter file is available with the NLDN hourly flash data. The default lightning NO production rate is set to 350 moles per flash for both CG and IC flashes, but these values can be modified through the CCTM environment variables (MOLSNCG and MOLSNIC).
* Use linear (log-linear) parameters derived from historical NLDN data and model predicted convective precipitation from the Kain-Fritsch convective scheme. This option is available when observed hourly flash count data (e.g., NLDN) are not available, such as air quality forecasts and future climate applications.

### CALMAP: Crop calendar map preprocessor


## CCTM: The CMAQ Chemistry-Transport Model
** >>COMMENT<< ** Consider moving details in this section to an updated CMAQ Science Document, rather than having them be part of the Users' Guide.  This will help focus the Users' Guide and will allow the content in the Users' Guide to be updated on a different schedule than the content in the Science Document.

** >>COMMENT<< ** Figure 4-6: Need to show which of these processes would be invoked only with user options to turn them on.

** >>COMMENT<< ** Throughout this section:  Lines at top of subsections referring readers to CMAQv5.2 release notes are annoying, but they also would need to be updated to v5.3.

** >>COMMENT<< ** Throughout this section: Need to de-emphasize changes that are part of CMAQv5.  We are on our 3rd major update to CMAQv5.

[Figure 4‑6](#Figure4-10) illustrates the CMAQ modeling system used to simulate the chemistry and transport of pollutants. This figure also shows how CMAQ relates to other parts of an air quality modeling system: the meteorological model, emissions model, and analysis software. To perform a model simulation, CMAQ needs input data, including meteorological and emissions data. Using this information, CCTM simulates each of the atmospheric processes that affect the transport, transformation, and removal of ozone, particulate matter, and other pollutants. CMAQ uses state-of-the-science techniques to simulate these processes, as described below.

<a id="Figure4-6"></a>

![](./images/Figure4-10.png "Figure4-6.png")

**Figure 4-6. CMAQ chemistry-transport model and associated preprocessors**

### Gas-Phase Chemistry


#### Photolysis

### Pollution Transport



### Particulate matter (aerosols) ###


### Clouds and aqueous-phase chemistry ###


### Deposition


### Emissions


### Process analysis

See the [CMAQv5.2.1 release notes](../../CCTM/docs/Release_Notes/README.md#procan) for updates on the process analysis algorithms in CMAQ.



## The CMAQ User Interface
** >>COMMENT<< ** "The CMAQ User Interface" refers to run scipts, GitHub and compilers.  This is out of place and should be deleted or moved to another section. Also GitHub, CMAS center, etc are not really part of the interface.

** >>COMMENT<< ** CMAS Center does not really support CMAQ with those compilers; CMAQ has been tested with those compilers.

** >>COMMENT<< ** last para:  I don't think you can really deviate here.  You have to use the same compiler, and often the same version of that compiler…at least for the libraries and the code that is pulling them in.

The CMAQ user interface that is distributed with the model source code consists of a series of C-shell scripts for building and running the various CMAQ programs on Linux operating systems. These scripts function primarily to set environment variables that are required by the program Bldmake or by the CMAQ program executables. The scripts can be adapted to work with any Linux shell scripting language (e.g., Bash, Bourne).

CMAQ source code can be viewed and downloaded from the [EPA CMAQ GitHub repository](https://github.com/USEPA/CMAQ). Alternatively, tarballs can be downloaded from [the CMAS Center website](https://www.cmascenter.org/cmaq/index.cfm). Each of CMAQ’s programs has separate build and run scripts. The build scripts are used to compile the source code into binary executables. The run scripts are used to set the required environment variables and execute the CMAQ programs. The user can manipulate the CMAQ scripts using a Linux text editor such as [emacs](https://en.wikipedia.org/wiki/Emacs), [gedit](https://en.wikipedia.org/wiki/Gedit), [nano](https://en.wikipedia.org/wiki/GNU_nano), or [vi](https://en.wikipedia.org/wiki/Vi). There are certain options that need to be set at compilation, and some that can be set before running a simulation. Details about using the scripts to build and run CMAQ are described in [Section 5](#CMAQ_System_Requirements_and_Installation), with further details in [Section 7](#CMAQ_Programs_and_Libraries).

The CMAS Center currently supports CMAQ on Linux systems using the Gnu, Portland Group, and Intel Fortran compilers. Community members are encouraged to share their experiences porting CMAQ to other operating systems and compilers.

**CMAQ users are strongly urged** to use the *same* Fortran compiler for *all* components of the CMAQ system, including the netCDF and I/O API libraries on which CMAQ depends.

## References for Chapter 4: Science Overview
** >>COMMENT<< ** References are too difficult to keep updated.  Suggest removing, but can include links to CMAQ website page on publications or How to Cite CMAQ somewhere else in the document.  Since these references are cited in this section would have to change the write-out to remove specific references.

Bash, J. O., E. J. Cooter, R. L. Dennis, J. T. Walker, and J. E. Pleim, 2013: Evaluation of a regional air-quality model with bidirectional NH3 exchange coupled to an agroecosystem model. *Biogeosciences*, **10**, 1635-1645.

Bash, J.O., 2010, Description and initial simulation of a dynamic bi-directional air-surface exchange model for mercury in CMAQ, *J. Geophys. Res.*, **115**, D06305

Binkowski, F.S., and U. Shankar, 1995: The Regional Particulate Model: Part I. Model description and preliminary results. *J. Geophys. Res*., **100**, 26 191–26 209.

Binkowski, F. S., and S. J. Roselle, 2003: Models-3 Community Multiscale Air Quality (CMAQ) model aerosol component. 1. Model description. ''J. Geophys. Res., **108**, 4183, <doi:10.1029/2001JD001409>.

Binkowski, F.S, , S. Arunachalam, Z. Adelman, and J. Pinto, Examining photolysis rates with a prototype on-line photolysis module in CMAQ, 2007, *J. Appl. Meteor. and Clim.*. **46**, 1252-1256, doi: 10.1175/JAM2531.1

Byun, D. W., 1999: Dynamically consistent formulations in meteorological and air quality models for Multiscale atmospheric studies. Part I: Governing equations in a generalized coordinate system. *J. Atmos. Sci*., **56**, 3789–3807.

Byun, D. W., and J. K. S. Ching, 1999: Science Algorithms of the EPA Models-3 Community Multiscale Air Quality (CMAQ) Modeling System. U. S. Environmental Protection Agency Rep. EPA‑600/R‑99/030, 727 pp. [Available from Office of Research and Development, EPA, Washington, DC 20460.]

Byun, D., and K. L. Schere, 2006: Review of the governing equations, computational algorithms, and other components of the Models-3 Community Multiscale Air Quality (CMAQ) modeling system. *Appl. Mech. Rev.*, **59**, 51–77. <doi:10.1115/1.2128636>

Carlton, A.G., B. J. Turpin, K. Altieri, S. Seitzinger, R. Mathur, S. Roselle, R. J. Weber, 2008. CMAQ model performance enhanced when in-cloud SOA is included: comparisons of OC predictions with measurements, Environ. Sci. Technol. , 42, (23), 8799-8802,

Carlton, A.G., P.V. Bhave, S.L. Napelenok, E.O. Edney, G. Sarwar, R.W. Pinder, G.A. Pouliot, M. Houyoux, 2010: Model Representation of Secondary Organic Aerosol in CMAQv4.7. *Env. Sci. & Techno*. **44 (22)**, 8553-8560.

Chang, J. S., P. B. Middleton, W. R. Stockwell, C. J. Walcek, J. E. Pleim, H. H. Lansford, F. S. Binkowski, S. Madronich, N. L. Seaman, D. R. Stauffer, D. Byun, J. N. McHenry, P. J. Samson, and H. Hass, 1990: The regional acid deposition model and engineering model, *Acidic Deposition: State of Science and Technology*, Report 4, National Acid Precipitation Assessment Program.

Colella, P., and P. L. Woodward, 1984: The piecewise parabolic method (PPM) for gas-dynamical simulations. *J. Comput. Phys*.,'' **54**, 174–201.

Cooter, E.J., Bash, J.O., Benson V., Ran, L.-M., 2012, Linking agricultural management and air-quality models for regional to national-scale nitrogen deposition assessments, *Biogeosciences*, **9**, 4023-4035

Damian, V., A. Sandu, M. Damian, F. Potra, and G.R. Carmichael, 2002: The Kinetic PreProcessor KPP -- A Software Environment for Solving Chemical Kinetics, *Computers and Chemical Engineering*, **26**, 1567-1579.

Donahue, N. M., et al. 2012: A two-dimensional volatility basis set – Part 2: Diagnostics of organic-aerosol evolution. *Atmospheric Chemistry and Physics,* **12(2)**, 615-634.

Edney, E. O., T. E. Kleindienst, M. Lewandowski, and J. H. Offenberg, 2007. Updated SOA chemical mechanism for the Community Multi-Scale Air Quality model, EPA 600/X-07/025, U.S. EPA, Research Triangle Park, NC.

Elterman, L., R. Wexler, and D. T. Chang, 1969: Features of tropospheric and stratospheric dust. *Appl. Optics*, **8**, 893–903.

Fahey, K.M., A.G. Carlton, H.O.T. Pye, J. Baek, W.T. Hutzell, C.O. Stanier, K.R. Baker, K.W. Appel, M. Jaoui, J.H. Offenberg, 2017: A framework for expanding aqueous chemistry in the Community Multiscale Air Quality (CMAQ) model version 5.1, *Geosci. Model Dev.*, **10**, 1587-1605.

Fountoukis, C and A. Nenes, 2007: ISORROPIA II: A computational efficient thermodynamic equilibrium model for K+-Ca2+-Mg2+-NH4+-Na+-SO42—NO3—Cl—H2O aerosols, ''Atmos. Chem. And Phys., **7**, 4639-4659.

Hertel O., R. Berkowicz, J. Christensen, and O. Hov, 1993: Test of two numerical schemes for use in atmospheric transport-chemistry models. *Atmos. Environ.*, **27A**, 2591–2611

Jacobson, M., and R. P. Turco, 1994: SMVGEAR: A sparse-matrix, vectorized Gear code for atmospheric models. *Atmos. Environ.*, **28**, 2991–3003.

Jiang, W., S. Smyth, É. Giroux, H. Roth, and D. Yin, 2006: Differences between CMAQ fine mode particle and PM<sub>2.5</sub> concentrations and their impact on model performance evaluation in the lower Fraser valley. *Atmos. Environ*., **40**, 4973–4985.

Murphy, B. N., et al., 2017: Semivolatile POA and parameterized total combustion SOA in CMAQv5.2: impacts on source strength and partitioning. *Atmospheric Chemistry and Physics Discussions,* 2017: 1-44.

Otte, T. L., and J. E. Pleim, 2010: The Meteorology-Chemistry Interface Processor (MCIP) for the CMAQ modeling system: updates through MCIPv3.4.1. *Geoscientific Model Development*, **3**, 243-256.

Pleim, J.E., J. O. Bash, J. T. Walker, and E. J. Cooter, 2013. *J. Geophys. Res.*, **118**, 3794-3806.

Pleim, J. E., and J. S. Chang, 1992: A non‑local closure model in the convective boundary layer. *Atmos. Environ.*, **26A**, 965–981.

Pleim, J.E., and L. Ran, 2011: Surface Flux Modeling for Air Quality Applications. *Atmosphere*, **2**, 271-302.

Pleim, J. E., A. Xiu, P. L. Finkelstein, and T. L. Otte, 2001: A coupled land-surface and dry deposition model and comparison to field measurements of surface heat, moisture, and ozone fluxes. *Water Air Soil Pollut. Focus*, **1**, 243–252.

Pleim, J, 2007: A combined local and nonlocal closure model for the atmospheric boundary layer. Part I: model description and testing, *J. of Appl Met. and Climatology*, **46**, 1383-1395

Pye, H.O.T., R.W. Pinder, I.R. Piletic, Y. Xie, S.L. Capps, Y.H. Lin, J.D. Surratt, Z.F. Zhang, A. Gold, D.J. Luecken, W.T. Hutzell, M. Jaoui, J.H. Offenberg, T.E. Kleindienst, M. Lewandowski, E.O. Edney, 2013: Epoxide pathways improve model predictions of isoprene markers and reveal key role of acidity in aerosol formation, *Environ. Sci. Technol.*, **47(19)**, 11056-11064.

Reff, A., P.V. Bhave, H. Simon, T.G. Pace, G.A. Pouliot, J.D. Mobley, M. Houyoux, 2009: Emissions inventory of PM2.5 trace elements across the United States, *Env. Sci. & Technol*. **43**, 5790-5796.

Sandu, A., J. G. Verwer, J. G., Blom, E. J. Spee, G. R. Carmichael, and F. A. Potra, 1997: Benchmarking stiff ODE solvers for atmospheric chemistry problems. II: Rosenbrock solvers. *Atmos. Environ.*, **31**, 3459–3472.

Schwartz, S.E., 1986: Mass transport considerations pertinent to aqueous-phase reactions of gases in liquid water clouds. In Chemistry of multiphase atmospheric systems, NATO ASI Series, *G6*, 415-471.

Tonnesen, G.S., Dennis, R.L., 2000: Analysis of radical propagation efficiency to assess ozone sensitivity to hydrocarbons and NO x : 1. Local indicators of instantaneous odd oxygen production sensitivity, *J. Geophys. Res.*, **105(D7)**, 9213-9225.

National Oceanic and Atmospheric Administration, 1976: *U.S. Standard Atmosphere*, U.S. Government Printing Office, Washington, DC, NOAA‑S/T76‑1562.

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch03_features.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch05_sys_req.md)<br>
CMAQ Operational Guidance Document (c) 2016<br>

<!-- END COMMENT -->
