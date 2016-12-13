[<< Previous Chapter](CMAQ_OGD_ch02_overview) - [Home](CMAQ_OGD_index) - [Next Chapter >>](CMAQ_OGD_ch04_science)

Features of CMAQ for Application Users
======================================

The CMAQ modeling system provides a variety of important features to users who are interested in applying the model for investigating scientific research questions or for regulatory applications such as preparation of State Implementation Plans (SIPs).

-   CMAQ is designed to address the complex interactions among multiple air quality issues simultaneously. Using a one-atmosphere approach to air quality modeling by applying multiscale and multipollutant modeling techniques, CMAQ can provide independent but dynamically consistent predictions of several different pollutants at varying spatial scales. The modularity of the CMAQ design provides an unprecedented level of flexibility in air quality model configuration for optimizing model performance for different applications and spatial resolutions.
-   Close interactions among the development communities for CMAQ and for the meteorology and emissions models provide for a tight integration of the three main components of the air quality modeling system.
-   Serial and multiprocessor execution options allow the application user to optimize the performance of CMAQ on various computer platforms.
-   Community development expedites the expansion of CMAQ’s capabilities through the pursuit of multiple research agendas by a variety of research groups. Application users thus avoid the limitations inherent in having to rely on a single, centralized development group.
-   A comprehensive training program is available through the Community Modeling and Analysis System (CMAS) Center ([<http://www.cmascenter.org>](http://www.cmascenter.org/)). The CMAS Center is a support resource for users of CMAQ and other modeling systems; it is described in [Section 13](#Section13 "wikilink").
-   Members of the large, international community of users connected through the CMAS Center can help each other by sharing data and experiences and providing technical support.

Features of CMAQ for Air Quality Model Developers
-------------------------------------------------

Designed under a community-modeling paradigm, CMAQ is distributed as open-source software engineered with a modular code design to facilitate decentralized development. Built around a layered I/O API and netCDF code framework, CMAQ provides a valuable, flexible platform for testing new science algorithms, chemistry representations, and optimization techniques. CMAQ provides the following features to scientists interested in developing new algorithms or adding science to the model:

-   Developed and distributed following open-source software conventions, CMAQ source code is easily accessible and free to obtain.
-   Designed for modularity, CCTM uses standardized input/output (I/O) routines to facilitate extensibility.
-   The diverse and continually growing community of CMAQ developers provides an excellent forum for discussing development-related topics of all kinds.

New Features in CMAQ version 5.0
--------------------------------

CMAQ version 5.0 contains many new features and improvements over the previous release of the model. A list of the updates to CMAQv5 is provided below. Technical details about these features are contained in the CMAQ Wiki ([<http://www.cmaq-model.org/cmaqwiki>](http://www.cmaq-model.org/cmaqwiki)).

### Aerosol Module

-   Redesign
    -   Eliminated dependencies and duplications across modules
-   AERO6
    -   PM Other speciation
        -   Added 9 new PM<sub>2.5</sub> species: APNCOMI/J, AFEJ, AALJ, SSIJ, ATIJ, ACAJ, AMGJ, AKJ, and AMNJ
        -   Included anthropogenic emissions of ANAJ, ACLJ, AH2OJ, and ANH4J
        -   Changed the names of two species: AORGPA → APOC and A25 → AOTHR
        -   AERO6 expects emissions inputs for 13 new PM species. CCTM will crash if any emitted PM species is not included in the emissions input file. See the CMAQv5 Technical Documentation for a [list of the emissions species](http://www.airqualitymodeling.org/cmaqwiki/index.php?title=CMAQv5.0_PM_emitted_species_list) required with AERO6.
    -   POA aging
        -   Oxidative aging of POC leads to increases in APNCOM
        -   Aging is modeled as a second-order reaction between OH and reduced primary organic carbon
    -   Addition of ISORROPIA2
    -   Addition of an inline windblown dust module
    -   Sulfur chemistry updates
-   Updated SOA yield parameterization (AERO5 and AERO6)

### Gas-phase Chemistry

-   Carbon Bond 05 (CB05)
    -   Replaced existing toluene chemistry in CB05 with updated toluene chemistry (CB05‑TU [Whitten et al., 2010])
    -   Revised rate constants for N<sub>2</sub>O<sub>5</sub> hydrolysis in CB05 based on latest recommendations of IUPAC
    -   Added reactions of toluene and xylene with chlorine radical
-   SAPRC-07 (TB & TC)
    -   Fully updated organic and inorganic reactions
    -   Updated photolysis rates
    -   Use operator species to better represent chemical reactions in low-NO<sub>x</sub> conditions
    -   Additional species with high emissions, high toxicity, or high SOA formation are treated explicitly
-   In-line Photolysis
    -   Moved opacity and photolysis data (absorption cross section and quantum yield data) to a mechanism-dependent ASCII input file that is created by a preprocessor
        -   Allows more flexibility to change/introduce data for photolysis rates or create/modify chemical mechanisms
        -   The ASCII file defines the number of wavelengths and temperature interpolation points, allowing for adjustments to improve the accuracy of the radiative transfer calculation
        -   The CCTM run script specifies the new input file with the environment variable CSQY\_DATA
    -   Added new algorithm that calculates the surface albedo based on land use categories, zenith angle, seasonal vegetation, and snow/sea ice coverage

### Cloud Module

In coordination with MCIPv4.0, the CCTM cloud module was updated to simulate subgrid clouds only when the meteorological driver uses a convective cloud parameterization. The minimum horizontal grid resolution restriction that was present in previous CMAQ versions was removed.

### Vertical Diffusion

-   Updates to the ACM module
    -   Default read of C-staggered wind components from MET\_DOT\_3D
    -   Corrected component-wise shear to properly use B-staggered winds; removed map-scale factor from wind shear calculation
    -   Modified eddy diffusivity for stable conditions. Same modifications will be included in ACM2 in the next release of WRF (v3.4)
    -   Reduced the minimum eddy diffusivity from 0.5 m<sup>2</sup>/s to 0.01 m<sup>2</sup>/s, and from 2.0 m<sup>2</sup>/s to 1.0 m<sup>2</sup>/s for urban areas. Minimum eddy diffusivity in WRF/ACM2 is 0.01 m<sup>2</sup>/s everywhere.

### Vertical Advection

A new method for computing the vertical velocity has been implemented that follows the omega calculation in WRF but uses CMAQ’s advection schemes (PPM) to compute horizontal mass divergence. It is a two-step process in which we first integrate the continuity equation through the column to get the change in column mass and then solve for omega layer by layer using the horizontal mass divergence. See equation 2.27 in the WRF Tech Doc: [<http://www.mmm.ucar.edu/wrf/users/docs/arw_v3.pdf>](http://www.mmm.ucar.edu/wrf/users/docs/arw_v3.pdf). The new scheme is much less diffusive in the upper layers because it is constrained to have zero flux at the model top. However, mass conservation is not guaranteed. Testing has shown very small mass errors.

### Production of Lightning NOx

-   CCTM supports four lightning NO emissions settings:
    -   No lightning NO emissions
    -   Input a 4-D netCDF file with lightning NO emissions calculated off-line
    -   Calculate in-line using flash count detections
    -   Calculate in-line using convective precipitation estimates from WRF/MCIP
-   The lightning NO emissions have a significant impact on summer nitrogen deposition and a minor impact on surface NO<sub>x</sub> concentrations

### Dry Deposition

-   Bidirectional exchange of NH<sub>3</sub>
    -   In-line calculation of fertilizer NH<sub>3</sub> emissions
    -   This feature substantially affects the wet and dry deposition of reduced nitrogen and ambient reduced nitrogen concentrations
-   Bidirectional exchange of Hg<sup>0</sup>
    -   In-line processing of direct natural and recycled Hg<sup>0</sup> emissions
-   Mesophyll resistance added to the CCTM dry deposition routine m3dry
-   MOSAIC
    -   Run-time option (CTM\_MOSAIC) for output of land-use-specific deposition velocity and flux
    -   Includes optional output of stomatal flux estimates (CTM\_FST)
-   NO<sub>2</sub> deposition velocity
    -   Excluded the effects of HONO heterogeneous chemistry on deposition velocities
    -   Mesophyll resistance is now calculated as a function of the Henry’s Law constant

### Structural Updates

-   Redesign of the CGRID model species implementation to use a namelist file
-   Updates to comply with I/O API version 3.1
    -   CMAQv5 requires the use of I/O API version 3.1 or newer
    -   Removed deprecated INCLUDE files (PARMS3.EXT, FDESC3.EXT, and IODECL3.EXT)
    -   Removed all instances of TRIMLEN
    -   Utilize M3UTILIO module

### Multipollutant Modeling

-   Includes multipollutant chemical mechanisms and species for gas, aqueous, aerosols, air toxics, mercury, and sea salt
-   Bidirectional exchange of Hg<sup>0</sup>

### Two-way Coupled WRF-CMAQ

Coupled version of CMAQ that integrates WRF version 3.3 and CMAQv5. In this version, simulated aerosols from CCTM provide direct feedback to the WRF radiation calculations.

Discontinued Features in CMAQ version 5.0
-----------------------------------------

-   Mechanism conversions are no longer supported in ICON and BCON. Input profiles must be prepared in the chemical mechanism terms that will be output by the software.

Known Issues in CMAQ version 5.0
--------------------------------

### general

-   When using the in-line option for biogenic emissions, only 1-h output time steps are supported for the diagnostic file
-   The in-line lightning NO emissions algorithm assumes a 1-h time step for the meteorology data provided in the MCIP files

### GCC

Compiling CMAQ-5 with the [GNU Compiler Collection](http://gcc.gnu.org/) (GCC), including GNU Fortran (gfortran), requires version 4.3 or greater. To determine which version you have, run

`$ gfortran --version`

from your shell. If needed, learn more about

-   [packaged versions](http://gcc.gnu.org/wiki/GFortranDistros) of GCC that ship with, or that are available for, your linux distro
-   [unofficial GCC binaries for linux](http://gcc.gnu.org/wiki/GFortranBinaries#GNU.2BAC8-Linux)
-   [compiling GCC from source](http://gcc.gnu.org/wiki/GFortranBinaries#FromSource)

New Features in MCIP version 4.0
--------------------------------

-   The option to compute dry deposition velocities in MCIP has been removed. The dry deposition velocities are computed exclusively in CMAQ's CCTM.
-   Corrected error in computing map-scale factors for polar stereographic grids when true latitude is not at the pole.
-   Changed calculation of dot-point and face-point latitude and longitude for polar stereographic projection to interpolation to eliminate error in the calculation. (The approximation from interpolation is adequate for CMAQ.)
-   Improved support for long MCIP runs from long WRF runs.
-   Corrected error in propagating canopy wetness from WRF runs to MCIP output where scaling was over air density rather than water density.
-   Added sea ice to output in METCRO2D. Corrected land/water mask so that ice cells are considered water.
-   Added option for precipitation tipping bucket to be used in WRF and processed correctly by MCIP.
-   Changed values of convective precipitation in output to negative (i.e., nonphysical) values if a cumulus parameterization scheme was not used in the meteorological model. This works with a change in the sub-grid cloud scheme in CMAQv5.0.
-   Updated metadata with options from WRFv3.2 and WRFv3.3. Added shallow convection option to metadata.
-   Fully compliant with Fortran 2003 coding standards. Several changes throughout MCIP to eliminate F77 legacy coding practices and upgrade to F90 standards.

[<< Previous Chapter](CMAQ_OGD_ch02_overview) - [Home](CMAQ_OGD_index) - [Next Chapter >>](CMAQ_OGD_ch04_science)
