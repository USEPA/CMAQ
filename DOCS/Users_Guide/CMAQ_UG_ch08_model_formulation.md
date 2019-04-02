
<!-- BEGIN COMMENT -->

[Home](README.md) - [Next Chapter >>](CMAQ_UG_ch05_new_simulation.md)

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
## Grid Configuration

CMAQ is a three-dimensional Eulerian air quality model. The *domain* of a model run (the extent of its area of interest) is divided into three-dimensional cells, the boundaries of which (the *grid* of the domain) must be rigorously and consistently defined for all functional components of the model (e.g., chemistry, emissions, meteorology). Mathematical algorithms describing atmospheric transport and air/surface exchange govern the flow of material into and out of each grid cell. Mathematical algorithms describing chemical reactions and aerosol dynamics govern the production and loss of material contained in each grid cell.

Horizontal (or *2D*) and vertical components of a model run's grid are treated differently. The horizontal grid specification (setting the *x* and *y* dimensions) must be *regular*: the horizontal projection of each grid cell (sometimes referred to as a *pixel*) has the same resolution, and the boundaries of each pixel are time-invariant. By contrast, the vertical grid specification (setting the *z* dimension) need not be regular; it can vary in space and time.

After determining the horizontal and vertical extent of the domain of interest, a meteorological model must be run for a horizontal domain slightly larger than the CMAQ domain. A larger meteorology domain is necessary for distinguishing the meteorological boundary conditions from the CMAQ boundary conditions.

### Supported CMAQ Coordinate Systems

** >> Comment <<** Linking to Wikipedia is really in bad form.

** >> Comment <<** WRF also supports lat/lon, but we have not tested it with CMAQ.  (We probably should.)

The choice of horizontal coordinate system, or map projection, for CMAQ is governed by the input emissions inventories and meteorological model fields, which must agree. [WRF/ARW](http://www.wrf-model.org) support the [Lambert Conformal](https://en.wikipedia.org/wiki/Lambert_conformal_conic_projection), [Polar Stereographic](https://en.wikipedia.org/wiki/Universal_polar_stereographic_coordinate_system), and [Mercator](https://en.wikipedia.org/wiki/Mercator_projection) projections, which can be directly passed to CMAQ.

### Horizontal Grids

Available horizontal grids for a given CMAQ run are defined at runtime by setting the GRIDDESC and GRID\_NAME environment variables to point to an existing grid definition file and to one of the grids defined in the file, respectively. Horizontal grids are defined by the [grid definition file (GRIDDESC)](https://www.cmascenter.org/ioapi/documentation/all_versions/html/GRIDDESC.html), which can be edited by the user (more below).

The extent of the horizontal grid used in CMAQ is limited by the size of the domain of the input meteorology. MCIP and the [I/O API Tools](https://www.cmascenter.org/ioapi/documentation/all_versions/html/AA.html#tools) can be used to *window* subsets of meteorology data. Choosing the appropriate horizontal grid scale and extent for a CCTM run is largely dependent on the issues to be addressed by the modeling. However, practical consideration should also be paid to the relationship between grid size, output file size, and execution times, i.e., output data volume and run times increase as the number of horizontal grid cells increase.

#### CMAQ horizontal grid conventions

** >> Comment <<** Figure is not labeled with a figure number. Figure REALLY needs to be updated because "dot" points are MM5-based. (They are not "dot cells".) EDSS predates SMOKE. (!) This figure (or its replacement) needs to be much better explained.

Grid conventions are specified (at length) by the [I/O API](https://www.cmascenter.org/ioapi/documentation/all_versions/html/GRIDS.html). In summary, users should be aware that CMAQ uses both "cross-point" and "dot-point" grids.


![Figure 4-1. relating cross and dot grids](./images/CMAQ_IOAPI_dot_and_point_grids.jpeg)

"Cross-point" is often abbreviated *CRO*, as in `GRID_CRO_2D`. "Dot-point" is often abbreviated *DOT*, as in `MET_DOT_3D`. Similarly, the user should be aware of the grid's [projection](https://en.wikipedia.org/wiki/Map_projection) units—usually meters, except when using [lat-lon coordinate systems](https://en.wikipedia.org/wiki/Geographic_coordinates#Geographic_latitude_and_longitude).

The terms associated with I/O API grid definitions are listed in [Table 4-1](#Table9-1).



 **Table 4-1. I/O API Grid Type Terms**

|Term|Definition|
|------------|-------------------------------------------------------------------------|
|origin|lower left corner of the cell at column=row=1|
|`X_ORIG`|X coordinate of the grid origin (in projection units)|
|`Y_ORIG` |  Y coordinate of the grid origin (in projection units)|
|`X_CELL`|horizontal resolution parallel to the X coordinate axis (in projection units)|
|`Y_CELL`|horizontal resolution parallel to the Y coordinate axis (in projection units)|
|`NCOLS`|number of grid columns, dimensionality in the X direction|
|`NROWS`|number of grid rows, dimensionality in the Y direction|

CMAQ is distributed with a GRIDDESC file that contains a definition for a 12-km grid covering California that uses a Lambert Conformal Conic projection. The definition of this grid is below.

** >> Comment <<** The benchmark domain is not California anymore.  Not sure that this information belongs here anyway.

- Coordinate: Lambert Conformal
- Latitude 0: 40.0
- Longitude 0: -97.0
- Standard Parallel 1: 33.0
- Standard Parallel 2: 45.0
- X origin = -2,376,000
- Y origin = -792,000
- Rows: 100
- Columns: 72
- dX = 12,000
- dY = 12,000
- Layers = 35

#### Creating or modifying horizontal grids

Creating a grid in CMAQ involves simply adding a few lines of text to the GRIDDESC file. Using a combination of the [I/O API GRIDDESC file format documentation](https://www.cmascenter.org/ioapi/documentation/all_versions/html/GRIDDESC.html) and existing grid definitions as examples, new grids can be defined for CMAQ by adding a coordinate and grid description to the GRIDDESC file. Set the GRID_NAME environment variable in the CMAQ run scripts to point to the name of the new grid.

** >> Comment <<** This information seems to fit better in the MCIP section.

**>>COMMENT<<** Update or remove reference to chapter 7 from old User’s Document

The most common situation for creating a new CMAQ grid definition is encountered when using meteorology and/or emissions data that have not yet been modeled with CMAQ. WRF‑ARW outputs can be run through MCIP to generate a GRIDDESC file that can be input directly to both CMAQ and SMOKE. MCIP includes a set of variables for trimming boundary cells from the WRF output, windowing the WRF domain, and setting the reference latitude of the projection. A description of the MCIP variables is provided in [Chapter 7](CMAQ_OGD_ch07_programs_libraries.md). The MCIP variables for defining horizontal grids are provided below.

-   **BTRIM**: Sets the number of boundary points to remove on each of the four horizontal sides of the MCIP domain. Setting BTRIM = 0 will specify the maximum extent of the input meteorology domain. To remove the WRF‑ARW lateral boundaries, set BTRIM = 5 (recommended). For windowing a subset domain of the input meteorology, set BTRIM = -1; this setting causes BTRIM to be replaced by the information provided by X0, Y0, NCOLS, and NROWS (see below).

-   **X0**: The *x*-coordinate of the lower-left corner of the full MCIP cross-point domain (including the MCIP lateral boundary) based on the input WRF‑ARW domain. X0 refers to the number of grid cells in the east-west direction from the origin of the WRF domain to the origin of the MCIP cross-point domain. This setting is only used when BTRIM = -1.

-   **Y0**: The *y*-coordinate of the lower-left corner of the full MCIP cross-point domain (including the MCIP lateral boundary) based on the input WRF‑ARW domain. X0 refers to the number of grid cells in the north-south direction from the origin of the WRF domain to the origin of the MCIP cross-point domain. This setting is only used when BTRIM = -1.

-   **NCOLS**: Number of columns in the output MCIP domain (excluding MCIP lateral boundaries). This setting is only used when BTRIM = -1.

-   **NROWS**: Number of rows in the output MCIP domain (excluding MCIP lateral boundaries). This setting is only used when BTRIM = -1.

-   **WRF\_LC\_REF\_LAT**: WRF Lambert Conformal reference latitude. Use this setting to force the reference latitude in the output MCIP data. If not set, MCIP will use the average of the two true latitudes.

#### Example BTRIM Calculation

Figure 4-2 shows an example of how the BTRIM calculation works for windowing WRF data. The 12-km grid resolution WRF domain has a lower left corner that is offset from the projection center by 600,000 m West and 1,680,000 m South. The CMAQ/MCIP output domain has an offset (lower left corner) that is 108,000 m East and 1,620,000 m South of the projection center.

The MCIP variables X0 and Y0 set the number of grid cells to "trim" from the WRF to domain to get to the MCIP domain lower left corner. Adding "1" in each of these calculations accounts for the addition of the MCIP lateral boundary.

** >> Comment <<** This explanation is terrible and more complicated than it needs to be.

```
`X0 = |(WRF X origin - MCIP X origin)|/(Grid Resolution) + 1`
`X0 = |(-600,000 - 108,000)|/12,000 + 1`
`X0 = 60`

`Y0 = |(WRF Y origin - MCIP Y origin)|/(Grid Resolution) + 1`
`Y0 = |(-1,680,000 - -1,620,000)|/12,000 + 1`
`Y0 = 6`
```



![Figure 4-2. BTRIM Example](./images/BTRIM_Example.png)

#### Further information on horizontal grids

** >> Comment <<** Delete. This is not necessary and only partially accurate.

-  If the meteorology data have already been processed by MCIP and the GRIDDESC file is missing, the grid definition of the input meteorology (and emissions) can be determined by using the netCDF utility *ncdump* to view the header of one of the I/O API files and then use that information to manually create a GRIDDESC file.
-   Horizontal grid dimensions should be no smaller than 30 rows and 30 columns.
-   External boundary thickness should be set to “1”.
-   A CMAQ grid should be smaller than its parent meteorology grid by at least four grid cells on a side, and preferably by six.
-   Horizontal grid spacing for the parent meteorology grid often has a 3:1 ratio, although other ratios have been employed.

CMAQ Vertical Layers
-----------
** >> Comment <<** Model top has been at least 50 hPa for a very long time.

The vertical structure of CMAQ is inherited from the model used to prepare the meteorological information. WRF-ARW uses a sigma coordinate that is based upon surface pressure, not sea level pressure, and a pressure at the top boundary (e.g., 100 hecto-Pascals). The sigma coordinate is terrain following. WRF is a non-hydrostatic model with a time-varying vertical coordinate.

### Vertical layer resolution

** >> Comment <<** At this point, increasing the vertical resolution is probably a moot point because the user likely already has some WRF output. You'd want this information before you run WRF!

Resolving the surface boundary layer requires high resolution (i.e., shallow vertical layers) near the surface for meteorological simulations. To determine mass exchange between the boundary layer and free troposphere, high resolution near the boundary layer top is also preferable. In addition, different cloud parameter­izations may perform differently depending on the layering structure. Layer definitions should be appropriate for the topographic features of the simulation domain. Aerodynamic resistance, which influences dry deposition velocities, is a function of layer thickness and the boundary layer stability. For emissions processing, the layer thickness affects the plume rise from major stacks. The vertical extent of the surface-based emission effects is determined by the thickness of the lowest model layer for CCTM. For consistency, CCTM should use the same vertical resolution as the meteorological model used to prepare the input data.

### Further information on vertical layers

** >> Comment <<** bullet 1:  This is really misleading and confusing.

** >> Comment <<** bullet 2:  While I absolutely agree with this, I think OAQPS still collapses layers.  We have no published research to back this up, so we may want to soften this statement.

** >> Comment <<** bullet 3:  This is obvious, and I recommend deleting it.  Reading bullets 2 and 3 back-to-back creates an unnecessary conundrum for the users.

-   CMAQ redefines the vertical coordinates to monotonically increase with height, a capability necessary to handle a generalized coordinate system.
-   Although MCIP may be used to reduce the number of vertical layers by collapsing layers, this is ***not recommended,*** as dynamical inconsistencies can develop and lead to misleading results. This is particularly true when cloud processes are important.
-   Increasing the number of vertical layers increases the CPU time and the computational complexity.
-   Computational limits arise from the Courant number limitation of vertical advection and diffusion processes. When using K-theory, a very shallow layer definition increases CPU time tremendously under the convective conditions.

### References for grid and vertical coordinate system topics

** >> Comment <<** Otte and Pleim is 2010, not 2009.  It seems like we should add the references within the chapter rather than hyperlinks in a sub-subsection.

- [On The Definition of Horizontal and Vertical Grids and Coordinates for Models-3](https://www.cmascenter.org/ioapi/documentation/all_versions/html/GRIDS.html)
-   [Chapter 12 (MCIP) of the 1999 Models-3/CMAQ Science document](http://www.cmascenter.org/cmaq/science_documentation/pdf/ch12.pdf)
-   [Otte and Pleim 2009 (in GMD) on MCIP](http://www.geosci-model-dev.net/3/243/2010/gmd-3-243-2010.html)

## Emissions
## 4.1 Introduction

CMAQ introduces emissions of trace gases and aerosols from a variety of important sources (e.g. electric generating utilities, vehicles, fires, trees, dust storms, farms, etc.). Some emissions are applied in the surface layer of the model grid, while others are applied at higher altitudes if, for example, they originate from point source like an elevated stack, or a large forest fire. Many sources that are related to local meteorology may be calculated online in CMAQ. However, most sources, especially anthropogenic ones, are preprocessed using software like the Sparse Matrix Operator Kerner Emissions (SMOKE) Modeling System. Once these external tools have calculated the offline emissions, they may merge them into larger aggregated files. We refer to emissions that are either calculated online or read into CMAQ from a file as emission "streams" (see [Fig. 4c-1](#Figure4c-1)).

<a id=Figure4c-1></a>
``![](./images/Figure4c-1.png "Figure4c-1.png")``

Because CMAQ represents both primary and secondary pollutants, emissions are processed for a subset of the species CMAQ treats. The emissions chemical speciation must be compatible with the chemical mechanism chosen for CMAQ (e.g. cb6r3_ae7_aq) because different mechanisms represent large compounds like functionalized hydrocarbons with different formulae. CMAQv5.3 has introduced new features that make the process of mapping emissions species to CMAQ species more transparent and flexible (see Emission Control with DESID[link]). In fact, users can now toggle, modify, and augment emissions from all available streams in order to better tailor their simulations to the questions they are asking CMAQ to help answer. For tutorials covering specific tasks, please see the DESID tutorial page [link].

## 4.2 Emission Streams
Depending on the nature of any stream and the information used to quantify its emissions, it may be treated as one of three types:

***4.2.1 Online Stream:***
CMAQ will calculate the emission rates from this source using information about local meteorology, land characteristics, etc. The streams available for running Online in CMAQ are: biogenics (BEIS) [link], marine gas[link], lightning NO [link], wind-blown dust [link], and sea-spray[ link].

***4.2.2 Gridded Stream (offline):***
CMAQ will read emission rates from an input file, which is organized into an array that is identical in shape to the grid CMAQ is running. Typically these rates are stored at hourly time points and are then interpolated within CMAQ to each time step. Some common examples of Gridded emissions include:

- Mobile sources such as passenger vehicles, trains, ships, scooters, etc.
- Low-level point source emissions that are not large enough to be treated individually
- Residential heating
- Consumer product use (e.g. adhesives, personal care products, pesticides, etc.)
- Agricultural (e.g. burning, dust, animal waste, etc.)
- Road, Construction and mechanically generated dust
- Biogenic VOCs (if not calculated online with BEIS)

Users add Gridded emissions to a simulation via the RunScript. First the variable N_EMIS_GR must be set to the number of Gridded Streams to be used:

```
setenv N_EMIS_GR 3
```

The RunScript must also specify the location of the input files using three-digit suffixes for the stream number:

```
setenv GR_EMIS_001 /home/user/path-to-file/emiss_stream_1_${DATE}.nc
```

and the short-name label to be used to refer to the Stream in logfiles:

```
setenv GR_EMIS_LAB_001 MOBILE
```
If N_EMIS_GR is set 0, then CMAQ will run with no Gridded emissions even if the values for GR_EMIS_XXX and GR_EMIS_LAB_XXX are all set.

***4.2.3 Inline Stream (offline):***
For these streams, emission rates and stack characteristics are provided for many individual sources on the same file. CMAQ uses the stack information to calculate important quantities like the injection height online taking into account local meteorology. A specific latitude/longitude pair is given for each source to locate it in the CMAQ grid.  Some common examples of Inline emissions include:

- Stacks (electric generation units, industrial sources, manufacturing, etc.)
- Forest fires
- Large prescribed fire events  

Users add Inline emissions to a simulation via the RunScript. First the variable N_EMIS_PT must be set to the number of Inline Streams to be used:

```
setenv N_EMIS_PT 3
```
The RunScript must also specify the location of the input files using three-digit suffixes for the stream number:

```
setenv STK_EMIS_002 /home/user/path-to-file/inline_emiss_stream_2_${DATE}.nc
```

The location to the "stack file" with static information about the properties of each source on the stream:
```
setenv STK_GRPS_002 /home/user/path-to-file/inline_stack_groups_2.nc
```
and the short-name label to be used to refer to the Stream in logfiles:
```
setenv STK_EMIS_LAB_002 POINT_FIRES
```
If N_EMIS_PT is set 0, then CMAQ will run with no Inline emissions even if the values for STK_EMIS_XXX, STK_GRPS_XXX and STK_EMIS_LAB_XXX are all set.

## 4.4 Online Emission Streams
### 4.4.1 BEIS


### 4.4.2 Marine Gas

### 4.4.3 Lightning NO

### 4.4.4 Wind-Blown Dust

### 4.4.5 Sea Spray

** >>COMMENT<< ** Start old EMissions section:

** >>COMMENT<< ** The biogenic emissions section needs some work.  There are many concepts and file that are not defined well.  Also, cite Jesse's paper on BEIS.

See the [CMAQv5.2.1 release notes](../../CCTM/docs/Release_Notes/README.md#emissions) for updates on the emissions algorithms in CMAQ.

CMAQ includes several in-line options for calculating and processing emissions in the CCTM. The in-line emissions options in CMAQv5 include the following:

-   The BEIS3 biogenic emissions model can be used to calculate emissions from vegetation and soils. This biogenic model is based on the same model that is included in SMOKE. User documentation for BEIS can be found in [Chapter 6.17 of the SMOKE manual](https://www.cmascenter.org/smoke/documentation/4.5/html/ch06s17.html). The temporal allocation of the biogenic emissions is included in CMAQ. However, the calculation of the gridded normalized emissions for winter and summer is a time independent calculation and must be done with normbeis3 prior to running the inline biogenic option in CMAQ. The user must either provide a BIOSEASON file for simulations that are not summer only or winter only (e.g., multiple seasons, spring, fall) or set the SUMMER_YN flag to Y for summer or N for winter. Without the BIOSEASON file, all biogenic emissions will be calculated using summer factors or winter factors. Additionally, when using the inline biogenic option, the user must point to the SOILOUT file from one day’s simulation as the SOILINP file for the next day. The user must also decide whether to write over SOILOUT files from previous days or create a uniquely named SOILOUT file for each day. The latter approach is recommended if the user wishes to retain the capability to restart simulations in the middle of a sequence of simulations.
-    Plume rise can be calculated for large point sources. Plume rise can be calculated in-line within CMAQ provided the emission files have been processed with SMOKE for in-line processing. The NPTGRPS sets the number of “sectors” for which the user wishes to provide a stack_groups file and an inline emissions file. Optionally, the user can request 2 optional output diagnostic files that include a 3-D file of the emissions with plume rise included and a layer fractions (PLAY) file that includes the fractional amount of emission per model layer.
-   Windblown dust emissions can be estimated using meteorology and land-cover data.
-   Updated sea salt emissions. In AERO6 sea salt emissions in the accumulation mode are speciated into Na, Cl, SO4, Ca, K, and Mg. All cations in the coarse-mode sea salt (i.e., Na, Ca, K, and Mg) are lumped into a species called ASEACAT.

[Add Ben's new section: /home/bmurphy/models/cmaq/User_Guide_v5.3/CMAQ_UG_ch04c_emissions.md]
## Transport
See the [CMAQv5.2.1 release notes](../../CCTM/docs/Release_Notes/README.md#transport) for updates on the transport algorithms in CMAQ.

Pollutant transport includes both advection and sub-grid-scale diffusion. Advection has to do with pollutant transport that is due to the mean wind fields, while diffusion involves sub-grid-scale turbulent mixing of pollutants. If a pollutant plume is transported primarily by advection, then it may travel a long distance without much change in pollutant concentrations. On the other hand, if a plume is transported primarily by diffusion, then the pollutants will mix more quickly and nearer to the source, which will result in substantial changes to pollutant concentrations.

#### Advection ####
** >>COMMENT<< ** This really gets pretty technical for an operational guide.

** >>COMMENT<< ** last para:  What is omega?

In CCTM, the advection process is divided into horizontal and vertical components. This distinction is possible because mean atmospheric motion is mostly horizontal. Often, the vertical motion is related to the interaction of dynamics and thermodynamics. The advection process relies on the mass conservation characteristics of the continuity equation. Data consis­tency is maintained for air quality simulations by using dynamically and thermodynamically consistent meteorology data from MCIP. When the meteorological data and the numerical advection algorithms are not exactly mass consistent, one needs to solve a modified advection equation (Byun, 1999).

The horizontal advection scheme for CMAQ is the piecewise parabolic method (PPM) (Colella and Woodward, 1984). This algorithm is based on the finite-volume subgrid definition of the advected scalar. In PPM, the subgrid distribution is described by a parabola in each grid interval. PPM is a monotonic and positive-definite scheme. Positive-definite schemes maintain the sign of input values, which in this case means that positive concentrations will remain positive and cannot become negative. These codes are implemented in a global mass-conserving scheme introduced in v4.6 that is similar to the one used in the air quality forecasting version of CMAQ. Inspired by discussions with Robert Yamartino of Cambridge Environmental, the method uses the PPM scheme for horizontal advection, deriving a vertical velocity component at each grid cell that satisfies the continuity equation using the driving meteorological model’s air density.

The vertical advection modules solve for the vertical advection with no mass-exchange boundary conditions at the bottom or top of the model. CMAQ also uses PPM as its vertical advection module. Starting in CMAQv5.0, a new method for computing the vertical velocity was implemented that follows the omega calculation in WRF but uses PPM to compute horizontal mass divergence. The two-step process first integrates the continuity equation through the vertical column to get the change in column mass and then solves for omega layer by layer using the horizontal mass divergence (see equation 2.27 in [the WRF ARWv3 Technical Note](http://www2.mmm.ucar.edu/wrf/users/docs/arw_v3.pdf)). In CCTM, the PPM algorithm with a steepening procedure is implemented for vertical advection as the default because of the strong gradients in the tracer species that are observed in photochemical air quality conditions.

#### Diffusion ####
** >>COMMENT<< ** This really gets pretty technical for an operational guide.

** >>COMMENT<< **  Sentence on improvements to ACM2 is not needed.

In CCTM, vertical diffusion is represented by the Asymmetric Convective Method (ACM) of Pleim and Chang (1992). ACM2 (Pleim, 2007), an updated version of ACM, was implemented starting in CMAQv5.0 (note that ACM2 has been updated in CCTM beyond what is reflected in Pleim, 2007). This method recognizes that under convective conditions (when the surface is warming), heated air is transported vertically by buoyancy and mixes with ambient air at each level above the surface until the temperature of the rising air equals the ambient temperature. This process results from fast-moving air in narrow updrafts and slower-moving air in broader downdrafts. Thus, under convective conditions, vertical diffusion is asymmetric. An in-line method for treating biogenic and point-source emissions uses ACM to vertically distribute these emissions during a CMAQ calculation.

Under non-convective conditions (when the surface is cooling), vertical diffusion is represented by an eddy diffusivity approach. Eddy diffusivity is a local mixing scheme and is estimated using the same planetary boundary layer (PBL) similarity-based algorithm as in the Regional Acid Deposition Model (Chang et al., 1987, 1990). In CCTM, the deposition process is simulated as a flux boundary condition that affects the concentration in the lowest vertical model layer. By treating the deposition process as the loss of mass due to the diffusion flux at the bottom of the model, one can relate the bottom boundary condition in the generalized coordinate system to that in the Cartesian coordinate system. CMAQv5 has an improved version of the minimum allowable vertical eddy diffusivity scheme. The new version interpolates between urban and nonurban land cover, allowing a larger minimum vertical diffusivity value for grid cells that are primarily urban.

Horizontal diffusion is implemented with a single eddy diffusion algorithm that is based on local wind deformation and is scaled to the grid cell size. The horizontal eddy diffusivity is assumed to be uniform but dependent on the grid resolution of the model. This diffusivity is larger for a higher-resolution run where the numerical diffusion due to the advection process is smaller.
## Photochemistry

**>>COMMENT<<** Moved inline_phot_preproc, create_ebi and jproc documentation to README.md files in UTIL folder

**>>COMMENT<<** Have Bill H. check for needed updates
**>>COMMENT<<**  First sentence makes no sense.

Photolysis or photodissociation energize and break apart compounds in several key of chemical processes in the atmosphere. It plays in the formation of ozone and particular material that affect human health. Computing the rate of photolysis reactions therefore strongly influences how well an air quality model simulates reality.

The calculation of a photolysis rate must include multiple influences. Clouds, surface features, atmospheric gas and aerosols affect photolysis rates because each scatter and absorb light. A given photolysis reaction depends on molecular properties of the compound involved. Two properties describe the dependence. The absorption cross section represents the effective molecular area of a compound for absorbing solar radiation. The quantum yield gives the chance that the molecule dissociates after absorbing the radiation. Each property depends on the wavelength of the incident radiation, as well as air temperature and density.

The in-line method (Binkowski et al., 2007) is the preferred method for calculating photolysis rates in the CCTM program of CMAQ model system. The method uses aerosol and ozone predicted within a simulation to calculate the solar radation. Two input files support the calculation. The PHOT_OPTICS file describe the optical properties of clouds, aerosols, and the earth’s surface. The OMI file is used to determine how much light is absorbed by atmosphere above the model domain. Both files are included in the released version of CMAQ. Calculating photolysis rates uses an additional input file called the CSQY_DATA file. It contains the cross sections and quantum yields of photolysis rates in a given chemical mechanism. CSQY_DATA files are provided for all chemical mechanisms in a released version of CMAQ. If a user creates a mechanism using new or additional photolysis rates, they have to create a new CSQY_DATA file. The inline_phot_preproc utility produces this file based on the Fortran modules describing the mechanism (see the section on the CHEMMECH utility) and individual files describing the absorption cross-section and quantum yields described for each photolysis reaction.  

The CMAQ modeling system includes an additional method to calculate photolysis rates based on look-up tables. The tables provide a mechanism’s photolysis rates under cloud free conditions based on a fixed meridional cross-sections of atmospheric composition, temperature, density and aerosols. Each table represents rates as a function of altitude, latitude and the hour angle of the sun on a specified Julian date. In model simulations, the method interpolates rates in the table for the date and corrects them to account for clouds described by the meteorological input files.

## Chemistry Mechanisms
**>>COMMENT<<** Moved chemmech and csv2nml documentation to README.md files in UTIL folder

**>>COMMENT<<** A section on chemical mechanisms should precede this section.

** >>COMMENT<< ** This single sentence is underwhelming.

See the [CMAQv5.2.1 release notes](../../CCTM/docs/Release_Notes/README.md#chemistry) for updates on the chemistry algorithms in CMAQ.

#### Gas-phase chemistry solvers
**>>COMMENT<<** Mentioning ODEs seems too technical here.

**>>COMMENT<<** If I were a new user, I'd want to know how to choose from these solvers for my work.

**>>COMMENT<<** Under what circumstances would I create a new chemical mechanism?  What if I'm using ROS3 or SMVGEAR?  Is there a utility like "create_ebi" for that?

To determine the time dependent concentrations of species described by a chemical mechanism, the CCTM uses numerical methods to solve ordinary differential equations representing the chemical transformations. Three solution methods are available and differ in terms of accuracy, generalization, and computational efficiency, and have properties that can substantially affect model run times. They include the Rosenbrock (ROS3) solver (Sandu et al., 1997), the Euler Backward Iterative (EBI) solver (Hertel et al., 1993), and the Sparse Matrix Vectorized GEAR (SMVGEAR) solver (Jacobson and Turco, 1994). SMVGEAR and ROS3 are considered more accurate, in the order listed. Both solutions are labeled as “generalized” because using either only requires the mechanism’s namelist and FORTRAN modules. The EBI solver is more computationally efficient but is less accurate and is not a “generalized” solver so each chemical mechanism requires its own EBI solver; it can still be preferable if accelerated model execution is worth a compromise in accuracy. CMAQ includes EBI solvers for each mechanism definitions file released with a model version. Consult the CMAQ release notes for what mechanisms are in a specific version. If a user creates or modifies a chemical mechanism, they have to create a new EBI solver by using the create_ebi utility.

### Mechanisms

The CMAQ modeling system accounts for chemistry in three phases: a gas phase, aerosols (solid or liquid), and an aqueous phase. Refer to the release notes to find the gas‑phase chemistry mechanisms available in each version of CMAQ. Several variations of the base gas-phase mechanisms, with and without chlorine, mercury, and toxic species chemistry, are distributed with CMAQ. The modularity of CMAQ makes it possible to create or modify the gas-phase chemical mechanism.

Gas-phase chemical mechanisms are defined in CMAQ through Fortran source files. Located in subdirectories of the $CMAQ_MODEL/CCTM/src/MECHS directory (each correspond­ing to a mechanism name), these files define the source, reaction parameters, and atmospheric processes (e.g., diffusion, deposition, advection) of the various mechanism species. The species definitions for each mechanism are contained in namelist files that are read in during execution of the CMAQ programs. The CMAQ mechanism configuration is more similar to the science module configuration than to the horizontal grid or vertical layer configuration in that the mechanism is defined at compilation, resulting in executables that are hard-wired to a specific gas-phase mechanism. To change chemical mechanisms between simulations, a new executable that includes the desired mechanism configuration must be compiled.
### Using predefined chemical mechanisms

To select a predefined mechanism configuration in CMAQ, set the *Mechanism* variable in the build scripts to the name of one of the mechanism directories located under $CMAQ_MODEL/CCTM/src/MECHS. Refer to the [CMAQv5.2.1 Photochemical Mechanisms release notes](https://github.com/USEPA/CMAQ/blob/5.2.1/CCTM/docs/Release_Notes/CMAQv5.2.1_Mechanisms.md) for the list of mechanisms available in CMAQv5.2.1.

### Further information on chemical mechanisms

-   The same chemical mechanism must be used for CCTM and all of the mechanism-dependent input processors that are part of the CMAQ system.
-   The Euler Backward Iterative (EBI) chemistry solver is mechanism-dependent. If a chemical mechanism is modified, then new EBI solver source code must be generated based on the mechanism definition. The CMAQ utility program CREATE_EBI reads the output from CHEMMECH to generate new EBI solver source code.
-   The Rosenbrock and SMVGEAR solvers are mechanism-independent choices of chemistry solvers for the CCTM.
-   When adding new species to CMAQ, it is important to check that the sources of these new species into the modeling domain are accounted for correctly in the mechanism definition files. If species are added to the domain through the emissions files, the namelist files that define the mechanism species must contain these new species.

### Solvers
** >> Comment <<** Where is this in Fig. 7-1?

### Description

The program CREATE_EBI generates Fortran source code for the mechanism-dependent Euler Backward Iterative (EBI) solver. This utility program allows users to create new EBI solver code when mechanisms are modified or added to CMAQ. The source code generated by CREATE_EBI should be used to build versions of the CCTM that use the new or modified chemistry mechanisms.

See [Chapter 9](CMAQ_OGD_ch09_grid_defn.md) for details on how to update existing mechanisms or create new mechanisms in CMAQ.

**>>COMMENT<<** Moved create_ebi to README.md files in UTIL folder

### Files, configuration, and environment variables

To implement a new mechanism in CMAQ, start with a mechanism definition (mech.def) file and CSV species files from an existing mechanism in the model. Edit the mech.def file to include the new reactions, species, and reaction rates and provide this new mech.def file as input to the program [CHEMMECH](#CHEMMECH). CHEMMECH will output a RXNS_DATA_MODULE.F90 file, which is used as input to CREATE_EBI.


Creating or modifying mechanisms in CMAQ requires the use of the CMAQ chemical mechanism compiler, CHEMMECH, to produce the required Fortran source (F90) and namelist files. CHEMMECH translates an ASCII mechanism listing to the F90 and namelist files required by CMAQ. Like all of the CMAQ preprocessors, CHEMMECH is a Fortran program that must be compiled prior to use. Distributed with a Makefile for compilation and run scripts for execution, CHEMMECH reads a mechanism definition (mech.def) file and outputs the mechanism F90 and namelist files. See Chapter 7 for a description of CHEMMECH.

To modify an existing mechanism, copy the mech.def file that is contained in one of the existing mechanism directories to a new directory and modify the mech.def file accordingly. Provide this modified mechanism definition file to CHEMMECH as input to produce the mechanism F90 and namelist files needed to compile CMAQ.  

To invoke this new mechanism in CMAQ, set the *Mechanism* variable in the CMAQ build scripts to the name of the new mechanism directory and compile new executables.

To create a new mechanism for CMAQ, follow a procedure similar to the above for modifying mechanisms. Use an existing mech.def file as a template to format the new mechanism for inclusion in CMAQ. After formatting the mechanism in the form of the mech.def file, provide this file as an input to CHEMMECH to create the required mechanism input files for CMAQ. Move the resulting mechanism files to a new directory under $CMAQ_MODEL/CCTM/src/MECHS. To invoke this new mechanism, set the *Mechanism* variable in the CMAQ build scripts to the name of the new mechanism directory and compile new executables.

The species namelist files define the parameters of the gas, aerosol, non-reactive, and tracer species simulated by the model. The CMAQ programs read the namelist files during execution to define the sources and processes that impact the simulated concentrations of each of the model output species. The namelist files can be used to apply uniform scaling factors by model species for major model processes. For example, emissions of NO can be reduced by 50% across the board by applying a factor of 0.5 to the emissions scalar column of the gas-phase species namelist file. Similarly, the boundary conditions of O<sub>3</sub> can be increased by 50% by applying a factor of 1.5 to the boundary conditions scalar column of the gas-phase species namelist file.

**>>COMMENT<<** Update or remove reference to chapter 8 from old User’s Document

See [Chapter 8](CMAQ_OGD_ch08_input_and_output_files.md) for a description of the format of the namelist file.

When mechanisms are modified or created in CMAQ, new namelist files must be created that include the new species in the mechanism. As described above, the program CHEMMECH will generate namelist files from a mech.def mechanism definition file. Alternatively, existing namelist files can be used as templates to guide the manual creation of new files.

## Aerosol Processes
** >>COMMENT<< ** If this section and its material are retained, add an update for AERO7 and for marine chemistry.

** >>COMMENT<< ** The ISORROPIA details seem a bit much for an OGD.

** >>COMMENT<< ** Make sure the haze section is up to date

See the [CMAQv5.2.1 release notes](../../CCTM/docs/Release_Notes/README.md#chemistry) for updates on the aerosol chemistry algorithms in CMAQ.

Within the air quality community, atmospheric aerosol particles are referred to as particulate matter (PM). PM can be either primary (directly emitted) or secondary (formed in the atmosphere) and from natural or anthropogenic (man-made) sources. Secondary sources include gas-phase oxidation of SO<sub>2</sub> to sulfate, condensation of ammonia and nitrate, and oxidation of gas-phase VOCs such as isoprene, monoterpenes, aromatics, and alkanes. Cloud processes also contribute to the formation of PM; for example, aqueous oxidation of sulfur dioxide in cloud droplets is a significant pathway for production of particulate sulfate. CCTM represents PM using three interacting lognormal distributions, or modes. Two modes (Aitken and accumulation) are generally less than 2.5 microns in diameter while the coarse mode contains significant amounts of mass above 2.5 microns. PM<sub>2.5</sub> and PM<sub>10</sub>, species aggregate metrics within the NAAQS, can be obtained from the model mass concentration and size distribution information.

Particulate matter is removed from the atmosphere by wet scavenging, settling, or by dry deposition at the surface, all of which are modeled by CMAQ. In dry deposition, the transfer is by turbulent air motion and by direct gravitational sedimentation of larger particles. The deposition velocity for particles must be calculated from the aerosol size distribution and from meteorological and land use information. CMAQ’s dry deposition module calculates the size distribution from the mass and number concentration for each of the three modes and then calculates the dry deposition velocity. Particles in the coarse mode are so large that CMAQ explicitly accounts for their gravitational settling as well. In wet deposition, PM is transferred by rainfall. Wet deposition is calculated within CMAQ’s cloud module. Starting in CMAQv4.7, the wet deposition algorithm was modified to include an impaction term in the coarse and accumulation modes.

The 6th generation CMAQ aerosol module (AERO6) expands the chemical speciation of PM. Eight new PM species are added to CMAQ in AERO6: Al, Ca, Fe, Si, Ti, Mg, K, and Mn. Four species that were explicitly treated in previous versions of CMAQ but were not modeled can now be treated as primary anthropogenic species: H2O, Na, Cl, and NH4. The PM emissions mass that remains after speciation into the new components is now input to the model as PMOTHER. AERO6 requires 18 PM emissions species: OC, EC, sulfate, nitrate, H<sub>2</sub>O, Na, Cl, NH<sub>4</sub>, NCOM, Al, Ca, Fe, Si, Ti, Mg, K, Mn, and Other (Reff et al., 2009).

CMAQ includes two options for representing POA: nonvolatile or semivolatile. For the nonvolatile POA configuration, mass is tracked separately in terms of its carbon (OC) and non-carbon (NCOM) content. With this approach, mass can be added to the non-carbon species to simulate the aging of POA in response to atmospheric oxidants. Simon and Bhave (2011) document the implementation of the second-order reaction between primary organic carbon and OH radicals. The semivolatile POA configuration segregates POA into several model species based on a combination of volatility and oxidation state (Murphy et al., 2017). There are five POA species at low oxidation state representing low volatility, semivolatile and intermediate volatility compounds (LVPO1, SVPO1, SVPO2, SVPO3, IVPO1). As the gas-phase species (e.g., VLVPO1) oxidize with OH they form species with higher oxidation state (i.e., LVOO1, LVOO2, SVOO1, SVOO2, SVOO3). The multigenerational aging chemistry for the semivolatile POA configuration is derived from the approach of Donahue et al. (2012) which takes into account the functionalization and fragmentation of organic vapors upon oxidation. The semivolatile POA configuration also includes the option (on by default) of potential secondary organic aerosol from combustion sources (pcSOA). This species is emitted as a VOC (pcVOC) and forms SOA after reaction with OH. The emissions of pcVOC may be zeroed out by the user.

AERO6 uses ISORROPIA in the “reverse mode” to calculate the condensation/evaporation of volatile inorganic gases to/from the gas-phase concentrations of known coarse particle surfaces. It also uses ISORROPIA in the “forward mode” to calculate instantaneous thermodynamic equilibrium between the gas and fine-particle modes. The mass transfer of all semivolatile organic species is calculated assuming equilibrium absorptive partitioning, although some nonvolatile species do exist (e.g., cloud-processed organic aerosol, oligomers, nonvolatile POA (if selected)).

CMAQ can output the reduction in visual range caused by the presence of PM, perceived as haze. CCTM integrates Mie scattering (a generalized particulate light-scattering mechanism that follows from the laws of electromagnetism applied to particulate matter) over the entire range of particle sizes to obtain a single visibility value for each model grid cell at each time step. More detailed descriptions of the PM calculation techniques used in CCTM can be found in Binkowski and Shankar (1995), Binkowski and Roselle (2003), and Byun and Schere (2006).

For easier comparison of CMAQ’s output PM values with measurements, time-dependent cutoff fractions may be output by the model (e.g., Jiang et al., 2006). These include quantities for describing the fraction of each mode that would be categorized as PM2.5 (i.e., PM25AT, PM25AC, and PM25CO) and PM1.0 (i.e., PM1AT, PM1AC, and PM1CO) as well as the fraction of particles from each mode that would be detected by an AMS (i.e AMSAT, AMSAC, and AMSCO). There is also a surface interaction module in the multipollutant version of CMAQ that calculates the flux of mercury to and from the surface (rather than just depositing mercury).

Further discussion on the scientific improvements to the CMAQ PM treatment is available in the release notes for each version of the model.

## Aqueous and Heterogeneous Chemistry
** >>COMMENT<< **  Add info for KMT2?

See the [CMAQv5.2.1 release notes](../../CCTM/docs/Release_Notes/README.md#chemistry) for updates on the heterogeneous chemistry algorithms in CMAQ.

Clouds are an important component of air quality modeling and play a key role in aqueous chemical reactions, vertical mixing of pollutants, and removal of pollutants by wet deposition. Clouds also indirectly affect pollutant concentrations by altering the solar radiation, which in turn affects photochemical pollutants (such as ozone) and the flux of biogenic emissions. The cloud module in CMAQ performs several functions related to cloud physics and chemistry. Three types of clouds are modeled in CMAQ: sub-grid convective precipitating clouds, sub-grid nonprecipitating clouds, and grid-resolved clouds. The meteorological model provides information about grid-resolved clouds, with no additional cloud dynamics considered in CMAQ. For the two types of sub-grid clouds, the cloud module in CCTM vertically redistributes pollutants, calculates in-cloud and precipitation scavenging, performs aqueous chemistry calculations, and accumulates wet deposition amounts. An important improvement in the CMAQv5 convective cloud mixing algorithm corrects a tendency to predict excessive transport from upper layers in the cloud to sub-cloud layers.

CMAQ’s standard cloud chemistry treatment estimates sulfate production from five sulfur oxidation reactions, as well as secondary organic aerosol formation from the reaction of glyoxal and methylglyoxal with the hydroxyl radical.  The distribution between gas and aqueous phases is determined by instantaneous Henry’s law equilibrium, and the bisection method is used to estimate pH (and the distribution of ionic species) assuming electroneutrality. Beginning with CMAQv5.1, two additional cloud chemistry module options, AQCHEM-KMT and AQCHEM-KMTI, were made available along with standard AQCHEM (Fahey et al., 2017). These modules employ a Rosenbrock solver generated using the Kinetic PreProcessor (KPP), version 2.2.3 (Damian et al., 2002) to solve cloud chemistry, ionic dissociation, wet deposition, and kinetic mass transfer between the gas and aqueous phases (Schwartz, 1986). AQCHEM-KMTI also includes an expanded aqueous-phase chemical mechanism that treats SOA formation from biogenic-derived epoxides (Pye et al., 2013) in cloud, in addition to the standard sulfur and -dicarbonyl oxidation reactions. In all cloud chemistry modules, the parameters for cation content of coarse species have been updated to be consistent with the rest of CMAQ.

## Air-Surface Exchange
** >>COMMENT<< ** Add information about multiple deposition modules

** >>COMMENT<< **  Update information on surrogates

** >>COMMENT<< ** The FEST-C/EPIC section needs some work.

** >>COMMENT<< **  Need to state that EPIC can only be used over US.  What if I want bidi and I'm outside the US?

** >>COMMENT<< ** Figure 4-7 Relationship between EPIC and FEST-C is unclear from this figure.  Relationship between meteorological model and EPIC (per the caption) is unclear from this figure.

See the [CMAQv5.2.1 release notes](../../CCTM/docs/Release_Notes/README.md#exchange) for updates on the air-surface exchange algorithms in CMAQ.

CMAQ optionally calculates the wet and dry deposition of chemical species. Information on the algorithms used can be found in Pleim and Ran (2011), Bash et al (2013), and Pleim et al (2013). For deposition to be considered in a model run, the entry in the namelist file must indicate a deposition surrogate species and a deposition factor. The default namelist files contain the standard configuration for known deposition of species. For wet deposition, the scavenging factor (SCAV_FAC) should be set to 1 and the surrogate species (SCAV_SUR) must be one of the chemicals listed in the HLCONST.F subroutine. If the chemical does not have an appropriate surrogate species listed in the HLCONST.F subroutine, one may be added to the model source code. For dry deposition, the deposition factor (DEPV_FAC) should be set to 1 and the deposition velocity surrogate (DEPV_SUR) should be set to one of the species listed in the DEPVDEFN.F subroutine. The species listed in this file are further cross-referenced to a table in ASX_DATA_MOD.F where the diffusivity in air, reactivity, mesophyll resistance, LeBas molar volume, and wet surface scavenging surrogate are specified. The wet surface scavenging surrogate must be a chemical in the HLCONST.F subroutine and is typically the same species that is used for wet deposition of the chemical. If a proper deposition velocity surrogate species does not exist in the tables, one can be added to the model source code.

A runtime flag in the CMAQ model controls whether the bidirectional modeules for ammonia and mercury are invoked. The bidirectional modules simulate two-way exchange between the atmosphere and the surface for these species (as opposed to only deposition). The mercury bidirectional module (Bash 2010) is part of the CMAQv5 multipollutant configuration. To use the bidirectional option for ammonia, additional input file are required. The files are created from the Environmental Policy Integrated Climate (EPIC) model (Cooter et al., 2012). There are two time-independent files which provide information on the soil and the landcover. A time-dependent file contains information on fertilizer application method and amount.



![](./images/Figure4-11.png "Figure4-7.png")

**Figure 4-4. Data flow between EPIC, the meteorological model, and CMAQ from Cooter et al. (2012)**

An additional configuration option related to deposition is the CMAQ_MOSAIC runtime option. This option outputs land use specific deposition velocities and fluxes.

In previous versions of CMAQ, the effects of HONO heterogeneous chemistry on deposition velocities were included. These effects were removed in CMAQv5.0.
<!-- BEGIN COMMENT -->

[Home](README.md) - [Next Chapter >>](CMAQ_UG_ch05_new_simulation.md)<br>
CMAQ User's Guide (c) 2019<br>

<!-- END COMMENT -->
