<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch12_sulfur_tracking.md) - [Home](README.md) - [Tables and Figures >>](CMAQ_UG_tables_figures.md)

<!-- END COMMENT -->

# 13. WRF-CMAQ Coupled Model

## 13.1 Introduction
Air quality models are typically run in two different ways:
* Standalone – Archived output from a meteorological model is used to drive the air quality model.
* Coupled – The air quality and meteorological models are run simultaneously and the chemistry can impact the weather.

The latter “coupled” method is beneficial for studying important interactions between present of aerosol and weather. For example, aerosols can affect the amount of sunlight that reaches the surface, thus impacting temperature (aerosol direct effect). Aerosols also have important impacts on cloud formation (aerosol indirect effect) and cloud albedo. CMAQ has been coupled to the Weather Research and Forecasting (WRF) model for this purpose with the emphasis on aerosol direct effect. In addition, meteorological information is passing to CMAQ in a high frequency than standalone mode. 

In the WRF-CMAQ coupled model (Wong et al., 2012), WRF and CMAQ are simultaneously integrated and information from CMAQ, like aerosol concentration, is passed into WRF so that the present of aerosol can impact the weather. Specifically, the CMAQv5.3 coupled model gives users the options to pass aerosol optical properties to the radiation modules in WRF (aerosol direct radiative effects).  The ability to pass aerosol information into the cloud microphysics routines (aerosol indirect effects; Yu et al, 2014) is currently under development and will be available in a future release.   

## 13.2 Aerosol Direct Radiative Feedback Effects
Aerosol information from CMAQ is transferred to the meteorological model, WRF.  Wavelength dependent aerosol optical properties (extinction, single scattering albedo, and asymmetry factor) are estimated using aerosol composition and size distribution information simulated by CMAQ in conjunction with an algorithm based on Mie theory.  Black carbon is treated by the core-shell approach developed by Frank Binkowski based on Bohren and Huffman (1983). This has been implemented in the shortwave Rapid Radiative Transfer Model for General Circulation Models (RRTMG) radiation scheme in WRF, where aerosol optical properties are calculated for 14 wavelength bands (Clough et al. 2005). The aerosol optics calculations in the WRF-CMAQ model were assessed through comparison to measured optical properties of ambient aerosols made during the Carbonaceous Aerosol and Radiation Effects Study (CARES) as detailed by Gan et al. (2015a).

## 13.3 Application and Evaluation 
The ability of the coupled WRF-CMAQ system to reproduce historical trends in the tropospheric aerosol burden, aerosol optical depths, and clear-sky short wave radiation across the northern hemisphere and the U.S., has recently been assessed through extensive comparisons of long-term simulations of these quantities with observation-derived records from 1990 to 2010 (Xing et al. 2015a,b; Gan et al., 2015b). The model captured declining Aerosol Optical Depth (AOD) trends along with the corresponding decreased top-of-atmosphere (TOA) short-wave radiation (SWR), or  upwellingHelpupwellingThe upward movement of an air mass in the atmosphere., and increased surface SWR, or downwellingHelpdownwellingThe downwelling movement of an air mass in the atmosphere., in the eastern US, Europe and the northern Atlantic for the period of 2000–2010. Estimates of the aerosol direct radiative effects (ADE) at TOA were comparable with those derived from measurements and, compared to general circulation models, the model exhibited better estimates of surface-aerosol direct radiative efficiency (Eτ) (Xing et al., 2015b).

Additionally, top-of-atmosphere clear-sky shortwave radiation during 2000-2010, inferred from the NASA Cloud and Earth’s Radiant Energy System (CERES) satellite retrievals show decreasing trends in the eastern U.S. and increasing trends in eastern China. The inclusion of ADE in WRF-CMAQ yielded better agreement with these contrasting trends suggesting that the trends in clear-sky radiation are influenced by trends in the tropospheric aerosol burden.

Impacts of aerosol cooling are not limited to changes in surface temperature, since variation in atmospheric dynamics caused by the increased stability can worsen local air quality and impact human health.

Hemispheric WRF-CMAQ model simulation over two decades (1990−2010) shows enhanced surface PM2.5 concentrations in the most polluted regions of the world due to the aerosol direct effect.


## 13.4 Build Instructions

- Download WRF 4.1.1 and unzip it (recommend command: `tar xfz the_zip_file`). At the end of this step, you will see a new directory WRFV4 and rename it
   to WRFV411.
- Configure WRF by typing `configure`. This creates a configure.wrf file.
- If you have never done WRF configure before, here are some guidelines:
   1. If the configure script does not find the NETCDF path, follow the prompt to enter the explicit NETCDF include path and library path.
   2. Option selection determines by choosing an approriate compiler vendor and hardware description on right most column of the displayed option table and intercept with the third column (dmpar).  Example: for INTEL (ifort/icc), selection is 15 (you might consider 66 or 70 for a specific hardware if you own).
   3. In the compile for nesting section, choose the default value.
- Download IOAPI 3.2 and install it.
- Go through regular building CMAQ model process. Make sure bldit.cctm script  have:

    `set MakeFileOnly` line uncomment out

    `set build_twoway` line uncomment out

- After running the blidit.cctm script, rename BLD_\* as cmaq and move it into WRFV411 directory.

 - Download coupled model tarball from the EPA ftp site: ftp://newftp.epa.gov/exposure/CMAQ/V5_2/WRF-CMAQ_Coupled_Model/ (file name="WRF411_CMAQ5.3_TwoWay_Model.tar.gz"). Unzip the tarball and then move the twoway directory inside WRFV411 as well.

- Go into directory WRFV411 and execute the following command:

   `twoway/assemble`

   This command will update all necessary files in WRF and CMAQ to create
   the twoway model. You can find the original files inside twoway/misc/orig
   directory.

- Note:  
   1. You might need to edit the IOAPI and MPI paths in configure.wrf base on
      the location of those two libraries on your system.

   2. You can also replace the netCDF link with explicit netCDF path under
      LIB_EXTERNAL and NETCDFPATH. Copy configure.wrf to configure.wrf.saved.
      Once you have this done and in the future if you type `clean -a`, you
      can restore the configure file by `cp configure.wrf.saved configure.wrf`
      without going through configure step again.

   3. In the future, when you bring in a newer version of cmaq, you just
      need to type:

      `twoway/assemble m`

      and a Makefile.twoway will be created inside that new cmaq directory.

- Compile the twoway model by typing `compile em_real >& mylog`. This might take some time for completion and you can monitor size changes of file, mylog. If compilation is done successfully, you can find main/wrf.exe file.

## 13.5 WRF-CMAQ Input/Output Data and Run Instructions
A test dataset is available from the [CMAS Center Software Clearinghouse](https://www.cmascenter.org/download/software/cmaq). After logging in to the CMAS Center, select Download -> Software -> CMAQ and choose version 5.3.  Click on "Download Datasets" and then browse to the folder WRF411-CMAQ53_Coupled_Model to download model input and reference output (WRFv4.1.1_CMAQv5.3_Input.tar.gz, WRFv4.1.1_CMAQv5.3_Output.tar.gz). Once you unpack the files, you can store them anywhere you want.  

A sample run script, twoway_model_run_script, is in the WRFv4.1.1_CMAQv5.3_TwoWay_Model.tar.gz under the script subdirectory.
In order to use this script to run the WRF-CMAQ two-way model, you need to modify two variables: WRF_DIR and INPDIR.
    In general, the area in between

`# ##### begin user define area #####`

and

`# ##### end user define area #####`

can be modified to suit a particular simulation.

The WRF-CMAQ benchmark data provide examples of the files needed to run the model. The general list of inputs required for WRF-CMAQ include,

* REAL outputs :: wrfbdy, wrflowinp, wrffdda, wrfsfdda, wrfrstrt
* CMAQ inputs  :: emissions, IC, BC, OMI, ocean file

WRF-CMAQ outputs standard WRF (wrfout) and CMAQ output files.

If you have any question, please contact David Wong at wong.david-c@epa.gov

## 13.6 References

Clough, S.A., Shephard, M. W., Mlawer, E. J., Delamere, J. S., Iacono, M. J., Cady-Pereira, K., Boukabara, S., & Brown, P. D. (2005). Atmospheric radiative transfer modeling: a summary of the AER codes. J. Quant. Spectrosc. Ra., 91, 233–244.

Gan, C., Binkowski, F., Pleim, J., Xing, J., Wong, D-C., Mathur, R., & Gilliam, R. (2015a). Assessment of the Aerosol Optics Component of the Coupled WRF-CMAQ Model using CARES Field Campaign data and a Single Column Model. Atmospheric Environment, 115, 670-682. doi: 10.1016/j.atmosenv.2014.11.028 

Gan, C., Pleim, J., Mathur, R., Hogrefe, C., Long, C., Xing, J., Wong, D-C., Gilliam, R., & Wei, C. (2015b). Assessment of long-term WRF–CMAQ simulations for understanding direct aerosol effects on radiation "brightening" in the United States. Atmospheric Chemistry and Physics, 15, 12193-12209. doi: 10.5194/acp-15-12193-2015 EXIT

Mathur, R., Pleim, J., Wong, D., Otte, T., Gilliam, R., Roselle, S., Young, J. (2011). Overview of the Two-way Coupled WRF-CMAQ Modeling System. 2011 CMAS Conference, Chapel Hill, NC. Presentation available from the CMAS conference website. 

Wong, D.C., Pleim, J., Mathur, R., Binkowski, F., Otte, T., Gilliam, R., Pouliot, G., Xiu, A., and Kang, D. (2012). WRF-CMAQ two-way coupled system with aerosol feedback: software development and preliminary results. Geosci. Model Dev., 5, 299-312. doi: 10.5194/gmd-5-299-2012

Yu, S., Mathur, R., Pleim, J., Wong, D., Gilliam, R., Alapaty, K., Zhao, C., Liu, X. (2014). Aerosol indirect effect on the grid-scale clouds in the two-way coupled WRF-CMAQ: model description, development, evaluation and regional analysis.  Atmos. Chem. Phys., 14, 11247–11285. doi: 10.5194/acp-14-11247-2014

For an overview of the 2-way Coupled WRF-CMAQ see: http://www.cmascenter.org/conference/2011/slides/mathur_overview_two-way_2011.pptx

and for more details on the 2-way Coupled WRF-CMAQ system see: http://www.cmascenter.org/conference/2011/slides/wong_wrf-cmaq_two-way_2011.pptx
<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch12_sulfur_tracking.md) - [Home](README.md) - [Tables and Figures >>](CMAQ_UG_tables_figures.md)<br>
CMAQ User's Guide (c) 2019<br>

<!-- END COMMENT -->
