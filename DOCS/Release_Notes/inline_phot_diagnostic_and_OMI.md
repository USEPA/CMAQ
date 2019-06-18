# Updates to the inline photolysis diagnostics and OMI data files.

**Author/P.O.C.:**, [William T. Hutzell](mailto:hutzell.bill@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

Changes were made to the in-line photolysis module to accomplish below goals

1. Update the OMI total ozone column data file and its interpolation method.
* Supplement the data file cover to Jan 10, 2018
   * the new file is CCTM/src/phot/inline/OMI_1979_to_2017.dat
* Change the interpolation method.
   * allows the interpolation to vary over time of day
     * currently only depends on Julian day
   * resolution of the OMI data is no longer hard coded but defined in the data file.
       * **<p style='color:red'>the change will cause the version 5.2 of the OMI data file to crash the CMAQ model from an IO error </p>**
       * to use the version 5.2 of OMI file add the below lines at the top of the file

                  nlat     17
                  nlon     17

* Diagnostic files have increased in number and contents
   * The new file PHOTDIAG3 contains optical and radiative results that are new or moved from the old PHOTDIAG2 file
   * The new output variable include the aerosol optical depth in PHOTDIAG1 and interpolated to 550 nm plus Total Extinction, Extinction from gases (Rayleigh scattering, NO<sub>2</sub>, and O<sub>3</sub>),
and Aerosol extinction at the wavelengths use calculate photolysis rates in PHOTDIAG3.
   * New run script environment variables to tailor the numder of layers, NLAYS_PHOTDIAG, and the wavelengths, NWAVE_PHOTDIAG, written to PHOTDIAG2 and PHOTDIAG3 files.
   * Read the below subsection for more information.

##### Description of Diagnostic files from In-line Photolysis

* PHOTDIAG1 File  
   * Surface Values of Optical Inputs and Radiative Results from the In-line calculation of Photolysis Rates
   * The variable AOD_W550_ANGST is not used to calculate photolysis rates and is calculated from aerosol extinction at 550 nm of each layer. See information on the PHOTDIAG3 file.

<p align="center">Table 1. PHOTDIAG1 contents </p>

|Variable Name|Units|Description                                   |
|:----|:----:|:---------------------------------------------|
|         COSZENS|            none|Cosine of Solar Zenith Angle|
|    OZONE_COLUMN|              DU|Observed Total Ozone Column Density|
| TROPO_O3_COLUMN|              DU|Predicted Tropospheric Ozone Column Density|
|            JNO2|           1/min|Photodissociation rate of NO<sub>2</sub>|
|          JO3O1D|           1/min|Photodissociation rate of ozone producing O(1D) |
|  RESOLVED_CFRAC|            none|Resolved Cloud Fraction averaged over cloudy layers|
|   RESOLVED_WBAR| g/m<sup>3</sup>|Resolved Cloud Hydrometeor Content averaged over cloudy layers|
|   SUBGRID_CFRAC|            none|Subgrid Cloud Fraction averaged over cloudy layers|
|    SUBGRID_WBAR| g/m<sup>3</sup>|Subgrid Cloud Hydrometeor Content averaged over cloudy layers|
|   TRANS_DIFFUSE|            none|broad band transmission coefficient for diffuse radiation at surface|
|    TRANS_DIRECT|            none|broad band transmission coefficient for direct radiation at surface|
|      REFLECTION|            none|broad band reflection coefficient at top of atmosphere|
|   CLR_TRANS_DIF|            none|broad band diffuse transmission for clear sky at surface|
|   CLR_TRANS_DIR|            none|broad band direct transmission for clear sky at surface|
|  CLR_REFLECTION|            none|broad band reflection for clear sky at top of atmosphere|
| TROPO_O3_EXCEED|            none|Average Exceedance of modeled ozone column from max fraction of Total Column, Relative Fraction|
| N_EXCEED_TROPO3|            none|# of times predicted tropospheric ozone column exceeds observed total column per time step|
|   ETOT_SFC_W294|      Watts/m<sup>2</sup>|Total Downward Irradiance at surface, 294 nm|
|        AOD_W294|            none|Aerosol Optical Depth, 294 nm|
|  TAU_CLOUD_W294|            none|Cloud Optical Depth, 294 nm|
|    TAU_TOT_W294|            none|Total Optical Depth, 294 nm|
|  TAUO3_TOP_W294|            none|Optical Depth of O<sub>3</sub> above model domain, 294 nm|
|     ALBEDO_W294|            none|Surface Albedo at the wavelength 294 nm|
|   ETOT_SFC_W303|      Watts/m<sup>2</sup>|Total Downward Irradiance at surface, 303 nm|
|        AOD_W303|            none|Aerosol Optical Depth, 303 nm|
|  TAU_CLOUD_W303|            none|Cloud Optical Depth, 303 nm|
|    TAU_TOT_W303|            none|Total Optical Depth, 303 nm|
|  TAUO3_TOP_W303|            none|Optical Depth of O<sub>3</sub> above model domain, 303 nm|
|     ALBEDO_W303|            none|Surface Albedo at the wavelength 303 nm|
|   ETOT_SFC_W310|      Watts/m<sup>2</sup>|Total Downward Irradiance at surface, 310 nm|
|        AOD_W310|            none|Aerosol Optical Depth, 310 nm|
|  TAU_CLOUD_W310|            none|Cloud Optical Depth, 310 nm|
|    TAU_TOT_W310|            none|Total Optical Depth, 310 nm|
|  TAUO3_TOP_W310|            none|Optical Depth of O<sub>3</sub> above model domain, 310 nm|
|     ALBEDO_W310|            none|Surface Albedo at the wavelength 310 nm|
|   ETOT_SFC_W316|      Watts/m<sup>2</sup>|Total Downward Irradiance at surface, 316 nm|
|        AOD_W316|            none|Aerosol Optical Depth, 316 nm|
|  TAU_CLOUD_W316|            none|Cloud Optical Depth, 316 nm|
|    TAU_TOT_W316|            none|Total Optical Depth, 316 nm|
|  TAUO3_TOP_W316|            none|Optical Depth of O<sub>3</sub> above model domain, 316 nm|
|     ALBEDO_W316|            none|Surface Albedo at the wavelength 316 nm|
|   ETOT_SFC_W333|      Watts/m<sup>2</sup>|Total Downward Irradiance at surface, 333 nm|
|        AOD_W333|            none|Aerosol Optical Depth, 333 nm|
|  TAU_CLOUD_W333|            none|Cloud Optical Depth, 333 nm|
|    TAU_TOT_W333|            none|Total Optical Depth, 333 nm|
|  TAUO3_TOP_W333|            none|Optical Depth of O<sub>3</sub> above model domain, 333 nm|
|     ALBEDO_W333|            none|Surface Albedo at the wavelength 333 nm|
|   ETOT_SFC_W381|      Watts/m<sup>2</sup>|Total Downward Irradiance at surface, 381 nm|
|        AOD_W381|            none|Aerosol Optical Depth, 381 nm|
|  TAU_CLOUD_W381|            none|Cloud Optical Depth, 381 nm|
|    TAU_TOT_W381|            none|Total Optical Depth, 381 nm|
|  TAUO3_TOP_W381|            none|Optical Depth of O<sub>3</sub> above model domain, 381 nm|
|     ALBEDO_W381|            none|Surface Albedo at the wavelength 381 nm|
|   ETOT_SFC_W607|      Watts/m<sup>2</sup>|Total Downward Irradiance at surface, 607 nm|
|        AOD_W607|            none|Aerosol Optical Depth, 607 nm|
|  TAU_CLOUD_W607|            none|Cloud Optical Depth, 607 nm|
|    TAU_TOT_W607|            none|Total Optical Depth, 607 nm|
|  TAUO3_TOP_W607|            none|Optical Depth of O<sub>3</sub> above model domain, 607 nm|
|     ALBEDO_W607|            none|Surface Albedo at the wavelength 607 nm|
|  AOD_W550_ANGST|            none|Aerosol Optical Depth at 550 nm based on an Angstrom Interpolation|

* PHOTDIAG2 File
  * Three dimensionals values of Photolysis rates used to make predictions from the In-line calculation of Photolysis Rates.  
  * The run script can set the number of layers for the file by the environment variable, NLAYS_PHOTDIAG. The default value equals all layers of the simulation.
When the run script sets NLAYS_PHOTDIAG, PHOTDIAG2 covers from the first layer to the value of NLAYS_PHOTDIAG.
  * Variables names can change with the photochemical mechanism used because each mechanism is developed with its own rate constants.
  * Files containing cross-section and quantum yield data are in CMAQ repository under subdirectory, UTIL/inline_phot_preproc/photolysis_CSQY_data.
  * The below table gives an example of the PHOTDIAG2 contents when using the cb6r3_ae6_aq mechanism in CMAQ version 5.2.

<p align="center">Table 2. PHOTDIAG2 contents if using the cb6r3_ae6_aq mechanism</p>

|Variable Name|Units|Description                                   |
|:----|:----:|:---------------------------------------------|
|     NO2_IUPAC10|           1/min|Photolysis rates calculated based on data file; NO2_IUPAC10|
|  O3_O3P_IUPAC10|           1/min|Photolysis rates calculated based on data file; O3_O3P_IUPAC10|
|  O3_O1D_IUPAC10|           1/min|Photolysis rates calculated based on data file; O3_O1D_IUPAC10|
|    H2O2_IUPAC10|           1/min|Photolysis rates calculated based on data file; H2O2_IUPAC10|
|       NO3NO2_06|           1/min|Photolysis rates calculated based on data file; NO3NO2_06|
|        NO3NO_06|           1/min|Photolysis rates calculated based on data file; NO3NO_06|
|    N2O5_IUPAC10|           1/min|Photolysis rates calculated based on data file; N2O5_IUPAC10|
|    HONO_IUPAC10|           1/min|Photolysis rates calculated based on data file; HONO_IUPAC10|
|    HNO3_IUPAC10|           1/min|Photolysis rates calculated based on data file; HNO3_IUPAC10|
|     PNA_IUPAC10|           1/min|Photolysis rates calculated based on data file; PNA_IUPAC10|
|     PAN_IUPAC10|           1/min|Photolysis rates calculated based on data file; PAN_IUPAC10|
|    MEPX_IUPAC10|           1/min|Photolysis rates calculated based on data file; MEPX_IUPAC10|
|     NTR_IUPAC10|           1/min|Photolysis rates calculated based on data file; NTR_IUPAC10|
|  FORM_R_IUPAC10|           1/min|Photolysis rates calculated based on data file; FORM_R_IUPAC10|
|  FORM_M_IUPAC10|           1/min|Photolysis rates calculated based on data file; FORM_M_IUPAC10|
|  ALD2_R_IUPAC10|           1/min|Photolysis rates calculated based on data file; ALD2_R_IUPAC10|
|  ALDX_R_IUPAC10|           1/min|Photolysis rates calculated based on data file; ALDX_R_IUPAC10|
|    GLYD_IUPAC10|           1/min|Photolysis rates calculated based on data file; GLYD_IUPAC10|
|   GLY_R_IUPAC10|           1/min|Photolysis rates calculated based on data file; GLY_R_IUPAC10|
|    MGLY_IUPAC10|           1/min|Photolysis rates calculated based on data file; MGLY_IUPAC10|
|     KET_IUPAC10|           1/min|Photolysis rates calculated based on data file; KET_IUPAC10|
|    ACET_IUPAC10|           1/min|Photolysis rates calculated based on data file; ACET_IUPAC10|
|            ISPD|           1/min|Photolysis rates calculated based on data file; ISPD|
|           HPALD|           1/min|Photolysis rates calculated based on data file; HPALD|
|     CL2_IUPAC04|           1/min|Photolysis rates calculated based on data file; CL2_IUPAC04|
|    HOCL_IUPAC04|           1/min|Photolysis rates calculated based on data file; HOCL_IUPAC04|
|    FMCL_IUPAC04|           1/min|Photolysis rates calculated based on data file; FMCL_IUPAC04|
|           CLNO2|           1/min|Photolysis rates calculated based on data file; CLNO2|
|         ACRO_09|           1/min|Photolysis rates calculated based on data file; ACRO_09|

* PHOTDIAG3 File (New)
  * Three dimensionals values of Optical Inputs and Radiative Results used to make predictions from the In-line photolysis calculation.

  * The run script can set the number of layers for the file by the environment variable, NLAYS_PHOTDIAG. The default values equals all layers of the simulation.
If the run script sets NLAYS_PHOTDIAG, PHOTDIAG3 covers from the first layer to the value of NLAYS_PHOTDIAG.
  * The run script can set the wavelengths output for the Aerosol Single Scattering Albedo, Aerosol Asymetry Factor, Total Extinction, Extinction from gases,
Aerosol extinction, and Actinic Flux by using the environment list, NWAVE_PHOTDIAG. If NWAVE_PHOTDIAG is not set or contains no values, PHOTDIAG3 includes all wavelengths. The list
only uses integer truncated values of the wavelengths used to calculated photolysis rates. 
      * Values set by the PHOT_OPTICS.dat file and changing them is strongly not recommended. 
      
      * For CMAQ version 5.2, the list can contain any of the following values, 294, 303, 310, 316, 333, 381, and 607.

      * example of setting for NWAVE_PHOTDIAG,

             setenv NWAVE_PHOTDIAG "294 303 310 316 333 381 607"

  * The variable EXT_AERO_W550 is not used to calculate photolysis rate and is derived from aerosol extinction at the other wavelengths using an Angstrom interpolation.

<p align="center">Table 3. PHOTDIAG3 contents if NWAVE_PHOTDIAG not set </p>

|Variable Name|Units|Description                                   |
|:----|:----:|:---------------------------------------------|
|   AERO_SSA_W294|            none|Aerosol Single Scattering Albedo at 294 nm|
|  AERO_ASYM_W294|            none|Aerosol Asymetry Factor at 294 nm|
|        EXT_W294|          1/Mm|Total Extinction of layer for 294 nm|
|    GAS_EXT_W294|          1/Mm|Total Extinction from Rayleigh Scattering, NO<sub>2</sub>, and Ozone in layer for 294 nm|
|   EXT_AERO_W294|          1/Mm|Aerosol Extinction of layer for 294 nm|
|   AERO_SSA_W303|            none|Aerosol Single Scattering Albedo at 303 nm|
|  AERO_ASYM_W303|            none|Aerosol Asymetry Factor at 303 nm|
|        EXT_W303|          1/Mm|Total Extinction of layer for 303 nm|
|    GAS_EXT_W303|          1/Mm|Total Extinction from Rayleigh Scattering, NO<sub>2</sub>, and Ozone in layer for 303 nm|
|   EXT_AERO_W303|          1/Mm|Aerosol Extinction of layer for 303 nm|
|   AERO_SSA_W310|            none|Aerosol Single Scattering Albedo at 310 nm|
|  AERO_ASYM_W310|            none|Aerosol Asymetry Factor at 310 nm|
|        EXT_W310|          1/Mm|Total Extinction of layer for 310 nm|
|    GAS_EXT_W310|          1/Mm|Total Extinction from Rayleigh Scattering, NO<sub>2</sub>, and Ozone in layer for 310 nm|
|   EXT_AERO_W310|          1/Mm|Aerosol Extinction of layer for 310 nm|
|   AERO_SSA_W316|            none|Aerosol Single Scattering Albedo at 316 nm|
|  AERO_ASYM_W316|            none|Aerosol Asymetry Factor at 316 nm|
|        EXT_W316|          1/Mm|Total Extinction of layer for 316 nm|
|    GAS_EXT_W316|          1/Mm|Total Extinction from Rayleigh Scattering, NO<sub>2</sub>, and Ozone in layer for 316 nm|
|   EXT_AERO_W316|          1/Mm|Aerosol Extinction of layer for 316 nm|
|   AERO_SSA_W333|            none|Aerosol Single Scattering Albedo at 333 nm|
|  AERO_ASYM_W333|            none|Aerosol Asymetry Factor at 333 nm|
|        EXT_W333|          1/Mm|Total Extinction of layer for 333 nm|
|    GAS_EXT_W333|          1/Mm|Total Extinction from Rayleigh Scattering, NO<sub>2</sub>, and Ozone in layer for 333 nm|
|   EXT_AERO_W333|          1/Mm|Aerosol Extinction of layer for 333 nm|
|   AERO_SSA_W381|            none|Aerosol Single Scattering Albedo at 381 nm|
|  AERO_ASYM_W381|            none|Aerosol Asymetry Factor at 381 nm|
|        EXT_W381|          1/Mm|Total Extinction of layer for 381 nm|
|    GAS_EXT_W381|          1/Mm|Total Extinction from Rayleigh Scattering, NO<sub>2</sub>, and Ozone in layer for 381 nm|
|   EXT_AERO_W381|          1/Mm|Aerosol Extinction of layer for 381 nm|
|   AERO_SSA_W607|            none|Aerosol Single Scattering Albedo at 607 nm|
|  AERO_ASYM_W607|            none|Aerosol Asymetry Factor at 607 nm|
|        EXT_W607|          1/Mm|Total Extinction of layer for 607 nm|
|    GAS_EXT_W607|          1/Mm|Total Extinction from Rayleigh Scattering, NO<sub>2</sub>, and Ozone in layer for 607 nm|
|   EXT_AERO_W607|          1/Mm|Aerosol Extinction of layer for 607 nm|
|        CFRAC_3D|            none|Resolved Cloud Fraction in grid cell|
|   EXT_AERO_W550|          1/Mm| Aerosol Extinction of layer for 550 nm, Angstrom Interpolation|
| ACTINIC_FX_W294|      Watts/m<sup>2</sup>|Net Actinic Flux, 294 nm|
| ACTINIC_FX_W303|      Watts/m<sup>2</sup>|Net Actinic Flux, 303 nm|
| ACTINIC_FX_W310|      Watts/m<sup>2</sup>|Net Actinic Flux, 310 nm|
| ACTINIC_FX_W316|      Watts/m<sup>2</sup>|Net Actinic Flux, 316 nm|
| ACTINIC_FX_W333|      Watts/m<sup>2</sup>|Net Actinic Flux, 333 nm|
| ACTINIC_FX_W381|      Watts/m<sup>2</sup>|Net Actinic Flux, 381 nm|
| ACTINIC_FX_W607|      Watts/m<sup>2</sup>|Net Actinic Flux, 607 nm|

## Files Affected
* CCTM/scripts/run_cctm.csh
* CCTM/src/ICL/fixed/filenames/FILES_CTM.EXT
* CCTM/src/phot/inline/AERO_PHOTDATA.F
* renamed and formatted CCTM/src/phot/inline/OMI_1979_to_2015.dat to CCTM/src/phot/inline/OMI_1979_to_2017.dat
* CCTM/src/phot/inline/PHOT_MOD.F
* CCTM/src/phot/inline/phot.F
* CCTM/src/phot/inline/opphot.F
* CCTM/src/phot/inline/o3totcol.f
