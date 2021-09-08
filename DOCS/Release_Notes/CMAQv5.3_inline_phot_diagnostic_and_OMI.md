# Updates to the inline photolysis diagnostics and OMI data files

[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency

## Brief Description

Changes were made to the inline photolysis module to update the OMI total ozone column data and to create more comprehensive photolysis diagnostic files.

1. **OMI total ozone column data:**  The OMI total ozone column data have been extended through 10 January 2018. The OMI data file is now CCTM/src/phot/inline/OMI_1979_to_2017.dat. In addition, the interpolation method now allows ozone to vary over the time of day. The data currently only depend on Julian day. In addition, the resolution of the OMI data is no longer hard-coded; it is now defined in the data file. The latter change denies the use of the CMAQv5.2 OMI data file with CMAQv5.3, unless the following lines are added to the top of the file:

                  nlat     17
                  nlon     17


2. **Updates to PHOTDIAG files:**  The diagnostic photolysis files are now more comprehensive. There are now three photolysis diagnostic files. More detailed information is in the subsections, below.
   * The new output variables include: the aerosol optical depth in *PHOTDIAG1* and interpolated to 550 nm plus total extinction, extinction from gases (Rayleigh scattering, NO<sub>2</sub>, and O<sub>3</sub>),
and aerosol extinction at the wavelengths used calculate photolysis rates in *PHOTDIAG3*.
   * New runscript environment variables tailor the number of layers (NLAYS_PHOTDIAG) and the wavelengths (NWAVE_PHOTDIAG) written to *PHOTDIAG2* and *PHOTDIAG3*.
    * The new file, *PHOTDIAG3*, contains optical and radiative information, some of which is new and some which was moved from *PHOTDIAG2*.

## Data output in the diagnostic files from inline photolysis

* *PHOTDIAG1* (see Table 1)  
   * Surface values of optical inputs and radiative results from the inline calculation of photolysis rates
   * The variable, AOD_W550_ANGST, is not used to calculate photolysis rates and is calculated from aerosol extinction at 550 nm of each layer. See additional information on *PHOTDIAG3*.
   
* *PHOTDIAG2* (see Table 2)
  * Three-dimensional values of photolysis rates used to make predictions from the inline calculation of photolysis rates.  
  * The runscript can set the number of layers for the file by the environment variable, NLAYS_PHOTDIAG. The default value equals all layers of the simulation. When the runscript sets NLAYS_PHOTDIAG, *PHOTDIAG2* includes the lowest-model layer to the value of NLAYS_PHOTDIAG.
  * Variables names can change with the photochemical mechanism used because each mechanism is developed with its own rate constants.
  * Files containing cross-section and quantum yield data are in CMAQ repository in UTIL/inline_phot_preproc/photolysis_CSQY_data.
  
* *PHOTDIAG3* (New; see Table 3)
  * Includes three-dimensional values of optical inputs and radiative results used to make predictions from the inline photolysis calculation.
  * The runscript can set the number of layers for the file using the environment variable, NLAYS_PHOTDIAG. The default values equal all layers of the simulation. If the runscript sets NLAYS_PHOTDIAG, *PHOTDIAG3* includes the lowest model layer to the value of NLAYS_PHOTDIAG.
  * The environment list, NWAVE_PHOTDIAG, defines which wavelengths are output for the variables listed below. If NWAVE_PHOTDIAG is not set or contains no values, *PHOTDIAG3* includes all wavelengths. The list only uses integer truncated values of the wavelengths that are used to calculated photolysis rates.
    * aerosol single scattering albedo
    * aerosol asymmetry factor
    * total extinction
    * extinction from gases
    * aerosol extinction
    * actinic flux
     
  * Values are set by the PHOT_OPTICS.dat file. **Changing these values is strongly not recommended.** 
      
  * For CMAQv5.3, the list can contain data at any of the following wavelengths (nm): 294, 303, 310, 316, 333, 381, and 607.  The environment variable list for NWAVE_PHOTDIAG can be set following:

             setenv NWAVE_PHOTDIAG "294 303 310 316 333 381 607"

  * The variable, EXT_AERO_W550, is not used to calculate photolysis rate and is derived from aerosol extinction at the other wavelengths using an Angstrom interpolation.


<p align="center">Table 1. <i>PHOTDIAG1</i> contents </p>

|Variable Name|Units|Description|
|:----|:--------------:|:---------------------|
|         COSZENS|            none|cosine of solar zenith angle|
|    OZONE_COLUMN|              DU|observed total ozone column density|
| TROPO_O3_COLUMN|              DU|predicted tropospheric ozone column density|
|            JNO2|min<sup>-1</sup>|photodissociation rate of NO<sub>2</sub>|
|          JO3O1D|min<sup>-1</sup>|photodissociation rate of ozone producing O(1D) |
|  RESOLVED_CFRAC|            none|resolved cloud fraction averaged over cloudy layers|
|   RESOLVED_WBAR|g&nbsp;m<sup>&#8209;3</sup>|resolved cloud hydrometeor content averaged over cloudy layers|
|   SUBGRID_CFRAC|            none|subgrid cloud fraction averaged over cloudy layers|
|    SUBGRID_WBAR|g&nbsp;m<sup>&#8209;3</sup>|subgrid cloud hydrometeor content averaged over cloudy layers|
|   TRANS_DIFFUSE|            none|broad band transmission coefficient for diffuse radiation at surface|
|    TRANS_DIRECT|            none|broad band transmission coefficient for direct radiation at surface|
|      REFLECTION|            none|broad band reflection coefficient at top of atmosphere|
|   CLR_TRANS_DIF|            none|broad band diffuse transmission for clear sky at surface|
|   CLR_TRANS_DIR|            none|broad band direct transmission for clear sky at surface|
|  CLR_REFLECTION|            none|broad band reflection for clear sky at top of atmosphere|
| TROPO_O3_EXCEED|            none|average exceedance of modeled ozone column from max fraction of total column, relative rraction|
| N_EXCEED_TROPO3|            none|# of times predicted tropospheric ozone column exceeds observed total column per time step|
|   ETOT_SFC_W294|W&nbsp;m<sup>&#8209;2</sup>|total downward irradiance at surface at 294 nm|
|        AOD_W294|            none|aerosol optical depth at 294 nm|
|  TAU_CLOUD_W294|            none|cloud optical depth at 294 nm|
|    TAU_TOT_W294|            none|total optical depth at 294 nm|
|  TAUO3_TOP_W294|            none|optical depth of O<sub>3</sub> above model domain at 294 nm|
|     ALBEDO_W294|            none|surface albedo at 294 nm|
|   ETOT_SFC_W303|W&nbsp;m<sup>&#8209;2</sup>|total downward irradiance at surface at 303 nm|
|        AOD_W303|            none|aerosol optical depth at 303 nm|
|  TAU_CLOUD_W303|            none|cloud optical depth at 303 nm|
|    TAU_TOT_W303|            none|total optical depth at 303 nm|
|  TAUO3_TOP_W303|            none|optical depth of O<sub>3</sub> above model domain at 303 nm|
|     ALBEDO_W303|            none|surface albedo at 303 nm|
|   ETOT_SFC_W310|W&nbsp;m<sup>&#8209;2</sup>|total downward irradiance at surface at 310 nm|
|        AOD_W310|            none|aerosol optical depth at 310 nm|
|  TAU_CLOUD_W310|            none|cloud optical depth at 310 nm|
|    TAU_TOT_W310|            none|total optical depth at 310 nm|
|  TAUO3_TOP_W310|            none|optical depth of O<sub>3</sub> above model domain at 310 nm|
|     ALBEDO_W310|            none|surface albedo at 310 nm|
|   ETOT_SFC_W316|W&nbsp;m<sup>&#8209;2</sup>|total downward irradiance at surface at 316 nm|
|        AOD_W316|            none|aerosol optical depth at 316 nm|
|  TAU_CLOUD_W316|            none|cloud optical depth at 316 nm|
|    TAU_TOT_W316|            none|total optical depth at 316 nm|
|  TAUO3_TOP_W316|            none|optical depth of O<sub>3</sub> above model domain at 316 nm|
|     ALBEDO_W316|            none|surface albedo at 316 nm|
|   ETOT_SFC_W333|W&nbsp;m<sup>&#8209;2</sup>|total downward irradiance at surface at 333 nm|
|        AOD_W333|            none|aerosol optical depth at 333 nm|
|  TAU_CLOUD_W333|            none|cloud optical depth at 333 nm|
|    TAU_TOT_W333|            none|total optical depth at 333 nm|
|  TAUO3_TOP_W333|            none|optical depth of O<sub>3</sub> above model domain at 333 nm|
|     ALBEDO_W333|            none|surface albedo at 333 nm|
|   ETOT_SFC_W381|W&nbsp;m<sup>&#8209;2</sup>|total downward irradiance at surface at 381 nm|
|        AOD_W381|            none|aerosol optical depth at 381 nm|
|  TAU_CLOUD_W381|            none|cloud optical depth at 381 nm|
|    TAU_TOT_W381|            none|total optical depth at 381 nm|
|  TAUO3_TOP_W381|            none|optical depth of O<sub>3</sub> above model domain at 381 nm|
|     ALBEDO_W381|            none|surface albedo at 381 nm|
|   ETOT_SFC_W607|W&nbsp;m<sup>&#8209;2</sup>|total downward irradiance at surface at 607 nm|
|        AOD_W607|            none|aerosol optical depth at 607 nm|
|  TAU_CLOUD_W607|            none|cloud optical depth at 607 nm|
|    TAU_TOT_W607|            none|total optical depth at 607 nm|
|  TAUO3_TOP_W607|            none|optical depth of O<sub>3</sub> above model domain at 607 nm|
|     ALBEDO_W607|            none|surface albedo at the wavelength at 607 nm|
|  AOD_W550_ANGST|            none|aerosol optical depth at 550 nm based on an Angstrom interpolation|



<p align="center">Table 2. <i>PHOTDIAG2</i> contents from the <i>cb6r3_ae6_aq</i> mechanism</p>

|Variable Name|Units|Description                                   |
|:----|:----:|:---------------------------------------------|
|     NO2_IUPAC10|           min<sup>-1</sup>|Photolysis rates calculated based on data file; NO2_IUPAC10|
|  O3_O3P_IUPAC10|           min<sup>-1</sup>|Photolysis rates calculated based on data file; O3_O3P_IUPAC10|
|  O3_O1D_IUPAC10|           min<sup>-1</sup>|Photolysis rates calculated based on data file; O3_O1D_IUPAC10|
|    H2O2_IUPAC10|           min<sup>-1</sup>|Photolysis rates calculated based on data file; H2O2_IUPAC10|
|       NO3NO2_06|           min<sup>-1</sup>|Photolysis rates calculated based on data file; NO3NO2_06|
|        NO3NO_06|           min<sup>-1</sup>|Photolysis rates calculated based on data file; NO3NO_06|
|    N2O5_IUPAC10|           min<sup>-1</sup>|Photolysis rates calculated based on data file; N2O5_IUPAC10|
|    HONO_IUPAC10|           min<sup>-1</sup>|Photolysis rates calculated based on data file; HONO_IUPAC10|
|    HNO3_IUPAC10|           min<sup>-1</sup>|Photolysis rates calculated based on data file; HNO3_IUPAC10|
|     PNA_IUPAC10|           min<sup>-1</sup>|Photolysis rates calculated based on data file; PNA_IUPAC10|
|     PAN_IUPAC10|           min<sup>-1</sup>|Photolysis rates calculated based on data file; PAN_IUPAC10|
|    MEPX_IUPAC10|           min<sup>-1</sup>|Photolysis rates calculated based on data file; MEPX_IUPAC10|
|     NTR_IUPAC10|           min<sup>-1</sup>|Photolysis rates calculated based on data file; NTR_IUPAC10|
|  FORM_R_IUPAC10|           min<sup>-1</sup>|Photolysis rates calculated based on data file; FORM_R_IUPAC10|
|  FORM_M_IUPAC10|           min<sup>-1</sup>|Photolysis rates calculated based on data file; FORM_M_IUPAC10|
|  ALD2_R_IUPAC10|           min<sup>-1</sup>|Photolysis rates calculated based on data file; ALD2_R_IUPAC10|
|  ALDX_R_IUPAC10|           min<sup>-1</sup>|Photolysis rates calculated based on data file; ALDX_R_IUPAC10|
|    GLYD_IUPAC10|           min<sup>-1</sup>|Photolysis rates calculated based on data file; GLYD_IUPAC10|
|   GLY_R_IUPAC10|           min<sup>-1</sup>|Photolysis rates calculated based on data file; GLY_R_IUPAC10|
|    MGLY_IUPAC10|           min<sup>-1</sup>|Photolysis rates calculated based on data file; MGLY_IUPAC10|
|     KET_IUPAC10|           min<sup>-1</sup>|Photolysis rates calculated based on data file; KET_IUPAC10|
|    ACET_IUPAC10|           min<sup>-1</sup>|Photolysis rates calculated based on data file; ACET_IUPAC10|
|            ISPD|           min<sup>-1</sup>|Photolysis rates calculated based on data file; ISPD|
|           HPALD|           min<sup>-1</sup>|Photolysis rates calculated based on data file; HPALD|
|     CL2_IUPAC04|           min<sup>-1</sup>|Photolysis rates calculated based on data file; CL2_IUPAC04|
|    HOCL_IUPAC04|           min<sup>-1</sup>|Photolysis rates calculated based on data file; HOCL_IUPAC04|
|    FMCL_IUPAC04|           min<sup>-1</sup>|Photolysis rates calculated based on data file; FMCL_IUPAC04|
|           CLNO2|           min<sup>-1</sup>|Photolysis rates calculated based on data file; CLNO2|
|         ACRO_09|           min<sup>-1</sup>|Photolysis rates calculated based on data file; ACRO_09|


<p align="center">Table 3. <i>PHOTDIAG3</i> contents.  All variables are included if NWAVE_PHOTDIAG is not set.</p>

|Variable Name|Units|Description                                   |
|:----|:----:|:---------------------------------------------|
|   AERO_SSA_W294|            none|aerosol single scattering albedo at 294 nm|
|  AERO_ASYM_W294|            none|aerosol asymmetry factor at 294 nm|
|        EXT_W294|          Mm<sup>-1</sup>|total extinction of layer at 294 nm|
|    GAS_EXT_W294|          Mm<sup>-1</sup>|total extinction from Rayleigh scattering, NO<sub>2</sub>, and ozone in layer at 294 nm|
|   EXT_AERO_W294|          Mm<sup>-1</sup>|aerosol extinction of layer at 294 nm|
|   AERO_SSA_W303|            none|aerosol single scattering albedo at 303 nm|
|  AERO_ASYM_W303|            none|aerosol asymmetry factor at 303 nm|
|        EXT_W303|          Mm<sup>-1</sup>|total extinction of layer at 303 nm|
|    GAS_EXT_W303|          Mm<sup>-1</sup>|total extinction from Rayleigh scattering, NO<sub>2</sub>, and ozone in layer at 303 nm|
|   EXT_AERO_W303|          Mm<sup>-1</sup>|aerosol extinction of layer at 303 nm|
|   AERO_SSA_W310|            none|aerosol single scattering albedo at 310 nm|
|  AERO_ASYM_W310|            none|aerosol asymmetry factor at 310 nm|
|        EXT_W310|          Mm<sup>-1</sup>|total extinction of layer at 310 nm|
|    GAS_EXT_W310|          Mm<sup>-1</sup>|total extinction from Rayleigh scattering, NO<sub>2</sub>, and zzone in layer at 310 nm|
|   EXT_AERO_W310|          Mm<sup>-1</sup>|aerosol extinction of layer at 310 nm|
|   AERO_SSA_W316|            none|aerosol single scattering albedo at 316 nm|
|  AERO_ASYM_W316|            none|aerosol asymmetry factor at 316 nm|
|        EXT_W316|          Mm<sup>-1</sup>|total extinction of layer at 316 nm|
|    GAS_EXT_W316|          Mm<sup>-1</sup>|total extinction from Rayleigh scattering, NO<sub>2</sub>, and ozone in layer at 316 nm|
|   EXT_AERO_W316|          Mm<sup>-1</sup>|aerosol extinction of layer at 316 nm|
|   AERO_SSA_W333|            none|aerosol single scattering albedo at 333 nm|
|  AERO_ASYM_W333|            none|aerosol asymmetry factor at 333 nm|
|        EXT_W333|          Mm<sup>-1</sup>|total extinction of layer at 333 nm|
|    GAS_EXT_W333|          Mm<sup>-1</sup>|total extinction from Rayleigh scattering, NO<sub>2</sub>, and ozone in layer at 333 nm|
|   EXT_AERO_W333|          Mm<sup>-1</sup>|aerosol extinction of layer at 333 nm|
|   AERO_SSA_W381|            none|aerosol single scattering albedo at 381 nm|
|  AERO_ASYM_W381|            none|aerosol asymmetry Factor at 381 nm|
|        EXT_W381|          Mm<sup>-1</sup>|total extinction of layer at 381 nm|
|    GAS_EXT_W381|          Mm<sup>-1</sup>|total extinction from Rayleigh scattering, NO<sub>2</sub>, and ozone in layer at 381 nm|
|   EXT_AERO_W381|          Mm<sup>-1</sup>|aerosol extinction of layer at 381 nm|
|   AERO_SSA_W607|            none|aerosol single scattering albedo at 607 nm|
|  AERO_ASYM_W607|            none|aerosol asymmetry factor at 607 nm|
|        EXT_W607|          Mm<sup>-1</sup>|total extinction of layer at 607 nm|
|    GAS_EXT_W607|          Mm<sup>-1</sup>|total extinction from Rayleigh scattering, NO<sub>2</sub>, and ozone in layer at 607 nm|
|   EXT_AERO_W607|          Mm<sup>-1</sup>|aerosol extinction of layer at 607 nm|
|        CFRAC_3D|            none|resolved cloud fraction in grid cell|
|   EXT_AERO_W550|          Mm<sup>-1</sup>|aerosol extinction of layer at 550 nm, Angstrom interpolation|
| ACTINIC_FX_W294|      W&nbsp;m<sup>&#8209;2</sup>|net actinic flux at 294 nm|
| ACTINIC_FX_W303|      W&nbsp;m<sup>&#8209;2</sup>|net actinic flux at 303 nm|
| ACTINIC_FX_W310|      W&nbsp;m<sup>&#8209;2</sup>|net actinic flux at 310 nm|
| ACTINIC_FX_W316|      W&nbsp;m<sup>&#8209;2</sup>|net actinic flux at 316 nm|
| ACTINIC_FX_W333|      W&nbsp;m<sup>&#8209;2</sup>|net actinic flux at 333 nm|
| ACTINIC_FX_W381|      W&nbsp;m<sup>&#8209;2</sup>|net actinic flux at 381 nm|
| ACTINIC_FX_W607|      W&nbsp;m<sup>&#8209;2</sup>|net actinic flux at 607 nm|

## Files Affected
* CCTM/scripts/run_cctm.csh
* CCTM/src/ICL/fixed/filenames/FILES_CTM.EXT
* CCTM/src/phot/inline/AERO_PHOTDATA.F
* CCTM/src/phot/inline/OMI_1979_to_2015.dat (**deleted**)
* CCTM/src/phot/inline/OMI_1979_to_2017.dat (**new**)
* CCTM/src/phot/inline/PHOT_MOD.F
* CCTM/src/phot/inline/phot.F
* CCTM/src/phot/inline/opphot.F
* CCTM/src/phot/inline/o3totcol.f
