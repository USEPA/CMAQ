# A new inline biogenic emissions option (MEGAN)

[Jeff Willison](mailto:willison.jeffrey@epa.gov), U.S. Environmental Protection Agency

## Brief Description

CMAQ now supports inline biogenic emissions from the Model of Emissions of Gases and Aerosols from Nature (MEGAN). This is an alternative to the BEIS model for biogenic VOCs, but users can enable both models. For example, if ISOPRENE is desired from BEIS while APIN from MEGAN is preferred. This can be achieved via the emissions control file. It is not the recommended configuration but could be useful for testing specific sensitivities. 

Like the atmospheric model WRF, MEGAN requires creation of input files using the MEGAN preprocessor. These files are time independent but domain specific. Files for the CONUS, HEMI, and SEBENCH domains are available from EPA. Users must run the preprocessor for other domains, or if they would like to modify the surface data that the input files are based on.

## Significance and Impact

BEIS’ dependence on the Biogenic Emission Landcover Database (BELD3) limits its functionality to North American domains. The new MEGAN option allows for inline biogenic emissions when doing hemispheric or global modeling. 

## Files Affected: 

*  CCTM/src/vdiff/acm2_m3dry/ASX_DATA_MOD.F – enable SOIM2 even when ABFLUX is off
*  CCTM/src/util/util/RUNTIME_VARS.F – read in environmental variables from the run script
*  CCTM/src/cio/centralized_io_module.F – read in MEGAN input fields 
*  CCTM/src/emis/emis/EMIS_DEFN.F – add MEGAN to the BIOG stream options alongside BEIS
*  CCTM/scripts/bldit_cctm.csh – compile MEGAN module
*  CCTM/scripts/run*.csh - include new environmental variables and options
*  CCTM/src/biog/megan3 - new directory with MEGAN code

The environmental variable CTM_BIOGEMIS is deprecated and will no longer enable BEIS.
