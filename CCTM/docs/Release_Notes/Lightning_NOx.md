# Lightning NOx Production Updates

**Author/P.O.C.:**, [Daiwen Kang](mailto:kang.daiwen@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

Two major updates for lightning NOx production are included in the CMAQ v5.2 release:  
1. Use of hourly NLDN lightning data to directly estimate lightning NOx (LNOx) production.  
2. New LNOx parameterization based on the model convective rainfall rate (RC – rain convective) that is designed for future forecasts where observed lightning data are not available.  

## Significance and Impact

The first major update uses raw NLDN data to produce hourly lightning strikes gridded to the model domain. These data are then used to estimate hourly LNOx production. The old CMAQ method used monthly gridded total lightning strike data, which were then scaled by the modeled precipitation rate, to estimate hourly LNOx. Thus, the use of the hourly (instead of monthly) NLDN data improves the timing/location of LNOx production. A test run was made with CMAQv5.1 for July 2011 to examine the impacts of this update. The results revealed that the use of hourly NLDN data improves surface ozone bias and error by 0.5-2 ppb. The improvements are most notable over the southeast United States where lightning events are most prevalent. The update presumably will have positive impacts on ozone and other compounds (e.g., NO2) throughout the depth of the troposphere. A future study will evaluate the update more extensively. However, the results so far are very encouraging and confirm that the update is working correctly (in CMAQv5.1). More test cases would be helpful to support these preliminary results.

The second major update bases the new LNOx parameterization on the linear (and/or log linear) regression relationship between multi-years’ WRF generated convective rainfall and the observed NLDN lightning strike data. The goal of this parameterization was to realistically estimate LNOx in situations where lightning data are not available (e.g., future forecasts). Preliminary results indicated that the new parameterization matched NLDN-derived LNOx better than the previous method. A more in-depth description of the new method and more results can be made available in the near future.

One caveat is that this relationship was derived from WRF simulations that all used the Kain-Fritsch convective parameterization. Therefore, the relationship between convective rainfall and observed lightning derived here might not be applicable to other convective schemes (e.g., Grell). Additionally, the derived relationship is valid over the continental U.S. only, and, therefore, is not applicable to other domains.

## Affected Files:

emis/emis/LTNG_DEFN.F

## Additional Notes:

Input Files:  

1. Hourly gridded NLDN lightning strike data (24 hours/day).  
2. Parameter file – ocean mask, climatological IC/CG ratios, and parameters for parameterization scheme when NLDN data are not available (e.g. forecast and future climate studies). Details to be presented next  

[Download hourly NLDN data and a parameter file](https://www.cmascenter.org/download/data/nldn.cfm) on a CONUS 12km domain for 2004-2013 from the CMAS Center.

Output Diagnostic Files (optional):  
1. Vertical profile (3D)  
2. Column total (2D)  

Run Script Configurations:  

There are four options for the CCTM lightning NOx module.
For retropective model simulations, Option 4 will give the most accurate results. Best results will be achieved if lightning assimilation is also used in the met model.
### 1. No Lightning NOx ###
CMAQ will not generate NO from lightning. 

Settings:
```
setenv CTM_LTNG_NO N or unsetenv CTM_LTNG_NO
```

### 2. Lightning NOx from a 4-D file ###
This option reads in NO defined as a rate of production (moles/sec) for each model layer at each timestep. 

Settings:
```
setenv CTM_LTNG_NO Y
setenv LTNGNO [4-D netCDF file of lightning NO emissions]
```

### 3. Lightning NOx Interpolated from the Convective Precipitation Rate ###
As described above, this option is based on a regression relationship between multi-years’ WRF generated convective rainfall and observed NLDN lightning strike data. Recommended for applications where NLDN data are not available to calculate lightning NOx emissions. The variable LOG_START defines the convective precipation rate (RC) at which to transition from a linear to log-linear relationship between precipitation and lightning flashes.

Settings: 
```
setenv CTM_LTNG_NO Y
setenv LTNGNO Inline
setenv LTNGPARAM  N
setenv USE_NLDN N
setenv LOG_START 2.0
```

### 4. Lightning NOx Derived from Hourly NLDN Flash Counts ###
This option uses hourly flash counts to ensure that the seasonal lightning totals match observed lightning totals. 

Settings: 
```
setenv CTM_LTNG_NO Y
setenv LTNGNO Inline
setenv LTNGPARAM  Y
setenv USE_NLDN Y
setenv LTNGPARMS_FILE [netCDF file of lightning parameters*]
setenv NLDN_STRIKES [netCDF file of hourly strike counts]
```

Gridded NLDN Lightning Strike Data:

Hourly data over the CONUS 12km domain in I/O API netCDF format (direct input to CMAQ) are available and able to provide data for any other domains if domain description files are provided. Contact Daiwen Kang (kang.daiwen@epa.gov, 919-541-4587) for details.

Lightning Parameters File:

Time-independent I/O API netCDF file with the following variables.

- monthly flash totals per CMAQ grid cell
- a grid-cell specific scaling factor for calculating flashes using the convective precipitation rate, such that the calculated flash count matches the monthly total
- ratio of intercloud to cloud to ground flashes
- moles of NO per cloud to ground flash
- moles of NO per inter-could flash
- mask for offshore flashes -- to remove spurious flashes over the ocean

## References:
NA

-----
## Internal Records:

### Relevant Pull Requests:
  [PR #128](https://github.com/usepa/cmaq_dev/pull/128)   
  [PR #142](https://github.com/usepa/cmaq_dev/pull/142)

### Commit IDs:
e1f99c41cce2650878744d783c5c90fb4caf7209   
a1ea3afd51517d85eeded3c9c611811c879cb4d5  
da869d5316357996f1394010a53c79e5a08761be  
f7332d8ee2b2e7d31d7fc6b4da865cd30fe2b8c3  
aacf2a5f333df2f42ca7b63e73d1f8f66d60451e  
9b14c48ad556e71b0f742311672c9ac6873ed8f0  
