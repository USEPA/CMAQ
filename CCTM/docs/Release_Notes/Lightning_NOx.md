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
1. Hourly gridded NLDN lightning strike data (24 hours/day)
2. Parameter file – ocean mask, climatological IC/CG ratios, and parameters for parameterization scheme when NLDN data are not available (e.g. forecast and future climate studies). Details to be presented next

Output Diagnostic Files (optional):
1. Vertical profile (3D) and
2. Column total (2D)

Run Script Configurations:

+ setenv LTNGNO "InLine"  
+ setenv LTNGPARAM Y  
+ setenv USE_NLDN Y  
+ setenv LTNGOUT1 $OUTDIR/${EXEC}"\_LTNGDIAG1".${CTM_APPL}  
+ setenv LTNGOUT2 $OUTDIR/${EXEC}"\_LTNGDIAG2".${CTM_APPL}  
+ if($USE_NLDN == 'Y') then  
    setenv NLDN_STRIKES $NLDN_LTpath/NLDN.12US1.${today}.ioapi  
  else  
    setenv LOG_START 2.0  (RC value to transit linear to log linear)  
  endif  
+ setenv LTNGPARMS_FILE $IN_LTpath/LTNG_AllParms_12US1.ncf

Gridded NLDN Lightning Strike Data:

Hourly data over the CONUS 12km domain in IOAPI format (direct input to CMAQ) are available and able to provide data for any other domains if domain description files are provided. Contact Daiwen Kang (kang.daiwen@epa.gov, 919-541-4587) for details.


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
