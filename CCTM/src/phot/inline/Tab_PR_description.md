*Priority:* Medium

*Description* This pull request attempts to accomplish the below goals regarding the optical properties used in the the inline option for calculating photolysis frequencies.

1. Add a new method for determining the optical properties. The new method should better match properties determined by solving Mie Scattering Theory for spherical particles than the default method (FastOptics) but produce comparable model runtimes.
2. Simplify how the model runtime options set how aerosol optical properties are calculated. The change combines the two options currently used in one option.
3. Change the contents in the photolysis diagnostic files one and three. The change intends to add a way evaluate the optical propertes against observations or theory.

*Significant:* The Pull Request induces a new option for aerosol optics properties for calculating photolysis frequencies. The method better matches solving Mie Scattering Theory for uniformly mixed spherical aerosols but has a lower computational cost.

*Impact on Model Results:* None because FastOptics remains the default option for aerosol optics.


#### Background

In the CCTM build option for inline calculation of photolysis frequencies, the aerosol optics method determine how the aerosol optic properties are calculated for the direct aerosol feedback. The fastest and default option (FastOptics) uses case approximations of Mie Scattering Theory for a uniformly mixed sphere whose refractive index is a volume weighted averaged of the aerosol modal component's refractive indices. The cases are based on the Mie Parameter ($2\pi r/\ \lambda$ ) and the average refractive index. In the current model, the two other options exist.  One is the Mie Solution (MieCalc) for uniformly mixed sphere. The other use a mixing model or representation of the internal structure where a uniformly mixed shell surrounds an black carbon core (CoreShell) if the aerosol mode has a black carbon component or the component's volume composes more than one billionth of the modal volume. Otherwise, an aerosol mode optical properites are determine by FastOptics or MieCalc based on setting of runtime options. Each method's computational expense follows an order as listed in Tables 2 and 3. How well each method represents aerosol optical properties also varies. FastOptics approximates MieCalc and has mean baises around 10% against MieCalc but also shows spatial pattern not predicted by MieCalc. CoreShell may better represent the internal structure of aerosols but model simulations do not show large differences against aerosol optics properties from MieCalc.

The option selected affects model evaluation because the predicted aerosol optical depth (usually at 550 nm) is often compared against observations. Ideally, FastOptics should show small relative difference against the MieCalc of aerosol optical depth across wavelength. The below plot show how these differences vary between 5% to 15%.

######   Hemospheric Domain June 25, 2018   


![Jun_25_2018_FastOptics_vs_MieCalc_AOD_W310_Jun_25_2018_Layer1_TilePlots_layer1](https://github.com/bhutzell/CMAQ_Dev/assets/16845494/5e35109b-920e-44c2-ad31-b736c1c37230)


![Jun_25_2018_FastOptics_vs_MieCalc_AOD_W550_ANGST_Jun_25_2018_Layer1_TilePlots_layer1](https://github.com/bhutzell/CMAQ_Dev/assets/16845494/c2d9d936-439b-4f92-a6f8-e19b5f6199cf)


#####  12US1 Domain June 25, 2018    


![June_25_2018_FastOptics_vs_MieCalc_AOD_W310_June_25_2018_Layer1_TilePlots_layer1](https://github.com/bhutzell/CMAQ_Dev/assets/16845494/2cd40642-6fa6-4a10-a6e4-daf3e495569d)


![June_25_2018_FastOptics_vs_MieCalc_AOD_W550_ANGST_June_25_2018_Layer1_TilePlots_layer1](https://github.com/bhutzell/CMAQ_Dev/assets/16845494/ec649c83-1876-4c76-92ed-e04868eb09bf)





<center>Wavelength Dependence of Normalized Mean Bias</center>
<center> Aerosol Optical Depth FastOptics against Mie Calculation    

| Wavelength (nm) | NMB: HEMI June 25, 2018 |  12US1 June 25, 2018 |    
|:----------------------:|:-------------------------------------:|:-------------------------------------:|    
|   294 |  13.3%  | 19.4%  |     
|   303   |  12.8%   | 17.8%  |   
|   310  |  12.4%   | 16.4%   |    
|   316  |  12.0%   | 15.1%   |  
|   333 |  11.0%  | 11.8% |   
|   381 |  7.8%  | 3.8%  |    
|   550 (Interpolation via Angstrom Exponent)  |  -2.19%  | -7.8% |  
|   607 |  -5.2%  | -11.2% |    


 </center>
 
 
#### New Method for Aerosol Optical Properties.

The Pull Request adapts a look-up table and interpolation method (MieTab) for aerosol optical properteis described by Fast et al. (2005) and implemented in  [module_fastj_mie](https://github.com/wrf-model/WRF/blob/master/chem/module_fastj_mie.F) of the WRF-CHEM model version 4.5. Wavelength and aerosol refactive index are the independent variables of the table. The dependent variable is coefficients from curving fitting on Mie Scattering Solutions over aerosol radius for a fixed wavelength and refractive index. A bilinear interpolation estimates the fitting functions' coefficients at the aerosol refractive index during a model timestep and  specified wavelength.

The adaptation modifies the Fast et al. method for the tri-modal decription of CMAQ aerosols and removes the wavelength dependence because Mie Scattering theory depends on the Mie Parameter  ($2\pi r/\ \lambda$). The removal means curving fitting over Mie Parameter rather than aerosol radius. The modification reduces run time at model initialized because the lookup table is created inline then saved to an ASCII file when NEW_START equals true or when the ASCII file is missing. The step saves thirty to forty seconds when the ASCII file exist whose filename is set by the below environment variable. 

     setenv MIE_TABLE $OUTDIR/mie_table_coeffs_${compilerString}.txt

The time savings is signicifant to total model runtime for the 108 hemispheric, benchmark, and column model domains. 

Why created the table inline? The table's data depends on information in the model code such as max/min geometric mean diameters allowed for each aerosol model or in the inputs such as the range of refractive indices of aerosol components. In general, the information does not change between model configuration or application but is still subject to change. Inline creation of the table insures a better fit to the configuration or application. 

How does the new method (MieTab) compared to solutions to Mie Theory (MieCalc)? 


<center>Wavelength Dependence of Normalized Mean Bias</center>
<center> Aerosol Optical Depth MieTab against MieCalc    

| Wavelength (nm) | NMB: HEMI June 25, 2018 |  12US1 June 25, 2018 |    
|:----------------------:|:-------------------------------------:|:-------------------------------------:|    
|   294 |  -0.77%  | -.38%  |     
|   303   | -0.92%   | -0.64%  |   
|   310  | -0.99%   | -0.84%   |    
|   316  |  -1.0%   | -1.0%   |  
|   333 |  -0.99%  | -1.4% |   
|   381 |  -0.18%  | -1.0%  |    
|   550 (Interpolation via Angstrom Exponent)  |  -2.2%  | -0.53% |  
|   607 |  -2.8%  | -0.38% |    


 </center>

#### Simpified Runtime Option to set the Method for Aerosol Optical Properties.

The Pull request replaces the two current runtime options, OPTICS_MIE_CALC and CORE_SHELL_OPTICS, with one called AEROSOL_OPTICS. The replacement attempts to clarify how aerosol optics are calculated because the current options do not state the default method for calculating optical properties or how optical properties are calculated when CORE_SHELL_OPTICS is set to true and an aerosol mode does not significant a black carbon core (_The core composes less than one billionth of the total particle volume._) The new runtime option has six allowed integer values that determine the method for optical properties. The below table defines these values.



 <center>Table 1. Aerosol Optics Method per Value.</center>  

| AEROSOL_OPTICS Value | Alias | Method |
|:----------------------:|---|--------|   
|   1   | MieTab | Inline Tabular Mie Method based on Mie Calculations over range of aerosol properties for a Uniformly Volume Mixed Sphere   |   
|   2   | MieCalc | Solution to Mie Theory for a Uniformly Volume Mixed Sphere   |   
|   3 (default value) | FastOptics | Approximations to Mie Theory for a Uniformly Volume Mixed Sphere based on Mie Parameter and mean refractive index |   
|   4   |CoreShell-MieTab | Core-shell mixing model if the aerosol mode has signficant black carbon core otherwise uses Tabular Mie Method as Option 1      |   
|   5   | CoreShell-MieCalc | Core-shell mixing model if the aerosol mode has signficant black carbon core otherwise uses Mie Solution as Option 2       |   
|   6   | CoreShell-FastOptics | Core-shell mixing model if the aerosol mode has signficant black carbon core otherwise uses Mie Approximations as Option 3     |     
  
 



 Table 2. 12US1 Domain Runtimes per Aerosol Optics Method. Model compiled with Intel compiler version 21.4. Simulations used 128 processors. 2017 and 2018 simulations set the PHOTDIAG to True.

| AEROSOL_OPTICS Alias | Avg HEMI RunTime Dec. Period 2017 (s) |  Avg HEMI RunTime Jun. Period 2018 (s)| HEMI RunTime Oct 1<sup>st</sup> 2015 (s)|   
|:----------------------:|:-------------------------------------:|:----------------------------------------:|:---------------------------------------:| 
|   FastOptics (default) |  655.175   | 672.215  | 1183.30 |
|   MieTab   |  640.952   | 687.616  | 1275.80 |
|   MieCalc   |  826.872   | 873.353   |  1728.80 |   
|    CoreShell-MieCalc  |   1594.88  |  1665.84 | 4370.10 |

 Table 3. 12US1 Domain Runtimes per Aerosol Optics Method. Model compiled with Intel compiler version 18.0. Simulations used 128 processors. Simulations set the PHOTDIAG to True.
  
| AEROSOL_OPTICS Alais | Avg 12US1 RunTime Jun. Period 2018 (s)|    
|:----------------------:|----------------------------------------|   
|   FastOptics (default) |  3385.09   | 
|   MieTab   |  3316.10  |
|   MieCalc   | 4190.02   | 
|    CoreShell-MieCalc  |   10838.96  | 

#### Changes to Photolysis Diagnostic Files One and Three.

The Pull Request adds to a new output variable to the diagnostic one file and replaces a variable in the diagnostic three file. The new output variable is aerosol absorption optical depth (AAOD) per wavelength. The information can support diagnosing how model aerosols affect photolysis reactions and help evaluating how model predictions compare to observations because the AERONET site reports AAOD from Aerosol Inversion Results and uses AAOD for calculating aerosol single scattering albedo (SSA) over a vertical column. In the photolysis diagnostic three file, the variable replacement switches aerosol single scattering albedo with aerosol scattering coefficient for a grid cell. The latter better allows comparing methods for aerosol optical properties because each method directly calculates the coefficient and not single scattering alobedo.


#### References  

 Andrews, E., Ogren, J. A., Kinne, S., and Samset, B.: Comparison of AOD, AAOD and column single scattering albedo from AERONET retrievals and in situ profiling measurements, Atmos. Chem. Phys., 17, 6041–6072, https://doi.org/10.5194/acp-17-6041-2017, 2017. 

 Fast, J. D., W. I. Gustafson Jr., R. C. Easter, R. A. Zaveri, J. C. Barnard, E. G. Chapman, G. A. Grell, and S. E. Peckham(2006), Evolution of ozone, particulates, and aerosol direct radiative forcing in the vicinity of Houston using a fully coupledmeteorology-chemistry-aerosol model,J. Geophys. Res.,111, D21305, doi:10.1029/2005JD006721
