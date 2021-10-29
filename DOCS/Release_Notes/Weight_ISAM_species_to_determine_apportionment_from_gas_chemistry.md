### Brief Description
The pull request modifies ISAM's method for apportioning source contributions from gas chemistry. The changes weight specific species that are chemical reactants so a reaction's product are totally apportioned to the weighted reactant. If both reactants are weighted, products are equally apportioned between reactants. Note that the unmodified method always equally apportions products. The changes seek to isolate sources that emit or secondary produce the weighted reactants because their sources are controllable or deemed responsible for deteriorating air quality. _Note to the integrators and reviewers: this pull request includes changes to CCTM/src/procan/pa/PA_DEFN.F, CCTM/src/hadv/ppm/x_ppm.F, CCTM/src/hadv/ppm/y_ppm.F and CCTM/src/hadv/ppm/hppm.F. They remove a gfortran error and may be replaced by changes in PR #699._

The scheme focuses on apportioning NOx and ozone concentrations so weighted species include several reactive nitrogen compounds, oxygenated VOCs, organic peroxy radicals and operators. A new runtime option, ISAM_O3_WEIGHTS, determines what species are weighted. The below tables define what different values set for ISAM_O3_WEIGHTS. The first define option values 1 thru 4. Option 1 reproduces results from the unmodified code. The second table describes option 5 that toggles between two weight settings listed in the first table. 

| New ISAM Runtime Options|
|:-----:|
| Table 1. |
|![image](https://user-images.githubusercontent.com/16845494/138957728-fd524c6e-0687-418c-81e7-bb5aaaf511ec.png)|
| Table 2.|
|![image](https://user-images.githubusercontent.com/16845494/138961747-2bd1f652-6e49-4ad0-8914-d820868604e8.png)|

The runtime options, ISAM_NOX_CASE and ISAM_VOC_CASE, determine the two settings. Toggling is determined by whether the cell grid's ozone production has NOx or VOC limiting conditions. Option 5 uses H2O2 production over HNO3 production (see appendix A in Sillman (1995)) whether former or latter condition exists. Sillman (1995) states that VOC limiting exist when the ratio is less than 0.35 but the ratio's transition value is uncertain (Tonnesen and Dennis, 2000a and 2000b) so a final runtime option sets the transition value,  VOC_NOX_TRANS. Note that all the repositories run-scripts include the below commands setting the new ISAM options using their default values.

       #> Options used to favor tracked species in reaction for Ozone-NOx chemistry
       setenv ISAM_O3_WEIGHTS 5   # weights for tracked species Default is 5
                                  #     OPTIONS
                                  # 1 does not weight any species
                                  # 2 weights NOx and subset of NOz species
                                  # 3 uses with from option 2 plus weight OVOC species, organic radicals and operators
                                  # 4 weight OVOC species, organic radicals and operators
                                  # 5 toggles between two wieghting set based on VOC and NOx limiting ozone production
       # Below options only used if ISAM_O3_WEIGHTS set to 5
       setenv ISAM_NOX_CASE  2    # weights for tracked species when ozone production is NOx limiting. Default is 2
       setenv ISAM_VOC_CASE  4    # weights for tracked species when ozone production is VOC limiting. Default is 4
       setenv VOC_NOX_TRANS  0.35 # value of Prod H2O2 over Prod HNO3 less than where
                                  # ISAM_VOC_CASE weights are used. Otherwise, ISAM_NOX_CASE
                                  # weights are used. Default is 0.35

### Impact on ISAM predictions

The below plots show how the unmodified code apportions NOx and ozone concentration to Electrical Generatoring Units (EGUs) over eastern Virginia during July 8, 2017.

![image](https://user-images.githubusercontent.com/16845494/138982265-cdc2f9b3-33af-49b9-bb73-6d63cac24b3a.png)![image](https://user-images.githubusercontent.com/16845494/138982322-e71b8beb-c331-487e-b34b-cbc90d1344ed.png)|

In comparison, the next three plots slow NOx apportioned EGU using ISAM_O3_WEIGHTS set to 1, 2 and 5 from left to right.
The leftmost plot confirm the ISAM_O3_WEIGHT set 1 reproduces results from the unmodified code while the center and rightmost plots increase NOx apportioned to EGUs.
 
![image](https://user-images.githubusercontent.com/16845494/139360957-fb867ca8-3195-40c6-9f36-8ff4ac453e50.png)

For ozone, similar plots produced similar conclusions as shown below. Ozone apportioned to EGU is may be lower expect so the VOC_NOX_TRANS could be lowered to 0.08 as suggested by Tonnesen and Dennis (2000a and 2000b).

![image](https://user-images.githubusercontent.com/16845494/139361060-523b16c1-dc41-4759-9ede-ef0091f42511.png)


### Files Changed
CCTM/scripts/run_cctm_2010_4CALIF1.csh
CCTM/scripts/run_cctm_2011_12US1.csh
CCTM/scripts/run_cctm_2014_12US1.csh
CCTM/scripts/run_cctm_2015_HEMI.csh
CCTM/scripts/run_cctm_2016_12US1.csh
CCTM/scripts/run_cctm_Bench_2011_12SE1.csh
CCTM/scripts/run_cctm_Bench_2016_12SE1.csh
CCTM/src/hadv/ppm/hppm.F
CCTM/src/hadv/ppm/x_ppm.F
CCTM/src/hadv/ppm/y_ppm.F
CCTM/src/isam/SA_DEFN.F
CCTM/src/isam/SA_IRR_DEFN.F
CCTM/src/procan/pa/PA_DEFN.F
CCTM/src/util/util/RUNTIME_VARS.F

### Significant Commit ID(s) 

0153f7d396ed8dcf792e59e3ae4e522237626143
0e6a556ab17ba55a51a0e8b6bfb4f25f0054e926
16c74f0af9e079ef6622f9c4d042bda5c22c9e1f
851945516f30f76cce0c533379ce585af3326544
39b98de830b006f42477721ffcd9d2e90ccbf1dd

### References
Sillman, Sanford. (1995). The use of NOy, H2O2, and HNO3 as indicators for ozone-NOx-hydrocarbon sensitivity in urban locations. Journal of Geophysical Research. 1001. 14175-14188. 
Tonnesen, G.S. and Dennis, R.L. (2000a). Analysis of radical propagation efficiency to assess ozone sensitivity to hydrocarbons and NOx. 1. Long-lived species as indicators of ozone concentration sensitivity. Journal of Geophysical Research. 105. 9213-9225.     
Tonnesen, G.S. and Dennis, R.L. (2000b). Analysis of radical propagation efficiency to assess ozone sensitivity to hydrocarbons and NOx. 2. Long-lived species as indicators of ozone concentration sensitivity. Journal of Geophysical Research. 105. 9227-9241.

