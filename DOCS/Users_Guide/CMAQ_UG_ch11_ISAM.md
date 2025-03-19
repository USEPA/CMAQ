

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch10_HDDM-3D.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch12_sulfur_tracking.md)

<!-- END COMMENT -->

# 11. Integrated Source Apportionment Method (CMAQ-ISAM)
## 11.1 Introduction

The Integrated Source Apportionment Method (ISAM) calculates source attribution information for user specified ozone and particulate matter precursors within the CMAQ model.  CMAQ-ISAM has been substantially updated starting with the CMAQv5.3 release, and now differs significantly from previous releases. The major changes to the ISAM chemistry solver are detailed in the [ISAM Chemistry Supplement][link_11_pdf]. In addition, signifcant updates and multiple minor fixes were included in the  subsequent releases including substantial updates to the gas-phase chemistry apportionment algorithms that improve both physical and numerical aspects of the method. Users of CMAQ-ISAM are strongly encouraged to update to the latest available release of the model.

The base CMAQ model provides users the concentration and deposition fields of many pollutant species. These species are usually combinations of different types of primary emissions and secondary formation that have been physically and chemically transformed in the model. However, sometimes it is desirable to know specific source attribution information for the model outputs. For example, how much of the ozone in an urban area was formed due to nitrogen oxides emitted from motor vehicles in a neighboring state?

Answering this type of question often requires running an air quality model twice, once with the standard emissions scenario and once with the source of interest completely removed. The difference between the two runs is then assumed to be attributed to the removed source.  While this approach is reasonably straightforward to implement, it has some drawbacks.  For example, removing a large source from the system in a highly nonlinear chemical mixture can lead to some errors. Also, calculating source attribution of many sources can be logistically and computationally prohibitive.

Alternatively, running CMAQ with ISAM enabled allows the user the ability to calculate source attribution of a large number of sources directly by the model in one simulation.

CMAQ-ISAM supports source apportionment for all modeled gas and aerosol species (depending on base model configuration). The selection of species for which source apportionment should be performed in a given simulation can be controlled through the following `TAG CLASSES`:

```
SULFATE      - ASO4J, ASO4I, SO2, SULF, SULRXN       
NITRATE      - ANO3J, ANO3I, HNO3, ANO3J, ANO3I, HNO3, NO, NO2, NO3, HONO, N2O5, PNA, PAN, PANX, NTR1, NTR2, INTR           
AMMONIUM     - ANH4J, ANH4I, NH3       
EC           - AECJ, AECI          
OA_TOT       - Organic aerosol species
VOC          - Various species depending on mechanism. Now includes CO. (see CCTM/src/isam/SA_DEFN.F for complete list)      
PM_IONS      - ANAI, ANAJ, AMGJ, AKJ, ACAJ, AFEJ, AALJ, ASIJ, ATIJ, AMNJ, AOTHRI, AOTHRJ      
OZONE        - all NITRATE species + all VOC species     
CHLORINE     - ACLI, ACLJ, HCL
NVPOA        - Non-Volatile Primary Organic Aerosol (APOC, APNCOM)
PM_TOT       - all PM species
HAP_GAS      - Gaseous Hazardous Air Pollutants
HAP_AERO     - Aerosol Hazardous Air Pollutants
HAP_TOT      - HAP_GAS and HAP_AERO
URBAN_TOXICS - Classic Urban Air Toxics 
PAH_TEQ      - NonReactive PAH tracers
BENAPYRENE   - Gaseous and aerosol Benzo[a]Pyrene
MERCURY      - Gaseous and aeroosl Mercury species
ALL          - All model species

    
```

## 11.2 Build Instructions

Starting with CMAQv5.3 model release, ISAM is provided directly with the source code of the base model. To use ISAM, follow the normal build process for CMAQ described in [Chapter 5](CMAQ_UG_ch05_running_a_simulation.md), but make sure to uncomment the following line in bldit_cctm.csh: 

```
set ISAM_CCTM
```

**A note about I/O API installation for ISAM applications**

I/O APIv3.2  supports up to MXFILE3=256 open files, each with up to MXVARS3=2048. ISAM applications configured to calculate source attribution of a large number of sources may exceed this upper limit of model variables, leading to a model crash. To avoid this issue, users may use I/O API version 3.2 "large" that increases MXFILE3 to 512 and MXVARS3 to 16384. Instructions to build this version are found in [Chapter 3](CMAQ_UG_ch03_preparing_compute_environment.md#333-io-api-library).
Note, using this ioapi-large version is not required for the CMAQ-ISAM Benchmark Case. 
If a user needs to use larger setting for MXFILE3 and MXVAR3 to support their application, note that the memory requirements will be increased.
This version is available as a zip file from the following address:

https://www.cmascenter.org/ioapi/download/ioapi-3.2-large-20200828.tar.gz

**A note about solver configuration**

ISAM is currently only implemented for the EBI solver. For some CMAQ applications, the Rosenbrock solver is desirable and is set as the default in the sample runscripts. For example, this is the case when using the CB6R5M chemical mechanism for hemispheric simulations. In such cases, the bldit script needs to be modified to not select the Rosenbrock solver. While this may incur a performance penalty in terms of CPU time and increase the likelyhood of convergence warnings, it will allow the ISAM simulation to proceed in most cases.

## 11.3 Run Instructions

To begin a CMAQ simulation with source apportionment enabled, the ISAM section of the runscript must be configured.  The additional necessary environment variables are listed in Table 11-1.

<a id=Table11-1></a>

**Table 11-1. ISAM run script variables**

|**Variable** | **Settings** | **Description**|
|-------|----------|------------|
|CTM_ISAM|Y/N|Set this to Y to enable ISAM|
|SA_IOLIST|path/filename|Provide the location of the ISAM control file (discussed below)|
|ISAM_BLEV_ELEV|" MINVALUE MAX VALUE "|LAYER range for the instantaneous ISAM output concentrations|
|AISAM_BLEV_ELEV|" MINVALUE MAX VALUE "|LAYER range for the average ISAM output concentrations|
|ISAM_NEW_START|Y/N|set Y for a new simulation and N for continuing from a previous day's outputs|
|ISAM_PREVDAY|path/filename|Provide the location of the previous day's ISAM restart file|
|SA_ACONC_1|path/filename|ISAM output for average apportioned concentrations|
|SA_CONC_1|path/filename|ISAM output for instanteneous apportioned concentrations|
|SA_DD_1|path/filename|ISAM output for apportioned dry deposition|
|SA_WD_1|path/filename|ISAM output for apportioned wet deposition|
|SA_CGRID_1|path/filename|ISAM output for a restart file to continue the simulation further in time|
|ISAM_O3_WEIGHTS| 1,2,3,4,5 (default is 5) | sets what tracked species are favored or _weighted_ when determining apportionment in gas phase chemistry |
|ISAM_NOX_CASE| 1,2,3,4 (default is 2) | what tracked species are weighted when grid cell NOx limited ozone production. Only used if ISAM_O3_WEIGHT equal 5. | 
|ISAM_VOC_CASE| 1,2,3,4 (default is 4) | what tracked species are weighted when grid cell VOC limited ozone production. Only used if ISAM_O3_WEIGHT equal 5. | 
|VOC_NOX_TRANS | >= 0.0 (default is 0.35)| value of Prod H2O2 over Prod HNO3 less than where ISAM_VOC_CASE weights are used. Otherwise, ISAM_NOX_CASE weights are used. VOC_NOX_TRANS only used if ISAM_O3_WEIGHT equal 5.|

Additionally, ISAM can track emissions confined to geographic regions.  This functionality can be enabled through CMAQ's `RegionsRegistry` set in the `CMAQ_Control_DESID` namelist (Appendix B.4) and is discussed further below.



#### ISAM and bidirectional NH<sub>3</sub> exchange

ISAM in CMAQ v5.3 supports bidirectional NH<sub>3</sub> exchange using both M3Dry and STAGE deposition options. To run with this option the AMMONIUM species class must be set in the ISAM control file 

```
TAG CLASSES     |AMMONIUM
```

and the ABFLUX must be set in the run script.

```
setenv CTM_ABFLUX Y          #> ammonia bi-directional flux for in-line deposition

```

Setting these options will automatically set the BID tag for model output. Modeled species output with the BID tag represent the influence of NH<sub>3</sub> emissions from fertilizer and biogenic NH<sub>3</sub> emission sources. Biogenic NH<sub>3</sub> emissions include the evasion of NH<sub>3</sub> from non-agricultural vegetation and soil NH<sub>4</sub> pools as parameterized in the STAGE or M3Dry models.  

#### ISAM run-time options for gas chemistry and ozone production

In the runtime table, the last four rows deal with ISAM's method for apportioning source contributions from gas chemistry. They weight specific species that are chemical reactants so a reaction's product are totally apportioned to the weighted reactant. If both reactants are weighted, products are equally apportioned between reactants. Note that the unmodified method always equally apportions products. The changes seek to isolate sources that emit or secondary produce the weighted reactants because their sources are controllable or deemed responsible for deteriorating air quality. 

The weighting schemes focus on apportioning NOx and ozone concentrations so weighted species include several reactive nitrogen compounds, oxygenated VOCs, organic peroxy radicals and operators. A new runtime option, ISAM_O3_WEIGHTS, determines what species are weighted. The below two tables define what different values set for ISAM_O3_WEIGHTS. The first define option values 1 thru 4. Option 1 reproduces results from the unmodified code. The second table describes option 5 that toggles between two weight settings listed in the first table.

| **Species** |	 **Option 1** |	**Option 2** |	 **Option 3** |	 **Option 4** |
|:-----------:|:-------------:|:------------:|:--------------:|:--------------|
|      NO	    |      NO	      |      YES	   |         YES	  |       NO      |
|NO2	        |NO	            |YES	         |YES	            |NO             |
|NO3	        |NO	            |YES	         |YES	            |NO             |
|HONO	        |NO	            |YES	         |YES	            |NO             |
|ANO3(I or J)	|     NO	      | YES	         | YES	          |NO             |
|HCHO	            |  NO	      | NO	         |YES	            |YES            |
|CH3CHO	          |  NO	      | NO	         |YES	            |YES            |
|Higher Aldehydes |  NO	      | NO	         |YES	            |YES            |
|Acetone	        |  NO	      | NO	         |YES	            |YES            |
|Lumped Ketones	  |  NO	      | NO	         |YES	            |YES            |
|Isoprene peroxy radical	        |NO	       |NO	 | YES	    |YES            |
|Acetyl peroxy radicals	          |NO	       |NO	 | YES	    |YES            |
|peroxy operators(such as cb6's XO2 and XO2H)|NO	|NO |	YES	  |YES            |

| **Option 5 Algorithm** | 
|:----------------------:|                                        
|   IF( (H2O2 production)/(HNO3 Production) > VOC_NOX_TRANS){ISAM_NOX_CASE}else{ISAM_VOC_CASE} |
	
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
	
### 11.3.1 ISAM control file (SA_IOLIST)

The ISAM `SA_IOLIST` is a text file used to configure which tag classes, emissions streams, and source regions the model will track.  An example of this file, `isam_control.txt`, is provided in $CMAQ_HOME/CCTM/scripts.  The order and formating of this file must be kept intact, but it does allow for insertion of comment lines.  

Each ISAM simulation requires the specification of the `TAG CLASSES` that the user desires to apportion.  The full list of available tag classes (e.g. `SULFATE`, `NITRATE`, `AMMONIUM`, `EC`, `OC`, `VOC`, `PM25_IONS`, `OZONE`, `OA_TOT`, or `PM_TOT`) and the species associated with each of these are provided in section 11.1.  One or more of these tag classes must be specified in `SA_IOLIST`.  Multiple tag classes are comma delimited. Incorrectly specified choices for this field will cause a model crash.

```
TAG CLASSES     |OZONE, SULFATE
```

After setting tag classes for the simulation, information for one or more tags is required. Each individual tag will track the species from the specified `TAG CLASSES` and has its own set of three options in the control file.  The first option is the name:

```
TAG NAME        |EGU
```

It is recommended that the text string for the tag name be kept short (three characters or less) in order to accommodate the longer species names from some chemical mechanisms in the ISAM output files.  When a 'TAG NAME' is specified that is too long to accomodate every tagged CMAQ species name and the appended tag name, the resulting ISAM species name may not be able to be written to output files correctly, due to 16 character limit for variable names in IOAPI.  For example, 'BUTADIENE13_EGU' would work, because it is 15 characters, but 'BUTADIENE13_EGUT1' would fail at 17 characters.

The second option is the comma delimited list of regions to track with this tag.  The keyword 'EVERYWHERE' is used to track domain-wide emissions.  To track region-constrained emissions, variable names from the regions file specified in the `CMAQ_Control_DESID` namelist are used instead of the "EVERYWHERE' keyword. The regions file requirements are identical to the optional file used to scale emissions in predetermined geographical areas. See [Appendix B.4](Appendix/CMAQ_UG_appendixB_emissions_control.md#b4-applying-masks-for-spatial-dependence) for further details on the regions file, including how to download an example file.

```
REGION(S)       |EVERYWHERE
```

or

```
REGION(S)       |NC, SC, GA
```

Finally, the emissions streams labels are required as the third option in the control file.  Labels correspond to emissions and other input streams set in build and run scripts for the base CMAQ simulation. Additionally, it is possible to specify 'PVO3' as a stream label in order to track contribution to concentrations from upper layer injections due to potential vorticity calculations.  This option also requires enabling the corresponding run script variable to support these calculations. 

```
EMIS STREAM(S)  |PT_EGU, PT_NONEGU
```

The final line in the control file needs to be kept unchanged in order to aid the file parser in reading this file.

```
ENDLIST eof
```

In addition to the user-specified list, ISAM will alway track and output three additional default tags with every simulation and the BID tag if the simulation includes both bidirectional NH<sub>3</sub> and the 'AMMONIUM' species class (note, that at least one valid user-specified tag must be defined, so a minimum of 4 tags are required):

```
ICO - contribution from initial conditions specified for the first day of the simulation
BCO - contribution from boundary conditions throughout the simulation
OTH - contribution from all non-tagged emissions streams and other processes in the model.
BID - contribution from bidirectional NH3 exchange 
```

Please, note that, currently, ISAM results for the same user defined tag may differ depending on the overall configuration and content of the ISAM control file.  This weakness of the method is detailed in the last section of the [ISAM Chemistry Supplement](Supplement/CMAQ_ISAM_Chemistry_Supplemental_Equations.pdf).  Generally, tracking a larger number of tags produces more consistent apportionment results.  

#### Defining ISAM Tags for In-line Sources.
 
The CMAQ model allows several types of emissions that are calculated in-line or during a model simulation instead of provided by the user as inputs. A simulation can use all of these inline emissions and ISAM can calculate apportionment from these sources. The former is done by setting appropriate emissions options in the CMAQ runscript. For ISAM to calculate apportionment for an in-line source, the isam control file needs to define a tagname using the correct stream name.  The below table lists currently supported inline emissions streams in CMAQ:
 
|**Emission Stream Name**|**Inline Emissions Source**|
|-----------|------------------------|
| BIOG | Biogenic Emissions (BEIS) |
| MIOG | Biogenic Emission (MEGAN) |
| MGEGM | Marine Gas Emissions |
| LTNG | Lightning NO Emissions |
| ASEA | Sea Spray Aerosol Emissions |
| DUST | Wind-Blown Dust Emissions |
| PVO3 | Potential Vorticity Incursion* |

*Although it is not an emission stream, it is possible to tag the ozone incursions at the top of the simulated volume if the potential vorticity option `CTM_PVO3` is activated in the run script.

#### Interpretation of 'OTH' tag
The OTH tag (e.g.“O3_OTH” in the ISAM benchmark) represents concentrations for that species attributed to 1) all other emissions streams, 2) precursor species not included in the specified tag class(es), and 3) other processes in the model.

For item 1), this includes internally calculated emissions that a user decides to exclude from the control file (perhaps to reduce computational cost of running with tags that are not of interest to a particular application). These are things like online biogenics, online lightning, dust, etc.

For item 2), these are some secondarily produced intermediate species that have minor impact on ozone production.

For item 3), these are processes in the model that create a given species but not from the emissions streams that can be specified with the control file.  For example, 'O3_OTH' includes ozone that is produced from background methane that is specified in the model as a constant.

Finally, ISAM is an approximation for attribution. In the formulation, assumptions are made about which species are most important in the chemical formuation of the species being studied.  For example in cb6r3 based mechanisms, peroxyl radicals from aromatic compounds affect ozone production by a small amount so ISAM neglects their contribution. The contribution to ozone from species not included in the ISAM formulation will go into 'O3_OTH', even if the emission source of these species is included in the control file. 

## 11.4 ISAM Benchmark 
See the [CMAQ-ISAM Benchmark Tutorial](Tutorials/CMAQ_UG_tutorial_ISAM.md) for step-by-step instructions for running the 2 day benchmark case.  The input files for the CMAQv5.5 ISAM benchmark case are the same as the benchmark inputs for the base model. Output source apportionment files associated with the sample `isam_control.txt` provided in this release package are included in the benchmark outputs for the base model. The tutorial also provides example instructions for post-processing ISAM output for individual species contained in the SA_ACONC_1 output file into aggregate species like NOx or PM2.5 using the [`combine`](../../POST/combine/README.md) utility and customized `SPECIES_DEF` species definition files. Note that the [Explicit and Lumped Model Output (ELMO) module](./Appendix/CMAQ_UG_appendixF_elmo_output.md) available for aggregating base model concentrations does not currently support the aggregation of ISAM species within CMAQ, necessitating a customized post-processing workflow. 

## 11.5 References

Kwok, R.H.F, Napelenok, S.L., & Baker, K.R. (2013). Implementation and evaluation of PM2.5 source contribution analysis in a photochemical model. Atmospheric Environment, 80, 398–407 [doi:10.1016/j.atmosenv.2013.08.017](https://doi.org/10.1016/j.atmosenv.2013.08.017).

Kwok, R.H.F, Baker, K.R., Napelenok, S.L., & Tonnesen, G.S. (2015). Photochemical grid model implementation of VOC, NOx, and O3 source apportionment. Geosci. Model Dev., 8, 99-114. [doi:10.5194/gmd-8-99-2015](https://doi.org/10.5194/gmd-8-99-2015).  

Sillman, Sanford. (1995). The use of NOy, H2O2, and HNO3 as indicators for ozone-NOx-hydrocarbon sensitivity in urban locations. Journal of Geophysical Research. 1001. 14175-14188.

Tonnesen, G.S. & Dennis, R.L. (2000a). Analysis of radical propagation efficiency to assess ozone sensitivity to hydrocarbons and NOx. 1. Long-lived species as indicators of ozone concentration sensitivity. Journal of Geophysical Research. 105. 9213-9225.

Tonnesen, G.S. & Dennis, R.L. (2000b). Analysis of radical propagation efficiency to assess ozone sensitivity to hydrocarbons and NOx. 2. Long-lived species as indicators of ozone concentration sensitivity. Journal of Geophysical Research. 105. 9227-9241.

**Contact**

[Sergey L. Napelenok](mailto:napelenok.sergey@epa.gov), Computational Exposure Division, U.S. EPA


<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch10_HDDM-3D.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch12_sulfur_tracking.md)<br>
CMAQv5.5 User's Guide <br>

<!-- END COMMENT -->

<!-- START_OF_COMMENT -->

[link_11_pdf]: ./Supplement/CMAQ_ISAM_Chemistry_Supplemental_Equations.pdf 

<!-- END_OF_COMMENT -->

[link_11_pdf]: https://github.com/USEPA/CMAQ/blob/main/DOCS/Users_Guide/Supplement/CMAQ_ISAM_Chemistry_Supplemental_Equations.pdf
