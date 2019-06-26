<!-- BEGIN COMMENT -->

[<< Previous Appendix](CMAQ_UG_appendixA_model_options.md) - [Home](../README.md) - [Next Appendix >>](CMAQ_UG_appendixC_spatial_data.md)

<!-- END COMMENT -->

* * *

# Appendix B: Emissions Input and Control
[Jump to DESID Tutorial](../Tutorials/CMAQ_Emissions.md) for step by step instructions on performing some basic manipulation of emission streams.
[Jump to DESID overview](CMAQ_UG_ch04_model_formulation.md) in Chapter 4 of this User's Guide.

## B.1 Emissions Control with the Detailed Emissions Scaling, Isolation and Diagnostics Module (DESID)

In addition to the options available in the RunScript, CMAQ now reads a dedicated namelist in order to apply comprehensive rules for reading and scaling emissions. The namelist, called the **Emission Control Namelist** is named "EmissCtrl.nml" by default and a separate version exists for every mechanism because these namelists are preloaded with likely rules linking emissions of important CMAQ primary species to their typical surrogate names as output by SMOKE. By default, this namelist is stored in each chemical mechanism folder (e.g. MECHS/cb6r3_ae7_aq), and is copied into the user's build directory when bldit_cctm.csh is executed. If the user modifies the name or location of this namelist, then the following command in the RunScript should be updated as well:
```
setenv EMISSCTRL_NML ${BLD}/EmissCtrl.nml
```

The Detailed Emissions Speciation, Isolation and Diagnostics (DESID) module included with CMAQv5.3 provides comprehensive customization and transparency of emissions manipulation to the user. The customization of emissions is accomplished via the Emission Control Namelist, which contains four sections of variables that modify the behavior of the emissions module. These include ***General Specs***, ***Emission Scaling Rules***, ***Size Distributions***, and ***Regions Registry***

## B.2 ***General Specs***
These variables modify or constrain the effects of other sections of the namelist. The "Guard_XXX" options allow the user to protect specific streams from being modified by scaling rules (explained in section 4.3.1.2) with the "ALL" keyword in the stream field. For example, the "Guard_BiogenicVOC" option instructs the model not to scale biogenic VOC emissions from the online BEIS module, even if a rule indicates that "ALL" streams are to be scaled. The other "Guard_XXX" options achieve the same effect for other online emissions sources like wind-blown dust, sea spray, marine gas, and lightning NO.

## B.3 ***Emission Scaling Rules***
With the rules present in this section, the user is able to exert sophisticated, precise control over the scaling applied to emissions from specific streams, in specific geographic areas, and/or for specific compounds. The set of rules used by CMAQ to interpret emissions shall be provided in one array called EM_NML. It is necessary that every field (i.e. column) be populated for every rule. The fields are given and defined here and in the comment section of the Emission Control Namelist:
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
```
- 'Region Label' - Apply scaling for specific regions of the domain. Set this field to "EVERYWHERE" to apply the rule to the entire domain.
- 'Stream Label' - Short Name from Run Script (ie. the value of GR_EMIS_01_LAB or STK_EMIS_01_LAB). There are a few reserved names that apply to inline emissions streams. These are:
  - BIOG - Biogenic VOC emissions
  - MGEM - Marine Gas Emissions
  - LTNG - Lightning NO Emissions
  - WBDUST - Wind-Blown Dust Emissions
  - SeaSpray - Sea Spray Aerosol Emissions  

  Set this field to 'ALL' to apply the rule to all emission streams.  
- 'Emission Surrogate' - The character string identifying the surrogate on the emission file or in the inline calculation that the CMAQ species should be mapped to. Usually this name is the same as the CMAQ species for convenience. For aerosols, it's usually slightly different (e.g ANO3 vs. PNO3). Set this field to 'ALL' to apply the rule to all emission surogates.  
- 'CMAQ-Species' - Internal Species Name. Set this field to 'ALL' to apply the rule to all CMAQ internal species.
- 'Phase/Mode' - If the CMAQ-Species is a Gas, this field should equal 'Gas'. If the CMAQ-Species is an aerosol, this field should indicate one of the possible emission aerosol modes. Every stream by default is given a 'COARSE' and 'FINE' mode. The user may refer to these, or define others above and refer to them as well. This level of specificity is needed so that aerosol number and surface area are calculated correctly, and so that any unit conversions between gases and aerosols can be handled correctly.  
- 'Scale Factor' - Adjustment factor to be applied to the mapping
- 'Basis' - Specifies whether the scaling option should directly apply, or if the operation should conserve moles or mass when performing scaling operations. CMAQ has a lookup table of molecular weights for known emission surrogate species and can use these to translate molar and mass emission rates from the input file to the CMAQ species. CMAQ determines the units of the emission surrogate species by reading the file header (i.e. it is important the units are accurate. Options for input are:
  - 'MASS' - Conserve Mass. For example, if emissions of an aerosol are to be scaled to emissions of a gas surrogate, it is common to want to conserve mass.
  - 'MOLE' - Conserve Moles. For example, if emissions of a gas-phase species are to be scaled to another gas, it is sometimes desired to conserve moles since gas emissions are provided on a mole basis.
  - 'UNIT' - Ignore molecular weight conversions and apply emission rate directly regardless of units.
- 'Operation' - Specifies the kind of rule to be carried out. Options are:
  - 'a' - add the rule to existing instructions. This operation should be used for new entries, too.
  - 'm' - find existing scaling instructions matching this rule's features (ie. species, streams, etc) and multiply them by the factor in this particular rule.
  - 'o' - find existing scaling instructions matching this rule and overwrite them.  

### B.3.1 Default Rules
The Emission Control Namelists provided with the CMAQ repo have default rules included that correspond to each chemical mechanism. Here is an example default rule that links NO in CMAQ to NO from every emission stream in every model grid cell with a scale factor of 1.0.
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'All'         ,'NO'     ,'NO'          ,'GAS' ,1.0  ,'UNIT','a',
```
Many rules are needed here in order to properly link every emitted pollutant to a CMAQ species. Rules are needed for gas- and aerosol-phase species. Additional rules also exist for online aerosol modules like wind-blown dust and sea spray because the names of aerosol surrogates from these modules are different than those typically used for SMOKE output. For example, fine-mode aerosol sulfate is commonly called PSO4 in SMOKE, but is PMFINE_SO4 from dust and sea spray.

### B.3.2 Modifying Default rules
The user can modify any default rule in order to change the scale factor applied. Alternatively, the user can add new rules after the default rules in order to customize the emissions. Typical modifications may include multiplying the emissions of a particular species from a particular stream by a factor of 2, zeroing out emissions of all species from a particular stream, etc. Please see the tutorial on [Prescribing Emissions with DESID](../Tutorials/CMAQ_Emissions.md) for specific examples of modifications and the syntax used to invoke them.

#### B.3.2.1 Supporting the Volatility Basis Set
The *Volatility Basis Set* for treating the semivolatile partitioning of primary organic emissions is an example of a model feature that is well-supported by DESID. The approach involves distributing the emissions of total primary organic aerosol (carbon and noncarbon mass, or POC and PNCOM) among a series of aerosol and gas species of varying volatility.

If the user would like to invoke the nonvolatile partitioning assumption, it can be accomplished by directing all POC and PNCOM emissions to the POC and PNCOM species in CMAQ.
```
  ! --> Nonvolatile POA
  'EVERYWHERE', 'ALL'         ,'POC'    ,'APOC'        ,'FINE',1.   ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'APNCOM'      ,'FINE',1.   ,'MASS','a',
```
If the user would like to apply the default volatility distribution to the POA emissions, it can be accomplished with the following default rules.
```
  ! --> Semivolatile POA
  'EVERYWHERE', 'ALL'         ,'POC'    ,'VLVPO1'      ,'GAS' ,0.   ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'VLVPO1'      ,'GAS' ,0.   ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'POC'    ,'VSVPO1'      ,'GAS' ,0.045,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'VSVPO1'      ,'GAS' ,0.045,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'POC'    ,'VSVPO2'      ,'GAS' ,0.14 ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'VSVPO2'      ,'GAS' ,0.14 ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'POC'    ,'VSVPO3'      ,'GAS' ,0.18 ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'VSVPO3'      ,'GAS' ,0.18 ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'POC'    ,'VIVPO1'      ,'GAS' ,0.50 ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'VIVPO1'      ,'GAS' ,0.50 ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'POC'    ,'ALVPO1'      ,'FINE',0.09 ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'ALVPO1'      ,'FINE',0.09 ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'POC'    ,'ASVPO1'      ,'FINE',0.045,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'ASVPO1'      ,'FINE',0.045,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'POC'    ,'ASVPO2'      ,'FINE',0.   ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'ASVPO2'      ,'FINE',0.   ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'POC'    ,'ASVPO3'      ,'FINE',0.   ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'ASVPO3'      ,'FINE',0.   ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'POC'    ,'AIVPO1'      ,'FINE',0.   ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'AIVPO1'      ,'FINE',0.   ,'MASS','a',
```
Notice that for each species (e.g. ALVPO1) a rule is needed to link the species to the emissions of POC and another rule is needed to add PNCOM. This is because both carbon and noncarbon mass are part of the emissions of every semivolatile species. To change the volatility distribution for all streams, the user may modify the scaling factors in the default rules above. To introduce specialized volatility distributions for specific stream (e.g. residential wood burning, forest fires, diesel vehicles, etc), rules may be added whiech explicitly identify a strem in the "Stream Label" field.

## B.4 Applying Masks for Spatial Dependence
Gridded masks are used to apply rules to specific areas of the domain. For example, the following rule:
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'KENTUCKY'    , 'All'         ,'All'    ,'All'         ,'All' ,1.50 ,'UNIT','m',
```
will scale emissions of all species from all streams by +50% but only in grid cells in the state of Kentucky. A gridded field of real numbers from 0.0 to 1.0 is needed to accomplish this task, with 0.0 outside of the region of interest and 1.0 completely inside the region. Grid cells on the border of a region would then have some fraction of their emissions, corresponding to the real decimal number in the gridded mask, modified according to the scaling rule.

These masks are provided to CMAQ by at least one offline gridded file, which are identified in the RunScript using environment variables (more later). The *RegionsRegistry* section of the Emission Control Namelist maps each "Region Label" to specific variables on specific files. Here is the *RegionsRegistry* section in the default namelist:
```
&RegionsRegistry
 RGN_NML  =   
 !          | Region Label   | File_Label  | Variable on File
 !<Default>    'EVERYWHERE'  ,'N/A'        ,'N/A',
 !<Example>    'WATER'       ,'CMAQ_MASKS' ,'OPEN',
 !<Example>    'ALL'         ,'CMAQ_MASKS' ,'ALL',
/
```
As indicated, the Region Label "EVERYWHERE" is active by default and returns a mask that operates uniformly across the entire domain. The "File_Label" field identifies the environment variable in the RunScript that stores the location and name of the file containing the mask. The user may modify this to any name they wish as long as it is consistent with the variable name on the RunScript. The "Variable on File" field identifies the variable on the input file that stores gridded field to be used for this region. Examples are provided for two cases.

In the first case, a region with label "WATER" is defined and referenced to the variable "OPEN", which is short for *open water*. Using this "WATER" region will apply a scaling rule only for open water grid cells and fractionally along coastlines. The second example provides a shortcut for files with many variables that are all desired (e.g. states of the Unites States). Rather than listing out all variables on the file and explicitly linking them to "Region Labels", the user can invoke the "All" keyword and all variables will be read and stored with "Region Labels" that equal the names of the variables on the file.

## B.5 Aerosol Size Distributions
The treatment of aerosol size distributions in CMAQv5.3 has been updated to be more consistent with the way particle sizes and modes are treated by the National Emission Inventory and in emissions processing tools like SMOKE, MOVES, SPECIATE, and Speciation Tool. Specifically, in these tools, aerosol emissions are typically parameterized into two main modes, Fine and Coarse. Although the size distribution parameters (i.e. total number, diameter, standard deviation, etc.) for these modes will vary among emission sources, previous versions of CMAQ assumed that all primary fine particles had the same size distribution upon emission. Coarse-mode particles were assumed to exhibit a larger diameter but were also uniform across all sources (excluding wind-blown dust and sea spray).

In CMAQv5.3, users link particle emission surrogates to CMAQ particle species via the Emission Control Namelist. Examples of default mapping rules can be found in any of the Emission Control Namelists in the CMAQ repository. The three lines below assign emissions for all streams for particulate-phase sulfate, ammoium, and nitrate.
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'ALL'         ,'PSO4'   ,'ASO4'        ,'FINE',1.0   ,'UNIT','a',
'EVERYWHERE'  , 'ALL'         ,'PNH4'   ,'ANH4'        ,'FINE',1.0   ,'UNIT','a',
'EVERYWHERE'  , 'ALL'         ,'PNO3'   ,'ANO3'        ,'FINE',1.0   ,'UNIT','a',
```
The CMAQ-Species field should be popoluated with bulk chemical names (e.g. ASO4, AEC, AK, ACA, etc). In other words, the 'i','j', or 'k' which usually designates the mode of the aerosol species name should be omitted. A list of the valid aerosol bulknames exists in the source file "AERO_DATA.F" in the array named "aerolist". The user should also identify the aerosol mode to be populated using the "Phase/Mode" field. In the example above, all of the rules identify the "FINE" mode as the desitination mode. CMAQ uses this value to look up the size distribution parameters (diameter and standard deviation) to apply for this particular emission.

Aerosol mode keywords are linked to reference mode labels in the SizeDistributions section of the Emission Control Namelist. These assignments can be made for all streams at once, as demonstrated by the first two default entries initializing the 'FINE' and 'COARSE' modes, or they can be made on a stream-by-stream basis as shown below for Wind-Blown Dust and Sea Spray aerosol.
```
&SizeDistributions
 SD_NML    =
 !         | Stream Label   | Mode Keyword | Ref. Mode
 !<Default>  'ALL'          ,'FINE'        ,'FINE_REF',
 !<Default>  'ALL'          ,'COARSE'      ,'COARSE_REF',
             'WBDUST'       ,'FINE'        ,'FINE_WBDUST',
             'WBDUST'       ,'COARSE'      ,'COARSE_WBDUST',
             'SEASPRAY'     ,'FINE'        ,'FINE_SEASPRAY',
             'SEASPRAY'     ,'COARSE'      ,'COARSE_SEASPRAY',
 !<Example>  'AIRCRAFT'     ,'FINE'        ,'AIR_FINE',   !To use these examples, you
 !<Example>  'AIRCRAFT'     ,'COARSE'      ,'AIR_COARSE', ! must add entries for AIR_FINE
                                                          ! and AIR_COARSE to the data structure
                                                          ! em_aero_ref in AERO_DATA.
```
The 'Ref. Mode Labels' are used to lookup size distrubution parameters in AERO_DATA.F. The following reference modes are defined in the public repo:
```
TYPE em_aero
    Character( 20 ) :: name
    Real            :: split( n_mode )  ! dimensionless
    Real            :: dgvem( n_mode )  ! meters
    Real            :: sgem ( n_mode )  ! dimensionless
END TYPE em_aero
INTEGER, PARAMETER  :: n_em_aero_ref = 9

TYPE( em_aero ), Parameter :: em_aero_ref( n_em_aero_ref ) = (/

!              ----Name----     -----Split-----    ---Geo. Mean Diameter---   ---Stnd Dev.---
& em_aero('FINE_REF       ',(/0.1,0.9,0.0/),(/0.06E-6,0.28E-6 ,6.0E-6 /),(/1.7,1.7,2.2/)), ! Default Accum and Aitken Mode
& em_aero('ACC_REF        ',(/0.0,1.0,0.0/),(/0.06E-6,0.28E-6 ,6.0E-6 /),(/1.7,1.7,2.2/)), ! Just Accumulation Mode
& em_aero('COARSE_REF     ',(/0.0,0.0,1.0/),(/0.06E-6,0.28E-6 ,6.0E-6 /),(/1.7,1.7,2.2/)), ! Just Coarse Mode
& em_aero('UNITY_REF      ',(/1.0,1.0,1.0/),(/0.06E-6,0.28E-6 ,6.0E-6 /),(/1.7,1.7,2.2/)), ! Used for online sectors (e.g. SeaSpray)
& em_aero('ZERO_REF       ',(/0.0,0.0,0.0/),(/0.06E-6,0.28E-6 ,6.0E-6 /),(/1.7,1.7,2.2/)), ! Zero out the emissions
& em_aero('FINE_WBDUST    ',(/0.0,1.0,0.0/),(/0.06E-6,1.391E-6,5.26E-6/),(/1.7,2.0,2.0/)), ! Default Fine Wind-Blown Dust Parameterization
& em_aero('COARSE_WBDUST  ',(/0.0,0.0,1.0/),(/0.06E-6,1.391E-6,5.26E-6/),(/1.7,2.0,2.0/)), ! Default Coarse Wind-Blown Dust Param.
& em_aero('FINE_SEASPRAY  ',(/0.0,1.0,0.0/),(/0.06E-6,1.391E-6,5.26E-6/),(/1.7,2.0,2.0/)), ! Fine Sea Spray Parameterization is Dynamic.
& em_aero('COARSE_SEASPRAY',(/0.0,0.0,1.0/),(/0.06E-6,1.391E-6,5.26E-6/),(/1.7,2.0,2.0/))  ! Coarse Sea Spray Parameterization is Dynamic.
                                                                                           !  The values here are not actually used but
                                                                                           !  are replaced in SSEMIS when FACNUM and FACSRF
                                                                                           !  are calculated online.
& /)
````
Users can add as many new size distributions as they want, as long as they increment the variable n_em_aero_ref to always equal the number of size distirbutions in the lookup array (em_aero_ref).

CMAQ will use the size distribution reference value linked to each rule via the phase/mode keyword to calculate the fraction of each aerosol primary emission that should go into the 'i', 'j', and 'k' modes in the internal aerosol module. At first, it may seem that the lining step between phase/mode keywords and reference mode labels is unnecesary, but it serves an important function. As stated earlier, it is common that modes of similar size from a variety of sources will be referred to by common names like 'FINE' and 'COARSE', even though the size distribution parameters may differ considerably. With the linking step provided in the SizeDistributions section, parameters for several streams can be specified individually, but all be labeled 'FINE' and applied with one rule in the EmissionsScalingRules section.

In the example above, fine mode Wind-Blown Dust are linked to 'FINE_WBDUST', sea spray aerosols are linked to 'FINE_SEASPRAY' and all other sources are linked to 'FINE_REF'. Thus different size distributions will be calculated for each of these streams. However, if the user wants to scale the mass of all fine mode aerosol by a factor of 2, the following emission rule is valid:
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'ALL'         ,'ALL'    ,'ALL'         ,'FINE',1.0   ,'UNIT','m',
```

## B.6 Additional DESID Features
### B.6.1 Summary Output to Processor-Specific Logfiles
Diagnostic output is an important feature of the new emissions module, DESID. Because the impact of emissions is so critical for CMAQ predictions and because the features available for scaling emissions are now quite complex, a comprehensive text-based output has been added to the CMAQ logfiles to enhance transparency.

The logfiles now provide several lists of information to help protect users from mistakes like inconsistent naming between emissions and CMAQ speciation. First, CMAQ reports for each stream the number and names of all of the surrogate species that were not used. Second, prints the names of surrogates that the user told it to look for but that it could not find on any of the emission streams. If the environment variable:
```
setenv CTM_EMISCHK Y         #> Abort CMAQ if missing surrogates from emissions Input files
```
is set to 'Y' or 'True', then the model will abort if it cannot find any individual surrogate. If the variable is set to 'N' or 'False' then CMAQ will print a warning and proceed.

Finally, CMAQ loops through stream and outputs the size distribution modes available for each stream and the full list of every emission instructions applied to each stream. These are ordered by CMAQ species (with 'i', 'j', and 'k' modes listed separately) and surrogate species name so that a full understanding of the scaling rules applied to each CMAQ species' emissions can be grasped quickly. Columns are printed for the applicable region of the grid, the phase/mode applied, the input scale factor, the scaling basis, the operation, and the final scale factor applied taking into account any molecular weight conversions, if needed, and size distribution fractions.

### B.6.2 Diagnostic Gridded Output Files
Many complex scaling procedures are now possible with DESID. Users are advised to confirm that the emissions are scaled the way they have intended. One tool to help this step is the Gridded Diagnostic Output. This is enabled on a stream-by-stream basis in the CMAQ RunScript with the following options:
```
# Gridded Emissions Diagnostic files
  setenv GR_EMIS_DIAG_001 TRUE
  setenv GR_EMIS_DIAG_002 2D

# Stack emissions diagnostic files
  setenv STK_EMIS_DIAG_001 2DSUM
  setenv STK_EMIS_DIAG_002 2DSUM
  setenv STK_EMIS_DIAG_003 FALSE
  setenv STK_EMIS_DIAG_004 2DSUM
  setenv STK_EMIS_DIAG_005 2DSUM
```
The lines above set the behavior of the gridded diagnostic output for gridded and inline emission streams. The values available for each stream are 'TRUE', 'FALSE', '2D', '2DSUM', and '3D'. The '2D' option prints just the surface layer of emissions for a particular stream. The '3D' option prints all layers populated by that stream. The '2DSUM' option prints one 2D field, but it equals the column of sum of emissions throughout the gridded model domain. The 'TRUE' option equates to '2D'. The user can also set the diagnostic behavior of online streams using the following variables:
```
setenv BIOG_EMIS_DIAG TRUE
setenv MG_EMIS_DIAG TRUE
setenv LTNG_EMIS_DIAG TRUE
setenv DUST_EMIS_DIAG TRUE
setenv SEASPRAY_EMIS_DIAG TRUE
```
The gridded diagnostic output files that are created are named systematically with the format "CCTM_EMDIAG_[XXX]_[CTM_APPL]_[DATE].nc" where XXX is the emissions stream label, CTM_APPL is the application name defined in the CCTM runscript, and DATE is the date of the simulation. In order to change the default value of all emission streams modify the "EMIS_DIAG" variable:
```
setenv EMIS_DIAG TRUE
```
The emission rates printed to the diagnostic files reflect all of the scaling rules applied and are written just before the emissions are added to the CMAQ transport module. Because the model interpolates in time, it is very likley that the rates written to the diagnostic file will not correspond in time to the rates from the input files. In most cases, the rates will be one-half time step before the top of the hour, the time point of the emission inputs. For this reason, it is not entirely helpful for users to compare the scaled emissions directly to the rates on the input files. However, comparing them qualitatively can be helpful.


[<< Previous Appendix](CMAQ_UG_appendixA_model_options.md) - [Home](../README.md) - [Next Appendix >>](CMAQ_UG_appendixC_spatial_data.md)<br>
CMAQ User's Guide (c) 2019<br>
