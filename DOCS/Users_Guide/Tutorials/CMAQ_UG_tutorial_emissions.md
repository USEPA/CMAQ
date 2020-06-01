## CMAQ Tutorial ##
### Prescribing Emissions Using the DESID, the Detailed Emissions Scaling, Isolation and Diagnostics Module ###
Purpose: This tutorial will guide users to utilizing the Emission Control namelist to perform some basic manipulation
of their emission streams. For additional questions, contact Ben Murphy (murphy.ben@epa.gov).

------------

### Definitions of Terms
- Stream: An Online emission source or a group of sources processed offline and read into CMAQ from one file. Common
examples of streams include biogenic VOCs, mobile sources, wind-blown dust, prescribed fires, electric-generating units,
residential heating, etc.  
- Species: A variable representing a chemical compound or group of compounds in CMAQ.  
- Surrogate: A variable representing a chemical compound or group of compounds on an emission Stream.  

### Important Notes to Remember
- Rules are applied in the order they are provided.

### Example Use Cases
- [1. Zero out emissions](#zero_out)  
- [2. Add emissions for a new tracer species](#add_emissions)  
- [3. Scale emissions from one stream by a common factor](#scale_stream)  
- [4. Scale emissions for one species on all streams](#scale_species)  
- [5. Scale all gas phase emissions but leave aerosols alone](#scale_gases)  
- [6. Scale all aerosols](#scale_aerosols)  
- [7. Add or subtract emissions from one surrogate to existing emissions]([#add_surrogate)  
- [8. Overwrite the scale factor for a single stream or species](#overwrite)  
- [9. Scale all species except one by a common factor](#scale_all_but_one)  
- [10. Apply scaling while conserving moles or mass](#scale_moles_mass)  
- [11. Apply scaling with spatial dependence](#apply_mask) 
- [12. Define families of streams, regions, or chemical species](#define_families) 
- [13. Use a family of streams to scale emissions for a group of sources](#fam_stream)  
- [14. Use a family of regions to scale emissions in a new location](#fam_region)  
- [15. Use a family of species to scale emissions for a custom group of pollutants](#fam_chem)  
- [A1. Appendix: Example Emission Control File](#appendix1)  
- [A2. Appendix: Example Emissions Section of CCTM RunScript File](#appendix2)  


<a id=zero_out></a>
### 1. Zero Out Emissions
Emission streams can be zeroed using the options for individual streams in the CMAQ RunScript or creating rules in the Emission Control Namelist.

##### a. Using Options in the CMAQ RunScript
For gridded or inline emissions, just reduce the value of N_EMIS_GR or N_EMIS_PT, respectively and adjust the values of the file paths and stream labels accordingly, if necessary.

Note that if you zero out the sea-spray or wind-blown dust emissions, you should also edit the emission control file by commenting out the coarse and fine species expected from those modules. Some of these species are used by both emission streams, so if you only want to zero out the sea-spray or dust stream but not the other stream, you will need to determine which species to comment out. Please check the AERO_DATA module for the list of species produced by each stream.  

Alternatively, you may set the CTM_EMISCHK variable to FALSE in the runscript to avoid crashing CMAQ if it can't find species it is looking for from emissions streams that have been disabled.  

To zero Sea Spray aerosol emissions,
```
setenv CTM_OCEAN_CHEM N
```
To zero Wind-Blown Dust aerosol emissions,
```
setenv CTM_WB_DUST N
```
To zero online Biogenic VOC emissions calculated by BEIS,
```
setenv CTM_BIOGEMIS N
```
To zero Lightning NO emissions,
```
setenv CTM_LTNG_NO N
```

##### b. Creating Rules in the Emission Control Namelist
All streams can be zeroed by creating a rule that refers to 'All' streams. For example,
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE' , 'All'          ,'All'    ,'All'         ,'All' ,0.    ,'UNIT','o',
```
Alternatively, individual streams can be zeroed by creating rules that refer to specific streams.
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'SEASPRAY'    ,'All'    ,'All'         ,'All' ,0.    ,'UNIT','o',
'EVERYWHERE'  , 'WBDUST'      ,'All'    ,'All'         ,'All' ,0.    ,'UNIT','o',
'EVERYWHERE'  , 'BIOG'        ,'All'    ,'All'         ,'All' ,0.    ,'UNIT','o',
'EVERYWHERE'  , 'LTNG'        ,'All'    ,'All'         ,'All' ,0.    ,'UNIT','o',
'EVERYWHERE'  , 'MOBILE'      ,'All'    ,'All'         ,'All' ,0.    ,'UNIT','o',
```
The last rule assumes that one of the emissions streams has been labeled 'MOBILE' in the RunScript. For example,
```
setenv GR_EMIS_LAB_001 MOBILE
```

<a id=add_emissions></a>
### 2. Add Emissions For a New Chemical Species
If a species is named "CHEMX" on the gas, aerosol, or nonreactive namelist, then a rule can be created on the Emission Control Namelist that will link that new species to any existing surrogate from the emission streams. In this example, CHEMX is scaled to 25% of NO emissions.
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'All'         ,'NO'     ,'CHEMX'       ,'All' ,0.25  ,'UNIT','a',
```
Note that the 'a' operator is important here to create the new link from 'CHEMX' to NO. If we want to scale to 25% of total NO<sub>x</sub> concentrations, we can add a line to include NO<sub>2</sub>.
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'All'         ,'NO'     ,'CHEMX'       ,'All' ,0.25  ,'UNIT','a',
'EVERYWHERE'  , 'All'         ,'NO2'    ,'CHEMX'       ,'All' ,0.25  ,'UNIT','a',
```

<a id=scale_stream></a>
### 3. Scale emissions from one stream by a common factor
The following rule in the Emissions Control Namelist will scale all emissions from one example stream ("MOBILE") by a factor of 2. The operator 'm' will multiply the cumulative value of all scale factors that have been applied for the "MOBILE" stream to that point.
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'MOBILE'      ,'All'    ,'All'         ,'All' ,2.0  ,'UNIT','m',
```

<a id=scale_species></a>
### 4. Scale emissions for one species on all streams
To scale emissions for one species, in this case elemental (or black) carbon, on all streams by a factor of 0.5 (50%), use the following rule:
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'All'         ,'All'    ,'AEC'         ,'All' ,0.5  ,'UNIT','m',
```

<a id=scale_gases></a>
### 5. Scale all gas phase emissions but leave aerosols alone
If the user wants to target all gas-phase emissions broadly, but leave aerosol emissions alone, the "Phase/Mode" field may be used. The following rule scales all gas-phase emissions by 66%.
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'All'         ,'All'    ,'All'         ,'GAS' ,0.66  ,'UNIT','m',
```
And this rule is just for one stream, for example, the hypothetical "MOBILE" stream:
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'MOBILE'      ,'All'    ,'All'         ,'GAS' ,0.66  ,'UNIT','m',
```

<a id=scale_aerosols></a>
### 6. Scale all aerosols
If instead, the user is interested in scaling all aerosol species by a factor of 3.0 and leaving gas-phase emissions alone, a rule of this form may be used:
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'All'         ,'All'    ,'All'         ,'AERO',3.0  ,'UNIT','m',
```

<a id=scale_surrogate></a>
### 7. Add or subtract emissions from one surrogate to existing emissions
If the following rule is already present on the default emission control namelist,
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'All'         ,'TOL'    ,'TOL'         ,'GAS' ,1.0  ,'UNIT','a',
```
and the user wants to add or subtract toluene emissions based on the value of a different emission surrogate, CO for example, then this rule could be used:
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'All'         ,'CO'     ,'TOL'         ,'GAS' ,0.1  ,'UNIT','a',
```
This rule adds more toluene emissions at the rate of 10% of CO emissions to every stream that has CO emissions. If the user wants to subtract 10% of CO emissions instead:
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'All'         ,'CO'     ,'TOL'         ,'GAS' ,-0.1  ,'UNIT','a',
```
Note it is important to use the 'a' operator for these rules since the effects of these rules should be added to existing scale factors.

<a id=overwrite></a>
### 8. Overwrite the scale factor for a single stream or species
If the following rule is already present on the default emission control namelist,
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'All'         ,'TOL'    ,'TOL'         ,'GAS' ,1.0  ,'UNIT','a',
```
and the user wants to overwrite the scale factor with a different one, 30% for example, use a rule of this form:
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'All'         ,'TOL'    ,'TOL'         ,'GAS' ,0.3 ,'UNIT','o',
```

<a id=scale_all_but_one></a>
### 9. Scale all species except one by a common factor
Putting examples 3 and 8 together then can give us this desired result. If the Emission Control Namelist already has unity scaling for all emissions by default, then the user may scale all emissions from all streams by a factor of 2.0. Then the user can reset the scale factor of one species, NO for example.
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'All'         ,'All'    ,'All'         ,'GAS' ,2.0 ,'UNIT','m',
'EVERYWHERE'  , 'All'         ,'NO'     ,'NO'          ,'GAS' ,1.0 ,'UNIT','o',
```

<a id=scale_moles_mass></a>
### 10. Apply scaling while conserving moles or mass
The user may elect to conserve moles or mass instead of applying factors directly using the "Basis" field. For example, the rule
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'All'         ,'CO'     ,'ANO3'        ,'FINE' ,0.15 ,'MASS','a',
```
will add 15% of CO emissions to the emissions of fine-mode particulate nitrate, but the scale factor will also be adjusted by multiplying by the molecular weight of CO to conserve mass.

Reminder: gas-phase emission rates are usually provided to CMAQ in molar units while particle emissions are usually provided in mass. Note that if the user scales a particle species to a gas surrogate, or vice-versa, it is important in most cases to choose either "MASS" or "MOLE" appropriately for the Basis to ensure proper unit conversions. 

If the user is scaling one gas species to another gas surrogate, both will likely have molar emissions units. In this case, selecting "MOLE" as the basis will be equivalent to selecting "UNIT". In other words, there will be no modification of the user-defined scale factor due to unit conversion concerns. If, however, "MASS" is selected, then the scale factor will be modified by first multiplying by the molecular weight of the surrogate and then dividing by the molecular weight of the CMAQ species.
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'All'         ,'CO'     ,'ETHYLBENZ'   ,'GAS' ,0.003 ,'MASS','a',
```

<a id=apply_mask></a>
### 11. Apply scaling with spatial dependence
The user may apply a scale factor to a specific area of the domain by identifying the name of the mask to be used in the emission rule. For example, this rule increases all emissions in "KENTUCKY" by 50%:
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'KENTUCKY'    , 'All'         ,'All'    ,'All'         ,'All' ,1.50 ,'UNIT','m',
```
The label for "KENTUCKY" should be linked to a specific gridded variable mask (of real numbers) using the "RegionsRegistry" section on the Emission Control Namelist.
```
&RegionsRegistry
 RGN_NML  =   
 !          | Region Label   | File_Label  | Variable on File
 !<Default>    'EVERYWHERE'  ,'N/A'        ,'N/A',
 !<Example>    'WATER'       ,'CMAQ_MASKS' ,'OPEN',
 !<Example>    'ALL'         ,'CMAQ_MASKS' ,'ALL',
               'KENTUCKY'    ,'CMAQ_STATES','KY',
/
```
This is just an example of defining one mask named "KENTUCKY". For a complete explanation of the spatial-dependent scaling feature, see the CMAQ Appendix [B.4 "Applying Masks"](../Appendix/CMAQ_UG_appendixB_emissions_control.md#b4-applying-masks-for-spatial-dependence).

Two example mask files are available on the CMAS Data Warehouse: US states grid mask file and NOAA climate regions grid mask file.  These mask files can be used with the 12US1 modeling grid domain (grid origin x = -2556000 m, y = -1728000 m; N columns = 459, N rows = 299).

* [Link to grid mask files on CMAS Data Warehouse Google Drive](https://drive.google.com/drive/folders/1x9mJUbKjJaMDFawgy2PUbETwEUopAQDl)
* [Link to metadata for the grid mask files is posted on the CMAS Center Dataverse site](https://doi.org/10.15139/S3/XDYYB9)


<a id=define_families></a>
### 12.  Define families of streams, regions, or chemical species
Users can define any number of custom groups or "families" of emission streams, regions or chemical species to be used to streamline (i.e. enhance) prescribed emissions rules. For example, if a user would like to scale NOx by 50% from 4 different emission streams (e.g. PT_EGU, GRIDDED, MOBILE and PT_NONEGU) without using famlies, they would need 8 rules, one for NO and NO2 for each of 4 streams. However, by defining a family of 4 streams and another family of two chemical species (i.e. NOx), 1 rule can be used to achieve the same result.  

Chemical families are defined by prescribing, via the Emission Control File, the total number of chemical families to be used, the name of each, the number of members of each family, and the name of each family member. For example,  
```
&ChemicalFamilies
 NChemFamilies         = 2     
 ChemFamilyName(1)     = 'NOX'    
 ChemFamilyNum(1)      = 2  
 ChemFamilyMembers(1,:)= 'NO','NO2'  
 ChemFamilyName(2)     = 'POA'    
 ChemFamilyNum(2)      = 2  
 ChemFamilyMembers(2,:)= 'POC','PNCOM'  
/
```  
In this example, only 1 chemical family "NOX" is defined with 2 members, "NO" and "NO2".  
Stream families are defined analogously:  
```
&StreamFamilies  
 NStreamFamilies         = 1  
 StreamFamilyName(1)     = 'CONTROLLED_SOURCES'  
 StreamFamilyNum(1)      = 4  
 StreamFamilyMembers(1,:)= 'PT_EGU','GRIDDED','MOBILE','PT_NONEGU'  
/  
```

As are region families:  
```
&RegionFamilies
 NRegionFamilies         = 1
 RegionFamilyName(1)     = 'SouthEastUS'
 RegionFamilyNum(1)      = 9
 RegionFamilyMembers(1,:)= 'KY','VA','TN','NC','MS','AL','GA','SC','FL'
/
```

<a id=fam_stream></a>
### 13. Use a family of streams to scale emissions for a group of sources
To then use a stream family to apply a rule to multiple streams, just use the family name in the Stream Label column. 
```
! Region      | Stream Label         |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |                      |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'CONTROLLED_SOURCES' ,'NO2'    ,'NO2'         ,'GAS' ,0.50 ,'UNIT','m',
```

<a id=fam_region></a>
### 14. Use a family of regions to scale emissions in a new location
To use a region family, use the family name in the Region Label column.  
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'SouthEastUS' , 'ALL'         ,'NO2'    ,'NO2'         ,'GAS' ,0.50 ,'UNIT','m',
```

<a id=fam_chem></a>
### 15. Use a family of species to scale emissions for a custom group of pollutants
Chemical families may be applied in the CMAQ-species column:  
```
! Region      | Stream Label |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |              |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'GRIDDED'    ,'ALL'    ,'NOX'         ,'GAS' ,0.50 ,'UNIT','m',
```  
or in the Emission Surrogate column:  
```
! Region      | Stream Label |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |              |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'GRIDDED'    ,'NOX'    ,'ALL'         ,'GAS' ,0.50 ,'UNIT','m',
```  
In both of these cases, both NO and NO2 (as NOx is defined above) are multiplied by 50%. The same is accomplished by using NOX in both columns.  
```
! Region      | Stream Label |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |              |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'GRIDDED'    ,'NOX'    ,'NOX'         ,'GAS' ,0.50 ,'UNIT','m',
```  
Because the 'm' operator is used, CMAQ will look for pre-existing relationships between the members of 'NOX' in order to apply the scaling rule, which 'multiplies' the existing scaling by 50%. So this example assumes that the following two rules, or something similar, preceed the instructions in this section:  
```
! Region      | Stream Label |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |              |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'GRIDDED'    ,'NO'     ,'NO'          ,'GAS' ,1.00  ,'UNIT','a',
'EVERYWHERE'  , 'GRIDDED'    ,'NO2'    ,'NO2'         ,'GAS' ,1.00  ,'UNIT','a',
```  
In this case, CMAQ is adding a relationship between NO and NO2 surrogates and model species. Thus families are most useful when using the 'm' or 'o' operators. 

However, sometimes the 'a' operator is useful with chemical families. In the example below, a relationship is added between POA surrogates (defined in example 12 above) and CMAQ model species:  
```
! Region      | Stream Label |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |              |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'GRIDDED'    ,'POA'    ,'ALVPO1'      ,'FINE',0.09  ,'UNIT','a',
```  
CMAQ will use this rule to add POC and PNCOM surrogates together, multiply by 0.09 and assign their emissions to ALVPO1, a semivolatile POA species.  
The way CMAQ uses chemical families for adding relationships with the 'a' is nuanced. The following logic is applied: 
- If a chemical family is used for either the emissions surrogate or the CMAQ-Species but not both, then connections are made between each member of the family and the prescribed single-species in the other column.  
- If both columns include chemical families or the 'ALL' keyword, then each pair of members will be compared. If the names match exactly or a relationship already exists, then the 'a' operation will be applied. If not, then the pair will be ignored. This precaution is in place to protect against the case where a user prescribes an addition (i.e. 'a') rule with the keyword 'ALL' or very large chemical families in both the Emission Surrogate and CMAQ-Species columns. Without the precaution in place, adding relationships for ALL surrogates to ALL model species would be an extremely large data structure and almost certainly not an intended use of CMAQ.   


<a id=appendix1></a>
### A1. Appendix 1: Example Emission Control File
```
!------------------------------------------------------------------------------!
! EMISSION CONTROL INPUT FILE                                                  !
!  FOR THE                                                                     !
! COMMUNITY MULTISCALE AIR QUALITY (CMAQ) MODEL                                !
!  DEVELOPED AND MAINTAINED BY THE                                             !
! NATIONAL EXPOSURE RESEARCH LABORATORY, OFFICE OF RESEARCH AND DEVELOPMENT    !
! UNITED STATES ENVIRONMENTAL PROTECTION AGENCY                                !
!                                                                              !
! THIS VERSION CONSISTENT WITH THE RELEASE OF CMAQv5.3 (SPRING 2019)           !
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Emissions Scaling Specification Section                                      !
!   Each rule is presented as a row of inputs. The syntax of the               !
!   emissions scaling rules is the following:                                  !
!      EM_INSTR(I) = 'Region Label', 'Stream Label', 'Emission Surrogate',     !
!                     'CMAQ-Species', 'Phase/Mode', 'Scale Factor', 'Operation'!
!   Definition of Fields:                                                      !
!         'Region Label' - Apply scaling for specific regions of the domain.   !
!         'Stream Label' - Short Name from Run Script (ie. the value of        !
!                          GR_EMIS_01_LAB or STK_EMIS_01_LAB). There are a few !
!                          reserved names that apply to inline emissions       !
!                          streams. These are:                                 !
!                            BIOG - Biogenic VOC emissions                     !
!                            MGEM - Marine Gas Emissions                       !
!                            LTNG - Lightning NO Emissions                     !
!                            WBDUST - Wind-Blown Dust Emissions                !
!                            SeaSpray - Sea Spray Aerosol Emissions            ! 
!         'Emission   - The character string identifying the surrogate on the  !
!          Surrogate'   emission file or in the inline calculation that the    !
!                       CMAQ species should be mapped to. Usually this name is !
!                       the same as the CMAQ species for convenience. For      !
!                       aerosols, its usually slightly different (e.g ANO3 vs.!
!                       PNO3)                                                  !
!         'CMAQ-Species' - Internal Species Name                               !
!         'Phase/Mode'- If the CMAQ-Species is a Gas, this field should equal  !
!                       'Gas'. If the CMAQ-Species is an aerosol, this field   !
!                       should indicate one of the possible emission aerosol   !
!                       modes. Every stream by default is given a 'COARSE' and !
!                       'FINE' mode. The user may refer to these, or define    !
!                       others above and refer to them as well. This level of  !
!                       specificity is needed so that aerosol number and       !
!                       surface area are calculated correctly, and so that any !
!                       unit conversions between gases and aerosols can be     !
!                       handled correctly.                                     !
!         'Scale Factor' - Adjustment factor to be applied to the mapping      !
!         'Operation' - Specifies what kind of rule is to be carried           !
!                       out. Options are:                                      !
!                          'a' - add the rule to existing instructions. This   !
!                                operation should be used for new entries, too.!
!                          'm' - find existing scaling instructions matching   !
!                                this rules features (ie. species, streams,   !
!                                etc) and multiply them by the factor in this  !
!                                particular rule.                              !
!                          'o' - find existing scaling instructions matching   !
!                                this rule and overwrite them.                 !
!                                                                              !
!                                                                              !
!------------------------------------------------------------------------------!

&EmissionScalingRules
 EM_NML=
 ! Region      | Stream Label  |Emission | CMAQ-   |Phase/ |Scale  |Basis | Op  
 !  Label      |               |Surrogate| Species |Mode   |Factor |      |     
 ! 'WATER'     , 'All'         ,'All'    ,'All'    ,'All'  ,0.     ,'MASS','o', !Zero out all emissions over 
                                                                                ! water grid cells
 ! 'EVERYWHERE', 'ONROAD_GAS'  ,'NO'     ,'NO'     ,'GAS'  ,2.     ,'MOLE','o', !Scale NO from gasoline 
                                                                                ! vehicles by a factor of 2
 ! 'EVERYWHERE', 'ONROAD_GAS'  ,'NO'     ,'NO'     ,'GAS'  ,2.     ,'MOLE','m', !Alternative: Scale NO 
                                                                                ! from gasoline vehicles
                                                                                ! by a factor of 2
 ! 'EVERYWHERE', 'FIRES'       ,'POC'    ,'ALL'    ,'ALL'  ,0.5    ,'MASS','m', !Scale all Organic Carbon 
                                                                                ! mass from fires by 50%
 ! 'EVERYWHERE', 'FIRES'       ,'PNCOM'  ,'ALL'    ,'ALL'  ,0.5    ,'MASS','m', !Scale all Organic Non-Carbon 
                                                                                ! mass from fires by 50%
 ! 'EVERYWHERE', 'FIRES'       ,'PNCOM'  ,'ALL'    ,'ALL'  ,0.5    ,'MASS','m', !Scale all Organic Non-Carbon 
                                                                                ! mass from fires by 50%
 
 ! 'EVERYWHERE', 'AIRCRAFT'    ,'VOC_INV','PAR'    ,'GAS'  ,0.02   ,'MOLE','a', !Add more mass to PAR from aircraft
                                                                                         ! equal to 2% of the VOC_INV from
                                                                                         ! aircraft
 ! 'EVERYWHERE','ALL'          ,'ALL'    ,'VOC'    ,'GAS'  ,0.5    ,'UNIT','m', !Scale all species defined as members
                                                                                ! of the VOC family (see families below)
                                                                                ! by 50%
 ! 'REGION9'   ,'ALL'          ,'ALL'    ,'NO2'    ,'GAS'  ,2.0    ,'UNIT','m', !Scale NO2 in the region labelled
                                                                                ! "REGION9" by 200%
 ! 'EVERYWHERE','PT_SOURCES'   ,'PEC'    ,'AEC'    ,'FINE' ,0.1    ,'UNIT','m', !Scale black carbon PM from the custom
                                                                                ! stream family "PT_SOURCES" to 10%
 ! 'REGION9'   ,'PT_SOURCES'   ,'ALL'    ,'VOC'    ,'GAS'  ,4.0    ,'UNIT','m', !Scale all VOCs from the family 
                                                                                ! "PT_SOURCES" by 400% only in the region
                                                                                ! labelled "REGION9"
 ! 'REGION9'   ,'PT_SOURCES'   ,'NVOL'   ,'HONO'   ,'GAS'  ,0.66   ,'UNIT','a', !Add 0.66*NVOL as HONO in the region
                                                                                ! labelled "REGION9", but only from 
                                                                                ! streams in the "PT_SOURCES" family


/

!------------------------------------------------------------------------------!
! Size Distribution Specification Section                                      !
!   Each size distribution rule either modifies the parameters associated with !
!   the aerosol modes of a particular stream, or adds new modes to a particular!
!   stream if they do not already exist.                                       !
!                                                                              !
!   Definition of Fields:                                                      !
!      'Stream - Label for the emissions stream that the instruction will      !
!        Label'   apply to. If the label is 'ALL', then the instruction will   !
!                 be expanded to apply to all streams.                         !
!      'Surrogate Mode' - With this label, the user identifies which mode from !
!                         the emissions is to be modified or created. With this!
!                         specificity, multiple modes can be defined and mapped!
!                         in the emissions instructions in the next section.   !
!      'Reference Mode' - This label maps the emissions surrogate aerosol mode !
!                         to specific parameters catalogued in the AERO_DATA   !
!                         module.
!------------------------------------------------------------------------------!

&SizeDistributions
 SD_NML    = 
 !         | Stream Label   | Surr. Mode   | Ref. Mode 
 !<Default>  'ALL'          ,'FINE'        ,'FINE_REF',
 !<Default>  'ALL'          ,'COARSE'      ,'COARSE_REF',
 !<Example>  'AIRCRAFT'     ,'FINE'        ,'AIR_FINE',   !To use these examples, you 
 !<Example>  'AIRCRAFT'     ,'COARSE'      ,'AIR_COARSE', ! must add entries for AIR_FINE
                                                          ! and AIR_COARSE to the data structure
                                                          ! em_aero_ref in AERO_DATA.

/

!------------------------------------------------------------------------------!
! Region-Based Scaling Specification Section                                   !
!   It is possible in CMAQ to scale emissions for a subset of the model domain !
!   using gridded masks to indicate where the scaling should occur. These masks!
!   should be of type real and provided as variables on a file with format     !
!   consistent with IO-API. Any number of files and variables may be used to   !
!   specify 1 or more "regions" to be used in CMAQ. This section of the name-  !
!   list provides users with an interface to name these regions and identify   !
!   the stream data for each.
!                                                                              !
!   Definition of Fields:                                                      !
!      'Region  - Label for the region that is being specified. By default, the!
!        Label'   first region, which will never be specified here, is the     !
!                 whole domain, or "EVERYWHERE". It is included in this file   !
!                 for transparency but should always be commented out.         !
!      'File  - With this label, the user identifies the file that the data for!
!       Label'  this region is stored on. The CMAQ RunScript should provide the!
!               path to this file using the environment variable construct. For!
!               example, to refer to file "CMAQ_REGIONS" with path             !
!               "/home/user/data/cmaq/cmaq_region_file.nc" the specification in!
!               the CMAQ RunScript would look like:                            !
!                  SETENV CMAQ_REGIONS /home/user/data/cmaq/cmaq_region_file.nc!
!               Note that an unlimited number of files can be used here, but   !
!               each must contain a path for reference in the RunScript.       !
!      'Variable  - This label identifies the variable on the region file that !
!        on File'   should be used to populate this particular region. Each    !
!                   variable should be of type real and have dimensions equal  !
!                   to the size of the CMAQ domain. In this way, the fraction  !
!                   of every region should be available for every model grid   !
!                   cell.
!------------------------------------------------------------------------------!

&RegionsRegistry
 RGN_NML  =   
 !          | Region Label   | File_Label  | Variable on File
 !<Default>    'EVERYWHERE'  ,'N/A'        ,'N/A',
 !<Example>    'WATER'       ,'CMAQ_MASKS' ,'OPEN',
 !<Example>    'ALL'         ,'CMAQ_MASKS' ,'ALL',
/

!------------------------------------------------------------------------------!
! Emissions Scaling Family Definitions                                         !
!    This section includes definitions for families of CMAQ chemical species,  !
!    emission streams and region combinations. Please see the Emissions        !
!    Scaling Specification Section for a definitions of CMAQ species, Regions, !
!    and Streams. For each type of family, please indicate the number of       !
!    families you are prescribing (e.g. NChemFamilies=1). Then for each Family !
!    indicate the Name, the number of components, and the name of each         !
!    component. All entries are case-insensitive. See the Emissions tutorial   !
!    in the CMAQ Repository for detailed directions for how to work with       !
!    Families.                                                                 !
!                                                                              !
!    The examples below may be uncommented and modified for your use.          !
!------------------------------------------------------------------------------!

!&ChemicalFamilies
! NChemFamilies         = 1
! ChemFamilyName(1)     = 'NOX'
! ChemFamilyNum(1)      = 2
! ChemFamilyMembers(1,:)= 'NO','NO2'
!/

!&StreamFamilies
! NStreamFamilies         = 1
! StreamFamilyName(1)     = 'PT_SOURCES'
! StreamFamilyNum(1)      = 3
! StreamFamilyMembers(1,:)= 'PT_NONEGU','PT_EGU','PT_OTHER'
!/

!&RegionFamilies
! NRegionFamilies         = 1
! RegionFamilyName(1)     = 'Water'
! RegionFamilyNum(1)      = 2
! RegionFamilyMembers(1,:)= 'SURF','OPEN'
!/
 
```


<a id=appendix2></a>
### A2. Appendix 2: Example Emissions Section of CCTM Run Script

Three environment variables specified in the RunScript are important for broad control over emissions application and diagnostic behavior.
```
#> I/O Controls
...
setenv CTM_EMISCHK Y         #> Abort CMAQ if missing surrogates from emissions Input files
setenv EMISDIAG F            #> Print Emission Rates at the output time step after they have been
                             #>   scaled and modified by the user Rules [options: F | T or 2D | 3D | 2DSUM ]
                             #>   Individual streams can be modified using the variables:
                             #>       GR_EMIS_DIAG_## | STK_EMIS_DIAG_## | BIOG_EMIS_DIAG
                             #>       MG_EMIS_DIAG    | LTNG_EMIS_DIAG   | DUST_EMIS_DIAG
                             #>       SEASPRAY_EMIS_DIAG   
                             #>   Note that these diagnostics are different than other emissions diagnostic
                             #>   output because they occur after scaling.
setenv EMIS_SYM_DATE N       #> Master switch for allowing CMAQ to use the date from each Emission file
                             #>   rather than checking the emissions date against the internal model date.
                             #>   [options: T | F or Y | N]. If false (F/N), then the date from CMAQs internal
                             #>   time will be used and an error check will be performed (recommended). Users 
                             #>   may switch the behavior for individual emission files below using the variables:
                             #>       GR_EM_SYM_DATE_## | STK_EM_SYM_DATE_## [ default : N ]                             
...
#> Emissions Control File
setenv EMISSCTRL_NML ${WORKDIR}/EmissCtrl.nml

#> Spatial Masks For Emissions Scaling
setenv CMAQ_MASKS $SZpath/12US1_surf_bench.nc #> horizontal grid-dependent surf zone file

```
If you want to specify more than one file to be accessed when defining masks, just add their paths with a corresponding variable name. For example,
```
#> Spatial Masks For Emissions Scaling
setenv CMAQ_MASKS_01 12US1_surf_bench.nc #> horizontal grid-dependent surf zone file
setenv CMAQ_MASKS_02 state_boundaries.nc #> horizontal grid-dependent surf zone file
```
The name used for the CMAQ masks file variables are completely arbitrary, as long as that same name is used on the Emission Control File to reference the file.
```
#> Spatial Masks For Emissions Scaling
setenv OCEAN_MASKS 12US1_surf_bench.nc #> horizontal grid-dependent surf zone file
setenv STATES_FILE state_boundaries.nc #> horizontal grid-dependent surf zone file
```

The Run Script also includes a number of variables used for specifying information at the stream level.
```
#> Gridded Emissions Files 
setenv N_EMIS_GR 1
set EMISfile  = emis_mole_all_${YYYYMMDD}_cb6_bench.nc
setenv GR_EMIS_001 ${EMISpath}/${EMISfile}
setenv GR_EMIS_LAB_001 GRIDDED_EMIS
setenv GR_EM_SYM_DATE_001 F

#> In-line point emissions configuration
setenv N_EMIS_PT 5          #> Number of elevated source groups

set STKCASEG = 12US1_2011ek_cb6cmaq_v6_11g              # Stack Group Version Label
set STKCASEE = 12US1_cmaq_cb6e51_2011ek_cb6cmaq_v6_11g  # Stack Emission Version Label

# Time-Independent Stack Parameters for Inline Point Sources
setenv STK_GRPS_001 $IN_PTpath/stack_groups/stack_groups_ptnonipm_${STKCASEG}.nc
setenv STK_GRPS_002 $IN_PTpath/stack_groups/stack_groups_ptegu_${STKCASEG}.nc
setenv STK_GRPS_003 $IN_PTpath/stack_groups/stack_groups_othpt_${STKCASEG}.nc
setenv STK_GRPS_004 $IN_PTpath/stack_groups/stack_groups_ptfire_${YYYYMMDD}_${STKCASEG}.nc
setenv STK_GRPS_005 $IN_PTpath/stack_groups/stack_groups_pt_oilgas_${STKCASEG}.nc
setenv LAYP_STTIME $STTIME
setenv LAYP_NSTEPS $NSTEPS

# Emission Rates for Inline Point Sources
setenv STK_EMIS_001 $IN_PTpath/ptnonipm/inln_mole_ptnonipm_${YYYYMMDD}_${STKCASEE}.nc
setenv STK_EMIS_002 $IN_PTpath/ptegu/inln_mole_ptegu_${YYYYMMDD}_${STKCASEE}.nc
setenv STK_EMIS_003 $IN_PTpath/othpt/inln_mole_othpt_${YYYYMMDD}_${STKCASEE}.nc
setenv STK_EMIS_004 $IN_PTpath/ptfire/inln_mole_ptfire_${YYYYMMDD}_${STKCASEE}.nc
setenv STK_EMIS_005 $IN_PTpath/pt_oilgas/inln_mole_pt_oilgas_${YYYYMMDD}_${STKCASEE}.nc
setenv LAYP_STDATE $YYYYJJJ

# Label Each Emissions Stream
setenv STK_EMIS_LAB_001 POINT_NONEGU
setenv STK_EMIS_LAB_002 POINT_EGU
setenv STK_EMIS_LAB_003 POINT_OTHER
setenv STK_EMIS_LAB_004 POINT_FIRES
setenv STK_EMIS_LAB_005 POINT_OILGAS

# Stack emissions diagnostic files
#setenv STK_EMIS_DIAG_001 2DSUM
#setenv STK_EMIS_DIAG_002 2DSUM
#setenv STK_EMIS_DIAG_003 2DSUM
#setenv STK_EMIS_DIAG_004 2DSUM
#setenv STK_EMIS_DIAG_005 2DSUM

# Allow CMAQ to Use Point Source files with dates that do not
# match the internal model date
setenv STK_EM_SYM_DATE_001 T
setenv STK_EM_SYM_DATE_002 T
setenv STK_EM_SYM_DATE_003 T
setenv STK_EM_SYM_DATE_004 T
setenv STK_EM_SYM_DATE_005 T
```




