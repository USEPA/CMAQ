## CMAQ Tutorial ##
### Prescribing Emissions Using DESID (Detailed Emissions Scaling, Isolation and Diagnostics) Module ###
Purpose: This tutorial will guide users to utilizing the DESID Control namelists to perform some basic manipulation
of their emission streams. For additional questions, contact Ben Murphy (murphy.ben@epa.gov) or visit the CMAS Forum.

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
- [7. Add or subtract emissions from one surrogate to existing emissions](#scale_surrogate)  
- [8. Overwrite the scale factor for a single stream or species](#overwrite)  
- [9. Scale all species except one by a common factor](#scale_all_but_one)  
- [10. Apply scaling while conserving moles or mass](#scale_moles_mass)  
- [11. Apply scaling with spatial dependence](#apply_mask) 
- [12. Define families of streams, regions, or chemical species](#define_families) 
- [13. Use a family of streams to scale emissions for a group of sources](#fam_stream)  
- [14. Use a family of regions to scale emissions in a new location](#fam_region)  
- [15. Use a family of species to scale emissions for a custom group of pollutants](#fam_chem)
- [16. Miscellaneous Notes](#misc_notes)
- [Example DESID Control File](../../../CCTM/src/emis/emis/CMAQ_Control_DESID.nml)  
- [Example DESID Scaling Rules File](../../../CCTM/src/MECHS/cracmm2/CMAQ_Control_DESID_cracmm2.nml)  
- [Example Emissions Section of CCTM RunScript File](../../../CCTM/scripts/run_cctm_Bench_2018_12NE3_CRACMM2.csh#L327)   



<a id=zero_out></a>
### 1. Zero Out Emissions
Emission streams can be zeroed using the options for individual streams in the CMAQ RunScript or creating rules in the DESID Chemical Mapping Control Namelist.

##### a. Using Options in the CMAQ RunScript
For gridded or inline emissions, reduce the value of N_EMIS_GR or N_EMIS_PT, respectively and adjust the values of the file paths and stream labels accordingly, if necessary.

Note that if you zero out the sea-spray or wind-blown dust emissions, you should also edit the emission control file by commenting out the coarse and fine species expected from those modules. Some of these species are used by both emission streams, so if you only want to zero out the sea-spray or dust stream but not the other stream, you will need to determine which species to comment out. Please check the AERO_DATA module for the list of species produced by each stream.  

Alternatively, you may set the CTM_EMISCHK variable to FALSE in the runscript to avoid crashing CMAQ if it cannot find species it is looking for from emissions streams that have been disabled.  

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

##### b. Creating Rules in the Chemical Mapping Control Namelist (CMAQ_Control_DESID_${MECH}.nml)
All streams can be zeroed by creating a rule that refers to 'All' streams. For example,
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE' , 'All'          ,'All'    ,'All'         ,'All' ,0.    ,'UNIT','o',
```
Here, the 'o' operator regers to *overwrite* and will instruct DESID to change existing instructions that emission variables and CMAQ-species to the new Scale Factor. Additionally, individual streams can be zeroed by creating rules that refer to specific streams.
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
If a species is named "CHEMX" on the gas, aerosol, or nonreactive namelist, then a rule can be created on the Chemical Maping Control Namelist that will link that new species to any existing surrogate from the emission streams. In this example, CHEMX is scaled to 25% of NO emissions.
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
The following rule in the Chemical Maping Control Namelist will scale all emissions from one example stream ("MOBILE") by a factor of 2. The operator 'm' will multiply the cumulative value of all scale factors that have been applied for the "MOBILE" stream to that point.
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
If the following rule is already present on the default emission control namelist. It maps, for all streams, the emission variable TOL (for toluene and possibly toluene-like compounds) to the CMAQ Species TOL.
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'All'         ,'TOL'    ,'TOL'         ,'GAS' ,1.0  ,'UNIT','a',
```
The user wants to add or subtract toluene emissions based on the value of a different emission surrogate, CO for example, then this rule could be used:
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
If the following rule is already present on the default emission control namelist. It maps, for all streams, the emission variable TOL (for toluene and possibly toluene-like compounds) to the CMAQ Species TOL.  
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'All'         ,'TOL'    ,'TOL'         ,'GAS' ,1.0  ,'UNIT','a',
```
The user wants to overwrite the scale factor with a different one, 30% for example, use a rule of this form:
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'All'         ,'TOL'    ,'TOL'         ,'GAS' ,0.3 ,'UNIT','o',
```

<a id=scale_all_but_one></a>
### 9. Scale all species except one by a common factor
Putting examples 3 and 8 together then can give us this desired result. If the Chemical Mapping Control Namelist already has unity scaling for all emissions by default, then the user may scale all emissions from all streams by a factor of 2.0. Then the user can reset the scale factor of one species, NO for example.
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

Reminder: gas-phase emission rates are usually provided to CMAQ in molar units while particle emissions are usually provided in mass. Note that if the user scales a particle species to a gas emission variable, or vice-versa, then there will likely be a mass to mole unit conversion necessary. It is important in most cases to choose either "MASS" or "MOLE" for the Basis to ensure that conservation of mass or conservation of moles is preserved, depending on user preference.  

If the user is scaling one gas species to another gas emission variable, both will likely have molar emissions units. In this case, selecting "MOLE" as the basis will be equivalent to selecting "UNIT". In other words, there will be no modification of the user-defined scale factor due to unit conversion concerns. If, however, "MASS" is selected, then DESID will adjust the scale factor by first multiplying by the molecular weight of the emission variable and then dividing by the molecular weight of the CMAQ species. These tasks do not need to be completed by the user.  
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
The label for "KENTUCKY" should be linked to a specific gridded variable mask (of real numbers) using the "Desid_RegionDef" section on the [DESID Control Namelist](../../../CCTM/src/emis/emis/CMAQ_Control_DESID.nml#L137).
```
&Desid_RegionDef
 RGN_NML  =   
 !          | Region Label   | File_Label  | Variable on File
 !<Default>    'EVERYWHERE'  ,'N/A'        ,'N/A',
 !<Example>    'WATER'       ,'CMAQ_MASKS' ,'OPEN',
 !<Example>    'ALL'         ,'CMAQ_MASKS' ,'ALL',
               'KENTUCKY'    ,'CMAQ_STATES','KY',
/
```
This is just an example of defining one mask named "KENTUCKY". For a complete explanation of the spatial-dependent scaling feature, see the CMAQ Appendix [B.3.4 "Defining and Using Regions and Region Families"](../Appendix/CMAQ_UG_appendixB_emissions_control.md).

Two example mask files are available on the CMAS Data Warehouse: US states grid mask file and NOAA climate regions grid mask file.  These mask files can be used with the 12US1 modeling grid domain (grid origin x = -2556000 m, y = -1728000 m; N columns = 459, N rows = 299).

* [Link to grid mask files on CMAS Data Warehouse Google Drive](https://drive.google.com/drive/folders/1x9mJUbKjJaMDFawgy2PUbETwEUopAQDl)
* [Link to metadata for the grid mask files is posted on the CMAS Center Dataverse site](https://doi.org/10.15139/S3/XDYYB9)

Custom mask files may also be made using the [shp2cmaq](../../../PYTOOLS/shp2cmaq/README.md) tool, which provides instructions for obtaining geospatial data via shape files and converting them to CMAQ gridded input files. One may also populate a CMAQ gridded input file with arbitrary geometric shapes (e.g. squares, diamonds, or other polygons) using the IOAPI library of tools and any common coding language (e.g. Fortran, R, or Python)

<a id=define_families></a>
### 12.  Define families of streams, regions, or chemical species
Users can define any number of custom groups or "families" of emission streams, regions or chemical species to be used to streamline (i.e. enhance) prescribed emissions rules. For example, if a user would like to scale NOx by 50% from 4 different emission streams (e.g. PT_EGU, GRIDDED, MOBILE and PT_NONEGU) without using famlies, they would need 8 rules, one for NO and NO2 for each of 4 streams. However, by defining a family of 4 streams and another family of two chemical species (i.e. NOx), 1 rule can be used to achieve the same result.  

Chemical families are defined by prescribing, via the [CMAQ Miscellaneous Control File](../../../CCTM/src/util/util/CMAQ_Control_Misc.nml), the total number of chemical families to be used, the name of each, the number of members of each family, and the name of each family member. For example,  
```
&Chemical_FamVars
 N_Chem_Fams = 2
 Max_Chem_Fam_Members = 20
/

&ChemicalFamilies
 ChemFamilyName(1)     = 'NOX'    
 ChemFamilyMembers(1,:)= 'NO','NO2'  
 ChemFamilyName(2)     = 'POA'    
 ChemFamilyMembers(2,:)= 'POC','PNCOM'  
/
```  
In this example, 2 chemical families, "NOX" and "POA", are defined with 2 members, "NO" and "NO2", and "POC" and "PNCOM". Note that CMAQv5.3 required the variable ChemFamilyNum to be specified and this value is internally calculated in CMAQv5.4. If the variable is provided, the model will crash. Also, it is required to ensure that no Chemical Family Name is identical to any emission species or CMAQ species. Currently, CMAQ will not detect a name conflict but results will be compromised. A future version of CMAQ will check for duplicative names, trigger an error, and stop the model.

Stream families are defined analogously in the DESID Control File (CMAQ_Control_DESID.nml):  
```
&Desid_StreamFamVars
 Desid_N_Stream_Fams = 3
 Desid_Max_Stream_Fam_Members = 20
/

&Desid_StreamFam  
 ! General sources of POA
 StreamFamilyName(1)     = 'GENERAL_POA'
 StreamFamilyMembers(1,1:5)= 'GRIDDED_OTHER','GRIDDED_SOLVENTS','PT_CANADA_SOLVENTS','PT_OTHER','PT_US_SOLVENTS'

 ! Diesel-Like Sources
 StreamFamilyName(2)     = 'DIESEL'
 StreamFamilyMembers(2,1:2)= 'GRIDDED_DIESEL','PT_CMV'

 ! US and Canada Aircraft
 StreamFamilyName(3)     = 'AIRCRAFT'
 StreamFamilyMembers(3,1:2)= 'GRIDDED_US_AIRCRAFT','PT_CANADA_AIRPORTS'
/  
```

As are region families:  
```
&Desid_RegionDefVars
 Desid_Max_Reg    = 60           
 Desid_N_Reg_Fams = 0           
 Desid_Max_Reg_Fam_Members = 100 
/

&Desid_RegionFam
 RegionFamilyName(1)     = 'SouthEastUS'
 RegionFamilyMembers(1,:)= 'KY','VA','TN','NC','MS','AL','GA','SC','FL'
/
```

<a id=fam_stream></a>
### 13. Use a family of streams to scale emissions for a group of sources
To then use a stream family to apply a rule to multiple streams, just use the family name in the Stream Label column. 
```
! Region      | Stream Label         |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |                      |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'DIESEL'             ,'NO2'    ,'NO2'         ,'GAS' ,0.50 ,'UNIT','m',
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
'EVERYWHERE'  , 'AIRCRAFT'   ,'ALL'    ,'NOX'         ,'GAS' ,0.50 ,'UNIT','m',
```  
or in the Emission Surrogate column:  
```
! Region      | Stream Label |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |              |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'AIRCRAFT'   ,'NOX'    ,'ALL'         ,'GAS' ,0.50 ,'UNIT','m',
```  
In both of these cases, both NO and NO2 (as NOx is defined above) are multiplied by 50%. The same is accomplished by using NOX in both columns.  
```
! Region      | Stream Label |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |              |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'AIRCRAFT'   ,'NOX'    ,'NOX'         ,'GAS' ,0.50 ,'UNIT','m',
```  
Because the 'm' operator is used, CMAQ will look for pre-existing relationships between the members of 'NOX' in order to apply the scaling rule, which 'multiplies' the existing scaling by 50%. So this example assumes that the following two rules, or something similar, preceed the instructions in this section:  
```
! Region      | Stream Label |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |              |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'AIRCRAFT'   ,'NO'     ,'NO'          ,'GAS' ,1.00  ,'UNIT','a',
'EVERYWHERE'  , 'AIRCRAFT'   ,'NO2'    ,'NO2'         ,'GAS' ,1.00  ,'UNIT','a',
```  
In this case, CMAQ is adding a relationship between NO and NO2 surrogates and model species. Thus families are most useful when using the 'm' or 'o' operators. 

However, sometimes the 'a' operator is useful with chemical families. In the example below, a relationship is added between POA surrogates (defined in section 12 above) and CMAQ model species:  
```
! Region      | Stream Label |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |              |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'AIRCRAFT'   ,'POA'    ,'ALVPO1'      ,'FINE',0.09  ,'UNIT','a',
```  
CMAQ will use this rule to add POC and PNCOM surrogates together, multiply by 0.09 and assign their emissions to ALVPO1, a semivolatile POA species.  
The way CMAQ uses chemical families for adding relationships with the 'a' is nuanced. The following logic is applied: 
- If a chemical family is used for either the emission variable or the CMAQ-Species but not both, then connections are made between each member of the family and the prescribed single-species in the other column.  
- If both columns include chemical families or the 'ALL' keyword, then each pair of members will be compared. If the names match exactly or a relationship already exists, then the 'a' operation will be applied. If not, then the pair will be ignored. This precaution is in place to protect against the case where a user prescribes an addition (i.e. 'a') rule with the keyword 'ALL' or very large chemical families in both the emission variable and CMAQ-Species columns. 
Without the precaution in place, adding relationships for ALL surrogates to ALL model species would be an extremely large data structure and almost certainly not an intended use of CMAQ.

<a id=misc_notes></a>
### 16. Miscellaneous Notes
In the default emissions mapping configuration, sulfuric acid (SULF) mass is mapped to ASO4 (particulate sulfate). If these emissions are perturbed directly or as part of a broader sector- or region-wide scaling, it is recommended to confirm specifically that these emissions have been scaled as desired. For example, if a family named 'SOX' is defined that includes 'SO2' and 'SULF' and then 'SOX' is specified as the CMAQ species in a scaling rule, then the 'SULF' to 'ASO4' mapping would not be detected.

<!-- START_OF_COMMENT -->
[link_emtut_1]: ../../../CCTM/src/emis/emis/CMAQ_Control_DESID.nml
[link_emtut_2]: ../../../CCTM/src/MECHS/cracmm2/CMAQ_Control_DESID_cracmm2.nml
[link_emtut_3]: ../../../CCTM/scripts/run_cctm_cracmm_2019_12US1_CRACMM2_EPA2019.csh#L420
[link_emtut_4]: ../../../CCTM/src/emis/emis/CMAQ_Control_DESID.nml#L137
[link_emtut_5]: ../../../PREP/shp2cmaq/README.md
[link_emtut_6]: ../../../CCTM/src/util/util/CMAQ_Control_Misc.nml

<!-- END_OF_COMMENT -->

[link_emtut_1]: https://github.com/USEPA/CMAQ/blob/main/CCTM/src/emis/emis/CMAQ_Control_DESID.nml
[link_emtut_2]: https://github.com/USEPA/CMAQ/blob/main/CCTM/src/MECHS/cracmm2/CMAQ_Control_DESID_cracmm2.nml
[link_emtut_3]: https://github.com/USEPA/CMAQ/blob/main/CCTM/scripts/run_cctm_cracmm_2019_12US1_CRACMM2_EPA2019.csh#L420
[link_emtut_4]: https://github.com/USEPA/CMAQ/blob/main/CCTM/src/emis/emis/CMAQ_Control_DESID.nml#L137
[link_emtut_5]: https://github.com/USEPA/CMAQ/blob/main/PREP/shp2cmaq/README.md
[link_emtut_6]: https://github.com/USEPA/CMAQ/blob/main/CCTM/src/util/util/CMAQ_Control_Misc.nml
