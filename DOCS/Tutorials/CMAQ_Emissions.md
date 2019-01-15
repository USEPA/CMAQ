## CMAQ Tutorial ##
### Prescribing Emissions Using the DESID, the Detailed Emissions Scaling, Isolation and Diagnostics Module ###
Purpose: This tutorial will guide users to utilizing the Emission Control namelist to perform some basic manipulation
of their emission streams. For additional questions, contact Ben Murphy (murphy.ben@epa.gov).

------------

### Definitions of Terms
- Stream: an Online emission source or a group of sources processed offline and read into CMAQ from one file. Common
examples of streams include biogenic VOCs, mobile sources, winf-blown dust, prescribed fires, electric-generating units,
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


<a id=zero_out></a>
### 1. Zero Out Emissions
Emission streams can be zeroed using the options for individual streams in the CMAQ RunScript or creating rules in the Emission Control Namelist.

##### a. Using Options in the CMAQ RunScript
For gridded or inline emissions, just reduce the value of N_EMIS_GR or N_EMIS_PT, respectively and adjust the values of the filepaths and stream labels accordingly, if necessary.

To zero Sea Spray aerosol emissions,
```
setenv CTM_SS_AERO N
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
The last rule assumes that one of the emissions streams has been labeled 'MOBILE' in the RunScript. Fo example,
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
Note that the 'a' operator is important here to create the new link from 'CHEMX' to NO. If we want to scale to 25% of total NOx concentrations, we can add a line to include NO2.
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
will add 15% of CO emissions to the emissions of fine-mode particulate nitrate, but the scale factor will also be adjusted by multiplying by the molecular weight of CO in order to conserve mass.

Reminder: gas-phase emission rates are usually provided to CMAQ in molar units while particle emissions are usually provided in mass. Note that if the user scales a particle species to a gas surrogate, or vice-versa, it is important in most cases to choose "MOLE" or "MASS" for the Basis in order to ensure proper unit conversions.

<a id=apply_mask></a>
### 11. Apply scaling with spatial dependence
The user may apply a scale factor to a specific area of the domain by identifying the name of the mask to be used in the emission rule. For example, this rule increases all emisisons in "KENTUCKY" by 50%:
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
This is just an example of defining one mask named "KETUCKY". For a complete explanation of the spatial-dependent scaling feature, see the CMAQ User Guide (section 4.3.2 "Applying Masks").
