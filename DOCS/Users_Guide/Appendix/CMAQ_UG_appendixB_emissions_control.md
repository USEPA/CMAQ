<!-- BEGIN COMMENT -->

[<< Previous Appendix](CMAQ_UG_appendixA_model_options.md) - [Home](../README.md) - [Next Appendix >>](CMAQ_UG_appendixC_spatial_data.md)

<!-- END COMMENT -->

* * *

# Appendix B: Emissions Input and Control
[Jump to DESID Tutorial](../Tutorials/CMAQ_UG_tutorial_emissions.md) for step by step instructions on performing some basic manipulation of emission streams.

[Jump to Emissions overview](../CMAQ_UG_ch06_model_configuration_options.md) in Chapter 6 of this User's Guide.

## B.1 Emissions Control with the Detailed Emissions Scaling, Isolation and Diagnostics Module (DESID)

The Detailed Emissions Scaling, Isolation and Diagnostics (DESID) module included with CMAQv5.3+ provides comprehensive customization and transparency of emissions manipulation to the user. The customization of emissions is accomplished via a series of Control Namelists, which contain variables that modify the behavior of the emissions module. These include ***Emission Scaling Rules***, ***Size Distributions***, ***Regions Registry***, ***Chemical Families***, ***Region Families***, and ***Area Adjustments***.

To determine its configuration, DESID makes use of input primarily from four files: the CMAQ runscript, the CMAQ Miscellaneous Control File ([CMAQ_Control_Misc.nml][link_B.1_misc]), the DESID Control file ([CMAQ_Control_DESID.nml][link_B.1_desis]), and the DESID Chemical Mapping File (e.g. [CMAQ_Control_DESID_cb6r5_ae7_aq.nml][link_B.1_desid]). 
A separate version of the chemical mapping control file exists for every mechanism because these namelists are preloaded with likely rules linking emissions of important CMAQ primary species to their typical emission species names as output by SMOKE. 
By default, this namelist is stored in each chemical mechanism folder (e.g. MECHS/cb6r5_ae7_aq) and is copied into the user's build directory when bldit_cctm.csh is executed and a chemical mechanism is chosen. If the user modifies the name or location of the DESID control file or chemical mapping file, then the following commands in the RunScript should be updated as well:
```
setenv DESID_CTRL_NML ${BLD}/CMAQ_Control_DESID.nml
setenv DESID_CHEM_CTRL_NML ${BLD}/CMAQ_Control_DESID_${MECH}.nml
```

If the user does not provide a DESID Control Files or the path to the files in the RunScript are incorrect, then the model will abort and indicate the error. If the user would like all emissions set to 0, it is recommended that they use the syntax outlined here and in the DESID tutorial to do so.   


## B.2 Chemical Mapping Control
The chemical mapping file contains emission scaling rules that allow the user to exert sophisticated, precise control over the emissions from specific streams, in specific geographic areas, and/or for specific compounds. 
The set of rules used by CMAQ to interpret emissions shall be provided in one array called DESID_Rules_nml. It is necessary that every field (i.e. column) be populated for every rule. The fields are given and defined here:
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
```
- 'Region Label' - Apply scaling for specific regions of the domain. Set this field to "EVERYWHERE" to apply the rule to the entire domain.
- 'Stream Label' - Short Name from Run Script (e.g. the value of GR_EMIS_01_LAB or STK_EMIS_01_LAB). There are a few reserved names that apply to online emissions streams. These are:
  - BIOG - Biogenic VOC emissions computed by BEIS
  - MIOG - Biogenic VOC emissions computed by MEGAN
  - MGEM - Marine Gas Emissions
  - LTNG - Lightning NO Emissions
  - WBDUST - Wind-Blown Dust Emissions
  - SeaSpray - Sea Spray Aerosol Emissions  

  Set this field to 'ALL' to apply the rule to all emission streams.  
- 'Emission Species' - The character string identifying the species on the emission file or in the online calculation that the CMAQ species should be mapped to. 
For gases, usually this name is the same as the CMAQ species. For aerosols, it is usually slightly different (e.g ANO3 vs. PNO3). Set this field to 'ALL' to apply the rule to all emission species.  
- 'CMAQ-Species' - Internal Species Name. Set this field to 'ALL' to apply the rule to all CMAQ internal species.
- 'Phase/Mode' - If the CMAQ-Species is a Gas, this field should equal 'Gas'. If the CMAQ-Species is an aerosol, this field should indicate one of the possible emission aerosol modes. Every stream by default is given a 'COARSE' and 'FINE' mode. The user may refer to these or define others above and refer to them as well. This level of specificity is needed so that aerosol number and surface area are calculated correctly, and so that any unit conversions between gases and aerosols can be handled correctly.  
- 'Scale Factor' - Numerical adjustment factor to be applied to the mapping.
- 'Basis' - Specifies whether the scaling option should apply with consideration of mass/mole conversions, or if the operation should ignore the units of the incoming variable and target CMAQ species. This parameter is ignored for multiply rules (i.e. the 'm' operator is specified) because unit conversion will have already been considered in the preceding add ('a') and/or overwrite ('o') rules. CMAQ includes a lookup table of molecular weights for known emission species (in desid_vars.F) and can use these to translate molar and mass emission rates from the input file to rates in units corresponding to the CMAQ internal species. CMAQ determines the units of the emission species by reading the file header (i.e. it is important the units are accurate). Options for input are:
  - 'MASS' - Conserve Mass. For example, if emissions of an aerosol are to be scaled to emissions of a gas species, it is common to want to conserve mass.
  - 'MOLE' - Conserve Moles. For example, if emissions of a gas-phase species are to be scaled to another gas, it is sometimes desired to conserve moles since gas emissions are provided on a mole basis.
  - 'UNIT' - Ignore molecular weight conversions and apply emission rate directly regardless of units.
  - Example 1: Particle-phase variable Y (units in g/s) on the emission file is mapped to CMAQ gas-phase species X with a scale factor of B. Emissions for X are needed in mol/s.
    - If Basis is set to UNIT, then EMIS(X) = EMIS(Y) * B
    - If Basis is set to MASS, then EMIS(X) = EMIS(Y) * B * MW(X) 
    - If Basis is set to MOLE, then EMIS(X) = EMIS(Y) * B / MW(Y) 
  - Example 2: Gas-phase variable Y (units in mol/s) on the emission file is mapped to CMAQ gas-phase species X with a scale factor of B. Emissions for X are needed in mol/s.
    - If Basis is set to UNIT, then EMIS(X) = EMIS(Y) * B
    - If Basis is set to MASS, then EMIS(X) = EMIS(Y) * B * MW(Y) / MW(X) 
    - If Basis is set to MOLE, then EMIS(X) = EMIS(Y) * B 
  - Example 3: Particle-phase variable Y (units in g/s) on the emission file is mapped to CMAQ particle-phase species X with a scale factor of B. Emissions for X are needed in g/s.
    - If Basis is set to UNIT, then EMIS(X) = EMIS(Y) * B 
    - If Basis is set to MASS, then EMIS(X) = EMIS(Y) * B
    - If Basis is set to MOLE, then EMIS(X) = EMIS(Y) * B / MW(Y) * MW(X)

- 'Operation' - Specifies the kind of rule to be carried out. Options are:
  - 'a' - add the rule to existing instructions. This operation should be used for new entries, too.
  - 'm' - find existing scaling instructions matching this rule's features (ie. species, streams, etc) and multiply them by the factor in this particular rule. This operator ignores the 'BASIS' input parameter regardless of its value (UNIT, MASS or MOLES).  
  - 'o' - find existing scaling instructions matching this rule and overwrite them.  

### B.2.1 Default Rules
The Chemical Mapping Control Namelists provided with the CMAQ repo have default rules included that correspond to each chemical mechanism. Here is an example default rule that links NO in CMAQ to NO from every emission stream in every model grid cell with a scale factor of 1.0.
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Species  | Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'All'         ,'NO'     ,'NO'          ,'GAS' ,1.0  ,'UNIT','a',
```
Many rules are needed in order to properly link every emitted pollutant to a CMAQ species. Rules are needed for gas- and aerosol-phase species. Additional rules also exist for online aerosol modules like wind-blown dust and sea spray because the names of aerosol emission species from these modules are different than those typically used for SMOKE output. For example, fine-mode aerosol sulfate is commonly called PSO4 in SMOKE, but is PMFINE_SO4 from dust and sea spray.

### B.2.2 Modifying Default rules
The user can modify any default rule to change the scale factor applied, the spatial area to be considered, or the streams to be applied to. Alternatively, the user can add new rules after the default rules to customize the emissions. Typical modifications may include multiplying the emissions of a particular species from a particular stream by a factor of 2, zeroing out emissions of all species from a particular stream, etc. Please see the tutorial on [Prescribing Emissions with DESID](../Tutorials/CMAQ_UG_tutorial_emissions.md) for specific examples of modifications and the syntax used to invoke them.

#### B.2.2.1 Supporting the Volatility Basis Set
The *Volatility Basis Set* for treating the semivolatile partitioning of primary organic emissions is an example of a model feature that is well-supported by DESID. The approach involves distributing the emissions of total primary organic aerosol (carbon and noncarbon mass, or POC and PNCOM) among a series of aerosol and gas species of varying volatility.

If the user would like to invoke the nonvolatile partitioning assumption, it can be accomplished by directing all POC and PNCOM emissions to the POC and PNCOM species in CMAQ.
```
  ! --> Nonvolatile POA
  'EVERYWHERE', 'ALL'         ,'POC'    ,'APOC'        ,'FINE',1.   ,'MASS','a',
  'EVERYWHERE', 'ALL'         ,'PNCOM'  ,'APNCOM'      ,'FINE',1.   ,'MASS','a',
```
If the user would like to apply a uniform volatility distribution to the POA emissions, it can be accomplished with the following rules.
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
Notice that for each species (e.g. ALVPO1) a rule is needed to link the species to the emissions of POC and another rule is needed to add PNCOM. This is because current mechanisms both carbon and noncarbon mass are part of the emissions of every semivolatile species. To change the volatility distribution for all streams, the user may modify the scaling factors in the default rules above. To introduce specialized volatility distributions for specific stream (e.g. residential wood burning, forest fires, diesel vehicles, etc), rules may be added which explicitly identify a stream in the "Stream Label" field.
Alternatively, emitted species may be added on the offline file that correspond to individual emission sectors, and sector-specific volatility distributions may then be applied to those species.  

To avoid large swings in repartitioning after emission, it is a good idea to split mass between gas and particle phases, with all mass going to the particle in the C* = 0.1-1 range and all gas for C* = 100-1000. Species with C*=10 can generally have mass split between gas and particle or be put in all gas if conditions are very clean. If too much mass evaporates or condenses upon emission, the aerosol size distribution will be affected.  

#### B.2.2.2 Supporting Potential Combustion SOA

Potential Combustion SOA (pcSOA) is a CMAQ species introduced to account for missing pathways for SOA formation from combustion sources. It includes IVOC oxidation as well as other phenomena (Murphy et al., ACP, 2017). It was parameterized primarily in LA, where vehicle exhaust continues to dominate. The following emission rulese add the gas-phase precursor to pcSOA to the model scaled to POA.
    'EVERYWHERE', 'ALL'          ,'POC'   ,'PCVOC'      ,'GAS' ,6.579,'MASS','a',  
    'EVERYWHERE', 'ALL'          ,'PNCOM' ,'PCVOC'      ,'GAS' ,6.579,'MASS','a',  

However, the added pcSOA is probably inappropriate for Fire sources, especially in its current configuration. This pathway should be zeroed out for all fire and wood-burning related sources. The default emission control interfaces include a number of emission rules with the most common stream names for fire emission inputs in order to maximize the likelihood of zeroing out pcSOA from fires. The user should confirm that pcVOC emissions from fire sources are zero for all simulations.  
    'EVERYWHERE', 'PT_FIRES'     ,'ALL'   ,'PCVOC'      ,'GAS' ,0.0  ,'MASS','o',
    'EVERYWHERE', 'PT_RXFIRES'   ,'ALL'   ,'PCVOC'      ,'GAS' ,0.0  ,'MASS','o',
    'EVERYWHERE', 'PT_AGFIRES'   ,'ALL'   ,'PCVOC'      ,'GAS' ,0.0  ,'MASS','o',
    'EVERYWHERE', 'PT_OTHFIRES'  ,'ALL'   ,'PCVOC'      ,'GAS' ,0.0  ,'MASS','o',
    'EVERYWHERE', 'PT_FIRES_MXCA','ALL'   ,'PCVOC'      ,'GAS' ,0.0  ,'MASS','o',
    'EVERYWHERE', 'GR_RES_FIRES' ,'ALL'   ,'PCVOC'      ,'GAS' ,0.0  ,'MASS','o',

The CRACMM mechanism (introduced in CMAQv5.4) does not use PCSOA or PCVOC because it provides a mechanistic estimate of OA production. If those species are present, they should be removed. The only exception is if they are coming from the boundary conditions (i.e. if boundary conditions are being supplied by a non-CRACMM model run).  

## B.3 DESID Control
The DESID control file (CMAQ_Control_DESID.nml) provides chemical mechanism-independent user inputs to DESID. Important variable sections are the General Options, Area Normalization, Size Distribution Parameters, Region Definitions, and Diagnostic Output Configuration.  

### B.3.1 General Options
The maximum number of DEISD layers can be set with
```
&Desid_Options
 Desid_MaxLays = 0  
/
```
This variable can limit the impact of vertical mixing (plume-rise) algorithms in CMAQ. If Desid_MaxLays = 0, then all model layers are available for emissions.


### B.3.2 Area Normalization and Projection Adjustment
Specify area-normalized emission fluxes from input streams. If emission inputs are provided to CMAQ in area-normalized fluxes for any stream, this component may be used to convert them correctly to emission rates appropriate for the model simulation grid projection. 
For example, if biogenic emissions are provided in units of moles/m2/s, then that  stream may be identified here and DESID can be instructed to adjust each flux to moles/s and adjust the rates using the map scale factor that converts emissions in real geographic space into projected grid space.  

Definition of Fields:                                                      
      'Stream Label' - Declare a stream. If the label is 'ALL', then the instruction will be expanded to all OFFLINE streams.  
      'Area Normalization' - If TRUE, then this stream's emissions will be multiplied by grid cell area. If AUTO, then DESID will deduce from the units string whether or not each variable on the stream input file should be multiplied by the area scale factor.  
      'Projection Adjustment' - If TRUE, then this stream's emissions will be multiplied by the area Map Scale Factor. If AUTO, then DESID  will inherit the per-stream value from Area Normalization. If Area Normalization is TRUE, the Projection Adjustment is True.   

Example:
```
&AreaNorm
 AREA_NML  =
 !         | Stream Label   | Area Normalization | Projection Adjustment
                'ALL'       ,'AUTO'              ,'AUTO',
                'GRIDDED'   ,'TRUE'              ,'TRUE',
                'FIRES'     ,'TRUE'              ,'FALSE',
                'VCP'       ,'FALSE'             ,'TRUE',
/
```
If emissions have been prepared by SMOKE or a similar emissions processing tool that computes emissions on a model grid, it is not expected that an area normalization or projection adjustment will be needed. 
This feature was developed primarily for applications where an offline emission model produces area-normalized emission fluxes, not emission rates.  

### B.3.3 Aerosol Size Distributions  
The treatment of aerosol size distributions in CMAQv5.3 has been updated to be more consistent with the way particle sizes and modes are treated by the National Emission Inventory and in emissions processing tools like SMOKE, MOVES, SPECIATE, and Speciation Tool. Specifically, in these tools, aerosol emissions are typically parameterized into two main modes, Fine and Coarse. Although the size distribution parameters (i.e. total number, diameter, standard deviation, etc.) for these modes will vary among emission sources, previous versions of CMAQ assumed that all primary fine particles had the same size distribution upon emission. Coarse-mode particles were assumed to exhibit a larger diameter but were also uniform across all sources (excluding wind-blown dust and sea spray).

In CMAQv5.3 and beyond, users link particle emission species to CMAQ particle species via the DESID_Scaling section of the Chemical Mapping Control Namelist. Examples of default mapping rules can be found in any of the Chemical Mapping Control Namelists in the CMAQ repository. The three lines below assign emissions for all streams for particulate-phase sulfate, ammonium, and nitrate.
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Species  | Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'ALL'         ,'PSO4'   ,'ASO4'        ,'FINE',1.0   ,'UNIT','a',
'EVERYWHERE'  , 'ALL'         ,'PNH4'   ,'ANH4'        ,'FINE',1.0   ,'UNIT','a',
'EVERYWHERE'  , 'ALL'         ,'PNO3'   ,'ANO3'        ,'FINE',1.0   ,'UNIT','a',
```
The CMAQ-Species field should be populated with bulk chemical names (e.g. ASO4, AEC, AK, ACA, etc). In other words, the 'i','j', or 'k' which usually designates the mode of the aerosol species name should be omitted. A list of the valid aerosol bulknames exists in the source file "[AERO_DATA.F][link_B.3_aero]" in the array named "aerolist". The user should also identify the aerosol mode to be populated using the "Phase/Mode" field. In the example above, all of the rules identify the "FINE" mode as the destination mode. CMAQ uses this value to look up the size distribution parameters (diameter and standard deviation) to apply for this particular emission.

Aerosol mode keywords from the DESID_Scaling section are linked to reference mode labels in the Desid_SizeDist section of the DESID Control Namelist. These assignments can be made for all streams at once, as demonstrated by the first two default entries initializing the 'FINE' and 'COARSE' modes, or they can be made on a stream-by-stream basis as shown below for Wind-Blown Dust and Sea Spray aerosol.
```
&Desid_SizeDist
 Desid_Sd_nml    =
 !         | Stream Label   | Emiss. Mode  | Ref. Mode
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
The 'Ref. Mode Labels' are used to lookup size distribution parameters in [AERO_DATA.F][link_B.3_aero]. The following reference modes are defined in this file:
```
TYPE em_aero
    Character( 20 ) :: name
    Real            :: split( n_mode )  ! dimensionless
    Real            :: dgvem( n_mode )  ! meters
    Real            :: sgem ( n_mode )  ! dimensionless
END TYPE em_aero
INTEGER, PARAMETER  :: desid_n_aero_ref = 9

TYPE( em_aero ), Parameter :: desid_aero_ref( desid_n_aero_ref ) = (/

!          ----Name----     -----Split-----    ---Geo. Mean Diameter---   ---Stnd Dev.---
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
Users can add as many new size distributions as they want, as long as they increment the variable n_em_aero_ref to always equal the number of size distributions in the lookup array (desid_aero_ref).

CMAQ will use the size distribution reference value linked to each emissions scaling rule via the phase/mode keyword to calculate the fraction of each aerosol primary emission that should go into the 'i', 'j', and 'k' modes in the internal aerosol module. 
At first, it may seem that the linking step between phase/mode keywords in the DESID_Scaling section, the corresponding mode keywords in the Desid_SizeDist section, and the reference mode labels is redundant, but it serves an important function. As stated earlier, it is common that modes of similar size from a variety of sources will be referred to by common names like 'FINE' and 'COARSE', even though the size distribution parameters may differ considerably. 
With the linking step provided in the DESID_SizeDist section, parameters for several streams can be specified individually, but all be labeled 'FINE' and applied with one rule in the DESID_Scaling section.

In the example above, fine mode Wind-Blown Dust are linked to 'FINE_WBDUST', sea spray aerosols are linked to 'FINE_SEASPRAY' and all other sources are linked to 'FINE_REF'. Thus, different size distributions will be calculated for each of these streams. However, if the user wants to scale the mass of all fine mode aerosol by a factor of 2, the following emission rule is valid:
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Species  | Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'ALL'         ,'ALL'    ,'ALL'         ,'FINE',1.0   ,'UNIT','m',
```
 
### B.3.4 Defining and Using Regions and Region Families
#### B.3.4.1 Using Defined Regions
Gridded masks are used to apply rules to specific areas of the domain. For example, the following rule:
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Species  | Species      |Mode  |Factor|      |
'KENTUCKY'    , 'All'         ,'All'    ,'All'         ,'All' ,1.50 ,'UNIT','m',
```
will scale emissions of all species from all streams by +50% but only in grid cells in the state of Kentucky. One or more I/O API formatted input files containing geographic region definitions are required to take advantage of this option.  Such files should contain a separate variable for each spatial region of interest.  Each variable is a gridded field of real numbers from 0.0 to 1.0, with 0.0 outside of the region of interest and 1.0 completely inside the region. Region border grid cells should have the geographic fraction attributed to the region (for example, a grid cell that 35% in Kentucky and 65% in Tennessee would have have the number 0.35 for the variable representing the Kentucky mask.

#### B.3.4.2 Defining Regions  

The Desid_RegionDef section of the DESID Control Namelist maps each "Region Label" to specific variables on specific files. Here is the Desid_RegionDef section in the default namelist:
```
&Desid_RegionDef
 Desid_Reg_nml  =   
 !          | Region Label   | File_Label    | Variable on File
 !<Default>    'EVERYWHERE'  ,'N/A'          ,'N/A',
               'WATER'       ,'CMAQ_MASKS'   ,'OPEN',
/
```
As indicated, the Region Label "EVERYWHERE" is active by default and returns a mask that operates uniformly across the entire domain. 
The "File_Label" field identifies the environment variable in the RunScript that stores the location and name of the file containing the mask. 
The user may modify this to any name they wish as long as it is consistent with the variable name on the RunScript. 
The "Variable on File" field identifies the variable on the input file that stores the gridded field to be used for this region. 
Examples are provided for two cases. 
The variable Desid_Max_Reg in the Desid_RegionDefVars section must be greater than the number of regions that will be defined.

In this case, a region with label "WATER" is defined and referenced to the variable "OPEN" (which is short for *open water*) in the file 'CMAQ_MASKS' which needs to be defined in the RunScript. Using this "WATER" region will apply a scaling rule only for open water grid cells and fractionally along coastlines.  

As an additional example, let's assume file us_states.nc is defined in the runscript as US_STATES as follows:
```
setenv US_STATES /home/${CMAQ_HOME}/CCTM/scripts/us_states.nc
```
and contains two variables called NC and SC, representing the fraction of each grid cell that is located in North Carolina and South Carolina, respectively." These two variables in the file can be assigned to region labels NC and SC using either of the following methods:
```
&Desid_RegionDef
 Desid_Reg_nml  =   
 !          | Region Label   | File_Label    | Variable on File
 !<Default>    'EVERYWHERE'  ,'N/A'          ,'N/A',
               'NC'          ,'US_STATES'    ,'NC',
               'SC'          ,'US_STATES'    ,'SC',
/
```

Alternatively, all the variables on the US_STATES file may be enabled at once:
```
&Desid_RegionDef
 Desid_Reg_nml  =   
 !          | Region Label   | File_Label    | Variable on File
 !<Default>    'EVERYWHERE'  ,'N/A'          ,'N/A',
               'ALL'         ,'US_STATES' ,'ALL',
/
```
Rather than listing out all variables on the file and explicitly linking them to "Region Labels", the user can invoke the "ALL" keyword in both the 'Region Label' and 'Variable on File' fields and all variables will be read and stored. Once either of these definitions are included in the &Desid_RegionDef section, region labels NC and SC can be used in emission scaling instructions as in the Kentucky example above.  

These gridded mask files are read by CMAQ through environmental variables, which are identified in the RunScript. If variables from multiple mask files are used, each of these mask files needs to be defined in the RunScript. Two example mask files are available on the CMAS Data Warehouse: US states grid mask file and NOAA climate regions grid mask file.  These mask files can be used with the 12US1 modeling grid domain (grid origin x = -2556000 m, y = -1728000 m; N columns = 459, N rows = 299).

* [Link to grid mask files on CMAS Data Warehouse Google Drive](https://drive.google.com/drive/folders/1x9mJUbKjJaMDFawgy2PUbETwEUopAQDl)
* [Link to metadata for the grid mask files is posted on the CMAS Center Dataverse site](https://doi.org/10.15139/S3/XDYYB9)

Custom mask files may also be made using the [shp2cmaq][link_B.3] tool, which provides instructions for obtaining geospatial data via shape files and converting them to CMAQ gridded input files. One may also populate a CMAQ gridded input file with arbitrary geometric shapes (e.g. squares, diamonds, or other polygons) using the IOAPI library of tools and any common coding language (e.g. Fortran, R, or Python).

#### B.3.4.3 Region Families
Users can define families of regions to reduce the number emission rules needed to operate on a group of regions. 
For example, a user could group regions defined in the RegionsRegistry together. For example, if there are already regions that represent North Carolina (let's call it NC) and South Carolina (let's call it SC), then the user could group NC and SC together and call them "CAROLINAS". Then when "CAROLINAS" is used as the region in a scaling rule, the rule will be distributed and applied to grid cells in both NC and SC. 
The Desid_RegionFam section contains variables for this purpose.
```
&Desid_RegionFam
  ! Region Family Definitions
  RegionFamiliyName(1)        = 'SOUTH'
  RegionFamilyMembers(1,:)    = 'TEXAS','LOUISIANA','MISSISSIPPI','ALABAMA'

  RegionFamiliyName(2)        = 'WEST'
  RegionFamilyMembers(2,:)    = 'CALIFORNIA','OREGON'
/
```
The variable Desid_N_Reg_Fams in the Desid_RegionDefVars section must equal the number of region families the user would like to read. 
The variable Desid_Max_Reg_Fam_Members must be greater than the number of regions on any list of region family members. 


### B.3.5 Stream Families  
The DESID Control File provides an interface for defining stream families that can be used to dramatically simplify the rules a user wishes to apply to their emissions inputs. 
This could be especially useful when defining, for example, a group emission streams relevant for electric power generation or mobile sources.  
Example 1 (set Desid_N_Steam_Fams=1 in Desid_StreamFamVars):
```
&Desid_StreamFam
 ! For emission streams available in several run scripts under CCTM/scripts
 StreamFamilyName(1)     = 'PT_SOURCES'
 StreamFamilyMembers(1,1:3)= 'PT_NONEGU','PT_EGU','PT_OTHER'
/
```
Example 2 (set Desid_N_Steam_Fams=3 in Desid_StreamFamVars):  
```
&Desid_StreamFam
 ! For emission streams unique in CCTM/src/run_cctm_cracmm_4LISTOS1.csh script
 ! Sources that do not have a source-specific POA treatment
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
The variable Desid_Max_Stream_Fam_Members should be set higher than the maximum number of stream members for any stream family.

### B.3.6 Chemical Families
Chemical families are defined analogously to stream and region families but in the CMAQ Miscellaneous Control file. This is because they are useful to modules beyond DESID, including [ELMO](CMAQ_UG_appendixF_elmo_output.md) and the [Budget Tool](../CMAQ_UG_ch09_process_analysis.md). 
See the [Miscellaneous Control File Description](../CMAQ_UG_ch04_model_inputs.md#miscctrl) in Chapter 4 for details. 

One additional note: if a chemical familiy is defined for use in an emission scaling rule, the user should be careful about confirming that the members of that family are present on the emission input file or the CMAQ model species list, depending on which the user is trying to modify. Since the names on the input files are often different than those on the CMAQ model species list, care is advised. DESID will print warnings to the CMAQ log file when it cannot find species that it is looking for from a chemical family on an input file or in the list of CMAQ model species. Please confirm that the model is operating as you expect.  
 
## B.3.7 Emissions Diagnostics  
### B.3.7.1 Summary Output to Processor-Specific Logfiles  
Diagnostic output is an important feature of the new emissions module, DESID. Because the impact of emissions is so critical for CMAQ predictions and because the features available for scaling emissions are now quite complex, a comprehensive text-based output has been added to the CMAQ logfiles to enhance transparency.

The logfiles now provide several lists of information to support users from unexpected behaviors or conflicts like inconsistent naming between emissions and CMAQ speciation. First, CMAQ reports for each stream the number and names of all the emission species that were not used. Second, it prints the names of emission species that the user told it to look for but that it could not find on any of the emission streams. If the environment variable:
```
setenv CTM_EMISCHK Y         #> Abort CMAQ if missing emission species from emissions Input files
```
is set to 'Y' or 'True', then the model will abort if it cannot find any individual emission species. If the variable is set to 'N' or 'False' then CMAQ will print a warning and proceed.

Finally, CMAQ loops through streams and outputs the size distribution modes available for each stream and the full list of every emission instructions applied to each stream. These are ordered by CMAQ species (with 'i', 'j', and 'k' modes listed separately) and emission species name so that a full understanding of the scaling rules applied to each CMAQ species'' emissions can be grasped quickly. Columns are printed for the applicable region(s) of the grid, the phase/mode applied, the input scale factor, the scaling basis, the operation, and the final scale factor applied taking into account any molecular weight conversions, if needed, and size distribution fractions.

### B.3.7.2 Diagnostic Gridded Output Files
This component allows users to specify individual species for output on emissions diagnostic output files. In this way, users are able to probe emissions magnitudes and scaling changes for species of interest while not sacrificing the hard disk space needed to save the emission rates of all species for every emission stream. 
It is also possible to combine components of chemical families or stream families as the user''s interest dictates.   

The Desid_Diag section of the DESID Control File contains variables for configuring this diagnostic output. 
Users may specify any number of rules that, when processed, will result in one or more diagnostic files to be output. This example:
```
&Desid_Diag
   Desid_Diag_Streams_Nml(1,:)= 'ALL'
   Desid_Diag_Fmt_Nml(1)      = 'COLSUM'    ! Options: LAYER1, COLSUM, 3D
   Desid_Diag_Spec_Nml(1,:)   = 'NO','NO2','NOX','ASO4','CO'
/
```   
prints a diagnostic file for every offline and online stream separately. This is specified by the variable Desid_Diag_Streams_Nml. 
The Desid_Diag_Fmt_Nml variable indicates that the data are to be summed throughout each vertical column and only a surface should be written. 
Other options include just layer 1 data (LAYER1) and all vertical levels (3D). 
The Desid_Diag_Spec_Nml variable specifies the variables to include on each file. These may refer to CMAQ model species, chemical families, or bulk aerosol species names (all modes would be summed). 
The chemical species can not refer to variables on the emissions files themselves, unless they are equal to the CMAQ species name. 
If any stream does not include emissions for one or more of the species (e.g. windlown dust will not have NO or NO2), then they are omitted from that file automatically. 
For this example, the variable Desid_N_Diag_Rules in the Desid_DiagVars section should be set to 1. 

The keyword TOTAL may be used in place of ALL in the Streams variable to indicate a sum across all streams rather than an individual file for each. The '*' may also be prepended to any chemical or stream family to instruct DESID to break that family up into its members. For example:
```
&Desid_Diag
  EmissDiagStreams(1,:)= 'ALL'
  EmissDiagFmt(1)      = 'COLSUM'    ! Options: LAYER1, COLSUM, 3D
  EmissDiagSpec(1,:)   = 'NO','NO2','NOX','ASO4','CO'

  EmissDiagStreams(2,:)= 'TOTAL'
  EmissDiagFmt(2)      = '3D'
  EmissDiagSpec(2,:)   = 'AEC','AECI','NO2','ACLK','AMG','TERP'

  EmissDiagStreams(3,:)= 'PT_SOURCES'
  EmissDiagFmt(3)      = 'COLSUM'
  EmissDiagSpec(3,:)   = 'ALL'
/
```
We have already described the first example. The second exmaple will sum up all streams using the keyword with the '*' expansion and create one 3D gridded file with six variables: 'AEC' = AECI + AECJ + AECK; 'AECI'; 'NO2'; 'ACLK'; 'AMG' and 'TERP'. 
The third example will create a diagnostic of the sum of the components of the PT_SOURCES family (defined in the stream family section). This file will be column sums and will include all the emitted species appearing on at least one of the streams within PT_SOURCES. 
For this set of example, Desid_N_Diag_Rules in the Desid_DiagVars section should be set to 3. Desid_Max_Diag_Streams and Desid_Max_Diag_Spec should be greater than the maximum number of streams or species on any diagnostic rule list.  


<!-- BEGIN COMMENT -->

[<< Previous Appendix](CMAQ_UG_appendixA_model_options.md) - [Home](../README.md) - [Next Appendix >>](CMAQ_UG_appendixC_spatial_data.md)<br>
CMAQv5.5 User's Guide <br>

<!-- END COMMENT -->

<!-- START_OF_COMMENT -->  

[link_B.3]: ../../../PYTOOLS/shp2cmaq/
[link_B.1_misc]: ../../../CCTM/src/util/util/CMAQ_Control_Misc.nml
[link_B.1_desis]: ../../../CCTM/src/emis/emis/CMAQ_Control_DESID.nml
[link_B.1_desid]: ../../../CCTM/src/MECHS/cb6r5_ae7_aq/CMAQ_Control_DESID_cb6r5_ae7_aq.nml
[link_B.3_aero]:  ../../../CCTM/src/aero/aero6/AERO_DATA.F

<!-- END_OF_COMMENT -->

[link_B.3]: https://github.com/USEPA/CMAQ/blob/main/PYTOOLS/shp2cmaq/ 
[link_B.1_misc]: https://github.com/USEPA/CMAQ/blob/main/CCTM/src/util/util/CMAQ_Control_Misc.nml
[link_B.1_desis]: https://github.com/USEPA/CMAQ/blob/main/CCTM/src/emis/emis/CMAQ_Control_DESID.nml
[link_B.1_desid]: https://github.com/USEPA/CMAQ/blob/main/CCTM/src/MECHS/cb6r5_ae7_aq/CMAQ_Control_DESID_cb6r5_ae7_aq.nml
[link_B.3_aero]: https://github.com/USEPA/CMAQ/blob/main/CCTM/src/aero/aero6/AERO_DATA.F
