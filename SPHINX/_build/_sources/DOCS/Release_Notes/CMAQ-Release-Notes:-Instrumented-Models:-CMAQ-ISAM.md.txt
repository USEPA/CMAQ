##  Source Apportionment of SOA
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency      
**Type of update**: Science Update    
**Release Version**: CMAQv5.5  

**Description**: The changes expand ISAM capabilities to quantify source contributions to total secondary organic aerosol (SOA) and individual species.

**Significance and Impact**: The model update extends the apportionment already represented for VOC species to their oxidation products and finally to the aerosol counterparts of those products. The connections between reversibly partitioning particle- and vapor-phase species are prescribed in the main table for aerosol parameters in AERO_DATA.F. Some SOA species are so low in volatility that they irreversibly partition to the particle phase. These species are connected to their reaction counter variable from the gas-phase mechanism.

Dynamic equilibrium is a challenging concept in the context of source apportionment. If mass is transferred to and from both the particle and vapor phases during a time step, then it is difficult to represent the mixing of source attribution that most likely occurs. As a simplification, CMAQ-ISAM assumes that the particle- and vapor-phase source attribution for each species are identical at the end of the time step. Nevertheless, attribution typically varies widely from species to species, and this variability impacts the apparent source attribution of the bulk SOA.

With SOA source apportionment now supported, all components of the particulate and gas phases are represented by CMAQ-ISAM and the apportionment of bulk PM metrics may be quantified.  

In the example below, CMAQ-ISAM is used to investigate how SOA formed from biogenic VOCs is impacted by anthropogenic sources. Between 2005 and 2018, US NOX and SO2 emissions reductions yielded substantial reductions in this "biogenic SOA". 
<img src="https://github.com/user-attachments/assets/6c2e2110-4b3f-4c74-b72a-cb5d659dfe4a">
<img src="https://github.com/user-attachments/assets/0dc5e0fe-9281-4002-af43-e544d3acd3f5">


|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#773](https://github.com/USEPA/CMAQ/commit/f36332fdb4986a657acbf5e7b3d9a96d65389253) | [PR#773](https://github.com/USEPA/CMAQ_Dev/pull/773)  |
|[Merge for PR#699](https://github.com/USEPA/CMAQ/commit/9957dc5f89879a0a58dec83685a2629a6c075564) | [PR#699](https://github.com/USEPA/CMAQ_Dev/pull/699)  |
| [Merge for PR#698](https://github.com/USEPA/CMAQ/commit/f5756e053dac4184474c15e9e38793bf872b5463)| [PR#698](https://github.com/USEPA/CMAQ_Dev/pull/698)  |
| [Merge for PR#676](https://github.com/USEPA/CMAQ/commit/5a365698f578e6ca1bc82291c87e0b5dbc8b7cd4)| [PR#676](https://github.com/USEPA/CMAQ_Dev/pull/676)  |
| [Merge for PR#675](https://github.com/USEPA/CMAQ/commit/77d4685a63f09bc4c1a0baf4ffda8a657a61a676)| [PR#675](https://github.com/USEPA/CMAQ_Dev/pull/675)  |


##  Correct ISAM for aerosol sedimentation
[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency      
**Type of update**: Science Update    
**Release Version**: CMAQv5.5  

**Description**: 
Allows the CMAQ ISAM model to account for loss of aerosol mass from gravitational sedimentation.  

**Significance and Impact**: The changes improves ISAM predictions for aerosol contributions for source sectors so the sum over source sectors better agrees with the concentrations. The improvements are strongest for coarse aerosols species that do not have complex aerosol chemistry. Aerosol species representing toxic metals belong to this category. Improvement was also noticed in accumulation and coarse modes for sulfate. However, the Normalized Mean Bias shifted from negative to positive in the accumulation modes for toxic metals but remained less than one precent.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#979](https://github.com/USEPA/CMAQ/commit/c862ad8f561eb14196a6bb81329242940c8e1b8c) | [PR#979](https://github.com/USEPA/CMAQ_Dev/pull/979)  |


## ISAM Cloud Processing Update
[Sergey L. Napelenok](mailto:contact.email@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version**: CMAQv5.5  

**Description**: In rare instances, the slight difference in mass between the sum of tags and the corresponding bulk quantity leads to instability in ISAM results. This PR also addresses the zero arrays for the first hour of the ISAM output files.  

**Significance and Impact**: Some instabilities in the ISAM results can manifest as a results of how mass is distributed between tags immediately after cloud chemistry processing. ISAM output files will also have not-zero values during the first output timestep.  


|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1057](https://github.com/USEPA/CMAQ/commit/d026eddeaf7c12c7e0828ce1528878ed794fa93e) | [PR#1057](https://github.com/USEPA/CMAQ_Dev/pull/1057)  |


## CMAQ-ISAM Bug Fixes to CMAQ-ISAM released with v5.4 
[Sergey L. Napelenok](mailto:contact.email@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version**: CMAQv5.5  

**Description**: This update includes several bugfixes:  
1. CMAQ-ISAM crashes when compiled in debug mode and run for the TAGCLASS 'PM_TOT'. This error relates to aerosol indexing of water species.   
2. The CMAQ-ISAM apportionment of the species 'AORGCJ' was not being calculated correctly, resulting in extremely high values (~E+20). 'AORGCJ' is tracked when the 'ALL' TAGCLASS is defined, but was not being properly tracked through aqueous chemistry. Additionally, 'MGLY' a precursor to 'AORGCJ' was missing from the CMAQ-ISAM gas species.  
3. CMAQ-ISAM crashes when compiled in debug mode and run for the TAGCLASS 'ALL'. This error relates to CMAQ-ISAM tracking air density and incorrectly trying to do unit conversions on this species.  
4. Ground level production and loss calculations in the deposition modules (M3DRY and STAGE) was incorrect. 

**Significance and Impact**: No impact on CMAQ species concentrations found in the "CCTM_CONC* & CCTM_ACONC*" files.  
1. The model no longer crashes in debug mode for the TAGCLASSES 'PM_TOT'.   
2. The addition of 'MGLY' may impact O3 apportionment in some application. 'AORGCJ' apportionment no longer produces high values.   
3. The model no longer crashes in debug mode for the TAGCLASSES 'ALL'.   
4. The changes to ground level production and loss terms may impact a number of tracked CMAQ-ISAM species and TAGCLASSES depending on the case.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#973](https://github.com/USEPA/CMAQ/commit/3a6ad06d130b6dc81de1cc2ccc8a4ed0da448099) | [PR#973](https://github.com/USEPA/CMAQ_Dev/pull/973)  |


## CMAQ-ISAM potential vorticity apportionment  
[Sergey L. Napelenok](mailto:contact.email@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Science Update  
**Release Version**: CMAQv5.5   
 
**Description**: It is possible to apportion species in the OZONE TAGCLASS to ozone incursions at the top of the simulated volume if the base model is compiled with potential vorticity module enabled. For CMAQ-ISAM, this apportionment can be further subdivided by geographical region.   

**Significance and Impact**: Ozone introduced at the top of the model can be tracked by specifying the keyword 'PVO3' in the 'EMIS STREAM(S)' section of the control file as follows:

    TAG NAME        |PVO    
    REGION(S)       |EVERYWHERE     
    EMIS STREAM(S)  |PVO3   

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#957](https://github.com/USEPA/CMAQ/commit/bb4ebd757ebbb70f0f5e7ce32db52c716d08fdc8) | [PR#957](https://github.com/USEPA/CMAQ_Dev/pull/957)  |


## CMAQ-ISAM version 5.4
[Sergey L. Napelenok](mailto:contact.email@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Science Update  
**Release Version**: CMAQv5.4  
  
**Description**:  
The changes to ISAM include the added flexibility for the user to define how secondarily formed gaseous species are assigned to sources of parent reactants. Previously, daughter products were always assigned equally among the parents. However, in some applications, particularly in O3 apportionment, other assignment schemes are desired and requested by the users. The following options are now available in CMAQ-ISAM:

Option 1. Equal assignment (previous version).

Option 2. If parent reactants include the species NO, NO2, NO3, HONO, or ANO3, assignment to these sources only.  Reactions without these species proceed with equal assignment.

Option 3. Option 2 with species list expanded to include reactive VOC species and radicals. Reactions without these species proceed with equal assignment.

Option 4. If parent reactants include the reactive VOC species and radicals, assignment to these sources only.  Reactions without these species proceed with equal assignment.

Option 5. Assignment is based on the ratio of production H2O2 to production HNO3. Reactions without the nitrogen species listed above and reactive VOCs proceed with equal assignment.

Additionally, the transitional value for Option 5 is also customizable (default is PH2O2/PHNO3=0.35).  

**Significance and Impact**:  
The changes here allow for greater ISAM user flexibility. Please, refer to the User's Guide chapter on ISAM for additional information.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#473](https://github.com/USEPA/CMAQ/commit/e57609fda0b2ebfe536160663140bba5a7e3aadc) | [PR#473](https://github.com/USEPA/CMAQ_Dev/pull/473)  |
|[Merge for PR#579](https://github.com/USEPA/CMAQ/commit/d4a1b233cb5a04ed475b5d2efd6470ac3d90fe3d) | [PR#579](https://github.com/USEPA/CMAQ_Dev/pull/579)  |
|[Merge for PR#620](https://github.com/USEPA/CMAQ/commit/0945d28d177dbde3157e12e5f4bed32b2cce30cd) | [PR#620](https://github.com/USEPA/CMAQ_Dev/pull/620)  |
|[Merge for PR#644](https://github.com/USEPA/CMAQ/commit/65cea9503a2ff489d4e9fdc8b7fec349669424fb) | [PR#644](https://github.com/USEPA/CMAQ_Dev/pull/644)  |
|[Merge for PR#649](https://github.com/USEPA/CMAQ/commit/d80a986b1ad1559d26216895a2baec78893402f6) | [PR#649](https://github.com/USEPA/CMAQ/pull/649)  |
|[Merge for PR#650](https://github.com/USEPA/CMAQ/commit/c545abd1cb28be92d346d756bc281994a95dac8d) | [PR#650](https://github.com/USEPA/CMAQ_Dev/pull/650)  |
|[Merge for PR#655](https://github.com/USEPA/CMAQ/commit/155bdeef0d62caec50c31f46c784b6f2dfa89df6) | [PR#655](https://github.com/USEPA/CMAQ_Dev/pull/655)  |
|[Merge for PR#661](https://github.com/USEPA/CMAQ/commit/2c30969f720575d0281532113228b21d88a4d3e6) | [PR#657](https://github.com/USEPA/CMAQ_Dev/pull/657)  |
|[Merge for PR#661](https://github.com/USEPA/CMAQ/commit/f15d6a8fe67c0b81ad944a30d43b919915c6593e) | [PR#661](https://github.com/USEPA/CMAQ_Dev/pull/661)  |
|[Merge for PR#675](https://github.com/USEPA/CMAQ/commit/77d4685a63f09bc4c1a0baf4ffda8a657a61a676) | [PR#675](https://github.com/USEPA/CMAQ_Dev/pull/675)  |
|[Merge for PR#676](https://github.com/USEPA/CMAQ/commit/5a365698f578e6ca1bc82291c87e0b5dbc8b7cd4) | [PR#676](https://github.com/USEPA/CMAQ_Dev/pull/676)  |
|[Merge for PR#698](https://github.com/USEPA/CMAQ/commit/f5756e053dac4184474c15e9e38793bf872b5463) | [PR#698](https://github.com/USEPA/CMAQ_Dev/pull/698)  |
|[Merge for PR#699](https://github.com/USEPA/CMAQ/commit/9957dc5f89879a0a58dec83685a2629a6c075564) | [PR#699](https://github.com/USEPA/CMAQ_Dev/pull/699)  |
|[Merge for PR#778](https://github.com/USEPA/CMAQ/commit/b938e98a8e1fdb4678ae89ee33f1d3457c850331) | [PR#778](https://github.com/USEPA/CMAQ_Dev/pull/778)  |


## CMAQ-ISAM tagclasses for Hazardous Air Pollutants (HAPs)  
[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: New Feature  
**Release Version**: CMAQv5.4  
   
**Description**: Several tagclasses were added to CMAQ-ISAM to track the source contributions for HAPs. The below table lists the tagclasses and their HAPs.

| Tagclass Name | HAPS<sup>1</sup> |
|:------------- |:-----|
| HAP_GAS       | formaldehyde, acetaldehyde, 1,3-butadiene, acrolein, molecular chlorine |
| HAP_AERO      | aerosol bound nickel, lead, trivalent chromium, hexavalent chromium, beryllium, cadmium, diesel emissions, manganese from the Air Toxics Inventory, and arsenic |
| PAH_TEQ       | Polycycle Aromatic Compounds lumped based on Toxic Equivalency Quotient |
| MERCURY       | Gas and Aerosol Phases of Atmospheric Mercury |
| BENAPYRENE    | Gas and Aerosol Phases of Benzo[a]pyrene      |
1. The exact species available per Tagclass depends on the chemical mechanism and the name-lists used.

**Significance and Impact**: Based on the chemical mechanism used, the new tagclasses allow simulating how the emissions source contribute to concentrations and deposition of HAPS in air toxic assessments.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#922](https://github.com/USEPA/CMAQ/commit/df82fd1c8381f873d9b56ab74a2af6b9ed4ee61e) | [PR#922](https://github.com/USEPA/CMAQ_Dev/pull/922)  |
|[Merge for PR#887](https://github.com/USEPA/CMAQ/commit/2a64783ac1d0f6dc8ad708920f112c6f2035f8bc) | [PR#887](https://github.com/USEPA/CMAQ_Dev/pull/887)  |
|[Merge for PR#773](https://github.com/USEPA/CMAQ/commit/f36332fdb4986a657acbf5e7b3d9a96d65389253) | [PR#773](https://github.com/USEPA/CMAQ_Dev/pull/773)  |
|[Merge for PR#699](https://github.com/USEPA/CMAQ/commit/9957dc5f89879a0a58dec83685a2629a6c075564) | [PR#699](https://github.com/USEPA/CMAQ_Dev/pull/699)  |
