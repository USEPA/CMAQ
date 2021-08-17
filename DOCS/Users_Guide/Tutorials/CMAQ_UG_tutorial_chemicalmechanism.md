## CMAQ Tutorial ##

Author: Elyse Pennington (epenning@caltech.edu)

### Modifying a Chemical Mechanism in CMAQ ###


Goal: Modify the gas- and aerosol-phase chemical mechanisms in CMAQ, create a new solver, and reflect all the changes in Github. This tutorial includes examples impacting SOA precursors and products, and does not include impacts on ozone or other radical chemistry. Caution: If significant modifications are made to the gas-phase mechanism that alter the radical balance, the ebi implementation of the modified mechanism should be checked against an alternative solver such as ros3 or smvgear. This example does not include extensive modifications.

### Files to edit ##
1. mech_*.def
2. AE namelist
3. GC namelist
4. NR namelist
5. EmissCtrl namelist
6. AERO_DATA.F
7. SOA_DEFN.F
8. hlconst.F
9. SpecDef_*.txt
10. SpecDef_Dep_*.txt


### Utilities to use
1. chemmech (see [documentation](../../../UTIL/chemmech/README.md))
2. create_ebi (see [documentation](../../../UTIL/create_ebi/README.md))


<a id=modifychem></a>
## Modifying the chemical mechanism ##
### 1. See the [git instructions](#github) below if you'd like to reflect the chemical mechanism changes in your Github repository.


<a id=mech_def></a>
### 2. Edit mech.def.
The mech.def file lists all of CMAQ's chemical reactions and is located at $CMAQ_REPO/CCTM/src/MECHS/${mechanism}/mech_${mechanism}.def. The [chemmech documentation](../../../UTIL/chemmech/README.md) describes formats for reaction rate constants dependent on temperature, atmospheric number density, water vapor, sunlight, model species and constants such as oxygen and methane mixing ratios. The documentation also gives a more detailed explanation of the mech.def (mechanism definitions) sections and formatting rules.
- All reactions must begin with a name in < > brackets.
- All reactions must end with # followed by a reaction rate constant with units of cm<sup>3</sup>/(molecules s)
- In this tutorial, all reactions regenerate the oxidant.

In this example, we add an Odum 2-product model by reacting a gas-phase precursor (TPROD) with OH to form two semivolatile gas-phase species (SVTPROD1, SVTPROD2) with alpha values of 0.15 and 0.8 by mole and a rate constant of 4.5 x 10^<sup>-11</sup> cm<sup>3</sup>/(molecules s):
```
<TWOPROD> TPROD + OH = OH + 0.15 * SVTPROD1 + 0.80 * SVTPROD2 #4.50E-11;
```
To form a nonvolatile, accumulation mode SOA species (ANONVJ) from a gas-phase IVOC species (NONVG) with an SOA yield of 5% by mole and a rate constant of 2 x 10<sup>-11</sup> cm<sup>3</sup>/(molecules s):
```
<NONV> NONVG + OH = OH + 0.05 * ANONVJ #2.00E-11;
```


<a id=GCnml></a>
### 3. Edit GC namelist.
The GC namelist defines gas-phase species and their physical and chemical properties. It's located at $CMAQ_REPO/CCTM/src/MECHS/${mechanism}/GC_${mechanism}.nml.
You must add a new row for every gas-phase species that was added to [mech.def](#mech_def). See [Chapter 4](../CMAQ_UG_ch04_model_inputs.md) for more information.
TPROD, SVTPROD1, SVTPROD2, and NONVG from the examples above must be added to the GC namelist because they're gas-phase species. Column descriptions can be found in [Chapter 4](../CMAQ_UG_ch04_model_inputs.md). In this example, TPROD does not participate in dry deposition - similar to many other VOCs in CMAQ - so 'DRYDEP SURR' and 'DDEP' are empty and FAC is -1. NONVG (an IVOC as defined above), SVTPROD1, and SVTPROD2 do participate in dry deposition because of their low volatilities. This tutorial does not explain the process of creating new dry deposition surrogates, but it is possible to do so and replace 'VD_GEN_ALD'. The WET-SCAV SURR are described in the [hlconst.F](#hlconst) section below. 'GC2AE SURR' lists the species that partition between gas and aerosol phases in [SOA_DEFN.F](#SOA_DEFN).
```
!SPECIES        ,MOLWT   ,IC     ,IC_FAC ,BC     ,BC_FAC ,DRYDEP SURR       ,FAC  ,WET-SCAV SURR     ,FAC ,GC2AE SURR     ,GC2AQ SURR,TRNS  ,DDEP  ,WDEP  ,CONC
'SVTPROD1'      ,216.66  ,''     ,-1     ,''     ,-1     ,'VD_GEN_ALD'      , 1   ,'SVTPROD1'        , 1  ,'SVTPROD1'     ,''        ,'Yes' ,'Yes' ,'Yes' ,'Yes',
'SVTPROD2'      ,182.66  ,''     ,-1     ,''     ,-1     ,'VD_GEN_ALD'      , 1   ,'SVTPROD2'        , 1  ,'SVTPROD2'     ,''        ,'Yes' ,'Yes' ,'Yes' ,'Yes',
'TPROD'         ,168.66  ,''     ,-1     ,''     ,-1     ,''                ,-1   ,'TPROD'           , 1  ,''             ,''        ,'Yes' ,''    ,'Yes' ,'Yes',
'NONVG'         ,119.54  ,''     ,-1     ,''     ,-1     ,'VD_GEN_ALD'      , 1   ,'NONVG'           , 1  ,''             ,''        ,'Yes' ,'Yes' ,'Yes' ,'Yes'
```



<a id=AEnml></a>
### 4. Edit AE namelist.
The AE namelist defines all aerosol-phase species and their physical and chemical properties and is located at $CMAQ_REPO/CCTM/src/MECHS/${mechanism}/AE_${mechanism}.nml
You must add a new row for every aerosol-phase species added to [AERO_DATA.F](#AERO_DATA). See [Chapter 4](../CMAQ_UG_ch04_model_inputs.md) for more information.
ANONVJ and the aerosol products from the Odum 2-product model must be added to the AE namelist. The semivolatile Odum 2-product species (SVTPROD1 and SVTPROD2) partition between the gas and accumulation mode aerosol phase with ATPROD1J and ATPROD2J. Column descriptions can be found in [Chapter 4](../CMAQ_UG_ch04_model_inputs.md). 
```
!SPECIES   ,MOLWT   ,IC     ,IC_FAC ,BC     ,BC_FAC ,DRYDEP SURR ,FAC ,WET-SCAV SURR  ,FAC ,AE2AQ SURR     ,TRNS    ,DDEP    ,WDEP    ,CONC
'ATPROD1J' ,216.66  ,''     ,-1     ,''     ,-1     ,'VMASSJ'    , 1  ,'ORG_ACCUM'    , 1  ,'SOA_ACCUM'    ,'Yes'   ,'Yes'   ,'Yes'   ,'Yes',
'ATPROD2J' ,182.66  ,''     ,-1     ,''     ,-1     ,'VMASSJ'    , 1  ,'ORG_ACCUM'    , 1  ,'SOA_ACCUM'    ,'Yes'   ,'Yes'   ,'Yes'   ,'Yes',
'ANONVJ'   ,135.54  ,''     ,-1     ,''     ,-1     ,'VMASSJ'    , 1  ,'ORG_ACCUM'    , 1  ,'SOA_ACCUM'    ,'Yes'   ,'Yes'   ,'Yes'   ,'Yes',
```



<a id=NRnml></a>
### 5. Edit NR namelist.
The NR namelist defines gas-phase species that are not in the mech.def file, and their physical and chemical properties. Species in this file are typically the semivolatile gases that partition between the gas- and aerosol-phases. It's located at $CMAQ_REPO/CCTM/src/MECHS/${mechanism}/NR_${mechanism}.nml.
You must add a new row for every nonreactive species, if any, added to the chemical mechanism that is not explicitly modeled in [mech.def](#mech_def). See [Chapter 4](../CMAQ_UG_ch04_model_inputs.md) for descriptions of the information in each column.
The examples used in this tutorial do not include species that need to be added to the NR namelist. Follow the sesquiterpene SOA formation mechanism as an example of NR species (e.g. follow SESQRXN, SVSQT, and ASQTJ in $CMAQ_REPO/CCTM/src/MECHS/${mechanism}/ and $CMAQ_REPO/CCTM/src/aero/aero6/).



<a id=EmissCtrl></a>
### 6. Edit Emissions Control file.
The Emissions Control file describes how to input emissions and is located at $CMAQ_REPO/CCTM/src/MECHS/${mechanism}/EmissCtrl_${mechanism}.nml. Any new species included in the mech.def or GC, AE, and NR namelists that is directly emitted should be included in this file. Examples of adding new species are given in the !> CUSTOM MAPPING EXAMPLES <! section and further description can be found in the [DESID tutorial](CMAQ_UG_tutorial_emissions.md).



<a id=SpecDef></a>
### 7. Edit SpecDef file.
The SpecDef file is used to aggregate CMAQ output species (e.g. into PM<sub>2.5</sub>) and convert units. It is used to run the post-processing tool [combine](../../../POST/combine/README.md) and is located at $CMAQ_REPO/CCTM/src/MECHS/${mechanism}/SpecDef_{mechanism}.txt.
To convert the units of a gas-phase species to ppb, add the following line:
```
NEWGAS          ,ppbV      ,1000.*NEWGAS[1]
```
To add a new species to OA mass, add it to the appropriate POA or SOA variables. For example, to add a new SOA accumulation-mode aerosol species ANEWJ, include '+ANEWJ[1]' in ASOMJ. This change will be reflected in subsequent variable definitions that use ASOMJ.

If your simulation domain is an urban area, move AGLYJ from AORGB (biogenic VOC-derived aerosol) to AORGA (anthropogenic VOC-derived SOA).

In some cases you may want to remove pcSOA from your SOA. In this case, you must create new variables with APCSOJ subtracted. For example, to calculate PM<sub>1</sub> SOA without pcSOA, update the following variables:
```
AOMJ_MP         ,ug m-3    ,APOMJ[0]  + ASOMJ[0] - APCSOJ[1]
ATOTJ_MP        ,ug m-3    ,ASO4J[1]+ANO3J[1]+ANH4J[1]+ANAJ[1]+ACLJ[1] \
                           +AECJ[1]+AOMJ_MP[0]+AOTHRJ[1]+AFEJ[1]+ASIJ[1]  \
                           +ATIJ[1]+ACAJ[1]+AMGJ[1]+AMNJ[1]+AALJ[1]+AKJ[1]
PM1_TOT_MP      ,ug m-3    ,ATOTI[0]*PM1AT[3]+ATOTJ_MP[0]*PM1AC[3]+ATOTK[0]*PM1CO[3]
```
To update the OC variables or the deposition of OC variables in the SpecDef_Dep_{mechanism}.txt file, you must know the OM:OC ratios of the new organic aerosol species.



<a id=SOA_DEFN></a>
### 8. Edit SOA_DEFN.F
SOA_DEFN.F describes SOA precursors, SOA species and their properties dealing with gas to particle partitioning. It is located at $CMAQ_REPO/CCTM/src/aero/aero6/SOA_DEFN.F. Note that the aero7 directory is linked to the aero6 directory.

You must add a row for every new SOA species and increase n_oa_list by the number of species added to the list.

To add semivolatile species that partition between the gas and aerosol phases (with a gas-phase species defined in the [GC namelist](#GCnml)), include their effective saturation concentrations (C*) and enthalpies of vaporization. In this example, ATPROD1 has the corresponding gas-phase species SVTPROD1 and has C* = 0.95 ug/m<sup>3</sup> and enthalpy of vaporization = 131 J/mol. ATPROD2 has the corresponding gas-phase species SVTPROD2 and has C* = 485 ug/m<sup>3</sup> and enthalpy of vaporization = 101 J/mol:
```
& oa_type('ATPROD1', 'SVTPROD1', '        ',  0.0000,     0.95, 131.0E3,   F ),
& oa_type('ATPROD2', 'SVTPROD2', '        ',  0.0000,   485.00, 101.0E3,   F )
```
To add a nonvolatile aerosol species:
```
& oa_type('ANONV  ', '        ', '        ',  0.0000,   1.E-10,   1.0E0,   T )
```
Note that these aerosol definitions do not include a specification of the size bin they fall into. That is instead defined in the [AE namelist](#AEnml) by I, J, or K (for Aitken, accumulation, or coarse mode aerosol, respectively) at the end of the variable name.



<a id=AERO_DATA></a>
### 9. Edit AERO_DATA.F
AERO_DATA.F defines all aerosol species and some of their properties. It is located at $CMAQ_REPO/CCTM/src/aero/aero6/AERO_DATA.F. Note that the aero7 directory is linked to the aero6 directory.

You must add a row for every new aerosol species and increase n_aerolist be the number of species added to the list.

To add a semivolatile organic aerosol species, set OM to T, set no_M2Wet to T, calculate korg from e.g. Pye et al., ACP, 2017, and use properties that match other organic species:
```
& spcs_list_type('ATPROD1 ', cm_set, 1400.0, T,F,  0,  2.8, 6.1,T, 'DUST  ', 0.09),
& spcs_list_type('ATPROD2 ', cm_set, 1400.0, T,F,  0,  2.8, 6.1,T, 'DUST  ', 0.05),
```
To add a nonvolatile organic aerosol species, set no_M2Wet to F:
```
& spcs_list_type('ANONV   ', cm_set, 1400.0, F,F,  0,  2.8, 6.1,T, 'DUST  ', 0.07),
```
Note that these aerosol definitions do not include a specification of the size bin they fall into. That is instead defined in the [AE namelist](#AEnml) by I, J, or K (for Aitken, accumulation, or coarse mode aerosol, respectively) at the end of the variable name.



<a id=hlconst></a>
### 10. Edit hlconst.F
hlconst.F calculates Henry's Law constants for species that participate in wet deposition. It's located in the relevant cloud directory at $CMAQ_REPO/CCTM/src/cloud/*/hlconst.F.

Each new row corresponds to a name used in the 'WET-SCAV SURR' column of the [GC namelist](#GCnml). Increase MXSPCS by the number of species added to the list.

Based on the additions to the [GC namelist](#GCnml) in these examples, wet deposition surrogates must be added for TPROD, SVTPROD1, SVTPROD2, and NONVG. The first 3 columns are row numbers in the data matrix. Column 4 is the name of the wet deposition surrogate used in the [GC namelist](#GCnml) and will often be the same as the species name. Column 5 is the Henry's law constant at 298.15 K (M/atm). Column 6 is the enthalpy; for organic semivolatile species with unknown enthalpy, 6.0E+03 may be used. See references listed in hlconst.F for models to calculate Henry's Law constants where experimental data is unavailable.
```
      DATA SUBNAME(217), A(217), E(217) / 'TPROD           ', 3.87E+02,  6.0E+03 /
      DATA SUBNAME(218), A(218), E(218) / 'SVTPROD1        ', 2.97E+06,  6.0E+03 /
      DATA SUBNAME(219), A(219), E(219) / 'SVTPROD2        ', 7.99E+05,  6.0E+03 /
      DATA SUBNAME(220), A(220), E(220) / 'NONVG           ', 2.22E+03,  6.0E+03 /

```
Dry deposition surrogates may also be added, but are not covered in this tutorial.



<a id=copy_src></a>
### 11. Build CMAQ_PROJECT.
See [Chapter 5](../CMAQ_UG_ch05_running_a_simulation.md) or the [Tutorials](README.md) for more information.


<a id=chemmech_build></a>
### 12. Build chemmech.
Copy the source code from CMAQ_REPO to CMAQ_PROJECT.
```
cp -r $CMAQ_REPO/UTIL/chemmech/ $CMAQ_PROJECT/UTIL/
```
Edit $CMAQ_PROJECT/UTIL/chemmech/scripts/bldit_chemmech.csh to make sure the correct compiler is set. Then run the build script.
```
./bldit_chemmech.csh
```



<a id=chemmech_run></a>
### 13. Run chemmech.
Edit run_chemmech.csh
- COMPILER
- Update correct Mechanism
- Set Mpath to the location of the [mech.def](#mech_def) file you modified above.
- Change the location of the tracer namelist file
```
set TR_NML = $CMAQ_REPO/CCTM/src/MECHS/trac0/Species_Table_TR_0.nml
```
Run:
```
./run_chemmech.csh
```
If successful, it will output, for example:
```
Normal Completion of CHEMMECH
Author is NAME
output written to ../output/saprc07tic_ae7i_aq-Sep-02-2020
```
and will write RXNS_DATA_MODULE.F90 and RXNS_FUNC_MODULE.F90 to the output path. Check the output mechanism csv, html, and markdown files to confirm that chemmech ran correctly. Copy the two Fortran files to $CMAQ_REPO/CCTM/src/MECHS/${Mechanism}/.



<a id=ebi_build></a>
### 14. Build+run create_ebi.
Copy the source code from CMAQ_REPO to CMAQ_PROJECT.
```
cp -r $CMAQ_REPO/UTIL/create_ebi/ $CMAQ_PROJECT/UTIL/
```
Move bldrun_create_ebi.csh up one directory (from $CMAQ_PROJECT/UTIL/create_ebi/scripts/ to $CMAQ_PROJECT/UTIL/create_ebi/). Edit bldrun_create_ebi.csh:
- COMPILER
- Update MECH for your mechanism.
- Set RXNS_DATA to the location of your chemmech output files.
- Set PAR_NEG_FLAG, DEGRADE_SUBS, SOLVER_DELT, and all MECH_*(species) variables to the setting that matches your mechanism. E.g. for saprc07tic_ae7i_aq:
```
 setenv PAR_NEG_FLAG    F    
 setenv DEGRADE_SUBS    F    
 setenv SOLVER_DELT     1.25 
```
The reactions added in this tutorial do not affect radical species in ozone chemistry. If it did, we recommend checking predictions using the EBI solver against an alternative gas solver listed in the cctm build script such as ros3 and smvgear. Check Table 1 in the [create_ebi documentation](../../../UTIL/create_ebi/README.md) as an initial list of radical species that may require such benchmarking. The list grows if new radical cycles are added to a mechanism such as radicals from halogen compounds.
Run:
```
./bldrun_create_ebi.csh
```
If successful, it will output:
```
The following 10 output files were created:
     hrdriver.F
     hrsolver.F
     hrdata_mod.F
     hrinit.F
     hrg1.F
     hrg2.F
     hrg3.F
     hrg4.F
     hrprodloss.F
     hrrates.F
Program CR_EBI_SOLVER completed successfully
if ( F == T ) then
exit ( )
```
and will write the hr*.F files to ${CMAQ_HOME}/CMAQv533/UTIL/create_ebi/output/ebi_${Mechanism}-$DATE-${COMPILER}/. Copy the hr*.F files to ${CMAQ_REPO}/CCTM/src/gas/ebi_${Mechanism}/.



<a id=cctm_build></a>
### 15. Build the CCTM executable.
See [Chapter 5](../CMAQ_UG_ch05_running_a_simulation.md) or the [Tutorials](README.md) for more information. This will include all of the new files from ${CMAQ_REPO}/CCTM/src/ in $CMAQ_PROJECT/CCTM/BLD.


<a id=cmaq_run></a>
### 16. Run CCTM and post-processing tools.
See [Chapter 5](../CMAQ_UG_ch05_running_a_simulation.md) for more information about running the CCTM. See [Chapter 8](../CMAQ_UG_ch08_analysis_tools.md) for more information about running AMET, combine, sitecmp, etc. While running these post-processing tools, be sure to set file paths to the new files created in $CMAQ_REPO or $CMAQ_PROJECT/CCTM/BLD.


<a id=github></a>
## Reflecting the changes in Github ##
### 1. Fork from USEPA CMAQ.
On the [CMAQ Github page](../../../README.md), fork the main branch to your personal repository using the Fork button in the upper right.

### 2. Clone.
Clone your repository to your remote account. For example:
```
git clone https://username@github.com/username/CMAQ.git CMAQ_REPO_v533
```
This will request your Github password. You will now see the entire CMAQ repository in the directory you cloned it into. If you enter the top directory (e.g. CMAQ_REPO_v533/), there should now exist a file named .git.

### 3. Rename remote.
Rename the remote link. For example:
```
git remote rename origin dev_push_repo
```

### 4. Link to USEPA CMAQ.
Link the cloned repo to the USEPA Github repo.
```
git remote add dev_repo https://username@github.com/USEPA/CMAQ.git
```

### 5. Branching.
When modifying your repository, it's a good idea to check out a new branch. To create the branch:
```
git branch newchem
```
To move to that branch:
```
git checkout newchem
```
To look at all of your branches:
```
git branch
```
The branch you're currently working from will have an asterisk.

### 6. Modify the mechanism according to the [instructions](#modifychem) above.
If the USEPA repository is updated by EPA, you will see a statement such as "This branch is X commits behind USEPA:main" in Github online. You will likely want to keep your CMAQ up-to-date and will want to pull the updates to your repo. Make sure the files you've edited are backed up.
Check the names of your remotes using:
```
git remote -v
```
If you've followed these instructions, your repository should be named dev_push_repo and the USEPA's repository should be named dev_repo. To pull in the updates from USEPA's main branch:
```
git pull dev_repo main
```
To view a summary of the changes you've made to your repo since your last commit, type "git status" from anywhere in the repo. If you've followed the instructions above, you should see:
```
# On branch newchem
# Changes not staged for commit:
#   (use "git add <file>..." to update what will be committed)
#   (use "git checkout -- <file>..." to discard changes in working directory)
#
#       modified:   CCTM/src/MECHS/saprc07tic_ae7i_aq/AE_saprc07tic_ae7i_aq.nml
#       modified:   CCTM/src/MECHS/saprc07tic_ae7i_aq/EmissCtrl_saprc07tic_ae7i_aq.nml
#       modified:   CCTM/src/MECHS/saprc07tic_ae7i_aq/GC_saprc07tic_ae7i_aq.nml
#       modified:   CCTM/src/MECHS/saprc07tic_ae7i_aq/RXNS_DATA_MODULE.F90
#       modified:   CCTM/src/MECHS/saprc07tic_ae7i_aq/RXNS_FUNC_MODULE.F90
#       modified:   CCTM/src/MECHS/saprc07tic_ae7i_aq/SpecDef_saprc07tic_ae7i_aq.txt
#       modified:   CCTM/src/MECHS/saprc07tic_ae7i_aq/mech_saprc07tic_ae7i_aq.def
#       modified:   CCTM/src/aero/aero6/AERO_DATA.F
#       modified:   CCTM/src/aero/aero6/SOA_DEFN.F
#       modified:   CCTM/src/cloud/acm_ae6/hlconst.F
#       modified:   CCTM/src/gas/ebi_saprc07tic_ae7i_aq/hrdata_mod.F
#       modified:   CCTM/src/gas/ebi_saprc07tic_ae7i_aq/hrdriver.F
#       modified:   CCTM/src/gas/ebi_saprc07tic_ae7i_aq/hrg1.F
#       modified:   CCTM/src/gas/ebi_saprc07tic_ae7i_aq/hrg2.F
#       modified:   CCTM/src/gas/ebi_saprc07tic_ae7i_aq/hrg3.F
#       modified:   CCTM/src/gas/ebi_saprc07tic_ae7i_aq/hrg4.F
#       modified:   CCTM/src/gas/ebi_saprc07tic_ae7i_aq/hrinit.F
#       modified:   CCTM/src/gas/ebi_saprc07tic_ae7i_aq/hrprodloss.F
#       modified:   CCTM/src/gas/ebi_saprc07tic_ae7i_aq/hrrates.F
#       modified:   CCTM/src/gas/ebi_saprc07tic_ae7i_aq/hrsolver.F
#       modified:   bldit_project.csh
```
To see a list of all lines that have been modified in those files, type "git diff".

### 7. Commit the changes.
To stage all modified files in current directory for commit:
```
git add -u
```
To stage specific files for commit:
```
git add [filename1] [filename2]
```
To commit:
```
git commit
```
A page indicating all changes in the commit will be displayed. Enter a description at the top and close the page using :x and Enter.

### 8. Push the changes to your Github respository.
Make sure you don't push the changes to the USEPA CMAQ Github!

To push your changes from your newchem branch to your Github repository:
```
git push dev_push_repo newchem
```
You should now be able to see these changes in Github online.



