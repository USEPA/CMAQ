# Create EBI Solver (create_ebi)

### Quick Start


To create a new EBI solver:

1) Copy and edit scripts/bldrun.create_ebi.csh for your compiler and Mechanism. Save to build the software.

2) Execute the script.

To report potential program errors or EBI solver failures, contact Bill Hutzell/USEPA at hutzell.bill@epa.gov

### Description

The create_ebi utility generates an Euler Backward Iterative (EBI) solver for a photochemical mechanism. Source code generated should be used to build the CMAQ CCTM using the photochemical mechanism. The solver is based on Hertel et. al (1993) and was developed to solve the Ox, HOx, NOx, VOC cycles in tropospheric photochemistry. It combines analytical solutions for specific mechanism species and a numerical method for the remaining mechanism species. The photochemical mechanism must include the specific species and their chemistry needs to meet set rules. If it does not satisfy these constraints, an EBI solver produced by create_ebi should not be used. The create_ebi utility attempts to test for meeting these constraints and stop if they are not met but the tests may not detect all possible cases for violations.

Table 1.
Photochemistry Species or Compounds Required for an EBI solver 
Model species can be different between CMAQ mechanisms

| Name  |   Formula            |     
|------| ------  |
| nitric oxide |  NO |  
| nitrogen dioxide | NO<sub>2</sub>  |
| ozone |  O<sub>3</sub> |
| ground state oxygen atom | O(3P)  | 
| excited state oxygen atom | O(1D) |
| hydroxyl radical | OH |
| hydroperoxy radical | HO<sub>2</sub>| 
| nitrous acid | HONO |
| peroxynitric acid | HNO<sub>4</sub> | 
| peroxy acetyl nitrate | C<sub>2</sub>H<sub>3</sub>NO<sub>5</sub> |
| peroxy acetyl radical | C<sub>2</sub>H<sub>3</sub>O<sub>3</sub> |
| nitrate radical |  NO<sub>3</sub> |
| dinitrogen pentoxide | N<sub>2</sub>O<sub>5</sub> |


### Files, configuration, and environment variables

To implement a new mechanism in CMAQ, start with a mechanism definition (mech.def) file and CSV species files from an existing mechanism in the model. Edit the mech.def file to include the new reactions, species, and reaction rates and provide this new mech.def file as input to the program [CHEMMECH](#CHEMMECH). CHEMMECH will output a RXNS_DATA_MODULE.F90 file, which is used as input to CREATE_EBI.

#### CREATE_EBI input files

**Table 7-13. CREATE_EBI input files**

|**File Name**|**Format**|**Description**|
|----------------------------------|----------|----------------------------------------------------------|
|RXNS_DATA_MODULE.F90|ASCII|a Fortran 90 module describing the photochemical mechanism produced by the CHEMMECH utility |


#### CREATE_EBI output files

**Table 7‑14. CREATE_EBI output files**

|File Name|Format|Description|
|---------------------------------------|---------------|-------------------------------------------------------|
|hr\*.F|ASCII F90|Fortran 90 files for the CCTM EBI chemistry solver|


The location of the CREATE_EBI output files is set in the run script by the variable OUTDIR. To compile a version of the CMAQ programs that use the F90 files created by CREATE_EBI, these output F90 files need to be moved to a new directory under the `$CMAQ_HOME/CCTM/src/gas` directory. Point the CMAQ build scripts to this new directory through the “Mechanism” variable.

#### Compilation Configuration Variables

-   `GC_NAME [default: None]`  
     Name identifier for gas phase mechanisms
     -  `CB6R3`  
    Carbon Bond version 6 revision 3
     -  `CB05E51`  
     Carbon Bond 05 with modifications for CMAQ version 5.1
     -  `CB05MP51`  
     Carbon Bond 05 multipollutant mechanism for CMAQ version 5.1
     -  `CB05TUCL`  
     Carbon Bond 05 with modified toluene and chlorine chemistry
     -  `CB05TUMP`  
     Carbon Bond 05 with modified toluene and multipollutant chemistry
     -  `SAPRC07TB`  
     SAPRC07 with modified toluene chemistry
     -  `SAPRC07TC`  
     SAPRC07 with modified toluene chemistry
     -  `SAPRC07TIC`  
     SAPRC07 with modified toluene chemistry
     -  `RACM2`  
     RACM2 chemistry
-   `AE_NAME [default: None]`  
    Name identifier for particle phase mechanisms
    - `AE6`  
    CMAQ aerosols version 6
    - `AE6I`  
    CMAQ aerosols version 6i
-   `AQ_NAME [default: AQ]`  
    Name identifier for the CMAQ aqueous phase mechanism

#### Execution Configuration Variables

The environment variables listed here are invoked at run time and are set in the CREATE_EBI run script.

-   `EXEC [default: CHEMMECH]`  
    Executable name
-   `GC_NAME [default: None]`  
    Name identifier for gas phase mechanisms
    -  `CB6R3`  
    Carbon Bond version 6 revision 3
    -  `CB05E51`  
    Carbon Bond 05 with modifications for CMAQ version 5.1
    -  `CB05MP51`  
    Carbon Bond 05 multipollutant mechanism for CMAQ version 5.1
    -  `CB05TUCL`  
    Carbon Bond 05 with modified toluene and chlorine chemistry
    -  `CB05TUMP`  
    Carbon Bond 05 with modified toluene and multipollutant chemistry
    -  `SAPRC07TB`  
    SAPRC07 with modified toluene chemistry
    -  `SAPRC07TC`  
    SAPRC07 with modified toluene chemistry
    -  `SAPRC07TIC`  
    SAPRC07 with modified toluene chemistry
    -  `RACM2`  
    RACM2 chemistry  
-   `AE_NAME [default: None]`  
    Name identifier for particle phase mechanisms
    - `AE6`  
    CMAQ aerosols version 6
    - `AE6I`  
    CMAQ aerosols version 6i
-   `AQ_NAME [default: AQ]`  
    Name identifier for the CMAQ aqueous phase mechanism
-   `OUTDIR [default: ../output]`  
    Output file directory path
-   `COPYRT_FLAG`
-   `CVS_HDR_FLAG`
-   `PAR_NEG_FLAG [default: F]`
    Include PAR negative stoichiometry.
    - `T` for Carbon Bond mechanisms
    - `F` for SAPRC and RACM mechanisms
-   `DEGRADE_SUBS`
-   `NO2EX_CYCLE`
-   `MECH_NO`  
    Mechanism name for nitric oxide
-   `MECH_NO2`  
    Mechanism name for nitrogen dioxide
-   `MECH_NO2EX`  
     SAPRC, RACM Mechanism name for excited nitrogen dioxide; not in Carbon Bond
-   `MECH_O3`  
     Mechanism name for ozone
-   `MECH_O3P`  
    Mechanism name for ground state oxygen atom
    - `O` for Carbon Bond mechanisms
    - `O3P` for SAPRC and RACM mechanisms
-   `MECHO_O1D`  
    Mechanism name for excited state oxygen atom
-   `MECH_OH`  
    Mechanism name for hydroxyl radical
-   `MECH_HO2`  
    Mechanism name for hydroperoxy radical
-   `MECH_HONO`  
     Mechanism name for nitrous acid
-   `MECH_HNO4`  
    Mechanism name for peroxynitric acid
    - `PNA` for Carbon Bond mechanisms
    - `HNO4` for SAPRC and RACM mechanisms
-   `MECH_PAN`  
    Mechanism name for peroxy acetyl nitrate
-   `MECH_C2O3`  
    Mechanism name for peroxy acetyl radical
    - `C2O3` for Carbon Bond mechanisms
    - `MECO3` for SAPRC and RACM mechanisms
-   `MECHO_NO3`  
    Mechanism name for nitrate radical
-   `MECH_N2O5`  
    Mechanism name for dinitrogen pentoxide

### Compiling and Running

#### Compile CREATE_EBI ####

To compile CREATE_EBI, invoke the build file at the command line:

```
cd $CMAQ_HOME/UTIL/create_ebi/scripts
./bldit.create_ebi.csh |& tee build.create_ebi.log
```

To port CREATE_EBI to different compilers, change the `COMPILER` variable in the bldit script.

#### Run CREATE_EBI ####

Set the run script settings according to the execution configuration variables described above. Run CREATE_EBI using the following command.
```
cd $CMAQ_HOME/UTIL/create_ebi/scripts
./run.create_ebi.csh |& tee run.create_ebi.log
```
