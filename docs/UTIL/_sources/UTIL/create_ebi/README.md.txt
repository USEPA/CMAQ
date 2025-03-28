# Create EBI Solver (create_ebi)

### Background

The create_ebi utility generates an Euler Backward Iterative (EBI) solver for a photochemical mechanism. Source code generated should be used to build the CMAQ CCTM using the photochemical mechanism. The solver is based on Hertel et. al (1993) and was developed to solve the Ox, HOx, NOx-NOy, VOC cycles in tropospheric photochemistry. It combines analytical solutions for specific mechanism species and a numerical method for the remaining mechanism species. The photochemical mechanism must include the specific species and their chemistry needs to meet set rules (Tables 1 and 2).
If it does not satisfy these constraints, an EBI solver produced by create_ebi should not be used. The create_ebi utility attempts to test for meeting these constraints and stops if they are not met but the tests may not detect all possible cases for violations.


<center> Table 1. 
Photochemistry Species or Compounds Required;    
model species names can be different between CMAQ mechanisms
</center>

| Name  |   Formula            |   Group<sup>1</sup> | 
|:------| :------:  |:-----:|
| nitric oxide |  NO |  1 |
| nitrogen dioxide | NO<sub>2</sub>  | 1 |
| ozone |  O<sub>3</sub> | 1
| ground state oxygen atom | O(3P)  | 1 |
| excited state oxygen atom | O(1D) | 1 |
| hydroxyl radical | OH | 2 |
| hydroperoxy radical | HO<sub>2</sub>| 2 |
| nitrous acid | HONO | 2
| peroxynitric acid | HNO<sub>4</sub> | 2 |
| peroxy acetyl nitrate | C<sub>2</sub>H<sub>3</sub>NO<sub>5</sub> | 3 |
| peroxy acetyl radical | C<sub>2</sub>H<sub>3</sub>O<sub>3</sub> | 3 |
| nitrate radical |  NO<sub>3</sub> | 4 |
| dinitrogen pentoxide | N<sub>2</sub>O<sub>5</sub> | 4 |

1.  Hertel et al. (1993) sorted the analytically solved species into groups. The group number denote their order. Note that Hertel et al. did not analytically solve for O(1D) in the original paper but the CMAQ EBI solver does.

<center> Table 2. 
Photochemical Mechanism Constraints.    
</center>

| Mechanism Constraint  |   Notes            |     
|:------|:------  |
| All reactions destorying O(1D) are first order | Excludes reactants that are atmospheric species held constant such as N<sub>2</sub>, O<sub>2</sub>, H<sub>2</sub>O, etc. |  
| O(1D) (+ Constant Species) ---> O(3P) present | Needed to solve Group 1 and 2  |
| O(1D) (+ H</sub>2</sub>O) ---> 2OH present  |  Needed to solve Group 1 and 2  |
| NO<sub>2</sub>           ---> NO+O(3P) present | Needed to solve Group 1 |
| N<sub>2</sub>O and excited NO<sub>2</sub> are not active in NOx cycle  | If the two species are present, their chemistry upsets the accuracy of the analytical soluton for NOx species |
| O(3P) (+ O<sub>2</sub>)    ---> O<sub>3</sub> present | Needed to solve Group 1  |
| NO + O<sub>3</sub>       ---> NO<sub>2</sub> present | Needed to solve Group 1 and 2  |
| HONO          ---> OH + NO present | Needed to solve Group 2; often a photolysis reaction  |
| OH + NO       ---> HONO present | Needed to solve Group 2; often a photolysis reaction  |
| HNO<sub>4</sub>         ---> HO<sub>2</sub> + NO<sub>2</sub> present | Needed to solve Group 2  |
| HO<sub>2</sub> + NO<sub>2</sub>     ---> HNO<sub>4</sub> present | Needed to solve Group 2  |
| HO<sub>2</sub> + HO<sub>2</sub>     --->  H<sub>2</sub>O<sub>2</sub> present | Needed to solve Group 2  |
| C<sub>2</sub>H<sub>3</sub>O<sub>3</sub> + C<sub>2</sub>H<sub>3</sub>O<sub>3</sub> ---> _products_ present | Needed to solve Group 3; products mechanism dependent  |
| Negative product coefficients are only allowed for a photochemical species named PAR | Exception made for Carbon Bond mechanisms  |


### Using create_ebi

The create_ebi utility is designed compiled and run once for each application. Beside the utility's own source code and data files, compiling needs a photochemical mechanism's data module defined by the run script (Table 3.). If compilation is successful, the utility runs based on the run script options. 

To create a new EBI solver based on photochemical mechanism's reactions data module:

1) Copy scripts/bldrun.create_ebi.csh into its parent directory. _The bldrun script assumes that value of base is {CMAQ_REPO}/UTIL/create_ebi._

2) Edit to define the FORTRAN compiler and mechanism's data module. See to Table 3 for options set by bldrun script.

3) Execute the script. _The script compiles create_ebi then runs the utility._

4) Check the OUTDIR for the code files for the ebi solver, produced.

 <center> Table 3. create_ebi environment settings or run time options </center>

 |  Names | Definition | Notes or Recommeded Value |
 |:-----|:-----|:------|
 |  COMPILER        | FORTRAN compiler to building create_ebi | the utility's makefile, _makefile.v5XX_, is step up for the Intel (INTEL), Portland Group (PGF90) and GCC gfortran (GFORT) compilers. If a separate compiler is to be used, the user has to modify the makefile to define the compiler and its compile flags, recommend including debugging flags| 
 |   RXNS_DATA_SRC  | Full path to mechanism's RXNS_DATA_MODULE.F90  | Produced by CHEMMECH utility | 
 |   TMPLDIR        | Full path to for FORTRAN templates for solver files | ${CMAQ_REPO}/UTIL/create_ebi/template_RXNSU_OPT |   
 |   DEGRADE_CODES  | Full path to FOTRAN code fors exponential decay of select air toxic. Check Table 4. | ${CMAQ_REPO}/UTIL/create_ebi/degrade_codes_serial-RXNST |   
 |   SRCDIR         | Full path to FORTRAN codes for create_ebi utility | ${CMAQ_REPO}/UTIL/create_ebi/src_RXNSU |   
 |   OUTDIR         | Full path where to write output files  | actual value is up to the user |   
 |   PAR_NEG_FLAG   | whether mechamisms has the species PAR and the species as negative production coefficients | T for Carbon Bond based mechanism but F for other mechanisms<sup>1</sup> |      
 |   DEGRADE_SUBS   | include calls for HAPs degrade routines | T |  
 |   SOLVER_DELT    | Default time step of solver in minutes | 2.5 but saprc07tic mechanism uses 1.25 | 
 |   MECH_NO        | mechanism's name for nitrogen oxide | **NO** for cb6, saprc07t and racm2<sup>2</sup> based photochemical mechanisms | 
 |   MECH_NO2       | mechanism's name for nitrogen dioxide | **NO2** for cb6, saprc07t and racm2 based photochemical mechanisms | 
 |   MECH_O3        | mechanism's name for ozone | **O3** for cb6, saprc07t and racm2 based photochemical mechanisms | 
 |   MECH_O3P       | mechanism's name for ground state oxygen atom | **O** for cb6 but **O3P** for saprc07t and racm2 based photochemical mechanisms | 
 |   MECH_O1D       | mechanism's name for excited state oxygen atom | **O1D** for cb6, saprc07t and racm2 based photochemical mechanisms | 
 |   MECH_OH        | mechanism's name for hydroyxl radical | **OH** for cb6 and saprc07t but **HO** for racm2 based photochemical mechanisms | 
 |   MECH_HO2       | mechanism's name for hydroperoxy radical | **HO2** for cb6, saprc07t and racm2 based photochemical mechanisms | 
 |   MECH_HONO      | mechanism's name for nitrous acid | **HONO** for cb6, saprc07t and racm2 based photochemical mechanisms | 
 |   MECH_HNO4      | mechanism's name for proxynitric acid | **PNA** for cb6 but **HNO4** for saprc07t and racm2 based photochemical mechanisms | 
 |   MECH_PAN       | mechanism's name for peroxy acetyl nitrate | **PAN** for cb6, saprc07t and racm2 based photochemical mechanisms | 
 |   MECH_C2O3      | mechanism's name for peroxy acetyl radical | **C2O3** for cb6, **MECO3** for saprc07t and **ACO3** racm2 based photochemical mechanisms | 
 |   MECH_NO3       | mechanism's name for nitrate radical | **NO3** for cb6, saprc07t and racm2 based photochemical mechanisms | 
 |   MECH_N2O5      | mechanism's name for dinitrogen pentoxide | **N2O5** for cb6, saprc07t and racm2 based photochemical mechanisms | 
 
 1. Negative product coefficients are only allowed for a photochemical species named PAR, common to Carbon Bond based photochemical mechanisms.
 2. The three photochemical mechanisms released in CMAQ version 5.3.
 
To report potential program errors or EBI solver failures, contact Bill Hutzell/USEPA at hutzell.bill@epa.gov

### Example Application.

#### Compile CREATE_EBI ####

After copying the bldrun_create_ebi.csh from $CMAQ_HOME/UTIL/create_ebi/scripts to another directory then editing the script to produce a solver for the saprc07tic_ae7i_aq. 

```
bldrun_create_ebi.csh
```

Execute the script (the screen output can be redirected to a log file.) 


First set of lines send to screen will echo script options and show compilation commands. Upon its execution, create_ebi writes the below lines
if the execution is successful.


   
     Value for OUTDIR: /home/hwo/CCTM_git_repository/UTIL/create_ebi/output/ebi_saprc07tic_ae7i_aq-Apr-08-2019-INTEL   
     Value for TMPLDIR: /home/hwo/CCTM_git_repository/UTIL/create_ebi/template_RXNSU_OPT   
     Value for PAR_NEG_FLAG: F returning FALSE   
     Value for DEGRADE_SUBS: F returning FALSE   
     Value for SOLVER_DELT:     1.250E+00   
     Value for MECH_NO2: NO2   
     Value for MECH_NO: NO   
     Value for MECH_O3: O3   
     Value for MECH_O1D: O1D   
     Value for MECH_O3P: O3P   
     Value for MECH_OH: OH   
     Value for MECH_HO2: HO2   
     Value for MECH_HONO: HONO   
     Value for MECH_HNO4: HNO4   
     Value for MECH_PAN: PAN   
     Value for MECH_C2O3: MECO3   
     Value for MECH_NO3: NO3   
     Value for MECH_N2O5: N2O5   


     Group species mapping results:
        nitric oxide (NO):                NO
        nitrogen dioxide (NO2):           NO2
        ozone (O3):                       O3
        ground state atomic oxygen (O3P): O3P
        excited atomic oxygen (O1D):      O1D
        hydroxyl radical (OH):            OH
        hydroperoxy radical (HO2):        HO2
        nitrous acid (HONO):              HONO
        peroxynitric acid (HNO4):         HNO4
        nitrate radical (NO3):            NO3
        nitrogen pentoxide (N2O5):        N2O5
        peroxy acetyl radical (C2O3):     MECO3
        peroxy acetyl nitrate (PAN):      PAN


     Checking mechanism for EBI solver requirements.

     No mechanism requirement problems detected - continuing.  
     Found O1D destruction in reaction #          20   
     Found O1D destruction in reaction #          21   
     Found OH production via O1D in reaction #          20  
     No HO2 production via O1D found  

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


### Input and Output files

To create an EBI solver for a photochemical mechanism to be used in the CMAQ model, a user needs the Fortran data module describing the photochemical mechanism. The chemmech utility produces the module based on a mechanism definitions file and species namelist files. For more information, consult the README under ${CMAQ_REPO}/UTIL/chemmech.

**Table 4. CREATE_EBI input files**

|**File Name**|**Format**|**Description**|
|----------------------------------|----------|----------------------------------------------------------|
|RXNS_DATA_MODULE.F90|ASCII|a Fortran 90 module describing the photochemical mechanism produced by the chemmech utility |


#### CREATE_EBI output files

A successful application produces files under a directory defined the environment variable, OUTDIR. Their number depends on run time options (Table 5.) 

**Table 5. CREATE_EBI output files**

|File Name|Format|Description|
|---------------------------------------|---------------|-------------------------------------------------------|
| hr\*.F|ASCII | Fortran files for the CCTM EBI chemistry solver|
| DEGRADE_SETUP_TOX.F<sup>1</sup> | ASCII | Fortran module that calculates exponent decay for a set of toxic air pollutants |
| init_degrade.F<sup>1</sup> | ASCII | Fortran code that initializes arrays in DEGRADE_SETUP_TOX.F |
| degrade_data.F<sup>1</sup> | ASCII | Fortran code that lists the set of toxic pollutants and their photochemical loss processes |
| find_degrade.F<sup>1</sup> | ASCII | Fortran code that search for the toxic pollutants in species namelists |
| degrade.F<sup>1</sup> | ASCII | Fortran code that calculated the exponental decay for the toxic pollutants |
| final_degrade.F<sup>1</sup> | ASCII | Fortran code that updates CGRID array |

1. Produced if DEGRADE_SUBS equals T.

Compiling a version of the CMAQ model using this photochemical mechanism and its EBI solver. A user has two options. 

 1. update the source code ( _the two photochemical reaction modules produced by CHEMMECH and EBI solver files_) and Makefile in an existing CMAQ build directory. 
 2. Add the new mechanism and solver files to their CMAQ repository then use the cctm build-it script to build the CMAQ CCTM model. 
 
The latter option is more complicated because it creates subdirectories under `$CMAQ_HOME/CCTM/src/MECHS` and `$CMAQ_HOME/CCTM/src/gas` and involve files not produced by the user and other utilities. 


## References.

Hertel O., Berkowicz R., Christensen J., and Hov O. (1993).  Test of Two Numerical Schemes for Use in Atmospheric Transport-Chemistry Models. Atmospheric Environment, Vol. 27A, No. 16, 2591-2661.
