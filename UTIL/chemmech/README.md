# CHEMMECH, the CMAQ photochemical mechanism processor

## Table of Contents

1.  [Background](#background)
2.  [Using CHEMMECH](#using-chemmech)
    -  [CHEMMECH Inputs](#chemmech-inputs)
3.  [Using output files in the CMAQ model](#using-output-files-in-the-cmaq-model)
    -  [Output Files for F0AM Box Modeling](#using-output-files-in-the-cmaq-model)
4.  [Option for Elemental Balance Check](#option-for-elemental-balance-check)
5.  [Chemical Reactions Input Format](#chemical-reactions-input-format)
    - [General rules](#general-rules)
    - [Defining Elements](#defining-elements)
    - [Format of Specific Blocks](#format-of-specific-blocks)
        - [Mechanism Name](#mechanism-name)
        - [SPECIAL](#special)
        - [ELIMINATE](#eliminate)
        - [REACTIONS](#reactions)
        - [CONSTANTS](#constants)
        - [FUNCTIONS](#functions)
6.  [Debugging CHEMMECH](#debugging-chemmech)
7.  [Reporting errors or problems with CHEMMECH](#reporting-errors-or-problems-with-chemmech)
8.  [References](#references)

<a id=background></a>
##  Background

The chemical mechanism processor (CHEMMECH) allows altering a photochemical mechanisms or using a different mechanism in the CMAQ model. 

Two output files implement the photochemical mechanism in CMAQ and are compiled along its source code. Both output files contain FORTRAN 90 modules. RXNS_DATA_MOD.F90 defines the mechanism species, their reactions and rate constants. RXN_FUNC_MOD.F90 specifies functions that map CMAQ model species to photochemical mechanism species and calculate reaction rate constants. CHEMMECH produces additional output files to check whether the two modules represent the photochemical mechanism intended by the user.  One additional ouput file, SPCS.ext, lists the species participating in the photochemical mechanism. 

Other output files support using the Kinetic PreProcess (KPP) (Damian et al., 2002), document the input data, and aid using the input's chemical mechanism in F0AM box model (Wolfe et al., 2016). The KPP file ares prototypes for the species and equations files used to run the program. They have not been tested in several years so a user should use them with discretion. Documentation files are markdown, csv and, html tables that list reactions, their rate constant formula, and values at specified atmospheric conditions. 

<a id=using-chemmech></a>
## Using CHEMMECH

The UTIL/chemmech directory includes a scripts subdirectory with a template build and run scripts along for creating the RXNS_DATA_MODULE.F90 and RXNS_FUNC_MODULE.F90 files. 

To create mechanism modules:

1.  Compile CHEMMECH by modifying the build script in the scripts directory. The syntax is ./bldit_chemmech.csh [compiler]
    - Set the Fortran compiler based on your system, save and execute the script. The script runs a Makefile set for three compilers: Intel (intel), Portland Group (pgi), and gcc gfortran (gcc). If a user wishes to use a compiler outside this group, they have to modify the Makefile under the src subdirectory.

2.  Modify the run script by setting the Photochemical Mechanism to use. Table 1 lists run time options.

3.  Execute the run script and inspect the results under the output directory.

Two methods exist for compiling CHEMMECH. The standard method executes the bldit_chemmech.csh script. The other compiles CHEMMECH via the Makefile under build directory created by bldit_chemmech.csh or under the CMAQ repositories UTIL/chemmech/src directory. A user may want to recompile if they are modifying the CHEMMECH source code or wish to use a different compiler from a previous application. When modifying and recompiling CHEMMECH, users should remove the existing object and module files by using the _make clean_ command because the Makefile does not include file dependencies for creating object files.

Running CHEMMECH is accomplished by the run script under the scripts subdirectory. In the run script, environment variables define names, directories and runtime options. The script contains comments describing each variable such as paths for the inputs, outputs, and the CHEMMECH executable. The option, USE_SPCS_NAMELISTS states whether CHEMMECH reads model species namelists to checks whether the mechanism definitions file has a chemistry species not found in the namelists. CHEMMECH will stop when this occurs. Running CHEMMECH using the namelists is not required but the option provides a check when modifying an existing photochemical mechanism within the CMAQ model system. A user may want to set USE_SPCS_NAMELISTS  to false, F,  if they are creating a new photochemical mechanism. Read the [Option for Elemental Balance Check](#option-for-elemental-balance-check) section for the run script's COMPUTE_DELTA_ATOMS option.

 <center> Table 1. CHEMMECH environment settings or run time options </center>

| Variable | Description | Notes |   
|:---------|:-----------|:-------|   
| USE_SPCS_NAMELISTS | Use CMAQ species namelists to check if photochemistry is defined in namelists | CHEMMECH stops if photochemical species not found |
| MECHDEF | Full Path for Mechanism Definitions File | Soft naming convention for mech_*MECHANISM-NAME*.def; the file's directory set by value of Mpath |
| MAPPING_ROUTINE | Full path for a FORTRAN subroutine that determines a map between CMAQ model species and photochemistry species | RXNS_FUNC_MODULE.F90 incorporates the subroutine |  
| gc_matrix_nml | Full path for gas namelist for mechanism | In general, the list contains all gases species in the photochemistry |
| ae_matrix_nml | Full path for aerosol namelist for mechanism | Allows reaction involving gas and aerosol model species |
| nr_matrix_nml | Full path for non-reactive namelist for mechanism | In general, photochemistry does not use non-reactive species despite that they are gases |
| tr_matrix_nml | Full path for tracer namelist for mechanism | Recommend not using tracer species in photochemistry to preserve the intent for tracer species |
| OUTDIR        | directory for output files | defined by value of Opath |
| SPCSDATX      | Full path for output file listing photochemistry species found in Mechanism Definitions File | can support contructing species namelist if USE_SPCS_NAMELISTS File is F |
| RXNS_DATA_MODULE  | Full path for output RXNS_DATA_MODULE.F90, the mechanism data module | Used to compile a version of CMAQ that use the photochemistry in the  Mechanism Definitions File |
| RXNS_FUNC_MODULE  | Full path for output RXNS_FUNC_MODULE.F90, the mechanism function module | Used to compile a version of CMAQ that use the photochemistry in the  Mechanism Definitions File |
| EQNS_KPP_FILE  | Full path for equations file to run the Kinetic Preprocessor (KPP) tool | Based on the MECHDEF content and not tested |
| SPCS_KPP_FILE  | Full path for species file to run the KPP tool | Based on the MECHDEF content and not tested |
| COMPUTE_DELTA_ATOMS | Rewrite mechanism definitions file to append reactions with change in tracked atoms| Default is False |
| NAMELISTS_LIST_ATOMS | For atoms or elements composing chemistry species, read comments trailing each species definition in species namelist.  | Default is True but only used if COMPUTE_DELTA_ATOMS is True. |
| ATOMS_FILE | Full path to file separate from species namelists. It gives composition of each chemistry but can include other model species. | Only read if NAMELIST_LAST_ATOMS is False. |

<a id=chemmech-inputs></a>
### CHEMMECH Inputs

Input files include a mechanism chemical definitions (MECHDEF or mech.def) file and CMAQ species namelist files. All are ASCII files. Namelists specify species participating in photochemical reaction divided into the Gas (GC), Aerosol (AE), Nonreactive (NR) and Tracer (TR) groups but not all namelist species have to participate in photochemical reactions. The namelist are optional but are recommended when modifying an existing photochemical mechanism because CHEMMECH will check whether species used in the mech.def exist in the namelists. The mech.def file lists the reactions and other data representing the photochemistry. Input files follow a rigid format; the CCTM/src/MECHS subdirectories contain examples. Rules for the mech.def are more difficults to interpret. Read the subsection on [_Chemical Reactions Input Format_](#chemical-reactions-input-format) for more information. 

<a id=using-output-files-in-the-cmaq-model></a>
## Using output files in the CMAQ model 

Outputs includes two FORTRAN 90 modules, RXNS_DATA_MODULE.F90 and RXNS_FUNCTION.F90, for compiling the CMAQ Chemical Transport Model (CCTM) to use the photochemical mechanism. Compiling the CCTM requires no additional files if the model uses the Sparse Matrix Vectorized versions of the Rosenbrock (Sandu et al., 1997) or Gear (Jacobson and Turco, 1994) chemistry solver (repository directories, CCTM/src/gas/ros3 or CCTM/src/gas/smvgear). 

Besides the species namelists, executing the CCTM requires a CSQY_DATA\_**mechanism-name** file containing cross-sections and quantum yields for the photolysis rates used by the mechanism. The _inline_phot_preproc_ utility creates file by using the data module. Check the subdirectory containing this utility for more information. 

If a user wants to use a gas chemistry solver faster than Rosenbrock or Gear, they have to create a Euler Backward Interative (EBI) solver (Hertel et al., 1993) for the photochemical mechanism. The _create\_ebi_ utility creates an EBI solver specific to a photochemical mechanism by using its data module. Check this utility's subdirectory for more information.

### Output Files for F0AM Box Modeling

The output files for running the FOAM boxmodel are listed below.

1. **mechanism_name**_AllRxns.m defines the mechanism species, reactions and their rate constants.
2. **mechanism_name**_J.m sets the mechanism's photolysis frequencies. Note that F0AM provides a fixed set to available photolysis frequencies so a user has to map the mechanism's frequencies to the available frequencies. If the user wishes to use photolysis frequencies native to the mechanism, they have to perform the below steps.

    - Add each frequency' cross-section and quantum yield files to the relevant subdirectories of the F0AM box-model.   
    - Modify the file, Chem/Photolysis/J_BottomUp.m, in the F0AM code.    
    - Run Chem/Photolysis/calc_HybridJtables.m. in the FOAM code.
    
3. J_BottomUp_insert_**mechanism_name**.m contains a list  of the datafiles need to accomplish the above task. The file can also be used to modify Chem/Photolysis/J_BottomUp.m.
4. **mechanism_name**_K.m sets the heterogeneous reaction rate constants. The file sets their values to zero because CCTM/src/aero/aero6/AEROSOL_CHEMISTRY.F calculates the values in CCTM and is not controlled by the CHEMMECH utility. 

<a id=option-for-elemental-balance-check></a>
## Option for Elemental Balance Check.

The CHEMMECH utility has run-time option revising the input mechanism definitions file to show how each reaction changes the balance of elements such as carbon, nitrogen, and sulfur. If a reaction does not balance the initial and final number of elements, the revised file appends the reaction with the variables and coefficients measuring the unbalance. For each unbalance _element_, the added variable is called DELTA\__element_. DELTA\__element_'s are not active chemistry species so CHEMMECH does not output information to solve how the DELTA\__element_'s evolve over time. The option places the revised mechanism definitions file into the output directory. It also produces a diagnostic file called _mechanism-name_\_reactions\_deltas.dat that shows calculations for each reaction's DELTA\__element_'s.

Two methods obtain information used to calculate the DELTA\__element_'s. Both methods are set by the _NAMELISTS\_LIST\_ATOMS_ option.

When _NAMELISTS\_LIST\_ATOMS_ equals true, the model species namelists give the information as comments at the end of lines defining model species. The comments use comma deliminated format to convey the information. In CMAQ version 5.4, the species namelists for cracmm1_aq show these trailing comments. Table 2 lists items in the comments' information. Note that CHEMMECH only uses the SMILES information and only recognizes a subset of chemical elements (Ca, Mn, Cl, Hg, Br ,Na, Si, S, Ti, Fe, K, I, N, and C). To allow users to determine whether the SMILEs are read correctly, a diagnostic file lists the information captured from the comments and the interpreted species compositions. The file is called atom_counts\__mechanism-name_\_species.dat. 

<center> Table 2. Contents in Namelist Trailing Comments </center>

| Order in Content | Description | Notes |   
|:---------|:-----------|:-------|
| Representative Compound | Compound determining chemical and physical properties of the model species | Impacts several model process such as photolysis and deposition |
| E or L | Is the model species an explicit compound and Lumped (aggregrate) of several compounds? | Lumped infers averaging assumptions in the species' chemistry |
| DTXSID | Label identifies the species in Distributed Structure-Searchable Toxicity database of the US EPA | Also found at the US EPA's CompTox Chemistry Dashboard |
| SMILES | Character String describing the chemical structure and composition | **Upper case required for determining elemental composition** | 

When _NAMELISTS\_LIST\_ATOMS_ equals False, an ASCII file defined by the ATOMS_FILE environment variable contains the information in comma deliminated format. The file gives the information in one of two ways. One way, a SMILES table, uses the content listed in Table 2 and is illustrated in Figure 1.

<center> Figure 1. ATOMS_FILE using a SMILES table </center>

     !species information 
     SPECIES       ,RepCmp,ExplicitorLumped,DTXSID,SMILES
     O3            ,Ozone,E,DTXSID0021098,[O-][O+]=O
     O3P           ,Ground state oxygen,E,DTXSID00170378,[O]
     O1D           ,Excited oxygen,E,DTXSID00170378,[O]
     H2O2          ,Hydrogen peroxide,E,DTXSID2020715,OO
     HO            ,Hydroxyl radical,E,NA,[OH]
     NO2           ,Nitrogen dioxide,E,DTXSID7020974,N(=O)[O]
     NO            ,Nitric oxide,E,DTXSID1020938,[N]=O
     NO3           ,Nitrate radical,E,NA,[O]N(=O)=O
     HONO          ,Nitrous acid,E,DTXSID7064813,N(=O)O
     HNO3          ,Nitric acid,E,DTXSID5029685,[N+](=O)(O)[O-]
     HNO4          ,Hydroxy nitrate,E,DTXSID201030501,[N+](=O)([O-])OO
     HO2           ,Hydroperoxy,E,DTXSID30894777,O[O-] 
     HCHO          ,Formaldehyde,E,DTXSID7020637,C=O
     CO            ,Carbon monoxide,E,DTXSID5027273,[C-]#[O+]
     ACD           ,Acetaldehyde,E,DTXSID5039224,CC=O
     MO2           ,Methylperoxy,E,DTXSID10944007,CO[O]
     ALD           ,Propanal,L,DTXSID2021658,CCC=O
     ETHP          ,Ethylperoxy,L,DTXSID90953652,CCO[O]
     ACT           ,Acetone,E,DTXSID8021482,CC(C)=O
     ACO3          ,Acetylperoxy,E,DTXSID40957943,CC(=O)O[O]


The second way, a brakedown table, brakes down each chemistry species versus elements listed in the ATOM_FILE's header line as in Figure 1. Different ATOMS_FILEs can list different number and order of chemical elements.

<center> Figure 2. ATOMS_FILE using a species brakedown table </center>

    Species     ,      N,  S, CL
    NO2         ,   1.0, 0.0,0.0
    NO          ,   1.0, 0.0,0.0
    O           ,   0.0, 0.0,0.0
    O3          ,   0.0, 0.0,0.0
    NO3         ,   1.0, 0.0,0.0
    O1D         ,   0.0, 0.0,0.0
    OH          ,   0.0, 0.0,0.0
    HO2         ,   0.0, 0.0,0.0
    H2O2        ,   0.0, 0.0,0.0
    N2O5        ,   2.0, 0.0,0.0
    HNO3        ,   1.0, 0.0,0.0
    HONO        ,   1.0, 0.0,0.0


<a id=chemical-reactions-input-format></a>
## Chemical Reactions Input Format <a name="Chemical Reactions Input Format"></a>

A mech.def file follows formatting rules based on Gery and Grouse (1990) and Jeffries (1990) but the rules have evolved along with the CMAQ model.  The file consists six sequential blocks: a mechanism name, operator definitions, an ignored species list, the reactions list, constant and functions definitions. Only the reaction list is required. Blocks after the mechanism name begin with the respective key words, SPECIAL, ELIMINATE, REACTIONS, CONSTANT, and FUNCTIONS. These blocks terminate with _END_ or _end_ (case sensitive). Their content also follows the below rules and use the same elements to define information.

Each block will be discussed separately below but first the discussion lists rules and elements for entering the mechanism data.

<a id=general-rules></a>
### General rules. <a name="General rules"></a>

* Data Lines lie between columns 1 and 80.  
* White spaces are ignored.  
* Data lines can wrap around (i.e., entries can be continued on a subsequent line after a hard return).  
* Lines beginning with an exclamation point contain a comment line.  
* Data lines cannot contain a comment line.   
* Text enclosed by _{}_ or _()_ contain comments within a data line.

<a id=defining-elements></a>
### Defining Elements. <a name="Defining Elements"></a>

1.  Species Types and Naming Rules.  
    1.  Constants species whose names and volume mixing ratio are fixed. Names include M (any molecule in the atmosphere), O2 (oxygen), CH4 (methane), H2 (hydrogen), N2 (nitrogen), and H2O (water vapor).  Reactions use constant species to calculate rate constants by including them as reactants. Their names cannot be used to represent other species.  
    2.  Model species are produced or destroyed by the mechanism's reactions. Species namelist define them. Their names satisfy the below rules.  
        1.   Do not contain blanks and can be up to 16 characters long. However, the maximum length is recommended to equal 13 if the mechanism is to be used in the DDM version of the CMAQ model.  
        2.   Must begin with an alphabetic character but may contain any alphanumeric character (i.e., "A-Z," "a-z," and "0-9") or the characters ":" and "\_" after the first position.  
        3.   Changing case changes the species so NO2 and no2 represent two different model species.  
        4.   Using embedded comments in a species name cannot span two lines.  
2.  Labels are used to define or refer to reactions, operators or other processes.  
    1.   Often start with "<" and ends with ">".  
    2.   Contain up to 16 non-blank characters long.  
    3.   Cannot contain a comment or a label delimiter.   
    4.   A label may span lines.  
3.  Numbers can be read as the below types.  
    1.  Integer (e.g., 5)  
    2.  Floating point (e.g., 5.0)  
    3.  Exponential (e.g., 5.0E+00).   
    4.  With the exponential format, the "E" may be either upper or lowercase; a positive exponent is assumed if the sign of the exponent is missing.  
    5.   All numbers are read in as REAL(4) FORTRAN types but may converted to REAL (8) FORTRAN type in output files.  

<a id=format-of-specific-blocks></a>
### Format of Specific Blocks
<a id=mechanism-name></a>
#### Mechanism Name.

The mechanism name is an optional input. If it is included, it must be the first non-comment entry in the mechanism. Rules for the mechanism name are the same as those for species names except that it can be up to 32 characters long.   No delimiter is required to end of the name but a "hard return" after the entry is suggested for creating a legible input file.
<a id=special></a>
#### SPECIAL.

The key word SPECIAL lists operators used in the REACTIONS block to express reaction rate constant. Operators combine reaction rate constants and species concentrations. A mechanism often uses them to lump reactions together with an already defined rate constant. An example shows an operator called RY is derived from two following reactions with respective rate constants, RKA RKB, RKH, and RKI used in a photochemical mechanism.

        X + Y = 0.3*Z
        U + Y = 0.5*W
        H + Y = 0.2*M
        I + Y = 0.7*N

The reactions can be represented as the below reaction with the effective rate constant, RY, where X and U concentrations are values at the beginning of the integration time step of the chemistry solver.

         Y = Z + W
         Y = M + N

RKXU and RKHI equal the weighted values of RKA\*X plus RKB\*W and RKA\*X plus RKB\*I. The SPECIAL block expresses RY with formula.

       SPECIAL =
          RKXU = 0.3*K<RKA>*C<X> + 0.5*K<RKB>*C<U>;
          RKHI = 0.2*K<RKH>*C<H> + 0.7*K<RKI>*C<I>;
        end special

 Operator definitions allow using an already defined expression so for example the RKZ operator can use RKXU and RKHI in its definition.

      SPECIAL =
         RKXU = 0.3*K<RKA>*C<X> + 0.5*K<RKB>*C<U>;
         RKHI = 0.2*K<RKH>*C<H> + 0.7*K<RKI>*C<I>;
         RKZ  = RKXU + RKHI;
      end special

The value of RKZ corresponds to the rate constant for the below reaction. 

      Y = Z + W + M + N

Check Reaction Type 11 in __Table 2__ on to how access on
an operator defined in the SPECIAL block. For an example of a CMAQ photochemical mechanism that uses the SPECIAL block, examine the mechanism
definitions of the saprc07tb_ae6_aq mechanism in CMAQ version 5.2.

Operators can increase computational efficiency for solving concentrations. Using them assumes that the concentrations used
are constant over subtime steps within the photochemical solver. Mechanism developers should test the assumption before commiting 
to them by comparing two mechanisms that do and do not use such operators.

<a id=eliminate></a>
#### ELIMINATE.


This key word followed by an equal sign lists products used in reactions that are not to be included as a model species or accounted for in CHEMMECH output files. A semicolon must follow each species name in the list. A developer may want to omit specific products because they lack relevance to research goals or because solving their concentrations greatly increases duration of model simulations.

<a id=reactions></a>
#### REACTIONS.

The key word proceeds the list of reactions in the mechanism. Only the first four characters (i.e., REAC) are actually required. The key word is followed closed brackets and an equal sign. The bracket's enclosure indicates units for rate constants. Allowed enclosures "PP" and "CM," ppm-min units and molecule-cc-sec units, respectively. Either enclosure is case insensitive. A delimiter is not required after the equal sign but a "hard return" after the entry is suggested for clarity of the input file. Examples of valid inputs include the following:

                           REAC[PP]=
                           RE ACTIONS [CM]=
                           REACTIONS[ppm]=
                           REAC[cms]=

Individual reactions lines consist of the following: 1) an optional label, 2) up to 3 reactants 3) an equal sign (=) to separate reactants from products, 4) up to 40 products with optional numerical coefficients, 5) a reaction rate constant in one of the prescribed formats, and 6) an ending semicolon (;). Because line wrapping is allowed, a single reaction can span multiple lines in the input file. A reaction has the below generic format where brackets denotes optional content.

            [<label>]    reac1,[+reac2[+reac3]] = [±[p,*]prod1, [±[p2*]prod2 [... ± [p3*]prod3]]]  RKI;

-	label names the reaction    
-	reacn defines the nth reactant                                           
-	prodn defines the nth product   
-	pn gives the stoichiometric coefficient of the nth product   
-	RKI defines type and parameters of the rate constant   

Each of the components of the reaction is described below:

Reaction labels are optional but are recommended because they can serve as references to define rate constant for following reactions or support Integration Rate Analysis within Process Analysis.

A reaction can have a maximum number of three reactants. Stoichiometric coefficients are not allowed for reactants and are set to 1.0. Note that if a constant species name is used as a reactant, the output file factors its concentration is factored into the rate constant.

Products consist species names separated by plus (+) or minus (-) signs with optional numerical coefficients. As noted above, a reaction can have up to 40 products. Stoichiometric coefficients use the number formats mentioned above and must be separated from the species names by an asterisk(*).

Rate constant parameters begin with either a # sign or the expression, "%s#", where s equal 1, 2, 3, or H. The following characters and numbers specify parameters to calculate the reaction's rate constant. Table 2 defines formats corresponding to the available formulas. A semi-colon (;) denote the end of a reaction's definition.   

<center>  Table 3.  </center>

| Type | Mechanism Definition File Expression| Formula, where M is air number density (molecules/cm3), T is air temperature(degrees K), and P is air pressure (Atm) |
|:---:|:-------------------:|:---:|
| -1   | #  A\\< HETEOROGENOUS>  | A\*H |
| 0   | #  A\\< PHOTOLYSIS>  | A\*J |
| 1   | #  A               | A   |
| 2   | #  A \^B             | A\*(T/300)\*\*B |
| 3   | #  A@C             | A\*EXP(-C/T) |
| 4   | #  A\^B@C           | A\*(T/300)\*\*B\*EXP(-C/T) |
| 5   | #  A@C\*E\<REACTION\> | K\*EXP(C/T)/A |
| 6   | #  A*K\<REACTION\>   | A\*K |
| 7   | %1  #  A           | A\*(1+0.6\*P) |
| 8   | %2  #  A0@C0&A2@C2&A3@C3       | k0  +  k3\*M/(1+k3/k2) where  k0  =  A0\*exp(-C0/T),  k2  =  A2\*EXP(-C2/T),  and  k3  =  A3\*EXP(-C3/T)  |
| 9   | %3  #  A0@C0&A1@C1             | A0\*EXP(-C0/T)+A1\*EXP(-C1/T)\*M |
| 9.1 | %3  #  A0^B0@C0&A1\^B1@C1&A2@C2 | A0\*(T/300)\*\*B0\*EXP(-C0/T)+A1\*(T/300)\*\*B1\*EXP(-C1/T)\*M+A2\*EXP(-C2/T) |
| 10  | #  A0^B0@C0&A1\^B1@C1&N&F       | [  ko\*M/(1+ko\*M/kinf)]F\*\*G  where  ko  =  A0\*(T/300)\*\*B0\*EXP(-C0/T),  kinf  =  A1\*(T/300)\*\*B1\*EXP(-C1/T)  and  G  =  G=1/[1+(log10(k0\*M/kinf)/n)**2)]  |
| 11  | #A?OPERATOR                    | A\*O |
| 12  |  %H  #  A0@C0&A1@C1&A2            | min(A0\*EXP(-C0\*P)+A1\*EXP(-C1\*P), A2)  if  the  sun  is  above  the  horizon  and  open  water  plus  surf  zone covers the surface by more than 0.1\%.  0.0  if  otherwise |
| 13  | %4 # _Text String_     | Simple Fortran formula for rate constant |

**Notes:**
1.  For rate constants with the form A\<Reference\> or A\*Reference, reference gives label for a photolysis rate (J), a heteorogeneous rate constant (H), rate constant for the given (K) reaction label or an operator (O). A equals one if not given.
2.  Reaction Type 5 represents the rate constant for the reverse equilibrium reaction to the reaction label specified between the brackets.
3.  Calculating the photolysis and heteorogeneous rates takes place outside the RXNS_FUNC_MODULE.F90 file produced by the CHEMMECH processor.
4.  Operators are defined the SPECIAL block where <''REACTION''> is the rate constant for the given ''REACTION'' and [''species''] equals the concentration of a mechanism ''species'' at the beginning of the integration time-step for the chemistry's n
5.  Type 12 is used to include ozone destruction by marine bromine and iodide compounds. It parameterizes effects from by a photochemical mechanism that includes such compounds.
6.  Type 13 can use TEMP (K), PRES (atm), and the constant atmospheric species (molec/cm\*\*3). They also can use function and operator defined in the __FUNCTIONS__ and __SPECIAL__ blocks.

<a id=constants></a>
#### CONSTANTS

The key word defines volume mixing ratio of the subset of fixed list of constant species as below.

      CONSTANTS
       < C1> ATM_AIR = 1.0E+06
       < C2> ATM_H2   = 0.56
       < C3> ATM_N2   = 0.7808E+06
       < C4> ATM_O2   = 0.2095E+06
       < C5> ATM_CH4 = 1.85
       end constants

The values have units of parts per million. ATM_AIR equals the mixing ratio of M, any gas molecule. The block does not define the mixing ratio for H2O because the meteorological input data specific their values so the values depend on time and location.

<a id=functions></a>
#### FUNCTIONS
    
The FUNCTIONS block defines formulas for calculating rate constant that can used by reaction Type 13. They are limited to one line in lenght but can reference preceeding formulas within the FUNCTIONS block. They can use TEMP (K), PRES (atm), and the constant atmospheric species (molec/cm\*\*3). The syntax obeys
FORTRAN mathematical expressions.

The below lines gives an example based on the CRI mechanism version version 2.1 (Jenkin et al., 2008 and Watson et al., 2008).

        FUNCTIONS
         KD0 = 4.90D-3*EXP(-12100/TEMP)*M;
         KDI = 5.4D+16*EXP(-13830/TEMP);
         KRD = KD0/KDI;
         FCD = 0.30;
         NCD = 0.75-1.27*(LOG10(FCD));
         FD = 10**(LOG10(FCD)/(1+(LOG10(KRD)/NCD)**2));
         KBPAN = (KD0*KDI)*FD/(KD0+KDI);
         KC0 = 2.7D-28*M*(TEMP/300)**-7.1;
         KCI = 1.2D-11*(TEMP/300)**-0.9;
         KRC = KC0/KCI;
         FCC = 0.30;
         NC = 0.75-1.27*(LOG10(FCC));
         FC = 10**(LOG10(FCC)/(1+(LOG10(KRC)/NC)**2));
         KFPAN = (KC0*KCI)*FC/(KC0+KCI);
         KMT06 = 1 + (1.40D-21*EXP(2200/TEMP)*H2O);
        END FUNCTIONS

Use reaction type 13 to access the value of a formula expressed in the __FUNCTIONS__ block.

        <R22>  HO2 + HO2    = H2O2          %4 # 2.20D-13*KMT06*EXP(600/TEMP);
        <R348> CH3CO3 + NO2 = PAN           %4 # KFPAN;
        <R721> PAN          = CH3CO3 + NO2  %4 # KBPAN;        

<a id=debugging-chemmech></a>
## Debugging CHEMMECH 

CHEMMECH's Makefile includes debug flags in the compiler option so the user can identify the cause and location when CHEMMECH crashes. Crashes often occur if the mech.def contains information exceeding the parameters defining array dimensions. The UTIL/chemmech/src/MECHANISM_PARMS.f file defines these parameters. The user can change many of the parameters then re-compiled CHEMMECH via the Makefile under build directory so the CHEMMECH fits the application. Table 3 lists the parameters.

<center> Table 3. Limits placed on a Mechanism Definitions File </center>

| Parameter	| Value |     Description |    
|:-----|:----:|:-----------|           
| MAXRXNUM    |  	2000 | Maximum Reactions |    
| MAXSPEC     |  	700 | Maximum Photochemical Species |
| MAXPRODS    |  	40 | Maximum Products per Reaction |
| MAXRCTNTS    | 	3 | Maximum Reactants per Reaction |
| MAXPHOTRXNS  | 	600 | Maximum Photolysis or Heteorogeneous Rates and Reactions of Type -1 or 0 |
| MAXSPECRXNS  | 	600  | Maximum Number of Operators in SPECIAL Block and Type 11 Reactions  |
| MAXFUNCTIONS 	|  6 * MAXRXNUM | Maximum Number of Functions in FUNCTION Block and Reactions using them |
| MAXSPECTERMS | 	 MAXSPEC | Maximum Number of Terms used to define an operator in the SPECIAL BLOCK |
| MAXFALLOFF  |  	150 | Number Number of Pressure Dependent Reactions, Type 8 through 10 |   
| MAX3BODIES |   	150 | Number Number of Reactions using CONSTANT Species |
| MAXWRDLEN  |   	16 | Maximum Character Length |
| MAXCONSTS  |   	5 |  Maximum CONSTANT Species |

<a id=reporting-errors-or-problems-with-chemmech></a>
##  Reporting errors or problems with CHEMMECH

If errors occur at running CHEMMECH, check the _Compiling and debugging CHEMMECH_ subsection for possible solutions. Otherwise,  report potential program errors or failures, contact Bill Hutzell/USEPA at hutzell.bill@epa.gov. 

<a id=references></a>
## References.

Damian V., Sandu A., Damian M., Potra F., Carmichael G.R. (2002). The kinetic preprocessor KPP - A software environment for solving chemical kinetics. Computers and Chemical Engineering,  26(11) , pp. 1567-1579.

Gery, M.W. and Crouse, R.R. (1990) User's Guide for Executing OZIPR, EPA/6008-90, U.S. Enivironmental Protection Agency, Research Triangle Park, 27711, NC.

Hertel O., Berkowicz R., Christensen J., and Hov O. (1993).  Test of Two Numerical Schemes for Use in Atmospheric Transport-Chemistry Models. Atmospheric Environment, Vol. 27A, No. 16, 2591-2661.

Jacobson M.Z. and Turco R.P. (1994). SMVGEAR: A sparse-matrix, vectorized gear code for atmospheric models. Atmospheric Environment, Volume 28, Issue 2, Pages 273-284,

Jefferies, H.E. (1990) User Guide to Photochemical Kinetics Simulation System PC-PKSS Software Version 3, Chapel Hill, NC 27514.

Jenkin M.E., Watson L.A., Shallcross D.E., Utembe S.R. (2008). A Common Representative Intermediate (CRI) mechanism for VOC degradation. Part-1: gas phase mechanism development
Atmos. Environ., 42, pp. 7185-7195

Sandu A., Verwer J.G, Blom J.G., Spee E.J., Carmichael G.R. and Potra F.A (1997). Benchmarking stiff ODE solvers for atmospheric chemistry problems II: Rosenbrock solvers. Atmospheric environment 31 (20), 3459-3472.

Watson L.A., Shallcross D.E., Utembe S.R., Jenkin M.E. (2008). A Common Representative Intermediates (CRI) mechanism for VOC degradation. Part 2: Gas phase mechanism reduction
Atmospheric Environment, 42 (31) , pp. 7185-7193

Wolfe G. M. , Marvin M. R., Roberts S. J., Travis K. R., and J. Liao K. R. (2016). The Framework for 0-D Atmospheric Modeling (F0AM) v3.1, Geoscientific Model Development, 9, 3309-3319, doi: 10.5194/gmd-9-3309-2016.
