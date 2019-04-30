# chemmech, the CMAQ photochemical mechanism processor


##  Background

The chemical mechanism processor (chemmech) allows altering a photochemical mechanisms or using a different mechanism in the CMAQ model.
Two output files implement the photochemical mechanism in CMAQ and are compiled along its source code. Both output files contain FORTRAN 90 modules. RXNS_DATA_MOD.F90 defines the mechanism species, their reactions and rate constants. RXN_FUNC_MOD.F90 specifies functions that map CMAQ model species to photochemical mechanism species and calculate reaction rate constants. chemmech produces additional output files to check whether the two modules represent the photochemical mechanism intended by the user.  One additional ouput file, SPCS.ext, lists the species participating in the photochemical mechanism. Two other additional output files are prototypes for the species and equations files used to run the Kinetic PreProcess (KPP) (Damian et al., 2002).  The KPP inputs have not been tested in several years so a user should use them with discretion.
Remaining output files are markdown, csv and, html tables that list reactions, their rate constant formula, and values at specified atmospheric conditions.

## Using chemmech

This directory includes a scripts subdirectory with a template build and run scripts along for creating
the RXNS_DATA_MODULE.F90 and RXNS_FUNC_MODULE.F90 files. 

To create mechanism modules:

1.  Compile chemmech by modifying the build script in the scripts directory. 
    - Set the Fortran **COMPILER** based on your system, save and execute the script. The script runs a Makefile set for 
    three options of compilers: Intel (INTEL), Portland Group (PGF90), and gcc gfortran (GFORT). If a user wishes to use a compiler
    outside this group, they have to modify the Makefile under the src subdirectory.

2.  Modify the run script by setting the Photochemical Mechanism to use. Table 1 lists run time options.

3.  Execute the run script and inspect the results under the output directory.

 <center> Table 1. chemmech environment settings or run time options </center>

| Variable | Description | Notes |   
|:---------|:-----------|:-------|   
| USE_SPCS_NAMELISTS | Use CMAQ species namelists to check if photochemistry is defined in namelists | chemmech stops if photochemical species not found |
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


Inputs include a mechanism chemical definitions (MECHDEF or mech.def) file and CMAQ species namelist files.
All are ASCII files. Namelists specify species participating in photochemical reaction divided into the Gas (GC), Aerosol (AE), Nonreactive (NR) and Tracer (TR) groups but not all namelist species have to participate in photochemical reactions. The namelist are optional but are recommended when modifying an existing photochemical mechanism because chemmech will check whether species used in the mech.def exist in the namelists. 
The mech.def file lists the reactions and other data representing the photochemistry. Input files follow a rigid format; the CCTM/src/MECHS subdirectories contain examples. Rules for the mech.def are more difficults to interpret. Read the subsection on _Chemical Reactions Input Format_ for more information. 

If errors occur at running chemmech, check the _Compiling and debugging chemmech_ subsection for possible solutions. Otherwise,  report potential program errors or failures, contact Bill Hutzell/USEPA at hutzell.bill@epa.gov. 


## Using output files in the CMAQ model

Outputs includes FORTRAN 90 modules, RXNS_DATA_MODULE.F90 and RXNS_FUNCTION.F90, for compiling the CMAQ Chemical Transport Model (CCTM) to use the photochemical mechanism. Compiling the CCTM requires no additional files if the model uses the Sparse Matrix Vectorized versions of the Rosenbrock (Sandu et al., 1997) or Gear (Jacobson and Turco, 1994) chemistry solver (repository directories, CCTM/src/gas/ros3 or CCTM/src/gas/smvgear). 

Besides the species namelists, executing the CCTM requires a CSQY_DATA\_**mechanism-name** file containing cross-sections and quantum yields for the photolysis rates used by the mechanism. The _inline_phot_preproc_ utility creates file by using the data module. Check the subdirectory containing this utility for more information. 

If a user wants to use a gas chemistry solver faster than Rosenbrock or Gear, they have to create a Euler Backward Interative (EBI) solver (Hertel et al., 1993) for the photochemical mechanism. The _create\_ebi_ utility creates an EBI solver specific to a photochemical mechanism by using its data module. Check this utility’s subdirectory for more information.

## Chemical Reactions Input Format

A mech.def file follows formatting rules based on Gery and Grouse (1990) and Jeffries (1990) but the rules have evolved along with the CMAQ model.  The file consists six sequential blocks: a mechanism name, operator definitions, an ignored species list, the reactions list, constant and functions definitions. Only the reaction list is required. Blocks after the mechanism name begin with the respective key words, SPECIAL, ELIMINATE, REACTIONS, CONSTANT, and FUNCTIONS. These blocks terminate with “END” or “end” (case sensitive). Their content also follows the below rules and use the same elements to define information.

Each block will be discussed separately below but first the discussion lists rules and elements for entering the mechanism data.

### General rules.

* Data Lines lie between columns 1 and 80.  
* White spaces are ignored.  
* Data lines can wrap around (i.e., entries can be continued on a subsequent line after a hard return).  
* Lines beginning with an exclamation point contain a comment line.  
* Data lines cannot contain a comment line.   
* Text enclosed by “{}” or “()” contain comments within a data line.

### Defining Elements.

1.  Species Types and Naming Rules.  
    1.  Constants species whose names and volume mixing ratio are fixed. Names include M (any molecule in the atmosphere), O2 (oxygen), CH4 (methane), H2 (hydrogen), N2 (nitrogen), and H2O (water vapor).  Reactions use constant species to calculate rate constants by including them as reactants. Their names cannot be used to represent other species.  
    2.  Model species are produced or destroyed by the mechanism’s reactions. Species namelist define them. Their names satisfy the below rules.  
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

### Format of Specific Blocks

#### Mechanism Name.

The mechanism name is an optional input. If it is included, it must be the first non-comment entry in the mechanism. Rules for the mechanism name are the same as those for species names except that it can be up to 32 characters long.   No delimiter is required to end of the name but a "hard return" after the entry is suggested for creating a legible input file.

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

#### ELIMINATE.

This key word followed by an equal sign lists products used in reactions that are not to be included as a model species or accounted for in chemmech output files. A semicolon must follow each species name in the list. A developer may want to omit specific products because they lack relevance to research goals or because solving their concentrations greatly increases duration of model simulations.

#### REACTIONS.

The key word proceeds the list of reactions in the mechanism. Only the first four characters (i.e., REAC) are actually required. The key word is followed closed brackets and an equal sign. The bracket’s enclosure indicates units for rate constants. Allowed enclosures "PP" and "CM," ppm-min units and molecule-cc-sec units, respectively. Either enclosure is case insensitive. A delimiter is not required after the equal sign but a "hard return" after the entry is suggested for clarity of the input file. Examples of valid inputs include the following:

                           REAC[PP]=
                           RE ACTIONS [CM]=
                           REACTIONS[ppm]=
                           REAC[cms]=

Individual reactions lines consist of the following: 1) an optional label, 2) up to 3 reactants 3) an equal sign (=) to separate reactants from products, 4) up to 40 products with optional numerical coefficients, 5) a reaction rate constant in one of the prescribed formats, and 6) an ending semicolon (;). Because line wrapping is allowed, a single reaction can span multiple lines in the input file. A reaction has the below generic format where brackets denotes optional content.

            [<label>]    reac1,[+reac2[+reac3]] = [±[p,*]prod1, [±[p2*]prod2 [... ± [p3*]prod3]]]  RKI;

•	label names the reaction    
•	reacn defines the nth reactant                                           
•	prodn defines the nth product   
•	pn gives the stoichiometric coefficient of the nth product   
•	RKI defines type and parameters of the rate constant   

Each of the components of the reaction is described below:

Reaction labels are optional but are recommended because they can serve as references to define rate constant for following reactions or support Integration Rate Analysis within Process Analysis.

A reaction can have a maximum number of three reactants. Stoichiometric coefficients are not allowed for reactants and are set to 1.0. Note that if a constant species name is used as a reactant, the output file factors its concentration is factored into the rate constant.

Products consist species names separated by plus (+) or minus (-) signs with optional numerical coefficients. As noted above, a reaction can have up to 40 products. Stoichiometric coefficients use the number formats mentioned above and must be separated from the species names by an asterisk(*).

Rate constant parameters begin with either a # sign or the expression, "%s#", where s equal 1, 2, 3, or H. The following characters and numbers specify parameters to calculate the reaction’s rate constant. Table 2 defines formats corresponding to the available formulas. A semi-colon (;) denote the end of a reaction’s definition.   

<center>  Table 2.  </center>

| Type | Mechanism Definition File Expression| Formula, where M is air number density (molecules/cm3), T is air temperature(degrees K), and P is air pressure (Atm) |  
|:---:|:-------------------:|:---:|   
| -1   | #  A\\< HETEOROGENOUS>  | A\*H |  
| 0   | #  A\\< PHOTOLYSIS>  | A\*J |  
| 1   | #  A               | A   |  
| 2   | #  A \^B             | A\*(T/300)\*\*B |  
| 3   | #  A@B             | A\*EXP(-B/T) |  
| 4   |	#  A\^B@B           | A\*(T/300)\*\*B\*EXP(-E/T) |  
| 5   |	#  A@C\*E<REACTION> | K\*EXP(C/T)/A |  
| 6   |	#  A*K<REACTION>   | A\*K |  
| 7   |	%1  #  A           | A\*(1+0.6\*P) |  
| 8   |	%2  #  A0@E0&A2@E2&A3@E3       | k0  +  k3\*M/(1+k3/k2) where  k0  =  A0\*exp(-E0/T),  k2  =  A2\*EXP(-E2/T),  and  k3  =  A3\*EXP(-E3/T)  |  
| 9   |	%3  #  A0@E0&A1@E1             | A0\*EXP(-E0/T)+A1\*EXP(-E1/T)\*M |  
| 9.1 | %3  #  A0^B0@E0&A1\^B1@E1&A2@E2 | A0\*(T/300)\*\*B0\*EXP(-E0/T)+A1\*(T/300)\*\*B1\*EXP(-E1/T)\*M+A2\*EXP(-E2/T) |  
| 10  | #  A0^B0@E0&A1\^B1@E1&N&F       | [  ko\*M/(1+ko\*M/kinf)]F\*\*G  where  ko  =  A0\*(T/300)\*\*B0\*EXP(-E0/T),  kinf  =  A1\*(T/300)\*\*B1\*EXP(-E1/T)  and  G  =  G=1/[1+(log10(k0\*M/kinf)/n)**2)]  |  
| 11  | #A?OPERATOR                    | A\*O |  
| 12  |  %H  #  A0@E0&A1@E1            | A0\*EXP(-E0\*P)+A1\*EXP(-E1\*P)  if  the  sun  is  above  the  horizon  and  the  surface  is over  open  water  with  no  surf  zone.  0.0  if  otherwise |  
| 13  | %4 # _Text String_     | Simple Fortran formula for rate constant | 

**Notes:**   
1.  For rate constants with the form A<Reference> or A*Reference, reference gives label for a photolysis rate (J), a heteorogeneous rate constant (H), rate constant for the given (K) reaction label or an operator (O). A equals one if not given.
2.  Reaction Type 4 represents the rate constant for the reverse equilibrium reaction to the reaction labeled K.  
3.  Calculating the photolysis and heteorogeneous rates takes place outside the RXNS_FUNC_MODULE.F90 file produced by the chemmech processor.   
4.  Operators are defined the SPECIAL block where <''REACTION''> is the rate constant for the given ''REACTION'' and [''species''] equals the concentration of a mechanism ''species'' at the beginning of the integration time-step for the chemistry's numerical solver.   
5.  Type 12 is used to include ozone destruction by marine bromine and iodide compounds. It parameters effects from reactions predicted by a photochemical mechanism that includes them.  
6.  Type 13 can use TEMP (K), PRES (atm), and the constant atmospheric species (molec/cm\*\*3). They also can use function and operator defined in the __FUNCTIONS__ and __SPECIAL__ blocks.

#### CONSTANTS.

The key word defines volume mixing ratio of the subset of fixed list of constant species as below.

      CONSTANTS
       < C1> ATM_AIR = 1.0E+06
       < C2> ATM_H2   = 0.56
       < C3> ATM_N2   = 0.7808E+06
       < C4> ATM_O2   = 0.2095E+06
       < C5> ATM_CH4 = 1.85
       end constants

The values have units of parts per million. ATM_AIR equals the mixing ratio of M, any gas molecule. The block does not define the mixing ratio for H2O because the meteorological input data specific their values so the values depend on time and location.

#### FUNCTIONS.

The FUNCTIONS block defines formulas for calculating rate constant that can used by reaction Type 13. 
They are limited to one line in lenght but can reference preceeding formulas within the FUNCTIONS block.
They can use TEMP (K), PRES (atm), and the constant atmospheric species (molec/cm\*\*3). The syntax obeys
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

## Compiling and debugging chemmech

Two methods exist for building chemmech. The method to use depends on the FORTRAN compiler that will be used. If the Intel (INTEL), Portland (PGF90) or GCC (GFORT) compilers are available, the first and standard method executes the bldit_chemmech.csh script after changing the script’s COMPILER variable to one of the three options. If none of these compilers are to be used, the user has to modify src/Makefile to use the intended compiler and create chemmech using the make command.  As implied by the compilers available in the bldit script, chemmech has been tested with each to verify consistent results between compilers. The current Makefile includes the debug flags in the compilers options so the user can identify the cause and location when chemmech crashes. Crashes occur the mech.def contains information that exceeds the parameters defining array dimensions. The src/MECHANISM_PARMS.f file defines these parameters. The user can change many of the parameters then rebuild chemmech so the utility fits the application. Table 3 lists the parameter and state whether user should change their values.


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


Running chemmech is accomplished by modifying and executing the run script under the scripts subdirectory. In the run script, environment variables define names, directories and runtime options. The script contains comments describing each variable. Names and directory are used to set paths for the inputs, outputs, and chemmech. Run time options are set based on the application and user. The option, compile, determines whether to recompile chemmech. A user may want to recompile if they are modifying the chemmech source code or wish to use a different compiler from a previous application. The variable, COMPILER sets which the compiler to use from possible values mentioned above if compile equals true. The option, USE_SPCS_NAMELISTS states whether chemmech reads in the three mechanism species namelists and then checks whether the mech.def file uses a species not found in the namelists. chemmech will stop when this occurs. Running chemmech using the namelists is not required but the option provides check for potential errors when modifying an existing photochemical mechanism within the CMAQ model system. A user may want to set USE_SPCS_NAMELISTS  to false, F,  if they are creating a new photochemical mechanism.


## References.

Damian V., Sandu A., Damian M., Potra F., Carmichael G.R. (2002). The kinetic preprocessor KPP - A software environment for solving chemical kinetics. Computers and Chemical Engineering,  26(11) , pp. 1567-1579.

Gery, M.W. and Crouse, R.R. (1990) User’s Guide for Executing OZIPR,” EPA/6008-90, U.S. Enivironmental Protection Agency, Research Trianlge Park, 27711, NC.

Hertel O., Berkowicz R., Christensen J., and Hov O. (1993).  Test of Two Numerical Schemes for Use in Atmospheric Transport-Chemistry Models. Atmospheric Environment, Vol. 27A, No. 16, 2591-2661.

Jacobson M.Z. and Turco R.P. (1994). SMVGEAR: A sparse-matrix, vectorized gear code for atmospheric models. Atmospheric Environment, Volume 28, Issue 2, Pages 273-284,

Jefferies, H.E. (1990) User Guide to Photochemical Kinetics Simulation System PC-PKSS Software Version 3, Chapel Hill, NC 27514.

Jenkin M.E., Watson L.A., Shallcross D.E., Utembe S.R. (2008). A Common Representative Intermediate (CRI) mechanism for VOC degradation. Part-1: gas phase mechanism development
Atmos. Environ., 42, pp. 7185-7195

Sandu A., Verwer J.G, Blom J.G., Spee E.J., Carmichael G.R. and Potra F.A (1997). Benchmarking stiff ODE solvers for atmospheric chemistry problems II: Rosenbrock solvers. Atmospheric environment 31 (20), 3459-3472.

Watson L.A., Shallcross D.E., Utembe S.R., Jenkin M.E. (2008). A Common Representative Intermediates (CRI) mechanism for VOC degradation. Part 2: Gas phase mechanism reduction
Atmospheric Environment, 42 (31) , pp. 7185-7193
