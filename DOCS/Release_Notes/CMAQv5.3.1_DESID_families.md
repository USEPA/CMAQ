# DESID Families
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency

## Brief Description
The Detailed Emission Scaling Isolation and Diagnostic (DESID) module is meant to make it trivial to modify emissions online in CMAQ through invoking complex scaling tasks with intuitive syntax for defining rules. For example, users have control over scaling individual compounds for each emission stream independently and over custom geographical areas of the model domain. One unattractive feature was that a lot of repetitive rules would be needed for complex, but easy-to-articulate tasks. For example, if a user wanted to scale NOx (NO + NO2) for 4 emission streams to 50%, then 8 rules would be required in the emission control file:  
```
! Region      | Stream Label  |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |               |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'PT_EGU'      ,'NO2'    ,'NO2'         ,'GAS' ,0.50 ,'UNIT','m',
'EVERYWHERE'  , 'GRIDDED'     ,'NO2'    ,'NO2'         ,'GAS' ,0.50 ,'UNIT','m',
'EVERYWHERE'  , 'MOBILE'      ,'NO2'    ,'NO2'         ,'GAS' ,0.50 ,'UNIT','m',
'EVERYWHERE'  , 'PT_NONEGU'   ,'NO2'    ,'NO2'         ,'GAS' ,0.50 ,'UNIT','m',
'EVERYWHERE'  , 'PT_EGU'      ,'NO'     ,'NO'          ,'GAS' ,0.50 ,'UNIT','m',
'EVERYWHERE'  , 'GRIDDED'     ,'NO'     ,'NO'          ,'GAS' ,0.50 ,'UNIT','m',
'EVERYWHERE'  , 'MOBILE'      ,'NO'     ,'NO'          ,'GAS' ,0.50 ,'UNIT','m',
'EVERYWHERE'  , 'PT_NONEGU'   ,'NO'     ,'NO'          ,'GAS' ,0.50 ,'UNIT','m',
```  
This pull request implements family definitions for emission streams, chemical families, and geographical regions. So, if for example, the user defines the stream family "CONTROLLED_SOURCES" as the group made up by "PT_EGU","GRIDDED","MOBILE" and "PT_NONEGU" and the chemical family "NOX" made up by "NO" and "NO2", then the task above can be accomplished with one rule:
```
! Region      | Stream Label         |Emission | CMAQ-        |Phase/|Scale |Basis |Op  
!  Label      |                      |Surrogate| Species      |Mode  |Factor|      |
'EVERYWHERE'  , 'CONTROLLED_SOURCES' ,'NOX'    ,'NOX'         ,'GAS' ,0.50 ,'UNIT','m',
```  
This feature drastically improves the usability of the DESID features for practical applications. 

**See the [DESID Tutorial in the CMAQ User's Guide](../Users_Guide/Tutorials/CMAQ_UG_tutorial_emissions.md) for further instructions on how to utilize the emission control namelist to perform manipulation of emission streams.**

## Significance and Impact  
There is no quantitative impact on results, but there will be a noticeable impact on the time necessary to articualte emission control files.  

## Affected Files
CCTM/src/emis/emis/EMIS_DEFN.F
CCTM/src/emis/emis/EMIS_VARS.F
CCTM/src/cio/centralized_io_mofdule.F

-----
## Internal Records:
#### Relevant Pull Requests:
586

#### Commit 
IDs:                        
58ddfd3d9b308a69e4de7416732c75e41c4d94a2  
21add7748fd1f72dd4186d8197943b5e98a2e27e  
b45d83568d56ccf33d61c65690bea43ab2a31df1  
1ad85c9ec91cd37eacf8f1bb9542bc0a15c6bd0b  
a751cb05cca01f10237e032074089329fb461efa  
e3813218fc47bfe6b73a50f08633e864ee14e00d  
85a4b0282faccb8a02b37c8805e4c9e500b162a7  
951b8aab48fe6c69fc18234d4c9adba09be6f37f  
921c8218db4c67a4b4f9bd9072d83713fb05ef9c  
c7e40dd8c2c4467893e4d58578128f9b79e95047  
300b8d5404d1ce2c9b9980e721814b50191f510d  
47ef9f1f9e8152be5b1466a55049e7d57bd095d0  
3db5dedd3249737c3722c9de409a2e060d4b91fb  
7ac745bef1f558156ea8015b65589c9a8937cf5f  
05daf66d82e76d3c6de2a0f66616da494b31bd64  
75f210a273734fb78b6fd6d28a79561852dc397a  
13e18828c41171f2eedf9e9e8158718e8fcb8fee  
1a8b56329f87fcdb86032ef87a8e21000e86bd21  
7d3c02678840d80b5f49abe86d5dc7e7d82d0391  
83a1bdeb4c66e6257ddeb2f6d9a4834ac2f1c7b0  

-----
