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

## Significance and Impact  
There is no quantitative impact on results, but there will be a noticeable impact on the time necessary to articualte emission control files.  

## Affected Files
CCTM/src/emis/emis/EMIS_DEFN.F
CCTM/src/emis/emis/EMIS_VARS.F
CCTM/src/cio/centralized_io_mofdule.F

-----
## Internal Records:
#### Relevant Pull Requests:


#### Commit 
IDs:                        

-----
