# Log File Output: Streamlining and Centralization

[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency

## Brief Description
CMAQ users and developers have often noted the difficulty in using the default log file for information 
about standard runs and encountered errors. The code responsible for generating log files has been 
updated as follows:

1) The main (i.e., screen) output is now reserved for high-level information pertinent to the simulation. 
This information includes a list of all environment variables with the values selected for the run, 
a time-step by time-step log of the run times for each sub-process, and a table documenting the per-day 
run time for all days. This table is processed and output by the default CMAQ runscript, not by the source 
code.  
2) Each processor now creates a log file using the filename syntax *CTM_LOG_XXX_...*, where *XXX* is the three-
digit processor number. These logs have been updated to better identify and organize the information 
printed to the logs.

## Significance and Impact
The log files are now expected to better accomodate the kinds of questions users have about runs they have 
performed recently or far in the past. The processor-based logfiles should also be slightly better at 
helping users diagnose errors they encounter. However, crafting more informative logfiles is an ongoing 
process which requires continual input from users and developers.

## Affected Files
CCTM/scripts/EmissCtrl.nml  

CCTM/scripts/run_cctm.csh  

CCTM/src/ICL/fixed/const/CONST.EXT  

CCTM/src/ICL/fixed/emctrl/EMISPRM.EXT  

CCTM/src/ICL/fixed/filenames/FILES_CTM.EXT  

CCTM/src/ICL/fixed/mpi/PE_COMM.EXT  

CCTM/src/ICL/procan/pa/PA_CTL.EXT  

CCTM/src/ICL/procan/pa/PA_DAT.EXT  

CCTM/src/ICL/procan/pa_noop/PA_CTL.EXT  

CCTM/src/ICL/procan/pa_noop/PA_DAT.EXT  

CCTM/src/MECHS/cb6r3_ae6_aq/RXNS_FUNC_MODULE.F90  

CCTM/src/MECHS/cb6r3_ae7_aq/RXNS_FUNC_MODULE.F90  

CCTM/src/MECHS/racm2_ae6_aq/RXNS_FUNC_MODULE.F90  

CCTM/src/MECHS/saprc07tc_ae6_aq/RXNS_FUNC_MODULE.F90  

CCTM/src/MECHS/saprc07tic_ae6i_aq/RXNS_FUNC_MODULE.F90  

CCTM/src/PARIO/boundary.f  

CCTM/src/PARIO/pinterpb.f  

CCTM/src/PARIO/pm3err.f  

CCTM/src/PARIO/pm3exit.f  

CCTM/src/PARIO/pm3warn.f  

CCTM/src/PARIO/ptrwrite3.f  

CCTM/src/PARIO/pwrite3.f  

CCTM/src/PARIO/wrsubdmap.f  

CCTM/src/STENEX/noop/noop_comm_module.f  

CCTM/src/aero/aero6/AEROSOL_CHEMISTRY.F  

CCTM/src/aero/aero6/AERO_DATA.F  

CCTM/src/aero/aero6/AERO_EMIS.F  

CCTM/src/aero/aero6/PRECURSOR_DATA.F  

CCTM/src/aero/aero6/SOA_DEFN.F  

CCTM/src/aero/aero6/aero_driver.F  

CCTM/scripts/run_cctm.csh  

CCTM/scripts/run_cctm_2010_4CALIF1.csh  

CCTM/scripts/run_cctm_2014_12US1.csh  

CCTM/scripts/run_cctm_2015_HEMI.csh  

CCTM/src/driver/yamo/advstep.F  


-----
## Internal Records:
#### Relevant Pull Requests:
[PR #277](https://github.com/USEPA/CMAQ_Dev/pull/277)  

[PR #382](https://github.com/USEPA/CMAQ_Dev/pull/382)

[PR #384](https://github.com/USEPA/CMAQ_Dev/pull/384)

#### Commit 
IDs:                        
bb9b72b810da2250f0449ae3ae59ba70b51225b9   
44fb7163689b7fca828036f4dd7306ca943bc678   
ec5613b1945a11371cf71c06843d5d1fe7731a78   
5ea6a3a3c95d01d63a45437037c34643ca8a779c   
05ad5b23fc889f731fc823e513b3436316d6e69c   
08c66505d33fea15ce37cad8088276e0569879ca   
8bd3a1e8c213e5e800bdefc411b71c0436bdef59     
191a6fb1893c0965680726e1703ef10b06bfcda1     
38dcb88c2da5bc71b38da70b2b6d80d55afed936   
8e66f7c1b11dc333993e94623e08b5a7f57c23d9   
29254825456ea5ab2c942a8d619047a6e8572d2c   
0a9f33bbad1d16a9ee86ecf8399bd0faf322a726   
52da25cdb13f7013c78814a90ed708b951e3ebe6     
068f52d40d419b8cf6c7fbdb09beb9abf2b4e420     
40525343a9b02c266dce400bb3ec908cfe36f22b   
0dfc3dbb0a3e47944fcc9cb77c1b6511c03dad63   
8bd1ff0e4f25233cd8de9a5db2c14b4faf41d679   
30dcd7362886e1230359ee568fc2b777a41bfc99   
810e047e03401ce262442cffc99b7ba964377395     
ae320da859eafa648882f7ee9cc7579bdc26e12d     
f9ee038f9ba5eb2ccd965b848044321303388265   
df087a4432a6f2b415cd8c85ae610a3d1e249506   
f2d77f61cdce5d1af724d4f7eba0784f8603cd3d   
42553d3701053698bdd0f699db0d96c4a1e94e60    
439461abe85ab2a8b143678f52d1f20844edb249      
6ad23e1db2ce12a3ce123f80f37d6b4a453a1bff      
39c6c2ee42598d2d2a8888801e252b7114e3e7b5    
27200763bc4818d479bf6dcbeabc6d0a1376e7ce    
31ee693c71daf7943623c739c7971cfc91279de1      
5c433bb610904129c8819e89aa60f34df80c137b      
fd76e00c906daa4c8833bd5d9e8a066e5292fdee      
d95fbdf0784d06ecc8d6d3c41056dba58524f01f    
                                          
-----                                     
                                          
                                          
