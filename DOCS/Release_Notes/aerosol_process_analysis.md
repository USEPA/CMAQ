# Process Analysis for Aerosol Sub-Processes

[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency

## Brief Description
The Process Analysis (PA) module has been extended to quantify the contribution of aerosol subprocess
such as condensation, coagulation, mode reallocation and new particle formation on all model species. 
Process Analysis input and output have been integrated with this new feature. The short-names of these 
new processes (to be used in the PA control file) are:

- COND --> Condensation
- COAG --> Coagulation
- NPF --> New particle formation
- GROWTH --> Model reallocation

## Significance and Impact  
Users may now quantify the impact of aerosol sub-process on specific aerosol and gas-phase species of 
interest. This will become more valuable as alternative size distribution schemes are explored in the 
future.

## Affected Files
CCTM/scripts/EmissCtrl.nml  

CCTM/scripts/PA.ctrl  

CCTM/scripts/run_cctm.csh  

CCTM/scripts/run_cctm_2010_4CALIF1.csh  

CCTM/scripts/run_cctm_2014_12US1.csh  

CCTM/src/ICL/fixed/filenames/FILES_CTM.EXT  

CCTM/src/STENEX/se/se_data_copy_module.f  

CCTM/src/aero/aero6/AERO_BUDGET.F  

CCTM/src/aero/aero6/AERO_DATA.F  

CCTM/src/aero/aero6/PRECURSOR_DATA.F  

CCTM/src/aero/aero6/SOA_DEFN.F  

CCTM/src/aero/aero6/aero_driver.F  

CCTM/src/aero/aero6/aero_subs.F  

CCTM/src/aero/aero6/opapmdiag.F  

CCTM/src/aero/aero6/oppmdiag.F  

CCTM/src/driver/wrf/sciproc.F  

CCTM/src/driver/yamo/sciproc.F  

CCTM/src/emis/emis/EMIS_DEFN.F  

CCTM/src/grid/cartesian/PAGRD_DEFN.F  

CCTM/src/procan/pa/PA_DEFN.F  

CCTM/src/procan/pa/PA_GLOBAL.F  

CCTM/src/procan/pa/PA_IPRDEF.F  

CCTM/src/procan/pa/PA_IPRVARS.F  

CCTM/src/procan/pa/pa_errcheck.F  

CCTM/src/procan/pa/pa_init.F  

CCTM/src/procan/pa/pa_output.F  

CCTM/src/procan/pa/pa_read.F  

CCTM/src/procan/pa/pa_report.F  

CCTM/src/procan/pa/pa_setup_ipr.F  

CCTM/src/procan/pa/pa_update.F  

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #311](https://github.com/USEPA/CMAQ_Dev/pull/311)  

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
27200763bc4818d479bf6dcbeabc6d0a1376e7ce  
31ee693c71daf7943623c739c7971cfc91279de1  
5c433bb610904129c8819e89aa60f34df80c137b  
c4b42722f97db02562784cbb30dff67cd977132f  
70cc9a1e2a252701ed7b00a4d3c99043577d5132  
e64a34be00c3d6e3031f00ad9a940d67f4907178  
97be7405f0bb4a6887010e8b0fac1cdb445e12ea  


-----

