# DESID: Detailed Emissions Scaling, Isolation, and Diagnostic Module

[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency

## Brief Description
To address specific research questions, CMAQ allows users to set and scale emissions on a per-species basis for gases and 
aerosols and turn on/off specific online modules, like BEIS, wind-blown dust and lightning NO. Yet, with previous versions of CMAQ,
users could not run with multiple gridded 
emission files, could not scale a species from different sources independently, could not turn off sea-spray 
emissions, and could not easily detemrine how aerosol size distributions were applied.

These and other features have now been supported with *DESID*, a new emissions module that now considers emissions 
from offline sources (such as those processed using SMOKE) similarly to online sources (such as wind-blown dust or those processed using BEIS). Gases and 
aerosols can be scaled independently for different sources. All emissions can be scaled with respect to 
gridded masks identified by the user. Rates can be scaled on a UNIT basis or by conserving moles or mass at 
the user's discretion. These tasks are accomplished using the newly instituted emission control file, which is available 
in each mechanism folder and copied to the build directory when the build script is executed.  

Importantly, an extensive diagnostic output log has been designed to inform users about surrogates from emission 
inputs that were not used, CMAQ species that received no emissions at all, and scale factors that were applied 
for each source, species, and mask.

## Significance and Impact
These updates enable users to implement research questions that involve scaling emissions with greater 
ease and flexibility. These updates will also provide the log information to more readily diagnose potential 
errors.

## Affected Files
CCTM/scripts/EmissCtrl.nml  

CCTM/src/ICL/fixed/filenames/FILES_CTM.EXT  

CCTM/src/MECHS/cb6mp_ae6_aq/AE_cb6mp_ae6_aq.nml

CCTM/src/MECHS/cb6mp_ae6_aq/NR_cb6mp_ae6_aq.nml

CCTM/src/MECHS/racm2_ae6_aq/AE_racm2_ae6_aq.nml

CCTM/src/MECHS/racm2_ae6_aq/GC_racm2_ae6_aq.nml

CCTM/src/MECHS/racm2_ae6_aq/NR_racm2_ae6_aq.nml

CCTM/src/MECHS/saprc07tc_ae6_aq/AE_saprc07tc_ae6_aq.nml

CCTM/src/MECHS/saprc07tc_ae6_aq/GC_saprc07tc_ae6_aq.nml

CCTM/src/MECHS/saprc07tc_ae6_aq/NR_saprc07tc_ae6_aq.nml

CCTM/src/MECHS/saprc07tic_ae6i_aq/AE_saprc07tic_ae6i_aq.nml

CCTM/src/MECHS/saprc07tic_ae6i_aq/GC_saprc07tic_ae6i_aq.nml

CCTM/src/MECHS/saprc07tic_ae6i_aq/NR_saprc07tic_ae6i_aq.nml

CCTM/src/MECHS/saprc07tic_ae6i_aqkmti/AE_saprc07tic_ae6i_aq.nml

CCTM/src/MECHS/saprc07tic_ae6i_aqkmti/GC_saprc07tic_ae6i_aq.nml

CCTM/src/MECHS/saprc07tic_ae7i_aq/AE_saprc07tic_ae7i_aq.nml

CCTM/src/MECHS/saprc07tic_ae7i_aq/GC_saprc07tic_ae7i_aq.nml

CCTM/src/MECHS/saprc07tic_ae7i_aq/NR_saprc07tic_ae7i_aq.nml

CCTM/src/MECHS/cb6r3_ae6_aq/GC_cb6r3_ae6_aq.nml

CCTM/src/MECHS/cb6r3_ae7_aq/GC_cb6r3_ae7_aq.nml

CCTM/src/emis/emis/EMIS_VARS.F

CCTM/src/aero/aero6/AERO_DATA.F

CCTM/scripts/run_cctm.csh

CCTM/scripts/run_cctm_2010_4CALIF1.csh

CCTM/scripts/run_cctm_2014_12US1.csh

CCTM/src/aero/aero6/AERO_EMIS.F

CCTM/src/depv/m3dry/ABFLUX_MOD.F

CCTM/src/depv/m3dry/m3dry.F

CCTM/src/emis/emis/EMIS_DEFN.F

CCTM/src/emis/emis/EMIS_VARS.F

CCTM/src/emis/emis/LTNG_DEFN.F

CCTM/src/emis/emis/PT3D_DEFN.F

CCTM/src/emis/emis/PTMET.F

CCTM/src/plrise/smoke/oppt3d_diag.F

CCTM/src/util/util/RUNTIME_VARS.F

CCTM/src/vdiff/acm2_m3dry/vdiffacmx.F

CCTM/src/vdiff/acm2_stage/vdiffacmx.F

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #305](https://github.com/USEPA/CMAQ_Dev/pull/305)

[PR #355](https://github.com/USEPA/CMAQ_Dev/pull/355)

[PR #356](https://github.com/USEPA/CMAQ_Dev/pull/356)

[PR #371](https://github.com/USEPA/CMAQ_Dev/pull/371) 

[PR #376](https://github.com/USEPA/CMAQ_Dev/pull/376)

[PR #383](https://github.com/USEPA/CMAQ_Dev/pull/383) 

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
bf7af5ac0e067de00ca35065a0de13f825882a6d  
6a5367854d7c1f38791cb7b6349373ab5c869a41  
eb6de12ffa424e0a506dbbeb26a6e8222428f019  
eb16d513d46b6cdfb29a8646e3c210441d71b012  
bf7af5ac0e067de00ca35065a0de13f825882a6d  
6a5367854d7c1f38791cb7b6349373ab5c869a41  
2fab271c2b2c2e5af958a5be77a4f25183c24da1  
60031df0496e83b1ed88471af2f3c178e97964f7  
756fb64864eec4bee1d86e46b812c181a6c1ca7f  
819aea18a11e8b43420dd018b2c0a8ec1333f1a3  
bf4e91b2d237c3ea019352ec451a23180b07016e    
11b95cb67cdae827bb91c4e8381ee987e9af6c21  
b89376aa54bad19bbcb84870142de286754b566a  
b1f7c7e2d87f5f89bf061e67abe1b3bcfd709ca1  
a0027b375cb3bc1b153283c7a675e8f85ac81272  
4778d0f70fb23ec3339052ac4cdafa2b78c64d7b  
0902dc07e78e0bc952bff8224087a463cebcac01  
e785093a03809cf8019beb958e69cceeba017c02  
6c779077627efaa15ab4d486632538cb113ecb0c  
73ae9e404ebbe786f01f63cf022e9638ea924a4f  

-----                                     
                                          
                                          
