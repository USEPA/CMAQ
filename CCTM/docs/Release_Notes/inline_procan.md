# Inlining Process Analysis in the CCTM

**Author/P.O.C.:**, [Jeff Young](mailto:young.jeff@epa.gov), Computational Exposure Divisi
on, U.S. EPA

## Brief Description
Process Analysis (PA) in the CCTM has two major optional components: Integrated Process Rates (IPR) and Integrated Reaction Rates (IRR). IPR enables science process analysis on specified model species or families (e.g., examine the timestep-by-timestep effect of the horizontal advection of NOx). IRR enables detailed analysis of gas chemistry reactions. For example, IRR provides the data to examine the timestep-by-timestep evolution of the production of NO2 from N2O5. Currently, IRR can be used only with the Rosenbrock (ros3) or Sparse-Matrix-Vectorized Gear (smvgear) solvers.
Previously PA was enabled by a preprocessing program that generated three Fortran include files, which were incorporated in a subsequent compilation of CCTM. This procedure required careful coordination of the selected chemistry definition in the CCTM to ensure appropriate include files.

An additional upgrade is a modification to the PA_UPDATE routine, which calculates the IPR changes for a timestep. These changes record mixing ratio values of the selected model species as maintained in the main processing array, CGRID, which is updated in turn by each of the science processes in the time-splitting steps of the model execution. These recorded values are written to output diagnostic files in mixing ratio units at each output timestep.

The horizontal diffusion and advection processes require a grid-Jacobian-weighted mass concentration CGRID species set, instead of molar mixing ratio units. The controlling SCIPROC routine "couples" the CGRID species for these science processes. Therefore PA_UPDATE must "decouple" for these processes to get to molar mixing ratio units. Previously this additional decoupling of CGRID introduced numerical "noise" sufficient to produce an undesirable, small difference in the concentration outputs compared with identical runs without IPR turned on. This upgrade changes PA_UPDATE to eliminate this noise by making a CGRID copy to decouple.

## Significance and Impact
This upgrade incorporates a user option for inline PA. If selected, a preprocessing step activates at run time. Thus the need for a PA preprocessing program and static compilation for a specific chemistry definition is eliminated. Consistency with the model's chemistry version is also assured. Using Process Analysis also becomes considerably more convenient. The impact of the increased size of the executable image is not significant.  

The run time increases by a small percentage when using PA, depending on the user's choices made for IPR, IRR, or both.
The additional cpu memory required for the PA_UPDATE change in making a copy of CGRID is not significant.

## Affected Files
modified: aero/aero6/aero_driver.F  
 deleted: couple/gencoor_wrf/couple.F  
modified: driver/wrf/sciproc.F  
modified: driver/yamo/driver.F  
modified: grid/cartesian/PAGRD_DEFN.F  
new file: procan/pa/PA_DEFN.F  
new file: procan/pa/PA_GLOBAL.F  
new file: procan/pa/PA_IPRDEF.F  
new file: procan/pa/PA_IPRVARS.F  
new file: procan/pa/PA_PARSE.F  
new file: procan/pa/PA_VARS.F  
new file: procan/pa/pa_compmech.F  
new file: procan/pa/pa_datagen.F  
new file: procan/pa/pa_errcheck.F  
new file: procan/pa/pa_getcoef.F  
new file: procan/pa/pa_getcycle.F  
new file: procan/pa/pa_getdesc.F  
new file: procan/pa/pa_getfamily.F  
new file: procan/pa/pa_getiprout.F  
new file: procan/pa/pa_getirrout.F  
new file: procan/pa/pa_getrxns.F  
new file: procan/pa/pa_getrxnsum.F  
modified: procan/pa/pa_init.F  
modified: procan/pa/pa_irr.F  
modified: procan/pa/pa_irr_ctl.F  
modified: procan/pa/pa_mkhdr.F  
new file: procan/pa/pa_molcloss.F  
new file: procan/pa/pa_molcprod.F  
modified: procan/pa/pa_output.F  
new file: procan/pa/pa_read.F  
new file: procan/pa/pa_report.F  
new file: procan/pa/pa_setup_ipr.F  
new file: procan/pa/pa_setup_irr.F  
modified: procan/pa/pa_update.F  
new file: procan/pa/pa_wrtpadefn.F  
modified: spcs/cgrid_spcs_nml/CGRID_SPCS.F  
modified: vdiff/acm2/vdiffacmx.F  
modified: vdiff/acm2/vdiffproc.F  
modified: gas/ros3/rbdriver.F  
modified: gas/smvgear/grdriver.F  
modified: gas/ebi_cb05e51_ae6_aq/hrdriver.F  
modified: gas/ebi_cb05e51_ae6nvPOA_aq/hrdriver.F  
modified: gas/ebi_cb05eh51_ae6_aq/hrdriver.F  
modified: gas/ebi_cb05mp51_ae6_aq/hrdriver.F  
modified: gas/ebi_cb05tucl_ae6_aq/hrdriver.F  
modified: gas/ebi_cb05tump_ae6_aq/hrdriver.F  
modified: gas/ebi_cb6r3_ae6_aq/hrdriver.F  
modified: gas/ebi_cb6r3_ae6nvPOA_aq/hrdriver.F  
modified: gas/ebi_racm2_ae6_aq/hrdriver.F  
modified: gas/ebi_saprc07tb_ae6_aq/hrdriver.F  
modified: gas/ebi_saprc07tc_ae6_aq/hrdriver.F  
modified: gas/ebi_saprc07tc_ae6nvPOA_aq/hrdriver.F  
modified: gas/ebi_saprc07tic_ae6i_aq/hrdriver.F  
modified: gas/ebi_saprc07tic_ae6invPOA_aq/hrdriver.F  

## References

Byun, D.W. and K.L. Schere (2006), Review of governing equations, computational algorithms, and other components of the Models-3 Community Multiscale Air Quality (CMAQ) modeling system, Applied Mechanics Review, 59, 51-77.

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #143](https://github.com/usepa/cmaq_dev/pulls/143)

#### Commit IDs:
d175b5f4711ff206589932473862137d52d45461  
2223d6a940238b0c14e1909c3f757e4ddbd03acb  
2df285439977492d9d4d06ffef8fbc55ca387084  
782a10347bac1564469dcf9c98ff30f4458d8e1b  
640dc4bb962cff3f17724463ba52cc1d43e33bf9  
b82f2e719e26c8c545fdca32a3b518fa471b0277  
4a3a9b351a355e0eb0d8a23e816bf42a2ecc3bd9  
e69dcd8d94e5d7b8684094a56f30426f7b71c02d  
3f6b4aca8fe33604cd23f4cef4d60bab3585bf4c  
f7a82733bd9cfb0e7073a0bcb3260c289bcbaf41  

-----
