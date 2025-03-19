### Tuning up the Gear Solver

[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency  

**Type of update**: Numerical Science Update  
**Release Version/Date**: v5.5    
**Description**:  The CCTM version of the smvgear solver is based on an optimized Gear for vectorized computers (Jacobson and Turco, 1994). It does not employ updates in a latter version by Jacobson called smvgearII. Two updates are adapted from smvgearII to use in CMAQ's smvgear solver to improve its computational efficiency.

The first update changes how to determine the absolute tolerance for testing convergence of a grid cell's concentrations.  The method determines the absolute tolerance based on the fraction of chemistry species, _f<sub>0</sub>_, above a specific concentration (Jacobson, 1998). The concentration is determined from log<sub>10</sub> intervals between a highest and lowest absolute tolerances allowed. Based on what the Jacobson (1998) used for tropospheric chemistry, the highest allowed tolerance is 100 times greater than the lowest allowed tolerance. For the CMAQ smvgear solver, the highest and lowest allowed tolerances are set to 10<sup>-7</sup> and 10<sup>-9</sup> ppm based on default absolute tolerance in the original smvgear solver, 10<sup>-9</sup>. Two environment variables,  **GEAR_MAX_ATOL** and  **GEAR_MIN_ATOL**, allow setting the highest and lowest allowed tolerances to difference values. Jacobson (1998) sets _f<sub>0</sub>_ to 0.4 times the number of chemistry species.  Because this number had limited effects reducing runtime in the CMAQ model, this work sets  _f<sub>0</sub>_ to 0.4 times the chemistry species above a minimum concentration that determine radical cycles and key species concentrations in the tropospheric chemistry, 10<sup>-12</sup>  ppm. The number also corresponds to the product of the default absolute and relative tolerances in the original smvgear solver. An environment variable, **GEAR_CONC_FLOOR**, allows setting the minimum concentration to a different value.  

The second update taken from smvgearII changes the solver's Newton Iteration where it jumps to a higher order approximation. In addition to considering the number of iterations taken, the original smvgear considers the rate of decrease in prediction errors. The update drops the rate test and sets the maximim number of allowed iterations to 3 based on timing tests in the CMAQ model. The smvgearII solver uses 1 iteration.

**Significance and Impact**: 

CCTM runtimes are reduced when the model uses the revised smvgear solver. For cb6r5_ae7_aq and cb6r5m_ae7_aq mechanisms, runtimes are reduced by factors greater than four. For the cracmm1_aq and saprc07tic_ae7i_aq mechanisms, the runtimes decrease around 25% against original smvgear solver. Tests show that the updated smvgear has runtimes averaging between 7% and 13% slower than the Rosenbrock (ros3) solver but this comparison against ros3 showed -6% to 28% range of relative differences in the sample of runtimes. The results imply that the revised solver is a more viable option for checking model predictions from ebi and ros3 solvers.

Changes to the smvgear gas solver produce less than 0.1% normalized mean biases in prediction concentrations from the ros3 and the original smvgear solvers for most mechanism species. Exceptions occur for the cb6r5m_ae7_aq mechanism. Chlorine and iodine oxides show normalized mean biases up to several precent over the hemispheric domain during July 2018. I2O4 showed the greatest magnitude of difference with a Normalized mean biases had values around -7%.

**References**:   
Jacobson, M.Z. (1993). SMVGEAR (Version II) [Source Code], U.S. Copyright Office Registration No. txu 670-279  
Jacobson, M.Z. and Turco, R.P (1994). SMVGEAR: A sparse-matrix, vectorized gear code for atmospheric models. Atmospheric Environment 28(2), 273-284 pp.  
Jacobson, M.Z. (1998). Improvement of SMVGEAR II on vector and scalar machines through absolute error tolerance control. Atmospheric Environment 32(4), 791-796 pp.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#954](https://github.com/USEPA/CMAQ/commit/defef195f15d828cdf47d222866f79b855684361) | [PR#954](https://github.com/USEPA/CMAQ_Dev/pull/954)  | 


### EBI Solver Update

[Golam Sarwar](mailto:sarwar.golam@epa.gov), U.S. Environmental Protection Agency  

**Type of update**: Science Update  
**Release Version/Date**: v5.4  
**Description**:  

CMAQ provides three different gas-phase chemistry solvers: the Euler Backward Iterative (EBI), the Rosenbrock and the Gear solvers. The EBI method is not a generalized solver and needs to be developed for each chemical mechanism. However, it is generally faster than the Rosenbrock and the Gear solvers. CMAQ uses following procedure for determining convergence in the EBI solver:

o	AERROR( S ) = ABS( YC(S)-YCP(S) )

o	RERROR( S ) = AERROR( S ) / ABS( YC(S)+YCP(S) )

Where, AERROR = absolute error, RERROR = relative error, YC = species concentrations (initial), YCP = species concentrations (updated). The use of such a convergence criterion can utilize computational time in finding a solution at very low concentrations without improving predicted concentrations. CAMx uses a similar but slightly different approach at very low concentrations. Here, we revise the convergence criteria in CMAQ following the procedure used in CAMx. At very low concentrations, model uses prescribed value for determining relative error and can potentially save some computational time.

o	AERROR( S ) = MAX( ABS( YC(S)-YCP(S) ), 1.0D-30 )

o	RERROR( S ) = AERROR( S ) / MAX(1.0D-08, ABS( YC(S)+YCP(S) ) )


**Significance and Impact**: 

These updates are implemented in solvers for all mechanisms and have been tested in cb6r3_ae7_aq, cb6r5_ae7_aq, racm2_ae6_aq, saprc07tic_ae7i_aq. Model sensitivity runs were completed using existing and updated convergence criteria for a 10-day period in summer using 128 processors. Model with updated convergence criteria shows some improvement in model runtime without any substantial impact on model results. For example, model with cb6r3_ae7_aq shows a runtime improvement of 2%, cb6r5_ae7_aq shows an improvement of 1%, racm2_ae6_aq shows an improvement of 3%, and saprc07tic_ae7i_aq shows an improvement of 4%. 

Model with updated convergence criteria has only small impacts on model results. The largest difference in hourly predicted ozone concentrations during the 10-day period with cb6r3_ae7_aq and racm2_ae6_aq are shown in Figure 1. Model results with other mechanisms are also similar and are not shown. 


![image](https://user-images.githubusercontent.com/17162838/172241106-c248b1c4-4ed6-47dc-b412-c75ac0ad2fac.png)

Figure 1: Impact of updated convergence criteria in EBI solver on predicted ozone

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#782](https://github.com/USEPA/CMAQ/commit/e8f37b49d3f67023b874d4cd5d299d9dfd4345e0) | [PR#782](https://github.com/USEPA/CMAQ_Dev/pull/782)  | 
