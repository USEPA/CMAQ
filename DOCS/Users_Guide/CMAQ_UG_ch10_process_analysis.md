
<!-- BEGIN COMMENT -->

[Home](README.md) - [Next Chapter >>](CMAQ_UG_ch07_HDDM-3D.md)

<!-- END COMMENT -->

# Process Analysis
Process analysis (PA) is a technique for separating out and quantifying the contributions of individual physical and chemical processes to the changes in the predicted concentrations of a pollutant. PA does not have to be activated in a CMAQ simulation, but including PA provides additional information that can be useful in interpreting CMAQ results. PA has two components:  Integrated process rate (IPR) analysis, and Integrated Reaction Rate (IRR) analysis. IPR analysis quantifies the individual physical processes of advection, diffusion, emissions, dry deposition, aerosol processes, and cloud processes, and the overall impact of chemical processes. IRR analysis allows the output of individual chemical reaction rates or user-specified combinations of chemical reactions and species cycling.  

As a tool for identifying the relative importance of individual chemical and physical processes, PA has many applications, including:
- When attempting to identify the major contributors to the concentration of a chemical species, PA helps to unravel the large number of complex processes that control species concentrations. This is also useful for species that have both production and decay processes occurring in the same time step, because the final concentration may show little change, but individual decay and production rates may be large.   
- To characterize the chemical “state” of a particular grid cell, IRR can be used to calculate quantifies such as the production of odd oxygen, the production of new radicals, and the termination of radicals. (For example, see Tonnesen and Dennis, 2000.)
- As a tool for model development, PA can help evaluate the effect of modifications made to a model or process module.
- For QA purposes, PA can be used to help identify compensating or unresolved errors in the model or input data which may not be reflected in the total change in concentration. For example, if an error in the emissions input data causes the model to calculate negative concentration values in an intermediate step, this could be masked in the final predicted concentrations if compensated for by larger positive values resulting from the chemistry calculations.

PA variables are computed by saving the differential operators associated with each process or reaction, integrated over the model synchronization time step –  the same variables that are used in solving the continuity equations within the model. For processes that are solved simultaneously in the same operator, PA uses mass balance to compute the contribution of each process.

If the user activates PA during CMAQ runtime (CTM_PROCAN=Y), the PA input file (PACM_INFILE) specifies whether IPR, IRR or both analyses are performed, and defines which variables are required for each analysis. The IRR parameters are highly customizable and can be easily modified for new chemical mechanisms, but must be checked carefully before running to ensure that they correspond to the mechanism being used. The PA_REPORT output file should always be reviewed to verify that the calculations are being performed correctly. Note that while the IPR option can be run with any of the chemical solvers, use of IRR in CMAQ requires either the Rosenbrock or the SMVGEAR solvers.

## Process analysis options

-   `CTM_PROCAN [default: N]`  
    Activate process analysis in the CCTM. Set this to Y and use $CMAQ_DATA/pacp/pacp.inp to configure the integrated process rate and integrated reaction rate settings for the CCTM.  Additional process analysis output files will be created when this setting is activated.
-   `PA_BCOL_ECOL [default: None]`  
    Modeling grid domain column range for the process analysis calculations. Set to the two digits representing the beginning and ending column number bounding the process analysis domain.
-   `PA_BROW_EROW [default: None]`  
    Modeling grid domain row range for the process analysis calculations. Set to the two digits representing the beginning and ending row number bounding the process analysis domain.
-   `PA_BLEV_ELEV [default: None]`  
    Modeling grid domain layer range for the process analysis calculations. Set to the two digits representing the bottom and top layer numbers bounding the process analysis domain.

<!-- BEGIN COMMENT -->

[Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch07_HDDM-3D.md)<br>
CMAQ User's Guide (c) 2019<br>

<!-- END COMMENT -->
