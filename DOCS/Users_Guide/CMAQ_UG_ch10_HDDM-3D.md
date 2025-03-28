
<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch09_process_analysis.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch11_ISAM.md)

<!-- END COMMENT -->

# 10. Decoupled Direct Method in Three Dimensions (CMAQ-DDM-3D)

## 10.1 Introduction

The Decoupled Direct Method in Three Dimensions (DDM-3D) provides CMAQ concentration and deposition sensitivity information for user specified model parameters.

In air quality modeling, sensitivities measure the response of a model output to a change in one or several predefined model parameters. In policy applications, the parameters of interest are usually emissions and the output of interest is pollutant concentrations. We may be interested in emissions from a particular geographical region, like an urban area, a group of states, or a country, and/or emissions from a particular source, such as wildfires, electricity generating units (EGUs), or light duty diesel trucks.

Emissions sensitivities can be calculated by simply running the air quality model twice – once with standard emissions inputs, and once with the emissions of interest adjusted in some way. The difference in outputs between the two runs in relation to the size of the adjustment then becomes the model sensitivity. While this process is fairly easy to implement and interpret, it quickly becomes computationally complex as the number of desired sensitivities increases. For example, calculating sensitivity to EGU emissions from 10 southeastern states in the U.S. would require 11 separate air quality model simulations.

Alternatively, model sensitivities can be calculated with CMAQ-DDM-3D. This is done by altering the existing model algorithms to allow for sensitivity propagation through every science module in CMAQ. While this process does require more computational resources than standard CMAQ, it scales favorably with the number of desired parameters.

Besides emissions, sensitivities to other model parameters can also be calculated. Currently, CMAQ-DDM-3D can be used for sensitivity to emission rates, boundary conditions, initial conditions, reaction rates, potential vorticity, or any combination of these parameters. Second order sensitivity calculations, or sensitivity of sensitivity, sometimes known as higher-order DDM-3D (HDDM-3D) are also available.  Note: second order sensitivity output for particulate matter species are still in development. 

## 10.2 CMAQ-DDM-3D Releases and Build Instructions

Starting from CMAQv5.4, DDM-3D has been fully integrated into the base model and is accessed through a flag in the standard build script.

To use CMAQ-DDM-3D, follow the normal build process for CMAQ described in [Chapter 5](CMAQ_UG_ch05_running_a_simulation.md), but make sure to uncomment the following line in bldit_cctm.csh: 

```
 set DDM3D_CCTM                        #> uncomment to compile CCTM with DDM-3D activated
```

**A note about I/O API installation for DDM applications**

I/O APIv3.2  supports up to MXFILE3=64 open files, each with up to MXVARS3=2048. DDM-3D applications configured to calculate sensitivity to a large number of parameters may exceed this upper limit of model variables, leading to a model crash. To avoid this issue, users may use I/O API version 3.2 "large" that increases MXFILE3 to 512 and MXVARS3 to 16384. Instructions to build this version are found in [Chapter 3](CMAQ_UG_ch03_preparing_compute_environment.md#333-io-api-library).

# 10.3 CMAQ-DDM-3D Run Instructions

The included model run scripts each have a section that enables CMAQ-DDM-3D options. The DDM-3D configuration options shown in the table below. Additinally, a separate DDM-3D control input file is also required. Details on this file are included below.

The CMAQ-DDM-3D benchmark simulation uses the same input data as the base model.  

# 10.3.1 DDM run script settings

|Option | Settings | Description|
|:-------------:|:-------------:|-----|
|CTM_DDM3D|Y/N|Sets up requisite script settings for DDM-3D; requires that the CCTM was compiled for DDM simulations|
|CTM_NPMAX|#|Number of sensitivity parameters defined in SEN_INPUT|
|SEN_INPUT||Path and name of the sensitivity control file|
|DDM3D_HIGH|Y/N|Allow higher order sensitivity parameters in SEN_INPUT|
|DDM3D_RST|Y/N|Begin sensitivities from a restart file|
|S_ICpath||Path of the restart file; Analogous to ICpath
|S_ICfile||Name of the restart file; Analogous to ICfile


# 10.3.2 CMAQ-DDM-3D Control File (SEN_INPUT)

Users must define the DDM-3D sensitivity parameters in the DDM-3D Control File `SEN_INPUT` linked in the runscript. 

The DDM-3D Control File accommodates various types of sensitivity configuration parameters for CMAQ-DDM-3D simulations, including the tagging of multiple emission sources and/or species. As proper formatting of this file is required, users are referred to the sample control file provided with the release for formatting examples. Sample definitions of sensitivity parameters are shown below.

Example 1
Calculate the sensitivity to total emissions of SO2.  The keyphrase 'TOTA' indicates all model emissions:

    ES2     
     EMIS
      TOTA
     SPECIES
      SO2

    END
    
Example 2
Calculate the sensitivity to NOx emissions from point source EGU and gridded emissions at the same time: (gridded sources, point sources, and/or inline sources are all allowed). 

These environment variables, with the full path to the files, must be defined in the run script:

    ENX     
     EMIS
      GRIDDEDEMIS, PT_EGU
     SPECIES
      NO, NO2

    END
The variables 'GRIDDEDEMIS' and 'PT_EGU' are comma-delimited in the control file and are both defined in the runscript. The list of species is also comma-delimited in the control file.

Example 3
It is also possible to calculate sensitivity to inline emissions streams. The following example calculates sensitivity to inline BEIS emissions of isprene:

    EBI     
     EMIS
      BEIS
     SPECIES
      ISOP

    END

Example 4 
Several sensitivities can be calculated in one simulation. In the example below, there are four sensitivities defined in this control file.  In the first (ES2), DDM-3D sensitivities would be calculated to emissions of SO2 from one gridded emissions file and two point source emissions files together.  In the second (ENX), DDM-3D sensitivities would be calculated to total emissions of NOx (NO+NO2). In the third (2NX), higher order DDM-3D sensitivities would be calculated to NOx emissions. In the fourth (RT1), DDM-3D sensitivities would be calculated to the rate of reaction 1 in the photochemical mechanism.

    ES2     
     EMIS
      EGRIDFILE1, PT3DFILE1, PT3DFILE2
     SPECIES
      SO2

    ENX
     EMIS
      TOTA
     SPECIES
      NO, NO2

    2NX
     HIGH
     ENX
     ENX

    RT1
     RATE
     REACTION
      1

    END
    
Example 5
It is possible to calculate the sensitivity to ozone incursions at the top of the simulated volume if the base model is compiled with potential vorticity module enabled.

    PO3
     PVO3
     SPECIES
      O3  
    
CMAQ-DDM-3D is flexible in the number of files that the code can handle and also allows for inline emissions streams as well.  Depending on the application and model settings, the following inline streams may be available for sensitivity calculation:

|Stream Keyword | Description|
|:-------------:|-----|
|BIOG|BEIS biogenic emissions|
|MIOG|MEGAN biogenic emissions|
|MGEM|Marine Gas Emissions|
|LTNG|Lightning NO Emissions|
|ASEA|Sea Spray Aerosol Emissions|
|DUST|Wind-Blown Dust Emissions|


## 10.3.2.1 DDM-3D Control File Format

For each sensitivity:
1. (mandatory) The first line is the name of the sensitivity parameter; any 3-character name of the user's choosing, no leading spaces
2. (mandatory) The next line specifies the type of sensitivity (One leading space followed by 4 capitalized characters)
      * EMIS: Emissions 
      * ICON: Initial Conditions
      * BCON: Boundary Conditions
      * RATE: Reaction rate
      * HIGH: Higher-order sensitivity.
      * PVO3: Potential vorticity
3. (mandatory) For EMIS, the next line specifies the emissions streams to be used for this parameter. The comma-delimited list must have two leading spaces. The stream names must be defined in the run script. Alternatively, the key word 'TOTA' will use all model emissions for this parameter. 
4. (mandatory)
      * For EMIS, ICON, BCON, or PVO3 sensitivity: The term ' SPECIES' (all-cap, one leading space) must appear next.
      * For RATE sensitivity: The term ' REACTION' (all-cap, one leading space) must appear next.
      * For HIGH sensitivity: The next 2 lines must each be one leading space followed by the name of the sensitivity to which we're taking higher order sensitivity. That name must have already been defined as the name of a sensitivity parameter. No further information should be defined for a higher-order sensitivity parameter.
5. (mandatory)
      * EMIS, ICON, BCON, or PVO3 sensitivity: Specify one or more species.  The comma-delimited species list must have two leading spaces and then exactly match a species from model species list. The keyword 'ALL' here allows for all model species instead. Note that only the species "O3" will result in a signal for PVO3 sensitivity.
      * For RATE sensitivity: Specify one or more reactions.  Names must have two leading spaces and then exactly match the _label_ from mech.def (also in RXDT.EXT).
6. (optional) 
     * The term ' REGION' (all-cap, one leading space). If this term is used, the next line must be a comma-delimited list of region variables.
     
 * NOTE1: This list must be consistent with the max # of sens parameters (NPMAX) set in the runscript.
 * NOTE2: For better understanding of how this file is read, or to modify/add features, look at sinput.F in the code.


## 10.4 DDM-3D Input/Output Files

With the exception of the control file, CMAQ-DDM-3D requires the same input files as a normal CMAQ run.  Additional input files may be required depending on the choice of calculated sensitivity parameters.  The following table includes a list of all possible files specific to sensitivity calculations.

Output Files Specific to DDM-3D Simulations

|File|Type|Contains|Base model analog|
|----|----|--------|-----------------|
| ASENS|Output| Averaged hourly sensitivities. List defined by 'AVG_CONC_SPCS' variable in the run script.|ACONC|
| SENGRID| Output| Last hour's sensitivity fields to be used as initial conditions for the following time period| CGRID|
| SENWDEP| Output| Sensitivities of wet deposited species| WETDEP1|
| SENDDEP| Output| Sensitivities of dry deposited species| DRYDEP|


## 11.5 DDM-3D Benchmark Test Case
See the [CMAQ-DDM-3D Benchmark Tutorial](Tutorials/CMAQ_UG_tutorial_DDM3D.md) for step-by-step instructions for running the 2 day benchmark case.  The input files for the DDM-3D benchmark case are the same as the benchmark inputs for the base model. Output DDM files associated with the sample DDM control file `sensinput.2018_12NE3.dat` provided in this release package are included in the benchmark outputs for the base model.  

# 10.6 Summary

CMAQ-DDM-3D has proven to be a very effective tool for air quality studies.  This implementation in CMAQ has been done with the intent to provide flexibility and computational efficiency, and also maintain the base CMAQ code structure. CMAQ-DDM-3D has been found to accurately simulate sensitivity of ozone and PM species to initial conditions, boundary conditions, and emissions of precursor species. However, CMAQ-DDM-3D remains a work in progress with known shortcomings and its accuracy has not been tested for all conceivable applications. Any errors should be reported to the provided contacts.


## 10.6 References

Cohan, D.S., & Napelenok, S.L. (2011). Air Quality Response Modeling for Decision Support. Atmosphere, 2(3), 407-425. [doi: 10.3390/atmos2030407](https://www.mdpi.com/2073-4433/2/3/407)

Napelenok, S.L., Cohan, D.S., Odman, M.T., & Tonse, S. (2008). Extension and evaluation of sensitivity analysis capabilities in a photochemical model. Environmental Modelling & Software, 23(8), 994-999. [doi: 10.1016/j.envsoft.2007.11.004](https://www.sciencedirect.com/science/article/pii/S1364815207002186)

Napelenok, S.L., Cohan, D.S., Hu, Y.T., & Russell, A.G. (2006). Decoupled direct 3D sensitivity analysis for particulate matter (DDM-3D/PM). Atmospheric Environment, 40(32), 6112-6121. [doi: 10.1016/j.atmosenv.2006.05.039](https://www.sciencedirect.com/science/article/pii/S1352231006005012)

Cohan, D., Y. Hu, A. Hakami, A. Russell, 2005: Nonlinear response of ozone to emissions: source apportionment and sensitivity analysis. Environ. Sci. Technol., 39, 6739-6748.

Dunker, A., G. Yarwood, J. Ortmann, and G. Wilson, 2002: The decoupled direct method for sensitivity analysis in a three-dimensional air quality model 'Implementation, accuracy, and efficiency. Environ. Sci. Technol., 36, 2965-2976.

Dunker, A. 1984: The decoupled direct method for calculating sensitivity coefficients in chemical kinetics. J. Chem. Phys., 81, 2385-2393.

**Contact**

[Sergey L. Napelenok](mailto:napelenok.sergey@epa.gov), Computational Exposure Division, U.S. EPA

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch09_process_analysis.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch11_ISAM.md)<br>
CMAQv5.5 User's Guide <br>

<!-- END COMMENT -->
