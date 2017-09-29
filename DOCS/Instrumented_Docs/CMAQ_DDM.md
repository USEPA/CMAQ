Original Release Date: September 29, 2017

# Overview


The '''CMAQ Decoupled Direct Method in 3 Dimensions (CMAQ-DDM-3D)''' is a sensitivity analysis technique for computing sensitivity coefficients simultaneously while air pollutant concentrations are being computed (see References). The sensitivity coefficients represent the change in concentration, of any modeled species at any modeled time, associated with a change in a model input (e.g., an initial condition, boundary condition or emission rate) or a parameter (e.g., a reaction rate).

CMAQ-DDM-3D generates concentration outputs that are essentially identical to normal CMAQ results, while simultaneously computing sensitivity coefficients for any species concentration to a change in initial conditions, boundary conditions, or emission rates.  We have attempted to adhere to CMAQ programming conventions.  Some changes have been made to the input and output of data to keep the sensitivity output files to a reasonable size and to facilitate the necessary passing of information from mother domains to daughter domains for nested runs (see Output of Data).

Current DDM-3D implementation is available for version 5.2 of the Community Multiscale Air Quality (CMAQ) model.  This version of the code has been the work of Sergey L. Napelenok, and is based on previous contributions from Wenxian Zhang, Daniel Cohan, Ted Russell, Yueh-Jiun Yang, Amir Hakami and James Boylan.  This code can be compiled and executed in parallel mode identically to the base CMAQ model to take advantage of available multiprocessing capabilities.

This guide is intended to assist users of our implementation of DDM sensitivity analysis into CMAQ.  We acknowledge that our implementation is a work in progress and list cautionary notes (see Shortcomings and Unimplemented Features) that should be considered when using CMAQ-DDM-3D. However, we have tested that for ozone chemistry and PM processes CMAQ-DDM-3D gives results in good agreement with sensitivities calculated by differencing multiple brute-force runs of CMAQ, at a significant savings of computational time.

CMAQ-DDM-3D is released as part of public release version 5.2.  Updates to the code include the migration to the most recent base model science (version 5.2). CMAQ 5.0.2 updates included 2nd order sensitivity calculation for particulate matter species, and improved flexibility in the control file.

Implementation approach/methodology remains largely unchanged, so only the major changes are described in this document.

## Implementation Highlight:  CMAQv5.2

CMAQ-DDM-3D for model release verion 5.2 followed the same science updates and code restructuring as the base model.  No major functionality in the sensitivity model was added at this update.

## Implementation Highlight:  CMAQv5.0.2 High-Order DDM-3D for Particulate Matter (HDDM3D/PM)

:Wenxian Zhang and Sergey Napelenok

High-order DDM sensitivity analysis for particulate matter was implemented in CMAQv5.0.2.  This new feature is an important extension of the CMAQ-DDM-3D and provides an advanced and efficient approach to calculate high-order sensitivity coefficients of particulate matter. The development and implementation process as well as the performance evaluation can be found in Zhang et al. (2012).

Implementation of HDDM3D/PM involves both aero6 and cloud modules. The key step of implementing HDDM3d/PM is developing high-order DDM sensitivity analysis in ISORROPIAv2.1.  The performance of the sensitivity calculation is improved by using the following approaches:     

    * A case-specific approach that tracks the subcase in ISORROPIAv2.1
    * Including the sensitivities of activity coefficients and water content in the calculation of both first- and second-order sensitivities of the aerosol species
    * Treating acidic and neutral particles in different manners to be consistent with the algorithms used by ISORROPIAv2.1

### Impact of the implementation

    * Extended the model's ability to account for high-order sensitivities of particulate matter
    * Enabled a series of studies and applications associated with the nonlinear response of particulate matter to precursors, which is a critical factor to consider in particulate matter management
    * Significantly improved the performance of first-order sensitivities of ammonium and nitrate aerosols

New files: 

```
src/ddm3d/aero_ddmsens.f
src/ddm3d/aero_hddmsens.f
```

Affected files: src/ddm3d/aero_sens.F, src/aero/aero6/isocom.f, src/aero/aero6/isofwd.f, src/cloud/acm_ae6/aqchem.F

### Current status of CMAQ v5.2 High-order Implementation

Although the v5.0.2 implementation of HDDM-3D was extented to version 5.2, it should be considered a research option at this time.  The model does compile and simulates do complete without non-physical results (as of current testing). The performance compared to brute force is currently good for gaseous model species, but is objectively poor for secondary particulates.  There are issues with thermodynamics are are currently under investigaton. This fuctionality will be update to full use status in the future.

## Getting the CMAQ Repository for the first time
This CMAQ Git archive is organized with each official public release stored as a branch on the main USEPA/CMAQ repository.
To clone code from the CMAQ Git archive, specify the branch (i.e. version number) and issue the following command from within
a working directory on your server:

```
git clone -b 5.2_DDM-3D https://github.com/USEPA/CMAQ.git CMAQ_REPO
```

## Switching branches to 5.2_DDM-3D within an existing local CMAQ Repository (skip these instructions step if you have already cloned the 5.2_DDM-3D branch in the step above)
Check on the current status of your repository

```
git status
```
 
Result will be something like this:

``` 
On branch 5.2
Your branch is up-to-date with 'origin/5.2'.
 
Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git checkout -- <file>..." to discard changes in working directory)
 
                modified:   CCTM/scripts/bldit_cctm.csh
                modified:   CCTM/scripts/run_cctm.csh
                modified:   config_cmaq.csh
``` 
 
If there are any files that have been changed locally, that you want to save, use the following command:

```
git stash
```
 
Use the following command to bring your local repository up to date with the EPA github site:

```
git pull
```
 
Result: this will bring down the new branch

``` 
From https://github.com/USEPA/CMAQ
   409467d..68330b8  5.2        -> origin/5.2
   8633653..8fa4f10  5.0.2      -> origin/5.0.2
 * [new branch]      5.2_DDM-3D -> origin/5.2_DDM-3D
``` 
 
Then, to use the new DDM-3D code, switch branches using the command:

```
git checkout 5.2_DDM-3D
```
 
Then use the following command to restore the files that you had modified for your local machine.

```
git stash apply
```


# Build Instructions

The CMAQv5.2 DDM-3D installation includes a build script.


First build a new project
```
 ./bldit_project.csh <project_name>
```
Change to this new project directory location

```
cd $CMAQ_REPO/<project_name>
```

Edit the config_cmaq.csh script to specify the path to the compiler and required libraries.

Change to the CCTM/scripts directory.

```
cd $CMAQ_REPO/<project_name>/CCTM/scripts
```

Run the bldit_cctm.csh script 

```
 ./bldit_cctm.csh |& tee bldit_cctm_ddm.log
```


# Run Instructions

A sample run script is provided in the CMAQ-DDM-3D release package under $CMAQ_REPO/CCTM/<project_name>/scripts/run_cctm.csh. Along with the run time options for the base CCTM, this script includes DDM configuration options shown in Table 1. A DDM control input file is required when DDM3D is activated in the CCTM. Details on this file are included in the following section.

The CMAQ-DDM-3D test run uses the same input data as the base CMAQv5.2 distribution package.  To run the CMAQ-DDM-3D test case:

1. Download the base CMAQv5.2 distribution, including the model and input data to obtain/prepare inputs for CMAQ-DDM-3D.  
2. Run the ICON and BCON processors from the base model package to create initial and boundary conditions input files for the CMAQ DDM test case.
3. Point the CMAQ-DDM-3D run script to the emissions and ICBC data from the base CMAQv5.2 distribution
4. Confirm that the run script is pointing to the DDM control input file for the test run
5. Execute the CMAQ-DDM-3D run script the same way that you would run the base model


DDM run script settings

|Option | Settings | Description|
|:-------------:|:-------------:|-----|
|CTM_DDM3D|Y/N|Activate the DDM-3D calculations; requires that the CCTM was compiled for DDM simulations
|CTM_NPMAX|#|Number of sensitivity parameters|
|DDM3D_HIGH|Y/N|Allow higher order sensitivity parameters|
|DDM3D_RGN|Y/N|Use an input file to specify DDM source regions|
|DDM3D_BCRGN|Y/N|Use an input file to specify DDM boundary regions
|DDM3D_ES|Y/N|Split the emissions into different categories|
|DDM3D_RST|Y/N|Begin sensitivities from a restart file|
|DDM3D_BCS|Y/N|Use a sensitivity BC file for nested simulations|
|SEN_INPUT||Name of the sensitivity control file|

# CMAQ-DDM-3D Input/Output Data

Users must specify the DDM-3D sensitivity parameters in the DDM-3D Control File `SEN_INPUT`

## CMAQ-DDM-3D Control File (SEN_INPUT) Description

The DDM-3D Control File accommodates various types of sensitivity configuration parameters for CMAQ-DDM-3D simulations, including the tagging of multiple emission sources. As proper formatting of this file is required, users are referred to the sample control file provided with the release for formatting examples. Sample control file packets are shown below.

Calculate the sensitivity to total emissions of a particular species (SO2 in this example):

    SENNAME     
     EMIS
      TOTA
     SPECIES
      SO2

Calculate the sensitivity to emissions from gridded files (2D or 3D). The keyword "GRID" is followed by an integer (1 in this example) that defines the number of files to use (in a sum). One or more environment variables then follow that point to the emissions filename(s) (EGRIDFILE1...EGRIDFILE#). These environment variables, with the full path to the files, must be defined in the run script:

    SENNAME     
     EMIS
      GRID
      1
      EGRIDFILE1
     SPECIES
      SO2

Calculate the sensitivity to emissions from inline point sources. The keyword "PT3D" in this example is followed by an integer (2) that defines the number of stack/surface file pairs. One or more environment variables then follow that point to the emissions filename(s) (PT3DSTACK1...PT3DSTACK#, PT3DFILE1...PT3DFILE#). These environment variables, with the full path to the files, must be defined in the runscript.  The stack file variable (PT3DSTACK) must appear directly above the corresponding surface emissions file variable (PT3DFILE):

    SENNAME     
     EMIS
      PT3D
      2
      PT3DSTACK1
      PT3DFILE1
      PT3DSTACK2
      PT3DFILE2
     SPECIES
      SO2

Calculate the sensitivity to inline biogenic emissions:

    SENNAME     
     EMIS
      BEIS
     SPECIES
      ISOP

Several sensitivities can be calculated in one simulation. In the example below, there are four sensitivities defined in this control file.  In the first (EMISSO2), DDM-3D sensitivities would be calculated to emissions of SO2 from one gridded emissions file and two point source emissions files together.  In the second (EMISNOX), DDM-3D sensitivities would be calculated to emissions of NOx (NO+NO2). In the third (2ENOX), higher order DDM3D sensitivities would be calculated to NOx emissions. In the fourth (RATE1), DDM sensitivities would be calculated to the rate of reaction 1 in the photochemical mechanism.

    EMISSO2     
     EMIS
      GRID
      1
      EGRIDFILE1
      PT3D
      2
      PT3DSTACK1
      PT3DFILE1
      PT3DSTACK2
      PT3DFILE2
     SPECIES
      SO2

    EMISNOX
     EMIS
     SPECIES
      NO
      NO2

    2ENOX
     HIGH
     EMISNOX
     EMISNOX

    RATE1
     RATE
     REACTION
      1

CMAQ-DDM-3D is flexible in the number of files that the code can handle and also allows for expansion to other types of emissions (currently lightning and dust are being implemented).

## DDM-3D Control File Format

For each sensitivity:
1. (mandatory) The first line is the name of the sensitivity parameter; any 8-character name of the user's choosing, no leading spaces
2. (mandatory) The next line specifies the type of sensitivity (One leading space followed by 4 capitalized characters)
      * EMIS: Emissions
      * INIT: Initial Conditions
      * BOUN: Boundary Conditions
      * RATE: Reaction rate
      * HIGH: Higher-order sensitivity.

3. (mandatory)
      * For EMIS, INIT, or BOUN sensitivity: The term ' SPECIES' (all-cap, one leading space) must appear next.
      * For RATE sensitivity: The term ' REACTION' (all-cap, one leading space must appear next.
      * For HIGH sensitivity: The next 2 lines must each be one leading space followed by the name of the sensitivity to which we're taking higher order sensitivity. That name must have already been defined as the name of a sensitivity parameter. No further information should be defined for a higher-order sensitivity parameter.
4. (mandatory)
      * For EMIS, INIT, or BOUN sensitivity: Specify one or more species.  Names must have two leading spaces and then exactly match a species from $M3MODEL/include/release/saprc99_ae3_aq/GC_SPC.EXT.
      * For RATE sensitivity: Specify one or more reactions.  Names must have two leading spaces and then exactly match the _label_ from mech.def (also in RXDT.EXT).
5. (optional) The term ' AMOUNT' (all-cap, one leading space).  This may be used only for emissions (EMIS).  After ' AMOUNT', the next line may either be:
     * a positive real number to set the hourly amount of emissions
     * a negative number, followed by 24 numbers which specify the hourly emissions rates.
     * NOTE: All numbers must be followed by two leading spaces.
     * NOTE2: Amount is the emission rate from each gridcell of the desired region (as specified by GRIDCELL and/or REGION).
     * NOTE3: If amount is specified, amount numbers must be given for each species listed under SPECIES.  Either 1 amount number (for a continuous, constant amount) or 25 amount numbers (a negative followed by 24 hourly values) must be specified FOR EACH SPECIES.
     * NOTE4: AMOUNT is assumed to be a ground-level emission (layer 1). The LAYER term (below) must be used to change this if the desired emitting gridcell or region is not layer 1. DDM is not currently set up to allow "AMOUNT" to be used with emissions into more than one layer.
     * DEFAULT: sensitivity to emissions is assessed relative to 100% of the emissions inventory.
6. (optional) The term '  LAYER' (all-cap, 2 leading spaces). Can be used only following AMOUNT. If this term is used, next line must give an integer (3-leading spaces) of the layer in which emissions are released. Only one layer may be set. DEFAULT: AMOUNT is assumed to be a ground-level emission (layer 1).
7. (optional) The term ' DATE' (all-cap, one leading space). If this term is used, the next line must give an integer (2-leading spaces) saying how many dates are desired.  The following line(s) must give the datenumbers that are desired, one date per line (yyyyddd format). DEFAULT: all dates will be used.
8. (optional) The term ' TIME' (all-cap, one leading space). If this term is used, the next line must contain two numbers (two leading spaces, hhmmss, space, hhmmss).  The numbers specify the beginning and end time of the desired interval.  To straddle midnight, the first number may be later than the second number. DEFAULT: all times will be used.
9. (optional) The following three terms are used to specify IREGION, which defines the region over which a sensitivity parameter applies.  If more than one of the following choices are used, IREGION will combine the regions, gridcells and corner-regions specified.  DEFAULT: If neither REGIONS nor GRIDCELLS nor CORNERS nor CIRCLES are specified, domainwide is assumed. NOTE: If multiple forms are used, they must be specified in the order REGIONS, GRIDCELLS, CORNERS, then CIRCLES.
     * The term ' REGIONS' (all-cap, one leading space). If this term is used, the next line must give an integer (2-leading spaces) number of regions to be defined. The following lines are used to specify regions from a regions netcdf file, and must exactly match the region name (2-leading spaces).
     *  The term ' GRIDCELLS' (all-cap, one leading space). If this term is used, the next line must give an integer (2-leading spaces) number of gridcells to be defined. The following lines specify one gridcell per line with format __##_##_## i.e., 2 leading spaces, 2-digit column number, space, 2-digit row number, space, 2-digit level number. Use 99 for the level number to include all levels.
     *  The term ' CORNERS' (all-cap, one leading space). If this term is used, the next line must give an integer (2-leading spaces) number of rectangular zones to be defined. The following line(s) must, for each zone, give 6 integers in the format '  C1 C2 R1 R2 L1 L2' (all 2 digit numbers, 2 leading spaces, 1 space between each number).  The numbers represent a region bounded by these minimum and maximum columns numbers (C1,C2), row numbers (R1,R2), and layer numbers (L1,L2).
     *  The term ' CIRCLES' (all-cap, one leading space). If this term is used, the next line must give an integer number of circular (or ring-       shaped) zones to be defined. The following line must give the gridsize in km. The following line(s) must, for each circular/ring zone, give 4 numbers in a single line: COL_CENTER, ROW_CENTER, MINRAD(in km), MAXRAD(in km). Cells will be included in the ring iff the distance from that cell to the center cell is .GE. MINRAD, and .LE. MAXRAD.
10. A line 'END' (no leading spaces) signifies the end of the list. A line with no leading spaces and any other name is interpreted as a new sensitivity parameter (return to step 1).
     * NOTE1: This list must be consistent with the max # of sens parameters (NPMAX) set in the runscript.
     * NOTE2: For better understanding of how this file is read, or to modify/add features, look at sinput.F in the CMAQ-DDM code.


## DDM-3D Input/Output Files

With the exception of the control file, CMAQ-DDM-3D requires the same input files as a normal CMAQ run.  Additional input files may be required depending on the choice of calculated sensitivity parameters.  The following table includes a list of all possible files specific to sensitivity calculations.

Files Specific to DDM-3D Simulations

|File|Type|Contains|Base model analog|
|----|----|--------|-----------------|
| ASENS|Output| Averaged hourly sensitivities. List defined by 'AVG_CONC_SPCS' variable in the run script.|ACONC|
| SENGRID| Output| Last hour's sensitivity fields to be used as initial conditions for the following time period| CGRID|
| SENWDEP| Output| Sensitivities of wet deposited species| WETDEP1|
| SENDDEP| Output| Sensitivities of dry deposited species| DRYDEP|
| REGIONS| Input| Regional definitions| N/A|
| EGRIDFILE1| Input| Gridded emissions file 1| N/A|
| EGRIDFILE2| Input| Gridded emissions file 2| N/A|
| EGRIDFILEn| Input| Gridded emissions file n| N/A|
| PT3DFILE1| Input| Inline point source emissions file 1| N/A|
| PT3DFILE2| Input| Inline point source emissions file 2| N/A|
| PT3DFILEn| Input| Inline point source emissions file n| N/A|
| PT3DSTACK1| Input| Inline point source stack groups file 1| N/A|
| PT3DSTACK2| Input| Inline point source stack groups file 2| N/A|
| PT3DSTACKn| Input| Inline point source stack groups file n| N/A|
| BNDY_GASC_S| Input| Sensitivity field boundary conditions| BCON|
| INIT_GASC_S| Input| Sensitivity field initial conditions| ICON|

# Summary

CMAQ-DDM-3D has proven to be a very effective tool for air quality studies.  This implementation in CMAQ has been done with the intent to provide flexibility and computational efficiency, and also maintain the base CMAQ code structure. CMAQ-DDM-3D has been found to accurately simulate sensitivity of ozone and PM species to initial conditions, boundary conditions, and emissions of precursor species. However, CMAQ-DDM-3D remains a work in progress with known shortcomings and its accuracy has not been tested for all conceivable applications. Any errors should be reported to the contacts provided on the cover page.

# References

Cohan, D., Y. Hu, A. Hakami, M. T. Odman, A. Russell, 2002: Implementation of a direct sensitivity method into CMAQ. Models-3 User's Workshop, RTP, North Carolina, October 22, 2002. Available at:   www.cmascenter.org/workshop/session5/cohan_abstract.pdf

Cohan, D., Y. Hu, A. Hakami, A. Russell, 2003: Sensitivity Analysis of Ozone in the Southeast. Models-3 User's Workshop, RTP, North Carolina, October 27, 2003. Available at: www.cmascenter.org/2003_workshop/session2/cohan_abstract.pdf

Cohan, D., Y. Hu, A. Hakami, A. Russell, 2005: Nonlinear response of ozone to emissions: source apportionment and sensitivity analysis. Environ. Sci. Technol., 39, 6739-6748.

Cohan, D., D. Tian, Y. Hu, and A. Russell (2006). Control strategy optimization for attainment and exposure mitigation: Case study for ozone in Macon, Georgia. Environmental Management, 38, 451-462.

Cohan, D., Y. Hu, and A. Russell (2006). Dependence of ozone sensitivity analysis on grid resolution. Atmospheric Environment, 40, 126-135.

Dunker, A., 1981: Calculation of sensitivity coefficients for complex atmospheric models. Atmos. Environ., 15, 1155-1161.

Dunker, A. 1984: The decoupled direct method for calculating sensitivity coefficients in chemical kinetics. J. Chem. Phys., 81, 2385-2393.

Dunker, A., G. Yarwood, J. Ortmann, and G. Wilson, 2002: The decoupled direct method for sensitivity analysis in a three-dimensional air quality model 'Implementation, accuracy, and efficiency. Environ. Sci. Technol., 36, 2965-2976.

Napelenok, S., D. Cohan, Y. Hu, A. Russell, 2006: Decoupled direct 3D sensitivity analysis for particulate matter (DDM-3D/PM). Atmos. Environ., 40, 6112-6121.

Napelenok, S., D. Cohan, M.T. Odman, S. Tonse, 2008: Extension and evaluation of sensitivity analysis capabilities in a photochemical model. Environ. Model. & Software., 23(8), 994-999.

Yang, Y-J, J. Wilkinson, and A. Russell, 1997: Fast, direct sensitivity analysis of multidimensional photochemical models. Environ. Sci. Technol, 31, 2859-2868.

Zhang, W., Capps, S. L., Hu, Y., Nenes, A., Napelenok, S. L., and Russell, A. G.: Development of the high-order decoupled direct method in three dimensions for particulate matter: enabling advanced sensitivity analysis in air quality models, Geosci. Model Dev., 5, 355–368, doi:10.5194/gmd-5-355-2012, 2012.

# Contact

'''[mailto:napelenok.sergey@epa.gov Sergey L. Napelenok]''', Atmospheric Modeling and Analysis Division, U.S. EPA
