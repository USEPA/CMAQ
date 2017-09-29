Release Date:

# Overview

CMAQv5.2 includes an optional diagnostic model configuration that provides detailed information on the modeled sulfur budget. This model version, referred to as the '''CMAQ-Sulfur Tracking Model (CMAQ-STM)''', tracks sulfate production from gas- and aqueous-phase chemical reactions, as well as contributions from emissions and initial and boundary conditions. Each tracked species is treated as other modeled species, undergoing transport (advection, diffusion, cloud-mixing) and removal by deposition (both wet and dry).

## Build Instructions

The CMAQv5.2 STM installation includes a build script for compiling a version of the CCTM instrumented with the Sulfur Tracking Model.

For installing CMAQ-STM, first download, install, and build the base version of the model]. Then download the CMAQ STM tar file and untar into the CMAQv5..2 home directory:

```
 cd $M3HOME/../
 tar xvzf CMAQv5.2_STM.Apr2014.tar.gz
 ```

Use the bldit.cctm.stm script as you would the base cctm build script.

```
 cd $M3HOME/scripts/cctm_stm
 ./bldit.cctm.stm |& tee bldit.cctm.stm.log
 ```
Note that you will need to have the libraries  (I/O API, netCDF, MPI, Stenex, and Pario) and model builder (bldmake) required by the base model to compile this version of the code. See the base model README for instructions on building these components.

## Run Instructions

A sample run script is provided in the STM release package under $M3HOME/scripts/cctm_stm.

The CMAQ STM test run uses the same input data as the base CMAQv5.2 distribution package.  To run the CMAQ STM test case:

1. Download the base CMAQv5.2 distribution, including the model and input data to obtain/prepare inputs for CMAQ STM.  
2. Run the ICON and BCON processors from the base model package to create initial and boundary conditions input files for the CMAQ STM test case.
3. Point the CMAQ STM run script to the emissions and ICBC data from the base CMAQv5.0.2 distribution
4. Execute the CMAQ STM run script the same way that you would run the base model

## CMAQ STM Input/Output Data

CMAQ-STM will work with initial conditions, boundary conditions, and emissions files available for use with the standard, non-instrumented CCTM. No additional processing is required to prepare input data for this model version.

The output concentration and deposition files will include additional species beyond the normal species list. These new output species are listed in Table 1 below.

Sulfur Tracking Species List

|Species Name|Description|
|--------|--------------------------------|
ASO4AQH2O2J|ASO4J produced by aqueous-phase hydrogen peroxide oxidation reaction:H<sub>2</sub>O<sub>2</sub> + S(IV) -> S(VI) + H<sub>2</sub>O</div>|
| ASO4AQO3J|ASO4J produced by aqueous-phase ozone oxidation reaction:O<sub>3</sub> + S(IV) -> S(VI) + O<sub>2</sub>|
| ASO4AQFEMNJ|ASO4J produced by aqueous-phase oxygen catalyzed by Fe<sup>+++</sup> and Mn<sup>++</sup> oxidation reaction: O<sub>2</sub> + S(IV) -> S(VI)|
| ASO4AQMHPJ|ASO4J produced by aqueous-phase methyl hydrogen peroxide oxidation reaction:MHP + S(IV) -> S(VI)|
|ASO4AQPAAJ|ASO4J produced by aqueous-phase peroxyacetic acid oxidation reaction:PAA + S(IV) -&gt; S(VI)|
|ASO4GASI|ASO4I nucleation and/or condensation following gas-phase reaction:OH + SO<sub>2</sub> -> SULF + HO<sub>2</sub>|
| ASO4GASJ|ASO4J condensation following gas-phase reaction:OH + SO<sub>2</sub> -&gt; SULF + HO<sub>2</sub>|
| ASO4GASK|ASO4K attributed to the gas-phase reaction:OH + SO<sub>2</sub> -&gt; SULF + HO<sub>2</sub>|
|ASO4EMISI|ASO4I from source emissions|
|ASO4EMISJ|ASO4J from source emissions|
|ASO4EMISK|ASO4K from source emissions|
|ASO4ICBCI|ASO4I from boundary and initial conditions|
|ASO4ICBCJ|ASO4J from boundary and initial conditions|
|ASO4ICBCK|ASO4K from boundary and initial conditions|
| SULF_ICBC|Sulfuric acid vapor (SULF) from boundary and initial conditions|

## Contact
'''[mailto:roselle.shawn@epa.gov Shawn Roselle]''', Atmospheric Modeling and Analysis Division, U.S. EPA
