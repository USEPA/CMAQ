
<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch11_HDDM-3D.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch13_sulfur_tracking.md)

<!-- END COMMENT -->

# 12. ISAM
## 12.1 Overview

The Integrated Source Apportionment Method (ISAM) calculates source attribution information for user specified ozone and particulate matter precursors within the CMAQ model.

The CMAQ model provides users the concentration and deposition fields of many pollutant species. These species are usually combinations of different types of primary emissions and secondary formation that have been physically and chemically transformed in the model. However, sometimes it's desirable to know specific source attribution information for the model outputs. For example, how much of the ozone in an urban area was formed due to nitrogen oxides emitted from motor vehicles in a neighboring state?

Answering this type of question often requires running an air quality model twice, once with the standard emissions scenario and once with the source of interest completely removed. The difference between the two runs is then assumed to be attributed to the removed source.  While this approach is reasonably straightforward to implement, it has some drawbacks.  For example, removing a large source from the system in a highly nonlinear chemical mixture can lead to some errors. Also, calculating source attribution of many sources can be logistically and computationally complex.

Alternatively, running CMAQ using ISAM allows the user the ability to calculate source attribution of a large number of sources directly by the model in one simulation.

**>>Comment<<** The following text was pulled from the ISAMv5.0.2 documentation.  Some updating is still needed to reflect CMAQv5.3 changes.
## 12.2 Build Instructions

The CMAQv5.2 ISAM installation includes a build script for compiling a version of the CCTM instrumented with ISAM.  For installing CMAQ-ISAM, first download, install, and build the base version of the model. Then download the CMAQ ISAM tar file and untar into the CMAQv5.2 home directory:

```
 cd $CMAQ_REPO/
 tar xvzf CMAQv5.2_ISAM.tar.gz
 ```

Use the bldit.cctm.isam script as you would the base cctm build script.

```
 cd $M3HOME/scripts/cctm_isam
 ./bldit.cctm.isam |& tee bldit.cctm.isam.log
```
**>>Comment<<**  $M3HOME is no longer in CMAQ right? 


Note that you will need to have the libraries  (I/O API, netCDF, MPI, Stenex, and Pario) and model builder (bldmake) required by the base model to compile this version of the code. See the base model README for instructions on building these components.

## 12.3 Run Instructions

A sample run script is provided in the ISAM release package under $M3HOME/scripts/cctm_isam. Along with the run time options for the base CCTM, this script includes the ISAM configuration options shown in Table 1.

The CMAQ ISAM test run uses the same input data as the base CMAQv5.2 distribution package.  To run the CMAQ ISAM test case:  
**>>Comment<<**  I am confused. CMAQ base test case already has ICBC. does CMAQ ISAM use different ICON and BCON from base case?
Following steps should be clearer.

   * Download the base CMAQv5.2 distribution, including the model and input data to obtain/prepare inputs for CMAQ ISAM.  
   * Run the ICON and BCON processors from the base model package to create initial and boundary conditions input files for the CMAQ ISAM test case.
   * Point the CMAQ ISAM run script to the emissions and ICBC data from the base CMAQv5.2 distribution
   * Execute the CMAQ ISAM run script the same way that you would run the base model

**Table 8-1: ISAM run script settings**

|**Option** | **Settings** | **Description**|
|-------|----------|------------|
|CTM_PT3DEMIS|Y/N|This is a setting in the base CCTM; set to "Y" for tagging inline point sources|
|LETSPRINT|Y/N|Set to Y to write ISAM logging to a file|
|DO_RENORM|Y/N|Set to Y to do renormalization; set to N otherwise|
|YES_OXLOSS|Y/N|Set to N for production and loss of O3 alone; Y for O3+NO2, but result accuracy is not guaranteed.|

**>>Comment<<**  not real input/output data. it does not use any input data right? May just call CMAQ ISAM input files.
## 12.4 CMAQ ISAM Input/Output Data

`SRC_CONTROL_FILE` is a required input to ISAM. It is a text file that specifies the emission tagging inputs for ISAM. An example control file, sa_input_ctrl.txt, is provided in the release package.

`SRC_APP_MAP` is an optional input to ISAM that defines emissions tagging source regions. The I/O API gridded map file can be created using the Spatial Allocator with an input shapefile to compute an area percentage for each state or other region of interest.  An example shapefile (statefp_mask_all) containing the State Abbreviations and the corresponding STATEFP code with the value for that state set to 1 and the value for all other states set to zero has been provided in a src_app_map.tar.gz file available for download with the CMAQ-ISAM distribution. An example alloc_create_areapercent.csh script is provided to calculate an area percentage from the shapefile to create a src_app_map file for the benchmark case.  The variable names in the src_app_map file are typically state or county names and are limited to 16 characters.  An example map file for the state of NC, state_NC_CMAQ-BENCHMARK.ncf, is provided in the release package. The values of the variables are the fractions of each state or county in each model grid cell, NC_0 contains the fractions outside of NC, NC_1 contains the fractions within NC. The file is grid-dependent, two-dimensional and time independent. The NC_1 variable can be used as the REGION in the SA Control File.

### 12.4.1 SA Control File

The SA Control File (SRC_CONTROL_FILE) is used to generate a list of tags in a temporary text file "sa_io_list.tmp". This text file will be read by the CCTM instrumented with ISAM to determine the number of tags, the tracking species, and the total number of the combined tags and species.

Suppose there are eight groups of emissions to be tagged, five inline point sectors (1-5) and three surface emissions sectors (6-8). The CCTM ISAM run script sets the environment variables for the tagged emission files SG01, SGO2,...,SG08, and for the corresponding stack group files STK01, STK02, ..., STK08. All inline point source emissions (SG01 to SG05) must be paired to their corresponding stack group files (STK01 to STK05). The gridded surface emission sectors (SG06 to SG08) do not need any stack group files, so STK06 to STK08 would be left blank. An example of how these variables are set in the script is shown below.

```
 setenv SG01 $STK_EMIS_01;setenv STK01 $STK_GRPS_01
 setenv SG02 $STK_EMIS_02;setenv STK02 $STK_GRPS_02
 setenv SG03 $STK_EMIS_03;setenv STK03 $STK_GRPS_03
 setenv SG04 $STK_EMIS_04;setenv STK04 $STK_GRPS_04
 setenv SG05 $STK_EMIS_05;setenv STK05 $STK_GRPS_05
 setenv SG06 $EMIS_ONROAD;setenv STK06
 setenv SG07 $EMIS_OFFROAD;setenv STK07
 setenv SG08 $EMIS_BIOG;setenv STK08
 ```

The example SA Control File below shows how this group of eight emissions sources would be tagged for ISAM.

```
 cat << eof > $RUNDIR/sa_io_list_$CASE.tmp
 TAG NAME |ANTHr45
 TAG CLASSES |OZONE PM25_IONS
 REGION(S) |NC
 FILENAME(S) |SG01,SG02,SG03,SG04,SG05,SG06,SG07
 STACK FILE(S) |STK01,STK02,STK03,STK04,STK05,STK06,STK07

 TAG NAME |MOBILE
 TAG CLASSES |EC,OC,CO,AMMONIUM
 REGION(S) |EVERYWHERE
 FILENAME(S) |SG06,SG07
 STACK FILE(S) |STK06,STK07

 TAG NAME |POINT
 TAG CLASSES |SULFATE
 REGION(S) |EVERYWHERE
 FILENAME(S) |SG01,SG02,SG03
 STACK FILE(S) |STK01,STK02,STK03

 TAG NAME |BIOG
 TAG CLASSES |OZONE
 REGION(S) |EVERYWHERE
 FILENAME(S) |SG08
 STACK FILE(S) |STK08

 ENDLIST eof
 ```

In this example there are four ISAM tag names (ANTHr45, MOBILE, POINT, BIOG). ISAM will supplement the emissions tags with BCON, OTHR, and ICON tags, so that the tag total for each species conserves the bulk concentration. As the nomenclature suggests, BCON comes from lateral boundary conditions, ICON from initial conditions at the very first hour of the entire simulation period, and OTHR is the remaining "untagged" emissions sources, which is derived by subtracting emissions in the tags from the total model emissions.

**>>Comment<<** It is hard to read by following bullet points. How about presenting a full list of Control file. Then add necessary notations in each line where descriptions and restrictions are needed to be highlighted.

### 12.4.2 SA Control File Format

1. Each tag is separated by a blank line.
2. Each tag consists of the following attributes: TAG NAME, TAG CLASSES, REGION(S), FILENAME(S), and STACK FILE(S).
3. Attribute are 16 characters long; if less than 16 characters, the attribute name must be supplemented with blank characters (not tabs) up to and including the 16th column. A "|" sign should be put at the 17th column.
4. The value of the TAG NAME attribute must be 7 characters or less.
5. Except for TAG NAME, each attribute can have one or more entries, separated by a comma.
6. The first entry for each attribute must immediately follow the "|" sign, with no white space.
7. Only alphanumeric characters are allowed for the TAG NAME; the attribute value is case-insensitive.
8. Valid entries for TAG CLASSES are EC, OC, SULFATE, NITRATE, AMMONIUM, OZONE, PM25_IONS, and CO.
9. A valid entry for REGION(S) is either a variable name from $SRC_APP_MAP file, a reserved word "EVERYWHERE", or just left blank. An empty entry is equivalent to "EVERYWHERE", which instructs the program to track the emission sector over the entire domain. The empty entry is used only when $SRC_APP_MAP file is not available.
10. A blank line follows the last tag, which in turn is followed by the expression "ENDLIST".

There are restrictions on the nature of emission sectors.

1. If there are no inline point source emissions (CTM_PT3DEMIS = N)
2. The gridded base model emission EMIS_1 must be three-dimensional.
3. Gridded emission sectors SG?? can be two- or three-dimensional.
4. No inline point source emissions are tracked by ISAM.
5. If inline point source emissions are include as base model emissions (CTM_PT3DEMIS = Y)
6. The variable EMIS_1 must point to a two-dimensional gridded surface emissions file
7. Gridded emission sectors SG?? must also be two-dimensional
8. Any inline point sources to be tracked by ISAM must come from one of the inline point source emissions for base model run.
9. Excessive number of tagged emission files will exceed the limit of netCDF files opened by Models3 IO/API which is 64. Try optimizing the number of tagged files.

### 12.4.3 SA Control File for Labeled Inline Emissions

ISAM can optionally track emissions point sources if point source identifiers are included in the emissions stack group files. SMOKEv3.5.1 can optionally write point source group IDs to the stack groups file, assigning the variable name "IGROUP" for use by ISAM. The variable IGROUP in the stack groups file is an integer, with each value representing a different group of sources (i.e. fires, electricity generating units, etc). The point source group ID's are independent of and do not require the region map `$SRC_APP_MAP`. See the [SMOKEv3.5.1 manual Section 4.4.24](http://www.cmascenter.org/smoke/documentation/3.5.1/html/ch04s04s24.html) for details on how to label the emissions species for use with CMAQ ISAM.

To illustrate the usage of source IDs, consider another example with the inline emission file `$STK_EMIS_01` and the corresponding stack group file `$STK_GRPS_01`. The input control file is constructed as follows:

```
 setenv SG01 $STK_EMIS_01
 setenv STK01 $STK_GRPS_01
 cat << eof > $RUNDIR/sa_io_list_$CASE.tmp
 TAG NAME        |NIPM
 TAG CLASSES     |OZONE
 REGION(S)       |INLINE,22
 FILENAME(S)     |SG01
 STACK FILE(S)   |STK01
 ENDLIST
 eof
 ```

To track the inline point sources identified in the emissions file `STK_GRPS_01` as IGROUP=22, enter "INLINE,22" for the REGION(S) tag in the control file. The source group tracking capability is not limited to one IGROUP value in one point source. It is also valid to have:
1. Multiple inline point sources with multiple common IGROUP values
2. One inline point source with multiple IGROUP values
3. Multiple inline point sources with a single common IGROUP value

Additional rules for constructing inline point source tracking include:
1. A comma must separate the "INLINE" and IGROUP values
2. IGROUP can be either a single- or double-digit integer
3. You cannot combine "INLINE" and gridded emissions sectors in a single ISAM simulation; to activate inline tagging (and deactivate gridded emissions tagging), set REGION(S) to "INLINE" and provide an IGROUP value from the stack groups file
4. You may only combine inline point sources with gridded emission sectors when IGROUP is not used. In this case, set REGION(S) to either "EVERYWHERE" or a variable name from $SRC_APP_MAP; REGION(S) may not be set to "INLINE" when considering gridded emissions tagging.

### 12.4.4 ISAM Tag Classes

The ISAM TAG CLASSES consist of generic chemical names that determine what model species will be tracked by the algorithm. For example, by specifying SULFATE the program will fetch model species SO2, ASO4I and ASO4J for tracking and emission species SULF for updating ASO4J. When the tag class OZONE is selected, certain CB05 VOC species as well as the tag class NITRATE is automatically tracked for that tag. Selecting NITRATE again for the OZONE tag will cause the program stop with a double-counting error. See Table 8-2 for the complete list of TAG CLASSES.

**Table 8-2. Tag classes and associated species**

|TAG CLASS| Species in EMISfile| Species in IC/BC, CGRID, DRYDEP, WETDEP|
|-------|---------------|-------------------|
| EC| PEC| AECI, AECJ|
| OC| POC, PNCOM| APOCI, APOCJ, APNCOMI, APNCOMJ|
| SULFATE| SO2,&nbsp; SULF, PSO4| SO2, SULF, ASO4I, ASO4J, SULRXN|
| NITRATE| PNO3, NO2, NO, HONO| ANO3I, ANO3J, HNO3, NTR, NO2, NO, NO3, HONO, N2O5, PNA, PAN, PANX|
| AMMONIUM| NH3, PNH4| NH3, ANH4I, ANH4J|
| OZONE| PNO3, NO2, NO, HONO<br>ALD2, ALDX, ETH, EHTA, ETOH<br>FORM, IOLE, ISOP, MEOH, OLE<br>PAR, TERP, TOL, XYL| O3N, O3V<br>ANO3I, ANO3J,HNO3, NTR, NO2, NO, NO3, HONO, N2O5, PNA, PAN, PANX<br>VOC|
| PM25_IONS| PCL, PNA, PMG, PK, PCA<br>PFE, PAL, PSI, PTI, PMN<br>PMOTHR| ACLI, ACLJ, ANAI, ANAJ, AMGJ, AKJ, ACAJ<br>AFEJ, AALJ, ASIJ, ATIJ, AMNJ<br>AOTHRI, AOTHRJ|
| CO| CO| CO|

### 12.4.5 ISAM Output Files
**Table 8-3. CMAQ ISAM Output Files**

|File| Description| Base Model Analog|
|----|-------------|--------------------|
|SA_ACONC|Averaged hourly concentrations of tagged tracer species. List defined by 'AVG_CONC_SPCS' variable in the run script|ACONC|
|SA_CGRID|Last hour's concentration fields of tagged tracer species to be used as initial conditions for the following time period|CGRID|
|SA_CONC|Instantaneous hourly concentrations of tagged tracer species|CONC|
|SA_DRYDEP|Hourly dry deposition fields of tagged tracer species|DRYDEP|
|SA_WETDEP1|Hourly wet deposition fields of tagged tracer species|WETDEP1|

## References

Kwok, R.H.F, S.L. Napelenok,K.R. Baker (2013) Implementation and evaluation of PM2.5 source contribution analysis in a photochemical model, Atmospheric Environment, Volume 80, 398–407, http://dx.doi.org/10.1016/j.atmosenv.2013.08.017

Kwok, R.H.F, K.R. Baker, S.L. Napelenok, G.S. Tonnesen (2014) Photochemical grid model implementation of VOC, NOx, and O3 source apportionment. Submitted to Geoscientific Model Development.

## Contact
[Roger H.F. Kwok](mailto:kwok.roger@epa.gov) or [Sergey L. Napelenok](mailto:napelenok.sergey@epa.gov), Atmospheric Modeling and Analysis Division, U.S. EPA


<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch11_HDDM-3D.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch13_sulfur_tracking.md)<br>
CMAQ User's Guide (c) 2019<br>

<!-- END COMMENT -->
