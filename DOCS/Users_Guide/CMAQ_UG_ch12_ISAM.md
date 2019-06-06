
<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch11_HDDM-3D.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch13_sulfur_tracking.md)

<!-- END COMMENT -->

# 12. ISAM
## 12.1 Overview

The Integrated Source Apportionment Method (ISAM) calculates source attribution information for user specified ozone and particulate matter precursors within the CMAQ model.  CMAQ-ISAM has been substantially updated in CMAQv5.3 release.

The CMAQ model provides users the concentration and deposition fields of many pollutant species. These species are usually combinations of different types of primary emissions and secondary formation that have been physically and chemically transformed in the model. However, sometimes it's desirable to know specific source attribution information for the model outputs. For example, how much of the ozone in an urban area was formed due to nitrogen oxides emitted from motor vehicles in a neighboring state?

Answering this type of question often requires running an air quality model twice, once with the standard emissions scenario and once with the source of interest completely removed. The difference between the two runs is then assumed to be attributed to the removed source.  While this approach is reasonably straightforward to implement, it has some drawbacks.  For example, removing a large source from the system in a highly nonlinear chemical mixture can lead to some errors. Also, calculating source attribution of many sources can be logistically and computationally prohibitive.

Alternatively, running CMAQ with ISAM enabled allows the user the ability to calculate source attribution of a large number of sources directly by the model in one simulation.

Note: While full model species list apportioment is in development, currently ISAM is limited to the following species classes in CMAQ:

```
SULFATE   - ASO4J, ASO4I, SO2, SULF, SULRXN
NITRATE   - ANO3J, ANO3I, HNO3, ANO3J, ANO3I, HNO3, NO, NO2, NO3, HONO, N2O5, PNA, PAN, PANX, NTR1, NTR2, INTR
AMMONIUM  - ANH4J, ANH4I, NH3
EC        - AECJ, AECI
OC        - ALVPO1I, ALVPO1J, ASVPO1I, ASVPO1J, ASVPO2I, ASVPO2J, ASVPO3J, AIVPO1J, VLVPO1, VSVPO1, VSVPO2, VSVO3, VIVPO1
VOC       - 21 species in CB6R3 (see OZ_DEFN.F)
PM25_IONS - ACLI/J,ANAI/J,AMGJ,AKJ,ACAJ,AFEJ,AALJ,ASIJ,ATIJ,AMNJ,AOTHRI/J
CO        - CO
OZONE     - all NITRATE species + all VOC species
```

## 12.2 Build Instructions

Starting with CMAQv5.3 model release, ISAM is provided directly with the source code of the base model.  In order to use ISAM, build CMAQ as normal with the provided scripts.  Then, exercise the ISAM preprocessor compiler option using the following command, while in the BUILD directory:

```
make isam=TRUE
```

Note, you may want to 'make clean' first, if your scripts were configured to automatically compile the model.

## 12.3 Run Instructions

To begin a CMAQ simulation with apportionment enabled, the ISAM section of the runscript must be configured.  The additional necessary environment variables are listed in Table 12-1.

**Table 12-1: ISAM run script variables**

|**Variable** | **Settings** | **Description**|
|-------|----------|------------|
|CTM_ISAM|Y/N|Set this to Y to enable ISAM|
|SA_IOLIST|path/filename|Provide the location of the ISAM control file (discussed below)|
|ISAM_BLEV_ELEV|" MINVALUE MAX VALUE "|LAYER range for the instantaneous ISAM output concentrations|
|AISAM_BLEV_ELEV|" MINVALUE MAX VALUE "|LAYER range for the average ISAM output concentrations|
|ISAM_NEW_START|Y/N|set Y for a new simulation and N for continuing from a previous day's outputs|
|ISAM_PREVDAY|path/filename|Provide the location of the previous day's ISAM restart file|
|SA_ACONC_1|path/filename|ISAM output for average apportioned concentrations|
|SA_CONC_1|path/filename|ISAM output for instanteneous apportioned concentrations|
|SA_DD_1|path/filename|ISAM output for apportioned dry deposition|
|SA_WD_1|path/filename|ISAM output for apportioned wet deposition|
|SA_CGRID_1|path/filename|ISAM output for a restart file to continue the simulation further in time|

Additionally, ISAM is able to track emissions confined to geographic regions.  This functionality can enabled through CMAQ's `RegionsRegistry` set in the `EmissCtrl` namelist and is discussed further below.


!!!! Add cross-reference to the map stuff wherever it is !!!!


### 12.3.1 ISAM control file (SA_IOLIST)

The ISAM `SA_IOLIST` is a text file used to configure which tag classes, emissions streams, and source regions the model will track.  An example of this file, `isam_control.txt`, is provided in the release package.  The formating of this file must be kept intact, but it does allow for insertion of comment lines.  

Each ISAM simulation requires the specification of the `TAG CLASSES` that the user desires to apportion.  The current list includes the following choices `SULFATE, NITRATE, AMMONIUM, EC, OC, VOC, PM25_IONS, CO, OZONE`.  Species associated with each of these are provided in section 12.1.  One or more of these tag classes must be specified in `SA_IOLIST`.  Multiple tag classes are comma delimited.

```
TAG CLASSES     |OZONE, SULFATE
```

After setting tag classes for the simulation, information for one or more tags is required. Each individual tag will track the species from the specified `TAG CLASSES` and has its own set of three options in the control file.  The first option is the name:

```
TAG NAME        |EGU
```

It is recommended that the text string for the tag name be kept short (ideally three characters) in order to accomodate the longer species names from some chemical mechanisms in the ISAM output files.

The second option is the comma delimited list of regions to track with this tag.  The keyword 'EVERYWHERE' is used to track domain-wide emissions.  To track region-constrained emissions, variable names from the regions file specified in the `EmissCtrl` namelist are used insted.

```
REGION(S)       |EVERYWHERE
```

or

```
REGION(S)       |NC, SC, GA
```

Finally, the emissions streams labels are required as the third option in the control file.  These are the labels set in the runscript for the base CMAQ simulation.  

```
EMIS STREAM(S)  |PT_EGU, PT_NONEGU
```

The final line in the control file needs to be kept unchanged in order to aid the file parser in reading this file.

```
ENDLIST eof
```

## 12.4 References

Kwok, R.H.F, S.L. Napelenok,K.R. Baker (2013) Implementation and evaluation of PM2.5 source contribution analysis in a photochemical model, Atmospheric Environment, Volume 80, 398–407, http://dx.doi.org/10.1016/j.atmosenv.2013.08.017

Kwok, R.H.F, K.R. Baker, S.L. Napelenok, G.S. Tonnesen (2014) Photochemical grid model implementation of VOC, NOx, and O3 source apportionment. Submitted to Geoscientific Model Development.

## Contact
[Sergey L. Napelenok](mailto:napelenok.sergey@epa.gov), Computational Exposure Division, U.S. EPA


<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch11_HDDM-3D.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch13_sulfur_tracking.md)<br>
CMAQ User's Guide (c) 2019<br>

<!-- END COMMENT -->
