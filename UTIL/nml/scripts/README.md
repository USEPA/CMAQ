# Convert .csv to namelist format (csv2nml) & Convert namelist format to .csv (nml2csv)



CSV2NML and NML2CSV are used to convert the species definition files from CSV format to NAMELIST files and from a NAMELIST format to a CSV file, respectively. The NAMELIST files are used as inputs to the CMAQ programs ICON, BCON, or CCTM to define the processes that will impact each model species. Four NAMELIST files define the processes for gas-phase species (GC.nml), aerosol species (AE.nml), nonreactive species (NR.nml) and tracer speces (TR.nml).

#### CSV2NML input files & NML2CSV output files

Detailed descriptions of the formats of the files shown in [Table 7-11](#Table5-9) are provided in [Chapter 8](CMAQ_OGD_ch08_input_files.md). 

<a id=Table7-11></a>

**Table 7‑11. CSV2NML input files & NML2CSV output files**

|**File Name**|**Format**|**Description**|
|--------|--------|--------------------------------------------------------------------------|
|GC.csv|ASCII|Gas-phase species process parameters. This file defines the source and sink processes that impact the concentrations of every gas-phase species in the chemical mechanism.|
|AE.csv|ASCII|Aerosol-phase species process parameters. This file defines the source and sink processes that impact the concentrations of every aerosol-phase species in the chemical mechanism.|
|NR.csv|ASCII|Nonreactive species process parameters. This file defines the source and sink processes that impact the concentrations of every nonreactive species in the chemical mechanism.|
|TR.csv|ASCII|Tracer species process parameters. This file defines the source and sink processes that impact the concentrations of every tracer species in the chemical mechanism.|

#### CSV2NML output files & NML2CSV input files

<a id=Table7-12></a>

**Table 7‑12. CSV2NML output files & NML2CSV input files**

|**File Name**|**Format**|**Description**|
|--------|--------|--------------------------------------------------------------------------|
|GC.nml|ASCII|Gas-phase species process parameters. This file defines the source and sink processes that impact the concentrations of every gas-phase species in the chemical mechanism|
|AE.nml|ASCII|Aerosol-phase species process parameters. This file defines the source and sink processes that impact the concentrations of every aerosol-phase species in the chemical mechanism|
|NR.nml|ASCII|Nonreactive species process parameters. This file defines the source and sink processes that impact the concentrations of every nonreactive species in the chemical mechanism|
|TR.nml|ASCII|Tracer species process parameters. This file defines the source and sink processes that impact the concentrations of every tracer species in the chemical mechanism|

#### CSV2NML usage

The CSV2NML script is configured to read in a CSV file from the command line and output a NAMELIST file that can be used with CMAQ. The CSV file to be input, currently, has to configured in a certain format to be consistent with how it is read in the existing CMAQ program suite. 

An example of how to configure your CSV file is shown below: 

<a id=Table3></a>

**TABLE 3. Example Tracer CSV File**

|**Species**|**MOLWT**|**ICBC**|**ICBC_FAC**|**DEPV**|**DEPV_FAC**|**SCAV**|**SCAV_FAC**|**TR2AE**|**TR2AQ**|**ADVC**|**DIFF**|**DDEP**|**WDEP**|**CONC**|
|:------:|:----:|:----:|:---:|:-----:|:---:|:---:|:----:|:---:|:----:|:----:|:----:|:----:|:----:|:----:|
|O3_BC|48.0|O3|1|VD_O3|1|O3|1|    |    |YES|YES|YES|YES|YES|


An example of how to use CSV2NML to create a gas-phase species NAMELIST file is include below:

```
cd $CMAQ_HOME/UTIL/nml/scripts
./csv2nml.csh GC.CSV
```

There is also a script to convert an existing namelist file to a CSV

```
cd $CMAQ_HOME/UTIL/nml/scripts
./nml2csv.csh GC.nml
```
