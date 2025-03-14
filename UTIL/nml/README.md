# Convert .csv to namelist format (csv2nml) & Convert namelist format to .csv (nml2csv)



CSV2NML and NML2CSV are used to convert the species definition files from CSV format to NAMELIST files and from a NAMELIST format to a CSV file, respectively. The NAMELIST files are used as inputs to the CMAQ programs ICON, BCON, or CCTM to define the processes that will impact each model species. Four NAMELIST files define the processes for gas-phase species (GC.nml), aerosol species (AE.nml), nonreactive species (NR.nml) and tracer speces (TR.nml).

#### CSV2NML input files & NML2CSV output files

Detailed descriptions of the formats of the files shown in [Table 4-1](../../DOCS/Users_Guide/CMAQ_UG_ch04_model_inputs.md#Table4-1) are provided in [Chapter 4](../../DOCS/Users_Guide/CMAQ_UG_ch04_model_inputs.md#Table4-1). 

<a id=Table1></a>

**Table 1. CSV2NML input files & NML2CSV output files**

|**File Name**|**Format**|**Description**|
|--------|--------|--------------------------------------------------------------------------|
|GC.csv|ASCII|Gas-phase species process parameters. This file defines the source and sink processes that impact the concentrations of every gas-phase species in the chemical mechanism.|
|AE.csv|ASCII|Aerosol-phase species process parameters. This file defines the source and sink processes that impact the concentrations of every aerosol-phase species in the chemical mechanism.|
|NR.csv|ASCII|Nonreactive species process parameters. This file defines the source and sink processes that impact the concentrations of every nonreactive species in the chemical mechanism.|

<a id=Table2></a>

**Table 2. CSV2NML output files & NML2CSV input files**

|**File Name**|**Format**|**Description**|
|--------|--------|--------------------------------------------------------------------------|
|GC.nml|ASCII|Gas-phase species process parameters. This file defines the source and sink processes that impact the concentrations of every gas-phase species in the chemical mechanism.|
|AE.nml|ASCII|Aerosol-phase species process parameters. This file defines the source and sink processes that impact the concentrations of every aerosol-phase species in the chemical mechanism.|
|NR.nml|ASCII|Nonreactive species process parameters. This file defines the source and sink processes that impact the concentrations of every nonreactive species in the chemical mechanism.|
|TR.nml|ASCII|Tracer species process parameters. This file defines the source and sink processes that impact the concentrations of every tracer species in the chemical mechanism.|

#### CSV2NML & NML2CSV usage 

The CSV2NML script is configured to read in a CSV file from the command line and output a NAMELIST file that can be used with CMAQ. The CSV file to be input, currently, has to configured with a certain format to be consistent with how it is read in the existing CMAQ program suite. 

An example of how to configure your CSV file is shown below: 

<a id=Table3></a>

**Example Tracer CSV File**

|**Species**|**MOLWT**|**ICBC**|**ICBC_FAC**|**DEPV**|**DEPV_FAC**|**SCAV**|**SCAV_FAC**|**TR2AE**|**TR2AQ**|**ADVC**|**DIFF**|**DDEP**|**WDEP**|**CONC**|
|:------:|:----:|:----:|:---:|:-----:|:---:|:---:|:----:|:---:|:----:|:----:|:----:|:----:|:----:|:----:|
|O3_1|48.0|O3|1|VD_O3|1|O3|1|  |   |YES|YES|YES|YES|YES|

**NOTE: Certain columns can be left blank, if desired, like in the above example. Also, please be sure to save your CSV file as either a GC, AE, NR or TR file as seen in Table 7-11. Refer to [Chapter 4](../../DOCS/Users_Guide/CMAQ_UG_ch04_model_inputs.md) for more information.**


An example of how to use CSV2NML to create a tracer species NAMELIST file is included below:

```
cd $CMAQ_HOME/UTIL/nml/scripts
./csv2nml.csh TR.CSV
```

There is also a script to convert an existing namelist file to a CSV, the command to call this script is shown below:

```
cd $CMAQ_HOME/UTIL/nml/scripts
./nml2csv.csh TR.nml
```


