# CMAQ Data
* [CMAQv6.0 Test Case Data](#v60_input_data)
* [CMAQv5.5 Test Case Data](#v55_test_case_data)
* [Input and Output Files](#cmaq_inputs_outputs)
* [EQUATES Data](#equates_data)
* [Measurement-Model "Fused" CMAQ Outputs](#cmaq_fused)

<a id=v60_input_data></a>
## CMAQv6.0 inputs for 2022
A full set of inputs for 2022 are provided for the 12US1 domain (299 column x  459 row x 35 layer, 12-km horizontal grid spacing) on Amazon Web Services (AWS) Open Data Registry, including emissions compatible with both the CRACMM2 and CRACMM3 chemical mechanisms (referred to as 2022r1 CRACMM2 emissions).<br>
<br>
Input files can be used for running CMAQv6.0 (with CRACMM2 or CRACMM3) or CMAQv5.5 (with CRACMM2)<br> 
Sample scripts and control files are provided that are specific to each emissions case. <br> 
2022r1 CRACMM2 emissions largely follow the [EPA's 2022 modeling platform inventory](https://www.epa.gov/air-emissions-modeling/2022v1-emissions-modeling-platform) methods used to generate the data available from this AWS S3 bucket except for CRACMM2 speciation ([Skipper et al., 2024](https://doi.org/10.5194/acp-24-12903-2024)).<br> 
Mobile emissions have been further updated to follow [Murphy et al. 2023](https://doi.org/10.5194/acp-23-13469-2023).<br> 
<br>
In addition, and new to CRACMM2, CRACMM2 changes how emissions inventories of primary organic aerosol (POA) are allocated to model species of different volatility and functionality. In CRACMM2, POA emissions are assigned to CRACMM species in S2S-Tool+SMOKE and present in the CMAQ-ready files with CRACMM names. The POA inventory is aligned with species of saturation concentrations of 100 µg/m3 and below as described by Pye et al. in prep. DESID should not be used to apply POA volatility profiles to emissions as they have already been applied upstream.<br>

However, in the case of the residential wood combustion files included in this dataset, an additional adjustment (not available at the time of CRACMM2 nor when these emissions were created) is recommended at runtime in DESID.  See the “Scale Fire ROC Emissions to account for organic compound volatility” section in the CMAQv6.0 alpha CMAQ_Chem_Control_cracmm3_2022r1.nml and CMAQv5.5 CMAQ_Control_DESID_cracmm2_2022r1.nml namelist files for further details. These files are located under the scripts directory linked below.<br>


<a id=v6_test_case_data></a>
Benchmark/tutorial data for the CMAQv6 release are available from the CMAS Data Warehouse.  The input and output files are stored on Amazon Web Services (AWS) Open Data Registry.  CMAQv6 benchmark input is new, providing a July 1-2, 2022 case over the Southeast US. This 12SE1 2022 input data is used for all CMAQv6 tutorials listed below.  CMAQv6 comes with new output data for running several different model configurations (links below).  Tutorials are provided for using the benchmark data to test running of the base CMAQ model with either the CB6r5 or CRACMMv3 mechanisms, CMAQ-ISAM, and CMAQ-DDM. The input datasets include a grid mask file for the United States (GRIDMASK_STATES_12SE1.nc). The grid mask file is used for running the ISAM test case, or to test out regional emissions scaling with [DESID](Users_Guide/Tutorials/CMAQ_UG_tutorial_emissions.md).  The input datasets also include an ocean file with variables needed to use the cracmm3, and cb6r5_ae7 mechanisms. See the [Ocean File tutorial](Users_Guide/Tutorials/CMAQ_UG_tutorial_oceanfile.md) for more information on changes to the required ocean file input that began in v5.4.

Note that some of the 2022 12US1 inputs netCDF-4/HDF5 compressed (*.nc4) while others are classic netCDF files (*.nc). <br>

|**CMAQ Version**|**Data Type (Size)**|**Domain**|**Simulation Dates**|**Data Access**|**Link**|**Tutorial**|
|:----:|:----:|:--------------:|:----:|:--------:|:----:|:----:|
|v6 CRACMM3 | Input (46.3 GB) | 12SE1 | July 1-2, 2022|[Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/C0VKIR)  <br /> | [Input AWS Link](https://cmas-cmaq.s3.amazonaws.com/index.html#CMAQv6.0_2022_12SE1_Benchmark_2day/) |[CRACMM3 Tutorial](Users_Guide/Tutorials/CMAQ_UG_tutorial_benchmark_cracmm3_stage.md)|
|v6 cb6r5_ae7_aq | Input (46.3 GB) | 12SE1 | July 1-2, 2022|[Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/C0VKIR)  <br /> | [Input AWS Link](https://cmas-cmaq.s3.amazonaws.com/index.html#CMAQv6.0_2022_12SE1_Benchmark_2day/) |[cb6r5 Tutorial](Users_Guide/Tutorials/CMAQ_UG_tutorial_benchmark.md)|
|v6 CRACMM3, STAGE| Output (1.9 GB) | 12SE1 | July 1-2, 2022|[Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/C0VKIR)  <br /> | [Output AWS Link](https://cmas-cmaq.s3.amazonaws.com/index.html#CMAQv6.0_2022_12SE1_Benchmark_2day/benchmark_outputs/output_CCTM_v6_gcc_Bench_2022_12SE1_cracmm3/)|[CRACMM3 Tutorial](Users_Guide/Tutorials/CMAQ_UG_tutorial_benchmark_cracmm3_stage.md)|
|v6 cb6r5_ae7_aq, STAGE| Output (1.9 GB) | 12SE1 | July 1-2, 2022|[Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/C0VKIR)  <br /> | [Output AWS Link](https://cmas-cmaq.s3.amazonaws.com/index.html#CMAQv6.0_2022_12SE1_Benchmark_2day/benchmark_outputs/output_CCTM_v6_gcc_Bench_2022_12SE1_cb6r5_ae7_aq/) |[cb6r5 Tutorial](Users_Guide/Tutorials/CMAQ_UG_tutorial_benchmark.md)|
|v6-ISAM CRACMM3 STAGE | Output (3.9 GB) | 12SE1 |  July 1 - 2, 2022  | [Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/C0VKIR) <br> | [Output AWS Download Link](https://cmas-cmaq.s3.amazonaws.com/index.html#CMAQv6.0_2022_12SE1_Benchmark_2day/benchmark_outputs/output_CCTM_v6_ISAM_gcc_Bench_2022_12SE1/) |[ISAM Tutorial](Users_Guide/Tutorials/CMAQ_UG_tutorial_ISAM.md)|
|v6-DDM3D CRACMM3 STAGE | Output (2 GB) | 12SE1 |  July 1 - 2, 2022  | [Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/C0VKIR) <br> | [Output AWS Download Link](https://cmas-cmaq.s3.amazonaws.com/index.html#CMAQv6.0_2022_12SE1_Benchmark_2day/benchmark_outputs/output_CCTM_v6_DDM3D_intel23.2_Bench_2022_12SE1/) |[DDM Tutorial](Users_Guide/Tutorials/CMAQ_UG_tutorial_DDM3D.md)|
|v6 CRACMM3, STAGE| Input (2.7 TiB) | 12US1 | Dec 22, 2021 - Dec 31, 2022|[Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/KZYFDB)  <br /> | [Input AWS Link](https://cmaq-v6-2022-data.s3.amazonaws.com/index.html#CMAQv6/2022_12US1/inputs/) |[Run Scripts, AWS Link](https://cmaq-v6-2022-data.s3.amazonaws.com/index.html#CMAQv6/2022_12US1/CCTM/scripts)|
|v6 CRACMM3, STAGE| Output (301.8 GiB) | 12US1 | Dec 22, 2021 |[Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/KZYFDB)  <br /> | [Output AWS Link](https://cmaq-v6-2022-data.s3.amazonaws.com/index.html#CMAQv6/2022_12US1/outputs/benchmark_outputs) |[2 month POST combine output AWS Link](https://cmaq-v6-2022-data.s3.amazonaws.com/index.html#CMAQv6/2022_12US1/outputs/POST)|



<a id=v55_test_case_data></a>
## CMAQv5.5 Test Case Data
Benchmark/tutorial data for the CMAQv5.5 release are available from the CMAS Data Warehouse.  The input and output files are stored on Amazon Web Services (AWS) Open Data Registry.  CMAQv5.5 benchmark input is the same as CMAQv5.4, providing a July 1-2, 2018 case over the Northeast US.  CMAQv5.5 comes with new output data for running several different model configurations (links below).  Tutorials are provided for using the benchmark data to test running of the base CMAQ model with either the CB6r5 or CRACMMv2 mechanisms, WRF-CMAQ, CMAQ-ISAM, and CMAQ-DDM. The input datasets include a grid mask file for the United States (GRIDMASK_STATES_12SE1.nc). The grid mask file is used for running the ISAM test case, or to test out regional emissions scaling with [DESID](Users_Guide/Tutorials/CMAQ_UG_tutorial_emissions.md).  The input datasets also include an ocean file with variables needed to use the cb6r5_ae7 and cb6r5m_ae7 mechanisms. See the [Ocean File tutorial](Users_Guide/Tutorials/CMAQ_UG_tutorial_oceanfile.md) for more information on changes to the required ocean file input beginning in v5.4.  

In addition, a full set of inputs for 2018 are provided for the 12US1 domain (299 column x  459 row x 35 layer, 12-km horizontal grid spacing) on AWS, including emissions compatible with both the CB6r5 and CRACMMv1.0 chemical mechanisms.  Note that many of the 12US1 inputs are  netCDF-4/HDF5 compressed files to substantially reduce file sizes. Through testing at the EPA, we’ve noticed that certain domains encounter model crashes from reading in large amounts of compressed netCDF data.  A work around for those cases is uncompressing the data manually via [nccopy 1](https://www.unidata.ucar.edu/software/netcdf/workshops/2011/utilities/Nccopy.html) or [m3cple](https://www.cmascenter.org/ioapi/documentation/all_versions/html/M3CPLE.html) (compiled with HDF5) before running the CMAQ simulation.

|**CMAQ Version**|**Data Type (Size)**|**Domain**|**Simulation Dates**|**Data Access**|**Link**|**Tutorial**| 
|:----:|:----:|:--------------:|:----:|:--------:|:----:|:----:|
|MPAS-CMAQ| Input (215 GB) | Global (uniform 120) | Jan 1, 2017|[Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/PAHQFO)  <br /> | [AWS Link](https://mpas-cmaq.s3.amazonaws.com/index.html) |[Tutorial](https://github.com/USEPA/CMAQ/blob/MPAS_CMAQ/DOCS/Users_Guide/PDF/MPAS_CMAQ_guide.pdf)|
|v5.4 CB6r5 | Input (6.1 TB) | 12US1 | Jan 1 - Dec 31, 2018 | [Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/LDTWKH)  <br /> | [AWS Link](https://cmas-cmaq-modeling-platform-2018.s3.amazonaws.com/index.html) ||
|v5.4 CB6r5 | Input (10.3 GB)| Northeast US| July 1 - 2, 2018| [Metadata, DOI, and download instructions ](https://doi.org/10.15139/S3/BWMI8X) <br /> | [Google Drive Link](https://drive.google.com/drive/folders/1AFUB-4kzIXXoZr4hOHNBqRvy9JQ9_MDp)  <br /> [AWS Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_4/CMAQv5.4_2018_12NE3_Benchmark_2Day_Input.tar.gz)||
|v5.5 CRACMM2| Input (6 GB) | 12NE3 |  July 1 - 2, 2018  | [Metadata, DOI, and links to data on AWS]( https://doi.org/10.15139/S3/X5SZM2) <br> | [AWS Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/CMAQv5.5_2018_12NE3_Benchmark_cracmm2_stage_2Day_Input.tar.gz) ||
|v5.5 CRACMM2| Output (19 GB) | 12NE3 |  July 1 - 2, 2018  | [Metadata, DOI, and links to data on AWS]( https://doi.org/10.15139/S3/X5SZM2) <br> | [AWS Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/output_CCTM_v55_gcc_Bench_2018_12NE3_cracmm2_stage.tar.gz)|[Tutorial](Users_Guide/Tutorials/CMAQ_UG_tutorial_benchmark_cracmm2_stage.md)|
|v5.5 CB6r5 M3Dry | Output (15 GB) | 12NE3 | July 1 - 2, 2018 |  [Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/X5SZM2) <br> |  [AWS Download Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/output_CCTM_v55_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.tar.gz) |[Tutorial](Users_Guide/Tutorials/CMAQ_UG_tutorial_benchmark.md)|
|v5.5 CB6r5 STAGE | Output (16 GB) | 12NE3 | July 1 - 2, 2018 |  [Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/X5SZM2) <br> | [AWS Download Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/output_CCTM_v55_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_stage.tar.gz) |[Modify the M3DRY Tutorial](Users_Guide/Tutorials/CMAQ_UG_tutorial_benchmark.md)|
|v5.5-ISAM CB6r5 M3Dry | Output (52 GB) | 12NE3 |  July 1 - 2, 2018  | [Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/X5SZM2) <br> | [AWS Download Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/output_CCTM_v55_ISAM_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.tar.gz) |[Tutorial](Users_Guide/Tutorials/CMAQ_UG_tutorial_ISAM.md)|
|v5.5-DDM3D CB6r5 M3Dry | Output (16 GB) | 12NE3 |  July 1 - 2, 2018  | [Metadata, DOI, and links to data on AWS](https://doi.org/10.15139/S3/X5SZM2) <br> | [AWS Download Link](https://cmaq-release-benchmark-data-for-easy-download.s3.amazonaws.com/v5_5/output_CCTM_v55_DDM3D_gcc_Bench_2018_12NE3_cb6r5_ae7_aq_m3dry.tar.gz) |[Tutorial](Users_Guide/Tutorials/CMAQ_UG_tutorial_DDM3D.md)|
|v5.5 CRACMM2 | Input | 12US1 | Dec 2017- Dec 2018| [Metadata, DOI, and links to data on AWS]( https://doi.org/10.15139/S3/X5SZM2) <br> | [AWS Link to Scripts](https://cmas-cmaq-modeling-platform-2018.s3.amazonaws.com/index.html#2018_12US1/CMAQ_v55_cracmm2_scripts/)|
|v5.5 CRACMM2 | Output ( 1.9 TB) | 12US1 | Dec. 22, 2017 - Jan 2 2018 | [Metadata, DOI, and links to data on AWS]( https://doi.org/10.15139/S3/X5SZM2) <br> | [AWS Link to Output](https://cmas-cmaq-modeling-platform-2018.s3.amazonaws.com/index.html#2018_12US1/output/output_CCTM_v55_intel_STAGE_EM_2018_12US1_two_week_16x8/)|

<a id=cmaq_inputs_outputs></a>
## CMAQ Input and Output Data
The CMAS Center Data Warehouse cataloges publicly-available CMAQ-ready input files for a range of years and domains
* [Catalogue of available  meteorology, emissions and air quality model input and output](https://dataverse.unc.edu/dataverse/cmascenter)
* [CMAS Registry of Open Data on AWS](https://registry.opendata.aws/cmas-data-warehouse/)

<a id=equates_data></a>
## EQUATES Data
Meteorology, emissions, and CMAQ air pollutant and deposition estimates from the [EPA's Air Quality Time Series (EQUATES) Project](https://www.epa.gov/cmaq/equates) are available for 2002-2019
  * [EQUATES Metadata and DOI](https://doi.org/10.15139/S3/F2KJSK)
  * [EQUATES Data Use Policy](https://drive.google.com/file/d/1F1Ed9MMQQ6fuiiXxlGEbYHXdhvjP-Xkx/view?usp=sharing)
  * [EQUATES Data Dictionary](https://drive.google.com/file/d/1TVTsrH94zDyOyMJEHNEi9_obwG2ZcPaa/view?usp=sharing)

<a id=cmaq_fused></a>
## Measurement-Model "Fused" CMAQ Outputs
CMAQ output is often combined, or "fused", with observed air quality measurements to remove any consistent model biases prior to using the model predictions for a particular application. 
* ["Fused" CMAQ Ozone, PM<sub>2.5</sub>, and deposition](https://www.epa.gov/cmaq/data-download-step-2#model_obs_fused_CMAQ_outputs)
