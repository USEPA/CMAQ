CMAQv5.4 with SAPRC-22 Chemistry Mechanism Option
==========

US EPA Community Multiscale Air Quality Model (CMAQ) Website: https://www.epa.gov/cmaq


CMAQ is an active open-source development project of the U.S. EPA's Office of Research and Development that consists of a suite of programs for conducting air quality model simulations.
CMAQ is supported by the CMAS Center: http://www.cmascenter.org

CMAQ combines current knowledge in atmospheric science and air quality modeling with multi-processor
computing techniques in an open-source framework to deliver fast, technically sound estimates of ozone,
particulates, toxics, and acid deposition.

## SAPRC-22 Branch
The latest SAPRC mechanism, referred to as SAPRC-22, has been implemented in CMAQ version 5.4.

A report describing the implementation of the mechanism and its testing is provided under the DOCS folder: [SAPRC-22 Implementation Report](DOCS/SAPRC22_DraftReport_Deliverable_3.4.25.pdf). Users wishing to use CMAQ with the SAPRC-22 mechanism are recommended to read the report. In particular, Section 4 of the report provides guidance on speciating total organic gas emissions. The Appendix section provides a listing of the mechanism and associated species.

### Getting the CMAQv5.4_SAPRC22 Repository
To clone the 5.4_saprc22 branch code from the CMAQ Git archive, specify the branch (i.e. version number) and issue the following command from within a working directory on your server:

`git clone -b 5.4_saprc22 https://github.com/USEPA/CMAQ.git CMAQ_REPO_5.4_saprc22`

### CMAQv5.4_SAPRC22 Modifications to the build script: 

To build CMAQ with the SAPRC-22 mechanism, the following changes were made to the default bldit_cctm.csh script.

1. Specify mechanism<br>
```setenv Mechanism saprc22_ae65_aq```

2. Include saprc-22 in mechanism list  
```set MechList = "cb6r5hap_ae7_aq, cb6r3_ae7_aq, cb6r5_ae7_aq, cb6r5_ae7_aqkmt2, cb6r5m_ae7_aq, racm2_ae6_aq, saprc07tc_ae6_aq, saprc07tic_ae7i_aq, saprc07tic_ae7i_aqkmt2, saprc22_ae65_aq" ```

### CMAQv5.4_SAPRC22 Test Data 

CMAQ input data for the contiguous US (CONUS) for July 1-7, 2016 are available on the CMAS AWS Open Data Platform to test buidling and running CMAQv5.4_SAPRC22. To browse and download the data on AWS: <a href="https://cmaqv54-saprc22-12us2-2016-07-test-data.s3.amazonaws.com/index.html">CMAQv5.4_SAPRC22 Test Data -- 07/01/2016 - 07/14/2016 12km CONUS  (12US2 domain)</a><br>

To download all of the data (260.1 GB) required to run the test case for the CMAQv5.4 SAPRC-22 test case, use the following command, editing the your_local_directory to specify a path on your local machine:  
<br>
```aws s3 cp --recursive s3://cmaqv54-saprc22-12us2-2016-07-test-data/cmaqv54-saprc22-12us2-2016-07-test-data/2016_12US2 /your_local_directory/cmaqv54-saprc22-12us2-2016-07-test-data/2016_12US2```

Additional information on downloading the data is available in the top level [Readme](https://cmaqv54-saprc22-12us2-2016-07-test-data.s3.amazonaws.com/readme.html) page for the AWS Simple Storage Service (S3) bucket.  Metadata, including the data DOI, are available on the CMAS DataVerse: <a href="https://dataverse.unc.edu/dataset.xhtml?persistentId=doi:10.15139/S3/JWQWMC">CMAQv5.4_SAPRC22 Test Data -- 07/01/2016 - 07/14/2016 12km CONUS  (12US2 domain)</a><br>



### CMAQv5.4_SAPRC22 Sample Build and Run Scripts

Sample build and run scripts for the 2016 test case are available in this repository under *CCTM/srcipts* ([bldit_cctm_saprc22.csh](CCTM/scripts/bldit_cctm_saprc22.csh) and [run_cctm_5.4_saprc22_2016_12US2.csh](CCTM/scripts/run_cctm_5.4_saprc22_2016_12US2.csh)).  The scripts are also stored on AWS with the input data under *2016_12US2/scripts*. If you would like to obtain the bldit_cctm_saprc22.csh and the run_cctm_2016_saprc22_ebi_May2025_updates.csh scripts without downloading the full bucket, use the wget commands:  

```wget https://cmaqv54-saprc22-12us2-2016-07-test-data.s3.amazonaws.com/2016_12US2/scripts/bldit_cctm_saprc22.csh```   
```wget https://cmaqv54-saprc22-12us2-2016-07-test-data.s3.amazonaws.com/2016_12US2/scripts/run_cctm_2016_saprc22_ebi_May2025_updates.csh```



### User Support
Questions regarding this mechanism can be directed to EPA (baublitz.colleen@epa.gov).

## CMAQ version 5.4 Overview

See the [CMAQv5.4 branch](https://github.com/USEPA/CMAQ/tree/5.4) for additional information on version 5.4, including the Release FAQ and Release Notes.


## EPA Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

* [Open source license](license.md)
