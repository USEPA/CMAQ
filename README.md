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

To build CMAQ with the SAPRC-22 mechanism, the following changes were made to the default bldit_cctm.csh script. The bldit_cctm_saprc22.csh script is provided in both the CMAQ_REPO_5.4_saprc22 and the s3 bucket under ./CCTM/scripts/bldit_cctm_saprc22.csh:<br>

<pre>
1. Specify mechanism<br>
setenv Mechanism saprc22_ae65_aq              #> chemical mechanism (see $CMAQ_MODEL/CCTM/src/MECHS)

2. Include saprc-22 in mechanism list
set MechList = "cb6r5hap_ae7_aq, cb6r3_ae7_aq, cb6r5_ae7_aq, cb6r5_ae7_aqkmt2, cb6r5m_ae7_aq, racm2_ae6_aq, saprc07tc_ae6_aq, saprc07tic_a
e7i_aq, saprc07tic_ae7i_aqkmt2, saprc22_ae65_aq"
</pre>

### CMAQv5.4_SAPRC22 Test Data and Sample Build and Run Scripts

The run script has been modified to use the data from the s3 bucket titled: <a href="https://cmaqv54-saprc22-12us2-2016-07-test-data.s3.amazonaws.com/index.html">CMAQv5.4_SAPRC22 Test Data -- 07/01/2016 - 07/14/2016 12km CONUS  (12US2 domain)</a> <br>
<br>
If you would like to obtain the bldit_cctm_saprc22.csh and the run_cctm_2016_saprc22_ebi_May2025_updates.csh scripts without downloading the full bucket, use the wget commands:
<br>
<pre>
wget https://cmaqv54-saprc22-12us2-2016-07-test-data.s3.amazonaws.com/2016_12US2/scripts/bldit_cctm_saprc22.csh
wget https://cmaqv54-saprc22-12us2-2016-07-test-data.s3.amazonaws.com/2016_12US2/scripts/run_cctm_2016_saprc22_ebi_May2025_updates.csh
</pre>
<br>
To download all of the data required to run the test case for the CMAQv5.4 SAPRC-22 benchmark case, use the following command, editing the your_local_directory to specify a path on your local machine:<br>
<br>
<pre>
aws s3 cp --recursive s3://cmaqv54-saprc22-12us2-2016-07-test-data/cmaqv54-saprc22-12us2-2016-07-test-data/2016_12US2 /your_local_directory/cmaqv54-saprc22-12us2-2016-07-test-data/2016_12US2
</pre>

To obtain the aws command line if you do not already have it, please follow these instructions<br>
<br>
<a href="https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html">Instructions to Download AWS Command Line</a><br>
<br>
The total size of the data is 260.1 GiB<br>
<br>

Metadata to understand and reference this data: <a href="https://dataverse.unc.edu/dataset.xhtml?persistentId=doi:10.15139/S3/JWQWMC">CMAQv5.4_SAPRC22 Test Data -- 07/01/2016 - 07/14/2016 12km CONUS  (12US2 domain)</a><br>

Browse the s3 bucket: <a href="https://cmaqv54-saprc22-12us2-2016-07-test-data.s3.amazonaws.com/index.html">CMAQv5.4_SAPRC22 Test Data -- 07/01/2016 - 07/14/2016 12km CONUS  (12US2 domain)</a>.<br>

Readme.html with information about the data in the s3 bucket: <a href="https://cmaqv54-saprc22-12us2-2016-07-test-data.s3.amazonaws.com/readme.html">https://cmaqv54-saprc22-12us2-2016-07-test-data.s3.amazonaws.com/readme.html</a>


### User Support
Questions regarding this mechanism can be directed to EPA (baublitz.colleen@epa.gov).

## CMAQ version 5.4 Overview

See the [CMAQv5.4 branch](https://github.com/USEPA/CMAQ/tree/5.4) for additional information on version 5.4, including the Release FAQ and Release Notes.


## EPA Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

* [Open source license](license.md)
