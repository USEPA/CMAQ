CMAQ
====

Community Multiscale Air Quality Model [US EPA Website](https://www.epa.gov/air-research/community-multi-scale-air-quality-cmaq-modeling-system-air-quality-management)

CMAQ is an active open-source development project of the U.S. EPA Computational Exposure Division
that consists of a suite of programs for conducting air quality model simulations.
CMAQ is supported by the CMAS Center (http://www.cmascenter.org).

CMAQ combines current knowledge in atmospheric science and air quality modeling with multi-processor
computing techniques in an open-source framework to deliver fast, technically sound estimates of ozone,
particulates, toxics, and acid deposition.

**Take our 2018 CMAQ User Survey!**
* [Link to online survey](https://unc.az1.qualtrics.com/jfe/form/SV_1IcuS1wIZ2PQUu1)
* Deadline to complete the survey: October 31, 2018.

### Repository Framework
The CMAQ Git repository is organized with each official public release stored as a branch on the public GitHub USEPA CMAQ repository.
To clone code from the CMAQ Git repository, specify the branch (i.e. version number) and issue the following command from within a working directory on your server. For example, to get the 5.2 version to a local repository that you would name CMAQ_REPO:
```
git clone -b 5.2 https://github.com/USEPA/CMAQ.git CMAQ_REPO
```
The followoing release versions of CMAQ are currently available on GitHub.  DOI values from Zenodo can be used when referencing a specific version.
* [v5.2.1 (March 2018)](https://github.com/USEPA/CMAQ/tree/5.2.1) - [doi:10.5281/zenodo.1212601](https://zenodo.org/record/1212601) | [Users Guide](https://github.com/USEPA/CMAQ/blob/5.2.1/DOCS/User_Manual/README.md) | [Known Issues](https://github.com/USEPA/CMAQ/blob/5.2.1/DOCS/Known_Issues/README.md) | [Release Notes](https://github.com/USEPA/CMAQ/blob/5.2.1/CCTM/docs/Release_Notes/README.md) | [Tutorials](https://github.com/USEPA/CMAQ/tree/5.2.1/DOCS/Tutorials)  

* [v5.2 (June 2017)](https://github.com/USEPA/CMAQ/tree/5.2) - [doi:10.5281/zenodo.1167892](https://zenodo.org/record/1167892) | [Users Guide](https://github.com/USEPA/CMAQ/blob/5.2/DOCS/User_Manual/README.md) | [Known Issues](https://github.com/USEPA/CMAQ/blob/5.2/DOCS/Known_Issues/README.md) | [Release Notes](https://github.com/USEPA/CMAQ/blob/5.2/CCTM/docs/Release_Notes/README.md) | [Tutorials](https://github.com/USEPA/CMAQ/tree/5.2/DOCS/Tutorials)
* [v5.1   (December 2015)](https://github.com/USEPA/CMAQ/tree/5.1) - [doi:10.5281/zenodo.1079909](https://zenodo.org/record/1079909)
* [v5.0.2 (April 2014)](https://github.com/USEPA/CMAQ/tree/5.0.2) - [doi:10.5281/zenodo.1079898](https://zenodo.org/record/1079898)
* [v5.0.1 (July 2012)](https://github.com/USEPA/CMAQ/tree/5.0.1)
* [v5.0   (February 2012)](https://github.com/USEPA/CMAQ/tree/5.0) - [doi:10.5281/zenodo.1079888](https://zenodo.org/record/1079888)
* [v4.7.1 (June 2010)](https://github.com/USEPA/CMAQ/tree/4.7.1) - [doi:10.5281/zenodo.1079879](https://zenodo.org/record/1079879)

### Benchmark Data
Benchmark/tutorial input and output data for each CMAQ release version are available from the CMAS Center Software Clearinghouse. Go to http://www.cmascenter.org, select Download -> Software -> CMAQ and choose the version to get the tutorial data.  

### EPA Disclaimer  
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.    [<img src="https://licensebuttons.net/p/mark/1.0/88x31.png" width="50" height="15">](https://creativecommons.org/publicdomain/zero/1.0/)
