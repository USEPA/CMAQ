<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch01_intro.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch03_features.md)

<!-- END COMMENT -->

# Overview of CMAQ System Components #


## Installation overview


### Installing from GitHub

There are two options for obtaining CMAQ source code from GitHub:
1. Download a zipped code archive from the GitHub web client.
2. Clone the repository directly to a Linux server using the command line. For example, to download CMAQ version 5.1, issue the following command (this assumes that git is installed on your system):

  <pre><code>git clone -b 5.2.1 https://github.com/USEPA/CMAQ.git CMAQ_REPO</code></pre>

### Installing from git-based tarballs (CMAQ version 5.0.2 and later)

Users need not install git to install CMAQ. Zip archives of the source code and scripts are available from GitHub. Click the "Clone or download" button on the [U.S. EPA GitHub repository](https://github.com/USEPA/CMAQ) and select "Download ZIP" to get a zip file of the full CMAQ repository, including scripts and source code. Unzip this file on the Linux file system directory where you would like to install CMAQ.

## Configuration options


## Chemistry-transport model conceptual formulation


### Inline processes
** >>COMMENT<< ** Not sure this belongs here and maybe should be part of CCTM section

** >>COMMENT<< ** we do test against the bidi option to know which emissions to use

** >>COMMENT<< ** "Eulerian" needs to be defined.

** >>COMMENT<< ** On double-counting emissions, can we do something to prevent that from happening?  Not sure how to instruct users on how to avoid double-counting emissions.

In the context of air quality modeling, inline processes refer to those that are run at the same time as the chemistry-transport model. The major advantage of including processes inline to the CCTM is that the simulation of one parameter can feedback to the simulation of one or more other parameters. For example, by including the photolysis rate calculations inline in the CCTM, the rates will be influenced by the radiative impacts of the simulated aerosol loading. Other inline features to the CCTM, such as emissions processing, provide efficiency advantages to the modeling operation and facilitate coupled meteorology-chemistry modeling. CMAQ versions 5.0 and forward support emissions calculations for biogenic sources, windblown dust, seasalt, lightning NOx, and point source plume rise in the CCTM.

Users must be careful to avoid double counting emissions when designing an inline CCTM simulation. The CCTM does not have the ability to identify which emissions sources were calculated offline and are being input to the model. For example, including biogenic emissions in the input emissions files and configuring the CCTM to calculate the biogenic emissions inline is redundant and will produce erroneous double counting of the emissions source.

Photolysis rates are calculated inline in the CCTM.  **The preprocessing program JPROC is no longer a required part of the CMAQ modeling sequence.**  

## Summary descriptions of the major CMAQ programs
** >>COMMENT<< ** This section and onwards: Not sure why we need to remind the reader in each subsection that these codes are further described in Chapter 7.  Seems like this could be done once near the top of the section.


The major CMAQ components and ancillary programs are described below. More detailed discussions on the formulations of the CMAQ programs are available in [Chapter 4](CMAQ_OGD_ch04_science.md), in Byun and Ching (1999), and in Byun and Schere (2006).

Note that the following list of programs is generally the order in which the CMAQ programs are run.

<a id="bldmake"></a>

### Model Builder (Bldmake)


<a id="icon"></a>

### Initial Conditions Processor (ICON)


<a id="bcon"></a>

### Boundary Conditions Processor (BCON)


<a id="mcip"></a>

### Meteorology-Chemistry Interface Processor (MCIP)

<a id="cctm"></a>

### CMAQ Chemistry-Transport Model (CCTM)


<a id="chemmech"></a>

### Chemical Mechanism Compiler (CHEMMECH)


<a id="create_ebi"></a>

### EBI chemistry solver builder (CREATE_EBI)

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch01_intro.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch03_features.md)<br>
CMAQ Operational Guidance Document (c) 2016<br>

<!-- END COMMENT -->
