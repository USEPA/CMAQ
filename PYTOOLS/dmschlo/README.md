Ocean Data for CMAQ
===================

---
    Author: Barron H. Henderson, Golam Sarwar, Brett Gantt, Jeff Willison
    Date: 2021-08-13
    Updated: 2022-04-29
---


Table of Contents
-----------------

* Overview
* Instructions
* Known Issues

Overview
--------

This tool is designed to augment standard CMAQ "Ocean" files to enable dimethyl
sulfide (DMS) and halogen chemistry. DMS emissions are calculated based on DMS
ocean-water concentrations (nM). Halogen emissons are calculated based on
chlorophyll-a (CHLO) concentrations (`mg/m**3`).

* DMS concentrations are required in the OCEAN file for cb6r5,
* DMS concentrations and CHLO concentrations are required for cb6r5m, and
* DMS and CHLO are available at monthly resolution to make 12 OCEAN files.

This processor requires a CMAQ OCEAN file as an input. OCEAN is a time-
independent I/O API file that identifies the fractional [0-1] coverage in each
model grid cell allocated to open ocean (OPEN) or surf zone (SURF). OPEN and
SURF should already account for SEAICE grid cell coverage. This affects seasalt
emissions, and is not relevant for DMS or halogens except indirectly via
seasalt.

For details on how to make a OCEAN file, see the [OCEAN File Tutorial](https://github.com/USEPA/CMAQ_Dev/blob/operational/DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_oceanfile.md)

DMS and CHLO have previously been provided by scripts maintained and run by
Brett Gantt but was never released to the public. This tool uses similar inputs
and processes, but has updated interpolation routines that improve the overall
product.

We have developed a new Jupyter Notebook that allows users to easily create
DMS and CHLO variables. These Notebooks can be run on your own server or in the
cloud (e.g., on Google Colab). Known issues and their status are described after
the instructions.


Instructions
------------

Creating a fully function ocean file with OPEN, SURF, CHLO, and DMS requires 4
separate steps describe below. You will need an existing OCEAN file with OPEN
and SURF. For some domains, the OCEAN file varies by month. In that case, you
will need all the OCEAN files.

1. Open CMAQ_DMS_ChlorA.ipynb in a Jupyter Environment
    * Google Colab, Binder, etc
    * Local machine
2. Edit User Inputs
3. Click "Run" then "Run All"
4. Open CMAQ_DMS_ChlorA_Plot.ipynb
    * Click "Run" then "Run All"
    * Review plots

Known Issues
------------

1. Problems with Polar Resolution Remapping
  * Status: Resolved
  * Summary: When running for a polar stereographic domain, you require CDO v1.9.6+. Earlier versions did not support second-order flux conserving remapping.
  * Resolution: I have changed the prerequisite installation process to use conda. This allows a newer version of cdo.
2. DMS results look blocky
  * Status: Open
  * Summary: Old DMS used bilinear interpolation. We are using area overlap fraction, which leads to some blockiness.
  * Resolution: N/A
3. Visualization Notebook cannot open results on Google Colab (or other web service)
  * Status: Known
  * Summary: When the CMAQ_DMS_ChlorA.ipynb is complete, the results should be downloaded from the web service. Starting the visualization notebook may create a new virtual machine. That machine will not have the output unless you upload it.
  * Resolution: Download outputs and upload them before using visualization tool.