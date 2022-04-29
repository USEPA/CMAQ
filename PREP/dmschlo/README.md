Ocean Data for CMAQ
===================

---
    Author: Barron H. Henderson, Golam Sarwar, Brett Gantt, Jeff Williston
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
chlorophyll-a (CHLO) concentrations (mg/m**3).

This processor requires a CMAQ OCEAN file as an input. The OCEAN file should
include a open ocean variable (OPEN) and a surf zone variable (SURF), where both
variables contain the fraction of the grid cell covered. OPEN and SURF should
already account for SEAICE grid cell coverage. This affects seasalt emissions,
and is not relevant for DMS or halogens except indirectly via seasalt.

OPEN and SURF can be created by the standard CMAQ Spatial Allocator Tool.

DMS and CHLO have previously been provided by scripts maintained and run by
Brett Gantt. This tool uses similar inputs and processes, but has updated
interpolation routines that improve the overall product.

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
2. Cloud-based DMS/Chlor-A processing needs access to Ocean outputs
  * Status: Resolved
  * Summary: When running in the cloud, ocean files are notyour results from each Notebook may not be available from the previous step unless you upload them.
  * Resolution: I have updated each notebook to create a zip file of outputs and updated the DMS_ChlorA notebook to look for an uploaded zip files (downloadocean.zip and downloadseaice.zip) as part of the process. This does require that users download results at each step and upload them to the DMS/Chlor-A instance.
3. DMS results look blocky
  * Status: Open
  * Summary: Old DMS used bilinear interpolation. We are using area overlap fraction, which leads to some blockiness.
  * Resolution: N/A
