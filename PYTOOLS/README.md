Python Tools for CMAQ
=====================

This folder holds python tools for CMAQ. These tools may fill roles that are
usually stored in PREP or POST folders. They are stored here to centralize the
installation requirements.

These Python Tools require Python v3.6 or higher. Some are written as Jupyter
Notebooks, while others as scripts. All will require some libraries.

The best way to check your installation is to run:

`python install/show_versions.py install/requirements.txt`

For information about how to install, see the install folder.

Tool Listing
============

* [install](install/README.md) Describes preparing an environment for CMAQ/PYTOOLS.
* [dmschlo](dmschlo/README.md) Prepares DMS and Chlorophyll-A concentrations for CMAQ.
* [shp2cmaq](../PYTOOLS/shp2cmaq/README.md) Creates a CMAQ-ready grid mask from a shape file that can be used for defining regions and region families with DESID and using geographic source regions when running CMAQ-ISAM. 
