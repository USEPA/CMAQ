<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch08_input_and_output_files.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch10_new_simulation.md)

<!-- END COMMENT -->

** >> Comment <<** Overall I think most of this information belongs in other sections rather than in a standalone section. Perhaps we need a new section where some of the setup information should be captured, e.g., "Getting Started with CMAQ" or "Before You Run CMAQ".

# Defining Grids, Layers, and Chemistry #

** >> Comment <<** Although MCIP is not explicitly listed here, need to make it more clear that some parts pertain to MCIP (such as the grid structure) and others do not (such as the layers and the chemistry).

This chapter describes how to define new horizontal grids, vertical layers, and chemical mechanisms in CMAQ. These specifications apply to multiple programs in the CMAQ modeling system, including ICON, BCON, JPROC, and CCTM. When configuring new simulations, users must define the location, extent, and structure of the horizontal and vertical grids, and the chemical mechanism for representing pollutant chemical transformations. CMAQ contains several default options for these parameters that can be used as templates for setting up new configurations. Before deciding to create definitions for new grids and mechanisms, check to see whether the existing options are sufficient for your model simulation. If a predefined choice is not appro­priate, then follow the steps described in this section to create a new definition.

Once you have configured a simulation that is suitable for your purposes in terms of the horizontal grid, vertical layers, and chemical mechanism, proceed to [Chapter 10](CMAQ_OGD_ch10_new_simulation.md) to learn how to develop new model executables for running a CMAQ simulation.

Grids and coordinate systems
----------------------------




CMAQ Chemical Mechanisms
------------------

### Using predefined chemical mechanisms

### Creating or modifying chemical mechanisms


### Using species namelist files

### Further information on chemical mechanisms

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch08_input_and_output_files.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch10_new_simulation.md)<br>
CMAQ Operational Guidance Document (c) 2016<br>

<!-- END COMMENT -->
