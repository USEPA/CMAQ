Create CMAQ-Ready File from Shapefile
=====================================

---
    author: Barron H. Henderson
    date: 2020-04-25
    updated: 2020-04-29
---

This Notebook uses geopandas and cmaqsatproc to create IOAPI-like files for
CMAQ. The overall process requires a shapefile with attributes and a GRIDDESC
file. It produces variables that specify grid cell fractional coverage by each
unique value of a specified attribute. It also produces total and dominant
attribute variables. The total specifies the fraction coverage of any
attribute. The dominant variable specifies which attribute has the largest
area coverage.

Examples
--------

## Census State Boundaries

The US Census state boundary shapefile has Polygons for each state with the
STUSPS attribute. With attrkey set to STUSPS, the notebook will create a
variable for each unique value of STUSPS (e.g, AL, AK, AR, ... WV, WY) with
the fractional area covered by that state. It will also create a
variable STUSPS_DOM that uses numeric codes (in alphabetic order) to
identify which STUSPS has the largest coverage of that cell. And, it will
include a variable STUSPS_TOT with the total coverage of any STUSPS.


## Natural Earth

The Natural Earth countries shapefile (administrative-level 0) has Polygons
with the country identifier attribute ADM0_A3. The notebook will create a
variable for each unique value of ADM0_A3 (e.g, USA, MEX, CAN). It will also
create a variable (ADM0_A3_DOM) that uses numeric codes (in alphabetic order)
to identify which ADM0_A3 has the largest coverage of that cell. And, it will
include a variable ADM0_A3_TOT with the total coverage of any ADM0_A3.
