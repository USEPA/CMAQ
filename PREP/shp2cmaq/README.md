Create CMAQ-Ready File from Shapefile
=====================================

---
    author: Barron H. Henderson
    date: 2020-04-25
    updated: 2020-04-29
---

This Notebook uses geopandas and PseudoNetCDF to create IOAPI-like files for
CMAQ. Geopandas supports STRtree optimized searches and projection conversions.
The overall process requires a shapefile with attributes and a GRIDDESC file. It
produces variables that specify grid cell fractional coverage by each unique
value of a specified attribute. It also produces a dominant attribute variable
that specifies which attribute has the largest area coverage.

For example, the Natural Earth countries file (administrative-level 0) has
Polygons with the country identifier attribute ADM0_A3. The notebook will create
a variable for each unique value of ADM0_A3 (e.g, USA, MEX, CAN). It will also
create a variable (DOM_ADM0_A3) that uses numeric codes (alphabetic order) to
identify which ADM0_A3 has the largest coverage of that cell.
