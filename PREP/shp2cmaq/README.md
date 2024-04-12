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

## Census State Boundaries for 12US1 Domain

The US Census state boundary shapefile has Polygons for each state with the
STUSPS attribute. With attrkey set to STUSPS, the notebook will create a
variable for each unique value of STUSPS (e.g, AL, AK, AR, ... WV, WY) with
the fractional area covered by that state. It will also create a
variable STUSPS_DOM that uses numeric codes (in alphabetic order) to
identify which STUSPS has the largest coverage of that cell. And, it will
include a variable STUSPS_TOT with the total coverage of any STUSPS.

### Steps

1. Download https://www2.census.gov/geo/tiger/GENZ2022/shp/cb_2022_us_state_500k.zip
2. Run tool:

From SHELL:
```bash
python shp2cmaq.py cb_2022_us_state_500k.zip STUSPS 12US1
```

In python:
```python
import shp2cmaq
shp2cmaq('cb_2022_us_state_500k.zip', 'STUSPS', '12US1')
```

## Natural Earth Countries for Hemispheric Domain

The Natural Earth countries shapefile (administrative-level 0) has Polygons
with the country identifier attribute ADM0_A3. The notebook will create a
variable for each unique value of ADM0_A3 (e.g, USA, MEX, CAN). It will also
create a variable (ADM0_A3_DOM) that uses numeric codes (in alphabetic order)
to identify which ADM0_A3 has the largest coverage of that cell. And, it will
include a variable ADM0_A3_TOT with the total coverage of any ADM0_A3.

1. Download http://naturalearth.s3.amazonaws.com/110m_cultural/ne_110m_admin_0_countries.zip
2. Run tool:

From SHELL:
```bash
python shp2cmaq.py ne_110m_admin_0_countries.zip ADM0_A3 108NHEMI2
```

In python:
```python
import shp2cmaq
shp2cmaq('ne_110m_admin_0_countries.zip', 'ADM0_A3', '108NHEMI2')
```

## Natural Earth State/Province Level for 36US3

The Natural Earth countries shapefile (administrative-level 1) has Polygons
with the state/provice identifier attribute iso_3166_2. The notebook will
create a variable for each unique value of iso_3166_2. It will also
create a variable (iso_3166_2) that uses numeric codes (in alphabetic order)
to identify which iso_3166_2_DOM has the largest coverage of that cell. And,
it will include a variable iso_3166_2_TOT with the total coverage of any
iso_3166_2.

1. Download https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_1_states_provinces.zip
2. Run tool:

From SHELL:
```bash
python shp2cmaq.py ne_10m_admin_1_states_provinces.zip iso_3166_2 36US3
```

In python:
```python
import shp2cmaq
shp2cmaq('ne_10m_admin_1_states_provinces.zip', 'iso_3166_2', '36US3')
```
