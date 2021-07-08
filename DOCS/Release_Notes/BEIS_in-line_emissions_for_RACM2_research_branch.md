
# Research Branch BEIS in-line emissions for RACM2 

[Golam Sarwar](mailto:sarwar.golam@epa.gov), U.S. Environmental Protection Agency

## Brief Description
CMAQ has two options for calculating in-line biogenic emissions: BEIS and MEGAN. For BEIS, CMAQ uses an emission profile for calculating in-line biogenic emissions. Xiaoyang Chen at Northeastern University notified that CMAQ (BEIS) is not generating any monoterpene emissions when in-line option is enabled with RACM2. Emission profile 'B10RD' is currently used for RACM2; however, it does not contain correct mapping which results in no monoterpene emissions. An emission profile 'B3V10' was generated when RACM2 was initially implemented in CMAQ which contains correct mapping of model species. The model is revised to remove emission profile 'B10RD' and add the emission profile 'B3V10'. Biogenic emissions calculation using MEGAN does not use this profile and works properly.

Note: This pull request is for the research branch and address the issue #730.
 
## Significance and Impact

Model tests were completed with the revised emissions profile. It generates correct biogenic emissions for RACM2 using BEIS.

## Affected Files

CCTM/CCTM/src/emis/emis/BIOG_EMIS.F
CMAQ/CCTM/CCTM/src/biog/beis3/gspro_biogenics.txt

## References:
None

-----
## Internal Records
#### Relevant Pull Requests:
[PR #739](https://github.com/usepa/cmaq_dev/pull/739)
#### Commit IDs:
e9c49072845a71e88242c9e6917a224eabaea048


-----
