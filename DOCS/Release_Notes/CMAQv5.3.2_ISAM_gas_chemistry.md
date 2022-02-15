# ISAM Chemistry Update 
[Bill Hutzell](mailto:Hutzell.Bill@epa.gov), [Sergey Napelenok](mailto:napelenok.sergey@epa.gov), U.S. Environmental Protection Agency


## Brief Description
Changes attempt to better estimate source contributions/concentrations for model species that participate in gas chemistry. They also change names for several base tags, remove the CO tag class, and add error checks in reading the isam control file.

The pull request attempts to remove two problems in results on how gas chemistry affects the jth source concentration of the sth chemical species, c(s,j), such as ozone from biogenic emissions.


The pull request attempts to remove two problems in results on how gas chemistry affects the  j<sup>th</sup> source concentration of the s<sup>th</sup> chemical species, c(s,j), such as ozone from biogenic emissions.    
1.   The "other" (_i.e., not explicitly tagged sources_), boundary and initial contributions showed unrealistically large contributions that often dominated the other source contributions.      
2.   For some species, we noticed that the sum of c(s,j) over all sources did not always match bulk concentrations, c<sub>T</sub>(s).   

Modifications introduced in the ISAM algorithm to quantify and track secondary production from tagged sources now help rectify the two unrealistic behaviors. SA_IRR_DEFN.F was changed. They replace how the tracked species are updated after  c<sub>T</sub>(s) values are determined at t=dt. The below algorithm is used where the current code calculates P<sub>T</sub>(s), P<sub>T</sub>(s), and P(s,j).


![Chem_ISAM_algorithm](https://user-images.githubusercontent.com/16845494/83574883-5f8cd380-a4fc-11ea-9b55-5bfd740c6eea.png)


SA_DEFN.F and other files in the CCTM/src/isam are revised to accomplish the below items:

1) Remove the CO tag class and add CO as a tracked species to the VOC tag class even though CO is not an organic compound.
2) Change the OTHER, ICON, and BCON tag names to OTH, ICO, and BCO. Recommend a three character limit on tag names defined by a user. The step allows a twelve character limit in tracked model species.
3) Allow tag classes have duplicates of tracked species. The revised code removes duplicates at run time.
4) Add emitted methane, ECH4, as tracked species in VOC tag class if the chemical mechanism is cb6r3 based.
5) Add dimethyl sulfide, DMS, as tracked species in SULFATE tag class if the chemical mechanism is cb6r3m_ae7_kmtbr
6) Add error checking and messaging in routines that read the isam control file. The checks determine whether tag classes and tagged sources are correctly defined. The model stops with error messages if not. Checks on tagged sources only determine if their name, region(s) and emission stream(s) are given.

## Significance and Impact
The update substantially changes the predictions of tagged secondary gaseous species (particularly ozone).  The new estimates do a much better job at assigning secondary mass to appropriate sources rather than to "OTHER" and boundary/initial conditions. See the [CMAQ User's Guide Chapter 11](../Users_Guide/CMAQ_UG_ch11_ISAM.md) for additional information on CMAQ-ISAM, including build and run instructions. 


## Affected Files
CCTM/src/isam/SA_DEFN.F   
CCTM/src/isam/SA_IRR_DEFN.F  
CCTM/src/isam/op_sa.F  
CCTM/src/isam/sa_array_init.F  
CCTM/src/isam/sa_dim.F  

## Relevant Pull Requests:
[PR #644](https://github.com/USEPA/CMAQ_Dev/pull/644)

## Commit IDs:
a283ee70b7bfcbf2c40075f99fe8f2086fd8444c   
b9af03a5889460b56c0a1f943e6eeba75de85c3b  
a5b4c458a0cb9518b54052bbe2ad36369a9d8b64   
affbf95d173a5295254d9bfef4d1e2449488319e  

