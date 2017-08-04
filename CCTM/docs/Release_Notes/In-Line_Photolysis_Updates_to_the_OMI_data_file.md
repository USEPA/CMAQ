# Updates to the OMI data file

**Author/P.O.C.:**, [William T. Hutzell](mailto:hutzell.bill@epa.gov), Computational Exposure Division, U.S. EPA    

## Brief Description

Two changes to CMAQv5.2 are described here. First, the total OMI ozone column data that are used to calculate photolysis rates are changed. New data have higher (daily) temporal resolution of the total ozone column. Second, the OMI file in prior versions of CMAQ denoted missing values by negative one (-1). The new method, however, replaces missing values on a given date with values from the last available date. The file's name was also changed to reflect the full calendar years covered.

This update also modifies the error messages when the simulated tropospheric ozone column exceeds the OMI column.

Note that this update also includes earlier changes to **phot/inline/CSQY_DATA.F**. It adds to number cases for pressure
and temperature corrections to quantum yields. The changes were added for future developments in photochemical
mechanisms such as using CRI and MCM mechanisms.    

## Significance and Impact

The change replaces the file OMI.dat with OMI_1979_to_2015.dat so the user has to change the value of the **OMI** environment variable in the run script.

Test simulations during January and July 2011 showed the mean  in ozone predictions changed less than 1 ppbV. However, hourly predictions changed
by up to plus or minus 4 ppbV at isolated locations. Analysis indicated that changes in O1D production caused changes in OH concentrations.  

## Affected Files:

aero/aero6/SOA_DEFN.F  
phot/inline/CSQY_DATA.F  
phot/inline/OMI.dat (deleted)  
phot/inline/OMI_1979_to_2015.dat (added)  
phot/inline/PHOT_MOD.F  
phot/inline/o3totcol.f  
phot/inline/opphot.F  
phot/inline/phot.F  

## References:    

None

-----
## Internal Records:

### Relevant Pull Requests:
  [PR #100](https://github.com/USEPA/CMAQ_Dev/pull/100)

### Commit IDs:

c6bf48c7aa3498d9de58da2322c8ae5ea2e3fe83  
d5312ebd925a5fd2cb2d0b3f79ecca39702107f7  
304492d7d0a45c9bad21f566fc9f92ecd93af8ed  
80de1280929f2b3f216a95214c41782888699086  
9540e558d50ca03b64930e104eb93a458a6c33c1  
ceac6943a67b542913f97cfe7aa1a9cf8afc0504  
8b5b5627b4fb6aa02476326adf1d3e31fdd7509d  
1331cf940aee9a29e395e4b5f3bb188fb4a0fe26  
79093b57e44609a88c1723915a87aa97bf0c0218  
edc8286d2a43652a512d8383cb08ef21be9c01f7  
8ab05d887d695acabe1ef8a5e106cad6773209d1  
19a4cdc0edfbacb766f039e0648bfa5fc432e687  
31efb12e677c2cb9dd027d47c820d4694a88cb98  
116e08782e212152b71c49b5f2ced1b8110cbbff  
f9e0c2cf6b83b6c267e02a6eceb6ff68b0b8e329  
5691f0554d72e72134cb099f154dc8bc568f8b80  
ee81e7a54e667148cd85573c85cf240b043d8b48  
a43ed4622e61bc1107fd66aab1e91927d047fdd5  
fec193d31d33267b2b69718103123e1efbb158bd  
86d1b21458789ba5907ffda64ce652bc1360ae3c  
97dbab568cf4d9d6ee8faa39d3d9bb9128b37088  
b0734ea99b51f07b9d15edb0bfc48839c72fbc16  
72f8a3e4c0ed8bb4141928f60514dcdf32fe6907  
9eb17b070a8fa334eba32a2d4d9992f7a06896c4  
9d304dd9aac522c420c85b5a3706547062e53e98  
dc098cd364b19629e64733e1f006d89803eea843  
6a7bb8972de7350332da1bd2246eeb7708addfee  
7f07f4f15f5e1b85459530d782faf2d061fe56bb  
