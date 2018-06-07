# Resolve bugs in initialization of Aerosol Schemes  

**Author/P.O.C.:**, [Ben Murphy](mailto:murphy.benjamin@epa.gov), Computational Exposure Division, U.S. EPA  

## Brief Description

 CMAQv5.2 employs a routine to check that the aerosol size distributions read in to the model from initial and boundary conditions is within reasonable bounds for the numerical approaches used in the aerosol module. Unfortunately, the approach in the release version of the code makes a number of small mistakes. First, it does not use the "dry" aerosol mass as it should, but instead uses the "wet" mass. Second, the restrictions on the aerosol diameter are too tight, especially at the upper bound. Third, the aerosols really should not be checked from the ICs on a restart day. It is more important for the size distribution to be continuous from day to day.
 Because of these errors, number concentrations in all modes is artificially high at the beginning of every day. The discontinuity in number concentration, surface area, standard deviation and diameter can easily be seen. The mass is completely conserved and unaffected by these errors.
The following updates have been introduced:

- A mask has been used to select for only the dry components of the aerosol when summing the mass, LBCDRY( N_AE_TRANS )

- The tolerances on the aerosol diameter, min_diam_g and max_diam_g, have been relaxed.

- The "NEW_START" environment variable is now read and used by load_cgrid.F in order to distinguish between the initial day and subsequent days.

 In addition, the subroutine getpar is needed to synchronize the aerosol properties like diameter and standard deviaiton with advected quantities such as number, surface area and mass. This routine also updates the current estimate of the aerosol bulk density. It should be called at the beginning of the aerosol module, but was erroneously skipped before calculation of organic aerosol partitioning. 

## Significance and Impact

 The resolution of these initialization issues should result in more stable code that avoids numerical inconsistencies and spontaneous model hangs.
 The impact on PM2.5 species of correctly calling getpar is sporadic and difficult to predict. Tests within EPA have revealed differences generally less than 1 ug/m3 but could be as much as 6 ug/m3 in parts of the Southeast US in summer, 2011. The wet diameter of the accumulation mode can be quite high on the order of 100 nm and the standard deviation of that mode was affected by as much as 1.2, but generally less than 0.6.
 It is also suspected, though unconfirmed, that this issue can be partly responsible for variable predictions of particle properties aloft when using different compilers.

## Affected Files:

CCTM/src/aero/aero6/AEROSOL_CHEMISTRY.F  
CCTM/src/aero/aero6/AERO_DATA.F  
CCTM/src/aero/aero6/SOA_DEFN.F  
CCTM/src/aero/aero6/aero_driver.F  
CCTM/src/aero/aero6/aero_subs.F  
CCTM/src/aero/aero6/getpar.f  
CCTM/src/hadv/yamo/rdbcon.F  
CCTM/src/init/yamo/load_cgrid.F  

## References:    

-----
## Internal Records:


### Relevant Pull Requests:
  [PR #245](https://github.com/USEPA/CMAQ_Dev/pull/245)  
  [PR #252](https://github.com/USEPA/CMAQ_Dev/pull/252)  
  [PR #258](https://github.com/USEPA/CMAQ_Dev/pull/258)  

### Commit IDs:

c4634d52313b02d7a8cb858f0c1363d65564e80a  
ca6d2aacc4db96683fc69a88ef9e29dce91fcfb8  
79753be2514e9d61210f6462152d576f8e9519da  
9e6fc95e7f22b86b412401bd3712bc7060160255  
b42df907c11020b7024db9da1aaa390a0565eb5c  
6146c854aeb7301b494b183af1e16144a1634f0e  
aa9ba8a6e672f486ce8e6b2489cb0a80bc943f5a  
1c02286e589b06c60566fe84b82e5bd5f284e5bd  
7508c9c3cb288a21ab2f882e5cd5f67c5158da5b  
cc0e8574f3f5a2403d983a84098d5bd4156a6694  
c09f8401e2d83e79d4ef86e5e5c39a44debb2176  

