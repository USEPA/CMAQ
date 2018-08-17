# Reduce underflow errors in aerosol physics and chemistry

**Author/P.O.C.:**, [William T. Hutzell](mailto:hutzell.bill@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

These changes were made to prevent underflow floating points errors detected. They were detected using the gfortran
version 4.9.3 compiler with a coupled CHEM-AERO box model. Most changes removed implicit conversions between real(4) and
real(8) variables. Other changes added tests to prevent underflow errors or reduced the number of floating point
calculations to improve efficiency.


## Significance and Impact

A five day simulation using the saprc07tic_ae6i_aq mechanism showed that the above changes can alter hourly predictions of
ozone and fine mode sulfate up to four ppbV and less than one ug/m<sup>3</sup>, respectively.  The ozone effect results when aerosol optical
depth, single scattering albedo, and the asymmetry parameter change enough to alter actinic flux between revised and
original models.  The sensitivity occurs through altering mean diameters of aerosol modes. {NO PLOTS PROVIDED: The below plots show some of
the effects based on the last two days of the simulation.}    

## Affected Files:
MECHS/saprc07tb_ae6_aq/RXNS_DATA_MODULE.F90  
aero/aero6/AEROSOL_CHEMISTRY.F  
aero/aero6/aero_subs.F  
aero/aero6/getpar.f  
phot/inline/AERO_PHOTDATA.F  

## References:    

None

-----
## Internal Records:


### Relevant Pull Requests:
  [PR #113](https://github.com/USEPA/CMAQ_Dev/pull/113)

### Commit IDs:

313c11a6bec0cebabe0badf3651062c1b3e3fc83  
3d3162d14906ac3fd98c9c93d36501b82d3faed2  
f702ef68634bb238e6c4d1fbf81c6e473f3feee9  
c4448d8d141d917ab8bda2d4511dbe561c6b2665  
a5dba9bd72521e2d16efbb9797beada6785c7233  
ec9b5ad47422b37ef80d369835e12da82ad68646  
7f946bc5ec8fd24eb61071d8ed72b0301486b567  
e6dbd825444c2d3d87f18ad5ee4cd6f1e52a4a12  
870ccc83f02fc1e32f25070ad60518b5daffb94a  
680d51927af649a2342c1aec63860787af3063ed  
d673476cd0537b0932b96f7e9877ff220cd5671e  
1164848c11fac2730da5616f4d39a00310671b04  
