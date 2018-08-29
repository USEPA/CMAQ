# MECHS RXNS_DATA_MODULEs comply to FORTRAN 132 column limit

**Author/P.O.C.:**, [William T. Hutzell](mailto:hutzell.bill@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

For F90 files, some FORTRAN compilers do not allow lines to extend past column 132. This limitation is in the FORTRAN standard. This update changed line lengths in F90 files to conform to the standard.

## Significance and Impact

Changed the affected files to make them compatible with F90 FORTRAN compilers.

## Affected Files:
MECHS/cb05e51_ae6_aq/RXNS_DATA_MODULE.F90  
MECHS/cb05tucl_ae6_aq/RXNS_DATA_MODULE.F90  
MECHS/cb05tump_ae6_aq/RXNS_DATA_MODULE.F90  
MECHS/racm2_ae6_aq/RXNS_DATA_MODULE.F90  
MECHS/saprc07tb_ae6_aq/RXNS_DATA_MODULE.F90  
MECHS/saprc07tb_ae6_aq/RXNS_FUNC_MODULE.F90  
MECHS/saprc07tc_ae6_aq/RXNS_DATA_MODULE.F90  
MECHS/saprc07tic_ae6i_aq/RXNS_DATA_MODULE.F90  

## References:    

None

-----
## Internal Records:

### Relevant Pull Requests:
[PR #90](https://github.com/USEPA/CMAQ_Dev/pull/90)  

### Commit IDs:
66fa93ce679070b1e1ac39ed964e5a217221631d   
