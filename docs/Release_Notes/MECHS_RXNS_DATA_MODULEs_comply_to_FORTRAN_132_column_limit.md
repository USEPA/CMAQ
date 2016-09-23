# MECHS RXNS_DATA_MODULEs comply to FORTRAN 132 column limit
    
**Author/P.O.C.:**, [William T. Hutzell](mailto:hutzell.bill@epa.gov), Computational Exposure Division, U.S. EPA
    
## Brief Description

For F90 files, some FORTRAN compilers will do not allow line lengths to exceed column 132 that is a limit in the
FORTRAN standard. Most of the changes remove the problem. 
    
## Significance and Impact

Makes to the model code for robust regards to changing the  FORTRAN compiler.    
    
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
Pull Request #90 (/usepa/cmaq_dev/pull/90)
    
### Commit IDs:
66fa93ce679070b1e1ac39ed964e5a217221631d
    
