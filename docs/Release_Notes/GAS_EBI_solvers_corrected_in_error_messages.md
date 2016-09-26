# Chemistry EBI solvers corrected in error messages
    
**Author/P.O.C.:**, [William T. Hutzell](mailto:hutzell.bill@epa.gov), Computational Exposure Division, U.S. EPA
    
## Brief Description

The changes prevent a model crash when the solver writes a warning message before reducing the time step because
it has encountered a maximum prediction error. A segmentation error causes the crash because the CHEMISTRY_SPC array is
given an index one greater than the array's size. The bad value comes from a completed DO loop that tests convergence
for all chemistry species. Because a preprocessor creates an EBI solver for an updated or a new photochemical
mechanism, the preprocessor has incorporated a correction based on this pull request. 
    
## Significance and Impact
    
Improves error messaging when then EBI solver encounters a convergence error.  
    
## Affected Files:
    
gas/ebi_cb05e51_ae6_aq/hrsolver.F  
gas/ebi_cb05e51_ae6nvPOA_aq/hrsolver.F  
gas/ebi_cb05eh51_ae6_aq/hrsolver.F  
gas/ebi_cb05mp51_ae6_aq/hrsolver.F  
gas/ebi_cb05tucl_ae6_aq/hrsolver.F  
gas/ebi_cb05tump_ae6_aq/hrsolver.F  
gas/ebi_cb6r3_ae6_aq/hrsolver.F  
gas/ebi_cb6r3_ae6nvPOA_aq/hrsolver.F  
gas/ebi_racm2_ae6_aq/hrsolver.F  
gas/ebi_saprc07tb_ae6_aq/hrsolver.F  
gas/ebi_saprc07tc_ae6_aq/hrsolver.F  
gas/ebi_saprc07tc_ae6nvPOA_aq/hrsolver.F  
gas/ebi_saprc07tic_ae6i_aq/hrsolver.F  
gas/ebi_saprc07tic_ae6invPOA_aq/hrsolver.F  

## References:    

None
    
-----
## Internal Records:
    
    
### Relevant Pull Requests:
  [PR #136](https://github.com/USEPA/CMAQ_Dev/pull/136)
    
### Commit IDs:
    
c1f3fe71edbb1eb71ead0be0898fa505fe2a55b6  
6b6e3e87e57195016d61dc5c868729fecddabd07  
fbac629f9413280877b643f304979ea7cd3431d2  
d640e4495b40348fb7f30f6f731d3a3039ca1c5b  
      
