# Variable Units on Output Files

**Author/P.O.C.:** [Ben Murphy](mailto:murphy.ben@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description
As CMAQ has evolved, the conventions for variable units have evolved inconsistently, with different 
modules optiong for different unit formats (e.g. "m s-1" versus "m/s"). This model update catalogued 
all of the units used throughout the model output file (standard and diagnostic) and updated them all 
to be consistent across CMAQ and as consistent as possible with existing standards in the climate 
modeling community.

Note: dimensionless units are left blank. Units for relative fractions (like RH) are set to 1. Please 
see the Users Guide Appendix for a complete list of the definitions of all units.

## Significance and Impact  
There is now less confusion about the meaning of variable units.

## Affected Files  
CCTM/src/aero/aero6/AOD_DEFN.F  
CCTM/src/aero/aero6/PMDIAG_DATA.F  
CCTM/src/aero/aero6/opavis.F  
CCTM/src/aero/aero6/opvis.F  
CCTM/src/biog/beis3/hrno.F  
CCTM/src/biog/beis3/tmpbeis.F  
CCTM/src/cloud/acm_ae6/opwdep.F  
CCTM/src/depv/m3dry/BIDI_MOD.F  
CCTM/src/depv/m3dry/opdepv_diag.F  
CCTM/src/depv/m3dry/opdepv_fst.F  
CCTM/src/depv/m3dry/opdepv_mos.F  
CCTM/src/diag/vertext_module.F  
CCTM/src/driver/yamo/wr_cgrid.F  
CCTM/src/emis/emis/DUST_EMIS.F  
CCTM/src/emis/emis/LTNG_DEFN.F  
CCTM/src/emis/emis/MGEMIS.F  
CCTM/src/emis/emis/SSEMIS.F  
CCTM/src/gas/ros3/rbdriver.F  
CCTM/src/init/yamo/opaconc.F  
CCTM/src/init/yamo/opconc.F  
CCTM/src/phot/inline/opphot.F  
CCTM/src/phot/table/opphot.F  
CCTM/src/plrise/smoke/openlayout.F  
CCTM/src/plrise/smoke/oppt3d_diag.F  
CCTM/src/procan/pa/pa_mkhdr.F  
CCTM/src/twoway/twoway_aqprep.F90  
CCTM/src/twoway/twoway_feedback.F90   
CCTM/src/vdiff/acm2/VDIFF_DIAG.F  
CCTM/src/vdiff/acm2/opddep.F  
CCTM/src/vdiff/acm2/opddep_fst.F  

## References
NA           

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #323]   

#### Commit 
IDs:                        
63e51197817ffe2acb7c8bdb070410a670df1888  
6425d5d69f21b203514b39f883603b81ff4de8a4  
ad6328843afbe5be8eb45941346fbf556761ba79  

-----

