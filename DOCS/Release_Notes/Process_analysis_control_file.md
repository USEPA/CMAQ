# Release Note Title

**Author/P.O.C.:** [Chris Nolte](mailto:nolte.chris@epa.gov), U.S. Environmental Protection Agency

## Brief Description
The sample Process Analysis control file for the cb6r3_ae7_aq mechanism and documented in the 
[User's Guide](Users_Guide/CMAQ_UG_ch09_process_analysis.md) had typos in the definitions of
two of the IRR output variables. These have been corrected so that the definitions match
the variable names.

## Significance and Impact
No effect on results for the base model. The process analysis control file that is provided is 
merely an illustrative example that is intended to be modified by the user for a 
specific application.

## Affected Files
#### Files modified:
* CCTM/src/MECHS/cb6r3_ae7_aq/pa_cb6r3_ae7_aq.ctl
* CCTM/src/MECHS/cb6r3_ae6_aq/pa_cb6r3_ae6_aq.ctl
* CCTM/src/MECHS/cb6r3_ae7_aqkmt2/pa_cb6r3_ae7_aq.ctl


## References
      
## Internal Records:
#### Relevant Pull Requests:
[PR #617](https://github.com/USEPA/CMAQ_Dev/pull/617)  

#### Commit IDs:
[839561b85521b3dc35374cd810d2fbf109641bec](https://github.com/USEPA/CMAQ_Dev/pull/617/commits/839561b85521b3dc35374cd810d2fbf109641bec)
