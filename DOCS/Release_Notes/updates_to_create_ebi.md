# Updates to the Utilities that creates an EBI solver

**Author/P.O.C.:**, [William T. Hutzell](mailto:hutzell.bill@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

The create_ebi utility has been updated to accomplish two tasks. First, it adds a run-script option to the create_ebi utility that sets the maximum time step for integrating for a solution. The utility writes the setting to hrdata_mod.F as the value for DELTAT. Note that the utility uses a default value of 2.5 minutes if the run-script does not set the option. For the second task, updates make a step toward incorporating the ISAM version of CMAQ into the base version. The step focuses on changes needed to the EBI solver of a photochemical mechanism and the utility that creates an EBI solver. The changes add FORTRAN processing statement to subroutines and modify subroutines that compute analytical solutions for O(1D), HNO<sub>4</sub>, NO<sub>3</sub>, and N<sub>2</sub>O<sub>5</sub>. The former changes do not affect model predictions. **This preprocessing option is not operational because additional files are needed and future pull requests will add these files.** The latter changes do affect results from the solver. The analytical solutions were modified; Changes to the hrg[1,2,3].F in the ebi solvers to improve performance for calculating source apportionment.  The change also produced better agreement between the concentration of photochemical species and the sum of their source contributions. 

## Significance and Impact

To determine how these changes alter model predictions, sensitivity tests were conducted over hemispheric, 12 km<sup>2</sup> continental US, and 4 km<sup>2</sup> California domains using the cb6r3_ae6_aq and saprc07tic_ae7i_aq mechanisms, respectively. Ozone and fine mode sulfate show less than 0.5 ppbV and 0.5 ugm<sup>3</sup> in the maximum absolute value of hourly differences. The only species with differences greater than a ppbV was HNO<sub>3</sub>. The differences occurred for wintertime cases or VOC limiting areas. The noticeable differences occurred in isolated locations and at sporadic times so the mean differences were equal to or more than two orders of magnitude lower than grid cell averages for HNO<sub>3</sub>.

## Files Affected
All ebi solvers for all photochemical mechanisms under CCTM/src/gas.
