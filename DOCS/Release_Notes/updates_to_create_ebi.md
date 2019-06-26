# Updates to the utilities that create an EBI solver

[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency

**>>COMMENT<<** In Brief Description, is the bolded text still necessary, or was that overcome in the course of development for v5.3?  TLS 26 Jun 2019

**>>COMMENT<<** In Brief Description, is "improve performance for calculating source apportionment" related to model accuracy or model speed?  TLS 26 Jun 2019

**>>COMMENT<<** In Significance and Impact, what is the resolution of the hemispheric domain used for testing?  TLS 26 Jun 2019

## Brief Description

The *create_ebi* utility has been updated to accomplish two tasks. First, a run-time option was added to *create_ebi* to set the maximum integration time step. The utility writes the setting to hrdata_mod.F as the value for DELTAT. The default value of 2.5 minutes is used for DELTAT when the run-script does not set the option. Second, these updates are a step toward fully incorporating ISAM into CMAQ by changing the EBI solver for a photochemical mechanism and the utility that creates an EBI solver. The changes add FORTRAN processing statements to subroutines and modify subroutines that compute analytical solutions for O(1D), HNO<sub>4</sub>, NO<sub>3</sub>, and N<sub>2</sub>O<sub>5</sub>. The former changes do not affect model predictions. **This preprocessing option is not operational because additional files are needed and future pull requests will add these files.** The latter changes affect results from the solver. The analytical solutions were modified; Changes were introduced into the hrg[1,2,3].F in the EBI solvers to improve performance for calculating source apportionment.  The change also produced better agreement between the concentration of photochemical species and the sum of their source contributions. 

## Significance and Impact

To determine how these changes alter model predictions, sensitivity tests were conducted over hemispheric, 12-km<sup>2</sup> continental U.S., and 4-km<sup>2</sup> California domains using the cb6r3_ae6_aq and saprc07tic_ae7i_aq mechanisms. Differences in the absolute value of the hourly maximum ozone and fine-mode sulfate are less than 0.5 ppbV and 0.5 ug m<sup>-3</sup>, respectively. The only species with differences greater than 1 ppbV was HNO<sub>3</sub>, which occurred for wintertime cases or VOC-limiting areas. The noticeable differences occurred in isolated locations and at sporadic times so the mean differences were equal to or more than two orders of magnitude lower than grid-cell averages for HNO<sub>3</sub>.

## Files Affected
All EBI solvers for all photochemical mechanisms under CCTM/src/gas.
