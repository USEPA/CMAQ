# Implemented Aerosol Transmission Factor Calculations for PM1 and AMS Applications

'''Author/P.O.C.:''', Ben Murphy, murphy.benjamin@epa.gov, Computational Exposure Division, U.S. EPA

## Brief Description 

The code currently outputs a factor (one for each mode) quantifying the sharp cutoff for PM2.5. It does so by converting the Aerodynamic Diameter Bound (2.5 um) to Stokes Diameter (used inside CMAQ) with an approximation to the slip correction factor adjustment. These factors are output in the AERODIAM file, if requested. Factors have been added for calculating the sharp cutoff for PM1 and PM10. There was a concern that the numeric approximation used (Jiang et al., 2004) would not be applicable at small sizes. It was ofund that it is indeed fine to apply at small sizes and so the calculations can be easily transcribed. The subroutine is renamed AERO_INLET.

A new subroutine has also been added (AERO_AMS) which calculates the transmission factor for each mode as if it were sampled by an AMS (with an aerodynamic lens). Several details are considered:
1) The particles likely lose a lot of water in the system. In fact, it is now recommended that operators dry the particles before they go into the AMS. So the code recalculates the modal parameters without water.
2) There is not a hard cutoff for the sample. Instead there are three piecewise ranges for transmission. (40 nm < Dp < 100 nm | 100 nm < Dp < 550 nm | 550 nm < Dp < 2000 nm). They each have a transmission function as detailed on Jimenez AMS Users Website and in Ensberg et al, 2013. The factor has been implemented following Ensberg et al., 2013 (Appendix B).
3) The instrument samples in Vacuum Aerodnynamic Diameter, which has to be converted to Stokes diameter, consistent with DeCarlo et al., 2004 and Ensberg et al, 2013.

Finally, the output for Modal composite densities was added to the AERODIAM (now PMDIAG) output file.

## Significance and Impact

(significance and impact on modeled results and runtime)

## Relevant Pull Requests: 
  [PR #22](/usepa/cmaq/pull/22)

## References: 

1. DeCarlo et al., Particle Morphology and Density Characterization by Combined Mobility and Aerodynamic Diameter Measurements. Part 1: Theory, Aerosol Sci. and Technology, 38:1185-1205, 2004
2. Ensberg et al., Inorganic and black carbin aerosols in the Los Angeles Basin during CalNex, Journ. Geophys. Res., 2013.
3. Jiang et al., 2004
