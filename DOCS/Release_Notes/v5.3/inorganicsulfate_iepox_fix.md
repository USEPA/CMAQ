# Conversion of inorganic to organic sulfate

**Author/P.O.C.:**, [Havala O. T. Pye](mailto:pye.havala@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description
                       
Acid-catalyzed reactions of IEPOX result in a number of condensed-phase products
including 2-methyltetrols and IEPOX-organosulfates. IEPOX SOA in CMAQ has
conceptually included this composition since v5.1. In ae6i/ae7i treatments, the
speciation into tetrols, organosulfates, dimers, etc are explicit. In ae6/ae7 treatments
all IEPOX aerosol products are summed together as AISO3J.

Recent work by a number of groups has built confidence in our understanding of IEPOX-SOA
composition and supports the idea that IEPOX-derived organosulfates are a large fraction
of the IEPOX aerosol. All ae7 and ae7i mechanisms have been updated to consume inorganic
sulfate when IEPOX-SOA is formed. 
The amount of inorganic sulfate consumed is determined based on the
relative rate of particle phase reaction with sulfate vs other nucleophiles as indicated
in Pye et al. 2013 (with updated organosulfate rate constant following the work of Piletic
et al. 2013 as implemented in Pye et al. 2017).

Due to logistical considerations, ae6i was also updated. AERO6 mechanisms were not updated.

## Significance and Impact
Decreased accumulation mode inorganic sulfate (decreased PM2.5 sulfate) where isoprene emissions are abundant (e.g. southeast
United States). Slight reduction in IEPOX-SOA due to feedback in which less inorganic sulfate
means lower acidity and less abundant nucleophiles. Effects are minimal when inorganic
sulfate is high relative to IEPOX (e.g. present day eastern US effects <5-10%). Effects more substantial when inorganic sulfate is low
compared to IEPOX (e.g. Amazon, future US emission scenarios).

## Affected Files
CCTM/src/aero/aero7/AEROSOL_CHEMISTRY.F
CCTM/src/MECHS/*/mech*.def
CCTM/src/MECHS/*/GC*.nml
associated RXNS*F90 and ebi files


## References

Pye, H. O. T., Murphy, B. N., Xu, L., Ng, N. L., Carlton, A. G., Guo, H., Weber, R., Vasilakos, P., Appel, K. W., Budisulistiorini, S. H., Surratt, J. D., Nenes, A., Hu, W., Jimenez, J. L., Isaacman-VanWertz, G., Misztal, P. K., and Goldstein, A. H.: On the implications of aerosol liquid water and phase separation for organic aerosol mass, Atmos. Chem. Phys., 17, 343-369, https://doi.org/10.5194/acp-17-343-2017, 2017.

Pye, H. O. T., R. W. Pinder, I. Piletic, Y. Xie, S. L. Capps, Y.-H. Lin, J. D. Surratt, Z. Zhang, A. Gold, D. J. Luecken, W. T. Hutzell, M. Jaoui, J. H. Offenberg, T. E. Kleindienst, M. Lewandowski, and E. O. Edney, Epoxide pathways improve model predictions of isoprene markers and reveal key role of acidity in aerosol formation, Environ. Sci.  Technol., doi:10.1021/es402106h, 2013.


-----
## Internal Records:
#### Relevant Pull Requests:
[PR #480]

-----
