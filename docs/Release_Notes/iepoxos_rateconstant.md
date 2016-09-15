# Updated IEPOX organosulfate formation rate constant

**Author/P.O.C.:**, [Havala Pye](mailto:pye.havala@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

IEPOX uptake to aqueous particles results in 2-methyltetrols and organosulfates along with other species. The organosulfate formation rate constant was updated to use
the IEPOXOS:tetrol relative value from Piletic et al. (2013). Thus the aqueous organosulfate formation rate constant was updated to 8.83 x 10<sup>-3</sup> M<sup>-2</sup> s<sup>-1</sup>.


## Significance and Impact

Increases IEPOX organosulfates (species AIEOSJ in aero6i or a portion of AISO3J in aero6) and thus total SOA.

## Affected Files

aero/aero6/AEROSOL_CHEMISTRY.F

cloud/acm_ae6/hlconst.F

## References

Piletic, I. R., E. O. Edney, L. J. Bartolotti, A computational study of acid catalyzed aerosol reactions of atmospherically relevant epoxides, Phys. Chem. Chem. Phys., 2013,15, 18065-18076, 
DOI: 10.1039/C3CP52851K. [link](http://pubs.rsc.org/en/Content/ArticleLanding/2013/CP/c3cp52851k#!divAbstract)


-----
## Internal Records:
#### Relevant Pull Requests:
[PR #11](https://github.com/usepa/cmaq_dev/pulls/11) 


#### Commit IDs:
e246de9d80a9076575a3d4a83071ed4edeca4a56


-----
