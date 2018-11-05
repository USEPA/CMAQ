# Updates to the chlorine chemistry in CB6r3 mechanisms in CMAQv5.3

**Author/P.O.C.:** [Deborah Luecken](mailto:luecken.deborah@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

The chlorine chemistry in CB6 has been reviewed and many of the reactions have been updated for the v5.3 release. Several new reactions have been added, including two heterogeneous reactions. Most of the existing reactions are retained unless updates are needed, either due to new information on reaction rates/prodocts or changes in mechanism species.  One new model species is added in this update: chlorine nitrate.  The chlorine chemistry in CMAQv5.3 is intended to be consistent with the chlorine chemistry in CAMx CB6r2 (Ramboll Environ, 2016).  The chlorine chemistry is the same in all versions of CB6 included in CMAQv5.3.

* Reactions unchanged from CMAQv5.2:

      CL1, CL2, CL3, CL4, CL5, CL8, CL9, CL14, CL15, CL16, CL18-23, HET_H2NO3PIB, HET_H2NO3PJB, HET_H2NO3PKB 

* Reactions updated to match IUPAC, or CB6r2:

      CL6, CL10, CL11, CL13, CL17, CL24-27

* New reactions:

      CL7,  CL12,CL28-31, HET_CLNO3_WAJ, HET_CLNO3_WAK 
 

## New Species
Chlorine nitrate (species CLNO3)

## Files Affected
* CCTM/src/MECHS/cb6mp_ae6_aq/*
* CCTM/src/MECHS/cb6r3_ae6_aq/*
* CCTM/src/MECHS/cb6r3_ae7_aq/*
* CCTM/src/MECHS/cb6r3_ae7_aqkmt2/*

## References
Dieber,G., George, Ch, LeCalve, S., Schweitzer, F., Mirabel, Ph., 2004.  Uptake study of ClONO2 and BrONO2 by Halide containing droplets, atmos. chem. phys., 4, 1291-1299. 

IUPAC, 2017. Task Group on Atmospheric Chemical Kinetic Data Evaluation.  http://iupac.pole-ether.fr/ (accessed 10-2-17).
Ramboll Environ, 2016.  Users Guide Comprehensive Air Quality Model with Extensions, version 6.3, Novato, CA, http://www.camx.com/files/camxusersguide_v6-30.pdf

Sarwar, G., email thread of 6-21 thorugh 6-25-2018.  

Yarwood, G., Rao, S., Yocke, M., Whitten, G.Z., 2005. Updates to the Carbon Bond Mechanism: CB05.  Final Report to the US EPA, RT-0400675. Yocke and Company, Novato, CA.

## Internal Records:
#### Relevant Pull Requests:
[PR #359](https://github.com/USEPA/CMAQ_Dev/pull/359), [PR #342](https://github.com/USEPA/CMAQ_Dev/pull/342)


