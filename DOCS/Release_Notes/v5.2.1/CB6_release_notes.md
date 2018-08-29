# Inclusion of CB6r3 chemical mechanism

**Author/P.O.C.:**, [Deborah Luecken] (mailto:luecken.deborah@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

Included with CMAQv5.2 is a new mechanism that has not been available in previous versions, the CB6r3 (Emery et al., 2015). CB6r3 is the fourth version of CB6 (Yarwood et al. 2010), which is based on CB05.

CB6 contains numerous changes from CB05, including updated reaction rates throughout, new operators to better represent alkoxy and peroxy radicals, additional explicit species to represent long-lived and abundant chemicals and those that form SOA, and additional representation of organic nitrates.  These changes are detailed in reports listed in the References section.

## Significance and Impact

Running CB6r3 in CMAQ takes approximately the same amount of time as CB05e51.

In simulations, CB6 generally produces lower NO2 and lower ozone, due to multiple factors including lowered production of alkylnitrates in the winter, lack of NOx recycling from alkyl nitrates, lower HO2 by considering alkoxy radical reactions where it is not automatically produced, and heterogeneous removal of nitrates. Since so many reactions and species have changed, it is difficult to separate out each modification, as some will act to increase ozone and some to decrease ozone and radicals.

## Affected Files:

All files in MECHS/cb6r3_ae6_aq  
All files in gas/ebi_cb6r3_ae6_aq  
aero/aero6/AEROSOL_CHEMISTRY.F  
aero/aero6/SOA_DEFN.F  
spcs/cgrid_spcs_nml/CGRID_SPCS.F  

## References:

Emery, C., Jung, J., Koo, B., Yarwood, G., 2015. Improvements to CAMx Snow Cover Treatments and Carbon Bond Chemical Mechanism for Winter Ozone. Final report for Utah DAQ, project UDAQ PO 480 52000000001.

Hildebrandt Ruiz, L., Yarwood, G., 2013. Interactions between Organic Aerosol and NOy: Influence on Oxidant Production. Final report for AQRP project 12‐012. Available at http://aqrp.ceer.utexas.edu/projectinfoFY12_13/12-012/12-012%20Final%20Report.pdf.

Yarwood, G., Whitten, G.Z., Jung, J., Heo, G., Allen, D.T., 2010. Development, evaluation and testing of version 6 of the Carbon Bond chemical mechanism (CB6), Final report to the Texas Commission on Environmental Quality, Work Order No. 582-7-84005-FY10-26.

----
## Internal Records:

#### Relevant Pull Requests:
[PR #44](https://github.com/usepa/cmaq/pulls/44)  
[PR #115](https://github.com/usepa/cmaq/pulls/115)

#### Commit IDs:
577a3f04d710c622c910b3f043c49cb60d87c1de
072917fc2a8a91f254a3958e3d4f9a13afc893d2
1f94d4cc74be5b1d11481d63e8610ff0b70ef36b
97e7b4413c4abcb1ac48bb29f64475bfedd2a338
89120a16ba1860d9dfeb54fb9b06b6029275af77
c1efa16d7cbd26779a46986d017bb3f535c6ba56
6acc771f5266da0b8ee6d01d84d123699a9e47da
1b728cfc8d7424aa48a76e46e45941d6bdca954b
43f3f2c1d7ddcaba4d9b952bf0cd7406335d62ce
2ecec2e06075b17cca1dde8a805691daeefbcf0e
c595a6894fa625ce9bff75f98a4cfcae26edd05c
