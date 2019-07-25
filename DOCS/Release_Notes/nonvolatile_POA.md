# Nonvolatile POA Option in CMAQv5.3

[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency


## Brief Description
Before CMAQv5.2, POA was assumed to be nonvolatile and represented by two model species for all sources: POC, which represents organic carbon mass; and PNCOM, which represents non-carbon organic mass. In CMAQv5.2, species were introduced to represent the semivolatile partitioning and gas-phase aging of POA. In this model version, users were able to choose from chemical mechanisms with semivolatile POA or mechanisms with nonvolatile POA, depending on their application. This arrangement led to a lot of overhead in both develpoment and use since the model had to be recompiled in order to toggle between options.  

Beginning with CMAQv5.3, both nonvolatile and semivolatile POA approaches are supported in every mechanism. Every Aerosol chemical namelist contains both the semivolatile and nonvolatile species and every chemical mechanism will compute the aging of both sets of species. It is up to the user to direct the emissions from every source stream to either the nonvolaitle or semivolatile species depending on the assumptions they would like to enforce about the volatility distribution of the organic particulate compounds emitted from each stream. This assumption is invoked through the DESID emission control interface. There is documentation available in the emission control interface file template to guide users through the process of setting this volatility. By default, all source streams are assumed to be semivolatile.

## Significance and Impact
This improvement dramatically reduces the maintenance cost of providing an option for nonvolatile POA. It does increase slightly the operational cost since, for example, semivolatile POA species will have to be transported in CMAQ even if the user is not passing any emissions to these species. However, the total runtime for CMAQ is not affected signicantly by this penalty.

## Affected Files
CCTM/src/MECHS/cb6mp_ae6_aq/AE_cb6mp_ae6_aq.nml  
CCTM/src/MECHS/cb6mp_ae6_aq/EmissCtrl_cb6mp_ae6_aq.nml  
CCTM/src/MECHS/cb6mp_ae6_aq/SpecDef_cb6mp_ae6_aq.txt  
CCTM/src/MECHS/cb6r3_ae6_aq/AE_cb6r3_ae6_aq.nml  
CCTM/src/MECHS/cb6r3_ae6_aq/EmissCtrl_cb6r3_ae6_aq.nml  
CCTM/src/MECHS/cb6r3_ae6_aq/RXNS_DATA_MODULE.F90  
CCTM/src/MECHS/cb6r3_ae6_aq/RXNS_FUNC_MODULE.F90  
CCTM/src/MECHS/cb6r3_ae6_aq/SpecDef_cb6r3_ae6_aq.txt  
CCTM/src/MECHS/cb6r3_ae6_aq/mech_cb6r3_ae6_aq.def  
CCTM/src/MECHS/cb6r3_ae7_aq/AE_cb6r3_ae7_aq.nml  
CCTM/src/MECHS/cb6r3_ae7_aq/EmissCtrl_cb6r3_ae7_aq.nml  
CCTM/src/MECHS/cb6r3_ae7_aq/RXNS_DATA_MODULE.F90  
CCTM/src/MECHS/cb6r3_ae7_aq/RXNS_FUNC_MODULE.F90  
CCTM/src/MECHS/cb6r3_ae7_aq/SpecDef_cb6r3_ae7_aq.txt  
CCTM/src/MECHS/cb6r3_ae7_aq/mech_cb6r3_ae7_aq.def  
CCTM/src/MECHS/cb6r3_ae7_aqkmt2/AE_cb6r3_ae7_aq.nml  
CCTM/src/MECHS/cb6r3_ae7_aqkmt2/GC_cb6r3_ae7_aq.nml  
CCTM/src/MECHS/cb6r3m_ae7_kmtbr/AE_cb6r3m_ae7_kmtbr.nml  
CCTM/src/MECHS/cb6r3m_ae7_kmtbr/EmissCtrl_cb6r3m_ae7_kmtbr.nml  
CCTM/src/MECHS/cb6r3m_ae7_kmtbr/RXNS_DATA_MODULE.F90  
CCTM/src/MECHS/cb6r3m_ae7_kmtbr/RXNS_FUNC_MODULE.F90  
CCTM/src/MECHS/cb6r3m_ae7_kmtbr/SpecDef_cb6r3m_ae7_kmtbr.txt  
CCTM/src/MECHS/cb6r3m_ae7_kmtbr/mech_cb6r3m_ae7_kmtbr.def  
CCTM/src/MECHS/mechanism_information/cb6mp_ae6_aq/AE6_species_table.md  
CCTM/src/MECHS/mechanism_information/cb6mp_ae6_aq/cb6mp_ae6_aq_species_table.md  
CCTM/src/MECHS/mechanism_information/cb6r3_ae6_aq/AE6_species_table.md  
CCTM/src/MECHS/mechanism_information/cb6r3_ae6_aq/cb6r3_ae6_aq_species_table.md  
CCTM/src/MECHS/mechanism_information/cb6r3_ae6_aq/mech_cb6r3_ae6_aq.md  
CCTM/src/MECHS/mechanism_information/cb6r3_ae7_aq/AE7_species_table.md  
CCTM/src/MECHS/mechanism_information/cb6r3_ae7_aq/cb6r3_ae7_aq_species_table.md  

## References

-----
## Internal Records:
#### Relevant Pull Requests:
85404794d443ff8627c9c763cd5c3a76e0154465  
49085f33fe3c998a8f04136c66f92fd1fa15d14e  
1b2c208f59135c7e260478b866704f5e9cdcac23  
87ccf530ee3e6794a2db01087abfbd91522a2b23  
981bfbd48343031b8e71f45dcdf95d9a0ed2de94  
1662ba94edb2f6c3af54c91d9d1204c2f8fa5cb3  
b79b1936c61b61cf2c8b7e565985aed1e94c649b  
8ad67489739fadeeda26dcd49eee4b80afdf9d1f  
7d835ce672ea77cc2ad08e8882e7caa7abffa3ed  
6a2e0d03b1f49f2d186f553ff553ca469e06e69d  
67376d9021cbcf7206fcab94e9154ab726e2675a  
89e67723a49143b553675af2b9f6a97a6bf28408  
64e3c9473f6ac117f0e35326fe237d2b5c580184  
ffb95f1932f17db77979063ede02a1550e441462  
35c389166fc9f3bcba6c4ae9c2f3c560648e5337  
836b9b49c648a3e3d05ecd816c44c6c8dcc33304  



-----

