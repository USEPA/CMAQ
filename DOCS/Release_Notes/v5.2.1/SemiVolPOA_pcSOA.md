# Implemented Semivolatile POA and Potential Combustion SOA (pcSOA)

**Author/P.O.C.:**, [Ben Murphy](mailto:murphy.benjamin@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

Mounting evidence from field and laboratory observations coupled with atmospheric model analysis shows that primary combustion emissions of organic compounds exhibit a broad spectrum of volatility, leading to dynamic partitioning of these compounds, especially in the early stages of their atmospheric lifetime. The most recent version of CMAQ v5.2 accounts for the semivolatle partitioning and gas-phase aging of these primary organic aerosol (POA) compounds consistent with experimentally derived parameterizations.

We include a new surrogate species, potential secondary organic aerosol from combustion (pcSOA). It provides a cumulative representation of the SOA from combustion sources that could be missing from current chemical transport model predictions. The reasons for this missing mass likely include:  
1. Missing intermediate volatility organic compound (IVOC) emissions in current inventories.  
2. Multigenerational aging of organic vapor products from known SOA precursors (e.g., toluene, alkanes).  
3. Underestimation of SOA yields due to vapor losses at the walls in smog chamber experiments.  
4. Organic-water interactions and aqueuous-phase processing of known organic vapor emissions.  

## Significance and Impact

The effect on organic aerosol (OA) concentrations is significant, especially near combustion sources. In cities, especially in winter with low photochemical activity, the OA concentrations should decrease. In summer and/or rural areas, concentrations may stay the same or increase slightly depending on the behavior of pcSOA. Although it is technically a new source of OA to the model, in practice it compensates for a lot of the material evaporated from the POA source.

Representation of semivolatile POA improves model predictions of hourly observations over the nonvolatile model at sites in California during the CalNex and CARES campaigns (May-June 2010) as well as in the southeast during the Southern Oxidant and Aerosol Study (SOAS) (May 2013) and at SEARCH sites in 2011 winter and summer. Model improvements manifest in better correlations (e.g. correlation coefficient at Pasadena at night increases from 0.38 to 0.62) and reductions in underprediction during the photochemically active afternoon period (e.g. bias at Pasadena from -5.62 to -2.42). Daily-averaged predictions of observations at routine monitoring networks show modest improvement during winter with mean biases reducing from 1.14 to 0.73, but less change in the summer where the contributions from nonvolatile POA were replaced by pcSOA and by SOA from the oxidation of volatilized particle emissions.

## Affected Files:
aero/aero6/SOA_DEFN.F
emis/emis/EMIS_DEFN.F
emis/emis/PT3D_DEFN.F
emis/emis/PTMAP.F
aero/aero6/AERO_DATA.F
aero/aero6/AERO_EMIS.F
aero/aero6/SOA_DEFN.F
aero/aero6/aero_subs.F
hadv/yamo/advbc_map.F
hadv/yamo/rdbcon.F
init/yamo/load_cgrid.F
MECHS/cb05e51_ae6_aq/mech_cb05e51_ae6_aq.def
MECHS/cb05e51_ae6nvPOA_aq/mech_cb05e51_ae6nvPOA_aq.def
MECHS/cb05eh51_ae6_aq/mech_cb05eh51_ae6_aq.def
MECHS/cb05mp51_ae6_aq/mech_cb05mp51_ae6_aq.def
MECHS/cb05tucl_ae6_aq/AE_cb05tucl_ae6_aq.csv
MECHS/cb05tucl_ae6_aq/GC_cb05tucl_ae6_aq.csv
MECHS/cb05tucl_ae6_aq/NR_cb05tucl_ae6_aq.csv
MECHS/cb05tucl_ae6_aq/mech_cb05tucl_ae6_aq.def
MECHS/cb05tump_ae6_aq/AE_cb05tump_ae6_aq.csv
MECHS/cb05tump_ae6_aq/GC_cb05tump_ae6_aq.csv
MECHS/cb05tump_ae6_aq/NR_cb05tump_ae6_aq.csv
MECHS/racm2_ae6_aq/AE_racm2_ae6_aq.csv
MECHS/racm2_ae6_aq/GC_racm2_ae6_aq.csv
MECHS/racm2_ae6_aq/NR_racm2_ae6_aq.csv
MECHS/racm2_ae6_aq/mech_racm2_ae6_aq.def
MECHS/saprc07tb_ae6_aq/AE_saprc07tb_ae6_aq.csv
MECHS/saprc07tb_ae6_aq/GC_saprc07tb_ae6_aq.csv
MECHS/saprc07tb_ae6_aq/NR_saprc07tb_ae6_aq.csv
MECHS/saprc07tb_ae6_aq/mech_saprc07tb_ae6_aq.def
MECHS/saprc07tc_ae6_aq/AE_saprc07tc_ae6_aq.csv
MECHS/saprc07tc_ae6_aq/GC_saprc07tc_ae6_aq.csv
MECHS/saprc07tc_ae6_aq/NR_saprc07tc_ae6_aq.csv
MECHS/saprc07tc_ae6_aq/mech_saprc07tc_ae6_aq.def
MECHS/saprc07tc_ae6nvPOA_aq/AE_saprc07tc_ae6_aq.csv
MECHS/saprc07tc_ae6nvPOA_aq/GC_saprc07tc_ae6_aq.csv
MECHS/saprc07tc_ae6nvPOA_aq/NR_saprc07tc_ae6_aq.csv
MECHS/saprc07tc_ae6nvPOA_aq/mech_saprc07tc_ae6nvPOA_aq.def
MECHS/cb05e51_ae6_aq/AE_cb05e51_ae6_aq.nml
MECHS/cb05e51_ae6_aq/GC_cb05e51_ae6_aq.nml
MECHS/cb05e51_ae6_aq/NR_cb05e51_ae6_aq.nml
MECHS/cb05e51_ae6_aq/RXNS_DATA_MODULE.F90
MECHS/cb05e51_ae6_aq/RXNS_FUNC_MODULE.F90
MECHS/cb05e51_ae6_aq/Species_Table_TR_0.nml
MECHS/cb05e51_ae6_aq/mech_CB05e51.def
MECHS/cb05e51_ae6nvPOA_aq/AE_cb05e51_ae6nvPOA_aq.nml
MECHS/cb05e51_ae6nvPOA_aq/CSQY_DATA_cb05e51_ae6nvPOA_aq
MECHS/cb05e51_ae6nvPOA_aq/GC_cb05e51_ae6nvPOA_aq.nml
MECHS/cb05e51_ae6nvPOA_aq/NR_cb05e51_ae6nvPOA_aq.nml
MECHS/cb05e51_ae6nvPOA_aq/RXNS_DATA_MODULE.F90
MECHS/cb05e51_ae6nvPOA_aq/RXNS_FUNC_MODULE.F90
MECHS/cb05e51_ae6nvPOA_aq/mech_CB05e51.def
MECHS/cb05eh51_ae6_aq/AE_cb05eh51_ae6_aq.nml
MECHS/cb05eh51_ae6_aq/GC_cb05eh51_ae6_aq.nml
MECHS/cb05eh51_ae6_aq/NR_cb05eh51_ae6_aq.nml
MECHS/cb05eh51_ae6_aq/RXNS_DATA_MODULE.F90
MECHS/cb05eh51_ae6_aq/RXNS_FUNC_MODULE.F90
MECHS/cb05eh51_ae6_aq/Species_Table_TR_0.nml
MECHS/cb05eh51_ae6_aq/mech_CB05eh51.def
MECHS/cb05mp51_ae6_aq/AE_cb05mp51_ae6_aq.nml
MECHS/cb05mp51_ae6_aq/NR_cb05mp51_ae6_aq.nml
MECHS/cb05tucl_ae6_aq/AE_cb05tucl_ae6_aq.nml
MECHS/cb05tucl_ae6_aq/GC_cb05tucl_ae6_aq.nml
MECHS/cb05tucl_ae6_aq/NR_cb05tucl_ae6_aq.nml
MECHS/cb05tucl_ae6_aq/RXNS_DATA_MODULE.F90
MECHS/cb05tucl_ae6_aq/RXNS_FUNC_MODULE.F90
MECHS/cb05tucl_ae6_aq/mech.def
MECHS/cb05tump_ae6_aq/AE_cb05tump_ae6_aq.nml
emis/emis/EMIS_DEFN.F
emis/emis/PT3D_DEFN.F


## References:

Murphy, B. N., Woody, M. C., Jimenez, J. L., Carlton, A. M. G., Hayes, P. L., Liu, S., Ng, N. L., Russell, L. M., Setyan, A., Xu, L., Young, J., Zaveri, R. A., Zhang, Q., and Pye, H. O. T.: Semivolatile POA and parameterized total combustion SOA in CMAQv5.2: impacts on source strength and partitioning, Atmos. Chem. Phys. Discuss., https://doi.org/10.5194/acp-2017-193, in review, 2017.

-----
## Internal Records:

### Relevant Pull Requests:
  [PR #18](https://github.com/usepa/cmaq_dev/pull/18)  
  [PR #20](https://github.com/usepa/cmaq_dev/pull/20)  
  [PR #106](https://github.com/usepa/cmaq_dev/pull/106)  
  [PR #116](https://github.com/usepa/cmaq_dev/pull/116)  
  [PR #121](https://github.com/usepa/cmaq_dev/pull/121)  
  [PR #123](https://github.com/usepa/cmaq_dev/pull/123)  
  [PR #124](https://github.com/usepa/cmaq_dev/pull/124)  

### Commit IDs:
ce70e37fe1d18628bc2cb0fb969dd5da27e5de90
0413e7c1b13906bdd12723f491f034464dceb8be
09b5d28d16fc6818d8db4df3d1898976c70412bd
1885d663e772e4edd55f2fd751a1f566afe82366
a70301a7be279038c77c5ec105616830205688c3
6f813545f83dc7d8f762fd2a9668389c8c9e790d
871db709d68421622cdf90ee38bbbcfd1271e788
5530bd74c2e6f5bfe48379c2d822f25a9f157dc3
6d79eb4e7d13c4efdf4ee4f8cf9f206ecaf6ffa4
55b4eebd19ddad7568cf9e11b417311834c7128b
2dd8b961e982fac66ede6d959f7407c3eb614496
a31466339c526ec8b9e116b16460f5de49d62f33
bd9efdf0759383bb8a019db359066965c0f41979
dcefb566d2f647da3ff65e9685db378d71407d1d
88e4847ac347e30a2f4f712f36074c6bb3d8ead1
0cefba5bf728183a34e8b5c77c87c28bb48b11f2
378b80788c340a78af6f89b749ef4741c4f86d22
46eceb32e54e96bac7d00a73fbff1f80a0a656fd
cad1621b2daeeb8a96e9d6bc88668619e8e2c7d3
6d389f9d6b64a091779b20dc2dd8e29f76ac13d8
26d37e0bda05dc4cab216c1adf78998675f0f36b
257e42a95b18b666d268162b75a95377926759fb
620e899e621970d5f90c15cc2597e27ea8faf9b1
67156293c8080d16e06efbabc2324c1df4a0c35b
213c98619ca2deb4ced982af45b9ad460f371046
504f03a4b390f6cfa6eee57128f5fdd901bb053d
ff7db26ce797732839546f3143a67c86b180f910
f38aa6ac3a470640a7828c443b6ea043012291f3
e513b8e1d0231ec4905f21fdffbea1c38b4e2f35
d7ea0f5a0dc0b5696d3832ff6f16390793e89776
eb1ee03b50a402600f494b33b001423b38f70814
6c63d549926aca4fe1eb3f704b34e0694271de4d
8e13cd6c43139a03e519eaa95b170cc7d58f10d9
8d24ed1cdd13ee0b9a6e5701432abc9091fc75b5
f411cc033a530d405dba96ebc55cc66fb0b9140c
5b1ab14d2722ae8083757a91c55cfada991c0f12
21650cb00df702da34e15a67246f3a372b39a9b1
33a76fcaa3545823a781b39cda97d2091a438e06
5945eb1a6230bba2f5e181fd088c7aed95f49f10
626243d43e84dbb960e4b89a2466120cf510307a
9b7d91026fd892e0e62bb608d5f389b598a90cde
4d0f556451261b3817254ae81d524b956860622b
3abec6fd7d280c5b7320497b9cce308fefcedab8
565c6b036a4643f24ca18f830538c071438a3685
b92b83c46dec367f31ea5cb10b40bd77332de634
8c1994256c76326da6ff93b4457c659ec0616ca9
fe40fea3ac47e8aaceecae2464b1919cda91530a
2ed961cb4087f45fc48ba89fad781ad60fbe53af
c5c71e62cb63b23af3952219e1fa42f9c2a0ed5e
1cc15085717eeb9a2234fbebe6b98af411836b74
13407a40dfc187225adcca45be075447f9ecfa21
216a2b0a712d42f8a86c09995e5d5f4af48e0d8d
7dc54b2a19317e5e95c7b24f4fe0b35cadb17ff9
