# Sulfur Tracking Method

**Author/P.O.C.:** [Shawn Roselle](mailto:roselle.shawn@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

CMAQv5.3 includes a runtime diagnostic model option that provides detailed information on the modeled sulfur budget. This option, referred to as the "Sulfur Tracking Method (STM)", tracks sulfate production from gas- and aqueous-phase chemical reactions, as well as contributions from emissions and initial and boundary conditions. Each tracked species is treated as other modeled species, undergoing transport (advection, diffusion, cloud-mixing) and removal by deposition (both wet and dry).  Several notable features in the CMAQv5.3 release of STM include:

- The STM is now a runtime option enabled by an environment variable.
- In addition to the standard sulfate tracking species (Table 1), new species have been included to track the loss of inorganic sulfate to organosulfate for chemical mechanisms that include this loss pathway (Table 2).

Table 1. Sulfur Tracking Species

|Species Group|Species Name| MW   | Description |
|:------------|:-----------|:-----|:------------|
|AE           |ASO4AQH2O2J | 96.0 |Accumulation mode sulfate (ASO4J) produced by aqueous-phase hydrogen peroxide oxidation reaction:  H<sub>2</sub>O<sub>2</sub> + S(IV) -> S(VI) + H<sub>2</sub>O |
|AE           |ASO4AQO3J   | 96.0 |ASO4J produced by aqueous-phase ozone oxidation reaction:  O<sub>3</sub> + S(IV) -> S(VI) + O<sub>2</sub> |
|AE           |ASO4AQFEMNJ | 96.0 |ASO4J produced by aqueous-phase oxygen catalyzed by Fe<sup>3+</sup> and Mn<sup>2+</sup> oxidation reaction: O<sub>2</sub> + S(IV) -> S(VI) |
|AE           |ASO4AQMHPJ  | 96.0 |ASO4J produced by aqueous-phase methyl hydrogen peroxide oxidation reaction:  MHP + S(IV) -> S(VI) |
|AE           |ASO4AQPAAJ  | 96.0 |ASO4J produced by aqueous-phase peroxyacetic acid oxidation reaction:  PAA + S(IV) -> S(VI) |
|AE           |ASO4GASJ    | 96.0 |ASO4J condensation following gas-phase reaction:  OH + SO<sub>2</sub> -> SULF + HO<sub>2</sub> |
|AE           |ASO4EMISJ   | 96.0 |ASO4J from source emissions |
|AE           |ASO4ICBCJ   | 96.0 |ASO4J from boundary and initial conditions |
|AE           |ASO4GASI    | 96.0 |Aitken mode sulfate (ASO4I) nucleation and/or condensation following gas-phase reaction:  OH + SO<sub>2</sub> -> SULF + HO<sub>2</sub> |
|AE           |ASO4EMISI   | 96.0 |ASO4I from source emissions |
|AE           |ASO4ICBCI   | 96.0 |ASO4I from boundary and initial conditions |
|AE           |ASO4GASK    | 96.0 |Coarse mode sulfate (ASO4K) condensation following gas-phase reaction:  OH + SO<sub>2</sub> -> SULF + HO<sub>2</sub>  |
|AE           |ASO4EMISK   | 96.0 |ASO4K from source emissions |
|AE           |ASO4ICBCK   | 96.0 |ASO4K from boundary and initial conditions |
|NR           |SULF_ICBC   | 98.0 |Sulfuric acid vapor (SULF) from boundary and initial conditions |

Table 2.  Additional Tracking Species Representing Loss of Inorganic Sulfate to Organosulfate (only included if using SAPRC07TIC_AE6I, SAPRC07TIC_AE7I, CB6R3_AE7, or CB6R3M_AE7 mechanisms).

|Species Group|Species Name| MW   | Description |
|:------------|:-----------|:-----|:------------|
|AE           |OSO4J       | 96.0 |Loss of ASO4J to organosulfate |
|AE           |OSO4AQH2O2J | 96.0 |Loss of ASO4AQH2O2J to organosulfate |
|AE           |OSO4AQO3J   | 96.0 |Loss of ASO4AQO3J to organosulfate |
|AE           |OSO4AQFEMNJ | 96.0 |Loss of ASO4AQFEMNJ to organosulfate |
|AE           |OSO4AQMHPJ  | 96.0 |Loss of ASO4AQMHPJ to organosulfate |
|AE           |OSO4AQPAAJ  | 96.0 |Loss of ASO4AQPAAJ to organosulfate |
|AE           |OSO4GASJ    | 96.0 |Loss of ASO4GASJ to organosulfate |
|AE           |OSO4EMISJ   | 96.0 |Loss of ASO4EMISJ to organosulfate |
|AE           |OSO4ICBCJ   | 96.0 |Loss of ASO4ICBCJ to organosulfate |


**Impact on runtime**:  Expect a small increase in runtime with STM turned on (<7.5%, see Table 3).

Table 3. Runtime comparison: 1-Day Simulation

| Code version/branch |SE52BENCH Wall Time (sec)|12US1 Wall Time (sec)|108NHEMI1 Wall Time (sec)|
|:-------------|:--------------:|:-:|:-:|
| STM off | 434.42 |2674.40|692.07|
| STM on  | 466.83 |2851.62|717.39|
|||||
|**Application Info**||||
|Mechanism|cb6r3_ae7_aq|cb6r3_ae7_aq|cb6r3_ae7_aq|
|# Grid Cells (ROW x COL x LAY)|280,000|4,803,435|1,538,636|
|NROWS|80|299|187|
|NCOLS|100|459|187|
|NLAYS|35|35|44|
|# PEs|32|128|128|


## Affected Files

#### Files modified:

CCTM/scripts/bldit_cctm.csh  
CCTM/scripts/run_cctm_2010_4CALIF1.csh  
CCTM/scripts/run_cctm_2011_12US1.csh
CCTM/scripts/run_cctm_2014_12US1.csh
CCTM/scripts/run_cctm_2015_HEMI.csh
CCTM/scripts/run_cctm_2016_12US1.csh
CCTM/scripts/run_cctm_Bench_2011_12SE1.csh
CCTM/scripts/run_cctm_Bench_2016_12SE1.csh
CCTM/src/aero/aero6/AERO_DATA.F
CCTM/src/cloud/acm_ae6/AQ_DATA.F
CCTM/src/cloud/acm_ae6/aqchem.F
CCTM/src/cloud/acm_ae6/convcld_acm.F
CCTM/src/cloud/acm_ae6/rescld.F
CCTM/src/driver/wrf/sciproc.F
CCTM/src/emis/emis/EMIS_VARS.F
CCTM/src/init/yamo/load_cgrid.F
CCTM/src/spcs/cgrid_spcs_nml/CGRID_SPCS.F
CCTM/src/util/util/RUNTIME_VARS.F

#### Files added:

CCTM/src/stm/STM_MODULE.F
CCTM/src/stm/STM_VARS.F

## References

NA           

-----

## Internal Records:

#### Relevant Pull Requests:

[PR #518](https://github.com/USEPA/CMAQ_Dev/pull/518)

#### Commit IDs:

[096e3279220e8af6d58762c2266b9753690dfca2](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/096e3279220e8af6d58762c2266b9753690dfca2)
[9cde9edd40144aa5414a779026e1e8bc8668097d](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/9cde9edd40144aa5414a779026e1e8bc8668097d)
[bbbc0a32173419b6827de54c32af0d49ba50b26f](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/bbbc0a32173419b6827de54c32af0d49ba50b26f)
[09a279dc9f8dd7ca72cd62956626144d0e2edc6d](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/09a279dc9f8dd7ca72cd62956626144d0e2edc6d)
[86f145cee7a448c45d110fd0829eadc58c8f2b73](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/86f145cee7a448c45d110fd0829eadc58c8f2b73)
[d7b6957d19155ae387167a59f153d3b129db3015](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/d7b6957d19155ae387167a59f153d3b129db3015)
[ebc7bceb41d9fc8a649d78af2facca7f91a2ba69](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/ebc7bceb41d9fc8a649d78af2facca7f91a2ba69)
[a6fef3fabb5657d50bc03c1986b0a9d187389723](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/a6fef3fabb5657d50bc03c1986b0a9d187389723)
[28e825d72d3569bf44a442e73d18159000a05be6](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/28e825d72d3569bf44a442e73d18159000a05be6)
[bd0425664ed15a8ffd8a56fb1bf4d1ef34c6ff1e](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/bd0425664ed15a8ffd8a56fb1bf4d1ef34c6ff1e)
[fa564455b03443b5488e431d7b64bcaa7703b0e3](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/fa564455b03443b5488e431d7b64bcaa7703b0e3)
[e48709d549f84bee3334cfeb9480cfbe23db1a59](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/e48709d549f84bee3334cfeb9480cfbe23db1a59)
[aac7573de93a22d0b8de56108ad895b3a2ac5444](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/aac7573de93a22d0b8de56108ad895b3a2ac5444)
[48e00a595e2af96112bef656050654517e51b0bb](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/48e00a595e2af96112bef656050654517e51b0bb)
[e0ef6134c483d80c1982dc6ca181ecd9d33a5f8b](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/e0ef6134c483d80c1982dc6ca181ecd9d33a5f8b)
[a45f600b8ff5889cc843369dbf3e7377763fa61f](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/a45f600b8ff5889cc843369dbf3e7377763fa61f)
[5bd1f9b6517ac8eb5188c135cb2f500918f3518b](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/5bd1f9b6517ac8eb5188c135cb2f500918f3518b)
[dec285073f6b130b686e36e73725e605d764fb6d](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/dec285073f6b130b686e36e73725e605d764fb6d)
[bf1c42c1e21e6368ff905c025947444680a97a1c](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/bf1c42c1e21e6368ff905c025947444680a97a1c)
[a6fdb42ab4197bf9556abb8d037b05f3be864c0f](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/a6fdb42ab4197bf9556abb8d037b05f3be864c0f)
[47bf00099b2dce9e92d2e3f1e46d195c6c52b0fb](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/47bf00099b2dce9e92d2e3f1e46d195c6c52b0fb)
[a824917145d418907fffb5b3d18cf0443c2f97c8](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/a824917145d418907fffb5b3d18cf0443c2f97c8)
[6f42e1efc6ec83a60372db9112089ad3a648f28a](https://github.com/USEPA/CMAQ_Dev/pull/518/commits/6f42e1efc6ec83a60372db9112089ad3a648f28a)



-----
