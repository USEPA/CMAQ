
<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch11_ISAM.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch13_WRF-CMAQ.md)

<!-- END COMMENT -->

# 12. Sulfur Tracking Method
## 12.1 Introduction
Starting with CMAQv5.3, a runtime diagnostic model option that provides detailed information on the modeled sulfur budget. This option, referred to as the "Sulfur Tracking Method (STM)", tracks sulfate production from gas- and aqueous-phase chemical reactions, as well as contributions from emissions and initial and boundary conditions. Each tracked species is treated as other modeled species, undergoing transport (advection, diffusion, cloud-mixing) and removal by deposition (both wet and dry).  Several notable features in the CMAQv5.3 release of STM include:

- The STM is now a runtime option enabled by an environment variable.
- Additional species (Table 12-2) are included to track the loss of inorganic sulfate to organosulfate for chemical mechanisms that include this loss pathway.

## 12.2 Usage

To activate the STM option, edit the CCTM runscript and set the following environment variable to "Y" (the default is "N"):

- setenv STM_SO4TRACK Y

The STM option does not require any additional input files, and uses the initial conditions, boundary conditions, and emissions files available for with the standard, non-instrumented CCTM.

Next, run the CMAQ CTM following the instructions described in Chapter 5, section 5.7.

Note that several of the standard CMAQ output files (ACONC, CONC, CGRID, DDEP, and WDEP) will include additional species beyond the standard base model species list.  A list of the additional species output by the STM option are provided in Table 12-1 and Table 12-2.  These data can be post-processed using standard utilities, such as:

-   combine (to combine multiple days into one file or to aggregate various tracking species)
-   m3tproc (to sum/average over multiple days)
-   verdi (for data visualization)

<a id=Table12-1></a>

**Table 12-1. Sulfur Tracking Species List**

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

<a id=Table12-2></a>

**Table 12-2.  Additional Tracking Species Representing Loss of Inorganic Sulfate to Organosulfate (only included if using SAPRC07TIC_AE7I, CB6R3_AE7, CB6R5_AE7,CB6R5M_AE7, CRACMM1 or CRACMM1AMORE based mechanisms)**

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

**Contact**

 [William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. EPA
 
<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch11_ISAM.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch13_WRF-CMAQ.md) <br>
CMAQv5.5 User's Guide <br>

<!-- END COMMENT -->
