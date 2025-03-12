# Getting started with CRACMM

Author: Havala Pye (pye.havala@epa.gov) | v5.4

Update: Havala Pye (pye.havala@epa.gov) | v5.5

CRACMM was first introduced in CMAQv5.4 as two versions: CRACMM1 and CRACMM1AMORE. Those versions are retained in v5.5 and an updated version, CRACMM2, is released for the first time. CRACMM2 merges the CRACMM and CRACMM-AMORE lines of development into one path going forward. Thus, CRACMM2 is an update to both CRACMM1 and CRACMM1AMORE. CRACMM has a separate supporting repository at https://www.github.com/USEPA/CRACMM which contains additional information to facilitate use of CRACMM in CMAQ. This tutorial provides an overview of how to get starting running CRACMM in CMAQ.

## How is running CRACMM different than running other chemical mechanisms in CMAQ?

Running CRACMM is just like running other mechanisms in CMAQ. No matter what mechanism you choose, CMAQ will require several inputs that are mechanism specific (including the gas and aerosol species lists, initial conditions, boundary conditions, and emissions) as well as other inputs that are mechanism independent (meteorology, land use files, grid description file). Some of these inputs are distributed with CMAQ while others are generated from separate tools. See [Chapter 4 of the CMAQ Users' Guide](../CMAQ_UG_ch04_model_inputs.md) for a complete list of CMAQ inputs.

## How do I select CRACMM?

Once you have obtained CMAQv5.5 and set up your project directory, CRACMM2 can be selected in the CCTM build script (CCTM/scripts/bldit_cctm.csh) by indicating "CRACMM2" as the Mechanism. The "_aq" extension is no longer part of the mechanism name starting with CRACMM2. The selection of the corresponding aerosol module, cloud module, and default gas-phase chemical solver will occur automatically in the build script based on the mechanism name. 

Currently, CRACMM cannot be built with the gfortran compiler version 9.1 and later since CMAQ-CRACMM has trailing comments in the gas, aerosol, and non-reactive species namelists. If you want to pursue that compiler now, delete the comments (starting with "!" ) in the GC, AE, and NR namelists. Look for an upcoming bug fix on the v5.5+ branch that will address this issue.

## What deposition and biogenic emission modules work with CRACMM? 

CRACMM is compatible and has been tested with both STAGE and M3DRY deposition modules. CRACMM is compatible with both BEIS and MEGAN in-line biogenic emissions.

## How do I prepare emission inputs for CRACMM?

CRACMM1 and CRACMM1AMORE use the same emissions since the two versions differ only in the representation of isoprene chemistry. 
CRACMM2 emissions are slightly different than CRACMM1 as ethylbenzene and styrene are now explicit. CRACMM2 emissions may be used in CRACMM1/CRACMM1AMORE (and vice versa) using approximations (see the work of Skipper et al. and https://github.com/USEPA/CRACMM/tree/main/emissions).
CRACMM includes a set of rules for how individual organic species map to the mechanism species. 
These rules are distributed in python code in the supporting repository. 
In addition, the repository contains the mapping of individual organic species from the SPECIATE database, BEIS, and MEGAN to CRACMM species.
CMAQ-ready emissions are generally prepared using the SMOKE model. 

## How do I obtain initial and boundary conditions?
 
Initial and boundary conditions can be mapped from other mechanisms including Carbon Bond. The CMAQ repository bcon PREP tools (available in the PREP/bcon/map2mech folder) contain mappings from Carbon Bond to CRACMM. Initial conditions may be obtained in a similar manner. Because Carbon Bond species are not a good match to CRACMM species when emissions are fresh, the initial conditions should be followed by sufficient spin-up time to remove their influence. See the work by [Hogrefe et al.](
https://doi.org/10.1016/j.atmosenv.2017.04.009) for the impact of initial conditions on CMAQ predictions as a function of spin-up time. The larger the domain (e.g., hemisphere vs a country) and the more remote the study area (e.g., free troposphere vs surface), the longer the spin-up time will need to be. Adequate spin-up time can always be checked by adding more spin-up days and verifying the model predictions have not significantly changed.

## What does CRACMM assume about the volatility of primary organic aerosol and how to I prepare those emissions?

The majority of speciation profiles in SPECIATE assume POA is nonvolatile. A few volatility resolved profiles are in SPECIATE starting with v5.1, and work is underway to build volatility information for all relevant sources into future versions of SPECIATE.  

[S2S-Tool](https://github.com/USEPA/S2S-Tool) workflows in place for CRACMM supplement the POA information in SPECIATE by implementing semivolatile POA profiles in the emission files. 
Prior to S2S-Tool, workflows in place for CRACMM in v5.4 implemented semivoltile POA in the DESID control files by adding together PMOCN2 and PMNOCMN2 on the emission files to create a 
total POA equivalent that is then distributed to different volatility species in the model based on the expected volatility of POA emissions. Your specific configuration will depend on when and how your emissions were prepared.

For emissions to be properly ingested by CMAQ, three files need to be customized and synchronized for your simulation: 
* The run script. This is where the model is told what files to read in and each file (stream) is given a string label.
* The DESID mechanism-specific namelist. This is where species on the emission files are mapped to model species. For older workflows, PMOCN2 and PMNCOMN2 can be mapped to species of different volatility.
* The DESID mechanism-independent control file. This is where emission files can be grouped so that multiple sources can be treated the same (e.g., wildland fires, prescribed burning, and residential wood burning can have the same volatility profile).

**The main CMAQ log file and the log file for at least one processor should be checked for any initial run.** Warnings about potential emission problems will be displayed in those logs. More guidance on using DESID is available in a [CMAQ emissions tutorial](CMAQ_UG_tutorial_emissions.md).

CRACMM mechanisms currently allow traditional nonvolatile 'APOC' and 'ANCOM' as legacy species in the model. The species can be transported and removed but do not undergo any heterogeneous or other chemistry. This species will be removed in a future version. To use the model species 'APOC' and 'ANCOM', the DESID control file would need to include their mappings.

If your emission preparation workflow is custom, you can create semivolatle POA species directly in your inputs. CRACMM contains low volatility through semivolatile organic species with either alkane-like or oxygenated functionality that can represent semivolatile POA emissions. You can use the CRACMM python emission mapper to map emissions to representative CRACMM species. In CMAQ, check that the DESID files include your species names.

Future releases of CRACMM, CMAQ, and emission tools should be checked for cross compatibility.

## Additional resources

[SPECIATE Database](https://www.epa.gov/air-emissions-modeling/speciate): A database of PM<sub>2.5</sub> and VOC emissions composition by individual species.

[S2S-Tool](https://github.com/USEPA/S2S-Tool): A tool to convert SPECIATE profiles with individual species to profiles of mechanism species for use in SMOKE. Replaces Speciation Tool.

[SMOKE](https://github.com/CEMPD/SMOKE/): A tool that combines emission magnitudes with speciation profiles as well as temporal and spatial proxies to create CMAQ-ready emission files.

[CRAMM Repository](https://github.com/USEPA/CRACMM/): A repository containing additional information on how emissions map to CRACMM as well as metadata for CRACMM.
