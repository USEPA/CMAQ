CMAQv5.2 Release Notes
======================


Refer to the CMAQv5.0 [[CMAQ_version_5.0_(February_2012_release)_Technical_Documentation | Technical Documentation ]] for information on the technical features of this version.<br>

= Release Notes for CMAQv5.1 - November 2015  =

The Community Multiscale Air Quality (CMAQ) Model version 5.1 is major update to CMAQ that includes several changes to the science algorithms in the base model.  CMAQ v5.1 was developed by the U.S. EPA with contributions from other research partners. CMAQv5.1 builds upon the beta version made available to the modeling community in April 2015 by the U.S. EPA.  Summarized below are the main enhancements to the modeling system since the previous release, CMAQ v5.0.2. 

Build instructions, how to run a model test case, and details of the changes included in CMAQv5.1 are provided below.

== January 2016 Patch ==

The CMAQv5.1 inline dust model has a tendency to occasionally produce unrealistically high emissions and concentrations. This patch disables the inline windblown dust model as the default option in the CCTM.  The default had previously been to enable the dust model.

The inline dust option can still be turned on by setting this line in the CCTM run script:

  setenv CTM_WB_DUST   Y

  If running a US modeling domain and using BELD3 land cover data, add the following environment variable to the CCTM run script:

    setenv CTM_WBDUST_BELD BELD3 

    We are in the process of developing and evaluating a new dust model that will be released in 2016.

    = Base Documentation  =
    *[[CMAQv5.1 Readme file|Building and running CMAQv5.1]]
    *[[CMAQv5.1_Two-way_model_release_notes|Building and running WRF-CMAQ Two Way Model]]

    = Summary of CMAQ v5.1 Release =

    == Chemistry ==
    === Photochemistry  ===
    *There are nine (9) chemical mechanisms being released with CMAQv5.1, which are described in more detail in [[CMAQv5.1 Mechanisms|CMAQ v5.1 Chemical mechanism updates]].  All of the gas phase mechanisms have been updated to maintain compatibility with changes to the heterogeneous chemistry in other areas of CMAQ, including:
    **ClNO<sub>2</sub> and first order ozone depletion parameterized from marine halogen chemistry. [[CMAQv5.1 ClNO2 chemistry|Details of ClNO<sub>2</sub> updates]]
    **[[CMAQv5.1 Halogen chemistry|Additional Halogen chemistry updates]]. 
    **[[CMAQv5.1 Integration of gas and heterogeneous chemistry|Integration of heterogeneous and homogeneous chemistry]], including oligomerization reactions, POA aging, HONO production from NO<sub>2</sub>, and heterogeneous N<sub>2</sub>O<sub>5</sub> reaction to produce HNO3

    *RACM2 was modified to include:
    **New secondary organic aerosol (SOA) sources from isoprene, alkanes, and polyaromatic hydrocarbons (PAHs), with the PAH assumed to be 2% of the model species XYL emissions

    *Extensive updates to the other mechanisms (SAPRC07 and CB05e51), including:
    **New SOA sources from isoprene, alkanes, and PAHs
    **Updates to photochemical cross sections and quantum yields (CB05e51)
    **Updates to important inorganic and organic rates and products to ensure consistency with IUPAC.
    **Correction of minor errors in rate constants where appropriate
    **Additional representation of NOy species, physical and chemical properties (CB05e51)

    *Major update to the implementation of photochemical mechanisms in CMAQ:
    **The mechanism definition files have additional reaction rates to characterize the heterogeneous reaction rates that were previously defined within the AEROSOL_CHEMISTRY.F and other files. This change allows a more transparent display of the chemistry, allows easier modification, and more accurate solution.
    **Data representing chemical reactions are now expressed in two new Fortran files: RXNS_DATA_MODULE.F90 and RXNS_FUNC_MODULE.F90. 
    ***RXNS_DATA_MODULE.F90 replaces the deprecated CMAQ include files, RXDT.EXT and RXCM.EXT.
    ***RXNS_FUNC_MODULE.F90 contains routines that map the mechanism species to CGRID species and that calculate rate constants. It replaces routines in all chemistry solvers that calculated rate constants.
    **The solver files using the Euler Backward Iterative (EBI) method for each mechanism have been rederived.
    **New utilities for generating the mechanism *.F90 files and EBI solver codes are distributed with CMAQ v5.1

    === [[CMAQv5.1 In-line Calculation of Photolysis Rates|Photolysis Rates]]  ===
    The in-line calculation of photolysis rates has undergone significant changes in following areas:
    *The description of clouds has changed. In CMAQ v5.0.2, a vertical column had a single cloud deck with a constant cloud fraction and water droplet mixing ratio. In CMAQ v5.1, a column can have multiple cloud decks with variable cloud fractions and multiple types of water condensates. The new description is more consistent with the meteorological model used to support CMAQ simulations.
    *For scattering and extinction from aerosols, aerosol species now have refractive indices that depend on wavelength. Mostly importantly, elemental or black carbon has new indices that are based on current scientific consensus. These new indices increase the absorptive capacity of the simulated black carbon. Changes in aerosol scattering and extinction also introduce run-time options for how to calculate their optical properties. These changes allow a user to choose the aerosol mixing model and method to solve Mie scattering theory. 
    *New photolysis diagnostic output files. Several variables have been added that describe optical properties of aerosol and clouds and their radiative effects.
    *Fortran code changes. The photolysis rate calculation algorithms have been segregated into files based on the physical process or parameter represented by the code. This change attempts to make the algorithms and their interactions easier to understand. The Fortran code has also changed to improve computational efficiency.

    === Aerosol Chemistry ===
    CMAQ v5.1 provides two new options for aerosol chemistry: AERO6 and AERO6i. The CMAQ v5.1 AERO6 and AERO6i mechanisms are updated from the CMAQ v5.0.2 AERO6 mechanism in terms of the SOA and ISOROPIA algorithms. Details of the CMAQ v5.1 aerosol chemistry updates, include:
    *SOA Updates: [[CMAQv5.1 SOA Update| AERO6]] and [[CMAQ_v5.1_SAPRC07tic_AE6i| AERO6i]]
    *[[CMAQv5.1 Isorropia|ISORROPIA updates]] 
    *[[CMAQv5.1 Aerosol size dist| Binary nucleation and PM2.5 emissions size distribution updates]]
    *[[CMAQv5.1 Gravitational settling | Gravitational settling of coarse aerosols from upper layers]]
    *AERO5 has been deprecated and is no longer available.  
    *The other inorganic aerosol species (PMOther) retains the [http://www.airqualitymodeling.org/cmaqwiki/index.php?title=CMAQv5.0_PMother_speciation treatment used in CMAQ v5.0]

    Aerosols treatments retained from previous CMAQ versions:
    *[http://www.airqualitymodeling.org/cmaqwiki/index.php?title=CMAQv5.0_PMother_speciation  PMother]
    *[http://www.airqualitymodeling.org/cmaqwiki/index.php?title=CMAQv5.0_PM_emitted_species_list Emitted PM species]

    === [[CMAQv5.1 Aqueous Chemistry| Aqueous and Heterogeneous Chemistry]]  ===
    ====Cloud/Fog chemistry====
    An optional aqueous phase chemistry treatment (AQCHEM-KMT) is now available using the Rodas3 Rosenbrock solver to solve cloud chemistry, kinetic mass transfer, ionic dissociation, and wet deposition.  Kinetic PreProcessor (KPP) version 2.2.3 was used to generate the solver code.  There are two aqueous chemical mechanisms considered:
    *standard AQCHEM chemistry with 5 sulfur oxidation reactions and two SOA forming reactions from glyoxal and methylglyoxal
    *standard AQCHEM chemistry + SOA formation from IEPOX/MPAN species in cloud water.

    ====Aqueous aerosol chemistry====
    The CMAQv5.1 photochemical mechanisms (cb05e51, saprc07t*, etc) include aerosol formation from IEPOX due to uptake and acid-catalyzed reactions on aqueous particles. For saprc07tic with aero6i chemistry, uptake of MPAN-derived products, glyoxal, and methylglyoxal occurs on particles leading to additional SOA.

    == Transport Processes  ==

    There have been major revisions to the Pleim-Xiu Land Surface Model (PX-LSM) and the Assymetric Convective Model version 2 Planetary Boundary Layer (ACM2 PBL) model in WRF version 3.7 and later (Pleim et al, 2015).  To maintain consistency with these revisions in WRF, the ACM2 scheme in CMAQ was also revised.
      
      ===Pleim-Xiu Land Surface Model (PX LSM)=== 

      There were two changes to the PX-LSM in WRF.  The original stomatal conductance function for photosynthetically active radiation (PAR) was revised such that the new function has significantly lower magnitude when surface short wave radiation values are less than 350 W m-2. This change results in reduced latent heat flux and enhanced sensible heat flux, causing a delay in surface stabilization during the evening transition.  This change has the effect of reducing overpredictions in water vapor mixing ratios which are common during the evening transition.  Similarly, overpreditions of surface emitted chemical species (e.g. NOx, CO, EC) during evening transitions are also reduced.  The second change is an updated (reduced) value for the heat capacity of vegetation, which has the effect of reducing overpredictions of minimum 2-m temperature occurring around dawn and underpredictions during the post-dawn morning hours.

      ===Asymmectric Convective Model version 2 (ACM2)=== 

      Two major revisions have been made to ACM2.  For the WRF application, the ACM2 now estimates and applies different eddy diffusivities for momentum (Km) and heat (Kh) which means that Prandtl number is no longer assumed to be unity (Pr = Km/Kh ≠ 1).  The other major modification to ACM2 is that new stability functions have been developed for both heat and momentum for stable conditions.  This change is intended to allow more mixing under more stable atmospheric conditions, particularly moderately stable conditions such as occur in the early evening.  CMAQv5.1 has also been modified to include the same stability functions that are used in WRFv3.7.  Therefore, '''for best consistency WRFv3.7 and CMAQv5.1 should be used together'''.

      ===Monin-Obukhov Length Consistency Fix===  

      We recently found that values of the Monin-Obukhov length (MOL) used in the ACM2 model in CMAQ differed from the MOL values used in the ACM2 model in WRF.  The reason was that the output from WRF was for a preliminary estimate of MOL that was computed in the surface layer module in WRF (module_sf_pxsfclay.F).  The MOL was later re-computed in ACM2 but not loaded into the output array.  This inconsistency has been fixed in CMAQv5.1 by re-computing MOL in CMAQ exactly as it is computed in ACM2 in WRF.  Note that this is an interim fix because in the next release of WRFv3.8 the final version of MOL re-computed in ACM2 will be in the output.  Also, the estimate of MOL in the surface layer model will be improved such that there will be very little difference between the initial MOL estimate and the final re-calculation.

      ===[[CMAQv5.1 Gravitational settling | Gravitational Settling of Coarse Aerosols]]=== 

      We have added the effects of gravitational settling of coarse aerosols from upper to lower layers
      to include this important process and more realistically simulate aerosol mass distribution.

      '''References'''

      Pleim, J., A. A. R. Gilliam, W. Appel, and L. Ran, 2015: Recent Advances in Modeling of the Atmospheric Boundary Layer and Land Surface in the Coupled WRF-CMAQ Model, 34th International Technical Meeting on Air Pollution Modelling and its Application, 4-8 May, 2015, Montpellier, France.

      == Air-Surface Exchange ==

      Dry deposition and vertical diffusion in CMAQ v5.1 were restructured to reduce the complexity and redundancy of the code and simplify maintaining and updating the processes contained in this code. In addition, updates have been made in four areas of the model that affect air-surface exchange.

      ===[[CMAQv5.1 Sea Spray Aerosol Update|Sea Spray Aerosols]]=== 

      Modifications have been made to the emissions and aerosol size distribution of sea salt.

      ===[[CMAQv5.1 Biogenic Emissions (BEIS) Update|Biogenic Emissions (BEIS)]]=== 

      The biogenic emissions module was updated to include the addition of a two-layer canopy model, updated light response function, new leaf temperature algorithm, and updates to the Biogenic Emissions Landcover Dataset (BELD) and vegetation species emission factors.

      ===[[CMAQv5.1 Windblown Dust Emissions Updates|Windblown Dust Emissions]]=== 

      We have added a land use module to expand options beyond BELD3, e.g. for hemispheric applications, and we have modified some of the parameterizations to be consistent with the literature. Testing over the continental U.S. shows sporadic cases where the windblown dust generates excessive PM concentrations. We are working on new parameterizations based on more recent research to address this issue and more accurately simulate the effects of windblown dust.

      ===[[CMAQv5.1 Dry Deposition Updates|Dry Deposition]]=== 

      Previous versions of CMAQ contained a very limited treatment for organic nitrogen (N) compounds.  In combination with changes to the chemical mechanisms, additional surrogate species have been added to account for dry deposition of organic N species. The deposition velocity of ozone over oceans has been adjusted to account for the enhanced chemical sink due to reactions with iodide in the seawater.  The cuticular resistance used in determining the ozone deposition velocity for vegetated surafces has been modified.

      ==[[CMAQv5.1 VOC Emission Updates|VOC Emission Updates]]==  

      Updates to the CB05, RACM2, and SAPRC07 chemical mechanisms require the addition of the following emission species:
      *Carbon Bond 5 (CB05) Mechanism: SOAALK, NAPH, XYLMN
      *SAPRC07 Mechanism: SOAALK, ARO2MN, NAPH
      *RACM2 Mechanism: SOAALK, NAPH

      These new species can either be derived directly with an emission processing system such as SMOKE or estimated using factors if using older emission files.

      == [[CMAQv5.1 Structure Updates|Structural Updates]]==
      PARIO and STENEX libraries no longer require separate compilation as they are now included as part of the CCTM code.

      == [[ CMAQv5.1 Tools and Utilities|Tools & Utilities]]==
      CMAQv5.1 includes optional utility programs to process and prepare data for model evaluation. These programs are located in the $M3MODEL/TOOLS archive.

      == [[CMAQv5.1 WRF-CMAQ Two-way Updates|Two-way Coupled WRF-CMAQ]]  ==
      In CMAQ v5.1, the two-way coupled option is available with WRF 3.7. In addition, changes were made to the calculation of water insoluble aerosols.

      == Instrumented Models ==
      The instrumented versions of CMAQv5.1 (e.g. CMAQ-DDM) will be release at a later date.

      == Community Contributions ==
      None

      = Release Testing =

      The CMAQv5.1 release package was tested with the Portland Group 15.7, Intel 16.0, and GNU Fortran 4.8.1 compilers.  In addition to different serial and parallel configurations, the release was tested under different science configurations. See the details of the [http://www.airqualitymodeling.org/cmaqwiki/index.php?title=CMAQv5.1_Readme_file#Testing_Procedures CMAQv5.1 Release Test Results], including run times for different compiler configurations.
