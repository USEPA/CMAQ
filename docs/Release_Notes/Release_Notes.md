CMAQv5.2 Release Notes - October 2016
=====================================

Refer to the CMAQv5.0 [[CMAQ_version_5.0_(February_2012_release)_Technical_Documentation | Technical Documentation ]] for information on the technical features of this version.<br>


The Community Multiscale Air Quality (CMAQ) Model version 5.1 is major update to CMAQ that includes several changes to the science algorithms in the base model.  CMAQ v5.1 was developed by the U.S. EPA with contributions from other research partners. CMAQv5.1 builds upon the beta version made available to the modeling community in April 2015 by the U.S. EPA.  Summarized below are the main enhancements to the modeling system since the previous release, CMAQ v5.1. 

# Base Documentation
    *[https:/github.com/usepa/cmaq/blob/CMAQv5.1 Readme file|Building and running CMAQv5.1]]
    *[[CMAQv5.1_Two-way_model_release_notes|Building and running WRF-CMAQ Two Way Model]]

# Patches 
    ## Patch 1 

# Release Notes

    ## Chemistry 
    ### Photochemistry 

    ### Aerosol Chemistry 

    ### Aqueous and Heterogeneous Chemistry]]
    #### Cloud/Fog chemistry

    #### Aqueous aerosol chemistry


    ## Transport Processes 
      
    ### Pleim-Xiu Land Surface Model (PX LSM)

      
      ### Asymmectric Convective Model version 2 (ACM2)

      ### Monin-Obukhov Length Consistency Fix


      ### Gravitational Settling of Coarse Aerosols

      '''References'''

      Pleim, J., A. A. R. Gilliam, W. Appel, and L. Ran, 2015: Recent Advances in Modeling of the Atmospheric Boundary Layer and Land Surface in the Coupled WRF-CMAQ Model, 34th International Technical Meeting on Air Pollution Modelling and its Application, 4-8 May, 2015, Montpellier, France.

    ## Air-Surface Exchange 

    Dry deposition and vertical diffusion in CMAQ v5.1 were restructured to reduce the complexity and redundancy of the code and simplify maintaining and updating the processes contained in this code. In addition, updates have been made in four areas of the model that affect air-surface exchange.

    ### Sea Spray Aerosols

      Modifications have been made to the emissions and aerosol size distribution of sea salt.

    ### Biogenic Emissions (BEIS)

      The biogenic emissions module was updated to include the addition of a two-layer canopy model, updated light response function, new leaf temperature algorithm, and updates to the Biogenic Emissions Landcover Dataset (BELD) and vegetation species emission factors.

    ### Windblown Dust Emissions


    ### Dry Deposition


    ## VOC Emission Updates

      Updates to the CB05, RACM2, and SAPRC07 chemical mechanisms require the addition of the following emission species:
      *Carbon Bond 5 (CB05) Mechanism: SOAALK, NAPH, XYLMN
      *SAPRC07 Mechanism: SOAALK, ARO2MN, NAPH
      *RACM2 Mechanism: SOAALK, NAPH

      These new species can either be derived directly with an emission processing system such as SMOKE or estimated using factors if using older emission files.

    ## Structural Updates
      PARIO and STENEX libraries no longer require separate compilation as they are now included as part of the CCTM code.

    ## Tools & Utilities
      CMAQv5.1 includes optional utility programs to process and prepare data for model evaluation. These programs are located in the $M3MODEL/TOOLS archive.

    ## Two-way Coupled WRF-CMAQ
      In CMAQ v5.1, the two-way coupled option is available with WRF 3.7. In addition, changes were made to the calculation of water insoluble aerosols.

    ## Instrumented Models
      The instrumented versions of CMAQv5.2 (e.g. CMAQ-DDM) will be release at a later date.

    ## Community Contributions ==
      None

# Release Testing

      The CMAQv5.1 release package was tested with the Portland Group 15.7, Intel 16.0, and GNU Fortran 4.8.1 compilers.  In addition to different serial and parallel configurations, the release was tested under different science configurations. See the details of the [http://www.airqualitymodeling.org/cmaqwiki/index.php?title=CMAQv5.1_Readme_file#Testing_Procedures CMAQv5.1 Release Test Results], including run times for different compiler configurations.
