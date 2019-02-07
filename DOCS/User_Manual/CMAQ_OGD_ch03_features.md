
<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch02_overview.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch04_science.md)<br>

<!-- END COMMENT -->

# CMAQ Features #

Features of CMAQ for Application Users
-------------------------------------------------
** >>COMMENT<< **  This information does not need to be in a separate chapter.  Most of it is redundant with information found in Chapters 1 and 2.  The only new information seems to be links to release notes.

** >>COMMENT<< **  suggest deleting "independent but" in first bullet

** >>COMMENT<< ** If this information is retained somewhere, need to explain regulatory applications and SIPs.  Be generic for international user community.

** >>COMMENT<< ** bullet 2:  Delete.  All of the modularity/flexibility jargon drove the original design and is not necessarily applicable today.  We certainly do not optimize model performance for anyone.

** >>COMMENT<< ** bullet 5:  The community aspect is not necessarily true (and it never really was).  In principle, the community really relies on a single, centralized development group.
 
** >>COMMENT<< ** bullet 6:  The first sentence on training is misleading.  It implies that you can get online training from CMAS, which only appears to be true for SMOKE.  The vast majority of the training is in-residence for a fee.

** >>COMMENT<< ** bullet 6:  The link for "support resource" appears to be broken.

** >>COMMENT<< ** Add a bullet about the user forum for support and to exchange development ideas.


The CMAQ modeling system provides a variety of important features to users who are interested in applying the model for investigating scientific research questions or for regulatory applications such as preparation of State Implementation Plans (SIPs).

-   CMAQ is designed to address the complex interactions among multiple air quality issues simultaneously. Using a one-atmosphere approach to air quality modeling by applying multiscale and multipollutant modeling techniques, CMAQ can provide independent but dynamically consistent predictions of several different pollutants at varying spatial scales.
-   The modularity of the CMAQ design provides flexibility in air quality model configuration for optimizing model performance for different applications and spatial resolutions.
-   Close interactions among the development communities for CMAQ and for the meteorology and emissions models provide for a tight integration of the three main components of the air quality modeling system.
-   Serial and multiprocessor execution options allow the application user to optimize the performance of CMAQ on various computer platforms.
-   Community development expedites the expansion of CMAQ’s capabilities through the pursuit of multiple research agendas by a variety of research groups. Application users thus avoid the limitations inherent in having to rely on a single, centralized development group.
-   A comprehensive training program is available through the Community Modeling and Analysis System (CMAS) Center [website](http://www.cmascenter.org). The CMAS Center is a [support resource](CMAQ_OGD_ch13_support.md) for users of CMAQ and other modeling systems.
-   Members of the large, international community of users connected through the CMAS Center help each other by sharing data and experiences and providing technical support.

Features of CMAQ for Air Quality Model Developers
-------------------------------------------------
** >>COMMENT<< ** Remove opening clause "Designed under a community modeling paradigm", and delete "with a modular…"

** >>COMMENT<< **  Remove references to I/O API and netCDF.  This will get blurry really soon.

** >>COMMENT<< **  should rework the IO discussion to convey that the system employs flexible I/O and internal data structures that facilitate its modularity and extensibility

** >>COMMENT<< **  Actually, if this material is retained somewhere, consider shortening the opening material to simply say: "CMAQ is distributed as open-source software.  CMAQ provides the following features to scientists interested in developing new algorithms or adding science to the model:"

** >>COMMENT<< **  Combine bullets 1 and 2, and delete bullet 3.

Designed under a community-modeling paradigm, CMAQ is distributed as open-source software engineered with a modular code design to facilitate decentralized development. Built around a layered [I/O API](https://www.cmascenter.org/ioapi) and [netCDF](http://www.unidata.ucar.edu/software/netcdf) code framework, CMAQ provides a flexible platform for testing new science algorithms, chemistry representations, and optimization techniques. CMAQ provides the following features to scientists interested in developing new algorithms or adding science to the model:

-   All CMAQ source code is available through [GitHub](https://github.com/USEPA/CMAQ).
-   Developed and distributed following open-source software conventions, CMAQ source code is easily accessible and free to obtain.
-   Designed for modularity, CCTM uses standardized input/output (I/O) routines to facilitate extensibility.
-   The diverse and continually growing community of CMAQ developers provides an excellent forum for discussing development-related topics of all kinds.

New Features in CMAQ and MCIP
--------------------------------
** >>COMMENT<< ** Delete the first sentence; that's pretty much obvious.

** >>COMMENT<< ** 2nd sentence needs to be reworded (grammatically incorrect)

** >>COMMENT<< ** No need to include hyperlinks to each version of CMAQ and MCIP; those should be easy to find from GitHub.


Each release version of CMAQ contains new features and improvements over the previous release of the model. Details of the new features in each release are available as release notes is provided below. Technical details about these features are contained in the [CMAQ Wiki](https://www.airqualitymodeling.org/index.php/CMAQ) for versions 5.1 and earlier.  Beginning with version 5.2, release notes are available on the [U.S. EPA GitHub repository](https://github.com/USEPA/CMAQ). The following links provide details about the new features added to CMAQ since version 5.0.

**New Features by CMAQ version**

[Version 5.2.1](https://github.com/USEPA/CMAQ/blob/5.2.1/CCTM/docs/Release_Notes/README.md)
[Version 5.2](https://github.com/USEPA/CMAQ/blob/5.2/CCTM/docs/Release_Notes/README.md)  
[Version 5.1](https://www.airqualitymodeling.org/index.php/CMAQ_version_5.1_November_2015_release_Technical_Documentation)  
[Version 5.0.2](https://www.airqualitymodeling.org/index.php/CMAQ_version_5.0.2_April_2014_release_Technical_Documentation)  
[Version 5.0.1](https://www.airqualitymodeling.org/index.php/CMAQ_version_5.0.1_July_2012_release_Technical_Documentation)  
[Version 5.0](https://www.airqualitymodeling.org/index.php/CMAQ_version_5.0_February_2012_release_Technical_Documentation)  

**New Features by MCIP version**

[Version 4.3](https://www.airqualitymodeling.org/index.php/MCIP_version_4.3_Release_Notes)  
[Version 4.2](https://www.airqualitymodeling.org/index.php/MCIP_version_4.2_Release_Notes)  
[Version 4.1](https://www.airqualitymodeling.org/index.php/MCIP_version_4.1_Release_Notes)  

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_OGD_ch02_overview.md) - [Home](README.md) - [Next Chapter >>](CMAQ_OGD_ch04_science.md)<br> 
CMAQ Operational Guidance Document (c) 2016<br>

<!-- END COMMENT -->
