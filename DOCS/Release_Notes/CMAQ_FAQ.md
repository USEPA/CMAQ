# Frequently Asked Questions for Upgrading to the Latest CMAQ Version

## What do I need to do to update to v5.3?
* A major change to CMAQ in version 5.3 is the incorporation of the new emissions module, DESID, and 
its control via a number of RunScript variables and the Emission Control File, which is explained in 
the [CMAQ User’s Guide section 6.9.3](../Users_Guide/CMAQ_UG_ch06_model_configuration_options.md).
* If you are upgrading to CMAQv5.3 but you are not changing mechanisms (i.e. you have been using cb6r3_ae6 and you 
will continue to do so), it is hoped that you will not need to do anything special to maintain default behavior. 
In other words, the default emission control files will handle simple assignment of CMAQ species to emissions 
surrogates (e.g. NO --> NO; ASO4 --> PSO4, etc.).
* However, it is good practice to review the new CMAQ logfiles labeled "CTM_LOG_[XXX]…" to confirm that your 
emissions are being read as you expect. In particular pay attention to the sections beginning:
  * "SCALING EMISSIONS CONSISTENT WITH EMISSIONS CONTROL FILE SUPPLIED BY USER"
  * "Checking for unused Emissions Surrogates"
  * "EMISSIONS SCALING DIAGNOSTIC"
  
	Note that the warnings and notices for potential emissions issues have improved in clarity in v5.3.1. 
* If you are upgrading to a new mechanism (i.e. cb6r3_ae6 --> cb6r3_ae7) then it is even more imperative 
that you review the log file, tutorials for how to control the emissions, and the Emissions Control File 
itself to make sure the correct assignments are being made. Alpha-pinene is of particular concern and you 
may read more about that in the [CMAQ User’s Guide section 6.9.3](../Users_Guide/CMAQ_UG_ch06_model_configuration_options.md).
* If you have custom-mapped any CMAQ species to an emission surrogate species, then DESID will help you 
perform this action in a more transparent way. Note that the previous method of mapping emissions (in CMAQv5.2 and before) 
via the chemical species namelists has been removed. Please see the User's Guide documentation (User’s Guide section 6.9.3) and 
the [DESID tutorial for the Emission Control File](../Users_Guide/Tutorials/CMAQ_UG_tutorial_emissions.md) to learn how to implement your changes from now on. 
* If you are running CMAQ with the M3DRY m3dry option for dry deposition and ammonia bidirectional surface flux (bidi) you will need 
to prepare EPIC soil properties (L2C_SOIL) and  EPIC crop types and fertilizer application 
(E2C_CHEM) input files using the latest version of FEST-C, version 1.4. See Chapter 6 for further information. Running CMAQ with the 
STAGE m3dry option and ammonia bidirectional surface flux is compatible with previous versions of FEST-C.  
Please see the User's Guide [Chapter 6](../Users_Guide/CMAQ_UG_ch06_model_configuration_options.md) for further information on 
M3DRY and STAGE and [Chapter 4](CMAQ_UG_ch04_model_inputs.md) to read more about the land surface input files required for running with ammonia bidi. 


## What differences should I expect in my model results with v5.3 compared to v5.2.1?

## What differences should I expect in my model results with v5.3.1 compared to v5.3?

## Additional FAQ
A more general list of Frequent CMAQ Questions can be found on our website: https://www.epa.gov/cmaq/frequent-cmaq-questions

## Technical support for CMAQ
Technical support for CMAQ, including questions about model inputs, downloading, compiling, and running the model, 
and pre- and post-processing utilities, should be directed to the [CMAS Center User Forum](https://forum.cmascenter.org/). 
