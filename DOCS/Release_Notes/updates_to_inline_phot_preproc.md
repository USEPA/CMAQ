# Updates to the inlinephot_preproc utility 

**Author/P.O.C.:**, [William T. Hutzell](mailto:hutzell.bill@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

This utility and inputs have been changed to accomplish the below tasks.
 
1.  Allow the bldrun script to set the number of wavelength bands used by the two files. To support the change, the wavelength spectra have been supplemented the input files for cross-section and quantum yield of several photolysis rates. The supplement can cause minor changes because it changes the end points for interpolating the data inputs. See the updated build run script for recommend settings for the number of wavelength.

2.  Update the cross-section and quantum yield data for CLNO2 based on the 2013 IUPAC recommendations. The change supports updates to the cb6r3 mechanism.

3.  Allow the utility to process a flexible number and name for aerosol refractive indices written to the PHOT_OPTICS.dat output file. **The CCTM file, CSQY_DATA.F, has been changed so the PHOT_OPTICS.dat file can have the number of aerosol refractive indices to be other than five.**

4. Added input files for new refractive indices that can used instead of the five used in previous versions of CMAQ. The new data include refractive indices compiled by the Adient Project as well as values for ice. The buildrun script still specifies refractive indices used by previous versions of CMAQ. 

