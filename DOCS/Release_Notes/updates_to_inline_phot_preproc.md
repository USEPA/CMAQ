# Updates to the *inline_phot_preproc* utility 

[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency

## Brief Description

The *inline_phot_preproc* utility has been changed as follows.
 
1.  Allow the *bldrun* script to set the number of wavelength bands used by the two files. To support the change, the wavelength spectra have been supplemented with the input files for cross-section and quantum yield of several photolysis rates. The supplement can cause minor changes because it changes the endpoints for interpolating the data inputs. See the updated *bldrun* script for recommended settings for the number of wavelengths.

2.  Update the cross-section and quantum yield data for ClNO<sub>2</sub> based on the 2013 IUPAC recommendations. The change supports updates to the *CB6r3* mechanism.

3.  Allow the utility to process a flexible number and name for aerosol refractive indices written to the PHOT_OPTICS.dat output file. **The CCTM file, CSQY_DATA.F, has been changed so the PHOT_OPTICS.dat file is no longer restricted to five aerosol refractive indices.**

4. Added input files for new refractive indices that can used instead of the five used in previous versions of CMAQ. The new data include refractive indices compiled by the Adient Project, as well as values for ice. The *bldrun* script still specifies refractive indices used by previous versions of CMAQ. 

