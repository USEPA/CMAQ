# Updates to AQCHEM-KMT(I)

**Author/P.O.C.:**, [Kathleen Fahey](mailto:fahey.kathleen@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

AQCHEM-KMT and AQCHEM-KMT(I) have been updated in the following ways to maintain or improve consistency across CMAQ:  
1. Use the HLCONST function to calculate temperature-adjusted Henry's law coefficients (as opposed to using hard-coded values).  This allows any updates to Henry's law coefficients to automatically propagate through to the KMT mass transfer coefficient calculations in AQCHEM-KMT(I).  
2. Use constants in the AERO_DATA module to calculate coarse cation concentrations following J. Young updates in AQCHEM.F (as opposed to using hard-coded values).  
3. Update the IEPOX+SO4 rate coefficient in AQCHEM-KMT(I), to be consistent with aero6i updates for aerosol chemistry.  
4. Update the HO(aqueous) concentration calculation to be more consistent with standard AQCHEM. Now initial gas phase HO represents the "total" (gas+aqueous) HO available for the system (rather than just the gas phase concentration). This should have only a minor impact on results, as HO is not very soluble.  
5. Minor updates to array sizes and inclusion of simple Aitken/wet scavenging treatment processing of Hg/toxic tracer species to accommodate changes made to AQ_DATA.F and aq_map.F.  


## Significance and Impact

Impacts to model results (compared to standard AQCHEM) and run-time are expected to be minor.

## Affected Files:

cloud/acm_ae6_kmt/aqchem_Global.F90 <br>
cloud/acm_ae6_kmt/aqchem_Initialize.F90 <br>
cloud/acm_ae6_kmt/aqchem_kmt.F90 <br>
cloud/acm_ae6i_kmti/aqchem_Global.F90 <br>
cloud/acm_ae6i_kmti/aqchem_Initialize.F90 <br>
cloud/acm_ae6i_kmti/aqchem_Rates.F90 <br>
cloud/acm_ae6i_kmti/aqchem_kmt.F90 <br>
cloud/acm_ae6i_kmti/AQ_DATA.F <br>


## References:

NA

-----
## Internal Records:

### Relevant Pull Requests:
[PR #81](https://github.com/USEPA/CMAQ_Dev/pull/81)
[PR #110](https://github.com/USEPA/CMAQ_Dev/pull/110)


### Commit IDs:
20c41468c99fbf34de8a6a19a0ec7ca64c1ee210 <br>
b84b1d652595e88d88778e230dbd9f64be3667f9 <br>
fe430d5be61557149f4116aae8d6f911364f9bc9 <br>
00d838d0a7d9f074416da005f84bfc0c17099f03 <br>
ebf4ab6543fca00807bca7e59a0a0c0ed7c19264 <br>
e26621efcbadf984e87de60a84e4d9dc6b627e46 <br>
5ab28f0c1843e0f7f75778c0ae3555cce9b3c222 <br>
