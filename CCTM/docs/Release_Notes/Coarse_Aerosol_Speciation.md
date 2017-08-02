# Updated Speciation for Coarse-Mode Aerosol 

**Author/P.O.C.:**, [Chris Nolte](mailto:nolte.chris@epa.gov), U.S. EPA

## Brief Description 
  Constant values for speciation factors, which used to occur throughout various routines (e.g., VOLINORG, AERO_EMIS, AQCHEM) have been placed into the AERO_DATA module for easy reference. Also, the speciation factor for Mg has been changed from an implicit value of 0.0 to 0.017, consistent with Upadhyay et al. 

  POA from dust sources is distributed among the fresh POA and oxidation products of POA in order to conserve both the total dust POA mass and its degree of oxygenation.

## Significance and Impact
  Small potential impact of ASOIL contribution to PM composition during large dust events. Little to no impact on long-term trends.

## Affected Files:
  aero/aero6/AERO_DATA.F
  aero/aero6/AERO_EMIS.F
  aero/aero6/aero_subs.F
  emis/emis//DUST_EMIS.F
  cloud/acm_ae6/aqchem.F

## References: 
  H. Simon et al., The development and uses of EPA's SPECIATE database, Atmos. Poll. Res., 1, 196-206, 2010.  
  N. Upadhyay et al., Size-differentiated chemical composition of re-suspended soil dust from the desert southwest United States, Aerosol Air Qual. Res., 15, 387-398, 2015, https://doi.org/10.4209/aaqr.2013.07.0253.
 
-----
## Internal Records:

### Relevant Pull Requests: 
  [PR #85](/usepa/cmaq_dev/pull/85)
  [PR #43](/useepa/cmaq_dev/pull/43)

### Commit IDs:
 85c228d4c282c00b3dc25c114eaa9b2746ccad09
