# Implemented a new windblown dust emission parametrization

**Author/P.O.C.:**, [Hosein Foroutan](mailto:foroutan.hosein@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description 

This update implements a new physics-based windblown dust emission scheme. A new dynamic relation for the surface roughness length is developed [Foroutan et al., 2016], and the friction velocity at the surface is calculated accordingly. The MODIS FPAR satelite observation is used to represent more realistic time-varying vegetation coverage in this model that affects both the friction velocity and the drag partitioning coefficient. Additionally, the threshold friction velocity for dust emission is updated to reflect the dependency on soil grain size following []. Finally, a physics-based sandblasting efficiency formulation [] is used in the present model that includes the effect of soil plasticity, texture, and density into account. 

## Significance and Impact

Detailed evaluations [] showed significant improvements over the previous scheme

## Affected Files:
emis/emis/DUST_EMIS.F  


## References: 

1. DeCarlo et al., Particle Morphology and Density Characterization by Combined Mobility and Aerodynamic Diameter Measurements. Part 1: Theory, Aerosol Sci. and Technology, 38:1185-1205, 2004
2. Ensberg et al., Inorganic and black carbin aerosols in the Los Angeles Basin during CalNex, Journ. Geophys. Res., 2013.
3. Jiang et al., 2004

-----
## Internal Records:

### Relevant Pull Requests: 
  [PR #22](/usepa/cmaq/pull/22)

### Commit IDs:
8f3bd301099c354350a3d152088c9f2fb961c720  
dfe134ae37240de92fc19292e2ce98306742b984  
dc9a180de6d3452db7852dab7007d1a2a38d8ad5  
77df3e978480eecdea4081599fee89a2a98c597f  
5db1fa3af2f49c9dc382f45593a7657fbb964abf  
e246de9d80a9076575a3d4a83071ed4edeca4a56  
879f22428b8932cd0113c12f42acead44a9bdf1c  


