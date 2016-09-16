# Implemented a new windblown dust emission parametrization

**Author/P.O.C.:**, [Hosein Foroutan](mailto:foroutan.hosein@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description 

This update implements a new physics-based windblown dust emission scheme. A new dynamic relation for the surface roughness length is developed [Foroutan et al., 2016], and the friction velocity at the surface is calculated accordingly. The MODIS FPAR satelite observation is used to represent more realistic time-varying vegetation coverage in this model that affects both the friction velocity and the drag partitioning coefficient. Additionally, the threshold friction velocity for dust emission is updated to reflect the dependency on soil grain size following Shao and Lu [2000]. Finally, a physics-based sandblasting efficiency formulation [Lu and Shao, 1999] is used in the present model that includes the effect of soil plasticity, texture, and density into account. 

## Significance and Impact

Evaluations [Foroutan et al., 2016] showed significant improvements over the previous scheme in CMAQ.

## Affected Files:
emis/emis/DUST_EMIS.F  


## References: 

1. Foroutan et al. (2016) Development and evaluation of a physics-based windblown dust emission scheme implemented in the CMAQ modeling system, J Geophys Res, submitted.  
2. Lu H and Shao Y (1999) A new model for dust emission by saltation bombardment, J Geophys Res 104: 16827-16842.
3. Shao Y and Lu H (2000) A simple expression for wind erosion threshold friction velocity, J Geophys Res 105: 22437-22443.

-----
## Internal Records:

### Relevant Pull Requests: 
  [PR #26](/usepa/cmaq/pull/26)
  [PR #135](/usepa/cmaq/pull/135)

### Commit IDs:
?


