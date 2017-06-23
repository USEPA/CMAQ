# Subgrid Shallow Convective Cloud Improvement

**Author/P.O.C.:**, [Jesse Bash](mailto:bash.jesse@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description
CMAQ overestimated the sub-grid shallow convective clouds over the ocean. This change introduced a scale dependent lower and upper RH threshold for shallow convective clouds over land and water. The parameterization was largely taken from WRF 3.7 and follows Sundqvist et al. 1989 using the formulation of Mocko and Cotton 1995. This resulted in changing the humidity threshold in convcld_acm.F and introducing a scale dependency on the humidity thresholds. 

## Significance and Impact

The cloud reflectivity estimated by CMAQ now more closely match those estimated by WRF and measured by GOES. The change in the cloud cover can both increase surface ozone due to increased photolysis or decrease due to a reduction in mixing from aloft. Generally, the changes resulted in a slight reduction (~1ppb) in estimated O3 concentrations along the Gulf Coast. 

## Affected Files:

CCTM/src/cloud/acm_ae6/convcld_acm.F
CCTM/src/cloud/acm_ae6_mp/convcld_acm.F

## References:

Sundqvist, H., Berge, E., Kristjansson, J.E.: Condensation and Cloud Parameterization Studies with a Mesoscale Numerical Weather Prediction Model, Mon. Wea. Rev., 117, 1641-1657

Mocko, D.M., Cotton, W.R.: Evaluation of Fractional Cloudiness Parameterizations for Use in a Mesoscale Model, J. Atmos. Sci., 52(16), 2884-2901

-----
## Internal Records:

### Relevant Pull Requests:
  [PR #33](https://github.com/usepa/cmaq_dev/pull/33)

### Commit IDs:
992729db506091be3ce80f5086d909e0ea15ae9f  
3dc45f1e9b2e9b35454ad51eb218e420fc57b701  
62e4165b45ef933f29b34d061e0a545c8cb8632e  
60647d3b104b09e2e0afa47f53fd7bb5083aa82a    
