# Update the Lightning NO Vertical Profile

[Daiwen Kang](mailto:kang.daiwen@epa.gov), U.S. Environmental Protection Agency

## Brief Description
The lightning NO (LNO) emission vertical distribution algorithm (Kang et al., 2019) is composed of two Gaussian normal distributions centered (mean)
at two pressure levels with different standard deviation. The first distribution has mean of 350 hPa and standard deviation
of 200 hpa and the second distribution has the mean of 600 hPa and a standard deviation of 50 hPa.

At each model layer, the weight (W) is the combination of the PDFs from each of the distribution. To distribute the column
LNO corrected, two conditions apply: (1) the sum of the weights (W) across all the layers should be 1, and (2) the shape of the vertical profile
should be similar to those reported in literature (e.g., Allen et al., 2012).
 
The existing vertical profile in earlier CMAQ versions (up to version 5.3) has factor of 1 from the first distribution and a factor
of 0.2 from the second one. The resulting vertical weight profile imputes unproportionally larger share at lower model layers (around 4 km). 
To satisfy the two conditions, after careful calculation, two scale factors are applied to the weight function for the two distributions,
respectively: F1=0.95 and F2=0.12. The resulted vertical weight profile resembles those reported in Allen et al. (2012). 

This update would change model results when lightning NO is used.

## Affected File
CCTM/src/emis/emis/LTNG_DEFN.F

## References:
* Kang, D., Pickering, K. E., Allen, D. J., Foley, K. M., Wong, D., Mathur, R., and Roselle, S. J.: Simulating Lightning NOx Production
  in CMAQv5.2: Evolution of Scientific Updates, Geosci. Model Dev., 12, 3071-3083, doi:10.5194/gmd-12-3071, 2019.
* Allen, D. J., Pickering, K. E., Pinder, R. W., Henderson, B. H., Appel, K. W., and Prados, A.: Impact of lightning-NO on eastern 
  United States photochemistry during the summer of 2006 as determined using the CMAQ model, Atmos. Chem. Phys., 12, 1737–1758, https://doi.org/10.5194/acp-12-1737-2012, 2012.


## Internal Records
#### Relevant Pull Requests:
[PR #565](https://github.com/usepa/cmaq_dev/pull/565)

#### Commit IDs:
89ba32e6dadd61f9f3e78151e98cbc0e3fc64d26

