# In-line Photolysis CLOUD_OPTICS.F fixed for possible floating point error

**Author/P.O.C.:**, [William T. Hutzell](mailto:hutzell.bill@epa.gov), Computational Exposure Division, U.S. EPA


## Brief Description
The update adds limiting cases into the subroutine aggreg_size_effective to prevent division by zero. The zeroes were caused by
underflow errors in the exponential function. The limiting cases were derived from spreadsheet calculations.

## Significance and Impact

Test simulations showed max ozone differences of 2.0E<sup>-7</sup> ppm compared to the original code. The simulations spanned the CONUS12 domain for Febuary 1-2, 2011.    

## Affected Files:

phot/inline/CLOUD_OPTICS.F  
phot/inline/phot.F

## References:    

None

-----
## Internal Records:


### Relevant Pull Requests:
  [PR #14](https://github.com/USEPA/CMAQ_Dev/pull/14)

### Commit IDs:

353ea2341f353181c9cae16d8c92336201bb3e58  
0987030346b489ff8e220dcbb55ac50efa6eab2a  
6bb9c813fd521e68b3507c20b8c75f9cacc9ea60  
7a12f56783edfa0780d00fffe1c09325e0a9ae0b  
2ba4c7cd13d4226c24704f669ee7f066f9090bc4  
4445becffcd7bfe69c7c88da170f031edd4a5ecf  
