# Resolve issues with Generalized Solvers when compiling with Intel17

**Author/P.O.C.:**, [William T. Hutzell](mailto:hutzell.bill@epa.gov), Computational Exposure Division, U.S. EPA  

## Brief Description

The Rosenbrock solver fails to converge or crashes when using the -O3 flag with ifort version 17 and possibly version 15 because of a segmentation error. The pull request's removes the problem by altering how temporary arrays are allocated and initialized in the RBSPARSE subroutine. Similar changes were made to the smvgear solver because it has a similar subroutine called GRSPRSE.

## Significance and Impact

When building the CMAQ model, a user can choose to use the Rosenbrock (ros3) or Sparse Matrix Vectorized Gear (smvgear) solver for the photochemistry. The option allows using Integrated Rate Analysis in process analysis, _a run-time option,_ because the EBI solvers do not support it. The build option also support developing photochemical mechanisms where ros3 or smvgear solver is often used instead of an EBI solver.

## Affected Files:

CCTM/src/gas/ros3/rbsparse.F  
CCTM/src/gas/smvgear/grsprse.F  

## References:    

-----
## Internal Records:


### Relevant Pull Requests:
  [PR #239](https://github.com/USEPA/CMAQ_Dev/pull/239)  

### Commit IDs:

6d1a66474e130ec5343d6abd1bf23ebceb3bef34  

