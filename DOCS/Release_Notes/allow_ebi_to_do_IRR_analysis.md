# Allow EBI solvers to conduct integrated reaction rate analysis

[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency

## Brief Description
The changes revise process analysis so that an EBI solver can conduct integrated reaction rate (IRR) analysis. The IRR calculation has been changed from single-precision ("real(4)") to double-precision ("real(8)") to align with the photochemical solvers. The *create_ebi* utility has also been changed so new EBI solvers will include this enhancement. 

## Signficance and Impact
Users can now conduct IRR analysis without changing the photochemical solver.

## Files Affected
All EBI solvers for all photochemical mechanisms under CCTM/src/gas.
