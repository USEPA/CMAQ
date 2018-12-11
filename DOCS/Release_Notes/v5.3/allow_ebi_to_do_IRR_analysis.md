# Allow EBI solver to conduct Integrated Reaction Rate analysis

**Author/P.O.C.:**, [William T. Hutzell](mailto:hutzell.bill@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description
The changes revises procan/pa so an ebi solver can conduct IRR analysis. They also change the IRR calculation from real(4) to real(8) precision because the photochemical solvers use real(8) precision. Changes include updates to the create_ebi utility has been changed so a new ebi solver includes this enhancement. 

## Signficance and Impact
Users can conduct IRR analysis without having to change the photochemical solver used.

## Files Affected
All ebi solvers for all photochemical mechanisms under CCTM/src/gas.
