SAPRC-22 Updates
==================

The latest SAPRC mechanism, referred to as SAPRC-22, has been implemented in the [Community Multiscale Air Quality (CMAQ)](http://www.epa.gov/cmaq) modeling system as a community contribution.

A report describing the implementation of the mechanism and its testing is provided here [SAPRC-22 Implementation Report](SAPRC22_DraftReport_Deliverable_3.4.25.pdf). Users wishing to use CMAQ with the SAPRC-22 mechanism are recommended to read the report. In particular, Section 4 of the report provides guidance on speciating total organic gas emissions. The Appendix section provides a listing of the mechanism and associated species.

To build CMAQ with the SAPRC-22 mechanism, make the following changes to the bldit script:

#Specify mechanism
setenv Mechanism saprc22_ae65_aq              #> chemical mechanism (see $CMAQ_MODEL/CCTM/src/MECHS)

#Include saprc-22 in mechanism list
set MechList = "cb6r5hap_ae7_aq, cb6r3_ae7_aq, cb6r5_ae7_aq, cb6r5_ae7_aqkmt2, cb6r5m_ae7_aq, racm2_ae6_aq, saprc07tc_ae6_aq, saprc07tic_a
e7i_aq, saprc07tic_ae7i_aqkmt2, saprc22_ae65_aq"


