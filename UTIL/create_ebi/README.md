# CMAQ.Create_EBI_Solver
Data, runscript, and FORTRAN code that CMAQ v52 EBI Solver

To create a new EBI solver:

1) Edit scripts/bldit.create_ebi for your compiler and Mechanism. Save and run to build the software.

2) Edit scrips/run.create_ebi for your application. Mechanism needs to match what was selected in the build script.

3) Execute the script. Inputs and reference outputs are provided.

T report potential program errors or EBI solver failures, 
contact Bill Hutzell/USEPA at hutzell.bill@epa.gov
