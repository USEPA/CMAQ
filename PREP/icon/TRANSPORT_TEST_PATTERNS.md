Transport/Diffusion Pattern Test Species
======

The ICON and BCON processors also contain the capability to generate ICs and BCs
for special pattern test species that can be used to investigate the accuracy of
the various transport and diffusion algorithms available in the CCTM system.
This capability has been included primarily to facilitate the evaluation of
alternative algorithms, and thus would not likely be used in most air quality
modeling applications.

ICs and BCs are generated for eight different patterns. Each one can be used to
examine different properties of transport algorithms. The defined names and a
brief description of each test pattern species follow. (Note that the ICs for
these species can be viewed with one of the visualization tools to examine the
patterns more closely.)

* **IC1_BC0**: All ICs in the modeling domain are set to 1.0 and all BCs are 
               set to 0.0.

* **IC1_BC1**: All ICs in the modeling domain are set to 1.0 and all BCs are 
               set to 1.0.

* **IC0_BC1**: All ICs in the modeling domain are set to 0.0 and all BCs are 
               set to 1.0.

* **STREET**: Within any horizontal cross-section of cells, IC cell 
              concentrations are set to either 1.0 or 0.0 such that the 
              overall domain pattern resembles a street grid. All BCs are 
              set to zero.

* **CHKBRD**: Within any horizontal cross-section of cells, IC cell 
              concentrations are set to either 1.0 or 0.0 such that the 
              overall domain pattern resembles a checkerboard. All BCs are 
              set to zero.

* **SPOS_A**: ICs and BCs are set such that a concentration mound is centered 
              at cell (10,10) in the modeling domain, and the concentration 
              profile is defined below.

* **SPOS_B**: ICs and BCs are set such that a concentration mound is centered 
              at cell (10,10) in the modeling domain, and the concentration 
              profile is defined below.

* **SPOS_C**: ICs and BCs are set such that a concentration mound is centered 
              at cell (10,10) in the modeling domain, and the concentration 
              profile is defined below.


*h<sub>s</sub> = a<sup>2</sup> / ( a<sup>2</sup> + ( x - x<sub>r</sub> ) )<sup>2</sup> + ( y - y<sub>r</sub> )<sup>2</sup> )*

The last three tracer species are designed for superposition tests of 
transport algorithms. The base shape of the concentration mounds is described 
by the Witch of Agnesi surface: where *a* is the radius of the mound, *x<sub>r</sub>*
and *y<sub>r</sub>* define the position of the peak of the mound, and *h<sub>s</sub>* is the height 
of the mound. In the ICON and BCON processors, *a* is set to 3 and *(x<sub>r</sub>, y<sub>r</sub>)* 
to (10,10). The three tracer signals *h<sub>A</sub>*, *h<sub>B</sub>*, and *h<sub>C</sub>* for SPOS_A, SPOS_B, 
and SPOS_C are then defined as follows:

*h<sub>A</sub> = q<sub>max</sub> ( 1 + h<sub>s</sub> ) + q<sub>min</sub>*

*h<sub>B</sub> = 2 q<sub>max</sub> ( 1 + h<sub>s</sub> ) - q<sub>min</sub>*

*h<sub>C</sub> = - q<sub>max</sub> ( 1 + h<sub>s</sub> ) + 2 q<sub>min</sub>*

where *q<sub>max</sub>* and *q<sub>min</sub>* determine the amplitude and background values of the 
signals, and both are set to 50 in the ICON and BCON processor. With these 
signals, the addition of mounds B and C will yield mound A. Thus, the 
degree to which *h<sub>A</sub> - ( h<sub>B</sub> + h<sub>C</sub> )* differs from zero in an advection test 
provides a measure of the nonlinearity of the advection algorithm.
