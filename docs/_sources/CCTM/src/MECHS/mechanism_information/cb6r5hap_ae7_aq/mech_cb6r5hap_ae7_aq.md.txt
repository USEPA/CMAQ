Information is based on the mech.def file.
* Fall-off or pressure dependent reaction rate constants (M equals air number density):
 * For rate constants with k<sub>o</sub>, k<sub>i</sub>, n, F values: k = [ k<sub>o</sub>M/(1+k<sub>o</sub>M/k<sub>i</sub>)]F<sup>G</sup>, where G=(1+(log<sub>10</sub>(k<sub>o</sub>M/k<sub>i</sub>)/n)<sup>2</sup>))<sup>-1</sup> 
 * For rate constants with k<sub>1</sub>, k<sub>2</sub>: k = k<sub>1</sub> + k<sub>2</sub>M
 * For rate constants with k<sub>0</sub>, k<sub>2</sub>, k<sub>3</sub>: k = k<sub>0</sub> + k<sub>3</sub>M/(1+k<sub>3</sub>M/k<sub>2</sub>)
 * For rate constants with k<sub>1</sub>, k<sub>2</sub>, k<sub>3</sub>: k = k<sub>1</sub> + k<sub>2</sub>M + k<sub>3</sub> 

* For rate constants with the form A<_Reference_>, k equals A times a reference that represents photolysis rate, a heteorogeneous rate constant, rate constant for the given reaction or an operator. A equals one if not given.

* In the mechanism definition file, the rate is formatted as
 * A~<_HETEOROGENEOUS_>
 * A*K<_REACTION_>
 * A/<_PHOTOLYSIS_>
 * A?<_OPERATOR_>

|Label|Reaction            |Rate Constant Formula| Value<br> molecules/(sec*cm<sup>3</sup>)|   
|:---|:-------------------|:--------------------|:----:|   
| R1   | NO2 ----> NO + O  | NO2_IUPAC10 | Not Available<sup>1</sup> | 
| R2   | O + O2 + M ----> O3  |   6.00E-34(T/300)<sup> -2.60</sup> |   6.0973E-34 |
| R3   | O3 + NO ----> NO2  |   2.07E-12e<sup> -1400.00/T</sup> |   1.8910E-14 |
| R4   | O + NO ----> NO2  | k<sub>o</sub>=  1.00E-31e<sup>     0.0/T</sup>(T/300)<sup> -1.60</sup><br>k<sub>i</sub> =   5.00E-11e<sup>     0.0/T</sup>(T/300)<sup>  0.30</sup><br>n=     0.84;F=     0.85 |   2.2577E-12 |
| R5   | O + NO2 ----> NO  |   5.10E-12e<sup>   198.00/T</sup> |   9.9079E-12 |
| R6   | O + NO2 ----> NO3  | k<sub>o</sub>=  1.30E-31e<sup>     0.0/T</sup>(T/300)<sup> -1.50</sup><br>k<sub>i</sub> =   2.30E-11e<sup>     0.0/T</sup>(T/300)<sup>  0.24</sup><br>n=     1.03;F=     0.60 |   2.0907E-12 |
| R7   | O + O3 ----> |   8.00E-12e<sup> -2060.00/T</sup> |   7.9879E-15 |
| R8   | O3 ----> O  | O3_O3P_IUPAC10 | Not Available<sup>1</sup> | 
| R9   | O3 ----> O1D  | O3_O1D_IUPAC10 | Not Available<sup>1</sup> | 
| R10   | O1D + M ----> O  |   2.23E-11e<sup>   115.00/T</sup> |   3.2796E-11 |
| R11   | O1D + H2O ---->   2.000\*OH  |   2.1400E-10 |   2.1400E-10 |
| R12   | O3 + OH ----> HO2  |   1.70E-12e<sup>  -940.00/T</sup> |   7.2647E-14 |
| R13   | O3 + HO2 ----> OH  |   2.03E-16e<sup>   693.00/T</sup>(T/300)<sup>  4.57 </sup> |   2.0168E-15 |
| R14   | OH + O ----> HO2  |   2.40E-11e<sup>   110.00/T</sup> |   3.4709E-11 |
| R15   | HO2 + O ----> OH  |   3.00E-11e<sup>   200.00/T</sup> |   5.8674E-11 |
| R16   | OH + OH ----> O  |   6.20E-14e<sup>   945.00/T</sup>(T/300)<sup>  2.60 </sup> |   1.4519E-12 |
| R17   | OH + OH ----> H2O2  | k<sub>o</sub>=  9.00E-31e<sup>     0.0/T</sup>(T/300)<sup> -3.20</sup><br>k<sub>i</sub> =   3.90E-11e<sup>     0.0/T</sup>(T/300)<sup> -0.47</sup><br>n=     1.23;F=     0.42 |   6.2071E-12 |
| R18   | OH + HO2 ----> |   4.80E-11e<sup>   250.00/T</sup> |   1.1102E-10 |
| R19   | HO2 + HO2 ----> H2O2  | k<sub>0</sub>=  2.20E-13e<sup>   600.0/T</sup><br>k<sub>1</sub>=  1.90E-33e<sup>   980.0/T</sup> |   2.8975E-12 |
| R20   | HO2 + HO2 + H2O ----> H2O2  | k<sub>0</sub>=  3.08E-34e<sup>  2800.0/T</sup><br>k<sub>1</sub>=  2.66E-54e<sup>  3180.0/T</sup> |   6.4973E-30 |
| R21   | H2O2 ---->   2.000\*OH  | H2O2_IUPAC10 | Not Available<sup>1</sup> | 
| R22   | H2O2 + OH ----> HO2  |   1.8000E-12 |   1.8000E-12 |
| R23   | H2O2 + O ----> OH + HO2  |   1.40E-12e<sup> -2000.00/T</sup> |   1.7095E-15 |
| R24   | NO + NO + O2 ---->   2.000\*NO2  |   4.25E-39e<sup>   664.00/T</sup> |   3.9409E-38 |
| R25   | HO2 + NO ----> OH + NO2  |   3.45E-12e<sup>   270.00/T</sup> |   8.5332E-12 |
| R26   | NO2 + O3 ----> NO3  |   1.40E-13e<sup> -2470.00/T</sup> |   3.5339E-17 |
| R27   | NO3 ----> NO2 + O  | NO3NO2_06 | Not Available<sup>1</sup> | 
| R28   | NO3 ----> NO  | NO3NO_06 | Not Available<sup>1</sup> | 
| R29   | NO3 + NO ---->   2.000\*NO2  |   1.80E-11e<sup>   110.00/T</sup> |   2.6032E-11 |
| R30   | NO3 + NO2 ----> NO + NO2  |   4.50E-14e<sup> -1260.00/T</sup> |   6.5744E-16 |
| R31   | NO3 + O ----> NO2  |   1.7000E-11 |   1.7000E-11 |
| R32   | NO3 + OH ----> HO2 + NO2  |   2.0000E-11 |   2.0000E-11 |
| R33   | NO3 + HO2 ----> OH + NO2  |   4.0000E-12 |   4.0000E-12 |
| R34   | NO3 + O3 ----> NO2  |   1.0000E-17 |   1.0000E-17 |
| R35   | NO3 + NO3 ---->   2.000\*NO2  |   8.50E-13e<sup> -2450.00/T</sup> |   2.2944E-16 |
| R36   | NO3 + NO2 ----> N2O5  | k<sub>o</sub>=  3.60E-30e<sup>     0.0/T</sup>(T/300)<sup> -4.10</sup><br>k<sub>i</sub> =   1.90E-12e<sup>     0.0/T</sup>(T/300)<sup>  0.20</sup><br>n=     1.33;F=     0.35 |   1.2406E-12 |
| R37   | N2O5 ----> NO3 + NO2  | k<sub>o</sub>=  1.30E-03e<sup>-11000.0/T</sup>(T/300)<sup> -3.50</sup><br>k<sub>i</sub> =   9.70E+14e<sup>-11080.0/T</sup>(T/300)<sup>  0.10</sup><br>n=     1.33;F=     0.35 |   4.5396E-02 |
| R38   | N2O5 ----> NO2 + NO3  | N2O5_IUPAC10 | Not Available<sup>1</sup> | 
| R39   | N2O5 + H2O ---->   2.000\*HNO3  |   1.0000E-22 |   1.0000E-22 |
| R40   | NO + OH ----> HONO  | k<sub>o</sub>=  7.40E-31e<sup>     0.0/T</sup>(T/300)<sup> -2.40</sup><br>k<sub>i</sub> =   3.30E-11e<sup>     0.0/T</sup>(T/300)<sup> -0.30</sup><br>n=     0.87;F=     0.81 |   9.7627E-12 |
| R41   | NO + NO2 + H2O ---->   2.000\*HONO  |   5.0000E-40 |   5.0000E-40 |
| R42   | HONO + HONO ----> NO + NO2  |   1.0000E-20 |   1.0000E-20 |
| R43   | HONO ----> NO + OH  | HONO_IUPAC10 | Not Available<sup>1</sup> | 
| R44   | HONO + OH ----> NO2  |   2.50E-12e<sup>   260.00/T</sup> |   5.9795E-12 |
| R45   | NO2 + OH ----> HNO3  | k<sub>o</sub>=  1.80E-30e<sup>     0.0/T</sup>(T/300)<sup> -3.00</sup><br>k<sub>i</sub> =   2.80E-11e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.00;F=     0.60 |   1.0589E-11 |
| R46   | HNO3 + OH ----> NO3  | k<sub>0</sub>=  2.40E-14e<sup>   460.0/T</sup><br>k<sub>1</sub>=  2.70E-17e<sup>  2199.0/T</sup><br>k<sub>3</sub>=  6.50E-34e<sup>  1335.0/T</sup> |   1.5409E-13 |
| R47   | HNO3 ----> OH + NO2  | HNO3_IUPAC10 | Not Available<sup>1</sup> | 
| R48   | HO2 + NO2 ----> PNA  | k<sub>o</sub>=  1.40E-31e<sup>     0.0/T</sup>(T/300)<sup> -3.10</sup><br>k<sub>i</sub> =   4.00E-12e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.26;F=     0.40 |   7.4949E-13 |
| R49   | PNA ----> HO2 + NO2  | k<sub>o</sub>=  4.10E-05e<sup>-10650.0/T</sup>(T/300)<sup>  0.00</sup><br>k<sub>i</sub> =   6.00E+15e<sup>-11170.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.26;F=     0.40 |   6.3131E-02 |
| R50   | PNA ---->   0.590\*HO2 +    0.590\*NO2 +    0.410\*OH +    0.410\*NO3  | PNA_IUPAC10 | Not Available<sup>1</sup> | 
| R51   | PNA + OH ----> NO2  |   3.20E-13e<sup>   690.00/T</sup> |   3.2376E-12 |
| R52   | SO2 + OH ----> SULF + HO2 + SULRXN  | k<sub>o</sub>=  2.80E-31e<sup>     0.0/T</sup>(T/300)<sup> -2.60</sup><br>k<sub>i</sub> =   2.00E-12e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.10;F=     0.53 |   9.3422E-13 |
| R53   | C2O3 + NO ----> NO2 + MEO2 + RO2  |   7.50E-12e<sup>   290.00/T</sup> |   1.9837E-11 |
| R54   | C2O3 + NO2 ----> PAN  | k<sub>o</sub>=  3.61E-28e<sup>     0.0/T</sup>(T/300)<sup> -6.87</sup><br>k<sub>i</sub> =   1.24E-11e<sup>     0.0/T</sup>(T/300)<sup> -1.10</sup><br>n=     1.41;F=     0.30 |   9.8670E-12 |
| R55   | PAN ----> NO2 + C2O3  | k<sub>o</sub>=  1.10E-05e<sup>-10100.0/T</sup>(T/300)<sup>  0.00</sup><br>k<sub>i</sub> =   1.90E+17e<sup>-14100.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.41;F=     0.30 |   4.4073E-04 |
| R56   | PAN ---->   0.600\*NO2 +    0.600\*C2O3 +    0.400\*NO3 +    0.400\*MEO2 +    0.400\*RO2  | PAN_IUPAC10 | Not Available<sup>1</sup> | 
| R57   | C2O3 + HO2 ---->   0.370\*PACD +    0.130\*AACD +    0.130\*O3 +    0.500\*MEO2 +    0.500\*RO2 +    0.500\*OH  |   3.14E-12e<sup>   580.00/T</sup> |   2.1967E-11 |
| R58   | C2O3 + RO2 ----> MEO2  |   4.40E-13e<sup>  1070.00/T</sup> |   1.5924E-11 |
| R59   | C2O3 + C2O3 ---->   2.000\*MEO2 +    2.000\*RO2  |   2.90E-12e<sup>   500.00/T</sup> |   1.5514E-11 |
| R60   | C2O3 + CXO3 ----> MEO2 + ALD2 + XO2H +    2.000\*RO2  |   R59 |   1.5514E-11<sup>7</sup>| 
| R61   | CXO3 + NO ----> NO2 + ALD2 + XO2H + RO2  |   6.70E-12e<sup>   340.00/T</sup> |   2.0957E-11 |
| R62   | CXO3 + NO2 ----> PANX  |   8.4000E-01\*R54 |   8.2883E-12<sup>7</sup>| 
| R63   | PANX ----> NO2 + CXO3  |   8.4000E-01\*R55 |   3.7022E-04<sup>7</sup>| 
| R64   | PANX ---->   0.600\*NO2 +    0.600\*CXO3 +    0.400\*NO3 +    0.400\*ALD2 +    0.400\*XO2H +    0.400\*RO2  | PAN_IUPAC10 | Not Available<sup>1</sup> | 
| R65   | CXO3 + HO2 ---->   0.370\*PACD +    0.130\*AACD +    0.130\*O3 +    0.500\*OH +    0.500\*MEO2 +    0.500\*RO2  |   R57 |   2.1967E-11<sup>7</sup>| 
| R66   | CXO3 + RO2 ----> MEO2  |   R58 |   1.5924E-11<sup>7</sup>| 
| R67   | CXO3 + CXO3 ---->   2.000\*MEO2 +    2.000\*RO2  |   R59 |   1.5514E-11<sup>7</sup>| 
| R68   | RO2 + NO ----> NO  |   2.40E-12e<sup>   360.00/T</sup> |   8.0278E-12 |
| R69   | RO2 + HO2 ----> HO2  |   4.80E-13e<sup>   800.00/T</sup> |   7.0234E-12 |
| R70   | RO2 + RO2 ----> |   6.50E-14e<sup>   500.00/T</sup> |   3.4772E-13 |
| R71   | MEO2 + NO ----> FORM + HO2 + NO2  |   2.30E-12e<sup>   360.00/T</sup> |   7.6933E-12 |
| R72   | MEO2 + HO2 ---->   0.900\*MEPX +    0.100\*FORM  |   3.80E-13e<sup>   780.00/T</sup> |   5.1994E-12 |
| R73   | MEO2 + C2O3 ----> FORM +    0.900\*HO2 +    0.900\*MEO2 +    0.100\*AACD +    0.900\*RO2  |   2.00E-12e<sup>   500.00/T</sup> |   1.0699E-11 |
| R74   | MEO2 + RO2 ---->   0.685\*FORM +    0.315\*MEOH +    0.370\*HO2 + RO2  |   R70 |   3.4772E-13<sup>7</sup>| 
| R75   | XO2H + NO ----> NO2 + HO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R76   | XO2H + HO2 ----> ROOH  |   6.80E-13e<sup>   800.00/T</sup> |   9.9498E-12 |
| R77   | XO2H + C2O3 ---->   0.800\*HO2 +    0.800\*MEO2 +    0.200\*AACD +    0.800\*RO2  |   R58 |   1.5924E-11<sup>7</sup>| 
| R78   | XO2H + RO2 ---->   0.600\*HO2 + RO2  |   R70 |   3.4772E-13<sup>7</sup>| 
| R79   | XO2 + NO ----> NO2  |   R75 |   9.0313E-12<sup>7</sup>| 
| R80   | XO2 + HO2 ----> ROOH  |   R76 |   9.9498E-12<sup>7</sup>| 
| R81   | XO2 + C2O3 ---->   0.800\*MEO2 +    0.200\*AACD +    0.800\*RO2  |   R58 |   1.5924E-11<sup>7</sup>| 
| R82   | XO2 + RO2 ----> RO2  |   R70 |   3.4772E-13<sup>7</sup>| 
| R83   | XO2N + NO ---->   0.500\*NTR1 +    0.500\*NTR2  |   R75 |   9.0313E-12<sup>7</sup>| 
| R84   | XO2N + HO2 ----> ROOH  |   R76 |   9.9498E-12<sup>7</sup>| 
| R85   | XO2N + C2O3 ---->   0.800\*HO2 +    0.800\*MEO2 +    0.200\*AACD +    0.800\*RO2  |   R58 |   1.5924E-11<sup>7</sup>| 
| R86   | XO2N + RO2 ----> RO2  |   R70 |   3.4772E-13<sup>7</sup>| 
| R87   | MEPX + OH ---->   0.600\*MEO2 +    0.600\*RO2 +    0.400\*FORM +    0.400\*OH  |   5.30E-12e<sup>   190.00/T</sup> |   1.0024E-11 |
| R88   | MEPX ----> MEO2 + RO2 + OH  | MEPX_IUPAC10 | Not Available<sup>1</sup> | 
| R89   | ROOH + OH ---->   0.540\*XO2H +    0.060\*XO2N +    0.600\*RO2 +    0.400\*OH  |   5.30E-12e<sup>   190.00/T</sup> |   1.0024E-11 |
| R90   | ROOH ----> HO2 + OH  | MEPX_IUPAC10 | Not Available<sup>1</sup> | 
| R91   | NTR1 + OH ----> NTR2  |   2.0000E-12 |   2.0000E-12 |
| R92   | NTR1 ----> NO2  | NTR_IUPAC10 | Not Available<sup>1</sup> | 
| R93   | FACD + OH ----> HO2  |   4.5000E-13 |   4.5000E-13 |
| R94   | AACD + OH ----> MEO2 + RO2  |   4.00E-14e<sup>   850.00/T</sup> |   6.9214E-13 |
| R95   | PACD + OH ----> C2O3  |   5.30E-12e<sup>   190.00/T</sup> |   1.0024E-11 |
| R96   | FORM + OH ----> HO2 + CO  |   5.40E-12e<sup>   135.00/T</sup> |   8.4926E-12 |
| R97   | FORM ---->   2.000\*HO2 + CO  | FORM_R_IUPAC13 | Not Available<sup>1</sup> | 
| R98   | FORM ----> CO  | FORM_M_IUPAC13 | Not Available<sup>1</sup> | 
| R99   | FORM + O ----> OH + HO2 + CO  |   3.40E-11e<sup> -1600.00/T</sup> |   1.5881E-13 |
| R100   | FORM + NO3 ----> HNO3 + HO2 + CO  |   5.5000E-16 |   5.5000E-16 |
| R101   | FORM + HO2 ----> HCO3  |   9.70E-15e<sup>   625.00/T</sup> |   7.8916E-14 |
| R102   | HCO3 ----> FORM + HO2  |   2.40E+12e<sup> -7000.00/T</sup> |   1.5268E+02 |
| R103   | HCO3 + NO ----> FACD + NO2 + HO2  |   5.6000E-12 |   5.6000E-12 |
| R104   | HCO3 + HO2 ---->   0.500\*MEPX +    0.500\*FACD +    0.200\*OH +    0.200\*HO2  |   5.60E-15e<sup>  2300.00/T</sup> |   1.2544E-11 |
| R105   | ALD2 + O ----> C2O3 + OH  |   1.80E-11e<sup> -1100.00/T</sup> |   4.4976E-13 |
| R106   | ALD2 + OH ----> C2O3  |   4.70E-12e<sup>   345.00/T</sup> |   1.4950E-11 |
| R107   | ALD2 + NO3 ----> C2O3 + HNO3  |   1.40E-12e<sup> -1860.00/T</sup> |   2.7340E-15 |
| R108   | ALD2 ----> MEO2 + RO2 + CO + HO2  | ALD2_R_IUPAC13 | Not Available<sup>1</sup> | 
| R109   | ALDX + O ----> CXO3 + OH  |   1.30E-11e<sup>  -870.00/T</sup> |   7.0255E-13 |
| R110   | ALDX + OH ----> CXO3  |   4.90E-12e<sup>   405.00/T</sup> |   1.9060E-11 |
| R111   | ALDX + NO3 ----> CXO3 + HNO3  |   6.3000E-15 |   6.3000E-15 |
| R112   | ALDX ----> ALD2 + XO2H + RO2 + CO + HO2  | ALDX_R_IUPAC13 | Not Available<sup>1</sup> | 
| R113   | GLYD + OH ---->   0.200\*GLY +    0.200\*HO2 +    0.800\*C2O3  |   8.0000E-12 |   8.0000E-12 |
| R114   | GLYD ---->   0.740\*FORM +    0.890\*CO +    1.400\*HO2 +    0.150\*MEOH +    0.190\*OH +    0.110\*GLY +    0.110\*XO2H +    0.110\*RO2  | GLYD_IUPAC13 | Not Available<sup>1</sup> | 
| R115   | GLYD + NO3 ----> HNO3 + C2O3  |   R107 |   2.7340E-15<sup>7</sup>| 
| R116   | GLY + OH ---->   1.800\*CO +    0.200\*XO2 +    0.200\*RO2 + HO2  |   3.10E-12e<sup>   340.00/T</sup> |   9.6965E-12 |
| R117   | GLY ---->   2.000\*HO2 +    2.000\*CO  | GLY_R_IUPAC13 | Not Available<sup>1</sup> | 
| R118   | GLY + NO3 ----> HNO3 +    1.500\*CO +    0.500\*XO2 +    0.500\*RO2 + HO2  |   4.0000E-16 |   4.0000E-16 |
| R119   | MGLY ----> C2O3 + HO2 + CO  | MGLY_IUPAC10 | Not Available<sup>1</sup> | 
| R120   | MGLY + NO3 ----> HNO3 + C2O3 + XO2 + RO2  |   5.0000E-16 |   5.0000E-16 |
| R121   | MGLY + OH ----> C2O3 + CO  |   1.90E-12e<sup>   575.00/T</sup> |   1.3071E-11 |
| R122   | OH + H2 ----> HO2  |   7.70E-12e<sup> -2100.00/T</sup> |   6.7230E-15 |
| R123   | CO + OH ----> HO2  | k<sub>0</sub>=  1.44E-13e<sup>     0.0/T</sup><br>k<sub>1</sub>=  3.43E-33e<sup>     0.0/T</sup> |   2.2843E-13 |
| R124   | OH + CH4 ----> MEO2 + RO2  |   1.85E-12e<sup> -1690.00/T</sup> |   6.3895E-15 |
| R125   | ETHA + OH ---->   0.991\*ALD2 +    0.991\*XO2H +    0.009\*XO2N + RO2  |   6.90E-12e<sup> -1000.00/T</sup> |   2.4111E-13 |
| R126   | MEOH + OH ----> FORM + HO2  |   2.85E-12e<sup>  -345.00/T</sup> |   8.9600E-13 |
| R127   | ETOH + OH ---->   0.950\*ALD2 +    0.900\*HO2 +    0.100\*XO2H +    0.100\*RO2 +    0.078\*FORM +    0.011\*GLYD  |   3.00E-12e<sup>    20.00/T</sup> |   3.2081E-12 |
| R128   | KET ---->   0.500\*ALD2 +    0.500\*C2O3 +    0.500\*XO2H +    0.500\*CXO3 +    0.500\*MEO2 + RO2 -    2.500\*PAR  | KET_IUPAC10 | Not Available<sup>1</sup> | 
| R129   | ACET ---->   0.380\*CO +    1.380\*MEO2 +    1.380\*RO2 +    0.620\*C2O3  | ACET_IUPAC10 | Not Available<sup>1</sup> | 
| R130   | ACET + OH ----> FORM + C2O3 + XO2 + RO2  |   1.41E-12e<sup>  -620.60/T</sup> |   1.7589E-13 |
| R131   | PRPA + OH ----> XPRP  |   7.60E-12e<sup>  -585.00/T</sup> |   1.0683E-12 |
| R132   | PAR + OH ----> XPAR  |   8.1000E-13 |   8.1000E-13 |
| R133   | ROR ---->   0.200\*KET +    0.420\*ACET +    0.740\*ALD2 +    0.370\*ALDX +    0.040\*XO2N +    0.940\*XO2H +    0.980\*RO2 +    0.020\*ROR -    2.700\*PAR  |   5.70E+12e<sup> -5780.00/T</sup> |   2.1704E+04 |
| R134   | ROR + O2 ----> KET + HO2  |   1.50E-14e<sup>  -200.00/T</sup> |   7.6695E-15 |
| R135   | ROR + NO2 ----> NTR1  |   8.60E-12e<sup>   400.00/T</sup> |   3.2897E-11 |
| R136   | ETHY + OH ---->   0.700\*GLY +    0.700\*OH +    0.300\*FACD +    0.300\*CO +    0.300\*HO2  | k<sub>o</sub>=  5.00E-30e<sup>     0.0/T</sup>(T/300)<sup> -1.50</sup><br>k<sub>i</sub> =   1.00E-12e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.30;F=     0.37 |   7.5233E-13 |
| R137   | ETH + O ----> FORM + HO2 + CO +    0.700\*XO2H +    0.700\*RO2 +    0.300\*OH  |   1.04E-11e<sup>  -792.00/T</sup> |   7.3010E-13 |
| R138   | ETH + OH ----> XO2H + RO2 +    1.560\*FORM +    0.220\*GLYD  | k<sub>o</sub>=  8.60E-29e<sup>     0.0/T</sup>(T/300)<sup> -3.10</sup><br>k<sub>i</sub> =   9.00E-12e<sup>     0.0/T</sup>(T/300)<sup> -0.85</sup><br>n=     1.15;F=     0.48 |   7.8392E-12 |
| R139   | ETH + O3 ----> FORM +    0.350\*CO +    0.270\*HO2 +    0.170\*OH +    0.420\*FACD  |   6.82E-15e<sup> -2500.00/T</sup> |   1.5567E-18 |
| R140   | ETH + NO3 ---->   0.500\*NO2 +    0.500\*NTR1 +    0.500\*XO2H +    0.500\*XO2 + RO2 +    1.125\*FORM  |   3.30E-12e<sup> -2880.00/T</sup> |   2.1058E-16 |
| R141   | OLE + O ---->   0.200\*ALD2 +    0.300\*ALDX +    0.100\*HO2 +    0.200\*XO2H +    0.200\*CO +    0.200\*FORM +    0.010\*XO2N +    0.210\*RO2 +    0.200\*PAR +    0.100\*OH  |   1.00E-11e<sup>  -280.00/T</sup> |   3.9097E-12 |
| R142   | OLE + OH ---->   0.781\*FORM +    0.488\*ALD2 +    0.488\*ALDX +    0.976\*XO2H +    0.195\*XO2 +    0.024\*XO2N +    1.195\*RO2 -    0.730\*PAR  | k<sub>o</sub>=  8.00E-27e<sup>     0.0/T</sup>(T/300)<sup> -3.50</sup><br>k<sub>i</sub> =   3.00E-11e<sup>     0.0/T</sup>(T/300)<sup> -1.00</sup><br>n=     1.13;F=     0.50 |   2.8547E-11 |
| R143   | OLE + O3 ---->   0.295\*ALD2 +    0.555\*FORM +    0.270\*ALDX +    0.150\*XO2H +    0.150\*RO2 +    0.334\*OH +    0.080\*HO2 +    0.378\*CO +    0.075\*GLY +    0.075\*MGLY +    0.090\*FACD +    0.130\*AACD +    0.040\*H2O2 -    0.790\*PAR  |   5.50E-15e<sup> -1880.00/T</sup> |   1.0044E-17 |
| R144   | OLE + NO3 ---->   0.500\*NO2 +    0.500\*NTR1 +    0.480\*XO2 +    0.480\*XO2H +    0.040\*XO2N + RO2 +    0.500\*FORM +    0.250\*ALD2 +    0.375\*ALDX - PAR  |   4.60E-13e<sup> -1155.00/T</sup> |   9.5576E-15 |
| R145   | IOLE + O ---->   1.240\*ALD2 +    0.660\*ALDX +    0.100\*XO2H +    0.100\*RO2 +    0.100\*CO +    0.100\*PAR  |   2.3000E-11 |   2.3000E-11 |
| R146   | IOLE + OH ---->   1.300\*ALD2 +    0.700\*ALDX + XO2H + RO2  |   1.05E-11e<sup>   519.00/T</sup> |   5.9866E-11 |
| R147   | IOLE + O3 ---->   0.732\*ALD2 +    0.442\*ALDX +    0.128\*FORM +    0.245\*CO +    0.500\*OH +    0.300\*XO2H +    0.300\*RO2 +    0.240\*GLY +    0.060\*MGLY +    0.290\*PAR +    0.080\*AACD +    0.080\*H2O2  |   4.70E-15e<sup> -1013.00/T</sup> |   1.5723E-16 |
| R148   | IOLE + NO3 ---->   0.500\*NO2 +    0.500\*NTR1 +    0.480\*XO2 +    0.480\*XO2H +    0.040\*XO2N + RO2 +    0.500\*ALD2 +    0.625\*ALDX + PAR  |   3.7000E-13 |   3.7000E-13 |
| R149   | ISOP + OH ----> ISO2 + RO2 + ISOPRXN  |   2.70E-11e<sup>   390.00/T</sup> |   9.9873E-11 |
| R150   | ISOP + O ---->   0.750\*ISPD +    0.500\*FORM +    0.250\*XO2 +    0.250\*RO2 +    0.250\*HO2 +    0.250\*CXO3 +    0.250\*PAR  |   3.0000E-11 |   3.0000E-11 |
| R151   | ISO2 + NO ---->   0.100\*INTR +    0.900\*NO2 +    0.673\*FORM +    0.900\*ISPD +    0.818\*HO2 +    0.082\*XO2H +    0.082\*RO2  |   2.39E-12e<sup>   365.00/T</sup> |   8.1296E-12 |
| R152   | ISO2 + HO2 ---->   0.880\*ISPX +    0.120\*OH +    0.120\*HO2 +    0.120\*FORM +    0.120\*ISPD  |   7.43E-13e<sup>   700.00/T</sup> |   7.7737E-12 |
| R153   | ISO2 + C2O3 ---->   0.598\*FORM + ISPD +    0.728\*HO2 +    0.072\*XO2H +    0.800\*MEO2 +    0.200\*AACD +    0.872\*RO2  |   R58 |   1.5924E-11<sup>7</sup>| 
| R154   | ISO2 + RO2 ---->   0.598\*FORM + ISPD +    0.728\*HO2 +    0.072\*XO2H +    1.072\*RO2  |   R70 |   3.4772E-13<sup>7</sup>| 
| R155   | ISO2 ----> HO2 + HPLD  |   3.30E+09e<sup> -8300.00/T</sup> |   2.6821E-03 |
| R156   | ISOP + O3 ---->   0.600\*FORM +    0.650\*ISPD +    0.150\*ALDX +    0.200\*CXO3 +    0.350\*PAR +    0.266\*OH +    0.200\*XO2 +    0.200\*RO2 +    0.066\*HO2 +    0.066\*CO  |   1.03E-14e<sup> -1995.00/T</sup> |   1.2790E-17 |
| R157   | ISOP + NO3 ---->   0.350\*NO2 +    0.650\*NTR2 +    0.640\*XO2H +    0.330\*XO2 +    0.030\*XO2N + RO2 +    0.350\*FORM +    0.350\*ISPD + ISOPRXN  |   3.03E-12e<sup>  -448.00/T</sup> |   6.7433E-13 |
| R158   | ISPD + OH ---->   0.022\*XO2N +    0.521\*XO2 +    0.115\*MGLY +    0.115\*MEO2 +    0.269\*GLYD +    0.269\*C2O3 +    0.457\*OPO3 +    0.117\*PAR +    0.137\*ACET +    0.137\*CO +    0.137\*HO2 +    0.658\*RO2  |   5.58E-12e<sup>   511.00/T</sup> |   3.0972E-11 |
| R159   | ISPD + O3 ---->   0.040\*ALD2 +    0.231\*FORM +    0.531\*MGLY +    0.170\*GLY +    0.170\*ACET +    0.543\*CO +    0.461\*OH +    0.150\*FACD +    0.398\*HO2 +    0.143\*C2O3  |   3.88E-15e<sup> -1770.00/T</sup> |   1.0247E-17 |
| R160   | ISPD + NO3 ---->   0.717\*HNO3 +    0.142\*NTR2 +    0.142\*NO2 +    0.142\*XO2 +    0.142\*XO2H +    0.113\*GLYD +    0.113\*MGLY +    0.717\*PAR +    0.717\*CXO3 +    0.284\*RO2  |   4.10E-12e<sup> -1860.00/T</sup> |   8.0066E-15 |
| R161   | ISPD ---->   0.760\*HO2 +    0.340\*XO2H +    0.160\*XO2 +    0.340\*MEO2 +    0.208\*C2O3 +    0.260\*FORM +    0.240\*OLE +    0.240\*PAR +    0.170\*ACET +    0.128\*GLYD +    0.840\*RO2  | ISPD | Not Available<sup>1</sup> | 
| R162   | ISPX + OH ---->   0.904\*EPOX +    0.933\*OH +    0.067\*ISO2 +    0.067\*RO2 +    0.029\*IOLE +    0.029\*ALDX  |   2.23E-11e<sup>   372.00/T</sup> |   7.7655E-11 |
| R163   | HPLD ----> OH + ISPD  | HPALD | Not Available<sup>1</sup> | 
| R164   | HPLD + NO3 ----> HNO3 + ISPD  |   6.00E-12e<sup> -1860.00/T</sup> |   1.1717E-14 |
| R165   | EPOX + OH ----> EPX2 + RO2  |   5.78E-11e<sup>  -400.00/T</sup> |   1.5110E-11 |
| R166   | EPX2 + HO2 ---->   0.275\*GLYD +    0.275\*GLY +    0.275\*MGLY +    1.125\*OH +    0.825\*HO2 +    0.375\*FORM +    0.074\*FACD +    0.251\*CO +    2.175\*PAR  |   7.43E-13e<sup>   700.00/T</sup> |   7.7737E-12 |
| R167   | EPX2 + NO ---->   0.275\*GLYD +    0.275\*GLY +    0.275\*MGLY +    0.125\*OH +    0.825\*HO2 +    0.375\*FORM + NO2 +    0.251\*CO +    2.175\*PAR  |   2.39E-12e<sup>   365.00/T</sup> |   8.1296E-12 |
| R168   | EPX2 + C2O3 ---->   0.220\*GLYD +    0.220\*GLY +    0.220\*MGLY +    0.100\*OH +    0.660\*HO2 +    0.300\*FORM +    0.200\*CO +    1.740\*PAR +    0.800\*MEO2 +    0.200\*AACD +    0.800\*RO2  |   R58 |   1.5924E-11<sup>7</sup>| 
| R169   | EPX2 + RO2 ---->   0.275\*GLYD +    0.275\*GLY +    0.275\*MGLY +    0.125\*OH +    0.825\*HO2 +    0.375\*FORM +    0.251\*CO +    2.175\*PAR + RO2  |   R70 |   3.4772E-13<sup>7</sup>| 
| R170   | INTR + OH ---->   0.630\*XO2 +    0.370\*XO2H + RO2 +    0.444\*NO2 +    0.185\*NO3 +    0.104\*INTR +    0.592\*FORM +    0.331\*GLYD +    0.185\*FACD +    2.700\*PAR +    0.098\*OLE +    0.078\*ALDX +    0.266\*NTR2  |   3.1000E-11 |   3.1000E-11 |
| R171   | TERP + O ---->   0.150\*ALDX +    5.120\*PAR + TRPRXN  |   3.6000E-11 |   3.6000E-11 |
| R172   | TERP + OH ---->   0.750\*XO2H +    0.500\*XO2 +    0.250\*XO2N +    1.500\*RO2 +    0.280\*FORM +    1.660\*PAR +    0.470\*ALDX + TRPRXN  |   1.50E-11e<sup>   449.00/T</sup> |   6.7627E-11 |
| R173   | TERP + O3 ---->   0.570\*OH +    0.070\*XO2H +    0.690\*XO2 +    0.180\*XO2N +    0.940\*RO2 +    0.240\*FORM +    0.001\*CO +    7.000\*PAR +    0.210\*ALDX +    0.390\*CXO3 + TRPRXN  |   1.20E-15e<sup>  -821.00/T</sup> |   7.6434E-17 |
| R174   | TERP + NO3 ---->   0.470\*NO2 +    0.280\*XO2H +    0.750\*XO2 +    0.250\*XO2N +    1.280\*RO2 +    0.470\*ALDX +    0.530\*NTR2 + TERPNRO2  |   3.70E-12e<sup>   175.00/T</sup> |   6.6544E-12 |
| R171a   | APIN + O ---->   0.150\*ALDX +    5.120\*PAR + TRPRXN  |   3.6000E-11 |   3.6000E-11 |
| R172a   | APIN + OH ---->   0.750\*XO2H +    0.500\*XO2 +    0.250\*XO2N +    1.500\*RO2 +    0.280\*FORM +    1.660\*PAR +    0.470\*ALDX + TRPRXN  |   1.50E-11e<sup>   449.00/T</sup> |   6.7627E-11 |
| R173a   | APIN + O3 ---->   0.570\*OH +    0.070\*XO2H +    0.690\*XO2 +    0.180\*XO2N +    0.940\*RO2 +    0.240\*FORM +    0.001\*CO +    7.000\*PAR +    0.210\*ALDX +    0.390\*CXO3 + TRPRXN  |   1.20E-15e<sup>  -821.00/T</sup> |   7.6434E-17 |
| R174a   | APIN + NO3 ---->   0.470\*NO2 +    0.280\*XO2H +    0.750\*XO2 +    0.250\*XO2N +    1.280\*RO2 +    0.470\*ALDX +    0.530\*NTR2  |   3.70E-12e<sup>   175.00/T</sup> |   6.6544E-12 |
| R175   | BENZENE + OH ---->   0.530\*CRES +    0.352\*BZO2 +    0.352\*RO2 +    0.118\*OPEN +    0.118\*OH +    0.530\*HO2 + BENZRO2  |   2.30E-12e<sup>  -190.00/T</sup> |   1.2161E-12 |
| R176   | BZO2 + NO ---->   0.918\*NO2 +    0.082\*NTR2 +    0.918\*GLY +    0.918\*OPEN +    0.918\*HO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R177   | BZO2 + C2O3 ----> GLY + OPEN + HO2 + MEO2 + RO2  |   R58 |   1.5924E-11<sup>7</sup>| 
| R178   | BZO2 + HO2 ----> |   1.90E-13e<sup>  1300.00/T</sup> |   1.4872E-11 |
| R179   | BZO2 + RO2 ----> GLY + OPEN + HO2 + RO2  |   R70 |   3.4772E-13<sup>7</sup>| 
| R180   | TOL + OH ---->   0.180\*CRES +    0.650\*TO2 +    0.720\*RO2 +    0.100\*OPEN +    0.100\*OH +    0.070\*XO2H +    0.180\*HO2 + TOLRO2  |   1.80E-12e<sup>   340.00/T</sup> |   5.6302E-12 |
| R181   | TO2 + NO ---->   0.860\*NO2 +    0.140\*NTR2 +    0.417\*GLY +    0.443\*MGLY +    0.660\*OPEN +    0.200\*XOPN +    0.860\*HO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R182   | TO2 + C2O3 ---->   0.480\*GLY +    0.520\*MGLY +    0.770\*OPEN +    0.230\*XOPN + HO2 + MEO2 + RO2  |   R58 |   1.5924E-11<sup>7</sup>| 
| R183   | TO2 + HO2 ----> |   1.90E-13e<sup>  1300.00/T</sup> |   1.4872E-11 |
| R184   | TO2 + RO2 ---->   0.480\*GLY +    0.520\*MGLY +    0.770\*OPEN +    0.230\*XOPN + HO2 + RO2  |   R70 |   3.4772E-13<sup>7</sup>| 
| R185   | XYLMN + OH ---->   0.155\*CRES +    0.544\*XLO2 +    0.602\*RO2 +    0.244\*XOPN +    0.244\*OH +    0.058\*XO2H +    0.155\*HO2 + XYLRO2  |   1.8500E-11 |   1.8500E-11 |
| R185a   | NAPH + OH ---->   0.155\*CRES +    0.544\*XLO2 +    0.602\*RO2 +    0.244\*XOPN +    0.244\*OH +    0.058\*XO2H +    0.155\*HO2 + PAHRO2  |   1.8500E-11 |   1.8500E-11 |
| R186   | XLO2 + NO ---->   0.860\*NO2 +    0.140\*NTR2 +    0.221\*GLY +    0.675\*MGLY +    0.300\*OPEN +    0.560\*XOPN +    0.860\*HO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R187   | XLO2 + HO2 ----> |   1.90E-13e<sup>  1300.00/T</sup> |   1.4872E-11 |
| R188   | XLO2 + C2O3 ---->   0.260\*GLY +    0.770\*MGLY +    0.350\*OPEN +    0.650\*XOPN + HO2 + MEO2 + RO2  |   R58 |   1.5924E-11<sup>7</sup>| 
| R189   | XLO2 + RO2 ---->   0.260\*GLY +    0.770\*MGLY +    0.350\*OPEN +    0.650\*XOPN + HO2 + RO2  |   R70 |   3.4772E-13<sup>7</sup>| 
| R190   | CRES + OH ---->   0.025\*GLY +    0.025\*OPEN + HO2 +    0.200\*CRO +    0.732\*CAT1 +    0.020\*XO2N +    0.020\*RO2  |   1.70E-12e<sup>   950.00/T</sup> |   4.1138E-11 |
| R191   | CRES + NO3 ---->   0.300\*CRO + HNO3 +    0.480\*XO2 +    0.120\*XO2H +    0.240\*GLY +    0.240\*MGLY +    0.480\*OPO3 +    0.100\*XO2N +    0.700\*RO2  |   1.4000E-11 |   1.4000E-11 |
| R192   | CRO + NO2 ----> CRON  |   2.1000E-12 |   2.1000E-12 |
| R193   | CRO + HO2 ----> CRES  |   5.5000E-12 |   5.5000E-12 |
| R194   | CRON + OH ----> NTR2 +    0.500\*CRO  |   1.5300E-12 |   1.5300E-12 |
| R195   | CRON + NO3 ----> NTR2 +    0.500\*CRO + HNO3  |   3.8000E-12 |   3.8000E-12 |
| R196   | CRON ----> HONO + HO2 + FORM + OPEN  | NTR_IUPAC10 | Not Available<sup>1</sup> | 
| R197   | XOPN ---->   0.400\*GLY + XO2H +    0.700\*HO2 +    0.700\*CO +    0.300\*C2O3  |   5.0000E-02\*NO2_IUPAC10 | Not Available<sup>1</sup> | 
| R198   | XOPN + OH ----> MGLY +    0.400\*GLY +    2.000\*XO2H +    2.000\*RO2  |   9.0000E-11 |   9.0000E-11 |
| R199   | XOPN + O3 ---->   1.200\*MGLY +    0.500\*OH +    0.600\*C2O3 +    0.100\*ALD2 +    0.500\*CO +    0.300\*XO2H +    0.300\*RO2  |   1.08E-16e<sup>  -500.00/T</sup> |   2.0189E-17 |
| R200   | XOPN + NO3 ---->   0.500\*NO2 +    0.500\*NTR2 +    0.450\*XO2H +    0.450\*XO2 +    0.100\*XO2N + RO2 +    0.250\*OPEN +    0.250\*MGLY  |   3.0000E-12 |   3.0000E-12 |
| R201   | OPEN ----> OPO3 + HO2 + CO  |   2.8000E-02\*NO2_IUPAC10 | Not Available<sup>1</sup> | 
| R202   | OPEN + OH ---->   0.600\*OPO3 +    0.400\*XO2H +    0.400\*RO2 +    0.400\*GLY  |   4.4000E-11 |   4.4000E-11 |
| R203   | OPEN + O3 ---->   1.400\*GLY +    0.240\*MGLY +    0.500\*OH +    0.120\*C2O3 +    0.080\*FORM +    0.020\*ALD2 +    1.980\*CO +    0.560\*HO2  |   5.40E-17e<sup>  -500.00/T</sup> |   1.0094E-17 |
| R204   | OPEN + NO3 ----> OPO3 + HNO3  |   3.8000E-12 |   3.8000E-12 |
| R205   | CAT1 + OH ---->   0.140\*FORM +    0.200\*HO2 +    0.500\*CRO  |   5.0000E-11 |   5.0000E-11 |
| R206   | CAT1 + NO3 ----> CRO + HNO3  |   1.7000E-10 |   1.7000E-10 |
| R207   | OPO3 + NO ----> NO2 +    0.500\*GLY +    0.500\*CO +    0.800\*HO2 +    0.200\*CXO3  |   R61 |   2.0957E-11<sup>7</sup>| 
| R208   | OPO3 + NO2 ----> OPAN  |   R62 |   8.2883E-12<sup>7</sup>| 
| R209   | OPAN ----> OPO3 + NO2  |   R63 |   3.7022E-04<sup>7</sup>| 
| R210   | OPO3 + HO2 ---->   0.370\*PACD +    0.130\*AACD +    0.130\*O3 +    0.500\*OH +    0.500\*MEO2 +    0.500\*RO2  |   R57 |   2.1967E-11<sup>7</sup>| 
| R211   | OPO3 + C2O3 ----> MEO2 + XO2 + ALDX +    2.000\*RO2  |   R59 |   1.5514E-11<sup>7</sup>| 
| R212   | OPO3 + RO2 ---->   0.800\*XO2H +    0.800\*ALDX +    1.800\*RO2 +    0.200\*AACD  |   R58 |   1.5924E-11<sup>7</sup>| 
| R213   | OPAN + OH ---->   0.500\*NO2 +    0.500\*GLY + CO +    0.500\*NTR2  |   3.6000E-11 |   3.6000E-11 |
| R214   | PANX + OH ----> ALD2 + NO2  |   3.0000E-12 |   3.0000E-12 |
| R216   | ECH4 + OH ----> MEO2 + RO2  |   1.85E-12e<sup> -1690.00/T</sup> |   6.3895E-15 |
| R217   | XPRP ----> XO2N + RO2  | k<sub>o</sub>=  2.37E-21e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>k<sub>i</sub> =   4.07E-01e<sup>     0.0/T</sup>(T/300)<sup> -8.00</sup><br>n=     1.00;F=     0.41 |   3.0828E-02 |
| R218   | XPRP ---->   0.732\*ACET +    0.268\*ALDX +    0.268\*PAR + XO2H + RO2  |   1.0000E+00 |   1.0000E+00 |
| R219   | XPAR ----> XO2N + RO2  | k<sub>o</sub>=  4.81E-20e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>k<sub>i</sub> =   4.07E-01e<sup>     0.0/T</sup>(T/300)<sup> -8.00</sup><br>n=     1.00;F=     0.41 |   1.4904E-01 |
| R220   | XPAR ---->   0.126\*ALDX +    0.874\*ROR +    0.126\*XO2H +    0.874\*XO2 + RO2 -    0.126\*PAR  |   1.0000E+00 |   1.0000E+00 |
| R221   | NO2 + OH + H2O ----> HNO3  |   1.1000E-30 |   1.1000E-30 |
| CL1   | CL2 ---->   2.000\*CL  | CL2_IUPAC04 | Not Available<sup>1</sup> | 
| CL2   | HOCL ----> OH + CL  | HOCL_IUPAC04 | Not Available<sup>1</sup> | 
| CL3   | CL + O3 ----> CLO  |   2.30E-11e<sup>  -200.00/T</sup> |   1.1760E-11 |
| CL4   | CLO + CLO ---->   0.300\*CL2 +    1.400\*CL  |   1.6300E-14 |   1.6300E-14 |
| CL5   | CLO + NO ----> CL + NO2  |   6.40E-12e<sup>   290.00/T</sup> |   1.6928E-11 |
| CL6   | CLO + HO2 ----> HOCL  |   2.20E-12e<sup>   340.00/T</sup> |   6.8814E-12 |
| CL7   | CLO + MEO2 ----> CL + FORM + HO2  |   3.20E-12e<sup>  -110.00/T</sup> |   2.2127E-12 |
| CL8   | OH + FMCL ----> CL + CO  |   5.0000E-13 |   5.0000E-13 |
| CL9   | FMCL ----> CL + CO + HO2  | FMCL_IUPAC04 | Not Available<sup>1</sup> | 
| CL10   | CL + CH4 ----> HCL + MEO2 + RO2  |   6.60E-12e<sup> -1240.00/T</sup> |   1.0311E-13 |
| CL11   | CL + PAR ----> HCL + XPAR  |   5.0000E-11 |   5.0000E-11 |
| CL12   | CL + PRPA ----> HCL + ACET +    0.970\*XO2H +    0.030\*XO2N + RO2  |   1.4000E-10 |   1.4000E-10 |
| CL13   | CL + ETHA ----> HCL +    0.991\*ALD2 +    0.991\*XO2H +    0.009\*XO2N + RO2  |   8.30E-11e<sup>  -100.00/T</sup> |   5.9349E-11 |
| CL14   | CL + ETH ----> FMCL +    2.000\*XO2 + HO2 + FORM  |   1.0700E-10 |   1.0700E-10 |
| CL15   | CL + OLE ----> FMCL +    0.330\*ALD2 +    0.670\*ALDX +    2.000\*XO2 + HO2 - PAR  |   2.5000E-10 |   2.5000E-10 |
| CL16   | CL + IOLE ---->   0.300\*HCL +    0.700\*FMCL +    0.450\*ALD2 +    0.550\*ALDX +    0.300\*OLE +    0.300\*PAR +    1.700\*XO2 + HO2  |   3.5000E-10 |   3.5000E-10 |
| CL17   | CL + ISOP ----> FMCL + ISPD +    0.960\*XO2H +    0.040\*XO2N + RO2  |   4.3000E-10 |   4.3000E-10 |
| CL18   | CL + FORM ----> HCL + HO2 + CO  |   8.20E-11e<sup>   -34.00/T</sup> |   7.3162E-11 |
| CL19   | CL + ALD2 ----> HCL + C2O3  |   7.9000E-11 |   7.9000E-11 |
| CL20   | CL + ALDX ----> HCL + CXO3  |   1.3000E-10 |   1.3000E-10 |
| CL21   | CL + MEOH ----> HCL + HO2 + FORM  |   5.5000E-11 |   5.5000E-11 |
| CL22   | CL + ETOH ----> HCL + HO2 + ALD2  |   8.20E-11e<sup>    45.00/T</sup> |   9.5359E-11 |
| CL23   | HCL + OH ----> CL  |   6.58E-13e<sup>    58.00/T</sup>(T/300)<sup>  1.16 </sup> |   7.9359E-13 |
| CL24   | CL + TOL ----> HCL +    0.180\*CRES +    0.650\*TO2 +    0.720\*RO2 +    0.100\*OPEN +    0.100\*OH +    0.070\*XO2H +    0.180\*HO2 + TOLRO2  |   6.1000E-11 |   6.1000E-11 |
| CL25   | CL + XYLMN ----> HCL +    0.155\*CRES +    0.544\*XLO2 +    0.602\*RO2 +    0.244\*XOPN +    0.244\*OH +    0.058\*XO2H +    0.155\*HO2 + XYLRO2  |   1.2000E-10 |   1.2000E-10 |
| CL26   | CL + NAPH ----> HCL +    0.155\*CRES +    0.544\*XLO2 +    0.602\*RO2 +    0.244\*XOPN +    0.244\*OH +    0.058\*XO2H +    0.155\*HO2 + PAHRO2  |   1.2000E-10 |   1.2000E-10 |
| CL27   | CLNO2 ----> CL + NO2  | CLNO2_IUPAC13 | Not Available<sup>1</sup> | 
| CL28   | CLO + NO2 ----> CLNO3  | k<sub>o</sub>=  1.80E-31e<sup>     0.0/T</sup>(T/300)<sup> -3.40</sup><br>k<sub>i</sub> =   1.50E-11e<sup>     0.0/T</sup>(T/300)<sup> -1.90</sup><br>n=     1.00;F=     0.60 |   2.3359E-12 |
| CL30   | CLNO3 ----> CLO + NO2  | CLONO2_1 | Not Available<sup>1</sup> | 
| CL31   | CLNO3 ----> CL + NO3  | CLONO2_2 | Not Available<sup>1</sup> | 
| HET_CLNO3_WAI   | CLNO3 ----> HOCL + HNO3  | HETERO_CLNO3_WAI | Not Available<sup>2</sup> | 
| HET_CLNO3_WAJ   | CLNO3 ----> HOCL + HNO3  | HETERO_CLNO3_WAJ | Not Available<sup>2</sup> | 
| SA01   | TOLRO2 + NO ----> NO +    0.016\*SVAVB2 +    0.051\*SVAVB3 +    0.047\*SVAVB4  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| SA02   | TOLRO2 + HO2 ----> HO2 +    0.140\*SVAVB1  |   1.90E-13e<sup>  1300.00/T</sup> |   1.4872E-11 |
| SA03   | XYLRO2 + NO ----> NO +    0.015\*SVAVB2 +    0.023\*SVAVB3 +    0.060\*SVAVB4  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| SA04   | XYLRO2 + HO2 ----> HO2 +    0.193\*SVAVB1  |   1.90E-13e<sup>  1300.00/T</sup> |   1.4872E-11 |
| SA06   | BENZRO2 + NO ----> NO +    0.034\*SVAVB2 +    0.392\*SVAVB4  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| SA07   | BENZRO2 + HO2 ----> HO2 +    0.146\*SVAVB1  |   1.90E-13e<sup>  1300.00/T</sup> |   1.4872E-11 |
| SA08   | SESQ + O3 ----> O3 + SESQRXN  |   1.1600E-14 |   1.1600E-14 |
| SA09   | SESQ + OH ----> OH + SESQRXN  |   1.9700E-10 |   1.9700E-10 |
| SA10   | SESQ + NO3 ----> NO3 + SESQRXN  |   1.9000E-11 |   1.9000E-11 |
| SA11   | PAHRO2 + NO ----> NO +    0.028\*SVAVB2 +    0.225\*SVAVB3 +    0.191\*SVAVB4  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| SA12   | PAHRO2 + HO2 ----> HO2 +    0.473\*SVAVB1  |   1.90E-13e<sup>  1300.00/T</sup> |   1.4872E-11 |
| SA13   | SOAALK + OH ----> OH +    0.006\*SVAVB2 +    0.052\*SVAVB3 +    0.081\*SVAVB4  |   2.70E-12e<sup>   374.00/T</sup> |   9.4655E-12 |
| HET_NTR2   | NTR2 ----> HNO3  |   1.4000E+00\*HETERO_NTR2 | Not Available<sup>2</sup> | 
| HET_N2O5IJ   | N2O5 ----> HNO3 + H2NO3PIJ  | HETERO_N2O5IJ | Not Available<sup>2</sup> | 
| HET_N2O5K   | N2O5 ----> HNO3 + H2NO3PK  | HETERO_N2O5K | Not Available<sup>2</sup> | 
| HET_H2NO3PIJA   | H2NO3PIJ ----> HNO3  | HETERO_H2NO3PAIJ | Not Available<sup>2</sup> | 
| HET_H2NO3PKA   | H2NO3PK ----> HNO3  | HETERO_H2NO3PAK | Not Available<sup>2</sup> | 
| HET_H2NO3PIB   | H2NO3PIJ + ACLI ----> CLNO2  | HETERO_H2NO3PBIJ | Not Available<sup>2</sup> | 
| HET_H2NO3PJB   | H2NO3PIJ + ACLJ ----> CLNO2  | HETERO_H2NO3PBIJ | Not Available<sup>2</sup> | 
| HET_H2NO3PKB   | H2NO3PK + ACLK ----> CLNO2  | HETERO_H2NO3PBK | Not Available<sup>2</sup> | 
| HET_N02   | NO2 ---->   0.500\*HONO +    0.500\*HNO3  | HETERO_NO2 | Not Available<sup>2</sup> | 
| HAL_Ozone   | O3 ----> | SEAWATER*min( 6.701E-11e<sup> 1.074E+01P</sup>+ 3.415E-08e<sup>-6.713E-01P</sup>, <br> 2.000E-06) |   2.0000E-06<sup>4</sup>| 
| HET_IEPOX   | EPOX ----> IEPOXP  | HETERO_IEPOX | Not Available<sup>2</sup> | 
| HET_IEPOXOS   | IEPOXP + ASO4J ----> AISO3J  | HETERO_IEPOXOS | Not Available<sup>2</sup> | 
| HET_TETROL   | IEPOXP ----> AISO3J  | HETERO_TETROL | Not Available<sup>2</sup> | 
| HET_GLY   | GLY ----> AGLYJ  | HETERO_GLY | Not Available<sup>2</sup> | 
| HET_MGLY   | MGLY ----> AGLYJ  | HETERO_MGLY | Not Available<sup>2</sup> | 
| BL18a   | TERPNRO2 + NO ----> NO +    0.688\*MTNO3  |   2.60E-12e<sup>   380.00/T</sup> |   9.3002E-12 |
| BL18b   | TERPNRO2 + HO2 ----> HO2 + MTNO3  |   2.65E-13e<sup>  1300.00/T</sup> |   2.0743E-11 |
| BL18c   | TERPNRO2 + NO3 ----> NO3 +    0.422\*MTNO3  |   2.3000E-12 |   2.3000E-12 |
| BL18d   | TERPNRO2 + RO2 ----> RO2 +    0.711\*MTNO3  |   3.5000E-14 |   3.5000E-14 |
| CP07mtp   | MTNO3 + CL ----> CL +    0.370\*MTNO3  |   1.9200E-10 |   1.9200E-10 |
| BP70mtp   | MTNO3 + OH ----> OH +    0.240\*MTNO3  |   7.2000E-12 |   7.2000E-12 |
| BP71mtp   | MTNO3 ----> | IC3ONO2 | Not Available<sup>1</sup> | 
| HYD_MT   | AMTNO3J ----> AMTHYDJ  |   9.2590E-05 |   9.2590E-05 |
| OLIG_AROMATIC1   | AAVB2J ---->   0.907\*AOLGAJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_AROMATIC2   | AAVB3J ---->   0.925\*AOLGAJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_AROMATIC3   | AAVB4J ---->   0.943\*AOLGAJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_ISOPRENE1   | AISO1J ---->   0.500\*AOLGBJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_ISOPRENE2   | AISO2J ---->   0.500\*AOLGBJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_SESQT1   | ASQTJ ---->   1.500\*AOLGBJ  |   9.4882E-06 |   9.4882E-06 |
| RPOAGEPI   | APOCI + OH ---->   1.250\*APNCOMI + APOCI + OH  |   2.5000E-12 |   2.5000E-12 |
| RPOAGELI   | APNCOMI + OH ----> OH  | HETERO_PNCOMLI | Not Available<sup>2</sup> | 
| RPOAGEPJ   | APOCJ + OH ---->   1.250\*APNCOMJ + APOCJ + OH  |   2.5000E-12 |   2.5000E-12 |
| RPOAGELJ   | APNCOMJ + OH ----> OH  | HETERO_PNCOMLJ | Not Available<sup>2</sup> | 
| PCSOA   | PCVOC + OH ----> OH + PCSOARXN  |   1.2500E-11 |   1.2500E-11 |
| POA_AGE1   | VLVPO1 + OH ----> OH +    0.486\*VLVPO1 +    0.006\*VSVPO1 +    0.003\*VSVPO2 +    0.003\*VSVPO3 +    0.002\*VIVPO1 +    0.294\*VLVOO1 +    0.202\*VLVOO2 +    0.002\*VSVOO2 +    0.002\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE2   | VSVPO1 + OH ----> OH +    0.300\*VLVPO1 +    0.286\*VSVPO1 +    0.004\*VSVPO2 +    0.004\*VSVPO3 +    0.224\*VLVOO1 +    0.182\*VLVOO2  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE3   | VSVPO2 + OH ----> OH +    0.386\*VLVPO1 +    0.095\*VSVPO1 +    0.137\*VSVPO2 +    0.001\*VSVPO3 +    0.205\*VLVOO1 +    0.176\*VLVOO2  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE4   | VSVPO3 + OH ----> OH +    0.218\*VLVPO1 +    0.306\*VSVPO1 +    0.015\*VSVPO2 +    0.104\*VSVPO3 +    0.189\*VLVOO1 +    0.167\*VLVOO2  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE5   | VIVPO1 + OH ----> OH +    0.241\*VLVPO1 +    0.209\*VSVPO1 +    0.300\*VSVPO2 +    0.203\*VLVOO1 +    0.047\*VLVOO2  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE6   | VLVOO1 + OH ----> OH +    0.666\*VLVOO1 +    0.014\*VLVOO2 +    0.012\*VSVOO1 +    0.124\*VSVOO2 +    0.183\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE7   | VLVOO2 + OH ----> OH +    0.286\*VLVOO1 +    0.393\*VLVOO2 +    0.014\*VSVOO1 +    0.103\*VSVOO2 +    0.204\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE8   | VSVOO1 + OH ----> OH +    0.330\*VLVOO1 +    0.227\*VLVOO2 +    0.261\*VSVOO1 +    0.070\*VSVOO2 +    0.112\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE9   | VSVOO2 + OH ----> OH +    0.344\*VLVOO1 +    0.275\*VLVOO2 +    0.049\*VSVOO1 +    0.258\*VSVOO2 +    0.074\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE10   | VSVOO3 + OH ----> OH +    0.389\*VLVOO1 +    0.242\*VLVOO2 +    0.064\*VSVOO1 +    0.038\*VSVOO2 +    0.267\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| T01   | FORM_PRIMARY + OH ----> OH  |   5.40E-12e<sup>   135.00/T</sup> |   8.4926E-12 |
| T02   | FORM_PRIMARY + NO3 ----> NO3  |   5.5000E-16 |   5.5000E-16 |
| T03   | FORM_PRIMARY + O ----> O  |   3.40E-11e<sup> -1600.00/T</sup> |   1.5881E-13 |
| T04   | FORM_PRIMARY ----> | FORM_R_IUPAC13 | Not Available<sup>1</sup> | 
| T05   | FORM_PRIMARY ----> | FORM_M_IUPAC13 | Not Available<sup>1</sup> | 
| TCL1   | FORM_PRIMARY + CL ----> CL  |   8.20E-11e<sup>   -34.00/T</sup> |   7.3162E-11 |
| T06   | ALD2_PRIMARY + OH ----> OH  |   4.70E-12e<sup>   345.00/T</sup> |   1.4950E-11 |
| T07   | ALD2_PRIMARY + NO3 ----> NO3  |   1.40E-12e<sup> -1860.00/T</sup> |   2.7340E-15 |
| T08   | ALD2_PRIMARY + O ----> O  |   1.80E-11e<sup> -1100.00/T</sup> |   4.4976E-13 |
| T09   | ALD2_PRIMARY ----> | ALD2_R_IUPAC13 | Not Available<sup>1</sup> | 
| TCL2   | ALD2_PRIMARY + CL ----> CL  |   7.9000E-11 |   7.9000E-11 |
| T10   | BUTADIENE13 + OH ----> OH +    0.580\*ACROLEIN  |   1.48E-11e<sup>   448.00/T</sup> |   6.6502E-11 |
| T11   | BUTADIENE13 + O3 ----> O3 +    0.520\*ACROLEIN  |   1.34E-14e<sup> -2283.00/T</sup> |   6.3331E-18 |
| T12   | BUTADIENE13 + NO3 ----> NO3 +    0.045\*ACROLEIN  |   1.7900E-13 |   1.7900E-13 |
| TCL3   | BUTADIENE13 + CL ----> CL +    0.580\*ACROLEIN  |   2.5100E-10 |   2.5100E-10 |
| T13   | ACRO_PRIMARY + OH ----> OH  |   2.0000E-11 |   2.0000E-11 |
| T14   | ACRO_PRIMARY + O3 ----> O3  |   2.6100E-19 |   2.6100E-19 |
| T15   | ACRO_PRIMARY + NO3 ----> NO3  |   1.1500E-15 |   1.1500E-15 |
| T16   | ACRO_PRIMARY ----> | ACRO_09 | Not Available<sup>1</sup> | 
| TCL4   | ACRO_PRIMARY + CL ----> CL  |   2.3700E-10 |   2.3700E-10 |
| T17   | ACROLEIN + OH ----> OH  |   2.0000E-11 |   2.0000E-11 |
| T18   | ACROLEIN + O3 ----> O3  |   2.6100E-19 |   2.6100E-19 |
| T19   | ACROLEIN + NO3 ----> NO3  |   1.1500E-15 |   1.1500E-15 |
| T20   | ACROLEIN ----> | ACRO_09 | Not Available<sup>1</sup> | 
| TCL5   | ACROLEIN + CL ----> CL  |   2.3700E-10 |   2.3700E-10 |
| T21   | TOLU + OH ----> OH  |   1.80E-12e<sup>   340.00/T</sup> |   5.6302E-12 |
| TCL6   | TOLU + CL ----> CL  |   6.1000E-11 |   6.1000E-11 |
| HG1   | HG + O3 ---->   0.500\*HGIIAER +    0.500\*HGIIGAS + O3  |   2.11E-18e<sup> -1256.50/T</sup> |   3.1191E-20 |
| HG2   | HG + CL2 ----> HGIIGAS + CL2  |   2.6000E-18 |   2.6000E-18 |
| HG3   | HG + H2O2 ----> HGIIGAS + H2O2  |   8.5000E-19 |   8.5000E-19 |
| HG4   | HG + OH ---->   0.500\*HGIIAER +    0.500\*HGIIGAS + OH  |   7.7000E-14 |   7.7000E-14 |
| HG5   | HG + CL + M ---->   0.500\*HG +    0.500\*HGIIGAS + CL  |   2.25E-33e<sup>   680.00/T</sup> |   2.2014E-32 |

<sup>0</sup>Units molecules/(sec*cm<sup>3</sup>); Value at 298.15 K;   2.4615E+19 molcules/cm<sup>3</sup>;   1.00 Atm.     
<sup>1</sup>Photolysis Reaction;depends on radiation and predicted concentrations     
<sup>2</sup>Heteorogeneous Reaction;Depends predicted concentrations                
<sup>4</sup>Set to zero if sun is below the horizon. SEAWATER equals surface fraction covered by ice free open ocean plus surf zones. P equals air pressure in atmospheres.         
<sup>7</sup>Rate constant multiple of constant for listed reaction   
