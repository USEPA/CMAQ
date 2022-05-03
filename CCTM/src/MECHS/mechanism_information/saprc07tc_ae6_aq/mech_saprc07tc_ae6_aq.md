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
| 1   | NO2 ----> NO + O3P  | NO2_06 | Not Available<sup>1</sup> | 
| 2   | O3P + O2 + M ----> O3  |   5.68E-34(T/300)<sup> -2.60</sup> |   5.7721E-34 |
| 3   | O3P + O3 ----> |   8.00E-12e<sup> -2060.00/T</sup> |   7.9879E-15 |
| 4   | O3P + NO ----> NO2  | k<sub>o</sub>=  9.00E-32e<sup>     0.0/T</sup>(T/300)<sup> -1.50</sup><br>k<sub>i</sub> =   3.00E-11e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.00;F=     0.60 |   1.6618E-12 |
| 5   | O3P + NO2 ----> NO  |   5.50E-12e<sup>   188.00/T</sup> |   1.0333E-11 |
| 6   | O3P + NO2 ----> NO3  | k<sub>o</sub>=  2.50E-31e<sup>     0.0/T</sup>(T/300)<sup> -1.80</sup><br>k<sub>i</sub> =   2.20E-11e<sup>     0.0/T</sup>(T/300)<sup> -0.70</sup><br>n=     1.00;F=     0.60 |   3.2805E-12 |
| 7   | O3 + NO ----> NO2  |   3.00E-12e<sup> -1500.00/T</sup> |   1.9596E-14 |
| 8   | O3 + NO2 ----> NO3  |   1.40E-13e<sup> -2470.00/T</sup> |   3.5339E-17 |
| 9   | NO + NO3 ---->   2.000\*NO2  |   1.80E-11e<sup>   110.00/T</sup> |   2.6032E-11 |
| 10   | NO + NO + O2 ---->   2.000\*NO2  |   3.30E-39e<sup>   530.00/T</sup> |   1.9522E-38 |
| 11   | NO2 + NO3 ----> N2O5  | k<sub>o</sub>=  3.60E-30e<sup>     0.0/T</sup>(T/300)<sup> -4.10</sup><br>k<sub>i</sub> =   1.90E-12e<sup>     0.0/T</sup>(T/300)<sup>  0.20</sup><br>n=     1.33;F=     0.35 |   1.2406E-12 |
| 12   | N2O5 ----> NO2 + NO3  | k<sub>o</sub>=  1.30E-03e<sup>-11000.0/T</sup>(T/300)<sup> -3.50</sup><br>k<sub>i</sub> =   9.70E+14e<sup>-11080.0/T</sup>(T/300)<sup>  0.10</sup><br>n=     1.33;F=     0.35 |   4.5396E-02 |
| 13   | N2O5 + H2O ---->   2.000\*HNO3  |   1.0000E-22 |   1.0000E-22 |
| 14   | N2O5 + H2O + H2O ---->   2.000\*HNO3  |   0.0000E+00 |   0.0000E+00 |
| 15   | NO2 + NO3 ----> NO + NO2  |   4.50E-14e<sup> -1260.00/T</sup> |   6.5744E-16 |
| 16   | NO3 ----> NO  | NO3NO_06 | Not Available<sup>1</sup> | 
| 17   | NO3 ----> NO2 + O3P  | NO3NO2_6 | Not Available<sup>1</sup> | 
| 18   | O3 ----> O1D  | O3O1D_06 | Not Available<sup>1</sup> | 
| 19   | O3 ----> O3P  | O3O3P_06 | Not Available<sup>1</sup> | 
| 20   | O1D + H2O ---->   2.000\*OH  |   1.63E-10e<sup>    60.00/T</sup> |   1.9934E-10 |
| 21   | O1D + M ----> O3P  |   2.38E-11e<sup>    96.00/T</sup> |   3.2841E-11 |
| 22   | OH + NO ----> HONO  | k<sub>o</sub>=  7.00E-31e<sup>     0.0/T</sup>(T/300)<sup> -2.60</sup><br>k<sub>i</sub> =   3.60E-11e<sup>     0.0/T</sup>(T/300)<sup> -0.10</sup><br>n=     1.00;F=     0.60 |   7.3998E-12 |
| 23   | HONO ----> OH + NO  | HONO_06 | Not Available<sup>1</sup> | 
| 24   | OH + HONO ----> NO2  |   2.50E-12e<sup>   260.00/T</sup> |   5.9795E-12 |
| 25   | OH + NO2 ----> HNO3  | k<sub>o</sub>=  3.20E-30e<sup>     0.0/T</sup>(T/300)<sup> -4.50</sup><br>k<sub>i</sub> =   3.00E-11e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.24;F=     0.41 |   9.8821E-12 |
| 26   | OH + NO3 ----> HO2 + NO2  |   2.0000E-11 |   2.0000E-11 |
| 27   | OH + HNO3 ----> NO3  | k<sub>0</sub>=  2.40E-14e<sup>   460.0/T</sup><br>k<sub>1</sub>=  2.70E-17e<sup>  2199.0/T</sup><br>k<sub>3</sub>=  6.50E-34e<sup>  1335.0/T</sup> |   1.5409E-13 |
| 28   | HNO3 ----> OH + NO2  | HNO3 | Not Available<sup>1</sup> | 
| 29   | OH + CO ----> HO2 + CO2  | k<sub>0</sub>=  1.44E-13e<sup>     0.0/T</sup><br>k<sub>1</sub>=  3.43E-33e<sup>     0.0/T</sup> |   2.2843E-13 |
| 30   | OH + O3 ----> HO2  |   1.70E-12e<sup>  -940.00/T</sup> |   7.2647E-14 |
| 31   | HO2 + NO ----> OH + NO2  |   3.60E-12e<sup>   270.00/T</sup> |   8.9042E-12 |
| 32   | HO2 + NO2 ----> HNO4  | k<sub>o</sub>=  2.00E-31e<sup>     0.0/T</sup>(T/300)<sup> -3.40</sup><br>k<sub>i</sub> =   2.90E-12e<sup>     0.0/T</sup>(T/300)<sup> -1.10</sup><br>n=     1.00;F=     0.60 |   1.1385E-12 |
| 33   | HNO4 ----> HO2 + NO2  | k<sub>o</sub>=  3.72E-05e<sup>-10650.0/T</sup>(T/300)<sup> -2.40</sup><br>k<sub>i</sub> =   5.42E+15e<sup>-11170.0/T</sup>(T/300)<sup> -2.30</sup><br>n=     1.00;F=     0.60 |   8.6986E-02 |
| 34   | HNO4 ---->   0.610\*HO2 +    0.610\*NO2 +    0.390\*OH +    0.390\*NO3  | HNO4_06 | Not Available<sup>1</sup> | 
| 35   | HNO4 + OH ----> NO2  |   1.30E-12e<sup>   380.00/T</sup> |   4.6501E-12 |
| 36   | HO2 + O3 ----> OH  |   2.03E-16e<sup>   693.00/T</sup>(T/300)<sup>  4.57 </sup> |   2.0168E-15 |
| 37   | HO2 + HO2 ----> HO2H  | k<sub>0</sub>=  2.20E-13e<sup>   600.0/T</sup><br>k<sub>1</sub>=  1.90E-33e<sup>   980.0/T</sup> |   2.8975E-12 |
| 38   | HO2 + HO2 + H2O ----> HO2H  | k<sub>0</sub>=  3.08E-34e<sup>  2800.0/T</sup><br>k<sub>1</sub>=  2.66E-54e<sup>  3180.0/T</sup> |   6.4973E-30 |
| 39   | NO3 + HO2 ---->   0.800\*OH +    0.800\*NO2 +    0.200\*HNO3  |   4.0000E-12 |   4.0000E-12 |
| 40   | NO3 + NO3 ---->   2.000\*NO2  |   8.50E-13e<sup> -2450.00/T</sup> |   2.2944E-16 |
| 41   | HO2H ---->   2.000\*OH  | H2O2 | Not Available<sup>1</sup> | 
| 42   | HO2H + OH ----> HO2  |   1.8000E-12 |   1.8000E-12 |
| 43   | OH + HO2 ----> |   4.80E-11e<sup>   250.00/T</sup> |   1.1102E-10 |
| 44   | OH + SO2 ----> HO2 + SULF + SULRXN  | k<sub>o</sub>=  3.30E-31e<sup>     0.0/T</sup>(T/300)<sup> -4.30</sup><br>k<sub>i</sub> =   1.60E-12e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.00;F=     0.60 |   9.5810E-13 |
| 45   | OH + H2 ----> HO2  |   7.70E-12e<sup> -2100.00/T</sup> |   6.7230E-15 |
| BR01   | MEO2 + NO ----> NO2 + HCHO + HO2  |   2.30E-12e<sup>   360.00/T</sup> |   7.6933E-12 |
| BR02   | MEO2 + HO2 ----> COOH  |   3.46E-13e<sup>   780.00/T</sup>(T/300)<sup>  0.36 </sup> |   4.7237E-12 |
| BR03   | MEO2 + HO2 ----> HCHO  |   3.34E-14e<sup>   780.00/T</sup>(T/300)<sup> -3.53 </sup> |   4.6709E-13 |
| BR04   | MEO2 + NO3 ----> HCHO + HO2 + NO2  |   1.3000E-12 |   1.3000E-12 |
| BR05   | MEO2 + MEO2 ----> MEOH + HCHO  |   6.39E-14e<sup>   365.00/T</sup>(T/300)<sup> -1.80 </sup> |   2.1979E-13 |
| BR06   | MEO2 + MEO2 ---->   2.000\*HCHO +    2.000\*HO2  |   7.40E-13e<sup>  -520.00/T</sup> |   1.2936E-13 |
| BR07   | RO2C + NO ----> NO2  |   2.60E-12e<sup>   380.00/T</sup> |   9.3002E-12 |
| BR08   | RO2C + HO2 ----> |   3.80E-13e<sup>   900.00/T</sup> |   7.7759E-12 |
| BR09   | RO2C + NO3 ----> NO2  |   2.3000E-12 |   2.3000E-12 |
| BR10   | RO2C + MEO2 ---->   0.500\*HO2 +    0.750\*HCHO +    0.250\*MEOH  |   2.0000E-13 |   2.0000E-13 |
| BR11   | RO2C + RO2C ----> |   3.5000E-14 |   3.5000E-14 |
| BR12   | RO2XC + NO ----> |   BR07 |   9.3002E-12<sup>7</sup>| 
| BR13   | RO2XC + HO2 ----> |   BR08 |   7.7759E-12<sup>7</sup>| 
| BR14   | RO2XC + NO3 ----> NO2  |   BR09 |   2.3000E-12<sup>7</sup>| 
| BR15   | RO2XC + MEO2 ---->   0.500\*HO2 +    0.750\*HCHO +    0.250\*MEOH  |   BR10 |   2.0000E-13<sup>7</sup>| 
| BR16   | RO2XC + RO2C ----> |   BR11 |   3.5000E-14<sup>7</sup>| 
| BR17   | RO2XC + RO2XC ----> |   BR11 |   3.5000E-14<sup>7</sup>| 
| BR18   | MECO3 + NO2 ----> PAN  | k<sub>o</sub>=  2.70E-28e<sup>     0.0/T</sup>(T/300)<sup> -7.10</sup><br>k<sub>i</sub> =   1.21E-11e<sup>     0.0/T</sup>(T/300)<sup> -0.90</sup><br>n=     1.41;F=     0.30 |   9.4624E-12 |
| BR19   | PAN ----> MECO3 + NO2  | k<sub>o</sub>=  4.90E-03e<sup>-12100.0/T</sup>(T/300)<sup>  0.00</sup><br>k<sub>i</sub> =   4.00E+16e<sup>-13600.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.41;F=     0.30 |   4.7483E-04 |
| BR20   | PAN ---->   0.600\*MECO3 +    0.600\*NO2 +    0.400\*MEO2 +    0.400\*CO2 +    0.400\*NO3  | PAN | Not Available<sup>1</sup> | 
| BR21   | MECO3 + NO ----> MEO2 + CO2 + NO2  |   7.50E-12e<sup>   290.00/T</sup> |   1.9837E-11 |
| BR22   | MECO3 + HO2 ---->   0.105\*CCOOOH +    0.045\*CCOOH +    0.150\*O3 +    0.440\*OH +    0.440\*MEO2 +    0.440\*CO2  |   5.20E-13e<sup>   980.00/T</sup> |   1.3916E-11 |
| BR23   | MECO3 + NO3 ----> MEO2 + CO2 + NO2  |   BR09 |   2.3000E-12<sup>7</sup>| 
| BR24   | MECO3 + MEO2 ---->   0.100\*CCOOH + HCHO +    0.900\*HO2 +    0.900\*MEO2 +    0.900\*CO2  |   2.00E-12e<sup>   500.00/T</sup> |   1.0699E-11 |
| BR25   | MECO3 + RO2C ----> MEO2 + CO2  |   4.40E-13e<sup>  1070.00/T</sup> |   1.5924E-11 |
| BR26   | MECO3 + RO2XC ----> MEO2 + CO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BR27   | MECO3 + MECO3 ---->   2.000\*MEO2 +    2.000\*CO2  |   2.90E-12e<sup>   500.00/T</sup> |   1.5514E-11 |
| BR28   | RCO3 + NO2 ----> PAN2  |   1.21E-11e<sup>     0.00/T</sup>(T/300)<sup> -1.07 </sup> |   1.2180E-11 |
| BR29   | PAN2 ----> RCO3 + NO2  |   8.30E+16e<sup>-13940.00/T</sup> |   4.1081E-04 |
| BR30   | PAN2 ---->   0.600\*RCO3 +    0.600\*NO2 +    0.400\*RO2C +    0.400\*xHO2 +    0.400\*yROOH +    0.400\*xCCHO +    0.400\*CO2 +    0.400\*NO3  | PAN | Not Available<sup>1</sup> | 
| BR31   | RCO3 + NO ----> NO2 + RO2C + xHO2 + yROOH + xCCHO + CO2  |   6.70E-12e<sup>   340.00/T</sup> |   2.0957E-11 |
| BR32   | RCO3 + HO2 ---->   0.307\*RCOOOH +    0.102\*RCOOH +    0.150\*O3 +    0.440\*OH +    0.440\*xHO2 +    0.440\*RO2C +    0.440\*CO2 +    0.440\*xCCHO +    0.440\*yROOH  |   BR22 |   1.3916E-11<sup>7</sup>| 
| BR33   | RCO3 + NO3 ----> NO2 + RO2C + xHO2 + yROOH + xCCHO + CO2  |   BR09 |   2.3000E-12<sup>7</sup>| 
| BR34   | RCO3 + MEO2 ----> HCHO + HO2 + RO2C + xHO2 + xCCHO + yROOH + CO2  |   BR24 |   1.0699E-11<sup>7</sup>| 
| BR35   | RCO3 + RO2C ----> RO2C + xHO2 + xCCHO + yROOH + CO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BR36   | RCO3 + RO2XC ----> RO2C + xHO2 + xCCHO + yROOH + CO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BR37   | RCO3 + MECO3 ---->   2.000\*CO2 + MEO2 + RO2C + xHO2 + yROOH + xCCHO  |   BR27 |   1.5514E-11<sup>7</sup>| 
| BR38   | RCO3 + RCO3 ---->   2.000\*RO2C +    2.000\*xHO2 +    2.000\*xCCHO +    2.000\*yROOH +    2.000\*CO2  |   BR27 |   1.5514E-11<sup>7</sup>| 
| BR39   | BZCO3 + NO2 ----> PBZN  |   1.3700E-11 |   1.3700E-11 |
| BR40   | PBZN ----> BZCO3 + NO2  |   7.90E+16e<sup>-14000.00/T</sup> |   3.1974E-04 |
| BR41   | PBZN ---->   0.600\*BZCO3 +    0.600\*NO2 +    0.400\*CO2 +    0.400\*BZO +    0.400\*RO2C +    0.400\*NO3  | PAN | Not Available<sup>1</sup> | 
| BR42   | BZCO3 + NO ----> NO2 + CO2 + BZO + RO2C  |   BR31 |   2.0957E-11<sup>7</sup>| 
| BR43   | BZCO3 + HO2 ---->   0.307\*RCOOOH +    0.102\*RCOOH +    0.150\*O3 +    0.440\*OH +    0.440\*BZO +    0.440\*RO2C +    0.440\*CO2  |   BR22 |   1.3916E-11<sup>7</sup>| 
| BR44   | BZCO3 + NO3 ----> NO2 + CO2 + BZO + RO2C  |   BR09 |   2.3000E-12<sup>7</sup>| 
| BR45   | BZCO3 + MEO2 ----> HCHO + HO2 + RO2C + BZO + CO2  |   BR24 |   1.0699E-11<sup>7</sup>| 
| BR46   | BZCO3 + RO2C ----> RO2C + BZO + CO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BR47   | BZCO3 + RO2XC ----> RO2C + BZO + CO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BR48   | BZCO3 + MECO3 ---->   2.000\*CO2 + MEO2 + BZO + RO2C  |   BR27 |   1.5514E-11<sup>7</sup>| 
| BR49   | BZCO3 + RCO3 ---->   2.000\*CO2 +    2.000\*RO2C + xHO2 + yROOH + xCCHO + BZO  |   BR27 |   1.5514E-11<sup>7</sup>| 
| BR50   | BZCO3 + BZCO3 ---->   2.000\*BZO +    2.000\*RO2C +    2.000\*CO2  |   BR27 |   1.5514E-11<sup>7</sup>| 
| BR51   | MACO3 + NO2 ----> MAPAN  |   BR28 |   1.2180E-11<sup>7</sup>| 
| BR52   | MAPAN ----> MACO3 + NO2  |   1.60E+16e<sup>-13486.00/T</sup> |   3.6308E-04 |
| BR53   | MAPAN ---->   0.600\*MACO3 +    0.600\*NO2 +    0.400\*CO2 +    0.400\*HCHO +    0.400\*MECO3 +    0.400\*NO3  | PAN | Not Available<sup>1</sup> | 
| BR54   | MACO3 + NO ----> NO2 + CO2 + HCHO + MECO3  |   BR31 |   2.0957E-11<sup>7</sup>| 
| BR55   | MACO3 + HO2 ---->   0.307\*RCOOOH +    0.102\*RCOOH +    0.150\*O3 +    0.440\*OH +    0.440\*HCHO +    0.440\*MECO3 +    0.440\*CO2  |   BR22 |   1.3916E-11<sup>7</sup>| 
| BR56   | MACO3 + NO3 ----> NO2 + CO2 + HCHO + MECO3  |   BR09 |   2.3000E-12<sup>7</sup>| 
| BR57   | MACO3 + MEO2 ---->   2.000\*HCHO + HO2 + CO2 + MECO3  |   BR24 |   1.0699E-11<sup>7</sup>| 
| BR58   | MACO3 + RO2C ----> CO2 + HCHO + MECO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BR59   | MACO3 + RO2XC ----> CO2 + HCHO + MECO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BR60   | MACO3 + MECO3 ---->   2.000\*CO2 + MEO2 + HCHO + MECO3  |   BR27 |   1.5514E-11<sup>7</sup>| 
| BR61   | MACO3 + RCO3 ----> HCHO + MECO3 + RO2C + xHO2 + yROOH + xCCHO +    2.000\*CO2  |   BR27 |   1.5514E-11<sup>7</sup>| 
| BR62   | MACO3 + BZCO3 ----> HCHO + MECO3 + BZO + RO2C +    2.000\*CO2  |   BR27 |   1.5514E-11<sup>7</sup>| 
| BR63   | MACO3 + MACO3 ---->   2.000\*HCHO +    2.000\*MECO3 +    2.000\*CO2  |   BR27 |   1.5514E-11<sup>7</sup>| 
| BR64   | TBUO + NO2 ----> RNO3  |   2.4000E-11 |   2.4000E-11 |
| BR65   | TBUO ----> ACETONE + MEO2  |   7.50E+14e<sup> -8152.00/T</sup> |   1.0014E+03 |
| BR66   | BZO + NO2 ----> NPHE  |   2.30E-11e<sup>   150.00/T</sup> |   3.8038E-11 |
| BR67   | BZO + HO2 ----> CRES  |   BR08 |   7.7759E-12<sup>7</sup>| 
| BR68   | BZO ----> CRES + RO2C + xHO2  |   1.0000E-03 |   1.0000E-03 |
| R019   | xHO2 + NO ----> NO + HO2  |   BR07 |   9.3002E-12<sup>7</sup>| 
| R020   | xHO2 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| R021   | xHO2 + NO3 ----> NO3 + HO2  |   BR09 |   2.3000E-12<sup>7</sup>| 
| R022   | xHO2 + MEO2 ----> MEO2 +    0.500\*HO2  |   BR10 |   2.0000E-13<sup>7</sup>| 
| R023   | xHO2 + RO2C ----> RO2C +    0.500\*HO2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R024   | xHO2 + RO2XC ----> RO2XC +    0.500\*HO2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R025   | xHO2 + MECO3 ----> MECO3 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R026   | xHO2 + RCO3 ----> RCO3 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R027   | xHO2 + BZCO3 ----> BZCO3 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R028   | xHO2 + MACO3 ----> MACO3 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R029   | xOH + NO ----> NO + OH  |   BR07 |   9.3002E-12<sup>7</sup>| 
| R030   | xOH + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| R031   | xOH + NO3 ----> NO3 + OH  |   BR09 |   2.3000E-12<sup>7</sup>| 
| R032   | xOH + MEO2 ----> MEO2 +    0.500\*OH  |   BR10 |   2.0000E-13<sup>7</sup>| 
| R033   | xOH + RO2C ----> RO2C +    0.500\*OH  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R034   | xOH + RO2XC ----> RO2XC +    0.500\*OH  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R035   | xOH + MECO3 ----> MECO3 + OH  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R036   | xOH + RCO3 ----> RCO3 + OH  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R037   | xOH + BZCO3 ----> BZCO3 + OH  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R038   | xOH + MACO3 ----> MACO3 + OH  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R039   | xNO2 + NO ----> NO + NO2  |   BR07 |   9.3002E-12<sup>7</sup>| 
| R040   | xNO2 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| R041   | xNO2 + NO3 ----> NO3 + NO2  |   BR09 |   2.3000E-12<sup>7</sup>| 
| R042   | xNO2 + MEO2 ----> MEO2 +    0.500\*NO2  |   BR10 |   2.0000E-13<sup>7</sup>| 
| R043   | xNO2 + RO2C ----> RO2C +    0.500\*NO2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R044   | xNO2 + RO2XC ----> RO2XC +    0.500\*NO2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R045   | xNO2 + MECO3 ----> MECO3 + NO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R046   | xNO2 + RCO3 ----> RCO3 + NO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R047   | xNO2 + BZCO3 ----> BZCO3 + NO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R048   | xNO2 + MACO3 ----> MACO3 + NO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R049   | xMEO2 + NO ----> NO + MEO2  |   BR07 |   9.3002E-12<sup>7</sup>| 
| R050   | xMEO2 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| R051   | xMEO2 + NO3 ----> NO3 + MEO2  |   BR09 |   2.3000E-12<sup>7</sup>| 
| R052   | xMEO2 + MEO2 ---->   1.500\*MEO2  |   BR10 |   2.0000E-13<sup>7</sup>| 
| R053   | xMEO2 + RO2C ----> RO2C +    0.500\*MEO2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R054   | xMEO2 + RO2XC ----> RO2XC +    0.500\*MEO2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R055   | xMEO2 + MECO3 ----> MECO3 + MEO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R056   | xMEO2 + RCO3 ----> RCO3 + MEO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R057   | xMEO2 + BZCO3 ----> BZCO3 + MEO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R058   | xMEO2 + MACO3 ----> MACO3 + MEO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R059   | xMECO3 + NO ----> NO + MECO3  |   BR07 |   9.3002E-12<sup>7</sup>| 
| R060   | xMECO3 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| R061   | xMECO3 + NO3 ----> NO3 + MECO3  |   BR09 |   2.3000E-12<sup>7</sup>| 
| R062   | xMECO3 + MEO2 ----> MEO2 +    0.500\*MECO3  |   BR10 |   2.0000E-13<sup>7</sup>| 
| R063   | xMECO3 + RO2C ----> RO2C +    0.500\*MECO3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R064   | xMECO3 + RO2XC ----> RO2XC +    0.500\*MECO3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R065   | xMECO3 + MECO3 ---->   2.000\*MECO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R066   | xMECO3 + RCO3 ----> RCO3 + MECO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R067   | xMECO3 + BZCO3 ----> BZCO3 + MECO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R068   | xMECO3 + MACO3 ----> MACO3 + MECO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R069   | xRCO3 + NO ----> NO + RCO3  |   BR07 |   9.3002E-12<sup>7</sup>| 
| R070   | xRCO3 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| R071   | xRCO3 + NO3 ----> NO3 + RCO3  |   BR09 |   2.3000E-12<sup>7</sup>| 
| R072   | xRCO3 + MEO2 ----> MEO2 +    0.500\*RCO3  |   BR10 |   2.0000E-13<sup>7</sup>| 
| R073   | xRCO3 + RO2C ----> RO2C +    0.500\*RCO3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R074   | xRCO3 + RO2XC ----> RO2XC +    0.500\*RCO3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R075   | xRCO3 + MECO3 ----> MECO3 + RCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R076   | xRCO3 + RCO3 ---->   2.000\*RCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R077   | xRCO3 + BZCO3 ----> BZCO3 + RCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R078   | xRCO3 + MACO3 ----> MACO3 + RCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R079   | xMACO3 + NO ----> NO + MACO3  |   BR07 |   9.3002E-12<sup>7</sup>| 
| R080   | xMACO3 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| R081   | xMACO3 + NO3 ----> NO3 + MACO3  |   BR09 |   2.3000E-12<sup>7</sup>| 
| R082   | xMACO3 + MEO2 ----> MEO2 +    0.500\*MACO3  |   BR10 |   2.0000E-13<sup>7</sup>| 
| R083   | xMACO3 + RO2C ----> RO2C +    0.500\*MACO3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R084   | xMACO3 + RO2XC ----> RO2XC +    0.500\*MACO3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R085   | xMACO3 + MECO3 ----> MECO3 + MACO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R086   | xMACO3 + RCO3 ----> RCO3 + MACO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R087   | xMACO3 + BZCO3 ----> BZCO3 + MACO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R088   | xMACO3 + MACO3 ---->   2.000\*MACO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R089   | xTBUO + NO ----> NO + TBUO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| R090   | xTBUO + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| R091   | xTBUO + NO3 ----> NO3 + TBUO  |   BR09 |   2.3000E-12<sup>7</sup>| 
| R092   | xTBUO + MEO2 ----> MEO2 +    0.500\*TBUO  |   BR10 |   2.0000E-13<sup>7</sup>| 
| R093   | xTBUO + RO2C ----> RO2C +    0.500\*TBUO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R094   | xTBUO + RO2XC ----> RO2XC +    0.500\*TBUO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R095   | xTBUO + MECO3 ----> MECO3 + TBUO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R096   | xTBUO + RCO3 ----> RCO3 + TBUO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R097   | xTBUO + BZCO3 ----> BZCO3 + TBUO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R098   | xTBUO + MACO3 ----> MACO3 + TBUO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R099   | xCO + NO ----> NO + CO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| R100   | xCO + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| R101   | xCO + NO3 ----> NO3 + CO  |   BR09 |   2.3000E-12<sup>7</sup>| 
| R102   | xCO + MEO2 ----> MEO2 +    0.500\*CO  |   BR10 |   2.0000E-13<sup>7</sup>| 
| R103   | xCO + RO2C ----> RO2C +    0.500\*CO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R104   | xCO + RO2XC ----> RO2XC +    0.500\*CO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| R105   | xCO + MECO3 ----> MECO3 + CO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R106   | xCO + RCO3 ----> RCO3 + CO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R107   | xCO + BZCO3 ----> BZCO3 + CO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| R108   | xCO + MACO3 ----> MACO3 + CO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BP01   | HCHO ---->   2.000\*HO2 + CO  | HCHOR_06 | Not Available<sup>1</sup> | 
| BP02   | HCHO ----> CO  | HCHOM_06 | Not Available<sup>1</sup> | 
| BP03   | HCHO + OH ----> HO2 + CO  |   5.40E-12e<sup>   135.00/T</sup> |   8.4926E-12 |
| BP07   | HCHO + NO3 ----> HNO3 + HO2 + CO  |   2.00E-12e<sup> -2431.00/T</sup> |   5.7539E-16 |
| BP08   | CCHO + OH ----> MECO3  |   4.40E-12e<sup>   365.00/T</sup> |   1.4967E-11 |
| BP09   | CCHO ----> CO + HO2 + MEO2  | CCHO_R | Not Available<sup>1</sup> | 
| BP10   | CCHO + NO3 ----> HNO3 + MECO3  |   1.40E-12e<sup> -1860.00/T</sup> |   2.7340E-15 |
| BP11   | RCHO + OH ---->   0.965\*RCO3 +    0.035\*RO2C +    0.035\*xHO2 +    0.035\*xCO +    0.035\*xCCHO +    0.035\*yROOH  |   5.10E-12e<sup>   405.00/T</sup> |   1.9838E-11 |
| BP12   | RCHO ----> RO2C + xHO2 + yROOH + xCCHO + CO + HO2  | C2CHO | Not Available<sup>1</sup> | 
| BP13   | RCHO + NO3 ----> HNO3 + RCO3  |   1.40E-12e<sup> -1601.00/T</sup> |   6.5172E-15 |
| BP14   | ACETONE + OH ----> RO2C + xMECO3 + xHCHO + yROOH  |   4.56E-14e<sup>   429.00/T</sup>(T/300)<sup>  3.65 </sup> |   1.8796E-13 |
| BP15   | ACETONE ---->   0.620\*MECO3 +    1.380\*MEO2 +    0.380\*CO  |   5.0000E-01\*ACET_06 | Not Available<sup>1</sup> | 
| BP16   | MEK + OH ---->   0.967\*RO2C +    0.039\*RO2XC +    0.039\*zRNO3 +    0.376\*xHO2 +    0.510\*xMECO3 +    0.074\*xRCO3 +    0.088\*xHCHO +    0.504\*xCCHO +    0.376\*xRCHO + yROOH  |   1.30E-12e<sup>   -25.00/T</sup>(T/300)<sup>  2.00 </sup> |   1.1807E-12 |
| BP17   | MEK ----> MECO3 + RO2C + xHO2 + xCCHO + yROOH  |   1.7500E-01\*MEK_06 | Not Available<sup>1</sup> | 
| BP18   | MEOH + OH ----> HCHO + HO2  |   2.85E-12e<sup>  -345.00/T</sup> |   8.9600E-13 |
| BP19   | HCOOH + OH ----> HO2 + CO2  |   4.5000E-13 |   4.5000E-13 |
| BP20   | CCOOH + OH ---->   0.509\*MEO2 +    0.491\*RO2C +    0.509\*CO2 +    0.491\*xHO2 +    0.491\*xMGLY +    0.491\*yROOH  |   4.20E-14e<sup>   855.00/T</sup> |   7.3904E-13 |
| BP21   | RCOOH + OH ----> RO2C + xHO2 +    0.143\*CO2 +    0.142\*xCCHO +    0.400\*xRCHO +    0.457\*xBACL + yROOH  |   1.2000E-12 |   1.2000E-12 |
| BP22   | COOH + OH ---->   0.300\*HCHO +    0.300\*OH +    0.700\*MEO2  |   3.80E-12e<sup>   200.00/T</sup> |   7.4321E-12 |
| BP23   | COOH ----> HCHO + HO2 + OH  | COOH | Not Available<sup>1</sup> | 
| BP24   | ROOH + OH ---->   0.744\*OH +    0.251\*RO2C +    0.004\*RO2XC +    0.004\*zRNO3 +    0.744\*RCHO +    0.239\*xHO2 +    0.012\*xOH +    0.012\*xHCHO +    0.012\*xCCHO +    0.205\*xRCHO +    0.034\*xPROD2 +    0.256\*yROOH  |   2.5000E-11 |   2.5000E-11 |
| BP25   | ROOH ----> RCHO + HO2 + OH  | COOH | Not Available<sup>1</sup> | 
| BP26   | R6OOH + OH ---->   0.840\*OH +    0.222\*RO2C +    0.029\*RO2XC +    0.029\*zRNO3 +    0.840\*PRD2 +    0.090\*xHO2 +    0.041\*xOH +    0.020\*xCCHO +    0.075\*xRCHO +    0.084\*xPROD2 +    0.160\*yROOH  |   5.6000E-11 |   5.6000E-11 |
| BP27   | R6OOH ----> OH +    0.142\*HO2 +    0.782\*RO2C +    0.077\*RO2XC +    0.077\*zRNO3 +    0.085\*RCHO +    0.142\*PRD2 +    0.782\*xHO2 +    0.026\*xCCHO +    0.058\*xRCHO +    0.698\*xPROD2 +    0.858\*yR6OOH  | COOH | Not Available<sup>1</sup> | 
| BP28   | RAOOH + OH ---->   0.139\*OH +    0.148\*HO2 +    0.589\*RO2C +    0.124\*RO2XC +    0.124\*zRNO3 +    0.074\*PRD2 +    0.147\*MGLY +    0.139\*IPRD +    0.565\*xHO2 +    0.024\*xOH +    0.448\*xRCHO +    0.026\*xGLY +    0.030\*xMEK +    0.252\*xMGLY +    0.073\*xAFG1 +    0.073\*xAFG2 +    0.713\*yR6OOH  |   1.4100E-10 |   1.4100E-10 |
| BP29   | RAOOH ----> OH + HO2 +    0.500\*GLY +    0.500\*MGLY +    0.500\*AFG1 +    0.500\*AFG2  | COOH | Not Available<sup>1</sup> | 
| BP30   | GLY ---->   2.000\*CO +    2.000\*HO2  | GLY_07R | Not Available<sup>1</sup> | 
| BP31   | GLY ----> HCHO + CO  | GLY_07M | Not Available<sup>1</sup> | 
| BP32   | GLY + OH ---->   0.700\*HO2 +    1.400\*CO +    0.300\*HCOCO3  |   3.10E-12e<sup>   342.20/T</sup> |   9.7683E-12 |
| BP33   | GLY + NO3 ----> HNO3 +    0.700\*HO2 +    1.400\*CO +    0.300\*HCOCO3  |   2.80E-12e<sup> -2390.00/T</sup> |   9.2429E-16 |
| BP34   | MGLY ----> HO2 + CO + MECO3  | MGLY_06 | Not Available<sup>1</sup> | 
| BP35   | MGLY + OH ----> CO + MECO3  |   1.5000E-11 |   1.5000E-11 |
| BP36   | MGLY + NO3 ----> HNO3 + CO + MECO3  |   1.40E-12e<sup> -1895.00/T</sup> |   2.4312E-15 |
| BP37   | BACL ---->   2.000\*MECO3  | BACL_07 | Not Available<sup>1</sup> | 
| BP38   | CRES + OH ---->   0.200\*BZO +    0.800\*RO2C +    0.800\*xHO2 +    0.800\*yR6OOH +    0.250\*xMGLY  |   1.70E-12e<sup>   950.00/T</sup> |   4.1138E-11 |
| BP39   | CRES + NO3 ----> HNO3 + BZO  |   1.4000E-11 |   1.4000E-11 |
| BP40   | NPHE + OH ----> BZO  |   3.5000E-12 |   3.5000E-12 |
| BP41   | NPHE ----> HONO  |   1.5000E-03\*NO2_06 | Not Available<sup>1</sup> | 
| BP42   | NPHE ----> |   1.5000E-02\*NO2_06 | Not Available<sup>1</sup> | 
| BP43   | BALD + OH ----> BZCO3  |   1.2000E-11 |   1.2000E-11 |
| BP44   | BALD ----> |   6.0000E-02\*BALD_06 | Not Available<sup>1</sup> | 
| BP45   | BALD + NO3 ----> HNO3 + BZCO3  |   1.34E-12e<sup> -1860.00/T</sup> |   2.6168E-15 |
| BP46   | AFG1 + OH ---->   0.217\*MACO3 +    0.723\*RO2C +    0.060\*RO2XC +    0.060\*zRNO3 +    0.521\*xHO2 +    0.201\*xMECO3 +    0.334\*xCO +    0.407\*xRCHO +    0.129\*xMEK +    0.107\*xGLY +    0.267\*xMGLY +    0.783\*yR6OOH  |   7.4000E-11 |   7.4000E-11 |
| BP47   | AFG1 + O3 ---->   0.826\*OH +    0.522\*HO2 +    0.652\*RO2C +    0.522\*CO +    0.174\*CO2 +    0.432\*GLY +    0.568\*MGLY +    0.652\*xRCO3 +    0.652\*xHCHO +    0.652\*yR6OOH  |   9.6600E-18 |   9.6600E-18 |
| BP48   | AFG1 ---->   1.023\*HO2 +    0.173\*MEO2 +    0.305\*MECO3 +    0.500\*MACO3 +    0.695\*CO +    0.195\*GLY +    0.305\*MGLY  | AFG1 | Not Available<sup>1</sup> | 
| BP49   | AFG2 + OH ---->   0.217\*MACO3 +    0.723\*RO2C +    0.060\*RO2XC +    0.060\*zRNO3 +    0.521\*xHO2 +    0.201\*xMECO3 +    0.334\*xCO +    0.407\*xRCHO +    0.129\*xMEK +    0.107\*xGLY +    0.267\*xMGLY +    0.783\*yR6OOH  |   7.4000E-11 |   7.4000E-11 |
| BP50   | AFG2 + O3 ---->   0.826\*OH +    0.522\*HO2 +    0.652\*RO2C +    0.522\*CO +    0.174\*CO2 +    0.432\*GLY +    0.568\*MGLY +    0.652\*xRCO3 +    0.652\*xHCHO +    0.652\*yR6OOH  |   9.6600E-18 |   9.6600E-18 |
| BP51   | AFG2 ----> PRD2  | AFG1 | Not Available<sup>1</sup> | 
| BP52   | AFG3 + OH ---->   0.206\*MACO3 +    0.733\*RO2C +    0.117\*RO2XC +    0.117\*zRNO3 +    0.561\*xHO2 +    0.117\*xMECO3 +    0.114\*xCO +    0.274\*xGLY +    0.153\*xMGLY +    0.019\*xBACL +    0.195\*xAFG1 +    0.195\*xAFG2 +    0.231\*xIPRD +    0.794\*yR6OOH  |   9.3500E-11 |   9.3500E-11 |
| BP53   | AFG3 + O3 ---->   0.471\*OH +    0.554\*HO2 +    0.013\*MECO3 +    0.258\*RO2C +    0.007\*RO2XC +    0.007\*zRNO3 +    0.580\*CO +    0.190\*CO2 +    0.366\*GLY +    0.184\*MGLY +    0.350\*AFG1 +    0.350\*AFG2 +    0.139\*AFG3 +    0.003\*MACR +    0.004\*MVK +    0.003\*IPRD +    0.095\*xHO2 +    0.163\*xRCO3 +    0.163\*xHCHO +    0.095\*xMGLY +    0.264\*yR6OOH  |   1.4300E-17 |   1.4300E-17 |
| BP54   | MACR + OH ---->   0.500\*MACO3 +    0.500\*RO2C +    0.500\*xHO2 +    0.416\*xCO +    0.084\*xHCHO +    0.416\*xMEK +    0.084\*xMGLY +    0.500\*yROOH  |   8.00E-12e<sup>   380.00/T</sup> |   2.8616E-11 |
| BP55   | MACR + O3 ---->   0.208\*OH +    0.108\*HO2 +    0.100\*RO2C +    0.450\*CO +    0.117\*CO2 +    0.100\*HCHO +    0.900\*MGLY +    0.333\*HCOOH +    0.100\*xRCO3 +    0.100\*xHCHO +    0.100\*yROOH  |   1.40E-15e<sup> -2100.00/T</sup> |   1.2224E-18 |
| BP56   | MACR + NO3 ---->   0.500\*MACO3 +    0.500\*RO2C +    0.500\*HNO3 +    0.500\*xHO2 +    0.500\*xCO +    0.500\*yROOH  |   1.50E-12e<sup> -1815.00/T</sup> |   3.4065E-15 |
| BP57   | MACR + O3P ----> RCHO  |   6.3400E-12 |   6.3400E-12 |
| BP58   | MACR ---->   0.330\*OH +    0.670\*HO2 +    0.340\*MECO3 +    0.330\*MACO3 +    0.330\*RO2C +    0.670\*CO +    0.340\*HCHO +    0.330\*xMECO3 +    0.330\*xHCHO +    0.330\*yROOH  | MACR_06 | Not Available<sup>1</sup> | 
| BP59   | MVK + OH ---->   0.975\*RO2C +    0.025\*RO2XC +    0.025\*zRNO3 +    0.300\*xHO2 +    0.675\*xMECO3 +    0.300\*xHCHO +    0.675\*xHOCCHO +    0.300\*xMGLY + yROOH  |   2.60E-12e<sup>   610.00/T</sup> |   2.0115E-11 |
| BP60   | MVK + O3 ---->   0.164\*OH +    0.064\*HO2 +    0.050\*RO2C +    0.050\*xHO2 +    0.475\*CO +    0.124\*CO2 +    0.050\*HCHO +    0.950\*MGLY +    0.351\*HCOOH +    0.050\*xRCO3 +    0.050\*xHCHO +    0.050\*yROOH  |   8.50E-16e<sup> -1520.00/T</sup> |   5.1921E-18 |
| BP62   | MVK + O3P ---->   0.450\*RCHO +    0.550\*MEK  |   4.3200E-12 |   4.3200E-12 |
| BP63   | MVK ---->   0.400\*MEO2 +    0.600\*CO +    0.600\*PRD2 +    0.400\*MACO3  | MVK_06 | Not Available<sup>1</sup> | 
| BP64   | IPRD + OH ---->   0.289\*MACO3 +    0.670\*RO2C +    0.670\*xHO2 +    0.041\*RO2XC +    0.041\*zRNO3 +    0.336\*xCO +    0.055\*xHCHO +    0.129\*xHOCCHO +    0.013\*xRCHO +    0.150\*xMEK +    0.332\*xPROD2 +    0.150\*xGLY +    0.174\*xMGLY +    0.711\*yR6OOH  |   6.1900E-11 |   6.1900E-11 |
| BP65   | IPRD + O3 ---->   0.285\*OH +    0.400\*HO2 +    0.048\*RO2C +    0.048\*xRCO3 +    0.498\*CO +    0.140\*CO2 +    0.124\*HCHO +    0.210\*MEK +    0.023\*GLY +    0.742\*MGLY +    0.100\*HCOOH +    0.372\*RCOOH +    0.047\*xHOCCHO +    0.001\*xHCHO +    0.048\*yR6OOH  |   4.1800E-18 |   4.1800E-18 |
| BP66   | IPRD + NO3 ---->   0.150\*MACO3 +    0.150\*HNO3 +    0.799\*RO2C +    0.799\*xHO2 +    0.051\*RO2XC +    0.051\*zRNO3 +    0.572\*xCO +    0.227\*xHCHO +    0.218\*xRCHO +    0.008\*xMGLY +    0.572\*xRNO3 +    0.850\*yR6OOH  |   1.0000E-13 |   1.0000E-13 |
| BP67   | IPRD ---->   1.233\*HO2 +    0.467\*MECO3 +    0.300\*RCO3 +    1.233\*CO +    0.300\*HCHO +    0.467\*HOCCHO +    0.233\*MEK  | MACR_06 | Not Available<sup>1</sup> | 
| BP68   | PRD2 + OH ---->   0.472\*HO2 +    0.379\*xHO2 +    0.029\*xMECO3 +    0.049\*xRCO3 +    0.473\*RO2C +    0.071\*RO2XC +    0.071\*zRNO3 +    0.002\*HCHO +    0.211\*xHCHO +    0.001\*CCHO +    0.083\*xCCHO +    0.143\*RCHO +    0.402\*xRCHO +    0.115\*xMEK +    0.329\*PRD2 +    0.007\*xPROD2 +    0.528\*yR6OOH  |   1.5500E-11 |   1.5500E-11 |
| BP69   | PRD2 ---->   0.913\*xHO2 +    0.400\*MECO3 +    0.600\*RCO3 +    1.590\*RO2C +    0.087\*RO2XC +    0.087\*zRNO3 +    0.303\*xHCHO +    0.163\*xCCHO +    0.780\*xRCHO + yR6OOH  |   4.8600E-03\*MEK_06 | Not Available<sup>1</sup> | 
| BP70   | RNO3 + OH ---->   0.189\*HO2 +    0.305\*xHO2 +    0.019\*NO2 +    0.313\*xNO2 +    0.976\*RO2C +    0.175\*RO2XC +    0.175\*zRNO3 +    0.011\*xHCHO +    0.429\*xCCHO +    0.001\*RCHO +    0.036\*xRCHO +    0.004\*xACETONE +    0.010\*MEK +    0.170\*xMEK +    0.008\*PRD2 +    0.031\*xPROD2 +    0.189\*RNO3 +    0.305\*xRNO3 +    0.157\*yROOH +    0.636\*yR6OOH  |   7.2000E-12 |   7.2000E-12 |
| BP71   | RNO3 ---->   0.344\*HO2 +    0.554\*xHO2 + NO2 +    0.721\*RO2C +    0.102\*RO2XC +    0.102\*zRNO3 +    0.074\*HCHO +    0.061\*xHCHO +    0.214\*CCHO +    0.230\*xCCHO +    0.074\*RCHO +    0.063\*xRCHO +    0.008\*xACETONE +    0.124\*MEK +    0.083\*xMEK +    0.190\*PRD2 +    0.261\*xPROD2 +    0.066\*yROOH +    0.591\*yR6OOH  | IC3ONO2 | Not Available<sup>1</sup> | 
| BP72   | HOCCHO + OH ----> MECO3  |   BP08 |   1.4967E-11<sup>7</sup>| 
| BP73   | HOCCHO ----> CO +    2.000\*HO2 + HCHO  | HOCCHO_IUPAC | Not Available<sup>1</sup> | 
| BP74   | HOCCHO + NO3 ----> HNO3 + MECO3  |   BP10 |   2.7340E-15<sup>7</sup>| 
| BP75   | ACROLEIN + OH ---->   0.250\*xHO2 +    0.750\*MACO3 +    0.250\*RO2C +    0.167\*xCO +    0.083\*xHCHO +    0.167\*xCCHO +    0.083\*xGLY +    0.250\*yROOH  |   1.9900E-11 |   1.9900E-11 |
| BP76   | ACROLEIN + O3 ---->   0.830\*HO2 +    0.330\*OH +    1.005\*CO +    0.310\*CO2 +    0.500\*HCHO +    0.185\*HCOOH +    0.500\*GLY  |   1.40E-15e<sup> -2528.00/T</sup> |   2.9091E-19 |
| BP77   | ACROLEIN + NO3 ---->   0.031\*xHO2 +    0.967\*MACO3 +    0.031\*RO2C +    0.002\*RO2XC +    0.002\*zRNO3 +    0.967\*HNO3 +    0.031\*xCO +    0.031\*xRNO3 +    0.033\*yROOH  |   1.1800E-15 |   1.1800E-15 |
| BP78   | ACROLEIN + O3P ----> RCHO  |   2.3700E-12 |   2.3700E-12 |
| BP79   | ACROLEIN ---->   1.066\*HO2 +    0.178\*OH +    0.234\*MEO2 +    0.330\*MACO3 +    1.188\*CO +    0.102\*CO2 +    0.340\*HCHO +    0.050\*CCOOH  | ACRO_09 | Not Available<sup>1</sup> | 
| BP80   | CCOOOH + OH ---->   0.980\*MECO3 +    0.020\*RO2C +    0.020\*CO2 +    0.020\*xOH +    0.020\*xHCHO +    0.020\*yROOH  |   5.2800E-12 |   5.2800E-12 |
| BP81   | CCOOOH ----> MEO2 + CO2 + OH  | PAA | Not Available<sup>1</sup> | 
| BP82   | RCOOOH + OH ---->   0.806\*RCO3 +    0.194\*RO2C +    0.194\*yROOH +    0.110\*CO2 +    0.110\*xOH +    0.110\*xCCHO +    0.084\*xHO2 +    0.084\*xRCHO  |   6.4200E-12 |   6.4200E-12 |
| BP83   | RCOOOH ----> xHO2 + xCCHO + yROOH + CO2 + OH  | PAA | Not Available<sup>1</sup> | 
| BP84   | HCOCO3 + NO ----> HO2 + CO + CO2 + NO2  |   BR31 |   2.0957E-11<sup>7</sup>| 
| BP85   | HCOCO3 + NO2 ----> HO2 + CO + CO2 + NO3  |   BR28 |   1.2180E-11<sup>7</sup>| 
| BP86   | HCOCO3 + HO2 ---->   0.440\*OH +    0.440\*HO2 +    0.440\*CO +    0.440\*CO2 +    0.560\*GLY +    0.150\*O3  |   BR22 |   1.3916E-11<sup>7</sup>| 
| P001   | xHCHO + NO ----> NO + HCHO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P002   | xHCHO + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P003   | xHCHO + NO3 ----> NO3 + HCHO  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P004   | xHCHO + MEO2 ----> MEO2 +    0.500\*HCHO  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P005   | xHCHO + RO2C ----> RO2C +    0.500\*HCHO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P006   | xHCHO + RO2XC ----> RO2XC +    0.500\*HCHO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P007   | xHCHO + MECO3 ----> MECO3 + HCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P008   | xHCHO + RCO3 ----> RCO3 + HCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P009   | xHCHO + BZCO3 ----> BZCO3 + HCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P010   | xHCHO + MACO3 ----> MACO3 + HCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P011   | xCCHO + NO ----> NO + CCHO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P012   | xCCHO + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P013   | xCCHO + NO3 ----> NO3 + CCHO  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P014   | xCCHO + MEO2 ----> MEO2 +    0.500\*CCHO  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P015   | xCCHO + RO2C ----> RO2C +    0.500\*CCHO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P016   | xCCHO + RO2XC ----> RO2XC +    0.500\*CCHO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P017   | xCCHO + MECO3 ----> MECO3 + CCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P018   | xCCHO + RCO3 ----> RCO3 + CCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P019   | xCCHO + BZCO3 ----> BZCO3 + CCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P020   | xCCHO + MACO3 ----> MACO3 + CCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P021   | xRCHO + NO ----> NO + RCHO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P022   | xRCHO + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P023   | xRCHO + NO3 ----> NO3 + RCHO  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P024   | xRCHO + MEO2 ----> MEO2 +    0.500\*RCHO  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P025   | xRCHO + RO2C ----> RO2C +    0.500\*RCHO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P026   | xRCHO + RO2XC ----> RO2XC +    0.500\*RCHO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P027   | xRCHO + MECO3 ----> MECO3 + RCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P028   | xRCHO + RCO3 ----> RCO3 + RCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P029   | xRCHO + BZCO3 ----> BZCO3 + RCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P030   | xRCHO + MACO3 ----> MACO3 + RCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P031   | xACETONE + NO ----> NO + ACETONE  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P032   | xACETONE + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P033   | xACETONE + NO3 ----> NO3 + ACETONE  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P034   | xACETONE + MEO2 ----> MEO2 +    0.500\*ACETONE  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P035   | xACETONE + RO2C ----> RO2C +    0.500\*ACETONE  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P036   | xACETONE + RO2XC ----> RO2XC +    0.500\*ACETONE  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P037   | xACETONE + MECO3 ----> MECO3 + ACETONE  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P038   | xACETONE + RCO3 ----> RCO3 + ACETONE  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P039   | xACETONE + BZCO3 ----> BZCO3 + ACETONE  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P040   | xACETONE + MACO3 ----> MACO3 + ACETONE  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P041   | xMEK + NO ----> NO + MEK  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P042   | xMEK + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P043   | xMEK + NO3 ----> NO3 + MEK  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P044   | xMEK + MEO2 ----> MEO2 +    0.500\*MEK  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P045   | xMEK + RO2C ----> RO2C +    0.500\*MEK  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P046   | xMEK + RO2XC ----> RO2XC +    0.500\*MEK  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P047   | xMEK + MECO3 ----> MECO3 + MEK  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P048   | xMEK + RCO3 ----> RCO3 + MEK  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P049   | xMEK + BZCO3 ----> BZCO3 + MEK  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P050   | xMEK + MACO3 ----> MACO3 + MEK  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P051   | xPROD2 + NO ----> NO + PRD2  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P052   | xPROD2 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P053   | xPROD2 + NO3 ----> NO3 + PRD2  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P054   | xPROD2 + MEO2 ----> MEO2 +    0.500\*PRD2  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P055   | xPROD2 + RO2C ----> RO2C +    0.500\*PRD2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P056   | xPROD2 + RO2XC ----> RO2XC +    0.500\*PRD2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P057   | xPROD2 + MECO3 ----> MECO3 + PRD2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P058   | xPROD2 + RCO3 ----> RCO3 + PRD2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P059   | xPROD2 + BZCO3 ----> BZCO3 + PRD2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P060   | xPROD2 + MACO3 ----> MACO3 + PRD2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P061   | xGLY + NO ----> NO + GLY  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P062   | xGLY + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P063   | xGLY + NO3 ----> NO3 + GLY  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P064   | xGLY + MEO2 ----> MEO2 +    0.500\*GLY  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P065   | xGLY + RO2C ----> RO2C +    0.500\*GLY  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P066   | xGLY + RO2XC ----> RO2XC +    0.500\*GLY  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P067   | xGLY + MECO3 ----> MECO3 + GLY  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P068   | xGLY + RCO3 ----> RCO3 + GLY  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P069   | xGLY + BZCO3 ----> BZCO3 + GLY  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P070   | xGLY + MACO3 ----> MACO3 + GLY  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P071   | xMGLY + NO ----> NO + MGLY  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P072   | xMGLY + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P073   | xMGLY + NO3 ----> NO3 + MGLY  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P074   | xMGLY + MEO2 ----> MEO2 +    0.500\*MGLY  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P075   | xMGLY + RO2C ----> RO2C +    0.500\*MGLY  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P076   | xMGLY + RO2XC ----> RO2XC +    0.500\*MGLY  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P077   | xMGLY + MECO3 ----> MECO3 + MGLY  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P078   | xMGLY + RCO3 ----> RCO3 + MGLY  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P079   | xMGLY + BZCO3 ----> BZCO3 + MGLY  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P080   | xMGLY + MACO3 ----> MACO3 + MGLY  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P081   | xBACL + NO ----> NO + BACL  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P082   | xBACL + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P083   | xBACL + NO3 ----> NO3 + BACL  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P084   | xBACL + MEO2 ----> MEO2 +    0.500\*BACL  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P085   | xBACL + RO2C ----> RO2C +    0.500\*BACL  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P086   | xBACL + RO2XC ----> RO2XC +    0.500\*BACL  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P087   | xBACL + MECO3 ----> MECO3 + BACL  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P088   | xBACL + RCO3 ----> RCO3 + BACL  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P089   | xBACL + BZCO3 ----> BZCO3 + BACL  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P090   | xBACL + MACO3 ----> MACO3 + BACL  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P091   | xBALD + NO ----> NO + BALD  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P092   | xBALD + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P093   | xBALD + NO3 ----> NO3 + BALD  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P094   | xBALD + MEO2 ----> MEO2 +    0.500\*BALD  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P095   | xBALD + RO2C ----> RO2C +    0.500\*BALD  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P096   | xBALD + RO2XC ----> RO2XC +    0.500\*BALD  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P097   | xBALD + MECO3 ----> MECO3 + BALD  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P098   | xBALD + RCO3 ----> RCO3 + BALD  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P099   | xBALD + BZCO3 ----> BZCO3 + BALD  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P100   | xBALD + MACO3 ----> MACO3 + BALD  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P101   | xAFG1 + NO ----> NO + AFG1  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P102   | xAFG1 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P103   | xAFG1 + NO3 ----> NO3 + AFG1  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P104   | xAFG1 + MEO2 ----> MEO2 +    0.500\*AFG1  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P105   | xAFG1 + RO2C ----> RO2C +    0.500\*AFG1  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P106   | xAFG1 + RO2XC ----> RO2XC +    0.500\*AFG1  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P107   | xAFG1 + MECO3 ----> MECO3 + AFG1  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P108   | xAFG1 + RCO3 ----> RCO3 + AFG1  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P109   | xAFG1 + BZCO3 ----> BZCO3 + AFG1  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P110   | xAFG1 + MACO3 ----> MACO3 + AFG1  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P111   | xAFG2 + NO ----> NO + AFG2  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P112   | xAFG2 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P113   | xAFG2 + NO3 ----> NO3 + AFG2  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P114   | xAFG2 + MEO2 ----> MEO2 +    0.500\*AFG2  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P115   | xAFG2 + RO2C ----> RO2C +    0.500\*AFG2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P116   | xAFG2 + RO2XC ----> RO2XC +    0.500\*AFG2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P117   | xAFG2 + MECO3 ----> MECO3 + AFG2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P118   | xAFG2 + RCO3 ----> RCO3 + AFG2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P119   | xAFG2 + BZCO3 ----> BZCO3 + AFG2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P120   | xAFG2 + MACO3 ----> MACO3 + AFG2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P121   | xAFG3 + NO ----> NO + AFG3  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P122   | xAFG3 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P123   | xAFG3 + NO3 ----> NO3 + AFG3  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P124   | xAFG3 + MEO2 ----> MEO2 +    0.500\*AFG3  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P125   | xAFG3 + RO2C ----> RO2C +    0.500\*AFG3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P126   | xAFG3 + RO2XC ----> RO2XC +    0.500\*AFG3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P127   | xAFG3 + MECO3 ----> MECO3 + AFG3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P128   | xAFG3 + RCO3 ----> RCO3 + AFG3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P129   | xAFG3 + BZCO3 ----> BZCO3 + AFG3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P130   | xAFG3 + MACO3 ----> MACO3 + AFG3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P131   | xMACR + NO ----> NO + MACR  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P132   | xMACR + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P133   | xMACR + NO3 ----> NO3 + MACR  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P134   | xMACR + MEO2 ----> MEO2 +    0.500\*MACR  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P135   | xMACR + RO2C ----> RO2C +    0.500\*MACR  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P136   | xMACR + RO2XC ----> RO2XC +    0.500\*MACR  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P137   | xMACR + MECO3 ----> MECO3 + MACR  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P138   | xMACR + RCO3 ----> RCO3 + MACR  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P139   | xMACR + BZCO3 ----> BZCO3 + MACR  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P140   | xMACR + MACO3 ----> MACO3 + MACR  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P141   | xMVK + NO ----> NO + MVK  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P142   | xMVK + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P143   | xMVK + NO3 ----> NO3 + MVK  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P144   | xMVK + MEO2 ----> MEO2 +    0.500\*MVK  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P145   | xMVK + RO2C ----> RO2C +    0.500\*MVK  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P146   | xMVK + RO2XC ----> RO2XC +    0.500\*MVK  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P147   | xMVK + MECO3 ----> MECO3 + MVK  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P148   | xMVK + RCO3 ----> RCO3 + MVK  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P149   | xMVK + BZCO3 ----> BZCO3 + MVK  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P150   | xMVK + MACO3 ----> MACO3 + MVK  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P151   | xIPRD + NO ----> NO + IPRD  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P152   | xIPRD + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P153   | xIPRD + NO3 ----> NO3 + IPRD  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P154   | xIPRD + MEO2 ----> MEO2 +    0.500\*IPRD  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P155   | xIPRD + RO2C ----> RO2C +    0.500\*IPRD  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P156   | xIPRD + RO2XC ----> RO2XC +    0.500\*IPRD  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P157   | xIPRD + MECO3 ----> MECO3 + IPRD  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P158   | xIPRD + RCO3 ----> RCO3 + IPRD  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P159   | xIPRD + BZCO3 ----> BZCO3 + IPRD  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P160   | xIPRD + MACO3 ----> MACO3 + IPRD  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P161   | xRNO3 + NO ----> NO + RNO3  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P162   | xRNO3 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P163   | xRNO3 + NO3 ----> NO3 + RNO3  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P164   | xRNO3 + MEO2 ----> MEO2 +    0.500\*RNO3  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P165   | xRNO3 + RO2C ----> RO2C +    0.500\*RNO3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P166   | xRNO3 + RO2XC ----> RO2XC +    0.500\*RNO3  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P167   | xRNO3 + MECO3 ----> MECO3 + RNO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P168   | xRNO3 + RCO3 ----> RCO3 + RNO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P169   | xRNO3 + BZCO3 ----> BZCO3 + RNO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P170   | xRNO3 + MACO3 ----> MACO3 + RNO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P171   | yROOH + NO ----> NO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P172   | yROOH + HO2 ----> HO2 + ROOH  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P173   | yROOH + NO3 ----> NO3  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P174   | yROOH + MEO2 ----> MEO2 +    0.500\*MEK  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P175   | yROOH + RO2C ----> RO2C +    0.500\*MEK  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P176   | yROOH + RO2XC ----> RO2XC +    0.500\*MEK  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P177   | yROOH + MECO3 ----> MECO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P178   | yROOH + RCO3 ----> RCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P179   | yROOH + BZCO3 ----> BZCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P180   | yROOH + MACO3 ----> MACO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P181   | yR6OOH + NO ----> NO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P182   | yR6OOH + HO2 ----> HO2 + R6OOH  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P183   | yR6OOH + NO3 ----> NO3  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P184   | yR6OOH + MEO2 ----> MEO2 +    0.500\*PRD2  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P185   | yR6OOH + RO2C ----> RO2C +    0.500\*PRD2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P186   | yR6OOH + RO2XC ----> RO2XC +    0.500\*PRD2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P187   | yR6OOH + MECO3 ----> MECO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P188   | yR6OOH + RCO3 ----> RCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P189   | yR6OOH + BZCO3 ----> BZCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P190   | yR6OOH + MACO3 ----> MACO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P181a   | yISOPOOH + NO ----> NO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P182a   | yISOPOOH + HO2 ----> HO2 + ISOPOOH  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P183a   | yISOPOOH + NO3 ----> NO3  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P184a   | yISOPOOH + MEO2 ----> MEO2 +    0.500\*PRD2  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P185a   | yISOPOOH + RO2C ----> RO2C +    0.500\*PRD2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P186a   | yISOPOOH + RO2XC ----> RO2XC +    0.500\*PRD2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P187a   | yISOPOOH + MECO3 ----> MECO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P188a   | yISOPOOH + RCO3 ----> RCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P189a   | yISOPOOH + BZCO3 ----> BZCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P190a   | yISOPOOH + MACO3 ----> MACO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P191   | yRAOOH + NO ----> NO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P192   | yRAOOH + HO2 ----> HO2 + RAOOH  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P193   | yRAOOH + NO3 ----> NO3  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P194   | yRAOOH + MEO2 ----> MEO2 +    0.500\*PRD2  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P195   | yRAOOH + RO2C ----> RO2C +    0.500\*PRD2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P196   | yRAOOH + RO2XC ----> RO2XC +    0.500\*PRD2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P197   | yRAOOH + MECO3 ----> MECO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P198   | yRAOOH + RCO3 ----> RCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P199   | yRAOOH + BZCO3 ----> BZCO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P200   | yRAOOH + MACO3 ----> MACO3  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P201   | zRNO3 + NO ----> NO + RNO3  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P202   | zRNO3 + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P203   | zRNO3 + NO3 ----> NO3 + PRD2 + HO2  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P204   | zRNO3 + MEO2 ----> MEO2 +    0.500\*PRD2 +    0.500\*HO2  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P205   | zRNO3 + RO2C ----> RO2C +    0.500\*PRD2 +    0.500\*HO2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P206   | zRNO3 + RO2XC ----> RO2XC +    0.500\*PRD2 +    0.500\*HO2  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P207   | zRNO3 + MECO3 ----> MECO3 + PRD2 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P208   | zRNO3 + RCO3 ----> RCO3 + PRD2 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P209   | zRNO3 + BZCO3 ----> BZCO3 + PRD2 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P210   | zRNO3 + MACO3 ----> MACO3 + PRD2 + HO2  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P211   | xHOCCHO + NO ----> NO + HOCCHO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P212   | xHOCCHO + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P213   | xHOCCHO + NO3 ----> NO3 + HOCCHO  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P214   | xHOCCHO + MEO2 ----> MEO2 +    0.500\*HOCCHO  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P215   | xHOCCHO + RO2C ----> RO2C +    0.500\*HOCCHO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P216   | xHOCCHO + RO2XC ----> RO2XC +    0.500\*HOCCHO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P217   | xHOCCHO + MECO3 ----> MECO3 + HOCCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P218   | xHOCCHO + RCO3 ----> RCO3 + HOCCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P219   | xHOCCHO + BZCO3 ----> BZCO3 + HOCCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P220   | xHOCCHO + MACO3 ----> MACO3 + HOCCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P221   | xACROLEIN + NO ----> NO + ACROLEIN  |   BR07 |   9.3002E-12<sup>7</sup>| 
| P222   | xACROLEIN + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| P223   | xACROLEIN + NO3 ----> NO3 + ACROLEIN  |   BR09 |   2.3000E-12<sup>7</sup>| 
| P224   | xACROLEIN + MEO2 ----> MEO2 +    0.500\*ACROLEIN  |   BR10 |   2.0000E-13<sup>7</sup>| 
| P225   | xACROLEIN + RO2C ----> RO2C +    0.500\*ACROLEIN  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P226   | xACROLEIN + RO2XC ----> RO2XC +    0.500\*ACROLEIN  |   BR11 |   3.5000E-14<sup>7</sup>| 
| P227   | xACROLEIN + MECO3 ----> MECO3 + ACROLEIN  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P228   | xACROLEIN + RCO3 ----> RCO3 + ACROLEIN  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P229   | xACROLEIN + BZCO3 ----> BZCO3 + ACROLEIN  |   BR25 |   1.5924E-11<sup>7</sup>| 
| P230   | xACROLEIN + MACO3 ----> MACO3 + ACROLEIN  |   BR25 |   1.5924E-11<sup>7</sup>| 
| BE01   | OH + CH4 ----> MEO2  |   1.85E-12e<sup> -1690.00/T</sup> |   6.3895E-15 |
| BE02   | ETHENE + OH ----> xHO2 + RO2C +    1.610\*xHCHO +    0.195\*xHOCCHO + yROOH  | k<sub>o</sub>=  1.00E-28e<sup>     0.0/T</sup>(T/300)<sup> -4.50</sup><br>k<sub>i</sub> =   8.80E-12e<sup>     0.0/T</sup>(T/300)<sup> -0.85</sup><br>n=     1.00;F=     0.60 |   8.1981E-12 |
| BE03   | ETHENE + O3 ---->   0.160\*HO2 +    0.160\*OH +    0.510\*CO +    0.120\*CO2 + HCHO +    0.370\*HCOOH  |   9.14E-15e<sup> -2580.00/T</sup> |   1.5953E-18 |
| BE04   | ETHENE + NO3 ----> xHO2 + RO2C + xRCHO + yROOH  |   3.30E-12e<sup> -2880.00/T</sup> |   2.1058E-16 |
| BE05   | ETHENE + O3P ---->   0.800\*HO2 +    0.290\*xHO2 +    0.510\*MEO2 +    0.290\*RO2C +    0.510\*CO +    0.278\*xCO +    0.278\*xHCHO +    0.100\*CCHO +    0.012\*xGLY +    0.290\*yROOH  |   1.07E-11e<sup>  -800.00/T</sup> |   7.3127E-13 |
| BT01   | PROPENE + OH ---->   0.984\*xHO2 +    0.984\*RO2C +    0.016\*RO2XC +    0.016\*zRNO3 +    0.984\*xHCHO +    0.984\*xCCHO + yROOH  |   4.85E-12e<sup>   504.00/T</sup> |   2.6296E-11 |
| BT02   | PROPENE + O3 ---->   0.165\*HO2 +    0.350\*OH +    0.355\*MEO2 +    0.525\*CO +    0.215\*CO2 +    0.500\*HCHO +    0.500\*CCHO +    0.185\*HCOOH +    0.075\*CCOOH  |   5.51E-15e<sup> -1878.00/T</sup> |   1.0130E-17 |
| BT03   | PROPENE + NO3 ---->   0.949\*xHO2 +    0.949\*RO2C +    0.051\*RO2XC +    0.051\*zRNO3 + yROOH  |   4.59E-13e<sup> -1156.00/T</sup> |   9.5049E-15 |
| BT04   | PROPENE + O3P ---->   0.450\*RCHO +    0.550\*MEK  |   1.02E-11e<sup>  -280.00/T</sup> |   3.9879E-12 |
| BT05   | BUTADIENE13 + OH ---->   0.951\*xHO2 +    1.189\*RO2C +    0.049\*RO2XC +    0.049\*zRNO3 +    0.708\*xHCHO +    0.480\*xACROLEIN +    0.471\*xIPRD + yROOH  |   1.48E-11e<sup>   448.00/T</sup> |   6.6502E-11 |
| BT06   | BUTADIENE13 + O3 ---->   0.080\*HO2 +    0.080\*OH +    0.255\*CO +    0.185\*CO2 +    0.500\*HCHO +    0.185\*HCOOH +    0.500\*ACROLEIN +    0.375\*MVK +    0.125\*PRD2  |   1.34E-14e<sup> -2283.00/T</sup> |   6.3331E-18 |
| BT07   | BUTADIENE13 + NO3 ---->   0.815\*xHO2 +    0.120\*xNO2 +    1.055\*RO2C +    0.065\*RO2XC +    0.065\*zRNO3 +    0.115\*xHCHO +    0.460\*xMVK +    0.120\*xIPRD +    0.355\*xRNO3 + yROOH  |   1.0000E-13 |   1.0000E-13 |
| BT08   | BUTADIENE13 + O3P ---->   0.250\*HO2 +    0.117\*xHO2 +    0.118\*xMACO3 +    0.235\*RO2C +    0.015\*RO2XC +    0.015\*zRNO3 +    0.115\*xCO +    0.115\*xACROLEIN +    0.001\*xAFG1 +    0.001\*xAFG2 +    0.750\*PRD2 +    0.250\*yROOH  |   2.26E-11e<sup>   -40.00/T</sup> |   1.9763E-11 |
| BE06   | ISOPRENE + OH ---->   0.907\*xHO2 +    0.986\*RO2C +    0.093\*RO2XC +    0.093\*zRNO3 +    0.624\*xHCHO +    0.230\*xMACR +    0.320\*xMVK +    0.357\*xIPRD + yISOPOOH + ISOPRXN  |   2.54E-11e<sup>   410.00/T</sup> |   1.0047E-10 |
| BE07   | ISOPRENE + O3 ---->   0.066\*HO2 +    0.266\*OH +    0.192\*xMACO3 +    0.192\*RO2C +    0.008\*RO2XC +    0.008\*zRNO3 +    0.275\*CO +    0.122\*CO2 +    0.400\*HCHO +    0.192\*xHCHO +    0.204\*HCOOH +    0.390\*MACR +    0.160\*MVK +    0.150\*IPRD +    0.100\*PRD2 +    0.200\*yR6OOH  |   7.86E-15e<sup> -1912.00/T</sup> |   1.2893E-17 |
| BE08   | ISOPRENE + NO3 ---->   0.749\*xHO2 +    0.187\*xNO2 +    0.936\*RO2C +    0.064\*RO2XC +    0.064\*zRNO3 +    0.936\*xIPRD + yR6OOH + ISOPRXN  |   3.03E-12e<sup>  -448.00/T</sup> |   6.7433E-13 |
| BE09   | ISOPRENE + O3P ---->   0.250\*MEO2 +    0.240\*xMACO3 +    0.240\*RO2C +    0.010\*RO2XC +    0.010\*zRNO3 +    0.240\*xHCHO +    0.750\*PRD2 +    0.250\*yR6OOH  |   3.5000E-11 |   3.5000E-11 |
| IS88   | ISOPOOH + OH ----> IEPOX + OH  |   1.90E-11e<sup>   390.00/T</sup> |   7.0281E-11 |
| IS89   | ISOPOOH + OH ---->   0.160\*xMVK +    0.100\*xMACR +    0.350\*RO2C +    0.050\*xRNO3 +    0.260\*xHCHO +    0.040\*xRCHO +    0.310\*xHO2 +    0.020\*ARO2MN +    0.387\*yISOPOOH +    0.610\*RCHO +    0.610\*OH  |   4.75E-12e<sup>   200.00/T</sup> |   9.2901E-12 |
| IS92   | ISOPOOH ----> OH +    0.910\*HO2 +    0.750\*HCHO +    0.450\*MVK +    0.290\*MACR +    0.090\*RO2C +    0.110\*RCHO +    0.050\*ARO2MN  | COOH | Not Available<sup>1</sup> | 
| IS90   | IEPOX + OH ----> IEPOXOO  |   5.78E-11e<sup>  -400.00/T</sup> |   1.5110E-11 |
| IS91   | IEPOXOO + HO2 ---->   0.725\*MEK +    0.275\*HOCCHO +    0.275\*GLY +    0.275\*MGLY +    1.125\*OH +    0.825\*HO2 +    0.200\*CO2 +    0.375\*HCHO +    0.074\*HCOOH +    0.251\*CO  |   2.06E-13e<sup>  1300.00/T</sup> |   1.6125E-11 |
| IS96   | IEPOXOO + NO ---->   0.725\*MEK +    0.275\*HOCCHO +    0.275\*GLY +    0.275\*MGLY +    0.125\*OH +    0.825\*HO2 +    0.200\*CO2 +    0.375\*HCHO +    0.074\*HCOOH +    0.251\*CO + NO2  |   2.60E-12e<sup>   380.00/T</sup> |   9.3002E-12 |
| IS112   | IEPOXOO + MEO2 ---->   0.363\*MEK +    0.138\*HOCCHO +    0.138\*GLY +    0.138\*MGLY +    0.063\*OH +    0.913\*HO2 +    0.100\*CO2 +    0.938\*HCHO +    0.037\*HCOOH +    0.126\*CO +    0.500\*PRD2 +    0.250\*MEOH  |   2.0000E-13 |   2.0000E-13 |
| IS113   | IEPOXOO + RO2C ---->   0.363\*MEK +    0.138\*HOCCHO +    0.138\*GLY +    0.138\*MGLY +    0.063\*OH +    0.413\*HO2 +    0.100\*CO2 +    0.188\*HCHO +    0.037\*HCOOH +    0.126\*CO +    0.500\*PRD2  |   3.5000E-14 |   3.5000E-14 |
| IS114   | IEPOXOO + MECO3 ---->   0.725\*MEK +    0.275\*HOCCHO +    0.275\*GLY +    0.275\*MGLY +    0.125\*OH +    0.825\*HO2 +    1.200\*CO2 +    0.375\*HCHO +    0.074\*HCOOH +    0.251\*CO + MEO2  |   4.40E-13e<sup>  1070.00/T</sup> |   1.5924E-11 |
| BT09   | APIN + OH ---->   0.799\*xHO2 +    0.004\*xRCO3 +    1.042\*RO2C +    0.197\*RO2XC +    0.197\*zRNO3 +    0.002\*xCO +    0.022\*xHCHO +    0.776\*xRCHO +    0.034\*xACETONE +    0.020\*xMGLY +    0.023\*xBACL + yR6OOH + TRPRXN  |   1.21E-11e<sup>   436.00/T</sup> |   5.2225E-11 |
| BT10   | APIN + O3 ---->   0.009\*HO2 +    0.102\*xHO2 +    0.728\*OH +    0.001\*xMECO3 +    0.297\*xRCO3 +    1.511\*RO2C +    0.337\*RO2XC +    0.337\*zRNO3 +    0.029\*CO +    0.051\*xCO +    0.017\*CO2 +    0.344\*xHCHO +    0.240\*xRCHO +    0.345\*xACETONE +    0.008\*MEK +    0.002\*xGLY +    0.081\*xBACL +    0.255\*PRD2 +    0.737\*yR6OOH + TRPRXN  |   5.00E-16e<sup>  -530.00/T</sup> |   8.4519E-17 |
| BT11   | APIN + NO3 ---->   0.056\*xHO2 +    0.643\*xNO2 +    0.007\*xRCO3 +    1.050\*RO2C +    0.293\*RO2XC +    0.293\*zRNO3 +    0.005\*xCO +    0.007\*xHCHO +    0.684\*xRCHO +    0.069\*xACETONE +    0.002\*xMGLY +    0.056\*xRNO3 + yR6OOH + TRPRXN  |   1.19E-12e<sup>   490.00/T</sup> |   6.1560E-12 |
| BT12   | APIN + O3P ----> PRD2 + TRPRXN  |   3.2000E-11 |   3.2000E-11 |
| BE10   | ACETYLENE + OH ---->   0.300\*HO2 +    0.700\*OH +    0.300\*CO +    0.300\*HCOOH +    0.700\*GLY  | k<sub>o</sub>=  5.50E-30e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>k<sub>i</sub> =   8.30E-13e<sup>     0.0/T</sup>(T/300)<sup> -2.00</sup><br>n=     1.00;F=     0.60 |   7.6556E-13 |
| BE11   | ACETYLENE + O3 ---->   1.500\*HO2 +    0.500\*OH +    1.500\*CO +    0.500\*CO2  |   1.00E-14e<sup> -4100.00/T</sup> |   1.0661E-20 |
| BE12   | BENZENE + OH ---->   0.570\*HO2 +    0.290\*xHO2 +    0.116\*OH +    0.290\*RO2C +    0.024\*RO2XC +    0.024\*zRNO3 +    0.290\*xGLY +    0.570\*CRES +    0.029\*xAFG1 +    0.261\*xAFG2 +    0.116\*AFG3 +    0.314\*yRAOOH + BENZRO2  |   2.33E-12e<sup>  -193.00/T</sup> |   1.2196E-12 |
| BT13   | TOLUENE + OH ---->   0.181\*HO2 +    0.454\*xHO2 +    0.312\*OH +    0.454\*RO2C +    0.054\*RO2XC +    0.054\*zRNO3 +    0.238\*xGLY +    0.151\*xMGLY +    0.181\*CRES +    0.065\*xBALD +    0.195\*xAFG1 +    0.195\*xAFG2 +    0.312\*AFG3 +    0.073\*yR6OOH +    0.435\*yRAOOH + TOLRO2  |   1.81E-12e<sup>   338.00/T</sup> |   5.6237E-12 |
| BT14   | MXYL + OH ---->   0.159\*HO2 +    0.520\*xHO2 +    0.239\*OH +    0.520\*RO2C +    0.082\*RO2XC +    0.082\*zRNO3 +    0.100\*xGLY +    0.380\*xMGLY +    0.159\*CRES +    0.041\*xBALD +    0.336\*xAFG1 +    0.144\*xAFG2 +    0.239\*AFG3 +    0.047\*yR6OOH +    0.555\*yRAOOH + XYLRO2  |   2.3100E-11 |   2.3100E-11 |
| BT15   | OXYL + OH ---->   0.161\*HO2 +    0.554\*xHO2 +    0.198\*OH +    0.554\*RO2C +    0.087\*RO2XC +    0.087\*zRNO3 +    0.084\*xGLY +    0.238\*xMGLY +    0.185\*xBACL +    0.161\*CRES +    0.047\*xBALD +    0.253\*xAFG1 +    0.253\*xAFG2 +    0.198\*AFG3 +    0.055\*yR6OOH +    0.586\*yRAOOH + XYLRO2  |   1.3600E-11 |   1.3600E-11 |
| BT16   | PXYL + OH ---->   0.159\*HO2 +    0.487\*xHO2 +    0.278\*OH +    0.487\*RO2C +    0.076\*RO2XC +    0.076\*zRNO3 +    0.286\*xGLY +    0.112\*xMGLY +    0.159\*CRES +    0.088\*xBALD +    0.045\*xAFG1 +    0.067\*xAFG2 +    0.278\*AFG3 +    0.286\*xAFG3 +    0.102\*yR6OOH +    0.461\*yRAOOH + XYLRO2  |   1.4300E-11 |   1.4300E-11 |
| BT17   | TMBENZ124 + OH ---->   0.022\*HO2 +    0.627\*xHO2 +    0.230\*OH +    0.627\*RO2C +    0.121\*RO2XC +    0.121\*zRNO3 +    0.074\*xGLY +    0.405\*xMGLY +    0.112\*xBACL +    0.022\*CRES +    0.036\*xBALD +    0.088\*xAFG1 +    0.352\*xAFG2 +    0.230\*AFG3 +    0.151\*xAFG3 +    0.043\*yR6OOH +    0.705\*yRAOOH + XYLRO2  |   3.2500E-11 |   3.2500E-11 |
| BT18   | ETOH + OH ---->   0.950\*HO2 +    0.050\*xHO2 +    0.050\*RO2C +    0.081\*xHCHO +    0.950\*CCHO +    0.010\*xHOCCHO +    0.050\*yROOH  |   5.49E-13e<sup>   530.00/T</sup>(T/300)<sup>  2.00 </sup> |   3.2078E-12 |
| BL01   | ALK1 + OH ----> xHO2 + RO2C + xCCHO + yROOH  |   1.34E-12e<sup>  -499.00/T</sup>(T/300)<sup>  2.00 </sup> |   2.4824E-13 |
| BL02   | ALK2 + OH ---->   0.965\*xHO2 +    0.965\*RO2C +    0.035\*RO2XC +    0.035\*zRNO3 +    0.261\*xRCHO +    0.704\*xACETONE + yROOH  |   1.49E-12e<sup>   -87.00/T</sup>(T/300)<sup>  2.00 </sup> |   1.0992E-12 |
| BL03   | ALK3 + OH ---->   0.695\*xHO2 +    0.236\*xTBUO +    1.253\*RO2C +    0.070\*RO2XC +    0.070\*zRNO3 +    0.026\*xHCHO +    0.445\*xCCHO +    0.122\*xRCHO +    0.024\*xACETONE +    0.332\*xMEK +    0.983\*yROOH +    0.017\*yR6OOH  |   1.51E-12e<sup>   126.00/T</sup> |   2.3042E-12 |
| BL04   | ALK4 + OH ---->   0.830\*xHO2 +    0.010\*xMEO2 +    0.011\*xMECO3 +    1.763\*RO2C +    0.149\*RO2XC +    0.149\*zRNO3 +    0.002\*xCO +    0.029\*xHCHO +    0.438\*xCCHO +    0.236\*xRCHO +    0.426\*xACETONE +    0.106\*xMEK +    0.146\*xPROD2 + yR6OOH  |   3.75E-12e<sup>    44.00/T</sup> |   4.3463E-12 |
| BL05   | ALK5 + OH ---->   0.647\*xHO2 +    1.605\*RO2C +    0.353\*RO2XC +    0.353\*zRNO3 +    0.040\*xHCHO +    0.106\*xCCHO +    0.209\*xRCHO +    0.071\*xACETONE +    0.086\*xMEK +    0.407\*xPROD2 + yR6OOH  |   2.70E-12e<sup>   374.00/T</sup> |   9.4655E-12 |
| AALK   | SOAALK + OH ----> OH +    0.470\*ALKRXN  |   2.70E-12e<sup>   374.00/T</sup> |   9.4655E-12 |
| BL06   | OLE1 + OH ---->   0.871\*xHO2 +    0.001\*xMEO2 +    1.202\*RO2C +    0.128\*RO2XC +    0.128\*zRNO3 +    0.582\*xHCHO +    0.010\*xCCHO +    0.007\*xHOCCHO +    0.666\*xRCHO +    0.007\*xACETONE +    0.036\*xACROLEIN +    0.001\*xMACR +    0.012\*xMVK +    0.009\*xIPRD +    0.168\*xPROD2 +    0.169\*yROOH +    0.831\*yR6OOH  |   6.72E-12e<sup>   501.00/T</sup> |   3.6070E-11 |
| BL07   | OLE1 + O3 ---->   0.095\*HO2 +    0.057\*xHO2 +    0.128\*OH +    0.090\*RO2C +    0.005\*RO2XC +    0.005\*zRNO3 +    0.303\*CO +    0.088\*CO2 +    0.500\*HCHO +    0.011\*xCCHO +    0.500\*RCHO +    0.044\*xRCHO +    0.003\*xACETONE +    0.009\*MEK +    0.185\*HCOOH +    0.159\*RCOOH +    0.268\*PRD2 +    0.011\*yROOH +    0.052\*yR6OOH  |   3.19E-15e<sup> -1701.00/T</sup> |   1.0618E-17 |
| BL08   | OLE1 + NO3 ---->   0.772\*xHO2 +    1.463\*RO2C +    0.228\*RO2XC +    0.228\*zRNO3 +    0.013\*xCCHO +    0.003\*xRCHO +    0.034\*xACETONE +    0.774\*xRNO3 +    0.169\*yROOH +    0.831\*yR6OOH  |   5.37E-13e<sup> -1047.00/T</sup> |   1.6028E-14 |
| BL09   | OLE1 + O3P ---->   0.450\*RCHO +    0.390\*MEK +    0.160\*PRD2  |   1.61E-11e<sup>  -326.00/T</sup> |   5.3947E-12 |
| BL10   | OLE2 + OH ---->   0.912\*xHO2 +    0.953\*RO2C +    0.088\*RO2XC +    0.088\*zRNO3 +    0.179\*xHCHO +    0.835\*xCCHO +    0.510\*xRCHO +    0.144\*xACETONE +    0.080\*xMEK +    0.002\*xMVK +    0.012\*xIPRD +    0.023\*xPROD2 +    0.319\*yROOH +    0.681\*yR6OOH  |   1.26E-11e<sup>   488.00/T</sup> |   6.4745E-11 |
| BL11   | OLE2 + O3 ---->   0.094\*HO2 +    0.041\*xHO2 +    0.443\*OH +    0.307\*MEO2 +    0.156\*xMECO3 +    0.008\*xRCO3 +    0.212\*RO2C +    0.003\*RO2XC +    0.003\*zRNO3 +    0.299\*CO +    0.161\*CO2 +    0.131\*HCHO +    0.114\*xHCHO +    0.453\*CCHO +    0.071\*xCCHO +    0.333\*RCHO +    0.019\*xRCHO +    0.051\*ACETONE +    0.033\*MEK +    0.001\*xMEK +    0.024\*HCOOH +    0.065\*CCOOH +    0.235\*RCOOH +    0.037\*PRD2 +    0.073\*yROOH +    0.136\*yR6OOH  |   8.59E-15e<sup> -1255.00/T</sup> |   1.2762E-16 |
| BL12   | OLE2 + NO3 ---->   0.400\*xHO2 +    0.426\*xNO2 +    0.035\*xMEO2 +    1.193\*RO2C +    0.140\*RO2XC +    0.140\*zRNO3 +    0.072\*xHCHO +    0.579\*xCCHO +    0.163\*xRCHO +    0.116\*xACETONE +    0.002\*xMEK +    0.320\*xRNO3 +    0.319\*yROOH +    0.681\*yR6OOH  |   2.31E-13e<sup>   382.00/T</sup> |   8.3185E-13 |
| BL13   | OLE2 + O3P ---->   0.079\*RCHO +    0.751\*MEK +    0.170\*PRD2  |   1.43E-11e<sup>   111.00/T</sup> |   2.0750E-11 |
| BL14   | ARO1 + OH ---->   0.123\*HO2 +    0.566\*xHO2 +    0.202\*OH +    0.566\*RO2C +    0.110\*RO2XC +    0.110\*zRNO3 +    0.158\*xGLY +    0.100\*xMGLY +    0.123\*CRES +    0.072\*xAFG1 +    0.185\*xAFG2 +    0.202\*AFG3 +    0.309\*xPROD2 +    0.369\*yR6OOH + TOLRO2  |   7.8400E-12 |   7.8400E-12 |
| BL15   | ARO2MN + OH ---->   0.077\*HO2 +    0.617\*xHO2 +    0.178\*OH +    0.617\*RO2C +    0.128\*RO2XC +    0.128\*zRNO3 +    0.088\*xGLY +    0.312\*xMGLY +    0.134\*xBACL +    0.077\*CRES +    0.026\*xBALD +    0.221\*xAFG1 +    0.247\*xAFG2 +    0.178\*AFG3 +    0.068\*xAFG3 +    0.057\*xPROD2 +    0.101\*yR6OOH + XYLRO2  |   3.0900E-11 |   3.0900E-11 |
| BL15b   | NAPHTHAL + OH ---->   0.077\*HO2 +    0.617\*xHO2 +    0.178\*OH +    0.617\*RO2C +    0.128\*RO2XC +    0.128\*zRNO3 +    0.088\*xGLY +    0.312\*xMGLY +    0.134\*xBACL +    0.077\*CRES +    0.026\*xBALD +    0.221\*xAFG1 +    0.247\*xAFG2 +    0.178\*AFG3 +    0.068\*xAFG3 +    0.057\*xPROD2 +    0.101\*yR6OOH + PAHRO2  |   3.0900E-11 |   3.0900E-11 |
| BL16   | TERP + OH ---->   0.734\*xHO2 +    0.064\*xRCO3 +    1.211\*RO2C +    0.201\*RO2XC +    0.201\*zRNO3 +    0.001\*xCO +    0.411\*xHCHO +    0.385\*xRCHO +    0.037\*xACETONE +    0.007\*xMEK +    0.003\*xMGLY +    0.009\*xBACL +    0.003\*xMVK +    0.002\*xIPRD +    0.409\*xPROD2 + yR6OOH + TRPRXN  |   2.27E-11e<sup>   435.00/T</sup> |   9.7647E-11 |
| BL17   | TERP + O3 ---->   0.078\*HO2 +    0.046\*xHO2 +    0.499\*OH +    0.202\*xMECO3 +    0.059\*xRCO3 +    0.490\*RO2C +    0.121\*RO2XC +    0.121\*zRNO3 +    0.249\*CO +    0.063\*CO2 +    0.127\*HCHO +    0.033\*xHCHO +    0.208\*xRCHO +    0.057\*xACETONE +    0.002\*MEK +    0.172\*HCOOH +    0.068\*RCOOH +    0.003\*xMGLY +    0.039\*xBACL +    0.002\*xMACR +    0.001\*xIPRD +    0.502\*PRD2 +    0.428\*yR6OOH + TRPRXN  |   8.28E-16e<sup>  -785.00/T</sup> |   5.9508E-17 |
| BL18   | TERP + NO3 ---->   0.227\*xHO2 +    0.287\*xNO2 +    0.026\*xRCO3 +    1.786\*RO2C +    0.460\*RO2XC +    0.460\*zRNO3 +    0.012\*xCO +    0.023\*xHCHO +    0.002\*xHOCCHO +    0.403\*xRCHO +    0.239\*xACETONE +    0.005\*xMACR +    0.001\*xMVK +    0.004\*xIPRD +    0.228\*xRNO3 + yR6OOH + TRPRXN  |   1.33E-12e<sup>   490.00/T</sup> |   6.8802E-12 |
| BL19   | TERP + O3P ---->   0.237\*RCHO +    0.763\*PRD2 + TRPRXN  |   4.0200E-11 |   4.0200E-11 |
| BT19   | SESQ + OH ---->   0.734\*xHO2 +    0.064\*xRCO3 +    1.211\*RO2C +    0.201\*RO2XC +    0.201\*zRNO3 +    0.001\*xCO +    0.411\*xHCHO +    0.385\*xRCHO +    0.037\*xACETONE +    0.007\*xMEK +    0.003\*xMGLY +    0.009\*xBACL +    0.003\*xMVK +    0.002\*xIPRD +    0.409\*xPROD2 + yR6OOH + SESQRXN  |   BL16 |   9.7647E-11<sup>7</sup>| 
| BT20   | SESQ + O3 ---->   0.078\*HO2 +    0.046\*xHO2 +    0.499\*OH +    0.202\*xMECO3 +    0.059\*xRCO3 +    0.490\*RO2C +    0.121\*RO2XC +    0.121\*zRNO3 +    0.249\*CO +    0.063\*CO2 +    0.127\*HCHO +    0.033\*xHCHO +    0.208\*xRCHO +    0.057\*xACETONE +    0.002\*MEK +    0.172\*HCOOH +    0.068\*RCOOH +    0.003\*xMGLY +    0.039\*xBACL +    0.002\*xMACR +    0.001\*xIPRD +    0.502\*PRD2 +    0.428\*yR6OOH + SESQRXN  |   BL17 |   5.9508E-17<sup>7</sup>| 
| BT21   | SESQ + NO3 ---->   0.227\*xHO2 +    0.287\*xNO2 +    0.026\*xRCO3 +    1.786\*RO2C +    0.460\*RO2XC +    0.460\*zRNO3 +    0.012\*xCO +    0.023\*xHCHO +    0.002\*xCCHO +    0.403\*xRCHO +    0.239\*xACETONE +    0.005\*xMACR +    0.001\*xMVK +    0.004\*xIPRD +    0.228\*xRNO3 + yR6OOH + SESQRXN  |   BL18 |   6.8802E-12<sup>7</sup>| 
| BT22   | SESQ + O3P ---->   0.237\*RCHO +    0.763\*PRD2 + SESQRXN  |   BL19 |   4.0200E-11<sup>7</sup>| 
| CI01   | CL2 ---->   2.000\*CL  | CL2 | Not Available<sup>1</sup> | 
| CI02   | CL + NO + M ----> CLNO  |   7.60E-32(T/300)<sup> -1.80</sup> |   7.6851E-32 |
| CI03   | CLNO ----> CL + NO  | CLNO_06 | Not Available<sup>1</sup> | 
| CI04   | CL + NO2 ----> CLONO  | k<sub>o</sub>=  1.30E-30e<sup>     0.0/T</sup>(T/300)<sup> -2.00</sup><br>k<sub>i</sub> =   1.00E-10e<sup>     0.0/T</sup>(T/300)<sup> -1.00</sup><br>n=     1.00;F=     0.60 |   1.6244E-11 |
| CI05   | CL + NO2 ----> CLNO2  | k<sub>o</sub>=  1.80E-31e<sup>     0.0/T</sup>(T/300)<sup> -2.00</sup><br>k<sub>i</sub> =   1.00E-10e<sup>     0.0/T</sup>(T/300)<sup> -1.00</sup><br>n=     1.00;F=     0.60 |   3.5840E-12 |
| CI06   | CLONO ----> CL + NO2  | CLONO | Not Available<sup>1</sup> | 
| CI07   | CLNO2 ----> CL + NO2  | CLNO2 | Not Available<sup>1</sup> | 
| CI08   | CL + HO2 ----> HCL  |   3.44E-11(T/300)<sup> -0.56</sup> |   3.4519E-11 |
| CI09   | CL + HO2 ----> CLO + OH  |   9.41E-12(T/300)<sup>  2.10</sup> |   9.2886E-12 |
| CI10   | CL + O3 ----> CLO  |   2.80E-11e<sup>  -250.00/T</sup> |   1.2106E-11 |
| CI11   | CL + NO3 ----> CLO + NO2  |   2.4000E-11 |   2.4000E-11 |
| CI12   | CLO + NO ----> CL + NO2  |   6.20E-12e<sup>   295.00/T</sup> |   1.6676E-11 |
| CI13   | CLO + NO2 ----> CLONO2  | k<sub>o</sub>=  1.80E-31e<sup>     0.0/T</sup>(T/300)<sup> -3.40</sup><br>k<sub>i</sub> =   1.50E-11e<sup>     0.0/T</sup>(T/300)<sup> -1.90</sup><br>n=     1.00;F=     0.60 |   2.3359E-12 |
| CI14   | CLONO2 ----> CLO + NO2  | CLONO2_1 | Not Available<sup>1</sup> | 
| CI15   | CLONO2 ----> CL + NO3  | CLONO2_2 | Not Available<sup>1</sup> | 
| CI16   | CLONO2 ----> CLO + NO2  | k<sub>o</sub>=  4.48E-05e<sup>-12530.0/T</sup>(T/300)<sup> -1.00</sup><br>k<sub>i</sub> =   3.71E+15e<sup>-12530.0/T</sup>(T/300)<sup>  3.50</sup><br>n=     1.00;F=     0.60 |   3.1797E-04 |
| CI17   | CL + CLONO2 ----> CL2 + NO3  |   6.20E-12e<sup>   145.00/T</sup> |   1.0083E-11 |
| CI18   | CLO + HO2 ----> HOCL  |   2.20E-12e<sup>   340.00/T</sup> |   6.8814E-12 |
| CI19   | HOCL ----> OH + CL  | HOCL_06 | Not Available<sup>1</sup> | 
| CI20   | CLO + CLO ---->   0.290\*CL2 +    1.420\*CL  |   1.25E-11e<sup> -1960.00/T</sup> |   1.7455E-14 |
| CI21   | OH + HCL ----> CL  |   1.70E-12e<sup>  -230.00/T</sup> |   7.8600E-13 |
| CI22   | CL + H2 ----> HCL + HO2  |   3.90E-11e<sup> -2310.00/T</sup> |   1.6836E-14 |
| CP01   | HCHO + CL ----> HCL + HO2 + CO  |   8.10E-11e<sup>   -30.00/T</sup> |   7.3246E-11 |
| CP02   | CCHO + CL ----> HCL + MECO3  |   8.0000E-11 |   8.0000E-11 |
| CP03   | MEOH + CL ----> HCL + HCHO + HO2  |   5.5000E-11 |   5.5000E-11 |
| CP04   | RCHO + CL ----> HCL +    0.900\*RCO3 +    0.100\*RO2C +    0.100\*xCCHO +    0.100\*xCO +    0.100\*xHO2 +    0.100\*yROOH  |   1.2300E-10 |   1.2300E-10 |
| CP05   | ACETONE + CL ----> HCL + RO2C + xHCHO + xMECO3 + yROOH  |   7.70E-11e<sup> -1000.00/T</sup> |   2.6907E-12 |
| CP06   | MEK + CL ----> HCL +    0.975\*RO2C +    0.039\*RO2XC +    0.039\*zRNO3 +    0.840\*xHO2 +    0.085\*xMECO3 +    0.036\*xRCO3 +    0.065\*xHCHO +    0.070\*xCCHO +    0.840\*xRCHO + yROOH  |   3.6000E-11 |   3.6000E-11 |
| CP07   | RNO3 + CL ----> HCL +    0.038\*NO2 +    0.055\*HO2 +    1.282\*RO2C +    0.202\*RO2XC +    0.202\*zRNO3 +    0.009\*RCHO +    0.018\*MEK +    0.012\*PRD2 +    0.055\*RNO3 +    0.159\*xNO2 +    0.547\*xHO2 +    0.045\*xHCHO +    0.300\*xCCHO +    0.020\*xRCHO +    0.003\*xACETONE +    0.041\*xMEK +    0.046\*xPROD2 +    0.547\*xRNO3 +    0.908\*yR6OOH  |   1.9200E-10 |   1.9200E-10 |
| CP08   | PRD2 + CL ----> HCL +    0.314\*HO2 +    0.680\*RO2C +    0.116\*RO2XC +    0.116\*zRNO3 +    0.198\*RCHO +    0.116\*PRD2 +    0.541\*xHO2 +    0.007\*xMECO3 +    0.022\*xRCO3 +    0.237\*xHCHO +    0.109\*xCCHO +    0.591\*xRCHO +    0.051\*xMEK +    0.040\*xPROD2 +    0.686\*yR6OOH  |   2.0000E-10 |   2.0000E-10 |
| CP09   | GLY + CL ----> HCL +    0.630\*HO2 +    1.260\*CO +    0.370\*RCO3  |   8.10E-11e<sup>   -30.00/T</sup> |   7.3246E-11 |
| CP10   | MGLY + CL ----> HCL + CO + MECO3  |   8.0000E-11 |   8.0000E-11 |
| CP11   | CRES + CL ----> HCL + xHO2 + xBALD + yR6OOH  |   6.2000E-11 |   6.2000E-11 |
| CP12   | BALD + CL ----> HCL + BZCO3  |   8.0000E-11 |   8.0000E-11 |
| CP13   | ROOH + CL ----> HCL +    0.414\*OH +    0.588\*RO2C +    0.414\*RCHO +    0.104\*xOH +    0.482\*xHO2 +    0.106\*xHCHO +    0.104\*xCCHO +    0.197\*xRCHO +    0.285\*xMEK +    0.586\*yROOH  |   1.6600E-10 |   1.6600E-10 |
| CP14   | R6OOH + CL ----> HCL +    0.145\*OH +    1.078\*RO2C +    0.117\*RO2XC +    0.117\*zRNO3 +    0.145\*PRD2 +    0.502\*xOH +    0.237\*xHO2 +    0.186\*xCCHO +    0.676\*xRCHO +    0.280\*xPROD2 +    0.855\*yR6OOH  |   3.0000E-10 |   3.0000E-10 |
| CP15   | RAOOH + CL ---->   0.404\*HCL +    0.139\*OH +    0.148\*HO2 +    0.589\*RO2C +    0.124\*RO2XC +    0.124\*zRNO3 +    0.074\*PRD2 +    0.147\*MGLY +    0.139\*IPRD +    0.565\*xHO2 +    0.024\*xOH +    0.448\*xRCHO +    0.026\*xGLY +    0.030\*xMEK +    0.252\*xMGLY +    0.073\*xAFG1 +    0.073\*xAFG2 +    0.713\*yR6OOH  |   4.2900E-10 |   4.2900E-10 |
| TP01   | ACROLEIN + CL ---->   0.484\*xHO2 +    0.274\*xCL +    0.216\*MACO3 +    1.032\*RO2C +    0.026\*RO2XC +    0.026\*zRNO3 +    0.216\*HCL +    0.484\*xCO +    0.274\*xHCHO +    0.274\*xGLY +    0.484\*xCLCCHO +    0.784\*yROOH  |   2.9400E-10 |   2.9400E-10 |
| CP16   | MACR + CL ---->   0.250\*HCL +    0.165\*MACO3 +    0.802\*RO2C +    0.033\*RO2XC +    0.033\*zRNO3 +    0.802\*xHO2 +    0.541\*xCO +    0.082\*xIPRD +    0.180\*xCLCCHO +    0.541\*xCLACET +    0.835\*yROOH  |   3.8500E-10 |   3.8500E-10 |
| CP17   | MVK + CL ---->   1.283\*RO2C +    0.053\*RO2XC +    0.053\*zRNO3 +    0.322\*xHO2 +    0.625\*xMECO3 +    0.947\*xCLCCHO + yROOH  |   2.3200E-10 |   2.3200E-10 |
| CP18   | IPRD + CL ---->   0.401\*HCL +    0.084\*HO2 +    0.154\*MACO3 +    0.730\*RO2C +    0.051\*RO2XC +    0.051\*zRNO3 +    0.042\*AFG1 +    0.042\*AFG2 +    0.712\*xHO2 +    0.498\*xCO +    0.195\*xHCHO +    0.017\*xMGLY +    0.009\*xAFG1 +    0.009\*xAFG2 +    0.115\*xIPRD +    0.140\*xCLCCHO +    0.420\*xCLACET +    0.762\*yR6OOH  |   4.1200E-10 |   4.1200E-10 |
| CP19   | CLCCHO ----> HO2 + CO + RO2C + xCL + xHCHO + yROOH  | CLCCHO | Not Available<sup>1</sup> | 
| CP20   | CLCCHO + OH ----> RCO3  |   3.1000E-12 |   3.1000E-12 |
| CP21   | CLCCHO + CL ----> HCL + RCO3  |   1.2900E-11 |   1.2900E-11 |
| CP22   | CLACET ----> MECO3 + RO2C + xCL + xHCHO + yROOH  |   5.0000E-01\*CLACET | Not Available<sup>1</sup> | 
| CP29   | xCL + NO ----> NO + CL  |   BR07 |   9.3002E-12<sup>7</sup>| 
| CP30   | xCL + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| CP31   | xCL + NO3 ----> NO3 + CL  |   BR09 |   2.3000E-12<sup>7</sup>| 
| CP32   | xCL + MEO2 ----> MEO2 +    0.500\*CL  |   BR10 |   2.0000E-13<sup>7</sup>| 
| CP33   | xCL + RO2C ----> RO2C +    0.500\*CL  |   BR11 |   3.5000E-14<sup>7</sup>| 
| CP34   | xCL + RO2XC ----> RO2XC +    0.500\*CL  |   BR11 |   3.5000E-14<sup>7</sup>| 
| CP35   | xCL + MECO3 ----> MECO3 + CL  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP36   | xCL + RCO3 ----> RCO3 + CL  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP37   | xCL + BZCO3 ----> BZCO3 + CL  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP38   | xCL + MACO3 ----> MACO3 + CL  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP39   | xCLCCHO + NO ----> NO + CLCCHO  |   BR07 |   9.3002E-12<sup>7</sup>| 
| CP40   | xCLCCHO + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| CP41   | xCLCCHO + NO3 ----> NO3 + CLCCHO  |   BR09 |   2.3000E-12<sup>7</sup>| 
| CP42   | xCLCCHO + MEO2 ----> MEO2 +    0.500\*CLCCHO  |   BR10 |   2.0000E-13<sup>7</sup>| 
| CP43   | xCLCCHO + RO2C ----> RO2C +    0.500\*CLCCHO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| CP44   | xCLCCHO + RO2XC ----> RO2XC +    0.500\*CLCCHO  |   BR11 |   3.5000E-14<sup>7</sup>| 
| CP45   | xCLCCHO + MECO3 ----> MECO3 + CLCCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP46   | xCLCCHO + RCO3 ----> RCO3 + CLCCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP47   | xCLCCHO + BZCO3 ----> BZCO3 + CLCCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP48   | xCLCCHO + MACO3 ----> MACO3 + CLCCHO  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP49   | xCLACET + NO ----> NO + CLACET  |   BR07 |   9.3002E-12<sup>7</sup>| 
| CP50   | xCLACET + HO2 ----> HO2  |   BR08 |   7.7759E-12<sup>7</sup>| 
| CP51   | xCLACET + NO3 ----> NO3 + CLACET  |   BR09 |   2.3000E-12<sup>7</sup>| 
| CP52   | xCLACET + MEO2 ----> MEO2 +    0.500\*CLACET  |   BR10 |   2.0000E-13<sup>7</sup>| 
| CP53   | xCLACET + RO2C ----> RO2C +    0.500\*CLACET  |   BR11 |   3.5000E-14<sup>7</sup>| 
| CP54   | xCLACET + RO2XC ----> RO2XC +    0.500\*CLACET  |   BR11 |   3.5000E-14<sup>7</sup>| 
| CP55   | xCLACET + MECO3 ----> MECO3 + CLACET  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP56   | xCLACET + RCO3 ----> RCO3 + CLACET  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP57   | xCLACET + BZCO3 ----> BZCO3 + CLACET  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CP58   | xCLACET + MACO3 ----> MACO3 + CLACET  |   BR25 |   1.5924E-11<sup>7</sup>| 
| CE01   | CL + CH4 ----> HCL + MEO2  |   7.30E-12e<sup> -1280.00/T</sup> |   9.9732E-14 |
| CE02   | ETHENE + CL ----> xHO2 +    2.000\*RO2C + xHCHO + CLCHO  | k<sub>o</sub>=  1.60E-29e<sup>     0.0/T</sup>(T/300)<sup> -3.30</sup><br>k<sub>i</sub> =   3.10E-10e<sup>     0.0/T</sup>(T/300)<sup> -1.00</sup><br>n=     1.00;F=     0.60 |   1.0603E-10 |
| TE01   | PROPENE + CL ---->   0.124\*HCL +    0.971\*xHO2 +    0.971\*RO2C +    0.029\*RO2XC +    0.029\*zRNO3 +    0.124\*xACROLEIN +    0.306\*xCLCCHO +    0.540\*xCLACET + yROOH  |   2.6700E-10 |   2.6700E-10 |
| TE02   | BUTADIENE13 + CL ---->   0.390\*xHO2 +    0.541\*xCL +    1.884\*RO2C +    0.069\*RO2XC +    0.069\*zRNO3 +    0.863\*xHCHO +    0.457\*xACROLEIN +    0.473\*xIPRD + yROOH  |   4.9000E-10 |   4.9000E-10 |
| CE03   | ISOPRENE + CL ---->   0.150\*HCL +    0.738\*xHO2 +    0.177\*xCL +    1.168\*RO2C +    0.085\*RO2XC +    0.085\*zRNO3 +    0.275\*xHCHO +    0.177\*xMVK +    0.671\*xIPRD +    0.067\*xCLCCHO + yR6OOH  |   4.8000E-10 |   4.8000E-10 |
| TE03   | APIN + CL ---->   0.548\*HCL +    0.252\*xHO2 +    0.068\*xCL +    0.034\*xMECO3 +    0.050\*xRCO3 +    0.016\*xMACO3 +    2.258\*RO2C +    0.582\*RO2XC +    0.582\*zRNO3 +    0.035\*xCO +    0.158\*xHCHO +    0.185\*xRCHO +    0.274\*xACETONE +    0.007\*xGLY +    0.003\*xBACL +    0.003\*xMVK +    0.158\*xIPRD +    0.006\*xAFG1 +    0.006\*xAFG2 +    0.001\*xAFG3 +    0.109\*xCLCCHO + yR6OOH  |   5.4600E-10 |   5.4600E-10 |
| CE04   | ACETYLENE + CL ----> HO2 + CO  | k<sub>o</sub>=  5.20E-30e<sup>     0.0/T</sup>(T/300)<sup> -2.40</sup><br>k<sub>i</sub> =   2.20E-10e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.00;F=     0.60 |   5.0269E-11 |
| TE04   | TOLUENE + CL ---->   0.894\*xHO2 +    0.894\*RO2C +    0.106\*RO2XC +    0.106\*zRNO3 +    0.894\*xBALD  |   6.2000E-11 |   6.2000E-11 |
| TE05   | MXYL + CL ---->   0.864\*xHO2 +    0.864\*RO2C +    0.136\*RO2XC +    0.136\*zRNO3 +    0.864\*xBALD  |   1.3500E-10 |   1.3500E-10 |
| TE06   | OXYL + CL ---->   0.864\*xHO2 +    0.864\*RO2C +    0.136\*RO2XC +    0.136\*zRNO3 +    0.864\*xBALD  |   1.4000E-10 |   1.4000E-10 |
| TE07   | PXYL + CL ---->   0.864\*xHO2 +    0.864\*RO2C +    0.136\*RO2XC +    0.136\*zRNO3 +    0.864\*xBALD  |   1.4400E-10 |   1.4400E-10 |
| TE08   | TMBENZ124 + CL ---->   0.838\*xHO2 +    0.838\*RO2C +    0.162\*RO2XC +    0.162\*zRNO3 +    0.838\*xBALD  |   2.4200E-10 |   2.4200E-10 |
| TE09   | ETOH + CL ----> HCL +    0.688\*HO2 +    0.312\*xHO2 +    0.312\*RO2C +    0.503\*xHCHO +    0.688\*CCHO +    0.061\*xHOCCHO +    0.312\*yROOH  |   8.60E-11e<sup>    45.00/T</sup> |   1.0001E-10 |
| BC01   | ALK1 + CL ----> HCL + xHO2 + RO2C + xCCHO + yROOH  |   8.30E-11e<sup>  -100.00/T</sup> |   5.9349E-11 |
| BC02   | ALK2 + CL ----> HCL +    0.970\*xHO2 +    0.970\*RO2C +    0.030\*RO2XC +    0.030\*zRNO3 +    0.482\*xRCHO +    0.488\*xACETONE + yROOH  |   1.20E-10e<sup>    40.00/T</sup> |   1.3723E-10 |
| BC03   | ALK3 + CL ----> HCL +    0.835\*xHO2 +    0.094\*xTBUO +    1.361\*RO2C +    0.070\*RO2XC +    0.070\*zRNO3 +    0.078\*xHCHO +    0.340\*xCCHO +    0.343\*xRCHO +    0.075\*xACETONE +    0.253\*xMEK +    0.983\*yROOH +    0.017\*yR6OOH  |   1.8600E-10 |   1.8600E-10 |
| BC04   | ALK4 + CL ----> HCL +    0.827\*xHO2 +    0.003\*xMEO2 +    0.004\*xMECO3 +    1.737\*RO2C +    0.165\*RO2XC +    0.165\*zRNO3 +    0.003\*xCO +    0.034\*xHCHO +    0.287\*xCCHO +    0.412\*xRCHO +    0.247\*xACETONE +    0.076\*xMEK +    0.130\*xPROD2 + yR6OOH  |   2.6300E-10 |   2.6300E-10 |
| BC05   | ALK5 + CL ----> HCL +    0.647\*xHO2 +    1.541\*RO2C +    0.352\*RO2XC +    0.352\*zRNO3 +    0.022\*xHCHO +    0.080\*xCCHO +    0.258\*xRCHO +    0.044\*xACETONE +    0.041\*xMEK +    0.378\*xPROD2 + yR6OOH  |   4.2100E-10 |   4.2100E-10 |
| BC06   | OLE1 + CL ---->   0.384\*HCL +    0.873\*xHO2 +    1.608\*RO2C +    0.127\*RO2XC +    0.127\*zRNO3 +    0.036\*xHCHO +    0.206\*xCCHO +    0.072\*xRCHO +    0.215\*xACROLEIN +    0.019\*xMVK +    0.038\*xIPRD +    0.192\*xCLCCHO +    0.337\*xCLACET +    0.169\*yROOH +    0.831\*yR6OOH  |   3.9200E-10 |   3.9200E-10 |
| BC07   | OLE2 + CL ---->   0.279\*HCL +    0.450\*xHO2 +    0.442\*xCL +    0.001\*xMEO2 +    1.492\*RO2C +    0.106\*RO2XC +    0.106\*zRNO3 +    0.190\*xHCHO +    0.383\*xCCHO +    0.317\*xRCHO +    0.086\*xACETONE +    0.042\*xMEK +    0.025\*xMACR +    0.058\*xMVK +    0.161\*xIPRD +    0.013\*xCLCCHO +    0.191\*xCLACET +    0.319\*yROOH +    0.681\*yR6OOH  |   3.7700E-10 |   3.7700E-10 |
| BC08   | ARO1 + CL ---->   0.840\*xHO2 +    0.840\*RO2C +    0.160\*RO2XC +    0.160\*zRNO3 +    0.840\*xPROD2  |   2.1600E-10 |   2.1600E-10 |
| BC09   | ARO2MN + CL ---->   0.828\*xHO2 +    0.828\*RO2C +    0.172\*RO2XC +    0.172\*zRNO3 +    0.469\*xBALD +    0.359\*xPROD2  |   2.6600E-10 |   2.6600E-10 |
| BC09b   | NAPHTHAL + CL ---->   0.828\*xHO2 +    0.828\*RO2C +    0.172\*RO2XC +    0.172\*zRNO3 +    0.469\*xBALD +    0.359\*xPROD2  |   2.6600E-10 |   2.6600E-10 |
| BC10   | TERP + CL ---->   0.548\*HCL +    0.252\*xHO2 +    0.068\*xCL +    0.034\*xMECO3 +    0.050\*xRCO3 +    0.016\*xMACO3 +    2.258\*RO2C +    0.582\*RO2XC +    0.582\*zRNO3 +    0.035\*xCO +    0.158\*xHCHO +    0.185\*xRCHO +    0.274\*xACETONE +    0.007\*xGLY +    0.003\*xBACL +    0.003\*xMVK +    0.158\*xIPRD +    0.006\*xAFG1 +    0.006\*xAFG2 +    0.001\*xAFG3 +    0.109\*xCLCCHO + yR6OOH  |   5.4600E-10 |   5.4600E-10 |
| BC11   | SESQ + CL ---->   0.252\*xHO2 +    0.068\*xCL +    0.034\*xMECO3 +    0.050\*xRCO3 +    0.016\*xMACO3 +    2.258\*RO2C +    0.582\*RO2XC +    0.582\*zRNO3 +    0.548\*HCL +    0.035\*xCO +    0.158\*xHCHO +    0.185\*xRCHO +    0.274\*xACETONE +    0.007\*xGLY +    0.003\*xBACL +    0.003\*xMVK +    0.158\*xIPRD +    0.006\*xAFG1 +    0.006\*xAFG2 +    0.001\*xAFG3 +    0.109\*xCLCCHO + yR6OOH  |   BC10 |   5.4600E-10<sup>7</sup>| 
| AE51   | BENZRO2 + NO ----> NO + BNZNRXN  |   BR07 |   9.3002E-12<sup>7</sup>| 
| AE52   | BENZRO2 + HO2 ----> HO2 + BNZHRXN  |   BR08 |   7.7759E-12<sup>7</sup>| 
| AE53   | XYLRO2 + NO ----> NO + XYLNRXN  |   BR07 |   9.3002E-12<sup>7</sup>| 
| AE54   | XYLRO2 + HO2 ----> HO2 + XYLHRXN  |   BR08 |   7.7759E-12<sup>7</sup>| 
| AE55   | TOLRO2 + NO ----> NO + TOLNRXN  |   BR07 |   9.3002E-12<sup>7</sup>| 
| AE56   | TOLRO2 + HO2 ----> HO2 + TOLHRXN  |   BR08 |   7.7759E-12<sup>7</sup>| 
| AE55b   | PAHRO2 + NO ----> NO + PAHNRXN  |   BR07 |   9.3002E-12<sup>7</sup>| 
| AE56b   | PAHRO2 + HO2 ----> HO2 + PAHHRXN  |   BR08 |   7.7759E-12<sup>7</sup>| 
| TR01   | HCHO_PRIMARY ----> | HCHOR_06 | Not Available<sup>1</sup> | 
| TR02   | HCHO_PRIMARY ----> | HCHOM_06 | Not Available<sup>1</sup> | 
| TR03   | HCHO_PRIMARY + OH ----> OH  |   5.40E-12e<sup>   135.00/T</sup> |   8.4926E-12 |
| TR05   | HCHO_PRIMARY + NO3 ----> NO3  |   2.00E-12e<sup> -2431.00/T</sup> |   5.7539E-16 |
| TR06   | HCHO_PRIMARY + CL ----> CL  |   8.10E-11e<sup>   -30.00/T</sup> |   7.3246E-11 |
| TR07   | CCHO_PRIMARY + OH ----> OH  |   4.40E-12e<sup>   365.00/T</sup> |   1.4967E-11 |
| TR08   | CCHO_PRIMARY ----> | CCHO_R | Not Available<sup>1</sup> | 
| TR09   | CCHO_PRIMARY + NO3 ----> NO3  |   1.40E-12e<sup> -1860.00/T</sup> |   2.7340E-15 |
| TR10   | CCHO_PRIMARY + CL ----> CL  |   8.0000E-11 |   8.0000E-11 |
| TR11   | ACRO_PRIMARY + OH ----> OH  |   1.9900E-11 |   1.9900E-11 |
| TR12   | ACRO_PRIMARY + O3 ----> O3  |   1.40E-15e<sup> -2528.00/T</sup> |   2.9091E-19 |
| TR13   | ACRO_PRIMARY + NO3 ----> NO3  |   1.1800E-15 |   1.1800E-15 |
| TR14   | ACRO_PRIMARY + O3P ----> O3P  |   2.3700E-12 |   2.3700E-12 |
| TR15   | ACRO_PRIMARY ----> | ACRO_09 | Not Available<sup>1</sup> | 
| TR16   | ACRO_PRIMARY + CL ----> CL  |   2.9400E-10 |   2.9400E-10 |
| HET_N02   | NO2 ---->   0.500\*HONO +    0.500\*HNO3  | HETERO_NO2 | Not Available<sup>2</sup> | 
| HET_N2O5IJ   | N2O5 ----> HNO3 + H2NO3PIJ  | HETERO_N2O5IJ | Not Available<sup>2</sup> | 
| HET_N2O5K   | N2O5 ----> HNO3 + H2NO3PK  | HETERO_N2O5K | Not Available<sup>2</sup> | 
| HET_H2NO3PIJA   | H2NO3PIJ ----> HNO3  | HETERO_H2NO3PAIJ | Not Available<sup>2</sup> | 
| HET_H2NO3PKA   | H2NO3PK ----> HNO3  | HETERO_H2NO3PAK | Not Available<sup>2</sup> | 
| HET_H2NO3PIB   | H2NO3PIJ + ACLI ----> CLNO2  | HETERO_H2NO3PBIJ | Not Available<sup>2</sup> | 
| HET_H2NO3PJB   | H2NO3PIJ + ACLJ ----> CLNO2  | HETERO_H2NO3PBIJ | Not Available<sup>2</sup> | 
| HET_H2NO3PKB   | H2NO3PK + ACLK ----> CLNO2  | HETERO_H2NO3PBK | Not Available<sup>2</sup> | 
| HAL_Ozone   | O3 ----> | SEAWATER*min( 6.701E-11e<sup> 1.074E+01P</sup>+ 3.415E-08e<sup>-6.713E-01P</sup>, <br> 2.000E-06) |   2.0000E-06<sup>4</sup>| 
| OLIG_XYLENE1   | AXYL1J ---->   0.857\*AOLGAJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_XYLENE2   | AXYL2J ---->   1.143\*AOLGAJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_TOLUENE1   | ATOL1J ---->   0.857\*AOLGAJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_TOLUENE2   | ATOL2J ---->   1.143\*AOLGAJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_BENZENE1   | ABNZ1J ---->   0.714\*AOLGAJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_BENZENE2   | ABNZ2J ---->   0.714\*AOLGAJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_TERPENE1   | ATRP1J ---->   0.800\*AOLGBJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_TERPENE2   | ATRP2J ---->   0.900\*AOLGBJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_ISOPRENE1   | AISO1J ---->   0.500\*AOLGBJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_ISOPRENE2   | AISO2J ---->   0.500\*AOLGBJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_SESQT1   | ASQTJ ---->   1.500\*AOLGBJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_PAH1   | APAH1J ---->   1.429\*AOLGAJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_PAH2   | APAH2J ---->   1.429\*AOLGAJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_ALK1   | AALK1J ---->   1.714\*AOLGAJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_ALK2   | AALK2J ---->   1.714\*AOLGAJ  |   9.4882E-06 |   9.4882E-06 |
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
| HET_IEPOX   | IEPOX ----> AISO3J  | HETERO_IEPOX | Not Available<sup>2</sup> | 

<sup>0</sup>Units molecules/(sec*cm<sup>3</sup>); Value at 298.15 K;   2.4615E+19 molcules/cm<sup>3</sup>;   1.00 Atm.     
<sup>1</sup>Photolysis Reaction;depends on radiation and predicted concentrations     
<sup>2</sup>Heteorogeneous Reaction;Depends predicted concentrations                
<sup>4</sup>Set to zero if sun is below the horizon. SEAWATER equals surface fraction covered by ice free open ocean plus surf zones. P equals air pressure in atmospheres.         
<sup>7</sup>Rate constant multiple of constant for listed reaction   
