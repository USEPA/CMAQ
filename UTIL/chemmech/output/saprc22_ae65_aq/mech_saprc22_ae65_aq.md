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
| 2   | O3P + O2 + M ----> O3  |   6.00E-34(T/300)<sup>  2.40</sup> |   5.9116E-34 |
| 3   | O3P + O3 ----> |   8.00E-12e<sup> -2060.00/T</sup> |   7.9879E-15 |
| 4   | O3P + NO ----> NO2  | k<sub>o</sub>=  9.00E-32e<sup>     0.0/T</sup>(T/300)<sup> -1.50</sup><br>k<sub>i</sub> =   3.00E-11e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.00;F=     0.60 |   1.6618E-12 |
| 5   | O3P + NO2 ----> NO  |   5.10E-12e<sup>   210.00/T</sup> |   1.0315E-11 |
| 6   | O3P + NO2 ----> NO3  | k<sub>o</sub>=  2.50E-31e<sup>     0.0/T</sup>(T/300)<sup> -1.80</sup><br>k<sub>i</sub> =   2.20E-11e<sup>     0.0/T</sup>(T/300)<sup> -0.70</sup><br>n=     1.00;F=     0.60 |   3.2805E-12 |
| 7   | O3 + NO ----> NO2  |   3.00E-12e<sup> -1500.00/T</sup> |   1.9596E-14 |
| 8   | O3 + NO2 ----> NO3  |   1.20E-13e<sup> -2450.00/T</sup> |   3.2392E-17 |
| 9   | NO + NO3 ---->   2.0000\*NO2  |   1.50E-11e<sup>   170.00/T</sup> |   2.6529E-11 |
| 10   | NO + NO + O2 ---->   2.0000\*NO2  |   3.30E-39e<sup>   530.00/T</sup> |   1.9522E-38 |
| 11   | NO2 + NO3 ----> N2O5  | k<sub>o</sub>=  3.60E-30e<sup>     0.0/T</sup>(T/300)<sup> -4.10</sup><br>k<sub>i</sub> =   1.90E-12e<sup>     0.0/T</sup>(T/300)<sup>  0.20</sup><br>n=     1.33;F=     0.35 |   1.2406E-12 |
| 12   | N2O5 ----> NO2 + NO3  | k<sub>o</sub>=  1.30E-03e<sup>-11000.0/T</sup>(T/300)<sup> -3.50</sup><br>k<sub>i</sub> =   9.70E+14e<sup>-11080.0/T</sup>(T/300)<sup>  0.10</sup><br>n=     1.33;F=     0.35 |   4.5396E-02 |
| 13   | N2O5 + H2O ---->   2.0000\*HNO3  |   0.0000E+00 |   0.0000E+00 |
| 14   | N2O5 + H2O + H2O ---->   2.0000\*HNO3  |   0.0000E+00 |   0.0000E+00 |
| 15   | NO2 + NO3 ----> NO + NO2  |   4.50E-14e<sup> -1260.00/T</sup> |   6.5744E-16 |
| 16   | NO3 ----> NO  | NO3NO_06 | Not Available<sup>1</sup> | 
| 17   | NO3 ----> NO2 + O3P  | NO3NO2_6 | Not Available<sup>1</sup> | 
| 18   | O3 ----> O1D  | O3O1D_06 | Not Available<sup>1</sup> | 
| 19   | O3 ----> O3P  | O3O3P_06 | Not Available<sup>1</sup> | 
| 20   | O1D + H2O ---->   2.0000\*OH  |   1.63E-10e<sup>    60.00/T</sup> |   1.9934E-10 |
| 21   | O1D + M ----> O3P  |   2.65E-11e<sup>    98.00/T</sup> |   3.6813E-11 |
| 22   | OH + NO ----> HONO  | k<sub>o</sub>=  7.00E-31e<sup>     0.0/T</sup>(T/300)<sup> -2.60</sup><br>k<sub>i</sub> =   3.60E-11e<sup>     0.0/T</sup>(T/300)<sup> -0.10</sup><br>n=     1.00;F=     0.60 |   7.3998E-12 |
| 23   | HONO ----> OH + NO  | HONO_06 | Not Available<sup>1</sup> | 
| 24   | OH + HONO ----> NO2  |   1.80E-11e<sup>  -390.00/T</sup> |   4.8662E-12 |
| 25   | OH + NO2 ----> HNO3  | k<sub>o</sub>=  3.20E-30e<sup>     0.0/T</sup>(T/300)<sup> -4.50</sup><br>k<sub>i</sub> =   3.00E-11e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.24;F=     0.41 |   9.8821E-12 |
| 26   | OH + NO3 ----> HO2 + NO2  |   2.2000E-11 |   2.2000E-11 |
| 27   | OH + HNO3 ----> NO3  | k<sub>0</sub>=  2.40E-14e<sup>   460.0/T</sup><br>k<sub>1</sub>=  2.70E-17e<sup>  2199.0/T</sup><br>k<sub>3</sub>=  6.50E-34e<sup>  1335.0/T</sup> |   1.5409E-13 |
| 28   | HNO3 ----> OH + NO2  | HNO3 | Not Available<sup>1</sup> | 
| 29   | OH + O3 ----> HO2  |   1.70E-12e<sup>  -940.00/T</sup> |   7.2647E-14 |
| 30   | HO2 + NO ----> OH + NO2  |   3.30E-12e<sup>   270.00/T</sup> |   8.1621E-12 |
| 31   | HO2 + NO ----> HNO3  | k<sub>0</sub>=  2.3900E-12e<sup> -1711.0/T</sup>(T/300)<sup>-13.77</sup><br>k<sub>2</sub>=  1.8300E-32e<sup>  -772.0/T</sup>(T/300)<sup> -4.85</sup><br>k<sub>3</sub>=  0.0000E+00e<sup>     0.00/T</sup> |   4.3224E-14 |
| 32   | HO2 + NO + H2O ----> HNO3  |   1.20E-35e<sup>  2944.00/T</sup> |   2.3308E-31 |
| 33   | HO2 + NO2 ----> HNO4  | k<sub>o</sub>=  1.40E-31e<sup>     0.0/T</sup>(T/300)<sup> -3.10</sup><br>k<sub>i</sub> =   4.00E-12e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.26;F=     0.40 |   7.4949E-13 |
| 34   | HNO4 ----> HO2 + NO2  | k<sub>o</sub>=  4.10E-05e<sup>-10650.0/T</sup>(T/300)<sup>  0.00</sup><br>k<sub>i</sub> =   6.00E+15e<sup>-11170.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.26;F=     0.40 |   6.3131E-02 |
| 35   | HNO4 ---->   0.8000\*HO2 +    0.8000\*NO2 +    0.2000\*OH +    0.2000\*NO3  | HNO4_06 | Not Available<sup>1</sup> | 
| 36   | HNO4 + OH ----> NO2  |   1.30E-12e<sup>   380.00/T</sup> |   4.6501E-12 |
| 37   | HO2 + O3 ----> OH  |   1.00E-14e<sup>  -490.00/T</sup> |   1.9331E-15 |
| 38   | HO2 + HO2 ----> HO2H  | k<sub>0</sub>=  3.00E-13e<sup>   460.0/T</sup><br>k<sub>1</sub>=  2.10E-33e<sup>   920.0/T</sup> |   2.5345E-12 |
| 39   | HO2 + HO2 + H2O ----> HO2H  | k<sub>0</sub>=  4.20E-34e<sup>  2660.0/T</sup><br>k<sub>1</sub>=  2.94E-54e<sup>  3120.0/T</sup> |   5.6834E-30 |
| 40   | NO3 + HO2 ----> OH + NO2  |   3.5000E-12 |   3.5000E-12 |
| 41   | NO3 + NO3 ---->   2.0000\*NO2  |   8.50E-13e<sup> -2450.00/T</sup> |   2.2944E-16 |
| 42   | HO2H ---->   2.0000\*OH  | H2O2 | Not Available<sup>1</sup> | 
| 43   | HO2H + OH ----> HO2  |   1.8000E-12 |   1.8000E-12 |
| 44   | OH + HO2 ----> |   4.80E-11e<sup>   250.00/T</sup> |   1.1102E-10 |
| 45   | CO + OH ----> HO2 + CO2  | k<sub>0</sub>=  1.44E-13e<sup>     0.0/T</sup><br>k<sub>1</sub>=  3.43E-33e<sup>     0.0/T</sup> |   2.2843E-13 |
| 46   | OH + H2 ----> HO2  |   2.80E-12e<sup> -1800.00/T</sup> |   6.6869E-15 |
| R2NO   | SumRO2 + NO ----> NO  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| R2H2   | SumRO2 + HO2 ----> HO2  |   1.4900E-11 |   1.4900E-11 |
| R2N3   | SumRO2 + NO3 ----> NO3  |   2.3000E-12 |   2.3000E-12 |
| R2R2   | SumRO2 + SumRO2 ----> |   1.6000E-14 |   1.6000E-14 |
| R3N2   | SumRCO3 + NO2 ----> NO2  |   7.70E-12(T/300)<sup> -0.20</sup> |   7.7095E-12 |
| R3NO   | SumRCO3 + NO ----> NO  |   6.70E-12e<sup>   340.00/T</sup> |   2.0957E-11 |
| R3H2   | SumRCO3 + HO2 ----> HO2  |   3.14E-12e<sup>   580.00/T</sup> |   2.1967E-11 |
| R3N3   | SumRCO3 + NO3 ----> NO3  |   4.0000E-12 |   4.0000E-12 |
| R3R2   | SumRCO3 + SumRO2 ----> |   4.40E-13e<sup>  1070.00/T</sup> |   1.5924E-11 |
| R3R3   | SumRCO3 + SumRCO3 ----> |   1.7000E-11 |   1.7000E-11 |
| 57   | RO2C + NO ----> NO2  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 58   | RO2C + HO2 ----> |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 59   | RO2C + NO3 ----> NO2  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 60   | RO2C + SumRO2 ----> SumRO2  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 61   | RO2C + SumRCO3 ----> SumRCO3  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 62   | RO2XC + NO ----> |   R2NO |   9.1214E-12<sup>7</sup>| 
| 63   | RO2XC + HO2 ----> |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 64   | RO2XC + NO3 ----> NO2  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 65   | RO2XC + SumRO2 ----> SumRO2  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 66   | RO2XC + SumRCO3 ----> SumRCO3  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 67   | MEO2 + NO ----> NO2 + HCHO + HO2  |   2.80E-12e<sup>   300.00/T</sup> |   7.6586E-12 |
| 68   | MEO2 + HO2 ---->   0.9000\*MEOOH +    0.1000\*HCHO  |   3.80E-13e<sup>   780.00/T</sup> |   5.1994E-12 |
| 69   | MEO2 + NO3 ----> HCHO + HO2 + NO2  |   1.2000E-12 |   1.2000E-12 |
| 70   | MEO2 + SumRO2 ---->   0.3000\*HO2 +    0.6500\*HCHO +    0.3500\*MEOH  |   2.1600E-13 |   2.1600E-13 |
| 71   | MEO2 + SumRCO3 ---->   0.9000\*HO2 + HCHO  |   2.00E-12e<sup>   500.00/T</sup> |   1.0699E-11 |
| 72   | ETO2 + NO ----> NO2 + HO2 + MECHO  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| 73   | ETO2 + HO2 ----> ROOH  |   7.4400E-12 |   7.4400E-12 |
| 74   | ETO2 + NO3 ----> NO2 + HO2 + MECHO  |   2.3000E-12 |   2.3000E-12 |
| 75   | ETO2 + SumRO2 ---->   0.5000\*HO2 +    0.2500\*ETOH +    0.7500\*MECHO  |   2.9000E-14 |   2.9000E-14 |
| 76   | ETO2 + SumRCO3 ---->   0.8000\*HO2 + MECHO  |   1.6000E-11 |   1.6000E-11 |
| 77   | BZO2 + NO ----> NO2 + BZO  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 78   | BZO2 + HO2 ----> ROOH  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 79   | BZO2 + NO3 ----> BZO + NO2  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 80   | BZO2 + SumRO2 ----> SumRO2 + BZO  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 81   | BZO2 + SumRCO3 ----> SumRCO3 + BZO  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| Q1NO   | MECO3 + NO ----> NO2 + MEO2 + CO2 + SumRO2  |   8.10E-12e<sup>   270.00/T</sup> |   2.0034E-11 |
| Q1N2   | MECO3 + NO2 ----> PAN  | k<sub>o</sub>=  9.70E-29e<sup>     0.0/T</sup>(T/300)<sup> -5.60</sup><br>k<sub>i</sub> =   9.30E-12e<sup>     0.0/T</sup>(T/300)<sup> -1.50</sup><br>n=     1.00;F=     0.60 |   8.6800E-12 |
| Q1N3   | MECO3 + NO3 ----> NO2 + MEO2 + CO2 + SumRO2  |   4.0000E-12 |   4.0000E-12 |
| Q1H2   | MECO3 + HO2 ---->   0.1300\*O3 +    0.5000\*OH +    0.5000\*MEO2 +    0.1300\*OACID +    0.3700\*PACID +    0.5000\*CO2 +    0.5000\*SumRO2  |   2.2000E-11 |   2.2000E-11 |
| Q1R2   | MECO3 + SumRO2 ---->   0.9000\*MEO2 +    0.1000\*OACID +    0.9000\*CO2 +    0.9000\*SumRO2  |   1.6000E-11 |   1.6000E-11 |
| Q1R3   | MECO3 + SumRCO3 ----> MEO2 + CO2 + SumRO2  |   1.4000E-11 |   1.4000E-11 |
| Q6N2   | BZCO3 + NO2 ----> PBZN  |   1.1100E-11 |   1.1100E-11 |
| Q6NO   | BZCO3 + NO ----> NO2 + CO2 + BZO2 + SumRO2  |   1.6000E-11 |   1.6000E-11 |
| Q6H2   | BZCO3 + HO2 ---->   0.5000\*ALK5 +    0.1300\*O3 +    0.5000\*OH +    0.5000\*BZO2 +    0.5000\*CO2 +    0.5000\*SumRO2  |   R3H2 |   2.1967E-11<sup>7</sup>| 
| Q6N3   | BZCO3 + NO3 ----> NO2 + CO2 + BZO2 + SumRO2  |   R3N3 |   4.0000E-12<sup>7</sup>| 
| Q6R2   | BZCO3 + SumRO2 ---->   2.0000\*SumRO2 + BZO2 + CO2  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| Q6R3   | BZCO3 + SumRCO3 ----> SumRCO3 + CO2 + BZO2 + SumRO2  |   R3R3 |   1.7000E-11<sup>7</sup>| 
| 94   | TBUO + NO2 ----> R1NO3  |   3.50E-12e<sup>   553.00/T</sup> |   2.2366E-11 |
| 95   | TBUO ----> ACET + MEO2 + SumRO2  |   1.40E+13e<sup> -6856.00/T</sup> |   1.4437E+03 |
| 96   | BZO + NO2 ----> NPHE  |   2.0800E-12 |   2.0800E-12 |
| 97   | BZO + HO2 ----> CRES  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 98   | BZO + O3 ----> BZO2 + SumRO2  |   2.8600E-13 |   2.8600E-13 |
| 99   | BZO + BZO ----> |   1.4900E-11 |   1.4900E-11 |
| 100   | NPRAD + NO2 ----> NPHE  |   R3N2 |   7.7095E-12<sup>7</sup>| 
| 101   | NPRAD + HO2 ----> NAPPRD  |   R3H2 |   2.1967E-11<sup>7</sup>| 
| 102   | NPRAD ----> NAPPRD  |   1.0000E-03 |   1.0000E-03 |
| 103   | PNAMIN + NO2 ----> NAMIN  |   R3N2 |   7.7095E-12<sup>7</sup>| 
| 104   | PNAMIN + HO2 ----> AMINS  |   R3H2 |   2.1967E-11<sup>7</sup>| 
| 105   | PNAMIN ----> AMINS  |   1.0000E-03 |   1.0000E-03 |
| G1N2   | HCHO2 + NO2 ----> HCHO + NO3  |   7.0000E-12 |   7.0000E-12 |
| G1WA   | HCHO2 + H2O ----> HCOOH  |   2.4000E-15 |   2.4000E-15 |
| G1S2   | HCHO2 + SO2 ----> SULF + HCHO + SULRXN  |   3.8000E-11 |   3.8000E-11 |
| G2N2   | MECHO2 + NO2 ----> MECHO + NO3  |   7.0000E-12 |   7.0000E-12 |
| G2WA   | MECHO2 + H2O ----> OACID  |   2.4000E-15 |   2.4000E-15 |
| G2S2   | MECHO2 + SO2 ----> SULF + MECHO + SULRXN  |   3.8000E-11 |   3.8000E-11 |
| G3N2   | RCHO2 + NO2 ----> RCHO + NO3  |   7.0000E-12 |   7.0000E-12 |
| G3WA   | RCHO2 + H2O ----> OACID  |   2.4000E-15 |   2.4000E-15 |
| G3S2   | RCHO2 + SO2 ----> SULF + RCHO + SULRXN  |   3.8000E-11 |   3.8000E-11 |
| S2OH   | SO2 + OH ----> HO2 + SULF + SULRXN  | k<sub>o</sub>=  3.30E-31e<sup>     0.0/T</sup>(T/300)<sup> -4.30</sup><br>k<sub>i</sub> =   1.60E-12e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.00;F=     0.60 |   9.5810E-13 |
| C1OH   | OH + CH4 ----> MEO2 + SumRO2  |   2.45E-12e<sup> -1775.00/T</sup> |   6.3628E-15 |
| 117   | HCHO ---->   2.0000\*HO2 + CO  | HCHOR_13 | Not Available<sup>1</sup> | 
| 118   | HCHO ----> CO  | HCHOM_13 | Not Available<sup>1</sup> | 
| 119   | HCHO + OH ----> HO2 + CO  |   5.50E-12e<sup>   125.00/T</sup> |   8.3645E-12 |
| 120   | HCHO + NO3 ----> HNO3 + HO2 + CO  |   5.8000E-16 |   5.8000E-16 |
| P1UI   | PAN ----> NO2 + MECO3 + SumRCO3  | k<sub>o</sub>=  1.08E+00e<sup>-14000.0/T</sup>(T/300)<sup> -5.60</sup><br>k<sub>i</sub> =   1.03E+17e<sup>-14000.0/T</sup>(T/300)<sup> -1.50</sup><br>n=     1.00;F=     0.60 |   3.8914E-04 |
| P1HV   | PAN ---->   0.6000\*NO2 +    0.4000\*NO3 +    0.4000\*MEO2 +    0.6000\*MECO3 +    0.4000\*CO2 +    0.4000\*SumRO2 +    0.6000\*SumRCO3  | PAN_11 | Not Available<sup>1</sup> | 
| 123   | GLY ---->   2.0000\*CO +    2.0000\*HO2  | GLY_I13R | Not Available<sup>1</sup> | 
| 124   | GLY ----> HCHO + CO  | GLY_I13M | Not Available<sup>1</sup> | 
| 125   | GLY + OH ---->   1.7000\*CO +    0.7000\*HO2 +    0.3000\*OH +    0.3000\*CO2  |   1.1500E-11 |   1.1500E-11 |
| 126   | GLY + NO3 ----> HNO3 +    1.7000\*CO +    0.7000\*HO2 +    0.3000\*OH +    0.3000\*CO2  |   4.0000E-16 |   4.0000E-16 |
| BLOH   | BALD + OH ----> BZCO3 + SumRCO3  |   1.2000E-11 |   1.2000E-11 |
| BLHV   | BALD ----> |   9.0000E-02*BALD_11 | Not Available<sup>1</sup> | 
| BLN3   | BALD + NO3 ----> HNO3 + BZCO3 + SumRCO3  |   4.0000E-15 |   4.0000E-15 |
| PBUI   | PBZN ----> BZCO3 + NO2 + SumRCO3  |   2.10E+16e<sup>-13600.00/T</sup> |   3.2512E-04 |
| PBHV   | PBZN ---->   0.6000\*BZCO3 +    0.6000\*NO2 +    0.4000\*CO2 +    0.4000\*BZO2 +    0.4000\*NO3 +    0.4000\*SumRO2 +    0.6000\*SumRCO3  | PPN_11 | Not Available<sup>1</sup> | 
| NPOH   | NPHE + OH ----> BZO + NO2  |   3.5000E-12 |   3.5000E-12 |
| NPHV   | NPHE ----> HONO + PHEN  |   1.5000E-03*NO2_06 | Not Available<sup>1</sup> | 
| NAOH   | NAPS + OH ---->   0.7410\*HO2 +    0.7070\*CATL +    0.0340\*RO2C +    0.0170\*AFG2A +    0.0170\*AFG2B +    0.0340\*GLY +    0.3300\*NPRAD +    0.2500\*MACO3 +    0.0340\*SumRO2 +    0.2500\*SumRCO3 + PAHRO2  |   1.55E-11e<sup>   117.00/T</sup> |   2.2949E-11 |
| CTOH   | CATL3 + OH ----> HO2 + OTHN  |   5.9700E-10 |   5.9700E-10 |
| CTN3   | CATL3 + NO3 ----> HNO3 + OTHN  |   4.8600E-10 |   4.8600E-10 |
| PNOH   | NAPPRD + OH ----> HO2 + OTHN  |   2.0000E-10 |   2.0000E-10 |
| PNN3   | NAPPRD + NO3 ----> HNO3 + OTHN  |   1.7000E-10 |   1.7000E-10 |
| 139   | PHOT ---->   2.0000\*HO2 +    2.0000\*RO2C +    2.0000\*SumRO2 + ALK3  | BACL_11 | Not Available<sup>1</sup> | 
| 140   | IMINE ----> MECHO  |   2.7800E-04 |   2.7800E-04 |
| 141   | CLETHE + OH ----> xHO2 + RO2C + xHCHO + yROOH + SumRO2  |   2.54E-12e<sup>   325.00/T</sup> |   7.5551E-12 |
| 142   | ACRLNT + OH ----> xHO2 + RO2C + xHCHO + yROOH + SumRO2  |   4.1300E-12 |   4.1300E-12 |
| 143   | PCE + OH ----> xHO2 + RO2C + yROOH + SumRO2  |   3.50E-12e<sup>  -920.00/T</sup> |   1.5994E-13 |
| 144   | PCLBEN + OH ----> xHO2 + RO2C + yROOH + SumRO2  |   4.0300E-13 |   4.0300E-13 |
| 145   | MECL2 + OH ----> xHO2 + RO2C + yROOH + SumRO2  |   1.80E-12e<sup>  -860.00/T</sup> |   1.0059E-13 |
| 146   | ETBR2 + OH ----> xHO2 + RO2C + yROOH + SumRO2  |   7.69E-12e<sup> -1056.00/T</sup> |   2.2270E-13 |
| 147   | ETCL2 + OH ----> xHO2 + RO2C + yROOH + SumRO2  |   8.69E-12e<sup> -1070.00/T</sup> |   2.4012E-13 |
| 148   | ETOX + OH ----> xHO2 + RO2C + yROOH + SumRO2  |   1.63E-12e<sup>  -856.00/T</sup> |   9.2323E-14 |
| 149   | CHCL3 + OH ----> xHO2 + RO2C + yROOH + SumRO2  |   1.80E-12e<sup>  -850.00/T</sup> |   1.0402E-13 |
| 150   | xHO2 + NO ----> NO + HO2  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 151   | xHO2 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 152   | xHO2 + NO3 ----> NO3 + HO2  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 153   | xHO2 + SumRO2 ----> SumRO2 +    0.5000\*HO2  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 154   | xHO2 + SumRCO3 ----> SumRCO3 + HO2  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 155   | xOH + NO ----> NO + OH  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 156   | xOH + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 157   | xOH + NO3 ----> NO3 + OH  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 158   | xOH + SumRO2 ----> SumRO2 +    0.5000\*OH  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 159   | xOH + SumRCO3 ----> SumRCO3 + OH  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 160   | xNO2 + NO ----> NO + NO2  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 161   | xNO2 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 162   | xNO2 + NO3 ----> NO3 + NO2  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 163   | xNO2 + SumRO2 ----> SumRO2 +    0.5000\*NO2  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 164   | xNO2 + SumRCO3 ----> SumRCO3 + NO2  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 165   | xNO3 + NO ----> NO + NO3  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 166   | xNO3 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 167   | xNO3 + NO3 ---->   2.0000\*NO3  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 168   | xNO3 + SumRO2 ----> SumRO2 +    0.5000\*NO3  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 169   | xNO3 + SumRCO3 ----> SumRCO3 + NO3  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 170   | xHCHO + NO ----> NO + HCHO  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 171   | xHCHO + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 172   | xHCHO + NO3 ----> NO3 + HCHO  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 173   | xHCHO + SumRO2 ----> SumRO2 +    0.5000\*HCHO  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 174   | xHCHO + SumRCO3 ----> SumRCO3 + HCHO  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 175   | xGLY + NO ----> NO + GLY  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 176   | xGLY + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 177   | xGLY + NO3 ----> NO3 + GLY  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 178   | xGLY + SumRO2 ----> SumRO2 +    0.5000\*GLY  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 179   | xGLY + SumRCO3 ----> SumRCO3 + GLY  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 180   | xHCOOH + NO ----> NO + HCOOH  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 181   | xHCOOH + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 182   | xHCOOH + NO3 ----> NO3 + HCOOH  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 183   | xHCOOH + SumRO2 ----> SumRO2 +    0.5000\*HCOOH  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 184   | xHCOOH + SumRCO3 ----> SumRCO3 + HCOOH  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 185   | xMECHO + NO ----> NO + MECHO  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 186   | xMECHO + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 187   | xMECHO + NO3 ----> NO3 + MECHO  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 188   | xMECHO + SumRO2 ----> SumRO2 +    0.5000\*MECHO  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 189   | xMECHO + SumRCO3 ----> SumRCO3 + MECHO  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 190   | xETCHO + NO ----> NO + ETCHO  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 191   | xETCHO + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 192   | xETCHO + NO3 ----> NO3 + ETCHO  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 193   | xETCHO + SumRO2 ----> SumRO2 +    0.5000\*ETCHO  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 194   | xETCHO + SumRCO3 ----> SumRCO3 + ETCHO  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 195   | xGLCHO + NO ----> NO + GLCHO  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 196   | xGLCHO + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 197   | xGLCHO + NO3 ----> NO3 + GLCHO  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 198   | xGLCHO + SumRO2 ----> SumRO2 +    0.5000\*GLCHO  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 199   | xGLCHO + SumRCO3 ----> SumRCO3 + GLCHO  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 200   | xMEK + NO ----> NO + MEK  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 201   | xMEK + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 202   | xMEK + NO3 ----> NO3 + MEK  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 203   | xMEK + SumRO2 ----> SumRO2 +    0.5000\*MEK  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 204   | xMEK + SumRCO3 ----> SumRCO3 + MEK  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 205   | xACRO + NO ----> NO + ACRO  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 206   | xACRO + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 207   | xACRO + NO3 ----> NO3 + ACRO  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 208   | xACRO + SumRO2 ----> SumRO2 +    0.5000\*ACRO  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 209   | xACRO + SumRCO3 ----> SumRCO3 + ACRO  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 210   | xACET + NO ----> NO + ACET  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 211   | xACET + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 212   | xACET + NO3 ----> NO3 + ACET  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 213   | xACET + SumRO2 ----> SumRO2 +    0.5000\*ACET  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 214   | xACET + SumRCO3 ----> SumRCO3 + ACET  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 215   | xMACR + NO ----> NO + MACR  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 216   | xMACR + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 217   | xMACR + NO3 ----> NO3 + MACR  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 218   | xMACR + SumRO2 ----> SumRO2 +    0.5000\*MACR  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 219   | xMACR + SumRCO3 ----> SumRCO3 + MACR  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 220   | xMVK + NO ----> NO + MVK  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 221   | xMVK + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 222   | xMVK + NO3 ----> NO3 + MVK  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 223   | xMVK + SumRO2 ----> SumRO2 +    0.5000\*MVK  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 224   | xMVK + SumRCO3 ----> SumRCO3 + MVK  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 225   | xBACL + NO ----> NO + BACL  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 226   | xBACL + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 227   | xBACL + NO3 ----> NO3 + BACL  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 228   | xBACL + SumRO2 ----> SumRO2 +    0.5000\*BACL  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 229   | xBACL + SumRCO3 ----> SumRCO3 + BACL  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 230   | xMGLY + NO ----> NO + MGLY  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 231   | xMGLY + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 232   | xMGLY + NO3 ----> NO3 + MGLY  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 233   | xMGLY + SumRO2 ----> SumRO2 +    0.5000\*MGLY  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 234   | xMGLY + SumRCO3 ----> SumRCO3 + MGLY  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 235   | xBUDAL + NO ----> NO + BUDAL  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 236   | xBUDAL + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 237   | xBUDAL + NO3 ----> NO3 + BUDAL  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 238   | xBUDAL + SumRO2 ----> SumRO2 +    0.5000\*BUDAL  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 239   | xBUDAL + SumRCO3 ----> SumRCO3 + BUDAL  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 240   | xFURNS + NO ----> NO + FURNS  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 241   | xFURNS + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 242   | xFURNS + NO3 ----> NO3 + FURNS  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 243   | xFURNS + SumRO2 ----> SumRO2 +    0.5000\*FURNS  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 244   | xFURNS + SumRCO3 ----> SumRCO3 + FURNS  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 245   | xBALD + NO ----> NO + BALD  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 246   | xBALD + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 247   | xBALD + NO3 ----> NO3 + BALD  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 248   | xBALD + SumRO2 ----> SumRO2 +    0.5000\*BALD  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 249   | xBALD + SumRCO3 ----> SumRCO3 + BALD  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 250   | xBENX + NO ----> NO + BENX  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 251   | xBENX + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 252   | xBENX + NO3 ----> NO3 + BENX  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 253   | xBENX + SumRO2 ----> SumRO2 +    0.5000\*BENX  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 254   | xBENX + SumRCO3 ----> SumRCO3 + BENX  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 255   | xRCHO + NO ----> NO + RCHO  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 256   | xRCHO + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 257   | xRCHO + NO3 ----> NO3 + RCHO  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 258   | xRCHO + SumRO2 ----> SumRO2 +    0.5000\*RCHO  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 259   | xRCHO + SumRCO3 ----> SumRCO3 + RCHO  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 260   | xKET2 + NO ----> NO + KET2  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 261   | xKET2 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 262   | xKET2 + NO3 ----> NO3 + KET2  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 263   | xKET2 + SumRO2 ----> SumRO2 +    0.5000\*KET2  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 264   | xKET2 + SumRCO3 ----> SumRCO3 + KET2  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 265   | xLVKS + NO ----> NO + LVKS  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 266   | xLVKS + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 267   | xLVKS + NO3 ----> NO3 + LVKS  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 268   | xLVKS + SumRO2 ----> SumRO2 +    0.5000\*LVKS  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 269   | xLVKS + SumRCO3 ----> SumRCO3 + LVKS  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 270   | xOLEA1 + NO ----> NO + OLEA1  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 271   | xOLEA1 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 272   | xOLEA1 + NO3 ----> NO3 + OLEA1  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 273   | xOLEA1 + SumRO2 ----> SumRO2 +    0.5000\*OLEA1  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 274   | xOLEA1 + SumRCO3 ----> SumRCO3 + OLEA1  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 275   | xOLEA2 + NO ----> NO + OLEA2  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 276   | xOLEA2 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 277   | xOLEA2 + NO3 ----> NO3 + OLEA2  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 278   | xOLEA2 + SumRO2 ----> SumRO2 +    0.5000\*OLEA2  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 279   | xOLEA2 + SumRCO3 ----> SumRCO3 + OLEA2  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 280   | xOLEP + NO ----> NO + OLEP  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 281   | xOLEP + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 282   | xOLEP + NO3 ----> NO3 + OLEP  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 283   | xOLEP + SumRO2 ----> SumRO2 +    0.5000\*OLEP  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 284   | xOLEP + SumRCO3 ----> SumRCO3 + OLEP  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 285   | xOACID + NO ----> NO + OACID  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 286   | xOACID + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 287   | xOACID + NO3 ----> NO3 + OACID  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 288   | xOACID + SumRO2 ----> SumRO2 +    0.5000\*OACID  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 289   | xOACID + SumRCO3 ----> SumRCO3 + OACID  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 290   | xPACID + NO ----> NO + PACID  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 291   | xPACID + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 292   | xPACID + NO3 ----> NO3 + PACID  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 293   | xPACID + SumRO2 ----> SumRO2 +    0.5000\*PACID  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 294   | xPACID + SumRCO3 ----> SumRCO3 + PACID  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 295   | xAMINS + NO ----> NO + AMINS  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 296   | xAMINS + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 297   | xAMINS + NO3 ----> NO3 + AMINS  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 298   | xAMINS + SumRO2 ----> SumRO2 +    0.5000\*AMINS  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 299   | xAMINS + SumRCO3 ----> SumRCO3 + AMINS  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 305   | xRPNO3 + NO ----> NO + RPNO3  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 306   | xRPNO3 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 307   | xRPNO3 + NO3 ----> NO3 + RPNO3  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 308   | xRPNO3 + SumRO2 ----> SumRO2 +    0.5000\*RPNO3  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 309   | xRPNO3 + SumRCO3 ----> SumRCO3 + RPNO3  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 310   | xRCNO3 + NO ----> NO + RCNO3  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 311   | xRCNO3 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 312   | xRCNO3 + NO3 ----> NO3 + RCNO3  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 313   | xRCNO3 + SumRO2 ----> SumRO2 +    0.5000\*RCNO3  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 314   | xRCNO3 + SumRCO3 ----> SumRCO3 + RCNO3  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 315   | xRHNO3 + NO ----> NO + RHNO3  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 316   | xRHNO3 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 317   | xRHNO3 + NO3 ----> NO3 + RHNO3  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 318   | xRHNO3 + SumRO2 ----> SumRO2 +    0.5000\*RHNO3  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 319   | xRHNO3 + SumRCO3 ----> SumRCO3 + RHNO3  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 320   | xRDNO3 + NO ----> NO + RDNO3  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 321   | xRDNO3 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 322   | xRDNO3 + NO3 ----> NO3 + RDNO3  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 323   | xRDNO3 + SumRO2 ----> SumRO2 +    0.5000\*RDNO3  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 324   | xRDNO3 + SumRCO3 ----> SumRCO3 + RDNO3  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 330   | xHPCRB + NO ----> NO + HPCRB  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 331   | xHPCRB + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 332   | xHPCRB + NO3 ----> NO3 + HPCRB  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 333   | xHPCRB + SumRO2 ----> SumRO2 +    0.5000\*HPCRB  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 334   | xHPCRB + SumRCO3 ----> SumRCO3 + HPCRB  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 335   | xAFG1 + NO ----> NO + AFG1  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 336   | xAFG1 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 337   | xAFG1 + NO3 ----> NO3 + AFG1  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 338   | xAFG1 + SumRO2 ----> SumRO2 +    0.5000\*AFG1  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 339   | xAFG1 + SumRCO3 ----> SumRCO3 + AFG1  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 340   | xAFG2A + NO ----> NO + AFG2A  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 341   | xAFG2A + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 342   | xAFG2A + NO3 ----> NO3 + AFG2A  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 343   | xAFG2A + SumRO2 ----> SumRO2 +    0.5000\*AFG2A  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 344   | xAFG2A + SumRCO3 ----> SumRCO3 + AFG2A  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 345   | xAFG2B + NO ----> NO + AFG2B  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 346   | xAFG2B + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 347   | xAFG2B + NO3 ----> NO3 + AFG2B  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 348   | xAFG2B + SumRO2 ----> SumRO2 +    0.5000\*AFG2B  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 349   | xAFG2B + SumRCO3 ----> SumRCO3 + AFG2B  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 350   | xAFG3 + NO ----> NO + AFG3  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 351   | xAFG3 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 352   | xAFG3 + NO3 ----> NO3 + AFG3  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 353   | xAFG3 + SumRO2 ----> SumRO2 +    0.5000\*AFG3  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 354   | xAFG3 + SumRCO3 ----> SumRCO3 + AFG3  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 355   | xPAN2 + NO ----> NO + PAN2  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 356   | xPAN2 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 357   | xPAN2 + NO3 ----> NO3 + PAN2  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 358   | xPAN2 + SumRO2 ----> SumRO2 +    0.5000\*PAN2  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 359   | xPAN2 + SumRCO3 ----> SumRCO3 + PAN2  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 365   | xMEO2 + NO ----> NO + MEO2 + SumRO2  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 366   | xMEO2 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 367   | xMEO2 + NO3 ----> NO3 + MEO2 + SumRO2  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 368   | xMEO2 + SumRO2 ---->   1.5000\*SumRO2 +    0.5000\*MEO2  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 369   | xMEO2 + SumRCO3 ----> SumRCO3 + MEO2 + SumRO2  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 370   | xETO2 + NO ----> NO + ETO2 + SumRO2  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 371   | xETO2 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 372   | xETO2 + NO3 ----> NO3 + ETO2 + SumRO2  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 373   | xETO2 + SumRO2 ---->   1.5000\*SumRO2 +    0.5000\*ETO2  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 374   | xETO2 + SumRCO3 ----> SumRCO3 + ETO2 + SumRO2  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 375   | xMECO3 + NO ----> NO + MECO3 + SumRCO3  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 376   | xMECO3 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 377   | xMECO3 + NO3 ----> NO3 + MECO3 + SumRCO3  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 378   | xMECO3 + SumRO2 ----> SumRO2 +    0.5000\*MECO3 +    0.5000\*SumRCO3  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 379   | xMECO3 + SumRCO3 ---->   2.0000\*SumRCO3 + MECO3  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 380   | xR2CO3 + NO ----> NO + R2CO3 + SumRCO3  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 381   | xR2CO3 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 382   | xR2CO3 + NO3 ----> NO3 + R2CO3 + SumRCO3  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 383   | xR2CO3 + SumRO2 ----> SumRO2 +    0.5000\*R2CO3 +    0.5000\*SumRCO3  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 384   | xR2CO3 + SumRCO3 ---->   2.0000\*SumRCO3 + R2CO3  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 385   | xMACO3 + NO ----> NO + MACO3 + SumRCO3  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 386   | xMACO3 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 387   | xMACO3 + NO3 ----> NO3 + MACO3 + SumRCO3  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 388   | xMACO3 + SumRO2 ----> SumRO2 +    0.5000\*MACO3 +    0.5000\*SumRCO3  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 389   | xMACO3 + SumRCO3 ---->   2.0000\*SumRCO3 + MACO3  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 390   | xTBUO + NO ----> NO + TBUO  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 391   | xTBUO + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 392   | xTBUO + NO3 ----> NO3 + TBUO  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 393   | xTBUO + SumRO2 ----> SumRO2 +    0.5000\*TBUO  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 394   | xTBUO + SumRCO3 ----> SumRCO3 + TBUO  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 395   | xBZO + NO ----> NO + BZO  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 396   | xBZO + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 397   | xBZO + NO3 ----> NO3 + BZO  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 398   | xBZO + SumRO2 ----> SumRO2 +    0.5000\*BZO  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 399   | xBZO + SumRCO3 ----> SumRCO3 + BZO  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 400   | yROOH + NO ----> NO  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 401   | yROOH + HO2 ----> HO2 + ROOH  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 402   | yROOH + NO3 ----> NO3  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 403   | yROOH + SumRO2 ----> SumRO2 +    0.5000\*KET2  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 404   | yROOH + SumRCO3 ----> SumRCO3  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 405   | yRUOOH + NO ----> NO  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 406   | yRUOOH + HO2 ----> HO2 + RUOOH  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 407   | yRUOOH + NO3 ----> NO3  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 408   | yRUOOH + SumRO2 ----> SumRO2 +    0.5000\*OLEP  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 409   | yRUOOH + SumRCO3 ----> SumRCO3  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 410   | yRAOOH + NO ----> NO  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 411   | yRAOOH + HO2 ----> HO2 + RAOOH  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 412   | yRAOOH + NO3 ----> NO3  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 413   | yRAOOH + SumRO2 ----> SumRO2 +    0.5000\*OLEP  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 414   | yRAOOH + SumRCO3 ----> SumRCO3  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 415   | yHPCRB + NO ----> NO  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 416   | yHPCRB + HO2 ----> HO2 + HPCRB  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 417   | yHPCRB + NO3 ----> NO3  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 418   | yHPCRB + SumRO2 ----> SumRO2 +    0.5000\*KET2  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 419   | yHPCRB + SumRCO3 ----> SumRCO3  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 420   | yRPNO3 + NO ----> NO  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 421   | yRPNO3 + HO2 ----> HO2 + RPNO3  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 422   | yRPNO3 + NO3 ----> NO3  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 423   | yRPNO3 + SumRO2 ----> SumRO2 +    0.5000\*R1NO3  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 424   | yRPNO3 + SumRCO3 ----> SumRCO3  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 425   | zR1NO3 + NO ----> NO + R1NO3  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 426   | zR1NO3 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 427   | zR1NO3 + NO3 ----> NO3 + KET2 + HO2  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 428   | zR1NO3 + SumRO2 ----> SumRO2 +    0.5000\*KET2 +    0.5000\*HO2  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 429   | zR1NO3 + SumRCO3 ----> SumRCO3 + KET2 + HO2  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 430   | zR2NO3 + NO ----> NO + R2NO3  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 431   | zR2NO3 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 432   | zR2NO3 + NO3 ----> NO3 + KET2 + HO2  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 433   | zR2NO3 + SumRO2 ----> SumRO2 +    0.5000\*KET2 +    0.5000\*HO2  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 434   | zR2NO3 + SumRCO3 ----> SumRCO3 + KET2 + HO2  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 435   | zRHNO3 + NO ----> NO + RHNO3  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 436   | zRHNO3 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 437   | zRHNO3 + NO3 ----> NO3 + KET2 + HO2  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 438   | zRHNO3 + SumRO2 ----> SumRO2 +    0.5000\*KET2 +    0.5000\*HO2  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 439   | zRHNO3 + SumRCO3 ----> SumRCO3 + KET2 + HO2  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 440   | zRCNO3 + NO ----> NO + RCNO3  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 441   | zRCNO3 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 442   | zRCNO3 + NO3 ----> NO3 + KET2 + HO2  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 443   | zRCNO3 + SumRO2 ----> SumRO2 +    0.5000\*KET2 +    0.5000\*HO2  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 444   | zRCNO3 + SumRCO3 ----> SumRCO3 + KET2 + HO2  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 445   | zRANO3 + NO ----> NO + RANO3  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 446   | zRANO3 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 447   | zRANO3 + NO3 ----> NO3 + RUOOH + HO2  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 448   | zRANO3 + SumRO2 ----> SumRO2 +    0.5000\*RUOOH +    0.5000\*HO2  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 449   | zRANO3 + SumRCO3 ----> SumRCO3 + RUOOH + HO2  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 450   | zRPNO3 + NO ----> NO + RPNO3  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 451   | zRPNO3 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 452   | zRPNO3 + NO3 ----> NO3 + RUOOH + HO2  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 453   | zRPNO3 + SumRO2 ----> SumRO2 +    0.5000\*RUOOH +    0.5000\*HO2  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 454   | zRPNO3 + SumRCO3 ----> SumRCO3 + RUOOH + HO2  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 455   | zRDNO3 + NO ----> NO + RDNO3  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 456   | zRDNO3 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 457   | zRDNO3 + NO3 ----> NO3 + R1NO3 + HO2  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 458   | zRDNO3 + SumRO2 ----> SumRO2 +    0.5000\*R1NO3 +    0.5000\*HO2  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 459   | zRDNO3 + SumRCO3 ----> SumRCO3 + R1NO3 + HO2  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 460   | zRNNO3 + NO ----> NO + RNNO3  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 461   | zRNNO3 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 462   | zRNNO3 + NO3 ----> NO3 + OTHN + HO2  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 463   | zRNNO3 + SumRO2 ----> SumRO2 +    0.5000\*OTHN +    0.5000\*HO2  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 464   | zRNNO3 + SumRCO3 ----> SumRCO3 + OTHN + HO2  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| 465   | zPAN2 + NO ----> NO + PAN2  |   R2NO |   9.1214E-12<sup>7</sup>| 
| 466   | zPAN2 + HO2 ----> HO2  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| 467   | zPAN2 + NO3 ----> NO3 + RCHO + HO2  |   R2N3 |   2.3000E-12<sup>7</sup>| 
| 468   | zPAN2 + SumRO2 ----> SumRO2 +    0.5000\*RCHO +    0.5000\*HO2  |   R2R2 |   1.6000E-14<sup>7</sup>| 
| 469   | zPAN2 + SumRCO3 ----> SumRCO3 + RCHO + HO2  |   R3R2 |   1.5924E-11<sup>7</sup>| 
| Q2NO   | R2CO3 + NO ----> NO2 +    0.9500\*xHO2 +    0.9600\*RO2C +    0.0400\*RO2XC +    0.9500\*xETCHO +    0.0400\*zR1NO3 + yROOH + CO2 + SumRO2  |   6.70E-12e<sup>   340.00/T</sup> |   2.0957E-11 |
| Q2N2   | R2CO3 + NO2 ----> PAN2  |   7.7000E-12 |   7.7000E-12 |
| Q2N3   | R2CO3 + NO3 ----> NO2 +    0.9500\*xHO2 +    0.9600\*RO2C +    0.0400\*RO2XC +    0.9500\*xETCHO +    0.0400\*zR1NO3 + yROOH + CO2 + SumRO2  |   4.0000E-12 |   4.0000E-12 |
| Q2H2   | R2CO3 + HO2 ---->   0.1300\*O3 +    0.5000\*OH +    0.4800\*xHO2 +    0.4800\*RO2C +    0.0200\*RO2XC +    0.4800\*xETCHO +    0.1300\*OACID +    0.3700\*PACID +    0.0200\*zR1NO3 +    0.5000\*yROOH +    0.5000\*CO2 +    0.5000\*SumRO2  |   2.2000E-11 |   2.2000E-11 |
| Q2R2   | R2CO3 + SumRO2 ---->   0.8600\*xHO2 +    0.8600\*RO2C +    0.0400\*RO2XC +    0.8600\*xETCHO +    0.1000\*OACID +    0.0400\*zR1NO3 +    0.9000\*yROOH +    0.9000\*CO2 +    0.9000\*SumRO2  |   1.6000E-11 |   1.6000E-11 |
| Q2R3   | R2CO3 + SumRCO3 ---->   0.9500\*xHO2 +    0.9600\*RO2C +    0.0400\*RO2XC +    0.9500\*xETCHO +    0.0400\*zR1NO3 + yROOH + CO2 + SumRO2  |   1.4000E-11 |   1.4000E-11 |
| Q5UI   | MACO3 ---->   0.9300\*xHO2 +    0.9300\*RO2C +    0.0700\*RO2XC +    0.9300\*xPACID +    0.0700\*zRCNO3 + SumRO2  |   7.79E+08e<sup> -5003.00/T</sup> |   4.0180E+01 |
| Q5NO   | MACO3 + NO ----> NO2 + MEO2 + HCHO + CO2 + CO + SumRO2  |   6.70E-12e<sup>   340.00/T</sup> |   2.0957E-11 |
| Q5N2   | MACO3 + NO2 ----> APANS  |   7.7000E-12 |   7.7000E-12 |
| Q5N3   | MACO3 + NO3 ----> NO2 + MEO2 + HCHO + CO2 + CO + SumRO2  |   4.0000E-12 |   4.0000E-12 |
| Q5H2   | MACO3 + HO2 ---->   0.1300\*O3 +    0.5000\*OH +    0.5000\*MEO2 +    0.5000\*HCHO +    0.3700\*PACID +    0.1300\*OLEP +    0.5000\*CO2 +    0.5000\*CO +    0.5000\*SumRO2  |   2.2000E-11 |   2.2000E-11 |
| Q5R2   | MACO3 + SumRO2 ---->   0.9000\*MEO2 +    0.9000\*HCHO +    0.1000\*OLEP +    0.9000\*CO2 +    0.9000\*CO +    0.9000\*SumRO2  |   1.6000E-11 |   1.6000E-11 |
| Q5R3   | MACO3 + SumRCO3 ----> MEO2 + HCHO + CO2 + CO + SumRO2  |   1.4000E-11 |   1.4000E-11 |
| C2OH   | ETHAN + OH ----> ETO2 + SumRO2  |   1.51E-12e<sup>  -533.00/T</sup>(T/300)<sup>  1.92 </sup> |   2.4971E-13 |
| C3OH   | PROP + OH ---->   0.9500\*xHO2 +    0.9600\*RO2C +    0.0400\*RO2XC +    0.0100\*xMEO2 +    0.0100\*xMECHO +    0.2700\*xETCHO +    0.6800\*xACET +    0.0400\*zR1NO3 + yROOH + SumRO2  |   2.00E-12e<sup>  -172.00/T</sup>(T/300)<sup>  1.76 </sup> |   1.1111E-12 |
| C4OH   | NC4 + OH ---->   0.5900\*xHO2 +    1.0200\*RO2C +    0.0800\*RO2XC +    0.3300\*xETO2 +    0.3300\*xMECHO +    0.1200\*xRCHO +    0.4800\*xMEK +    0.0700\*zR1NO3 +    0.0100\*zRHNO3 +    1.1000\*yROOH +    1.1000\*SumRO2  |   2.09E-12e<sup>    42.00/T</sup>(T/300)<sup>  1.82 </sup> |   2.3792E-12 |
| E1OH   | ETHEN + OH ----> xHO2 + RO2C +    1.4800\*xHCHO +    0.2600\*xGLCHO + yROOH + SumRO2  | k<sub>o</sub>=  1.10E-28e<sup>     0.0/T</sup>(T/300)<sup> -3.50</sup><br>k<sub>i</sub> =   8.40E-12e<sup>     0.0/T</sup>(T/300)<sup> -1.75</sup><br>n=     1.00;F=     0.60 |   7.8945E-12 |
| E1O3   | ETHEN + O3 ---->   0.1700\*OH +    0.2700\*HO2 +    0.4200\*HCHO2 + HCHO +    0.2300\*CO2 +    0.3500\*CO  |   6.82E-15e<sup> -2500.00/T</sup> |   1.5567E-18 |
| E1N3   | ETHEN + NO3 ---->   0.0100\*xNO2 +    0.9900\*xHO2 + RO2C +    0.0100\*xHCHO +    0.9900\*xRCNO3 + yRPNO3 + SumRO2  |   3.30E-12e<sup> -2880.00/T</sup> |   2.1058E-16 |
| E1OP   | ETHEN + O3P ----> ETHEN_OP +    0.8000\*HO2 +    0.2200\*xHO2 +    0.2900\*RO2C +    0.5100\*MEO2 +    0.0700\*xHCHO +    0.1000\*MECHO +    4.4100\*NROG +    0.5100\*CO +    0.8000\*SumRO2  |   1.07E-11e<sup>  -800.00/T</sup> |   7.3127E-13 |
| 495   | ETHEN_OP ---->   0.0700\*xOH +    0.2200\*xPACID +    0.0700\*CO2  |   2.5700E+00 |   2.5700E+00 |
| 496   | ETHEN_OP + NO ----> NO +    0.0700\*xHO2 +    0.2000\*xHCHO +    0.0200\*xGLY +    0.2400\*yHPCRB +    0.2700\*CO  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| E2OH   | PROPE + OH ---->   0.9700\*xHO2 +    0.9700\*RO2C +    0.0300\*RO2XC +    0.9700\*xHCHO +    0.9700\*xMECHO +    0.0300\*zRHNO3 + yROOH + SumRO2  |   1.20E-11e<sup>   210.00/T</sup>(T/300)<sup> -0.62 </sup> |   2.4363E-11 |
| E2O3   | PROPE + O3 ----> PROPE_O3 +    0.3000\*OH +    0.1700\*HO2 +    0.1600\*xHO2 +    0.2200\*RO2C +    0.0300\*MEO2 +    0.2100\*HCHO2 +    0.1200\*MECHO2 +    0.5000\*HCHO +    0.0500\*xHCHO +    0.5000\*MECHO +    0.0500\*MEOH +    0.2400\*CO2 +    0.2200\*CO +    0.2500\*SumRO2  |   5.77E-15e<sup> -1880.00/T</sup> |   1.0537E-17 |
| 499   | PROPE_O3 ---->   0.0500\*xOH +    0.1600\*xPACID +    0.0500\*CO2  |   2.4500E+00 |   2.4500E+00 |
| 500   | PROPE_O3 + NO ----> NO +    0.0600\*xHO2 +    0.1500\*xHCHO +    0.1800\*yHPCRB +    0.2000\*CO  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| E2N3   | PROPE + NO3 ---->   0.2900\*xNO2 +    0.6800\*xHO2 +    0.9700\*RO2C +    0.0300\*RO2XC +    0.2900\*xHCHO +    0.2900\*xMECHO +    0.6800\*xRCNO3 + yRPNO3 +    0.0300\*zRDNO3 + SumRO2  |   4.60E-13e<sup> -1155.00/T</sup> |   9.5576E-15 |
| E2OP   | PROPE + O3P ---->   0.2500\*ETCHO +    0.2500\*ACET +    0.5000\*ALK2  |   1.02E-11e<sup>  -280.00/T</sup> |   3.9879E-12 |
| IPOH   | ISOP + OH ----> ISOP_OH +    0.5500\*xHO2 +    0.5500\*RO2C +    0.0500\*RO2XC +    0.5000\*xHCHO +    0.2300\*xMACR +    0.2700\*xMVK +    0.0500\*zRHNO3 +    0.6000\*yRUOOH +    0.0500\*xFURNS +    0.6000\*SumRO2  |   2.70E-11e<sup>   390.00/T</sup> |   9.9873E-11 |
| 504   | ISOP_OH ---->   0.0600\*OH +    0.3400\*HO2 +    0.3900\*HPCRB  |   1.2900E+00 |   1.2900E+00 |
| 505   | ISOP_OH + NO ----> NO +    0.3500\*xHO2 +    0.3800\*RO2C +    0.0500\*RO2XC +    0.0300\*xHCHO +    0.3400\*xOLEA1 +    0.0500\*zRHNO3 +    0.4200\*yRUOOH +    0.4300\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| IPO3   | ISOP + O3 ----> ISOP_O3 +    0.1300\*OH +    0.2900\*HO2 +    0.1200\*xHO2 +    0.3300\*RO2C +    0.0100\*RO2XC +    0.1600\*MECO3 +    0.1300\*xMECO3 +    0.0400\*xMACO3 +    0.2300\*HCHO2 +    0.0600\*RCHO2 +    0.4000\*HCHO +    0.2100\*xHCHO +    0.3900\*MACR +    0.1600\*MVK +    0.0500\*OLEP +    0.0100\*zRCNO3 +    0.1500\*yHPCRB +    0.1300\*CO2 +    0.3300\*CO +    0.3400\*SumRO2 +    0.1600\*SumRCO3 + ISOPRXN  |   1.05E-14e<sup> -2000.00/T</sup> |   1.2821E-17 |
| 507   | ISOP_O3 ---->   0.0400\*xOH +    0.1200\*xPACID +    0.0400\*CO2  |   2.8100E+00 |   2.8100E+00 |
| 508   | ISOP_O3 + NO ----> NO +    0.0400\*xHO2 +    0.1100\*xHCHO +    0.1300\*yHPCRB +    0.1400\*CO  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| IPN3   | ISOP + NO3 ----> ISOP_N3 +    0.7000\*xNO2 +    0.0300\*xHO2 +    0.9000\*RO2C +    0.1000\*RO2XC +    0.4800\*xHCHO +    0.2200\*xOLEA1 +    0.4800\*xMVK +    0.0300\*xRCNO3 + yRPNO3 +    0.1000\*zRDNO3 + SumRO2 + ISOPRXN  |   2.95E-12e<sup>  -450.00/T</sup> |   6.5214E-13 |
| 510   | ISOP_N3 ---->   0.1200\*NO2 +    0.0400\*HO2 +    0.0400\*RPNO3 +    0.1200\*HPCRB  |   1.0300E+00 |   1.0300E+00 |
| 511   | ISOP_N3 + NO ----> NO +    0.1500\*xHO2 +    0.1500\*RO2C +    0.0100\*RO2XC +    0.1000\*xHCHO +    0.1000\*xRCNO3 +    0.0500\*xRHNO3 +    0.1700\*yRPNO3 +    0.0100\*zRDNO3 +    0.1600\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| IPOP   | ISOP + O3P ---->   0.2300\*RO2C +    0.0200\*RO2XC +    0.2500\*MEO2 +    0.2300\*xMACO3 +    0.2300\*xHCHO +    0.7500\*OLEP +    0.0200\*zRCNO3 +    0.2100\*yHPCRB +    0.5000\*SumRO2  |   3.5000E-11 |   3.5000E-11 |
| E3OH   | BUT13 + OH ----> BUT13_OH +    0.6300\*xHO2 +    0.6300\*RO2C +    0.0400\*RO2XC +    0.5800\*xHCHO +    0.5800\*xACRO +    0.0400\*zRHNO3 +    0.6700\*yRUOOH +    0.0500\*xFURNS +    0.6700\*SumRO2  |   1.12E-11e<sup>   530.00/T</sup> |   6.6257E-11 |
| 514   | BUT13_OH ---->   0.3100\*HO2 +    0.3300\*HPCRB  |   1.0700E+00 |   1.0700E+00 |
| 515   | BUT13_OH + NO ----> NO +    0.3100\*xHO2 +    0.3300\*RO2C +    0.0200\*RO2XC +    0.0300\*xHCHO +    0.3100\*xOLEA1 +    0.0200\*zRHNO3 +    0.3500\*yRUOOH +    0.3500\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| E3O3   | BUT13 + O3 ----> BUT13_O3 +    0.0800\*OH +    0.5000\*HO2 +    0.2700\*xHO2 +    0.3600\*RO2C +    0.2100\*HCHO2 +    0.1400\*RCHO2 +    0.5000\*HCHO +    0.0900\*xHCHO +    0.5000\*ACRO +    0.1200\*CO2 +    0.5400\*CO +    0.3600\*SumRO2  |   1.34E-14e<sup> -2283.00/T</sup> |   6.3331E-18 |
| 517   | BUT13_O3 ---->   0.0900\*xOH +    0.2700\*xPACID +    0.0900\*CO2  |   2.5500E+00 |   2.5500E+00 |
| 518   | BUT13_O3 + NO ----> NO +    0.0900\*xHO2 +    0.2500\*xHCHO +    0.0200\*xGLY +    0.3000\*yHPCRB +    0.3300\*CO  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| E3N3   | BUT13 + NO3 ---->   0.8900\*xNO2 +    0.0600\*xHO2 +    0.9400\*RO2C +    0.0600\*RO2XC +    0.7400\*xHCHO +    0.1400\*xOLEA1 +    0.7400\*xACRO +    0.0600\*xRCNO3 + yRPNO3 +    0.0600\*zRDNO3 + SumRO2  |   1.1000E-13 |   1.1000E-13 |
| E3OP   | BUT13 + O3P ---->   0.2500\*OLEA2 +    0.2500\*MVK +    0.5000\*OLEP  |   2.26E-11e<sup>   -40.00/T</sup> |   1.9763E-11 |
| APOH   | APINE + OH ----> APINE_OH +    0.0200\*HO2 +    0.5900\*xHO2 +    1.0700\*RO2C +    0.3000\*RO2XC +    0.0800\*xHCHO +    0.5100\*xRCHO +    0.0800\*xOLEA2 +    0.1700\*xACET +    0.0600\*xMVK +    0.0300\*xLVKS +    0.0100\*zR2NO3 +    0.0100\*zRCNO3 +    0.2800\*zRHNO3 +    0.5300\*yROOH +    0.7100\*yRUOOH +    0.0200\*HPCRB +    0.1300\*yHPCRB +    1.3700\*SumRO2 + TRPRXN  |   1.34E-11e<sup>   410.00/T</sup> |   5.3006E-11 |
| 522   | APINE_OH ---->   0.0100\*OH +    0.0800\*HO2 +    0.0700\*HPCRB  |   7.4900E+00 |   7.4900E+00 |
| 523   | APINE_OH + NO ----> NO +    0.0600\*xHO2 +    0.0800\*RO2C +    0.0200\*RO2XC +    0.0100\*xHCHO +    0.0600\*xOLEA2 +    0.0200\*zRHNO3 +    0.0900\*yHPCRB +    0.1000\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| APO3   | APINE + O3 ---->   0.6900\*OH +    0.0100\*xOH +    0.0100\*HO2 +    0.1700\*xHO2 +    0.7900\*RO2C +    0.2600\*RO2XC +    0.0300\*xMECO3 +    0.2000\*xR2CO3 +    0.2900\*RCHO2 +    0.2000\*xHCHO +    0.0200\*RCHO +    0.1300\*xRCHO +    0.0300\*xBACL +    0.0900\*xACET +    0.0300\*KET2 +    0.0700\*xPACID +    0.2600\*zRCNO3 +    0.8200\*yHPCRB +    0.0300\*CO2 +    0.1700\*CO +    1.0500\*SumRO2 + TRPRXN  |   8.22E-16e<sup>  -640.00/T</sup> |   9.6079E-17 |
| APN3   | APINE + NO3 ---->   0.8100\*xNO2 +    0.8100\*RO2C +    0.1900\*RO2XC +    0.8100\*xRCHO + yRPNO3 +    0.1900\*zRDNO3 + SumRO2  |   1.20E-12e<sup>   490.00/T</sup> |   6.2077E-12 |
| APOP   | APINE + O3P ---->   0.5000\*KET2 +    0.5000\*ALK4 + TRPRXN  |   3.2000E-11 |   3.2000E-11 |
| BPOH   | BPINE + OH ----> BPINE_OH +    0.0100\*xOH +    0.0100\*HO2 +    0.3700\*xHO2 +    1.4300\*RO2C +    0.4200\*RO2XC +    0.0300\*xR2CO3 +    0.3500\*xHCHO +    0.0300\*xRCHO +    0.2300\*xOLEA2 +    0.2900\*xACET +    0.1100\*xKET2 +    0.0200\*xPACID +    0.0200\*zR2NO3 +    0.0900\*zRCNO3 +    0.3100\*zRHNO3 +    0.3200\*yROOH + yRUOOH +    0.0100\*HPCRB +    0.4500\*yHPCRB +    1.8500\*SumRO2 + TRPRXN  |   1.62E-11e<sup>   460.00/T</sup> |   7.5782E-11 |
| 528   | BPINE_OH ---->   0.0400\*OH +    0.1200\*HO2 +    0.0300\*PACID +    0.0100\*xPACID +    0.1100\*HPCRB +    0.0100\*CO2  |   3.4500E+00 |   3.4500E+00 |
| 529   | BPINE_OH + NO ----> NO +    0.1100\*xHO2 +    0.1500\*RO2C +    0.0400\*RO2XC +    0.0200\*xHCHO +    0.0100\*xRCHO +    0.0800\*xOLEA2 +    0.0100\*zRCNO3 +    0.0300\*zRHNO3 +    0.0100\*yRUOOH +    0.2100\*yHPCRB +    0.1900\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| BPO3   | BPINE + O3 ---->   0.3900\*OH +    0.1400\*HO2 +    0.2300\*RO2C +    0.0800\*RO2XC +    0.2300\*xR2CO3 +    0.2100\*HCHO2 +    0.2000\*RCHO2 +    0.5000\*HCHO +    0.5000\*KET2 +    0.0800\*zRCNO3 +    0.2600\*yHPCRB +    0.1200\*CO2 +    0.1700\*CO +    0.3100\*SumRO2 + TRPRXN  |   1.39E-15e<sup> -1280.00/T</sup> |   1.8990E-17 |
| BPN3   | BPINE + NO3 ---->   0.0200\*OH +    0.0400\*xOH +    0.1800\*xHO2 +    2.2500\*RO2C +    0.6300\*RO2XC +    0.1400\*xR2CO3 +    0.0400\*xHCHO +    0.0200\*xRCHO +    0.2700\*xACET +    0.1400\*xPACID +    0.0200\*RCNO3 +    0.2200\*xRCNO3 +    0.4300\*zRCNO3 + yRPNO3 +    0.1900\*zRDNO3 +    0.0400\*CO2 +    2.8800\*SumRO2 + TRPRXN  |   2.5000E-12 |   2.5000E-12 |
| BPOP   | BPINE + O3P ---->   0.5000\*RCHO +    0.5000\*ALK5 + TRPRXN  |   2.7000E-11 |   2.7000E-11 |
| ACOH   | ACETL + OH ---->   0.6700\*OH +    0.3300\*HO2 +    0.6700\*GLY +    0.3300\*HCOOH +    0.3300\*CO  | k<sub>o</sub>=  5.50E-30e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>k<sub>i</sub> =   8.30E-13e<sup>     0.0/T</sup>(T/300)<sup>  2.00</sup><br>n=     1.00;F=     0.60 |   7.4748E-13 |
| ACO3   | ACETL + O3 ---->   0.2600\*HO2 +    0.3400\*RCHO2 +    0.3400\*HCHO +    0.1800\*HCOOH +    0.4700\*CO2 +    0.3100\*CO  |   1.0000E-20 |   1.0000E-20 |
| BZOH   | BENZ + OH ---->   0.6900\*HO2 +    0.2800\*xHO2 +    0.2800\*RO2C +    0.0400\*RO2XC +    0.1200\*OLEA2 +    0.2800\*xGLY +    0.2800\*xBUDAL +    0.5700\*PHEN +    0.0400\*zRANO3 +    0.3100\*yRAOOH +    0.3200\*SumRO2 + BENZRO2  |   2.30E-12e<sup>  -190.00/T</sup> |   1.2161E-12 |
| TLOH   | TOLU + OH ---->   0.4100\*HO2 +    0.5000\*xHO2 +    0.5000\*RO2C +    0.0800\*RO2XC +    0.0100\*OLEA1 +    0.2100\*OLEA2 +    0.2200\*xGLY +    0.2200\*xMGLY +    0.2200\*xBUDAL +    0.0200\*xAFG1 +    0.1900\*xAFG2A +    0.0600\*xBALD +    0.1900\*CRES +    0.0100\*zR1NO3 +    0.0700\*zRANO3 +    0.0800\*yROOH +    0.5100\*yRAOOH +    0.5800\*SumRO2 + TOLRO2  |   1.80E-12e<sup>   340.00/T</sup> |   5.6302E-12 |
| OXOH   | OXYL + OH ---->   0.3500\*HO2 +    0.5400\*xHO2 +    0.5400\*RO2C +    0.1100\*RO2XC +    0.1000\*OLEA1 +    0.1000\*OLEA2 +    0.1200\*xGLY +    0.2200\*xMGLY +    0.1500\*xBACL +    0.1500\*xBUDAL +    0.0100\*xAFG1 +    0.2200\*xAFG2A +    0.1100\*xAFG2B +    0.0600\*xBALD +    0.0600\*LVKS +    0.0800\*XYNL +    0.0200\*zR2NO3 +    0.0900\*zRANO3 +    0.0800\*yROOH +    0.5700\*yRAOOH +    0.6500\*SumRO2 + XYLRO2  |   1.3600E-11 |   1.3600E-11 |
| MXOH   | MXYL + OH ---->   0.2100\*HO2 +    0.6600\*xHO2 +    0.6600\*RO2C +    0.1300\*RO2XC +    0.0100\*OLEA1 +    0.1300\*OLEA2 +    0.0400\*xGLY +    0.5900\*xMGLY +    0.0500\*xAFG1 +    0.5800\*xAFG2A +    0.0300\*xBALD +    0.0700\*XYNL +    0.0100\*zR2NO3 +    0.1200\*zRANO3 +    0.0300\*yROOH +    0.7500\*yRAOOH +    0.7900\*SumRO2 + XYLRO2  |   2.3100E-11 |   2.3100E-11 |
| PXOH   | PXYL + OH ---->   0.3800\*HO2 +    0.5200\*xHO2 +    0.5200\*RO2C +    0.1100\*RO2XC +    0.0200\*OLEA1 +    0.2300\*OLEA2 +    0.1600\*xGLY +    0.2900\*xMGLY +    0.2900\*xAFG1 +    0.0700\*xBALD +    0.1600\*xAFG3 +    0.1400\*XYNL +    0.0200\*zR2NO3 +    0.0900\*zRANO3 +    0.0900\*yROOH +    0.5300\*yRAOOH +    0.6300\*SumRO2 + XYLRO2  |   4.14E-12e<sup>   319.00/T</sup> |   1.2069E-11 |
| X1OH   | BZ123 + OH ---->   0.1800\*HO2 +    0.6700\*xHO2 +    0.6700\*RO2C +    0.1500\*RO2XC +    0.0300\*OLEA1 +    0.0300\*OLEA2 +    0.0300\*xGLY +    0.0700\*xMGLY +    0.5400\*xBACL +    0.5400\*xAFG2A +    0.1000\*xAFG2B +    0.0200\*xBALD +    0.1000\*LVKS +    0.0200\*XYNL +    0.0100\*zR2NO3 +    0.1400\*zRANO3 +    0.0300\*yROOH +    0.7800\*yRAOOH +    0.8200\*SumRO2 + XYLRO2  |   3.2700E-11 |   3.2700E-11 |
| X2OH   | BZ124 + OH ---->   0.2300\*HO2 +    0.6300\*xHO2 +    0.6300\*RO2C +    0.1400\*RO2XC +    0.0400\*OLEA1 +    0.1100\*OLEA2 +    0.0300\*xGLY +    0.5100\*xMGLY +    0.0600\*xBACL +    0.0800\*xAFG1 +    0.0400\*xAFG2A +    0.2700\*xAFG2B +    0.0300\*xBALD +    0.0200\*LVKS +    0.2100\*xAFG3 +    0.0500\*XYNL +    0.0100\*zR2NO3 +    0.1300\*zRANO3 +    0.0400\*yROOH +    0.7300\*yRAOOH +    0.7700\*SumRO2 + XYLRO2  |   3.2500E-11 |   3.2500E-11 |
| X3OH   | BZ135 + OH ---->   0.1700\*HO2 +    0.6800\*xHO2 +    0.6800\*RO2C +    0.1500\*RO2XC +    0.1100\*OLEA2 +    0.6700\*xMGLY +    0.6700\*xAFG2A +    0.0200\*xBALD +    0.0500\*XYNL +    0.0100\*zR2NO3 +    0.1500\*zRANO3 +    0.0200\*yROOH +    0.8100\*yRAOOH +    0.8300\*SumRO2 + XYLRO2  |   5.8600E-11 |   5.8600E-11 |
| EBOH   | C2BEN + OH ---->   0.3600\*HO2 +    0.5100\*xHO2 +    0.5400\*RO2C +    0.1200\*RO2XC +    0.0100\*xMEO2 +    0.0200\*xHCHO +    0.0100\*OLEA1 +    0.1800\*OLEA2 +    0.1800\*xGLY +    0.1800\*xMGLY +    0.1800\*xBUDAL +    0.0200\*xAFG1 +    0.1600\*xAFG2A +    0.0300\*xBALD +    0.1600\*XYNL +    0.0500\*zR2NO3 +    0.0700\*zRANO3 +    0.2300\*yROOH +    0.4400\*yRAOOH +    0.1300\*xBENX +    0.6600\*SumRO2  |   7.0000E-12 |   7.0000E-12 |
| MTOH   | MTBE + OH ---->   0.7200\*xHO2 +    1.1200\*RO2C +    0.0900\*RO2XC +    0.1900\*xMEO2 +    0.2000\*xHCHO +    0.0900\*zR1NO3 +    0.8900\*yROOH +    0.1700\*ALK1 +    0.7200\*ALK2 +    0.0100\*ALK3 +    1.2100\*SumRO2  |   1.87E-13e<sup>   843.00/T</sup>(T/300)<sup>  3.34 </sup> |   3.0961E-12 |
| MLOH   | MEOH + OH ----> HO2 + HCHO  |   2.32E-13e<sup>   402.00/T</sup>(T/300)<sup>  2.72 </sup> |   8.7851E-13 |
| FAOH   | HCOOH + OH ----> HO2 + CO2  |   4.5000E-13 |   4.5000E-13 |
| H1OH   | MEOOH + OH ---->   0.0300\*OH +    0.9700\*MEO2 +    0.0300\*HCHO +    0.9700\*SumRO2  |   5.30E-12e<sup>   190.00/T</sup> |   1.0024E-11 |
| H1HV   | MEOOH ----> OH + HO2 + HCHO  | COOH | Not Available<sup>1</sup> | 
| A2OH   | MECHO + OH ----> MECHO_OH +    0.0400\*xHO2 +    0.0500\*RO2C +    0.9500\*MECO3 +    0.0100\*xHCHO +    0.0500\*SumRO2 +    0.9500\*SumRCO3  |   2.40E-12e<sup>   546.00/T</sup>(T/300)<sup>  0.77 </sup> |   1.4910E-11 |
| 550   | MECHO_OH ---->   0.0100\*xOH +    0.0400\*xPACID +    0.0100\*CO2  |   3.7500E+00 |   3.7500E+00 |
| 551   | MECHO_OH + NO ----> NO +    0.0100\*xHO2 +    0.0400\*xHCHO +    0.0400\*yHPCRB +    0.0500\*CO  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| A2N3   | MECHO + NO3 ----> HNO3 + MECO3 + SumRCO3  |   1.40E-12e<sup> -1860.00/T</sup> |   2.7340E-15 |
| A2HV   | MECHO ----> HO2 +    0.9000\*MEO2 +    0.1000\*MECO3 +    0.9000\*CO +    0.9000\*SumRO2 +    0.1000\*SumRCO3  | CCHOR_13 | Not Available<sup>1</sup> | 
| EAOH   | ETOH + OH ---->   0.9500\*HO2 +    0.0500\*xHO2 +    0.0500\*RO2C +    0.0700\*xHCHO +    0.9500\*MECHO +    0.0100\*xGLCHO +    0.0500\*yROOH +    0.0500\*SumRO2  |   4.42E-13e<sup>   606.00/T</sup>(T/300)<sup>  2.29 </sup> |   3.3265E-12 |
| GAOH   | GLCHO + OH ---->   0.2000\*HO2 +    0.8000\*R2CO3 +    0.2000\*GLY +    0.8000\*SumRCO3  |   1.1000E-11 |   1.1000E-11 |
| GAN3   | GLCHO + NO3 ----> HNO3 +    0.1000\*HO2 +    0.9000\*R2CO3 +    0.1000\*GLY +    0.9000\*SumRCO3  |   1.8400E-14 |   1.8400E-14 |
| GAHV   | GLCHO ----> GLCHO_HV +    0.0700\*OH +    1.6600\*HO2 +    0.0500\*xHO2 +    0.0700\*RO2C +    0.8300\*HCHO +    0.0200\*xHCHO +    0.1000\*MEOH +    0.9300\*CO +    0.0700\*SumRO2  | GLALD_14 | Not Available<sup>1</sup> | 
| 558   | GLCHO_HV ---->   0.0200\*xOH +    0.0500\*xPACID +    0.0200\*CO2  |   2.6200E+00 |   2.6200E+00 |
| 559   | GLCHO_HV + NO ----> NO +    0.0200\*xHO2 +    0.0400\*xHCHO +    0.0600\*yHPCRB +    0.0600\*CO  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| A3OH   | ETCHO + OH ---->   0.0400\*xHO2 +    0.0400\*RO2C +    0.9600\*R2CO3 +    0.0400\*xMECHO +    0.0100\*xPACID +    0.0300\*yHPCRB +    0.0400\*CO +    0.0400\*SumRO2 +    0.9600\*SumRCO3  |   6.63E-13e<sup>  1018.00/T</sup>(T/300)<sup>  1.99 </sup> |   1.9908E-11 |
| A3N3   | ETCHO + NO3 ----> HNO3 + R2CO3 + SumRCO3  |   6.3000E-15 |   6.3000E-15 |
| A3HV   | ETCHO ----> HO2 + ETO2 + CO + SumRO2  | C2CHOabs | Not Available<sup>1</sup> | 
| AROH   | ACRO + OH ----> ACRO_OH +    0.3100\*xHO2 +    0.3100\*RO2C +    0.0100\*RO2XC +    0.6800\*MACO3 +    0.0700\*xHCHO +    0.0100\*xGLY +    0.2400\*xGLCHO +    0.0100\*zRHNO3 +    0.2200\*yHPCRB +    0.2400\*CO +    0.3200\*SumRO2 +    0.6800\*SumRCO3  |   7.10E-12e<sup>   333.00/T</sup> |   2.1693E-11 |
| 564   | ACRO_OH ---->   0.0600\*xPACID  |   1.6900E+00 |   1.6900E+00 |
| 565   | ACRO_OH + NO ----> NO +    0.0600\*xGLY +    0.0600\*yHPCRB  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| ARO3   | ACRO + O3 ---->   0.1500\*OH +    0.2700\*HO2 +    0.3800\*HCHO2 +    0.0300\*RCHO2 +    0.1300\*HCHO +    0.9000\*GLY +    0.0200\*HCOOH +    0.2600\*CO2 +    0.3400\*CO  |   2.8000E-19 |   2.8000E-19 |
| ARN3   | ACRO + NO3 ---->   0.9400\*HNO3 +    0.0600\*xHO2 +    0.0600\*RO2C +    0.9400\*MACO3 +    0.0600\*xRCNO3 +    0.0500\*yRPNO3 +    0.0600\*CO +    0.0600\*SumRO2 +    0.9400\*SumRCO3  |   1.1000E-15 |   1.1000E-15 |
| ARHV   | ACRO ----> ACRO_HV +    0.2200\*OH +    0.4900\*HO2 +    0.1700\*xHO2 +    0.2200\*RO2C +    0.0500\*MEO2 +    0.1500\*MACO3 +    0.1500\*HCHO +    0.0600\*xHCHO +    0.0600\*MEOH +    0.2500\*ETHEN +    0.1600\*CO2 +    1.0600\*CO +    0.2700\*SumRO2 +    0.1500\*SumRCO3  | ACROL_16 | Not Available<sup>1</sup> | 
| 569   | ACRO_HV ---->   0.0600\*xOH +    0.1700\*xPACID +    0.0600\*CO2  |   2.4000E+00 |   2.4000E+00 |
| 570   | ACRO_HV + NO ----> NO +    0.0500\*xHO2 +    0.1500\*xHCHO +    0.1900\*yHPCRB +    0.2100\*CO  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| K3OH   | ACET + OH ---->   0.9600\*RO2C +    0.0400\*RO2XC +    0.9600\*xMECO3 +    0.9600\*xHCHO +    0.0400\*zRCNO3 +    0.8500\*yHPCRB + SumRO2  |   1.97E-14e<sup>   678.00/T</sup>(T/300)<sup>  3.88 </sup> |   1.8691E-13 |
| K3HV   | ACET ----> MEO2 + MECO3 + SumRO2 + SumRCO3  | ACET_06 | Not Available<sup>1</sup> | 
| K4OH   | MEK + OH ---->   0.2900\*xHO2 +    0.9400\*RO2C +    0.0700\*RO2XC +    0.5500\*xMECO3 +    0.0800\*xR2CO3 +    0.1100\*xHCHO +    0.5400\*xMECHO +    0.2900\*xRCHO +    0.0700\*zRCNO3 +    0.9100\*yHPCRB +    1.0100\*SumRO2  |   5.42E-14e<sup>   889.00/T</sup>(T/300)<sup>  3.57 </sup> |   1.0456E-12 |
| K4HV   | MEK ---->   0.1500\*MEO2 +    0.8500\*ETO2 +    0.8500\*MECO3 +    0.1500\*R2CO3 + SumRO2 + SumRCO3  |   1.7500E-01*MEK_06 | Not Available<sup>1</sup> | 
| MAOH   | MACR + OH ----> MACR_OH +    0.0500\*xHO2 +    0.7500\*RO2C +    0.0400\*RO2XC +    0.2100\*MACO3 +    0.0500\*xHCHO +    0.6100\*xKET2 +    0.7900\*SumRO2 +    0.2100\*SumRCO3  |   8.00E-12e<sup>   380.00/T</sup> |   2.8616E-11 |
| 576   | MACR_OH ---->   0.6900\*xOH +    0.0800\*xKET2 +    0.0500\*xPACID +    0.0400\*zRCNO3 +    0.6900\*CO2  |   5.2600E-01 |   5.2600E-01 |
| 577   | MACR_OH + NO ----> NO +    0.6900\*xHO2 +    0.0900\*xHCHO +    0.1400\*xMGLY +    0.0400\*zRHNO3 +    0.6800\*yHPCRB +    0.6100\*CO  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| MAO3   | MACR + O3 ---->   0.1900\*OH +    0.0100\*xOH +    0.2500\*HO2 +    0.0300\*xHO2 +    0.0400\*RO2C +    0.0100\*MECO3 +    0.3800\*HCHO2 +    0.0300\*RCHO2 +    0.1000\*HCHO +    0.0200\*xHCHO +    0.0200\*MECHO +    0.9000\*MGLY +    0.0100\*OACID +    0.0200\*xPACID +    0.0100\*yHPCRB +    0.2400\*CO2 +    0.3800\*CO +    0.0400\*SumRO2 +    0.0100\*SumRCO3  |   1.40E-15e<sup> -2100.00/T</sup> |   1.2224E-18 |
| MAN3   | MACR + NO3 ----> MACR_N3 +    0.3000\*HNO3 +    0.6600\*RO2C +    0.0400\*RO2XC +    0.3000\*MACO3 +    0.6600\*xRCNO3 +    0.7000\*SumRO2 +    0.3000\*SumRCO3  |   3.4000E-15 |   3.4000E-15 |
| 580   | MACR_N3 ---->   0.6600\*xOH +    0.0400\*zRCNO3 +    0.6600\*CO2  |   5.2400E-01 |   5.2400E-01 |
| 581   | MACR_N3 + NO ----> NO +    0.6600\*xHO2 +    0.5900\*yRPNO3 +    0.0400\*zRDNO3 +    0.6600\*CO  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| MAHV   | MACR ---->   0.4500\*OH +    0.3000\*HO2 +    0.4300\*RO2C +    0.0200\*RO2XC +    0.1500\*MEO2 +    0.4300\*xMECO3 +    0.1500\*MACO3 +    0.1500\*HCHO +    0.4300\*xHCHO +    0.0200\*zRCNO3 +    0.3800\*yHPCRB +    0.2500\*PROPE + CO +    0.6000\*SumRO2 +    0.1500\*SumRCO3  | MACR_06 | Not Available<sup>1</sup> | 
| MVOH   | MVK + OH ---->   0.2800\*xHO2 +    0.9500\*RO2C +    0.0500\*RO2XC +    0.6600\*xMECO3 +    0.2800\*xHCHO +    0.6600\*xGLCHO +    0.2800\*xMGLY +    0.0500\*zRCNO3 +    0.9000\*yHPCRB + SumRO2  |   2.60E-12e<sup>   610.00/T</sup> |   2.0115E-11 |
| MVO3   | MVK + O3 ---->   0.1800\*OH +    0.2600\*HO2 +    0.0200\*xHO2 +    0.0200\*RO2C +    0.4000\*HCHO2 +    0.0100\*RCHO2 +    0.0500\*HCHO +    0.0100\*xHCHO +    0.0100\*MECHO +    0.9500\*MGLY +    0.0100\*xPACID +    0.0100\*yHPCRB +    0.2300\*CO2 +    0.3600\*CO +    0.0200\*SumRO2  |   8.50E-16e<sup> -1520.00/T</sup> |   5.1921E-18 |
| MVHV   | MVK ---->   0.4000\*MEO2 +    0.4000\*MACO3 +    0.6000\*PROPE +    0.6000\*CO +    0.4000\*SumRO2 +    0.4000\*SumRCO3  | MVK_16 | Not Available<sup>1</sup> | 
| F1OH   | BUDAL + OH ----> BUDAL_OH +    0.5400\*OH +    0.4400\*xHO2 +    0.4400\*RO2C +    0.0200\*RO2XC +    0.4100\*xGLY +    0.5400\*MALAH +    0.0500\*xPACID +    0.0200\*CO +    0.4600\*SumRO2  |   5.2900E-11 |   5.2900E-11 |
| 587   | BUDAL_OH ---->   0.3900\*xPACID +    0.0200\*zRCNO3  |   2.0200E+01 |   2.0200E+01 |
| 588   | BUDAL_OH + NO ----> NO +    0.3700\*xGLY +    0.3500\*yHPCRB  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| F1HV   | BUDAL ----> OH + HO2 + MALAH  |   2.5000E-01*AFGS | Not Available<sup>1</sup> | 
| PHOH   | PHEN + OH ---->   0.8500\*HO2 +    0.0700\*xHO2 +    0.0700\*RO2C +    0.0100\*RO2XC +    0.0700\*BZO +    0.0100\*OLEA1 +    0.0400\*OLEA2 +    0.0300\*xGLY +    0.0400\*xMGLY +    0.0400\*xBUDAL +    0.0100\*xAFG1 +    0.0200\*xAFG2A +    0.0300\*OLEP +    0.7700\*CATL +    0.0100\*zRANO3 +    0.0800\*yRAOOH +    0.0800\*SumRO2  |   4.70E-13e<sup>  1220.00/T</sup> |   2.8131E-11 |
| PHN3   | PHEN + NO3 ----> HNO3 + BZO  |   4.5000E-12 |   4.5000E-12 |
| L1OH   | ALK1 + OH ---->   0.9600\*xHO2 +    0.9700\*RO2C +    0.0300\*RO2XC +    0.0100\*xMECO3 +    0.0100\*xHCHO +    0.0900\*xMGLY +    0.4300\*xOACID +    0.0300\*zRCNO3 +    0.6200\*yHPCRB +   39.3300\*NROG +    0.4300\*CO + SumRO2  |   3.3500E-13 |   3.3500E-13 |
| L2OH   | ALK2 + OH ---->   0.1000\*xHO2 +    0.9500\*RO2C +    0.0500\*RO2XC +    0.0100\*xMEO2 +    0.8400\*xMECO3 +    0.0100\*xHCHO +    0.0800\*xRCHO +    0.0100\*xMGLY +    0.8400\*xOACID +    0.0500\*zRCNO3 +    0.6400\*yHPCRB +    1.3800\*NROG + SumRO2  |   1.6700E-12 |   1.6700E-12 |
| L3OH   | ALK3 + OH ---->   0.3400\*xHO2 +    1.2000\*RO2C +    0.1200\*RO2XC +    0.1400\*xETO2 +    0.0700\*xR2CO3 +    0.3300\*xTBUO +    0.1000\*xHCHO +    0.1000\*xMECHO +    0.0900\*xRCHO +    0.0700\*xACET +    0.0100\*xKET2 +    0.1200\*xOACID +    0.0700\*zR1NO3 +    0.0300\*zRCNO3 +    0.0200\*zRHNO3 +    0.8100\*yROOH +    0.3300\*yHPCRB +    0.1200\*ALK1 +   16.9100\*NROG +    0.0500\*CO +    1.3200\*SumRO2  |   2.8500E-12 |   2.8500E-12 |
| L4OH   | ALK4 + OH ---->   0.0100\*xOH +    0.2700\*HO2 +    0.3600\*xHO2 +    0.9600\*RO2C +    0.1200\*RO2XC +    0.2300\*xETO2 +    0.0600\*xHCHO +    0.1300\*xMECHO +    0.0500\*xETCHO +    0.0700\*xRCHO +    0.2600\*ACET +    0.3000\*xACET +    0.0300\*xMEK +    0.1500\*xKET2 +    0.0900\*zR1NO3 +    0.0300\*zRHNO3 +    1.0500\*yROOH +    0.0100\*HPCRB +    0.0100\*yHPCRB +    0.0100\*CO2 +    1.0800\*SumRO2  |   4.5400E-12 |   4.5400E-12 |
| L5OH   | ALK5 + OH ----> ALK5_OH +    0.3000\*HO2 +    0.3300\*xHO2 + RO2C +    0.2200\*RO2XC +    0.0200\*xETO2 +    0.0100\*xR2CO3 +    0.0100\*HCHO +    0.0300\*xHCHO +    0.0400\*xMECHO +    0.0200\*xETCHO +    0.1100\*RCHO +    0.0400\*xRCHO +    0.0600\*GLCHO +    0.0100\*xGLCHO +    0.0500\*xACET +    0.0500\*xMEK +    0.1300\*KET2 +    0.2300\*xKET2 +    0.0100\*xPACID +    0.0800\*zR1NO3 +    0.0500\*zR2NO3 +    0.0200\*zRCNO3 +    0.0600\*zRHNO3 +    1.0500\*yROOH +    0.1200\*yHPCRB +    0.0100\*ALK4 +    0.0100\*ALK5 +    1.2200\*SumRO2  |   1.1400E-11 |   1.1400E-11 |
| 597   | ALK5_OH ---->   0.1100\*HO2 +    0.0300\*PACID +    0.0800\*HPCRB  |   2.8700E-01 |   2.8700E-01 |
| 598   | ALK5_OH + NO ----> NO +    0.0600\*xHO2 +    0.1100\*RO2C +    0.0200\*RO2XC +    0.0200\*xMECO3 +    0.0100\*xR2CO3 +    0.0200\*xETCHO +    0.0600\*xRCHO +    0.0100\*xKET2 +    0.0100\*zRCNO3 +    0.0100\*zRHNO3 +    0.0500\*yROOH +    0.0800\*yHPCRB +    0.0100\*ALK5 +    0.1200\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| L6OH   | ALK6 + OH ----> ALK6_OH +    0.1600\*HO2 +    0.4000\*xHO2 +    0.9700\*RO2C +    0.3300\*RO2XC +    0.0100\*xR2CO3 +    0.0100\*xTBUO +    0.0800\*xHCHO +    0.0100\*xMECHO +    0.0100\*xETCHO +    0.0700\*RCHO +    0.0800\*xRCHO +    0.1500\*xACET +    0.0900\*KET2 +    0.2600\*xKET2 +    0.0100\*zR1NO3 +    0.1500\*zR2NO3 +    0.0500\*zRCNO3 +    0.1200\*zRHNO3 +    1.0500\*yROOH +    0.2400\*yHPCRB +    0.0600\*ALK3 +    0.0100\*ALK4 +    0.0100\*ALK5 +    1.3100\*SumRO2  |   1.6300E-11 |   1.6300E-11 |
| 600   | ALK6_OH ---->   0.0300\*HO2 +    0.0800\*HPCRB  |   2.7000E-01 |   2.7000E-01 |
| 601   | ALK6_OH + NO ----> NO +    0.0600\*xHO2 +    0.1200\*RO2C +    0.0200\*RO2XC +    0.0100\*xR2CO3 +    0.0200\*xHCHO +    0.0100\*xMECHO +    0.0200\*xRCHO +    0.0400\*xACET +    0.0100\*xKET2 +    0.0100\*zRCNO3 +    0.0100\*zRHNO3 +    0.0600\*yROOH +    0.0400\*yHPCRB +    0.0400\*ALK2 +    0.0100\*ALK5 +    0.1400\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| O1OH   | OLE1 + OH ---->   0.7800\*xHO2 +    1.1200\*RO2C +    0.1000\*RO2XC +    0.0100\*xMEO2 +    0.1100\*xTBUO +    0.6900\*xHCHO +    0.0100\*xMECHO +    0.3500\*xETCHO +    0.3500\*xRCHO +    0.1500\*xGLCHO +    0.0200\*xACRO +    0.0300\*xACET +    0.0200\*xKET2 +    0.0100\*xMVK +    0.0100\*zR1NO3 +    0.0100\*zR2NO3 +    0.0800\*zRHNO3 +    1.1500\*yROOH +    0.0700\*yRUOOH +    1.2200\*SumRO2  |   3.1800E-11 |   3.1800E-11 |
| O1O3   | OLE1 + O3 ---->   0.2600\*OH +    0.0100\*xOH +    0.1700\*HO2 +    0.1600\*xHO2 +    0.1800\*RO2C +    0.0100\*RO2XC +    0.0100\*ETO2 +    0.0100\*xTBUO +    0.2100\*HCHO2 +    0.1700\*RCHO2 +    0.5000\*HCHO +    0.0800\*xMECHO +    0.1900\*ETCHO +    0.0400\*xETCHO +    0.3100\*RCHO +    0.0200\*xRCHO +    0.0400\*xACET +    0.0200\*ETOH +    0.0100\*zRCNO3 +    0.0200\*yROOH +    0.1400\*yHPCRB +    0.0300\*ETHAN +    0.0200\*PROP +    0.0100\*NC4 +    0.0100\*ALK2 +    0.0200\*ALK3 +    0.0100\*ALK4 +    0.2400\*CO2 +    0.3700\*CO +    0.2000\*SumRO2  |   8.7000E-18 |   8.7000E-18 |
| O1N3   | OLE1 + NO3 ---->   0.0900\*xNO2 +    0.5500\*xHO2 +    1.4100\*RO2C +    0.1300\*RO2XC +    0.0900\*xETO2 +    0.1500\*xTBUO +    0.0900\*xHCHO +    0.0900\*xETCHO +    0.0100\*xRCHO +    0.1400\*xACET +    0.0200\*zR1NO3 +    0.8100\*xRCNO3 +    1.2200\*yRPNO3 +    0.1100\*zRDNO3 +    0.3100\*yROOH +    1.5300\*SumRO2  |   1.4400E-14 |   1.4400E-14 |
| O1OP   | OLE1 + O3P ---->   0.2500\*RCHO +    0.1000\*MEK +    0.1500\*KET2 +    0.0900\*ALK2 +    0.3600\*ALK3 +    0.0500\*ALK4  |   4.4300E-12 |   4.4300E-12 |
| O2OH   | OLE2 + OH ---->   0.9200\*xHO2 +    0.9400\*RO2C +    0.0700\*RO2XC +    1.2500\*xMECHO +    0.4000\*xETCHO +    0.1200\*xRCHO +    0.0100\*xACRO +    0.0700\*zRHNO3 +    0.9900\*yROOH +    0.0100\*yRUOOH +    1.0100\*SumRO2  |   6.2900E-11 |   6.2900E-11 |
| O2O3   | OLE2 + O3 ----> OLE2_O3 +    0.4600\*OH +    0.0600\*HO2 +    0.3200\*xHO2 +    0.4000\*RO2C +    0.0100\*RO2XC +    0.0400\*MEO2 +    0.0100\*ETO2 +    0.1600\*MECHO2 +    0.1000\*RCHO2 +    0.0700\*xHCHO +    0.6700\*MECHO +    0.0900\*xMECHO +    0.2200\*ETCHO +    0.0100\*xETCHO +    0.0700\*RCHO +    0.0100\*xACET +    0.0600\*MEOH +    0.0200\*ETOH +    0.0100\*zRCNO3 +    0.0900\*yHPCRB +    0.0400\*ETHAN +    0.2300\*CO2 +    0.1800\*CO +    0.4600\*SumRO2  |   1.9000E-16 |   1.9000E-16 |
| 608   | OLE2_O3 ---->   0.0100\*OH +    0.0800\*xOH +    0.0100\*RCHO +    0.2200\*xPACID +    0.0800\*CO2  |   2.4500E+00 |   2.4500E+00 |
| 609   | OLE2_O3 + NO ----> NO +    0.0800\*xHO2 +    0.0100\*RO2C +    0.0100\*xR2CO3 +    0.2000\*xHCHO +    0.0200\*xGLY +    0.2600\*yHPCRB +    0.2900\*CO +    0.0100\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| O2N3   | OLE2 + NO3 ---->   0.8000\*xNO2 +    0.1100\*xHO2 +    1.0100\*RO2C +    0.0800\*RO2XC +    1.1100\*xMECHO +    0.3300\*xETCHO +    0.0900\*xRCHO +    0.0100\*xACET +    0.1100\*xRCNO3 +    1.0800\*yRPNO3 +    0.0800\*zRDNO3 +    0.0100\*yROOH +    1.0900\*SumRO2  |   4.3400E-13 |   4.3400E-13 |
| O2OP   | OLE2 + O3P ---->   0.2100\*MEK +    0.2900\*KET2 +    0.2100\*ALK1 +    0.2200\*ALK2 +    0.0700\*ALK3  |   1.9500E-11 |   1.9500E-11 |
| O3OH   | OLE3 + OH ---->   0.9400\*xHO2 +    0.9400\*RO2C +    0.0500\*RO2XC +    0.9400\*xHCHO +    0.8200\*xACET +    0.1200\*xMEK +    0.0500\*zRHNO3 + yROOH + SumRO2  |   5.2600E-11 |   5.2600E-11 |
| O3O3   | OLE3 + O3 ---->   0.5800\*OH +    0.1400\*HO2 +    0.4800\*RO2C +    0.0200\*RO2XC +    0.4500\*xMECO3 +    0.0300\*xR2CO3 +    0.2100\*HCHO2 +    0.5000\*HCHO +    0.4500\*xHCHO +    0.0300\*xMECHO +    0.4300\*ACET +    0.0700\*MEK +    0.0200\*zRCNO3 +    0.4200\*yHPCRB +    0.1200\*CO2 +    0.1700\*CO +    0.5000\*SumRO2  |   1.1800E-17 |   1.1800E-17 |
| O3N3   | OLE3 + NO3 ---->   0.8600\*xNO2 +    0.9500\*RO2C +    0.0500\*RO2XC +    0.0100\*xMEO2 +    0.0700\*xETO2 +    0.8600\*xHCHO +    0.8000\*xACET +    0.0600\*xMEK +    0.0800\*xRCNO3 + yRPNO3 +    0.0500\*zRDNO3 + SumRO2  |   3.6200E-13 |   3.6200E-13 |
| O3OP   | OLE3 + O3P ---->   0.5000\*RCHO +    0.5000\*ALK2  |   1.7000E-11 |   1.7000E-11 |
| O4OH   | OLE4 + OH ---->   0.9200\*xHO2 +    0.9200\*RO2C +    0.0800\*RO2XC +    0.8300\*xMECHO +    0.0900\*xETCHO +    0.9200\*xACET +    0.0800\*zRHNO3 + yROOH + SumRO2  |   8.7100E-11 |   8.7100E-11 |
| O4O3   | OLE4 + O3 ----> OLE4_O3 +    0.7200\*OH +    0.0300\*HO2 +    0.1600\*xHO2 +    0.7000\*RO2C +    0.0200\*RO2XC +    0.0300\*MEO2 +    0.4800\*xMECO3 +    0.1100\*MECHO2 +    0.0100\*RCHO2 +    0.5300\*xHCHO +    0.4500\*MECHO +    0.0200\*xMECHO +    0.0500\*ETCHO +    0.5000\*ACET +    0.0500\*MEOH +    0.0200\*zRCNO3 +    0.4400\*yHPCRB +    0.0100\*ETHAN +    0.1200\*CO2 +    0.0700\*CO +    0.7500\*SumRO2  |   4.0500E-16 |   4.0500E-16 |
| 618   | OLE4_O3 ---->   0.0500\*xOH +    0.1400\*xPACID +    0.0500\*CO2  |   2.4500E+00 |   2.4500E+00 |
| 619   | OLE4_O3 + NO ----> NO +    0.0500\*xHO2 +    0.1400\*xHCHO +    0.1700\*yHPCRB +    0.1800\*CO  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| O4N3   | OLE4 + NO3 ---->   0.9200\*xNO2 +    0.9200\*RO2C +    0.0800\*RO2XC +    0.8300\*xMECHO +    0.0900\*xETCHO +    0.9200\*xACET + yRPNO3 +    0.0800\*zRDNO3 + SumRO2  |   9.3100E-12 |   9.3100E-12 |
| O4OP   | OLE4 + O3P ---->   0.5000\*KET2 +    0.5000\*ALK2  |   5.1100E-11 |   5.1100E-11 |
| TPOH   | TERP + OH ----> TERP_OH +    0.0200\*HO2 +    0.5300\*xHO2 +    1.0700\*RO2C +    0.2800\*RO2XC +    0.0100\*xMACO3 +    0.1500\*xHCHO +    0.2900\*xRCHO +    0.1300\*xOLEA2 +    0.0900\*xACET +    0.0500\*xKET2 +    0.0100\*xMVK +    0.0400\*xLVKS +    0.0500\*xOLEP +    0.0200\*zR2NO3 +    0.0600\*zRCNO3 +    0.2000\*zRHNO3 +    0.5100\*yROOH +    0.5500\*yRUOOH +    0.0100\*HPCRB +    0.2400\*yHPCRB +    1.3500\*SumRO2 + TRPRXN  |   1.1000E-10 |   1.1000E-10 |
| 623   | TERP_OH ---->   0.0600\*OH +    0.0100\*xOH +    0.0900\*HO2 +    0.0200\*AFG2A +    0.0100\*xKET2 +    0.0300\*PACID +    0.0100\*xPACID +    0.0600\*HPCRB  |   1.4800E+00 |   1.4800E+00 |
| 624   | TERP_OH + NO ----> NO +    0.0700\*xHO2 +    0.1600\*RO2C +    0.0500\*RO2XC +    0.0300\*xR2CO3 +    0.0200\*xHCHO +    0.0300\*xRCHO +    0.0200\*xOLEA1 +    0.0200\*xOLEA2 +    0.0100\*xAFG2A +    0.0100\*xOLEP +    0.0400\*zRCNO3 +    0.0100\*zRHNO3 +    0.0100\*yRUOOH +    0.2200\*yHPCRB +    0.2100\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| TPO3   | TERP + O3 ----> TERP_O3 +    0.5800\*OH +    0.0200\*xOH +    0.0700\*HO2 +    0.0800\*xHO2 +    0.3700\*RO2C +    0.1200\*RO2XC +    0.0800\*xMECO3 +    0.1100\*xR2CO3 +    0.0800\*HCHO2 +    0.2500\*RCHO2 +    0.1800\*HCHO +    0.0600\*xHCHO +    0.1300\*xRCHO +    0.0300\*xGLY +    0.0100\*xMACR +    0.0100\*ACET +    0.0100\*xACET +    0.1700\*KET2 +    0.0100\*LVKS +    0.0100\*xAFG3 +    0.0100\*xPACID +    0.0200\*OLEP +    0.1200\*zRCNO3 +    0.0100\*yROOH +    0.0100\*xHPCRB +    0.4100\*yHPCRB +    0.0600\*CO2 +    0.1200\*CO +    0.4900\*SumRO2 + TRPRXN  |   1.1700E-16 |   1.1700E-16 |
| 626   | TERP_O3 ---->   0.0600\*OH +    0.0700\*HO2 +    0.0100\*PACID +    0.0100\*OTHN +    0.0500\*HPCRB  |   1.0700E+00 |   1.0700E+00 |
| 627   | TERP_O3 + NO ----> NO +    0.0200\*xHO2 +    0.1200\*RO2C +    0.0400\*RO2XC +    0.0400\*xMECO3 +    0.0400\*xMACO3 +    0.0400\*xHCHO +    0.0400\*xOLEA2 +    0.0400\*zRCNO3 +    0.1300\*yHPCRB +    0.1600\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| TPN3   | TERP + NO3 ----> TERP_N3 +    0.5100\*xNO2 +    0.0100\*OH +    0.0100\*xOH +    0.0500\*xHO2 +    1.1200\*RO2C +    0.2800\*RO2XC +    0.0100\*xHCHO +    0.2900\*xRCHO +    0.2100\*xOLEA2 +    0.1500\*xACET +    0.0100\*xMVK +    0.0100\*xOLEP +    0.0100\*RCNO3 +    0.0700\*xRCNO3 +    0.0800\*zRCNO3 + yRPNO3 +    0.1900\*zRDNO3 +    0.0400\*yROOH +    0.0100\*yRUOOH +    1.4000\*SumRO2 + TRPRXN  |   1.1100E-11 |   1.1100E-11 |
| 629   | TERP_N3 ---->   0.1000\*OH +    0.0400\*HO2 +    0.1000\*RCNO3 +    0.0100\*HPCRB  |   1.2700E+00 |   1.2700E+00 |
| 630   | TERP_N3 + NO ----> NO +    0.0600\*xHO2 +    0.1700\*RO2C +    0.0500\*RO2XC +    0.0300\*xR2CO3 +    0.0100\*xHCHO +    0.0100\*xACET +    0.0600\*xRCNO3 +    0.0500\*zRCNO3 +    0.0100\*yRPNO3 +    0.0100\*yROOH +    0.0100\*yHPCRB +    0.0100\*CO +    0.2200\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| TPOP   | TERP + O3P ---->   0.1600\*RCHO +    0.0400\*OLEA2 +    0.1800\*KET2 +    0.0100\*LVKS +    0.2700\*OLEP +    0.1800\*ALK3 +    0.0800\*ALK4 +    0.0800\*ALK5 + TRPRXN  |   4.2400E-11 |   4.2400E-11 |
| SQOH   | SESQ + OH ----> SESQ_OH +    0.0500\*OH +    0.0500\*HO2 +    0.4700\*xHO2 +    0.7700\*RO2C +    0.2300\*RO2XC +    0.0200\*xHCHO +    0.4400\*xOLEA2 +    0.0500\*OLEP +    0.0200\*xOLEP +    0.0300\*zRCNO3 +    0.1800\*zRHNO3 +    0.8400\*yRUOOH +    0.0200\*HPCRB +    0.0900\*yHPCRB + SumRO2 + SESQRXN  |   2.0000E-10 |   2.0000E-10 |
| 633   | SESQ_OH ---->   0.0700\*OH +    0.1300\*HO2 +    0.0100\*OLEP +    0.0100\*zRPNO3 +    0.0100\*OTHN +    0.1200\*HPCRB  |   5.5800E+00 |   5.5800E+00 |
| 634   | SESQ_OH + NO ----> NO +    0.0800\*xHO2 +    0.3200\*RO2C +    0.1200\*RO2XC +    0.1000\*xHCHO +    0.0800\*xOLEA2 +    0.1000\*xACET +    0.0100\*zRCNO3 +    0.1200\*zRHNO3 +    0.0100\*yROOH +    0.2400\*yRUOOH +    0.2100\*yHPCRB +    0.4400\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| SQO3   | SESQ + O3 ----> SESQ_O3 +    0.6600\*OH +    0.0100\*HO2 +    0.1900\*RO2C +    0.0700\*RO2XC +    0.1500\*xMACO3 +    0.3300\*RCHO2 +    0.0100\*HCHO +    0.1500\*xHCHO +    0.0100\*OLEP +    0.0700\*zRCNO3 +    0.0100\*HPCRB +    0.2200\*yHPCRB +    0.2600\*SumRO2 + SESQRXN  |   3.1400E-16 |   3.1400E-16 |
| 636   | SESQ_O3 ---->   0.0700\*OH +    0.3600\*HO2 +    0.0600\*OTHN +    0.0100\*HPCRB  |   4.3200E+00 |   4.3200E+00 |
| 637   | SESQ_O3 + NO ----> NO +    0.0100\*xOH +    0.4300\*RO2C +    0.1700\*RO2XC +    0.1700\*xMECO3 +    0.0200\*xHCHO +    0.0200\*xRCHO +    0.2200\*xOLEA2 +    0.1700\*zRCNO3 +    0.0100\*xHPCRB +    0.5100\*yHPCRB +    0.0400\*CO +    0.6000\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| SQN3   | SESQ + NO3 ----> SESQ_N3 +    0.7400\*xNO2 +    0.0200\*OH +    0.8200\*RO2C +    0.2200\*RO2XC +    0.7400\*xOLEA2 +    0.0200\*RCNO3 +    0.0100\*zRCNO3 + yRPNO3 +    0.2100\*zRDNO3 +    1.0400\*SumRO2 + SESQRXN  |   1.9000E-11 |   1.9000E-11 |
| 639   | SESQ_N3 ---->   0.0100\*OH +    0.0100\*HO2 +    0.0100\*RCNO3  |   2.5300E+00 |   2.5300E+00 |
| 640   | SESQ_N3 + NO ----> NO +    0.0400\*RO2C +    0.0100\*RO2XC +    0.0100\*zRCNO3 +    0.0500\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| SQOP   | SESQ + O3P ---->   0.1300\*OLEA2 +    0.8700\*OLEP + SESQRXN  |   6.8500E-11 |   6.8500E-11 |
| BXOH   | BENX + OH ---->   0.6900\*HO2 +    0.2800\*xHO2 +    0.2800\*RO2C +    0.0400\*RO2XC +    0.1200\*OLEA2 +    0.2800\*xGLY +    0.2800\*xBUDAL +    0.5700\*PHEN +    0.0400\*zRANO3 +    0.3100\*yRAOOH +    0.3200\*SumRO2  |   1.2100E-12 |   1.2100E-12 |
| B1OH   | ARO1 + OH ---->   0.2700\*HO2 +    0.4800\*xHO2 +    0.7500\*RO2C +    0.2100\*RO2XC +    0.0500\*xETO2 +    0.0100\*xHCHO +    0.0600\*xMECHO +    0.1100\*xETCHO +    0.0100\*xRCHO +    0.0100\*OLEA1 +    0.1300\*OLEA2 +    0.1300\*xGLY +    0.1300\*xMGLY +    0.1300\*xBUDAL +    0.0100\*xAFG1 +    0.1200\*xAFG2A +    0.2000\*xBALD +    0.1200\*XYNL +    0.0300\*zR1NO3 +    0.1100\*zR2NO3 +    0.0100\*zRHNO3 +    0.0600\*zRANO3 +    0.6400\*yROOH +    0.3200\*yRAOOH +    0.0400\*ARO1 +    3.1100\*NROG +    0.9600\*SumRO2 + TOLRO2  |   7.6900E-12 |   7.6900E-12 |
| B2OH   | ARO2 + OH ----> ARO2_OH +    0.2900\*HO2 +    0.5600\*xHO2 +    0.6000\*RO2C +    0.1400\*RO2XC +    0.0300\*OLEA1 +    0.1300\*OLEA2 +    0.0700\*xGLY +    0.3900\*xMGLY +    0.0300\*xBACL +    0.0300\*xBUDAL +    0.0700\*xAFG1 +    0.3000\*xAFG2A +    0.0400\*xAFG2B +    0.0400\*BALD +    0.0300\*xBALD +    0.0100\*xKET2 +    0.0100\*LVKS +    0.0400\*xAFG3 +    0.0800\*XYNL +    0.0200\*zR2NO3 +    0.0100\*zRCNO3 +    0.1100\*zRANO3 +    0.1000\*yROOH +    0.5900\*yRAOOH +    0.0400\*yHPCRB +    0.0300\*xBENX +    1.3800\*NROG +    0.7400\*SumRO2 + XYLRO2  |   2.1300E-11 |   2.1300E-11 |
| 645   | ARO2_OH ---->   0.0100\*HPCRB  |   1.7600E-01 |   1.7600E-01 |
| 646   | ARO2_OH + NO ----> NO +    0.0100\*RO2C +    0.0100\*yHPCRB +    0.3200\*NROG +    0.0100\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| FUOH   | FURNS + OH ---->   0.7500\*HO2 +    0.2400\*xHO2 +    0.2400\*RO2C +    0.0100\*RO2XC +    0.0700\*xRCHO +    0.0300\*xOLEA1 +    0.7500\*BUDAL +    0.1400\*xOLEP +    0.0100\*zRHNO3 +    0.1500\*yRUOOH +    0.0100\*yHPCRB +    0.0800\*CO +    0.2500\*SumRO2  |   3.8400E-11 |   3.8400E-11 |
| FUO3   | FURNS + O3 ---->   0.3900\*HO2 +    0.3800\*xHO2 +    0.3800\*RO2C +    0.0100\*RO2XC +    0.4000\*RCHO2 +    0.1400\*OLEA1 +    0.0500\*xGLY +    0.0100\*zRCNO3 +    0.0700\*HPCRB +    0.2500\*yHPCRB +    0.3300\*ALK1 +    0.1900\*CO2 +    0.7900\*CO +    0.3900\*SumRO2  |   2.4000E-18 |   2.4000E-18 |
| FUN3   | FURNS + NO3 ---->   0.0800\*xNO2 +    0.8700\*xHO2 +    0.9500\*RO2C +    0.0500\*RO2XC +    0.0700\*xOLEA1 +    0.0100\*xOLEP +    0.8700\*xRCNO3 +    0.6300\*yRPNO3 +    0.0500\*zRDNO3 +    0.2800\*CO + SumRO2  |   1.2000E-12 |   1.2000E-12 |
| STOH   | STYRS + OH ---->   0.0600\*HO2 +    0.7900\*xHO2 +    0.7900\*RO2C +    0.1500\*RO2XC +    0.7400\*xHCHO +    0.0300\*OLEA2 +    0.0200\*xGLY +    0.0200\*xMGLY +    0.0300\*xBUDAL +    0.0200\*xAFG2A +    0.7400\*xBALD +    0.0200\*XYNL +    0.1400\*zRHNO3 +    0.0100\*zRANO3 +    0.8800\*yROOH +    0.0600\*yRAOOH +    0.9400\*SumRO2 + XYLRO2  |   5.8000E-11 |   5.8000E-11 |
| STO3   | STYRS + O3 ---->   0.0800\*OH +    0.1700\*HO2 +    0.0300\*RO2C +    0.0300\*xBZO +    0.2100\*HCHO2 +    0.3400\*RCHO2 +    0.5000\*HCHO +    0.5000\*BALD +    0.0500\*PHEN +    0.0300\*yROOH +    0.0900\*BENZ +    0.2300\*CO2 +    0.2200\*CO +    0.0300\*SumRO2  |   1.6000E-17 |   1.6000E-17 |
| AMOH   | AMINS + OH ---->   0.0200\*HO2 +    0.9600\*xHO2 +    0.9700\*RO2C +    0.0200\*RO2XC +    0.0100\*xMEO2 +    0.0800\*xHCHO +    0.0200\*RCHO +    0.0200\*zR2NO3 +    0.9900\*yROOH +    0.9700\*xAMINS +    0.9900\*SumRO2  |   4.3500E-11 |   4.3500E-11 |
| AMO3   | AMINS + O3 ---->   0.6100\*AMINS +   29.5400\*NROG  |   3.0900E-18 |   3.0900E-18 |
| TAOH   | TAMNS + OH ---->   0.0600\*RO2C +    0.0300\*xMEO2 +    0.0300\*xHCHO +    0.0600\*yROOH +    0.9700\*PNAMIN +    0.0300\*xAMINS +    0.0600\*SumRO2  |   1.0100E-11 |   1.0100E-11 |
| A4OH   | RCHO + OH ---->   0.0100\*OH +    0.0100\*xOH +    0.1000\*HO2 +    0.0700\*xHO2 +    0.0900\*RO2C +    0.0100\*RO2XC +    0.8000\*R2CO3 +    0.0400\*xHCHO +    0.0100\*xMECHO +    0.0600\*RCHO +    0.0200\*xRCHO +    0.0500\*MGLY +    0.0100\*xACET +    0.0300\*xPACID +    0.0100\*zRCNO3 +    0.0400\*yHPCRB +    0.0100\*ALK4 +    0.1400\*NROG +    0.0100\*CO2 +    0.0200\*CO +    0.1000\*SumRO2 +    0.8000\*SumRCO3  |   3.2900E-11 |   3.2900E-11 |
| A4N3   | RCHO + NO3 ----> HNO3 +    0.0700\*HO2 +    0.0100\*RO2C +    0.9200\*R2CO3 +    0.0400\*RCHO +    0.0300\*MGLY +    0.0100\*SumRO2 +    0.9200\*SumRCO3  |   2.2300E-14 |   2.2300E-14 |
| A4HV   | RCHO ---->   1.4000\*HO2 +    0.5100\*xHO2 +    0.6500\*RO2C +    0.0400\*RO2XC +    0.0100\*xETO2 +    0.0500\*xMECO3 +    0.1500\*xHCHO +    0.1700\*MECHO +    0.0500\*xMECHO +    0.0500\*xETCHO +    0.0100\*RCHO +    0.2300\*xRCHO +    0.2200\*GLCHO +    0.0100\*xGLCHO +    0.0100\*xMGLY +    0.0700\*xACET +    0.0100\*xMEK +    0.0600\*xKET2 +    0.0100\*zR1NO3 +    0.0300\*zRHNO3 +    0.5900\*yROOH +    0.0800\*yHPCRB +    0.0300\*ALK4 +    0.0800\*NROG + CO +    0.6900\*SumRO2  | C2CHOabs | Not Available<sup>1</sup> | 
| A5OH   | OLEA1 + OH ----> OLEA1_OH +    0.2200\*HO2 +    0.2600\*xHO2 +    0.5900\*RO2C +    0.0500\*RO2XC +    0.0300\*MACO3 +    0.0100\*xHCHO +    0.0600\*RCHO +    0.0500\*xGLY +    0.2000\*xGLCHO +    0.0500\*AFG1 +    0.0300\*AFG2A +    0.0800\*AFG2B +    0.1700\*xKET2 +    0.0200\*xPACID +    0.0100\*HPCRB +    0.0500\*yHPCRB +    0.6500\*SumRO2 +    0.0300\*SumRCO3  |   5.0600E-11 |   5.0600E-11 |
| 659   | OLEA1_OH ---->   0.3300\*xOH +    0.1000\*HO2 +    0.2000\*xKET2 +    0.1900\*xPACID +    0.0500\*zRCNO3 +    0.1000\*HPCRB +    0.3300\*CO2  |   7.5200E-01 |   7.5200E-01 |
| 660   | OLEA1_OH + NO ----> NO +    0.4200\*xHO2 +    0.0900\*RO2C +    0.0100\*RO2XC +    0.0900\*xGLY +    0.2000\*xGLCHO +    0.4800\*xMGLY +    0.0900\*xHCOOH +    0.0500\*zRHNO3 +    0.6000\*yHPCRB +    0.0400\*CO +    0.1000\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| A5O3   | OLEA1 + O3 ---->   0.6200\*OH +    0.0100\*xOH +    0.5900\*HO2 +    0.0200\*xHO2 +    0.1200\*RO2C +    0.0100\*MECO3 +    0.0900\*xR2CO3 +    0.1600\*RCHO2 +    0.0400\*HCHO +    0.1000\*xHCHO +    0.0200\*MECHO +    0.4200\*GLY +    0.0600\*GLCHO +    0.8100\*MGLY +    0.0200\*KET2 +    0.1000\*MEOH +    0.0200\*HCOOH +    0.0100\*OACID +    0.0200\*xPACID +    0.0800\*yHPCRB +    0.0500\*ALK4 +    0.3300\*CO2 +    0.1100\*CO +    0.1200\*SumRO2 +    0.0100\*SumRCO3  |   3.5000E-18 |   3.5000E-18 |
| A5N3   | OLEA1 + NO3 ----> OLEA1_N3 +    0.5600\*xNO2 +    0.0400\*HNO3 +    0.2200\*HO2 +    0.1600\*xHO2 +    0.7100\*RO2C +    0.0600\*RO2XC +    0.0100\*xHCHO +    0.0300\*xGLY +    0.5300\*xGLCHO +    0.0200\*AFG1 +    0.0100\*AFG2B +    0.0300\*xKET2 +    0.2200\*xPACID +    0.1900\*RCNO3 +    0.0100\*xRCNO3 +    0.0200\*zRCNO3 +    0.1500\*xRHNO3 +    0.1700\*yRPNO3 +    0.0200\*zRDNO3 +    0.1500\*CO +    0.7800\*SumRO2  |   9.6400E-14 |   9.6400E-14 |
| 663   | OLEA1_N3 ---->   0.0100\*AFG2B +    0.3100\*xPACID +    0.0300\*zRCNO3  |   5.5100E+01 |   5.5100E+01 |
| 664   | OLEA1_N3 + NO ----> NO +    0.0100\*RO2C +    0.3100\*xMGLY +    0.3400\*yRPNO3 +    0.0300\*zRDNO3 +    0.0100\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| A5HV   | OLEA1 ---->   0.4300\*OH +    0.0100\*xOH +    0.4900\*HO2 +    0.1100\*xHO2 +    0.3600\*RO2C +    0.0200\*RO2XC +    0.1000\*MEO2 +    0.1300\*xMECO3 +    0.1000\*xR2CO3 +    0.0300\*MACO3 +    0.1300\*xHCHO +    0.0100\*xMECHO +    0.0600\*xRCHO +    0.0900\*GLCHO +    0.1300\*xGLCHO +    0.0600\*MGLY +    0.0300\*AFG2A +    0.0800\*AFG2B +    0.0300\*KET2 +    0.0400\*xKET2 +    0.0100\*HCOOH +    0.0100\*xPACID +    0.2500\*OLEP +    0.0200\*zRCNO3 +    0.0100\*yROOH +    0.2900\*yHPCRB +    0.0200\*ALK4 +    0.0100\*ALK5 +    0.0500\*CO2 +    1.0200\*CO +    0.4800\*SumRO2 +    0.0300\*SumRCO3  | MACR_06 | Not Available<sup>1</sup> | 
| A6OH   | OLEA2 + OH ----> OLEA2_OH +    0.0600\*OH +    0.0900\*xOH +    0.0500\*HO2 +    0.3100\*xHO2 +    0.6500\*RO2C +    0.1300\*RO2XC +    0.0100\*xMECO3 +    0.0300\*xR2CO3 +    0.1800\*MACO3 +    0.0300\*xMACO3 +    0.2200\*xHCHO +    0.3100\*xRCHO +    0.0200\*OLEA1 +    0.0200\*OLEA2 +    0.0200\*xGLY +    0.0100\*xGLCHO +    0.0100\*xMGLY +    0.0800\*xKET2 +    0.0100\*LVKS +    0.0500\*MALAH +    0.1200\*xPACID +    0.0200\*OLEP +    0.1100\*zRCNO3 +    0.0100\*zRHNO3 +    0.0400\*HPCRB +    0.4700\*yHPCRB +    0.0400\*CO2 +    0.0900\*CO +    0.7800\*SumRO2 +    0.1800\*SumRCO3  |   8.7800E-11 |   8.7800E-11 |
| 667   | OLEA2_OH ---->   0.0400\*OH +    0.0300\*xOH +    0.0300\*HO2 +    0.0200\*xKET2 +    0.0500\*xPACID +    0.0500\*HPCRB +    0.0200\*CO2  |   4.1400E+00 |   4.1400E+00 |
| 668   | OLEA2_OH + NO ----> NO +    0.0600\*xHO2 +    0.0700\*RO2C +    0.0200\*RO2XC +    0.0200\*xMECO3 +    0.0100\*xR2CO3 +    0.0200\*xHCHO +    0.0500\*xRCHO +    0.0300\*xMGLY +    0.0100\*xACET +    0.0100\*MALAH +    0.0200\*zRCNO3 +    0.1800\*yHPCRB +    0.0200\*CO +    0.0900\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| A6O3   | OLEA2 + O3 ----> OLEA2_O3 +    0.5200\*OH +    0.1200\*HO2 +    0.0800\*xHO2 +    0.3500\*RO2C +    0.0700\*RO2XC +    0.1200\*xMECO3 +    0.1300\*HCHO2 +    0.1500\*RCHO2 +    0.2900\*HCHO +    0.0800\*xHCHO +    0.3800\*RCHO +    0.1100\*xRCHO +    0.1600\*GLY +    0.0100\*xGLY +    0.2000\*MGLY +    0.0100\*xPACID +    0.0700\*zRCNO3 +    0.1400\*yHPCRB +    0.1400\*CO2 +    0.2900\*CO +    0.4300\*SumRO2  |   1.6500E-17 |   1.6500E-17 |
| 670   | OLEA2_O3 ---->   0.1500\*xOH +    0.0300\*xBACL +    0.1000\*xKET2 +    0.0800\*xPACID +    0.0500\*CO2  |   1.5300E+00 |   1.5300E+00 |
| 671   | OLEA2_O3 + NO ----> NO +    0.0200\*OH +    0.0500\*xHO2 +    0.0100\*RO2C +    0.0100\*RO2XC +    0.0700\*xR2CO3 +    0.0100\*xRCHO +    0.0300\*xGLY +    0.0700\*xMGLY +    0.0200\*KET2 +    0.0100\*zRCNO3 +    0.2400\*yHPCRB +    0.0800\*CO +    0.0200\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| A6N3   | OLEA2 + NO3 ----> OLEA2_N3 +    0.0900\*xNO2 +    0.2800\*HNO3 +    0.1300\*xOH +    0.2000\*xHO2 +    1.1700\*RO2C +    0.2200\*RO2XC +    0.0300\*xMECO3 +    0.1400\*xR2CO3 +    0.0700\*MACO3 +    0.0700\*xMACO3 +    0.0500\*xHCHO +    0.2800\*xRCHO +    0.0100\*xMGLY +    0.0100\*xAFG3 +    0.0500\*MALAH +    0.2600\*xPACID +    0.3200\*xRCNO3 +    0.2100\*zRCNO3 +    0.0200\*xRHNO3 +    0.0700\*yRPNO3 +    0.0100\*zRDNO3 +    0.0200\*yHPCRB +    0.0700\*CO2 +    0.0300\*CO +    1.4000\*SumRO2 +    0.0700\*SumRCO3  |   1.1900E-12 |   1.1900E-12 |
| 673   | OLEA2_N3 ---->   0.0100\*NO2 +    0.0200\*OH +    0.0300\*xOH +    0.1300\*xPACID +    0.0100\*RCNO3 +    0.0100\*HPCRB +    0.0300\*CO2  |   3.1700E+01 |   3.1700E+01 |
| 674   | OLEA2_N3 + NO ----> NO +    0.0200\*xNO2 +    0.0200\*xHO2 +    0.0300\*RO2C +    0.0100\*xMECO3 +    0.0700\*xHCHO +    0.0500\*xRCHO +    0.0200\*xMGLY +    0.0100\*xRCNO3 +    0.0300\*yRPNO3 +    0.2400\*yHPCRB +    0.0700\*CO +    0.0300\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| A6HV   | OLEA2 ----> OLEA2_HV +    0.1500\*OH +    0.0100\*xOH +    1.0400\*HO2 +    0.3500\*xHO2 +    1.0600\*RO2C +    0.2600\*RO2XC +    0.0400\*xMECO3 +    0.4500\*xHCHO +    0.1700\*xRCHO +    0.0100\*OLEA2 +    0.0600\*xOLEA2 +    0.1900\*xMACR +    0.1300\*xAFG2A +    0.0100\*xAFG2B +    0.0100\*xLVKS +    0.1300\*OLEP +    0.2400\*zRCNO3 +    0.0200\*zRHNO3 +    0.0700\*yRUOOH +    0.0400\*HPCRB +    1.2300\*yHPCRB +    1.1100\*CO +    1.3300\*SumRO2  | C2CHOabs | Not Available<sup>1</sup> | 
| 676   | OLEA2_HV ---->   0.1400\*OH +    0.0200\*HO2 +    0.0100\*AFG2A +    0.1100\*HPCRB +    0.0100\*CO2  |   2.2200E+00 |   2.2200E+00 |
| 677   | OLEA2_HV + NO ----> NO +    0.0900\*xHO2 +    0.2200\*RO2C +    0.0600\*RO2XC +    0.0100\*xMECO3 +    0.0400\*xHCHO +    0.0300\*xOLEA2 +    0.0300\*xLVKS +    0.0500\*zRCNO3 +    0.0100\*zRHNO3 +    0.0100\*yRUOOH +    0.2700\*yHPCRB +    0.2800\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| K5OH   | KET2 + OH ---->   0.5600\*HO2 +    0.1600\*xHO2 +    0.4500\*RO2C +    0.0700\*RO2XC +    0.0200\*MECO3 +    0.0500\*xMECO3 +    0.0100\*R2CO3 +    0.1200\*xR2CO3 +    0.0200\*HCHO +    0.1400\*xHCHO +    0.0900\*xMECHO +    0.0100\*xETCHO +    0.1300\*RCHO +    0.1600\*xRCHO +    0.3700\*MGLY +    0.0100\*xACET +    0.0700\*KET2 +    0.0300\*xKET2 +    0.0700\*zRCNO3 +    0.5000\*yHPCRB +    0.5200\*SumRO2 +    0.0200\*SumRCO3  |   9.5600E-12 |   9.5600E-12 |
| K5HV   | KET2 ---->   0.3700\*HO2 +    0.3000\*xHO2 +    0.3700\*RO2C +    0.0200\*RO2XC +    0.0900\*MEO2 +    0.1300\*ETO2 +    0.5100\*MECO3 +    0.4000\*R2CO3 +    0.3600\*HCHO +    0.0300\*xHCHO +    0.0200\*xMECHO +    0.0400\*xETCHO +    0.2600\*xRCHO +    0.0200\*zRHNO3 +    0.3900\*yROOH +    0.0800\*ALK4 +    0.0100\*ALK5 +    0.0900\*CO +    0.6100\*SumRO2 +    0.9100\*SumRCO3  |   7.5300E-02*MEK_06 | Not Available<sup>1</sup> | 
| K6OH   | LVKS + OH ----> LVKS_OH +    0.2200\*HO2 +    0.0400\*xHO2 +    0.3900\*RO2C +    0.0300\*RO2XC +    0.1300\*MECO3 +    0.0900\*xMECO3 +    0.2500\*xR2CO3 +    0.0400\*xHCHO +    0.2000\*RCHO +    0.0900\*xRCHO +    0.1300\*OLEA1 +    0.0100\*MGLY +    0.0400\*xBACL +    0.2500\*xKET2 +    0.0300\*zRCNO3 +    0.3600\*yHPCRB +    0.4200\*SumRO2 +    0.1300\*SumRCO3  |   6.0900E-11 |   6.0900E-11 |
| 681   | LVKS_OH ---->   0.2300\*OH +    0.0100\*HO2 +    0.2300\*HPCRB  |   4.6800E-01 |   4.6800E-01 |
| 682   | LVKS_OH + NO ----> NO +    0.0100\*xOH +    0.0900\*xHO2 +    0.2100\*RO2C +    0.0400\*RO2XC +    0.1000\*xMECO3 +    0.0900\*xRCHO +    0.0100\*xBACL +    0.0100\*xAFG2B +    0.0100\*xPACID +    0.0400\*zRCNO3 +    0.0900\*xHPCRB +    0.2300\*yHPCRB +    0.0100\*CO +    0.2500\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| K6O3   | LVKS + O3 ----> LVKS_O3 +    0.5300\*OH +    0.3800\*HO2 +    0.0200\*xHO2 +    0.1800\*RO2C +    0.0100\*RO2XC +    0.0100\*MECO3 +    0.0900\*xMECO3 +    0.0100\*xR2CO3 +    0.1300\*HCHO2 +    0.1300\*RCHO2 +    0.0300\*HCHO +    0.0200\*xHCHO +    0.0100\*MECHO +    0.0300\*RCHO +    0.0100\*xGLY +    0.5900\*MGLY +    0.0100\*xMGLY +    0.3200\*BACL +    0.0100\*xBACL +    0.0600\*KET2 +    0.0300\*HCOOH +    0.0100\*OACID +    0.0200\*xPACID +    0.0100\*zRCNO3 +    0.0400\*yHPCRB +    0.4000\*CO2 +    0.3000\*CO +    0.1900\*SumRO2 +    0.0100\*SumRCO3  |   3.0400E-17 |   3.0400E-17 |
| 684   | LVKS_O3 ---->   0.0200\*OH +    0.0100\*xOH +    0.0400\*xMECO3 +    0.1200\*xPACID +    0.0100\*CO2  |   5.1300E+00 |   5.1300E+00 |
| 685   | LVKS_O3 + NO ----> NO +    0.0500\*xHO2 +    0.0200\*RO2C +    0.0100\*RO2XC +    0.0200\*xHCHO +    0.1000\*xMGLY +    0.0100\*zRCNO3 +    0.1300\*yHPCRB +    0.0200\*CO +    0.0300\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| K6HV   | LVKS ---->   0.0600\*xOH +    0.1400\*HO2 +    0.1200\*RO2C +    0.0300\*RO2XC +    0.2600\*MEO2 +    0.0600\*xMECO3 +    0.2500\*MACO3 +    0.1400\*HCHO +    0.0600\*xMGLY +    0.0600\*MALAH +    0.0600\*xPACID +    0.6000\*OLEP +    0.0300\*zRCNO3 +    0.6000\*CO +    0.4100\*SumRO2 +    0.2500\*SumRCO3  | MVK_16 | Not Available<sup>1</sup> | 
| O5OH   | OLEP + OH ----> OLEP_OH +    0.0100\*OH +    0.1300\*HO2 +    0.5500\*xHO2 +    0.6500\*RO2C +    0.1400\*RO2XC +    0.0200\*xHCHO +    0.5300\*xRCHO +    0.1100\*OLEA1 +    0.0100\*xBACL +    0.0200\*xKET2 +    0.0100\*xMVK +    0.0100\*OLEP +    0.1400\*zRCNO3 +    0.0300\*yROOH +    0.0100\*yRUOOH +    0.0100\*HPCRB +    0.7400\*yHPCRB +    0.8000\*SumRO2  |   8.3400E-11 |   8.3400E-11 |
| 688   | OLEP_OH ---->   0.0500\*OH +    0.0100\*xOH +    0.1000\*HO2 +    0.1200\*HPCRB +    0.0100\*CO2  |   2.3000E+00 |   2.3000E+00 |
| 689   | OLEP_OH + NO ----> NO +    0.0700\*xHO2 +    0.2500\*RO2C +    0.0300\*RO2XC +    0.0600\*xMECO3 +    0.1100\*xHCHO +    0.0600\*xGLCHO +    0.0200\*xMGLY +    0.0300\*xKET2 +    0.0200\*zRCNO3 +    0.0100\*zRHNO3 +    0.0700\*yRUOOH +    0.1400\*yHPCRB +    0.0400\*CO +    0.2800\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| O5O3   | OLEP + O3 ----> OLEP_O3 +    0.6100\*OH +    0.0500\*HO2 +    0.0800\*xHO2 +    0.3300\*RO2C +    0.0900\*RO2XC +    0.0400\*xMECO3 +    0.1100\*xR2CO3 +    0.0600\*HCHO2 +    0.2300\*RCHO2 +    0.1400\*HCHO +    0.1400\*xHCHO +    0.0800\*RCHO +    0.0900\*xRCHO +    0.0200\*xGLY +    0.1300\*KET2 +    0.0900\*zRCNO3 +    0.3500\*yHPCRB +    0.0300\*ALK2 +    0.0900\*CO2 +    0.1800\*CO +    0.4100\*SumRO2  |   1.5000E-16 |   1.5000E-16 |
| 691   | OLEP_O3 ---->   0.1700\*OH +    0.0400\*HO2 +    0.0100\*KET2 +    0.1700\*HPCRB  |   9.3600E-01 |   9.3600E-01 |
| 692   | OLEP_O3 + NO ----> NO +    0.1700\*RO2C +    0.0600\*RO2XC +    0.1500\*xMECO3 +    0.0100\*xR2CO3 +    0.1500\*xRCHO +    0.0600\*zRCNO3 +    0.1800\*yHPCRB +    0.2200\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| O5N3   | OLEP + NO3 ---->   0.7400\*xNO2 +    0.0800\*xHO2 +    0.8800\*RO2C +    0.1600\*RO2XC +    0.0100\*xMECO3 +    0.1800\*xHCHO +    0.6400\*xRCHO +    0.1100\*xKET2 +    0.0900\*xRCNO3 +    0.1400\*zRCNO3 +    0.2800\*yRPNO3 +    0.0200\*zRDNO3 +    0.0100\*yHPCRB +    1.0400\*SumRO2  |   8.5000E-12 |   8.5000E-12 |
| OAOH   | OACID + OH ---->   0.3000\*xHO2 +    0.3000\*RO2C +    0.7000\*MEO2 +    0.0200\*xHCHO +    0.2800\*xMGLY +    0.2600\*yHPCRB +    0.7200\*CO2 + SumRO2  |   7.4700E-13 |   7.4700E-13 |
| PAOH   | PACID + OH ---->   0.1900\*xOH +    0.5600\*xHO2 +    0.7400\*RO2C +    0.2600\*MECO3 +    0.1900\*xHCHO +    0.5600\*xPACID +    0.1900\*CO2 +    0.7400\*SumRO2 +    0.2600\*SumRCO3  |   3.0000E-14 |   3.0000E-14 |
| PAHV   | PACID ----> OH + MEO2 + CO2 + SumRO2  | PAA | Not Available<sup>1</sup> | 
| MGOH   | MGLY + OH ---->   0.0100\*xHO2 +    0.0100\*RO2C +    0.9900\*MECO3 +    0.0100\*xHCHO +    0.0100\*xPACID + CO +    0.0100\*SumRO2 +    0.9900\*SumRCO3  |   1.1900E-11 |   1.1900E-11 |
| MGN3   | MGLY + NO3 ----> HNO3 + MECO3 + CO + SumRCO3  |   5.0000E-16 |   5.0000E-16 |
| MGHV   | MGLY ----> HO2 + MECO3 + CO + SumRCO3  | MGLY_13 | Not Available<sup>1</sup> | 
| BAHV   | BACL ---->   2.0000\*MECO3 +    2.0000\*SumRCO3  | BACL_11 | Not Available<sup>1</sup> | 
| CROH   | CRES + OH ---->   0.8400\*HO2 +    0.1100\*xHO2 +    0.1100\*RO2C +    0.0200\*RO2XC +    0.0300\*BZO +    0.0200\*OLEA1 +    0.0800\*OLEA2 +    0.0200\*xGLY +    0.0500\*xMGLY +    0.0300\*xBACL +    0.0300\*xBUDAL +    0.0100\*xAFG1 +    0.0500\*xAFG2A +    0.0200\*xAFG2B +    0.0100\*xBALD +    0.1700\*LVKS +    0.1400\*OLEP +    0.4200\*CATL +    0.0200\*zRANO3 +    0.0200\*yROOH +    0.1200\*yRAOOH +    0.1300\*SumRO2  |   4.6500E-11 |   4.6500E-11 |
| CRN3   | CRES + NO3 ----> HNO3 + BZO  |   1.2700E-11 |   1.2700E-11 |
| XLOH   | XYNL + OH ---->   0.7800\*HO2 +    0.1600\*xHO2 +    0.1700\*RO2C +    0.0400\*RO2XC +    0.0200\*BZO +    0.0200\*OLEA1 +    0.0600\*OLEA2 +    0.0100\*xGLY +    0.0400\*xMGLY +    0.1000\*xBACL +    0.0100\*xBUDAL +    0.0100\*xAFG1 +    0.0900\*xAFG2A +    0.0300\*xAFG2B +    0.0100\*xBALD +    0.2600\*LVKS +    0.0100\*xAFG3 +    0.1600\*OLEP +    0.0100\*XYNL +    0.2700\*CATL +    0.0300\*zRANO3 +    0.0200\*yROOH +    0.1800\*yRAOOH +    0.2000\*SumRO2  |   6.7300E-11 |   6.7300E-11 |
| XLN3   | XYNL + NO3 ----> HNO3 + BZO  |   3.0900E-11 |   3.0900E-11 |
| CAOH   | CATL + OH ---->   0.9600\*HO2 +    0.0300\*xHO2 +    0.0300\*RO2C +    0.0100\*RO2XC +    0.0100\*BZO +    0.0100\*OLEA1 +    0.0200\*OLEA2 +    0.0200\*xBACL +    0.0200\*xAFG2A +    0.7300\*LVKS +    0.0600\*OLEP +    0.1200\*CATL3 +    0.0100\*zRANO3 +    0.0300\*yRAOOH +    0.0300\*SumRO2  |   1.5600E-10 |   1.5600E-10 |
| CAN3   | CATL + NO3 ----> HNO3 + BZO  |   4.0400E-11 |   4.0400E-11 |
| N4OH   | RCNO3 + OH ----> RCNO3_OH +    0.3400\*NO2 +    0.1000\*xNO2 +    0.0200\*OH +    0.0100\*xOH +    0.0500\*HO2 +    0.1300\*xHO2 +    0.4900\*RO2C +    0.0700\*RO2XC +    0.0500\*xMECO3 +    0.0700\*R2CO3 +    0.0800\*xR2CO3 +    0.0300\*MACO3 +    0.1300\*xHCHO +    0.0500\*xMECHO +    0.0500\*RCHO +    0.0500\*xRCHO +    0.0100\*xGLY +    0.0500\*MGLY +    0.0200\*xACET +    0.1200\*KET2 +    0.0100\*xKET2 +    0.0100\*PACID +    0.0400\*xPACID +    0.0400\*OLEP +    0.0700\*RCNO3 +    0.2100\*xRCNO3 +    0.0700\*zRCNO3 +    0.0300\*yRPNO3 +    0.0300\*yHPCRB +    0.0600\*ALK2 +    4.7400\*NROG +    0.0100\*CO2 +    0.0100\*CO +    0.5600\*SumRO2 +    0.0900\*SumRCO3  |   2.0600E-11 |   2.0600E-11 |
| 708   | RCNO3_OH ---->   0.0500\*HO2 +    0.0100\*xPACID +    0.0500\*RCNO3  |   2.7600E+00 |   2.7600E+00 |
| 709   | RCNO3_OH + NO ----> NO +    0.0100\*xHO2 +    0.0700\*RO2C +    0.0200\*RO2XC +    0.0300\*xR2CO3 +    0.0300\*xHCHO +    0.0100\*xGLY +    0.0400\*xRCNO3 +    0.0200\*zRCNO3 +    0.0100\*yRPNO3 +    0.0200\*yHPCRB +    0.0900\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| N4HV   | RCNO3 ----> RCNO3_HV +    0.7700\*NO2 +    0.0400\*xNO2 +    0.0100\*xOH +    0.3800\*HO2 +    0.1900\*xHO2 +    0.5300\*RO2C +    0.1000\*RO2XC +    0.0700\*ETO2 +    0.1100\*MECO3 +    0.0400\*R2CO3 +    0.0300\*xR2CO3 +    0.1100\*HCHO +    0.0200\*xHCHO +    0.0500\*MECHO +    0.0200\*RCHO +    0.0700\*xRCHO +    0.0900\*OLEA2 +    0.0300\*MACR +    0.0400\*AFG2A +    0.0700\*xACET +    0.0400\*xKET2 +    0.0400\*OACID +    0.0300\*xOACID +    0.0200\*PACID +    0.0400\*xPACID +    0.0200\*xRCNO3 +    0.0800\*zRCNO3 +    0.0300\*xRHNO3 +    0.1700\*yRPNO3 +    0.0200\*zRDNO3 +    0.2200\*yHPCRB +    8.7700\*NROG +    0.0100\*CO2 +    0.3400\*CO +    0.7100\*SumRO2 +    0.1400\*SumRCO3  | CRBNIT | Not Available<sup>1</sup> | 
| 711   | RCNO3_HV ---->   0.2500\*HO2 +    0.0600\*PACID +    0.0200\*xPACID +    0.0600\*RCNO3 +    0.0200\*RPNO3 +    0.1000\*HPCRB  |   3.1500E-01 |   3.1500E-01 |
| 712   | RCNO3_HV + NO ----> NO +    0.0500\*xOH +    0.0600\*xHO2 +    0.2100\*RO2C +    0.0400\*RO2XC +    0.0500\*xMECO3 +    0.0500\*xR2CO3 +    0.0100\*xHCHO +    0.1400\*xRCHO +    0.0100\*xMGLY +    0.0400\*zRCNO3 +    0.0700\*xRHNO3 +    0.0200\*yRPNO3 +    0.1400\*yHPCRB +    0.0500\*CO2 +    0.0200\*CO +    0.2500\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| N3OH   | RHNO3 + OH ---->   0.6000\*NO2 +    0.0800\*HO2 +    0.2600\*xHO2 +    0.3000\*RO2C +    0.0500\*RO2XC +    0.0700\*xHCHO +    0.0300\*xGLCHO +    0.0100\*KET2 +    0.0700\*xKET2 +    0.0100\*LVKS +    0.0800\*RCNO3 +    0.2100\*xRCNO3 +    0.0500\*xRHNO3 +    0.3400\*yRPNO3 +    0.0500\*zRDNO3 +    0.0200\*ALK4 +    0.5600\*ALK5 +    0.3500\*SumRO2  |   3.8000E-11 |   3.8000E-11 |
| N3HV   | RHNO3 ----> NO2 +    0.9400\*HO2 +    0.0300\*xHO2 +    0.0900\*RO2C +    0.0200\*RO2XC +    0.7400\*HCHO +    0.0200\*xHCHO +    0.0200\*MECHO +    0.0600\*OLEA1 +    0.0600\*OLEA2 +    0.0200\*xOLEA2 +    0.3200\*MACR +    0.0200\*ACET +    0.0300\*KET2 +    0.3700\*MVK +    0.0200\*OLEP +    0.0100\*zRCNO3 +    0.0100\*zRHNO3 +    0.0500\*yRUOOH +    0.0100\*HPCRB +    0.0500\*yHPCRB +    0.0700\*FURNS +    0.1100\*SumRO2  | IC3ONO2 | Not Available<sup>1</sup> | 
| N5OH   | RANO3 + OH ---->   0.4200\*NO2 +    0.5800\*HO2 +    0.0200\*AFG2A +    0.0100\*OLEP +    0.4700\*RCNO3 +    0.1100\*RHNO3 +    0.0100\*ALK5 +    0.3700\*ALK6  |   4.4900E-11 |   4.4900E-11 |
| N5HV   | RANO3 ---->   0.6600\*RCNO3 +    0.3400\*RHNO3  | COOH | Not Available<sup>1</sup> | 
| N6OH   | RPNO3 + OH ----> RPNO3_OH +    0.0100\*zRNNO3 +    0.3700\*NO2 +    0.0700\*xNO2 +    0.1900\*OH +    0.1200\*HO2 +    0.1100\*xHO2 +    0.2300\*RO2C +    0.0500\*RO2XC +    0.1000\*xHCHO +    0.0400\*xOLEA1 +    0.0100\*xOLEA2 +    0.0300\*xKET2 +    0.0200\*xMVK +    0.0300\*RCNO3 +    0.1000\*xRCNO3 +    0.1600\*RHNO3 +    0.1200\*RPNO3 +    0.0100\*xRPNO3 +    0.1400\*yRPNO3 +    0.0300\*zRDNO3 +    0.3200\*ROOH +    0.0500\*HPCRB +    0.0100\*xHPCRB +    0.2800\*SumRO2  |   5.1900E-11 |   5.1900E-11 |
| 718   | RPNO3_OH ---->   0.0300\*NO2 +    0.0500\*OH +    0.0200\*HO2 +    0.0500\*RCNO3 +    0.0100\*RPNO3 +    0.0300\*HPCRB  |   4.7700E-01 |   4.7700E-01 |
| 719   | RPNO3_OH + NO ----> NO +    0.0300\*xNO2 +    0.0500\*xHO2 +    0.0900\*RO2C +    0.0100\*RO2XC +    0.0200\*xHCHO +    0.0300\*xGLY +    0.0200\*xOLEP +    0.0300\*xRCNO3 +    0.0200\*xRPNO3 +    0.1000\*yRPNO3 +    0.0100\*zRDNO3 +    0.0200\*xHPCRB +    0.1100\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| N6HV   | RPNO3 ----> RPNO3_HV +    0.8100\*NO2 + OH +    0.1400\*HO2 +    0.0100\*RO2C +    0.6300\*HCHO +    0.4300\*OLEA1 +    0.1000\*OLEA2 +    0.1200\*MVK +    0.1500\*OLEP +    0.1400\*RCNO3 +    0.0100\*yRPNO3 +    0.0100\*SumRO2  | COOH | Not Available<sup>1</sup> | 
| 721   | RPNO3_HV ---->   0.0300\*NO2 +    0.0100\*HO2 +    0.0100\*RPNO3 +    0.0300\*HPCRB  |   1.1100E+00 |   1.1100E+00 |
| 722   | RPNO3_HV + NO ----> NO +    0.0400\*xHO2 +    0.0500\*RO2C +    0.0100\*RO2XC +    0.0300\*xHCHO +    0.0300\*xRCNO3 +    0.0100\*xRHNO3 +    0.0500\*yRPNO3 +    0.0500\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| NDOH   | RDNO3 + OH ---->   0.5300\*NO2 +    0.0400\*xNO2 +    0.0100\*HO2 +    0.2500\*xHO2 +    0.6000\*RO2C +    0.1500\*RO2XC +    0.2000\*xHCHO +    0.0300\*xACET +    0.1000\*xKET2 +    0.0200\*RCNO3 +    0.2500\*xRCNO3 +    0.0100\*zRCNO3 +    0.5200\*RHNO3 +    0.0500\*xRDNO3 +    0.1300\*zRDNO3 +    0.0100\*CO +    0.7500\*SumRO2  |   3.6000E-11 |   3.6000E-11 |
| NDHV   | RDNO3 ----> RDNO3_HV +    1.8600\*NO2 +    0.0300\*HO2 +    0.0100\*xHO2 +    0.0800\*RO2C +    0.0300\*RO2XC +    0.0100\*xR2CO3 +    0.3000\*HCHO +    0.2100\*OLEA1 +    0.3500\*OLEA2 +    0.0200\*xACET +    0.0500\*KET2 +    0.2500\*MVK +    0.0300\*RCNO3 +    0.0100\*xRCNO3 +    0.0300\*zRCNO3 +    0.1100\*SumRO2  | DIONO2 | Not Available<sup>1</sup> | 
| 725   | RDNO3_HV ---->   0.0400\*NO2 +    0.0200\*HO2 +    0.0200\*RPNO3 +    0.0400\*HPCRB  |   1.0900E+00 |   1.0900E+00 |
| 726   | RDNO3_HV + NO ----> NO +    0.0600\*xHO2 +    0.0600\*RO2C +    0.0100\*RO2XC +    0.0400\*xHCHO +    0.0400\*xRCNO3 +    0.0200\*xRHNO3 +    0.0600\*yRPNO3 +    0.0100\*zRDNO3 +    0.0700\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| N1OH   | R1NO3 + OH ---->   0.1700\*NO2 +    0.4100\*xNO2 +    0.0100\*HO2 +    0.2900\*xHO2 +    0.9900\*RO2C +    0.1200\*RO2XC +    0.1000\*xHCHO +    0.2900\*xMECHO +    0.0500\*xETCHO +    0.0200\*xRCHO +    0.0500\*ACET +    0.2500\*xACET +    0.0500\*MEK +    0.0400\*xMEK +    0.0700\*KET2 +    0.0500\*xKET2 +    0.1900\*xRCNO3 +    0.1000\*xRHNO3 +    0.0100\*RPNO3 +    1.1200\*yRPNO3 +    0.1200\*zRDNO3 +    1.1200\*SumRO2  |   1.4700E-12 |   1.4700E-12 |
| N1HV   | R1NO3 ----> NO2 +    0.1900\*HO2 +    0.3800\*xHO2 +    0.4500\*RO2C +    0.0600\*RO2XC +    0.3200\*ETO2 +    0.0400\*TBUO +    0.0100\*xTBUO +    0.0500\*xHCHO +    0.1200\*MECHO +    0.0300\*ETCHO +    0.0300\*xETCHO +    0.0200\*xRCHO +    0.3400\*ACET +    0.1200\*xACET +    0.1100\*MEK +    0.0500\*KET2 +    0.2200\*xKET2 +    0.0100\*zR1NO3 +    0.0600\*zRHNO3 +    0.5100\*yROOH +    0.8300\*SumRO2  | IC3ONO2 | Not Available<sup>1</sup> | 
| N2OH   | R2NO3 + OH ---->   0.0600\*NO2 +    0.1200\*xNO2 +    0.0100\*HO2 +    0.5200\*xHO2 +    1.1200\*RO2C +    0.2900\*RO2XC +    0.0600\*xRCHO +    0.0400\*KET2 +    0.0700\*xKET2 +    0.0100\*RCNO3 +    0.4900\*xRCNO3 +    0.0100\*zRCNO3 +    0.0300\*xRHNO3 +    1.3600\*yRPNO3 +    0.2900\*zRDNO3 +    0.0100\*HPCRB +    0.0100\*CO +    1.4200\*SumRO2  |   2.5100E-11 |   2.5100E-11 |
| N2HV   | R2NO3 ----> R2NO3_HV + NO2 +    0.1000\*HO2 +    0.4600\*xHO2 +    0.7400\*RO2C +    0.2100\*RO2XC +    0.0500\*xMACO3 +    0.0400\*xRCHO +    0.0800\*KET2 +    0.3700\*xKET2 +    0.0400\*xMVK +    0.0300\*xPACID +    0.0700\*zRCNO3 +    0.1400\*zRHNO3 +    0.5100\*yROOH +    0.0200\*HPCRB +    0.3600\*yHPCRB +    0.0100\*CO +    0.9500\*SumRO2  | IC3ONO2 | Not Available<sup>1</sup> | 
| 731   | R2NO3_HV ---->   0.0900\*OH +    0.0900\*HO2 +    0.0700\*PACID +    0.0300\*xPACID +    0.1000\*HPCRB  |   9.6900E-01 |   9.6900E-01 |
| 732   | R2NO3_HV + NO ----> NO +    0.1300\*xHO2 +    0.2200\*RO2C +    0.0500\*RO2XC +    0.0300\*xHCHO +    0.1100\*xRCHO +    0.0400\*xOLEA2 +    0.0300\*zRCNO3 +    0.0200\*zRHNO3 +    0.2900\*yHPCRB +    0.0100\*CO +    0.2700\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| H4OH   | RAOOH + OH ---->   0.7600\*OH +    0.1700\*HO2 +    0.0600\*xHO2 +    0.0600\*RO2C +    0.0100\*RO2XC +    0.3100\*RCHO +    0.0200\*xGLY +    0.0400\*xMGLY +    0.0200\*xBUDAL +    0.0200\*AFG2A +    0.0300\*xAFG2A +    0.0100\*KET2 +    0.0300\*OLEP +    0.0100\*zRANO3 +    0.0700\*yRAOOH +    0.1700\*HPCRB +    0.0100\*ALK5 +    0.3700\*ALK6 +    0.0700\*SumRO2  |   8.2700E-11 |   8.2700E-11 |
| H4HV   | RAOOH ----> HPCRB  | COOH | Not Available<sup>1</sup> | 
| H3OH   | RUOOH + OH ----> RUOOH_OH +    0.6300\*OH +    0.0800\*HO2 +    0.1000\*xHO2 +    0.1100\*RO2C +    0.0100\*RO2XC +    0.0800\*xHCHO +    0.0300\*xMACR +    0.0500\*xMVK +    0.0200\*LVKS +    0.0100\*zRHNO3 +    0.0100\*yROOH +    0.1000\*yRUOOH +    0.0800\*HPCRB +    0.0100\*xHPCRB +    0.0200\*ALK4 +    0.5900\*ALK5 +    0.0100\*xFURNS +    0.1200\*SumRO2  |   5.9700E-11 |   5.9700E-11 |
| 736   | RUOOH_OH ---->   0.0600\*OH +    0.1200\*HO2 +    0.1700\*HPCRB  |   1.2800E-02 |   1.2800E-02 |
| 737   | RUOOH_OH + NO ----> NO +    0.1600\*xHO2 +    0.1600\*RO2C +    0.0100\*RO2XC +    0.0300\*xHCHO +    0.0300\*xGLCHO +    0.0500\*xKET2 +    0.0100\*zRPNO3 +    0.1700\*yROOH +    0.1600\*xHPCRB +    0.1700\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| H3HV   | RUOOH ----> OH +    0.9900\*HO2 +    0.0200\*RO2C +    0.8600\*HCHO +    0.0400\*OLEA1 +    0.3900\*MACR +    0.0100\*KET2 +    0.4600\*MVK +    0.0100\*OLEP +    0.0100\*yRUOOH +    0.0100\*yHPCRB +    0.0900\*FURNS +    0.0200\*SumRO2  | COOH | Not Available<sup>1</sup> | 
| H5OH   | HPCRB + OH ----> HPCRB_OH +    0.5000\*OH +    0.0100\*HO2 +    0.0300\*xHO2 +    0.0300\*RO2C +    0.0300\*xHCHO +    0.3500\*RCHO +    0.0100\*OLEA1 +    0.0200\*AFG1 +    0.0300\*xPACID +    0.1300\*OLEP +    0.0100\*HPCRB +    0.0300\*SumRO2  |   5.4000E-11 |   5.4000E-11 |
| 740   | HPCRB_OH ---->   0.4400\*OH +    0.0200\*HO2 +    0.0100\*AFG2B +    0.0100\*PACID +    0.0800\*OLEP +    0.3600\*HPCRB  |   6.3700E-01 |   6.3700E-01 |
| 741   | HPCRB_OH + NO ----> NO +    0.4200\*xHO2 +    0.4300\*RO2C +    0.0400\*RO2XC +    0.0100\*xHCHO +    0.0500\*xGLY +    0.0100\*xGLCHO +    0.2400\*xMGLY +    0.0500\*xAFG2B +    0.0100\*xPACID +    0.0100\*zRCNO3 +    0.0300\*zRPNO3 +    0.3400\*xHPCRB +    0.4500\*yHPCRB +    0.0400\*CO +    0.4700\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| H5HV   | HPCRB ----> OH +    0.0100\*xOH +    0.9000\*HO2 +    0.0800\*xHO2 +    0.0900\*RO2C +    0.0100\*RO2XC +    0.1100\*HCHO +    0.0600\*xHCHO +    0.1100\*OLEA1 +    0.0500\*AFG1 +    0.2100\*AFG2A +    0.5300\*AFG2B +    0.0200\*xAFG2B +    0.0100\*xLVKS +    0.0600\*xPACID +    0.0100\*zRCNO3 +    0.0100\*CO2 +    0.1000\*SumRO2  |   1.0000E-01*HPALDS | Not Available<sup>1</sup> | 
| H2OH   | ROOH + OH ----> ROOH_OH +    0.1900\*OH +    0.0300\*xOH +    0.2200\*HO2 +    0.3700\*xHO2 +    0.5600\*RO2C +    0.0400\*RO2XC +    0.0700\*xETO2 +    0.0200\*xTBUO +    0.0100\*HCHO +    0.3500\*xHCHO +    0.1100\*xMECHO +    0.0100\*xETCHO +    0.0100\*RCHO +    0.0100\*xRCHO +    0.0600\*GLCHO +    0.0400\*xGLCHO +    0.0200\*ACET +    0.1500\*xACET +    0.0200\*MEK +    0.0200\*xMEK +    0.0800\*KET2 +    0.0400\*xKET2 +    0.0200\*zR1NO3 +    0.0100\*zRHNO3 +    0.0100\*zRPNO3 +    0.5900\*yROOH +    0.2200\*HPCRB +    0.0100\*yHPCRB +    0.6000\*SumRO2  |   1.1600E-11 |   1.1600E-11 |
| 744   | ROOH_OH ---->   0.0400\*OH +    0.0100\*HO2 +    0.0500\*HPCRB  |   6.7800E-02 |   6.7800E-02 |
| 745   | ROOH_OH + NO ----> NO +    0.0300\*xOH +    0.0100\*xHO2 +    0.0500\*RO2C +    0.0100\*RO2XC +    0.0100\*xHCHO +    0.0300\*xMECHO +    0.0100\*xETCHO +    0.0100\*xRCHO +    0.0100\*xACET +    0.0100\*zRPNO3 +    0.0600\*yROOH +    0.0100\*xHPCRB +    0.0600\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| H2HV   | ROOH ----> OH +    0.7400\*HO2 +    0.0900\*xHO2 +    0.1300\*RO2C +    0.0200\*RO2XC +    0.1200\*ETO2 +    0.0200\*TBUO +    0.7700\*HCHO +    0.2500\*MECHO +    0.0100\*ETCHO +    0.0100\*xETCHO +    0.0100\*xRCHO +    0.1000\*GLCHO +    0.1800\*ACET +    0.0300\*xACET +    0.0400\*MEK +    0.0600\*KET2 +    0.0500\*xKET2 +    0.0100\*zRCNO3 +    0.0100\*zRHNO3 +    0.1000\*yROOH +    0.0300\*yHPCRB +    0.2700\*SumRO2  | COOH | Not Available<sup>1</sup> | 
| F2OH   | AFG1 + OH ----> AFG1_OH +    0.4300\*OH +    0.0200\*HO2 +    0.1900\*xHO2 +    0.1900\*RO2C +    0.0200\*RO2XC +    0.1800\*xGLY +    0.4100\*MALAH +    0.0300\*xPACID +    0.0300\*HPCRB +    0.0100\*CO +    0.2100\*SumRO2  |   3.3900E-11 |   3.3900E-11 |
| 748   | AFG1_OH ---->   0.0200\*OH +    0.3300\*HO2 +    0.0200\*PACID +    0.1600\*xPACID +    0.0200\*zRCNO3 +    0.3300\*HPCRB  |   1.3100E+01 |   1.3100E+01 |
| 749   | AFG1_OH + NO ----> NO +    0.3000\*xHO2 +    0.3200\*RO2C +    0.0300\*RO2XC +    0.3000\*xGLY +    0.4600\*xMGLY +    0.0200\*MALAH +    0.4300\*yHPCRB +    0.3500\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| F2HV   | AFG1 ----> AFG1_HV +    0.3000\*OH +    0.9400\*HO2 +    0.0300\*xHO2 +    0.0300\*RO2C +    0.0600\*MEO2 +    0.0100\*AFG1 +    0.0200\*AFG2A +    0.2600\*PACID +    0.0300\*xPACID +    0.0200\*HPCRB +    0.0300\*CO +    0.0900\*SumRO2  |   2.2000E-01*AFGS | Not Available<sup>1</sup> | 
| 751   | AFG1_HV ---->   0.6700\*OH +    0.3300\*PACID +    0.3300\*HPCRB  |   1.7500E+01 |   1.7500E+01 |
| 752   | AFG1_HV + NO ----> NO +    0.5200\*xOH +    0.0700\*xHO2 +    0.5900\*RO2C +    0.0800\*RO2XC +    0.5200\*MALAH +    0.0800\*zRCNO3 +    0.6700\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| F3OH   | AFG2A + OH ----> AFG2A_OH +    0.5700\*xHO2 +    0.7300\*RO2C +    0.0700\*RO2XC +    0.1500\*xMECO3 +    0.0100\*xR2CO3 +    0.2000\*MACO3 +    0.0300\*xRCHO +    0.4700\*xGLY +    0.5400\*xMGLY +    0.0200\*xPACID +    0.0700\*zRCNO3 +    0.4600\*yHPCRB +    0.0300\*CO +    0.8000\*SumRO2 +    0.2000\*SumRCO3  |   5.9900E-11 |   5.9900E-11 |
| 754   | AFG2A_OH ---->   0.2200\*xPACID  |   2.3400E+01 |   2.3400E+01 |
| 755   | AFG2A_OH + NO ----> NO +    0.1400\*xRCHO +    0.0600\*xGLY +    0.2000\*yHPCRB  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| F3HV   | AFG2A ----> OH +    0.9100\*MEO2 +    0.0900\*ETO2 + MALAH + SumRO2  |   2.5000E-01*AFGS | Not Available<sup>1</sup> | 
| F4OH   | AFG2B + OH ----> AFG2B_OH +    0.0200\*HO2 +    0.4000\*xHO2 +    0.4000\*RO2C +    0.0500\*RO2XC +    0.1700\*MACO3 +    0.3900\*xGLY +    0.3900\*xBACL +    0.0500\*zRCNO3 +    0.0200\*HPCRB +    0.3800\*yHPCRB +    0.4500\*SumRO2 +    0.1700\*SumRCO3  |   4.5800E-11 |   4.5800E-11 |
| 758   | AFG2B_OH ---->   0.3600\*HO2 +    0.3600\*HPCRB  |   1.1600E+01 |   1.1600E+01 |
| 759   | AFG2B_OH + NO ----> NO +    0.2000\*xHO2 +    0.3200\*RO2C +    0.0400\*RO2XC +    0.1200\*xMECO3 +    0.1300\*xRCHO +    0.2000\*xGLY +    0.2000\*xBACL +    0.0400\*zRCNO3 +    0.3100\*yHPCRB +    0.3600\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| F4HV   | AFG2B ----> AFG2B_HV +    0.8800\*OH +    0.0300\*xHO2 +    0.0300\*RO2C + MEO2 +    0.8700\*MALAH +    0.0300\*xPACID +    0.0300\*CO +    1.0300\*SumRO2  |   2.2000E-01*AFGS | Not Available<sup>1</sup> | 
| 761   | AFG2B_HV ---->   0.0900\*OH +    0.0500\*PACID +    0.0500\*HPCRB  |   8.6200E+00 |   8.6200E+00 |
| 762   | AFG2B_HV + NO ----> NO +    0.0800\*RO2C +    0.0800\*MALAH +    0.0900\*SumRO2  |   2.55E-12e<sup>   380.00/T</sup> |   9.1214E-12 |
| F5OH   | AFG3 + OH ---->   0.2700\*xHO2 +    0.8900\*RO2C +    0.1100\*RO2XC +    0.6200\*xMECO3 +    0.6200\*xRCHO +    0.5400\*xMGLY +    0.1100\*zRCNO3 +    0.8500\*yHPCRB + SumRO2  |   7.2000E-11 |   7.2000E-11 |
| P2UI   | PAN2 ----> NO2 + R2CO3 + SumRCO3  |   3.3900E-04 |   3.3900E-04 |
| P2OH   | PAN2 + OH ---->   0.0700\*zPAN2 +    0.0500\*NO3 +    0.7300\*xNO3 +    1.1500\*xHO2 +    1.5500\*RO2C +    0.0700\*RO2XC +    0.1700\*xHCHO +    0.6600\*xMECHO +    0.0700\*xETCHO +    0.1500\*xPAN2 +    0.0500\*ALK3 +    0.7300\*CO2 +    0.5000\*CO +    1.6200\*SumRO2  |   3.4200E-12 |   3.4200E-12 |
| P2HV   | PAN2 ----> NO2 + R2CO3 + SumRCO3  | PPN_11 | Not Available<sup>1</sup> | 
| P4UI   | APANS ----> NO2 + MACO3 + SumRCO3  |   3.3900E-04 |   3.3900E-04 |
| P4OH   | APANS + OH ---->   0.0100\*zPAN2 +    0.7400\*NO3 +    0.1800\*xNO3 +    0.0700\*xHO2 +    0.2500\*RO2C +    0.0100\*RO2XC +    0.0700\*xHCHO +    0.1800\*xKET2 +    0.1900\*OACID +    0.0700\*xPAN2 +    0.5600\*ALK4 +    0.1800\*CO2 +    0.2600\*SumRO2  |   2.9000E-11 |   2.9000E-11 |
| P4O3   | APANS + O3 ---->   0.0500\*NO2 +    0.1900\*OH +    0.2500\*HO2 +    0.0900\*RO2C +    0.0900\*xR2CO3 +    0.3800\*HCHO2 +    0.0100\*RCHO2 +    0.1000\*HCHO +    0.0400\*xHCHO +    0.9000\*PAN2 +    0.2100\*CO2 +    0.3100\*CO +    0.0900\*SumRO2  |   8.2000E-18 |   8.2000E-18 |
| P4N3   | APANS + NO3 ---->   0.0500\*zPAN2 +    0.9500\*xNO3 +    0.9500\*RO2C +    0.0500\*RO2XC +    0.9500\*xRCNO3 +    0.9500\*CO2 + SumRO2  |   1.6000E-16 |   1.6000E-16 |
| P4HV   | APANS ---->   0.6000\*NO2 +    0.4000\*NO3 +    0.4000\*MEO2 +    0.6000\*MACO3 +    0.4000\*HCHO +    0.4000\*CO2 +    0.4000\*CO +    0.4000\*SumRO2 +    0.6000\*SumRCO3  | PPN_11 | Not Available<sup>1</sup> | 
| AALK   | SOAALK + OH ----> OH +    0.0060\*SVAVB2 +    0.0520\*SVAVB3 +    0.0810\*SVAVB4  |   2.70E-12e<sup>   374.00/T</sup> |   9.4655E-12 |
| AE51   | BENZRO2 + NO ----> NO +    0.0340\*SVAVB2 +    0.3920\*SVAVB4  |   R2NO |   9.1214E-12<sup>7</sup>| 
| AE52   | BENZRO2 + HO2 ----> HO2 +    0.1460\*SVAVB1  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| AE53   | XYLRO2 + NO ----> NO +    0.0150\*SVAVB2 +    0.0230\*SVAVB3 +    0.0600\*SVAVB4  |   R2NO |   9.1214E-12<sup>7</sup>| 
| AE54   | XYLRO2 + HO2 ----> HO2 +    0.1930\*SVAVB1  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| AE55   | TOLRO2 + NO ----> NO +    0.0160\*SVAVB2 +    0.0510\*SVAVB3 +    0.0470\*SVAVB4  |   R2NO |   9.1214E-12<sup>7</sup>| 
| AE56   | TOLRO2 + HO2 ----> HO2 +    0.1400\*SVAVB1  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| AE57   | PAHRO2 + NO ----> NO +    0.0280\*SVAVB2 +    0.2250\*SVAVB3 +    0.1910\*SVAVB4  |   R2NO |   9.1214E-12<sup>7</sup>| 
| AE58   | PAHRO2 + HO2 ----> HO2 +    0.4730\*SVAVB1  |   R2H2 |   1.4900E-11<sup>7</sup>| 
| HET_NO2   | NO2 ---->   0.5000\*HONO +    0.5000\*HNO3  | HETERO_NO2 | Not Available<sup>2</sup> | 
| HET_N2O5IJ   | N2O5 ----> HNO3 + H2NO3PIJ  | HETERO_N2O5IJ | Not Available<sup>2</sup> | 
| HET_N2O5K   | N2O5 ----> HNO3 + H2NO3PK  | HETERO_N2O5K | Not Available<sup>2</sup> | 
| HET_H2NO3PIJA   | H2NO3PIJ ----> HNO3  | HETERO_H2NO3PAIJ | Not Available<sup>2</sup> | 
| HET_H2NO3PKA   | H2NO3PK ----> HNO3  | HETERO_H2NO3PAK | Not Available<sup>2</sup> | 
| HAL_Ozone   | O3 ----> | SEAWATER*min( 6.701E-11e<sup> 1.074E+01P</sup>+ 3.415E-08e<sup>-6.713E-01P</sup>, <br> 2.000E-06) |   2.0000E-06<sup>4</sup>| 
| HET_NO3   | NO3 ----> HNO3  | HETERO_NO3 | Not Available<sup>2</sup> | 
| OLIG_ISOPRENE1   | AISO1J ---->   0.5000\*AOLGBJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_ISOPRENE2   | AISO2J ---->   0.5000\*AOLGBJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_SESQT1   | ASQTJ ---->   1.5000\*AOLGBJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_AROMATIC1   | AAVB2J ---->   0.9070\*AOLGAJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_AROMATIC2   | AAVB3J ---->   0.9250\*AOLGAJ  |   9.4882E-06 |   9.4882E-06 |
| OLIG_AROMATIC3   | AAVB4J ---->   0.9430\*AOLGAJ  |   9.4882E-06 |   9.4882E-06 |
| RPOAGEPI   | APOCI + OH ---->   1.2500\*APNCOMI + APOCI + OH  |   2.5000E-12 |   2.5000E-12 |
| RPOAGELI   | APNCOMI + OH ----> OH  | HETERO_PNCOMLI | Not Available<sup>2</sup> | 
| RPOAGEPJ   | APOCJ + OH ---->   1.2500\*APNCOMJ + APOCJ + OH  |   2.5000E-12 |   2.5000E-12 |
| RPOAGELJ   | APNCOMJ + OH ----> OH  | HETERO_PNCOMLJ | Not Available<sup>2</sup> | 
| PCSOA   | PCVOC + OH ----> OH + PCSOARXN  |   1.2500E-11 |   1.2500E-11 |
| POA_AGE1   | VLVPO1 + OH ----> OH +    0.4857\*VLVPO1 +    0.0062\*VSVPO1 +    0.0025\*VSVPO2 +    0.0026\*VSVPO3 +    0.0023\*VIVPO1 +    0.2944\*VLVOO1 +    0.2021\*VLVOO2 +    0.0019\*VSVOO2 +    0.0023\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE2   | VSVPO1 + OH ----> OH +    0.3003\*VLVPO1 +    0.2862\*VSVPO1 +    0.0041\*VSVPO2 +    0.0035\*VSVPO3 +    0.2239\*VLVOO1 +    0.1820\*VLVOO2  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE3   | VSVPO2 + OH ----> OH +    0.3856\*VLVPO1 +    0.0950\*VSVPO1 +    0.1373\*VSVPO2 +    0.0005\*VSVPO3 +    0.2051\*VLVOO1 +    0.1764\*VLVOO2  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE4   | VSVPO3 + OH ----> OH +    0.2181\*VLVPO1 +    0.3063\*VSVPO1 +    0.0153\*VSVPO2 +    0.1043\*VSVPO3 +    0.1893\*VLVOO1 +    0.1668\*VLVOO2  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE5   | VIVPO1 + OH ----> OH +    0.2412\*VLVPO1 +    0.2089\*VSVPO1 +    0.3000\*VSVPO2 +    0.2028\*VLVOO1 +    0.0471\*VLVOO2  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE6   | VLVOO1 + OH ----> OH +    0.6664\*VLVOO1 +    0.0143\*VLVOO2 +    0.0123\*VSVOO1 +    0.1239\*VSVOO2 +    0.1831\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE7   | VLVOO2 + OH ----> OH +    0.2858\*VLVOO1 +    0.3931\*VLVOO2 +    0.0139\*VSVOO1 +    0.1027\*VSVOO2 +    0.2045\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE8   | VSVOO1 + OH ----> OH +    0.3303\*VLVOO1 +    0.2272\*VLVOO2 +    0.2607\*VSVOO1 +    0.0702\*VSVOO2 +    0.1116\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE9   | VSVOO2 + OH ----> OH +    0.3444\*VLVOO1 +    0.2749\*VLVOO2 +    0.0491\*VSVOO1 +    0.2577\*VSVOO2 +    0.0739\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE10   | VSVOO3 + OH ----> OH +    0.3886\*VLVOO1 +    0.2421\*VLVOO2 +    0.0640\*VSVOO1 +    0.0385\*VSVOO2 +    0.2667\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| HET_GLY   | GLY ----> AGLYJ  | HETERO_GLY | Not Available<sup>2</sup> | 
| HET_MGLY   | MGLY ----> AGLYJ  | HETERO_MGLY | Not Available<sup>2</sup> | 
| TR01   | HCHO_PRIMARY ----> | HCHOR_13 | Not Available<sup>1</sup> | 
| TR02   | HCHO_PRIMARY ----> | HCHOM_13 | Not Available<sup>1</sup> | 
| TR03   | HCHO_PRIMARY + OH ----> OH  |   5.50E-12e<sup>   125.00/T</sup> |   8.3645E-12 |
| TR05   | HCHO_PRIMARY + NO3 ----> NO3  |   5.8000E-16 |   5.8000E-16 |
| TR07   | CCHO_PRIMARY + OH ----> OH  |   2.40E-12e<sup>   546.00/T</sup>(T/300)<sup>  0.77 </sup> |   1.4910E-11 |
| TR08   | CCHO_PRIMARY ----> | CCHOR_13 | Not Available<sup>1</sup> | 
| TR09   | CCHO_PRIMARY + NO3 ----> NO3  |   1.40E-12e<sup> -1860.00/T</sup> |   2.7340E-15 |
| TR11   | ACRO_PRIMARY + OH ----> OH  |   7.10E-12e<sup>   333.00/T</sup> |   2.1693E-11 |
| TR12   | ACRO_PRIMARY + O3 ----> O3  |   2.8000E-19 |   2.8000E-19 |
| TR13   | ACRO_PRIMARY + NO3 ----> NO3  |   1.1000E-15 |   1.1000E-15 |
| TR15   | ACRO_PRIMARY ----> | ACROL_16 | Not Available<sup>1</sup> | 

<sup>0</sup>Units molecules/(sec*cm<sup>3</sup>); Value at 298.15 K;   2.4615E+19 molcules/cm<sup>3</sup>;   1.00 Atm.     
<sup>1</sup>Photolysis Reaction;depends on radiation and predicted concentrations     
<sup>2</sup>Heteorogeneous Reaction;Depends predicted concentrations                
<sup>4</sup>Set to zero if sun is below the horizon. SEAWATER equals surface fraction covered by ice free open ocean plus surf zones. P equals air pressure in atmospheres.         
<sup>7</sup>Rate constant multiple of constant for listed reaction   
