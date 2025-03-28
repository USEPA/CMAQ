Information is based on the mech.def file.
* Fall-off or pressure dependent reaction rate constants (M equals air number density):
 * For rate constants with k<sub>o</sub>, k<sub>i</sub>, n, F values: k = [ k<sub>o</sub>M/(1+k<sub>o</sub>M/k<sub>i</sub>)]F<sup>G</sup>, where G=(1+(log<sub>10</sub>(k<sub>o</sub>M/k<sub>i</sub>)/n)<sup>2</sup>))<sup>-1</sup> 
 * For rate constants with k<sub>1</sub>, k<sub>2</sub>: k = k<sub>1</sub> + k<sub>2</sub>M
 * For rate constants with k<sub>0</sub>, k<sub>2</sub>, k<sub>3</sub>: k = k<sub>0</sub> + k<sub>3</sub>M/(1+k<sub>3</sub>M/k<sub>2</sub>)
 * For rate constants with k<sub>1</sub>, k<sub>2</sub>, k<sub>3</sub>: k = k<sub>1</sub> + k<sub>2</sub>M + k<sub>3</sub> 

* For rate constants with the form A<_Reference_>, k equals A times a reference that represents photolysis rate, a heteorogeneous rate constant, rate constant for the given reaction or an operator. A equals one if not given.

* In the mechanism definition file, the rate is formatted as
 * A~<_HETEROGENEOUS_>
 * A*K<_REACTION_>
 * A/<_PHOTOLYSIS_>
 * A?<_OPERATOR_>

|Label|Reaction            |Rate Constant Formula| Value<br> molecules/(sec*cm<sup>3</sup>)|   
|:---|:-------------------|:--------------------|:----:|   
| R001   | O3 ----> O3P  | O3O3P_NASA06 | Not Available<sup>1</sup> | 
| R002   | O3 ----> O1D  | O3O1D_NASA06 | Not Available<sup>1</sup> | 
| R003   | H2O2 ---->   2.000\*HO  | H2O2_RACM2 | Not Available<sup>1</sup> | 
| R004   | NO2 ----> O3P + NO  | NO2_RACM2 | Not Available<sup>1</sup> | 
| R005   | NO3 ----> NO  | NO3NO_RACM2 | Not Available<sup>1</sup> | 
| R006   | NO3 ----> O3P + NO2  | NO3NO2_RACM2 | Not Available<sup>1</sup> | 
| R007   | HONO ----> HO + NO  | HONO_RACM2 | Not Available<sup>1</sup> | 
| R008   | HNO3 ----> HO + NO2  | HNO3_RACM2 | Not Available<sup>1</sup> | 
| R009   | HNO4 ---->   0.200\*HO +    0.800\*HO2 +    0.800\*NO2 +    0.200\*NO3  | HNO4_RACM2 | Not Available<sup>1</sup> | 
| R010   | HCHO ----> CO  | HCHO_MOL_JPL19 | Not Available<sup>1</sup> | 
| R011   | HCHO ---->   2.000\*HO2 + CO  | HCHO_RAD_JPL19 | Not Available<sup>1</sup> | 
| R012   | ACD ----> HO2 + MO2 + CO  | CH3CHO_RACM2 | Not Available<sup>1</sup> | 
| R013   | ALD ----> HO2 + ETHP + CO  | ALD_JPL19 | Not Available<sup>1</sup> | 
| R014   | ACT ----> MO2 + ACO3  | CH3COCH3A_JPL19 | Not Available<sup>1</sup> | 
| R014a   | ACT ---->   2.000\*MO2 + CO  | CH3COCH3B_JPL19 | Not Available<sup>1</sup> | 
| R015   | UALD ---->   1.220\*HO2 +    0.784\*ACO3 +    1.220\*CO +    0.350\*HCHO +    0.434\*ALD +    0.216\*KET  | UALD_RACM2 | Not Available<sup>1</sup> | 
| TRP01   | PINAL ----> HO2 + HC10P + CO  | ALD_JPL19 | Not Available<sup>1</sup> | 
| TRP02   | LIMAL ----> HO2 + HC10P + CO  | ALD_JPL19 | Not Available<sup>1</sup> | 
| R016   | MEK ---->   0.100\*MO2 + ETHP +    0.900\*ACO3 +    0.100\*CO  | MEK_JGR19 | Not Available<sup>1</sup> | 
| R017   | KET ---->   1.500\*ETHP +    0.500\*ACO3 +    0.500\*CO  | KET_JGR19 | Not Available<sup>1</sup> | 
| R018   | HKET ----> HO2 + ACO3 + HCHO  | HKET_RACM2 | Not Available<sup>1</sup> | 
| R019   | MACR ---->   0.340\*HO +    0.660\*HO2 +    0.670\*ACO3 +    0.330\*MACP +    0.340\*XO2 +    0.670\*CO +    0.670\*HCHO  | MACR_RACM2 | Not Available<sup>1</sup> | 
| R020   | MVK ---->   0.300\*MO2 +    0.300\*MACP +    0.700\*CO +    0.700\*UALD  | MVK_RACM2 | Not Available<sup>1</sup> | 
| R021   | GLY ---->   2.000\*CO  | GLYH2_RACM2 | Not Available<sup>1</sup> | 
| R022   | GLY ----> HCHO + CO  | GLYF_RACM2 | Not Available<sup>1</sup> | 
| R023   | GLY ---->   2.000\*HO2 +    2.000\*CO  | GLYHX_RACM2 | Not Available<sup>1</sup> | 
| R024   | MGLY ----> HO2 + ACO3 + CO  | MGLY_RACM2 | Not Available<sup>1</sup> | 
| R025   | DCB1 ---->   1.500\*HO2 +    0.250\*ACO3 +    0.200\*XO2 + CO +    0.500\*GLY +    0.500\*MGLY  | MGLY_RACM2 | Not Available<sup>1</sup> | 
| R026   | DCB2 ---->   1.500\*HO2 +    0.250\*ACO3 +    0.200\*XO2 + CO +    0.500\*GLY +    0.500\*MGLY  | MGLY_RACM2 | Not Available<sup>1</sup> | 
| R027   | BALD ----> CHO + HO2 + CO  | BALD_RACM2 | Not Available<sup>1</sup> | 
| R028   | OP1 ----> HO + HO2 + HCHO  | OP1_RACM2 | Not Available<sup>1</sup> | 
| R029   | OP2 ----> HO + HO2 + ALD  | OP1_RACM2 | Not Available<sup>1</sup> | 
| TRP03   | OPB ----> HO + HO2 + ALD  | OP1_RACM2 | Not Available<sup>1</sup> | 
| R029a   | OP3 ----> HO + HO2 + ALD  | OP1_RACM2 | Not Available<sup>1</sup> | 
| R030   | PAA ----> HO + MO2  | PAA_RACM2 | Not Available<sup>1</sup> | 
| R031   | ONIT ----> HO2 + NO2 +    0.200\*ALD +    0.800\*KET  | ONIT_RACM2 | Not Available<sup>1</sup> | 
| R032   | PAN ----> ACO3 + NO2  | PAN1_RACM2 | Not Available<sup>1</sup> | 
| R033   | PAN ----> MO2 + NO3  | PAN2_RACM2 | Not Available<sup>1</sup> | 
| R034   | O3 + HO ----> HO2  |   1.70E-12e<sup>  -940.00/T</sup> |   7.2647E-14 |
| R035   | O3 + HO2 ----> HO  |   1.00E-14e<sup>  -490.00/T</sup> |   1.9331E-15 |
| R036   | O3 + NO ----> NO2  |   3.00E-12e<sup> -1500.00/T</sup> |   1.9596E-14 |
| R037   | O3 + NO2 ----> NO3  |   1.20E-13e<sup> -2450.00/T</sup> |   3.2392E-17 |
| R038   | O3P + O2 + M ----> O3  |   6.10E-34(T/300)<sup> -2.40</sup> |   6.1912E-34 |
| R039   | O3P + O3 ----> |   8.00E-12e<sup> -2060.00/T</sup> |   7.9879E-15 |
| R040   | O1D + O2 ----> O3P  |   3.30E-11e<sup>    55.00/T</sup> |   3.9685E-11 |
| R041   | O1D + N2 ----> O3P  |   2.15E-11e<sup>   110.00/T</sup> |   3.1093E-11 |
| R042   | O1D + H2O ---->   2.000\*HO  |   1.63E-10e<sup>    60.00/T</sup> |   1.9934E-10 |
| R043   | HO + H2 ----> HO2  |   2.80E-12e<sup> -1800.00/T</sup> |   6.6869E-15 |
| R044   | HO + HO2 ----> |   4.80E-11e<sup>   250.00/T</sup> |   1.1102E-10 |
| R045   | HO2 + HO2 ----> H2O2  | k<sub>0</sub>=  3.00E-13e<sup>   460.0/T</sup><br>k<sub>1</sub>=  2.10E-33e<sup>   920.0/T</sup> |   2.5345E-12 |
| R046   | HO2 + HO2 + H2O ----> H2O2  | k<sub>0</sub>=  4.20E-34e<sup>  2660.0/T</sup><br>k<sub>1</sub>=  2.94E-54e<sup>  3120.0/T</sup> |   5.6834E-30 |
| R047   | H2O2 + HO ----> HO2  |   1.80E-12e<sup>     0.00/T</sup> |   1.8000E-12 |
| R048   | NO + O3P ----> NO2  | k<sub>o</sub>=  9.10E-32e<sup>     0.0/T</sup>(T/300)<sup> -1.50</sup><br>k<sub>i</sub> =   3.00E-11e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.00;F=     0.60 |   1.6772E-12 |
| R049   | NO + HO ----> HONO  | k<sub>o</sub>=  7.10E-31e<sup>     0.0/T</sup>(T/300)<sup> -2.60</sup><br>k<sub>i</sub> =   3.60E-11e<sup>     0.0/T</sup>(T/300)<sup> -0.10</sup><br>n=     1.00;F=     0.60 |   7.4585E-12 |
| R050   | NO + HO2 ----> NO2 + HO  |   3.44E-12e<sup>   260.00/T</sup> |   8.2278E-12 |
| R051   | NO + HO2 ----> HNO3  | k<sub>0</sub>=  6.0950E-14e<sup>   270.0/T</sup>(T/300)<sup> -1.00</sup><br>k<sub>2</sub>=  6.8570E-34e<sup>   270.0/T</sup>(T/300)<sup>  1.00</sup><br>k<sub>3</sub>= -5.9680E-14e<sup>   270.00/T</sup> |   4.5566E-14 |
| R052   | NO + NO + O2 ---->   2.000\*NO2  |   4.25E-39e<sup>   663.50/T</sup> |   3.9343E-38 |
| R053   | HONO + HO ----> NO2  |   3.00E-12e<sup>   250.00/T</sup> |   6.9387E-12 |
| R054   | NO2 + O3P ----> NO  |   5.30E-12e<sup>   200.00/T</sup> |   1.0366E-11 |
| R055   | NO2 + O3P ----> NO3  | k<sub>o</sub>=  3.40E-31e<sup>     0.0/T</sup>(T/300)<sup> -1.60</sup><br>k<sub>i</sub> =   2.30E-11e<sup>     0.0/T</sup>(T/300)<sup> -0.20</sup><br>n=     1.00;F=     0.60 |   4.0243E-12 |
| R056   | NO2 + HO ----> HNO3  | k<sub>o</sub>=  1.80E-30e<sup>     0.0/T</sup>(T/300)<sup> -3.00</sup><br>k<sub>i</sub> =   2.80E-11e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.00;F=     0.60 |   1.0589E-11 |
| R057   | HNO3 + HO ----> NO3  | k<sub>0</sub>=  2.40E-14e<sup>   460.0/T</sup><br>k<sub>1</sub>=  2.70E-17e<sup>  2199.0/T</sup><br>k<sub>3</sub>=  6.50E-34e<sup>  1335.0/T</sup> |   1.5409E-13 |
| R058   | NO3 + HO ----> HO2 + NO2  |   2.0000E-11 |   2.0000E-11 |
| R059   | NO3 + HO2 ---->   0.700\*HO +    0.700\*NO2 +    0.300\*HNO3  |   3.5000E-12 |   3.5000E-12 |
| R060   | NO3 + NO ---->   2.000\*NO2  |   1.70E-11e<sup>   125.00/T</sup> |   2.5854E-11 |
| R061   | NO3 + NO2 ----> NO + NO2  |   4.35E-14e<sup> -1335.00/T</sup> |   4.9418E-16 |
| R062   | NO3 + NO3 ---->   2.000\*NO2  |   8.50E-13e<sup> -2450.00/T</sup> |   2.2944E-16 |
| R063   | NO3 + NO2 ----> N2O5  | k<sub>o</sub>=  2.40E-30e<sup>     0.0/T</sup>(T/300)<sup> -3.00</sup><br>k<sub>i</sub> =   1.60E-12e<sup>     0.0/T</sup>(T/300)<sup>  0.10</sup><br>n=     1.00;F=     0.60 |   1.3451E-12 |
| R064   | N2O5 ----> NO2 + NO3  |   1.72E+26e<sup>-10840.00/T</sup> \*R063 |   3.7623E-02<sup>8</sup>| 
| R065   | N2O5 + H2O ---->   2.000\*HNO3  |   1.0000E-22 |   1.0000E-22 |
| R066   | NO2 + HO2 ----> HNO4  | k<sub>o</sub>=  1.90E-31e<sup>     0.0/T</sup>(T/300)<sup> -3.40</sup><br>k<sub>i</sub> =   4.00E-12e<sup>     0.0/T</sup>(T/300)<sup> -0.30</sup><br>n=     1.00;F=     0.60 |   1.3113E-12 |
| R067   | HNO4 ----> HO2 + NO2  |   4.76E+26e<sup>-10900.00/T</sup> \*R066 |   8.2835E-02<sup>8</sup>| 
| R068   | HNO4 + HO ----> NO2  |   4.50E-13e<sup>   610.00/T</sup> |   3.4814E-12 |
| R069   | SO2 + HO ----> HO2 + SULF + SULRXN  | k<sub>o</sub>=  2.90E-31e<sup>     0.0/T</sup>(T/300)<sup> -4.10</sup><br>k<sub>i</sub> =   1.70E-12e<sup>     0.0/T</sup>(T/300)<sup>  0.20</sup><br>n=     1.00;F=     0.60 |   9.5762E-13 |
| R070   | CO + HO ----> HO2  | k<sub>0</sub>=  1.44E-13e<sup>     0.0/T</sup><br>k<sub>1</sub>=  2.74E-33e<sup>     0.0/T</sup> |   2.1145E-13 |
| R071   | HO + CH4 ----> MO2  |   2.45E-12e<sup> -1775.00/T</sup> |   6.3628E-15 |
| R072   | ETH + HO ----> ETHP  |   7.66E-12e<sup> -1020.00/T</sup> |   2.5030E-13 |
| R073   | HC3 + HO ----> HC3P +    0.000\*ASOATJ  |   7.68E-12e<sup>  -370.00/T</sup> |   2.2203E-12 |
| R074   | HC5 + HO ----> HC5P +    0.001\*ASOATJ  |   1.01E-11e<sup>  -245.00/T</sup> |   4.4407E-12 |
| R076   | ETE + HO ----> ETEP  | k<sub>o</sub>=  1.00E-28e<sup>     0.0/T</sup>(T/300)<sup> -4.50</sup><br>k<sub>i</sub> =   8.80E-12e<sup>     0.0/T</sup>(T/300)<sup> -0.85</sup><br>n=     1.00;F=     0.60 |   8.1981E-12 |
| R077   | OLT + HO ----> OLTP  |   5.72E-12e<sup>   500.00/T</sup> |   3.0599E-11 |
| R078   | OLI + HO ----> OLIP  |   1.33E-11e<sup>   500.00/T</sup> |   7.1149E-11 |
| R080   | ACE + HO ---->   0.650\*HO +    0.350\*HO2 +    0.350\*CO +    0.650\*GLY +    0.350\*ORA1  | k<sub>o</sub>=  5.50E-30e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>k<sub>i</sub> =   8.30E-13e<sup>     0.0/T</sup>(T/300)<sup>  2.00</sup><br>n=     1.00;F=     0.60 |   7.4748E-13 |
| ROCARO31   | BEN + HO ---->   0.470\*BENP +    0.530\*PHEN +    0.530\*HO2  |   2.33E-12e<sup>  -193.00/T</sup> |   1.2196E-12 |
| ROCARO41   | TOL + HO ---->   0.820\*TOLP +    0.180\*CSL +    0.180\*HO2  |   1.81E-12e<sup>   354.00/T</sup> |   5.9337E-12 |
| ROCARO51   | XYM + HO ---->   0.830\*XYMP +    0.170\*CSL +    0.170\*HO2  |   2.3300E-11 |   2.3300E-11 |
| ROCARO61   | XYE + HO ---->   0.820\*XYEP +    0.180\*CSL +    0.180\*HO2  |   7.1600E-12 |   7.1600E-12 |
| RAM01   | ISO + O3 ---->   0.531\*MACR +    0.189\*MVK +    1.160\*HCHO +    0.250\*HO +    0.250\*HO2 +    0.080\*MO2 +    0.100\*ACO3 +    0.090\*H2O2 +    0.100\*MACP +    0.140\*CO +    0.280\*ORA1 +    0.150\*OLT  |   1.58E-14e<sup> -2000.00/T</sup> |   1.9293E-17 |
| RAM02   | ISO + NO3 ----> INO2 +    0.300\*HCHO +    0.300\*NO2 +    0.300\*NALD  |   2.95E-12e<sup>  -450.00/T</sup> |   6.5214E-13 |
| RAM03   | ISO + HO ----> ISOP +    0.020\*MO2  |   2.69E-11e<sup>   390.00/T</sup> |   9.9503E-11 |
| RAM04   | ISOP + HO2 ----> ISHP +    0.600\*HO2 +    0.150\*HCHO  |   4.50E-13e<sup>  1300.00/T</sup> |   3.5224E-11 |
| RAM05   | ISOP + NO ---->   0.140\*ISON +    0.700\*HCHO +    0.440\*MVK +    0.880\*HO2 +    0.780\*NO2 +    0.280\*MACR +    0.021\*GLY  |   2.70E-12e<sup>   350.00/T</sup> |   8.7334E-12 |
| RAM06   | ISHP + HO ----> ISOP  |   4.60E-12e<sup>   200.00/T</sup> |   8.9967E-12 |
| RAM07   | INO2 + HO2 ----> IPN + HO  |   3.14E-14e<sup>   580.00/T</sup> |   2.1967E-13 |
| RAM08   | INO2 + NO ---->   0.900\*HCHO +    0.500\*MGLY +    0.800\*MVK +    0.500\*NO2 + HO2 +    0.200\*NALD +    0.100\*MO2  |   9.42E-16e<sup>   580.00/T</sup> |   6.5902E-15 |
| RAM9   | IPN + HO2 ---->   0.800\*NO2 + HO2 +    0.400\*HCHO +    0.050\*GLY +    0.100\*MGLY +    0.400\*MACR +    0.940\*MVK +    0.200\*NALD +    0.100\*MO2  |   3.40E-11e<sup>   390.00/T</sup> |   1.2577E-10 |
| RAM10   | ISON + HO ----> NALD + HO +    0.200\*IEPOX  |   2.40E-07e<sup>   580.00/T</sup> |   1.6790E-06 |
| RAM11   | ISHP + HO ---->   0.150\*HCHO +    0.050\*MGLY +    0.150\*MACR +    0.020\*GLY +    0.200\*MVK +    0.400\*NO2 +    0.050\*IPC +    0.580\*IEPOX +    0.800\*HO  |   2.97E-11e<sup>   390.00/T</sup> |   1.0986E-10 |
| RAM12   | ISHP ---->   0.400\*HCHO +    0.100\*MGLY +    0.060\*ACO3  | HCHO_RAD_RACM2 | Not Available<sup>1</sup> | 
| RAM13   | IPC + NO ---->   0.350\*NO2 +    0.800\*NO  |   1.0000E-10 |   1.0000E-10 |
| RAM14   | NALD + HO ----> CO +    0.120\*NO2  |   5.0000E-11 |   5.0000E-11 |
| RAM15   | NALD + NO3 ----> CO  |   2.0000E-14 |   2.0000E-14 |
| RAM16   | ISON ----> HNO3 + ROH  |   2.3000E-05 |   2.3000E-05 |
| R087   | API + HO ---->   0.975\*APIP1 +    0.025\*APIP2  |   1.21E-11e<sup>   440.00/T</sup> |   5.2930E-11 |
| R088   | LIM + HO ---->   0.945\*LIMP1 +    0.055\*LIMP2  |   4.20E-11e<sup>   401.00/T</sup> |   1.6120E-10 |
| TRP04   | PINAL + HO ---->   0.230\*PINALP +    0.770\*RCO3  |   5.20E-12e<sup>   600.00/T</sup> |   3.8903E-11 |
| TRP05   | LIMAL + HO ---->   0.700\*LIMALP +    0.300\*RCO3  |   1.0000E-10 |   1.0000E-10 |
| R089   | HCHO + HO ----> HO2 + CO  |   5.50E-12e<sup>   125.00/T</sup> |   8.3645E-12 |
| R090   | ACD + HO ----> ACO3  |   4.70E-12e<sup>   345.00/T</sup> |   1.4950E-11 |
| R091   | ALD + HO ----> RCO3  |   4.90E-12e<sup>   405.00/T</sup> |   1.9060E-11 |
| R092   | ACT + HO ----> ACTP  |   4.56E-14e<sup>  -427.00/T</sup>(T/300)<sup>  3.65 </sup> |   1.0646E-14 |
| R093   | MEK + HO ----> MEKP  |   1.50E-12e<sup>   -90.00/T</sup> |   1.1092E-12 |
| R094   | KET + HO ----> KETP  |   2.80E-12e<sup>    10.00/T</sup> |   2.8955E-12 |
| R095   | HKET + HO ----> HO2 + MGLY  |   3.0000E-12 |   3.0000E-12 |
| R096   | MACR + HO ---->   0.570\*MACP +    0.430\*MCP  |   8.00E-12e<sup>   380.00/T</sup> |   2.8616E-11 |
| R097   | MVK + HO ----> MVKP  |   2.60E-12e<sup>   610.00/T</sup> |   2.0115E-11 |
| R098   | UALD + HO ---->   0.313\*ACO3 +    0.687\*UALP  |   5.77E-12e<sup>   533.00/T</sup> |   3.4479E-11 |
| R099   | GLY + HO ----> HO2 +    2.000\*CO  |   1.1000E-11 |   1.1000E-11 |
| R100   | MGLY + HO ----> ACO3 + CO  |   9.26E-13e<sup>   830.00/T</sup> |   1.4984E-11 |
| R101   | DCB1 + HO ---->   0.520\*HO2 +    0.330\*CO +    0.400\*ALD +    0.780\*KET +    0.100\*GLY +    0.010\*MGLY  |   2.80E-11e<sup>   175.00/T</sup> |   5.0358E-11 |
| R102   | DCB2 + HO ---->   0.520\*HO2 +    0.330\*CO +    0.130\*MEK +    0.100\*GLY +    0.010\*MGLY +    0.780\*OP2  |   2.80E-11e<sup>   175.00/T</sup> |   5.0358E-11 |
| R103   | DCB3 + HO ---->   0.560\*HO2 +    0.210\*MACP +    0.110\*CO +    0.270\*GLY +    0.010\*MGLY +    0.790\*OP2  |   1.0000E-11 |   1.0000E-11 |
| R104   | BALD + HO ----> BALP  |   5.32E-12e<sup>   243.00/T</sup> |   1.2019E-11 |
| R105   | PHEN + HO ---->   0.152\*ASOATJ +    0.619\*HO2 +    0.170\*ADDC +    0.059\*CHO +    0.619\*MCT  |   6.75E-12e<sup>   405.00/T</sup> |   2.6257E-11 |
| R106   | CSL + HO ---->   0.200\*ASOATJ +    0.584\*HO2 +    0.160\*ADDC +    0.056\*CHO +    0.584\*MCT  |   4.65E-11e<sup>     0.00/T</sup> |   4.6500E-11 |
| R108   | MCT + HO ----> MCTO  |   2.05E-10e<sup>     0.00/T</sup> |   2.0500E-10 |
| R109   | MOH + HO ----> HO2 + HCHO  |   2.85E-12e<sup>  -345.00/T</sup> |   8.9600E-13 |
| R110   | EOH + HO ----> HO2 + ACD  |   3.00E-12e<sup>    20.00/T</sup> |   3.2081E-12 |
| R111   | ROH + HO ----> HO2 +    0.719\*ALD +    0.184\*ACD  |   2.60E-12e<sup>   200.00/T</sup> |   5.0851E-12 |
| R112   | ETEG + HO ----> HO2 + ALD  |   1.4700E-11 |   1.4700E-11 |
| R113   | OP1 + HO ---->   0.350\*HO +    0.650\*MO2 +    0.350\*HCHO  |   2.90E-12e<sup>   190.00/T</sup> |   5.4848E-12 |
| R114   | OP2 + HO ---->   0.010\*HO +    0.440\*HC3P +    0.070\*XO2 +    0.080\*ALD +    0.410\*KET  |   3.40E-12e<sup>   190.00/T</sup> |   6.4304E-12 |
| TRP06   | OPB + HO ---->   0.010\*HO +    0.440\*HC10P +    0.070\*XO2 +    0.080\*ALD +    0.410\*KET  |   3.40E-12e<sup>   190.00/T</sup> |   6.4304E-12 |
| R114a   | OP3 + HO ---->   0.010\*HO +    0.440\*HC10P +    0.070\*XO2 +    0.080\*ALD +    0.410\*KET  |   3.40E-12e<sup>   190.00/T</sup> |   6.4304E-12 |
| R116   | MAHP + HO ----> MACP  |   3.0000E-11 |   3.0000E-11 |
| R117   | ORA1 + HO ----> HO2  |   4.5000E-13 |   4.5000E-13 |
| R118   | ORA2 + HO ---->   0.640\*MO2 +    0.360\*ORAP  |   4.00E-14e<sup>   850.00/T</sup> |   6.9214E-13 |
| R119   | PAA + HO ---->   0.350\*HO +    0.650\*ACO3 +    0.350\*XO2 +    0.350\*HCHO  |   2.93E-12e<sup>   190.00/T</sup> |   5.5415E-12 |
| R120   | PAN + HO ----> XO2 + NO3 + HCHO  |   4.0000E-14 |   4.0000E-14 |
| R121   | PPN + HO ----> XO2 + NO3 + HCHO  |   4.0000E-14 |   4.0000E-14 |
| R122   | MPAN + HO ----> NO2 + HKET  |   3.2000E-11 |   3.2000E-11 |
| R123   | ONIT + HO ----> HC3P + NO2  |   5.31E-12e<sup>  -260.00/T</sup> |   2.2201E-12 |
| TRP07   | TRPN + HO ----> HOM  |   4.8000E-12 |   4.8000E-12 |
| R126   | ETE + O3 ---->   0.080\*HO +    0.150\*HO2 +    0.430\*CO + HCHO +    0.370\*ORA1  |   9.14E-15e<sup> -2580.00/T</sup> |   1.5953E-18 |
| R127   | OLT + O3 ---->   0.220\*HO +    0.320\*HO2 +    0.080\*MO2 +    0.060\*ETHP +    0.040\*HC3P +    0.020\*HC5P +    0.068\*H2O2 +    0.430\*CO +    0.020\*ETH +    0.015\*HC3 +    0.006\*HC5 +    0.032\*BEN +    0.560\*HCHO +    0.010\*ACD +    0.440\*ALD +    0.030\*ACT +    0.020\*BALD +    0.060\*MEK +    0.010\*HKET +    0.030\*ORA1 +    0.060\*ORA2  |   4.33E-15e<sup> -1800.00/T</sup> |   1.0341E-17 |
| R128   | OLI + O3 ---->   0.460\*HO +    0.070\*HO2 +    0.320\*MO2 +    0.070\*ETHP +    0.040\*HC3P +    0.090\*ACO3 +    0.370\*CO +    0.026\*H2O2 +    0.010\*ETH +    0.010\*HC3 +    0.090\*HCHO +    0.457\*ACD +    0.730\*ALD +    0.110\*ACT +    0.017\*KET +    0.044\*HKET +    0.017\*ORA2  |   4.40E-15e<sup>  -845.00/T</sup> |   2.5858E-16 |
| R131   | API + O3 ---->   0.900\*HO +    0.900\*APIP1 +    0.050\*APIP2 +    0.050\*PINAL +    0.050\*H2O2 +    0.140\*CO  |   5.00E-16e<sup>  -530.00/T</sup> |   8.4519E-17 |
| R132   | LIM + O3 ---->   0.840\*HO +    0.840\*LIMP1 +    0.110\*LIMP2 +    0.050\*LIMAL +    0.050\*H2O2 +    0.140\*CO  |   2.95E-15e<sup>  -783.00/T</sup> |   2.1344E-16 |
| TRP08   | LIMAL + O3 ---->   0.040\*HO +    0.670\*HC10P +    0.790\*HCHO +    0.330\*KET +    0.040\*HO2 +    0.200\*CO  |   8.3000E-18 |   8.3000E-18 |
| TRP09   | TRPN + O3 ----> HOM  |   1.6700E-16 |   1.6700E-16 |
| R133   | MACR + O3 ---->   0.190\*HO +    0.140\*HO2 +    0.100\*ACO3 +    0.220\*CO +    0.500\*MGLY +    0.450\*ORA1  |   1.36E-15e<sup> -2112.00/T</sup> |   1.1406E-18 |
| R134   | MVK + O3 ---->   0.160\*HO +    0.110\*HO2 +    0.280\*ACO3 +    0.010\*XO2 +    0.560\*CO +    0.100\*HCHO +    0.540\*MGLY +    0.070\*ORA1 +    0.070\*ORA2 +    0.100\*ALD  |   8.50E-16e<sup> -1520.00/T</sup> |   5.1921E-18 |
| R135   | UALD + O3 ---->   0.100\*HO +    0.072\*HO2 +    0.008\*MO2 +    0.002\*ACO3 +    0.100\*XO2 +    0.243\*CO +    0.080\*HCHO +    0.420\*ACD +    0.028\*KET +    0.491\*GLY +    0.003\*MGLY +    0.044\*ORA1  |   1.6600E-18 |   1.6600E-18 |
| R136   | DCB1 + O3 ---->   0.050\*HO + HO2 +    0.600\*RCO3 +    0.600\*XO2 +    1.500\*CO +    0.050\*HCHO +    0.050\*GLY +    0.080\*MGLY +    0.650\*OP2  |   2.0000E-16 |   2.0000E-16 |
| R137   | DCB2 + O3 ---->   0.050\*HO + HO2 +    0.600\*RCO3 +    0.600\*XO2 +    1.500\*CO +    0.050\*HCHO +    0.050\*GLY +    0.080\*MGLY +    0.700\*DCB1 +    0.650\*OP2  |   2.0000E-16 |   2.0000E-16 |
| R138   | DCB3 + O3 ---->   0.050\*HO + HO2 +    1.500\*CO +    0.480\*GLY +    0.700\*DCB1 +    0.250\*ORA1 +    0.250\*ORA2 +    0.110\*PAA  |   9.0000E-17 |   9.0000E-17 |
| R140   | MCTO + O3 ----> MCTP  |   2.8600E-13 |   2.8600E-13 |
| R141   | ETE + NO3 ---->   0.800\*OLNN +    0.200\*OLND  |   4.39E-13e<sup> -2282.00/T</sup>(T/300)<sup>  2.00 </sup> |   2.0571E-16 |
| R142   | OLT + NO3 ---->   0.430\*OLNN +    0.570\*OLND  |   1.79E-13e<sup>  -450.00/T</sup> |   3.9570E-14 |
| R143   | OLI + NO3 ---->   0.110\*OLNN +    0.890\*OLND  |   8.64E-13e<sup>   450.00/T</sup> |   3.9084E-12 |
| R146   | API + NO3 ---->   0.975\*APINP1 +    0.025\*APINP2  |   1.19E-12e<sup>   490.00/T</sup> |   6.1560E-12 |
| R147   | LIM + NO3 ---->   0.945\*LIMNP1 +    0.055\*LIMNP2  |   1.2200E-11 |   1.2200E-11 |
| TRP10   | TRPN + NO3 ----> HOM  |   3.15E-14e<sup>  -448.00/T</sup> |   7.0104E-15 |
| R148   | HCHO + NO3 ----> HO2 + CO + HNO3  |   2.00E-12e<sup> -2440.00/T</sup> |   5.5828E-16 |
| R149   | ACD + NO3 ----> ACO3 + HNO3  |   1.40E-12e<sup> -1900.00/T</sup> |   2.3907E-15 |
| R150   | ALD + NO3 ----> RCO3 + HNO3  |   3.76E-12e<sup> -1900.00/T</sup> |   6.4208E-15 |
| R151   | MACR + NO3 ---->   0.680\*HCHO +    0.320\*MACP +    0.680\*XO2 +    0.680\*MGLY +    0.320\*HNO3 +    0.680\*NO2  |   3.4000E-15 |   3.4000E-15 |
| R152   | UALD + NO3 ----> HO2 + XO2 +    0.668\*CO +    0.332\*HCHO +    0.332\*ALD + ONIT  |   5.02E-13e<sup> -1076.00/T</sup> |   1.3595E-14 |
| R153   | GLY + NO3 ----> HO2 +    2.000\*CO + HNO3  |   2.90E-12e<sup> -1900.00/T</sup> |   4.9522E-15 |
| R154   | MGLY + NO3 ----> ACO3 + CO + HNO3  |   3.76E-12e<sup> -1900.00/T</sup> |   6.4208E-15 |
| R155   | PHEN + NO3 ---->   0.152\*ASOATJ +    0.339\*CHO +    0.850\*ADDC +    0.424\*ADCN +    0.424\*HNO3  |   3.7800E-12 |   3.7800E-12 |
| R156   | CSL + NO3 ---->   0.200\*ASOATJ +    0.320\*CHO +    0.080\*ADDC +    0.400\*ADCN +    0.400\*HNO3  |   1.0600E-12 |   1.0600E-12 |
| R158   | MCT + NO3 ----> MCTO + HNO3  |   2.0100E-10 |   2.0100E-10 |
| R159   | MPAN + NO3 ----> MACP + NO2  |   2.20E-14e<sup>  -500.00/T</sup> |   4.1125E-15 |
| TRP11   | PINALP ----> HOM  |   1.0000E+00 |   1.0000E+00 |
| TRP12   | LIMALP ----> HOM  |   1.0000E+00 |   1.0000E+00 |
| R166   | ACO3 + NO2 ----> PAN  | k<sub>o</sub>=  9.70E-29e<sup>     0.0/T</sup>(T/300)<sup> -5.60</sup><br>k<sub>i</sub> =   9.30E-12e<sup>     0.0/T</sup>(T/300)<sup> -1.50</sup><br>n=     1.00;F=     0.60 |   8.6800E-12 |
| R167   | PAN ----> ACO3 + NO2  |   1.11E+28e<sup>-14000.00/T</sup> \*R166 |   3.9034E-04<sup>8</sup>| 
| R168   | RCO3 + NO2 ----> PPN  | k<sub>o</sub>=  9.70E-29e<sup>     0.0/T</sup>(T/300)<sup> -5.60</sup><br>k<sub>i</sub> =   9.30E-12e<sup>     0.0/T</sup>(T/300)<sup> -1.50</sup><br>n=     1.00;F=     0.60 |   8.6800E-12 |
| R169   | PPN ----> RCO3 + NO2  |   1.11E+28e<sup>-14000.00/T</sup> \*R168 |   3.9034E-04<sup>8</sup>| 
| R170   | MACP + NO2 ----> MPAN  |   2.80E-12e<sup>   181.00/T</sup> |   5.1382E-12 |
| R171   | MPAN ----> MACP + NO2  |   1.60E+16e<sup>-13486.00/T</sup> |   3.6308E-04 |
| R172   | MO2 + NO ----> HO2 + NO2 + HCHO  |   2.80E-12e<sup>   300.00/T</sup> |   7.6586E-12 |
| R173   | ETHP + NO ----> HO2 + NO2 + ACD  |   2.60E-12e<sup>   365.00/T</sup> |   8.8439E-12 |
| R174   | HC3P + NO ---->   0.660\*HO2 +    0.131\*MO2 +    0.048\*ETHP +    0.089\*XO2 +    0.935\*NO2 +    0.504\*ACD +    0.132\*ALD +    0.165\*ACT +    0.042\*MEK +    0.065\*ONIT  |   4.0000E-12 |   4.0000E-12 |
| R175   | HC5P + NO ---->   0.200\*HO2 +    0.051\*MO2 +    0.231\*ETHP +    0.235\*XO2 +    0.864\*NO2 +    0.018\*HCHO +    0.045\*ACD +    0.203\*ALD +    0.033\*MEK +    0.217\*ACT +    0.033\*KET +    0.272\*HKET +    0.136\*ONIT  |   4.0000E-12 |   4.0000E-12 |
| R177   | ETEP + NO ----> HO2 + NO2 +    1.600\*HCHO +    0.200\*ALD  |   9.0000E-12 |   9.0000E-12 |
| R178   | OLTP + NO ---->   0.780\*HO2 +    0.970\*NO2 +    0.780\*HCHO +    0.012\*ACD +    0.440\*ALD +    0.060\*ACT +    0.130\*MEK +    0.030\*ONIT  |   4.0000E-12 |   4.0000E-12 |
| R179   | OLIP + NO ---->   0.830\*HO2 +    0.950\*NO2 +    0.810\*ACD +    0.680\*ALD +    0.200\*ACT +    0.090\*KET +    0.020\*HKET +    0.050\*ONIT  |   4.0000E-12 |   4.0000E-12 |
| ROCARO33   | BENP + NO ---->   0.000\*ONIT +    0.001\*VROCP4OXY2 +    0.001\*VROCN1OXY6 +    0.998\*NO2 +    0.998\*HO2 +    0.000\*BALD +    0.998\*GLY +    0.499\*FURANONE +    0.249\*DCB2 +    0.249\*DCB3  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCARO43   | TOLP + NO ---->   0.000\*ONIT +    0.001\*VROCP4OXY2 +    0.001\*VROCN1OXY6 +    0.998\*NO2 +    0.998\*HO2 +    0.085\*BALD +    0.548\*GLY +    0.365\*MGLY +    0.365\*FURANONE +    0.548\*DCB1  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCARO53   | XYMP + NO ---->   0.000\*ONIT +    0.001\*VROCP3OXY2 +    0.001\*VROCP0OXY4 +    0.998\*NO2 +    0.998\*HO2 +    0.048\*BALD +    0.703\*GLY +    0.247\*MGLY +    0.351\*FURANONE +    0.598\*DCB2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCARO63   | XYEP + NO ---->   0.000\*ONIT +    0.001\*VROCP3OXY2 +    0.001\*VROCP0OXY4 +    0.998\*NO2 +    0.998\*HO2 +    0.085\*BALD +    0.548\*GLY +    0.365\*MGLY +    0.456\*FURANONE +    0.456\*DCB2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R189   | APIP1 + NO ---->   0.820\*HO2 +    0.820\*NO2 +    0.820\*PINAL +    0.180\*TRPN  |   4.0000E-12 |   4.0000E-12 |
| TRP13   | APIP2 + NO ---->   0.820\*HO +    0.820\*NO2 + HOM  |   4.0000E-12 |   4.0000E-12 |
| TRP14   | APINP1 + NO ---->   2.000\*NO2 + PINAL  |   4.0000E-12 |   4.0000E-12 |
| TRP15   | APINP2 + NO ---->   0.820\*NO2 +    0.820\*HO + HOM  |   4.0000E-12 |   4.0000E-12 |
| R190   | LIMP1 + NO ---->   0.770\*HO2 +    0.770\*NO2 +    0.490\*LIMAL +    0.280\*HCHO +    0.280\*UALD +    0.230\*TRPN  |   4.0000E-12 |   4.0000E-12 |
| TRP16   | LIMP2 + NO ---->   0.770\*HO +    0.770\*NO2 + HOM  |   4.0000E-12 |   4.0000E-12 |
| TRP17   | LIMNP1 + NO ---->   2.000\*NO2 + LIMAL  |   4.0000E-12 |   4.0000E-12 |
| TRP18   | LIMNP2 + NO ---->   0.770\*NO2 +    0.770\*HO + HOM  |   4.0000E-12 |   4.0000E-12 |
| TRP19   | PINALP + NO ---->   0.950\*HO2 +    0.950\*NO2 +    0.050\*TRPN +    0.950\*HCHO +    0.950\*KET  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| TRP20   | LIMALP + NO ---->   0.940\*HO2 +    0.940\*NO2 +    0.060\*TRPN +    0.940\*HCHO +    0.940\*KET  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R191   | ACO3 + NO ----> MO2 + NO2  |   8.10E-12e<sup>   270.00/T</sup> |   2.0034E-11 |
| R192   | RCO3 + NO ----> ETHP + NO2  |   8.10E-12e<sup>   270.00/T</sup> |   2.0034E-11 |
| R193   | ACTP + NO ----> ACO3 + NO2 + HCHO  |   2.90E-12e<sup>   300.00/T</sup> |   7.9321E-12 |
| R194   | MEKP + NO ---->   0.670\*HO2 + NO2 +    0.330\*HCHO +    0.670\*DCB1  |   4.0000E-12 |   4.0000E-12 |
| R195   | KETP + NO ---->   0.770\*HO2 +    0.230\*ACO3 +    0.160\*XO2 + NO2 +    0.460\*ALD +    0.540\*MGLY  |   4.0000E-12 |   4.0000E-12 |
| R196   | MACP + NO ---->   0.650\*MO2 +    0.350\*ACO3 + NO2 +    0.650\*CO +    0.650\*HCHO  |   2.54E-12e<sup>   360.00/T</sup> |   8.4961E-12 |
| R197   | MCP + NO ----> NO2 +    0.500\*HO2 +    0.500\*HCHO + HKET  |   2.54E-12e<sup>   360.00/T</sup> |   8.4961E-12 |
| R198   | MVKP + NO ---->   0.300\*HO2 +    0.700\*ACO3 +    0.700\*XO2 + NO2 +    0.300\*HCHO +    0.700\*ALD +    0.300\*MGLY  |   2.54E-12e<sup>   360.00/T</sup> |   8.4961E-12 |
| R199   | UALP + NO ----> HO2 + NO2 +    0.610\*CO +    0.030\*HCHO +    0.270\*ALD +    0.180\*GLY +    0.700\*KET +    0.210\*MGLY  |   2.54E-12e<sup>   360.00/T</sup> |   8.4961E-12 |
| R200   | BALP + NO ----> BAL1 + NO2  |   4.0000E-12 |   4.0000E-12 |
| R201   | BAL1 + NO ----> BAL2 + NO2  |   4.0000E-12 |   4.0000E-12 |
| R202   | ADDC + NO ----> HO2 + NO2 +    0.320\*HKET +    0.680\*GLY +    0.680\*OP2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R203   | MCTP + NO ----> MCTO + NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R204   | ORAP + NO ----> NO2 + GLY + HO2  |   4.0000E-12 |   4.0000E-12 |
| R205   | OLNN + NO ----> NO2 + HO2 + ONIT  |   4.0000E-12 |   4.0000E-12 |
| R206   | OLND + NO ---->   2.000\*NO2 +    0.287\*HCHO +    1.240\*ALD +    0.464\*KET  |   4.0000E-12 |   4.0000E-12 |
| R207   | ADCN + NO ---->   2.000\*NO2 + GLY + OP2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R208   | XO2 + NO ----> NO2  |   4.0000E-12 |   4.0000E-12 |
| R209   | BAL2 + NO2 ----> ONIT  |   2.0000E-11 |   2.0000E-11 |
| R210   | CHO + NO2 ----> ONIT  |   2.0000E-11 |   2.0000E-11 |
| R211   | MCTO + NO2 ----> ONIT  |   2.0800E-12 |   2.0800E-12 |
| R212   | MO2 + HO2 ----> OP1  |   4.10E-13e<sup>   750.00/T</sup> |   5.0729E-12 |
| R213   | ETHP + HO2 ----> OP2  |   7.50E-13e<sup>   700.00/T</sup> |   7.8470E-12 |
| R214   | HC3P + HO2 ----> OP2  |   1.66E-13e<sup>  1300.00/T</sup> |   1.2994E-11 |
| R215   | HC5P + HO2 ----> OP2  |   1.66E-13e<sup>  1300.00/T</sup> |   1.2994E-11 |
| R217   | ETEP + HO2 ----> OP2  |   1.90E-13e<sup>  1300.00/T</sup> |   1.4872E-11 |
| R218   | OLTP + HO2 ----> OP2  |   1.66E-13e<sup>  1300.00/T</sup> |   1.2994E-11 |
| R219   | OLIP + HO2 ----> OP2  |   1.66E-13e<sup>  1300.00/T</sup> |   1.2994E-11 |
| ROCARO32   | BENP + HO2 ---->   0.602\*OP2 +    0.398\*VROCN1OXY6  |   2.91E-13e<sup>  1300.00/T</sup> |   2.2778E-11 |
| ROCARO42   | TOLP + HO2 ---->   0.720\*OP2 +    0.281\*VROCN1OXY6  |   2.91E-13e<sup>  1300.00/T</sup> |   2.2778E-11 |
| ROCARO52   | XYMP + HO2 ---->   0.048\*OP2 +    0.675\*OP3 +    0.277\*VROCP0OXY4  |   2.91E-13e<sup>  1300.00/T</sup> |   2.2778E-11 |
| ROCARO62   | XYEP + HO2 ---->   0.085\*OP2 +    0.634\*OP3 +    0.281\*VROCP0OXY4  |   2.91E-13e<sup>  1300.00/T</sup> |   2.2778E-11 |
| R229   | APIP1 + HO2 ----> OPB  |   1.5000E-11 |   1.5000E-11 |
| TRP21   | APIP2 + HO2 ----> HOM  |   1.5000E-11 |   1.5000E-11 |
| TRP22   | APINP1 + HO2 ----> TRPN  |   1.5000E-11 |   1.5000E-11 |
| TRP23   | APINP2 + HO2 ----> HOM  |   1.5000E-11 |   1.5000E-11 |
| R230   | LIMP1 + HO2 ----> OPB  |   1.5000E-11 |   1.5000E-11 |
| TRP24   | LIMP2 + HO2 ----> HOM  |   1.5000E-11 |   1.5000E-11 |
| TRP25   | LIMNP1 + HO2 ----> TRPN  |   1.5000E-11 |   1.5000E-11 |
| TRP26   | LIMNP2 + HO2 ----> HOM  |   1.5000E-11 |   1.5000E-11 |
| TRP27   | PINALP + HO2 ----> OPB  |   2.91E-13e<sup>  1300.00/T</sup> |   2.2778E-11 |
| TRP28   | LIMALP + HO2 ----> OPB  |   2.91E-13e<sup>  1300.00/T</sup> |   2.2778E-11 |
| R231   | ACO3 + HO2 ---->   0.440\*HO +    0.440\*MO2 +    0.150\*ORA2 +    0.410\*PAA  |   4.30E-13e<sup>  1040.00/T</sup> |   1.4072E-11 |
| R232   | RCO3 + HO2 ---->   0.440\*HO +    0.440\*ETHP +    0.150\*ORA2 +    0.410\*PAA  |   4.30E-13e<sup>  1040.00/T</sup> |   1.4072E-11 |
| R233   | ACTP + HO2 ---->   0.150\*HO +    0.150\*ACO3 +    0.150\*HCHO +    0.850\*OP2  |   1.15E-13e<sup>  1300.00/T</sup> |   9.0016E-12 |
| R234   | MEKP + HO2 ----> OP2  |   1.15E-13e<sup>  1300.00/T</sup> |   9.0016E-12 |
| R235   | KETP + HO2 ----> OP2  |   1.15E-13e<sup>  1300.00/T</sup> |   9.0016E-12 |
| R236   | MACP + HO2 ----> MAHP  |   1.82E-13e<sup>  1300.00/T</sup> |   1.4246E-11 |
| R237   | MCP + HO2 ----> MAHP  |   1.82E-13e<sup>  1300.00/T</sup> |   1.4246E-11 |
| R238   | MVKP + HO2 ----> OP2  |   2.91E-13e<sup>  1300.00/T</sup> |   2.2778E-11 |
| R239   | UALP + HO2 ----> OP2  |   2.91E-13e<sup>  1300.00/T</sup> |   2.2778E-11 |
| R240   | ADDC + HO2 ----> OP2  |   3.75E-13e<sup>   980.00/T</sup> |   1.0035E-11 |
| R241   | CHO + HO2 ----> CSL  |   1.0000E-11 |   1.0000E-11 |
| R242   | MCTP + HO2 ----> OP2  |   3.75E-13e<sup>   980.00/T</sup> |   1.0035E-11 |
| R243   | ORAP + HO2 ----> OP2  |   1.15E-13e<sup>  1300.00/T</sup> |   9.0016E-12 |
| R244   | OLNN + HO2 ----> ONIT  |   1.66E-13e<sup>  1300.00/T</sup> |   1.2994E-11 |
| R245   | OLND + HO2 ----> ONIT  |   1.66E-13e<sup>  1300.00/T</sup> |   1.2994E-11 |
| R246   | ADCN + HO2 ----> OP2  |   3.75E-13e<sup>   980.00/T</sup> |   1.0035E-11 |
| R247   | XO2 + HO2 ----> OP2  |   1.66E-13e<sup>  1300.00/T</sup> |   1.2994E-11 |
| R248   | MO2 + MO2 ---->   0.740\*HO2 +    1.370\*HCHO +    0.630\*MOH  |   9.50E-14e<sup>   390.00/T</sup> |   3.5141E-13 |
| R249   | ETHP + MO2 ----> HO2 +    0.750\*HCHO +    0.750\*ACD +    0.250\*MOH +    0.250\*EOH  |   1.18E-13e<sup>   158.00/T</sup> |   2.0046E-13 |
| R250   | HC3P + MO2 ---->   0.894\*HO2 +    0.080\*MO2 +    0.026\*ETHP +    0.026\*XO2 +    0.827\*HCHO +    0.198\*ALD +    0.497\*KET +    0.050\*GLY +    0.250\*MOH +    0.250\*ROH  |   9.46E-14e<sup>   431.00/T</sup> |   4.0151E-13 |
| R251   | HC5P + MO2 ---->   0.842\*HO2 +    0.018\*MO2 +    0.140\*ETHP +    0.191\*XO2 +    0.777\*HCHO +    0.251\*ALD +    0.618\*KET +    0.250\*MOH +    0.250\*ROH  |   1.00E-13e<sup>   467.00/T</sup> |   4.7890E-13 |
| R253   | ETEP + MO2 ----> HO2 +    1.950\*HCHO +    0.150\*ALD +    0.250\*MOH +    0.250\*ETEG  |   1.71E-13e<sup>   708.00/T</sup> |   1.8378E-12 |
| R254   | OLTP + MO2 ----> HO2 +    1.500\*HCHO +    0.705\*ALD +    0.045\*KET +    0.250\*MOH +    0.250\*ROH  |   1.46E-13e<sup>   708.00/T</sup> |   1.5691E-12 |
| R255   | OLIP + MO2 ----> HO2 +    0.750\*HCHO +    1.280\*ALD +    0.218\*KET +    0.250\*MOH +    0.250\*ROH  |   9.18E-14e<sup>   708.00/T</sup> |   9.8659E-13 |
| ROCARO35   | BENP + MO2 ---->   0.680\*HCHO +    1.370\*HO2 +    0.320\*MOH +    0.000\*BALD + GLY +    0.500\*FURANONE +    0.250\*DCB2 +    0.250\*DCB3  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| ROCARO45   | TOLP + MO2 ---->   0.680\*HCHO +    1.285\*HO2 +    0.320\*MOH +    0.085\*BALD +    0.549\*GLY +    0.366\*MGLY +    0.366\*FURANONE +    0.549\*DCB1  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| ROCARO55   | XYMP + MO2 ---->   0.680\*HCHO +    1.322\*HO2 +    0.320\*MOH +    0.048\*BALD +    0.704\*GLY +    0.247\*MGLY +    0.352\*FURANONE +    0.600\*DCB2  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| ROCARO65   | XYEP + MO2 ---->   0.680\*HCHO +    1.285\*HO2 +    0.320\*MOH +    0.085\*BALD +    0.549\*GLY +    0.366\*MGLY +    0.457\*FURANONE +    0.457\*DCB2  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| R264   | ISOP + MO2 ----> HO2 +    1.310\*HCHO +    0.159\*MACR +    0.250\*MVK +    0.250\*MOH +    0.250\*ROH +    0.023\*ALD +    0.018\*GLY +    0.016\*HKET  |   3.40E-14e<sup>   221.00/T</sup> |   7.1350E-14 |
| R265   | APIP1 + MO2 ----> HO2 +    0.680\*HCHO +    0.600\*PINAL +    0.070\*KET +    0.320\*MOH +    0.250\*ROH  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| TRP29   | APIP2 + MO2 ----> HO2 +    0.750\*HCHO +    0.250\*MOH + HOM  |   1.0000E-10 |   1.0000E-10 |
| TRP30   | APINP1 + MO2 ---->   0.370\*HO2 +    0.860\*NO2 +    0.680\*HCHO +    0.860\*PINAL +    0.320\*MOH +    0.140\*TRPN  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| TRP31   | APINP2 + MO2 ---->   0.750\*HO2 +    0.750\*NO2 +    0.250\*MOH +    0.750\*HCHO + HOM  |   1.0000E-10 |   1.0000E-10 |
| R266   | LIMP1 + MO2 ----> HO2 + HCHO +    0.420\*LIMAL +    0.300\*KET +    0.320\*MOH +    0.270\*ROH  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| TRP32   | LIMP2 + MO2 ----> HO2 +    0.750\*HCHO +    0.250\*MOH + HOM  |   1.0000E-10 |   1.0000E-10 |
| TRP33   | LIMNP1 + MO2 ---->   0.370\*HO2 +    0.680\*HCHO +    0.700\*LIMAL +    0.700\*NO2 +    0.320\*MOH +    0.300\*TRPN  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| TRP34   | LIMNP2 + MO2 ---->   0.750\*HO2 +    0.750\*HCHO +    0.750\*NO2 +    0.250\*MOH + HOM  |   1.0000E-10 |   1.0000E-10 |
| R267   | ACO3 + MO2 ---->   0.900\*HO2 +    0.900\*MO2 + HCHO +    0.100\*ORA2  |   2.00E-11e<sup>   500.00/T</sup> |   1.0699E-10 |
| R268   | RCO3 + MO2 ---->   0.900\*HO2 +    0.900\*MO2 + HCHO +    0.100\*ORA2  |   2.00E-11e<sup>   500.00/T</sup> |   1.0699E-10 |
| R269   | ACTP + MO2 ---->   0.500\*HO2 +    0.500\*ACO3 +    1.500\*HCHO +    0.250\*MOH +    0.250\*ROH +    0.125\*ORA2  |   7.50E-13e<sup>   500.00/T</sup> |   4.0121E-12 |
| R270   | MEKP + MO2 ---->   0.834\*HO2 + HCHO +    0.334\*DCB1 +    0.250\*MOH +    0.250\*ROH  |   6.91E-13e<sup>   508.00/T</sup> |   3.7971E-12 |
| R271   | KETP + MO2 ----> HO2 +    0.750\*HCHO +    0.500\*DCB1 +    0.250\*MOH +    0.250\*ROH  |   6.91E-13e<sup>   508.00/T</sup> |   3.7971E-12 |
| R272   | MACP + MO2 ---->   0.500\*HO2 +    0.269\*ACO3 +    0.500\*CO +    1.660\*HCHO +    0.067\*ORA2 +    0.250\*MO2 +    0.250\*MOH +    0.250\*ROH  |   3.40E-14e<sup>   221.00/T</sup> |   7.1350E-14 |
| R273   | MCP + MO2 ----> NO2 + HO2 +    1.500\*HCHO +    0.500\*HKET +    0.250\*MOH +    0.250\*ROH  |   3.40E-14e<sup>   221.00/T</sup> |   7.1350E-14 |
| R274   | MVKP + MO2 ----> HO2 +    1.160\*ACO3 +    1.160\*XO2 +    1.500\*HCHO +    1.750\*ALD +    0.500\*MGLY +    0.250\*MOH +    0.250\*ROH +    0.292\*ORA2  |   8.3700E-14 |   8.3700E-14 |
| R275   | UALP + MO2 ----> HO2 +    0.305\*CO +    0.773\*HCHO +    0.203\*ALD +    0.525\*KET +    0.135\*GLY +    0.105\*MGLY +    0.250\*MOH +    0.250\*ROH  |   3.40E-14e<sup>   221.00/T</sup> |   7.1350E-14 |
| R276   | BALP + MO2 ----> HO2 + BAL1 + HCHO  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| R277   | BAL1 + MO2 ----> HO2 + BAL2 + HCHO  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| R278   | ADDC + MO2 ---->   2.000\*HO2 + HCHO +    0.320\*HKET +    0.680\*GLY +    0.680\*OP2  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| R279   | MCTP + MO2 ----> HO2 + MCTO + HCHO  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| R280   | ORAP + MO2 ----> HCHO + HO2 + GLY  |   7.50E-13e<sup>   500.00/T</sup> |   4.0121E-12 |
| R281   | OLNN + MO2 ---->   2.000\*HO2 + HCHO + ONIT  |   1.60E-13e<sup>   708.00/T</sup> |   1.7195E-12 |
| R282   | OLND + MO2 ---->   0.500\*HO2 +    0.500\*NO2 +    0.965\*HCHO +    0.930\*ALD +    0.348\*KET +    0.250\*MOH +    0.250\*ROH +    0.500\*ONIT  |   9.68E-14e<sup>   708.00/T</sup> |   1.0403E-12 |
| R283   | ADCN + MO2 ----> HO2 +    0.700\*NO2 + HCHO +    0.700\*GLY +    0.700\*OP2 +    0.300\*ONIT  |   3.5600E-14 |   3.5600E-14 |
| R284   | XO2 + MO2 ----> HO2 + HCHO  |   5.99E-15e<sup>  1510.00/T</sup> |   9.4829E-13 |
| R285   | ETHP + ACO3 ---->   0.500\*HO2 +    0.500\*MO2 + ACD +    0.500\*ORA2  |   1.03E-12e<sup>   211.00/T</sup> |   2.0902E-12 |
| R286   | HC3P + ACO3 ---->   0.394\*HO2 +    0.580\*MO2 +    0.026\*ETHP +    0.026\*XO2 +    0.130\*HCHO +    0.273\*ALD +    0.662\*KET +    0.067\*GLY +    0.500\*ORA2  |   6.90E-13e<sup>   460.00/T</sup> |   3.2277E-12 |
| R287   | HC5P + ACO3 ---->   0.342\*HO2 +    0.518\*MO2 +    0.140\*ETHP +    0.191\*XO2 +    0.042\*HCHO +    0.381\*ALD +    0.824\*KET +    0.500\*ORA2  |   5.59E-13e<sup>   522.00/T</sup> |   3.2194E-12 |
| R289   | ETEP + ACO3 ---->   0.500\*HO2 +    0.500\*MO2 +    1.600\*HCHO +    0.200\*ALD +    0.500\*ORA2  |   9.48E-13e<sup>   765.00/T</sup> |   1.2335E-11 |
| R290   | OLTP + ACO3 ---->   0.500\*HO2 +    0.500\*MO2 + HCHO +    0.940\*ALD +    0.060\*KET +    0.500\*ORA2  |   8.11E-13e<sup>   765.00/T</sup> |   1.0552E-11 |
| R291   | OLIP + ACO3 ---->   0.500\*HO2 +    0.500\*MO2 +    1.710\*ALD +    0.290\*KET +    0.500\*ORA2  |   5.09E-13e<sup>   765.00/T</sup> |   6.6228E-12 |
| ROCARO36   | BENP + ACO3 ---->   0.700\*MO2 + HO2 +    0.300\*ORA2 +    0.000\*BALD + GLY +    0.500\*FURANONE +    0.250\*DCB2 +    0.250\*DCB3  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| ROCARO46   | TOLP + ACO3 ---->   0.700\*MO2 +    0.915\*HO2 +    0.300\*ORA2 +    0.085\*BALD +    0.549\*GLY +    0.366\*MGLY +    0.366\*FURANONE +    0.549\*DCB1  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| ROCARO56   | XYMP + ACO3 ---->   0.700\*MO2 +    0.952\*HO2 +    0.300\*ORA2 +    0.048\*BALD +    0.704\*GLY +    0.247\*MGLY +    0.352\*FURANONE +    0.600\*DCB2  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| ROCARO66   | XYEP + ACO3 ---->   0.700\*MO2 +    0.915\*HO2 +    0.300\*ORA2 +    0.085\*BALD +    0.549\*GLY +    0.366\*MGLY +    0.457\*FURANONE +    0.457\*DCB2  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| R300   | ISOP + ACO3 ---->   0.500\*HO2 +    0.500\*MO2 +    1.048\*HCHO +    0.219\*MACR +    0.305\*MVK +    0.500\*ORA2  |   8.40E-14e<sup>   221.00/T</sup> |   1.7628E-13 |
| R301   | APIP1 + ACO3 ---->   0.630\*HO2 +    0.700\*MO2 +    0.600\*PINAL +    0.300\*ORA2 +    0.070\*KET +    0.250\*ROH  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| TRP35   | APIP2 + ACO3 ---->   0.500\*HO +    0.500\*MO2 +    0.500\*ORA2 + HOM  |   1.0000E-10 |   1.0000E-10 |
| TRP36   | APINP1 + ACO3 ---->   0.860\*NO2 +    0.140\*TRPN +    0.860\*PINAL +    0.700\*MO2 +    0.300\*ORA2  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| TRP37   | APINP2 + ACO3 ---->   0.500\*NO2 +    0.500\*MO2 +    0.500\*ORA2 + HOM  |   1.0000E-10 |   1.0000E-10 |
| R302   | LIMP1 + ACO3 ---->   0.630\*HO2 +    0.700\*MO2 +    0.420\*LIMAL +    0.300\*KET +    0.300\*ORA2 +    0.320\*HCHO +    0.270\*ROH  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| TRP38   | LIMP2 + ACO3 ---->   0.500\*HO +    0.500\*MO2 +    0.500\*ORA2 + HOM  |   1.0000E-10 |   1.0000E-10 |
| TRP39   | LIMNP1 + ACO3 ---->   0.700\*NO2 +    0.700\*LIMAL +    0.300\*TRPN +    0.700\*MO2 +    0.300\*ORA2  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| TRP40   | LIMNP2 + ACO3 ---->   0.500\*MO2 +    0.500\*NO2 +    0.500\*ORA2 + HOM  |   1.0000E-10 |   1.0000E-10 |
| R303   | ACO3 + ACO3 ---->   2.000\*MO2  |   2.50E-12e<sup>   500.00/T</sup> |   1.3374E-11 |
| R304   | RCO3 + ACO3 ----> MO2 + ETHP  |   2.50E-12e<sup>   500.00/T</sup> |   1.3374E-11 |
| R305   | ACTP + ACO3 ---->   0.500\*MO2 +    0.500\*ACO3 + HCHO +    0.750\*ORA2  |   7.51E-13e<sup>   565.00/T</sup> |   4.9962E-12 |
| R306   | MEKP + ACO3 ---->   0.330\*HO2 +    0.500\*MO2 +    0.330\*HCHO +    0.334\*DCB1 +    0.500\*ORA2  |   7.51E-13e<sup>   565.00/T</sup> |   4.9962E-12 |
| R307   | KETP + ACO3 ---->   0.500\*HO2 +    0.500\*MO2 +    0.500\*DCB1 +    0.500\*ORA2  |   7.51E-13e<sup>   565.00/T</sup> |   4.9962E-12 |
| R308   | MACP + ACO3 ---->   0.635\*ORA2 +    0.500\*MO2 +    0.269\*ACO3 +    0.500\*CO + HCHO  |   8.40E-14e<sup>   221.00/T</sup> |   1.7628E-13 |
| R309   | MCP + ACO3 ----> NO2 +    0.500\*HO2 + HCHO +    0.500\*HKET +    0.500\*MO2 +    0.500\*ORA2  |   8.40E-14e<sup>   221.00/T</sup> |   1.7628E-13 |
| R310   | MVKP + ACO3 ---->   0.500\*HO2 +    0.500\*MO2 +    1.160\*ACO3 +    1.160\*XO2 + HCHO +    2.300\*ALD +    0.500\*MGLY +    1.083\*ORA2  |   1.68E-12e<sup>   500.00/T</sup> |   8.9872E-12 |
| R311   | UALP + ACO3 ---->   0.500\*HO2 +    0.500\*MO2 +    0.500\*CO +    0.030\*HCHO +    0.270\*ALD +    0.700\*KET +    0.180\*GLY +    0.105\*MGLY +    0.500\*ORA2  |   1.68E-12e<sup>   500.00/T</sup> |   8.9872E-12 |
| R312   | BALP + ACO3 ----> MO2 + BAL1  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| R313   | BAL1 + ACO3 ----> MO2 + BAL2  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| R314   | ADDC + ACO3 ---->   2.000\*HO2 + MO2 +    0.320\*HKET +    0.680\*GLY +    0.680\*OP2  |   7.40E-13e<sup>   708.00/T</sup> |   7.9529E-12 |
| R315   | MCTP + ACO3 ----> HO2 + MO2 + MCTO  |   7.40E-13e<sup>   708.00/T</sup> |   7.9529E-12 |
| R316   | ORAP + ACO3 ----> MO2 + GLY  |   7.51E-13e<sup>   565.00/T</sup> |   4.9962E-12 |
| R317   | OLNN + ACO3 ----> HO2 + MO2 + ONIT  |   8.85E-13e<sup>   765.00/T</sup> |   1.1515E-11 |
| R318   | OLND + ACO3 ---->   0.500\*MO2 + NO2 +    0.287\*HCHO +    1.240\*ALD +    0.464\*KET +    0.500\*ORA2  |   5.37E-13e<sup>   765.00/T</sup> |   6.9871E-12 |
| R319   | ADCN + ACO3 ----> HO2 + MO2 +    0.700\*NO2 +    0.700\*GLY +    0.700\*OP2 +    0.300\*ONIT  |   7.40E-13e<sup>   708.00/T</sup> |   7.9529E-12 |
| R320   | XO2 + ACO3 ----> MO2  |   3.40E-14e<sup>  1560.00/T</sup> |   6.3654E-12 |
| R321   | RCO3 + RCO3 ---->   2.000\*ETHP  |   2.50E-12e<sup>   500.00/T</sup> |   1.3374E-11 |
| R322   | MO2 + NO3 ----> HO2 + HCHO + NO2  |   1.2000E-12 |   1.2000E-12 |
| R323   | ETHP + NO3 ----> HO2 + NO2 + ACD  |   1.2000E-12 |   1.2000E-12 |
| R324   | HC3P + NO3 ---->   0.254\*HO2 +    0.140\*MO2 +    0.092\*XO2 +    0.503\*ETHP + NO2 +    0.519\*ACD +    0.147\*ALD +    0.075\*MEK +    0.095\*ACT  |   1.2000E-12 |   1.2000E-12 |
| R325   | HC5P + NO3 ---->   0.488\*HO2 +    0.055\*MO2 +    0.280\*ETHP +    0.485\*XO2 + NO2 +    0.024\*HCHO +    0.241\*ALD +    0.060\*KET +    0.063\*MEK +    0.247\*ACT +    0.048\*ACD +    0.275\*HKET  |   1.2000E-12 |   1.2000E-12 |
| R327   | ETEP + NO3 ----> HO2 + NO2 +    1.600\*HCHO +    0.200\*ALD  |   1.2000E-12 |   1.2000E-12 |
| R328   | OLTP + NO3 ---->   0.470\*ALD +    0.790\*HCHO +    0.790\*HO2 + NO2 +    0.180\*MEK +    0.020\*ACD +    0.090\*ACT  |   1.2000E-12 |   1.2000E-12 |
| R329   | OLIP + NO3 ---->   0.860\*HO2 +    0.720\*ALD +    0.110\*KET + NO2 +    0.200\*ACT +    0.850\*ACD +    0.040\*HKET  |   1.2000E-12 |   1.2000E-12 |
| ROCARO34   | BENP + NO3 ----> NO2 + HO2 +    0.000\*BALD + GLY +    0.500\*FURANONE +    0.250\*DCB2 +    0.250\*DCB3  |   2.3000E-12 |   2.3000E-12 |
| ROCARO44   | TOLP + NO3 ----> NO2 +    0.915\*HO2 +    0.085\*BALD +    0.549\*GLY +    0.366\*MGLY +    0.366\*FURANONE +    0.549\*DCB1  |   2.3000E-12 |   2.3000E-12 |
| ROCARO54   | XYMP + NO3 ----> NO2 +    0.952\*HO2 +    0.048\*BALD +    0.704\*GLY +    0.247\*MGLY +    0.352\*FURANONE +    0.600\*DCB2  |   2.3000E-12 |   2.3000E-12 |
| ROCARO64   | XYEP + NO3 ----> NO2 +    0.915\*HO2 +    0.085\*BALD +    0.549\*GLY +    0.366\*MGLY +    0.457\*FURANONE +    0.457\*DCB2  |   2.3000E-12 |   2.3000E-12 |
| R338   | ISOP + NO3 ----> HO2 + NO2 +    0.750\*HCHO +    0.318\*MACR +    0.500\*MVK +    0.024\*GLY +    0.033\*HKET +    0.031\*ALD  |   1.2000E-12 |   1.2000E-12 |
| R339   | APIP1 + NO3 ----> HO2 + NO2 + ALD + KET  |   1.2000E-12 |   1.2000E-12 |
| R340   | LIMP1 + NO3 ----> HO2 + NO2 +    0.385\*OLI +    0.385\*HCHO +    0.615\*MACR  |   1.2000E-12 |   1.2000E-12 |
| R341   | ACO3 + NO3 ----> MO2 + NO2  |   4.0000E-12 |   4.0000E-12 |
| R342   | RCO3 + NO3 ----> ETHP + NO2  |   4.0000E-12 |   4.0000E-12 |
| R343   | ACTP + NO3 ----> ACO3 + NO2 + HCHO  |   1.2000E-12 |   1.2000E-12 |
| R344   | MEKP + NO3 ---->   0.670\*HO2 + NO2 +    0.330\*HCHO +    0.670\*DCB1  |   1.2000E-12 |   1.2000E-12 |
| R345   | KETP + NO3 ----> HO2 + NO2 + DCB1  |   1.2000E-12 |   1.2000E-12 |
| R346   | MACP + NO3 ----> HCHO +    0.538\*ACO3 + CO + NO2  |   1.2000E-12 |   1.2000E-12 |
| R347   | MCP + NO3 ----> NO2 + HO2 + HCHO + HKET  |   1.2000E-12 |   1.2000E-12 |
| R348   | MVKP + NO3 ---->   0.300\*HO2 +    0.700\*ACO3 +    0.700\*XO2 + NO2 +    0.300\*HCHO +    0.700\*ALD +    0.300\*MGLY  |   2.5000E-12 |   2.5000E-12 |
| R349   | UALP + NO3 ----> HO2 + NO2 +    0.610\*CO +    0.030\*HCHO +    0.270\*ALD +    0.700\*KET +    0.180\*GLY +    0.210\*MGLY  |   2.5000E-12 |   2.5000E-12 |
| R350   | BALP + NO3 ----> BAL1 + NO2  |   2.5000E-12 |   2.5000E-12 |
| R351   | BAL1 + NO3 ----> BAL2 + NO2  |   2.5000E-12 |   2.5000E-12 |
| R352   | ADDC + NO3 ----> HO2 + NO2 +    0.320\*HKET +    0.680\*GLY +    0.680\*OP2  |   1.2000E-12 |   1.2000E-12 |
| R353   | MCTP + NO3 ----> NO2 + MCTO  |   1.2000E-12 |   1.2000E-12 |
| R354   | ORAP + NO3 ----> NO2 + GLY + HO2  |   1.2000E-12 |   1.2000E-12 |
| R355   | OLNN + NO3 ----> HO2 + NO2 + ONIT  |   1.2000E-12 |   1.2000E-12 |
| R356   | OLND + NO3 ---->   2.000\*NO2 +    0.287\*HCHO +    1.240\*ALD +    0.464\*KET  |   1.2000E-12 |   1.2000E-12 |
| R357   | ADCN + NO3 ---->   2.000\*NO2 + GLY + OP2  |   1.2000E-12 |   1.2000E-12 |
| R358   | OLNN + OLNN ----> HO2 +    2.000\*ONIT  |   7.00E-14e<sup>  1000.00/T</sup> |   2.0032E-12 |
| R359   | OLNN + OLND ---->   0.500\*HO2 +    0.500\*NO2 +    0.202\*HCHO +    0.640\*ALD +    0.149\*KET +    1.500\*ONIT  |   4.25E-14e<sup>  1000.00/T</sup> |   1.2162E-12 |
| R360   | OLND + OLND ----> NO2 +    0.504\*HCHO +    1.210\*ALD +    0.285\*KET + ONIT  |   2.96E-14e<sup>  1000.00/T</sup> |   8.4708E-13 |
| R361   | XO2 + NO3 ----> NO2  |   1.2000E-12 |   1.2000E-12 |
| R362   | XO2 + RCO3 ----> ETHP  |   2.50E-12e<sup>   500.00/T</sup> |   1.3374E-11 |
| R363   | XO2 + XO2 ----> |   7.13E-17e<sup>  2950.00/T</sup> |   1.4130E-12 |
| TRP41   | APIP2 + APIP1 ---->   0.960\*HOM +    0.480\*ROH +    0.480\*PINAL +    0.480\*HO +    0.480\*HO2 +    0.040\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP42   | APIP2 + LIMP1 ---->   0.960\*HOM +    0.480\*ROH +    0.480\*LIMAL +    0.480\*HO +    0.480\*HO2 +    0.040\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP43   | APIP2 + ISOP ---->   0.960\*HOM +    0.480\*ROH +    0.480\*HCHO +    0.480\*MVK +    0.480\*HO +    0.480\*HO2 +    0.040\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP44   | LIMP2 + APIP1 ---->   0.960\*HOM +    0.480\*ROH +    0.480\*PINAL +    0.480\*HO +    0.480\*HO2 +    0.040\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP45   | LIMP2 + LIMP1 ---->   0.960\*HOM +    0.480\*ROH +    0.480\*LIMAL +    0.480\*HO +    0.480\*HO2 +    0.040\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP46   | LIMP2 + ISOP ---->   0.960\*HOM +    0.480\*ROH +    0.480\*HCHO +    0.480\*MVK +    0.480\*HO +    0.480\*HO2 +    0.040\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP47   | APINP2 + APIP1 ---->   0.960\*HOM +    0.480\*ROH +    0.480\*PINAL +    0.480\*NO2 +    0.480\*HO2 +    0.040\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP48   | APINP2 + LIMP1 ---->   0.960\*HOM +    0.480\*ROH +    0.480\*LIMAL +    0.480\*NO2 +    0.480\*HO2 +    0.040\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP49   | APINP2 + ISOP ---->   0.960\*HOM +    0.480\*ROH +    0.480\*HCHO +    0.480\*MVK +    0.480\*NO2 +    0.480\*HO2 +    0.040\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP50   | LIMNP2 + APIP1 ---->   0.960\*HOM +    0.480\*ROH +    0.480\*PINAL +    0.480\*NO2 +    0.480\*HO2 +    0.040\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP51   | LIMNP2 + LIMP1 ---->   0.960\*HOM +    0.480\*ROH +    0.480\*LIMAL +    0.480\*NO2 +    0.480\*HO2 +    0.040\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP52   | LIMNP2 + ISOP ---->   0.960\*HOM +    0.480\*ROH +    0.480\*HCHO +    0.480\*MVK +    0.480\*NO2 +    0.480\*HO2 +    0.040\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| RAM17   | IEPOX + HO ----> HO  |   5.78E-11e<sup>  -400.00/T</sup> |   1.5110E-11 |
| R001c   | VROCIOXY + HO ---->   0.852\*ETHP +    0.149\*ASOATJ  |   6.8900E-12 |   6.8900E-12 |
| R002c   | SLOWROC + HO ----> ETHP +    0.001\*ASOATJ  |   6.5500E-14 |   6.5500E-14 |
| T17   | ACRO + HO ---->   0.570\*MACP +    0.430\*MCP  |   8.00E-12e<sup>   380.00/T</sup> |   2.8616E-11 |
| T18   | ACRO + O3 ---->   0.840\*CO +    0.560\*HO2 +    0.280\*HO +    0.720\*HCHO +    0.620\*GLY  |   2.9000E-19 |   2.9000E-19 |
| T19   | ACRO + NO3 ---->   0.680\*HCHO +    0.320\*MACP +    0.680\*XO2 +    0.680\*MGLY +    0.320\*HNO3 +    0.680\*NO2  |   3.4000E-15 |   3.4000E-15 |
| T20   | ACRO ----> CO +    0.477\*HO2 +    0.250\*ETE +    0.354\*ACO3 +    0.204\*HO +    0.150\*HCHO +    0.027\*MO2  | ACRO_09 | Not Available<sup>1</sup> | 
| T10   | BDE13 + HO ---->   0.667\*BDE13P +    0.333\*UALD +    0.333\*HO2  |   1.48E-11e<sup>   448.00/T</sup> |   6.6502E-11 |
| T10a   | BDE13P + NO ---->   0.968\*HO2 +    0.968\*NO2 +    0.895\*ACRO +    0.895\*HCHO +    0.072\*FURAN +    0.032\*ONIT  |   9.0500E-12 |   9.0500E-12 |
| T10b   | BDE13P + NO3 ----> HO2 + NO2 +    0.925\*ACRO +    0.925\*HCHO +    0.075\*FURAN  |   2.3000E-12 |   2.3000E-12 |
| T10c   | BDE13P + HO2 ----> OP2  |   1.6100E-11 |   1.6100E-11 |
| T10d   | BDE13P + MO2 ---->   0.320\*MOH +    1.143\*HCHO +    0.870\*HO2 +    0.463\*ACRO +    0.250\*OLT +    0.231\*MVK +    0.037\*FURAN +    0.019\*UALD  |   2.3900E-12 |   2.3900E-12 |
| T10e   | BDE13P + ACO3 ---->   0.700\*MO2 +    0.300\*ORA2 +    0.800\*HO2 +    0.740\*ACRO +    0.740\*HCHO +    0.185\*MVK +    0.060\*FURAN +    0.015\*UALD  |   1.3700E-11 |   1.3700E-11 |
| T11   | BDE13 + O3 ---->   0.620\*ACRO +    0.630\*CO +    0.420\*HO2 +    0.080\*HO +    0.830\*HCHO +    0.170\*ETE  |   1.34E-14e<sup> -2283.00/T</sup> |   6.3331E-18 |
| T12   | BDE13 + NO3 ---->   0.900\*OLNN +    0.100\*OLND +    0.900\*ACRO  |   1.0000E-13 |   1.0000E-13 |
| R003c   | FURAN + HO ---->   0.490\*DCB1 +    0.490\*HO2 +    0.510\*FURANO2  |   5.0100E-11 |   5.0100E-11 |
| R004c   | FURANO2 + NO ---->   0.080\*ONIT +    0.920\*NO2 +    0.920\*FURANONE +    0.750\*HO2 +    0.170\*MO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R005c   | FURANO2 + HO2 ---->   0.600\*OP2 +    0.400\*FURANONE +    0.400\*HO +    0.320\*HO2 +    0.080\*MO2  |   3.75E-13e<sup>   980.00/T</sup> |   1.0035E-11 |
| R006c   | FURANONE + HO ---->   0.650\*KET +    0.310\*GLY +    0.660\*HO2 +    0.340\*MO2 +    0.430\*CO +    0.040\*ASOATJ  |   4.4000E-11 |   4.4000E-11 |
| R007c   | FURAN + O3 ---->   0.020\*HO + ALD  |   3.4300E-17 |   3.4300E-17 |
| R008c   | FURAN + NO3 ----> NO2 +    0.800\*DCB1 +    0.200\*DCB3  |   8.9900E-12 |   8.9900E-12 |
| R010c   | PROG + HO ---->   0.613\*HKET +    0.387\*ALD + HO2  |   1.2000E-11 |   1.2000E-11 |
| R011c   | SESQ + NO3 ----> SESQNRO2  |   1.9000E-11 |   1.9000E-11 |
| R012c   | SESQNRO2 + HO2 ----> VROCP0OXY4  |   2.84E-13e<sup>  1300.00/T</sup> |   2.2230E-11 |
| R013c   | SESQNRO2 + NO ----> VROCP3OXY2 +    2.000\*NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R014c   | SESQNRO2 + NO3 ----> VROCP3OXY2 +    2.000\*NO2  |   2.3000E-12 |   2.3000E-12 |
| R015c   | SESQ + O3 ---->   0.982\*VROCP3OXY2 +    0.018\*VROCN2OXY2  |   1.2000E-14 |   1.2000E-14 |
| R016c   | SESQ + HO ----> SESQRO2  |   1.9700E-10 |   1.9700E-10 |
| R017c   | SESQRO2 + HO2 ----> VROCP0OXY2  |   2.84E-13e<sup>  1300.00/T</sup> |   2.2230E-11 |
| R019c   | SESQRO2 + NO3 ----> VROCP3OXY2  |   2.3000E-12 |   2.3000E-12 |
| R020c   | SESQRO2 + NO ---->   0.247\*VROCP1OXY3 +    0.753\*VROCP3OXY2 +    0.753\*NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| HET_GLY   | GLY ----> AGLYJ  | HETERO_GLY | Not Available<sup>2</sup> | 
| HET_MGLY   | MGLY ----> AGLYJ  | HETERO_MGLY | Not Available<sup>2</sup> | 
| HET_N2O5   | N2O5 ---->   2.000\*HNO3  | HETERO_N2O5IJ | Not Available<sup>2</sup> | 
| HET_N02   | NO2 ---->   0.500\*HONO +    0.500\*HNO3  | HETERO_NO2 | Not Available<sup>2</sup> | 
| HAL_Ozone   | O3 ----> | SEAWATER*min( 6.701E-11e<sup> 1.074E+01P</sup>+ 3.415E-08e<sup>-6.713E-01P</sup>, <br> 2.000E-06) |   2.0000E-06<sup>4</sup>| 
| HET_IEPOX   | IEPOX ----> IEPOXP  | HETERO_IEPOX | Not Available<sup>2</sup> | 
| HET_ISO3TET   | IEPOXP ----> AISO3NOSJ  | HETERO_ISO3NOSJ | Not Available<sup>2</sup> | 
| HET_IEPOXOS   | IEPOXP + ASO4J ----> AISO3OSJ  | HETERO_ISO3OSJ | Not Available<sup>2</sup> | 
| ROCALK1c   | VROCP6ALK + HO ----> VROCP6ALKP  |   1.5300E-11 |   1.5300E-11 |
| ROCALK2c   | VROCP5ALK + HO ----> VROCP5ALKP  |   1.6800E-11 |   1.6800E-11 |
| ROCALK3c   | VROCP4ALK + HO ----> VROCP4ALKP  |   2.2400E-11 |   2.2400E-11 |
| ROCALK4c   | VROCP3ALK + HO ----> VROCP3ALKP  |   2.6700E-11 |   2.6700E-11 |
| ROCALK5c   | VROCP2ALK + HO ----> VROCP2ALKP  |   3.0900E-11 |   3.0900E-11 |
| ROCALK6c   | VROCP1ALK + HO ----> VROCP1ALKP  |   3.3800E-11 |   3.3800E-11 |
| HC1001   | HC10 + HO ----> HC10P  |   1.1000E-11 |   1.1000E-11 |
| ROCALK7c   | VROCP6ALKP + NO ---->   0.720\*VROCP6ALKP2 +    0.280\*VROCP4OXY2 +    0.720\*NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK8c   | VROCP5ALKP + NO ---->   0.720\*VROCP5ALKP2 +    0.280\*VROCP3OXY2 +    0.720\*NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK9c   | VROCP4ALKP + NO ---->   0.720\*VROCP4ALKP2 +    0.280\*VROCP2OXY2 +    0.720\*NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK10c   | VROCP3ALKP + NO ---->   0.720\*VROCP3ALKP2 +    0.280\*VROCP1OXY1 +    0.720\*NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK11c   | VROCP2ALKP + NO ---->   0.720\*VROCP2ALKP2 +    0.280\*VROCP0OXY2 +    0.720\*NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK12c   | VROCP1ALKP + NO ---->   0.720\*VROCP1ALKP2 +    0.280\*VROCN1OXY1 +    0.720\*NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| HC1002   | HC10P + NO ---->   0.740\*HC10P2 +    0.260\*ONIT +    0.740\*NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK13c   | VROCP6ALKP + NO3 ----> VROCP6ALKP2 + NO2  |   2.3000E-12 |   2.3000E-12 |
| ROCALK14c   | VROCP5ALKP + NO3 ----> VROCP5ALKP2 + NO2  |   2.3000E-12 |   2.3000E-12 |
| ROCALK15c   | VROCP4ALKP + NO3 ----> VROCP4ALKP2 + NO2  |   2.3000E-12 |   2.3000E-12 |
| ROCALK16c   | VROCP3ALKP + NO3 ----> VROCP3ALKP2 + NO2  |   2.3000E-12 |   2.3000E-12 |
| ROCALK17c   | VROCP2ALKP + NO3 ----> VROCP2ALKP2 + NO2  |   2.3000E-12 |   2.3000E-12 |
| ROCALK18c   | VROCP1ALKP + NO3 ----> VROCP1ALKP2 + NO2  |   2.3000E-12 |   2.3000E-12 |
| HC1003   | HC10P + NO3 ----> HC10P2 + NO2  |   2.3000E-12 |   2.3000E-12 |
| ROCALK19c   | VROCP6ALKP + HO2 ----> VROCP3OXY2  |   2.1700E-11 |   2.1700E-11 |
| ROCALK20c   | VROCP5ALKP + HO2 ----> VROCP2OXY2  |   2.2000E-11 |   2.2000E-11 |
| ROCALK21c   | VROCP4ALKP + HO2 ----> VROCP1OXY1  |   2.2500E-11 |   2.2500E-11 |
| ROCALK22c   | VROCP3ALKP + HO2 ----> VROCP0OXY2  |   2.2600E-11 |   2.2600E-11 |
| ROCALK23c   | VROCP2ALKP + HO2 ----> VROCN1OXY1  |   2.2700E-11 |   2.2700E-11 |
| ROCALK24c   | VROCP1ALKP + HO2 ----> VROCN2OXY2  |   2.2700E-11 |   2.2700E-11 |
| HC1004   | HC10P + HO2 ----> OP2  |   2.66E-13e<sup>  1300.00/T</sup> |   2.0821E-11 |
| ROCALK25c   | VROCP6ALKP2 ----> HO2 + VROCP3OXY2  |   1.8800E-01 |   1.8800E-01 |
| ROCALK26c   | VROCP5ALKP2 ----> HO2 + VROCP2OXY2  |   1.8800E-01 |   1.8800E-01 |
| ROCALK27c   | VROCP4ALKP2 ----> HO2 + VROCP1OXY1  |   1.8800E-01 |   1.8800E-01 |
| ROCALK28c   | VROCP3ALKP2 ----> HO2 + VROCP0OXY2  |   1.8800E-01 |   1.8800E-01 |
| ROCALK29c   | VROCP2ALKP2 ----> HO2 + VROCN1OXY1  |   1.8800E-01 |   1.8800E-01 |
| ROCALK30c   | VROCP1ALKP2 ----> HO2 + VROCN2OXY2  |   1.8800E-01 |   1.8800E-01 |
| HC1005   | HC10P2 ----> HO2 + VROCP4OXY2  |   1.8800E-01 |   1.8800E-01 |
| ROCALK31c   | VROCP6ALKP2 + NO ---->   0.140\*VROCP2OXY2 +    0.860\*NO2 +    0.860\*VROCP3OXY2 +    0.860\*HO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK32c   | VROCP5ALKP2 + NO ---->   0.140\*VROCP1OXY3 +    0.860\*NO2 +    0.860\*VROCP2OXY2 +    0.860\*HO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK33c   | VROCP4ALKP2 + NO ---->   0.140\*VROCP0OXY2 +    0.860\*NO2 +    0.860\*VROCP1OXY1 +    0.860\*HO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK34c   | VROCP3ALKP2 + NO ---->   0.140\*VROCN1OXY1 +    0.860\*NO2 +    0.860\*VROCP0OXY2 +    0.860\*HO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK35c   | VROCP2ALKP2 + NO ---->   0.140\*VROCN2OXY2 +    0.860\*NO2 +    0.860\*VROCN1OXY1 +    0.860\*HO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK36c   | VROCP1ALKP2 + NO ----> VROCN2OXY2 +    0.860\*NO2 +    0.860\*HO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| HC1006   | HC10P2 + NO ---->   0.120\*ONIT +    0.880\*NO2 +    0.880\*KET +    0.880\*HO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK37c   | VROCP6ALKP2 + NO3 ----> NO2 + VROCP3OXY2 + HO2  |   2.3000E-12 |   2.3000E-12 |
| ROCALK38c   | VROCP5ALKP2 + NO3 ----> NO2 + VROCP2OXY2 + HO2  |   2.3000E-12 |   2.3000E-12 |
| ROCALK39c   | VROCP4ALKP2 + NO3 ----> NO2 + VROCP1OXY1 + HO2  |   2.3000E-12 |   2.3000E-12 |
| ROCALK40c   | VROCP3ALKP2 + NO3 ----> NO2 + VROCP0OXY2 + HO2  |   2.3000E-12 |   2.3000E-12 |
| ROCALK41c   | VROCP2ALKP2 + NO3 ----> NO2 + VROCN1OXY1 + HO2  |   2.3000E-12 |   2.3000E-12 |
| ROCALK42c   | VROCP1ALKP2 + NO3 ----> NO2 + VROCN2OXY2 + HO2  |   2.3000E-12 |   2.3000E-12 |
| HC1007   | HC10P2 + NO3 ----> NO2 + KET + HO2  |   2.3000E-12 |   2.3000E-12 |
| ROCALK43c   | VROCP6ALKP2 + HO2 ----> VROCP1OXY3  |   2.1700E-11 |   2.1700E-11 |
| ROCALK44c   | VROCP5ALKP2 + HO2 ----> VROCP0OXY2  |   2.2000E-11 |   2.2000E-11 |
| ROCALK45c   | VROCP4ALKP2 + HO2 ----> VROCN1OXY1  |   2.2500E-11 |   2.2500E-11 |
| ROCALK46c   | VROCP3ALKP2 + HO2 ----> VROCN2OXY2  |   2.2600E-11 |   2.2600E-11 |
| ROCALK47c   | VROCP2ALKP2 + HO2 ----> VROCN2OXY2  |   2.2700E-11 |   2.2700E-11 |
| ROCALK48c   | VROCP1ALKP2 + HO2 ----> VROCN2OXY2  |   2.2700E-11 |   2.2700E-11 |
| HC1008   | HC10P2 + HO2 ----> VROCP2OXY2  |   2.66E-13e<sup>  1300.00/T</sup> |   2.0821E-11 |
| ROCARO01   | VROCP6ARO + HO ---->   0.840\*VROCP6AROP +    0.160\*HO2 +    0.160\*VROCP4OXY2  |   1.8100E-11 |   1.8100E-11 |
| ROCARO02   | VROCP6AROP + HO2 ---->   0.059\*VROCP4OXY2 +    0.905\*VROCP1OXY3 +    0.036\*VROCN2OXY4  |   2.91E-13e<sup>  1300.00/T</sup> |   2.2778E-11 |
| ROCARO03   | VROCP6AROP + NO ---->   0.000\*VROCP4OXY2 +    0.002\*VROCP2OXY2 +    0.000\*VROCN1OXY3 +    0.998\*NO2 +    0.998\*HO2 +    0.059\*BALD +    0.469\*GLY +    0.469\*MGLY +    0.469\*FURANONE +    0.469\*DCB2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCARO04   | VROCP6AROP + NO3 ----> NO2 +    0.941\*HO2 +    0.059\*BALD +    0.470\*GLY +    0.470\*MGLY +    0.470\*FURANONE +    0.470\*DCB2  |   2.3000E-12 |   2.3000E-12 |
| ROCARO05   | VROCP6AROP + MO2 ---->   0.680\*HCHO +    1.310\*HO2 +    0.320\*MOH +    0.059\*BALD +    0.470\*GLY +    0.470\*MGLY +    0.470\*FURANONE +    0.470\*DCB2  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| ROCARO06   | VROCP6AROP + ACO3 ---->   0.700\*MO2 +    0.941\*HO2 +    0.300\*ORA2 +    0.059\*BALD +    0.470\*GLY +    0.470\*MGLY +    0.470\*FURANONE +    0.470\*DCB2  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| ROCARO11   | VROCP5ARO + HO ---->   0.840\*VROCP5AROP +    0.160\*HO2 +    0.160\*VROCP3OXY2  |   1.8100E-11 |   1.8100E-11 |
| ROCARO12   | VROCP5AROP + HO2 ---->   0.059\*VROCP3OXY2 +    0.905\*VROCP0OXY2 +    0.036\*VROCN2OXY4  |   2.91E-13e<sup>  1300.00/T</sup> |   2.2778E-11 |
| ROCARO13   | VROCP5AROP + NO ---->   0.000\*VROCP3OXY2 +    0.002\*VROCP1OXY3 +    0.000\*VROCN2OXY4 +    0.998\*NO2 +    0.998\*HO2 +    0.059\*VROCP4OXY2 +    0.469\*GLY +    0.469\*MGLY +    0.469\*FURANONE +    0.469\*DCB2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCARO14   | VROCP5AROP + NO3 ----> NO2 +    0.941\*HO2 +    0.059\*VROCP4OXY2 +    0.470\*GLY +    0.470\*MGLY +    0.470\*FURANONE +    0.470\*DCB2  |   2.3000E-12 |   2.3000E-12 |
| ROCARO15   | VROCP5AROP + MO2 ---->   0.680\*HCHO +    1.310\*HO2 +    0.320\*MOH +    0.059\*VROCP4OXY2 +    0.470\*GLY +    0.470\*MGLY +    0.470\*FURANONE +    0.470\*DCB2  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| ROCARO16   | VROCP5AROP + ACO3 ---->   0.700\*MO2 +    0.941\*HO2 +    0.300\*ORA2 +    0.059\*VROCP4OXY2 +    0.470\*GLY +    0.470\*MGLY +    0.470\*FURANONE +    0.470\*DCB2  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| ROCARO21   | NAPH + HO ---->   0.840\*NAPHP +    0.160\*HO2 +    0.160\*VROCP3OXY2  |   2.3100E-11 |   2.3100E-11 |
| ROCARO22   | NAPHP + HO2 ---->   0.059\*VROCP3OXY2 +    0.905\*VROCP1OXY3 +    0.036\*VROCN2OXY8  |   2.91E-13e<sup>  1300.00/T</sup> |   2.2778E-11 |
| ROCARO23   | NAPHP + NO ---->   0.060\*VROCP4OXY2 +    0.002\*VROCP2OXY2 +    0.000\*VROCN2OXY8 +    0.998\*NO2 +    0.998\*HO2 +    0.469\*GLY +    0.469\*MGLY +    0.469\*FURANONE +    0.469\*DCB2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCARO24   | NAPHP + NO3 ----> NO2 +    0.941\*HO2 +    0.059\*VROCP4OXY2 +    0.470\*GLY +    0.470\*MGLY +    0.470\*FURANONE +    0.470\*DCB2  |   2.3000E-12 |   2.3000E-12 |
| ROCARO25   | NAPHP + MO2 ---->   0.680\*HCHO +    1.310\*HO2 +    0.320\*MOH +    0.059\*VROCP4OXY2 +    0.470\*GLY +    0.470\*MGLY +    0.470\*FURANONE +    0.470\*DCB2  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| ROCARO26   | NAPHP + ACO3 ---->   0.700\*MO2 +    0.941\*HO2 +    0.300\*ORA2 +    0.059\*VROCP4OXY2 +    0.470\*GLY +    0.470\*MGLY +    0.470\*FURANONE +    0.470\*DCB2  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| ROCOXY1c   | VROCN2OXY8 + HO ----> HO +    0.085\*VROCN2OXY8 +    0.258\*DCB1 +    0.258\*MEK +    0.258\*ACD +    0.258\*ALD +    0.258\*MO2 +    0.258\*ETHP +    0.258\*HC3P +    0.258\*MEKP  |   5.9000E-11 |   5.9000E-11 |
| ROCOXY2c   | VROCN2OXY4 + HO ----> HO +    0.464\*VROCN2OXY8 +    0.198\*VROCN2OXY4 +    0.012\*VROCN1OXY6 +    0.015\*VROCN1OXY3 +    0.062\*VROCP0OXY4 +    0.039\*VROCP1OXY3 +    0.049\*VROCP2OXY2 +    0.040\*VROCP3OXY2 +    0.018\*VROCP4OXY2 +    0.031\*OP3 +    0.004\*OP2 +    0.079\*DCB1 +    0.079\*MEK +    0.079\*KET +    0.079\*ACD +    0.079\*ALD +    0.079\*MO2 +    0.079\*ETHP +    0.079\*HC3P +    0.079\*MEKP +    0.079\*HC5P +    0.079\*KETP  |   6.0700E-11 |   6.0700E-11 |
| ROCOXY3c   | VROCN2OXY2 + HO ----> HO +    0.104\*VROCN2OXY8 +    0.564\*VROCN2OXY4 +    0.214\*VROCN2OXY2 +    0.015\*VROCN1OXY6 +    0.030\*VROCN1OXY3 +    0.010\*VROCN1OXY1 +    0.019\*VROCP0OXY4 +    0.046\*VROCP0OXY2 +    0.031\*VROCP1OXY3 +    0.020\*VROCP1OXY1 +    0.046\*VROCP2OXY2 +    0.045\*VROCP3OXY2 +    0.045\*VROCP4OXY2 +    0.033\*VROCP5OXY1 +    0.037\*VROCP6OXY1 +    0.003\*OP3 +    0.039\*DCB1 +    0.039\*HKET +    0.039\*MEK +    0.039\*ACD +    0.039\*ALD +    0.039\*MO2 +    0.039\*ETHP +    0.039\*HC3P +    0.039\*MEKP +    0.092\*HC5P  |   5.5400E-11 |   5.5400E-11 |
| ROCOXY4c   | VROCN1OXY6 + HO ----> HO +    0.204\*VROCN2OXY8 +    0.007\*VROCN2OXY4 +    0.184\*DCB1 +    0.184\*MEK +    0.184\*KET +    0.184\*ACD +    0.184\*ALD +    0.184\*MO2 +    0.184\*ETHP +    0.184\*HC3P +    0.184\*MEKP +    0.184\*HC5P  |   5.6300E-11 |   5.6300E-11 |
| ROCOXY5c   | VROCN1OXY3 + HO ----> HO +    0.279\*VROCN2OXY8 +    0.403\*VROCN2OXY4 +    0.009\*VROCN2OXY2 +    0.032\*VROCN1OXY6 +    0.008\*VROCN1OXY3 +    0.019\*VROCP0OXY4 +    0.010\*VROCP0OXY2 +    0.051\*VROCP1OXY3 +    0.007\*VROCP1OXY1 +    0.051\*VROCP2OXY2 +    0.046\*VROCP3OXY2 +    0.051\*VROCP4OXY2 +    0.014\*VROCP5OXY1 +    0.013\*OP2 +    0.065\*DCB1 +    0.065\*HKET +    0.065\*MEK +    0.065\*ACD +    0.065\*ALD +    0.065\*MO2 +    0.065\*ETHP +    0.065\*HC3P +    0.065\*MEKP +    0.175\*HC5P  |   5.4600E-11 |   5.4600E-11 |
| ROCOXY6c   | VROCN1OXY1 + HO ----> HO +    0.007\*VROCN2OXY8 +    0.119\*VROCN2OXY4 +    0.726\*VROCN2OXY2 +    0.012\*VROCN1OXY6 +    0.030\*VROCN1OXY3 +    0.007\*VROCN1OXY1 +    0.029\*VROCP0OXY4 +    0.045\*VROCP0OXY2 +    0.023\*VROCP1OXY3 +    0.035\*VROCP1OXY1 +    0.062\*VROCP2OXY2 +    0.052\*VROCP3OXY2 +    0.051\*VROCP4OXY2 +    0.035\*VROCP5OXY1 +    0.075\*VROCP6OXY1 +    0.016\*OP3 +    0.006\*OP2 +    0.024\*DCB1 +    0.024\*HKET +    0.024\*MEK +    0.024\*ACD +    0.024\*ALD +    0.024\*MO2 +    0.024\*ETHP +    0.024\*HC3P +    0.024\*MEKP +    0.054\*HC5P  |   4.5000E-11 |   4.5000E-11 |
| ROCOXY7c   | VROCP0OXY4 + HO ----> HO +    0.282\*VROCN2OXY8 +    0.117\*VROCN2OXY4 +    0.032\*VROCN1OXY6 +    0.018\*VROCN1OXY3 +    0.001\*VROCP0OXY4 +    0.066\*VROCP2OXY2 +    0.053\*VROCP3OXY2 +    0.025\*VROCP4OXY2 +    0.005\*OP2 +    0.107\*DCB1 +    0.107\*MEK +    0.107\*KET +    0.107\*ACD +    0.107\*ALD +    0.107\*MO2 +    0.107\*ETHP +    0.107\*HC3P +    0.107\*MEKP +    0.107\*HC5P +    0.107\*KETP  |   5.1700E-11 |   5.1700E-11 |
| ROCOXY8c   | VROCP0OXY2 + HO ----> HO +    0.066\*VROCN2OXY8 +    0.458\*VROCN2OXY4 +    0.116\*VROCN2OXY2 +    0.033\*VROCN1OXY6 +    0.066\*VROCN1OXY3 +    0.005\*VROCN1OXY1 +    0.031\*VROCP0OXY4 +    0.002\*VROCP0OXY2 +    0.040\*VROCP1OXY3 +    0.021\*VROCP1OXY1 +    0.054\*VROCP2OXY2 +    0.052\*VROCP3OXY2 +    0.052\*VROCP4OXY2 +    0.037\*VROCP5OXY1 +    0.042\*VROCP6OXY1 +    0.011\*OP3 +    0.044\*DCB1 +    0.044\*HKET +    0.044\*MEK +    0.044\*ACD +    0.044\*ALD +    0.044\*MO2 +    0.044\*ETHP +    0.044\*HC3P +    0.044\*MEKP +    0.105\*HC5P  |   4.7300E-11 |   4.7300E-11 |
| ROCOXY9c   | VROCP1OXY3 + HO ----> HO +    0.178\*VROCN2OXY8 +    0.192\*VROCN2OXY4 +    0.000\*VROCN2OXY2 +    0.074\*VROCN1OXY6 +    0.045\*VROCN1OXY3 +    0.063\*VROCP0OXY4 +    0.001\*VROCP0OXY2 +    0.001\*VROCP1OXY3 +    0.023\*VROCP2OXY2 +    0.059\*VROCP3OXY2 +    0.065\*VROCP4OXY2 +    0.017\*VROCP5OXY1 +    0.015\*OP3 +    0.017\*OP2 +    0.082\*DCB1 +    0.082\*HKET +    0.082\*MEK +    0.082\*ACD +    0.082\*ALD +    0.082\*MO2 +    0.082\*ETHP +    0.082\*HC3P +    0.082\*MEKP +    0.222\*HC5P  |   4.6000E-11 |   4.6000E-11 |
| ROCOXY10c   | VROCP1OXY1 + HO ----> HO +    0.002\*VROCN2OXY8 +    0.134\*VROCN2OXY4 +    0.335\*VROCN2OXY2 +    0.008\*VROCN1OXY6 +    0.119\*VROCN1OXY3 +    0.076\*VROCN1OXY1 +    0.029\*VROCP0OXY4 +    0.077\*VROCP0OXY2 +    0.028\*VROCP1OXY3 +    0.012\*VROCP1OXY1 +    0.065\*VROCP2OXY2 +    0.071\*VROCP3OXY2 +    0.067\*VROCP4OXY2 +    0.042\*VROCP5OXY1 +    0.091\*VROCP6OXY1 +    0.007\*OP3 +    0.003\*OP2 +    0.030\*DCB1 +    0.030\*HKET +    0.030\*MEK +    0.030\*ACD +    0.030\*ALD +    0.030\*MO2 +    0.030\*ETHP +    0.030\*HC3P +    0.030\*MEKP +    0.065\*HC5P  |   3.8000E-11 |   3.8000E-11 |
| ROCOXY11c   | VROCP2OXY2 + HO ----> HO +    0.044\*VROCN2OXY8 +    0.173\*VROCN2OXY4 +    0.010\*VROCN2OXY2 +    0.051\*VROCN1OXY6 +    0.112\*VROCN1OXY3 +    0.001\*VROCN1OXY1 +    0.134\*VROCP0OXY4 +    0.040\*VROCP0OXY2 +    0.051\*VROCP1OXY3 +    0.007\*VROCP1OXY1 +    0.024\*VROCP2OXY2 +    0.029\*VROCP3OXY2 +    0.073\*VROCP4OXY2 +    0.052\*VROCP5OXY1 +    0.059\*VROCP6OXY1 +    0.004\*OP3 +    0.002\*OP2 +    0.063\*DCB1 +    0.063\*HKET +    0.063\*MEK +    0.063\*ACD +    0.063\*ALD +    0.063\*MO2 +    0.063\*ETHP +    0.063\*HC3P +    0.063\*MEKP +    0.149\*HC5P  |   3.9300E-11 |   3.9300E-11 |
| ROCOXY12c   | VROCP3OXY2 + HO ----> HO +    0.032\*VROCN2OXY8 +    0.076\*VROCN2OXY4 +    0.001\*VROCN2OXY2 +    0.053\*VROCN1OXY6 +    0.049\*VROCN1OXY3 +    0.155\*VROCP0OXY4 +    0.015\*VROCP0OXY2 +    0.105\*VROCP1OXY3 +    0.001\*VROCP1OXY1 +    0.053\*VROCP2OXY2 +    0.009\*VROCP3OXY2 +    0.043\*VROCP4OXY2 +    0.058\*VROCP5OXY1 +    0.066\*VROCP6OXY1 +    0.051\*OP3 +    0.011\*OP2 +    0.070\*DCB1 +    0.070\*HKET +    0.070\*MEK +    0.070\*ACD +    0.070\*ALD +    0.070\*MO2 +    0.070\*ETHP +    0.070\*HC3P +    0.070\*MEKP +    0.166\*HC5P  |   3.5200E-11 |   3.5200E-11 |
| ROCOXY13c   | VROCP4OXY2 + HO ----> HO +    0.012\*VROCN2OXY8 +    0.017\*VROCN2OXY4 +    0.048\*VROCN1OXY6 +    0.025\*VROCN1OXY3 +    0.088\*VROCP0OXY4 +    0.092\*VROCP1OXY3 +    0.007\*VROCP1OXY1 +    0.097\*VROCP2OXY2 +    0.046\*VROCP3OXY2 +    0.002\*VROCP4OXY2 +    0.048\*VROCP5OXY1 +    0.074\*VROCP6OXY1 +    0.061\*OP3 +    0.015\*OP2 +    0.079\*DCB1 +    0.079\*HKET +    0.079\*MEK +    0.079\*ACD +    0.079\*ALD +    0.079\*MO2 +    0.079\*ETHP +    0.079\*HC3P +    0.079\*MEKP +    0.173\*HC5P  |   3.1200E-11 |   3.1200E-11 |
| ROCOXY14c   | VROCP5OXY1 + HO ----> HO +    0.010\*VROCN2OXY4 +    0.001\*VROCN2OXY2 +    0.009\*VROCN1OXY6 +    0.015\*VROCN1OXY3 +    0.070\*VROCP0OXY4 +    0.015\*VROCP0OXY2 +    0.104\*VROCP1OXY3 +    0.003\*VROCP1OXY1 +    0.165\*VROCP2OXY2 +    0.157\*VROCP3OXY2 +    0.072\*VROCP4OXY2 +    0.006\*VROCP5OXY1 +    0.140\*VROCP6OXY1 +    0.022\*OP3 +    0.038\*OP2 +    0.053\*DCB1 +    0.053\*HKET +    0.053\*MEK +    0.053\*ACD +    0.053\*ALD +    0.053\*MO2 +    0.053\*ETHP +    0.053\*HC3P +    0.053\*MEKP +    0.128\*HC5P  |   2.4000E-11 |   2.4000E-11 |
| ROCOXY15c   | VROCP6OXY1 + HO ----> HO +    0.006\*VROCN1OXY6 +    0.005\*VROCN1OXY3 +    0.022\*VROCP0OXY4 +    0.050\*VROCP1OXY3 +    0.002\*VROCP1OXY1 +    0.088\*VROCP2OXY2 +    0.138\*VROCP3OXY2 +    0.146\*VROCP4OXY2 +    0.043\*VROCP5OXY1 +    0.096\*VROCP6OXY1 +    0.032\*OP3 +    0.059\*OP2 +    0.057\*DCB1 +    0.057\*HKET +    0.057\*MEK +    0.057\*ACD +    0.057\*ALD +    0.057\*MO2 +    0.057\*ETHP +    0.057\*HC3P +    0.057\*MEKP +    0.154\*HC5P  |   2.0500E-11 |   2.0500E-11 |
| ROCOXY16c   | OP3 + HO ----> HO +    0.119\*VROCN2OXY8 +    0.001\*VROCN2OXY4 +    0.039\*VROCN1OXY6 +    0.011\*VROCP0OXY4 +    0.227\*DCB1 +    0.227\*MEK +    0.227\*ACD +    0.227\*ALD +    0.227\*MO2 +    0.227\*ETHP +    0.227\*HC3P +    0.227\*MEKP  |   4.6900E-11 |   4.6900E-11 |

<sup>0</sup>Units molecules/(sec*cm<sup>3</sup>); Value at 298.15 K;   2.4615E+19 molecules/cm<sup>3</sup>;   1.00 Atm.     
<sup>1</sup>Photolysis Reaction;depends on radiation and predicted concentrations     
<sup>2</sup>Heterogeneous Reaction; depends on predicted concentrations                
<sup>4</sup>Set to zero if sun is below the horizon. SEAWATER equals surface fraction covered by ice free open ocean plus surf zones. P equals air pressure in atmospheres.         
<sup>8</sup>Rate constant scaled as reverse equilibrium to constant for listed reaction    
