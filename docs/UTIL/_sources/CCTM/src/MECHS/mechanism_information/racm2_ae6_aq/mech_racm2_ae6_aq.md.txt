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
| R013   | ALD ----> HO2 + ETHP + CO  | ALD_RACM2 | Not Available<sup>1</sup> | 
| R014   | ACT ----> MO2 + ACO3  | CH3COCH3_RACM2 | Not Available<sup>1</sup> | 
| R015   | UALD ---->   1.220\*HO2 +    0.784\*ACO3 +    1.220\*CO +    0.350\*HCHO +    0.434\*ALD +    0.216\*KET  | UALD_RACM2 | Not Available<sup>1</sup> | 
| R016   | MEK ---->   0.500\*MO2 +    0.500\*ETHP + ACO3  | MEK_RACM2 | Not Available<sup>1</sup> | 
| R017   | KET ----> ETHP + ACO3  | KET_RACM2 | Not Available<sup>1</sup> | 
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
| R030   | PAA ----> HO + MO2  | PAA_RACM2 | Not Available<sup>1</sup> | 
| R031   | ONIT ----> HO2 + NO2 +    0.200\*ALD +    0.800\*KET  | ONIT_RACM2 | Not Available<sup>1</sup> | 
| R032   | PAN ----> ACO3 + NO2  | PAN1_RACM2 | Not Available<sup>1</sup> | 
| R033   | PAN ----> MO2 + NO3  | PAN2_RACM2 | Not Available<sup>1</sup> | 
| R034   | O3 + HO ----> HO2  |   1.70E-12e<sup>  -940.00/T</sup> |   7.2647E-14 |
| R035   | O3 + HO2 ----> HO  |   1.00E-14e<sup>  -490.00/T</sup> |   1.9331E-15 |
| R036   | O3 + NO ----> NO2  |   1.40E-12e<sup> -1310.00/T</sup> |   1.7296E-14 |
| R037   | O3 + NO2 ----> NO3  |   1.40E-13e<sup> -2470.00/T</sup> |   3.5339E-17 |
| R038   | O3P + O2 + M ----> O3  |   5.74E-34(T/300)<sup> -2.60</sup> |   5.8331E-34 |
| R039   | O3P + O3 ----> |   8.00E-12e<sup> -2060.00/T</sup> |   7.9879E-15 |
| R040   | O1D + O2 ----> O3P  |   3.30E-11e<sup>    67.00/T</sup> |   4.1315E-11 |
| R041   | O1D + N2 ----> O3P  |   2.00E-11e<sup>   130.00/T</sup> |   3.0931E-11 |
| R042   | O1D + H2O ---->   2.000\*HO  |   2.1400E-10 |   2.1400E-10 |
| R043   | HO + H2 ----> HO2  |   7.70E-12e<sup> -2100.00/T</sup> |   6.7230E-15 |
| R044   | HO + HO2 ----> |   4.80E-11e<sup>   250.00/T</sup> |   1.1102E-10 |
| R045   | HO2 + HO2 ----> H2O2  | k<sub>0</sub>=  2.20E-13e<sup>   600.0/T</sup><br>k<sub>1</sub>=  1.90E-33e<sup>   980.0/T</sup> |   2.8975E-12 |
| R046   | HO2 + HO2 + H2O ----> H2O2  | k<sub>0</sub>=  3.08E-34e<sup>  2800.0/T</sup><br>k<sub>1</sub>=  2.59E-54e<sup>  3180.0/T</sup> |   6.4234E-30 |
| R047   | H2O2 + HO ----> HO2  |   2.90E-12e<sup>  -160.00/T</sup> |   1.6957E-12 |
| R048   | NO + O3P ----> NO2  | k<sub>o</sub>=  9.00E-32e<sup>     0.0/T</sup>(T/300)<sup> -1.50</sup><br>k<sub>i</sub> =   3.00E-11e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.00;F=     0.60 |   1.6618E-12 |
| R049   | NO + HO ----> HONO  | k<sub>o</sub>=  7.00E-31e<sup>     0.0/T</sup>(T/300)<sup> -2.60</sup><br>k<sub>i</sub> =   3.60E-11e<sup>     0.0/T</sup>(T/300)<sup> -0.10</sup><br>n=     1.00;F=     0.60 |   7.3998E-12 |
| R050   | NO + HO2 ----> NO2 + HO  |   3.45E-12e<sup>   270.00/T</sup> |   8.5332E-12 |
| R051   | NO + HO2 ----> HNO3  | k<sub>0</sub>=  6.0950E-14e<sup>   270.0/T</sup>(T/300)<sup> -1.00</sup><br>k<sub>2</sub>=  6.8570E-34e<sup>   270.0/T</sup>(T/300)<sup>  1.00</sup><br>k<sub>3</sub>= -5.9680E-14e<sup>   270.00/T</sup> |   4.5566E-14 |
| R052   | NO + NO + O2 ---->   2.000\*NO2  |   3.30E-39e<sup>   530.00/T</sup> |   1.9522E-38 |
| R053   | HONO + HO ----> NO2  |   2.50E-12e<sup>   260.00/T</sup> |   5.9795E-12 |
| R054   | NO2 + O3P ----> NO  |   5.50E-12e<sup>   188.00/T</sup> |   1.0333E-11 |
| R055   | NO2 + O3P ----> NO3  | k<sub>o</sub>=  2.50E-31e<sup>     0.0/T</sup>(T/300)<sup> -1.80</sup><br>k<sub>i</sub> =   2.20E-11e<sup>     0.0/T</sup>(T/300)<sup> -0.70</sup><br>n=     1.00;F=     0.60 |   3.2805E-12 |
| R056   | NO2 + HO ----> HNO3  | k<sub>o</sub>=  1.51E-30e<sup>     0.0/T</sup>(T/300)<sup> -3.00</sup><br>k<sub>i</sub> =   2.58E-11e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.00;F=     0.60 |   9.3347E-12 |
| R057   | HNO3 + HO ----> NO3  | k<sub>0</sub>=  2.40E-14e<sup>   460.0/T</sup><br>k<sub>1</sub>=  2.70E-17e<sup>  2199.0/T</sup><br>k<sub>3</sub>=  6.50E-34e<sup>  1335.0/T</sup> |   1.5409E-13 |
| R058   | NO3 + HO ----> HO2 + NO2  |   2.0000E-11 |   2.0000E-11 |
| R059   | NO3 + HO2 ---->   0.700\*HO +    0.700\*NO2 +    0.300\*HNO3  |   4.0000E-12 |   4.0000E-12 |
| R060   | NO3 + NO ---->   2.000\*NO2  |   1.80E-11e<sup>   110.00/T</sup> |   2.6032E-11 |
| R061   | NO3 + NO2 ----> NO + NO2  |   4.50E-14e<sup> -1260.00/T</sup> |   6.5744E-16 |
| R062   | NO3 + NO3 ---->   2.000\*NO2  |   8.50E-13e<sup> -2450.00/T</sup> |   2.2944E-16 |
| R063   | NO3 + NO2 ----> N2O5  | k<sub>o</sub>=  2.00E-30e<sup>     0.0/T</sup>(T/300)<sup> -4.40</sup><br>k<sub>i</sub> =   1.40E-12e<sup>     0.0/T</sup>(T/300)<sup> -0.70</sup><br>n=     1.00;F=     0.60 |   1.1783E-12 |
| R064   | N2O5 ----> NO2 + NO3  |   3.70E+26e<sup>-11000.00/T</sup> \*R063 |   4.1396E-02<sup>8</sup>| 
| R065   | N2O5 + H2O ---->   2.000\*HNO3  |   1.0000E-22 |   1.0000E-22 |
| R066   | NO2 + HO2 ----> HNO4  | k<sub>o</sub>=  2.00E-31e<sup>     0.0/T</sup>(T/300)<sup> -3.40</sup><br>k<sub>i</sub> =   2.90E-12e<sup>     0.0/T</sup>(T/300)<sup> -1.10</sup><br>n=     1.00;F=     0.60 |   1.1385E-12 |
| R067   | HNO4 ----> HO2 + NO2  |   4.76E+26e<sup>-10900.00/T</sup> \*R066 |   7.1920E-02<sup>8</sup>| 
| R068   | HNO4 + HO ----> NO2  |   1.30E-12e<sup>   380.00/T</sup> |   4.6501E-12 |
| R069   | SO2 + HO ----> HO2 + SULF + SULRXN  | k<sub>o</sub>=  3.30E-31e<sup>     0.0/T</sup>(T/300)<sup> -4.30</sup><br>k<sub>i</sub> =   1.60E-12e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.00;F=     0.60 |   9.5810E-13 |
| R070   | CO + HO ----> HO2  | k<sub>0</sub>=  1.44E-13e<sup>     0.0/T</sup><br>k<sub>1</sub>=  2.88E-33e<sup>     0.0/T</sup> |   2.1489E-13 |
| R071   | HO + CH4 ----> MO2  |   1.85E-12e<sup> -1690.00/T</sup> |   6.3895E-15 |
| R072   | ETH + HO ----> ETHP  |   6.90E-12e<sup> -1000.00/T</sup> |   2.4111E-13 |
| R073   | HC3 + HO ----> HC3P  |   7.68E-12e<sup>  -370.00/T</sup> |   2.2203E-12 |
| R074   | HC5 + HO ----> HC5P  |   1.01E-11e<sup>  -245.00/T</sup> |   4.4407E-12 |
| R075   | HC8 + HO ---->   0.049\*HO2 +    0.951\*HC8P +    0.025\*ALD +    0.024\*HKET  |   2.82E-11e<sup>  -273.00/T</sup> |   1.1287E-11 |
| R076   | ETE + HO ----> ETEP  | k<sub>o</sub>=  1.00E-28e<sup>     0.0/T</sup>(T/300)<sup> -4.50</sup><br>k<sub>i</sub> =   8.80E-12e<sup>     0.0/T</sup>(T/300)<sup> -0.85</sup><br>n=     1.00;F=     0.60 |   8.1981E-12 |
| R077   | OLT + HO ----> OLTP  |   5.72E-12e<sup>   500.00/T</sup> |   3.0599E-11 |
| R078   | OLI + HO ----> OLIP  |   1.33E-11e<sup>   500.00/T</sup> |   7.1149E-11 |
| R079   | DIEN + HO ----> OLIP  |   1.48E-11e<sup>   448.00/T</sup> |   6.6502E-11 |
| R080   | ACE + HO ---->   0.650\*HO +    0.350\*HO2 +    0.350\*CO +    0.650\*GLY +    0.350\*ORA1  | k<sub>o</sub>=  5.50E-30e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>k<sub>i</sub> =   8.30E-13e<sup>     0.0/T</sup>(T/300)<sup>  2.00</sup><br>n=     1.00;F=     0.60 |   7.4748E-13 |
| R081   | BENZENE + HO ---->   0.648\*HO2 +    0.352\*BENP +    0.118\*EPX +    0.530\*PHEN + BENZRO2  |   2.33E-12e<sup>  -193.00/T</sup> |   1.2196E-12 |
| R082   | TOL + HO ---->   0.177\*HO2 +    0.763\*TR2 +    0.060\*TLP1 +    0.177\*CSL + TOLRO2  |   1.81E-12e<sup>   354.00/T</sup> |   5.9337E-12 |
| R083   | XYM + HO ---->   0.177\*HO2 +    0.763\*XY2 +    0.060\*XYL1 +    0.177\*CSL +    0.980\*XYLRO2  |   2.3100E-11 |   2.3100E-11 |
| R084   | XYP + HO ---->   0.177\*HO2 +    0.763\*XY2 +    0.060\*XYL1 +    0.177\*CSL + XYLRO2  |   1.4300E-11 |   1.4300E-11 |
| R085   | XYO + HO ---->   0.177\*HO2 +    0.763\*XYO2 +    0.060\*XYL1 +    0.177\*CSL + XYLRO2  |   1.3600E-11 |   1.3600E-11 |
| R086   | ISO + HO ----> ISOP + ISOPRXN  |   2.70E-11e<sup>   390.00/T</sup> |   9.9873E-11 |
| R087   | API + HO ----> APIP + TRPRXN  |   1.21E-11e<sup>   440.00/T</sup> |   5.2930E-11 |
| R088   | LIM + HO ----> LIMP + TRPRXN  |   4.20E-11e<sup>   401.00/T</sup> |   1.6120E-10 |
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
| R105   | PHEN + HO ---->   0.730\*HO2 +    0.200\*ADDC +    0.070\*CHO +    0.730\*MCT  |   6.75E-12e<sup>   405.00/T</sup> |   2.6257E-11 |
| R106   | CSL + HO ---->   0.730\*HO2 +    0.200\*ADDC +    0.070\*CHO +    0.730\*MCT  |   4.65E-11e<sup>     0.00/T</sup> |   4.6500E-11 |
| R107   | EPX + HO ----> HO2 + XO2 + CO + ALD  |   2.80E-11e<sup>   175.00/T</sup> |   5.0358E-11 |
| R108   | MCT + HO ----> MCTO  |   2.05E-10e<sup>     0.00/T</sup> |   2.0500E-10 |
| R109   | MOH + HO ----> HO2 + HCHO  |   2.85E-12e<sup>  -345.00/T</sup> |   8.9600E-13 |
| R110   | EOH + HO ----> HO2 + ACD  |   3.00E-12e<sup>    20.00/T</sup> |   3.2081E-12 |
| R111   | ROH + HO ----> HO2 +    0.719\*ALD +    0.184\*ACD  |   2.60E-12e<sup>   200.00/T</sup> |   5.0851E-12 |
| R112   | ETEG + HO ----> HO2 + ALD  |   1.4700E-11 |   1.4700E-11 |
| R113   | OP1 + HO ---->   0.350\*HO +    0.650\*MO2 +    0.350\*HCHO  |   2.90E-12e<sup>   190.00/T</sup> |   5.4848E-12 |
| R114   | OP2 + HO ---->   0.010\*HO +    0.440\*HC3P +    0.070\*XO2 +    0.080\*ALD +    0.410\*KET  |   3.40E-12e<sup>   190.00/T</sup> |   6.4304E-12 |
| R115   | ISHP + HO ----> HO + MACR +    0.904\*IEPOX  |   1.0000E-10 |   1.0000E-10 |
| R116   | MAHP + HO ----> MACP  |   3.0000E-11 |   3.0000E-11 |
| R117   | ORA1 + HO ----> HO2  |   4.5000E-13 |   4.5000E-13 |
| R118   | ORA2 + HO ---->   0.640\*MO2 +    0.360\*ORAP  |   4.00E-14e<sup>   850.00/T</sup> |   6.9214E-13 |
| R119   | PAA + HO ---->   0.350\*HO +    0.650\*ACO3 +    0.350\*XO2 +    0.350\*HCHO  |   2.93E-12e<sup>   190.00/T</sup> |   5.5415E-12 |
| R120   | PAN + HO ----> XO2 + NO3 + HCHO  |   4.0000E-14 |   4.0000E-14 |
| R121   | PPN + HO ----> XO2 + NO3 + HCHO  |   4.0000E-14 |   4.0000E-14 |
| R122   | MPAN + HO ----> NO2 + HKET  |   3.2000E-11 |   3.2000E-11 |
| R123   | ONIT + HO ----> HC3P + NO2  |   5.31E-12e<sup>  -260.00/T</sup> |   2.2201E-12 |
| R124   | NALD + HO ----> NO2 + XO2 + HKET  |   5.60E-12e<sup>   270.00/T</sup> |   1.3851E-11 |
| R125   | ISON + HO ----> NALD +    0.070\*HKET +    0.070\*HCHO  |   1.3000E-11 |   1.3000E-11 |
| R126   | ETE + O3 ---->   0.080\*HO +    0.150\*HO2 +    0.430\*CO + HCHO +    0.370\*ORA1  |   9.14E-15e<sup> -2580.00/T</sup> |   1.5953E-18 |
| R127   | OLT + O3 ---->   0.220\*HO +    0.320\*HO2 +    0.080\*MO2 +    0.060\*ETHP +    0.040\*HC3P +    0.020\*HC5P +    0.068\*H2O2 +    0.430\*CO +    0.020\*ETH +    0.015\*HC3 +    0.006\*HC5 +    0.032\*BENZENE +    0.560\*HCHO +    0.010\*ACD +    0.440\*ALD +    0.030\*ACT +    0.020\*BALD +    0.060\*MEK +    0.010\*HKET +    0.030\*ORA1 +    0.060\*ORA2  |   4.33E-15e<sup> -1800.00/T</sup> |   1.0341E-17 |
| R128   | OLI + O3 ---->   0.460\*HO +    0.070\*HO2 +    0.320\*MO2 +    0.070\*ETHP +    0.040\*HC3P +    0.090\*ACO3 +    0.370\*CO +    0.026\*H2O2 +    0.010\*ETH +    0.010\*HC3 +    0.090\*HCHO +    0.457\*ACD +    0.730\*ALD +    0.110\*ACT +    0.017\*KET +    0.044\*HKET +    0.017\*ORA2  |   4.40E-15e<sup>  -845.00/T</sup> |   2.5858E-16 |
| R129   | DIEN + O3 ---->   0.090\*O3P +    0.280\*HO +    0.300\*HO2 +    0.030\*MO2 +    0.150\*ACO3 +    0.020\*KETP +    0.130\*XO2 +    0.001\*H2O2 +    0.360\*CO +    0.350\*OLT +    0.900\*HCHO +    0.390\*MACR +    0.150\*ORA1  |   1.34E-14e<sup> -2283.00/T</sup> |   6.3331E-18 |
| R130   | ISO + O3 ---->   0.250\*HO +    0.250\*HO2 +    0.080\*MO2 +    0.100\*ACO3 +    0.100\*MACP +    0.090\*H2O2 +    0.140\*CO +    0.580\*HCHO +    0.461\*MACR +    0.189\*MVK +    0.280\*ORA1 +    0.153\*OLT  |   7.86E-15e<sup> -1913.00/T</sup> |   1.2850E-17 |
| R131   | API + O3 ---->   0.850\*HO +    0.100\*HO2 +    0.200\*ETHP +    0.420\*KETP +    0.020\*H2O2 +    0.140\*CO +    0.650\*ALD +    0.530\*KET + TRPRXN  |   5.00E-16e<sup>  -530.00/T</sup> |   8.4519E-17 |
| R132   | LIM + O3 ---->   0.850\*HO +    0.100\*HO2 +    0.160\*ETHP +    0.420\*KETP +    0.020\*H2O2 +    0.140\*CO +    0.460\*OLT +    0.040\*HCHO +    0.790\*MACR +    0.010\*ORA1 +    0.070\*ORA2 + TRPRXN  |   2.95E-15e<sup>  -783.00/T</sup> |   2.1344E-16 |
| R133   | MACR + O3 ---->   0.190\*HO +    0.140\*HO2 +    0.100\*ACO3 +    0.220\*CO +    0.500\*MGLY +    0.450\*ORA1  |   1.36E-15e<sup> -2112.00/T</sup> |   1.1406E-18 |
| R134   | MVK + O3 ---->   0.160\*HO +    0.110\*HO2 +    0.280\*ACO3 +    0.010\*XO2 +    0.560\*CO +    0.100\*HCHO +    0.540\*MGLY +    0.070\*ORA1 +    0.070\*ORA2 +    0.100\*ALD  |   8.50E-16e<sup> -1520.00/T</sup> |   5.1921E-18 |
| R135   | UALD + O3 ---->   0.100\*HO +    0.072\*HO2 +    0.008\*MO2 +    0.002\*ACO3 +    0.100\*XO2 +    0.243\*CO +    0.080\*HCHO +    0.420\*ACD +    0.028\*KET +    0.491\*GLY +    0.003\*MGLY +    0.044\*ORA1  |   1.6600E-18 |   1.6600E-18 |
| R136   | DCB1 + O3 ---->   0.050\*HO + HO2 +    0.600\*RCO3 +    0.600\*XO2 +    1.500\*CO +    0.050\*HCHO +    0.050\*GLY +    0.080\*MGLY +    0.650\*OP2  |   2.0000E-16 |   2.0000E-16 |
| R137   | DCB2 + O3 ---->   0.050\*HO + HO2 +    0.600\*RCO3 +    0.600\*XO2 +    1.500\*CO +    0.050\*HCHO +    0.050\*GLY +    0.080\*MGLY +    0.700\*DCB1 +    0.650\*OP2  |   2.0000E-16 |   2.0000E-16 |
| R138   | DCB3 + O3 ---->   0.050\*HO + HO2 +    1.500\*CO +    0.480\*GLY +    0.700\*DCB1 +    0.250\*ORA1 +    0.250\*ORA2 +    0.110\*PAA  |   9.0000E-17 |   9.0000E-17 |
| R139   | EPX + O3 ---->   0.050\*HO +    1.500\*HO2 +    1.500\*CO +    0.850\*BALD + GLY  |   5.0000E-16 |   5.0000E-16 |
| R140   | MCTO + O3 ----> MCTP  |   2.8600E-13 |   2.8600E-13 |
| R141   | ETE + NO3 ---->   0.800\*OLNN +    0.200\*OLND  |   4.39E-13e<sup> -2282.00/T</sup>(T/300)<sup>  2.00 </sup> |   2.0571E-16 |
| R142   | OLT + NO3 ---->   0.430\*OLNN +    0.570\*OLND  |   1.79E-13e<sup>  -450.00/T</sup> |   3.9570E-14 |
| R143   | OLI + NO3 ---->   0.110\*OLNN +    0.890\*OLND  |   8.64E-13e<sup>   450.00/T</sup> |   3.9084E-12 |
| R144   | DIEN + NO3 ---->   0.900\*OLNN +    0.100\*OLND +    0.900\*MACR  |   1.0000E-13 |   1.0000E-13 |
| R145   | ISO + NO3 ----> ISON + ISOPRXN  |   3.03E-12e<sup>  -446.00/T</sup> |   6.7887E-13 |
| R146   | API + NO3 ---->   0.100\*OLNN +    0.900\*OLND + TRPRXN  |   1.19E-12e<sup>   490.00/T</sup> |   6.1560E-12 |
| R147   | LIM + NO3 ---->   0.710\*OLNN +    0.290\*OLND + TRPRXN  |   1.2200E-11 |   1.2200E-11 |
| R148   | HCHO + NO3 ----> HO2 + CO + HNO3  |   2.00E-12e<sup> -2440.00/T</sup> |   5.5828E-16 |
| R149   | ACD + NO3 ----> ACO3 + HNO3  |   1.40E-12e<sup> -1900.00/T</sup> |   2.3907E-15 |
| R150   | ALD + NO3 ----> RCO3 + HNO3  |   3.76E-12e<sup> -1900.00/T</sup> |   6.4208E-15 |
| R151   | MACR + NO3 ---->   0.680\*HCHO +    0.320\*MACP +    0.680\*XO2 +    0.680\*MGLY +    0.320\*HNO3 +    0.680\*NO2  |   3.4000E-15 |   3.4000E-15 |
| R152   | UALD + NO3 ----> HO2 + XO2 +    0.668\*CO +    0.332\*HCHO +    0.332\*ALD + ONIT  |   5.02E-13e<sup> -1076.00/T</sup> |   1.3595E-14 |
| R153   | GLY + NO3 ----> HO2 +    2.000\*CO + HNO3  |   2.90E-12e<sup> -1900.00/T</sup> |   4.9522E-15 |
| R154   | MGLY + NO3 ----> ACO3 + CO + HNO3  |   3.76E-12e<sup> -1900.00/T</sup> |   6.4208E-15 |
| R155   | PHEN + NO3 ---->   0.400\*CHO +    0.100\*ADDC +    0.500\*ADCN +    0.500\*HNO3  |   3.7800E-12 |   3.7800E-12 |
| R156   | CSL + NO3 ---->   0.400\*CHO +    0.100\*ADDC +    0.500\*ADCN +    0.500\*HNO3  |   1.0600E-12 |   1.0600E-12 |
| R157   | EPX + NO3 ---->   0.500\*HO +    1.500\*HO2 +    1.500\*CO + GLY +    0.500\*NO2 +    0.500\*HNO3  |   2.87E-13e<sup> -1000.00/T</sup> |   1.0029E-14 |
| R158   | MCT + NO3 ----> MCTO + HNO3  |   2.0100E-10 |   2.0100E-10 |
| R159   | MPAN + NO3 ----> MACP + NO2  |   2.20E-14e<sup>  -500.00/T</sup> |   4.1125E-15 |
| R160   | TR2 ---->   0.280\*HO +    0.290\*HO2 +    0.280\*TOLP +    0.150\*PER1 +    0.280\*DCB2 +    0.010\*CSL +    0.280\*EPX  |   1.0000E+03 |   1.0000E+03 |
| R161   | TOLP ---->   0.490\*HO +    0.010\*HO2 +    0.500\*PER1 +    0.490\*DCB2 +    0.010\*CSL  |   1.0000E+03 |   1.0000E+03 |
| R162   | XY2 ---->   0.158\*HO +    0.308\*HO2 +    0.250\*RCO3 +    0.308\*XYLP +    0.150\*PER2 +    0.224\*DCB2 +    0.010\*CSL +    0.840\*EPX  |   1.0000E+03 |   1.0000E+03 |
| R163   | XYLP ---->   0.390\*HO +    0.010\*HO2 +    0.300\*PER2 +    0.490\*DCB2 +    0.010\*CSL  |   1.0000E+03 |   1.0000E+03 |
| R164   | XYO2 ---->   0.158\*HO +    0.308\*HO2 +    0.250\*RCO3 +    0.150\*PER2 +    0.308\*XYOP +    0.224\*DCB2 +    0.010\*CSL +    0.840\*EPX  |   1.0000E+03 |   1.0000E+03 |
| R165   | XYOP ---->   0.390\*HO +    0.010\*HO2 +    0.500\*PER2 +    0.490\*DCB2 +    0.010\*CSL  |   1.0000E+03 |   1.0000E+03 |
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
| R176   | HC8P + NO ---->   0.606\*HO2 +    0.133\*ETHP +    0.416\*XO2 +    0.739\*NO2 +    0.150\*ALD +    0.642\*KET +    0.261\*ONIT  |   4.0000E-12 |   4.0000E-12 |
| R177   | ETEP + NO ----> HO2 + NO2 +    1.600\*HCHO +    0.200\*ALD  |   9.0000E-12 |   9.0000E-12 |
| R178   | OLTP + NO ---->   0.780\*HO2 +    0.970\*NO2 +    0.780\*HCHO +    0.012\*ACD +    0.440\*ALD +    0.060\*ACT +    0.130\*MEK +    0.030\*ONIT  |   4.0000E-12 |   4.0000E-12 |
| R179   | OLIP + NO ---->   0.830\*HO2 +    0.950\*NO2 +    0.810\*ACD +    0.680\*ALD +    0.200\*ACT +    0.090\*KET +    0.020\*HKET +    0.050\*ONIT  |   4.0000E-12 |   4.0000E-12 |
| R180   | BENP + NO ---->   0.918\*HO2 +    0.918\*NO2 +    0.459\*DCB2 +    0.459\*DCB3 +    0.918\*GLY +    0.082\*ONIT  |   2.54E-12e<sup>   360.00/T</sup> |   8.4961E-12 |
| R181   | TLP1 + NO ----> NO2 + BALD  |   4.0000E-12 |   4.0000E-12 |
| R182   | TOLP + NO ---->   0.950\*HO2 +    0.950\*NO2 +    0.950\*DCB2 +    0.050\*ONIT  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R183   | PER1 + NO ---->   0.500\*HO2 +    0.950\*NO2 +    0.500\*BALD +    0.500\*MGLY +    0.500\*DCB1 +    0.050\*ONIT  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R184   | XYL1 + NO ----> NO2 + BALD  |   4.0000E-12 |   4.0000E-12 |
| R185   | XYLP + NO ---->   0.950\*HO2 +    0.950\*NO2 +    0.950\*DCB3 +    0.050\*ONIT  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R186   | PER2 + NO ---->   0.950\*HO2 +    0.950\*NO2 +    0.950\*MGLY +    0.950\*DCB1 +    1.050\*DCB3 +    0.050\*ONIT  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R187   | XYOP + NO ---->   0.950\*HO2 +    0.950\*NO2 +    0.350\*GLY +    0.600\*MGLY +    0.700\*DCB1 +    0.073\*DCB2 +    0.177\*DCB3 +    0.050\*ONIT  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R188   | ISOP + NO ---->   0.880\*HO2 +    0.880\*NO2 +    0.200\*HCHO +    0.280\*MACR +    0.440\*MVK +    0.120\*ISON +    0.021\*GLY +    0.029\*HKET +    0.027\*ALD  |   2.43E-12e<sup>   360.00/T</sup> |   8.1282E-12 |
| R189   | APIP + NO ---->   0.820\*HO2 +    0.820\*NO2 +    0.230\*HCHO +    0.430\*ALD +    0.110\*ACT +    0.440\*KET +    0.070\*ORA1 +    0.180\*ONIT  |   4.0000E-12 |   4.0000E-12 |
| R190   | LIMP + NO ----> HO2 +    0.680\*UALD +    0.430\*HCHO +    0.070\*ORA1 + NO2 +    0.050\*OLI  |   4.0000E-12 |   4.0000E-12 |
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
| R216   | HC8P + HO2 ----> OP2  |   1.66E-13e<sup>  1300.00/T</sup> |   1.2994E-11 |
| R217   | ETEP + HO2 ----> OP2  |   1.90E-13e<sup>  1300.00/T</sup> |   1.4872E-11 |
| R218   | OLTP + HO2 ----> OP2  |   1.66E-13e<sup>  1300.00/T</sup> |   1.2994E-11 |
| R219   | OLIP + HO2 ----> OP2  |   1.66E-13e<sup>  1300.00/T</sup> |   1.2994E-11 |
| R220   | BENP + HO2 ----> OP2  |   2.91E-13e<sup>  1300.00/T</sup> |   2.2778E-11 |
| R221   | TLP1 + HO2 ----> OP2  |   3.75E-13e<sup>   980.00/T</sup> |   1.0035E-11 |
| R222   | TOLP + HO2 ----> OP2  |   3.75E-13e<sup>   980.00/T</sup> |   1.0035E-11 |
| R223   | PER1 + HO2 ----> OP2  |   3.75E-13e<sup>   980.00/T</sup> |   1.0035E-11 |
| R224   | XYL1 + HO2 ----> OP2  |   3.75E-13e<sup>   980.00/T</sup> |   1.0035E-11 |
| R225   | XYLP + HO2 ----> OP2  |   3.75E-13e<sup>   980.00/T</sup> |   1.0035E-11 |
| R226   | PER2 + HO2 ----> OP2  |   3.75E-13e<sup>   980.00/T</sup> |   1.0035E-11 |
| R227   | XYOP + HO2 ----> OP2  |   3.75E-13e<sup>   980.00/T</sup> |   1.0035E-11 |
| R228   | ISOP + HO2 ----> ISHP  |   2.05E-13e<sup>  1300.00/T</sup> |   1.6046E-11 |
| R229   | APIP + HO2 ----> OP2  |   1.5000E-11 |   1.5000E-11 |
| R230   | LIMP + HO2 ----> OP2  |   1.5000E-11 |   1.5000E-11 |
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
| R252   | HC8P + MO2 ---->   0.910\*HO2 +    0.090\*ETHP +    0.281\*XO2 +    0.750\*HCHO +    0.197\*ALD +    0.652\*KET +    0.250\*MOH +    0.250\*ROH  |   4.34E-14e<sup>   633.00/T</sup> |   3.6269E-13 |
| R253   | ETEP + MO2 ----> HO2 +    1.950\*HCHO +    0.150\*ALD +    0.250\*MOH +    0.250\*ETEG  |   1.71E-13e<sup>   708.00/T</sup> |   1.8378E-12 |
| R254   | OLTP + MO2 ----> HO2 +    1.500\*HCHO +    0.705\*ALD +    0.045\*KET +    0.250\*MOH +    0.250\*ROH  |   1.46E-13e<sup>   708.00/T</sup> |   1.5691E-12 |
| R255   | OLIP + MO2 ----> HO2 +    0.750\*HCHO +    1.280\*ALD +    0.218\*KET +    0.250\*MOH +    0.250\*ROH  |   9.18E-14e<sup>   708.00/T</sup> |   9.8659E-13 |
| R256   | BENP + MO2 ---->   1.600\*HO2 +    0.459\*DCB3 + HCHO +    0.459\*DCB2 +    0.600\*GLY  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| R257   | TLP1 + MO2 ----> HO2 + HCHO + BALD  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| R258   | TOLP + MO2 ---->   2.000\*HO2 + HCHO +    0.271\*GLY + DCB2  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| R259   | PER1 + MO2 ---->   2.000\*HO2 + HCHO + MGLY + DCB1  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| R260   | XYL1 + MO2 ----> HO2 + HCHO + BALD  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| R261   | XYLP + MO2 ---->   2.000\*HO2 + HCHO + DCB2  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| R262   | PER2 + MO2 ---->   2.000\*HO2 + HCHO + MGLY + DCB1 +    1.050\*DCB3  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| R263   | XYOP + MO2 ---->   2.000\*HO2 + HCHO +    0.368\*GLY +    0.632\*MGLY +    0.737\*DCB1 +    0.077\*DCB2 +    0.186\*DCB3  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| R264   | ISOP + MO2 ----> HO2 +    1.310\*HCHO +    0.159\*MACR +    0.250\*MVK +    0.250\*MOH +    0.250\*ROH +    0.023\*ALD +    0.018\*GLY +    0.016\*HKET  |   3.40E-14e<sup>   221.00/T</sup> |   7.1350E-14 |
| R265   | APIP + MO2 ----> HO2 +    0.750\*HCHO +    0.750\*ALD +    0.750\*KET +    0.250\*MOH +    0.250\*ROH  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| R266   | LIMP + MO2 ----> HO2 +    1.040\*HCHO +    0.192\*OLI +    0.308\*MACR +    0.250\*MOH +    0.250\*ROH  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
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
| R288   | HC8P + ACO3 ---->   0.303\*HO2 +    0.500\*MO2 +    0.067\*ETHP +    0.208\*XO2 +    0.217\*ALD +    0.642\*KET +    0.495\*ORA2  |   2.47E-13e<sup>   683.00/T</sup> |   2.4410E-12 |
| R289   | ETEP + ACO3 ---->   0.500\*HO2 +    0.500\*MO2 +    1.600\*HCHO +    0.200\*ALD +    0.500\*ORA2  |   9.48E-13e<sup>   765.00/T</sup> |   1.2335E-11 |
| R290   | OLTP + ACO3 ---->   0.500\*HO2 +    0.500\*MO2 + HCHO +    0.940\*ALD +    0.060\*KET +    0.500\*ORA2  |   8.11E-13e<sup>   765.00/T</sup> |   1.0552E-11 |
| R291   | OLIP + ACO3 ---->   0.500\*HO2 +    0.500\*MO2 +    1.710\*ALD +    0.290\*KET +    0.500\*ORA2  |   5.09E-13e<sup>   765.00/T</sup> |   6.6228E-12 |
| R292   | BENP + ACO3 ---->   0.600\*HO2 + MO2 +    0.459\*DCB2 +    0.458\*DCB3 +    0.600\*GLY  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| R293   | TLP1 + ACO3 ----> MO2 + BALD  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| R294   | TOLP + ACO3 ----> HO2 + MO2 + DCB2  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| R295   | PER1 + ACO3 ----> HO2 + MO2 + MGLY + DCB1  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| R296   | XYL1 + ACO3 ----> MO2 + BALD  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| R297   | XYLP + ACO3 ----> HO2 + MO2 + DCB2  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| R298   | PER2 + ACO3 ----> HO2 + MO2 + MGLY + DCB1 +    1.050\*DCB3  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| R299   | XYOP + ACO3 ----> HO2 + MO2 +    0.368\*GLY +    0.632\*MGLY +    0.737\*DCB1 +    0.077\*DCB2 +    0.186\*DCB3  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| R300   | ISOP + ACO3 ---->   0.500\*HO2 +    0.500\*MO2 +    1.048\*HCHO +    0.219\*MACR +    0.305\*MVK +    0.500\*ORA2  |   8.40E-14e<sup>   221.00/T</sup> |   1.7628E-13 |
| R301   | APIP + ACO3 ---->   0.500\*HO2 +    0.500\*MO2 + ALD + KET + ORA2  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| R302   | LIMP + ACO3 ---->   0.500\*HO2 +    0.500\*MO2 +    0.192\*OLI +    0.385\*HCHO +    0.308\*MACR +    0.500\*ORA2  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
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
| R326   | HC8P + NO3 ---->   0.820\*HO2 +    0.180\*ETHP +    0.563\*XO2 + NO2 +    0.203\*ALD +    0.869\*KET  |   1.2000E-12 |   1.2000E-12 |
| R327   | ETEP + NO3 ----> HO2 + NO2 +    1.600\*HCHO +    0.200\*ALD  |   1.2000E-12 |   1.2000E-12 |
| R328   | OLTP + NO3 ---->   0.470\*ALD +    0.790\*HCHO +    0.790\*HO2 + NO2 +    0.180\*MEK +    0.020\*ACD +    0.090\*ACT  |   1.2000E-12 |   1.2000E-12 |
| R329   | OLIP + NO3 ---->   0.860\*HO2 +    0.720\*ALD +    0.110\*KET + NO2 +    0.200\*ACT +    0.850\*ACD +    0.040\*HKET  |   1.2000E-12 |   1.2000E-12 |
| R330   | BENP + NO3 ----> HO2 + NO2 +    0.500\*DCB2 +    0.500\*DCB3 + GLY  |   1.2000E-12 |   1.2000E-12 |
| R331   | TLP1 + NO3 ----> NO2 + BALD  |   1.2000E-12 |   1.2000E-12 |
| R332   | TOLP + NO3 ----> HO2 + NO2 + DCB2  |   1.2000E-12 |   1.2000E-12 |
| R333   | PER1 + NO3 ---->   0.500\*HO2 + NO2 +    0.500\*MGLY +    0.500\*DCB1 +    0.500\*BALD  |   1.2000E-12 |   1.2000E-12 |
| R334   | XYL1 + NO3 ----> NO2 + BALD  |   1.2000E-12 |   1.2000E-12 |
| R335   | XYLP + NO3 ----> HO2 + NO2 + DCB3  |   1.2000E-12 |   1.2000E-12 |
| R336   | PER2 + NO3 ----> HO2 + NO2 + MGLY + DCB1 +    1.050\*DCB3  |   1.2000E-12 |   1.2000E-12 |
| R337   | XYOP + NO3 ----> HO2 + NO2 +    0.368\*GLY +    0.632\*MGLY +    0.737\*DCB1 +    0.077\*DCB2 +    0.186\*DCB3  |   1.2000E-12 |   1.2000E-12 |
| R338   | ISOP + NO3 ----> HO2 + NO2 +    0.750\*HCHO +    0.318\*MACR +    0.500\*MVK +    0.024\*GLY +    0.033\*HKET +    0.031\*ALD  |   1.2000E-12 |   1.2000E-12 |
| R339   | APIP + NO3 ----> HO2 + NO2 + ALD + KET  |   1.2000E-12 |   1.2000E-12 |
| R340   | LIMP + NO3 ----> HO2 + NO2 +    0.385\*OLI +    0.385\*HCHO +    0.615\*MACR  |   1.2000E-12 |   1.2000E-12 |
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
| SA01   | TOLRO2 + NO ----> NO + TOLNRXN  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| SA02   | TOLRO2 + HO2 ----> HO2 + TOLHRXN  |   1.90E-13e<sup>  1300.00/T</sup> |   1.4872E-11 |
| SA03   | XYLRO2 + NO ----> NO + XYLNRXN  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| SA04   | XYLRO2 + HO2 ----> HO2 + XYLHRXN  |   1.90E-13e<sup>  1300.00/T</sup> |   1.4872E-11 |
| SA05   | BENZRO2 + NO ----> NO + BNZNRXN  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| SA06   | BENZRO2 + HO2 ----> HO2 + BNZHRXN  |   1.90E-13e<sup>  1300.00/T</sup> |   1.4872E-11 |
| SA07   | SESQ + O3 ----> O3 + SESQRXN  |   1.1600E-14 |   1.1600E-14 |
| SA08   | SESQ + HO ----> HO + SESQRXN  |   1.9700E-10 |   1.9700E-10 |
| SA09   | SESQ + NO3 ----> NO3 + SESQRXN  |   1.9000E-11 |   1.9000E-11 |
| SA10   | NAPH + HO ----> HO + PAHRO2  |   2.3100E-11 |   2.3100E-11 |
| SA11   | PAHRO2 + NO ----> NO + PAHNRXN  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| SA12   | PAHRO2 + HO2 ----> HO2 + PAHHRXN  |   1.90E-13e<sup>  1300.00/T</sup> |   1.4872E-11 |
| SA13   | SOAALK + HO ----> HO +    0.470\*ALKRXN  |   2.70E-12e<sup>   374.00/T</sup> |   9.4655E-12 |
| SA14   | IEPOX + HO ----> HO  |   5.78E-11e<sup>  -400.00/T</sup> |   1.5110E-11 |
| HET_N2O5   | N2O5 ---->   2.000\*HNO3  | HETERO_N2O5IJ | Not Available<sup>2</sup> | 
| HET_N02   | NO2 ---->   0.500\*HONO +    0.500\*HNO3  | HETERO_NO2 | Not Available<sup>2</sup> | 
| HAL_Ozone   | O3 ----> | SEAWATER*min( 6.701E-11e<sup> 1.074E+01P</sup>+ 3.415E-08e<sup>-6.713E-01P</sup>, <br> 2.000E-06) |   2.0000E-06<sup>4</sup>| 
| HET_IEPOX   | IEPOX ----> AISO3J  | HETERO_IEPOX | Not Available<sup>2</sup> | 
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
| RPOAGEPI   | APOCI + HO ---->   1.250\*APNCOMI + APOCI + HO  |   2.5000E-12 |   2.5000E-12 |
| RPOAGELI   | APNCOMI + HO ----> HO  | HETERO_PNCOMLI | Not Available<sup>2</sup> | 
| RPOAGEPJ   | APOCJ + HO ---->   1.250\*APNCOMJ + APOCJ + HO  |   2.5000E-12 |   2.5000E-12 |
| RPOAGELJ   | APNCOMJ + HO ----> HO  | HETERO_PNCOMLJ | Not Available<sup>2</sup> | 
| PCSOA   | PCVOC + HO ----> HO + PCSOARXN  |   1.2500E-11 |   1.2500E-11 |
| POA_AGE1   | VLVPO1 + HO ----> HO +    0.486\*VLVPO1 +    0.006\*VSVPO1 +    0.003\*VSVPO2 +    0.003\*VSVPO3 +    0.002\*VIVPO1 +    0.294\*VLVOO1 +    0.202\*VLVOO2 +    0.002\*VSVOO2 +    0.002\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE2   | VSVPO1 + HO ----> HO +    0.300\*VLVPO1 +    0.286\*VSVPO1 +    0.004\*VSVPO2 +    0.004\*VSVPO3 +    0.224\*VLVOO1 +    0.182\*VLVOO2  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE3   | VSVPO2 + HO ----> HO +    0.386\*VLVPO1 +    0.095\*VSVPO1 +    0.137\*VSVPO2 +    0.001\*VSVPO3 +    0.205\*VLVOO1 +    0.176\*VLVOO2  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE4   | VSVPO3 + HO ----> HO +    0.218\*VLVPO1 +    0.306\*VSVPO1 +    0.015\*VSVPO2 +    0.104\*VSVPO3 +    0.189\*VLVOO1 +    0.167\*VLVOO2  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE5   | VIVPO1 + HO ----> HO +    0.241\*VLVPO1 +    0.209\*VSVPO1 +    0.300\*VSVPO2 +    0.203\*VLVOO1 +    0.047\*VLVOO2  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE6   | VLVOO1 + HO ----> HO +    0.666\*VLVOO1 +    0.014\*VLVOO2 +    0.012\*VSVOO1 +    0.124\*VSVOO2 +    0.183\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE7   | VLVOO2 + HO ----> HO +    0.286\*VLVOO1 +    0.393\*VLVOO2 +    0.014\*VSVOO1 +    0.103\*VSVOO2 +    0.204\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE8   | VSVOO1 + HO ----> HO +    0.330\*VLVOO1 +    0.227\*VLVOO2 +    0.261\*VSVOO1 +    0.070\*VSVOO2 +    0.112\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE9   | VSVOO2 + HO ----> HO +    0.344\*VLVOO1 +    0.275\*VLVOO2 +    0.049\*VSVOO1 +    0.258\*VSVOO2 +    0.074\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |
| POA_AGE10   | VSVOO3 + HO ----> HO +    0.389\*VLVOO1 +    0.242\*VLVOO2 +    0.064\*VSVOO1 +    0.038\*VSVOO2 +    0.267\*VSVOO3  |   4.0000E-11 |   4.0000E-11 |

<sup>0</sup>Units molecules/(sec*cm<sup>3</sup>); Value at 298.15 K;   2.4615E+19 molcules/cm<sup>3</sup>;   1.00 Atm.     
<sup>1</sup>Photolysis Reaction;depends on radiation and predicted concentrations     
<sup>2</sup>Heteorogeneous Reaction;Depends predicted concentrations                
<sup>4</sup>Set to zero if sun is below the horizon. SEAWATER equals surface fraction covered by ice free open ocean plus surf zones. P equals air pressure in atmospheres.         
<sup>8</sup>Rate constant scaled as reverse equilibrium to constant for listed reaction    
