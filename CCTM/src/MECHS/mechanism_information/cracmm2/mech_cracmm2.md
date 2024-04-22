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
| R003   | H2O2 ---->   2.0000\*HO  | H2O2_RACM2 | Not Available<sup>1</sup> | 
| R004   | NO2 ----> O3P + NO  | NO2_RACM2 | Not Available<sup>1</sup> | 
| R005   | NO3 ----> NO  | NO3NO_RACM2 | Not Available<sup>1</sup> | 
| R006   | NO3 ----> O3P + NO2  | NO3NO2_RACM2 | Not Available<sup>1</sup> | 
| R007   | HONO ----> HO + NO  | HONO_RACM2 | Not Available<sup>1</sup> | 
| R008   | HNO3 ----> HO + NO2  | HNO3_RACM2 | Not Available<sup>1</sup> | 
| R009   | HNO4 ---->   0.2000\*HO +    0.8000\*HO2 +    0.8000\*NO2 +    0.2000\*NO3  | HNO4_RACM2 | Not Available<sup>1</sup> | 
| R010   | HCHO ----> CO  | HCHO_MOL_JPL19 | Not Available<sup>1</sup> | 
| R011   | HCHO ---->   2.0000\*HO2 + CO  | HCHO_RAD_JPL19 | Not Available<sup>1</sup> | 
| R012   | ACD ----> HO2 + MO2 + CO  | CH3CHO_RACM2 | Not Available<sup>1</sup> | 
| R013   | ALD ----> HO2 + ETHP + CO  | ALD_JPL19 | Not Available<sup>1</sup> | 
| R014   | ACT ----> MO2 + ACO3  | CH3COCH3A_JPL19 | Not Available<sup>1</sup> | 
| R014a   | ACT ---->   2.0000\*MO2 + CO  | CH3COCH3B_JPL19 | Not Available<sup>1</sup> | 
| R015   | UALD ---->   1.2200\*HO2 +    0.7840\*ACO3 +    1.2200\*CO +    0.3500\*HCHO +    0.4340\*ALD +    0.2160\*KET  | UALD_RACM2 | Not Available<sup>1</sup> | 
| TRP01   | PINAL ----> HO2 + HC10P + CO  | ALD_JPL19 | Not Available<sup>1</sup> | 
| TRP02   | LIMAL ----> HO2 + HC10P + CO  | ALD_JPL19 | Not Available<sup>1</sup> | 
| R016   | MEK ---->   0.1000\*MO2 + ETHP +    0.9000\*ACO3 +    0.1000\*CO  | MEK_JGR19 | Not Available<sup>1</sup> | 
| R017   | KET ---->   1.5000\*ETHP +    0.5000\*ACO3 +    0.5000\*CO  | KET_JGR19 | Not Available<sup>1</sup> | 
| R018   | HKET ----> HO2 + ACO3 + HCHO  | HKET_RACM2 | Not Available<sup>1</sup> | 
| R019   | MACR ---->   0.3400\*HO +    0.6600\*HO2 +    0.6700\*ACO3 +    0.3300\*MACP +    0.3400\*XO2 +    0.6700\*CO +    0.6700\*HCHO  | MACR_RACM2 | Not Available<sup>1</sup> | 
| R020   | MVK ---->   0.3000\*MO2 +    0.3000\*MACP +    0.7000\*CO +    0.7000\*UALD  | MVK_RACM2 | Not Available<sup>1</sup> | 
| R021   | GLY ---->   2.0000\*CO  | GLYH2_RACM2 | Not Available<sup>1</sup> | 
| R022   | GLY ----> HCHO + CO  | GLYF_RACM2 | Not Available<sup>1</sup> | 
| R023   | GLY ---->   2.0000\*HO2 +    2.0000\*CO  | GLYHX_RACM2 | Not Available<sup>1</sup> | 
| R024   | MGLY ----> HO2 + ACO3 + CO  | MGLY_RACM2 | Not Available<sup>1</sup> | 
| R025   | DCB1 ---->   1.5000\*HO2 +    0.2500\*ACO3 +    0.2000\*XO2 + CO +    0.5000\*GLY +    0.5000\*MGLY  | MGLY_RACM2 | Not Available<sup>1</sup> | 
| R026   | DCB2 ---->   1.5000\*HO2 +    0.2500\*ACO3 +    0.2000\*XO2 + CO +    0.5000\*GLY +    0.5000\*MGLY  | MGLY_RACM2 | Not Available<sup>1</sup> | 
| R027   | BALD ----> CHO + HO2 + CO  | BALD_RACM2 | Not Available<sup>1</sup> | 
| R028   | OP1 ----> HO + HO2 + HCHO  | OP1_RACM2 | Not Available<sup>1</sup> | 
| R029   | OP2 ----> HO + HO2 + ALD  | OP1_RACM2 | Not Available<sup>1</sup> | 
| TRP03   | OPB ----> HO + HO2 + ALD  | OP1_RACM2 | Not Available<sup>1</sup> | 
| R029a   | OP3 ----> HO + HO2 + ALD  | OP1_RACM2 | Not Available<sup>1</sup> | 
| R030   | PAA ----> HO + MO2  | PAA_RACM2 | Not Available<sup>1</sup> | 
| R031   | ONIT ----> HO2 + NO2 +    0.2000\*ALD +    0.8000\*KET  | ONIT_RACM2 | Not Available<sup>1</sup> | 
| R032   | PAN ----> ACO3 + NO2  | PAN1_RACM2 | Not Available<sup>1</sup> | 
| R033   | PAN ----> MO2 + NO3  | PAN2_RACM2 | Not Available<sup>1</sup> | 
| TRP55   | TRPN ----> NO2 +    0.6700\*KET +    0.3300\*UALD  | ONIT_RACM2 | Not Available<sup>1</sup> | 
| TRP56   | HONIT ----> HKET + NO2  | ONIT_RACM2 | Not Available<sup>1</sup> | 
| R034   | O3 + HO ----> HO2  |   1.70E-12e<sup>  -940.00/T</sup> |   7.2647E-14 |
| R035   | O3 + HO2 ----> HO  |   1.00E-14e<sup>  -490.00/T</sup> |   1.9331E-15 |
| R036   | O3 + NO ----> NO2  |   3.00E-12e<sup> -1500.00/T</sup> |   1.9596E-14 |
| R037   | O3 + NO2 ----> NO3  |   1.20E-13e<sup> -2450.00/T</sup> |   3.2392E-17 |
| R038   | O3P + O2 + M ----> O3  |   6.10E-34(T/300)<sup> -2.40</sup> |   6.1912E-34 |
| R039   | O3P + O3 ----> |   8.00E-12e<sup> -2060.00/T</sup> |   7.9879E-15 |
| R040   | O1D + O2 ----> O3P  |   3.30E-11e<sup>    55.00/T</sup> |   3.9685E-11 |
| R041   | O1D + N2 ----> O3P  |   2.15E-11e<sup>   110.00/T</sup> |   3.1093E-11 |
| R042   | O1D + H2O ---->   2.0000\*HO  |   1.63E-10e<sup>    60.00/T</sup> |   1.9934E-10 |
| R043   | HO + H2 ----> HO2  |   2.80E-12e<sup> -1800.00/T</sup> |   6.6869E-15 |
| R044   | HO + HO2 ----> |   4.80E-11e<sup>   250.00/T</sup> |   1.1102E-10 |
| R045   | HO2 + HO2 ----> H2O2  | k<sub>0</sub>=  3.00E-13e<sup>   460.0/T</sup><br>k<sub>1</sub>=  2.10E-33e<sup>   920.0/T</sup> |   2.5345E-12 |
| R046   | HO2 + HO2 + H2O ----> H2O2  | k<sub>0</sub>=  4.20E-34e<sup>  2660.0/T</sup><br>k<sub>1</sub>=  2.94E-54e<sup>  3120.0/T</sup> |   5.6834E-30 |
| R047   | H2O2 + HO ----> HO2  |   1.80E-12e<sup>     0.00/T</sup> |   1.8000E-12 |
| R048   | NO + O3P ----> NO2  | k<sub>o</sub>=  9.10E-32e<sup>     0.0/T</sup>(T/300)<sup> -1.50</sup><br>k<sub>i</sub> =   3.00E-11e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.00;F=     0.60 |   1.6772E-12 |
| R049   | NO + HO ----> HONO  | k<sub>o</sub>=  7.10E-31e<sup>     0.0/T</sup>(T/300)<sup> -2.60</sup><br>k<sub>i</sub> =   3.60E-11e<sup>     0.0/T</sup>(T/300)<sup> -0.10</sup><br>n=     1.00;F=     0.60 |   7.4585E-12 |
| R050   | NO + HO2 ----> NO2 + HO  |   3.44E-12e<sup>   260.00/T</sup> |   8.2278E-12 |
| R051   | NO + HO2 ----> HNO3  | k<sub>0</sub>=  6.0950E-14e<sup>   270.0/T</sup>(T/300)<sup> -1.00</sup><br>k<sub>2</sub>=  6.8570E-34e<sup>   270.0/T</sup>(T/300)<sup>  1.00</sup><br>k<sub>3</sub>= -5.9680E-14e<sup>   270.00/T</sup> |   4.5566E-14 |
| R052   | NO + NO + O2 ---->   2.0000\*NO2  |   4.25E-39e<sup>   663.50/T</sup> |   3.9343E-38 |
| R053   | HONO + HO ----> NO2  |   3.00E-12e<sup>   250.00/T</sup> |   6.9387E-12 |
| R054   | NO2 + O3P ----> NO  |   5.30E-12e<sup>   200.00/T</sup> |   1.0366E-11 |
| R055   | NO2 + O3P ----> NO3  | k<sub>o</sub>=  3.40E-31e<sup>     0.0/T</sup>(T/300)<sup> -1.60</sup><br>k<sub>i</sub> =   2.30E-11e<sup>     0.0/T</sup>(T/300)<sup> -0.20</sup><br>n=     1.00;F=     0.60 |   4.0243E-12 |
| R056   | NO2 + HO ----> HNO3  | k<sub>o</sub>=  1.80E-30e<sup>     0.0/T</sup>(T/300)<sup> -3.00</sup><br>k<sub>i</sub> =   2.80E-11e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>n=     1.00;F=     0.60 |   1.0589E-11 |
| R057   | HNO3 + HO ----> NO3  | k<sub>0</sub>=  2.40E-14e<sup>   460.0/T</sup><br>k<sub>1</sub>=  2.70E-17e<sup>  2199.0/T</sup><br>k<sub>3</sub>=  6.50E-34e<sup>  1335.0/T</sup> |   1.5409E-13 |
| R058   | NO3 + HO ----> HO2 + NO2  |   2.0000E-11 |   2.0000E-11 |
| R059   | NO3 + HO2 ---->   0.7000\*HO +    0.7000\*NO2 +    0.3000\*HNO3  |   3.5000E-12 |   3.5000E-12 |
| R060   | NO3 + NO ---->   2.0000\*NO2  |   1.70E-11e<sup>   125.00/T</sup> |   2.5854E-11 |
| R061   | NO3 + NO2 ----> NO + NO2  |   4.35E-14e<sup> -1335.00/T</sup> |   4.9418E-16 |
| R062   | NO3 + NO3 ---->   2.0000\*NO2  |   8.50E-13e<sup> -2450.00/T</sup> |   2.2944E-16 |
| R063   | NO3 + NO2 ----> N2O5  | k<sub>o</sub>=  2.40E-30e<sup>     0.0/T</sup>(T/300)<sup> -3.00</sup><br>k<sub>i</sub> =   1.60E-12e<sup>     0.0/T</sup>(T/300)<sup>  0.10</sup><br>n=     1.00;F=     0.60 |   1.3451E-12 |
| R064   | N2O5 ----> NO2 + NO3  |   1.72E+26e<sup>-10840.00/T</sup> \*R063 |   3.7623E-02<sup>8</sup>| 
| R065   | N2O5 + H2O ---->   2.0000\*HNO3  |   1.0000E-22 |   1.0000E-22 |
| R066   | NO2 + HO2 ----> HNO4  | k<sub>o</sub>=  1.90E-31e<sup>     0.0/T</sup>(T/300)<sup> -3.40</sup><br>k<sub>i</sub> =   4.00E-12e<sup>     0.0/T</sup>(T/300)<sup> -0.30</sup><br>n=     1.00;F=     0.60 |   1.3113E-12 |
| R067   | HNO4 ----> HO2 + NO2  |   4.76E+26e<sup>-10900.00/T</sup> \*R066 |   8.2835E-02<sup>8</sup>| 
| R068   | HNO4 + HO ----> NO2  |   4.50E-13e<sup>   610.00/T</sup> |   3.4814E-12 |
| R069   | SO2 + HO ----> HO2 + SULF + SULRXN  | k<sub>o</sub>=  2.90E-31e<sup>     0.0/T</sup>(T/300)<sup> -4.10</sup><br>k<sub>i</sub> =   1.70E-12e<sup>     0.0/T</sup>(T/300)<sup>  0.20</sup><br>n=     1.00;F=     0.60 |   9.5762E-13 |
| R070   | CO + HO ----> HO2  | k<sub>0</sub>=  1.44E-13e<sup>     0.0/T</sup><br>k<sub>1</sub>=  2.74E-33e<sup>     0.0/T</sup> |   2.1145E-13 |
| R071   | HO + CH4 ----> MO2  |   2.45E-12e<sup> -1775.00/T</sup> |   6.3628E-15 |
| R072   | ETH + HO ----> ETHP  |   7.66E-12e<sup> -1020.00/T</sup> |   2.5030E-13 |
| R073   | HC3 + HO ----> HC3P +    0.0000\*ASOATJ  |   7.68E-12e<sup>  -370.00/T</sup> |   2.2203E-12 |
| R074   | HC5 + HO ----> HC5P +    0.0013\*ASOATJ  |   1.01E-11e<sup>  -245.00/T</sup> |   4.4407E-12 |
| R076   | ETE + HO ----> ETEP  | k<sub>o</sub>=  1.00E-28e<sup>     0.0/T</sup>(T/300)<sup> -4.50</sup><br>k<sub>i</sub> =   8.80E-12e<sup>     0.0/T</sup>(T/300)<sup> -0.85</sup><br>n=     1.00;F=     0.60 |   8.1981E-12 |
| R077   | OLT + HO ----> OLTP  |   5.72E-12e<sup>   500.00/T</sup> |   3.0599E-11 |
| R078   | OLI + HO ----> OLIP  |   1.33E-11e<sup>   500.00/T</sup> |   7.1149E-11 |
| R080   | ACE + HO ---->   0.6500\*HO +    0.3500\*HO2 +    0.3500\*CO +    0.6500\*GLY +    0.3500\*ORA1  | k<sub>o</sub>=  5.50E-30e<sup>     0.0/T</sup>(T/300)<sup>  0.00</sup><br>k<sub>i</sub> =   8.30E-13e<sup>     0.0/T</sup>(T/300)<sup>  2.00</sup><br>n=     1.00;F=     0.60 |   7.4748E-13 |
| ROCARO31   | BEN + HO ---->   0.4700\*BENP +    0.5300\*PHEN +    0.5300\*HO2  |   2.33E-12e<sup>  -193.00/T</sup> |   1.2196E-12 |
| ROCARO41   | TOL + HO ---->   0.8200\*TOLP +    0.1800\*CSL +    0.1800\*HO2  |   1.81E-12e<sup>   354.00/T</sup> |   5.9337E-12 |
| ROCARO51   | XYL + HO ---->   0.8300\*XYLP +    0.1700\*CSL +    0.1700\*HO2  |   2.3300E-11 |   2.3300E-11 |
| ROCARO61   | EBZ + HO ---->   0.8200\*EBZP +    0.1800\*CSL +    0.1800\*HO2  |   7.1600E-12 |   7.1600E-12 |
| RAM01   | ISO + O3 ---->   0.2500\*HO +    0.2500\*HO2 +    0.4000\*MO2 +    0.0180\*ACO3 +    0.1000\*MACP +    0.0900\*H2O2 +    0.2200\*CO + HCHO +    0.3000\*MACR +    0.1400\*MVK +    0.2800\*ORA1 +    0.1530\*OLT  |   1.58E-14e<sup> -2000.00/T</sup> |   1.9293E-17 |
| RAM02   | ISO + NO3 ---->   0.4000\*NO2 +    0.0450\*ISON +    0.3500\*HCHO +    0.5550\*INO2 +    0.2600\*MVK +    0.0280\*MACR  |   2.95E-12e<sup>  -450.00/T</sup> |   6.5214E-13 |
| RAM03   | ISO + HO ----> ISOP +    0.2500\*HCHO +    0.0300\*MACR +    0.0500\*MGLY  |   2.69E-11e<sup>   390.00/T</sup> |   9.9503E-11 |
| RAM04   | ISOP + HO2 ----> ISHP +    0.0700\*HO2 +    0.5000\*HO  |   4.50E-13e<sup>  1300.00/T</sup> |   3.5224E-11 |
| RAM05   | ISOP + NO ---->   0.1300\*ISON +    0.4000\*HCHO +    0.8800\*HO2 +    0.8700\*NO2 +    0.1800\*MACR +    0.5100\*MVK  |   6.00E-12e<sup>   350.00/T</sup> |   1.9408E-11 |
| RAM06   | ISHP + HO ----> ISOP  |   4.60E-12e<sup>   200.00/T</sup> |   8.9967E-12 |
| RAM07   | ISHP + HO ---->   0.0400\*MGLY +    0.0200\*GLY +    0.1300\*MVK +    0.4400\*IEPOX +    0.1100\*ACO3 +    0.0300\*MACR +    2.0000\*HO +    0.3400\*HO2 +    0.1400\*IPX + CO  |   2.97E-11e<sup>   390.00/T</sup> |   1.0986E-10 |
| RAM08   | INO2 + HO2 ---->   0.4500\*HO +    0.9500\*INALD +    0.0200\*IPX  |   3.14E-11e<sup>   580.00/T</sup> |   2.1967E-10 |
| RAM09   | INO2 + NO ---->   0.1500\*MVK +    0.6500\*INALD +    0.0500\*ISON +    0.2000\*HCHO +    1.3000\*NO2  |   9.42E-12e<sup>   580.00/T</sup> |   6.5902E-11 |
| RAM10   | ISON + HO ----> HO +    0.3500\*INALD +    0.1700\*IEPOX +    0.6500\*NO2  |   2.40E-11e<sup>   390.00/T</sup> |   8.8776E-11 |
| RAM11   | HO + INALD ----> CO + NO2 +    0.3000\*HO2 + HCHO  |   1.5000E-11 |   1.5000E-11 |
| RAM12   | ISON ----> HNO3 + ROH  |   4.0000E-05 |   4.0000E-05 |
| RAM13   | IPX + HO ---->   0.5700\*MACR +    0.4300\*MVK  |   3.0000E-12 |   3.0000E-12 |
| R087   | API + HO ---->   0.9750\*APIP1 +    0.0250\*APIP2  |   1.21E-11e<sup>   440.00/T</sup> |   5.2930E-11 |
| R088   | LIM + HO ---->   0.9450\*LIMP1 +    0.0550\*LIMP2  |   4.20E-11e<sup>   401.00/T</sup> |   1.6120E-10 |
| TRP04   | PINAL + HO ---->   0.2300\*PINALP +    0.7700\*RCO3  |   5.20E-12e<sup>   600.00/T</sup> |   3.8903E-11 |
| TRP05   | LIMAL + HO ---->   0.8300\*LIMALP +    0.1700\*RCO3  |   1.1000E-10 |   1.1000E-10 |
| R089   | HCHO + HO ----> HO2 + CO  |   5.50E-12e<sup>   125.00/T</sup> |   8.3645E-12 |
| R090   | ACD + HO ----> ACO3  |   4.70E-12e<sup>   345.00/T</sup> |   1.4950E-11 |
| R091   | ALD + HO ----> RCO3  |   4.90E-12e<sup>   405.00/T</sup> |   1.9060E-11 |
| R092   | ACT + HO ----> ACTP  |   4.56E-14e<sup>  -427.00/T</sup>(T/300)<sup>  3.65 </sup> |   1.0646E-14 |
| R093   | MEK + HO ----> MEKP  |   1.50E-12e<sup>   -90.00/T</sup> |   1.1092E-12 |
| R094   | KET + HO ----> KETP  |   2.80E-12e<sup>    10.00/T</sup> |   2.8955E-12 |
| R095   | HKET + HO ----> HO2 + MGLY  |   3.0000E-12 |   3.0000E-12 |
| R096   | MACR + HO ---->   0.5700\*MACP +    0.4300\*MCP  |   8.00E-12e<sup>   380.00/T</sup> |   2.8616E-11 |
| R097   | MVK + HO ----> MVKP  |   2.60E-12e<sup>   610.00/T</sup> |   2.0115E-11 |
| R098   | UALD + HO ---->   0.3130\*ACO3 +    0.6870\*UALP  |   5.77E-12e<sup>   533.00/T</sup> |   3.4479E-11 |
| R099   | GLY + HO ----> HO2 +    2.0000\*CO  |   1.1000E-11 |   1.1000E-11 |
| R100   | MGLY + HO ----> ACO3 + CO  |   9.26E-13e<sup>   830.00/T</sup> |   1.4984E-11 |
| R101   | DCB1 + HO ---->   0.5200\*HO2 +    0.3300\*CO +    0.4000\*ALD +    0.7800\*KET +    0.1000\*GLY +    0.0100\*MGLY  |   2.80E-11e<sup>   175.00/T</sup> |   5.0358E-11 |
| R102   | DCB2 + HO ---->   0.5200\*HO2 +    0.3300\*CO +    0.1300\*MEK +    0.1000\*GLY +    0.0100\*MGLY +    0.7800\*OP2  |   2.80E-11e<sup>   175.00/T</sup> |   5.0358E-11 |
| R103   | DCB3 + HO ---->   0.5600\*HO2 +    0.2100\*MACP +    0.1100\*CO +    0.2700\*GLY +    0.0100\*MGLY +    0.7900\*OP2  |   1.0000E-11 |   1.0000E-11 |
| R104   | BALD + HO ----> BALP  |   5.32E-12e<sup>   243.00/T</sup> |   1.2019E-11 |
| R105   | PHEN + HO ---->   0.1520\*ASOATJ +    0.6190\*HO2 +    0.1700\*ADDC +    0.0590\*CHO +    0.6190\*MCT  |   6.75E-12e<sup>   405.00/T</sup> |   2.6257E-11 |
| R106   | CSL + HO ---->   0.2000\*ASOATJ +    0.5840\*HO2 +    0.1600\*ADDC +    0.0560\*CHO +    0.5840\*MCT  |   4.65E-11e<sup>     0.00/T</sup> |   4.6500E-11 |
| R108   | MCT + HO ----> MCTO  |   2.05E-10e<sup>     0.00/T</sup> |   2.0500E-10 |
| R109   | MOH + HO ----> HO2 + HCHO  |   2.85E-12e<sup>  -345.00/T</sup> |   8.9600E-13 |
| R110   | EOH + HO ----> HO2 + ACD  |   3.00E-12e<sup>    20.00/T</sup> |   3.2081E-12 |
| R111   | ROH + HO ----> HO2 +    0.7190\*ALD +    0.1840\*ACD  |   2.60E-12e<sup>   200.00/T</sup> |   5.0851E-12 |
| R112   | ETEG + HO ----> HO2 + ALD  |   1.4700E-11 |   1.4700E-11 |
| R113   | OP1 + HO ---->   0.3500\*HO +    0.6500\*MO2 +    0.3500\*HCHO  |   2.90E-12e<sup>   190.00/T</sup> |   5.4848E-12 |
| R114   | OP2 + HO ---->   0.0100\*HO +    0.4400\*HC3P +    0.0700\*XO2 +    0.0800\*ALD +    0.4100\*KET  |   3.40E-12e<sup>   190.00/T</sup> |   6.4304E-12 |
| TRP06   | OPB + HO ---->   0.0100\*HO +    0.4400\*HC10P +    0.0700\*XO2 +    0.0800\*ALD +    0.4100\*KET  |   3.40E-12e<sup>   190.00/T</sup> |   6.4304E-12 |
| R114a   | OP3 + HO ---->   0.0100\*HO +    0.4400\*HC10P +    0.0700\*XO2 +    0.0800\*ALD +    0.4100\*KET  |   3.40E-12e<sup>   190.00/T</sup> |   6.4304E-12 |
| R116   | MAHP + HO ----> MACP  |   3.0000E-11 |   3.0000E-11 |
| R117   | ORA1 + HO ----> HO2  |   4.5000E-13 |   4.5000E-13 |
| R118   | ORA2 + HO ---->   0.6400\*MO2 +    0.3600\*ORAP  |   4.00E-14e<sup>   850.00/T</sup> |   6.9214E-13 |
| R119   | PAA + HO ---->   0.3500\*HO +    0.6500\*ACO3 +    0.3500\*XO2 +    0.3500\*HCHO  |   2.93E-12e<sup>   190.00/T</sup> |   5.5415E-12 |
| R120   | PAN + HO ----> XO2 + NO3 + HCHO  |   4.0000E-14 |   4.0000E-14 |
| R121   | PPN + HO ----> XO2 + NO3 + HCHO  |   4.0000E-14 |   4.0000E-14 |
| R122   | MPAN + HO ----> NO2 + HKET  |   3.2000E-11 |   3.2000E-11 |
| R123   | ONIT + HO ----> HC3P + NO2  |   5.31E-12e<sup>  -260.00/T</sup> |   2.2201E-12 |
| TRP07   | TRPN + HO ---->   0.3300\*HONIT +    0.6700\*NO2 +    0.2700\*PINAL +    0.3800\*KET +    0.2100\*HCHO +    0.0200\*ALD  |   4.8000E-12 |   4.8000E-12 |
| TRP57   | HONIT + HO ----> HKET + NO3  | k<sub>0</sub>=  2.40E-14e<sup>   460.0/T</sup><br>k<sub>1</sub>=  2.70E-17e<sup>  2199.0/T</sup><br>k<sub>3</sub>=  6.50E-34e<sup>  1335.0/T</sup> |   1.5409E-13 |
| R126   | ETE + O3 ---->   0.0800\*HO +    0.1500\*HO2 +    0.4300\*CO + HCHO +    0.3700\*ORA1  |   9.14E-15e<sup> -2580.00/T</sup> |   1.5953E-18 |
| R127   | OLT + O3 ---->   0.2200\*HO +    0.3200\*HO2 +    0.0800\*MO2 +    0.0600\*ETHP +    0.0400\*HC3P +    0.0200\*HC5P +    0.0680\*H2O2 +    0.4300\*CO +    0.0200\*ETH +    0.0150\*HC3 +    0.0060\*HC5 +    0.0320\*BEN +    0.5600\*HCHO +    0.0100\*ACD +    0.4400\*ALD +    0.0300\*ACT +    0.0200\*BALD +    0.0600\*MEK +    0.0100\*HKET +    0.0300\*ORA1 +    0.0600\*ORA2  |   4.33E-15e<sup> -1800.00/T</sup> |   1.0341E-17 |
| R128   | OLI + O3 ---->   0.4600\*HO +    0.0700\*HO2 +    0.3200\*MO2 +    0.0700\*ETHP +    0.0400\*HC3P +    0.0900\*ACO3 +    0.3700\*CO +    0.0260\*H2O2 +    0.0100\*ETH +    0.0100\*HC3 +    0.0900\*HCHO +    0.4570\*ACD +    0.7300\*ALD +    0.1100\*ACT +    0.0170\*KET +    0.0440\*HKET +    0.0170\*ORA2  |   4.40E-15e<sup>  -845.00/T</sup> |   2.5858E-16 |
| R131   | API + O3 ---->   0.6055\*HO +    0.2145\*PINALP +    0.2550\*H2O2 +    0.2535\*PINAL +    0.0065\*ORA2 +    0.1105\*HO2 +    0.1105\*CO +    0.4590\*HCHO +    0.2805\*RCO3 +    0.1785\*KET +    0.0665\*HC3 +    0.0385\*OP1 +    0.0280\*ORA1  |   8.05E-16e<sup>  -640.00/T</sup> |   9.4092E-17 |
| R132   | LIM + O3 ---->   0.6600\*HO +    0.6600\*LIMAL +    0.3300\*ACO3 +    0.3300\*HCHO +    0.3300\*RCO3 +    0.3300\*H2O2 +    0.0100\*ORA2  |   2.80E-15e<sup>  -770.00/T</sup> |   2.1162E-16 |
| TRP08   | LIMAL + O3 ---->   0.0900\*HO + ALD +    0.6200\*HCHO +    0.2300\*OP1 +    0.0200\*H2O2 +    0.1500\*ORA1  |   8.3000E-18 |   8.3000E-18 |
| TRP09   | TRPN + O3 ---->   0.3300\*HONIT +    0.6700\*NO2 +    0.2700\*PINAL +    0.3800\*KET +    0.2100\*HCHO +    0.0200\*ALD  |   1.6700E-16 |   1.6700E-16 |
| R133   | MACR + O3 ---->   0.1900\*HO +    0.1400\*HO2 +    0.1000\*ACO3 +    0.2200\*CO +    0.5000\*MGLY +    0.4500\*ORA1  |   1.36E-15e<sup> -2112.00/T</sup> |   1.1406E-18 |
| R134   | MVK + O3 ---->   0.1600\*HO +    0.1100\*HO2 +    0.2800\*ACO3 +    0.0100\*XO2 +    0.5600\*CO +    0.1000\*HCHO +    0.5400\*MGLY +    0.0700\*ORA1 +    0.0700\*ORA2 +    0.1000\*ALD  |   8.50E-16e<sup> -1520.00/T</sup> |   5.1921E-18 |
| R135   | UALD + O3 ---->   0.1000\*HO +    0.0720\*HO2 +    0.0080\*MO2 +    0.0020\*ACO3 +    0.1000\*XO2 +    0.2430\*CO +    0.0800\*HCHO +    0.4200\*ACD +    0.0280\*KET +    0.4910\*GLY +    0.0030\*MGLY +    0.0440\*ORA1  |   1.6600E-18 |   1.6600E-18 |
| R136   | DCB1 + O3 ---->   0.0500\*HO + HO2 +    0.6000\*RCO3 +    0.6000\*XO2 +    1.5000\*CO +    0.0500\*HCHO +    0.0500\*GLY +    0.0800\*MGLY +    0.6500\*OP2  |   2.0000E-16 |   2.0000E-16 |
| R137   | DCB2 + O3 ---->   0.0500\*HO + HO2 +    0.6000\*RCO3 +    0.6000\*XO2 +    1.5000\*CO +    0.0500\*HCHO +    0.0500\*GLY +    0.0800\*MGLY +    0.7000\*DCB1 +    0.6500\*OP2  |   2.0000E-16 |   2.0000E-16 |
| R138   | DCB3 + O3 ---->   0.0500\*HO + HO2 +    1.5000\*CO +    0.4800\*GLY +    0.7000\*DCB1 +    0.2500\*ORA1 +    0.2500\*ORA2 +    0.1100\*PAA  |   9.0000E-17 |   9.0000E-17 |
| R140   | MCTO + O3 ----> MCTP  |   2.8600E-13 |   2.8600E-13 |
| R141   | ETE + NO3 ---->   0.8000\*OLNN +    0.2000\*OLND  |   4.39E-13e<sup> -2282.00/T</sup>(T/300)<sup>  2.00 </sup> |   2.0571E-16 |
| R142   | OLT + NO3 ---->   0.4300\*OLNN +    0.5700\*OLND  |   1.79E-13e<sup>  -450.00/T</sup> |   3.9570E-14 |
| R143   | OLI + NO3 ---->   0.1100\*OLNN +    0.8900\*OLND  |   8.64E-13e<sup>   450.00/T</sup> |   3.9084E-12 |
| R146   | API + NO3 ---->   0.9750\*APINP1 +    0.0250\*APINP2  |   1.19E-12e<sup>   490.00/T</sup> |   6.1560E-12 |
| R147   | LIM + NO3 ---->   0.9450\*LIMNP1 +    0.0550\*LIMNP2  |   1.2200E-11 |   1.2200E-11 |
| TRP10   | TRPN + NO3 ---->   0.3300\*HONIT +    0.6700\*NO2 +    0.2700\*PINAL +    0.3800\*KET +    0.2100\*HCHO +    0.0200\*ALD  |   3.15E-13e<sup>  -448.00/T</sup> |   7.0104E-14 |
| R148   | HCHO + NO3 ----> HO2 + CO + HNO3  |   2.00E-12e<sup> -2440.00/T</sup> |   5.5828E-16 |
| R149   | ACD + NO3 ----> ACO3 + HNO3  |   1.40E-12e<sup> -1900.00/T</sup> |   2.3907E-15 |
| R150   | ALD + NO3 ----> RCO3 + HNO3  |   3.76E-12e<sup> -1900.00/T</sup> |   6.4208E-15 |
| R151   | MACR + NO3 ---->   0.6800\*HCHO +    0.3200\*MACP +    0.6800\*XO2 +    0.6800\*MGLY +    0.3200\*HNO3 +    0.6800\*NO2  |   3.4000E-15 |   3.4000E-15 |
| R152   | UALD + NO3 ----> HO2 + XO2 +    0.6680\*CO +    0.3320\*HCHO +    0.3320\*ALD + ONIT  |   5.02E-13e<sup> -1076.00/T</sup> |   1.3595E-14 |
| R153   | GLY + NO3 ----> HO2 +    2.0000\*CO + HNO3  |   2.90E-12e<sup> -1900.00/T</sup> |   4.9522E-15 |
| R154   | MGLY + NO3 ----> ACO3 + CO + HNO3  |   3.76E-12e<sup> -1900.00/T</sup> |   6.4208E-15 |
| R155   | PHEN + NO3 ---->   0.1520\*ASOATJ +    0.3390\*CHO +    0.8500\*ADDC +    0.4240\*ADCN +    0.4240\*HNO3  |   3.7800E-12 |   3.7800E-12 |
| R156   | CSL + NO3 ---->   0.2000\*ASOATJ +    0.3200\*CHO +    0.0800\*ADDC +    0.4000\*ADCN +    0.4000\*HNO3  |   1.0600E-12 |   1.0600E-12 |
| R158   | MCT + NO3 ----> MCTO + HNO3  |   2.0100E-10 |   2.0100E-10 |
| R159   | MPAN + NO3 ----> MACP + NO2  |   2.20E-14e<sup>  -500.00/T</sup> |   4.1125E-15 |
| TRP11   | PINALP ----> HOM  |   2.9000E-02 |   2.9000E-02 |
| TRP12   | LIMALP ----> HOM  |   2.4000E-02 |   2.4000E-02 |
| R166   | ACO3 + NO2 ----> PAN  | k<sub>o</sub>=  9.70E-29e<sup>     0.0/T</sup>(T/300)<sup> -5.60</sup><br>k<sub>i</sub> =   9.30E-12e<sup>     0.0/T</sup>(T/300)<sup> -1.50</sup><br>n=     1.00;F=     0.60 |   8.6800E-12 |
| R167   | PAN ----> ACO3 + NO2  |   1.11E+28e<sup>-14000.00/T</sup> \*R166 |   3.9034E-04<sup>8</sup>| 
| R168   | RCO3 + NO2 ----> PPN  | k<sub>o</sub>=  9.70E-29e<sup>     0.0/T</sup>(T/300)<sup> -5.60</sup><br>k<sub>i</sub> =   9.30E-12e<sup>     0.0/T</sup>(T/300)<sup> -1.50</sup><br>n=     1.00;F=     0.60 |   8.6800E-12 |
| R169   | PPN ----> RCO3 + NO2  |   1.11E+28e<sup>-14000.00/T</sup> \*R168 |   3.9034E-04<sup>8</sup>| 
| R170   | MACP + NO2 ----> MPAN  |   2.80E-12e<sup>   181.00/T</sup> |   5.1382E-12 |
| R171   | MPAN ----> MACP + NO2  |   1.60E+16e<sup>-13486.00/T</sup> |   3.6308E-04 |
| R172   | MO2 + NO ----> HO2 + NO2 + HCHO  |   2.80E-12e<sup>   300.00/T</sup> |   7.6586E-12 |
| R173   | ETHP + NO ----> HO2 + NO2 + ACD  |   2.60E-12e<sup>   365.00/T</sup> |   8.8439E-12 |
| R174   | HC3P + NO ---->   0.6600\*HO2 +    0.1310\*MO2 +    0.0480\*ETHP +    0.0890\*XO2 +    0.9350\*NO2 +    0.5040\*ACD +    0.1320\*ALD +    0.1650\*ACT +    0.0420\*MEK +    0.0650\*ONIT  |   4.0000E-12 |   4.0000E-12 |
| R175   | HC5P + NO ---->   0.2000\*HO2 +    0.0510\*MO2 +    0.2310\*ETHP +    0.2350\*XO2 +    0.8640\*NO2 +    0.0180\*HCHO +    0.0450\*ACD +    0.2030\*ALD +    0.0330\*MEK +    0.2170\*ACT +    0.0330\*KET +    0.2720\*HKET +    0.1360\*ONIT  |   4.0000E-12 |   4.0000E-12 |
| R177   | ETEP + NO ----> HO2 + NO2 +    1.6000\*HCHO +    0.2000\*ALD  |   9.0000E-12 |   9.0000E-12 |
| R178   | OLTP + NO ---->   0.7800\*HO2 +    0.9700\*NO2 +    0.7800\*HCHO +    0.0120\*ACD +    0.4400\*ALD +    0.0600\*ACT +    0.1300\*MEK +    0.0300\*ONIT  |   4.0000E-12 |   4.0000E-12 |
| R179   | OLIP + NO ---->   0.8300\*HO2 +    0.9500\*NO2 +    0.8100\*ACD +    0.6800\*ALD +    0.2000\*ACT +    0.0900\*KET +    0.0200\*HKET +    0.0500\*ONIT  |   4.0000E-12 |   4.0000E-12 |
| ROCARO33   | BENP + NO ---->   0.0000\*ONIT +    0.0012\*VROCP4OXY2 +    0.0008\*VROCN1OXY6 +    0.9980\*NO2 +    0.9980\*HO2 +    0.0000\*BALD +    0.9980\*GLY +    0.4990\*FURANONE +    0.2495\*DCB2 +    0.2495\*DCB3  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCARO43   | TOLP + NO ---->   0.0002\*ONIT +    0.0013\*VROCP4OXY2 +    0.0006\*VROCN1OXY6 +    0.9980\*NO2 +    0.9980\*HO2 +    0.0852\*BALD +    0.5477\*GLY +    0.3651\*MGLY +    0.3651\*FURANONE +    0.5477\*DCB1  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCARO53   | XYLP + NO ---->   0.0001\*ONIT +    0.0013\*VROCP3OXY2 +    0.0006\*VROCP0OXY4 +    0.9980\*NO2 +    0.9980\*HO2 +    0.0481\*BALD +    0.7029\*GLY +    0.2470\*MGLY +    0.3515\*FURANONE +    0.5984\*DCB2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCARO63   | EBZP + NO ---->   0.0002\*ONIT +    0.0013\*VROCP3OXY2 +    0.0006\*VROCP0OXY4 +    0.9980\*NO2 +    0.9980\*HO2 +    0.0852\*BALD +    0.5477\*GLY +    0.3651\*MGLY +    0.4564\*FURANONE +    0.4564\*DCB2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R189   | APIP1 + NO ---->   0.2370\*TRPN +    0.7630\*HO2 +    0.7630\*NO2 +    0.1950\*PINAL +    0.2840\*ALD +    0.0865\*ACT +    0.1285\*LIMAL +    0.3080\*HCHO +    0.0715\*OPB +    0.0840\*KET  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| TRP13   | APIP2 + NO ---->   0.8200\*HO +    0.8200\*NO2 + HOM  |   4.0000E-12 |   4.0000E-12 |
| TRP14   | APINP1 + NO ---->   1.6955\*NO2 +    0.2345\*TRPN +    0.0700\*ONIT +    0.6045\*PINAL +    0.1540\*ALD +    0.0070\*KET +    0.0070\*HCHO +    0.1645\*HO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| TRP15   | APINP2 + NO ---->   0.8200\*NO2 +    0.8200\*HO + HOM  |   4.0000E-12 |   4.0000E-12 |
| R190   | LIMP1 + NO ---->   0.2300\*TRPN +    0.7700\*NO2 +    0.7700\*LIMAL +    0.7700\*HO2 +    0.4300\*HCHO  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| TRP16   | LIMP2 + NO ---->   0.7700\*HO +    0.7700\*NO2 + HOM  |   4.0000E-12 |   4.0000E-12 |
| TRP17   | LIMNP1 + NO ---->   0.5700\*TRPN +    0.0700\*ONIT +    1.3600\*NO2 +    0.4300\*LIMAL +    0.5000\*HO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| TRP18   | LIMNP2 + NO ---->   0.7700\*NO2 +    0.7700\*HO + HOM  |   4.0000E-12 |   4.0000E-12 |
| TRP19   | PINALP + NO ---->   0.3600\*TRPN +    0.6400\*HOM +    0.6400\*NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| TRP20   | LIMALP + NO ---->   0.6400\*TRPN +    0.3600\*NO2 +    0.3600\*HO2 +    0.3600\*HCHO +    0.3600\*PAA  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R191   | ACO3 + NO ----> MO2 + NO2  |   8.10E-12e<sup>   270.00/T</sup> |   2.0034E-11 |
| R192   | RCO3 + NO ----> ETHP + NO2  |   8.10E-12e<sup>   270.00/T</sup> |   2.0034E-11 |
| R193   | ACTP + NO ----> ACO3 + NO2 + HCHO  |   2.90E-12e<sup>   300.00/T</sup> |   7.9321E-12 |
| R194   | MEKP + NO ---->   0.6700\*HO2 + NO2 +    0.3300\*HCHO +    0.6700\*DCB1  |   4.0000E-12 |   4.0000E-12 |
| R195   | KETP + NO ---->   0.7700\*HO2 +    0.2300\*ACO3 +    0.1600\*XO2 + NO2 +    0.4600\*ALD +    0.5400\*MGLY  |   4.0000E-12 |   4.0000E-12 |
| R196   | MACP + NO ---->   0.6500\*MO2 +    0.3500\*ACO3 + NO2 +    0.6500\*CO +    0.6500\*HCHO  |   2.54E-12e<sup>   360.00/T</sup> |   8.4961E-12 |
| R197   | MCP + NO ----> NO2 +    0.5000\*HO2 +    0.5000\*HCHO + HKET  |   2.54E-12e<sup>   360.00/T</sup> |   8.4961E-12 |
| R198   | MVKP + NO ---->   0.3000\*HO2 +    0.7000\*ACO3 +    0.7000\*XO2 + NO2 +    0.3000\*HCHO +    0.7000\*ALD +    0.3000\*MGLY  |   2.54E-12e<sup>   360.00/T</sup> |   8.4961E-12 |
| R199   | UALP + NO ----> HO2 + NO2 +    0.6100\*CO +    0.0300\*HCHO +    0.2700\*ALD +    0.1800\*GLY +    0.7000\*KET +    0.2100\*MGLY  |   2.54E-12e<sup>   360.00/T</sup> |   8.4961E-12 |
| R200   | BALP + NO ----> BAL1 + NO2  |   4.0000E-12 |   4.0000E-12 |
| R201   | BAL1 + NO ----> BAL2 + NO2  |   4.0000E-12 |   4.0000E-12 |
| R202   | ADDC + NO ----> HO2 + NO2 +    0.3200\*HKET +    0.6800\*GLY +    0.6800\*OP2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R203   | MCTP + NO ----> MCTO + NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R204   | ORAP + NO ----> NO2 + GLY + HO2  |   4.0000E-12 |   4.0000E-12 |
| R205   | OLNN + NO ----> NO2 + HO2 + ONIT  |   4.0000E-12 |   4.0000E-12 |
| R206   | OLND + NO ---->   2.0000\*NO2 +    0.2870\*HCHO +    1.2400\*ALD +    0.4640\*KET  |   4.0000E-12 |   4.0000E-12 |
| R207   | ADCN + NO ---->   2.0000\*NO2 + GLY + OP2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
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
| ROCARO32   | BENP + HO2 ---->   0.6021\*OP2 +    0.3979\*VROCN1OXY6  |   2.91E-13e<sup>  1300.00/T</sup> |   2.2778E-11 |
| ROCARO42   | TOLP + HO2 ---->   0.7195\*OP2 +    0.2805\*VROCN1OXY6  |   2.91E-13e<sup>  1300.00/T</sup> |   2.2778E-11 |
| ROCARO52   | XYLP + HO2 ---->   0.0482\*OP2 +    0.6747\*OP3 +    0.2771\*VROCP0OXY4  |   2.91E-13e<sup>  1300.00/T</sup> |   2.2778E-11 |
| ROCARO62   | EBZP + HO2 ---->   0.0854\*OP2 +    0.6341\*OP3 +    0.2805\*VROCP0OXY4  |   2.91E-13e<sup>  1300.00/T</sup> |   2.2778E-11 |
| R229   | APIP1 + HO2 ---->   0.0390\*ACT +    0.0390\*LIMAL +    0.0625\*HCHO +    0.7620\*OPB +    0.3225\*HO2 +    0.1885\*PINAL +    0.2380\*HO +    0.0105\*KET  |   2.60E-13e<sup>  1300.00/T</sup> |   2.0351E-11 |
| TRP21   | APIP2 + HO2 ----> HOM  |   1.5000E-11 |   1.5000E-11 |
| TRP22   | APINP1 + HO2 ---->   0.4610\*TRPN +    0.4550\*PINAL +    0.5390\*NO2 +    0.6195\*HO +    0.0770\*ALD +    0.0070\*KET +    0.0070\*HCHO  |   2.71E-13e<sup>  1300.00/T</sup> |   2.1212E-11 |
| TRP23   | APINP2 + HO2 ----> HOM  |   1.5000E-11 |   1.5000E-11 |
| R230   | LIMP1 + HO2 ---->   0.9000\*OPB +    0.1000\*LIMAL +    0.1000\*HO +    0.1000\*HO2 +    0.0600\*HCHO  |   2.60E-13e<sup>  1300.00/T</sup> |   2.0351E-11 |
| TRP24   | LIMP2 + HO2 ----> HOM  |   1.5000E-11 |   1.5000E-11 |
| TRP25   | LIMNP1 + HO2 ---->   0.7700\*TRPN +    0.5000\*HO +    0.2300\*LIMAL +    0.2300\*NO2 +    0.2700\*HO2  |   2.71E-13e<sup>  1300.00/T</sup> |   2.1212E-11 |
| TRP26   | LIMNP2 + HO2 ----> HOM  |   1.5000E-11 |   1.5000E-11 |
| TRP27   | PINALP + HO2 ---->   0.7500\*OPB +    0.2500\*HO +    0.2500\*HOM  |   2.71E-13e<sup>  1300.00/T</sup> |   2.1212E-11 |
| TRP28   | LIMALP + HO2 ---->   0.9000\*OPB +    0.1000\*HO +    0.1000\*HO2 +    0.1000\*HCHO +    0.1000\*PAA  |   2.73E-13e<sup>  1300.00/T</sup> |   2.1369E-11 |
| R231   | ACO3 + HO2 ---->   0.4400\*HO +    0.4400\*MO2 +    0.1500\*ORA2 +    0.4100\*PAA  |   4.30E-13e<sup>  1040.00/T</sup> |   1.4072E-11 |
| R232   | RCO3 + HO2 ---->   0.4400\*HO +    0.4400\*ETHP +    0.1500\*ORA2 +    0.4100\*PAA  |   4.30E-13e<sup>  1040.00/T</sup> |   1.4072E-11 |
| R233   | ACTP + HO2 ---->   0.1500\*HO +    0.1500\*ACO3 +    0.1500\*HCHO +    0.8500\*OP2  |   1.15E-13e<sup>  1300.00/T</sup> |   9.0016E-12 |
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
| R248   | MO2 + MO2 ---->   0.7400\*HO2 +    1.3700\*HCHO +    0.6300\*MOH  |   9.50E-14e<sup>   390.00/T</sup> |   3.5141E-13 |
| R249   | ETHP + MO2 ----> HO2 +    0.7500\*HCHO +    0.7500\*ACD +    0.2500\*MOH +    0.2500\*EOH  |   1.18E-13e<sup>   158.00/T</sup> |   2.0046E-13 |
| R250   | HC3P + MO2 ---->   0.8940\*HO2 +    0.0800\*MO2 +    0.0260\*ETHP +    0.0260\*XO2 +    0.8270\*HCHO +    0.1980\*ALD +    0.4970\*KET +    0.0500\*GLY +    0.2500\*MOH +    0.2500\*ROH  |   9.46E-14e<sup>   431.00/T</sup> |   4.0151E-13 |
| R251   | HC5P + MO2 ---->   0.8420\*HO2 +    0.0180\*MO2 +    0.1400\*ETHP +    0.1910\*XO2 +    0.7770\*HCHO +    0.2510\*ALD +    0.6180\*KET +    0.2500\*MOH +    0.2500\*ROH  |   1.00E-13e<sup>   467.00/T</sup> |   4.7890E-13 |
| R253   | ETEP + MO2 ----> HO2 +    1.9500\*HCHO +    0.1500\*ALD +    0.2500\*MOH +    0.2500\*ETEG  |   1.71E-13e<sup>   708.00/T</sup> |   1.8378E-12 |
| R254   | OLTP + MO2 ----> HO2 +    1.5000\*HCHO +    0.7050\*ALD +    0.0450\*KET +    0.2500\*MOH +    0.2500\*ROH  |   1.46E-13e<sup>   708.00/T</sup> |   1.5691E-12 |
| R255   | OLIP + MO2 ----> HO2 +    0.7500\*HCHO +    1.2800\*ALD +    0.2180\*KET +    0.2500\*MOH +    0.2500\*ROH  |   9.18E-14e<sup>   708.00/T</sup> |   9.8659E-13 |
| ROCARO35   | BENP + MO2 ---->   0.6800\*HCHO +    1.3700\*HO2 +    0.3200\*MOH +    0.0000\*BALD + GLY +    0.5000\*FURANONE +    0.2500\*DCB2 +    0.2500\*DCB3  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| ROCARO45   | TOLP + MO2 ---->   0.6800\*HCHO +    1.2846\*HO2 +    0.3200\*MOH +    0.0854\*BALD +    0.5488\*GLY +    0.3659\*MGLY +    0.3659\*FURANONE +    0.5488\*DCB1  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| ROCARO55   | XYLP + MO2 ---->   0.6800\*HCHO +    1.3218\*HO2 +    0.3200\*MOH +    0.0482\*BALD +    0.7043\*GLY +    0.2475\*MGLY +    0.3522\*FURANONE +    0.5996\*DCB2  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| ROCARO65   | EBZP + MO2 ---->   0.6800\*HCHO +    1.2846\*HO2 +    0.3200\*MOH +    0.0854\*BALD +    0.5488\*GLY +    0.3659\*MGLY +    0.4573\*FURANONE +    0.4573\*DCB2  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| R264   | ISOP + MO2 ----> HO2 +    1.3100\*HCHO +    0.1590\*MACR +    0.2500\*MVK +    0.2500\*MOH +    0.2500\*ROH +    0.0230\*ALD +    0.0180\*GLY +    0.0160\*HKET  |   3.40E-14e<sup>   221.00/T</sup> |   7.1350E-14 |
| R265   | APIP1 + MO2 ---->   1.0295\*HCHO +    0.2205\*LIMAL +    0.2730\*PINAL +    0.2385\*ALD +    0.0845\*OPB +    0.1105\*MOH +    0.1835\*KET +    0.0670\*ACT +    1.2790\*HO2  |   2.0000E-12 |   2.0000E-12 |
| TRP29   | APIP2 + MO2 ----> HO2 +    0.7500\*HCHO +    0.2500\*MOH + HOM  |   1.0000E-10 |   1.0000E-10 |
| TRP30   | APINP1 + MO2 ---->   0.3410\*TRPN +    0.9640\*HCHO +    0.0430\*MOH +    0.9180\*HO2 +    0.6590\*NO2 +    0.5330\*PINAL +    0.0070\*KET +    0.1190\*ALD  |   2.0000E-12 |   2.0000E-12 |
| TRP31   | APINP2 + MO2 ---->   0.7500\*HO2 +    0.7500\*NO2 +    0.2500\*MOH +    0.7500\*HCHO + HOM  |   1.0000E-10 |   1.0000E-10 |
| R266   | LIMP1 + MO2 ---->   0.2500\*MOH + LIMAL +    1.0300\*HCHO + HO2  |   2.0000E-12 |   2.0000E-12 |
| TRP32   | LIMP2 + MO2 ----> HO2 +    0.7500\*HCHO +    0.2500\*MOH + HOM  |   1.0000E-10 |   1.0000E-10 |
| TRP33   | LIMNP1 + MO2 ---->   0.6900\*TRPN +    0.9100\*HCHO +    0.0900\*MOH +    1.0100\*HO2 +    0.3100\*LIMAL +    0.3100\*NO2  |   2.0000E-12 |   2.0000E-12 |
| TRP34   | LIMNP2 + MO2 ---->   0.7500\*HO2 +    0.7500\*HCHO +    0.7500\*NO2 +    0.2500\*MOH + HOM  |   1.0000E-10 |   1.0000E-10 |
| R267   | ACO3 + MO2 ---->   0.9000\*HO2 +    0.9000\*MO2 + HCHO +    0.1000\*ORA2  |   2.00E-11e<sup>   500.00/T</sup> |   1.0699E-10 |
| R268   | RCO3 + MO2 ---->   0.9000\*HO2 +    0.9000\*MO2 + HCHO +    0.1000\*ORA2  |   2.00E-11e<sup>   500.00/T</sup> |   1.0699E-10 |
| R269   | ACTP + MO2 ---->   0.5000\*HO2 +    0.5000\*ACO3 +    1.5000\*HCHO +    0.2500\*MOH +    0.2500\*ROH +    0.1250\*ORA2  |   7.50E-13e<sup>   500.00/T</sup> |   4.0121E-12 |
| R270   | MEKP + MO2 ---->   0.8340\*HO2 + HCHO +    0.3340\*DCB1 +    0.2500\*MOH +    0.2500\*ROH  |   6.91E-13e<sup>   508.00/T</sup> |   3.7971E-12 |
| R271   | KETP + MO2 ----> HO2 +    0.7500\*HCHO +    0.5000\*DCB1 +    0.2500\*MOH +    0.2500\*ROH  |   6.91E-13e<sup>   508.00/T</sup> |   3.7971E-12 |
| R272   | MACP + MO2 ---->   0.5000\*HO2 +    0.2690\*ACO3 +    0.5000\*CO +    1.6600\*HCHO +    0.0670\*ORA2 +    0.2500\*MO2 +    0.2500\*MOH +    0.2500\*ROH  |   3.40E-14e<sup>   221.00/T</sup> |   7.1350E-14 |
| R273   | MCP + MO2 ----> NO2 + HO2 +    1.5000\*HCHO +    0.5000\*HKET +    0.2500\*MOH +    0.2500\*ROH  |   3.40E-14e<sup>   221.00/T</sup> |   7.1350E-14 |
| R274   | MVKP + MO2 ----> HO2 +    1.1600\*ACO3 +    1.1600\*XO2 +    1.5000\*HCHO +    1.7500\*ALD +    0.5000\*MGLY +    0.2500\*MOH +    0.2500\*ROH +    0.2920\*ORA2  |   8.3700E-14 |   8.3700E-14 |
| R275   | UALP + MO2 ----> HO2 +    0.3050\*CO +    0.7730\*HCHO +    0.2030\*ALD +    0.5250\*KET +    0.1350\*GLY +    0.1050\*MGLY +    0.2500\*MOH +    0.2500\*ROH  |   3.40E-14e<sup>   221.00/T</sup> |   7.1350E-14 |
| R276   | BALP + MO2 ----> HO2 + BAL1 + HCHO  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| R277   | BAL1 + MO2 ----> HO2 + BAL2 + HCHO  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| R278   | ADDC + MO2 ---->   2.0000\*HO2 + HCHO +    0.3200\*HKET +    0.6800\*GLY +    0.6800\*OP2  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| R279   | MCTP + MO2 ----> HO2 + MCTO + HCHO  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| R280   | ORAP + MO2 ----> HCHO + HO2 + GLY  |   7.50E-13e<sup>   500.00/T</sup> |   4.0121E-12 |
| R281   | OLNN + MO2 ---->   2.0000\*HO2 + HCHO + ONIT  |   1.60E-13e<sup>   708.00/T</sup> |   1.7195E-12 |
| R282   | OLND + MO2 ---->   0.5000\*HO2 +    0.5000\*NO2 +    0.9650\*HCHO +    0.9300\*ALD +    0.3480\*KET +    0.2500\*MOH +    0.2500\*ROH +    0.5000\*ONIT  |   9.68E-14e<sup>   708.00/T</sup> |   1.0403E-12 |
| R283   | ADCN + MO2 ----> HO2 +    0.7000\*NO2 + HCHO +    0.7000\*GLY +    0.7000\*OP2 +    0.3000\*ONIT  |   3.5600E-14 |   3.5600E-14 |
| R284   | XO2 + MO2 ----> HO2 + HCHO  |   5.99E-15e<sup>  1510.00/T</sup> |   9.4829E-13 |
| R285   | ETHP + ACO3 ---->   0.5000\*HO2 +    0.5000\*MO2 + ACD +    0.5000\*ORA2  |   1.03E-12e<sup>   211.00/T</sup> |   2.0902E-12 |
| R286   | HC3P + ACO3 ---->   0.3940\*HO2 +    0.5800\*MO2 +    0.0260\*ETHP +    0.0260\*XO2 +    0.1300\*HCHO +    0.2730\*ALD +    0.6620\*KET +    0.0670\*GLY +    0.5000\*ORA2  |   6.90E-13e<sup>   460.00/T</sup> |   3.2277E-12 |
| R287   | HC5P + ACO3 ---->   0.3420\*HO2 +    0.5180\*MO2 +    0.1400\*ETHP +    0.1910\*XO2 +    0.0420\*HCHO +    0.3810\*ALD +    0.8240\*KET +    0.5000\*ORA2  |   5.59E-13e<sup>   522.00/T</sup> |   3.2194E-12 |
| R289   | ETEP + ACO3 ---->   0.5000\*HO2 +    0.5000\*MO2 +    1.6000\*HCHO +    0.2000\*ALD +    0.5000\*ORA2  |   9.48E-13e<sup>   765.00/T</sup> |   1.2335E-11 |
| R290   | OLTP + ACO3 ---->   0.5000\*HO2 +    0.5000\*MO2 + HCHO +    0.9400\*ALD +    0.0600\*KET +    0.5000\*ORA2  |   8.11E-13e<sup>   765.00/T</sup> |   1.0552E-11 |
| R291   | OLIP + ACO3 ---->   0.5000\*HO2 +    0.5000\*MO2 +    1.7100\*ALD +    0.2900\*KET +    0.5000\*ORA2  |   5.09E-13e<sup>   765.00/T</sup> |   6.6228E-12 |
| ROCARO36   | BENP + ACO3 ---->   0.7000\*MO2 + HO2 +    0.3000\*ORA2 +    0.0000\*BALD + GLY +    0.5000\*FURANONE +    0.2500\*DCB2 +    0.2500\*DCB3  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| ROCARO46   | TOLP + ACO3 ---->   0.7000\*MO2 +    0.9146\*HO2 +    0.3000\*ORA2 +    0.0854\*BALD +    0.5488\*GLY +    0.3659\*MGLY +    0.3659\*FURANONE +    0.5488\*DCB1  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| ROCARO56   | XYLP + ACO3 ---->   0.7000\*MO2 +    0.9518\*HO2 +    0.3000\*ORA2 +    0.0482\*BALD +    0.7043\*GLY +    0.2475\*MGLY +    0.3522\*FURANONE +    0.5996\*DCB2  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| ROCARO66   | EBZP + ACO3 ---->   0.7000\*MO2 +    0.9146\*HO2 +    0.3000\*ORA2 +    0.0854\*BALD +    0.5488\*GLY +    0.3659\*MGLY +    0.4573\*FURANONE +    0.4573\*DCB2  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| R300   | ISOP + ACO3 ---->   0.5000\*HO2 +    0.5000\*MO2 +    1.0480\*HCHO +    0.2190\*MACR +    0.3050\*MVK +    0.5000\*ORA2  |   8.40E-14e<sup>   221.00/T</sup> |   1.7628E-13 |
| R301   | APIP1 + ACO3 ---->   0.2535\*PINAL +    0.3710\*ALD +    0.0910\*OPB +    0.1165\*ACT +    0.1725\*LIMAL +    0.4030\*HCHO + HO2 + MO2 +    0.1120\*KET  |   2.00E-12e<sup>   500.00/T</sup> |   1.0699E-11 |
| TRP35   | APIP2 + ACO3 ---->   0.5000\*HO +    0.5000\*MO2 +    0.5000\*ORA2 + HOM  |   1.0000E-10 |   1.0000E-10 |
| TRP36   | APINP1 + ACO3 ---->   0.8250\*NO2 +    0.6500\*PINAL + MO2 +    0.1750\*TRPN +    0.1680\*ALD +    0.0070\*KET +    0.0070\*HCHO +    0.1750\*HO2  |   2.00E-12e<sup>   500.00/T</sup> |   1.0699E-11 |
| TRP37   | APINP2 + ACO3 ---->   0.5000\*NO2 +    0.5000\*MO2 +    0.5000\*ORA2 + HOM  |   1.0000E-10 |   1.0000E-10 |
| R302   | LIMP1 + ACO3 ----> LIMAL +    0.5600\*HCHO + HO2 + MO2  |   2.00E-12e<sup>   500.00/T</sup> |   1.0699E-11 |
| TRP38   | LIMP2 + ACO3 ---->   0.5000\*HO +    0.5000\*MO2 +    0.5000\*ORA2 + HOM  |   1.0000E-10 |   1.0000E-10 |
| TRP39   | LIMNP1 + ACO3 ----> MO2 +    0.4600\*NO2 +    0.4600\*LIMAL +    0.5400\*TRPN +    0.5400\*HO2  |   2.00E-12e<sup>   500.00/T</sup> |   1.0699E-11 |
| TRP40   | LIMNP2 + ACO3 ---->   0.5000\*MO2 +    0.5000\*NO2 +    0.5000\*ORA2 + HOM  |   1.0000E-10 |   1.0000E-10 |
| R303   | ACO3 + ACO3 ---->   2.0000\*MO2  |   2.50E-12e<sup>   500.00/T</sup> |   1.3374E-11 |
| R304   | RCO3 + ACO3 ----> MO2 + ETHP  |   2.50E-12e<sup>   500.00/T</sup> |   1.3374E-11 |
| R305   | ACTP + ACO3 ---->   0.5000\*MO2 +    0.5000\*ACO3 + HCHO +    0.7500\*ORA2  |   7.51E-13e<sup>   565.00/T</sup> |   4.9962E-12 |
| R306   | MEKP + ACO3 ---->   0.3300\*HO2 +    0.5000\*MO2 +    0.3300\*HCHO +    0.3340\*DCB1 +    0.5000\*ORA2  |   7.51E-13e<sup>   565.00/T</sup> |   4.9962E-12 |
| R307   | KETP + ACO3 ---->   0.5000\*HO2 +    0.5000\*MO2 +    0.5000\*DCB1 +    0.5000\*ORA2  |   7.51E-13e<sup>   565.00/T</sup> |   4.9962E-12 |
| R308   | MACP + ACO3 ---->   0.6350\*ORA2 +    0.5000\*MO2 +    0.2690\*ACO3 +    0.5000\*CO + HCHO  |   8.40E-14e<sup>   221.00/T</sup> |   1.7628E-13 |
| R309   | MCP + ACO3 ----> NO2 +    0.5000\*HO2 + HCHO +    0.5000\*HKET +    0.5000\*MO2 +    0.5000\*ORA2  |   8.40E-14e<sup>   221.00/T</sup> |   1.7628E-13 |
| R310   | MVKP + ACO3 ---->   0.5000\*HO2 +    0.5000\*MO2 +    1.1600\*ACO3 +    1.1600\*XO2 + HCHO +    2.3000\*ALD +    0.5000\*MGLY +    1.0830\*ORA2  |   1.68E-12e<sup>   500.00/T</sup> |   8.9872E-12 |
| R311   | UALP + ACO3 ---->   0.5000\*HO2 +    0.5000\*MO2 +    0.5000\*CO +    0.0300\*HCHO +    0.2700\*ALD +    0.7000\*KET +    0.1800\*GLY +    0.1050\*MGLY +    0.5000\*ORA2  |   1.68E-12e<sup>   500.00/T</sup> |   8.9872E-12 |
| R312   | BALP + ACO3 ----> MO2 + BAL1  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| R313   | BAL1 + ACO3 ----> MO2 + BAL2  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| R314   | ADDC + ACO3 ---->   2.0000\*HO2 + MO2 +    0.3200\*HKET +    0.6800\*GLY +    0.6800\*OP2  |   7.40E-13e<sup>   708.00/T</sup> |   7.9529E-12 |
| R315   | MCTP + ACO3 ----> HO2 + MO2 + MCTO  |   7.40E-13e<sup>   708.00/T</sup> |   7.9529E-12 |
| R316   | ORAP + ACO3 ----> MO2 + GLY  |   7.51E-13e<sup>   565.00/T</sup> |   4.9962E-12 |
| R317   | OLNN + ACO3 ----> HO2 + MO2 + ONIT  |   8.85E-13e<sup>   765.00/T</sup> |   1.1515E-11 |
| R318   | OLND + ACO3 ---->   0.5000\*MO2 + NO2 +    0.2870\*HCHO +    1.2400\*ALD +    0.4640\*KET +    0.5000\*ORA2  |   5.37E-13e<sup>   765.00/T</sup> |   6.9871E-12 |
| R319   | ADCN + ACO3 ----> HO2 + MO2 +    0.7000\*NO2 +    0.7000\*GLY +    0.7000\*OP2 +    0.3000\*ONIT  |   7.40E-13e<sup>   708.00/T</sup> |   7.9529E-12 |
| R320   | XO2 + ACO3 ----> MO2  |   3.40E-14e<sup>  1560.00/T</sup> |   6.3654E-12 |
| R321   | RCO3 + RCO3 ---->   2.0000\*ETHP  |   2.50E-12e<sup>   500.00/T</sup> |   1.3374E-11 |
| R322   | MO2 + NO3 ----> HO2 + HCHO + NO2  |   1.2000E-12 |   1.2000E-12 |
| R323   | ETHP + NO3 ----> HO2 + NO2 + ACD  |   1.2000E-12 |   1.2000E-12 |
| R324   | HC3P + NO3 ---->   0.2540\*HO2 +    0.1400\*MO2 +    0.0920\*XO2 +    0.5030\*ETHP + NO2 +    0.5190\*ACD +    0.1470\*ALD +    0.0750\*MEK +    0.0950\*ACT  |   1.2000E-12 |   1.2000E-12 |
| R325   | HC5P + NO3 ---->   0.4880\*HO2 +    0.0550\*MO2 +    0.2800\*ETHP +    0.4850\*XO2 + NO2 +    0.0240\*HCHO +    0.2410\*ALD +    0.0600\*KET +    0.0630\*MEK +    0.2470\*ACT +    0.0480\*ACD +    0.2750\*HKET  |   1.2000E-12 |   1.2000E-12 |
| R327   | ETEP + NO3 ----> HO2 + NO2 +    1.6000\*HCHO +    0.2000\*ALD  |   1.2000E-12 |   1.2000E-12 |
| R328   | OLTP + NO3 ---->   0.4700\*ALD +    0.7900\*HCHO +    0.7900\*HO2 + NO2 +    0.1800\*MEK +    0.0200\*ACD +    0.0900\*ACT  |   1.2000E-12 |   1.2000E-12 |
| R329   | OLIP + NO3 ---->   0.8600\*HO2 +    0.7200\*ALD +    0.1100\*KET + NO2 +    0.2000\*ACT +    0.8500\*ACD +    0.0400\*HKET  |   1.2000E-12 |   1.2000E-12 |
| ROCARO34   | BENP + NO3 ----> NO2 + HO2 +    0.0000\*BALD + GLY +    0.5000\*FURANONE +    0.2500\*DCB2 +    0.2500\*DCB3  |   2.3000E-12 |   2.3000E-12 |
| ROCARO44   | TOLP + NO3 ----> NO2 +    0.9146\*HO2 +    0.0854\*BALD +    0.5488\*GLY +    0.3659\*MGLY +    0.3659\*FURANONE +    0.5488\*DCB1  |   2.3000E-12 |   2.3000E-12 |
| ROCARO54   | XYLP + NO3 ----> NO2 +    0.9518\*HO2 +    0.0482\*BALD +    0.7043\*GLY +    0.2475\*MGLY +    0.3522\*FURANONE +    0.5996\*DCB2  |   2.3000E-12 |   2.3000E-12 |
| ROCARO64   | EBZP + NO3 ----> NO2 +    0.9146\*HO2 +    0.0854\*BALD +    0.5488\*GLY +    0.3659\*MGLY +    0.4573\*FURANONE +    0.4573\*DCB2  |   2.3000E-12 |   2.3000E-12 |
| R338   | ISOP + NO3 ----> HO2 + NO2 +    0.7500\*HCHO +    0.3180\*MACR +    0.5000\*MVK +    0.0240\*GLY +    0.0330\*HKET +    0.0310\*ALD  |   1.2000E-12 |   1.2000E-12 |
| R339   | APIP1 + NO3 ----> NO2 + HO2 +    0.2535\*PINAL +    0.3710\*ALD +    0.1165\*ACT +    0.1725\*LIMAL +    0.4030\*HCHO +    0.0910\*OPB +    0.1120\*KET  |   2.3000E-12 |   2.3000E-12 |
| R340   | LIMP1 + NO3 ----> HO2 + NO2 + LIMAL +    0.5600\*HCHO  |   2.3000E-12 |   2.3000E-12 |
| TRP53   | APINP1 + NO3 ---->   1.8250\*NO2 +    0.6500\*PINAL +    0.1750\*TRPN +    0.1680\*ALD +    0.0070\*KET +    0.0070\*HCHO +    0.1750\*HO2  |   2.3000E-12 |   2.3000E-12 |
| TRP54   | LIMNP1 + NO3 ---->   1.4600\*NO2 +    0.4600\*LIMAL +    0.5400\*TRPN +    0.5400\*HO2  |   2.3000E-12 |   2.3000E-12 |
| R341   | ACO3 + NO3 ----> MO2 + NO2  |   4.0000E-12 |   4.0000E-12 |
| R342   | RCO3 + NO3 ----> ETHP + NO2  |   4.0000E-12 |   4.0000E-12 |
| R343   | ACTP + NO3 ----> ACO3 + NO2 + HCHO  |   1.2000E-12 |   1.2000E-12 |
| R344   | MEKP + NO3 ---->   0.6700\*HO2 + NO2 +    0.3300\*HCHO +    0.6700\*DCB1  |   1.2000E-12 |   1.2000E-12 |
| R345   | KETP + NO3 ----> HO2 + NO2 + DCB1  |   1.2000E-12 |   1.2000E-12 |
| R346   | MACP + NO3 ----> HCHO +    0.5380\*ACO3 + CO + NO2  |   1.2000E-12 |   1.2000E-12 |
| R347   | MCP + NO3 ----> NO2 + HO2 + HCHO + HKET  |   1.2000E-12 |   1.2000E-12 |
| R348   | MVKP + NO3 ---->   0.3000\*HO2 +    0.7000\*ACO3 +    0.7000\*XO2 + NO2 +    0.3000\*HCHO +    0.7000\*ALD +    0.3000\*MGLY  |   2.5000E-12 |   2.5000E-12 |
| R349   | UALP + NO3 ----> HO2 + NO2 +    0.6100\*CO +    0.0300\*HCHO +    0.2700\*ALD +    0.7000\*KET +    0.1800\*GLY +    0.2100\*MGLY  |   2.5000E-12 |   2.5000E-12 |
| R350   | BALP + NO3 ----> BAL1 + NO2  |   2.5000E-12 |   2.5000E-12 |
| R351   | BAL1 + NO3 ----> BAL2 + NO2  |   2.5000E-12 |   2.5000E-12 |
| R352   | ADDC + NO3 ----> HO2 + NO2 +    0.3200\*HKET +    0.6800\*GLY +    0.6800\*OP2  |   1.2000E-12 |   1.2000E-12 |
| R353   | MCTP + NO3 ----> NO2 + MCTO  |   1.2000E-12 |   1.2000E-12 |
| R354   | ORAP + NO3 ----> NO2 + GLY + HO2  |   1.2000E-12 |   1.2000E-12 |
| R355   | OLNN + NO3 ----> HO2 + NO2 + ONIT  |   1.2000E-12 |   1.2000E-12 |
| R356   | OLND + NO3 ---->   2.0000\*NO2 +    0.2870\*HCHO +    1.2400\*ALD +    0.4640\*KET  |   1.2000E-12 |   1.2000E-12 |
| R357   | ADCN + NO3 ---->   2.0000\*NO2 + GLY + OP2  |   1.2000E-12 |   1.2000E-12 |
| R358   | OLNN + OLNN ----> HO2 +    2.0000\*ONIT  |   7.00E-14e<sup>  1000.00/T</sup> |   2.0032E-12 |
| R359   | OLNN + OLND ---->   0.5000\*HO2 +    0.5000\*NO2 +    0.2020\*HCHO +    0.6400\*ALD +    0.1490\*KET +    1.5000\*ONIT  |   4.25E-14e<sup>  1000.00/T</sup> |   1.2162E-12 |
| R360   | OLND + OLND ----> NO2 +    0.5040\*HCHO +    1.2100\*ALD +    0.2850\*KET + ONIT  |   2.96E-14e<sup>  1000.00/T</sup> |   8.4708E-13 |
| R361   | XO2 + NO3 ----> NO2  |   1.2000E-12 |   1.2000E-12 |
| R362   | XO2 + RCO3 ----> ETHP  |   2.50E-12e<sup>   500.00/T</sup> |   1.3374E-11 |
| R363   | XO2 + XO2 ----> |   7.13E-17e<sup>  2950.00/T</sup> |   1.4130E-12 |
| TRP41   | APIP2 + APIP1 ---->   0.9600\*HOM +    0.4800\*ROH +    0.4800\*PINAL +    0.4800\*HO +    0.4800\*HO2 +    0.0400\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP42   | APIP2 + LIMP1 ---->   0.9600\*HOM +    0.4800\*ROH +    0.4800\*LIMAL +    0.4800\*HO +    0.4800\*HO2 +    0.0400\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP43   | APIP2 + ISOP ---->   0.9600\*HOM +    0.4800\*ROH +    0.4800\*HCHO +    0.4800\*MVK +    0.4800\*HO +    0.4800\*HO2 +    0.0400\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP44   | LIMP2 + APIP1 ---->   0.9600\*HOM +    0.4800\*ROH +    0.4800\*PINAL +    0.4800\*HO +    0.4800\*HO2 +    0.0400\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP45   | LIMP2 + LIMP1 ---->   0.9600\*HOM +    0.4800\*ROH +    0.4800\*LIMAL +    0.4800\*HO +    0.4800\*HO2 +    0.0400\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP46   | LIMP2 + ISOP ---->   0.9600\*HOM +    0.4800\*ROH +    0.4800\*HCHO +    0.4800\*MVK +    0.4800\*HO +    0.4800\*HO2 +    0.0400\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP47   | APINP2 + APIP1 ---->   0.9600\*HOM +    0.4800\*ROH +    0.4800\*PINAL +    0.4800\*NO2 +    0.4800\*HO2 +    0.0400\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP48   | APINP2 + LIMP1 ---->   0.9600\*HOM +    0.4800\*ROH +    0.4800\*LIMAL +    0.4800\*NO2 +    0.4800\*HO2 +    0.0400\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP49   | APINP2 + ISOP ---->   0.9600\*HOM +    0.4800\*ROH +    0.4800\*HCHO +    0.4800\*MVK +    0.4800\*NO2 +    0.4800\*HO2 +    0.0400\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP50   | LIMNP2 + APIP1 ---->   0.9600\*HOM +    0.4800\*ROH +    0.4800\*PINAL +    0.4800\*NO2 +    0.4800\*HO2 +    0.0400\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP51   | LIMNP2 + LIMP1 ---->   0.9600\*HOM +    0.4800\*ROH +    0.4800\*LIMAL +    0.4800\*NO2 +    0.4800\*HO2 +    0.0400\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| TRP52   | LIMNP2 + ISOP ---->   0.9600\*HOM +    0.4800\*ROH +    0.4800\*HCHO +    0.4800\*MVK +    0.4800\*NO2 +    0.4800\*HO2 +    0.0400\*ELHOM  |   1.0000E-10 |   1.0000E-10 |
| RAM17   | IEPOX + HO ----> HO  |   5.78E-11e<sup>  -400.00/T</sup> |   1.5110E-11 |
| R001c   | VROCIOXY + HO ---->   0.8520\*ETHP +    0.1490\*ASOATJ  |   6.8900E-12 |   6.8900E-12 |
| R002c   | SLOWROC + HO ----> ETHP +    0.0010\*ASOATJ  |   6.5500E-14 |   6.5500E-14 |
| T17   | ACRO + HO ---->   0.5700\*MACP +    0.4300\*MCP  |   8.00E-12e<sup>   380.00/T</sup> |   2.8616E-11 |
| T18   | ACRO + O3 ---->   0.8400\*CO +    0.5600\*HO2 +    0.2800\*HO +    0.7200\*HCHO +    0.6200\*GLY  |   2.9000E-19 |   2.9000E-19 |
| T19   | ACRO + NO3 ---->   0.6800\*HCHO +    0.3200\*MACP +    0.6800\*XO2 +    0.6800\*MGLY +    0.3200\*HNO3 +    0.6800\*NO2  |   3.4000E-15 |   3.4000E-15 |
| T20   | ACRO ----> CO +    0.4770\*HO2 +    0.2500\*ETE +    0.3540\*ACO3 +    0.2040\*HO +    0.1500\*HCHO +    0.0270\*MO2  | ACRO_09 | Not Available<sup>1</sup> | 
| T10   | BDE13 + HO ---->   0.6670\*BDE13P +    0.3330\*UALD +    0.3330\*HO2  |   1.48E-11e<sup>   448.00/T</sup> |   6.6502E-11 |
| T10a   | BDE13P + NO ---->   0.9680\*HO2 +    0.9680\*NO2 +    0.8950\*ACRO +    0.8950\*HCHO +    0.0720\*FURAN +    0.0320\*ONIT  |   9.0500E-12 |   9.0500E-12 |
| T10b   | BDE13P + NO3 ----> HO2 + NO2 +    0.9250\*ACRO +    0.9250\*HCHO +    0.0750\*FURAN  |   2.3000E-12 |   2.3000E-12 |
| T10c   | BDE13P + HO2 ----> OP2  |   1.6100E-11 |   1.6100E-11 |
| T10d   | BDE13P + MO2 ---->   0.3200\*MOH +    1.1430\*HCHO +    0.8700\*HO2 +    0.4630\*ACRO +    0.2500\*OLT +    0.2310\*MVK +    0.0370\*FURAN +    0.0190\*UALD  |   2.3900E-12 |   2.3900E-12 |
| T10e   | BDE13P + ACO3 ---->   0.7000\*MO2 +    0.3000\*ORA2 +    0.8000\*HO2 +    0.7400\*ACRO +    0.7400\*HCHO +    0.1850\*MVK +    0.0600\*FURAN +    0.0150\*UALD  |   1.3700E-11 |   1.3700E-11 |
| T11   | BDE13 + O3 ---->   0.6200\*ACRO +    0.6300\*CO +    0.4200\*HO2 +    0.0800\*HO +    0.8300\*HCHO +    0.1700\*ETE  |   1.34E-14e<sup> -2283.00/T</sup> |   6.3331E-18 |
| T12   | BDE13 + NO3 ---->   0.9000\*OLNN +    0.1000\*OLND +    0.9000\*ACRO  |   1.0000E-13 |   1.0000E-13 |
| R003c   | FURAN + HO ---->   0.4900\*DCB1 +    0.4900\*HO2 +    0.5100\*FURANO2  |   5.0100E-11 |   5.0100E-11 |
| R004c   | FURANO2 + NO ---->   0.0800\*ONIT +    0.9200\*NO2 +    0.9200\*FURANONE +    0.7500\*HO2 +    0.1700\*MO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R005c   | FURANO2 + HO2 ---->   0.6000\*OP2 +    0.4000\*FURANONE +    0.4000\*HO +    0.3200\*HO2 +    0.0800\*MO2  |   3.75E-13e<sup>   980.00/T</sup> |   1.0035E-11 |
| R006c   | FURANONE + HO ---->   0.6500\*KET +    0.3100\*GLY +    0.6600\*HO2 +    0.3400\*MO2 +    0.4300\*CO +    0.0400\*ASOATJ  |   4.4000E-11 |   4.4000E-11 |
| R007c   | FURAN + O3 ---->   0.0200\*HO + ALD  |   3.4300E-17 |   3.4300E-17 |
| R008c   | FURAN + NO3 ----> NO2 +    0.8000\*DCB1 +    0.2000\*DCB3  |   8.9900E-12 |   8.9900E-12 |
| R010c   | PROG + HO ---->   0.6130\*HKET +    0.3870\*ALD + HO2  |   1.2000E-11 |   1.2000E-11 |
| R011c   | SESQ + NO3 ----> SESQNRO2  |   1.9000E-11 |   1.9000E-11 |
| R012c   | SESQNRO2 + HO2 ----> VROCP0OXY4  |   2.84E-13e<sup>  1300.00/T</sup> |   2.2230E-11 |
| R013c   | SESQNRO2 + NO ----> VROCP3OXY2 +    2.0000\*NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| R014c   | SESQNRO2 + NO3 ----> VROCP3OXY2 +    2.0000\*NO2  |   2.3000E-12 |   2.3000E-12 |
| R015c   | SESQ + O3 ---->   0.9820\*VROCP3OXY2 +    0.0180\*VROCN2OXY2  |   1.2000E-14 |   1.2000E-14 |
| R016c   | SESQ + HO ----> SESQRO2  |   1.9700E-10 |   1.9700E-10 |
| R017c   | SESQRO2 + HO2 ----> VROCP0OXY2  |   2.84E-13e<sup>  1300.00/T</sup> |   2.2230E-11 |
| R019c   | SESQRO2 + NO3 ----> VROCP3OXY2  |   2.3000E-12 |   2.3000E-12 |
| R020c   | SESQRO2 + NO ---->   0.2470\*VROCP1OXY3 +    0.7530\*VROCP3OXY2 +    0.7530\*NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| HET_GLY   | GLY ----> AGLYJ  | HETERO_GLY | Not Available<sup>2</sup> | 
| HET_MGLY   | MGLY ----> AGLYJ  | HETERO_MGLY | Not Available<sup>2</sup> | 
| HET_N2O5   | N2O5 ---->   2.0000\*HNO3  | HETERO_N2O5IJ | Not Available<sup>2</sup> | 
| HET_NO2   | NO2 ---->   0.5000\*HONO +    0.5000\*HNO3  | HETERO_NO2 | Not Available<sup>2</sup> | 
| HET_HO2   | HO2 ----> | HETERO_HO2 | Not Available<sup>2</sup> | 
| HET_NO3   | NO3 ----> HNO3  | HETERO_NO3 | Not Available<sup>2</sup> | 
| HAL_Ozone   | O3 ----> | SEAWATER*min( 6.701E-11e<sup> 1.074E+01P</sup>+ 3.415E-08e<sup>-6.713E-01P</sup>, <br> 2.000E-06) |   2.0000E-06<sup>4</sup>| 
| HET_IEPOX   | IEPOX ----> IEPOXP  | HETERO_IEPOX | Not Available<sup>2</sup> | 
| HET_ISO3TET   | IEPOXP ----> AISO3NOSJ  | HETERO_ISO3NOSJ | Not Available<sup>2</sup> | 
| HET_IEPOXOS   | IEPOXP + ASO4J ----> AISO3OSJ  | HETERO_ISO3OSJ | Not Available<sup>2</sup> | 
| HET_IPX   | IPX ----> AISO4J  |   2.0000E+00\*HETERO_IEPOX | Not Available<sup>2</sup> | 
| HET_INALD   | INALD ----> AISO5J + HNO3  |   5.0000E-01\*HETERO_IEPOX | Not Available<sup>2</sup> | 
| ROCALK1c   | VROCP6ALK + HO ----> VROCP6ALKP  |   1.5300E-11 |   1.5300E-11 |
| ROCALK2c   | VROCP5ALK + HO ----> VROCP5ALKP  |   1.6800E-11 |   1.6800E-11 |
| ROCALK3c   | VROCP4ALK + HO ----> VROCP4ALKP  |   2.2400E-11 |   2.2400E-11 |
| ROCALK4c   | VROCP3ALK + HO ----> VROCP3ALKP  |   2.6700E-11 |   2.6700E-11 |
| ROCALK5c   | VROCP2ALK + HO ----> VROCP2ALKP  |   3.0900E-11 |   3.0900E-11 |
| ROCALK6c   | VROCP1ALK + HO ----> VROCP1ALKP  |   3.3800E-11 |   3.3800E-11 |
| HC1001   | HC10 + HO ----> HC10P  |   1.1000E-11 |   1.1000E-11 |
| ROCALK7c   | VROCP6ALKP + NO ---->   0.7200\*VROCP6ALKP2 +    0.2800\*VROCP4OXY2 +    0.7200\*NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK8c   | VROCP5ALKP + NO ---->   0.7200\*VROCP5ALKP2 +    0.2800\*VROCP3OXY2 +    0.7200\*NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK9c   | VROCP4ALKP + NO ---->   0.7200\*VROCP4ALKP2 +    0.2800\*VROCP2OXY2 +    0.7200\*NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK10c   | VROCP3ALKP + NO ---->   0.7200\*VROCP3ALKP2 +    0.2800\*VROCP1OXY1 +    0.7200\*NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK11c   | VROCP2ALKP + NO ---->   0.7200\*VROCP2ALKP2 +    0.2800\*VROCP0OXY2 +    0.7200\*NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK12c   | VROCP1ALKP + NO ---->   0.7200\*VROCP1ALKP2 +    0.2800\*VROCN1OXY1 +    0.7200\*NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| HC1002   | HC10P + NO ---->   0.7400\*HC10P2 +    0.2600\*ONIT +    0.7400\*NO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
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
| ROCALK31c   | VROCP6ALKP2 + NO ---->   0.1400\*VROCP2OXY2 +    0.8600\*NO2 +    0.8600\*VROCP3OXY2 +    0.8600\*HO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK32c   | VROCP5ALKP2 + NO ---->   0.1400\*VROCP1OXY3 +    0.8600\*NO2 +    0.8600\*VROCP2OXY2 +    0.8600\*HO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK33c   | VROCP4ALKP2 + NO ---->   0.1400\*VROCP0OXY2 +    0.8600\*NO2 +    0.8600\*VROCP1OXY1 +    0.8600\*HO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK34c   | VROCP3ALKP2 + NO ---->   0.1400\*VROCN1OXY1 +    0.8600\*NO2 +    0.8600\*VROCP0OXY2 +    0.8600\*HO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK35c   | VROCP2ALKP2 + NO ---->   0.1400\*VROCN2OXY2 +    0.8600\*NO2 +    0.8600\*VROCN1OXY1 +    0.8600\*HO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCALK36c   | VROCP1ALKP2 + NO ----> VROCN2OXY2 +    0.8600\*NO2 +    0.8600\*HO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| HC1006   | HC10P2 + NO ---->   0.1200\*ONIT +    0.8800\*NO2 +    0.8800\*KET +    0.8800\*HO2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
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
| ROCARO01   | VROCP6ARO + HO ---->   0.8400\*VROCP6AROP +    0.1600\*HO2 +    0.1600\*VROCP4OXY2  |   1.8100E-11 |   1.8100E-11 |
| ROCARO02   | VROCP6AROP + HO2 ---->   0.0595\*VROCP4OXY2 +    0.9048\*VROCP1OXY3 +    0.0357\*VROCN2OXY4  |   2.91E-13e<sup>  1300.00/T</sup> |   2.2778E-11 |
| ROCARO03   | VROCP6AROP + NO ---->   0.0001\*VROCP4OXY2 +    0.0018\*VROCP2OXY2 +    0.0001\*VROCN1OXY3 +    0.9980\*NO2 +    0.9980\*HO2 +    0.0594\*BALD +    0.4693\*GLY +    0.4693\*MGLY +    0.4693\*FURANONE +    0.4693\*DCB2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCARO04   | VROCP6AROP + NO3 ----> NO2 +    0.9405\*HO2 +    0.0595\*BALD +    0.4702\*GLY +    0.4702\*MGLY +    0.4702\*FURANONE +    0.4702\*DCB2  |   2.3000E-12 |   2.3000E-12 |
| ROCARO05   | VROCP6AROP + MO2 ---->   0.6800\*HCHO +    1.3105\*HO2 +    0.3200\*MOH +    0.0595\*BALD +    0.4702\*GLY +    0.4702\*MGLY +    0.4702\*FURANONE +    0.4702\*DCB2  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| ROCARO06   | VROCP6AROP + ACO3 ---->   0.7000\*MO2 +    0.9405\*HO2 +    0.3000\*ORA2 +    0.0595\*BALD +    0.4702\*GLY +    0.4702\*MGLY +    0.4702\*FURANONE +    0.4702\*DCB2  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| ROCARO11   | VROCP5ARO + HO ---->   0.8400\*VROCP5AROP +    0.1600\*HO2 +    0.1600\*VROCP3OXY2  |   1.8100E-11 |   1.8100E-11 |
| ROCARO12   | VROCP5AROP + HO2 ---->   0.0595\*VROCP3OXY2 +    0.9048\*VROCP0OXY2 +    0.0357\*VROCN2OXY4  |   2.91E-13e<sup>  1300.00/T</sup> |   2.2778E-11 |
| ROCARO13   | VROCP5AROP + NO ---->   0.0001\*VROCP3OXY2 +    0.0018\*VROCP1OXY3 +    0.0001\*VROCN2OXY4 +    0.9980\*NO2 +    0.9980\*HO2 +    0.0594\*VROCP4OXY2 +    0.4693\*GLY +    0.4693\*MGLY +    0.4693\*FURANONE +    0.4693\*DCB2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCARO14   | VROCP5AROP + NO3 ----> NO2 +    0.9405\*HO2 +    0.0595\*VROCP4OXY2 +    0.4702\*GLY +    0.4702\*MGLY +    0.4702\*FURANONE +    0.4702\*DCB2  |   2.3000E-12 |   2.3000E-12 |
| ROCARO15   | VROCP5AROP + MO2 ---->   0.6800\*HCHO +    1.3105\*HO2 +    0.3200\*MOH +    0.0595\*VROCP4OXY2 +    0.4702\*GLY +    0.4702\*MGLY +    0.4702\*FURANONE +    0.4702\*DCB2  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| ROCARO16   | VROCP5AROP + ACO3 ---->   0.7000\*MO2 +    0.9405\*HO2 +    0.3000\*ORA2 +    0.0595\*VROCP4OXY2 +    0.4702\*GLY +    0.4702\*MGLY +    0.4702\*FURANONE +    0.4702\*DCB2  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| ROCARO21   | NAPH + HO ---->   0.8400\*NAPHP +    0.1600\*HO2 +    0.1600\*VROCP3OXY2  |   2.3100E-11 |   2.3100E-11 |
| ROCARO22   | NAPHP + HO2 ---->   0.0595\*VROCP3OXY2 +    0.9048\*VROCP1OXY3 +    0.0357\*VROCN2OXY8  |   2.91E-13e<sup>  1300.00/T</sup> |   2.2778E-11 |
| ROCARO23   | NAPHP + NO ---->   0.0595\*VROCP4OXY2 +    0.0018\*VROCP2OXY2 +    0.0001\*VROCN2OXY8 +    0.9980\*NO2 +    0.9980\*HO2 +    0.4693\*GLY +    0.4693\*MGLY +    0.4693\*FURANONE +    0.4693\*DCB2  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCARO24   | NAPHP + NO3 ----> NO2 +    0.9405\*HO2 +    0.0595\*VROCP4OXY2 +    0.4702\*GLY +    0.4702\*MGLY +    0.4702\*FURANONE +    0.4702\*DCB2  |   2.3000E-12 |   2.3000E-12 |
| ROCARO25   | NAPHP + MO2 ---->   0.6800\*HCHO +    1.3105\*HO2 +    0.3200\*MOH +    0.0595\*VROCP4OXY2 +    0.4702\*GLY +    0.4702\*MGLY +    0.4702\*FURANONE +    0.4702\*DCB2  |   3.56E-14e<sup>   708.00/T</sup> |   3.8260E-13 |
| ROCARO26   | NAPHP + ACO3 ---->   0.7000\*MO2 +    0.9405\*HO2 +    0.3000\*ORA2 +    0.0595\*VROCP4OXY2 +    0.4702\*GLY +    0.4702\*MGLY +    0.4702\*FURANONE +    0.4702\*DCB2  |   7.40E-13e<sup>   765.00/T</sup> |   9.6284E-12 |
| ROCOXY1c   | VROCN2OXY8 + HO ----> HO +    0.0854\*VROCN2OXY8 +    0.2581\*DCB1 +    0.2581\*MEK +    0.2581\*ACD +    0.2581\*ALD +    0.2581\*MO2 +    0.2581\*ETHP +    0.2581\*HC3P +    0.2581\*MEKP  |   5.9000E-11 |   5.9000E-11 |
| ROCOXY2c   | VROCN2OXY4 + HO ----> HO +    0.4640\*VROCN2OXY8 +    0.1977\*VROCN2OXY4 +    0.0121\*VROCN1OXY6 +    0.0152\*VROCN1OXY3 +    0.0624\*VROCP0OXY4 +    0.0388\*VROCP1OXY3 +    0.0491\*VROCP2OXY2 +    0.0398\*VROCP3OXY2 +    0.0183\*VROCP4OXY2 +    0.0308\*OP3 +    0.0040\*OP2 +    0.0794\*DCB1 +    0.0794\*MEK +    0.0794\*KET +    0.0794\*ACD +    0.0794\*ALD +    0.0794\*MO2 +    0.0794\*ETHP +    0.0794\*HC3P +    0.0794\*MEKP +    0.0794\*HC5P +    0.0794\*KETP  |   6.0700E-11 |   6.0700E-11 |
| ROCOXY3c   | VROCN2OXY2 + HO ----> HO +    0.1041\*VROCN2OXY8 +    0.5638\*VROCN2OXY4 +    0.2141\*VROCN2OXY2 +    0.0153\*VROCN1OXY6 +    0.0298\*VROCN1OXY3 +    0.0096\*VROCN1OXY1 +    0.0189\*VROCP0OXY4 +    0.0456\*VROCP0OXY2 +    0.0314\*VROCP1OXY3 +    0.0199\*VROCP1OXY1 +    0.0459\*VROCP2OXY2 +    0.0452\*VROCP3OXY2 +    0.0455\*VROCP4OXY2 +    0.0325\*VROCP5OXY1 +    0.0369\*VROCP6OXY1 +    0.0026\*OP3 +    0.0390\*DCB1 +    0.0390\*HKET +    0.0390\*MEK +    0.0390\*ACD +    0.0390\*ALD +    0.0390\*MO2 +    0.0390\*ETHP +    0.0390\*HC3P +    0.0390\*MEKP +    0.0925\*HC5P  |   5.5400E-11 |   5.5400E-11 |
| ROCOXY4c   | VROCN1OXY6 + HO ----> HO +    0.2036\*VROCN2OXY8 +    0.0071\*VROCN2OXY4 +    0.1840\*DCB1 +    0.1840\*MEK +    0.1840\*KET +    0.1840\*ACD +    0.1840\*ALD +    0.1840\*MO2 +    0.1840\*ETHP +    0.1840\*HC3P +    0.1840\*MEKP +    0.1840\*HC5P  |   5.6300E-11 |   5.6300E-11 |
| ROCOXY5c   | VROCN1OXY3 + HO ----> HO +    0.2792\*VROCN2OXY8 +    0.4025\*VROCN2OXY4 +    0.0088\*VROCN2OXY2 +    0.0319\*VROCN1OXY6 +    0.0076\*VROCN1OXY3 +    0.0194\*VROCP0OXY4 +    0.0104\*VROCP0OXY2 +    0.0510\*VROCP1OXY3 +    0.0075\*VROCP1OXY1 +    0.0512\*VROCP2OXY2 +    0.0462\*VROCP3OXY2 +    0.0512\*VROCP4OXY2 +    0.0138\*VROCP5OXY1 +    0.0135\*OP2 +    0.0646\*DCB1 +    0.0646\*HKET +    0.0646\*MEK +    0.0646\*ACD +    0.0646\*ALD +    0.0646\*MO2 +    0.0646\*ETHP +    0.0646\*HC3P +    0.0646\*MEKP +    0.1753\*HC5P  |   5.4600E-11 |   5.4600E-11 |
| ROCOXY6c   | VROCN1OXY1 + HO ----> HO +    0.0074\*VROCN2OXY8 +    0.1190\*VROCN2OXY4 +    0.7261\*VROCN2OXY2 +    0.0122\*VROCN1OXY6 +    0.0305\*VROCN1OXY3 +    0.0070\*VROCN1OXY1 +    0.0291\*VROCP0OXY4 +    0.0454\*VROCP0OXY2 +    0.0234\*VROCP1OXY3 +    0.0352\*VROCP1OXY1 +    0.0624\*VROCP2OXY2 +    0.0518\*VROCP3OXY2 +    0.0509\*VROCP4OXY2 +    0.0347\*VROCP5OXY1 +    0.0748\*VROCP6OXY1 +    0.0163\*OP3 +    0.0062\*OP2 +    0.0244\*DCB1 +    0.0244\*HKET +    0.0244\*MEK +    0.0244\*ACD +    0.0244\*ALD +    0.0244\*MO2 +    0.0244\*ETHP +    0.0244\*HC3P +    0.0244\*MEKP +    0.0537\*HC5P  |   4.5000E-11 |   4.5000E-11 |
| ROCOXY7c   | VROCP0OXY4 + HO ----> HO +    0.2822\*VROCN2OXY8 +    0.1165\*VROCN2OXY4 +    0.0320\*VROCN1OXY6 +    0.0183\*VROCN1OXY3 +    0.0011\*VROCP0OXY4 +    0.0660\*VROCP2OXY2 +    0.0535\*VROCP3OXY2 +    0.0246\*VROCP4OXY2 +    0.0054\*OP2 +    0.1068\*DCB1 +    0.1068\*MEK +    0.1068\*KET +    0.1068\*ACD +    0.1068\*ALD +    0.1068\*MO2 +    0.1068\*ETHP +    0.1068\*HC3P +    0.1068\*MEKP +    0.1068\*HC5P +    0.1068\*KETP  |   5.1700E-11 |   5.1700E-11 |
| ROCOXY8c   | VROCP0OXY2 + HO ----> HO +    0.0659\*VROCN2OXY8 +    0.4579\*VROCN2OXY4 +    0.1156\*VROCN2OXY2 +    0.0325\*VROCN1OXY6 +    0.0657\*VROCN1OXY3 +    0.0046\*VROCN1OXY1 +    0.0307\*VROCP0OXY4 +    0.0024\*VROCP0OXY2 +    0.0395\*VROCP1OXY3 +    0.0215\*VROCP1OXY1 +    0.0539\*VROCP2OXY2 +    0.0516\*VROCP3OXY2 +    0.0519\*VROCP4OXY2 +    0.0371\*VROCP5OXY1 +    0.0421\*VROCP6OXY1 +    0.0105\*OP3 +    0.0445\*DCB1 +    0.0445\*HKET +    0.0445\*MEK +    0.0445\*ACD +    0.0445\*ALD +    0.0445\*MO2 +    0.0445\*ETHP +    0.0445\*HC3P +    0.0445\*MEKP +    0.1055\*HC5P  |   4.7300E-11 |   4.7300E-11 |
| ROCOXY9c   | VROCP1OXY3 + HO ----> HO +    0.1778\*VROCN2OXY8 +    0.1924\*VROCN2OXY4 +    0.0004\*VROCN2OXY2 +    0.0740\*VROCN1OXY6 +    0.0452\*VROCN1OXY3 +    0.0631\*VROCP0OXY4 +    0.0007\*VROCP0OXY2 +    0.0006\*VROCP1OXY3 +    0.0227\*VROCP2OXY2 +    0.0585\*VROCP3OXY2 +    0.0649\*VROCP4OXY2 +    0.0174\*VROCP5OXY1 +    0.0154\*OP3 +    0.0170\*OP2 +    0.0818\*DCB1 +    0.0818\*HKET +    0.0818\*MEK +    0.0818\*ACD +    0.0818\*ALD +    0.0818\*MO2 +    0.0818\*ETHP +    0.0818\*HC3P +    0.0818\*MEKP +    0.2220\*HC5P  |   4.6000E-11 |   4.6000E-11 |
| ROCOXY10c   | VROCP1OXY1 + HO ----> HO +    0.0023\*VROCN2OXY8 +    0.1340\*VROCN2OXY4 +    0.3349\*VROCN2OXY2 +    0.0080\*VROCN1OXY6 +    0.1193\*VROCN1OXY3 +    0.0758\*VROCN1OXY1 +    0.0292\*VROCP0OXY4 +    0.0766\*VROCP0OXY2 +    0.0277\*VROCP1OXY3 +    0.0118\*VROCP1OXY1 +    0.0651\*VROCP2OXY2 +    0.0709\*VROCP3OXY2 +    0.0668\*VROCP4OXY2 +    0.0423\*VROCP5OXY1 +    0.0911\*VROCP6OXY1 +    0.0066\*OP3 +    0.0025\*OP2 +    0.0297\*DCB1 +    0.0297\*HKET +    0.0297\*MEK +    0.0297\*ACD +    0.0297\*ALD +    0.0297\*MO2 +    0.0297\*ETHP +    0.0297\*HC3P +    0.0297\*MEKP +    0.0654\*HC5P  |   3.8000E-11 |   3.8000E-11 |
| ROCOXY11c   | VROCP2OXY2 + HO ----> HO +    0.0445\*VROCN2OXY8 +    0.1726\*VROCN2OXY4 +    0.0104\*VROCN2OXY2 +    0.0513\*VROCN1OXY6 +    0.1118\*VROCN1OXY3 +    0.0013\*VROCN1OXY1 +    0.1337\*VROCP0OXY4 +    0.0403\*VROCP0OXY2 +    0.0511\*VROCP1OXY3 +    0.0068\*VROCP1OXY1 +    0.0236\*VROCP2OXY2 +    0.0293\*VROCP3OXY2 +    0.0733\*VROCP4OXY2 +    0.0523\*VROCP5OXY1 +    0.0595\*VROCP6OXY1 +    0.0041\*OP3 +    0.0023\*OP2 +    0.0628\*DCB1 +    0.0628\*HKET +    0.0628\*MEK +    0.0628\*ACD +    0.0628\*ALD +    0.0628\*MO2 +    0.0628\*ETHP +    0.0628\*HC3P +    0.0628\*MEKP +    0.1489\*HC5P  |   3.9300E-11 |   3.9300E-11 |
| ROCOXY12c   | VROCP3OXY2 + HO ----> HO +    0.0317\*VROCN2OXY8 +    0.0765\*VROCN2OXY4 +    0.0009\*VROCN2OXY2 +    0.0526\*VROCN1OXY6 +    0.0489\*VROCN1OXY3 +    0.1550\*VROCP0OXY4 +    0.0155\*VROCP0OXY2 +    0.1051\*VROCP1OXY3 +    0.0013\*VROCP1OXY1 +    0.0535\*VROCP2OXY2 +    0.0086\*VROCP3OXY2 +    0.0426\*VROCP4OXY2 +    0.0582\*VROCP5OXY1 +    0.0661\*VROCP6OXY1 +    0.0506\*OP3 +    0.0114\*OP2 +    0.0698\*DCB1 +    0.0698\*HKET +    0.0698\*MEK +    0.0698\*ACD +    0.0698\*ALD +    0.0698\*MO2 +    0.0698\*ETHP +    0.0698\*HC3P +    0.0698\*MEKP +    0.1656\*HC5P  |   3.5200E-11 |   3.5200E-11 |
| ROCOXY13c   | VROCP4OXY2 + HO ----> HO +    0.0117\*VROCN2OXY8 +    0.0167\*VROCN2OXY4 +    0.0480\*VROCN1OXY6 +    0.0246\*VROCN1OXY3 +    0.0881\*VROCP0OXY4 +    0.0916\*VROCP1OXY3 +    0.0073\*VROCP1OXY1 +    0.0972\*VROCP2OXY2 +    0.0456\*VROCP3OXY2 +    0.0024\*VROCP4OXY2 +    0.0479\*VROCP5OXY1 +    0.0745\*VROCP6OXY1 +    0.0607\*OP3 +    0.0155\*OP2 +    0.0786\*DCB1 +    0.0786\*HKET +    0.0786\*MEK +    0.0786\*ACD +    0.0786\*ALD +    0.0786\*MO2 +    0.0786\*ETHP +    0.0786\*HC3P +    0.0786\*MEKP +    0.1730\*HC5P  |   3.1200E-11 |   3.1200E-11 |
| ROCOXY14c   | VROCP5OXY1 + HO ----> HO +    0.0103\*VROCN2OXY4 +    0.0006\*VROCN2OXY2 +    0.0090\*VROCN1OXY6 +    0.0146\*VROCN1OXY3 +    0.0702\*VROCP0OXY4 +    0.0153\*VROCP0OXY2 +    0.1038\*VROCP1OXY3 +    0.0031\*VROCP1OXY1 +    0.1650\*VROCP2OXY2 +    0.1566\*VROCP3OXY2 +    0.0724\*VROCP4OXY2 +    0.0062\*VROCP5OXY1 +    0.1398\*VROCP6OXY1 +    0.0216\*OP3 +    0.0384\*OP2 +    0.0526\*DCB1 +    0.0526\*HKET +    0.0526\*MEK +    0.0526\*ACD +    0.0526\*ALD +    0.0526\*MO2 +    0.0526\*ETHP +    0.0526\*HC3P +    0.0526\*MEKP +    0.1280\*HC5P  |   2.4000E-11 |   2.4000E-11 |
| ROCOXY15c   | VROCP6OXY1 + HO ----> HO +    0.0061\*VROCN1OXY6 +    0.0049\*VROCN1OXY3 +    0.0224\*VROCP0OXY4 +    0.0503\*VROCP1OXY3 +    0.0022\*VROCP1OXY1 +    0.0879\*VROCP2OXY2 +    0.1384\*VROCP3OXY2 +    0.1463\*VROCP4OXY2 +    0.0432\*VROCP5OXY1 +    0.0957\*VROCP6OXY1 +    0.0316\*OP3 +    0.0585\*OP2 +    0.0571\*DCB1 +    0.0571\*HKET +    0.0571\*MEK +    0.0571\*ACD +    0.0571\*ALD +    0.0571\*MO2 +    0.0571\*ETHP +    0.0571\*HC3P +    0.0571\*MEKP +    0.1544\*HC5P  |   2.0500E-11 |   2.0500E-11 |
| ROCOXY16c   | OP3 + HO ----> HO +    0.1188\*VROCN2OXY8 +    0.0008\*VROCN2OXY4 +    0.0390\*VROCN1OXY6 +    0.0114\*VROCP0OXY4 +    0.2266\*DCB1 +    0.2266\*MEK +    0.2266\*ACD +    0.2266\*ALD +    0.2266\*MO2 +    0.2266\*ETHP +    0.2266\*HC3P +    0.2266\*MEKP  |   4.6900E-11 |   4.6900E-11 |
| R364   | ECH4 + HO ----> MO2  |   2.45E-12e<sup> -1775.00/T</sup> |   6.3628E-15 |
| TRP58   | ATRPNJ ----> AHOMJ + HNO3  |   9.2600E-05 |   9.2600E-05 |
| TRP59   | AHONITJ ----> AHOMJ + HNO3  |   9.2600E-05 |   9.2600E-05 |
| ROCARO71   | STY + HO ----> STYP  |   5.8000E-11 |   5.8000E-11 |
| ROCARO72   | STYP + HO2 ----> VROCP3OXY2  |   2.91E-12e<sup>  1300.00/T</sup> |   2.2778E-10 |
| ROCARO73   | STYP + NO ----> NO2 + HO2 + HCHO + BALD  |   2.70E-12e<sup>   360.00/T</sup> |   9.0313E-12 |
| ROCARO74   | STYP + NO3 ----> NO2 + HO2 + HCHO + BALD  |   2.3000E-12 |   2.3000E-12 |
| ROCARO75   | STYP + MO2 ---->   1.3700\*HO2 +    1.6800\*HCHO + BALD +    0.3200\*MOH  |   2.5000E-13 |   2.5000E-13 |
| ROCARO76   | STYP + ACO3 ----> HO2 + HCHO + BALD +    0.7000\*MO2 +    0.3000\*ORA2  |   2.5000E-13 |   2.5000E-13 |

<sup>0</sup>Units molecules/(sec*cm<sup>3</sup>); Value at 298.15 K;   2.4615E+19 molecules/cm<sup>3</sup>;   1.00 Atm.     
<sup>1</sup>Photolysis Reaction;depends on radiation and predicted concentrations     
<sup>2</sup>Heterogeneous Reaction; depends on predicted concentrations                
<sup>4</sup>Set to zero if sun is below the horizon. SEAWATER equals fraction of ocean plus surf zones not covered by seaice. P equals air pressure in atmospheres.         
<sup>8</sup>Rate constant scaled as reverse equilibrium to constant for listed reaction    
