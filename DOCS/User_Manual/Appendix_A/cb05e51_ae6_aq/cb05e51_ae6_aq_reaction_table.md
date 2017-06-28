Information is taken directly from the mech.def file. cb05e51 and cb05mp51 are identical gas-phase chemical mechanisms.

*Fall-off/pressure dependent reaction rate constants ([M] equals air number density):
**For rate constants with k<sub>o</sub>, k<sub>inf</sub>, n, F values: k = [ k<sub>0</sub>[M]/(1+k<sub>0</sub>[M]/k<sub>inf</sub>)]F<sup>G</sup>, where G=1/[1+(log(k<sub>0</sub>[M]/k<sub>inf</sub>)/n)<sup>-2</sup>)]
**For rate constants with k<sub>1</sub>, k<sub>2</sub>: k = k<sub>1</sub> + k<sub>2</sub> [M]
**For rate constants with k<sub>0</sub>, k<sub>2</sub>, k<sub>3</sub>: k = k<sub>0</sub> + k<sub>3</sub>[M]/(1+k<sub>3</sub>[M]/k<sub>2</sub>)
**For rate constants with k<sub>1</sub>, k<sub>2</sub>, k<sub>3</sub>: k = k<sub>1</sub> + k<sub>2</sub>[M] + k<sub>3</sub>

*For rate constants with the form A/<''PHOT''>, k equals A times the photolysis rates, ''PHOT''.

*For rate constants with the form A~<''HETERO''>, k equals A times the heterogeneous rate constant, ''HETERO''.

*For rate constants with the form A*K<''RCONST''>, k equals A times the previously defined rate constant, ''RCONST''.

*Units of rate constants give reactions rates in units of molecules cm<sup>-3</sup> s<sup>-1</sup>. Note that T equals air temperature in degrees K in the below table.

Check the [[cb05e51_species_table|species table]] for the reactants and products used the below reactions.

**Table A‑2. Mechanism for cb05e51_ae6_aq uses the following reactions.**

| **Label** | **Reaction** | **Rate Const** | **Notes** | **Reference** |
|---|--------------------------------|--------------------------|------------------|----------------------|
|``<R1>``|NO2 = NO + O|1.0/``<NO2_IUPAC10>``| | |
|``<R2>``|O + O2 + M = O3 + M|6.0E-34*(T/300)<sup>(-2.4)</sup>| | |
|``<R3>``|O3 + NO = NO2|3.0E-12*exp<sup>(-1500.0/T)</sup>| | |
|``<R4>``|O + NO2 = NO|5.6E-12*exp<sup>(180.0/T)</sup>| | |
|``<R5>``|O + NO2 = NO3|k<sub>0</sub>=2.5E-31*(T/300)<sup>(-1.8)</sup>, k<sub>inf</sub>=2.2E-11*(T/300)<sup>(-0.7)</sup>, F=0.6, n=1| | |
|``<R6>``|O + NO = NO2|k<sub>0</sub>=9.0E-32*(T/300)<sup>(-1.5)</sup>, k<sub>inf</sub>=3.0E-11, F=0.6, n=1 | | |
|<R7>|NO2 + O3 = NO3|1.2E-13*exp<sup>(-2450/T)</sup>| | |
|<R8>|O3 = O|1.0/<O3_O3P_IUPAC10>| | |
|<R9>|O3 = O1D|1.0/<O3_O1D_IUPAC10>| | |
|<R10>|O1D + M = O + M|2.1E-11*exp<sup>(102./T)</sup>| | |
|<R11>|O1D + H2O = 2.000*OH|2.2E-10| | |
|<R12>|O3 + OH = HO2|1.7E-12*exp<sup>(-940/T)</sup>| | |
|<R13>|O3 + HO2 = OH|1.0E-14*exp<sup>(-490/T)</sup>| | |
|<R14>|NO3 = NO2 + O|1.0/<NO3NO2_06>| | |
|<R15>|NO3 = NO|1.0/<NO3NO_06>| | |
|<R16>|NO3 + NO = 2.000*NO2|1.5E-11*exp<sup>(170/T)</sup>| | |
|<R17>|NO3 + NO2 = NO + NO2|4.5E-14*exp<sup>(-1260/T)</sup>| | |
|<R18>|NO3 + NO2 = N2O5|k<sub>0</sub>=2.0E-30*(T/300)<sup>(-4.4)</sup>, k<sub>inf</sub>=1.4E-12*(T/300)<sup>(-0.7)</sup>, F=0.6, n=1| | |
|<R19>|N2O5 + H2O = 2.000*HNO3|1.0E-22| | |
|<R20>|N2O5 + H2O + H2O = 2.000*HNO3|0.0| | |
|<R21>|N2O5 = NO3 + NO2|k<sub>0</sub>=1.0E-03*(T/300)<sup>(-3.5)</sup>*exp<sup>(-11000/T)</sup>, k<sub>inf</sub>=9.7E + 14*(T/300)<sup>(0.1)</sup>*exp<sup>(-11080/T)</sup>, F=0.45, n=1.0| | |
|<R22>|NO + NO + O2 = 2.000*NO2|3.3E-39*exp<sup>(530/T)</sup>| | |
|<R23>|NO + NO2 + H2O = 2.000*HONO|5.0E-40| | |
|<R24>|NO + OH = HONO|k<sub>0</sub>=7.0E-31*(T/300)<sup>(-2.6)</sup>, k<sub>inf</sub>=3.6E-11*(T/300)<sup>(-0.1)</sup>, F=0.6, n=1| | |
|<R25>|HONO = NO + OH|1.0/<HONO_IUPAC10>| | |
|<R26>|OH + HONO = NO2|1.8E-11*exp<sup>(-390/T)</sup>| | |
|<R27>|HONO + HONO = NO + NO2|1.0E-20| | |
|<R28>|NO2 + OH = HNO3|k<sub>0</sub>=3.2E-30*(T/300)<sup>(-4.5)</sup>, k<sub>inf</sub>=3.0E-11, F=0.41, n=1.24| | |
|<R29>|OH + HNO3 = NO3| k<sub>0</sub>=2.4E-14*exp<sup>(460/T)</sup>, k<sub>2</sub>=2.7E-17*exp<sup>(2199/T)</sup>, k<sub>3</sub>=6.5E-34*exp<sup>(1335/T)</sup>| | |
|<R30>|HO2 + NO = OH + NO2|3.5E-12*exp<sup>(250/T)</sup>| | |
|<R30a>|NO + HO2 = HNO3| k<sub>1</sub>=6.095e-14*(T/300)<sup>(-1.0)</sup>*exp<sup>(270.0/T)</sup>, k<sub>2</sub>=6.857e-34*(T/300)<sup>(1.0)</sup>*exp<sup>(270.0/T)</sup>, k<sub>3</sub>=-5.968e-14*exp<sup>(270.0/T)</sup>| | |
|<R31>|HO2 + NO2 = PNA|k<sub>0</sub>=1.8E-31*(T/300)<sup>(-3.2)</sup>, k<sub>inf</sub>=4.7E-12, F=0.6, n=1| | |
|<R32>|PNA = HO2 + NO2|k<sub>0</sub>=4.1E-5*exp<sup>(-10650/T)</sup>, k<sub>inf</sub>=4.8E15*exp<sup>(-11170/T)</sup>, F=0.6, n=1| | |
|<R33>|OH + PNA = NO2|1.3E-12*exp<sup>(380/T)</sup>| | |
|<R34>|HO2 + HO2 = H2O2| k<sub>1</sub>=2.3E-13*exp<sup>(600/T)</sup>, k<sub>2</sub>=1.7E-33*exp<sup>(1000/T)</sup>| | |
|<R35>|HO2 + HO2 + H2O = H2O2| k<sub>1</sub>=3.22E-34*exp<sup>(2800/T)</sup>, k<sub>2</sub>=2.38E-54*exp<sup>(3200/T)</sup>| | |
|<R36>|H2O2 = 2.000*OH|1.0/<H2O2_IUPAC10>| | |
|<R37>|OH + H2O2 = HO2|2.9E-12*exp<sup>(-160/T)</sup>| | |
|<R38>|O1D + H2 = OH + HO2|1.1E-10| | |
|<R39>|OH + H2 = HO2|5.5E-12*exp<sup>(-2000./T)</sup>| | |
|<R40>|OH + O = HO2|2.2E-11*exp<sup>(120./T)</sup>| | |
|<R41>|OH + OH = O|4.2E-12*exp<sup>(-240/T)</sup>| | |
|<R42>|OH + OH = H2O2|k<sub>0</sub>=6.9E-31*(T/300)<sup>(-1.0)</sup>, k<sub>inf</sub>=2.6E-11, F=0.6, n=1| | |
|<R43>|OH + HO2 = |4.8E-11*exp<sup>(250./T)</sup>| | |
|<R44>|HO2 + O = OH|3.0E-11*exp<sup>(200./T)</sup>| | |
|<R45>|H2O2 + O = OH + HO2|1.4E-12*exp<sup>(-2000./T)</sup>| | |
|<R46>|NO3 + O = NO2|1.0E-11| | |
|<R47>|NO3 + OH = HO2 + NO2|2.2E-11| | |
|<R48>|NO3 + HO2 = HNO3|3.5E-12| | |
|<R49>|NO3 + O3 = NO2|1.0E-17| | |
|<R50>|NO3 + NO3 = 2.000*NO2|8.5E-13*exp<sup>(-2450./T)</sup>| | |
|<R51>|PNA = 0.610*HO2 + 0.610*NO2 + 0.390*OH + 0.390*NO3|1.0/<PNA_IUPAC10>| | |
|<R52>|HNO3 = OH + NO2|1.0/<HNO3_IUPAC10>| | |
|<R53>|N2O5 = NO2 + NO3|1.0/<N2O5_IUPAC10>| | |
|<R54>|XO2 + NO = NO2|2.6E-12*exp<sup>(365/T)</sup>| | |
|<R55>|XO2N + NO = 0.5*NTROH + 0.5*NTRALK|2.6E-12*exp<sup>(365/T)</sup>|50% alkylnitrate; 50% hydroxynitrate | |
|<R56>|XO2 + HO2 = ROOH|7.5E-13*exp<sup>(700/T)</sup>| | |
|<R57>|XO2N + HO2 = ROOH|7.5E-13*exp<sup>(700/T)</sup>| | |
|<R58>|XO2 + XO2 = |6.8E-14| | |
|<R59>|XO2N + XO2N = |6.8E-14| | |
|<R60>|XO2 + XO2N = |6.8E-14| | |
|<R63>|ROOH + OH = XO2 + 0.500*ALD2 + 0.500*ALDX|3.01E-12*exp<sup>(190/T)</sup>| | |
|<R64>|ROOH = OH + HO2 + 0.500*ALD2 + 0.500*ALDX|1.0/<MEPX_IUPAC10>| | |
|<R64a>|ISOPX + OH = 0.904*IEPOX + 0.933*OH + 0.067*ISOPO2 + 0.029*IOLE + 0.029*ALDX|2.23E-11*exp<sup>(372/T)</sup>| | |
|<R64b>|IEPOX + OH = IEPXO2|5.78E-11*exp<sup>(-400/T)</sup>| | |
|<R64c>|IEPXO2 + HO2 = 0.275*ALD2 + 0.275*MGLY + 1.125*OH + 0.825*HO2 + 0.650*FORM + 0.074*FACD + 0.251*CO + 2.45*PAR|7.43E-13*exp<sup>(700/T)</sup>| | |
|<R64d>|IEPXO2 + NO = 0.275*ALD2 + 0.275*MGLY + 0.125*OH + 0.825*HO2 + 0.65*FORM + NO2 + 0.251*CO + 2.45*PAR|2.39E-12*exp<sup>(365/T)</sup>| | |
|<R64e>|IEPXO2 + C2O3 = 0.22*ALD2 + 0.22*MGLY + 0.1*OH + 0.66*HO2 + 0.52*FORM + 0.2*CO + 1.96*PAR + 0.8*MEO2 + 0.2*AACD|8.90E-13*exp<sup>(800/T)</sup>| | |
|<R65>|OH + CO = HO2| k<sub>1</sub>=1.44E-13, k<sub>2</sub>=3.43E-33*exp<sup>(-0.0/T)</sup>| | |
|<R66>|OH + CH4 = MEO2|2.45E-12*exp<sup>(-1775/T)</sup>| | |
|<R67>|MEO2 + NO = FORM + HO2 + NO2|2.8E-12*exp<sup>(300/T)</sup>| | |
|<R68>|MEO2 + HO2 = MEPX|4.1E-13*exp<sup>(750/T)</sup>| | |
|<R69>|MEO2 + MEO2 = 1.370*FORM + 0.740*HO2 + 0.630*MEOH|9.5E-14*exp<sup>(390/T)</sup>| | |
|<R70>|MEPX + OH = 0.700*MEO2 + 0.300*XO2 + 0.300*HO2|3.8E-12*exp<sup>(200/T)</sup>| | |
|<R71>|MEPX = FORM + HO2 + OH|1.0/<MEPX_IUPAC10>| | |
|<R72>|MEOH + OH = FORM + HO2|7.3E-12*exp<sup>(-620/T)</sup>| | |
|<R73>|FORM + OH = HO2 + CO|9.0E-12| | |
|<R74>|FORM = 2.000*HO2 + CO|1.0/<FORM_R_IUPAC10>| | |
|<R75>|FORM = CO|1.0/<FORM_M_IUPAC10>| | |
|<R76>|FORM + O = OH + HO2 + CO|3.4E-11*exp<sup>(-1600/T)</sup>| | |
|<R77>|FORM + NO3 = HNO3 + HO2 + CO|5.8E-16| | |
|<R78>|FORM + HO2 = HCO3|9.7E-15*exp<sup>(625/T)</sup>| | |
|<R79>|HCO3 = FORM + HO2|2.4E + 12*exp<sup>(-7000/T)</sup>| | |
|<R80>|HCO3 + NO = FACD + NO2 + HO2|5.6E-12| | |
|<R81>|HCO3 + HO2 = MEPX|5.6E-15*exp<sup>(2300/T)</sup>| | |
|<R82>|FACD + OH = HO2|4.0E-13| | |
|<R83>|ALD2 + O = C2O3 + OH|1.8E-11*exp<sup>(-1100/T)</sup>| | |
|<R84>|ALD2 + OH = C2O3|5.6E-12*exp<sup>(270/T)</sup>| | |
|<R85>|ALD2 + NO3 = C2O3 + HNO3|1.4E-12*exp<sup>(-1900/T)</sup>| | |
|<R86>|ALD2 = MEO2 + CO + HO2|1.0/<ALD2_R_IUPAC10>| | |
|<R87>|C2O3 + NO = MEO2 + NO2|8.1E-12*exp<sup>(270/T)</sup>| | |
|<R88>|C2O3 + NO2 = PAN|k<sub>0</sub>=2.7E-28*(T/300)<sup>(-7.1)</sup>, k<sub>inf</sub>=1.2E-11*(T/300)<sup>(-0.9)</sup>, F=0.3, n=1.41|added N=1.41| IUPAC |
|<R89>|PAN = C2O3 + NO2|k<sub>0</sub>=4.9E-3*exp<sup>(-12100/T)</sup>, k<sub>inf</sub>=5.4E16*exp<sup>(-13830/T)</sup>, F=0.3, n=1.41|added N=1.41| |
|<R90>|PAN = 0.6*NO2 + 0.6*C2O3 + 0.4*NO3 + 0.4*MEO2|1.0/<PAN_IUPAC10>|added 40% yield of MEO2 and NO3| |
|<R91>|C2O3 + HO2 = 0.410*PACD + 0.150*AACD + 0.440*OH + 0.440*MEO2 + 0.150*O3|4.3E-13*exp<sup>(1040/T)</sup>| | |
|<R92>|C2O3 + MEO2 = 0.900*MEO2 + 0.900*HO2 + FORM + 0.100*AACD|2.0E-12*exp<sup>(500/T)</sup>| | |
|<R93>|C2O3 + XO2 = 0.900*MEO2 + 0.100*AACD|4.4E-13*exp<sup>(1070/T)</sup>| | |
|<R94>|C2O3 + C2O3 = 2.000*MEO2|2.9E-12*exp<sup>(500/T)</sup>| | |
|<R95>|PACD + OH = C2O3|4.0E-13*exp<sup>(200/T)</sup>| | |
|<R96>|PACD = MEO2 + OH|1.0/<PACD_CB05>| | |
|<R97>|AACD + OH = MEO2|4.0E-13*exp<sup>(200/T)</sup>| | |
|<R98>|ALDX + O = CXO3 + OH|1.3E-11*exp<sup>(-870/T)</sup>| | |
|<R99>|ALDX + OH = CXO3|5.1E-12*exp<sup>(405/T)</sup>| | |
|<R100>|ALDX + NO3 = CXO3 + HNO3|6.5E-15| | |
|<R101>|ALDX = MEO2 + CO + HO2|1.0/<ALDX_R_IUPAC10>| | |
|<R102>|CXO3 + NO = ALD2 + NO2 + HO2 + XO2|6.7E-12*exp<sup>(340/T)</sup>| | |
|<R103>|CXO3 + NO2 = PANX|k<sub>0</sub>=2.7E-28*(T/300)<sup>(-7.1)</sup>, k<sub>inf</sub>=1.2E-11*(T/300)<sup>(-0.9)</sup>, F=0.3, n=1.41|added N=1.41 | |
|<R104>|PANX = CXO3 + NO2|k<sub>0</sub>=1.7E-3*exp<sup>(-11280/T)</sup>, k<sub>inf</sub>=8.3E16*exp<sup>(-13940/T)</sup>, F=0.36, n=1|set equal to PPN| |
|<R105>|PANX = 0.6*CXO3 + 0.6*NO2 + 0.4*NO3 + 0.4*ALD2 + 0.4*HO2 + 0.4*XO2|1.0/<PAN_IUPAC10>|added 40% other products | |
|<R106>|PANX + OH = ALD2 + NO2|3.0E-13| | |
|<R107>|CXO3 + HO2 = 0.410*PACD + 0.150*AACD + 0.440*OH + 0.440*XO2 + 0.150*O3|4.3E-13*exp<sup>(1040/T)</sup>| | |
|<R108>|CXO3 + MEO2 = 0.900*ALD2 + 0.900*XO2 + HO2 + 0.100*AACD + 0.100*FORM|2.0E-12*exp<sup>(500/T)</sup>| | |
|<R109>|CXO3 + XO2 = 0.900*ALD2 + 0.100*AACD|4.4E-13*exp<sup>(1070./T)</sup>| | |
|<R110>|CXO3 + CXO3 = 2.000*ALD2 + 2.000*XO2 + 2.000*HO2|2.9E-12*exp<sup>(500/T)</sup>| | |
|<R111>|CXO3 + C2O3 = MEO2 + XO2 + HO2 + ALD2|2.9E-12*exp<sup>(500/T)</sup>| | |
|<R112>|PAR + OH = 0.870*XO2 + 0.130*XO2N + 0.110*HO2 + 0.060*ALD2-0.110*PAR + 0.760*ROR + 0.050*ALDX|8.1E-13| | |
|<R113>|ROR = 0.960*XO2 + 0.600*ALD2 + 0.940*HO2-2.100*PAR + 0.040*XO2N + 0.020*ROR + 0.500*ALDX|1.E + 15*exp<sup>(-8000./T)</sup>| | | |<R114>|ROR = HO2|1.6E + 3| | |
|<R115>|ROR + NO2 = NTRALK|1.5E-11| | |
|<R116>|O + OLE = 0.200*ALD2 + 0.300*ALDX + 0.300*HO2 + 0.200*XO2 + 0.200*CO + 0.200*FORM + 0.010*XO2N + 0.200*PAR + 0.100*OH|1.E-11*exp<sup>(-280./T)</sup>| | |
|<R117>|OH + OLE = 0.800*FORM + 0.330*ALD2 + 0.620*ALDX + 0.800*XO2 + 0.950*HO2-0.700*PAR|3.2E-11| | |
|<R118>|O3 + OLE = 0.180*ALD2 + 0.740*FORM + 0.320*ALDX + 0.220*XO2 + 0.100*OH + 0.330*CO + 0.440*HO2-1.000*PAR|6.5E-15*exp<sup>(-1900./T)</sup>| | |
|<R119>|NO3 + OLE = NO2 + FORM + 0.910*XO2 + 0.090*XO2N + 0.560*ALDX + 0.350*ALD2-1.000*PAR|7.0E-13*exp<sup>(-2160./T)</sup>| | |
|<R120>|O + ETH = FORM + 1.700*HO2 + CO + 0.700*XO2 + 0.300*OH|1.04E-11*exp<sup>(-792/T)</sup>| | |
|<R121>|OH + ETH = XO2 + 1.560*FORM + 0.220*ALDX + HO2|k<sub>0</sub>=1.0E-28*(T/300)<sup>(-0.8)</sup>, k<sub>inf</sub>=8.8E-12, F=0.6, n=1| | |
|<R122>|O3 + ETH = FORM + 0.630*CO + 0.130*HO2 + 0.130*OH + 0.370*FACD|1.2E-14*exp<sup>(-2630/T)</sup>| | |
|<R123>|NO3 + ETH = NO2 + XO2 + 2.0*FORM|3.3E-12*exp<sup>(-2880./T)</sup>| | |
|<R124>|IOLE + O = 1.240*ALD2 + 0.660*ALDX + 0.100*HO2 + 0.100*XO2 + 0.100*CO + 0.100*PAR|2.3E-11| | |
|<R125>|IOLE + OH = 1.300*ALD2 + 0.700*ALDX + HO2 + XO2|1.0E-11*exp<sup>(550./T)</sup>| | |
|<R126>|IOLE + O3 = 0.650*ALD2 + 0.350*ALDX + 0.250*FORM + 0.250*CO + 0.500*O + 0.500*OH + 0.500*HO2|8.4E-15*exp<sup>(-1100./T)</sup>| | |
|<R127>|IOLE + NO3 = 1.180*ALD2 + 0.640*ALDX + HO2 + NO2|9.6E-13*exp<sup>(-270./T)</sup>| | |
|<R128>|TOL + OH = 0.28*HO2 + 0.1*XO2 + 0.18*CRES + 0.65*TO2 + 0.072*OH + 1.0*TOLRO2|1.8E-12*exp<sup>(355/T)</sup>| | |
|<R129>|TO2 + NO = 0.86*NO2 + 1.2*HO2 + 0.86*OPEN + 0.14*NTROH + 0.52*MGLY + 0.336*FORM + 0.336*CO|2.70E-12*exp<sup>(360/T)</sup>| | |
|<R130>|TO2 + HO2 = |1.90E-13*exp<sup>(1300/T)</sup>| | |
|<R131>|OH + CRES = 0.06*CRO + 0.12*XO2 + 1.12*HO2 + 0.13*OPEN + 0.732*CAT1 + 0.06*CO + 0.06*XO2N + 0.06*FORM|1.70E-12*exp<sup>(950/T)</sup>| | |
|<R132>|CRES + NO3 = 0.3*CRO + HNO3 + 0.6*XO2 + 0.36*HO2 + 0.48*ALDX + 0.24*FORM + 0.24*MGLY + 0.12*OPEN + 0.1*XO2N + 0.24*CO|1.4E-11| | |
|<R133>|CRO + NO2 = CRON|2.1E-12| | |
|<R134>|CRO + HO2 = CRES|5.5E-12| | |
|<R135>|CRON + OH = CRNO|1.53E-12| | |
|<R136>|CRON + NO3 = CRNO + HNO3|3.8E-12| | |
|<R137>|CRNO + NO2 = 2*NTROH|2.1E-12| | |
|<R138>|CRNO + O3 = CRN2|2.86E-13| | |
|<R139>|CRN2 + NO = CRNO + NO2|2.54E-12*exp<sup>(360/T)</sup>| | |
|<R140>|CRN2 + HO2 = CRPX|2.4E-13*exp<sup>(1300/T)</sup>| | |
|<R141>|CRPX = CRNO + OH|0.01/<NO2_IUPAC10>| | |
|<R142>|CRPX + OH = CRN2|1.9E-12*exp<sup>(190/T)</sup>| | |
|<R143>|OPEN = OPO3 + HO2 + CO|0.04/<NO2_IUPAC10>| | |
|<R144>|OPEN + OH = 0.6*OPO3 + 0.4*CAO2|4.4E-11| | |
|<R145>|OPEN + O3 = 0.03*ALDX + 0.62*OPO3 + 0.7*FORM + 0.03*XO2 + 0.69*CO + 0.08*OH + 0.76*HO2 + 0.2*MGLY|5.4E-17*exp<sup>(-500./T)</sup>| | |
|<R146>|OPEN + NO3 = OPO3 + HNO3|3.8E-12| | |
|<R147>|CAT1 + OH = CAO2|7.0E-11| | |
|<R148>|CAT1 + NO3 = CRO + HNO3|1.7E-10| | |
|<R149>|CAO2 + NO = 0.86*NO2 + 1.2*HO2 + 0.344*FORM + 0.344*CO + 0.14*NTROH|2.54E-12*exp<sup>(360./T)</sup>| | |
|<R150>|CAO2 + HO2 = |2.40E-13*exp<sup>(1300./T)</sup>| | |
|<R151>|OPO3 + NO = NO2 + XO2 + HO2 + ALDX|1.1E-11| | |
|<R152>|OPO3 + NO2 = OPAN|1.1E-11| | |
|<R153>|OPAN = OPO3 + NO2|1.0E-4| | |
|<R154a>|OH + XYLMN = 0.700*HO2 + 0.500*XO2 + 0.200*CRES + 0.800*MGLY + 1.100*PAR + 0.300*TO2 + 1.0*XYLRO2|1.7E-11*exp<sup>(116./T)</sup>|xylene model species without naphthalene | |
|<R154b>|OH + NAPH = 0.700*HO2 + 0.500*XO2 + 0.200*CRES + 0.800*MGLY + 1.100*PAR + 0.300*TO2 + 1.0*PAHRO2|1.7E-11*exp<sup>(116./T)</sup>|same products and rate as XYL | |
|<R155>|OH + MGLY = XO2 + C2O3|1.8E-11| | |
|<R156>|MGLY = C2O3 + HO2 + CO|1.0/<MGLY_IUPAC10>| | |
|<R157>|O + ISOP = 0.750*ISPD + 0.500*FORM + 0.250*XO2 + 0.250*HO2 + 0.250*CXO3 + 0.250*PAR|3.6E-11| | |
|<R158>|ISOP + OH = ISOPO2 + ISOPRXN|2.70E-11*exp<sup>(390/T)</sup>| | |
|<R158a>|ISOPO2 + NO = 0.1*NTRM + 0.9*NO2 + 0.673*FORM + 0.9*ISPD + 0.9*HO2|2.39E-12*exp<sup>(365/T)</sup>|NTRM is specific to biogenics | |
|<R158b>|ISOPO2 + C2O3 = 0.598*FORM + 1.0*ISPD + 0.8*HO2 + 0.8*MEO2 + 0.2*AACD|1.00E + 0*K<R58>| | |
|<R158c>|ISOPO2 + HO2 = 1.00*ISOPX|7.43E-13*exp<sup>(700/T)</sup>| | |
|<R159>|O3 + ISOP = 0.650*ISPD + 0.600*FORM + 0.200*XO2 + 0.066*HO2 + 0.266*OH + 0.200*MACO3 + 0.150*ALDX + 0.350*PAR + 0.066*CO|7.86E-15*exp<sup>(-1912/T)</sup>| | |
|<R160>|NO3 + ISOP = 0.200*ISPD + 0.800*NTRM + XO2 + 0.800*HO2 + 0.200*NO2 + 0.800*ALDX + ISOPRXN + 2.400*PAR|3.03E-12*exp<sup>(-448/T)</sup>| | |
|<R161>|OH + ISPD = 1.565*PAR + 0.167*FORM + 0.713*XO2 + 0.503*HO2 + 0.334*CO + 0.168*MGLY + 0.252*ALD2 + 0.330*C2O3 + 0.130*MACO3 + 0.120*ALDX|3.36E-11| | |
|<R162>|O3 + ISPD = 0.114*C2O3 + 0.150*FORM + 0.850*MGLY + 0.154*HO2 + 0.268*OH + 0.064*XO2 + 0.020*ALD2 + 0.360*PAR + 0.225*CO|7.1E-18| | |
|<R163>|NO3 + ISPD = 0.357*ALDX + 0.282*FORM + 1.282*PAR + 0.925*HO2 + 0.643*CO + 0.850*NTRI + 0.075*MACO3 + 0.075*XO2 + 0.150*HNO3|1.0E-15| | |
|<R164>|ISPD = 0.333*CO + 0.067*ALD2 + 0.900*FORM + 0.832*PAR + 0.700*XO2 + 1.033*HO2 + 0.700*MACO3 + 0.267*C2O3|0.0036/<ACRO_09>| | |
|<R165>|TERP + O = 0.150*ALDX + 5.12*PAR + TRPRXN|3.6E-11| | |
|<R166>|TERP + OH = 0.750*HO2 + 1.250*XO2 + 0.250*XO2T + 0.280*FORM + 1.66*PAR + 0.470*ALDX + TRPRXN|1.5E-11*exp<sup>(449./T)</sup>| | |
|<R166a>|XO2T + NO = NTRM|2.6E-12*exp<sup>(365/T)</sup>|XO2T instead of XO2 | |
|<R166b>|XO2T + HO2 = ROOH|7.5E-13*exp<sup>(700/T)</sup>| | |
|<R166c>|XO2T + XO2T = |6.8E-14| | |
|<R166d>|XO2 + XO2T = |6.8E-14| | |
|<R167>|TERP + O3 = 0.570*OH + 0.070*HO2 + 0.760*XO2 + 0.180*XO2T + 0.240*FORM + 0.001*CO + 7.000*PAR + 0.210*ALDX + 0.390*CXO3 + TRPRXN|1.2E-15*exp<sup>(-821./T)</sup>| | |
|<R168>|TERP + NO3 = 0.470*NO2 + 0.280*HO2 + 1.030*XO2 + 0.250*XO2T + 0.470*ALDX + 0.530*NTRM + TRPRXN|3.7E-12*exp<sup>(175./T)</sup>| | |
|<R169>|SO2 + OH = SULF + HO2 + SULRXN|k<sub>0</sub>=3.3E-31*(T/300)<sup>(-4.3)</sup>, k<sub>inf</sub>=1.6E-12*(T/300)<sup>(0.0)</sup>, F=0.6, n=1| | |
|<R170>|OH + ETOH = HO2 + 0.950*ALD2 + 0.010*ALDX + 0.080*FORM + 0.050*XO2|6.9E-12*exp<sup>(-230/T)</sup>| | |
|<R171>|OH + ETHA = 0.991*ALD2 + 0.991*XO2 + 0.009*XO2N + HO2|8.7E-12*exp<sup>(-1070/T)</sup>| | |
|<R172>|NO2 + ISOP = 0.200*ISPD + 0.800*NTROH + XO2 + 0.800*HO2 + 0.200*NO + 0.800*ALDX + 2.400*PAR|1.5E-19| | |
|<CL1>|CL2 = 2.000*CL|1.0/<CL2_IUPAC04>| | |
|<CL2>|HOCL = OH + CL|1.0/<HOCL_IUPAC04>| | |
|<CL3>|CL + O3 = CLO|2.3E-11*exp<sup>(-200/T)</sup>| | |
|<CL4>|CLO + CLO = 0.300*CL2 + 1.400*CL|1.63E-14| | |
|<CL5>|CLO + NO = CL + NO2|6.4E-12*exp<sup>(290/T)</sup>| | |
|<CL6>|CLO + HO2 = HOCL|2.7E-12*exp<sup>(220/T)</sup>| | |
|<CL7>|OH + FMCL = CL + CO|5.0E-13| | |
|<CL8>|FMCL = CL + CO + HO2|1.0/<FMCL_IUPAC04>| | |
|<CL9>|CL + CH4 = HCL + MEO2|6.6E-12*exp<sup>(-1240/T)</sup>| | |
|<CL10>|CL + PAR = HCL + 0.870*XO2 + 0.130*XO2N + 0.110*HO2 + 0.060*ALD2-0.110*PAR + 0.760*ROR + 0.050*ALDX|5.0E-11| | |
|<CL11>|CL + ETHA = HCL + 0.991*ALD2 + 0.991*XO2 + 0.009*XO2N + HO2|8.3-11*exp<sup>(-100/T)</sup>| | |
|<CL12>|CL + ETH = FMCL + 2.000*XO2 + 1.000*HO2 + 1.000*FORM|1.07E-10| | |
|<CL13>|CL + OLE = FMCL + 0.330*ALD2 + 0.670*ALDX + 2.000*XO2 + 1.000*HO2-1.000*PAR|2.5E-10| | |
|<CL14>|CL + IOLE = 0.300*HCL + 0.700*FMCL + 0.450*ALD2 + 0.550*ALDX + 0.300*OLE + 0.300*PAR + 1.700*XO2 + 1.000*HO2|3.5E-10| | |
|<CL15>|CL + ISOP = 0.15*HCL + 1.000*XO2 + 1.000*HO2 + 0.850*FMCL + 1.000*ISPD|4.3E-10| | |
|<CL16>|CL + FORM = HCL + 1.000*HO2 + 1.000*CO|8.2E-11*exp<sup>(-34/T)</sup>| | |
|<CL17>|CL + ALD2 = HCL + 1.000*C2O3|7.9E-11| | |
|<CL18>|CL + ALDX = HCL + 1.000*CXO3|1.3E-10| | |
|<CL19>|CL + MEOH = HCL + 1.000*HO2 + 1.000*FORM|5.5E-11| | |
|<CL20>|CL + ETOH = HCL + 1.000*HO2 + 1.000*ALD2|8.2E-11*exp<sup>(45/T)</sup>| | |
|<CL21>|HCL + OH = CL|6.58E-13*(T/300)<sup>(1.16)</sup>*exp<sup>(58/T)</sup>| | |
|<CL22>|CL + TOL = HCL + 0.88*XO2 + 0.88*HO2 + 0.12*XO2N|6.1E-11| | |
|<CL23a>|CL + XYLMN = HCL + 0.84*XO2 + 0.84*HO2 + 0.16*XO2N|1.2E-10| | |
|<CL23b>|CL + NAPH = HCL + 0.84*XO2 + 0.84*HO2 + 0.16*XO2N|1.2E-10| | |
|<CL24>|CL + NO2 = CLNO2|k<sub>0</sub>=1.80E-31*(T/300)<sup>(-2.00)</sup>, k<sub>inf</sub>=1.00E-10*(T/300)<sup>(-1.00)</sup>, F=0.60, n=1.0| | |
|<CL25>|CLNO2 = CL + NO2|1.0/<CLNO2>| | |
|<SA01>|TOLRO2 + NO = NO + TOLNRXN|2.70e-12*exp<sup>(360/T)</sup>| | |
|<SA02>|TOLRO2 + HO2 = HO2 + TOLHRXN|1.90e-13*exp<sup>(1300/T)</sup>| | |
|<SA03>|XYLRO2 + NO = NO + XYLNRXN|2.70e-12*exp<sup>(360/T)</sup>| | |
|<SA04>|XYLRO2 + HO2 = HO2 + XYLHRXN|1.90e-13*exp<sup>(1300/T)</sup>| | |
|<SA05>|BENZENE + OH = OH + 1.0*BENZRO2|2.47e-12*exp<sup>(-206/T)</sup>| | |
|<SA06>|BENZRO2 + NO = NO + BNZNRXN|2.70e-12*exp<sup>(360/T)</sup>| | |
|<SA07>|BENZRO2 + HO2 = HO2 + BNZHRXN|1.90e-13*exp<sup>(1300/T)</sup>| | |
|<SA08>|SESQ + O3 = O3 + SESQRXN|1.16E-14| | |
|<SA09>|SESQ + OH = OH + SESQRXN|1.97E-10| | |
|<SA10>|SESQ + NO3 = NO3 + SESQRXN|1.90E-11| | |
|<SA11>|PAHRO2 + NO = NO + PAHNRXN|2.70e-12*exp<sup>(360/T)</sup>| | |
|<SA12>|PAHRO2 + HO2 = HO2 + PAHHRXN|1.90e-13*exp<sup>(1300/T)</sup>| | |
|<SA13>|SOAALK + OH = OH + 0.47*ALKRXN|2.70e-12*exp<sup>(374/T)</sup>| | |
|<R63M>|MACO3 + NO = FORM + NO2 + MEO2|6.7E-12*exp<sup>(340/T)</sup>| | |
|<R64M>|MACO3 + NO2 = MAPAN|1.21E-11*(T/300)<sup>(-1.07)</sup>*exp<sup>(-0/T)</sup>| | |
|<R65M>|MAPAN = MACO3 + NO2|1.6E + 16*exp<sup>(-13486/T)</sup>| | |
|<R66M>|MAPAN = 0.6*MACO3 + 0.6*NO2 + 0.4*NO3 + 0.4*FORM + 0.4*C2O3|1.0/<PAN_IUPAC10>| | |
|<R67M>|MAPAN + OH = ALD2 + CO + NO2|2.90e-11| | |
|<R68M>|MACO3 + HO2 = 0.15*C2O3 + 0.44*FORM + 0.29*MEO2 + 0.29*CO + 0.44*OH + 0.15*AACD + 0.15*O3 + 0.41*PACD|5.2E-13*exp<sup>(980/T)</sup>| | |
|<R69M>|MACO3 + MEO2 = 1.9*FORM + 0.9*HO2 + 0.3*C2O3 + 0.6*MEO2 + 0.1*AACD|2.0E-12*exp<sup>(500/T)</sup>| | |
|<R70M>|MACO3 + XO2 = ALD2 + 0.3*C2O3 + 0.9*FORM + 0.6*MEO2 + 0.6*CO + 0.1*AACD|4.4E-13*exp<sup>(1070./T)</sup>| | |
|<R71M>|MACO3 + CXO3 = ALD2 + XO2 + HO2 + FORM + 0.35*C2O3 + 0.65*CO|2.9E-12*exp<sup>(500/T)</sup>| | |
|<R72M>|MACO3 + C2O3 = 1.65*MEO2 + FORM + 0.35*C2O3 + 0.65*CO|2.9E-12*exp<sup>(500/T)</sup>| | |
|<N08>|NTRALK + OH = NALKO2|1.29e-12| | |
|<N08b>|NALKO2 + NO = 1.30*NO2 + 0.15*NTRCN + 0.55*NTRCNOH|2.7e-12*exp<sup>(360/T)</sup>| | |
|<N08c>|NALKO2 + HO2 = NTRPX|2.05e-13*exp<sup>(1300/T)</sup>| | |
|<N09>|NTROH + OH = NOHO2|7.26E-12| | |
|<N09b>|NOHO2 + NO = 1.22*NO2 + 0.53*NTRCN + 0.25*NTRCNOH|2.7e-12*exp<sup>(360/T)</sup>| | |
|<N09c>|NOHO2 + HO2 = NTRPX|2.05e-13*exp<sup>(1300/T)</sup>| | |
|<N10>|NTRCN + OH = NCNO2|1.1e-12| | |
|<N10b>|NCNO2 + NO = 1.53*NO2 + 0.21*NTRCN + 0.26*NTRCNOH|2.7e-12*exp<sup>(360/T)</sup>| | |
|<N10c>|NCNO2 + HO2 = NTRPX|2.05e-13*exp<sup>(1300/T)</sup>| | |
|<N11>|NTRCNOH + OH = NCNOHO2|5.7e-12| | |
|<N11b>|NCNOHO2 + NO = 1.24*NO2 + 0.59*NTRCNOH + 0.17*NTRCN|2.7e-12*exp<sup>(360/T)</sup>| | |
|<N11c>|NCNOHO2 + HO2 = NTRPX|2.05e-13*exp<sup>(1300/T)</sup>| | |
|<N14>|NTRPX + OH = NTRCN + OH|6.0E-12| | |
|<N15>|NTRM + OH = NTRMO2|3.3e-11| | |
|<N15b>|NTRMO2 + NO = 0.87*NO2 + 1.15*NTRI + 0.68*HO2 + 0.55*FORM + 0.15*ALD2 + 0.43*PAR|2.7e-12*exp<sup>(360/T)</sup>| | |
|<N15c>|NTRMO2 + HO2 = NTRI|2.05e-13*exp<sup>(1300/T)</sup>| | |
|<N16>|NTRI + OH = NTRIO2|2.32e-12| | |
|<N16b>|NTRIO2 + NO = 1.40*NO2 + 0.60*NTRI|2.7e-12*exp<sup>(360/T)</sup>| | |
|<N16c>|NTRIO2 + HO2 = NTRI|2.05e-13*exp<sup>(1300/T)</sup>| | |
|<N17>|NTRM = NO2 + HO2 + 1.0*ALD2 + 1.0*IOLE|1.0/<NTR_IUPAC10>| | |
|<N18b>|NTRI = NO2 + HO2 + 0.330*FORM + 0.330*ALD2 + 0.330*ALDX-0.660*PAR|1.0/<NOA_14>| | |
|<N19>|NTRALK = NO2 + HO2 + 0.330*FORM + 0.330*ALD2 + 0.330*ALDX-0.660*PAR|1.0/<NTR_IUPAC10>| | |
|<N20>|NTROH = NO2 + HO2 + 0.330*FORM + 0.330*ALD2 + 0.330*ALDX-0.660*PAR|1.0/<NTR_IUPAC10>| | |
|<N21>|NTRCN = NO2 + HO2 + 0.330*FORM + 0.330*ALD2 + 0.330*ALDX-0.660*PAR|1.0/<NBO_14>| | |
|<N22>|NTRCNOH = NO2 + HO2 + 0.330*FORM + 0.330*ALD2 + 0.330*ALDX-0.660*PAR|1.0/<NBO_14>| | |
|<N25>|NTRPX = NO2 + HO2 + 1.0*ROOH|2.0/<NTR_IUPAC10>| | |
|<HET_NT1>|NTRALK = HNO3|1.0~<HETERO_NTR2>| | |
|<HET_NT2>|NTROH = HNO3|1.0~<HETERO_NTR2>| | |
|<HET_NT3>|NTRCN = HNO3|1.0~<HETERO_NTR2>| | |
|<HET_NT4>|NTRCNOH = HNO3|1.0~<HETERO_NTR2>| | |
|<HET_NT5>|NTRPX = HNO3|1.0~<HETERO_NTR2>| | |
|<HET_NT6>|NTRM = HNO3|1.0~<HETERO_NTR2>| | |
|<HET_NT7>|NTRI = HNO3|1.0~<HETERO_NTR2>| | |
|<HET_N2O5IJ>|N2O5 = HNO3 + H2NO3PIJ|1.0~<HETERO_N2O5IJ>| | |
|<HET_N2O5K>|N2O5 = HNO3 + H2NO3PK|1.0~<HETERO_N2O5K>| | |
|<HET_H2NO3PIJA>|H2NO3PIJ = HNO3|1.0~<HETERO_H2NO3PAIJ>| | |
|<HET_H2NO3PKA>|H2NO3PK = HNO3|1.0~<HETERO_H2NO3PAK>| | |
|<HET_H2NO3PIB>|H2NO3PIJ + ACLI = CLNO2|1.0~<HETERO_H2NO3PBIJ>| | |
|<HET_H2NO3PJB>|H2NO3PIJ + ACLJ = CLNO2|1.0~<HETERO_H2NO3PBIJ>| | |
|<HET_H2NO3PKB>|H2NO3PK + ACLK = CLNO2|1.0~<HETERO_H2NO3PBK>| | |
|<HET_N02>|NO2 = 0.5*HONO + 0.5*HNO3|1.0~<HETERO_NO2>| | |
|<HAL_Ozone>|O3 = |min(1.0E-40*exp<sup>(78.4256*P)</sup>+4.0582E-9*exp<sup>(5.8212*P)</sup>, 2.4E-06) |Set to zero '''if sun is below the horizon''' and '''if surface does not include sea or surf zones'''; '''P''' equals air pressure in atmospheres | [[CMAQv5.1_Halogen_chemistry]] |
|<HET_IEPOX>|IEPOX = AISO3J|1.0~<HETERO_IEPOX>| | |
|<OLIG_XYLENE1>|AXYL1J = 1.1428*AOLGAJ|9.48816E-6| | |
|<OLIG_XYLENE2>|AXYL2J = 1.1428*AOLGAJ|9.48816E-6| | |
|<OLIG_TOLUENE1>|ATOL1J = 1.0000*AOLGAJ|9.48816E-6| | |
|<OLIG_TOLUENE2>|ATOL2J = 1.0000*AOLGAJ|9.48816E-6| | |
|<OLIG_BENZENE1>|ABNZ1J = 0.85714*AOLGAJ|9.48816E-6| | |
|<OLIG_BENZENE2>|ABNZ2J = 0.85714*AOLGAJ|9.48816E-6| | |
|<OLIG_TERPENE1>|ATRP1J = 1.0000*AOLGBJ|9.48816E-6| | |
|<OLIG_TERPENE2>|ATRP2J = 1.0000*AOLGBJ|9.48816E-6| | |
|<OLIG_ISOPRENE1>|AISO1J = 0.50*AOLGBJ|9.48816E-6| | |
|<OLIG_ISOPRENE2>|AISO2J = 0.50*AOLGBJ|9.48816E-6| | |
|<OLIG_SESQT1>|ASQTJ = 1.50*AOLGBJ|9.48816E-6| | |
|<OLIG_PAH1>|APAH1J = 1.4286*AOLGAJ|9.48816E-6| | |
|<OLIG_PAH2>|APAH2J = 1.4286*AOLGAJ|9.48816E-6| | |
|<OLIG_ALK1>|AALK1J = 1.7143*AOLGAJ|9.48816E-6| | |
|<OLIG_ALK2>|AALK2J = 1.7143*AOLGAJ|9.48816E-6| | |
|<RPOAGEPI>|APOCI + OH = 1.25*APNCOMI + APOCI + OH|2.5E-12| | |
|<RPOAGELI>|APNCOMI + OH = OH|1.0~<HETERO_PNCOMLI>| | |
|<RPOAGEPJ>|APOCJ + OH = 1.25*APNCOMJ + APOCJ + OH|2.5E-12| | |
|<RPOAGELJ>|APNCOMJ + OH = OH|1.0~<HETERO_PNCOMLJ>| | |
|<T01>|FORM_PRIMARY + OH = OH|9.0E-12|tracer for emitted HCHO | |
|<T02>|FORM_PRIMARY + NO3 = NO3|5.8E-16| | |
|<T03>|FORM_PRIMARY + O = O|3.4E-11*exp<sup>(-1600/T)</sup>| | |
|<T04>|FORM_PRIMARY = |1.0/<FORM_R_IUPAC10>| | |
|<T05>|FORM_PRIMARY = |1.0/<FORM_M_IUPAC10>| | |
|<TCL1>|FORM_PRIMARY + CL = CL|8.2E-11*exp<sup>(-34/T)</sup>| | |
|<T06>|ALD2_PRIMARY + OH = OH|5.6E-12*exp<sup>(270/T)</sup>|tracer for emitted acetaldehyde | |
|<T07>|ALD2_PRIMARY + NO3 = NO3|1.4E-12*exp<sup>(-1900/T)</sup>| | |
|<T08>|ALD2_PRIMARY + O = O|1.8E-11*exp<sup>(-1100/T)</sup>| | |
|<T09>|ALD2_PRIMARY = |1.0/<ALD2_R_IUPAC10>| | |
|<TCL2>|ALD2_PRIMARY + CL = CL|7.9E-11| | |
|<T10>|BUTADIENE13 + OH = OH + 0.58*ACROLEIN|1.4E-11*exp<sup>(424./T)</sup>| | |
|<T11>|BUTADIENE13 + O3 = O3 + 0.52*ACROLEIN|8.2E-15*exp<sup>(-2070./T)</sup>| | |
|<T12>|BUTADIENE13 + NO3 = NO3 + 0.045*ACROLEIN|1.79E-13| | |
|<TCL3>|BUTADIENE13 + CL = CL + 0.58*ACROLEIN|2.51E-10| | |
|<T14>|ACRO_PRIMARY + OH = OH|2.0E-11|tracer for emitted acrolein| |
|<T15>|ACRO_PRIMARY + O3 = O3|2.61E-19| | |
|<T16>|ACRO_PRIMARY + NO3 = NO3|1.7E-11*exp<sup>(-3131./T)</sup>| | |
|<T17>|ACRO_PRIMARY = |1.0/<ACRO_09>| | |
|<TCL4>|ACRO_PRIMARY + CL = CL|2.37E-10| | |
|<T18>|ACROLEIN + OH = OH|2.0E-11| | |
|<T19>|ACROLEIN + O3 = O3|2.61E-19| | |
|<T20>|ACROLEIN + NO3 = NO3|1.7E-11*exp<sup>(-3131./T)</sup>| | |
|<T21>|ACROLEIN = |1.0/<ACRO_09>| | |
|<TCL5>|ACROLEIN + CL = CL|2.37E-10| | |
|<T22>|TOLU + OH = OH|1.8E-12*exp<sup>(355./T)</sup>| | |
|<TCL6>|TOLU + CL = CL|6.1E-11| | |
|<T23>|MXYL + OH = OH|1.7E-11*exp<sup>(116./T)</sup>| | |
|<TCL7>|MXYL + CL = CL|1.4E-10| | |
|<T24>|OXYL + OH = OH|1.22E-11| | |
|<TCL8>|OXYL + CL = CL|1.5E-10| | |
|<T25>|PXYL + OH = OH|1.3E-11| | |
|<TCL9>|PXYL + CL = CL|1.5E-10| | |
|<T26>|APIN + O = O|2.79E-11| | |
|<T27>|APIN + OH = OH|1.2E-11*exp<sup>(440./T)</sup>| | |
|<T28>|APIN + O3 = O3|6.3E-16*exp<sup>(-580./T)</sup>| | |
|<T29>|APIN + NO3 = NO3|1.2E-12*exp<sup>(490./T)</sup>| | |
|<TCL10>|APIN + CL = CL|4.7E-10| | |
|<T30>|BPIN + O = O|2.81E-11| | |
|<T31>|BPIN + OH = OH|7.51E-11| | |
|<T32>|BPIN + O3 = O3|1.74E-15*exp<sup>(-1260./T)</sup>| | |
|<T33>|BPIN + NO3 = NO3|2.81E-11| | |
|<TCL11>|BPIN + CL = CL|5.3E-10| | |
|<HG1>|HG + O3 = 0.5*HGIIAER + 0.5*HGIIGAS + O3|2.11E-18*exp<sup>(-1256.5/T)</sup>| | |
|<HG2>|HG + CL2 = HGIIGAS + CL2|2.6E-18| | |
|<HG3>|HG + H2O2 = HGIIGAS + H2O2|8.5E-19| | |
|<HG4>|HG + OH = 0.5*HGIIAER + 0.5*HGIIGAS + OH|7.7E-14| | |
|<HG5>|HG + CL + M = 0.5*HG + 0.5*HGIIGAS + M + CL|2.25E-33*exp<sup>(-680.0/T)</sup>| | |
