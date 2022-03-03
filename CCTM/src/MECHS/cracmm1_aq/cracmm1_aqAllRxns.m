%Foam Reactions File based on the mech.def file for the cracmm1_aq mechanism.
% # of species   =  180
% # of reactions =  508
% file created by Bryan Place

% Set constant species by scaling to air number density
N2  =  0.780800000.*M;
O2  =  0.209500000.*M;
H2  =  0.000000560.*M;
CH4 =  0.000001850.*M;


SpeciesToAdd = {...
'O3'; 'O3P'; 'O1D'; 'H2O2'; 'HO'; ...
'NO2'; 'NO'; 'NO3'; 'HONO'; 'HNO3'; ...
'HNO4'; 'HO2'; 'HCHO'; 'CO'; 'ACD'; ...
'MO2'; 'ALD'; 'ETHP'; 'ACT'; 'ACO3'; ...
'UALD'; 'KET'; 'PINAL'; 'HC10P'; 'LIMAL'; ...
'MEK'; 'HKET'; 'MACR'; 'MACP'; 'XO2'; ...
'MVK'; 'GLY'; 'MGLY'; 'DCB1'; 'DCB2'; ...
'BALD'; 'CHO'; 'OP1'; 'OP2'; 'OPB'; ...
'PAA'; 'ONIT'; 'PAN'; 'N2O5'; 'SO2'; ...
'SULF'; 'SULRXN'; 'ETH'; 'HC3'; 'HC3P'; ...
'ASOATJ'; 'HC5'; 'HC5P'; 'ETE'; 'ETEP'; ...
'OLT'; 'OLTP'; 'OLI'; 'OLIP'; 'ACE'; ...
'ORA1'; 'BEN'; 'BENP'; 'PHEN'; 'TOL'; ...
'TOLP'; 'CSL'; 'XYM'; 'XYMP'; 'XYE'; ...
'XYEP'; 'ISO'; 'ISOP'; 'API'; 'APIP1'; ...
'APIP2'; 'LIM'; 'LIMP1'; 'LIMP2'; 'PINALP'; ...
'RCO3'; 'LIMALP'; 'ACTP'; 'MEKP'; 'KETP'; ...
'MCP'; 'MVKP'; 'UALP'; 'DCB3'; 'BALP'; ...
'ADDC'; 'MCT'; 'MCTO'; 'MOH'; 'EOH'; ...
'ROH'; 'ETEG'; 'ISHP'; 'IEPOX'; 'MAHP'; ...
'ORA2'; 'ORAP'; 'PPN'; 'MPAN'; 'TRPN'; ...
'HOM'; 'NALD'; 'ISON'; 'MCTP'; 'OLNN'; ...
'OLND'; 'APINP1'; 'APINP2'; 'LIMNP1'; 'LIMNP2'; ...
'ADCN'; 'VROCP4OXY2'; 'VROCN1OXY6'; 'FURANONE'; 'VROCP3OXY2'; ...
'VROCP0OXY4'; 'BAL1'; 'BAL2'; 'OP3'; 'ELHOM'; ...
'VROCIOXY'; 'SLOWROC'; 'ACRO'; 'BDE13'; 'BDE13P'; ...
'FURAN'; 'FURANO2'; 'PROG'; 'SESQ'; 'SESQNRO2'; ...
'VROCN2OXY2'; 'SESQRO2'; 'VROCP0OXY2'; 'VROCP1OXY3'; 'AGLYJ'; ...
'AISONJ'; 'ATRPNJ'; 'IEPOXP'; 'AISO3NOSJ'; 'ASO4J'; ...
'AISO3OSJ'; 'VROCP6ALK'; 'VROCP6ALKP'; 'VROCP5ALK'; 'VROCP5ALKP'; ...
'VROCP4ALK'; 'VROCP4ALKP'; 'VROCP3ALK'; 'VROCP3ALKP'; 'VROCP2ALK'; ...
'VROCP2ALKP'; 'VROCP1ALK'; 'VROCP1ALKP'; 'HC10'; 'VROCP6ALKP2'; ...
'VROCP5ALKP2'; 'VROCP4ALKP2'; 'VROCP2OXY2'; 'VROCP3ALKP2'; 'VROCP1OXY1'; ...
'VROCP2ALKP2'; 'VROCP1ALKP2'; 'VROCN1OXY1'; 'HC10P2'; 'VROCP6ARO'; ...
'VROCP6AROP'; 'VROCN2OXY4'; 'VROCN1OXY3'; 'VROCP5ARO'; 'VROCP5AROP'; ...
'NAPH'; 'NAPHP'; 'VROCN2OXY8'; 'VROCP5OXY1'; 'VROCP6OXY1'; ...
};


AddSpecies


%   1, <R001>
i=i+1;
Rnames{   1} = 'O3 = O3P ';
k(:,i) = (JO3O3P_NASA06 ); 
Gstr{i,   1}='O3';
fO3(i)=fO3(i)-1.0;
fO3P(i)=fO3P(i)+  1.000;

%   2, <R002>
i=i+1;
Rnames{   2} = 'O3 = O1D ';
k(:,i) = (JO3O1D_NASA06 ); 
Gstr{i,   1}='O3';
fO3(i)=fO3(i)-1.0;
fO1D(i)=fO1D(i)+  1.000;

%   3, <R003>
i=i+1;
Rnames{   3} = 'H2O2 = 2.00000*HO ';
k(:,i) = (JH2O2_RACM2 ); 
Gstr{i,   1}='H2O2';
fH2O2(i)=fH2O2(i)-1.0;
fHO(i)=fHO(i)+  2.000;

%   4, <R004>
i=i+1;
Rnames{   4} = 'NO2 = O3P + NO ';
k(:,i) = (JNO2_RACM2 ); 
Gstr{i,   1}='NO2';
fNO2(i)=fNO2(i)-1.0;
fO3P(i)=fO3P(i)+  1.000;fNO(i)=fNO(i)+  1.000;

%   5, <R005>
i=i+1;
Rnames{   5} = 'NO3 = NO ';
k(:,i) = (JNO3NO_RACM2 ); 
Gstr{i,   1}='NO3';
fNO3(i)=fNO3(i)-1.0;
fNO(i)=fNO(i)+  1.000;

%   6, <R006>
i=i+1;
Rnames{   6} = 'NO3 = O3P + NO2 ';
k(:,i) = (JNO3NO2_RACM2 ); 
Gstr{i,   1}='NO3';
fNO3(i)=fNO3(i)-1.0;
fO3P(i)=fO3P(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

%   7, <R007>
i=i+1;
Rnames{   7} = 'HONO = HO + NO ';
k(:,i) = (JHONO_RACM2 ); 
Gstr{i,   1}='HONO';
fHONO(i)=fHONO(i)-1.0;
fHO(i)=fHO(i)+  1.000;fNO(i)=fNO(i)+  1.000;

%   8, <R008>
i=i+1;
Rnames{   8} = 'HNO3 = HO + NO2 ';
k(:,i) = (JHNO3_RACM2 ); 
Gstr{i,   1}='HNO3';
fHNO3(i)=fHNO3(i)-1.0;
fHO(i)=fHO(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

%   9, <R009>
i=i+1;
Rnames{   9} = 'HNO4 = 0.20000*HO +  0.80000*HO2 +  0.80000*NO2 +  0.20000*NO3 ';
k(:,i) = (JHNO4_RACM2 ); 
Gstr{i,   1}='HNO4';
fHNO4(i)=fHNO4(i)-1.0;
fHO(i)=fHO(i)+  0.200;fHO2(i)=fHO2(i)+  0.800;fNO2(i)=fNO2(i)+  0.800;fNO3(i)=fNO3(i)+  0.200;

%  10, <R010>
i=i+1;
Rnames{  10} = 'HCHO = CO ';
k(:,i) = (JHCHO_MOL_JPL19 ); 
Gstr{i,   1}='HCHO';
fHCHO(i)=fHCHO(i)-1.0;
fCO(i)=fCO(i)+  1.000;

%  11, <R011>
i=i+1;
Rnames{  11} = 'HCHO = 2.00000*HO2 + CO ';
k(:,i) = (JHCHO_RAD_JPL19 ); 
Gstr{i,   1}='HCHO';
fHCHO(i)=fHCHO(i)-1.0;
fHO2(i)=fHO2(i)+  2.000;fCO(i)=fCO(i)+  1.000;

%  12, <R012>
i=i+1;
Rnames{  12} = 'ACD = HO2 + MO2 + CO ';
k(:,i) = (JCH3CHO_RACM2 ); 
Gstr{i,   1}='ACD';
fACD(i)=fACD(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fMO2(i)=fMO2(i)+  1.000;fCO(i)=fCO(i)+  1.000;

%  13, <R013>
i=i+1;
Rnames{  13} = 'ALD = HO2 + ETHP + CO ';
k(:,i) = (JALD_JPL19 ); 
Gstr{i,   1}='ALD';
fALD(i)=fALD(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fETHP(i)=fETHP(i)+  1.000;fCO(i)=fCO(i)+  1.000;

%  14, <R014>
i=i+1;
Rnames{  14} = 'ACT = MO2 + ACO3 ';
k(:,i) = (JCH3COCH3A_JPL19 ); 
Gstr{i,   1}='ACT';
fACT(i)=fACT(i)-1.0;
fMO2(i)=fMO2(i)+  1.000;fACO3(i)=fACO3(i)+  1.000;

%  15, <R014a>
i=i+1;
Rnames{  15} = 'ACT = 2.00000*MO2 + CO ';
k(:,i) = (JCH3COCH3B_JPL19 ); 
Gstr{i,   1}='ACT';
fACT(i)=fACT(i)-1.0;
fMO2(i)=fMO2(i)+  2.000;fCO(i)=fCO(i)+  1.000;

%  16, <R015>
i=i+1;
Rnames{  16} = 'UALD = 1.22000*HO2 +  0.78400*ACO3 +  1.22000*CO +  0.35000*HCHO +  0.43400*ALD +  0.21600*KET ';
k(:,i) = (JUALD_RACM2 ); 
Gstr{i,   1}='UALD';
fUALD(i)=fUALD(i)-1.0;
fHO2(i)=fHO2(i)+  1.220;fACO3(i)=fACO3(i)+  0.784;fCO(i)=fCO(i)+  1.220;fHCHO(i)=fHCHO(i)+  0.350;fALD(i)=fALD(i)+  0.434;fKET(i)=fKET(i)+  0.216;

%  17, <TRP01>
i=i+1;
Rnames{  17} = 'PINAL = HO2 + HC10P + CO ';
k(:,i) = (JALD_JPL19 ); 
Gstr{i,   1}='PINAL';
fPINAL(i)=fPINAL(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fHC10P(i)=fHC10P(i)+  1.000;fCO(i)=fCO(i)+  1.000;

%  18, <TRP02>
i=i+1;
Rnames{  18} = 'LIMAL = HO2 + HC10P + CO ';
k(:,i) = (JALD_JPL19 ); 
Gstr{i,   1}='LIMAL';
fLIMAL(i)=fLIMAL(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fHC10P(i)=fHC10P(i)+  1.000;fCO(i)=fCO(i)+  1.000;

%  19, <R016>
i=i+1;
Rnames{  19} = 'MEK = 0.10000*MO2 + ETHP +  0.90000*ACO3 +  0.10000*CO ';
k(:,i) = (JMEK_JGR19 ); 
Gstr{i,   1}='MEK';
fMEK(i)=fMEK(i)-1.0;
fMO2(i)=fMO2(i)+  0.100;fETHP(i)=fETHP(i)+  1.000;fACO3(i)=fACO3(i)+  0.900;fCO(i)=fCO(i)+  0.100;

%  20, <R017>
i=i+1;
Rnames{  20} = 'KET = 1.50000*ETHP +  0.50000*ACO3 +  0.50000*CO ';
k(:,i) = (JKET_JGR19 ); 
Gstr{i,   1}='KET';
fKET(i)=fKET(i)-1.0;
fETHP(i)=fETHP(i)+  1.500;fACO3(i)=fACO3(i)+  0.500;fCO(i)=fCO(i)+  0.500;

%  21, <R018>
i=i+1;
Rnames{  21} = 'HKET = HO2 + ACO3 + HCHO ';
k(:,i) = (JHKET_RACM2 ); 
Gstr{i,   1}='HKET';
fHKET(i)=fHKET(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fACO3(i)=fACO3(i)+  1.000;fHCHO(i)=fHCHO(i)+  1.000;

%  22, <R019>
i=i+1;
Rnames{  22} = 'MACR = 0.34000*HO +  0.66000*HO2 +  0.67000*ACO3 +  0.33000*MACP +  0.34000*XO2 +  0.67000*CO +  0.67000*HCHO ';
k(:,i) = (JMACR_RACM2 ); 
Gstr{i,   1}='MACR';
fMACR(i)=fMACR(i)-1.0;
fHO(i)=fHO(i)+  0.340;fHO2(i)=fHO2(i)+  0.660;fACO3(i)=fACO3(i)+  0.670;fMACP(i)=fMACP(i)+  0.330;fXO2(i)=fXO2(i)+  0.340;fCO(i)=fCO(i)+  0.670;fHCHO(i)=fHCHO(i)+  0.670;

%  23, <R020>
i=i+1;
Rnames{  23} = 'MVK = 0.30000*MO2 +  0.30000*MACP +  0.70000*CO +  0.70000*UALD ';
k(:,i) = (JMVK_RACM2 ); 
Gstr{i,   1}='MVK';
fMVK(i)=fMVK(i)-1.0;
fMO2(i)=fMO2(i)+  0.300;fMACP(i)=fMACP(i)+  0.300;fCO(i)=fCO(i)+  0.700;fUALD(i)=fUALD(i)+  0.700;

%  24, <R021>
i=i+1;
Rnames{  24} = 'GLY = 2.00000*CO ';
k(:,i) = (JGLYH2_RACM2 ); 
Gstr{i,   1}='GLY';
fGLY(i)=fGLY(i)-1.0;
fCO(i)=fCO(i)+  2.000;

%  25, <R022>
i=i+1;
Rnames{  25} = 'GLY = HCHO + CO ';
k(:,i) = (JGLYF_RACM2 ); 
Gstr{i,   1}='GLY';
fGLY(i)=fGLY(i)-1.0;
fHCHO(i)=fHCHO(i)+  1.000;fCO(i)=fCO(i)+  1.000;

%  26, <R023>
i=i+1;
Rnames{  26} = 'GLY = 2.00000*HO2 +  2.00000*CO ';
k(:,i) = (JGLYHX_RACM2 ); 
Gstr{i,   1}='GLY';
fGLY(i)=fGLY(i)-1.0;
fHO2(i)=fHO2(i)+  2.000;fCO(i)=fCO(i)+  2.000;

%  27, <R024>
i=i+1;
Rnames{  27} = 'MGLY = HO2 + ACO3 + CO ';
k(:,i) = (JMGLY_RACM2 ); 
Gstr{i,   1}='MGLY';
fMGLY(i)=fMGLY(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fACO3(i)=fACO3(i)+  1.000;fCO(i)=fCO(i)+  1.000;

%  28, <R025>
i=i+1;
Rnames{  28} = 'DCB1 = 1.50000*HO2 +  0.25000*ACO3 +  0.20000*XO2 + CO +  0.50000*GLY +  0.50000*MGLY ';
k(:,i) = (JMGLY_RACM2 ); 
Gstr{i,   1}='DCB1';
fDCB1(i)=fDCB1(i)-1.0;
fHO2(i)=fHO2(i)+  1.500;fACO3(i)=fACO3(i)+  0.250;fXO2(i)=fXO2(i)+  0.200;fCO(i)=fCO(i)+  1.000;fGLY(i)=fGLY(i)+  0.500;fMGLY(i)=fMGLY(i)+  0.500;

%  29, <R026>
i=i+1;
Rnames{  29} = 'DCB2 = 1.50000*HO2 +  0.25000*ACO3 +  0.20000*XO2 + CO +  0.50000*GLY +  0.50000*MGLY ';
k(:,i) = (JMGLY_RACM2 ); 
Gstr{i,   1}='DCB2';
fDCB2(i)=fDCB2(i)-1.0;
fHO2(i)=fHO2(i)+  1.500;fACO3(i)=fACO3(i)+  0.250;fXO2(i)=fXO2(i)+  0.200;fCO(i)=fCO(i)+  1.000;fGLY(i)=fGLY(i)+  0.500;fMGLY(i)=fMGLY(i)+  0.500;

%  30, <R027>
i=i+1;
Rnames{  30} = 'BALD = CHO + HO2 + CO ';
k(:,i) = (JBALD_RACM2 ); 
Gstr{i,   1}='BALD';
fBALD(i)=fBALD(i)-1.0;
fCHO(i)=fCHO(i)+  1.000;fHO2(i)=fHO2(i)+  1.000;fCO(i)=fCO(i)+  1.000;

%  31, <R028>
i=i+1;
Rnames{  31} = 'OP1 = HO + HO2 + HCHO ';
k(:,i) = (JOP1_RACM2 ); 
Gstr{i,   1}='OP1';
fOP1(i)=fOP1(i)-1.0;
fHO(i)=fHO(i)+  1.000;fHO2(i)=fHO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  1.000;

%  32, <R029>
i=i+1;
Rnames{  32} = 'OP2 = HO + HO2 + ALD ';
k(:,i) = (JOP1_RACM2 ); 
Gstr{i,   1}='OP2';
fOP2(i)=fOP2(i)-1.0;
fHO(i)=fHO(i)+  1.000;fHO2(i)=fHO2(i)+  1.000;fALD(i)=fALD(i)+  1.000;

%  33, <TRP03>
i=i+1;
Rnames{  33} = 'OPB = HO + HO2 + ALD ';
k(:,i) = (JOP1_RACM2 ); 
Gstr{i,   1}='OPB';
fOPB(i)=fOPB(i)-1.0;
fHO(i)=fHO(i)+  1.000;fHO2(i)=fHO2(i)+  1.000;fALD(i)=fALD(i)+  1.000;

%  34, <R030>
i=i+1;
Rnames{  34} = 'PAA = HO + MO2 ';
k(:,i) = (JPAA_RACM2 ); 
Gstr{i,   1}='PAA';
fPAA(i)=fPAA(i)-1.0;
fHO(i)=fHO(i)+  1.000;fMO2(i)=fMO2(i)+  1.000;

%  35, <R031>
i=i+1;
Rnames{  35} = 'ONIT = HO2 + NO2 +  0.20000*ALD +  0.80000*KET ';
k(:,i) = (JONIT_RACM2 ); 
Gstr{i,   1}='ONIT';
fONIT(i)=fONIT(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;fALD(i)=fALD(i)+  0.200;fKET(i)=fKET(i)+  0.800;

%  36, <R032>
i=i+1;
Rnames{  36} = 'PAN = ACO3 + NO2 ';
k(:,i) = (JPAN1_RACM2 ); 
Gstr{i,   1}='PAN';
fPAN(i)=fPAN(i)-1.0;
fACO3(i)=fACO3(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

%  37, <R033>
i=i+1;
Rnames{  37} = 'PAN = MO2 + NO3 ';
k(:,i) = (JPAN2_RACM2 ); 
Gstr{i,   1}='PAN';
fPAN(i)=fPAN(i)-1.0;
fMO2(i)=fMO2(i)+  1.000;fNO3(i)=fNO3(i)+  1.000;

%  38, <R034>
i=i+1;
Rnames{  38} = 'O3 + HO = HO2 ';
k(:,i) = (  1.7000E-12.*exp( -9.4000E+02./T) ); 
Gstr{i,   1}='O3';Gstr{i,   2}='HO';
fO3(i)=fO3(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;

%  39, <R035>
i=i+1;
Rnames{  39} = 'O3 + HO2 = HO ';
k(:,i) = (  1.0000E-14.*exp( -4.9000E+02./T) ); 
Gstr{i,   1}='O3';Gstr{i,   2}='HO2';
fO3(i)=fO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO(i)=fHO(i)+  1.000;

%  40, <R036>
i=i+1;
Rnames{  40} = 'O3 + NO = NO2 ';
k(:,i) = (  3.0000E-12.*exp( -1.5000E+03./T) ); 
Gstr{i,   1}='O3';Gstr{i,   2}='NO';
fO3(i)=fO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;

%  41, <R037>
i=i+1;
Rnames{  41} = 'O3 + NO2 = NO3 ';
k(:,i) = (  1.2000E-13.*exp( -2.4500E+03./T) ); 
Gstr{i,   1}='O3';Gstr{i,   2}='NO2';
fO3(i)=fO3(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fNO3(i)=fNO3(i)+  1.000;

%  42, <R038>
i=i+1;
Rnames{  42} = 'O3P + O2 + M = O3 ';
k(:,i) = (  6.1000E-34.*(T./300).^( -2.4000E+00) ).*O2.*M; 
Gstr{i,   1}='O3P';
fO3P(i)=fO3P(i)-1.0;
fO3(i)=fO3(i)+  1.000;

%  43, <R039>
i=i+1;
Rnames{  43} = 'O3P + O3 =';
k(:,i) = (  8.0000E-12.*exp( -2.0600E+03./T) ); 
Gstr{i,   1}='O3P';Gstr{i,   2}='O3';
fO3P(i)=fO3P(i)-1.0;fO3(i)=fO3(i)-1.0;


%  44, <R040>
i=i+1;
Rnames{  44} = 'O1D + O2 = O3P ';
k(:,i) = (  3.3000E-11.*exp(  5.5000E+01./T) ).*O2; 
Gstr{i,   1}='O1D';
fO1D(i)=fO1D(i)-1.0;
fO3P(i)=fO3P(i)+  1.000;

%  45, <R041>
i=i+1;
Rnames{  45} = 'O1D + N2 = O3P ';
k(:,i) = (  2.1500E-11.*exp(  1.1000E+02./T) ).*N2; 
Gstr{i,   1}='O1D';
fO1D(i)=fO1D(i)-1.0;
fO3P(i)=fO3P(i)+  1.000;

%  46, <R042>
i=i+1;
Rnames{  46} = 'O1D + H2O = 2.00000*HO ';
k(:,i) = (  1.6300E-10.*exp(  6.0000E+01./T) ).*H2O; 
Gstr{i,   1}='O1D';
fO1D(i)=fO1D(i)-1.0;
fHO(i)=fHO(i)+  2.000;

%  47, <R043>
i=i+1;
Rnames{  47} = 'HO + H2 = HO2 ';
k(:,i) = (  2.8000E-12.*exp( -1.8000E+03./T) ).*H2; 
Gstr{i,   1}='HO';
fHO(i)=fHO(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;

%  48, <R044>
i=i+1;
Rnames{  48} = 'HO + HO2 =';
k(:,i) = (  4.8000E-11.*exp(  2.5000E+02./T) ); 
Gstr{i,   1}='HO';Gstr{i,   2}='HO2';
fHO(i)=fHO(i)-1.0;fHO2(i)=fHO2(i)-1.0;


%  49, <R045>
i=i+1;
Rnames{  49} = 'HO2 + HO2 = H2O2 ';
xk0 =   3.0000E-13.*exp(  4.6000E+02./T);
xk1 =   2.1000E-33.*exp(  9.2000E+02./T);
k(:,i) = (xk0+xk1.*M ); 
Gstr{i,   1}='HO2';Gstr{i,   2}='HO2';
fHO2(i)=fHO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fH2O2(i)=fH2O2(i)+  1.000;

%  50, <R046>
i=i+1;
Rnames{  50} = 'HO2 + HO2 + H2O = H2O2 ';
xk0 =   4.2000E-34.*exp(  2.6600E+03./T);
xk1 =   2.9400E-54.*exp(  3.1200E+03./T);
k(:,i) = (xk0+xk1.*M ).*H2O; 
Gstr{i,   1}='HO2';Gstr{i,   2}='HO2';
fHO2(i)=fHO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fH2O2(i)=fH2O2(i)+  1.000;

%  51, <R047>
i=i+1;
Rnames{  51} = 'H2O2 + HO = HO2 ';
k(:,i) = (  1.8000E-12.*exp(  0.0000E+00./T) ); 
Gstr{i,   1}='H2O2';Gstr{i,   2}='HO';
fH2O2(i)=fH2O2(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;

%  52, <R048>
i=i+1;
Rnames{  52} = 'NO + O3P = NO2 ';
xko =   9.1000E-32.*M.*exp(  0.0000E+00./T).*(T./300).^ -1.5000E+00;
xkinf =   3.0000E-11.*exp(  0.0000E+00./T).*(T./300).^  0.0000E+00;
xn =   1.0000E+00;
F =   6.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='NO';Gstr{i,   2}='O3P';
fNO(i)=fNO(i)-1.0;fO3P(i)=fO3P(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;

%  53, <R049>
i=i+1;
Rnames{  53} = 'NO + HO = HONO ';
xko =   7.1000E-31.*M.*exp(  0.0000E+00./T).*(T./300).^ -2.6000E+00;
xkinf =   3.6000E-11.*exp(  0.0000E+00./T).*(T./300).^ -1.0000E-01;
xn =   1.0000E+00;
F =   6.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='NO';Gstr{i,   2}='HO';
fNO(i)=fNO(i)-1.0;fHO(i)=fHO(i)-1.0;
fHONO(i)=fHONO(i)+  1.000;

%  54, <R050>
i=i+1;
Rnames{  54} = 'NO + HO2 = NO2 + HO ';
k(:,i) = (  3.4400E-12.*exp(  2.6000E+02./T) ); 
Gstr{i,   1}='NO';Gstr{i,   2}='HO2';
fNO(i)=fNO(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fHO(i)=fHO(i)+  1.000;

%  55, <R051>
i=i+1;
Rnames{  55} = 'NO + HO2 = HNO3 ';
xk0 =   6.0950E-14.*exp(  2.7000E+02./T).*(T./300).^ -1.0000E+00;
xk1 =   6.8570E-34.*exp(  2.7000E+02./T).*(T./300).^  1.0000E+00;
xk2 =  -5.9680E-14.*exp(  2.7000E+02./T);
k(:,i) = (xk0+xk1.*M+xk2 ); 
Gstr{i,   1}='NO';Gstr{i,   2}='HO2';
fNO(i)=fNO(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.000;

%  56, <R052>
i=i+1;
Rnames{  56} = 'NO + NO + O2 = 2.00000*NO2 ';
k(:,i) = (  4.2500E-39.*exp(  6.6350E+02./T) ).*O2; 
Gstr{i,   1}='NO';Gstr{i,   2}='NO';
fNO(i)=fNO(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  2.000;

%  57, <R053>
i=i+1;
Rnames{  57} = 'HONO + HO = NO2 ';
k(:,i) = (  3.0000E-12.*exp(  2.5000E+02./T) ); 
Gstr{i,   1}='HONO';Gstr{i,   2}='HO';
fHONO(i)=fHONO(i)-1.0;fHO(i)=fHO(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;

%  58, <R054>
i=i+1;
Rnames{  58} = 'NO2 + O3P = NO ';
k(:,i) = (  5.3000E-12.*exp(  2.0000E+02./T) ); 
Gstr{i,   1}='NO2';Gstr{i,   2}='O3P';
fNO2(i)=fNO2(i)-1.0;fO3P(i)=fO3P(i)-1.0;
fNO(i)=fNO(i)+  1.000;

%  59, <R055>
i=i+1;
Rnames{  59} = 'NO2 + O3P = NO3 ';
xko =   3.4000E-31.*M.*exp(  0.0000E+00./T).*(T./300).^ -1.6000E+00;
xkinf =   2.3000E-11.*exp(  0.0000E+00./T).*(T./300).^ -2.0000E-01;
xn =   1.0000E+00;
F =   6.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='NO2';Gstr{i,   2}='O3P';
fNO2(i)=fNO2(i)-1.0;fO3P(i)=fO3P(i)-1.0;
fNO3(i)=fNO3(i)+  1.000;

%  60, <R056>
i=i+1;
Rnames{  60} = 'NO2 + HO = HNO3 ';
xko =   1.8000E-30.*M.*exp(  0.0000E+00./T).*(T./300).^ -3.0000E+00;
xkinf =   2.8000E-11.*exp(  0.0000E+00./T).*(T./300).^  0.0000E+00;
xn =   1.0000E+00;
F =   6.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='NO2';Gstr{i,   2}='HO';
fNO2(i)=fNO2(i)-1.0;fHO(i)=fHO(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.000;

%  61, <R057>
i=i+1;
Rnames{  61} = 'HNO3 + HO = NO3 ';
 xk0 =   2.4000E-14.*exp(  4.6000E+02./T);
 xk2 =   2.7000E-17.*exp(  2.1990E+03./T);
 xk3 =   6.5000E-34.*exp(  1.3350E+03./T);
k(:,i) = (xk0+xk3.*M./(1.0+xk3.*M./xk2) ); 
Gstr{i,   1}='HNO3';Gstr{i,   2}='HO';
fHNO3(i)=fHNO3(i)-1.0;fHO(i)=fHO(i)-1.0;
fNO3(i)=fNO3(i)+  1.000;

%  62, <R058>
i=i+1;
Rnames{  62} = 'NO3 + HO = HO2 + NO2 ';
k(:,i) = (  2.0000E-11 ); 
Gstr{i,   1}='NO3';Gstr{i,   2}='HO';
fNO3(i)=fNO3(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

%  63, <R059>
i=i+1;
Rnames{  63} = 'NO3 + HO2 = 0.70000*HO +  0.70000*NO2 +  0.30000*HNO3 ';
k(:,i) = (  3.5000E-12 ); 
Gstr{i,   1}='NO3';Gstr{i,   2}='HO2';
fNO3(i)=fNO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO(i)=fHO(i)+  0.700;fNO2(i)=fNO2(i)+  0.700;fHNO3(i)=fHNO3(i)+  0.300;

%  64, <R060>
i=i+1;
Rnames{  64} = 'NO3 + NO = 2.00000*NO2 ';
k(:,i) = (  1.7000E-11.*exp(  1.2500E+02./T) ); 
Gstr{i,   1}='NO3';Gstr{i,   2}='NO';
fNO3(i)=fNO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  2.000;

%  65, <R061>
i=i+1;
Rnames{  65} = 'NO3 + NO2 = NO + NO2 ';
k(:,i) = (  4.3500E-14.*exp( -1.3350E+03./T) ); 
Gstr{i,   1}='NO3';Gstr{i,   2}='NO2';
fNO3(i)=fNO3(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fNO(i)=fNO(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

%  66, <R062>
i=i+1;
Rnames{  66} = 'NO3 + NO3 = 2.00000*NO2 ';
k(:,i) = (  8.5000E-13.*exp( -2.4500E+03./T) ); 
Gstr{i,   1}='NO3';Gstr{i,   2}='NO3';
fNO3(i)=fNO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  2.000;

%  67, <R063>
i=i+1;
Rnames{  67} = 'NO3 + NO2 = N2O5 ';
xko =   2.4000E-30.*M.*exp(  0.0000E+00./T).*(T./300).^ -3.0000E+00;
xkinf =   1.6000E-12.*exp(  0.0000E+00./T).*(T./300).^  1.0000E-01;
xn =   1.0000E+00;
F =   6.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='NO3';Gstr{i,   2}='NO2';
fNO3(i)=fNO3(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fN2O5(i)=fN2O5(i)+  1.000;

%  68, <R064>
i=i+1;
Rnames{  68} = 'N2O5 = NO2 + NO3 ';
k(:,i) = (  1.7241E+26.*exp( -1.0840E+04./T).*k(:,  67) ); 
Gstr{i,   1}='N2O5';
fN2O5(i)=fN2O5(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fNO3(i)=fNO3(i)+  1.000;

%  69, <R065>
i=i+1;
Rnames{  69} = 'N2O5 + H2O = 2.00000*HNO3 ';
k(:,i) = (  1.0000E-22 ).*H2O; 
Gstr{i,   1}='N2O5';
fN2O5(i)=fN2O5(i)-1.0;
fHNO3(i)=fHNO3(i)+  2.000;

%  70, <R066>
i=i+1;
Rnames{  70} = 'NO2 + HO2 = HNO4 ';
xko =   1.9000E-31.*M.*exp(  0.0000E+00./T).*(T./300).^ -3.4000E+00;
xkinf =   4.0000E-12.*exp(  0.0000E+00./T).*(T./300).^ -3.0000E-01;
xn =   1.0000E+00;
F =   6.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='NO2';Gstr{i,   2}='HO2';
fNO2(i)=fNO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHNO4(i)=fHNO4(i)+  1.000;

%  71, <R067>
i=i+1;
Rnames{  71} = 'HNO4 = HO2 + NO2 ';
k(:,i) = (  4.7619E+26.*exp( -1.0900E+04./T).*k(:,  70) ); 
Gstr{i,   1}='HNO4';
fHNO4(i)=fHNO4(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

%  72, <R068>
i=i+1;
Rnames{  72} = 'HNO4 + HO = NO2 ';
k(:,i) = (  4.5000E-13.*exp(  6.1000E+02./T) ); 
Gstr{i,   1}='HNO4';Gstr{i,   2}='HO';
fHNO4(i)=fHNO4(i)-1.0;fHO(i)=fHO(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;

%  73, <R069>
i=i+1;
Rnames{  73} = 'SO2 + HO = HO2 + SULF + SULRXN ';
xko =   2.9000E-31.*M.*exp(  0.0000E+00./T).*(T./300).^ -4.1000E+00;
xkinf =   1.7000E-12.*exp(  0.0000E+00./T).*(T./300).^  2.0000E-01;
xn =   1.0000E+00;
F =   6.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='SO2';Gstr{i,   2}='HO';
fSO2(i)=fSO2(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fSULF(i)=fSULF(i)+  1.000;fSULRXN(i)=fSULRXN(i)+  1.000;

%  74, <R070>
i=i+1;
Rnames{  74} = 'CO + HO = HO2 ';
xk0 =   1.4400E-13.*exp(  0.0000E+00./T);
xk1 =   2.7400E-33.*exp(  0.0000E+00./T);
k(:,i) = (xk0+xk1.*M ); 
Gstr{i,   1}='CO';Gstr{i,   2}='HO';
fCO(i)=fCO(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;

%  75, <R071>
i=i+1;
Rnames{  75} = 'HO + CH4 = MO2 ';
k(:,i) = (  2.4500E-12.*exp( -1.7750E+03./T) ).*CH4; 
Gstr{i,   1}='HO';
fHO(i)=fHO(i)-1.0;
fMO2(i)=fMO2(i)+  1.000;

%  76, <R072>
i=i+1;
Rnames{  76} = 'ETH + HO = ETHP ';
k(:,i) = (  7.6600E-12.*exp( -1.0200E+03./T) ); 
Gstr{i,   1}='ETH';Gstr{i,   2}='HO';
fETH(i)=fETH(i)-1.0;fHO(i)=fHO(i)-1.0;
fETHP(i)=fETHP(i)+  1.000;

%  77, <R073>
i=i+1;
Rnames{  77} = 'HC3 + HO = HC3P +  0.00003*ASOATJ ';
k(:,i) = (  7.6800E-12.*exp( -3.7000E+02./T) ); 
Gstr{i,   1}='HC3';Gstr{i,   2}='HO';
fHC3(i)=fHC3(i)-1.0;fHO(i)=fHO(i)-1.0;
fHC3P(i)=fHC3P(i)+  1.000;fASOATJ(i)=fASOATJ(i)+  0.000;

%  78, <R074>
i=i+1;
Rnames{  78} = 'HC5 + HO = HC5P +  0.00158*ASOATJ ';
k(:,i) = (  1.0100E-11.*exp( -2.4500E+02./T) ); 
Gstr{i,   1}='HC5';Gstr{i,   2}='HO';
fHC5(i)=fHC5(i)-1.0;fHO(i)=fHO(i)-1.0;
fHC5P(i)=fHC5P(i)+  1.000;fASOATJ(i)=fASOATJ(i)+  0.002;

%  79, <R076>
i=i+1;
Rnames{  79} = 'ETE + HO = ETEP ';
xko =   1.0000E-28.*M.*exp(  0.0000E+00./T).*(T./300).^ -4.5000E+00;
xkinf =   8.8000E-12.*exp(  0.0000E+00./T).*(T./300).^ -8.5000E-01;
xn =   1.0000E+00;
F =   6.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='ETE';Gstr{i,   2}='HO';
fETE(i)=fETE(i)-1.0;fHO(i)=fHO(i)-1.0;
fETEP(i)=fETEP(i)+  1.000;

%  80, <R077>
i=i+1;
Rnames{  80} = 'OLT + HO = OLTP ';
k(:,i) = (  5.7200E-12.*exp(  5.0000E+02./T) ); 
Gstr{i,   1}='OLT';Gstr{i,   2}='HO';
fOLT(i)=fOLT(i)-1.0;fHO(i)=fHO(i)-1.0;
fOLTP(i)=fOLTP(i)+  1.000;

%  81, <R078>
i=i+1;
Rnames{  81} = 'OLI + HO = OLIP ';
k(:,i) = (  1.3300E-11.*exp(  5.0000E+02./T) ); 
Gstr{i,   1}='OLI';Gstr{i,   2}='HO';
fOLI(i)=fOLI(i)-1.0;fHO(i)=fHO(i)-1.0;
fOLIP(i)=fOLIP(i)+  1.000;

%  82, <R080>
i=i+1;
Rnames{  82} = 'ACE + HO = 0.65000*HO +  0.35000*HO2 +  0.35000*CO +  0.65000*GLY +  0.35000*ORA1 ';
xko =   5.5000E-30.*M.*exp(  0.0000E+00./T).*(T./300).^  0.0000E+00;
xkinf =   8.3000E-13.*exp(  0.0000E+00./T).*(T./300).^  2.0000E+00;
xn =   1.0000E+00;
F =   6.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='ACE';Gstr{i,   2}='HO';
fACE(i)=fACE(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  0.650;fHO2(i)=fHO2(i)+  0.350;fCO(i)=fCO(i)+  0.350;fGLY(i)=fGLY(i)+  0.650;fORA1(i)=fORA1(i)+  0.350;

%  83, <ROCARO31>
i=i+1;
Rnames{  83} = 'BEN + HO = 0.47000*BENP +  0.53000*PHEN +  0.53000*HO2 ';
k(:,i) = (  2.3300E-12.*exp( -1.9300E+02./T) ); 
Gstr{i,   1}='BEN';Gstr{i,   2}='HO';
fBEN(i)=fBEN(i)-1.0;fHO(i)=fHO(i)-1.0;
fBENP(i)=fBENP(i)+  0.470;fPHEN(i)=fPHEN(i)+  0.530;fHO2(i)=fHO2(i)+  0.530;

%  84, <ROCARO41>
i=i+1;
Rnames{  84} = 'TOL + HO = 0.82000*TOLP +  0.18000*CSL +  0.18000*HO2 ';
k(:,i) = (  1.8100E-12.*exp(  3.5400E+02./T) ); 
Gstr{i,   1}='TOL';Gstr{i,   2}='HO';
fTOL(i)=fTOL(i)-1.0;fHO(i)=fHO(i)-1.0;
fTOLP(i)=fTOLP(i)+  0.820;fCSL(i)=fCSL(i)+  0.180;fHO2(i)=fHO2(i)+  0.180;

%  85, <ROCARO51>
i=i+1;
Rnames{  85} = 'XYM + HO = 0.83000*XYMP +  0.17000*CSL +  0.17000*HO2 ';
k(:,i) = (  2.3300E-11 ); 
Gstr{i,   1}='XYM';Gstr{i,   2}='HO';
fXYM(i)=fXYM(i)-1.0;fHO(i)=fHO(i)-1.0;
fXYMP(i)=fXYMP(i)+  0.830;fCSL(i)=fCSL(i)+  0.170;fHO2(i)=fHO2(i)+  0.170;

%  86, <ROCARO61>
i=i+1;
Rnames{  86} = 'XYE + HO = 0.82000*XYEP +  0.18000*CSL +  0.18000*HO2 ';
k(:,i) = (  7.1600E-12 ); 
Gstr{i,   1}='XYE';Gstr{i,   2}='HO';
fXYE(i)=fXYE(i)-1.0;fHO(i)=fHO(i)-1.0;
fXYEP(i)=fXYEP(i)+  0.820;fCSL(i)=fCSL(i)+  0.180;fHO2(i)=fHO2(i)+  0.180;

%  87, <R086>
i=i+1;
Rnames{  87} = 'ISO + HO = ISOP ';
k(:,i) = (  2.7000E-11.*exp(  3.9000E+02./T) ); 
Gstr{i,   1}='ISO';Gstr{i,   2}='HO';
fISO(i)=fISO(i)-1.0;fHO(i)=fHO(i)-1.0;
fISOP(i)=fISOP(i)+  1.000;

%  88, <R087>
i=i+1;
Rnames{  88} = 'API + HO = 0.97500*APIP1 +  0.02500*APIP2 ';
k(:,i) = (  1.2100E-11.*exp(  4.4000E+02./T) ); 
Gstr{i,   1}='API';Gstr{i,   2}='HO';
fAPI(i)=fAPI(i)-1.0;fHO(i)=fHO(i)-1.0;
fAPIP1(i)=fAPIP1(i)+  0.975;fAPIP2(i)=fAPIP2(i)+  0.025;

%  89, <R088>
i=i+1;
Rnames{  89} = 'LIM + HO = 0.94500*LIMP1 +  0.05500*LIMP2 ';
k(:,i) = (  4.2000E-11.*exp(  4.0100E+02./T) ); 
Gstr{i,   1}='LIM';Gstr{i,   2}='HO';
fLIM(i)=fLIM(i)-1.0;fHO(i)=fHO(i)-1.0;
fLIMP1(i)=fLIMP1(i)+  0.945;fLIMP2(i)=fLIMP2(i)+  0.055;

%  90, <TRP04>
i=i+1;
Rnames{  90} = 'PINAL + HO = 0.23000*PINALP +  0.77000*RCO3 ';
k(:,i) = (  5.2000E-12.*exp(  6.0000E+02./T) ); 
Gstr{i,   1}='PINAL';Gstr{i,   2}='HO';
fPINAL(i)=fPINAL(i)-1.0;fHO(i)=fHO(i)-1.0;
fPINALP(i)=fPINALP(i)+  0.230;fRCO3(i)=fRCO3(i)+  0.770;

%  91, <TRP05>
i=i+1;
Rnames{  91} = 'LIMAL + HO = 0.70000*LIMALP +  0.30000*RCO3 ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='LIMAL';Gstr{i,   2}='HO';
fLIMAL(i)=fLIMAL(i)-1.0;fHO(i)=fHO(i)-1.0;
fLIMALP(i)=fLIMALP(i)+  0.700;fRCO3(i)=fRCO3(i)+  0.300;

%  92, <R089>
i=i+1;
Rnames{  92} = 'HCHO + HO = HO2 + CO ';
k(:,i) = (  5.5000E-12.*exp(  1.2500E+02./T) ); 
Gstr{i,   1}='HCHO';Gstr{i,   2}='HO';
fHCHO(i)=fHCHO(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fCO(i)=fCO(i)+  1.000;

%  93, <R090>
i=i+1;
Rnames{  93} = 'ACD + HO = ACO3 ';
k(:,i) = (  4.7000E-12.*exp(  3.4500E+02./T) ); 
Gstr{i,   1}='ACD';Gstr{i,   2}='HO';
fACD(i)=fACD(i)-1.0;fHO(i)=fHO(i)-1.0;
fACO3(i)=fACO3(i)+  1.000;

%  94, <R091>
i=i+1;
Rnames{  94} = 'ALD + HO = RCO3 ';
k(:,i) = (  4.9000E-12.*exp(  4.0500E+02./T) ); 
Gstr{i,   1}='ALD';Gstr{i,   2}='HO';
fALD(i)=fALD(i)-1.0;fHO(i)=fHO(i)-1.0;
fRCO3(i)=fRCO3(i)+  1.000;

%  95, <R092>
i=i+1;
Rnames{  95} = 'ACT + HO = ACTP ';
k(:,i) = (  4.5600E-14.*exp( -4.2700E+02./T).*(T./300).^(  3.6500E+00 ) ); 
Gstr{i,   1}='ACT';Gstr{i,   2}='HO';
fACT(i)=fACT(i)-1.0;fHO(i)=fHO(i)-1.0;
fACTP(i)=fACTP(i)+  1.000;

%  96, <R093>
i=i+1;
Rnames{  96} = 'MEK + HO = MEKP ';
k(:,i) = (  1.5000E-12.*exp( -9.0000E+01./T) ); 
Gstr{i,   1}='MEK';Gstr{i,   2}='HO';
fMEK(i)=fMEK(i)-1.0;fHO(i)=fHO(i)-1.0;
fMEKP(i)=fMEKP(i)+  1.000;

%  97, <R094>
i=i+1;
Rnames{  97} = 'KET + HO = KETP ';
k(:,i) = (  2.8000E-12.*exp(  1.0000E+01./T) ); 
Gstr{i,   1}='KET';Gstr{i,   2}='HO';
fKET(i)=fKET(i)-1.0;fHO(i)=fHO(i)-1.0;
fKETP(i)=fKETP(i)+  1.000;

%  98, <R095>
i=i+1;
Rnames{  98} = 'HKET + HO = HO2 + MGLY ';
k(:,i) = (  3.0000E-12 ); 
Gstr{i,   1}='HKET';Gstr{i,   2}='HO';
fHKET(i)=fHKET(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fMGLY(i)=fMGLY(i)+  1.000;

%  99, <R096>
i=i+1;
Rnames{  99} = 'MACR + HO = 0.57000*MACP +  0.43000*MCP ';
k(:,i) = (  8.0000E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='MACR';Gstr{i,   2}='HO';
fMACR(i)=fMACR(i)-1.0;fHO(i)=fHO(i)-1.0;
fMACP(i)=fMACP(i)+  0.570;fMCP(i)=fMCP(i)+  0.430;

% 100, <R097>
i=i+1;
Rnames{ 100} = 'MVK + HO = MVKP ';
k(:,i) = (  2.6000E-12.*exp(  6.1000E+02./T) ); 
Gstr{i,   1}='MVK';Gstr{i,   2}='HO';
fMVK(i)=fMVK(i)-1.0;fHO(i)=fHO(i)-1.0;
fMVKP(i)=fMVKP(i)+  1.000;

% 101, <R098>
i=i+1;
Rnames{ 101} = 'UALD + HO = 0.31300*ACO3 +  0.68700*UALP ';
k(:,i) = (  5.7700E-12.*exp(  5.3300E+02./T) ); 
Gstr{i,   1}='UALD';Gstr{i,   2}='HO';
fUALD(i)=fUALD(i)-1.0;fHO(i)=fHO(i)-1.0;
fACO3(i)=fACO3(i)+  0.313;fUALP(i)=fUALP(i)+  0.687;

% 102, <R099>
i=i+1;
Rnames{ 102} = 'GLY + HO = HO2 +  2.00000*CO ';
k(:,i) = (  1.1000E-11 ); 
Gstr{i,   1}='GLY';Gstr{i,   2}='HO';
fGLY(i)=fGLY(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fCO(i)=fCO(i)+  2.000;

% 103, <R100>
i=i+1;
Rnames{ 103} = 'MGLY + HO = ACO3 + CO ';
k(:,i) = (  9.2600E-13.*exp(  8.3000E+02./T) ); 
Gstr{i,   1}='MGLY';Gstr{i,   2}='HO';
fMGLY(i)=fMGLY(i)-1.0;fHO(i)=fHO(i)-1.0;
fACO3(i)=fACO3(i)+  1.000;fCO(i)=fCO(i)+  1.000;

% 104, <R101>
i=i+1;
Rnames{ 104} = 'DCB1 + HO = 0.52000*HO2 +  0.33000*CO +  0.40000*ALD +  0.78000*KET +  0.10000*GLY +  0.01000*MGLY ';
k(:,i) = (  2.8000E-11.*exp(  1.7500E+02./T) ); 
Gstr{i,   1}='DCB1';Gstr{i,   2}='HO';
fDCB1(i)=fDCB1(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO2(i)=fHO2(i)+  0.520;fCO(i)=fCO(i)+  0.330;fALD(i)=fALD(i)+  0.400;fKET(i)=fKET(i)+  0.780;fGLY(i)=fGLY(i)+  0.100;fMGLY(i)=fMGLY(i)+  0.010;

% 105, <R102>
i=i+1;
Rnames{ 105} = 'DCB2 + HO = 0.52000*HO2 +  0.33000*CO +  0.13000*MEK +  0.10000*GLY +  0.01000*MGLY +  0.78000*OP2 ';
k(:,i) = (  2.8000E-11.*exp(  1.7500E+02./T) ); 
Gstr{i,   1}='DCB2';Gstr{i,   2}='HO';
fDCB2(i)=fDCB2(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO2(i)=fHO2(i)+  0.520;fCO(i)=fCO(i)+  0.330;fMEK(i)=fMEK(i)+  0.130;fGLY(i)=fGLY(i)+  0.100;fMGLY(i)=fMGLY(i)+  0.010;fOP2(i)=fOP2(i)+  0.780;

% 106, <R103>
i=i+1;
Rnames{ 106} = 'DCB3 + HO = 0.56000*HO2 +  0.21000*MACP +  0.11000*CO +  0.27000*GLY +  0.01000*MGLY +  0.79000*OP2 ';
k(:,i) = (  1.0000E-11 ); 
Gstr{i,   1}='DCB3';Gstr{i,   2}='HO';
fDCB3(i)=fDCB3(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO2(i)=fHO2(i)+  0.560;fMACP(i)=fMACP(i)+  0.210;fCO(i)=fCO(i)+  0.110;fGLY(i)=fGLY(i)+  0.270;fMGLY(i)=fMGLY(i)+  0.010;fOP2(i)=fOP2(i)+  0.790;

% 107, <R104>
i=i+1;
Rnames{ 107} = 'BALD + HO = BALP ';
k(:,i) = (  5.3200E-12.*exp(  2.4300E+02./T) ); 
Gstr{i,   1}='BALD';Gstr{i,   2}='HO';
fBALD(i)=fBALD(i)-1.0;fHO(i)=fHO(i)-1.0;
fBALP(i)=fBALP(i)+  1.000;

% 108, <R105>
i=i+1;
Rnames{ 108} = 'PHEN + HO = 0.15200*ASOATJ +  0.61900*HO2 +  0.17000*ADDC +  0.05900*CHO +  0.61900*MCT ';
k(:,i) = (  6.7500E-12.*exp(  4.0500E+02./T) ); 
Gstr{i,   1}='PHEN';Gstr{i,   2}='HO';
fPHEN(i)=fPHEN(i)-1.0;fHO(i)=fHO(i)-1.0;
fASOATJ(i)=fASOATJ(i)+  0.152;fHO2(i)=fHO2(i)+  0.619;fADDC(i)=fADDC(i)+  0.170;fCHO(i)=fCHO(i)+  0.059;fMCT(i)=fMCT(i)+  0.619;

% 109, <R106>
i=i+1;
Rnames{ 109} = 'CSL + HO = 0.20000*ASOATJ +  0.58400*HO2 +  0.16000*ADDC +  0.05600*CHO +  0.58400*MCT ';
k(:,i) = (  4.6500E-11.*exp(  0.0000E+00./T) ); 
Gstr{i,   1}='CSL';Gstr{i,   2}='HO';
fCSL(i)=fCSL(i)-1.0;fHO(i)=fHO(i)-1.0;
fASOATJ(i)=fASOATJ(i)+  0.200;fHO2(i)=fHO2(i)+  0.584;fADDC(i)=fADDC(i)+  0.160;fCHO(i)=fCHO(i)+  0.056;fMCT(i)=fMCT(i)+  0.584;

% 110, <R108>
i=i+1;
Rnames{ 110} = 'MCT + HO = MCTO ';
k(:,i) = (  2.0500E-10.*exp(  0.0000E+00./T) ); 
Gstr{i,   1}='MCT';Gstr{i,   2}='HO';
fMCT(i)=fMCT(i)-1.0;fHO(i)=fHO(i)-1.0;
fMCTO(i)=fMCTO(i)+  1.000;

% 111, <R109>
i=i+1;
Rnames{ 111} = 'MOH + HO = HO2 + HCHO ';
k(:,i) = (  2.8500E-12.*exp( -3.4500E+02./T) ); 
Gstr{i,   1}='MOH';Gstr{i,   2}='HO';
fMOH(i)=fMOH(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  1.000;

% 112, <R110>
i=i+1;
Rnames{ 112} = 'EOH + HO = HO2 + ACD ';
k(:,i) = (  3.0000E-12.*exp(  2.0000E+01./T) ); 
Gstr{i,   1}='EOH';Gstr{i,   2}='HO';
fEOH(i)=fEOH(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fACD(i)=fACD(i)+  1.000;

% 113, <R111>
i=i+1;
Rnames{ 113} = 'ROH + HO = HO2 +  0.71900*ALD +  0.18400*ACD ';
k(:,i) = (  2.6000E-12.*exp(  2.0000E+02./T) ); 
Gstr{i,   1}='ROH';Gstr{i,   2}='HO';
fROH(i)=fROH(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fALD(i)=fALD(i)+  0.719;fACD(i)=fACD(i)+  0.184;

% 114, <R112>
i=i+1;
Rnames{ 114} = 'ETEG + HO = HO2 + ALD ';
k(:,i) = (  1.4700E-11 ); 
Gstr{i,   1}='ETEG';Gstr{i,   2}='HO';
fETEG(i)=fETEG(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fALD(i)=fALD(i)+  1.000;

% 115, <R113>
i=i+1;
Rnames{ 115} = 'OP1 + HO = 0.35000*HO +  0.65000*MO2 +  0.35000*HCHO ';
k(:,i) = (  2.9000E-12.*exp(  1.9000E+02./T) ); 
Gstr{i,   1}='OP1';Gstr{i,   2}='HO';
fOP1(i)=fOP1(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  0.350;fMO2(i)=fMO2(i)+  0.650;fHCHO(i)=fHCHO(i)+  0.350;

% 116, <R114>
i=i+1;
Rnames{ 116} = 'OP2 + HO = 0.01000*HO +  0.44000*HC3P +  0.07000*XO2 +  0.08000*ALD +  0.41000*KET ';
k(:,i) = (  3.4000E-12.*exp(  1.9000E+02./T) ); 
Gstr{i,   1}='OP2';Gstr{i,   2}='HO';
fOP2(i)=fOP2(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  0.010;fHC3P(i)=fHC3P(i)+  0.440;fXO2(i)=fXO2(i)+  0.070;fALD(i)=fALD(i)+  0.080;fKET(i)=fKET(i)+  0.410;

% 117, <TRP06>
i=i+1;
Rnames{ 117} = 'OPB + HO = 0.01000*HO +  0.44000*HC10P +  0.07000*XO2 +  0.08000*ALD +  0.41000*KET ';
k(:,i) = (  3.4000E-12.*exp(  1.9000E+02./T) ); 
Gstr{i,   1}='OPB';Gstr{i,   2}='HO';
fOPB(i)=fOPB(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  0.010;fHC10P(i)=fHC10P(i)+  0.440;fXO2(i)=fXO2(i)+  0.070;fALD(i)=fALD(i)+  0.080;fKET(i)=fKET(i)+  0.410;

% 118, <R115>
i=i+1;
Rnames{ 118} = 'ISHP + HO = HO + MACR +  0.90400*IEPOX ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='ISHP';Gstr{i,   2}='HO';
fISHP(i)=fISHP(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  1.000;fMACR(i)=fMACR(i)+  1.000;fIEPOX(i)=fIEPOX(i)+  0.904;

% 119, <R116>
i=i+1;
Rnames{ 119} = 'MAHP + HO = MACP ';
k(:,i) = (  3.0000E-11 ); 
Gstr{i,   1}='MAHP';Gstr{i,   2}='HO';
fMAHP(i)=fMAHP(i)-1.0;fHO(i)=fHO(i)-1.0;
fMACP(i)=fMACP(i)+  1.000;

% 120, <R117>
i=i+1;
Rnames{ 120} = 'ORA1 + HO = HO2 ';
k(:,i) = (  4.5000E-13 ); 
Gstr{i,   1}='ORA1';Gstr{i,   2}='HO';
fORA1(i)=fORA1(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;

% 121, <R118>
i=i+1;
Rnames{ 121} = 'ORA2 + HO = 0.64000*MO2 +  0.36000*ORAP ';
k(:,i) = (  4.0000E-14.*exp(  8.5000E+02./T) ); 
Gstr{i,   1}='ORA2';Gstr{i,   2}='HO';
fORA2(i)=fORA2(i)-1.0;fHO(i)=fHO(i)-1.0;
fMO2(i)=fMO2(i)+  0.640;fORAP(i)=fORAP(i)+  0.360;

% 122, <R119>
i=i+1;
Rnames{ 122} = 'PAA + HO = 0.35000*HO +  0.65000*ACO3 +  0.35000*XO2 +  0.35000*HCHO ';
k(:,i) = (  2.9300E-12.*exp(  1.9000E+02./T) ); 
Gstr{i,   1}='PAA';Gstr{i,   2}='HO';
fPAA(i)=fPAA(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  0.350;fACO3(i)=fACO3(i)+  0.650;fXO2(i)=fXO2(i)+  0.350;fHCHO(i)=fHCHO(i)+  0.350;

% 123, <R120>
i=i+1;
Rnames{ 123} = 'PAN + HO = XO2 + NO3 + HCHO ';
k(:,i) = (  4.0000E-14 ); 
Gstr{i,   1}='PAN';Gstr{i,   2}='HO';
fPAN(i)=fPAN(i)-1.0;fHO(i)=fHO(i)-1.0;
fXO2(i)=fXO2(i)+  1.000;fNO3(i)=fNO3(i)+  1.000;fHCHO(i)=fHCHO(i)+  1.000;

% 124, <R121>
i=i+1;
Rnames{ 124} = 'PPN + HO = XO2 + NO3 + HCHO ';
k(:,i) = (  4.0000E-14 ); 
Gstr{i,   1}='PPN';Gstr{i,   2}='HO';
fPPN(i)=fPPN(i)-1.0;fHO(i)=fHO(i)-1.0;
fXO2(i)=fXO2(i)+  1.000;fNO3(i)=fNO3(i)+  1.000;fHCHO(i)=fHCHO(i)+  1.000;

% 125, <R122>
i=i+1;
Rnames{ 125} = 'MPAN + HO = NO2 + HKET ';
k(:,i) = (  3.2000E-11 ); 
Gstr{i,   1}='MPAN';Gstr{i,   2}='HO';
fMPAN(i)=fMPAN(i)-1.0;fHO(i)=fHO(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fHKET(i)=fHKET(i)+  1.000;

% 126, <R123>
i=i+1;
Rnames{ 126} = 'ONIT + HO = HC3P + NO2 ';
k(:,i) = (  5.3100E-12.*exp( -2.6000E+02./T) ); 
Gstr{i,   1}='ONIT';Gstr{i,   2}='HO';
fONIT(i)=fONIT(i)-1.0;fHO(i)=fHO(i)-1.0;
fHC3P(i)=fHC3P(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 127, <TRP07>
i=i+1;
Rnames{ 127} = 'TRPN + HO = HOM ';
k(:,i) = (  4.8000E-12 ); 
Gstr{i,   1}='TRPN';Gstr{i,   2}='HO';
fTRPN(i)=fTRPN(i)-1.0;fHO(i)=fHO(i)-1.0;
fHOM(i)=fHOM(i)+  1.000;

% 128, <R124>
i=i+1;
Rnames{ 128} = 'NALD + HO = NO2 + XO2 + HKET ';
k(:,i) = (  5.6000E-12.*exp(  2.7000E+02./T) ); 
Gstr{i,   1}='NALD';Gstr{i,   2}='HO';
fNALD(i)=fNALD(i)-1.0;fHO(i)=fHO(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fXO2(i)=fXO2(i)+  1.000;fHKET(i)=fHKET(i)+  1.000;

% 129, <R125>
i=i+1;
Rnames{ 129} = 'ISON + HO = NALD +  0.07000*HKET +  0.07000*HCHO ';
k(:,i) = (  1.3000E-11 ); 
Gstr{i,   1}='ISON';Gstr{i,   2}='HO';
fISON(i)=fISON(i)-1.0;fHO(i)=fHO(i)-1.0;
fNALD(i)=fNALD(i)+  1.000;fHKET(i)=fHKET(i)+  0.070;fHCHO(i)=fHCHO(i)+  0.070;

% 130, <R126>
i=i+1;
Rnames{ 130} = 'ETE + O3 = 0.08000*HO +  0.15000*HO2 +  0.43000*CO + HCHO +  0.37000*ORA1 ';
k(:,i) = (  9.1400E-15.*exp( -2.5800E+03./T) ); 
Gstr{i,   1}='ETE';Gstr{i,   2}='O3';
fETE(i)=fETE(i)-1.0;fO3(i)=fO3(i)-1.0;
fHO(i)=fHO(i)+  0.080;fHO2(i)=fHO2(i)+  0.150;fCO(i)=fCO(i)+  0.430;fHCHO(i)=fHCHO(i)+  1.000;fORA1(i)=fORA1(i)+  0.370;

% 131, <R127>
i=i+1;
Rnames{ 131} = 'OLT + O3 = 0.22000*HO +  0.32000*HO2 +  0.08000*MO2 +  0.06000*ETHP +  0.04000*HC3P +  0.02000*HC5P +  0.06800*H2O2 +  0.43000*CO +  0.02000*ETH +  0.01500*HC3 +  0.00600*HC5 +  0.03200*BEN +  0.56000*HCHO +  0.01000*ACD +  0.44000*ALD +  0.03000*ACT +  0.02000*BALD +  0.06000*MEK +  0.01000*HKET +  0.03000*ORA1 +  0.06000*ORA2 ';
k(:,i) = (  4.3300E-15.*exp( -1.8000E+03./T) ); 
Gstr{i,   1}='OLT';Gstr{i,   2}='O3';
fOLT(i)=fOLT(i)-1.0;fO3(i)=fO3(i)-1.0;
fHO(i)=fHO(i)+  0.220;fHO2(i)=fHO2(i)+  0.320;fMO2(i)=fMO2(i)+  0.080;fETHP(i)=fETHP(i)+  0.060;fHC3P(i)=fHC3P(i)+  0.040;fHC5P(i)=fHC5P(i)+  0.020;fH2O2(i)=fH2O2(i)+  0.068;fCO(i)=fCO(i)+  0.430;fETH(i)=fETH(i)+  0.020;fHC3(i)=fHC3(i)+  0.015;fHC5(i)=fHC5(i)+  0.006;fBEN(i)=fBEN(i)+  0.032;fHCHO(i)=fHCHO(i)+  0.560;fACD(i)=fACD(i)+  0.010;fALD(i)=fALD(i)+  0.440;fACT(i)=fACT(i)+  0.030;fBALD(i)=fBALD(i)+  0.020;fMEK(i)=fMEK(i)+  0.060;fHKET(i)=fHKET(i)+  0.010;fORA1(i)=fORA1(i)+  0.030;fORA2(i)=fORA2(i)+  0.060;

% 132, <R128>
i=i+1;
Rnames{ 132} = 'OLI + O3 = 0.46000*HO +  0.07000*HO2 +  0.32000*MO2 +  0.07000*ETHP +  0.04000*HC3P +  0.09000*ACO3 +  0.37000*CO +  0.02600*H2O2 +  0.01000*ETH +  0.01000*HC3 +  0.09000*HCHO +  0.45700*ACD +  0.73000*ALD +  0.11000*ACT +  0.01700*KET +  0.04400*HKET +  0.01700*ORA2 ';
k(:,i) = (  4.4000E-15.*exp( -8.4500E+02./T) ); 
Gstr{i,   1}='OLI';Gstr{i,   2}='O3';
fOLI(i)=fOLI(i)-1.0;fO3(i)=fO3(i)-1.0;
fHO(i)=fHO(i)+  0.460;fHO2(i)=fHO2(i)+  0.070;fMO2(i)=fMO2(i)+  0.320;fETHP(i)=fETHP(i)+  0.070;fHC3P(i)=fHC3P(i)+  0.040;fACO3(i)=fACO3(i)+  0.090;fCO(i)=fCO(i)+  0.370;fH2O2(i)=fH2O2(i)+  0.026;fETH(i)=fETH(i)+  0.010;fHC3(i)=fHC3(i)+  0.010;fHCHO(i)=fHCHO(i)+  0.090;fACD(i)=fACD(i)+  0.457;fALD(i)=fALD(i)+  0.730;fACT(i)=fACT(i)+  0.110;fKET(i)=fKET(i)+  0.017;fHKET(i)=fHKET(i)+  0.044;fORA2(i)=fORA2(i)+  0.017;

% 133, <R130>
i=i+1;
Rnames{ 133} = 'ISO + O3 = 0.25000*HO +  0.25000*HO2 +  0.08000*MO2 +  0.10000*ACO3 +  0.10000*MACP +  0.09000*H2O2 +  0.14000*CO +  0.58000*HCHO +  0.46100*MACR +  0.18900*MVK +  0.28000*ORA1 +  0.15300*OLT ';
k(:,i) = (  7.8600E-15.*exp( -1.9130E+03./T) ); 
Gstr{i,   1}='ISO';Gstr{i,   2}='O3';
fISO(i)=fISO(i)-1.0;fO3(i)=fO3(i)-1.0;
fHO(i)=fHO(i)+  0.250;fHO2(i)=fHO2(i)+  0.250;fMO2(i)=fMO2(i)+  0.080;fACO3(i)=fACO3(i)+  0.100;fMACP(i)=fMACP(i)+  0.100;fH2O2(i)=fH2O2(i)+  0.090;fCO(i)=fCO(i)+  0.140;fHCHO(i)=fHCHO(i)+  0.580;fMACR(i)=fMACR(i)+  0.461;fMVK(i)=fMVK(i)+  0.189;fORA1(i)=fORA1(i)+  0.280;fOLT(i)=fOLT(i)+  0.153;

% 134, <R131>
i=i+1;
Rnames{ 134} = 'API + O3 = 0.90000*HO +  0.90000*APIP1 +  0.05000*APIP2 +  0.05000*PINAL +  0.05000*H2O2 +  0.14000*CO ';
k(:,i) = (  5.0000E-16.*exp( -5.3000E+02./T) ); 
Gstr{i,   1}='API';Gstr{i,   2}='O3';
fAPI(i)=fAPI(i)-1.0;fO3(i)=fO3(i)-1.0;
fHO(i)=fHO(i)+  0.900;fAPIP1(i)=fAPIP1(i)+  0.900;fAPIP2(i)=fAPIP2(i)+  0.050;fPINAL(i)=fPINAL(i)+  0.050;fH2O2(i)=fH2O2(i)+  0.050;fCO(i)=fCO(i)+  0.140;

% 135, <R132>
i=i+1;
Rnames{ 135} = 'LIM + O3 = 0.84000*HO +  0.84000*LIMP1 +  0.11000*LIMP2 +  0.05000*LIMAL +  0.05000*H2O2 +  0.14000*CO ';
k(:,i) = (  2.9500E-15.*exp( -7.8300E+02./T) ); 
Gstr{i,   1}='LIM';Gstr{i,   2}='O3';
fLIM(i)=fLIM(i)-1.0;fO3(i)=fO3(i)-1.0;
fHO(i)=fHO(i)+  0.840;fLIMP1(i)=fLIMP1(i)+  0.840;fLIMP2(i)=fLIMP2(i)+  0.110;fLIMAL(i)=fLIMAL(i)+  0.050;fH2O2(i)=fH2O2(i)+  0.050;fCO(i)=fCO(i)+  0.140;

% 136, <TRP08>
i=i+1;
Rnames{ 136} = 'LIMAL + O3 = 0.04000*HO +  0.67000*HC10P +  0.79000*HCHO +  0.33000*KET +  0.04000*HO2 +  0.20000*CO ';
k(:,i) = (  8.3000E-18 ); 
Gstr{i,   1}='LIMAL';Gstr{i,   2}='O3';
fLIMAL(i)=fLIMAL(i)-1.0;fO3(i)=fO3(i)-1.0;
fHO(i)=fHO(i)+  0.040;fHC10P(i)=fHC10P(i)+  0.670;fHCHO(i)=fHCHO(i)+  0.790;fKET(i)=fKET(i)+  0.330;fHO2(i)=fHO2(i)+  0.040;fCO(i)=fCO(i)+  0.200;

% 137, <TRP09>
i=i+1;
Rnames{ 137} = 'TRPN + O3 = HOM ';
k(:,i) = (  1.6700E-16 ); 
Gstr{i,   1}='TRPN';Gstr{i,   2}='O3';
fTRPN(i)=fTRPN(i)-1.0;fO3(i)=fO3(i)-1.0;
fHOM(i)=fHOM(i)+  1.000;

% 138, <R132>
i=i+1;
Rnames{ 138} = 'MACR + O3 = 0.19000*HO +  0.14000*HO2 +  0.10000*ACO3 +  0.22000*CO +  0.50000*MGLY +  0.45000*ORA1 ';
k(:,i) = (  1.3600E-15.*exp( -2.1120E+03./T) ); 
Gstr{i,   1}='MACR';Gstr{i,   2}='O3';
fMACR(i)=fMACR(i)-1.0;fO3(i)=fO3(i)-1.0;
fHO(i)=fHO(i)+  0.190;fHO2(i)=fHO2(i)+  0.140;fACO3(i)=fACO3(i)+  0.100;fCO(i)=fCO(i)+  0.220;fMGLY(i)=fMGLY(i)+  0.500;fORA1(i)=fORA1(i)+  0.450;

% 139, <R134>
i=i+1;
Rnames{ 139} = 'MVK + O3 = 0.16000*HO +  0.11000*HO2 +  0.28000*ACO3 +  0.01000*XO2 +  0.56000*CO +  0.10000*HCHO +  0.54000*MGLY +  0.07000*ORA1 +  0.07000*ORA2 +  0.10000*ALD ';
k(:,i) = (  8.5000E-16.*exp( -1.5200E+03./T) ); 
Gstr{i,   1}='MVK';Gstr{i,   2}='O3';
fMVK(i)=fMVK(i)-1.0;fO3(i)=fO3(i)-1.0;
fHO(i)=fHO(i)+  0.160;fHO2(i)=fHO2(i)+  0.110;fACO3(i)=fACO3(i)+  0.280;fXO2(i)=fXO2(i)+  0.010;fCO(i)=fCO(i)+  0.560;fHCHO(i)=fHCHO(i)+  0.100;fMGLY(i)=fMGLY(i)+  0.540;fORA1(i)=fORA1(i)+  0.070;fORA2(i)=fORA2(i)+  0.070;fALD(i)=fALD(i)+  0.100;

% 140, <R135>
i=i+1;
Rnames{ 140} = 'UALD + O3 = 0.10000*HO +  0.07200*HO2 +  0.00800*MO2 +  0.00200*ACO3 +  0.10000*XO2 +  0.24300*CO +  0.08000*HCHO +  0.42000*ACD +  0.02800*KET +  0.49100*GLY +  0.00300*MGLY +  0.04400*ORA1 ';
k(:,i) = (  1.6600E-18 ); 
Gstr{i,   1}='UALD';Gstr{i,   2}='O3';
fUALD(i)=fUALD(i)-1.0;fO3(i)=fO3(i)-1.0;
fHO(i)=fHO(i)+  0.100;fHO2(i)=fHO2(i)+  0.072;fMO2(i)=fMO2(i)+  0.008;fACO3(i)=fACO3(i)+  0.002;fXO2(i)=fXO2(i)+  0.100;fCO(i)=fCO(i)+  0.243;fHCHO(i)=fHCHO(i)+  0.080;fACD(i)=fACD(i)+  0.420;fKET(i)=fKET(i)+  0.028;fGLY(i)=fGLY(i)+  0.491;fMGLY(i)=fMGLY(i)+  0.003;fORA1(i)=fORA1(i)+  0.044;

% 141, <R136>
i=i+1;
Rnames{ 141} = 'DCB1 + O3 = 0.05000*HO + HO2 +  0.60000*RCO3 +  0.60000*XO2 +  1.50000*CO +  0.05000*HCHO +  0.05000*GLY +  0.08000*MGLY +  0.65000*OP2 ';
k(:,i) = (  2.0000E-16 ); 
Gstr{i,   1}='DCB1';Gstr{i,   2}='O3';
fDCB1(i)=fDCB1(i)-1.0;fO3(i)=fO3(i)-1.0;
fHO(i)=fHO(i)+  0.050;fHO2(i)=fHO2(i)+  1.000;fRCO3(i)=fRCO3(i)+  0.600;fXO2(i)=fXO2(i)+  0.600;fCO(i)=fCO(i)+  1.500;fHCHO(i)=fHCHO(i)+  0.050;fGLY(i)=fGLY(i)+  0.050;fMGLY(i)=fMGLY(i)+  0.080;fOP2(i)=fOP2(i)+  0.650;

% 142, <R137>
i=i+1;
Rnames{ 142} = 'DCB2 + O3 = 0.05000*HO + HO2 +  0.60000*RCO3 +  0.60000*XO2 +  1.50000*CO +  0.05000*HCHO +  0.05000*GLY +  0.08000*MGLY +  0.70000*DCB1 +  0.65000*OP2 ';
k(:,i) = (  2.0000E-16 ); 
Gstr{i,   1}='DCB2';Gstr{i,   2}='O3';
fDCB2(i)=fDCB2(i)-1.0;fO3(i)=fO3(i)-1.0;
fHO(i)=fHO(i)+  0.050;fHO2(i)=fHO2(i)+  1.000;fRCO3(i)=fRCO3(i)+  0.600;fXO2(i)=fXO2(i)+  0.600;fCO(i)=fCO(i)+  1.500;fHCHO(i)=fHCHO(i)+  0.050;fGLY(i)=fGLY(i)+  0.050;fMGLY(i)=fMGLY(i)+  0.080;fDCB1(i)=fDCB1(i)+  0.700;fOP2(i)=fOP2(i)+  0.650;

% 143, <R138>
i=i+1;
Rnames{ 143} = 'DCB3 + O3 = 0.05000*HO + HO2 +  1.50000*CO +  0.48000*GLY +  0.70000*DCB1 +  0.25000*ORA1 +  0.25000*ORA2 +  0.11000*PAA ';
k(:,i) = (  9.0000E-17 ); 
Gstr{i,   1}='DCB3';Gstr{i,   2}='O3';
fDCB3(i)=fDCB3(i)-1.0;fO3(i)=fO3(i)-1.0;
fHO(i)=fHO(i)+  0.050;fHO2(i)=fHO2(i)+  1.000;fCO(i)=fCO(i)+  1.500;fGLY(i)=fGLY(i)+  0.480;fDCB1(i)=fDCB1(i)+  0.700;fORA1(i)=fORA1(i)+  0.250;fORA2(i)=fORA2(i)+  0.250;fPAA(i)=fPAA(i)+  0.110;

% 144, <R140>
i=i+1;
Rnames{ 144} = 'MCTO + O3 = MCTP ';
k(:,i) = (  2.8600E-13 ); 
Gstr{i,   1}='MCTO';Gstr{i,   2}='O3';
fMCTO(i)=fMCTO(i)-1.0;fO3(i)=fO3(i)-1.0;
fMCTP(i)=fMCTP(i)+  1.000;

% 145, <R141>
i=i+1;
Rnames{ 145} = 'ETE + NO3 = 0.80000*OLNN +  0.20000*OLND ';
k(:,i) = (  4.3920E-13.*exp( -2.2820E+03./T).*(T./300).^(  2.0000E+00 ) ); 
Gstr{i,   1}='ETE';Gstr{i,   2}='NO3';
fETE(i)=fETE(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fOLNN(i)=fOLNN(i)+  0.800;fOLND(i)=fOLND(i)+  0.200;

% 146, <R142>
i=i+1;
Rnames{ 146} = 'OLT + NO3 = 0.43000*OLNN +  0.57000*OLND ';
k(:,i) = (  1.7900E-13.*exp( -4.5000E+02./T) ); 
Gstr{i,   1}='OLT';Gstr{i,   2}='NO3';
fOLT(i)=fOLT(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fOLNN(i)=fOLNN(i)+  0.430;fOLND(i)=fOLND(i)+  0.570;

% 147, <R143>
i=i+1;
Rnames{ 147} = 'OLI + NO3 = 0.11000*OLNN +  0.89000*OLND ';
k(:,i) = (  8.6400E-13.*exp(  4.5000E+02./T) ); 
Gstr{i,   1}='OLI';Gstr{i,   2}='NO3';
fOLI(i)=fOLI(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fOLNN(i)=fOLNN(i)+  0.110;fOLND(i)=fOLND(i)+  0.890;

% 148, <R145>
i=i+1;
Rnames{ 148} = 'ISO + NO3 = ISON ';
k(:,i) = (  3.0300E-12.*exp( -4.4600E+02./T) ); 
Gstr{i,   1}='ISO';Gstr{i,   2}='NO3';
fISO(i)=fISO(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fISON(i)=fISON(i)+  1.000;

% 149, <R146>
i=i+1;
Rnames{ 149} = 'API + NO3 = 0.97500*APINP1 +  0.02500*APINP2 ';
k(:,i) = (  1.1900E-12.*exp(  4.9000E+02./T) ); 
Gstr{i,   1}='API';Gstr{i,   2}='NO3';
fAPI(i)=fAPI(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fAPINP1(i)=fAPINP1(i)+  0.975;fAPINP2(i)=fAPINP2(i)+  0.025;

% 150, <R147>
i=i+1;
Rnames{ 150} = 'LIM + NO3 = 0.94500*LIMNP1 +  0.05500*LIMNP2 ';
k(:,i) = (  1.2200E-11 ); 
Gstr{i,   1}='LIM';Gstr{i,   2}='NO3';
fLIM(i)=fLIM(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fLIMNP1(i)=fLIMNP1(i)+  0.945;fLIMNP2(i)=fLIMNP2(i)+  0.055;

% 151, <TRP10>
i=i+1;
Rnames{ 151} = 'TRPN + NO3 = HOM ';
k(:,i) = (  3.1500E-14.*exp( -4.4800E+02./T) ); 
Gstr{i,   1}='TRPN';Gstr{i,   2}='NO3';
fTRPN(i)=fTRPN(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHOM(i)=fHOM(i)+  1.000;

% 152, <R148>
i=i+1;
Rnames{ 152} = 'HCHO + NO3 = HO2 + CO + HNO3 ';
k(:,i) = (  2.0000E-12.*exp( -2.4400E+03./T) ); 
Gstr{i,   1}='HCHO';Gstr{i,   2}='NO3';
fHCHO(i)=fHCHO(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fCO(i)=fCO(i)+  1.000;fHNO3(i)=fHNO3(i)+  1.000;

% 153, <R149>
i=i+1;
Rnames{ 153} = 'ACD + NO3 = ACO3 + HNO3 ';
k(:,i) = (  1.4000E-12.*exp( -1.9000E+03./T) ); 
Gstr{i,   1}='ACD';Gstr{i,   2}='NO3';
fACD(i)=fACD(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fACO3(i)=fACO3(i)+  1.000;fHNO3(i)=fHNO3(i)+  1.000;

% 154, <R150>
i=i+1;
Rnames{ 154} = 'ALD + NO3 = RCO3 + HNO3 ';
k(:,i) = (  3.7600E-12.*exp( -1.9000E+03./T) ); 
Gstr{i,   1}='ALD';Gstr{i,   2}='NO3';
fALD(i)=fALD(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fRCO3(i)=fRCO3(i)+  1.000;fHNO3(i)=fHNO3(i)+  1.000;

% 155, <R151>
i=i+1;
Rnames{ 155} = 'MACR + NO3 = 0.68000*HCHO +  0.32000*MACP +  0.68000*XO2 +  0.68000*MGLY +  0.32000*HNO3 +  0.68000*NO2 ';
k(:,i) = (  3.4000E-15 ); 
Gstr{i,   1}='MACR';Gstr{i,   2}='NO3';
fMACR(i)=fMACR(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHCHO(i)=fHCHO(i)+  0.680;fMACP(i)=fMACP(i)+  0.320;fXO2(i)=fXO2(i)+  0.680;fMGLY(i)=fMGLY(i)+  0.680;fHNO3(i)=fHNO3(i)+  0.320;fNO2(i)=fNO2(i)+  0.680;

% 156, <R152>
i=i+1;
Rnames{ 156} = 'UALD + NO3 = HO2 + XO2 +  0.66800*CO +  0.33200*HCHO +  0.33200*ALD + ONIT ';
k(:,i) = (  5.0200E-13.*exp( -1.0760E+03./T) ); 
Gstr{i,   1}='UALD';Gstr{i,   2}='NO3';
fUALD(i)=fUALD(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fXO2(i)=fXO2(i)+  1.000;fCO(i)=fCO(i)+  0.668;fHCHO(i)=fHCHO(i)+  0.332;fALD(i)=fALD(i)+  0.332;fONIT(i)=fONIT(i)+  1.000;

% 157, <R153>
i=i+1;
Rnames{ 157} = 'GLY + NO3 = HO2 +  2.00000*CO + HNO3 ';
k(:,i) = (  2.9000E-12.*exp( -1.9000E+03./T) ); 
Gstr{i,   1}='GLY';Gstr{i,   2}='NO3';
fGLY(i)=fGLY(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fCO(i)=fCO(i)+  2.000;fHNO3(i)=fHNO3(i)+  1.000;

% 158, <R154>
i=i+1;
Rnames{ 158} = 'MGLY + NO3 = ACO3 + CO + HNO3 ';
k(:,i) = (  3.7600E-12.*exp( -1.9000E+03./T) ); 
Gstr{i,   1}='MGLY';Gstr{i,   2}='NO3';
fMGLY(i)=fMGLY(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fACO3(i)=fACO3(i)+  1.000;fCO(i)=fCO(i)+  1.000;fHNO3(i)=fHNO3(i)+  1.000;

% 159, <R155>
i=i+1;
Rnames{ 159} = 'PHEN + NO3 = 0.15200*ASOATJ +  0.33900*CHO +  0.85000*ADDC +  0.42400*ADCN +  0.42400*HNO3 ';
k(:,i) = (  3.7800E-12 ); 
Gstr{i,   1}='PHEN';Gstr{i,   2}='NO3';
fPHEN(i)=fPHEN(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fASOATJ(i)=fASOATJ(i)+  0.152;fCHO(i)=fCHO(i)+  0.339;fADDC(i)=fADDC(i)+  0.850;fADCN(i)=fADCN(i)+  0.424;fHNO3(i)=fHNO3(i)+  0.424;

% 160, <R156>
i=i+1;
Rnames{ 160} = 'CSL + NO3 = 0.20000*ASOATJ +  0.32000*CHO +  0.08000*ADDC +  0.40000*ADCN +  0.40000*HNO3 ';
k(:,i) = (  1.0600E-12 ); 
Gstr{i,   1}='CSL';Gstr{i,   2}='NO3';
fCSL(i)=fCSL(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fASOATJ(i)=fASOATJ(i)+  0.200;fCHO(i)=fCHO(i)+  0.320;fADDC(i)=fADDC(i)+  0.080;fADCN(i)=fADCN(i)+  0.400;fHNO3(i)=fHNO3(i)+  0.400;

% 161, <R158>
i=i+1;
Rnames{ 161} = 'MCT + NO3 = MCTO + HNO3 ';
k(:,i) = (  2.0100E-10 ); 
Gstr{i,   1}='MCT';Gstr{i,   2}='NO3';
fMCT(i)=fMCT(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fMCTO(i)=fMCTO(i)+  1.000;fHNO3(i)=fHNO3(i)+  1.000;

% 162, <R159>
i=i+1;
Rnames{ 162} = 'MPAN + NO3 = MACP + NO2 ';
k(:,i) = (  2.2000E-14.*exp( -5.0000E+02./T) ); 
Gstr{i,   1}='MPAN';Gstr{i,   2}='NO3';
fMPAN(i)=fMPAN(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fMACP(i)=fMACP(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 163, <TRP11>
i=i+1;
Rnames{ 163} = 'PINALP = HOM ';
k(:,i) = (  1.0000E+00 ); 
Gstr{i,   1}='PINALP';
fPINALP(i)=fPINALP(i)-1.0;
fHOM(i)=fHOM(i)+  1.000;

% 164, <TRP12>
i=i+1;
Rnames{ 164} = 'LIMALP = HOM ';
k(:,i) = (  1.0000E+00 ); 
Gstr{i,   1}='LIMALP';
fLIMALP(i)=fLIMALP(i)-1.0;
fHOM(i)=fHOM(i)+  1.000;

% 165, <R166>
i=i+1;
Rnames{ 165} = 'ACO3 + NO2 = PAN ';
xko =   9.7000E-29.*M.*exp(  0.0000E+00./T).*(T./300).^ -5.6000E+00;
xkinf =   9.3000E-12.*exp(  0.0000E+00./T).*(T./300).^ -1.5000E+00;
xn =   1.0000E+00;
F =   6.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='ACO3';Gstr{i,   2}='NO2';
fACO3(i)=fACO3(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fPAN(i)=fPAN(i)+  1.000;

% 166, <R167>
i=i+1;
Rnames{ 166} = 'PAN = ACO3 + NO2 ';
k(:,i) = (  1.1111E+28.*exp( -1.4000E+04./T).*k(:, 165) ); 
Gstr{i,   1}='PAN';
fPAN(i)=fPAN(i)-1.0;
fACO3(i)=fACO3(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 167, <R168>
i=i+1;
Rnames{ 167} = 'RCO3 + NO2 = PPN ';
xko =   9.7000E-29.*M.*exp(  0.0000E+00./T).*(T./300).^ -5.6000E+00;
xkinf =   9.3000E-12.*exp(  0.0000E+00./T).*(T./300).^ -1.5000E+00;
xn =   1.0000E+00;
F =   6.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='RCO3';Gstr{i,   2}='NO2';
fRCO3(i)=fRCO3(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fPPN(i)=fPPN(i)+  1.000;

% 168, <R169>
i=i+1;
Rnames{ 168} = 'PPN = RCO3 + NO2 ';
k(:,i) = (  1.1111E+28.*exp( -1.4000E+04./T).*k(:, 167) ); 
Gstr{i,   1}='PPN';
fPPN(i)=fPPN(i)-1.0;
fRCO3(i)=fRCO3(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 169, <R170>
i=i+1;
Rnames{ 169} = 'MACP + NO2 = MPAN ';
k(:,i) = (  2.8000E-12.*exp(  1.8100E+02./T) ); 
Gstr{i,   1}='MACP';Gstr{i,   2}='NO2';
fMACP(i)=fMACP(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fMPAN(i)=fMPAN(i)+  1.000;

% 170, <R171>
i=i+1;
Rnames{ 170} = 'MPAN = MACP + NO2 ';
k(:,i) = (  1.6000E+16.*exp( -1.3486E+04./T) ); 
Gstr{i,   1}='MPAN';
fMPAN(i)=fMPAN(i)-1.0;
fMACP(i)=fMACP(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 171, <R172>
i=i+1;
Rnames{ 171} = 'MO2 + NO = HO2 + NO2 + HCHO ';
k(:,i) = (  2.8000E-12.*exp(  3.0000E+02./T) ); 
Gstr{i,   1}='MO2';Gstr{i,   2}='NO';
fMO2(i)=fMO2(i)-1.0;fNO(i)=fNO(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  1.000;

% 172, <R173>
i=i+1;
Rnames{ 172} = 'ETHP + NO = HO2 + NO2 + ACD ';
k(:,i) = (  2.6000E-12.*exp(  3.6500E+02./T) ); 
Gstr{i,   1}='ETHP';Gstr{i,   2}='NO';
fETHP(i)=fETHP(i)-1.0;fNO(i)=fNO(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;fACD(i)=fACD(i)+  1.000;

% 173, <R174>
i=i+1;
Rnames{ 173} = 'HC3P + NO = 0.66000*HO2 +  0.13100*MO2 +  0.04800*ETHP +  0.08900*XO2 +  0.93500*NO2 +  0.50400*ACD +  0.13200*ALD +  0.16500*ACT +  0.04200*MEK +  0.06500*ONIT ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='HC3P';Gstr{i,   2}='NO';
fHC3P(i)=fHC3P(i)-1.0;fNO(i)=fNO(i)-1.0;
fHO2(i)=fHO2(i)+  0.660;fMO2(i)=fMO2(i)+  0.131;fETHP(i)=fETHP(i)+  0.048;fXO2(i)=fXO2(i)+  0.089;fNO2(i)=fNO2(i)+  0.935;fACD(i)=fACD(i)+  0.504;fALD(i)=fALD(i)+  0.132;fACT(i)=fACT(i)+  0.165;fMEK(i)=fMEK(i)+  0.042;fONIT(i)=fONIT(i)+  0.065;

% 174, <R175>
i=i+1;
Rnames{ 174} = 'HC5P + NO = 0.20000*HO2 +  0.05100*MO2 +  0.23100*ETHP +  0.23500*XO2 +  0.86400*NO2 +  0.01800*HCHO +  0.04500*ACD +  0.20300*ALD +  0.03300*MEK +  0.21700*ACT +  0.03300*KET +  0.27200*HKET +  0.13600*ONIT ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='HC5P';Gstr{i,   2}='NO';
fHC5P(i)=fHC5P(i)-1.0;fNO(i)=fNO(i)-1.0;
fHO2(i)=fHO2(i)+  0.200;fMO2(i)=fMO2(i)+  0.051;fETHP(i)=fETHP(i)+  0.231;fXO2(i)=fXO2(i)+  0.235;fNO2(i)=fNO2(i)+  0.864;fHCHO(i)=fHCHO(i)+  0.018;fACD(i)=fACD(i)+  0.045;fALD(i)=fALD(i)+  0.203;fMEK(i)=fMEK(i)+  0.033;fACT(i)=fACT(i)+  0.217;fKET(i)=fKET(i)+  0.033;fHKET(i)=fHKET(i)+  0.272;fONIT(i)=fONIT(i)+  0.136;

% 175, <R177>
i=i+1;
Rnames{ 175} = 'ETEP + NO = HO2 + NO2 +  1.60000*HCHO +  0.20000*ALD ';
k(:,i) = (  9.0000E-12 ); 
Gstr{i,   1}='ETEP';Gstr{i,   2}='NO';
fETEP(i)=fETEP(i)-1.0;fNO(i)=fNO(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  1.600;fALD(i)=fALD(i)+  0.200;

% 176, <R178>
i=i+1;
Rnames{ 176} = 'OLTP + NO = 0.78000*HO2 +  0.97000*NO2 +  0.78000*HCHO +  0.01200*ACD +  0.44000*ALD +  0.06000*ACT +  0.13000*MEK +  0.03000*ONIT ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='OLTP';Gstr{i,   2}='NO';
fOLTP(i)=fOLTP(i)-1.0;fNO(i)=fNO(i)-1.0;
fHO2(i)=fHO2(i)+  0.780;fNO2(i)=fNO2(i)+  0.970;fHCHO(i)=fHCHO(i)+  0.780;fACD(i)=fACD(i)+  0.012;fALD(i)=fALD(i)+  0.440;fACT(i)=fACT(i)+  0.060;fMEK(i)=fMEK(i)+  0.130;fONIT(i)=fONIT(i)+  0.030;

% 177, <R179>
i=i+1;
Rnames{ 177} = 'OLIP + NO = 0.83000*HO2 +  0.95000*NO2 +  0.81000*ACD +  0.68000*ALD +  0.20000*ACT +  0.09000*KET +  0.02000*HKET +  0.05000*ONIT ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='OLIP';Gstr{i,   2}='NO';
fOLIP(i)=fOLIP(i)-1.0;fNO(i)=fNO(i)-1.0;
fHO2(i)=fHO2(i)+  0.830;fNO2(i)=fNO2(i)+  0.950;fACD(i)=fACD(i)+  0.810;fALD(i)=fALD(i)+  0.680;fACT(i)=fACT(i)+  0.200;fKET(i)=fKET(i)+  0.090;fHKET(i)=fHKET(i)+  0.020;fONIT(i)=fONIT(i)+  0.050;

% 178, <ROCARO33>
i=i+1;
Rnames{ 178} = 'BENP + NO = 0.00000*ONIT +  0.00120*VROCP4OXY2 +  0.00080*VROCN1OXY6 +  0.99800*NO2 +  0.99800*HO2 +  0.00000*BALD +  0.99800*GLY +  0.49900*FURANONE +  0.24950*DCB2 +  0.24950*DCB3 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='BENP';Gstr{i,   2}='NO';
fBENP(i)=fBENP(i)-1.0;fNO(i)=fNO(i)-1.0;
fONIT(i)=fONIT(i)-  0.000;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.001;fVROCN1OXY6(i)=fVROCN1OXY6(i)+  0.001;fNO2(i)=fNO2(i)+  0.998;fHO2(i)=fHO2(i)+  0.998;fBALD(i)=fBALD(i)-  0.000;fGLY(i)=fGLY(i)+  0.998;fFURANONE(i)=fFURANONE(i)+  0.499;fDCB2(i)=fDCB2(i)+  0.249;fDCB3(i)=fDCB3(i)+  0.249;

% 179, <ROCARO43>
i=i+1;
Rnames{ 179} = 'TOLP + NO = 0.00020*ONIT +  0.00130*VROCP4OXY2 +  0.00060*VROCN1OXY6 +  0.99800*NO2 +  0.99800*HO2 +  0.08520*BALD +  0.54770*GLY +  0.36510*MGLY +  0.36510*FURANONE +  0.54770*DCB1 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='TOLP';Gstr{i,   2}='NO';
fTOLP(i)=fTOLP(i)-1.0;fNO(i)=fNO(i)-1.0;
fONIT(i)=fONIT(i)+  0.000;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.001;fVROCN1OXY6(i)=fVROCN1OXY6(i)+  0.001;fNO2(i)=fNO2(i)+  0.998;fHO2(i)=fHO2(i)+  0.998;fBALD(i)=fBALD(i)+  0.085;fGLY(i)=fGLY(i)+  0.548;fMGLY(i)=fMGLY(i)+  0.365;fFURANONE(i)=fFURANONE(i)+  0.365;fDCB1(i)=fDCB1(i)+  0.548;

% 180, <ROCARO53>
i=i+1;
Rnames{ 180} = 'XYMP + NO = 0.00010*ONIT +  0.00130*VROCP3OXY2 +  0.00060*VROCP0OXY4 +  0.99800*NO2 +  0.99800*HO2 +  0.04810*BALD +  0.70290*GLY +  0.24700*MGLY +  0.35150*FURANONE +  0.59840*DCB2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='XYMP';Gstr{i,   2}='NO';
fXYMP(i)=fXYMP(i)-1.0;fNO(i)=fNO(i)-1.0;
fONIT(i)=fONIT(i)+  0.000;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.001;fVROCP0OXY4(i)=fVROCP0OXY4(i)+  0.001;fNO2(i)=fNO2(i)+  0.998;fHO2(i)=fHO2(i)+  0.998;fBALD(i)=fBALD(i)+  0.048;fGLY(i)=fGLY(i)+  0.703;fMGLY(i)=fMGLY(i)+  0.247;fFURANONE(i)=fFURANONE(i)+  0.351;fDCB2(i)=fDCB2(i)+  0.598;

% 181, <ROCARO63>
i=i+1;
Rnames{ 181} = 'XYEP + NO = 0.00020*ONIT +  0.00130*VROCP3OXY2 +  0.00060*VROCP0OXY4 +  0.99800*NO2 +  0.99800*HO2 +  0.08520*BALD +  0.54770*GLY +  0.36510*MGLY +  0.45640*FURANONE +  0.45640*DCB2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='XYEP';Gstr{i,   2}='NO';
fXYEP(i)=fXYEP(i)-1.0;fNO(i)=fNO(i)-1.0;
fONIT(i)=fONIT(i)+  0.000;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.001;fVROCP0OXY4(i)=fVROCP0OXY4(i)+  0.001;fNO2(i)=fNO2(i)+  0.998;fHO2(i)=fHO2(i)+  0.998;fBALD(i)=fBALD(i)+  0.085;fGLY(i)=fGLY(i)+  0.548;fMGLY(i)=fMGLY(i)+  0.365;fFURANONE(i)=fFURANONE(i)+  0.456;fDCB2(i)=fDCB2(i)+  0.456;

% 182, <R188>
i=i+1;
Rnames{ 182} = 'ISOP + NO = 0.88000*HO2 +  0.88000*NO2 +  0.20000*HCHO +  0.28000*MACR +  0.44000*MVK +  0.12000*ISON +  0.02100*GLY +  0.02900*HKET +  0.02700*ALD ';
k(:,i) = (  2.4300E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='ISOP';Gstr{i,   2}='NO';
fISOP(i)=fISOP(i)-1.0;fNO(i)=fNO(i)-1.0;
fHO2(i)=fHO2(i)+  0.880;fNO2(i)=fNO2(i)+  0.880;fHCHO(i)=fHCHO(i)+  0.200;fMACR(i)=fMACR(i)+  0.280;fMVK(i)=fMVK(i)+  0.440;fISON(i)=fISON(i)+  0.120;fGLY(i)=fGLY(i)+  0.021;fHKET(i)=fHKET(i)+  0.029;fALD(i)=fALD(i)+  0.027;

% 183, <R189>
i=i+1;
Rnames{ 183} = 'APIP1 + NO = 0.82000*HO2 +  0.82000*NO2 +  0.82000*PINAL +  0.18000*TRPN ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='APIP1';Gstr{i,   2}='NO';
fAPIP1(i)=fAPIP1(i)-1.0;fNO(i)=fNO(i)-1.0;
fHO2(i)=fHO2(i)+  0.820;fNO2(i)=fNO2(i)+  0.820;fPINAL(i)=fPINAL(i)+  0.820;fTRPN(i)=fTRPN(i)+  0.180;

% 184, <TRP13>
i=i+1;
Rnames{ 184} = 'APIP2 + NO = 0.82000*HO +  0.82000*NO2 + HOM ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='APIP2';Gstr{i,   2}='NO';
fAPIP2(i)=fAPIP2(i)-1.0;fNO(i)=fNO(i)-1.0;
fHO(i)=fHO(i)+  0.820;fNO2(i)=fNO2(i)+  0.820;fHOM(i)=fHOM(i)+  1.000;

% 185, <TRP14>
i=i+1;
Rnames{ 185} = 'APINP1 + NO = 2.00000*NO2 + PINAL ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='APINP1';Gstr{i,   2}='NO';
fAPINP1(i)=fAPINP1(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  2.000;fPINAL(i)=fPINAL(i)+  1.000;

% 186, <TRP15>
i=i+1;
Rnames{ 186} = 'APINP2 + NO = 0.82000*NO2 +  0.82000*HO + HOM ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='APINP2';Gstr{i,   2}='NO';
fAPINP2(i)=fAPINP2(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  0.820;fHO(i)=fHO(i)+  0.820;fHOM(i)=fHOM(i)+  1.000;

% 187, <R190>
i=i+1;
Rnames{ 187} = 'LIMP1 + NO = 0.77000*HO2 +  0.77000*NO2 +  0.49000*LIMAL +  0.28000*HCHO +  0.28000*UALD +  0.23000*TRPN ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='LIMP1';Gstr{i,   2}='NO';
fLIMP1(i)=fLIMP1(i)-1.0;fNO(i)=fNO(i)-1.0;
fHO2(i)=fHO2(i)+  0.770;fNO2(i)=fNO2(i)+  0.770;fLIMAL(i)=fLIMAL(i)+  0.490;fHCHO(i)=fHCHO(i)+  0.280;fUALD(i)=fUALD(i)+  0.280;fTRPN(i)=fTRPN(i)+  0.230;

% 188, <TRP16>
i=i+1;
Rnames{ 188} = 'LIMP2 + NO = 0.77000*HO +  0.77000*NO2 + HOM ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='LIMP2';Gstr{i,   2}='NO';
fLIMP2(i)=fLIMP2(i)-1.0;fNO(i)=fNO(i)-1.0;
fHO(i)=fHO(i)+  0.770;fNO2(i)=fNO2(i)+  0.770;fHOM(i)=fHOM(i)+  1.000;

% 189, <TRP17>
i=i+1;
Rnames{ 189} = 'LIMNP1 + NO = 2.00000*NO2 + LIMAL ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='LIMNP1';Gstr{i,   2}='NO';
fLIMNP1(i)=fLIMNP1(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  2.000;fLIMAL(i)=fLIMAL(i)+  1.000;

% 190, <TRP18>
i=i+1;
Rnames{ 190} = 'LIMNP2 + NO = 0.77000*NO2 +  0.77000*HO + HOM ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='LIMNP2';Gstr{i,   2}='NO';
fLIMNP2(i)=fLIMNP2(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  0.770;fHO(i)=fHO(i)+  0.770;fHOM(i)=fHOM(i)+  1.000;

% 191, <TRP19>
i=i+1;
Rnames{ 191} = 'PINALP + NO = 0.95000*HO2 +  0.95000*NO2 +  0.05000*TRPN +  0.95000*HCHO +  0.95000*KET ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='PINALP';Gstr{i,   2}='NO';
fPINALP(i)=fPINALP(i)-1.0;fNO(i)=fNO(i)-1.0;
fHO2(i)=fHO2(i)+  0.950;fNO2(i)=fNO2(i)+  0.950;fTRPN(i)=fTRPN(i)+  0.050;fHCHO(i)=fHCHO(i)+  0.950;fKET(i)=fKET(i)+  0.950;

% 192, <TRP20>
i=i+1;
Rnames{ 192} = 'LIMALP + NO = 0.94000*HO2 +  0.94000*NO2 +  0.06000*TRPN +  0.94000*HCHO +  0.94000*KET ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='LIMALP';Gstr{i,   2}='NO';
fLIMALP(i)=fLIMALP(i)-1.0;fNO(i)=fNO(i)-1.0;
fHO2(i)=fHO2(i)+  0.940;fNO2(i)=fNO2(i)+  0.940;fTRPN(i)=fTRPN(i)+  0.060;fHCHO(i)=fHCHO(i)+  0.940;fKET(i)=fKET(i)+  0.940;

% 193, <R191>
i=i+1;
Rnames{ 193} = 'ACO3 + NO = MO2 + NO2 ';
k(:,i) = (  8.1000E-12.*exp(  2.7000E+02./T) ); 
Gstr{i,   1}='ACO3';Gstr{i,   2}='NO';
fACO3(i)=fACO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fMO2(i)=fMO2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 194, <R192>
i=i+1;
Rnames{ 194} = 'RCO3 + NO = ETHP + NO2 ';
k(:,i) = (  8.1000E-12.*exp(  2.7000E+02./T) ); 
Gstr{i,   1}='RCO3';Gstr{i,   2}='NO';
fRCO3(i)=fRCO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fETHP(i)=fETHP(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 195, <R193>
i=i+1;
Rnames{ 195} = 'ACTP + NO = ACO3 + NO2 + HCHO ';
k(:,i) = (  2.9000E-12.*exp(  3.0000E+02./T) ); 
Gstr{i,   1}='ACTP';Gstr{i,   2}='NO';
fACTP(i)=fACTP(i)-1.0;fNO(i)=fNO(i)-1.0;
fACO3(i)=fACO3(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  1.000;

% 196, <R194>
i=i+1;
Rnames{ 196} = 'MEKP + NO = 0.67000*HO2 + NO2 +  0.33000*HCHO +  0.67000*DCB1 ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='MEKP';Gstr{i,   2}='NO';
fMEKP(i)=fMEKP(i)-1.0;fNO(i)=fNO(i)-1.0;
fHO2(i)=fHO2(i)+  0.670;fNO2(i)=fNO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  0.330;fDCB1(i)=fDCB1(i)+  0.670;

% 197, <R195>
i=i+1;
Rnames{ 197} = 'KETP + NO = 0.77000*HO2 +  0.23000*ACO3 +  0.16000*XO2 + NO2 +  0.46000*ALD +  0.54000*MGLY ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='KETP';Gstr{i,   2}='NO';
fKETP(i)=fKETP(i)-1.0;fNO(i)=fNO(i)-1.0;
fHO2(i)=fHO2(i)+  0.770;fACO3(i)=fACO3(i)+  0.230;fXO2(i)=fXO2(i)+  0.160;fNO2(i)=fNO2(i)+  1.000;fALD(i)=fALD(i)+  0.460;fMGLY(i)=fMGLY(i)+  0.540;

% 198, <R196>
i=i+1;
Rnames{ 198} = 'MACP + NO = 0.65000*MO2 +  0.35000*ACO3 + NO2 +  0.65000*CO +  0.65000*HCHO ';
k(:,i) = (  2.5400E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='MACP';Gstr{i,   2}='NO';
fMACP(i)=fMACP(i)-1.0;fNO(i)=fNO(i)-1.0;
fMO2(i)=fMO2(i)+  0.650;fACO3(i)=fACO3(i)+  0.350;fNO2(i)=fNO2(i)+  1.000;fCO(i)=fCO(i)+  0.650;fHCHO(i)=fHCHO(i)+  0.650;

% 199, <R197>
i=i+1;
Rnames{ 199} = 'MCP + NO = NO2 +  0.50000*HO2 +  0.50000*HCHO + HKET ';
k(:,i) = (  2.5400E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='MCP';Gstr{i,   2}='NO';
fMCP(i)=fMCP(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fHO2(i)=fHO2(i)+  0.500;fHCHO(i)=fHCHO(i)+  0.500;fHKET(i)=fHKET(i)+  1.000;

% 200, <R198>
i=i+1;
Rnames{ 200} = 'MVKP + NO = 0.30000*HO2 +  0.70000*ACO3 +  0.70000*XO2 + NO2 +  0.30000*HCHO +  0.70000*ALD +  0.30000*MGLY ';
k(:,i) = (  2.5400E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='MVKP';Gstr{i,   2}='NO';
fMVKP(i)=fMVKP(i)-1.0;fNO(i)=fNO(i)-1.0;
fHO2(i)=fHO2(i)+  0.300;fACO3(i)=fACO3(i)+  0.700;fXO2(i)=fXO2(i)+  0.700;fNO2(i)=fNO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  0.300;fALD(i)=fALD(i)+  0.700;fMGLY(i)=fMGLY(i)+  0.300;

% 201, <R199>
i=i+1;
Rnames{ 201} = 'UALP + NO = HO2 + NO2 +  0.61000*CO +  0.03000*HCHO +  0.27000*ALD +  0.18000*GLY +  0.70000*KET +  0.21000*MGLY ';
k(:,i) = (  2.5400E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='UALP';Gstr{i,   2}='NO';
fUALP(i)=fUALP(i)-1.0;fNO(i)=fNO(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;fCO(i)=fCO(i)+  0.610;fHCHO(i)=fHCHO(i)+  0.030;fALD(i)=fALD(i)+  0.270;fGLY(i)=fGLY(i)+  0.180;fKET(i)=fKET(i)+  0.700;fMGLY(i)=fMGLY(i)+  0.210;

% 202, <R200>
i=i+1;
Rnames{ 202} = 'BALP + NO = BAL1 + NO2 ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='BALP';Gstr{i,   2}='NO';
fBALP(i)=fBALP(i)-1.0;fNO(i)=fNO(i)-1.0;
fBAL1(i)=fBAL1(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 203, <R201>
i=i+1;
Rnames{ 203} = 'BAL1 + NO = BAL2 + NO2 ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='BAL1';Gstr{i,   2}='NO';
fBAL1(i)=fBAL1(i)-1.0;fNO(i)=fNO(i)-1.0;
fBAL2(i)=fBAL2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 204, <R202>
i=i+1;
Rnames{ 204} = 'ADDC + NO = HO2 + NO2 +  0.32000*HKET +  0.68000*GLY +  0.68000*OP2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='ADDC';Gstr{i,   2}='NO';
fADDC(i)=fADDC(i)-1.0;fNO(i)=fNO(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;fHKET(i)=fHKET(i)+  0.320;fGLY(i)=fGLY(i)+  0.680;fOP2(i)=fOP2(i)+  0.680;

% 205, <R203>
i=i+1;
Rnames{ 205} = 'MCTP + NO = MCTO + NO2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='MCTP';Gstr{i,   2}='NO';
fMCTP(i)=fMCTP(i)-1.0;fNO(i)=fNO(i)-1.0;
fMCTO(i)=fMCTO(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 206, <R204>
i=i+1;
Rnames{ 206} = 'ORAP + NO = NO2 + GLY + HO2 ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='ORAP';Gstr{i,   2}='NO';
fORAP(i)=fORAP(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fGLY(i)=fGLY(i)+  1.000;fHO2(i)=fHO2(i)+  1.000;

% 207, <R205>
i=i+1;
Rnames{ 207} = 'OLNN + NO = NO2 + HO2 + ONIT ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='OLNN';Gstr{i,   2}='NO';
fOLNN(i)=fOLNN(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fHO2(i)=fHO2(i)+  1.000;fONIT(i)=fONIT(i)+  1.000;

% 208, <R206>
i=i+1;
Rnames{ 208} = 'OLND + NO = 2.00000*NO2 +  0.28700*HCHO +  1.24000*ALD +  0.46400*KET ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='OLND';Gstr{i,   2}='NO';
fOLND(i)=fOLND(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  2.000;fHCHO(i)=fHCHO(i)+  0.287;fALD(i)=fALD(i)+  1.240;fKET(i)=fKET(i)+  0.464;

% 209, <R207>
i=i+1;
Rnames{ 209} = 'ADCN + NO = 2.00000*NO2 + GLY + OP2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='ADCN';Gstr{i,   2}='NO';
fADCN(i)=fADCN(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  2.000;fGLY(i)=fGLY(i)+  1.000;fOP2(i)=fOP2(i)+  1.000;

% 210, <R208>
i=i+1;
Rnames{ 210} = 'XO2 + NO = NO2 ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='XO2';Gstr{i,   2}='NO';
fXO2(i)=fXO2(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;

% 211, <R209>
i=i+1;
Rnames{ 211} = 'BAL2 + NO2 = ONIT ';
k(:,i) = (  2.0000E-11 ); 
Gstr{i,   1}='BAL2';Gstr{i,   2}='NO2';
fBAL2(i)=fBAL2(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fONIT(i)=fONIT(i)+  1.000;

% 212, <R210>
i=i+1;
Rnames{ 212} = 'CHO + NO2 = ONIT ';
k(:,i) = (  2.0000E-11 ); 
Gstr{i,   1}='CHO';Gstr{i,   2}='NO2';
fCHO(i)=fCHO(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fONIT(i)=fONIT(i)+  1.000;

% 213, <R211>
i=i+1;
Rnames{ 213} = 'MCTO + NO2 = ONIT ';
k(:,i) = (  2.0800E-12 ); 
Gstr{i,   1}='MCTO';Gstr{i,   2}='NO2';
fMCTO(i)=fMCTO(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fONIT(i)=fONIT(i)+  1.000;

% 214, <R212>
i=i+1;
Rnames{ 214} = 'MO2 + HO2 = OP1 ';
k(:,i) = (  4.1000E-13.*exp(  7.5000E+02./T) ); 
Gstr{i,   1}='MO2';Gstr{i,   2}='HO2';
fMO2(i)=fMO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP1(i)=fOP1(i)+  1.000;

% 215, <R213>
i=i+1;
Rnames{ 215} = 'ETHP + HO2 = OP2 ';
k(:,i) = (  7.5000E-13.*exp(  7.0000E+02./T) ); 
Gstr{i,   1}='ETHP';Gstr{i,   2}='HO2';
fETHP(i)=fETHP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  1.000;

% 216, <R214>
i=i+1;
Rnames{ 216} = 'HC3P + HO2 = OP2 ';
k(:,i) = (  1.6600E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='HC3P';Gstr{i,   2}='HO2';
fHC3P(i)=fHC3P(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  1.000;

% 217, <R215>
i=i+1;
Rnames{ 217} = 'HC5P + HO2 = OP2 ';
k(:,i) = (  1.6600E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='HC5P';Gstr{i,   2}='HO2';
fHC5P(i)=fHC5P(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  1.000;

% 218, <R217>
i=i+1;
Rnames{ 218} = 'ETEP + HO2 = OP2 ';
k(:,i) = (  1.9000E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='ETEP';Gstr{i,   2}='HO2';
fETEP(i)=fETEP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  1.000;

% 219, <R218>
i=i+1;
Rnames{ 219} = 'OLTP + HO2 = OP2 ';
k(:,i) = (  1.6600E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='OLTP';Gstr{i,   2}='HO2';
fOLTP(i)=fOLTP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  1.000;

% 220, <R219>
i=i+1;
Rnames{ 220} = 'OLIP + HO2 = OP2 ';
k(:,i) = (  1.6600E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='OLIP';Gstr{i,   2}='HO2';
fOLIP(i)=fOLIP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  1.000;

% 221, <ROCARO32>
i=i+1;
Rnames{ 221} = 'BENP + HO2 = 0.60210*OP2 +  0.39790*VROCN1OXY6 ';
k(:,i) = (  2.9100E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='BENP';Gstr{i,   2}='HO2';
fBENP(i)=fBENP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  0.602;fVROCN1OXY6(i)=fVROCN1OXY6(i)+  0.398;

% 222, <ROCARO42>
i=i+1;
Rnames{ 222} = 'TOLP + HO2 = 0.71950*OP2 +  0.28050*VROCN1OXY6 ';
k(:,i) = (  2.9100E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='TOLP';Gstr{i,   2}='HO2';
fTOLP(i)=fTOLP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  0.720;fVROCN1OXY6(i)=fVROCN1OXY6(i)+  0.281;

% 223, <ROCARO52>
i=i+1;
Rnames{ 223} = 'XYMP + HO2 = 0.04820*OP2 +  0.67470*OP3 +  0.27710*VROCP0OXY4 ';
k(:,i) = (  2.9100E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='XYMP';Gstr{i,   2}='HO2';
fXYMP(i)=fXYMP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  0.048;fOP3(i)=fOP3(i)+  0.675;fVROCP0OXY4(i)=fVROCP0OXY4(i)+  0.277;

% 224, <ROCARO62>
i=i+1;
Rnames{ 224} = 'XYEP + HO2 = 0.08540*OP2 +  0.63410*OP3 +  0.28050*VROCP0OXY4 ';
k(:,i) = (  2.9100E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='XYEP';Gstr{i,   2}='HO2';
fXYEP(i)=fXYEP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  0.085;fOP3(i)=fOP3(i)+  0.634;fVROCP0OXY4(i)=fVROCP0OXY4(i)+  0.281;

% 225, <R228>
i=i+1;
Rnames{ 225} = 'ISOP + HO2 = ISHP ';
k(:,i) = (  2.0500E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='ISOP';Gstr{i,   2}='HO2';
fISOP(i)=fISOP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fISHP(i)=fISHP(i)+  1.000;

% 226, <R229>
i=i+1;
Rnames{ 226} = 'APIP1 + HO2 = OPB ';
k(:,i) = (  1.5000E-11 ); 
Gstr{i,   1}='APIP1';Gstr{i,   2}='HO2';
fAPIP1(i)=fAPIP1(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOPB(i)=fOPB(i)+  1.000;

% 227, <TRP21>
i=i+1;
Rnames{ 227} = 'APIP2 + HO2 = HOM ';
k(:,i) = (  1.5000E-11 ); 
Gstr{i,   1}='APIP2';Gstr{i,   2}='HO2';
fAPIP2(i)=fAPIP2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHOM(i)=fHOM(i)+  1.000;

% 228, <TRP22>
i=i+1;
Rnames{ 228} = 'APINP1 + HO2 = TRPN ';
k(:,i) = (  1.5000E-11 ); 
Gstr{i,   1}='APINP1';Gstr{i,   2}='HO2';
fAPINP1(i)=fAPINP1(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fTRPN(i)=fTRPN(i)+  1.000;

% 229, <TRP23>
i=i+1;
Rnames{ 229} = 'APINP2 + HO2 = HOM ';
k(:,i) = (  1.5000E-11 ); 
Gstr{i,   1}='APINP2';Gstr{i,   2}='HO2';
fAPINP2(i)=fAPINP2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHOM(i)=fHOM(i)+  1.000;

% 230, <R230>
i=i+1;
Rnames{ 230} = 'LIMP1 + HO2 = OPB ';
k(:,i) = (  1.5000E-11 ); 
Gstr{i,   1}='LIMP1';Gstr{i,   2}='HO2';
fLIMP1(i)=fLIMP1(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOPB(i)=fOPB(i)+  1.000;

% 231, <TRP24>
i=i+1;
Rnames{ 231} = 'LIMP2 + HO2 = HOM ';
k(:,i) = (  1.5000E-11 ); 
Gstr{i,   1}='LIMP2';Gstr{i,   2}='HO2';
fLIMP2(i)=fLIMP2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHOM(i)=fHOM(i)+  1.000;

% 232, <TRP25>
i=i+1;
Rnames{ 232} = 'LIMNP1 + HO2 = TRPN ';
k(:,i) = (  1.5000E-11 ); 
Gstr{i,   1}='LIMNP1';Gstr{i,   2}='HO2';
fLIMNP1(i)=fLIMNP1(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fTRPN(i)=fTRPN(i)+  1.000;

% 233, <TRP26>
i=i+1;
Rnames{ 233} = 'LIMNP2 + HO2 = HOM ';
k(:,i) = (  1.5000E-11 ); 
Gstr{i,   1}='LIMNP2';Gstr{i,   2}='HO2';
fLIMNP2(i)=fLIMNP2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHOM(i)=fHOM(i)+  1.000;

% 234, <TRP27>
i=i+1;
Rnames{ 234} = 'PINALP + HO2 = OPB ';
k(:,i) = (  2.9100E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='PINALP';Gstr{i,   2}='HO2';
fPINALP(i)=fPINALP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOPB(i)=fOPB(i)+  1.000;

% 235, <TRP28>
i=i+1;
Rnames{ 235} = 'LIMALP + HO2 = OPB ';
k(:,i) = (  2.9100E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='LIMALP';Gstr{i,   2}='HO2';
fLIMALP(i)=fLIMALP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOPB(i)=fOPB(i)+  1.000;

% 236, <R231>
i=i+1;
Rnames{ 236} = 'ACO3 + HO2 = 0.44000*HO +  0.44000*MO2 +  0.15000*ORA2 +  0.41000*PAA ';
k(:,i) = (  4.3000E-13.*exp(  1.0400E+03./T) ); 
Gstr{i,   1}='ACO3';Gstr{i,   2}='HO2';
fACO3(i)=fACO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO(i)=fHO(i)+  0.440;fMO2(i)=fMO2(i)+  0.440;fORA2(i)=fORA2(i)+  0.150;fPAA(i)=fPAA(i)+  0.410;

% 237, <R232>
i=i+1;
Rnames{ 237} = 'RCO3 + HO2 = 0.44000*HO +  0.44000*ETHP +  0.15000*ORA2 +  0.41000*PAA ';
k(:,i) = (  4.3000E-13.*exp(  1.0400E+03./T) ); 
Gstr{i,   1}='RCO3';Gstr{i,   2}='HO2';
fRCO3(i)=fRCO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO(i)=fHO(i)+  0.440;fETHP(i)=fETHP(i)+  0.440;fORA2(i)=fORA2(i)+  0.150;fPAA(i)=fPAA(i)+  0.410;

% 238, <R233>
i=i+1;
Rnames{ 238} = 'ACTP + HO2 = 0.15000*HO +  0.15000*ACO3 +  0.15000*HCHO +  0.85000*OP2 ';
k(:,i) = (  1.1500E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='ACTP';Gstr{i,   2}='HO2';
fACTP(i)=fACTP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO(i)=fHO(i)+  0.150;fACO3(i)=fACO3(i)+  0.150;fHCHO(i)=fHCHO(i)+  0.150;fOP2(i)=fOP2(i)+  0.850;

% 239, <R234>
i=i+1;
Rnames{ 239} = 'MEKP + HO2 = OP2 ';
k(:,i) = (  1.1500E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='MEKP';Gstr{i,   2}='HO2';
fMEKP(i)=fMEKP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  1.000;

% 240, <R235>
i=i+1;
Rnames{ 240} = 'KETP + HO2 = OP2 ';
k(:,i) = (  1.1500E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='KETP';Gstr{i,   2}='HO2';
fKETP(i)=fKETP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  1.000;

% 241, <R236>
i=i+1;
Rnames{ 241} = 'MACP + HO2 = MAHP ';
k(:,i) = (  1.8200E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='MACP';Gstr{i,   2}='HO2';
fMACP(i)=fMACP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fMAHP(i)=fMAHP(i)+  1.000;

% 242, <R237>
i=i+1;
Rnames{ 242} = 'MCP + HO2 = MAHP ';
k(:,i) = (  1.8200E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='MCP';Gstr{i,   2}='HO2';
fMCP(i)=fMCP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fMAHP(i)=fMAHP(i)+  1.000;

% 243, <R238>
i=i+1;
Rnames{ 243} = 'MVKP + HO2 = OP2 ';
k(:,i) = (  2.9100E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='MVKP';Gstr{i,   2}='HO2';
fMVKP(i)=fMVKP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  1.000;

% 244, <R239>
i=i+1;
Rnames{ 244} = 'UALP + HO2 = OP2 ';
k(:,i) = (  2.9100E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='UALP';Gstr{i,   2}='HO2';
fUALP(i)=fUALP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  1.000;

% 245, <R240>
i=i+1;
Rnames{ 245} = 'ADDC + HO2 = OP2 ';
k(:,i) = (  3.7500E-13.*exp(  9.8000E+02./T) ); 
Gstr{i,   1}='ADDC';Gstr{i,   2}='HO2';
fADDC(i)=fADDC(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  1.000;

% 246, <R241>
i=i+1;
Rnames{ 246} = 'CHO + HO2 = CSL ';
k(:,i) = (  1.0000E-11 ); 
Gstr{i,   1}='CHO';Gstr{i,   2}='HO2';
fCHO(i)=fCHO(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fCSL(i)=fCSL(i)+  1.000;

% 247, <R242>
i=i+1;
Rnames{ 247} = 'MCTP + HO2 = OP2 ';
k(:,i) = (  3.7500E-13.*exp(  9.8000E+02./T) ); 
Gstr{i,   1}='MCTP';Gstr{i,   2}='HO2';
fMCTP(i)=fMCTP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  1.000;

% 248, <R243>
i=i+1;
Rnames{ 248} = 'ORAP + HO2 = OP2 ';
k(:,i) = (  1.1500E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='ORAP';Gstr{i,   2}='HO2';
fORAP(i)=fORAP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  1.000;

% 249, <R244>
i=i+1;
Rnames{ 249} = 'OLNN + HO2 = ONIT ';
k(:,i) = (  1.6600E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='OLNN';Gstr{i,   2}='HO2';
fOLNN(i)=fOLNN(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fONIT(i)=fONIT(i)+  1.000;

% 250, <R245>
i=i+1;
Rnames{ 250} = 'OLND + HO2 = ONIT ';
k(:,i) = (  1.6600E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='OLND';Gstr{i,   2}='HO2';
fOLND(i)=fOLND(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fONIT(i)=fONIT(i)+  1.000;

% 251, <R246>
i=i+1;
Rnames{ 251} = 'ADCN + HO2 = OP2 ';
k(:,i) = (  3.7500E-13.*exp(  9.8000E+02./T) ); 
Gstr{i,   1}='ADCN';Gstr{i,   2}='HO2';
fADCN(i)=fADCN(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  1.000;

% 252, <R247>
i=i+1;
Rnames{ 252} = 'XO2 + HO2 = OP2 ';
k(:,i) = (  1.6600E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='XO2';Gstr{i,   2}='HO2';
fXO2(i)=fXO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  1.000;

% 253, <R248>
i=i+1;
Rnames{ 253} = 'MO2 + MO2 = 0.74000*HO2 +  1.37000*HCHO +  0.63000*MOH ';
k(:,i) = (  9.5000E-14.*exp(  3.9000E+02./T) ); 
Gstr{i,   1}='MO2';Gstr{i,   2}='MO2';
fMO2(i)=fMO2(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  0.740;fHCHO(i)=fHCHO(i)+  1.370;fMOH(i)=fMOH(i)+  0.630;

% 254, <R249>
i=i+1;
Rnames{ 254} = 'ETHP + MO2 = HO2 +  0.75000*HCHO +  0.75000*ACD +  0.25000*MOH +  0.25000*EOH ';
k(:,i) = (  1.1800E-13.*exp(  1.5800E+02./T) ); 
Gstr{i,   1}='ETHP';Gstr{i,   2}='MO2';
fETHP(i)=fETHP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  0.750;fACD(i)=fACD(i)+  0.750;fMOH(i)=fMOH(i)+  0.250;fEOH(i)=fEOH(i)+  0.250;

% 255, <R250>
i=i+1;
Rnames{ 255} = 'HC3P + MO2 = 0.89400*HO2 +  0.08000*MO2 +  0.02600*ETHP +  0.02600*XO2 +  0.82700*HCHO +  0.19800*ALD +  0.49700*KET +  0.05000*GLY +  0.25000*MOH +  0.25000*ROH ';
k(:,i) = (  9.4600E-14.*exp(  4.3100E+02./T) ); 
Gstr{i,   1}='HC3P';Gstr{i,   2}='MO2';
fHC3P(i)=fHC3P(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  0.894;fMO2(i)=fMO2(i)+  0.080;fETHP(i)=fETHP(i)+  0.026;fXO2(i)=fXO2(i)+  0.026;fHCHO(i)=fHCHO(i)+  0.827;fALD(i)=fALD(i)+  0.198;fKET(i)=fKET(i)+  0.497;fGLY(i)=fGLY(i)+  0.050;fMOH(i)=fMOH(i)+  0.250;fROH(i)=fROH(i)+  0.250;

% 256, <R251>
i=i+1;
Rnames{ 256} = 'HC5P + MO2 = 0.84200*HO2 +  0.01800*MO2 +  0.14000*ETHP +  0.19100*XO2 +  0.77700*HCHO +  0.25100*ALD +  0.61800*KET +  0.25000*MOH +  0.25000*ROH ';
k(:,i) = (  1.0000E-13.*exp(  4.6700E+02./T) ); 
Gstr{i,   1}='HC5P';Gstr{i,   2}='MO2';
fHC5P(i)=fHC5P(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  0.842;fMO2(i)=fMO2(i)+  0.018;fETHP(i)=fETHP(i)+  0.140;fXO2(i)=fXO2(i)+  0.191;fHCHO(i)=fHCHO(i)+  0.777;fALD(i)=fALD(i)+  0.251;fKET(i)=fKET(i)+  0.618;fMOH(i)=fMOH(i)+  0.250;fROH(i)=fROH(i)+  0.250;

% 257, <R253>
i=i+1;
Rnames{ 257} = 'ETEP + MO2 = HO2 +  1.95000*HCHO +  0.15000*ALD +  0.25000*MOH +  0.25000*ETEG ';
k(:,i) = (  1.7100E-13.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='ETEP';Gstr{i,   2}='MO2';
fETEP(i)=fETEP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  1.950;fALD(i)=fALD(i)+  0.150;fMOH(i)=fMOH(i)+  0.250;fETEG(i)=fETEG(i)+  0.250;

% 258, <R254>
i=i+1;
Rnames{ 258} = 'OLTP + MO2 = HO2 +  1.50000*HCHO +  0.70500*ALD +  0.04500*KET +  0.25000*MOH +  0.25000*ROH ';
k(:,i) = (  1.4600E-13.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='OLTP';Gstr{i,   2}='MO2';
fOLTP(i)=fOLTP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  1.500;fALD(i)=fALD(i)+  0.705;fKET(i)=fKET(i)+  0.045;fMOH(i)=fMOH(i)+  0.250;fROH(i)=fROH(i)+  0.250;

% 259, <R255>
i=i+1;
Rnames{ 259} = 'OLIP + MO2 = HO2 +  0.75000*HCHO +  1.28000*ALD +  0.21800*KET +  0.25000*MOH +  0.25000*ROH ';
k(:,i) = (  9.1800E-14.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='OLIP';Gstr{i,   2}='MO2';
fOLIP(i)=fOLIP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  0.750;fALD(i)=fALD(i)+  1.280;fKET(i)=fKET(i)+  0.218;fMOH(i)=fMOH(i)+  0.250;fROH(i)=fROH(i)+  0.250;

% 260, <ROCARO35>
i=i+1;
Rnames{ 260} = 'BENP + MO2 = 0.68000*HCHO +  1.37000*HO2 +  0.32000*MOH +  0.00000*BALD + GLY +  0.50000*FURANONE +  0.25000*DCB2 +  0.25000*DCB3 ';
k(:,i) = (  3.5600E-14.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='BENP';Gstr{i,   2}='MO2';
fBENP(i)=fBENP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHCHO(i)=fHCHO(i)+  0.680;fHO2(i)=fHO2(i)+  1.370;fMOH(i)=fMOH(i)+  0.320;fBALD(i)=fBALD(i)-  0.000;fGLY(i)=fGLY(i)+  1.000;fFURANONE(i)=fFURANONE(i)+  0.500;fDCB2(i)=fDCB2(i)+  0.250;fDCB3(i)=fDCB3(i)+  0.250;

% 261, <ROCARO45>
i=i+1;
Rnames{ 261} = 'TOLP + MO2 = 0.68000*HCHO +  1.28460*HO2 +  0.32000*MOH +  0.08540*BALD +  0.54880*GLY +  0.36590*MGLY +  0.36590*FURANONE +  0.54880*DCB1 ';
k(:,i) = (  3.5600E-14.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='TOLP';Gstr{i,   2}='MO2';
fTOLP(i)=fTOLP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHCHO(i)=fHCHO(i)+  0.680;fHO2(i)=fHO2(i)+  1.285;fMOH(i)=fMOH(i)+  0.320;fBALD(i)=fBALD(i)+  0.085;fGLY(i)=fGLY(i)+  0.549;fMGLY(i)=fMGLY(i)+  0.366;fFURANONE(i)=fFURANONE(i)+  0.366;fDCB1(i)=fDCB1(i)+  0.549;

% 262, <ROCARO55>
i=i+1;
Rnames{ 262} = 'XYMP + MO2 = 0.68000*HCHO +  1.32180*HO2 +  0.32000*MOH +  0.04820*BALD +  0.70430*GLY +  0.24750*MGLY +  0.35220*FURANONE +  0.59960*DCB2 ';
k(:,i) = (  3.5600E-14.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='XYMP';Gstr{i,   2}='MO2';
fXYMP(i)=fXYMP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHCHO(i)=fHCHO(i)+  0.680;fHO2(i)=fHO2(i)+  1.322;fMOH(i)=fMOH(i)+  0.320;fBALD(i)=fBALD(i)+  0.048;fGLY(i)=fGLY(i)+  0.704;fMGLY(i)=fMGLY(i)+  0.247;fFURANONE(i)=fFURANONE(i)+  0.352;fDCB2(i)=fDCB2(i)+  0.600;

% 263, <ROCARO65>
i=i+1;
Rnames{ 263} = 'XYEP + MO2 = 0.68000*HCHO +  1.28460*HO2 +  0.32000*MOH +  0.08540*BALD +  0.54880*GLY +  0.36590*MGLY +  0.45730*FURANONE +  0.45730*DCB2 ';
k(:,i) = (  3.5600E-14.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='XYEP';Gstr{i,   2}='MO2';
fXYEP(i)=fXYEP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHCHO(i)=fHCHO(i)+  0.680;fHO2(i)=fHO2(i)+  1.285;fMOH(i)=fMOH(i)+  0.320;fBALD(i)=fBALD(i)+  0.085;fGLY(i)=fGLY(i)+  0.549;fMGLY(i)=fMGLY(i)+  0.366;fFURANONE(i)=fFURANONE(i)+  0.457;fDCB2(i)=fDCB2(i)+  0.457;

% 264, <R264>
i=i+1;
Rnames{ 264} = 'ISOP + MO2 = HO2 +  1.31000*HCHO +  0.15900*MACR +  0.25000*MVK +  0.25000*MOH +  0.25000*ROH +  0.02300*ALD +  0.01800*GLY +  0.01600*HKET ';
k(:,i) = (  3.4000E-14.*exp(  2.2100E+02./T) ); 
Gstr{i,   1}='ISOP';Gstr{i,   2}='MO2';
fISOP(i)=fISOP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  1.310;fMACR(i)=fMACR(i)+  0.159;fMVK(i)=fMVK(i)+  0.250;fMOH(i)=fMOH(i)+  0.250;fROH(i)=fROH(i)+  0.250;fALD(i)=fALD(i)+  0.023;fGLY(i)=fGLY(i)+  0.018;fHKET(i)=fHKET(i)+  0.016;

% 265, <R265>
i=i+1;
Rnames{ 265} = 'APIP1 + MO2 = HO2 +  0.75000*HCHO +  0.50000*PINAL +  0.25000*KET +  0.25000*MOH +  0.25000*ROH ';
k(:,i) = (  3.5600E-14.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='APIP1';Gstr{i,   2}='MO2';
fAPIP1(i)=fAPIP1(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  0.750;fPINAL(i)=fPINAL(i)+  0.500;fKET(i)=fKET(i)+  0.250;fMOH(i)=fMOH(i)+  0.250;fROH(i)=fROH(i)+  0.250;

% 266, <TRP29>
i=i+1;
Rnames{ 266} = 'APIP2 + MO2 = HO2 +  0.75000*HCHO +  0.25000*MOH + HOM ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='APIP2';Gstr{i,   2}='MO2';
fAPIP2(i)=fAPIP2(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  0.750;fMOH(i)=fMOH(i)+  0.250;fHOM(i)=fHOM(i)+  1.000;

% 267, <TRP30>
i=i+1;
Rnames{ 267} = 'APINP1 + MO2 = 0.50000*HO2 +  0.50000*NO2 +  0.75000*HCHO +  0.50000*PINAL +  0.25000*KET +  0.25000*MOH +  0.25000*ROH ';
k(:,i) = (  3.5600E-14.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='APINP1';Gstr{i,   2}='MO2';
fAPINP1(i)=fAPINP1(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  0.500;fNO2(i)=fNO2(i)+  0.500;fHCHO(i)=fHCHO(i)+  0.750;fPINAL(i)=fPINAL(i)+  0.500;fKET(i)=fKET(i)+  0.250;fMOH(i)=fMOH(i)+  0.250;fROH(i)=fROH(i)+  0.250;

% 268, <TRP31>
i=i+1;
Rnames{ 268} = 'APINP2 + MO2 = 0.75000*HO2 +  0.75000*NO2 +  0.25000*MOH +  0.75000*HCHO + HOM ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='APINP2';Gstr{i,   2}='MO2';
fAPINP2(i)=fAPINP2(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  0.750;fNO2(i)=fNO2(i)+  0.750;fMOH(i)=fMOH(i)+  0.250;fHCHO(i)=fHCHO(i)+  0.750;fHOM(i)=fHOM(i)+  1.000;

% 269, <R266>
i=i+1;
Rnames{ 269} = 'LIMP1 + MO2 = HO2 +  0.75000*HCHO +  0.50000*LIMAL +  0.25000*KET +  0.25000*MOH +  0.25000*ROH ';
k(:,i) = (  3.5600E-14.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='LIMP1';Gstr{i,   2}='MO2';
fLIMP1(i)=fLIMP1(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  0.750;fLIMAL(i)=fLIMAL(i)+  0.500;fKET(i)=fKET(i)+  0.250;fMOH(i)=fMOH(i)+  0.250;fROH(i)=fROH(i)+  0.250;

% 270, <TRP32>
i=i+1;
Rnames{ 270} = 'LIMP2 + MO2 = HO2 +  0.75000*HCHO +  0.25000*MOH + HOM ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='LIMP2';Gstr{i,   2}='MO2';
fLIMP2(i)=fLIMP2(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  0.750;fMOH(i)=fMOH(i)+  0.250;fHOM(i)=fHOM(i)+  1.000;

% 271, <TRP33>
i=i+1;
Rnames{ 271} = 'LIMNP1 + MO2 = 0.50000*HO2 +  0.75000*HCHO +  0.50000*LIMAL +  0.50000*NO2 +  0.25000*KET +  0.25000*MOH +  0.25000*ROH ';
k(:,i) = (  3.5600E-14.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='LIMNP1';Gstr{i,   2}='MO2';
fLIMNP1(i)=fLIMNP1(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  0.500;fHCHO(i)=fHCHO(i)+  0.750;fLIMAL(i)=fLIMAL(i)+  0.500;fNO2(i)=fNO2(i)+  0.500;fKET(i)=fKET(i)+  0.250;fMOH(i)=fMOH(i)+  0.250;fROH(i)=fROH(i)+  0.250;

% 272, <TRP34>
i=i+1;
Rnames{ 272} = 'LIMNP2 + MO2 = 0.75000*HO2 +  0.75000*HCHO +  0.50000*NO2 +  0.25000*MOH + HOM ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='LIMNP2';Gstr{i,   2}='MO2';
fLIMNP2(i)=fLIMNP2(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  0.750;fHCHO(i)=fHCHO(i)+  0.750;fNO2(i)=fNO2(i)+  0.500;fMOH(i)=fMOH(i)+  0.250;fHOM(i)=fHOM(i)+  1.000;

% 273, <R267>
i=i+1;
Rnames{ 273} = 'ACO3 + MO2 = 0.90000*HO2 +  0.90000*MO2 + HCHO +  0.10000*ORA2 ';
k(:,i) = (  2.0000E-11.*exp(  5.0000E+02./T) ); 
Gstr{i,   1}='ACO3';Gstr{i,   2}='MO2';
fACO3(i)=fACO3(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  0.900;fMO2(i)=fMO2(i)+  0.900;fHCHO(i)=fHCHO(i)+  1.000;fORA2(i)=fORA2(i)+  0.100;

% 274, <R268>
i=i+1;
Rnames{ 274} = 'RCO3 + MO2 = 0.90000*HO2 +  0.90000*MO2 + HCHO +  0.10000*ORA2 ';
k(:,i) = (  2.0000E-11.*exp(  5.0000E+02./T) ); 
Gstr{i,   1}='RCO3';Gstr{i,   2}='MO2';
fRCO3(i)=fRCO3(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  0.900;fMO2(i)=fMO2(i)+  0.900;fHCHO(i)=fHCHO(i)+  1.000;fORA2(i)=fORA2(i)+  0.100;

% 275, <R269>
i=i+1;
Rnames{ 275} = 'ACTP + MO2 = 0.50000*HO2 +  0.50000*ACO3 +  1.50000*HCHO +  0.25000*MOH +  0.25000*ROH +  0.12500*ORA2 ';
k(:,i) = (  7.5000E-13.*exp(  5.0000E+02./T) ); 
Gstr{i,   1}='ACTP';Gstr{i,   2}='MO2';
fACTP(i)=fACTP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  0.500;fACO3(i)=fACO3(i)+  0.500;fHCHO(i)=fHCHO(i)+  1.500;fMOH(i)=fMOH(i)+  0.250;fROH(i)=fROH(i)+  0.250;fORA2(i)=fORA2(i)+  0.125;

% 276, <R270>
i=i+1;
Rnames{ 276} = 'MEKP + MO2 = 0.83400*HO2 + HCHO +  0.33400*DCB1 +  0.25000*MOH +  0.25000*ROH ';
k(:,i) = (  6.9100E-13.*exp(  5.0800E+02./T) ); 
Gstr{i,   1}='MEKP';Gstr{i,   2}='MO2';
fMEKP(i)=fMEKP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  0.834;fHCHO(i)=fHCHO(i)+  1.000;fDCB1(i)=fDCB1(i)+  0.334;fMOH(i)=fMOH(i)+  0.250;fROH(i)=fROH(i)+  0.250;

% 277, <R271>
i=i+1;
Rnames{ 277} = 'KETP + MO2 = HO2 +  0.75000*HCHO +  0.50000*DCB1 +  0.25000*MOH +  0.25000*ROH ';
k(:,i) = (  6.9100E-13.*exp(  5.0800E+02./T) ); 
Gstr{i,   1}='KETP';Gstr{i,   2}='MO2';
fKETP(i)=fKETP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  0.750;fDCB1(i)=fDCB1(i)+  0.500;fMOH(i)=fMOH(i)+  0.250;fROH(i)=fROH(i)+  0.250;

% 278, <R272>
i=i+1;
Rnames{ 278} = 'MACP + MO2 = 0.50000*HO2 +  0.26900*ACO3 +  0.50000*CO +  1.66000*HCHO +  0.06700*ORA2 +  0.25000*MO2 +  0.25000*MOH +  0.25000*ROH ';
k(:,i) = (  3.4000E-14.*exp(  2.2100E+02./T) ); 
Gstr{i,   1}='MACP';Gstr{i,   2}='MO2';
fMACP(i)=fMACP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  0.500;fACO3(i)=fACO3(i)+  0.269;fCO(i)=fCO(i)+  0.500;fHCHO(i)=fHCHO(i)+  1.660;fORA2(i)=fORA2(i)+  0.067;fMO2(i)=fMO2(i)+  0.250;fMOH(i)=fMOH(i)+  0.250;fROH(i)=fROH(i)+  0.250;

% 279, <R273>
i=i+1;
Rnames{ 279} = 'MCP + MO2 = NO2 + HO2 +  1.50000*HCHO +  0.50000*HKET +  0.25000*MOH +  0.25000*ROH ';
k(:,i) = (  3.4000E-14.*exp(  2.2100E+02./T) ); 
Gstr{i,   1}='MCP';Gstr{i,   2}='MO2';
fMCP(i)=fMCP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fHO2(i)=fHO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  1.500;fHKET(i)=fHKET(i)+  0.500;fMOH(i)=fMOH(i)+  0.250;fROH(i)=fROH(i)+  0.250;

% 280, <R274>
i=i+1;
Rnames{ 280} = 'MVKP + MO2 = HO2 +  1.16000*ACO3 +  1.16000*XO2 +  1.50000*HCHO +  1.75000*ALD +  0.50000*MGLY +  0.25000*MOH +  0.25000*ROH +  0.29200*ORA2 ';
k(:,i) = (  8.3700E-14 ); 
Gstr{i,   1}='MVKP';Gstr{i,   2}='MO2';
fMVKP(i)=fMVKP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fACO3(i)=fACO3(i)+  1.160;fXO2(i)=fXO2(i)+  1.160;fHCHO(i)=fHCHO(i)+  1.500;fALD(i)=fALD(i)+  1.750;fMGLY(i)=fMGLY(i)+  0.500;fMOH(i)=fMOH(i)+  0.250;fROH(i)=fROH(i)+  0.250;fORA2(i)=fORA2(i)+  0.292;

% 281, <R275>
i=i+1;
Rnames{ 281} = 'UALP + MO2 = HO2 +  0.30500*CO +  0.77300*HCHO +  0.20300*ALD +  0.52500*KET +  0.13500*GLY +  0.10500*MGLY +  0.25000*MOH +  0.25000*ROH ';
k(:,i) = (  3.4000E-14.*exp(  2.2100E+02./T) ); 
Gstr{i,   1}='UALP';Gstr{i,   2}='MO2';
fUALP(i)=fUALP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fCO(i)=fCO(i)+  0.305;fHCHO(i)=fHCHO(i)+  0.773;fALD(i)=fALD(i)+  0.203;fKET(i)=fKET(i)+  0.525;fGLY(i)=fGLY(i)+  0.135;fMGLY(i)=fMGLY(i)+  0.105;fMOH(i)=fMOH(i)+  0.250;fROH(i)=fROH(i)+  0.250;

% 282, <R276>
i=i+1;
Rnames{ 282} = 'BALP + MO2 = HO2 + BAL1 + HCHO ';
k(:,i) = (  3.5600E-14.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='BALP';Gstr{i,   2}='MO2';
fBALP(i)=fBALP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fBAL1(i)=fBAL1(i)+  1.000;fHCHO(i)=fHCHO(i)+  1.000;

% 283, <R277>
i=i+1;
Rnames{ 283} = 'BAL1 + MO2 = HO2 + BAL2 + HCHO ';
k(:,i) = (  3.5600E-14.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='BAL1';Gstr{i,   2}='MO2';
fBAL1(i)=fBAL1(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fBAL2(i)=fBAL2(i)+  1.000;fHCHO(i)=fHCHO(i)+  1.000;

% 284, <R278>
i=i+1;
Rnames{ 284} = 'ADDC + MO2 = 2.00000*HO2 + HCHO +  0.32000*HKET +  0.68000*GLY +  0.68000*OP2 ';
k(:,i) = (  3.5600E-14.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='ADDC';Gstr{i,   2}='MO2';
fADDC(i)=fADDC(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  2.000;fHCHO(i)=fHCHO(i)+  1.000;fHKET(i)=fHKET(i)+  0.320;fGLY(i)=fGLY(i)+  0.680;fOP2(i)=fOP2(i)+  0.680;

% 285, <R279>
i=i+1;
Rnames{ 285} = 'MCTP + MO2 = HO2 + MCTO + HCHO ';
k(:,i) = (  3.5600E-14.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='MCTP';Gstr{i,   2}='MO2';
fMCTP(i)=fMCTP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fMCTO(i)=fMCTO(i)+  1.000;fHCHO(i)=fHCHO(i)+  1.000;

% 286, <R280>
i=i+1;
Rnames{ 286} = 'ORAP + MO2 = HCHO + HO2 + GLY ';
k(:,i) = (  7.5000E-13.*exp(  5.0000E+02./T) ); 
Gstr{i,   1}='ORAP';Gstr{i,   2}='MO2';
fORAP(i)=fORAP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHCHO(i)=fHCHO(i)+  1.000;fHO2(i)=fHO2(i)+  1.000;fGLY(i)=fGLY(i)+  1.000;

% 287, <R281>
i=i+1;
Rnames{ 287} = 'OLNN + MO2 = 2.00000*HO2 + HCHO + ONIT ';
k(:,i) = (  1.6000E-13.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='OLNN';Gstr{i,   2}='MO2';
fOLNN(i)=fOLNN(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  2.000;fHCHO(i)=fHCHO(i)+  1.000;fONIT(i)=fONIT(i)+  1.000;

% 288, <R282>
i=i+1;
Rnames{ 288} = 'OLND + MO2 = 0.50000*HO2 +  0.50000*NO2 +  0.96500*HCHO +  0.93000*ALD +  0.34800*KET +  0.25000*MOH +  0.25000*ROH +  0.50000*ONIT ';
k(:,i) = (  9.6800E-14.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='OLND';Gstr{i,   2}='MO2';
fOLND(i)=fOLND(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  0.500;fNO2(i)=fNO2(i)+  0.500;fHCHO(i)=fHCHO(i)+  0.965;fALD(i)=fALD(i)+  0.930;fKET(i)=fKET(i)+  0.348;fMOH(i)=fMOH(i)+  0.250;fROH(i)=fROH(i)+  0.250;fONIT(i)=fONIT(i)+  0.500;

% 289, <R283>
i=i+1;
Rnames{ 289} = 'ADCN + MO2 = HO2 +  0.70000*NO2 + HCHO +  0.70000*GLY +  0.70000*OP2 +  0.30000*ONIT ';
k(:,i) = (  3.5600E-14 ); 
Gstr{i,   1}='ADCN';Gstr{i,   2}='MO2';
fADCN(i)=fADCN(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fNO2(i)=fNO2(i)+  0.700;fHCHO(i)=fHCHO(i)+  1.000;fGLY(i)=fGLY(i)+  0.700;fOP2(i)=fOP2(i)+  0.700;fONIT(i)=fONIT(i)+  0.300;

% 290, <R284>
i=i+1;
Rnames{ 290} = 'XO2 + MO2 = HO2 + HCHO ';
k(:,i) = (  5.9900E-15.*exp(  1.5100E+03./T) ); 
Gstr{i,   1}='XO2';Gstr{i,   2}='MO2';
fXO2(i)=fXO2(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  1.000;

% 291, <R285>
i=i+1;
Rnames{ 291} = 'ETHP + ACO3 = 0.50000*HO2 +  0.50000*MO2 + ACD +  0.50000*ORA2 ';
k(:,i) = (  1.0300E-12.*exp(  2.1100E+02./T) ); 
Gstr{i,   1}='ETHP';Gstr{i,   2}='ACO3';
fETHP(i)=fETHP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.500;fMO2(i)=fMO2(i)+  0.500;fACD(i)=fACD(i)+  1.000;fORA2(i)=fORA2(i)+  0.500;

% 292, <R286>
i=i+1;
Rnames{ 292} = 'HC3P + ACO3 = 0.39400*HO2 +  0.58000*MO2 +  0.02600*ETHP +  0.02600*XO2 +  0.13000*HCHO +  0.27300*ALD +  0.66200*KET +  0.06700*GLY +  0.50000*ORA2 ';
k(:,i) = (  6.9000E-13.*exp(  4.6000E+02./T) ); 
Gstr{i,   1}='HC3P';Gstr{i,   2}='ACO3';
fHC3P(i)=fHC3P(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.394;fMO2(i)=fMO2(i)+  0.580;fETHP(i)=fETHP(i)+  0.026;fXO2(i)=fXO2(i)+  0.026;fHCHO(i)=fHCHO(i)+  0.130;fALD(i)=fALD(i)+  0.273;fKET(i)=fKET(i)+  0.662;fGLY(i)=fGLY(i)+  0.067;fORA2(i)=fORA2(i)+  0.500;

% 293, <R287>
i=i+1;
Rnames{ 293} = 'HC5P + ACO3 = 0.34200*HO2 +  0.51800*MO2 +  0.14000*ETHP +  0.19100*XO2 +  0.04200*HCHO +  0.38100*ALD +  0.82400*KET +  0.50000*ORA2 ';
k(:,i) = (  5.5900E-13.*exp(  5.2200E+02./T) ); 
Gstr{i,   1}='HC5P';Gstr{i,   2}='ACO3';
fHC5P(i)=fHC5P(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.342;fMO2(i)=fMO2(i)+  0.518;fETHP(i)=fETHP(i)+  0.140;fXO2(i)=fXO2(i)+  0.191;fHCHO(i)=fHCHO(i)+  0.042;fALD(i)=fALD(i)+  0.381;fKET(i)=fKET(i)+  0.824;fORA2(i)=fORA2(i)+  0.500;

% 294, <R289>
i=i+1;
Rnames{ 294} = 'ETEP + ACO3 = 0.50000*HO2 +  0.50000*MO2 +  1.60000*HCHO +  0.20000*ALD +  0.50000*ORA2 ';
k(:,i) = (  9.4800E-13.*exp(  7.6500E+02./T) ); 
Gstr{i,   1}='ETEP';Gstr{i,   2}='ACO3';
fETEP(i)=fETEP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.500;fMO2(i)=fMO2(i)+  0.500;fHCHO(i)=fHCHO(i)+  1.600;fALD(i)=fALD(i)+  0.200;fORA2(i)=fORA2(i)+  0.500;

% 295, <R290>
i=i+1;
Rnames{ 295} = 'OLTP + ACO3 = 0.50000*HO2 +  0.50000*MO2 + HCHO +  0.94000*ALD +  0.06000*KET +  0.50000*ORA2 ';
k(:,i) = (  8.1100E-13.*exp(  7.6500E+02./T) ); 
Gstr{i,   1}='OLTP';Gstr{i,   2}='ACO3';
fOLTP(i)=fOLTP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.500;fMO2(i)=fMO2(i)+  0.500;fHCHO(i)=fHCHO(i)+  1.000;fALD(i)=fALD(i)+  0.940;fKET(i)=fKET(i)+  0.060;fORA2(i)=fORA2(i)+  0.500;

% 296, <R291>
i=i+1;
Rnames{ 296} = 'OLIP + ACO3 = 0.50000*HO2 +  0.50000*MO2 +  1.71000*ALD +  0.29000*KET +  0.50000*ORA2 ';
k(:,i) = (  5.0900E-13.*exp(  7.6500E+02./T) ); 
Gstr{i,   1}='OLIP';Gstr{i,   2}='ACO3';
fOLIP(i)=fOLIP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.500;fMO2(i)=fMO2(i)+  0.500;fALD(i)=fALD(i)+  1.710;fKET(i)=fKET(i)+  0.290;fORA2(i)=fORA2(i)+  0.500;

% 297, <ROCARO36>
i=i+1;
Rnames{ 297} = 'BENP + ACO3 = 0.70000*MO2 + HO2 +  0.30000*ORA2 +  0.00000*BALD + GLY +  0.50000*FURANONE +  0.25000*DCB2 +  0.25000*DCB3 ';
k(:,i) = (  7.4000E-13.*exp(  7.6500E+02./T) ); 
Gstr{i,   1}='BENP';Gstr{i,   2}='ACO3';
fBENP(i)=fBENP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fMO2(i)=fMO2(i)+  0.700;fHO2(i)=fHO2(i)+  1.000;fORA2(i)=fORA2(i)+  0.300;fBALD(i)=fBALD(i)-  0.000;fGLY(i)=fGLY(i)+  1.000;fFURANONE(i)=fFURANONE(i)+  0.500;fDCB2(i)=fDCB2(i)+  0.250;fDCB3(i)=fDCB3(i)+  0.250;

% 298, <ROCARO46>
i=i+1;
Rnames{ 298} = 'TOLP + ACO3 = 0.70000*MO2 +  0.91460*HO2 +  0.30000*ORA2 +  0.08540*BALD +  0.54880*GLY +  0.36590*MGLY +  0.36590*FURANONE +  0.54880*DCB1 ';
k(:,i) = (  7.4000E-13.*exp(  7.6500E+02./T) ); 
Gstr{i,   1}='TOLP';Gstr{i,   2}='ACO3';
fTOLP(i)=fTOLP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fMO2(i)=fMO2(i)+  0.700;fHO2(i)=fHO2(i)+  0.915;fORA2(i)=fORA2(i)+  0.300;fBALD(i)=fBALD(i)+  0.085;fGLY(i)=fGLY(i)+  0.549;fMGLY(i)=fMGLY(i)+  0.366;fFURANONE(i)=fFURANONE(i)+  0.366;fDCB1(i)=fDCB1(i)+  0.549;

% 299, <ROCARO56>
i=i+1;
Rnames{ 299} = 'XYMP + ACO3 = 0.70000*MO2 +  0.95180*HO2 +  0.30000*ORA2 +  0.04820*BALD +  0.70430*GLY +  0.24750*MGLY +  0.35220*FURANONE +  0.59960*DCB2 ';
k(:,i) = (  7.4000E-13.*exp(  7.6500E+02./T) ); 
Gstr{i,   1}='XYMP';Gstr{i,   2}='ACO3';
fXYMP(i)=fXYMP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fMO2(i)=fMO2(i)+  0.700;fHO2(i)=fHO2(i)+  0.952;fORA2(i)=fORA2(i)+  0.300;fBALD(i)=fBALD(i)+  0.048;fGLY(i)=fGLY(i)+  0.704;fMGLY(i)=fMGLY(i)+  0.247;fFURANONE(i)=fFURANONE(i)+  0.352;fDCB2(i)=fDCB2(i)+  0.600;

% 300, <ROCARO66>
i=i+1;
Rnames{ 300} = 'XYEP + ACO3 = 0.70000*MO2 +  0.91460*HO2 +  0.30000*ORA2 +  0.08540*BALD +  0.54880*GLY +  0.36590*MGLY +  0.45730*FURANONE +  0.45730*DCB2 ';
k(:,i) = (  7.4000E-13.*exp(  7.6500E+02./T) ); 
Gstr{i,   1}='XYEP';Gstr{i,   2}='ACO3';
fXYEP(i)=fXYEP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fMO2(i)=fMO2(i)+  0.700;fHO2(i)=fHO2(i)+  0.915;fORA2(i)=fORA2(i)+  0.300;fBALD(i)=fBALD(i)+  0.085;fGLY(i)=fGLY(i)+  0.549;fMGLY(i)=fMGLY(i)+  0.366;fFURANONE(i)=fFURANONE(i)+  0.457;fDCB2(i)=fDCB2(i)+  0.457;

% 301, <R300>
i=i+1;
Rnames{ 301} = 'ISOP + ACO3 = 0.50000*HO2 +  0.50000*MO2 +  1.04800*HCHO +  0.21900*MACR +  0.30500*MVK +  0.50000*ORA2 ';
k(:,i) = (  8.4000E-14.*exp(  2.2100E+02./T) ); 
Gstr{i,   1}='ISOP';Gstr{i,   2}='ACO3';
fISOP(i)=fISOP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.500;fMO2(i)=fMO2(i)+  0.500;fHCHO(i)=fHCHO(i)+  1.048;fMACR(i)=fMACR(i)+  0.219;fMVK(i)=fMVK(i)+  0.305;fORA2(i)=fORA2(i)+  0.500;

% 302, <R301>
i=i+1;
Rnames{ 302} = 'APIP1 + ACO3 = 0.50000*HO2 +  0.50000*MO2 +  0.50000*PINAL +  0.50000*ORA2 +  0.50000*KET ';
k(:,i) = (  7.4000E-13.*exp(  7.6500E+02./T) ); 
Gstr{i,   1}='APIP1';Gstr{i,   2}='ACO3';
fAPIP1(i)=fAPIP1(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.500;fMO2(i)=fMO2(i)+  0.500;fPINAL(i)=fPINAL(i)+  0.500;fORA2(i)=fORA2(i)+  0.500;fKET(i)=fKET(i)+  0.500;

% 303, <TRP35>
i=i+1;
Rnames{ 303} = 'APIP2 + ACO3 = 0.50000*HO +  0.50000*MO2 +  0.50000*ORA2 + HOM ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='APIP2';Gstr{i,   2}='ACO3';
fAPIP2(i)=fAPIP2(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO(i)=fHO(i)+  0.500;fMO2(i)=fMO2(i)+  0.500;fORA2(i)=fORA2(i)+  0.500;fHOM(i)=fHOM(i)+  1.000;

% 304, <TRP36>
i=i+1;
Rnames{ 304} = 'APINP1 + ACO3 = 0.50000*HO2 +  0.50000*NO2 +  0.50000*PINAL +  0.50000*MO2 +  0.50000*ORA2 +  0.50000*KET ';
k(:,i) = (  7.4000E-13.*exp(  7.6500E+02./T) ); 
Gstr{i,   1}='APINP1';Gstr{i,   2}='ACO3';
fAPINP1(i)=fAPINP1(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.500;fNO2(i)=fNO2(i)+  0.500;fPINAL(i)=fPINAL(i)+  0.500;fMO2(i)=fMO2(i)+  0.500;fORA2(i)=fORA2(i)+  0.500;fKET(i)=fKET(i)+  0.500;

% 305, <TRP37>
i=i+1;
Rnames{ 305} = 'APINP2 + ACO3 = 0.50000*NO2 +  0.50000*MO2 +  0.50000*ORA2 + HOM ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='APINP2';Gstr{i,   2}='ACO3';
fAPINP2(i)=fAPINP2(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fNO2(i)=fNO2(i)+  0.500;fMO2(i)=fMO2(i)+  0.500;fORA2(i)=fORA2(i)+  0.500;fHOM(i)=fHOM(i)+  1.000;

% 306, <R302>
i=i+1;
Rnames{ 306} = 'LIMP1 + ACO3 = 0.50000*HO2 +  0.50000*MO2 +  0.50000*LIMAL +  0.50000*KET +  0.50000*ORA2 ';
k(:,i) = (  7.4000E-13.*exp(  7.6500E+02./T) ); 
Gstr{i,   1}='LIMP1';Gstr{i,   2}='ACO3';
fLIMP1(i)=fLIMP1(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.500;fMO2(i)=fMO2(i)+  0.500;fLIMAL(i)=fLIMAL(i)+  0.500;fKET(i)=fKET(i)+  0.500;fORA2(i)=fORA2(i)+  0.500;

% 307, <TRP38>
i=i+1;
Rnames{ 307} = 'LIMP2 + ACO3 = 0.50000*HO +  0.50000*MO2 +  0.50000*ORA2 + HOM ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='LIMP2';Gstr{i,   2}='ACO3';
fLIMP2(i)=fLIMP2(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO(i)=fHO(i)+  0.500;fMO2(i)=fMO2(i)+  0.500;fORA2(i)=fORA2(i)+  0.500;fHOM(i)=fHOM(i)+  1.000;

% 308, <TRP39>
i=i+1;
Rnames{ 308} = 'LIMNP1 + ACO3 = 0.50000*HO2 +  0.50000*LIMAL +  0.50000*NO2 +  0.50000*MO2 +  0.50000*ORA2 +  0.50000*KET ';
k(:,i) = (  7.4000E-13.*exp(  7.6500E+02./T) ); 
Gstr{i,   1}='LIMNP1';Gstr{i,   2}='ACO3';
fLIMNP1(i)=fLIMNP1(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.500;fLIMAL(i)=fLIMAL(i)+  0.500;fNO2(i)=fNO2(i)+  0.500;fMO2(i)=fMO2(i)+  0.500;fORA2(i)=fORA2(i)+  0.500;fKET(i)=fKET(i)+  0.500;

% 309, <TRP40>
i=i+1;
Rnames{ 309} = 'LIMNP2 + ACO3 = 0.50000*MO2 +  0.50000*NO2 +  0.50000*ORA2 + HOM ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='LIMNP2';Gstr{i,   2}='ACO3';
fLIMNP2(i)=fLIMNP2(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fMO2(i)=fMO2(i)+  0.500;fNO2(i)=fNO2(i)+  0.500;fORA2(i)=fORA2(i)+  0.500;fHOM(i)=fHOM(i)+  1.000;

% 310, <R303>
i=i+1;
Rnames{ 310} = 'ACO3 + ACO3 = 2.00000*MO2 ';
k(:,i) = (  2.5000E-12.*exp(  5.0000E+02./T) ); 
Gstr{i,   1}='ACO3';Gstr{i,   2}='ACO3';
fACO3(i)=fACO3(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fMO2(i)=fMO2(i)+  2.000;

% 311, <R304>
i=i+1;
Rnames{ 311} = 'RCO3 + ACO3 = MO2 + ETHP ';
k(:,i) = (  2.5000E-12.*exp(  5.0000E+02./T) ); 
Gstr{i,   1}='RCO3';Gstr{i,   2}='ACO3';
fRCO3(i)=fRCO3(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fMO2(i)=fMO2(i)+  1.000;fETHP(i)=fETHP(i)+  1.000;

% 312, <R305>
i=i+1;
Rnames{ 312} = 'ACTP + ACO3 = 0.50000*MO2 +  0.50000*ACO3 + HCHO +  0.75000*ORA2 ';
k(:,i) = (  7.5100E-13.*exp(  5.6500E+02./T) ); 
Gstr{i,   1}='ACTP';Gstr{i,   2}='ACO3';
fACTP(i)=fACTP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fMO2(i)=fMO2(i)+  0.500;fACO3(i)=fACO3(i)+  0.500;fHCHO(i)=fHCHO(i)+  1.000;fORA2(i)=fORA2(i)+  0.750;

% 313, <R306>
i=i+1;
Rnames{ 313} = 'MEKP + ACO3 = 0.33000*HO2 +  0.50000*MO2 +  0.33000*HCHO +  0.33400*DCB1 +  0.50000*ORA2 ';
k(:,i) = (  7.5100E-13.*exp(  5.6500E+02./T) ); 
Gstr{i,   1}='MEKP';Gstr{i,   2}='ACO3';
fMEKP(i)=fMEKP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.330;fMO2(i)=fMO2(i)+  0.500;fHCHO(i)=fHCHO(i)+  0.330;fDCB1(i)=fDCB1(i)+  0.334;fORA2(i)=fORA2(i)+  0.500;

% 314, <R307>
i=i+1;
Rnames{ 314} = 'KETP + ACO3 = 0.50000*HO2 +  0.50000*MO2 +  0.50000*DCB1 +  0.50000*ORA2 ';
k(:,i) = (  7.5100E-13.*exp(  5.6500E+02./T) ); 
Gstr{i,   1}='KETP';Gstr{i,   2}='ACO3';
fKETP(i)=fKETP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.500;fMO2(i)=fMO2(i)+  0.500;fDCB1(i)=fDCB1(i)+  0.500;fORA2(i)=fORA2(i)+  0.500;

% 315, <R308>
i=i+1;
Rnames{ 315} = 'MACP + ACO3 = 0.63500*ORA2 +  0.50000*MO2 +  0.26900*ACO3 +  0.50000*CO + HCHO ';
k(:,i) = (  8.4000E-14.*exp(  2.2100E+02./T) ); 
Gstr{i,   1}='MACP';Gstr{i,   2}='ACO3';
fMACP(i)=fMACP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fORA2(i)=fORA2(i)+  0.635;fMO2(i)=fMO2(i)+  0.500;fACO3(i)=fACO3(i)+  0.269;fCO(i)=fCO(i)+  0.500;fHCHO(i)=fHCHO(i)+  1.000;

% 316, <R309>
i=i+1;
Rnames{ 316} = 'MCP + ACO3 = NO2 +  0.50000*HO2 + HCHO +  0.50000*HKET +  0.50000*MO2 +  0.50000*ORA2 ';
k(:,i) = (  8.4000E-14.*exp(  2.2100E+02./T) ); 
Gstr{i,   1}='MCP';Gstr{i,   2}='ACO3';
fMCP(i)=fMCP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fHO2(i)=fHO2(i)+  0.500;fHCHO(i)=fHCHO(i)+  1.000;fHKET(i)=fHKET(i)+  0.500;fMO2(i)=fMO2(i)+  0.500;fORA2(i)=fORA2(i)+  0.500;

% 317, <R310>
i=i+1;
Rnames{ 317} = 'MVKP + ACO3 = 0.50000*HO2 +  0.50000*MO2 +  1.16000*ACO3 +  1.16000*XO2 + HCHO +  2.30000*ALD +  0.50000*MGLY +  1.08300*ORA2 ';
k(:,i) = (  1.6800E-12.*exp(  5.0000E+02./T) ); 
Gstr{i,   1}='MVKP';Gstr{i,   2}='ACO3';
fMVKP(i)=fMVKP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.500;fMO2(i)=fMO2(i)+  0.500;fACO3(i)=fACO3(i)+  1.160;fXO2(i)=fXO2(i)+  1.160;fHCHO(i)=fHCHO(i)+  1.000;fALD(i)=fALD(i)+  2.300;fMGLY(i)=fMGLY(i)+  0.500;fORA2(i)=fORA2(i)+  1.083;

% 318, <R311>
i=i+1;
Rnames{ 318} = 'UALP + ACO3 = 0.50000*HO2 +  0.50000*MO2 +  0.50000*CO +  0.03000*HCHO +  0.27000*ALD +  0.70000*KET +  0.18000*GLY +  0.10500*MGLY +  0.50000*ORA2 ';
k(:,i) = (  1.6800E-12.*exp(  5.0000E+02./T) ); 
Gstr{i,   1}='UALP';Gstr{i,   2}='ACO3';
fUALP(i)=fUALP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.500;fMO2(i)=fMO2(i)+  0.500;fCO(i)=fCO(i)+  0.500;fHCHO(i)=fHCHO(i)+  0.030;fALD(i)=fALD(i)+  0.270;fKET(i)=fKET(i)+  0.700;fGLY(i)=fGLY(i)+  0.180;fMGLY(i)=fMGLY(i)+  0.105;fORA2(i)=fORA2(i)+  0.500;

% 319, <R312>
i=i+1;
Rnames{ 319} = 'BALP + ACO3 = MO2 + BAL1 ';
k(:,i) = (  7.4000E-13.*exp(  7.6500E+02./T) ); 
Gstr{i,   1}='BALP';Gstr{i,   2}='ACO3';
fBALP(i)=fBALP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fMO2(i)=fMO2(i)+  1.000;fBAL1(i)=fBAL1(i)+  1.000;

% 320, <R313>
i=i+1;
Rnames{ 320} = 'BAL1 + ACO3 = MO2 + BAL2 ';
k(:,i) = (  7.4000E-13.*exp(  7.6500E+02./T) ); 
Gstr{i,   1}='BAL1';Gstr{i,   2}='ACO3';
fBAL1(i)=fBAL1(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fMO2(i)=fMO2(i)+  1.000;fBAL2(i)=fBAL2(i)+  1.000;

% 321, <R314>
i=i+1;
Rnames{ 321} = 'ADDC + ACO3 = 2.00000*HO2 + MO2 +  0.32000*HKET +  0.68000*GLY +  0.68000*OP2 ';
k(:,i) = (  7.4000E-13.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='ADDC';Gstr{i,   2}='ACO3';
fADDC(i)=fADDC(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO2(i)=fHO2(i)+  2.000;fMO2(i)=fMO2(i)+  1.000;fHKET(i)=fHKET(i)+  0.320;fGLY(i)=fGLY(i)+  0.680;fOP2(i)=fOP2(i)+  0.680;

% 322, <R315>
i=i+1;
Rnames{ 322} = 'MCTP + ACO3 = HO2 + MO2 + MCTO ';
k(:,i) = (  7.4000E-13.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='MCTP';Gstr{i,   2}='ACO3';
fMCTP(i)=fMCTP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fMO2(i)=fMO2(i)+  1.000;fMCTO(i)=fMCTO(i)+  1.000;

% 323, <R316>
i=i+1;
Rnames{ 323} = 'ORAP + ACO3 = MO2 + GLY ';
k(:,i) = (  7.5100E-13.*exp(  5.6500E+02./T) ); 
Gstr{i,   1}='ORAP';Gstr{i,   2}='ACO3';
fORAP(i)=fORAP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fMO2(i)=fMO2(i)+  1.000;fGLY(i)=fGLY(i)+  1.000;

% 324, <R317>
i=i+1;
Rnames{ 324} = 'OLNN + ACO3 = HO2 + MO2 + ONIT ';
k(:,i) = (  8.8500E-13.*exp(  7.6500E+02./T) ); 
Gstr{i,   1}='OLNN';Gstr{i,   2}='ACO3';
fOLNN(i)=fOLNN(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fMO2(i)=fMO2(i)+  1.000;fONIT(i)=fONIT(i)+  1.000;

% 325, <R318>
i=i+1;
Rnames{ 325} = 'OLND + ACO3 = 0.50000*MO2 + NO2 +  0.28700*HCHO +  1.24000*ALD +  0.46400*KET +  0.50000*ORA2 ';
k(:,i) = (  5.3700E-13.*exp(  7.6500E+02./T) ); 
Gstr{i,   1}='OLND';Gstr{i,   2}='ACO3';
fOLND(i)=fOLND(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fMO2(i)=fMO2(i)+  0.500;fNO2(i)=fNO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  0.287;fALD(i)=fALD(i)+  1.240;fKET(i)=fKET(i)+  0.464;fORA2(i)=fORA2(i)+  0.500;

% 326, <R319>
i=i+1;
Rnames{ 326} = 'ADCN + ACO3 = HO2 + MO2 +  0.70000*NO2 +  0.70000*GLY +  0.70000*OP2 +  0.30000*ONIT ';
k(:,i) = (  7.4000E-13.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='ADCN';Gstr{i,   2}='ACO3';
fADCN(i)=fADCN(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fMO2(i)=fMO2(i)+  1.000;fNO2(i)=fNO2(i)+  0.700;fGLY(i)=fGLY(i)+  0.700;fOP2(i)=fOP2(i)+  0.700;fONIT(i)=fONIT(i)+  0.300;

% 327, <R320>
i=i+1;
Rnames{ 327} = 'XO2 + ACO3 = MO2 ';
k(:,i) = (  3.4000E-14.*exp(  1.5600E+03./T) ); 
Gstr{i,   1}='XO2';Gstr{i,   2}='ACO3';
fXO2(i)=fXO2(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fMO2(i)=fMO2(i)+  1.000;

% 328, <R321>
i=i+1;
Rnames{ 328} = 'RCO3 + RCO3 = 2.00000*ETHP ';
k(:,i) = (  2.5000E-12.*exp(  5.0000E+02./T) ); 
Gstr{i,   1}='RCO3';Gstr{i,   2}='RCO3';
fRCO3(i)=fRCO3(i)-1.0;fRCO3(i)=fRCO3(i)-1.0;
fETHP(i)=fETHP(i)+  2.000;

% 329, <R322>
i=i+1;
Rnames{ 329} = 'MO2 + NO3 = HO2 + HCHO + NO2 ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='MO2';Gstr{i,   2}='NO3';
fMO2(i)=fMO2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 330, <R323>
i=i+1;
Rnames{ 330} = 'ETHP + NO3 = HO2 + NO2 + ACD ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='ETHP';Gstr{i,   2}='NO3';
fETHP(i)=fETHP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;fACD(i)=fACD(i)+  1.000;

% 331, <R324>
i=i+1;
Rnames{ 331} = 'HC3P + NO3 = 0.25400*HO2 +  0.14000*MO2 +  0.09200*XO2 +  0.50300*ETHP + NO2 +  0.51900*ACD +  0.14700*ALD +  0.07500*MEK +  0.09500*ACT ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='HC3P';Gstr{i,   2}='NO3';
fHC3P(i)=fHC3P(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.254;fMO2(i)=fMO2(i)+  0.140;fXO2(i)=fXO2(i)+  0.092;fETHP(i)=fETHP(i)+  0.503;fNO2(i)=fNO2(i)+  1.000;fACD(i)=fACD(i)+  0.519;fALD(i)=fALD(i)+  0.147;fMEK(i)=fMEK(i)+  0.075;fACT(i)=fACT(i)+  0.095;

% 332, <R325>
i=i+1;
Rnames{ 332} = 'HC5P + NO3 = 0.48800*HO2 +  0.05500*MO2 +  0.28000*ETHP +  0.48500*XO2 + NO2 +  0.02400*HCHO +  0.24100*ALD +  0.06000*KET +  0.06300*MEK +  0.24700*ACT +  0.04800*ACD +  0.27500*HKET ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='HC5P';Gstr{i,   2}='NO3';
fHC5P(i)=fHC5P(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.488;fMO2(i)=fMO2(i)+  0.055;fETHP(i)=fETHP(i)+  0.280;fXO2(i)=fXO2(i)+  0.485;fNO2(i)=fNO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  0.024;fALD(i)=fALD(i)+  0.241;fKET(i)=fKET(i)+  0.060;fMEK(i)=fMEK(i)+  0.063;fACT(i)=fACT(i)+  0.247;fACD(i)=fACD(i)+  0.048;fHKET(i)=fHKET(i)+  0.275;

% 333, <R327>
i=i+1;
Rnames{ 333} = 'ETEP + NO3 = HO2 + NO2 +  1.60000*HCHO +  0.20000*ALD ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='ETEP';Gstr{i,   2}='NO3';
fETEP(i)=fETEP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  1.600;fALD(i)=fALD(i)+  0.200;

% 334, <R328>
i=i+1;
Rnames{ 334} = 'OLTP + NO3 = 0.47000*ALD +  0.79000*HCHO +  0.79000*HO2 + NO2 +  0.18000*MEK +  0.02000*ACD +  0.09000*ACT ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='OLTP';Gstr{i,   2}='NO3';
fOLTP(i)=fOLTP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fALD(i)=fALD(i)+  0.470;fHCHO(i)=fHCHO(i)+  0.790;fHO2(i)=fHO2(i)+  0.790;fNO2(i)=fNO2(i)+  1.000;fMEK(i)=fMEK(i)+  0.180;fACD(i)=fACD(i)+  0.020;fACT(i)=fACT(i)+  0.090;

% 335, <R329>
i=i+1;
Rnames{ 335} = 'OLIP + NO3 = 0.86000*HO2 +  0.72000*ALD +  0.11000*KET + NO2 +  0.20000*ACT +  0.85000*ACD +  0.04000*HKET ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='OLIP';Gstr{i,   2}='NO3';
fOLIP(i)=fOLIP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.860;fALD(i)=fALD(i)+  0.720;fKET(i)=fKET(i)+  0.110;fNO2(i)=fNO2(i)+  1.000;fACT(i)=fACT(i)+  0.200;fACD(i)=fACD(i)+  0.850;fHKET(i)=fHKET(i)+  0.040;

% 336, <ROCARO34>
i=i+1;
Rnames{ 336} = 'BENP + NO3 = NO2 + HO2 +  0.00000*BALD + GLY +  0.50000*FURANONE +  0.25000*DCB2 +  0.25000*DCB3 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='BENP';Gstr{i,   2}='NO3';
fBENP(i)=fBENP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fHO2(i)=fHO2(i)+  1.000;fBALD(i)=fBALD(i)-  0.000;fGLY(i)=fGLY(i)+  1.000;fFURANONE(i)=fFURANONE(i)+  0.500;fDCB2(i)=fDCB2(i)+  0.250;fDCB3(i)=fDCB3(i)+  0.250;

% 337, <ROCARO44>
i=i+1;
Rnames{ 337} = 'TOLP + NO3 = NO2 +  0.91460*HO2 +  0.08540*BALD +  0.54880*GLY +  0.36590*MGLY +  0.36590*FURANONE +  0.54880*DCB1 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='TOLP';Gstr{i,   2}='NO3';
fTOLP(i)=fTOLP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fHO2(i)=fHO2(i)+  0.915;fBALD(i)=fBALD(i)+  0.085;fGLY(i)=fGLY(i)+  0.549;fMGLY(i)=fMGLY(i)+  0.366;fFURANONE(i)=fFURANONE(i)+  0.366;fDCB1(i)=fDCB1(i)+  0.549;

% 338, <ROCARO54>
i=i+1;
Rnames{ 338} = 'XYMP + NO3 = NO2 +  0.95180*HO2 +  0.04820*BALD +  0.70430*GLY +  0.24750*MGLY +  0.35220*FURANONE +  0.59960*DCB2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='XYMP';Gstr{i,   2}='NO3';
fXYMP(i)=fXYMP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fHO2(i)=fHO2(i)+  0.952;fBALD(i)=fBALD(i)+  0.048;fGLY(i)=fGLY(i)+  0.704;fMGLY(i)=fMGLY(i)+  0.247;fFURANONE(i)=fFURANONE(i)+  0.352;fDCB2(i)=fDCB2(i)+  0.600;

% 339, <ROCARO64>
i=i+1;
Rnames{ 339} = 'XYEP + NO3 = NO2 +  0.91460*HO2 +  0.08540*BALD +  0.54880*GLY +  0.36590*MGLY +  0.45730*FURANONE +  0.45730*DCB2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='XYEP';Gstr{i,   2}='NO3';
fXYEP(i)=fXYEP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fHO2(i)=fHO2(i)+  0.915;fBALD(i)=fBALD(i)+  0.085;fGLY(i)=fGLY(i)+  0.549;fMGLY(i)=fMGLY(i)+  0.366;fFURANONE(i)=fFURANONE(i)+  0.457;fDCB2(i)=fDCB2(i)+  0.457;

% 340, <R338>
i=i+1;
Rnames{ 340} = 'ISOP + NO3 = HO2 + NO2 +  0.75000*HCHO +  0.31800*MACR +  0.50000*MVK +  0.02400*GLY +  0.03300*HKET +  0.03100*ALD ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='ISOP';Gstr{i,   2}='NO3';
fISOP(i)=fISOP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  0.750;fMACR(i)=fMACR(i)+  0.318;fMVK(i)=fMVK(i)+  0.500;fGLY(i)=fGLY(i)+  0.024;fHKET(i)=fHKET(i)+  0.033;fALD(i)=fALD(i)+  0.031;

% 341, <R339>
i=i+1;
Rnames{ 341} = 'APIP1 + NO3 = HO2 + NO2 + ALD + KET ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='APIP1';Gstr{i,   2}='NO3';
fAPIP1(i)=fAPIP1(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;fALD(i)=fALD(i)+  1.000;fKET(i)=fKET(i)+  1.000;

% 342, <R340>
i=i+1;
Rnames{ 342} = 'LIMP1 + NO3 = HO2 + NO2 +  0.38500*OLI +  0.38500*HCHO +  0.61500*MACR ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='LIMP1';Gstr{i,   2}='NO3';
fLIMP1(i)=fLIMP1(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;fOLI(i)=fOLI(i)+  0.385;fHCHO(i)=fHCHO(i)+  0.385;fMACR(i)=fMACR(i)+  0.615;

% 343, <R341>
i=i+1;
Rnames{ 343} = 'ACO3 + NO3 = MO2 + NO2 ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='ACO3';Gstr{i,   2}='NO3';
fACO3(i)=fACO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fMO2(i)=fMO2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 344, <R342>
i=i+1;
Rnames{ 344} = 'RCO3 + NO3 = ETHP + NO2 ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='RCO3';Gstr{i,   2}='NO3';
fRCO3(i)=fRCO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fETHP(i)=fETHP(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 345, <R343>
i=i+1;
Rnames{ 345} = 'ACTP + NO3 = ACO3 + NO2 + HCHO ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='ACTP';Gstr{i,   2}='NO3';
fACTP(i)=fACTP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fACO3(i)=fACO3(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  1.000;

% 346, <R344>
i=i+1;
Rnames{ 346} = 'MEKP + NO3 = 0.67000*HO2 + NO2 +  0.33000*HCHO +  0.67000*DCB1 ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='MEKP';Gstr{i,   2}='NO3';
fMEKP(i)=fMEKP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.670;fNO2(i)=fNO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  0.330;fDCB1(i)=fDCB1(i)+  0.670;

% 347, <R345>
i=i+1;
Rnames{ 347} = 'KETP + NO3 = HO2 + NO2 + DCB1 ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='KETP';Gstr{i,   2}='NO3';
fKETP(i)=fKETP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;fDCB1(i)=fDCB1(i)+  1.000;

% 348, <R346>
i=i+1;
Rnames{ 348} = 'MACP + NO3 = HCHO +  0.53800*ACO3 + CO + NO2 ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='MACP';Gstr{i,   2}='NO3';
fMACP(i)=fMACP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHCHO(i)=fHCHO(i)+  1.000;fACO3(i)=fACO3(i)+  0.538;fCO(i)=fCO(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 349, <R347>
i=i+1;
Rnames{ 349} = 'MCP + NO3 = NO2 + HO2 + HCHO + HKET ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='MCP';Gstr{i,   2}='NO3';
fMCP(i)=fMCP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fHO2(i)=fHO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  1.000;fHKET(i)=fHKET(i)+  1.000;

% 350, <R348>
i=i+1;
Rnames{ 350} = 'MVKP + NO3 = 0.30000*HO2 +  0.70000*ACO3 +  0.70000*XO2 + NO2 +  0.30000*HCHO +  0.70000*ALD +  0.30000*MGLY ';
k(:,i) = (  2.5000E-12 ); 
Gstr{i,   1}='MVKP';Gstr{i,   2}='NO3';
fMVKP(i)=fMVKP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.300;fACO3(i)=fACO3(i)+  0.700;fXO2(i)=fXO2(i)+  0.700;fNO2(i)=fNO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  0.300;fALD(i)=fALD(i)+  0.700;fMGLY(i)=fMGLY(i)+  0.300;

% 351, <R349>
i=i+1;
Rnames{ 351} = 'UALP + NO3 = HO2 + NO2 +  0.61000*CO +  0.03000*HCHO +  0.27000*ALD +  0.70000*KET +  0.18000*GLY +  0.21000*MGLY ';
k(:,i) = (  2.5000E-12 ); 
Gstr{i,   1}='UALP';Gstr{i,   2}='NO3';
fUALP(i)=fUALP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;fCO(i)=fCO(i)+  0.610;fHCHO(i)=fHCHO(i)+  0.030;fALD(i)=fALD(i)+  0.270;fKET(i)=fKET(i)+  0.700;fGLY(i)=fGLY(i)+  0.180;fMGLY(i)=fMGLY(i)+  0.210;

% 352, <R350>
i=i+1;
Rnames{ 352} = 'BALP + NO3 = BAL1 + NO2 ';
k(:,i) = (  2.5000E-12 ); 
Gstr{i,   1}='BALP';Gstr{i,   2}='NO3';
fBALP(i)=fBALP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fBAL1(i)=fBAL1(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 353, <R351>
i=i+1;
Rnames{ 353} = 'BAL1 + NO3 = BAL2 + NO2 ';
k(:,i) = (  2.5000E-12 ); 
Gstr{i,   1}='BAL1';Gstr{i,   2}='NO3';
fBAL1(i)=fBAL1(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fBAL2(i)=fBAL2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 354, <R352>
i=i+1;
Rnames{ 354} = 'ADDC + NO3 = HO2 + NO2 +  0.32000*HKET +  0.68000*GLY +  0.68000*OP2 ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='ADDC';Gstr{i,   2}='NO3';
fADDC(i)=fADDC(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;fHKET(i)=fHKET(i)+  0.320;fGLY(i)=fGLY(i)+  0.680;fOP2(i)=fOP2(i)+  0.680;

% 355, <R353>
i=i+1;
Rnames{ 355} = 'MCTP + NO3 = NO2 + MCTO ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='MCTP';Gstr{i,   2}='NO3';
fMCTP(i)=fMCTP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fMCTO(i)=fMCTO(i)+  1.000;

% 356, <R354>
i=i+1;
Rnames{ 356} = 'ORAP + NO3 = NO2 + GLY + HO2 ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='ORAP';Gstr{i,   2}='NO3';
fORAP(i)=fORAP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fGLY(i)=fGLY(i)+  1.000;fHO2(i)=fHO2(i)+  1.000;

% 357, <R355>
i=i+1;
Rnames{ 357} = 'OLNN + NO3 = HO2 + NO2 + ONIT ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='OLNN';Gstr{i,   2}='NO3';
fOLNN(i)=fOLNN(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;fONIT(i)=fONIT(i)+  1.000;

% 358, <R356>
i=i+1;
Rnames{ 358} = 'OLND + NO3 = 2.00000*NO2 +  0.28700*HCHO +  1.24000*ALD +  0.46400*KET ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='OLND';Gstr{i,   2}='NO3';
fOLND(i)=fOLND(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  2.000;fHCHO(i)=fHCHO(i)+  0.287;fALD(i)=fALD(i)+  1.240;fKET(i)=fKET(i)+  0.464;

% 359, <R357>
i=i+1;
Rnames{ 359} = 'ADCN + NO3 = 2.00000*NO2 + GLY + OP2 ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='ADCN';Gstr{i,   2}='NO3';
fADCN(i)=fADCN(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  2.000;fGLY(i)=fGLY(i)+  1.000;fOP2(i)=fOP2(i)+  1.000;

% 360, <R358>
i=i+1;
Rnames{ 360} = 'OLNN + OLNN = HO2 +  2.00000*ONIT ';
k(:,i) = (  7.0000E-14.*exp(  1.0000E+03./T) ); 
Gstr{i,   1}='OLNN';Gstr{i,   2}='OLNN';
fOLNN(i)=fOLNN(i)-1.0;fOLNN(i)=fOLNN(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fONIT(i)=fONIT(i)+  2.000;

% 361, <R359>
i=i+1;
Rnames{ 361} = 'OLNN + OLND = 0.50000*HO2 +  0.50000*NO2 +  0.20200*HCHO +  0.64000*ALD +  0.14900*KET +  1.50000*ONIT ';
k(:,i) = (  4.2500E-14.*exp(  1.0000E+03./T) ); 
Gstr{i,   1}='OLNN';Gstr{i,   2}='OLND';
fOLNN(i)=fOLNN(i)-1.0;fOLND(i)=fOLND(i)-1.0;
fHO2(i)=fHO2(i)+  0.500;fNO2(i)=fNO2(i)+  0.500;fHCHO(i)=fHCHO(i)+  0.202;fALD(i)=fALD(i)+  0.640;fKET(i)=fKET(i)+  0.149;fONIT(i)=fONIT(i)+  1.500;

% 362, <R360>
i=i+1;
Rnames{ 362} = 'OLND + OLND = NO2 +  0.50400*HCHO +  1.21000*ALD +  0.28500*KET + ONIT ';
k(:,i) = (  2.9600E-14.*exp(  1.0000E+03./T) ); 
Gstr{i,   1}='OLND';Gstr{i,   2}='OLND';
fOLND(i)=fOLND(i)-1.0;fOLND(i)=fOLND(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fHCHO(i)=fHCHO(i)+  0.504;fALD(i)=fALD(i)+  1.210;fKET(i)=fKET(i)+  0.285;fONIT(i)=fONIT(i)+  1.000;

% 363, <R361>
i=i+1;
Rnames{ 363} = 'XO2 + NO3 = NO2 ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='XO2';Gstr{i,   2}='NO3';
fXO2(i)=fXO2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;

% 364, <R362>
i=i+1;
Rnames{ 364} = 'XO2 + RCO3 = ETHP ';
k(:,i) = (  2.5000E-12.*exp(  5.0000E+02./T) ); 
Gstr{i,   1}='XO2';Gstr{i,   2}='RCO3';
fXO2(i)=fXO2(i)-1.0;fRCO3(i)=fRCO3(i)-1.0;
fETHP(i)=fETHP(i)+  1.000;

% 365, <R363>
i=i+1;
Rnames{ 365} = 'XO2 + XO2 =';
k(:,i) = (  7.1300E-17.*exp(  2.9500E+03./T) ); 
Gstr{i,   1}='XO2';Gstr{i,   2}='XO2';
fXO2(i)=fXO2(i)-1.0;fXO2(i)=fXO2(i)-1.0;


% 366, <TRP41>
i=i+1;
Rnames{ 366} = 'APIP2 + APIP1 = 0.96000*HOM +  0.48000*ROH +  0.48000*PINAL +  0.48000*HO +  0.48000*HO2 +  0.04000*ELHOM ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='APIP2';Gstr{i,   2}='APIP1';
fAPIP2(i)=fAPIP2(i)-1.0;fAPIP1(i)=fAPIP1(i)-1.0;
fHOM(i)=fHOM(i)+  0.960;fROH(i)=fROH(i)+  0.480;fPINAL(i)=fPINAL(i)+  0.480;fHO(i)=fHO(i)+  0.480;fHO2(i)=fHO2(i)+  0.480;fELHOM(i)=fELHOM(i)+  0.040;

% 367, <TRP42>
i=i+1;
Rnames{ 367} = 'APIP2 + LIMP1 = 0.96000*HOM +  0.48000*ROH +  0.48000*LIMAL +  0.48000*HO +  0.48000*HO2 +  0.04000*ELHOM ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='APIP2';Gstr{i,   2}='LIMP1';
fAPIP2(i)=fAPIP2(i)-1.0;fLIMP1(i)=fLIMP1(i)-1.0;
fHOM(i)=fHOM(i)+  0.960;fROH(i)=fROH(i)+  0.480;fLIMAL(i)=fLIMAL(i)+  0.480;fHO(i)=fHO(i)+  0.480;fHO2(i)=fHO2(i)+  0.480;fELHOM(i)=fELHOM(i)+  0.040;

% 368, <TRP43>
i=i+1;
Rnames{ 368} = 'APIP2 + ISOP = 0.96000*HOM +  0.48000*ROH +  0.48000*HCHO +  0.48000*MVK +  0.48000*HO +  0.48000*HO2 +  0.04000*ELHOM ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='APIP2';Gstr{i,   2}='ISOP';
fAPIP2(i)=fAPIP2(i)-1.0;fISOP(i)=fISOP(i)-1.0;
fHOM(i)=fHOM(i)+  0.960;fROH(i)=fROH(i)+  0.480;fHCHO(i)=fHCHO(i)+  0.480;fMVK(i)=fMVK(i)+  0.480;fHO(i)=fHO(i)+  0.480;fHO2(i)=fHO2(i)+  0.480;fELHOM(i)=fELHOM(i)+  0.040;

% 369, <TRP44>
i=i+1;
Rnames{ 369} = 'LIMP2 + APIP1 = 0.96000*HOM +  0.48000*ROH +  0.48000*PINAL +  0.48000*HO +  0.48000*HO2 +  0.04000*ELHOM ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='LIMP2';Gstr{i,   2}='APIP1';
fLIMP2(i)=fLIMP2(i)-1.0;fAPIP1(i)=fAPIP1(i)-1.0;
fHOM(i)=fHOM(i)+  0.960;fROH(i)=fROH(i)+  0.480;fPINAL(i)=fPINAL(i)+  0.480;fHO(i)=fHO(i)+  0.480;fHO2(i)=fHO2(i)+  0.480;fELHOM(i)=fELHOM(i)+  0.040;

% 370, <TRP45>
i=i+1;
Rnames{ 370} = 'LIMP2 + LIMP1 = 0.96000*HOM +  0.48000*ROH +  0.48000*LIMAL +  0.48000*HO +  0.48000*HO2 +  0.04000*ELHOM ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='LIMP2';Gstr{i,   2}='LIMP1';
fLIMP2(i)=fLIMP2(i)-1.0;fLIMP1(i)=fLIMP1(i)-1.0;
fHOM(i)=fHOM(i)+  0.960;fROH(i)=fROH(i)+  0.480;fLIMAL(i)=fLIMAL(i)+  0.480;fHO(i)=fHO(i)+  0.480;fHO2(i)=fHO2(i)+  0.480;fELHOM(i)=fELHOM(i)+  0.040;

% 371, <TRP46>
i=i+1;
Rnames{ 371} = 'LIMP2 + ISOP = 0.96000*HOM +  0.48000*ROH +  0.48000*HCHO +  0.48000*MVK +  0.48000*HO +  0.48000*HO2 +  0.04000*ELHOM ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='LIMP2';Gstr{i,   2}='ISOP';
fLIMP2(i)=fLIMP2(i)-1.0;fISOP(i)=fISOP(i)-1.0;
fHOM(i)=fHOM(i)+  0.960;fROH(i)=fROH(i)+  0.480;fHCHO(i)=fHCHO(i)+  0.480;fMVK(i)=fMVK(i)+  0.480;fHO(i)=fHO(i)+  0.480;fHO2(i)=fHO2(i)+  0.480;fELHOM(i)=fELHOM(i)+  0.040;

% 372, <TRP47>
i=i+1;
Rnames{ 372} = 'APINP2 + APIP1 = 0.96000*HOM +  0.48000*ROH +  0.48000*PINAL +  0.48000*NO2 +  0.48000*HO2 +  0.04000*ELHOM ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='APINP2';Gstr{i,   2}='APIP1';
fAPINP2(i)=fAPINP2(i)-1.0;fAPIP1(i)=fAPIP1(i)-1.0;
fHOM(i)=fHOM(i)+  0.960;fROH(i)=fROH(i)+  0.480;fPINAL(i)=fPINAL(i)+  0.480;fNO2(i)=fNO2(i)+  0.480;fHO2(i)=fHO2(i)+  0.480;fELHOM(i)=fELHOM(i)+  0.040;

% 373, <TRP48>
i=i+1;
Rnames{ 373} = 'APINP2 + LIMP1 = 0.96000*HOM +  0.48000*ROH +  0.48000*LIMAL +  0.48000*NO2 +  0.48000*HO2 +  0.04000*ELHOM ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='APINP2';Gstr{i,   2}='LIMP1';
fAPINP2(i)=fAPINP2(i)-1.0;fLIMP1(i)=fLIMP1(i)-1.0;
fHOM(i)=fHOM(i)+  0.960;fROH(i)=fROH(i)+  0.480;fLIMAL(i)=fLIMAL(i)+  0.480;fNO2(i)=fNO2(i)+  0.480;fHO2(i)=fHO2(i)+  0.480;fELHOM(i)=fELHOM(i)+  0.040;

% 374, <TRP49>
i=i+1;
Rnames{ 374} = 'APINP2 + ISOP = 0.96000*HOM +  0.48000*ROH +  0.48000*HCHO +  0.48000*MVK +  0.48000*NO2 +  0.48000*HO2 +  0.04000*ELHOM ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='APINP2';Gstr{i,   2}='ISOP';
fAPINP2(i)=fAPINP2(i)-1.0;fISOP(i)=fISOP(i)-1.0;
fHOM(i)=fHOM(i)+  0.960;fROH(i)=fROH(i)+  0.480;fHCHO(i)=fHCHO(i)+  0.480;fMVK(i)=fMVK(i)+  0.480;fNO2(i)=fNO2(i)+  0.480;fHO2(i)=fHO2(i)+  0.480;fELHOM(i)=fELHOM(i)+  0.040;

% 375, <TRP50>
i=i+1;
Rnames{ 375} = 'LIMNP2 + APIP1 = 0.96000*HOM +  0.48000*ROH +  0.48000*PINAL +  0.48000*NO2 +  0.48000*HO2 +  0.04000*ELHOM ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='LIMNP2';Gstr{i,   2}='APIP1';
fLIMNP2(i)=fLIMNP2(i)-1.0;fAPIP1(i)=fAPIP1(i)-1.0;
fHOM(i)=fHOM(i)+  0.960;fROH(i)=fROH(i)+  0.480;fPINAL(i)=fPINAL(i)+  0.480;fNO2(i)=fNO2(i)+  0.480;fHO2(i)=fHO2(i)+  0.480;fELHOM(i)=fELHOM(i)+  0.040;

% 376, <TRP51>
i=i+1;
Rnames{ 376} = 'LIMNP2 + LIMP1 = 0.96000*HOM +  0.48000*ROH +  0.48000*LIMAL +  0.48000*NO2 +  0.48000*HO2 +  0.04000*ELHOM ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='LIMNP2';Gstr{i,   2}='LIMP1';
fLIMNP2(i)=fLIMNP2(i)-1.0;fLIMP1(i)=fLIMP1(i)-1.0;
fHOM(i)=fHOM(i)+  0.960;fROH(i)=fROH(i)+  0.480;fLIMAL(i)=fLIMAL(i)+  0.480;fNO2(i)=fNO2(i)+  0.480;fHO2(i)=fHO2(i)+  0.480;fELHOM(i)=fELHOM(i)+  0.040;

% 377, <TRP52>
i=i+1;
Rnames{ 377} = 'LIMNP2 + ISOP = 0.96000*HOM +  0.48000*ROH +  0.48000*HCHO +  0.48000*MVK +  0.48000*NO2 +  0.48000*HO2 +  0.04000*ELHOM ';
k(:,i) = (  1.0000E-10 ); 
Gstr{i,   1}='LIMNP2';Gstr{i,   2}='ISOP';
fLIMNP2(i)=fLIMNP2(i)-1.0;fISOP(i)=fISOP(i)-1.0;
fHOM(i)=fHOM(i)+  0.960;fROH(i)=fROH(i)+  0.480;fHCHO(i)=fHCHO(i)+  0.480;fMVK(i)=fMVK(i)+  0.480;fNO2(i)=fNO2(i)+  0.480;fHO2(i)=fHO2(i)+  0.480;fELHOM(i)=fELHOM(i)+  0.040;

% 378, <SA14>
i=i+1;
Rnames{ 378} = 'IEPOX + HO = HO ';
k(:,i) = (  5.7800E-11.*exp( -4.0000E+02./T) ); 
Gstr{i,   1}='IEPOX';Gstr{i,   2}='HO';
fIEPOX(i)=fIEPOX(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  1.000;

% 379, <R001c>
i=i+1;
Rnames{ 379} = 'VROCIOXY + HO = 0.85200*ETHP +  0.14900*ASOATJ ';
k(:,i) = (  6.8900E-12 ); 
Gstr{i,   1}='VROCIOXY';Gstr{i,   2}='HO';
fVROCIOXY(i)=fVROCIOXY(i)-1.0;fHO(i)=fHO(i)-1.0;
fETHP(i)=fETHP(i)+  0.852;fASOATJ(i)=fASOATJ(i)+  0.149;

% 380, <R002c>
i=i+1;
Rnames{ 380} = 'SLOWROC + HO = ETHP +  0.00101*ASOATJ ';
k(:,i) = (  6.5500E-14 ); 
Gstr{i,   1}='SLOWROC';Gstr{i,   2}='HO';
fSLOWROC(i)=fSLOWROC(i)-1.0;fHO(i)=fHO(i)-1.0;
fETHP(i)=fETHP(i)+  1.000;fASOATJ(i)=fASOATJ(i)+  0.001;

% 381, <T17>
i=i+1;
Rnames{ 381} = 'ACRO + HO = 0.57000*MACP +  0.43000*MCP ';
k(:,i) = (  8.0000E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='ACRO';Gstr{i,   2}='HO';
fACRO(i)=fACRO(i)-1.0;fHO(i)=fHO(i)-1.0;
fMACP(i)=fMACP(i)+  0.570;fMCP(i)=fMCP(i)+  0.430;

% 382, <T18>
i=i+1;
Rnames{ 382} = 'ACRO + O3 = 0.84000*CO +  0.56000*HO2 +  0.28000*HO +  0.72000*HCHO +  0.62000*GLY ';
k(:,i) = (  2.9000E-19 ); 
Gstr{i,   1}='ACRO';Gstr{i,   2}='O3';
fACRO(i)=fACRO(i)-1.0;fO3(i)=fO3(i)-1.0;
fCO(i)=fCO(i)+  0.840;fHO2(i)=fHO2(i)+  0.560;fHO(i)=fHO(i)+  0.280;fHCHO(i)=fHCHO(i)+  0.720;fGLY(i)=fGLY(i)+  0.620;

% 383, <T19>
i=i+1;
Rnames{ 383} = 'ACRO + NO3 = 0.68000*HCHO +  0.32000*MACP +  0.68000*XO2 +  0.68000*MGLY +  0.32000*HNO3 +  0.68000*NO2 ';
k(:,i) = (  3.4000E-15 ); 
Gstr{i,   1}='ACRO';Gstr{i,   2}='NO3';
fACRO(i)=fACRO(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHCHO(i)=fHCHO(i)+  0.680;fMACP(i)=fMACP(i)+  0.320;fXO2(i)=fXO2(i)+  0.680;fMGLY(i)=fMGLY(i)+  0.680;fHNO3(i)=fHNO3(i)+  0.320;fNO2(i)=fNO2(i)+  0.680;

% 384, <T20>
i=i+1;
Rnames{ 384} = 'ACRO = CO +  0.47700*HO2 +  0.25000*ETE +  0.35400*ACO3 +  0.20400*HO +  0.15000*HCHO +  0.02700*MO2 ';
k(:,i) = (JACRO_09 ); 
Gstr{i,   1}='ACRO';
fACRO(i)=fACRO(i)-1.0;
fCO(i)=fCO(i)+  1.000;fHO2(i)=fHO2(i)+  0.477;fETE(i)=fETE(i)+  0.250;fACO3(i)=fACO3(i)+  0.354;fHO(i)=fHO(i)+  0.204;fHCHO(i)=fHCHO(i)+  0.150;fMO2(i)=fMO2(i)+  0.027;

% 385, <T10>
i=i+1;
Rnames{ 385} = 'BDE13 + HO = 0.66700*BDE13P +  0.33300*UALD +  0.33300*HO2 ';
k(:,i) = (  1.4800E-11.*exp(  4.4800E+02./T) ); 
Gstr{i,   1}='BDE13';Gstr{i,   2}='HO';
fBDE13(i)=fBDE13(i)-1.0;fHO(i)=fHO(i)-1.0;
fBDE13P(i)=fBDE13P(i)+  0.667;fUALD(i)=fUALD(i)+  0.333;fHO2(i)=fHO2(i)+  0.333;

% 386, <T10a>
i=i+1;
Rnames{ 386} = 'BDE13P + NO = 0.96800*HO2 +  0.96800*NO2 +  0.89500*ACRO +  0.89500*HCHO +  0.07200*FURAN +  0.03200*ONIT ';
k(:,i) = (  9.0500E-12 ); 
Gstr{i,   1}='BDE13P';Gstr{i,   2}='NO';
fBDE13P(i)=fBDE13P(i)-1.0;fNO(i)=fNO(i)-1.0;
fHO2(i)=fHO2(i)+  0.968;fNO2(i)=fNO2(i)+  0.968;fACRO(i)=fACRO(i)+  0.895;fHCHO(i)=fHCHO(i)+  0.895;fFURAN(i)=fFURAN(i)+  0.072;fONIT(i)=fONIT(i)+  0.032;

% 387, <T10b>
i=i+1;
Rnames{ 387} = 'BDE13P + NO3 = HO2 + NO2 +  0.92500*ACRO +  0.92500*HCHO +  0.07500*FURAN ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='BDE13P';Gstr{i,   2}='NO3';
fBDE13P(i)=fBDE13P(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;fACRO(i)=fACRO(i)+  0.925;fHCHO(i)=fHCHO(i)+  0.925;fFURAN(i)=fFURAN(i)+  0.075;

% 388, <T10c>
i=i+1;
Rnames{ 388} = 'BDE13P + HO2 = OP2 ';
k(:,i) = (  1.6100E-11 ); 
Gstr{i,   1}='BDE13P';Gstr{i,   2}='HO2';
fBDE13P(i)=fBDE13P(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  1.000;

% 389, <T10d>
i=i+1;
Rnames{ 389} = 'BDE13P + MO2 = 0.32000*MOH +  1.14300*HCHO +  0.87000*HO2 +  0.46300*ACRO +  0.25000*OLT +  0.23100*MVK +  0.03700*FURAN +  0.01900*UALD ';
k(:,i) = (  2.3900E-12 ); 
Gstr{i,   1}='BDE13P';Gstr{i,   2}='MO2';
fBDE13P(i)=fBDE13P(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fMOH(i)=fMOH(i)+  0.320;fHCHO(i)=fHCHO(i)+  1.143;fHO2(i)=fHO2(i)+  0.870;fACRO(i)=fACRO(i)+  0.463;fOLT(i)=fOLT(i)+  0.250;fMVK(i)=fMVK(i)+  0.231;fFURAN(i)=fFURAN(i)+  0.037;fUALD(i)=fUALD(i)+  0.019;

% 390, <T10e>
i=i+1;
Rnames{ 390} = 'BDE13P + ACO3 = 0.70000*MO2 +  0.30000*ORA2 +  0.80000*HO2 +  0.74000*ACRO +  0.74000*HCHO +  0.18500*MVK +  0.06000*FURAN +  0.01500*UALD ';
k(:,i) = (  1.3700E-11 ); 
Gstr{i,   1}='BDE13P';Gstr{i,   2}='ACO3';
fBDE13P(i)=fBDE13P(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fMO2(i)=fMO2(i)+  0.700;fORA2(i)=fORA2(i)+  0.300;fHO2(i)=fHO2(i)+  0.800;fACRO(i)=fACRO(i)+  0.740;fHCHO(i)=fHCHO(i)+  0.740;fMVK(i)=fMVK(i)+  0.185;fFURAN(i)=fFURAN(i)+  0.060;fUALD(i)=fUALD(i)+  0.015;

% 391, <T11>
i=i+1;
Rnames{ 391} = 'BDE13 + O3 = 0.62000*ACRO +  0.63000*CO +  0.42000*HO2 +  0.08000*HO +  0.83000*HCHO +  0.17000*ETE ';
k(:,i) = (  1.3400E-14.*exp( -2.2830E+03./T) ); 
Gstr{i,   1}='BDE13';Gstr{i,   2}='O3';
fBDE13(i)=fBDE13(i)-1.0;fO3(i)=fO3(i)-1.0;
fACRO(i)=fACRO(i)+  0.620;fCO(i)=fCO(i)+  0.630;fHO2(i)=fHO2(i)+  0.420;fHO(i)=fHO(i)+  0.080;fHCHO(i)=fHCHO(i)+  0.830;fETE(i)=fETE(i)+  0.170;

% 392, <T12>
i=i+1;
Rnames{ 392} = 'BDE13 + NO3 = 0.90000*OLNN +  0.10000*OLND +  0.90000*MACR ';
k(:,i) = (  1.0000E-13 ); 
Gstr{i,   1}='BDE13';Gstr{i,   2}='NO3';
fBDE13(i)=fBDE13(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fOLNN(i)=fOLNN(i)+  0.900;fOLND(i)=fOLND(i)+  0.100;fMACR(i)=fMACR(i)+  0.900;

% 393, <R003c>
i=i+1;
Rnames{ 393} = 'FURAN + HO = 0.49000*DCB1 +  0.49000*HO2 +  0.51000*FURANO2 ';
k(:,i) = (  5.0100E-11 ); 
Gstr{i,   1}='FURAN';Gstr{i,   2}='HO';
fFURAN(i)=fFURAN(i)-1.0;fHO(i)=fHO(i)-1.0;
fDCB1(i)=fDCB1(i)+  0.490;fHO2(i)=fHO2(i)+  0.490;fFURANO2(i)=fFURANO2(i)+  0.510;

% 394, <R004c>
i=i+1;
Rnames{ 394} = 'FURANO2 + NO = 0.08000*ONIT +  0.92000*NO2 +  0.92000*FURANONE +  0.75000*HO2 +  0.17000*MO2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='FURANO2';Gstr{i,   2}='NO';
fFURANO2(i)=fFURANO2(i)-1.0;fNO(i)=fNO(i)-1.0;
fONIT(i)=fONIT(i)+  0.080;fNO2(i)=fNO2(i)+  0.920;fFURANONE(i)=fFURANONE(i)+  0.920;fHO2(i)=fHO2(i)+  0.750;fMO2(i)=fMO2(i)+  0.170;

% 395, <R005c>
i=i+1;
Rnames{ 395} = 'FURANO2 + HO2 = 0.60000*OP2 +  0.40000*FURANONE +  0.40000*HO +  0.32000*HO2 +  0.08000*MO2 ';
k(:,i) = (  3.7500E-13.*exp(  9.8000E+02./T) ); 
Gstr{i,   1}='FURANO2';Gstr{i,   2}='HO2';
fFURANO2(i)=fFURANO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  0.600;fFURANONE(i)=fFURANONE(i)+  0.400;fHO(i)=fHO(i)+  0.400;fHO2(i)=fHO2(i)+  0.320;fMO2(i)=fMO2(i)+  0.080;

% 396, <R006c>
i=i+1;
Rnames{ 396} = 'FURANONE + HO = 0.65000*KET +  0.31000*GLY +  0.66000*HO2 +  0.34000*MO2 +  0.43000*CO +  0.04000*ASOATJ ';
k(:,i) = (  4.4000E-11 ); 
Gstr{i,   1}='FURANONE';Gstr{i,   2}='HO';
fFURANONE(i)=fFURANONE(i)-1.0;fHO(i)=fHO(i)-1.0;
fKET(i)=fKET(i)+  0.650;fGLY(i)=fGLY(i)+  0.310;fHO2(i)=fHO2(i)+  0.660;fMO2(i)=fMO2(i)+  0.340;fCO(i)=fCO(i)+  0.430;fASOATJ(i)=fASOATJ(i)+  0.040;

% 397, <R007c>
i=i+1;
Rnames{ 397} = 'FURAN + O3 = 0.02000*HO + ALD ';
k(:,i) = (  3.4300E-17 ); 
Gstr{i,   1}='FURAN';Gstr{i,   2}='O3';
fFURAN(i)=fFURAN(i)-1.0;fO3(i)=fO3(i)-1.0;
fHO(i)=fHO(i)+  0.020;fALD(i)=fALD(i)+  1.000;

% 398, <R008c>
i=i+1;
Rnames{ 398} = 'FURAN + NO3 = NO2 +  0.80000*DCB1 +  0.20000*DCB3 ';
k(:,i) = (  8.9900E-12 ); 
Gstr{i,   1}='FURAN';Gstr{i,   2}='NO3';
fFURAN(i)=fFURAN(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fDCB1(i)=fDCB1(i)+  0.800;fDCB3(i)=fDCB3(i)+  0.200;

% 399, <R010c>
i=i+1;
Rnames{ 399} = 'PROG + HO = 0.61300*HKET +  0.38700*ALD + HO2 ';
k(:,i) = (  1.2000E-11 ); 
Gstr{i,   1}='PROG';Gstr{i,   2}='HO';
fPROG(i)=fPROG(i)-1.0;fHO(i)=fHO(i)-1.0;
fHKET(i)=fHKET(i)+  0.613;fALD(i)=fALD(i)+  0.387;fHO2(i)=fHO2(i)+  1.000;

% 400, <R011c>
i=i+1;
Rnames{ 400} = 'SESQ + NO3 = SESQNRO2 ';
k(:,i) = (  1.9000E-11 ); 
Gstr{i,   1}='SESQ';Gstr{i,   2}='NO3';
fSESQ(i)=fSESQ(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fSESQNRO2(i)=fSESQNRO2(i)+  1.000;

% 401, <R012c>
i=i+1;
Rnames{ 401} = 'SESQNRO2 + HO2 = VROCP0OXY4 ';
k(:,i) = (  2.8400E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='SESQNRO2';Gstr{i,   2}='HO2';
fSESQNRO2(i)=fSESQNRO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fVROCP0OXY4(i)=fVROCP0OXY4(i)+  1.000;

% 402, <R013c>
i=i+1;
Rnames{ 402} = 'SESQNRO2 + NO = VROCP3OXY2 +  2.00000*NO2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='SESQNRO2';Gstr{i,   2}='NO';
fSESQNRO2(i)=fSESQNRO2(i)-1.0;fNO(i)=fNO(i)-1.0;
fVROCP3OXY2(i)=fVROCP3OXY2(i)+  1.000;fNO2(i)=fNO2(i)+  2.000;

% 403, <R014c>
i=i+1;
Rnames{ 403} = 'SESQNRO2 + NO3 = VROCP3OXY2 +  2.00000*NO2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='SESQNRO2';Gstr{i,   2}='NO3';
fSESQNRO2(i)=fSESQNRO2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fVROCP3OXY2(i)=fVROCP3OXY2(i)+  1.000;fNO2(i)=fNO2(i)+  2.000;

% 404, <R015c>
i=i+1;
Rnames{ 404} = 'SESQ + O3 = 0.98200*VROCP3OXY2 +  0.01800*VROCN2OXY2 ';
k(:,i) = (  1.2000E-14 ); 
Gstr{i,   1}='SESQ';Gstr{i,   2}='O3';
fSESQ(i)=fSESQ(i)-1.0;fO3(i)=fO3(i)-1.0;
fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.982;fVROCN2OXY2(i)=fVROCN2OXY2(i)+  0.018;

% 405, <R016c>
i=i+1;
Rnames{ 405} = 'SESQ + HO = SESQRO2 ';
k(:,i) = (  1.9700E-10 ); 
Gstr{i,   1}='SESQ';Gstr{i,   2}='HO';
fSESQ(i)=fSESQ(i)-1.0;fHO(i)=fHO(i)-1.0;
fSESQRO2(i)=fSESQRO2(i)+  1.000;

% 406, <R017c>
i=i+1;
Rnames{ 406} = 'SESQRO2 + HO2 = VROCP0OXY2 ';
k(:,i) = (  2.8400E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='SESQRO2';Gstr{i,   2}='HO2';
fSESQRO2(i)=fSESQRO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fVROCP0OXY2(i)=fVROCP0OXY2(i)+  1.000;

% 407, <R019c>
i=i+1;
Rnames{ 407} = 'SESQRO2 + NO3 = VROCP3OXY2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='SESQRO2';Gstr{i,   2}='NO3';
fSESQRO2(i)=fSESQRO2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fVROCP3OXY2(i)=fVROCP3OXY2(i)+  1.000;

% 408, <R020c>
i=i+1;
Rnames{ 408} = 'SESQRO2 + NO = 0.24700*VROCP1OXY3 +  0.75300*VROCP3OXY2 +  0.75300*NO2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='SESQRO2';Gstr{i,   2}='NO';
fSESQRO2(i)=fSESQRO2(i)-1.0;fNO(i)=fNO(i)-1.0;
fVROCP1OXY3(i)=fVROCP1OXY3(i)+  0.247;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.753;fNO2(i)=fNO2(i)+  0.753;

% 409, <HET_GLY>
i=i+1;
Rnames{ 409} = 'GLY = AGLYJ ';
k(:,i) = (K_HETERO_GLY ); 
Gstr{i,   1}='GLY';
fGLY(i)=fGLY(i)-1.0;
fAGLYJ(i)=fAGLYJ(i)+  1.000;

% 410, <HET_MGLY>
i=i+1;
Rnames{ 410} = 'MGLY = AGLYJ ';
k(:,i) = (K_HETERO_MGLY ); 
Gstr{i,   1}='MGLY';
fMGLY(i)=fMGLY(i)-1.0;
fAGLYJ(i)=fAGLYJ(i)+  1.000;

% 411, <HET_ISON>
i=i+1;
Rnames{ 411} = 'ISON = AISONJ ';
k(:,i) = (  6.5000E-07 ); 
Gstr{i,   1}='ISON';
fISON(i)=fISON(i)-1.0;
fAISONJ(i)=fAISONJ(i)+  1.000;

% 412, <HET_TRPN>
i=i+1;
Rnames{ 412} = 'TRPN = ATRPNJ ';
k(:,i) = (  1.3000E-06 ); 
Gstr{i,   1}='TRPN';
fTRPN(i)=fTRPN(i)-1.0;
fATRPNJ(i)=fATRPNJ(i)+  1.000;

% 413, <HET_N2O5>
i=i+1;
Rnames{ 413} = 'N2O5 = 2.00000*HNO3 ';
k(:,i) = (K_HETERO_N2O5IJ ); 
Gstr{i,   1}='N2O5';
fN2O5(i)=fN2O5(i)-1.0;
fHNO3(i)=fHNO3(i)+  2.000;

% 414, <HET_N02>
i=i+1;
Rnames{ 414} = 'NO2 = 0.50000*HONO +  0.50000*HNO3 ';
k(:,i) = (K_HETERO_NO2 ); 
Gstr{i,   1}='NO2';
fNO2(i)=fNO2(i)-1.0;
fHONO(i)=fHONO(i)+  0.500;fHNO3(i)=fHNO3(i)+  0.500;

% 415, <HAL_Ozone>
i=i+1;
Rnames{ 415} = 'O3 =';
ILLUMINATED =  ( SZA > 0.0 );
OPEN_OCEAN  = SZA;
OPEN_OCEAN  = true;
Patm = 0.001.*P;
a =  6.701E-11.*exp( 1.074E+01.*Patm) + 3.415E-08.*exp(-6.713E-01.*Patm);
b =  2.000E-06;
a(a>b) = b;
k(:,i) = a.*ILLUMINATED.*OPEN_OCEAN;
Gstr{i,   1}='O3';
fO3(i)=fO3(i)-1.0;


% 416, <HET_IEPOX>
i=i+1;
Rnames{ 416} = 'IEPOX = IEPOXP ';
k(:,i) = (K_HETERO_IEPOX ); 
Gstr{i,   1}='IEPOX';
fIEPOX(i)=fIEPOX(i)-1.0;
fIEPOXP(i)=fIEPOXP(i)+  1.000;

% 417, <HET_ISO3TET>
i=i+1;
Rnames{ 417} = 'IEPOXP = AISO3NOSJ ';
k(:,i) = (K_HETERO_ISO3NOSJ ); 
Gstr{i,   1}='IEPOXP';
fIEPOXP(i)=fIEPOXP(i)-1.0;
fAISO3NOSJ(i)=fAISO3NOSJ(i)+  1.000;

% 418, <HET_IEPOXOS>
i=i+1;
Rnames{ 418} = 'IEPOXP + ASO4J = AISO3OSJ ';
k(:,i) = (K_HETERO_ISO3OSJ ); 
Gstr{i,   1}='IEPOXP';Gstr{i,   2}='ASO4J';
fIEPOXP(i)=fIEPOXP(i)-1.0;fASO4J(i)=fASO4J(i)-1.0;
fAISO3OSJ(i)=fAISO3OSJ(i)+  1.000;

% 419, <ROCALK1c>
i=i+1;
Rnames{ 419} = 'VROCP6ALK + HO = VROCP6ALKP ';
k(:,i) = (  1.5300E-11 ); 
Gstr{i,   1}='VROCP6ALK';Gstr{i,   2}='HO';
fVROCP6ALK(i)=fVROCP6ALK(i)-1.0;fHO(i)=fHO(i)-1.0;
fVROCP6ALKP(i)=fVROCP6ALKP(i)+  1.000;

% 420, <ROCALK2c>
i=i+1;
Rnames{ 420} = 'VROCP5ALK + HO = VROCP5ALKP ';
k(:,i) = (  1.6800E-11 ); 
Gstr{i,   1}='VROCP5ALK';Gstr{i,   2}='HO';
fVROCP5ALK(i)=fVROCP5ALK(i)-1.0;fHO(i)=fHO(i)-1.0;
fVROCP5ALKP(i)=fVROCP5ALKP(i)+  1.000;

% 421, <ROCALK3c>
i=i+1;
Rnames{ 421} = 'VROCP4ALK + HO = VROCP4ALKP ';
k(:,i) = (  2.2400E-11 ); 
Gstr{i,   1}='VROCP4ALK';Gstr{i,   2}='HO';
fVROCP4ALK(i)=fVROCP4ALK(i)-1.0;fHO(i)=fHO(i)-1.0;
fVROCP4ALKP(i)=fVROCP4ALKP(i)+  1.000;

% 422, <ROCALK4c>
i=i+1;
Rnames{ 422} = 'VROCP3ALK + HO = VROCP3ALKP ';
k(:,i) = (  2.6700E-11 ); 
Gstr{i,   1}='VROCP3ALK';Gstr{i,   2}='HO';
fVROCP3ALK(i)=fVROCP3ALK(i)-1.0;fHO(i)=fHO(i)-1.0;
fVROCP3ALKP(i)=fVROCP3ALKP(i)+  1.000;

% 423, <ROCALK5c>
i=i+1;
Rnames{ 423} = 'VROCP2ALK + HO = VROCP2ALKP ';
k(:,i) = (  3.0900E-11 ); 
Gstr{i,   1}='VROCP2ALK';Gstr{i,   2}='HO';
fVROCP2ALK(i)=fVROCP2ALK(i)-1.0;fHO(i)=fHO(i)-1.0;
fVROCP2ALKP(i)=fVROCP2ALKP(i)+  1.000;

% 424, <ROCALK6c>
i=i+1;
Rnames{ 424} = 'VROCP1ALK + HO = VROCP1ALKP ';
k(:,i) = (  3.3800E-11 ); 
Gstr{i,   1}='VROCP1ALK';Gstr{i,   2}='HO';
fVROCP1ALK(i)=fVROCP1ALK(i)-1.0;fHO(i)=fHO(i)-1.0;
fVROCP1ALKP(i)=fVROCP1ALKP(i)+  1.000;

% 425, <HC1001>
i=i+1;
Rnames{ 425} = 'HC10 + HO = HC10P ';
k(:,i) = (  1.1000E-11 ); 
Gstr{i,   1}='HC10';Gstr{i,   2}='HO';
fHC10(i)=fHC10(i)-1.0;fHO(i)=fHO(i)-1.0;
fHC10P(i)=fHC10P(i)+  1.000;

% 426, <ROCALK7c>
i=i+1;
Rnames{ 426} = 'VROCP6ALKP + NO = 0.72000*VROCP6ALKP2 +  0.28000*VROCP4OXY2 +  0.72000*NO2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='VROCP6ALKP';Gstr{i,   2}='NO';
fVROCP6ALKP(i)=fVROCP6ALKP(i)-1.0;fNO(i)=fNO(i)-1.0;
fVROCP6ALKP2(i)=fVROCP6ALKP2(i)+  0.720;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.280;fNO2(i)=fNO2(i)+  0.720;

% 427, <ROCALK8c>
i=i+1;
Rnames{ 427} = 'VROCP5ALKP + NO = 0.72000*VROCP5ALKP2 +  0.28000*VROCP3OXY2 +  0.72000*NO2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='VROCP5ALKP';Gstr{i,   2}='NO';
fVROCP5ALKP(i)=fVROCP5ALKP(i)-1.0;fNO(i)=fNO(i)-1.0;
fVROCP5ALKP2(i)=fVROCP5ALKP2(i)+  0.720;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.280;fNO2(i)=fNO2(i)+  0.720;

% 428, <ROCALK9c>
i=i+1;
Rnames{ 428} = 'VROCP4ALKP + NO = 0.72000*VROCP4ALKP2 +  0.28000*VROCP2OXY2 +  0.72000*NO2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='VROCP4ALKP';Gstr{i,   2}='NO';
fVROCP4ALKP(i)=fVROCP4ALKP(i)-1.0;fNO(i)=fNO(i)-1.0;
fVROCP4ALKP2(i)=fVROCP4ALKP2(i)+  0.720;fVROCP2OXY2(i)=fVROCP2OXY2(i)+  0.280;fNO2(i)=fNO2(i)+  0.720;

% 429, <ROCALK10c>
i=i+1;
Rnames{ 429} = 'VROCP3ALKP + NO = 0.72000*VROCP3ALKP2 +  0.28000*VROCP1OXY1 +  0.72000*NO2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='VROCP3ALKP';Gstr{i,   2}='NO';
fVROCP3ALKP(i)=fVROCP3ALKP(i)-1.0;fNO(i)=fNO(i)-1.0;
fVROCP3ALKP2(i)=fVROCP3ALKP2(i)+  0.720;fVROCP1OXY1(i)=fVROCP1OXY1(i)+  0.280;fNO2(i)=fNO2(i)+  0.720;

% 430, <ROCALK11c>
i=i+1;
Rnames{ 430} = 'VROCP2ALKP + NO = 0.72000*VROCP2ALKP2 +  0.28000*VROCP0OXY2 +  0.72000*NO2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='VROCP2ALKP';Gstr{i,   2}='NO';
fVROCP2ALKP(i)=fVROCP2ALKP(i)-1.0;fNO(i)=fNO(i)-1.0;
fVROCP2ALKP2(i)=fVROCP2ALKP2(i)+  0.720;fVROCP0OXY2(i)=fVROCP0OXY2(i)+  0.280;fNO2(i)=fNO2(i)+  0.720;

% 431, <ROCALK12c>
i=i+1;
Rnames{ 431} = 'VROCP1ALKP + NO = 0.72000*VROCP1ALKP2 +  0.28000*VROCN1OXY1 +  0.72000*NO2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='VROCP1ALKP';Gstr{i,   2}='NO';
fVROCP1ALKP(i)=fVROCP1ALKP(i)-1.0;fNO(i)=fNO(i)-1.0;
fVROCP1ALKP2(i)=fVROCP1ALKP2(i)+  0.720;fVROCN1OXY1(i)=fVROCN1OXY1(i)+  0.280;fNO2(i)=fNO2(i)+  0.720;

% 432, <HC1002>
i=i+1;
Rnames{ 432} = 'HC10P + NO = 0.74000*HC10P2 +  0.26000*ONIT +  0.74000*NO2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='HC10P';Gstr{i,   2}='NO';
fHC10P(i)=fHC10P(i)-1.0;fNO(i)=fNO(i)-1.0;
fHC10P2(i)=fHC10P2(i)+  0.740;fONIT(i)=fONIT(i)+  0.260;fNO2(i)=fNO2(i)+  0.740;

% 433, <ROCALK13c>
i=i+1;
Rnames{ 433} = 'VROCP6ALKP + NO3 = VROCP6ALKP2 + NO2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='VROCP6ALKP';Gstr{i,   2}='NO3';
fVROCP6ALKP(i)=fVROCP6ALKP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fVROCP6ALKP2(i)=fVROCP6ALKP2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 434, <ROCALK14c>
i=i+1;
Rnames{ 434} = 'VROCP5ALKP + NO3 = VROCP5ALKP2 + NO2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='VROCP5ALKP';Gstr{i,   2}='NO3';
fVROCP5ALKP(i)=fVROCP5ALKP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fVROCP5ALKP2(i)=fVROCP5ALKP2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 435, <ROCALK15c>
i=i+1;
Rnames{ 435} = 'VROCP4ALKP + NO3 = VROCP4ALKP2 + NO2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='VROCP4ALKP';Gstr{i,   2}='NO3';
fVROCP4ALKP(i)=fVROCP4ALKP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fVROCP4ALKP2(i)=fVROCP4ALKP2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 436, <ROCALK16c>
i=i+1;
Rnames{ 436} = 'VROCP3ALKP + NO3 = VROCP3ALKP2 + NO2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='VROCP3ALKP';Gstr{i,   2}='NO3';
fVROCP3ALKP(i)=fVROCP3ALKP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fVROCP3ALKP2(i)=fVROCP3ALKP2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 437, <ROCALK17c>
i=i+1;
Rnames{ 437} = 'VROCP2ALKP + NO3 = VROCP2ALKP2 + NO2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='VROCP2ALKP';Gstr{i,   2}='NO3';
fVROCP2ALKP(i)=fVROCP2ALKP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fVROCP2ALKP2(i)=fVROCP2ALKP2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 438, <ROCALK18c>
i=i+1;
Rnames{ 438} = 'VROCP1ALKP + NO3 = VROCP1ALKP2 + NO2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='VROCP1ALKP';Gstr{i,   2}='NO3';
fVROCP1ALKP(i)=fVROCP1ALKP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fVROCP1ALKP2(i)=fVROCP1ALKP2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 439, <HC1003>
i=i+1;
Rnames{ 439} = 'HC10P + NO3 = HC10P2 + NO2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='HC10P';Gstr{i,   2}='NO3';
fHC10P(i)=fHC10P(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHC10P2(i)=fHC10P2(i)+  1.000;fNO2(i)=fNO2(i)+  1.000;

% 440, <ROCALK19c>
i=i+1;
Rnames{ 440} = 'VROCP6ALKP + HO2 = VROCP3OXY2 ';
k(:,i) = (  2.1700E-11 ); 
Gstr{i,   1}='VROCP6ALKP';Gstr{i,   2}='HO2';
fVROCP6ALKP(i)=fVROCP6ALKP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fVROCP3OXY2(i)=fVROCP3OXY2(i)+  1.000;

% 441, <ROCALK20c>
i=i+1;
Rnames{ 441} = 'VROCP5ALKP + HO2 = VROCP2OXY2 ';
k(:,i) = (  2.2000E-11 ); 
Gstr{i,   1}='VROCP5ALKP';Gstr{i,   2}='HO2';
fVROCP5ALKP(i)=fVROCP5ALKP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fVROCP2OXY2(i)=fVROCP2OXY2(i)+  1.000;

% 442, <ROCALK21c>
i=i+1;
Rnames{ 442} = 'VROCP4ALKP + HO2 = VROCP1OXY1 ';
k(:,i) = (  2.2500E-11 ); 
Gstr{i,   1}='VROCP4ALKP';Gstr{i,   2}='HO2';
fVROCP4ALKP(i)=fVROCP4ALKP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fVROCP1OXY1(i)=fVROCP1OXY1(i)+  1.000;

% 443, <ROCALK22c>
i=i+1;
Rnames{ 443} = 'VROCP3ALKP + HO2 = VROCP0OXY2 ';
k(:,i) = (  2.2600E-11 ); 
Gstr{i,   1}='VROCP3ALKP';Gstr{i,   2}='HO2';
fVROCP3ALKP(i)=fVROCP3ALKP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fVROCP0OXY2(i)=fVROCP0OXY2(i)+  1.000;

% 444, <ROCALK23c>
i=i+1;
Rnames{ 444} = 'VROCP2ALKP + HO2 = VROCN1OXY1 ';
k(:,i) = (  2.2700E-11 ); 
Gstr{i,   1}='VROCP2ALKP';Gstr{i,   2}='HO2';
fVROCP2ALKP(i)=fVROCP2ALKP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fVROCN1OXY1(i)=fVROCN1OXY1(i)+  1.000;

% 445, <ROCALK24c>
i=i+1;
Rnames{ 445} = 'VROCP1ALKP + HO2 = VROCN2OXY2 ';
k(:,i) = (  2.2700E-11 ); 
Gstr{i,   1}='VROCP1ALKP';Gstr{i,   2}='HO2';
fVROCP1ALKP(i)=fVROCP1ALKP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fVROCN2OXY2(i)=fVROCN2OXY2(i)+  1.000;

% 446, <HC1004>
i=i+1;
Rnames{ 446} = 'HC10P + HO2 = OP2 ';
k(:,i) = (  2.6600E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='HC10P';Gstr{i,   2}='HO2';
fHC10P(i)=fHC10P(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOP2(i)=fOP2(i)+  1.000;

% 447, <ROCALK25c>
i=i+1;
Rnames{ 447} = 'VROCP6ALKP2 = HO2 + VROCP3OXY2 ';
k(:,i) = (  1.8800E-01 ); 
Gstr{i,   1}='VROCP6ALKP2';
fVROCP6ALKP2(i)=fVROCP6ALKP2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  1.000;

% 448, <ROCALK26c>
i=i+1;
Rnames{ 448} = 'VROCP5ALKP2 = HO2 + VROCP2OXY2 ';
k(:,i) = (  1.8800E-01 ); 
Gstr{i,   1}='VROCP5ALKP2';
fVROCP5ALKP2(i)=fVROCP5ALKP2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fVROCP2OXY2(i)=fVROCP2OXY2(i)+  1.000;

% 449, <ROCALK27c>
i=i+1;
Rnames{ 449} = 'VROCP4ALKP2 = HO2 + VROCP1OXY1 ';
k(:,i) = (  1.8800E-01 ); 
Gstr{i,   1}='VROCP4ALKP2';
fVROCP4ALKP2(i)=fVROCP4ALKP2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fVROCP1OXY1(i)=fVROCP1OXY1(i)+  1.000;

% 450, <ROCALK28c>
i=i+1;
Rnames{ 450} = 'VROCP3ALKP2 = HO2 + VROCP0OXY2 ';
k(:,i) = (  1.8800E-01 ); 
Gstr{i,   1}='VROCP3ALKP2';
fVROCP3ALKP2(i)=fVROCP3ALKP2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fVROCP0OXY2(i)=fVROCP0OXY2(i)+  1.000;

% 451, <ROCALK29c>
i=i+1;
Rnames{ 451} = 'VROCP2ALKP2 = HO2 + VROCN1OXY1 ';
k(:,i) = (  1.8800E-01 ); 
Gstr{i,   1}='VROCP2ALKP2';
fVROCP2ALKP2(i)=fVROCP2ALKP2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fVROCN1OXY1(i)=fVROCN1OXY1(i)+  1.000;

% 452, <ROCALK30c>
i=i+1;
Rnames{ 452} = 'VROCP1ALKP2 = HO2 + VROCN2OXY2 ';
k(:,i) = (  1.8800E-01 ); 
Gstr{i,   1}='VROCP1ALKP2';
fVROCP1ALKP2(i)=fVROCP1ALKP2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fVROCN2OXY2(i)=fVROCN2OXY2(i)+  1.000;

% 453, <HC1005>
i=i+1;
Rnames{ 453} = 'HC10P2 = HO2 + VROCP4OXY2 ';
k(:,i) = (  1.8800E-01 ); 
Gstr{i,   1}='HC10P2';
fHC10P2(i)=fHC10P2(i)-1.0;
fHO2(i)=fHO2(i)+  1.000;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  1.000;

% 454, <ROCALK31c>
i=i+1;
Rnames{ 454} = 'VROCP6ALKP2 + NO = 0.14000*VROCP2OXY2 +  0.86000*NO2 +  0.86000*VROCP3OXY2 +  0.86000*HO2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='VROCP6ALKP2';Gstr{i,   2}='NO';
fVROCP6ALKP2(i)=fVROCP6ALKP2(i)-1.0;fNO(i)=fNO(i)-1.0;
fVROCP2OXY2(i)=fVROCP2OXY2(i)+  0.140;fNO2(i)=fNO2(i)+  0.860;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.860;fHO2(i)=fHO2(i)+  0.860;

% 455, <ROCALK32c>
i=i+1;
Rnames{ 455} = 'VROCP5ALKP2 + NO = 0.14000*VROCP1OXY3 +  0.86000*NO2 +  0.86000*VROCP2OXY2 +  0.86000*HO2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='VROCP5ALKP2';Gstr{i,   2}='NO';
fVROCP5ALKP2(i)=fVROCP5ALKP2(i)-1.0;fNO(i)=fNO(i)-1.0;
fVROCP1OXY3(i)=fVROCP1OXY3(i)+  0.140;fNO2(i)=fNO2(i)+  0.860;fVROCP2OXY2(i)=fVROCP2OXY2(i)+  0.860;fHO2(i)=fHO2(i)+  0.860;

% 456, <ROCALK33c>
i=i+1;
Rnames{ 456} = 'VROCP4ALKP2 + NO = 0.14000*VROCP0OXY2 +  0.86000*NO2 +  0.86000*VROCP1OXY1 +  0.86000*HO2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='VROCP4ALKP2';Gstr{i,   2}='NO';
fVROCP4ALKP2(i)=fVROCP4ALKP2(i)-1.0;fNO(i)=fNO(i)-1.0;
fVROCP0OXY2(i)=fVROCP0OXY2(i)+  0.140;fNO2(i)=fNO2(i)+  0.860;fVROCP1OXY1(i)=fVROCP1OXY1(i)+  0.860;fHO2(i)=fHO2(i)+  0.860;

% 457, <ROCALK34c>
i=i+1;
Rnames{ 457} = 'VROCP3ALKP2 + NO = 0.14000*VROCN1OXY1 +  0.86000*NO2 +  0.86000*VROCP0OXY2 +  0.86000*HO2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='VROCP3ALKP2';Gstr{i,   2}='NO';
fVROCP3ALKP2(i)=fVROCP3ALKP2(i)-1.0;fNO(i)=fNO(i)-1.0;
fVROCN1OXY1(i)=fVROCN1OXY1(i)+  0.140;fNO2(i)=fNO2(i)+  0.860;fVROCP0OXY2(i)=fVROCP0OXY2(i)+  0.860;fHO2(i)=fHO2(i)+  0.860;

% 458, <ROCALK35c>
i=i+1;
Rnames{ 458} = 'VROCP2ALKP2 + NO = 0.14000*VROCN2OXY2 +  0.86000*NO2 +  0.86000*VROCN1OXY1 +  0.86000*HO2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='VROCP2ALKP2';Gstr{i,   2}='NO';
fVROCP2ALKP2(i)=fVROCP2ALKP2(i)-1.0;fNO(i)=fNO(i)-1.0;
fVROCN2OXY2(i)=fVROCN2OXY2(i)+  0.140;fNO2(i)=fNO2(i)+  0.860;fVROCN1OXY1(i)=fVROCN1OXY1(i)+  0.860;fHO2(i)=fHO2(i)+  0.860;

% 459, <ROCALK36c>
i=i+1;
Rnames{ 459} = 'VROCP1ALKP2 + NO = VROCN2OXY2 +  0.86000*NO2 +  0.86000*HO2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='VROCP1ALKP2';Gstr{i,   2}='NO';
fVROCP1ALKP2(i)=fVROCP1ALKP2(i)-1.0;fNO(i)=fNO(i)-1.0;
fVROCN2OXY2(i)=fVROCN2OXY2(i)+  1.000;fNO2(i)=fNO2(i)+  0.860;fHO2(i)=fHO2(i)+  0.860;

% 460, <HC1006>
i=i+1;
Rnames{ 460} = 'HC10P2 + NO = 0.12000*ONIT +  0.88000*NO2 +  0.88000*KET +  0.88000*HO2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='HC10P2';Gstr{i,   2}='NO';
fHC10P2(i)=fHC10P2(i)-1.0;fNO(i)=fNO(i)-1.0;
fONIT(i)=fONIT(i)+  0.120;fNO2(i)=fNO2(i)+  0.880;fKET(i)=fKET(i)+  0.880;fHO2(i)=fHO2(i)+  0.880;

% 461, <ROCALK37c>
i=i+1;
Rnames{ 461} = 'VROCP6ALKP2 + NO3 = NO2 + VROCP3OXY2 + HO2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='VROCP6ALKP2';Gstr{i,   2}='NO3';
fVROCP6ALKP2(i)=fVROCP6ALKP2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  1.000;fHO2(i)=fHO2(i)+  1.000;

% 462, <ROCALK38c>
i=i+1;
Rnames{ 462} = 'VROCP5ALKP2 + NO3 = NO2 + VROCP2OXY2 + HO2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='VROCP5ALKP2';Gstr{i,   2}='NO3';
fVROCP5ALKP2(i)=fVROCP5ALKP2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fVROCP2OXY2(i)=fVROCP2OXY2(i)+  1.000;fHO2(i)=fHO2(i)+  1.000;

% 463, <ROCALK39c>
i=i+1;
Rnames{ 463} = 'VROCP4ALKP2 + NO3 = NO2 + VROCP1OXY1 + HO2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='VROCP4ALKP2';Gstr{i,   2}='NO3';
fVROCP4ALKP2(i)=fVROCP4ALKP2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fVROCP1OXY1(i)=fVROCP1OXY1(i)+  1.000;fHO2(i)=fHO2(i)+  1.000;

% 464, <ROCALK40c>
i=i+1;
Rnames{ 464} = 'VROCP3ALKP2 + NO3 = NO2 + VROCP0OXY2 + HO2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='VROCP3ALKP2';Gstr{i,   2}='NO3';
fVROCP3ALKP2(i)=fVROCP3ALKP2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fVROCP0OXY2(i)=fVROCP0OXY2(i)+  1.000;fHO2(i)=fHO2(i)+  1.000;

% 465, <ROCALK41c>
i=i+1;
Rnames{ 465} = 'VROCP2ALKP2 + NO3 = NO2 + VROCN1OXY1 + HO2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='VROCP2ALKP2';Gstr{i,   2}='NO3';
fVROCP2ALKP2(i)=fVROCP2ALKP2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fVROCN1OXY1(i)=fVROCN1OXY1(i)+  1.000;fHO2(i)=fHO2(i)+  1.000;

% 466, <ROCALK42c>
i=i+1;
Rnames{ 466} = 'VROCP1ALKP2 + NO3 = NO2 + VROCN2OXY2 + HO2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='VROCP1ALKP2';Gstr{i,   2}='NO3';
fVROCP1ALKP2(i)=fVROCP1ALKP2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fVROCN2OXY2(i)=fVROCN2OXY2(i)+  1.000;fHO2(i)=fHO2(i)+  1.000;

% 467, <HC1007>
i=i+1;
Rnames{ 467} = 'HC10P2 + NO3 = NO2 + KET + HO2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='HC10P2';Gstr{i,   2}='NO3';
fHC10P2(i)=fHC10P2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fKET(i)=fKET(i)+  1.000;fHO2(i)=fHO2(i)+  1.000;

% 468, <ROCALK43c>
i=i+1;
Rnames{ 468} = 'VROCP6ALKP2 + HO2 = VROCP1OXY3 ';
k(:,i) = (  2.1700E-11 ); 
Gstr{i,   1}='VROCP6ALKP2';Gstr{i,   2}='HO2';
fVROCP6ALKP2(i)=fVROCP6ALKP2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fVROCP1OXY3(i)=fVROCP1OXY3(i)+  1.000;

% 469, <ROCALK44c>
i=i+1;
Rnames{ 469} = 'VROCP5ALKP2 + HO2 = VROCP0OXY2 ';
k(:,i) = (  2.2000E-11 ); 
Gstr{i,   1}='VROCP5ALKP2';Gstr{i,   2}='HO2';
fVROCP5ALKP2(i)=fVROCP5ALKP2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fVROCP0OXY2(i)=fVROCP0OXY2(i)+  1.000;

% 470, <ROCALK45c>
i=i+1;
Rnames{ 470} = 'VROCP4ALKP2 + HO2 = VROCN1OXY1 ';
k(:,i) = (  2.2500E-11 ); 
Gstr{i,   1}='VROCP4ALKP2';Gstr{i,   2}='HO2';
fVROCP4ALKP2(i)=fVROCP4ALKP2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fVROCN1OXY1(i)=fVROCN1OXY1(i)+  1.000;

% 471, <ROCALK46c>
i=i+1;
Rnames{ 471} = 'VROCP3ALKP2 + HO2 = VROCN2OXY2 ';
k(:,i) = (  2.2600E-11 ); 
Gstr{i,   1}='VROCP3ALKP2';Gstr{i,   2}='HO2';
fVROCP3ALKP2(i)=fVROCP3ALKP2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fVROCN2OXY2(i)=fVROCN2OXY2(i)+  1.000;

% 472, <ROCALK47c>
i=i+1;
Rnames{ 472} = 'VROCP2ALKP2 + HO2 = VROCN2OXY2 ';
k(:,i) = (  2.2700E-11 ); 
Gstr{i,   1}='VROCP2ALKP2';Gstr{i,   2}='HO2';
fVROCP2ALKP2(i)=fVROCP2ALKP2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fVROCN2OXY2(i)=fVROCN2OXY2(i)+  1.000;

% 473, <ROCALK48c>
i=i+1;
Rnames{ 473} = 'VROCP1ALKP2 + HO2 = VROCN2OXY2 ';
k(:,i) = (  2.2700E-11 ); 
Gstr{i,   1}='VROCP1ALKP2';Gstr{i,   2}='HO2';
fVROCP1ALKP2(i)=fVROCP1ALKP2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fVROCN2OXY2(i)=fVROCN2OXY2(i)+  1.000;

% 474, <HC1008>
i=i+1;
Rnames{ 474} = 'HC10P2 + HO2 = VROCP2OXY2 ';
k(:,i) = (  2.6600E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='HC10P2';Gstr{i,   2}='HO2';
fHC10P2(i)=fHC10P2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fVROCP2OXY2(i)=fVROCP2OXY2(i)+  1.000;

% 475, <ROCARO01>
i=i+1;
Rnames{ 475} = 'VROCP6ARO + HO = 0.84000*VROCP6AROP +  0.16000*HO2 +  0.16000*VROCP4OXY2 ';
k(:,i) = (  1.8100E-11 ); 
Gstr{i,   1}='VROCP6ARO';Gstr{i,   2}='HO';
fVROCP6ARO(i)=fVROCP6ARO(i)-1.0;fHO(i)=fHO(i)-1.0;
fVROCP6AROP(i)=fVROCP6AROP(i)+  0.840;fHO2(i)=fHO2(i)+  0.160;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.160;

% 476, <ROCARO02>
i=i+1;
Rnames{ 476} = 'VROCP6AROP + HO2 = 0.05950*VROCP4OXY2 +  0.90480*VROCP1OXY3 +  0.03570*VROCN2OXY4 ';
k(:,i) = (  2.9100E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='VROCP6AROP';Gstr{i,   2}='HO2';
fVROCP6AROP(i)=fVROCP6AROP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.059;fVROCP1OXY3(i)=fVROCP1OXY3(i)+  0.905;fVROCN2OXY4(i)=fVROCN2OXY4(i)+  0.036;

% 477, <ROCARO03>
i=i+1;
Rnames{ 477} = 'VROCP6AROP + NO = 0.00010*VROCP4OXY2 +  0.00180*VROCP2OXY2 +  0.00010*VROCN1OXY3 +  0.99800*NO2 +  0.99800*HO2 +  0.05940*BALD +  0.46930*GLY +  0.46930*MGLY +  0.46930*FURANONE +  0.46930*DCB2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='VROCP6AROP';Gstr{i,   2}='NO';
fVROCP6AROP(i)=fVROCP6AROP(i)-1.0;fNO(i)=fNO(i)-1.0;
fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.000;fVROCP2OXY2(i)=fVROCP2OXY2(i)+  0.002;fVROCN1OXY3(i)=fVROCN1OXY3(i)+  0.000;fNO2(i)=fNO2(i)+  0.998;fHO2(i)=fHO2(i)+  0.998;fBALD(i)=fBALD(i)+  0.059;fGLY(i)=fGLY(i)+  0.469;fMGLY(i)=fMGLY(i)+  0.469;fFURANONE(i)=fFURANONE(i)+  0.469;fDCB2(i)=fDCB2(i)+  0.469;

% 478, <ROCARO04>
i=i+1;
Rnames{ 478} = 'VROCP6AROP + NO3 = NO2 +  0.94050*HO2 +  0.05950*BALD +  0.47020*GLY +  0.47020*MGLY +  0.47020*FURANONE +  0.47020*DCB2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='VROCP6AROP';Gstr{i,   2}='NO3';
fVROCP6AROP(i)=fVROCP6AROP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fHO2(i)=fHO2(i)+  0.941;fBALD(i)=fBALD(i)+  0.059;fGLY(i)=fGLY(i)+  0.470;fMGLY(i)=fMGLY(i)+  0.470;fFURANONE(i)=fFURANONE(i)+  0.470;fDCB2(i)=fDCB2(i)+  0.470;

% 479, <ROCARO05>
i=i+1;
Rnames{ 479} = 'VROCP6AROP + MO2 = 0.68000*HCHO +  1.31050*HO2 +  0.32000*MOH +  0.05950*BALD +  0.47020*GLY +  0.47020*MGLY +  0.47020*FURANONE +  0.47020*DCB2 ';
k(:,i) = (  3.5600E-14.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='VROCP6AROP';Gstr{i,   2}='MO2';
fVROCP6AROP(i)=fVROCP6AROP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHCHO(i)=fHCHO(i)+  0.680;fHO2(i)=fHO2(i)+  1.310;fMOH(i)=fMOH(i)+  0.320;fBALD(i)=fBALD(i)+  0.059;fGLY(i)=fGLY(i)+  0.470;fMGLY(i)=fMGLY(i)+  0.470;fFURANONE(i)=fFURANONE(i)+  0.470;fDCB2(i)=fDCB2(i)+  0.470;

% 480, <ROCARO06>
i=i+1;
Rnames{ 480} = 'VROCP6AROP + ACO3 = 0.70000*MO2 +  0.94050*HO2 +  0.30000*ORA2 +  0.05950*BALD +  0.47020*GLY +  0.47020*MGLY +  0.47020*FURANONE +  0.47020*DCB2 ';
k(:,i) = (  7.4000E-13.*exp(  7.6500E+02./T) ); 
Gstr{i,   1}='VROCP6AROP';Gstr{i,   2}='ACO3';
fVROCP6AROP(i)=fVROCP6AROP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fMO2(i)=fMO2(i)+  0.700;fHO2(i)=fHO2(i)+  0.941;fORA2(i)=fORA2(i)+  0.300;fBALD(i)=fBALD(i)+  0.059;fGLY(i)=fGLY(i)+  0.470;fMGLY(i)=fMGLY(i)+  0.470;fFURANONE(i)=fFURANONE(i)+  0.470;fDCB2(i)=fDCB2(i)+  0.470;

% 481, <ROCARO11>
i=i+1;
Rnames{ 481} = 'VROCP5ARO + HO = 0.84000*VROCP5AROP +  0.16000*HO2 +  0.16000*VROCP3OXY2 ';
k(:,i) = (  1.8100E-11 ); 
Gstr{i,   1}='VROCP5ARO';Gstr{i,   2}='HO';
fVROCP5ARO(i)=fVROCP5ARO(i)-1.0;fHO(i)=fHO(i)-1.0;
fVROCP5AROP(i)=fVROCP5AROP(i)+  0.840;fHO2(i)=fHO2(i)+  0.160;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.160;

% 482, <ROCARO12>
i=i+1;
Rnames{ 482} = 'VROCP5AROP + HO2 = 0.05950*VROCP3OXY2 +  0.90480*VROCP0OXY2 +  0.03570*VROCN2OXY4 ';
k(:,i) = (  2.9100E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='VROCP5AROP';Gstr{i,   2}='HO2';
fVROCP5AROP(i)=fVROCP5AROP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.059;fVROCP0OXY2(i)=fVROCP0OXY2(i)+  0.905;fVROCN2OXY4(i)=fVROCN2OXY4(i)+  0.036;

% 483, <ROCARO13>
i=i+1;
Rnames{ 483} = 'VROCP5AROP + NO = 0.00010*VROCP3OXY2 +  0.00180*VROCP1OXY3 +  0.00010*VROCN2OXY4 +  0.99800*NO2 +  0.99800*HO2 +  0.05940*VROCP4OXY2 +  0.46930*GLY +  0.46930*MGLY +  0.46930*FURANONE +  0.46930*DCB2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='VROCP5AROP';Gstr{i,   2}='NO';
fVROCP5AROP(i)=fVROCP5AROP(i)-1.0;fNO(i)=fNO(i)-1.0;
fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.000;fVROCP1OXY3(i)=fVROCP1OXY3(i)+  0.002;fVROCN2OXY4(i)=fVROCN2OXY4(i)+  0.000;fNO2(i)=fNO2(i)+  0.998;fHO2(i)=fHO2(i)+  0.998;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.059;fGLY(i)=fGLY(i)+  0.469;fMGLY(i)=fMGLY(i)+  0.469;fFURANONE(i)=fFURANONE(i)+  0.469;fDCB2(i)=fDCB2(i)+  0.469;

% 484, <ROCARO14>
i=i+1;
Rnames{ 484} = 'VROCP5AROP + NO3 = NO2 +  0.94050*HO2 +  0.05950*VROCP4OXY2 +  0.47020*GLY +  0.47020*MGLY +  0.47020*FURANONE +  0.47020*DCB2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='VROCP5AROP';Gstr{i,   2}='NO3';
fVROCP5AROP(i)=fVROCP5AROP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fHO2(i)=fHO2(i)+  0.941;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.059;fGLY(i)=fGLY(i)+  0.470;fMGLY(i)=fMGLY(i)+  0.470;fFURANONE(i)=fFURANONE(i)+  0.470;fDCB2(i)=fDCB2(i)+  0.470;

% 485, <ROCARO15>
i=i+1;
Rnames{ 485} = 'VROCP5AROP + MO2 = 0.68000*HCHO +  1.31050*HO2 +  0.32000*MOH +  0.05950*VROCP4OXY2 +  0.47020*GLY +  0.47020*MGLY +  0.47020*FURANONE +  0.47020*DCB2 ';
k(:,i) = (  3.5600E-14.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='VROCP5AROP';Gstr{i,   2}='MO2';
fVROCP5AROP(i)=fVROCP5AROP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHCHO(i)=fHCHO(i)+  0.680;fHO2(i)=fHO2(i)+  1.310;fMOH(i)=fMOH(i)+  0.320;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.059;fGLY(i)=fGLY(i)+  0.470;fMGLY(i)=fMGLY(i)+  0.470;fFURANONE(i)=fFURANONE(i)+  0.470;fDCB2(i)=fDCB2(i)+  0.470;

% 486, <ROCARO16>
i=i+1;
Rnames{ 486} = 'VROCP5AROP + ACO3 = 0.70000*MO2 +  0.94050*HO2 +  0.30000*ORA2 +  0.05950*VROCP4OXY2 +  0.47020*GLY +  0.47020*MGLY +  0.47020*FURANONE +  0.47020*DCB2 ';
k(:,i) = (  7.4000E-13.*exp(  7.6500E+02./T) ); 
Gstr{i,   1}='VROCP5AROP';Gstr{i,   2}='ACO3';
fVROCP5AROP(i)=fVROCP5AROP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fMO2(i)=fMO2(i)+  0.700;fHO2(i)=fHO2(i)+  0.941;fORA2(i)=fORA2(i)+  0.300;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.059;fGLY(i)=fGLY(i)+  0.470;fMGLY(i)=fMGLY(i)+  0.470;fFURANONE(i)=fFURANONE(i)+  0.470;fDCB2(i)=fDCB2(i)+  0.470;

% 487, <ROCARO21>
i=i+1;
Rnames{ 487} = 'NAPH + HO = 0.84000*NAPHP +  0.16000*HO2 +  0.16000*VROCP3OXY2 ';
k(:,i) = (  2.3100E-11 ); 
Gstr{i,   1}='NAPH';Gstr{i,   2}='HO';
fNAPH(i)=fNAPH(i)-1.0;fHO(i)=fHO(i)-1.0;
fNAPHP(i)=fNAPHP(i)+  0.840;fHO2(i)=fHO2(i)+  0.160;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.160;

% 488, <ROCARO22>
i=i+1;
Rnames{ 488} = 'NAPHP + HO2 = 0.05950*VROCP3OXY2 +  0.90480*VROCP1OXY3 +  0.03570*VROCN2OXY8 ';
k(:,i) = (  2.9100E-13.*exp(  1.3000E+03./T) ); 
Gstr{i,   1}='NAPHP';Gstr{i,   2}='HO2';
fNAPHP(i)=fNAPHP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.059;fVROCP1OXY3(i)=fVROCP1OXY3(i)+  0.905;fVROCN2OXY8(i)=fVROCN2OXY8(i)+  0.036;

% 489, <ROCARO23>
i=i+1;
Rnames{ 489} = 'NAPHP + NO = 0.05950*VROCP4OXY2 +  0.00180*VROCP2OXY2 +  0.00010*VROCN2OXY8 +  0.99800*NO2 +  0.99800*HO2 +  0.46930*GLY +  0.46930*MGLY +  0.46930*FURANONE +  0.46930*DCB2 ';
k(:,i) = (  2.7000E-12.*exp(  3.6000E+02./T) ); 
Gstr{i,   1}='NAPHP';Gstr{i,   2}='NO';
fNAPHP(i)=fNAPHP(i)-1.0;fNO(i)=fNO(i)-1.0;
fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.060;fVROCP2OXY2(i)=fVROCP2OXY2(i)+  0.002;fVROCN2OXY8(i)=fVROCN2OXY8(i)+  0.000;fNO2(i)=fNO2(i)+  0.998;fHO2(i)=fHO2(i)+  0.998;fGLY(i)=fGLY(i)+  0.469;fMGLY(i)=fMGLY(i)+  0.469;fFURANONE(i)=fFURANONE(i)+  0.469;fDCB2(i)=fDCB2(i)+  0.469;

% 490, <ROCARO24>
i=i+1;
Rnames{ 490} = 'NAPHP + NO3 = NO2 +  0.94050*HO2 +  0.05950*VROCP4OXY2 +  0.47020*GLY +  0.47020*MGLY +  0.47020*FURANONE +  0.47020*DCB2 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='NAPHP';Gstr{i,   2}='NO3';
fNAPHP(i)=fNAPHP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.000;fHO2(i)=fHO2(i)+  0.941;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.059;fGLY(i)=fGLY(i)+  0.470;fMGLY(i)=fMGLY(i)+  0.470;fFURANONE(i)=fFURANONE(i)+  0.470;fDCB2(i)=fDCB2(i)+  0.470;

% 491, <ROCARO25>
i=i+1;
Rnames{ 491} = 'NAPHP + MO2 = 0.68000*HCHO +  1.31050*HO2 +  0.32000*MOH +  0.05950*VROCP4OXY2 +  0.47020*GLY +  0.47020*MGLY +  0.47020*FURANONE +  0.47020*DCB2 ';
k(:,i) = (  3.5600E-14.*exp(  7.0800E+02./T) ); 
Gstr{i,   1}='NAPHP';Gstr{i,   2}='MO2';
fNAPHP(i)=fNAPHP(i)-1.0;fMO2(i)=fMO2(i)-1.0;
fHCHO(i)=fHCHO(i)+  0.680;fHO2(i)=fHO2(i)+  1.310;fMOH(i)=fMOH(i)+  0.320;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.059;fGLY(i)=fGLY(i)+  0.470;fMGLY(i)=fMGLY(i)+  0.470;fFURANONE(i)=fFURANONE(i)+  0.470;fDCB2(i)=fDCB2(i)+  0.470;

% 492, <ROCARO26>
i=i+1;
Rnames{ 492} = 'NAPHP + ACO3 = 0.70000*MO2 +  0.94050*HO2 +  0.30000*ORA2 +  0.05950*VROCP4OXY2 +  0.47020*GLY +  0.47020*MGLY +  0.47020*FURANONE +  0.47020*DCB2 ';
k(:,i) = (  7.4000E-13.*exp(  7.6500E+02./T) ); 
Gstr{i,   1}='NAPHP';Gstr{i,   2}='ACO3';
fNAPHP(i)=fNAPHP(i)-1.0;fACO3(i)=fACO3(i)-1.0;
fMO2(i)=fMO2(i)+  0.700;fHO2(i)=fHO2(i)+  0.941;fORA2(i)=fORA2(i)+  0.300;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.059;fGLY(i)=fGLY(i)+  0.470;fMGLY(i)=fMGLY(i)+  0.470;fFURANONE(i)=fFURANONE(i)+  0.470;fDCB2(i)=fDCB2(i)+  0.470;

% 493, <ROCOXY1c>
i=i+1;
Rnames{ 493} = 'VROCN2OXY8 + HO = HO +  0.08540*VROCN2OXY8 +  0.25810*DCB1 +  0.25810*MEK +  0.25810*ACD +  0.25810*ALD +  0.25810*MO2 +  0.25810*ETHP +  0.25810*HC3P +  0.25810*MEKP ';
k(:,i) = (  5.9000E-11 ); 
Gstr{i,   1}='VROCN2OXY8';Gstr{i,   2}='HO';
fVROCN2OXY8(i)=fVROCN2OXY8(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  1.000;fVROCN2OXY8(i)=fVROCN2OXY8(i)+  0.085;fDCB1(i)=fDCB1(i)+  0.258;fMEK(i)=fMEK(i)+  0.258;fACD(i)=fACD(i)+  0.258;fALD(i)=fALD(i)+  0.258;fMO2(i)=fMO2(i)+  0.258;fETHP(i)=fETHP(i)+  0.258;fHC3P(i)=fHC3P(i)+  0.258;fMEKP(i)=fMEKP(i)+  0.258;

% 494, <ROCOXY2c>
i=i+1;
Rnames{ 494} = 'VROCN2OXY4 + HO = HO +  0.46400*VROCN2OXY8 +  0.19770*VROCN2OXY4 +  0.01210*VROCN1OXY6 +  0.01520*VROCN1OXY3 +  0.06240*VROCP0OXY4 +  0.03880*VROCP1OXY3 +  0.04910*VROCP2OXY2 +  0.03980*VROCP3OXY2 +  0.01830*VROCP4OXY2 +  0.03080*OP3 +  0.00400*OP2 +  0.07940*DCB1 +  0.07940*MEK +  0.07940*KET +  0.07940*ACD +  0.07940*ALD +  0.07940*MO2 +  0.07940*ETHP +  0.07940*HC3P +  0.07940*MEKP +  0.07940*HC5P +  0.07940*KETP ';
k(:,i) = (  6.0700E-11 ); 
Gstr{i,   1}='VROCN2OXY4';Gstr{i,   2}='HO';
fVROCN2OXY4(i)=fVROCN2OXY4(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  1.000;fVROCN2OXY8(i)=fVROCN2OXY8(i)+  0.464;fVROCN2OXY4(i)=fVROCN2OXY4(i)+  0.198;fVROCN1OXY6(i)=fVROCN1OXY6(i)+  0.012;fVROCN1OXY3(i)=fVROCN1OXY3(i)+  0.015;fVROCP0OXY4(i)=fVROCP0OXY4(i)+  0.062;fVROCP1OXY3(i)=fVROCP1OXY3(i)+  0.039;fVROCP2OXY2(i)=fVROCP2OXY2(i)+  0.049;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.040;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.018;fOP3(i)=fOP3(i)+  0.031;fOP2(i)=fOP2(i)+  0.004;fDCB1(i)=fDCB1(i)+  0.079;fMEK(i)=fMEK(i)+  0.079;fKET(i)=fKET(i)+  0.079;fACD(i)=fACD(i)+  0.079;fALD(i)=fALD(i)+  0.079;fMO2(i)=fMO2(i)+  0.079;fETHP(i)=fETHP(i)+  0.079;fHC3P(i)=fHC3P(i)+  0.079;fMEKP(i)=fMEKP(i)+  0.079;fHC5P(i)=fHC5P(i)+  0.079;fKETP(i)=fKETP(i)+  0.079;

% 495, <ROCOXY3c>
i=i+1;
Rnames{ 495} = 'VROCN2OXY2 + HO = HO +  0.10410*VROCN2OXY8 +  0.56380*VROCN2OXY4 +  0.21410*VROCN2OXY2 +  0.01530*VROCN1OXY6 +  0.02980*VROCN1OXY3 +  0.00960*VROCN1OXY1 +  0.01890*VROCP0OXY4 +  0.04560*VROCP0OXY2 +  0.03140*VROCP1OXY3 +  0.01990*VROCP1OXY1 +  0.04590*VROCP2OXY2 +  0.04520*VROCP3OXY2 +  0.04550*VROCP4OXY2 +  0.03250*VROCP5OXY1 +  0.03690*VROCP6OXY1 +  0.00260*OP3 +  0.03900*DCB1 +  0.03900*HKET +  0.03900*MEK +  0.03900*ACD +  0.03900*ALD +  0.03900*MO2 +  0.03900*ETHP +  0.03900*HC3P +  0.03900*MEKP +  0.09250*HC5P ';
k(:,i) = (  5.5400E-11 ); 
Gstr{i,   1}='VROCN2OXY2';Gstr{i,   2}='HO';
fVROCN2OXY2(i)=fVROCN2OXY2(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  1.000;fVROCN2OXY8(i)=fVROCN2OXY8(i)+  0.104;fVROCN2OXY4(i)=fVROCN2OXY4(i)+  0.564;fVROCN2OXY2(i)=fVROCN2OXY2(i)+  0.214;fVROCN1OXY6(i)=fVROCN1OXY6(i)+  0.015;fVROCN1OXY3(i)=fVROCN1OXY3(i)+  0.030;fVROCN1OXY1(i)=fVROCN1OXY1(i)+  0.010;fVROCP0OXY4(i)=fVROCP0OXY4(i)+  0.019;fVROCP0OXY2(i)=fVROCP0OXY2(i)+  0.046;fVROCP1OXY3(i)=fVROCP1OXY3(i)+  0.031;fVROCP1OXY1(i)=fVROCP1OXY1(i)+  0.020;fVROCP2OXY2(i)=fVROCP2OXY2(i)+  0.046;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.045;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.045;fVROCP5OXY1(i)=fVROCP5OXY1(i)+  0.033;fVROCP6OXY1(i)=fVROCP6OXY1(i)+  0.037;fOP3(i)=fOP3(i)+  0.003;fDCB1(i)=fDCB1(i)+  0.039;fHKET(i)=fHKET(i)+  0.039;fMEK(i)=fMEK(i)+  0.039;fACD(i)=fACD(i)+  0.039;fALD(i)=fALD(i)+  0.039;fMO2(i)=fMO2(i)+  0.039;fETHP(i)=fETHP(i)+  0.039;fHC3P(i)=fHC3P(i)+  0.039;fMEKP(i)=fMEKP(i)+  0.039;fHC5P(i)=fHC5P(i)+  0.092;

% 496, <ROCOXY4c>
i=i+1;
Rnames{ 496} = 'VROCN1OXY6 + HO = HO +  0.20360*VROCN2OXY8 +  0.00710*VROCN2OXY4 +  0.18400*DCB1 +  0.18400*MEK +  0.18400*KET +  0.18400*ACD +  0.18400*ALD +  0.18400*MO2 +  0.18400*ETHP +  0.18400*HC3P +  0.18400*MEKP +  0.18400*HC5P ';
k(:,i) = (  5.6300E-11 ); 
Gstr{i,   1}='VROCN1OXY6';Gstr{i,   2}='HO';
fVROCN1OXY6(i)=fVROCN1OXY6(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  1.000;fVROCN2OXY8(i)=fVROCN2OXY8(i)+  0.204;fVROCN2OXY4(i)=fVROCN2OXY4(i)+  0.007;fDCB1(i)=fDCB1(i)+  0.184;fMEK(i)=fMEK(i)+  0.184;fKET(i)=fKET(i)+  0.184;fACD(i)=fACD(i)+  0.184;fALD(i)=fALD(i)+  0.184;fMO2(i)=fMO2(i)+  0.184;fETHP(i)=fETHP(i)+  0.184;fHC3P(i)=fHC3P(i)+  0.184;fMEKP(i)=fMEKP(i)+  0.184;fHC5P(i)=fHC5P(i)+  0.184;

% 497, <ROCOXY5c>
i=i+1;
Rnames{ 497} = 'VROCN1OXY3 + HO = HO +  0.27920*VROCN2OXY8 +  0.40250*VROCN2OXY4 +  0.00880*VROCN2OXY2 +  0.03190*VROCN1OXY6 +  0.00760*VROCN1OXY3 +  0.01940*VROCP0OXY4 +  0.01040*VROCP0OXY2 +  0.05100*VROCP1OXY3 +  0.00750*VROCP1OXY1 +  0.05120*VROCP2OXY2 +  0.04620*VROCP3OXY2 +  0.05120*VROCP4OXY2 +  0.01380*VROCP5OXY1 +  0.01350*OP2 +  0.06460*DCB1 +  0.06460*HKET +  0.06460*MEK +  0.06460*ACD +  0.06460*ALD +  0.06460*MO2 +  0.06460*ETHP +  0.06460*HC3P +  0.06460*MEKP +  0.17530*HC5P ';
k(:,i) = (  5.4600E-11 ); 
Gstr{i,   1}='VROCN1OXY3';Gstr{i,   2}='HO';
fVROCN1OXY3(i)=fVROCN1OXY3(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  1.000;fVROCN2OXY8(i)=fVROCN2OXY8(i)+  0.279;fVROCN2OXY4(i)=fVROCN2OXY4(i)+  0.403;fVROCN2OXY2(i)=fVROCN2OXY2(i)+  0.009;fVROCN1OXY6(i)=fVROCN1OXY6(i)+  0.032;fVROCN1OXY3(i)=fVROCN1OXY3(i)+  0.008;fVROCP0OXY4(i)=fVROCP0OXY4(i)+  0.019;fVROCP0OXY2(i)=fVROCP0OXY2(i)+  0.010;fVROCP1OXY3(i)=fVROCP1OXY3(i)+  0.051;fVROCP1OXY1(i)=fVROCP1OXY1(i)+  0.007;fVROCP2OXY2(i)=fVROCP2OXY2(i)+  0.051;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.046;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.051;fVROCP5OXY1(i)=fVROCP5OXY1(i)+  0.014;fOP2(i)=fOP2(i)+  0.013;fDCB1(i)=fDCB1(i)+  0.065;fHKET(i)=fHKET(i)+  0.065;fMEK(i)=fMEK(i)+  0.065;fACD(i)=fACD(i)+  0.065;fALD(i)=fALD(i)+  0.065;fMO2(i)=fMO2(i)+  0.065;fETHP(i)=fETHP(i)+  0.065;fHC3P(i)=fHC3P(i)+  0.065;fMEKP(i)=fMEKP(i)+  0.065;fHC5P(i)=fHC5P(i)+  0.175;

% 498, <ROCOXY6c>
i=i+1;
Rnames{ 498} = 'VROCN1OXY1 + HO = HO +  0.00740*VROCN2OXY8 +  0.11900*VROCN2OXY4 +  0.72610*VROCN2OXY2 +  0.01220*VROCN1OXY6 +  0.03050*VROCN1OXY3 +  0.00700*VROCN1OXY1 +  0.02910*VROCP0OXY4 +  0.04540*VROCP0OXY2 +  0.02340*VROCP1OXY3 +  0.03520*VROCP1OXY1 +  0.06240*VROCP2OXY2 +  0.05180*VROCP3OXY2 +  0.05090*VROCP4OXY2 +  0.03470*VROCP5OXY1 +  0.07480*VROCP6OXY1 +  0.01630*OP3 +  0.00620*OP2 +  0.02440*DCB1 +  0.02440*HKET +  0.02440*MEK +  0.02440*ACD +  0.02440*ALD +  0.02440*MO2 +  0.02440*ETHP +  0.02440*HC3P +  0.02440*MEKP +  0.05370*HC5P ';
k(:,i) = (  4.5000E-11 ); 
Gstr{i,   1}='VROCN1OXY1';Gstr{i,   2}='HO';
fVROCN1OXY1(i)=fVROCN1OXY1(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  1.000;fVROCN2OXY8(i)=fVROCN2OXY8(i)+  0.007;fVROCN2OXY4(i)=fVROCN2OXY4(i)+  0.119;fVROCN2OXY2(i)=fVROCN2OXY2(i)+  0.726;fVROCN1OXY6(i)=fVROCN1OXY6(i)+  0.012;fVROCN1OXY3(i)=fVROCN1OXY3(i)+  0.030;fVROCN1OXY1(i)=fVROCN1OXY1(i)+  0.007;fVROCP0OXY4(i)=fVROCP0OXY4(i)+  0.029;fVROCP0OXY2(i)=fVROCP0OXY2(i)+  0.045;fVROCP1OXY3(i)=fVROCP1OXY3(i)+  0.023;fVROCP1OXY1(i)=fVROCP1OXY1(i)+  0.035;fVROCP2OXY2(i)=fVROCP2OXY2(i)+  0.062;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.052;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.051;fVROCP5OXY1(i)=fVROCP5OXY1(i)+  0.035;fVROCP6OXY1(i)=fVROCP6OXY1(i)+  0.075;fOP3(i)=fOP3(i)+  0.016;fOP2(i)=fOP2(i)+  0.006;fDCB1(i)=fDCB1(i)+  0.024;fHKET(i)=fHKET(i)+  0.024;fMEK(i)=fMEK(i)+  0.024;fACD(i)=fACD(i)+  0.024;fALD(i)=fALD(i)+  0.024;fMO2(i)=fMO2(i)+  0.024;fETHP(i)=fETHP(i)+  0.024;fHC3P(i)=fHC3P(i)+  0.024;fMEKP(i)=fMEKP(i)+  0.024;fHC5P(i)=fHC5P(i)+  0.054;

% 499, <ROCOXY7c>
i=i+1;
Rnames{ 499} = 'VROCP0OXY4 + HO = HO +  0.28220*VROCN2OXY8 +  0.11650*VROCN2OXY4 +  0.03200*VROCN1OXY6 +  0.01830*VROCN1OXY3 +  0.00110*VROCP0OXY4 +  0.06600*VROCP2OXY2 +  0.05350*VROCP3OXY2 +  0.02460*VROCP4OXY2 +  0.00540*OP2 +  0.10680*DCB1 +  0.10680*MEK +  0.10680*KET +  0.10680*ACD +  0.10680*ALD +  0.10680*MO2 +  0.10680*ETHP +  0.10680*HC3P +  0.10680*MEKP +  0.10680*HC5P +  0.10680*KETP ';
k(:,i) = (  5.1700E-11 ); 
Gstr{i,   1}='VROCP0OXY4';Gstr{i,   2}='HO';
fVROCP0OXY4(i)=fVROCP0OXY4(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  1.000;fVROCN2OXY8(i)=fVROCN2OXY8(i)+  0.282;fVROCN2OXY4(i)=fVROCN2OXY4(i)+  0.117;fVROCN1OXY6(i)=fVROCN1OXY6(i)+  0.032;fVROCN1OXY3(i)=fVROCN1OXY3(i)+  0.018;fVROCP0OXY4(i)=fVROCP0OXY4(i)+  0.001;fVROCP2OXY2(i)=fVROCP2OXY2(i)+  0.066;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.053;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.025;fOP2(i)=fOP2(i)+  0.005;fDCB1(i)=fDCB1(i)+  0.107;fMEK(i)=fMEK(i)+  0.107;fKET(i)=fKET(i)+  0.107;fACD(i)=fACD(i)+  0.107;fALD(i)=fALD(i)+  0.107;fMO2(i)=fMO2(i)+  0.107;fETHP(i)=fETHP(i)+  0.107;fHC3P(i)=fHC3P(i)+  0.107;fMEKP(i)=fMEKP(i)+  0.107;fHC5P(i)=fHC5P(i)+  0.107;fKETP(i)=fKETP(i)+  0.107;

% 500, <ROCOXY8c>
i=i+1;
Rnames{ 500} = 'VROCP0OXY2 + HO = HO +  0.06590*VROCN2OXY8 +  0.45790*VROCN2OXY4 +  0.11560*VROCN2OXY2 +  0.03250*VROCN1OXY6 +  0.06570*VROCN1OXY3 +  0.00460*VROCN1OXY1 +  0.03070*VROCP0OXY4 +  0.00240*VROCP0OXY2 +  0.03950*VROCP1OXY3 +  0.02150*VROCP1OXY1 +  0.05390*VROCP2OXY2 +  0.05160*VROCP3OXY2 +  0.05190*VROCP4OXY2 +  0.03710*VROCP5OXY1 +  0.04210*VROCP6OXY1 +  0.01050*OP3 +  0.04450*DCB1 +  0.04450*HKET +  0.04450*MEK +  0.04450*ACD +  0.04450*ALD +  0.04450*MO2 +  0.04450*ETHP +  0.04450*HC3P +  0.04450*MEKP +  0.10550*HC5P ';
k(:,i) = (  4.7300E-11 ); 
Gstr{i,   1}='VROCP0OXY2';Gstr{i,   2}='HO';
fVROCP0OXY2(i)=fVROCP0OXY2(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  1.000;fVROCN2OXY8(i)=fVROCN2OXY8(i)+  0.066;fVROCN2OXY4(i)=fVROCN2OXY4(i)+  0.458;fVROCN2OXY2(i)=fVROCN2OXY2(i)+  0.116;fVROCN1OXY6(i)=fVROCN1OXY6(i)+  0.033;fVROCN1OXY3(i)=fVROCN1OXY3(i)+  0.066;fVROCN1OXY1(i)=fVROCN1OXY1(i)+  0.005;fVROCP0OXY4(i)=fVROCP0OXY4(i)+  0.031;fVROCP0OXY2(i)=fVROCP0OXY2(i)+  0.002;fVROCP1OXY3(i)=fVROCP1OXY3(i)+  0.040;fVROCP1OXY1(i)=fVROCP1OXY1(i)+  0.021;fVROCP2OXY2(i)=fVROCP2OXY2(i)+  0.054;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.052;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.052;fVROCP5OXY1(i)=fVROCP5OXY1(i)+  0.037;fVROCP6OXY1(i)=fVROCP6OXY1(i)+  0.042;fOP3(i)=fOP3(i)+  0.011;fDCB1(i)=fDCB1(i)+  0.044;fHKET(i)=fHKET(i)+  0.044;fMEK(i)=fMEK(i)+  0.044;fACD(i)=fACD(i)+  0.044;fALD(i)=fALD(i)+  0.044;fMO2(i)=fMO2(i)+  0.044;fETHP(i)=fETHP(i)+  0.044;fHC3P(i)=fHC3P(i)+  0.044;fMEKP(i)=fMEKP(i)+  0.044;fHC5P(i)=fHC5P(i)+  0.105;

% 501, <ROCOXY9c>
i=i+1;
Rnames{ 501} = 'VROCP1OXY3 + HO = HO +  0.17780*VROCN2OXY8 +  0.19240*VROCN2OXY4 +  0.00040*VROCN2OXY2 +  0.07400*VROCN1OXY6 +  0.04520*VROCN1OXY3 +  0.06310*VROCP0OXY4 +  0.00070*VROCP0OXY2 +  0.00060*VROCP1OXY3 +  0.02270*VROCP2OXY2 +  0.05850*VROCP3OXY2 +  0.06490*VROCP4OXY2 +  0.01740*VROCP5OXY1 +  0.01540*OP3 +  0.01700*OP2 +  0.08180*DCB1 +  0.08180*HKET +  0.08180*MEK +  0.08180*ACD +  0.08180*ALD +  0.08180*MO2 +  0.08180*ETHP +  0.08180*HC3P +  0.08180*MEKP +  0.22200*HC5P ';
k(:,i) = (  4.6000E-11 ); 
Gstr{i,   1}='VROCP1OXY3';Gstr{i,   2}='HO';
fVROCP1OXY3(i)=fVROCP1OXY3(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  1.000;fVROCN2OXY8(i)=fVROCN2OXY8(i)+  0.178;fVROCN2OXY4(i)=fVROCN2OXY4(i)+  0.192;fVROCN2OXY2(i)=fVROCN2OXY2(i)+  0.000;fVROCN1OXY6(i)=fVROCN1OXY6(i)+  0.074;fVROCN1OXY3(i)=fVROCN1OXY3(i)+  0.045;fVROCP0OXY4(i)=fVROCP0OXY4(i)+  0.063;fVROCP0OXY2(i)=fVROCP0OXY2(i)+  0.001;fVROCP1OXY3(i)=fVROCP1OXY3(i)+  0.001;fVROCP2OXY2(i)=fVROCP2OXY2(i)+  0.023;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.059;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.065;fVROCP5OXY1(i)=fVROCP5OXY1(i)+  0.017;fOP3(i)=fOP3(i)+  0.015;fOP2(i)=fOP2(i)+  0.017;fDCB1(i)=fDCB1(i)+  0.082;fHKET(i)=fHKET(i)+  0.082;fMEK(i)=fMEK(i)+  0.082;fACD(i)=fACD(i)+  0.082;fALD(i)=fALD(i)+  0.082;fMO2(i)=fMO2(i)+  0.082;fETHP(i)=fETHP(i)+  0.082;fHC3P(i)=fHC3P(i)+  0.082;fMEKP(i)=fMEKP(i)+  0.082;fHC5P(i)=fHC5P(i)+  0.222;

% 502, <ROCOXY10c>
i=i+1;
Rnames{ 502} = 'VROCP1OXY1 + HO = HO +  0.00230*VROCN2OXY8 +  0.13400*VROCN2OXY4 +  0.33490*VROCN2OXY2 +  0.00800*VROCN1OXY6 +  0.11930*VROCN1OXY3 +  0.07580*VROCN1OXY1 +  0.02920*VROCP0OXY4 +  0.07660*VROCP0OXY2 +  0.02770*VROCP1OXY3 +  0.01180*VROCP1OXY1 +  0.06510*VROCP2OXY2 +  0.07090*VROCP3OXY2 +  0.06680*VROCP4OXY2 +  0.04230*VROCP5OXY1 +  0.09110*VROCP6OXY1 +  0.00660*OP3 +  0.00250*OP2 +  0.02970*DCB1 +  0.02970*HKET +  0.02970*MEK +  0.02970*ACD +  0.02970*ALD +  0.02970*MO2 +  0.02970*ETHP +  0.02970*HC3P +  0.02970*MEKP +  0.06540*HC5P ';
k(:,i) = (  3.8000E-11 ); 
Gstr{i,   1}='VROCP1OXY1';Gstr{i,   2}='HO';
fVROCP1OXY1(i)=fVROCP1OXY1(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  1.000;fVROCN2OXY8(i)=fVROCN2OXY8(i)+  0.002;fVROCN2OXY4(i)=fVROCN2OXY4(i)+  0.134;fVROCN2OXY2(i)=fVROCN2OXY2(i)+  0.335;fVROCN1OXY6(i)=fVROCN1OXY6(i)+  0.008;fVROCN1OXY3(i)=fVROCN1OXY3(i)+  0.119;fVROCN1OXY1(i)=fVROCN1OXY1(i)+  0.076;fVROCP0OXY4(i)=fVROCP0OXY4(i)+  0.029;fVROCP0OXY2(i)=fVROCP0OXY2(i)+  0.077;fVROCP1OXY3(i)=fVROCP1OXY3(i)+  0.028;fVROCP1OXY1(i)=fVROCP1OXY1(i)+  0.012;fVROCP2OXY2(i)=fVROCP2OXY2(i)+  0.065;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.071;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.067;fVROCP5OXY1(i)=fVROCP5OXY1(i)+  0.042;fVROCP6OXY1(i)=fVROCP6OXY1(i)+  0.091;fOP3(i)=fOP3(i)+  0.007;fOP2(i)=fOP2(i)+  0.003;fDCB1(i)=fDCB1(i)+  0.030;fHKET(i)=fHKET(i)+  0.030;fMEK(i)=fMEK(i)+  0.030;fACD(i)=fACD(i)+  0.030;fALD(i)=fALD(i)+  0.030;fMO2(i)=fMO2(i)+  0.030;fETHP(i)=fETHP(i)+  0.030;fHC3P(i)=fHC3P(i)+  0.030;fMEKP(i)=fMEKP(i)+  0.030;fHC5P(i)=fHC5P(i)+  0.065;

% 503, <ROCOXY11c>
i=i+1;
Rnames{ 503} = 'VROCP2OXY2 + HO = HO +  0.04450*VROCN2OXY8 +  0.17260*VROCN2OXY4 +  0.01040*VROCN2OXY2 +  0.05130*VROCN1OXY6 +  0.11180*VROCN1OXY3 +  0.00130*VROCN1OXY1 +  0.13370*VROCP0OXY4 +  0.04030*VROCP0OXY2 +  0.05110*VROCP1OXY3 +  0.00680*VROCP1OXY1 +  0.02360*VROCP2OXY2 +  0.02930*VROCP3OXY2 +  0.07330*VROCP4OXY2 +  0.05230*VROCP5OXY1 +  0.05950*VROCP6OXY1 +  0.00410*OP3 +  0.00230*OP2 +  0.06280*DCB1 +  0.06280*HKET +  0.06280*MEK +  0.06280*ACD +  0.06280*ALD +  0.06280*MO2 +  0.06280*ETHP +  0.06280*HC3P +  0.06280*MEKP +  0.14890*HC5P ';
k(:,i) = (  3.9300E-11 ); 
Gstr{i,   1}='VROCP2OXY2';Gstr{i,   2}='HO';
fVROCP2OXY2(i)=fVROCP2OXY2(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  1.000;fVROCN2OXY8(i)=fVROCN2OXY8(i)+  0.044;fVROCN2OXY4(i)=fVROCN2OXY4(i)+  0.173;fVROCN2OXY2(i)=fVROCN2OXY2(i)+  0.010;fVROCN1OXY6(i)=fVROCN1OXY6(i)+  0.051;fVROCN1OXY3(i)=fVROCN1OXY3(i)+  0.112;fVROCN1OXY1(i)=fVROCN1OXY1(i)+  0.001;fVROCP0OXY4(i)=fVROCP0OXY4(i)+  0.134;fVROCP0OXY2(i)=fVROCP0OXY2(i)+  0.040;fVROCP1OXY3(i)=fVROCP1OXY3(i)+  0.051;fVROCP1OXY1(i)=fVROCP1OXY1(i)+  0.007;fVROCP2OXY2(i)=fVROCP2OXY2(i)+  0.024;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.029;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.073;fVROCP5OXY1(i)=fVROCP5OXY1(i)+  0.052;fVROCP6OXY1(i)=fVROCP6OXY1(i)+  0.059;fOP3(i)=fOP3(i)+  0.004;fOP2(i)=fOP2(i)+  0.002;fDCB1(i)=fDCB1(i)+  0.063;fHKET(i)=fHKET(i)+  0.063;fMEK(i)=fMEK(i)+  0.063;fACD(i)=fACD(i)+  0.063;fALD(i)=fALD(i)+  0.063;fMO2(i)=fMO2(i)+  0.063;fETHP(i)=fETHP(i)+  0.063;fHC3P(i)=fHC3P(i)+  0.063;fMEKP(i)=fMEKP(i)+  0.063;fHC5P(i)=fHC5P(i)+  0.149;

% 504, <ROCOXY12c>
i=i+1;
Rnames{ 504} = 'VROCP3OXY2 + HO = HO +  0.03170*VROCN2OXY8 +  0.07650*VROCN2OXY4 +  0.00090*VROCN2OXY2 +  0.05260*VROCN1OXY6 +  0.04890*VROCN1OXY3 +  0.15500*VROCP0OXY4 +  0.01550*VROCP0OXY2 +  0.10510*VROCP1OXY3 +  0.00130*VROCP1OXY1 +  0.05350*VROCP2OXY2 +  0.00860*VROCP3OXY2 +  0.04260*VROCP4OXY2 +  0.05820*VROCP5OXY1 +  0.06610*VROCP6OXY1 +  0.05060*OP3 +  0.01140*OP2 +  0.06980*DCB1 +  0.06980*HKET +  0.06980*MEK +  0.06980*ACD +  0.06980*ALD +  0.06980*MO2 +  0.06980*ETHP +  0.06980*HC3P +  0.06980*MEKP +  0.16560*HC5P ';
k(:,i) = (  3.5200E-11 ); 
Gstr{i,   1}='VROCP3OXY2';Gstr{i,   2}='HO';
fVROCP3OXY2(i)=fVROCP3OXY2(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  1.000;fVROCN2OXY8(i)=fVROCN2OXY8(i)+  0.032;fVROCN2OXY4(i)=fVROCN2OXY4(i)+  0.076;fVROCN2OXY2(i)=fVROCN2OXY2(i)+  0.001;fVROCN1OXY6(i)=fVROCN1OXY6(i)+  0.053;fVROCN1OXY3(i)=fVROCN1OXY3(i)+  0.049;fVROCP0OXY4(i)=fVROCP0OXY4(i)+  0.155;fVROCP0OXY2(i)=fVROCP0OXY2(i)+  0.015;fVROCP1OXY3(i)=fVROCP1OXY3(i)+  0.105;fVROCP1OXY1(i)=fVROCP1OXY1(i)+  0.001;fVROCP2OXY2(i)=fVROCP2OXY2(i)+  0.053;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.009;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.043;fVROCP5OXY1(i)=fVROCP5OXY1(i)+  0.058;fVROCP6OXY1(i)=fVROCP6OXY1(i)+  0.066;fOP3(i)=fOP3(i)+  0.051;fOP2(i)=fOP2(i)+  0.011;fDCB1(i)=fDCB1(i)+  0.070;fHKET(i)=fHKET(i)+  0.070;fMEK(i)=fMEK(i)+  0.070;fACD(i)=fACD(i)+  0.070;fALD(i)=fALD(i)+  0.070;fMO2(i)=fMO2(i)+  0.070;fETHP(i)=fETHP(i)+  0.070;fHC3P(i)=fHC3P(i)+  0.070;fMEKP(i)=fMEKP(i)+  0.070;fHC5P(i)=fHC5P(i)+  0.166;

% 505, <ROCOXY13c>
i=i+1;
Rnames{ 505} = 'VROCP4OXY2 + HO = HO +  0.01170*VROCN2OXY8 +  0.01670*VROCN2OXY4 +  0.04800*VROCN1OXY6 +  0.02460*VROCN1OXY3 +  0.08810*VROCP0OXY4 +  0.09160*VROCP1OXY3 +  0.00730*VROCP1OXY1 +  0.09720*VROCP2OXY2 +  0.04560*VROCP3OXY2 +  0.00240*VROCP4OXY2 +  0.04790*VROCP5OXY1 +  0.07450*VROCP6OXY1 +  0.06070*OP3 +  0.01550*OP2 +  0.07860*DCB1 +  0.07860*HKET +  0.07860*MEK +  0.07860*ACD +  0.07860*ALD +  0.07860*MO2 +  0.07860*ETHP +  0.07860*HC3P +  0.07860*MEKP +  0.17300*HC5P ';
k(:,i) = (  3.1200E-11 ); 
Gstr{i,   1}='VROCP4OXY2';Gstr{i,   2}='HO';
fVROCP4OXY2(i)=fVROCP4OXY2(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  1.000;fVROCN2OXY8(i)=fVROCN2OXY8(i)+  0.012;fVROCN2OXY4(i)=fVROCN2OXY4(i)+  0.017;fVROCN1OXY6(i)=fVROCN1OXY6(i)+  0.048;fVROCN1OXY3(i)=fVROCN1OXY3(i)+  0.025;fVROCP0OXY4(i)=fVROCP0OXY4(i)+  0.088;fVROCP1OXY3(i)=fVROCP1OXY3(i)+  0.092;fVROCP1OXY1(i)=fVROCP1OXY1(i)+  0.007;fVROCP2OXY2(i)=fVROCP2OXY2(i)+  0.097;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.046;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.002;fVROCP5OXY1(i)=fVROCP5OXY1(i)+  0.048;fVROCP6OXY1(i)=fVROCP6OXY1(i)+  0.074;fOP3(i)=fOP3(i)+  0.061;fOP2(i)=fOP2(i)+  0.015;fDCB1(i)=fDCB1(i)+  0.079;fHKET(i)=fHKET(i)+  0.079;fMEK(i)=fMEK(i)+  0.079;fACD(i)=fACD(i)+  0.079;fALD(i)=fALD(i)+  0.079;fMO2(i)=fMO2(i)+  0.079;fETHP(i)=fETHP(i)+  0.079;fHC3P(i)=fHC3P(i)+  0.079;fMEKP(i)=fMEKP(i)+  0.079;fHC5P(i)=fHC5P(i)+  0.173;

% 506, <ROCOXY14c>
i=i+1;
Rnames{ 506} = 'VROCP5OXY1 + HO = HO +  0.01030*VROCN2OXY4 +  0.00060*VROCN2OXY2 +  0.00900*VROCN1OXY6 +  0.01460*VROCN1OXY3 +  0.07020*VROCP0OXY4 +  0.01530*VROCP0OXY2 +  0.10380*VROCP1OXY3 +  0.00310*VROCP1OXY1 +  0.16500*VROCP2OXY2 +  0.15660*VROCP3OXY2 +  0.07240*VROCP4OXY2 +  0.00620*VROCP5OXY1 +  0.13980*VROCP6OXY1 +  0.02160*OP3 +  0.03840*OP2 +  0.05260*DCB1 +  0.05260*HKET +  0.05260*MEK +  0.05260*ACD +  0.05260*ALD +  0.05260*MO2 +  0.05260*ETHP +  0.05260*HC3P +  0.05260*MEKP +  0.12800*HC5P ';
k(:,i) = (  2.4000E-11 ); 
Gstr{i,   1}='VROCP5OXY1';Gstr{i,   2}='HO';
fVROCP5OXY1(i)=fVROCP5OXY1(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  1.000;fVROCN2OXY4(i)=fVROCN2OXY4(i)+  0.010;fVROCN2OXY2(i)=fVROCN2OXY2(i)+  0.001;fVROCN1OXY6(i)=fVROCN1OXY6(i)+  0.009;fVROCN1OXY3(i)=fVROCN1OXY3(i)+  0.015;fVROCP0OXY4(i)=fVROCP0OXY4(i)+  0.070;fVROCP0OXY2(i)=fVROCP0OXY2(i)+  0.015;fVROCP1OXY3(i)=fVROCP1OXY3(i)+  0.104;fVROCP1OXY1(i)=fVROCP1OXY1(i)+  0.003;fVROCP2OXY2(i)=fVROCP2OXY2(i)+  0.165;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.157;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.072;fVROCP5OXY1(i)=fVROCP5OXY1(i)+  0.006;fVROCP6OXY1(i)=fVROCP6OXY1(i)+  0.140;fOP3(i)=fOP3(i)+  0.022;fOP2(i)=fOP2(i)+  0.038;fDCB1(i)=fDCB1(i)+  0.053;fHKET(i)=fHKET(i)+  0.053;fMEK(i)=fMEK(i)+  0.053;fACD(i)=fACD(i)+  0.053;fALD(i)=fALD(i)+  0.053;fMO2(i)=fMO2(i)+  0.053;fETHP(i)=fETHP(i)+  0.053;fHC3P(i)=fHC3P(i)+  0.053;fMEKP(i)=fMEKP(i)+  0.053;fHC5P(i)=fHC5P(i)+  0.128;

% 507, <ROCOXY15c>
i=i+1;
Rnames{ 507} = 'VROCP6OXY1 + HO = HO +  0.00610*VROCN1OXY6 +  0.00490*VROCN1OXY3 +  0.02240*VROCP0OXY4 +  0.05030*VROCP1OXY3 +  0.00220*VROCP1OXY1 +  0.08790*VROCP2OXY2 +  0.13840*VROCP3OXY2 +  0.14630*VROCP4OXY2 +  0.04320*VROCP5OXY1 +  0.09570*VROCP6OXY1 +  0.03160*OP3 +  0.05850*OP2 +  0.05710*DCB1 +  0.05710*HKET +  0.05710*MEK +  0.05710*ACD +  0.05710*ALD +  0.05710*MO2 +  0.05710*ETHP +  0.05710*HC3P +  0.05710*MEKP +  0.15440*HC5P ';
k(:,i) = (  2.0500E-11 ); 
Gstr{i,   1}='VROCP6OXY1';Gstr{i,   2}='HO';
fVROCP6OXY1(i)=fVROCP6OXY1(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  1.000;fVROCN1OXY6(i)=fVROCN1OXY6(i)+  0.006;fVROCN1OXY3(i)=fVROCN1OXY3(i)+  0.005;fVROCP0OXY4(i)=fVROCP0OXY4(i)+  0.022;fVROCP1OXY3(i)=fVROCP1OXY3(i)+  0.050;fVROCP1OXY1(i)=fVROCP1OXY1(i)+  0.002;fVROCP2OXY2(i)=fVROCP2OXY2(i)+  0.088;fVROCP3OXY2(i)=fVROCP3OXY2(i)+  0.138;fVROCP4OXY2(i)=fVROCP4OXY2(i)+  0.146;fVROCP5OXY1(i)=fVROCP5OXY1(i)+  0.043;fVROCP6OXY1(i)=fVROCP6OXY1(i)+  0.096;fOP3(i)=fOP3(i)+  0.032;fOP2(i)=fOP2(i)+  0.059;fDCB1(i)=fDCB1(i)+  0.057;fHKET(i)=fHKET(i)+  0.057;fMEK(i)=fMEK(i)+  0.057;fACD(i)=fACD(i)+  0.057;fALD(i)=fALD(i)+  0.057;fMO2(i)=fMO2(i)+  0.057;fETHP(i)=fETHP(i)+  0.057;fHC3P(i)=fHC3P(i)+  0.057;fMEKP(i)=fMEKP(i)+  0.057;fHC5P(i)=fHC5P(i)+  0.154;

% 508, <ROCOXY16c>
i=i+1;
Rnames{ 508} = 'OP3 + HO = HO +  0.11880*VROCN2OXY8 +  0.00080*VROCN2OXY4 +  0.03900*VROCN1OXY6 +  0.01140*VROCP0OXY4 +  0.22660*DCB1 +  0.22660*MEK +  0.22660*ACD +  0.22660*ALD +  0.22660*MO2 +  0.22660*ETHP +  0.22660*HC3P +  0.22660*MEKP ';
k(:,i) = (  4.6900E-11 ); 
Gstr{i,   1}='OP3';Gstr{i,   2}='HO';
fOP3(i)=fOP3(i)-1.0;fHO(i)=fHO(i)-1.0;
fHO(i)=fHO(i)+  1.000;fVROCN2OXY8(i)=fVROCN2OXY8(i)+  0.119;fVROCN2OXY4(i)=fVROCN2OXY4(i)+  0.001;fVROCN1OXY6(i)=fVROCN1OXY6(i)+  0.039;fVROCP0OXY4(i)=fVROCP0OXY4(i)+  0.011;fDCB1(i)=fDCB1(i)+  0.227;fMEK(i)=fMEK(i)+  0.227;fACD(i)=fACD(i)+  0.227;fALD(i)=fALD(i)+  0.227;fMO2(i)=fMO2(i)+  0.227;fETHP(i)=fETHP(i)+  0.227;fHC3P(i)=fHC3P(i)+  0.227;fMEKP(i)=fMEKP(i)+  0.227;

