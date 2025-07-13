%Foam Reactions File based on the mech.def file for the saprc22_ae65_aq mechanism.
% # of species   =  298
% # of reactions =  801
% file created by Katie,Tuite,,

% Set constant species by scaling to air number density
N2  =  0.780800000.*M;
O2  =  0.209500000.*M;
H2  =  0.000000560.*M;
CH4 =  0.000001850.*M;


SpeciesToAdd = {...
'NO2'; 'NO'; 'O3P'; 'O3'; 'NO3'; ...
'N2O5'; 'HNO3'; 'O1D'; 'OH'; 'HONO'; ...
'HO2'; 'HNO4'; 'HO2H'; 'CO'; 'CO2'; ...
'SumRO2'; 'SumRCO3'; 'RO2C'; 'RO2XC'; 'MEO2'; ...
'HCHO'; 'MEOOH'; 'MEOH'; 'ETO2'; 'MECHO'; ...
'ROOH'; 'ETOH'; 'BZO2'; 'BZO'; 'MECO3'; ...
'PAN'; 'OACID'; 'PACID'; 'BZCO3'; 'PBZN'; ...
'ALK5'; 'TBUO'; 'R1NO3'; 'ACET'; 'NPHE'; ...
'CRES'; 'NPRAD'; 'NAPPRD'; 'PNAMIN'; 'NAMIN'; ...
'AMINS'; 'HCHO2'; 'HCOOH'; 'SO2'; 'SULF'; ...
'SULRXN'; 'MECHO2'; 'RCHO2'; 'RCHO'; 'GLY'; ...
'BALD'; 'PHEN'; 'NAPS'; 'CATL'; 'AFG2A'; ...
'AFG2B'; 'MACO3'; 'PAHRO2'; 'CATL3'; 'OTHN'; ...
'PHOT'; 'ALK3'; 'IMINE'; 'CLETHE'; 'xHO2'; ...
'xHCHO'; 'yROOH'; 'ACRLNT'; 'PCE'; 'PCLBEN'; ...
'MECL2'; 'ETBR2'; 'ETCL2'; 'ETOX'; 'CHCL3'; ...
'xOH'; 'xNO2'; 'xNO3'; 'xGLY'; 'xHCOOH'; ...
'xMECHO'; 'xETCHO'; 'ETCHO'; 'xGLCHO'; 'GLCHO'; ...
'xMEK'; 'MEK'; 'xACRO'; 'ACRO'; 'xACET'; ...
'xMACR'; 'MACR'; 'xMVK'; 'MVK'; 'xBACL'; ...
'BACL'; 'xMGLY'; 'MGLY'; 'xBUDAL'; 'BUDAL'; ...
'xFURNS'; 'FURNS'; 'xBALD'; 'xBENX'; 'BENX'; ...
'xRCHO'; 'xKET2'; 'KET2'; 'xLVKS'; 'LVKS'; ...
'xOLEA1'; 'OLEA1'; 'xOLEA2'; 'OLEA2'; 'xOLEP'; ...
'OLEP'; 'xOACID'; 'xPACID'; 'xAMINS'; 'xRPNO3'; ...
'RPNO3'; 'xRCNO3'; 'RCNO3'; 'xRHNO3'; 'RHNO3'; ...
'xRDNO3'; 'RDNO3'; 'xHPCRB'; 'HPCRB'; 'xAFG1'; ...
'AFG1'; 'xAFG2A'; 'xAFG2B'; 'xAFG3'; 'AFG3'; ...
'xPAN2'; 'PAN2'; 'xMEO2'; 'xETO2'; 'xMECO3'; ...
'xR2CO3'; 'R2CO3'; 'xMACO3'; 'xTBUO'; 'xBZO'; ...
'yRUOOH'; 'RUOOH'; 'yRAOOH'; 'RAOOH'; 'yHPCRB'; ...
'yRPNO3'; 'zR1NO3'; 'zR2NO3'; 'R2NO3'; 'zRHNO3'; ...
'zRCNO3'; 'zRANO3'; 'RANO3'; 'zRPNO3'; 'zRDNO3'; ...
'zRNNO3'; 'RNNO3'; 'zPAN2'; 'APANS'; 'ETHAN'; ...
'PROP'; 'NC4'; 'ETHEN'; 'ETHEN_OP'; 'NROG'; ...
'PROPE'; 'PROPE_O3'; 'ALK2'; 'ISOP'; 'ISOP_OH'; ...
'ISOP_O3'; 'ISOPRXN'; 'ISOP_N3'; 'BUT13'; 'BUT13_OH'; ...
'BUT13_O3'; 'APINE'; 'APINE_OH'; 'TRPRXN'; 'ALK4'; ...
'BPINE'; 'BPINE_OH'; 'ACETL'; 'BENZ'; 'BENZRO2'; ...
'TOLU'; 'TOLRO2'; 'OXYL'; 'XYNL'; 'XYLRO2'; ...
'MXYL'; 'PXYL'; 'BZ123'; 'BZ124'; 'BZ135'; ...
'C2BEN'; 'MTBE'; 'ALK1'; 'MECHO_OH'; 'GLCHO_HV'; ...
'ACRO_OH'; 'ACRO_HV'; 'MACR_OH'; 'MACR_N3'; 'BUDAL_OH'; ...
'MALAH'; 'ALK5_OH'; 'ALK6'; 'ALK6_OH'; 'OLE1'; ...
'OLE2'; 'OLE2_O3'; 'OLE3'; 'OLE4'; 'OLE4_O3'; ...
'TERP'; 'TERP_OH'; 'TERP_O3'; 'TERP_N3'; 'SESQ'; ...
'SESQ_OH'; 'SESQRXN'; 'SESQ_O3'; 'SESQ_N3'; 'ARO1'; ...
'ARO2'; 'ARO2_OH'; 'STYRS'; 'TAMNS'; 'OLEA1_OH'; ...
'OLEA1_N3'; 'OLEA2_OH'; 'OLEA2_O3'; 'OLEA2_N3'; 'OLEA2_HV'; ...
'LVKS_OH'; 'LVKS_O3'; 'OLEP_OH'; 'OLEP_O3'; 'RCNO3_OH'; ...
'RCNO3_HV'; 'RPNO3_OH'; 'RPNO3_HV'; 'RDNO3_HV'; 'R2NO3_HV'; ...
'RUOOH_OH'; 'HPCRB_OH'; 'ROOH_OH'; 'AFG1_OH'; 'AFG1_HV'; ...
'AFG2A_OH'; 'AFG2B_OH'; 'AFG2B_HV'; 'SOAALK'; 'SVAVB2'; ...
'SVAVB3'; 'SVAVB4'; 'SVAVB1'; 'H2NO3PIJ'; 'H2NO3PK'; ...
'AISO1J'; 'AOLGBJ'; 'AISO2J'; 'ASQTJ'; 'AAVB2J'; ...
'AOLGAJ'; 'AAVB3J'; 'AAVB4J'; 'APOCI'; 'APNCOMI'; ...
'APOCJ'; 'APNCOMJ'; 'PCVOC'; 'PCSOARXN'; 'VLVPO1'; ...
'VSVPO1'; 'VSVPO2'; 'VSVPO3'; 'VIVPO1'; 'VLVOO1'; ...
'VLVOO2'; 'VSVOO2'; 'VSVOO3'; 'VSVOO1'; 'AGLYJ'; ...
'HCHO_PRIMARY'; 'CCHO_PRIMARY'; 'ACRO_PRIMARY'; };


AddSpecies


%   1, <1>
i=i+1;
Rnames{   1} = 'NO2 = NO + O3P ';
k(:,i) = (JNO2_06 ); 
Gstr{i,   1}='NO2';
fNO2(i)=fNO2(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fO3P(i)=fO3P(i)+  1.0000;

%   2, <2>
i=i+1;
Rnames{   2} = 'O3P + O2 + M = O3 ';
k(:,i) = (  6.0000E-34.*(T./300).^(  2.4000E+00) ).*O2.*M; 
Gstr{i,   1}='O3P';
fO3P(i)=fO3P(i)-1.0;
fO3(i)=fO3(i)+  1.0000;

%   3, <3>
i=i+1;
Rnames{   3} = 'O3P + O3 =';
k(:,i) = (  8.0000E-12.*exp( -2.0600E+03./T) ); 
Gstr{i,   1}='O3P';Gstr{i,   2}='O3';
fO3P(i)=fO3P(i)-1.0;fO3(i)=fO3(i)-1.0;


%   4, <4>
i=i+1;
Rnames{   4} = 'O3P + NO = NO2 ';
xko =   9.0000E-32.*M.*exp(  0.0000E+00./T).*(T./300).^ -1.5000E+00;
xkinf =   3.0000E-11.*exp(  0.0000E+00./T).*(T./300).^  0.0000E+00;
xn =   1.0000E+00;
F =   6.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='O3P';Gstr{i,   2}='NO';
fO3P(i)=fO3P(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;

%   5, <5>
i=i+1;
Rnames{   5} = 'O3P + NO2 = NO ';
k(:,i) = (  5.1000E-12.*exp(  2.1000E+02./T) ); 
Gstr{i,   1}='O3P';Gstr{i,   2}='NO2';
fO3P(i)=fO3P(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fNO(i)=fNO(i)+  1.0000;

%   6, <6>
i=i+1;
Rnames{   6} = 'O3P + NO2 = NO3 ';
xko =   2.5000E-31.*M.*exp(  0.0000E+00./T).*(T./300).^ -1.8000E+00;
xkinf =   2.2000E-11.*exp(  0.0000E+00./T).*(T./300).^ -7.0000E-01;
xn =   1.0000E+00;
F =   6.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='O3P';Gstr{i,   2}='NO2';
fO3P(i)=fO3P(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;

%   7, <7>
i=i+1;
Rnames{   7} = 'O3 + NO = NO2 ';
k(:,i) = (  3.0000E-12.*exp( -1.5000E+03./T) ); 
Gstr{i,   1}='O3';Gstr{i,   2}='NO';
fO3(i)=fO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;

%   8, <8>
i=i+1;
Rnames{   8} = 'O3 + NO2 = NO3 ';
k(:,i) = (  1.2000E-13.*exp( -2.4500E+03./T) ); 
Gstr{i,   1}='O3';Gstr{i,   2}='NO2';
fO3(i)=fO3(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;

%   9, <9>
i=i+1;
Rnames{   9} = 'NO + NO3 = 2.00000*NO2 ';
k(:,i) = (  1.5000E-11.*exp(  1.7000E+02./T) ); 
Gstr{i,   1}='NO';Gstr{i,   2}='NO3';
fNO(i)=fNO(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  2.0000;

%  10, <10>
i=i+1;
Rnames{  10} = 'NO + NO + O2 = 2.00000*NO2 ';
k(:,i) = (  3.3000E-39.*exp(  5.3000E+02./T) ).*O2; 
Gstr{i,   1}='NO';Gstr{i,   2}='NO';
fNO(i)=fNO(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  2.0000;

%  11, <11>
i=i+1;
Rnames{  11} = 'NO2 + NO3 = N2O5 ';
xko =   3.6000E-30.*M.*exp(  0.0000E+00./T).*(T./300).^ -4.1000E+00;
xkinf =   1.9000E-12.*exp(  0.0000E+00./T).*(T./300).^  2.0000E-01;
xn =   1.3300E+00;
F =   3.5000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='NO2';Gstr{i,   2}='NO3';
fNO2(i)=fNO2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fN2O5(i)=fN2O5(i)+  1.0000;

%  12, <12>
i=i+1;
Rnames{  12} = 'N2O5 = NO2 + NO3 ';
xko =   1.3000E-03.*M.*exp( -1.1000E+04./T).*(T./300).^ -3.5000E+00;
xkinf =   9.7000E+14.*exp( -1.1080E+04./T).*(T./300).^  1.0000E-01;
xn =   1.3300E+00;
F =   3.5000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='N2O5';
fN2O5(i)=fN2O5(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;fNO3(i)=fNO3(i)+  1.0000;

%  13, <13>
i=i+1;
Rnames{  13} = 'N2O5 + H2O = 2.00000*HNO3 ';
k(:,i) = (  0.0000E+00 ).*H2O; 
Gstr{i,   1}='N2O5';
fN2O5(i)=fN2O5(i)-1.0;
fHNO3(i)=fHNO3(i)+  2.0000;

%  14, <14>
i=i+1;
Rnames{  14} = 'N2O5 + H2O + H2O = 2.00000*HNO3 ';
k(:,i) = (  0.0000E+00 ).*H2O.*H2O; 
Gstr{i,   1}='N2O5';
fN2O5(i)=fN2O5(i)-1.0;
fHNO3(i)=fHNO3(i)+  2.0000;

%  15, <15>
i=i+1;
Rnames{  15} = 'NO2 + NO3 = NO + NO2 ';
k(:,i) = (  4.5000E-14.*exp( -1.2600E+03./T) ); 
Gstr{i,   1}='NO2';Gstr{i,   2}='NO3';
fNO2(i)=fNO2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fNO2(i)=fNO2(i)+  1.0000;

%  16, <16>
i=i+1;
Rnames{  16} = 'NO3 = NO ';
k(:,i) = (JNO3NO_06 ); 
Gstr{i,   1}='NO3';
fNO3(i)=fNO3(i)-1.0;
fNO(i)=fNO(i)+  1.0000;

%  17, <17>
i=i+1;
Rnames{  17} = 'NO3 = NO2 + O3P ';
k(:,i) = (JNO3NO2_6 ); 
Gstr{i,   1}='NO3';
fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;fO3P(i)=fO3P(i)+  1.0000;

%  18, <18>
i=i+1;
Rnames{  18} = 'O3 = O1D ';
k(:,i) = (JO3O1D_06 ); 
Gstr{i,   1}='O3';
fO3(i)=fO3(i)-1.0;
fO1D(i)=fO1D(i)+  1.0000;

%  19, <19>
i=i+1;
Rnames{  19} = 'O3 = O3P ';
k(:,i) = (JO3O3P_06 ); 
Gstr{i,   1}='O3';
fO3(i)=fO3(i)-1.0;
fO3P(i)=fO3P(i)+  1.0000;

%  20, <20>
i=i+1;
Rnames{  20} = 'O1D + H2O = 2.00000*OH ';
k(:,i) = (  1.6300E-10.*exp(  6.0000E+01./T) ).*H2O; 
Gstr{i,   1}='O1D';
fO1D(i)=fO1D(i)-1.0;
fOH(i)=fOH(i)+  2.0000;

%  21, <21>
i=i+1;
Rnames{  21} = 'O1D + M = O3P ';
k(:,i) = (  2.6500E-11.*exp(  9.8000E+01./T) ).*M; 
Gstr{i,   1}='O1D';
fO1D(i)=fO1D(i)-1.0;
fO3P(i)=fO3P(i)+  1.0000;

%  22, <22>
i=i+1;
Rnames{  22} = 'OH + NO = HONO ';
xko =   7.0000E-31.*M.*exp(  0.0000E+00./T).*(T./300).^ -2.6000E+00;
xkinf =   3.6000E-11.*exp(  0.0000E+00./T).*(T./300).^ -1.0000E-01;
xn =   1.0000E+00;
F =   6.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='OH';Gstr{i,   2}='NO';
fOH(i)=fOH(i)-1.0;fNO(i)=fNO(i)-1.0;
fHONO(i)=fHONO(i)+  1.0000;

%  23, <23>
i=i+1;
Rnames{  23} = 'HONO = OH + NO ';
k(:,i) = (JHONO_06 ); 
Gstr{i,   1}='HONO';
fHONO(i)=fHONO(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fNO(i)=fNO(i)+  1.0000;

%  24, <24>
i=i+1;
Rnames{  24} = 'OH + HONO = NO2 ';
k(:,i) = (  1.8000E-11.*exp( -3.9000E+02./T) ); 
Gstr{i,   1}='OH';Gstr{i,   2}='HONO';
fOH(i)=fOH(i)-1.0;fHONO(i)=fHONO(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;

%  25, <25>
i=i+1;
Rnames{  25} = 'OH + NO2 = HNO3 ';
xko =   3.2000E-30.*M.*exp(  0.0000E+00./T).*(T./300).^ -4.5000E+00;
xkinf =   3.0000E-11.*exp(  0.0000E+00./T).*(T./300).^  0.0000E+00;
xn =   1.2400E+00;
F =   4.1000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='OH';Gstr{i,   2}='NO2';
fOH(i)=fOH(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;

%  26, <26>
i=i+1;
Rnames{  26} = 'OH + NO3 = HO2 + NO2 ';
k(:,i) = (  2.2000E-11 ); 
Gstr{i,   1}='OH';Gstr{i,   2}='NO3';
fOH(i)=fOH(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fNO2(i)=fNO2(i)+  1.0000;

%  27, <27>
i=i+1;
Rnames{  27} = 'OH + HNO3 = NO3 ';
 xk0 =   2.4000E-14.*exp(  4.6000E+02./T);
 xk2 =   2.7000E-17.*exp(  2.1990E+03./T);
 xk3 =   6.5000E-34.*exp(  1.3350E+03./T);
k(:,i) = (xk0+xk3.*M./(1.0+xk3.*M./xk2) ); 
Gstr{i,   1}='OH';Gstr{i,   2}='HNO3';
fOH(i)=fOH(i)-1.0;fHNO3(i)=fHNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;

%  28, <28>
i=i+1;
Rnames{  28} = 'HNO3 = OH + NO2 ';
k(:,i) = (JHNO3 ); 
Gstr{i,   1}='HNO3';
fHNO3(i)=fHNO3(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fNO2(i)=fNO2(i)+  1.0000;

%  29, <29>
i=i+1;
Rnames{  29} = 'OH + O3 = HO2 ';
k(:,i) = (  1.7000E-12.*exp( -9.4000E+02./T) ); 
Gstr{i,   1}='OH';Gstr{i,   2}='O3';
fOH(i)=fOH(i)-1.0;fO3(i)=fO3(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

%  30, <30>
i=i+1;
Rnames{  30} = 'HO2 + NO = OH + NO2 ';
k(:,i) = (  3.3000E-12.*exp(  2.7000E+02./T) ); 
Gstr{i,   1}='HO2';Gstr{i,   2}='NO';
fHO2(i)=fHO2(i)-1.0;fNO(i)=fNO(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fNO2(i)=fNO2(i)+  1.0000;

%  31, <31>
i=i+1;
Rnames{  31} = 'HO2 + NO = HNO3 ';
xk0 =   2.3900E-12.*exp( -1.7110E+03./T).*(T./300).^ -1.3770E+01;
xk1 =   1.8300E-32.*exp( -7.7200E+02./T).*(T./300).^ -4.8500E+00;
xk2 =   0.0000E+00.*exp(  0.0000E+00./T);
k(:,i) = (xk0+xk1.*M+xk2 ); 
Gstr{i,   1}='HO2';Gstr{i,   2}='NO';
fHO2(i)=fHO2(i)-1.0;fNO(i)=fNO(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;

%  32, <32>
i=i+1;
Rnames{  32} = 'HO2 + NO + H2O = HNO3 ';
k(:,i) = (  1.2000E-35.*exp(  2.9440E+03./T) ).*H2O; 
Gstr{i,   1}='HO2';Gstr{i,   2}='NO';
fHO2(i)=fHO2(i)-1.0;fNO(i)=fNO(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;

%  33, <33>
i=i+1;
Rnames{  33} = 'HO2 + NO2 = HNO4 ';
xko =   1.4000E-31.*M.*exp(  0.0000E+00./T).*(T./300).^ -3.1000E+00;
xkinf =   4.0000E-12.*exp(  0.0000E+00./T).*(T./300).^  0.0000E+00;
xn =   1.2600E+00;
F =   4.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='HO2';Gstr{i,   2}='NO2';
fHO2(i)=fHO2(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fHNO4(i)=fHNO4(i)+  1.0000;

%  34, <34>
i=i+1;
Rnames{  34} = 'HNO4 = HO2 + NO2 ';
xko =   4.1000E-05.*M.*exp( -1.0650E+04./T).*(T./300).^  0.0000E+00;
xkinf =   6.0000E+15.*exp( -1.1170E+04./T).*(T./300).^  0.0000E+00;
xn =   1.2600E+00;
F =   4.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='HNO4';
fHNO4(i)=fHNO4(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fNO2(i)=fNO2(i)+  1.0000;

%  35, <35>
i=i+1;
Rnames{  35} = 'HNO4 = 0.80000*HO2 +  0.80000*NO2 +  0.20000*OH +  0.20000*NO3 ';
k(:,i) = (JHNO4_06 ); 
Gstr{i,   1}='HNO4';
fHNO4(i)=fHNO4(i)-1.0;
fHO2(i)=fHO2(i)+  0.8000;fNO2(i)=fNO2(i)+  0.8000;fOH(i)=fOH(i)+  0.2000;fNO3(i)=fNO3(i)+  0.2000;

%  36, <36>
i=i+1;
Rnames{  36} = 'HNO4 + OH = NO2 ';
k(:,i) = (  1.3000E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='HNO4';Gstr{i,   2}='OH';
fHNO4(i)=fHNO4(i)-1.0;fOH(i)=fOH(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;

%  37, <37>
i=i+1;
Rnames{  37} = 'HO2 + O3 = OH ';
k(:,i) = (  1.0000E-14.*exp( -4.9000E+02./T) ); 
Gstr{i,   1}='HO2';Gstr{i,   2}='O3';
fHO2(i)=fHO2(i)-1.0;fO3(i)=fO3(i)-1.0;
fOH(i)=fOH(i)+  1.0000;

%  38, <38>
i=i+1;
Rnames{  38} = 'HO2 + HO2 = HO2H ';
xk0 =   3.0000E-13.*exp(  4.6000E+02./T);
xk1 =   2.1000E-33.*exp(  9.2000E+02./T);
k(:,i) = (xk0+xk1.*M ); 
Gstr{i,   1}='HO2';Gstr{i,   2}='HO2';
fHO2(i)=fHO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2H(i)=fHO2H(i)+  1.0000;

%  39, <39>
i=i+1;
Rnames{  39} = 'HO2 + HO2 + H2O = HO2H ';
xk0 =   4.2000E-34.*exp(  2.6600E+03./T);
xk1 =   2.9400E-54.*exp(  3.1200E+03./T);
k(:,i) = (xk0+xk1.*M ).*H2O; 
Gstr{i,   1}='HO2';Gstr{i,   2}='HO2';
fHO2(i)=fHO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2H(i)=fHO2H(i)+  1.0000;

%  40, <40>
i=i+1;
Rnames{  40} = 'NO3 + HO2 = OH + NO2 ';
k(:,i) = (  3.5000E-12 ); 
Gstr{i,   1}='NO3';Gstr{i,   2}='HO2';
fNO3(i)=fNO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fNO2(i)=fNO2(i)+  1.0000;

%  41, <41>
i=i+1;
Rnames{  41} = 'NO3 + NO3 = 2.00000*NO2 ';
k(:,i) = (  8.5000E-13.*exp( -2.4500E+03./T) ); 
Gstr{i,   1}='NO3';Gstr{i,   2}='NO3';
fNO3(i)=fNO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  2.0000;

%  42, <42>
i=i+1;
Rnames{  42} = 'HO2H = 2.00000*OH ';
k(:,i) = (JH2O2 ); 
Gstr{i,   1}='HO2H';
fHO2H(i)=fHO2H(i)-1.0;
fOH(i)=fOH(i)+  2.0000;

%  43, <43>
i=i+1;
Rnames{  43} = 'HO2H + OH = HO2 ';
k(:,i) = (  1.8000E-12 ); 
Gstr{i,   1}='HO2H';Gstr{i,   2}='OH';
fHO2H(i)=fHO2H(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

%  44, <44>
i=i+1;
Rnames{  44} = 'OH + HO2 =';
k(:,i) = (  4.8000E-11.*exp(  2.5000E+02./T) ); 
Gstr{i,   1}='OH';Gstr{i,   2}='HO2';
fOH(i)=fOH(i)-1.0;fHO2(i)=fHO2(i)-1.0;


%  45, <45>
i=i+1;
Rnames{  45} = 'CO + OH = HO2 + CO2 ';
xk0 =   1.4400E-13.*exp(  0.0000E+00./T);
xk1 =   3.4300E-33.*exp(  0.0000E+00./T);
k(:,i) = (xk0+xk1.*M ); 
Gstr{i,   1}='CO';Gstr{i,   2}='OH';
fCO(i)=fCO(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fCO2(i)=fCO2(i)+  1.0000;

%  46, <46>
i=i+1;
Rnames{  46} = 'OH + H2 = HO2 ';
k(:,i) = (  2.8000E-12.*exp( -1.8000E+03./T) ).*H2; 
Gstr{i,   1}='OH';
fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

%  47, <R2NO>
i=i+1;
Rnames{  47} = 'SumRO2 + NO = NO ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='SumRO2';Gstr{i,   2}='NO';
fSumRO2(i)=fSumRO2(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;

%  48, <R2H2>
i=i+1;
Rnames{  48} = 'SumRO2 + HO2 = HO2 ';
k(:,i) = (  1.4900E-11 ); 
Gstr{i,   1}='SumRO2';Gstr{i,   2}='HO2';
fSumRO2(i)=fSumRO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

%  49, <R2N3>
i=i+1;
Rnames{  49} = 'SumRO2 + NO3 = NO3 ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='SumRO2';Gstr{i,   2}='NO3';
fSumRO2(i)=fSumRO2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;

%  50, <R2R2>
i=i+1;
Rnames{  50} = 'SumRO2 + SumRO2 =';
k(:,i) = (  1.6000E-14 ); 
Gstr{i,   1}='SumRO2';Gstr{i,   2}='SumRO2';
fSumRO2(i)=fSumRO2(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;


%  51, <R3N2>
i=i+1;
Rnames{  51} = 'SumRCO3 + NO2 = NO2 ';
k(:,i) = (  7.7000E-12.*(T./300).^( -2.0000E-01) ); 
Gstr{i,   1}='SumRCO3';Gstr{i,   2}='NO2';
fSumRCO3(i)=fSumRCO3(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;

%  52, <R3NO>
i=i+1;
Rnames{  52} = 'SumRCO3 + NO = NO ';
k(:,i) = (  6.7000E-12.*exp(  3.4000E+02./T) ); 
Gstr{i,   1}='SumRCO3';Gstr{i,   2}='NO';
fSumRCO3(i)=fSumRCO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;

%  53, <R3H2>
i=i+1;
Rnames{  53} = 'SumRCO3 + HO2 = HO2 ';
k(:,i) = (  3.1400E-12.*exp(  5.8000E+02./T) ); 
Gstr{i,   1}='SumRCO3';Gstr{i,   2}='HO2';
fSumRCO3(i)=fSumRCO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

%  54, <R3N3>
i=i+1;
Rnames{  54} = 'SumRCO3 + NO3 = NO3 ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='SumRCO3';Gstr{i,   2}='NO3';
fSumRCO3(i)=fSumRCO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;

%  55, <R3R2>
i=i+1;
Rnames{  55} = 'SumRCO3 + SumRO2 =';
k(:,i) = (  4.4000E-13.*exp(  1.0700E+03./T) ); 
Gstr{i,   1}='SumRCO3';Gstr{i,   2}='SumRO2';
fSumRCO3(i)=fSumRCO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;


%  56, <R3R3>
i=i+1;
Rnames{  56} = 'SumRCO3 + SumRCO3 =';
k(:,i) = (  1.7000E-11 ); 
Gstr{i,   1}='SumRCO3';Gstr{i,   2}='SumRCO3';
fSumRCO3(i)=fSumRCO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;


%  57, <57>
i=i+1;
Rnames{  57} = 'RO2C + NO = NO2 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='RO2C';Gstr{i,   2}='NO';
fRO2C(i)=fRO2C(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;

%  58, <58>
i=i+1;
Rnames{  58} = 'RO2C + HO2 =';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='RO2C';Gstr{i,   2}='HO2';
fRO2C(i)=fRO2C(i)-1.0;fHO2(i)=fHO2(i)-1.0;


%  59, <59>
i=i+1;
Rnames{  59} = 'RO2C + NO3 = NO2 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='RO2C';Gstr{i,   2}='NO3';
fRO2C(i)=fRO2C(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;

%  60, <60>
i=i+1;
Rnames{  60} = 'RO2C + SumRO2 = SumRO2 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='RO2C';Gstr{i,   2}='SumRO2';
fRO2C(i)=fRO2C(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;

%  61, <61>
i=i+1;
Rnames{  61} = 'RO2C + SumRCO3 = SumRCO3 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='RO2C';Gstr{i,   2}='SumRCO3';
fRO2C(i)=fRO2C(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;

%  62, <62>
i=i+1;
Rnames{  62} = 'RO2XC + NO =';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='RO2XC';Gstr{i,   2}='NO';
fRO2XC(i)=fRO2XC(i)-1.0;fNO(i)=fNO(i)-1.0;


%  63, <63>
i=i+1;
Rnames{  63} = 'RO2XC + HO2 =';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='RO2XC';Gstr{i,   2}='HO2';
fRO2XC(i)=fRO2XC(i)-1.0;fHO2(i)=fHO2(i)-1.0;


%  64, <64>
i=i+1;
Rnames{  64} = 'RO2XC + NO3 = NO2 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='RO2XC';Gstr{i,   2}='NO3';
fRO2XC(i)=fRO2XC(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;

%  65, <65>
i=i+1;
Rnames{  65} = 'RO2XC + SumRO2 = SumRO2 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='RO2XC';Gstr{i,   2}='SumRO2';
fRO2XC(i)=fRO2XC(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;

%  66, <66>
i=i+1;
Rnames{  66} = 'RO2XC + SumRCO3 = SumRCO3 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='RO2XC';Gstr{i,   2}='SumRCO3';
fRO2XC(i)=fRO2XC(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;

%  67, <67>
i=i+1;
Rnames{  67} = 'MEO2 + NO = NO2 + HCHO + HO2 ';
k(:,i) = (  2.8000E-12.*exp(  3.0000E+02./T) ); 
Gstr{i,   1}='MEO2';Gstr{i,   2}='NO';
fMEO2(i)=fMEO2(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;fHCHO(i)=fHCHO(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

%  68, <68>
i=i+1;
Rnames{  68} = 'MEO2 + HO2 = 0.90000*MEOOH +  0.10000*HCHO ';
k(:,i) = (  3.8000E-13.*exp(  7.8000E+02./T) ); 
Gstr{i,   1}='MEO2';Gstr{i,   2}='HO2';
fMEO2(i)=fMEO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fMEOOH(i)=fMEOOH(i)+  0.9000;fHCHO(i)=fHCHO(i)+  0.1000;

%  69, <69>
i=i+1;
Rnames{  69} = 'MEO2 + NO3 = HCHO + HO2 + NO2 ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='MEO2';Gstr{i,   2}='NO3';
fMEO2(i)=fMEO2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHCHO(i)=fHCHO(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;fNO2(i)=fNO2(i)+  1.0000;

%  70, <70>
i=i+1;
Rnames{  70} = 'MEO2 + SumRO2 = 0.30000*HO2 +  0.65000*HCHO +  0.35000*MEOH ';
k(:,i) = (  2.1600E-13 ); 
Gstr{i,   1}='MEO2';Gstr{i,   2}='SumRO2';
fMEO2(i)=fMEO2(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fHO2(i)=fHO2(i)+  0.3000;fHCHO(i)=fHCHO(i)+  0.6500;fMEOH(i)=fMEOH(i)+  0.3500;

%  71, <71>
i=i+1;
Rnames{  71} = 'MEO2 + SumRCO3 = 0.90000*HO2 + HCHO ';
k(:,i) = (  2.0000E-12.*exp(  5.0000E+02./T) ); 
Gstr{i,   1}='MEO2';Gstr{i,   2}='SumRCO3';
fMEO2(i)=fMEO2(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.9000;fHCHO(i)=fHCHO(i)+  1.0000;

%  72, <72>
i=i+1;
Rnames{  72} = 'ETO2 + NO = NO2 + HO2 + MECHO ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='ETO2';Gstr{i,   2}='NO';
fETO2(i)=fETO2(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;fMECHO(i)=fMECHO(i)+  1.0000;

%  73, <73>
i=i+1;
Rnames{  73} = 'ETO2 + HO2 = ROOH ';
k(:,i) = (  7.4400E-12 ); 
Gstr{i,   1}='ETO2';Gstr{i,   2}='HO2';
fETO2(i)=fETO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fROOH(i)=fROOH(i)+  1.0000;

%  74, <74>
i=i+1;
Rnames{  74} = 'ETO2 + NO3 = NO2 + HO2 + MECHO ';
k(:,i) = (  2.3000E-12 ); 
Gstr{i,   1}='ETO2';Gstr{i,   2}='NO3';
fETO2(i)=fETO2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;fMECHO(i)=fMECHO(i)+  1.0000;

%  75, <75>
i=i+1;
Rnames{  75} = 'ETO2 + SumRO2 = 0.50000*HO2 +  0.25000*ETOH +  0.75000*MECHO ';
k(:,i) = (  2.9000E-14 ); 
Gstr{i,   1}='ETO2';Gstr{i,   2}='SumRO2';
fETO2(i)=fETO2(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fHO2(i)=fHO2(i)+  0.5000;fETOH(i)=fETOH(i)+  0.2500;fMECHO(i)=fMECHO(i)+  0.7500;

%  76, <76>
i=i+1;
Rnames{  76} = 'ETO2 + SumRCO3 = 0.80000*HO2 + MECHO ';
k(:,i) = (  1.6000E-11 ); 
Gstr{i,   1}='ETO2';Gstr{i,   2}='SumRCO3';
fETO2(i)=fETO2(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.8000;fMECHO(i)=fMECHO(i)+  1.0000;

%  77, <77>
i=i+1;
Rnames{  77} = 'BZO2 + NO = NO2 + BZO ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='BZO2';Gstr{i,   2}='NO';
fBZO2(i)=fBZO2(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;fBZO(i)=fBZO(i)+  1.0000;

%  78, <78>
i=i+1;
Rnames{  78} = 'BZO2 + HO2 = ROOH ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='BZO2';Gstr{i,   2}='HO2';
fBZO2(i)=fBZO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fROOH(i)=fROOH(i)+  1.0000;

%  79, <79>
i=i+1;
Rnames{  79} = 'BZO2 + NO3 = BZO + NO2 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='BZO2';Gstr{i,   2}='NO3';
fBZO2(i)=fBZO2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fBZO(i)=fBZO(i)+  1.0000;fNO2(i)=fNO2(i)+  1.0000;

%  80, <80>
i=i+1;
Rnames{  80} = 'BZO2 + SumRO2 = SumRO2 + BZO ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='BZO2';Gstr{i,   2}='SumRO2';
fBZO2(i)=fBZO2(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fBZO(i)=fBZO(i)+  1.0000;

%  81, <81>
i=i+1;
Rnames{  81} = 'BZO2 + SumRCO3 = SumRCO3 + BZO ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='BZO2';Gstr{i,   2}='SumRCO3';
fBZO2(i)=fBZO2(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fBZO(i)=fBZO(i)+  1.0000;

%  82, <Q1NO>
i=i+1;
Rnames{  82} = 'MECO3 + NO = NO2 + MEO2 + CO2 + SumRO2 ';
k(:,i) = (  8.1000E-12.*exp(  2.7000E+02./T) ); 
Gstr{i,   1}='MECO3';Gstr{i,   2}='NO';
fMECO3(i)=fMECO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;fMEO2(i)=fMEO2(i)+  1.0000;fCO2(i)=fCO2(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

%  83, <Q1N2>
i=i+1;
Rnames{  83} = 'MECO3 + NO2 = PAN ';
xko =   9.7000E-29.*M.*exp(  0.0000E+00./T).*(T./300).^ -5.6000E+00;
xkinf =   9.3000E-12.*exp(  0.0000E+00./T).*(T./300).^ -1.5000E+00;
xn =   1.0000E+00;
F =   6.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='MECO3';Gstr{i,   2}='NO2';
fMECO3(i)=fMECO3(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fPAN(i)=fPAN(i)+  1.0000;

%  84, <Q1N3>
i=i+1;
Rnames{  84} = 'MECO3 + NO3 = NO2 + MEO2 + CO2 + SumRO2 ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='MECO3';Gstr{i,   2}='NO3';
fMECO3(i)=fMECO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;fMEO2(i)=fMEO2(i)+  1.0000;fCO2(i)=fCO2(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

%  85, <Q1H2>
i=i+1;
Rnames{  85} = 'MECO3 + HO2 = 0.13000*O3 +  0.50000*OH +  0.50000*MEO2 +  0.13000*OACID +  0.37000*PACID +  0.50000*CO2 +  0.50000*SumRO2 ';
k(:,i) = (  2.2000E-11 ); 
Gstr{i,   1}='MECO3';Gstr{i,   2}='HO2';
fMECO3(i)=fMECO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fO3(i)=fO3(i)+  0.1300;fOH(i)=fOH(i)+  0.5000;fMEO2(i)=fMEO2(i)+  0.5000;fOACID(i)=fOACID(i)+  0.1300;fPACID(i)=fPACID(i)+  0.3700;fCO2(i)=fCO2(i)+  0.5000;fSumRO2(i)=fSumRO2(i)+  0.5000;

%  86, <Q1R2>
i=i+1;
Rnames{  86} = 'MECO3 + SumRO2 = 0.90000*MEO2 +  0.10000*OACID +  0.90000*CO2 +  0.90000*SumRO2 ';
k(:,i) = (  1.6000E-11 ); 
Gstr{i,   1}='MECO3';Gstr{i,   2}='SumRO2';
fMECO3(i)=fMECO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fMEO2(i)=fMEO2(i)+  0.9000;fOACID(i)=fOACID(i)+  0.1000;fCO2(i)=fCO2(i)+  0.9000;fSumRO2(i)=fSumRO2(i)+  0.9000;

%  87, <Q1R3>
i=i+1;
Rnames{  87} = 'MECO3 + SumRCO3 = MEO2 + CO2 + SumRO2 ';
k(:,i) = (  1.4000E-11 ); 
Gstr{i,   1}='MECO3';Gstr{i,   2}='SumRCO3';
fMECO3(i)=fMECO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fMEO2(i)=fMEO2(i)+  1.0000;fCO2(i)=fCO2(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

%  88, <Q6N2>
i=i+1;
Rnames{  88} = 'BZCO3 + NO2 = PBZN ';
k(:,i) = (  1.1100E-11 ); 
Gstr{i,   1}='BZCO3';Gstr{i,   2}='NO2';
fBZCO3(i)=fBZCO3(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fPBZN(i)=fPBZN(i)+  1.0000;

%  89, <Q6NO>
i=i+1;
Rnames{  89} = 'BZCO3 + NO = NO2 + CO2 + BZO2 + SumRO2 ';
k(:,i) = (  1.6000E-11 ); 
Gstr{i,   1}='BZCO3';Gstr{i,   2}='NO';
fBZCO3(i)=fBZCO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;fCO2(i)=fCO2(i)+  1.0000;fBZO2(i)=fBZO2(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

%  90, <Q6H2>
i=i+1;
Rnames{  90} = 'BZCO3 + HO2 = 0.50000*ALK5 +  0.13000*O3 +  0.50000*OH +  0.50000*BZO2 +  0.50000*CO2 +  0.50000*SumRO2 ';
k(:,i) = (k(:,  53) ); 
Gstr{i,   1}='BZCO3';Gstr{i,   2}='HO2';
fBZCO3(i)=fBZCO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fALK5(i)=fALK5(i)+  0.5000;fO3(i)=fO3(i)+  0.1300;fOH(i)=fOH(i)+  0.5000;fBZO2(i)=fBZO2(i)+  0.5000;fCO2(i)=fCO2(i)+  0.5000;fSumRO2(i)=fSumRO2(i)+  0.5000;

%  91, <Q6N3>
i=i+1;
Rnames{  91} = 'BZCO3 + NO3 = NO2 + CO2 + BZO2 + SumRO2 ';
k(:,i) = (k(:,  54) ); 
Gstr{i,   1}='BZCO3';Gstr{i,   2}='NO3';
fBZCO3(i)=fBZCO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;fCO2(i)=fCO2(i)+  1.0000;fBZO2(i)=fBZO2(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

%  92, <Q6R2>
i=i+1;
Rnames{  92} = 'BZCO3 + SumRO2 = 2.00000*SumRO2 + BZO2 + CO2 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='BZCO3';Gstr{i,   2}='SumRO2';
fBZCO3(i)=fBZCO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  2.0000;fBZO2(i)=fBZO2(i)+  1.0000;fCO2(i)=fCO2(i)+  1.0000;

%  93, <Q6R3>
i=i+1;
Rnames{  93} = 'BZCO3 + SumRCO3 = SumRCO3 + CO2 + BZO2 + SumRO2 ';
k(:,i) = (k(:,  56) ); 
Gstr{i,   1}='BZCO3';Gstr{i,   2}='SumRCO3';
fBZCO3(i)=fBZCO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fCO2(i)=fCO2(i)+  1.0000;fBZO2(i)=fBZO2(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

%  94, <94>
i=i+1;
Rnames{  94} = 'TBUO + NO2 = R1NO3 ';
k(:,i) = (  3.5000E-12.*exp(  5.5300E+02./T) ); 
Gstr{i,   1}='TBUO';Gstr{i,   2}='NO2';
fTBUO(i)=fTBUO(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fR1NO3(i)=fR1NO3(i)+  1.0000;

%  95, <95>
i=i+1;
Rnames{  95} = 'TBUO = ACET + MEO2 + SumRO2 ';
k(:,i) = (  1.4000E+13.*exp( -6.8560E+03./T) ); 
Gstr{i,   1}='TBUO';
fTBUO(i)=fTBUO(i)-1.0;
fACET(i)=fACET(i)+  1.0000;fMEO2(i)=fMEO2(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

%  96, <96>
i=i+1;
Rnames{  96} = 'BZO + NO2 = NPHE ';
k(:,i) = (  2.0800E-12 ); 
Gstr{i,   1}='BZO';Gstr{i,   2}='NO2';
fBZO(i)=fBZO(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fNPHE(i)=fNPHE(i)+  1.0000;

%  97, <97>
i=i+1;
Rnames{  97} = 'BZO + HO2 = CRES ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='BZO';Gstr{i,   2}='HO2';
fBZO(i)=fBZO(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fCRES(i)=fCRES(i)+  1.0000;

%  98, <98>
i=i+1;
Rnames{  98} = 'BZO + O3 = BZO2 + SumRO2 ';
k(:,i) = (  2.8600E-13 ); 
Gstr{i,   1}='BZO';Gstr{i,   2}='O3';
fBZO(i)=fBZO(i)-1.0;fO3(i)=fO3(i)-1.0;
fBZO2(i)=fBZO2(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

%  99, <99>
i=i+1;
Rnames{  99} = 'BZO + BZO =';
k(:,i) = (  1.4900E-11 ); 
Gstr{i,   1}='BZO';Gstr{i,   2}='BZO';
fBZO(i)=fBZO(i)-1.0;fBZO(i)=fBZO(i)-1.0;


% 100, <100>
i=i+1;
Rnames{ 100} = 'NPRAD + NO2 = NPHE ';
k(:,i) = (k(:,  51) ); 
Gstr{i,   1}='NPRAD';Gstr{i,   2}='NO2';
fNPRAD(i)=fNPRAD(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fNPHE(i)=fNPHE(i)+  1.0000;

% 101, <101>
i=i+1;
Rnames{ 101} = 'NPRAD + HO2 = NAPPRD ';
k(:,i) = (k(:,  53) ); 
Gstr{i,   1}='NPRAD';Gstr{i,   2}='HO2';
fNPRAD(i)=fNPRAD(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fNAPPRD(i)=fNAPPRD(i)+  1.0000;

% 102, <102>
i=i+1;
Rnames{ 102} = 'NPRAD = NAPPRD ';
k(:,i) = (  1.0000E-03 ); 
Gstr{i,   1}='NPRAD';
fNPRAD(i)=fNPRAD(i)-1.0;
fNAPPRD(i)=fNAPPRD(i)+  1.0000;

% 103, <103>
i=i+1;
Rnames{ 103} = 'PNAMIN + NO2 = NAMIN ';
k(:,i) = (k(:,  51) ); 
Gstr{i,   1}='PNAMIN';Gstr{i,   2}='NO2';
fPNAMIN(i)=fPNAMIN(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fNAMIN(i)=fNAMIN(i)+  1.0000;

% 104, <104>
i=i+1;
Rnames{ 104} = 'PNAMIN + HO2 = AMINS ';
k(:,i) = (k(:,  53) ); 
Gstr{i,   1}='PNAMIN';Gstr{i,   2}='HO2';
fPNAMIN(i)=fPNAMIN(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fAMINS(i)=fAMINS(i)+  1.0000;

% 105, <105>
i=i+1;
Rnames{ 105} = 'PNAMIN = AMINS ';
k(:,i) = (  1.0000E-03 ); 
Gstr{i,   1}='PNAMIN';
fPNAMIN(i)=fPNAMIN(i)-1.0;
fAMINS(i)=fAMINS(i)+  1.0000;

% 106, <G1N2>
i=i+1;
Rnames{ 106} = 'HCHO2 + NO2 = HCHO + NO3 ';
k(:,i) = (  7.0000E-12 ); 
Gstr{i,   1}='HCHO2';Gstr{i,   2}='NO2';
fHCHO2(i)=fHCHO2(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fHCHO(i)=fHCHO(i)+  1.0000;fNO3(i)=fNO3(i)+  1.0000;

% 107, <G1WA>
i=i+1;
Rnames{ 107} = 'HCHO2 + H2O = HCOOH ';
k(:,i) = (  2.4000E-15 ).*H2O; 
Gstr{i,   1}='HCHO2';
fHCHO2(i)=fHCHO2(i)-1.0;
fHCOOH(i)=fHCOOH(i)+  1.0000;

% 108, <G1S2>
i=i+1;
Rnames{ 108} = 'HCHO2 + SO2 = SULF + HCHO + SULRXN ';
k(:,i) = (  3.8000E-11 ); 
Gstr{i,   1}='HCHO2';Gstr{i,   2}='SO2';
fHCHO2(i)=fHCHO2(i)-1.0;fSO2(i)=fSO2(i)-1.0;
fSULF(i)=fSULF(i)+  1.0000;fHCHO(i)=fHCHO(i)+  1.0000;fSULRXN(i)=fSULRXN(i)+  1.0000;

% 109, <G2N2>
i=i+1;
Rnames{ 109} = 'MECHO2 + NO2 = MECHO + NO3 ';
k(:,i) = (  7.0000E-12 ); 
Gstr{i,   1}='MECHO2';Gstr{i,   2}='NO2';
fMECHO2(i)=fMECHO2(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fMECHO(i)=fMECHO(i)+  1.0000;fNO3(i)=fNO3(i)+  1.0000;

% 110, <G2WA>
i=i+1;
Rnames{ 110} = 'MECHO2 + H2O = OACID ';
k(:,i) = (  2.4000E-15 ).*H2O; 
Gstr{i,   1}='MECHO2';
fMECHO2(i)=fMECHO2(i)-1.0;
fOACID(i)=fOACID(i)+  1.0000;

% 111, <G2S2>
i=i+1;
Rnames{ 111} = 'MECHO2 + SO2 = SULF + MECHO + SULRXN ';
k(:,i) = (  3.8000E-11 ); 
Gstr{i,   1}='MECHO2';Gstr{i,   2}='SO2';
fMECHO2(i)=fMECHO2(i)-1.0;fSO2(i)=fSO2(i)-1.0;
fSULF(i)=fSULF(i)+  1.0000;fMECHO(i)=fMECHO(i)+  1.0000;fSULRXN(i)=fSULRXN(i)+  1.0000;

% 112, <G3N2>
i=i+1;
Rnames{ 112} = 'RCHO2 + NO2 = RCHO + NO3 ';
k(:,i) = (  7.0000E-12 ); 
Gstr{i,   1}='RCHO2';Gstr{i,   2}='NO2';
fRCHO2(i)=fRCHO2(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fRCHO(i)=fRCHO(i)+  1.0000;fNO3(i)=fNO3(i)+  1.0000;

% 113, <G3WA>
i=i+1;
Rnames{ 113} = 'RCHO2 + H2O = OACID ';
k(:,i) = (  2.4000E-15 ).*H2O; 
Gstr{i,   1}='RCHO2';
fRCHO2(i)=fRCHO2(i)-1.0;
fOACID(i)=fOACID(i)+  1.0000;

% 114, <G3S2>
i=i+1;
Rnames{ 114} = 'RCHO2 + SO2 = SULF + RCHO + SULRXN ';
k(:,i) = (  3.8000E-11 ); 
Gstr{i,   1}='RCHO2';Gstr{i,   2}='SO2';
fRCHO2(i)=fRCHO2(i)-1.0;fSO2(i)=fSO2(i)-1.0;
fSULF(i)=fSULF(i)+  1.0000;fRCHO(i)=fRCHO(i)+  1.0000;fSULRXN(i)=fSULRXN(i)+  1.0000;

% 115, <S2OH>
i=i+1;
Rnames{ 115} = 'SO2 + OH = HO2 + SULF + SULRXN ';
xko =   3.3000E-31.*M.*exp(  0.0000E+00./T).*(T./300).^ -4.3000E+00;
xkinf =   1.6000E-12.*exp(  0.0000E+00./T).*(T./300).^  0.0000E+00;
xn =   1.0000E+00;
F =   6.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='SO2';Gstr{i,   2}='OH';
fSO2(i)=fSO2(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fSULF(i)=fSULF(i)+  1.0000;fSULRXN(i)=fSULRXN(i)+  1.0000;

% 116, <C1OH>
i=i+1;
Rnames{ 116} = 'OH + CH4 = MEO2 + SumRO2 ';
k(:,i) = (  2.4500E-12.*exp( -1.7750E+03./T) ).*CH4; 
Gstr{i,   1}='OH';
fOH(i)=fOH(i)-1.0;
fMEO2(i)=fMEO2(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 117, <117>
i=i+1;
Rnames{ 117} = 'HCHO = 2.00000*HO2 + CO ';
k(:,i) = (JHCHOR_13 ); 
Gstr{i,   1}='HCHO';
fHCHO(i)=fHCHO(i)-1.0;
fHO2(i)=fHO2(i)+  2.0000;fCO(i)=fCO(i)+  1.0000;

% 118, <118>
i=i+1;
Rnames{ 118} = 'HCHO = CO ';
k(:,i) = (JHCHOM_13 ); 
Gstr{i,   1}='HCHO';
fHCHO(i)=fHCHO(i)-1.0;
fCO(i)=fCO(i)+  1.0000;

% 119, <119>
i=i+1;
Rnames{ 119} = 'HCHO + OH = HO2 + CO ';
k(:,i) = (  5.5000E-12.*exp(  1.2500E+02./T) ); 
Gstr{i,   1}='HCHO';Gstr{i,   2}='OH';
fHCHO(i)=fHCHO(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fCO(i)=fCO(i)+  1.0000;

% 120, <120>
i=i+1;
Rnames{ 120} = 'HCHO + NO3 = HNO3 + HO2 + CO ';
k(:,i) = (  5.8000E-16 ); 
Gstr{i,   1}='HCHO';Gstr{i,   2}='NO3';
fHCHO(i)=fHCHO(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;fCO(i)=fCO(i)+  1.0000;

% 121, <P1UI>
i=i+1;
Rnames{ 121} = 'PAN = NO2 + MECO3 + SumRCO3 ';
xko =   1.0800E+00.*M.*exp( -1.4000E+04./T).*(T./300).^ -5.6000E+00;
xkinf =   1.0300E+17.*exp( -1.4000E+04./T).*(T./300).^ -1.5000E+00;
xn =   1.0000E+00;
F =   6.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='PAN';
fPAN(i)=fPAN(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;fMECO3(i)=fMECO3(i)+  1.0000;fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 122, <P1HV>
i=i+1;
Rnames{ 122} = 'PAN = 0.60000*NO2 +  0.40000*NO3 +  0.40000*MEO2 +  0.60000*MECO3 +  0.40000*CO2 +  0.40000*SumRO2 +  0.60000*SumRCO3 ';
k(:,i) = (JPAN_11 ); 
Gstr{i,   1}='PAN';
fPAN(i)=fPAN(i)-1.0;
fNO2(i)=fNO2(i)+  0.6000;fNO3(i)=fNO3(i)+  0.4000;fMEO2(i)=fMEO2(i)+  0.4000;fMECO3(i)=fMECO3(i)+  0.6000;fCO2(i)=fCO2(i)+  0.4000;fSumRO2(i)=fSumRO2(i)+  0.4000;fSumRCO3(i)=fSumRCO3(i)+  0.6000;

% 123, <123>
i=i+1;
Rnames{ 123} = 'GLY = 2.00000*CO +  2.00000*HO2 ';
k(:,i) = (JGLY_I13R ); 
Gstr{i,   1}='GLY';
fGLY(i)=fGLY(i)-1.0;
fCO(i)=fCO(i)+  2.0000;fHO2(i)=fHO2(i)+  2.0000;

% 124, <124>
i=i+1;
Rnames{ 124} = 'GLY = HCHO + CO ';
k(:,i) = (JGLY_I13M ); 
Gstr{i,   1}='GLY';
fGLY(i)=fGLY(i)-1.0;
fHCHO(i)=fHCHO(i)+  1.0000;fCO(i)=fCO(i)+  1.0000;

% 125, <125>
i=i+1;
Rnames{ 125} = 'GLY + OH = 1.70000*CO +  0.70000*HO2 +  0.30000*OH +  0.30000*CO2 ';
k(:,i) = (  1.1500E-11 ); 
Gstr{i,   1}='GLY';Gstr{i,   2}='OH';
fGLY(i)=fGLY(i)-1.0;fOH(i)=fOH(i)-1.0;
fCO(i)=fCO(i)+  1.7000;fHO2(i)=fHO2(i)+  0.7000;fOH(i)=fOH(i)+  0.3000;fCO2(i)=fCO2(i)+  0.3000;

% 126, <126>
i=i+1;
Rnames{ 126} = 'GLY + NO3 = HNO3 +  1.70000*CO +  0.70000*HO2 +  0.30000*OH +  0.30000*CO2 ';
k(:,i) = (  4.0000E-16 ); 
Gstr{i,   1}='GLY';Gstr{i,   2}='NO3';
fGLY(i)=fGLY(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;fCO(i)=fCO(i)+  1.7000;fHO2(i)=fHO2(i)+  0.7000;fOH(i)=fOH(i)+  0.3000;fCO2(i)=fCO2(i)+  0.3000;

% 127, <BLOH>
i=i+1;
Rnames{ 127} = 'BALD + OH = BZCO3 + SumRCO3 ';
k(:,i) = (  1.2000E-11 ); 
Gstr{i,   1}='BALD';Gstr{i,   2}='OH';
fBALD(i)=fBALD(i)-1.0;fOH(i)=fOH(i)-1.0;
fBZCO3(i)=fBZCO3(i)+  1.0000;fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 128, <BLHV>
i=i+1;
Rnames{ 128} = 'BALD =';
k(:,i) = (  9.0000E-02.*JBALD_11 ); 
Gstr{i,   1}='BALD';
fBALD(i)=fBALD(i)-1.0;


% 129, <BLN3>
i=i+1;
Rnames{ 129} = 'BALD + NO3 = HNO3 + BZCO3 + SumRCO3 ';
k(:,i) = (  4.0000E-15 ); 
Gstr{i,   1}='BALD';Gstr{i,   2}='NO3';
fBALD(i)=fBALD(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;fBZCO3(i)=fBZCO3(i)+  1.0000;fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 130, <PBUI>
i=i+1;
Rnames{ 130} = 'PBZN = BZCO3 + NO2 + SumRCO3 ';
k(:,i) = (  2.1000E+16.*exp( -1.3600E+04./T) ); 
Gstr{i,   1}='PBZN';
fPBZN(i)=fPBZN(i)-1.0;
fBZCO3(i)=fBZCO3(i)+  1.0000;fNO2(i)=fNO2(i)+  1.0000;fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 131, <PBHV>
i=i+1;
Rnames{ 131} = 'PBZN = 0.60000*BZCO3 +  0.60000*NO2 +  0.40000*CO2 +  0.40000*BZO2 +  0.40000*NO3 +  0.40000*SumRO2 +  0.60000*SumRCO3 ';
k(:,i) = (JPPN_11 ); 
Gstr{i,   1}='PBZN';
fPBZN(i)=fPBZN(i)-1.0;
fBZCO3(i)=fBZCO3(i)+  0.6000;fNO2(i)=fNO2(i)+  0.6000;fCO2(i)=fCO2(i)+  0.4000;fBZO2(i)=fBZO2(i)+  0.4000;fNO3(i)=fNO3(i)+  0.4000;fSumRO2(i)=fSumRO2(i)+  0.4000;fSumRCO3(i)=fSumRCO3(i)+  0.6000;

% 132, <NPOH>
i=i+1;
Rnames{ 132} = 'NPHE + OH = BZO + NO2 ';
k(:,i) = (  3.5000E-12 ); 
Gstr{i,   1}='NPHE';Gstr{i,   2}='OH';
fNPHE(i)=fNPHE(i)-1.0;fOH(i)=fOH(i)-1.0;
fBZO(i)=fBZO(i)+  1.0000;fNO2(i)=fNO2(i)+  1.0000;

% 133, <NPHV>
i=i+1;
Rnames{ 133} = 'NPHE = HONO + PHEN ';
k(:,i) = (  1.5000E-03.*JNO2_06 ); 
Gstr{i,   1}='NPHE';
fNPHE(i)=fNPHE(i)-1.0;
fHONO(i)=fHONO(i)+  1.0000;fPHEN(i)=fPHEN(i)+  1.0000;

% 134, <NAOH>
i=i+1;
Rnames{ 134} = 'NAPS + OH = 0.74100*HO2 +  0.70700*CATL +  0.03400*RO2C +  0.01700*AFG2A +  0.01700*AFG2B +  0.03400*GLY +  0.33000*NPRAD +  0.25000*MACO3 +  0.03400*SumRO2 +  0.25000*SumRCO3 + PAHRO2 ';
k(:,i) = (  1.5500E-11.*exp(  1.1700E+02./T) ); 
Gstr{i,   1}='NAPS';Gstr{i,   2}='OH';
fNAPS(i)=fNAPS(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.7410;fCATL(i)=fCATL(i)+  0.7070;fRO2C(i)=fRO2C(i)+  0.0340;fAFG2A(i)=fAFG2A(i)+  0.0170;fAFG2B(i)=fAFG2B(i)+  0.0170;fGLY(i)=fGLY(i)+  0.0340;fNPRAD(i)=fNPRAD(i)+  0.3300;fMACO3(i)=fMACO3(i)+  0.2500;fSumRO2(i)=fSumRO2(i)+  0.0340;fSumRCO3(i)=fSumRCO3(i)+  0.2500;fPAHRO2(i)=fPAHRO2(i)+  1.0000;

% 135, <CTOH>
i=i+1;
Rnames{ 135} = 'CATL3 + OH = HO2 + OTHN ';
k(:,i) = (  5.9700E-10 ); 
Gstr{i,   1}='CATL3';Gstr{i,   2}='OH';
fCATL3(i)=fCATL3(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fOTHN(i)=fOTHN(i)+  1.0000;

% 136, <CTN3>
i=i+1;
Rnames{ 136} = 'CATL3 + NO3 = HNO3 + OTHN ';
k(:,i) = (  4.8600E-10 ); 
Gstr{i,   1}='CATL3';Gstr{i,   2}='NO3';
fCATL3(i)=fCATL3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;fOTHN(i)=fOTHN(i)+  1.0000;

% 137, <PNOH>
i=i+1;
Rnames{ 137} = 'NAPPRD + OH = HO2 + OTHN ';
k(:,i) = (  2.0000E-10 ); 
Gstr{i,   1}='NAPPRD';Gstr{i,   2}='OH';
fNAPPRD(i)=fNAPPRD(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fOTHN(i)=fOTHN(i)+  1.0000;

% 138, <PNN3>
i=i+1;
Rnames{ 138} = 'NAPPRD + NO3 = HNO3 + OTHN ';
k(:,i) = (  1.7000E-10 ); 
Gstr{i,   1}='NAPPRD';Gstr{i,   2}='NO3';
fNAPPRD(i)=fNAPPRD(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;fOTHN(i)=fOTHN(i)+  1.0000;

% 139, <139>
i=i+1;
Rnames{ 139} = 'PHOT = 2.00000*HO2 +  2.00000*RO2C +  2.00000*SumRO2 + ALK3 ';
k(:,i) = (JBACL_11 ); 
Gstr{i,   1}='PHOT';
fPHOT(i)=fPHOT(i)-1.0;
fHO2(i)=fHO2(i)+  2.0000;fRO2C(i)=fRO2C(i)+  2.0000;fSumRO2(i)=fSumRO2(i)+  2.0000;fALK3(i)=fALK3(i)+  1.0000;

% 140, <140>
i=i+1;
Rnames{ 140} = 'IMINE = MECHO ';
k(:,i) = (  2.7800E-04 ); 
Gstr{i,   1}='IMINE';
fIMINE(i)=fIMINE(i)-1.0;
fMECHO(i)=fMECHO(i)+  1.0000;

% 141, <141>
i=i+1;
Rnames{ 141} = 'CLETHE + OH = xHO2 + RO2C + xHCHO + yROOH + SumRO2 ';
k(:,i) = (  2.5400E-12.*exp(  3.2500E+02./T) ); 
Gstr{i,   1}='CLETHE';Gstr{i,   2}='OH';
fCLETHE(i)=fCLETHE(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  1.0000;fRO2C(i)=fRO2C(i)+  1.0000;fxHCHO(i)=fxHCHO(i)+  1.0000;fyROOH(i)=fyROOH(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 142, <142>
i=i+1;
Rnames{ 142} = 'ACRLNT + OH = xHO2 + RO2C + xHCHO + yROOH + SumRO2 ';
k(:,i) = (  4.1300E-12 ); 
Gstr{i,   1}='ACRLNT';Gstr{i,   2}='OH';
fACRLNT(i)=fACRLNT(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  1.0000;fRO2C(i)=fRO2C(i)+  1.0000;fxHCHO(i)=fxHCHO(i)+  1.0000;fyROOH(i)=fyROOH(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 143, <143>
i=i+1;
Rnames{ 143} = 'PCE + OH = xHO2 + RO2C + yROOH + SumRO2 ';
k(:,i) = (  3.5000E-12.*exp( -9.2000E+02./T) ); 
Gstr{i,   1}='PCE';Gstr{i,   2}='OH';
fPCE(i)=fPCE(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  1.0000;fRO2C(i)=fRO2C(i)+  1.0000;fyROOH(i)=fyROOH(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 144, <144>
i=i+1;
Rnames{ 144} = 'PCLBEN + OH = xHO2 + RO2C + yROOH + SumRO2 ';
k(:,i) = (  4.0300E-13 ); 
Gstr{i,   1}='PCLBEN';Gstr{i,   2}='OH';
fPCLBEN(i)=fPCLBEN(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  1.0000;fRO2C(i)=fRO2C(i)+  1.0000;fyROOH(i)=fyROOH(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 145, <145>
i=i+1;
Rnames{ 145} = 'MECL2 + OH = xHO2 + RO2C + yROOH + SumRO2 ';
k(:,i) = (  1.8000E-12.*exp( -8.6000E+02./T) ); 
Gstr{i,   1}='MECL2';Gstr{i,   2}='OH';
fMECL2(i)=fMECL2(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  1.0000;fRO2C(i)=fRO2C(i)+  1.0000;fyROOH(i)=fyROOH(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 146, <146>
i=i+1;
Rnames{ 146} = 'ETBR2 + OH = xHO2 + RO2C + yROOH + SumRO2 ';
k(:,i) = (  7.6900E-12.*exp( -1.0560E+03./T) ); 
Gstr{i,   1}='ETBR2';Gstr{i,   2}='OH';
fETBR2(i)=fETBR2(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  1.0000;fRO2C(i)=fRO2C(i)+  1.0000;fyROOH(i)=fyROOH(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 147, <147>
i=i+1;
Rnames{ 147} = 'ETCL2 + OH = xHO2 + RO2C + yROOH + SumRO2 ';
k(:,i) = (  8.6900E-12.*exp( -1.0700E+03./T) ); 
Gstr{i,   1}='ETCL2';Gstr{i,   2}='OH';
fETCL2(i)=fETCL2(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  1.0000;fRO2C(i)=fRO2C(i)+  1.0000;fyROOH(i)=fyROOH(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 148, <148>
i=i+1;
Rnames{ 148} = 'ETOX + OH = xHO2 + RO2C + yROOH + SumRO2 ';
k(:,i) = (  1.6300E-12.*exp( -8.5600E+02./T) ); 
Gstr{i,   1}='ETOX';Gstr{i,   2}='OH';
fETOX(i)=fETOX(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  1.0000;fRO2C(i)=fRO2C(i)+  1.0000;fyROOH(i)=fyROOH(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 149, <149>
i=i+1;
Rnames{ 149} = 'CHCL3 + OH = xHO2 + RO2C + yROOH + SumRO2 ';
k(:,i) = (  1.8000E-12.*exp( -8.5000E+02./T) ); 
Gstr{i,   1}='CHCL3';Gstr{i,   2}='OH';
fCHCL3(i)=fCHCL3(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  1.0000;fRO2C(i)=fRO2C(i)+  1.0000;fyROOH(i)=fyROOH(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 150, <150>
i=i+1;
Rnames{ 150} = 'xHO2 + NO = NO + HO2 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xHO2';Gstr{i,   2}='NO';
fxHO2(i)=fxHO2(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 151, <151>
i=i+1;
Rnames{ 151} = 'xHO2 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xHO2';Gstr{i,   2}='HO2';
fxHO2(i)=fxHO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 152, <152>
i=i+1;
Rnames{ 152} = 'xHO2 + NO3 = NO3 + HO2 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xHO2';Gstr{i,   2}='NO3';
fxHO2(i)=fxHO2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 153, <153>
i=i+1;
Rnames{ 153} = 'xHO2 + SumRO2 = SumRO2 +  0.50000*HO2 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xHO2';Gstr{i,   2}='SumRO2';
fxHO2(i)=fxHO2(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fHO2(i)=fHO2(i)+  0.5000;

% 154, <154>
i=i+1;
Rnames{ 154} = 'xHO2 + SumRCO3 = SumRCO3 + HO2 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xHO2';Gstr{i,   2}='SumRCO3';
fxHO2(i)=fxHO2(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 155, <155>
i=i+1;
Rnames{ 155} = 'xOH + NO = NO + OH ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xOH';Gstr{i,   2}='NO';
fxOH(i)=fxOH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fOH(i)=fOH(i)+  1.0000;

% 156, <156>
i=i+1;
Rnames{ 156} = 'xOH + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xOH';Gstr{i,   2}='HO2';
fxOH(i)=fxOH(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 157, <157>
i=i+1;
Rnames{ 157} = 'xOH + NO3 = NO3 + OH ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xOH';Gstr{i,   2}='NO3';
fxOH(i)=fxOH(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fOH(i)=fOH(i)+  1.0000;

% 158, <158>
i=i+1;
Rnames{ 158} = 'xOH + SumRO2 = SumRO2 +  0.50000*OH ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xOH';Gstr{i,   2}='SumRO2';
fxOH(i)=fxOH(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fOH(i)=fOH(i)+  0.5000;

% 159, <159>
i=i+1;
Rnames{ 159} = 'xOH + SumRCO3 = SumRCO3 + OH ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xOH';Gstr{i,   2}='SumRCO3';
fxOH(i)=fxOH(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fOH(i)=fOH(i)+  1.0000;

% 160, <160>
i=i+1;
Rnames{ 160} = 'xNO2 + NO = NO + NO2 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xNO2';Gstr{i,   2}='NO';
fxNO2(i)=fxNO2(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fNO2(i)=fNO2(i)+  1.0000;

% 161, <161>
i=i+1;
Rnames{ 161} = 'xNO2 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xNO2';Gstr{i,   2}='HO2';
fxNO2(i)=fxNO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 162, <162>
i=i+1;
Rnames{ 162} = 'xNO2 + NO3 = NO3 + NO2 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xNO2';Gstr{i,   2}='NO3';
fxNO2(i)=fxNO2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fNO2(i)=fNO2(i)+  1.0000;

% 163, <163>
i=i+1;
Rnames{ 163} = 'xNO2 + SumRO2 = SumRO2 +  0.50000*NO2 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xNO2';Gstr{i,   2}='SumRO2';
fxNO2(i)=fxNO2(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fNO2(i)=fNO2(i)+  0.5000;

% 164, <164>
i=i+1;
Rnames{ 164} = 'xNO2 + SumRCO3 = SumRCO3 + NO2 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xNO2';Gstr{i,   2}='SumRCO3';
fxNO2(i)=fxNO2(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fNO2(i)=fNO2(i)+  1.0000;

% 165, <165>
i=i+1;
Rnames{ 165} = 'xNO3 + NO = NO + NO3 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xNO3';Gstr{i,   2}='NO';
fxNO3(i)=fxNO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fNO3(i)=fNO3(i)+  1.0000;

% 166, <166>
i=i+1;
Rnames{ 166} = 'xNO3 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xNO3';Gstr{i,   2}='HO2';
fxNO3(i)=fxNO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 167, <167>
i=i+1;
Rnames{ 167} = 'xNO3 + NO3 = 2.00000*NO3 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xNO3';Gstr{i,   2}='NO3';
fxNO3(i)=fxNO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  2.0000;

% 168, <168>
i=i+1;
Rnames{ 168} = 'xNO3 + SumRO2 = SumRO2 +  0.50000*NO3 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xNO3';Gstr{i,   2}='SumRO2';
fxNO3(i)=fxNO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fNO3(i)=fNO3(i)+  0.5000;

% 169, <169>
i=i+1;
Rnames{ 169} = 'xNO3 + SumRCO3 = SumRCO3 + NO3 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xNO3';Gstr{i,   2}='SumRCO3';
fxNO3(i)=fxNO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fNO3(i)=fNO3(i)+  1.0000;

% 170, <170>
i=i+1;
Rnames{ 170} = 'xHCHO + NO = NO + HCHO ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xHCHO';Gstr{i,   2}='NO';
fxHCHO(i)=fxHCHO(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fHCHO(i)=fHCHO(i)+  1.0000;

% 171, <171>
i=i+1;
Rnames{ 171} = 'xHCHO + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xHCHO';Gstr{i,   2}='HO2';
fxHCHO(i)=fxHCHO(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 172, <172>
i=i+1;
Rnames{ 172} = 'xHCHO + NO3 = NO3 + HCHO ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xHCHO';Gstr{i,   2}='NO3';
fxHCHO(i)=fxHCHO(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fHCHO(i)=fHCHO(i)+  1.0000;

% 173, <173>
i=i+1;
Rnames{ 173} = 'xHCHO + SumRO2 = SumRO2 +  0.50000*HCHO ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xHCHO';Gstr{i,   2}='SumRO2';
fxHCHO(i)=fxHCHO(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fHCHO(i)=fHCHO(i)+  0.5000;

% 174, <174>
i=i+1;
Rnames{ 174} = 'xHCHO + SumRCO3 = SumRCO3 + HCHO ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xHCHO';Gstr{i,   2}='SumRCO3';
fxHCHO(i)=fxHCHO(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fHCHO(i)=fHCHO(i)+  1.0000;

% 175, <175>
i=i+1;
Rnames{ 175} = 'xGLY + NO = NO + GLY ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xGLY';Gstr{i,   2}='NO';
fxGLY(i)=fxGLY(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fGLY(i)=fGLY(i)+  1.0000;

% 176, <176>
i=i+1;
Rnames{ 176} = 'xGLY + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xGLY';Gstr{i,   2}='HO2';
fxGLY(i)=fxGLY(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 177, <177>
i=i+1;
Rnames{ 177} = 'xGLY + NO3 = NO3 + GLY ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xGLY';Gstr{i,   2}='NO3';
fxGLY(i)=fxGLY(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fGLY(i)=fGLY(i)+  1.0000;

% 178, <178>
i=i+1;
Rnames{ 178} = 'xGLY + SumRO2 = SumRO2 +  0.50000*GLY ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xGLY';Gstr{i,   2}='SumRO2';
fxGLY(i)=fxGLY(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fGLY(i)=fGLY(i)+  0.5000;

% 179, <179>
i=i+1;
Rnames{ 179} = 'xGLY + SumRCO3 = SumRCO3 + GLY ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xGLY';Gstr{i,   2}='SumRCO3';
fxGLY(i)=fxGLY(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fGLY(i)=fGLY(i)+  1.0000;

% 180, <180>
i=i+1;
Rnames{ 180} = 'xHCOOH + NO = NO + HCOOH ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xHCOOH';Gstr{i,   2}='NO';
fxHCOOH(i)=fxHCOOH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fHCOOH(i)=fHCOOH(i)+  1.0000;

% 181, <181>
i=i+1;
Rnames{ 181} = 'xHCOOH + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xHCOOH';Gstr{i,   2}='HO2';
fxHCOOH(i)=fxHCOOH(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 182, <182>
i=i+1;
Rnames{ 182} = 'xHCOOH + NO3 = NO3 + HCOOH ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xHCOOH';Gstr{i,   2}='NO3';
fxHCOOH(i)=fxHCOOH(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fHCOOH(i)=fHCOOH(i)+  1.0000;

% 183, <183>
i=i+1;
Rnames{ 183} = 'xHCOOH + SumRO2 = SumRO2 +  0.50000*HCOOH ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xHCOOH';Gstr{i,   2}='SumRO2';
fxHCOOH(i)=fxHCOOH(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fHCOOH(i)=fHCOOH(i)+  0.5000;

% 184, <184>
i=i+1;
Rnames{ 184} = 'xHCOOH + SumRCO3 = SumRCO3 + HCOOH ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xHCOOH';Gstr{i,   2}='SumRCO3';
fxHCOOH(i)=fxHCOOH(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fHCOOH(i)=fHCOOH(i)+  1.0000;

% 185, <185>
i=i+1;
Rnames{ 185} = 'xMECHO + NO = NO + MECHO ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xMECHO';Gstr{i,   2}='NO';
fxMECHO(i)=fxMECHO(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fMECHO(i)=fMECHO(i)+  1.0000;

% 186, <186>
i=i+1;
Rnames{ 186} = 'xMECHO + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xMECHO';Gstr{i,   2}='HO2';
fxMECHO(i)=fxMECHO(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 187, <187>
i=i+1;
Rnames{ 187} = 'xMECHO + NO3 = NO3 + MECHO ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xMECHO';Gstr{i,   2}='NO3';
fxMECHO(i)=fxMECHO(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fMECHO(i)=fMECHO(i)+  1.0000;

% 188, <188>
i=i+1;
Rnames{ 188} = 'xMECHO + SumRO2 = SumRO2 +  0.50000*MECHO ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xMECHO';Gstr{i,   2}='SumRO2';
fxMECHO(i)=fxMECHO(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fMECHO(i)=fMECHO(i)+  0.5000;

% 189, <189>
i=i+1;
Rnames{ 189} = 'xMECHO + SumRCO3 = SumRCO3 + MECHO ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xMECHO';Gstr{i,   2}='SumRCO3';
fxMECHO(i)=fxMECHO(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fMECHO(i)=fMECHO(i)+  1.0000;

% 190, <190>
i=i+1;
Rnames{ 190} = 'xETCHO + NO = NO + ETCHO ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xETCHO';Gstr{i,   2}='NO';
fxETCHO(i)=fxETCHO(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fETCHO(i)=fETCHO(i)+  1.0000;

% 191, <191>
i=i+1;
Rnames{ 191} = 'xETCHO + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xETCHO';Gstr{i,   2}='HO2';
fxETCHO(i)=fxETCHO(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 192, <192>
i=i+1;
Rnames{ 192} = 'xETCHO + NO3 = NO3 + ETCHO ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xETCHO';Gstr{i,   2}='NO3';
fxETCHO(i)=fxETCHO(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fETCHO(i)=fETCHO(i)+  1.0000;

% 193, <193>
i=i+1;
Rnames{ 193} = 'xETCHO + SumRO2 = SumRO2 +  0.50000*ETCHO ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xETCHO';Gstr{i,   2}='SumRO2';
fxETCHO(i)=fxETCHO(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fETCHO(i)=fETCHO(i)+  0.5000;

% 194, <194>
i=i+1;
Rnames{ 194} = 'xETCHO + SumRCO3 = SumRCO3 + ETCHO ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xETCHO';Gstr{i,   2}='SumRCO3';
fxETCHO(i)=fxETCHO(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fETCHO(i)=fETCHO(i)+  1.0000;

% 195, <195>
i=i+1;
Rnames{ 195} = 'xGLCHO + NO = NO + GLCHO ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xGLCHO';Gstr{i,   2}='NO';
fxGLCHO(i)=fxGLCHO(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fGLCHO(i)=fGLCHO(i)+  1.0000;

% 196, <196>
i=i+1;
Rnames{ 196} = 'xGLCHO + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xGLCHO';Gstr{i,   2}='HO2';
fxGLCHO(i)=fxGLCHO(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 197, <197>
i=i+1;
Rnames{ 197} = 'xGLCHO + NO3 = NO3 + GLCHO ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xGLCHO';Gstr{i,   2}='NO3';
fxGLCHO(i)=fxGLCHO(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fGLCHO(i)=fGLCHO(i)+  1.0000;

% 198, <198>
i=i+1;
Rnames{ 198} = 'xGLCHO + SumRO2 = SumRO2 +  0.50000*GLCHO ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xGLCHO';Gstr{i,   2}='SumRO2';
fxGLCHO(i)=fxGLCHO(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fGLCHO(i)=fGLCHO(i)+  0.5000;

% 199, <199>
i=i+1;
Rnames{ 199} = 'xGLCHO + SumRCO3 = SumRCO3 + GLCHO ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xGLCHO';Gstr{i,   2}='SumRCO3';
fxGLCHO(i)=fxGLCHO(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fGLCHO(i)=fGLCHO(i)+  1.0000;

% 200, <200>
i=i+1;
Rnames{ 200} = 'xMEK + NO = NO + MEK ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xMEK';Gstr{i,   2}='NO';
fxMEK(i)=fxMEK(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fMEK(i)=fMEK(i)+  1.0000;

% 201, <201>
i=i+1;
Rnames{ 201} = 'xMEK + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xMEK';Gstr{i,   2}='HO2';
fxMEK(i)=fxMEK(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 202, <202>
i=i+1;
Rnames{ 202} = 'xMEK + NO3 = NO3 + MEK ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xMEK';Gstr{i,   2}='NO3';
fxMEK(i)=fxMEK(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fMEK(i)=fMEK(i)+  1.0000;

% 203, <203>
i=i+1;
Rnames{ 203} = 'xMEK + SumRO2 = SumRO2 +  0.50000*MEK ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xMEK';Gstr{i,   2}='SumRO2';
fxMEK(i)=fxMEK(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fMEK(i)=fMEK(i)+  0.5000;

% 204, <204>
i=i+1;
Rnames{ 204} = 'xMEK + SumRCO3 = SumRCO3 + MEK ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xMEK';Gstr{i,   2}='SumRCO3';
fxMEK(i)=fxMEK(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fMEK(i)=fMEK(i)+  1.0000;

% 205, <205>
i=i+1;
Rnames{ 205} = 'xACRO + NO = NO + ACRO ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xACRO';Gstr{i,   2}='NO';
fxACRO(i)=fxACRO(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fACRO(i)=fACRO(i)+  1.0000;

% 206, <206>
i=i+1;
Rnames{ 206} = 'xACRO + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xACRO';Gstr{i,   2}='HO2';
fxACRO(i)=fxACRO(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 207, <207>
i=i+1;
Rnames{ 207} = 'xACRO + NO3 = NO3 + ACRO ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xACRO';Gstr{i,   2}='NO3';
fxACRO(i)=fxACRO(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fACRO(i)=fACRO(i)+  1.0000;

% 208, <208>
i=i+1;
Rnames{ 208} = 'xACRO + SumRO2 = SumRO2 +  0.50000*ACRO ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xACRO';Gstr{i,   2}='SumRO2';
fxACRO(i)=fxACRO(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fACRO(i)=fACRO(i)+  0.5000;

% 209, <209>
i=i+1;
Rnames{ 209} = 'xACRO + SumRCO3 = SumRCO3 + ACRO ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xACRO';Gstr{i,   2}='SumRCO3';
fxACRO(i)=fxACRO(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fACRO(i)=fACRO(i)+  1.0000;

% 210, <210>
i=i+1;
Rnames{ 210} = 'xACET + NO = NO + ACET ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xACET';Gstr{i,   2}='NO';
fxACET(i)=fxACET(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fACET(i)=fACET(i)+  1.0000;

% 211, <211>
i=i+1;
Rnames{ 211} = 'xACET + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xACET';Gstr{i,   2}='HO2';
fxACET(i)=fxACET(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 212, <212>
i=i+1;
Rnames{ 212} = 'xACET + NO3 = NO3 + ACET ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xACET';Gstr{i,   2}='NO3';
fxACET(i)=fxACET(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fACET(i)=fACET(i)+  1.0000;

% 213, <213>
i=i+1;
Rnames{ 213} = 'xACET + SumRO2 = SumRO2 +  0.50000*ACET ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xACET';Gstr{i,   2}='SumRO2';
fxACET(i)=fxACET(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fACET(i)=fACET(i)+  0.5000;

% 214, <214>
i=i+1;
Rnames{ 214} = 'xACET + SumRCO3 = SumRCO3 + ACET ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xACET';Gstr{i,   2}='SumRCO3';
fxACET(i)=fxACET(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fACET(i)=fACET(i)+  1.0000;

% 215, <215>
i=i+1;
Rnames{ 215} = 'xMACR + NO = NO + MACR ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xMACR';Gstr{i,   2}='NO';
fxMACR(i)=fxMACR(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fMACR(i)=fMACR(i)+  1.0000;

% 216, <216>
i=i+1;
Rnames{ 216} = 'xMACR + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xMACR';Gstr{i,   2}='HO2';
fxMACR(i)=fxMACR(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 217, <217>
i=i+1;
Rnames{ 217} = 'xMACR + NO3 = NO3 + MACR ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xMACR';Gstr{i,   2}='NO3';
fxMACR(i)=fxMACR(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fMACR(i)=fMACR(i)+  1.0000;

% 218, <218>
i=i+1;
Rnames{ 218} = 'xMACR + SumRO2 = SumRO2 +  0.50000*MACR ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xMACR';Gstr{i,   2}='SumRO2';
fxMACR(i)=fxMACR(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fMACR(i)=fMACR(i)+  0.5000;

% 219, <219>
i=i+1;
Rnames{ 219} = 'xMACR + SumRCO3 = SumRCO3 + MACR ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xMACR';Gstr{i,   2}='SumRCO3';
fxMACR(i)=fxMACR(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fMACR(i)=fMACR(i)+  1.0000;

% 220, <220>
i=i+1;
Rnames{ 220} = 'xMVK + NO = NO + MVK ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xMVK';Gstr{i,   2}='NO';
fxMVK(i)=fxMVK(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fMVK(i)=fMVK(i)+  1.0000;

% 221, <221>
i=i+1;
Rnames{ 221} = 'xMVK + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xMVK';Gstr{i,   2}='HO2';
fxMVK(i)=fxMVK(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 222, <222>
i=i+1;
Rnames{ 222} = 'xMVK + NO3 = NO3 + MVK ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xMVK';Gstr{i,   2}='NO3';
fxMVK(i)=fxMVK(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fMVK(i)=fMVK(i)+  1.0000;

% 223, <223>
i=i+1;
Rnames{ 223} = 'xMVK + SumRO2 = SumRO2 +  0.50000*MVK ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xMVK';Gstr{i,   2}='SumRO2';
fxMVK(i)=fxMVK(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fMVK(i)=fMVK(i)+  0.5000;

% 224, <224>
i=i+1;
Rnames{ 224} = 'xMVK + SumRCO3 = SumRCO3 + MVK ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xMVK';Gstr{i,   2}='SumRCO3';
fxMVK(i)=fxMVK(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fMVK(i)=fMVK(i)+  1.0000;

% 225, <225>
i=i+1;
Rnames{ 225} = 'xBACL + NO = NO + BACL ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xBACL';Gstr{i,   2}='NO';
fxBACL(i)=fxBACL(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fBACL(i)=fBACL(i)+  1.0000;

% 226, <226>
i=i+1;
Rnames{ 226} = 'xBACL + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xBACL';Gstr{i,   2}='HO2';
fxBACL(i)=fxBACL(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 227, <227>
i=i+1;
Rnames{ 227} = 'xBACL + NO3 = NO3 + BACL ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xBACL';Gstr{i,   2}='NO3';
fxBACL(i)=fxBACL(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fBACL(i)=fBACL(i)+  1.0000;

% 228, <228>
i=i+1;
Rnames{ 228} = 'xBACL + SumRO2 = SumRO2 +  0.50000*BACL ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xBACL';Gstr{i,   2}='SumRO2';
fxBACL(i)=fxBACL(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fBACL(i)=fBACL(i)+  0.5000;

% 229, <229>
i=i+1;
Rnames{ 229} = 'xBACL + SumRCO3 = SumRCO3 + BACL ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xBACL';Gstr{i,   2}='SumRCO3';
fxBACL(i)=fxBACL(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fBACL(i)=fBACL(i)+  1.0000;

% 230, <230>
i=i+1;
Rnames{ 230} = 'xMGLY + NO = NO + MGLY ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xMGLY';Gstr{i,   2}='NO';
fxMGLY(i)=fxMGLY(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fMGLY(i)=fMGLY(i)+  1.0000;

% 231, <231>
i=i+1;
Rnames{ 231} = 'xMGLY + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xMGLY';Gstr{i,   2}='HO2';
fxMGLY(i)=fxMGLY(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 232, <232>
i=i+1;
Rnames{ 232} = 'xMGLY + NO3 = NO3 + MGLY ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xMGLY';Gstr{i,   2}='NO3';
fxMGLY(i)=fxMGLY(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fMGLY(i)=fMGLY(i)+  1.0000;

% 233, <233>
i=i+1;
Rnames{ 233} = 'xMGLY + SumRO2 = SumRO2 +  0.50000*MGLY ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xMGLY';Gstr{i,   2}='SumRO2';
fxMGLY(i)=fxMGLY(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fMGLY(i)=fMGLY(i)+  0.5000;

% 234, <234>
i=i+1;
Rnames{ 234} = 'xMGLY + SumRCO3 = SumRCO3 + MGLY ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xMGLY';Gstr{i,   2}='SumRCO3';
fxMGLY(i)=fxMGLY(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fMGLY(i)=fMGLY(i)+  1.0000;

% 235, <235>
i=i+1;
Rnames{ 235} = 'xBUDAL + NO = NO + BUDAL ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xBUDAL';Gstr{i,   2}='NO';
fxBUDAL(i)=fxBUDAL(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fBUDAL(i)=fBUDAL(i)+  1.0000;

% 236, <236>
i=i+1;
Rnames{ 236} = 'xBUDAL + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xBUDAL';Gstr{i,   2}='HO2';
fxBUDAL(i)=fxBUDAL(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 237, <237>
i=i+1;
Rnames{ 237} = 'xBUDAL + NO3 = NO3 + BUDAL ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xBUDAL';Gstr{i,   2}='NO3';
fxBUDAL(i)=fxBUDAL(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fBUDAL(i)=fBUDAL(i)+  1.0000;

% 238, <238>
i=i+1;
Rnames{ 238} = 'xBUDAL + SumRO2 = SumRO2 +  0.50000*BUDAL ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xBUDAL';Gstr{i,   2}='SumRO2';
fxBUDAL(i)=fxBUDAL(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fBUDAL(i)=fBUDAL(i)+  0.5000;

% 239, <239>
i=i+1;
Rnames{ 239} = 'xBUDAL + SumRCO3 = SumRCO3 + BUDAL ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xBUDAL';Gstr{i,   2}='SumRCO3';
fxBUDAL(i)=fxBUDAL(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fBUDAL(i)=fBUDAL(i)+  1.0000;

% 240, <240>
i=i+1;
Rnames{ 240} = 'xFURNS + NO = NO + FURNS ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xFURNS';Gstr{i,   2}='NO';
fxFURNS(i)=fxFURNS(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fFURNS(i)=fFURNS(i)+  1.0000;

% 241, <241>
i=i+1;
Rnames{ 241} = 'xFURNS + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xFURNS';Gstr{i,   2}='HO2';
fxFURNS(i)=fxFURNS(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 242, <242>
i=i+1;
Rnames{ 242} = 'xFURNS + NO3 = NO3 + FURNS ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xFURNS';Gstr{i,   2}='NO3';
fxFURNS(i)=fxFURNS(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fFURNS(i)=fFURNS(i)+  1.0000;

% 243, <243>
i=i+1;
Rnames{ 243} = 'xFURNS + SumRO2 = SumRO2 +  0.50000*FURNS ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xFURNS';Gstr{i,   2}='SumRO2';
fxFURNS(i)=fxFURNS(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fFURNS(i)=fFURNS(i)+  0.5000;

% 244, <244>
i=i+1;
Rnames{ 244} = 'xFURNS + SumRCO3 = SumRCO3 + FURNS ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xFURNS';Gstr{i,   2}='SumRCO3';
fxFURNS(i)=fxFURNS(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fFURNS(i)=fFURNS(i)+  1.0000;

% 245, <245>
i=i+1;
Rnames{ 245} = 'xBALD + NO = NO + BALD ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xBALD';Gstr{i,   2}='NO';
fxBALD(i)=fxBALD(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fBALD(i)=fBALD(i)+  1.0000;

% 246, <246>
i=i+1;
Rnames{ 246} = 'xBALD + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xBALD';Gstr{i,   2}='HO2';
fxBALD(i)=fxBALD(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 247, <247>
i=i+1;
Rnames{ 247} = 'xBALD + NO3 = NO3 + BALD ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xBALD';Gstr{i,   2}='NO3';
fxBALD(i)=fxBALD(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fBALD(i)=fBALD(i)+  1.0000;

% 248, <248>
i=i+1;
Rnames{ 248} = 'xBALD + SumRO2 = SumRO2 +  0.50000*BALD ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xBALD';Gstr{i,   2}='SumRO2';
fxBALD(i)=fxBALD(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fBALD(i)=fBALD(i)+  0.5000;

% 249, <249>
i=i+1;
Rnames{ 249} = 'xBALD + SumRCO3 = SumRCO3 + BALD ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xBALD';Gstr{i,   2}='SumRCO3';
fxBALD(i)=fxBALD(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fBALD(i)=fBALD(i)+  1.0000;

% 250, <250>
i=i+1;
Rnames{ 250} = 'xBENX + NO = NO + BENX ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xBENX';Gstr{i,   2}='NO';
fxBENX(i)=fxBENX(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fBENX(i)=fBENX(i)+  1.0000;

% 251, <251>
i=i+1;
Rnames{ 251} = 'xBENX + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xBENX';Gstr{i,   2}='HO2';
fxBENX(i)=fxBENX(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 252, <252>
i=i+1;
Rnames{ 252} = 'xBENX + NO3 = NO3 + BENX ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xBENX';Gstr{i,   2}='NO3';
fxBENX(i)=fxBENX(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fBENX(i)=fBENX(i)+  1.0000;

% 253, <253>
i=i+1;
Rnames{ 253} = 'xBENX + SumRO2 = SumRO2 +  0.50000*BENX ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xBENX';Gstr{i,   2}='SumRO2';
fxBENX(i)=fxBENX(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fBENX(i)=fBENX(i)+  0.5000;

% 254, <254>
i=i+1;
Rnames{ 254} = 'xBENX + SumRCO3 = SumRCO3 + BENX ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xBENX';Gstr{i,   2}='SumRCO3';
fxBENX(i)=fxBENX(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fBENX(i)=fBENX(i)+  1.0000;

% 255, <255>
i=i+1;
Rnames{ 255} = 'xRCHO + NO = NO + RCHO ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xRCHO';Gstr{i,   2}='NO';
fxRCHO(i)=fxRCHO(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fRCHO(i)=fRCHO(i)+  1.0000;

% 256, <256>
i=i+1;
Rnames{ 256} = 'xRCHO + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xRCHO';Gstr{i,   2}='HO2';
fxRCHO(i)=fxRCHO(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 257, <257>
i=i+1;
Rnames{ 257} = 'xRCHO + NO3 = NO3 + RCHO ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xRCHO';Gstr{i,   2}='NO3';
fxRCHO(i)=fxRCHO(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fRCHO(i)=fRCHO(i)+  1.0000;

% 258, <258>
i=i+1;
Rnames{ 258} = 'xRCHO + SumRO2 = SumRO2 +  0.50000*RCHO ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xRCHO';Gstr{i,   2}='SumRO2';
fxRCHO(i)=fxRCHO(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fRCHO(i)=fRCHO(i)+  0.5000;

% 259, <259>
i=i+1;
Rnames{ 259} = 'xRCHO + SumRCO3 = SumRCO3 + RCHO ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xRCHO';Gstr{i,   2}='SumRCO3';
fxRCHO(i)=fxRCHO(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fRCHO(i)=fRCHO(i)+  1.0000;

% 260, <260>
i=i+1;
Rnames{ 260} = 'xKET2 + NO = NO + KET2 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xKET2';Gstr{i,   2}='NO';
fxKET2(i)=fxKET2(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fKET2(i)=fKET2(i)+  1.0000;

% 261, <261>
i=i+1;
Rnames{ 261} = 'xKET2 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xKET2';Gstr{i,   2}='HO2';
fxKET2(i)=fxKET2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 262, <262>
i=i+1;
Rnames{ 262} = 'xKET2 + NO3 = NO3 + KET2 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xKET2';Gstr{i,   2}='NO3';
fxKET2(i)=fxKET2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fKET2(i)=fKET2(i)+  1.0000;

% 263, <263>
i=i+1;
Rnames{ 263} = 'xKET2 + SumRO2 = SumRO2 +  0.50000*KET2 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xKET2';Gstr{i,   2}='SumRO2';
fxKET2(i)=fxKET2(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fKET2(i)=fKET2(i)+  0.5000;

% 264, <264>
i=i+1;
Rnames{ 264} = 'xKET2 + SumRCO3 = SumRCO3 + KET2 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xKET2';Gstr{i,   2}='SumRCO3';
fxKET2(i)=fxKET2(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fKET2(i)=fKET2(i)+  1.0000;

% 265, <265>
i=i+1;
Rnames{ 265} = 'xLVKS + NO = NO + LVKS ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xLVKS';Gstr{i,   2}='NO';
fxLVKS(i)=fxLVKS(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fLVKS(i)=fLVKS(i)+  1.0000;

% 266, <266>
i=i+1;
Rnames{ 266} = 'xLVKS + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xLVKS';Gstr{i,   2}='HO2';
fxLVKS(i)=fxLVKS(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 267, <267>
i=i+1;
Rnames{ 267} = 'xLVKS + NO3 = NO3 + LVKS ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xLVKS';Gstr{i,   2}='NO3';
fxLVKS(i)=fxLVKS(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fLVKS(i)=fLVKS(i)+  1.0000;

% 268, <268>
i=i+1;
Rnames{ 268} = 'xLVKS + SumRO2 = SumRO2 +  0.50000*LVKS ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xLVKS';Gstr{i,   2}='SumRO2';
fxLVKS(i)=fxLVKS(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fLVKS(i)=fLVKS(i)+  0.5000;

% 269, <269>
i=i+1;
Rnames{ 269} = 'xLVKS + SumRCO3 = SumRCO3 + LVKS ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xLVKS';Gstr{i,   2}='SumRCO3';
fxLVKS(i)=fxLVKS(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fLVKS(i)=fLVKS(i)+  1.0000;

% 270, <270>
i=i+1;
Rnames{ 270} = 'xOLEA1 + NO = NO + OLEA1 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xOLEA1';Gstr{i,   2}='NO';
fxOLEA1(i)=fxOLEA1(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fOLEA1(i)=fOLEA1(i)+  1.0000;

% 271, <271>
i=i+1;
Rnames{ 271} = 'xOLEA1 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xOLEA1';Gstr{i,   2}='HO2';
fxOLEA1(i)=fxOLEA1(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 272, <272>
i=i+1;
Rnames{ 272} = 'xOLEA1 + NO3 = NO3 + OLEA1 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xOLEA1';Gstr{i,   2}='NO3';
fxOLEA1(i)=fxOLEA1(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fOLEA1(i)=fOLEA1(i)+  1.0000;

% 273, <273>
i=i+1;
Rnames{ 273} = 'xOLEA1 + SumRO2 = SumRO2 +  0.50000*OLEA1 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xOLEA1';Gstr{i,   2}='SumRO2';
fxOLEA1(i)=fxOLEA1(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fOLEA1(i)=fOLEA1(i)+  0.5000;

% 274, <274>
i=i+1;
Rnames{ 274} = 'xOLEA1 + SumRCO3 = SumRCO3 + OLEA1 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xOLEA1';Gstr{i,   2}='SumRCO3';
fxOLEA1(i)=fxOLEA1(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fOLEA1(i)=fOLEA1(i)+  1.0000;

% 275, <275>
i=i+1;
Rnames{ 275} = 'xOLEA2 + NO = NO + OLEA2 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xOLEA2';Gstr{i,   2}='NO';
fxOLEA2(i)=fxOLEA2(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fOLEA2(i)=fOLEA2(i)+  1.0000;

% 276, <276>
i=i+1;
Rnames{ 276} = 'xOLEA2 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xOLEA2';Gstr{i,   2}='HO2';
fxOLEA2(i)=fxOLEA2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 277, <277>
i=i+1;
Rnames{ 277} = 'xOLEA2 + NO3 = NO3 + OLEA2 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xOLEA2';Gstr{i,   2}='NO3';
fxOLEA2(i)=fxOLEA2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fOLEA2(i)=fOLEA2(i)+  1.0000;

% 278, <278>
i=i+1;
Rnames{ 278} = 'xOLEA2 + SumRO2 = SumRO2 +  0.50000*OLEA2 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xOLEA2';Gstr{i,   2}='SumRO2';
fxOLEA2(i)=fxOLEA2(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fOLEA2(i)=fOLEA2(i)+  0.5000;

% 279, <279>
i=i+1;
Rnames{ 279} = 'xOLEA2 + SumRCO3 = SumRCO3 + OLEA2 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xOLEA2';Gstr{i,   2}='SumRCO3';
fxOLEA2(i)=fxOLEA2(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fOLEA2(i)=fOLEA2(i)+  1.0000;

% 280, <280>
i=i+1;
Rnames{ 280} = 'xOLEP + NO = NO + OLEP ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xOLEP';Gstr{i,   2}='NO';
fxOLEP(i)=fxOLEP(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fOLEP(i)=fOLEP(i)+  1.0000;

% 281, <281>
i=i+1;
Rnames{ 281} = 'xOLEP + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xOLEP';Gstr{i,   2}='HO2';
fxOLEP(i)=fxOLEP(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 282, <282>
i=i+1;
Rnames{ 282} = 'xOLEP + NO3 = NO3 + OLEP ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xOLEP';Gstr{i,   2}='NO3';
fxOLEP(i)=fxOLEP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fOLEP(i)=fOLEP(i)+  1.0000;

% 283, <283>
i=i+1;
Rnames{ 283} = 'xOLEP + SumRO2 = SumRO2 +  0.50000*OLEP ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xOLEP';Gstr{i,   2}='SumRO2';
fxOLEP(i)=fxOLEP(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fOLEP(i)=fOLEP(i)+  0.5000;

% 284, <284>
i=i+1;
Rnames{ 284} = 'xOLEP + SumRCO3 = SumRCO3 + OLEP ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xOLEP';Gstr{i,   2}='SumRCO3';
fxOLEP(i)=fxOLEP(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fOLEP(i)=fOLEP(i)+  1.0000;

% 285, <285>
i=i+1;
Rnames{ 285} = 'xOACID + NO = NO + OACID ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xOACID';Gstr{i,   2}='NO';
fxOACID(i)=fxOACID(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fOACID(i)=fOACID(i)+  1.0000;

% 286, <286>
i=i+1;
Rnames{ 286} = 'xOACID + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xOACID';Gstr{i,   2}='HO2';
fxOACID(i)=fxOACID(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 287, <287>
i=i+1;
Rnames{ 287} = 'xOACID + NO3 = NO3 + OACID ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xOACID';Gstr{i,   2}='NO3';
fxOACID(i)=fxOACID(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fOACID(i)=fOACID(i)+  1.0000;

% 288, <288>
i=i+1;
Rnames{ 288} = 'xOACID + SumRO2 = SumRO2 +  0.50000*OACID ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xOACID';Gstr{i,   2}='SumRO2';
fxOACID(i)=fxOACID(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fOACID(i)=fOACID(i)+  0.5000;

% 289, <289>
i=i+1;
Rnames{ 289} = 'xOACID + SumRCO3 = SumRCO3 + OACID ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xOACID';Gstr{i,   2}='SumRCO3';
fxOACID(i)=fxOACID(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fOACID(i)=fOACID(i)+  1.0000;

% 290, <290>
i=i+1;
Rnames{ 290} = 'xPACID + NO = NO + PACID ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xPACID';Gstr{i,   2}='NO';
fxPACID(i)=fxPACID(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fPACID(i)=fPACID(i)+  1.0000;

% 291, <291>
i=i+1;
Rnames{ 291} = 'xPACID + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xPACID';Gstr{i,   2}='HO2';
fxPACID(i)=fxPACID(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 292, <292>
i=i+1;
Rnames{ 292} = 'xPACID + NO3 = NO3 + PACID ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xPACID';Gstr{i,   2}='NO3';
fxPACID(i)=fxPACID(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fPACID(i)=fPACID(i)+  1.0000;

% 293, <293>
i=i+1;
Rnames{ 293} = 'xPACID + SumRO2 = SumRO2 +  0.50000*PACID ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xPACID';Gstr{i,   2}='SumRO2';
fxPACID(i)=fxPACID(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fPACID(i)=fPACID(i)+  0.5000;

% 294, <294>
i=i+1;
Rnames{ 294} = 'xPACID + SumRCO3 = SumRCO3 + PACID ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xPACID';Gstr{i,   2}='SumRCO3';
fxPACID(i)=fxPACID(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fPACID(i)=fPACID(i)+  1.0000;

% 295, <295>
i=i+1;
Rnames{ 295} = 'xAMINS + NO = NO + AMINS ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xAMINS';Gstr{i,   2}='NO';
fxAMINS(i)=fxAMINS(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fAMINS(i)=fAMINS(i)+  1.0000;

% 296, <296>
i=i+1;
Rnames{ 296} = 'xAMINS + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xAMINS';Gstr{i,   2}='HO2';
fxAMINS(i)=fxAMINS(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 297, <297>
i=i+1;
Rnames{ 297} = 'xAMINS + NO3 = NO3 + AMINS ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xAMINS';Gstr{i,   2}='NO3';
fxAMINS(i)=fxAMINS(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fAMINS(i)=fAMINS(i)+  1.0000;

% 298, <298>
i=i+1;
Rnames{ 298} = 'xAMINS + SumRO2 = SumRO2 +  0.50000*AMINS ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xAMINS';Gstr{i,   2}='SumRO2';
fxAMINS(i)=fxAMINS(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fAMINS(i)=fAMINS(i)+  0.5000;

% 299, <299>
i=i+1;
Rnames{ 299} = 'xAMINS + SumRCO3 = SumRCO3 + AMINS ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xAMINS';Gstr{i,   2}='SumRCO3';
fxAMINS(i)=fxAMINS(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fAMINS(i)=fAMINS(i)+  1.0000;

% 300, <305>
i=i+1;
Rnames{ 300} = 'xRPNO3 + NO = NO + RPNO3 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xRPNO3';Gstr{i,   2}='NO';
fxRPNO3(i)=fxRPNO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fRPNO3(i)=fRPNO3(i)+  1.0000;

% 301, <306>
i=i+1;
Rnames{ 301} = 'xRPNO3 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xRPNO3';Gstr{i,   2}='HO2';
fxRPNO3(i)=fxRPNO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 302, <307>
i=i+1;
Rnames{ 302} = 'xRPNO3 + NO3 = NO3 + RPNO3 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xRPNO3';Gstr{i,   2}='NO3';
fxRPNO3(i)=fxRPNO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fRPNO3(i)=fRPNO3(i)+  1.0000;

% 303, <308>
i=i+1;
Rnames{ 303} = 'xRPNO3 + SumRO2 = SumRO2 +  0.50000*RPNO3 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xRPNO3';Gstr{i,   2}='SumRO2';
fxRPNO3(i)=fxRPNO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fRPNO3(i)=fRPNO3(i)+  0.5000;

% 304, <309>
i=i+1;
Rnames{ 304} = 'xRPNO3 + SumRCO3 = SumRCO3 + RPNO3 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xRPNO3';Gstr{i,   2}='SumRCO3';
fxRPNO3(i)=fxRPNO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fRPNO3(i)=fRPNO3(i)+  1.0000;

% 305, <310>
i=i+1;
Rnames{ 305} = 'xRCNO3 + NO = NO + RCNO3 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xRCNO3';Gstr{i,   2}='NO';
fxRCNO3(i)=fxRCNO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fRCNO3(i)=fRCNO3(i)+  1.0000;

% 306, <311>
i=i+1;
Rnames{ 306} = 'xRCNO3 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xRCNO3';Gstr{i,   2}='HO2';
fxRCNO3(i)=fxRCNO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 307, <312>
i=i+1;
Rnames{ 307} = 'xRCNO3 + NO3 = NO3 + RCNO3 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xRCNO3';Gstr{i,   2}='NO3';
fxRCNO3(i)=fxRCNO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fRCNO3(i)=fRCNO3(i)+  1.0000;

% 308, <313>
i=i+1;
Rnames{ 308} = 'xRCNO3 + SumRO2 = SumRO2 +  0.50000*RCNO3 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xRCNO3';Gstr{i,   2}='SumRO2';
fxRCNO3(i)=fxRCNO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fRCNO3(i)=fRCNO3(i)+  0.5000;

% 309, <314>
i=i+1;
Rnames{ 309} = 'xRCNO3 + SumRCO3 = SumRCO3 + RCNO3 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xRCNO3';Gstr{i,   2}='SumRCO3';
fxRCNO3(i)=fxRCNO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fRCNO3(i)=fRCNO3(i)+  1.0000;

% 310, <315>
i=i+1;
Rnames{ 310} = 'xRHNO3 + NO = NO + RHNO3 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xRHNO3';Gstr{i,   2}='NO';
fxRHNO3(i)=fxRHNO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fRHNO3(i)=fRHNO3(i)+  1.0000;

% 311, <316>
i=i+1;
Rnames{ 311} = 'xRHNO3 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xRHNO3';Gstr{i,   2}='HO2';
fxRHNO3(i)=fxRHNO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 312, <317>
i=i+1;
Rnames{ 312} = 'xRHNO3 + NO3 = NO3 + RHNO3 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xRHNO3';Gstr{i,   2}='NO3';
fxRHNO3(i)=fxRHNO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fRHNO3(i)=fRHNO3(i)+  1.0000;

% 313, <318>
i=i+1;
Rnames{ 313} = 'xRHNO3 + SumRO2 = SumRO2 +  0.50000*RHNO3 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xRHNO3';Gstr{i,   2}='SumRO2';
fxRHNO3(i)=fxRHNO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fRHNO3(i)=fRHNO3(i)+  0.5000;

% 314, <319>
i=i+1;
Rnames{ 314} = 'xRHNO3 + SumRCO3 = SumRCO3 + RHNO3 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xRHNO3';Gstr{i,   2}='SumRCO3';
fxRHNO3(i)=fxRHNO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fRHNO3(i)=fRHNO3(i)+  1.0000;

% 315, <320>
i=i+1;
Rnames{ 315} = 'xRDNO3 + NO = NO + RDNO3 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xRDNO3';Gstr{i,   2}='NO';
fxRDNO3(i)=fxRDNO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fRDNO3(i)=fRDNO3(i)+  1.0000;

% 316, <321>
i=i+1;
Rnames{ 316} = 'xRDNO3 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xRDNO3';Gstr{i,   2}='HO2';
fxRDNO3(i)=fxRDNO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 317, <322>
i=i+1;
Rnames{ 317} = 'xRDNO3 + NO3 = NO3 + RDNO3 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xRDNO3';Gstr{i,   2}='NO3';
fxRDNO3(i)=fxRDNO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fRDNO3(i)=fRDNO3(i)+  1.0000;

% 318, <323>
i=i+1;
Rnames{ 318} = 'xRDNO3 + SumRO2 = SumRO2 +  0.50000*RDNO3 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xRDNO3';Gstr{i,   2}='SumRO2';
fxRDNO3(i)=fxRDNO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fRDNO3(i)=fRDNO3(i)+  0.5000;

% 319, <324>
i=i+1;
Rnames{ 319} = 'xRDNO3 + SumRCO3 = SumRCO3 + RDNO3 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xRDNO3';Gstr{i,   2}='SumRCO3';
fxRDNO3(i)=fxRDNO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fRDNO3(i)=fRDNO3(i)+  1.0000;

% 320, <330>
i=i+1;
Rnames{ 320} = 'xHPCRB + NO = NO + HPCRB ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xHPCRB';Gstr{i,   2}='NO';
fxHPCRB(i)=fxHPCRB(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fHPCRB(i)=fHPCRB(i)+  1.0000;

% 321, <331>
i=i+1;
Rnames{ 321} = 'xHPCRB + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xHPCRB';Gstr{i,   2}='HO2';
fxHPCRB(i)=fxHPCRB(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 322, <332>
i=i+1;
Rnames{ 322} = 'xHPCRB + NO3 = NO3 + HPCRB ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xHPCRB';Gstr{i,   2}='NO3';
fxHPCRB(i)=fxHPCRB(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fHPCRB(i)=fHPCRB(i)+  1.0000;

% 323, <333>
i=i+1;
Rnames{ 323} = 'xHPCRB + SumRO2 = SumRO2 +  0.50000*HPCRB ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xHPCRB';Gstr{i,   2}='SumRO2';
fxHPCRB(i)=fxHPCRB(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fHPCRB(i)=fHPCRB(i)+  0.5000;

% 324, <334>
i=i+1;
Rnames{ 324} = 'xHPCRB + SumRCO3 = SumRCO3 + HPCRB ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xHPCRB';Gstr{i,   2}='SumRCO3';
fxHPCRB(i)=fxHPCRB(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fHPCRB(i)=fHPCRB(i)+  1.0000;

% 325, <335>
i=i+1;
Rnames{ 325} = 'xAFG1 + NO = NO + AFG1 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xAFG1';Gstr{i,   2}='NO';
fxAFG1(i)=fxAFG1(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fAFG1(i)=fAFG1(i)+  1.0000;

% 326, <336>
i=i+1;
Rnames{ 326} = 'xAFG1 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xAFG1';Gstr{i,   2}='HO2';
fxAFG1(i)=fxAFG1(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 327, <337>
i=i+1;
Rnames{ 327} = 'xAFG1 + NO3 = NO3 + AFG1 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xAFG1';Gstr{i,   2}='NO3';
fxAFG1(i)=fxAFG1(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fAFG1(i)=fAFG1(i)+  1.0000;

% 328, <338>
i=i+1;
Rnames{ 328} = 'xAFG1 + SumRO2 = SumRO2 +  0.50000*AFG1 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xAFG1';Gstr{i,   2}='SumRO2';
fxAFG1(i)=fxAFG1(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fAFG1(i)=fAFG1(i)+  0.5000;

% 329, <339>
i=i+1;
Rnames{ 329} = 'xAFG1 + SumRCO3 = SumRCO3 + AFG1 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xAFG1';Gstr{i,   2}='SumRCO3';
fxAFG1(i)=fxAFG1(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fAFG1(i)=fAFG1(i)+  1.0000;

% 330, <340>
i=i+1;
Rnames{ 330} = 'xAFG2A + NO = NO + AFG2A ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xAFG2A';Gstr{i,   2}='NO';
fxAFG2A(i)=fxAFG2A(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fAFG2A(i)=fAFG2A(i)+  1.0000;

% 331, <341>
i=i+1;
Rnames{ 331} = 'xAFG2A + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xAFG2A';Gstr{i,   2}='HO2';
fxAFG2A(i)=fxAFG2A(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 332, <342>
i=i+1;
Rnames{ 332} = 'xAFG2A + NO3 = NO3 + AFG2A ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xAFG2A';Gstr{i,   2}='NO3';
fxAFG2A(i)=fxAFG2A(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fAFG2A(i)=fAFG2A(i)+  1.0000;

% 333, <343>
i=i+1;
Rnames{ 333} = 'xAFG2A + SumRO2 = SumRO2 +  0.50000*AFG2A ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xAFG2A';Gstr{i,   2}='SumRO2';
fxAFG2A(i)=fxAFG2A(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fAFG2A(i)=fAFG2A(i)+  0.5000;

% 334, <344>
i=i+1;
Rnames{ 334} = 'xAFG2A + SumRCO3 = SumRCO3 + AFG2A ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xAFG2A';Gstr{i,   2}='SumRCO3';
fxAFG2A(i)=fxAFG2A(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fAFG2A(i)=fAFG2A(i)+  1.0000;

% 335, <345>
i=i+1;
Rnames{ 335} = 'xAFG2B + NO = NO + AFG2B ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xAFG2B';Gstr{i,   2}='NO';
fxAFG2B(i)=fxAFG2B(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fAFG2B(i)=fAFG2B(i)+  1.0000;

% 336, <346>
i=i+1;
Rnames{ 336} = 'xAFG2B + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xAFG2B';Gstr{i,   2}='HO2';
fxAFG2B(i)=fxAFG2B(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 337, <347>
i=i+1;
Rnames{ 337} = 'xAFG2B + NO3 = NO3 + AFG2B ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xAFG2B';Gstr{i,   2}='NO3';
fxAFG2B(i)=fxAFG2B(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fAFG2B(i)=fAFG2B(i)+  1.0000;

% 338, <348>
i=i+1;
Rnames{ 338} = 'xAFG2B + SumRO2 = SumRO2 +  0.50000*AFG2B ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xAFG2B';Gstr{i,   2}='SumRO2';
fxAFG2B(i)=fxAFG2B(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fAFG2B(i)=fAFG2B(i)+  0.5000;

% 339, <349>
i=i+1;
Rnames{ 339} = 'xAFG2B + SumRCO3 = SumRCO3 + AFG2B ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xAFG2B';Gstr{i,   2}='SumRCO3';
fxAFG2B(i)=fxAFG2B(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fAFG2B(i)=fAFG2B(i)+  1.0000;

% 340, <350>
i=i+1;
Rnames{ 340} = 'xAFG3 + NO = NO + AFG3 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xAFG3';Gstr{i,   2}='NO';
fxAFG3(i)=fxAFG3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fAFG3(i)=fAFG3(i)+  1.0000;

% 341, <351>
i=i+1;
Rnames{ 341} = 'xAFG3 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xAFG3';Gstr{i,   2}='HO2';
fxAFG3(i)=fxAFG3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 342, <352>
i=i+1;
Rnames{ 342} = 'xAFG3 + NO3 = NO3 + AFG3 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xAFG3';Gstr{i,   2}='NO3';
fxAFG3(i)=fxAFG3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fAFG3(i)=fAFG3(i)+  1.0000;

% 343, <353>
i=i+1;
Rnames{ 343} = 'xAFG3 + SumRO2 = SumRO2 +  0.50000*AFG3 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xAFG3';Gstr{i,   2}='SumRO2';
fxAFG3(i)=fxAFG3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fAFG3(i)=fAFG3(i)+  0.5000;

% 344, <354>
i=i+1;
Rnames{ 344} = 'xAFG3 + SumRCO3 = SumRCO3 + AFG3 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xAFG3';Gstr{i,   2}='SumRCO3';
fxAFG3(i)=fxAFG3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fAFG3(i)=fAFG3(i)+  1.0000;

% 345, <355>
i=i+1;
Rnames{ 345} = 'xPAN2 + NO = NO + PAN2 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xPAN2';Gstr{i,   2}='NO';
fxPAN2(i)=fxPAN2(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fPAN2(i)=fPAN2(i)+  1.0000;

% 346, <356>
i=i+1;
Rnames{ 346} = 'xPAN2 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xPAN2';Gstr{i,   2}='HO2';
fxPAN2(i)=fxPAN2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 347, <357>
i=i+1;
Rnames{ 347} = 'xPAN2 + NO3 = NO3 + PAN2 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xPAN2';Gstr{i,   2}='NO3';
fxPAN2(i)=fxPAN2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fPAN2(i)=fPAN2(i)+  1.0000;

% 348, <358>
i=i+1;
Rnames{ 348} = 'xPAN2 + SumRO2 = SumRO2 +  0.50000*PAN2 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xPAN2';Gstr{i,   2}='SumRO2';
fxPAN2(i)=fxPAN2(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fPAN2(i)=fPAN2(i)+  0.5000;

% 349, <359>
i=i+1;
Rnames{ 349} = 'xPAN2 + SumRCO3 = SumRCO3 + PAN2 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xPAN2';Gstr{i,   2}='SumRCO3';
fxPAN2(i)=fxPAN2(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fPAN2(i)=fPAN2(i)+  1.0000;

% 350, <365>
i=i+1;
Rnames{ 350} = 'xMEO2 + NO = NO + MEO2 + SumRO2 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xMEO2';Gstr{i,   2}='NO';
fxMEO2(i)=fxMEO2(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fMEO2(i)=fMEO2(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 351, <366>
i=i+1;
Rnames{ 351} = 'xMEO2 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xMEO2';Gstr{i,   2}='HO2';
fxMEO2(i)=fxMEO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 352, <367>
i=i+1;
Rnames{ 352} = 'xMEO2 + NO3 = NO3 + MEO2 + SumRO2 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xMEO2';Gstr{i,   2}='NO3';
fxMEO2(i)=fxMEO2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fMEO2(i)=fMEO2(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 353, <368>
i=i+1;
Rnames{ 353} = 'xMEO2 + SumRO2 = 1.50000*SumRO2 +  0.50000*MEO2 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xMEO2';Gstr{i,   2}='SumRO2';
fxMEO2(i)=fxMEO2(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.5000;fMEO2(i)=fMEO2(i)+  0.5000;

% 354, <369>
i=i+1;
Rnames{ 354} = 'xMEO2 + SumRCO3 = SumRCO3 + MEO2 + SumRO2 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xMEO2';Gstr{i,   2}='SumRCO3';
fxMEO2(i)=fxMEO2(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fMEO2(i)=fMEO2(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 355, <370>
i=i+1;
Rnames{ 355} = 'xETO2 + NO = NO + ETO2 + SumRO2 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xETO2';Gstr{i,   2}='NO';
fxETO2(i)=fxETO2(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fETO2(i)=fETO2(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 356, <371>
i=i+1;
Rnames{ 356} = 'xETO2 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xETO2';Gstr{i,   2}='HO2';
fxETO2(i)=fxETO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 357, <372>
i=i+1;
Rnames{ 357} = 'xETO2 + NO3 = NO3 + ETO2 + SumRO2 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xETO2';Gstr{i,   2}='NO3';
fxETO2(i)=fxETO2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fETO2(i)=fETO2(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 358, <373>
i=i+1;
Rnames{ 358} = 'xETO2 + SumRO2 = 1.50000*SumRO2 +  0.50000*ETO2 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xETO2';Gstr{i,   2}='SumRO2';
fxETO2(i)=fxETO2(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.5000;fETO2(i)=fETO2(i)+  0.5000;

% 359, <374>
i=i+1;
Rnames{ 359} = 'xETO2 + SumRCO3 = SumRCO3 + ETO2 + SumRO2 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xETO2';Gstr{i,   2}='SumRCO3';
fxETO2(i)=fxETO2(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fETO2(i)=fETO2(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 360, <375>
i=i+1;
Rnames{ 360} = 'xMECO3 + NO = NO + MECO3 + SumRCO3 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xMECO3';Gstr{i,   2}='NO';
fxMECO3(i)=fxMECO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fMECO3(i)=fMECO3(i)+  1.0000;fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 361, <376>
i=i+1;
Rnames{ 361} = 'xMECO3 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xMECO3';Gstr{i,   2}='HO2';
fxMECO3(i)=fxMECO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 362, <377>
i=i+1;
Rnames{ 362} = 'xMECO3 + NO3 = NO3 + MECO3 + SumRCO3 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xMECO3';Gstr{i,   2}='NO3';
fxMECO3(i)=fxMECO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fMECO3(i)=fMECO3(i)+  1.0000;fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 363, <378>
i=i+1;
Rnames{ 363} = 'xMECO3 + SumRO2 = SumRO2 +  0.50000*MECO3 +  0.50000*SumRCO3 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xMECO3';Gstr{i,   2}='SumRO2';
fxMECO3(i)=fxMECO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fMECO3(i)=fMECO3(i)+  0.5000;fSumRCO3(i)=fSumRCO3(i)+  0.5000;

% 364, <379>
i=i+1;
Rnames{ 364} = 'xMECO3 + SumRCO3 = 2.00000*SumRCO3 + MECO3 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xMECO3';Gstr{i,   2}='SumRCO3';
fxMECO3(i)=fxMECO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  2.0000;fMECO3(i)=fMECO3(i)+  1.0000;

% 365, <380>
i=i+1;
Rnames{ 365} = 'xR2CO3 + NO = NO + R2CO3 + SumRCO3 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xR2CO3';Gstr{i,   2}='NO';
fxR2CO3(i)=fxR2CO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fR2CO3(i)=fR2CO3(i)+  1.0000;fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 366, <381>
i=i+1;
Rnames{ 366} = 'xR2CO3 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xR2CO3';Gstr{i,   2}='HO2';
fxR2CO3(i)=fxR2CO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 367, <382>
i=i+1;
Rnames{ 367} = 'xR2CO3 + NO3 = NO3 + R2CO3 + SumRCO3 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xR2CO3';Gstr{i,   2}='NO3';
fxR2CO3(i)=fxR2CO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fR2CO3(i)=fR2CO3(i)+  1.0000;fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 368, <383>
i=i+1;
Rnames{ 368} = 'xR2CO3 + SumRO2 = SumRO2 +  0.50000*R2CO3 +  0.50000*SumRCO3 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xR2CO3';Gstr{i,   2}='SumRO2';
fxR2CO3(i)=fxR2CO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fR2CO3(i)=fR2CO3(i)+  0.5000;fSumRCO3(i)=fSumRCO3(i)+  0.5000;

% 369, <384>
i=i+1;
Rnames{ 369} = 'xR2CO3 + SumRCO3 = 2.00000*SumRCO3 + R2CO3 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xR2CO3';Gstr{i,   2}='SumRCO3';
fxR2CO3(i)=fxR2CO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  2.0000;fR2CO3(i)=fR2CO3(i)+  1.0000;

% 370, <385>
i=i+1;
Rnames{ 370} = 'xMACO3 + NO = NO + MACO3 + SumRCO3 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xMACO3';Gstr{i,   2}='NO';
fxMACO3(i)=fxMACO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fMACO3(i)=fMACO3(i)+  1.0000;fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 371, <386>
i=i+1;
Rnames{ 371} = 'xMACO3 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xMACO3';Gstr{i,   2}='HO2';
fxMACO3(i)=fxMACO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 372, <387>
i=i+1;
Rnames{ 372} = 'xMACO3 + NO3 = NO3 + MACO3 + SumRCO3 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xMACO3';Gstr{i,   2}='NO3';
fxMACO3(i)=fxMACO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fMACO3(i)=fMACO3(i)+  1.0000;fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 373, <388>
i=i+1;
Rnames{ 373} = 'xMACO3 + SumRO2 = SumRO2 +  0.50000*MACO3 +  0.50000*SumRCO3 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xMACO3';Gstr{i,   2}='SumRO2';
fxMACO3(i)=fxMACO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fMACO3(i)=fMACO3(i)+  0.5000;fSumRCO3(i)=fSumRCO3(i)+  0.5000;

% 374, <389>
i=i+1;
Rnames{ 374} = 'xMACO3 + SumRCO3 = 2.00000*SumRCO3 + MACO3 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xMACO3';Gstr{i,   2}='SumRCO3';
fxMACO3(i)=fxMACO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  2.0000;fMACO3(i)=fMACO3(i)+  1.0000;

% 375, <390>
i=i+1;
Rnames{ 375} = 'xTBUO + NO = NO + TBUO ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xTBUO';Gstr{i,   2}='NO';
fxTBUO(i)=fxTBUO(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fTBUO(i)=fTBUO(i)+  1.0000;

% 376, <391>
i=i+1;
Rnames{ 376} = 'xTBUO + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xTBUO';Gstr{i,   2}='HO2';
fxTBUO(i)=fxTBUO(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 377, <392>
i=i+1;
Rnames{ 377} = 'xTBUO + NO3 = NO3 + TBUO ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xTBUO';Gstr{i,   2}='NO3';
fxTBUO(i)=fxTBUO(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fTBUO(i)=fTBUO(i)+  1.0000;

% 378, <393>
i=i+1;
Rnames{ 378} = 'xTBUO + SumRO2 = SumRO2 +  0.50000*TBUO ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xTBUO';Gstr{i,   2}='SumRO2';
fxTBUO(i)=fxTBUO(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fTBUO(i)=fTBUO(i)+  0.5000;

% 379, <394>
i=i+1;
Rnames{ 379} = 'xTBUO + SumRCO3 = SumRCO3 + TBUO ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xTBUO';Gstr{i,   2}='SumRCO3';
fxTBUO(i)=fxTBUO(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fTBUO(i)=fTBUO(i)+  1.0000;

% 380, <395>
i=i+1;
Rnames{ 380} = 'xBZO + NO = NO + BZO ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='xBZO';Gstr{i,   2}='NO';
fxBZO(i)=fxBZO(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fBZO(i)=fBZO(i)+  1.0000;

% 381, <396>
i=i+1;
Rnames{ 381} = 'xBZO + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='xBZO';Gstr{i,   2}='HO2';
fxBZO(i)=fxBZO(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 382, <397>
i=i+1;
Rnames{ 382} = 'xBZO + NO3 = NO3 + BZO ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='xBZO';Gstr{i,   2}='NO3';
fxBZO(i)=fxBZO(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fBZO(i)=fBZO(i)+  1.0000;

% 383, <398>
i=i+1;
Rnames{ 383} = 'xBZO + SumRO2 = SumRO2 +  0.50000*BZO ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='xBZO';Gstr{i,   2}='SumRO2';
fxBZO(i)=fxBZO(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fBZO(i)=fBZO(i)+  0.5000;

% 384, <399>
i=i+1;
Rnames{ 384} = 'xBZO + SumRCO3 = SumRCO3 + BZO ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='xBZO';Gstr{i,   2}='SumRCO3';
fxBZO(i)=fxBZO(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fBZO(i)=fBZO(i)+  1.0000;

% 385, <400>
i=i+1;
Rnames{ 385} = 'yROOH + NO = NO ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='yROOH';Gstr{i,   2}='NO';
fyROOH(i)=fyROOH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;

% 386, <401>
i=i+1;
Rnames{ 386} = 'yROOH + HO2 = HO2 + ROOH ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='yROOH';Gstr{i,   2}='HO2';
fyROOH(i)=fyROOH(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fROOH(i)=fROOH(i)+  1.0000;

% 387, <402>
i=i+1;
Rnames{ 387} = 'yROOH + NO3 = NO3 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='yROOH';Gstr{i,   2}='NO3';
fyROOH(i)=fyROOH(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;

% 388, <403>
i=i+1;
Rnames{ 388} = 'yROOH + SumRO2 = SumRO2 +  0.50000*KET2 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='yROOH';Gstr{i,   2}='SumRO2';
fyROOH(i)=fyROOH(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fKET2(i)=fKET2(i)+  0.5000;

% 389, <404>
i=i+1;
Rnames{ 389} = 'yROOH + SumRCO3 = SumRCO3 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='yROOH';Gstr{i,   2}='SumRCO3';
fyROOH(i)=fyROOH(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 390, <405>
i=i+1;
Rnames{ 390} = 'yRUOOH + NO = NO ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='yRUOOH';Gstr{i,   2}='NO';
fyRUOOH(i)=fyRUOOH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;

% 391, <406>
i=i+1;
Rnames{ 391} = 'yRUOOH + HO2 = HO2 + RUOOH ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='yRUOOH';Gstr{i,   2}='HO2';
fyRUOOH(i)=fyRUOOH(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fRUOOH(i)=fRUOOH(i)+  1.0000;

% 392, <407>
i=i+1;
Rnames{ 392} = 'yRUOOH + NO3 = NO3 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='yRUOOH';Gstr{i,   2}='NO3';
fyRUOOH(i)=fyRUOOH(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;

% 393, <408>
i=i+1;
Rnames{ 393} = 'yRUOOH + SumRO2 = SumRO2 +  0.50000*OLEP ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='yRUOOH';Gstr{i,   2}='SumRO2';
fyRUOOH(i)=fyRUOOH(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fOLEP(i)=fOLEP(i)+  0.5000;

% 394, <409>
i=i+1;
Rnames{ 394} = 'yRUOOH + SumRCO3 = SumRCO3 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='yRUOOH';Gstr{i,   2}='SumRCO3';
fyRUOOH(i)=fyRUOOH(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 395, <410>
i=i+1;
Rnames{ 395} = 'yRAOOH + NO = NO ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='yRAOOH';Gstr{i,   2}='NO';
fyRAOOH(i)=fyRAOOH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;

% 396, <411>
i=i+1;
Rnames{ 396} = 'yRAOOH + HO2 = HO2 + RAOOH ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='yRAOOH';Gstr{i,   2}='HO2';
fyRAOOH(i)=fyRAOOH(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fRAOOH(i)=fRAOOH(i)+  1.0000;

% 397, <412>
i=i+1;
Rnames{ 397} = 'yRAOOH + NO3 = NO3 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='yRAOOH';Gstr{i,   2}='NO3';
fyRAOOH(i)=fyRAOOH(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;

% 398, <413>
i=i+1;
Rnames{ 398} = 'yRAOOH + SumRO2 = SumRO2 +  0.50000*OLEP ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='yRAOOH';Gstr{i,   2}='SumRO2';
fyRAOOH(i)=fyRAOOH(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fOLEP(i)=fOLEP(i)+  0.5000;

% 399, <414>
i=i+1;
Rnames{ 399} = 'yRAOOH + SumRCO3 = SumRCO3 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='yRAOOH';Gstr{i,   2}='SumRCO3';
fyRAOOH(i)=fyRAOOH(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 400, <415>
i=i+1;
Rnames{ 400} = 'yHPCRB + NO = NO ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='yHPCRB';Gstr{i,   2}='NO';
fyHPCRB(i)=fyHPCRB(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;

% 401, <416>
i=i+1;
Rnames{ 401} = 'yHPCRB + HO2 = HO2 + HPCRB ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='yHPCRB';Gstr{i,   2}='HO2';
fyHPCRB(i)=fyHPCRB(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fHPCRB(i)=fHPCRB(i)+  1.0000;

% 402, <417>
i=i+1;
Rnames{ 402} = 'yHPCRB + NO3 = NO3 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='yHPCRB';Gstr{i,   2}='NO3';
fyHPCRB(i)=fyHPCRB(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;

% 403, <418>
i=i+1;
Rnames{ 403} = 'yHPCRB + SumRO2 = SumRO2 +  0.50000*KET2 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='yHPCRB';Gstr{i,   2}='SumRO2';
fyHPCRB(i)=fyHPCRB(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fKET2(i)=fKET2(i)+  0.5000;

% 404, <419>
i=i+1;
Rnames{ 404} = 'yHPCRB + SumRCO3 = SumRCO3 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='yHPCRB';Gstr{i,   2}='SumRCO3';
fyHPCRB(i)=fyHPCRB(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 405, <420>
i=i+1;
Rnames{ 405} = 'yRPNO3 + NO = NO ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='yRPNO3';Gstr{i,   2}='NO';
fyRPNO3(i)=fyRPNO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;

% 406, <421>
i=i+1;
Rnames{ 406} = 'yRPNO3 + HO2 = HO2 + RPNO3 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='yRPNO3';Gstr{i,   2}='HO2';
fyRPNO3(i)=fyRPNO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fRPNO3(i)=fRPNO3(i)+  1.0000;

% 407, <422>
i=i+1;
Rnames{ 407} = 'yRPNO3 + NO3 = NO3 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='yRPNO3';Gstr{i,   2}='NO3';
fyRPNO3(i)=fyRPNO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;

% 408, <423>
i=i+1;
Rnames{ 408} = 'yRPNO3 + SumRO2 = SumRO2 +  0.50000*R1NO3 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='yRPNO3';Gstr{i,   2}='SumRO2';
fyRPNO3(i)=fyRPNO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fR1NO3(i)=fR1NO3(i)+  0.5000;

% 409, <424>
i=i+1;
Rnames{ 409} = 'yRPNO3 + SumRCO3 = SumRCO3 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='yRPNO3';Gstr{i,   2}='SumRCO3';
fyRPNO3(i)=fyRPNO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 410, <425>
i=i+1;
Rnames{ 410} = 'zR1NO3 + NO = NO + R1NO3 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='zR1NO3';Gstr{i,   2}='NO';
fzR1NO3(i)=fzR1NO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fR1NO3(i)=fR1NO3(i)+  1.0000;

% 411, <426>
i=i+1;
Rnames{ 411} = 'zR1NO3 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='zR1NO3';Gstr{i,   2}='HO2';
fzR1NO3(i)=fzR1NO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 412, <427>
i=i+1;
Rnames{ 412} = 'zR1NO3 + NO3 = NO3 + KET2 + HO2 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='zR1NO3';Gstr{i,   2}='NO3';
fzR1NO3(i)=fzR1NO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fKET2(i)=fKET2(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 413, <428>
i=i+1;
Rnames{ 413} = 'zR1NO3 + SumRO2 = SumRO2 +  0.50000*KET2 +  0.50000*HO2 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='zR1NO3';Gstr{i,   2}='SumRO2';
fzR1NO3(i)=fzR1NO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fKET2(i)=fKET2(i)+  0.5000;fHO2(i)=fHO2(i)+  0.5000;

% 414, <429>
i=i+1;
Rnames{ 414} = 'zR1NO3 + SumRCO3 = SumRCO3 + KET2 + HO2 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='zR1NO3';Gstr{i,   2}='SumRCO3';
fzR1NO3(i)=fzR1NO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fKET2(i)=fKET2(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 415, <430>
i=i+1;
Rnames{ 415} = 'zR2NO3 + NO = NO + R2NO3 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='zR2NO3';Gstr{i,   2}='NO';
fzR2NO3(i)=fzR2NO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fR2NO3(i)=fR2NO3(i)+  1.0000;

% 416, <431>
i=i+1;
Rnames{ 416} = 'zR2NO3 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='zR2NO3';Gstr{i,   2}='HO2';
fzR2NO3(i)=fzR2NO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 417, <432>
i=i+1;
Rnames{ 417} = 'zR2NO3 + NO3 = NO3 + KET2 + HO2 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='zR2NO3';Gstr{i,   2}='NO3';
fzR2NO3(i)=fzR2NO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fKET2(i)=fKET2(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 418, <433>
i=i+1;
Rnames{ 418} = 'zR2NO3 + SumRO2 = SumRO2 +  0.50000*KET2 +  0.50000*HO2 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='zR2NO3';Gstr{i,   2}='SumRO2';
fzR2NO3(i)=fzR2NO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fKET2(i)=fKET2(i)+  0.5000;fHO2(i)=fHO2(i)+  0.5000;

% 419, <434>
i=i+1;
Rnames{ 419} = 'zR2NO3 + SumRCO3 = SumRCO3 + KET2 + HO2 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='zR2NO3';Gstr{i,   2}='SumRCO3';
fzR2NO3(i)=fzR2NO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fKET2(i)=fKET2(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 420, <435>
i=i+1;
Rnames{ 420} = 'zRHNO3 + NO = NO + RHNO3 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='zRHNO3';Gstr{i,   2}='NO';
fzRHNO3(i)=fzRHNO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fRHNO3(i)=fRHNO3(i)+  1.0000;

% 421, <436>
i=i+1;
Rnames{ 421} = 'zRHNO3 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='zRHNO3';Gstr{i,   2}='HO2';
fzRHNO3(i)=fzRHNO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 422, <437>
i=i+1;
Rnames{ 422} = 'zRHNO3 + NO3 = NO3 + KET2 + HO2 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='zRHNO3';Gstr{i,   2}='NO3';
fzRHNO3(i)=fzRHNO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fKET2(i)=fKET2(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 423, <438>
i=i+1;
Rnames{ 423} = 'zRHNO3 + SumRO2 = SumRO2 +  0.50000*KET2 +  0.50000*HO2 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='zRHNO3';Gstr{i,   2}='SumRO2';
fzRHNO3(i)=fzRHNO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fKET2(i)=fKET2(i)+  0.5000;fHO2(i)=fHO2(i)+  0.5000;

% 424, <439>
i=i+1;
Rnames{ 424} = 'zRHNO3 + SumRCO3 = SumRCO3 + KET2 + HO2 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='zRHNO3';Gstr{i,   2}='SumRCO3';
fzRHNO3(i)=fzRHNO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fKET2(i)=fKET2(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 425, <440>
i=i+1;
Rnames{ 425} = 'zRCNO3 + NO = NO + RCNO3 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='zRCNO3';Gstr{i,   2}='NO';
fzRCNO3(i)=fzRCNO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fRCNO3(i)=fRCNO3(i)+  1.0000;

% 426, <441>
i=i+1;
Rnames{ 426} = 'zRCNO3 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='zRCNO3';Gstr{i,   2}='HO2';
fzRCNO3(i)=fzRCNO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 427, <442>
i=i+1;
Rnames{ 427} = 'zRCNO3 + NO3 = NO3 + KET2 + HO2 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='zRCNO3';Gstr{i,   2}='NO3';
fzRCNO3(i)=fzRCNO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fKET2(i)=fKET2(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 428, <443>
i=i+1;
Rnames{ 428} = 'zRCNO3 + SumRO2 = SumRO2 +  0.50000*KET2 +  0.50000*HO2 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='zRCNO3';Gstr{i,   2}='SumRO2';
fzRCNO3(i)=fzRCNO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fKET2(i)=fKET2(i)+  0.5000;fHO2(i)=fHO2(i)+  0.5000;

% 429, <444>
i=i+1;
Rnames{ 429} = 'zRCNO3 + SumRCO3 = SumRCO3 + KET2 + HO2 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='zRCNO3';Gstr{i,   2}='SumRCO3';
fzRCNO3(i)=fzRCNO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fKET2(i)=fKET2(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 430, <445>
i=i+1;
Rnames{ 430} = 'zRANO3 + NO = NO + RANO3 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='zRANO3';Gstr{i,   2}='NO';
fzRANO3(i)=fzRANO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fRANO3(i)=fRANO3(i)+  1.0000;

% 431, <446>
i=i+1;
Rnames{ 431} = 'zRANO3 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='zRANO3';Gstr{i,   2}='HO2';
fzRANO3(i)=fzRANO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 432, <447>
i=i+1;
Rnames{ 432} = 'zRANO3 + NO3 = NO3 + RUOOH + HO2 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='zRANO3';Gstr{i,   2}='NO3';
fzRANO3(i)=fzRANO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fRUOOH(i)=fRUOOH(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 433, <448>
i=i+1;
Rnames{ 433} = 'zRANO3 + SumRO2 = SumRO2 +  0.50000*RUOOH +  0.50000*HO2 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='zRANO3';Gstr{i,   2}='SumRO2';
fzRANO3(i)=fzRANO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fRUOOH(i)=fRUOOH(i)+  0.5000;fHO2(i)=fHO2(i)+  0.5000;

% 434, <449>
i=i+1;
Rnames{ 434} = 'zRANO3 + SumRCO3 = SumRCO3 + RUOOH + HO2 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='zRANO3';Gstr{i,   2}='SumRCO3';
fzRANO3(i)=fzRANO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fRUOOH(i)=fRUOOH(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 435, <450>
i=i+1;
Rnames{ 435} = 'zRPNO3 + NO = NO + RPNO3 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='zRPNO3';Gstr{i,   2}='NO';
fzRPNO3(i)=fzRPNO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fRPNO3(i)=fRPNO3(i)+  1.0000;

% 436, <451>
i=i+1;
Rnames{ 436} = 'zRPNO3 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='zRPNO3';Gstr{i,   2}='HO2';
fzRPNO3(i)=fzRPNO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 437, <452>
i=i+1;
Rnames{ 437} = 'zRPNO3 + NO3 = NO3 + RUOOH + HO2 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='zRPNO3';Gstr{i,   2}='NO3';
fzRPNO3(i)=fzRPNO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fRUOOH(i)=fRUOOH(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 438, <453>
i=i+1;
Rnames{ 438} = 'zRPNO3 + SumRO2 = SumRO2 +  0.50000*RUOOH +  0.50000*HO2 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='zRPNO3';Gstr{i,   2}='SumRO2';
fzRPNO3(i)=fzRPNO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fRUOOH(i)=fRUOOH(i)+  0.5000;fHO2(i)=fHO2(i)+  0.5000;

% 439, <454>
i=i+1;
Rnames{ 439} = 'zRPNO3 + SumRCO3 = SumRCO3 + RUOOH + HO2 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='zRPNO3';Gstr{i,   2}='SumRCO3';
fzRPNO3(i)=fzRPNO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fRUOOH(i)=fRUOOH(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 440, <455>
i=i+1;
Rnames{ 440} = 'zRDNO3 + NO = NO + RDNO3 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='zRDNO3';Gstr{i,   2}='NO';
fzRDNO3(i)=fzRDNO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fRDNO3(i)=fRDNO3(i)+  1.0000;

% 441, <456>
i=i+1;
Rnames{ 441} = 'zRDNO3 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='zRDNO3';Gstr{i,   2}='HO2';
fzRDNO3(i)=fzRDNO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 442, <457>
i=i+1;
Rnames{ 442} = 'zRDNO3 + NO3 = NO3 + R1NO3 + HO2 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='zRDNO3';Gstr{i,   2}='NO3';
fzRDNO3(i)=fzRDNO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fR1NO3(i)=fR1NO3(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 443, <458>
i=i+1;
Rnames{ 443} = 'zRDNO3 + SumRO2 = SumRO2 +  0.50000*R1NO3 +  0.50000*HO2 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='zRDNO3';Gstr{i,   2}='SumRO2';
fzRDNO3(i)=fzRDNO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fR1NO3(i)=fR1NO3(i)+  0.5000;fHO2(i)=fHO2(i)+  0.5000;

% 444, <459>
i=i+1;
Rnames{ 444} = 'zRDNO3 + SumRCO3 = SumRCO3 + R1NO3 + HO2 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='zRDNO3';Gstr{i,   2}='SumRCO3';
fzRDNO3(i)=fzRDNO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fR1NO3(i)=fR1NO3(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 445, <460>
i=i+1;
Rnames{ 445} = 'zRNNO3 + NO = NO + RNNO3 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='zRNNO3';Gstr{i,   2}='NO';
fzRNNO3(i)=fzRNNO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fRNNO3(i)=fRNNO3(i)+  1.0000;

% 446, <461>
i=i+1;
Rnames{ 446} = 'zRNNO3 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='zRNNO3';Gstr{i,   2}='HO2';
fzRNNO3(i)=fzRNNO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 447, <462>
i=i+1;
Rnames{ 447} = 'zRNNO3 + NO3 = NO3 + OTHN + HO2 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='zRNNO3';Gstr{i,   2}='NO3';
fzRNNO3(i)=fzRNNO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fOTHN(i)=fOTHN(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 448, <463>
i=i+1;
Rnames{ 448} = 'zRNNO3 + SumRO2 = SumRO2 +  0.50000*OTHN +  0.50000*HO2 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='zRNNO3';Gstr{i,   2}='SumRO2';
fzRNNO3(i)=fzRNNO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fOTHN(i)=fOTHN(i)+  0.5000;fHO2(i)=fHO2(i)+  0.5000;

% 449, <464>
i=i+1;
Rnames{ 449} = 'zRNNO3 + SumRCO3 = SumRCO3 + OTHN + HO2 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='zRNNO3';Gstr{i,   2}='SumRCO3';
fzRNNO3(i)=fzRNNO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fOTHN(i)=fOTHN(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 450, <465>
i=i+1;
Rnames{ 450} = 'zPAN2 + NO = NO + PAN2 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='zPAN2';Gstr{i,   2}='NO';
fzPAN2(i)=fzPAN2(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fPAN2(i)=fPAN2(i)+  1.0000;

% 451, <466>
i=i+1;
Rnames{ 451} = 'zPAN2 + HO2 = HO2 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='zPAN2';Gstr{i,   2}='HO2';
fzPAN2(i)=fzPAN2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;

% 452, <467>
i=i+1;
Rnames{ 452} = 'zPAN2 + NO3 = NO3 + RCHO + HO2 ';
k(:,i) = (k(:,  49) ); 
Gstr{i,   1}='zPAN2';Gstr{i,   2}='NO3';
fzPAN2(i)=fzPAN2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;fRCHO(i)=fRCHO(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 453, <468>
i=i+1;
Rnames{ 453} = 'zPAN2 + SumRO2 = SumRO2 +  0.50000*RCHO +  0.50000*HO2 ';
k(:,i) = (k(:,  50) ); 
Gstr{i,   1}='zPAN2';Gstr{i,   2}='SumRO2';
fzPAN2(i)=fzPAN2(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fSumRO2(i)=fSumRO2(i)+  1.0000;fRCHO(i)=fRCHO(i)+  0.5000;fHO2(i)=fHO2(i)+  0.5000;

% 454, <469>
i=i+1;
Rnames{ 454} = 'zPAN2 + SumRCO3 = SumRCO3 + RCHO + HO2 ';
k(:,i) = (k(:,  55) ); 
Gstr{i,   1}='zPAN2';Gstr{i,   2}='SumRCO3';
fzPAN2(i)=fzPAN2(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fSumRCO3(i)=fSumRCO3(i)+  1.0000;fRCHO(i)=fRCHO(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;

% 455, <Q2NO>
i=i+1;
Rnames{ 455} = 'R2CO3 + NO = NO2 +  0.95000*xHO2 +  0.96000*RO2C +  0.04000*RO2XC +  0.95000*xETCHO +  0.04000*zR1NO3 + yROOH + CO2 + SumRO2 ';
k(:,i) = (  6.7000E-12.*exp(  3.4000E+02./T) ); 
Gstr{i,   1}='R2CO3';Gstr{i,   2}='NO';
fR2CO3(i)=fR2CO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.9500;fRO2C(i)=fRO2C(i)+  0.9600;fRO2XC(i)=fRO2XC(i)+  0.0400;fxETCHO(i)=fxETCHO(i)+  0.9500;fzR1NO3(i)=fzR1NO3(i)+  0.0400;fyROOH(i)=fyROOH(i)+  1.0000;fCO2(i)=fCO2(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 456, <Q2N2>
i=i+1;
Rnames{ 456} = 'R2CO3 + NO2 = PAN2 ';
k(:,i) = (  7.7000E-12 ); 
Gstr{i,   1}='R2CO3';Gstr{i,   2}='NO2';
fR2CO3(i)=fR2CO3(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fPAN2(i)=fPAN2(i)+  1.0000;

% 457, <Q2N3>
i=i+1;
Rnames{ 457} = 'R2CO3 + NO3 = NO2 +  0.95000*xHO2 +  0.96000*RO2C +  0.04000*RO2XC +  0.95000*xETCHO +  0.04000*zR1NO3 + yROOH + CO2 + SumRO2 ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='R2CO3';Gstr{i,   2}='NO3';
fR2CO3(i)=fR2CO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.9500;fRO2C(i)=fRO2C(i)+  0.9600;fRO2XC(i)=fRO2XC(i)+  0.0400;fxETCHO(i)=fxETCHO(i)+  0.9500;fzR1NO3(i)=fzR1NO3(i)+  0.0400;fyROOH(i)=fyROOH(i)+  1.0000;fCO2(i)=fCO2(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 458, <Q2H2>
i=i+1;
Rnames{ 458} = 'R2CO3 + HO2 = 0.13000*O3 +  0.50000*OH +  0.48000*xHO2 +  0.48000*RO2C +  0.02000*RO2XC +  0.48000*xETCHO +  0.13000*OACID +  0.37000*PACID +  0.02000*zR1NO3 +  0.50000*yROOH +  0.50000*CO2 +  0.50000*SumRO2 ';
k(:,i) = (  2.2000E-11 ); 
Gstr{i,   1}='R2CO3';Gstr{i,   2}='HO2';
fR2CO3(i)=fR2CO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fO3(i)=fO3(i)+  0.1300;fOH(i)=fOH(i)+  0.5000;fxHO2(i)=fxHO2(i)+  0.4800;fRO2C(i)=fRO2C(i)+  0.4800;fRO2XC(i)=fRO2XC(i)+  0.0200;fxETCHO(i)=fxETCHO(i)+  0.4800;fOACID(i)=fOACID(i)+  0.1300;fPACID(i)=fPACID(i)+  0.3700;fzR1NO3(i)=fzR1NO3(i)+  0.0200;fyROOH(i)=fyROOH(i)+  0.5000;fCO2(i)=fCO2(i)+  0.5000;fSumRO2(i)=fSumRO2(i)+  0.5000;

% 459, <Q2R2>
i=i+1;
Rnames{ 459} = 'R2CO3 + SumRO2 = 0.86000*xHO2 +  0.86000*RO2C +  0.04000*RO2XC +  0.86000*xETCHO +  0.10000*OACID +  0.04000*zR1NO3 +  0.90000*yROOH +  0.90000*CO2 +  0.90000*SumRO2 ';
k(:,i) = (  1.6000E-11 ); 
Gstr{i,   1}='R2CO3';Gstr{i,   2}='SumRO2';
fR2CO3(i)=fR2CO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fxHO2(i)=fxHO2(i)+  0.8600;fRO2C(i)=fRO2C(i)+  0.8600;fRO2XC(i)=fRO2XC(i)+  0.0400;fxETCHO(i)=fxETCHO(i)+  0.8600;fOACID(i)=fOACID(i)+  0.1000;fzR1NO3(i)=fzR1NO3(i)+  0.0400;fyROOH(i)=fyROOH(i)+  0.9000;fCO2(i)=fCO2(i)+  0.9000;fSumRO2(i)=fSumRO2(i)+  0.9000;

% 460, <Q2R3>
i=i+1;
Rnames{ 460} = 'R2CO3 + SumRCO3 = 0.95000*xHO2 +  0.96000*RO2C +  0.04000*RO2XC +  0.95000*xETCHO +  0.04000*zR1NO3 + yROOH + CO2 + SumRO2 ';
k(:,i) = (  1.4000E-11 ); 
Gstr{i,   1}='R2CO3';Gstr{i,   2}='SumRCO3';
fR2CO3(i)=fR2CO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fxHO2(i)=fxHO2(i)+  0.9500;fRO2C(i)=fRO2C(i)+  0.9600;fRO2XC(i)=fRO2XC(i)+  0.0400;fxETCHO(i)=fxETCHO(i)+  0.9500;fzR1NO3(i)=fzR1NO3(i)+  0.0400;fyROOH(i)=fyROOH(i)+  1.0000;fCO2(i)=fCO2(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 461, <Q5UI>
i=i+1;
Rnames{ 461} = 'MACO3 = 0.93000*xHO2 +  0.93000*RO2C +  0.07000*RO2XC +  0.93000*xPACID +  0.07000*zRCNO3 + SumRO2 ';
k(:,i) = (  7.7900E+08.*exp( -5.0030E+03./T) ); 
Gstr{i,   1}='MACO3';
fMACO3(i)=fMACO3(i)-1.0;
fxHO2(i)=fxHO2(i)+  0.9300;fRO2C(i)=fRO2C(i)+  0.9300;fRO2XC(i)=fRO2XC(i)+  0.0700;fxPACID(i)=fxPACID(i)+  0.9300;fzRCNO3(i)=fzRCNO3(i)+  0.0700;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 462, <Q5NO>
i=i+1;
Rnames{ 462} = 'MACO3 + NO = NO2 + MEO2 + HCHO + CO2 + CO + SumRO2 ';
k(:,i) = (  6.7000E-12.*exp(  3.4000E+02./T) ); 
Gstr{i,   1}='MACO3';Gstr{i,   2}='NO';
fMACO3(i)=fMACO3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;fMEO2(i)=fMEO2(i)+  1.0000;fHCHO(i)=fHCHO(i)+  1.0000;fCO2(i)=fCO2(i)+  1.0000;fCO(i)=fCO(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 463, <Q5N2>
i=i+1;
Rnames{ 463} = 'MACO3 + NO2 = APANS ';
k(:,i) = (  7.7000E-12 ); 
Gstr{i,   1}='MACO3';Gstr{i,   2}='NO2';
fMACO3(i)=fMACO3(i)-1.0;fNO2(i)=fNO2(i)-1.0;
fAPANS(i)=fAPANS(i)+  1.0000;

% 464, <Q5N3>
i=i+1;
Rnames{ 464} = 'MACO3 + NO3 = NO2 + MEO2 + HCHO + CO2 + CO + SumRO2 ';
k(:,i) = (  4.0000E-12 ); 
Gstr{i,   1}='MACO3';Gstr{i,   2}='NO3';
fMACO3(i)=fMACO3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;fMEO2(i)=fMEO2(i)+  1.0000;fHCHO(i)=fHCHO(i)+  1.0000;fCO2(i)=fCO2(i)+  1.0000;fCO(i)=fCO(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 465, <Q5H2>
i=i+1;
Rnames{ 465} = 'MACO3 + HO2 = 0.13000*O3 +  0.50000*OH +  0.50000*MEO2 +  0.50000*HCHO +  0.37000*PACID +  0.13000*OLEP +  0.50000*CO2 +  0.50000*CO +  0.50000*SumRO2 ';
k(:,i) = (  2.2000E-11 ); 
Gstr{i,   1}='MACO3';Gstr{i,   2}='HO2';
fMACO3(i)=fMACO3(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fO3(i)=fO3(i)+  0.1300;fOH(i)=fOH(i)+  0.5000;fMEO2(i)=fMEO2(i)+  0.5000;fHCHO(i)=fHCHO(i)+  0.5000;fPACID(i)=fPACID(i)+  0.3700;fOLEP(i)=fOLEP(i)+  0.1300;fCO2(i)=fCO2(i)+  0.5000;fCO(i)=fCO(i)+  0.5000;fSumRO2(i)=fSumRO2(i)+  0.5000;

% 466, <Q5R2>
i=i+1;
Rnames{ 466} = 'MACO3 + SumRO2 = 0.90000*MEO2 +  0.90000*HCHO +  0.10000*OLEP +  0.90000*CO2 +  0.90000*CO +  0.90000*SumRO2 ';
k(:,i) = (  1.6000E-11 ); 
Gstr{i,   1}='MACO3';Gstr{i,   2}='SumRO2';
fMACO3(i)=fMACO3(i)-1.0;fSumRO2(i)=fSumRO2(i)-1.0;
fMEO2(i)=fMEO2(i)+  0.9000;fHCHO(i)=fHCHO(i)+  0.9000;fOLEP(i)=fOLEP(i)+  0.1000;fCO2(i)=fCO2(i)+  0.9000;fCO(i)=fCO(i)+  0.9000;fSumRO2(i)=fSumRO2(i)+  0.9000;

% 467, <Q5R3>
i=i+1;
Rnames{ 467} = 'MACO3 + SumRCO3 = MEO2 + HCHO + CO2 + CO + SumRO2 ';
k(:,i) = (  1.4000E-11 ); 
Gstr{i,   1}='MACO3';Gstr{i,   2}='SumRCO3';
fMACO3(i)=fMACO3(i)-1.0;fSumRCO3(i)=fSumRCO3(i)-1.0;
fMEO2(i)=fMEO2(i)+  1.0000;fHCHO(i)=fHCHO(i)+  1.0000;fCO2(i)=fCO2(i)+  1.0000;fCO(i)=fCO(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 468, <C2OH>
i=i+1;
Rnames{ 468} = 'ETHAN + OH = ETO2 + SumRO2 ';
k(:,i) = (  1.5100E-12.*exp( -5.3300E+02./T).*(T./300).^(  1.9200E+00 ) ); 
Gstr{i,   1}='ETHAN';Gstr{i,   2}='OH';
fETHAN(i)=fETHAN(i)-1.0;fOH(i)=fOH(i)-1.0;
fETO2(i)=fETO2(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 469, <C3OH>
i=i+1;
Rnames{ 469} = 'PROP + OH = 0.95000*xHO2 +  0.96000*RO2C +  0.04000*RO2XC +  0.01000*xMEO2 +  0.01000*xMECHO +  0.27000*xETCHO +  0.68000*xACET +  0.04000*zR1NO3 + yROOH + SumRO2 ';
k(:,i) = (  2.0000E-12.*exp( -1.7200E+02./T).*(T./300).^(  1.7600E+00 ) ); 
Gstr{i,   1}='PROP';Gstr{i,   2}='OH';
fPROP(i)=fPROP(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  0.9500;fRO2C(i)=fRO2C(i)+  0.9600;fRO2XC(i)=fRO2XC(i)+  0.0400;fxMEO2(i)=fxMEO2(i)+  0.0100;fxMECHO(i)=fxMECHO(i)+  0.0100;fxETCHO(i)=fxETCHO(i)+  0.2700;fxACET(i)=fxACET(i)+  0.6800;fzR1NO3(i)=fzR1NO3(i)+  0.0400;fyROOH(i)=fyROOH(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 470, <C4OH>
i=i+1;
Rnames{ 470} = 'NC4 + OH = 0.59000*xHO2 +  1.02000*RO2C +  0.08000*RO2XC +  0.33000*xETO2 +  0.33000*xMECHO +  0.12000*xRCHO +  0.48000*xMEK +  0.07000*zR1NO3 +  0.01000*zRHNO3 +  1.10000*yROOH +  1.10000*SumRO2 ';
k(:,i) = (  2.0900E-12.*exp(  4.2000E+01./T).*(T./300).^(  1.8200E+00 ) ); 
Gstr{i,   1}='NC4';Gstr{i,   2}='OH';
fNC4(i)=fNC4(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  0.5900;fRO2C(i)=fRO2C(i)+  1.0200;fRO2XC(i)=fRO2XC(i)+  0.0800;fxETO2(i)=fxETO2(i)+  0.3300;fxMECHO(i)=fxMECHO(i)+  0.3300;fxRCHO(i)=fxRCHO(i)+  0.1200;fxMEK(i)=fxMEK(i)+  0.4800;fzR1NO3(i)=fzR1NO3(i)+  0.0700;fzRHNO3(i)=fzRHNO3(i)+  0.0100;fyROOH(i)=fyROOH(i)+  1.1000;fSumRO2(i)=fSumRO2(i)+  1.1000;

% 471, <E1OH>
i=i+1;
Rnames{ 471} = 'ETHEN + OH = xHO2 + RO2C +  1.48000*xHCHO +  0.26000*xGLCHO + yROOH + SumRO2 ';
xko =   1.1000E-28.*M.*exp(  0.0000E+00./T).*(T./300).^ -3.5000E+00;
xkinf =   8.4000E-12.*exp(  0.0000E+00./T).*(T./300).^ -1.7500E+00;
xn =   1.0000E+00;
F =   6.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='ETHEN';Gstr{i,   2}='OH';
fETHEN(i)=fETHEN(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  1.0000;fRO2C(i)=fRO2C(i)+  1.0000;fxHCHO(i)=fxHCHO(i)+  1.4800;fxGLCHO(i)=fxGLCHO(i)+  0.2600;fyROOH(i)=fyROOH(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 472, <E1O3>
i=i+1;
Rnames{ 472} = 'ETHEN + O3 = 0.17000*OH +  0.27000*HO2 +  0.42000*HCHO2 + HCHO +  0.23000*CO2 +  0.35000*CO ';
k(:,i) = (  6.8200E-15.*exp( -2.5000E+03./T) ); 
Gstr{i,   1}='ETHEN';Gstr{i,   2}='O3';
fETHEN(i)=fETHEN(i)-1.0;fO3(i)=fO3(i)-1.0;
fOH(i)=fOH(i)+  0.1700;fHO2(i)=fHO2(i)+  0.2700;fHCHO2(i)=fHCHO2(i)+  0.4200;fHCHO(i)=fHCHO(i)+  1.0000;fCO2(i)=fCO2(i)+  0.2300;fCO(i)=fCO(i)+  0.3500;

% 473, <E1N3>
i=i+1;
Rnames{ 473} = 'ETHEN + NO3 = 0.01000*xNO2 +  0.99000*xHO2 + RO2C +  0.01000*xHCHO +  0.99000*xRCNO3 + yRPNO3 + SumRO2 ';
k(:,i) = (  3.3000E-12.*exp( -2.8800E+03./T) ); 
Gstr{i,   1}='ETHEN';Gstr{i,   2}='NO3';
fETHEN(i)=fETHEN(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fxNO2(i)=fxNO2(i)+  0.0100;fxHO2(i)=fxHO2(i)+  0.9900;fRO2C(i)=fRO2C(i)+  1.0000;fxHCHO(i)=fxHCHO(i)+  0.0100;fxRCNO3(i)=fxRCNO3(i)+  0.9900;fyRPNO3(i)=fyRPNO3(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 474, <E1OP>
i=i+1;
Rnames{ 474} = 'ETHEN + O3P = ETHEN_OP +  0.80000*HO2 +  0.22000*xHO2 +  0.29000*RO2C +  0.51000*MEO2 +  0.07000*xHCHO +  0.10000*MECHO +  4.41000*NROG +  0.51000*CO +  0.80000*SumRO2 ';
k(:,i) = (  1.0700E-11.*exp( -8.0000E+02./T) ); 
Gstr{i,   1}='ETHEN';Gstr{i,   2}='O3P';
fETHEN(i)=fETHEN(i)-1.0;fO3P(i)=fO3P(i)-1.0;
fETHEN_OP(i)=fETHEN_OP(i)+  1.0000;fHO2(i)=fHO2(i)+  0.8000;fxHO2(i)=fxHO2(i)+  0.2200;fRO2C(i)=fRO2C(i)+  0.2900;fMEO2(i)=fMEO2(i)+  0.5100;fxHCHO(i)=fxHCHO(i)+  0.0700;fMECHO(i)=fMECHO(i)+  0.1000;fNROG(i)=fNROG(i)+  4.4100;fCO(i)=fCO(i)+  0.5100;fSumRO2(i)=fSumRO2(i)+  0.8000;

% 475, <495>
i=i+1;
Rnames{ 475} = 'ETHEN_OP = 0.07000*xOH +  0.22000*xPACID +  0.07000*CO2 ';
k(:,i) = (  2.5700E+00 ); 
Gstr{i,   1}='ETHEN_OP';
fETHEN_OP(i)=fETHEN_OP(i)-1.0;
fxOH(i)=fxOH(i)+  0.0700;fxPACID(i)=fxPACID(i)+  0.2200;fCO2(i)=fCO2(i)+  0.0700;

% 476, <496>
i=i+1;
Rnames{ 476} = 'ETHEN_OP + NO = NO +  0.07000*xHO2 +  0.20000*xHCHO +  0.02000*xGLY +  0.24000*yHPCRB +  0.27000*CO ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='ETHEN_OP';Gstr{i,   2}='NO';
fETHEN_OP(i)=fETHEN_OP(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0700;fxHCHO(i)=fxHCHO(i)+  0.2000;fxGLY(i)=fxGLY(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.2400;fCO(i)=fCO(i)+  0.2700;

% 477, <E2OH>
i=i+1;
Rnames{ 477} = 'PROPE + OH = 0.97000*xHO2 +  0.97000*RO2C +  0.03000*RO2XC +  0.97000*xHCHO +  0.97000*xMECHO +  0.03000*zRHNO3 + yROOH + SumRO2 ';
k(:,i) = (  1.2000E-11.*exp(  2.1000E+02./T).*(T./300).^( -6.2000E-01 ) ); 
Gstr{i,   1}='PROPE';Gstr{i,   2}='OH';
fPROPE(i)=fPROPE(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  0.9700;fRO2C(i)=fRO2C(i)+  0.9700;fRO2XC(i)=fRO2XC(i)+  0.0300;fxHCHO(i)=fxHCHO(i)+  0.9700;fxMECHO(i)=fxMECHO(i)+  0.9700;fzRHNO3(i)=fzRHNO3(i)+  0.0300;fyROOH(i)=fyROOH(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 478, <E2O3>
i=i+1;
Rnames{ 478} = 'PROPE + O3 = PROPE_O3 +  0.30000*OH +  0.17000*HO2 +  0.16000*xHO2 +  0.22000*RO2C +  0.03000*MEO2 +  0.21000*HCHO2 +  0.12000*MECHO2 +  0.50000*HCHO +  0.05000*xHCHO +  0.50000*MECHO +  0.05000*MEOH +  0.24000*CO2 +  0.22000*CO +  0.25000*SumRO2 ';
k(:,i) = (  5.7700E-15.*exp( -1.8800E+03./T) ); 
Gstr{i,   1}='PROPE';Gstr{i,   2}='O3';
fPROPE(i)=fPROPE(i)-1.0;fO3(i)=fO3(i)-1.0;
fPROPE_O3(i)=fPROPE_O3(i)+  1.0000;fOH(i)=fOH(i)+  0.3000;fHO2(i)=fHO2(i)+  0.1700;fxHO2(i)=fxHO2(i)+  0.1600;fRO2C(i)=fRO2C(i)+  0.2200;fMEO2(i)=fMEO2(i)+  0.0300;fHCHO2(i)=fHCHO2(i)+  0.2100;fMECHO2(i)=fMECHO2(i)+  0.1200;fHCHO(i)=fHCHO(i)+  0.5000;fxHCHO(i)=fxHCHO(i)+  0.0500;fMECHO(i)=fMECHO(i)+  0.5000;fMEOH(i)=fMEOH(i)+  0.0500;fCO2(i)=fCO2(i)+  0.2400;fCO(i)=fCO(i)+  0.2200;fSumRO2(i)=fSumRO2(i)+  0.2500;

% 479, <499>
i=i+1;
Rnames{ 479} = 'PROPE_O3 = 0.05000*xOH +  0.16000*xPACID +  0.05000*CO2 ';
k(:,i) = (  2.4500E+00 ); 
Gstr{i,   1}='PROPE_O3';
fPROPE_O3(i)=fPROPE_O3(i)-1.0;
fxOH(i)=fxOH(i)+  0.0500;fxPACID(i)=fxPACID(i)+  0.1600;fCO2(i)=fCO2(i)+  0.0500;

% 480, <500>
i=i+1;
Rnames{ 480} = 'PROPE_O3 + NO = NO +  0.06000*xHO2 +  0.15000*xHCHO +  0.18000*yHPCRB +  0.20000*CO ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='PROPE_O3';Gstr{i,   2}='NO';
fPROPE_O3(i)=fPROPE_O3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0600;fxHCHO(i)=fxHCHO(i)+  0.1500;fyHPCRB(i)=fyHPCRB(i)+  0.1800;fCO(i)=fCO(i)+  0.2000;

% 481, <E2N3>
i=i+1;
Rnames{ 481} = 'PROPE + NO3 = 0.29000*xNO2 +  0.68000*xHO2 +  0.97000*RO2C +  0.03000*RO2XC +  0.29000*xHCHO +  0.29000*xMECHO +  0.68000*xRCNO3 + yRPNO3 +  0.03000*zRDNO3 + SumRO2 ';
k(:,i) = (  4.6000E-13.*exp( -1.1550E+03./T) ); 
Gstr{i,   1}='PROPE';Gstr{i,   2}='NO3';
fPROPE(i)=fPROPE(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fxNO2(i)=fxNO2(i)+  0.2900;fxHO2(i)=fxHO2(i)+  0.6800;fRO2C(i)=fRO2C(i)+  0.9700;fRO2XC(i)=fRO2XC(i)+  0.0300;fxHCHO(i)=fxHCHO(i)+  0.2900;fxMECHO(i)=fxMECHO(i)+  0.2900;fxRCNO3(i)=fxRCNO3(i)+  0.6800;fyRPNO3(i)=fyRPNO3(i)+  1.0000;fzRDNO3(i)=fzRDNO3(i)+  0.0300;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 482, <E2OP>
i=i+1;
Rnames{ 482} = 'PROPE + O3P = 0.25000*ETCHO +  0.25000*ACET +  0.50000*ALK2 ';
k(:,i) = (  1.0200E-11.*exp( -2.8000E+02./T) ); 
Gstr{i,   1}='PROPE';Gstr{i,   2}='O3P';
fPROPE(i)=fPROPE(i)-1.0;fO3P(i)=fO3P(i)-1.0;
fETCHO(i)=fETCHO(i)+  0.2500;fACET(i)=fACET(i)+  0.2500;fALK2(i)=fALK2(i)+  0.5000;

% 483, <IPOH>
i=i+1;
Rnames{ 483} = 'ISOP + OH = ISOP_OH +  0.55000*xHO2 +  0.55000*RO2C +  0.05000*RO2XC +  0.50000*xHCHO +  0.23000*xMACR +  0.27000*xMVK +  0.05000*zRHNO3 +  0.60000*yRUOOH +  0.05000*xFURNS +  0.60000*SumRO2 ';
k(:,i) = (  2.7000E-11.*exp(  3.9000E+02./T) ); 
Gstr{i,   1}='ISOP';Gstr{i,   2}='OH';
fISOP(i)=fISOP(i)-1.0;fOH(i)=fOH(i)-1.0;
fISOP_OH(i)=fISOP_OH(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.5500;fRO2C(i)=fRO2C(i)+  0.5500;fRO2XC(i)=fRO2XC(i)+  0.0500;fxHCHO(i)=fxHCHO(i)+  0.5000;fxMACR(i)=fxMACR(i)+  0.2300;fxMVK(i)=fxMVK(i)+  0.2700;fzRHNO3(i)=fzRHNO3(i)+  0.0500;fyRUOOH(i)=fyRUOOH(i)+  0.6000;fxFURNS(i)=fxFURNS(i)+  0.0500;fSumRO2(i)=fSumRO2(i)+  0.6000;

% 484, <504>
i=i+1;
Rnames{ 484} = 'ISOP_OH = 0.06000*OH +  0.34000*HO2 +  0.39000*HPCRB ';
k(:,i) = (  1.2900E+00 ); 
Gstr{i,   1}='ISOP_OH';
fISOP_OH(i)=fISOP_OH(i)-1.0;
fOH(i)=fOH(i)+  0.0600;fHO2(i)=fHO2(i)+  0.3400;fHPCRB(i)=fHPCRB(i)+  0.3900;

% 485, <505>
i=i+1;
Rnames{ 485} = 'ISOP_OH + NO = NO +  0.35000*xHO2 +  0.38000*RO2C +  0.05000*RO2XC +  0.03000*xHCHO +  0.34000*xOLEA1 +  0.05000*zRHNO3 +  0.42000*yRUOOH +  0.43000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='ISOP_OH';Gstr{i,   2}='NO';
fISOP_OH(i)=fISOP_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.3500;fRO2C(i)=fRO2C(i)+  0.3800;fRO2XC(i)=fRO2XC(i)+  0.0500;fxHCHO(i)=fxHCHO(i)+  0.0300;fxOLEA1(i)=fxOLEA1(i)+  0.3400;fzRHNO3(i)=fzRHNO3(i)+  0.0500;fyRUOOH(i)=fyRUOOH(i)+  0.4200;fSumRO2(i)=fSumRO2(i)+  0.4300;

% 486, <IPO3>
i=i+1;
Rnames{ 486} = 'ISOP + O3 = ISOP_O3 +  0.13000*OH +  0.29000*HO2 +  0.12000*xHO2 +  0.33000*RO2C +  0.01000*RO2XC +  0.16000*MECO3 +  0.13000*xMECO3 +  0.04000*xMACO3 +  0.23000*HCHO2 +  0.06000*RCHO2 +  0.40000*HCHO +  0.21000*xHCHO +  0.39000*MACR +  0.16000*MVK +  0.05000*OLEP +  0.01000*zRCNO3 +  0.15000*yHPCRB +  0.13000*CO2 +  0.33000*CO +  0.34000*SumRO2 +  0.16000*SumRCO3 + ISOPRXN ';
k(:,i) = (  1.0500E-14.*exp( -2.0000E+03./T) ); 
Gstr{i,   1}='ISOP';Gstr{i,   2}='O3';
fISOP(i)=fISOP(i)-1.0;fO3(i)=fO3(i)-1.0;
fISOP_O3(i)=fISOP_O3(i)+  1.0000;fOH(i)=fOH(i)+  0.1300;fHO2(i)=fHO2(i)+  0.2900;fxHO2(i)=fxHO2(i)+  0.1200;fRO2C(i)=fRO2C(i)+  0.3300;fRO2XC(i)=fRO2XC(i)+  0.0100;fMECO3(i)=fMECO3(i)+  0.1600;fxMECO3(i)=fxMECO3(i)+  0.1300;fxMACO3(i)=fxMACO3(i)+  0.0400;fHCHO2(i)=fHCHO2(i)+  0.2300;fRCHO2(i)=fRCHO2(i)+  0.0600;fHCHO(i)=fHCHO(i)+  0.4000;fxHCHO(i)=fxHCHO(i)+  0.2100;fMACR(i)=fMACR(i)+  0.3900;fMVK(i)=fMVK(i)+  0.1600;fOLEP(i)=fOLEP(i)+  0.0500;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.1500;fCO2(i)=fCO2(i)+  0.1300;fCO(i)=fCO(i)+  0.3300;fSumRO2(i)=fSumRO2(i)+  0.3400;fSumRCO3(i)=fSumRCO3(i)+  0.1600;fISOPRXN(i)=fISOPRXN(i)+  1.0000;

% 487, <507>
i=i+1;
Rnames{ 487} = 'ISOP_O3 = 0.04000*xOH +  0.12000*xPACID +  0.04000*CO2 ';
k(:,i) = (  2.8100E+00 ); 
Gstr{i,   1}='ISOP_O3';
fISOP_O3(i)=fISOP_O3(i)-1.0;
fxOH(i)=fxOH(i)+  0.0400;fxPACID(i)=fxPACID(i)+  0.1200;fCO2(i)=fCO2(i)+  0.0400;

% 488, <508>
i=i+1;
Rnames{ 488} = 'ISOP_O3 + NO = NO +  0.04000*xHO2 +  0.11000*xHCHO +  0.13000*yHPCRB +  0.14000*CO ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='ISOP_O3';Gstr{i,   2}='NO';
fISOP_O3(i)=fISOP_O3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0400;fxHCHO(i)=fxHCHO(i)+  0.1100;fyHPCRB(i)=fyHPCRB(i)+  0.1300;fCO(i)=fCO(i)+  0.1400;

% 489, <IPN3>
i=i+1;
Rnames{ 489} = 'ISOP + NO3 = ISOP_N3 +  0.70000*xNO2 +  0.03000*xHO2 +  0.90000*RO2C +  0.10000*RO2XC +  0.48000*xHCHO +  0.22000*xOLEA1 +  0.48000*xMVK +  0.03000*xRCNO3 + yRPNO3 +  0.10000*zRDNO3 + SumRO2 + ISOPRXN ';
k(:,i) = (  2.9500E-12.*exp( -4.5000E+02./T) ); 
Gstr{i,   1}='ISOP';Gstr{i,   2}='NO3';
fISOP(i)=fISOP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fISOP_N3(i)=fISOP_N3(i)+  1.0000;fxNO2(i)=fxNO2(i)+  0.7000;fxHO2(i)=fxHO2(i)+  0.0300;fRO2C(i)=fRO2C(i)+  0.9000;fRO2XC(i)=fRO2XC(i)+  0.1000;fxHCHO(i)=fxHCHO(i)+  0.4800;fxOLEA1(i)=fxOLEA1(i)+  0.2200;fxMVK(i)=fxMVK(i)+  0.4800;fxRCNO3(i)=fxRCNO3(i)+  0.0300;fyRPNO3(i)=fyRPNO3(i)+  1.0000;fzRDNO3(i)=fzRDNO3(i)+  0.1000;fSumRO2(i)=fSumRO2(i)+  1.0000;fISOPRXN(i)=fISOPRXN(i)+  1.0000;

% 490, <510>
i=i+1;
Rnames{ 490} = 'ISOP_N3 = 0.12000*NO2 +  0.04000*HO2 +  0.04000*RPNO3 +  0.12000*HPCRB ';
k(:,i) = (  1.0300E+00 ); 
Gstr{i,   1}='ISOP_N3';
fISOP_N3(i)=fISOP_N3(i)-1.0;
fNO2(i)=fNO2(i)+  0.1200;fHO2(i)=fHO2(i)+  0.0400;fRPNO3(i)=fRPNO3(i)+  0.0400;fHPCRB(i)=fHPCRB(i)+  0.1200;

% 491, <511>
i=i+1;
Rnames{ 491} = 'ISOP_N3 + NO = NO +  0.15000*xHO2 +  0.15000*RO2C +  0.01000*RO2XC +  0.10000*xHCHO +  0.10000*xRCNO3 +  0.05000*xRHNO3 +  0.17000*yRPNO3 +  0.01000*zRDNO3 +  0.16000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='ISOP_N3';Gstr{i,   2}='NO';
fISOP_N3(i)=fISOP_N3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.1500;fRO2C(i)=fRO2C(i)+  0.1500;fRO2XC(i)=fRO2XC(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.1000;fxRCNO3(i)=fxRCNO3(i)+  0.1000;fxRHNO3(i)=fxRHNO3(i)+  0.0500;fyRPNO3(i)=fyRPNO3(i)+  0.1700;fzRDNO3(i)=fzRDNO3(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  0.1600;

% 492, <IPOP>
i=i+1;
Rnames{ 492} = 'ISOP + O3P = 0.23000*RO2C +  0.02000*RO2XC +  0.25000*MEO2 +  0.23000*xMACO3 +  0.23000*xHCHO +  0.75000*OLEP +  0.02000*zRCNO3 +  0.21000*yHPCRB +  0.50000*SumRO2 ';
k(:,i) = (  3.5000E-11 ); 
Gstr{i,   1}='ISOP';Gstr{i,   2}='O3P';
fISOP(i)=fISOP(i)-1.0;fO3P(i)=fO3P(i)-1.0;
fRO2C(i)=fRO2C(i)+  0.2300;fRO2XC(i)=fRO2XC(i)+  0.0200;fMEO2(i)=fMEO2(i)+  0.2500;fxMACO3(i)=fxMACO3(i)+  0.2300;fxHCHO(i)=fxHCHO(i)+  0.2300;fOLEP(i)=fOLEP(i)+  0.7500;fzRCNO3(i)=fzRCNO3(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.2100;fSumRO2(i)=fSumRO2(i)+  0.5000;

% 493, <E3OH>
i=i+1;
Rnames{ 493} = 'BUT13 + OH = BUT13_OH +  0.63000*xHO2 +  0.63000*RO2C +  0.04000*RO2XC +  0.58000*xHCHO +  0.58000*xACRO +  0.04000*zRHNO3 +  0.67000*yRUOOH +  0.05000*xFURNS +  0.67000*SumRO2 ';
k(:,i) = (  1.1200E-11.*exp(  5.3000E+02./T) ); 
Gstr{i,   1}='BUT13';Gstr{i,   2}='OH';
fBUT13(i)=fBUT13(i)-1.0;fOH(i)=fOH(i)-1.0;
fBUT13_OH(i)=fBUT13_OH(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.6300;fRO2C(i)=fRO2C(i)+  0.6300;fRO2XC(i)=fRO2XC(i)+  0.0400;fxHCHO(i)=fxHCHO(i)+  0.5800;fxACRO(i)=fxACRO(i)+  0.5800;fzRHNO3(i)=fzRHNO3(i)+  0.0400;fyRUOOH(i)=fyRUOOH(i)+  0.6700;fxFURNS(i)=fxFURNS(i)+  0.0500;fSumRO2(i)=fSumRO2(i)+  0.6700;

% 494, <514>
i=i+1;
Rnames{ 494} = 'BUT13_OH = 0.31000*HO2 +  0.33000*HPCRB ';
k(:,i) = (  1.0700E+00 ); 
Gstr{i,   1}='BUT13_OH';
fBUT13_OH(i)=fBUT13_OH(i)-1.0;
fHO2(i)=fHO2(i)+  0.3100;fHPCRB(i)=fHPCRB(i)+  0.3300;

% 495, <515>
i=i+1;
Rnames{ 495} = 'BUT13_OH + NO = NO +  0.31000*xHO2 +  0.33000*RO2C +  0.02000*RO2XC +  0.03000*xHCHO +  0.31000*xOLEA1 +  0.02000*zRHNO3 +  0.35000*yRUOOH +  0.35000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='BUT13_OH';Gstr{i,   2}='NO';
fBUT13_OH(i)=fBUT13_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.3100;fRO2C(i)=fRO2C(i)+  0.3300;fRO2XC(i)=fRO2XC(i)+  0.0200;fxHCHO(i)=fxHCHO(i)+  0.0300;fxOLEA1(i)=fxOLEA1(i)+  0.3100;fzRHNO3(i)=fzRHNO3(i)+  0.0200;fyRUOOH(i)=fyRUOOH(i)+  0.3500;fSumRO2(i)=fSumRO2(i)+  0.3500;

% 496, <E3O3>
i=i+1;
Rnames{ 496} = 'BUT13 + O3 = BUT13_O3 +  0.08000*OH +  0.50000*HO2 +  0.27000*xHO2 +  0.36000*RO2C +  0.21000*HCHO2 +  0.14000*RCHO2 +  0.50000*HCHO +  0.09000*xHCHO +  0.50000*ACRO +  0.12000*CO2 +  0.54000*CO +  0.36000*SumRO2 ';
k(:,i) = (  1.3400E-14.*exp( -2.2830E+03./T) ); 
Gstr{i,   1}='BUT13';Gstr{i,   2}='O3';
fBUT13(i)=fBUT13(i)-1.0;fO3(i)=fO3(i)-1.0;
fBUT13_O3(i)=fBUT13_O3(i)+  1.0000;fOH(i)=fOH(i)+  0.0800;fHO2(i)=fHO2(i)+  0.5000;fxHO2(i)=fxHO2(i)+  0.2700;fRO2C(i)=fRO2C(i)+  0.3600;fHCHO2(i)=fHCHO2(i)+  0.2100;fRCHO2(i)=fRCHO2(i)+  0.1400;fHCHO(i)=fHCHO(i)+  0.5000;fxHCHO(i)=fxHCHO(i)+  0.0900;fACRO(i)=fACRO(i)+  0.5000;fCO2(i)=fCO2(i)+  0.1200;fCO(i)=fCO(i)+  0.5400;fSumRO2(i)=fSumRO2(i)+  0.3600;

% 497, <517>
i=i+1;
Rnames{ 497} = 'BUT13_O3 = 0.09000*xOH +  0.27000*xPACID +  0.09000*CO2 ';
k(:,i) = (  2.5500E+00 ); 
Gstr{i,   1}='BUT13_O3';
fBUT13_O3(i)=fBUT13_O3(i)-1.0;
fxOH(i)=fxOH(i)+  0.0900;fxPACID(i)=fxPACID(i)+  0.2700;fCO2(i)=fCO2(i)+  0.0900;

% 498, <518>
i=i+1;
Rnames{ 498} = 'BUT13_O3 + NO = NO +  0.09000*xHO2 +  0.25000*xHCHO +  0.02000*xGLY +  0.30000*yHPCRB +  0.33000*CO ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='BUT13_O3';Gstr{i,   2}='NO';
fBUT13_O3(i)=fBUT13_O3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0900;fxHCHO(i)=fxHCHO(i)+  0.2500;fxGLY(i)=fxGLY(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.3000;fCO(i)=fCO(i)+  0.3300;

% 499, <E3N3>
i=i+1;
Rnames{ 499} = 'BUT13 + NO3 = 0.89000*xNO2 +  0.06000*xHO2 +  0.94000*RO2C +  0.06000*RO2XC +  0.74000*xHCHO +  0.14000*xOLEA1 +  0.74000*xACRO +  0.06000*xRCNO3 + yRPNO3 +  0.06000*zRDNO3 + SumRO2 ';
k(:,i) = (  1.1000E-13 ); 
Gstr{i,   1}='BUT13';Gstr{i,   2}='NO3';
fBUT13(i)=fBUT13(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fxNO2(i)=fxNO2(i)+  0.8900;fxHO2(i)=fxHO2(i)+  0.0600;fRO2C(i)=fRO2C(i)+  0.9400;fRO2XC(i)=fRO2XC(i)+  0.0600;fxHCHO(i)=fxHCHO(i)+  0.7400;fxOLEA1(i)=fxOLEA1(i)+  0.1400;fxACRO(i)=fxACRO(i)+  0.7400;fxRCNO3(i)=fxRCNO3(i)+  0.0600;fyRPNO3(i)=fyRPNO3(i)+  1.0000;fzRDNO3(i)=fzRDNO3(i)+  0.0600;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 500, <E3OP>
i=i+1;
Rnames{ 500} = 'BUT13 + O3P = 0.25000*OLEA2 +  0.25000*MVK +  0.50000*OLEP ';
k(:,i) = (  2.2600E-11.*exp( -4.0000E+01./T) ); 
Gstr{i,   1}='BUT13';Gstr{i,   2}='O3P';
fBUT13(i)=fBUT13(i)-1.0;fO3P(i)=fO3P(i)-1.0;
fOLEA2(i)=fOLEA2(i)+  0.2500;fMVK(i)=fMVK(i)+  0.2500;fOLEP(i)=fOLEP(i)+  0.5000;

% 501, <APOH>
i=i+1;
Rnames{ 501} = 'APINE + OH = APINE_OH +  0.02000*HO2 +  0.59000*xHO2 +  1.07000*RO2C +  0.30000*RO2XC +  0.08000*xHCHO +  0.51000*xRCHO +  0.08000*xOLEA2 +  0.17000*xACET +  0.06000*xMVK +  0.03000*xLVKS +  0.01000*zR2NO3 +  0.01000*zRCNO3 +  0.28000*zRHNO3 +  0.53000*yROOH +  0.71000*yRUOOH +  0.02000*HPCRB +  0.13000*yHPCRB +  1.37000*SumRO2 + TRPRXN ';
k(:,i) = (  1.3400E-11.*exp(  4.1000E+02./T) ); 
Gstr{i,   1}='APINE';Gstr{i,   2}='OH';
fAPINE(i)=fAPINE(i)-1.0;fOH(i)=fOH(i)-1.0;
fAPINE_OH(i)=fAPINE_OH(i)+  1.0000;fHO2(i)=fHO2(i)+  0.0200;fxHO2(i)=fxHO2(i)+  0.5900;fRO2C(i)=fRO2C(i)+  1.0700;fRO2XC(i)=fRO2XC(i)+  0.3000;fxHCHO(i)=fxHCHO(i)+  0.0800;fxRCHO(i)=fxRCHO(i)+  0.5100;fxOLEA2(i)=fxOLEA2(i)+  0.0800;fxACET(i)=fxACET(i)+  0.1700;fxMVK(i)=fxMVK(i)+  0.0600;fxLVKS(i)=fxLVKS(i)+  0.0300;fzR2NO3(i)=fzR2NO3(i)+  0.0100;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fzRHNO3(i)=fzRHNO3(i)+  0.2800;fyROOH(i)=fyROOH(i)+  0.5300;fyRUOOH(i)=fyRUOOH(i)+  0.7100;fHPCRB(i)=fHPCRB(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.1300;fSumRO2(i)=fSumRO2(i)+  1.3700;fTRPRXN(i)=fTRPRXN(i)+  1.0000;

% 502, <522>
i=i+1;
Rnames{ 502} = 'APINE_OH = 0.01000*OH +  0.08000*HO2 +  0.07000*HPCRB ';
k(:,i) = (  7.4900E+00 ); 
Gstr{i,   1}='APINE_OH';
fAPINE_OH(i)=fAPINE_OH(i)-1.0;
fOH(i)=fOH(i)+  0.0100;fHO2(i)=fHO2(i)+  0.0800;fHPCRB(i)=fHPCRB(i)+  0.0700;

% 503, <523>
i=i+1;
Rnames{ 503} = 'APINE_OH + NO = NO +  0.06000*xHO2 +  0.08000*RO2C +  0.02000*RO2XC +  0.01000*xHCHO +  0.06000*xOLEA2 +  0.02000*zRHNO3 +  0.09000*yHPCRB +  0.10000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='APINE_OH';Gstr{i,   2}='NO';
fAPINE_OH(i)=fAPINE_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0600;fRO2C(i)=fRO2C(i)+  0.0800;fRO2XC(i)=fRO2XC(i)+  0.0200;fxHCHO(i)=fxHCHO(i)+  0.0100;fxOLEA2(i)=fxOLEA2(i)+  0.0600;fzRHNO3(i)=fzRHNO3(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.0900;fSumRO2(i)=fSumRO2(i)+  0.1000;

% 504, <APO3>
i=i+1;
Rnames{ 504} = 'APINE + O3 = 0.69000*OH +  0.01000*xOH +  0.01000*HO2 +  0.17000*xHO2 +  0.79000*RO2C +  0.26000*RO2XC +  0.03000*xMECO3 +  0.20000*xR2CO3 +  0.29000*RCHO2 +  0.20000*xHCHO +  0.02000*RCHO +  0.13000*xRCHO +  0.03000*xBACL +  0.09000*xACET +  0.03000*KET2 +  0.07000*xPACID +  0.26000*zRCNO3 +  0.82000*yHPCRB +  0.03000*CO2 +  0.17000*CO +  1.05000*SumRO2 + TRPRXN ';
k(:,i) = (  8.2200E-16.*exp( -6.4000E+02./T) ); 
Gstr{i,   1}='APINE';Gstr{i,   2}='O3';
fAPINE(i)=fAPINE(i)-1.0;fO3(i)=fO3(i)-1.0;
fOH(i)=fOH(i)+  0.6900;fxOH(i)=fxOH(i)+  0.0100;fHO2(i)=fHO2(i)+  0.0100;fxHO2(i)=fxHO2(i)+  0.1700;fRO2C(i)=fRO2C(i)+  0.7900;fRO2XC(i)=fRO2XC(i)+  0.2600;fxMECO3(i)=fxMECO3(i)+  0.0300;fxR2CO3(i)=fxR2CO3(i)+  0.2000;fRCHO2(i)=fRCHO2(i)+  0.2900;fxHCHO(i)=fxHCHO(i)+  0.2000;fRCHO(i)=fRCHO(i)+  0.0200;fxRCHO(i)=fxRCHO(i)+  0.1300;fxBACL(i)=fxBACL(i)+  0.0300;fxACET(i)=fxACET(i)+  0.0900;fKET2(i)=fKET2(i)+  0.0300;fxPACID(i)=fxPACID(i)+  0.0700;fzRCNO3(i)=fzRCNO3(i)+  0.2600;fyHPCRB(i)=fyHPCRB(i)+  0.8200;fCO2(i)=fCO2(i)+  0.0300;fCO(i)=fCO(i)+  0.1700;fSumRO2(i)=fSumRO2(i)+  1.0500;fTRPRXN(i)=fTRPRXN(i)+  1.0000;

% 505, <APN3>
i=i+1;
Rnames{ 505} = 'APINE + NO3 = 0.81000*xNO2 +  0.81000*RO2C +  0.19000*RO2XC +  0.81000*xRCHO + yRPNO3 +  0.19000*zRDNO3 + SumRO2 ';
k(:,i) = (  1.2000E-12.*exp(  4.9000E+02./T) ); 
Gstr{i,   1}='APINE';Gstr{i,   2}='NO3';
fAPINE(i)=fAPINE(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fxNO2(i)=fxNO2(i)+  0.8100;fRO2C(i)=fRO2C(i)+  0.8100;fRO2XC(i)=fRO2XC(i)+  0.1900;fxRCHO(i)=fxRCHO(i)+  0.8100;fyRPNO3(i)=fyRPNO3(i)+  1.0000;fzRDNO3(i)=fzRDNO3(i)+  0.1900;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 506, <APOP>
i=i+1;
Rnames{ 506} = 'APINE + O3P = 0.50000*KET2 +  0.50000*ALK4 + TRPRXN ';
k(:,i) = (  3.2000E-11 ); 
Gstr{i,   1}='APINE';Gstr{i,   2}='O3P';
fAPINE(i)=fAPINE(i)-1.0;fO3P(i)=fO3P(i)-1.0;
fKET2(i)=fKET2(i)+  0.5000;fALK4(i)=fALK4(i)+  0.5000;fTRPRXN(i)=fTRPRXN(i)+  1.0000;

% 507, <BPOH>
i=i+1;
Rnames{ 507} = 'BPINE + OH = BPINE_OH +  0.01000*xOH +  0.01000*HO2 +  0.37000*xHO2 +  1.43000*RO2C +  0.42000*RO2XC +  0.03000*xR2CO3 +  0.35000*xHCHO +  0.03000*xRCHO +  0.23000*xOLEA2 +  0.29000*xACET +  0.11000*xKET2 +  0.02000*xPACID +  0.02000*zR2NO3 +  0.09000*zRCNO3 +  0.31000*zRHNO3 +  0.32000*yROOH + yRUOOH +  0.01000*HPCRB +  0.45000*yHPCRB +  1.85000*SumRO2 + TRPRXN ';
k(:,i) = (  1.6200E-11.*exp(  4.6000E+02./T) ); 
Gstr{i,   1}='BPINE';Gstr{i,   2}='OH';
fBPINE(i)=fBPINE(i)-1.0;fOH(i)=fOH(i)-1.0;
fBPINE_OH(i)=fBPINE_OH(i)+  1.0000;fxOH(i)=fxOH(i)+  0.0100;fHO2(i)=fHO2(i)+  0.0100;fxHO2(i)=fxHO2(i)+  0.3700;fRO2C(i)=fRO2C(i)+  1.4300;fRO2XC(i)=fRO2XC(i)+  0.4200;fxR2CO3(i)=fxR2CO3(i)+  0.0300;fxHCHO(i)=fxHCHO(i)+  0.3500;fxRCHO(i)=fxRCHO(i)+  0.0300;fxOLEA2(i)=fxOLEA2(i)+  0.2300;fxACET(i)=fxACET(i)+  0.2900;fxKET2(i)=fxKET2(i)+  0.1100;fxPACID(i)=fxPACID(i)+  0.0200;fzR2NO3(i)=fzR2NO3(i)+  0.0200;fzRCNO3(i)=fzRCNO3(i)+  0.0900;fzRHNO3(i)=fzRHNO3(i)+  0.3100;fyROOH(i)=fyROOH(i)+  0.3200;fyRUOOH(i)=fyRUOOH(i)+  1.0000;fHPCRB(i)=fHPCRB(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.4500;fSumRO2(i)=fSumRO2(i)+  1.8500;fTRPRXN(i)=fTRPRXN(i)+  1.0000;

% 508, <528>
i=i+1;
Rnames{ 508} = 'BPINE_OH = 0.04000*OH +  0.12000*HO2 +  0.03000*PACID +  0.01000*xPACID +  0.11000*HPCRB +  0.01000*CO2 ';
k(:,i) = (  3.4500E+00 ); 
Gstr{i,   1}='BPINE_OH';
fBPINE_OH(i)=fBPINE_OH(i)-1.0;
fOH(i)=fOH(i)+  0.0400;fHO2(i)=fHO2(i)+  0.1200;fPACID(i)=fPACID(i)+  0.0300;fxPACID(i)=fxPACID(i)+  0.0100;fHPCRB(i)=fHPCRB(i)+  0.1100;fCO2(i)=fCO2(i)+  0.0100;

% 509, <529>
i=i+1;
Rnames{ 509} = 'BPINE_OH + NO = NO +  0.11000*xHO2 +  0.15000*RO2C +  0.04000*RO2XC +  0.02000*xHCHO +  0.01000*xRCHO +  0.08000*xOLEA2 +  0.01000*zRCNO3 +  0.03000*zRHNO3 +  0.01000*yRUOOH +  0.21000*yHPCRB +  0.19000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='BPINE_OH';Gstr{i,   2}='NO';
fBPINE_OH(i)=fBPINE_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.1100;fRO2C(i)=fRO2C(i)+  0.1500;fRO2XC(i)=fRO2XC(i)+  0.0400;fxHCHO(i)=fxHCHO(i)+  0.0200;fxRCHO(i)=fxRCHO(i)+  0.0100;fxOLEA2(i)=fxOLEA2(i)+  0.0800;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fzRHNO3(i)=fzRHNO3(i)+  0.0300;fyRUOOH(i)=fyRUOOH(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.2100;fSumRO2(i)=fSumRO2(i)+  0.1900;

% 510, <BPO3>
i=i+1;
Rnames{ 510} = 'BPINE + O3 = 0.39000*OH +  0.14000*HO2 +  0.23000*RO2C +  0.08000*RO2XC +  0.23000*xR2CO3 +  0.21000*HCHO2 +  0.20000*RCHO2 +  0.50000*HCHO +  0.50000*KET2 +  0.08000*zRCNO3 +  0.26000*yHPCRB +  0.12000*CO2 +  0.17000*CO +  0.31000*SumRO2 + TRPRXN ';
k(:,i) = (  1.3900E-15.*exp( -1.2800E+03./T) ); 
Gstr{i,   1}='BPINE';Gstr{i,   2}='O3';
fBPINE(i)=fBPINE(i)-1.0;fO3(i)=fO3(i)-1.0;
fOH(i)=fOH(i)+  0.3900;fHO2(i)=fHO2(i)+  0.1400;fRO2C(i)=fRO2C(i)+  0.2300;fRO2XC(i)=fRO2XC(i)+  0.0800;fxR2CO3(i)=fxR2CO3(i)+  0.2300;fHCHO2(i)=fHCHO2(i)+  0.2100;fRCHO2(i)=fRCHO2(i)+  0.2000;fHCHO(i)=fHCHO(i)+  0.5000;fKET2(i)=fKET2(i)+  0.5000;fzRCNO3(i)=fzRCNO3(i)+  0.0800;fyHPCRB(i)=fyHPCRB(i)+  0.2600;fCO2(i)=fCO2(i)+  0.1200;fCO(i)=fCO(i)+  0.1700;fSumRO2(i)=fSumRO2(i)+  0.3100;fTRPRXN(i)=fTRPRXN(i)+  1.0000;

% 511, <BPN3>
i=i+1;
Rnames{ 511} = 'BPINE + NO3 = 0.02000*OH +  0.04000*xOH +  0.18000*xHO2 +  2.25000*RO2C +  0.63000*RO2XC +  0.14000*xR2CO3 +  0.04000*xHCHO +  0.02000*xRCHO +  0.27000*xACET +  0.14000*xPACID +  0.02000*RCNO3 +  0.22000*xRCNO3 +  0.43000*zRCNO3 + yRPNO3 +  0.19000*zRDNO3 +  0.04000*CO2 +  2.88000*SumRO2 + TRPRXN ';
k(:,i) = (  2.5000E-12 ); 
Gstr{i,   1}='BPINE';Gstr{i,   2}='NO3';
fBPINE(i)=fBPINE(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fOH(i)=fOH(i)+  0.0200;fxOH(i)=fxOH(i)+  0.0400;fxHO2(i)=fxHO2(i)+  0.1800;fRO2C(i)=fRO2C(i)+  2.2500;fRO2XC(i)=fRO2XC(i)+  0.6300;fxR2CO3(i)=fxR2CO3(i)+  0.1400;fxHCHO(i)=fxHCHO(i)+  0.0400;fxRCHO(i)=fxRCHO(i)+  0.0200;fxACET(i)=fxACET(i)+  0.2700;fxPACID(i)=fxPACID(i)+  0.1400;fRCNO3(i)=fRCNO3(i)+  0.0200;fxRCNO3(i)=fxRCNO3(i)+  0.2200;fzRCNO3(i)=fzRCNO3(i)+  0.4300;fyRPNO3(i)=fyRPNO3(i)+  1.0000;fzRDNO3(i)=fzRDNO3(i)+  0.1900;fCO2(i)=fCO2(i)+  0.0400;fSumRO2(i)=fSumRO2(i)+  2.8800;fTRPRXN(i)=fTRPRXN(i)+  1.0000;

% 512, <BPOP>
i=i+1;
Rnames{ 512} = 'BPINE + O3P = 0.50000*RCHO +  0.50000*ALK5 + TRPRXN ';
k(:,i) = (  2.7000E-11 ); 
Gstr{i,   1}='BPINE';Gstr{i,   2}='O3P';
fBPINE(i)=fBPINE(i)-1.0;fO3P(i)=fO3P(i)-1.0;
fRCHO(i)=fRCHO(i)+  0.5000;fALK5(i)=fALK5(i)+  0.5000;fTRPRXN(i)=fTRPRXN(i)+  1.0000;

% 513, <ACOH>
i=i+1;
Rnames{ 513} = 'ACETL + OH = 0.67000*OH +  0.33000*HO2 +  0.67000*GLY +  0.33000*HCOOH +  0.33000*CO ';
xko =   5.5000E-30.*M.*exp(  0.0000E+00./T).*(T./300).^  0.0000E+00;
xkinf =   8.3000E-13.*exp(  0.0000E+00./T).*(T./300).^  2.0000E+00;
xn =   1.0000E+00;
F =   6.0000E-01;
G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);
k(:,i) = (xko./( 1.0+xko./xkinf).*F.^G ); 
Gstr{i,   1}='ACETL';Gstr{i,   2}='OH';
fACETL(i)=fACETL(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  0.6700;fHO2(i)=fHO2(i)+  0.3300;fGLY(i)=fGLY(i)+  0.6700;fHCOOH(i)=fHCOOH(i)+  0.3300;fCO(i)=fCO(i)+  0.3300;

% 514, <ACO3>
i=i+1;
Rnames{ 514} = 'ACETL + O3 = 0.26000*HO2 +  0.34000*RCHO2 +  0.34000*HCHO +  0.18000*HCOOH +  0.47000*CO2 +  0.31000*CO ';
k(:,i) = (  1.0000E-20 ); 
Gstr{i,   1}='ACETL';Gstr{i,   2}='O3';
fACETL(i)=fACETL(i)-1.0;fO3(i)=fO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.2600;fRCHO2(i)=fRCHO2(i)+  0.3400;fHCHO(i)=fHCHO(i)+  0.3400;fHCOOH(i)=fHCOOH(i)+  0.1800;fCO2(i)=fCO2(i)+  0.4700;fCO(i)=fCO(i)+  0.3100;

% 515, <BZOH>
i=i+1;
Rnames{ 515} = 'BENZ + OH = 0.69000*HO2 +  0.28000*xHO2 +  0.28000*RO2C +  0.04000*RO2XC +  0.12000*OLEA2 +  0.28000*xGLY +  0.28000*xBUDAL +  0.57000*PHEN +  0.04000*zRANO3 +  0.31000*yRAOOH +  0.32000*SumRO2 + BENZRO2 ';
k(:,i) = (  2.3000E-12.*exp( -1.9000E+02./T) ); 
Gstr{i,   1}='BENZ';Gstr{i,   2}='OH';
fBENZ(i)=fBENZ(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.6900;fxHO2(i)=fxHO2(i)+  0.2800;fRO2C(i)=fRO2C(i)+  0.2800;fRO2XC(i)=fRO2XC(i)+  0.0400;fOLEA2(i)=fOLEA2(i)+  0.1200;fxGLY(i)=fxGLY(i)+  0.2800;fxBUDAL(i)=fxBUDAL(i)+  0.2800;fPHEN(i)=fPHEN(i)+  0.5700;fzRANO3(i)=fzRANO3(i)+  0.0400;fyRAOOH(i)=fyRAOOH(i)+  0.3100;fSumRO2(i)=fSumRO2(i)+  0.3200;fBENZRO2(i)=fBENZRO2(i)+  1.0000;

% 516, <TLOH>
i=i+1;
Rnames{ 516} = 'TOLU + OH = 0.41000*HO2 +  0.50000*xHO2 +  0.50000*RO2C +  0.08000*RO2XC +  0.01000*OLEA1 +  0.21000*OLEA2 +  0.22000*xGLY +  0.22000*xMGLY +  0.22000*xBUDAL +  0.02000*xAFG1 +  0.19000*xAFG2A +  0.06000*xBALD +  0.19000*CRES +  0.01000*zR1NO3 +  0.07000*zRANO3 +  0.08000*yROOH +  0.51000*yRAOOH +  0.58000*SumRO2 + TOLRO2 ';
k(:,i) = (  1.8000E-12.*exp(  3.4000E+02./T) ); 
Gstr{i,   1}='TOLU';Gstr{i,   2}='OH';
fTOLU(i)=fTOLU(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.4100;fxHO2(i)=fxHO2(i)+  0.5000;fRO2C(i)=fRO2C(i)+  0.5000;fRO2XC(i)=fRO2XC(i)+  0.0800;fOLEA1(i)=fOLEA1(i)+  0.0100;fOLEA2(i)=fOLEA2(i)+  0.2100;fxGLY(i)=fxGLY(i)+  0.2200;fxMGLY(i)=fxMGLY(i)+  0.2200;fxBUDAL(i)=fxBUDAL(i)+  0.2200;fxAFG1(i)=fxAFG1(i)+  0.0200;fxAFG2A(i)=fxAFG2A(i)+  0.1900;fxBALD(i)=fxBALD(i)+  0.0600;fCRES(i)=fCRES(i)+  0.1900;fzR1NO3(i)=fzR1NO3(i)+  0.0100;fzRANO3(i)=fzRANO3(i)+  0.0700;fyROOH(i)=fyROOH(i)+  0.0800;fyRAOOH(i)=fyRAOOH(i)+  0.5100;fSumRO2(i)=fSumRO2(i)+  0.5800;fTOLRO2(i)=fTOLRO2(i)+  1.0000;

% 517, <OXOH>
i=i+1;
Rnames{ 517} = 'OXYL + OH = 0.35000*HO2 +  0.54000*xHO2 +  0.54000*RO2C +  0.11000*RO2XC +  0.10000*OLEA1 +  0.10000*OLEA2 +  0.12000*xGLY +  0.22000*xMGLY +  0.15000*xBACL +  0.15000*xBUDAL +  0.01000*xAFG1 +  0.22000*xAFG2A +  0.11000*xAFG2B +  0.06000*xBALD +  0.06000*LVKS +  0.08000*XYNL +  0.02000*zR2NO3 +  0.09000*zRANO3 +  0.08000*yROOH +  0.57000*yRAOOH +  0.65000*SumRO2 + XYLRO2 ';
k(:,i) = (  1.3600E-11 ); 
Gstr{i,   1}='OXYL';Gstr{i,   2}='OH';
fOXYL(i)=fOXYL(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.3500;fxHO2(i)=fxHO2(i)+  0.5400;fRO2C(i)=fRO2C(i)+  0.5400;fRO2XC(i)=fRO2XC(i)+  0.1100;fOLEA1(i)=fOLEA1(i)+  0.1000;fOLEA2(i)=fOLEA2(i)+  0.1000;fxGLY(i)=fxGLY(i)+  0.1200;fxMGLY(i)=fxMGLY(i)+  0.2200;fxBACL(i)=fxBACL(i)+  0.1500;fxBUDAL(i)=fxBUDAL(i)+  0.1500;fxAFG1(i)=fxAFG1(i)+  0.0100;fxAFG2A(i)=fxAFG2A(i)+  0.2200;fxAFG2B(i)=fxAFG2B(i)+  0.1100;fxBALD(i)=fxBALD(i)+  0.0600;fLVKS(i)=fLVKS(i)+  0.0600;fXYNL(i)=fXYNL(i)+  0.0800;fzR2NO3(i)=fzR2NO3(i)+  0.0200;fzRANO3(i)=fzRANO3(i)+  0.0900;fyROOH(i)=fyROOH(i)+  0.0800;fyRAOOH(i)=fyRAOOH(i)+  0.5700;fSumRO2(i)=fSumRO2(i)+  0.6500;fXYLRO2(i)=fXYLRO2(i)+  1.0000;

% 518, <MXOH>
i=i+1;
Rnames{ 518} = 'MXYL + OH = 0.21000*HO2 +  0.66000*xHO2 +  0.66000*RO2C +  0.13000*RO2XC +  0.01000*OLEA1 +  0.13000*OLEA2 +  0.04000*xGLY +  0.59000*xMGLY +  0.05000*xAFG1 +  0.58000*xAFG2A +  0.03000*xBALD +  0.07000*XYNL +  0.01000*zR2NO3 +  0.12000*zRANO3 +  0.03000*yROOH +  0.75000*yRAOOH +  0.79000*SumRO2 + XYLRO2 ';
k(:,i) = (  2.3100E-11 ); 
Gstr{i,   1}='MXYL';Gstr{i,   2}='OH';
fMXYL(i)=fMXYL(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.2100;fxHO2(i)=fxHO2(i)+  0.6600;fRO2C(i)=fRO2C(i)+  0.6600;fRO2XC(i)=fRO2XC(i)+  0.1300;fOLEA1(i)=fOLEA1(i)+  0.0100;fOLEA2(i)=fOLEA2(i)+  0.1300;fxGLY(i)=fxGLY(i)+  0.0400;fxMGLY(i)=fxMGLY(i)+  0.5900;fxAFG1(i)=fxAFG1(i)+  0.0500;fxAFG2A(i)=fxAFG2A(i)+  0.5800;fxBALD(i)=fxBALD(i)+  0.0300;fXYNL(i)=fXYNL(i)+  0.0700;fzR2NO3(i)=fzR2NO3(i)+  0.0100;fzRANO3(i)=fzRANO3(i)+  0.1200;fyROOH(i)=fyROOH(i)+  0.0300;fyRAOOH(i)=fyRAOOH(i)+  0.7500;fSumRO2(i)=fSumRO2(i)+  0.7900;fXYLRO2(i)=fXYLRO2(i)+  1.0000;

% 519, <PXOH>
i=i+1;
Rnames{ 519} = 'PXYL + OH = 0.38000*HO2 +  0.52000*xHO2 +  0.52000*RO2C +  0.11000*RO2XC +  0.02000*OLEA1 +  0.23000*OLEA2 +  0.16000*xGLY +  0.29000*xMGLY +  0.29000*xAFG1 +  0.07000*xBALD +  0.16000*xAFG3 +  0.14000*XYNL +  0.02000*zR2NO3 +  0.09000*zRANO3 +  0.09000*yROOH +  0.53000*yRAOOH +  0.63000*SumRO2 + XYLRO2 ';
k(:,i) = (  4.1400E-12.*exp(  3.1900E+02./T) ); 
Gstr{i,   1}='PXYL';Gstr{i,   2}='OH';
fPXYL(i)=fPXYL(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.3800;fxHO2(i)=fxHO2(i)+  0.5200;fRO2C(i)=fRO2C(i)+  0.5200;fRO2XC(i)=fRO2XC(i)+  0.1100;fOLEA1(i)=fOLEA1(i)+  0.0200;fOLEA2(i)=fOLEA2(i)+  0.2300;fxGLY(i)=fxGLY(i)+  0.1600;fxMGLY(i)=fxMGLY(i)+  0.2900;fxAFG1(i)=fxAFG1(i)+  0.2900;fxBALD(i)=fxBALD(i)+  0.0700;fxAFG3(i)=fxAFG3(i)+  0.1600;fXYNL(i)=fXYNL(i)+  0.1400;fzR2NO3(i)=fzR2NO3(i)+  0.0200;fzRANO3(i)=fzRANO3(i)+  0.0900;fyROOH(i)=fyROOH(i)+  0.0900;fyRAOOH(i)=fyRAOOH(i)+  0.5300;fSumRO2(i)=fSumRO2(i)+  0.6300;fXYLRO2(i)=fXYLRO2(i)+  1.0000;

% 520, <X1OH>
i=i+1;
Rnames{ 520} = 'BZ123 + OH = 0.18000*HO2 +  0.67000*xHO2 +  0.67000*RO2C +  0.15000*RO2XC +  0.03000*OLEA1 +  0.03000*OLEA2 +  0.03000*xGLY +  0.07000*xMGLY +  0.54000*xBACL +  0.54000*xAFG2A +  0.10000*xAFG2B +  0.02000*xBALD +  0.10000*LVKS +  0.02000*XYNL +  0.01000*zR2NO3 +  0.14000*zRANO3 +  0.03000*yROOH +  0.78000*yRAOOH +  0.82000*SumRO2 + XYLRO2 ';
k(:,i) = (  3.2700E-11 ); 
Gstr{i,   1}='BZ123';Gstr{i,   2}='OH';
fBZ123(i)=fBZ123(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.1800;fxHO2(i)=fxHO2(i)+  0.6700;fRO2C(i)=fRO2C(i)+  0.6700;fRO2XC(i)=fRO2XC(i)+  0.1500;fOLEA1(i)=fOLEA1(i)+  0.0300;fOLEA2(i)=fOLEA2(i)+  0.0300;fxGLY(i)=fxGLY(i)+  0.0300;fxMGLY(i)=fxMGLY(i)+  0.0700;fxBACL(i)=fxBACL(i)+  0.5400;fxAFG2A(i)=fxAFG2A(i)+  0.5400;fxAFG2B(i)=fxAFG2B(i)+  0.1000;fxBALD(i)=fxBALD(i)+  0.0200;fLVKS(i)=fLVKS(i)+  0.1000;fXYNL(i)=fXYNL(i)+  0.0200;fzR2NO3(i)=fzR2NO3(i)+  0.0100;fzRANO3(i)=fzRANO3(i)+  0.1400;fyROOH(i)=fyROOH(i)+  0.0300;fyRAOOH(i)=fyRAOOH(i)+  0.7800;fSumRO2(i)=fSumRO2(i)+  0.8200;fXYLRO2(i)=fXYLRO2(i)+  1.0000;

% 521, <X2OH>
i=i+1;
Rnames{ 521} = 'BZ124 + OH = 0.23000*HO2 +  0.63000*xHO2 +  0.63000*RO2C +  0.14000*RO2XC +  0.04000*OLEA1 +  0.11000*OLEA2 +  0.03000*xGLY +  0.51000*xMGLY +  0.06000*xBACL +  0.08000*xAFG1 +  0.04000*xAFG2A +  0.27000*xAFG2B +  0.03000*xBALD +  0.02000*LVKS +  0.21000*xAFG3 +  0.05000*XYNL +  0.01000*zR2NO3 +  0.13000*zRANO3 +  0.04000*yROOH +  0.73000*yRAOOH +  0.77000*SumRO2 + XYLRO2 ';
k(:,i) = (  3.2500E-11 ); 
Gstr{i,   1}='BZ124';Gstr{i,   2}='OH';
fBZ124(i)=fBZ124(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.2300;fxHO2(i)=fxHO2(i)+  0.6300;fRO2C(i)=fRO2C(i)+  0.6300;fRO2XC(i)=fRO2XC(i)+  0.1400;fOLEA1(i)=fOLEA1(i)+  0.0400;fOLEA2(i)=fOLEA2(i)+  0.1100;fxGLY(i)=fxGLY(i)+  0.0300;fxMGLY(i)=fxMGLY(i)+  0.5100;fxBACL(i)=fxBACL(i)+  0.0600;fxAFG1(i)=fxAFG1(i)+  0.0800;fxAFG2A(i)=fxAFG2A(i)+  0.0400;fxAFG2B(i)=fxAFG2B(i)+  0.2700;fxBALD(i)=fxBALD(i)+  0.0300;fLVKS(i)=fLVKS(i)+  0.0200;fxAFG3(i)=fxAFG3(i)+  0.2100;fXYNL(i)=fXYNL(i)+  0.0500;fzR2NO3(i)=fzR2NO3(i)+  0.0100;fzRANO3(i)=fzRANO3(i)+  0.1300;fyROOH(i)=fyROOH(i)+  0.0400;fyRAOOH(i)=fyRAOOH(i)+  0.7300;fSumRO2(i)=fSumRO2(i)+  0.7700;fXYLRO2(i)=fXYLRO2(i)+  1.0000;

% 522, <X3OH>
i=i+1;
Rnames{ 522} = 'BZ135 + OH = 0.17000*HO2 +  0.68000*xHO2 +  0.68000*RO2C +  0.15000*RO2XC +  0.11000*OLEA2 +  0.67000*xMGLY +  0.67000*xAFG2A +  0.02000*xBALD +  0.05000*XYNL +  0.01000*zR2NO3 +  0.15000*zRANO3 +  0.02000*yROOH +  0.81000*yRAOOH +  0.83000*SumRO2 + XYLRO2 ';
k(:,i) = (  5.8600E-11 ); 
Gstr{i,   1}='BZ135';Gstr{i,   2}='OH';
fBZ135(i)=fBZ135(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.1700;fxHO2(i)=fxHO2(i)+  0.6800;fRO2C(i)=fRO2C(i)+  0.6800;fRO2XC(i)=fRO2XC(i)+  0.1500;fOLEA2(i)=fOLEA2(i)+  0.1100;fxMGLY(i)=fxMGLY(i)+  0.6700;fxAFG2A(i)=fxAFG2A(i)+  0.6700;fxBALD(i)=fxBALD(i)+  0.0200;fXYNL(i)=fXYNL(i)+  0.0500;fzR2NO3(i)=fzR2NO3(i)+  0.0100;fzRANO3(i)=fzRANO3(i)+  0.1500;fyROOH(i)=fyROOH(i)+  0.0200;fyRAOOH(i)=fyRAOOH(i)+  0.8100;fSumRO2(i)=fSumRO2(i)+  0.8300;fXYLRO2(i)=fXYLRO2(i)+  1.0000;

% 523, <EBOH>
i=i+1;
Rnames{ 523} = 'C2BEN + OH = 0.36000*HO2 +  0.51000*xHO2 +  0.54000*RO2C +  0.12000*RO2XC +  0.01000*xMEO2 +  0.02000*xHCHO +  0.01000*OLEA1 +  0.18000*OLEA2 +  0.18000*xGLY +  0.18000*xMGLY +  0.18000*xBUDAL +  0.02000*xAFG1 +  0.16000*xAFG2A +  0.03000*xBALD +  0.16000*XYNL +  0.05000*zR2NO3 +  0.07000*zRANO3 +  0.23000*yROOH +  0.44000*yRAOOH +  0.13000*xBENX +  0.66000*SumRO2 ';
k(:,i) = (  7.0000E-12 ); 
Gstr{i,   1}='C2BEN';Gstr{i,   2}='OH';
fC2BEN(i)=fC2BEN(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.3600;fxHO2(i)=fxHO2(i)+  0.5100;fRO2C(i)=fRO2C(i)+  0.5400;fRO2XC(i)=fRO2XC(i)+  0.1200;fxMEO2(i)=fxMEO2(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.0200;fOLEA1(i)=fOLEA1(i)+  0.0100;fOLEA2(i)=fOLEA2(i)+  0.1800;fxGLY(i)=fxGLY(i)+  0.1800;fxMGLY(i)=fxMGLY(i)+  0.1800;fxBUDAL(i)=fxBUDAL(i)+  0.1800;fxAFG1(i)=fxAFG1(i)+  0.0200;fxAFG2A(i)=fxAFG2A(i)+  0.1600;fxBALD(i)=fxBALD(i)+  0.0300;fXYNL(i)=fXYNL(i)+  0.1600;fzR2NO3(i)=fzR2NO3(i)+  0.0500;fzRANO3(i)=fzRANO3(i)+  0.0700;fyROOH(i)=fyROOH(i)+  0.2300;fyRAOOH(i)=fyRAOOH(i)+  0.4400;fxBENX(i)=fxBENX(i)+  0.1300;fSumRO2(i)=fSumRO2(i)+  0.6600;

% 524, <MTOH>
i=i+1;
Rnames{ 524} = 'MTBE + OH = 0.72000*xHO2 +  1.12000*RO2C +  0.09000*RO2XC +  0.19000*xMEO2 +  0.20000*xHCHO +  0.09000*zR1NO3 +  0.89000*yROOH +  0.17000*ALK1 +  0.72000*ALK2 +  0.01000*ALK3 +  1.21000*SumRO2 ';
k(:,i) = (  1.8700E-13.*exp(  8.4300E+02./T).*(T./300).^(  3.3400E+00 ) ); 
Gstr{i,   1}='MTBE';Gstr{i,   2}='OH';
fMTBE(i)=fMTBE(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  0.7200;fRO2C(i)=fRO2C(i)+  1.1200;fRO2XC(i)=fRO2XC(i)+  0.0900;fxMEO2(i)=fxMEO2(i)+  0.1900;fxHCHO(i)=fxHCHO(i)+  0.2000;fzR1NO3(i)=fzR1NO3(i)+  0.0900;fyROOH(i)=fyROOH(i)+  0.8900;fALK1(i)=fALK1(i)+  0.1700;fALK2(i)=fALK2(i)+  0.7200;fALK3(i)=fALK3(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  1.2100;

% 525, <MLOH>
i=i+1;
Rnames{ 525} = 'MEOH + OH = HO2 + HCHO ';
k(:,i) = (  2.3200E-13.*exp(  4.0200E+02./T).*(T./300).^(  2.7200E+00 ) ); 
Gstr{i,   1}='MEOH';Gstr{i,   2}='OH';
fMEOH(i)=fMEOH(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fHCHO(i)=fHCHO(i)+  1.0000;

% 526, <FAOH>
i=i+1;
Rnames{ 526} = 'HCOOH + OH = HO2 + CO2 ';
k(:,i) = (  4.5000E-13 ); 
Gstr{i,   1}='HCOOH';Gstr{i,   2}='OH';
fHCOOH(i)=fHCOOH(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fCO2(i)=fCO2(i)+  1.0000;

% 527, <H1OH>
i=i+1;
Rnames{ 527} = 'MEOOH + OH = 0.03000*OH +  0.97000*MEO2 +  0.03000*HCHO +  0.97000*SumRO2 ';
k(:,i) = (  5.3000E-12.*exp(  1.9000E+02./T) ); 
Gstr{i,   1}='MEOOH';Gstr{i,   2}='OH';
fMEOOH(i)=fMEOOH(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  0.0300;fMEO2(i)=fMEO2(i)+  0.9700;fHCHO(i)=fHCHO(i)+  0.0300;fSumRO2(i)=fSumRO2(i)+  0.9700;

% 528, <H1HV>
i=i+1;
Rnames{ 528} = 'MEOOH = OH + HO2 + HCHO ';
k(:,i) = (JCOOH ); 
Gstr{i,   1}='MEOOH';
fMEOOH(i)=fMEOOH(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;fHCHO(i)=fHCHO(i)+  1.0000;

% 529, <A2OH>
i=i+1;
Rnames{ 529} = 'MECHO + OH = MECHO_OH +  0.04000*xHO2 +  0.05000*RO2C +  0.95000*MECO3 +  0.01000*xHCHO +  0.05000*SumRO2 +  0.95000*SumRCO3 ';
k(:,i) = (  2.4000E-12.*exp(  5.4600E+02./T).*(T./300).^(  7.7000E-01 ) ); 
Gstr{i,   1}='MECHO';Gstr{i,   2}='OH';
fMECHO(i)=fMECHO(i)-1.0;fOH(i)=fOH(i)-1.0;
fMECHO_OH(i)=fMECHO_OH(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0400;fRO2C(i)=fRO2C(i)+  0.0500;fMECO3(i)=fMECO3(i)+  0.9500;fxHCHO(i)=fxHCHO(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  0.0500;fSumRCO3(i)=fSumRCO3(i)+  0.9500;

% 530, <550>
i=i+1;
Rnames{ 530} = 'MECHO_OH = 0.01000*xOH +  0.04000*xPACID +  0.01000*CO2 ';
k(:,i) = (  3.7500E+00 ); 
Gstr{i,   1}='MECHO_OH';
fMECHO_OH(i)=fMECHO_OH(i)-1.0;
fxOH(i)=fxOH(i)+  0.0100;fxPACID(i)=fxPACID(i)+  0.0400;fCO2(i)=fCO2(i)+  0.0100;

% 531, <551>
i=i+1;
Rnames{ 531} = 'MECHO_OH + NO = NO +  0.01000*xHO2 +  0.04000*xHCHO +  0.04000*yHPCRB +  0.05000*CO ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='MECHO_OH';Gstr{i,   2}='NO';
fMECHO_OH(i)=fMECHO_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.0400;fyHPCRB(i)=fyHPCRB(i)+  0.0400;fCO(i)=fCO(i)+  0.0500;

% 532, <A2N3>
i=i+1;
Rnames{ 532} = 'MECHO + NO3 = HNO3 + MECO3 + SumRCO3 ';
k(:,i) = (  1.4000E-12.*exp( -1.8600E+03./T) ); 
Gstr{i,   1}='MECHO';Gstr{i,   2}='NO3';
fMECHO(i)=fMECHO(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;fMECO3(i)=fMECO3(i)+  1.0000;fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 533, <A2HV>
i=i+1;
Rnames{ 533} = 'MECHO = HO2 +  0.90000*MEO2 +  0.10000*MECO3 +  0.90000*CO +  0.90000*SumRO2 +  0.10000*SumRCO3 ';
k(:,i) = (JCCHOR_13 ); 
Gstr{i,   1}='MECHO';
fMECHO(i)=fMECHO(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fMEO2(i)=fMEO2(i)+  0.9000;fMECO3(i)=fMECO3(i)+  0.1000;fCO(i)=fCO(i)+  0.9000;fSumRO2(i)=fSumRO2(i)+  0.9000;fSumRCO3(i)=fSumRCO3(i)+  0.1000;

% 534, <EAOH>
i=i+1;
Rnames{ 534} = 'ETOH + OH = 0.95000*HO2 +  0.05000*xHO2 +  0.05000*RO2C +  0.07000*xHCHO +  0.95000*MECHO +  0.01000*xGLCHO +  0.05000*yROOH +  0.05000*SumRO2 ';
k(:,i) = (  4.4200E-13.*exp(  6.0600E+02./T).*(T./300).^(  2.2900E+00 ) ); 
Gstr{i,   1}='ETOH';Gstr{i,   2}='OH';
fETOH(i)=fETOH(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.9500;fxHO2(i)=fxHO2(i)+  0.0500;fRO2C(i)=fRO2C(i)+  0.0500;fxHCHO(i)=fxHCHO(i)+  0.0700;fMECHO(i)=fMECHO(i)+  0.9500;fxGLCHO(i)=fxGLCHO(i)+  0.0100;fyROOH(i)=fyROOH(i)+  0.0500;fSumRO2(i)=fSumRO2(i)+  0.0500;

% 535, <GAOH>
i=i+1;
Rnames{ 535} = 'GLCHO + OH = 0.20000*HO2 +  0.80000*R2CO3 +  0.20000*GLY +  0.80000*SumRCO3 ';
k(:,i) = (  1.1000E-11 ); 
Gstr{i,   1}='GLCHO';Gstr{i,   2}='OH';
fGLCHO(i)=fGLCHO(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.2000;fR2CO3(i)=fR2CO3(i)+  0.8000;fGLY(i)=fGLY(i)+  0.2000;fSumRCO3(i)=fSumRCO3(i)+  0.8000;

% 536, <GAN3>
i=i+1;
Rnames{ 536} = 'GLCHO + NO3 = HNO3 +  0.10000*HO2 +  0.90000*R2CO3 +  0.10000*GLY +  0.90000*SumRCO3 ';
k(:,i) = (  1.8400E-14 ); 
Gstr{i,   1}='GLCHO';Gstr{i,   2}='NO3';
fGLCHO(i)=fGLCHO(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;fHO2(i)=fHO2(i)+  0.1000;fR2CO3(i)=fR2CO3(i)+  0.9000;fGLY(i)=fGLY(i)+  0.1000;fSumRCO3(i)=fSumRCO3(i)+  0.9000;

% 537, <GAHV>
i=i+1;
Rnames{ 537} = 'GLCHO = GLCHO_HV +  0.07000*OH +  1.66000*HO2 +  0.05000*xHO2 +  0.07000*RO2C +  0.83000*HCHO +  0.02000*xHCHO +  0.10000*MEOH +  0.93000*CO +  0.07000*SumRO2 ';
k(:,i) = (JGLALD_14 ); 
Gstr{i,   1}='GLCHO';
fGLCHO(i)=fGLCHO(i)-1.0;
fGLCHO_HV(i)=fGLCHO_HV(i)+  1.0000;fOH(i)=fOH(i)+  0.0700;fHO2(i)=fHO2(i)+  1.6600;fxHO2(i)=fxHO2(i)+  0.0500;fRO2C(i)=fRO2C(i)+  0.0700;fHCHO(i)=fHCHO(i)+  0.8300;fxHCHO(i)=fxHCHO(i)+  0.0200;fMEOH(i)=fMEOH(i)+  0.1000;fCO(i)=fCO(i)+  0.9300;fSumRO2(i)=fSumRO2(i)+  0.0700;

% 538, <558>
i=i+1;
Rnames{ 538} = 'GLCHO_HV = 0.02000*xOH +  0.05000*xPACID +  0.02000*CO2 ';
k(:,i) = (  2.6200E+00 ); 
Gstr{i,   1}='GLCHO_HV';
fGLCHO_HV(i)=fGLCHO_HV(i)-1.0;
fxOH(i)=fxOH(i)+  0.0200;fxPACID(i)=fxPACID(i)+  0.0500;fCO2(i)=fCO2(i)+  0.0200;

% 539, <559>
i=i+1;
Rnames{ 539} = 'GLCHO_HV + NO = NO +  0.02000*xHO2 +  0.04000*xHCHO +  0.06000*yHPCRB +  0.06000*CO ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='GLCHO_HV';Gstr{i,   2}='NO';
fGLCHO_HV(i)=fGLCHO_HV(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0200;fxHCHO(i)=fxHCHO(i)+  0.0400;fyHPCRB(i)=fyHPCRB(i)+  0.0600;fCO(i)=fCO(i)+  0.0600;

% 540, <A3OH>
i=i+1;
Rnames{ 540} = 'ETCHO + OH = 0.04000*xHO2 +  0.04000*RO2C +  0.96000*R2CO3 +  0.04000*xMECHO +  0.01000*xPACID +  0.03000*yHPCRB +  0.04000*CO +  0.04000*SumRO2 +  0.96000*SumRCO3 ';
k(:,i) = (  6.6300E-13.*exp(  1.0180E+03./T).*(T./300).^(  1.9900E+00 ) ); 
Gstr{i,   1}='ETCHO';Gstr{i,   2}='OH';
fETCHO(i)=fETCHO(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  0.0400;fRO2C(i)=fRO2C(i)+  0.0400;fR2CO3(i)=fR2CO3(i)+  0.9600;fxMECHO(i)=fxMECHO(i)+  0.0400;fxPACID(i)=fxPACID(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.0300;fCO(i)=fCO(i)+  0.0400;fSumRO2(i)=fSumRO2(i)+  0.0400;fSumRCO3(i)=fSumRCO3(i)+  0.9600;

% 541, <A3N3>
i=i+1;
Rnames{ 541} = 'ETCHO + NO3 = HNO3 + R2CO3 + SumRCO3 ';
k(:,i) = (  6.3000E-15 ); 
Gstr{i,   1}='ETCHO';Gstr{i,   2}='NO3';
fETCHO(i)=fETCHO(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;fR2CO3(i)=fR2CO3(i)+  1.0000;fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 542, <A3HV>
i=i+1;
Rnames{ 542} = 'ETCHO = HO2 + ETO2 + CO + SumRO2 ';
k(:,i) = (JC2CHOabs ); 
Gstr{i,   1}='ETCHO';
fETCHO(i)=fETCHO(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fETO2(i)=fETO2(i)+  1.0000;fCO(i)=fCO(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 543, <AROH>
i=i+1;
Rnames{ 543} = 'ACRO + OH = ACRO_OH +  0.31000*xHO2 +  0.31000*RO2C +  0.01000*RO2XC +  0.68000*MACO3 +  0.07000*xHCHO +  0.01000*xGLY +  0.24000*xGLCHO +  0.01000*zRHNO3 +  0.22000*yHPCRB +  0.24000*CO +  0.32000*SumRO2 +  0.68000*SumRCO3 ';
k(:,i) = (  7.1000E-12.*exp(  3.3300E+02./T) ); 
Gstr{i,   1}='ACRO';Gstr{i,   2}='OH';
fACRO(i)=fACRO(i)-1.0;fOH(i)=fOH(i)-1.0;
fACRO_OH(i)=fACRO_OH(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.3100;fRO2C(i)=fRO2C(i)+  0.3100;fRO2XC(i)=fRO2XC(i)+  0.0100;fMACO3(i)=fMACO3(i)+  0.6800;fxHCHO(i)=fxHCHO(i)+  0.0700;fxGLY(i)=fxGLY(i)+  0.0100;fxGLCHO(i)=fxGLCHO(i)+  0.2400;fzRHNO3(i)=fzRHNO3(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.2200;fCO(i)=fCO(i)+  0.2400;fSumRO2(i)=fSumRO2(i)+  0.3200;fSumRCO3(i)=fSumRCO3(i)+  0.6800;

% 544, <564>
i=i+1;
Rnames{ 544} = 'ACRO_OH = 0.06000*xPACID ';
k(:,i) = (  1.6900E+00 ); 
Gstr{i,   1}='ACRO_OH';
fACRO_OH(i)=fACRO_OH(i)-1.0;
fxPACID(i)=fxPACID(i)+  0.0600;

% 545, <565>
i=i+1;
Rnames{ 545} = 'ACRO_OH + NO = NO +  0.06000*xGLY +  0.06000*yHPCRB ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='ACRO_OH';Gstr{i,   2}='NO';
fACRO_OH(i)=fACRO_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxGLY(i)=fxGLY(i)+  0.0600;fyHPCRB(i)=fyHPCRB(i)+  0.0600;

% 546, <ARO3>
i=i+1;
Rnames{ 546} = 'ACRO + O3 = 0.15000*OH +  0.27000*HO2 +  0.38000*HCHO2 +  0.03000*RCHO2 +  0.13000*HCHO +  0.90000*GLY +  0.02000*HCOOH +  0.26000*CO2 +  0.34000*CO ';
k(:,i) = (  2.8000E-19 ); 
Gstr{i,   1}='ACRO';Gstr{i,   2}='O3';
fACRO(i)=fACRO(i)-1.0;fO3(i)=fO3(i)-1.0;
fOH(i)=fOH(i)+  0.1500;fHO2(i)=fHO2(i)+  0.2700;fHCHO2(i)=fHCHO2(i)+  0.3800;fRCHO2(i)=fRCHO2(i)+  0.0300;fHCHO(i)=fHCHO(i)+  0.1300;fGLY(i)=fGLY(i)+  0.9000;fHCOOH(i)=fHCOOH(i)+  0.0200;fCO2(i)=fCO2(i)+  0.2600;fCO(i)=fCO(i)+  0.3400;

% 547, <ARN3>
i=i+1;
Rnames{ 547} = 'ACRO + NO3 = 0.94000*HNO3 +  0.06000*xHO2 +  0.06000*RO2C +  0.94000*MACO3 +  0.06000*xRCNO3 +  0.05000*yRPNO3 +  0.06000*CO +  0.06000*SumRO2 +  0.94000*SumRCO3 ';
k(:,i) = (  1.1000E-15 ); 
Gstr{i,   1}='ACRO';Gstr{i,   2}='NO3';
fACRO(i)=fACRO(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHNO3(i)=fHNO3(i)+  0.9400;fxHO2(i)=fxHO2(i)+  0.0600;fRO2C(i)=fRO2C(i)+  0.0600;fMACO3(i)=fMACO3(i)+  0.9400;fxRCNO3(i)=fxRCNO3(i)+  0.0600;fyRPNO3(i)=fyRPNO3(i)+  0.0500;fCO(i)=fCO(i)+  0.0600;fSumRO2(i)=fSumRO2(i)+  0.0600;fSumRCO3(i)=fSumRCO3(i)+  0.9400;

% 548, <ARHV>
i=i+1;
Rnames{ 548} = 'ACRO = ACRO_HV +  0.22000*OH +  0.49000*HO2 +  0.17000*xHO2 +  0.22000*RO2C +  0.05000*MEO2 +  0.15000*MACO3 +  0.15000*HCHO +  0.06000*xHCHO +  0.06000*MEOH +  0.25000*ETHEN +  0.16000*CO2 +  1.06000*CO +  0.27000*SumRO2 +  0.15000*SumRCO3 ';
k(:,i) = (JACROL_16 ); 
Gstr{i,   1}='ACRO';
fACRO(i)=fACRO(i)-1.0;
fACRO_HV(i)=fACRO_HV(i)+  1.0000;fOH(i)=fOH(i)+  0.2200;fHO2(i)=fHO2(i)+  0.4900;fxHO2(i)=fxHO2(i)+  0.1700;fRO2C(i)=fRO2C(i)+  0.2200;fMEO2(i)=fMEO2(i)+  0.0500;fMACO3(i)=fMACO3(i)+  0.1500;fHCHO(i)=fHCHO(i)+  0.1500;fxHCHO(i)=fxHCHO(i)+  0.0600;fMEOH(i)=fMEOH(i)+  0.0600;fETHEN(i)=fETHEN(i)+  0.2500;fCO2(i)=fCO2(i)+  0.1600;fCO(i)=fCO(i)+  1.0600;fSumRO2(i)=fSumRO2(i)+  0.2700;fSumRCO3(i)=fSumRCO3(i)+  0.1500;

% 549, <569>
i=i+1;
Rnames{ 549} = 'ACRO_HV = 0.06000*xOH +  0.17000*xPACID +  0.06000*CO2 ';
k(:,i) = (  2.4000E+00 ); 
Gstr{i,   1}='ACRO_HV';
fACRO_HV(i)=fACRO_HV(i)-1.0;
fxOH(i)=fxOH(i)+  0.0600;fxPACID(i)=fxPACID(i)+  0.1700;fCO2(i)=fCO2(i)+  0.0600;

% 550, <570>
i=i+1;
Rnames{ 550} = 'ACRO_HV + NO = NO +  0.05000*xHO2 +  0.15000*xHCHO +  0.19000*yHPCRB +  0.21000*CO ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='ACRO_HV';Gstr{i,   2}='NO';
fACRO_HV(i)=fACRO_HV(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0500;fxHCHO(i)=fxHCHO(i)+  0.1500;fyHPCRB(i)=fyHPCRB(i)+  0.1900;fCO(i)=fCO(i)+  0.2100;

% 551, <K3OH>
i=i+1;
Rnames{ 551} = 'ACET + OH = 0.96000*RO2C +  0.04000*RO2XC +  0.96000*xMECO3 +  0.96000*xHCHO +  0.04000*zRCNO3 +  0.85000*yHPCRB + SumRO2 ';
k(:,i) = (  1.9700E-14.*exp(  6.7800E+02./T).*(T./300).^(  3.8800E+00 ) ); 
Gstr{i,   1}='ACET';Gstr{i,   2}='OH';
fACET(i)=fACET(i)-1.0;fOH(i)=fOH(i)-1.0;
fRO2C(i)=fRO2C(i)+  0.9600;fRO2XC(i)=fRO2XC(i)+  0.0400;fxMECO3(i)=fxMECO3(i)+  0.9600;fxHCHO(i)=fxHCHO(i)+  0.9600;fzRCNO3(i)=fzRCNO3(i)+  0.0400;fyHPCRB(i)=fyHPCRB(i)+  0.8500;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 552, <K3HV>
i=i+1;
Rnames{ 552} = 'ACET = MEO2 + MECO3 + SumRO2 + SumRCO3 ';
k(:,i) = (JACET_06 ); 
Gstr{i,   1}='ACET';
fACET(i)=fACET(i)-1.0;
fMEO2(i)=fMEO2(i)+  1.0000;fMECO3(i)=fMECO3(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 553, <K4OH>
i=i+1;
Rnames{ 553} = 'MEK + OH = 0.29000*xHO2 +  0.94000*RO2C +  0.07000*RO2XC +  0.55000*xMECO3 +  0.08000*xR2CO3 +  0.11000*xHCHO +  0.54000*xMECHO +  0.29000*xRCHO +  0.07000*zRCNO3 +  0.91000*yHPCRB +  1.01000*SumRO2 ';
k(:,i) = (  5.4200E-14.*exp(  8.8900E+02./T).*(T./300).^(  3.5700E+00 ) ); 
Gstr{i,   1}='MEK';Gstr{i,   2}='OH';
fMEK(i)=fMEK(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  0.2900;fRO2C(i)=fRO2C(i)+  0.9400;fRO2XC(i)=fRO2XC(i)+  0.0700;fxMECO3(i)=fxMECO3(i)+  0.5500;fxR2CO3(i)=fxR2CO3(i)+  0.0800;fxHCHO(i)=fxHCHO(i)+  0.1100;fxMECHO(i)=fxMECHO(i)+  0.5400;fxRCHO(i)=fxRCHO(i)+  0.2900;fzRCNO3(i)=fzRCNO3(i)+  0.0700;fyHPCRB(i)=fyHPCRB(i)+  0.9100;fSumRO2(i)=fSumRO2(i)+  1.0100;

% 554, <K4HV>
i=i+1;
Rnames{ 554} = 'MEK = 0.15000*MEO2 +  0.85000*ETO2 +  0.85000*MECO3 +  0.15000*R2CO3 + SumRO2 + SumRCO3 ';
k(:,i) = (  1.7500E-01.*JMEK_06 ); 
Gstr{i,   1}='MEK';
fMEK(i)=fMEK(i)-1.0;
fMEO2(i)=fMEO2(i)+  0.1500;fETO2(i)=fETO2(i)+  0.8500;fMECO3(i)=fMECO3(i)+  0.8500;fR2CO3(i)=fR2CO3(i)+  0.1500;fSumRO2(i)=fSumRO2(i)+  1.0000;fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 555, <MAOH>
i=i+1;
Rnames{ 555} = 'MACR + OH = MACR_OH +  0.05000*xHO2 +  0.75000*RO2C +  0.04000*RO2XC +  0.21000*MACO3 +  0.05000*xHCHO +  0.61000*xKET2 +  0.79000*SumRO2 +  0.21000*SumRCO3 ';
k(:,i) = (  8.0000E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='MACR';Gstr{i,   2}='OH';
fMACR(i)=fMACR(i)-1.0;fOH(i)=fOH(i)-1.0;
fMACR_OH(i)=fMACR_OH(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0500;fRO2C(i)=fRO2C(i)+  0.7500;fRO2XC(i)=fRO2XC(i)+  0.0400;fMACO3(i)=fMACO3(i)+  0.2100;fxHCHO(i)=fxHCHO(i)+  0.0500;fxKET2(i)=fxKET2(i)+  0.6100;fSumRO2(i)=fSumRO2(i)+  0.7900;fSumRCO3(i)=fSumRCO3(i)+  0.2100;

% 556, <576>
i=i+1;
Rnames{ 556} = 'MACR_OH = 0.69000*xOH +  0.08000*xKET2 +  0.05000*xPACID +  0.04000*zRCNO3 +  0.69000*CO2 ';
k(:,i) = (  5.2600E-01 ); 
Gstr{i,   1}='MACR_OH';
fMACR_OH(i)=fMACR_OH(i)-1.0;
fxOH(i)=fxOH(i)+  0.6900;fxKET2(i)=fxKET2(i)+  0.0800;fxPACID(i)=fxPACID(i)+  0.0500;fzRCNO3(i)=fzRCNO3(i)+  0.0400;fCO2(i)=fCO2(i)+  0.6900;

% 557, <577>
i=i+1;
Rnames{ 557} = 'MACR_OH + NO = NO +  0.69000*xHO2 +  0.09000*xHCHO +  0.14000*xMGLY +  0.04000*zRHNO3 +  0.68000*yHPCRB +  0.61000*CO ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='MACR_OH';Gstr{i,   2}='NO';
fMACR_OH(i)=fMACR_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.6900;fxHCHO(i)=fxHCHO(i)+  0.0900;fxMGLY(i)=fxMGLY(i)+  0.1400;fzRHNO3(i)=fzRHNO3(i)+  0.0400;fyHPCRB(i)=fyHPCRB(i)+  0.6800;fCO(i)=fCO(i)+  0.6100;

% 558, <MAO3>
i=i+1;
Rnames{ 558} = 'MACR + O3 = 0.19000*OH +  0.01000*xOH +  0.25000*HO2 +  0.03000*xHO2 +  0.04000*RO2C +  0.01000*MECO3 +  0.38000*HCHO2 +  0.03000*RCHO2 +  0.10000*HCHO +  0.02000*xHCHO +  0.02000*MECHO +  0.90000*MGLY +  0.01000*OACID +  0.02000*xPACID +  0.01000*yHPCRB +  0.24000*CO2 +  0.38000*CO +  0.04000*SumRO2 +  0.01000*SumRCO3 ';
k(:,i) = (  1.4000E-15.*exp( -2.1000E+03./T) ); 
Gstr{i,   1}='MACR';Gstr{i,   2}='O3';
fMACR(i)=fMACR(i)-1.0;fO3(i)=fO3(i)-1.0;
fOH(i)=fOH(i)+  0.1900;fxOH(i)=fxOH(i)+  0.0100;fHO2(i)=fHO2(i)+  0.2500;fxHO2(i)=fxHO2(i)+  0.0300;fRO2C(i)=fRO2C(i)+  0.0400;fMECO3(i)=fMECO3(i)+  0.0100;fHCHO2(i)=fHCHO2(i)+  0.3800;fRCHO2(i)=fRCHO2(i)+  0.0300;fHCHO(i)=fHCHO(i)+  0.1000;fxHCHO(i)=fxHCHO(i)+  0.0200;fMECHO(i)=fMECHO(i)+  0.0200;fMGLY(i)=fMGLY(i)+  0.9000;fOACID(i)=fOACID(i)+  0.0100;fxPACID(i)=fxPACID(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.0100;fCO2(i)=fCO2(i)+  0.2400;fCO(i)=fCO(i)+  0.3800;fSumRO2(i)=fSumRO2(i)+  0.0400;fSumRCO3(i)=fSumRCO3(i)+  0.0100;

% 559, <MAN3>
i=i+1;
Rnames{ 559} = 'MACR + NO3 = MACR_N3 +  0.30000*HNO3 +  0.66000*RO2C +  0.04000*RO2XC +  0.30000*MACO3 +  0.66000*xRCNO3 +  0.70000*SumRO2 +  0.30000*SumRCO3 ';
k(:,i) = (  3.4000E-15 ); 
Gstr{i,   1}='MACR';Gstr{i,   2}='NO3';
fMACR(i)=fMACR(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fMACR_N3(i)=fMACR_N3(i)+  1.0000;fHNO3(i)=fHNO3(i)+  0.3000;fRO2C(i)=fRO2C(i)+  0.6600;fRO2XC(i)=fRO2XC(i)+  0.0400;fMACO3(i)=fMACO3(i)+  0.3000;fxRCNO3(i)=fxRCNO3(i)+  0.6600;fSumRO2(i)=fSumRO2(i)+  0.7000;fSumRCO3(i)=fSumRCO3(i)+  0.3000;

% 560, <580>
i=i+1;
Rnames{ 560} = 'MACR_N3 = 0.66000*xOH +  0.04000*zRCNO3 +  0.66000*CO2 ';
k(:,i) = (  5.2400E-01 ); 
Gstr{i,   1}='MACR_N3';
fMACR_N3(i)=fMACR_N3(i)-1.0;
fxOH(i)=fxOH(i)+  0.6600;fzRCNO3(i)=fzRCNO3(i)+  0.0400;fCO2(i)=fCO2(i)+  0.6600;

% 561, <581>
i=i+1;
Rnames{ 561} = 'MACR_N3 + NO = NO +  0.66000*xHO2 +  0.59000*yRPNO3 +  0.04000*zRDNO3 +  0.66000*CO ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='MACR_N3';Gstr{i,   2}='NO';
fMACR_N3(i)=fMACR_N3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.6600;fyRPNO3(i)=fyRPNO3(i)+  0.5900;fzRDNO3(i)=fzRDNO3(i)+  0.0400;fCO(i)=fCO(i)+  0.6600;

% 562, <MAHV>
i=i+1;
Rnames{ 562} = 'MACR = 0.45000*OH +  0.30000*HO2 +  0.43000*RO2C +  0.02000*RO2XC +  0.15000*MEO2 +  0.43000*xMECO3 +  0.15000*MACO3 +  0.15000*HCHO +  0.43000*xHCHO +  0.02000*zRCNO3 +  0.38000*yHPCRB +  0.25000*PROPE + CO +  0.60000*SumRO2 +  0.15000*SumRCO3 ';
k(:,i) = (JMACR_06 ); 
Gstr{i,   1}='MACR';
fMACR(i)=fMACR(i)-1.0;
fOH(i)=fOH(i)+  0.4500;fHO2(i)=fHO2(i)+  0.3000;fRO2C(i)=fRO2C(i)+  0.4300;fRO2XC(i)=fRO2XC(i)+  0.0200;fMEO2(i)=fMEO2(i)+  0.1500;fxMECO3(i)=fxMECO3(i)+  0.4300;fMACO3(i)=fMACO3(i)+  0.1500;fHCHO(i)=fHCHO(i)+  0.1500;fxHCHO(i)=fxHCHO(i)+  0.4300;fzRCNO3(i)=fzRCNO3(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.3800;fPROPE(i)=fPROPE(i)+  0.2500;fCO(i)=fCO(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  0.6000;fSumRCO3(i)=fSumRCO3(i)+  0.1500;

% 563, <MVOH>
i=i+1;
Rnames{ 563} = 'MVK + OH = 0.28000*xHO2 +  0.95000*RO2C +  0.05000*RO2XC +  0.66000*xMECO3 +  0.28000*xHCHO +  0.66000*xGLCHO +  0.28000*xMGLY +  0.05000*zRCNO3 +  0.90000*yHPCRB + SumRO2 ';
k(:,i) = (  2.6000E-12.*exp(  6.1000E+02./T) ); 
Gstr{i,   1}='MVK';Gstr{i,   2}='OH';
fMVK(i)=fMVK(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  0.2800;fRO2C(i)=fRO2C(i)+  0.9500;fRO2XC(i)=fRO2XC(i)+  0.0500;fxMECO3(i)=fxMECO3(i)+  0.6600;fxHCHO(i)=fxHCHO(i)+  0.2800;fxGLCHO(i)=fxGLCHO(i)+  0.6600;fxMGLY(i)=fxMGLY(i)+  0.2800;fzRCNO3(i)=fzRCNO3(i)+  0.0500;fyHPCRB(i)=fyHPCRB(i)+  0.9000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 564, <MVO3>
i=i+1;
Rnames{ 564} = 'MVK + O3 = 0.18000*OH +  0.26000*HO2 +  0.02000*xHO2 +  0.02000*RO2C +  0.40000*HCHO2 +  0.01000*RCHO2 +  0.05000*HCHO +  0.01000*xHCHO +  0.01000*MECHO +  0.95000*MGLY +  0.01000*xPACID +  0.01000*yHPCRB +  0.23000*CO2 +  0.36000*CO +  0.02000*SumRO2 ';
k(:,i) = (  8.5000E-16.*exp( -1.5200E+03./T) ); 
Gstr{i,   1}='MVK';Gstr{i,   2}='O3';
fMVK(i)=fMVK(i)-1.0;fO3(i)=fO3(i)-1.0;
fOH(i)=fOH(i)+  0.1800;fHO2(i)=fHO2(i)+  0.2600;fxHO2(i)=fxHO2(i)+  0.0200;fRO2C(i)=fRO2C(i)+  0.0200;fHCHO2(i)=fHCHO2(i)+  0.4000;fRCHO2(i)=fRCHO2(i)+  0.0100;fHCHO(i)=fHCHO(i)+  0.0500;fxHCHO(i)=fxHCHO(i)+  0.0100;fMECHO(i)=fMECHO(i)+  0.0100;fMGLY(i)=fMGLY(i)+  0.9500;fxPACID(i)=fxPACID(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.0100;fCO2(i)=fCO2(i)+  0.2300;fCO(i)=fCO(i)+  0.3600;fSumRO2(i)=fSumRO2(i)+  0.0200;

% 565, <MVHV>
i=i+1;
Rnames{ 565} = 'MVK = 0.40000*MEO2 +  0.40000*MACO3 +  0.60000*PROPE +  0.60000*CO +  0.40000*SumRO2 +  0.40000*SumRCO3 ';
k(:,i) = (JMVK_16 ); 
Gstr{i,   1}='MVK';
fMVK(i)=fMVK(i)-1.0;
fMEO2(i)=fMEO2(i)+  0.4000;fMACO3(i)=fMACO3(i)+  0.4000;fPROPE(i)=fPROPE(i)+  0.6000;fCO(i)=fCO(i)+  0.6000;fSumRO2(i)=fSumRO2(i)+  0.4000;fSumRCO3(i)=fSumRCO3(i)+  0.4000;

% 566, <F1OH>
i=i+1;
Rnames{ 566} = 'BUDAL + OH = BUDAL_OH +  0.54000*OH +  0.44000*xHO2 +  0.44000*RO2C +  0.02000*RO2XC +  0.41000*xGLY +  0.54000*MALAH +  0.05000*xPACID +  0.02000*CO +  0.46000*SumRO2 ';
k(:,i) = (  5.2900E-11 ); 
Gstr{i,   1}='BUDAL';Gstr{i,   2}='OH';
fBUDAL(i)=fBUDAL(i)-1.0;fOH(i)=fOH(i)-1.0;
fBUDAL_OH(i)=fBUDAL_OH(i)+  1.0000;fOH(i)=fOH(i)+  0.5400;fxHO2(i)=fxHO2(i)+  0.4400;fRO2C(i)=fRO2C(i)+  0.4400;fRO2XC(i)=fRO2XC(i)+  0.0200;fxGLY(i)=fxGLY(i)+  0.4100;fMALAH(i)=fMALAH(i)+  0.5400;fxPACID(i)=fxPACID(i)+  0.0500;fCO(i)=fCO(i)+  0.0200;fSumRO2(i)=fSumRO2(i)+  0.4600;

% 567, <587>
i=i+1;
Rnames{ 567} = 'BUDAL_OH = 0.39000*xPACID +  0.02000*zRCNO3 ';
k(:,i) = (  2.0200E+01 ); 
Gstr{i,   1}='BUDAL_OH';
fBUDAL_OH(i)=fBUDAL_OH(i)-1.0;
fxPACID(i)=fxPACID(i)+  0.3900;fzRCNO3(i)=fzRCNO3(i)+  0.0200;

% 568, <588>
i=i+1;
Rnames{ 568} = 'BUDAL_OH + NO = NO +  0.37000*xGLY +  0.35000*yHPCRB ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='BUDAL_OH';Gstr{i,   2}='NO';
fBUDAL_OH(i)=fBUDAL_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxGLY(i)=fxGLY(i)+  0.3700;fyHPCRB(i)=fyHPCRB(i)+  0.3500;

% 569, <F1HV>
i=i+1;
Rnames{ 569} = 'BUDAL = OH + HO2 + MALAH ';
k(:,i) = (  2.5000E-01.*JAFGS ); 
Gstr{i,   1}='BUDAL';
fBUDAL(i)=fBUDAL(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fHO2(i)=fHO2(i)+  1.0000;fMALAH(i)=fMALAH(i)+  1.0000;

% 570, <PHOH>
i=i+1;
Rnames{ 570} = 'PHEN + OH = 0.85000*HO2 +  0.07000*xHO2 +  0.07000*RO2C +  0.01000*RO2XC +  0.07000*BZO +  0.01000*OLEA1 +  0.04000*OLEA2 +  0.03000*xGLY +  0.04000*xMGLY +  0.04000*xBUDAL +  0.01000*xAFG1 +  0.02000*xAFG2A +  0.03000*OLEP +  0.77000*CATL +  0.01000*zRANO3 +  0.08000*yRAOOH +  0.08000*SumRO2 ';
k(:,i) = (  4.7000E-13.*exp(  1.2200E+03./T) ); 
Gstr{i,   1}='PHEN';Gstr{i,   2}='OH';
fPHEN(i)=fPHEN(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.8500;fxHO2(i)=fxHO2(i)+  0.0700;fRO2C(i)=fRO2C(i)+  0.0700;fRO2XC(i)=fRO2XC(i)+  0.0100;fBZO(i)=fBZO(i)+  0.0700;fOLEA1(i)=fOLEA1(i)+  0.0100;fOLEA2(i)=fOLEA2(i)+  0.0400;fxGLY(i)=fxGLY(i)+  0.0300;fxMGLY(i)=fxMGLY(i)+  0.0400;fxBUDAL(i)=fxBUDAL(i)+  0.0400;fxAFG1(i)=fxAFG1(i)+  0.0100;fxAFG2A(i)=fxAFG2A(i)+  0.0200;fOLEP(i)=fOLEP(i)+  0.0300;fCATL(i)=fCATL(i)+  0.7700;fzRANO3(i)=fzRANO3(i)+  0.0100;fyRAOOH(i)=fyRAOOH(i)+  0.0800;fSumRO2(i)=fSumRO2(i)+  0.0800;

% 571, <PHN3>
i=i+1;
Rnames{ 571} = 'PHEN + NO3 = HNO3 + BZO ';
k(:,i) = (  4.5000E-12 ); 
Gstr{i,   1}='PHEN';Gstr{i,   2}='NO3';
fPHEN(i)=fPHEN(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;fBZO(i)=fBZO(i)+  1.0000;

% 572, <L1OH>
i=i+1;
Rnames{ 572} = 'ALK1 + OH = 0.96000*xHO2 +  0.97000*RO2C +  0.03000*RO2XC +  0.01000*xMECO3 +  0.01000*xHCHO +  0.09000*xMGLY +  0.43000*xOACID +  0.03000*zRCNO3 +  0.62000*yHPCRB + 39.33000*NROG +  0.43000*CO + SumRO2 ';
k(:,i) = (  3.3500E-13 ); 
Gstr{i,   1}='ALK1';Gstr{i,   2}='OH';
fALK1(i)=fALK1(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  0.9600;fRO2C(i)=fRO2C(i)+  0.9700;fRO2XC(i)=fRO2XC(i)+  0.0300;fxMECO3(i)=fxMECO3(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.0100;fxMGLY(i)=fxMGLY(i)+  0.0900;fxOACID(i)=fxOACID(i)+  0.4300;fzRCNO3(i)=fzRCNO3(i)+  0.0300;fyHPCRB(i)=fyHPCRB(i)+  0.6200;fNROG(i)=fNROG(i)+ 39.3300;fCO(i)=fCO(i)+  0.4300;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 573, <L2OH>
i=i+1;
Rnames{ 573} = 'ALK2 + OH = 0.10000*xHO2 +  0.95000*RO2C +  0.05000*RO2XC +  0.01000*xMEO2 +  0.84000*xMECO3 +  0.01000*xHCHO +  0.08000*xRCHO +  0.01000*xMGLY +  0.84000*xOACID +  0.05000*zRCNO3 +  0.64000*yHPCRB +  1.38000*NROG + SumRO2 ';
k(:,i) = (  1.6700E-12 ); 
Gstr{i,   1}='ALK2';Gstr{i,   2}='OH';
fALK2(i)=fALK2(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  0.1000;fRO2C(i)=fRO2C(i)+  0.9500;fRO2XC(i)=fRO2XC(i)+  0.0500;fxMEO2(i)=fxMEO2(i)+  0.0100;fxMECO3(i)=fxMECO3(i)+  0.8400;fxHCHO(i)=fxHCHO(i)+  0.0100;fxRCHO(i)=fxRCHO(i)+  0.0800;fxMGLY(i)=fxMGLY(i)+  0.0100;fxOACID(i)=fxOACID(i)+  0.8400;fzRCNO3(i)=fzRCNO3(i)+  0.0500;fyHPCRB(i)=fyHPCRB(i)+  0.6400;fNROG(i)=fNROG(i)+  1.3800;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 574, <L3OH>
i=i+1;
Rnames{ 574} = 'ALK3 + OH = 0.34000*xHO2 +  1.20000*RO2C +  0.12000*RO2XC +  0.14000*xETO2 +  0.07000*xR2CO3 +  0.33000*xTBUO +  0.10000*xHCHO +  0.10000*xMECHO +  0.09000*xRCHO +  0.07000*xACET +  0.01000*xKET2 +  0.12000*xOACID +  0.07000*zR1NO3 +  0.03000*zRCNO3 +  0.02000*zRHNO3 +  0.81000*yROOH +  0.33000*yHPCRB +  0.12000*ALK1 + 16.91000*NROG +  0.05000*CO +  1.32000*SumRO2 ';
k(:,i) = (  2.8500E-12 ); 
Gstr{i,   1}='ALK3';Gstr{i,   2}='OH';
fALK3(i)=fALK3(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  0.3400;fRO2C(i)=fRO2C(i)+  1.2000;fRO2XC(i)=fRO2XC(i)+  0.1200;fxETO2(i)=fxETO2(i)+  0.1400;fxR2CO3(i)=fxR2CO3(i)+  0.0700;fxTBUO(i)=fxTBUO(i)+  0.3300;fxHCHO(i)=fxHCHO(i)+  0.1000;fxMECHO(i)=fxMECHO(i)+  0.1000;fxRCHO(i)=fxRCHO(i)+  0.0900;fxACET(i)=fxACET(i)+  0.0700;fxKET2(i)=fxKET2(i)+  0.0100;fxOACID(i)=fxOACID(i)+  0.1200;fzR1NO3(i)=fzR1NO3(i)+  0.0700;fzRCNO3(i)=fzRCNO3(i)+  0.0300;fzRHNO3(i)=fzRHNO3(i)+  0.0200;fyROOH(i)=fyROOH(i)+  0.8100;fyHPCRB(i)=fyHPCRB(i)+  0.3300;fALK1(i)=fALK1(i)+  0.1200;fNROG(i)=fNROG(i)+ 16.9100;fCO(i)=fCO(i)+  0.0500;fSumRO2(i)=fSumRO2(i)+  1.3200;

% 575, <L4OH>
i=i+1;
Rnames{ 575} = 'ALK4 + OH = 0.01000*xOH +  0.27000*HO2 +  0.36000*xHO2 +  0.96000*RO2C +  0.12000*RO2XC +  0.23000*xETO2 +  0.06000*xHCHO +  0.13000*xMECHO +  0.05000*xETCHO +  0.07000*xRCHO +  0.26000*ACET +  0.30000*xACET +  0.03000*xMEK +  0.15000*xKET2 +  0.09000*zR1NO3 +  0.03000*zRHNO3 +  1.05000*yROOH +  0.01000*HPCRB +  0.01000*yHPCRB +  0.01000*CO2 +  1.08000*SumRO2 ';
k(:,i) = (  4.5400E-12 ); 
Gstr{i,   1}='ALK4';Gstr{i,   2}='OH';
fALK4(i)=fALK4(i)-1.0;fOH(i)=fOH(i)-1.0;
fxOH(i)=fxOH(i)+  0.0100;fHO2(i)=fHO2(i)+  0.2700;fxHO2(i)=fxHO2(i)+  0.3600;fRO2C(i)=fRO2C(i)+  0.9600;fRO2XC(i)=fRO2XC(i)+  0.1200;fxETO2(i)=fxETO2(i)+  0.2300;fxHCHO(i)=fxHCHO(i)+  0.0600;fxMECHO(i)=fxMECHO(i)+  0.1300;fxETCHO(i)=fxETCHO(i)+  0.0500;fxRCHO(i)=fxRCHO(i)+  0.0700;fACET(i)=fACET(i)+  0.2600;fxACET(i)=fxACET(i)+  0.3000;fxMEK(i)=fxMEK(i)+  0.0300;fxKET2(i)=fxKET2(i)+  0.1500;fzR1NO3(i)=fzR1NO3(i)+  0.0900;fzRHNO3(i)=fzRHNO3(i)+  0.0300;fyROOH(i)=fyROOH(i)+  1.0500;fHPCRB(i)=fHPCRB(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.0100;fCO2(i)=fCO2(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  1.0800;

% 576, <L5OH>
i=i+1;
Rnames{ 576} = 'ALK5 + OH = ALK5_OH +  0.30000*HO2 +  0.33000*xHO2 + RO2C +  0.22000*RO2XC +  0.02000*xETO2 +  0.01000*xR2CO3 +  0.01000*HCHO +  0.03000*xHCHO +  0.04000*xMECHO +  0.02000*xETCHO +  0.11000*RCHO +  0.04000*xRCHO +  0.06000*GLCHO +  0.01000*xGLCHO +  0.05000*xACET +  0.05000*xMEK +  0.13000*KET2 +  0.23000*xKET2 +  0.01000*xPACID +  0.08000*zR1NO3 +  0.05000*zR2NO3 +  0.02000*zRCNO3 +  0.06000*zRHNO3 +  1.05000*yROOH +  0.12000*yHPCRB +  0.01000*ALK4 +  0.01000*ALK5 +  1.22000*SumRO2 ';
k(:,i) = (  1.1400E-11 ); 
Gstr{i,   1}='ALK5';Gstr{i,   2}='OH';
fALK5(i)=fALK5(i)-1.0;fOH(i)=fOH(i)-1.0;
fALK5_OH(i)=fALK5_OH(i)+  1.0000;fHO2(i)=fHO2(i)+  0.3000;fxHO2(i)=fxHO2(i)+  0.3300;fRO2C(i)=fRO2C(i)+  1.0000;fRO2XC(i)=fRO2XC(i)+  0.2200;fxETO2(i)=fxETO2(i)+  0.0200;fxR2CO3(i)=fxR2CO3(i)+  0.0100;fHCHO(i)=fHCHO(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.0300;fxMECHO(i)=fxMECHO(i)+  0.0400;fxETCHO(i)=fxETCHO(i)+  0.0200;fRCHO(i)=fRCHO(i)+  0.1100;fxRCHO(i)=fxRCHO(i)+  0.0400;fGLCHO(i)=fGLCHO(i)+  0.0600;fxGLCHO(i)=fxGLCHO(i)+  0.0100;fxACET(i)=fxACET(i)+  0.0500;fxMEK(i)=fxMEK(i)+  0.0500;fKET2(i)=fKET2(i)+  0.1300;fxKET2(i)=fxKET2(i)+  0.2300;fxPACID(i)=fxPACID(i)+  0.0100;fzR1NO3(i)=fzR1NO3(i)+  0.0800;fzR2NO3(i)=fzR2NO3(i)+  0.0500;fzRCNO3(i)=fzRCNO3(i)+  0.0200;fzRHNO3(i)=fzRHNO3(i)+  0.0600;fyROOH(i)=fyROOH(i)+  1.0500;fyHPCRB(i)=fyHPCRB(i)+  0.1200;fALK4(i)=fALK4(i)+  0.0100;fALK5(i)=fALK5(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  1.2200;

% 577, <597>
i=i+1;
Rnames{ 577} = 'ALK5_OH = 0.11000*HO2 +  0.03000*PACID +  0.08000*HPCRB ';
k(:,i) = (  2.8700E-01 ); 
Gstr{i,   1}='ALK5_OH';
fALK5_OH(i)=fALK5_OH(i)-1.0;
fHO2(i)=fHO2(i)+  0.1100;fPACID(i)=fPACID(i)+  0.0300;fHPCRB(i)=fHPCRB(i)+  0.0800;

% 578, <598>
i=i+1;
Rnames{ 578} = 'ALK5_OH + NO = NO +  0.06000*xHO2 +  0.11000*RO2C +  0.02000*RO2XC +  0.02000*xMECO3 +  0.01000*xR2CO3 +  0.02000*xETCHO +  0.06000*xRCHO +  0.01000*xKET2 +  0.01000*zRCNO3 +  0.01000*zRHNO3 +  0.05000*yROOH +  0.08000*yHPCRB +  0.01000*ALK5 +  0.12000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='ALK5_OH';Gstr{i,   2}='NO';
fALK5_OH(i)=fALK5_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0600;fRO2C(i)=fRO2C(i)+  0.1100;fRO2XC(i)=fRO2XC(i)+  0.0200;fxMECO3(i)=fxMECO3(i)+  0.0200;fxR2CO3(i)=fxR2CO3(i)+  0.0100;fxETCHO(i)=fxETCHO(i)+  0.0200;fxRCHO(i)=fxRCHO(i)+  0.0600;fxKET2(i)=fxKET2(i)+  0.0100;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fzRHNO3(i)=fzRHNO3(i)+  0.0100;fyROOH(i)=fyROOH(i)+  0.0500;fyHPCRB(i)=fyHPCRB(i)+  0.0800;fALK5(i)=fALK5(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  0.1200;

% 579, <L6OH>
i=i+1;
Rnames{ 579} = 'ALK6 + OH = ALK6_OH +  0.16000*HO2 +  0.40000*xHO2 +  0.97000*RO2C +  0.33000*RO2XC +  0.01000*xR2CO3 +  0.01000*xTBUO +  0.08000*xHCHO +  0.01000*xMECHO +  0.01000*xETCHO +  0.07000*RCHO +  0.08000*xRCHO +  0.15000*xACET +  0.09000*KET2 +  0.26000*xKET2 +  0.01000*zR1NO3 +  0.15000*zR2NO3 +  0.05000*zRCNO3 +  0.12000*zRHNO3 +  1.05000*yROOH +  0.24000*yHPCRB +  0.06000*ALK3 +  0.01000*ALK4 +  0.01000*ALK5 +  1.31000*SumRO2 ';
k(:,i) = (  1.6300E-11 ); 
Gstr{i,   1}='ALK6';Gstr{i,   2}='OH';
fALK6(i)=fALK6(i)-1.0;fOH(i)=fOH(i)-1.0;
fALK6_OH(i)=fALK6_OH(i)+  1.0000;fHO2(i)=fHO2(i)+  0.1600;fxHO2(i)=fxHO2(i)+  0.4000;fRO2C(i)=fRO2C(i)+  0.9700;fRO2XC(i)=fRO2XC(i)+  0.3300;fxR2CO3(i)=fxR2CO3(i)+  0.0100;fxTBUO(i)=fxTBUO(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.0800;fxMECHO(i)=fxMECHO(i)+  0.0100;fxETCHO(i)=fxETCHO(i)+  0.0100;fRCHO(i)=fRCHO(i)+  0.0700;fxRCHO(i)=fxRCHO(i)+  0.0800;fxACET(i)=fxACET(i)+  0.1500;fKET2(i)=fKET2(i)+  0.0900;fxKET2(i)=fxKET2(i)+  0.2600;fzR1NO3(i)=fzR1NO3(i)+  0.0100;fzR2NO3(i)=fzR2NO3(i)+  0.1500;fzRCNO3(i)=fzRCNO3(i)+  0.0500;fzRHNO3(i)=fzRHNO3(i)+  0.1200;fyROOH(i)=fyROOH(i)+  1.0500;fyHPCRB(i)=fyHPCRB(i)+  0.2400;fALK3(i)=fALK3(i)+  0.0600;fALK4(i)=fALK4(i)+  0.0100;fALK5(i)=fALK5(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  1.3100;

% 580, <600>
i=i+1;
Rnames{ 580} = 'ALK6_OH = 0.03000*HO2 +  0.08000*HPCRB ';
k(:,i) = (  2.7000E-01 ); 
Gstr{i,   1}='ALK6_OH';
fALK6_OH(i)=fALK6_OH(i)-1.0;
fHO2(i)=fHO2(i)+  0.0300;fHPCRB(i)=fHPCRB(i)+  0.0800;

% 581, <601>
i=i+1;
Rnames{ 581} = 'ALK6_OH + NO = NO +  0.06000*xHO2 +  0.12000*RO2C +  0.02000*RO2XC +  0.01000*xR2CO3 +  0.02000*xHCHO +  0.01000*xMECHO +  0.02000*xRCHO +  0.04000*xACET +  0.01000*xKET2 +  0.01000*zRCNO3 +  0.01000*zRHNO3 +  0.06000*yROOH +  0.04000*yHPCRB +  0.04000*ALK2 +  0.01000*ALK5 +  0.14000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='ALK6_OH';Gstr{i,   2}='NO';
fALK6_OH(i)=fALK6_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0600;fRO2C(i)=fRO2C(i)+  0.1200;fRO2XC(i)=fRO2XC(i)+  0.0200;fxR2CO3(i)=fxR2CO3(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.0200;fxMECHO(i)=fxMECHO(i)+  0.0100;fxRCHO(i)=fxRCHO(i)+  0.0200;fxACET(i)=fxACET(i)+  0.0400;fxKET2(i)=fxKET2(i)+  0.0100;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fzRHNO3(i)=fzRHNO3(i)+  0.0100;fyROOH(i)=fyROOH(i)+  0.0600;fyHPCRB(i)=fyHPCRB(i)+  0.0400;fALK2(i)=fALK2(i)+  0.0400;fALK5(i)=fALK5(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  0.1400;

% 582, <O1OH>
i=i+1;
Rnames{ 582} = 'OLE1 + OH = 0.78000*xHO2 +  1.12000*RO2C +  0.10000*RO2XC +  0.01000*xMEO2 +  0.11000*xTBUO +  0.69000*xHCHO +  0.01000*xMECHO +  0.35000*xETCHO +  0.35000*xRCHO +  0.15000*xGLCHO +  0.02000*xACRO +  0.03000*xACET +  0.02000*xKET2 +  0.01000*xMVK +  0.01000*zR1NO3 +  0.01000*zR2NO3 +  0.08000*zRHNO3 +  1.15000*yROOH +  0.07000*yRUOOH +  1.22000*SumRO2 ';
k(:,i) = (  3.1800E-11 ); 
Gstr{i,   1}='OLE1';Gstr{i,   2}='OH';
fOLE1(i)=fOLE1(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  0.7800;fRO2C(i)=fRO2C(i)+  1.1200;fRO2XC(i)=fRO2XC(i)+  0.1000;fxMEO2(i)=fxMEO2(i)+  0.0100;fxTBUO(i)=fxTBUO(i)+  0.1100;fxHCHO(i)=fxHCHO(i)+  0.6900;fxMECHO(i)=fxMECHO(i)+  0.0100;fxETCHO(i)=fxETCHO(i)+  0.3500;fxRCHO(i)=fxRCHO(i)+  0.3500;fxGLCHO(i)=fxGLCHO(i)+  0.1500;fxACRO(i)=fxACRO(i)+  0.0200;fxACET(i)=fxACET(i)+  0.0300;fxKET2(i)=fxKET2(i)+  0.0200;fxMVK(i)=fxMVK(i)+  0.0100;fzR1NO3(i)=fzR1NO3(i)+  0.0100;fzR2NO3(i)=fzR2NO3(i)+  0.0100;fzRHNO3(i)=fzRHNO3(i)+  0.0800;fyROOH(i)=fyROOH(i)+  1.1500;fyRUOOH(i)=fyRUOOH(i)+  0.0700;fSumRO2(i)=fSumRO2(i)+  1.2200;

% 583, <O1O3>
i=i+1;
Rnames{ 583} = 'OLE1 + O3 = 0.26000*OH +  0.01000*xOH +  0.17000*HO2 +  0.16000*xHO2 +  0.18000*RO2C +  0.01000*RO2XC +  0.01000*ETO2 +  0.01000*xTBUO +  0.21000*HCHO2 +  0.17000*RCHO2 +  0.50000*HCHO +  0.08000*xMECHO +  0.19000*ETCHO +  0.04000*xETCHO +  0.31000*RCHO +  0.02000*xRCHO +  0.04000*xACET +  0.02000*ETOH +  0.01000*zRCNO3 +  0.02000*yROOH +  0.14000*yHPCRB +  0.03000*ETHAN +  0.02000*PROP +  0.01000*NC4 +  0.01000*ALK2 +  0.02000*ALK3 +  0.01000*ALK4 +  0.24000*CO2 +  0.37000*CO +  0.20000*SumRO2 ';
k(:,i) = (  8.7000E-18 ); 
Gstr{i,   1}='OLE1';Gstr{i,   2}='O3';
fOLE1(i)=fOLE1(i)-1.0;fO3(i)=fO3(i)-1.0;
fOH(i)=fOH(i)+  0.2600;fxOH(i)=fxOH(i)+  0.0100;fHO2(i)=fHO2(i)+  0.1700;fxHO2(i)=fxHO2(i)+  0.1600;fRO2C(i)=fRO2C(i)+  0.1800;fRO2XC(i)=fRO2XC(i)+  0.0100;fETO2(i)=fETO2(i)+  0.0100;fxTBUO(i)=fxTBUO(i)+  0.0100;fHCHO2(i)=fHCHO2(i)+  0.2100;fRCHO2(i)=fRCHO2(i)+  0.1700;fHCHO(i)=fHCHO(i)+  0.5000;fxMECHO(i)=fxMECHO(i)+  0.0800;fETCHO(i)=fETCHO(i)+  0.1900;fxETCHO(i)=fxETCHO(i)+  0.0400;fRCHO(i)=fRCHO(i)+  0.3100;fxRCHO(i)=fxRCHO(i)+  0.0200;fxACET(i)=fxACET(i)+  0.0400;fETOH(i)=fETOH(i)+  0.0200;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fyROOH(i)=fyROOH(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.1400;fETHAN(i)=fETHAN(i)+  0.0300;fPROP(i)=fPROP(i)+  0.0200;fNC4(i)=fNC4(i)+  0.0100;fALK2(i)=fALK2(i)+  0.0100;fALK3(i)=fALK3(i)+  0.0200;fALK4(i)=fALK4(i)+  0.0100;fCO2(i)=fCO2(i)+  0.2400;fCO(i)=fCO(i)+  0.3700;fSumRO2(i)=fSumRO2(i)+  0.2000;

% 584, <O1N3>
i=i+1;
Rnames{ 584} = 'OLE1 + NO3 = 0.09000*xNO2 +  0.55000*xHO2 +  1.41000*RO2C +  0.13000*RO2XC +  0.09000*xETO2 +  0.15000*xTBUO +  0.09000*xHCHO +  0.09000*xETCHO +  0.01000*xRCHO +  0.14000*xACET +  0.02000*zR1NO3 +  0.81000*xRCNO3 +  1.22000*yRPNO3 +  0.11000*zRDNO3 +  0.31000*yROOH +  1.53000*SumRO2 ';
k(:,i) = (  1.4400E-14 ); 
Gstr{i,   1}='OLE1';Gstr{i,   2}='NO3';
fOLE1(i)=fOLE1(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fxNO2(i)=fxNO2(i)+  0.0900;fxHO2(i)=fxHO2(i)+  0.5500;fRO2C(i)=fRO2C(i)+  1.4100;fRO2XC(i)=fRO2XC(i)+  0.1300;fxETO2(i)=fxETO2(i)+  0.0900;fxTBUO(i)=fxTBUO(i)+  0.1500;fxHCHO(i)=fxHCHO(i)+  0.0900;fxETCHO(i)=fxETCHO(i)+  0.0900;fxRCHO(i)=fxRCHO(i)+  0.0100;fxACET(i)=fxACET(i)+  0.1400;fzR1NO3(i)=fzR1NO3(i)+  0.0200;fxRCNO3(i)=fxRCNO3(i)+  0.8100;fyRPNO3(i)=fyRPNO3(i)+  1.2200;fzRDNO3(i)=fzRDNO3(i)+  0.1100;fyROOH(i)=fyROOH(i)+  0.3100;fSumRO2(i)=fSumRO2(i)+  1.5300;

% 585, <O1OP>
i=i+1;
Rnames{ 585} = 'OLE1 + O3P = 0.25000*RCHO +  0.10000*MEK +  0.15000*KET2 +  0.09000*ALK2 +  0.36000*ALK3 +  0.05000*ALK4 ';
k(:,i) = (  4.4300E-12 ); 
Gstr{i,   1}='OLE1';Gstr{i,   2}='O3P';
fOLE1(i)=fOLE1(i)-1.0;fO3P(i)=fO3P(i)-1.0;
fRCHO(i)=fRCHO(i)+  0.2500;fMEK(i)=fMEK(i)+  0.1000;fKET2(i)=fKET2(i)+  0.1500;fALK2(i)=fALK2(i)+  0.0900;fALK3(i)=fALK3(i)+  0.3600;fALK4(i)=fALK4(i)+  0.0500;

% 586, <O2OH>
i=i+1;
Rnames{ 586} = 'OLE2 + OH = 0.92000*xHO2 +  0.94000*RO2C +  0.07000*RO2XC +  1.25000*xMECHO +  0.40000*xETCHO +  0.12000*xRCHO +  0.01000*xACRO +  0.07000*zRHNO3 +  0.99000*yROOH +  0.01000*yRUOOH +  1.01000*SumRO2 ';
k(:,i) = (  6.2900E-11 ); 
Gstr{i,   1}='OLE2';Gstr{i,   2}='OH';
fOLE2(i)=fOLE2(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  0.9200;fRO2C(i)=fRO2C(i)+  0.9400;fRO2XC(i)=fRO2XC(i)+  0.0700;fxMECHO(i)=fxMECHO(i)+  1.2500;fxETCHO(i)=fxETCHO(i)+  0.4000;fxRCHO(i)=fxRCHO(i)+  0.1200;fxACRO(i)=fxACRO(i)+  0.0100;fzRHNO3(i)=fzRHNO3(i)+  0.0700;fyROOH(i)=fyROOH(i)+  0.9900;fyRUOOH(i)=fyRUOOH(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  1.0100;

% 587, <O2O3>
i=i+1;
Rnames{ 587} = 'OLE2 + O3 = OLE2_O3 +  0.46000*OH +  0.06000*HO2 +  0.32000*xHO2 +  0.40000*RO2C +  0.01000*RO2XC +  0.04000*MEO2 +  0.01000*ETO2 +  0.16000*MECHO2 +  0.10000*RCHO2 +  0.07000*xHCHO +  0.67000*MECHO +  0.09000*xMECHO +  0.22000*ETCHO +  0.01000*xETCHO +  0.07000*RCHO +  0.01000*xACET +  0.06000*MEOH +  0.02000*ETOH +  0.01000*zRCNO3 +  0.09000*yHPCRB +  0.04000*ETHAN +  0.23000*CO2 +  0.18000*CO +  0.46000*SumRO2 ';
k(:,i) = (  1.9000E-16 ); 
Gstr{i,   1}='OLE2';Gstr{i,   2}='O3';
fOLE2(i)=fOLE2(i)-1.0;fO3(i)=fO3(i)-1.0;
fOLE2_O3(i)=fOLE2_O3(i)+  1.0000;fOH(i)=fOH(i)+  0.4600;fHO2(i)=fHO2(i)+  0.0600;fxHO2(i)=fxHO2(i)+  0.3200;fRO2C(i)=fRO2C(i)+  0.4000;fRO2XC(i)=fRO2XC(i)+  0.0100;fMEO2(i)=fMEO2(i)+  0.0400;fETO2(i)=fETO2(i)+  0.0100;fMECHO2(i)=fMECHO2(i)+  0.1600;fRCHO2(i)=fRCHO2(i)+  0.1000;fxHCHO(i)=fxHCHO(i)+  0.0700;fMECHO(i)=fMECHO(i)+  0.6700;fxMECHO(i)=fxMECHO(i)+  0.0900;fETCHO(i)=fETCHO(i)+  0.2200;fxETCHO(i)=fxETCHO(i)+  0.0100;fRCHO(i)=fRCHO(i)+  0.0700;fxACET(i)=fxACET(i)+  0.0100;fMEOH(i)=fMEOH(i)+  0.0600;fETOH(i)=fETOH(i)+  0.0200;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.0900;fETHAN(i)=fETHAN(i)+  0.0400;fCO2(i)=fCO2(i)+  0.2300;fCO(i)=fCO(i)+  0.1800;fSumRO2(i)=fSumRO2(i)+  0.4600;

% 588, <608>
i=i+1;
Rnames{ 588} = 'OLE2_O3 = 0.01000*OH +  0.08000*xOH +  0.01000*RCHO +  0.22000*xPACID +  0.08000*CO2 ';
k(:,i) = (  2.4500E+00 ); 
Gstr{i,   1}='OLE2_O3';
fOLE2_O3(i)=fOLE2_O3(i)-1.0;
fOH(i)=fOH(i)+  0.0100;fxOH(i)=fxOH(i)+  0.0800;fRCHO(i)=fRCHO(i)+  0.0100;fxPACID(i)=fxPACID(i)+  0.2200;fCO2(i)=fCO2(i)+  0.0800;

% 589, <609>
i=i+1;
Rnames{ 589} = 'OLE2_O3 + NO = NO +  0.08000*xHO2 +  0.01000*RO2C +  0.01000*xR2CO3 +  0.20000*xHCHO +  0.02000*xGLY +  0.26000*yHPCRB +  0.29000*CO +  0.01000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='OLE2_O3';Gstr{i,   2}='NO';
fOLE2_O3(i)=fOLE2_O3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0800;fRO2C(i)=fRO2C(i)+  0.0100;fxR2CO3(i)=fxR2CO3(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.2000;fxGLY(i)=fxGLY(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.2600;fCO(i)=fCO(i)+  0.2900;fSumRO2(i)=fSumRO2(i)+  0.0100;

% 590, <O2N3>
i=i+1;
Rnames{ 590} = 'OLE2 + NO3 = 0.80000*xNO2 +  0.11000*xHO2 +  1.01000*RO2C +  0.08000*RO2XC +  1.11000*xMECHO +  0.33000*xETCHO +  0.09000*xRCHO +  0.01000*xACET +  0.11000*xRCNO3 +  1.08000*yRPNO3 +  0.08000*zRDNO3 +  0.01000*yROOH +  1.09000*SumRO2 ';
k(:,i) = (  4.3400E-13 ); 
Gstr{i,   1}='OLE2';Gstr{i,   2}='NO3';
fOLE2(i)=fOLE2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fxNO2(i)=fxNO2(i)+  0.8000;fxHO2(i)=fxHO2(i)+  0.1100;fRO2C(i)=fRO2C(i)+  1.0100;fRO2XC(i)=fRO2XC(i)+  0.0800;fxMECHO(i)=fxMECHO(i)+  1.1100;fxETCHO(i)=fxETCHO(i)+  0.3300;fxRCHO(i)=fxRCHO(i)+  0.0900;fxACET(i)=fxACET(i)+  0.0100;fxRCNO3(i)=fxRCNO3(i)+  0.1100;fyRPNO3(i)=fyRPNO3(i)+  1.0800;fzRDNO3(i)=fzRDNO3(i)+  0.0800;fyROOH(i)=fyROOH(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  1.0900;

% 591, <O2OP>
i=i+1;
Rnames{ 591} = 'OLE2 + O3P = 0.21000*MEK +  0.29000*KET2 +  0.21000*ALK1 +  0.22000*ALK2 +  0.07000*ALK3 ';
k(:,i) = (  1.9500E-11 ); 
Gstr{i,   1}='OLE2';Gstr{i,   2}='O3P';
fOLE2(i)=fOLE2(i)-1.0;fO3P(i)=fO3P(i)-1.0;
fMEK(i)=fMEK(i)+  0.2100;fKET2(i)=fKET2(i)+  0.2900;fALK1(i)=fALK1(i)+  0.2100;fALK2(i)=fALK2(i)+  0.2200;fALK3(i)=fALK3(i)+  0.0700;

% 592, <O3OH>
i=i+1;
Rnames{ 592} = 'OLE3 + OH = 0.94000*xHO2 +  0.94000*RO2C +  0.05000*RO2XC +  0.94000*xHCHO +  0.82000*xACET +  0.12000*xMEK +  0.05000*zRHNO3 + yROOH + SumRO2 ';
k(:,i) = (  5.2600E-11 ); 
Gstr{i,   1}='OLE3';Gstr{i,   2}='OH';
fOLE3(i)=fOLE3(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  0.9400;fRO2C(i)=fRO2C(i)+  0.9400;fRO2XC(i)=fRO2XC(i)+  0.0500;fxHCHO(i)=fxHCHO(i)+  0.9400;fxACET(i)=fxACET(i)+  0.8200;fxMEK(i)=fxMEK(i)+  0.1200;fzRHNO3(i)=fzRHNO3(i)+  0.0500;fyROOH(i)=fyROOH(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 593, <O3O3>
i=i+1;
Rnames{ 593} = 'OLE3 + O3 = 0.58000*OH +  0.14000*HO2 +  0.48000*RO2C +  0.02000*RO2XC +  0.45000*xMECO3 +  0.03000*xR2CO3 +  0.21000*HCHO2 +  0.50000*HCHO +  0.45000*xHCHO +  0.03000*xMECHO +  0.43000*ACET +  0.07000*MEK +  0.02000*zRCNO3 +  0.42000*yHPCRB +  0.12000*CO2 +  0.17000*CO +  0.50000*SumRO2 ';
k(:,i) = (  1.1800E-17 ); 
Gstr{i,   1}='OLE3';Gstr{i,   2}='O3';
fOLE3(i)=fOLE3(i)-1.0;fO3(i)=fO3(i)-1.0;
fOH(i)=fOH(i)+  0.5800;fHO2(i)=fHO2(i)+  0.1400;fRO2C(i)=fRO2C(i)+  0.4800;fRO2XC(i)=fRO2XC(i)+  0.0200;fxMECO3(i)=fxMECO3(i)+  0.4500;fxR2CO3(i)=fxR2CO3(i)+  0.0300;fHCHO2(i)=fHCHO2(i)+  0.2100;fHCHO(i)=fHCHO(i)+  0.5000;fxHCHO(i)=fxHCHO(i)+  0.4500;fxMECHO(i)=fxMECHO(i)+  0.0300;fACET(i)=fACET(i)+  0.4300;fMEK(i)=fMEK(i)+  0.0700;fzRCNO3(i)=fzRCNO3(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.4200;fCO2(i)=fCO2(i)+  0.1200;fCO(i)=fCO(i)+  0.1700;fSumRO2(i)=fSumRO2(i)+  0.5000;

% 594, <O3N3>
i=i+1;
Rnames{ 594} = 'OLE3 + NO3 = 0.86000*xNO2 +  0.95000*RO2C +  0.05000*RO2XC +  0.01000*xMEO2 +  0.07000*xETO2 +  0.86000*xHCHO +  0.80000*xACET +  0.06000*xMEK +  0.08000*xRCNO3 + yRPNO3 +  0.05000*zRDNO3 + SumRO2 ';
k(:,i) = (  3.6200E-13 ); 
Gstr{i,   1}='OLE3';Gstr{i,   2}='NO3';
fOLE3(i)=fOLE3(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fxNO2(i)=fxNO2(i)+  0.8600;fRO2C(i)=fRO2C(i)+  0.9500;fRO2XC(i)=fRO2XC(i)+  0.0500;fxMEO2(i)=fxMEO2(i)+  0.0100;fxETO2(i)=fxETO2(i)+  0.0700;fxHCHO(i)=fxHCHO(i)+  0.8600;fxACET(i)=fxACET(i)+  0.8000;fxMEK(i)=fxMEK(i)+  0.0600;fxRCNO3(i)=fxRCNO3(i)+  0.0800;fyRPNO3(i)=fyRPNO3(i)+  1.0000;fzRDNO3(i)=fzRDNO3(i)+  0.0500;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 595, <O3OP>
i=i+1;
Rnames{ 595} = 'OLE3 + O3P = 0.50000*RCHO +  0.50000*ALK2 ';
k(:,i) = (  1.7000E-11 ); 
Gstr{i,   1}='OLE3';Gstr{i,   2}='O3P';
fOLE3(i)=fOLE3(i)-1.0;fO3P(i)=fO3P(i)-1.0;
fRCHO(i)=fRCHO(i)+  0.5000;fALK2(i)=fALK2(i)+  0.5000;

% 596, <O4OH>
i=i+1;
Rnames{ 596} = 'OLE4 + OH = 0.92000*xHO2 +  0.92000*RO2C +  0.08000*RO2XC +  0.83000*xMECHO +  0.09000*xETCHO +  0.92000*xACET +  0.08000*zRHNO3 + yROOH + SumRO2 ';
k(:,i) = (  8.7100E-11 ); 
Gstr{i,   1}='OLE4';Gstr{i,   2}='OH';
fOLE4(i)=fOLE4(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  0.9200;fRO2C(i)=fRO2C(i)+  0.9200;fRO2XC(i)=fRO2XC(i)+  0.0800;fxMECHO(i)=fxMECHO(i)+  0.8300;fxETCHO(i)=fxETCHO(i)+  0.0900;fxACET(i)=fxACET(i)+  0.9200;fzRHNO3(i)=fzRHNO3(i)+  0.0800;fyROOH(i)=fyROOH(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 597, <O4O3>
i=i+1;
Rnames{ 597} = 'OLE4 + O3 = OLE4_O3 +  0.72000*OH +  0.03000*HO2 +  0.16000*xHO2 +  0.70000*RO2C +  0.02000*RO2XC +  0.03000*MEO2 +  0.48000*xMECO3 +  0.11000*MECHO2 +  0.01000*RCHO2 +  0.53000*xHCHO +  0.45000*MECHO +  0.02000*xMECHO +  0.05000*ETCHO +  0.50000*ACET +  0.05000*MEOH +  0.02000*zRCNO3 +  0.44000*yHPCRB +  0.01000*ETHAN +  0.12000*CO2 +  0.07000*CO +  0.75000*SumRO2 ';
k(:,i) = (  4.0500E-16 ); 
Gstr{i,   1}='OLE4';Gstr{i,   2}='O3';
fOLE4(i)=fOLE4(i)-1.0;fO3(i)=fO3(i)-1.0;
fOLE4_O3(i)=fOLE4_O3(i)+  1.0000;fOH(i)=fOH(i)+  0.7200;fHO2(i)=fHO2(i)+  0.0300;fxHO2(i)=fxHO2(i)+  0.1600;fRO2C(i)=fRO2C(i)+  0.7000;fRO2XC(i)=fRO2XC(i)+  0.0200;fMEO2(i)=fMEO2(i)+  0.0300;fxMECO3(i)=fxMECO3(i)+  0.4800;fMECHO2(i)=fMECHO2(i)+  0.1100;fRCHO2(i)=fRCHO2(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.5300;fMECHO(i)=fMECHO(i)+  0.4500;fxMECHO(i)=fxMECHO(i)+  0.0200;fETCHO(i)=fETCHO(i)+  0.0500;fACET(i)=fACET(i)+  0.5000;fMEOH(i)=fMEOH(i)+  0.0500;fzRCNO3(i)=fzRCNO3(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.4400;fETHAN(i)=fETHAN(i)+  0.0100;fCO2(i)=fCO2(i)+  0.1200;fCO(i)=fCO(i)+  0.0700;fSumRO2(i)=fSumRO2(i)+  0.7500;

% 598, <618>
i=i+1;
Rnames{ 598} = 'OLE4_O3 = 0.05000*xOH +  0.14000*xPACID +  0.05000*CO2 ';
k(:,i) = (  2.4500E+00 ); 
Gstr{i,   1}='OLE4_O3';
fOLE4_O3(i)=fOLE4_O3(i)-1.0;
fxOH(i)=fxOH(i)+  0.0500;fxPACID(i)=fxPACID(i)+  0.1400;fCO2(i)=fCO2(i)+  0.0500;

% 599, <619>
i=i+1;
Rnames{ 599} = 'OLE4_O3 + NO = NO +  0.05000*xHO2 +  0.14000*xHCHO +  0.17000*yHPCRB +  0.18000*CO ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='OLE4_O3';Gstr{i,   2}='NO';
fOLE4_O3(i)=fOLE4_O3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0500;fxHCHO(i)=fxHCHO(i)+  0.1400;fyHPCRB(i)=fyHPCRB(i)+  0.1700;fCO(i)=fCO(i)+  0.1800;

% 600, <O4N3>
i=i+1;
Rnames{ 600} = 'OLE4 + NO3 = 0.92000*xNO2 +  0.92000*RO2C +  0.08000*RO2XC +  0.83000*xMECHO +  0.09000*xETCHO +  0.92000*xACET + yRPNO3 +  0.08000*zRDNO3 + SumRO2 ';
k(:,i) = (  9.3100E-12 ); 
Gstr{i,   1}='OLE4';Gstr{i,   2}='NO3';
fOLE4(i)=fOLE4(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fxNO2(i)=fxNO2(i)+  0.9200;fRO2C(i)=fRO2C(i)+  0.9200;fRO2XC(i)=fRO2XC(i)+  0.0800;fxMECHO(i)=fxMECHO(i)+  0.8300;fxETCHO(i)=fxETCHO(i)+  0.0900;fxACET(i)=fxACET(i)+  0.9200;fyRPNO3(i)=fyRPNO3(i)+  1.0000;fzRDNO3(i)=fzRDNO3(i)+  0.0800;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 601, <O4OP>
i=i+1;
Rnames{ 601} = 'OLE4 + O3P = 0.50000*KET2 +  0.50000*ALK2 ';
k(:,i) = (  5.1100E-11 ); 
Gstr{i,   1}='OLE4';Gstr{i,   2}='O3P';
fOLE4(i)=fOLE4(i)-1.0;fO3P(i)=fO3P(i)-1.0;
fKET2(i)=fKET2(i)+  0.5000;fALK2(i)=fALK2(i)+  0.5000;

% 602, <TPOH>
i=i+1;
Rnames{ 602} = 'TERP + OH = TERP_OH +  0.02000*HO2 +  0.53000*xHO2 +  1.07000*RO2C +  0.28000*RO2XC +  0.01000*xMACO3 +  0.15000*xHCHO +  0.29000*xRCHO +  0.13000*xOLEA2 +  0.09000*xACET +  0.05000*xKET2 +  0.01000*xMVK +  0.04000*xLVKS +  0.05000*xOLEP +  0.02000*zR2NO3 +  0.06000*zRCNO3 +  0.20000*zRHNO3 +  0.51000*yROOH +  0.55000*yRUOOH +  0.01000*HPCRB +  0.24000*yHPCRB +  1.35000*SumRO2 + TRPRXN ';
k(:,i) = (  1.1000E-10 ); 
Gstr{i,   1}='TERP';Gstr{i,   2}='OH';
fTERP(i)=fTERP(i)-1.0;fOH(i)=fOH(i)-1.0;
fTERP_OH(i)=fTERP_OH(i)+  1.0000;fHO2(i)=fHO2(i)+  0.0200;fxHO2(i)=fxHO2(i)+  0.5300;fRO2C(i)=fRO2C(i)+  1.0700;fRO2XC(i)=fRO2XC(i)+  0.2800;fxMACO3(i)=fxMACO3(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.1500;fxRCHO(i)=fxRCHO(i)+  0.2900;fxOLEA2(i)=fxOLEA2(i)+  0.1300;fxACET(i)=fxACET(i)+  0.0900;fxKET2(i)=fxKET2(i)+  0.0500;fxMVK(i)=fxMVK(i)+  0.0100;fxLVKS(i)=fxLVKS(i)+  0.0400;fxOLEP(i)=fxOLEP(i)+  0.0500;fzR2NO3(i)=fzR2NO3(i)+  0.0200;fzRCNO3(i)=fzRCNO3(i)+  0.0600;fzRHNO3(i)=fzRHNO3(i)+  0.2000;fyROOH(i)=fyROOH(i)+  0.5100;fyRUOOH(i)=fyRUOOH(i)+  0.5500;fHPCRB(i)=fHPCRB(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.2400;fSumRO2(i)=fSumRO2(i)+  1.3500;fTRPRXN(i)=fTRPRXN(i)+  1.0000;

% 603, <623>
i=i+1;
Rnames{ 603} = 'TERP_OH = 0.06000*OH +  0.01000*xOH +  0.09000*HO2 +  0.02000*AFG2A +  0.01000*xKET2 +  0.03000*PACID +  0.01000*xPACID +  0.06000*HPCRB ';
k(:,i) = (  1.4800E+00 ); 
Gstr{i,   1}='TERP_OH';
fTERP_OH(i)=fTERP_OH(i)-1.0;
fOH(i)=fOH(i)+  0.0600;fxOH(i)=fxOH(i)+  0.0100;fHO2(i)=fHO2(i)+  0.0900;fAFG2A(i)=fAFG2A(i)+  0.0200;fxKET2(i)=fxKET2(i)+  0.0100;fPACID(i)=fPACID(i)+  0.0300;fxPACID(i)=fxPACID(i)+  0.0100;fHPCRB(i)=fHPCRB(i)+  0.0600;

% 604, <624>
i=i+1;
Rnames{ 604} = 'TERP_OH + NO = NO +  0.07000*xHO2 +  0.16000*RO2C +  0.05000*RO2XC +  0.03000*xR2CO3 +  0.02000*xHCHO +  0.03000*xRCHO +  0.02000*xOLEA1 +  0.02000*xOLEA2 +  0.01000*xAFG2A +  0.01000*xOLEP +  0.04000*zRCNO3 +  0.01000*zRHNO3 +  0.01000*yRUOOH +  0.22000*yHPCRB +  0.21000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='TERP_OH';Gstr{i,   2}='NO';
fTERP_OH(i)=fTERP_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0700;fRO2C(i)=fRO2C(i)+  0.1600;fRO2XC(i)=fRO2XC(i)+  0.0500;fxR2CO3(i)=fxR2CO3(i)+  0.0300;fxHCHO(i)=fxHCHO(i)+  0.0200;fxRCHO(i)=fxRCHO(i)+  0.0300;fxOLEA1(i)=fxOLEA1(i)+  0.0200;fxOLEA2(i)=fxOLEA2(i)+  0.0200;fxAFG2A(i)=fxAFG2A(i)+  0.0100;fxOLEP(i)=fxOLEP(i)+  0.0100;fzRCNO3(i)=fzRCNO3(i)+  0.0400;fzRHNO3(i)=fzRHNO3(i)+  0.0100;fyRUOOH(i)=fyRUOOH(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.2200;fSumRO2(i)=fSumRO2(i)+  0.2100;

% 605, <TPO3>
i=i+1;
Rnames{ 605} = 'TERP + O3 = TERP_O3 +  0.58000*OH +  0.02000*xOH +  0.07000*HO2 +  0.08000*xHO2 +  0.37000*RO2C +  0.12000*RO2XC +  0.08000*xMECO3 +  0.11000*xR2CO3 +  0.08000*HCHO2 +  0.25000*RCHO2 +  0.18000*HCHO +  0.06000*xHCHO +  0.13000*xRCHO +  0.03000*xGLY +  0.01000*xMACR +  0.01000*ACET +  0.01000*xACET +  0.17000*KET2 +  0.01000*LVKS +  0.01000*xAFG3 +  0.01000*xPACID +  0.02000*OLEP +  0.12000*zRCNO3 +  0.01000*yROOH +  0.01000*xHPCRB +  0.41000*yHPCRB +  0.06000*CO2 +  0.12000*CO +  0.49000*SumRO2 + TRPRXN ';
k(:,i) = (  1.1700E-16 ); 
Gstr{i,   1}='TERP';Gstr{i,   2}='O3';
fTERP(i)=fTERP(i)-1.0;fO3(i)=fO3(i)-1.0;
fTERP_O3(i)=fTERP_O3(i)+  1.0000;fOH(i)=fOH(i)+  0.5800;fxOH(i)=fxOH(i)+  0.0200;fHO2(i)=fHO2(i)+  0.0700;fxHO2(i)=fxHO2(i)+  0.0800;fRO2C(i)=fRO2C(i)+  0.3700;fRO2XC(i)=fRO2XC(i)+  0.1200;fxMECO3(i)=fxMECO3(i)+  0.0800;fxR2CO3(i)=fxR2CO3(i)+  0.1100;fHCHO2(i)=fHCHO2(i)+  0.0800;fRCHO2(i)=fRCHO2(i)+  0.2500;fHCHO(i)=fHCHO(i)+  0.1800;fxHCHO(i)=fxHCHO(i)+  0.0600;fxRCHO(i)=fxRCHO(i)+  0.1300;fxGLY(i)=fxGLY(i)+  0.0300;fxMACR(i)=fxMACR(i)+  0.0100;fACET(i)=fACET(i)+  0.0100;fxACET(i)=fxACET(i)+  0.0100;fKET2(i)=fKET2(i)+  0.1700;fLVKS(i)=fLVKS(i)+  0.0100;fxAFG3(i)=fxAFG3(i)+  0.0100;fxPACID(i)=fxPACID(i)+  0.0100;fOLEP(i)=fOLEP(i)+  0.0200;fzRCNO3(i)=fzRCNO3(i)+  0.1200;fyROOH(i)=fyROOH(i)+  0.0100;fxHPCRB(i)=fxHPCRB(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.4100;fCO2(i)=fCO2(i)+  0.0600;fCO(i)=fCO(i)+  0.1200;fSumRO2(i)=fSumRO2(i)+  0.4900;fTRPRXN(i)=fTRPRXN(i)+  1.0000;

% 606, <626>
i=i+1;
Rnames{ 606} = 'TERP_O3 = 0.06000*OH +  0.07000*HO2 +  0.01000*PACID +  0.01000*OTHN +  0.05000*HPCRB ';
k(:,i) = (  1.0700E+00 ); 
Gstr{i,   1}='TERP_O3';
fTERP_O3(i)=fTERP_O3(i)-1.0;
fOH(i)=fOH(i)+  0.0600;fHO2(i)=fHO2(i)+  0.0700;fPACID(i)=fPACID(i)+  0.0100;fOTHN(i)=fOTHN(i)+  0.0100;fHPCRB(i)=fHPCRB(i)+  0.0500;

% 607, <627>
i=i+1;
Rnames{ 607} = 'TERP_O3 + NO = NO +  0.02000*xHO2 +  0.12000*RO2C +  0.04000*RO2XC +  0.04000*xMECO3 +  0.04000*xMACO3 +  0.04000*xHCHO +  0.04000*xOLEA2 +  0.04000*zRCNO3 +  0.13000*yHPCRB +  0.16000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='TERP_O3';Gstr{i,   2}='NO';
fTERP_O3(i)=fTERP_O3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0200;fRO2C(i)=fRO2C(i)+  0.1200;fRO2XC(i)=fRO2XC(i)+  0.0400;fxMECO3(i)=fxMECO3(i)+  0.0400;fxMACO3(i)=fxMACO3(i)+  0.0400;fxHCHO(i)=fxHCHO(i)+  0.0400;fxOLEA2(i)=fxOLEA2(i)+  0.0400;fzRCNO3(i)=fzRCNO3(i)+  0.0400;fyHPCRB(i)=fyHPCRB(i)+  0.1300;fSumRO2(i)=fSumRO2(i)+  0.1600;

% 608, <TPN3>
i=i+1;
Rnames{ 608} = 'TERP + NO3 = TERP_N3 +  0.51000*xNO2 +  0.01000*OH +  0.01000*xOH +  0.05000*xHO2 +  1.12000*RO2C +  0.28000*RO2XC +  0.01000*xHCHO +  0.29000*xRCHO +  0.21000*xOLEA2 +  0.15000*xACET +  0.01000*xMVK +  0.01000*xOLEP +  0.01000*RCNO3 +  0.07000*xRCNO3 +  0.08000*zRCNO3 + yRPNO3 +  0.19000*zRDNO3 +  0.04000*yROOH +  0.01000*yRUOOH +  1.40000*SumRO2 + TRPRXN ';
k(:,i) = (  1.1100E-11 ); 
Gstr{i,   1}='TERP';Gstr{i,   2}='NO3';
fTERP(i)=fTERP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fTERP_N3(i)=fTERP_N3(i)+  1.0000;fxNO2(i)=fxNO2(i)+  0.5100;fOH(i)=fOH(i)+  0.0100;fxOH(i)=fxOH(i)+  0.0100;fxHO2(i)=fxHO2(i)+  0.0500;fRO2C(i)=fRO2C(i)+  1.1200;fRO2XC(i)=fRO2XC(i)+  0.2800;fxHCHO(i)=fxHCHO(i)+  0.0100;fxRCHO(i)=fxRCHO(i)+  0.2900;fxOLEA2(i)=fxOLEA2(i)+  0.2100;fxACET(i)=fxACET(i)+  0.1500;fxMVK(i)=fxMVK(i)+  0.0100;fxOLEP(i)=fxOLEP(i)+  0.0100;fRCNO3(i)=fRCNO3(i)+  0.0100;fxRCNO3(i)=fxRCNO3(i)+  0.0700;fzRCNO3(i)=fzRCNO3(i)+  0.0800;fyRPNO3(i)=fyRPNO3(i)+  1.0000;fzRDNO3(i)=fzRDNO3(i)+  0.1900;fyROOH(i)=fyROOH(i)+  0.0400;fyRUOOH(i)=fyRUOOH(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  1.4000;fTRPRXN(i)=fTRPRXN(i)+  1.0000;

% 609, <629>
i=i+1;
Rnames{ 609} = 'TERP_N3 = 0.10000*OH +  0.04000*HO2 +  0.10000*RCNO3 +  0.01000*HPCRB ';
k(:,i) = (  1.2700E+00 ); 
Gstr{i,   1}='TERP_N3';
fTERP_N3(i)=fTERP_N3(i)-1.0;
fOH(i)=fOH(i)+  0.1000;fHO2(i)=fHO2(i)+  0.0400;fRCNO3(i)=fRCNO3(i)+  0.1000;fHPCRB(i)=fHPCRB(i)+  0.0100;

% 610, <630>
i=i+1;
Rnames{ 610} = 'TERP_N3 + NO = NO +  0.06000*xHO2 +  0.17000*RO2C +  0.05000*RO2XC +  0.03000*xR2CO3 +  0.01000*xHCHO +  0.01000*xACET +  0.06000*xRCNO3 +  0.05000*zRCNO3 +  0.01000*yRPNO3 +  0.01000*yROOH +  0.01000*yHPCRB +  0.01000*CO +  0.22000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='TERP_N3';Gstr{i,   2}='NO';
fTERP_N3(i)=fTERP_N3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0600;fRO2C(i)=fRO2C(i)+  0.1700;fRO2XC(i)=fRO2XC(i)+  0.0500;fxR2CO3(i)=fxR2CO3(i)+  0.0300;fxHCHO(i)=fxHCHO(i)+  0.0100;fxACET(i)=fxACET(i)+  0.0100;fxRCNO3(i)=fxRCNO3(i)+  0.0600;fzRCNO3(i)=fzRCNO3(i)+  0.0500;fyRPNO3(i)=fyRPNO3(i)+  0.0100;fyROOH(i)=fyROOH(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.0100;fCO(i)=fCO(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  0.2200;

% 611, <TPOP>
i=i+1;
Rnames{ 611} = 'TERP + O3P = 0.16000*RCHO +  0.04000*OLEA2 +  0.18000*KET2 +  0.01000*LVKS +  0.27000*OLEP +  0.18000*ALK3 +  0.08000*ALK4 +  0.08000*ALK5 + TRPRXN ';
k(:,i) = (  4.2400E-11 ); 
Gstr{i,   1}='TERP';Gstr{i,   2}='O3P';
fTERP(i)=fTERP(i)-1.0;fO3P(i)=fO3P(i)-1.0;
fRCHO(i)=fRCHO(i)+  0.1600;fOLEA2(i)=fOLEA2(i)+  0.0400;fKET2(i)=fKET2(i)+  0.1800;fLVKS(i)=fLVKS(i)+  0.0100;fOLEP(i)=fOLEP(i)+  0.2700;fALK3(i)=fALK3(i)+  0.1800;fALK4(i)=fALK4(i)+  0.0800;fALK5(i)=fALK5(i)+  0.0800;fTRPRXN(i)=fTRPRXN(i)+  1.0000;

% 612, <SQOH>
i=i+1;
Rnames{ 612} = 'SESQ + OH = SESQ_OH +  0.05000*OH +  0.05000*HO2 +  0.47000*xHO2 +  0.77000*RO2C +  0.23000*RO2XC +  0.02000*xHCHO +  0.44000*xOLEA2 +  0.05000*OLEP +  0.02000*xOLEP +  0.03000*zRCNO3 +  0.18000*zRHNO3 +  0.84000*yRUOOH +  0.02000*HPCRB +  0.09000*yHPCRB + SumRO2 + SESQRXN ';
k(:,i) = (  2.0000E-10 ); 
Gstr{i,   1}='SESQ';Gstr{i,   2}='OH';
fSESQ(i)=fSESQ(i)-1.0;fOH(i)=fOH(i)-1.0;
fSESQ_OH(i)=fSESQ_OH(i)+  1.0000;fOH(i)=fOH(i)+  0.0500;fHO2(i)=fHO2(i)+  0.0500;fxHO2(i)=fxHO2(i)+  0.4700;fRO2C(i)=fRO2C(i)+  0.7700;fRO2XC(i)=fRO2XC(i)+  0.2300;fxHCHO(i)=fxHCHO(i)+  0.0200;fxOLEA2(i)=fxOLEA2(i)+  0.4400;fOLEP(i)=fOLEP(i)+  0.0500;fxOLEP(i)=fxOLEP(i)+  0.0200;fzRCNO3(i)=fzRCNO3(i)+  0.0300;fzRHNO3(i)=fzRHNO3(i)+  0.1800;fyRUOOH(i)=fyRUOOH(i)+  0.8400;fHPCRB(i)=fHPCRB(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.0900;fSumRO2(i)=fSumRO2(i)+  1.0000;fSESQRXN(i)=fSESQRXN(i)+  1.0000;

% 613, <633>
i=i+1;
Rnames{ 613} = 'SESQ_OH = 0.07000*OH +  0.13000*HO2 +  0.01000*OLEP +  0.01000*zRPNO3 +  0.01000*OTHN +  0.12000*HPCRB ';
k(:,i) = (  5.5800E+00 ); 
Gstr{i,   1}='SESQ_OH';
fSESQ_OH(i)=fSESQ_OH(i)-1.0;
fOH(i)=fOH(i)+  0.0700;fHO2(i)=fHO2(i)+  0.1300;fOLEP(i)=fOLEP(i)+  0.0100;fzRPNO3(i)=fzRPNO3(i)+  0.0100;fOTHN(i)=fOTHN(i)+  0.0100;fHPCRB(i)=fHPCRB(i)+  0.1200;

% 614, <634>
i=i+1;
Rnames{ 614} = 'SESQ_OH + NO = NO +  0.08000*xHO2 +  0.32000*RO2C +  0.12000*RO2XC +  0.10000*xHCHO +  0.08000*xOLEA2 +  0.10000*xACET +  0.01000*zRCNO3 +  0.12000*zRHNO3 +  0.01000*yROOH +  0.24000*yRUOOH +  0.21000*yHPCRB +  0.44000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='SESQ_OH';Gstr{i,   2}='NO';
fSESQ_OH(i)=fSESQ_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0800;fRO2C(i)=fRO2C(i)+  0.3200;fRO2XC(i)=fRO2XC(i)+  0.1200;fxHCHO(i)=fxHCHO(i)+  0.1000;fxOLEA2(i)=fxOLEA2(i)+  0.0800;fxACET(i)=fxACET(i)+  0.1000;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fzRHNO3(i)=fzRHNO3(i)+  0.1200;fyROOH(i)=fyROOH(i)+  0.0100;fyRUOOH(i)=fyRUOOH(i)+  0.2400;fyHPCRB(i)=fyHPCRB(i)+  0.2100;fSumRO2(i)=fSumRO2(i)+  0.4400;

% 615, <SQO3>
i=i+1;
Rnames{ 615} = 'SESQ + O3 = SESQ_O3 +  0.66000*OH +  0.01000*HO2 +  0.19000*RO2C +  0.07000*RO2XC +  0.15000*xMACO3 +  0.33000*RCHO2 +  0.01000*HCHO +  0.15000*xHCHO +  0.01000*OLEP +  0.07000*zRCNO3 +  0.01000*HPCRB +  0.22000*yHPCRB +  0.26000*SumRO2 + SESQRXN ';
k(:,i) = (  3.1400E-16 ); 
Gstr{i,   1}='SESQ';Gstr{i,   2}='O3';
fSESQ(i)=fSESQ(i)-1.0;fO3(i)=fO3(i)-1.0;
fSESQ_O3(i)=fSESQ_O3(i)+  1.0000;fOH(i)=fOH(i)+  0.6600;fHO2(i)=fHO2(i)+  0.0100;fRO2C(i)=fRO2C(i)+  0.1900;fRO2XC(i)=fRO2XC(i)+  0.0700;fxMACO3(i)=fxMACO3(i)+  0.1500;fRCHO2(i)=fRCHO2(i)+  0.3300;fHCHO(i)=fHCHO(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.1500;fOLEP(i)=fOLEP(i)+  0.0100;fzRCNO3(i)=fzRCNO3(i)+  0.0700;fHPCRB(i)=fHPCRB(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.2200;fSumRO2(i)=fSumRO2(i)+  0.2600;fSESQRXN(i)=fSESQRXN(i)+  1.0000;

% 616, <636>
i=i+1;
Rnames{ 616} = 'SESQ_O3 = 0.07000*OH +  0.36000*HO2 +  0.06000*OTHN +  0.01000*HPCRB ';
k(:,i) = (  4.3200E+00 ); 
Gstr{i,   1}='SESQ_O3';
fSESQ_O3(i)=fSESQ_O3(i)-1.0;
fOH(i)=fOH(i)+  0.0700;fHO2(i)=fHO2(i)+  0.3600;fOTHN(i)=fOTHN(i)+  0.0600;fHPCRB(i)=fHPCRB(i)+  0.0100;

% 617, <637>
i=i+1;
Rnames{ 617} = 'SESQ_O3 + NO = NO +  0.01000*xOH +  0.43000*RO2C +  0.17000*RO2XC +  0.17000*xMECO3 +  0.02000*xHCHO +  0.02000*xRCHO +  0.22000*xOLEA2 +  0.17000*zRCNO3 +  0.01000*xHPCRB +  0.51000*yHPCRB +  0.04000*CO +  0.60000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='SESQ_O3';Gstr{i,   2}='NO';
fSESQ_O3(i)=fSESQ_O3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxOH(i)=fxOH(i)+  0.0100;fRO2C(i)=fRO2C(i)+  0.4300;fRO2XC(i)=fRO2XC(i)+  0.1700;fxMECO3(i)=fxMECO3(i)+  0.1700;fxHCHO(i)=fxHCHO(i)+  0.0200;fxRCHO(i)=fxRCHO(i)+  0.0200;fxOLEA2(i)=fxOLEA2(i)+  0.2200;fzRCNO3(i)=fzRCNO3(i)+  0.1700;fxHPCRB(i)=fxHPCRB(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.5100;fCO(i)=fCO(i)+  0.0400;fSumRO2(i)=fSumRO2(i)+  0.6000;

% 618, <SQN3>
i=i+1;
Rnames{ 618} = 'SESQ + NO3 = SESQ_N3 +  0.74000*xNO2 +  0.02000*OH +  0.82000*RO2C +  0.22000*RO2XC +  0.74000*xOLEA2 +  0.02000*RCNO3 +  0.01000*zRCNO3 + yRPNO3 +  0.21000*zRDNO3 +  1.04000*SumRO2 + SESQRXN ';
k(:,i) = (  1.9000E-11 ); 
Gstr{i,   1}='SESQ';Gstr{i,   2}='NO3';
fSESQ(i)=fSESQ(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fSESQ_N3(i)=fSESQ_N3(i)+  1.0000;fxNO2(i)=fxNO2(i)+  0.7400;fOH(i)=fOH(i)+  0.0200;fRO2C(i)=fRO2C(i)+  0.8200;fRO2XC(i)=fRO2XC(i)+  0.2200;fxOLEA2(i)=fxOLEA2(i)+  0.7400;fRCNO3(i)=fRCNO3(i)+  0.0200;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fyRPNO3(i)=fyRPNO3(i)+  1.0000;fzRDNO3(i)=fzRDNO3(i)+  0.2100;fSumRO2(i)=fSumRO2(i)+  1.0400;fSESQRXN(i)=fSESQRXN(i)+  1.0000;

% 619, <639>
i=i+1;
Rnames{ 619} = 'SESQ_N3 = 0.01000*OH +  0.01000*HO2 +  0.01000*RCNO3 ';
k(:,i) = (  2.5300E+00 ); 
Gstr{i,   1}='SESQ_N3';
fSESQ_N3(i)=fSESQ_N3(i)-1.0;
fOH(i)=fOH(i)+  0.0100;fHO2(i)=fHO2(i)+  0.0100;fRCNO3(i)=fRCNO3(i)+  0.0100;

% 620, <640>
i=i+1;
Rnames{ 620} = 'SESQ_N3 + NO = NO +  0.04000*RO2C +  0.01000*RO2XC +  0.01000*zRCNO3 +  0.05000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='SESQ_N3';Gstr{i,   2}='NO';
fSESQ_N3(i)=fSESQ_N3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fRO2C(i)=fRO2C(i)+  0.0400;fRO2XC(i)=fRO2XC(i)+  0.0100;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  0.0500;

% 621, <SQOP>
i=i+1;
Rnames{ 621} = 'SESQ + O3P = 0.13000*OLEA2 +  0.87000*OLEP + SESQRXN ';
k(:,i) = (  6.8500E-11 ); 
Gstr{i,   1}='SESQ';Gstr{i,   2}='O3P';
fSESQ(i)=fSESQ(i)-1.0;fO3P(i)=fO3P(i)-1.0;
fOLEA2(i)=fOLEA2(i)+  0.1300;fOLEP(i)=fOLEP(i)+  0.8700;fSESQRXN(i)=fSESQRXN(i)+  1.0000;

% 622, <BXOH>
i=i+1;
Rnames{ 622} = 'BENX + OH = 0.69000*HO2 +  0.28000*xHO2 +  0.28000*RO2C +  0.04000*RO2XC +  0.12000*OLEA2 +  0.28000*xGLY +  0.28000*xBUDAL +  0.57000*PHEN +  0.04000*zRANO3 +  0.31000*yRAOOH +  0.32000*SumRO2 ';
k(:,i) = (  1.2100E-12 ); 
Gstr{i,   1}='BENX';Gstr{i,   2}='OH';
fBENX(i)=fBENX(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.6900;fxHO2(i)=fxHO2(i)+  0.2800;fRO2C(i)=fRO2C(i)+  0.2800;fRO2XC(i)=fRO2XC(i)+  0.0400;fOLEA2(i)=fOLEA2(i)+  0.1200;fxGLY(i)=fxGLY(i)+  0.2800;fxBUDAL(i)=fxBUDAL(i)+  0.2800;fPHEN(i)=fPHEN(i)+  0.5700;fzRANO3(i)=fzRANO3(i)+  0.0400;fyRAOOH(i)=fyRAOOH(i)+  0.3100;fSumRO2(i)=fSumRO2(i)+  0.3200;

% 623, <B1OH>
i=i+1;
Rnames{ 623} = 'ARO1 + OH = 0.27000*HO2 +  0.48000*xHO2 +  0.75000*RO2C +  0.21000*RO2XC +  0.05000*xETO2 +  0.01000*xHCHO +  0.06000*xMECHO +  0.11000*xETCHO +  0.01000*xRCHO +  0.01000*OLEA1 +  0.13000*OLEA2 +  0.13000*xGLY +  0.13000*xMGLY +  0.13000*xBUDAL +  0.01000*xAFG1 +  0.12000*xAFG2A +  0.20000*xBALD +  0.12000*XYNL +  0.03000*zR1NO3 +  0.11000*zR2NO3 +  0.01000*zRHNO3 +  0.06000*zRANO3 +  0.64000*yROOH +  0.32000*yRAOOH +  0.04000*ARO1 +  3.11000*NROG +  0.96000*SumRO2 + TOLRO2 ';
k(:,i) = (  7.6900E-12 ); 
Gstr{i,   1}='ARO1';Gstr{i,   2}='OH';
fARO1(i)=fARO1(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.2700;fxHO2(i)=fxHO2(i)+  0.4800;fRO2C(i)=fRO2C(i)+  0.7500;fRO2XC(i)=fRO2XC(i)+  0.2100;fxETO2(i)=fxETO2(i)+  0.0500;fxHCHO(i)=fxHCHO(i)+  0.0100;fxMECHO(i)=fxMECHO(i)+  0.0600;fxETCHO(i)=fxETCHO(i)+  0.1100;fxRCHO(i)=fxRCHO(i)+  0.0100;fOLEA1(i)=fOLEA1(i)+  0.0100;fOLEA2(i)=fOLEA2(i)+  0.1300;fxGLY(i)=fxGLY(i)+  0.1300;fxMGLY(i)=fxMGLY(i)+  0.1300;fxBUDAL(i)=fxBUDAL(i)+  0.1300;fxAFG1(i)=fxAFG1(i)+  0.0100;fxAFG2A(i)=fxAFG2A(i)+  0.1200;fxBALD(i)=fxBALD(i)+  0.2000;fXYNL(i)=fXYNL(i)+  0.1200;fzR1NO3(i)=fzR1NO3(i)+  0.0300;fzR2NO3(i)=fzR2NO3(i)+  0.1100;fzRHNO3(i)=fzRHNO3(i)+  0.0100;fzRANO3(i)=fzRANO3(i)+  0.0600;fyROOH(i)=fyROOH(i)+  0.6400;fyRAOOH(i)=fyRAOOH(i)+  0.3200;fARO1(i)=fARO1(i)+  0.0400;fNROG(i)=fNROG(i)+  3.1100;fSumRO2(i)=fSumRO2(i)+  0.9600;fTOLRO2(i)=fTOLRO2(i)+  1.0000;

% 624, <B2OH>
i=i+1;
Rnames{ 624} = 'ARO2 + OH = ARO2_OH +  0.29000*HO2 +  0.56000*xHO2 +  0.60000*RO2C +  0.14000*RO2XC +  0.03000*OLEA1 +  0.13000*OLEA2 +  0.07000*xGLY +  0.39000*xMGLY +  0.03000*xBACL +  0.03000*xBUDAL +  0.07000*xAFG1 +  0.30000*xAFG2A +  0.04000*xAFG2B +  0.04000*BALD +  0.03000*xBALD +  0.01000*xKET2 +  0.01000*LVKS +  0.04000*xAFG3 +  0.08000*XYNL +  0.02000*zR2NO3 +  0.01000*zRCNO3 +  0.11000*zRANO3 +  0.10000*yROOH +  0.59000*yRAOOH +  0.04000*yHPCRB +  0.03000*xBENX +  1.38000*NROG +  0.74000*SumRO2 + XYLRO2 ';
k(:,i) = (  2.1300E-11 ); 
Gstr{i,   1}='ARO2';Gstr{i,   2}='OH';
fARO2(i)=fARO2(i)-1.0;fOH(i)=fOH(i)-1.0;
fARO2_OH(i)=fARO2_OH(i)+  1.0000;fHO2(i)=fHO2(i)+  0.2900;fxHO2(i)=fxHO2(i)+  0.5600;fRO2C(i)=fRO2C(i)+  0.6000;fRO2XC(i)=fRO2XC(i)+  0.1400;fOLEA1(i)=fOLEA1(i)+  0.0300;fOLEA2(i)=fOLEA2(i)+  0.1300;fxGLY(i)=fxGLY(i)+  0.0700;fxMGLY(i)=fxMGLY(i)+  0.3900;fxBACL(i)=fxBACL(i)+  0.0300;fxBUDAL(i)=fxBUDAL(i)+  0.0300;fxAFG1(i)=fxAFG1(i)+  0.0700;fxAFG2A(i)=fxAFG2A(i)+  0.3000;fxAFG2B(i)=fxAFG2B(i)+  0.0400;fBALD(i)=fBALD(i)+  0.0400;fxBALD(i)=fxBALD(i)+  0.0300;fxKET2(i)=fxKET2(i)+  0.0100;fLVKS(i)=fLVKS(i)+  0.0100;fxAFG3(i)=fxAFG3(i)+  0.0400;fXYNL(i)=fXYNL(i)+  0.0800;fzR2NO3(i)=fzR2NO3(i)+  0.0200;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fzRANO3(i)=fzRANO3(i)+  0.1100;fyROOH(i)=fyROOH(i)+  0.1000;fyRAOOH(i)=fyRAOOH(i)+  0.5900;fyHPCRB(i)=fyHPCRB(i)+  0.0400;fxBENX(i)=fxBENX(i)+  0.0300;fNROG(i)=fNROG(i)+  1.3800;fSumRO2(i)=fSumRO2(i)+  0.7400;fXYLRO2(i)=fXYLRO2(i)+  1.0000;

% 625, <645>
i=i+1;
Rnames{ 625} = 'ARO2_OH = 0.01000*HPCRB ';
k(:,i) = (  1.7600E-01 ); 
Gstr{i,   1}='ARO2_OH';
fARO2_OH(i)=fARO2_OH(i)-1.0;
fHPCRB(i)=fHPCRB(i)+  0.0100;

% 626, <646>
i=i+1;
Rnames{ 626} = 'ARO2_OH + NO = NO +  0.01000*RO2C +  0.01000*yHPCRB +  0.32000*NROG +  0.01000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='ARO2_OH';Gstr{i,   2}='NO';
fARO2_OH(i)=fARO2_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fRO2C(i)=fRO2C(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.0100;fNROG(i)=fNROG(i)+  0.3200;fSumRO2(i)=fSumRO2(i)+  0.0100;

% 627, <FUOH>
i=i+1;
Rnames{ 627} = 'FURNS + OH = 0.75000*HO2 +  0.24000*xHO2 +  0.24000*RO2C +  0.01000*RO2XC +  0.07000*xRCHO +  0.03000*xOLEA1 +  0.75000*BUDAL +  0.14000*xOLEP +  0.01000*zRHNO3 +  0.15000*yRUOOH +  0.01000*yHPCRB +  0.08000*CO +  0.25000*SumRO2 ';
k(:,i) = (  3.8400E-11 ); 
Gstr{i,   1}='FURNS';Gstr{i,   2}='OH';
fFURNS(i)=fFURNS(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.7500;fxHO2(i)=fxHO2(i)+  0.2400;fRO2C(i)=fRO2C(i)+  0.2400;fRO2XC(i)=fRO2XC(i)+  0.0100;fxRCHO(i)=fxRCHO(i)+  0.0700;fxOLEA1(i)=fxOLEA1(i)+  0.0300;fBUDAL(i)=fBUDAL(i)+  0.7500;fxOLEP(i)=fxOLEP(i)+  0.1400;fzRHNO3(i)=fzRHNO3(i)+  0.0100;fyRUOOH(i)=fyRUOOH(i)+  0.1500;fyHPCRB(i)=fyHPCRB(i)+  0.0100;fCO(i)=fCO(i)+  0.0800;fSumRO2(i)=fSumRO2(i)+  0.2500;

% 628, <FUO3>
i=i+1;
Rnames{ 628} = 'FURNS + O3 = 0.39000*HO2 +  0.38000*xHO2 +  0.38000*RO2C +  0.01000*RO2XC +  0.40000*RCHO2 +  0.14000*OLEA1 +  0.05000*xGLY +  0.01000*zRCNO3 +  0.07000*HPCRB +  0.25000*yHPCRB +  0.33000*ALK1 +  0.19000*CO2 +  0.79000*CO +  0.39000*SumRO2 ';
k(:,i) = (  2.4000E-18 ); 
Gstr{i,   1}='FURNS';Gstr{i,   2}='O3';
fFURNS(i)=fFURNS(i)-1.0;fO3(i)=fO3(i)-1.0;
fHO2(i)=fHO2(i)+  0.3900;fxHO2(i)=fxHO2(i)+  0.3800;fRO2C(i)=fRO2C(i)+  0.3800;fRO2XC(i)=fRO2XC(i)+  0.0100;fRCHO2(i)=fRCHO2(i)+  0.4000;fOLEA1(i)=fOLEA1(i)+  0.1400;fxGLY(i)=fxGLY(i)+  0.0500;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fHPCRB(i)=fHPCRB(i)+  0.0700;fyHPCRB(i)=fyHPCRB(i)+  0.2500;fALK1(i)=fALK1(i)+  0.3300;fCO2(i)=fCO2(i)+  0.1900;fCO(i)=fCO(i)+  0.7900;fSumRO2(i)=fSumRO2(i)+  0.3900;

% 629, <FUN3>
i=i+1;
Rnames{ 629} = 'FURNS + NO3 = 0.08000*xNO2 +  0.87000*xHO2 +  0.95000*RO2C +  0.05000*RO2XC +  0.07000*xOLEA1 +  0.01000*xOLEP +  0.87000*xRCNO3 +  0.63000*yRPNO3 +  0.05000*zRDNO3 +  0.28000*CO + SumRO2 ';
k(:,i) = (  1.2000E-12 ); 
Gstr{i,   1}='FURNS';Gstr{i,   2}='NO3';
fFURNS(i)=fFURNS(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fxNO2(i)=fxNO2(i)+  0.0800;fxHO2(i)=fxHO2(i)+  0.8700;fRO2C(i)=fRO2C(i)+  0.9500;fRO2XC(i)=fRO2XC(i)+  0.0500;fxOLEA1(i)=fxOLEA1(i)+  0.0700;fxOLEP(i)=fxOLEP(i)+  0.0100;fxRCNO3(i)=fxRCNO3(i)+  0.8700;fyRPNO3(i)=fyRPNO3(i)+  0.6300;fzRDNO3(i)=fzRDNO3(i)+  0.0500;fCO(i)=fCO(i)+  0.2800;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 630, <STOH>
i=i+1;
Rnames{ 630} = 'STYRS + OH = 0.06000*HO2 +  0.79000*xHO2 +  0.79000*RO2C +  0.15000*RO2XC +  0.74000*xHCHO +  0.03000*OLEA2 +  0.02000*xGLY +  0.02000*xMGLY +  0.03000*xBUDAL +  0.02000*xAFG2A +  0.74000*xBALD +  0.02000*XYNL +  0.14000*zRHNO3 +  0.01000*zRANO3 +  0.88000*yROOH +  0.06000*yRAOOH +  0.94000*SumRO2 + XYLRO2 ';
k(:,i) = (  5.8000E-11 ); 
Gstr{i,   1}='STYRS';Gstr{i,   2}='OH';
fSTYRS(i)=fSTYRS(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.0600;fxHO2(i)=fxHO2(i)+  0.7900;fRO2C(i)=fRO2C(i)+  0.7900;fRO2XC(i)=fRO2XC(i)+  0.1500;fxHCHO(i)=fxHCHO(i)+  0.7400;fOLEA2(i)=fOLEA2(i)+  0.0300;fxGLY(i)=fxGLY(i)+  0.0200;fxMGLY(i)=fxMGLY(i)+  0.0200;fxBUDAL(i)=fxBUDAL(i)+  0.0300;fxAFG2A(i)=fxAFG2A(i)+  0.0200;fxBALD(i)=fxBALD(i)+  0.7400;fXYNL(i)=fXYNL(i)+  0.0200;fzRHNO3(i)=fzRHNO3(i)+  0.1400;fzRANO3(i)=fzRANO3(i)+  0.0100;fyROOH(i)=fyROOH(i)+  0.8800;fyRAOOH(i)=fyRAOOH(i)+  0.0600;fSumRO2(i)=fSumRO2(i)+  0.9400;fXYLRO2(i)=fXYLRO2(i)+  1.0000;

% 631, <STO3>
i=i+1;
Rnames{ 631} = 'STYRS + O3 = 0.08000*OH +  0.17000*HO2 +  0.03000*RO2C +  0.03000*xBZO +  0.21000*HCHO2 +  0.34000*RCHO2 +  0.50000*HCHO +  0.50000*BALD +  0.05000*PHEN +  0.03000*yROOH +  0.09000*BENZ +  0.23000*CO2 +  0.22000*CO +  0.03000*SumRO2 ';
k(:,i) = (  1.6000E-17 ); 
Gstr{i,   1}='STYRS';Gstr{i,   2}='O3';
fSTYRS(i)=fSTYRS(i)-1.0;fO3(i)=fO3(i)-1.0;
fOH(i)=fOH(i)+  0.0800;fHO2(i)=fHO2(i)+  0.1700;fRO2C(i)=fRO2C(i)+  0.0300;fxBZO(i)=fxBZO(i)+  0.0300;fHCHO2(i)=fHCHO2(i)+  0.2100;fRCHO2(i)=fRCHO2(i)+  0.3400;fHCHO(i)=fHCHO(i)+  0.5000;fBALD(i)=fBALD(i)+  0.5000;fPHEN(i)=fPHEN(i)+  0.0500;fyROOH(i)=fyROOH(i)+  0.0300;fBENZ(i)=fBENZ(i)+  0.0900;fCO2(i)=fCO2(i)+  0.2300;fCO(i)=fCO(i)+  0.2200;fSumRO2(i)=fSumRO2(i)+  0.0300;

% 632, <AMOH>
i=i+1;
Rnames{ 632} = 'AMINS + OH = 0.02000*HO2 +  0.96000*xHO2 +  0.97000*RO2C +  0.02000*RO2XC +  0.01000*xMEO2 +  0.08000*xHCHO +  0.02000*RCHO +  0.02000*zR2NO3 +  0.99000*yROOH +  0.97000*xAMINS +  0.99000*SumRO2 ';
k(:,i) = (  4.3500E-11 ); 
Gstr{i,   1}='AMINS';Gstr{i,   2}='OH';
fAMINS(i)=fAMINS(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.0200;fxHO2(i)=fxHO2(i)+  0.9600;fRO2C(i)=fRO2C(i)+  0.9700;fRO2XC(i)=fRO2XC(i)+  0.0200;fxMEO2(i)=fxMEO2(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.0800;fRCHO(i)=fRCHO(i)+  0.0200;fzR2NO3(i)=fzR2NO3(i)+  0.0200;fyROOH(i)=fyROOH(i)+  0.9900;fxAMINS(i)=fxAMINS(i)+  0.9700;fSumRO2(i)=fSumRO2(i)+  0.9900;

% 633, <AMO3>
i=i+1;
Rnames{ 633} = 'AMINS + O3 = 0.61000*AMINS + 29.54000*NROG ';
k(:,i) = (  3.0900E-18 ); 
Gstr{i,   1}='AMINS';Gstr{i,   2}='O3';
fAMINS(i)=fAMINS(i)-1.0;fO3(i)=fO3(i)-1.0;
fAMINS(i)=fAMINS(i)+  0.6100;fNROG(i)=fNROG(i)+ 29.5400;

% 634, <TAOH>
i=i+1;
Rnames{ 634} = 'TAMNS + OH = 0.06000*RO2C +  0.03000*xMEO2 +  0.03000*xHCHO +  0.06000*yROOH +  0.97000*PNAMIN +  0.03000*xAMINS +  0.06000*SumRO2 ';
k(:,i) = (  1.0100E-11 ); 
Gstr{i,   1}='TAMNS';Gstr{i,   2}='OH';
fTAMNS(i)=fTAMNS(i)-1.0;fOH(i)=fOH(i)-1.0;
fRO2C(i)=fRO2C(i)+  0.0600;fxMEO2(i)=fxMEO2(i)+  0.0300;fxHCHO(i)=fxHCHO(i)+  0.0300;fyROOH(i)=fyROOH(i)+  0.0600;fPNAMIN(i)=fPNAMIN(i)+  0.9700;fxAMINS(i)=fxAMINS(i)+  0.0300;fSumRO2(i)=fSumRO2(i)+  0.0600;

% 635, <A4OH>
i=i+1;
Rnames{ 635} = 'RCHO + OH = 0.01000*OH +  0.01000*xOH +  0.10000*HO2 +  0.07000*xHO2 +  0.09000*RO2C +  0.01000*RO2XC +  0.80000*R2CO3 +  0.04000*xHCHO +  0.01000*xMECHO +  0.06000*RCHO +  0.02000*xRCHO +  0.05000*MGLY +  0.01000*xACET +  0.03000*xPACID +  0.01000*zRCNO3 +  0.04000*yHPCRB +  0.01000*ALK4 +  0.14000*NROG +  0.01000*CO2 +  0.02000*CO +  0.10000*SumRO2 +  0.80000*SumRCO3 ';
k(:,i) = (  3.2900E-11 ); 
Gstr{i,   1}='RCHO';Gstr{i,   2}='OH';
fRCHO(i)=fRCHO(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  0.0100;fxOH(i)=fxOH(i)+  0.0100;fHO2(i)=fHO2(i)+  0.1000;fxHO2(i)=fxHO2(i)+  0.0700;fRO2C(i)=fRO2C(i)+  0.0900;fRO2XC(i)=fRO2XC(i)+  0.0100;fR2CO3(i)=fR2CO3(i)+  0.8000;fxHCHO(i)=fxHCHO(i)+  0.0400;fxMECHO(i)=fxMECHO(i)+  0.0100;fRCHO(i)=fRCHO(i)+  0.0600;fxRCHO(i)=fxRCHO(i)+  0.0200;fMGLY(i)=fMGLY(i)+  0.0500;fxACET(i)=fxACET(i)+  0.0100;fxPACID(i)=fxPACID(i)+  0.0300;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.0400;fALK4(i)=fALK4(i)+  0.0100;fNROG(i)=fNROG(i)+  0.1400;fCO2(i)=fCO2(i)+  0.0100;fCO(i)=fCO(i)+  0.0200;fSumRO2(i)=fSumRO2(i)+  0.1000;fSumRCO3(i)=fSumRCO3(i)+  0.8000;

% 636, <A4N3>
i=i+1;
Rnames{ 636} = 'RCHO + NO3 = HNO3 +  0.07000*HO2 +  0.01000*RO2C +  0.92000*R2CO3 +  0.04000*RCHO +  0.03000*MGLY +  0.01000*SumRO2 +  0.92000*SumRCO3 ';
k(:,i) = (  2.2300E-14 ); 
Gstr{i,   1}='RCHO';Gstr{i,   2}='NO3';
fRCHO(i)=fRCHO(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;fHO2(i)=fHO2(i)+  0.0700;fRO2C(i)=fRO2C(i)+  0.0100;fR2CO3(i)=fR2CO3(i)+  0.9200;fRCHO(i)=fRCHO(i)+  0.0400;fMGLY(i)=fMGLY(i)+  0.0300;fSumRO2(i)=fSumRO2(i)+  0.0100;fSumRCO3(i)=fSumRCO3(i)+  0.9200;

% 637, <A4HV>
i=i+1;
Rnames{ 637} = 'RCHO = 1.40000*HO2 +  0.51000*xHO2 +  0.65000*RO2C +  0.04000*RO2XC +  0.01000*xETO2 +  0.05000*xMECO3 +  0.15000*xHCHO +  0.17000*MECHO +  0.05000*xMECHO +  0.05000*xETCHO +  0.01000*RCHO +  0.23000*xRCHO +  0.22000*GLCHO +  0.01000*xGLCHO +  0.01000*xMGLY +  0.07000*xACET +  0.01000*xMEK +  0.06000*xKET2 +  0.01000*zR1NO3 +  0.03000*zRHNO3 +  0.59000*yROOH +  0.08000*yHPCRB +  0.03000*ALK4 +  0.08000*NROG + CO +  0.69000*SumRO2 ';
k(:,i) = (JC2CHOabs ); 
Gstr{i,   1}='RCHO';
fRCHO(i)=fRCHO(i)-1.0;
fHO2(i)=fHO2(i)+  1.4000;fxHO2(i)=fxHO2(i)+  0.5100;fRO2C(i)=fRO2C(i)+  0.6500;fRO2XC(i)=fRO2XC(i)+  0.0400;fxETO2(i)=fxETO2(i)+  0.0100;fxMECO3(i)=fxMECO3(i)+  0.0500;fxHCHO(i)=fxHCHO(i)+  0.1500;fMECHO(i)=fMECHO(i)+  0.1700;fxMECHO(i)=fxMECHO(i)+  0.0500;fxETCHO(i)=fxETCHO(i)+  0.0500;fRCHO(i)=fRCHO(i)+  0.0100;fxRCHO(i)=fxRCHO(i)+  0.2300;fGLCHO(i)=fGLCHO(i)+  0.2200;fxGLCHO(i)=fxGLCHO(i)+  0.0100;fxMGLY(i)=fxMGLY(i)+  0.0100;fxACET(i)=fxACET(i)+  0.0700;fxMEK(i)=fxMEK(i)+  0.0100;fxKET2(i)=fxKET2(i)+  0.0600;fzR1NO3(i)=fzR1NO3(i)+  0.0100;fzRHNO3(i)=fzRHNO3(i)+  0.0300;fyROOH(i)=fyROOH(i)+  0.5900;fyHPCRB(i)=fyHPCRB(i)+  0.0800;fALK4(i)=fALK4(i)+  0.0300;fNROG(i)=fNROG(i)+  0.0800;fCO(i)=fCO(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  0.6900;

% 638, <A5OH>
i=i+1;
Rnames{ 638} = 'OLEA1 + OH = OLEA1_OH +  0.22000*HO2 +  0.26000*xHO2 +  0.59000*RO2C +  0.05000*RO2XC +  0.03000*MACO3 +  0.01000*xHCHO +  0.06000*RCHO +  0.05000*xGLY +  0.20000*xGLCHO +  0.05000*AFG1 +  0.03000*AFG2A +  0.08000*AFG2B +  0.17000*xKET2 +  0.02000*xPACID +  0.01000*HPCRB +  0.05000*yHPCRB +  0.65000*SumRO2 +  0.03000*SumRCO3 ';
k(:,i) = (  5.0600E-11 ); 
Gstr{i,   1}='OLEA1';Gstr{i,   2}='OH';
fOLEA1(i)=fOLEA1(i)-1.0;fOH(i)=fOH(i)-1.0;
fOLEA1_OH(i)=fOLEA1_OH(i)+  1.0000;fHO2(i)=fHO2(i)+  0.2200;fxHO2(i)=fxHO2(i)+  0.2600;fRO2C(i)=fRO2C(i)+  0.5900;fRO2XC(i)=fRO2XC(i)+  0.0500;fMACO3(i)=fMACO3(i)+  0.0300;fxHCHO(i)=fxHCHO(i)+  0.0100;fRCHO(i)=fRCHO(i)+  0.0600;fxGLY(i)=fxGLY(i)+  0.0500;fxGLCHO(i)=fxGLCHO(i)+  0.2000;fAFG1(i)=fAFG1(i)+  0.0500;fAFG2A(i)=fAFG2A(i)+  0.0300;fAFG2B(i)=fAFG2B(i)+  0.0800;fxKET2(i)=fxKET2(i)+  0.1700;fxPACID(i)=fxPACID(i)+  0.0200;fHPCRB(i)=fHPCRB(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.0500;fSumRO2(i)=fSumRO2(i)+  0.6500;fSumRCO3(i)=fSumRCO3(i)+  0.0300;

% 639, <659>
i=i+1;
Rnames{ 639} = 'OLEA1_OH = 0.33000*xOH +  0.10000*HO2 +  0.20000*xKET2 +  0.19000*xPACID +  0.05000*zRCNO3 +  0.10000*HPCRB +  0.33000*CO2 ';
k(:,i) = (  7.5200E-01 ); 
Gstr{i,   1}='OLEA1_OH';
fOLEA1_OH(i)=fOLEA1_OH(i)-1.0;
fxOH(i)=fxOH(i)+  0.3300;fHO2(i)=fHO2(i)+  0.1000;fxKET2(i)=fxKET2(i)+  0.2000;fxPACID(i)=fxPACID(i)+  0.1900;fzRCNO3(i)=fzRCNO3(i)+  0.0500;fHPCRB(i)=fHPCRB(i)+  0.1000;fCO2(i)=fCO2(i)+  0.3300;

% 640, <660>
i=i+1;
Rnames{ 640} = 'OLEA1_OH + NO = NO +  0.42000*xHO2 +  0.09000*RO2C +  0.01000*RO2XC +  0.09000*xGLY +  0.20000*xGLCHO +  0.48000*xMGLY +  0.09000*xHCOOH +  0.05000*zRHNO3 +  0.60000*yHPCRB +  0.04000*CO +  0.10000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='OLEA1_OH';Gstr{i,   2}='NO';
fOLEA1_OH(i)=fOLEA1_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.4200;fRO2C(i)=fRO2C(i)+  0.0900;fRO2XC(i)=fRO2XC(i)+  0.0100;fxGLY(i)=fxGLY(i)+  0.0900;fxGLCHO(i)=fxGLCHO(i)+  0.2000;fxMGLY(i)=fxMGLY(i)+  0.4800;fxHCOOH(i)=fxHCOOH(i)+  0.0900;fzRHNO3(i)=fzRHNO3(i)+  0.0500;fyHPCRB(i)=fyHPCRB(i)+  0.6000;fCO(i)=fCO(i)+  0.0400;fSumRO2(i)=fSumRO2(i)+  0.1000;

% 641, <A5O3>
i=i+1;
Rnames{ 641} = 'OLEA1 + O3 = 0.62000*OH +  0.01000*xOH +  0.59000*HO2 +  0.02000*xHO2 +  0.12000*RO2C +  0.01000*MECO3 +  0.09000*xR2CO3 +  0.16000*RCHO2 +  0.04000*HCHO +  0.10000*xHCHO +  0.02000*MECHO +  0.42000*GLY +  0.06000*GLCHO +  0.81000*MGLY +  0.02000*KET2 +  0.10000*MEOH +  0.02000*HCOOH +  0.01000*OACID +  0.02000*xPACID +  0.08000*yHPCRB +  0.05000*ALK4 +  0.33000*CO2 +  0.11000*CO +  0.12000*SumRO2 +  0.01000*SumRCO3 ';
k(:,i) = (  3.5000E-18 ); 
Gstr{i,   1}='OLEA1';Gstr{i,   2}='O3';
fOLEA1(i)=fOLEA1(i)-1.0;fO3(i)=fO3(i)-1.0;
fOH(i)=fOH(i)+  0.6200;fxOH(i)=fxOH(i)+  0.0100;fHO2(i)=fHO2(i)+  0.5900;fxHO2(i)=fxHO2(i)+  0.0200;fRO2C(i)=fRO2C(i)+  0.1200;fMECO3(i)=fMECO3(i)+  0.0100;fxR2CO3(i)=fxR2CO3(i)+  0.0900;fRCHO2(i)=fRCHO2(i)+  0.1600;fHCHO(i)=fHCHO(i)+  0.0400;fxHCHO(i)=fxHCHO(i)+  0.1000;fMECHO(i)=fMECHO(i)+  0.0200;fGLY(i)=fGLY(i)+  0.4200;fGLCHO(i)=fGLCHO(i)+  0.0600;fMGLY(i)=fMGLY(i)+  0.8100;fKET2(i)=fKET2(i)+  0.0200;fMEOH(i)=fMEOH(i)+  0.1000;fHCOOH(i)=fHCOOH(i)+  0.0200;fOACID(i)=fOACID(i)+  0.0100;fxPACID(i)=fxPACID(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.0800;fALK4(i)=fALK4(i)+  0.0500;fCO2(i)=fCO2(i)+  0.3300;fCO(i)=fCO(i)+  0.1100;fSumRO2(i)=fSumRO2(i)+  0.1200;fSumRCO3(i)=fSumRCO3(i)+  0.0100;

% 642, <A5N3>
i=i+1;
Rnames{ 642} = 'OLEA1 + NO3 = OLEA1_N3 +  0.56000*xNO2 +  0.04000*HNO3 +  0.22000*HO2 +  0.16000*xHO2 +  0.71000*RO2C +  0.06000*RO2XC +  0.01000*xHCHO +  0.03000*xGLY +  0.53000*xGLCHO +  0.02000*AFG1 +  0.01000*AFG2B +  0.03000*xKET2 +  0.22000*xPACID +  0.19000*RCNO3 +  0.01000*xRCNO3 +  0.02000*zRCNO3 +  0.15000*xRHNO3 +  0.17000*yRPNO3 +  0.02000*zRDNO3 +  0.15000*CO +  0.78000*SumRO2 ';
k(:,i) = (  9.6400E-14 ); 
Gstr{i,   1}='OLEA1';Gstr{i,   2}='NO3';
fOLEA1(i)=fOLEA1(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fOLEA1_N3(i)=fOLEA1_N3(i)+  1.0000;fxNO2(i)=fxNO2(i)+  0.5600;fHNO3(i)=fHNO3(i)+  0.0400;fHO2(i)=fHO2(i)+  0.2200;fxHO2(i)=fxHO2(i)+  0.1600;fRO2C(i)=fRO2C(i)+  0.7100;fRO2XC(i)=fRO2XC(i)+  0.0600;fxHCHO(i)=fxHCHO(i)+  0.0100;fxGLY(i)=fxGLY(i)+  0.0300;fxGLCHO(i)=fxGLCHO(i)+  0.5300;fAFG1(i)=fAFG1(i)+  0.0200;fAFG2B(i)=fAFG2B(i)+  0.0100;fxKET2(i)=fxKET2(i)+  0.0300;fxPACID(i)=fxPACID(i)+  0.2200;fRCNO3(i)=fRCNO3(i)+  0.1900;fxRCNO3(i)=fxRCNO3(i)+  0.0100;fzRCNO3(i)=fzRCNO3(i)+  0.0200;fxRHNO3(i)=fxRHNO3(i)+  0.1500;fyRPNO3(i)=fyRPNO3(i)+  0.1700;fzRDNO3(i)=fzRDNO3(i)+  0.0200;fCO(i)=fCO(i)+  0.1500;fSumRO2(i)=fSumRO2(i)+  0.7800;

% 643, <663>
i=i+1;
Rnames{ 643} = 'OLEA1_N3 = 0.01000*AFG2B +  0.31000*xPACID +  0.03000*zRCNO3 ';
k(:,i) = (  5.5100E+01 ); 
Gstr{i,   1}='OLEA1_N3';
fOLEA1_N3(i)=fOLEA1_N3(i)-1.0;
fAFG2B(i)=fAFG2B(i)+  0.0100;fxPACID(i)=fxPACID(i)+  0.3100;fzRCNO3(i)=fzRCNO3(i)+  0.0300;

% 644, <664>
i=i+1;
Rnames{ 644} = 'OLEA1_N3 + NO = NO +  0.01000*RO2C +  0.31000*xMGLY +  0.34000*yRPNO3 +  0.03000*zRDNO3 +  0.01000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='OLEA1_N3';Gstr{i,   2}='NO';
fOLEA1_N3(i)=fOLEA1_N3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fRO2C(i)=fRO2C(i)+  0.0100;fxMGLY(i)=fxMGLY(i)+  0.3100;fyRPNO3(i)=fyRPNO3(i)+  0.3400;fzRDNO3(i)=fzRDNO3(i)+  0.0300;fSumRO2(i)=fSumRO2(i)+  0.0100;

% 645, <A5HV>
i=i+1;
Rnames{ 645} = 'OLEA1 = 0.43000*OH +  0.01000*xOH +  0.49000*HO2 +  0.11000*xHO2 +  0.36000*RO2C +  0.02000*RO2XC +  0.10000*MEO2 +  0.13000*xMECO3 +  0.10000*xR2CO3 +  0.03000*MACO3 +  0.13000*xHCHO +  0.01000*xMECHO +  0.06000*xRCHO +  0.09000*GLCHO +  0.13000*xGLCHO +  0.06000*MGLY +  0.03000*AFG2A +  0.08000*AFG2B +  0.03000*KET2 +  0.04000*xKET2 +  0.01000*HCOOH +  0.01000*xPACID +  0.25000*OLEP +  0.02000*zRCNO3 +  0.01000*yROOH +  0.29000*yHPCRB +  0.02000*ALK4 +  0.01000*ALK5 +  0.05000*CO2 +  1.02000*CO +  0.48000*SumRO2 +  0.03000*SumRCO3 ';
k(:,i) = (JMACR_06 ); 
Gstr{i,   1}='OLEA1';
fOLEA1(i)=fOLEA1(i)-1.0;
fOH(i)=fOH(i)+  0.4300;fxOH(i)=fxOH(i)+  0.0100;fHO2(i)=fHO2(i)+  0.4900;fxHO2(i)=fxHO2(i)+  0.1100;fRO2C(i)=fRO2C(i)+  0.3600;fRO2XC(i)=fRO2XC(i)+  0.0200;fMEO2(i)=fMEO2(i)+  0.1000;fxMECO3(i)=fxMECO3(i)+  0.1300;fxR2CO3(i)=fxR2CO3(i)+  0.1000;fMACO3(i)=fMACO3(i)+  0.0300;fxHCHO(i)=fxHCHO(i)+  0.1300;fxMECHO(i)=fxMECHO(i)+  0.0100;fxRCHO(i)=fxRCHO(i)+  0.0600;fGLCHO(i)=fGLCHO(i)+  0.0900;fxGLCHO(i)=fxGLCHO(i)+  0.1300;fMGLY(i)=fMGLY(i)+  0.0600;fAFG2A(i)=fAFG2A(i)+  0.0300;fAFG2B(i)=fAFG2B(i)+  0.0800;fKET2(i)=fKET2(i)+  0.0300;fxKET2(i)=fxKET2(i)+  0.0400;fHCOOH(i)=fHCOOH(i)+  0.0100;fxPACID(i)=fxPACID(i)+  0.0100;fOLEP(i)=fOLEP(i)+  0.2500;fzRCNO3(i)=fzRCNO3(i)+  0.0200;fyROOH(i)=fyROOH(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.2900;fALK4(i)=fALK4(i)+  0.0200;fALK5(i)=fALK5(i)+  0.0100;fCO2(i)=fCO2(i)+  0.0500;fCO(i)=fCO(i)+  1.0200;fSumRO2(i)=fSumRO2(i)+  0.4800;fSumRCO3(i)=fSumRCO3(i)+  0.0300;

% 646, <A6OH>
i=i+1;
Rnames{ 646} = 'OLEA2 + OH = OLEA2_OH +  0.06000*OH +  0.09000*xOH +  0.05000*HO2 +  0.31000*xHO2 +  0.65000*RO2C +  0.13000*RO2XC +  0.01000*xMECO3 +  0.03000*xR2CO3 +  0.18000*MACO3 +  0.03000*xMACO3 +  0.22000*xHCHO +  0.31000*xRCHO +  0.02000*OLEA1 +  0.02000*OLEA2 +  0.02000*xGLY +  0.01000*xGLCHO +  0.01000*xMGLY +  0.08000*xKET2 +  0.01000*LVKS +  0.05000*MALAH +  0.12000*xPACID +  0.02000*OLEP +  0.11000*zRCNO3 +  0.01000*zRHNO3 +  0.04000*HPCRB +  0.47000*yHPCRB +  0.04000*CO2 +  0.09000*CO +  0.78000*SumRO2 +  0.18000*SumRCO3 ';
k(:,i) = (  8.7800E-11 ); 
Gstr{i,   1}='OLEA2';Gstr{i,   2}='OH';
fOLEA2(i)=fOLEA2(i)-1.0;fOH(i)=fOH(i)-1.0;
fOLEA2_OH(i)=fOLEA2_OH(i)+  1.0000;fOH(i)=fOH(i)+  0.0600;fxOH(i)=fxOH(i)+  0.0900;fHO2(i)=fHO2(i)+  0.0500;fxHO2(i)=fxHO2(i)+  0.3100;fRO2C(i)=fRO2C(i)+  0.6500;fRO2XC(i)=fRO2XC(i)+  0.1300;fxMECO3(i)=fxMECO3(i)+  0.0100;fxR2CO3(i)=fxR2CO3(i)+  0.0300;fMACO3(i)=fMACO3(i)+  0.1800;fxMACO3(i)=fxMACO3(i)+  0.0300;fxHCHO(i)=fxHCHO(i)+  0.2200;fxRCHO(i)=fxRCHO(i)+  0.3100;fOLEA1(i)=fOLEA1(i)+  0.0200;fOLEA2(i)=fOLEA2(i)+  0.0200;fxGLY(i)=fxGLY(i)+  0.0200;fxGLCHO(i)=fxGLCHO(i)+  0.0100;fxMGLY(i)=fxMGLY(i)+  0.0100;fxKET2(i)=fxKET2(i)+  0.0800;fLVKS(i)=fLVKS(i)+  0.0100;fMALAH(i)=fMALAH(i)+  0.0500;fxPACID(i)=fxPACID(i)+  0.1200;fOLEP(i)=fOLEP(i)+  0.0200;fzRCNO3(i)=fzRCNO3(i)+  0.1100;fzRHNO3(i)=fzRHNO3(i)+  0.0100;fHPCRB(i)=fHPCRB(i)+  0.0400;fyHPCRB(i)=fyHPCRB(i)+  0.4700;fCO2(i)=fCO2(i)+  0.0400;fCO(i)=fCO(i)+  0.0900;fSumRO2(i)=fSumRO2(i)+  0.7800;fSumRCO3(i)=fSumRCO3(i)+  0.1800;

% 647, <667>
i=i+1;
Rnames{ 647} = 'OLEA2_OH = 0.04000*OH +  0.03000*xOH +  0.03000*HO2 +  0.02000*xKET2 +  0.05000*xPACID +  0.05000*HPCRB +  0.02000*CO2 ';
k(:,i) = (  4.1400E+00 ); 
Gstr{i,   1}='OLEA2_OH';
fOLEA2_OH(i)=fOLEA2_OH(i)-1.0;
fOH(i)=fOH(i)+  0.0400;fxOH(i)=fxOH(i)+  0.0300;fHO2(i)=fHO2(i)+  0.0300;fxKET2(i)=fxKET2(i)+  0.0200;fxPACID(i)=fxPACID(i)+  0.0500;fHPCRB(i)=fHPCRB(i)+  0.0500;fCO2(i)=fCO2(i)+  0.0200;

% 648, <668>
i=i+1;
Rnames{ 648} = 'OLEA2_OH + NO = NO +  0.06000*xHO2 +  0.07000*RO2C +  0.02000*RO2XC +  0.02000*xMECO3 +  0.01000*xR2CO3 +  0.02000*xHCHO +  0.05000*xRCHO +  0.03000*xMGLY +  0.01000*xACET +  0.01000*MALAH +  0.02000*zRCNO3 +  0.18000*yHPCRB +  0.02000*CO +  0.09000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='OLEA2_OH';Gstr{i,   2}='NO';
fOLEA2_OH(i)=fOLEA2_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0600;fRO2C(i)=fRO2C(i)+  0.0700;fRO2XC(i)=fRO2XC(i)+  0.0200;fxMECO3(i)=fxMECO3(i)+  0.0200;fxR2CO3(i)=fxR2CO3(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.0200;fxRCHO(i)=fxRCHO(i)+  0.0500;fxMGLY(i)=fxMGLY(i)+  0.0300;fxACET(i)=fxACET(i)+  0.0100;fMALAH(i)=fMALAH(i)+  0.0100;fzRCNO3(i)=fzRCNO3(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.1800;fCO(i)=fCO(i)+  0.0200;fSumRO2(i)=fSumRO2(i)+  0.0900;

% 649, <A6O3>
i=i+1;
Rnames{ 649} = 'OLEA2 + O3 = OLEA2_O3 +  0.52000*OH +  0.12000*HO2 +  0.08000*xHO2 +  0.35000*RO2C +  0.07000*RO2XC +  0.12000*xMECO3 +  0.13000*HCHO2 +  0.15000*RCHO2 +  0.29000*HCHO +  0.08000*xHCHO +  0.38000*RCHO +  0.11000*xRCHO +  0.16000*GLY +  0.01000*xGLY +  0.20000*MGLY +  0.01000*xPACID +  0.07000*zRCNO3 +  0.14000*yHPCRB +  0.14000*CO2 +  0.29000*CO +  0.43000*SumRO2 ';
k(:,i) = (  1.6500E-17 ); 
Gstr{i,   1}='OLEA2';Gstr{i,   2}='O3';
fOLEA2(i)=fOLEA2(i)-1.0;fO3(i)=fO3(i)-1.0;
fOLEA2_O3(i)=fOLEA2_O3(i)+  1.0000;fOH(i)=fOH(i)+  0.5200;fHO2(i)=fHO2(i)+  0.1200;fxHO2(i)=fxHO2(i)+  0.0800;fRO2C(i)=fRO2C(i)+  0.3500;fRO2XC(i)=fRO2XC(i)+  0.0700;fxMECO3(i)=fxMECO3(i)+  0.1200;fHCHO2(i)=fHCHO2(i)+  0.1300;fRCHO2(i)=fRCHO2(i)+  0.1500;fHCHO(i)=fHCHO(i)+  0.2900;fxHCHO(i)=fxHCHO(i)+  0.0800;fRCHO(i)=fRCHO(i)+  0.3800;fxRCHO(i)=fxRCHO(i)+  0.1100;fGLY(i)=fGLY(i)+  0.1600;fxGLY(i)=fxGLY(i)+  0.0100;fMGLY(i)=fMGLY(i)+  0.2000;fxPACID(i)=fxPACID(i)+  0.0100;fzRCNO3(i)=fzRCNO3(i)+  0.0700;fyHPCRB(i)=fyHPCRB(i)+  0.1400;fCO2(i)=fCO2(i)+  0.1400;fCO(i)=fCO(i)+  0.2900;fSumRO2(i)=fSumRO2(i)+  0.4300;

% 650, <670>
i=i+1;
Rnames{ 650} = 'OLEA2_O3 = 0.15000*xOH +  0.03000*xBACL +  0.10000*xKET2 +  0.08000*xPACID +  0.05000*CO2 ';
k(:,i) = (  1.5300E+00 ); 
Gstr{i,   1}='OLEA2_O3';
fOLEA2_O3(i)=fOLEA2_O3(i)-1.0;
fxOH(i)=fxOH(i)+  0.1500;fxBACL(i)=fxBACL(i)+  0.0300;fxKET2(i)=fxKET2(i)+  0.1000;fxPACID(i)=fxPACID(i)+  0.0800;fCO2(i)=fCO2(i)+  0.0500;

% 651, <671>
i=i+1;
Rnames{ 651} = 'OLEA2_O3 + NO = NO +  0.02000*OH +  0.05000*xHO2 +  0.01000*RO2C +  0.01000*RO2XC +  0.07000*xR2CO3 +  0.01000*xRCHO +  0.03000*xGLY +  0.07000*xMGLY +  0.02000*KET2 +  0.01000*zRCNO3 +  0.24000*yHPCRB +  0.08000*CO +  0.02000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='OLEA2_O3';Gstr{i,   2}='NO';
fOLEA2_O3(i)=fOLEA2_O3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fOH(i)=fOH(i)+  0.0200;fxHO2(i)=fxHO2(i)+  0.0500;fRO2C(i)=fRO2C(i)+  0.0100;fRO2XC(i)=fRO2XC(i)+  0.0100;fxR2CO3(i)=fxR2CO3(i)+  0.0700;fxRCHO(i)=fxRCHO(i)+  0.0100;fxGLY(i)=fxGLY(i)+  0.0300;fxMGLY(i)=fxMGLY(i)+  0.0700;fKET2(i)=fKET2(i)+  0.0200;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.2400;fCO(i)=fCO(i)+  0.0800;fSumRO2(i)=fSumRO2(i)+  0.0200;

% 652, <A6N3>
i=i+1;
Rnames{ 652} = 'OLEA2 + NO3 = OLEA2_N3 +  0.09000*xNO2 +  0.28000*HNO3 +  0.13000*xOH +  0.20000*xHO2 +  1.17000*RO2C +  0.22000*RO2XC +  0.03000*xMECO3 +  0.14000*xR2CO3 +  0.07000*MACO3 +  0.07000*xMACO3 +  0.05000*xHCHO +  0.28000*xRCHO +  0.01000*xMGLY +  0.01000*xAFG3 +  0.05000*MALAH +  0.26000*xPACID +  0.32000*xRCNO3 +  0.21000*zRCNO3 +  0.02000*xRHNO3 +  0.07000*yRPNO3 +  0.01000*zRDNO3 +  0.02000*yHPCRB +  0.07000*CO2 +  0.03000*CO +  1.40000*SumRO2 +  0.07000*SumRCO3 ';
k(:,i) = (  1.1900E-12 ); 
Gstr{i,   1}='OLEA2';Gstr{i,   2}='NO3';
fOLEA2(i)=fOLEA2(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fOLEA2_N3(i)=fOLEA2_N3(i)+  1.0000;fxNO2(i)=fxNO2(i)+  0.0900;fHNO3(i)=fHNO3(i)+  0.2800;fxOH(i)=fxOH(i)+  0.1300;fxHO2(i)=fxHO2(i)+  0.2000;fRO2C(i)=fRO2C(i)+  1.1700;fRO2XC(i)=fRO2XC(i)+  0.2200;fxMECO3(i)=fxMECO3(i)+  0.0300;fxR2CO3(i)=fxR2CO3(i)+  0.1400;fMACO3(i)=fMACO3(i)+  0.0700;fxMACO3(i)=fxMACO3(i)+  0.0700;fxHCHO(i)=fxHCHO(i)+  0.0500;fxRCHO(i)=fxRCHO(i)+  0.2800;fxMGLY(i)=fxMGLY(i)+  0.0100;fxAFG3(i)=fxAFG3(i)+  0.0100;fMALAH(i)=fMALAH(i)+  0.0500;fxPACID(i)=fxPACID(i)+  0.2600;fxRCNO3(i)=fxRCNO3(i)+  0.3200;fzRCNO3(i)=fzRCNO3(i)+  0.2100;fxRHNO3(i)=fxRHNO3(i)+  0.0200;fyRPNO3(i)=fyRPNO3(i)+  0.0700;fzRDNO3(i)=fzRDNO3(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.0200;fCO2(i)=fCO2(i)+  0.0700;fCO(i)=fCO(i)+  0.0300;fSumRO2(i)=fSumRO2(i)+  1.4000;fSumRCO3(i)=fSumRCO3(i)+  0.0700;

% 653, <673>
i=i+1;
Rnames{ 653} = 'OLEA2_N3 = 0.01000*NO2 +  0.02000*OH +  0.03000*xOH +  0.13000*xPACID +  0.01000*RCNO3 +  0.01000*HPCRB +  0.03000*CO2 ';
k(:,i) = (  3.1700E+01 ); 
Gstr{i,   1}='OLEA2_N3';
fOLEA2_N3(i)=fOLEA2_N3(i)-1.0;
fNO2(i)=fNO2(i)+  0.0100;fOH(i)=fOH(i)+  0.0200;fxOH(i)=fxOH(i)+  0.0300;fxPACID(i)=fxPACID(i)+  0.1300;fRCNO3(i)=fRCNO3(i)+  0.0100;fHPCRB(i)=fHPCRB(i)+  0.0100;fCO2(i)=fCO2(i)+  0.0300;

% 654, <674>
i=i+1;
Rnames{ 654} = 'OLEA2_N3 + NO = NO +  0.02000*xNO2 +  0.02000*xHO2 +  0.03000*RO2C +  0.01000*xMECO3 +  0.07000*xHCHO +  0.05000*xRCHO +  0.02000*xMGLY +  0.01000*xRCNO3 +  0.03000*yRPNO3 +  0.24000*yHPCRB +  0.07000*CO +  0.03000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='OLEA2_N3';Gstr{i,   2}='NO';
fOLEA2_N3(i)=fOLEA2_N3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxNO2(i)=fxNO2(i)+  0.0200;fxHO2(i)=fxHO2(i)+  0.0200;fRO2C(i)=fRO2C(i)+  0.0300;fxMECO3(i)=fxMECO3(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.0700;fxRCHO(i)=fxRCHO(i)+  0.0500;fxMGLY(i)=fxMGLY(i)+  0.0200;fxRCNO3(i)=fxRCNO3(i)+  0.0100;fyRPNO3(i)=fyRPNO3(i)+  0.0300;fyHPCRB(i)=fyHPCRB(i)+  0.2400;fCO(i)=fCO(i)+  0.0700;fSumRO2(i)=fSumRO2(i)+  0.0300;

% 655, <A6HV>
i=i+1;
Rnames{ 655} = 'OLEA2 = OLEA2_HV +  0.15000*OH +  0.01000*xOH +  1.04000*HO2 +  0.35000*xHO2 +  1.06000*RO2C +  0.26000*RO2XC +  0.04000*xMECO3 +  0.45000*xHCHO +  0.17000*xRCHO +  0.01000*OLEA2 +  0.06000*xOLEA2 +  0.19000*xMACR +  0.13000*xAFG2A +  0.01000*xAFG2B +  0.01000*xLVKS +  0.13000*OLEP +  0.24000*zRCNO3 +  0.02000*zRHNO3 +  0.07000*yRUOOH +  0.04000*HPCRB +  1.23000*yHPCRB +  1.11000*CO +  1.33000*SumRO2 ';
k(:,i) = (JC2CHOabs ); 
Gstr{i,   1}='OLEA2';
fOLEA2(i)=fOLEA2(i)-1.0;
fOLEA2_HV(i)=fOLEA2_HV(i)+  1.0000;fOH(i)=fOH(i)+  0.1500;fxOH(i)=fxOH(i)+  0.0100;fHO2(i)=fHO2(i)+  1.0400;fxHO2(i)=fxHO2(i)+  0.3500;fRO2C(i)=fRO2C(i)+  1.0600;fRO2XC(i)=fRO2XC(i)+  0.2600;fxMECO3(i)=fxMECO3(i)+  0.0400;fxHCHO(i)=fxHCHO(i)+  0.4500;fxRCHO(i)=fxRCHO(i)+  0.1700;fOLEA2(i)=fOLEA2(i)+  0.0100;fxOLEA2(i)=fxOLEA2(i)+  0.0600;fxMACR(i)=fxMACR(i)+  0.1900;fxAFG2A(i)=fxAFG2A(i)+  0.1300;fxAFG2B(i)=fxAFG2B(i)+  0.0100;fxLVKS(i)=fxLVKS(i)+  0.0100;fOLEP(i)=fOLEP(i)+  0.1300;fzRCNO3(i)=fzRCNO3(i)+  0.2400;fzRHNO3(i)=fzRHNO3(i)+  0.0200;fyRUOOH(i)=fyRUOOH(i)+  0.0700;fHPCRB(i)=fHPCRB(i)+  0.0400;fyHPCRB(i)=fyHPCRB(i)+  1.2300;fCO(i)=fCO(i)+  1.1100;fSumRO2(i)=fSumRO2(i)+  1.3300;

% 656, <676>
i=i+1;
Rnames{ 656} = 'OLEA2_HV = 0.14000*OH +  0.02000*HO2 +  0.01000*AFG2A +  0.11000*HPCRB +  0.01000*CO2 ';
k(:,i) = (  2.2200E+00 ); 
Gstr{i,   1}='OLEA2_HV';
fOLEA2_HV(i)=fOLEA2_HV(i)-1.0;
fOH(i)=fOH(i)+  0.1400;fHO2(i)=fHO2(i)+  0.0200;fAFG2A(i)=fAFG2A(i)+  0.0100;fHPCRB(i)=fHPCRB(i)+  0.1100;fCO2(i)=fCO2(i)+  0.0100;

% 657, <677>
i=i+1;
Rnames{ 657} = 'OLEA2_HV + NO = NO +  0.09000*xHO2 +  0.22000*RO2C +  0.06000*RO2XC +  0.01000*xMECO3 +  0.04000*xHCHO +  0.03000*xOLEA2 +  0.03000*xLVKS +  0.05000*zRCNO3 +  0.01000*zRHNO3 +  0.01000*yRUOOH +  0.27000*yHPCRB +  0.28000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='OLEA2_HV';Gstr{i,   2}='NO';
fOLEA2_HV(i)=fOLEA2_HV(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0900;fRO2C(i)=fRO2C(i)+  0.2200;fRO2XC(i)=fRO2XC(i)+  0.0600;fxMECO3(i)=fxMECO3(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.0400;fxOLEA2(i)=fxOLEA2(i)+  0.0300;fxLVKS(i)=fxLVKS(i)+  0.0300;fzRCNO3(i)=fzRCNO3(i)+  0.0500;fzRHNO3(i)=fzRHNO3(i)+  0.0100;fyRUOOH(i)=fyRUOOH(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.2700;fSumRO2(i)=fSumRO2(i)+  0.2800;

% 658, <K5OH>
i=i+1;
Rnames{ 658} = 'KET2 + OH = 0.56000*HO2 +  0.16000*xHO2 +  0.45000*RO2C +  0.07000*RO2XC +  0.02000*MECO3 +  0.05000*xMECO3 +  0.01000*R2CO3 +  0.12000*xR2CO3 +  0.02000*HCHO +  0.14000*xHCHO +  0.09000*xMECHO +  0.01000*xETCHO +  0.13000*RCHO +  0.16000*xRCHO +  0.37000*MGLY +  0.01000*xACET +  0.07000*KET2 +  0.03000*xKET2 +  0.07000*zRCNO3 +  0.50000*yHPCRB +  0.52000*SumRO2 +  0.02000*SumRCO3 ';
k(:,i) = (  9.5600E-12 ); 
Gstr{i,   1}='KET2';Gstr{i,   2}='OH';
fKET2(i)=fKET2(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.5600;fxHO2(i)=fxHO2(i)+  0.1600;fRO2C(i)=fRO2C(i)+  0.4500;fRO2XC(i)=fRO2XC(i)+  0.0700;fMECO3(i)=fMECO3(i)+  0.0200;fxMECO3(i)=fxMECO3(i)+  0.0500;fR2CO3(i)=fR2CO3(i)+  0.0100;fxR2CO3(i)=fxR2CO3(i)+  0.1200;fHCHO(i)=fHCHO(i)+  0.0200;fxHCHO(i)=fxHCHO(i)+  0.1400;fxMECHO(i)=fxMECHO(i)+  0.0900;fxETCHO(i)=fxETCHO(i)+  0.0100;fRCHO(i)=fRCHO(i)+  0.1300;fxRCHO(i)=fxRCHO(i)+  0.1600;fMGLY(i)=fMGLY(i)+  0.3700;fxACET(i)=fxACET(i)+  0.0100;fKET2(i)=fKET2(i)+  0.0700;fxKET2(i)=fxKET2(i)+  0.0300;fzRCNO3(i)=fzRCNO3(i)+  0.0700;fyHPCRB(i)=fyHPCRB(i)+  0.5000;fSumRO2(i)=fSumRO2(i)+  0.5200;fSumRCO3(i)=fSumRCO3(i)+  0.0200;

% 659, <K5HV>
i=i+1;
Rnames{ 659} = 'KET2 = 0.37000*HO2 +  0.30000*xHO2 +  0.37000*RO2C +  0.02000*RO2XC +  0.09000*MEO2 +  0.13000*ETO2 +  0.51000*MECO3 +  0.40000*R2CO3 +  0.36000*HCHO +  0.03000*xHCHO +  0.02000*xMECHO +  0.04000*xETCHO +  0.26000*xRCHO +  0.02000*zRHNO3 +  0.39000*yROOH +  0.08000*ALK4 +  0.01000*ALK5 +  0.09000*CO +  0.61000*SumRO2 +  0.91000*SumRCO3 ';
k(:,i) = (  7.5300E-02.*JMEK_06 ); 
Gstr{i,   1}='KET2';
fKET2(i)=fKET2(i)-1.0;
fHO2(i)=fHO2(i)+  0.3700;fxHO2(i)=fxHO2(i)+  0.3000;fRO2C(i)=fRO2C(i)+  0.3700;fRO2XC(i)=fRO2XC(i)+  0.0200;fMEO2(i)=fMEO2(i)+  0.0900;fETO2(i)=fETO2(i)+  0.1300;fMECO3(i)=fMECO3(i)+  0.5100;fR2CO3(i)=fR2CO3(i)+  0.4000;fHCHO(i)=fHCHO(i)+  0.3600;fxHCHO(i)=fxHCHO(i)+  0.0300;fxMECHO(i)=fxMECHO(i)+  0.0200;fxETCHO(i)=fxETCHO(i)+  0.0400;fxRCHO(i)=fxRCHO(i)+  0.2600;fzRHNO3(i)=fzRHNO3(i)+  0.0200;fyROOH(i)=fyROOH(i)+  0.3900;fALK4(i)=fALK4(i)+  0.0800;fALK5(i)=fALK5(i)+  0.0100;fCO(i)=fCO(i)+  0.0900;fSumRO2(i)=fSumRO2(i)+  0.6100;fSumRCO3(i)=fSumRCO3(i)+  0.9100;

% 660, <K6OH>
i=i+1;
Rnames{ 660} = 'LVKS + OH = LVKS_OH +  0.22000*HO2 +  0.04000*xHO2 +  0.39000*RO2C +  0.03000*RO2XC +  0.13000*MECO3 +  0.09000*xMECO3 +  0.25000*xR2CO3 +  0.04000*xHCHO +  0.20000*RCHO +  0.09000*xRCHO +  0.13000*OLEA1 +  0.01000*MGLY +  0.04000*xBACL +  0.25000*xKET2 +  0.03000*zRCNO3 +  0.36000*yHPCRB +  0.42000*SumRO2 +  0.13000*SumRCO3 ';
k(:,i) = (  6.0900E-11 ); 
Gstr{i,   1}='LVKS';Gstr{i,   2}='OH';
fLVKS(i)=fLVKS(i)-1.0;fOH(i)=fOH(i)-1.0;
fLVKS_OH(i)=fLVKS_OH(i)+  1.0000;fHO2(i)=fHO2(i)+  0.2200;fxHO2(i)=fxHO2(i)+  0.0400;fRO2C(i)=fRO2C(i)+  0.3900;fRO2XC(i)=fRO2XC(i)+  0.0300;fMECO3(i)=fMECO3(i)+  0.1300;fxMECO3(i)=fxMECO3(i)+  0.0900;fxR2CO3(i)=fxR2CO3(i)+  0.2500;fxHCHO(i)=fxHCHO(i)+  0.0400;fRCHO(i)=fRCHO(i)+  0.2000;fxRCHO(i)=fxRCHO(i)+  0.0900;fOLEA1(i)=fOLEA1(i)+  0.1300;fMGLY(i)=fMGLY(i)+  0.0100;fxBACL(i)=fxBACL(i)+  0.0400;fxKET2(i)=fxKET2(i)+  0.2500;fzRCNO3(i)=fzRCNO3(i)+  0.0300;fyHPCRB(i)=fyHPCRB(i)+  0.3600;fSumRO2(i)=fSumRO2(i)+  0.4200;fSumRCO3(i)=fSumRCO3(i)+  0.1300;

% 661, <681>
i=i+1;
Rnames{ 661} = 'LVKS_OH = 0.23000*OH +  0.01000*HO2 +  0.23000*HPCRB ';
k(:,i) = (  4.6800E-01 ); 
Gstr{i,   1}='LVKS_OH';
fLVKS_OH(i)=fLVKS_OH(i)-1.0;
fOH(i)=fOH(i)+  0.2300;fHO2(i)=fHO2(i)+  0.0100;fHPCRB(i)=fHPCRB(i)+  0.2300;

% 662, <682>
i=i+1;
Rnames{ 662} = 'LVKS_OH + NO = NO +  0.01000*xOH +  0.09000*xHO2 +  0.21000*RO2C +  0.04000*RO2XC +  0.10000*xMECO3 +  0.09000*xRCHO +  0.01000*xBACL +  0.01000*xAFG2B +  0.01000*xPACID +  0.04000*zRCNO3 +  0.09000*xHPCRB +  0.23000*yHPCRB +  0.01000*CO +  0.25000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='LVKS_OH';Gstr{i,   2}='NO';
fLVKS_OH(i)=fLVKS_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxOH(i)=fxOH(i)+  0.0100;fxHO2(i)=fxHO2(i)+  0.0900;fRO2C(i)=fRO2C(i)+  0.2100;fRO2XC(i)=fRO2XC(i)+  0.0400;fxMECO3(i)=fxMECO3(i)+  0.1000;fxRCHO(i)=fxRCHO(i)+  0.0900;fxBACL(i)=fxBACL(i)+  0.0100;fxAFG2B(i)=fxAFG2B(i)+  0.0100;fxPACID(i)=fxPACID(i)+  0.0100;fzRCNO3(i)=fzRCNO3(i)+  0.0400;fxHPCRB(i)=fxHPCRB(i)+  0.0900;fyHPCRB(i)=fyHPCRB(i)+  0.2300;fCO(i)=fCO(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  0.2500;

% 663, <K6O3>
i=i+1;
Rnames{ 663} = 'LVKS + O3 = LVKS_O3 +  0.53000*OH +  0.38000*HO2 +  0.02000*xHO2 +  0.18000*RO2C +  0.01000*RO2XC +  0.01000*MECO3 +  0.09000*xMECO3 +  0.01000*xR2CO3 +  0.13000*HCHO2 +  0.13000*RCHO2 +  0.03000*HCHO +  0.02000*xHCHO +  0.01000*MECHO +  0.03000*RCHO +  0.01000*xGLY +  0.59000*MGLY +  0.01000*xMGLY +  0.32000*BACL +  0.01000*xBACL +  0.06000*KET2 +  0.03000*HCOOH +  0.01000*OACID +  0.02000*xPACID +  0.01000*zRCNO3 +  0.04000*yHPCRB +  0.40000*CO2 +  0.30000*CO +  0.19000*SumRO2 +  0.01000*SumRCO3 ';
k(:,i) = (  3.0400E-17 ); 
Gstr{i,   1}='LVKS';Gstr{i,   2}='O3';
fLVKS(i)=fLVKS(i)-1.0;fO3(i)=fO3(i)-1.0;
fLVKS_O3(i)=fLVKS_O3(i)+  1.0000;fOH(i)=fOH(i)+  0.5300;fHO2(i)=fHO2(i)+  0.3800;fxHO2(i)=fxHO2(i)+  0.0200;fRO2C(i)=fRO2C(i)+  0.1800;fRO2XC(i)=fRO2XC(i)+  0.0100;fMECO3(i)=fMECO3(i)+  0.0100;fxMECO3(i)=fxMECO3(i)+  0.0900;fxR2CO3(i)=fxR2CO3(i)+  0.0100;fHCHO2(i)=fHCHO2(i)+  0.1300;fRCHO2(i)=fRCHO2(i)+  0.1300;fHCHO(i)=fHCHO(i)+  0.0300;fxHCHO(i)=fxHCHO(i)+  0.0200;fMECHO(i)=fMECHO(i)+  0.0100;fRCHO(i)=fRCHO(i)+  0.0300;fxGLY(i)=fxGLY(i)+  0.0100;fMGLY(i)=fMGLY(i)+  0.5900;fxMGLY(i)=fxMGLY(i)+  0.0100;fBACL(i)=fBACL(i)+  0.3200;fxBACL(i)=fxBACL(i)+  0.0100;fKET2(i)=fKET2(i)+  0.0600;fHCOOH(i)=fHCOOH(i)+  0.0300;fOACID(i)=fOACID(i)+  0.0100;fxPACID(i)=fxPACID(i)+  0.0200;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.0400;fCO2(i)=fCO2(i)+  0.4000;fCO(i)=fCO(i)+  0.3000;fSumRO2(i)=fSumRO2(i)+  0.1900;fSumRCO3(i)=fSumRCO3(i)+  0.0100;

% 664, <684>
i=i+1;
Rnames{ 664} = 'LVKS_O3 = 0.02000*OH +  0.01000*xOH +  0.04000*xMECO3 +  0.12000*xPACID +  0.01000*CO2 ';
k(:,i) = (  5.1300E+00 ); 
Gstr{i,   1}='LVKS_O3';
fLVKS_O3(i)=fLVKS_O3(i)-1.0;
fOH(i)=fOH(i)+  0.0200;fxOH(i)=fxOH(i)+  0.0100;fxMECO3(i)=fxMECO3(i)+  0.0400;fxPACID(i)=fxPACID(i)+  0.1200;fCO2(i)=fCO2(i)+  0.0100;

% 665, <685>
i=i+1;
Rnames{ 665} = 'LVKS_O3 + NO = NO +  0.05000*xHO2 +  0.02000*RO2C +  0.01000*RO2XC +  0.02000*xHCHO +  0.10000*xMGLY +  0.01000*zRCNO3 +  0.13000*yHPCRB +  0.02000*CO +  0.03000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='LVKS_O3';Gstr{i,   2}='NO';
fLVKS_O3(i)=fLVKS_O3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0500;fRO2C(i)=fRO2C(i)+  0.0200;fRO2XC(i)=fRO2XC(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.0200;fxMGLY(i)=fxMGLY(i)+  0.1000;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.1300;fCO(i)=fCO(i)+  0.0200;fSumRO2(i)=fSumRO2(i)+  0.0300;

% 666, <K6HV>
i=i+1;
Rnames{ 666} = 'LVKS = 0.06000*xOH +  0.14000*HO2 +  0.12000*RO2C +  0.03000*RO2XC +  0.26000*MEO2 +  0.06000*xMECO3 +  0.25000*MACO3 +  0.14000*HCHO +  0.06000*xMGLY +  0.06000*MALAH +  0.06000*xPACID +  0.60000*OLEP +  0.03000*zRCNO3 +  0.60000*CO +  0.41000*SumRO2 +  0.25000*SumRCO3 ';
k(:,i) = (JMVK_16 ); 
Gstr{i,   1}='LVKS';
fLVKS(i)=fLVKS(i)-1.0;
fxOH(i)=fxOH(i)+  0.0600;fHO2(i)=fHO2(i)+  0.1400;fRO2C(i)=fRO2C(i)+  0.1200;fRO2XC(i)=fRO2XC(i)+  0.0300;fMEO2(i)=fMEO2(i)+  0.2600;fxMECO3(i)=fxMECO3(i)+  0.0600;fMACO3(i)=fMACO3(i)+  0.2500;fHCHO(i)=fHCHO(i)+  0.1400;fxMGLY(i)=fxMGLY(i)+  0.0600;fMALAH(i)=fMALAH(i)+  0.0600;fxPACID(i)=fxPACID(i)+  0.0600;fOLEP(i)=fOLEP(i)+  0.6000;fzRCNO3(i)=fzRCNO3(i)+  0.0300;fCO(i)=fCO(i)+  0.6000;fSumRO2(i)=fSumRO2(i)+  0.4100;fSumRCO3(i)=fSumRCO3(i)+  0.2500;

% 667, <O5OH>
i=i+1;
Rnames{ 667} = 'OLEP + OH = OLEP_OH +  0.01000*OH +  0.13000*HO2 +  0.55000*xHO2 +  0.65000*RO2C +  0.14000*RO2XC +  0.02000*xHCHO +  0.53000*xRCHO +  0.11000*OLEA1 +  0.01000*xBACL +  0.02000*xKET2 +  0.01000*xMVK +  0.01000*OLEP +  0.14000*zRCNO3 +  0.03000*yROOH +  0.01000*yRUOOH +  0.01000*HPCRB +  0.74000*yHPCRB +  0.80000*SumRO2 ';
k(:,i) = (  8.3400E-11 ); 
Gstr{i,   1}='OLEP';Gstr{i,   2}='OH';
fOLEP(i)=fOLEP(i)-1.0;fOH(i)=fOH(i)-1.0;
fOLEP_OH(i)=fOLEP_OH(i)+  1.0000;fOH(i)=fOH(i)+  0.0100;fHO2(i)=fHO2(i)+  0.1300;fxHO2(i)=fxHO2(i)+  0.5500;fRO2C(i)=fRO2C(i)+  0.6500;fRO2XC(i)=fRO2XC(i)+  0.1400;fxHCHO(i)=fxHCHO(i)+  0.0200;fxRCHO(i)=fxRCHO(i)+  0.5300;fOLEA1(i)=fOLEA1(i)+  0.1100;fxBACL(i)=fxBACL(i)+  0.0100;fxKET2(i)=fxKET2(i)+  0.0200;fxMVK(i)=fxMVK(i)+  0.0100;fOLEP(i)=fOLEP(i)+  0.0100;fzRCNO3(i)=fzRCNO3(i)+  0.1400;fyROOH(i)=fyROOH(i)+  0.0300;fyRUOOH(i)=fyRUOOH(i)+  0.0100;fHPCRB(i)=fHPCRB(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.7400;fSumRO2(i)=fSumRO2(i)+  0.8000;

% 668, <688>
i=i+1;
Rnames{ 668} = 'OLEP_OH = 0.05000*OH +  0.01000*xOH +  0.10000*HO2 +  0.12000*HPCRB +  0.01000*CO2 ';
k(:,i) = (  2.3000E+00 ); 
Gstr{i,   1}='OLEP_OH';
fOLEP_OH(i)=fOLEP_OH(i)-1.0;
fOH(i)=fOH(i)+  0.0500;fxOH(i)=fxOH(i)+  0.0100;fHO2(i)=fHO2(i)+  0.1000;fHPCRB(i)=fHPCRB(i)+  0.1200;fCO2(i)=fCO2(i)+  0.0100;

% 669, <689>
i=i+1;
Rnames{ 669} = 'OLEP_OH + NO = NO +  0.07000*xHO2 +  0.25000*RO2C +  0.03000*RO2XC +  0.06000*xMECO3 +  0.11000*xHCHO +  0.06000*xGLCHO +  0.02000*xMGLY +  0.03000*xKET2 +  0.02000*zRCNO3 +  0.01000*zRHNO3 +  0.07000*yRUOOH +  0.14000*yHPCRB +  0.04000*CO +  0.28000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='OLEP_OH';Gstr{i,   2}='NO';
fOLEP_OH(i)=fOLEP_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0700;fRO2C(i)=fRO2C(i)+  0.2500;fRO2XC(i)=fRO2XC(i)+  0.0300;fxMECO3(i)=fxMECO3(i)+  0.0600;fxHCHO(i)=fxHCHO(i)+  0.1100;fxGLCHO(i)=fxGLCHO(i)+  0.0600;fxMGLY(i)=fxMGLY(i)+  0.0200;fxKET2(i)=fxKET2(i)+  0.0300;fzRCNO3(i)=fzRCNO3(i)+  0.0200;fzRHNO3(i)=fzRHNO3(i)+  0.0100;fyRUOOH(i)=fyRUOOH(i)+  0.0700;fyHPCRB(i)=fyHPCRB(i)+  0.1400;fCO(i)=fCO(i)+  0.0400;fSumRO2(i)=fSumRO2(i)+  0.2800;

% 670, <O5O3>
i=i+1;
Rnames{ 670} = 'OLEP + O3 = OLEP_O3 +  0.61000*OH +  0.05000*HO2 +  0.08000*xHO2 +  0.33000*RO2C +  0.09000*RO2XC +  0.04000*xMECO3 +  0.11000*xR2CO3 +  0.06000*HCHO2 +  0.23000*RCHO2 +  0.14000*HCHO +  0.14000*xHCHO +  0.08000*RCHO +  0.09000*xRCHO +  0.02000*xGLY +  0.13000*KET2 +  0.09000*zRCNO3 +  0.35000*yHPCRB +  0.03000*ALK2 +  0.09000*CO2 +  0.18000*CO +  0.41000*SumRO2 ';
k(:,i) = (  1.5000E-16 ); 
Gstr{i,   1}='OLEP';Gstr{i,   2}='O3';
fOLEP(i)=fOLEP(i)-1.0;fO3(i)=fO3(i)-1.0;
fOLEP_O3(i)=fOLEP_O3(i)+  1.0000;fOH(i)=fOH(i)+  0.6100;fHO2(i)=fHO2(i)+  0.0500;fxHO2(i)=fxHO2(i)+  0.0800;fRO2C(i)=fRO2C(i)+  0.3300;fRO2XC(i)=fRO2XC(i)+  0.0900;fxMECO3(i)=fxMECO3(i)+  0.0400;fxR2CO3(i)=fxR2CO3(i)+  0.1100;fHCHO2(i)=fHCHO2(i)+  0.0600;fRCHO2(i)=fRCHO2(i)+  0.2300;fHCHO(i)=fHCHO(i)+  0.1400;fxHCHO(i)=fxHCHO(i)+  0.1400;fRCHO(i)=fRCHO(i)+  0.0800;fxRCHO(i)=fxRCHO(i)+  0.0900;fxGLY(i)=fxGLY(i)+  0.0200;fKET2(i)=fKET2(i)+  0.1300;fzRCNO3(i)=fzRCNO3(i)+  0.0900;fyHPCRB(i)=fyHPCRB(i)+  0.3500;fALK2(i)=fALK2(i)+  0.0300;fCO2(i)=fCO2(i)+  0.0900;fCO(i)=fCO(i)+  0.1800;fSumRO2(i)=fSumRO2(i)+  0.4100;

% 671, <691>
i=i+1;
Rnames{ 671} = 'OLEP_O3 = 0.17000*OH +  0.04000*HO2 +  0.01000*KET2 +  0.17000*HPCRB ';
k(:,i) = (  9.3600E-01 ); 
Gstr{i,   1}='OLEP_O3';
fOLEP_O3(i)=fOLEP_O3(i)-1.0;
fOH(i)=fOH(i)+  0.1700;fHO2(i)=fHO2(i)+  0.0400;fKET2(i)=fKET2(i)+  0.0100;fHPCRB(i)=fHPCRB(i)+  0.1700;

% 672, <692>
i=i+1;
Rnames{ 672} = 'OLEP_O3 + NO = NO +  0.17000*RO2C +  0.06000*RO2XC +  0.15000*xMECO3 +  0.01000*xR2CO3 +  0.15000*xRCHO +  0.06000*zRCNO3 +  0.18000*yHPCRB +  0.22000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='OLEP_O3';Gstr{i,   2}='NO';
fOLEP_O3(i)=fOLEP_O3(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fRO2C(i)=fRO2C(i)+  0.1700;fRO2XC(i)=fRO2XC(i)+  0.0600;fxMECO3(i)=fxMECO3(i)+  0.1500;fxR2CO3(i)=fxR2CO3(i)+  0.0100;fxRCHO(i)=fxRCHO(i)+  0.1500;fzRCNO3(i)=fzRCNO3(i)+  0.0600;fyHPCRB(i)=fyHPCRB(i)+  0.1800;fSumRO2(i)=fSumRO2(i)+  0.2200;

% 673, <O5N3>
i=i+1;
Rnames{ 673} = 'OLEP + NO3 = 0.74000*xNO2 +  0.08000*xHO2 +  0.88000*RO2C +  0.16000*RO2XC +  0.01000*xMECO3 +  0.18000*xHCHO +  0.64000*xRCHO +  0.11000*xKET2 +  0.09000*xRCNO3 +  0.14000*zRCNO3 +  0.28000*yRPNO3 +  0.02000*zRDNO3 +  0.01000*yHPCRB +  1.04000*SumRO2 ';
k(:,i) = (  8.5000E-12 ); 
Gstr{i,   1}='OLEP';Gstr{i,   2}='NO3';
fOLEP(i)=fOLEP(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fxNO2(i)=fxNO2(i)+  0.7400;fxHO2(i)=fxHO2(i)+  0.0800;fRO2C(i)=fRO2C(i)+  0.8800;fRO2XC(i)=fRO2XC(i)+  0.1600;fxMECO3(i)=fxMECO3(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.1800;fxRCHO(i)=fxRCHO(i)+  0.6400;fxKET2(i)=fxKET2(i)+  0.1100;fxRCNO3(i)=fxRCNO3(i)+  0.0900;fzRCNO3(i)=fzRCNO3(i)+  0.1400;fyRPNO3(i)=fyRPNO3(i)+  0.2800;fzRDNO3(i)=fzRDNO3(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  1.0400;

% 674, <OAOH>
i=i+1;
Rnames{ 674} = 'OACID + OH = 0.30000*xHO2 +  0.30000*RO2C +  0.70000*MEO2 +  0.02000*xHCHO +  0.28000*xMGLY +  0.26000*yHPCRB +  0.72000*CO2 + SumRO2 ';
k(:,i) = (  7.4700E-13 ); 
Gstr{i,   1}='OACID';Gstr{i,   2}='OH';
fOACID(i)=fOACID(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  0.3000;fRO2C(i)=fRO2C(i)+  0.3000;fMEO2(i)=fMEO2(i)+  0.7000;fxHCHO(i)=fxHCHO(i)+  0.0200;fxMGLY(i)=fxMGLY(i)+  0.2800;fyHPCRB(i)=fyHPCRB(i)+  0.2600;fCO2(i)=fCO2(i)+  0.7200;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 675, <PAOH>
i=i+1;
Rnames{ 675} = 'PACID + OH = 0.19000*xOH +  0.56000*xHO2 +  0.74000*RO2C +  0.26000*MECO3 +  0.19000*xHCHO +  0.56000*xPACID +  0.19000*CO2 +  0.74000*SumRO2 +  0.26000*SumRCO3 ';
k(:,i) = (  3.0000E-14 ); 
Gstr{i,   1}='PACID';Gstr{i,   2}='OH';
fPACID(i)=fPACID(i)-1.0;fOH(i)=fOH(i)-1.0;
fxOH(i)=fxOH(i)+  0.1900;fxHO2(i)=fxHO2(i)+  0.5600;fRO2C(i)=fRO2C(i)+  0.7400;fMECO3(i)=fMECO3(i)+  0.2600;fxHCHO(i)=fxHCHO(i)+  0.1900;fxPACID(i)=fxPACID(i)+  0.5600;fCO2(i)=fCO2(i)+  0.1900;fSumRO2(i)=fSumRO2(i)+  0.7400;fSumRCO3(i)=fSumRCO3(i)+  0.2600;

% 676, <PAHV>
i=i+1;
Rnames{ 676} = 'PACID = OH + MEO2 + CO2 + SumRO2 ';
k(:,i) = (JPAA ); 
Gstr{i,   1}='PACID';
fPACID(i)=fPACID(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fMEO2(i)=fMEO2(i)+  1.0000;fCO2(i)=fCO2(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 677, <MGOH>
i=i+1;
Rnames{ 677} = 'MGLY + OH = 0.01000*xHO2 +  0.01000*RO2C +  0.99000*MECO3 +  0.01000*xHCHO +  0.01000*xPACID + CO +  0.01000*SumRO2 +  0.99000*SumRCO3 ';
k(:,i) = (  1.1900E-11 ); 
Gstr{i,   1}='MGLY';Gstr{i,   2}='OH';
fMGLY(i)=fMGLY(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  0.0100;fRO2C(i)=fRO2C(i)+  0.0100;fMECO3(i)=fMECO3(i)+  0.9900;fxHCHO(i)=fxHCHO(i)+  0.0100;fxPACID(i)=fxPACID(i)+  0.0100;fCO(i)=fCO(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  0.0100;fSumRCO3(i)=fSumRCO3(i)+  0.9900;

% 678, <MGN3>
i=i+1;
Rnames{ 678} = 'MGLY + NO3 = HNO3 + MECO3 + CO + SumRCO3 ';
k(:,i) = (  5.0000E-16 ); 
Gstr{i,   1}='MGLY';Gstr{i,   2}='NO3';
fMGLY(i)=fMGLY(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;fMECO3(i)=fMECO3(i)+  1.0000;fCO(i)=fCO(i)+  1.0000;fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 679, <MGHV>
i=i+1;
Rnames{ 679} = 'MGLY = HO2 + MECO3 + CO + SumRCO3 ';
k(:,i) = (JMGLY_13 ); 
Gstr{i,   1}='MGLY';
fMGLY(i)=fMGLY(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fMECO3(i)=fMECO3(i)+  1.0000;fCO(i)=fCO(i)+  1.0000;fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 680, <BAHV>
i=i+1;
Rnames{ 680} = 'BACL = 2.00000*MECO3 +  2.00000*SumRCO3 ';
k(:,i) = (JBACL_11 ); 
Gstr{i,   1}='BACL';
fBACL(i)=fBACL(i)-1.0;
fMECO3(i)=fMECO3(i)+  2.0000;fSumRCO3(i)=fSumRCO3(i)+  2.0000;

% 681, <CROH>
i=i+1;
Rnames{ 681} = 'CRES + OH = 0.84000*HO2 +  0.11000*xHO2 +  0.11000*RO2C +  0.02000*RO2XC +  0.03000*BZO +  0.02000*OLEA1 +  0.08000*OLEA2 +  0.02000*xGLY +  0.05000*xMGLY +  0.03000*xBACL +  0.03000*xBUDAL +  0.01000*xAFG1 +  0.05000*xAFG2A +  0.02000*xAFG2B +  0.01000*xBALD +  0.17000*LVKS +  0.14000*OLEP +  0.42000*CATL +  0.02000*zRANO3 +  0.02000*yROOH +  0.12000*yRAOOH +  0.13000*SumRO2 ';
k(:,i) = (  4.6500E-11 ); 
Gstr{i,   1}='CRES';Gstr{i,   2}='OH';
fCRES(i)=fCRES(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.8400;fxHO2(i)=fxHO2(i)+  0.1100;fRO2C(i)=fRO2C(i)+  0.1100;fRO2XC(i)=fRO2XC(i)+  0.0200;fBZO(i)=fBZO(i)+  0.0300;fOLEA1(i)=fOLEA1(i)+  0.0200;fOLEA2(i)=fOLEA2(i)+  0.0800;fxGLY(i)=fxGLY(i)+  0.0200;fxMGLY(i)=fxMGLY(i)+  0.0500;fxBACL(i)=fxBACL(i)+  0.0300;fxBUDAL(i)=fxBUDAL(i)+  0.0300;fxAFG1(i)=fxAFG1(i)+  0.0100;fxAFG2A(i)=fxAFG2A(i)+  0.0500;fxAFG2B(i)=fxAFG2B(i)+  0.0200;fxBALD(i)=fxBALD(i)+  0.0100;fLVKS(i)=fLVKS(i)+  0.1700;fOLEP(i)=fOLEP(i)+  0.1400;fCATL(i)=fCATL(i)+  0.4200;fzRANO3(i)=fzRANO3(i)+  0.0200;fyROOH(i)=fyROOH(i)+  0.0200;fyRAOOH(i)=fyRAOOH(i)+  0.1200;fSumRO2(i)=fSumRO2(i)+  0.1300;

% 682, <CRN3>
i=i+1;
Rnames{ 682} = 'CRES + NO3 = HNO3 + BZO ';
k(:,i) = (  1.2700E-11 ); 
Gstr{i,   1}='CRES';Gstr{i,   2}='NO3';
fCRES(i)=fCRES(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;fBZO(i)=fBZO(i)+  1.0000;

% 683, <XLOH>
i=i+1;
Rnames{ 683} = 'XYNL + OH = 0.78000*HO2 +  0.16000*xHO2 +  0.17000*RO2C +  0.04000*RO2XC +  0.02000*BZO +  0.02000*OLEA1 +  0.06000*OLEA2 +  0.01000*xGLY +  0.04000*xMGLY +  0.10000*xBACL +  0.01000*xBUDAL +  0.01000*xAFG1 +  0.09000*xAFG2A +  0.03000*xAFG2B +  0.01000*xBALD +  0.26000*LVKS +  0.01000*xAFG3 +  0.16000*OLEP +  0.01000*XYNL +  0.27000*CATL +  0.03000*zRANO3 +  0.02000*yROOH +  0.18000*yRAOOH +  0.20000*SumRO2 ';
k(:,i) = (  6.7300E-11 ); 
Gstr{i,   1}='XYNL';Gstr{i,   2}='OH';
fXYNL(i)=fXYNL(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.7800;fxHO2(i)=fxHO2(i)+  0.1600;fRO2C(i)=fRO2C(i)+  0.1700;fRO2XC(i)=fRO2XC(i)+  0.0400;fBZO(i)=fBZO(i)+  0.0200;fOLEA1(i)=fOLEA1(i)+  0.0200;fOLEA2(i)=fOLEA2(i)+  0.0600;fxGLY(i)=fxGLY(i)+  0.0100;fxMGLY(i)=fxMGLY(i)+  0.0400;fxBACL(i)=fxBACL(i)+  0.1000;fxBUDAL(i)=fxBUDAL(i)+  0.0100;fxAFG1(i)=fxAFG1(i)+  0.0100;fxAFG2A(i)=fxAFG2A(i)+  0.0900;fxAFG2B(i)=fxAFG2B(i)+  0.0300;fxBALD(i)=fxBALD(i)+  0.0100;fLVKS(i)=fLVKS(i)+  0.2600;fxAFG3(i)=fxAFG3(i)+  0.0100;fOLEP(i)=fOLEP(i)+  0.1600;fXYNL(i)=fXYNL(i)+  0.0100;fCATL(i)=fCATL(i)+  0.2700;fzRANO3(i)=fzRANO3(i)+  0.0300;fyROOH(i)=fyROOH(i)+  0.0200;fyRAOOH(i)=fyRAOOH(i)+  0.1800;fSumRO2(i)=fSumRO2(i)+  0.2000;

% 684, <XLN3>
i=i+1;
Rnames{ 684} = 'XYNL + NO3 = HNO3 + BZO ';
k(:,i) = (  3.0900E-11 ); 
Gstr{i,   1}='XYNL';Gstr{i,   2}='NO3';
fXYNL(i)=fXYNL(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;fBZO(i)=fBZO(i)+  1.0000;

% 685, <CAOH>
i=i+1;
Rnames{ 685} = 'CATL + OH = 0.96000*HO2 +  0.03000*xHO2 +  0.03000*RO2C +  0.01000*RO2XC +  0.01000*BZO +  0.01000*OLEA1 +  0.02000*OLEA2 +  0.02000*xBACL +  0.02000*xAFG2A +  0.73000*LVKS +  0.06000*OLEP +  0.12000*CATL3 +  0.01000*zRANO3 +  0.03000*yRAOOH +  0.03000*SumRO2 ';
k(:,i) = (  1.5600E-10 ); 
Gstr{i,   1}='CATL';Gstr{i,   2}='OH';
fCATL(i)=fCATL(i)-1.0;fOH(i)=fOH(i)-1.0;
fHO2(i)=fHO2(i)+  0.9600;fxHO2(i)=fxHO2(i)+  0.0300;fRO2C(i)=fRO2C(i)+  0.0300;fRO2XC(i)=fRO2XC(i)+  0.0100;fBZO(i)=fBZO(i)+  0.0100;fOLEA1(i)=fOLEA1(i)+  0.0100;fOLEA2(i)=fOLEA2(i)+  0.0200;fxBACL(i)=fxBACL(i)+  0.0200;fxAFG2A(i)=fxAFG2A(i)+  0.0200;fLVKS(i)=fLVKS(i)+  0.7300;fOLEP(i)=fOLEP(i)+  0.0600;fCATL3(i)=fCATL3(i)+  0.1200;fzRANO3(i)=fzRANO3(i)+  0.0100;fyRAOOH(i)=fyRAOOH(i)+  0.0300;fSumRO2(i)=fSumRO2(i)+  0.0300;

% 686, <CAN3>
i=i+1;
Rnames{ 686} = 'CATL + NO3 = HNO3 + BZO ';
k(:,i) = (  4.0400E-11 ); 
Gstr{i,   1}='CATL';Gstr{i,   2}='NO3';
fCATL(i)=fCATL(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;fBZO(i)=fBZO(i)+  1.0000;

% 687, <N4OH>
i=i+1;
Rnames{ 687} = 'RCNO3 + OH = RCNO3_OH +  0.34000*NO2 +  0.10000*xNO2 +  0.02000*OH +  0.01000*xOH +  0.05000*HO2 +  0.13000*xHO2 +  0.49000*RO2C +  0.07000*RO2XC +  0.05000*xMECO3 +  0.07000*R2CO3 +  0.08000*xR2CO3 +  0.03000*MACO3 +  0.13000*xHCHO +  0.05000*xMECHO +  0.05000*RCHO +  0.05000*xRCHO +  0.01000*xGLY +  0.05000*MGLY +  0.02000*xACET +  0.12000*KET2 +  0.01000*xKET2 +  0.01000*PACID +  0.04000*xPACID +  0.04000*OLEP +  0.07000*RCNO3 +  0.21000*xRCNO3 +  0.07000*zRCNO3 +  0.03000*yRPNO3 +  0.03000*yHPCRB +  0.06000*ALK2 +  4.74000*NROG +  0.01000*CO2 +  0.01000*CO +  0.56000*SumRO2 +  0.09000*SumRCO3 ';
k(:,i) = (  2.0600E-11 ); 
Gstr{i,   1}='RCNO3';Gstr{i,   2}='OH';
fRCNO3(i)=fRCNO3(i)-1.0;fOH(i)=fOH(i)-1.0;
fRCNO3_OH(i)=fRCNO3_OH(i)+  1.0000;fNO2(i)=fNO2(i)+  0.3400;fxNO2(i)=fxNO2(i)+  0.1000;fOH(i)=fOH(i)+  0.0200;fxOH(i)=fxOH(i)+  0.0100;fHO2(i)=fHO2(i)+  0.0500;fxHO2(i)=fxHO2(i)+  0.1300;fRO2C(i)=fRO2C(i)+  0.4900;fRO2XC(i)=fRO2XC(i)+  0.0700;fxMECO3(i)=fxMECO3(i)+  0.0500;fR2CO3(i)=fR2CO3(i)+  0.0700;fxR2CO3(i)=fxR2CO3(i)+  0.0800;fMACO3(i)=fMACO3(i)+  0.0300;fxHCHO(i)=fxHCHO(i)+  0.1300;fxMECHO(i)=fxMECHO(i)+  0.0500;fRCHO(i)=fRCHO(i)+  0.0500;fxRCHO(i)=fxRCHO(i)+  0.0500;fxGLY(i)=fxGLY(i)+  0.0100;fMGLY(i)=fMGLY(i)+  0.0500;fxACET(i)=fxACET(i)+  0.0200;fKET2(i)=fKET2(i)+  0.1200;fxKET2(i)=fxKET2(i)+  0.0100;fPACID(i)=fPACID(i)+  0.0100;fxPACID(i)=fxPACID(i)+  0.0400;fOLEP(i)=fOLEP(i)+  0.0400;fRCNO3(i)=fRCNO3(i)+  0.0700;fxRCNO3(i)=fxRCNO3(i)+  0.2100;fzRCNO3(i)=fzRCNO3(i)+  0.0700;fyRPNO3(i)=fyRPNO3(i)+  0.0300;fyHPCRB(i)=fyHPCRB(i)+  0.0300;fALK2(i)=fALK2(i)+  0.0600;fNROG(i)=fNROG(i)+  4.7400;fCO2(i)=fCO2(i)+  0.0100;fCO(i)=fCO(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  0.5600;fSumRCO3(i)=fSumRCO3(i)+  0.0900;

% 688, <708>
i=i+1;
Rnames{ 688} = 'RCNO3_OH = 0.05000*HO2 +  0.01000*xPACID +  0.05000*RCNO3 ';
k(:,i) = (  2.7600E+00 ); 
Gstr{i,   1}='RCNO3_OH';
fRCNO3_OH(i)=fRCNO3_OH(i)-1.0;
fHO2(i)=fHO2(i)+  0.0500;fxPACID(i)=fxPACID(i)+  0.0100;fRCNO3(i)=fRCNO3(i)+  0.0500;

% 689, <709>
i=i+1;
Rnames{ 689} = 'RCNO3_OH + NO = NO +  0.01000*xHO2 +  0.07000*RO2C +  0.02000*RO2XC +  0.03000*xR2CO3 +  0.03000*xHCHO +  0.01000*xGLY +  0.04000*xRCNO3 +  0.02000*zRCNO3 +  0.01000*yRPNO3 +  0.02000*yHPCRB +  0.09000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='RCNO3_OH';Gstr{i,   2}='NO';
fRCNO3_OH(i)=fRCNO3_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0100;fRO2C(i)=fRO2C(i)+  0.0700;fRO2XC(i)=fRO2XC(i)+  0.0200;fxR2CO3(i)=fxR2CO3(i)+  0.0300;fxHCHO(i)=fxHCHO(i)+  0.0300;fxGLY(i)=fxGLY(i)+  0.0100;fxRCNO3(i)=fxRCNO3(i)+  0.0400;fzRCNO3(i)=fzRCNO3(i)+  0.0200;fyRPNO3(i)=fyRPNO3(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.0200;fSumRO2(i)=fSumRO2(i)+  0.0900;

% 690, <N4HV>
i=i+1;
Rnames{ 690} = 'RCNO3 = RCNO3_HV +  0.77000*NO2 +  0.04000*xNO2 +  0.01000*xOH +  0.38000*HO2 +  0.19000*xHO2 +  0.53000*RO2C +  0.10000*RO2XC +  0.07000*ETO2 +  0.11000*MECO3 +  0.04000*R2CO3 +  0.03000*xR2CO3 +  0.11000*HCHO +  0.02000*xHCHO +  0.05000*MECHO +  0.02000*RCHO +  0.07000*xRCHO +  0.09000*OLEA2 +  0.03000*MACR +  0.04000*AFG2A +  0.07000*xACET +  0.04000*xKET2 +  0.04000*OACID +  0.03000*xOACID +  0.02000*PACID +  0.04000*xPACID +  0.02000*xRCNO3 +  0.08000*zRCNO3 +  0.03000*xRHNO3 +  0.17000*yRPNO3 +  0.02000*zRDNO3 +  0.22000*yHPCRB +  8.77000*NROG +  0.01000*CO2 +  0.34000*CO +  0.71000*SumRO2 +  0.14000*SumRCO3 ';
k(:,i) = (JCRBNIT ); 
Gstr{i,   1}='RCNO3';
fRCNO3(i)=fRCNO3(i)-1.0;
fRCNO3_HV(i)=fRCNO3_HV(i)+  1.0000;fNO2(i)=fNO2(i)+  0.7700;fxNO2(i)=fxNO2(i)+  0.0400;fxOH(i)=fxOH(i)+  0.0100;fHO2(i)=fHO2(i)+  0.3800;fxHO2(i)=fxHO2(i)+  0.1900;fRO2C(i)=fRO2C(i)+  0.5300;fRO2XC(i)=fRO2XC(i)+  0.1000;fETO2(i)=fETO2(i)+  0.0700;fMECO3(i)=fMECO3(i)+  0.1100;fR2CO3(i)=fR2CO3(i)+  0.0400;fxR2CO3(i)=fxR2CO3(i)+  0.0300;fHCHO(i)=fHCHO(i)+  0.1100;fxHCHO(i)=fxHCHO(i)+  0.0200;fMECHO(i)=fMECHO(i)+  0.0500;fRCHO(i)=fRCHO(i)+  0.0200;fxRCHO(i)=fxRCHO(i)+  0.0700;fOLEA2(i)=fOLEA2(i)+  0.0900;fMACR(i)=fMACR(i)+  0.0300;fAFG2A(i)=fAFG2A(i)+  0.0400;fxACET(i)=fxACET(i)+  0.0700;fxKET2(i)=fxKET2(i)+  0.0400;fOACID(i)=fOACID(i)+  0.0400;fxOACID(i)=fxOACID(i)+  0.0300;fPACID(i)=fPACID(i)+  0.0200;fxPACID(i)=fxPACID(i)+  0.0400;fxRCNO3(i)=fxRCNO3(i)+  0.0200;fzRCNO3(i)=fzRCNO3(i)+  0.0800;fxRHNO3(i)=fxRHNO3(i)+  0.0300;fyRPNO3(i)=fyRPNO3(i)+  0.1700;fzRDNO3(i)=fzRDNO3(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.2200;fNROG(i)=fNROG(i)+  8.7700;fCO2(i)=fCO2(i)+  0.0100;fCO(i)=fCO(i)+  0.3400;fSumRO2(i)=fSumRO2(i)+  0.7100;fSumRCO3(i)=fSumRCO3(i)+  0.1400;

% 691, <711>
i=i+1;
Rnames{ 691} = 'RCNO3_HV = 0.25000*HO2 +  0.06000*PACID +  0.02000*xPACID +  0.06000*RCNO3 +  0.02000*RPNO3 +  0.10000*HPCRB ';
k(:,i) = (  3.1500E-01 ); 
Gstr{i,   1}='RCNO3_HV';
fRCNO3_HV(i)=fRCNO3_HV(i)-1.0;
fHO2(i)=fHO2(i)+  0.2500;fPACID(i)=fPACID(i)+  0.0600;fxPACID(i)=fxPACID(i)+  0.0200;fRCNO3(i)=fRCNO3(i)+  0.0600;fRPNO3(i)=fRPNO3(i)+  0.0200;fHPCRB(i)=fHPCRB(i)+  0.1000;

% 692, <712>
i=i+1;
Rnames{ 692} = 'RCNO3_HV + NO = NO +  0.05000*xOH +  0.06000*xHO2 +  0.21000*RO2C +  0.04000*RO2XC +  0.05000*xMECO3 +  0.05000*xR2CO3 +  0.01000*xHCHO +  0.14000*xRCHO +  0.01000*xMGLY +  0.04000*zRCNO3 +  0.07000*xRHNO3 +  0.02000*yRPNO3 +  0.14000*yHPCRB +  0.05000*CO2 +  0.02000*CO +  0.25000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='RCNO3_HV';Gstr{i,   2}='NO';
fRCNO3_HV(i)=fRCNO3_HV(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxOH(i)=fxOH(i)+  0.0500;fxHO2(i)=fxHO2(i)+  0.0600;fRO2C(i)=fRO2C(i)+  0.2100;fRO2XC(i)=fRO2XC(i)+  0.0400;fxMECO3(i)=fxMECO3(i)+  0.0500;fxR2CO3(i)=fxR2CO3(i)+  0.0500;fxHCHO(i)=fxHCHO(i)+  0.0100;fxRCHO(i)=fxRCHO(i)+  0.1400;fxMGLY(i)=fxMGLY(i)+  0.0100;fzRCNO3(i)=fzRCNO3(i)+  0.0400;fxRHNO3(i)=fxRHNO3(i)+  0.0700;fyRPNO3(i)=fyRPNO3(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.1400;fCO2(i)=fCO2(i)+  0.0500;fCO(i)=fCO(i)+  0.0200;fSumRO2(i)=fSumRO2(i)+  0.2500;

% 693, <N3OH>
i=i+1;
Rnames{ 693} = 'RHNO3 + OH = 0.60000*NO2 +  0.08000*HO2 +  0.26000*xHO2 +  0.30000*RO2C +  0.05000*RO2XC +  0.07000*xHCHO +  0.03000*xGLCHO +  0.01000*KET2 +  0.07000*xKET2 +  0.01000*LVKS +  0.08000*RCNO3 +  0.21000*xRCNO3 +  0.05000*xRHNO3 +  0.34000*yRPNO3 +  0.05000*zRDNO3 +  0.02000*ALK4 +  0.56000*ALK5 +  0.35000*SumRO2 ';
k(:,i) = (  3.8000E-11 ); 
Gstr{i,   1}='RHNO3';Gstr{i,   2}='OH';
fRHNO3(i)=fRHNO3(i)-1.0;fOH(i)=fOH(i)-1.0;
fNO2(i)=fNO2(i)+  0.6000;fHO2(i)=fHO2(i)+  0.0800;fxHO2(i)=fxHO2(i)+  0.2600;fRO2C(i)=fRO2C(i)+  0.3000;fRO2XC(i)=fRO2XC(i)+  0.0500;fxHCHO(i)=fxHCHO(i)+  0.0700;fxGLCHO(i)=fxGLCHO(i)+  0.0300;fKET2(i)=fKET2(i)+  0.0100;fxKET2(i)=fxKET2(i)+  0.0700;fLVKS(i)=fLVKS(i)+  0.0100;fRCNO3(i)=fRCNO3(i)+  0.0800;fxRCNO3(i)=fxRCNO3(i)+  0.2100;fxRHNO3(i)=fxRHNO3(i)+  0.0500;fyRPNO3(i)=fyRPNO3(i)+  0.3400;fzRDNO3(i)=fzRDNO3(i)+  0.0500;fALK4(i)=fALK4(i)+  0.0200;fALK5(i)=fALK5(i)+  0.5600;fSumRO2(i)=fSumRO2(i)+  0.3500;

% 694, <N3HV>
i=i+1;
Rnames{ 694} = 'RHNO3 = NO2 +  0.94000*HO2 +  0.03000*xHO2 +  0.09000*RO2C +  0.02000*RO2XC +  0.74000*HCHO +  0.02000*xHCHO +  0.02000*MECHO +  0.06000*OLEA1 +  0.06000*OLEA2 +  0.02000*xOLEA2 +  0.32000*MACR +  0.02000*ACET +  0.03000*KET2 +  0.37000*MVK +  0.02000*OLEP +  0.01000*zRCNO3 +  0.01000*zRHNO3 +  0.05000*yRUOOH +  0.01000*HPCRB +  0.05000*yHPCRB +  0.07000*FURNS +  0.11000*SumRO2 ';
k(:,i) = (JIC3ONO2 ); 
Gstr{i,   1}='RHNO3';
fRHNO3(i)=fRHNO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;fHO2(i)=fHO2(i)+  0.9400;fxHO2(i)=fxHO2(i)+  0.0300;fRO2C(i)=fRO2C(i)+  0.0900;fRO2XC(i)=fRO2XC(i)+  0.0200;fHCHO(i)=fHCHO(i)+  0.7400;fxHCHO(i)=fxHCHO(i)+  0.0200;fMECHO(i)=fMECHO(i)+  0.0200;fOLEA1(i)=fOLEA1(i)+  0.0600;fOLEA2(i)=fOLEA2(i)+  0.0600;fxOLEA2(i)=fxOLEA2(i)+  0.0200;fMACR(i)=fMACR(i)+  0.3200;fACET(i)=fACET(i)+  0.0200;fKET2(i)=fKET2(i)+  0.0300;fMVK(i)=fMVK(i)+  0.3700;fOLEP(i)=fOLEP(i)+  0.0200;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fzRHNO3(i)=fzRHNO3(i)+  0.0100;fyRUOOH(i)=fyRUOOH(i)+  0.0500;fHPCRB(i)=fHPCRB(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.0500;fFURNS(i)=fFURNS(i)+  0.0700;fSumRO2(i)=fSumRO2(i)+  0.1100;

% 695, <N5OH>
i=i+1;
Rnames{ 695} = 'RANO3 + OH = 0.42000*NO2 +  0.58000*HO2 +  0.02000*AFG2A +  0.01000*OLEP +  0.47000*RCNO3 +  0.11000*RHNO3 +  0.01000*ALK5 +  0.37000*ALK6 ';
k(:,i) = (  4.4900E-11 ); 
Gstr{i,   1}='RANO3';Gstr{i,   2}='OH';
fRANO3(i)=fRANO3(i)-1.0;fOH(i)=fOH(i)-1.0;
fNO2(i)=fNO2(i)+  0.4200;fHO2(i)=fHO2(i)+  0.5800;fAFG2A(i)=fAFG2A(i)+  0.0200;fOLEP(i)=fOLEP(i)+  0.0100;fRCNO3(i)=fRCNO3(i)+  0.4700;fRHNO3(i)=fRHNO3(i)+  0.1100;fALK5(i)=fALK5(i)+  0.0100;fALK6(i)=fALK6(i)+  0.3700;

% 696, <N5HV>
i=i+1;
Rnames{ 696} = 'RANO3 = 0.66000*RCNO3 +  0.34000*RHNO3 ';
k(:,i) = (JCOOH ); 
Gstr{i,   1}='RANO3';
fRANO3(i)=fRANO3(i)-1.0;
fRCNO3(i)=fRCNO3(i)+  0.6600;fRHNO3(i)=fRHNO3(i)+  0.3400;

% 697, <N6OH>
i=i+1;
Rnames{ 697} = 'RPNO3 + OH = RPNO3_OH +  0.01000*zRNNO3 +  0.37000*NO2 +  0.07000*xNO2 +  0.19000*OH +  0.12000*HO2 +  0.11000*xHO2 +  0.23000*RO2C +  0.05000*RO2XC +  0.10000*xHCHO +  0.04000*xOLEA1 +  0.01000*xOLEA2 +  0.03000*xKET2 +  0.02000*xMVK +  0.03000*RCNO3 +  0.10000*xRCNO3 +  0.16000*RHNO3 +  0.12000*RPNO3 +  0.01000*xRPNO3 +  0.14000*yRPNO3 +  0.03000*zRDNO3 +  0.32000*ROOH +  0.05000*HPCRB +  0.01000*xHPCRB +  0.28000*SumRO2 ';
k(:,i) = (  5.1900E-11 ); 
Gstr{i,   1}='RPNO3';Gstr{i,   2}='OH';
fRPNO3(i)=fRPNO3(i)-1.0;fOH(i)=fOH(i)-1.0;
fRPNO3_OH(i)=fRPNO3_OH(i)+  1.0000;fzRNNO3(i)=fzRNNO3(i)+  0.0100;fNO2(i)=fNO2(i)+  0.3700;fxNO2(i)=fxNO2(i)+  0.0700;fOH(i)=fOH(i)+  0.1900;fHO2(i)=fHO2(i)+  0.1200;fxHO2(i)=fxHO2(i)+  0.1100;fRO2C(i)=fRO2C(i)+  0.2300;fRO2XC(i)=fRO2XC(i)+  0.0500;fxHCHO(i)=fxHCHO(i)+  0.1000;fxOLEA1(i)=fxOLEA1(i)+  0.0400;fxOLEA2(i)=fxOLEA2(i)+  0.0100;fxKET2(i)=fxKET2(i)+  0.0300;fxMVK(i)=fxMVK(i)+  0.0200;fRCNO3(i)=fRCNO3(i)+  0.0300;fxRCNO3(i)=fxRCNO3(i)+  0.1000;fRHNO3(i)=fRHNO3(i)+  0.1600;fRPNO3(i)=fRPNO3(i)+  0.1200;fxRPNO3(i)=fxRPNO3(i)+  0.0100;fyRPNO3(i)=fyRPNO3(i)+  0.1400;fzRDNO3(i)=fzRDNO3(i)+  0.0300;fROOH(i)=fROOH(i)+  0.3200;fHPCRB(i)=fHPCRB(i)+  0.0500;fxHPCRB(i)=fxHPCRB(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  0.2800;

% 698, <718>
i=i+1;
Rnames{ 698} = 'RPNO3_OH = 0.03000*NO2 +  0.05000*OH +  0.02000*HO2 +  0.05000*RCNO3 +  0.01000*RPNO3 +  0.03000*HPCRB ';
k(:,i) = (  4.7700E-01 ); 
Gstr{i,   1}='RPNO3_OH';
fRPNO3_OH(i)=fRPNO3_OH(i)-1.0;
fNO2(i)=fNO2(i)+  0.0300;fOH(i)=fOH(i)+  0.0500;fHO2(i)=fHO2(i)+  0.0200;fRCNO3(i)=fRCNO3(i)+  0.0500;fRPNO3(i)=fRPNO3(i)+  0.0100;fHPCRB(i)=fHPCRB(i)+  0.0300;

% 699, <719>
i=i+1;
Rnames{ 699} = 'RPNO3_OH + NO = NO +  0.03000*xNO2 +  0.05000*xHO2 +  0.09000*RO2C +  0.01000*RO2XC +  0.02000*xHCHO +  0.03000*xGLY +  0.02000*xOLEP +  0.03000*xRCNO3 +  0.02000*xRPNO3 +  0.10000*yRPNO3 +  0.01000*zRDNO3 +  0.02000*xHPCRB +  0.11000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='RPNO3_OH';Gstr{i,   2}='NO';
fRPNO3_OH(i)=fRPNO3_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxNO2(i)=fxNO2(i)+  0.0300;fxHO2(i)=fxHO2(i)+  0.0500;fRO2C(i)=fRO2C(i)+  0.0900;fRO2XC(i)=fRO2XC(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.0200;fxGLY(i)=fxGLY(i)+  0.0300;fxOLEP(i)=fxOLEP(i)+  0.0200;fxRCNO3(i)=fxRCNO3(i)+  0.0300;fxRPNO3(i)=fxRPNO3(i)+  0.0200;fyRPNO3(i)=fyRPNO3(i)+  0.1000;fzRDNO3(i)=fzRDNO3(i)+  0.0100;fxHPCRB(i)=fxHPCRB(i)+  0.0200;fSumRO2(i)=fSumRO2(i)+  0.1100;

% 700, <N6HV>
i=i+1;
Rnames{ 700} = 'RPNO3 = RPNO3_HV +  0.81000*NO2 + OH +  0.14000*HO2 +  0.01000*RO2C +  0.63000*HCHO +  0.43000*OLEA1 +  0.10000*OLEA2 +  0.12000*MVK +  0.15000*OLEP +  0.14000*RCNO3 +  0.01000*yRPNO3 +  0.01000*SumRO2 ';
k(:,i) = (JCOOH ); 
Gstr{i,   1}='RPNO3';
fRPNO3(i)=fRPNO3(i)-1.0;
fRPNO3_HV(i)=fRPNO3_HV(i)+  1.0000;fNO2(i)=fNO2(i)+  0.8100;fOH(i)=fOH(i)+  1.0000;fHO2(i)=fHO2(i)+  0.1400;fRO2C(i)=fRO2C(i)+  0.0100;fHCHO(i)=fHCHO(i)+  0.6300;fOLEA1(i)=fOLEA1(i)+  0.4300;fOLEA2(i)=fOLEA2(i)+  0.1000;fMVK(i)=fMVK(i)+  0.1200;fOLEP(i)=fOLEP(i)+  0.1500;fRCNO3(i)=fRCNO3(i)+  0.1400;fyRPNO3(i)=fyRPNO3(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  0.0100;

% 701, <721>
i=i+1;
Rnames{ 701} = 'RPNO3_HV = 0.03000*NO2 +  0.01000*HO2 +  0.01000*RPNO3 +  0.03000*HPCRB ';
k(:,i) = (  1.1100E+00 ); 
Gstr{i,   1}='RPNO3_HV';
fRPNO3_HV(i)=fRPNO3_HV(i)-1.0;
fNO2(i)=fNO2(i)+  0.0300;fHO2(i)=fHO2(i)+  0.0100;fRPNO3(i)=fRPNO3(i)+  0.0100;fHPCRB(i)=fHPCRB(i)+  0.0300;

% 702, <722>
i=i+1;
Rnames{ 702} = 'RPNO3_HV + NO = NO +  0.04000*xHO2 +  0.05000*RO2C +  0.01000*RO2XC +  0.03000*xHCHO +  0.03000*xRCNO3 +  0.01000*xRHNO3 +  0.05000*yRPNO3 +  0.05000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='RPNO3_HV';Gstr{i,   2}='NO';
fRPNO3_HV(i)=fRPNO3_HV(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0400;fRO2C(i)=fRO2C(i)+  0.0500;fRO2XC(i)=fRO2XC(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.0300;fxRCNO3(i)=fxRCNO3(i)+  0.0300;fxRHNO3(i)=fxRHNO3(i)+  0.0100;fyRPNO3(i)=fyRPNO3(i)+  0.0500;fSumRO2(i)=fSumRO2(i)+  0.0500;

% 703, <NDOH>
i=i+1;
Rnames{ 703} = 'RDNO3 + OH = 0.53000*NO2 +  0.04000*xNO2 +  0.01000*HO2 +  0.25000*xHO2 +  0.60000*RO2C +  0.15000*RO2XC +  0.20000*xHCHO +  0.03000*xACET +  0.10000*xKET2 +  0.02000*RCNO3 +  0.25000*xRCNO3 +  0.01000*zRCNO3 +  0.52000*RHNO3 +  0.05000*xRDNO3 +  0.13000*zRDNO3 +  0.01000*CO +  0.75000*SumRO2 ';
k(:,i) = (  3.6000E-11 ); 
Gstr{i,   1}='RDNO3';Gstr{i,   2}='OH';
fRDNO3(i)=fRDNO3(i)-1.0;fOH(i)=fOH(i)-1.0;
fNO2(i)=fNO2(i)+  0.5300;fxNO2(i)=fxNO2(i)+  0.0400;fHO2(i)=fHO2(i)+  0.0100;fxHO2(i)=fxHO2(i)+  0.2500;fRO2C(i)=fRO2C(i)+  0.6000;fRO2XC(i)=fRO2XC(i)+  0.1500;fxHCHO(i)=fxHCHO(i)+  0.2000;fxACET(i)=fxACET(i)+  0.0300;fxKET2(i)=fxKET2(i)+  0.1000;fRCNO3(i)=fRCNO3(i)+  0.0200;fxRCNO3(i)=fxRCNO3(i)+  0.2500;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fRHNO3(i)=fRHNO3(i)+  0.5200;fxRDNO3(i)=fxRDNO3(i)+  0.0500;fzRDNO3(i)=fzRDNO3(i)+  0.1300;fCO(i)=fCO(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  0.7500;

% 704, <NDHV>
i=i+1;
Rnames{ 704} = 'RDNO3 = RDNO3_HV +  1.86000*NO2 +  0.03000*HO2 +  0.01000*xHO2 +  0.08000*RO2C +  0.03000*RO2XC +  0.01000*xR2CO3 +  0.30000*HCHO +  0.21000*OLEA1 +  0.35000*OLEA2 +  0.02000*xACET +  0.05000*KET2 +  0.25000*MVK +  0.03000*RCNO3 +  0.01000*xRCNO3 +  0.03000*zRCNO3 +  0.11000*SumRO2 ';
k(:,i) = (JDIONO2 ); 
Gstr{i,   1}='RDNO3';
fRDNO3(i)=fRDNO3(i)-1.0;
fRDNO3_HV(i)=fRDNO3_HV(i)+  1.0000;fNO2(i)=fNO2(i)+  1.8600;fHO2(i)=fHO2(i)+  0.0300;fxHO2(i)=fxHO2(i)+  0.0100;fRO2C(i)=fRO2C(i)+  0.0800;fRO2XC(i)=fRO2XC(i)+  0.0300;fxR2CO3(i)=fxR2CO3(i)+  0.0100;fHCHO(i)=fHCHO(i)+  0.3000;fOLEA1(i)=fOLEA1(i)+  0.2100;fOLEA2(i)=fOLEA2(i)+  0.3500;fxACET(i)=fxACET(i)+  0.0200;fKET2(i)=fKET2(i)+  0.0500;fMVK(i)=fMVK(i)+  0.2500;fRCNO3(i)=fRCNO3(i)+  0.0300;fxRCNO3(i)=fxRCNO3(i)+  0.0100;fzRCNO3(i)=fzRCNO3(i)+  0.0300;fSumRO2(i)=fSumRO2(i)+  0.1100;

% 705, <725>
i=i+1;
Rnames{ 705} = 'RDNO3_HV = 0.04000*NO2 +  0.02000*HO2 +  0.02000*RPNO3 +  0.04000*HPCRB ';
k(:,i) = (  1.0900E+00 ); 
Gstr{i,   1}='RDNO3_HV';
fRDNO3_HV(i)=fRDNO3_HV(i)-1.0;
fNO2(i)=fNO2(i)+  0.0400;fHO2(i)=fHO2(i)+  0.0200;fRPNO3(i)=fRPNO3(i)+  0.0200;fHPCRB(i)=fHPCRB(i)+  0.0400;

% 706, <726>
i=i+1;
Rnames{ 706} = 'RDNO3_HV + NO = NO +  0.06000*xHO2 +  0.06000*RO2C +  0.01000*RO2XC +  0.04000*xHCHO +  0.04000*xRCNO3 +  0.02000*xRHNO3 +  0.06000*yRPNO3 +  0.01000*zRDNO3 +  0.07000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='RDNO3_HV';Gstr{i,   2}='NO';
fRDNO3_HV(i)=fRDNO3_HV(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.0600;fRO2C(i)=fRO2C(i)+  0.0600;fRO2XC(i)=fRO2XC(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.0400;fxRCNO3(i)=fxRCNO3(i)+  0.0400;fxRHNO3(i)=fxRHNO3(i)+  0.0200;fyRPNO3(i)=fyRPNO3(i)+  0.0600;fzRDNO3(i)=fzRDNO3(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  0.0700;

% 707, <N1OH>
i=i+1;
Rnames{ 707} = 'R1NO3 + OH = 0.17000*NO2 +  0.41000*xNO2 +  0.01000*HO2 +  0.29000*xHO2 +  0.99000*RO2C +  0.12000*RO2XC +  0.10000*xHCHO +  0.29000*xMECHO +  0.05000*xETCHO +  0.02000*xRCHO +  0.05000*ACET +  0.25000*xACET +  0.05000*MEK +  0.04000*xMEK +  0.07000*KET2 +  0.05000*xKET2 +  0.19000*xRCNO3 +  0.10000*xRHNO3 +  0.01000*RPNO3 +  1.12000*yRPNO3 +  0.12000*zRDNO3 +  1.12000*SumRO2 ';
k(:,i) = (  1.4700E-12 ); 
Gstr{i,   1}='R1NO3';Gstr{i,   2}='OH';
fR1NO3(i)=fR1NO3(i)-1.0;fOH(i)=fOH(i)-1.0;
fNO2(i)=fNO2(i)+  0.1700;fxNO2(i)=fxNO2(i)+  0.4100;fHO2(i)=fHO2(i)+  0.0100;fxHO2(i)=fxHO2(i)+  0.2900;fRO2C(i)=fRO2C(i)+  0.9900;fRO2XC(i)=fRO2XC(i)+  0.1200;fxHCHO(i)=fxHCHO(i)+  0.1000;fxMECHO(i)=fxMECHO(i)+  0.2900;fxETCHO(i)=fxETCHO(i)+  0.0500;fxRCHO(i)=fxRCHO(i)+  0.0200;fACET(i)=fACET(i)+  0.0500;fxACET(i)=fxACET(i)+  0.2500;fMEK(i)=fMEK(i)+  0.0500;fxMEK(i)=fxMEK(i)+  0.0400;fKET2(i)=fKET2(i)+  0.0700;fxKET2(i)=fxKET2(i)+  0.0500;fxRCNO3(i)=fxRCNO3(i)+  0.1900;fxRHNO3(i)=fxRHNO3(i)+  0.1000;fRPNO3(i)=fRPNO3(i)+  0.0100;fyRPNO3(i)=fyRPNO3(i)+  1.1200;fzRDNO3(i)=fzRDNO3(i)+  0.1200;fSumRO2(i)=fSumRO2(i)+  1.1200;

% 708, <N1HV>
i=i+1;
Rnames{ 708} = 'R1NO3 = NO2 +  0.19000*HO2 +  0.38000*xHO2 +  0.45000*RO2C +  0.06000*RO2XC +  0.32000*ETO2 +  0.04000*TBUO +  0.01000*xTBUO +  0.05000*xHCHO +  0.12000*MECHO +  0.03000*ETCHO +  0.03000*xETCHO +  0.02000*xRCHO +  0.34000*ACET +  0.12000*xACET +  0.11000*MEK +  0.05000*KET2 +  0.22000*xKET2 +  0.01000*zR1NO3 +  0.06000*zRHNO3 +  0.51000*yROOH +  0.83000*SumRO2 ';
k(:,i) = (JIC3ONO2 ); 
Gstr{i,   1}='R1NO3';
fR1NO3(i)=fR1NO3(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;fHO2(i)=fHO2(i)+  0.1900;fxHO2(i)=fxHO2(i)+  0.3800;fRO2C(i)=fRO2C(i)+  0.4500;fRO2XC(i)=fRO2XC(i)+  0.0600;fETO2(i)=fETO2(i)+  0.3200;fTBUO(i)=fTBUO(i)+  0.0400;fxTBUO(i)=fxTBUO(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.0500;fMECHO(i)=fMECHO(i)+  0.1200;fETCHO(i)=fETCHO(i)+  0.0300;fxETCHO(i)=fxETCHO(i)+  0.0300;fxRCHO(i)=fxRCHO(i)+  0.0200;fACET(i)=fACET(i)+  0.3400;fxACET(i)=fxACET(i)+  0.1200;fMEK(i)=fMEK(i)+  0.1100;fKET2(i)=fKET2(i)+  0.0500;fxKET2(i)=fxKET2(i)+  0.2200;fzR1NO3(i)=fzR1NO3(i)+  0.0100;fzRHNO3(i)=fzRHNO3(i)+  0.0600;fyROOH(i)=fyROOH(i)+  0.5100;fSumRO2(i)=fSumRO2(i)+  0.8300;

% 709, <N2OH>
i=i+1;
Rnames{ 709} = 'R2NO3 + OH = 0.06000*NO2 +  0.12000*xNO2 +  0.01000*HO2 +  0.52000*xHO2 +  1.12000*RO2C +  0.29000*RO2XC +  0.06000*xRCHO +  0.04000*KET2 +  0.07000*xKET2 +  0.01000*RCNO3 +  0.49000*xRCNO3 +  0.01000*zRCNO3 +  0.03000*xRHNO3 +  1.36000*yRPNO3 +  0.29000*zRDNO3 +  0.01000*HPCRB +  0.01000*CO +  1.42000*SumRO2 ';
k(:,i) = (  2.5100E-11 ); 
Gstr{i,   1}='R2NO3';Gstr{i,   2}='OH';
fR2NO3(i)=fR2NO3(i)-1.0;fOH(i)=fOH(i)-1.0;
fNO2(i)=fNO2(i)+  0.0600;fxNO2(i)=fxNO2(i)+  0.1200;fHO2(i)=fHO2(i)+  0.0100;fxHO2(i)=fxHO2(i)+  0.5200;fRO2C(i)=fRO2C(i)+  1.1200;fRO2XC(i)=fRO2XC(i)+  0.2900;fxRCHO(i)=fxRCHO(i)+  0.0600;fKET2(i)=fKET2(i)+  0.0400;fxKET2(i)=fxKET2(i)+  0.0700;fRCNO3(i)=fRCNO3(i)+  0.0100;fxRCNO3(i)=fxRCNO3(i)+  0.4900;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fxRHNO3(i)=fxRHNO3(i)+  0.0300;fyRPNO3(i)=fyRPNO3(i)+  1.3600;fzRDNO3(i)=fzRDNO3(i)+  0.2900;fHPCRB(i)=fHPCRB(i)+  0.0100;fCO(i)=fCO(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  1.4200;

% 710, <N2HV>
i=i+1;
Rnames{ 710} = 'R2NO3 = R2NO3_HV + NO2 +  0.10000*HO2 +  0.46000*xHO2 +  0.74000*RO2C +  0.21000*RO2XC +  0.05000*xMACO3 +  0.04000*xRCHO +  0.08000*KET2 +  0.37000*xKET2 +  0.04000*xMVK +  0.03000*xPACID +  0.07000*zRCNO3 +  0.14000*zRHNO3 +  0.51000*yROOH +  0.02000*HPCRB +  0.36000*yHPCRB +  0.01000*CO +  0.95000*SumRO2 ';
k(:,i) = (JIC3ONO2 ); 
Gstr{i,   1}='R2NO3';
fR2NO3(i)=fR2NO3(i)-1.0;
fR2NO3_HV(i)=fR2NO3_HV(i)+  1.0000;fNO2(i)=fNO2(i)+  1.0000;fHO2(i)=fHO2(i)+  0.1000;fxHO2(i)=fxHO2(i)+  0.4600;fRO2C(i)=fRO2C(i)+  0.7400;fRO2XC(i)=fRO2XC(i)+  0.2100;fxMACO3(i)=fxMACO3(i)+  0.0500;fxRCHO(i)=fxRCHO(i)+  0.0400;fKET2(i)=fKET2(i)+  0.0800;fxKET2(i)=fxKET2(i)+  0.3700;fxMVK(i)=fxMVK(i)+  0.0400;fxPACID(i)=fxPACID(i)+  0.0300;fzRCNO3(i)=fzRCNO3(i)+  0.0700;fzRHNO3(i)=fzRHNO3(i)+  0.1400;fyROOH(i)=fyROOH(i)+  0.5100;fHPCRB(i)=fHPCRB(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.3600;fCO(i)=fCO(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  0.9500;

% 711, <731>
i=i+1;
Rnames{ 711} = 'R2NO3_HV = 0.09000*OH +  0.09000*HO2 +  0.07000*PACID +  0.03000*xPACID +  0.10000*HPCRB ';
k(:,i) = (  9.6900E-01 ); 
Gstr{i,   1}='R2NO3_HV';
fR2NO3_HV(i)=fR2NO3_HV(i)-1.0;
fOH(i)=fOH(i)+  0.0900;fHO2(i)=fHO2(i)+  0.0900;fPACID(i)=fPACID(i)+  0.0700;fxPACID(i)=fxPACID(i)+  0.0300;fHPCRB(i)=fHPCRB(i)+  0.1000;

% 712, <732>
i=i+1;
Rnames{ 712} = 'R2NO3_HV + NO = NO +  0.13000*xHO2 +  0.22000*RO2C +  0.05000*RO2XC +  0.03000*xHCHO +  0.11000*xRCHO +  0.04000*xOLEA2 +  0.03000*zRCNO3 +  0.02000*zRHNO3 +  0.29000*yHPCRB +  0.01000*CO +  0.27000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='R2NO3_HV';Gstr{i,   2}='NO';
fR2NO3_HV(i)=fR2NO3_HV(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.1300;fRO2C(i)=fRO2C(i)+  0.2200;fRO2XC(i)=fRO2XC(i)+  0.0500;fxHCHO(i)=fxHCHO(i)+  0.0300;fxRCHO(i)=fxRCHO(i)+  0.1100;fxOLEA2(i)=fxOLEA2(i)+  0.0400;fzRCNO3(i)=fzRCNO3(i)+  0.0300;fzRHNO3(i)=fzRHNO3(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.2900;fCO(i)=fCO(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  0.2700;

% 713, <H4OH>
i=i+1;
Rnames{ 713} = 'RAOOH + OH = 0.76000*OH +  0.17000*HO2 +  0.06000*xHO2 +  0.06000*RO2C +  0.01000*RO2XC +  0.31000*RCHO +  0.02000*xGLY +  0.04000*xMGLY +  0.02000*xBUDAL +  0.02000*AFG2A +  0.03000*xAFG2A +  0.01000*KET2 +  0.03000*OLEP +  0.01000*zRANO3 +  0.07000*yRAOOH +  0.17000*HPCRB +  0.01000*ALK5 +  0.37000*ALK6 +  0.07000*SumRO2 ';
k(:,i) = (  8.2700E-11 ); 
Gstr{i,   1}='RAOOH';Gstr{i,   2}='OH';
fRAOOH(i)=fRAOOH(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  0.7600;fHO2(i)=fHO2(i)+  0.1700;fxHO2(i)=fxHO2(i)+  0.0600;fRO2C(i)=fRO2C(i)+  0.0600;fRO2XC(i)=fRO2XC(i)+  0.0100;fRCHO(i)=fRCHO(i)+  0.3100;fxGLY(i)=fxGLY(i)+  0.0200;fxMGLY(i)=fxMGLY(i)+  0.0400;fxBUDAL(i)=fxBUDAL(i)+  0.0200;fAFG2A(i)=fAFG2A(i)+  0.0200;fxAFG2A(i)=fxAFG2A(i)+  0.0300;fKET2(i)=fKET2(i)+  0.0100;fOLEP(i)=fOLEP(i)+  0.0300;fzRANO3(i)=fzRANO3(i)+  0.0100;fyRAOOH(i)=fyRAOOH(i)+  0.0700;fHPCRB(i)=fHPCRB(i)+  0.1700;fALK5(i)=fALK5(i)+  0.0100;fALK6(i)=fALK6(i)+  0.3700;fSumRO2(i)=fSumRO2(i)+  0.0700;

% 714, <H4HV>
i=i+1;
Rnames{ 714} = 'RAOOH = HPCRB ';
k(:,i) = (JCOOH ); 
Gstr{i,   1}='RAOOH';
fRAOOH(i)=fRAOOH(i)-1.0;
fHPCRB(i)=fHPCRB(i)+  1.0000;

% 715, <H3OH>
i=i+1;
Rnames{ 715} = 'RUOOH + OH = RUOOH_OH +  0.63000*OH +  0.08000*HO2 +  0.10000*xHO2 +  0.11000*RO2C +  0.01000*RO2XC +  0.08000*xHCHO +  0.03000*xMACR +  0.05000*xMVK +  0.02000*LVKS +  0.01000*zRHNO3 +  0.01000*yROOH +  0.10000*yRUOOH +  0.08000*HPCRB +  0.01000*xHPCRB +  0.02000*ALK4 +  0.59000*ALK5 +  0.01000*xFURNS +  0.12000*SumRO2 ';
k(:,i) = (  5.9700E-11 ); 
Gstr{i,   1}='RUOOH';Gstr{i,   2}='OH';
fRUOOH(i)=fRUOOH(i)-1.0;fOH(i)=fOH(i)-1.0;
fRUOOH_OH(i)=fRUOOH_OH(i)+  1.0000;fOH(i)=fOH(i)+  0.6300;fHO2(i)=fHO2(i)+  0.0800;fxHO2(i)=fxHO2(i)+  0.1000;fRO2C(i)=fRO2C(i)+  0.1100;fRO2XC(i)=fRO2XC(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.0800;fxMACR(i)=fxMACR(i)+  0.0300;fxMVK(i)=fxMVK(i)+  0.0500;fLVKS(i)=fLVKS(i)+  0.0200;fzRHNO3(i)=fzRHNO3(i)+  0.0100;fyROOH(i)=fyROOH(i)+  0.0100;fyRUOOH(i)=fyRUOOH(i)+  0.1000;fHPCRB(i)=fHPCRB(i)+  0.0800;fxHPCRB(i)=fxHPCRB(i)+  0.0100;fALK4(i)=fALK4(i)+  0.0200;fALK5(i)=fALK5(i)+  0.5900;fxFURNS(i)=fxFURNS(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  0.1200;

% 716, <736>
i=i+1;
Rnames{ 716} = 'RUOOH_OH = 0.06000*OH +  0.12000*HO2 +  0.17000*HPCRB ';
k(:,i) = (  1.2800E-02 ); 
Gstr{i,   1}='RUOOH_OH';
fRUOOH_OH(i)=fRUOOH_OH(i)-1.0;
fOH(i)=fOH(i)+  0.0600;fHO2(i)=fHO2(i)+  0.1200;fHPCRB(i)=fHPCRB(i)+  0.1700;

% 717, <737>
i=i+1;
Rnames{ 717} = 'RUOOH_OH + NO = NO +  0.16000*xHO2 +  0.16000*RO2C +  0.01000*RO2XC +  0.03000*xHCHO +  0.03000*xGLCHO +  0.05000*xKET2 +  0.01000*zRPNO3 +  0.17000*yROOH +  0.16000*xHPCRB +  0.17000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='RUOOH_OH';Gstr{i,   2}='NO';
fRUOOH_OH(i)=fRUOOH_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.1600;fRO2C(i)=fRO2C(i)+  0.1600;fRO2XC(i)=fRO2XC(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.0300;fxGLCHO(i)=fxGLCHO(i)+  0.0300;fxKET2(i)=fxKET2(i)+  0.0500;fzRPNO3(i)=fzRPNO3(i)+  0.0100;fyROOH(i)=fyROOH(i)+  0.1700;fxHPCRB(i)=fxHPCRB(i)+  0.1600;fSumRO2(i)=fSumRO2(i)+  0.1700;

% 718, <H3HV>
i=i+1;
Rnames{ 718} = 'RUOOH = OH +  0.99000*HO2 +  0.02000*RO2C +  0.86000*HCHO +  0.04000*OLEA1 +  0.39000*MACR +  0.01000*KET2 +  0.46000*MVK +  0.01000*OLEP +  0.01000*yRUOOH +  0.01000*yHPCRB +  0.09000*FURNS +  0.02000*SumRO2 ';
k(:,i) = (JCOOH ); 
Gstr{i,   1}='RUOOH';
fRUOOH(i)=fRUOOH(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fHO2(i)=fHO2(i)+  0.9900;fRO2C(i)=fRO2C(i)+  0.0200;fHCHO(i)=fHCHO(i)+  0.8600;fOLEA1(i)=fOLEA1(i)+  0.0400;fMACR(i)=fMACR(i)+  0.3900;fKET2(i)=fKET2(i)+  0.0100;fMVK(i)=fMVK(i)+  0.4600;fOLEP(i)=fOLEP(i)+  0.0100;fyRUOOH(i)=fyRUOOH(i)+  0.0100;fyHPCRB(i)=fyHPCRB(i)+  0.0100;fFURNS(i)=fFURNS(i)+  0.0900;fSumRO2(i)=fSumRO2(i)+  0.0200;

% 719, <H5OH>
i=i+1;
Rnames{ 719} = 'HPCRB + OH = HPCRB_OH +  0.50000*OH +  0.01000*HO2 +  0.03000*xHO2 +  0.03000*RO2C +  0.03000*xHCHO +  0.35000*RCHO +  0.01000*OLEA1 +  0.02000*AFG1 +  0.03000*xPACID +  0.13000*OLEP +  0.01000*HPCRB +  0.03000*SumRO2 ';
k(:,i) = (  5.4000E-11 ); 
Gstr{i,   1}='HPCRB';Gstr{i,   2}='OH';
fHPCRB(i)=fHPCRB(i)-1.0;fOH(i)=fOH(i)-1.0;
fHPCRB_OH(i)=fHPCRB_OH(i)+  1.0000;fOH(i)=fOH(i)+  0.5000;fHO2(i)=fHO2(i)+  0.0100;fxHO2(i)=fxHO2(i)+  0.0300;fRO2C(i)=fRO2C(i)+  0.0300;fxHCHO(i)=fxHCHO(i)+  0.0300;fRCHO(i)=fRCHO(i)+  0.3500;fOLEA1(i)=fOLEA1(i)+  0.0100;fAFG1(i)=fAFG1(i)+  0.0200;fxPACID(i)=fxPACID(i)+  0.0300;fOLEP(i)=fOLEP(i)+  0.1300;fHPCRB(i)=fHPCRB(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  0.0300;

% 720, <740>
i=i+1;
Rnames{ 720} = 'HPCRB_OH = 0.44000*OH +  0.02000*HO2 +  0.01000*AFG2B +  0.01000*PACID +  0.08000*OLEP +  0.36000*HPCRB ';
k(:,i) = (  6.3700E-01 ); 
Gstr{i,   1}='HPCRB_OH';
fHPCRB_OH(i)=fHPCRB_OH(i)-1.0;
fOH(i)=fOH(i)+  0.4400;fHO2(i)=fHO2(i)+  0.0200;fAFG2B(i)=fAFG2B(i)+  0.0100;fPACID(i)=fPACID(i)+  0.0100;fOLEP(i)=fOLEP(i)+  0.0800;fHPCRB(i)=fHPCRB(i)+  0.3600;

% 721, <741>
i=i+1;
Rnames{ 721} = 'HPCRB_OH + NO = NO +  0.42000*xHO2 +  0.43000*RO2C +  0.04000*RO2XC +  0.01000*xHCHO +  0.05000*xGLY +  0.01000*xGLCHO +  0.24000*xMGLY +  0.05000*xAFG2B +  0.01000*xPACID +  0.01000*zRCNO3 +  0.03000*zRPNO3 +  0.34000*xHPCRB +  0.45000*yHPCRB +  0.04000*CO +  0.47000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='HPCRB_OH';Gstr{i,   2}='NO';
fHPCRB_OH(i)=fHPCRB_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.4200;fRO2C(i)=fRO2C(i)+  0.4300;fRO2XC(i)=fRO2XC(i)+  0.0400;fxHCHO(i)=fxHCHO(i)+  0.0100;fxGLY(i)=fxGLY(i)+  0.0500;fxGLCHO(i)=fxGLCHO(i)+  0.0100;fxMGLY(i)=fxMGLY(i)+  0.2400;fxAFG2B(i)=fxAFG2B(i)+  0.0500;fxPACID(i)=fxPACID(i)+  0.0100;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fzRPNO3(i)=fzRPNO3(i)+  0.0300;fxHPCRB(i)=fxHPCRB(i)+  0.3400;fyHPCRB(i)=fyHPCRB(i)+  0.4500;fCO(i)=fCO(i)+  0.0400;fSumRO2(i)=fSumRO2(i)+  0.4700;

% 722, <H5HV>
i=i+1;
Rnames{ 722} = 'HPCRB = OH +  0.01000*xOH +  0.90000*HO2 +  0.08000*xHO2 +  0.09000*RO2C +  0.01000*RO2XC +  0.11000*HCHO +  0.06000*xHCHO +  0.11000*OLEA1 +  0.05000*AFG1 +  0.21000*AFG2A +  0.53000*AFG2B +  0.02000*xAFG2B +  0.01000*xLVKS +  0.06000*xPACID +  0.01000*zRCNO3 +  0.01000*CO2 +  0.10000*SumRO2 ';
k(:,i) = (  1.0000E-01.*JHPALDS ); 
Gstr{i,   1}='HPCRB';
fHPCRB(i)=fHPCRB(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fxOH(i)=fxOH(i)+  0.0100;fHO2(i)=fHO2(i)+  0.9000;fxHO2(i)=fxHO2(i)+  0.0800;fRO2C(i)=fRO2C(i)+  0.0900;fRO2XC(i)=fRO2XC(i)+  0.0100;fHCHO(i)=fHCHO(i)+  0.1100;fxHCHO(i)=fxHCHO(i)+  0.0600;fOLEA1(i)=fOLEA1(i)+  0.1100;fAFG1(i)=fAFG1(i)+  0.0500;fAFG2A(i)=fAFG2A(i)+  0.2100;fAFG2B(i)=fAFG2B(i)+  0.5300;fxAFG2B(i)=fxAFG2B(i)+  0.0200;fxLVKS(i)=fxLVKS(i)+  0.0100;fxPACID(i)=fxPACID(i)+  0.0600;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fCO2(i)=fCO2(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  0.1000;

% 723, <H2OH>
i=i+1;
Rnames{ 723} = 'ROOH + OH = ROOH_OH +  0.19000*OH +  0.03000*xOH +  0.22000*HO2 +  0.37000*xHO2 +  0.56000*RO2C +  0.04000*RO2XC +  0.07000*xETO2 +  0.02000*xTBUO +  0.01000*HCHO +  0.35000*xHCHO +  0.11000*xMECHO +  0.01000*xETCHO +  0.01000*RCHO +  0.01000*xRCHO +  0.06000*GLCHO +  0.04000*xGLCHO +  0.02000*ACET +  0.15000*xACET +  0.02000*MEK +  0.02000*xMEK +  0.08000*KET2 +  0.04000*xKET2 +  0.02000*zR1NO3 +  0.01000*zRHNO3 +  0.01000*zRPNO3 +  0.59000*yROOH +  0.22000*HPCRB +  0.01000*yHPCRB +  0.60000*SumRO2 ';
k(:,i) = (  1.1600E-11 ); 
Gstr{i,   1}='ROOH';Gstr{i,   2}='OH';
fROOH(i)=fROOH(i)-1.0;fOH(i)=fOH(i)-1.0;
fROOH_OH(i)=fROOH_OH(i)+  1.0000;fOH(i)=fOH(i)+  0.1900;fxOH(i)=fxOH(i)+  0.0300;fHO2(i)=fHO2(i)+  0.2200;fxHO2(i)=fxHO2(i)+  0.3700;fRO2C(i)=fRO2C(i)+  0.5600;fRO2XC(i)=fRO2XC(i)+  0.0400;fxETO2(i)=fxETO2(i)+  0.0700;fxTBUO(i)=fxTBUO(i)+  0.0200;fHCHO(i)=fHCHO(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.3500;fxMECHO(i)=fxMECHO(i)+  0.1100;fxETCHO(i)=fxETCHO(i)+  0.0100;fRCHO(i)=fRCHO(i)+  0.0100;fxRCHO(i)=fxRCHO(i)+  0.0100;fGLCHO(i)=fGLCHO(i)+  0.0600;fxGLCHO(i)=fxGLCHO(i)+  0.0400;fACET(i)=fACET(i)+  0.0200;fxACET(i)=fxACET(i)+  0.1500;fMEK(i)=fMEK(i)+  0.0200;fxMEK(i)=fxMEK(i)+  0.0200;fKET2(i)=fKET2(i)+  0.0800;fxKET2(i)=fxKET2(i)+  0.0400;fzR1NO3(i)=fzR1NO3(i)+  0.0200;fzRHNO3(i)=fzRHNO3(i)+  0.0100;fzRPNO3(i)=fzRPNO3(i)+  0.0100;fyROOH(i)=fyROOH(i)+  0.5900;fHPCRB(i)=fHPCRB(i)+  0.2200;fyHPCRB(i)=fyHPCRB(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  0.6000;

% 724, <744>
i=i+1;
Rnames{ 724} = 'ROOH_OH = 0.04000*OH +  0.01000*HO2 +  0.05000*HPCRB ';
k(:,i) = (  6.7800E-02 ); 
Gstr{i,   1}='ROOH_OH';
fROOH_OH(i)=fROOH_OH(i)-1.0;
fOH(i)=fOH(i)+  0.0400;fHO2(i)=fHO2(i)+  0.0100;fHPCRB(i)=fHPCRB(i)+  0.0500;

% 725, <745>
i=i+1;
Rnames{ 725} = 'ROOH_OH + NO = NO +  0.03000*xOH +  0.01000*xHO2 +  0.05000*RO2C +  0.01000*RO2XC +  0.01000*xHCHO +  0.03000*xMECHO +  0.01000*xETCHO +  0.01000*xRCHO +  0.01000*xACET +  0.01000*zRPNO3 +  0.06000*yROOH +  0.01000*xHPCRB +  0.06000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='ROOH_OH';Gstr{i,   2}='NO';
fROOH_OH(i)=fROOH_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxOH(i)=fxOH(i)+  0.0300;fxHO2(i)=fxHO2(i)+  0.0100;fRO2C(i)=fRO2C(i)+  0.0500;fRO2XC(i)=fRO2XC(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.0100;fxMECHO(i)=fxMECHO(i)+  0.0300;fxETCHO(i)=fxETCHO(i)+  0.0100;fxRCHO(i)=fxRCHO(i)+  0.0100;fxACET(i)=fxACET(i)+  0.0100;fzRPNO3(i)=fzRPNO3(i)+  0.0100;fyROOH(i)=fyROOH(i)+  0.0600;fxHPCRB(i)=fxHPCRB(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  0.0600;

% 726, <H2HV>
i=i+1;
Rnames{ 726} = 'ROOH = OH +  0.74000*HO2 +  0.09000*xHO2 +  0.13000*RO2C +  0.02000*RO2XC +  0.12000*ETO2 +  0.02000*TBUO +  0.77000*HCHO +  0.25000*MECHO +  0.01000*ETCHO +  0.01000*xETCHO +  0.01000*xRCHO +  0.10000*GLCHO +  0.18000*ACET +  0.03000*xACET +  0.04000*MEK +  0.06000*KET2 +  0.05000*xKET2 +  0.01000*zRCNO3 +  0.01000*zRHNO3 +  0.10000*yROOH +  0.03000*yHPCRB +  0.27000*SumRO2 ';
k(:,i) = (JCOOH ); 
Gstr{i,   1}='ROOH';
fROOH(i)=fROOH(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fHO2(i)=fHO2(i)+  0.7400;fxHO2(i)=fxHO2(i)+  0.0900;fRO2C(i)=fRO2C(i)+  0.1300;fRO2XC(i)=fRO2XC(i)+  0.0200;fETO2(i)=fETO2(i)+  0.1200;fTBUO(i)=fTBUO(i)+  0.0200;fHCHO(i)=fHCHO(i)+  0.7700;fMECHO(i)=fMECHO(i)+  0.2500;fETCHO(i)=fETCHO(i)+  0.0100;fxETCHO(i)=fxETCHO(i)+  0.0100;fxRCHO(i)=fxRCHO(i)+  0.0100;fGLCHO(i)=fGLCHO(i)+  0.1000;fACET(i)=fACET(i)+  0.1800;fxACET(i)=fxACET(i)+  0.0300;fMEK(i)=fMEK(i)+  0.0400;fKET2(i)=fKET2(i)+  0.0600;fxKET2(i)=fxKET2(i)+  0.0500;fzRCNO3(i)=fzRCNO3(i)+  0.0100;fzRHNO3(i)=fzRHNO3(i)+  0.0100;fyROOH(i)=fyROOH(i)+  0.1000;fyHPCRB(i)=fyHPCRB(i)+  0.0300;fSumRO2(i)=fSumRO2(i)+  0.2700;

% 727, <F2OH>
i=i+1;
Rnames{ 727} = 'AFG1 + OH = AFG1_OH +  0.43000*OH +  0.02000*HO2 +  0.19000*xHO2 +  0.19000*RO2C +  0.02000*RO2XC +  0.18000*xGLY +  0.41000*MALAH +  0.03000*xPACID +  0.03000*HPCRB +  0.01000*CO +  0.21000*SumRO2 ';
k(:,i) = (  3.3900E-11 ); 
Gstr{i,   1}='AFG1';Gstr{i,   2}='OH';
fAFG1(i)=fAFG1(i)-1.0;fOH(i)=fOH(i)-1.0;
fAFG1_OH(i)=fAFG1_OH(i)+  1.0000;fOH(i)=fOH(i)+  0.4300;fHO2(i)=fHO2(i)+  0.0200;fxHO2(i)=fxHO2(i)+  0.1900;fRO2C(i)=fRO2C(i)+  0.1900;fRO2XC(i)=fRO2XC(i)+  0.0200;fxGLY(i)=fxGLY(i)+  0.1800;fMALAH(i)=fMALAH(i)+  0.4100;fxPACID(i)=fxPACID(i)+  0.0300;fHPCRB(i)=fHPCRB(i)+  0.0300;fCO(i)=fCO(i)+  0.0100;fSumRO2(i)=fSumRO2(i)+  0.2100;

% 728, <748>
i=i+1;
Rnames{ 728} = 'AFG1_OH = 0.02000*OH +  0.33000*HO2 +  0.02000*PACID +  0.16000*xPACID +  0.02000*zRCNO3 +  0.33000*HPCRB ';
k(:,i) = (  1.3100E+01 ); 
Gstr{i,   1}='AFG1_OH';
fAFG1_OH(i)=fAFG1_OH(i)-1.0;
fOH(i)=fOH(i)+  0.0200;fHO2(i)=fHO2(i)+  0.3300;fPACID(i)=fPACID(i)+  0.0200;fxPACID(i)=fxPACID(i)+  0.1600;fzRCNO3(i)=fzRCNO3(i)+  0.0200;fHPCRB(i)=fHPCRB(i)+  0.3300;

% 729, <749>
i=i+1;
Rnames{ 729} = 'AFG1_OH + NO = NO +  0.30000*xHO2 +  0.32000*RO2C +  0.03000*RO2XC +  0.30000*xGLY +  0.46000*xMGLY +  0.02000*MALAH +  0.43000*yHPCRB +  0.35000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='AFG1_OH';Gstr{i,   2}='NO';
fAFG1_OH(i)=fAFG1_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.3000;fRO2C(i)=fRO2C(i)+  0.3200;fRO2XC(i)=fRO2XC(i)+  0.0300;fxGLY(i)=fxGLY(i)+  0.3000;fxMGLY(i)=fxMGLY(i)+  0.4600;fMALAH(i)=fMALAH(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.4300;fSumRO2(i)=fSumRO2(i)+  0.3500;

% 730, <F2HV>
i=i+1;
Rnames{ 730} = 'AFG1 = AFG1_HV +  0.30000*OH +  0.94000*HO2 +  0.03000*xHO2 +  0.03000*RO2C +  0.06000*MEO2 +  0.01000*AFG1 +  0.02000*AFG2A +  0.26000*PACID +  0.03000*xPACID +  0.02000*HPCRB +  0.03000*CO +  0.09000*SumRO2 ';
k(:,i) = (  2.2000E-01.*JAFGS ); 
Gstr{i,   1}='AFG1';
fAFG1(i)=fAFG1(i)-1.0;
fAFG1_HV(i)=fAFG1_HV(i)+  1.0000;fOH(i)=fOH(i)+  0.3000;fHO2(i)=fHO2(i)+  0.9400;fxHO2(i)=fxHO2(i)+  0.0300;fRO2C(i)=fRO2C(i)+  0.0300;fMEO2(i)=fMEO2(i)+  0.0600;fAFG1(i)=fAFG1(i)+  0.0100;fAFG2A(i)=fAFG2A(i)+  0.0200;fPACID(i)=fPACID(i)+  0.2600;fxPACID(i)=fxPACID(i)+  0.0300;fHPCRB(i)=fHPCRB(i)+  0.0200;fCO(i)=fCO(i)+  0.0300;fSumRO2(i)=fSumRO2(i)+  0.0900;

% 731, <751>
i=i+1;
Rnames{ 731} = 'AFG1_HV = 0.67000*OH +  0.33000*PACID +  0.33000*HPCRB ';
k(:,i) = (  1.7500E+01 ); 
Gstr{i,   1}='AFG1_HV';
fAFG1_HV(i)=fAFG1_HV(i)-1.0;
fOH(i)=fOH(i)+  0.6700;fPACID(i)=fPACID(i)+  0.3300;fHPCRB(i)=fHPCRB(i)+  0.3300;

% 732, <752>
i=i+1;
Rnames{ 732} = 'AFG1_HV + NO = NO +  0.52000*xOH +  0.07000*xHO2 +  0.59000*RO2C +  0.08000*RO2XC +  0.52000*MALAH +  0.08000*zRCNO3 +  0.67000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='AFG1_HV';Gstr{i,   2}='NO';
fAFG1_HV(i)=fAFG1_HV(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxOH(i)=fxOH(i)+  0.5200;fxHO2(i)=fxHO2(i)+  0.0700;fRO2C(i)=fRO2C(i)+  0.5900;fRO2XC(i)=fRO2XC(i)+  0.0800;fMALAH(i)=fMALAH(i)+  0.5200;fzRCNO3(i)=fzRCNO3(i)+  0.0800;fSumRO2(i)=fSumRO2(i)+  0.6700;

% 733, <F3OH>
i=i+1;
Rnames{ 733} = 'AFG2A + OH = AFG2A_OH +  0.57000*xHO2 +  0.73000*RO2C +  0.07000*RO2XC +  0.15000*xMECO3 +  0.01000*xR2CO3 +  0.20000*MACO3 +  0.03000*xRCHO +  0.47000*xGLY +  0.54000*xMGLY +  0.02000*xPACID +  0.07000*zRCNO3 +  0.46000*yHPCRB +  0.03000*CO +  0.80000*SumRO2 +  0.20000*SumRCO3 ';
k(:,i) = (  5.9900E-11 ); 
Gstr{i,   1}='AFG2A';Gstr{i,   2}='OH';
fAFG2A(i)=fAFG2A(i)-1.0;fOH(i)=fOH(i)-1.0;
fAFG2A_OH(i)=fAFG2A_OH(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.5700;fRO2C(i)=fRO2C(i)+  0.7300;fRO2XC(i)=fRO2XC(i)+  0.0700;fxMECO3(i)=fxMECO3(i)+  0.1500;fxR2CO3(i)=fxR2CO3(i)+  0.0100;fMACO3(i)=fMACO3(i)+  0.2000;fxRCHO(i)=fxRCHO(i)+  0.0300;fxGLY(i)=fxGLY(i)+  0.4700;fxMGLY(i)=fxMGLY(i)+  0.5400;fxPACID(i)=fxPACID(i)+  0.0200;fzRCNO3(i)=fzRCNO3(i)+  0.0700;fyHPCRB(i)=fyHPCRB(i)+  0.4600;fCO(i)=fCO(i)+  0.0300;fSumRO2(i)=fSumRO2(i)+  0.8000;fSumRCO3(i)=fSumRCO3(i)+  0.2000;

% 734, <754>
i=i+1;
Rnames{ 734} = 'AFG2A_OH = 0.22000*xPACID ';
k(:,i) = (  2.3400E+01 ); 
Gstr{i,   1}='AFG2A_OH';
fAFG2A_OH(i)=fAFG2A_OH(i)-1.0;
fxPACID(i)=fxPACID(i)+  0.2200;

% 735, <755>
i=i+1;
Rnames{ 735} = 'AFG2A_OH + NO = NO +  0.14000*xRCHO +  0.06000*xGLY +  0.20000*yHPCRB ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='AFG2A_OH';Gstr{i,   2}='NO';
fAFG2A_OH(i)=fAFG2A_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxRCHO(i)=fxRCHO(i)+  0.1400;fxGLY(i)=fxGLY(i)+  0.0600;fyHPCRB(i)=fyHPCRB(i)+  0.2000;

% 736, <F3HV>
i=i+1;
Rnames{ 736} = 'AFG2A = OH +  0.91000*MEO2 +  0.09000*ETO2 + MALAH + SumRO2 ';
k(:,i) = (  2.5000E-01.*JAFGS ); 
Gstr{i,   1}='AFG2A';
fAFG2A(i)=fAFG2A(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fMEO2(i)=fMEO2(i)+  0.9100;fETO2(i)=fETO2(i)+  0.0900;fMALAH(i)=fMALAH(i)+  1.0000;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 737, <F4OH>
i=i+1;
Rnames{ 737} = 'AFG2B + OH = AFG2B_OH +  0.02000*HO2 +  0.40000*xHO2 +  0.40000*RO2C +  0.05000*RO2XC +  0.17000*MACO3 +  0.39000*xGLY +  0.39000*xBACL +  0.05000*zRCNO3 +  0.02000*HPCRB +  0.38000*yHPCRB +  0.45000*SumRO2 +  0.17000*SumRCO3 ';
k(:,i) = (  4.5800E-11 ); 
Gstr{i,   1}='AFG2B';Gstr{i,   2}='OH';
fAFG2B(i)=fAFG2B(i)-1.0;fOH(i)=fOH(i)-1.0;
fAFG2B_OH(i)=fAFG2B_OH(i)+  1.0000;fHO2(i)=fHO2(i)+  0.0200;fxHO2(i)=fxHO2(i)+  0.4000;fRO2C(i)=fRO2C(i)+  0.4000;fRO2XC(i)=fRO2XC(i)+  0.0500;fMACO3(i)=fMACO3(i)+  0.1700;fxGLY(i)=fxGLY(i)+  0.3900;fxBACL(i)=fxBACL(i)+  0.3900;fzRCNO3(i)=fzRCNO3(i)+  0.0500;fHPCRB(i)=fHPCRB(i)+  0.0200;fyHPCRB(i)=fyHPCRB(i)+  0.3800;fSumRO2(i)=fSumRO2(i)+  0.4500;fSumRCO3(i)=fSumRCO3(i)+  0.1700;

% 738, <758>
i=i+1;
Rnames{ 738} = 'AFG2B_OH = 0.36000*HO2 +  0.36000*HPCRB ';
k(:,i) = (  1.1600E+01 ); 
Gstr{i,   1}='AFG2B_OH';
fAFG2B_OH(i)=fAFG2B_OH(i)-1.0;
fHO2(i)=fHO2(i)+  0.3600;fHPCRB(i)=fHPCRB(i)+  0.3600;

% 739, <759>
i=i+1;
Rnames{ 739} = 'AFG2B_OH + NO = NO +  0.20000*xHO2 +  0.32000*RO2C +  0.04000*RO2XC +  0.12000*xMECO3 +  0.13000*xRCHO +  0.20000*xGLY +  0.20000*xBACL +  0.04000*zRCNO3 +  0.31000*yHPCRB +  0.36000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='AFG2B_OH';Gstr{i,   2}='NO';
fAFG2B_OH(i)=fAFG2B_OH(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fxHO2(i)=fxHO2(i)+  0.2000;fRO2C(i)=fRO2C(i)+  0.3200;fRO2XC(i)=fRO2XC(i)+  0.0400;fxMECO3(i)=fxMECO3(i)+  0.1200;fxRCHO(i)=fxRCHO(i)+  0.1300;fxGLY(i)=fxGLY(i)+  0.2000;fxBACL(i)=fxBACL(i)+  0.2000;fzRCNO3(i)=fzRCNO3(i)+  0.0400;fyHPCRB(i)=fyHPCRB(i)+  0.3100;fSumRO2(i)=fSumRO2(i)+  0.3600;

% 740, <F4HV>
i=i+1;
Rnames{ 740} = 'AFG2B = AFG2B_HV +  0.88000*OH +  0.03000*xHO2 +  0.03000*RO2C + MEO2 +  0.87000*MALAH +  0.03000*xPACID +  0.03000*CO +  1.03000*SumRO2 ';
k(:,i) = (  2.2000E-01.*JAFGS ); 
Gstr{i,   1}='AFG2B';
fAFG2B(i)=fAFG2B(i)-1.0;
fAFG2B_HV(i)=fAFG2B_HV(i)+  1.0000;fOH(i)=fOH(i)+  0.8800;fxHO2(i)=fxHO2(i)+  0.0300;fRO2C(i)=fRO2C(i)+  0.0300;fMEO2(i)=fMEO2(i)+  1.0000;fMALAH(i)=fMALAH(i)+  0.8700;fxPACID(i)=fxPACID(i)+  0.0300;fCO(i)=fCO(i)+  0.0300;fSumRO2(i)=fSumRO2(i)+  1.0300;

% 741, <761>
i=i+1;
Rnames{ 741} = 'AFG2B_HV = 0.09000*OH +  0.05000*PACID +  0.05000*HPCRB ';
k(:,i) = (  8.6200E+00 ); 
Gstr{i,   1}='AFG2B_HV';
fAFG2B_HV(i)=fAFG2B_HV(i)-1.0;
fOH(i)=fOH(i)+  0.0900;fPACID(i)=fPACID(i)+  0.0500;fHPCRB(i)=fHPCRB(i)+  0.0500;

% 742, <762>
i=i+1;
Rnames{ 742} = 'AFG2B_HV + NO = NO +  0.08000*RO2C +  0.08000*MALAH +  0.09000*SumRO2 ';
k(:,i) = (  2.5500E-12.*exp(  3.8000E+02./T) ); 
Gstr{i,   1}='AFG2B_HV';Gstr{i,   2}='NO';
fAFG2B_HV(i)=fAFG2B_HV(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fRO2C(i)=fRO2C(i)+  0.0800;fMALAH(i)=fMALAH(i)+  0.0800;fSumRO2(i)=fSumRO2(i)+  0.0900;

% 743, <F5OH>
i=i+1;
Rnames{ 743} = 'AFG3 + OH = 0.27000*xHO2 +  0.89000*RO2C +  0.11000*RO2XC +  0.62000*xMECO3 +  0.62000*xRCHO +  0.54000*xMGLY +  0.11000*zRCNO3 +  0.85000*yHPCRB + SumRO2 ';
k(:,i) = (  7.2000E-11 ); 
Gstr{i,   1}='AFG3';Gstr{i,   2}='OH';
fAFG3(i)=fAFG3(i)-1.0;fOH(i)=fOH(i)-1.0;
fxHO2(i)=fxHO2(i)+  0.2700;fRO2C(i)=fRO2C(i)+  0.8900;fRO2XC(i)=fRO2XC(i)+  0.1100;fxMECO3(i)=fxMECO3(i)+  0.6200;fxRCHO(i)=fxRCHO(i)+  0.6200;fxMGLY(i)=fxMGLY(i)+  0.5400;fzRCNO3(i)=fzRCNO3(i)+  0.1100;fyHPCRB(i)=fyHPCRB(i)+  0.8500;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 744, <P2UI>
i=i+1;
Rnames{ 744} = 'PAN2 = NO2 + R2CO3 + SumRCO3 ';
k(:,i) = (  3.3900E-04 ); 
Gstr{i,   1}='PAN2';
fPAN2(i)=fPAN2(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;fR2CO3(i)=fR2CO3(i)+  1.0000;fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 745, <P2OH>
i=i+1;
Rnames{ 745} = 'PAN2 + OH = 0.07000*zPAN2 +  0.05000*NO3 +  0.73000*xNO3 +  1.15000*xHO2 +  1.55000*RO2C +  0.07000*RO2XC +  0.17000*xHCHO +  0.66000*xMECHO +  0.07000*xETCHO +  0.15000*xPAN2 +  0.05000*ALK3 +  0.73000*CO2 +  0.50000*CO +  1.62000*SumRO2 ';
k(:,i) = (  3.4200E-12 ); 
Gstr{i,   1}='PAN2';Gstr{i,   2}='OH';
fPAN2(i)=fPAN2(i)-1.0;fOH(i)=fOH(i)-1.0;
fzPAN2(i)=fzPAN2(i)+  0.0700;fNO3(i)=fNO3(i)+  0.0500;fxNO3(i)=fxNO3(i)+  0.7300;fxHO2(i)=fxHO2(i)+  1.1500;fRO2C(i)=fRO2C(i)+  1.5500;fRO2XC(i)=fRO2XC(i)+  0.0700;fxHCHO(i)=fxHCHO(i)+  0.1700;fxMECHO(i)=fxMECHO(i)+  0.6600;fxETCHO(i)=fxETCHO(i)+  0.0700;fxPAN2(i)=fxPAN2(i)+  0.1500;fALK3(i)=fALK3(i)+  0.0500;fCO2(i)=fCO2(i)+  0.7300;fCO(i)=fCO(i)+  0.5000;fSumRO2(i)=fSumRO2(i)+  1.6200;

% 746, <P2HV>
i=i+1;
Rnames{ 746} = 'PAN2 = NO2 + R2CO3 + SumRCO3 ';
k(:,i) = (JPPN_11 ); 
Gstr{i,   1}='PAN2';
fPAN2(i)=fPAN2(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;fR2CO3(i)=fR2CO3(i)+  1.0000;fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 747, <P4UI>
i=i+1;
Rnames{ 747} = 'APANS = NO2 + MACO3 + SumRCO3 ';
k(:,i) = (  3.3900E-04 ); 
Gstr{i,   1}='APANS';
fAPANS(i)=fAPANS(i)-1.0;
fNO2(i)=fNO2(i)+  1.0000;fMACO3(i)=fMACO3(i)+  1.0000;fSumRCO3(i)=fSumRCO3(i)+  1.0000;

% 748, <P4OH>
i=i+1;
Rnames{ 748} = 'APANS + OH = 0.01000*zPAN2 +  0.74000*NO3 +  0.18000*xNO3 +  0.07000*xHO2 +  0.25000*RO2C +  0.01000*RO2XC +  0.07000*xHCHO +  0.18000*xKET2 +  0.19000*OACID +  0.07000*xPAN2 +  0.56000*ALK4 +  0.18000*CO2 +  0.26000*SumRO2 ';
k(:,i) = (  2.9000E-11 ); 
Gstr{i,   1}='APANS';Gstr{i,   2}='OH';
fAPANS(i)=fAPANS(i)-1.0;fOH(i)=fOH(i)-1.0;
fzPAN2(i)=fzPAN2(i)+  0.0100;fNO3(i)=fNO3(i)+  0.7400;fxNO3(i)=fxNO3(i)+  0.1800;fxHO2(i)=fxHO2(i)+  0.0700;fRO2C(i)=fRO2C(i)+  0.2500;fRO2XC(i)=fRO2XC(i)+  0.0100;fxHCHO(i)=fxHCHO(i)+  0.0700;fxKET2(i)=fxKET2(i)+  0.1800;fOACID(i)=fOACID(i)+  0.1900;fxPAN2(i)=fxPAN2(i)+  0.0700;fALK4(i)=fALK4(i)+  0.5600;fCO2(i)=fCO2(i)+  0.1800;fSumRO2(i)=fSumRO2(i)+  0.2600;

% 749, <P4O3>
i=i+1;
Rnames{ 749} = 'APANS + O3 = 0.05000*NO2 +  0.19000*OH +  0.25000*HO2 +  0.09000*RO2C +  0.09000*xR2CO3 +  0.38000*HCHO2 +  0.01000*RCHO2 +  0.10000*HCHO +  0.04000*xHCHO +  0.90000*PAN2 +  0.21000*CO2 +  0.31000*CO +  0.09000*SumRO2 ';
k(:,i) = (  8.2000E-18 ); 
Gstr{i,   1}='APANS';Gstr{i,   2}='O3';
fAPANS(i)=fAPANS(i)-1.0;fO3(i)=fO3(i)-1.0;
fNO2(i)=fNO2(i)+  0.0500;fOH(i)=fOH(i)+  0.1900;fHO2(i)=fHO2(i)+  0.2500;fRO2C(i)=fRO2C(i)+  0.0900;fxR2CO3(i)=fxR2CO3(i)+  0.0900;fHCHO2(i)=fHCHO2(i)+  0.3800;fRCHO2(i)=fRCHO2(i)+  0.0100;fHCHO(i)=fHCHO(i)+  0.1000;fxHCHO(i)=fxHCHO(i)+  0.0400;fPAN2(i)=fPAN2(i)+  0.9000;fCO2(i)=fCO2(i)+  0.2100;fCO(i)=fCO(i)+  0.3100;fSumRO2(i)=fSumRO2(i)+  0.0900;

% 750, <P4N3>
i=i+1;
Rnames{ 750} = 'APANS + NO3 = 0.05000*zPAN2 +  0.95000*xNO3 +  0.95000*RO2C +  0.05000*RO2XC +  0.95000*xRCNO3 +  0.95000*CO2 + SumRO2 ';
k(:,i) = (  1.6000E-16 ); 
Gstr{i,   1}='APANS';Gstr{i,   2}='NO3';
fAPANS(i)=fAPANS(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fzPAN2(i)=fzPAN2(i)+  0.0500;fxNO3(i)=fxNO3(i)+  0.9500;fRO2C(i)=fRO2C(i)+  0.9500;fRO2XC(i)=fRO2XC(i)+  0.0500;fxRCNO3(i)=fxRCNO3(i)+  0.9500;fCO2(i)=fCO2(i)+  0.9500;fSumRO2(i)=fSumRO2(i)+  1.0000;

% 751, <P4HV>
i=i+1;
Rnames{ 751} = 'APANS = 0.60000*NO2 +  0.40000*NO3 +  0.40000*MEO2 +  0.60000*MACO3 +  0.40000*HCHO +  0.40000*CO2 +  0.40000*CO +  0.40000*SumRO2 +  0.60000*SumRCO3 ';
k(:,i) = (JPPN_11 ); 
Gstr{i,   1}='APANS';
fAPANS(i)=fAPANS(i)-1.0;
fNO2(i)=fNO2(i)+  0.6000;fNO3(i)=fNO3(i)+  0.4000;fMEO2(i)=fMEO2(i)+  0.4000;fMACO3(i)=fMACO3(i)+  0.6000;fHCHO(i)=fHCHO(i)+  0.4000;fCO2(i)=fCO2(i)+  0.4000;fCO(i)=fCO(i)+  0.4000;fSumRO2(i)=fSumRO2(i)+  0.4000;fSumRCO3(i)=fSumRCO3(i)+  0.6000;

% 752, <AALK>
i=i+1;
Rnames{ 752} = 'SOAALK + OH = OH +  0.00600*SVAVB2 +  0.05200*SVAVB3 +  0.08100*SVAVB4 ';
k(:,i) = (  2.7000E-12.*exp(  3.7400E+02./T) ); 
Gstr{i,   1}='SOAALK';Gstr{i,   2}='OH';
fSOAALK(i)=fSOAALK(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fSVAVB2(i)=fSVAVB2(i)+  0.0060;fSVAVB3(i)=fSVAVB3(i)+  0.0520;fSVAVB4(i)=fSVAVB4(i)+  0.0810;

% 753, <AE51>
i=i+1;
Rnames{ 753} = 'BENZRO2 + NO = NO +  0.03400*SVAVB2 +  0.39200*SVAVB4 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='BENZRO2';Gstr{i,   2}='NO';
fBENZRO2(i)=fBENZRO2(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fSVAVB2(i)=fSVAVB2(i)+  0.0340;fSVAVB4(i)=fSVAVB4(i)+  0.3920;

% 754, <AE52>
i=i+1;
Rnames{ 754} = 'BENZRO2 + HO2 = HO2 +  0.14600*SVAVB1 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='BENZRO2';Gstr{i,   2}='HO2';
fBENZRO2(i)=fBENZRO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fSVAVB1(i)=fSVAVB1(i)+  0.1460;

% 755, <AE53>
i=i+1;
Rnames{ 755} = 'XYLRO2 + NO = NO +  0.01500*SVAVB2 +  0.02300*SVAVB3 +  0.06000*SVAVB4 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='XYLRO2';Gstr{i,   2}='NO';
fXYLRO2(i)=fXYLRO2(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fSVAVB2(i)=fSVAVB2(i)+  0.0150;fSVAVB3(i)=fSVAVB3(i)+  0.0230;fSVAVB4(i)=fSVAVB4(i)+  0.0600;

% 756, <AE54>
i=i+1;
Rnames{ 756} = 'XYLRO2 + HO2 = HO2 +  0.19300*SVAVB1 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='XYLRO2';Gstr{i,   2}='HO2';
fXYLRO2(i)=fXYLRO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fSVAVB1(i)=fSVAVB1(i)+  0.1930;

% 757, <AE55>
i=i+1;
Rnames{ 757} = 'TOLRO2 + NO = NO +  0.01600*SVAVB2 +  0.05100*SVAVB3 +  0.04700*SVAVB4 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='TOLRO2';Gstr{i,   2}='NO';
fTOLRO2(i)=fTOLRO2(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fSVAVB2(i)=fSVAVB2(i)+  0.0160;fSVAVB3(i)=fSVAVB3(i)+  0.0510;fSVAVB4(i)=fSVAVB4(i)+  0.0470;

% 758, <AE56>
i=i+1;
Rnames{ 758} = 'TOLRO2 + HO2 = HO2 +  0.14000*SVAVB1 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='TOLRO2';Gstr{i,   2}='HO2';
fTOLRO2(i)=fTOLRO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fSVAVB1(i)=fSVAVB1(i)+  0.1400;

% 759, <AE57>
i=i+1;
Rnames{ 759} = 'PAHRO2 + NO = NO +  0.02800*SVAVB2 +  0.22500*SVAVB3 +  0.19100*SVAVB4 ';
k(:,i) = (k(:,  47) ); 
Gstr{i,   1}='PAHRO2';Gstr{i,   2}='NO';
fPAHRO2(i)=fPAHRO2(i)-1.0;fNO(i)=fNO(i)-1.0;
fNO(i)=fNO(i)+  1.0000;fSVAVB2(i)=fSVAVB2(i)+  0.0280;fSVAVB3(i)=fSVAVB3(i)+  0.2250;fSVAVB4(i)=fSVAVB4(i)+  0.1910;

% 760, <AE58>
i=i+1;
Rnames{ 760} = 'PAHRO2 + HO2 = HO2 +  0.47300*SVAVB1 ';
k(:,i) = (k(:,  48) ); 
Gstr{i,   1}='PAHRO2';Gstr{i,   2}='HO2';
fPAHRO2(i)=fPAHRO2(i)-1.0;fHO2(i)=fHO2(i)-1.0;
fHO2(i)=fHO2(i)+  1.0000;fSVAVB1(i)=fSVAVB1(i)+  0.4730;

% 761, <HET_NO2>
i=i+1;
Rnames{ 761} = 'NO2 = 0.50000*HONO +  0.50000*HNO3 ';
k(:,i) = (K_HETERO_NO2 ); 
Gstr{i,   1}='NO2';
fNO2(i)=fNO2(i)-1.0;
fHONO(i)=fHONO(i)+  0.5000;fHNO3(i)=fHNO3(i)+  0.5000;

% 762, <HET_N2O5IJ>
i=i+1;
Rnames{ 762} = 'N2O5 = HNO3 + H2NO3PIJ ';
k(:,i) = (K_HETERO_N2O5IJ ); 
Gstr{i,   1}='N2O5';
fN2O5(i)=fN2O5(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;fH2NO3PIJ(i)=fH2NO3PIJ(i)+  1.0000;

% 763, <HET_N2O5K>
i=i+1;
Rnames{ 763} = 'N2O5 = HNO3 + H2NO3PK ';
k(:,i) = (K_HETERO_N2O5K ); 
Gstr{i,   1}='N2O5';
fN2O5(i)=fN2O5(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;fH2NO3PK(i)=fH2NO3PK(i)+  1.0000;

% 764, <HET_H2NO3PIJA>
i=i+1;
Rnames{ 764} = 'H2NO3PIJ = HNO3 ';
k(:,i) = (K_HETERO_H2NO3PAIJ ); 
Gstr{i,   1}='H2NO3PIJ';
fH2NO3PIJ(i)=fH2NO3PIJ(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;

% 765, <HET_H2NO3PKA>
i=i+1;
Rnames{ 765} = 'H2NO3PK = HNO3 ';
k(:,i) = (K_HETERO_H2NO3PAK ); 
Gstr{i,   1}='H2NO3PK';
fH2NO3PK(i)=fH2NO3PK(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;

% 766, <HAL_Ozone>
i=i+1;
Rnames{ 766} = 'O3 =';
ILLUMINATED =  ( SZA > 0.0 );
OPEN_OCEAN  = 0.0;
SURF_ZONE   = 0.0;
SEA_ICE     = 0.0;
SEAWATER    = (OPEN_OCEAN+SURF_ZONE)-SEA_ICE;
SEAWATER(SEAWATER<0.0)    = 0.0;
Patm = 0.001.*P;
a =  6.701E-11.*exp( 1.074E+01.*Patm) + 3.415E-08.*exp(-6.713E-01.*Patm);
b =  2.000E-06;
a(a>b) = b;
k(:,i) = a.*ILLUMINATED.*SEAWATER;
Gstr{i,   1}='O3';
fO3(i)=fO3(i)-1.0;


% 767, <HET_NO3>
i=i+1;
Rnames{ 767} = 'NO3 = HNO3 ';
k(:,i) = (K_HETERO_NO3 ); 
Gstr{i,   1}='NO3';
fNO3(i)=fNO3(i)-1.0;
fHNO3(i)=fHNO3(i)+  1.0000;

% 768, <OLIG_ISOPRENE1>
i=i+1;
Rnames{ 768} = 'AISO1J = 0.50000*AOLGBJ ';
k(:,i) = (  9.4882E-06 ); 
Gstr{i,   1}='AISO1J';
fAISO1J(i)=fAISO1J(i)-1.0;
fAOLGBJ(i)=fAOLGBJ(i)+  0.5000;

% 769, <OLIG_ISOPRENE2>
i=i+1;
Rnames{ 769} = 'AISO2J = 0.50000*AOLGBJ ';
k(:,i) = (  9.4882E-06 ); 
Gstr{i,   1}='AISO2J';
fAISO2J(i)=fAISO2J(i)-1.0;
fAOLGBJ(i)=fAOLGBJ(i)+  0.5000;

% 770, <OLIG_SESQT1>
i=i+1;
Rnames{ 770} = 'ASQTJ = 1.50000*AOLGBJ ';
k(:,i) = (  9.4882E-06 ); 
Gstr{i,   1}='ASQTJ';
fASQTJ(i)=fASQTJ(i)-1.0;
fAOLGBJ(i)=fAOLGBJ(i)+  1.5000;

% 771, <OLIG_AROMATIC1>
i=i+1;
Rnames{ 771} = 'AAVB2J = 0.90700*AOLGAJ ';
k(:,i) = (  9.4882E-06 ); 
Gstr{i,   1}='AAVB2J';
fAAVB2J(i)=fAAVB2J(i)-1.0;
fAOLGAJ(i)=fAOLGAJ(i)+  0.9070;

% 772, <OLIG_AROMATIC2>
i=i+1;
Rnames{ 772} = 'AAVB3J = 0.92500*AOLGAJ ';
k(:,i) = (  9.4882E-06 ); 
Gstr{i,   1}='AAVB3J';
fAAVB3J(i)=fAAVB3J(i)-1.0;
fAOLGAJ(i)=fAOLGAJ(i)+  0.9250;

% 773, <OLIG_AROMATIC3>
i=i+1;
Rnames{ 773} = 'AAVB4J = 0.94300*AOLGAJ ';
k(:,i) = (  9.4882E-06 ); 
Gstr{i,   1}='AAVB4J';
fAAVB4J(i)=fAAVB4J(i)-1.0;
fAOLGAJ(i)=fAOLGAJ(i)+  0.9430;

% 774, <RPOAGEPI>
i=i+1;
Rnames{ 774} = 'APOCI + OH = 1.25000*APNCOMI + APOCI + OH ';
k(:,i) = (  2.5000E-12 ); 
Gstr{i,   1}='APOCI';Gstr{i,   2}='OH';
fAPOCI(i)=fAPOCI(i)-1.0;fOH(i)=fOH(i)-1.0;
fAPNCOMI(i)=fAPNCOMI(i)+  1.2500;fAPOCI(i)=fAPOCI(i)+  1.0000;fOH(i)=fOH(i)+  1.0000;

% 775, <RPOAGELI>
i=i+1;
Rnames{ 775} = 'APNCOMI + OH = OH ';
k(:,i) = (K_HETERO_PNCOMLI ); 
Gstr{i,   1}='APNCOMI';Gstr{i,   2}='OH';
fAPNCOMI(i)=fAPNCOMI(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  1.0000;

% 776, <RPOAGEPJ>
i=i+1;
Rnames{ 776} = 'APOCJ + OH = 1.25000*APNCOMJ + APOCJ + OH ';
k(:,i) = (  2.5000E-12 ); 
Gstr{i,   1}='APOCJ';Gstr{i,   2}='OH';
fAPOCJ(i)=fAPOCJ(i)-1.0;fOH(i)=fOH(i)-1.0;
fAPNCOMJ(i)=fAPNCOMJ(i)+  1.2500;fAPOCJ(i)=fAPOCJ(i)+  1.0000;fOH(i)=fOH(i)+  1.0000;

% 777, <RPOAGELJ>
i=i+1;
Rnames{ 777} = 'APNCOMJ + OH = OH ';
k(:,i) = (K_HETERO_PNCOMLJ ); 
Gstr{i,   1}='APNCOMJ';Gstr{i,   2}='OH';
fAPNCOMJ(i)=fAPNCOMJ(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  1.0000;

% 778, <PCSOA>
i=i+1;
Rnames{ 778} = 'PCVOC + OH = OH + PCSOARXN ';
k(:,i) = (  1.2500E-11 ); 
Gstr{i,   1}='PCVOC';Gstr{i,   2}='OH';
fPCVOC(i)=fPCVOC(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fPCSOARXN(i)=fPCSOARXN(i)+  1.0000;

% 779, <POA_AGE1>
i=i+1;
Rnames{ 779} = 'VLVPO1 + OH = OH +  0.48570*VLVPO1 +  0.00620*VSVPO1 +  0.00250*VSVPO2 +  0.00260*VSVPO3 +  0.00230*VIVPO1 +  0.29440*VLVOO1 +  0.20210*VLVOO2 +  0.00190*VSVOO2 +  0.00230*VSVOO3 ';
k(:,i) = (  4.0000E-11 ); 
Gstr{i,   1}='VLVPO1';Gstr{i,   2}='OH';
fVLVPO1(i)=fVLVPO1(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fVLVPO1(i)=fVLVPO1(i)+  0.4857;fVSVPO1(i)=fVSVPO1(i)+  0.0062;fVSVPO2(i)=fVSVPO2(i)+  0.0025;fVSVPO3(i)=fVSVPO3(i)+  0.0026;fVIVPO1(i)=fVIVPO1(i)+  0.0023;fVLVOO1(i)=fVLVOO1(i)+  0.2944;fVLVOO2(i)=fVLVOO2(i)+  0.2021;fVSVOO2(i)=fVSVOO2(i)+  0.0019;fVSVOO3(i)=fVSVOO3(i)+  0.0023;

% 780, <POA_AGE2>
i=i+1;
Rnames{ 780} = 'VSVPO1 + OH = OH +  0.30030*VLVPO1 +  0.28620*VSVPO1 +  0.00410*VSVPO2 +  0.00350*VSVPO3 +  0.22390*VLVOO1 +  0.18200*VLVOO2 ';
k(:,i) = (  4.0000E-11 ); 
Gstr{i,   1}='VSVPO1';Gstr{i,   2}='OH';
fVSVPO1(i)=fVSVPO1(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fVLVPO1(i)=fVLVPO1(i)+  0.3003;fVSVPO1(i)=fVSVPO1(i)+  0.2862;fVSVPO2(i)=fVSVPO2(i)+  0.0041;fVSVPO3(i)=fVSVPO3(i)+  0.0035;fVLVOO1(i)=fVLVOO1(i)+  0.2239;fVLVOO2(i)=fVLVOO2(i)+  0.1820;

% 781, <POA_AGE3>
i=i+1;
Rnames{ 781} = 'VSVPO2 + OH = OH +  0.38560*VLVPO1 +  0.09500*VSVPO1 +  0.13730*VSVPO2 +  0.00050*VSVPO3 +  0.20510*VLVOO1 +  0.17640*VLVOO2 ';
k(:,i) = (  4.0000E-11 ); 
Gstr{i,   1}='VSVPO2';Gstr{i,   2}='OH';
fVSVPO2(i)=fVSVPO2(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fVLVPO1(i)=fVLVPO1(i)+  0.3856;fVSVPO1(i)=fVSVPO1(i)+  0.0950;fVSVPO2(i)=fVSVPO2(i)+  0.1373;fVSVPO3(i)=fVSVPO3(i)+  0.0005;fVLVOO1(i)=fVLVOO1(i)+  0.2051;fVLVOO2(i)=fVLVOO2(i)+  0.1764;

% 782, <POA_AGE4>
i=i+1;
Rnames{ 782} = 'VSVPO3 + OH = OH +  0.21810*VLVPO1 +  0.30630*VSVPO1 +  0.01530*VSVPO2 +  0.10430*VSVPO3 +  0.18930*VLVOO1 +  0.16680*VLVOO2 ';
k(:,i) = (  4.0000E-11 ); 
Gstr{i,   1}='VSVPO3';Gstr{i,   2}='OH';
fVSVPO3(i)=fVSVPO3(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fVLVPO1(i)=fVLVPO1(i)+  0.2181;fVSVPO1(i)=fVSVPO1(i)+  0.3063;fVSVPO2(i)=fVSVPO2(i)+  0.0153;fVSVPO3(i)=fVSVPO3(i)+  0.1043;fVLVOO1(i)=fVLVOO1(i)+  0.1893;fVLVOO2(i)=fVLVOO2(i)+  0.1668;

% 783, <POA_AGE5>
i=i+1;
Rnames{ 783} = 'VIVPO1 + OH = OH +  0.24120*VLVPO1 +  0.20890*VSVPO1 +  0.30000*VSVPO2 +  0.20280*VLVOO1 +  0.04710*VLVOO2 ';
k(:,i) = (  4.0000E-11 ); 
Gstr{i,   1}='VIVPO1';Gstr{i,   2}='OH';
fVIVPO1(i)=fVIVPO1(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fVLVPO1(i)=fVLVPO1(i)+  0.2412;fVSVPO1(i)=fVSVPO1(i)+  0.2089;fVSVPO2(i)=fVSVPO2(i)+  0.3000;fVLVOO1(i)=fVLVOO1(i)+  0.2028;fVLVOO2(i)=fVLVOO2(i)+  0.0471;

% 784, <POA_AGE6>
i=i+1;
Rnames{ 784} = 'VLVOO1 + OH = OH +  0.66640*VLVOO1 +  0.01430*VLVOO2 +  0.01230*VSVOO1 +  0.12390*VSVOO2 +  0.18310*VSVOO3 ';
k(:,i) = (  4.0000E-11 ); 
Gstr{i,   1}='VLVOO1';Gstr{i,   2}='OH';
fVLVOO1(i)=fVLVOO1(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fVLVOO1(i)=fVLVOO1(i)+  0.6664;fVLVOO2(i)=fVLVOO2(i)+  0.0143;fVSVOO1(i)=fVSVOO1(i)+  0.0123;fVSVOO2(i)=fVSVOO2(i)+  0.1239;fVSVOO3(i)=fVSVOO3(i)+  0.1831;

% 785, <POA_AGE7>
i=i+1;
Rnames{ 785} = 'VLVOO2 + OH = OH +  0.28580*VLVOO1 +  0.39310*VLVOO2 +  0.01390*VSVOO1 +  0.10270*VSVOO2 +  0.20450*VSVOO3 ';
k(:,i) = (  4.0000E-11 ); 
Gstr{i,   1}='VLVOO2';Gstr{i,   2}='OH';
fVLVOO2(i)=fVLVOO2(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fVLVOO1(i)=fVLVOO1(i)+  0.2858;fVLVOO2(i)=fVLVOO2(i)+  0.3931;fVSVOO1(i)=fVSVOO1(i)+  0.0139;fVSVOO2(i)=fVSVOO2(i)+  0.1027;fVSVOO3(i)=fVSVOO3(i)+  0.2045;

% 786, <POA_AGE8>
i=i+1;
Rnames{ 786} = 'VSVOO1 + OH = OH +  0.33030*VLVOO1 +  0.22720*VLVOO2 +  0.26070*VSVOO1 +  0.07020*VSVOO2 +  0.11160*VSVOO3 ';
k(:,i) = (  4.0000E-11 ); 
Gstr{i,   1}='VSVOO1';Gstr{i,   2}='OH';
fVSVOO1(i)=fVSVOO1(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fVLVOO1(i)=fVLVOO1(i)+  0.3303;fVLVOO2(i)=fVLVOO2(i)+  0.2272;fVSVOO1(i)=fVSVOO1(i)+  0.2607;fVSVOO2(i)=fVSVOO2(i)+  0.0702;fVSVOO3(i)=fVSVOO3(i)+  0.1116;

% 787, <POA_AGE9>
i=i+1;
Rnames{ 787} = 'VSVOO2 + OH = OH +  0.34440*VLVOO1 +  0.27490*VLVOO2 +  0.04910*VSVOO1 +  0.25770*VSVOO2 +  0.07390*VSVOO3 ';
k(:,i) = (  4.0000E-11 ); 
Gstr{i,   1}='VSVOO2';Gstr{i,   2}='OH';
fVSVOO2(i)=fVSVOO2(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fVLVOO1(i)=fVLVOO1(i)+  0.3444;fVLVOO2(i)=fVLVOO2(i)+  0.2749;fVSVOO1(i)=fVSVOO1(i)+  0.0491;fVSVOO2(i)=fVSVOO2(i)+  0.2577;fVSVOO3(i)=fVSVOO3(i)+  0.0739;

% 788, <POA_AGE10>
i=i+1;
Rnames{ 788} = 'VSVOO3 + OH = OH +  0.38860*VLVOO1 +  0.24210*VLVOO2 +  0.06400*VSVOO1 +  0.03850*VSVOO2 +  0.26670*VSVOO3 ';
k(:,i) = (  4.0000E-11 ); 
Gstr{i,   1}='VSVOO3';Gstr{i,   2}='OH';
fVSVOO3(i)=fVSVOO3(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  1.0000;fVLVOO1(i)=fVLVOO1(i)+  0.3886;fVLVOO2(i)=fVLVOO2(i)+  0.2421;fVSVOO1(i)=fVSVOO1(i)+  0.0640;fVSVOO2(i)=fVSVOO2(i)+  0.0385;fVSVOO3(i)=fVSVOO3(i)+  0.2667;

% 789, <HET_GLY>
i=i+1;
Rnames{ 789} = 'GLY = AGLYJ ';
k(:,i) = (K_HETERO_GLY ); 
Gstr{i,   1}='GLY';
fGLY(i)=fGLY(i)-1.0;
fAGLYJ(i)=fAGLYJ(i)+  1.0000;

% 790, <HET_MGLY>
i=i+1;
Rnames{ 790} = 'MGLY = AGLYJ ';
k(:,i) = (K_HETERO_MGLY ); 
Gstr{i,   1}='MGLY';
fMGLY(i)=fMGLY(i)-1.0;
fAGLYJ(i)=fAGLYJ(i)+  1.0000;

% 791, <TR01>
i=i+1;
Rnames{ 791} = 'HCHO_PRIMARY =';
k(:,i) = (JHCHOR_13 ); 
Gstr{i,   1}='HCHO_PRIMARY';
fHCHO_PRIMARY(i)=fHCHO_PRIMARY(i)-1.0;


% 792, <TR02>
i=i+1;
Rnames{ 792} = 'HCHO_PRIMARY =';
k(:,i) = (JHCHOM_13 ); 
Gstr{i,   1}='HCHO_PRIMARY';
fHCHO_PRIMARY(i)=fHCHO_PRIMARY(i)-1.0;


% 793, <TR03>
i=i+1;
Rnames{ 793} = 'HCHO_PRIMARY + OH = OH ';
k(:,i) = (  5.5000E-12.*exp(  1.2500E+02./T) ); 
Gstr{i,   1}='HCHO_PRIMARY';Gstr{i,   2}='OH';
fHCHO_PRIMARY(i)=fHCHO_PRIMARY(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  1.0000;

% 794, <TR05>
i=i+1;
Rnames{ 794} = 'HCHO_PRIMARY + NO3 = NO3 ';
k(:,i) = (  5.8000E-16 ); 
Gstr{i,   1}='HCHO_PRIMARY';Gstr{i,   2}='NO3';
fHCHO_PRIMARY(i)=fHCHO_PRIMARY(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;

% 795, <TR07>
i=i+1;
Rnames{ 795} = 'CCHO_PRIMARY + OH = OH ';
k(:,i) = (  2.4000E-12.*exp(  5.4600E+02./T).*(T./300).^(  7.7000E-01 ) ); 
Gstr{i,   1}='CCHO_PRIMARY';Gstr{i,   2}='OH';
fCCHO_PRIMARY(i)=fCCHO_PRIMARY(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  1.0000;

% 796, <TR08>
i=i+1;
Rnames{ 796} = 'CCHO_PRIMARY =';
k(:,i) = (JCCHOR_13 ); 
Gstr{i,   1}='CCHO_PRIMARY';
fCCHO_PRIMARY(i)=fCCHO_PRIMARY(i)-1.0;


% 797, <TR09>
i=i+1;
Rnames{ 797} = 'CCHO_PRIMARY + NO3 = NO3 ';
k(:,i) = (  1.4000E-12.*exp( -1.8600E+03./T) ); 
Gstr{i,   1}='CCHO_PRIMARY';Gstr{i,   2}='NO3';
fCCHO_PRIMARY(i)=fCCHO_PRIMARY(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;

% 798, <TR11>
i=i+1;
Rnames{ 798} = 'ACRO_PRIMARY + OH = OH ';
k(:,i) = (  7.1000E-12.*exp(  3.3300E+02./T) ); 
Gstr{i,   1}='ACRO_PRIMARY';Gstr{i,   2}='OH';
fACRO_PRIMARY(i)=fACRO_PRIMARY(i)-1.0;fOH(i)=fOH(i)-1.0;
fOH(i)=fOH(i)+  1.0000;

% 799, <TR12>
i=i+1;
Rnames{ 799} = 'ACRO_PRIMARY + O3 = O3 ';
k(:,i) = (  2.8000E-19 ); 
Gstr{i,   1}='ACRO_PRIMARY';Gstr{i,   2}='O3';
fACRO_PRIMARY(i)=fACRO_PRIMARY(i)-1.0;fO3(i)=fO3(i)-1.0;
fO3(i)=fO3(i)+  1.0000;

% 800, <TR13>
i=i+1;
Rnames{ 800} = 'ACRO_PRIMARY + NO3 = NO3 ';
k(:,i) = (  1.1000E-15 ); 
Gstr{i,   1}='ACRO_PRIMARY';Gstr{i,   2}='NO3';
fACRO_PRIMARY(i)=fACRO_PRIMARY(i)-1.0;fNO3(i)=fNO3(i)-1.0;
fNO3(i)=fNO3(i)+  1.0000;

% 801, <TR15>
i=i+1;
Rnames{ 801} = 'ACRO_PRIMARY =';
k(:,i) = (JACROL_16 ); 
Gstr{i,   1}='ACRO_PRIMARY';
fACRO_PRIMARY(i)=fACRO_PRIMARY(i)-1.0;


