function J = SAPRC22_AE65_AQ_J(Met,Jmethod)
% Calculates photolysis frequencies for the saprc22_ae65_aq mechanism in the CMAQ model
% Met: structure containing required meteorological constraints. Required vars depend on Jmethod.
%       Met.SZA: solar zenith angle in degrees
%       Met.ALT: altitude, meters
%       Met.O3col: overhead ozone column, DU
%       Met.albedo: surface reflectance, 0-1 (unitless)
%       Met.T: temperature, T
%       Met.P: pressure, mbar
%       Met.LFlux: name of a text file containing an actinic flux spectrum
%
% Jmethod: numeric flag or string specifying how to calculate J-values. Default is 'MCM'.
%       0 or 'MCM':      use MCMv3.3.1 parameterization.
%                         Some reactions are not included in MCM. For these, 'HYBRID' values are used.
%                         Required Met fields: SZA
%       1 or 'BOTTOMUP': bottom-up integration of cross sections/quantum yields.
%                         See J_BottomUp.m for more info.
%                         Required Met fields: LFlux, T, P
%       2 or 'HYBRID':   Interpolation of hybrid J-values from TUV solar spectra.
%                         See J_TUVhybrid.m for more info.
%                         Required Met fields: SZA, ALT, O3col, albedo
%
% OUTPUTS:
% J: structure of J-values.
%
% INPUTS
struct2var(Met)

if nargin<2
    Jmethod = 'MCM';
elseif ischar(Jmethod)
    Jmethod = upper(Jmethod);
end

% J-Values
switch Jmethod
    case {0,'MCM'}
        error(['MCM option not functional for saprc22_ae65_aq mechanism.'])

    case {1,'BOTTOMUP'}
        Jmcm = J_BottomUp(LFlux,T,P);

    case {2,'HYBRID'}
        Jmcm = J_Hybrid(SZA,ALT,O3col,albedo);

    otherwise
        fprintf('Jmethod = %f
',Jmethod);
        error(['MCMv331_J: invalid Jmethod option selected'])

end
%rename
J=struct;
J.JNO2_06 = Jmcm.J_NO2_06;
J.JNO3NO_06 = Jmcm.J_NO3NO_06;
J.JNO3NO2_6 = Jmcm.J_NO3NO2_6;
J.JO3O1D_06 = Jmcm.J_O3O1D_06;
J.JO3O3P_06 = Jmcm.J_O3O3P_06;
J.JHONO_06 = Jmcm.J_HONO_06;
J.JHNO3 = Jmcm.J_HNO3;
J.JHNO4_06 = Jmcm.J_HNO4_06;
J.JH2O2 = Jmcm.J_H2O2;
J.JHCHOR_13 = Jmcm.J_HCHOR_13;
J.JHCHOM_13 = Jmcm.J_HCHOM_13;
J.JPAN_11 = Jmcm.J_PAN_11;
J.JGLY_I13R = Jmcm.J_GLY_I13R;
J.JGLY_I13M = Jmcm.J_GLY_I13M;
J.JBALD_11 = Jmcm.J_BALD_11;
J.JPPN_11 = Jmcm.J_PPN_11;
J.JBACL_11 = Jmcm.J_BACL_11;
J.JCOOH = Jmcm.J_COOH;
J.JCCHOR_13 = Jmcm.J_CCHOR_13;
J.JGLALD_14 = Jmcm.J_GLALD_14;
J.JC2CHOabs = Jmcm.J_C2CHOabs;
J.JACROL_16 = Jmcm.J_ACROL_16;
J.JACET_06 = Jmcm.J_ACET_06;
J.JMEK_06 = Jmcm.J_MEK_06;
J.JMACR_06 = Jmcm.J_MACR_06;
J.JMVK_16 = Jmcm.J_MVK_16;
J.JAFGS = Jmcm.J_AFGS;
J.JPAA = Jmcm.J_PAA;
J.JMGLY_13 = Jmcm.J_MGLY_13;
J.JCRBNIT = Jmcm.J_CRBNIT;
J.JIC3ONO2 = Jmcm.J_IC3ONO2;
J.JDIONO2 = Jmcm.J_DIONO2;
J.JHPALDS = Jmcm.J_HPALDS;
