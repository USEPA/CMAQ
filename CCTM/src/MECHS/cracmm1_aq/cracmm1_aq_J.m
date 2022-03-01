function J = CRACMM1_AQ_J(Met,Jmethod)
% Calculates photolysis frequencies for the cracmm1_aq mechanism in the CMAQ model
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
        error(['MCM option not functional for cracmm1_aq mechanism.'])

    case {1,'BOTTOMUP'}
        Jmcm = J_BottomUp(LFlux,T,P);

    case {2,'HYBRID'}
        Jmcm = J_Hybrid(SZA,ALT,O3col,albedo);

    otherwise
        fprintf('Jmethod = %f\n',Jmethod);
        error(['MCMv331_J: invalid Jmethod option selected'])

end
%rename
J=struct;
J.JO3O3P_NASA06 = Jmcm.J_O3O3P_NASA06;
J.JO3O1D_NASA06 = Jmcm.J_O3O1D_NASA06;
J.JH2O2_RACM2 = Jmcm.J_H2O2_RACM2;
J.JNO2_RACM2 = Jmcm.J_NO2_RACM2;
J.JNO3NO_RACM2 = Jmcm.J_NO3NO_RACM2;
J.JNO3NO2_RACM2 = Jmcm.J_NO3NO2_RACM2;
J.JHONO_RACM2 = Jmcm.J_HONO_RACM2;
J.JHNO3_RACM2 = Jmcm.J_HNO3_RACM2;
J.JHNO4_RACM2 = Jmcm.J_HNO4_RACM2;
J.JHCHO_MOL_JPL19 = Jmcm.J_HCHO_MOL_JPL19;
J.JHCHO_RAD_JPL19 = Jmcm.J_HCHO_RAD_JPL19;
J.JCH3CHO_RACM2 = Jmcm.J_CH3CHO_RACM2;
J.JALD_JPL19 = Jmcm.J_ALD_JPL19;
J.JCH3COCH3A_JPL19 = Jmcm.J_CH3COCH3A_JPL19;
J.JCH3COCH3B_JPL19 = Jmcm.J_CH3COCH3B_JPL19;
J.JUALD_RACM2 = Jmcm.J_UALD_RACM2;
J.JMEK_JGR19 = Jmcm.J_MEK_JGR19;
J.JKET_JGR19 = Jmcm.J_KET_JGR19;
J.JHKET_RACM2 = Jmcm.J_HKET_RACM2;
J.JMACR_RACM2 = Jmcm.J_MACR_RACM2;
J.JMVK_RACM2 = Jmcm.J_MVK_RACM2;
J.JGLYH2_RACM2 = Jmcm.J_GLYH2_RACM2;
J.JGLYF_RACM2 = Jmcm.J_GLYF_RACM2;
J.JGLYHX_RACM2 = Jmcm.J_GLYHX_RACM2;
J.JMGLY_RACM2 = Jmcm.J_MGLY_RACM2;
J.JBALD_RACM2 = Jmcm.J_BALD_RACM2;
J.JOP1_RACM2 = Jmcm.J_OP1_RACM2;
J.JPAA_RACM2 = Jmcm.J_PAA_RACM2;
J.JONIT_RACM2 = Jmcm.J_ONIT_RACM2;
J.JPAN1_RACM2 = Jmcm.J_PAN1_RACM2;
J.JPAN2_RACM2 = Jmcm.J_PAN2_RACM2;
J.JACRO_09 = Jmcm.J_ACRO_09;
