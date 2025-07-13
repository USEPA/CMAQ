function K = SAPRC22_AE65_AQ_K(Met,Jmethod)
% Calculate heteorogeneous and other rate constants for use with the saprc22_ae65_aq chemical mechanism.
% Met: structure containing required meteorological constraints. Required vars depend on Jmethod.
%       Met.T: temperature, T
%       Met.P: pressure, mbar
%
%
% OUTPUTS:
% J: structure of J-values.
%
% INPUTS
struct2var(Met)

nk =  10; %number of rate constants
krx = nan(length(T),nk);
krx = 1.0E-60;
Knames = cell(nk,1);
i=0;


i = i + 1
Knames{i}   = 'K_HETERO_NO2';
krx(:,i)    =  1.0E-60;


i = i + 1
Knames{i}   = 'K_HETERO_N2O5IJ';
krx(:,i)    =  1.0E-60;


i = i + 1
Knames{i}   = 'K_HETERO_N2O5K';
krx(:,i)    =  1.0E-60;


i = i + 1
Knames{i}   = 'K_HETERO_H2NO3PAIJ';
krx(:,i)    =  1.0E-60;


i = i + 1
Knames{i}   = 'K_HETERO_H2NO3PAK';
krx(:,i)    =  1.0E-60;


i = i + 1
Knames{i}   = 'K_HETERO_NO3';
krx(:,i)    =  1.0E-60;


i = i + 1
Knames{i}   = 'K_HETERO_PNCOMLI';
krx(:,i)    =  1.0E-60;


i = i + 1
Knames{i}   = 'K_HETERO_PNCOMLJ';
krx(:,i)    =  1.0E-60;


i = i + 1
Knames{i}   = 'K_HETERO_GLY';
krx(:,i)    =  1.0E-60;


i = i + 1
Knames{i}   = 'K_HETERO_MGLY';
krx(:,i)    =  1.0E-60;

%% accumulate
K = struct;
for i=1:length(Knames)
    K.(Knames{i}) = krx(:,i);
end
