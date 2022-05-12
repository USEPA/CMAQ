! for racm2_ae6_aq
!
!=======================================================================
! IPR_OUTPUTS
!=======================================================================
IPR_OUTPUT O3    =  CHEM+DDEP+CLDS+AERO+TRNM;
IPR_OUTPUT HNO3    =  CHEM+DDEP+CLDS+AERO;
IPR_OUTPUT PAN    =  CHEM+DDEP+CLDS+AERO+TRNM;
IPR_OUTPUT N2O5    =  CHEM+DDEP+CLDS+AERO;

DEFINE FAMILY EC = AECI + AECJ;
IPR_OUTPUT EC = COAG + EMIS + HADV;
IPR_OUTPUT ASO4J = COND + COAG + EMIS + CHEM + VDIF + CLDS;
IPR_OUTPUT ASO4I = NPF + COND + COAG + EMIS + CHEM + VDIF +CLDS;
IPR_OUTPUT NUMATKN;
!IPR_OUTPUT NUMACC;
!IPR_OUTPUT NUMCOR;
!
ENDPA;
