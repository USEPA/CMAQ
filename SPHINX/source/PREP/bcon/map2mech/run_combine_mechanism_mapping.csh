#! /bin/csh -f

# ====================== COMBINE_v5.4 Run Script ======================== 
# Usage: run.combine.uncoupled.csh >&! combine_v53_uncoupled.log &                                
#
# To report problems or request help with this script/program:     
#             http://www.epa.gov/cmaq    (EPA CMAQ Website)
#             http://www.cmascenter.org  (CMAS Website)
# ===================================================================  

set CMAQ_HOME       = /home/user/CMAQ_Project
set CMAQ_DATA       = /home/user/CMAQ_Project/data

set compiler        = intel
set compilerVrsn    = 18.0
set compilerString  = ${compiler}${compilerVrsn}

set VRSN            = v54

set BINDIR          = $CMAQ_HOME/POST/combine/scripts/BLD_combine_${VRSN}_${compilerString}

set EXEC            = combine_${VRSN}.exe

setenv GENSPEC N

#> Set the species definition file used for mechanism mapping

setenv SPECIES_DEF SpecDef_racm_ae6_aq_derived_from_cb6r3m_ae7_kmtbr.txt
#setenv SPECIES_DEF SpecDef_racm2_ae6_aq_derived_from_cb6r3m_ae7_kmtbr.txt
#setenv SPECIES_DEF SpecDef_saprc07tc_ae6_aq_derived_from_cb6r3m_ae7_kmtbr.txt
#setenv SPECIES_DEF SpecDef_saprc07tic_ae7i_aq_derived_from_cb6r3m_ae7_kmtbr.txt
#setenv SPECIES_DEF SpecDef_cracmm1_aq_derived_from_cb6r5m_ae7_aq.txt
#setenv SPECIES_DEF SpecDef_cracmm2_derived_from_cb6r5m_ae7_aq.txt

#> Set the input file, e.g. the seasonal average H-CMAQ file 
#> CCTM_CONC_v53beta2_intel17.0_HEMIS_cb6r3m_ae7_kmtbr_m3dry_2016_quarterly_av.nc
#> available for download from the CMAS data warehouse 

setenv INFILE1 ${CMAQ_DATA}/CCTM_CONC_v53beta2_intel17.0_HEMIS_cb6r3m_ae7_kmtbr_m3dry_2016_quarterly_av.nc

#> Set the output file containing the mapped species

setenv OUTFILE ${CMAQ_DATA}/CCTM_CONC_v53beta2_intel17.0_HEMIS_racm_ae6_aq_derived_from_cb6r3m_ae7_kmtbr_m3dry_2016_quarterly_av.nc
#setenv OUTFILE ${CMAQ_DATA}/CCTM_CONC_v53beta2_intel17.0_HEMIS_racm2_ae6_aq_derived_from_cb6r3m_ae7_kmtbr_m3dry_2016_quarterly_av.nc
#setenv OUTFILE ${CMAQ_DATA}/CCTM_CONC_v53beta2_intel17.0_HEMIS_saprc07tc_ae6_aq_derived_from_cb6r3m_ae7_kmtbr_m3dry_2016_quarterly_av.nc
#setenv OUTFILE ${CMAQ_DATA}/CCTM_CONC_v53beta2_intel17.0_HEMIS_saprc07tic_ae7i_aq_derived_from_cb6r3m_ae7_kmtbr_m3dry_2016_quarterly_av.nc
#setenv OUTFILE ${CMAQ_DATA}/CCTM_CONC_v53beta2_intel17.0_HEMIS_cracmm1_aq_derived_from_cb6r3m_ae7_kmtbr_m3dry_2016_quarterly_av.nc
#setenv OUTFILE ${CMAQ_DATA}/CCTM_CONC_v53beta2_intel17.0_HEMIS_cracmm2_derived_from_cb6r3m_ae7_kmtbr_m3dry_2016_quarterly_av.nc

#> Executable call:
${BINDIR}/${EXEC}


exit()
