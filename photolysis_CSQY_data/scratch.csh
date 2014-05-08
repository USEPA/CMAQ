#!/bin/csh -f

set echo

set files = (  "NO2_06          "  "NO3NO_06        "  "NO3NO2_6        " \
               "O3O1D_06        "  "O3O3P_06        "  "HONO_06         " \
               "HNO3            "  "HNO4_06         "  "H2O2            " \
               "NO2EX           "  "PAN             "  "HCHOR_06        " \
               "HCHOM_06        "  "CCHO_R          "  "C2CHO           " \
               "ACET_06         "  "MEK_06          "  "COOH            " \
               "GLY_07R         "  "GLY_07M         "  "MGLY_06         " \
               "BACL_07         "  "BALD_06         "  "AFG1            " \
               "MACR_06         "  "MVK_06          "  "IC3ONO2         " \
               "HOCCHO_IUPAC    "  "ACRO_09         "  "PAA             " \
               "CL2             "  "CLNO_06         "  "CLONO           " \
               "CLNO2           "  "CLONO2_1        "  "CLONO2_2        " \
               "HOCL_06         "  "CLCCHO          "  "CLACET          " )

ls $files

set OUTDIR  = "/home/hwo/jproc/S07T-CSQY_data"

if( ! -d ${OUTDIR} )then
    mkdir ${OUTDIR}
endif

foreach xx ( ${files} )

  ls ${xx}
  \cp -f ${xx} ${OUTDIR}/.

end

exit()


