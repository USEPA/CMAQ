#!/bin/sh
#
# RCS file, release, date & time of last delta, author, state, [and locker]
# $Header: /nas01/depts/ie/cempd/apps/CMAQ/v5.0.1/CMAQv5.0.1/models/TOOLS/src/cast2ext/runCast2ext.sh,v 1.1.1.1 2012/04/19 19:48:37 sjr Exp $ 
#
# script for running the cast2ext program on Unix
#
#  converts CASTNET hourly data to a format used by sitecmp
#

BASE=/project/model_evalb/extract_util
 
EXECUTION_ID=cast2ext; export EXECUTION_ID
 
EXEC=${BASE}/bin/${EXECUTION_ID}.exe


#   input file (downloaded from CASTNET web site) 
#   containing site-id, time-period, and data fields
INFILE=${BASE}/castnet/obs/castnet_hr.csv; export INFILE  


#  output file (csv text file can be used as input to sitecmp)
OUTFILE=${BASE}/castnet/output/cast2extV1.0.2001.csv; export OUTFILE

 ${EXEC}

echo run completed, output file = ${OUTFILE}

