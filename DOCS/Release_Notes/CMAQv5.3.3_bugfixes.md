# CMAQv5.3.3 Bugfixes

## 1. POST tool bug fixes in hr2day and sitecmp_dailyo3
[Christian Hogrefe](mailto:hogrefe.christian@epa.gov), U.S. Environmental Protection Agency

### Description of model issue

*hr2day*: When using the hr2day MAXDIF operation in conjunction with setting PARTIAL_DAY to F, the previous code returned missing values for all days.

*hr2day*: When run script variable START_DATE was set to a date later than the start date of M3_FILE_1, the previous code generated empty time steps for the time period between the start date of M3_FILE_1 and environment variable START_DATE. 

*sitecmp_dailyo3*: When the values in the in `OZONE_F` column of CASTNET `IN_TABLE` files were enclosed in quotes, the code did not remove those quotes, therefore  did not properly match it to any of the known QA codes for which values should be discarded, and consequently did not discard such flagged values before computing the daily metrics. In the CASTNET files distributed via CMAS, this only affected the 2005 observation file.

### Solution in CMAQv5.3.3

The *hr2day* code was updated to correct the behavior of the MAXDIF operation when PARTIAL_DAY is set to F. It also was updated so that OUTFILE only contains time steps between MAX(start of M3_FILE_1, START_DATE) and MIN(end of M3_FILE_n, END_DATE)
 
The *sitecmp_dailyo3* code was updated to remove any quotes from the `OZONE_F` column of CASTNET `IN_TABLE` files. 

### Files Affected 

POST/hr2day/src/hr2day.F
POST/sitecmp_dailyo3/src/utilities.F
