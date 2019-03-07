# The create_CMAQ_OMI_file Utility

The utility creates the OMI_DAT input file describing how total ozone column density varies over the globe and time.
The file support CMAQ model's in-line calculation of photolysis rates. Creating the OMI_DAT file involves processing observations 
from satellites, ASCII files for the latitude/longitude distribution of the ozone for a calendar day. The utility also create a 
IOAPI files for visualing the observations and data in the OMI_DAT file. They differ because the utility interpolates
observations to horizontal resolution of the OMI_DAT file. The resolution is an option specified by the utility run-script.
