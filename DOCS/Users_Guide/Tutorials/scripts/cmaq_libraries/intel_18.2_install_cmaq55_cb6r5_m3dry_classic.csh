#!/bin/csh -f
# for intel 18.2 and using netCDF classic and I/O API 

#  -----------------------
#  Download and build CMAQ
#  -----------------------
# NOTE - please change the BUILD, CMAQ_HOME and OPENMPI directory to your local paths
# To find the openmpi path, search for mpirun, and then look for the include and lib directories
# > which mpirun
setenv BUILD $cwd/LIBRARIES_intel_classic
setenv IOAPI_DIR $BUILD/ioapi-3.2/Linux2_x86_64ifort
setenv NETCDF_DIR $BUILD/lib
setenv NETCDFF_DIR $BUILD/lib
setenv OPENMPI /nas/longleaf/apps-dogwood/mpi/intel_18.2/openmpi_3.1.4
cd $BUILD/..
#git clone -b 55  https://github.com/USEPA/CMAQ/CMAQ.git CMAQ_REPO_v55
git clone -b main ssh://github.com/USEPA/CMAQ.git CMAQ_REPO_v55

echo "downloaded CMAQv55"
cd CMAQ_REPO_v55
cd $BUILD/../CMAQ_REPO_v55/
# Change CMAQ_HOME to a local directory
   #This will remove # from the start of line 102 or add it if it wasn't already there:
      sed -i '19s/^#/\n/; 19s/^[^\n]/#&/; 19s/^\n//' bldit_project.csh
   # EDIT this path to specify the BUILD directory set above 
      sed -i '20i set CMAQ_HOME = /21dayscratch/scr/l/i/lizadams/test/openmpi_intel_classic' bldit_project.csh

set CMAQ_HOME = $BUILD/../openmpi_intel_classic
mkdir $BUILD/../openmpi_intel_classic
./bldit_project.csh
# Load the Openmpi module 
# Edit this name to match what is available on your local machine

module load openmpi_3.1.4/intel_18.2
 # edit config_cmaq.csh to specify the library locations
 cd $BUILD/../openmpi_intel_classic/
 # EDIT this path to specify the location of the BUILD directory set above
 sed -i '81i \       setenv BUILD /21dayscratch/scr/l/i/lizadams/test/LIBRARIES_intel_classic' config_cmaq.csh
 # EDIT this path to specify the location of the mpirun path, find using which mpirun after loading the openmpi module 
 sed -i '82i \       setenv OPENMPI /nas/longleaf/apps-dogwood/mpi/intel_18.2/openmpi_3.1.4/' config_cmaq.csh
 sed -i 's@ioapi_inc_intel@$BUILD\/ioapi-3.2\/ioapi\/fixed_src@g' config_cmaq.csh
 sed -i 's@ioapi_lib_intel@$BUILD\/ioapi-3.2\/Linux2_x86_64ifort@g' config_cmaq.csh
 sed -i 's@netcdf_lib_intel@$BUILD\/lib@g' config_cmaq.csh
 sed -i 's@netcdf_inc_intel@$BUILD\/include@g' config_cmaq.csh
 sed -i 's@netcdff_lib_intel@$BUILD\/lib@g' config_cmaq.csh
 sed -i 's@netcdff_inc_intel@$BUILD\/include@g' config_cmaq.csh
 sed -i 's@mpi_incl_intel@$OPENMPI\/include@g' config_cmaq.csh
 sed -i 's@mpi_lib_intel@$OPENMPI\/lib@g' config_cmaq.csh
 #edit the config_cmaq.csh to use -fopenmp due to it being used by default for I/O API Library
 sed -i '172i \       setenv myLINK_FLAG -fopenmp' config_cmaq.csh
 #edit the config_cmaq.csh to add extra libraries
 sed -i 's@-lnetcdf\"  #@-lnetcdf \"  #@g'  config_cmaq.csh
 sed -i -e 's/mpiifort/mpifort/g' config_cmaq.csh
cd ${CMAQ_HOME}/CCTM/scripts/
 cp bldit_cctm.csh bldit_cctmv55_cb6r5_m3dry.csh
 # Add extra libs to support nc4 compression in config_cmaq.csh
 #  -lnetcdf -lhdf5_hl -lhdf5 -lm -ldl -lz -lcurl
  setenv extra_lib "-lnetcdf -lm"
 # Add openmp flag to match what was used in I/O API in config_cmaq.csh
 # setenv myLINK_FLAG  "-fopenmp" # openMP not supported w/ CMAQ

./bldit_cctmv55_cb6r5_m3dry.csh intel |& tee ./bldit_cctmv55_cb6r5_m3dry.log
# Verify that the executable was created.
ls -rlt BLD_CCTM_v55_intel_cb6r5_ae7_aq_m3dry/*.exe


#Note, to run CMAQ, please create modules or set the LD_LIBRARY_PATH to include the directories for $BUILD/lib at run time.
