#!/bin/csh -f

#  -----------------------
#  Download and build CMAQ
#  -----------------------
# NOTE - please change the BUILD, CMAQ_HOME and OPENMPI directory to your local paths
# To find the openmpi path, search for mpirun, and then look for the include and lib directories
# > which mpirun
setenv BUILD $cwd/LIBRARIES_intel
setenv IOAPI_DIR $BUILD/ioapi-3.2/Linux2_x86_64ifort
setenv NETCDF_DIR $BUILD/lib
setenv NETCDFF_DIR $BUILD/lib
# Load the OPENMPI module 
# EDIT this module load command to match the module available on your machine
module load openmpi/4.1.4-intel_20.2
# EDIT this path to specify the location of the mpirun path, find using which mpirun after loading the openmpi module 
setenv OPENMPI /nas/longleaf/rhel8/apps/openmpi/4.1.4 
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
      sed -i '20i set CMAQ_HOME = /work/users/l/i/lizadams/test_nc4/openmpi_intel' bldit_project.csh

set CMAQ_HOME = $BUILD/../openmpi_intel
mkdir $BUILD/../openmpi_intel
./bldit_project.csh

 # edit config_cmaq.csh to specify the library locations
 cd $BUILD/../openmpi_intel/
 # EDIT this path to specify the location of the BUILD directory set above
 sed -i '81i \       setenv BUILD /work/users/l/i/lizadams/test_nc4/LIBRARIES_intel' config_cmaq.csh
 # EDIT this path to specify the location of the mpirun path, find using which mpirun after loading the openmpi module 
 sed -i '82i \       setenv OPENMPI /nas/longleaf/rhel8/apps/openmpi/4.1.4' config_cmaq.csh
 sed -i 's@ioapi_inc_intel@$BUILD\/ioapi-3.2\/ioapi\/fixed_src@g' config_cmaq.csh
 sed -i 's@ioapi_lib_intel@$BUILD\/ioapi-3.2\/Linux2_x86_64ifort@g' config_cmaq.csh
 sed -i 's@netcdf_lib_intel@$BUILD\/lib@g' config_cmaq.csh
 sed -i 's@netcdf_inc_intel@$BUILD\/include@g' config_cmaq.csh
 sed -i 's@netcdff_lib_intel@$BUILD\/lib@g' config_cmaq.csh
 sed -i 's@netcdff_inc_intel@$BUILD\/include@g' config_cmaq.csh
 sed -i 's@mpi_incl_intel@$OPENMPI\/include@g' config_cmaq.csh
 sed -i 's@mpi_lib_intel@$OPENMPI\/lib@g' config_cmaq.csh
 #edit the config_cmaq.csh to use -fopenmp due to it being used by default for I/O API Library
 sed -i '172i \       setenv myLINK_FLAG -qopenmp' config_cmaq.csh
 #edit the config_cmaq.csh to add extra libraries
 sed -i 's@-lnetcdf\"  #@-lnetcdf -lcurl -lhdf5 -lhdf5_hl \"  #@g'  config_cmaq.csh
 #edit the config_cmaq.csh to change mpiifort to mpifort
 sed -i -e 's/mpiifort/mpifort/g'  config_cmaq.csh
cd $CMAQ_HOME/CCTM/scripts/
 cp bldit_cctm.csh bldit_cctmv55_cb6r5_m3dry.csh
 # Add extra libs to support nc4 compression in config_cmaq.csh
 #  -lnetcdf -lhdf5_hl -lhdf5 -lm -ldl -lz -lcurl
  setenv extra_lib "-lnetcdf -lhdf5_hl -lhdf5 -lm -ldl -lz -lsz -lcurl"
 # Add openmp flag to match what was used in I/O API in config_cmaq.csh
 # setenv myLINK_FLAG  "-fopenmp" # openMP not supported w/ CMAQ

./bldit_cctmv55_cb6r5_m3dry.csh intel |& tee ./bldit_cctmv55_cb6r5_m3dry_intel.log
# Verify that the executable was created.
ls -rlt BLD_CCTM_v55_intel_cb6r5_ae7_aq_m3dry/*.exe


#Note, to run CMAQ, please create modules or set the LD_LIBRARY_PATH to include the directories for $BUILD/lib at run time.

##see this tutorial for instructions to install modules: 
##https://pcluster-cmaq.readthedocs.io/en/latest/user_guide_pcluster/developers_guide/cmaq-vm/install.html#install-environment-modules
## If you have modules on your machine, you can create custom modules
## https://pcluster-cmaq.readthedocs.io/en/latest/user_guide_pcluster/developers_guide/cmaq-vm/install.html#create-custom-environment-module-for-libraries


