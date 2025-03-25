#!/bin/csh -f
# for gcc 11.4.1
# using classic netCDF and I/O API libraries

#  -----------------------
#  Download and build CMAQ
#  -----------------------
# NOTE - please change the BUILD, CMAQ_HOME and OPENMPI directory to your local paths
# To find the openmpi path, search for mpirun, and then look for the include and lib directories
# > which mpirun
setenv BUILD $cwd/LIBRARIES_gcc_disable-dap
setenv IOAPI_DIR $BUILD/ioapi-3.2/Linux2_x86_64gfort10
setenv NETCDF_DIR $BUILD/lib
setenv NETCDFF_DIR $BUILD/lib
setenv OPENMPI /nas/sycamore/apps/openmpi/5.0.5/
cd $BUILD/..
#git clone -b 55  https://github.com/USEPA/CMAQ/CMAQ.git CMAQ_REPO_v55
git clone -b main ssh://github.com/USEPA/CMAQ.git CMAQ_REPO_v55

echo "downloaded CMAQv55"
cd CMAQ_REPO_v55
cd $BUILD/../CMAQ_REPO_v55/
# Change CMAQ_HOME to a local directory
   #This will remove # from the start of line 102 or add it if it wasn't already there:
      sed -i '19s/^#/\n/; 19s/^[^\n]/#&/; 19s/^\n//' bldit_project.csh
      sed -i '20i set CMAQ_HOME = /proj/ie/proj/CMAS/CMAQ/CMAQv5.5/build_sycamore/openmpi_gcc_disable_dap' bldit_project.csh

set CMAQ_HOME = $BUILD/../openmpi_gcc_disable_dap
mkdir $BUILD/../openmpi_gcc_disable_dap
./bldit_project.csh
# Load the Openmpi module 
# Edit this name to match what is available on your local machine

module load openmpi_5.0.5/gcc_11.4.1
 # edit config_cmaq.csh to specify the library locations
 cd $BUILD/../openmpi_gcc_disable_dap/
 sed -i '144i \       setenv BUILD /proj/ie/proj/CMAS/CMAQ/CMAQv5.5/build/LIBRARIES_gcc_disable-dap' config_cmaq.csh
 sed -i '145i \       setenv OPENMPI /nas/sycamore/apps/openmpi/5.0.5/' config_cmaq.csh
 sed -i 's@ioapi_inc_gcc@$BUILD\/ioapi-3.2\/ioapi\/fixed_src@g' config_cmaq.csh
 sed -i 's@ioapi_lib_gcc@$BUILD\/ioapi-3.2\/Linux2_x86_64gfort10@g' config_cmaq.csh
 sed -i 's@netcdf_lib_gcc@$BUILD\/lib@g' config_cmaq.csh
 sed -i 's@netcdf_inc_gcc@$BUILD\/include@g' config_cmaq.csh
 sed -i 's@netcdff_lib_gcc@$BUILD\/lib@g' config_cmaq.csh
 sed -i 's@netcdff_inc_gcc@$BUILD\/include@g' config_cmaq.csh
 sed -i 's@mpi_incl_gcc@$OPENMPI\/include@g' config_cmaq.csh
 sed -i 's@mpi_lib_gcc@$OPENMPI\/lib@g' config_cmaq.csh
 #edit the config_cmaq.csh to use -fopenmp due to it being used by default for I/O API Library
 sed -i '172i \       setenv myLINK_FLAG -fopenmp' config_cmaq.csh
 #edit the config_cmaq.csh to add extra libraries
 sed -i 's@-lnetcdf\"  #@-lnetcdf -lcurl -lm -lzip \"  #@g'  config_cmaq.csh
cd $BUILD/../openmpi_gcc_disable_dap/CCTM/scripts/
 cp bldit_cctm.csh bldit_cctmv55_cb6r5_m3dry.csh
 # Add extra libs to support nc4 compression in config_cmaq.csh
 #  -lnetcdf -lhdf5_hl -lhdf5 -lm -ldl -lz -lcurl
  setenv extra_lib "-lnetcdf -lm -lzip -lcurl"
 # Add openmp flag to match what was used in I/O API in config_cmaq.csh
 # setenv myLINK_FLAG  "-fopenmp" # openMP not supported w/ CMAQ

./bldit_cctmv55_cb6r5_m3dry.csh gcc |& tee ./bldit_cctmv55_cb6r5_m3dry.log
# Verify that the executable was created.
ls -rlt BLD_CCTM_v55_gcc_cb6r5_ae7_aq_m3dry/*.exe


#Note, to run CMAQ, please create modules or set the LD_LIBRARY_PATH to include the directories for $BUILD/lib at run time.



