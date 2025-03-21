#!/bin/csh -f
set echo

#
#  Install used tcsh and intel version 18.2 and openmpi
#  for classic version of netCDF library 
#

module load openmpi_3.1.4/intel_18.2
   /bin/tcsh --version
   ifort --version
   icc --version
   module list | grep openmpi
   which mpirun
   
# compilers
setenv SERIAL_FC ifort
setenv SERIAL_F77 ifort
setenv SERIAL_CC icc
setenv SERIAL_CXX icpc
setenv MPI_FC mpifort
setenv MPI_F77 mpifort
setenv MPI_CC mpicc
setenv MPI_CXX mpic++
setenv CC $SERIAL_CC
setenv CXX $SERIAL_CXX
setenv F77 $SERIAL_F77
setenv FC $SERIAL_FC
unsetenv F90  # This seems to be set by default on NCAR's Cheyenne and is problematic
unsetenv F90FLAGS


#
#  unset envioronment variables that would conflict with this installation
#

   unsetenv LDFLAGS
   unsetenv CPPFLAGS

#  --------------------
#  Set directory for CMAQ Libraries 
#  -------------------

   mkdir -p $cwd/LIBRARIES_intel_classic
   setenv INSTDIR $cwd/LIBRARIES_intel_classic

#  ---------------------------
#  Download and build netCDF-C
#  ---------------------------
   cd  ${INSTDIR}
   wget https://github.com/Unidata/netcdf-c/archive/refs/tags/v4.8.1.tar.gz
   tar xvf v4.8.1.tar.gz
   cd netcdf-c-4.8.1
   setenv LDFLAGS "-L${INSTDIR}/lib"
   setenv CPPFLAGS "-I${INSTDIR}/include"
   ./configure --disable-netcdf-4 --disable-shared --disable-dap --prefix=$INSTDIR
   make -j 4 |& tee  make.intel.log
   make install
#  ---------------------------------
#  Download and build netCDF-Fortran
#  ---------------------------------
   cd ${INSTDIR}
   wget https://github.com/Unidata/netcdf-fortran/archive/refs/tags/v4.5.3.tar.gz
   # installation instructions
   tar xvf v4.5.3.tar.gz
   cd netcdf-fortran-4.5.3
   #cd netcdf-fortran-4.4.5
   # Edit configure to remove -qversion
   sed -i -e 's/-qversion//g'
   ## Note, if non-standard locaions are used for the following compilers, you may need to specify their locations here: 
   setenv LDFLAGS "-L${INSTDIR}/lib"
   setenv CPPFLAGS "-I${INSTDIR}/include"
   setenv LIBS "-L${INSTDIR}/lib -lnetcdf"
   setenv NCDIR ${INSTDIR}
   setenv CPPFLAGS "-I${INSTDIR}/include"
   setenv LDFLAGS "-L${INSTDIR}/lib"
   setenv LD_LIBRARY_PATH ${INSTDIR}/lib:${LD_LIBRARY_PATH}
   ./configure --disable-shared --disable-zstandard-plugin --disable-netcdf-4 -prefix=$INSTDIR
   make |& tee make.intel.log 
   make install |& tee make.intel.log

# check version that has been installed
   cd $INSTDIR/bin
   ./nc-config --version
   ./nf-config --version
   #   ncxx4-config --version
