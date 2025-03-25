#!/bin/csh -f
set echo

#
#  Install used tcsh and gcc/gfortran version 11.4.1 and openmpi
#

module load openmpi_5.0.5/gcc_11.4.1
   /bin/tcsh --version
   gcc --version
   gfortran --version
   module list | grep openmpi
   which mpirun
   
# compilers
setenv SERIAL_FC gfortran
setenv SERIAL_F77 gfortran
setenv SERIAL_CC gcc
setenv SERIAL_CXX g++
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

   mkdir -p $cwd/LIBRARIES_gcc_disable-dap
   setenv INSTDIR $cwd/LIBRARIES_gcc_disable-dap

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
   make -j 4 |& tee  make.gcc.log
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
   ## Note, if non-standard locaions are used for the following compilers, you may need to specify their locations here: 
   setenv LDFLAGS "-L${INSTDIR}/lib"
   setenv CPPFLAGS "-I${INSTDIR}/include"
   setenv LIBS "-L${INSTDIR}/lib -lnetcdf"
   setenv NCDIR ${INSTDIR}
   setenv CPPFLAGS "-I${INSTDIR}/include"
   setenv LDFLAGS "-L${INSTDIR}/lib"
   setenv LD_LIBRARY_PATH ${INSTDIR}/lib:${LD_LIBRARY_PATH}
   ./configure --disable-shared --disable-zstandard-plugin --disable-netcdf-4 -prefix=$INSTDIR
   make |& tee make.gcc.log 
   make install |& tee make.gcc.log

# check version that has been installed
   cd $INSTDIR/bin
   ./nc-config --version
   ./nf-config --version
   #   ncxx4-config --version
