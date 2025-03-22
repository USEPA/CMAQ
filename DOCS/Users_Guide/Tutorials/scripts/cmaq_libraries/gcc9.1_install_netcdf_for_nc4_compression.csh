#!/bin/csh -f
set echo

#
#  Install used tcsh and gcc/gfortran version 9.1.0 and openmpi
#

   module load openmpi_4.0.1/gcc_9.1.0
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

   mkdir -p $cwd/LIBRARIES_gcc
   setenv INSTDIR $cwd/LIBRARIES_gcc

# ----------------------
# Build and install curl
# ---------------------

 cd ${INSTDIR}
 wget https://github.com/curl/curl/releases/download/curl-8_11_0/curl-8.11.0.tar.gz
 tar -xzvf curl-8.11.0.tar.gz
 cd curl-8.11.0
 ./configure --prefix=${INSTDIR} --without-ssl --without-libpsl
 make |& tee make.curl.log
 make install |& tee make.install.curl.log

#  ----------------------
# Build and install zlib
#  ---------------------

  cd ${INSTDIR}
  wget https://github.com/madler/zlib/releases/download/v1.3/zlib-1.3.tar.gz 
  tar -xzvf zlib-1.3.tar.gz
  cd zlib-1.3
  ./configure --prefix=${INSTDIR}
  make -j 4
  #make test |& tee make.test.log
  make install |& tee make.install.log

#  -----------------------
#  Download and build HDF5
#  -----------------------
   cd ${INSTDIR}
   wget https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.10/hdf5-1.10.5/src/hdf5-1.10.5.tar.gz
   tar xvf hdf5-1.10.5.tar.gz
   rm -f hdf5-1.10.5.tar.gz
   cd hdf5-1.10.5
   setenv LDFLAGS "-L${INSTDIR}/lib"
   setenv CPPFLAGS "-I${INSTDIR}/include"
   setenv CFLAGS "-O3"
   setenv FFLAGS "-O3"
   setenv CXXFLAGS "-O3"
   setenv FCFLAGS "-O3"
   ./configure --prefix=${INSTDIR} --enable-fortran --enable-cxx --with-zlib=${INSTDIR}/include,${INSTDIR}/lib -enable-shared --enable-hl
   make -j 4 |& tee make.gcc9.log
#  make check > make.gcc9.check
   make install |& tee make.gcc9.log
#  ---------------------------
#  Download and build netCDF-C
#  ---------------------------
   cd  ${INSTDIR}
   wget https://github.com/Unidata/netcdf-c/archive/refs/tags/v4.8.1.tar.gz
   tar xvf v4.8.1.tar.gz
   cd netcdf-c-4.8.1
   setenv LDFLAGS "-L${INSTDIR}/lib"
   setenv CPPFLAGS "-I${INSTDIR}/include"
   ./configure --with-pic --enable-netcdf-4 --enable-shared --prefix=${INSTDIR}
   make -j 4 |& tee  make.gcc9.log
   make install
#  ---------------------------------
#  Download and build netCDF-Fortran
#  ---------------------------------
   cd ${INSTDIR}
   wget https://github.com/Unidata/netcdf-fortran/archive/refs/tags/v4.5.3.tar.gz
   # installation instructions
   tar xvf v4.5.3.tar.gz
   #tar xzvf v4.4.5.tar.gz
   cd netcdf-fortran-4.5.3
   #cd netcdf-fortran-4.4.5
   ## Note, if non-standard locaions are used for the following compilers, you may need to specify their locations here: 
   setenv LIBS "-L${INSTDIR}/lib -lnetcdf -lhdf5_hl -lhdf5 -lhdf5_fortran -lhdf5hl_fortran -lm -ldl -lz -lcurl "
   setenv NCDIR ${INSTDIR}
   setenv CPPFLAGS "-I${INSTDIR}/include"
   setenv LDFLAGS "-L${INSTDIR}/lib"
   setenv LD_LIBRARY_PATH ${INSTDIR}/lib:${LD_LIBRARY_PATH}
   ./configure --with-pic  --enable-shared --prefix=${INSTDIR}
   make |& tee make.gcc9.log 
   make install |& tee make.gcc9.log
#  -----------------------------
#  Download and build netCDF-CXX
#  -----------------------------
#   cd  $INSTDIR
#   wget https://github.com/Unidata/netcdf-cxx4/archive/refs/tags/v4.3.1.tar.gz
#   tar xvf v4.3.1.tar.gz
#   cd netcdf-cxx4-4.3.1
#   ./configure --enable-shared --prefix=$INSTDIR
#   make |& tee  make.gcc9.log
#   make install
#  --------------------------
#  Download and build OpenMPI
#  --------------------------
#   cd $INSTDIR
#   wget https://download.open-mpi.org/release/open-mpi/v3.1/openmpi-3.1.4.tar.gz
#   tar xvf openmpi-3.1.4.tar.gz
#   rm -f openmpi-3.1.4.tar.gz
#   cd openmpi-3.1.4
#   export CFLAGS="-O3"
#   export FFLAGS="-O3"
#   export CXXFLAGS="-O3"
#   export FCFLAGS="-O3"
#   ./configure --prefix=$INSTDIR --enable-mpi-cxx
#   make |& tee make.gcc9.log
##  make check > make.gcc9.check
#   make install
#  ----------------------------------
#  Download and build Parallel netCDF
#  ----------------------------------
#  NOTE: the openmpi directory path is hard coded on this script and needs to be updated for your local machine
##
#cd  $INSTDIR
#   wget https://parallel-netcdf.github.io/Release/pnetcdf-1.12.1.tar.gz
#   tar xvf pnetcdf-1.12.1.tar.gz
#   rm -f pnetcdf-1.12.1.tar.gz
#   cd pnetcdf-1.12.1
#   #export CFLAGS="-O3 -fPIC"
#   #export FFLAGS="-O3 -fPIC"
#   #export CXXFLAGS="-O3 -fPIC"
#   #export FCFLAGS="-O3 -fPIC"
#   #./configure --prefix=$INSTDIR MPIF77=mpif90 MPIF90=mpif90 MPICC=mpicc MPICXX=mpicxx --with-mpi=/nas/longleaf/apps/r/4.1.3/openmpi
#   ./configure --prefix=$INSTDIR MPIF77=mpif90 MPIF90=mpif90 MPICC=mpicc MPICXX=mpicxx --with-mpi=/nas/longleaf/apps-dogwood/mpi/gcc_9.1.0/openmpi_4.0.1
#   make |& tee make.gcc9.log
#   make install
#  ----------------------------------------
#  Use tcsh 6.20 instead of the broken 6.21
#  ----------------------------------------
#   cd /shared/build-hdf5
#   wget http://ftp.funet.fi/pub/mirrors/ftp.astron.com/pub/tcsh/old/tcsh-6.20.00.tar.gz
#   tar xvf tcsh-6.20.00.tar.gz
#   rm -f tcsh-6.20.00.tar.gz
#   cd tcsh-6.20.00
#   ./configure --disable-nls
#   make > make.gcc9.log 2>&1
#   make install
#   ln -s /usr/local/bin/tcsh /bin/csh
#  ----------------------
#  Download and build vim
#  ----------------------
#   cd /usr/local/src
#   git clone https://github.com/vim/vim.git vim
#   cd vim
#   ./configure
#   make > make.gcc9.log 2>&1
#   make install
#   cd /usr/local/bin
#   ln -s vim vi

# install test
   cd $INSTDIR/bin
   ls h5diff
   whereis h5diff
   ./nc-config --version
   ./nf-config --version
   #   ncxx4-config --version
