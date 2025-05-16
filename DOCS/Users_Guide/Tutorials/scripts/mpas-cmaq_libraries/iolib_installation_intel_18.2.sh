#!/usr/bin/env bash
# Script works for module load intel/18.2 and openmpi_3.1.4/intel_18.2 modules

#
# Sources for all libraries used in this script can be found at
# http://www2.mmm.ucar.edu/people/duda/files/mpas/sources/ 
#

# Where to find sources for libraries
export LIBSRC=$PWD

# Where to install libraries
export LIBBASE=$PWD

# Compilers
export SERIAL_FC=ifort
export SERIAL_F77=ifort
export SERIAL_CC=icc
export SERIAL_CXX=icpc
export MPI_FC=mpif90
export MPI_F77=mpif77
export MPI_CC=mpicc
export MPI_CXX=mpic++


export CC=$SERIAL_CC
export CXX=$SERIAL_CXX
export F77=$SERIAL_F77
export FC=$SERIAL_FC
unset F90  # This seems to be set by default on NCAR's Cheyenne and is problematic
unset F90FLAGS
#export CFLAGS="-g"
#export FFLAGS="-g -fbacktrace"
#export FCFLAGS="-g -fbacktrace"
#export F77FLAGS="-g -fbacktrace"


########################################
# MPICH
########################################
#tar xzvf ${LIBSRC}/mpich-3.3.1.tar.gz 
#cd mpich-3.3.1
#./configure --prefix=${LIBBASE}
#make -j 4
##make check
#make install
##make testing
#export PATH=${LIBBASE}/bin:$PATH
#export LD_LIBRARY_PATH=${LIBBASE}/lib:$LD_LIBRARY_PATH
#cd ..
#rm -rf mpich-3.3.1

########################################
# zlib
########################################
wget https://www2.mmm.ucar.edu/people/duda/files/mpas/sources/zlib-1.2.11.tar.gz
tar xzvf ${LIBSRC}/zlib-1.2.11.tar.gz
cd zlib-1.2.11
./configure --prefix=${LIBBASE} --static
make -j 4
make install
cd ..
rm -rf zlib-1.2.11

# ----------------------
# Build and install curl
# ---------------------

wget https://curl.se/download/curl-8.10.1.tar.gz
tar -xzvf curl-8.10.1.tar.gz
cd curl-8.10.1
./configure --prefix=${LIBBASE} --without-ssl --without-libpsl
make |& tee make.curl.log
make install |& tee make.install.curl.log


########################################
# HDF5
########################################
wget https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.10/hdf5-1.10.5/src/hdf5-1.10.5.tar.bz2
tar xjvf ${LIBSRC}/hdf5-1.10.5.tar.bz2
cd hdf5-1.10.5
export FC=$MPI_FC
export CC=$MPI_CC
export CXX=$MPI_CXX
./configure --prefix=${LIBBASE} --enable-parallel --with-zlib=${LIBBASE} --disable-shared
make -j 4
#make check
make install
cd ..
rm -rf hdf5-1.10.5

########################################
# Parallel-netCDF
########################################
wget https://parallel-netcdf.github.io/Release/pnetcdf-1.11.2.tar.gz
tar xzvf ${LIBSRC}/pnetcdf-1.11.2.tar.gz
cd pnetcdf-1.11.2
export CC=$SERIAL_CC
export CXX=$SERIAL_CXX
export F77=$SERIAL_F77
export FC=$SERIAL_FC
export MPICC=$MPI_CC
export MPICXX=$MPI_CXX
export MPIF77=$MPI_F77
export MPIF90=$MPI_FC
### Will also need gcc in path
./configure --prefix=${LIBBASE}
make -j 4
#make check
#make ptest
#make testing
make install
export PNETCDF=${LIBBASE}
cd ..
rm -rf pnetcdf-1.11.2

########################################
# netCDF (C library)
########################################
wget https://github.com/Unidata/netcdf-c/archive/refs/tags/v4.7.0.tar.gz
tar xzvf ${LIBSRC}/v4.7.0.tar.gz
cd netcdf-c-4.7.0
export CPPFLAGS="-I${LIBBASE}/include"
export LDFLAGS="-L${LIBBASE}/lib"
export LIBS="-lhdf5_hl -lhdf5 -lz -ldl"
export CC=$MPI_CC
./configure --prefix=${LIBBASE} --disable-dap --enable-netcdf4 --enable-pnetcdf --enable-cdf5 --enable-parallel-tests --disable-shared
make -j 4 
#make check
make install
export NETCDF=${LIBBASE}
cd ..
rm -rf netcdf-c-4.7.0

########################################
# netCDF (Fortran interface library)
########################################
wget https://www2.mmm.ucar.edu/people/duda/files/mpas/sources/netcdf-fortran-4.4.5.tar.gz
tar xzvf ${LIBSRC}/netcdf-fortran-4.4.5.tar.gz
cd netcdf-fortran-4.4.5
export FC=$MPI_FC
export F77=$MPI_F77
export LIBS="-lnetcdf ${LIBS}"
./configure --prefix=${LIBBASE} --enable-parallel-tests --disable-shared
make -j 4
#make check
make install
cd ..
rm -rf netcdf-fortran-4.4.5

########################################
# PIO
########################################
git clone ssh://github.com/NCAR/ParallelIO
cd ParallelIO
git checkout -b pio-2.4.4 pio2_4_4
export PIOSRC=`pwd`
cd ..
mkdir pio
cd pio
export CC=$MPI_CC
export FC=$MPI_FC
cmake -DNetCDF_C_PATH=$NETCDF -DNetCDF_Fortran_PATH=$NETCDF -DPnetCDF_PATH=$PNETCDF -DHDF5_PATH=$NETCDF -DCMAKE_INSTALL_PREFIX=$LIBBASE -DPIO_USE_MALLOC=ON -DCMAKE_VERBOSE_MAKEFILE=1 -DPIO_ENABLE_TIMING=OFF $PIOSRC
make
#make check
make install
cd ..
#rm -rf pio ParallelIO
export PIO=$LIBBASE

########################################
# Other environment vars needed by MPAS
########################################
export MPAS_EXTERNAL_LIBS="-L${LIBBASE}/lib -lhdf5_hl -lhdf5 -ldl -lz"
export MPAS_EXTERNAL_INCLUDES="-I${LIBBASE}/include"
