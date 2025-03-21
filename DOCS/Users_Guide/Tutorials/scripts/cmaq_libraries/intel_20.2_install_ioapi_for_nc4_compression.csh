#!/bin/csh -f
# Build I/O API version that supports NCF4 
# Note - this script works for intel 20.2
set echo

#  Install used tcsh and intel and openmpi
#   module load  intel/20.2 openmpi/4.1.4-intel_20.2

   setenv INSTDIR $cwd/LIBRARIES_intel
   cd $INSTDIR

#  --------------------------------------
#  Add  to the library path
#  --------------------------------------
   if (! $?LD_LIBRARY_PATH) then
      setenv  LD_LIBRARY_PATH $INSTDIR/lib
   else
     setenv  LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:$INSTDIR/lib
   endif
#  ----------------------
#  Unpack and build IOAPI
#  ----------------------
   git clone ssh://github.com/cjcoats/ioapi-3.2
   #git clone https://github.com/cjcoats/ioapi-3.2
   cd ioapi-3.2
   git checkout -b 20200828
   setenv BASEDIR $INSTDIR/ioapi-3.2
   setenv BIN Linux2_x86_64ifort
   mkdir $BASEDIR/$BIN
   setenv CPLMODE nocpl
   # Edit Makefile or use syntax: make BIN=Linux2_x86_64pg  CPLMODE=nocpl INSTALL=$INSTDIR
   cd $BASEDIR/ioapi
   # Copy the Makefile template
   cp $BASEDIR/ioapi/Makefile.$CPLMODE  ${BASEDIR}/ioapi/Makefile
   cp ${BASEDIR}/m3tools/Makefile.$CPLMODE  ${BASEDIR}/m3tools/Makefile
   # Modify to specify the path of the netcdf libraries
   sed -i 's/\-lnetcdff/\-L\$\{HOME\}\/lib \-lnetcdff \-lnetcdf \-lhdf5_hl \-lhdf5 \-lm \-lz \-lsz \-lcurl/g' ${BASEDIR}/m3tools/Makefile
   # need updated Makefile to include ‘-DIOAPI_NCF4=1’ to the MFLAGS make-variable to avoid multiple definition of `nf_get_vara_int64_’
   # Makefile can be edited to use these options instead of the default options
   #    VFLAG  = -DVERSION='3.2-nocpl-ncf4'
   #    DEFINEFLAGS = -DIOAPI_NCF4=1 $(ARCHFLAGS) $(PARFLAGS)
   #This will remove # from the start of line 102 or add it if it wasn't already there:
   sed -i '102s/^#/\n/; 102s/^[^\n]/#&/; 102s/^\n//' Makefile
   sed -i '100s/^#/\n/; 100s/^[^\n]/#&/; 100s/^\n//' Makefile
   sed -i '109s/^#/\n/; 109s/^[^\n]/#&/; 109s/^\n//' Makefile
   sed -i '111s/^#/\n/; 111s/^[^\n]/#&/; 111s/^\n//' Makefile
   #sed -i -e 's/m64/m64 -DIOAPI_NCF4=1/g' Makeinclude.Linux2_x86_64ifort 
   sed -i -e 's/-openmp/#-openmp/g' Makeinclude.Linux2_x86_64ifort
   make HOME=$INSTDIR | & tee make.ioapi.log
   cd $INSTDIR/ioapi-3.2/m3tools
   make HOME=$INSTDIR | & tee make.m3tools.log
