#!/bin/csh -f
set echo

#  --------------------------------------
#  Add /usr/local/lib to the library path
#  --------------------------------------
#   if [ -z ${LD_LIBRARY_PATH} ]
#   then
#      export LD_LIBRARY_PATH=/usr/local/lib
#   else
#      export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/local/lib
#   fi
#  ----------------------
#  Unpack and build IOAPI
#  ----------------------
   setenv INSTDIR /21dayscratch/scr/l/i/lizadams/CMAQv5.5/LIBRARIES
   cd $INSTDIR
   git clone https://github.com/cjcoats/ioapi-3.2
   cd ioapi-3.2
   git checkout -b 20200828
   setenv BASEDIR $INSTDIR/ioapi-3.2
   setenv BIN Linux2_x86_64gfort
   mkdir $BASEDIR/$BIN
   setenv CPLMODE nocpl
   # Edit Makefile or use syntax: make BIN=Linux2_x86_64pg  CPLMODE=pncf INSTALL=/foo/bar
   cd $BASEDIR/ioapi
   cp Makefile.$CPLMODE  ${BASEDIR}/ioapi/Makefile
   cp Makefile.$CPLMODE  ${BASEDIR}/m3tools/Makefile
   # need updated Makefile to include ‘-DIOAPI_NCF4=1’ to the MFLAGS make-variable to avoid multiple definition of `nf_get_vara_int64_’
   sed -i -e 's/m64/m64 -DIOAPI_NCF4=1/g' Makeinclude.Linux2_x86_64gfort 
   make HOME=$INSTDIR | & tee make.log
   cd $INSTDIR/ioapi-3.2/m3tools
   make HOME=$INSTDIR | & tee make.log
   #   cd $BASEDIR/m3tools
   #cp $PDIR/Makefile.template Makefile
   #make HOME=$DIR/install
