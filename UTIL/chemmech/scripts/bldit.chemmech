#! /bin/csh -f

# Script to compile the program CHEMMECH for CMAQv5.2beta

#> option to set compiler and build a new executable
 setenv COMPILER  PGF90 #> INTEL, PGF90, or GFORT

 set Xpath = ../src               #> Executable directory
 set EXEC  = CHEMMECH                  #> Executable name
 
cd ${Xpath}; make clean; make
if( ! ( -e ${EXEC} ) )then
   echo "failed to compile ${Xpath}/${EXEC}"
   exit()
endif

exit()

