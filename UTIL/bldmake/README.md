The bldmake utility can be compiled without invoking a script for separate program. The repository includes a Makefile to accomplish the task. It supports compiling with the INTEL (ifort), PGI (pgf90), and GCC (gfortran) compilers. The file uses ifort by default. Edit the Makefile to change the default compiler to pgf90 or gfortran. Note the Makefile uses relative paths for each compiler so the PATH environment variable needs to specify where to find the compiler used. 

Follow the below steps to compile bldmake.

*  Go into its source code directory, **src**.
*  Type "make clean" if recompiling the utility.
*  Type "make" if using the default compiler or "make COMPILER=_compiler_ where _compiler_ equals INTEL, PGI, or GCC. 

If the user is using a different compiler, they need to modify the Makefile.
