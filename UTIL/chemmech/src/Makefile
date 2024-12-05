# original file copied from /home/yoj/src/mech/BLD_2, 1/31/2008

  MODEL = CHEMMECH.exe

ifndef compiler
 compiler = intel
#compiler = gcc
#compiler = pgi
#compiler = nvhpc
endif

ifeq "$(compiler)" "intel"

 FC = ifort
 CC = icc
 f_FLAGS   = -fixed -132 -O0 -fpe0 -check uninit -check bounds -check format -g -traceback -fno-alias -mp1  -I . 
 F_FLAGS   = $(f_FLAGS)
 f90_FLAGS = -free -132 -O0 -fpe0 -check uninit -check bounds -check format -g -traceback -fno-alias -mp1  -I . 
 F90_FLAGS = $(f90_FLAGS)
 C_FLAGS   =  -O2  -DFLDMN=1
 LINK_FLAGS = 

else ifeq "$(compiler)" "pgi"

 FC = pgf90
 CC = pgcc
 f_FLAGS   = -Mfixed -Mextend -Mbounds  -Mchkptr -Mchkstk -traceback -Ktrap=fp -O0 -I . -g
 F_FLAGS   = $(f_FLAGS) 
 f90_FLAGS = -Mfree -Mextend -Mbounds -Mchkptr -Mchkstk -traceback -Ktrap=fp -O0 -I . -g
 F90_FLAGS = $(f90_FLAGS)
 C_FLAGS   =  -O2  -DFLDMN=1
 LINK_FLAGS = 

else ifeq "$(compiler)" "nvhpc"

 FC = nvfortran
 CC = nvc
 f_FLAGS   = -Mfixed -Mextend -Mbounds  -Mchkptr -Mchkstk -traceback -Ktrap=fp -O0 -I . -g
 F_FLAGS   = $(f_FLAGS) 
 f90_FLAGS = -Mfree -Mextend -Mbounds  -Mchkptr -Mchkstk -traceback -Ktrap=fp -O0 -I . -g
 F90_FLAGS = $(f90_FLAGS)
 C_FLAGS   =  -O2  -DFLDMN=1
 LINK_FLAGS = 

else ifeq "$(compiler)" "gcc"

 FC    = gfortran
 CC    = gcc
 f_FLAGS       = -ffixed-form -ffixed-line-length-132 -O0 -funroll-loops -finit-character=32 -fbacktrace -fcheck=all -I. 
 F_FLAGS       = $(f_FLAGS)
 f90_FLAGS     = -cpp -ffree-form -ffree-line-length-none -O0 -funroll-loops -finit-character=32 --fbacktrace fcheck=all -I. 
 F90_FLAGS     = $(f90_FLAGS)
 C_FLAGS       = -O2 -DFLDMN -I .
 LINKER        = $(FC)
 LINK_FLAGS    = 

else
 ERROR1 = "Makefile not configured to support the specified compiler, $(compiler). User must modify Makefile."
endif
 
ifdef ERROR1
 $(error $(ERROR1))
endif

CPP_FLAGS = -DCHEMMECH

OBJECTS =\
    MECHANISM_PARMS.o \
    module_envvar.o \
    TEXT_UTILITIES.o \
    BASIC_WRITE_ROUTINES.o \
    MECHANISM_DATA.o \
    GET_MECHDEF_DATA.o \
    module_functions_evaluator.o \
    cctm_species.o \
    SPECIES_ATOMS_DATA.o \
    MECHANISM_DOCS.o \
    chemmech.o \
    junit.o \
    index1.o \
    wrexts.o \
    wrspecial_ext.o \
    wrss_ext.o  \
    wrt_kpp_inputs.o \
    wrt_reactions_module.o \
    wrspecial_ext_fortran90.o \
    wrexts_fortran90.o \
    wrt_rates.o \
    wrss_ext_fortran90.o
        
#   wrhdr1.o \
#   wrbf6.o \
#   wrbf16c.o \
#   wrbf12d.o \
#   wrbf12s.o \
#   wrbf6_fortran90.o \
#   wrbf16c_fortran90.o \
#   wrbf12d_fortran90.o \
#   wrbf12s_fortran90.o \

.SUFFIXES: .f .c

$(MODEL): $(OBJECTS)
	$(FC) $(LINK_FLAGS) $(OBJECTS) -o $@

.f.o:
	$(FC) $(F_FLAGS) -c $<

.F.o:
	$(FC) $(F_FLAGS) $(CPP_FLAGS) -c $<

.c.o:
	$(CC) $(C_FLAGS) -c $<

clean:
	\rm -f *.o *.mod $(MODEL)
