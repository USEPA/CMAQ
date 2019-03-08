# make file to build combine program

PROGRAM = ro3_mod_env

ifndef COMPILER
  COMPILER = intel
# COMPILER = pgi
# COMPILER = gfort
endif
 
ifneq (,$(filter $(debug), TRUE true ))
    DEBUG = TRUE
endif

ifeq ($(COMPILER),intel)

  FC = ifort
  CC = icc
  LIB = /home/hwo/tools/create_CMAQ_OMI_file/lib/x86_64_atmos/intel
  include_path = -I $(LIB)/ioapi/modules \
                 -I $(LIB)/ioapi/include_files \
                 -I $(LIB)/netcdf/include

  WARN = 
  FSTD = -O2 -traceback
  DBG  = -O0 -g -check bounds -check uninit -fpe0 -fno-alias -ftrapuv -traceback

  ifneq (,$(filter $(DEBUG), TRUE true ))
     f_FLAGS   = -fixed -132 $(DBG) $(include_path)
     f90_FLAGS = -free $(DBG) $(include_path)
  else
      f_FLAGS   = -fixed -132 $(FSTD) $(include_path)
      f90_FLAGS = -free $(FSTD) $(include_path)
  endif
 
   LINK_FLAGS = -i-static

else ifeq ($(COMPILER),pgi)

  FC = pgf90
  CC = pgcc
 
  LIB = /home/hwo/tools/create_CMAQ_OMI_file/lib/x86_64_atmos/pgi
  include_path = -I $(LIB)/ioapi/modules \
                 -I $(LIB)/ioapi/include_files \
                 -I $(LIB)/netcdf/include

  WARN = 
  FSTD = -O3 -Mextend
  DBG  = -O0 -g -Mbounds -Mchkptr -traceback -Ktrap=fp -Mextend

  ifneq (,$(filter $(DEBUG), TRUE true ))
     f_FLAGS   = -Mfixed $(DBG) $(include_path)
     f90_FLAGS = -Mfree $(DBG) $(include_path)
  else
      f_FLAGS   = -Mfixed $(FSTD) $(include_path)
      f90_FLAGS = -Mfree $(FSTD) $(include_path)
  endif

else ifeq ($(COMPILER),gcc)

 FC = gfortran
 CC = cc

 LIB = /home/hwo/tools/create_CMAQ_OMI_file/lib/x86_64_atmos/gcc
 include_path = -I $(LIB)/ioapi/modules \
                -I $(LIB)/ioapi/include_files \
                -I $(LIB)/netcdf/include

 WARN = 
 FSTD = -O2 -funroll-loops -finit-character=32 -Wconversion-extra -Wtabs -Wsurprising
 DBG  = -Wall -O0 -g -fcheck=all -ffpe-trap=invalid,zero,overflow -fbounds-check -fbacktrace -Wno-zerotrip -Wno-unused-function

 ifneq (,$(filter $(DEBUG), TRUE true ))
     f_FLAGS   = -ffixed-form -ffixed-line-length-132 -funroll-loops -finit-character=32 $(DBG) $(include_path)
     f90_FLAGS = -ffree-form -ffree-line-length-none -funroll-loops -finit-character=32 $(DBG) $(include_path)
 else
     f_FLAGS   = -ffixed-form -ffixed-line-length-132 -funroll-loops -finit-character=32 $(FSTD) $(include_path)
     f90_FLAGS = -ffree-form -ffree-line-length-none -funroll-loops -finit-character=32 $(FSTD) $(include_path)
 endif

else
 ERROR1 = "Makefile not configured to support the specified compiler, $(COMPILER). USER MUST USE intel, pgi or gcc options or MODIFY Makefile"
endif

ifdef ERROR1
 $(error $(ERROR1))
endif

F_FLAGS   = $(f_FLAGS)
F90_FLAGS = $(f90_FLAGS)
C_FLAGS   = -I.

LINKER     = $(FC)
LINK_FLAGS = 

CPP = $(FC)
CPP_FLAGS = 

IOAPI  = -L$(LIB)/ioapi/lib -lioapi
NETCDF = -L$(LIB)/netcdf/lib -lnetcdf -lnetcdff
LIBRARIES = $(IOAPI) $(NETCDF)


SRC = \
  get_env_vars.o \
  module_envvar.o \
  create_CMAQ_OMI.o \
  create_extend_OMI.o \
  create_ioapi_OMI.o \
  ro3_mod_env.o \
  viz_cmaqv51_o3totcol.o

OBJS = \
  $() \
  $(SRC)

.SUFFIXES: .F .f .c .F90 .f90

$(PROGRAM): $(OBJS)
	$(LINKER) $(LINK_FLAGS) $(OBJS) $(LIBRARIES) -o $@

.F.o:
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

.f.o:
	$(FC) -c $(F_FLAGS) $<

.F90.o:
	$(FC) -c $(F90_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

.f90.o:
	$(FC) -c $(F90_FLAGS) $<

.c.o:
	$(CC) -c $(C_FLAGS) $<

clean:
	\rm -f $(OBJS) *.mod


# dependencies

module_envvar.o:	get_env_vars.o
ro3_mod_env.o:	module_envvar.o
