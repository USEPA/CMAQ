SHELL=/bin/sh

  MODEL = dpp.x

# Compiler Definitions
FC = ifort
CC = cc

# Preprocessor
FPP  = $FC

  lib_path   = /home/wdx/lib/x86_64/ifc

  NETCDF_PATH = ${lib_path}/netcdf

  INCLUDES = -I ${NETCDF_PATH}/include -I.

# Compiler Flags
# intel
 ADD_FLAGS  = 
 F_FLAGS    = $(ADD_FLAGS) -O3 -I.
 FR_FLAGS   = $(ADD_FLAGS) -O3 -free -I.
 C_FLAGS    = $(myCFLAGS) -DFLDMN -D__USE_LARGEFILE64
 LINK_FLAGS = $(myLINK_FLAG)

CPP_FLAGS = \
 -D_LARGE_FILES \
 -DFLDMN=1

# Libraries
LIBRARIES = \
  -L$(NETCDF_PATH)/lib -lnetcdf -lnetcdff 

# List of Object files needed for linking
OBJECTS= \
type_def_mod.o \
data_mod.o \
get_env.o \
misc_util.o \
extract_util.o \
io_util.o \
process_util.o \
dpp.o

.SUFFIXES: .F .f .c .f90

$(MODEL): $(OBJECTS)
	$(FC) $(LINK_FLAGS) $(OBJECTS) $(LIBRARIES) -o $@

process_util.o: io_util.o misc_util.o

dpp.o: data_mod.o extract_util.o io_util.o misc_util.o process_util.o type_def_mod.o

extract_util.o: data_mod.o misc_util.o

io_util.o: misc_util.o type_def_mod.o

data_mod.o: type_def_mod.o

misc_util.o: type_def_mod.o

.f90.o:
	$(FC) -c $(FR_FLAGS) $<

.F.o:
	$(FC) -c $(FR_FLAGS) $(INCLUDES) $(CPP_FLAGS) $<

.f.o:
	$(FC) -c $(FR_FLAGS) $(INCLUDES) $(CPP_FLAGS) $<

.c.o:
	$(CC) -c $(INCLUDES) $(C_FLAGS) $(INCLUDES-h) $(CPP_FLAGS) $<

clean:
	-rm -f $(OBJECTS) $(MODEL) *.mod


