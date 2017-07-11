# Source the config.cmaq script before running this Makefile 
 
 MODEL   = CALMAP 

 IOAPI   = $(CMAQ_LIB)/ioapi
 LIOAPI  = -L$(IOAPI)/lib $(ioapi_lib)
 LNETCDF = -L$(CMAQ_LIB)/netcdf/lib $(netcdf_lib)
 LIBS    = $(LIOAPI) $(LNETCDF) $(extra_lib) 
 FC      = $(myFC) 
 FSTD    = $(myFSTD) $(myFFLAGS)
 DBG     = $(myDBG)
 F_FLAGS = $(FSTD) -I. -I$(IOAPI)/include -c
 L_FLAGS = $(myLINK_FLAG)
 CPP_FLAGS = 
 INCLUDES = 

OBJS = \
 calmap.o \
 states.o

.SUFFIXES: .F .f .c

$(MODEL): $(OBJS)
	$(FC) $(L_FLAGS) $(OBJS) $(LIBS) -o $@

.F.o:
	$(FC) $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

.f.o:
	$(FC) $(F_FLAGS) -c $<

.c.o:
	$(CC) $(C_FLAGS) -c $<

clean:
	/bin/rm -f $(OBJS)

