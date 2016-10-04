# Set the M3LIB environment variable before running this Makefile 
 
 MODEL   = CALMAP 

 IOAPI   = $(M3LIB)/ioapi
 LIOAPI  = -L$(IOAPI)/lib -lioapi
 LNETCDF = -L$(M3LIB)/netcdf/lib -lnetcdf
 LIBS    = $(LIOAPI) $(LNETCDF) -lcurl
 FC      = ifort 
 FSTD    = -fixed -extend_source -fno-alias -nowarn
 DBG     = -g -check bounds -fpe0 -check uninit -ftrapuv -traceback
 F_FLAGS = $(FSTD) -O3 -mp1 -I. -I$(IOAPI)/include -c
 L_FLAGS = -openmp 
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

