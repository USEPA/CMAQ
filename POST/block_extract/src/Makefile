.SUFFIXES: .o .f .F
MODEL = block_extract
SRCDIR = /home/nmg/postproc/block_extract/src/
#FC     = /usr/local/intel/ictce/3.2.2.013/fc/bin/intel64/ifort
FC     = /usr/local/apps/intel/composer_xe_2013.2.146/bin/intel64/ifort

lib_basedir = /home/wdx/lib
system = x86_64
compiler = intel
lib_path = $(lib_basedir)/$(system)/$(compiler)

INCL   = $(lib_path)/ioapi_3.1/ioapi/fixed_src
FFLAGS = -fixed -132 -O3 -override-limits -fno-alias -mp1 -I$(INCL) -I$(SRCDIR)

LIBS   = -L$(lib_path)/ioapi_3.1/Linux2_x86_64ifort -lioapi \
         -L$(lib_path)/netcdf/lib -lnetcdf -lnetcdff

MODULES = module_envvar.o \
          module_file.o \
          module_grid.o \
          module_tstep.o

OBJS = driver.o \
       ck_ctms.o \
       cksetup.o \
       get_gridval.o \
       get_timout.o \
       process.o \
       output.o

all:
	@$(MAKE) $(MODULES)
	@$(MAKE) $(MODEL)

$(MODEL): $(OBJS)
	$(FC) $(LFLAGS) $(OBJS) $(MODULES) $(LIBS) -o $@

$(OBJS): $(MODULES)

.f.o:
	$(FC) $(FFLAGS) -c $<

.F.o:
	$(FC) $(FFLAGS) $(CPP_FLAGS) -c $<

clean:
	rm -f *.o *.mod $(MODEL)
