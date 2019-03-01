# make file to build combine program

PROGRAM = ro3_mod_env

FC = ifort

WDX_LIB = /home/bhutzell/tools
FC_FLAGS = -fixed -O2 -132 -I. -CB -CU -c -override-limits -g -traceback -fno-alias -mp1 -fp-model source -I $(WDX_LIB)/ioapi_3.1/Linux2_x86_64ifort
#FC_FLAGS = -fixed -O3 -132 -I. -c -override-limits -fno-alias -mp1 -fp-model source -I $(WDX_LIB)/ioapi_3.1/Linux2_x86_64ifort

netcdf = intel_netcdf
NETCDF = netcdf


LIBRARIES = -L  $(WDX_LIB)/ioapi_3.1/Linux2_x86_64ifort  -lioapi \
            -L $(WDX_LIB)/$(netcdf)/lib  -l$(NETCDF)

OBJS = get_env_vars.o module_envvar.o \
create_ioapi_OMI.o ro3_mod_env.o create_CMAQ_OMI.o  viz_cmaqv51_o3totcol.o create_extend_OMI.o 


$(PROGRAM):  $(OBJS)
	$(FC) $(OBJS) $(LIBRARIES) -o $(PROGRAM)

.f.o:
	$(FC) $(FC_FLAGS) $<

.F.o:
	$(FC) $(FC_FLAGS) $<


clean:
	\rm -f *.mod *.o $(PROGRAM)
