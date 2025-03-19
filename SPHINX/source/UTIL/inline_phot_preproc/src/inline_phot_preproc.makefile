
ifndef EXEC
  MODEL = CSQY_TABLE_PROCESSOR.EXE
else
  MODEL = $(EXEC)
endif

ifndef compiler
 compiler = gcc   # other options (intel | pgi )
endif
 
ifeq ($(compiler),intel)

FC = ifort
CC = icc
F_FLAGS = -fixed -132 -fp-model source -fpe0 -O0 -check uninit -warn nounused -check bounds -check format -g -traceback -fno-alias -mp1  -I . -g
f_FLAGS = -fixed -132 -fp-model source -fpe0 -O0 -check uninit -warn nounused -check bounds -check format -g -traceback -fno-alias -mp1  -I . -g
F90_FLAGS = -free -fp-model source -fpe0 -O0 -check uninit -warn nounused -check bounds -check format -g -traceback -fno-alias -mp1  -I . -g
f90_FLAGS = -free -fp-model source -fpe0 -O0 -check uninit -warn nounused -check bounds -check format -g -traceback -fno-alias -mp1  -I . -g

C_FLAGS =  -O2  -DFLDMN=1
LINK_FLAGS = 

else ifeq ($(compiler),pgi)

 FC = pgf90
 CC = pgcc
 F_FLAGS   = -Mfixed -Mextend -Mbounds -O0 -traceback -Mchkptr -Mchkstk -traceback -Ktrap=fp -I . -g
 f_FLAGS   = -Mfixed -Mextend -Mbounds -O0 -traceback -Mchkptr -Mchkstk -traceback -Ktrap=fp -I . -g
 f90_FLAGS = -Mfree  -Mextend -Mbounds -O0 -traceback -Mchkptr -Mchkstk -traceback -Ktrap=fp -I . -g
 F90_FLAGS = -Mfree  -Mextend -Mbounds -O0 -traceback -Mchkptr -Mchkstk -traceback -Ktrap=fp -I . -g
 C_FLAGS =  -O2  -DFLDMN=1
 LINK_FLAGS =
#LINK_FLAGS = -Bstatic
#LINK_FLAGS = -Bstatic  -Bstatic_pgi

else ifeq ($(compiler),nvhpc)

 FC = nvfortran
 CC = nvc
 F_FLAGS   = -Mfixed -Mextend -Mbounds -O0 -traceback -Mchkptr -Mchkstk -traceback -Ktrap=fp -I . -g
 f_FLAGS   = -Mfixed -Mextend -Mbounds -O0 -traceback -Mchkptr -Mchkstk -traceback -Ktrap=fp -I . -g
 f90_FLAGS = -Mfree  -Mextend -Mbounds -O0 -traceback -Mchkptr -Mchkstk -traceback -Ktrap=fp -I . -g
 F90_FLAGS = -Mfree  -Mextend -Mbounds -O0 -traceback -Mchkptr -Mchkstk -traceback -Ktrap=fp -I . -g
 C_FLAGS =  -O2  -DFLDMN=1
 LINK_FLAGS =
#LINK_FLAGS = -Bstatic  -Bstatic_pgi

else ifeq ($(compiler),gcc)
 FC    = gfortran
 CC    = gcc
 f_FLAGS   = -ffixed-form -ffixed-line-length-132 -funroll-loops -O0 -ffpe-trap=invalid,zero -g -finit-character=32 -I. -fcheck=all -fbounds-check
 F_FLAGS   = $(f_FLAGS)
 f90_FLAGS = -cpp -ffree-form -ffree-line-length-none -funroll-loops -O0 -ffpe-trap=invalid,zero -g -finit-character=32 -I. -fcheck=all -fbounds-check
 F90_FLAGS = $(f90_FLAGS)
 C_FLAGS   = -O2 -DFLDMN -I /home/wdx/lib/x86_64/gcc/mpich/include
 LINKER    = $(FC)
 LINK_FLAGS = 

else
 ERROR1 = "Makefile not configured to support the specified compiler, $(compiler). User must modify Makefile."
endif

ifdef ERROR1
 $(error $(ERROR1))
endif

ifdef INPDIR
 MECH_INC   = $(INPDIR)
else
 ERROR2 = "BuildRun script error: Input directory containing RXNS_DATA_MODULE.F90 not defined"
endif

ifdef ERROR2
 $(error $(ERROR2))
endif

LIBRARIES = 


ifndef USE_RXNS_MODULES
 INCLUDES = \
 -Dverbose_phot -Dmech_includes \
 -DSUBST_RXCMMN=\"$(MECH_INC)/RXCM.EXT\" \
 -DSUBST_RXDATA=\"$(MECH_INC)/RXDT.EXT\" 
else
 INCLUDES  = -Dverbose_phot
endif


#CHECK_CSQY_DATA.o \

ifndef USE_RXNS_MODULES
 OBJECTS =\
 module_envvar.o \
 BIN_DATA.o \
 CSQY_PARAMETERS.o \
 ALBEDO_REFER_DATA.o \
 CSQY_REFER_DATA.o \
 driver.o \
 intavg_b.o \
 intavg_c.o \
 intavg.o \
 wrt_csqy_data_only.o \
 wrt_csqy_data.o \
 wrt_optics_data.o \
 optics_water_cloud.o \
 optics_ice_cloud.o \
 aero_refract_index.o \
 process_csqys.o \
 spectral_reflect.o \
 wvbin_average-b.o \
 wvbin_average.o \
 xc_qy_td_effect_v3.o \
 convert_case.o 
else
 OBJECTS =\
 module_envvar.o \
 BIN_DATA.o \
 RXNS_DATA_MODULE.o \
 CSQY_PARAMETERS.o \
 ALBEDO_REFER_DATA.o \
 CSQY_REFER_DATA.o \
 driver.o \
 intavg_b.o \
 intavg_c.o \
 intavg.o \
 wrt_csqy_data_only.o \
 wrt_csqy_data.o \
 wrt_optics_data.o \
 optics_water_cloud.o \
 optics_ice_cloud.o \
 aero_refract_index.o \
 process_csqys.o \
 spectral_reflect.o \
 wvbin_average-b.o \
 wvbin_average.o \
 xc_qy_td_effect_v3.o \
 convert_case.o 
endif


.SUFFIXES: .F .f .c

$(MODEL): $(OBJECTS)
	$(FC) $(LINK_FLAGS) $(OBJECTS) $(LIBRARIES) -o $@

.F.o:
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

.f.o:
	$(FC) $(F_FLAGS) -c $<

.F90.o:
	$(FC) -c $(F90_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<


 RXNS_DATA_MODULE.o: $(MECH_INC)/RXNS_DATA_MODULE.F90
	$(FC) -c $(F90_FLAGS) $(CPP_FLAGS) $(INCLUDES) $(MECH_INC)/RXNS_DATA_MODULE.F90

.f90.o:
	$(FC) -c $(f90_FLAGS) $<

.c.o:
	$(CC) $(C_FLAGS) -c $<

clean:
	rm -f $(OBJECTS) *.o $(MODEL)_* *.mod
