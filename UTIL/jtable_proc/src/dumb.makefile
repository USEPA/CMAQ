 BASE = ../CSQY_TABLE_PROCESSOR

 ifndef APPL
    APPL = test
 endif 
 
 MODEL = $(BASE)_$(APPL)


#COMPILER = INTEL
#COMPILER = PGF90
#COMPILER = GFORT


ifndef COMPILER
#COMPILER = INTEL
#COMPILER = PGF90
 COMPILER = GFORT
endif


ifeq ($(COMPILER),INTEL)

#FC = /usr/local/intel/ictce/3.2.2.013/fc/bin/intel64/ifort
#CC = /usr/local/intel/ictce/3.2.2.013/cc/bin/intel64/icc
FC = ifort
CC = icc
F_FLAGS = -fixed -132 -fp-model precise -fpe0 -O3 -override-limits -check uninit -warn nounused -check bounds -check format -g -traceback -override-limits -fno-alias -mp1  -I . -g
f_FLAGS = -fixed -132 -fp-model precise -fpe0 -O3 -override-limits -check uninit -warn nounused -check bounds -check format -g -traceback -override-limits -fno-alias -mp1  -I . -g
F90_FLAGS = -free -132 -fp-model precise -fpe0 -O3 -override-limits -check uninit -warn nounused -check bounds -check format -g -traceback -override-limits -fno-alias -mp1  -I . -g
f90_FLAGS = -free -132 -fp-model precise -fpe0 -O3 -override-limits -check uninit -warn nounused -check bounds -check format -g -traceback -override-limits -fno-alias -mp1  -I . -g
C_FLAGS =  -O2  -DFLDMN=1
#  LINK_FLAGS = $(myLINK_FLAG)
LINK_FLAGS = -i-static

else
# FC = /usr/local/pgi/linux86-64/10.5/bin/pgf90
# CC = /usr/local/pgi/linux86-64/10.5/bin/pgcc
 FC = pgf90
 CC = pgcc
 
# compiler options for subroutines
 F_FLAGS = -Mfixed -Mextend -Mbounds -O3 -traceback  -Mchkfpstk -Mchkptr -Mchkstk -traceback -Ktrap=fp  -I . -g
 f_FLAGS = -Mfixed -Mextend -Mbounds -O3  -traceback -Mchkfpstk -Mchkptr -Mchkstk -traceback -Ktrap=fp -I . -g
 C_FLAGS =  -O2  -DFLDMN=1
 LINK_FLAGS = -Bstatic  -Bstatic_pgi

 ifeq ($(COMPILER),GFORT)
 # FC    = /usr/local/gcc-4.6/bin/gfortran
 # CC    = /usr/bin/gcc
    FC    = gfortran
    CC    = gcc
   f_FLAGS       = -ffixed-form -ffixed-line-length-132 -funroll-loops -O3 -ffpe-trap=invalid,zero -g -finit-character=32 -I. -fbounds-check -fbacktrace
   F_FLAGS       = $(f_FLAGS)
   f90_FLAGS     = -cpp -ffree-form -ffree-line-length-none -funroll-loops -O3 -ffpe-trap=invalid,zero -g -finit-character=32 -I. -fbounds-check -fbacktrace
   F90_FLAGS     = $(f90_FLAGS)
   C_FLAGS       = -O2 -DFLDMN -I /home/wdx/lib/x86_64/gcc/mpich/include
   LINKER        = $(FC)
   LINK_FLAGS    = 
 endif

endif


#GC_INC   =   /home/hwo/CCTM_git_repository/MECHS/racm2_ae6_aq
 MECH_INC   = $(GC_INC)
 TRAC_INC   = $(GC_INC)
 PROCAN_INC = $(GC_INC)

# LIBRARIES = \
# -L$(lib_path)/ioapi_3/$(LIOAPI) -lioapi \

# IOAPI_INC = $(lib_path)/ioapi_3/ioapi/fixed_src
# MPI_INC   = $(lib_path)/mpich/include

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
 xc_qy_td_effect.o \
 convert_case.o \
 nameval.o
else
 OBJECTS =\
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
 convert_case.o \
 nameval.o
endif
 
# wrbf12d.o \
# wrbf12d_w_headerb.o \

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
	rm -f $(OBJECTS) *.o $(BASE)_* *.mod
 
