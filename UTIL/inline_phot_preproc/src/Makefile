 #APPL = test
 BASE = ../CSQY_TABLE_PROCESSOR

 ifndef (APPL)
    APPL = test
 endif 

 MODEL = $(BASE)_$(APPL)

# COMPILER = INTEL
# COMPILER = PGF90
# COMPILER = GFORT

ifndef COMPILER
  COMPILER = INTEL
# COMPILER = PGF90
#COMPILER = GFORT
endif

 GC_INC   =   /home/hwo/CCTM_git_repository/MECHS/racm2_ae6_aq
 MECH_INC   = $(GC_INC)
 TRAC_INC   = $(GC_INC)
 PROCAN_INC = $(GC_INC)

 INCLUDES = \
 -DSUBST_RXCMMN=\"$(MECH_INC)/RXCM.EXT\" \
 -DSUBST_RXDATA=\"$(MECH_INC)/RXDT.EXT\" 


ifeq ($(COMPILER),INTEL)

#FC = /usr/local/intel/ictce/3.2.2.013/fc/bin/intel64/ifort
#CC = /usr/local/intel/ictce/3.2.2.013/cc/bin/intel64/icc
FC = ifort
CC = icc
F_FLAGS = -fixed -132 -O3 -override-limits -warn nounused -check uninit -check bounds -check format -g -traceback -override-limits -fno-alias -mp1  -I . -g
f_FLAGS = -fixed -132 -O3 -override-limits -warn nounused -check uninit -check bounds -check format -g -traceback -override-limits -fno-alias -mp1  -I . -g
C_FLAGS =  -O2  -DFLDMN=1
#  LINK_FLAGS = $(myLINK_FLAG)
LINK_FLAGS = -i-static

else
# FC = /usr/local/pgi/linux86-64/10.5/bin/pgf90
# CC = /usr/local/pgi/linux86-64/10.5/bin/pgcc
 FC = pgf90
 CC = pgcc
 
# compiler options for subroutines
 F_FLAGS = -Mfixed -Mextend -Mbounds  -Mchkfpstk -Mchkptr -Mchkstk -traceback -Ktrap=fp -O3 -I . -g
 f_FLAGS = -Mfixed -Mextend -Mbounds  -Mchkfpstk -Mchkptr -Mchkstk -traceback -Ktrap=fp -O3 -I . -g
 C_FLAGS =  -O2  -DFLDMN=1
 LINK_FLAGS = -Bstatic  -Bstatic_pgi

 ifeq ($(COMPILER),GFORT)
#   FC    = /usr/local/gcc-4.6/bin/gfortran
   FC    = gfortran
   CC    = gcc
 #FC    = gfortran
 #CC    = gcc
   f_FLAGS       = -ffixed-form -ffixed-line-length-132 -O3 -funroll-loops -I. -fbounds-check
   F_FLAGS       = $(f_FLAGS)
   f90_FLAGS     = -cpp -ffree-form -ffree-line-length-none -O3 -funroll-loops -I. -fbounds-check
   F90_FLAGS     = $(f90_FLAGS)
   C_FLAGS       = -O2 -DFLDMN -I /home/wdx/lib/x86_64/gcc/mpich/include
   LINKER        = $(FC)
   LINK_FLAGS    = 
 endif

endif



 LIBRARIES = \
 -L$(lib_path)/ioapi_3/$(LIOAPI) -lioapi \

 LIBRARIES = 
 
 IOAPI_INC = $(lib_path)/ioapi_3/ioapi/fixed_src
 MPI_INC   = $(lib_path)/mpich/include


# create_module.o \
# wrt_csqy_data.o \

#CHECK_CSQY_DATA.o \

 OBJECTS =\
 BIN_DATA.o \
 CSQY_PARAMETERS.o \
 ALBEDO_REFER_DATA.o \
 CSQY_REFER_DATA.o \
 driver.o \
 intavg_b.o \
 intavg_c.o \
 intavg.o \
 wrt_csqy_data.o \
 wrt_csqy_data_only.o \
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
 
# wrbf12d.o \
# wrbf12d_w_headerb.o \

.SUFFIXES: .F .f .c

$(MODEL): $(OBJECTS)
	$(FC) $(LINK_FLAGS) $(OBJECTS) $(LIBRARIES) -o $@

.F.o:
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

.f.o:
	$(FC) $(F_FLAGS) -c $<


.c.o:
	$(CC) $(C_FLAGS) -c $<

clean:
	rm -f *.o  $(BASE)_* *.mod
 
