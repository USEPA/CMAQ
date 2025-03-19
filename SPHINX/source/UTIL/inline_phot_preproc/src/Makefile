
ifndef EXEC
  MODEL = CSQY_TABLE_PROCESSOR.EXE
else
  MODEL = $(EXEC)
endif

ifndef compiler
 compiler = gcc   # other options (intel | pgi )
endif
 
 GC_INC     = $(INPDIR) 
 MECH_INC   = $(GC_INC)
 TRAC_INC   = $(GC_INC)
 PROCAN_INC = $(GC_INC)

 INCLUDES = \
 -DSUBST_RXCMMN=\"$(MECH_INC)/RXCM.EXT\" \
 -DSUBST_RXDATA=\"$(MECH_INC)/RXDT.EXT\" 

ifeq ($(compiler),intel)

FC = ifort
CC = icc
F_FLAGS = -fixed -132 -O3 -override-limits -warn nounused -check uninit -check bounds -check format -g -traceback -override-limits -fno-alias -mp1  -I . -g
f_FLAGS = -fixed -132 -O3 -override-limits -warn nounused -check uninit -check bounds -check format -g -traceback -override-limits -fno-alias -mp1  -I . -g
C_FLAGS =  -O2  -DFLDMN=1
LINK_FLAGS = -static-intel

else
 FC = pgf90
 CC = pgcc
 
 # compiler options for subroutines
 F_FLAGS = -Mfixed -Mextend -Mbounds  -Mchkfpstk -Mchkptr -Mchkstk -traceback -Ktrap=fp -O3 -I . -g
 f_FLAGS = -Mfixed -Mextend -Mbounds  -Mchkfpstk -Mchkptr -Mchkstk -traceback -Ktrap=fp -O3 -I . -g
 C_FLAGS =  -O2  -DFLDMN=1
 LINK_FLAGS = -Bstatic  -Bstatic_pgi

 ifeq ($(compiler),gcc)
   FC    = gfortran
   CC    = gcc

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
 
