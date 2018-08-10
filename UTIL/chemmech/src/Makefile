# original file copied from /home/yoj/src/mech/BLD_2, 1/31/2008
# need to source scom command for compilers

  MODEL = CHEMMECH

ifndef COMPILER
  COMPILER = INTEL
# COMPILER = PGF90
#COMPILER = GFORT
endif

ifeq ($(COMPILER),INTEL)

FC = ifort
CC = icc
C_FLAGS =  -O2  -DFLDMN=1
#F_FLAGS = -fixed -132 -O3 -qoverride-limits -check uninit -check bounds -check format -g -traceback -warn nounused -fno-alias -mp1  -I . -g
F_FLAGS = -fixed -132 -O0 -fpe0 -check uninit -check bounds -check format -g -traceback -warn nounused -fno-alias -mp1  -I . -g
LINK_FLAGS = 

else
 FC = pgf90
 CC = pgcc
 F_FLAGS = -Mfixed -Mextend -Mbounds  -Mchkfpstk -Mchkptr -Mchkstk -traceback -Ktrap=fp -O0 -I . -g
 f_FLAGS = -Mfixed -Mextend -Mbounds  -Mchkfpstk -Mchkptr -Mchkstk -traceback -Ktrap=fp -O0 -I . -g
 C_FLAGS =  -O2  -DFLDMN=1
 LINK_FLAGS = 

 ifeq ($(COMPILER),GFORT)
   FC    = gfortran
   CC    = gcc
   f_FLAGS       = -ffixed-form -ffixed-line-length-132 -O0 -funroll-loops -finit-character=32 -fcheck=all -I. 
   f_FLAGS       = -ffixed-form -ffixed-line-length-132 -O0 -funroll-loops -finit-character=32 -I. 
   F_FLAGS       = $(f_FLAGS)
   f90_FLAGS     = -cpp -ffree-form -ffree-line-length-none -O0 -funroll-loops -finit-character=32 -fcheck=all -I. 
   f90_FLAGS     = -cpp -ffree-form -ffree-line-length-none -O0 -funroll-loops -finit-character=32 -I. 
   F90_FLAGS     = $(f90_FLAGS)
   C_FLAGS       = -O2 -DFLDMN -I /home/wdx/lib/x86_64/gcc/mpich/include
   LINKER        = $(FC)
   LINK_FLAGS    = 
 endif

endif
 

OBJECTS =\
    MECHANISM_PARMS.o \
    MECHANISM_DATA.o \
    KPP_DATA.o \
    CGRID_SPCS.o \
    chemmech.o \
    eatcom.o \
    check_ss_spc.o \
    check_order_specials.o \
    getchar.o \
    getlabel.o \
    get_operator.o \
    getprdct.o \
    getrate.o \
    getrctnt.o \
    getreal.o \
    getword.o \
    get_ss_data.o \
    junit.o \
    index1.o \
    indexes.o \
    lkupspec.o \
    rdline.o \
    size.o \
    wrexts.o \
    wrhdr1.o \
    wrbf6.o \
    wrbf16c.o \
    wrbf12d.o \
    wrbf12s.o \
    wrspecial_ext.o \
    wrss_ext.o  \
    wrt_kpp_inputs.o \
    wrt_reactions_module.o \
    wrbf6_fortran90.o \
    wrbf16c_fortran90.o \
    wrbf12d_fortran90.o \
    wrbf12s_fortran90.o \
    wrspecial_ext_fortran90.o \
    wrexts_fortran90.o \
    wrss_ext_fortran90.o  \
    vallabchr.o \
    valwrdchr.o \
    nameval.o
        
.SUFFIXES: .f .c

$(MODEL): $(OBJECTS)
	$(FC) $(LINK_FLAGS) $(OBJECTS) -o $@

.f.o:
	$(FC) $(F_FLAGS) -c $<

.F.o:
	$(FC) $(F_FLAGS) -c $<

.c.o:
	$(CC) $(C_FLAGS) -c $<

clean:
	\rm -f *.o *.mod
