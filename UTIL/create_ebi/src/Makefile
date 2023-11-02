 
ifndef EXEC
  MODEL = CREATE_EBI.EXE
else
  MODEL = $(EXEC)
endif

ifndef compiler
 compiler = intel   # other options (gcc | pgi )
endif
 
ifeq ($(compiler),intel)

  FC = ifort
  CC = icc
  FPP = $(FC)

  F_FLAGS    = -fixed -132 -O0 -fno-alias -mp1 -fpe0  -I.  -g -CB -CU -traceback
  F90_FLAGS  = -free -O0 -fno-alias -mp1  -I. -fpe0 -g -CB -CU -traceback
  f_FLAGS    = -fixed -132 -O0 -fno-alias -mp1 -fpe0  -I.  -g -CB -CU -traceback
  f90_FLAGS  = -free -O0 -fno-alias -mp1  -I. -fpe0 -g -CB -CU -traceback
  C_FLAGS    = -O2  -DFLDMN
 
  CPP = $(FPP)
  CPP_FLAGS  =  
 
  LINK_FLAGS = -static-intel

else ifeq ($(compiler),pgi)

  FC = pgf90
  CC = pgcc
 
  F_FLAGS   = -Mfixed -Mextend -Mbounds  -Mchkptr -Mchkstk -traceback -Ktrap=fp -O0 -I . -g
  F90_FLAGS = -Mfree -Mbounds  -Mchkptr -Mchkstk -traceback -Ktrap=fp -O0 -I . -g
  f90_FLAGS = -Mfree -Mbounds  -Mchkptr -Mchkstk -traceback -Ktrap=fp -O0 -I . -g
  f_FLAGS   = -Mfixed -Mextend -Mbounds  -Mchkfpstk -Mchkptr -Mchkstk -traceback -Ktrap=fp -O0 -I . -g
  C_FLAGS   = -O2  -DFLDMN=1
  LINK_FLAGS =
# LINK_FLAGS = -Bstatic
# LINK_FLAGS = -Bstatic  -Bstatic_pgi

else ifeq ($(compiler),nvhpc)

  FC = nvfortran
  CC = nvc
 
  F_FLAGS   = -Mfixed -Mextend -Mbounds  -Mchkptr -Mchkstk -traceback -Ktrap=fp -O0 -I . -g
  F90_FLAGS = -Mfree -Mbounds  -Mchkptr -Mchkstk -traceback -Ktrap=fp -O0 -I . -g
  f90_FLAGS = -Mfree -Mbounds  -Mchkptr -Mchkstk -traceback -Ktrap=fp -O0 -I . -g
  f_FLAGS   = -Mfixed -Mextend -Mbounds  -Mchkfpstk -Mchkptr -Mchkstk -traceback -Ktrap=fp -O0 -I . -g
  C_FLAGS   = -O2  -DFLDMN=1
# LINK_FLAGS = -Bstatic  -Bstatic-nvidia

else ifeq ($(compiler),gcc)

   FC    = gfortran
   CC    = gcc
   f_FLAGS       = -ffixed-form -ffixed-line-length-132 -O0 -funroll-loops -I. -fcheck=all -fbounds-check
   F_FLAGS       = $(f_FLAGS)
   f90_FLAGS     = -cpp -ffree-form -ffree-line-length-none -O0 -funroll-loops -I. -fcheck=all -fbounds-check
   F90_FLAGS     = $(f90_FLAGS)
   C_FLAGS       = -O2 -DFLDMN 
   LINKER        = $(FC)
   LINK_FLAGS    = 

else
 ERROR1 = "Makefile not configured to support the specified compiler, $(compiler). USER MUST USE INTEL, PGF90 or GFORT options or MODIFY makefile.v50XX."
endif

ifdef ERROR1
 $(error $(ERROR1))
endif

LIBS = 

 LOAD     = $(FC) $(F_FLAGS) 
 LOAD_F90 = $(FC) $(F90_FLAGS) 
 LDFLAGS = $(LINK_FLAGS)

 LOAD     = $(FC) $(F_FLAGS) 
 LOAD_F90 = $(FC) $(F90_FLAGS) 
 LDFLAGS = $(LINK_FLAGS)


OBJS = module_env_functions.o \
       module_envvar.o \
       module_glbldata.o \
       RXNS_DATA_MODULE.o \
       junit.o \
       driver.o \
       set_glbldata.o \
       util_subs.o \
       err_check.o \
       wr_driver.o \
       wr_calcks.o \
       wr_solver.o \
       wr_init.o \
       wr_data.o \
       wr_gp1.o \
       wr_gp2.o \
       wr_gp3.o \
       wr_gp4.o \
       wr_prodloss.o \
       wr_rates.o \
       wr_ssa.o \
       wr_special.o \
       wr_copyrt.o \
       wr_cvshdr.o \
 

#
$(MODEL): $(OBJS) 
	$(LOAD) $(LDFLAGS) -o $(MODEL) $(OBJS) $(LIBS)

RXNS_DATA_MODULE.o: $(RXNS_DATA_SRC)
	$(LOAD_F90) -c $(INCLUDES) $(RXNS_DATA_SRC)

module_env_functions.o:  $(SRCDIR)/module_env_functions.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/module_env_functions.F

module_envvar.o:  $(SRCDIR)/module_envvar.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/module_envvar.F

module_glbldata.o:  $(SRCDIR)/module_glbldata.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/module_glbldata.F

driver.o:  $(SRCDIR)/driver.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/driver.F

set_glbldata.o:  $(SRCDIR)/set_glbldata.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/set_glbldata.F

junit.o:  $(SRCDIR)/junit.F
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/junit.F

name_index.o:  $(SRCDIR)/name_index.f
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/name_index.f

util_subs.o:  $(SRCDIR)/util_subs.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/util_subs.F

err_check.o:  $(SRCDIR)/err_check.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/err_check.F

wr_driver.o:  $(SRCDIR)/wr_driver.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/wr_driver.F

wr_calcks.o:  $(SRCDIR)/wr_calcks.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/wr_calcks.F

wr_solver.o:  $(SRCDIR)/wr_solver.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/wr_solver.F

wr_data.o:  $(SRCDIR)/wr_data.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/wr_data.F

wr_init.o:  $(SRCDIR)/wr_init.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/wr_init.F

wr_gp1.o:  $(SRCDIR)/wr_gp1.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/wr_gp1.F

wr_gp2.o:  $(SRCDIR)/wr_gp2.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/wr_gp2.F

wr_gp3.o:  $(SRCDIR)/wr_gp3.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/wr_gp3.F

wr_gp4.o:  $(SRCDIR)/wr_gp4.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/wr_gp4.F

wr_prodloss.o:  $(SRCDIR)/wr_prodloss.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/wr_prodloss.F

wr_rates.o:  $(SRCDIR)/wr_rates.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/wr_rates.F

wr_special.o:  $(SRCDIR)/wr_special.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/wr_special.F

wr_ssa.o:  $(SRCDIR)/wr_ssa.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/wr_ssa.F

wr_copyrt.o:  $(SRCDIR)/wr_copyrt.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/wr_copyrt.F

wr_cvshdr.o:  $(SRCDIR)/wr_cvshdr.F 
	$(LOAD) -c $(INCLUDES) $(SRCDIR)/wr_cvshdr.F

