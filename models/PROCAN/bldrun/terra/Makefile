# RCS file, release, date & time of last delta, author, state, [and locker]
# $Header: /project/yoj/arc/PROCAN/src/bldrun/terra/Makefile,v 1.1 2011/09/13 14:53:41 yoj Exp $

# Makefile for terrae

#MODEL = PACP_SAPRC99
#MODEL = PACP_SAPRC07T
#MODEL = PACP_CB05CL
 MODEL = PACP_CB05TUCL

 IOAPI  = /home/wdx/lib/src/ioapi_3.1/Linux2_x86_64ifort
 NETCDF = /home/wdx/lib/x86_64/ifc11/netcdf/lib

 FC    = ifort
 CC    = icc

#WARN = -warn declarations -warn errors -warn interfaces -warn unused -warn usage
 WARN = -warn declarations -warn errors -warn interfaces              -warn usage
#WARN = -warn declarations -warn errors                               -warn usage
#WARN =

 FSTD = -fixed -extend_source -fno-alias -mp1 $(WARN)
  DBG = -check bounds -check uninit -fpe0 -d-lines -fno-alias $(WARN) -ftrapuv -traceback

 F_FLAGS = $(FSTD) -O3       -I $(IOAPI)/Linux2_x86_64ifort -I.
#F_FLAGS = $(FSTD) -g $(DBG) -I $(IOAPI)/Linux2_x86_64ifort -I.
 F_FLAGS = $(FSTD) -g $(DBG) -I $(IOAPI) -I.
 C_FLAGS = -O2 -DFLDMN -I $(MPICH)/include
 LINK_FLAGS = -i-static

 CPP  = FC

 CPP_FLAGS =

#LIBRARIES = -L$(IOAPI)/Linux2_x86_64ifort -lioapi -L$(NETCDF) -lnetcdf
 LIBRARIES = -L$(IOAPI) -lioapi -L$(NETCDF) -lnetcdf

 INCL1 = /home/yoj/src/cmaqv51/BLD_V5c
#MECH  = cb05cl
 MECH  = cb05tucl

INCLUDES = \
 -DSUBST_RXCMMN=\"$(INCL1)/RXCM_$(MECH).EXT\" \
 -DSUBST_RXDATA=\"$(INCL1)/RXDT_$(MECH).EXT\"

OBJS =\
   CGRID_SPCS.o \
   pa_catstring.o \
   pa_compmech.o \
   pa_docoeff.o \
   pa_driver.o \
   pa_eatcom.o \
   pa_errcheck.o \
   pa_getchr.o \
   pa_getcoef.o \
   pa_getcycle.o \
   pa_getdesc.o \
   pa_getfamily.o \
   pa_getiprout.o \
   pa_getirrout.o \
   pa_getlabel.o \
   pa_getquote.o \
   pa_getreal.o \
   pa_getrxns.o \
   pa_getrxnsum.o \
   pa_getword.o \
   pa_glbldat.o \
   pa_init.o \
   pa_int2chr.o \
   pa_molcloss.o \
   pa_molcprod.o \
   pa_rdline.o \
   pa_read.o \
   pa_report.o \
   pa_setup_ipr.o \
   pa_setup_irr.o \
   pa_trimcoef.o \
   pa_wrtcmnext.o \
   pa_wrtctlext.o \
   pa_wrtdatext.o

.SUFFIXES: .F .f .c

$(MODEL): $(OBJS)
	$(FC) $(LINK_FLAGS) $(OBJS) $(LIBRARIES) -o $@

.F.o:
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

.f.o:
	$(FC) $(F_FLAGS) -c $<

.c.o:
	$(CC) $(C_FLAGS) -c $<

clean:
	/bin/rm -f $(OBJS) *.mod *__genmod.f90

clear:
	/bin/rm -f $(OBJS) *.mod *__genmod.f90 $(MODEL)
