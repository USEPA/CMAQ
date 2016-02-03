.SUFFIXES:
.SUFFIXES: .o .f90 .F90

MODEL = mcip.exe
NETCDF = /usr/local/netcdf-3.6.2
IOAPI_ROOT = /vortex/home/hhg/ioapi/ioapi-3.0

## Usage: Uncomment the settings for your system/compiler below and type "make" to build MCIP

#...IBM
#FC     = /usr/bin/xlf90
#FFLAGS = -qfree -O3 -qstrict -bmaxstack:512000000 -bmaxdata:2000000000 -I$(NETCDF)/include -I$(IOAPI_ROOT)/ioapi
#FFLAGS = -qfree -O0 -g -qcheck -qextchk -qflttrap=ov:und:zero:inv:imp:en -qsigtrap -qinitauto=FF911299 -bmaxstack:512000000 -bmaxdata:2000000000 -I$(NETCDF)/include -I$(IOAPI_ROOT)/ioapi
#LIBS   = -L$(IOAPI_ROOT)/AIX -lioapi \
#	 -L$(NETCDF)/lib -lnetcdf
#
#...Linux (PGF90)
#FC     = /share/linux9.0/pgi/linux86/6.2/bin/pgf90
#FFLAGS = -g -O0 -Ktrap=unf -Ktrap=denorm -Ktrap=inv -Ktrap=ovf -Ktrap=divz -Ktrap=fp -pc 32 -tp k8-32 -Mbounds -Mchkfpstk -Mchkptr -Kieee -Minform,inform -Mfree -byteswapio -I$(NETCDF)/include -I$(IOAPI_ROOT)/ioapi
#FFLAGS = -O4 -fastsse -pc 32 -Mfree -byteswapio -I$(NETCDF)/include -I$(IOAPI_ROOT)/ioapi
#LIBS   = -L$(IOAPI_ROOT)/Linux2_x86pg_pgcc -lioapi \
#	 -L$(NETCDF)/lib -lnetcdf
#
#...Linux (Intel)
#FC     = /share/linux9.0/intel/fc/9.0/bin/ifort
#FFLAGS = -g -O0 -check all -C -pc32 -traceback -FR -openmp -I$(NETCDF)/include -I$(IOAPI_ROOT)/ioapi
#FFLAGS = -O3 -FR -vec-report0 -openmp -I$(NETCDF)/include -I$(IOAPI_ROOT)/ioapi
#LIBS   = -L$(IOAPI_ROOT)/Linux2_x86ifort -lioapi \
#         -L$(NETCDF)/lib -lnetcdf
#
#...Linux (Intel 64)
#FC	= /opt/intel/fce/9.0/bin/ifort
#FFLAGS	= -g -O0 -check all -C -pc32 -traceback -FR -openmp -I$(NETCDF)/include -I$(IOAPI_ROOT)/ioapi
#FFLAGS	= -O3 -FR -vec-report0 -openmp -I$(NETCDF)/include -I$(IOAPI_ROOT)/ioapi
#LIBS	= -L$(IOAPI_ROOT)/Linux2_x86_64ifort -lioapi \
#	  -L$(NETCDF)/lib -lnetcdf
#
#...SGI Altix (using Intel Fortran)
#FC      = ifort
#FFLAGS = -g -O0 -check all -C -traceback -FR -I$(NETCDF)/include -I$(IOAPI_ROOT)/ioapi
#FFLAGS  = -FR -O3 -I$(NETCDF)/include -I$(IOAPI_ROOT)/ioapi
#LIBS    = -L$(IOAPI_ROOT)/Linux2_ia64ifortdbg -lioapi \
#          -L$(NETCDF)/lib -lnetcdf
#LIBS    = -L$(IOAPI_ROOT)/Linux2_ia64ifort -lioapi \
#          -L$(NETCDF)/lib -lnetcdf
#
#...gfortran
#FC	= gfortran
#FFLAGS	= -O3 -I$(NETCDF)/include -I$(IOAPI_ROOT)/include/ioapi
#LIBS    = -L$(IOAPI_ROOT)/Linux2_x86_64gfort -lioapi \
#          -L$(NETCDF)/lib -lnetcdf

DEFS   = 


MODULES =\
 const_mod.o \
 const_pbl_mod.o \
 coord_mod.o \
 date_pack_mod.o \
 date_time_mod.o \
 depvvars_mod.o \
 parms3_mod.o \
 fdesc3_mod.o \
 file_mod.o \
 iodecl3_mod.o \
 groutcom_mod.o \
 luvars_mod.o \
 mcipparm_mod.o \
 mcoutcom_mod.o \
 mdoutcom_mod.o \
 metinfo_mod.o \
 metvars_mod.o \
 vgrd_mod.o \
 wrf_netcdf_mod.o \
 xvars_mod.o \
 sat2mcip_mod.o
 
OBJS =\
 mcip.o \
 alloc_ctm.o \
 alloc_depv.o \
 alloc_lu.o \
 alloc_met.o \
 alloc_x.o \
 bcldprc_ak.o \
 blddesc.o \
 chkwpshdr.o \
 chkwrfhdr.o \
 close_files.o \
 collapx.o \
 comheader.o \
 cori.o \
 dealloc_ctm.o \
 dealloc_depv.o \
 dealloc_lu.o \
 dealloc_met.o \
 dealloc_x.o \
 dynflds.o \
 getgist.o \
 getluse.o \
 getmet.o \
 getpblht.o \
 getsdt.o \
 getversion.o \
 graceful_stop.o \
 gridout.o \
 hlconst.o \
 init_depv.o \
 init_io.o \
 init_lu.o \
 init_met.o \
 init_x.o \
 julian.o \
 layht.o \
 ll2xy_lam.o \
 ll2xy_lam_sec.o \
 ll2xy_lam_tan.o \
 ll2xy_merc.o \
 ll2xy_ps.o \
 locate.o \
 m3dry.o \
 mapfac_lam.o \
 mapfac_merc.o \
 mapfac_ps.o \
 metcro.o \
 metdot.o \
 metgrid2ctm.o \
 metvars2ctm.o \
 mm5v3opts.o \
 pblsup.o \
 pvs.o \
 rdmm5v3.o \
 rdwrfem.o \
 readnml.o \
 readter.o \
 refstate.o \
 resistcalc.o \
 setgriddefs.o \
 setup.o \
 setup_mm5v3.o \
 setup_wrfem.o \
 sfclayer.o \
 statflds.o \
 vertarys.o \
 vertnhy.o \
 vertnhy_wrf.o \
 vstamp.o \
 wind.o \
 wrfemopts.o \
 wrgdesc.o \
 xy2ll_lam.o \
 xy2ll_merc.o \
 xy2ll_ps.o

all:
	@$(MAKE) $(MODULES)
	@$(MAKE) $(MODEL)

$(MODEL): $(OBJS)
	$(FC) -o $(MODEL) $(FFLAGS) $(OBJS) $(MODULES) $(LIBS)

$(OBJS): $(MODULES)

.f90.o:
	$(FC) $(FFLAGS) $(INCLUDES) -c $<

.F90.o:
	$(FC) $(FFLAGS) $(INCLUDES) -c $< $(DEFS)

clean:
	rm -f *.o *.mod *.il $(MODEL)
