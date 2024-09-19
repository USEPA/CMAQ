#------------------------------------------------------------------------------#
#  The Community Multiscale Air Quality (CMAQ) system software is in           #
#  continuous development by various groups and is based on information        #
#  from these groups: Federal Government employees, contractors working        #
#  within a United States Government contract, and non-Federal sources         #
#  including research institutions.  These groups give the Government          #
#  permission to use, prepare derivative works of, and distribute copies       #
#  of their work in the CMAQ system to the public and to permit others         #
#  to do so.  The United States Environmental Protection Agency                #
#  therefore grants similar permission to use the CMAQ system software,        #
#  but users are requested to provide copies of derivative works or            #
#  products designed to operate in the CMAQ system to the United States        #
#  Government without restrictions as to use by others.  Software              #
#  that is used with the CMAQ system but distributed under the GNU             #
#  General Public License or the GNU Lesser General Public License is          #
#  subject to their copyright restrictions.                                    #
#------------------------------------------------------------------------------#

.SUFFIXES:
.SUFFIXES: .o .f90 .F90

MODEL = mcip.exe

#...Portland Group Fortran
###FC     = /usr/local/apps/nvidia/Linux_x86_64/22.11/compilers/bin/nvfortran
###NETCDF = /usr/local/apps/netcdf-4.9.2/nvhpc-22.11
###IOAPI_ROOT = /usr/local/apps/ioapi-3.2/nvhpc-22.11
###FFLAGS = -g -O0 -Ktrap=unf -Ktrap=ovf -Ktrap=divz -Ktrap=inv -Ktrap=fp  \
###         -Ktrap=inexact -pc 32 -Mbounds -Mchkptr -Kieee  \
###         -Minform,inform -Mfree -byteswapio -I$(NETCDF)/include  \
###         -I$(IOAPI_ROOT)/Linux2_x86_64pg
###FFLAGS = -O4 -fastsse -pc 32 -Mfree -byteswapio -I$(NETCDF)/include \
###         -I$(IOAPI_ROOT)/Linux2_x86_64pg
###LIBS   = -L$(IOAPI_ROOT)/Linux2_x86_64pg -lioapi \
###	 -L$(NETCDF)/lib -lnetcdff -lnetcdf

#...gfortran
###FC	= gfortran
###NETCDF = /usr/local/apps/netcdf-4.9.2/gcc-8.5
###IOAPI_ROOT = /usr/local/apps/ioapi-3.2/gcc-8.5
###FFLAGS	= -O3 -I$(NETCDF)/include -I$(IOAPI_ROOT)/Linux2_x86_64
###FFLAGS	= -g -O0  \
###          -ffpe-trap='invalid','zero','overflow','underflow'  \
###          -I$(NETCDF)/include -I$(IOAPI_ROOT)/Linux2_x86_64
###LIBS    = -L$(IOAPI_ROOT)/Linux2_x86_64 -lioapi  \
###          -L$(NETCDF)/lib -lnetcdff -lnetcdf

#...Intel Fortran
FC      = ifort
NETCDF = /usr/local/apps/netcdf-4.9.2/intel-21.4
IOAPI_ROOT = /usr/local/apps/ioapi-3.2/intel-21.4
###FFLAGS  = -g -O0 -check all -C -traceback -FR -I$(NETCDF)/include  \
###          -I$(IOAPI_ROOT)/Linux2_x86_64ifort
FFLAGS  = -O3 -traceback -FR -I$(NETCDF)/include  \
	  -I$(IOAPI_ROOT)/Linux2_x86_64ifort
LIBS    = -L$(IOAPI_ROOT)/lib -lioapi \
	  -L$(NETCDF)/lib -lnetcdff -lnetcdf

DEFS   = 


MODULES =\
 const_mod.o \
 const_pbl_mod.o \
 coord_mod.o \
 ctmvars_mod.o \
 date_time_mod.o \
 date_pack_mod.o \
 files_mod.o \
 lucats_mod.o \
 mcipparm_mod.o \
 metinfo_mod.o \
 metvars_mod.o \
 netcdf_io_mod.o \
 vgrd_mod.o \
 xvars_mod.o
 
OBJS =\
 mcip.o \
 alloc_ctm.o \
 alloc_met.o \
 alloc_x.o \
 bcldprc_ak.o \
 blddesc.o \
 chkwpshdr.o \
 chkwrfhdr.o \
 close_files.o \
 comheader.o \
 comheader_lufrac.o \
 comheader_mos.o \
 comheader_soi.o \
 ctmout.o \
 ctmproc.o \
 dealloc_ctm.o \
 dealloc_met.o \
 dealloc_x.o \
 detangle_soil_px.o \
 e_aerk.o \
 dynflds.o \
 getluse.o \
 getmet.o \
 getpblht.o \
 getpsih.o \
 getsdt.o \
 graceful_stop.o \
 gridout.o \
 gridproc.o \
 init_ctm.o \
 init_io.o \
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
 mapfac_lam.o \
 mapfac_merc.o \
 mapfac_ps.o \
 metgrid2ctm.o \
 metvars2ctm.o \
 outclog.o \
 outcm3io.o \
 outglog.o \
 outgm3io.o \
 outncf.o \
 outncfbdy.o \
 outncfglobal.o \
 pblsup.o \
 ptemp.o \
 pvs.o \
 qsat.o \
 rdwrfem.o \
 readnml.o \
 resistcalc.o \
 setgriddefs.o \
 setup.o \
 setup_wrfem.o \
 sfclayer.o \
 statflds.o \
 vertarys.o \
 vertnhy_wrf.o \
 vstamp.o \
 vtemp.o \
 wind.o \
 wrfemopts.o \
 wrgdesc.o \
 xy2ll_lam.o \
 xy2ll_merc.o

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
