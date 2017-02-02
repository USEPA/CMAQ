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

# Requirements: source config.cmaq before running this script

.SUFFIXES:
.SUFFIXES: .o .f90 .F90

MODEL = mcip.exe

FC     = ${myFC}
FFLAGS = $(myFRFLAGS) -I$(NETCDF_DIR)/include -I$(IOAPI_DIR)/include
LIBS   = -L$(IOAPI_DIR)/lib $(ioapi_lib) \
	 -L$(NETCDF_DIR)/lib $(netcdf_lib) $(extra_lib) 

DEFS   = 


MODULES =\
 const_mod.o \
 const_pbl_mod.o \
 coord_mod.o \
 date_time_mod.o \
 date_pack_mod.o \
 files_mod.o \
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
 dealloc_met.o \
 dealloc_x.o \
 detangle_soil_px.o \
 e_aerk.o \
 dynflds.o \
 getgist.o \
 getluse.o \
 getmet.o \
 getpblht.o \
 getsdt.o \
 getversion.o \
 graceful_stop.o \
 gridout.o \
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
 metcro.o \
 metdot.o \
 metgrid2ctm.o \
 metvars2ctm.o \
 mm5v3opts.o \
 pblsup.o \
 ptemp.o \
 pvs.o \
 qsat.o \
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
