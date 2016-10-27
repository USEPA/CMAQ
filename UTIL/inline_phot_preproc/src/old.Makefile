#MODEL = CSQY_TABLETABLE_PROCESSOR_cb05cl-si
#MODEL = JTABLE_PROCESSOR_s07tb-smodel
#APPL = AE-REFRACT
 MODEL = ../CSQY_TABLE_PROCESSOR_$(APPL)

#FC    = /share/linux9.0/pgi/linux86/6.0/bin/pgf90
#CC    = cc
#CPLUS = C++
#FPP   = /share/linux9.0/pgi/linux86/6.0/bin/pgf90

#F_FLAGS    = -Mfixed -Mextend -O2 -module /home/hutzellb/cmaq_toxics/releases_2010/MOD_DIR-v47-par -I. 
#C_FLAGS    = -v -O2 -I/share/linux9.0/mpich-1.2.7p1/include
#LINK_FLAGS = -Bstatic


FC    = $(myFC)
CC    = $(myCC)
FPP   = $FC

 lib_path  = $(lib_basedir)/${system}/${compiler}

 DEGUB = -CB -CU -traceback
#DEBUG = -Mbounds -Mchkptr -Mchkstk
#DEBUG = 

#F_FLAGS    = $(myFFLAGS) $(DEBUG) -I $(lib_path)/se_snl -I $(lib_path)/mpich/include -I .
# F_FLAGS    = $(myFFLAGS) -CB -CU -traceback  -I $(lib_path)/se_snl -I $(lib_path)/mpich/include -I .
 F_FLAGS    = $(myFFLAGS) -CB -CU -traceback  -I .
# C_FLAGS    = $(myCFLAGS) -I $(MPI_INC)
 C_FLAGS    = $(myCFLAGS) 
 LINK_FLAGS = $(myLINK_FLAG)

 CPP_FLAGS  =    

# x86 pgf90
#SE_SNL = -L/home/hutzellb/cmaq_toxics/tools/stenex_v4.6/Linux -lse_snl 
#PARIO = -L/home/hutzellb/cmaq_toxics/tools/pario-v4.6/Linux -lpario  
#MPICH = -L/share/linux9.0/mpich-1.2.7p1/lib -lmpich 
#IOAPI = -L/home/hutzellb/cmaq_toxics/tools/ioapi-3.1/pg -lioapi 
#NETCDF = -L/share/linux9.0/netcdf-3.6.0/lib  -lnetcdf
#LIBRARIES = $(SE_SNL) $(PARIO) $(MPICH) $(IOAPI) $(NETCDF)


 BASE_INC = /project/work/rep/include/release
#IOAPI_INC = /home/hutzellb/cmaq_toxics/tools/ioapi-3.1/fixed_src
# GC_INC   = /home/hutzellb/cmaq_toxics/tools/mech_processor/saprc99tx3_ae5_aq
#GC_INC   = /home/hutzellb/cmaq_toxics/tools/mech_processor/SAPRC07TB_AE5_AQ-Jan_2010_Not_SS-no_NO2EX
#GC_INC   =  /home/hutzellb/cmaq_toxics/tools/mech_processor/SAPRC07TC_AE5_AQ-Jan_2010_Not_SS-no_NO2EX
#GC_INC   =  /home/hutzellb/cmaq_toxics/releases_2010/new_arc_mechanisms/saprc99tx3_ae5_aq
#GC_INC   =  /home/hutzellb/cmaq_toxics/releases_2010/new_arc_mechanisms/saprc07tb_ae5_aq
#GC_INC   =  /home/hutzellb/cmaq_toxics/releases_2010/new_arc_mechanisms/cb05cl_ae5_aq
#GC_INC   =  
 MECH_INC   = $(GC_INC)
 TRAC_INC   = $(GC_INC)
 PROCAN_INC = $(GC_INC)
#MPI_INC = /share/linux9.0/mpich-1.2.7p1/include

 LIBRARIES = \
 -L$(lib_path)/ioapi_3/$(LIOAPI) -lioapi \

 IOAPI_INC = $(lib_path)/ioapi_3/ioapi/fixed_src
 MPI_INC   = $(lib_path)/mpich/include

 INCLUDES = \
 -DSUBST_RXCMMN=\"$(MECH_INC)/RXCM.EXT\" \
 -DSUBST_RXDATA=\"$(MECH_INC)/RXDT.EXT\" 

# create_module.o \
# wrt_csqy_data.o \

 OBJECTS =\
 BIN_DATA.o \
 JPROC_PROFILE_nlcd-2006.o \
 JPROC_PHOT_DATA.o \
 driver.o \
 intavg_b.o \
 intavg_c.o \
 intavg.o \
 wrt_csqy_data.o \
 optics_water_cloud.o \
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
	rm -f $(OBJECTS) $(MODEL)
 
