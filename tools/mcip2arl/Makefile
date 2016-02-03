# makefile to build the MCIP2ARL program

# RCS file, release, date & time of last delta, author, state, [and locker]
# $Header: /nas01/depts/ie/cempd/apps/CMAQ/v5.0.1/CMAQv5.0.1/models/TOOLS/src/mcip2arl/Makefile,v 1.1.1.1 2012/04/19 19:48:37 sjr Exp $ 

PROGRAM = MCIP2ARL

INC     = /share/linux/ioapi30_pg/include/free

FC = pgf90
FC_FLAGS = -c -tp px -Mfree -Mextend -Mbounds -I$(INC)

LIBRARIES = -L /share/linux/ioapi30_pg/lib -lioapi \
            -L /share/linux/netcdf_3.5/lib -lnetcdf 

OBJS=setgrid.o \
 crs2dot.o \
 mcip2arl.o \
 pakset.o \
 pakrec.o \
 pakndx.o \
 pakini.o \
 pakout.o \
 stlmbr.o \
 stcm2p.o \
 cxy2ll.o \
 cspanf.o \
 cnllxy.o \
 cll2xy.o \
 cnxyll.o \
 eqvlat.o

$(PROGRAM):  $(OBJS)
	$(FC) $(OBJS) $(LIBRARIES) -o $(PROGRAM)

.f.o:
	$(FC) $(FC_FLAGS) $<
