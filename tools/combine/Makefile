# make file to build combine program

# RCS file, release, date & time of last delta, author, state, [and locker]
# $Header: /nas01/depts/ie/cempd/apps/CMAQ/v5.0.1/CMAQv5.0.1/models/TOOLS/src/combine/Makefile,v 1.1.1.1 2012/04/19 19:48:37 sjr Exp $

PROGRAM = combine.exe


FC = ifort
FC_FLAGS = -c -FI -O2 -132 -WB -I$(M3LIB)/ioapi/include

LIBRARIES = -L $(M3LIB)/ioapi/lib -lioapi \
            -L $(M3LIB)/netcdf/lib -lnetcdf


OBJS = \
module_specdef.o \
module_file.o \
module_evaluator.o \
utils.o \
combine.o


$(PROGRAM):  $(OBJS)
	$(FC) $(OBJS) $(LIBRARIES) -o $(PROGRAM)

.F.o:
	$(FC) $(FC_FLAGS) $<


