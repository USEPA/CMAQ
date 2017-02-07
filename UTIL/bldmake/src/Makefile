# make file to build bldmake program
# source config.cmaq before running this script

EXEC = BLDMAKE

FC = $(myFC)
F_FLAGS = $(myFSTD) $(myFFLAGS)

OBJS= cfg_module.o parser.o utils.o bldmake.o

.SUFFIXES: .f

$(EXEC): $(OBJS)
	$(FC) $(OBJS) -o $@

.f.o:
	$(FC) -c $(F_FLAGS) $<

clean:
	rm *.o *.mod $EXEC

