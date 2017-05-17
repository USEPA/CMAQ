# make file to build bldmake program
 EXEC = BLDMAKE

 FC = /usr/local/apps/intel/composer_xe_2013.2.146/bin/intel64/ifort
 F_FLAGS = -g -fixed -extend_source -WB 

 OBJS= cfg_module.o parser.o utils.o bldmake.o

.SUFFIXES: .f

$(EXEC): $(OBJS)
	$(FC) $(OBJS) -o $@

.f.o:
	$(FC) -c $(F_FLAGS) $<

clean:
	rm *.o *.mod ${EXEC}

