# make file to build bldmake program

 EXEC = bldmake

 FC = /usr/local/intel/ictce/3.2.2.013/fc/bin/intel64/ifort
#F_FLAGS = -O2 -fixed -extend_source -WB 
 F_FLAGS = -g -fixed -extend_source -WB 

#FC = /usr/local/pgi-11.9/linux86-64/11.9/bin/pgf90
#F_FLAGS = -Bstatic -Mextend -Mbounds -Mfixed

#FC = /usr/local/gcc-4.6/bin/gfortran
#F_FLAGS = -ffixed-form -ffixed-line-length-132

 OBJS= cfg_module.o parser.o utils.o bldmake.o

.SUFFIXES: .f

$(EXEC): $(OBJS)
	$(FC) $(OBJS) -o $@

.f.o:
	$(FC) -c $(F_FLAGS) $<

clean:
	rm *.o *.mod bldmake
# make file to build bldmake program

