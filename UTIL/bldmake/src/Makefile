# make file to build bldmake program

 EXEC = bldmake

#To use nondefault compiler, type "make COMPILER=compiler" where compiler is PGF90 or GFORT.

ifndef COMPILER
 COMPILER = INTEL
#COMPILER = PGF90
#COMPILER = GFORT
endif

ifeq ($(COMPILER),INTEL)
 FC = ifort
#F_FLAGS = -O2 -fixed -extend_source -WB 
 F_FLAGS = -g -fixed -CU -CB -g -traceback -extend_source -WB 
else ifeq ($(COMPILER),PGF90)
 FC = pgf90
 F_FLAGS = -Bstatic -Mextend -Mbounds -Mfixed -traceback
else ifeq ($(COMPILER),GFORT)
 FC = gfortran
 F_FLAGS = -ffixed-form -ffixed-line-length-132  -fcheck=all -fbacktrace
else
 ERROR1 = "Makefile does not configure to support the specified compiler, $(COMPILER). User must modify Makefile."
endif

ifdef ERROR1
 $(error $(ERROR1))
endif

 OBJS= cfg_module.o parser.o utils.o bldmake.o

.SUFFIXES: .f

$(EXEC): $(OBJS)
	$(FC) $(OBJS) -o $@

.f.o:
	$(FC) -c $(F_FLAGS) $<

clean:
	\rm -f *.o *.mod 
# make file to build bldmake program

