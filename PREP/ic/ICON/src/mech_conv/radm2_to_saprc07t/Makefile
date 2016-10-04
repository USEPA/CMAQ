
# RCS file, release, date & time of last delta, author, state, [and locker]
# $Header: /project/yoj/arc/ICON/src/mech_conv/radm2_to_saprc07t/Makefile,v 1.1 2011/09/12 16:31:35 yoj Exp $

 EXEC = CONVDAT_SAPRC07T
 FC = ifort
 CC = icc
 CPP  = FC
 FSTD = -fixed -extend_source -fno-alias -mp1
 DBG = -check bounds -fpe0 -d-lines -fno-alias -ftrapuv -traceback

#F_FLAGS = $(FSTD) -O3       -I.
 F_FLAGS = $(FSTD) -g $(DBG) -I.
 C_FLAGS = -O2 -DFLDMN -I.
 CPP_FLAGS = -Ddebug
#CPP_FLAGS =
 INCLUDES =
 LIBRARIES =
 LINK_FLAGS = -i-static
 OBJS = saprc07t_table.o convdat.o wrdate.o

 .SUFFIXES: .F .f

$(EXEC): $(OBJS)
	$(FC) $(LINK_FLAGS) $(OBJS) $(LIBRARIES) -o $@

.F.o:
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

.f.o:
	$(FC) $(F_FLAGS) -c $<

clean:
	rm -f $(OBJS) *.mod *__genmod.f90

clear:
	rm -f $(OBJS) $(EXEC) *.mod *__genmod.f90


