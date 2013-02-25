# original file copied from /home/yoj/src/mech/BLD_2, 1/31/2008
# need to source scom command for compilers

  MODEL = CHEMMECH_ggb_SS4


#  FC = $(myFC) 
#  CC = $(myCC)
#  F_FLAGS = $(myFFLAGS) -I . -g
#  C_FLAGS = $(myCFLAGS) -DFLDMN=1

FC = /usr/local/intel/ictce/3.2.2.013/fc/bin/intel64/ifort
CC = /usr/local/intel/ictce/3.2.2.013/cc/bin/intel64/icc
F_FLAGS = -fixed -132 -O3 -override-limits -fno-alias -mp1  -I . -g
C_FLAGS =  -O2  -DFLDMN=1
#  LINK_FLAGS = $(myLINK_FLAG)
LINK_FLAGS = -i-static

FC = /usr/local/pgi/linux86-64/10.5/bin/pgf90
CC    = /usr/local/pgi/linux86-64/10.5/bin/pgcc

F_FLAGS = -Mfixed -Mextend -O3 -I . -g -C
C_FLAGS =  -O2  -DFLDMN=1
LINK_FLAGS = -Bstatic  -Bstatic_pgi

# compiler options for subroutines

 
OBJECTS =\
    chemmech.o \
    eatcom.o \
    check_ss_spc.o \
    getchar.o \
    getlabel.o \
    get_operator.o \
    getprdct.o \
    getrate.o \
    getrctnt.o \
    getreal.o \
    getword.o \
    get_ss_data.o \
    junit.o \
    index1.o \
    lkupspec.o \
    rdline.o \
    size.o \
    wrexts.o \
    wrhdr1.o \
    wrbf6.o \
    wrbf16c.o \
    wrbf12d.o \
    wrbf12s.o \
    wrspecial_ext.o \
    wrss_ext.o  \
    vallabchr.o \
    valwrdchr.o \
    nameval.o
        
.SUFFIXES: .f .c

$(MODEL): $(OBJECTS)
	$(FC) $(LINK_FLAGS) $(OBJECTS) -o $@

.f.o:
	$(FC) $(F_FLAGS) -c $<

.c.o:
	$(CC) $(C_FLAGS) -c $<

clean:
	\rm -f *.o
