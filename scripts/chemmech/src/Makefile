# original file copied from /home/yoj/src/mech/BLD_2, 1/31/2008
# need to source scom command for compilers

MODEL = CHEMMECH

FC = ifort 
CC = icc 
F_FLAGS = -fixed -132 -O3 -override-limits -fno-alias -mp1  -I . -g
C_FLAGS =  -O2  -DFLDMN=1

#  LINK_FLAGS = $(myLINK_FLAG)
LINK_FLAGS = -i-static

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
