!******************************************************************************
!Change MAX_MC and MAXPUF
!PK, AER, March 2005
!Increase MAXTLV -BC, March 2006
!Change definition of MAX_MC -BC, Jan 2007
!Increase MAXPUF, PKK, ENVIRON, Feb 2015
!******************************************************************************

      integer   MAXPUF, MAXCLS, MAXPTYP, MAXSTYP, MAXMTYP
      integer   MAXSGP, MAXTLV, MAXSG, MAXSF, MAXMAUX, MAXSAUX
      integer   I_LATLON, I_CARTESIAN, I_UTM, I_METERS, I_PIG
      integer   I_REMOVE, I_STATIC
      integer   IHAZ_OFF, IHAZ_ON, IHAZ_COMB
      integer   NP_SUM, NP_RAT, NP_REAL, NP_INT, NP_ALL

      integer   MAXPAUX
      integer   MAXPMAUX,MAXPMAUXX,PMAUX_BOUNDS,PMAUX_MEAN
      integer   MAXGMAUX
      integer   NRAIN,NSNOW,NWASH

      integer   MAX_MC, MAX_MC2
      integer   MCA_HEADER,MCA_SPECIES,MCA_LIN,MCA_QUAD
      integer   MCH_NSPEC,MCH_NLIN,MCH_NQUAD
      integer   MCH_NMCORR,MCH_NTCORR
      integer   MCH_NSDOS,MCH_NSDEP

      integer   NAUX_DYNAMICS_GAS,NAUX_DYNAMICS_PART,NAUX_TOTALCC
      integer   NAUX_DENSE_GAS,NAUX_DENSE_PART
      integer   NAUX_DYNAMICS
      integer   NAUX_DENSE, NAUX_BUOY
      integer   NAUX_STATIC

      integer   SCIPUFF_STATUS_ARRAY_SIZE
      integer   SCIPUFF_MAXRELPARAM
      integer   OPREADY_INDEX
      integer   DOMAIN_STATUS_ARRAY_SIZE
      integer   TIME_STATUS_ARRAY_SIZE
      integer   STANDARD_MODE,FAST_MODE

      parameter (MAXPUF   =  300000)  !Maximum Number of Puffs
      parameter (MAXCLS   =       3)  !Maximum Number of Classes
      parameter (MAXSTYP  =      40)  !Maximum Number of Surface types
      parameter (MAXMTYP  =       5)  !Maximum Number of Material types
      parameter (MAXSGP   =      50)  !Maximum Number of Groups/Type
      parameter (MAXTLV   =      20)  !Maximum Number of Time levels
      parameter (MAXPTYP  = 2*MAXSGP) !Maximum Number of Puff types
      parameter (MAXMAUX  =      600) !Maximum Number of Material aux. variables
      parameter (MAXSAUX  =      600) !Maximum Number of Source aux. variables

      parameter (NRAIN    =3) !Maximum Number of Rain groups
      parameter (NSNOW    =3) !Maximum Number of Snow groups
      parameter (NWASH    = NRAIN+NSNOW)      !Maximum Number of Precip. groups

!------ Coordinate system flag values

      parameter (I_LATLON    = 1)
      parameter (I_CARTESIAN = 2)
      parameter (I_UTM       = 3)
      parameter (I_METERS    = 4)
      parameter (I_PIG       = 5)

!------ Hazard area/Met uncertainty flags values

      parameter (IHAZ_OFF  = 0)
      parameter (IHAZ_ON   = 1)
      parameter (IHAZ_COMB = 2)

!------ Run modes

      parameter (STANDARD_MODE = 0)
      parameter (FAST_MODE     = 1)
      
!------ Special puff time level settings (must be negative)

      parameter (I_REMOVE    = -1)    !Remove puff
      parameter (I_STATIC    = -2)    !Static puff

!------ SCIPUFF dimensions

      parameter (NP_SUM  = 10)   !No. of puff variables merged by summimg
      parameter (NP_RAT  = 10)   !No. of puff variables merged by mass-weighted ratio
      parameter (NP_INT  =  7)   !No. of puff integer variables
      parameter (NP_REAL = 16 + NP_SUM + NP_RAT)
      parameter (NP_ALL  = NP_REAL + NP_INT)

      parameter (MAXSG = 25000)       !Max surface grid
      parameter (MAXSF =     3)       !No. fields associated with surface integral

      parameter (SCIPUFF_STATUS_ARRAY_SIZE = 60)   !Not implemented
      parameter (SCIPUFF_MAXRELPARAM = 25)         !Release aux data
      parameter (OPREADY_INDEX = 2)                !Not implemented
      parameter (DOMAIN_STATUS_ARRAY_SIZE = 20)    !Not implemented
      parameter (TIME_STATUS_ARRAY_SIZE = 5)       !Not implemented
      parameter (MAXPMAUX  =    5)    !Particle aux data per size bin
      parameter (MAXPMAUXX =    3)    !Overall particle aux data
      parameter (PMAUX_BOUNDS = 2)    !Offset to size bin boundary in aux data
      parameter (PMAUX_MEAN   = 3)    !Offset to mean size

      parameter (MAXGMAUX =     2)    !Gas material parameters

!------ MULTI-COMPONENT

      parameter (MAX_MC     = 250)        !Maximum no. of components
      parameter (MAX_MC2    = 4*MAX_MC+5) !Maximum size of mc array
      parameter (MCA_HEADER =  10)        !header size
      parameter (MCA_SPECIES=   4)        !species info
      parameter (MCA_LIN    =   6)        !linear eqn info
      parameter (MCA_QUAD   =   9)        !quadratic eqn info

      parameter (MCH_NSPEC  =   1)    !# of species location
      parameter (MCH_NLIN   =   2)    !# of linear eqs location
      parameter (MCH_NQUAD  =   3)    !# of quadratic eqs location
      parameter (MCH_NMCORR =   4)    !# of mean overlap species
      parameter (MCH_NTCORR =   5)    !# of turb correlations location
      parameter (MCH_NSDOS  =   6)    !# of sfc dose species
      parameter (MCH_NSDEP  =   7)    !# of sfc dep species

!------ Puff auxiliary dimensions

      parameter (NAUX_DYNAMICS_GAS  = 6)      !Dynamic data for gas
      parameter (NAUX_DYNAMICS_PART = 4)      !Dynamic data for particles
      parameter (NAUX_DENSE_PART    = 6)      !Dense gas dynamic variables
      parameter (NAUX_DENSE_GAS     = 11)     !Dense gas dynamic variables
      parameter (NAUX_BUOY  = 2)      !Buoyant gas dynamic variables
      parameter (NAUX_DYNAMICS      = NAUX_DYNAMICS_GAS)
      parameter (NAUX_DENSE = NAUX_DENSE_GAS)

      parameter (NAUX_TOTALCC  = 2)   !Data for particle CC
      parameter (NAUX_STATIC   = 3)   !Data for statics

      parameter (MAXPAUX  =  (MAX_MC2+20)*MAXPUF)!Maximum Number of Puff aux. variables
                                          !4*MAX_MC+5+NAUX_STATIC+NAUX_BUOY etc.
