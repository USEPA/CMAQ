!*******************************************************************************
!$RCSfile: sciprime_inc.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
MODULE sciprime_inc
USE param_inc
SAVE

INTEGER, PARAMETER            :: istrg=132,ifmax=40,ipn=2,ikn=6
  ! IFMAX  = Max Number of Fields Per Runstream Record
  ! ISTRG  = Length of Runstream Image Record
  ! IPN    = Number of Pathway IDs (Includes '**')
  ! IKN    = Number of keywords
INTEGER, PARAMETER            :: mxntr=50,nsrc=1,nsec=36,mxnz=100,nst=6
  ! MXNTR  = Maximum number of downwind distances for which
  !          numerical plume rise will be reported
  ! NSRC   = Max Number of Sources
  ! NSEC   = Number of Sectors for Building Dimensions
  ! MXNZ   = Maximum number of vertical layers in
  !          the meteorological data
  ! NST    = Number of stability classes

INTEGER, PARAMETER            :: mxnzp1=mxnz+1
INTEGER, PARAMETER            :: inunit=111, iounit=112
INTEGER,DIMENSION(ifmax)      :: locb, loce
INTEGER,DIMENSION(ikn)        :: isstat
INTEGER,DIMENSION(nsrc,ikn)   :: iwrk2
INTEGER                       :: iline,ifc,ipnum,ippnum,kst
REAL,DIMENSION(nsec,nsrc)     :: adsbh,adsbw,adsbl,adsxadj,adsyadj
CHARACTER                     :: runst(istrg)
CHARACTER*2                   :: path,ppath
CHARACTER*8                   :: pkeywd,keywrd,keywd(ikn),srcid(nsrc)
CHARACTER*40                  :: field(ifmax)
CHARACTER*132                 :: runst1 
LOGICAL                       :: bline,infld,rural,urban
LOGICAL,DIMENSION(nsrc)       :: isrural

REAL,DIMENSION(2)       :: xrel_prm,yrel_prm,zrel_prm,frac_prm,sigy_prm,sigz_prm
REAL,DIMENSION(2)       :: ky_prm,kz_prm,wmom_prm,buoy_prm
REAL                    :: cmass_prm
REAL,DIMENSION(MAX_MC)  :: rel_mc_prm

END MODULE sciprime_inc
