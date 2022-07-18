
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/aero/aero6/isrpia.inc,v 1.4 2011/10/21 16:10:15 yoj Exp $

C=======================================================================
C *** ISORROPIA CODE II
C *** INCLUDE FILE 'ISRPIA.INC'
C *** THIS FILE CONTAINS THE DECLARATIONS OF THE GLOBAL CONSTANTS
C     AND VARIABLES. 
C
C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C *** UPDATED BY CHRISTOS FOUNTOUKIS
C
C=======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NCOMP=8,NIONS=10,NGASAQ=3,NSLDS=19,NPAIR=23,NZSR=100,
     &           NERRMX=25)
C
C *** INPUT VARIABLES **************************************************
C
      INTEGER METSTBL
      COMMON /INPT/ W(NCOMP), WAER(NCOMP), TEMP, RH, IPROB, METSTBL,
     &              NADJ
C
C *** WATER ACTIVITIES OF PURE SALT SOLUTIONS **************************
C
      COMMON /ZSR / AWAS(NZSR), AWSS(NZSR), AWAC(NZSR), AWSC(NZSR),
     &              AWAN(NZSR), AWSN(NZSR), AWSB(NZSR), AWAB(NZSR),
     &              AWSA(NZSR), AWLC(NZSR), AWCS(NZSR), AWCN(NZSR),
     &              AWCC(NZSR), AWPS(NZSR), AWPB(NZSR), AWPN(NZSR),
     &              AWPC(NZSR), AWMS(NZSR), AWMN(NZSR), AWMC(NZSR)
C
C *** DELIQUESCENCE RELATIVE HUMIDITIES ********************************
C
      INTEGER WFTYP
      COMMON /DRH / DRH2SO4,  DRNH42S4, DRNAHSO4, DRNACL,   DRNANO3, 
     &              DRNA2SO4, DRNH4HS4, DRLC,     DRNH4NO3, DRNH4CL,
     &              DRCASO4,  DRCANO32, DRCACL2,  DRK2SO4,  DRKHSO4,
     &              DRKNO3,   DRKCL,    DRMGSO4,  DRMGNO32, DRMGCL2
C
      COMMON /MDRH/ DRMLCAB,  DRMLCAS,  DRMASAN,  DRMG1,    DRMG2,
     &              DRMG3,    DRMH1,    DRMH2,    DRMI1,    DRMI2,
     &              DRMI3,    DRMQ1,    DRMR1,    DRMR2,    DRMR3,
     &              DRMR4,    DRMR5,    DRMR6,    DRMR7,    DRMR8,
     &              DRMR9,    DRMR10,   DRMR11,   DRMR12,   DRMR13,
     &              WFTYP
C
      COMMON /MDRH2/ DRMO1,    DRMO2,    DRMO3,    DRML1,    DRML2,
     &               DRML3,    DRMM1,    DRMM2,    DRMP1,    DRMP2,
     &               DRMP3,    DRMP4,    DRMP5,    DRMV1

C
C *** VARIABLES FOR LIQUID AEROSOL PHASE *******************************
C
      DOUBLE PRECISION MOLAL, MOLALR, M0
      REAL IONIC
      LOGICAL CALAOU, CALAIN, FRST, DRYF
      COMMON /IONS/ MOLAL(NIONS), MOLALR(NPAIR), GAMA(NPAIR), ZZ(NPAIR),
     &              Z(NIONS),     GAMOU(NPAIR),  GAMIN(NPAIR),M0(NPAIR),
     &              GASAQ(NGASAQ),
     &              EPSACT,       COH,           CHNO3,       CHCL,         
     &              WATER,        IONIC,         IACALC,      
     &              FRST,         CALAIN,        CALAOU,      DRYF
C
C *** VARIABLES FOR SOLID AEROSOL PHASE ********************************
C
      COMMON /SALT/ CH2SO4,  CNH42S4, CNH4HS4, CNACL,   CNA2SO4, 
     &              CNANO3,  CNH4NO3, CNH4CL,  CNAHSO4, CLC, CCASO4,
     &              CCANO32, CCACL2,  CK2SO4,  CKHSO4,  CKNO3, CKCL,
     &              CMGSO4,  CMGNO32, CMGCL2
C
C *** VARIABLES FOR GAS PHASE ******************************************
C
      COMMON /GAS / GNH3, GHNO3, GHCL, GNH3D, GHNO3D, GHCLD
C
C *** EQUILIBRIUM CONSTANTS ********************************************
C
      COMMON /EQUK/ XK1, XK2, XK3, XK4, XK5, XK6, XK7, XK8, XK9, XK10,
     &              XK11,XK12,XK13,XK14,XKW, XK21,XK22,XK31,XK32,XK41,
     &              XK42, XK15, XK16, XK17, XK18, XK19, XK20, XK23,
     &              XK24, XK25
C     &              , XK26, XK27
C
C *** MOLECULAR WEIGHTS ************************************************
C
      DOUBLE PRECISION IMW
      COMMON /OTHR/ R, IMW(NIONS), WMW(NCOMP), SMW(NPAIR)
C
C *** SOLUTION/INFO VARIABLES ******************************************
C
      CHARACTER SCASE*15
      COMMON /CASE/ SULRATW, SULRAT, SODRAT, SO4RAT, CRNARAT, CRRAT,
     &              SCASE
C
      COMMON /SOLN/ EPS, MAXIT, NSWEEP, NDIV, ICLACT
C
C *** ERROR SYSTEM *****************************************************
C
      CHARACTER ERRMSG*40
      INTEGER   ERRSTK, NOFER   
      LOGICAL   STKOFL   
      COMMON /EROR/ STKOFL, NOFER, ERRSTK(NERRMX), ERRMSG(NERRMX)
C
C *** GENERIC VARIABLES ************************************************
C
      CHARACTER VERSION*15
      COMMON /CGEN/ GREAT, TINY, TINY2, ZERO, ONE, VERSION

C *** Wen added for ddmisrpia
C
C *** MOLAL BEFORE ADJUSTMENT OF MINOR SPEICIES ************************
C
      DOUBLE PRECISION MOLALD
      COMMON /MINOR/ MOLALD(NIONS)
C
C *** WEN ADDED FOR FILTERING OUT NONPHYSICAL POINTS IN CALCD3
C
      INTEGER NONPYS,SAT
      COMMON /FILT/ NONPYS,SAT
C
C *** END OF INCLUDE FILE **********************************************
C
