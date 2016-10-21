
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
C $Header: /project/yoj/arc/CCTM/src/depv/m3dry/DEPVVARS.F,v 1.7 2012/01/19 14:21:45 yoj Exp $

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE DEPVVARS

C-----------------------------------------------------------------------
C Name:     Dry Deposition Variables
C Purpose:  Contains arrays specific to dry deposition species.
C           Initializes dry deposition arrays.
C Revised:  19 Aug 2005  Original version.  (T. Otte and W. Hutzell)
C           25 Nov 2006 J.Young: combine
C           30 Apr 2008  Removed references to RADMDRY.  Added five air
C                        toxic species to LTOTG.  (T. Otte and W. Hutzell)
C           16 Feb 2011 S.Roselle: replaced I/O API include files with
C                        UTILIO_DEFN
C           11 May 2011 J.Bash: Updated for NH3 bidirectional exchange
C           04 Jan 2012 J.Young: Initialize char variables with blank padding
C           28 Aug 2014  G. Sarwar: added deposition for CLNO2 
C           07 Nov 14 J.Bash: Updated for the ASX_DATA_MOD shared data module. 
C           05 May 2015: H.Pye: Added ISOPNN and MTNO3 with Donna Schwede
C           24 Aug 2015: H.Pye: Added IEPOX and HACET with Donna Schwede
C           26 Jan 2016: H. Pye added info for SOA SVOCs
C           24 Mar 2016: G. Sarwar: added iodine and bromine species
C           26 Sep 2016: D. Wong: removed unnecessary use module statements

C-----------------------------------------------------------------------

      INTEGER, PARAMETER :: LDDEP = 4
      INTEGER, PARAMETER :: LTOTG = 100
      INTEGER, PARAMETER :: LCMP  = 8
      CHARACTER( 16 )    :: DEPV_METHOD

C-----------------------------------------------------------------------
C General dry deposition arrays.
C-----------------------------------------------------------------------

      INTEGER, SAVE         :: N_USE_DEPSPC        
      LOGICAL, SAVE         :: USE_DEPSPC( LTOTG )
      CHARACTER( 16 ), SAVE :: DEPSPC( LTOTG )

C----------------------------------------------------------------------
C local deposition velocites [ m/s ]
C----------------------------------------------------------------------

      CONTAINS

         FUNCTION DEPVVARS_INIT( ) RESULT ( SUCCESS )

         IMPLICIT NONE

         LOGICAL SUCCESS

         INTEGER ALLOCSTAT
         CHARACTER( 96 ) :: XMSG

         SUCCESS = .TRUE.

C-------------------------------------------------------------------------------
C For M3DRY, set up core species, and include toxic and chlorine compounds.
C-------------------------------------------------------------------------------

         DEPV_METHOD  = 'M3DRY           '

         DEPSPC(  1 ) = 'SO2             '
         DEPSPC(  2 ) = 'SULF            '
         DEPSPC(  3 ) = 'NO2             '
         DEPSPC(  4 ) = 'NO              '
         DEPSPC(  5 ) = 'O3              '
         DEPSPC(  6 ) = 'HNO3            '
         DEPSPC(  7 ) = 'H2O2            '
         DEPSPC(  8 ) = 'ALD             '
         DEPSPC(  9 ) = 'HCHO            '
         DEPSPC( 10 ) = 'OP              '
         DEPSPC( 11 ) = 'PAA             '
         DEPSPC( 12 ) = 'ORA             '
         DEPSPC( 13 ) = 'NH3             '
         DEPSPC( 14 ) = 'PAN             '
         DEPSPC( 15 ) = 'HONO            '
         DEPSPC( 16 ) = 'CO              '
         DEPSPC( 17 ) = 'METHANOL        '
         DEPSPC( 18 ) = 'N2O5            '
         DEPSPC( 19 ) = 'NO3             '
         DEPSPC( 20 ) = 'GEN_ALD         '
         DEPSPC( 21 ) = 'CL2             '
         DEPSPC( 22 ) = 'HOCL            '
         DEPSPC( 23 ) = 'HCL             '
         DEPSPC( 24 ) = 'FMCL            '
         DEPSPC( 25 ) = 'HG              '
         DEPSPC( 26 ) = 'HGIIGAS         '
         DEPSPC( 27 ) = 'TECDD_2378      '
         DEPSPC( 28 ) = 'PECDD_12378     '
         DEPSPC( 29 ) = 'HXCDD_123478    '
         DEPSPC( 30 ) = 'HXCDD_123678    '
         DEPSPC( 31 ) = 'HXCDD_123789    '
         DEPSPC( 32 ) = 'HPCDD_1234678   '
         DEPSPC( 33 ) = 'OTCDD           '
         DEPSPC( 34 ) = 'TECDF_2378      '
         DEPSPC( 35 ) = 'PECDF_12378     '
         DEPSPC( 36 ) = 'PECDF_23478     '
         DEPSPC( 37 ) = 'HXCDF_123478    '
         DEPSPC( 38 ) = 'HXCDF_123678    '
         DEPSPC( 39 ) = 'HXCDF_234678    '
         DEPSPC( 40 ) = 'HXCDF_123789    '
         DEPSPC( 41 ) = 'HPCDF_1234678   '
         DEPSPC( 42 ) = 'HPCDF_1234789   '
         DEPSPC( 43 ) = 'OTCDF           '
         DEPSPC( 44 ) = 'NAPHTHALENE     '
         DEPSPC( 45 ) = '1NITRONAPHTHA   '
         DEPSPC( 46 ) = '2NITRONAPHTHA   '
         DEPSPC( 47 ) = '14NAPHTHOQUIN   '
         DEPSPC( 48 ) = 'HEXMETH_DIIS    '
         DEPSPC( 49 ) = 'HYDRAZINE       '
         DEPSPC( 50 ) = 'MAL_ANHYDRIDE   '
         DEPSPC( 51 ) = 'TOLUENE_DIIS    '
         DEPSPC( 52 ) = 'TRIETHYLAMINE   '
         DEPSPC( 53 ) = 'NTR             '
         DEPSPC( 54 ) = 'NTRM            '
         DEPSPC( 55 ) = 'MPAN            '
         DEPSPC( 56 ) = 'PPN             '
         DEPSPC( 57 ) = 'ISPD            '
         DEPSPC( 58 ) = 'NTRDN           '
         DEPSPC( 59 ) = 'NTRALK          '
         DEPSPC( 60 ) = 'NTROH           '
         DEPSPC( 61 ) = 'NTRPX           '
         DEPSPC( 62 ) = 'PROPNN          '
         DEPSPC( 63 ) = 'CLNO2           '
         DEPSPC( 64 ) = 'ISOPNN          '
         DEPSPC( 65 ) = 'MTNO3           '
         DEPSPC( 66 ) = 'IEPOX           '
         DEPSPC( 67 ) = 'HACET           '
         DEPSPC( 68 ) = 'SVALK1          '
         DEPSPC( 69 ) = 'SVALK2          '
         DEPSPC( 70 ) = 'SVBNZ1          '
         DEPSPC( 71 ) = 'SVBNZ2          '
         DEPSPC( 72 ) = 'SVISO1          '
         DEPSPC( 73 ) = 'SVISO2          '
         DEPSPC( 74 ) = 'SVPAH1          '
         DEPSPC( 75 ) = 'SVPAH2          '
         DEPSPC( 76 ) = 'SVSQT           '
         DEPSPC( 77 ) = 'SVTOL1          '
         DEPSPC( 78 ) = 'SVTOL2          '
         DEPSPC( 79 ) = 'SVTRP1          '
         DEPSPC( 80 ) = 'SVTRP2          '
         DEPSPC( 81 ) = 'SVXYL1          '
         DEPSPC( 82 ) = 'SVXYL2          '
         DEPSPC( 83 ) = 'IO              '
         DEPSPC( 84 ) = 'OIO             '
         DEPSPC( 85 ) = 'I2O2            '
         DEPSPC( 86 ) = 'I2O3            '
         DEPSPC( 87 ) = 'I2O4            '
         DEPSPC( 88 ) = 'HI              '
         DEPSPC( 89 ) = 'HOI             '
         DEPSPC( 90 ) = 'INO             '
         DEPSPC( 91 ) = 'INO2            '
         DEPSPC( 92 ) = 'IONO2           '
         DEPSPC( 93 ) = 'BRO             '
         DEPSPC( 94 ) = 'HOBR            '
         DEPSPC( 95 ) = 'HBR             '
         DEPSPC( 96 ) = 'BRONO2          '
         DEPSPC( 97 ) = 'BRNO2           '
         DEPSPC( 98 ) = 'BRCL            '
         DEPSPC( 99 ) = 'DMS             '
         DEPSPC( 100) = 'MSA             '

         N_USE_DEPSPC = 0
         USE_DEPSPC = .FALSE.   ! array

         RETURN

         END FUNCTION DEPVVARS_INIT

      END MODULE DEPVVARS
