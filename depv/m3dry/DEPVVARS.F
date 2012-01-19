
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
C-----------------------------------------------------------------------

      INTEGER, PARAMETER :: LDDEP = 4
      INTEGER, PARAMETER :: LTOTG = 52
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

         USE HGRD_DEFN
         USE UTILIO_DEFN

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

         N_USE_DEPSPC = 0
         USE_DEPSPC = .FALSE.   ! array

         RETURN

         END FUNCTION DEPVVARS_INIT

      END MODULE DEPVVARS
