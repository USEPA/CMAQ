
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
C $Header: /project/yoj/arc/CCTM/src/vdiff/acm2/rddepv.F,v 1.6 2011/12/09 14:19:17 yoj Exp $
 
C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%
 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE RDDEPV ( JDATE, JTIME, DEPV )

C-----------------------------------------------------------------------
C Function:
C   Get deposition velocities for vertical diffusion
 
C Preconditions:
 
C Subroutines and functions called:
C INTERP3, M3EXIT, AERO_DEPV
 
C Revision History:
C   Jeff - Sep 97 - targetted, call subroutine for aerosol species
C   Jeff - Dec 97 - add MDEPV
C   2 October, 1998 by Al Bourgeois at LM: parallel implementation
C   Jeff - May 2000 - correct DEPV to "real world" values
C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN; replace INTERP3 with INTERPX
C   15 Mar 03 J.Young: swap depv and mdepv dimensions
C   21 Jul 03 J.Gipson: revised to compute avg DEPV for aero3 NR CG/SOA species
C   22 Aug 03 J.Young: aero in ppm units, eliminate MDEPV
C   23 Oct 03 J.Young: change depv from flux to m/s
C   14 Sep 04 J.Gipson: generalized avg DEPV calcs for aero3 SGTOT species
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   07 Jun 05 P.Bhave: added logic for 'AE4' mechanism
C   21 Jun 10 J.Young: convert for Namelist redesign
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   16 May 11 J.Young: remove references to AERO3 and AERO4
C   09 Dec 11 J.Young: remove unused Flag for SGTOT species
C   07 Jul 14 B.Hutzell: replace mechanism include file(s) with fortran module
C-----------------------------------------------------------------------

      USE HGRD_DEFN           ! horizontal grid specifications
      USE RXNS_DATA           ! chemical mechanism data
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:

      INTEGER, INTENT( IN )  :: JDATE, JTIME ! internal simulation date&time, tstep
      REAL,    INTENT( OUT ) :: DEPV( :,:,: )

C Paramters:

C External Functions:

C Local variables:

      REAL, ALLOCATABLE, SAVE :: DBUFF( :,: )       ! read buffer

      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      CHARACTER( 16 ), SAVE :: PNAME = 'RDDEPV'
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER ALLOCSTAT
      INTEGER C, R, V, N                      ! loop induction variables
      INTEGER STRT, FINI                      ! loop induction variables

      INTEGER GXOFF, GYOFF              ! global origin offset from file
C for INTERPX
      INTEGER, SAVE :: STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2

C-----------------------------------------------------------------------

      IF ( FIRSTIME )  THEN

         FIRSTIME  =  .FALSE.

         ALLOCATE ( DBUFF( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating BUFF'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         CALL SUBHFILE ( MET_CRO_2D, GXOFF, GYOFF,
     &                   STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2 )

      END IF   ! Firstime

      N = 0
      STRT = 1
      FINI = N_GC_DEPV
      DO V = STRT, FINI
         N = N + 1

C                                        vvvvvvv - dep vel surrogate name
         IF ( .NOT. INTERPX( MET_CRO_2D, GC_DEPV( N ), PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, DBUFF ) ) THEN
            XMSG = 'Could not read  ' // GC_DEPV( N ) // ' from ' // MET_CRO_2D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               DEPV( V,C,R ) = GC_DEPV_FAC( N ) * DBUFF( C,R ) 
            END DO
         END DO

      END DO

      N = 0
      STRT = N_GC_DEPV + N_AE_DEPV + 1
      FINI = N_GC_DEPV + N_AE_DEPV + N_NR_DEPV
      DO V = STRT, FINI
         N = N + 1

C                                        vvvvvvv - dep vel surrogate name
         IF ( .NOT. INTERPX( MET_CRO_2D, NR_DEPV( N ), PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, DBUFF ) ) THEN
            XMSG = 'Could not read  ' // NR_DEPV( N ) // ' from ' // MET_CRO_2D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               DEPV( V,C,R ) = NR_DEPV_FAC( N ) * DBUFF( C,R )
            END DO
         END DO

      END DO

      N = 0
      STRT = N_GC_DEPV + N_AE_DEPV + N_NR_DEPV + 1
      FINI = N_GC_DEPV + N_AE_DEPV + N_NR_DEPV + N_TR_DEPV 
      DO V = STRT, FINI
         N = N + 1

C                                         vvvvvvv - dep vel surrogate name
         IF ( .NOT. INTERPX( DEPV_TRAC_1, TR_DEPV( N ), PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, DBUFF ) ) THEN
            XMSG = 'Could not read  ' // TR_DEPV( N ) // ' from ' // DEPV_TRAC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               DEPV( V,C,R ) = TR_DEPV_FAC( N ) * DBUFF( C,R ) 
            END DO
         END DO

      END DO

      RETURN
      END
