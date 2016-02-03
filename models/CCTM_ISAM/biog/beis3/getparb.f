
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
C $Header: /project/yoj/arc/CCTM/src/biog/beis3/getparb.f,v 1.2 2011/10/21 16:10:18 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        SUBROUTINE GETPARB( RSOLAR, PRES, COSZ, PARDB, PARDIF )

C-----------------------------------------------------------------------
C Description:
C   Compute direct and diffuse photosynthetically active radiation (PAR).
C   Based on code from Bart Brashers (10/2000), which was based on
C   code from Weiss and Norman (1985).  
 
C Preconditions:
C   Solar radiation (W/m2) and pressure (mb)
 
C Subroutines and Functions Called:
 
C Revision History:
C   3/01 Prototype by JMV
C  10/17 J.Young: rename, optimize
C-----------------------------------------------------------------------
C Modified from:

C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling System
C File: @(#)$Id: getparb.f,v 1.2 2011/10/21 16:10:18 yoj Exp $
C COPYRIGHT (C) 2004, Environmental Modeling for Policy Development
C All Rights Reserved
C Carolina Environmental Program
C University of North Carolina at Chapel Hill
C 137 E. Franklin St., CB# 6116
C Chapel Hill, NC 27599-6116
C smoke@unc.edu
C Pathname: $Source: /project/yoj/arc/CCTM/src/biog/beis3/getparb.f,v $
C Last updated: $Date: 2011/10/21 16:10:18 $ 
C-----------------------------------------------------------------------

      IMPLICIT NONE

C Arguments:
      REAL, INTENT ( IN ) :: RSOLAR   ! modeled or observed total radiation [W/m2]
      REAL, INTENT ( IN ) :: PRES     ! atmospheric pressure [mb]
      REAL, INTENT ( IN ) :: COSZ     ! cosine of solar zenith angle
      REAL, INTENT( OUT ) :: PARDB    ! direct beam PAR [umol/m2-s]
      REAL, INTENT( OUT ) :: PARDIF   ! diffuse PAR [umol/m2-s]

C Parameters:
      REAL, PARAMETER :: WATT2UMOL = 4.6  ! convert W/m^2 to umol/m^2-s

C Local Variables:
      REAL RATIO              ! transmission fraction for total radiation
      REAL OT                 ! optical thickness
C     REAL RDVIS              ! possible direct visible beam [(W/m^2]
C     REAL RFVIS              ! possible visible diffuse [(W/m^2]
      REAL WA                 ! water absorption in near-IR [(W/m^2]
C     REAL RDIR               ! direct beam in near-IR [(W/m^2]
C     REAL RFIR               ! diffuse near-IR [(W/m^2]
C     REAL RVT                ! total possible visible radiation [(W/m^2]
C     REAL RIRT               ! total possible near-IR radiation [(W/m^2]
C     REAL FVIS               ! fraction of visible to total 
C     REAL FVB                ! fraction of visible that is direct beam
C     REAL FVD                ! fraction of visible that is diffuse
      REAL FAC                ! direct beam factor / combination factor

      REAL A, B, C, DEN, RW   ! replacement composite variables

      CHARACTER(  16 ) :: PNAME = 'GETPAR'   ! procedure name
      CHARACTER( 256 ) :: MESG = ' '

C-----------------------------------------------------------------------

C   Original implementation ............................

C (ZEN was originally an argument, but replaced by COS(ZEN) = COSZ)

C Assume that PAR = 0 if zenith angle is greater than 87 degrees (1.51844 radians)
C or if solar radiation is zero
C     IF ( ZEN .GE. 1.51844 .OR. RSOLAR .LE. 0.0 ) THEN
C        PARDB  = 0.0
C        PARDIF = 0.0
C        RETURN
C     END IF

C Compute clear sky (aka potential) radiation terms
C     OT    = PRES / 1013.25 / COSZ                ! Atmospheric Optical thickness
C     RDVIS = 600.0 * EXP( -0.185 * OT ) * COSZ    ! Direct visible beam, eqn (1)
C     RFVIS = 0.42 * ( 600.0 - RDVIS ) * COSZ      ! Visible Diffuse, eqn (3)
C     WA    = 1320.0 * 0.077 * ( 2.0 * OT ) ** 0.3 ! water absorption in near-IR, eqn (6)
C     RDIR  = ( 720.0 * EXP( -0.06 * OT ) - WA ) * COSZ ! Direct beam near-IR, eqn (4)
C     RFIR  = 0.65 * ( 720.0 - WA - RDIR ) * COSZ  ! Diffuse near-IR, eqn (5)

C     RVT   = RDVIS + RFVIS            ! Total visible radiation, eqn (9)
C     RIRT  = RDIR + RFIR              ! Total near-IR radiation, eqn (10) 
C     FVIS  = RVT / ( RIRT + RVT )     ! Fraction of visible to total radiation, eqn 7
C     RATIO = RSOLAR / ( RIRT + RVT )  ! Ratio of "actual" to clear sky solar radiation

C Compute fraction of visible that is direct beam
C     IF ( RATIO .GE. 0.89 ) THEN
C        FVB = RDVIS / RVT * 0.941124
C     ELSE IF ( RATIO .LE. 0.21 ) THEN
C        FVB = RDVIS / RVT * 9.55E-3
C     ELSE
C        FVB = RDVIS / RVT * ( 1.0 - ( ( 0.9 - RATIO ) / 0.7 ) ** 0.666667 )
C     END IF
C     FVD = 1.0 - FVB

C Compute PAR (direct beam and diffuse) in umol/m2-sec
C     PARDB  = RSOLAR * FVIS * FVB * WATT2UMOL
C     PARDIF = RSOLAR * FVIS * FVD * WATT2UMOL      

C   New implementation .................................

      IF ( COSZ .LE. 0.052336 .OR. RSOLAR .LE. 0.0 ) THEN
         PARDB  = 0.0
         PARDIF = 0.0
         RETURN
      END IF

      OT = PRES / 101325.0 / COSZ   ! PRES in Pa
      A = 600.0 * EXP( -0.185 * OT )
      WA = 125.1335182 * ( OT ) ** 0.3
      B = 720.0 * EXP( -0.06 * OT ) - WA
      C = ( 1.0 - 0.42 * COSZ ) * A
      DEN = 720.0 + C - 0.65 * WA + ( 1.0 - 0.65 * COSZ ) * B
      RATIO = RSOLAR / DEN / COSZ
      IF ( RATIO .GE. 0.89 ) THEN
         FAC = 0.941124
      ELSE IF ( RATIO .LE. 0.21 ) THEN
         FAC = 9.55E-3
      ELSE
         FAC = 1.0 - ( 1.42857143 * ( 0.9 - RATIO ) ) ** 0.666667
      END IF
      RW = RATIO * WATT2UMOL * COSZ
      PARDB = RW * FAC * A
      PARDIF = RW * ( 252.0 + C ) - PARDB

      RETURN 

      END SUBROUTINE GETPARB
