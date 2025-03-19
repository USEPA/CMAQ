
!-----------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in    !
!  continuous development by various groups and is based on information !
!  from these groups: Federal Government employees, contractors working !
!  within a United States Government contract, and non-Federal sources  !
!  including research institutions.  These groups give the Government   !
!  permission to use, prepare derivative works of, and distribute copies!
!  of their work in the CMAQ system to the public and to permit others  !
!  to do so.  The United States Environmental Protection Agency         !
!  therefore grants similar permission to use the CMAQ system software, !
!  but users are requested to provide copies of derivative works or     !
!  products designed to operate in the CMAQ system to the United States !
!  Government without restrictions as to use by others.  Software       !
!  that is used with the CMAQ system but distributed under the GNU      !
!  General Public License or the GNU Lesser General Public License is   !
!  subject to their copyright restrictions.                             !
!-----------------------------------------------------------------------!


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/JPROC/src/driver/jproc_table/setair.f,v 1.4 2011/10/29 01:03:55 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)setair.F	1.1 /project/mod3/JPROC/src/driver/jproc_table/SCCS/s.setair.F 23 May 1997 12:44:28

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SETAIR ( NWL, MIDWL, HAIR, OMRAY, GRAY, ARAYL )

C*********************************************************************
C
C  Specify air/Rayleigh parameters
C    HAIR      = air scale height, used to estimate ozone density
C                   column a upper boundary (50km).
C    OMRAY     = single scattering albedo, Rayleigh.  Use 1.00
C    GRAY      =  asymetry factor for Rayleigh scattering.  Use 0.0
C    ARAYL(KL) = Rayleigh scattering cross section, from
C                Frohlich and Shaw, Appl.Opt. v.11, p.1773 (1980).
C                overrides tabulation of JDATA.BASE
C
C*********************************************************************

      IMPLICIT NONE

      INCLUDE 'JVALPARMS.EXT'    ! jproc parameters

C...........ARGUMENTS and their descriptions

      INTEGER      NWL                ! number of wavelength bands

      REAL         HAIR               ! air scale height
      REAL         OMRAY              ! single scat albedo, Rayleigh
      REAL         GRAY               ! asymetry fact for Rayleigh scat
      REAL         ARAYL ( MXWL )     ! Rayleigh scat cross section
      REAL         MIDWL ( MXWL )     ! wavelength band midpoints

C...........LOCAL VARIABLES and their descriptions:

      INTEGER      IWL                ! wavelength index

      REAL         XX                 ! intermediate var
      REAL         WMICRON            ! wavelength in microns

C*********************************************************************
C     begin body of subroutine SETAIR

      HAIR  = 8.05
      OMRAY = 1.0
      GRAY  = 0.0

      DO IWL = 1, NWL
        WMICRON = MIDWL( IWL ) / 1.0E3
        XX = 3.916 + 0.074 * WMICRON + 0.050 / WMICRON
        ARAYL( IWL ) = 3.90E-28 / WMICRON**XX
      END DO

      RETURN
      END
