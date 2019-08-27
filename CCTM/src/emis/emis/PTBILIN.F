
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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE PTBILIN

C-----------------------------------------------------------------------
C Function: 3d point source bi-linear interpolation data

C Revision History:
C     20 Nov 2007 J.Young: initial implementation
C     13 Apr 2010 D. Wong: parallel bug fix - in subroutine ungridb2
C                          calculations use the lower left corner of the
C                          original domain
C     27 Jul 2011 David Wong: - fixed the description of XBND
C     12 Aug 2015 David Wong: - Based on the condition of MY_NSRC ( > 0 )
C                               to determine execution of code section
C      1 Feb 19 David Wong: removed all MY_N clauses
C      5 Aub 19 David Wong: removed BILIN_DATA
C-----------------------------------------------------------------------
      USE RUNTIME_VARS

      IMPLICIT NONE

C Arrays which record .true. if local processor contains an exterior boundary
      REAL( 8 )     :: METCXORIG   ! cross point grid X-coord origin of met grid
      REAL( 8 )     :: METCYORIG   ! cross point grid Y-coord origin of met grid
      REAL( 8 )     :: XCELLCG     ! cross point grid X-coordinate cell dimension
      REAL( 8 )     :: YCELLCG     ! cross point grid Y-coordinate cell dimension
      REAL( 8 )     :: METDXORIG   ! dot point grid X-coord origin of met grid
      REAL( 8 )     :: METDYORIG   ! dot point grid Y-coord origin of met grid
      REAL( 8 )     :: XCELLDG     ! dot point grid X-coordinate cell dimension
      REAL( 8 )     :: YCELLDG     ! dot point grid Y-coordinate cell dimension
      REAL( 8 )     :: MY_CXORIG   ! local processor cross point X-coord origin
      REAL( 8 )     :: MY_CYORIG   ! local processor cross point Y-coord origin
      REAL( 8 )     :: MY_DXORIG   ! local processor dot point X-coord origin
      REAL( 8 )     :: MY_DYORIG   ! local processor dot point Y-coord origin

C Allocatable cross- OR dot-point meteorology input buffers

      END MODULE PTBILIN
