
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

C....................................................................
C  The ENV_VARS module contains file unit identifiers for the log files,
C  input files, and the values of all environment variables.
C
C  It also contains routines for reading environment variables, and
C  opening input files.
C....................................................................

      MODULE ENV_VARS

      IMPLICIT NONE

      SAVE

      INTEGER :: LOGDEV    ! File Unit for Ascii Log File
      INTEGER :: DIAGDEV   ! File Unit For Ascii Diagnostic Output

      INTEGER, PARAMETER :: CTM_DIAG_LVL = 1
      
      PUBLIC LOGDEV, DIAGDEV, CTM_DIAG_LVL

      PRIVATE

      END MODULE ENV_VARS
