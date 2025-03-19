!------------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in           !
!  continuous development by various groups and is based on information        !
!  from these groups: Federal Government employees, contractors working        !
!  within a United States Government contract, and non-Federal sources         !
!  including research institutions.  These groups give the Government          !
!  permission to use, prepare derivative works of, and distribute copies       !
!  of their work in the CMAQ system to the public and to permit others         !
!  to do so.  The United States Environmental Protection Agency                !
!  therefore grants similar permission to use the CMAQ system software,        !
!  but users are requested to provide copies of derivative works or            !
!  products designed to operate in the CMAQ system to the United States        !
!  Government without restrictions as to use by others.  Software              !
!  that is used with the CMAQ system but distributed under the GNU             !
!  General Public License or the GNU Lesser General Public License is          !
!  subject to their copyright restrictions.                                    !
!------------------------------------------------------------------------------!

SUBROUTINE init_io

!-------------------------------------------------------------------------------
! Name:     Initializes I/O API
! Purpose:  Initialize I/O API.
! Revised:  18 Aug 2004  Original version.  (T. Otte)
!           08 Mar 2005  Enabled/mandated use of I/O API 3 or beyond.  (T. Otte)
!           30 Aug 2011  Replaced modules IODECL3 and PARMS3 with I/O API
!                        module M3UTILIO.  Removed call to I/O API utility
!                        routine IOPARMS3.  Mandates use of I/O API 3.1 or
!                        beyond.  Deleted unused variables.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE date_time
  USE m3utilio

  IMPLICIT NONE

  INTEGER                      :: funit

!-------------------------------------------------------------------------------
! Initialize I/O API.
!-------------------------------------------------------------------------------

  funit = init3()

!-------------------------------------------------------------------------------
! Initialize current date and time to missing values.
!-------------------------------------------------------------------------------

  sdate = imiss3
  stime = imiss3

END SUBROUTINE init_io
