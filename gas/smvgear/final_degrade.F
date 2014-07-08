
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
C $Header: /project/yoj/arc/CCTM/src/gas/smvgear/final_degrade.F,v 1.4 2011/10/21 16:11:12 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE FINAL_DEGRADE( CBLK )
C**********************************************************************
C
C  FUNCTION: Update CBLK concentrations with concentrations from degrade
C            routines
C
C  CALLED BY: GRDRIVER or RBDRIVER
C
C  REVISION HISTORY: 07/29/05 : B.Hutzell - Initial version
C                    09/30/11 : B.Hutzell - added CYCLE statements to allow 
C                               optional degraded species i.e., RXTANT_MAP( I )
C                               is less than zero
C
C
C**********************************************************************

      USE DEGRADE_SETUP_TOX

      IMPLICIT NONE

C.....ARGUMENTS:

      REAL( 8 ),   INTENT( INOUT ) ::  CBLK( :, : ) ! species conc in cell

C.....LOCAL VARIABLES:

      REAL, PARAMETER ::  CONMIN = 1.0E-30

      INTEGER         ::  I_RXT, I_PROD     ! indices
      INTEGER         ::  I, J, K, I_CELL   ! loop counters

C**********************************************************************

      LOOP_BLOCK: DO I_CELL = 1, NUM_CELLS

         LOOP_REACT: DO I = 1, N_REACT  ! Loop over each reactant

c..update CBLK

            I_RXT = RXTANT_MAP( I )

            IF( I_RXT < 0 )CYCLE LOOP_REACT

            CBLK( I_CELL, I_RXT ) = CURR_CONC( I_CELL, I_RXT )

            LOOP_PROD: DO J = 1, N_PROCESSES

               I_PROD = PROD_MAP( I, J )

               IF( I_PROD < 1 ) CYCLE   ! no specified product

               CBLK( I_CELL, I_PROD ) = CURR_CONC( I_CELL, I_PROD )

            ENDDO LOOP_PROD

         ENDDO LOOP_REACT

      ENDDO LOOP_BLOCK

      END SUBROUTINE FINAL_DEGRADE
