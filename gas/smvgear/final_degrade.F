
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/gas/smvgear/final_degrade.F,v 1.2 2011/10/20 13:21:59 sjr Exp $

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

      REAL( 8 ),   INTENT( INOUT ) ::  CBLK( BLKSIZE, NSPCSD ) ! species conc in cell

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
