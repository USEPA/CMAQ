
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
C $Header: /project/yoj/arc/CCTM/src/gas/ros3/init_degrade.F,v 1.5 2011/10/21 16:11:15 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE INIT_DEGRADE( CBLK, TCELL, DCELL, PHOTO_CELL, NUMCELLS,
     &                         JDATE, JTIME, LOGDEV, BLKID )
C**********************************************************************
C
C  FUNCTION:  Initialize arrays used by degrade routines then load
C             CBLK concentration needed in degrade routines.
C
C  CALLED BY: GRDRIVER or RBDRIVER
C
C  REVISION HISTORY:  07/29/05 : B.Hutzell - Initial version
C                     09/30/11 : B.Hutzell - added CYCLE statements to allow 
C                                optional degraded species i.e., RXTANT_MAP( I )
C                                is less than zero
C
C**********************************************************************

      USE DEGRADE_SETUP_TOX   ! inherits CGRID_SPCS

      IMPLICIT NONE

      INCLUDE SUBST_RXCMMN    ! Mechanism reaction common block

C.....ARGUMENTS:

      REAL( 8 ), INTENT( IN ) :: CBLK ( BLKSIZE, NSPCSD )           !  species concentration in cell
      REAL( 8 ), INTENT( IN ) :: TCELL( BLKSIZE )                    !  cell temperature  [ k ]
      REAL( 8 ), INTENT( IN ) :: DCELL( BLKSIZE )                   !  cell air density  [ kg/m^3 ]
      REAL,      INTENT( IN ) :: PHOTO_CELL( BLKSIZE, N_PHOTO_TAB ) !  Photolysis table for cell [1/s]

      INTEGER,   INTENT( IN ) :: NUMCELLS  ! number of grid cells in block
      INTEGER,   INTENT( IN ) :: JDATE     ! current model date , coded YYYYDDD
      INTEGER,   INTENT( IN ) :: JTIME     ! current model time , coded HHMMSS
      INTEGER,   INTENT( IN ) :: LOGDEV    ! unit number for the log
      INTEGER,   INTENT( IN ) :: BLKID     ! number for the block

C.....LOCAL VARIABLES:

      CHARACTER( 144 )        :: XMSG       ! Message text
      CHARACTER( 16  ), SAVE  :: PNAME = 'INIT_DEGRADE' ! Routine name

      REAL(8), SAVE ::  CONV_M2N      ! factor to convert ppm times mass density in [kg/m^3]
                                      ! into number density in [molecules/cm^3]
      REAL(8)       ::  CONV_FACT     ! conversion factor from ppm to molecules/cm^3
      REAL(8)       ::  INV_TEMP      ! reciprocal of temperature, [K^-1]

      INTEGER       :: I, J, K, I_CELL   ! loop counters

      LOGICAL, SAVE ::  FIRSTCALL
      DATA              FIRSTCALL / .TRUE. /

C**********************************************************************

      IF ( FIRSTCALL ) THEN  ! initialize constants and allocate arrays

         CONV_M2N = 1.0D-9 * AVO / MWAIR       ! AVO and MWAIR defined in CONST.EXT

         ALLOCATE( PREV_CONC( BLKSIZE, NSPCSD ) )
         ALLOCATE( CURR_CONC( BLKSIZE, NSPCSD ) )
         ALLOCATE( DELT_CONC( BLKSIZE, NSPCSD ) )
         ALLOCATE( NUMB_DENS( BLKSIZE ) )

         FIRSTCALL = .FALSE.

      ENDIF

      NUM_CELLS = NUMCELLS

      PREV_CONC  = 0.0D0
      CURR_CONC  = 0.0D0
      RATE_CONST = 0.0D0

      LOOP_BLOCK: DO I_CELL = 1, NUMCELLS

C.. initialize concentrations and their changes

C         DO J = 1, NSPCSD
C            PREV_CONC( I_CELL, J ) = CBLK( I_CELL, J )
C            CURR_CONC( I_CELL, J ) = CBLK( I_CELL, J )
C            DELT_CONC( I_CELL, J ) = 0.0D0
C         ENDDO

         PREV_CONC( I_CELL, 1:NSPCSD ) = CBLK( I_CELL, 1:NSPCSD )
         CURR_CONC( I_CELL, 1:NSPCSD ) = CBLK( I_CELL, 1:NSPCSD )
         DELT_CONC( I_CELL, 1:NSPCSD ) = 0.0D0

         NUMB_DENS( I_CELL ) = 1.0E-3 * REAL( DCELL( I_CELL ) ) / MWAIR
         TEMP = REAL( TCELL( I_CELL ) )

         CONV_FACT = CONV_M2N * DBLE( DCELL( I_CELL ) )
         INV_TEMP = 1.0D0 / TEMP

         LOOP_REACT: DO I = 1, N_REACT ! calculated rate constants

            IF( RXTANT_MAP( I ) < 0 )CYCLE LOOP_REACT

            LOOP_UNIRATE: DO J = 1, N_UNI_LOSS

                RATE_CONST( I_CELL, I, J ) = UNIRATE( I, J )
     &                                     * TEMP**UNI_TEXP( I, J )
     &                                     * EXP( -UNI_ACT( I, J )
     &                                     * INV_TEMP )

            ENDDO LOOP_UNIRATE

            LOOP_BIRATE: DO J = 1, N_BI_LOSS

               RATE_CONST( I_CELL, I, J+UNI_STOP ) = BIRATE( I, J )
     &                                             * CONV_FACT
     &                                             * TEMP**BI_TEXP( I, J )
     &                                             * EXP( -BI_ACT( I, J )
     &                                             * INV_TEMP )

            ENDDO LOOP_BIRATE

            LOOP_TRIRATE: DO J = 1, N_TRI_LOSS

               RATE_CONST( I_CELL, I, J+BI_STOP ) = TRIRATE( I, J )
     &                                            * CONV_FACT
     &                                            * CONV_FACT
     &                                            * TEMP**TRI_TEXP( I, J )
     &                                            * EXP( -TRI_ACT( I, J )
     &                                            * INV_TEMP )

            ENDDO LOOP_TRIRATE

            LOOP_PHOTORATE: DO J = 1, N_PHOTO_LOSS

                K = PHOTO_MAP( I, J )
                IF ( K < 1 ) CYCLE
                RATE_CONST( I_CELL, I, J+TRI_STOP ) = A_PHOTO( I, J )
     &                                              * PHOTO_CELL( I_CELL, K )

            ENDDO LOOP_PHOTORATE

         ENDDO LOOP_REACT

      ENDDO LOOP_BLOCK

      RETURN

      END SUBROUTINE INIT_DEGRADE
