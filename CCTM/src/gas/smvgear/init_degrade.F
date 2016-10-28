
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
C $Header: /project/yoj/arc/CCTM/src/gas/smvgear/init_degrade.F,v 1.5 2011/10/21 16:11:15 yoj Exp $

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

      USE RXNS_DATA
      USE DEGRADE_SETUP_TOX   ! inherits CGRID_SPCS

      IMPLICIT NONE


C.....ARGUMENTS:

      REAL( 8 ), INTENT( IN ) :: CBLK ( :, : )           !  species concentration in cell
      REAL( 8 ), INTENT( IN ) :: TCELL( : )              !  cell temperature  [ k ]
      REAL( 8 ), INTENT( IN ) :: DCELL( : )              !  cell air density  [ kg/m^3 ]
      REAL( 8 ), INTENT( IN ) :: PHOTO_CELL( :, : )     !  Photolysis table for cell [1/s]

      INTEGER,   INTENT( IN ) :: NUMCELLS  ! number of grid cells in block
      INTEGER,   INTENT( IN ) :: JDATE     ! current model date , coded YYYYDDD
      INTEGER,   INTENT( IN ) :: JTIME     ! current model time , coded HHMMSS
      INTEGER,   INTENT( IN ) :: LOGDEV    ! unit number for the log
      INTEGER,   INTENT( IN ) :: BLKID     ! number for the block

C.....LOCAL VARIABLES:

      CHARACTER( 144 )        :: XMSG       ! Message text
      CHARACTER( 16  ), SAVE  :: PNAME = 'INIT_DEGRADE' ! Routine name

      REAL(8), SAVE ::  CONV_M2N       ! factor to convert ppm times mass density in [kg/m^3]
                                       ! into number density in [molecules/cm^3]
      REAL(8), SAVE :: MASS_TO_NUMBER  ! air mass density to number density [ (# per moles)/Kg ]

      INTEGER       :: I, J, K, I_CELL   ! loop counters

      LOGICAL, SAVE ::  FIRSTCALL
      DATA              FIRSTCALL / .TRUE. /

C**********************************************************************

      IF ( FIRSTCALL ) THEN  ! initialize constants and allocate arrays

         CONV_M2N       = 1.0D-9 * REAL( (AVO / MWAIR), 8 )       ! AVO and MWAIR defined in CONST.EXT 

         MASS_TO_NUMBER = REAL( 1.0E-3*AVO / MWAIR, 8 )

         ALLOCATE( PREV_CONC( BLKSIZE, NSPCSD ) )
         ALLOCATE( CURR_CONC( BLKSIZE, NSPCSD ) )
         ALLOCATE( DELT_CONC( BLKSIZE, NSPCSD ) )
         ALLOCATE( NUMB_DENS( BLKSIZE ) )

         ALLOCATE(      TEMP( BLKSIZE ),
     &              INV_TEMP( BLKSIZE ),
     &             CONV_FACT( BLKSIZE ))

         FIRSTCALL = .FALSE.

      ENDIF

      NUM_CELLS = NUMCELLS

      PREV_CONC  = 0.0D0
      CURR_CONC  = 0.0D0
      RATE_CONST = 0.0D0
      DELT_CONC  = 0.0D0

C.. initialize concentrations and their changes

          DO J = 1, NSPCSD
             DO I_CELL = 1, NUMCELLS
                PREV_CONC( I_CELL, J ) = MAX( CBLK( I_CELL, J ), 0.0D0)
                CURR_CONC( I_CELL, J ) = PREV_CONC( I_CELL, J )
             END DO
         ENDDO

         DO I_CELL = 1, NUMCELLS
            NUMB_DENS( I_CELL ) = MASS_TO_NUMBER * DCELL( I_CELL )
            TEMP( I_CELL )      = REAL( TCELL( I_CELL ) )
            INV_TEMP( I_CELL )  = 1.0D0 / TCELL( I_CELL )
            CONV_FACT( I_CELL ) = CONV_M2N * DCELL( I_CELL )
         END DO


         LOOP_REACT: DO I = 1, N_REACT ! calculated rate constants

            IF( RXTANT_MAP( I ) < 0 )CYCLE LOOP_REACT


            LOOP_UNIRATE: DO J = 1, N_UNI_LOSS
               DO I_CELL = 1, NUMCELLS
                RATE_CONST( I_CELL, I, J ) = UNIRATE( I, J ) 
     &                                     * TCELL( I_CELL )**UNI_TEXP( I, J )
     &                                     * DEXP( -UNI_ACT( I, J )*INV_TEMP( I_CELL ) )
               END DO
            ENDDO LOOP_UNIRATE

            LOOP_BIRATE: DO J = 1, N_BI_LOSS
               DO I_CELL = 1, NUMCELLS
                 RATE_CONST( I_CELL, I, (J+UNI_STOP) ) = CONV_FACT( I_CELL ) * BIRATE( I, J ) 
     &                                                 * TCELL( I_CELL )**BI_TEXP( I, J )
     &                                                 * DEXP( -BI_ACT( I, J )*INV_TEMP( I_CELL ) )
 
              END DO
            ENDDO LOOP_BIRATE

            LOOP_TRIRATE: DO J = 1, N_TRI_LOSS
              DO I_CELL = 1, NUMCELLS 
               RATE_CONST( I_CELL, I, (J+BI_STOP) ) = CONV_FACT( I_CELL ) * CONV_FACT( I_CELL )
     &                                              * TRIRATE( I, J ) * TCELL( I_CELL )**TRI_TEXP( I, J )
     &                                              * DEXP( -TRI_ACT( I, J )*INV_TEMP( I_CELL ) )
              END DO
            ENDDO LOOP_TRIRATE

            LOOP_PHOTORATE: DO J = 1, N_PHOTO_LOSS
              DO I_CELL = 1, NUMCELLS
                K = PHOTO_MAP( I, J )
                IF ( K < 1 ) CYCLE
                RATE_CONST( I_CELL, I, (J+TRI_STOP) ) = A_PHOTO( I, J ) * PHOTO_CELL( I_CELL, K )
              END DO
            ENDDO LOOP_PHOTORATE

         ENDDO LOOP_REACT

      RETURN

      END SUBROUTINE INIT_DEGRADE
