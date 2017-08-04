
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
C $Header: /project/yoj/arc/ICON/src/common/ngc_spc_map.F,v 1.2 2011/10/21 16:41:52 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE NGC_SPC_MAP( LOGUNIT, NSPCS_IN, N_GC_IN, NSPCS_OUT, 
     &                        SPMAP, INFL_SP_NAME, SPNAME_OUT )

C*************************************************************************
 
C Function: Load arrays for non-gas phase species that are used
C           to compute ICs from the IC input file(s)         
              
C Preconditions: None
  
C Key Subroutines/Functions Called: None  
 
C Revision History:
C    Prototype created by Jerry Gipson, January, 1998
C    27 May 06 J.Young: error if no NSPCS_OUT
C    06 Jun 11 J.Young: Replaced I/O API include files with UTILIO_DEFN
C    13 Jul 11 J.Young: Convert for Namelist redesign
C    21 May 12 J.Young: Replaced IC_PARMS include file with an F90 module
 
C*************************************************************************

      USE UTILIO_DEFN
      USE CGRID_SPCS   ! CGRID mechanism species
      USE IC_PARMS     ! ICON parameters

      IMPLICIT NONE     

C Include Files:

C Arguments:
      INTEGER :: LOGUNIT        ! Unit number for output log
      INTEGER :: NSPCS_IN       ! No. of species in the IC input file(s)
      INTEGER :: N_GC_IN        ! No. of GC species in the IC input file(s)
      INTEGER :: NSPCS_OUT      ! Number of IC species on output file(s)
      INTEGER :: SPMAP( * )     ! Map to CTM species from IC species
      CHARACTER( 16 ) :: INFL_SP_NAME( * )  ! Species names on input file
      CHARACTER( 16 ) :: SPNAME_OUT( * )    ! Output file species names

C Parameters: None

C External Functions: 
      INTEGER, EXTERNAL :: FINDEX  
      
C Local Variables:
      CHARACTER( 16 ) :: PNAME = 'NGC_SPC_MAP'
      CHARACTER( 80 ) :: MSG = ' '

      INTEGER ICBC_INDX            ! Index for ICBC NML surrogate name
      INTEGER INDX_SPC             ! Index for NML species name in input file
      INTEGER INDX_SUR             ! Index for NML surrogate name in input file
      INTEGER N                    ! Loop index
      INTEGER NSP                  ! Output species counter

C**********************************************************************

      NSP = N_GC_IN   ! start with GC species
      IF ( NSP .LE. 0 ) THEN
         MSG = 'No gas phase species!'
         CALL M3WARN( PNAME, 0, 0, MSG )
      END IF

c Aerosol species
      DO N = 1, N_AE_SPC

         INDX_SPC = INDEX1( AE_SPC( N ), NSPCS_IN, INFL_SP_NAME )

         ICBC_INDX = FINDEX( N, N_AE_ICBC, AE_ICBC_MAP )
         IF ( ICBC_INDX .NE. 0 ) THEN
            INDX_SUR = INDEX1( AE_ICBC( ICBC_INDX ), NSPCS_IN, INFL_SP_NAME )
         ELSE
            INDX_SUR = 0
         END IF

         IF ( INDX_SPC .GT. 0 .OR. INDX_SUR .GT. 0 ) THEN
            NSP = NSP + 1
            IF ( INDX_SUR .GT. 0 ) THEN
               SPMAP( NSP ) = INDX_SUR
            ELSE
               SPMAP( NSP ) = INDX_SPC
            END IF
            SPNAME_OUT( NSP ) = INFL_SP_NAME( SPMAP( NSP ) )
         END IF

      END DO    
         
c Nonreactive species
      DO N = 1, N_NR_SPC

         INDX_SPC = INDEX1( NR_SPC( N ), NSPCS_IN, INFL_SP_NAME )

         ICBC_INDX = FINDEX( N, N_NR_ICBC, NR_ICBC_MAP )
         IF ( ICBC_INDX .NE. 0 ) THEN
            INDX_SUR = INDEX1( NR_ICBC( ICBC_INDX ), NSPCS_IN, INFL_SP_NAME )
         ELSE
            INDX_SUR = 0
         END IF

         IF ( INDX_SPC .GT. 0 .OR. INDX_SUR .GT. 0 ) THEN
            NSP = NSP + 1
            IF ( INDX_SUR .GT. 0 ) THEN
               SPMAP( NSP ) = INDX_SUR
            ELSE
               SPMAP( NSP ) = INDX_SPC
            END IF
            SPNAME_OUT( NSP ) = INFL_SP_NAME( SPMAP( NSP ) )
         END IF

      END DO    

c Tracer species
      DO N = 1, N_TR_SPC

         INDX_SPC = INDEX1( TR_SPC( N ), NSPCS_IN, INFL_SP_NAME )

         ICBC_INDX = FINDEX( N, N_TR_ICBC, TR_ICBC_MAP )
         IF ( ICBC_INDX .NE. 0 ) THEN
            INDX_SUR = INDEX1( TR_ICBC( ICBC_INDX ), NSPCS_IN, INFL_SP_NAME )
         ELSE
            INDX_SUR = 0
         END IF

         IF ( INDX_SPC .GT. 0 .OR. INDX_SUR .GT. 0 ) THEN
            NSP = NSP + 1
            IF ( INDX_SUR .GT. 0 ) THEN
               SPMAP( NSP ) = INDX_SUR
            ELSE
               SPMAP( NSP ) = INDX_SPC
            END IF
            SPNAME_OUT( NSP ) = INFL_SP_NAME( SPMAP( NSP ) )
         END IF

      END DO   

      NSPCS_OUT = NSP

      IF ( NSPCS_OUT .LE. 0 ) THEN
         MSG = 'No species for output!'
         CALL M3EXIT( PNAME, 0, 0, MSG, 1 )
      END IF

      RETURN

      END 
