! Version 1.0, January 2004 for Sep 2003 release of CMAQ (PKK, AER)
! Additional updates: February 2004 for V1601 SCICHEM (PKK, AER)
      MODULE INTERFACE_DEFINITIONS

         INTERFACE

            SUBROUTINE C_RELEASE( ISTEP, DTS , LEV1 , LEV2 , CGRID )
            IMPLICIT NONE
            INTEGER :: ISTEP, LEV1, LEV2
            REAL :: DTS
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE C_RELEASE

            LOGICAL FUNCTION CHKGRD_MERGE( X, Y, RXX, RYY, P, CGRID )
            USE STRUCT_INC
            IMPLICIT NONE
            REAL :: X, Y, RXX, RYY
            TYPE ( PUFF_STR ) P  !PUFF STRUCTURE
            REAL :: CGRID( :,:,:,: )
            END FUNCTION CHKGRD_MERGE

            SUBROUTINE CREATE_STATIC_PUFFS( ICREL, TSTATIC, T_SRF, DT0, CGRID )
            IMPLICIT NONE
            INTEGER :: ICREL
            REAL :: TSTATIC, T_SRF, DT0
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE CREATE_STATIC_PUFFS

            SUBROUTINE DUMP_PIG( P, CGRID ) 
            USE STRUCT_INC
            IMPLICIT NONE
            TYPE ( PUFF_STR ) P  !PUFF STRUCTURE
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE DUMP_PIG

            SUBROUTINE GET_AMB( X, Y, Z, TDUM, CGRID )
            IMPLICIT NONE
            REAL :: X, Y, Z, TDUM
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE GET_AMB

            SUBROUTINE GET_PUFF_VAL(IS,P,CGRID)
            USE STRUCT_INC
            IMPLICIT NONE
            INTEGER :: IS
            TYPE ( PUFF_STR ) P  !PUFF STRUCTURE
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE GET_PUFF_VAL

            SUBROUTINE GET_SAMP_VAL_ALLPUFFS(IS,CGRID)
            IMPLICIT NONE
            INTEGER :: IS
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE GET_SAMP_VAL_ALLPUFFS

            SUBROUTINE INITIAL( LFLAG, STDATE, STTIME, TSTEP, TENDHR, CGRID )
            IMPLICIT NONE
            LOGICAL :: LFLAG   !Restart flag from host model
            INTEGER :: STDATE  !Start date, coded YYYYDDD
            INTEGER :: STTIME  !Start time, coded HHMMSS
            INTEGER :: TSTEP   !Model time step,  coded HHMMSS
            REAL :: TENDHR     !Final hour of simulation (relative to start)
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE INITIAL

            SUBROUTINE INIT_PIG_INPUT( STDATE, STTIME, TENDHR, CGRID )
            IMPLICIT NONE
            INTEGER :: STDATE  !Start date, coded YYYYDDD
            INTEGER :: STTIME  !Start time, coded HHMMSS
            REAL :: TENDHR     !Final hour of simulation (relative to start)
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE INIT_PIG_INPUT

            SUBROUTINE INIT_PUFF_VAL_PIG(P,CGRID)
            USE STRUCT_INC
            IMPLICIT NONE
            TYPE ( PUFF_STR ) P  !PUFF STRUCTURE
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE INIT_PUFF_VAL_PIG

            SUBROUTINE INIT_STATIC_PUFFS( ICREL, T_INIT, DTS, CGRID )
            IMPLICIT NONE
            INTEGER :: ICREL
            REAL :: T_INIT, DTS
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE INIT_STATIC_PUFFS

            SUBROUTINE INTER( ILEV, JLEV, CGRID )
            IMPLICIT NONE
            INTEGER :: ILEV,JLEV
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE INTER

            SUBROUTINE MERGE( CGRID )
            IMPLICIT NONE
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE MERGE

            SUBROUTINE OUTPUT( CGRID )
            IMPLICIT NONE
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE OUTPUT

            SUBROUTINE OUTPUT_ALL( CGRID )
            IMPLICIT NONE
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE OUTPUT_ALL

            SUBROUTINE RESET_CSTAR( P, IPUF, CGRID )
            USE STRUCT_INC
            IMPLICIT NONE
            TYPE ( PUFF_STR ) P  !PUFF STRUCTURE
            INTEGER :: IPUF
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE RESET_CSTAR

            SUBROUTINE SET_AMB_KEQM( XAMB, YAMB, ZAMB, CGRID )
            IMPLICIT NONE
            REAL :: XAMB , YAMB , ZAMB
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE SET_AMB_KEQM

            SUBROUTINE SET_CGRID( CGRID )
            IMPLICIT NONE
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE SET_CGRID

            SUBROUTINE SET_CGRID_EQM( CGRID )
            IMPLICIT NONE
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE SET_CGRID_EQM

            SUBROUTINE SETALL_AMB_KEQM( CGRID )
            IMPLICIT NONE
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE SETALL_AMB_KEQM

            SUBROUTINE SPLIT( IPUF, LSPLIT, CGRID )
            IMPLICIT NONE
            INTEGER :: IPUF
            LOGICAL :: LSPLIT
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE SPLIT

            SUBROUTINE STEP_AMB( DT, CGRID )
            IMPLICIT NONE
            REAL :: DT
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE STEP_AMB

            SUBROUTINE STEP_P(DT,P,IPUF,LEV1,LEV2,NCHG,FAC_SRF,FAC_DIAG,CGRID)
            USE STRUCT_INC
            IMPLICIT NONE
            INTEGER :: IPUF, LEV1, LEV2, NCHG
            REAL :: DT, FAC_SRF, FAC_DIAG
            REAL :: CGRID( :,:,:,: )
            TYPE ( PUFF_STR ) P  !PUFF STRUCTURE
            END SUBROUTINE STEP_P

            SUBROUTINE STEP_TIME( CGRID )
            IMPLICIT NONE
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE STEP_TIME

            SUBROUTINE TMERGE( ILEV, JLEV, CGRID )
            IMPLICIT NONE
            INTEGER :: ILEV,JLEV
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE TMERGE

            SUBROUTINE UPDATE_MET( JDATE, JTIME, TSTEP, DEPV, WVEL )
            IMPLICIT NONE
            INTEGER         JDATE
            INTEGER         JTIME
            INTEGER         TSTEP
            REAL, INTENT(IN) :: DEPV( :,:,: )
            REAL, INTENT(IN) :: WVEL( :,:,: )
            END SUBROUTINE UPDATE_MET

            SUBROUTINE WRITE_AMB_DATA( CGRID )
            IMPLICIT NONE
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE WRITE_AMB_DATA

            SUBROUTINE WRITE_SMP( CGRID )
            IMPLICIT NONE
            REAL :: CGRID( :,:,:,: )
            END SUBROUTINE WRITE_SMP

         END INTERFACE

      END MODULE
