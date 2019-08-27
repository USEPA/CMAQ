       MODULE CSQY_PARAMETERS

#ifndef mech_includes
        USE RXNS_DATA
#endif	   

        IMPLICIT NONE


#ifdef mech_includes
           INCLUDE SUBST_RXCMMN    ! to get mech name
           INCLUDE SUBST_RXDATA    ! to get photolysis rate names
#endif	   

!    MXWL     = maximum number of wavelength bands to process
!    MXWLIN   = maximum number of wavelength bands on input files
!    NJPROC   = maximum number of vertical levels in radiative transfer
!               calculation
!    MXLEV    = maximum number of vertical levels in base atmospheric 
!               structure

          INTEGER, PARAMETER :: MXWL   = 601
          INTEGER, PARAMETER :: MXWLIN = 2000
          INTEGER, PARAMETER :: NJPROC = 200
          INTEGER, PARAMETER :: MXLEV  =  51

! max number of altitude levels used in temp and pressure routine for cross-sections and
! quantum yield
          INTEGER, PARAMETER :: KZ = 125
! max number of wavelength used in temp and pressure routine for cross-sections and
! quantum yield
          INTEGER, PARAMETER :: KW = MXWL ! N_INLINE_BAND ! MXWL
! time/sza
          INTEGER, PARAMETER :: KT = MXLEV	

          REAL,    PARAMETER :: T298K = 298.0     ! standard air temperature
          REAL,    PARAMETER :: DENS0 = 2.46E+19  ! approximate air number density at 1 Atm and 298K, molec/cm3 
                                                  ! should be 2.465E+19 but the original version of CSQY pre-processor
                                                  ! used 2.59E+19, a 3% difference


          REAL               :: Z_JPROC(    MXLEV )      ! altitudes for JPROC level, km
          REAL               :: O3_JPROC(   MXLEV )      ! JPROC ozone profile
          REAL               :: T_JPROC(    MXLEV )      ! JPROC temperature profile
          REAL               :: DENS_JPROC(   MXLEV )    ! JPROC number density profile, cm^-3

          LOGICAL, PARAMETER :: USE_JPROC = .TRUE.

! all rate listed in CB05, SAPRC99, SAPRC07T, plus seven additional rates 
! that copy SAPRC07T and SAPRC99 rates. Why? Rates hardwired in PHOT_MOD.F
! multiple occurrances will be eliminated in CREATE_MODULE subroutine

          INTEGER, PARAMETER :: NPHOTAB_ALL =  94
          CHARACTER( 16 )    :: PHOTAB_ALL( NPHOTAB_ALL )
          CHARACTER( 32 )    :: MECHNAME_ALL = 'ALL_MECHANISM'

          INTEGER            :: IRXXN_ALL

          DATA ( PHOTAB_ALL( IRXXN_ALL ), IRXXN_ALL = 1, NPHOTAB_ALL ) /
     &   'NO2-06          ', 'NO3NO-06        ', 'NO3NO2-6        ',
     &   'O3O1D-06        ', 'O3O3P-06        ', 'HONO-06         ',
     &   'HNO3            ', 'HNO4-06         ', 'H2O2            ',
     &   'NO2EX           ', 'PAN             ', 'HCHOR-06        ',
     &   'HCHOM-06        ', 'CCHO_R          ', 'C2CHO           ',
     &   'ACET-06         ', 'MEK-06          ', 'COOH            ',
     &   'GLY-07R         ', 'GLY-07M         ', 'MGLY-06         ',
     &   'BACL-07         ', 'BALD-06         ', 'AFG1            ',
     &   'MACR-06         ', 'MVK-06          ', 'IC3ONO2         ',
     &   'HOCCHO_IUPAC    ', 'ACRO-09         ', 'PAA             ',
     &   'CL2             ', 'CLNO-06         ', 'CLONO           ',
     &   'CLNO2           ', 'CLONO2-1        ', 'CLONO2-2        ',
     &   'HOCL-06         ', 'CLCCHO          ', 'CLACET          ',
     &   'NO2_SAPRC99     ', 'NO3NO_SAPRC99   ', 'NO3NO2_SAPRC99  ',
     &   'O3O3P_SAPRC99   ', 'O3O1D_SAPRC99   ', 'HONO_NO_SAPRC99 ',
     &   'HONO_NO2_SAPRC99', 'HNO3_SAPRC99    ', 'HO2NO2_SAPRC99  ',
     &   'H2O2_SAPRC99    ', 'HCHO_R_SAPRC99  ', 'HCHO_M_SAPRC99  ',
     &   'CCHO_R_SAPRC99  ', 'C2CHO_SAPRC99   ', 'ACETONE_SAPRC99 ',
     &   'KETONE_SAPRC99  ', 'COOH_SAPRC99    ', 'GLY_R_SAPRC99   ',
     &   'GLY_ABS_SAPRC99 ', 'MGLY_ADJ_SAPRC99', 'BACL_ADJ_SAPRC99',
     &   'BZCHO_SAPRC99   ', 'ACROLEIN_SAPRC99', 'IC3ONO2_SAPRC99 ',
     &   'MGLY_ABS_SAPRC99',
     &   'NO2_SAPRC99     ', 'O3_O3P_IUPAC04  ', 'O3_O1D_IUPAC04  ',
     &   'NO3NO2_SAPRC99  ', 'NO3NO_SAPRC99   ', 'HONO_IUPAC04    ',
     &   'H2O2_SAPRC99    ', 'HO2NO2_IUPAC04  ', 'HNO3_IUPAC04    ',
     &   'N2O5_IUPAC04    ', 'NTR_IUPAC04     ', 'COOH_SAPRC99    ',
     &   'HCHO_R_SAPRC99  ', 'HCHO_M_SAPRC99  ', 'CCHO_R_SAPRC99  ',
     &   'PAN_IUPAC04     ', 'PACD_CB05       ', 'C2CHO_SAPRC99   ',
     &   'MGLY_IUPAC04    ', 'ACROLEIN_SAPRC99', 'CL2_IUPAC04     ',
     &   'HOCL_IUPAC04    ', 'FMCL_IUPAC04    ', 'NO2             ',
     &   'O3O1D           ', 'O3O3P           ', 'KETONE          ',
     &   'MGLY_ABS        ', 'MGLY_ADJ        ', 'ACETONE         ' /

          LOGICAL, PARAMETER  :: USE_TUV_JVALUE  = .TRUE.
          LOGICAL, SAVE       :: SPLIT_OUTPUTS  
 
          INTEGER, SAVE                         :: N_USE_PHOTAB 
          CHARACTER( 16 ), ALLOCATABLE, SAVE    :: USE_PHOTAB( : )

!          DATA ( USE_PHOTAB( IRXXN ), IRXXN = 1, N_USE_PHOTAB ) /
!     &   'BALD-06         ', 'AFG1            ', 'IC3ONO2         ',
!     &   'CLNO-06         ', 'CLONO           ', 'CLNO2           ', 
!     &   'HOCL-06         ', 'CLCCHO          ', 'CLACET          '/

          LOGICAL, ALLOCATABLE, SAVE  :: USE_JPROC_CSQY ( : )
          INTEGER, ALLOCATABLE, SAVE  :: TUV_TO_JPROC( : )

          INTEGER, PARAMETER       :: N_TEMP_STRAT   = 6
          INTEGER, PARAMETER       :: N_TEMPERATURE  = 6

          INTEGER, SAVE            :: MODULE_UNIT = 75
          INTEGER, SAVE            :: JTABLE_UNIT = 105
          INTEGER, SAVE            :: ODATA_UNIT  = 106

          CHARACTER(586)           :: OUT_DIR = 'OUT_DIR'
          CHARACTER(16), SAVE      :: PHOT_DONE( NPHOTAB )
          INTEGER,       SAVE      :: NPHOT_DONE
          LOGICAL,       SAVE      :: PHOT_PROCESS( NPHOTAB )
          REAL, ALLOCATABLE, SAVE  :: WV_FASTJ( : ), XO3_FASTJ( : )

          INTEGER, SAVE            :: CSQY_UNIT  =  76

      CONTAINS
        SUBROUTINE INIT_CSQY_MODULE()

          USE GET_ENV_VARS
          USE BIN_DATA

          IMPLICIT NONE 

          LOGICAL, SAVE  :: FIRSTCALL = .TRUE.
          INTEGER        :: I, J, K, ITTR
          CHARACTER(16)  :: SAFE_NAME( NPHOTAB )
          CHARACTER(16)  :: PHOT_NAME( NPHOTAB )
          CHARACTER(600) :: JTABLE_NAME
          CHARACTER( 32) :: WORD
          CHARACTER(255) :: EQNAME

          IF( FIRSTCALL )THEN
              CALL INIT_BIN_DATA
              FIRSTCALL = .FALSE.
          ELSE       
              RETURN
          ENDIF
          
! get output directory
          EQNAME = 'OUT_DIR'
          CALL VALUE_NAME( EQNAME, OUT_DIR ) 
! create name of CSQY_DATA file
          WORD = MECHNAME
          CALL CONVERT_CASE ( WORD, .FALSE. )
          JTABLE_NAME = TRIM( OUT_DIR ) 
     &               // '/' 
     &               // 'CSQY_DATA_' // TRIM( WORD )


          OPEN(JTABLE_UNIT, FILE = JTABLE_NAME, STATUS = 'UNKNOWN')

          PHOT_DONE   = ' '
          SAFE_NAME   = ' '
          NPHOT_DONE  = 0
          PHOT_PROCESS  = .TRUE.
          ITTR = 0
          
          DO J = 1, NPHOTAB
             DO I = 1, NPHOT_DONE
                IF( PHOTAB(J) .EQ. PHOT_DONE(I) )THEN
                    PHOT_PROCESS( J ) = .FALSE.
                    EXIT
                ENDIF
             ENDDO
             IF( PHOT_PROCESS( J ) )THEN
                 NPHOT_DONE = NPHOT_DONE + 1
                 PHOT_DONE(NPHOT_DONE) = PHOTAB(J)
                 SAFE_NAME(NPHOT_DONE) = PHOT_DONE(NPHOT_DONE)
                 DO I = 1, LEN(SAFE_NAME(NPHOT_DONE))
                    IF( SAFE_NAME(NPHOT_DONE)(I:I) .EQ. '-' ) THEN
                        SAFE_NAME(NPHOT_DONE)(I:I) = '_'
                    ENDIF
                 ENDDO
 
             ELSE
                 ITTR = ITTR + 1
                 WRITE(6,'(I3,1X,A,1X,A,I3,1X,A)')ITTR, TRIM(PHOTAB(J)),
     &                ' already treated by ', I, TRIM(PHOT_DONE(I))
             ENDIF
         ENDDO

1996     format(6X,'INTEGER, PARAMETER :: NPHOT_REF = ',I3,' ! # ref phot reactions ')
1997     format(6X,'INTEGER, PARAMETER :: NTEMP_REF = ',I3,' ! # ref temperatures ')
1998     format(6X,'INTEGER, PARAMETER :: NWL_REF   = ',I3,' ! # ref wavelengths ')

         WRITE(JTABLE_UNIT,'(A22,A32)')'Table for Mechanism = ',MECHNAME
         WRITE(JTABLE_UNIT,'(A9,1X,I4)')'NPHOTAB =',NPHOT_DONE
         WRITE(JTABLE_UNIT,'(A)')'!Individual rates listed below:'    

  
         DO I = 1, NPHOT_DONE

            WRITE(JTABLE_UNIT,'(A16)')PHOT_DONE(I)
1999        FORMAT(6X,'INTEGER, PARAMETER :: I',A16, ' = ', I3, ' ! pointer to ', A16)

         ENDDO

2040     format(6X,'CHARACTER(16), SAVE :: PNAME_REF( NPHOT_REF )')

         DO I = 1, NPHOT_DONE

2000        FORMAT(6X,'DATA PNAME_REF( I',A16, ' ) / ''', A16, ''' /')

         ENDDO

2027     format(6X,'INTEGER, PARAMETER :: NPHOT_MAP = ',I3,' ! #  phot mapped reactions ')
2025     format(6X,'CHARACTER(16), SAVE :: PNAME_MAP( NPHOT_MAP )')
2026     format(6X,'INTEGER, SAVE       :: PHOT_MAP( NPHOT_MAP )')

         DO I = 1, NPHOT_DONE

2010        FORMAT(6X,'DATA PNAME_MAP( ', I3, ' ), ', ' PHOT_MAP( ', I3, ' )  / ''',
     &             A16, ''', I', A16, ' / ')

         ENDDO

2021     format(6X,'REAL, SAVE :: TEMP_REF( NTEMP_REF, NPHOT_REF )    ! reference temperatures')

2022     format(6X,'REAL, SAVE :: CS_REF( NPHOT_REF, NTEMP_REF, NWL_REF ) ! effective cross sections')

2023     format(6X,'REAL, SAVE :: QY_REF( NPHOT_REF, NTEMP_REF, NWL_REF ) ! effective quantum yields')

2024     format(6X,'REAL, SAVE :: ECS_REF( NPHOT_REF, NTEMP_REF, NWL_REF ) ! CS*QY averaged UCI Solar Flux')

2031     format(6X,'REAL, SAVE :: EQY_REF( NPHOT_REF, NTEMP_REF, NWL_REF ) ! eCS/CS averaged 77 bins in UCI Model')

2032     format('C...  quantum yields')
2033     format('C...    effective quantum yields were computed by performing separate')
2034     format('C...    interval integrations for the cross sections and for the') 
2035     format('C...    effective cross sections (cs*qy) (calculated on the finer')
2036     format('C...    wavelength grid.  The effective quantum yield values')
2037     format('C...    were then calculated for the 7 wavelength intervals by ')
2038     format('C...    dividing the effective cross sections by the interval average')
2039     format('C...    cross sections (eQY=eCS/CS).')

         PHOT_DONE   = ' '
         NPHOT_DONE  = 0


2003     format(1x)

          RETURN
        END SUBROUTINE INIT_CSQY_MODULE
        SUBROUTINE INIT_CXQY_MODULE()

          USE GET_ENV_VARS
          USE BIN_DATA

          IMPLICIT NONE 

          LOGICAL, SAVE  :: FIRSTCALL = .TRUE.
          INTEGER        :: I, J, K, ITTR
          CHARACTER(16)  :: SAFE_NAME( NPHOTAB )
          CHARACTER(16)  :: PHOT_NAME( NPHOTAB )
          CHARACTER(600) :: JTABLE_NAME
          CHARACTER(32)  :: WORD
          CHARACTER(255) :: EQNAME
          INTERFACE
           SUBROUTINE CONVERT_CASE ( BUFFER, UPPER )
             CHARACTER(LEN= *), INTENT( INOUT ) :: BUFFER
             LOGICAL,           INTENT( IN    ) :: UPPER
           END SUBROUTINE CONVERT_CASE
         END INTERFACE

          IF( FIRSTCALL )THEN
              CALL INIT_BIN_DATA
              FIRSTCALL = .FALSE.
          ELSE       
              RETURN
          ENDIF

! get output directory
          EQNAME = 'OUT_DIR'
          CALL VALUE_NAME( EQNAME, OUT_DIR ) 
! create name of CSQY_DATA file
          WORD = MECHNAME
          CALL CONVERT_CASE ( WORD, .FALSE. )
          JTABLE_NAME = TRIM( OUT_DIR ) 
     &               // '/' 
     &               // 'CSQY_DATA_' // TRIM( WORD )

          OPEN(JTABLE_UNIT, FILE = JTABLE_NAME, STATUS = 'UNKNOWN')

          PHOT_DONE   = ' '
          SAFE_NAME   = ' '
          NPHOT_DONE  = 0
          PHOT_PROCESS  = .TRUE.
          ITTR = 0

          DO J = 1, NPHOTAB
             DO I = 1, NPHOT_DONE
                IF( PHOTAB(J) .EQ. PHOT_DONE(I) )THEN
                    PHOT_PROCESS( J ) = .FALSE.
                    EXIT
                ENDIF
             ENDDO
             IF( PHOT_PROCESS( J ) )THEN
                 NPHOT_DONE = NPHOT_DONE + 1
                 PHOT_DONE(NPHOT_DONE) = PHOTAB(J)
                 SAFE_NAME(NPHOT_DONE) = PHOT_DONE(NPHOT_DONE)
                 DO I = 1, LEN(SAFE_NAME(NPHOT_DONE))
                    IF( SAFE_NAME(NPHOT_DONE)(I:I) .EQ. '-' ) THEN
                        SAFE_NAME(NPHOT_DONE)(I:I) = '_'
                    ENDIF
                 ENDDO
             ELSE
                 ITTR = ITTR + 1
                 WRITE(6,'(I3,1X,A,1X,A,I3,1X,A)')ITTR, TRIM(PHOTAB(J)),
     &                ' already treated by ', I, TRIM(PHOT_DONE(I))
             ENDIF
         ENDDO

         WRITE(JTABLE_UNIT,'(A22,A32)')'Table for Mechanism = ',MECHNAME
         WRITE(JTABLE_UNIT,'(A9,1X,I4)')'NPHOTAB =',NPHOT_DONE
         WRITE(JTABLE_UNIT,'(A)')'!Individual rates listed below:'    
          

         DO I = 1, NPHOT_DONE
            WRITE(JTABLE_UNIT,'(A16)')PHOT_DONE(I)
         ENDDO

         PHOT_DONE   = ' '
         NPHOT_DONE  = 0

1996     FORMAT(6X,'INTEGER, PARAMETER :: NPHOT_REF = ',I3,' ! # ref phot reactions ')
1997     FORMAT(6X,'INTEGER, PARAMETER :: NTEMP_REF = ',I3,' ! # ref temperatures ')
1998     FORMAT(6X,'INTEGER, PARAMETER :: NWL_REF   = ',I3,' ! # ref wavelengths ')
1999     FORMAT(6X,'INTEGER, PARAMETER :: I',A16, ' = ', I3, ' ! pointer to ', A16)
2040     FORMAT(6X,'CHARACTER(16), SAVE :: PNAME_REF( NPHOT_REF )')
2000     FORMAT(6X,'DATA PNAME_REF( I',A16, ' ) / ''', A16, ''' /')
2027     FORMAT(6X,'INTEGER, PARAMETER :: NPHOT_MAP = ',I3,' ! #  phot mapped reactions ')
2025     FORMAT(6X,'CHARACTER(16), SAVE :: PNAME_MAP( NPHOT_MAP )')
2026     FORMAT(6X,'INTEGER, SAVE       :: PHOT_MAP( NPHOT_MAP )')
2010     FORMAT(6X,'DATA PNAME_MAP( ', I3, ' ), ', ' PHOT_MAP( ', I3, ' )  / ''',
     &             A16, ''', I', A16, ' / ')
2021     FORMAT(6X,'REAL, SAVE :: TEMP_REF( NTEMP_REF, NPHOT_REF )    ! reference temperatures')
2022     FORMAT(6X,'REAL, SAVE :: CS_REF( NPHOT_REF, NTEMP_REF, NWL_REF ) ! effective cross sections')
2023     FORMAT(6X,'REAL, SAVE :: QY_REF( NPHOT_REF, NTEMP_REF, NWL_REF ) ! effective quantum yields')
2024     FORMAT(6X,'REAL, SAVE :: ECS_REF( NPHOT_REF, NTEMP_REF, NWL_REF ) ! CS*QY averaged UCI Solar Flux')
2031     FORMAT(6X,'REAL, SAVE :: EQY_REF( NPHOT_REF, NTEMP_REF, NWL_REF ) ! eCS/CS averaged 77 bins in UCI Model')
2032     FORMAT('C...  quantum yields')
2033     FORMAT('C...    effective quantum yields were computed by performing separate')
2034     FORMAT('C...    interval integrations for the cross sections and for the') 
2035     FORMAT('C...    effective cross sections (cs*qy) (calculated on the finer')
2036     FORMAT('C...    wavelength grid.  The effective quantum yield values')
2037     FORMAT('C...    were then calculated for the 7 wavelength intervals by ')
2038     FORMAT('C...    dividing the effective cross sections by the interval average')
2039     FORMAT('C...    cross sections (eQY=eCS/CS).')
2003     FORMAT(1x)

          RETURN
        END SUBROUTINE INIT_CXQY_MODULE
        
	END MODULE CSQY_PARAMETERS
