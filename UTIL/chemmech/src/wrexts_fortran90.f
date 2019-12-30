
C***************************************************************************
C  Significant portions of Models-3/CMAQ software were developed by        *
C  Government employees and under a United States Government contract.     *
C  Portions of the software were also based on information from non-       *
C  Federal sources, including software developed by research institutions  *
C  through jointly funded cooperative agreements. These research institu-  *
C  tions have given the Government permission to use, prepare derivative   *
C  works, and distribute copies of their work to the public within the     *
C  Models-3/CMAQ software release and to permit others to do so. EPA       *
C  therefore grants similar permissions for use of Models-3/CMAQ software, *
C  but users are requested to provide copies of derivative works to the    *
C  Government without re-strictions as to use by others.  Users are        *
C  responsible for acquiring their own copies of commercial software       *
C  associated with the Models-3/CMAQ release and are also responsible      *
C  to those vendors for complying with any of the vendors' copyright and   *
C  license restrictions. In particular users must obtain a Runtime license *
C  for Orbix from IONA Technologies for each CPU used in Models-3/CMAQ     *
C  applications.                                                           *
C                                                                          *
C  Portions of I/O API, PAVE, and the model builder are Copyrighted        *
C  1993-1997 by MCNC--North Carolina Supercomputing Center and are         *
C  used with their permissions subject to the above restrictions.          *
C***************************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/MECH/src/driver/mech/WREXTS.f,v 1.6 2001/03/05 19:50:14 yoj Exp $ 

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)WREXTS.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.WREXTS.F 02 Jan 1997 15:26:56

      SUBROUTINE WREXTS_FORTRAN90 ( WRUNIT,
     &                              EQNAME_MECH,
     &                              DESCRP_MECH,
     &                              NS, SPCLIS, SPC1RX,
     &                              NR,
     &                              IP, 
     &                              NAMCONSTS,
     &                              CVAL, SS1RX,
     &                              LITE ) 
      
      USE MECHANISM_DATA
      
      IMPLICIT NONE

 
C Argument variables

      INTEGER,           INTENT( IN ) :: WRUNIT     ! logical write unit no.
      CHARACTER( 120 ), INTENT ( IN ) :: EQNAME_MECH
      CHARACTER(  32 ), INTENT ( IN ) :: DESCRP_MECH
      INTEGER,          INTENT ( IN ) :: NS                ! no. of species found in mechanism table
      CHARACTER(  16 ), INTENT ( IN ) :: SPCLIS( : ) ! species list from mechanism table
      INTEGER,          INTENT ( IN ) :: NR                ! number of reaction
      INTEGER,          INTENT ( IN ) :: SPC1RX( : ) ! rx index of 1st occurence of species in mechanism table
      INTEGER,          INTENT ( IN ) :: IP                ! number of photolysis reactions
      CHARACTER( 16 ),  INTENT ( IN ) :: NAMCONSTS( : )
      REAL( 8 ),        INTENT ( IN ) :: CVAL( : )
      INTEGER,          INTENT ( IN ) :: SS1RX( : )
      LOGICAL,          INTENT ( IN ) :: LITE               ! option to omitted specific write statements
      
C Local Variables

      REAL,   PARAMETER   :: ONE  = 1.0
      REAL,   PARAMETER   :: ZERO = 0.0
      LOGICAL, PARAMETER  :: FALSE = .FALSE.
   
      
      INTEGER ISPC, ISPCNEW, IRX, IRXOUT, IFLD0, IFLD1, IFLD2, NLINES

      INTEGER, EXTERNAL :: JUNIT
      INTEGER, EXTERNAL :: INDEX1
 
      CHARACTER(  47 ) :: EXHEAD_SPCS
      CHARACTER(  50 ) :: EXHEAD_RXDT
      CHARACTER(  52 ) :: EXHEAD_RXCM


      LOGICAL         :: HAS_CONSTS = .FALSE.

      CHARACTER(  4 ) :: VARA4, VARB4
      CHARACTER( 20 ) :: BUFF20( MAXRXNUM )
      REAL( 8 ) ::  DBUFF( MAXRXNUM )
      REAL          SBUFF( MAXRXNUM )
      
      INTERFACE
        SUBROUTINE WRBF6( WRUNIT, AWPL, NEL, IVAR )
         INTEGER, INTENT( IN ) ::  WRUNIT     ! logical write unit no.
         INTEGER, INTENT( IN ) ::  AWPL       ! words per line (max at 10)
         INTEGER, INTENT( IN ) ::  NEL        ! number of list elements
         INTEGER, INTENT( IN ) ::  IVAR( : )  ! integer variable to write
         END SUBROUTINE WRBF6
        SUBROUTINE WRBF6_FORTRAN90( WRUNIT, AWPL, NEL, IVAR )
         INTEGER, INTENT( IN ) ::  WRUNIT     ! logical write unit no.
         INTEGER, INTENT( IN ) ::  AWPL       ! words per line (max at 10)
         INTEGER, INTENT( IN ) ::  NEL        ! number of list elements
         INTEGER, INTENT( IN ) ::  IVAR( : )  ! integer variable to write
        END SUBROUTINE WRBF6_FORTRAN90      
        SUBROUTINE WRBF12S ( WRUNIT, AWPL, NEL, VAR, AFMT )
           INTEGER, INTENT( IN )         :: WRUNIT   ! logical write unit no.
           INTEGER, INTENT( IN )         :: AWPL     ! words per line (max at 5)
           INTEGER, INTENT( IN )         :: NEL                       ! number of list elements
           REAL,    INTENT( IN )         :: VAR( : )   ! real variable to write
           CHARACTER(  1 ), INTENT( IN ) :: AFMT   ! write format: E -> 1PE11.4, F -> F11.5
        END SUBROUTINE WRBF12S
        SUBROUTINE WRBF12S_FORTRAN90 ( WRUNIT, AWPL, NEL, VAR, AFMT )
           INTEGER, INTENT( IN )         :: WRUNIT   ! logical write unit no.
           INTEGER, INTENT( IN )         :: AWPL     ! words per line (max at 5)
           INTEGER, INTENT( IN )         :: NEL                       ! number of list elements
           REAL,    INTENT( IN )         :: VAR( : )   ! real variable to write
           CHARACTER(  1 ), INTENT( IN ) :: AFMT   ! write format: E -> 1PE11.4, F -> F11.5
        END SUBROUTINE WRBF12S_FORTRAN90
        SUBROUTINE WRBF16C_FORTRAN90 ( WRUNIT, AWPL, NEL, VAR )
          INTEGER,         INTENT( IN ) :: WRUNIT      ! logical write unit no.
          INTEGER,         INTENT( IN ) :: AWPL        ! words per line (max at 5)
          INTEGER,         INTENT( IN ) :: NEL         ! number of list elements
          CHARACTER( 16 ), INTENT( IN ) :: VAR( : )  ! character variable to write
        END SUBROUTINE WRBF16C_FORTRAN90 
        SUBROUTINE WRBF16C ( WRUNIT, AWPL, NEL, VAR )
          INTEGER,         INTENT( IN ) :: WRUNIT      ! logical write unit no.
          INTEGER,         INTENT( IN ) :: AWPL        ! words per line (max at 5)
          INTEGER,         INTENT( IN ) :: NEL         ! number of list elements
          CHARACTER( 16 ), INTENT( IN ) :: VAR( : )  ! character variable to write
        END SUBROUTINE WRBF16C
      END INTERFACE

C----------------------------------------------------------------------



c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     Mechanism Reactions DATA INCLUDE File prologue
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
 
C                    12345678901234567890123456789012345678901234567890
      EXHEAD_RXDT = 'Photochemical Mechanism Reactions, Rates, etc. DATA Module File'
      WRITE( WRUNIT, 1037 ) EXHEAD_RXDT
1037  FORMAT( '!', 1X, 9('-'), 1X, A50, 1X,  9('-') )

      WRITE( WRUNIT, '("! Source file: ",A)' )TRIM(EQNAME_MECH)
      WRITE( WRUNIT, 1033 ) DESCRP_MECH
1033  FORMAT( '!', 1X, 'for Mechanism Name:', 1X, A32 )
      WRITE( WRUNIT, 1039 )
1039  FORMAT( /'!', 1X, 'This file is used to create mechanism data and functions')

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     Mechanism Reactions COMMON BLOCK INCLUDE File prologue
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
 
      IF ( MAXVAL( CVAL ) .GT. 0.0D0 ) HAS_CONSTS = .TRUE.

C                    1234567890123456789012345678901234567890123456789012
      EXHEAD_RXCM = 'Mechanism Reactions, Rates, etc. COMMON INCLUDE File'

      IF( WRITE_CGRID_DATA )THEN
         IF( LITE )THEN
           WRITE( WRUNIT, 1143 )
         ELSE
           WRITE( WRUNIT, 1043 )
         END IF 
      ELSE
          WRITE( WRUNIT, 1243 )
      END IF
      
1043  FORMAT( /'!', 1X, 'The following are reserved symbols declared in this',
     &              1X, 'file:'
     &        /'!', 4X, 'MECHNAME        = Mechanism name'
     &        /'!', 4X, 'N_GAS_CHEM_SPC  = Total number of gas species in chemical mechanism'
     &        /'!', 4X, 'NUMB_MECH_SPC   = Total number of species in chemical mechanism'
     &        /'!', 4X, 'N_ACT_SP        = Number of active (determined by ODE solver) species in mechanism'
     &        /'!', 4X, 'GAS_CHEM_SPC    = Names of gas species in chemical mechanism'
     &        /'!', 4X, 'CHEMISTRY_SPC   = Names of species in chemical mechanism'
     &        /'!', 4X, 'CGRID_INDEX     = CGRID Index of species in chemical mechanism'
     &        /'!', 4X, 'SPECIES_TYPE    = Group or type of species '
     &        /'!', 4X, 'SPECIES_MOLWT   = Molecular Weight of species (gm/mole)'
     &        /'!', 4X, 'NRXNS           = Number of mechanism reactions'
     &        /'!', 4X, 'ZERO_REACT_REACTIONS  = number zero reactant reactions',
     &        /'!', 4X, 'ONE_REACT_REACTIONS   = number one reactant reactions',
     &        /'!', 4X, 'TWO_REACT_REACTIONS   = number second order reactions',
     &        /'!', 4X, 'THREE_REACT_REACTIONS = number three reactant reactions',
     &        /'!', 4X, 'NSUNLIGHT_RXNS  = Number of mechanism reactions requiring sunlight',
     &        /'!', 4X, 'NTHERMAL_RXNS   = Number of mechanism reactions not requiring sunlight',
     &        /'!', 4X, 'KUNITS          = Units of mechanism reactions'
     &        /'!', 4X, 'KTYPE           = Reaction type'
     &        /'!', 4X, 'IRXBITS         = Bit test mask vector for selected reactions'
     &        /'!', 4X, 'IORDER          = Order of the reaction'
     &        /'!', 4x, 'NTERMS_JACOB    = Maximum number of nonzero terms in day/night Jacobian'
     &        /'!', 4x, 'MSTEPS_JACOB    = Maximum number of LU Decomposition steps to solve each Jacobian'
     &        /'!', 4X, 'KTN1            = Number of type 1 reactions'
     &        /'!', 4X, 'KRX1            = Reactions list pointer to type 1 reactions'
     &        /'!', 4X, 'KTN2            = Number of type 2 reactions'
     &        /'!', 4X, 'KRX2            = Reactions list pointer to type 2 reactions'
     &        /'!', 4X, 'KTN3            = Number of type 3 reactions'
     &        /'!', 4X, 'KRX3            = Reactions list pointer to type 3 reactions'
     &        /'!', 4X, 'KTN4            = Number of type 4 reactions'
     &        /'!', 4X, 'KRX4            = Reactions list pointer to type 4 reactions'
     &        /'!', 4X, 'KTN5            = Number of type 5 reactions'
     &        /'!', 4X, 'KRX5            = Reactions list pointer to type 5 reactions'
     &        /'!', 4X, 'KTN6            = Number of type 6 reactions'
     &        /'!', 4X, 'KRX6            = Reactions list pointer to type 6 reactions'
     &        /'!', 4X, 'KTN7            = Number of type 7 reactions'
     &        /'!', 4X, 'KRX7            = Reactions list pointer to type 7 reactions' )

1243  FORMAT( /'C', 1X, 'The following are reserved symbols declared in this',
     &              1X, 'INCLUDE file:'
     &        /'C', 4X, 'MECHNAME        = Mechanism name'
     &        /'C', 4X, 'N_GAS_CHEM_SPC  = Total number of gas species in chemical mechanism'
     &        /'C', 4X, 'NUMB_MECH_SPC   = Total number of species in chemical mechanism'
     &        /'C', 4X, 'N_ACT_SP        = Number of active (determined by ODE solver) species in mechanism'
     &        /'C', 4X, 'GAS_CHEM_SPC    = Names of gas species in chemical mechanism'
     &        /'C', 4X, 'NRXNS           = Number of mechanism reactions'
     &        /'!', 4X, 'ZERO_REACT_REACTIONS  = number zero reactant reactions',
     &        /'!', 4X, 'ONE_REACT_REACTIONS   = number one reactant reactions',
     &        /'!', 4X, 'TWO_REACT_REACTIONS   = number second order reactions',
     &        /'!', 4X, 'THREE_REACT_REACTIONS = number three reactant reactions',
     &        /'!', 4X, 'NSUNLIGHT_RXNS  = Number of mechanism reactions requiring sunlight',
     &        /'!', 4X, 'NTHERMAL_RXNS   = Number of mechanism reactions not requiring sunlight',
     &        /'C', 4X, 'KUNITS          = Units of mechanism reactions'
     &        /'C', 4X, 'KTYPE           = Reaction type'
     &        /'C', 4X, 'IRXBITS         = Bit test mask vector for selected reactions'
     &        /'C', 4X, 'IORDER          = Order of the reaction'
     &        /'!', 4x, 'NTERMS_JACOB    = Maximum number of nonzero terms in day/night Jacobian'
     &        /'!', 4x, 'NSTEPS_JACOB    = Maximum number of LU Decomposition steps to solve each Jacobian'
     &        /'C', 4X, 'KTN1            = Number of type 1 reactions'
     &        /'C', 4X, 'KRX1            = Reactions list pointer to type 1 reactions'
     &        /'C', 4X, 'KTN2            = Number of type 2 reactions'
     &        /'C', 4X, 'KRX2            = Reactions list pointer to type 2 reactions'
     &        /'C', 4X, 'KTN3            = Number of type 3 reactions'
     &        /'C', 4X, 'KRX3            = Reactions list pointer to type 3 reactions'
     &        /'C', 4X, 'KTN4            = Number of type 4 reactions'
     &        /'C', 4X, 'KRX4            = Reactions list pointer to type 4 reactions'
     &        /'C', 4X, 'KTN5            = Number of type 5 reactions'
     &        /'C', 4X, 'KRX5            = Reactions list pointer to type 5 reactions'
     &        /'C', 4X, 'KTN6            = Number of type 6 reactions'
     &        /'C', 4X, 'KRX6            = Reactions list pointer to type 6 reactions'
     &        /'C', 4X, 'KTN7            = Number of type 7 reactions'
     &        /'C', 4X, 'KRX7            = Reactions list pointer to type 7 reactions' )

1143  FORMAT( /'!', 1X, 'The following are reserved symbols declared in this',
     &              1X, 'file:'
     &        /'!', 4X, 'MECHNAME        = Mechanism name'
     &        /'!', 4X, 'N_GAS_CHEM_SPC  = Total number of gas species in chemical mechanism'
     &        /'!', 4X, 'NUMB_MECH_SPC   = Total number of species in chemical mechanism'
     &        /'!', 4X, 'N_ACT_SP        = Number of active (determined by ODE solver) species in mechanism'
     &        /'!', 4X, 'GAS_CHEM_SPC    = Names of gas species in chemical mechanism'
     &        /'!', 4X, 'CHEMISTRY_SPC   = Names of species in chemical mechanism'
     &        /'!', 4X, 'CGRID_INDEX     = CGRID Index of species in chemical mechanism'
     &        /'!', 4X, 'SPECIES_TYPE    = Group or type of species in chemical mechanism'
     &        /'!', 4X, 'SPECIES_MOLWT   = Molecular Weight of species (gm/mole)'
     &        /'!', 4X, 'NRXNS           = Number of mechanism reactions'
     &        /'!', 4X, 'ZERO_REACT_REACTIONS  = number zero reactant reactions',
     &        /'!', 4X, 'ONE_REACT_REACTIONS   = number one reactant reactions',
     &        /'!', 4X, 'TWO_REACT_REACTIONS   = number second order reactions',
     &        /'!', 4X, 'THREE_REACT_REACTIONS = number three reactant reactions',
     &        /'!', 4X, 'NSUNLIGHT_RXNS  = Number of mechanism reactions requiring sunlight',
     &        /'!', 4X, 'NTHERMAL_RXNS   = Number of mechanism reactions not requiring sunlight',
     &        /'C', 4X, 'KUNITS          = Units of mechanism reactions'
     &        /'!', 4X, 'IRXBITS         = Bit test mask vector for selected reactions'
     &        /'!', 4X, 'IORDER          = Order of the reaction'
     &        /'!', 4X, 'KTYPE           = Reaction type'
     &        /'!', 4x, 'NTERMS_JACOB    = Maximum number of nonzero terms in day/night Jacobian'
     &        /'!', 4x, 'NSTEPS_JACOB    = Maximum number of LU Decomposition steps to solve each Jacobian' )



      IF ( HAS_CONSTS ) THEN

         WRITE( WRUNIT, 1045 )
1045     FORMAT( /'!', 4X, 'NWM       = Number of air 3-body reactions'
     &           /'!', 4X, 'NRXWM     = Reactions list pointer to air 3-body',
     &                 1X, 'reactions'
     &           /'!', 4X, 'ATM_AIR   = air 3-body reactions concentration'
     &           /'!', 4X, 'NWW       = Number of H2O 3-body reactions'
     &           /'!', 4X, 'NRXWW     = Reactions list pointer to H2O 3-body',
     &                 1X, 'reactions'
     &           /'!', 4X, 'NWO2      = Number of reactions with O2'
     &           /'!', 4X, 'NRXWO2    = Reactions list pointer to O2 reactions'
     &           /'!', 4X, 'ATM_O2    = Oxygen reactions concentration'
     &           /'!', 4X, 'NWN2      = Number of N2 3-body reactions'
     &           /'!', 4X, 'NRXWN2    = Reactions list pointer to N2 3-body',
     &                 1X, 'reactions'
     &           /'!', 4X, 'ATM_N2    = Nitrogen 3-body reactions concentration'
     &           /'!', 4X, 'NWCH4     = Number of reactions with CH4'
     &           /'!', 4X, 'NRXWCH4   = Reactions list pointer to CH4 reactions'
     &           /'!', 4X, 'ATM_CH4   = Methane reactions concentration'
     &           /'!', 4X, 'NWH2      = Number of reactions with H2'
     &           /'!', 4X, 'NRXWH2    = Reactions list pointer to H2 reactions'
     &           /'!', 4X, 'ATM_H2    = Hydrogen reactions concentration' )

      ELSE

         WRITE( WRUNIT, 1047 )
1047     FORMAT( /'!', 4X, 'NWM       = Number of air 3-body reactions'
     &           /'!', 4X, 'NRXWM     = Reactions list pointer to air 3-body',
     &                 1X, 'reactions'
     &           /'!', 4X, 'NWW       = Number of H2O 3-body reactions'
     &           /'!', 4X, 'NRXWW     = Reactions list pointer to H2O 3-body',
     &                 1X, 'reactions'
     &           /'!', 4X, 'NWO2      = Number of reactions with O2'
     &           /'!', 4X, 'NRXWO2    = Reactions list pointer to O2 reactions'
     &           /'!', 4X, 'NWN2      = Number of N2 3-body reactions'
     &           /'!', 4X, 'NRXWN2    = Reactions list pointer to N2 3-body',
     &                 1X, 'reactions'
     &           /'!', 4X, 'NWCH4     = Number of reactions with CH4'
     &           /'!', 4X, 'NRXWCH4   = Reactions list pointer to CH4 reactions'
     &           /'!', 4X, 'NWH2      = Number of reactions with H2'
     &           /'!', 4X, 'NRXWH2    = Reactions list pointer to H2 reactions' )

      END IF


      IF( LITE )THEN
        WRITE( WRUNIT, 1149 )
      ELSE
        WRITE( WRUNIT, 1049 )
      END IF 

1049  FORMAT( /'!', 4X, 'MXPRD     = Maximum number of mechanism reaction',
     &              1X, 'products'
     &        /'!', 4X, 'IRR       = Reactions list pointer to reactants',
     &              1X, 'and products'
     &        /'!', 4X, 'RTDAT     = Kinetic reaction rates expressions',
     &              1X, 'components'
     &        /'!', 4X, 'NFALLOFFF = Number of falloff reactions'
     &        /'!', 4X, 'IRRFALL   = Reactions list pointer to falloff reactions'
     &        /'!', 4X, 'RFDAT     = Falloff reaction rates expressions',
     &              1X, 'components'
     &        /'!', 4X, 'SC        = Stoichiometric coefficients'
     &        /'!', 4X, 'NREACT    = Number of reactants in each mechanism',
     &              1X, 'reaction'
     &        /'!', 4X, 'NPRDCT    = Number of products in each mechanism',
     &              1X, 'reaction'
     &        /'!', 4X, 'RXLABEL   = Character label list for mechanism',
     &              1X, 'reactions'
     &        /'!', 4X, 'NMPHOT    = Number of mechanism photolytic reactions'
     &        /'!', 4X, 'NPHOTAB   = Number of photolytic reactions tables', 
     &        /'!', 4X, 'IPH       = Reactions list pointer to photolytic',
     &              1X, 'reactions and tables',
     &        /'!', 4X, 'MHETERO   = Number of mechanism heteorogenous reactions',
     &        /'!', 4X, 'NHETERO   = Number of unique heteorogenous rate constants', 
     &        /'!', 4X, 'IHETERO   = Reactions list pointer to heteorogenous',
     &              1X, 'reactions and tables' )

1149  FORMAT( /'!', 4X, 'MXPRD     = Maximum number of mechanism reaction',
     &              1X, 'products'
     &        /'!', 4X, 'IRR       = Reactions list pointer to reactants',
     &              1X, 'and products'
     &        /'!', 4X, 'SC        = Stoichiometric coefficients'
     &        /'!', 4X, 'NREACT    = Number of reactants in each mechanism',
     &              1X, 'reaction'
     &        /'!', 4X, 'NPRDCT    = Number of products in each mechanism',
     &              1X, 'reaction'
     &        /'!', 4X, 'RXLABEL   = Character label list for mechanism',
     &              1X, 'reactions'
     &        /'!', 4X, 'NMPHOT    = Number of mechanism photolytic reactions'
     &        /'!', 4X, 'NPHOTAB   = Number of photolytic reactions tables', 
     &        /'!', 4X, 'IPH       = Reactions list pointer to photolytic',
     &              1X, 'reactions and tables',
     &        /'!', 4X, 'MHETERO   = Number of mechanism heteorogenous reactions',
     &        /'!', 4X, 'NHETERO   = Number of unique heteorogenous rate constants', 
     &        /'!', 4X, 'IHETERO   = Reactions list pointer to heteorogenous',
     &              1X, 'reactions and tables' )

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     NS
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( EXUNIT_SPCS, 1053 ) NS + N_SS_SPC
1053  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NSPCS =', I4 )


c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     CTMSPC and SPC1RX
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

!     WRITE( EXUNIT_SPCS, 1055 )
!055  FORMAT( /6X, 'INTEGER    ISPCS' )

!      WRITE( EXUNIT_SPCS, 1057 )
!057  FORMAT( /6X, 'CHARACTER*16 CTMSPC(NSPCS )'
!1057  FORMAT( /6X, 'CHARACTER( 16 ) :: SPCNAMES( NSPCS )'
!     &        /6X, 'INTEGER         :: SPC1RX( NSPCS )' / )

 

!     DO ISPC = 1, NS
!        WRITE( EXUNIT_SPCS, 1059 ) ISPC, ISPC, SPCLIS( ISPC ), SPC1RX( ISPC )
!059     FORMAT( 6X, 'DATA', 1X, 'CTMSPC(', I3, '),', 1X, 'SPC1RX(', I3, ')',
!1059     FORMAT( 6X, 'DATA', 1X, 'SPCNAMES(', I3, '),', 1X, 'SPC1RX(', I3, ')',
!    &           2X, '/ ''', A16, ''',', I4, ' /' )
!     END DO

!     DO ISPC = 1, N_SS_SPC
!        WRITE( EXUNIT_SPCS, 1059 ) ISPC + NS, ISPC + NS, SS_SPC( ISPC ), SS1RX( ISPC )
!     END DO


c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     MECHNAME and GAS_CHEM_SPC list
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( WRUNIT, 1061 ) DESCRP_MECH(1:LEN_TRIM(DESCRP_MECH))
1061  FORMAT( /6X, 'CHARACTER( 32 ), PARAMETER ::',
     &         1X, 'MECHNAME = ''', A, '''' )
      
      N_GAS_CHEM_SPC = 0 
      DO ISPC = 1, NUMB_MECH_SPCS
         IF( SPECIES_TYPE( ISPC ) .EQ. 'AE' )CYCLE
          N_GAS_CHEM_SPC =  N_GAS_CHEM_SPC + 1
      END DO

      IF( WRITE_CGRID_DATA )THEN
          WRITE( WRUNIT, 2053 ) N_GAS_CHEM_SPC,  NUMB_MECH_SPCS
2053      FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: N_GAS_CHEM_SPC =', I4,
     &            /6X, 'INTEGER, PARAMETER', 1X, ':: NUMB_MECH_SPC  =', I4 )
      ELSE
          WRITE( WRUNIT, 2153 ) N_GAS_CHEM_SPC
2153      FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: N_GAS_CHEM_SPC =', I4 )
      
      END IF
       
      IF( WRITE_CGRID_DATA )THEN
          WRITE( WRUNIT, 2057 )
2057      FORMAT( /6X, 'CHARACTER( 16 ) :: GAS_CHEM_SPC( N_GAS_CHEM_SPC )',
     &            /6X, 'CHARACTER( 16 ) :: CHEMISTRY_SPC( NUMB_MECH_SPC )',
     &            /6X, 'CHARACTER( 16 ) :: SPECIES_TYPE(  NUMB_MECH_SPC )',
     &            /6X, 'INTEGER         :: CGRID_INDEX (  NUMB_MECH_SPC )',
     &            /6X, 'INTEGER         :: TYPE_INDEX  (  NUMB_MECH_SPC )',
     &            /6X, 'REAL( 8 )       :: SPECIES_MOLWT( NUMB_MECH_SPC )',
     &            /6X, 'LOGICAL         :: CONVERT_CONC(  NUMB_MECH_SPC )')
      ELSE
          WRITE( WRUNIT, 2157 )
2157      FORMAT( /6X, 'CHARACTER( 16 ) :: GAS_CHEM_SPC( N_GAS_CHEM_SPC )')
      END IF
      
      WRITE( WRUNIT, 2052 )
2052  FORMAT(/, '! The below character and integer arrays list the model species names used in the ',
     &       /, '! chemical mechanism. The gas species and their order should agree with ',
     &       /, '! the GC_SPC array for the gas phase chemistry to work correctly. ',
     &       /, '! If present, the CHEMISTRY_SPC names and species type should agree with the CGRID_SPCS module' /)

      IRX = 0
      DO ISPC = 1, NS 
         ISPCNEW = INEW2OLD( ISPC )
         IF( SPECIES_TYPE( ISPCNEW ) .EQ. 'AE' )CYCLE
         IRX = IRX + 1
         WRITE( WRUNIT, 2059 ) IRX, SPCLIS( ISPCNEW )
2059     FORMAT( 6X, 'DATA', 1X, 'GAS_CHEM_SPC(', I4, ' ) / ''', A16, ''' /')
      END DO

      DO ISPC = 1, N_SS_SPC
         ISPCNEW = INEW2OLD( ISPC )
         IF( SPECIES_TYPE( ISPCNEW ) .NE. 'GC' )CYCLE
         WRITE( WRUNIT, 2059 ) ISPC + NS, SS_SPC( ISPCNEW )
      END DO

      WRITE( WRUNIT,'( 2/ )')
      
      IF( HALOGEN_PARAMETER )THEN
         WRITE( WRUNIT, 3060 )
      ELSE
         WRITE( WRUNIT, 3061 )
      END IF
 
      WRITE( WRUNIT, 2052 )
2249  FORMAT(/, '! The below type is used to define species in the photochemical mechanism.',
     &       /, '! The array based on type contains the needed data for these names. The data should agree with',
     &       /, '! the CGRID_SPCS module' /)

      WRITE( WRUNIT, 2064 )      
      WRITE( WRUNIT, 2063 ) 


      IF( USE_SPCS_NAMELISTS )THEN
          WRITE( WRUNIT, 2250 )
          DO ISPC = 1, (NS + N_SS_SPC - 1)
              ISPCNEW = INEW2OLD( ISPC )
             WRITE( WRUNIT, 2161 ) MECHANISM_SPC( ISPCNEW ), CGRID_INDEX( ISPCNEW ), 
     &       SPECIES_TYPE( ISPCNEW ), SPECIES_MOLWT( ISPCNEW ), CONVERT_CONC( ISPCNEW )
          END DO
          ISPC = (NS + N_SS_SPC)
          ISPCNEW = INEW2OLD( ISPC )
          WRITE( WRUNIT, 2162 ) MECHANISM_SPC( ISPCNEW ), CGRID_INDEX( ISPCNEW ), 
     &    SPECIES_TYPE( ISPCNEW ), SPECIES_MOLWT( ISPCNEW ), CONVERT_CONC( ISPCNEW )
      ELSE
          WRITE( WRUNIT, 2250 )
          DO ISPC = 1, (NS + N_SS_SPC - 1)
              ISPCNEW = INEW2OLD( ISPC )
             WRITE( WRUNIT, 2161 ) MECHANISM_SPC( ISPCNEW ), CGRID_INDEX( ISPCNEW ), 
     &       SPECIES_TYPE( ISPCNEW ), ONE, FALSE
          END DO
          ISPC = (NS + N_SS_SPC)
          ISPCNEW = INEW2OLD( ISPC )
          WRITE( WRUNIT, 2162 ) MECHANISM_SPC( ISPCNEW ), CGRID_INDEX( ISPCNEW ), 
     &    SPECIES_TYPE( ISPCNEW ), ONE, FALSE
      END IF


2250  FORMAT(6X,  'TYPE MEMBER'
     &       /6X, '   CHARACTER( 16 ) :: CHEMISTRY_SPC',
     &       /6X, '   INTEGER         :: CGRID_INDEX',
     &       /6X, '   CHARACTER(  2 ) :: SPECIES_TYPE',
     &       /6X, '   REAL( 8 )       :: SPECIES_MOLWT',
     &       /6X, '   LOGICAL         :: CONVERT_CONC',
     &       /6X, 'END TYPE MEMBER',
     &       /6X, 'TYPE( MEMBER ) ::  SPECIES_LIST( NUMB_MECH_SPC ) = (/ &')
2161   FORMAT( 6X, '& MEMBER("', A16, '", ', I4,', "', A2, '"', ', ', F7.2,'D0, ', L1,'), &')
2162   FORMAT( 6X, '& MEMBER("', A16, '", ', I4,', "', A2, '"', ', ', F7.2,'D0, ', L1,') /)' /)

      IF( USE_SPCS_NAMELISTS )THEN
          DO ISPC = 1, NS + N_SS_SPC
              ISPCNEW = INEW2OLD( ISPC )
!             WRITE( WRUNIT, 2161 ) ISPC, ISPC, ISPC, ISPC,  ISPC, MECHANISM_SPC( ISPC ), CGRID_INDEX( ISPC ), 
!     &       SPECIES_TYPE( ISPC ), SPECIES_MOLWT( ISPC ), CONVERT_CONC( ISPC )
              WRITE( WRUNIT, 2061 ) ISPC, ISPC, MECHANISM_SPC( ISPCNEW ), SPECIES_MOLWT( ISPCNEW )
          END DO
          WRITE( WRUNIT,'( / )')
          DO ISPC = 1, NS + N_SS_SPC
              ISPCNEW = INEW2OLD( ISPC )
              WRITE( WRUNIT, 2065 ) ISPC, ISPC, ISPC, CGRID_INDEX( ISPCNEW ), 
     &       SPECIES_TYPE( ISPCNEW ), CONVERT_CONC( ISPCNEW ), TRIM( MECHANISM_SPC( ISPCNEW ) )
          END DO
      ELSE
          DO ISPC = 1, NS + N_SS_SPC
              ISPCNEW = INEW2OLD( ISPC )
!             WRITE( WRUNIT, 2161 ) ISPC, ISPC, ISPC, ISPC, ISPC, MECHANISM_SPC( ISPC ), CGRID_INDEX( ISPC ), 
!     &       SPECIES_TYPE( ISPC ), ONE, USE_SPCS_NAMELISTS
              WRITE( WRUNIT, 2061 ) ISPC, ISPC, MECHANISM_SPC( ISPCNEW ), ONE 
          END DO
          WRITE( WRUNIT,'( / )')
          DO ISPC = 1, NS + N_SS_SPC
              ISPCNEW = INEW2OLD( ISPC )
              WRITE( WRUNIT, 2065 ) ISPC, ISPC, ISPC, CGRID_INDEX( ISPCNEW ), 
     &       SPECIES_TYPE( ISPCNEW ), FALSE, TRIM( MECHANISM_SPC( ISPCNEW ) )
          END DO
      END IF

      WRITE( WRUNIT, 2260 )
2260  FORMAT(/, '! The below integers define the locations of mechanism species in the solver',
     &       /, '! concentration array.' / )
     
          DO ISPC = 1, NS + N_SS_SPC
              ISPCNEW = INEW2OLD( ISPC )
!             WRITE( WRUNIT, 2161 ) ISPC, ISPC, ISPC, ISPC,  ISPC, MECHANISM_SPC( ISPC ), CGRID_INDEX( ISPC ), 
!     &       SPECIES_TYPE( ISPC ), SPECIES_MOLWT( ISPC ), CONVERT_CONC( ISPC )
              WRITE( WRUNIT, 2261 ) MECHANISM_SPC( ISPCNEW )(1:MAXLEN_SPECIES), ISPC
          END DO
2261   FORMAT( 6X, 'INTEGER :: INDEX_', A, ' = ', I4  )



2061   FORMAT( 6X, 'DATA', 1X, 'CHEMISTRY_SPC(', I4, ' ), SPECIES_MOLWT(', I4,' ) / ''', A16, ''', ', F7.2,'D0 /')

2065   FORMAT( 6X, 'DATA', 1X, 'CGRID_INDEX(', I4,' ), SPECIES_TYPE(', I4,' ), CONVERT_CONC(', I4,' ) / ', 
     &              I4, ', ''', A2, ''', ',  L1,' /  ! ', A)

3060  FORMAT( /6X,'LOGICAL   :: HALOGEN_PARAMETER = .TRUE. '  /)
3061  FORMAT( /6X,'LOGICAL   :: HALOGEN_PARMAETER = .FALSE. ' /)
2062  FORMAT( /6X,'LOGICAL   :: MAPPED_TO_CGRID   = .TRUE. '  /)
2063  FORMAT( /6X,'LOGICAL   :: MAPPED_TO_CGRID   = .FALSE. ' /)
2064  FORMAT(/'! MAPPED_TO_CGRID declares whether CMAQ namelists were used to determine ',
     &       /'! the below values of CGRID_INDEX, SPECIES_TYPE, SPECIES_MOLWT, and CONVERT_CONC' 
     &       /6X, 'LOGICAL, PARAMETER, PRIVATE :: F = .FALSE.' 
     &       /6X, 'LOGICAL, PARAMETER, PRIVATE :: T = .TRUE.'  /)


c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     NR, KUNITS, NFALLOFF, etc.
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( WRUNIT, 1075 ) NS
1075  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: N_ACT_SP =', I4 )

      WRITE( WRUNIT, 1076 ) NR
1076  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NRXNS =', I4 )

      WRITE( WRUNIT, 1083 ) 'ONE_REACT_REACTIONS',   ONE_REACT_REACTIONS
      WRITE( WRUNIT, 1083 ) 'TWO_REACT_REACTIONS',   TWO_REACT_REACTIONS
      WRITE( WRUNIT, 1083 ) 'THREE_REACT_REACTIONS', THREE_REACT_REACTIONS
      WRITE( WRUNIT, 1083 ) 'ZERO_REACT_REACTIONS',  ZERO_REACT_REACTIONS
1083  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: ', A23,' = ', I4 )
1084  FORMAT( /6X, 'LOGICAL, PARAMETER', 1X, ':: ', A23,' = .FALSE.' )
1085  FORMAT( /6X, 'LOGICAL, PARAMETER', 1X, ':: ', A23,' = .TRUE.' )


1250  FORMAT( /'!', 1X, 'Reactions are grouped based on number of reactants',
     &        /'!', 1X, 'Following parameters state the starting index for each group')

      IRX = 1
      IF ( ONE_REACT_REACTIONS .LT. 1 )THEN
          IRXOUT = IRX - 1
          WRITE( WRUNIT, 1084 )'UNITARY_REACTIONS'
      ELSE
          IRXOUT = IRX
          WRITE( WRUNIT, 1085 )'UNITARY_REACTIONS'
      END IF
      WRITE( WRUNIT, 1083 ) 'ONE_REACT_START',   IRXOUT
      IRXOUT = IRXOUT + ONE_REACT_REACTIONS - 1
      WRITE( WRUNIT, 1083 ) 'ONE_REACT_STOP ',   IRXOUT
      IRX = ONE_REACT_REACTIONS + IRX

      IF ( TWO_REACT_REACTIONS .LT. 1 )THEN
          IRXOUT = IRX - 1
          WRITE( WRUNIT, 1084 )'BINARY_REACTIONS '
      ELSE
          IRXOUT = IRX
          WRITE( WRUNIT, 1085 )'BINARY_REACTIONS '
      END IF
      WRITE( WRUNIT, 1083 ) 'TWO_REACT_START',   IRXOUT
      IRXOUT = IRXOUT + TWO_REACT_REACTIONS - 1
      WRITE( WRUNIT, 1083 ) 'TWO_REACT_STOP ',   IRXOUT
      IRX = TWO_REACT_REACTIONS + IRX

      IF ( THREE_REACT_REACTIONS .LT. 1 )THEN
          IRXOUT = IRX - 1
          WRITE( WRUNIT, 1084 )'TERNARY_REACTIONS'
      ELSE
          IRXOUT = IRX
          WRITE( WRUNIT, 1085 )'TERNARY_REACTIONS'
      END IF
      WRITE( WRUNIT, 1083 ) 'THREE_REACT_START', IRXOUT
      IRXOUT = IRXOUT + THREE_REACT_REACTIONS - 1
      WRITE( WRUNIT, 1083 ) 'THREE_REACT_STOP ',   IRXOUT
      IRX = THREE_REACT_REACTIONS + IRX

      IF ( ZERO_REACT_REACTIONS .LT. 1 )THEN
          IRXOUT = IRX - 1
          WRITE( WRUNIT, 1084 )'NULL_REACTIONS   '
      ELSE
          IRXOUT = IRX
          WRITE( WRUNIT, 1085 )'NULL_REACTIONS   '
      END IF
      WRITE( WRUNIT, 1083 ) 'ZERO_REACT_START',  IRXOUT
      IRXOUT = IRXOUT + ZERO_REACT_REACTIONS - 1
      WRITE( WRUNIT, 1083 ) 'ZERO_REACT_STOP ',   IRXOUT
      
!     WRITE( WRUNIT, 1080 ) NSUNLIGHT
!1080  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NSUNLIGHT_RXNS =', I4 )
!      IF( SUN_BELOW )THEN 
!         IRXOUT = IRX
!         IRX = ZERO_REACT_REACTIONS + IRX 
!         WRITE( WRUNIT, 1083 ) 'ZERO_REACT_START',  IRXOUT
!         WRITE( WRUNIT, 1083 ) 'ZERO_REACT_STOP ',  IRX - 1
!         IRXOUT = IRX
!         IRX    = THREE_REACT_REACTIONS + IRX
!         WRITE( WRUNIT, 1083 ) 'THREE_REACT_START', IRXOUT
!         WRITE( WRUNIT, 1083 ) 'THREE_REACT_STOP ', IRX - 1
!         IRXOUT = IRX
!         IRX = TWO_REACT_REACTIONS + IRX
!         WRITE( WRUNIT, 1083 ) 'TWO_REACT_START',   IRXOUT
!         WRITE( WRUNIT, 1083 ) 'TWO_REACT_STOP ',   IRX - 1
!         IRXOUT = IRX
!         IRX    = ONE_REACT_REACTIONS + IRX 
!         WRITE( WRUNIT, 1083 ) 'ONE_REACT_START',   IRXOUT
!         WRITE( WRUNIT, 1083 ) 'ONE_REACT_STOP ',   IRX - 1
!         WRITE( WRUNIT, 1251 ) 'SUNLIGHT_BELOW','.TRUE.'
!         IRXOUT = NTHERMAL +  1
!         IRX    = IRXOUT + NSUNLIGHT 
!         WRITE( WRUNIT, 1083 ) 'NSUN_RXNS_START',  IRXOUT
!         WRITE( WRUNIT, 1083 ) 'NSUN_RXNS_STOP',  IRX - 1
!      ELSE
!         IRXOUT = IRX
!         IRX    = ONE_REACT_REACTIONS + IRX 
!         WRITE( WRUNIT, 1083 ) 'ONE_REACT_START',   IRXOUT
!         WRITE( WRUNIT, 1083 ) 'ONE_REACT_STOP ',   IRX - 1
!         IRXOUT = IRX
!         IRX = TWO_REACT_REACTIONS + IRX
!         WRITE( WRUNIT, 1083 ) 'TWO_REACT_START',   IRXOUT
!         WRITE( WRUNIT, 1083 ) 'TWO_REACT_STOP ',   IRX - 1
!         IRXOUT = IRX
!         IRX    = THREE_REACT_REACTIONS + IRX
!         WRITE( WRUNIT, 1083 ) 'THREE_REACT_START', IRXOUT
!         WRITE( WRUNIT, 1083 ) 'THREE_REACT_STOP ', IRX - 1
!         IRXOUT = IRX
!         IRX = ZERO_REACT_REACTIONS + IRX 
!         WRITE( WRUNIT, 1083 ) 'ZERO_REACT_START',  IRXOUT
!         WRITE( WRUNIT, 1083 ) 'ZERO_REACT_STOP ',  IRX - 1
!         WRITE( WRUNIT, 1251 ) 'SUNLIGHT_BELOW', '.FALSE.'
!         IRXOUT = 1
!         IRX    = IRXOUT + NSUNLIGHT 
!         WRITE( WRUNIT, 1083 ) 'NSUN_RXNS_START',  IRXOUT
!         WRITE( WRUNIT, 1083 ) 'NSUN_RXNS_STOP',  IRX - 1
!      END IF
!1251  FORMAT(/6X,'LOGICAL, PARAMETER', 1X, ':: ',A23,' = ', A)

      WRITE( WRUNIT, 1083 ) 'NSUNLIGHT_RXNS  ',  NSUNLIGHT

      WRITE( WRUNIT, 1083 ) 'NTHERMAL_RXNS   ',  NTHERMAL
      WRITE( WRUNIT, 1083 ) 'KUNITS          ',  KUNITS

      WRITE( WRUNIT, 1079 )

1079  FORMAT( /6X, 'INTEGER  :: IRXXN' )
c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     IP, IPH
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      IF ( IP .NE. 0 ) THEN

         WRITE( WRUNIT, 1701 ) IP
1701     FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NMPHOT =', I4 )

         WRITE( WRUNIT, 1703 )
1703     FORMAT(  6X, 'INTEGER', 12X, ':: IPH( NMPHOT,3 )' )

         WRITE( WRUNIT, 1705 ) '1'
1705     FORMAT( /6X, 'DATA ( IPH( IRXXN,', A, ' ), IRXXN = 1, NMPHOT ) / & ' )

         CALL WRBF6_FORTRAN90( WRUNIT, 10, IP, IPH( 1:IP,1 ) )

         WRITE( WRUNIT, 1705 ) '2'

         CALL WRBF6_FORTRAN90( WRUNIT, 10, IP, IPH( 1:IP,2 ) )

         WRITE( WRUNIT, 1705 ) '3'

         CALL WRBF6_FORTRAN90( WRUNIT, 10, IP, IPH( 1:IP,3 ) )

      ELSE

         WRITE( WRUNIT, 1707 )
1707     FORMAT( /'! Photolysis reactions information not available ...'
     &           /6X, 'INTEGER, PARAMETER', 1X, ':: NMPHOT = 0' )

         WRITE( WRUNIT, 1709 )
1709     FORMAT( /6X, 'INTEGER', 12X, ':: IPH( 1,3 )' )

      END IF

      IF( .NOT. LITE )THEN ! write rate constant parameters
         WRITE( WRUNIT, 1411 )
1411     FORMAT( /6X, 'REAL( 8 )', 10X, ':: RTDAT( 3,NRXNS )' )

         WRITE( WRUNIT, 1501 ) NFALLOFF
1501     FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NFALLOFF =', I4 )

         IF ( NFALLOFF .NE. 0 ) THEN
            WRITE( WRUNIT, 1521 )
1521        FORMAT(  6X, 'REAL( 8 )', 10X, ':: RFDAT( 5,NFALLOFF )' )
         ELSE
            WRITE( WRUNIT, 1525 )
1525        FORMAT(  6X, 'REAL( 8 )', 10X, ':: RFDAT( 1,1 )' )
         END IF
      END IF 
c
c     KTYPE
c
        WRITE( WRUNIT, 1101 )
1101    FORMAT( /6X, 'INTEGER', 12X, ':: KTYPE( NRXNS )' )

        WRITE( WRUNIT, 1103 )
1103    FORMAT( /6X, 'DATA ( KTYPE( IRXXN ), IRXXN = 1, NRXNS ) /  & ' )

        CALL WRBF6_FORTRAN90 ( WRUNIT, 10, NR, KTYPE )
c
c     IRXBITS
c
      WRITE( WRUNIT, 1105 )
1105  FORMAT( /6X, 'INTEGER', 12X, ':: IRXBITS( NRXNS )' )

      WRITE( WRUNIT, 1107 )
1107  FORMAT( /6X, 'DATA ( IRXBITS( IRXXN ), IRXXN = 1, NRXNS ) / & ' )

      CALL WRBF6_FORTRAN90( WRUNIT, 10, NR, IRXBITS )
c
c     Jacobian information
c
      WRITE( WRUNIT, 1115 ) MXARRAY
1115  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NTERMS_JACOB = ', I8 )

      WRITE( WRUNIT, 1117 ) MAXGL3
1117  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NSTEPS_JACOB = ', I8 )
c
c     IORDER
c
      WRITE( WRUNIT, 1109 )
1109  FORMAT( /6X, 'INTEGER', 12X, ':: IORDER( NRXNS )' )

      WRITE( WRUNIT, 1111 )
1111  FORMAT( /6X, 'DATA ( IORDER( IRXXN ), IRXXN = 1, NRXNS ) / & ' )

      CALL WRBF6_FORTRAN90(WRUNIT, 10, NR, IORDER )

      IF( .NOT. LITE )THEN   
c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     KTN1,KRX1, KTN2,KRX2, KTN3,KRX3, KTN4,KRX4,
c     KTN5,KRX5, KTN6,KRX6, KTN7,KRX7 
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
                
          VARA4 = 'KTN1'
          VARB4 = 'KRX1'
          WRITE( WRUNIT, 1201 ) VARA4, KTN1
1201      FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: ', A4, ' =', I4 )
          IF ( KTN1 .NE. 0 ) THEN
             WRITE( WRUNIT, 1203 ) VARB4, VARA4
1203         FORMAT(  6X, 'INTEGER', 12X, ':: ', A4, '( ', A4, ' )' )
             WRITE( WRUNIT, 1205 ) VARB4, VARA4
1205         FORMAT( /6X, 'DATA ( ', A4, '( IRXXN ), IRXXN = 1, ', A4, ' ) / & '  )

             CALL WRBF6_FORTRAN90( WRUNIT, 10, KTN1, KRX1 )
   
          ELSE
             WRITE( WRUNIT, 1207 ) VARB4
1207         FORMAT(  6X, 'INTEGER', 12X, ':: ', A4, '( 1 )' )
             WRITE( WRUNIT, 1209 ) VARB4
1209         FORMAT( /6X, 'DATA   ', A4, '( 1 )', ' / 0 /' )
          END IF

          VARA4 = 'KTN2'
          VARB4 = 'KRX2'
          WRITE( WRUNIT, 1201 ) VARA4, KTN2
          IF ( KTN2 .NE. 0 ) THEN
             WRITE( WRUNIT, 1203 ) VARB4, VARA4
             WRITE( WRUNIT, 1205 ) VARB4, VARA4

             CALL WRBF6_FORTRAN90( WRUNIT, 10, KTN2, KRX2 )
   
          ELSE
             WRITE( WRUNIT, 1207 ) VARB4
             WRITE( WRUNIT, 1209 ) VARB4
          END IF

          VARA4 = 'KTN3'
          VARB4 = 'KRX3'
          WRITE( WRUNIT, 1201 ) VARA4, KTN3
          IF ( KTN3 .NE. 0 ) THEN
             WRITE( WRUNIT, 1203 ) VARB4, VARA4
             WRITE( WRUNIT, 1205 ) VARB4, VARA4

             CALL WRBF6_FORTRAN90( WRUNIT, 10, KTN3, KRX3 )

          ELSE
             WRITE( WRUNIT, 1207 ) VARB4
             WRITE( WRUNIT, 1209 ) VARB4
          END IF

          VARA4 = 'KTN4'
          VARB4 = 'KRX4'
          WRITE( WRUNIT, 1201 ) VARA4, KTN4
          IF ( KTN4 .NE. 0 ) THEN
             WRITE( WRUNIT, 1203 ) VARB4, VARA4
             WRITE( WRUNIT, 1205 ) VARB4, VARA4

             CALL WRBF6_FORTRAN90( WRUNIT, 10, KTN4, KRX4 )

          ELSE
             WRITE( WRUNIT, 1207 ) VARB4
             WRITE( WRUNIT, 1209 ) VARB4
          END IF

          VARA4 = 'KTN5'
          VARB4 = 'KRX5'
          WRITE( WRUNIT, 1201 ) VARA4, KTN5
          IF ( KTN5 .NE. 0 ) THEN
             WRITE( WRUNIT, 1203 ) VARB4, VARA4
             WRITE( WRUNIT, 1205 ) VARB4, VARA4

             CALL WRBF6_FORTRAN90( WRUNIT, 10, KTN5, KRX5 )

          ELSE
             WRITE( WRUNIT, 1207 ) VARB4
             WRITE( WRUNIT, 1209 ) VARB4
          END IF

          VARA4 = 'KTN6'
          VARB4 = 'KRX6'
          WRITE( WRUNIT, 1201 ) VARA4, KTN6
          IF ( KTN6 .NE. 0 ) THEN
             WRITE( WRUNIT, 1203 ) VARB4, VARA4
             WRITE( WRUNIT, 1205 ) VARB4, VARA4

             CALL WRBF6_FORTRAN90( WRUNIT, 10, KTN6, KRX6 )
          ELSE
             WRITE( WRUNIT, 1207 ) VARB4
             WRITE( WRUNIT, 1209 ) VARB4
          END IF

          VARA4 = 'KTN7'
          VARB4 = 'KRX7'
          WRITE( WRUNIT, 1201 ) VARA4, KTN7
          IF ( KTN7 .NE. 0 ) THEN
             WRITE( WRUNIT, 1203 ) VARB4, VARA4
             WRITE( WRUNIT, 1205 ) VARB4, VARA4

             CALL WRBF6_FORTRAN90( WRUNIT, 10, KTN7, KRX7 )

          ELSE
             WRITE( WRUNIT, 1207 ) VARB4
             WRITE( WRUNIT, 1209 ) VARB4
          END IF


      END IF

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     KCNV, KRXCNV
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

!     WRITE( WRUNIT, 1221 ) KCNV
!221  FORMAT( /6X, 'INTEGER, PARAMETER', 3X, ':: KCNV =', I4 )
!     WRITE( WRUNIT, 1223 )
!223  FORMAT(  6X, 'INTEGER', 14X, ':: KRXCNV(KCNV)' )

!     WRITE( WRUNIT, 1225 )
!225  FORMAT( /6X, 'DATA (KRXCNV(IRXXN), IRXXN = 1, KCNV) /' )

!     CALL WRBF6_FORTRAN90( WRUNIT, 10, KCNV, KRXCNV )

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     NWM,NRXWM, NWW,NRXWW, NWO2,NRXWO2, NWN2,NRXWN,, NWCH4,NRXWCH4, NWH2,NRXWH2
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( WRUNIT, 1301 ) NWM
1301  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NWM =', I4  )
      IF ( NWM .NE. 0 ) THEN
         WRITE( WRUNIT, 1303 )
1303     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWM( NWM )' )
         WRITE( WRUNIT, 1305 )
1305     FORMAT( /6X, 'DATA ( NRXWM( IRXXN ), IRXXN = 1, NWM ) /  & ' )
         CALL WRBF6_FORTRAN90( WRUNIT, 10, NWM, NRXWM )
      ELSE
         WRITE( WRUNIT, 1307 )
1307     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWM( 1 )' )
         WRITE( WRUNIT, 1309 )
1309     FORMAT( /6X, 'DATA   NRXWM( 1 )', ' / 0 /' )
      END IF
      IF ( HAS_CONSTS ) THEN
         ISPC = INDEX1 ( 'ATM_AIR', MAXCONSTS, NAMCONSTS )
         WRITE( WRUNIT, 1310 )  CVAL(ISPC)
1310     FORMAT(  6X, 'REAL( 8 ),    PARAMETER ::', 1X, 'ATM_AIR =', 1PD12.5  )
      END IF

      WRITE( WRUNIT, 1311) NWW
1311  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NWW =', I4 )
      IF ( NWW .NE. 0 ) THEN
         WRITE( WRUNIT, 1313 )
1313     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWW( NWW )' )
         WRITE( WRUNIT, 1315 )
1315     FORMAT( /6X, 'DATA ( NRXWW( IRXXN ), IRXXN = 1, NWW ) / & ' )
         CALL WRBF6_FORTRAN90( WRUNIT, 10, NWW, NRXWW )
      ELSE
         WRITE( WRUNIT, 1317 )
1317     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWW( 1 )' )
         WRITE( WRUNIT, 1319 )
1319     FORMAT( /6X, 'DATA   NRXWW( 1 )', ' / 0 / ' )
      END IF

      WRITE( WRUNIT, 1321 ) NWO2
1321  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NWO2 =', I4 )
      IF ( NWO2 .NE. 0 ) THEN
         WRITE( WRUNIT, 1323 )
1323     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWO2( NWO2 )' )
         WRITE( WRUNIT, 1325 )
1325     FORMAT( /6X, 'DATA ( NRXWO2( IRXXN ), IRXXN = 1, NWO2 ) / & ' )
         CALL WRBF6_FORTRAN90( WRUNIT, 10, NWO2, NRXWO2 )
      ELSE
         WRITE( WRUNIT, 1327 )
1327     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWO2( 1 )' )
         WRITE( WRUNIT, 1329 )
1329     FORMAT( /6X, 'DATA   NRXWO2( 1 )', ' / 0 /' )
      END IF
      IF ( HAS_CONSTS ) THEN
         ISPC = INDEX1 ( 'ATM_O2', MAXCONSTS, NAMCONSTS )
         WRITE( WRUNIT, 1330 ) REAL(CVAL(ISPC), 4)
1330     FORMAT(  6X, 'REAL( 8 ),    PARAMETER ::', 1X, 'ATM_O2 =', 1PD12.5  )
      END IF

      WRITE( WRUNIT, 1331 ) NWN2
1331  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NWN2 =', I4 )
      IF ( NWN2 .NE. 0 ) THEN
         WRITE( WRUNIT, 1333 )
1333     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWN2( NWN2 )' )
         WRITE( WRUNIT, 1335 )
1335     FORMAT( /6X, 'DATA ( NRXWN2( IRXXN ), IRXXN = 1, NWN2 ) / & ' )
         CALL WRBF6_FORTRAN90( WRUNIT, 10, NWN2, NRXWN2 )
      ELSE
         WRITE( WRUNIT, 1337 )
1337     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWN2( 1 )' )
         WRITE( WRUNIT, 1339 )
1339     FORMAT( /6X, 'DATA   NRXWN2( 1 )', ' / 0 /' )
      END IF
      IF ( HAS_CONSTS ) THEN
         ISPC = INDEX1 ( 'ATM_N2', MAXCONSTS, NAMCONSTS )
         WRITE( WRUNIT, 1340 ) REAL(CVAL(ISPC), 4)
1340     FORMAT(  6X, 'REAL( 8 ),    PARAMETER ::', 1X, 'ATM_N2 =', 1PD12.5  )
      END IF

      WRITE( WRUNIT, 1341 ) NWCH4
1341  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NWCH4 =', I4 )
      IF ( NWCH4 .NE. 0 ) THEN
         WRITE( WRUNIT, 1343 )
1343     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWCH4( NWCH4 )' )
         WRITE( WRUNIT, 1345 )
1345     FORMAT( /6X, 'DATA ( NRXWCH4( IRXXN ), IRXXN = 1, NWCH4 ) / & ' )
         CALL WRBF6_FORTRAN90( WRUNIT, 10, NWCH4, NRXWCH4 )
      ELSE
         WRITE( WRUNIT, 1347 )
1347     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWCH4( 1 )' )
         WRITE( WRUNIT, 1349 )
1349     FORMAT( /6X, 'DATA   NRXWCH4( 1 )', ' / 0 /' )
      END IF
      IF ( HAS_CONSTS ) THEN
         ISPC = INDEX1 ( 'ATM_CH4', MAXCONSTS, NAMCONSTS )
         WRITE( WRUNIT, 1350 ) REAL(CVAL(ISPC), 4)
1350     FORMAT(  6X, 'REAL( 8 ),    PARAMETER ::', 1X, 'ATM_CH4 =', 1PD12.5  )
      END IF

      WRITE( WRUNIT, 1351 ) NWH2
1351  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NWH2 =', I4 )
      IF ( NWH2 .NE. 0 ) THEN
         WRITE( WRUNIT, 1353 )
1353     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWH2( NWH2 )' )
         WRITE( WRUNIT, 1355 )
1355     FORMAT( /6X, 'DATA ( NRXWH2( IRXXN ), IRXXN = 1, NWH2 ) / & ' )
         CALL WRBF6_FORTRAN90( WRUNIT, 10, NWH2, NRXWH2 )
      ELSE
         WRITE( WRUNIT, 1357 )
1357     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWH2( 1 )' )
         WRITE( WRUNIT, 1359 )
1359     FORMAT( /6X, 'DATA   NRXWH2( 1 )', ' / 0 /' )
      END IF
      IF ( HAS_CONSTS ) THEN
         ISPC = INDEX1 ( 'ATM_H2', MAXCONSTS, NAMCONSTS )
         WRITE( WRUNIT, 1360 ) REAL(CVAL(ISPC), 4)
1360     FORMAT(  6X, 'REAL( 8 ),    PARAMETER ::', 1X, 'ATM_H2 =', 1PD12.5  )
      END IF

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     IRR
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( WRUNIT, 1401 ) MXPRD
1401  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: MXPRD =', I4 )
      
      WRITE( WRUNIT, 1403 )
1403  FORMAT(  6X, 'INTEGER', 12X, ':: IRR( NRXNS,MXPRD+3 )' )

      IF ( REORDER_SPECIES ) THEN ! reset IRR to sorted species from SET_SPARSE_DATA
         DO IRX = 1, NR
            DO ISPC = 1, MXPRD+3
               IRR(IRX, ISPC) = IRM2(ISPC,IRX)
            END DO
         END DO
      END IF

      DO 701 ISPC = 1, MXPRD+3

      WRITE( WRUNIT, 1405 ) ISPC
1405  FORMAT( /6X, 'DATA ( IRR( IRXXN,', I3, ' ), IRXXN = 1, NRXNS ) / & ' )

      CALL WRBF6_FORTRAN90( WRUNIT, 10, NR, IRR( 1:NR,ISPC ) )

701   CONTINUE

      IF( .NOT. LITE )THEN
c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     RTDAT
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

          WRITE( WRUNIT, 1413 ) '1'
1413      FORMAT( /6X, 'DATA ( RTDAT( ', A, ',IRXXN ), IRXXN = 1, NRXNS ) / & ' )

          DO IRX = 1, NR
             DBUFF( IRX ) = RTDAT( 1,IRX )
          END DO
          CALL WRBF12D_FORTRAN90( WRUNIT, 5, NR, DBUFF, 'D' )
   
          WRITE( WRUNIT, 1413) '2'

          DO IRX = 1, NR
             DBUFF( IRX ) = RTDAT( 2,IRX )
          END DO
          CALL WRBF12D_FORTRAN90( WRUNIT, 5, NR, DBUFF, 'D' )

          WRITE( WRUNIT, 1413 ) '3'

          DO IRX = 1, NR
             DBUFF( IRX ) = RTDAT( 3,IRX )
          END DO
          CALL WRBF12D_FORTRAN90( WRUNIT, 5, NR, DBUFF, 'D' )

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     NFALLOFF, IRRFALL
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

          IF ( NFALLOFF .NE. 0 ) THEN
             WRITE( WRUNIT, 1503 )
1503         FORMAT(  6X, 'INTEGER', 12X, ':: IRRFALL( NFALLOFF )' )
             WRITE( WRUNIT, 1505 )
1505         FORMAT( /6X, 'DATA ( IRRFALL( IRXXN ), IRXXN = 1, NFALLOFF ) / & ' )

             CALL WRBF6_FORTRAN90( WRUNIT, 10, NFALLOFF, IRRFALL )
 
          ELSE
             WRITE( WRUNIT, 1507 )
1507         FORMAT(  6X, 'INTEGER', 12X, ':: IRRFALL( 1 )' )
          END IF

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     RFDAT
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

          IF ( NFALLOFF .NE. 0 ) THEN

             WRITE( WRUNIT, 1523 ) '1'
1523         FORMAT( /6X, 'DATA ( RFDAT( ', A, ',IRXXN ), IRXXN = 1, NFALLOFF ) / & ' )
             DO IRX = 1, NFALLOFF
                DBUFF( IRX ) = RFDAT( 1,IRX )
             END DO
             CALL WRBF12D_FORTRAN90( WRUNIT, 5, NFALLOFF, DBUFF, 'D' )

             WRITE( WRUNIT, 1523 ) '2'
             DO IRX = 1, NFALLOFF
                DBUFF( IRX ) = RFDAT( 2,IRX )
             END DO
             CALL WRBF12D_FORTRAN90( WRUNIT, 5, NFALLOFF, DBUFF, 'D' )

             WRITE( WRUNIT, 1523 ) '3'
             DO IRX = 1, NFALLOFF
                DBUFF( IRX ) = RFDAT( 3,IRX )
             END DO
             CALL WRBF12D_FORTRAN90( WRUNIT, 5, NFALLOFF, DBUFF, 'D' )

             WRITE( WRUNIT, 1523 ) '4'
             DO IRX = 1, NFALLOFF
                DBUFF( IRX ) = RFDAT( 4,IRX )
             END DO
             CALL WRBF12D_FORTRAN90( WRUNIT, 5, NFALLOFF, DBUFF, 'D' )

             WRITE( WRUNIT, 1523 ) '5'
             DO IRX = 1, NFALLOFF
                DBUFF( IRX ) = RFDAT( 5,IRX )
             END DO
             CALL WRBF12D_FORTRAN90( WRUNIT, 5, NFALLOFF, DBUFF, 'D' )

          END IF

      END IF
c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     SC
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( WRUNIT, 1551 )
1551  FORMAT( /6X, 'REAL( 8 )', 15X, ':: SC( NRXNS,MXPRD )' )

      DO 801 ISPC = 1, MXPRD

         WRITE( WRUNIT, 1553 ) ISPC
1553     FORMAT( /6X, 'DATA ( SC( IRXXN,', I3, ' ), IRXXN = 1, NRXNS ) / & ' )

         DO IRX = 1, NR
            DBUFF( IRX ) = REAL( SC( IRX,ISPC ), 8 )
         END DO
!         CALL WRBF12S_FORTRAN90 ( WRUNIT, 5, NR, SBUFF, 'F' )
          CALL WRBF12D_FORTRAN90 ( WRUNIT, 5, NR, DBUFF, 'D' )

801   CONTINUE

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     NREACT, NPRDCT
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( WRUNIT, 1601 )
1601  FORMAT( /6X, 'INTEGER', 12X, ':: NREACT( NRXNS )' )

      WRITE( WRUNIT, 1603 )
1603  FORMAT( /6X, 'DATA ( NREACT( IRXXN ), IRXXN = 1, NRXNS ) / & ' )

      CALL WRBF6_FORTRAN90( WRUNIT, 10, NR, NREACT )

      WRITE( WRUNIT, 1605 )
1605  FORMAT(  6X, 'INTEGER', 12X, ':: NPRDCT( NRXNS )' )

      WRITE( WRUNIT, 1607 )
1607  FORMAT( /6X, 'DATA ( NPRDCT( IRXXN ), IRXXN = 1, NRXNS ) / & ' )

      CALL WRBF6_FORTRAN90( WRUNIT, 10, NR, NPRDCT )


c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     MHETERO, IHETERO
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      IF ( MHETERO .NE. 0 ) THEN

         WRITE( WRUNIT, 1721 ) MHETERO
1721     FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: MHETERO =', I4 )

         WRITE( WRUNIT, 1723 )
1723     FORMAT(  6X, 'INTEGER', 12X, ':: IHETERO( MHETERO,2 )' )

         WRITE( WRUNIT, 1725 ) '1'
1725     FORMAT( /6X, 'DATA ( IHETERO( IRXXN,', A, ' ), IRXXN = 1, MHETERO ) / & ' )

         CALL WRBF6_FORTRAN90( WRUNIT, 10, MHETERO, IHETERO( 1:MHETERO,1 ) )

         WRITE( WRUNIT, 1725 ) '2'

         CALL WRBF6_FORTRAN90( WRUNIT, 10, MHETERO, IHETERO( 1:MHETERO,2 ) )

 
      ELSE

         WRITE( WRUNIT, 1727 )
1727     FORMAT( /'! Heteorogeneous reactions information not available ...'
     &           /6X, 'INTEGER, PARAMETER', 1X, ':: MHETERO = 0' )

         WRITE( WRUNIT, 1729 )
1729     FORMAT( /6X, 'INTEGER', 12X, ':: IHETERO( 1,2 )' )

      END IF

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c        PHOTAB AND HETERO
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      IF ( NPHOTAB .NE. 0 ) THEN

         WRITE( WRUNIT, 1711 ) NPHOTAB
1711     FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NPHOTAB =', I4 )

         WRITE( WRUNIT, 1713 )
1713     FORMAT(  6X, 'CHARACTER( 16 )', 4X, ':: PHOTAB( NPHOTAB )' )

         WRITE( WRUNIT, 1715 )
1715     FORMAT( /6X, 'DATA ( PHOTAB( IRXXN ), IRXXN = 1, NPHOTAB ) / & ' )

         DO IRX = 1, NPHOTAB - 1
            WRITE( BUFF20( IRX ), '(1X, "''", A16, "''", ",")' ) PHOTAB( IRX )
         END DO
         WRITE( BUFF20( NPHOTAB ), '(1X, "''", A16, "''", "/")' ) PHOTAB( NPHOTAB )

!         IF( 3 * INT(  REAL( NPHOTAB ) / 3.0 ) .NE.  NPHOTAB )THEN
         IF( 3 * INT(  NPHOTAB / 3 ) .NE.  NPHOTAB )THEN
             NLINES =  NPHOTAB / 3
         ELSE
             NLINES = NPHOTAB / 3 - 1
         END IF
         IFLD1 = 1
         DO IFLD0 = 1, NLINES
            IFLD2 = IFLD1 + 2
            IF( IFLD2 .EQ. NPHOTAB )EXIT
            WRITE( WRUNIT, 1759 ) ( BUFF20( IRX ), IRX = IFLD1, IFLD2 )
1759        FORMAT(5X, '&', 2X, 3A20, ' & ' )
            IFLD1 = IFLD2 + 1
         END DO
         IF ( IFLD1 .LE. NPHOTAB )
     &      WRITE( WRUNIT, 1859 ) ( BUFF20( IRX ), IRX = IFLD1, NPHOTAB )
1859        FORMAT(5X, '&', 2X, 3A20 )



      ELSE

         WRITE( WRUNIT, 1717 )
1717     FORMAT( /'! Photolysis table information not available ...'
     &           /6X, 'INTEGER, PARAMETER', 1X, ':: NPHOTAB = 0' )
                                                
         WRITE( WRUNIT, 1719 )
1719     FORMAT( /'! Photolysis table information not available ...'
     &           /6X, 'CHARACTER( 16 )', 4X, ':: PHOTAB( 1 )' )


      END IF
      
      IF ( NHETERO .NE. 0 ) THEN

         WRITE( WRUNIT, 1761 ) NHETERO
1761     FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NHETERO =', I4 )

         WRITE( WRUNIT, 1763 )
1763     FORMAT(  6X, 'CHARACTER( 16 )', 4X, ':: HETERO( NHETERO )' )

         WRITE( WRUNIT, 1765 )
1765     FORMAT( /6X, 'DATA ( HETERO( IRXXN ), IRXXN = 1, NHETERO ) / & ' )

         DO IRX = 1, NHETERO - 1
            WRITE( BUFF20( IRX ), '(1X, "''", A16, "''", ",")' ) HETERO( IRX )
         END DO
         WRITE( BUFF20( NHETERO ), '(1X, "''", A16, "''", "/")' ) HETERO( NHETERO )

         IF( 3 * INT(  NHETERO / 3 ) .NE.  NHETERO )THEN
             NLINES =  NHETERO / 3
         ELSE
             NLINES = NHETERO / 3 - 1
         END IF
         IFLD1 = 1
         DO IFLD0 = 1, NLINES
            IFLD2 = IFLD1 + 2
            IF( IFLD2 .EQ. NHETERO )EXIT
            WRITE( WRUNIT, 1769 ) ( BUFF20( IRX ), IRX = IFLD1, IFLD2 )
1769        FORMAT(5X, '&', 2X, 3A20, ' &' )
            IFLD1 = IFLD2 + 1
         END DO
         IF ( IFLD1 .LE. NHETERO )
     &      WRITE( WRUNIT, 1859 ) ( BUFF20( IRX ), IRX = IFLD1, NHETERO )

      ELSE

         WRITE( WRUNIT, 1757 )
1757     FORMAT( /'! Heteorogeneous reaction information not available ...'
     &           /6X, 'INTEGER, PARAMETER', 1X, ':: NHETERO = 0' )
                                                
         WRITE( WRUNIT, 1758 )
1758     FORMAT( /'! Heteorogeneous reaction information not available ...'
     &           /6X, 'CHARACTER( 16 )', 4X, ':: HETERO( 1 )' )


      END IF

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c        RXLABEL
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( WRUNIT, 1801 )
1801  FORMAT( /6X, 'CHARACTER( 16 )', 4X, ':: RXLABEL( NRXNS )' )

      WRITE( WRUNIT, 1803 )
1803  FORMAT( /6X, 'DATA ( RXLABEL( IRXXN ), IRXXN = 1, NRXNS ) / & ' )

      CALL WRBF16C_FORTRAN90 ( WRUNIT, 3, NR, RXLABEL )



c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     Fini
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-


      RETURN
      END
