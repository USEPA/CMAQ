
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
C $Header$

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)GETRATE.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.GETRATE.F 02 Jan 1997 15:26:44

C @(#)GETRATE.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.GETRATE.F 02 Jan 1997 15:26:44

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETRATE ( IMECH, INBUF, LPOINT, IEOL, CHR,
     &                     NXX, LABEL,
     &                     KTYPE, IRXBITS, NFALLOFF, IRRFALL,
     &                     IPH, IP, NPHOTAB, PHOTAB,
     &                     ISPECIAL, NSPECIAL_RXN, NSPECIAL, SPECIAL )

C=======================================================================
C sets up rate constant information
C input:
C         NXX          = current rx index
C         LABEL        = possible rx label
C                        LABEL(NXX,1): 1st label found in rx NXX
C                        LABEL(NXX,2): 2nd label found in rx NXX
C output:
C         NFALLOFF     = Number of falloff reactions
C         IRRFALL      = Reactions list pointer to falloff reactions
C         IPH(IP,1)    = Mech. rx number for this phot rx
C         IPH(IP,2)    = Photolysis table index
C         IPH(IP,3)    = 1, if dependent photolysis rx, else = 0
C         IP           = Total number of phot rx's
C         NPHOTAB      = Number of photolysis tables found
C         PHOTAB       = photolysis table list
C         ISPECIAL(IP,1)    = Mech. rx number for this special rate coeff.
C         ISPECIAL(IP,2)         = Special rate coeff. index
C         NSPECIAL_RXN      = Total number of rx's using special rates
C         NSPECIAL          = Number of special rate coefficients
C         PHOTAB            = List of Special Rate Coefficients
C=======================================================================

      USE MECHANISM_DATA
      
      IMPLICIT NONE
!      INCLUDE 'PARMS.e'
!      INCLUDE 'CHMECH.e'
      CHARACTER(  1 ) :: CHR
      CHARACTER( 81 ) :: INBUF
      INTEGER IMECH, LPOINT, IEOL
      INTEGER IP, NXX
      INTEGER IPH( MAXPHOTRXNS,3 )
      INTEGER NPHOTAB
      CHARACTER( 16 ) :: PHOTAB( MAXPHOTRXNS ) ! photolysis table label

      INTEGER NSPECIAL_RXN
      INTEGER ISPECIAL( MAXSPECRXNS,2 )
      INTEGER NSPECIAL
      CHARACTER( 16 ) :: SPECIAL( MAXSPECRXNS )

      CHARACTER( 16 ) :: TAG, LABEL( MAXRXNUM,2 )
      INTEGER NDX
      INTEGER, EXTERNAL :: INDEX1
      REAL( 8 ) ::  NUMBER

      INTEGER NUMANDS, NUMREALS, IRX

      NUMANDS = 0         ! no. of ampersands in mech. description (falloff)
      NUMREALS = 0        ! counter to switch signs (falloff)

C '#' or '%' signals beginning of part of line that has rate constant data
      IF ( CHR .EQ. '#' ) THEN
         
         CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
         CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )

         KTYPE( NXX ) = 1   ! posit type 1 standard reaction
         RTDAT( 1,NXX ) = NUMBER



         IF ( CHR .NE. '^' .AND.    ! type 2 or 4
     &        CHR .NE. '@' .AND.    ! type 3 or 4
     &        CHR .NE. '&' .AND.    ! falloff
     &        CHR .NE. '*' .AND.    ! 'refer back'
     &        CHR .NE. '?' .AND.    ! Special Rate Constant
     &        CHR .NE. '/' .AND.    ! photo
     &        CHR .NE. ';' ) THEN   ! eol
            WRITE( *, 2001 ) NXX, INBUF
            STOP
         END IF


101      CONTINUE


         IF ( CHR .EQ. '^' ) THEN
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            CALL GETREAL (IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
            IF ( NUMANDS .EQ. 0 ) THEN
               KTYPE( NXX ) = 2
               RTDAT( 2,NXX ) = NUMBER
            ELSE
               RFDAT( 2,NFALLOFF ) = NUMBER
            END IF               
            IF ( CHR .NE. '@' .AND. 
     &           CHR .NE. '&' .AND. 
     &           CHR .NE. ';' ) THEN
               WRITE( *,2003 ) NXX, INBUF
               STOP
            END IF
            GO TO 101
         END IF     ! CHR .EQ. '^'

         IF ( CHR .EQ. '@' ) THEN
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
            IF ( CHR .EQ. '*' ) THEN  !  reverse equilibrium
               KTYPE( NXX ) = 5
               RTDAT( 2,NXX ) = -NUMBER
               CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
               IF ( CHR .NE. 'E' ) THEN
                  WRITE( *,2005 ) NXX, INBUF
                  STOP
               ELSE
                  CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
                  IF ( CHR .EQ. '<' ) THEN
                     CALL GETLABEL ( IMECH, INBUF, LPOINT, IEOL, CHR,
     &                               LABEL( NXX,2 ) )
                  ELSE
                     WRITE( *,2007 ) NXX, INBUF
                     STOP
                  END IF
               END IF
               IF ( CHR .NE. ';' ) THEN
                  WRITE( *,2009 ) NXX, INBUF
                  STOP
               ELSE
                  GO TO 901
               END IF
            ELSE       ! CHR .ne. '*'
               IF ( KTYPE( NXX ) .EQ. 1 ) THEN
                  KTYPE( NXX ) = 3
               ELSE IF ( KTYPE( NXX ) .EQ. 2 ) THEN
                  KTYPE( NXX ) = 4
               END IF
               IF ( NUMANDS .EQ. 0 ) THEN
                  RTDAT( 3,NXX ) = -NUMBER
               ELSE
                  RFDAT( 3,NFALLOFF ) = -NUMBER
               END IF
               GO TO 101
            END IF        ! CHR .EQ. '*'
         END IF      ! CHR .EQ. '@'

         IF ( CHR .EQ. '&' ) THEN
            NUMANDS = NUMANDS + 1
            IF ( NUMANDS .EQ. 1 ) THEN
               IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 0 )
               NFALLOFF = NFALLOFF + 1
               IRRFALL( NFALLOFF ) = NXX
               KTYPE( NXX ) = 10
               RFDAT( 4,NFALLOFF ) = 0.6      ! default F
               RFDAT( 5,NFALLOFF ) = 1.0      ! default n
            END IF
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
            IF ( NUMANDS .EQ. 1 ) THEN
               RFDAT( 1,NFALLOFF ) = NUMBER
            ELSE IF ( NUMANDS .EQ. 2 ) THEN  ! override default F
               RFDAT( 4,NFALLOFF ) = NUMBER
            ELSE IF ( NUMANDS .EQ. 3 ) THEN  ! override default n
               RFDAT( 5,NFALLOFF ) = NUMBER
            END IF
            IF ( NUMANDS .EQ. 1 ) THEN
               IF ( CHR .NE. '^' .AND. 
     &              CHR .NE. '@' .AND. 
     &              CHR .NE. '&' .AND. 
     &              CHR .NE. ';' ) THEN
                  WRITE( *,2011 ) NXX, INBUF
                  STOP
               END IF
            ELSE IF ( NUMANDS .EQ. 2 ) THEN
               IF ( CHR .NE. '&' .AND. 
     &              CHR .NE. ';' ) THEN
                  WRITE( *,2013 ) NXX, INBUF
                  STOP
               END IF
            ELSE ! if NUMANDS > 2
               IF ( CHR .NE. ';' ) THEN
                  WRITE( *,2014 ) NXX, INBUF
                  STOP
               END IF
            END IF
            GO TO 101
         END IF      ! CHR .EQ. '&'

         IF ( CHR .EQ. '/' ) THEN        ! photolytic rx
            KTYPE( NXX ) = 0
            IRXBITS( NXX ) = IBSET ( IRXBITS( NXX ), 1 )
            IP = IP + 1
            IPH( IP,1 ) = NXX
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            IF ( CHR .EQ. '<' ) THEN
               IPH( IP,3 ) = IP
               CALL GETLABEL ( IMECH, INBUF, LPOINT, IEOL, CHR, TAG )
               NDX = INDEX1 ( TAG, NPHOTAB, PHOTAB )
               IF ( NDX .NE. 0 ) THEN     ! table label already found
                  IPH( IP,2 ) = NDX  
               ELSE                  ! new table label
                  NPHOTAB = NPHOTAB + 1
                  IPH( IP,2 ) = NPHOTAB
                  PHOTAB( NPHOTAB ) = TAG
               END IF
            ELSE IF ( CHR .EQ. '*' ) THEN
               IPH( IP,3 ) = 0
               CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
               IF ( CHR .EQ. '<' ) THEN
                  CALL GETLABEL ( IMECH, INBUF, LPOINT, IEOL, CHR, LABEL( NXX,2 ) )
C                 IPH(IP,2) to be resolved in caller (CHEMMECH.f)
               ELSE
                  WRITE( *,2007 ) NXX, INBUF
                  STOP
               END IF
            ELSE
               WRITE( *,2035 ) NXX, INBUF
               STOP
            END IF
            IF ( CHR .NE. ';' ) THEN
               WRITE( *,2017 ) NXX, INBUF
               STOP
            ELSE
               GO TO 901
            END IF
         END IF      ! CHR .EQ. '/'

         IF ( CHR .EQ. '*' ) THEN     ! linear dependency reaction
            KTYPE( NXX ) = 6     
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            IF ( CHR .NE. 'K' ) THEN
               WRITE( *,2019 ) NXX, INBUF
               STOP
            ELSE
               CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
               IF ( CHR .EQ. '<' ) THEN
                  CALL GETLABEL ( IMECH, INBUF, LPOINT, IEOL, CHR,
     &                            LABEL( NXX,2 ) )
               ELSE
                  WRITE( *,2007 ) NXX, INBUF
                  STOP
               END IF
            END IF
            IF ( CHR .NE. ';' ) THEN
               WRITE( *,2021 ) NXX, INBUF
               STOP
            ELSE
               GO TO 901 
            END IF
         END IF      ! CHR .EQ. '*'

         IF ( CHR .EQ. '?' ) THEN     ! Special rate coefficient

            KTYPE( NXX ) = 11
            
            NSPECIAL_RXN = NSPECIAL_RXN + 1
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            CALL GETWORD ( IMECH, INBUF, LPOINT, IEOL, CHR, TAG )
            NDX = INDEX1 ( TAG, NSPECIAL, SPECIAL )

            IF ( NDX .NE. 0 ) THEN     ! special found
                  ISPECIAL( NSPECIAL_RXN,1 ) = NXX
                  ISPECIAL( NSPECIAL_RXN,2 ) = NDX  
            ELSE              
               WRITE( *,2041 ) NXX, INBUF
               STOP
            END IF
            IF ( CHR .NE. ';' ) THEN
               WRITE( *,2042 ) NXX, INBUF
               STOP
            ELSE
               GO TO 901 
            END IF
 
         ENDIF

          
         IF ( CHR .EQ. ';' ) GO TO 901

      ELSE IF ( CHR .EQ. '%' ) THEN

         CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
         IF ( CHR .EQ. '1' ) THEN
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            IF ( CHR .NE. '#' ) THEN
               WRITE( *,2023 ) NXX, INBUF
               STOP
            ELSE
               CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
               CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
               KTYPE( NXX ) = 7
               RTDAT( 1,NXX ) = NUMBER
!              IF( CHR .EQ. '@' )THEN
!                  CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
!                  RTDAT( 2,NXX ) =  -NUMBER
!                  CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
!              ELSE
!                  RTDAT( 2,NXX ) = 0.0
!              END IF      
               IF ( CHR .NE. ';' ) THEN
                  WRITE( *,2025 ) NXX, INBUF
                  STOP
               ELSE
                  GO TO 901
               END IF
            END IF     ! CHR .NE. '#'

         ELSE IF ( CHR .EQ. '2' ) THEN    ! treated as falloff
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            IF ( CHR .NE. '#' ) THEN
               WRITE( *,2023 ) NXX, INBUF
               STOP
            ELSE
               NFALLOFF = NFALLOFF + 1
               KTYPE( NXX ) = 8
               IRRFALL( NFALLOFF ) = NXX
               NUMREALS = 0
201            CONTINUE
               CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
               CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
               NUMREALS = NUMREALS + 1
               IF ( NUMREALS .EQ. 2 .OR. 
     &              NUMREALS .EQ. 4 .OR. 
     &              NUMREALS .EQ. 6 )
     &            NUMBER = -NUMBER
               IF ( NUMREALS .LE. 3 ) THEN
                  RTDAT( NUMREALS,NXX ) = NUMBER
               ELSE
                  RFDAT( NUMREALS-3,NFALLOFF ) = NUMBER
               END IF
               IF ( NUMREALS .EQ. 1 .OR. 
     &              NUMREALS .EQ. 3 .OR. 
     &              NUMREALS .EQ. 5 ) THEN
                  IF ( CHR .NE. '@' ) THEN
                     WRITE( *,2027 ) NXX, INBUF
                     STOP
                  END IF
                  GO TO 201
               ELSE IF ( NUMREALS .LE. 4 ) THEN
                  IF ( CHR .NE. '&' ) THEN
                     WRITE( *,2029 ) NXX, INBUF
                     STOP
                  END IF
                  GO TO 201
               ELSE IF ( NUMREALS .GE. 6 ) THEN
                  IF ( CHR .NE. ';' ) THEN
                     WRITE( *,2029 ) NXX, INBUF
                     STOP
                  END IF
                  GO TO 901
               END IF                                      
            END IF     ! CHR .NE. '#'

         ELSE IF ( CHR .EQ. '3' ) THEN    ! treated as falloff
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            IF ( CHR .NE. '#' ) THEN
               WRITE( *,2023 ) NXX, INBUF
               STOP
            ELSE
               NFALLOFF = NFALLOFF + 1
               KTYPE( NXX ) = 9
               IRRFALL( NFALLOFF ) = NXX
               NUMREALS = 0
301            CONTINUE
               CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
               CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, NUMBER )
               NUMREALS = NUMREALS + 1
               IF ( NUMREALS .EQ. 2 .OR. 
     &              NUMREALS .EQ. 4 )
     &            NUMBER = -NUMBER
               IF ( NUMREALS .LE. 3 ) THEN
                  RTDAT( NUMREALS,NXX ) = NUMBER
               ELSE
                  RFDAT( NUMREALS-3,NFALLOFF ) = NUMBER
               END IF
               IF ( NUMREALS .EQ. 1 .OR. 
     &              NUMREALS .EQ. 3 ) THEN 
                  IF ( CHR .NE. '@' ) THEN
                     WRITE( *,2027 ) NXX, INBUF
                     STOP
                  END IF
                  GO TO 301
               ELSE IF ( NUMREALS .EQ. 2 ) THEN
                  IF ( CHR .NE. '&' ) THEN
                     WRITE( *,2029 ) NXX, INBUF
                     STOP
                  END IF
                  GO TO 301
               ELSE IF ( NUMREALS .GE. 4 ) THEN
                  IF ( CHR .NE. ';' ) THEN
                     WRITE( *,2029 ) NXX, INBUF
                     STOP
                  END IF
                  GO TO 901
               END IF                                      
            END IF     ! CHR .NE. '#'

         ELSE
            WRITE( *,2031 ) NXX, INBUF
            STOP

         END IF   ! CHAR .EQ. '1'

      ELSE

         WRITE( *,2033 ) NXX, INBUF
         STOP

      END IF      ! CHR .EQ. '#'   

901   CONTINUE

      IF ( NUMANDS .EQ. 0 ) THEN
         IF ( KTYPE( NXX ) .EQ. 1 ) THEN
            KTN1 = KTN1 + 1
            KRX1( KTN1 ) = NXX
!           KCNV = KCNV + 1
!           KRXCNV( KCNV ) = NXX
         ELSE IF ( KTYPE( NXX ) .EQ. 2 ) THEN
            KTN2 = KTN2 + 1
            KRX2( KTN2 ) = NXX
!           KCNV = KCNV + 1
!           KRXCNV( KCNV ) = NXX
         ELSE IF ( KTYPE( NXX ) .EQ. 3 ) THEN
            KTN3 = KTN3 + 1 
            KRX3( KTN3 ) = NXX
!           KCNV = KCNV + 1
!           KRXCNV( KCNV ) = NXX
         ELSE IF ( KTYPE( NXX ) .EQ. 4 ) THEN
            KTN4 = KTN4 + 1
            KRX4( KTN4 ) = NXX
!           KCNV = KCNV + 1
!           KRXCNV( KCNV ) = NXX
         ELSE IF ( KTYPE( NXX ) .EQ. 5 ) THEN
            KTN5 = KTN5 + 1
            KRX5( KTN5 ) = NXX
C           reverse equil. rx and 1st order: must undo forward rx conversion

         ELSE IF ( KTYPE( NXX ) .EQ. 6 ) THEN
            KTN6 = KTN6 + 1
            KRX6( KTN6 ) = NXX
!!          KCNV = KCNV + 1
!!          KRXCNV( KCNV ) = NXX
         ELSE IF ( KTYPE( NXX ) .EQ. 7 ) THEN
            KTN7 = KTN7 + 1
            KRX7( KTN7 ) = NXX
!           KCNV = KCNV + 1
!           KRXCNV( KCNV ) = NXX
         END IF
      END IF     ! NUMANDS .EQ. 0            
      RETURN

2001  FORMAT( / 5X, '*** ERROR: ',
     &              'Incorrect symbol following first #A'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2003  FORMAT( / 5X, '*** ERROR: ',
     &              '@, &, or ; expected after the ^B'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2005  FORMAT( / 5X, '*** ERROR: ',
     &              'E expected after * in reaction type A@C*En'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2007  FORMAT( / 5X, '*** ERROR: ',
     &              '< expected as start of label string'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2009  FORMAT( / 5X, '*** ERROR: ',
     &              '; expected after n in reaction type A@C*En'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2011  FORMAT( / 5X, '*** ERROR: ',
     &              '@, *, &, or ; expected after first &A'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2013  FORMAT( / 5X, '*** ERROR: ',
     &              '& or ; expected after second &F'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2014  FORMAT( / 5X, '*** ERROR: ',
     &              '; expected after third &n'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2015  FORMAT( / 5X, '*** ERROR: ',
     &              'A or R must follow / symbol'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2017  FORMAT( / 5X, '*** ERROR: ',
     &              '; must follow /Ln or /Rn'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2019  FORMAT( / 5X, '*** ERROR: ',
     &              'K expected after * for special reaction A*Kn'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2021  FORMAT( / 5X, '*** ERROR: ',
     &              '; expected after special reaction a*Kn'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2023  FORMAT( / 5X, '*** ERROR: ',
     &              '# must follow 1 or 2 0r 3 in % reactions'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2025  FORMAT( / 5X, '*** ERROR: ',
     &              '; must follow %1 A'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2027  FORMAT( / 5X, '*** ERROR: ',
     &              '@ must follow #A in %2 or %3 reaction'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2029  FORMAT( / 5X, '*** ERROR: ',
     &              '& or ; must follow @C in %2 or %3 reaction'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2031  FORMAT( / 5X, '*** ERROR: ',
     &              '1 or 2 or 3 must follow % in special reactions'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2033  FORMAT( / 5X, '*** ERROR: ',
     &              'Invalid character to start rate constant'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2035  FORMAT( / 5X, '*** ERROR: ',
     &              '* expected as alternate photolysis dependency reaction'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2041  FORMAT( / 5X, '*** ERROR: ',
     &              '; Special rate coefficient not found in Special Table'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
2042  FORMAT( / 5X, '*** ERROR: ',
     &              '; expected after special reaction rate coefficient a?R'
     &        / 5X, 'Processing for reaction number:', I6
     &        / 5X, 'Last line read was:' / A81 )
      END
