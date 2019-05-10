
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
C $Header: /amber/home/krt/cmq471/models/CCTM/src/driver/yamo/STD_CONC.F,v 1.1.1.1 2010/06/14 16:03:04 sjr Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE SA_LAYERS

C20140428 Has subroutine GET_SA_LAYS
C

      IMPLICIT NONE

C Function: species, layer pointers and definitions for standard CONC
C calculations

      INTEGER, SAVE :: ISAM_BLEV  ! Beginning level saved to conc file
      INTEGER, SAVE :: ISAM_ELEV  ! Ending level saved to conc file
      INTEGER, SAVE :: SA_NLAYS    ! Number of layers saved to conc file
      INTEGER, SAVE :: AISAM_BLEV  ! Beginning level saved to sa_aconc file
      INTEGER, SAVE :: AISAM_ELEV  ! Ending level saved to sa_aconc file
      INTEGER, SAVE :: AVGSA_LAYS    ! Number of layers saved to conc file

      INTERFACE
         SUBROUTINE GET_ENVLIST ( ENV_VAR, NVARS, VAL_LIST )
            IMPLICIT NONE
            CHARACTER( * ),  INTENT ( IN )  :: ENV_VAR
            INTEGER,         INTENT ( OUT ) :: NVARS
            CHARACTER( 16 ), INTENT ( OUT ) :: VAL_LIST( : )
         END SUBROUTINE GET_ENVLIST
      END INTERFACE

      CONTAINS

         SUBROUTINE GET_SA_LAYS ()

C20140428 Determine from run scripty4 number of vertical layers in outputs
C         SA_CONC_1 and SA_ACONC_1.
C
C         Called by driver.F

         USE HGRD_DEFN             ! horizontal domain specifications
         USE VGRD_DEFN             ! vertical layer specifications
         USE UTILIO_DEFN           ! 20120615

!0615    INCLUDE SUBST_IOPARMS     ! I/O parameters definitions

         CHARACTER( 16 ) :: PNAME = 'GET_SA_LAYS'
         CHARACTER( 96 ) :: XMSG = ' '

         INTEGER  V, NV 
         INTEGER :: JDATE = 0
         INTEGER :: JTIME = 0


         CHARACTER( 32 ) :: ISAM_BLEV_ELEV = 'ISAM_BLEV_ELEV'
         CHARACTER( 32 ) :: AISAM_BLEV_ELEV = 'AISAM_BLEV_ELEV'
         CHARACTER( 16 ) :: V_LIST( 2 )

!0605    integer, external :: init3


C-----------------------------------------------------------------------



C Retrieve the layer range used in the concentration file

         CALL GET_ENVLIST ( ISAM_BLEV_ELEV, NV, V_LIST )
         IF ( NV .LE. 0 ) THEN   ! assume 1:NLAYS
            ISAM_BLEV = 1
            ISAM_ELEV = NLAYS
         ELSE IF ( NV .NE. 2 ) THEN
            XMSG = 'Environment variable error for ' // ISAM_BLEV_ELEV
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         ELSE   ! NV = 2
            READ( V_LIST( 1 ), '( I4 )' ) ISAM_BLEV
            READ( V_LIST( 2 ), '( I4 )' ) ISAM_ELEV
            IF ( ISAM_BLEV .LE. 0 .OR. ISAM_ELEV .GT. NLAYS ) THEN
               WRITE( XMSG,
     &            '( "Layer range", 2I4, " invalid for this model" )' )
     &         ISAM_BLEV, ISAM_ELEV
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
            END IF
            IF ( ISAM_BLEV .NE. 1 ) THEN
               WRITE( XMSG,'( "Layer", I3, " Not 1st layer in CGRID" )' )
     &         ISAM_BLEV
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            END IF
         END IF

         SA_NLAYS = ISAM_ELEV - ISAM_BLEV + 1
         IF ( MYPE .EQ. 0 ) 
     &      print*,'In SA_LAYERS.F, ISAM_BLEV, ISAM_ELEV, SA_NLAYS:',ISAM_BLEV, ISAM_ELEV, SA_NLAYS

C Retrieve the layer range used in the sa_aconc file

         CALL GET_ENVLIST ( AISAM_BLEV_ELEV, NV, V_LIST )
         IF ( NV .LE. 0 ) THEN   ! assume 1:NLAYS
            AISAM_BLEV = 1
            AISAM_ELEV = NLAYS
         ELSE IF ( NV .NE. 2 ) THEN
            XMSG = 'Environment variable error for ' // AISAM_BLEV_ELEV
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         ELSE   ! NV = 2
            READ( V_LIST( 1 ), '( I4 )' ) AISAM_BLEV
            READ( V_LIST( 2 ), '( I4 )' ) AISAM_ELEV
            IF ( AISAM_BLEV .LE. 0 .OR. AISAM_ELEV .GT. NLAYS ) THEN
               WRITE( XMSG,
     &            '( "Layer range", 2I4, " invalid for this model" )' )
     &         AISAM_BLEV, AISAM_ELEV
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
            END IF
            IF ( AISAM_BLEV .NE. 1 ) THEN
               WRITE( XMSG,'( "Layer", I3, " Not 1st layer in CGRID" )' )
     &         ISAM_BLEV
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            END IF
         END IF

         AVGSA_LAYS = AISAM_ELEV - AISAM_BLEV + 1

         IF ( AVGSA_LAYS .GT. SA_NLAYS ) THEN
           WRITE( XMSG, '("No. layers in SA_ACONC: ",I4,
     &             " exceeds that in SA_CONC: ", I4," !")' ) AVGSA_LAYS, SA_NLAYS
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         ENDIF ! number of layers

         END SUBROUTINE GET_SA_LAYS

      END MODULE SA_LAYERS
