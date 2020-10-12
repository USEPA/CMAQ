#ifdef sens

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
C $Header: /project/work/rep/arc/CCTM/src/init/yamo/load_cgrid.F,v 1.9 2012/01/19 14:47:23 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE LOAD_SENGRID ( FNAME, JDATE, JTIME, SPC_CAT )

C-----------------------------------------------------------------------
C Function:
C   Initialize the model CGRID array from file data

C Revision history:
C   29 Nov 12 S.L.Napelenok: adapted from load_cgrid for cmaq 5.0
C   12 Dec 19 S.L.Napelenok: ddm-3d implementation for version 5.3.1
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
      USE DDM3D_DEFN, ONLY: NP, NPMAX, SENNUM, SPCNAME, SEN_PAR, SENGRID

      IMPLICIT NONE

      INCLUDE SUBST_CONST       ! constants
      INCLUDE SUBST_FILES_ID    ! file name parameters

C Arguments:

      CHARACTER( 16 ) :: FNAME
      INTEGER      JDATE
      INTEGER      JTIME
      CHARACTER( 2 ) :: SPC_CAT

C Parameters:

C minimum aerosol sulfate concentration [ ug/m**3 ]
c     REAL, PARAMETER :: AEROCONCMIN = 0.001

C The following two factors assume that sulfate density is 1.8e3 [ kg/m**3 ]
C and that the geometric mean diameter and geometric standard deviations
C for the Aitken mode are 0.01e-6 [ m ] and 1.7 respectively
C and are 0.07e-6 and 2.0 respectively for the accumulation mode.

C factor to calculate aerosol number concentration from aerosol sulfate mass
C concentration in the Aitken mode [ ug ].
c     REAL, PARAMETER :: NUMFACT_I = 2.988524 E11

C factor to calculate aerosol number concentration from aerosol sulfate mass
C concentration in the Accumulation mode [ ug ].
c     REAL, PARAMETER :: NUMFACT_J = 3.560191 E08

C fraction of sulfuric acid vapor taken as aerosol for first time step
c     REAL, PARAMETER :: SO4VAPTOAER = 0.999
C initial fraction of total aerosol sulfate in the Aitken mode
c     REAL, PARAMETER :: IFRACATKN = 0.04

      INTEGER, SAVE :: MXSPC
      INTEGER ASTAT

C File variables:

c     REAL      :: DENS( NCOLS,NROWS,NLAYS )       ! air density (kg/m^3)
      REAL      :: RHOJ( NCOLS,NROWS,NLAYS ) ! air density X Jacobian (kg/m^2)

C External Functions:

c     INTEGER, EXTERNAL :: FINDEX       !  looks up number in table.

C Local Variables

c     REAL         MWH2SO4                           ! H2SO4 molec. wt.
c     REAL         H2SO4CONV                         ! ppm -> ug/m**3
c     INTEGER      LSULF                             ! Gas chem CGRID index
c     INTEGER      ISO4AJ, ISO4AI, INUMATKN, INUMACC ! CGRID aerosol indices

      INTEGER      GXOFF, GYOFF               ! global origin offset from file

C for XTRACT3
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3
      INTEGER       :: STRTCOLINI, ENDCOLINI, STRTROWINI, ENDROWINI
      REAL      :: DBUFF( NCOLS,NROWS,NLAYS )

      INTEGER      SPC_STRT
      INTEGER      N_SPCS                     ! no. of species for this call
      INTEGER      NDX                        ! loop copy of INDX
c     INTEGER      ISUR                       ! surrogate index
      INTEGER, ALLOCATABLE, SAVE :: INDX( : ) ! Variable indices for all IC species
c     REAL,    ALLOCATABLE, SAVE :: ICBC_FAC( : ) ! Factor to be applied to ICs
      INTEGER      C, R, L, SPC, V            ! loop counters
      INTEGER      ASPC                       ! CGRID RHOJ pointer
c     INTEGER      NCOLSDENS, NROWSDENS       ! local for DENS

      CHARACTER( 16 ) :: PNAME = 'LOAD_SENGRID'
      CHARACTER( 16 ) :: VNAME
c     CHARACTER( 16 ) :: CONCMIN
      CHARACTER( 96 ) :: XMSG = ' '
      CHARACTER( 40 ) :: CHWARN = 'Domain extents dfrnt from model for '
c     CHARACTER( 24 ) :: ESTR1 = 'No IC found for species '
c     CHARACTER( 34 ) :: ESTR2 = ' '

      LOGICAL, SAVE :: FIRSTIME = .TRUE.

c     INTEGER, SAVE :: LOGDEV

c     INTEGER SENNUM
c     CHARACTER( 8 ) :: SPCNAME
c     CHARACTER( 16) :: SENNAME


C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         LOGDEV = INIT3 ()
         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )
C IOFDESC common now loaded with MET_CRO_3D header

         MXSPC = N_GC_SPC + N_AE_SPC + N_NR_SPC + N_TR_SPC + 1
         ALLOCATE ( INDX( MXSPC*NPMAX ),  STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'ERROR allocating INDX'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF
      END IF

c     WRITE( CONCMIN,'(1PE8.2)' ) CMIN

       write(logdev,*) "BEFORE OPEN load_sengrid"


      IF ( .NOT. OPEN3( FNAME, FSREAD3, PNAME ) ) THEN
         XMSG = 'Could not open ' // FNAME // ' file'
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
 
      IF ( .NOT. DESC3( FNAME ) ) THEN
         XMSG = 'Could not get ' // FNAME // ' file description'
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
 
      IF ( GL_NCOLS .NE. NCOLS3D ) THEN
         WRITE( LOGDEV,* ) ' '
         WRITE( LOGDEV,* ) '    WARNING: ' // CHWARN // FNAME
         WRITE( LOGDEV,* ) '>>  GL_NCOLS: ', GL_NCOLS, '  NCOLS3D: ', NCOLS3D
      END IF
 
      IF ( GL_NROWS .NE. NROWS3D ) THEN
         WRITE( LOGDEV,* ) ' '
         WRITE( LOGDEV,* ) '    WARNING: ' // CHWARN // FNAME
         WRITE( LOGDEV,* ) '>>  GL_NROWS: ', GL_NROWS, '  NROWS3D: ', NROWS3D
      END IF
 
      IF ( NLAYS .NE. NLAYS3D ) THEN
         XMSG = 'Wrong number of layers in ' // FNAME // ' file'
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

c     ESTR2 = ' in ' // TRIM( FNAME ) // '; set to ' // TRIM( CONCMIN )

C Get INDX
!     INDX = 0   ! array assignment
      DO SPC = 1, MXSPC*NPMAX
         INDX( SPC ) = 0
      END DO


       write(logdev,*) "load_sengrid", SPC_CAT

      IF ( SPC_CAT .EQ. 'GC' ) THEN

         WRITE( LOGDEV,1009 ) 'transported gas-phase species'
         SPC_STRT = GC_STRT
         N_SPCS = N_GC_SPC
         DO SPC = 1, N_SPCS
c           SPCNAME = GC_SPC( SPC )( 1:12 )
            DO NP = 1, NPMAX
               SPCNAME = GC_SPC( SPC )( 1:12 )
               SPCNAME = TRIM(SPCNAME) // '_' // SEN_PAR( NP )
               SENNUM = ( SPC - 1 ) * NPMAX + NP
               NDX = INDEX1( SPCNAME, NVARS3D, VNAME3D )
               INDX( SENNUM ) = NDX
               IF ( NDX .EQ. 0 ) THEN
                 XMSG = 'No IC found for sensitivity '
     &                // SPCNAME  // ' in ' // TRIM(FNAME)
     &                // ' Set to zero.'
                 CALL M3MESG ( XMSG )
               END IF
               IF ( INDX( SENNUM ) .GT. 0 )
     &            WRITE( LOGDEV,1013 ) INDX( SPC ), GC_SPC( SPC )
            END DO
         END DO

      ELSE IF ( SPC_CAT .EQ. 'AE' ) THEN

         WRITE( LOGDEV,1009 ) 'transported aerosol species'
         SPC_STRT = AE_STRT
         N_SPCS = N_AE_SPC
         DO SPC = 1, N_SPCS
c           SPCNAME = AE_SPC( SPC )( 1:12 )
            DO NP = 1, NPMAX
               SPCNAME = AE_SPC( SPC )( 1:12 )
               SENNUM = ( SPC + SPC_STRT - 2 ) * NPMAX + NP
               SPCNAME = TRIM(SPCNAME) // '_' // SEN_PAR( NP )
               NDX = INDEX1( SPCNAME, NVARS3D, VNAME3D )
               INDX( SENNUM ) = NDX
               IF ( NDX .EQ. 0 ) THEN
                 XMSG = 'No IC found for sensitivity ' 
     &                // SPCNAME  // ' in ' // TRIM(FNAME) 
     &                // ' Set to zero.'
                 CALL M3MESG ( XMSG )
               END IF
               IF ( INDX( SENNUM ) .GT. 0 )
     &            WRITE( LOGDEV,1013 ) INDX( SPC ), AE_SPC( SPC )
            END DO
         END DO

      ELSE IF ( SPC_CAT .EQ. 'NR' ) THEN

         WRITE( LOGDEV,1009 ) 'transported non-reactive gas species'
         SPC_STRT = NR_STRT
         N_SPCS = N_NR_SPC
         DO SPC = 1, N_SPCS
c           SPCNAME = NR_SPC( SPC )( 1:12 )
            DO NP = 1, NPMAX
               SPCNAME = NR_SPC( SPC )( 1:12 )
               SENNUM = ( SPC + SPC_STRT - 2 ) * NPMAX + NP
               SPCNAME = TRIM(SPCNAME) // '_' // SEN_PAR( NP )
               NDX = INDEX1( SPCNAME, NVARS3D, VNAME3D )
               INDX( SENNUM ) = NDX
               IF ( NDX .EQ. 0 ) THEN
                 XMSG = 'No IC found for sensitivity '
     &                // SPCNAME  // ' in ' // TRIM(FNAME)
     &                // ' Set to zero.'
                 CALL M3MESG ( XMSG )
               END IF
               IF ( INDX( SENNUM ) .GT. 0 )
     &            WRITE( LOGDEV,1013 ) INDX( SPC ), NR_SPC( SPC )
            END DO
         END DO

      ELSE IF ( SPC_CAT .EQ. 'TR' ) THEN

         WRITE( LOGDEV,1009 ) 'transported inert tracer gas species'
         SPC_STRT = TR_STRT
         N_SPCS = N_TR_SPC
         DO SPC = 1, N_SPCS
c           SPCNAME = TR_SPC( SPC )( 1:12 )
            DO NP = 1, NPMAX
               SPCNAME = TR_SPC( SPC )( 1:12 )
               SENNUM = ( SPC + SPC_STRT - 2 ) * NPMAX + NP
               SPCNAME = TRIM(SPCNAME) // '_' // SEN_PAR( NP )
               NDX = INDEX1( SPCNAME, NVARS3D, VNAME3D )
               INDX( SENNUM ) = NDX
               IF ( NDX .EQ. 0 ) THEN
                 XMSG = 'No IC found for sensitivity '
     &                // SPCNAME  // ' in ' // TRIM(FNAME)
     &                // ' Set to zero.'
                 CALL M3MESG ( XMSG )
               END IF
               IF ( INDX( SENNUM ) .GT. 0 )
     &            WRITE( LOGDEV,1013 ) INDX( SPC ), NR_SPC( SPC )
            END DO
         END DO

      ELSE IF ( SPC_CAT .EQ. 'RJ' ) THEN

         N_SPCS = 0
!        WRITE( LOGDEV,'(/ 5X, A)' ) 'loading Density*Jacobian into CGRID'

      ELSE

         XMSG = 'Species categories incorrect for CGRID '
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )

      END IF
        
C Read into SENGRID

      CALL SUBHFILE ( FNAME, GXOFF, GYOFF,
     &                STRTCOLINI, ENDCOLINI, STRTROWINI, ENDROWINI )
C IOFDESC common now loaded with FNAME header

      DO SPC = 1, N_SPCS
         V = SPC_STRT - 1 + SPC
         DO NP = 1, NPMAX
            SENNUM = ( SPC_STRT - 2 + SPC ) * NPMAX + NP
            NDX = INDX( SENNUM )

            IF ( NDX .GT. 0 ) THEN
               IF ( .NOT. XTRACT3( FNAME, VNAME3D ( NDX ),
     &              1,NLAYS, STRTROWINI,ENDROWINI, STRTCOLINI,ENDCOLINI,
     &              JDATE, JTIME, DBUFF ) ) THEN
                  XMSG = 'Could not read ' // TRIM( VNAME3D( NDX ) )
     &                 // ' from ' // FNAME
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
               DO L = 1, NLAYS
                  DO R = 1, NROWS
                     DO C = 1, NCOLS
                        SENGRID( C,R,L,NP,V ) = DBUFF( C,R,L )
                     END DO
                  END DO
               END DO
            ELSE
               DO L = 1, NLAYS
                  DO R = 1, NROWS
                     DO C = 1, NCOLS
                        SENGRID( C,R,L,NP,V ) = 0.0
                     END DO
                  END DO
               END DO
            END IF   ! INDX .GT. 0
         END DO
      END DO

      IF ( N_SPCS .NE. 0 ) WRITE( LOGDEV,'(/ 5X, A)' )
     &                            SPC_CAT // ' loaded into SENGRID'

      IF ( SPC_CAT .EQ. 'RJ' ) THEN

C Load RHOJ for transport and mixing ratio advection adjustment

         VNAME = 'DENSA_J'
         IF ( .NOT. XTRACT3( MET_CRO_3D, VNAME,
     &              1,NLAYS, STRTROWMC3,ENDROWMC3, STRTCOLMC3,ENDCOLMC3,
     &              JDATE, JTIME, RHOJ ) ) THEN
             XMSG = 'Could not read DENSA_J from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ASPC = GC_STRT - 1 + N_GC_SPCD
         DO NP = 1, NPMAX
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     SENGRID( C,R,L,NP,ASPC ) = RHOJ( C,R,L )
                  END DO
               END DO
            END DO
         END DO

         WRITE( LOGDEV,'(/ 5X, A)' ) 'Dens*Jacobian loaded into SENGRID'


      END IF

C Close the file

!     IF ( .NOT. CLOSE3( FNAME ) ) THEN
!        XMSG = 'Could not close ' // FNAME // ' file'
!        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!     END IF

      RETURN

1009  FORMAT( / 5X, 'IC/BC Factors used for ', A )
1013  FORMAT( 5X, I3, 2X, A, 1PG13.5 )
      END

#endif
