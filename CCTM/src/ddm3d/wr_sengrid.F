#ifdef sens
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
C $Header: /project/work/rep/CCTM/src/driver/yamo_ddm3d/wr_sengrid.F,v 1.1 2009/09/29 13:53:40 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      SUBROUTINE WR_SENGRID ( JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C Function:
C Save the instantaneous state of SENGRID in an I/O-API "circular buffer"
C file to use for restart/continuation for subsequent simulation. 

C Revision History:
C      Aug 08 S.L.Napelenok: initial (based on WR_CGRID)
C   09 Nov 12 S.L.Napelenok: update for cmaq 5.0.1
C   14 Oct 15 S.L.Napelenok: update for cmaq 5.1
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE CGRID_SPCS            ! CGRID species number and offsets
      USE UTILIO_DEFN

#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif

      USE DDM3D_DEFN

      IMPLICIT NONE

C Include Files:

      INCLUDE SUBST_FILES_ID    ! file name parameters

      INTEGER      JDATE                       ! current model date, coded YYYYDDD
      INTEGER      JTIME                       ! current model time, coded HHMMSS
      INTEGER      TSTEP                       ! output timestep (HHMMSS)

C Local variables:

      CHARACTER( 16 ) :: PNAME = 'WR_SENGRID'
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER K, MXK, SPC, VAR
      INTEGER ALLOCSTAT

      INTEGER TSTEP_RF, NTHIK_RF, NCOLS_RF, NROWS_RF, GDTYP_RF
      REAL( 8 ) :: P_ALP_RF, P_BET_RF, P_GAM_RF
      REAL( 8 ) :: XCENT_RF, YCENT_RF
      REAL( 8 ) :: XORIG_RF, YORIG_RF
      REAL( 8 ) :: XCELL_RF, YCELL_RF
      INTEGER VGTYP_RF 
      REAL VGTOP_RF

c     INTEGER, SAVE :: LOGDEV       ! FORTRAN unit number for log file
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL OK

c     CHARACTER( 8 )  :: SPCNAME

      REAL, POINTER :: WRBUFF(:,:,:,:)
      INTEGER :: c, r, s, d, i

      LOGICAL, EXTERNAL :: FLUSH3

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.
         LOGDEV = INIT3 ()

         ALLOCATE ( WRBUFF( NCOLS,NROWS,NLAYS,NSPCSD*NPMAX ), STAT = ALLOCSTAT  )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'WRBUFF memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Try to open existing file for update

         CALL SUBST_BARRIER
         OK = OPEN3( CTM_SENS_1, FSRDWR3, PNAME )
         CALL SUBST_GLOBAL_LOGICAL( OK, 'AND' )
         IF ( .NOT. OK ) THEN

            XMSG = 'Could not open ' // TRIM( CTM_SENS_1 )
     &           // ' file for update - try to open new'
            CALL M3MESG( XMSG )

c           IF ( MYPE .EQ. 0 ) THEN
            IF ( IO_PE_INCLUSIVE ) THEN

C Get default file header attibutes from CONC file (assumes file already open)

               IF ( .NOT. DESC3( CTM_CONC_1 ) ) THEN
                  XMSG = 'Could not get '
     &                 // TRIM( CTM_CONC_1 )
     &                 // ' file description'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               SDATE3D = JDATE
               STIME3D = JTIME

C Get SENGRID nvars

               NVARS3D = NSPCSD * NPMAX
               NLAYS3D = NLAYS

C Set file header attributes that differ from ASENS and open the file

               FDESC3D = ' '
               FDESC3D( 1 ) = 'Computational grid instantaneous sensitivities'
               FDESC3D( 2 ) = '- for scenario continuation.'

               WRITE( LOGDEV,* ) ' '
               WRITE( LOGDEV,* ) '       State SENGRID File Header Description:'
               DO K = 1, 2
                  WRITE( LOGDEV,* ) '    => ',
     &            TRIM( FDESC3D( K ) )
               END DO

C Create the names of the sensitivity outputs

C Gas species

               VAR = 0

               DO SPC = 1, N_GC_SPC
                  SPCNAME = GC_SPC( SPC )( 1:12 )
                  DO NP = 1, NPMAX
                     VAR = VAR + 1
                     VTYPE3D( VAR ) = M3REAL
                     VNAME3D( VAR ) = TRIM(SPCNAME) // '_' // SEN_PAR( NP )
                     UNITS3D( VAR ) = 'ppmV'
                     VDESC3D( VAR ) = 'Sens of ' // SPCNAME // ' to ' // SEN_PAR( NP )
                  END DO
               END DO

C RHOJ - should be zero

               SPCNAME = 'RHOJ'
               DO NP = 1, NPMAX
                  VAR = VAR + 1
                  VTYPE3D( VAR ) = M3REAL
                  VNAME3D( VAR ) = TRIM(SPCNAME) // '_' // SEN_PAR( NP )
                  UNITS3D( VAR ) = 'm*Kg/m**3'
                  VDESC3D( VAR ) = 'Sens of ' // SPCNAME // ' to ' // SEN_PAR( NP )
               END DO

C Aerosol species

               DO SPC = 1, N_AE_SPC
                  SPCNAME = AE_SPC( SPC )( 1:12 )
                  DO NP = 1, NPMAX
                     VAR = VAR + 1
                     VTYPE3D( VAR ) = M3REAL
                     VNAME3D( VAR ) = TRIM(SPCNAME) // '_' // SEN_PAR( NP )
                     IF ( VNAME3D( VAR )(1:3) .EQ. 'NUM' ) THEN
                        UNITS3D( VAR ) = 'number/m**3'
                     ELSE IF ( VNAME3D( VAR )(1:3) .EQ. 'SRF' ) THEN
                        UNITS3D( VAR ) = 'm**2/m**3'
                     ELSE
                        UNITS3D( VAR ) = 'micrograms/m**3'
                     END IF
                     VDESC3D( VAR ) = 'Sens of ' // SPCNAME // ' to ' // SEN_PAR( NP )
                  END DO
               END DO

C NR species

               DO SPC = 1, N_NR_SPC
                  SPCNAME = NR_SPC( SPC )( 1:12 )
                  DO NP = 1, NPMAX
                     VAR = VAR + 1
                     VTYPE3D( VAR ) = M3REAL
                     VNAME3D( VAR ) = TRIM(SPCNAME) // '_' // SEN_PAR( NP )
                     UNITS3D( VAR ) = 'ppmV'
                     VDESC3D( VAR ) = 'Sens of ' // SPCNAME // ' to ' // SEN_PAR( NP )
                  END DO
               END DO

c TR species

               DO SPC = 1, N_TR_SPC
                  SPCNAME = TR_SPC( SPC )( 1:12 )
                  DO NP = 1, NPMAX
                     VAR = VAR + 1
                     VTYPE3D( VAR ) = M3REAL
                     VNAME3D( VAR ) = TRIM(SPCNAME) // '_' // SEN_PAR( NP )
                     UNITS3D( VAR ) = 'ppmV'
                     VDESC3D( VAR ) = 'Sens of ' // SPCNAME // ' to ' // SEN_PAR( NP )
                  END DO
               END DO

c open the file

               IF ( .NOT. OPEN3( CTM_SENS_1, FSNEW3, PNAME ) ) THEN
                  XMSG = 'Could not open '
     &                 // TRIM( CTM_SENS_1 )  // ' file'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

            END IF   ! MYPE = 0

         ELSE

C File exists. Check header data with CONC file as reference. Currently only
C proc 0 has CTM_CONC_1 open

c           IF ( MYPE .EQ. 0 ) THEN
            IF ( IO_PE_INCLUSIVE ) THEN

               IF ( .NOT. DESC3( CTM_CONC_1 ) ) THEN
                  XMSG = 'Could not get '
     &                 // TRIM( CTM_CONC_1 )
     &                 // ' file description'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               TSTEP_RF = TSTEP3D
               NTHIK_RF = NTHIK3D
               NCOLS_RF = NCOLS3D
               NROWS_RF = NROWS3D
               GDTYP_RF = GDTYP3D
               P_ALP_RF = P_ALP3D
               P_BET_RF = P_BET3D
               P_GAM_RF = P_GAM3D
               XCENT_RF = XCENT3D
               YCENT_RF = YCENT3D
               XORIG_RF = XORIG3D
               YORIG_RF = YORIG3D
               XCELL_RF = XCELL3D
               YCELL_RF = YCELL3D
               VGTYP_RF = VGTYP3D
               VGTOP_RF = VGTOP3D

               IF ( .NOT. DESC3( CTM_SENS_1 ) ) THEN
                  XMSG = 'Could not get '
     &                 // TRIM( CTM_SENS_1 )
     &                 // ' file description'
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               IF ( TSTEP_RF .NE. ABS( TSTEP3D ) .OR.
     &              NTHIK_RF .NE. NTHIK3D .OR.
     &              NCOLS_RF .NE. NCOLS3D .OR.
     &              NROWS_RF .NE. NROWS3D .OR.
     &              GDTYP_RF .NE. GDTYP3D ) THEN
                    XMSG = 'Header inconsistent on existing CTM_SENS_1'
                    CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF
               IF ( P_ALP_RF .NE. P_ALP3D .OR.
     &              P_BET_RF .NE. P_BET3D .OR.
     &              P_GAM_RF .NE. P_GAM3D ) THEN
                    XMSG = 'Header inconsistent on existing CTM_SENS_1'
                    CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF
               IF ( XCENT_RF .NE. XCENT3D .OR.
     &              YCENT_RF .NE. YCENT3D ) THEN
                    XMSG = 'Header inconsistent on existing CTM_SENS_1'
                    CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF
               IF ( XORIG_RF .NE. XORIG3D .OR.
     &              YORIG_RF .NE. YORIG3D ) THEN
                    XMSG = 'Header inconsistent on existing CTM_SENS_1'
                    CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF
               IF ( XCELL_RF .NE. XCELL3D .OR.
     &              YCELL_RF .NE. YCELL3D ) THEN
                    XMSG = 'Header inconsistent on existing CTM_SENS_1'
                    CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF
               IF ( VGTYP_RF .NE. VGTYP3D ) THEN
                    XMSG = 'Header inconsistent on existing CTM_SENS_1'
                    CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF
               IF ( VGTOP_RF .NE. VGTOP3D ) THEN
                    XMSG = 'Header inconsistent on existing CTM_SENS_1'
                    CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF

            END IF   ! MYPE = 0

         END IF   ! .NOT. OPEN CTM_SENS_1

c         CALL SUBST_BARRIER

      END IF   ! FIRSTIME


      i = 0
      do s = 1, NSPCSD
         do d = 1, NPMAX
            i = i + 1
            do k = 1, NLAYS
               do r = 1, NROWS
                  do c = 1, NCOLS
                     WRBUFF(c,r,k,i) = SENGRID(c,r,k,d,s)
                  end do
               end do
            end do
         end do
      end do

#ifdef parallel_io
      IF ( IO_PE_INCLUSIVE ) THEN
         IF ( .NOT. FLUSH3 ( CTM_SENS_1 ) ) THEN
            XMSG = 'Could not flush ' // TRIM(CTM_SENS_1)
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END IF
      CALL SE_BARRIER
      IF ( .NOT. IO_PE_INCLUSIVE ) THEN
         IF ( .NOT. OPEN3( CTM_SENS_1, FSNONIO, PNAME ) ) THEN
            XMSG = 'Could not open ' // TRIM(CTM_SENS_1)
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END IF

      IF ( .NOT. WRITE3( CTM_SENS_1, ALLVAR3, JDATE, JTIME, WRBUFF ) ) THEN
         XMSG = 'Could not write CTM_SENS_1'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
#else

#ifdef parallel
      IF ( .NOT. PTRWRITE3( CTM_SENS_1, ALLVAR3, JDATE, JTIME, WRBUFF ) ) THEN
         XMSG = 'Could not write SENGRID'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
#else
      IF ( .NOT. WRITE3( CTM_SENS_1, ALLVAR3, JDATE, JTIME, WRBUFF ) ) THEN
         XMSG = 'Could not write SENGRID'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
#endif

#endif

      WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &      'Timestep written to', CTM_SENS_1,
     &      'for date and time', JDATE, JTIME




      RETURN
      END

#endif
