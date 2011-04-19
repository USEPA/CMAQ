
************************************************************************
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
C $Header: /project/yoj/arc/CCTM/src/couple/gencoor_wrf/Attic/ppmv_msmr.F,v 1.3 2011/04/19 18:29:44 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE PPMV_MSMR ( NSPCS, NLVLS, SGRID, MOLWT )

C-----------------------------------------------------------------------
C Function:
C    Species data are converted from various units into other units
C    based on the called ENTRY point.
C      Entry:            converts from:                  to:

C    PPMV_MSMR                ppmv                 mass mixing ratio
C    MSMR_PPMV          mass mixing ratio               ppmv

C    PPMV_MLMR                ppmv                molar mixing ratio
C    MLMR_PPMV         molar mixing ratio               ppmv

C    PPMV_MGPM3               ppmV              micro-grams per cubic meter
C    MGPM3_PPMV   micro-grams per cubic meter           ppmV

C    MLMR_MGPM3        molar mixing ratio       micro-grams per cubic meter
C    MGPM3_MLMR   micro-grams per cubic meter     molar mixing ratio

C    MGPM3_MSMR   micro-grams per cubic meter      mass mixing ratio
C    MSMR_MGPM3         mass mixing ratio       micro-grams per cubic meter

C    MGPM3_KGPM3  micro-grams per cubic meter    kilo-grams per cubic meter
C    KGPM3_MGPM3   kilo-grams per cubic meter   micro-grams per cubic meter

C    MLMR_MSMR         molar mixing ratio          mass mixing ratio
C    MSMR_MLMR          mass mixing ratio         molar mixing ratio

C    PPMV_MMPM3               ppmV              micro-moles per cubic meter
C    MMPM3_PPMV   micro-moles per cubic meter           ppmV

C    MMPM3_MGPM3  micro-moles per cubic meter   micro-grams per cubic meter
C    MGPM3_MMPM3  micro-grams per cubic meter   micro-moles per cubic meter

C    NPM3_MGPM3        number per cubic meter   micro-grams per cubic meter
C    MGPM3_NPM3   micro-grams per cubic meter        number per cubic meter

C    MLMR_NPM3         molar mixing ratio            number per cubic meter
C    NPM3_MLMR         number per cubic meter     molar mixing ratio
 
C    MLMR_NPKG         molar mixing ratio            number per kilo-gram
C    NPKG_MLMR         number per kilo-gram       molar mixing ratio

C    M2PM3_M2PM   meters squared per cubic meter  meters squared per mole
C    M2PM_M2PM3   meters squared per mole         meters squared per cubic meter
 
C    Where KGPM3 = Kg(i)/cubic meter of air,
C          MGPM3 = micro-grams(i)/cubic meter of air,
C          MLMR  = molar mixing ratio, mol(i)/mol(air),
C          MMPM3 = micro-moles(i)/cubic meter of air,
C          MSMR  = mass mixing ratio, kg(i)/kg(air) [or g(i)/g(air)],
C          NPKG  = molecules(i)/kg(air),
C          NPM3  = molecules(i)/cubic meter of air, and
C          PPMV  = parts(i)/million parts(air) in volume [= MLMR*10^6]
C          M2PM3 = surface area/cubic meter of air,
C          M2PM  = surface area/mol(air)

C Preconditions:
 
C Subroutines and functions called:
C   INTERPX, M3EXIT
  
C Revision  History:
C   96, 97    Jeff
C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN; replace INTERP3 with INTERPX
C   20 Aug 03 J.Young: add aero surf area conversions (M2PM3 <-> M2PM)
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                      removed deprecated TRIMLEN
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE UTILIO_DEFN

      IMPLICIT NONE

C Includes:

!     INCLUDE SUBST_HGRD_ID     ! horizontal dimensioning parameters
!     INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters
      INCLUDE SUBST_CONST       ! constants
      INCLUDE SUBST_FILES_ID    ! file name parameters

C Arguments:

      INTEGER, INTENT( IN ) :: NSPCS          ! number of species being converted
      INTEGER, INTENT( IN ) :: NLVLS          ! number of layers being converted
!     REAL       SGRID( NCOLS,NROWS,NLAYS,* ) ! CGRID subset
      REAL,    INTENT( INOUT ) :: SGRID( :,:,:,: ) ! CGRID subset
      REAL,    INTENT( IN ) :: MOLWT( NSPCS ) ! species molecular weights
      INTEGER, INTENT( IN ) :: CDATE          ! current date, coded YYYYDDD
      INTEGER, INTENT( IN ) :: CTIME          ! current time, coded HHMMSS

C Parameters:
 
      REAL, PARAMETER :: GPKG = 1.0E+03              ! g/kg
      REAL, PARAMETER :: MAOAVO = MWAIR / AVO / GPKG ! MWAIR in g/mol
      REAL, PARAMETER :: MGPG = 1.0E+06              ! micro-g/g
      REAL, PARAMETER :: MUAVO = AVO / MGPG          ! micro-Avogadro

C External Functions:

C Local Variables:

      REAL       DENS( NCOLS,NROWS,NLAYS ) ! air density (kg/m^3)

      INTEGER    NCOLSDENS, NROWSDENS    ! local for DENS

      INTEGER    COL, ROW, LVL, SPC      ! subscript indices
      REAL       FAC, CONV               ! conversion variables

      INTEGER    GXOFF, GYOFF            ! global origin offset from file
C for INTERPX
      INTEGER :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

      CHARACTER( 96 ) :: XMSG = ' '

C............................................................................
C entry PPMV_MSMR
 
C SGRID is in ppmV. Convert to Kg/Kg
C MOLWT in g/mol
       
      FAC = 1.0E-06 / MWAIR
      DO SPC = 1, NSPCS
         CONV = FAC * MOLWT( SPC )
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV
                  END DO
               END DO
            END DO
         END DO
 
      RETURN

C............................................................................
C entry MSMR_PPMV
 
      ENTRY MSMR_PPMV ( NSPCS, NLVLS, SGRID, MOLWT )

C SGRID is in Kg/Kg. Convert to ppmV
C MOLWT in g/mol
       
      FAC = 1.0E+06 * MWAIR
      DO SPC = 1, NSPCS
         CONV = FAC / MOLWT( SPC )
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV
                  END DO
               END DO
            END DO
         END DO
 
      RETURN

C............................................................................
C entry PPMV_MLMR
 
      ENTRY PPMV_MLMR ( NSPCS, NLVLS, SGRID )

C SGRID is in ppmV. Convert to mol/mol
       
      CONV = 1.0E-06
      DO SPC = 1, NSPCS
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV
                  END DO
               END DO
            END DO
         END DO
 
      RETURN

C............................................................................
C entry MLMR_PPMV
 
      ENTRY MLMR_PPMV ( NSPCS, NLVLS, SGRID )

C SGRID is in mol/mol. Convert to ppmV
       
      CONV = 1.0E+06
      DO SPC = 1, NSPCS
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV
                  END DO
               END DO
            END DO
         END DO
 
      RETURN

C............................................................................
C entry PPMV_MGPM3

      ENTRY PPMV_MGPM3 ( NSPCS, NLVLS, SGRID, MOLWT, CDATE, CTIME )

C Interpolate DENS to the argument date&time
 
      CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

      NCOLSDENS = ENDCOLMC3 - STRTCOLMC3 + 1
      NROWSDENS = ENDROWMC3 - STRTROWMC3 + 1
      IF ( NCOLSDENS .NE. MY_NCOLS .OR.
     &     NROWSDENS .NE. MY_NROWS ) THEN
         WRITE( XMSG,'( A, 4I8 )' ) 'Local Columns or Rows incorrect',
     &   NCOLSDENS, MY_NCOLS, NROWSDENS, MY_NROWS
         CALL M3EXIT ( 'PPMV_MGPM3', CDATE, CTIME, XMSG, XSTAT1 )
         END IF

      IF ( .NOT. INTERPX( MET_CRO_3D, 'DENS', 'PPMV_MGPM3',
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    CDATE, CTIME, DENS ) ) THEN
         XMSG = 'Could not interpolate DENS from ' // MET_CRO_3D
         CALL M3EXIT ( 'PPMV_MGPM3', CDATE, CTIME, XMSG, XSTAT1 )
         END IF

C SGRID is in ppmV. Convert to micro-g/m^3 (g/micro-g = 10**-6, cancels out ppm
C                                           = 10**6)
C MOLWT in g/mol
       
      FAC = GPKG / MWAIR
      DO SPC = 1, NSPCS
         CONV = FAC * MOLWT( SPC )
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV * DENS( COL,ROW,LVL )
                  END DO
               END DO
            END DO
         END DO

      RETURN

C............................................................................
C entry MGPM3_PPMV
 
      ENTRY MGPM3_PPMV ( NSPCS, NLVLS, SGRID, MOLWT, CDATE, CTIME )
 
C Interpolate DENS to the argument date&time
 
      CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

      NCOLSDENS = ENDCOLMC3 - STRTCOLMC3 + 1
      NROWSDENS = ENDROWMC3 - STRTROWMC3 + 1
      IF ( NCOLSDENS .NE. MY_NCOLS .OR.
     &     NROWSDENS .NE. MY_NROWS ) THEN
         WRITE( XMSG,'( A, 4I8 )' ) 'Local Columns or Rows incorrect',
     &   NCOLSDENS, MY_NCOLS, NROWSDENS, MY_NROWS
         CALL M3EXIT ( 'MGPM3_PPMV', CDATE, CTIME, XMSG, XSTAT1 )
         END IF

      IF ( .NOT. INTERPX( MET_CRO_3D, 'DENS', 'MGPM3_PPMV',
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    CDATE, CTIME, DENS ) ) THEN
         XMSG = 'Could not interpolate DENS from ' // MET_CRO_3D
         CALL M3EXIT ( 'MGPM3_PPMV', CDATE, CTIME, XMSG, XSTAT1 )
         END IF

C SGRID is in micro-g/m^3. Convert to ppmV (g/micro-g = 10**-6, cancels out ppm
C                                           = 10**6)
C MOLWT in g/mol
 
      FAC = MWAIR / GPKG
      DO SPC = 1, NSPCS
         CONV = FAC / MOLWT( SPC )
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV / DENS( COL,ROW,LVL )
                  END DO
               END DO
            END DO
         END DO

      RETURN

C............................................................................
C entry MLMR_MGPM3
 
      ENTRY MLMR_MGPM3 ( NSPCS, NLVLS, SGRID, MOLWT, CDATE, CTIME )
 
C Interpolate DENS to the argument date&time
 
      CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

      NCOLSDENS = ENDCOLMC3 - STRTCOLMC3 + 1
      NROWSDENS = ENDROWMC3 - STRTROWMC3 + 1
      IF ( NCOLSDENS .NE. MY_NCOLS .OR.
     &     NROWSDENS .NE. MY_NROWS ) THEN
         WRITE( XMSG,'( A, 4I8 )' ) 'Local Columns or Rows incorrect',
     &   NCOLSDENS, MY_NCOLS, NROWSDENS, MY_NROWS
         CALL M3EXIT ( 'MLMR_MGPM3', CDATE, CTIME, XMSG, XSTAT1 )
         END IF

      IF ( .NOT. INTERPX( MET_CRO_3D, 'DENS', 'MLMR_MGPM3',
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    CDATE, CTIME, DENS ) ) THEN
         XMSG = 'Could not interpolate DENS from ' // MET_CRO_3D
         CALL M3EXIT ( 'MLMR_MGPM3', CDATE, CTIME, XMSG, XSTAT1 )
         END IF

C SGRID is in mol/mol. Convert to micro-g/m^3
C MOLWT in g/mol
 
      FAC = GPKG * MGPG / MWAIR
      DO SPC = 1, NSPCS
         CONV = FAC * MOLWT( SPC )
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV * DENS( COL,ROW,LVL )
                  END DO
               END DO
            END DO
         END DO

      RETURN

C............................................................................
C entry MGPM3_MLMR
 
      ENTRY MGPM3_MLMR ( NSPCS, NLVLS, SGRID, MOLWT, CDATE, CTIME )
 
C Interpolate DENS to the argument date&time
 
      CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

      NCOLSDENS = ENDCOLMC3 - STRTCOLMC3 + 1
      NROWSDENS = ENDROWMC3 - STRTROWMC3 + 1
      IF ( NCOLSDENS .NE. MY_NCOLS .OR.
     &     NROWSDENS .NE. MY_NROWS ) THEN
         WRITE( XMSG,'( A, 4I8 )' ) 'Local Columns or Rows incorrect',
     &   NCOLSDENS, MY_NCOLS, NROWSDENS, MY_NROWS
         CALL M3EXIT ( 'MGPM3_MLMR', CDATE, CTIME, XMSG, XSTAT1 )
         END IF

      IF ( .NOT. INTERPX( MET_CRO_3D, 'DENS', 'MGPM3_MLMR',
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    CDATE, CTIME, DENS ) ) THEN
         XMSG = 'Could not interpolate DENS from ' // MET_CRO_3D
         CALL M3EXIT ( 'MGPM3_MLMR', CDATE, CTIME, XMSG, XSTAT1 )
         END IF
 
C SGRID is in micro-g/m^3. Convert to mol/mol
C MOLWT in g/mol
 
      FAC = MWAIR / ( GPKG * MGPG )
      DO SPC = 1, NSPCS
         CONV = FAC / MOLWT( SPC )
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV / DENS( COL,ROW,LVL )
                  END DO
               END DO
            END DO
         END DO

      RETURN

C............................................................................
C entry MGPM3_MSMR
 
      ENTRY MGPM3_MSMR ( NSPCS, NLVLS, SGRID, CDATE, CTIME )
 
C Interpolate DENS to the argument date&time
 
      CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

      NCOLSDENS = ENDCOLMC3 - STRTCOLMC3 + 1
      NROWSDENS = ENDROWMC3 - STRTROWMC3 + 1
      IF ( NCOLSDENS .NE. MY_NCOLS .OR.
     &     NROWSDENS .NE. MY_NROWS ) THEN
         WRITE( XMSG,'( A, 4I8 )' ) 'Local Columns or Rows incorrect',
     &   NCOLSDENS, MY_NCOLS, NROWSDENS, MY_NROWS
         CALL M3EXIT ( 'MGPM3_MSMR', CDATE, CTIME, XMSG, XSTAT1 )
         END IF

      IF ( .NOT. INTERPX( MET_CRO_3D, 'DENS', 'MGPM3_MSMR',
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    CDATE, CTIME, DENS ) ) THEN
         XMSG = 'Could not interpolate DENS from ' // MET_CRO_3D
         CALL M3EXIT ( 'MGPM3_MSMR', CDATE, CTIME, XMSG, XSTAT1 )
         END IF
 
C SGRID is in micro-g/m^3. Convert to Kg/Kg
 
      CONV = 1.0 / ( GPKG * MGPG )
      DO SPC = 1, NSPCS
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV / DENS( COL,ROW,LVL )
                  END DO
               END DO
            END DO
         END DO

      RETURN

C............................................................................
C entry MSMR_MGPM3
 
      ENTRY MSMR_MGPM3 ( NSPCS, NLVLS, SGRID, CDATE, CTIME )
 
C Interpolate DENS to the argument date&time
 
      CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

      NCOLSDENS = ENDCOLMC3 - STRTCOLMC3 + 1
      NROWSDENS = ENDROWMC3 - STRTROWMC3 + 1
      IF ( NCOLSDENS .NE. MY_NCOLS .OR.
     &     NROWSDENS .NE. MY_NROWS ) THEN
         WRITE( XMSG,'( A, 4I8 )' ) 'Local Columns or Rows incorrect',
     &   NCOLSDENS, MY_NCOLS, NROWSDENS, MY_NROWS
         CALL M3EXIT ( 'MSMR_MGPM3', CDATE, CTIME, XMSG, XSTAT1 )
         END IF

      IF ( .NOT. INTERPX( MET_CRO_3D, 'DENS', 'MSMR_MGPM3',
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    CDATE, CTIME, DENS ) ) THEN
         XMSG = 'Could not interpolate DENS from ' // MET_CRO_3D
         CALL M3EXIT ( 'MSMR_MGPM3', CDATE, CTIME, XMSG, XSTAT1 )
         END IF
 
C SGRID is in Kg/Kg. Convert to  micro-g/m^3
 
      CONV = GPKG * MGPG
      DO SPC = 1, NSPCS
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV * DENS( COL,ROW,LVL )
                  END DO
               END DO
            END DO
         END DO

      RETURN

C............................................................................
C entry MGPM3_KGPM3
 
      ENTRY MGPM3_KGPM3 ( NSPCS, NLVLS, SGRID )
 
C SGRID is in micro-g/m^3. Convert to Kg/m^3
 
      CONV = 1.0 / ( GPKG * MGPG )
      DO SPC = 1, NSPCS
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV
                  END DO
               END DO
            END DO
         END DO

      RETURN

C............................................................................
C entry KGPM3_MGPM3
 
      ENTRY KGPM3_MGPM3 ( NSPCS, NLVLS, SGRID )
 
C SGRID is in Kg/m^3. Convert to  micro-g/m^3
 
      CONV = GPKG * MGPG
      DO SPC = 1, NSPCS
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV
                  END DO
               END DO
            END DO
         END DO

      RETURN

C............................................................................
C entry MLMR_MSMR 
 
      ENTRY MLMR_MSMR ( NSPCS, NLVLS, SGRID, MOLWT )

C SGRID is in mol/mol. Convert to Kg/Kg
C MOLWT in g/mol
 
      FAC = 1.0 / MWAIR
      DO SPC = 1, NSPCS
         CONV = FAC * MOLWT( SPC )
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV
                  END DO
               END DO
            END DO
         END DO

      RETURN

C............................................................................
C entry MSMR_MLMR 
 
      ENTRY MSMR_MLMR ( NSPCS, NLVLS, SGRID, MOLWT )

C SGRID is in Kg/Kg. Convert to mol/mol
C MOLWT in g/mol
 
      FAC = MWAIR
      DO SPC = 1, NSPCS
         CONV = FAC / MOLWT( SPC )
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV
                  END DO
               END DO
            END DO
         END DO

      RETURN

C............................................................................
C entry PPMV_MMPM3
 
      ENTRY PPMV_MMPM3 ( NSPCS, NLVLS, SGRID, CDATE, CTIME )

C Initialization for density
 
      CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

      NCOLSDENS = ENDCOLMC3 - STRTCOLMC3 + 1
      NROWSDENS = ENDROWMC3 - STRTROWMC3 + 1
      IF ( NCOLSDENS .NE. MY_NCOLS .OR.
     &     NROWSDENS .NE. MY_NROWS ) THEN
         WRITE( XMSG,'( A, 4I8 )' ) 'Local Columns or Rows incorrect',
     &   NCOLSDENS, MY_NCOLS, NROWSDENS, MY_NROWS
         CALL M3EXIT ( 'PPMV_MMPM3', CDATE, CTIME, XMSG, XSTAT1 )
         END IF

      IF ( .NOT. INTERPX( MET_CRO_3D, 'DENS', 'PPMV_MMPM3',
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    CDATE, CTIME, DENS ) ) THEN
         XMSG = 'Could not interpolate DENS from ' // MET_CRO_3D
         CALL M3EXIT ( 'PPMV_MMPM3', CDATE, CTIME, XMSG, XSTAT1 )
         END IF
 
C SGRID is in ppmV. Convert to micro-mol/m^3
       
      CONV = GPKG / MWAIR
      DO SPC = 1, NSPCS
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV * DENS( COL,ROW,LVL )
                  END DO
               END DO
            END DO
         END DO
 
      RETURN

C............................................................................
C entry MMPM3_PPMV
 
      ENTRY MMPM3_PPMV ( NSPCS, NLVLS, SGRID, CDATE, CTIME )
 
C Interpolate DENS to the argument date&time
 
      CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

      NCOLSDENS = ENDCOLMC3 - STRTCOLMC3 + 1
      NROWSDENS = ENDROWMC3 - STRTROWMC3 + 1
      IF ( NCOLSDENS .NE. MY_NCOLS .OR.
     &     NROWSDENS .NE. MY_NROWS ) THEN
         WRITE( XMSG,'( A, 4I8 )' ) 'Local Columns or Rows incorrect',
     &   NCOLSDENS, MY_NCOLS, NROWSDENS, MY_NROWS
         CALL M3EXIT ( 'MMPM3_PPMV', CDATE, CTIME, XMSG, XSTAT1 )
         END IF

      IF ( .NOT. INTERPX( MET_CRO_3D, 'DENS', 'MMPM3_PPMV',
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    CDATE, CTIME, DENS ) ) THEN
         XMSG = 'Could not interpolate DENS from ' // MET_CRO_3D
         CALL M3EXIT ( 'MMPM3_PPMV', CDATE, CTIME, XMSG, XSTAT1 )
         END IF
 
C SGRID is in micro-mol/m^3. Convert to ppmV
 
      CONV = MWAIR / GPKG
      DO SPC = 1, NSPCS
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV / DENS( COL,ROW,LVL )
                  END DO
               END DO
            END DO
         END DO

      RETURN

C............................................................................
C entry MMPM3_MGPM3
 
      ENTRY MMPM3_MGPM3 ( NSPCS, NLVLS, SGRID, MOLWT )
 
C SGRID is in micro-mol/m^3. Convert to micro-g/m^3
C MLWTR in g/mol
 
      DO SPC = 1, NSPCS
         CONV = MOLWT( SPC )
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV
                  END DO
               END DO
            END DO
         END DO

      RETURN

C............................................................................
C entry MGPM3_MMPM3
 
      ENTRY MGPM3_MMPM3 ( NSPCS, NLVLS, SGRID, MOLWT )

C SGRID is in micro-g/m^3. Convert to micro-mol/m^3
C MLWTR in g/mol
       
      DO SPC = 1, NSPCS
         CONV = 1.0 / MOLWT( SPC )
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV
                  END DO
               END DO
            END DO
         END DO
 
      RETURN

C............................................................................
C entry NPM3_MGPM3
 
      ENTRY NPM3_MGPM3 ( NSPCS, NLVLS, SGRID, MOLWT )
 
C SGRID is in #/m^3. Convert to micro-g/m^3
C MOLWT in g/mol
 
      DO SPC = 1, NSPCS
         CONV = MOLWT( SPC ) / MUAVO 
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV
                  END DO
               END DO
            END DO
         END DO

      RETURN

C............................................................................
C entry MGPM3_NPM3
 
      ENTRY MGPM3_NPM3 ( NSPCS, NLVLS, SGRID, MOLWT )
 
C SGRID is in micro-g/m^3. Convert to #/m^3
C MOLWT in g/mol
 
      DO SPC = 1, NSPCS
         CONV = MUAVO / MOLWT( SPC )
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV
                  END DO
               END DO
            END DO
         END DO

      RETURN

C............................................................................
C entry MLMR_NPM3
 
      ENTRY MLMR_NPM3 ( NSPCS, NLVLS, SGRID, CDATE, CTIME )
 
C Interpolate DENS to the argument date&time
 
      CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

      NCOLSDENS = ENDCOLMC3 - STRTCOLMC3 + 1
      NROWSDENS = ENDROWMC3 - STRTROWMC3 + 1
      IF ( NCOLSDENS .NE. MY_NCOLS .OR.
     &     NROWSDENS .NE. MY_NROWS ) THEN
         WRITE( XMSG,'( A, 4I8 )' ) 'Local Columns or Rows incorrect',
     &   NCOLSDENS, MY_NCOLS, NROWSDENS, MY_NROWS
         CALL M3EXIT ( 'MLMR_NPM3', CDATE, CTIME, XMSG, XSTAT1 )
         END IF

      IF ( .NOT. INTERPX( MET_CRO_3D, 'DENS', 'MLMR_NPM3',
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    CDATE, CTIME, DENS ) ) THEN
         XMSG = 'Could not interpolate DENS from ' // MET_CRO_3D
         CALL M3EXIT ( 'MLMR_NPM3', CDATE, CTIME, XMSG, XSTAT1 )
         END IF

C SGRID is in mol/mol. Convert to #/m^3
 
      CONV = 1.0 / MAOAVO
      DO SPC = 1, NSPCS
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV * DENS( COL,ROW,LVL )
                  END DO
               END DO
            END DO
         END DO

      RETURN

C............................................................................
C entry NPM3_MLMR
 
      ENTRY NPM3_MLMR ( NSPCS, NLVLS, SGRID, CDATE, CTIME )
 
C Interpolate DENS to the argument date&time
 
      CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

      NCOLSDENS = ENDCOLMC3 - STRTCOLMC3 + 1
      NROWSDENS = ENDROWMC3 - STRTROWMC3 + 1
      IF ( NCOLSDENS .NE. MY_NCOLS .OR.
     &     NROWSDENS .NE. MY_NROWS ) THEN
         WRITE( XMSG,'( A, 4I8 )' ) 'Local Columns or Rows incorrect',
     &   NCOLSDENS, MY_NCOLS, NROWSDENS, MY_NROWS
         CALL M3EXIT ( 'NPM3_MLMR', CDATE, CTIME, XMSG, XSTAT1 )
         END IF

      IF ( .NOT. INTERPX( MET_CRO_3D, 'DENS', 'NPM3_MLMR',
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    CDATE, CTIME, DENS ) ) THEN
         XMSG = 'Could not interpolate DENS from ' // MET_CRO_3D
         CALL M3EXIT ( 'NPM3_MLMR', CDATE, CTIME, XMSG, XSTAT1 )
         END IF

C SGRID is in #/m^3. Convert to mol/mol
 
      CONV = MAOAVO
      DO SPC = 1, NSPCS
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV / DENS( COL,ROW,LVL )
                  END DO
               END DO
            END DO
         END DO

      RETURN

C............................................................................
C entry MLMR_NPKG
 
      ENTRY MLMR_NPKG ( NSPCS, NLVLS, SGRID )
 
C SGRID is in mol/mol. Convert to #/Kg
 
      CONV = 1.0 / MAOAVO
      DO SPC = 1, NSPCS
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV
                  END DO
               END DO
            END DO
         END DO

      RETURN

C............................................................................
C entry NPKG_MLMR
 
      ENTRY NPKG_MLMR ( NSPCS, NLVLS, SGRID )
 
C SGRID is in #/Kg. Convert to mol/mol
 
      CONV = MAOAVO
      DO SPC = 1, NSPCS
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV
                  END DO
               END DO
            END DO
         END DO

      RETURN

C............................................................................
C entry M2PM3_M2PM
 
      ENTRY M2PM3_M2PM ( NSPCS, NLVLS, SGRID, CDATE, CTIME )
 
C Interpolate DENS to the argument date&time
 
      CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

      NCOLSDENS = ENDCOLMC3 - STRTCOLMC3 + 1
      NROWSDENS = ENDROWMC3 - STRTROWMC3 + 1
      IF ( NCOLSDENS .NE. MY_NCOLS .OR.
     &     NROWSDENS .NE. MY_NROWS ) THEN
         WRITE( XMSG,'( A, 4I8 )' ) 'Local Columns or Rows incorrect',
     &   NCOLSDENS, MY_NCOLS, NROWSDENS, MY_NROWS
         CALL M3EXIT ( 'NPM3_MLMR', CDATE, CTIME, XMSG, XSTAT1 )
         END IF

      IF ( .NOT. INTERPX( MET_CRO_3D, 'DENS', 'NPM3_MLMR',
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    CDATE, CTIME, DENS ) ) THEN
         XMSG = 'Could not interpolate DENS from ' // MET_CRO_3D
         CALL M3EXIT ( 'NPM3_MLMR', CDATE, CTIME, XMSG, XSTAT1 )
         END IF

C SGRID is in m^2/m^3. Convert to m^2/mol
 
      CONV = MWAIR / GPKG
      DO SPC = 1, NSPCS
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV / DENS( COL,ROW,LVL )
                  END DO
               END DO
            END DO
         END DO

      RETURN

C............................................................................
C entry M2PM_M2PM3
 
      ENTRY M2PM_M2PM3 ( NSPCS, NLVLS, SGRID, CDATE, CTIME )
 
C Interpolate DENS to the argument date&time
 
      CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

      NCOLSDENS = ENDCOLMC3 - STRTCOLMC3 + 1
      NROWSDENS = ENDROWMC3 - STRTROWMC3 + 1
      IF ( NCOLSDENS .NE. MY_NCOLS .OR.
     &     NROWSDENS .NE. MY_NROWS ) THEN
         WRITE( XMSG,'( A, 4I8 )' ) 'Local Columns or Rows incorrect',
     &   NCOLSDENS, MY_NCOLS, NROWSDENS, MY_NROWS
         CALL M3EXIT ( 'NPM3_MLMR', CDATE, CTIME, XMSG, XSTAT1 )
         END IF

      IF ( .NOT. INTERPX( MET_CRO_3D, 'DENS', 'NPM3_MLMR',
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    CDATE, CTIME, DENS ) ) THEN
         XMSG = 'Could not interpolate DENS from ' // MET_CRO_3D
         CALL M3EXIT ( 'NPM3_MLMR', CDATE, CTIME, XMSG, XSTAT1 )
         END IF

C SGRID is in m^2/mol. Convert to m^2/m^3
 
      CONV = GPKG / MWAIR
      DO SPC = 1, NSPCS
         DO LVL = 1, NLVLS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  SGRID( COL,ROW,LVL,SPC ) = SGRID( COL,ROW,LVL,SPC )
     &                                     * CONV * DENS( COL,ROW,LVL )
                  END DO
               END DO
            END DO
         END DO

      RETURN

      END
