
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
C $Header: /project/yoj/arc/CCTM/src/procan/pa/pa_mkhdr.F,v 1.4 2011/09/23 17:11:27 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C=======================================================================
      SUBROUTINE PA_MKHDR ( TYPE, NFL, OUTFNAME, SDATE, STIME, TSTEP )

C   21 Jun 10 J.Young: convert for Namelist redesign
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   11 May 11 D.Wong: incorporated twoway model implementation

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
      USE PAGRD_DEFN            ! PA horiz domain specs

      IMPLICIT NONE

C used in pa_init
C FIRSTIME gets some FDESC3 common variables from GRID_CONF to be used
C for both IPR and IRR if needed.

      INCLUDE SUBST_PACMN_ID    ! PA common block definitions

C Arguments:
      CHARACTER( 3 ) ::  TYPE   ! IPR or IRR
      INTEGER NFL               ! file sequence no.
      CHARACTER( 16 ) :: OUTFNAME  ! Assigned IPR or IRR output file name
      INTEGER SDATE             ! starting date,    format YYYYDDD
      INTEGER STIME             ! starting time,    format HHMMSS
      INTEGER TSTEP( 3 )        ! time step vector (HHMMSS)
                                ! TSTEP(1) = local output step
                                ! TSTEP(2) = sciproc sync. step (chem)
                                ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                !            step and wrf/cmaq call frequency

C Local Variables:
      INTEGER AE_INDX   ! Index for aerosol species
      INTEGER IPRSPC    ! Index for IPR species
      INTEGER PL        ! Index for PA output level
      INTEGER VARINDX   ! Index for output variable
      INTEGER L, NOUT   ! Loop indicies
      INTEGER NSP, NPR  ! Loop indices for finding IPR species
      INTEGER ICG       ! species pointer

      CHARACTER( 16 ) ::  PNAME = 'PA_MKHDR'  ! Subroutine Name

      LOGICAL, SAVE :: FIRSTIME = .TRUE.

C-----------------------------------------------------------------------

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Get file header description from GRID_CONF and reconfigure output domain
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      P_ALP3D = P_ALP_GD
      P_BET3D = P_BET_GD
      P_GAM3D = P_GAM_GD

      XCENT3D = XCENT_GD
      YCENT3D = YCENT_GD
      XORIG3D = XORIG_GD
      YORIG3D = YORIG_GD
      XCELL3D = XCELL_GD
      YCELL3D = YCELL_GD

      VGTYP3D = VGTYP_GD
      VGTOP3D = VGTOP_GD

      FTYPE3D = GRDDED3
      GDNAM3D = GDNAME_GD
      UPNAM3D = PNAME
!     EXECN3D = '????????'
!     UPDSC3D =

      SDATE3D = SDATE
      STIME3D = STIME
      CALL NEXTIME( SDATE3D, STIME3D, TSTEP( 1 ) )
      TSTEP3D = TSTEP( 1 )

      NCOLS3D = PACOLS
      NROWS3D = PAROWS
      NLAYS3D = PALEVS

      XORIG3D = XORIG3D + DBLE( PA_BEGCOL - 1 ) * XCELL3D
      YORIG3D = YORIG3D + DBLE( PA_BEGROW - 1 ) * YCELL3D

      DO L = PA_BEGLEV, PA_ENDLEV + 1
         PL = L - PA_BEGLEV + 1
         VGLVS3D ( PL ) = VGLVS_GD( L )
      END DO

      IF ( TYPE .EQ. 'IPR' ) THEN

         FDESC3D( 1 ) = 'Integrated Process Rates Output File ' // OUTFNAME
         NVARS3D = MIN( MXVARS3 , ( NIPRVAR - ( NFL - 1 ) * MXVARS3 ) )

         DO NOUT = 1, NVARS3D
            VARINDX = ( NFL - 1 ) * MXVARS3 + NOUT
            VNAME3D( NOUT ) = IPRNAME( VARINDX )
            VTYPE3D( NOUT ) = M3REAL

C..Find the IPR species to set correct units descriptor
            DO NSP = 1, N_IPR_SPC
               DO NPR = 1, NPRCS
                  IF ( VARINDX .EQ. IPROUT( NSP, NPR ) ) THEN
                     IPRSPC = NSP
                     EXIT
                  END IF
               END DO
            END DO

            ICG = IPR2GRD( IPRSPC, 1 )
            UNITS3D( NOUT ) = 'ppmV'

            IF ( ICG .GE. AE_STRT .AND. ICG .LT. NR_STRT ) THEN
               AE_INDX = ICG - AE_STRT + 1
               IF ( AE_SPC( AE_INDX ) ( 1 : 3 ) .EQ. 'NUM' ) THEN
                  UNITS3D( NOUT ) = 'number/m**3'
               ELSE IF ( AE_SPC( AE_INDX ) ( 1 : 3 ) .EQ. 'SRF' ) THEN
                  UNITS3D( NOUT ) = 'm**2/m**3'
               ELSE
                  UNITS3D( NOUT ) = 'micrograms/m**3'
               END IF
            END IF

               VDESC3D( NOUT ) = IPRDESC( VARINDX )
         END DO

      ELSE IF ( TYPE .EQ. 'IRR' ) THEN

         FDESC3D( 1 ) = 'Integrated Reaction Rate Output File ' // OUTFNAME
         NVARS3D = MIN( MXVARS3 , ( NIRRVAR - ( ( NFL - 1 ) * MXVARS3 ) ) )

         DO NOUT = 1, NVARS3D
            VARINDX = ( NFL - 1 ) * MXVARS3 + NOUT
            VNAME3D( NOUT ) = IRRNAME( VARINDX )
            VTYPE3D( NOUT ) = M3REAL
            UNITS3D( NOUT ) = 'ppm'
            VDESC3D( NOUT ) = IRRDESC( VARINDX )
         END DO

      ELSE

         CALL M3EXIT( PNAME, SDATE, STIME,
     &        'Incorrect process analysis type specified', 2 )

      END IF

      RETURN
      END

