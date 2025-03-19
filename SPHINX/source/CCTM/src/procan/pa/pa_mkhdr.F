
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

C=======================================================================
      SUBROUTINE PA_MKHDR ( TYPE, NFL, OUTFNAME, SDATE, STIME, TSTEP )

C   21 Jun 10 J.Young: convert for Namelist redesign
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   11 May 11 D.Wong: incorporated twoway model implementation
C   16 Sep 16 J.Young: update for inline procan

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
      USE PA_DEFN               ! Process Anaylsis control and data variables
      USE PAGRD_DEFN            ! PA horiz domain specs

      IMPLICIT NONE

C used in pa_init
C FIRSTIME gets some FDESC3 common variables from GRID_CONF to be used
C for both IPR and IRR if needed.

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

C-----------------------------------------------------------------------

#ifndef mpas
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
            VDESC3D( NOUT ) = IPRDESC( VARINDX )

            ! Find the IPR species to set correct units descriptor
            IPRSPC = MAP_IPRVARtoSPC( VARINDX )
            ICG = MAP_IPRtoCGRID( IPRSPC, 1 )

            IF ( CGRID_MASK_NUM( ICG ) ) THEN
               UNITS3D( NOUT ) = 'm-3'
            ELSE IF ( CGRID_MASK_SRF( ICG ) ) THEN
               UNITS3D( NOUT ) = 'm2 m-3'
            ELSE IF ( CGRID_MASK_AERO( ICG ) ) THEN
               UNITS3D( NOUT ) = 'ug m-3'
            ELSE 
               UNITS3D( NOUT ) = 'ppmV'
            END IF

         END DO

      ELSE IF ( TYPE .EQ. 'IRR' ) THEN

         FDESC3D( 1 ) = 'Integrated Reaction Rate Output File ' // OUTFNAME
         NVARS3D = MIN( MXVARS3 , ( NIRRVAR - ( ( NFL - 1 ) * MXVARS3 ) ) )

         DO NOUT = 1, NVARS3D
            VARINDX = ( NFL - 1 ) * MXVARS3 + NOUT
            VNAME3D( NOUT ) = IRRNAME( VARINDX )
            VTYPE3D( NOUT ) = M3REAL
            UNITS3D( NOUT ) = 'ppmV'
            VDESC3D( NOUT ) = IRRDESC( VARINDX )
         END DO

      ELSE

         CALL M3EXIT( PNAME, SDATE, STIME,
     &        'Incorrect process analysis type specified', 2 )

      END IF
#endif

      RETURN
      END

