
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
C $Header: /project/yoj/arc/CCTM/src/aero/aero5/opdiam.F,v 1.5 2011/10/21 16:10:15 yoj Exp $
 
C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%
 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPPMDIAG ( JDATE, JTIME, TSTEP )

C Revision history
C   ??? Frank Binkowski
C   8 Sep 01 J.Young: dyn alloc - Use HGRD_DEFN
C   03 Sep 01 David Wong: for new pario
C   13 May 04 P.Bhave: added RH to species list; removed M0 and dry M2
C                      conc's from species list
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   18 Jul 05 P.Bhave: Added mass fractions of each mode < 2.5um to output list
C   06 Apr 06 P.Bhave: Added GAMMA_N2O5 to output list
C   25 May 06 P.Bhave: Changed units of all DG variables from m to um, as 
C                      suggested by Dr. Bill Hutzell
C   11 Apr 08 J.Kelly: Added STDEVCOR and variables to account for dry & wet
C                      DGCOR, M2COR_WET, and M3COR_DRY
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   26 Sep 14 H. Pye: Added GAMMA_IEPOX and replaced blank units with na
C   11 May 16 D. Wong: - Modified the code to retreive PMDIAG information from
C                        construct PMDIAG_SPC_RECORD stored in module cgrid_spcs
C                        to provide flexibility to handle AE6 and AE61
C                      - renamed the procedure name from OPDIAM to OPPMDIAG

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE UTILIO_DEFN
      USE RXNS_DATA, ONLY : MECHNAME
      USE PMDIAG_DATA_MODULE

      IMPLICIT NONE
 
!     INCLUDE SUBST_HGRD_ID   ! horizontal dimensioning parameters
!     INCLUDE SUBST_VGRD_ID   ! vertical dimensioning parameters
      INCLUDE SUBST_FILES_ID  ! file name parameters
!     INCLUDE SUBST_COORD_ID  ! coordinate and domain definitions (req IOPARMS)

C...Arguments:

      INTEGER      JDATE      ! current model date, coded YYYYDDD
      INTEGER      JTIME      ! current model time, coded HHMMSS
      INTEGER      TSTEP      ! output time step

C...Local variables:

      CHARACTER( 16 ), SAVE :: PNAME = 'OPPMDIAG'
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER      L          ! loop induction variables

C-----------------------------------------------------------------------

C Try to open existing file for update

      IF ( .NOT. OPEN3( CTM_PMDIAG_1, FSRDWR3, PNAME ) ) THEN

         XMSG = 'Could not open ' // CTM_PMDIAG_1 // ' file for update - '
     &        // 'try to open new'
         CALL M3MESG( XMSG )

C Set output file characteristics based on COORD.EXT and open the aerosol
C diagnostic file

         FTYPE3D = GRDDED3
         SDATE3D = JDATE
         STIME3D = JTIME
         TSTEP3D = TSTEP
         CALL NEXTIME ( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour

         NVARS3D = NUM_PMDIAG_SPC 
         NCOLS3D = GL_NCOLS
         NROWS3D = GL_NROWS
         NLAYS3D = NLAYS
         NTHIK3D =     1
         GDTYP3D = GDTYP_GD
         P_ALP3D = P_ALP_GD
         P_BET3D = P_BET_GD 
         P_GAM3D = P_GAM_GD
         XORIG3D = XORIG_GD
         YORIG3D = YORIG_GD
         XCENT3D = XCENT_GD
         YCENT3D = YCENT_GD
         XCELL3D = XCELL_GD
         YCELL3D = YCELL_GD
         VGTYP3D = VGTYP_GD
         VGTOP3D = VGTOP_GD
!        VGTPUN3D = VGTPUN_GD ! currently, not defined

         DO L = 1, NLAYS3D + 1
            VGLVS3D( L ) = VGLVS_GD( L )
            END DO

!        GDNAM3D = GDNAME_GD
         GDNAM3D = GRID_NAME  ! from HGRD_DEFN

         DO L = 1, NUM_PMDIAG_SPC
            VTYPE3D(L) = PMDIAG_SPC_RECORD(L)%VAR_TYPE
            VNAME3D(L) = PMDIAG_SPC_RECORD(L)%SPC_NAME
            UNITS3D(L) = PMDIAG_SPC_RECORD(L)%SPC_UNIT
            VDESC3D(L) = PMDIAG_SPC_RECORD(L)%SPC_DESC
         END DO

         FDESC3D( 1 ) = 'aerosol distribution and chemistry parameters'
         DO L = 2, MXDESC3
            FDESC3D( L ) = ' '
            END DO

C Open the aerosol diagnostic file

         IF ( .NOT. OPEN3( CTM_PMDIAG_1, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not create '// CTM_PMDIAG_1 // ' file'
            CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
            END IF

         END IF

      END SUBROUTINE OPPMDIAG
