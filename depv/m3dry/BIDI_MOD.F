
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
C $Header: /project/yoj/arc/CCTM/src/depv/m3dry/BIDI_MOD.F,v 1.4 2012/01/19 14:21:03 yoj Exp $

c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module Bidi_Mod

C Contains the shared variables and subrountes needed for the bidirectional 
C NH3 flux model in CMAQ
C
C OPASX_MEDIA - Opens the output file for NH4+ and H+ in the soil water solution
C               
C Revision History: J. Bash Mar 15 11: Created
C                   J. Bash May 11 11: Updated for CMAQ 5.0
C                   J. Bash Nov  7 14: Modified for the restructuring of vidff. Moved constants and data to 
C                       ASX_DATA_MOD.
C------------------------------------------------------------------------------------------

      Implicit None
! Private variables
      Integer, Parameter, Private :: NHx_AQ_CONC  = 4
      Integer, Parameter, Private :: NHx_GAS_CONC = 0      
      Integer, Parameter, Private :: NHx_SOL_CONC = 0     
      Integer, Parameter, Private :: NHx_TOT = NHx_AQ_CONC + NHx_GAS_CONC + NHx_SOL_CONC                       
      Integer, Save,      Private :: LOGDEV      
      Character( 96 ),    Private :: XMSG = ' '
! shared variables
      Character( 16 ), Save , Allocatable :: MEDIA_NAMES( : )
      Character( 16 ), Save , Allocatable :: MEDIA_UNITS( : )
      Character( 80 ), Save , Allocatable :: MEDIA_DESCR( : )
      Integer, Save           :: N_TOT
      Real, Save, Allocatable :: gamma1( :,: ) ! soil NH4/H
      Real, Save, Allocatable :: gamma2( :,: ) ! soil NH4/H
      Real, Save, Allocatable :: MHp1  ( :,: ) ! molar H+
      Real, Save, Allocatable :: MHp2  ( :,: ) ! molar H+  
      Real, Save, Allocatable :: CMEDIA( :,:,: ) ! surface layer concs 

      Contains
         SUBROUTINE INIT_BIDI( abflux )
         USE GRID_CONF
         USE CGRID_SPCS          ! CGRID mechanism species
         USE UTILIO_DEFN

         IMPLICIT NONE

         LOGICAL, INTENT( IN ) :: abflux 
         CHARACTER( 16 ) :: PNAME = 'INIT_BIDI       '
         CHARACTER( 80 ) :: VARDESC
         CHARACTER( 96 ) :: MSG = ' '         

         N_TOT = NHx_TOT
!        Write( Logdev,* ), 'N_TOT', N_TOT
!        Write( Logdev,* ), 'NHx_TOT', NHx_TOT
         ALLOCATE( MEDIA_NAMES( N_TOT ), MEDIA_UNITS( N_TOT ), MEDIA_DESCR( N_TOT ) )
 
         MEDIA_NAMES( 1 ) = 'Gamma1          '
         MEDIA_UNITS( 1 ) = '                '
         MEDIA_DESCR( 1 ) = 'NH4+/H+ in Soil layer 1'
         MEDIA_NAMES( 2 ) = 'Gamma2          '
         MEDIA_UNITS( 2 ) = '                '
         MEDIA_DESCR( 2 ) = 'NH4+/H+ in Soil layer 2'
         MEDIA_NAMES( 3 ) = 'MHpsl1          '
         MEDIA_UNITS( 3 ) = 'mol/l           '
         MEDIA_DESCR( 3 ) = 'Molar H+ in Soil layer 1'
         MEDIA_NAMES( 4 ) = 'MHpsl2          '
         MEDIA_UNITS( 4 ) = 'mol/l           '
         MEDIA_DESCR( 4 ) = 'Molar H+ in Soil layer 2'

         ! allocate the media array variable
         IF ( .NOT. ALLOCATED ( CMEDIA ) ) THEN
            ALLOCATE ( CMEDIA( NCOLS,NROWS,N_TOT ) )
            CMEDIA = 0.0   ! array assignment
         END IF

         RETURN

         END SUBROUTINE INIT_BIDI
!-----------------------------------------------------------------------------
!-------------- Iput / output section of the module --------------------------
!-----------------------------------------------------------------------------

         SUBROUTINE OPASX_MEDIA( JDATE, JTIME, TSTEP )

         USE GRID_CONF
         USE CGRID_SPCS          ! CGRID mechanism species
         USE UTILIO_DEFN

         IMPLICIT NONE

         INCLUDE SUBST_FILES_ID  ! file name parameters

         INTEGER, INTENT( IN ) :: JDATE
         INTEGER, INTENT( IN ) :: JTIME
         INTEGER, INTENT( IN ) :: TSTEP

         CHARACTER( 16 ) :: PNAME = 'OPASX_MEDIA'
         CHARACTER( 80 ) :: VARDESC
         CHARACTER( 96 ) :: MSG = ' '

         INTEGER  N, V, L

C--------------------------------------------------------------------------
         LOGDEV = INIT3()

!        Write( logdev,* ) 'In ', PNAME
         IF ( .NOT. OPEN3( MEDIA_CONC, FSRDWR3, PNAME ) ) THEN

!           Write( logdev,* ) 'After Open3'
!           Write( logdev,* ) 'N_TOT', N_TOT
!           Write( logdev,* ) 'JDATE', JDATE
!           Write( logdev,* ) 'JTIME', JTIME
            XMSG = 'Could not open ' // MEDIA_CONC // ' file for update - '
     &          // 'try to open new'
            CALL M3MESG( XMSG )

            FTYPE3D = GRDDED3
            SDATE3D = JDATE
            STIME3D = JTIME
            TSTEP3D = TSTEP
            CALL NEXTIME( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour

            NVARS3D = N_TOT
            NCOLS3D = GL_NCOLS
            NROWS3D = GL_NROWS
            NLAYS3D = 1
            NTHIK3D = 1
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
!           Write( logdev,* ) 'NLAYS3D', NLAYS3D
            DO L = 1, NLAYS3D + 1
               VGLVS3D( L ) = VGLVS_GD( L )
            END DO
            GDNAM3D = GRID_NAME  ! from HGRD_DEFN

            FDESC3D = ' '   ! array assignment

            FDESC3D( 1 ) = 'Multimedia concentration estimates from integrated ambient '
     &                  // 'concentraitons and surface exchange algorithms'

            N = 0

            DO V = 1, NHx_AQ_CONC
               N = N + 1
!              Write( logdev,* ) 'MEDIA NAME: ', MEDIA_NAMES( N )
               VTYPE3D( N ) = M3REAL
               VNAME3D( N ) = MEDIA_NAMES( N )
               UNITS3D( N ) = MEDIA_UNITS( N )
               VDESC3D( N ) = MEDIA_DESCR( N )
            END DO

            N = NHx_AQ_CONC

            DO V = 1, NHx_GAS_CONC
               N = N + 1
!              Write( logdev,* ) 'MEDIA NAME: ', MEDIA_NAMES( N )
               VTYPE3D( N ) = M3REAL
               VNAME3D( N ) = MEDIA_NAMES( N )
               UNITS3D( N ) = 'ppmV g'
               VDESC3D( N ) = 'gas phase concentration'
            END DO

            N = NHx_AQ_CONC + NHx_GAS_CONC

            DO V = 1, NHx_SOL_CONC
               N = N + 1
!              Write( logdev,* ) 'MEDIA NAME: ', MEDIA_NAMES( N )
               VTYPE3D( N ) = M3REAL
               VNAME3D( N ) = MEDIA_NAMES( N )
               UNITS3D( N ) = 'umol/g'
               VDESC3D( N ) = 'Solid phase concentration'
            END DO
         END IF

! Open file, then close it for subsequent open by all processors

!        Write( logdev,* ) 'Before Open3 MEDIA_CONC'
         IF ( .NOT. OPEN3( MEDIA_CONC, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not create '// TRIM( MEDIA_CONC) // ' file'
            CALL M3EXIT( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
         END IF

         RETURN

         END SUBROUTINE OPASX_MEDIA

!----------------------------------------------------------------------------

         SUBROUTINE WRASX_MEDIA( MDATE, MTIME )

         USE GRID_CONF           ! horizontal grid specifications
         USE UTILIO_DEFN

         IMPLICIT NONE

         INCLUDE SUBST_FILES_ID  ! file name parameters

         INTEGER, INTENT( IN ) :: MDATE
         INTEGER, INTENT( IN ) :: MTIME

         REAL    :: WRMC( NCols,NRows )         ! media write buffer
         INTEGER V, R, C

         CHARACTER( 16 ) :: PNAME = 'WRASX_MEDIA'

!-----------------------------------------------------------------------------
         LOGDEV = INIT3()

         Do R = 1, My_NRows
            Do C = 1, My_NCols
               CMedia( C,R,1 ) = Gamma1( C,R )
               CMedia( C,R,2 ) = Gamma2( C,R )
               CMedia( C,R,3 ) = MHp1( C,R )
               CMedia( C,R,4 ) = MHp2( C,R )
            End Do
         End Do

         DO V = 1, N_TOT  ! species/media loop
            DO R = 1, My_NRows
               DO C = 1, My_NCols
                  WRMC( C,R ) = CMEDIA( C,R,V )
               END DO
            END DO

            IF ( .NOT. WRITE3( MEDIA_CONC, MEDIA_NAMES( V ), MDATE, MTIME,
     &                         WRMC ) ) THEN
               XMSG = 'Could not write ' // MEDIA_CONC // ' file'
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF

         END DO

         WRITE( LOGDEV, '( /5X, A, I8, ":", I6.6 / )' )
     &         'Timestep written to "' // TRIM( MEDIA_CONC ) //
     &         '" for date and time', MDATE, MTIME

         RETURN

         END SUBROUTINE WRASX_MEDIA
      End Module Bidi_Mod
