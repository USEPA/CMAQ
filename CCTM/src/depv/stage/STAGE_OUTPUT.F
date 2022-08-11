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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE STAGE_OUTPUT

C-----------------------------------------------------------------------
C Function: Collection of ouput routines for STAGE deposition velocities
C           and dry deposition
C-----------------------------------------------------------------------
      USE RUNTIME_VARS, Only: MOSAIC
    
      Contains 
         Pure Function Write_time( WSTEP, TSTEP ) Result( Write_now ) 

         Implicit None

         Integer, Intent( IN ) :: WSTEP
         Integer, Intent( IN ) :: TSTEP
         Logical               :: Write_now     
        
         If( WSTEP .Ge. TSTEP ) Then
            Write_now = .TRUE.
         Else 
            Write_now = .FALSE.
         End If

         End Function Write_time

         Subroutine Open_depv( DATE, TIME, TSTEP )

         USE GRID_CONF           ! horizontal & vertical domain specifications
         USE CGRID_SPCS          ! CGRID mechanism species
         USE STAGE_DATA
         USE UTILIO_DEFN
         USE MOSAIC_MOD, Only: Tile_Data

         IMPLICIT NONE
 
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:

         INTEGER, INTENT( IN ) :: DATE    ! current model date, coded YYYYDDD
         INTEGER, INTENT( IN ) :: TIME    ! current model time, coded HHMMSS
         INTEGER, INTENT( IN ) :: TSTEP   ! output time step

C Local variables:

         CHARACTER( 16 ) :: PNAME = 'OPSTAGE_DEPV'
         CHARACTER( 16 ) :: VNAME            ! scratch string
         CHARACTER( 96 ) :: XMSG = ' '

         INTEGER      N_Vars
         INTEGER      V, N, L     ! loop induction variables

         SDATE3D = DATE
         STIME3D = TIME
         TSTEP3D = TSTEP
         CALL NEXTIME( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour

         FTYPE3D = GRDDED3
         NCOLS3D = GL_NCOLS
         NROWS3D = GL_NROWS
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
         GDNAM3D = GRID_NAME  ! from HGRD_DEFN

         FDESC3D = ' '   ! array

         L = LEN_TRIM( DEPV_METHOD )
         FDESC3D( 1 ) = 'Instanteous values of land use specific'
     &               // ' resistances and conductances:'
     &               // ' these parameters are' 
         FDESC3D( 2 ) = 'estimated by the '
     &               //  DEPV_METHOD( 1:L ) // ' algorithm.'

         IF ( .NOT. OPEN3( CTM_DEPV_DIAG, FSRDWR3, PNAME ) ) THEN

            XMSG = 'Could not open ' // CTM_DEPV_DIAG // ' file for update - '
     &           // 'try to open new'
            CALL M3MESG( XMSG )

C Set output file characteristics based on GRIDDESC and open the dep velocity dignostic file
            NVARS3D = Tile_Data%n_Vd
            NLAYS3D = 1
            DO L = 1, NLAYS3D + 1
               VGLVS3D( L ) = VGLVS_GD( L )
            END DO

            N = 0
            DO V = 1, N_SPC_DEPV
               If( Tile_Data%Vd_Out( V ) ) Then

                  N = N + 1 
                  VNAME = trim( Tile_Data%Vd_Name( V ) )
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = VNAME
                  UNITS3D( N ) = 'cm s-1'
                  VDESC3D( N ) = 'Dry deposition velocity'

               End If
            END DO

C Open diagnostic file, then close it for subsequent open by all processors

            IF ( .NOT. OPEN3( CTM_DEPV_DIAG, FSNEW3, PNAME ) ) THEN
               XMSG = 'Could not create '// TRIM( CTM_DEPV_DIAG ) // ' file'
               CALL M3EXIT( PNAME, DATE, TIME, XMSG, XSTAT1 )
            END IF

         END IF

         IF ( MOSAIC .AND. .NOT. OPEN3( CTM_DEPV_MOS, FSRDWR3, PNAME ) ) THEN

            XMSG = 'Could not open ' // CTM_DEPV_MOS // ' file for update - '
     &           // 'try to open new'
            CALL M3MESG( XMSG )

C Set output file characteristics based on GRIDDESC and open the dep velocity dignostic file
            NVARS3D = Tile_Data%n_Vd + 7
            NLAYS3D = Tile_Data%N_LUFRAC   ! preempting normal use of vertical layers

            N = 0
            N = N + 1 ! 1
            VTYPE3D( N ) = M3REAL
            VNAME3D( N ) = 'RA'
            UNITS3D( N ) = 's m-1'
            VDESC3D( N ) = 'aerodynamic resistance for land use category'

            N = N + 1 ! 2
            VTYPE3D( N ) = M3REAL
            VNAME3D( N ) = 'LUFRAC'
            UNITS3D( N ) = 'dimensionless'
            VDESC3D( N ) = 'Fractional land use'

            N = N + 1 ! 3
            VTYPE3D( N ) = M3REAL
            VNAME3D( N ) = 'LAI'
            UNITS3D( N ) = 'dimensionless'
            VDESC3D( N ) = 'leaf area index for land use category'

            N = N + 1 ! 4
            VTYPE3D( N ) = M3REAL
            VNAME3D( N ) = 'USTAR'
            UNITS3D( N ) = 'm s-1'
            VDESC3D( N ) = 'friction velocity for land use category'

            N = N + 1 ! 5
            VTYPE3D( N ) = M3REAL
            VNAME3D( N ) = 'Z0'
            UNITS3D( N ) = 'm'
            VDESC3D( N ) = 'surface roughness for land use category'

            N = N + 1 ! 6
            VTYPE3D( N ) = M3REAL
            VNAME3D( N ) = 'RST'
            UNITS3D( N ) = 's m-1'
            VDESC3D( N ) = 'Stomatal resistance to water vapor'

            N = N + 1 ! 7
            VTYPE3D( N ) = M3REAL
            VNAME3D( N ) = 'VEG'
            UNITS3D( N ) = 'ratio'
            VDESC3D( N ) = 'Vegetation coverage'

            DO V = 1, N_SPC_DEPV
               If( Tile_Data%Vd_Out( V ) ) Then

                  N = N + 1 
                  VNAME = trim( Tile_Data%Vd_Name( V ) )
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = VNAME
                  UNITS3D( N ) = 'cm s-1'
                  VDESC3D( N ) = 'Dry deposition velocity'

               End If
            END DO
         
C Open diagnostic file

            IF ( .NOT. OPEN3( CTM_DEPV_MOS, FSNEW3, PNAME ) ) THEN
               XMSG = 'Could not create '// TRIM( CTM_DEPV_MOS) // ' file'
               CALL M3EXIT( PNAME, DATE, TIME, XMSG, XSTAT1 )
            END IF

         END IF
         Return
         End Subroutine Open_depv

         Subroutine Write_depv( DATE, TIME )

         USE CGRID_SPCS, ONLY : N_SPC_DEPV
         USE HGRD_DEFN
         USE UTILIO_DEFN
         USE MOSAIC_MOD,   Only: Tile_Data
         USE ASX_DATA_MOD, Only: MOSAIC_DATA

         Implicit None    

         Integer, Intent( IN ) :: DATE
         Integer, Intent( IN ) :: TIME
!         Real,    Intent( IN ) :: DEPV(:,:,:)

         Integer               :: N

         CHARACTER( 16 )  :: PNAME = 'WRITE_DEPV'
         CHARACTER( 120 ) :: XMSG = ' '
         
         INCLUDE SUBST_FILES_ID  ! file name parameters

         DO N = 1, N_SPC_DEPV
            If( Tile_Data%Vd_Out( N ) ) Then
               IF ( .NOT. WRITE3( CTM_DEPV_DIAG, trim( Tile_Data%Vd_Name( N ) ),
     &                            DATE, TIME, Tile_Data%Grd_Vd(:,:,N)*100.0 ) ) THEN
                  XMSG = 'Could not write ' // CTM_DEPV_DIAG // ' file'
                  CALL M3EXIT ( PNAME, DATE, TIME, XMSG, XSTAT1 )
               END IF
            End If
         END DO

         IF ( MOSAIC ) THEN


!-----------------------------------------------------------------------------------------!
!                               STAGE Diagnostic Output                                   !
!-----------------------------------------------------------------------------------------!

               IF ( .NOT. WRITE3( CTM_DEPV_MOS, 'LUFRAC',
     &                            DATE, TIME, Tile_Data%LUFRAC ) ) THEN
                   XMSG = 'Could not write ' // CTM_DEPV_MOS // ' file'
                  CALL M3EXIT ( PNAME, DATE, TIME, XMSG, XSTAT1 )
               END IF


               IF ( .NOT. WRITE3( CTM_DEPV_MOS, 'RA',
     &                            DATE, TIME, MOSAIC_DATA%RA ) ) THEN
                   XMSG = 'Could not write ' // CTM_DEPV_MOS // ' file'
                  CALL M3EXIT ( PNAME, DATE, TIME, XMSG, XSTAT1 )
               END IF

               IF ( .NOT. WRITE3( CTM_DEPV_MOS, 'RST',
     &                            DATE, TIME, Mosaic_Data%RSTW ) ) THEN
                   XMSG = 'Could not write ' // CTM_DEPV_MOS // ' file'
                  CALL M3EXIT ( PNAME, DATE, TIME, XMSG, XSTAT1 )
               END IF

               IF ( .NOT. WRITE3( CTM_DEPV_MOS, 'LAI',
     &                            DATE, TIME, MOSAIC_DATA%LAI ) ) THEN
                   XMSG = 'Could not write ' // CTM_DEPV_MOS // ' file'
                  CALL M3EXIT ( PNAME, DATE, TIME, XMSG, XSTAT1 )
               END IF

               IF ( .NOT. WRITE3( CTM_DEPV_MOS, 'USTAR',
     &                            DATE, TIME, MOSAIC_DATA%USTAR ) ) THEN
                   XMSG = 'Could not write ' // CTM_DEPV_MOS // ' file'
                  CALL M3EXIT ( PNAME, DATE, TIME, XMSG, XSTAT1 )
               END IF

               IF ( .NOT. WRITE3( CTM_DEPV_MOS, 'Z0',
     &                            DATE, TIME, MOSAIC_DATA%Z0 ) ) THEN
                   XMSG = 'Could not write ' // CTM_DEPV_MOS // ' file'
                  CALL M3EXIT ( PNAME, DATE, TIME, XMSG, XSTAT1 )
               END IF

               IF ( .NOT. WRITE3( CTM_DEPV_MOS, 'VEG',
     &                            DATE, TIME, MOSAIC_DATA%VEG ) ) THEN
                   XMSG = 'Could not write ' // CTM_DEPV_MOS // ' file'
                  CALL M3EXIT ( PNAME, DATE, TIME, XMSG, XSTAT1 )
               END IF

               DO N = 1, N_SPC_DEPV
                  If( Tile_Data%Vd_Out( N ) ) Then
                     IF ( .NOT. WRITE3( CTM_DEPV_MOS, 
     &                                  trim( Tile_Data%Vd_Name( N ) ),
     &                                  DATE, TIME,  Tile_Data%Lu_Vd(:,:,N,:)*100.0 ) ) THEN
                         XMSG = 'Could not write ' // CTM_DEPV_MOS // ' file'
                        CALL M3EXIT ( PNAME, DATE, TIME, XMSG, XSTAT1 )
                     End If
                  End If
               END DO
         End If

         End Subroutine Write_depv

      End Module STAGE_OUTPUT
      
