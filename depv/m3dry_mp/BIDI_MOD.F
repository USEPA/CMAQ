
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
C $Header: /project/yoj/arc/CCTM/src/depv/m3dry_mp/BIDI_MOD.F,v 1.1 2011/09/30 19:10:29 sjr Exp $

c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module Bidi_Mod

C Contains the shared variables and subrountes needed for the bidirectional 
C NH3 flux model in CMAQ
C
C OPASX_MEDIA - Opens the output file for NH4+ and H+ in the soil water solution
C               
C Revision History: J. Bash Mar 15 11: Created
C                   J. Bash May 11 11: Updated for CMAQ 5.0

      Implicit None
! Private variables
      Integer, Parameter, Private :: NHx_AQ_CONC  = 4
      Integer, Parameter, Private :: NHx_GAS_CONC = 0      
      Integer, Parameter, Private :: NHx_SOL_CONC = 0     
      Integer, Parameter, Private :: NHx_TOT = NHx_AQ_CONC + NHx_GAS_CONC + NHx_SOL_CONC       
      Integer, Parameter, Private :: HG_AQ_CONC   = 3
      Integer, Parameter, Private :: HG_GAS_CONC  = 1      
      Integer, Parameter, Private :: HG_SOL_CONC = 2           
      
      Integer, Save,   Private :: N_Hg
      INTEGER, Save,   Private :: LOGDEV      
      CHARACTER( 96 ), Private :: XMSG = ' '
! shared variables
      CHARACTER( 16 ), Save , Allocatable :: MEDIA_NAMES( : )
      Integer, Parameter      :: HG_TOT = HG_AQ_CONC + HG_GAS_CONC + HG_SOL_CONC       
      Integer, Save           :: N_TOT
      Real, Save, Allocatable :: gamma1 ( :,: ) ! soil NH4/H
      Real, Save, Allocatable :: gamma2 ( :,: ) ! soil NH4/H
      Real, Save, Allocatable :: MHp1   ( :,: ) ! molar H+
      Real, Save, Allocatable :: MHp2   ( :,: ) ! molar H+  
      Real, Save, Allocatable :: CMEDIA ( :,:,: ) ! surface layer concs 
      Real, Save, Allocatable :: lufrac ( :,:,: ) 
      LOGICAL, SAVE     :: HGBIDI  
      LOGICAL, SAVE     :: INIT_LAI   
      LOGICAL, SAVE     :: INIT_ATX 
      LOGICAL, SAVE     :: INIT_ASX              

      Contains 
         SUBROUTINE INIT_BIDI( ABFLUX )
         USE GRID_CONF
         USE CGRID_SPCS          ! CGRID mechanism species
         USE UTILIO_DEFN
         
         IMPLICIT NONE         
         
         LOGICAL,     INTENT( IN ) :: abflux       
         
         IF(INDEX1('HG', N_GC_DDEP, GC_DDEP) .GT. 0) HGBIDI = .TRUE.
         
         If( ABFLUX .AND. .NOT. HGBIDI) Then
            N_TOT = NHx_TOT
            N_Hg  = 0            
            ALLOCATE( MEDIA_NAMES( N_TOT ) )

            MEDIA_NAMES(1) = 'Gamma1         ' 
            MEDIA_NAMES(2) = 'Gamma2         '
            MEDIA_NAMES(3) = 'MHpsl1         '
            MEDIA_NAMES(4) = 'MHpsl2         '
         End If
         If(.NOT. ABFLUX .AND. HGBIDI) Then
            N_TOT = HG_TOT
            N_Hg  = HG_TOT
            ALLOCATE( MEDIA_NAMES( HG_TOT ) )

            MEDIA_NAMES(1) = 'DGM            ' ! Dissolved gaseous Hg
            MEDIA_NAMES(2) = 'DRM            '! Dissolved reactive Hg
            MEDIA_NAMES(3) = 'HGSOIL         '
            MEDIA_NAMES(4) = 'HGZ0           '
            MEDIA_NAMES(5) = 'HGMES          ' ! mesophyll Hg
            MEDIA_NAMES(6) = 'HGCUT          '! cuticular Hg        
         End If
         If(ABFLUX .AND. HGBIDI) Then
            N_TOT = HG_TOT + NHx_TOT
            N_Hg  = HG_TOT
            ALLOCATE( MEDIA_NAMES( N_TOT ) )

            MEDIA_NAMES(1)  = 'DGM            ' ! Dissolved gaseous Hg
            MEDIA_NAMES(2)  = 'DRM            '! Dissolved reactive Hg
            MEDIA_NAMES(3)  = 'HGSOIL         '
            MEDIA_NAMES(4)  = 'HGZ0           '
            MEDIA_NAMES(5)  = 'HGMES          ' ! mesophyll Hg
            MEDIA_NAMES(6)  = 'HGCUT          '! cuticular Hg        
            MEDIA_NAMES(7)  = 'Gamma1         ' 
            MEDIA_NAMES(8)  = 'Gamma2         '
            MEDIA_NAMES(9)  = 'MHpsl1         '
            MEDIA_NAMES(10) = 'MHpsl2         '
         End If
         
         ! allocate the media array variable
         IF ( .NOT. ALLOCATED ( CMEDIA ) ) THEN
            ALLOCATE ( CMEDIA( N_TOT,NCOLS,NROWS ) )
            CMEDIA( :,:,: ) = 0.0
         END IF         
         
         RETURN
         
         END SUBROUTINE INIT_BIDI
!*****************************************************************************
!************** Iput / output section of the module **************************
!*****************************************************************************

         SUBROUTINE OPASX_MEDIA( JDATE, JTIME, TSTEP, ABFLUX )

         USE GRID_CONF
         USE CGRID_SPCS          ! CGRID mechanism species
         USE UTILIO_DEFN

         IMPLICIT NONE

         INCLUDE SUBST_FILES_ID  ! file name parameters

         INTEGER,     INTENT( IN ) :: JDATE
         INTEGER,     INTENT( IN ) :: JTIME
         INTEGER,     INTENT( IN ) :: TSTEP
         LOGICAL,     INTENT( IN ) :: abflux

         CHARACTER( 16 ) :: PNAME = 'OPASX_MEDIA'
         CHARACTER( 80 ) :: VARDESC
         CHARACTER( 96 ) :: MSG = ' '

         INTEGER  N, V, L
         
C--------------------------------------------------------------------------
         LOGDEV = INIT3()  
         
         If( ABFLUX .AND. .NOT. HGBIDI) Then
!            N_TOT = NHx_TOT
!            N_Hg  = 0            
!            ALLOCATE( MEDIA_NAMES( N_TOT ) )

!            MEDIA_NAMES(1) = 'Gamma1         ' 
!            MEDIA_NAMES(2) = 'Gamma2         '
!            MEDIA_NAMES(3) = 'MHpsl1         '
!            MEDIA_NAMES(4) = 'MHpsl2         '

            IF ( .NOT. OPEN3( MEDIA_CONC, FSRDWR3, PNAME ) ) THEN!

               XMSG = 'Could not open ' // MEDIA_CONC // ' file for update - '
     &             // 'try to open new'
               CALL M3MESG( XMSG )

               FTYPE3D = GRDDED3
               SDATE3D = JDATE
               STIME3D = JTIME
               TSTEP3D = TSTEP
               CALL NEXTIME( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour

               NVARS3D = NHx_TOT
               NCOLS3D = GL_NCOLS
               NROWS3D = GL_NROWS
               NLAYS3D =     1
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
               DO L = 1, NLAYS3D + 1
                  VGLVS3D( L ) = VGLVS_GD( L )
               END DO
               GDNAM3D = GRID_NAME  ! from HGRD_DEFN

               FDESC3D = ' '   ! array

               FDESC3D( 1 ) = 'Multimedia concentration estimates from integrated ambient '
     &                     // 'concentraitons and surface exchange algorithms'
                        
               N = 0

               DO V = 1, NHx_AQ_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'mol/l'
                  VDESC3D( N ) = 'aqueous phase concentration'
               END DO

               N = NHx_AQ_CONC

               DO V = 1, NHx_GAS_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'ppmV g'
                  VDESC3D( N ) = 'gas phase concentration'
               END DO

               N = NHx_AQ_CONC + NHx_GAS_CONC

               DO V = 1, NHx_SOL_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'umol/g'
                  VDESC3D( N ) = 'Solid phase concentration'
               END DO              
            EndIf
         EndIf ! abflux
         If(.NOT. ABFLUX .AND. HGBIDI) Then
!            N_TOT = HG_TOT
!            N_Hg  = HG_TOT
!            ALLOCATE( MEDIA_NAMES( HG_TOT ) )

!            MEDIA_NAMES(1) = 'DGM            ' ! Dissolved gaseous Hg
!            MEDIA_NAMES(2) = 'DRM            '! Dissolved reactive Hg
!            MEDIA_NAMES(3) = 'HGSOIL         '
!            MEDIA_NAMES(4) = 'HGZ0           '
!            MEDIA_NAMES(5) = 'HGMES          ' ! mesophyll Hg
!            MEDIA_NAMES(6) = 'HGCUT          '! cuticular Hg        
            
            IF ( .NOT. OPEN3( MEDIA_CONC, FSRDWR3, PNAME ) ) THEN!

               XMSG = 'Could not open ' // MEDIA_CONC // ' file for update - '
     &             // 'try to open new'
               CALL M3MESG( XMSG )

               FTYPE3D = GRDDED3
               SDATE3D = JDATE
               STIME3D = JTIME
               TSTEP3D = TSTEP
               CALL NEXTIME( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour            
               NVARS3D = HG_TOT
               NCOLS3D = GL_NCOLS
               NROWS3D = GL_NROWS
               NLAYS3D =     1
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

               FDESC3D = ' '   ! array

               FDESC3D( 1 ) = 'Multimedia concentration estimates from integrated ambient '
     &                     // 'concentraitons and surface exchange algorithms'

               NVARS3D = HG_AQ_CONC + HG_GAS_CONC + HG_SOL_CONC
               NLAYS3D = 1

               N = 0

               DO V = 1, HG_AQ_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'umol/mol'
                  VDESC3D( N ) = 'aqueous phase concentration'
               END DO

               N = HG_AQ_CONC

               DO V = 1, HG_GAS_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'ppmV g'
                  VDESC3D( N ) = 'gas phase concentration'
               END DO

               N = HG_AQ_CONC + HG_GAS_CONC

               DO V = 1, HG_SOL_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'umol/g'
                  VDESC3D( N ) = 'Solid phase concentration'
               END DO
            End If                                              
         End If
         
         If(ABFLUX .AND. HGBIDI) Then
!            N_TOT = HG_TOT + NHx_TOT
!            N_Hg  = HG_TOT
!            ALLOCATE( MEDIA_NAMES( N_TOT ) )

!            MEDIA_NAMES(1)  = 'DGM            ' ! Dissolved gaseous Hg
!            MEDIA_NAMES(2)  = 'DRM            '! Dissolved reactive Hg
!            MEDIA_NAMES(3)  = 'HGSOIL         '
!            MEDIA_NAMES(4)  = 'HGZ0           '
!            MEDIA_NAMES(5)  = 'HGMES          ' ! mesophyll Hg
!            MEDIA_NAMES(6)  = 'HGCUT          '! cuticular Hg        
!            MEDIA_NAMES(7)  = 'Gamma1         ' 
!            MEDIA_NAMES(8)  = 'Gamma2         '
!            MEDIA_NAMES(9)  = 'MHpsl1         '
!            MEDIA_NAMES(10) = 'MHpsl2         '
            
            IF ( .NOT. OPEN3( MEDIA_CONC, FSRDWR3, PNAME ) ) THEN!

               XMSG = 'Could not open ' // MEDIA_CONC // ' file for update - '
     &             // 'try to open new'
               CALL M3MESG( XMSG )

               FTYPE3D = GRDDED3
               SDATE3D = JDATE
               STIME3D = JTIME
               TSTEP3D = TSTEP
               CALL NEXTIME( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour            
               NVARS3D = HG_TOT
               NCOLS3D = GL_NCOLS
               NROWS3D = GL_NROWS
               NLAYS3D =     1
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

               FDESC3D = ' '   ! array

               FDESC3D( 1 ) = 'Multimedia concentration estimates from integrated ambient '
     &                     // 'concentraitons and surface exchange algorithms'

               NVARS3D = HG_AQ_CONC + HG_GAS_CONC + HG_SOL_CONC
               NLAYS3D = 1

               N = 0

               DO V = 1, HG_AQ_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'umol/mol'
                  VDESC3D( N ) = 'aqueous phase concentration'
               END DO

               N = HG_AQ_CONC

               DO V = 1, HG_GAS_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'ppmV g'
                  VDESC3D( N ) = 'gas phase concentration'
               END DO

               N = HG_AQ_CONC + HG_GAS_CONC

               DO V = 1, HG_SOL_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'umol/g'
                  VDESC3D( N ) = 'Solid phase concentration'
               END DO
               
               N = HG_TOT

               DO V = 1, NHx_AQ_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'mol/l'
                  VDESC3D( N ) = 'aqueous phase concentration'
               END DO

               N = HG_TOT + NHx_AQ_CONC

               DO V = 1, NHx_GAS_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'ppmV g'
                  VDESC3D( N ) = 'gas phase concentration'
               END DO

               N = HG_TOT + NHx_AQ_CONC + NHx_GAS_CONC

               DO V = 1, NHx_SOL_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'umol/g'
                  VDESC3D( N ) = 'Solid phase concentration'
               END DO        
            End IF                                      
         End If         
! Open file, then close it for subsequent open by all processors
            
         IF ( .NOT. OPEN3( MEDIA_CONC, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not create '// TRIM( MEDIA_CONC) // ' file'
            CALL M3EXIT( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
         END IF                   

         RETURN

         END SUBROUTINE OPASX_MEDIA

!****************************************************************************

         SUBROUTINE WRASX_MEDIA( MDATE, MTIME, ABFLUX )

         USE GRID_CONF           ! horizontal grid specifications
         USE SUBST_MODULES       ! stenex
         USE UTILIO_DEFN

         IMPLICIT NONE

         INCLUDE SUBST_FILES_ID  ! file name parameters

         INTEGER, INTENT( IN )    :: MDATE
         INTEGER, INTENT( IN )    :: MTIME
         LOGICAL, INTENT( IN )    :: ABFLUX

         REAL          WRMC( My_NCols,My_NRows )         ! media write buffer
         INTEGER V, R, C

         CHARACTER( 16 ) :: PNAME = 'WRASX_MEDIA'
         
!*****************************************************************************         
         LOGDEV = INIT3()    
         
         If( HGBIDI ) Then
            INIT_LAI = .FALSE.   
            INIT_ATX = .FALSE.   
            INIT_ASX = .FALSE.
         End If
         
         If( ABFLUX ) Then
            Do R = 1, My_NRows
               Do C = 1, My_NCols
                  CMedia( N_Hg+1,C,R ) = Gamma1( C,R )
                  CMedia( N_Hg+2,C,R ) = Gamma2( C,R )
                  CMedia( N_Hg+3,C,R ) = MHp1( C,R )
                  CMedia( N_Hg+4,C,R ) = MHp2( C,R )
               End Do
            End Do
         End If
         
         DO V = 1, N_TOT ! species/media loop
            DO R = 1, My_NRows ! row loop
               DO C = 1,My_NCols  ! column loop
                  WRMC( C, R ) = CMEDIA( V, C, R )
               END DO
            END DO

            IF ( .NOT. WRITE3( MEDIA_CONC, MEDIA_NAMES( V ), MDATE, MTIME,
     &                WRMC ) ) THEN
               XMSG = 'Could not write ' // MEDIA_CONC // ' file'
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF

            WRITE( LOGDEV, '( /5X, A, I8, ":", I6.6 )' )
     &         'Timestep written to "' // TRIM( MEDIA_CONC ) //
     &         '" for date and time', MDATE, MTIME

         END DO                 

         RETURN

         END SUBROUTINE WRASX_MEDIA
      End Module Bidi_Mod   
