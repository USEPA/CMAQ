
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

c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module Bidi_Mod

C Contains the shared variables and subroutines needed for the bidirectional 
C flux models in CMAQ
C
C OPASX_MEDIA - Opens the output file for NH4+ and H+ in the soil water solution
C               
C Revision History: J. Bash Mar 15 11: Created
C                   J. Bash May 11 11: Updated for CMAQ 5.0
C                   D. Wong Feb 01 19: removed all MY_N clauses

      Implicit None
! Private variables
      Integer, Parameter, Private :: NHx_AQ_CONC  = 4
      Integer, Parameter, Private :: NHx_GAS_CONC = 0      
      Integer, Parameter, Private :: NHx_SOL_CONC = 0     
      Integer, Parameter, Private :: NHx_TOT = NHx_AQ_CONC + NHx_GAS_CONC + NHx_SOL_CONC       
      Integer, Parameter, Private :: HG_AQ_CONC   = 3
      Integer, Parameter, Private :: HG_GAS_CONC  = 1      
      Integer, Parameter, Private :: HG_SOL_CONC  = 2           
      
      Integer, Save,   Private :: N_Hg
      Character( 96 ), Private :: XMSG = ' '
! shared variables
      Character( 16 ), Save , Allocatable :: MEDIA_NAMES( : )
      Character( 16 ), Save , Allocatable :: MEDIA_UNITS( : )
      Character( 80 ), Save , Allocatable :: MEDIA_DESCR( : )

      Integer, Parameter      :: HG_TOT = HG_AQ_CONC + HG_GAS_CONC + HG_SOL_CONC       
      Integer, Save           :: N_TOT
      Real, Save, Allocatable :: gamma1 ( :,: ) ! soil NH4/H
      Real, Save, Allocatable :: gamma2 ( :,: ) ! soil NH4/H
      Real, Save, Allocatable :: MHp1   ( :,: ) ! molar H+
      Real, Save, Allocatable :: MHp2   ( :,: ) ! molar H+  
      Real, Save, Allocatable :: CMEDIA ( :,:,: ) ! surface layer concs 

      Logical, Save     :: INIT_LAI   
      Logical, Save     :: INIT_COMP              


      Contains 
         Subroutine INIT_BIDI()
         Use GRID_CONF
         Use CGRID_SPCS          ! CGRID mechanism species
         Use UTILIO_DEFN
         Use RUNTIME_VARS
         
         Implicit None
         
C... Local:
 
         Character( 16 ) :: PNAME = 'INIT_BIDI       '
         Integer         :: STATUS
         Logical, SAVE   :: INITIALIZED = .FALSE.
         
C--------------------------------------------------------------------------
C Prevent initializing the code twice 
         If ( INITIALIZED ) Return 
         INITIALIZED = .TRUE.

         If ( ABFLUX .And. .Not. HGBIDI ) Then
         
            N_TOT = NHx_TOT
            N_Hg  = 0            
            Allocate( MEDIA_NAMES( N_TOT ), MEDIA_UNITS( N_TOT ), MEDIA_DESCR( N_TOT )  )

            MEDIA_NAMES( 1 ) = 'Gamma1          '
            MEDIA_UNITS( 1 ) = ' '
            MEDIA_DESCR( 1 ) = 'NH4+/H+ in Soil layer 1'
            MEDIA_NAMES( 2 ) = 'Gamma2          '
            MEDIA_UNITS( 2 ) = ' '
            MEDIA_DESCR( 2 ) = 'NH4+/H+ in Soil layer 2'
            MEDIA_NAMES( 3 ) = 'MHpsl1          '
            MEDIA_UNITS( 3 ) = 'mol/l'
            MEDIA_DESCR( 3 ) = 'Molar H+ in Soil layer 1'
            MEDIA_NAMES( 4 ) = 'MHpsl2          '
            MEDIA_UNITS( 4 ) = 'mol/l'
            MEDIA_DESCR( 4 ) = 'Molar H+ in Soil layer 2'
               
         Else If ( .Not. ABFLUX .And. HGBIDI ) Then
            N_TOT = HG_TOT
            N_Hg  = HG_TOT
            Allocate( MEDIA_NAMES( HG_TOT ), MEDIA_UNITS( N_TOT ), MEDIA_DESCR( N_TOT )  )

            MEDIA_NAMES( 1 ) = 'DGM             ' ! Dissolved gaseous Hg
            MEDIA_UNITS( 1 ) = 'umol mol-1' 
            MEDIA_DESCR( 1 ) = 'Surface water dissolved Hg(0)'
            MEDIA_NAMES( 2 ) = 'DRM             ' ! Dissolved reactive Hg
            MEDIA_UNITS( 2 ) = 'umol mol-1' 
            MEDIA_DESCR( 2 ) = 'Surface water dissolved Hg(II)'
            MEDIA_NAMES( 3 ) = 'HGSOIL          '
            MEDIA_UNITS( 3 ) = 'umol mol-1'      
            MEDIA_DESCR( 3 ) = 'Soil water dissolved Hg(0)'
            MEDIA_NAMES( 4 ) = 'HGZ0            '
            MEDIA_UNITS( 4 ) = 'ppmV'      
            MEDIA_DESCR( 4 ) = 'Hg(0) compensation point'
            MEDIA_NAMES( 5 ) = 'HGMES           ' ! mesophyll Hg
            MEDIA_UNITS( 5 ) = 'umol g-1'      
            MEDIA_DESCR( 5 ) = 'Hg(0) bound to leaf mesophyll'
            MEDIA_NAMES( 6 ) = 'HGCUT           ' ! cuticular Hg  
            MEDIA_UNITS( 6 ) = 'umol g-1'      
            MEDIA_DESCR( 6 ) = 'Hg(0) bound to vegetation surfaces'      
            
         Else If ( ABFLUX .And. HGBIDI ) Then
            N_TOT = HG_TOT + NHx_TOT
            N_Hg  = HG_TOT
            Allocate( MEDIA_NAMES( N_TOT ), MEDIA_UNITS( N_TOT ), MEDIA_DESCR( N_TOT )  )

            MEDIA_NAMES( 1 ) = 'DGM             ' ! Dissolved gaseous Hg
            MEDIA_UNITS( 1 ) = 'umol mol-1' 
            MEDIA_DESCR( 1 ) = 'Surface water dissolved Hg(0)'
            MEDIA_NAMES( 2 ) = 'DRM             ' ! Dissolved reactive Hg
            MEDIA_UNITS( 2 ) = 'umol mol-1' 
            MEDIA_DESCR( 2 ) = 'Surface water dissolved Hg(II)'
            MEDIA_NAMES( 3 ) = 'HGSOIL          '
            MEDIA_UNITS( 3 ) = 'umol mol-1'      
            MEDIA_DESCR( 3 ) = 'Soil water dissolved Hg(0)'
            MEDIA_NAMES( 4 ) = 'HGZ0            '
            MEDIA_UNITS( 4 ) = 'ppmV'      
            MEDIA_DESCR( 4 ) = 'Hg(0) compensation point'
            MEDIA_NAMES( 5 ) = 'HGMES           ' ! mesophyll Hg
            MEDIA_UNITS( 5 ) = 'umol g-1'      
            MEDIA_DESCR( 5 ) = 'Hg(0) bound to leaf mesophyll'
            MEDIA_NAMES( 6 ) = 'HGCUT           ' ! cuticular Hg  
            MEDIA_UNITS( 6 ) = 'umol g-1'      
            MEDIA_DESCR( 6 ) = 'Hg(0) bound to vegetation surfaces'   
            MEDIA_NAMES( 7 ) = 'Gamma1          ' 
            MEDIA_UNITS( 7 ) = '' 
            MEDIA_DESCR( 7 ) = 'NH4+/H+ in Soil layer 1'
            MEDIA_NAMES( 8 ) = 'Gamma2          '
            MEDIA_UNITS( 8 ) = '' 
            MEDIA_DESCR( 8 ) = 'NH4+/H+ in Soil layer 2'
            MEDIA_NAMES( 9 ) = 'MHpsl1'
            MEDIA_UNITS( 9 ) = 'mol L-1' 
            MEDIA_DESCR( 9 ) = 'Molar H+ in Soil layer 1'
            MEDIA_NAMES( 10 ) = 'MHpsl2         '
            MEDIA_UNITS( 10 ) = 'mol L-1' 
            MEDIA_DESCR( 10 ) = 'Molar H+ in Soil layer 2'
            
         End If
         
         ! allocate the media array variable
         If ( .Not. Allocated ( CMEDIA ) ) Then
            Allocate ( CMEDIA( NCOLS,NROWS,N_TOT ) )
            CMEDIA = 0.0
         End If         
         
         Return
         
         End Subroutine INIT_BIDI
!*****************************************************************************
!************** Iput / output section of the module **************************
!*****************************************************************************

         Subroutine OPASX_MEDIA( JDATE, JTIME, TSTEP )

         Use GRID_CONF
         Use CGRID_SPCS          ! CGRID mechanism species
         Use UTILIO_DEFN

         Implicit None

         Include SUBST_FILES_ID  ! file name parameters

         Integer,     Intent( In ) :: JDATE
         Integer,     Intent( In ) :: JTIME
         Integer,     Intent( In ) :: TSTEP

         Character( 16 ) :: PNAME = 'OPASX_MEDIA     '
         Character( 96 ) :: MSG = ' '

         Integer  N, V, L
         
C--------------------------------------------------------------------------
         
         If ( ABFLUX .And. .Not. HGBIDI) Then

            If ( .Not. OPEN3( MEDIA_CONC, FSRDWR3, PNAME ) ) Then

               XMSG = 'Could not open ' // MEDIA_CONC // ' file for update - '
     &             // 'try to open new'
               Call M3MESG( XMSG )

               FTYPE3D = GRDDED3
               SDATE3D = JDATE
               STIME3D = JTIME
               TSTEP3D = TSTEP
               Call NEXTIME( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour

               NVARS3D = N_TOT
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
               Do L = 1, NLAYS3D + 1
                  VGLVS3D( L ) = VGLVS_GD( L )
               End Do
               GDNAM3D = GRID_NAME  ! from HGRD_DEFN

               FDESC3D = ' '   ! array

               FDESC3D( 1 ) = 'Multimedia concentration estimates from integrated ambient '
     &                     // 'NH3 concentrations and surface exchange algorithms'
                        
               N = 0

               Do V = 1, NHx_AQ_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'mol L-1'
                  VDESC3D( N ) = 'aqueous phase concentration'
               End Do

               N = NHx_AQ_CONC

               Do V = 1, NHx_GAS_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'ppmV'
                  VDESC3D( N ) = 'gas phase concentration'
               End Do

               N = NHx_AQ_CONC + NHx_GAS_CONC

               Do V = 1, NHx_SOL_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'umol g-1'
                  VDESC3D( N ) = 'Solid phase concentration'
               End Do              
            End If
            
         Else If ( .Not. ABFLUX .And. HGBIDI ) Then              
            
            If ( .Not. OPEN3( MEDIA_CONC, FSRDWR3, PNAME ) ) Then!

               XMSG = 'Could not open ' // MEDIA_CONC // ' file for update - '
     &             // 'try to open new'
               Call M3MESG( XMSG )

               FTYPE3D = GRDDED3
               SDATE3D = JDATE
               STIME3D = JTIME
               TSTEP3D = TSTEP
               Call NEXTIME( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour            
               NVARS3D = N_TOT
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
     &                     // 'HG concentrations and surface exchange algorithms'

               NLAYS3D = 1

               N = 0

               Do V = 1, HG_AQ_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'umol mol-1'
                  VDESC3D( N ) = 'aqueous phase concentration'
               End Do

               N = HG_AQ_CONC

               Do V = 1, HG_GAS_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'ppmV'
                  VDESC3D( N ) = 'gas phase concentration'
               End Do

               N = HG_AQ_CONC + HG_GAS_CONC

               Do V = 1, HG_SOL_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'umol g-1'
                  VDESC3D( N ) = 'Solid phase concentration'
               End Do
            End If
            
         Else If ( ABFLUX .And. HGBIDI ) Then         
            
            If ( .Not. OPEN3( MEDIA_CONC, FSRDWR3, PNAME ) ) Then

               XMSG = 'Could not open ' // MEDIA_CONC // ' file for update - '
     &             // 'try to open new'
               Call M3MESG( XMSG )

               FTYPE3D = GRDDED3
               SDATE3D = JDATE
               STIME3D = JTIME
               TSTEP3D = TSTEP
               Call NEXTIME( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour            
               NVARS3D = N_TOT
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
     &                     // 'NH3 and HG concentrations and surface exchange algorithms'

               NLAYS3D = 1

               N = 0

               Do V = 1, HG_AQ_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'umol mol-1'
                  VDESC3D( N ) = 'aqueous phase concentration'
               End Do

               N = HG_AQ_CONC

               Do V = 1, HG_GAS_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'ppmV'
                  VDESC3D( N ) = 'gas phase concentration'
               End Do

               N = HG_AQ_CONC + HG_GAS_CONC

               Do V = 1, HG_SOL_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'umol g-1'
                  VDESC3D( N ) = 'Solid phase concentration'
               End Do
               
               N = HG_TOT

               Do V = 1, NHx_AQ_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'mol L-1'
                  VDESC3D( N ) = 'aqueous phase concentration'
               End Do

               N = HG_TOT + NHx_AQ_CONC

               Do V = 1, NHx_GAS_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'ppmV'
                  VDESC3D( N ) = 'gas phase concentration'
               End Do

               N = HG_TOT + NHx_AQ_CONC + NHx_GAS_CONC

               Do V = 1, NHx_SOL_CONC
                  N = N + 1
                  VTYPE3D( N ) = M3REAL
                  VNAME3D( N ) = MEDIA_NAMES( N )
                  UNITS3D( N ) = 'umol g-1'
                  VDESC3D( N ) = 'Solid phase concentration'
               End Do        
            End If
                                                  
         End If         
! Open file, then close it for subsequent open by all processors
            
         If ( .Not. OPEN3( MEDIA_CONC, FSNEW3, PNAME ) ) Then
            XMSG = 'Could not create '// TRIM( MEDIA_CONC) // ' file'
            Call M3EXIT( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
         End If                   

         Return

         End Subroutine OPASX_MEDIA

!****************************************************************************

         Subroutine WRASX_MEDIA( MDATE, MTIME )

! Revision History.
!     Aug 12, 15 D. Wong: added code to handle parallel I/O implementation
!     Jan 16, 16 J. Young: update log file once per output; consistent tokens

         Use GRID_CONF           ! horizontal grid specifications
         Use UTILIO_DEFN

         Implicit None

         Include SUBST_FILES_ID  ! file name parameters

         Integer, Intent( In )    :: MDATE
         Integer, Intent( In )    :: MTIME

         Logical, Save :: FIRSTIME = .TRUE.

         Real          WRMC( NCols,NRows )         ! media write buffer
         Integer V, R, C

         Character( 16 ) :: PNAME = 'WRASX_MEDIA     '
         
!*****************************************************************************         
         
         If ( HGBIDI ) Then
            INIT_LAI  = .FALSE.   
            INIT_COMP = .FALSE.   
         End If
         
         If ( ABFLUX ) Then
            Do R = 1, NRows
               Do C = 1, NCols
                  CMedia( C,R,N_Hg+1 ) = Gamma1( C,R )
                  CMedia( C,R,N_Hg+2 ) = Gamma2( C,R )
                  CMedia( C,R,N_Hg+3 ) = MHp1( C,R )
                  CMedia( C,R,N_Hg+4 ) = MHp2( C,R )
               End Do
            End Do
         End If

#ifdef parallel_io
         If ( FIRSTIME ) Then
            FIRSTIME = .FALSE.
            If ( .Not. IO_PE_INCLUSIVE ) Then
               If ( .Not. OPEN3( MEDIA_CONC, FSREAD3, PNAME ) ) Then
                  XMSG = 'Could not open ' // TRIM( MEDIA_CONC )
                  Call M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
               End If
            End If
         End If
#endif
         
         Do V = 1, N_TOT ! species/media loop
            Do R = 1, NRows ! row loop
               Do C = 1,NCols  ! column loop
                  WRMC( C, R ) = CMEDIA( C,R,V  )
               End Do
            End Do

            If ( .Not. WRITE3( MEDIA_CONC, MEDIA_NAMES( V ), MDATE, MTIME,
     &                WRMC ) ) Then
               XMSG = 'Could not write ' // MEDIA_CONC // ' file'
               Call M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            End If

         End Do                 

         Write( LOGDEV, '( /5X, A, I8, ":", I6.6 )' )
     &         'Timestep written to "' // TRIM( MEDIA_CONC ) //
     &         '" for date and time', MDATE, MTIME

         Return

         End Subroutine WRASX_MEDIA

      End Module Bidi_Mod   
