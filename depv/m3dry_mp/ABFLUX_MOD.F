
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
C $Header: /project/yoj/arc/CCTM/src/depv/m3dry_mp/ABFLUX_MOD.F,v 1.1 2011/09/30 13:06:39 sjr Exp $

c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module ABFlux_Mod

C Contains the shared variables and subrountes needed for the bidirectional 
C NH3 flux model in CMAQ
C
C INIT_ABFLUX - Intitiales the NH3 flux routines, allocates arrays, reads in
C               intial soil NH3 & H concentrations, and fertilizer application
C               amounts and timing for the model run
C
C Revision History: J. Bash Dec 08 10: Created
C                   J. Bash May 11 11: Updated for CMAQ 5.0

      Implicit None
C shared variables

C Private variables used in this module and subroutines  
      Real, Save, Allocatable, Private :: frac_ir( :,: ) ! irrigated fraction of ag    
      Real, Save, Allocatable, Private :: fagam1 ( :,: ) ! due to fertilizer app
      Real, Save, Allocatable, Private :: fagam2 ( :,: ) ! due to fertilizer app
      Real, Save, Allocatable, Private :: NH4ps1 ( :,:,: ) ! Initial Soil NH4
      Real, Save, Allocatable, Private :: NH4ps2 ( :,:,: ) ! Initial Soil NH4
      Real, Save, Allocatable, Private :: pHs1   ( :,:,: )
      Real, Save, Allocatable, Private :: pHs2   ( :,:,: )           
      Real, Save,              Private :: C_gam           ! Canadian fertilizer facter
      Integer, Save, Private      :: logdev       
     
      Contains 
         Subroutine Init_ABFlux(jdate,jtime)
 
         Use HGRD_DEFN           ! horizontal grid specifications
         Use UTILIO_DEFN         
         Use Bidi_Mod, Only: gamma1, gamma2, MHp1, MHp2

         Implicit None 
! Includes
         Include SUBST_CONST     ! constants
         Include SUBST_FILES_ID  ! file name parameters

! Local Variables

         Integer, Intent( In )  :: jdate
         Integer, Intent( In )  :: jtime
!         Integer, Intent( In )  :: endcol
!         Integer, Intent( In )  :: endrow
!         Integer, Intent( In )  :: strtcol
!         Integer, Intent( In )  :: strtrow   
         Integer                :: c,r,l,k
         Integer, Parameter :: e2c_cats = 42   ! number of crop catigories
         Integer, Parameter :: N_apps   = 10   ! number of fertilizer applicaitons        
         Integer            :: GXOFF, GYOFF    ! global origin offset from file
         integer, save      :: strtcol, endcol, strtrow, endrow
  
         Real, Save, Allocatable :: F_depth ( :,:,: ) ! fertlizer application date 
         Real, Save, Allocatable :: F_rate  ( :,:,: ) ! fertlizer application date 
         Real, Save, Allocatable :: Beld_ag ( :,:,: )        
         Real                    :: Tot_Ag          ! total ag in the grid cell    
 
         Character( 16 ), Parameter :: pname = 'Init_ABFlux' 
         CHARACTER( 16 )            :: vname
         CHARACTER( 96 )            :: xmsg = ' '
         Character( 20 ), Save      :: Beld_Names(e2c_cats)
         Data Beld_Names / 'Hay', 'Hay_ir', 'Alfalfa', 'Alfalfa_ir', 'Other_Grass',
     &                     'Other_Grass_ir', 'Barley', 'Barley_ir', 'BeansEdible',
     &                     'BeansEdible_ir', 'CornGrain', 'CornGrain_ir', 'CornSilage',
     &                     'CornSilage_ir', 'Cotton', 'Cotton_ir', 'Oats', 'Oats_ir',
     &                     'Peanuts', 'Peanuts_ir', 'Potatoes', 'Potatoes_ir', 
     &                     'Rice', 'Rice_ir', 'Rye', 'Rye_ir', 'SorghumGrain', 
     &                     'SorghumGrain_ir', 'SorghumSilage', 'SorghumSilage_ir', 
     &                     'Soybeans', 'Soybeans_ir','Wheat_Spring', 'Wheat_Spring_ir', 
     &                     'Wheat_Winter', 'Wheat_Winter_ir', 'Other_Crop', 'Other_Crop_ir', 
     &                     'Canola', 'Canola_ir', 'Beans', 'Beans_ir' /
 
         logdev =  init3() 
 
         IF ( .NOT. desc3( grid_cro_2d ) ) THEN
            xmsg = 'Could not get  grid_cro_2d  file description '
            CALL m3exit( pname, jdate, jtime, xmsg, xstat2 )
         END IF         
         
         CALL SUBHFILE ( grid_cro_2d, GXOFF, GYOFF, STRTCOL, ENDCOL, STRTROW, ENDROW )         

! Find the Canadian fertilizer factor based off of Sheppard et al 2010 Canadian J. Soil Sci. & 
! Zhang et al. 2010 JGR 
        Select Case(jdate)
           Case(60:90)
              C_gam = 811.5
           Case(91:120)
              C_gam = 3447.3
           Case(121:151)
              C_gam = 8702.8
           Case(152:181)
              C_gam = 1269.3
           Case(182:212)
              C_gam = 667.1
           Case(213:243)
              C_gam = 704.2
           Case(244:273)
              C_gam = 811.5
           Case(274:304)
              C_gam = 1376.7
           Case(305:334)
              C_gam = 1079.6
           Case Default
              C_gam = 630.0
        End Select           
                  
! get soil pH to estimate the emissions potential
! time in the files is not the same    
         IF ( .NOT. OPEN3( E2C_SOIL, FSREAD3, PNAME ) ) THEN
           XMSG = 'Could not open '// E2C_SOIL // ' file'
           CALL M3EXIT ( PNAME, jdate, jtime, XMSG, XSTAT1 )
         END IF
         IF ( .NOT. ALLOCATED ( pHs1 ) ) THEN
            ALLOCATE ( pHs1 ( my_ncols,my_nrows,e2c_cats ) )
         END IF
         

         vname = 'L1_PH'
         IF ( .NOT. Xtract3 (E2C_SOIL, vname, 1, e2c_cats, strtrow,  endrow,
     $                       strtcol, endcol, jdate, 0, pHs1) ) THEN
            WRITE( xmsg,9001 ) vname, E2C_SOIL
            GO TO 1001
         END IF
         IF ( .NOT. ALLOCATED ( pHs2 ) ) THEN
            ALLOCATE ( pHs2 ( my_ncols,my_nrows,e2c_cats ) )
         END IF
         
         vname = 'L2_PH'
         IF ( .NOT. Xtract3 (E2C_SOIL, vname, 1, e2c_cats, strtrow,  endrow,
     $                       strtcol, endcol, jdate, 0, pHs2) ) THEN
            WRITE( xmsg,9001 ) vname, E2C_SOIL
            GO TO 1001
         END IF
! get soil fertilizer applicaiton rates and dates
! time in the files is not the same    
         IF ( .NOT. OPEN3( E2C_FERT, FSREAD3, PNAME ) ) THEN
           XMSG = 'Could not open '// E2C_FERT // ' file'
           CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF                  
         IF ( .NOT. ALLOCATED ( F_depth ) ) THEN
            ALLOCATE ( F_depth ( my_ncols,my_nrows,e2c_cats ) )
         END IF
         IF ( .NOT. ALLOCATED ( F_rate ) ) THEN
            ALLOCATE ( F_rate ( my_ncols,my_nrows,e2c_cats ) )
         END IF
         
         vname = 'APP_DEPTH'
         IF ( .NOT. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow,  endrow,
     $                       strtcol, endcol, jdate, 0, F_depth ) ) THEN
            WRITE( xmsg,9001 ) vname, E2C_FERT
            GO TO 1001
         END IF
         vname = 'MN'
         IF ( .NOT. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow,  endrow,
     $                    strtcol, endcol, jdate, 0, F_rate ) ) THEN
            WRITE( xmsg,9001 ) vname, E2C_FERT
            GO TO 1001
         END IF                 
! get BELD4 land use types time independent files. 
         IF ( .NOT. OPEN3( BELD4_LU, FSREAD3, PNAME ) ) THEN
           XMSG = 'Could not open '// BELD4_LU // ' file'
           CALL M3EXIT ( PNAME, jdate, jtime, XMSG, XSTAT1 )
         END IF                
         IF ( .NOT. ALLOCATED ( Beld_ag ) ) THEN
            ALLOCATE ( Beld_ag ( my_ncols,my_nrows,e2c_cats ) )
         END IF
         DO k = 1, e2c_cats
            vname = BELD_Names(k)
            IF ( .NOT. Interpx (BELD4_LU, vname, pname, strtcol, endcol,
     &                          strtrow, endrow, 1, 1, jdate, jtime,
     &                          Beld_ag( :,:,k )) ) THEN   
               WRITE( xmsg,9001 ) vname, BELD4_LU
               GO TO 1001
            END IF
         END DO
  
! Allocate variable needed soil processes and fertilization
         IF ( .NOT. ALLOCATED ( gamma1 ) ) THEN
            ALLOCATE ( gamma1 ( my_ncols,my_nrows ) )
         END IF
         gamma1 = 0.0
         IF ( .NOT. ALLOCATED ( gamma2 ) ) THEN
            ALLOCATE ( gamma2 ( my_ncols,my_nrows ) )
         END IF
         gamma2 = 0.0
         IF ( .NOT. ALLOCATED ( fagam1 ) ) THEN
            ALLOCATE ( fagam1 ( my_ncols,my_nrows ) )
         END IF
         fagam1 = 0.0
         IF ( .NOT. ALLOCATED ( fagam2 ) ) THEN
            ALLOCATE ( fagam2 ( my_ncols,my_nrows ) )
         END IF
         fagam2 = 0.0
         IF ( .NOT. ALLOCATED ( MHp1 ) ) THEN
            ALLOCATE ( MHp1 ( my_ncols,my_nrows ) )
         END IF         
         MHp1 = 0.0
         IF ( .NOT. ALLOCATED ( MHp2 ) ) THEN
            ALLOCATE ( MHp2 ( my_ncols,my_nrows ) )
         END IF
         MHp2 = 0.0
         IF ( .NOT. ALLOCATED ( frac_ir ) ) THEN
            ALLOCATE ( frac_ir ( my_ncols,my_nrows ) )
         END IF
         frac_ir = 0.0                 
! get fertilizer from the previous month to estimate soil NH4+        
! time in the files is not the same
         IF ( .NOT. OPEN3( INIT_MEDC_1, FSREAD3, PNAME ) ) THEN
            Write(Logdev,*) 'Estimating soil NHx from EPIC output'
            IF ( .NOT. OPEN3( E2C_FERT, FSREAD3, PNAME ) ) THEN
               XMSG = 'Could not open '// E2C_FERT // ' file'
               CALL M3EXIT ( PNAME, jdate, jtime, XMSG, XSTAT1 )
            END IF
            IF ( .NOT. ALLOCATED ( NH4ps1 ) ) THEN
               ALLOCATE ( NH4ps1 ( my_ncols,my_nrows,e2c_cats ) )
            END IF
            NH4ps1 = 0.0
            IF ( .NOT. ALLOCATED ( NH4ps2 ) ) THEN
               ALLOCATE ( NH4ps2 ( my_ncols,my_nrows,e2c_cats ) )
            END IF
            NH4ps2 = 0.0  
            vname = 'L1_NO3'
            IF ( .NOT. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow,  endrow,
     $                       strtcol, endcol, jdate, 0, NH4ps1) ) THEN
               WRITE( xmsg,9001 ) vname, E2C_FERT
               GO TO 1001
            END IF
            vname = 'L2_NO3'
            IF ( .NOT. Xtract3 (E2C_FERT, vname, 1, e2c_cats, strtrow,  endrow,
     $                       strtcol, endcol, jdate, 0, NH4ps2) ) THEN
               WRITE( xmsg,9001 ) vname, E2C_FERT
               GO TO 1001
            END IF
            Do c = 1, my_ncols
               Do r = 1, my_nrows
               Tot_Ag = 0.0                   
                   Do l = 1, e2c_cats
! convert units from kg/ha N to mol/l
                     If(pHs1(c,r,l)   .GT. 4.0  .AND.
     $                  pHs2(c,r,l)   .GT. 4.0  .AND.
     $                  pHs1(c,r,l)   .LT. 9.0  .AND.
     $                  pHs2(c,r,l)   .LT. 9.0  .AND.
     $                        .NOT. IsNaN(pHs1(c,r,l)).AND. 
     $                        .NOT. IsNaN(pHs2(c,r,l)).AND.
     &                  Beld_ag(c,r,l) .GT. 0.0 ) Then
                        If(INDEX( Beld_Names( l ), '_ir' ) .GT. 0) Then
                           frac_ir(c,r)  = frac_ir(c,r) + Beld_ag(c,r,l)/100.0
                        End If
! EPIC currently only outputs nitrate concentrations, assume that soil NH4 is 25% of NO3 and NO2
! following based on Venterea and Rolston 2000 JGR                        
                        Tot_Ag        = Tot_Ag + Beld_ag(c,r,l)/100.0                  
                            gamma1(c,r)   = gamma1(c,r)+NH4ps1(c,r,l)*1.0e-4/14.0/0.01/
     &                                  10**(-pHs1(c,r,l))*0.25*Beld_ag(c,r,l)/100.0
                        MHp1(c,r)     = MHp1(c,r)+Beld_ag(c,r,l)/100.0*10**(-pHs1(c,r,l))
                        gamma2(c,r)   = gamma2(c,r)+NH4ps2(c,r,l)*1.0e-4/14.0/0.05/
     &                                  10**(-pHs2(c,r,l))*0.25*Beld_ag(c,r,l)/100.0
                        MHp2(c,r)     = MHp2(c,r)+Beld_ag(c,r,l)/100.0*10**(-pHs2(c,r,l))                                                                  
                        If(.NOT. IsNaN(F_rate(c,r,l)) .AND. F_rate(c,r,l) .GT. 0.0 .AND.
     $                     .NOT. IsNaN(F_depth(c,r,l)) ) Then
! FAO fertlizer use statistics estimate that US fertilizer use in 2002 contained 92.5% NH3     
! injected fertilizer                           
                           If( F_depth(c,r,l) .GT. 0.01 ) Then                                      
                              fagam2(c,r) = fagam2(c,r)+F_rate(c,r,l)*Beld_ag(c,r,l)/100.0*
     $                                            1.0e-4/14.0/0.05/10**(-pHs1(c,r,l))*0.925
                           End If
! Surface applied fertlizer                           
                           If( F_depth(c,r,l) .LE. 0.01 .AND. F_depth(c,r,l) .GE. 0.00 ) Then 
                              fagam1(c,r) = fagam1(c,r)+F_rate(c,r,l)*Beld_ag(c,r,l)/100.0*
     $                                            1.0e-4/14.0/0.01/10**(-pHs2(c,r,l))*0.925                                                      
                           EndIf
                        EndIf
                        If( IsNaN(fagam1(c,r)) .OR. IsNaN(fagam2(c,r))) Then
                            XMSG = 'NaN in fagam Calculation'
                            Call M3EXIT( PName, JDate, JTime,XMSG, XSTAT1)
                        EndIf     
                     EndIf                                
                  EndDo ! e2c_cats
                  if(Tot_Ag .ge. 1e-6 .AND. ! ~ 1.5 ha per grid cell
     $                     MHp1(c,r)/Tot_Ag .gt. 1e-9 .AND.
     $               MHp1(c,r)/Tot_Ag .lt. 1e-4 .AND. 
     $                     MHp2(c,r)/Tot_Ag .gt. 1e-9 .AND.
     $                     MHp2(c,r)/Tot_Ag .lt. 1e-4 ) then
! get the agricultrual only relevant number                                 
                     MHp1(c,r) = MHp1(c,r)/Tot_Ag
                     MHp2(c,r) = MHp2(c,r)/Tot_Ag         
                     gamma1(c,r) = (gamma1(c,r)+fagam1(c,r))/Tot_Ag
                     gamma2(c,r) = (gamma2(c,r)+fagam2(c,r))/Tot_Ag  
! Set a minimum Gamma for Ag based off of Zhang et al. 2010                                             
                     gamma1(c,r)   = max(gamma1(c,r),630.0)
                     gamma2(c,r)   = max(gamma2(c,r),630.0) 
                     If( IsNaN(gamma1(c,r)) .OR. IsNaN(gamma2(c,r)) .OR. 
     $                   IsNaN(MHp2(c,r)) .OR. IsNaN(MHp2(c,r))) Then
                        XMSG = 'NaN in grid cell Gamma Calculation'
                        Call M3EXIT( PName, JDate, JTime,XMSG, XSTAT1)   
                     EndIf
                  else 
                     MHp1(c,r) = 0.0
                     MHp2(c,r) = 0.0
                     gamma1(c,r) = 0.0
                     gamma2(c,r) = 0.0
                  endif        
               EndDo ! r
            EndDo ! c
!            XMSG = 'NaN in fagam Calculation' ! JOB debugging
!            Call M3EXIT( PName, JDate, JTime,XMSG, XSTAT1) ! JOB debuggin
         Else ! read          
! for whatever reason the jdate and jtime get messed up by the e2c files
            Write(Logdev,*) 'Reading soil NHx from Previous CMAQ run'         

            vname = 'Gamma1'
            IF ( .NOT. Xtract3 (INIT_MEDC_1, vname, 1, 1, strtrow,  endrow,
     $                       strtcol, endcol, jdate, 000000, gamma1) ) THEN
               WRITE( xmsg,9001 ) vname, INIT_MEDC_1
               GO TO 1001
            END IF
            vname = 'Gamma2'
            IF ( .NOT. Xtract3 (INIT_MEDC_1, vname, 1, 1, strtrow,  endrow,
     $                       strtcol, endcol, jdate, 000000, gamma2) ) THEN
               WRITE( xmsg,9001 ) vname, INIT_MEDC_1
               GO TO 1001
            END IF
            vname = 'MHpsl1'
            IF ( .NOT. Xtract3 (INIT_MEDC_1, vname, 1, 1, strtrow,  endrow,
     $                       strtcol, endcol, jdate, 000000, MHp1) ) THEN
               WRITE( xmsg,9001 ) vname, INIT_MEDC_1
               GO TO 1001
            END IF
             vname = 'MHpsl2'
            IF ( .NOT. Xtract3 (INIT_MEDC_1, vname, 1, 1, strtrow,  endrow,
     $                       strtcol, endcol, jdate, 000000, MHp2) ) THEN
               WRITE( xmsg,9001 ) vname, INIT_MEDC_1
               GO TO 1001
            END IF
            Do c = 1, My_Ncols
               Do r = 1, My_Nrows
                  Tot_Ag = 0.0 
                    Do l = 1, e2c_cats
! convert units from kg/ha N to mol/l 
                     If(INDEX( Beld_Names( l ), '_ir' ) .GT. 0) Then
                        frac_ir(c,r)  = frac_ir(c,r) + Beld_ag(c,r,l)/100.0
                     End If
                     Tot_Ag        = Tot_Ag + Beld_ag(c,r,l)/100.0                  
                     If(.NOT. IsNaN(F_rate(c,r,l)) .AND. F_rate(c,r,l) .GT. 0.0 .AND.
     $                  .NOT. IsNaN(F_depth(c,r,l)) ) Then
! Injected fertilizer                        
                        If(.NOT. IsNaN(F_depth(c,r,l)) .AND. F_depth(c,r,l) .GT. 0.01 ) Then                                      
                           fagam2(c,r) = fagam2(c,r)+F_rate(c,r,l)*Beld_ag(c,r,l)/100.0*
     $                                         1.0e-4/14.0/0.05/10**(-pHs1(c,r,l))*0.925
                        End If
! surface applied fertilizer                        
                        If(.NOT. IsNaN(F_depth(c,r,l)) .AND. F_depth(c,r,l) .LE. 0.01 
     &                     .AND. F_depth(c,r,l) .GE. 0.00 ) Then 
                           fagam1(c,r) = fagam1(c,r)+F_rate(c,r,l)*Beld_ag(c,r,l)/100.0*
     $                                         1.0e-4/14.0/0.01/10**(-pHs2(c,r,l))*0.925                                                      
                        EndIf
                     EndIf
                     If( IsNaN(fagam1(c,r)) .OR. IsNaN(fagam2(c,r)))        Then                           
                        XMSG = 'NaN in fagam Calculation'
                        Call M3EXIT( PName, JDate, JTime,XMSG, XSTAT1)
                     EndIf                         
                  EndDo ! e2c_cats 
! Agricultural only numbers were written to file                  
                  if(Tot_Ag    .gt. 1.0e-6  .AND.
     $                     MHp1(c,r) .gt. 1e-9 .AND.
     $               MHp1(c,r) .lt. 1e-5  .AND. 
     $                     MHp2(c,r) .gt. 1e-9 .AND.
     $                     MHp2(c,r) .lt. 1e-5 ) then
! get the agricultrual only relevant number                                 
                     gamma1(c,r) = gamma1(c,r)+fagam1(c,r)/Tot_Ag
                     gamma2(c,r) = gamma2(c,r)+fagam2(c,r)/Tot_Ag   
                     If( IsNaN(gamma1(c,r)) .OR. IsNaN(gamma2(c,r)) .OR. 
     $                   IsNaN(MHp2(c,r)) .OR. IsNaN(MHp2(c,r))) Then
                        XMSG = 'NaN in grid cell Gamma Calculation'
                        Call M3EXIT( PName, JDate, JTime,XMSG, XSTAT1)   
                     EndIf   
                  EndIf
               EndDo ! c
            EndDo !r
         EndIF ! INIT_MEDC_1
         
         Return
!------------------------------------------------------------------------------
! Error handeling section
!------------------------------------------------------------------------------
1001     Continue
         Call m3exit( pname, jdate, jtime, xmsg, xstat1 )

C-------------------------------------------------------------------------------
C Format statements.
C-------------------------------------------------------------------------------

9001     Format( 'Failure reading ', a, 1x, 'from ', a )

         Return
         
         End Subroutine Init_ABFlux
                  
!------------------------------------------------------------------------------
! Subroutine to get the soil and canopy compensation point
!------------------------------------------------------------------------------          
         Subroutine Get_Flux(tempg,rh_air,cNH3,rwetsfc,rgw,wg,w2,Sltype,dif,
     &                       row,col,sp,pvd,lnh3,f_stom,f_cut,f_soil,f_emis,f_dep,
     &                       f_ag, f_nat, f_wat, dt)
         
         USE UTILIO_DEFN
         Use Bidi_Mod, Only: gamma1, gamma2, MHp1, MHp2, lufrac
         Use Mosaic_Mod
         Use LSM_MOD
         
         Implicit None
         
         Include SUBST_CONST     ! constants
         Include SUBST_FILES_ID  ! file name parameters
         
         Integer, Intent( IN )  :: Row
         Integer, Intent( IN )  :: Col
         Integer, Intent( IN )  :: dt
         Integer, Intent( IN )  :: sp ! species index
         
         Real, Intent( IN )  :: Tempg        
         Real, Intent( IN )  :: rh_air         
         Real, Intent( IN )  :: cNH3
         Real, Intent( IN )  :: rwetsfc        
         Real, Intent( IN )  :: rgw
         Real, Intent( IN )  :: Wg
         Real, Intent( IN )  :: W2
         Real, Intent( IN )  :: Sltype                
         Real, Intent( IN )  :: dif        
         Real, Intent( OUT ) :: pvd
         Real, Intent( OUT ) :: lnh3
         Real, Intent( OUT ) :: f_stom
         Real, Intent( OUT ) :: f_cut
         Real, Intent( OUT ) :: f_soil
         Real, Intent( OUT ) :: f_emis
         Real, Intent( OUT ) :: f_dep
         Real, Intent( OUT ) :: f_ag
         Real, Intent( OUT ) :: f_nat
         Real, Intent( OUT ) :: f_wat         
         
         REAL,            PARAMETER :: kvis       = 0.132   ! [cm^2 / s] at 273.15K
         REAL(KIND=16)              :: aq           ! Quadradic equation variable
         REAL(KIND=16)              :: bq           ! Quadradic equation variable
         REAL                       :: cnh3c        ! In canopy NH3 concentration [ppm]
         REAL                       :: cnh3g1,cnh3g2        ! NH3 compensation concentration for ground [ppm]
         REAL                       :: cnh3s        ! NH3 compensation concentration for stomatal [ppm]
         REAL(KIND=16)              :: cq           ! Quadradic equation variable
         REAL                       :: del0         ! for Rbg
         REAL(KIND=16)              :: ga           ! Ga  = 1/Ra [m/s]
         REAL                       :: gammas       ! [NH4+]/[H+]
         REAL(KIND=16)              :: gcw
         REAL(KIND=16)              :: gg1(n_lufrac)! Gg  = 1/(Rgnd(nh3)+Rinc) [m/s]  
         REAL(KIND=16)              :: gg2(n_lufrac)! Gg  = 1/(Rgnd(nh3)+Rinc) [m/s]
         REAL(KIND=16)              :: gsb          ! Gsb = 1/(Rstom(nh3)+Rb(nh3)) [m/s]
         REAL(KIND=16)              :: gt
         INTEGER                    :: k,j
         REAL,            PARAMETER :: karman = 0.4 ! von Karman constant
         REAL(KIND=16 )             :: qq           ! intermediate variable
         REAL                       :: rbgj(n_lufrac)
         REAL,            PARAMETER :: rwm = 20!35.0   ! Minimum NH3 cuticle resistance [s/m]
         REAL                       :: rwmb         ! Rwmb = Rwm + Rb
         REAL                       :: rwx          ! Rw = Rwm + Rwx * CNH3C [s/m]
         REAL                       :: scn          ! for Rbg
         REAL                       :: ustg         ! for Rbg
         REAL                       :: vdg          ! Vd(nh3) to non-veg part [m/s]
         REAL                       :: wg_ir        ! 1 cm soil moisture  
         REAL                       :: w5cm         ! soil moisture in top 5 cm (vol frc)
         REAL,            PARAMETER :: d1 = 0.01    ! Top soil layer depth (1 cm)
         REAL(KIND=16)              :: ldry,dp,rsoil1,rsoil2,a1,agfrac,sqrr,ddd
         REAL(KIND=16)              :: watfrac ! water fraction
         INTEGER                    :: ist

         logdev =  init3()
! Compute quasi-laminar boundary layer resistance at the soil surface

         scn  = kvis / dif
         Do j=1,n_lufrac
            ustg = max(ustarj(j,col,row) * EXP(-laij(j,col,row)), 0.001)         
            del0 = 1.0E-4 * kvis / ( karman * ustg )
            rbgj(j)  = ( scn - LOG( 10.0 * del0 ) ) / ( karman * ustg )
         End Do
                        
! Compute soil resistance
                     
         ist = nint(Sltype)
         if (ist.lt.1.or.ist.gt.11) then
            ist = 4
         endif
         If(frac_ir(Col,Row) .GT. 0.0 .AND. wg .LT. 0.75*wfc(ist)) Then            
            wg_ir = (1-frac_ir(Col,Row))*wg+frac_ir(Col,Row)*0.75*wfc(ist)
         Else
            wg_ir = wg
         EndIf
         ldry= d1*(EXP((1-wg_ir/wsat(ist))**5)-1)/1.718
         dp  = dif*1.E-4 * wsat(ist)**2 *                           
     &         (1.-wres(ist)/wsat(ist))**(2.+3./bslp(ist))
         rsoil1=ldry/dp
         w5cm = 0.2 *wg_ir + 0.8 * w2
         If(frac_ir(Col,Row) .GT. 0.0 .AND. w5cm .LT. 0.75*wfc(ist)) Then
            w5cm = (1-frac_ir(Col,Row))*w5cm+frac_ir(Col,Row)*0.75*wfc(ist)
         EndIf
         ldry= 0.05*(EXP((1-w5cm/wsat(ist))**5)-1)/1.718
         rsoil2=ldry/dp
         
         If(Land_scheme .EQ. 'USGS24') Then
            agfrac  = lufrac(  2,Col,Row ) +      lufrac(  3,Col,Row ) + 
     &           0.67*lufrac(  5,Col,Row ) + 0.67*lufrac(  6,Col,Row )
            watfrac = lufrac( 16,Col,Row )  
         End If
         
         If(Land_scheme .EQ. 'NLCD-MODIS') Then
            agfrac  = lufrac( 19,Col,Row ) +      lufrac( 20,Col,Row ) + 
     &                lufrac( 43,Col,Row ) + 0.67*lufrac( 45,Col,Row )
            watfrac = lufrac(  1,Col,Row ) +      lufrac( 31,Col,Row ) + 
     &                lufrac( 48,Col,Row )         
         End If
         
         If(Land_scheme .EQ. 'MODIS') Then
            agfrac  = lufrac( 12,Col,Row ) + 0.67*lufrac( 14,Col,Row )
            watfrac = lufrac( 17,Col,Row )  
         End If
         
         pvd    = 0.0
         lnh3   = 0.0
         f_emis = 0.0
         f_dep  = 0.0
         f_stom = 0.0
         f_cut  = 0.0
         f_soil = 0.0
         f_ag   = 0.0
         f_nat  = 0.0
         f_wat  = 0.0
         
         Do j = 1, n_lufrac
            If( lufrac(j, col, row) .GT. 0.0 ) Then

! If the soil is frozen assume not evasive flux and skip calculation of comp. points         
                 If(Tempg .le. 273.15) Then
                  Cnh3s  = 0.0
                  Cnh3g1 = 0.0
                  Cnh3g2 = 0.0
                  Go To 1002
               End If

! Compute compensation point. gamma is specified according to the amount of 
! cultivated vegetation               
               Gammas = luf_fac( j )
               a1    = 161512.0 / tempg * 10.0 ** ( -4507.11 / tempg ) 
               a1    = a1 * 24.5 *1.0E6  ! ppm
               cnh3s = a1 * gammas       
         
! Set a maximum [NH4]/[H+] ratio at 200,000 based on output from the AIM aerosol
! model any [NH4] in excess of this ratio is assumed to partition into the solid
! phase. Canada soil gamma taken from Zhang et al 2010 JGR Table 5
               If (Land_Scheme .EQ. 'USGS24') Then
                  Select Case( j )
                     Case(16,24) ! water, ice or snow
                        Cnh3g1 = 0.0
                        Cnh3g2 = 0.0
                     Case(2:3) ! Ag                
                        Cnh3g1 = Min(max(gamma1(Col,Row)/wg_ir,C_gam/wg_ir),2.0e5)
                        Cnh3g2 = Min(max(gamma2(Col,Row)/w5cm,C_gam/w5cm),2.0e5)                     
                     Case(4:5) ! Ag mosaic 67% ag 33% mosaic
                        Cnh3g1 = Min(max(gamma1(Col,Row)/wg_ir,C_gam/wg_ir)*0.67+0.33*20.0/wg_ir,2.0e5)
                        Cnh3g2 = Min(max(gamma2(Col,Row)/w5cm,C_gam/w5cm)*0.67+0.33*20.0/w5cm,2.0e5)
                     Case Default ! not ag
                        Cnh3g1 = Min(20.0/wg_ir,2.0e5)
                        Cnh3g2 = Min(20.0/w5cm,2.0e5)
                  End Select
               End If
               
               If (Land_Scheme .EQ. 'NLCD-MODIS') Then
                  Select Case( j )
                     Case(1:2,31,46,48:50) ! water, ice or snow
                        Cnh3g1 = 0.0
                        Cnh3g2 = 0.0
                     Case(19:20) ! U.S. Ag
!                        write(logdev,*) 'frac_ir',frac_ir(Col,Row) !job debugging                
!                        write(logdev,*) 'Gamma1',gamma1(Col,Row) !job debugging                
!                        write(logdev,*) 'wg_ir',wg_ir !job debugging                                        
                        Cnh3g1 = Min(gamma1(Col,Row)/wg_ir,2.0e5)
                        Cnh3g2 = Min(gamma2(Col,Row)/w5cm,2.0e5)
                     Case(43)   ! Canada Ag
                        Cnh3g1 = Min(C_gam/wg_ir,2.0e5)
                        Cnh3g2 = Min(C_gam/w5cm,2.0e5)
                     Case(45) ! Canada Ag mosaic 67% ag 33% mosaic
                        Cnh3g1 = Min(C_gam*0.67/wg_ir+0.33*20.0/wg_ir,2.0e5)
                        Cnh3g2 = Min(C_gam*0.67/w5cm+0.33*20.0/w5cm,2.0e5)
                     Case Default ! not ag
                        Cnh3g1 = Min(20.0/wg_ir,2.0e5)
                        Cnh3g2 = Min(20.0/w5cm,2.0e5)
                  End Select
               End If
               
               If (Land_Scheme .EQ. 'MODIS') Then
                  Select Case( j )
                     Case(15:17) ! water, ice or snow
                        Cnh3g1 = 0.0
                        Cnh3g2 = 0.0
                     Case(12) ! Ag                
                        Cnh3g1 = Min(Max(gamma1(Col,Row)/wg_ir,C_gam/wg_ir),2.0e5)
                        Cnh3g2 = Min(Max(gamma2(Col,Row)/w5cm,C_gam/w5cm),2.0e5)                     
                     Case(14)   !Ag Mosaic
                        Cnh3g1 = Min(max(gamma1(Col,Row)/wg_ir,C_gam/wg_ir)*0.67+0.33*20.0/wg_ir,2.0e5)
                        Cnh3g2 = Min(max(gamma2(Col,Row)/w5cm,C_gam/w5cm)*0.67+0.33*20.0/w5cm,2.0e5)
                     Case Default ! not ag
                        Cnh3g1 = Min(20.0/wg_ir,2.0e5)
                        Cnh3g2 = Min(20.0/w5cm,2.0e5)
                  End Select
               End If
               
               Cnh3g1 = a1 * MAX(cnh3g1,0.01)         
               Cnh3g2 = a1 * MAX(cnh3g2,0.01)

1002           Continue

! Cuticle resistance : rw = rwx * cnh3c + rwm
               If( laij(j, col, row) .GT. 0.0 ) Then
                  rwx   = rwetsfc*17.01*1000./24.5 ! in ppb/ cso2c 
                  rwmb  = rwm* MAX(1.0,0.5*(100.-rh_air)) + rbcj(j)                
                  ga    = 1.0 / (raj(j,col,row) + 0.5*rincj(j,col,row)) 
                  gsb   = 1.0 / (rstomj(j,sp,col,row) + rbcj(j))
                  gg1(j)= 1.0 / ( rbgj(j) + 0.5*rincj(j,col,row) + rsoil1)
                  gg2(j)= 1.0 / ( rbgj(j) + 0.5*rincj(j,col,row) + rsoil2)
                  gcw   = laij(j,col,row) / ( rbcj(j) + rwetsfc )
                  gt    = gsb + gg1(j) + gg2(j) + ga + deltaj(j,col,row) * gcw
                  qq    = ga * cnh3 + gsb * cnh3s + gg1(j)* cnh3g1 + gg2(j)* cnh3g2
                  aq    = rwx * gt
                  bq    = rwmb * gt + laij(j,col,row) * ( 1.0 - deltaj(j,col,row) ) - rwx * qq
                  cq    = -rwmb * qq
                  sqrr = ( bq * bq - 4.0 * aq * cq )
                  if(sqrr.ge.0.0) THEN
                     cnh3c  = ( -bq + ( sqrr )**0.5 ) / ( 2.0 * aq )
                  else
                     WRITE(logdev,*)' sqrr,bq,aq,cq=',sqrr,bq,aq,cq
                     WRITE(logdev,*)' laij,raj,deltaj,lufrac=',laij(j,col,row),raj(j,col,row),deltaj(j,col,row),j
                     cnh3c  = ( -bq ) / ( 2.0 * aq )
                  endif
                  if(cnh3c.lt.0.0) THEN
                     WRITE(logdev,*) ' j,cnh3c=', j,cnh3c
                     WRITE(logdev,*)' sqrr,bq,aq,cq=',sqrr,bq,aq,cq
                     WRITE(logdev,*)' laij,raj,deltaj,lufrac=',laij(j,col,row),raj(j,col,row),deltaj(j,col,row),j
                     WRITE(logdev,*)' rwx,gt,qq,cnh3',rwx,gt,qq,cnh3
                     WRITE(logdev,*)' gsb,gg1,gg2,ga,gcw',gsb,gg1(j),gg2(j),ga,gcw
                     WRITE(logdev,*)' rwmb, rbgj,rincj,rsoil1,rsoil2',rwmb, rbgj(j),rincj(j, col, row),rsoil1,rsoil2
                     WRITE(logdev,*)' rbcj(j), rstomj(j)',rbcj(j), rstomj(j, sp, col, row)
                     Write(logdev,*)' cnh3s, cnh3g1, cnh3g2', cnh3s, cnh3g1, cnh3g2
                     cnh3c = 0.0
                  endif
                        
                  ddd =  (1.0-deltaj(j,col,row)) / rgndcj(j,col,row) + deltaj(j,col,row) / rgw 

                  vdg = 1.0 / ( raj(j,col,row) + rbcj(j) + 1/ddd )
               EndIf ! laij > 0.0            
            
! integrate over the land uses
               Select Case(j) 
                  Case(1,31,48:50) ! no vegetation
                     lnh3  = lnh3 + lufrac(j,col,row)/(rsurfj(j,sp,col,row)+raj(j,col,row))
                  Case Default     ! vegetation            
                     pvd   = pvd + lufrac(j,col,row)*cnh3c * ga * vegj(j,col,row)
                     lnh3  = lnh3 + lufrac(j,col,row)*(( 1 - vegj(j,col,row) ) * vdg + vegj(j,col,row) * ( ga ))
               End Select            

!-------Compute Fluxes  and integrate over LU for output JOB -------------------
               Select Case(j)
                  Case(1,31,48) ! water
                     f_wat = lufrac(j,col,row)/(rsurfj(j,sp,col,row)+raj(j,col,row))*cnh3/watfrac + f_wat
                  Case(2,8,16,17,49:50) ! no veg
! Zero out the compensation points for deposition
                     f_dep  = lufrac(j,col,row)*(vegj(j,col,row)*ga*( cnh3 - (rwx * ga * cnh3 -rwmb * gt - laij(j,col,row) * 
     &                  ( 1.0 - deltaj(j,col,row) ) + ((rwx * ga * cnh3 -rwmb * gt - 
     &                  laij(j,col,row) * ( 1.0 - deltaj(j,col,row) ))**2 + 4.0 * aq * rwmb * ga * 
     &                  cnh3 )**0.5)/(2.0*aq)) + ( 1 - vegj(j,col,row) ) * vdg * cnh3)+f_dep
                  Case(19:20,43,45) ! agriculture
! Zero out the atmospheric NH3 concentration for emissions
                     f_emis = lufrac(j,col,row)*vegj(j,col,row)*ga*( rwx * (qq-ga*cnh3) -rwmb * gt - laij(j,col,row) * 
     &                  ( 1.0 - deltaj(j, col,row) ) + ((rwx * (qq-ga*cnh3) -rwmb * gt - 
     &                  laij(j,col,row) * ( 1.0 - deltaj(j,col,row) ))**2 + 4.0 * aq * rwmb * 
     &                  (qq-ga*cnh3))**0.5)/(2.0*aq)+f_emis
! Zero out the compensation points for deposition
                     f_dep  = lufrac(j,col,row)*(vegj(j,col,row)*ga*( cnh3 - (rwx * ga * cnh3 -rwmb * gt - laij(j,col,row) * 
     &                  ( 1.0 - deltaj(j,col,row) ) + ((rwx * ga * cnh3 -rwmb * gt - 
     &                  laij(j,col,row) * ( 1.0 - deltaj(j,col,row) ))**2 + 4.0 * aq * rwmb * ga * 
     &                  cnh3 )**0.5)/(2.0*aq)) + ( 1 - vegj(j,col,row) ) * vdg * cnh3)+f_dep
! Components of the flux for stomatal, cuticular and soil surfaces
                     f_stom = lufrac(j,col,row)*vegj(j,col,row) * (cnh3s - cnh3c) * gsb + f_stom! component 1 stomatal
                     f_cut  = lufrac(j,col,row)*(-cnh3c * vegj(j,col,row) * laij(j,col,row) * 
     &                  ( deltaj(j,col,row)/( rbcj(j)+rwetsfc) +    ! wet cuticle
     &                  (1-deltaj(j,col,row))/( rwmb + rwx*cnh3c ))) + f_cut ! dry cuticle
                     f_soil = lufrac(j,col,row)*(vegj(j,col,row)*((cnh3g1 - cnh3c) * gg1(j) +  ! soil layer 1
     &                  (cnh3g2 - cnh3c) * gg2(j))) + f_soil   ! soil layer 2                                                  
                     f_ag = f_ag + lufrac(j,col,row)*(vegj(j,col,row)*ga*(cnh3c-cnh3)-(1-vegj(j,col,row))*vdg*cnh3)/agfrac
                     if(f_emis .lt. 0.0) Then
                        Write(logdev,*)'f_ag  : ', f_ag
                        Write(logdev,*)'f_dep : ', f_dep
                        Write(logdev,*)'f_emis: ', f_emis
                        Write(logdev,*)'f_stom: ', f_stom
                        Write(logdev,*)'f_cut : ', f_cut
                        Write(logdev,*)'f_soil: ', f_soil
                        Write(logdev,*)'lufrac: ', lufrac(j,col,row)
                        Write(logdev,*)'vegj  : ', vegj(j,col,row)
                        Write(logdev,*)'ga    : ', ga
                        Write(logdev,*)'vdg   : ', vdg
                        Write(logdev,*)'agfrac: ', agfrac
                     End if
                  Case Default ! natural 
! Zero out the atmospheric NH3 concentration for emissions
                     f_emis = lufrac(j,col,row)*vegj(j,col,row)*ga*( rwx * (qq-ga*cnh3) -rwmb * gt - laij(j,col,row) * 
     &                  ( 1.0 - deltaj(j, col,row) ) + ((rwx * (qq-ga*cnh3) -rwmb * gt - 
     &                  laij(j,col,row) * ( 1.0 - deltaj(j,col,row) ))**2 + 4.0 * aq * rwmb * 
     &                  (qq-ga*cnh3))**0.5)/(2.0*aq)+f_emis
! Zero out the compensation points for deposition
                     f_dep  = lufrac(j,col,row)*(vegj(j,col,row)*ga*( cnh3 - (rwx * ga * cnh3 -rwmb * gt - laij(j,col,row) * 
     &                  ( 1.0 - deltaj(j,col,row) ) + ((rwx * ga * cnh3 -rwmb * gt - 
     &                  laij(j,col,row) * ( 1.0 - deltaj(j,col,row) ))**2 + 4.0 * aq * rwmb * ga * 
     &                  cnh3 )**0.5)/(2.0*aq)) + ( 1 - vegj(j,col,row) ) * vdg * cnh3)+f_dep
! Components of the flux for stomatal, cuticular and soil surfaces
                     f_stom = lufrac(j,col,row)*vegj(j,col,row) * (cnh3s - cnh3c) * gsb + f_stom! component 1 stomatal
                     f_cut  = lufrac(j,col,row)*(-cnh3c * vegj(j,col,row) * laij(j,col,row) * 
     &                  ( deltaj(j,col,row)/( rbcj(j)+rwetsfc) +    ! wet cuticle
     &                  (1-deltaj(j,col,row))/( rwmb + rwx*cnh3c ))) + f_cut ! dry cuticle
                     f_soil = lufrac(j,col,row)*(vegj(j,col,row)*((cnh3g1 - cnh3c) * gg1(j) +  ! soil layer 1
     &                  (cnh3g2 - cnh3c) * gg2(j))) + f_soil   ! soil layer 2                        
                     f_nat = f_nat + lufrac(j,col,row)*(vegj(j,col,row)*ga*(cnh3c-cnh3)-(1-vegj(j,col,row))*vdg*cnh3)/(1-agfrac-watfrac)
                     If(f_emis .lt. 0.0) Then
                        Write(logdev,*)'f_nat : ', f_nat
                        Write(logdev,*)'f_dep : ', f_dep
                        Write(logdev,*)'f_emis: ', f_emis
                        Write(logdev,*)'f_stom: ', f_stom
                        Write(logdev,*)'f_cut : ', f_cut
                        Write(logdev,*)'f_soil: ', f_soil
                        Write(logdev,*)'lufrac: ', lufrac(j,col,row)
                        Write(logdev,*)'vegj  : ', vegj(j,col,row)
                        Write(logdev,*)'ga    : ', ga
                        Write(logdev,*)'vdg   : ', vdg
                        Write(logdev,*)'agfrac: ', agfrac
                    End If
               End Select
            End If ! Lufrac > 0.0
         End Do ! n_lufrac         
         
!------Compute nitrification rates for agricultural areas only where there is NHx and H+   
         If ( gamma1(Col,Row) .GT. 0.0 .AND. MHp1(Col,Row) .GT. 1.0e-9 .AND.
     &        MHp1(Col,Row) .LT. 1.0e-5 ) Then  
            Call Calc_Nitrif(Tempg,wg_ir,Cnh3c,gg1,gamma1(Col,Row),ist,dt, MHp1(Col,Row),Col,Row)
         EndIf
         If ( gamma2(Col,Row) .GT. 0.0 .AND. MHp2(Col,Row) .GT. 1.0e-9 .AND.
     &        MHp2(Col,Row) .LT. 1.0e-5 ) Then  
            Call Calc_Nitrif(Tempg,w5cm,Cnh3c,gg2,gamma2(Col,Row),ist,dt,MHp2(Col,Row),Col,Row)
         EndIf
                  
         Return         
         End Subroutine Get_Flux
!------------------------------------------------------------------------------
! Subroutine to update the soil ammonia and pH due to evasion, deposition, 
! nitrification, leaching, and run off
! Soil ammonium, pH, leaching, and run off are defined in the module
!------------------------------------------------------------------------------
         Subroutine Calc_Nitrif(Tempg,wg,CNH3c,gg, gam, ist, dt, MHp,C,R)  
         
         Use UTILIO_DEFN
         Use Bidi_Mod, Only: lufrac
         Use LSM_MOD
         
         Implicit None                
         
         Real,          Intent( IN )    :: Tempg     ! Soil Temperature
         Real,          Intent( IN )    :: wg        ! Soil Moisture
         Real,          Intent( IN )    :: CNH3c     ! Canopy compensation point
         Real,          Intent( IN )    :: MHp       ! molar H concentration
         Real(KIND=16), Intent( IN )    :: gg(n_lufrac) ! soil-air conductance        
         Real,          Intent( INOUT ) :: gam        ! updated gamma
         Integer,       Intent( IN )    :: ist    ! Soil Type
         Integer,       Intent( IN )    :: dt     ! time step in s
         Integer,       Intent( IN )    :: C      ! Column
         Integer,       Intent( IN )    :: R      ! Row
                 
         
         Real    :: fTg    ! nitrification temperature factor
         Real    :: fSWg   ! nitrification Soil Water factor
         Real    :: fpHg   ! nitrification pH factor 
         Real    :: Kn     ! nitrification rate 1/s
         Real    :: Kvs    ! Air-soil exchange rate for aqueous NH4+
         Real    :: kvg    ! Air-soil exchange rate for gas phase NH3
         Real    :: MNH3   ! molar atmospheric NH3 conc.
         Real    :: MNHx   ! molar soil water NH4+ + NH3 conc.
         Real    :: wg25   ! wilting point +0.25*(field capacity-wilting point)
         Real    :: pHsl   ! Soil pH
         Real    :: ag_tot ! total 
         Integer :: i   ! loop counter
         CHARACTER( 96 )            :: xmsg = ' '
         Character( 16 ), Parameter :: pname = 'Calc_Nitrif' 
         
! convert the ambient concentrations from ppm to mol/l
         MNH3 = CNH3c/24.45/1.0e6
         
! Get the soil pH         
         pHsl = -log10(MHp)
         
! get NH4+ from gamma ([NH4+]/[H+] with units in mol/l)         
         MNHx = min(gam/wg,2.0e5)*MHp

! Estimate the soil nitrification rate following Reddy et al 1979 starting with
! the temperature parameter         
         fTg = max(0.041*(Tempg-278.15),0.0)
! Estimate the soil water parameter         
         wg25 = wwlt(ist)+0.25*(wfc(ist)-wwlt(ist))
         If(wg .LE. wwlt(ist)) fSWg = 0.1
         If(wg .LT. wg25 .AND. wg .GT. wwlt(ist)) Then
            fSWg = max(0.1,0.1+0.9*((wg-wwlt(ist))/(wfc(ist)-wwlt(ist)))**0.5,
     &                 (wg-wwlt(ist))/(0.25*(wfc(ist)-wwlt(ist))))
         EndIf
         If(wg .GE. wg25 .AND. wg .LT. wfc(ist)) fSWg = 1
         
         If(wg .GE. wfc(ist)) Then 
            fSWg = max(0.1,1.0-(wg-wfc(ist))/(wsat(ist)-wfc(ist)))
         EndIf
! Estimate the pH factor
         If(pHsl .LT. 7.0) fpHg        = 0.307*pHsl-1.269
         If(pHsl .GE. 7.0 .AND. pHsl .LT. 7.4 ) fpHg = 1 
         If(pHsl .GE. 7.4) fpHg = 5.367-0.599
                  
         Kn = fTg*fSWg*fpHg/24/3600 !Kn is in 1/d in EPIC 
! Estimate the soil evasion rate         
         kvg    = 0.0
         Kvs    = 0.0
         ag_tot = 0.0
         Do i = 1, n_lufrac
            If(lufrac(i,C,R) .GT. 0.0) Then
               Select Case(i)
                  Case(19:20)
                     kvg    = kvg + gg(i)*lufrac(i,C,R)
                     Kvs    = Kvs + (161512.0 / tempg * 10.0 ** ( -4507.11 / tempg))*gg(i)/MHp*lufrac(i,C,R)
                     ag_tot = ag_tot + lufrac(i, C, R)                             
               End Select               
            End If
         End Do
         If(ag_tot .GT. 0.0 ) Then
            kvg = kvg/ag_tot
            Kvs = Kvs/ag_tot        
         
                     
            MNHx = kvg*MNH3/(Kvs+Kn)+(MNHx-kvg*MNH3/(Kvs+Kn))*exp(-(Kvs+Kn)*dt)         
         
            gam = MNHx/MHp*wg + max(gam/wg-2.0e5,0.0)*wg
         Else
            gam = 20.0
         End If            
         
         If( IsNaN(gam) ) Then
            XMSG = 'NaN in Gamma Calculation'
            Write(logdev,*) 'MNHx   :', MNHx
            Write(logdev,*) 'gam    :', gam
            Write(logdev,*) 'MHp    :', MHp
            Write(logdev,*) 'ag_tot :', ag_tot
            Call M3EXIT( PName, 0, 0,XMSG, XSTAT1)   
         EndIf
         
         Return
         End Subroutine Calc_Nitrif
         
      End Module ABFlux_Mod
