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
!  but users requested to provide copies of derivative works or          !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module STAGE_DATA
C-----------------------------------------------------------------------
C Function: User-defined types

C Revision History:
C     26 June 2014 J.Bash: initial implementation

      Implicit None

      Real,    Parameter, Private :: pr         = 0.709      ! [dim'less]
      Real,    Parameter, Private :: twothirds  = 2.0 / 3.0
      
      INTEGER,           Save            :: dep_gas_all 
      Character( 16 ), Allocatable, Save :: vd_name     ( : )       ! Deposition species name
      Real, Allocatable, Save            :: rel_rx      ( : )       ! reactivity relative to HNO3
      Real, Allocatable, Save            :: LeBasM      ( : )       ! Le Bas molar volume [cm3/mol ]
      Real, Allocatable, Save            :: f0          ( : )       ! Exception for species that
      Real, Allocatable, Save            :: M_ac        ( : )       ! Mass accommodation Coeff
      Real, Allocatable, Save            :: molwt_all   ( : )       ! Molar Weight from the namelist
      CHARACTER( 16 ), Allocatable, SAVE :: H_name_all  ( : )       ! Henry's constant name for all species        
      LOGICAL, Allocatable, SAVE         :: USE_DEPSPC  ( : )
      Logical, Allocatable, Save         :: asx_run_map ( : )

      INTEGER, SAVE                      :: N_USE_DEPSPC            ! number of gas species used in this simulation 
      CHARACTER( 16 ), Parameter         :: DEPV_METHOD = 'STAGE'

      Contains
         SUBROUTINE Map_STAGE( N_DEPV_NAMES, DEPV_NAMES, DEPV_SUR ) 

         USE CGRID_SPCS          ! CGRID mechanism species
         USE UTILIO_DEFN 
         USE RUNTIME_VARS, ONLY: STAGECTRL, LOGDEV!, log_message, log_subheading

         IMPLICIT NONE

         INTEGER,         INTENT( IN )    :: N_DEPV_NAMES  ! number of unique dep vel species
         CHARACTER( 16 ), INTENT( INOUT ) :: DEPV_NAMES( : ) ! names of dep vel species
         INTEGER,         INTENT( OUT )   :: DEPV_SUR( : ) ! pointers to model species for
                                                        ! calculated deposition velocities
                                                        ! (surrogate dep vels [m/s**2])

         Integer                       :: c, r, l, n, s, v
         INTEGER                       :: STRT, FINI
         Integer                       :: ALLOCSTAT
         INTEGER                       :: DEPCTRL_NML
         INTEGER                       :: STAT
         CHARACTER( 16 ), ALLOCATABLE  :: gc_depv_name( : )
         CHARACTER( 16 )               :: VNAME            ! scratch string
         CHARACTER( 16 )               :: PNAME = 'Map_STAGE'
         CHARACTER( 120 )              :: XMSG = ' '
         Logical                       :: n_xref_end

         Integer, Parameter            :: n_max_depv = 300

         Type :: Chem_Data_Type
            Character( 16 ) :: vd_name 
            Real            :: rel_rx           ! reactivity relative to HNO3
            Real            :: f0               ! Exception for species that
            Real            :: LeBasM           ! Le Bas molar volume [cm3/mol ]
            Real            :: M_ac             ! Mass accommodation Coeff 
         End Type Chem_Data_Type 
 
         Type( Chem_Data_Type ) :: STAGE_CHEM_DATA( n_max_depv )

         NAMELIST / STAGE_DATA / STAGE_CHEM_DATA

         STAGE_CHEM_DATA%vd_name = 'N/A'
         STAGE_CHEM_DATA%rel_rx  = 0.0
         STAGE_CHEM_DATA%f0      = 0.0
         STAGE_CHEM_DATA%LeBasM  = 0.0
         STAGE_CHEM_DATA%M_ac    = 0.0

      ! Retrieve the Name of the Deposition Control File
         IF ( STAGECTRL .EQ. "STAGECTRL_NML" ) THEN
            WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A)"),
     &        'You have chosen not to indicate the location of an',
     &        'STAGE deposition Control namelist file. Default ',
     &        'settings will be assumed.'
            RETURN
         END IF

      ! Open Deposition Control Namelist File
         DEPCTRL_NML = JUNIT()
         OPEN( FILE = STAGECTRL, UNIT = DEPCTRL_NML, STATUS = 'OLD',
     &         POSITION = 'REWIND', FORM='FORMATTED', IOSTAT = STAT )

      ! Check for Error in File Open Process
         IF ( STAT .NE. 0 ) THEN
            WRITE( XMSG, '(A,A,A)' ),'ERROR: Could not read ',
     &              'STAGE control namelist file: ',TRIM( STAGECTRL )
            CALL M3EXIT( 'STAGE_LU_MAPPER',0,0,XMSG,1 )
         END IF

         REWIND( DEPCTRL_NML )
         READ( NML = STAGE_DATA, UNIT = DEPCTRL_NML, IOSTAT=STAT )
         IF ( STAT .NE. 0 ) THEN
            WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &        'Warning! Something went wrong while reading the ',
     &        'STAGE land use section of the STAGE Deposition',
     &        'Control Namelist. Default values for this section ',
     &        'will be assumed.'
         END IF

         dep_gas_all = 1
         n_xref_end  = .TRUE.
         Do While( n_xref_end )
            If( STAGE_CHEM_DATA(dep_gas_all)%vd_name .Eq. 'N/A' ) Then
               n_xref_end = .FALSE.
               dep_gas_all = dep_gas_all - 1
            Else
               dep_gas_all = dep_gas_all + 1
            End If
         End Do
         
! Allocate Chem Data
         ALLOCATE ( vd_name     ( dep_gas_all ),
     &              rel_rx      ( dep_gas_all ), 
     &              LeBasM      ( dep_gas_all ),
     &              f0          ( dep_gas_all ),
     &              M_ac        ( dep_gas_all ),
     &              molwt_all   ( dep_gas_all ),
     &              H_name_all  ( dep_gas_all ),
     &              USE_DEPSPC  ( dep_gas_all ),
     &              asx_run_map ( dep_gas_all ),
     &              gc_depv_name( dep_gas_all ),STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating chemical deposition parameters '
            Call M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
         End If
          
! load arrays from STAGE namelist data
         vd_name = STAGE_CHEM_DATA(1:dep_gas_all)%vd_name
         rel_rx  = STAGE_CHEM_DATA(1:dep_gas_all)%rel_rx 
         f0      = STAGE_CHEM_DATA(1:dep_gas_all)%f0
         LeBasM  = STAGE_CHEM_DATA(1:dep_gas_all)%LeBasM
         M_ac    = STAGE_CHEM_DATA(1:dep_gas_all)%M_ac

! Initialize arrays to be populated by MECH namelist data
         molwt_all    = 0.0
         asx_run_map  = .FALSE.
         gc_depv_name = 'None'
         H_name_all   = 'None'
         
         Do v = 1, N_GC_DEPV
            s = index1( gc_depv( v ),dep_gas_all,'VD_'//vd_name ) 
            If( s .Gt. 0.0 ) Then
               molwt_all( s ) = gc_molwt( gc_depv_map( v ) )
               gc_depv_name( s ) = gc_spc( gc_depv_map( v ) )
               asx_run_map( s ) = .TRUE.
            End If
         End Do 
         Do v = 1, N_GC_SCAV
            s = index1(gc_spc(gc_scav_map(v)),dep_gas_all,gc_depv_name)
            if( s .Gt. 0.0 ) Then
               H_name_all( s ) = gc_scav(v)
            End if
         End Do
         Do v = 1, N_NR_DEPV
            s = index1( nr_depv( v ),dep_gas_all,'VD_'//vd_name ) 
            If( s .Gt. 0.0 ) Then
               molwt_all( s ) = nr_molwt( nr_depv_map( v ) )
               gc_depv_name( s ) = nr_spc( nr_depv_map( v ) )
               asx_run_map( s ) = .TRUE.
            End If
         End Do 
         Do v = 1, N_NR_SCAV
            s = index1(nr_spc(nr_scav_map(v)),dep_gas_all,gc_depv_name)
            if( s .Gt. 0.0 ) Then
               H_name_all( s ) = nr_scav(v)
            End if
         End Do
         Do v = 1, N_TR_DEPV
            s = index1( tr_depv( v ),dep_gas_all,'VD_'//vd_name ) 
            If( s .Gt. 0.0 ) Then
               molwt_all( s ) = tr_molwt( tr_depv_map( v ) )
               gc_depv_name( s ) = tr_spc( tr_depv_map( v ) )
               asx_run_map( s ) = .TRUE.
            End If
         End Do 
         Do v = 1, N_TR_SCAV
            s = index1(tr_spc(tr_scav_map(v)),dep_gas_all,gc_depv_name)
            if( s .Gt. 0.0 ) Then
               H_name_all( s ) = tr_scav(v)
            End if
         End Do

! Map the deposition surrogate species 
         IF ( ( N_GC_SPC + N_NR_SPC + N_TR_SPC ) .LE. 0 ) THEN
            XMSG = 'Model not compiled for gases'
            CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF

C Set up names and indices
         N_USE_DEPSPC = 0
         USE_DEPSPC = .FALSE.   ! array

C Set up the DEPV pointers to calculated deposition velocities

         DO V = 1, N_DEPV_NAMES
            VNAME = DEPV_NAMES( V )( 4:LEN_TRIM( DEPV_NAMES( V ) ) )
            N = INDEX1( VNAME, dep_gas_all, vd_name )
            IF ( N .NE. 0 ) THEN
               IF ( .NOT. USE_DEPSPC( N ) ) THEN
                  USE_DEPSPC( N ) = .TRUE.
                  N_USE_DEPSPC = N_USE_DEPSPC + 1
               END IF    
            ELSE
               XMSG = 'Could not find ' // TRIM( DEPV_NAMES( V ) ) // 
     &                ' in '            // TRIM( DEPV_METHOD )     //
     &                ' surrogate table.'
               CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
            END IF
         END DO

C Sort DEPV_NAMES based on the order in DEPSPC (DEPVVARS module)

         N = 0
         DO V = 1, dep_gas_all           
            IF ( USE_DEPSPC( V ) ) THEN
               N = N + 1
               VNAME = 'VD_' // vd_name( V )( 1:LEN_TRIM( vd_name( V ) ) )
               DEPV_NAMES( N ) = VNAME
            END IF
         END DO

C Locate model species' DEPVs in the unique DEPV_NAMES list

         DO V = 1, N_GC_DEPV
            VNAME = GC_DEPV( V )
            N = INDEX1( VNAME, N_DEPV_NAMES, DEPV_NAMES )
            IF ( N .NE. 0 ) THEN
               DEPV_SUR( V ) = N
            ELSE
               XMSG = 'Could not find ' // TRIM( GC_DEPV( V ) )
     &              // ' in STAGE Deposition Namelist.'  
               CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
            END IF
         END DO

         STRT = N_GC_DEPV + 1
         FINI = N_GC_DEPV + N_NR_DEPV 
         DO V = STRT, FINI
            VNAME = NR_DEPV( V - ( STRT-1 ) )
            N = INDEX1( VNAME, N_DEPV_NAMES, DEPV_NAMES )
            IF ( N .NE. 0 ) THEN
               DEPV_SUR( V ) = N
            ELSE
               XMSG = 'Could not find ' // TRIM( NR_DEPV( V ) )
     &              // ' in STAGE Deposition Namelist.'  
               CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
            END IF
         END DO

         STRT = N_GC_DEPV + N_NR_DEPV + 1
         FINI = N_GC_DEPV + N_NR_DEPV + N_TR_DEPV
         DO V = STRT, FINI
            VNAME = TR_DEPV( V - (STRT-1) )
            N = INDEX1( VNAME, N_DEPV_NAMES, DEPV_NAMES )
            IF ( N .NE. 0 ) THEN
               DEPV_SUR( V ) = N
            ELSE
               XMSG = 'Could not find ' // TRIM( TR_DEPV( V ) )
     &              // ' in STAGE Deposition Namelist.'  
               CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
            END IF
         END DO

         Write(Logdev,*) '*********************** STAGE Deposition Map ***********************'
         Write(Logdev,99901) 'Dep. Species', 'H Surrogate', 'Mol Weight'

         Do v = 1, dep_gas_all
           If( asx_run_map (v) ) Write(Logdev,99902) gc_depv_name( v ), H_name_all(v), molwt_all(v)
         End Do
       
         Return

99901    Format(A16,1x,A16,1x,A16)
99902    Format(A16,1x,A16,10x,F6.2)

         End Subroutine Map_Stage

      End Module Stage_Data
