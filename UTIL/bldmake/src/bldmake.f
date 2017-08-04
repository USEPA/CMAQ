!-----------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in    !
!  continuous development by various groups and is based on information !
!  from these groups: Federal Government employees, contractors working !
!  within a United States Government contract, and non-Federal sources  !
!  including research institutions.  These groups give the Government   !
!  permission to use, prepare derivative works of, and distribute copies!
!  of their work in the CMAQ system to the public and to permit others  !
!  to Do so.  The United States Environmental Protection Agency         !
!  therefore grants similar permission to use the CMAQ system software, !
!  but users are requested to provide copies of derivative works or     !
!  products designed to operate in the CMAQ system to the United States !
!  Government without restrictions as to use by others.  Software       !
!  that is used with the CMAQ system but distributed under the GNU      !
!  General Public License or the GNU Lesser General Public License is   !
!  subject to their copyright restrictions.                             !
!-----------------------------------------------------------------------!

!-------------------------------------------------------------------------------
!     PROGRAM bldmake
!     Generate a Makefile for source files extracted from a git repository
!     originally written in C by Steve Thorpe
!     rewritten in Fortran by Steve Howard (for a CVS repository)
!     redone to meet CMAQ coding standards by Jeff Young (Nov 2012)
!     Sep 2013 J.Young: Increased local variables "modName" and "modFile" name
!                       lengths from 32 to 64 chars
!     Dec 2013 D.Wong: Expanded functionality to handle twoway model
!     Sep 2014 D.Wong: Setup . as the path that contains *.EXT files
!     Aug 2015 D.Wong: Modified subroutine writeOBJS to put STENEX and PARIO
!                      in front of GLOBAL_MODULES in the OBJS sequence. In
!                      this arrangement, all routines in STENEX will stay under
!                      STENEX and all routines in PARIO will stay under PARIO
!                      rather than GLOBAL_MODULES
!     Oct 2015 J.Young: Rework to make macros in the makefile for libs and
!                       compiler "I" references; get rid of the CVS option.
!     Jan 2016 D.Wong: Fixed the include path of mpif.h
!-------------------------------------------------------------------------------

      Program bldmake

      Use ModelCFG

      Implicit None

      Character( EXT_LEN ) :: cfgFile   ! config file
      Integer :: lfn = 11               ! config file unit
      Integer status
      Integer n

! call Setup routine to process command line arguments
      Call setup( cfgFile )

! open cfgFile
      Open ( unit=lfn, file=cfgFile, status='old', iostat=status )
      If ( status .Ne. 0 ) Then
        Write( *,'(" Open error number:",i5)') status
        Call error_msg( 'Cannot open FILE [' // Trim(cfgFile) //']' )
      End If

! read CFG file
      Call readCFG( lfn )
      Close ( unit=lfn )

! GIT repository
      Call git_export( status )

! create Makefile
      Open ( unit=lfn, file='Makefile', iostat=status )
      If ( status .ne. 0 ) Call error_msg( 'Cannot create FILE [Makefile]' )

      Call makefile( lfn, cfgFile )

      Close (unit=lfn)

! If not makefile only (makefo), Then run the make command to compile
      If ( .not. makefo ) Then
        Call RunMake( status )
      End If

      Stop
      End Program bldmake

!-------------------------------------------------------------------------------
!     Setup routine:  gets input file and run options from command line
!-------------------------------------------------------------------------------
      Subroutine setup( cfgFile )

      Use ModelCfg

      Implicit None

! arguments
      Character( * ) :: cfgFile

! functions
      Integer :: IARGC

! local variables
      Integer :: status
      Integer :: nargs
      Integer :: n
      Character( NAME_LEN ) :: argv

! date and time variables
      Character( 8 )  :: cdate
      Character( 10 ) :: ctime
      Character( 5 )  :: czone
      Integer         :: dateValues( 8 )

! set defaults
      verbose   = .False.
      serial    = .False.
      debug     = .False.
      checkout  = .False.
      makefo    = .False.
      twoway    = .False.
      git_local = .False.
      repo      = ' '
      reporoot  = ' '

! check number of arguments on command line
      nargs = IARGC()  ! non-standard compiler extension returns the number of arguments
      If ( nargs .Eq. 0 ) Then
        Call help_msg('No arguments on command line')
        Stop
      End If

! get last argument (the bldit-created config file) and write to cfgFile
      Call GETARG( nargs, cfgFile )  ! non-standard compiler extension returns argument
                                     ! specified - the last argumet in this case
      If ( cfgFile( 1:1 ) .Eq. '-' ) Then   ! config file not the last argument
        Call ucase( cfgFile )

        If ( cfgFile .Eq. '-HELP' ) Then
          Call help_msg(' ')
          Call cfgHelp( ); Stop
        End If

        Call help_msg('Invalid configuration file argument:' // Trim( cfgFile ) )
        Stop
      End If

 !    write( *,* ) "cfg: ", cfgfile
 !    write( *,* ) "nargs: ", nargs

! check for run options
      Do n = 1, nargs-1
        Call GETARG( n, argv )

 !      write( *,* ) "n, argv: ", n, argv

        If ( argv( 1:1 ) .Ne. '-' ) Then
          Call help_msg('Invalid arguments on command line:' // Trim( argv ) )
          Stop
        End If

        Call ucase( argv )

        If ( argv .Eq. '-HELP' ) Then
          Call help_msg('Help option:' // Trim( argv ) )
          Call cfgHelp( ); Stop
        End If

        If ( argv .Eq. '-MAKEFO' ) Then     ! Make file only
          makefo = .True.; Cycle
        End If

        If ( argv .Eq. '-SERIAL' ) Then     ! compile for serial execution
          serial = .True.; Cycle
        End If

        If ( argv .Eq. '-TWOWAY' ) Then     ! WRF-CMAQ twoway CCTM
           If ( .Not. serial ) Then
              twoway = .True.; Cycle
           Else
              stop ' TwoWay not available for Serial execution'
           End If
        End If

        If ( argv .Eq. '-GIT_LOCAL' ) Then  ! do not copy source files to BLD directory
          git_local = .True.; Cycle
        End If

        If ( argv .Eq. '-DEBUG' ) Then
          debug = .True.; Cycle
        End If

        If ( argv .Eq. '-VERBOSE' ) Then
          verbose = .True.; Cycle
        End If

        Call help_msg( 'Invalid arguments [' // Trim(argv) // '] on command line' )

      End Do

! If REPOROOT is defined, use it, Else set to current directory
      Call GETENV( 'REPOROOT', reporoot )
      If ( Len_Trim( reporoot ) .Eq. 0 ) Then
        Call PWD( reporoot, status )
        If ( status.ne.0 ) reporoot = './'
      End If
      If ( debug .or. verbose ) Then
        Write( *,'(''REPOROOT set to:'',a,/)') Trim( reporoot )
      End If

! Get system date and time
      Call date_and_time( cdate, ctime, czone, dateValues )
      Write( currentDate, '(i2.2,"/",i2.2,"/",2i4.2,":",i2.2,":",i2.2)' )
     &   dateValues( 2 ), dateValues( 3 ), dateValues( 1 ),
     &   dateValues( 5 ), dateValues( 6 ), dateValues( 7 )

      Return
      End Subroutine setup

!-------------------------------------------------------------------------------
!     Help message:  Prints command line format and options and stops run
!-------------------------------------------------------------------------------
      Subroutine help_msg( msg )

      Implicit None

! arguments
      Character(*) msg

      Write( *,'(/,a)' ) Trim( msg )

      Write( *,'(/"Usage: bldmake [-<option>...] filename")' )

      Write( *,'(/"where <option> is one of the following:")' )
      Write( *,'("  -verbose   Echo actions")' )
      Write( *,'("  -debug     Echo all actions")' )
      Write( *,'("  -serial    Make for serial execution")' )
      Write( *,'("  -makefo    Creates Makefile without building")' )
      Write( *,'("  -git_local Does NOT copy source files to BLD directory")' )
      Write( *,'("  -help      Displays help screen")' )
      Write( *,'(//)' )

      End Subroutine help_msg

!-------------------------------------------------------------------------------
!     Error:  Prints error string and stops run
!-------------------------------------------------------------------------------
      Subroutine error_msg( msg )

      Implicit None

! arguments
      Character( * ) :: msg

      Write( *,'(/"*** Program terminated on Error ***"/)' )
      Write( *,'(5x,a//)' ) Trim( msg )

      Stop
      End Subroutine error_msg

!-------------------------------------------------------------------------------
!     Makefile routine: creates Makefile from CFG data
!-------------------------------------------------------------------------------
      Subroutine makefile( lfn, cfgFile )

      Use ModelCfg

      Implicit None

! arguments
      Integer lfn
      Character( * ) cfgFile

      If ( verbose ) Then
        Write( *,'(/"Generating Makefile"/)' )
      End If

! print header lines
      If ( serial ) Then
         Write( lfn, '("#   Makefile generated for serial execution using program bldmake")' )
      Else
         Write( lfn, '("#   Makefile generated for parallel execution using program bldmake")' )
      End If
      Write( lfn, '("#")' )
      Write( lfn, '("#   Generation date [",a,"]")' )      Trim( currentDate )
      Write( lfn, '("#   Configuration file [",a,"]")' )   Trim( cfgFile )
      Write( lfn, '("#   Using GIT repository [",a,"]")' ) Trim( repo )
      If ( Trim( mechanism ) .Ne. 'X' )
     &   Write( lfn, '("#   With mechanism [",a,"]")' )    Trim( mechanism )

      ! Document Explicit Compiler Execution Paths
      Write( lfn, '("#   Full Compiler Paths when Makefile was Built:")' ) 
      Write( lfn, '("#       FC = ",a)' ) Trim( f_compiler_path )
      Write( lfn, '("#       CC = ",a)' ) Trim( c_compiler_path )

      ! Document Explicit Library Paths
      Call GETENV( 'IOAPI_MOD_DIR',  ioapi_mod_dir )
      Call GETENV( 'IOAPI_INCL_DIR', ioapi_incl_dir )
      Call GETENV( 'IOAPI_LIB_DIR',  ioapi_lib_dir )
      Call GETENV( 'NETCDF_LIB_DIR', netcdf_lib_dir )
      Call GETENV( 'MPI_LIB_DIR',    mpi_lib_dir )
      Write( lfn, '("#   Library Paths:")' ) 
      Write( lfn, '("#      $(LIB)/ioapi/modules -> ",a)' ) Trim( ioapi_mod_dir )
      Write( lfn, '("#      $(LIB)/ioapi/include_files -> ",a)' ) Trim( ioapi_incl_dir )
      Write( lfn, '("#      $(LIB)/ioapi/lib -> ",a)' ) Trim( ioapi_lib_dir )
      Write( lfn, '("#      $(LIB)/mpi -> ",a)' ) Trim( mpi_lib_dir )
      Write( lfn, '("#      $(LIB)/netcdf -> ",a)' ) Trim( netcdf_lib_dir )

      ! Begin Makefile Commands
      Write( lfn, '(/" EXEC = ",a)' ) Trim( model )

      Write( lfn, '(/" FC = ",a)' ) Trim( f_compiler )
      Write( lfn, '( " CC = ",a)' ) Trim( c_compiler )

      Write( lfn, '(/" LIB = ",a)' ) Trim( lib_base )
      Write( lfn, '( " include_path = -I $(LIB)/",a,1x,a)' ) Trim( lib_1 ), backslash 
      If ( l_lib_3 ) Then
         Write( lfn, '( "                -I $(LIB)/",a,1x,a)' ) Trim( lib_2 ), backslash 
         Write( lfn, '( "                -I $(LIB)/",a)' )      Trim( lib_3 )
      Else
         Write( lfn, '( "                -I $(LIB)/",a,1x,a)' ) Trim( lib_2 )
      End If

      Write( lfn, '(/" WARN = ")' )
      Write( lfn, '( " FSTD = ",a)' ) Trim( fstd )
      Write( lfn, '( " DBG  = ",a)' ) Trim( dbg )

      Write( lfn, '(/" f_FLAGS   = ",a)' ) Trim( f_flags ) // " $(FSTD) $(include_path)"
      Write( lfn, '( " f90_FLAGS = ",a)' ) Trim( f90_flags ) // " $(FSTD) $(include_path)"

      Write( lfn, '( "#f_FLAGS   = ",a)' ) Trim( f_flags ) // " $(DBG) $(include_path)"
      Write( lfn, '( "#f90_FLAGS = ",a)' ) Trim( f90_flags ) // " $(DBG) $(include_path)"

      Write( lfn, '(/" F_FLAGS   = $(f_FLAGS)")' )
      Write( lfn, '( " F90_FLAGS = $(f90_FLAGS)")' )

      If ( serial ) Then
         Write( lfn, '( " C_FLAGS   = ",a)' ) Trim( c_flags ) // "-I."
      Else
         Write( lfn, '( " C_FLAGS   = ",a)' ) Trim( c_flags ) // "$(LIB)/mpi/include -I."
      End If

      If ( verbose ) Then
        Write( *, '("  Compilers defined")' )
      End If

      Write( lfn, '(/" LINKER     = ",a)' ) Trim( linker )
      Write( lfn, '( " LINK_FLAGS = ",a)' ) Trim( link_flags )

      If ( Len_Trim( reporoot ) .Gt. 0 ) Then
        Write( lfn, '(/" REPOROOT = ",a)' ) Trim( reporoot )
      End If

      If ( Len_Trim( VPATH ) .Gt. 0 ) Then
        Call writeVPATH( lfn )
      End If

      Call writeCPP( lfn )
      If ( verbose ) Then
        Write( *, '("  CPP Flags defined")' )
      End If

      Write( lfn, '(/" IOAPI  = -L$(LIB)/",a,1x,a)' ) Trim( lib_4 ), Trim( ioapi )
      Write( lfn, '( " NETCDF = -L$(LIB)/",a,1x,a)' ) "netcdf/lib", Trim( netcdf )
      If ( serial ) Then
         Write( lfn, '( " LIBRARIES = $(IOAPI) $(NETCDF)")' )
      Else
!         Write( lfn, '( " MPICH  = -L$(LIB)/",a,1x,a)' ) "mpich/lib", Trim( mpich )
         Write( lfn, '( " MPICH  = -L$(LIB)/",a,1x,a)' ) "mpi/lib", Trim( mpich )
         Write( lfn, '( " LIBRARIES = $(IOAPI) $(NETCDF) $(MPICH)")' )
      End If

!     Call writeLIB( lfn )
      If ( verbose ) Then
        Write( *, '("  Libraries defined")' )
      End If

      Call writeINC( lfn )
      If ( verbose ) Then
        Write( *, '("  Includes defined")' )
      End If

      Call writeOBJS( lfn )
      If ( verbose ) Then
        Write( *, '("  Objects defined")' )
      End If

      Call writeRules( lfn )
      If ( verbose ) Then
        Write( *, '("  Make rules defined")' )
      End If

      Call writeDEP( lfn )
      If ( verbose ) Then
        Write( *, '("  USE/MODULE dependencies defined")' )
      End If

      Write( *, '(/"Makefile generated")' )

      Return
      End Subroutine makefile

!-------------------------------------------------------------------------------
!     WriteVPATH routine:  Writes each directory on its own line
!-------------------------------------------------------------------------------
      Subroutine writeVPATH( lfn )

      Use ModelCFG

      Implicit None

! arguments
      Integer lfn

! functions
      Integer getFieldCount

! local variables
      Integer nfields
      Integer n
      Character( EXT_LEN ) field

      Write( lfn, '(/,"#   Search PATH for source files")' )

      nfields = getFieldCount( VPATH, ':' )

      Call getField( VPATH, ':', 1, field )
      Write( lfn, '(" VPATH = ",a,":",$)' ) Trim( field )

! print each field at a time
      Do n = 2, nfields
        Call getField( VPATH, ':', n, field )
        If ( Len_Trim(field) .Gt. 0 )
     &     Write( lfn, '(1x,a,/,2x,a,":",$)' ) backslash, Trim( field )
      End Do

      Write( lfn, '(1x)' )

      Return
      End Subroutine writeVPATH

!-------------------------------------------------------------------------------
!     WriteCPP routine:  Writes each '-D' on its own line
!-------------------------------------------------------------------------------
      Subroutine writeCPP( lfn )

      Use ModelCFG

      Implicit None

! arguments
      Integer lfn

! functions
      Integer getFieldCount

! local variables
      Integer nfields
      Integer n
      Character( EXT_LEN ) :: field

      Write( lfn, '(/" CPP = "a)' ) Trim( cpp )

      nfields = getFieldCount( cpp_flags, ' ' )

      If ( nfields .Le. 1 ) Then
        Write( lfn, '(" CPP_FLAGS = "a)' ) Trim( cpp_flags )
        Return
      End If

      Write( lfn, '(" CPP_FLAGS =",$)')

! print each field at a time
      Do n = 1, nfields
        Call getField( cpp_flags, ' ', n, field )
        Write( lfn, '(1x,a,/,2x,a,$)' ) backslash, Trim( field )
      End Do

      Write( lfn, '(1x)' )

      Return
      End Subroutine writeCPP

!-------------------------------------------------------------------------------
!     WriteLIB routine:  Writes libraries line to Makefile
!-------------------------------------------------------------------------------
      Subroutine writeLIB( lfn )

      Use ModelCFG

      Implicit None

! arguments
      Integer lfn

! functions
      Integer getFieldCount

! local variables
      Integer nfields
      Integer n
      Integer pos
      Character( EXT_LEN ) :: field
      Character( EXT_LEN ) :: librec
      Character( EXT_LEN ) :: libname
      Character( EXT_LEN ) :: libs

!     Write( lfn, '(/,"#   Library paths")' )
      Write( lfn, '(1x)' )

   !  nfields = getFieldCount( libraries, ' ' )

      If ( nfields .Le. 1 ) Then
   !    Write( lfn, '("LIBRARIES  = ",a)' ) Trim( libraries )
        Return
      End If

      libs = 'LIBRARIES ='

! parse library fields
      librec = ' '
      libname = ' '
      Do n = 1, nfields
   !    Call getField( libraries, ' ', n, field )

        If ( n .Gt. 1 .And. field(1:2) .Eq. '-L' ) Then
          If ( libname .Eq. ' ' ) Write( libname, '(''LIB'',i2.2)' ) n/2

          Write( lfn, '(a)' ) Trim( libname ) // ' = ' // Trim( librec )
          libs = Trim( libs ) // ' $(' // Trim( libname ) // ')'
          librec = ' '
          libname = ' '
        End If

        librec = Trim( librec ) // ' ' // Trim( field )

        pos = Index( librec, '-l' )
        If ( libname .Eq. ' ' .And. pos .Gt. 0 ) Then
           libname = librec( pos+2: )
           Call ucase( libname )
        End If

      End Do

      If ( libname .Eq. ' ' ) Write( libname, '("LIB",i2.2)' ) n/2
      Write( lfn, '(a)' ) Trim( libname ) // ' = ' // Trim( librec )
      libs = Trim( libs ) // ' $(' // Trim( libname ) // ')'

  !   Write( lfn, '(/,''#   Libraries'')' )
      Write( lfn, '(/,a)' ) Trim( libs )

      Return
      End Subroutine writeLIB

!-------------------------------------------------------------------------------
!     WriteINC routine:  Writes include lines to Makefile
!-------------------------------------------------------------------------------
      Subroutine writeINC( lfn )

      Use ModelCFG

      Implicit None

! arguments
      Integer lfn

! functions
      Integer getNumberOfFields

! parameters
!     Integer, Parameter :: n_Mac = 4
!     Integer            :: pathInd( n_Mac ) = (/ 2, 1, 1, 1 /)
!     Integer, Parameter :: n_Inc = 8
!     Integer, Parameter :: pathMap( n_Inc ) = (/ 1, 1, 1, 1, 3, 3, 3, 4 /)
      Integer, Parameter :: n_Mac = 3
      Integer            :: pathInd( n_Mac ) = (/ 2, 1, 1 /)
  !   parallel & serial
      Integer, Parameter :: n_Inc = 8
      Integer, Parameter :: pathMap( n_Inc ) = (/ 1, 1, 1, 1, 2, 2, 2, 3 /)
  !   Integer, Parameter :: n_Inc = 6
  !   Integer, Parameter :: pathMap( n_Inc ) = (/ 1, 1, 1, 2, 2, 2 /)

! local variables
      Integer i, j, n
      Integer Map
      Integer pos, pos2
      Integer n_M
      Character( 8 ) :: pathMacro( n_Mac ) =
     &                         (/'BASE_INC',
     &                           'PA_INC  ',
     &                           'MPI_INC '/)
      Character( EXT_LEN ) :: pathChk( n_Mac ) =
     &                         (/'SUBST_PE_COMM ',
     &                           'SUBST_PACTL_ID',
     &                           'SUBST_MPI     '/)
 !    Character( EXT_LEN ) :: pathInc( n_Inc ) =
 !   &                         (/'SUBST_PE_COMM ',
 !   &                           'SUBST_CONST   ',
 !   &                           'SUBST_FILES_ID',
 !   &                           'SUBST_EMISPRM ',
 !   &                           'SUBST_PACTL_ID',
 !   &                           'SUBST_PACMN_ID',
 !   &                           'SUBST_PADAT_ID',
 !   &                           'SUBST_MPI     '/)
      Character( EXT_LEN ) :: pathStr( n_Mac )
      Logical              :: hasPaths

      Character( EXT_LEN ) :: path
      Integer :: adjustment         ! for the twoway model

      If ( n_includes .Eq. 0 ) Return

      pathStr = ' '
      hasPaths = .False.

      If ( serial ) Then
         n_M = n_Mac - 1
      Else
         n_M = n_Mac
      End If

      If ( twoway ) Then

! find path strings
        Do n = 1, n_includes
          Do i = 1, n_Mac
            If ( include( n )%name .Eq. pathChk( i ) ) Then
              path = include( n )%path
              If ( pathInd( i ) .Gt. 1 ) Then
                adjustment = 1
              Else
                adjustment = 0
              End If
              Do j = 1, pathInd( i ) - adjustment
                pos = Index( path, '/', .True.)
                If ( pos .Le. 0 ) Exit
                path = path( 1:pos-1 )
              End Do
              pathStr( i ) = path
              hasPaths = .True.
            End If
          End Do   ! n_Mac loop
        End Do   ! n_includes loop

        If ( hasPaths ) Then
          Write( lfn, '(1x)' )
          Do i = 1, n_Mac
            If ( pathStr( i ) .Ne. ' ' ) Then
!             Write( lfn, '(a," = ",a)' ) Trim( pathMacro( i ) ), '.'
              Write( lfn, '(a," = ",a)' ) pathMacro( i ), '.'
            End If
          End Do
        End If

      Else

! find path strings
        Do n = 1, n_includes
          Do i = 1, n_M
  !       write( *,* ) "%name, chk: ", n, i, trim( include( n )%name ), ' ', trim( pathChk( i ) )
            If ( include( n )%name .Eq. pathChk( i ) ) Then
              path = include( n )%path
              pos = Index( path, '/', .True.) ! find char index of last "/" in "path"
  !           write( *,* ) " path, pos: ", n, i, trim( path ), pos
              If ( pos .Le. 0 ) Exit
              path = path( 1:pos-1 )
              pathStr( i ) = path
              hasPaths = .True.
            End If
          End Do  ! n_Mac loop
        End Do  ! n_includes loop

        If ( hasPaths ) Then
          Write( lfn, '(1x)' )
          Do i = 1, n_M
            If ( pathStr( i ) .Ne. ' ' ) Then
!             Write( lfn, '(1x,a," = ",a)' ) Trim( pathMacro(i) ), Trim( pathStr(i) )
              Write( lfn, '(1x,a," = ",a)' ) pathMacro( i ), Trim( pathStr( i ) )
            End If
          End Do
        End If

      End If

! write include lines
      Write( lfn, '(/" INCLUDES = ",$)' )

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  include(n)%name  include(n)%path
!  SUBST_PE_COMM = ./PE_COMM.EXT             <- BASE_INC
!  SUBST_CONST = ./CONST.EXT                 <- BASE_INC
!  SUBST_FILES_ID = ./FILES_CTM.EXT          <- BASE_INC
!  SUBST_EMISPRM = ./EMISPRM.EXT             <- BASE_INC
!  SUBST_PACTL_ID = ./PA_CTL.EXT             <- PA_INC
!  SUBST_PACMN_ID = ./PA_CMN.EXT             <- PA_INC
!  SUBST_PADAT_ID = ./PA_DAT.EXT             <- PA_INC
!  SUBST_MPI = /home/wdx/lib_sol/x86_64/intel/mpich/include/mpif.h
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      Do n = 1, n_includes
        path = include( n )%path
        Map = pathMap( n )

        Do i = 1, Size( pathMacro )
          pos = Index( path, Trim( pathStr( i ) ) )
          If ( pos .Gt. 0 .And. pathStr( i ) .Ne. ' ' ) Then
            pos2 = pos + Len_Trim( pathStr( i ) )
            If ( pos .Eq. 1 ) Then
              j = Index( path, 'mpi')
              if (j .eq. 0) then
                 path = '$(' // Trim( pathMacro( Map ) ) // ')' // path( pos2: )
              else
                 path = path(3:)
              end if
              Exit
            Else
              If ( twoway ) Then
                path = path( 1:pos-1 ) // '$('
     &                                 // Trim( pathMacro( i ) ) // ')'
              Else
                path = path( 1:pos-1 ) // '$('
     &                               // Trim( pathMacro( Map ) ) // ')' // path( pos2: )
              End If
            End If
          End If
        End Do

!!! BUT IF pathStr(i) is NOT unique for i = 1, 3, then path gets overwritten.
!!! The algorithm assumes different pathStr's for diff i's.
!!! For CopySrc option, pathStr(1) = pathStr(3) = '.'

        Write( lfn, '(1x,a,/,''  -D'',a,''='',a,''"'',a,a,''"'',$)' ) backslash,
     &         Trim( include( n )%name ), backslash, Trim( path ), backslash

! define include(n)%path2 to path with macro subsitutions
        include( n )%path2 = path

      End Do

      Write( lfn,'(1x)' )

      Return
      End Subroutine writeINC

!-------------------------------------------------------------------------------
!     WriteOBJS routine:  Writes objects files by modules to Makefile
!-------------------------------------------------------------------------------
      Subroutine writeOBJS( lfn )

      Use ModelCFG

      Implicit None

! arguments
      Integer lfn

! functions
      Integer getNumberOfFields

! local variables
      Integer :: nfiles
      Integer :: nfields
      Integer :: n
      Integer :: i
      Integer :: pos
      Character( FLD_LEN ) :: filename( MAX_FILES )
      Character( FLD_LEN ) :: modname
      Character( FLD_LEN ) :: modname1
      Character( FLD_LEN ) :: modname2
      Character( FLD_LEN ) :: obj
      Character( FLD_LEN ) :: objStr = ' '
      Character( FLD_LEN ) :: tobjStr = ' '

! get list of all global modules
                                  ! global
      Call orderfiles( module(1), .True., nfiles, filename )
      modname = 'GLOBAL_MODULES'

      If ( nfiles .Gt. 0 ) Then
        tobjStr = Trim( modname )
        Write( lfn, '(//a," =",$)' ) Trim( tobjStr )
        Do i = 1, nfiles
          pos = Index( filename(i), '.' )
          If ( pos .Le. 0 ) Cycle
          obj = filename(i)(1:pos) // 'o'
          Write( lfn, '(1x,a/2x,a,$)' ) backslash, Trim( obj )
        End Do
      End If

! The $(GLOBAL_MODULES) macro has to follow the $(STENEX) macro if serial, and the
! $(PARIO) macro, if parallel.
! assumes cfg module order for CCTM is:
!  -parallel-      -serial-
!   STENEX (se)     STENEX (noop)
!   PARIO           driver or other (If PARIO does not follow STENEX: assume serial CCTM)

      modname1 = module(1)%name; Call ucase( modname1 )
      modname2 = module(2)%name; Call ucase( modname2 )

      If ( Trim( modname1 ) .Eq. 'STENEX' .And. Trim( modname2 ) .Eq. 'PARIO' ) Then
        Do n = 1, n_modules
          Call orderfiles( module(n), .False., nfiles, filename )
          If ( nfiles .Gt. 0 ) Then
            modname = module(n)%name
            Call ucase( modname )
            Write( lfn, '(//a," =",$)' ) Trim( modname )
            objStr = Trim( objStr ) // ' $(' // Trim( modname ) // ')'
            If ( n .Eq. 2 ) Then
            objStr = Trim( objStr ) // ' $(' // Trim( tobjStr ) // ')'
            End If

            Do i = 1, nfiles
              pos = Index( filename(i), '.' )
              If ( pos .Le. 0 ) Cycle
              obj = filename(i)(1:pos) // 'o'
              Write( lfn, '(1x,a/2x,a,$)' ) backslash, Trim( obj )
            End Do

          End If
        End Do

      Else If ( Trim( modname1 ) .Eq. 'STENEX' .And. Trim( modname2 ) .Ne. 'PARIO' ) Then
        Do n = 1, n_modules
          Call orderfiles( module(n), .False., nfiles, filename )
          If ( nfiles .Gt. 0 ) Then
            modname = module(n)%name
            Call ucase( modname )
            Write( lfn, '(//a," =",$)' ) Trim( modname )
            objStr = Trim( objStr ) // ' $(' // Trim( modname ) // ')'
            If ( n .Eq. 1 ) Then
              objStr = Trim( objStr ) // ' $(' // Trim( tobjStr ) // ')'
            End If

            Do i = 1, nfiles
              pos = Index( filename(i), '.' )
              If ( pos .Le. 0 ) Cycle
              obj = filename(i)(1:pos) // 'o'
              Write( lfn, '(1x,a/2x,a,$)' ) backslash, Trim( obj )
            End Do

          End If
        End Do

      Else If ( Trim( modname1 ) .Ne. 'STENEX' .And. Trim( modname2 ) .Ne. 'PARIO' ) Then
        Do n = 1, n_modules
          Call orderfiles( module(n), .False., nfiles, filename )
          If ( nfiles .Gt. 0 ) Then
            modname = module(n)%name
            Call ucase( modname )
            Write( lfn, '(//a," =",$)' ) Trim( modname )
            If ( n .Eq. 1 ) Then
              objStr = ' $(' // Trim( tobjStr ) // ')'
            End If
            objStr = Trim( objStr ) // ' $(' // Trim( modname ) // ')'

            Do i = 1, nfiles
              pos = Index( filename(i), '.' )
              If ( pos .Le. 0 ) Cycle
              obj = filename(i)(1:pos) // 'o'
              Write( lfn, '(1x,a/2x,a,$)' ) backslash, Trim( obj )
            End Do

          End If
        End Do
      End If

      Write( lfn, '(//"OBJS =",$)' )

      nfields = getNumberOfFields( objStr, ' ' )
      Do n = 1, nfields
        Call getField( objStr, ' ', n, obj )
        Write( lfn, '(1x,a/2x,a,$)' ) backslash, Trim( obj )
      End Do

      Write( lfn,'(1x)' )

      Return
      End Subroutine writeOBJS

!-------------------------------------------------------------------------------
!     WriteDEP routine:  Writes USE/MODULE and INCLUDE dependencies lines to Makefile
!-------------------------------------------------------------------------------
      Subroutine writeDEP( lfn )

      Use ModelCFG

      Implicit None

! arguments
      Integer lfn

! functions
      Integer getNumberOfFields

! local variables
      Character( FLD_LEN ) :: objname
      Character( FLD_LEN ) :: reqStr 
      Character( NAME_LEN ) :: modName
      Character( NAME_LEN ) :: modFile
      Integer :: nfields
      Integer :: n
      Integer :: i
      Integer :: j
      Integer :: pos
      Character( 1 )       :: tab = char(9)

      Logical :: incdep( n_includes )
      Integer :: ndep
      Character( FLD_LEN ) :: depFile( MAX_FILES+n_includes )

      Write( lfn, '("# dependencies"/)' )

! loop thru each archive module and write list of source file dependencies
      Do n = 1, n_modules
        Do i = 1, module(n)%nfiles
          ndep = 0
          depFile = ''

! build object filename
          objname = module(n)%file(i)%name
          pos = Index( objname, '.' )
          If ( pos .Gt. 0 ) objname = objname( 1:pos ) // 'o'

! parse USEs string to get module names 
          nfields = getNumberOfFields( module(n)%file(i)%uses, ':' )
          write( *,* ) ' '
          If ( nfields .Gt. 2 ) Then 
            write( *,* ) 'file,nfields-1 ', Trim( module( n )%file( i )%name ),
     &                   ' ', nfields-1
            Do j = 2, nfields-1
              Call getField( module( n )%file( i )%uses, ':', j, modName )
              Call getModFile( modName, modFile ) 
              If ( Len_Trim( modFile ) .Gt. 0 ) Then 
                write( *,* ) 'modName,modFile ', j, trim( modName ),
     &                       ' ', trim( modFile )
                ndep = ndep+ 1
! change dependency file to object file
                pos = Index( ModFile, '.' )
                If ( pos .Gt. 0 ) DepFile( ndep ) = ModFile( 1:pos ) // 'o'
              Else
                write( *,* ) 'modName         ', j, trim( modName ),
     &                       '  -------------'
              End If
            End Do
          End If   ! has USEs files

! check for include file dependencies
          Call getIncDep( module( n )%file( i )%path, incdep )
          Do j = 1, n_includes
            If ( incdep( j ) ) Then
! Do not include mpif.h dependency
              pos = Index( include( j )%path2, 'mpif' )
              If ( pos .Eq. 0 ) Then
                ndep = ndep + 1
                depFile( ndep ) = include( j )%path2
              End If
  !         write( *,'(a,3i3,1x,a)' ) 'depend: ', n, j, ndep, trim( depFile( ndep ) )
            End If
          End Do
              
! write dependencies string
          reqStr = ''
          Do j = 1, ndep
            If ( Len_Trim( reqStr ) .Eq. 0 ) Then
              reqStr = Trim( objName ) // ':' // tab // depFile( j )
            Else If ( Len_Trim( reqStr ) .Le. 60 ) Then
              reqStr = Trim( reqStr ) // ' ' // depFile( j )
            Else
              Write( lfn,'(a,1x,a)' ) Trim( reqStr ), backslash
              reqStr = tab // tab // depFile( j )
            End If
          End Do

          If ( Len_Trim( reqStr ) .Gt. 0 ) Write( lfn,'(a)' ) Trim( reqStr )

        End Do    ! file loop
      End Do    ! module loop

      Return
      End Subroutine writeDEP 

!-------------------------------------------------------------------------------
!     WriteRules routine:  Writes rules to Makefile
!-------------------------------------------------------------------------------
      Subroutine writeRules( lfn )

      Use ModelCFG

      Implicit None

! arguments
      Integer              :: lfn

! local variables
      Integer              :: n
      Character( FLD_LEN ) :: record
      Character( 1 )       :: tab = char( 9 )

! build SUFFIXES record
      record = '.SUFFIXES:'

      Do n = 1, Size( extension )
        record = Trim( record ) // ' ' // extension( n )
      End Do

      Write( lfn, '(/,a)') Trim( record )

      Write( lfn, '(/"$(EXEC): $(OBJS)")' )
      Write( lfn, '(a,"$(LINKER) $(LINK_FLAGS) $(OBJS) $(LIBRARIES) -o $@"/)' ) tab

      Write( lfn, '(".F.o:")' )
      Write( lfn, '(a,"$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<"/)' ) tab

      Write( lfn, '(".f.o:")' )
      Write( lfn, '(a,"$(FC) -c $(F_FLAGS) $<"/)' ) tab

      Write( lfn, '(".F90.o:")' )
      Write( lfn, '(a,"$(FC) -c $(F90_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<"/)' ) tab

      Write( lfn, '(".f90.o:")' )
      Write( lfn, '(a,"$(FC) -c $(F90_FLAGS) $<"/)' ) tab

      Write( lfn, '(".c.o:")' )
      Write( lfn, '(a,"$(CC) -c $(C_FLAGS) $<"/)' ) tab

      Write( lfn, '("clean:")' )
  !   Write( lfn, '(a,"rm -f $(OBJS) $(MODEL) *.mod"/)' ) tab
      Write( lfn, '(a,"rm -f $(OBJS) *.mod"/)' ) tab

      Write( lfn, '(1x)' )

      Return
      End Subroutine writeRules

