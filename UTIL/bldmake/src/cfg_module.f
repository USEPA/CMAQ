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
!  Module containing config data for model builder
!  originally written in C by Steve Thorpe
!  rewritten in Fortran by Steve Howard
!  redone to meet CMAQ coding standards by Jeff Young (Nov 2012)
!  increased name lengths from 32 to 64 with new NAME_LEN parameter J.Young (Sep 2013)
!  Modified subroutine findGlobal to put all STENEX routines under STENEX and all PARIO 
!  routines under PARIO rather than under GLOBAL_MODULES by D. Wong (Aug 2015)
!-------------------------------------------------------------------------------
      Module ModelCfg

! parameters
      Integer, Parameter :: REC_LEN   = 2048
      Integer, Parameter :: FLD_LEN   = 1024
      Integer, Parameter :: FLN_LEN   =  512
      Integer, Parameter :: EXT_LEN   =  256
      Integer, Parameter :: NAME_LEN  =   64
      Integer, Parameter :: MAX_FLD   =  100
      Integer, Parameter :: MAX_ICL   =  200
      Integer, Parameter :: MAX_MOD   =  200
      Integer, Parameter :: MAX_FILES =  500

      Character(1), Parameter :: backslash = '\\'

! source file extensions
      Character(4), Parameter :: extension(5) =
     &                          (/ '.F  ', '.f  ', '.c  ', '.F90', '.f90' /)

! user derived types for storing CFG data

      Type file_type
        Character( FLN_LEN )  :: name              ! file name
        Character( FLN_LEN )  :: path              ! path name
        Character( NAME_LEN ) :: mod_name          ! F90 module name
        Character( FLD_LEN )  :: uses              ! F90 USE names
        Logical               :: global_module     ! F90 module USEd in other modules
      End Type file_type

      Type module_type
        Character( NAME_LEN ) :: name              ! module name
        Character( NAME_LEN ) :: version           ! repository version
        Integer               :: nfiles            ! Number of files in module
        Type( file_type )     :: file( MAX_FILES ) ! module files
      End Type module_type

      Type include_type
        Character( NAME_LEN ) :: name              ! include name
        Character( FLD_LEN )  :: path              ! include path
        Character( FLD_LEN )  :: path2             ! include path as macros
      End Type include_type

! global flags
      Logical :: verbose
      Logical :: debug
      Logical :: checkout
      Logical :: serial      ! .true. = create Makefile for serial execution
      Logical :: makefo      ! .true. = only create Makefile
      Logical :: git_local   ! .true. = do not copy source files to BLD directory
      Logical :: twoway      ! .true. = compile for WRF-CMAQ CCTM

! repository
      Character( FLN_LEN ) :: repo

! repository root
      Character( FLN_LEN ) :: reporoot

! mechanism
      Character( FLN_LEN ) :: mechanism = 'X'

! current system date
      Character( 20 ) :: currentDate

! model name
      Character( 128 ) :: model

! make file search path for source files
      Character( FLD_LEN ) :: VPATH = ' '

! make file include libraries
      Character( FLN_LEN ) :: lib_base
      Character( FLN_LEN ) :: lib_1
      Character( FLN_LEN ) :: lib_2
      Character( FLN_LEN ) :: lib_3
      Logical              :: l_lib_3
      Character( FLN_LEN ) :: lib_4

      Character( FLN_LEN ) :: fstd
      Character( FLN_LEN ) :: dbg
      
! compilers and flags
      Character( FLN_LEN ) :: f_compiler   ! Fortran compiler
      Character( FLN_LEN ) :: f_compiler_path ! Fortran compiler path
      Character( FLN_LEN ) :: f_flags      ! .f, .F
      Character( FLN_LEN ) :: Fflags       ! .F
      Character( FLN_LEN ) :: F90flags     ! .F90
      Character( FLN_LEN ) :: f90_flags    ! .f90, .F90

      Character( FLN_LEN ) :: c_compiler   ! c compiler
      Character( FLN_LEN ) :: c_compiler_path   ! c compiler path
      Character( FLN_LEN ) :: c_flags

      Character( FLN_LEN ) :: cpp          ! pre_compiler
      Character( FLD_LEN ) :: cpp_flags

! linker
      Character( FLN_LEN ) :: linker
      Character( FLD_LEN ) :: link_flags

! includes
      Integer :: n_includes
      Type( include_type ) :: include( MAX_ICL )

! modules
      Integer :: n_modules
      Type( module_type ) :: module( MAX_MOD )

! libraries
!     Character( FLD_LEN ) :: libraries
      Character( FLD_LEN ) :: ioapi
      Character( FLD_LEN ) :: netcdf
      Character( FLD_LEN ) :: mpich

! library locations
      Character( FLD_LEN ) :: ioapi_mod_dir
      Character( FLD_LEN ) :: ioapi_incl_dir
      Character( FLD_LEN ) :: ioapi_lib_dir
      Character( FLD_LEN ) :: netcdf_lib_dir
      Character( FLD_LEN ) :: mpi_lib_dir

! misc module number for local files
      Integer :: miscMod

      Contains

!-------------------------------------------------------------------------------
!     Reader routine for cfg file
!-------------------------------------------------------------------------------
      Subroutine readCFG( lfn )

      Implicit None

! arguments
      Integer lfn

! functions
      Integer getNumberOfFields

! local variables
      Integer :: status
      Character( REC_LEN ) :: record
      Character( REC_LEN ) :: nextrecord
      Character( FLD_LEN ) :: field
      Character( FLD_LEN ) :: key
      Character( FLD_LEN ) :: fields( MAX_FLD )
      Character( FLD_LEN ) :: fortran
      Integer :: nfields
      Integer :: n
      Integer :: i
      Integer :: reclen

      n_includes = 0
      n_modules = 0
      miscMod = 0
      l_lib_3 = .FALSE.

      model = 'a.out'

      f_compiler = 'mpiifort'
      c_compiler = 'cc'
      cpp = ' '
      linker = ' '

      f_flags = ' '
      f90_flags = ' '

      c_flags = ' '
      cpp_flags = ' '
      link_flags = ' '

      Do
        Read ( lfn, '(a)', iostat=status ) record
        If ( status .Ne. 0 ) Exit

! skip blank lines
        If ( record .Eq. ' ' ) Cycle

! skip comment lines
        If ( Index( '!/', record(1:1) ) .Gt. 0 ) Cycle

! append any continuation records
        Do
          ! check for eol - ';' character
          If ( Index( record, ';' ) .Gt. 0 ) Then
            reclen = Index( record, ';' )
            record = record(1:reclen-1)
            Exit  ! this Do
          End If

          Read ( lfn, '(a)', IOSTAT=status ) nextrecord
          If ( status .Ne. 0 ) Then
            Write(*,'("**ERROR** EOF reading continuation record")')
            Exit
          End If

          Call LeftTrim( nextrecord )

! check for record length exceeded
          If ( (Len_Trim(record) + Len_Trim(nextrecord)) .Ge. (Len(record)) ) Then
            Write(*,'(//"**ERROR** The maximum record length exceeded")')
            Write(*,'("  Maximum length =",i8)') Len(record)
            Write(*,'("  Record length = ",i8)') Len_Trim(record) + Len_Trim(nextrecord)
            Stop
          End If

! append nextrecord to record
          record = Trim( record ) // ' ' // Trim( nextrecord )

        End Do

        If ( debug ) Then
          Write(*,'("Parsing record:",a)') Trim( record )
        End If

! replace tabs with spaces
        Call replace( record, char(9), char(32), .False. )

! replace spaces with tabs within quot marks
        Call replace( record, char(32), char(9), .True. )

! parse fields by spaces
        nfields = getNumberOfFields( record, ' ' )
        If ( nfields .gt. Size(fields) ) nfields = Size(fields)
        fields = ' '

        Do n = 1, nfields
          Call getField( record, ' ', n, field )
          Call replace( field, char(9), char(32), .False. )
          Call rmQuots( field )
          fields(n) = field
        End Do

! set key to upper case of field 1
        key = fields(1)
        Call ucase( key )

! check for executable name
        If ( key .Eq. 'MODEL' ) Then
          model = fields(2)
          If ( verbose ) Write( *, '("Model set to ",a)' ) Trim( model )
          Cycle
        End If

! check for repository name
        If ( key .Eq. 'REPO' ) Then
          repo = fields(2)
          If ( verbose ) Write( *, '("Repository set to ",a)' ) Trim( repo )
          Cycle
        End If

! check for full chemistry mechanism name
        If ( key .Eq. 'MECHANISM' ) Then
          mechanism = fields(2)
          If ( verbose ) Write( *, '("Mechanism set to ",a)' ) Trim( mechanism )
          Cycle
        End If

! check for external libraries
        If ( key .Eq. 'LIB_BASE' ) Then
          lib_base = fields(2)
          If ( verbose ) Write( *, '("LIB_BASE set to ",a)' ) Trim( lib_base )
          Cycle
        End If

        If ( key .Eq. 'LIB_1' ) Then
          lib_1 = fields(2)
          If ( verbose ) Write( *, '("LIB_1 set to ",a)' ) Trim( lib_1 )
          Cycle
        End If

        If ( key .Eq. 'LIB_2' ) Then
          lib_2 = fields(2)
          If ( verbose ) Write( *, '("LIB_2 set to ",a)' ) Trim( lib_2 )
          Cycle
        End If

        If ( key .Eq. 'LIB_3' ) Then
          l_lib_3 = .TRUE.
          lib_3 = fields(2)
          If ( verbose ) Write( *, '("LIB_3 set to ",a)' ) Trim( lib_3 )
          Cycle
        End If
 
        If ( key .Eq. 'LIB_4' ) Then
          lib_4 = fields(2)
          If ( verbose ) Write( *, '("LIB_4 set to ",a)' ) Trim( lib_4 )
          Cycle
        End If

 
! check for Fortran compilers
        If ( key .Eq. 'F_COMPILER' ) Then
          f_compiler = fields(2)
          If ( verbose ) Write( *, '("F_COMPILER set to ",a)' ) Trim( f_compiler )
          Cycle
        End If

! check for compile flags
        If ( key .Eq. 'FSTD' ) Then
          fstd = fields(2)
          If ( verbose ) Write( *, '("FSTD set to ",a)' ) Trim( fstd )
          Cycle
        End If

        If ( key .Eq. 'DBG' ) Then
          dbg = fields(2)
          If ( verbose ) Write( *, '("DBG set to ",a)' ) Trim( dbg )
          Cycle
        End If

        If ( key .Eq. 'F_FLAGS' ) Then
          f_flags = fields(2)
          If ( verbose ) Write( *, '("f_flags set to ",a)' ) Trim( f_flags )
          Cycle
        End If

        If ( key .Eq. 'F90_FLAGS' ) Then
          f90_flags = fields(2)
          If ( verbose ) Write( *, '("f90_flags set to ",a)' ) Trim( f90_flags )
          Cycle
        End If

! check for c compiler
        If ( key .Eq. 'C_COMPILER' ) Then
          c_compiler = fields(2)
          If ( verbose ) Write( *, '("C_COMPILER set to ",a)' ) Trim( c_compiler )
          Cycle
        End If

        If ( key .Eq. 'C_FLAGS' ) Then
          c_flags = fields(2)
          If ( verbose ) Write( *, '("C_FLAGS set to ",a)' ) Trim( c_flags )
          Cycle
        End If

! check for preprocessor
        If ( key .Eq. 'CPP' ) Then
          cpp = fields(2)
          If ( verbose ) Write( *, '("CPP set to ",a)' ) Trim( cpp )
          Cycle
        End If

        If ( key .Eq. 'CPP_FLAGS' ) Then
          cpp_flags = fields(2)
          If ( verbose ) Write( *, '("CPP_FLAGS set to ",a)' ) Trim( cpp_flags )
          Cycle
        End If

! check for linker
        If ( key .Eq. 'LINKER' ) Then
          linker = fields(2)
          If ( verbose ) Write( *, '("LINKER set to ",a)' ) Trim( linker )
          Cycle
        End If

        If ( key .Eq. 'LINK_FLAGS' ) Then
          link_flags = fields(2)
          If ( verbose ) Write( *, '("LINK_FLAGS set to ",a)' ) Trim( link_flags )
          Cycle
        End If

        If ( key .Eq. 'IOAPI' ) Then
          ioapi = fields(2)
          If ( verbose ) Write( *, '("IOAPI set to ",a)' ) Trim( ioapi )
          Cycle
        End If

        If ( key .Eq. 'NETCDF' ) Then
          netcdf = fields(2)
          If ( verbose ) Write( *, '("NETCDF set to ",a)' ) Trim( netcdf )
          Cycle
        End If

        If ( key .Eq. 'MPICH' ) Then
          mpich = fields(2)
          If ( verbose ) Write( *, '("MPICH set to ",a)' ) Trim( mpich )
          Cycle
        End If

        If ( key .Eq. 'INCLUDE' ) Then
          n_includes = n_includes + 1
          include( n_includes )%name = fields(2)
          include( n_includes )%path = fields(3)
          If ( verbose ) Write( *, '("INCLUDE ",a," = ",a)' )
     &                            Trim( fields(2) ), Trim( fields(3) )
          Cycle
        End If

        If ( key .Eq. 'MODULE' ) Then
          n_modules = n_modules + 1
          module( n_modules )%name = fields(2)
          module( n_modules )%version = 'HEAD'
          If ( Len_Trim( fields(3) ) .Gt. 0 ) Then
            If ( fields(3) .Ne. 'release' )  module( n_modules )%version = fields(3)
          End If
          Cycle
        End If

        If ( key .Eq. 'MISC' ) Then
          If ( miscMod .Eq. 0 ) Then   !! add MISC module for local files
            n_modules = n_modules + 1
            miscMod = n_modules
            module( miscMod )%name = 'MISC'
            module( miscMod )%version = 'LOCAL'
            module( miscMod )%nfiles = 0
          End If

          Do i = 2, nfields
            module( miscMod )%nfiles = module( miscMod )%nfiles + 1
            If ( module( miscMod )%nfiles .Gt. MAX_FILES ) Then
              Write( *, '(/"**ERROR** Number of MISC files exceed Maximum")' )
              Stop
            End If

            module( miscMod )%file( module( miscMod )%nfiles )%name = fields(i)
          End Do

          Cycle
        End If

! invalid key
        If ( debug ) Then
          Write( *, '(/"**Warning** input key:",a)' ) Trim( key )
          Write( *, '("  on record:",a)' ) Trim( record )
        End If

      End Do  ! read loop

! set compilers to full path names
      If ( f_compiler(1:1) .Ne. '/' ) Then
        Call which( f_compiler, field, status )
        If ( status .Eq. 0 ) f_compiler_path = field
      End If 

      If ( c_compiler(1:1) .Ne. '/' ) Then
        Call which( c_compiler, field, status )
        If ( status .Eq. 0 ) c_compiler_path = field
      End If 

! set defaults
      If ( debug .And. linker .Eq. ' ' ) Then
        Write( *, '(/"**Warning** LINKER not defined, using [F_COMPILER]")' )
      End If
      If ( linker .Eq. ' ' ) linker = '$(FC)'

      If ( debug .And. cpp .Eq. ' ' ) Then
        Write( *, '(/"**Warning** CPP not defined, using [F_COMPILER]")' )
      End If
      If ( cpp .Eq. ' ' ) cpp = '$(FC)'   

      If ( debug .And. Fflags .Eq. ' ' ) Then
        Write( *, '(/"**Warning** F_FLAGS not defined, using [f_FLAGS]")' )
      End If
      If ( Fflags .Eq. ' ' ) Fflags = '$(f_FLAGS)'

      If ( debug .And. F90flags .Eq. ' ' ) Then
        Write( *, '(/"**Warning** F90_FLAGS not defined, using [f90_FLAGS]")' )
      End If
      If ( F90flags .Eq. ' ' ) F90flags = '$(f90_FLAGS)'

      Return
      End Subroutine readCFG

!-------------------------------------------------------------------------------
!     Print Help page for Config-file format
!-------------------------------------------------------------------------------
      Subroutine cfgHelp()

      Implicit None

      Write( *, '(/"   ***** BLDMAKE Configuration file Format *****",/)' )

      Write( *, '(/"       Keys                       Arguments")' )
      Write( *, '( "  ---------------  --------------------------------------------")' )

      Write( *, '( "   Model           (Model Executable Name)                     ")' )
      Write( *, '( "   Repo            (Model Repository Name)                     ")' )
      Write( *, '( "   Mechanism       (Model Mechanism Name)                      ")' )

      Write( *, '(/"   F_Compiler      (Fortran Compiler)                          ")' )
      Write( *, '( "   f_FLAGS         (Fortran Compiler options for .f sources)   ")' )
      Write( *, '( "   f90_FLAGS       (Fortran Compiler options for .f90 sources) ")' )

      Write( *, '(/"   C_Compiler      (C Compiler)                                ")' )
      Write( *, '( "   C_FLAGS         (C Compiler options for .c sources)         ")' )

      Write( *, '(/"   CPP             (Pre-Compiler)                              ")' )
      Write( *, '( "   CPP_FLAGS       (Pre-Compiler options)                      ")' )

      Write( *, '(/"   LINKER          (Linker)                                    ")' )
      Write( *, '( "   LINK_FLAGS      (Linker options)                            ")' )

!     Write( *, '(/"   LIBRARIES       (Libraries in ''-L/-l'' format)             ")' )
      Write( *, '(/"   IOAPI           (Libraries in ''-L/-l'' format)             ")' )
      Write( *, '(/"   NETCDF          (Libraries in ''-L/-l'' format)             ")' )
      Write( *, '(/"   MPICH           (Libraries in ''-L/-l'' format)             ")' )

      Write( *, '(/"   INCLUDE         (Include Subsitution Name) (File)           ")' )

      Write( *, '(/"   Module          (GIT Subdir Name)                           ")' )

      Write( *, '(/"   MISC            (Local source files)                        ")' )

      Write( *, '(//)' )

      Return
      End Subroutine cfgHelp

!-------------------------------------------------------------------------------
!     Build list of files from each module directory
!-------------------------------------------------------------------------------
      Subroutine git_export( status )

      Implicit None

! arguments
      Integer :: status

! functions
      Integer system
      Integer getNumberOfFields

! local variables
      Integer :: lfn = 25
      Integer :: n
      Integer :: i
      Integer :: j
      Integer :: npaths
      Integer :: pos
      Character( FLD_LEN ) :: scrfile
      Character( REC_LEN ) :: record
      Character( FLD_LEN ) :: path
      Character( FLD_LEN ) :: toFile
      Character( FLD_LEN ) :: cmdline
      Logical :: found

      If ( git_local ) VPATH = '$(REPOROOT):'

! loop thru each module directory and extract its files
      Do n = 1, n_modules
        If ( n .Eq. miscMod ) Cycle   !! skip MISC module

        If ( debug .Or. verbose ) Then
          Write(*,'(//,"Building file list for module ",a)') Trim(module(n)%name)
        End If

! create filename for scratch file
        Call getSCRNAME( scrfile )

        path = Trim( reporoot ) // '/' // module(n)%name

        cmdline = 'ls -1 ' // Trim(path)

! run ls command using system function
        status = system( Trim(cmdline) // ' > ' // Trim(scrfile) )
        If ( status .Ne. 0 ) Then
          If ( debug .Or. verbose ) Then
            Write( *, '(/,"**Warning** Module:",a," is not a directory",/)' )
     &                 Trim( module(n)%name )
          End If
          Call deletefile( scrfile, status )
          Cycle
        End If

! open scratch file to capture ls results
        Open ( unit=lfn, file=scrfile, iostat=status )
        If ( status .Ne. 0 ) Then
          Write( *, '("**ERROR** cannot Open scratch file to capture checkout results"/)')
          Stop
        End If

! add module name to VPATH
        If ( git_local ) VPATH = Trim(VPATH) // '$(REPOROOT)/'
     &                         // Trim(module(n)%name) // ':'

! remove any existing files from list
        module(n)%nfiles = 0
        module(n)%file = file_type(' ',' ',' ',' ',.False.)

! read output record to obtain file name
        Do   ! until iostat = 0
          Read ( lfn,'(a)',iostat=status ) record
          If ( status .Ne. 0 ) Exit

          module(n)%nfiles = module(n)%nfiles + 1
          j = module(n)%nfiles
          module(n)%file(j)%name = record        
          module(n)%file(j)%path = Trim( path ) // '/' // record

          If ( debug .or. verbose ) Then
            Write( *, '("  file ",a," added to make list")' )
     &                 Trim( module(n)%file(j)%path )
          End If

          if ( .Not. git_local ) Then
! copy file to current directory
            call copyfile( module( n )%file( j )%path, module( n )%file( j )%name, status )
! set file path to name
            module( n )%file( j )%path = module( n )%file( j )%name
          End If

        End Do

! close and delete command output file
        Close ( unit=lfn, status='delete' )

! rename module name to first directory in path   
        path = module( n )%name
        pos = Index(path, '/')
        If ( pos .Gt. 0 ) module( n )%name = path( 1:pos-1 )

      End Do     ! end of module loop

! copy MISC files to local directory
      If ( miscMod .Ne. 0 ) Then
        Do n = 1, module( miscMod )%nfiles

! copy misc file to current directory
          path = module( miscMod )%file( n )%name
          npaths = getNumberOfFields( path,'/' )
          Call getField( path, '/', npaths, toFile )
          Call copyfile( path, toFile, status )
          module( miscMod )%file( n )%name = toFile
          module( miscMod )%file( n )%path = toFile

! If file replaces an extracted file, Then add 'replaced' to extracted file name
          Call rename( toFile, Trim( toFile ) // '.replace', path )

        End Do
      End If      ! miscMod condition

! get F90 module name and USE statements for each source file
      Do n = 1, n_modules
        Call findMods( module(n) )
      End Do

! find and set global F90 modules
      Call findGlobal()

      status = 0
      Return
      End Subroutine git_export

!-------------------------------------------------------------------------------
!     get filename of F90 module
!-------------------------------------------------------------------------------
      Subroutine getModFile( modName, modFile )

      Implicit None

! arguments
      Character(*) :: modName
      Character(*) :: modFile

! local variables
      Integer n
      Integer i

      modFile = ' '

      Do n = 1, n_modules
        Do i = 1, module(n)%nfiles
          If ( modName .Eq. module(n)%file(i)%mod_name ) Then
            modFile = module(n)%file(i)%name
            Return
          End If
        End Do
      End Do

      Return
      End Subroutine getModFile

!-------------------------------------------------------------------------------
!     rename filename in module list
!-------------------------------------------------------------------------------
      Subroutine rename( oldName, newName, path )

      Implicit None

! arguments
      Character(*) :: oldName
      Character(*) :: newName
      Character(*) :: path

! local variables
      Integer :: n
      Integer :: i

      Do n = 1, n_modules
        If ( n .Eq. miscMod ) Cycle

        Do i = 1, module(n)%nfiles
          If ( module(n)%file(i)%name .eq. oldName ) Then
            module(n)%file(i)%name = Trim( newName )
            module(n)%file(i)%path = Trim( module(n)%file(i)%path ) // '.replaced'
            Write(*,'("replacing file [",a,":",a,"] with ",a)')
     &         Trim( module(n)%name ), Trim( oldName ), Trim( path )
          End If
        End Do  ! file loop
      End Do  ! module loop

      Return
      End Subroutine rename

!-------------------------------------------------------------------------------
!     Identify Fortran 90 module and use statements in source files
!-------------------------------------------------------------------------------
      Subroutine findMods( srcModule )

      Implicit None

! arguments
      Type( module_type ) :: srcModule

! local variables
      Integer :: status
      Integer :: lfn = 25
      Logical :: isSrc
      Integer :: n
      Integer :: i
      Character( REC_LEN ) :: record
      Character( FLD_LEN ) :: field

! loop thru files and search for Module and use statements
      Do n = 1, srcModule%nfiles

        ! check if file is a source file
        isSrc = .False.
        Do i = 1, Size(extension)
          If ( Index( srcModule%file(n)%name, extension(i) ) .gt. 0 ) Then
            isSrc = .True.
            Exit
          End If
        End Do

        ! skip If not a source file
        If ( .Not. isSrc ) Cycle

        srcModule%file(n)%mod_name = ' '
        srcModule%file(n)%uses = ':'

        ! Open file and scan for MODULE and USE statements
        Open (Unit=lfn, File=srcModule%file(n)%path, Status='OLD', Iostat=status)
        If ( status .Ne. 0 ) Then
          Write(*,'("**ERROR** Cannot Open source file:",a)') Trim(srcModule%file(n)%name)
          Stop
        End If

        Do
          Read (lfn,'(a)',iostat=status) record
          If ( status .Ne. 0 ) Exit

          Call ucase( record )
          Call LeftTrim( record )
          Call replace( record, ',', ' ', .False. )

          If ( record(1:7) .Eq. 'MODULE ' .And.
     &         record(8:16) .Ne. 'PROCEDURE' ) Then
            Call getField( record, ' ', 2, field )
            srcModule%file(n)%mod_name = field
          End If

          If ( record(1:4) .Eq. 'USE ' ) Then
            Call getField( record, ' ', 2, field )

            ! add modules to uses if not in list
            If ( Index( srcModule%file(n)%uses, ':'// Trim(field) // ':' ) .Eq. 0 ) Then
              srcModule%file(n)%uses = Trim(srcModule%file(n)%uses) // Trim( field ) // ':'
            End If
          End If

        End Do   ! read loop

        Close (unit=lfn)
      End Do  !  file loop

      Return
      End Subroutine findMods

!-------------------------------------------------------------------------------
!     find and set global F90 modules
!-------------------------------------------------------------------------------
      Subroutine findGlobal()

      Implicit None

! local variables
      Integer :: m
      Integer :: n
      Integer :: i
      Integer :: j
      Logical :: sorted
      Character( FLD_LEN ) :: modStr

      If ( verbose .or. debug ) Then
        Write( *, '(/"Determining global module files")' )
      End If

! loop thru each module and check If Module is used in other modules
      Do m = 1, n_modules
        if ( (trim( module(m)%name ) .Eq. 'STENEX' ) .or.
     &       (trim( module(m)%name ) .Eq. 'PARIO' ) ) cycle
        Do i = 1, module(m)%nfiles
          If ( module(m)%file(i)%mod_name .Eq. ' ' ) Cycle
          modstr = ':' // Trim(module(m)%file(i)%mod_name) // ':'

          Do n = 1, n_modules
            If ( ( n .Eq. m ) .or.
     &           ( trim( module(n)%name ) .Eq. 'STENEX' ) .or.
     &           ( trim( module(n)%name ) .Eq. 'PARIO' ) ) cycle
            If ( module(m)%file(i)%global_module ) Exit

            Do j = 1, module(n)%nfiles

              If ( Index( module(n)%file(j)%uses, Trim( modstr ) ) .gt. 0 ) Then
                module(m)%file(i)%global_module = .True.
                If ( verbose .Or. debug ) Then
                  Write( *, '("  file ",a," determined to be global")' )
     &                 Trim( module(m)%file(i)%name )
                End If
                Exit
              End If

            End Do    ! inner file loop (j)
          End Do    ! inner module loop (n)
        End Do    ! outer file loop (i)
      End Do    ! outer module loop (m)

! loop thru each module and check If global Module is used
      Do
        sorted = .True.
        Do m = 1, n_modules
          if ( ( trim( module(m)%name ) .Eq. 'STENEX' ) .or.
     &         ( trim( module(m)%name ) .Eq. 'PARIO' ) ) cycle
          Do i = 1, module(m)%nfiles
            If ( module(m)%file(i)%global_module ) Then
              modstr = ':' // Trim(module(m)%file(i)%mod_name) // ':'

              Do n = 1, n_modules
                if ( ( trim( module(n)%name ) .Eq. 'STENEX' ) .or.
     &               ( trim( module(n)%name ) .Eq. 'PARIO' ) ) cycle
                Do j = 1, module(n)%nfiles
                  If ( module(n)%file(j)%global_module ) Cycle

                  modstr = ':' // Trim( module(n)%file(j)%mod_name ) // ':'

                  If ( Index( module(m)%file(i)%uses, Trim( modstr ) ) .Gt. 0 ) Then
                    module(n)%file(j)%global_module = .True.
                    If ( verbose .Or. debug ) Then
                      Write(*,'("  file ",a," determined to be global")')
     &                   Trim( module(n)%file(j)%name )
                    End If
                    sorted = .False.
                  End If

                End Do    ! inner file loop (j)
              End Do    ! inner module loop (n)
            End If    ! if global module
          End Do    ! outer file loop (i)
        End Do    ! outer module loop (m)

        If ( sorted ) Exit
      End Do    ! sort loop

      Return
      End Subroutine findGlobal

!-------------------------------------------------------------------------------
!     Order files so F90 modules come before files that USE them
!-------------------------------------------------------------------------------
      Subroutine orderfiles( srcModule, global, nfiles, filename )

      Implicit None

! arguments
      Type( module_type )  :: srcModule              ! in
      Logical :: global                              ! in
      Integer :: nfiles                              ! out
      Character( FLD_LEN ) :: filename( MAX_FILES )  ! out

! local variables
      Character( FLD_LEN ) :: modname( MAX_FILES )
      Character( FLD_LEN ) :: usename( MAX_FILES )

      Character( FLD_LEN ) :: filetemp
      Character( FLD_LEN ) :: modtemp
      Character( FLD_LEN ) :: usetemp

      Integer :: n
      Integer :: i
      Integer :: m
      Logical :: isSrc
      Logical :: sorted
      Integer :: msort
      Integer :: nmodfiles

! Build list of files to sort
      nfiles = 0

! If global, build list from all modules
      If ( global ) Then
        Do n = 1, n_modules
          Do i = 1, module(n)%nfiles
            If ( module(n)%file(i)%global_module ) Then
              nfiles = nfiles + 1
              filename( nfiles ) = module(n)%file(i)%name
              modname( nfiles )  = module(n)%file(i)%mod_name
              usename( nfiles )  = module(n)%file(i)%uses
            End If

          End Do
        End Do
      End If     ! global

! If not global, build list from srcModule
      If ( .Not. global ) Then
        Do n = 1, srcModule%nfiles

          ! Skip if a global_module
          If ( srcModule%file(n)%global_module ) Cycle

          ! Check if file is a source file
          isSrc = .False.
          Do i = 1, Size( extension )
            If ( Index( srcModule%file(n)%name, extension(i) ) .Gt. 0 ) Then
              isSrc = .True.
              Exit
            End If
          End Do

          ! Skip if not a source file
          If ( .Not. isSrc ) Cycle

          nfiles = nfiles + 1
          filename( nfiles ) = srcModule%file(n)%name
          modname( nfiles )  = srcModule%file(n)%mod_name
          usename( nfiles )  = srcModule%file(n)%uses
        End Do  !  file loop
      End If   ! not global

! Count number of module files
      nmodfiles = 0
      Do n = 1, nfiles
        If ( modname(n) .Ne. ' ' ) nmodfiles = nmodfiles + 1
      End Do

      If ( nmodfiles .Gt. 0 ) Then
        ! Sort file list so modules are at the top
        msort = nfiles
        Do
          sorted = .True.
          msort = msort - 1

          Do n = 1, msort
            If ( modname(n) .Eq. ' ' .And. modname(n+1) .Ne. ' ' ) Then
              filetemp = filename(n)
              modtemp  = modname(n)
              usetemp  = usename(n)

              filename(n) = filename(n+1)
              modname(n)  = modname(n+1)
              usename(n)  = usename(n+1)

              filename(n+1) = filetemp
              modname(n+1)  = modtemp
              usename(n+1)  = usetemp
              sorted = .False.
            End If
          End Do

          If ( sorted ) Exit
        End Do    ! end of sort loop to put modules at top

! Sort module files where modules are before uses
        Do
          sorted = .True.

          Do n = 1, nmodfiles-1       ! use name loop
            Do m = n+1, nmodfiles     ! module name loop

              modtemp = ':' // Trim(modname(m)) // ':'

              If ( Index(usename(n), Trim(modtemp) ) .Gt. 0 ) Then
                filetemp = filename(n)
                modtemp  = modname(n)
                usetemp  = usename(n)

                filename(n) = filename(m)
                modname(n)  = modname(m)
                usename(n)  = usename(m)

                filename(m) = filetemp
                modname(m)  = modtemp
                usename(m)  = usetemp
                sorted = .False.
                Exit
              End If
            End Do

            If ( .Not.sorted ) Exit
          End Do

          If ( sorted ) Exit
        End Do

      End If  ! contains module files

      Return
      End Subroutine orderfiles

!-------------------------------------------------------------------------------
!     Identify include file depEndencies of a source file
!-------------------------------------------------------------------------------
      Subroutine getIncDep( srcFile, incdep )

      Implicit None

! arguments
      Character(*) :: srcFile 
      Logical :: incdep(n_includes) 

! local variables
      Integer :: status
      Integer :: lfn = 25
      Integer :: n
      Character( REC_LEN ) :: record
      Character( FLD_LEN ) :: field

      incdep = .False.

! open source file
      Open ( unit=lfn, FILE=srcFile, STATUS='OLD', IOSTAT=status )
      If ( status .Ne. 0 ) Then
        Write(*,'("**ERROR** Cannot Open source file:",a)') Trim( srcFile )
        Return
      End If

! loop thru file and search for INCLUDE statements
      Do
        Read ( lfn,'(a)',iostat=status ) record
        If ( status .Ne. 0 ) Exit

        field = record
        Call LeftTrim( field )
        Call ucase( field )

        If ( field( 1:7 ) .Eq. 'INCLUDE' ) Then
      
          Do n = 1, n_includes
            If ( .Not. incdep(n) ) Then
              If ( Index( record,Trim(include(n)%name) ) .Gt. 0 ) incdep(n) = .True.
            End If
          End Do  ! includes loop
       
        End If  ! INCLUDE record

      End Do   ! read loop

      Close ( unit=lfn )

      Return
      End Subroutine getIncDep

      End Module ModelCfg
