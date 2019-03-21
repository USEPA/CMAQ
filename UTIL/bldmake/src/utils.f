!-----------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in    !
!  continuous development by various groups and is based on information !
!  from these groups: Federal Government employees, contractors working !
!  within a United States Government contract, and non-Federal sources  !
!  including research institutions.  These groups give the Government   !
!  permission to use, prepare derivative works of, and distribute copies!
!  of their work in the CMAQ system to the public and to permit others  !
!  to do so.  The United States Environmental Protection Agency         !
!  therefore grants similar permission to use the CMAQ system software, !
!  but users are requested to provide copies of derivative works or     !
!  products designed to operate in the CMAQ system to the United States !
!  Government without restrictions as to use by others.  Software       !
!  that is used with the CMAQ system but distributed under the GNU      !
!  General Public License or the GNU Lesser General Public License is   !
!  subject to their copyright restrictions.                             !
!-----------------------------------------------------------------------!

!-------------------------------------------------------------------------------
!     Routines for processing files using system commands
!     originally written in C by Steve Thorpe
!     rewritten in Fortran by Steve Howard
!     redone to meet CMAQ coding standards by Jeff Young
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!     Routine to copy files
!-------------------------------------------------------------------------------
      Subroutine copyfile( fromfile, tofile, status )

      Use ModelCFG

      Implicit None

      ! arguments
      Character(*) :: fromfile
      Character(*) :: tofile
      Integer :: status

      ! functions
      Integer system

      ! local variables
      Character( FLD_LEN ) :: cmdline

      cmdline = 'cp -fp ' // Trim(fromfile) // ' ' // Trim(tofile)

      status = system( Trim(cmdline) )
      If ( status .Ne. 0 ) Then
        Write(*,'("**ERROR** while running copyFile command:")')
        Write(*,'(5x,"From file:",a)') Trim( fromfile)
        Write(*,'(5x,"To file:  ",a)') Trim( tofile)
      End If

      Return
      End Subroutine copyfile

!-------------------------------------------------------------------------------
!     Routine to delete files 
!-------------------------------------------------------------------------------
      Subroutine deletefile( file, status )

      Use ModelCFG

      Implicit None

      ! arguments
      Character(*) :: file
      Integer :: status

      ! functions
      Integer system

      ! local variables
      Character( FLD_LEN ) :: cmdline

      cmdline = 'rm -f ' // Trim(file)

      status = system( Trim(cmdline) )
      If ( status .Ne. 0 ) Then
        Write(*,'("**ERROR** deleting file:",a/)') Trim(file)
      End If

      Return
      End Subroutine deletefile

!-------------------------------------------------------------------------------
!     Routine to remove directory
!-------------------------------------------------------------------------------
      Subroutine rmdir( dir, status )

      Use ModelCFG

      Implicit None

      ! arguments
      Character(*) :: dir 
      Integer :: status

      ! functions
      Integer system

      ! local variables
      Character( FLD_LEN ) :: cmdline

      cmdline = 'rm -rf ' // Trim(dir) 

      status = system( Trim(cmdline) )
      If ( status .Ne. 0 ) Then
        Write(*,'("**ERROR** removing directory:",a/)') Trim(dir)
      End If

      Return
      End Subroutine rmdir      

!-------------------------------------------------------------------------------
!     Routine to run chmod to change file permissions 
!-------------------------------------------------------------------------------
      Subroutine chmod( file, arg, status )

      Use ModelCFG

      Implicit None

      ! arguments
      Character(*) :: file
      Character(*) :: arg  
      Integer :: status

      ! functions
      Integer system

      ! local variables
      Character( FLD_LEN ) :: cmdline

      cmdline = 'chmod ' // Trim(arg) // ' ' // Trim(file)

      status = system( Trim(cmdline) )
      If ( status .Ne. 0 ) Then
        Write(*,'("**ERROR** while running chmod command on file:",a/)') Trim(file)
      End If

      Return
      End Subroutine chmod

!-------------------------------------------------------------------------------
!     Routine to run which to get full path name of command
!-------------------------------------------------------------------------------
      Subroutine which( cmd, path, status )

      Use ModelCFG

      Implicit None

      ! arguments
      Character(*) :: cmd 
      Character(*) :: path
      Integer :: status

      ! functions
      Integer system

      ! local variables
      Character( FLD_LEN ) :: scrfile
      Character( FLD_LEN ) :: cmdline
      Integer :: lfn = 25

      cmdline = 'which ' // Trim(cmd)

      ! create filename for scratch file
      Call getSCRNAME( scrfile )

      ! run command using system function and direct output to scratch file
      status = system( Trim(cmdline) // ' > ' // Trim(scrfile) )
      If ( status .Ne. 0 ) Then
        Write(*,'("**ERROR** writing ",a," to ",a/)') Trim(cmdline), Trim(scrfile)
        Return
      End If

      ! open scratch file to capture checkout results
      Open (unit=lfn, FILE=scrfile, iostat=status)
      If ( status .Ne. 0 ) Then
        Write(*,'("**ERROR** opening scratch file to capture which results"/)')
        Stop
      End If

      ! read output to get path
      Read (lfn,'(a)',iostat=status) path   

      ! close and delete command output file
      Close (unit=lfn, status='delete')

      Return
      End Subroutine which

!-------------------------------------------------------------------------------
!     Routine to Return Present Working Directory
!-------------------------------------------------------------------------------
      Subroutine pwd( path, status )

      Use ModelCFG

      Implicit None

      ! arguments
      Character(*) :: path
      Integer :: status

      ! functions
      Integer system

      ! local variables
      Character( FLD_LEN ) :: scrfile
      Character( FLD_LEN ) :: cmdline
      Integer :: lfn = 25

      cmdline = 'pwd'

      ! create filename for scratch file
      Call getSCRNAME( scrfile )

      ! run command using system function and direct output to scratch file
      status = system( Trim(cmdline) // ' > ' // Trim(scrfile) )
      If ( status .Ne. 0 ) Then
        Write(*,'("**ERROR** writing ",a," to ",a/)') Trim(cmdline), Trim(scrfile)
        Return
      End If

      ! open scratch file to capture checkout results
      Open (unit=lfn, FILE=scrfile, iostat=status)
      If ( status .Ne. 0 ) Then
        Write(*,'("**ERROR** openning scratch file to capture pwd results"/)')
        Stop
      End If

      ! read output to get path
      Read (lfn,'(a)',iostat=status) path

      ! close and delete command output file
      Close (unit=lfn, status='delete')

      Return
      End Subroutine pwd   

!-------------------------------------------------------------------------------
!    Routine to run make using the current Makefile 
!-------------------------------------------------------------------------------
      Subroutine runMake( status )

      Use ModelCFG

      Implicit None

      ! arguments
      Integer :: status

      ! functions
      Integer system

      ! local variables
      Character( FLD_LEN ) :: cmdline

      cmdline = 'make'

      if ( debug_cctm ) then
         cmdline = trim(cmdline) // ' DEBUG=TRUE'
      end if

      if ( isam_cctm ) then
         cmdline = trim(cmdline) // ' ISAM=TRUE'
      end if

      status = system( Trim(cmdline) )
      If ( status .Ne. 0 ) Then
        Write(*,'("**ERROR** while running make command"/)')
      End If

      Return
      End Subroutine runMake

!-------------------------------------------------------------------------------
!     Subroutine that generates a random filename using time
!-------------------------------------------------------------------------------
      Subroutine getSCRNAME( name )

      Implicit None

      ! arguments
      Character(*) :: name

      ! local variables
      Integer ivalues(8) !! year, month, day, GMT_offset, hour, minutes, seconds, miliseconds

      Call DATE_AND_TIME( values=ivalues )

      Write(name,'("temp.",i3.3)') ivalues(8)

      Return
      End Subroutine getSCRNAME

