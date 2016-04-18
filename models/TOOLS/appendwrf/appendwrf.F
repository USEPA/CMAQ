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


C****************************************************************************************
C     program appendwrf  appends a set of WRF netcdf files
C    
c      PROGRAM appendwrf.exe
c
C*****************************************************************************************

      Use WRF_FILES

      IMPLICIT NONE

C Include netcdf header file
      INCLUDE 'netcdf.inc'


C local variables
     
      Integer status
      Character*256 outfile
      Integer NCIDout
      Integer v
      Integer nvars

C... open input files
      Call OPEN_FILES()

C... create output file
      Call GETENV('OUTFILE', outfile)
      if( outfile .eq. ' ' ) then
        Write(*,'(//''**ERROR**  output file not defined'')')
        Stop
        endif

      ! open output file
      status = NF_CREATE(outfile, 0, NCIDout)
      if( status.ne.0 ) then
        write(*,'(''**ERROR** Cannot open create file:'',a)') TRIM(outfile)
        Stop
        endif
      Write(*,'(''OUTFILE ['',a,'']  created'')') TRIM(outfile)

      ! copy header info from NCID(1) to NCIDout
      Call COPY_HEADER( NCID(1), NCIDout )

      ! get number of variables to copy
      status = NF_INQ_NVARS( NCIDout, nvars )

      ! variable copy loop
      do v = 1, nvars
        Call COPY_DATA( NCIDout, v )
        enddo  ! variable copy loop

      status = NF_CLOSE(NCIDout)
      Call CLOSE_FILES()
      stop
      end


