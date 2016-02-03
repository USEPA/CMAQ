program merge_deps

USE PCGRID_DEFN           ! inherits GRID_CONF and CGRID_SPCS:NSPCSD
USE M3UTILIO            ! i/o-api

!use host_inc
use common_puf
use multcomp_inc
use common_met
use files_inc

!  This program calculates depositions at the grid points from
!  the SCIPUFF %dep file (and multicomponent species) and merges
!  them with the host model depositions

implicit none

character*80 dfile, wfile
      CHARACTER( 16 ) :: CTM_DDEP_IN  = 'CTM_DRY_DEP_1'
      CHARACTER( 16 ) :: CTM_WDEP_IN  = 'CTM_WET_DEP_1'
      CHARACTER( 16 ) :: CTM_DDEP_OUT = 'CTM_DDEP_MERGED'
      CHARACTER( 16 ) :: CTM_WDEP_OUT = 'CTM_WDEP_MERGED'

      INTEGER         TSTEP     !  output time step, format HHMMSS
      real            tmstep    !  output time step, format hours
character*4  matname
real time, tx
real time_restart

real timeprev, timetest

      REAL, DIMENSION(:,:,:),ALLOCATABLE :: DENS  ! air density (kg/m3)
      REAL, DIMENSION(:,:,:),ALLOCATABLE :: DDEP  ! dry dep
      REAL, DIMENSION(:,:,:),ALLOCATABLE :: WDEP  ! wet dep

      INTEGER         NVARSDD, NVARSWD

INTEGER :: NPROCS, ERR, BEGTIME

CHARACTER( 16 ) :: PNAME = 'MERGE_DEPS'
CHARACTER( 96 ) :: XMSG = ' '

integer ios,i,j,JDATE,JTIME
integer nchp,k,ij,nhrs,ihr
integer ndat1, ndat2, nwrt, iwrt, is, numlinesdata, nn
CHARACTER( 16 ), DIMENSION( MAX_MC ) :: VARNAME, DNAME, WNAME
real, dimension(:,:),allocatable :: ddepos2d_cur,ddepos2d_prv
real, dimension(:,:),allocatable :: wdepos2d_cur,wdepos2d_prv

! ... External Functions (not already declared by IODECL3.EXT):
!      INTEGER, EXTERNAL :: TIME2SEC
!      INTEGER, EXTERNAL :: GETEFILE

!start debug
integer nspecdfile,size
character*16 spdfile(1000)
integer ndat, ndum1, ndum2
!end debug

INTERFACE

   SUBROUTINE UPDMET( JDATE, JTIME, TSTEP, DENS )
   IMPLICIT NONE

   INTEGER, INTENT( IN ) :: JDATE     !  current model date, coded YYYYDDD
   INTEGER, INTENT( IN ) :: JTIME     !  current model time, coded HHMMSS
   INTEGER, INTENT( IN ) :: TSTEP     !  model time step, coded HHMMSS
   REAL,    INTENT( OUT ) :: DENS( :,:,: )  ! density (kg/m3)

   END SUBROUTINE UPDMET

   SUBROUTINE READ_GRIDDEP( DEPFILE,DEP,NDEPS,DEPNAME,JDATE,JTIME )
   IMPLICIT NONE

   CHARACTER( 16 ), INTENT( IN ) :: DEPFILE
   REAL,    INTENT( OUT ) :: DEP( :,:,: )  ! deposition (kg/hectare)
   INTEGER, INTENT( IN ) :: NDEPS     !  no. of deposition species
   CHARACTER( 16 ), INTENT( IN ) :: DEPNAME( NDEPS )
   INTEGER, INTENT( IN ) :: JDATE     !  current model date, coded YYYYDDD
   INTEGER, INTENT( IN ) :: JTIME     !  current model time, coded HHMMSS

   END SUBROUTINE READ_GRIDDEP

   subroutine read_scidep(dfile,wfile,ddepos2d_cur,ddepos2d_prv, &
                          wdepos2d_cur,wdepos2d_prv,time,tx)

   implicit none

   character*(*) dfile, wfile
   real time,tx
   real, dimension(:,:) :: ddepos2d_cur,ddepos2d_prv
   real, dimension(:,:) :: wdepos2d_cur,wdepos2d_prv

   end subroutine read_scidep

   subroutine sum_depos(ddepos2d_cur,wdepos2d_cur,dens,nvarsdd,dname,ddep, &
                        nvarswd,wname,wdep)

      use common_met
      use files_inc
      use multcomp_inc

   implicit none

!   real, INTENT( IN ) :: ddepos2d_cur(:,:), wdepos2d_cur(:,:)
!   REAL, INTENT( IN ) :: DENS( :,:,: )  ! density (kg/m3)
!   INTEGER, INTENT( IN ) :: NVARSDD, NVARSWD
!   CHARACTER( 16 ), INTENT( IN ), DIMENSION( MAX_MC ) :: DNAME, WNAME
!   REAL, INTENT( IN ) :: DDEP( :,:,: )
!   REAL, INTENT( IN ) :: WDEP( :,:,: )
      real, dimension(:,:) :: ddepos2d_cur, wdepos2d_cur
      REAL, DIMENSION(:,:,:) :: ddep, wdep
      REAL, DIMENSION(:,:,:) :: dens    ! air density (kg/m3)
      INTEGER         NVARSDD, NVARSWD
      CHARACTER( 16 ), DIMENSION( MAX_MC ) :: DNAME, WNAME

   end subroutine sum_depos

   SUBROUTINE WRITE_DEP( DEPFILE,DEP,NVARSD,DNAME,JDATE,JTIME )     

!     USE HOST_INC
      USE ERROR_INC
      USE M3UTILIO
      USE COMMON_MET
      USE GRID_CONF    ! horizontal & vertical grid definitions
      USE MULTCOMP_INC

   IMPLICIT NONE

!   CHARACTER( 16 ), INTENT( IN ) :: DEPFILE
!   REAL,    INTENT( IN ) :: DEP( :,:,: )  ! deposition (kg/hectare)
!   INTEGER, INTENT( IN ) :: NVARSD     !  no. of deposition species
!   CHARACTER( 16 ), INTENT( IN ), DIMENSION( MAX_MC ) :: DNAME
!   INTEGER, INTENT( IN ) :: JDATE     !  current model date, coded YYYYDDD
!   INTEGER, INTENT( IN ) :: JTIME     !  current model time, coded HHMMSS
      INTEGER         JDATE     !  current model date, coded YYYYDDD
      INTEGER         JTIME     !  current model time, coded HHMMSS

      REAL, DIMENSION( :,:,: ) :: DEP
      INTEGER :: NVARSD
      CHARACTER( 16 ), DIMENSION( MAX_MC ) :: DNAME
      CHARACTER( 16 ) :: DEPFILE

   END SUBROUTINE WRITE_DEP

END INTERFACE

CALL MPCOMM_INIT( NPROCS, MYPE, BEGTIME, ERR )

! set horizontal grid definition

NPROCS = 1
MYPE = 0

IF ( .NOT. GRID_INIT ( NPROCS, MYPE ) ) THEN
   nError = AB_ERROR
   eRoutine = PNAME
   eMessage = '*** Failure defining horizontal & verical domains'
   call report_error
   GO TO 9999
END IF

ALLOCATE(DENS(GL_NCOLS, GL_NROWS, NLAYS), STAT = IOS)
IF (IOS /= 0) THEN
   XMSG = 'Could not allocate DENS array '
   CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
END IF

! Allocate deposition arrays

! --- Initialize SCICHEM horizontal grid parameters and allocate
! --- dynamic arrays
call init_pig_grid

allocate(ddepos2d_cur(MAX2D,MAX_MC), stat = ios)
if (ios /= 0) then
   XMSG = 'Could not allocate DDEPOS2D_CUR array '
   CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
end if

allocate(ddepos2d_prv(MAX2D,MAX_MC), stat = ios)
if (ios /= 0) then
   XMSG = 'Could not allocate DDEPOS2D_PRV array '
   CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
end if

allocate(wdepos2d_cur(MAX2D,MAX_MC), stat = ios)
if (ios /= 0) then
   XMSG = 'Could not allocate WDEPOS2D_CUR array '
   CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
end if

allocate(wdepos2d_prv(MAX2D,MAX_MC), stat = ios)
if (ios /= 0) then
   XMSG = 'Could not allocate WDEPOS2D_PRV array '
   CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
end if

if (nError /= NO_ERROR) then
  call report_error
  go to 9999
end if

call set_version(iversion_code)

!------ Read Input

call input1(matname,JDATE,JTIME,TSTEP,time,nhrs,dfile,wfile,nchp)
if (nError /= NO_ERROR) then
  call report_error
  go to 9999
end if

TMSTEP = FLOAT( TIME2SEC( TSTEP ) )/3600.

!------ open deposition files

lun_ddp=1
open(unit=lun_ddp,file=dfile,status='OLD',iostat=ios,form='UNFORMATTED')

lun_wdp=2
open(unit=lun_wdp,file=wfile,status='OLD',iostat=ios,form='UNFORMATTED')

write(6,*) 'Reading deposition files'
nxyb = nxb*nyb
do i = 1, nspecies
  do ij = 1, nxyb
    ddepos2d(ij,i) = 0.
    ddepos2d_cur(ij,i) = 0.
    ddepos2d_prv(ij,i) = 0.
    wdepos2d(ij,i) = 0.
    wdepos2d_cur(ij,i) = 0.
    wdepos2d_prv(ij,i) = 0.
  end do
end do

!read(lun_ddp,end=45,err=9996)
!read(lun_ddp,err=9996)
!read(lun_wdp,end=45,err=9996)
!read(lun_wdp,err=9996)
read(lun_ddp,end=45,err=9996)nspecdfile,size
read(lun_ddp,err=9996)(spdfile(i),i=1,nspecdfile)
!start debug
write(*,*)'Dry dep scichem file'
write(*,*)'nspecdfile: ',nspecdfile,'; size: ',size
do i = 1,nspecdfile
  write(*,*)'species: ',spdfile(i)
end do
!end debug
read(lun_wdp,end=45,err=9996)nspecdfile,size
read(lun_wdp,err=9996)(spdfile(i),i=1,nspecdfile)
!start debug
write(*,*)'Wet dep scichem file'
write(*,*)'nspecdfile: ',nspecdfile,'; size: ',size
do i = 1,nspecdfile
  write(*,*)'species: ',spdfile(i)
end do
!end debug
    
! -- advance deposition file to start time

if (time > 1.00001) then
  timeprev = time - 1.0
  timetest = timeprev
else
  timeprev = 0.
  timetest = time
end if

tx = -999.
!start debug
write(*,*)'timetest,tx: ',timetest,tx
!end debug
do while (timetest > tx)
!start debug
  write(*,*)'reading time stamp on ddep file'
  call flush(6)
!end debug
  read(lun_ddp,end=45,err=9996) tx,ndat1
!start debug
  write(*,*)'Dry dep file tx: ',tx,ndat1
  write(*,*)'reading data record on ddep file'
  call flush(6)
!end debug
  nwrt = ndat1
  is = 0
  do while (nwrt > 0)
    iwrt = min(nwrt,10000)
    read(lun_ddp) (ij,i,ddepos2d(ij,i),nn=is+1,is+iwrt)
    write(*,*) 'DBG1 DDP ij, i = ', ij, i
    call flush(6)
    nwrt = nwrt - iwrt
    is   = is   + iwrt
  end do

!start debug
  write(*,*)'reading time stamp on wdep file'
  call flush(6)
!end debug
  read(lun_wdp,end=45,err=9996) tx, ndat2
!start debug
  write(*,*)'Wet dep file tx: ',tx,ndat2
  write(*,*)'reading data record on wdep file'
  call flush(6)
!end debug
  nwrt = ndat2
  is = 0
  do while (nwrt > 0)
    iwrt = min(nwrt,10000)
    read(lun_wdp) (ij,i,wdepos2d(ij,i),nn=is+1,is+iwrt)
    write(*,*) 'DBG1 WDP ij, i = ', ij, i
    call flush(6)
    nwrt = nwrt - iwrt
    is   = is   + iwrt
  end do
  write(*,*)'finished reading data record on wdep file'
  call flush(6)

end do

backspace(lun_ddp)
if(ndat1>0) then
numlinesdata = ceiling(ndat1/10000.)
do i=1,numlinesdata
backspace(lun_ddp)
enddo
endif

backspace(lun_wdp)
if(ndat2>0) then
numlinesdata = ceiling(ndat2/10000.)
do i=1,numlinesdata
backspace(lun_wdp)
enddo
endif

!start debug
write(*,*)'tx: ',tx
!end debug

! -- correction for merging files from restarted run
! -- done to subtract previous hour's cumulative deposition from this
! -- hour's cumulative deposition to get hourly deposition.
if (timeprev > 0.) then
  call read_scidep(dfile(1:nchp),wfile(1:nchp),ddepos2d_cur,ddepos2d_prv, &
                   wdepos2d_cur,wdepos2d_prv,timeprev,tx)
end if

do i = 1, nspectot
  varname(i) = species(i)%name
  write(6,*) i,varname(i)
end do

! ---- Open NetCDF files
CALL OPEN_FILES( CTM_DDEP_IN,CTM_WDEP_IN,CTM_DDEP_OUT,CTM_WDEP_OUT, &
                 JDATE,JTIME,NHRS,NVARSDD,NVARSWD,DNAME,WNAME)

ALLOCATE(DDEP(GL_NCOLS, GL_NROWS, NVARSDD), STAT = IOS)
IF (IOS /= 0) THEN
   XMSG = 'Could not allocate DDEP array '
   CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
END IF

ALLOCATE(WDEP(GL_NCOLS, GL_NROWS, NVARSWD), STAT = IOS)
IF (IOS /= 0) THEN
   XMSG = 'Could not allocate WDEP array '
   CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
END IF

!------ Read deposition file

do ihr = 1, nhrs
  
  write(6,*) 'SCIPUFF time, JTIME:',time,JTIME
  call flush(6)
  
  call read_scidep(dfile(1:nchp),wfile(1:nchp),ddepos2d_cur,ddepos2d_prv, &
                   wdepos2d_cur,wdepos2d_prv,time,tx)
  write(*,*) 'DBG 2: ihr, time, tx =',ihr, time, tx
  if (time > tx) go to 9999

  CALL UPDMET( JDATE, JTIME, TSTEP, DENS )

  CALL READ_GRIDDEP( CTM_DDEP_IN, DDEP, NVARSDD, DNAME, JDATE, JTIME )

  CALL READ_GRIDDEP( CTM_WDEP_IN, WDEP, NVARSWD, WNAME, JDATE, JTIME )
  
  call sum_depos(ddepos2d_cur,wdepos2d_cur,DENS,NVARSDD,DNAME,DDEP,NVARSWD, &
                 WNAME,WDEP)
  
  CALL WRITE_DEP( CTM_DDEP_OUT, DDEP, NVARSDD, DNAME, JDATE, JTIME )

  CALL WRITE_DEP( CTM_WDEP_OUT, WDEP, NVARSWD, WNAME, JDATE, JTIME )
  
  time = time + tmstep
  CALL NEXTIME( JDATE, JTIME, TSTEP )
  
end do

CALL CLOSE_FILES( CTM_DDEP_IN, CTM_WDEP_IN, CTM_DDEP_OUT, CTM_WDEP_OUT, &
                  JDATE, JTIME )

9999 continue

stop

9000 write(6,*) 'Error opening deposition file'
goto 9999
45 write(6,*) 'End of file encountered for deposition file'
goto 9999
9996 write(6,*) 'Error reading deposition file'
goto 9999
end

!============================================================================

subroutine read_scidep(dfile,wfile,ddepos2d_cur,ddepos2d_prv, &
                       wdepos2d_cur,wdepos2d_prv,time,tx)

use common_puf
use common_met
use files_inc
use multcomp_inc

implicit none
character*(*) dfile, wfile
real time,tx
integer ij,i,ndat,nn
integer ndat1, ndat2
integer nwrt, iwrt, is, numlinesdata
real xbuf1, xbuf2
real, dimension(:,:) :: ddepos2d_cur,ddepos2d_prv
real, dimension(:,:) :: wdepos2d_cur,wdepos2d_prv

read(lun_ddp,end=45,err=9996) tx,ndat1
!start debug
write(*,*)'DDP: tx,ndat1: ',tx,ndat1
call flush(6)
!end debug
nwrt = ndat1
  is = 0
  do while (nwrt > 0)
    iwrt = min(nwrt,10000)
    read(lun_ddp) (ij,i,ddepos2d(ij,i),nn=is+1,is+iwrt)
    write(*,*) 'DBG2 DDP tx, ij, i = ', tx, ij, i
    call flush(6)
    nwrt = nwrt - iwrt
    is   = is   + iwrt
  end do

read(lun_wdp,end=45,err=9996) tx,ndat2
write(*,*)'WDP: tx,ndat2: ',tx,ndat2
call flush(6)
nwrt = ndat2
  is = 0
  do while (nwrt > 0)
    iwrt = min(nwrt,10000)
    read(lun_wdp) (ij,i,wdepos2d(ij,i),nn=is+1,is+iwrt)
    write(*,*) 'DBG2 WDP tx, ij, i = ', tx, ij, i
    call flush(6)
    nwrt = nwrt - iwrt
    is   = is   + iwrt
  end do

nxyb = nxb*nyb
do i = 1, nspecies
  do ij = 1, nxyb
    ddepos2d_cur(ij,i) = ddepos2d(ij,i) - ddepos2d_prv(ij,i)
    ddepos2d_prv(ij,i) = ddepos2d(ij,i)
    wdepos2d_cur(ij,i) = wdepos2d(ij,i) - wdepos2d_prv(ij,i)
    wdepos2d_prv(ij,i) = wdepos2d(ij,i)
  end do
end do

write(6,*) 't = ', tx, 'ndat1, ndat2 = ',ndat1, ndat2
if (time .ne. tx) write(6,*) '*************time expected ',time

9999 return

45     write(6,*)'****ERROR - EOF encountered reading depostion file'
goto 9999
9996 write(6,*) 'Error reading deposition file'
goto 9999

end

!============================================================================

subroutine get_file(prompt,file,nch)

character*(*) prompt,file

write(6,100,advance='no')prompt
100     format(' ',a,' file : ')
call get_line(5,nch,file,ieof)
if(ieof /= 0)goto 2000
return
2000    write(6,2100,advance='no')prompt
2100    format(' ***** EOF getting ',a,' file')
3000    file = ' '
nch = 0
return
end
