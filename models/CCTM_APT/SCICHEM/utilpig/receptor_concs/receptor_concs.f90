program receptor_concs

USE CGRID_SPCS, only: gc_strt, ae_strt, nr_strt, tr_strt, &
                      CGRID_SPCS_INIT
USE PCGRID_DEFN           ! inherits GRID_CONF and CGRID_SPCS:NSPCSD
USE M3UTILIO            ! i/o-api

use common_puf
use multcomp_inc
use common_met
use files_inc
use diagnostics
use interface_definitions, only: INIT_PUFF_VAL_PIG

!  This program calculates concentrations at specified receptor locations
!  using the grid model concentrations as the background and point values
!  from the SCIPUFF %puf file

IMPLICIT NONE

! Max. No. of sampler locations
INTEGER, PARAMETER :: MAXSMP = 200

character(80), DIMENSION(MAXSMP):: smpfiles
character(80) :: fmt
character(80) pfile

character(3) :: filenum

CHARACTER( 16 ) :: CONC_IN = 'CTM_CONC_1'
CHARACTER*512  SAMP_OUT

INTEGER         TSTEP     !  output time step, format HHMMSS
real            tmstep    !  output time step, format hours
character(4)  matname

REAL, POINTER :: CGRID( :,:,:,: )

real dum, hp, dp, zp, dum1, dum2, time, btime, tx
integer nsamp,nsp,ios,i,j,nch,nout,nblank,JDATE,JTIME
integer nchp,k,ii,ij,ijk,nhrs,ihr,ipuf,isam
integer nspec
character(16) varname(MAX_MC),spname(MAX_MC)
integer, dimension(MAX_MC) :: mapspc
! integer, dimension(nhrs) :: JDATE1,JTIME1
integer, dimension(:),allocatable :: JDATE1,JTIME1

real,dimension(:,:,:),allocatable :: psmp
real,dimension(:,:),allocatable :: xs
real,dimension(:),allocatable :: csmp
real :: xsmp, ysmp, zsmp
integer ix, iy, iz
real ppmfactor

INTEGER :: NPROCS, ERR, BEGTIME

CHARACTER( 16 ) :: PNAME = 'RECEPTOR_CONCS'
CHARACTER( 96 ) :: XMSG = ' '
CHARACTER( 32 ) :: CONC_SPCS   = 'CONC_SPCS'

logical IsMCParticle

INTERFACE

   SUBROUTINE READ_CGRID( CONCFILE,CGRID,JDATE,JTIME )
      IMPLICIT NONE
      CHARACTER( 16 ) :: CONCFILE
      REAL :: CGRID( :,:,:,: )
      INTEGER :: JDATE     !  current model date, coded YYYYDDD
      INTEGER :: JTIME     !  current model time, coded HHMMSS
   END SUBROUTINE READ_CGRID

   SUBROUTINE GET_ENVLIST ( ENV_VAR, NVARS, VAL_LIST )
      IMPLICIT NONE
      CHARACTER( * ),  INTENT ( IN )  :: ENV_VAR
      INTEGER,         INTENT ( OUT ) :: NVARS
      CHARACTER( 16 ), INTENT ( OUT ) :: VAL_LIST( : )
   END SUBROUTINE GET_ENVLIST

END INTERFACE

! Get number of species, and starting indices for CGRID array.

IF ( .NOT. CGRID_SPCS_INIT() ) THEN
   XMSG = 'Error in CGRID_SPCS:CGRID_SPCS_INIT'
   CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2  )
END IF

!debug
!debug write(*,*)'NSPCSD, GC_STRT, AE_STRT, NR_STRT, TR_STRT: ', &
!debug            NSPCSD, GC_STRT, AE_STRT, NR_STRT, TR_STRT
!debug

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

! Initialize PCGRID

IF ( .NOT. PCGRID_INIT() ) THEN
   XMSG = 'Failure defining CGRID pointer'
   CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2  )
END IF
      
CGRID => PCGRID(1:MY_NCOLS,1:MY_NROWS,:,:)

! --- Initialize SCICHEM horizontal grid parameters and allocate
! --- dynamic arrays
call init_pig_grid

if (nError /= NO_ERROR) then
  call report_error
  go to 9999
end if

call set_version(iversion_code)

!------ Read Input

call input1(matname,JDATE,JTIME,TSTEP,time,nhrs,pfile,nchp,nsp,nsamp)
if (nError /= NO_ERROR) then
  call report_error
  go to 9999
end if

if (nsp > MAXSMP) then
  write(6,*)'Error: No. of sampler locations ',nsp,' > Max allowed: ',MAXSMP
  goto 9999
end if

allocate(JDATE1(nhrs), stat = ios)
if (ios /= 0) then
  write(6,*)'Error allocating JDATE1'
  write(6,*)'Bytes requested =',nhrs*4
  write(6,*)'Number of hours =',nhrs
  goto 9999
end if

allocate(JTIME1(nhrs), stat = ios)
if (ios /= 0) then
  write(6,*)'Error allocating JTIME1'
  write(6,*)'Bytes requested =',nhrs*4
  write(6,*)'Number of hours =',nhrs
  goto 9999
end if

allocate(psmp(nspectot+2,nsp,nhrs), stat = ios)
if (ios /= 0) then
  write(6,*)'Error allocating psmp'
  write(6,*)'Bytes requested =',(nspectot+2)*nsp*nhrs*4
  write(6,*)'Number of samplers =',nsp
  goto 9999
end if

allocate(csmp(nspectot+2), stat = ios)
if (ios /= 0) then
  write(6,*)'Error allocating csmp'
  write(6,*)'Bytes requested =',(nspectot+2)*4
  goto 9999
end if

allocate(xs(3,nsp), stat = ios)
if (ios /= 0) then
  write(6,*)'Error allocating xs'
  write(6,*)'Bytes requested =',3*nsp*4
  write(6,*)'Number of samplers =',nsp
  goto 9999
end if

call input2(xs,nsp,nsamp)

TMSTEP = FLOAT( TIME2SEC( TSTEP ) )/3600.

!debug
!debug write(6,*)'list of SCICHEM species'
do i = 1, nspectot
  varname(i) = species(i)%name
!debug   write(6,*) i,varname(i)
end do
!debug call flush(6)
!debug

! --- Read names of species to be saved in sampler output
!debug
!debug write(6,*)'calling get_envlist'
!debug write(6,*)'CONC_SPCS: ',CONC_SPCS
!debug call flush(6)
!debug
CALL GET_ENVLIST ( CONC_SPCS, NSPEC, SPNAME )
!debug
!debug write(6,*)'nspec: ',nspec
!debug call flush(6)
!debug write(6,*)'list of output species'
!debug call flush(6)
!debug do i = 1, nspec
!debug   write(6,*) i,spname(i)
!debug end do
!debug call flush(6)
!debug

! --- Check if species are available
mapspc = 0
do i = 1, nspec
  do j = 1, nspectot
    if (TRIM(spname(i)) == TRIM(varname(j))) then
      mapspc(i) = j
      EXIT
    end if
  end do
end do

do i = 1, nspec
  if (mapspc(i) == 0) then
    write(*,*)'Error: Species ',spname(i),' is not a model species'
    go to 9999
  end if
!debug
!debug  write(*,*)'i,mapspc: ',i,mapspc(i)
!debug

end do

! ---- Open NetCDF files
CALL OPEN_FILES( CONC_IN,JDATE,JTIME,NHRS,NSPEC,SPNAME )

! ---- Open sampler output files, 1 for each sampler
!do isam = 1,nsp
!  write(filenum,'(I3)') isam
!  filenum = ADJUSTL(filenum) 
!  smpfiles(isam) = 'Sampler_'//TRIM(filenum)//'.smp'
!  open(100+isam,FILE=smpfiles(isam),form='FORMATTED',STATUS='NEW')
!  write(100+isam,1000)(ADJUSTR(spname(i)),i=1,nspec)
!end do

call GETENV('SAMP_OUT_1',SAMP_OUT)
open(101,FILE=SAMP_OUT,form='FORMATTED',STATUS='NEW')
write(101,1000)(ADJUSTR(spname(i)),i=1,nspec)

!------ Read grid file and puff file for each hour

!debug
!debug write(6,*)'xmin,ymin,dxb,dyb,nxb,nyb,nzb,nxyb: ',xmin,ymin,dxb,dyb,nxb,nyb,nzb,nxyb
!debug call flush(6)
!debug

psmp = 0.0
btime = time
do ihr = 1, nhrs

  write(6,*) 'ihr,time: ',ihr,time

  CALL READ_CGRID( CONC_IN,CGRID,JDATE,JTIME )

  write(6,*) 'SCIPUFF time, JTIME:',time,JTIME
  call flush(6)
  
!---- Merge concs only after 1st time step
  if (time > 0.) then
!debug write(6,*) 'pfile =',pfile(1:nchp) 
!debug call flush(6)
    call read_puff(pfile(1:nchp),time,tx)
!debug write(6,*)'finished read_puff'
!debug call flush(6)
    if (time > tx) goto 9999

!debug write(6,*)'calling updmet'
!debug call flush(6)
    CALL UPDMET( JDATE, JTIME, TSTEP )
!debug write(6,*)'finished updmet'
!debug call flush(6)

!   psmp = 0.0

! Loop over samplers
!debug
!debug    write(6,*)'nsp: ',nsp
!debug    call flush(6)
!debug
    do isam = 1,nsp
      xsmp = xs(1,isam)
      ysmp = xs(2,isam)
      zsmp = xs(3,isam)
!debug
!debug      write(6,*)'xsmp,ysmp,zsmp: ',xsmp,ysmp,zsmp
!debug      call flush(6)
!debug
! ---- find grid cell containing sampler
      ix = MIN(MAX(1,INT((xsmp - xmin)/dxb) + 1),nxb)
      iy = MIN(MAX(1,INT((ysmp - ymin)/dyb) + 1),nyb)
!debug
!debug      write(6,*)'ix,iy: ',ix,iy
!debug      call flush(6)
!debug

! -- transform z
      if (lter) then
        call get_topog(xsmp,ysmp,hp,dum1,dum2)
        dp = 1. - hp/zbtop
        zp = (zsmp - hp)/dp
      else
        zp = zsmp
      end if
!debug
!debug      write(6,*)'hp,zbtop,dp,zp: ',hp,zbtop,dp,zp
!debug      call flush(6)
!debug

      do i = 1,nzb - 1
        if (zp >= zbw(i) .and. zp < zbw(i+1)) then
          iz = i
          go to 45
        end if
      end do

      iz = nzb

45    continue
!debug
!debug      write(6,*)'iz: ',iz
!debug      call flush(6)
!debug

! - get the met
      ij  = (iy-1)*nxb + ix
      ijk = (iz-1)*nxyb + ij
!debug
!debug      write(6,*)'ij,ijk: ',ij,ijk
!debug      call flush(6)
!debug
      tb = t_ua(ijk)
      pb = p_ua(ijk)
!debug
!debug      write(6,*)'tb,pb: ',tb,pb
!debug      call flush(6)
!debug
      tab = tb*(pb**0.285714)
      ppmfactor = tab/(298.*pb)  !scale STP conc to cgrid ppm
!debug
!debug      write(6,*)'tab,ppmfactor: ',tab,ppmfactor
!debug      call flush(6)
!debug

! Loop over puffs to calculate puff contributions at each receptor
!debug
!debug      write(6,*)'npuf: ',npuf
!debug      call flush(6)
!debug
      do ipuf = 1, npuf

        tb = 298.
        pb = 1.
!debug
!debug        write(6,*)'calling init_puff_val_pig for puf ',ipuf
!debug        call flush(6)
!debug
        call init_puff_val_pig(puff(ipuf),CGRID)
!debug
!debug        write(6,*)'calling get_puff_val_pig for puf ',ipuf
!debug        call flush(6)
!debug
        call get_puff_val_pig(xsmp,ysmp,zsmp,puff(ipuf),csmp)
!debug
!debug        write(6,*)'finished get_puff_val_pig for puf ',ipuf
!debug        call flush(6)
!debug
        do i = 1, nspectot + 2
          psmp(i,isam,ihr) = psmp(i,isam,ihr) + csmp(i)
        end do
      end do

! Add background (host model concentration)
      do i = 1, nspectot

        ii = i_spec_list(i)
        if (IsMCParticle(i)) then
          psmp(i,isam,ihr) = psmp(i,isam,ihr) + cgrid(ix,iy,iz,ii)
        else
          psmp(i,isam,ihr) = psmp(i,isam,ihr)*ppmfactor + cgrid(ix,iy,iz,ii)
        end if

      end do
!     write(101,1200)isam,JDATE,JTIME,(psmp(mapspc(i),isam),i=1, nspec)
      JDATE1(ihr)=JDATE
      JTIME1(ihr)=JTIME
    end do !isam
  end if  !time > 0.

  time = time + tmstep
  CALL NEXTIME( JDATE, JTIME, TSTEP )
  
end do !nhrs

!write out samples at receptor locations
do isam = 1,nsp
  time = btime
  do ihr = 1, nhrs
    if (time > 0.) then
      write(101,1200)isam,JDATE1(ihr),JTIME1(ihr),(psmp(mapspc(i),isam,ihr),i=1, nspec)
    end if
    time = time + tmstep
  end do
end do

IF ( .NOT. CLOSE3( CONC_IN ) ) THEN

   XMSG = 'Could not close ' // CONC_IN // ' file'
   CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

END IF

9999 continue

if(nout /= 6) close(unit=nout,iostat=ios)

1000 format('Receptor #',5x,'Date',4x,'Time',200A)
1200 format(I10,2x,I7,2x,I6,200(4x,E12.5))

stop

end

!===============================================================================

subroutine read_puff(pfile,time,tx)
use common_puf
use multcomp_inc

character*(*) pfile
real amb(MAX_MC)

logical, save :: firstime = .TRUE.

if (firstime) then
!------ open input file

  open(unit=1,file=pfile,status='OLD',action="read", &
       form='unformatted',err=9000)

!------ find time

  tx = -999.
  told = tx

  do while (tx < time)
    told = tx
    read(1,end=10) t,npuf
    tx = t/3600.
!debug
    write(*,*)'tx,npuf: ',tx,npuf
!debug
  end do
  go to 11
10  backspace(1)
  backspace(1)
  read(1) t,npuf
  write(6,*) 't = ', t, 'npuf = ',npuf
  tx = t/3600.

11  if (abs(told-time) < abs(tx-time)) backspace(1)
  backspace(1)

  firstime = .FALSE.

end if

read(1,end=503) t,npuf &
                ,(puff(ipuf),ipuf=1,npuf) &
                ,npaux,(puff_aux(i),i=1,npaux-1) &
                ,nspectot,(amb(i), i = 1, nspectot)
tx = t/3600.

do i = 1, nspectot
  species(i)%amb = 0. !not used
end do

write(6,*)'Read puff file at t=',tx
write(6,*)'Read',npuf,' puffs'

return

503     write(6,*)'****ERROR - EOF encountered reading puff file'
stop

9000    write(6,*)'****ERROR - Opening Puff file ',pfile
stop

end
!===============================================================================

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
