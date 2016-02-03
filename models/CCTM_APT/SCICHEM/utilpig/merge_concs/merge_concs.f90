program merge_concs

USE CGRID_SPCS, only: gc_strt, ae_strt, nr_strt, tr_strt, &
                      CGRID_SPCS_INIT
USE PCGRID_DEFN           ! inherits GRID_CONF and CGRID_SPCS:NSPCSD
USE M3UTILIO            ! i/o-api

!use host_chem_inc
!use host_inc
use common_puf
use multcomp_inc
use common_met
!use common_srf
use files_inc
use diagnostics
use interface_definitions, only: DUMP_PIG


!  This program merges concentrations at the grid points from
!  the SCIPUFF %puf file (and multicomponent species) with
!  the host model concentrations

IMPLICIT NONE

character*80 pfile

CHARACTER( 16 ) :: CONC_IN = 'CTM_CONC_1'
CHARACTER( 16 ) :: CONC_OUT = 'CTM_CONC_MERGED'

INTEGER         TSTEP     !  output time step, format HHMMSS
real            tmstep    !  output time step, format hours
LOGICAL, DIMENSION( : ), ALLOCATABLE :: ISSAVED ! Species saved flag
character*4  matname

REAL, POINTER :: CGRID( :,:,:,: )

real dum, hp, dp, time, tx
integer nsamp,ios,i,j,nch,nout,nblank,JDATE,JTIME
integer nchp,k,ij,nhrs,ihr,ipuf
character*16 varname(MAX_MC)

INTEGER :: NPROCS, ERR, BEGTIME

CHARACTER( 16 ) :: PNAME = 'MERGE_CONCS'
CHARACTER( 96 ) :: XMSG = ' '

! ... External Function (not already declared by IODECL3.EXT):
!INTEGER, EXTERNAL :: TIME2SEC

INTERFACE

   SUBROUTINE READ_CGRID( CONCFILE,CGRID,JDATE,JTIME,ISSAVED )
   IMPLICIT NONE
   CHARACTER( 16 ) :: CONCFILE
   REAL :: CGRID( :,:,:,: )
   INTEGER :: JDATE     !  current model date, coded YYYYDDD
   INTEGER :: JTIME     !  current model time, coded HHMMSS
   LOGICAL :: ISSAVED( * ) ! Flag if species saved/not saved
   END SUBROUTINE READ_CGRID

   SUBROUTINE WRITE_CGRID( OUTFILE,CGRID,JDATE,JTIME,ISSAVED )
   IMPLICIT NONE
   CHARACTER( 16 ) :: OUTFILE
   REAL :: CGRID( :,:,:,: )
   INTEGER :: JDATE     !  current model date, coded YYYYDDD
   INTEGER :: JTIME     !  current model time, coded HHMMSS
   LOGICAL :: ISSAVED( * ) ! Flag if species saved/not saved
   END SUBROUTINE WRITE_CGRID

END INTERFACE

! Get number of species, and starting indices for CGRID array.

IF ( .NOT. CGRID_SPCS_INIT() ) THEN
   XMSG = 'Error in CGRID_SPCS:CGRID_SPCS_INIT'
   CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2  )
!  CALL M3EXIT ( PNAME, 0, 0, XMSG, 1  )
END IF

!debug
write(*,*)'NSPCSD, GC_STRT, AE_STRT, NR_STRT, TR_STRT: ', &
           NSPCSD, GC_STRT, AE_STRT, NR_STRT, TR_STRT
!debug

allocate(ISSAVED(NSPCSD),STAT=IOS)
if (ios /= 0) then
   nError = SZ_ERROR
   eRoutine = PNAME
   EMESSAGE = 'INSUFFICIENT MEMORY TO ALLOCATE ISSAVED ARRAY'
   WRITE(EINFORM,*) 'BYTES REQUESTED =',NSPCSD*4
   call report_error
   GO TO 9999
end if

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

call input1(matname,JDATE,JTIME,TSTEP,time,nhrs,pfile,nchp)
if (nError /= NO_ERROR) then
  call report_error
  go to 9999
end if

TMSTEP = FLOAT( TIME2SEC( TSTEP ) )/3600.

ndump = 0

do i = 1, nspectot
  varname(i) = species(i)%name
  write(6,*) i,varname(i)
end do

! ---- Open NetCDF files
CALL OPEN_FILES( CONC_IN,CONC_OUT,JDATE,JTIME,NHRS )

!------ Read puff file

do ihr = 1, nhrs

  CALL READ_CGRID( CONC_IN,CGRID,JDATE,JTIME,ISSAVED )

  write(6,*) 'SCIPUFF time, JTIME:',time,JTIME
  
!---- Merge concs only after 1st time step
  if (time > 0.) then
    write(6,*) 'pfile =',pfile(1:nchp) 
    call read_puff(pfile(1:nchp),time,tx)
    if (time > tx) goto 9999

    CALL UPDMET( JDATE, JTIME, TSTEP )

    do ipuf = 1, npuf
      call dump_pig(puff(ipuf),CGRID)
      if (nError /= NO_ERROR) then
        write(6,*) eAction
        write(6,*) eInform
        write(6,*) eRoutine
        goto 9999
      end if
    end do

    write(lun_dgn) time,ndump,(emiss(i),statics(i),bndry(i),trans(i), &
                   ddepos(i),wdepos(i),chem(i),active(i),remvd(i), &
                   ppmfac(i),i=1,nspectot+1)
		     
  end if

  CALL WRITE_CGRID( CONC_OUT,CGRID,JDATE,JTIME,ISSAVED )

  time = time + tmstep
  CALL NEXTIME( JDATE, JTIME, TSTEP )
  
end do

CALL CLOSE_FILES( CONC_IN, CONC_OUT, JDATE, JTIME )
deallocate(ISSAVED)

9999 continue

if(nout /= 6) close(unit=nout,iostat=ios)

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
