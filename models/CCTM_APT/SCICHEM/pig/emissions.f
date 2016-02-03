!    Subroutine init_emissions updated April 2005 for CMAQ-APT-PM (PKK, AER)

      subroutine set_scn
!******************************************************************************
! Developed jointly by the ARAP Group of Titan Corporation, Princeton, NJ and 
! Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION:  Set-up source parameters
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              check_real
!
! REVISION HISTORY: 
!
!    Fortran 90 updates + V1601 SCICHEM Update, February 2004 (PKK, AER)
!
!******************************************************************************
 
! --- MODULES
 
      use common_puf
      use relparam_fd
      use multcomp_inc
      use emissions_inc

      implicit none
 
! --- LOCALS

      logical opready

      real    lognorm_mmd, lognorm_sigma, random_spread
      real    horiz_uncertainty, vert_uncertainty, tmp

      integer number_random, random_seed
      integer i

!------ defaults

      xrel     = DEF_VAL_R
      yrel     = DEF_VAL_R
      zrel     = 0.
      trel     = 0.
      urel     = 0.
      vrel     = 0.
      wrel     = 0.
      tdur     = tdur_emit
      cmass    = 100.0
      wmom     = 0.
      buoy     = 0.
      size_rel = 0.
      subgroup = 1

      rel_param = NOT_SET_R
      rel_dist  = NOT_SET_I
      rel_mc = 0.

      reltyp      = ' '
      relmat      = 'UNKNOWN'
      name_rel    = ' '
      name_prime  = ' '

      opid = 0
      opmod = 0

      lognorm_mmd   = NOT_SET_R
      lognorm_sigma = NOT_SET_R
      number_random = NOT_SET_I
      random_spread = NOT_SET_R
      random_seed   = NOT_SET_I

      horiz_uncertainty = 0.0
      vert_uncertainty  = 0.0

      if (dynamic) then
        reltyp = 'CS'    !treat plume rise dynamically
      else
        reltyp = 'CSPR' !use plume rise formula
      end if
      relmat = 'TRAC'

      if (tdur <= 0.) then
         eMessage = 'Must set TDUR for RELTYP=C in scenario input'
         go to 9998
      end if
      if (tdur/=DEF_VAL_R) tdur = tdur*3600.

      if (lognorm_mmd /= NOT_SET_R) then
         rel_param(REL_MMD_INDX)   = lognorm_mmd
         rel_param(REL_SIGMA_INDX) = lognorm_sigma
         call check_real(lognorm_mmd,1.e-30,1.e30,'Lognormal MMD')
         call check_real(lognorm_sigma,1.,1.e30,'Lognormal sigma')
      end if
      if (number_random /= NOT_SET_I) then
         rel_param(REL_RAND_INDX)   = float(number_random)
         rel_param(REL_SPREAD_INDX) = random_spread
         if (random_seed == NOT_SET_I) then
            rel_param(REL_SEED_INDX) = NOT_SET_R
         else
            rel_param(REL_SEED_INDX) = float(random_seed)
         end if
         call check_real(random_spread,0.,1.e30,'Random spread')
      end if

      rel_param(REL_H_UNC_INDX) = horiz_uncertainty
      rel_param(REL_V_UNC_INDX) = vert_uncertainty
      call check_real(horiz_uncertainty,0.0,1.e30,
     &                'Horizontal source location uncertainty')
      call check_real(vert_uncertainty,0.0,1.e30,
     &                'Vertical source location uncertainty')

9999  return

!------ set read errors and go to return

9998  continue
      nError   = RD_ERROR
      eRoutine = 'set_scn'
      write(eAction,'(a,f8.2)') 'Release time TREL=',trel
      go to 9999

      end

      subroutine get_scn( JDATE, JTIME, MSTEP )
!******************************************************************************
!
! FUNCTION: Get the emissions for the next time or source
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES AND INCLUDES
 
      use common_puf
      use relparam_fd
      use multcomp_inc
      use emissions_inc
      use host_inc

      implicit none

! --- ARGUMENTS
 
      INTEGER         JDATE     !  current model date, coded YYYYDDD
      INTEGER         JTIME     !  current model time, coded HHMMSS
      INTEGER         MSTEP     !  model time step,  format HHMMSS

! --- LOCALS

      CHARACTER( 16 )  :: PNAME = 'GET_SCN'
      CHARACTER( 120 ) :: XMSG = ' '
      CHARACTER( 16 )  :: PING_MEPSE = 'PING'

      integer i

! --- read emissions
!debug
!      write(*,*)'in get_scn: vsulf,vpso4: ',vsulf,vpso4
!      write(*,*)'in get_scn: nemit, nsrc: ',nemit,nsrc
!debug
      do i = 1, nemit

         if (index_host_emit(i) == 0) then
!debug
!            write(*,*)'skipping ',nameemit(i)
!debug
            cycle
         end if
!debug
!      write(*,*)'reading ',nameemit(i),' emissions'
!debug
         IF ( .NOT. READ3( PING_MEPSE, nameemit(i), 1,
!    &        JDATE, JTIME, QSRC(1,i) ) ) THEN
     &        stkdate, JTIME, QSRC(1,i) ) ) THEN

            XMSG = 'Could not read ' // nameemit(i) // 'from '
     &             // TRIM( PING_MEPSE )
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF      !  if READ3 failed

      end do
!debug
!      write(*,*)'in get_scn: vsulf,vpso4: ',vsulf,vpso4
!debug

      return

      end

      subroutine init_emissions(JDATE,JTIME,TSTEP)
!******************************************************************************
!
! FUNCTION:  Initialize the emissions
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!    Updated April 2005 for CMAQ-APT-PM (PKK, AER)
!
!******************************************************************************
 
! --- MODULES AND INCLUDES
 
      use common_puf
      use multcomp_inc
      use emissions_inc
      use host_inc
      use aero_data, only: aerospc,N_AEROSPC,aso4_idx

      implicit none

! --- ARGUMENTS
 
      INTEGER         JDATE     !  current model date, coded YYYYDDD
      INTEGER         JTIME     !  current model time, coded HHMMSS
      INTEGER         TSTEP     !  model time step,  format HHMMSS

! --- LOCALS

      INTEGER :: LOGDEV           ! FORTRAN unit number for log file
      LOGICAL :: DOPRIME          ! flag for PRIME calculations, default = [F]
      INTEGER :: STATUS           ! ENV... status

      CHARACTER( 80 )  :: VARDESC  ! environment variable description

      CHARACTER( 16 )  :: APT_PRIME = 'APT_PRIME'  !  env. variable for PRIME

      CHARACTER( 16 )  :: BLD_PRIME = 'BLD_PRIME'  !  env. variable for file
                            ! containing file names of building data for PRIME
      CHARACTER( 256 ) :: PRIMEFILNM

      CHARACTER( 16 )  :: BLD_DIR   = 'BLD_DIR'  !  env. variable for directory
                            ! containing individual building data files
      CHARACTER( 60  ) :: BUILDDIRNM
      INTEGER          :: PRUNIT
      INTEGER          :: IOST             ! i/o status code
      LOGICAL          :: eof
      INTEGER          :: SRCID
      CHARACTER( 256 ) :: BLD_FILE
      INTEGER          :: PRIMESRC = 0     ! NO. OF PRIME SOURCES

      integer i, j
      integer isrc    !current source
!     integer emitspc !emitted species

      real dtstep

      CHARACTER( 16 )  :: PNAME = 'INIT_EMISSIONS'
      CHARACTER( 16 )  :: STACK_MEPSE = 'PING0'
      CHARACTER( 16 )  :: PING_MEPSE = 'PING'
      CHARACTER( 120 ) :: XMSG = ' '

      INTEGER :: SDATE, STIME

      integer :: astat

! ... External Functions (not already declared by IODECL3.EXT):
!     INTEGER, EXTERNAL :: TIME2SEC
!     LOGICAL, EXTERNAL :: ENVYN
!     INTEGER, EXTERNAL :: JUNIT              ! used to get next IO unit #
!
! Above now from host_inc, which includes m3utilio 

      write(6,*) 'init_emissions'

!------ open PiG stack data file

      IF ( .NOT. OPEN3( STACK_MEPSE, FSREAD3, PNAME ) ) THEN

         XMSG = 'Could not open ' // TRIM( STACK_MEPSE ) // ' file'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF

      IF ( .NOT. DESC3( STACK_MEPSE ) ) THEN

         XMSG = 'Could not get ' // TRIM( STACK_MEPSE ) // ' file description'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF              !  error abort if DESC3 failed

! --- get number of sources

      nsrc = NROWS3D
      SDATE = SDATE3D
      STIME = STIME3D
!debug
      write(*,*)'nsrc: ',nsrc
      write(*,*)'sdate,stime: ',sdate,stime
      write(*,*)'jdate,jtime: ',jdate,jtime
!debug
!----- read stack info
!----- first allocate stack arrays
      ALLOCATE ( XSRC( nsrc ), YSRC( nsrc ), ZSRC( nsrc ), STAT = ASTAT )
      if (astat /= 0) then
         XMSG = 'Could not allocate memory for stack coordinates '
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

      ALLOCATE ( IDSRC( nsrc ), PING_SRC( nsrc ), STAT = ASTAT )
      if (astat /= 0) then
         XMSG = 'Could not allocate memory for stack id or PinG flag'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

      ALLOCATE ( DSRC( nsrc ), TSRC( nsrc ),WSRC( nsrc ), STAT = ASTAT )
      if (astat /= 0) then
         XMSG = 'Could not allocate memory for stack parameters'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

      IF ( .NOT. READ3( STACK_MEPSE, 'XLOCA', ALLAYS3,
     &      SDATE, STIME, XSRC ) ) THEN
         XMSG = 'Could not read XLOCA from ' // TRIM( STACK_MEPSE )
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF      !  if READ3 failed

      IF ( .NOT. READ3( STACK_MEPSE, 'YLOCA', 1,
     &      SDATE, STIME, YSRC ) ) THEN
         XMSG = 'Could not read YLOCA from ' // TRIM( STACK_MEPSE )
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF      !  if READ3 failed

      IF ( .NOT. READ3( STACK_MEPSE, 'STKHT', 1,
     &      SDATE, STIME, ZSRC ) ) THEN
         XMSG = 'Could not read STKHT from ' // TRIM( STACK_MEPSE )
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF      !  if READ3 failed

      IF ( .NOT. READ3( STACK_MEPSE, 'ISTACK', 1,
     &      SDATE, STIME, IDSRC ) ) THEN
         XMSG = 'Could not read ISTACK from ' // TRIM( STACK_MEPSE )
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF      !  if READ3 failed

      IF ( .NOT. READ3( STACK_MEPSE, 'LPING', 1,
     &      SDATE, STIME, PING_SRC ) ) THEN
         XMSG = 'Could not read LPING from ' // TRIM( STACK_MEPSE )
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF      !  if READ3 failed

! --- read stack diameter, temperature and velocity
      IF ( .NOT. READ3( STACK_MEPSE, 'STKDM', 1,
     &      SDATE, STIME, DSRC ) ) THEN
         XMSG = 'Could not read STKDM from ' // TRIM( STACK_MEPSE )
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF      !  if READ3 failed

      IF ( .NOT. READ3( STACK_MEPSE, 'STKTK', 1,
     &      SDATE, STIME, TSRC ) ) THEN
         XMSG = 'Could not read STKTK from ' // TRIM( STACK_MEPSE )
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF      !  if READ3 failed

      IF ( .NOT. READ3( STACK_MEPSE, 'STKVE', 1,
     &      SDATE, STIME, WSRC ) ) THEN
         XMSG = 'Could not read STKVE from ' // TRIM( STACK_MEPSE )
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF      !  if READ3 failed

! --- Check flag for PRIME calculations
      DOPRIME = .FALSE.         ! default
      VARDESC = 'Flag for using PRIME for building downwash'
      DOPRIME = ENVYN( APT_PRIME, VARDESC, DOPRIME, STATUS )
      LOGDEV = INIT3 ()
      IF ( STATUS /= 0 ) THEN
         WRITE( LOGDEV, '(5X, A)' ) VARDESC
      ELSE
         WRITE( LOGDEV, * )'    Will use PRIME for downwash'
      END IF
      IF ( STATUS == 1 ) THEN
         XMSG = 'Environment variable improperly formatted'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
      ELSE IF ( STATUS == -1 ) THEN
         XMSG = 'Environment variable set, but empty ... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         WRITE( LOGDEV, * )'    Will not use PRIME for downwash'
      ELSE IF ( STATUS == -2 ) THEN
         XMSG = 'Environment variable not set ... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         WRITE( LOGDEV, * )'    Will not use PRIME for downwash'
      END IF

! --- If PRIME option is selected, then read name of file that
! --- contains file names of building data files for sources of
! --- interest

! --- Initialize PRIME building data (output of BPIP) file names
      ALLOCATE ( BFILE_SRC( nsrc ), STAT = ASTAT )
      if (astat /= 0) then
         XMSG = 'Could not allocate memory for PRIME building data files'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
      bfile_src = ' '

      IF ( DOPRIME ) THEN

         VARDESC = 'PRIME File Name'
         CALL ENVSTR( BLD_PRIME, VARDESC, PNAME, PRIMEFILNM, STATUS )
         IF ( STATUS /= 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS == 1 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         ELSE IF ( STATUS == -1 ) THEN
            XMSG = 'Environment variable set, but empty ... no default:'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         ELSE IF ( STATUS == -2 ) THEN
            XMSG = 'Environment variable not set ... no default:'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

         VARDESC = 'PRIME Directory Name'
         CALL ENVSTR( BLD_DIR, VARDESC, PNAME, BUILDDIRNM, STATUS )
         IF ( STATUS /= 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS == 1 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         ELSE IF ( STATUS == -1 ) THEN
            XMSG = 'Environment variable set, but empty ... '//
     &             'Using current directory'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
            BUILDDIRNM = ' '
         ELSE IF ( STATUS == -2 ) THEN
            XMSG = 'Environment variable not set ... '//
     &             'Using current directory'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
            BUILDDIRNM = ' '
         END IF

! --- Open and Read BPIP/PRIME file name "bfile_src" for each stack source 
         PRUNIT = JUNIT( )
         OPEN ( UNIT = PRUNIT, FILE = TRIM(PRIMEFILNM), STATUS = 'OLD',
     &          IOSTAT = IOST )
         IF ( IOST /= 0 ) THEN
            XMSG = 'Error opening PRIME file '//TRIM(PRIMEFILNM)
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         eof = .false.
         DO WHILE (.not. eof)

            READ (PRUNIT,*,END=999) SRCID, BLD_FILE
            do isrc = 1, nsrc
               if (idsrc(isrc) == srcid) then
                  if (LEN_TRIM(bfile_src(isrc)) /= 0) then
                     write(XMSG,9100)SRCID
                     go to 2400
                  end if
                  if (LEN_TRIM(BLD_FILE) /= 0) then
                     if (LEN_TRIM(BUILDDIRNM) /= 0) then
                        bfile_src(isrc) = TRIM(BUILDDIRNM) // '/' //
     &                                    TRIM(BLD_FILE)
                     else
                        bfile_src(isrc) = TRIM(BLD_FILE)
                     end if
                     primesrc = primesrc + 1
                     go to 2500
                  else
                     write(XMSG,9200)SRCID
                     go to 2400
                  end if
               end if
            end do
            write(XMSG,9300)SRCID
 9100       FORMAT('Source ',I10,
     &             ' in PRIME file list is duplicated')
 9200       FORMAT('Source ',I10,
     &             ' in PRIME file list has blank file name')
 9300       FORMAT('Source ',I10,
     &             ' in PRIME file list is not a PinG source')
 2400       CONTINUE
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            GO TO 2500
 999        eof = .true.
 2500       CONTINUE
         END DO

         if (primesrc == 0) then
            XMSG = 'No PinG source has building data specified'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         end if

         write(*,*)'PRIME will be used for the following PinG sources'
         do isrc = 1, nsrc
            if (LEN_TRIM(bfile_src(isrc)) /= 0) then
               write(*,*)idsrc(isrc)
            end if
         end do

      END IF  ! end of block for PRIME option

!------ open PiG emissions data file

      IF ( .NOT. OPEN3( PING_MEPSE, FSREAD3, PNAME ) ) THEN
         XMSG = 'Could not open ' // TRIM( PING_MEPSE ) // ' file'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

      IF ( .NOT. DESC3( PING_MEPSE ) ) THEN
         XMSG = 'Could not get ' // TRIM( PING_MEPSE ) // ' file description'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF              !  error abort if DESC3 failed

      if ( nsrc /= NROWS3D ) then
         nError   = RD_ERROR
         eRoutine = 'init_emissions'
         eMessage = 'Error: No. of sources in emissions file ' //
     &              'does not match no. in stack file'
         eInform  = 'File='//TRIM(PING_MEPSE)
         go to 9999
      end if

      nemit = NVARS3D

      stkdate = SDATE3D
!debug
      write(*,*)'nemit,stkdate: ',nemit,stkdate
!debug

! --- Allocate emissions array
      ALLOCATE ( QSRC( nsrc, nemit ), STAT = ASTAT )
      if (astat /= 0) then
         XMSG = 'Could not allocate memory for stack emissions'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

! --- Allocate emissions index array
      ALLOCATE ( index_host_emit( nemit ), STAT = ASTAT )
      if (astat /= 0) then
         XMSG = 'Could not allocate memory for emissions index array'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF 

      nameemit(1:nemit) = vname3d(1:nemit)

!debug
!     emitspc = 0
      do i = 1, nemit
!         emitspc = emitspc + 1
!         nameemit(emitspc) = vname3d(i)
        write(*,*)'i,nameemit: ',i,nameemit(i)
      end do
!debug

      do i = 1, nemit
         if (TRIM(nameemit(i)) == 'SULF') VSULF = i
         if (TRIM(nameemit(i)) == AEROSPC( ASO4_IDX )%EMIS) VPSO4 = i
         index_host_emit(i) = 0
         do j = 1, nspecies
            if (TRIM(nameemit(i)) == TRIM(species(j)%nameemit)) then
               index_host_emit(i) = j
               EXIT
            end if
         end do
         if (index_host_emit(i) == 0) then
            write(*,*)'Warning in init_emissions'
            write(*,*)'Unrecognized species ',TRIM(nameemit(i)),
     &                ' in point source file'
         end if
      end do

      do j = 1, nspecies
         index_emit(j) = 0
         do i = 1, nemit
            if (TRIM(nameemit(i)) == TRIM(species(j)%nameemit)) then
               index_emit(j) = i
!debug
               write(*,*)'SCICHEM species: ',j,species(j)%name,
     &                   '; emitted species: ',i,nameemit(i)
!debug
               EXIT
            end if
         end do
!debug
         if (index_emit(j) == 0) then
            write(*,*)'Species ',species(j)%name,' not emitted'
         end if
!debug
      end do
!DEBUG
      write(*,*)'VSULF,VPSO4: ',VSULF,VPSO4
!DEBUG
! -- read time header

      dtstep = TIME2SEC( tstep )/ 3600.
      tdur_emit = dtstep  !emission duration

! --- set emissions time (in SCICHEM time)

      if (restart) then
         tinit_emit = t
      else
         tinit_emit = 0.
      end if

9999  return

      end
