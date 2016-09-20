
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

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/procan/pa/pa_update.F,v 1.10 2011/12/08 16:22:11 sjr Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE PA_UPDATE( PRNAME, CGRID, JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C Function: Update the Process Analysis output arrays (for IPR only)
 
C Preconditions: None
 
C Key Subroutines/Functions Called: None
 
C Revision History:
C  Prototype created by Jerry Gipson, July, 1996
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Modified March, 1998 by Jerry Gipson to use units of moles/s for all
C                                       emisssions except aerosols
C  Modified Jun, 1998 by Jerry Gipson to add PING process
C  Modified Jun, 1998 by Jerry Gipson to print warning for unexpected
C                                     processes rather than abort
C  Modified 1/19/99 by David Wong at LM:
C                      -- add DATA_COPY function call to redistribute PA grid
C  Modified 2/26/99 by David Wong at LM:
C                      -- replaced DATA_COPY function with dimension specific
C                         DATA_COPY function and modified its argument list
C                      -- used ifdef statement to distinguish parallel
C                         implementation of IRR calculation which does not
C                         start at the origin
C  Modified 4/13/00 by Jerry Gipson to add AE surface area and correct AE
C                                   deposition sign
C  Modified 4/17/00 by David Wong at LM:
C                      -- bug fix: declare TDDEP as a 2D data rather than 3D,
C                         and use 2DE DATA COPY communication routine rather
C                         than 3D DATA COPY routine
C  Modified 5/4/00 by Jerry Gipson to correct DDEP calculations
C  Modified 22 Nov 00 by J.Young: Dave Wong`s f90 stenex DATA_COPY -
C                                 must explicitlt dimension CGRID, VEMIS, and DDEP
C  Modified 20 Jun 01 by J.Young: VEMIS, assumed shape
C                                 VEMIS assumed converted to ppm/sec form
C                                 NOTE: the arguments to DATA_COPY must have the layer
C                                 dimension the same as the full domain.
C  Modified 28 aug 01 by J.Young: dyn alloc - Use PAGRD_DEFN,
C                                 which uses HGRD_DEFN; replace INTERP3 with INTERPX
C                                 7 Mar 02 - J.Young: add units string variations
C  Modified  9 Oct 03 by J.Gipson: fixed subscript error for NR EMIS IPRs & re-did
C                                  AE EMIS IPRS for VEMIS in ppm units rather than
C                                  ug/m3 units
C  Modified 5 Nov 03 by J. Gipson to fix DDEP IPRs
C  Modified 25 Nov 03 by J Gipson to use step end time for couple/decouple
C  Modified 31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                              domain specifications in one module (GRID_CONF)
C   3 Apr 09 J.Young: replace EMISPRM... include files with simpler implementation
C  21 Jun 10 J.Young: convert for Namelist redesign
C  16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C  11 May 11 D.Wong: incorporated twoway model implementation
C  19 Jan 16 J.Young: flag for couple/decouple
C   6 May 16 J.Young: don`t couple/decouple; copy cgrid locally; only decouple the copy
C  16 Sep 16 J.Young: update for inline procan (IRR)
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain configuration
      USE CGRID_SPCS            ! CGRID mechanism species
      USE PA_DEFN               ! Process Anaylsis control and data variables
      USE PAGRD_DEFN            ! PA horiz domain specs
      USE UTILIO_DEFN           ! inherits PARUTILIO
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE, SE_DATA_COPY_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE, NOOP_DATA_COPY_MODULE)
#endif

      IMPLICIT NONE 

C Includes:
      INCLUDE SUBST_CONST       ! Constants
      INCLUDE SUBST_FILES_ID    ! file name parameters
      INCLUDE SUBST_EMISPRM     ! Emissions processing control parameters
      
C Arguments:
      CHARACTER( * ), INTENT( IN ) :: PRNAME   ! Last process called
      REAL, POINTER :: CGRID( :,:,:,: )  ! Conc array
      INTEGER, INTENT( IN ) :: JDATE       !  current date,    format YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME       !  current time,    format HHMMSS
      INTEGER, INTENT( IN ) :: TSTEP( 3 )  ! time step vector (HHMMSS)
                             ! TSTEP(1) = local output step
                             ! TSTEP(2) = sciproc sync. step (chem)
                             ! TSTEP(3) = twoway model time step w.r.t. wrf time
                             !            step and wrf/cmaq call frequency

C..Additional or other Arguments for ENTRY`s
      REAL,    INTENT( IN ) :: VEMIS ( :,:,:,: )  ! Emission rates (g/s)
      REAL,    INTENT( IN ) :: DDEP ( :,:,: )     ! Dry dep (Kg/ha)

C Gas species counts:
      INTEGER, SAVE :: N_SPCS

C Dimension index assigned for VDIF
      INTEGER, PARAMETER :: IND_VM = 1

C Dimension index assigned for CHEM 
      INTEGER, PARAMETER :: IND_CH = 2

C Dimension index assigned for AERO 
      INTEGER, PARAMETER :: IND_AE = 3

C Maximum allowable number of unexpected processes
      INTEGER, PARAMETER :: MXUNEXP = 50

C 1 hectare = 1.0e4 m**2
      REAL, PARAMETER :: CONVH2M = 1.0E-4

C mass to ppm factor
      REAL, PARAMETER :: CONVMW = 1.0E+06 * MWAIR

C aerosol emission conversion factor terms
      REAL, PARAMETER :: GPKG = 1.0E+03              ! g/kg
      REAL, PARAMETER :: MGPG = 1.0E+06              ! micro-g/g
      REAL, PARAMETER :: REFAC = 1.0E-06 * GPKG * MGPG / MWAIR

C External Functions: 
      INTEGER, EXTERNAL :: FINDEX    ! Finds the index of a number in a list

C Saved Local Variables:
      CHARACTER( 16 ), SAVE :: UNEXPPR( MXUNEXP )

      INTEGER, SAVE :: EMISLYRS
      INTEGER, SAVE :: NUNEXP = 0 ! Number of unexpected processes
      INTEGER, SAVE :: PRINDEM   ! Emissions output index
      INTEGER, SAVE :: PRINDVD   ! Vertical diffusion output index
      INTEGER, SAVE :: PRINDCH   ! Chemistry output index
      INTEGER, SAVE :: PRINDDD   ! Dry deposition output index
      INTEGER, SAVE :: PRINDAE   ! Dry deposition output index

C Indices for emission species in IPR outputs
      INTEGER, ALLOCATABLE, SAVE :: IPR_NGR2EM( :,:,: )  ! ( N_IPR_SPC,MXCGRID,3 )

C AE species?
      LOGICAL, ALLOCATABLE, SAVE :: IPR_AESPEC( :,: )    ! ( N_IPR_SPC,MXCGRID )

C Indices for dep species in IPR outputs
      INTEGER, ALLOCATABLE, SAVE :: IPR_NGR2DD( :,: )    ! ( N_IPR_SPC,MXCGRID )

      LOGICAL, SAVE :: LEMFIRST = .TRUE. ! Flag for 1st call of emis processing
      LOGICAL, SAVE :: LDDFIRST = .TRUE. ! Flag for 1st call of ddep processing

      LOGICAL, SAVE :: EM_AERO = .TRUE.   ! do aero emis processing in vdiff?
      LOGICAL, SAVE :: LAE_EM_IPR = .FALSE.  ! AE EMIS IPR requested?

      REAL, SAVE :: CONVDD( 1 )       ! Conversion factor for dry dep

      REAL, ALLOCATABLE, SAVE :: NUMFAC( : )  ! ddep conversion factor (AE only)
C ddep species mass to molar conversion factor
      REAL, ALLOCATABLE, SAVE :: RELWTDD( : )
C emiss species mass to molar conversion factor 
      REAL, ALLOCATABLE, SAVE :: RELWTEM( : )
C ae_conversion factors
      REAL, ALLOCATABLE, SAVE :: AE_EM_CONV( : )

C Reciprocal of map scale factor
      REAL, ALLOCATABLE, SAVE :: RMSFX2( :,: )   
      REAL, ALLOCATABLE, SAVE :: TRMSFX2( :,: )   

C for copy of CGRID
      REAL, ALLOCATABLE, SAVE :: CNGRD( :,:,:,: )   

C Offset for gases in vdif vs. chem gas-phase emis
      INTEGER, SAVE :: OFFSETG
C Offset for aerosols in vdif vs. chem gas-phase emis
      INTEGER, SAVE :: OFFSETA

C Flag for couple/decouple HADV, ZADV, and HDIFF
      LOGICAL, SAVE :: LCOUPLE

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER, SAVE :: LOGDEV

C Local Variables:
      CHARACTER( 80 ) :: MSG                  ! Message for output log
      CHARACTER( 16 ) :: PNAME = 'PA_UPDATE'  ! Routine name
      CHARACTER( 16 ) :: UNITS                ! Units of emissions
      CHARACTER( 16 ) :: VNAME                !  input variable name list

      INTEGER ASTAT     ! Allocate status code
      INTEGER C         ! Loop index for columns
      INTEGER ICG       ! Index for species in cgrid array
      INTEGER IDD       ! Index for deposition species
      INTEGER IEM       ! Index for emission species
      INTEGER IND       ! Species index
      INTEGER IPA       ! Index of process monitoring output
      INTEGER IPDD      ! Index of PA deposition output variable
      INTEGER IPEM      ! Index of PA emissions output variable
      INTEGER IPAJ      ! Index of PA vert. diff./chem output variable
      INTEGER ISV       ! Index for saved species conc array 
      INTEGER L         ! Loop index for layers
      INTEGER MDATE     ! Date of mid-point of timestep
      INTEGER MTIME     ! Time of mid-point of timestep
      INTEGER N         ! Loop index for saved species conc array
      INTEGER NGR       ! Loop index for number of cgrid species
      INTEGER NPA       ! No. of process monitoring outputs
      INTEGER PC        ! Index for PA output column
      INTEGER PL        ! Index for PA output level
      INTEGER PR        ! Index for PA output row
      INTEGER PRIND     ! Science process index
      INTEGER R         ! Loop index for rows
      INTEGER SP_INDX   ! Index of species in its class
      INTEGER SDATE     ! Date at end of timestep
      INTEGER STIME     ! Time at end of timestep
      INTEGER TOPLEV    ! Highest level for emisssions processing
      INTEGER UNIND     ! Index for unexpected processes

      LOGICAL LAESP     ! Flag for AE species

      REAL    CONVFC    ! Temporary conversion factor
      REAL    DT        ! Timestep in seconds
      REAL    DDX       ! Cell inverse x-width
      REAL    DDY       ! Cell inverse y-width
      REAL    DX        ! Cell x-width
      REAL    DY        ! Cell y-width
      REAL    EM        ! Computed emission rate
      REAL    NETDEP    ! Net dep for hour, converted to  kg/m**3

      REAL :: TCGRID  ( NCOLS,NROWS,NLAYS )  
      REAL :: DENSA_J ( NCOLS,NROWS,NLAYS )  ! Density times Jacobian
      REAL :: TDENSA_J( NCOLS,NROWS,NLAYS )  
      REAL :: DENS    ( NCOLS,NROWS,NLAYS )  ! Density of air
      REAL :: TDENS   ( NCOLS,NROWS,NLAYS )  
      REAL :: TVEMIS  ( NCOLS,NROWS,NLAYS )
      REAL :: TDDEP   ( NCOLS,NROWS )
      REAL :: ZF      ( NCOLS,NROWS,NLAYS )  ! Layer heights
      REAL :: TZF     ( NCOLS,NROWS,NLAYS )

      REAL :: X3FACE( 0:NLAYS )   ! vertical coordinate layer surface

      INTEGER GXOFF, GYOFF        ! global origin offset from file
C for INTERPX
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3
      INTEGER, SAVE :: STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2
    
      INTERFACE
         SUBROUTINE DECOUPLE ( CONC, JDATE, JTIME )
            REAL,    INTENT( INOUT ) :: CONC( :,:,:,: )
            INTEGER, INTENT( IN ) :: JDATE, JTIME
         END SUBROUTINE DECOUPLE
      END INTERFACE

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         LOGDEV = INIT3()
         ALLOCATE ( CNGRD( NCOLS,NROWS,NLAYS,SIZE( CGRID,4 ) ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            MSG = '*** ERROR allocating CNGRD'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF
      END IF

C Get process index and convert units if necessary
      PRIND = INDEX1( PRNAME, NPRCS, PROCNAME )

      IF ( PRIND .EQ. 0 ) THEN
         UNIND = INDEX1( PRNAME, MXUNEXP, UNEXPPR )
         IF ( UNIND .EQ. 0 ) THEN
            MSG = 'Warning: Process Analysis not expecting process ' // PRNAME 
            CALL M3MESG( MSG )
            NUNEXP = NUNEXP + 1
            IF ( NUNEXP .GT. MXUNEXP ) THEN
               MSG = 'Maximum number of unexpected processes for '
     &              // 'Process Analysis exceeded' 
               CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
            END IF
            UNEXPPR( NUNEXP ) = PRNAME
         END IF
      END IF

      LCOUPLE = PRNAME .EQ. 'HADV' .OR. PRNAME .EQ. 'ZADV' .OR. PRNAME .EQ. 'HDIF'

      IF ( LCOUPLE ) THEN
         SDATE = JDATE; STIME = JTIME
         CALL NEXTIME( SDATE, STIME, TSTEP( 2 ) )
#ifdef verbose_pa_update
         write( logdev,* ) "pa_update, call decouple:", stime, stime, "  ", prname
#endif
         CNGRD = CGRID  ! local copy (separate memory)
         CALL DECOUPLE( CNGRD, SDATE, STIME )

C..Compute delta conc for this process if requested
         IF ( PRIND .GT. 0 .AND. LPROCOUT( PRIND ) ) THEN
            DO NPA = 1, N_IPR_SPC
               IPA = IPROUT( NPA,PRIND )
               IF ( IPA .NE. 0 ) THEN
                  DO NGR = 1, NCGRID( NPA )
                     ICG = IPR2GRD( NPA,NGR )
                     ISV = IPR2SAV( NPA,NGR )
#ifdef parallel
                     CALL SUBST_DATA_COPY( CNGRD, TCGRID, ICG )
                     DO L = MY_BEGLEV, MY_ENDLEV
                        DO R = MY_BEGROW, MY_ENDROW
                           DO C = MY_BEGCOL, MY_ENDCOL
                              DELC( C,R,L,IPA ) = DELC( C,R,L,IPA )
     &                                          + SPCOEF( NPA,NGR )
     &                                          * ( TCGRID( C,R,L )
     &                                          -   CSAV( C,R,L,ISV ) )
                           END DO
                        END DO
                     END DO
#else
                     DO L = MY_BEGLEV, MY_ENDLEV
                        PL = L - MY_BEGLEV + 1
                        DO R = MY_BEGROW, MY_ENDROW
                           PR = R - MY_BEGROW + 1
                           DO C = MY_BEGCOL, MY_ENDCOL
                              PC = C - MY_BEGCOL + 1
                              DELC( PC,PR,PL,IPA ) = DELC( PC,PR,PL,IPA )
     &                                             + SPCOEF( NPA,NGR )
     &                                             * ( CNGRD( C,R,L,ICG )
     &                                             -   CSAV( PC,PR,PL,ISV ) )
                           END DO
                        END DO
                     END DO
#endif
                  END DO
               END IF
            END DO 
         END IF

C..Save concentrations for next delta c
         DO N = 1, NCSAVE
            ICG = SV2GRD( N )
#ifdef parallel
            CALL SUBST_DATA_COPY( CNGRD, CSAV, ICG, N )
#else
            DO L = MY_BEGLEV, MY_ENDLEV
               PL = L - MY_BEGLEV + 1
               DO R = MY_BEGROW, MY_ENDROW
                  PR = R - MY_BEGROW + 1
                  DO C = MY_BEGCOL, MY_ENDCOL
                     PC = C - MY_BEGCOL + 1
                     CSAV( PC,PR,PL,N ) = CNGRD( C,R,L,ICG ) 
                  END DO   
               END DO
            END DO
#endif
         END DO

      ELSE   ! .not. LCOUPLE

C..Compute delta conc for this process if requested
         IF ( PRIND .GT. 0 .AND. LPROCOUT( PRIND ) ) THEN
            DO NPA = 1, N_IPR_SPC
               IPA = IPROUT( NPA,PRIND )
               IF ( IPA .NE. 0 ) THEN
                  DO NGR = 1, NCGRID( NPA )
                     ICG = IPR2GRD( NPA,NGR )
                     ISV = IPR2SAV( NPA,NGR )
#ifdef parallel
                     CALL SUBST_DATA_COPY( CGRID, TCGRID, ICG )
                     DO L = MY_BEGLEV, MY_ENDLEV
                        DO R = MY_BEGROW, MY_ENDROW
                           DO C = MY_BEGCOL, MY_ENDCOL
                              DELC( C,R,L,IPA ) = DELC( C,R,L,IPA )
     &                                          + SPCOEF( NPA,NGR )
     &                                          * ( TCGRID( C,R,L )
     &                                          -   CSAV( C,R,L,ISV ) )
                           END DO
                        END DO
                     END DO
#else
                     DO L = MY_BEGLEV, MY_ENDLEV
                        PL = L - MY_BEGLEV + 1
                        DO R = MY_BEGROW, MY_ENDROW
                           PR = R - MY_BEGROW + 1
                           DO C = MY_BEGCOL, MY_ENDCOL
                              PC = C - MY_BEGCOL + 1
                              DELC( PC,PR,PL,IPA ) = DELC( PC,PR,PL,IPA )
     &                                             + SPCOEF( NPA,NGR )
     &                                             * ( CGRID( C,R,L,ICG )
     &                                             -   CSAV( PC,PR,PL,ISV ) )
                           END DO
                        END DO
                     END DO
#endif
                  END DO
               END IF
            END DO 
         END IF

C..Save concentrations for next delta c
         DO N = 1, NCSAVE
            ICG = SV2GRD( N )
#ifdef parallel
            CALL SUBST_DATA_COPY( CGRID, CSAV, ICG, N )
#else
            DO L = MY_BEGLEV, MY_ENDLEV
               PL = L - MY_BEGLEV + 1
               DO R = MY_BEGROW, MY_ENDROW
                  PR = R - MY_BEGROW + 1
                  DO C = MY_BEGCOL, MY_ENDCOL
                     PC = C - MY_BEGCOL + 1
                     CSAV( PC,PR,PL,N ) = CGRID( C,R,L,ICG ) 
                  END DO   
               END DO
            END DO
#endif
         END DO

      END IF   ! LCOUPLE

      RETURN

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Emissions processing section
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ENTRY PA_UPDATE_EMIS( PRNAME, VEMIS, JDATE, JTIME, TSTEP )

C..On first call, set pointers to emission species
      IF ( LEMFIRST ) THEN

#ifdef emis_chem
         EMISVD = .FALSE.
#else
         EMISVD = .TRUE.
#endif

         ALLOCATE ( IPR_NGR2EM( N_IPR_SPC,MXCGRID,3 ),
     &              IPR_AESPEC( N_IPR_SPC,MXCGRID ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            MSG = 'Failure allocating IPR_NGR2EM or IPR_AESPEC'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

         ALLOCATE ( AE_EM_CONV( N_AE_EMIS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            MSG = 'Failure allocating AE_EM_CONV'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

C..Set-up pointers to emis, vdiff, chem, and aero processes         
         PRINDEM = INDEX1( 'EMIS', NPRCS, PROCNAME )
         PRINDVD = INDEX1( 'VDIF', NPRCS, PROCNAME )
         PRINDCH = INDEX1( 'CHEM', NPRCS, PROCNAME )
         PRINDAE = INDEX1( 'AERO', NPRCS, PROCNAME )

C..Set an index offset if reactive gas emissions are done in VDIF
         IF ( EMISVD ) THEN
            OFFSETG = N_GC_EMIS
         ELSE
            OFFSETG = 0
         END IF
C..Set an index offset if aerosol emissions are done in VDIF
         IF ( EM_AERO ) THEN
            OFFSETA = OFFSETG + N_AE_EMIS
         ELSE
            OFFSETA = OFFSETG
         END IF

#ifdef verbose_pa_update
         write( logdev,* ) ' No. of families: ', N_IPR_SPC
#endif

C..Set the pointers to the emission array
         DO NPA = 1, N_IPR_SPC            ! foreach family

#ifdef verbose_pa_update
            write( logdev,* ) ' '
            write( logdev,* ) ' family index (NPA), NCGRID: ', NPA, NCGRID( NPA )
#endif

            DO NGR = 1, NCGRID( NPA )     ! foreach species in the family

#ifdef verbose_pa_update
               write( logdev,* ) ' CTM species index (NGR), IPR2GRD: ',
     &                             NGR, IPR2GRD( NPA,NGR )
#endif

               ICG = IPR2GRD( NPA,NGR )  ! CTM species index in the family
               IPR_NGR2EM( NPA,NGR,IND_VM ) = 0 
               IPR_NGR2EM( NPA,NGR,IND_CH ) = 0 
               IPR_NGR2EM( NPA,NGR,IND_AE ) = 0
               IPR_AESPEC( NPA,NGR ) = .FALSE.
               IND = 0

C..gas chem species
               IF ( ICG .LE. N_GC_SPC ) THEN

                  SP_INDX = ICG - GC_STRT + 1
                  IND = FINDEX( SP_INDX, N_GC_EMIS, GC_EMIS_MAP )

                  IF ( IND .GT. 0 ) THEN
                     IF ( EMISVD ) THEN
                        IPR_NGR2EM( NPA,NGR,IND_VM ) = IND 
                     ELSE
                        IPR_NGR2EM( NPA,NGR,IND_CH ) = IND 
                        IPR_NGR2EM( NPA,NGR,IND_AE ) = 0
                     END IF
!                    RELWTEM( ICG ) = CONVMW / GC_MOLWT( ICG )
!                    EM_FAC ( ICG ) = GC_EMIS_FAC( IND )
                  END IF   

C..aerosol species
               ELSE IF ( ICG. GE. AE_STRT .AND. ICG .LT. NR_STRT ) THEN

                  SP_INDX = ICG - AE_STRT + 1
                  IND = FINDEX ( SP_INDX, N_AE_EMIS, AE_EMIS_MAP )

                  IF ( IND .GT. 0 ) THEN
                     IF ( EM_AERO ) THEN
                        IPR_NGR2EM( NPA,NGR,IND_VM ) = IND + OFFSETG
                        IPR_AESPEC( NPA,NGR ) = .TRUE.
                        LAE_EM_IPR = .TRUE.
                     ELSE   ! PA in aero process not implemented
                        IPR_NGR2EM( NPA,NGR,IND_AE ) = 0
                     END IF
#ifdef verbose_pa_update
                  write( logdev,* ) ' aero: NPA, NGR, IND_VM, IPR_NGR2EM =',
     &                           NPA, NGR, IND_VM, IPR_NGR2EM( NPA,NGR,IND_VM )
#endif
                  END IF

C..non-reactive species
               ELSE IF ( ICG. GE. NR_STRT .AND. ICG .LT. TR_STRT ) THEN

                  SP_INDX = ICG - NR_STRT + 1  
                  IND = FINDEX ( SP_INDX, N_NR_EMIS, NR_EMIS_MAP )
                  IF ( IND .GT. 0 ) THEN
                     IPR_NGR2EM( NPA,NGR,IND_VM ) = IND + OFFSETA

#ifdef verbose_pa_update
                     write( logdev,* ) ' nonr: NPA, NGR, IND_VM, IPR_NGR2EM =',
     &                              NPA, NGR, IND_VM, IPR_NGR2EM( NPA, NGR, IND_VM )
#endif

!                    RELWTEM( ICG ) = CONVMW / NR_MOLWT( SP_INDX )   
!                    EM_FAC(  ICG ) = NR_EMIS_FAC( IND )
                  END IF   

C..tracer species
               ELSE IF ( ICG. GE. TR_STRT ) THEN

                  SP_INDX = ICG - TR_STRT + 1  
                  IND = FINDEX ( SP_INDX, N_TR_EMIS, TR_EMIS_MAP )

                  IF ( IND .GT. 0 ) THEN
#ifdef verbose_pa_update
                     write( logdev,* ) 'trac ind: ', ind
#endif
                     IPR_NGR2EM( NPA,NGR,IND_VM ) = IND + OFFSETA + N_NR_EMIS
                  END IF

               END IF

#ifdef verbose_pa_update
               write( logdev,* ) ' IPR_NGR2EM( NPA,NGR,IND_VM ): ',
     &                             IPR_NGR2EM( NPA,NGR,IND_VM )
               write( logdev,* ) ' IPR_NGR2EM( NPA,NGR,IND_CH ): ',
     &                             IPR_NGR2EM( NPA,NGR,IND_CH )
               write( logdev,* ) ' IPR_NGR2EM( NPA,NGR,IND_AE ): ',
     &                             IPR_NGR2EM( NPA,NGR,IND_AE )
#endif

            END DO      ! end species in the family loop
         END DO         ! end family loop

C..set layer layer thickenesses
!        X3FACE( 0 ) = VGLVS_GD( 1 )
!        DO L = 1, NLAYS
!           X3FACE( L ) = VGLVS_GD( L + 1 )
!           X3MIDL( L ) = 0.5 * ( X3FACE( L ) + X3FACE( L - 1 ) )
!        END DO

C..set cell widths
         IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
            DX = DG2M * XCELL_GD          ! in m.
            DY = DG2M * YCELL_GD *
     &           COS( PI180 * ( YORIG_GD + YCELL_GD * 
     &           FLOAT( NROWS ) ) )       ! in m.
         ELSE
            DX = XCELL_GD                  ! in m
            DY = YCELL_GD                  ! in m
         END IF
         DDX = 1.0 / DX
         DDY = 1.0 / DY

C..check units on emissions file
         IF ( .NOT. DESC3( EMIS_1 ) ) THEN
              MSG = 'Could not get '// EMIS_1 // ' file description'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 )
         END IF

         CALL UPCASE ( UNITS3D( 1 ) )
         IF ( UNITS3D( 1 ) .NE. 'MOLES/S'   .AND.
     &        UNITS3D( 1 ) .NE. 'MOLE/S'    .AND.
     &        UNITS3D( 1 ) .NE. 'MOL/S'     .AND.
     &        UNITS3D( 1 ) .NE. 'MOLES/SEC' .AND.
     &        UNITS3D( 1 ) .NE. 'MOLE/SEC'  .AND.
     &        UNITS3D( 1 ) .NE. 'MOL/SEC' ) THEN
            MSG = 'Units incorrect on ' // EMIS_1
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

C..get number of emission layers
         EMISLYRS = NLAYS3D
         
         LEMFIRST = .FALSE.

         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

C..get conversion factors for aero emissions; as of sep 03 release, incoming
C  units are in ppmV/sec for ae species, # aer x 10**6/ # molec air / sec for
C  NUM, and m2/mol sec for SRF.  Conversion factors convert to 
C  ug/m3 sec, #/m3 sec, and m2/m3 sec, respectively.

         IF ( LAE_EM_IPR ) THEN

            DO N = 1, N_AE_EMIS

               IF ( INDEX( AE_EMIS( N ), 'NUM' ) .NE. 0 ) THEN
                  AE_EM_CONV( N ) = REFAC * AVO / MGPG
               ELSE IF ( INDEX( AE_EMIS( N ), 'SRF' ) .NE. 0 ) THEN
                  AE_EM_CONV( N ) = 1.0E+06 * REFAC / MGPG
               ELSE
                  AE_EM_CONV( N ) = REFAC * AE_MOLWT( AE_EMIS_MAP( N ) )
               END IF

            END DO

        END IF

      END IF  ! LEMFIRST

C..get midpoint of time step
      MDATE = JDATE
      MTIME = JTIME
      CALL NEXTIME( MDATE, MTIME, SEC2TIME( TIME2SEC( TSTEP( 2 ) ) / 2 ) ) 

C..Get air density if needed

!     VNAME = 'DENSA_J'
!     IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, 'PA_UPDATE',
!!   &                    1,NCOLS, 1,NROWS, 1,NLAYS,
!    &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
!    &                    MDATE, MTIME, DENSA_J ) ) THEN
!           CALL M3EXIT( PNAME, JDATE, JTIME,
!    &                  'Could not interpolate DENSA_J', XSTAT1 )
!     END IF

      IF ( LAE_EM_IPR ) THEN
         VNAME = 'DENS'
         IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, 'PA_UPDATE',
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 
     &                       1,NLAYS,MDATE, MTIME, DENS ) ) THEN
           CALL M3EXIT( PNAME, JDATE, JTIME,
     &                  'Could not interpolate DENS', XSTAT1 )
         END IF
      END IF

C..Compute delta conc due to emissions and adjust vdiff or chem
C..output if necessary for each output species
      TOPLEV = MIN( MY_ENDLEV, EMISLYRS )
      DT = FLOAT( TIME2SEC( TSTEP( 2 ) ) )

      DO NPA = 1, N_IPR_SPC              ! foreach family
         IPEM = IPROUT( NPA,PRINDEM )    ! emis species index for this process

         IPAJ = 0
         IF ( PRNAME .EQ. 'VDIF' ) THEN
            IPAJ = IPROUT( NPA,PRINDVD )
            IND = IND_VM
         ELSE IF ( PRNAME .EQ. 'CHEM' ) THEN
            IPAJ = IPROUT( NPA,PRINDCH )
            IND = IND_CH
         ELSE IF ( PRNAME .EQ. 'AERO' ) THEN ! PA in aero not implemented!
            IPAJ = IPROUT( NPA,PRINDAE )
            IND = IND_AE
         END IF

         DO NGR = 1, NCGRID( NPA )       ! foreach species in the family
            ICG = IPR2GRD( NPA,NGR )     ! CTM species index in the family
            IEM = IPR_NGR2EM( NPA,NGR,IND )
            LAESP = IPR_AESPEC( NPA,NGR ) ! Flag for aerosol species

            IF ( IEM .NE. 0 .AND. ( IPEM .NE. 0 .OR. IPAJ .NE. 0 ) ) THEN

#ifdef parallel
               CALL SUBST_DATA_COPY ( VEMIS, TVEMIS, IEM )
               IF ( LAESP ) CALL SUBST_DATA_COPY ( DENS, TDENS )
#endif
               DO L = MY_BEGLEV, TOPLEV
                  PL = L - MY_BEGLEV + 1
                  DO R = MY_BEGROW, MY_ENDROW
                     PR = R - MY_BEGROW + 1
                     DO C = MY_BEGCOL, MY_ENDCOL
                        PC = C - MY_BEGCOL + 1

C..compute the emissions

                        IF ( .NOT. LAESP ) THEN
#ifdef parallel
                           EM = TVEMIS( C,R,L ) * DT
                        ELSE
                           EM = TVEMIS( C,R,L ) * DT * TDENS( C,R,L )
     &                        * AE_EM_CONV( IEM - OFFSETG )
#else
                           EM = VEMIS( C,R,L,IEM ) * DT
                        ELSE
                           EM = VEMIS( C,R,L,IEM ) * DT * DENS( C,R,L )
     &                        * AE_EM_CONV( IEM - OFFSETG )
#endif
                        END IF

C..modify both the emiss process and the calling process
                        IF ( IPEM .NE. 0 ) THEN
                           DELC( PC,PR,PL,IPEM ) = DELC( PC,PR,PL,IPEM )
     &                                           + SPCOEF( NPA,NGR ) * EM
                        END IF
                        IF ( IPAJ .NE. 0 ) THEN
                           DELC( PC,PR,PL,IPAJ ) = DELC( PC,PR,PL,IPAJ )
     &                                           - SPCOEF( NPA,NGR ) * EM
                        END IF
                     END DO
                  END DO
               END DO
            END IF
         END DO
      END DO

      RETURN

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Dry Deposition processing section
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ENTRY PA_UPDATE_DDEP( PRNAME, DDEP, JDATE, JTIME, TSTEP )

C..On first call, set pointers to deposition species 
      IF ( LDDFIRST ) THEN

         ALLOCATE ( IPR_NGR2DD( N_IPR_SPC,MXCGRID ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            MSG = 'Failure allocating IPR_NGR2DD'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF
 
         N_SPCS = N_GC_SPCD + N_AE_SPC + N_NR_SPC + N_TR_SPC

         ALLOCATE ( NUMFAC( N_SPCS ),
     &              RELWTDD( N_SPCS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            MSG = 'Failure allocating NUMFAC or RELWTDD'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

         PRINDDD = INDEX1( 'DDEP', NPRCS, PROCNAME )
         PRINDVD = INDEX1( 'VDIF', NPRCS, PROCNAME )

C..set pointers for the ddep array
         DO NPA = 1, N_IPR_SPC 
            DO NGR = 1, NCGRID( NPA )

               ICG = IPR2GRD( NPA, NGR )
               IPR_NGR2DD( NPA, NGR ) = 0
               IND = 0

               IF ( ICG .LT. AE_STRT ) THEN

                  SP_INDX = ICG - GC_STRT + 1
                  IND = FINDEX ( SP_INDX, N_GC_DEPV, GC_DEPV_MAP )

                  IF ( IND .NE. 0 ) THEN                  
                     IPR_NGR2DD( NPA, NGR ) = IND
                     RELWTDD( ICG ) = CONVMW / GC_MOLWT( ICG )
                  END IF

               ELSE IF ( ICG .GE. AE_STRT .AND. ICG .LT. NR_STRT ) THEN

                  SP_INDX = ICG - AE_STRT + 1
                  IND = FINDEX ( SP_INDX, N_AE_DEPV, AE_DEPV_MAP )

                  IF ( IND .NE. 0 ) THEN                 
                     IPR_NGR2DD( NPA, NGR ) = N_GC_DEPV + IND
                     IF ( AE_SPC( SP_INDX )( 1:3 ) .EQ. 'NUM' ) THEN
                        NUMFAC( ICG ) = 1.0
                     ELSE IF ( AE_SPC( SP_INDX )( 1:3 ) .EQ. 'SRF' ) THEN
                        NUMFAC( ICG ) = 1.0
                     ELSE
                        NUMFAC( ICG ) = 1.0E+09
                     END IF
                  END IF

               ELSE IF ( ICG .GE. NR_STRT .AND. ICG .LT. TR_STRT ) THEN

                  SP_INDX = ICG - NR_STRT + 1
                  IND = FINDEX ( SP_INDX, N_NR_DEPV, NR_DEPV_MAP )

                  IF ( IND .NE. 0 )  THEN
                     IPR_NGR2DD( NPA, NGR ) = N_GC_DEPV + N_AE_DEPV + IND
                     RELWTDD( ICG ) = CONVMW / NR_MOLWT( SP_INDX )
                  END IF

               ELSE IF ( ICG .GE. TR_STRT ) THEN

                  SP_INDX = ICG - TR_STRT + 1
                  IND = FINDEX ( SP_INDX, N_NR_DEPV, NR_DEPV_MAP )

                  IF ( IND .NE. 0 ) THEN
                     IPR_NGR2DD( NPA, NGR ) = N_GC_DEPV + N_AE_DEPV
     &                                      + N_NR_DEPV + IND
                     RELWTDD( ICG ) = CONVMW / TR_MOLWT( SP_INDX )
                  END IF
               END IF
            END DO            
         END DO

C..set layer layer thickenesses
         X3FACE( 0 ) = VGLVS_GD( 1 )
         DO L = 1, NLAYS
            X3FACE( L ) = VGLVS_GD( L + 1 )
         END DO

         L = 1
         CONVDD( L ) = 1.0 / ABS ( X3FACE( L ) - X3FACE( L - 1 ) ) 

c..get reciprocal of msfx2 (squared map scale factor)
         CALL SUBHFILE( GRID_CRO_2D, GXOFF, GYOFF,
     &                  STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2 )

         ALLOCATE ( RMSFX2( NCOLS,NROWS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            MSG = 'Failure allocating RMSFX4'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

         VNAME = 'MSFX2'
         IF ( .NOT. INTERPX( GRID_CRO_2D, VNAME, PNAME,
     &                       STRTCOLGC2,ENDCOLGC2, STRTROWGC2,ENDROWGC2, 1,1,
     &                       JDATE, JTIME, RMSFX2 ) ) THEN
            MSG = ' Error interpolating variable ' // TRIM( VNAME )
     &          // ' from ' // GRID_CRO_2D
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

         RMSFX2 = 1.0 / RMSFX2        ! Array calculation

#ifdef parallel
         ALLOCATE ( TRMSFX2( NCOLS,NROWS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            MSG = 'Failure allocating RMSFX4'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

         CALL SUBST_DATA_COPY( RMSFX2, TRMSFX2 )
#endif
         
         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

         LDDFIRST = .FALSE.           

      END IF

      IF ( MY_BEGLEV .GT. 1 ) RETURN

C..get midpoint of time step
      MDATE = JDATE
      MTIME = JTIME
      CALL NEXTIME( MDATE, MTIME, SEC2TIME( TIME2SEC( TSTEP( 2 ) ) / 2 ) )

C..get density x jacobian and layer heights

      VNAME = 'DENSA_J'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, 'PA_UPDATE',
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, DENSA_J ) ) THEN
            CALL M3EXIT( PNAME, JDATE, JTIME,
     &                   'Could not interpolate DENSA_J', XSTAT1 )
      END IF

      IF ( N_AE_SPC .GT. 0 ) THEN

         VNAME = 'ZF'
         IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, 'PA_UPDATE',
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,1,NLAYS,
     &                       MDATE, MTIME, ZF ) ) THEN
               CALL M3EXIT( PNAME, JDATE, JTIME,
     &                      'Could not interpolate ZF', XSTAT1 )
         END IF
      END IF

#ifdef parallel
      CALL SUBST_DATA_COPY( DENSA_J, TDENSA_J )

      IF ( N_AE_SPC .GT. 0 ) THEN
         CALL SUBST_DATA_COPY ( ZF, TZF )
      END IF
#endif

C..Compute delta conc due to ddep and adjust vdiff output if necessary
      DO NPA = 1, N_IPR_SPC
         IPDD = IPROUT( NPA,PRINDDD )
         IPAJ = IPROUT( NPA,PRINDVD ) 

         DO NGR = 1, NCGRID( NPA )                       
            ICG = IPR2GRD( NPA,NGR )
            IDD = IPR_NGR2DD( NPA,NGR )

            IF ( IDD .NE. 0 .AND. ( IPDD .NE. 0 .OR. IPAJ .NE. 0 ) ) THEN

#ifdef parallel
               CALL SUBST_DATA_COPY( DDEP, TDDEP, IDD )
#endif

               DO R = MY_BEGROW, MY_ENDROW
                  PR = R - MY_BEGROW + 1
                  DO C = MY_BEGCOL, MY_ENDCOL
                     PC = C - MY_BEGCOL + 1

C..compute the dep in ppm

                     IF ( ICG .GE. AE_STRT. AND. ICG .LT. NR_STRT ) THEN
#ifdef parallel
                        NETDEP = TDDEP( C,R ) * CONVH2M
     &                         * NUMFAC( ICG ) / TZF( C,R,1 )
                     ELSE
                        NETDEP = TDDEP( C,R ) * CONVH2M
     &                         * RELWTDD( ICG ) * CONVDD( 1 )
     &                         * TRMSFX2( C,R ) / TDENSA_J( C,R,1 ) 

#else
                        NETDEP = DDEP( C,R,IDD ) * CONVH2M
     &                         * NUMFAC( ICG ) / ZF( C,R,1 )
                     ELSE
                        NETDEP = DDEP( C,R,IDD ) * CONVH2M
     &                         * RELWTDD( ICG ) * CONVDD( 1 )
     &                         * RMSFX2( C,R ) / DENSA_J( C,R,1 ) 
#endif
                     END IF

C..adjust the process analysis output arrays

                     IF ( IPDD .NE .0 ) THEN
                        DELC( PC,PR,1,IPDD ) = DELC( PC,PR,1,IPDD )
     &                                       - SPCOEF( NPA,NGR ) * NETDEP
                     END IF

                     IF ( IPAJ .NE. 0 ) THEN
                        DELC( PC,PR,1,IPAJ ) = DELC( PC,PR,1,IPAJ )  
     &                                       + SPCOEF( NPA,NGR ) * NETDEP
                     END IF
                  END DO   ! C
               END DO   ! R

            END IF
         END DO   ! NGR
      END DO   ! NPA

      RETURN

      END SUBROUTINE PA_UPDATE

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE DECOUPLE ( CONC, JDATE, JTIME )

C-----------------------------------------------------------------------
C Function:
C   Convert units and decouple concentration values in CGRID from transport
C   CONC is a copy of the current CGRID
 
C Preconditions:
 
C Subroutines and functions called:
C   INTERPX, M3EXIT
 
C Revision History:
C  6 May 16 J.Young: initial - part of pa_update.F file
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN

      IMPLICIT NONE   

C Include files:
      INCLUDE SUBST_FILES_ID    ! file name parameters

C Arguments:
      REAL,    INTENT( INOUT ) :: CONC( :,:,:,: )   ! concentrations
      INTEGER, INTENT( IN ) :: JDATE      ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME      ! current model time, coded HHMMSS

C Parameters:
      REAL, PARAMETER :: GPKG = 1.0E+03   ! g/kg
      REAL, PARAMETER :: MGPG = 1.0E+06   ! micro-g/g
      REAL, PARAMETER :: CONV = GPKG * MGPG

C External Functions:

C File Variables:
      REAL       RJACOBM( NCOLS,NROWS,NLAYS )  ! reciprocal midlayer Jacobian
      REAL       RRHOJ  ( NCOLS,NROWS,NLAYS )  ! reciprocal Jacobian * air density

C Local Variables:
      CHARACTER( 16 ) :: PNAME = 'DECOUPLE'
      CHARACTER( 16 ) :: VNAME
      CHARACTER( 96 ) :: XMSG = ' '

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER, SAVE :: LOGDEV

      INTEGER, SAVE :: NQAE              ! number of micro-grams/m**3 species
      INTEGER, SAVE :: NNAE              ! number of #/m**3 species
      INTEGER, SAVE :: NSAE              ! number of m**2/m**3 species
      INTEGER, ALLOCATABLE, SAVE :: QAE( : ) ! CONC pointer to micro-grams/m**3 species
      INTEGER, ALLOCATABLE, SAVE :: NAE( : ) ! CONC pointer to #/m**3 species
      INTEGER, ALLOCATABLE, SAVE :: SAE( : ) ! CONC pointer to m**2/m**3 species

      INTEGER     ASTAT
      INTEGER     GXOFF, GYOFF          ! global origin offset from file
C for INTERPX
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

      INTEGER     OFF              ! loop offset to CONC species
      INTEGER     C, R, L, SPC, V  ! loop counters
      INTEGER     RHOJ_LOC         ! pointer to transported RHOJ (in CONC)

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         LOGDEV = INIT3()

         IF ( N_AE_SPC .GT. 0 ) THEN
C create aerosol species pointers to distinguish micro-grams/m**3,
C #/m**3 (number density), and m**2/m**3 (surface area) species
 
            ALLOCATE ( QAE( N_AE_SPC ),
     &                 NAE( N_AE_SPC ),
     &                 SAE( N_AE_SPC ), STAT = ASTAT )
            IF ( ASTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating QAE, NAE, or SAE'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
            NQAE = 0       ! no. of micro-grams/m**3 species
            NNAE = 0       ! no. of #/m**3 species
            NSAE = 0       ! no. of m**2/m**3 species
!           OFF = AE_STRT - 1
            OFF = 0
            DO SPC = 1, N_AE_SPC
               IF ( AE_SPC( SPC )( 1:3 ) .EQ. 'NUM' ) THEN
                  NNAE = NNAE + 1
                  NAE( NNAE ) = OFF + SPC
               ELSE IF ( AE_SPC( SPC )( 1:3 ) .EQ. 'SRF' ) THEN
                  NSAE = NSAE + 1
                  SAE( NSAE ) = OFF + SPC
               ELSE
                  NQAE = NQAE + 1
                  QAE( NQAE ) = OFF + SPC
               END IF
            END DO

         END IF

         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

      END IF       ! if firstime

C retrieve transported RhoJ
      RHOJ_LOC = GC_STRT + N_GC_SPC
      DO L = 1, NLAYS
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               RRHOJ( C,R,L ) = CONC( C,R,L,RHOJ_LOC )
            END DO
         END DO
      END DO
!     write( logdev,* ) "decouple, RHOJ from CONC:", jtime

!     VNAME = 'DENSA_J'
!     IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
!    &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
!    &                    JDATE, JTIME, RRHOJ ) ) THEN
!        XMSG = 'Could not read ' // VNAME // ' from ' // MET_CRO_3D
!        CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!     END IF
!     write( logdev,* ) "decouple, RHOJ from MET_CRO_3D:", jtime

      RRHOJ = 1.0 / RRHOJ   ! array assignment

      VNAME = 'JACOBM'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    JDATE, JTIME, RJACOBM ) ) THEN
         XMSG = 'Could not read ' // VNAME // ' from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

      RJACOBM = 1.0 / RJACOBM   ! array assignment
 
C decouple for chemistry and diffusion
 
C CONC in mixing ratio [ppmV] X (air density X Jacobian) -> mixing ratio [ppmV]
      IF ( N_GC_SPC .GT. 0 ) THEN
         OFF = GC_STRT - 1
         DO V = 1, N_GC_TRNS
            SPC = OFF + GC_TRNS_MAP( V )
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     CONC( C,R,L,SPC ) = CONC( C,R,L,SPC ) * RRHOJ( C,R,L )
                  END DO
               END DO
            END DO
         END DO
      END IF
 
      OFF = AE_STRT - 1
C CONC in Jacobian [m] X [Kg/m**3] -> [ug/m**3]
      IF ( NQAE .GT. 0 ) THEN
         DO V = 1, NQAE
            SPC = OFF + AE_TRNS_MAP( QAE( V ) )
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     CONC( C,R,L,SPC ) = CONV * CONC( C,R,L,SPC ) * RJACOBM( C,R,L )
                  END DO
               END DO
            END DO
         END DO
      END IF

C CONC in Jacobian [m] X [#/m**3] -> #/m**3
      IF ( NNAE .GT. 0 ) THEN
         DO V = 1, NNAE
            SPC = OFF + AE_TRNS_MAP( NAE( V ) )
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     CONC( C,R,L,SPC ) = CONC( C,R,L,SPC ) * RJACOBM( C,R,L )
                  END DO
               END DO
            END DO
         END DO
      END IF
 
C CONC in Jacobian [m] X [m**2/m**3] -> m**2/m**3
      IF ( NSAE .GT. 0 ) THEN
         DO V = 1, NSAE
            SPC = OFF + AE_TRNS_MAP( SAE( V ) )
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     CONC( C,R,L,SPC ) = CONC( C,R,L,SPC ) * RJACOBM( C,R,L )
                  END DO
               END DO
            END DO
         END DO
      END IF

C CONC in mixing ratio [ppmV] X (air density X jacobian) -> mixing ratio [ppmV]
      IF ( N_NR_SPC .GT. 0 ) THEN
         OFF = NR_STRT - 1
         DO V = 1, N_NR_TRNS
            SPC = OFF + NR_TRNS_MAP( V )
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     CONC( C,R,L,SPC ) = CONC( C,R,L,SPC ) * RRHOJ( C,R,L )
                  END DO
               END DO
            END DO
         END DO
      END IF
 
C CONC in mixing ratio [ppmV] X (air density X jacobian) -> mixing ratio [ppmV]
      IF ( N_TR_SPC .GT. 0 ) THEN
         OFF = TR_STRT - 1
         DO V = 1, N_TR_SPC
            SPC = OFF + V
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     CONC( C,R,L,SPC ) = CONC( C,R,L,SPC ) * RRHOJ( C,R,L )
                  END DO
               END DO
            END DO
         END DO
      END IF

      RETURN

      END SUBROUTINE DECOUPLE
