
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

      MODULE PA_UPDATE
 
      USE GRID_CONF             ! horizontal & vertical domain configuration
      USE CGRID_SPCS, ONLY : N_CGRID_SPC, CGRID_MASK_AERO,N_AE_SPC, N_SPC_DEPV,
     &                       MAP_DEPVtoCGRID, CGRID_MASK_GAS, CGRID_MASK_NR,
     &                       CGRID_MASK_NUM, CGRID_MASK_SRF, CGRID_MASK_TRAC,
     &                       CGRID_MW ! CGRID mechanism species
      USE VDIFF_MAP, ONLY : N_SPC_DIFF, DIFF_MW, DIFF_MASK_NUM, DIFF_MASK_SRF, DIFF_MAP
      USE PA_DEFN               ! Process Anaylsis control and data variables
      USE PAGRD_DEFN            ! PA horiz domain specs
      USE UTILIO_DEFN           ! inherits PARUTILIO
      USE EMIS_VARS
      USE CENTRALIZED_IO_MODULE
      USE BUDGET_DEFN

#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE, SE_DATA_COPY_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE, NOOP_DATA_COPY_MODULE)
#endif
 
      PUBLIC PA_UPDATE_PROC, PA_UPDATE_EMIS, PA_UPDATE_DDEP,
     &       PA_UPDATE_HADV, PA_UPDATE_AERO

      PRIVATE

      REAL, ALLOCATABLE, SAVE :: CNGRD( :,:,:,: )

      CONTAINS

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE PA_UPDATE_PROC( IPR_ID, CGRID, JDATE, JTIME, TSTEP )

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
C  01 Feb 19 D.Wong: Implemented centralized I/O approach
C-----------------------------------------------------------------------


      IMPLICIT NONE 

      ! Includes:
      INCLUDE SUBST_CONST       ! Constants
      INCLUDE SUBST_FILES_ID    ! file name parameters
      INCLUDE SUBST_EMISPRM     ! Emissions processing control parameters
      
      ! Arguments:
      INTEGER, INTENT( IN ) :: IPR_ID   ! Last process called
      REAL, POINTER         :: CGRID( :,:,:,: )  ! Conc array
      INTEGER, INTENT( IN ) :: JDATE       !  current date,    format YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME       !  current time,    format HHMMSS
      INTEGER, INTENT( IN ) :: TSTEP( 3 )  ! time step vector (HHMMSS)
                             ! TSTEP(1) = local output step
                             ! TSTEP(2) = sciproc sync. step (chem)
                             ! TSTEP(3) = twoway model time step w.r.t. wrf time
                             !            step and wrf/cmaq call frequency

      ! Flag for couple/decouple HADV, ZADV, and HDIFF
      LOGICAL, SAVE :: LCOUPLE

      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      ! Local Variables:
      CHARACTER( 80 ) :: MSG                  ! Message for output log
      CHARACTER( 16 ) :: PNAME = 'PA_UPDATE_PROC'  ! Routine name
      CHARACTER( 16 ) :: UNITS                ! Units of emissions
      CHARACTER( 16 ) :: VNAME                !  input variable name list

      INTEGER ASTAT     ! Allocate status code
      INTEGER C, R, L   ! Loop index for columns
      INTEGER IPRSPC    ! Index for each process species
      INTEGER IPRV      ! Index for each combination of species-process
      INTEGER ISPC      ! Index for each process species within each family
      INTEGER ICG       ! Index for species in cgrid array
      INTEGER ISV       ! Index for species in saved array
      INTEGER MDATE     ! Date of mid-point of timestep
      INTEGER MTIME     ! Time of mid-point of timestep
      INTEGER N         ! Loop index for saved species conc array
      INTEGER PC,PR,PL  ! Index for PA output column
      REAL    DT        ! Timestep in seconds
      INTEGER SDATE     ! Date at end of timestep
      INTEGER STIME     ! Time at end of timestep


      REAL :: TCGRID  ( MY_PACOLS,MY_PAROWS,PALEVS )  

!-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         ALLOCATE ( CNGRD( NCOLS,NROWS,NLAYS,N_CGRID_SPC ),
     &              CSAV( NCOLS,NROWS,NLAYS,N_CGRID_SPC ),
     &              STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            MSG = '*** ERROR allocating CNGRD'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF
         CSAV  = CGRID
      END IF

      CNGRD = CGRID  ! local copy (separate memory)
      
      ! Determine if concentrations need to converted from mixing
      ! ratios.
      LCOUPLE = IPR_ID .EQ. IPR_ZADV .OR. 
     &          IPR_ID .EQ. IPR_HDIF
      IF ( LCOUPLE ) THEN
         SDATE = JDATE; STIME = JTIME
         CALL NEXTIME( SDATE, STIME, TSTEP( 2 ) )
         CALL DECOUPLE_PA( CNGRD, SDATE, STIME )
      END IF

      ! Save Changes in Budget Array
      CALL STORE_BUDGET( IPR_ID, CNGRD-CSAV, JDATE, JTIME )

      IF ( LIPR ) THEN
         ! Compute delta conc for this process if requested
         DO IPRV = 1,NIPRVAR
            IF ( MASK_IPR_PROC( IPRV,IPR_ID ) ) THEN
               IPRSPC = MAP_IPRVARtoSPC( IPRV )
               DO ISPC = 1, NCGRID( IPRSPC )
                  ICG = MAP_IPRtoCGRID( IPRSPC,ISPC )
#ifdef parallel
                  CALL SUBST_DATA_COPY( CNGRD, TCGRID, ICG )
#else        
                  TCGRID( :,:,: ) = CNGRD( PA_BEGCOL:PA_ENDCOL,PA_BEGROW:PA_ENDROW,
     &                                     PA_BEGLEV:PA_ENDLEV,ICG )
#endif
                  DELC( :,:,:,IPRV ) = DELC( :,:,:,IPRV ) + SPCOEF( IPRSPC,ISPC ) 
     &                 * ( TCGRID( :,:,: ) - CSAV( :,:,:,ICG ) )
               END DO
            END IF
         END DO
      END IF

      CSAV = CNGRD

      RETURN
      END SUBROUTINE PA_UPDATE_PROC

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!  Emissions processing section
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE PA_UPDATE_EMIS( VEMIS, JDATE, JTIME, TSTEP )

      IMPLICIT NONE

      ! Includes:
      INCLUDE SUBST_CONST       ! Constants
      INCLUDE SUBST_FILES_ID    ! file name parameters
      INCLUDE SUBST_EMISPRM     ! Emissions processing control parameters
      
      REAL                  :: VEMIS ( :,:,:,: )  ! Emission rates (g/s)
      INTEGER, INTENT( IN ) :: JDATE       !  current date,    format YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME       !  current time,    format HHMMSS
      INTEGER, INTENT( IN ) :: TSTEP( 3 )  ! time step vector (HHMMSS)
                             ! TSTEP(1) = local output step
                             ! TSTEP(2) = sciproc sync. step (chem)
                             ! TSTEP(3) = twoway model time step w.r.t. wrf time
                             !            step and wrf/cmaq call frequency

      ! aerosol emission conversion factor terms
      REAL, PARAMETER :: GPKG = 1.0E+03       ! g kg-1
      REAL, PARAMETER :: MGPG = 1.0E+06       ! ug g-1
      REAL, PARAMETER :: REFAC = GPKG / MWAIR ! mol kg -1

      ! ae_conversion factors
      REAL, ALLOCATABLE, SAVE :: PA_EMIS_CONV( : )

      REAL :: DENS    ( NCOLS,NROWS,NLAYS )  ! Density of air
      REAL :: TVEMIS  ( MY_PACOLS,MY_PAROWS,PALEVS )  ! Computed emission rate
       
      INTEGER ASTAT     ! Allocate status code
      INTEGER C, R, L   ! Loop index for columns
      INTEGER IPRSPC    ! Index for each process species
      INTEGER ISPC      ! Index for each process species within each family
      INTEGER IPRV      ! Index for each combination of species-process
      INTEGER ICG       ! Index for species in cgrid array
      INTEGER ISV       ! Index for species in saved array
      INTEGER I
      REAL    DT
      INTEGER MDATE     ! Date of mid-point of timestep
      INTEGER MTIME     ! Time of mid-point of timestep
 
      CHARACTER( 80 ) :: MSG                  ! Message for output log
      CHARACTER( 16 ) :: PNAME = 'PA_UPDATE_EMIS'  ! Routine name
      LOGICAL,SAVE :: FIRST_TIME = .TRUE.

      ! On first call, set pointers to emission species
      IF ( FIRST_TIME ) THEN
         FIRST_TIME = .FALSE.

         ALLOCATE ( PA_EMIS_CONV( N_CGRID_SPC ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            MSG = 'Failure allocating PA_EMIS_CONV'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

         ! Get conversion factors for aero emissions; as of sep 03 release, incoming
         ! units are in ppmV/sec for ae species, # aer x 10**6/ # molec air / sec for
         ! NUM, and m2/mol sec for SRF.  Conversion factors convert to 
         ! ug/m3 sec, #/m3 sec, and m2/m3 sec, respectively.
         PA_EMIS_CONV = 1.0
         WHERE ( CGRID_MASK_NUM ) 
             PA_EMIS_CONV( : ) = REFAC !* AVO / MGPG
         ELSEWHERE ( CGRID_MASK_SRF ) 
             PA_EMIS_CONV( : ) = REFAC !1.0E+06 * REFAC / MGPG
         ELSEWHERE
             PA_EMIS_CONV( : ) = REFAC * CGRID_MW( : )
         END WHERE

      END IF  ! LEMFIRST

      ! Get midpoint of time step
      MDATE = JDATE
      MTIME = JTIME
      CALL NEXTIME( MDATE, MTIME, SEC2TIME( TIME2SEC( TSTEP( 2 ) ) / 2 ) ) 

      ! Compute delta conc due to emissions and adjust vdiff or chem
      ! output if necessary for each output species
      DT = FLOAT( TIME2SEC( TSTEP( 2 ) ) )

      ! Get air density
      call interpolate_var ('DENS', mdate, mtime, DENS)
      
      ! Convert Aerosol Emissions To Correct Units and Apply Species
      ! Coefficients. ug m-3 | N m-3 | and m2 m-3
      VEMIS = VEMIS  * DT
      DO ICG = 1,N_CGRID_SPC
         IF ( CGRID_MASK_AERO( ICG ) ) 
     &      VEMIS(:,:,:,ICG) = VEMIS(:,:,:,ICG) * DENS( :,:,: ) * PA_EMIS_CONV( ICG )
      END DO
      
      ! Save Changes in Budget Array
      CALL STORE_BUDGET( IPR_EMIS, VEMIS, JDATE, JTIME )

      ! Save Changes in Process Analysis Array
      IF ( LIPR ) THEN
         DO IPRV = 1,NIPRVAR
            IF ( MASK_IPR_PROC( IPRV,IPR_VDIF ) .OR. 
     &           MASK_IPR_PROC( IPRV,IPR_EMIS ) ) THEN
               ! Either VDIF or EMIS are needed for this IPR Variable.
               IPRSPC = MAP_IPRVARtoSPC( IPRV ) 

               DO ISPC = 1, NCGRID( IPRSPC )       ! foreach species in the family
                  ICG = MAP_IPRtoCGRID( IPRSPC,ISPC )     ! CTM species index in the family

                  ! Retrieve Emissions for this Time Step
#ifdef parallel
                  CALL SUBST_DATA_COPY ( VEMIS, TVEMIS, ICG )
#else
                  TVEMIS( :,:,: ) = VEMIS( PA_BEGROW:PA_ENDROW,
     &                                  PA_BEGCOL:PA_ENDCOL,
     &                                  PA_BEGLEV:PA_ENDLEV,
     &                                  ICG )
#endif
                  ! Add up Emissions and/or Vdiff depending on the process mask
                  IF ( MASK_IPR_PROC( IPRV,IPR_EMIS ) )
     &               DELC( :,:,:,IPRV ) = DELC( :,:,:,IPRV ) 
     &                                  + TVEMIS( :,:,: ) * SPCOEF( IPRSPC,ISPC )
               
                  IF ( MASK_IPR_PROC( IPRV,IPR_VDIF ) )
     &               DELC( :,:,:,IPRV ) = DELC( :,:,:,IPRV ) 
     &                                  - TVEMIS( :,:,: ) * SPCOEF( IPRSPC,ISPC )
               END DO
            END IF
         END DO
      END IF

      RETURN
      END SUBROUTINE PA_UPDATE_EMIS
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Dry Deposition processing section
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE PA_UPDATE_DDEP( DDEP, JDATE, JTIME, TSTEP )

      IMPLICIT NONE

      ! Includes:
      INCLUDE SUBST_CONST       ! Constants
      INCLUDE SUBST_FILES_ID    ! file name parameters
      INCLUDE SUBST_EMISPRM     ! Emissions processing control parameters
      
      ! Additional or other Arguments for ENTRY`s
      REAL                  :: DDEP ( :,:,: )     ! Dry dep (Kg/ha)
      INTEGER, INTENT( IN ) :: JDATE       !  current date,    format YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME       !  current time,    format HHMMSS
      INTEGER, INTENT( IN ) :: TSTEP( 3 )  ! time step vector (HHMMSS)
                             ! TSTEP(1) = local output step
                             ! TSTEP(2) = sciproc sync. step (chem)
                             ! TSTEP(3) = twoway model time step w.r.t. wrf time
                             !            step and wrf/cmaq call
                             !            frequency

      ! ae_conversion factors
      REAL, ALLOCATABLE, SAVE :: PA_DEPV_CONV( : )

      REAL, SAVE :: CONVDD       ! Conversion factor for dry dep
      LOGICAL, SAVE :: LDDFIRST = .TRUE. ! Flag for 1st call of ddep processing

      ! 1 hectare = 1.0e4 m**2
      REAL, PARAMETER :: CONVH2M = 1.0E-4

      ! mass to ppm factor
      REAL, PARAMETER :: CONVMW = 1.0E+06 * MWAIR ! ug mol-1

      REAL :: DENS    ( NCOLS,NROWS,NLAYS )  ! Density of air
      REAL :: DENSA_J ( NCOLS,NROWS,NLAYS )  ! Density of air
      REAL :: TDDEP   ( MY_PACOLS,MY_PAROWS )
      REAL :: ZF      ( NCOLS,NROWS,NLAYS )  ! Layer heights

      INTEGER ASTAT     ! Allocate status code
      INTEGER C, R, L   ! Loop index for columns
      INTEGER IPRSPC    ! Index for each process species
      INTEGER ISPC      ! Index for each process species within each family
      INTEGER IPRV      ! Index for each combination of species-process
      INTEGER ICG       ! Index for species in cgrid array
      INTEGER ISV       ! Index for species in saved array
      INTEGER MDATE     ! Date of mid-point of timestep
      INTEGER MTIME     ! Time of mid-point of timestep
      INTEGER I
 
      CHARACTER( 80 ) :: MSG                  ! Message for output log
      CHARACTER( 16 ) :: PNAME = 'PA_UPDATE_DDEP'  ! Routine name
      LOGICAL,SAVE :: FIRST_TIME = .TRUE.


      ! On first call, set pointers to deposition species 
      IF ( FIRST_TIME ) THEN
         FIRST_TIME = .FALSE.           

         ALLOCATE ( PA_DEPV_CONV( N_CGRID_SPC ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            MSG = 'Failure allocating PA_DEPV_CONV'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

         ! Set layer thickenesses
         CONVDD = 1.0 / ABS ( VGLVS_GD( 2 ) - VGLVS_GD( 1 ) ) 

         PA_DEPV_CONV = 1.0
         DO ICG = 1,N_CGRID_SPC
            ! Initialize DEPV conversion vector assuming all are gases
            IF ( CGRID_MASK_GAS( ICG ) .OR. 
     &           CGRID_MASK_NR( ICG )  .OR.
     &           CGRID_MASK_TRAC( ICG ) ) THEN
               ! Species is a gas
               ! g mol-1 air / g mol -1 species
               IF ( CGRID_MW( ICG ) .GT. 0. ) 
     &            PA_DEPV_CONV( ICG ) = MWAIR / CGRID_MW( ICG ) !* CONVDD

            ELSE IF ( CGRID_MASK_NUM( ICG ) .OR. 
     &                CGRID_MASK_SRF( ICG ) ) THEN
               ! Species is an aerosol number or surface area
               PA_DEPV_CONV( ICG ) = 1.0

            ELSEIF ( CGRID_MASK_AERO( ICG ) ) THEN
               ! Species is an aerosol mass (kg ha-1 -> ug ha-1)
               PA_DEPV_CONV( ICG ) = 1.0E+09
            END IF
         END DO
        
      END IF  ! First Time


C..get midpoint of time step
      MDATE = JDATE
      MTIME = JTIME
      CALL NEXTIME( MDATE, MTIME, SEC2TIME( TIME2SEC( TSTEP( 2 ) ) / 2 ) )

C..get density x jacobian and layer heights
      call interpolate_var ('DENS', mdate, mtime, DENS)
      call interpolate_var ('ZF', mdate, mtime, ZF)

      ! Convert DDEP to ppm, ug m-3, N m-3, and m2 m-3
      DDEP = DDEP * CONVH2M 

      DO ICG = 1,N_CGRID_SPC
         IF ( CGRID_MASK_AERO( ICG ) ) THEN
            DDEP(:,:,ICG) = DDEP(:,:,ICG) * PA_DEPV_CONV( ICG ) / ZF( :,:,1 )
         ELSE
            DDEP(:,:,ICG) = 1.0E+06 * DDEP(:,:,ICG) * PA_DEPV_CONV( ICG ) / DENS( :,:,1 ) / ZF(:,:,1)
         END IF
      END DO
 
      ! Save Changes in Budget Array
      CNGRD(:,:,1,: ) = -DDEP(:,:,:)
      CNGRD( :,:,2:NLAYS,: ) = 0.0
      CALL STORE_BUDGET( IPR_DDEP, CNGRD, JDATE, JTIME )

      IF ( LIPR .AND. MY_BEGLEV .EQ. 1 ) THEN
      ! Compute delta conc due to ddep and adjust vdiff output if necessary
      DO IPRV = 1, NIPRVAR
         IF ( MASK_IPR_PROC( IPRV,IPR_VDIF ) .OR. 
     &        MASK_IPR_PROC( IPRV,IPR_DDEP ) ) THEN
            ! Either VDIF or EMIS are needed for this IPR Variable.
            IPRSPC = MAP_IPRVARtoSPC( IPRV ) 

            DO ISPC = 1, NCGRID( IPRSPC )                       
               ICG = MAP_IPRtoCGRID( IPRSPC,ISPC )

#ifdef parallel
               CALL SUBST_DATA_COPY( DDEP, TDDEP, ICG )
#else
               TDDEP = DDEP( PA_BEGCOL:PA_ENDCOL,
     &                       PA_BEGROW:PA_ENDROW,
     &                       ICG )
#endif
               ! Adjust the process analysis output arrays
               IF ( MASK_IPR_PROC( IPRV,IPR_DDEP ) )
     &              DELC( :,:,1,IPRV ) = DELC( :,:,1,IPRV ) 
     &                    - TDDEP( :,: ) * SPCOEF( IPRSPC,ISPC )

               IF ( MASK_IPR_PROC( IPRV,IPR_VDIF ) )
     &              DELC( :,:,1,IPRV ) = DELC( :,:,1,IPRV ) 
     &                    + TDDEP( :,: ) * SPCOEF( IPRSPC,ISPC )
            END DO   ! ISPC
         END IF
      END DO   ! IPRV
      END IF

      RETURN

      END SUBROUTINE PA_UPDATE_DDEP

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE PA_UPDATE_HADV ( CGRID, JDATE, JTIME, TSTEP )

!-----------------------------------------------------------------------
      USE XY_BUDGET
      USE PA_DEFN
      USE UTILIO_DEFN
      USE PAGRD_DEFN
      USE CGRID_SPCS, ONLY : RHOJ_LOC

      IMPLICIT NONE

      REAL, POINTER         :: CGRID( :,:,:,: )  ! Conc array
      INTEGER, INTENT( IN ) :: JDATE, JTIME
      INTEGER, INTENT( IN ) :: TSTEP(3)
      INTEGER SDATE     ! Date at end of timestep
      INTEGER STIME     ! Time at end of timestep

      REAL :: TXADV  ( MY_PACOLS,MY_PAROWS,PALEVS )  ! Computed emission rate
      REAL :: TYADV  ( MY_PACOLS,MY_PAROWS,PALEVS )  ! Computed emission rate

      INTEGER :: IPRV
      INTEGER :: ICG, ISPC, IPRSPC, ISV
      LOGICAL,SAVE :: FIRST_TIME = .TRUE.

      ! Convert scalars from trasnport process units to mixing ratio and
      ! concentration units.
      CNGRD = CGRID
      SDATE = JDATE; STIME = JTIME
      CALL NEXTIME( SDATE, STIME, TSTEP( 2 ) )
      CALL DECOUPLE_PA( CNGRD, SDATE, STIME )

      ! Convert Xand Y Advection changes to mixing ratio and
      ! concentration units. The DECOUPLE_PA routine needs to have the
      ! correct RHOJ so this is passed from CNGRD
      DELC_XADV( :,:,:,RHOJ_LOC ) = CNGRD( :,:,:,RHOJ_LOC )
      CALL DECOUPLE_PA( DELC_XADV, SDATE, STIME )
      DELC_YADV( :,:,:,RHOJ_LOC ) = CNGRD( :,:,:,RHOJ_LOC )
      CALL DECOUPLE_PA( DELC_YADV, SDATE, STIME )

      ! Save Changes in Budget Array
      CALL STORE_BUDGET( IPR_XADV, DELC_XADV, JDATE, JTIME )
      CALL STORE_BUDGET( IPR_YADV, DELC_YADV, JDATE, JTIME )

      IF ( LIPR ) THEN
         DO IPRV = 1, NIPRVAR     ! foreach family
            IPRSPC = MAP_IPRVARtoSPC( IPRV ) 
            DO ISPC = 1,NCGRID( IPRSPC )      ! foreach species in the family
               ICG = MAP_IPRtoCGRID( IPRSPC,ISPC )     ! CGRID species index 

#ifdef parallel
               CALL SUBST_DATA_COPY( DELC_XADV, TXADV, ICG )
               CALL SUBST_DATA_COPY( DELC_YADV, TYADV, ICG )
#else
               TXADV = DELC_XADV( PA_BEGCOL:PA_ENDCOL,
     &                         PA_BEGROW:PA_ENDROW,
     &                         PA_BEGLEV:PA_ENDLEV, ICG )
               TYADV = DELC_YADV( PA_BEGCOL:PA_ENDCOL,
     &                         PA_BEGROW:PA_ENDROW,
     &                         PA_BEGLEV:PA_ENDLEV, ICG )
#endif

               ! Modify both the emiss process and the calling process
               IF ( MASK_IPR_PROC( IPRV,IPR_XADV ) ) THEN
                  DELC( :,:,:,IPRV ) = DELC( :,:,:,IPRV )
     &                               + SPCOEF( IPRSPC,ISPC ) * TXADV
               END IF

               IF ( MASK_IPR_PROC( IPRV,IPR_YADV ) ) THEN
                  DELC( :,:,:,IPRV ) = DELC( :,:,:,IPRV )
     &                               + SPCOEF( IPRSPC,ISPC ) * TYADV
               END IF
            END DO
         END DO
      END IF
 
      CSAV = CNGRD
      RETURN
 
      END SUBROUTINE PA_UPDATE_HADV
 
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE PA_UPDATE_AERO ( CGRID, JDATE, JTIME )

!-----------------------------------------------------------------------
      USE AERO_BUDGET
      USE PA_DEFN
      USE UTILIO_DEFN
      USE PAGRD_DEFN


      IMPLICIT NONE

      REAL, POINTER         :: CGRID( :,:,:,: )  ! Conc array
      INTEGER, INTENT( IN ) :: JDATE, JTIME

      INTEGER :: ICG, IPRV, IPRSPC, ISPC, ISV
      LOGICAL,SAVE :: FIRST_TIME = .TRUE.

      REAL :: TCOAG  ( MY_PACOLS,MY_PAROWS,PALEVS )  ! Computed emission ratedd
      REAL :: TCOND  ( MY_PACOLS,MY_PAROWS,PALEVS )  ! Computed emission rate
      REAL :: TNPF   ( MY_PACOLS,MY_PAROWS,PALEVS )  ! Computed emission rate
      REAL :: TGROW  ( MY_PACOLS,MY_PAROWS,PALEVS )  ! Computed emission rate

      ! Allocate some arrays the first time through
      IF ( FIRST_TIME ) THEN
          FIRST_TIME = .FALSE.
      END IF

      ! Save Changes in Budget Array
      CNGRD = CGRID
      CALL STORE_BUDGET( IPR_COAG, AERO_COAG, JDATE, JTIME )
      CALL STORE_BUDGET( IPR_COND, AERO_COND, JDATE, JTIME )
      CALL STORE_BUDGET( IPR_NPF,  AERO_NPF, JDATE, JTIME )
      CALL STORE_BUDGET( IPR_GROW, AERO_GROWTH, JDATE, JTIME )

      IF ( LIPR ) THEN
      DO IPRV = 1, NIPRVAR     ! foreach family
         IPRSPC = MAP_IPRVARtoSPC( IPRV ) 
         DO ISPC = 1,NCGRID( IPRSPC )      ! foreach species in the family
            ICG = MAP_IPRtoCGRID( IPRSPC,ISPC )     ! CGRID species index 

#ifdef parallel
            CALL SUBST_DATA_COPY( AERO_COAG, TCOAG, ICG )
            CALL SUBST_DATA_COPY( AERO_COND, TCOND, ICG )
            CALL SUBST_DATA_COPY( AERO_NPF,  TNPF,  ICG )
            CALL SUBST_DATA_COPY( AERO_GROWTH, TGROW, ICG )
#else
            TCOAG = SUM( AERO_COAG( PA_BEGCOL:PA_ENDCOL,
     &                         PA_BEGROW:PA_ENDROW,
     &                         PA_BEGLEV:PA_ENDLEV, ICG,: ),4 )
            TCOND = AERO_COND( PA_BEGCOL:PA_ENDCOL,
     &                         PA_BEGROW:PA_ENDROW,
     &                         PA_BEGLEV:PA_ENDLEV, ICG )
            TNPF  = AERO_NPF ( PA_BEGCOL:PA_ENDCOL,
     &                         PA_BEGROW:PA_ENDROW,
     &                         PA_BEGLEV:PA_ENDLEV, ICG )
            TGROW = AERO_GROWTH( PA_BEGCOL:PA_ENDCOL,
     &                         PA_BEGROW:PA_ENDROW,
     &                         PA_BEGLEV:PA_ENDLEV, ICG )
#endif

            ! Modify both the emiss process and the calling process
            IF ( MASK_IPR_PROC( IPRV,IPR_COAG ) )
     &         DELC( :,:,:,IPRV ) = DELC( :,:,:,IPRV )
     &                           + SPCOEF( IPRSPC,ISPC ) * TCOAG
            IF ( MASK_IPR_PROC( IPRV,IPR_COND ) )
     &         DELC( :,:,:,IPRV ) = DELC( :,:,:,IPRV )
     &                           + SPCOEF( IPRSPC,ISPC ) * TCOND
            IF ( MASK_IPR_PROC( IPRV,IPR_NPF ) )
     &         DELC( :,:,:,IPRV  ) = DELC( :,:,:,IPRV )
     &                           + SPCOEF( IPRSPC,ISPC ) * TNPF
            IF ( MASK_IPR_PROC( IPRV,IPR_GROW ) ) 
     &         DELC( :,:,:,IPRV )= DELC( :,:,:,IPRV )
     &                           + SPCOEF( IPRSPC,ISPC ) * TGROW
         END DO
      END DO
      END IF
  
      CSAV = CNGRD
      RETURN
 
      END SUBROUTINE PA_UPDATE_AERO

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE DECOUPLE_PA ( CONC, JDATE, JTIME )
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
      USE VDIFF_MAP, ONLY : N_SPC_DIFF, DIFF_MASK_SRF, DIFF_MASK_NUM, DIFF_MAP,
     &                      DIFF_MASK_AERO
      use CENTRALIZED_IO_MODULE, only : interpolate_var

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
      CHARACTER( 16 ) :: PNAME = 'DECOUPLE_PA'
      CHARACTER( 16 ) :: VNAME
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER     V  ! loop counters

C-----------------------------------------------------------------------

C retrieve transported RhoJ
      RRHOJ( :,:,: ) = CONC( :,:,:,RHOJ_LOC )
      RRHOJ = 1.0 / RRHOJ   ! array assignment

      call interpolate_var ('JACOBM', jdate, jtime, RJACOBM)

      RJACOBM = 1.0 / RJACOBM   ! array assignment
 
C decouple for chemistry and diffusion
C The CONC array is ordered like CGRID but only the DIFF species should
C be modified. Use DIFF_MAP
      DO V = 1,N_SPC_DIFF
          IF ( DIFF_MASK_NUM( V ) .OR. DIFF_MASK_SRF( V ) ) THEN 
            CONC( :,:,:,DIFF_MAP( V ) ) = 
     &          CONC( :,:,:,DIFF_MAP( V ) ) * RJACOBM( :,:,: )
          ELSE IF ( DIFF_MASK_AERO( V ) ) THEN
            CONC( :,:,:,DIFF_MAP( V ) ) = 
     &          CONC( :,:,:,DIFF_MAP( V ) ) * CONV * RJACOBM( :,:,: )
          ELSE
            CONC( :,:,:,DIFF_MAP( V ) ) = 
     &          CONC( :,:,:,DIFF_MAP( V ) ) * RRHOJ( :,:,: )
          END IF
      END DO

      RETURN

      END SUBROUTINE DECOUPLE_PA

      END MODULE PA_UPDATE
