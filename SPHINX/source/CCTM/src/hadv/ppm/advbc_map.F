
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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE ADVBC_MAP ( CONCMIN, BCNAME, BCFAC, ONFILE )
 
C-----------------------------------------------------------------------
C Function:
C   Set up advected species mapping to BCON data - read variable BCNAME
C   from file - if BCNAME is blank, skip read and zero out the BCON array
C   for the variable corresponding to the blank BCNAME
C   BCNAME returned is either the file variable name, the file name
C   corresponding to the ICBC surrogate nate, or blank

C Preconditions:

C Subroutines and functions called:

C   OPEN3, DESC3, INDEX1, TRIMLEN, FINDEX, M3MESG, M3EXIT

C Revision history:
C   Jeff - Aug 1997 Based on beta version
C   21 Jun 10 J.Young: convert for Namelist redesign
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                      removed deprecated TRIMLEN
C   10 Sep 11 J.Young: changed Namelist surrogate policy to enable use of
C                      icbc factors
C   21 Oct 16 D.Wong: If both surrogate and species are on the BC file, write to
C                     the log that the surrogate is used.
C   01 Feb 19 D.Wong: Implemented centralized I/O approach
C-----------------------------------------------------------------------

      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN
      USE CENTRALIZED_IO_MODULE, only : n_cio_bc_file_vars, cio_bc_file_var_name

      IMPLICIT NONE
 
C Includes:
      INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:
      CHARACTER( 16 ), INTENT( IN )  :: CONCMIN     ! min value allowed for BCs
      CHARACTER( 16 ), INTENT( OUT ) :: BCNAME( : ) ! BC name for adv species
      REAL,            INTENT( OUT ) :: BCFAC ( : ) ! Scale factor for BCs
      LOGICAL,         INTENT( OUT ) :: ONFILE( : ) ! BCs exist on file for this name

C External Functions:
      INTEGER, EXTERNAL :: FINDEX

C Local variables:
      CHARACTER( 16 ) :: PNAME = 'ADVBC_MAP'
!     CHARACTER( 16 ) :: BLNK = ' '
!     CHARACTER( 16 ) :: BLNK = '                '
      CHARACTER( 96 ) :: XMSG = ' '
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: WRVAR( : )

      INTEGER    WRCNT                ! no. of species not on file
      INTEGER :: XDATE = 999999       ! dummy date
      INTEGER :: XTIME = 999999       ! dummy time
      INTEGER    INDX                 ! Species index
      INTEGER    ISUR                 ! Surrogate index
      INTEGER    ITRNS                ! Transport species index
      INTEGER    SPC, VAR             ! loop copunters
      INTEGER    IERR
      LOGICAL, SAVE :: FIRSTIME = .TRUE.

C Map advected species to bc`s

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

         ALLOCATE ( WRVAR( N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_ADV ), STAT = IERR )
         IF ( IERR .NE. 0 ) THEN
            XMSG = 'Error allocating WRVAR'
            CALL M3EXIT( PNAME, XDATE, XTIME, XMSG, XSTAT1 )
         END IF

      END IF
 
      SPC = 0
 
      ! Map advected reactive gas species to bc`s

      IF ( N_GC_TRNS .GT. 0 ) THEN

         WRCNT = 0
         DO VAR = 1, N_GC_TRNS
            SPC = SPC + 1
            ONFILE( SPC ) = .TRUE.
            BCFAC( SPC ) = 1.0
            INDX = 0

            ISUR = FINDEX( GC_TRNS_MAP( VAR ), N_GC_BC, GC_BC_MAP )

            IF ( ISUR .NE. 0 ) THEN   ! variable has a surrogate name
               INDX = INDEX1( GC_BC( ISUR ), n_cio_bc_file_vars, cio_bc_file_var_name )
               IF ( INDX .NE. 0 ) THEN   ! BC file surrogate is used
                  BCNAME( SPC ) = cio_bc_file_var_name( INDX )
                  BCFAC( SPC ) = GC_BC_FAC( ISUR )
               END IF
            END IF

            ! If there is no surrogate or it can`t be found, look 
            ! for the adv species on the BC file
            IF ( ISUR .EQ. 0 .OR. INDX .EQ. 0 ) THEN
               ITRNS = INDEX1( GC_TRNS( VAR ), n_cio_bc_file_vars, cio_bc_file_var_name )
               IF ( ITRNS .NE. 0 ) THEN
                  BCNAME( SPC ) = cio_bc_file_var_name( ITRNS )
               ELSE
                  ONFILE( SPC ) = .FALSE.
                  WRCNT = WRCNT + 1
                  WRVAR( WRCNT ) = GC_TRNS( VAR )
               END IF
            END IF

#ifdef verbose_rdbcon
            write( logdev,* ) '=b=spc,name: ', spc, bcname( spc ),
     &                        '   bcfac:', bcfac( spc )
            write( logdev,* ) ' '
#endif

         END DO

         IF ( WRCNT .GT. 0 ) THEN
            WRITE( LOGDEV,1019 ) TRIM( BNDY_CONC_1 ), TRIM( CONCMIN )
            DO VAR = 1, WRCNT
               WRITE( LOGDEV,1025 ) TRIM( WRVAR( VAR ) )
            END DO
         END IF

      END IF

      ! Map advected aerosol species to bc`s

      IF ( N_AE_TRNS .GT. 0 ) THEN

         WRCNT = 0
         DO VAR = 1, N_AE_TRNS
            SPC = SPC + 1
            ONFILE( SPC ) = .TRUE.
            BCFAC( SPC ) = 1.0
            INDX = 0

            ISUR = FINDEX( AE_TRNS_MAP( VAR ), N_AE_BC, AE_BC_MAP )

            IF ( ISUR .NE. 0 ) THEN   ! variable has a surrogate name
               INDX = INDEX1( AE_BC( ISUR ), n_cio_bc_file_vars, cio_bc_file_var_name )
               IF ( INDX .NE. 0 ) THEN   ! BC file surrogate is used
                  BCNAME( SPC ) = cio_bc_file_var_name( INDX )
                  BCFAC( SPC ) = AE_BC_FAC( ISUR )
               END IF
            END IF
            
            ! If there is no surrogate or it can`t be found, look 
            ! for the adv species on the BC file
            IF ( ISUR .EQ. 0 .OR. INDX .EQ. 0 ) THEN
               ITRNS = INDEX1( AE_TRNS( VAR ), n_cio_bc_file_vars, cio_bc_file_var_name )
               IF ( ITRNS .NE. 0 ) THEN
                  BCNAME( SPC ) = cio_bc_file_var_name( ITRNS )
               ELSE
                  ONFILE( SPC ) = .FALSE.
                  WRCNT = WRCNT + 1
                  WRVAR( WRCNT ) = AE_TRNS( VAR )
               END IF
            END IF

#ifdef verbose_rdbcon
            write( logdev,* ) '=b=spc,name: ', spc, bcname( spc ),
     &                        '   bcfac:', bcfac( spc )
            write( logdev,* ) ' '
#endif

         END DO

         IF ( WRCNT .GT. 0 ) THEN
            WRITE( LOGDEV,1019 ) TRIM( BNDY_CONC_1 ), TRIM( CONCMIN )
            DO VAR = 1, WRCNT
               WRITE( LOGDEV,1025 ) TRIM( WRVAR( VAR ) )
            END DO
         END IF 

      END IF
  
      ! Map advected non-reactive species to bc`s

      IF ( N_NR_TRNS .GT. 0 ) THEN

         WRCNT = 0
         DO VAR = 1, N_NR_TRNS
            SPC = SPC + 1
            ONFILE( SPC ) = .TRUE.
            BCFAC( SPC ) = 1.0
            INDX = 0
            
            ISUR = FINDEX( NR_TRNS_MAP( VAR ), N_NR_BC, NR_BC_MAP )

            IF ( ISUR .NE. 0 ) THEN   ! variable has a surrogate name
               INDX = INDEX1( NR_BC( ISUR ), n_cio_bc_file_vars, cio_bc_file_var_name )
               IF ( INDX .NE. 0 ) THEN   ! BC file surrogate is used
                  BCNAME( SPC ) = cio_bc_file_var_name( INDX )
                  BCFAC( SPC ) = NR_BC_FAC( ISUR )
               END IF
            END IF

            ! If there is no surrogate or it can`t be found, look 
            ! for the adv species on the BC file
            IF ( ISUR .EQ. 0 .OR. INDX .EQ. 0 ) THEN
               ITRNS = INDEX1( NR_TRNS( VAR ), n_cio_bc_file_vars, cio_bc_file_var_name )
               IF ( ITRNS .NE. 0 ) THEN
                  BCNAME( SPC ) = cio_bc_file_var_name( ITRNS )
               ELSE
                  ONFILE( SPC ) = .FALSE.
                  WRCNT = WRCNT + 1
                  WRVAR( WRCNT ) = NR_TRNS( VAR )
               END IF
            END IF

#ifdef verbose_rdbcon
            write( logdev,* ) '=b=spc,name: ', spc, bcname( spc ),
     &                        '   bcfac:', bcfac( spc )
            write( logdev,* ) ' '
#endif

         END DO

         IF ( WRCNT .GT. 0 ) THEN
            WRITE( LOGDEV,1019 ) TRIM( BNDY_CONC_1 ), TRIM( CONCMIN )
            DO VAR = 1, WRCNT
               WRITE( LOGDEV,1025 ) TRIM( WRVAR( VAR ) )
            END DO
         END IF 

      END IF
  
      ! Map advected tracer species to bc`s

      IF ( N_TR_ADV .GT. 0 ) THEN

         WRCNT = 0
         DO VAR = 1, N_TR_ADV
            SPC = SPC + 1
            ONFILE( SPC ) = .TRUE.
            BCFAC( SPC ) = 1.0
            INDX = 0

            ISUR = FINDEX( TR_ADV_MAP( VAR ), N_TR_BC, TR_BC_MAP )

            IF ( ISUR .NE. 0 ) THEN   ! variable has a surrogate name
               INDX = INDEX1( TR_BC( ISUR ), n_cio_bc_file_vars, cio_bc_file_var_name )
               IF ( INDX .NE. 0 ) THEN   ! BC file surrogate is used
                  BCNAME( SPC ) = cio_bc_file_var_name( INDX )
                  BCFAC( SPC ) = TR_BC_FAC( ISUR )
               END IF
            END IF

            ! If there is no surrogate or it can`t be found, look 
            ! for the adv species on the BC file
            IF ( ISUR .EQ. 0 .OR. INDX .EQ. 0 ) THEN
               ITRNS = INDEX1( TR_ADV( VAR ), n_cio_bc_file_vars, cio_bc_file_var_name )
               IF ( ITRNS .NE. 0 ) THEN
                  BCNAME( SPC ) = cio_bc_file_var_name( ITRNS )
               ELSE
                  ONFILE( SPC ) = .FALSE.
                  WRCNT = WRCNT + 1
                  WRVAR( WRCNT ) = TR_ADV( VAR )
               END IF
            END IF

#ifdef verbose_rdbcon
            write( logdev,* ) '=b=spc,name: ', spc, bcname( spc ),
     &                        '   bcfac:', bcfac( spc )
            write( logdev,* ) ' '
#endif

         END DO

         IF ( WRCNT .GT. 0 ) THEN
            WRITE( LOGDEV,1019 ) TRIM( BNDY_CONC_1 ), TRIM( CONCMIN )
            DO VAR = 1, WRCNT
               WRITE( LOGDEV,1025 ) TRIM( WRVAR( VAR ) )
            END DO
         END IF 

      END IF

      RETURN

1015  FORMAT( / 5X, "BC file surrogate ", A, " used instead of adv species, ", A )
1019  FORMAT( / 5X, "No BC's in file " A, " for the following adv species:"
     &          1X, "Set to " A )
1025  FORMAT( 10X, A )

      END
