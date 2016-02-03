
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
C $Header: /project/yoj/arc/CCTM/src/hadv/yamo/advbc_map.F,v 1.6 2012/01/19 14:50:30 yoj Exp $
 
C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%
 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE ADVBC_MAP ( CONCMIN, BCNAME, BCFAC )
 
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
C    3 Sep 13 J.Young: change ICBC_FAC policy to assign factor, if specified;
C                      requires a surrogate character or string to be specified
C-----------------------------------------------------------------------

      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN

      IMPLICIT NONE
 
C Includes:
      INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:
      CHARACTER( 16 ), INTENT( IN )    :: CONCMIN     ! min value allowed for BCs
      CHARACTER( 16 ), INTENT( INOUT ) :: BCNAME( : ) ! BC name for adv species
      REAL,            INTENT( INOUT ) :: BCFAC ( : ) ! Scale factor for BCs

C External Functions:
      INTEGER, EXTERNAL :: FINDEX

C Local variables:
      CHARACTER( 16 ) :: PNAME = 'ADVBC_MAP'
      CHARACTER( 16 ) :: BLNK = ' '
!     CHARACTER( 16 ) :: BLNK = '                '
      CHARACTER( 96 ) :: XMSG = ' '
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: WRVAR( : )

      INTEGER    WRCNT                ! no. of species not on file
      INTEGER :: XDATE = 999999       ! dummy date
      INTEGER :: XTIME = 999999       ! dummy time
      INTEGER    INDX                 ! Species index
      INTEGER    ISUR                 ! Surrogate index
      INTEGER    SPC, VAR             ! loop copunters
      INTEGER    LOGDEV, IERR
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
#ifdef Verbose3
      logical, save :: onetime = .true.
      logical firstwrite
#endif

C Map advected species to bc`s

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         LOGDEV = INIT3 ()

         ALLOCATE ( WRVAR( N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_ADV ), STAT = IERR )
         IF ( IERR .NE. 0 ) THEN
            XMSG = 'Error allocating WRVAR'
            CALL M3EXIT( PNAME, XDATE, XTIME, XMSG, XSTAT1 )
         END IF

      END IF
 
      SPC = 0
 
C Map advected reactive gas species to bc`s

      IF ( N_GC_TRNS .GT. 0 ) THEN

         IF ( .NOT. OPEN3( BNDY_GASC_1, FSREAD3, PNAME ) ) THEN
            XMSG = 'Could not open ' // BNDY_GASC_1 // ' file'
            CALL M3EXIT( PNAME, XDATE, XTIME, XMSG, XSTAT1 )
         END IF
 
         IF ( .NOT. DESC3( BNDY_GASC_1 ) ) THEN
            XMSG = 'Could not get ' // BNDY_GASC_1 // ' file description'
            CALL M3EXIT( PNAME, XDATE, XTIME, XMSG, XSTAT1 )
         END IF

#ifdef Verbose3
         firstwrite = .true.
#endif
         WRCNT = 0
         DO VAR = 1, N_GC_TRNS
            SPC = SPC + 1
            BCFAC( SPC ) = 1.0
C is the (main) species name on the BC file?
            indx = index1( gc_trns( var ), nvars3d, vname3d )
            if ( indx .eq. 0 ) then             ! name not on file, check if surrogate is
               isur = findex( gc_trns_map( var ), n_gc_icbc, gc_icbc_map )
               if ( isur .eq. 0 ) then          ! surrogate not in ICBC table either
                  bcname( spc ) = blnk
                  wrcnt = wrcnt + 1
                  wrvar( wrcnt ) = gc_trns( var )
               else                             ! surrogate found in ICBC table
                  indx = index1( gc_icbc( isur), nvars3d, vname3d )
                  if ( indx .ne. 0 ) then       ! surrogate found on file
                     bcname( spc ) = vname3d( indx )
                     bcfac( spc ) = gc_icbc_fac( isur )
                  else
                     bcname( spc ) = blnk
                     wrcnt = wrcnt + 1
                     wrvar( wrcnt ) = gc_trns( var )
                  end if
               end if
            else                                ! name on file
               bcname( spc ) = vname3d( indx )
               isur = findex( gc_trns_map( var ), n_gc_icbc, gc_icbc_map )
               if ( isur .ne. 0 ) then
                  bcfac( spc ) = gc_icbc_fac( isur )
               end if
            end if

#ifdef Verbose3
            if ( onetime ) then
               if ( firstwrite ) then
                  firstwrite = .false.
                  write( logdev,1031 )
               end if
               if ( bcname( spc ) .ne. blnk ) then
                  write( logdev,1033 ) spc, bcname( spc ), bcfac( spc )
               else
                  write( logdev,1033 ) spc, 'not on file'
               end if
            end if
1031  format( ' =b=', 3x, 'spc', 6x, 'name', 15x, 'bcfac' )
1033  format( ' =b=', i5, 6x, a, 1pg13.5 )
#endif

         END DO

         IF ( WRCNT .GT. 0 ) THEN
            WRITE( LOGDEV,1019 ) TRIM( BNDY_GASC_1 ), TRIM( CONCMIN )
            DO VAR = 1, WRCNT
               WRITE( LOGDEV,1025 ) TRIM( WRVAR( VAR ) )
            END DO
         END IF

      END IF

C Map advected aerosol species to bc`s

      IF ( N_AE_TRNS .GT. 0 ) THEN

         IF ( .NOT. OPEN3( BNDY_AERO_1, FSREAD3, PNAME ) ) THEN
            XMSG = 'Could not open ' // BNDY_AERO_1 // ' file'
            CALL M3EXIT( PNAME, XDATE, XTIME, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. DESC3( BNDY_AERO_1 ) ) THEN
            XMSG = 'Could not get ' // BNDY_AERO_1 // ' file description'
            CALL M3EXIT( PNAME, XDATE, XTIME, XMSG, XSTAT1 )
         END IF

#ifdef Verbose3
         firstwrite = .true.
#endif
         WRCNT = 0
         DO VAR = 1, N_AE_TRNS
            SPC = SPC + 1
            BCFAC( SPC ) = 1.0
C is the (main) species name on the BC file?
            indx = index1( ae_trns( var ), nvars3d, vname3d )
            if ( indx .eq. 0 ) then             ! name not on file, check if surrogate is
               isur = findex( ae_trns_map( var ), n_ae_icbc, ae_icbc_map )
               if ( isur .eq. 0 ) then          ! surrogate not in ICBC table either
                  bcname( spc ) = blnk
                  wrcnt = wrcnt + 1
                  wrvar( wrcnt ) = ae_trns( var )
               else                             ! surrogate found in ICBC table
                  indx = index1( ae_icbc( isur), nvars3d, vname3d )
                  if ( indx .ne. 0 ) then       ! surrogate found on file
                     bcname( spc ) = vname3d( indx )
                     bcfac( spc ) = ae_icbc_fac( isur )
                  else
                     bcname( spc ) = blnk
                     wrcnt = wrcnt + 1
                     wrvar( wrcnt ) = ae_trns( var )
                  end if
               end if
            else                                ! name on file
               bcname( spc ) = vname3d( indx )
               isur = findex( ae_trns_map( var ), n_ae_icbc, ae_icbc_map )
               if ( isur .ne. 0 ) then
                  bcfac( spc ) = ae_icbc_fac( isur )
               end if
            end if

#ifdef Verbose3
            if ( onetime ) then
               if ( firstwrite ) then
                  firstwrite = .false.
                  write( logdev,1031 )
               end if
               if ( bcname( spc ) .ne. blnk ) then
                  write( logdev,1033 ) spc, bcname( spc ), bcfac( spc )
               else
                  write( logdev,1033 ) spc, 'not on file'
               end if
            end if
#endif

         END DO
         IF ( WRCNT .GT. 0 ) THEN
            WRITE( LOGDEV,1019 ) TRIM( BNDY_AERO_1 ), TRIM( CONCMIN )
            DO VAR = 1, WRCNT
               WRITE( LOGDEV,1025 ) TRIM( WRVAR( VAR ) )
            END DO
         END IF 

      END IF
  
C Map advected non-reactive species to bc`s

      IF ( N_NR_TRNS .GT. 0 ) THEN

         IF ( .NOT. OPEN3( BNDY_NONR_1, FSREAD3, PNAME ) ) THEN
            XMSG = 'Could not open ' // BNDY_NONR_1 // ' file'
            CALL M3EXIT( PNAME, XDATE, XTIME, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. DESC3( BNDY_NONR_1 ) ) THEN
            XMSG = 'Could not get ' // BNDY_NONR_1 // ' file description'
            CALL M3EXIT( PNAME, XDATE, XTIME, XMSG, XSTAT1 )
         END IF

#ifdef Verbose3
         firstwrite = .true.
#endif
         WRCNT = 0
         DO VAR = 1, N_NR_TRNS
            SPC = SPC + 1
            BCFAC( SPC ) = 1.0
C is the (main) species name on the BC file?
            indx = index1( nr_trns( var ), nvars3d, vname3d )
            if ( indx .eq. 0 ) then             ! name not on file, check if surrogate is
               isur = findex( nr_trns_map( var ), n_nr_icbc, nr_icbc_map )
               if ( isur .eq. 0 ) then          ! surrogate not in ICBC table either
                  bcname( spc ) = blnk
                  wrcnt = wrcnt + 1
                  wrvar( wrcnt ) = nr_trns( var )
               else                             ! surrogate found in ICBC table
                  indx = index1( nr_icbc( isur), nvars3d, vname3d )
                  if ( indx .ne. 0 ) then       ! surrogate found on file
                     bcname( spc ) = vname3d( indx )
                     bcfac( spc ) = nr_icbc_fac( isur )
                  else
                     bcname( spc ) = blnk
                     wrcnt = wrcnt + 1
                     wrvar( wrcnt ) = nr_trns( var )
                  end if
               end if
            else                                ! name on file
               bcname( spc ) = vname3d( indx )
               isur = findex( nr_trns_map( var ), n_nr_icbc, nr_icbc_map )
               if ( isur .ne. 0 ) then
                  bcfac( spc ) = nr_icbc_fac( isur )
               end if
            end if

#ifdef Verbose3
            if ( onetime ) then
               if ( firstwrite ) then
                  firstwrite = .false.
                  write( logdev,1031 )
               end if
               if ( bcname( spc ) .ne. blnk ) then
                  write( logdev,1033 ) spc, bcname( spc ), bcfac( spc )
               else
                  write( logdev,1033 ) spc, 'not on file'
               end if
            end if
#endif

         END DO
         IF ( WRCNT .GT. 0 ) THEN
            WRITE( LOGDEV,1019 ) TRIM( BNDY_NONR_1 ), TRIM( CONCMIN )
            DO VAR = 1, WRCNT
               WRITE( LOGDEV,1025 ) TRIM( WRVAR( VAR ) )
            END DO
         END IF 

      END IF
  
C Map advected tracer species to bc`s

      IF ( N_TR_ADV .GT. 0 ) THEN

         IF ( .NOT. OPEN3( BNDY_TRAC_1, FSREAD3, PNAME ) ) THEN
            XMSG = 'Could not open ' // BNDY_TRAC_1 // ' file'
            CALL M3EXIT( PNAME, XDATE, XTIME, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. DESC3( BNDY_TRAC_1 ) ) THEN
            XMSG = 'Could not get ' // BNDY_TRAC_1 // ' file description'
            CALL M3EXIT( PNAME, XDATE, XTIME, XMSG, XSTAT1 )
         END IF

#ifdef Verbose3
         firstwrite = .true.
#endif
         WRCNT = 0
         DO VAR = 1, N_TR_ADV
            SPC = SPC + 1
            BCFAC( SPC ) = 1.0
C is the (main) species name on the BC file?
            indx = index1( tr_adv( var ), nvars3d, vname3d )
            if ( indx .eq. 0 ) then             ! name not on file, check if surrogate is
               isur = findex( tr_adv_map( var ), n_tr_icbc, tr_icbc_map )
               if ( isur .eq. 0 ) then          ! surrogate not in ICBC table either
                  bcname( spc ) = blnk
                  wrcnt = wrcnt + 1
                  wrvar( wrcnt ) = tr_adv( var )
               else                             ! surrogate found in ICBC table
                  indx = index1( tr_icbc( isur), nvars3d, vname3d )
                  if ( indx .ne. 0 ) then       ! surrogate found on file
                     bcname( spc ) = vname3d( indx )
                     bcfac( spc ) = tr_icbc_fac( isur )
                  else
                     bcname( spc ) = blnk
                     wrcnt = wrcnt + 1
                     wrvar( wrcnt ) = tr_adv( var )
                  end if
               end if
            else                                ! name on file
               bcname( spc ) = vname3d( indx )
               isur = findex( tr_adv_map( var ), n_tr_icbc, tr_icbc_map )
               if ( isur .ne. 0 ) then
                  bcfac( spc ) = tr_icbc_fac( isur )
               end if
            end if

#ifdef Verbose3
            if ( onetime ) then
               if ( firstwrite ) then
                  firstwrite = .false.
                  write( logdev,1031 )
               end if
               if ( bcname( spc ) .ne. blnk ) then
                  write( logdev,1033 ) spc, bcname( spc ), bcfac( spc )
               else
                  write( logdev,1033 ) spc, 'not on file'
               end if
            end if
#endif

         END DO
         IF ( WRCNT .GT. 0 ) THEN
            WRITE( LOGDEV,1019 ) TRIM( BNDY_TRAC_1 ), TRIM( CONCMIN )
            DO VAR = 1, WRCNT
               WRITE( LOGDEV,1025 ) TRIM( WRVAR( VAR ) )
            END DO
         END IF 

#ifdef Verbose3
      onetime = .false.
#endif

      END IF

      RETURN

1019  FORMAT( / 5X, "No BC's in file ", A, " for the following adv species:",
     &          1X, "Set to ", A )
1025  FORMAT( 10X, A )

      END
