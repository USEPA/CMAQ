
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/hadv/yamo/advbc_map.F,v 1.4 2011/10/20 15:59:10 yoj Exp $
 
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

         WRCNT = 0
         DO VAR = 1, N_GC_TRNS
            SPC = SPC + 1
            BCFAC( SPC ) = 1.0
C is there a surrogate name?
            ISUR = INDEX1( GC_TRNS( VAR ), N_GC_ICBC, GC_ICBC )
!           write( logdev,* ) '=b=var,spc,isur: ', var, spc, isur
            IF ( ISUR .NE. 0 ) THEN
C is it on the BC file?
               INDX = INDEX1( GC_ICBC( ISUR ), NVARS3D, VNAME3D )
               IF ( INDX .NE. 0 ) THEN
C if there`s a surrogate name, CGRID_SPCS will handle setting GC_ICBC_FAC
                  BCNAME( SPC ) = VNAME3D( INDX )
               ELSE   ! not on the BC file
                  BCNAME( SPC ) = BLNK
                  WRCNT = WRCNT + 1
                  WRVAR( WRCNT ) = GC_TRNS( VAR )
               END IF
               BCFAC( SPC ) = GC_ICBC_FAC( ISUR )
!              write( logdev,* ) '=b=_fac in sur: ', bcfac( spc )
            ELSE
C is the (main) species name on the BC file?
               INDX = INDEX1( GC_TRNS( VAR ), NVARS3D, VNAME3D )
               IF ( INDX .NE. 0 ) THEN
                  BCNAME( SPC ) = VNAME3D( INDX )
               ELSE
                  BCNAME( SPC ) = BLNK
                  WRCNT = WRCNT + 1
                  WRVAR( WRCNT ) = GC_TRNS( VAR )
               END IF
            END IF

#ifdef Verbose3
            write( logdev,* ) '=b=spc,name: ', spc, trim( bcname( spc ) ),
     &                        '   bcfac:', bcfac( spc )
            write( logdev,* ) ' '
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

         WRCNT = 0
         DO VAR = 1, N_AE_TRNS
            SPC = SPC + 1
            BCFAC( SPC ) = 1.0
C is there a surrogate name?
            ISUR = INDEX1( AE_TRNS( VAR ), N_AE_ICBC, AE_ICBC )
!           write( logdev,* ) '=b=var,spc,isur: ', var, spc, isur
            IF ( ISUR .NE. 0 ) THEN
C is it on the BC file?
               INDX = INDEX1( AE_ICBC( ISUR ), NVARS3D, VNAME3D )
               IF ( INDX .NE. 0 ) THEN
                  BCNAME( SPC ) = VNAME3D( INDX )
               ELSE   ! not on the BC file
                  BCNAME( SPC ) = BLNK
                  WRCNT = WRCNT + 1
                  WRVAR( WRCNT ) = AE_TRNS( VAR )
               END IF
               BCFAC( SPC ) = AE_ICBC_FAC( ISUR )
!              write( logdev,* ) '=b=_fac in sur: ', bcfac( spc )
            ELSE
C is the (main) species name on the BC file?
               INDX = INDEX1( AE_TRNS( VAR ), NVARS3D, VNAME3D )
               IF ( INDX .NE. 0 ) THEN
                  BCNAME( SPC ) = VNAME3D( INDX )
               ELSE
                  BCNAME( SPC ) = BLNK
                  WRCNT = WRCNT + 1
                  WRVAR( WRCNT ) = AE_TRNS( VAR )
               END IF
            END IF

#ifdef Verbose3
            write( logdev,* ) '=b=spc,name: ', spc, trim( bcname( spc ) ),
     &                        '   bcfac:', bcfac( spc )
            write( logdev,* ) ' '
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

         WRCNT = 0
         DO VAR = 1, N_NR_TRNS
            SPC = SPC + 1
            BCFAC( SPC ) = 1.0
C is there a surrogate name?
            ISUR = INDEX1( NR_TRNS( VAR ), N_NR_ICBC, NR_ICBC )
!           write( logdev,* ) '=b=var,spc,isur: ', var, spc, isur
            IF ( ISUR .NE. 0 ) THEN
C is it on the BC file?
               INDX = INDEX1( NR_ICBC( ISUR ), NVARS3D, VNAME3D )
               IF ( INDX .NE. 0 ) THEN
                  BCNAME( SPC ) = VNAME3D( INDX )
               ELSE   ! not on the BC file
                  BCNAME( SPC ) = BLNK
                  WRCNT = WRCNT + 1
                  WRVAR( WRCNT ) = NR_TRNS( VAR )
               END IF
               BCFAC( SPC ) = NR_ICBC_FAC( ISUR )
!              write( logdev,* ) '=b=_fac in sur: ', bcfac( spc )
            ELSE
C is the (main) species name on the BC file?
               INDX = INDEX1( NR_TRNS( VAR ), NVARS3D, VNAME3D )
               IF ( INDX .NE. 0 ) THEN
                  BCNAME( SPC ) = VNAME3D( INDX )
               ELSE
                  BCNAME( SPC ) = BLNK
                  WRCNT = WRCNT + 1
                  WRVAR( WRCNT ) = NR_TRNS( VAR )
               END IF
            END IF

#ifdef Verbose3
            write( logdev,* ) '=b=spc,name: ', spc, trim( bcname( spc ) ),
     &                        '   bcfac:', bcfac( spc )
            write( logdev,* ) ' '
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

         WRCNT = 0
         DO VAR = 1, N_TR_ADV
            SPC = SPC + 1
            BCFAC( SPC ) = 1.0
C is there a surrogate name?
            ISUR = INDEX1( TR_ADV( VAR ), N_TR_ICBC, TR_ICBC )
!           write( logdev,* ) '=b=var,spc,isur: ', var, spc, isur
            IF ( ISUR .NE. 0 ) THEN
C is it on the BC file?
               INDX = INDEX1( TR_ICBC( ISUR ), NVARS3D, VNAME3D )
               IF ( INDX .NE. 0 ) THEN
                  BCNAME( SPC ) = VNAME3D( INDX )
               ELSE   ! not on the BC file
                  BCNAME( SPC ) = BLNK
                  WRCNT = WRCNT + 1
                  WRVAR( WRCNT ) = TR_ADV( VAR )
               END IF
               BCFAC( SPC ) = TR_ICBC_FAC( ISUR )
!              write( logdev,* ) '=b=_fac in sur: ', bcfac( spc )
            ELSE
C is the (main) species name on the BC file?
               INDX = INDEX1( TR_ADV( VAR ), NVARS3D, VNAME3D )
               IF ( INDX .NE. 0 ) THEN
                  BCNAME( SPC ) = VNAME3D( INDX )
               ELSE
                  BCNAME( SPC ) = BLNK
                  WRCNT = WRCNT + 1
                  WRVAR( WRCNT ) = TR_ADV( VAR )
               END IF
            END IF

#ifdef Verbose3
            write( logdev,* ) '=b=spc,name: ', spc, trim( bcname( spc ) ),
     &                        '   bcfac:', bcfac( spc )
            write( logdev,* ) ' '
#endif

         END DO
         IF ( WRCNT .GT. 0 ) THEN
            WRITE( LOGDEV,1019 ) TRIM( BNDY_TRAC_1 ), TRIM( CONCMIN )
            DO VAR = 1, WRCNT
               WRITE( LOGDEV,1025 ) TRIM( WRVAR( VAR ) )
            END DO
         END IF 

      END IF

      RETURN

1019  FORMAT( / 5X, 'No BC''s in file ', A, ' for the following adv species:',
     &          1X, 'Set to ', A )
1025  FORMAT( 10X, A )

      END
