
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
C $Header: /project/yoj/arc/CCTM/src/couple/gencoor_wrf/couplewrf.F,v 1.2 2011/09/30 14:45:22 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)couple.F	1.1 /project/mod3/CMAQ/src/convert/couple/SCCS/s.couple.F 03 Jun 1997 11:41:33

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE COUPLE_WRF ( CGRID, JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C Function:
C   Convert units and couple concentration values in CGRID for transport
 
C Preconditions:
 
C Subroutines and functions called:
C   INTERPX, M3EXIT
 
C Revision History:
C    Jeff Sep 97 - leave gas chem, non-reactive and tracer species in
C                  standard (ppmV) units for transport
C    2 October, 1998 by Al Bourgeois at LM: parallel implementation
C   Jeff - Dec 00 - move CGRID_MAP into f90 module
C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN; replace INTERP3 with INTERPX
C        - Jun 01 - update units conversion calls and comments
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   21 Jun 10 J.Young: convert for Namelist redesign
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                      removed deprecated TRIMLEN
C   16 Sep 11 S.Roselle: for Pleim`s zadv-wrf blend
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN

      IMPLICIT NONE   

C Include files:

      INCLUDE SUBST_FILES_ID    ! file name parameters

C Arguments:

!     REAL      :: CGRID( :,:,:,: )  ! concentrations
      REAL, POINTER :: CGRID( :,:,:,: )   ! concentrations
      INTEGER, INTENT( IN ) :: JDATE      ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME      ! current model time, coded HHMMSS
      INTEGER, INTENT( IN ) :: TSTEP( 3 ) ! time step vector (HHMMSS)
                                          ! TSTEP(1) = local output step
                                          ! TSTEP(2) = sciproc sync. step (chem)
                                          ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                          !            step and wrf/cmaq call frequency
C Parameters:

C External Functions:

C File Variables:
 
      REAL     :: JACOBM( NCOLS,NROWS,NLAYS )  !"total" Jacobian
      REAL     :: RHOJ  ( NCOLS,NROWS,NLAYS )  !"total" Jacobian * air density

C Local Variables:

      CHARACTER( 16 ) :: PNAME = 'COUPLE_WRF'
      CHARACTER( 16 ) :: VNAME
      CHARACTER( 96 ) :: XMSG = ' '

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER, SAVE :: LOGDEV

      INTEGER, SAVE :: NQAE              ! number of micro-grams/m**3 species
      INTEGER, SAVE :: NNAE              ! number of #/m**3 species
      INTEGER, SAVE :: NSAE              ! number of m**2/m**3 species
      INTEGER, ALLOCATABLE, SAVE :: QAE( : ) ! CGRID pointer to micro-grams/m**3 species
      INTEGER, ALLOCATABLE, SAVE :: NAE( : ) ! CGRID pointer to #/m**3 species
      INTEGER, ALLOCATABLE, SAVE :: SAE( : ) ! CGRID pointer to m**2/m**3 species

!     REAL        SGRID( NCOLS,NROWS,NLAYS,N_AE_SPCD ) ! aerosol subsets
      REAL, ALLOCATABLE :: SGRID( :,:,:,: ) ! aerosol subsets

      INTEGER     ALLOCSTAT
      INTEGER     GXOFF, GYOFF          ! global origin offset from file
C for INTERPX
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

      INTEGER     OFF              ! loop offset to CGRID species
      INTEGER     C, R, L, SPC, V  ! loop counters
      INTEGER     RHOJ_LOC         ! pointer to transported RHOJ (in CGRID)

!     logical, save :: write1

      INTERFACE
         SUBROUTINE MGPM3_KGPM3 ( NSPCS, NLVLS, SGRID )
            USE GRID_CONF
            IMPLICIT NONE
            INTEGER, INTENT( IN ) :: NSPCS, NLVLS
            REAL, INTENT( INOUT ) :: SGRID( :,:,:,: )
         END SUBROUTINE MGPM3_KGPM3
         SUBROUTINE KGPM3_MGPM3 ( NSPCS, NLVLS, SGRID )
            USE GRID_CONF
            IMPLICIT NONE
            INTEGER, INTENT( IN ) :: NSPCS, NLVLS
            REAL, INTENT( INOUT ) :: SGRID( :,:,:,: )
         END SUBROUTINE KGPM3_MGPM3
      END INTERFACE
C-----------------------------------------------------------------------

C If ISPCA .ne. 0, then air is advected and concs. are adjusted

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         LOGDEV = INIT3()

         IF ( N_AE_SPC .GT. 0 ) THEN
C create aerosol species pointers to distinguish micro-grams/m**3,
C #/m**3 (number density), and m**2/m**3 (surface area) species
 
            ALLOCATE ( QAE( N_AE_SPC ),
     &                 NAE( N_AE_SPC ),
     &                 SAE( N_AE_SPC ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
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

!        write( logdev,'(a,i4)' ) ' nqae:', nqae
!        do v = 1, nqae
!           write( logdev,'(a,i4,2x,a)' ) ' v, qae_spc: ', v, ae_spc( qae( v ) )
!        end do
!        write( logdev,'(a,i4)' ) ' nnae:', nnae
!        do v = 1, nnae
!           write( logdev,'(a,i4,2x,a)' ) ' v, nae_spc: ', v, ae_spc( nae( v ) )
!        end do
!        write( logdev,'(a,i4)' ) ' nsae:', nsae
!        do v = 1, nsae
!           write( logdev,'(a,i4,2x,a)' ) ' v, sae_spc: ', v, ae_spc( sae( v ) )
!        end do

         END IF

         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

      END IF       ! if firstime

C Read Jacobian X Air Density (Jacobian =  sq. root det. metric tensor)

      VNAME = 'DENSA_J'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    JDATE, JTIME,
     &                    RHOJ ) ) THEN
         XMSG = 'Could not read ' // VNAME // ' from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
 
      VNAME = 'JACOBM'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    JDATE, JTIME,
     &                    JACOBM ) ) THEN
         XMSG = 'Could not read ' // VNAME // ' from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

C couple for advection - use density times the square root of the determinant
C of the metric tensor (the Jacobian) = RHOJ

!     write1 = .true.
 
C (air density X "total" jacobian) X mixing ratio [ppmV]
      IF ( N_GC_SPC .GT. 0 ) THEN
         OFF = GC_STRT - 1
!        write( logdev,* ) '=== GC'
         DO V = 1, N_GC_TRNS
!           if ( write1 ) then
!           spc = gc_trns_map( v )
!           write( logdev,'(a,2i4,2(2x,a))' ) ' v, gc_trns_map, gc_trns, gc_spc: ',
!    &                            v, spc, gc_trns( v ), gc_spc( spc )
!           end if
            SPC = OFF + GC_TRNS_MAP( V )
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) * RHOJ( C,R,L )
                  END DO
               END DO
            END DO
         END DO
      END IF
 
C initialize RhoJ from MCIP
      RHOJ_LOC = GC_STRT + N_GC_SPC
      DO L = 1, NLAYS
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               CGRID( C,R,L,RHOJ_LOC ) = RHOJ( C,R,L )
            END DO
         END DO
      END DO

      OFF = AE_STRT - 1
C micro-grams/m**3 -> Kg/m**3
      IF ( NQAE .GT. 0 ) THEN
         ALLOCATE ( SGRID( MY_NCOLS,MY_NROWS,NLAYS,NQAE ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating SGRID'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
!        write( logdev,* ) '=== QAE'
         DO V = 1, NQAE
!           if ( write1 ) then
!           spc = ae_trns_map( qae( v ) )
!           write( logdev,'(a,2i4,2(2x,a))' ) ' v, ae_trns_map, ae_trns, ae_spc: ',
!    &                            v, spc, ae_trns( qae( v ) ), ae_spc( spc )
!           end if
            SPC = OFF + AE_TRNS_MAP( QAE( V ) )
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     SGRID( C,R,L,V ) = CGRID( C,R,L,SPC )
                  END DO
               END DO
            END DO
         END DO
         CALL MGPM3_KGPM3 ( NQAE, NLAYS, SGRID )
C ("total" jacobian) X [Kg/m**3]
         DO V = 1, NQAE
            SPC = OFF + AE_TRNS_MAP( QAE( V ) )
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     CGRID( C,R,L,SPC ) = SGRID( C,R,L,V ) * JACOBM( C,R,L )
                  END DO
               END DO
            END DO
         END DO
         DEALLOCATE ( SGRID )
      END IF

C CGRID in #/m**3 -> ("total" jacobian) X [#/m**3]
      IF ( NNAE .GT. 0 ) THEN
!        write( logdev,* ) '=== NAE'
         DO V = 1, NNAE
!           if ( write1 ) then
!           spc = ae_trns_map( nae( v ) )
!           write( logdev,'(a,2i4,2(2x,a))' ) ' v, ae_trns_map, ae_trns, ae_spc: ',
!    &                            v, spc, ae_trns( nae( v ) ), ae_spc( spc )
!           end if
            SPC = OFF + AE_TRNS_MAP( NAE( V ) )
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) * JACOBM( C,R,L )
                  END DO
               END DO
            END DO
         END DO
      END IF

C CGRID in m**2/m**3 -> ("total" jacobian) X [m**2/m**3]
      IF ( NSAE .GT. 0 ) THEN
!        write( logdev,* ) '=== SAE'
         DO V = 1, NSAE
!           if ( write1 ) then
!           spc = ae_trns_map( sae( v ) )
!           write( logdev,'(a,2i4,2(2x,a))' ) ' v, ae_trns_map, ae_trns, ae_spc: ',
!    &                            v, spc, ae_trns( sae( v ) ), ae_spc( spc )
!           end if
            SPC = OFF + AE_TRNS_MAP( SAE( V ) )
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) * JACOBM( C,R,L )
                  END DO
               END DO
            END DO
         END DO
      END IF

C (air density X "total" jacobian) X mixing ratio [ppmV]
      IF ( N_NR_SPC .GT. 0 ) THEN
         OFF = NR_STRT - 1
!        write( logdev,* ) '=== NR'
         DO V = 1, N_NR_TRNS
!           if ( write1 ) then
!           spc = nr_trns_map( v )
!           write( logdev,'(a,2i4,2(2x,a))' ) ' v, nr_trns_map, nr_trns, nr_spc: ',
!    &                            v, spc, nr_trns( v ), nr_spc( spc )
!           end if
            SPC = OFF + NR_TRNS_MAP( V )
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) * RHOJ( C,R,L )
                  END DO
               END DO
            END DO
         END DO
      END IF
 
C (air density X "total" jacobian) X mixing ratio [ppmV]
      IF ( N_TR_SPC .GT. 0 ) THEN
         DO SPC = TR_STRT, TR_STRT + N_TR_SPC - 1
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) * RHOJ( C,R,L )
                  END DO
               END DO
            END DO
         END DO
      END IF

!     write1 = .false.

      RETURN
 
C............................................................................
C entry DECOUPLE
 
      ENTRY DECOUPLE_WRF ( CGRID, JDATE, JTIME, TSTEP )

      VNAME = 'JACOBM'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    JDATE, JTIME,
     &                    JACOBM ) ) THEN
         XMSG = 'Could not read ' // VNAME // ' from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF
 
C Load RhoJ from transported RhoJ
      RHOJ_LOC = GC_STRT + N_GC_SPC
      DO L = 1, NLAYS
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               RHOJ( C,R,L ) = CGRID( C,R,L,RHOJ_LOC )
            END DO
         END DO
      END DO

C decouple for chemistry and diffusion
 
C mixing ratio [ppmV] / (air density X "total" jacobian)
      IF ( N_GC_SPC .GT. 0 ) THEN
         OFF = GC_STRT - 1
         DO V = 1, N_GC_TRNS
            SPC = OFF + GC_TRNS_MAP( V )
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) / RHOJ( C,R,L )
                  END DO
               END DO
            END DO
         END DO
      END IF
 
      OFF = AE_STRT - 1
C {J}Kg/m**3 -> {J}micro-grams/m**3
      IF ( NQAE .GT. 0 ) THEN
         ALLOCATE ( SGRID( MY_NCOLS,MY_NROWS,NLAYS,NQAE ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating SGRID'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         DO V = 1, NQAE
            SPC = OFF + AE_TRNS_MAP( QAE( V ) )
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     SGRID( C,R,L,V ) = CGRID( C,R,L,SPC )
                  END DO
               END DO
            END DO
         END DO
         CALL KGPM3_MGPM3 ( NQAE, NLAYS, SGRID )
C [micro-grams/m**3] / ("total" jacobian)
         DO V = 1, NQAE
            SPC = OFF + AE_TRNS_MAP( QAE( V ) )
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     CGRID( C,R,L,SPC ) = SGRID( C,R,L,V ) / JACOBM( C,R,L )
                  END DO
               END DO
            END DO
         END DO
         DEALLOCATE ( SGRID )
      END IF

C CGRID in {J}[#/m**3] -> #/m**3
      IF ( NNAE .GT. 0 ) THEN
         DO V = 1, NNAE
            SPC = OFF + AE_TRNS_MAP( NAE( V ) )
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) / JACOBM( C,R,L )
                  END DO
               END DO
            END DO
         END DO
      END IF

C CGRID in {J}[m**2/m**3] -> m**2/m**3
      IF ( NSAE .GT. 0 ) THEN
         DO V = 1, NSAE
            SPC = OFF + AE_TRNS_MAP( SAE( V ) )
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) / JACOBM( C,R,L )
                  END DO
               END DO
            END DO
         END DO
      END IF

C mixing ratio [ppmV] / (air density X "total" jacobian)
      IF ( N_NR_SPC .GT. 0 ) THEN
         OFF = NR_STRT - 1
         DO V = 1, N_NR_TRNS
            SPC = OFF + NR_TRNS_MAP( V )
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) / RHOJ( C,R,L )
                  END DO
               END DO
            END DO
         END DO
      END IF
 
C mixing ratio [ppmV] / (air density X "total" jacobian)
      IF ( N_TR_SPC .GT. 0 ) THEN
         DO SPC = TR_STRT, TR_STRT + N_TR_SPC - 1
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) / RHOJ( C,R,L )
                  END DO
               END DO
            END DO
         END DO
      END IF

      RETURN
      END
