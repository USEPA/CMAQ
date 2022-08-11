
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
      SUBROUTINE COUPLE ( CGRID, JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C Function:
C   Convert units and couple concentration values in CGRID for transport
 
C Preconditions:
 
C Subroutines and functions called:
C    M3EXIT
 
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
C   11 May 11 D.Wong: incorporated twoway model implementation
C   11 Oct 11 J.Young: eliminate ppmv_msmr
C   08 July 19 F. Sidi: Updated subroutine with CIO changes
C   09 Dec 19 S.L.Napelenok: ddm-3d implementation for cmaq v5.3.1
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
      USE CENTRALIZED_IO_MODULE, only : interpolate_var
#ifdef isam
      USE SA_DEFN, ONLY: ISAM, NSPC_SA, NTAG_SA, MAP_SAtoCGR,
     &    TRANSPORT_SPC, ITAG
#endif 

#ifdef sens
      USE DDM3D_DEFN, ONLY:SENGRID, NPMAX, NP
#endif 

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
      REAL, PARAMETER :: GPKG = 1.0E+03   ! g/kg
      REAL, PARAMETER :: MGPG = 1.0E+06   ! micro-g/g
      REAL, PARAMETER :: CONV = GPKG * MGPG

C External Functions:

C File Variables:
 
      REAL        JACOBM( NCOLS,NROWS,NLAYS )  !"total" Jacobian
      REAL        RHOJ  ( NCOLS,NROWS,NLAYS )  !"total" Jacobian * air density

C Local Variables:

      CHARACTER( 16 ) :: PNAME = 'COUPLE'
      CHARACTER( 16 ) :: VNAME
      CHARACTER( 96 ) :: XMSG = ' '

      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      INTEGER, SAVE :: NQAE              ! number of micro-grams/m**3 species
      INTEGER, SAVE :: NNAE              ! number of #/m**3 species
      INTEGER, SAVE :: NSAE              ! number of m**2/m**3 species
      INTEGER, ALLOCATABLE, SAVE :: QAE( : ) ! CGRID pointer to micro-grams/m**3 species
      INTEGER, ALLOCATABLE, SAVE :: NAE( : ) ! CGRID pointer to #/m**3 species
      INTEGER, ALLOCATABLE, SAVE :: SAE( : ) ! CGRID pointer to m**2/m**3 species

      INTEGER     ALLOCSTAT

      INTEGER     OFF              ! loop offset to CGRID species
      INTEGER     C, R, L, SPC, V  ! loop counters

#ifdef isam
      INTEGER  :: SPC_CGRID
#endif

C-----------------------------------------------------------------------

C If ISPCA .ne. 0, then air is advected and concs. are adjusted

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

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

2009        format( 5x, 2i4, 2x, a )

         END IF

      END IF       ! if firstime

C Read Jacobian X Air Density (Jacobian =  sq. root det. metric tensor)

      call interpolate_var ('DENSA_J', JDATE, JTIME, RHOJ)
 
      call interpolate_var ('JACOBM', JDATE, JTIME, JACOBM)

C couple for advection - use density times the square root of the determinant
C of the metric tensor (the Jacobian) = RHOJ
 
C CGRID in mixing ratio [ppmV] -> (air density X "total" Jacobian) X mixing ratio [ppmV]
      IF ( N_GC_SPC .GT. 0 ) THEN
         OFF = GC_STRT - 1
         DO V = 1, N_GC_SPC
            SPC = OFF + V
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) * RHOJ( C,R,L )
#ifdef sens
                     DO NP = 1, NPMAX
                        SENGRID( C,R,L,NP,SPC ) = SENGRID(C,R,L,NP,SPC) * RHOJ( C,R,L )
                     END DO
#endif
                  END DO
               END DO
            END DO
         END DO
      END IF
 
      OFF = AE_STRT - 1
C CGRID in ug/m**3 -> ("total" Jacobian [m]) X [Kg/m**3]
      IF ( NQAE .GT. 0 ) THEN
         DO V = 1, NQAE
            SPC = OFF + QAE( V )
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) * JACOBM( C,R,L ) / CONV
#ifdef sens       
                     DO NP = 1, NPMAX
                        SENGRID( C,R,L,NP,SPC ) = SENGRID(C,R,L,NP,SPC) * JACOBM( C,R,L ) / CONV
                     END DO
#endif
                  END DO
               END DO
            END DO
         END DO
      END IF

C CGRID in #/m**3 -> ("total" Jacobian [m]) X [#/m**3]
      IF ( NNAE .GT. 0 ) THEN
         DO V = 1, NNAE
            SPC = OFF + NAE( V )
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) * JACOBM( C,R,L )
#ifdef sens       
                     DO NP = 1, NPMAX
                        SENGRID( C,R,L,NP,SPC ) = SENGRID(C,R,L,NP,SPC) * JACOBM( C,R,L )
                     END DO
#endif               
                  END DO
               END DO
            END DO
         END DO
      END IF
 
C CGRID in m**2/m**3 -> ("total" Jacobian [m]) X [m**2/m**3]
      IF ( NSAE .GT. 0 ) THEN
         DO V = 1, NSAE
            SPC = OFF + SAE( V )
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) * JACOBM( C,R,L )
#ifdef sens       
                     DO NP = 1, NPMAX
                        SENGRID( C,R,L,NP,SPC ) = SENGRID(C,R,L,NP,SPC) * JACOBM( C,R,L )
                     END DO
#endif               
                  END DO
               END DO
            END DO
         END DO
      END IF

C CGRID in mixing ratio [ppmV] -> (air density X "total" Jacobian) X mixing ratio [ppmV]
      IF ( N_NR_SPC .GT. 0 ) THEN
         OFF = NR_STRT - 1
         DO V = 1, N_NR_SPC
            SPC = OFF + V
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) * RHOJ( C,R,L )
#ifdef sens       
                     DO NP = 1, NPMAX
                        SENGRID( C,R,L,NP,SPC ) = SENGRID(C,R,L,NP,SPC) * RHOJ( C,R,L )
                     END DO
#endif               
                  END DO
               END DO
            END DO
         END DO
      END IF
 
C CGRID in mixing ratio [ppmV] -> (air density X "total" Jacobian) X mixing ratio [ppmV]
      IF ( N_TR_SPC .GT. 0 ) THEN
         OFF = TR_STRT - 1
         DO V = 1, N_TR_SPC 
            SPC = OFF + V
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) * RHOJ( C,R,L )
#ifdef sens       
                     DO NP = 1, NPMAX
                        SENGRID( C,R,L,NP,SPC ) = SENGRID(C,R,L,NP,SPC) * RHOJ( C,R,L )
                     END DO
#endif               
                  END DO
               END DO
            END DO
         END DO
      END IF


#ifdef isam
      DO SPC = 1, NSPC_SA
        IF ( TRANSPORT_SPC( SPC ) ) THEN
          SPC_CGRID = MAP_SAtoCGR(SPC)
          IF ( SPC_CGRID .LE. N_GC_SPC .OR. SPC_CGRID .GE. NR_STRT ) THEN ! this is not an aerosol
            DO ITAG = 1, NTAG_SA
              DO L = 1, NLAYS
                 DO R = 1, NROWS
                    DO C = 1, NCOLS
                      ISAM( C,R,L,SPC,ITAG ) = ISAM( C,R,L,SPC,ITAG ) * RHOJ( C,R,L )
                    END DO
                 END DO
              END DO
            END DO
          ELSE ! this is an aerosol (mass only)
            DO ITAG = 1, NTAG_SA
              DO L = 1, NLAYS
                 DO R = 1, NROWS
                    DO C = 1, NCOLS
                      ISAM( C,R,L,SPC,ITAG ) = JACOBM( C,R,L ) * ISAM( C,R,L,SPC,ITAG ) / CONV
                    END DO
                 END DO
              END DO
            END DO
          END IF
        ENDIF
      END DO
#endif


      RETURN
 
C............................................................................
C entry DECOUPLE
 
      ENTRY DECOUPLE ( CGRID, JDATE, JTIME, TSTEP )

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

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

         END IF

         END IF       ! if firstime

      call interpolate_var ('DENSA_J', JDATE, JTIME, RHOJ)
 
      call interpolate_var ('JACOBM', JDATE, JTIME, JACOBM)
 
C decouple for chemistry and diffusion
 
C CGRID in mixing ratio [ppmV] X (air density X "total" Jacobian) -> mixing ratio [ppmV]
      IF ( N_GC_SPC .GT. 0 ) THEN
         OFF = GC_STRT - 1
         DO V = 1, N_GC_SPC
            SPC = OFF + V
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) / RHOJ( C,R,L )
#ifdef sens       
                     DO NP = 1, NPMAX
                        SENGRID( C,R,L,NP,SPC ) = SENGRID(C,R,L,NP,SPC) / RHOJ( C,R,L )
                     END DO
#endif               
                  END DO
               END DO
            END DO
         END DO
      END IF
 
      OFF = AE_STRT - 1
C CGRID in Jacobian [m] X [Kg/m**3] -> [ug/m**3]
      IF ( NQAE .GT. 0 ) THEN
         DO V = 1, NQAE
            SPC = OFF + QAE( V )
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CGRID( C,R,L,SPC ) = CONV * CGRID( C,R,L,SPC ) / JACOBM( C,R,L )
#ifdef sens       
                     DO NP = 1, NPMAX
                        SENGRID( C,R,L,NP,SPC ) = CONV * SENGRID(C,R,L,NP,SPC) / JACOBM( C,R,L )
                     END DO
#endif               
                  END DO
               END DO
            END DO
         END DO
      END IF

C CGRID in Jacobian [m] X [#/m**3] -> #/m**3
      IF ( NNAE .GT. 0 ) THEN
         DO V = 1, NNAE
            SPC = OFF + NAE( V )
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) / JACOBM( C,R,L )
#ifdef sens       
                     DO NP = 1, NPMAX
                        SENGRID( C,R,L,NP,SPC ) = SENGRID(C,R,L,NP,SPC) / JACOBM( C,R,L )
                     END DO
#endif               
                  END DO
               END DO
            END DO
         END DO
      END IF
 
C CGRID in Jacobian [m] X [m**2/m**3] -> m**2/m**3
      IF ( NSAE .GT. 0 ) THEN
         DO V = 1, NSAE
            SPC = OFF + SAE( V )
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) / JACOBM( C,R,L )
#ifdef sens       
                     DO NP = 1, NPMAX
                        SENGRID( C,R,L,NP,SPC ) = SENGRID(C,R,L,NP,SPC) / JACOBM( C,R,L )
                     END DO
#endif               
                  END DO
               END DO
            END DO
         END DO
      END IF

C CGRID in mixing ratio [ppmV] X (air density X "total" jacobian) -> mixing ratio [ppmV]
      IF ( N_NR_SPC .GT. 0 ) THEN
         OFF = NR_STRT - 1
         DO V = 1, N_NR_SPC
            SPC = OFF + V
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) / RHOJ( C,R,L )
#ifdef sens       
                     DO NP = 1, NPMAX
                        SENGRID( C,R,L,NP,SPC ) = SENGRID(C,R,L,NP,SPC) / RHOJ( C,R,L )
                     END DO
#endif               
                  END DO
               END DO
            END DO
         END DO
      END IF
 
C CGRID in mixing ratio [ppmV] X (air density X "total" jacobian) -> mixing ratio [ppmV]
      IF ( N_TR_SPC .GT. 0 ) THEN
         OFF = TR_STRT - 1
         DO V = 1, N_TR_SPC
            SPC = OFF + V
            DO L = 1, NLAYS
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CGRID( C,R,L,SPC ) = CGRID( C,R,L,SPC ) / RHOJ( C,R,L )
#ifdef sens       
                     DO NP = 1, NPMAX
                        SENGRID( C,R,L,NP,SPC ) = SENGRID(C,R,L,NP,SPC) / RHOJ( C,R,L )
                     END DO
#endif               
                  END DO
               END DO
            END DO
         END DO
      END IF

      
#ifdef isam
      DO SPC = 1, NSPC_SA
        IF ( TRANSPORT_SPC( SPC ) ) THEN
          SPC_CGRID = MAP_SAtoCGR(SPC)
          IF ( SPC_CGRID .LE. N_GC_SPC .OR. SPC_CGRID .GE. NR_STRT ) THEN ! this is not an aerosol
            DO ITAG = 1, NTAG_SA
              DO L = 1, NLAYS
                 DO R = 1, NROWS
                    DO C = 1, NCOLS
                      ISAM( C,R,L,SPC,ITAG ) = ISAM( C,R,L,SPC,ITAG ) / RHOJ( C,R,L )
                    END DO
                 END DO
              END DO
            END DO
          ELSE ! this is an aerosol (mass only)
            DO ITAG = 1, NTAG_SA
              DO L = 1, NLAYS
                 DO R = 1, NROWS
                    DO C = 1, NCOLS
                      ISAM( C,R,L,SPC,ITAG ) = CONV * ISAM( C,R,L,SPC,ITAG ) / JACOBM( C,R,L )
                    END DO
                 END DO
              END DO
            END DO
          END IF
        END IF
      END DO
#endif    
      
      
      
      
      RETURN
      END
