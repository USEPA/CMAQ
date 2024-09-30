
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
C $Header: 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE UNLOAD_CGRID ( CGRID )
!     SUBROUTINE UNLOAD_CGRID ( CGRID, scalars_in)
!     SUBROUTINE UNLOAD_CGRID ( CGRID, gc_species_in,
!    &                          ae_species_in, nr_species_in)

C-----------------------------------------------------------------------
C Function:

C Revision history:
C   David Wong - Mar 17 - initial implmentation
C-----------------------------------------------------------------------

      USE CGRID_SPCS            ! CGRID mechanism species
      USE HGRD_DEFN, ONLY : NCOLS, NROWS
      USE VGRD_DEFN, ONLY : NLAYS
      USE coupler_module, ONLY : cmaq_species, num_cmaq_species

      IMPLICIT NONE

C Arguments:

      REAL, INTENT(IN)  :: CGRID( :,:,:,: )  ! for initial CONC
!     real, intent(out) :: scalars_in(:,:,:)
!     real, intent(out) :: gc_species_in(:,:,:),
!    &                     ae_species_in(:,:,:),
!    &                     nr_species_in(:,:,:)

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER :: C, R, L, V, SPC

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
      END IF

!     print '(a14, 10i5)', ' ==d== unload ', GC_STRT, N_GC_SPC, AE_STRT, N_AE_SPC, NR_STRT, N_NR_SPC, 
!    &           size(cmaq_species, 4), size(cgrid,4)

! for GC species
      DO L = 1, NLAYS
         DO C = 1, NCOLS
            DO v = 1, N_GC_SPC
              cmaq_species( c,1,l,v ) = CGRID( C,1,L,V )
            END DO
         END DO
      END DO

! for other species
      DO L = 1, NLAYS
         DO C = 1, NCOLS
            DO v = N_GC_SPC+1, num_cmaq_species
              cmaq_species( c,1,l,v ) = CGRID( C,1,L,V+1 )
            END DO
         END DO
      END DO

      END SUBROUTINE UNLOAD_CGRID
