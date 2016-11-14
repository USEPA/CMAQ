
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
C $Header: /project/yoj/arc/CCTM/src/gas/smvgear/grbacksub.F,v 1.2 2011/10/21 16:11:13 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)grbacksub.F       1.1 /project/mod3/CMAQ/src/chem/smvgear/SCCS/s.grbacksub .F 07 Jul 1997 12:45:20

       SUBROUTINE BACKSUB

C**********************************************************************
C
C  FUNCTION:  To solve the set of linear simultaneous equations of the
C             form [A]{x}={b} using the decomposed lower and upper
C             triangular matrices [L] and [U]. The subroutine first 
C             solves for {c} in [L]{c}={b}, and then for {x} in
C             [U]{x}={c}.
C
C  PRECONDITIONS: Subroutine DECOMP must have been called
C                                                                     
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, June, 1995.
C                      Based on  the code originally developed by 
C                      M. Jacobson, (Atm. Env., Vol 28, No 2, 1994).
C                    Revised 3/14/96 by Jerry Gipson to conform to
C                      the Models-3 minimum IOV configuration.
C                    Revised December 1996 by Jerry Gipson to conform
C                      to the Models-3 interim CTM that includes emissions
C                      in chemistry.
C                    Modified June, 1997 by Jerry Gipson to be consistent
C                      with beta CTM
C                    Modified September, 1997 by Jerry Gipson to be 
C                      consistent with the targetted CTM
C                    16 Aug 01 J.Young: Use HGRD_DEFN
C                    31 Jan 05 J.Young: get BLKSIZE from dyn alloc horizontal
C                    & vertical domain specifications module (GRID_CONF)
C                    28 Jun 10 J.Young: remove unnecessary modules and includes
C***********************************************************************

      USE GRVARS              ! inherits GRID_CONF

      IMPLICIT NONE
      
C..INCLUDES: None
      
C..ARGUMENTS: None

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      INTEGER I              ! Loop index for number of species
      INTEGER IJ             ! Counter of # of terms summed
      INTEGER IJ0,IJ1,IJ2,   ! Pointers to loaction of ij entries in
     &        IJ3,IJ4        ! decomposed matrix
      INTEGER J,J1,J2,J3,J4  ! Pointers to species # for dc/dt
      INTEGER JZ             ! Loop index inner backsub loops
      INTEGER NCELL          ! Loop index for number of cells

C***********************************************************************

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Back-substition loop 1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IJ = 1 
      DO 80 I = 1, ISCHAN
      
c.....sum 5 terms at a time
         DO JZ = 1, KZHI0( I, NCSP ), 5     
            IJ0 = IJ 
            IJ1 = IJ + 1
            IJ2 = IJ + 2 
            IJ3 = IJ + 3 
            IJ4 = IJ + 4 
            J   = KZERO( IJ0, NCSP )
            J1  = KZERO( IJ1, NCSP )
            J2  = KZERO( IJ2, NCSP )
            J3  = KZERO( IJ3, NCSP )
            J4  = KZERO( IJ4, NCSP )
            IJ  = IJ + 5      
            DO NCELL = 1, NUMCELLS
               GLOSS( NCELL,I ) = GLOSS( NCELL, I )
     &                          - CC2( NCELL, IJ0 ) * GLOSS( NCELL,  J )
     &                          - CC2( NCELL, IJ1 ) * GLOSS( NCELL, J1 )
     &                          - CC2( NCELL, IJ2 ) * GLOSS( NCELL, J2 )
     &                          - CC2( NCELL, IJ3 ) * GLOSS( NCELL, J3 )
     &                          - CC2( NCELL, IJ4 ) * GLOSS( NCELL, J4 )
            END DO
         END DO
   
c.....sum 2 terms at a time
         DO JZ = KZLO1( I, NCSP ), KZHI1( I, NCSP ), 2    
            IJ0 = IJ 
            IJ1 = IJ + 1
            J   = KZERO( IJ0, NCSP )
            J1  = KZERO( IJ1, NCSP )
            IJ  = IJ + 2       
            DO NCELL    = 1, NUMCELLS
               GLOSS( NCELL, I ) = GLOSS( NCELL, I )
     &                           - CC2( NCELL,IJ0 ) * GLOSS( NCELL,  J )
     &                           - CC2( NCELL,IJ1 ) * GLOSS( NCELL, J1 )
            END DO
         END DO
   
c.....sum 1 term at a time
         DO JZ = KZLO2( I, NCSP ), KZILCH( I, NCSP )    
            IJ0 = IJ
            J   = KZERO( IJ0, NCSP )
            IJ  = IJ + 1        
            DO NCELL = 1, NUMCELLS
               GLOSS( NCELL, I ) = GLOSS( NCELL, I )
     &                           - CC2( NCELL, IJ0 ) * GLOSS( NCELL, J )
            END DO
         END DO

80    CONTINUE
  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Back-substitution loop 2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO 180 I = ISCHAN, 1, -1

c...sum 5 terms at a time
         DO JZ = 1, MZHI0( I, NCSP ), 5   
            IJ0 = IJ
            IJ1 = IJ + 1
            IJ2 = IJ + 2 
            IJ3 = IJ + 3 
            IJ4 = IJ + 4 
            J   = KZERO( IJ0, NCSP )
            J1  = KZERO( IJ1, NCSP )
            J2  = KZERO( IJ2, NCSP )
            J3  = KZERO( IJ3, NCSP )
            J4  = KZERO( IJ4, NCSP )
            IJ  = IJ + 5
            DO NCELL = 1, NUMCELLS
               GLOSS( NCELL, I ) = GLOSS( NCELL, I )
     &                          - CC2( NCELL, IJ0 ) * GLOSS( NCELL,  J ) 
     &                          - CC2( NCELL, IJ1 ) * GLOSS( NCELL, J1 )
     &                          - CC2( NCELL, IJ2 ) * GLOSS( NCELL, J2 )
     &                          - CC2( NCELL, IJ3 ) * GLOSS( NCELL, J3 )
     &                          - CC2( NCELL, IJ4 ) * GLOSS( NCELL, J4 )
            END DO
         END DO
  
c...sum 2 terms at a time 
         DO JZ = MZLO1( I, NCSP ), MZHI1( I, NCSP ), 2 
            IJ0 = IJ 
            IJ1 = IJ + 1
            J   = KZERO( IJ0, NCSP )
            J1  = KZERO( IJ1, NCSP )
            IJ  = IJ + 2 
            DO NCELL = 1, NUMCELLS
               GLOSS( NCELL, I ) = GLOSS( NCELL, I )
     &                          - CC2( NCELL, IJ0 ) * GLOSS( NCELL,  J ) 
     &                          - CC2( NCELL, IJ1 ) * GLOSS( NCELL, J1 ) 
            END DO
         END DO
 
c...sum 1 term at a time
         DO JZ = MZLO2( I, NCSP ), MZILCH( I, NCSP ) 
            IJ0 = IJ 
            J   = KZERO( IJ0, NCSP )
            IJ  = IJ + 1  
            DO NCELL    = 1, NUMCELLS
               GLOSS( NCELL, I ) = GLOSS( NCELL, I )
     &                           - CC2( NCELL, IJ0 ) * GLOSS( NCELL, J ) 
            END DO
         END DO

c...adjust diagonal element
         DO NCELL = 1, NUMCELLS
            GLOSS( NCELL, I ) = GLOSS( NCELL, I ) * VDIAG( NCELL, I )
         END DO

180   CONTINUE
 
      RETURN
      END

