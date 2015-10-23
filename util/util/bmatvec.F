
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

        MODULE BMATVEC_MODULE

!=======================================================================
! Version "@(#)$Header$"
! EDSS/Models-3 I/O API.
! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
! (C) 2003-2010 Baron Advanced Meteorological Systems
! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
! See file "LGPL.txt" for conditions of use.
!.......................................................................
!  subroutine body starts at line  76
!
!  FUNCTION:  apply a 4-band sparse matrix to an array ("layered vector")
!	      as used for bilinear interpolation of meteorology in LAYPOINT
!
!  NOTE:  Transposes from I/O subscript order V( M,P )
!         to computational subscript order    C( P,N )
!
!       For bilinear interpolation of gridded data having dimension NC,NR
!       to a list of locations having grid-normal coordinates <X(S),Y(S)>:
!       let C(S) and R(S) be INT( X(S) ) and INT( Y(S) ), P(S) and Q(S)
!       be AMOD( X(S), 1.0 ) and AMOD( Y(S), 1.0 ).  Then IX has the 
!	single-indexing subscripts into the grid for the cell=corners
!       surrounding the <X(S),Y(S)>
!           IX(1,S) = C + NC * ( R - 1 ) + 1
!           IX(2,S) = C + NC * ( R - 1 ) 
!           IX(3,S) = C + NC *   R
!           IX(4,S) = C + NC *   R       + 1
!       and AX has the bilinear-interpolation coefficients:
!           AX(1,S) = ( 1.0 - P( S ) )*( 1.0 - Q( S ) )
!           AX(2,S) =         P( S )  *( 1.0 - Q( S ) )
!           AX(3,S) = ( 1.0 - P( S ) )*        Q( S )       
!           AX(4,S) =         P( S )  *        Q( S )
!
!  SEE ALSO:
!       UNGRIDB() which produces such matrices
!       BILIN()   which performs combined interpolate-only,
!                 preserving the subscript-order.
!
!  PRECONDITIONS REQUIRED:
!       Number of layers same for input and output.
!       Index and coefficients set up so that the equation for
!       the multiplication (at each row R and layer L) is
!
!       C(L,R) = SUM_{ J=1...4 }  A( J,R ) * V( I( J,R ),L )
!
!  SUBROUTINES AND FUNCTIONS CALLED:  none
!
!  REVISION  HISTORY:
!       prototype 12 Aug 15 by David Wong: 
!          -- Based on bmatvec.f in ioapi library to eliminate run time 
!             dynamical array and to handle 1d or 2d array C by F90
!             poly-morphism feature
!***********************************************************************

        INTERFACE BMATVECN
          MODULE PROCEDURE BMATVEC1, BMATVEC2
        END INTERFACE

        CONTAINS
      
        SUBROUTINE  BMATVEC1( M, N, P, IX, AX, V, C )

        IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: M               ! length of input  vector
        INTEGER, INTENT(IN   ) :: N               ! length of output vector
        INTEGER, INTENT(IN   ) :: P               ! number of layers
        INTEGER, INTENT(IN   ) :: IX( :,: )       ! index array
        REAL   , INTENT(IN   ) :: AX( :,: )       ! 4-band coeff matrix
        REAL   , INTENT(IN   ) :: V( :,: )        ! P-layered input  vector
        REAL   , INTENT(  OUT) :: C( : )        ! P-layered output vector


C...........   LOCAL VARIABLES and their descriptions:
        
        INTEGER         S, L, J1, J2, J3, J4

!***********************************************************************
!   begin body of subroutine  BMATVEC1

        DO  S = 1, N
            
            J1 = IX( 1,S )
            J2 = IX( 2,S )
            J3 = IX( 3,S )
            J4 = IX( 4,S )

            DO  L = 1, P
                C( S ) = AX( 1,S ) * V( J1,L )  +
     &                   AX( 2,S ) * V( J2,L )  +
     &                   AX( 3,S ) * V( J3,L )  +
     &                   AX( 4,S ) * V( J4,L )
            END DO
        
        END DO

        END SUBROUTINE BMATVEC1

! --------------------------------------------------------------------
        SUBROUTINE  BMATVEC2( M, N, P, IX, AX, V, C )

        IMPLICIT NONE

!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: M               ! length of input  vector
        INTEGER, INTENT(IN   ) :: N               ! length of output vector
        INTEGER, INTENT(IN   ) :: P               ! number of layers
        INTEGER, INTENT(IN   ) :: IX( :,: )       ! index array
        REAL   , INTENT(IN   ) :: AX( :,: )       ! 4-band coeff matrix
        REAL   , INTENT(IN   ) :: V( :,: )        ! P-layered input  vector
        REAL   , INTENT(  OUT) :: C( :,: )        ! P-layered output vector


!...........   LOCAL VARIABLES and their descriptions:
        
        INTEGER         S, L, J1, J2, J3, J4

!***********************************************************************
!   begin body of subroutine  BMATVEC

        DO  S = 1, N
            
            J1 = IX( 1,S )
            J2 = IX( 2,S )
            J3 = IX( 3,S )
            J4 = IX( 4,S )

            DO  L = 1, P
                C( L,S ) = AX( 1,S ) * V( J1,L )  +
     &                     AX( 2,S ) * V( J2,L )  +
     &                     AX( 3,S ) * V( J3,L )  +
     &                     AX( 4,S ) * V( J4,L )
            END DO
        
        END DO

        END SUBROUTINE BMATVEC2

        END MODULE BMATVEC_MODULE
