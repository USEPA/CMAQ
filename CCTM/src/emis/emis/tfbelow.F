
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
C $Header: /project/work/rep/arc/CCTM/src/emis/emis/tfbelow.F,v 1.3 2011/10/21 16:10:48 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine tfbelow ( jdate, jtime, tfb )

C-----------------------------------------------------------------------
C Description:
C   Extracts selected landuse types from BELD01 and BELD03 and merges
C   the selections into a dust-related landuse array (ULAND).
C   Optionally, reads 3 gridded crop calendar file and calculates an
C   erodible agriculture land fraction.
C   Applies a predetermined removal fraction in and below canopy to
C   ULAND and determines a transport factor (TFB) for this regime.
 
C Subroutines and Functions Called:
C      OPEN3, INTERPX, XTRACT3, M3EXIT

C Revison History:
C  Jun 2009 D. Tong
C  Jan 2011 J. Young: mods for inline wind-blown dust module
C  Apr 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C  Jul 2011 J.Young: pad vnmld for uniform string lengths
C  Oct 2018 D. Wong: With new re-structure of LUS_DEFN, most of the data
C                    declaration has been moved to lus_data_module
C  Feb 2019 David Wong: removed all MY_N clauses
C-----------------------------------------------------------------------

      use hgrd_defn                     ! horizontal domain specifications
      use lus_data_module, only: uland  ! land use/cover schema definitions
                                        ! uland in %
      use utilio_defn

      implicit none

C Includes:

C Arguments:
      integer, intent( in )  :: jdate      ! current model date, coded YYYYDDD
      integer, intent( in )  :: jtime      ! current model time, coded HHMMSS
      real,    intent( out ) :: tfb( :,: ) ! dust transport factor from
                                           ! flow into canopy
 
      character(  16 ) :: pname = 'TFBELOW'
      character( 128 ) :: xmsg = ' '

      real              :: ufc  ! total weighted removal fraction for each cell

      integer i, c, r

      real   :: fr( 4 ) = ! removal fraction in and below canopy
     &         (/ 0.0,    ! water
     &            0.10,   ! grass&crop
     &            0.50,   ! building
     &            0.95 /) ! forest

      do r = 1, nrows
      do c = 1, ncols

         ufc = 0.0
         do i = 1, 4
            ufc = ufc + fr( i ) * uland( c,r,i )
         end do
         tfb( c,r ) = 1.0 - ufc * 0.01

         if ( tfb( c,r ) .gt. 1.0 .or. tfb( c,r ) .lt. 0.0 ) then
            write( xmsg,'( a, e10.3, a, 2i4 )' ) '*** Transport factor error: ',
     &            tfb( c,r ), ' At col, row: ', c, r 
            call m3exit( pname, jdate, jtime, xmsg, xstat1 )
         end if

      end do   ! ncols
      end do   ! nrows

      return
      end

