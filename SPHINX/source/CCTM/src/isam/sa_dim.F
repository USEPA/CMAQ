         
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

        SUBROUTINE SA_DIM 

C KWOK: Determine dimensions of tagging array based on user-supplied sa_io_list
C KWOK: Created Oct 20, 2010
C
C20140428  Called by driver.F
C

        USE UTILIO_DEFN
        USE SA_DEFN
        USE DESID_VARS, ONLY: DESID_N_SRM

        IMPLICIT NONE

        CHARACTER( 16 ) :: PNAME = 'SA_DIM'

        INTEGER IOST

        INTEGER NTAGS

C----------------------------------------------

C...Count the number of tags specified by user
          CALL CNT_SA_IO_LIST( NTAGS )

C...Reading in the tag information
          ALLOCATE ( TAGNAME( NTAGS + 4 ), STAT = IOST )
          CALL CHECKMEM( IOST, 'TAGNAME', PNAME )
          TAGNAME = "NOT"

          ALLOCATE ( TAGSTREAMS_TEMP( NTAGS ), STAT = IOST )
          CALL CHECKMEM( IOST, 'TAGSTREAMS_TEMP', PNAME )
          TAGSTREAMS_TEMP = ''

          ALLOCATE ( TAGSTREAMS_NUM( NTAGS ) )
          TAGSTREAMS_NUM = 0

          ALLOCATE ( ISAMRGN_TEMP( NTAGS ), STAT = IOST )
          CALL CHECKMEM( IOST, 'ISAMRGN_TEMP', PNAME )

          ALLOCATE ( ISAMRGN_NUM( NTAGS ), STAT = IOST )
          CALL CHECKMEM( IOST, 'ISAMRGN_NUM', PNAME )
          ISAMRGN_NUM = 0

C...Process tag infromation from control file
          CALL RD_SA_IO_LIST( NTAGS )

C...Assign Tag Indices To ICON, BCON, OTHER *in that order*
          BCONTAG = NTAGS + 1
          OTHRTAG = BCONTAG + 1
          ICONTAG = OTHRTAG + 1
          NTAG_SA = ICONTAG

          TAGNAME( BCONTAG ) = "BCO"
          TAGNAME( OTHRTAG ) = "OTH"
          TAGNAME( ICONTAG ) = "ICO"

          CALL GET_NSPC_SA 

          N_SPCTAG = NSPC_SA * NTAG_SA

          CALL GET_MAP_SAtoCGR ()

          ! Allocate arrays of species indices, tagging indices, and
          ! variable names for the combined species-tags
          ! i.e. set s_spctag, t_spctag, and vnam_spctag
          ALLOCATE ( S_SPCTAG( N_SPCTAG ), STAT=IOST )
          CALL CHECKMEM( IOST, 'S_SPCTAG', PNAME )
          ALLOCATE ( T_SPCTAG( N_SPCTAG ), STAT=IOST )
          CALL CHECKMEM( IOST, 'T_SPCTAG', PNAME )
          ALLOCATE ( VNAM_SPCTAG( N_SPCTAG ), STAT=IOST )
          CALL CHECKMEM( IOST, 'VNAM_SPCTAG', PNAME )
          ALLOCATE ( BCON_SPC( N_SPCTAG ), STAT=IOST )
          CALL CHECKMEM( IOST, 'BCON_SPC', PNAME )

          ! Assign BCON tag indicies used in advection
          BCON_SPC = .FALSE.
          BCON_SPC( ((BCONTAG - 1)*NSPC_SA+1) : (NSPC_SA*BCONTAG) ) = .TRUE.

           
        END SUBROUTINE SA_DIM

