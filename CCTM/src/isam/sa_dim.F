         
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

C PCGRID_DEFN inherits CGRID_SPC and GRID_CONF( inherits HGRD_DEFN, VGRD_DEFN )
        USE PCGRID_DEFN
        USE UTILIO_DEFN       ! 20120615
        USE SA_DEFN

        USE EMIS_VARS, ONLY: N_EM_SRM

        IMPLICIT NONE

        CHARACTER( 16 ) :: PNAME = 'SA_DIM'

        INTEGER IOST
        CHARACTER( 80 ) :: XMSG

        INTEGER NTAGS

C...External functions
!0615   CHARACTER( 16 ) :: PROMPTMFILE
        INTEGER, EXTERNAL :: TRIM_LEN
c...externals above


C----------------------------------------------

CCC     IF ( MYPE .EQ. 0 ) THEN
C
C...Count the number of tags specified by user
          CALL CNT_SA_IO_LIST( NTAGS )
C
C...Reading in the tag information
          ALLOCATE ( TAGNAME( NTAGS ), STAT = IOST )
          CALL CHECKMEM( IOST, 'TAGNAME', PNAME )

c         ALLOCATE ( TAGCLASSES( NTAGS ), STAT = IOST )
c         CALL CHECKMEM( IOST, 'TAGCLASSES', PNAME )

          ALLOCATE ( TAGSTREAMS_TEMP( NTAGS ), STAT = IOST )
          CALL CHECKMEM( IOST, 'TAGSTREAMS', PNAME )
          TAGSTREAMS_TEMP = ''

c...Only for the calling of RD_SA_IO_LIST
          ALLOCATE ( TAGRGN( NTAGS ), STAT = IOST )
          CALL CHECKMEM( IOST, 'TAGRGN', PNAME )


!20130702          ALLOCATE ( SGFILE( NTAGS ), STAT = IOST )
!20130702          CALL CHECKMEM( IOST, 'SGFILE', PNAME )
!20130702          ALLOCATE ( STKFILE( NTAGS ), STAT = IOST )
!20130702          CALL CHECKMEM( IOST, 'STKFILE', PNAME )

c         ALLOCATE( YESSTK( NTAGS, N_SGSTACKS ), STAT = IOST )
c         CALL CHECKMEM( IOST, 'YESSTK', PNAME )
c         YESSTK = .FALSE.

          CALL RD_SA_IO_LIST( NTAGS )

C..Assign Tag Indices To ICON, BCON, OTHER *in that order*
          BCONTAG = NTAGS + 1
          OTHRTAG = BCONTAG + 1
          ICONTAG = OTHRTAG + 1
          NTAG_SA = ICONTAG
           
c...Only for the calling of GET_NSPC_SA
c         ALLOCATE ( L_EC( NTAGS+3 ), STAT = IOST ) 
c         CALL CHECKMEM( IOST, 'L_EC', PNAME )
c         ALLOCATE ( L_OC( NTAGS+3 ), STAT = IOST )
c         CALL CHECKMEM( IOST, 'L_OC', PNAME )
c         ALLOCATE ( L_SFATE( NTAGS+3 ), STAT = IOST )
c         CALL CHECKMEM( IOST, 'L_SFATE', PNAME )
c         ALLOCATE ( L_NTRATE( NTAGS+3 ), STAT = IOST )
c         CALL CHECKMEM( IOST, 'L_NTRATE', PNAME )
c         ALLOCATE ( L_NH4( NTAGS+3 ), STAT = IOST )
c         CALL CHECKMEM( IOST, 'L_NH4', PNAME )
c         ALLOCATE ( L_PM25( NTAGS+3 ), STAT = IOST )
c         CALL CHECKMEM( IOST, 'L_PM25', PNAME )
c         ALLOCATE ( L_CO( NTAGS+3 ), STAT = IOST )
c         CALL CHECKMEM( IOST, 'L_CO', PNAME )
c         ALLOCATE ( L_OZONE( NTAGS+3 ), STAT = IOST )
c         CALL CHECKMEM( IOST, 'L_OZONE', PNAME )
c         ALLOCATE ( L_VOC( NTAGS+3 ), STAT = IOST )
c         CALL CHECKMEM( IOST, 'L_VOC', PNAME )

          CALL GET_NSPC_SA 

CCC     ENDIF ! MYPE eq 0

        END SUBROUTINE SA_DIM

