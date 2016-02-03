
      SUBROUTINE WR_SRCRGN( MFRC_P, NTAGS )

C20140428  Writes source regions to a netcdf for diagnostic purpose.
C          However, this subroutine is no longer in use.
C
C          Would be called by driver.F

      USE GRID_CONF
      USE UTILIO_DEFN            ! 20120615

      IMPLICIT NONE

!0615 INCLUDE SUBST_IOPARMS
!0615#include      SUBST_IODECL  # I/O definitions and declarations
!0615      INCLUDE SUBST_IOFDESC ! FSREAD3 needs it
      INCLUDE SUBST_FILES_ID

      CHARACTER( 16 ) :: MAPNAME
      INTEGER, INTENT(IN) :: NTAGS
      REAL, POINTER      :: MFRC_P( :,:,: )

      CHARACTER( 16 ) :: PNAME = 'WR_SRCRGN'
      INTEGER LOGDEV

c Output file name
      CHARACTER( 16 ) :: TAGGED_MAPNAME = 'TAGGED_MAPNAME'
c output name above

      INTEGER ITAG
      INTEGER IOST
      INTEGER C, R, L, S
      CHARACTER( 80 ) :: XMSG
c External functions
!0615 CHARACTER( 16 ) :: PROMPTMFILE
      INTEGER LEN_TRIM
c external above

      INTEGER  GXOFF, GYOFF
      INTEGER, SAVE :: STRTCOL, ENDCOL, STRTROW, ENDROW

c Ioapi attributes
      INTEGER      FTYPE      ! file type
      INTEGER      SDATE      ! file start date YYYYDDD
      INTEGER      STIME      ! file start time  HHMMSS
      INTEGER      TSTEP      ! file time step   HHMMSS
      INTEGER      NCOLALL      ! number of grid columns
      INTEGER      NROWALL      ! number of grid rows
      INTEGER      NLAYALL      ! number of layers
      INTEGER      NTHKALL      ! BOUNDARY:  perim thickness (cells)
                                  ! SPARSE MATRIX:  number of matrix-cols
      INTEGER      GDTYP        ! grid type:  1=LAT-LON, 2=Lambert, ...

      REAL*8       ALFA      ! first, second, third map
      REAL*8       BETA      ! projection descriptive
      REAL*8       GAMA      ! parameters.

      REAL*8       XCEN      ! lon for coord-system X=0
      REAL*8       YCEN      ! lat for coord-system Y=0
      REAL*8       XORG      ! X-coordinate origin of grid (map units)
      REAL*8       YORG      ! Y-coordinate origin of grid
      REAL*8       XCEL      ! X-coordinate cell dimension
      REAL*8       YCEL      ! Y-coordinate cell dimension

      INTEGER      VGTYP      !  vertical coordinate type (VGSIGP3, ...)
      REAL         VGTOP      !  model-top, for sigma coord types.
      REAL, ALLOCATABLE :: VGLV( : )  !  vertical coord values.

      CHARACTER*16   GDNAM      ! grid name             (length NAMLEN3=16)
c ioapi attr above

      REAL, ALLOCATABLE :: BUFF2( :,: )
      CHARACTER*2  CDIGITS, CHR

C------------------------------------------------------------

C...Extract attributes of original source map file
!0710   MAPNAME = PROMPTMFILE( 'Enter name for source region ncf file',
!0710&       FSREAD3, 'SA_APPMAP', PNAME )
!0710
!0710   IF ( .NOT. DESC3( MAPNAME ) ) THEN
!0710     XMSG = 'Could not get file description from ' // MAPNAME
!0710     CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
!0710   ENDIF ! original map file description obtained ?
!0710
!0710   GL_NCOLS = NCOLS3D
!0710   GL_NROWS = NROWS3D
!0710   GDTYP_GD = GDTYP3D
!0710   P_ALP_GD = P_ALP3D
!0710   P_BET_GD = P_BET3D
!0710   P_GAM_GD = P_GAM3D
!0710   XORIG_GD = XORIG3D
!0710   YORIG_GD = YORIG3D
!0710   XCENT_GD = XCENT3D
!0710   YCENT_GD = YCENT3D
!0710   XCELL_GD = XCELL3D
!0710   YCELL_GD = YCELL3D
!0710   VGTYP_GD = VGTYP3D
!0710   VGTOP_GD = VGTOP3D
        if ( MYPE .EQ. 0 ) then
          print*,'GL_NCOLS, GL_NROWS, GDTYP_GD:',GL_NCOLS, GL_NROWS, GDTYP_GD
          print*,'P_ALP_GD, P_BET_GD, P_GAM_GD:',P_ALP_GD, P_BET_GD, P_GAM_GD
          print*,'XORIG_GD, YORIG_GD, XCENT_GD, YCENT_GD:',XORIG_GD, YORIG_GD, XCENT_GD, YCENT_GD
          print*,'XCELL_GD, YCELL_GD, VGTYP_GD, VGTOP_GD:',XCELL_GD, YCELL_GD, VGTYP_GD, VGTOP_GD
        endif

        DO L = 1, NLAYS3D + 1
          VGLVS_GD( L ) = VGLVS3D( L )
!0709     if ( MYPE .EQ. 0 ) print*,'L, VGLVS_GD(L):',L, VGLVS_GD(L)
        END DO

C...Create new source map of tagging regions

        LOGDEV = INIT3()

        FTYPE3D = GRDDED3
        SDATE3D = 2010256
        STIME3D = 0
        TSTEP3D = 10000
        NCOLS3D = GL_NCOLS
        NROWS3D = GL_NROWS
        NLAYS3D = 1
        NTHIK3D = 1
        GDTYP3D = GDTYP_GD
        P_ALP3D = P_ALP_GD
        P_BET3D = P_BET_GD
        P_GAM3D = P_GAM_GD
        XORIG3D = XORIG_GD
        YORIG3D = YORIG_GD
        XCENT3D = XCENT_GD
        YCENT3D = YCENT_GD
        XCELL3D = XCELL_GD
        YCELL3D = YCELL_GD
        VGTYP3D = VGTYP_GD
        VGTOP3D = VGTOP_GD

        DO L = 1, NLAYS3D + 1
          VGLVS3D( L ) = VGLVS_GD( L )
        END DO
        GDNAM3D = GDNAM  ! from HGRD_DEFN

        NVARS3D = NTAGS
        DO S = 1, NTAGS
          VTYPE3D( S ) = M3REAL
          WRITE( CDIGITS, '( I2 )' ) S
          CHR = CDIGITS
          IF ( S .LT. 10 ) CHR = "0" // CDIGITS( 2:2 )
          VNAME3D( S ) = 'RgnsInTag' // CHR
          UNITS3D( S ) = 'None'
          VDESC3D( S ) = 'Region '// VNAME3D( S )
        ENDDO

        FDESC3D = ''

      IF ( MYPE .EQ. 0 ) THEN
        IF ( .NOT. OPEN3( TAGGED_MAPNAME, FSNEW3, PNAME) )THEN
          WRITE(*, *) 'Error: open file failed (3)'
        ENDIF
      ENDIF 

C...Write to output

!0709 print*,'Within Subroutine WR_SRCRGN...'
!0709 print*,'MY_NCOLS, MY_NROWS'
!0709 print*, MY_NCOLS, MY_NROWS 


      ALLOCATE( BUFF2( MY_NCOLS, MY_NROWS ) )
      DO S = 1, NTAGS
!0709   print*,'S TAGRGN(S):',S, TAGRGN(S)
!0709   print*,'S, MFRC_P( 10, 10, S):',S, MFRC_P( 10, 10, S)
        BUFF2 = MFRC_P( 1:MY_NCOLS,1:MY_NROWS,S ) 
        IF ( .NOT. WRITE3( TAGGED_MAPNAME, VNAME3D( S ),
     &    SDATE3D, STIME3D, BUFF2 ) ) THEN
           XMSG = 'Could not write ' // TAGGED_MAPNAME
           CALL M3EXIT( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
        ENDIF
      ENDDO ! loop over ntags
      IF ( ALLOCATED( BUFF2 ) ) DEALLOCATE( BUFF2 )

      RETURN
      END SUBROUTINE WR_SRCRGN
