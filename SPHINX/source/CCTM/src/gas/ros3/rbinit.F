
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
C $Header: /project/yoj/arc/CCTM/src/gas/ros3/rbinit.F,v 1.4 2011/10/21 16:11:10 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE RBINIT
C***********************************************************************
C
C  Function: To initialize species tolerances, allocate arrays, and 
C            define grid structure
C
C  Preconditions: None
C
C  Return Values: None
C
C  Key Subroutines/Functions CALLED: None
C
C  Revision History: Prototype created by Jerry Gipson, August, 2004
C                    31 Jan 05 J.Young: dyn alloc - establish both horizontal
C                    & vertical domain specifications in one module (GRID_CONF)
C                    Get BLKSIZE from module GRID_CONF
C                    29 Jul 05 WTH: allocate variables used by degrade routines.
C                    28 Jun 10 J.Young: convert for Namelist redesign
C                    29 Mar 11 S.Roselle: Replaced I/O API include files
C                               with UTILIO_DEFN
C                   15 Jul 14 B.Hutzell: 1) replaced mechanism include files with 
C                   RXNS_DATA module, 2) inserted call to function MAP_CHEMISTRY_SPECIES 
C                   RXNS_FUNCTION module, and 3) inserted do loop that calculates species
C                   unit conversion factors based on species type
C                   21 Mar 22 G . Sarwar: changed BLKLAND to BLKSEAWATER
C                      
C***********************************************************************
      USE RXNS_DATA
      USE GRID_CONF                ! horizontal & vertical domain specifications
      USE RBDATA                   ! Rosenbrock solver data
      USE CGRID_SPCS               ! CGRID mechanism species
      USE UTILIO_DEFN
      USE RXNS_FUNCTION

      IMPLICIT NONE

C.....Includes:
      Include SUBST_CONST          ! common constants

C.....Arguments: NONE

C.....Parameters:
      CHARACTER( 16 ), PARAMETER   :: PNAME = 'RBINIT'    ! Procedure name

C.....External Functions:

C.....Local Variables: 
      CHARACTER( 132 ) :: XMSG     ! Log error message
      CHARACTER(  80 ) :: VARDESC  ! Description of environment variable 

      INTEGER N                    ! Loop index

      INTEGER COL                  ! Column number index
      INTEGER IAVGSIZE             ! Average number of cells per block
      INTEGER LEV                  ! Level number index
      INTEGER OFFSET               ! Pointer for start cell number in a block
      INTEGER NBLK                 ! Block number index
      INTEGER NCOUNT               ! Counter for number of cells for grid
      INTEGER NOXYZ                ! Total number of cells for grid
      INTEGER ROW                  ! Row number index
      INTEGER STATUS               ! Status code for functions

      REAL    DEFTOL               ! Default tolerance value

C***********************************************************************

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c..Initialize vars & allocate arrays used in sparse matrix treatment
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      LREORDER = .TRUE.

      N_SPEC = NUMB_MECH_SPC
      N_RXNS = NRXNS   ! loads RBDATA from RXCM.EXT

      MXRR = 3 * MXRCT
      MXRP = 3 * MXPRD

      MXCOUNT1 = NUMB_MECH_SPC * MAXGL3 * 3
      MXCOUNT2 = NUMB_MECH_SPC * MAXGL3 * 3

      ALLOCATE( NKUSERAT( NRXNS,NCS2 ),
     &          NDERIVL ( NRXNS,NCS2 ),
     &          NDERIVP ( NRXNS,NCS2 ), STAT = STATUS )
      IF ( STATUS .NE. 0 ) THEN
         XMSG = 'ERROR allocating NKUSERAT, NDERIVL or NDERIVP'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( IRM2( NRXNS,MXRCT+MXPRD,NCS2 ),
     &          ICOEFF( NRXNS,MXRP,NCS2 ), STAT = STATUS )
      IF ( STATUS .NE. 0 ) THEN
         XMSG = 'ERROR allocating IRM2 or ICOEFF'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( JARRAYPT( NUMB_MECH_SPC,NUMB_MECH_SPC,NCS2 ), STAT = STATUS )
      IF ( STATUS .NE. 0 ) THEN
         XMSG = 'ERROR allocating JARRAYPT'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( JARRL( NRXNS,MXRR,NCS2 ),
     &          JARRP( NRXNS,MXRP,NCS2 ),
     &          JLIAL( NRXNS,MXRR,NCS2 ),
     &          JPIAL( NRXNS,MXRP,NCS2 ), STAT = STATUS )
      IF ( STATUS .NE. 0 ) THEN
         XMSG = 'ERROR allocating JARRL, JARRP, JLIAL, or JPIAL'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( INEW2OLD( NUMB_MECH_SPC,NCS ),
     &          IOLD2NEW( NUMB_MECH_SPC,NCS ), STAT = STATUS )
      IF ( STATUS .NE. 0 ) THEN
         XMSG = 'ERROR allocating INEW2OLD or IOLD2NEW'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( JZEROA( MXARRAY ),
     &          JZEROB( MXARRAY ), STAT = STATUS )
      IF ( STATUS .NE. 0 ) THEN
         XMSG = 'ERROR allocating JZEROA or JZEROB'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( JZLO( NCS2 ),
     &          IDEC1LO( NUMB_MECH_SPC,NCS2 ),
     &          IDEC1HI( NUMB_MECH_SPC,NCS2 ), STAT = STATUS )
      IF ( STATUS .NE. 0 ) THEN
         XMSG = 'ERROR allocating JZLO, IDEC1LO or IDEC1HI'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( IJDECA( MXCOUNT2 ),
     &          IJDECB( MXCOUNT2 ),
     &          IKDECA( MXCOUNT2 ),
     &          IKDECB( MXCOUNT2 ),
     &          KJDECA( MXCOUNT2 ),
     &          KJDECB( MXCOUNT2 ), STAT = STATUS )
      IF ( STATUS .NE. 0 ) THEN
         XMSG = 'ERROR allocating IJDECA, IJDECB, IKDECA, IKDECB, KJDECA, or KJDECB'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( JHIZ1( NUMB_MECH_SPC,NCS2 ),
     &          JHIZ2( NUMB_MECH_SPC,NCS2 ),
     &          KZLO1( NUMB_MECH_SPC,NCS2 ),
     &          KZLO2( NUMB_MECH_SPC,NCS2 ),
     &          KZHI0( NUMB_MECH_SPC,NCS2 ),
     &          KZHI1( NUMB_MECH_SPC,NCS2 ), STAT = STATUS )
      IF ( STATUS .NE. 0 ) THEN
         XMSG = 'ERROR allocating JHIZ1, JHIZ2, KZLO1, KZLO2, KZHI0, or KZHI1'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( KZERO( MXARRAY,NCS2 ), STAT = STATUS )
      IF ( STATUS .NE. 0 ) THEN
         XMSG = 'ERROR allocating KZERO'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( KZILCH( NUMB_MECH_SPC,NCS2 ),
     &          MZHI0 ( NUMB_MECH_SPC,NCS2 ),
     &          MZHI1 ( NUMB_MECH_SPC,NCS2 ),
     &          MZILCH( NUMB_MECH_SPC,NCS2 ),
     &          MZLO1 ( NUMB_MECH_SPC,NCS2 ),
     &          MZLO2 ( NUMB_MECH_SPC,NCS2 ), STAT = STATUS )
      IF ( STATUS .NE. 0 ) THEN
         XMSG = 'ERROR allocating KZILCH, MZHI0, MZHI1, MZILCH, MZLO1, or MZLO2'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( VDIAG( BLKSIZE,NUMB_MECH_SPC ), STAT = STATUS )
      IF ( STATUS .NE. 0 ) THEN
         XMSG = 'ERROR allocating VDIAG'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( CC2( BLKSIZE,0:MXARRAY ), STAT = STATUS )
      IF ( STATUS .NE. 0 ) THEN
         XMSG = 'ERROR allocating CC2'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

c..cell & solver data
      ALLOCATE( BLKTEMP( BLKSIZE ),
     &          BLKPRES( BLKSIZE ),
     &          BLKCH2O( BLKSIZE ),
     &          BLKDENS( BLKSIZE ),
     &          BLKSVOL( BLKSIZE ), 
     &          BLKSEAWATER( BLKSIZE ), STAT = STATUS )
      IF ( STATUS .NE. 0 ) THEN
         XMSG = 'ERROR allocating BLKTEMP, BLKPRES, BLKCH2O, BLKDENS, '
     &       // 'BLKSVOL, BLKSEAWATER '
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      NJPHOT  = NPHOTAB
      ALLOCATE( RJBLK( BLKSIZE,NJPHOT ), STAT = STATUS )    
      IF ( STATUS .NE. 0 ) THEN
         XMSG = 'ERROR allocating RJBLK'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      ALLOCATE( RKI( BLKSIZE,NRXNS ),
     &          ATOL( NUMB_MECH_SPC ),
     &          RTOL( NUMB_MECH_SPC ),
     &          FORWARD_CONV( NUMB_MECH_SPC ),
     &          REVERSE_CONV( NUMB_MECH_SPC ),
     &          Y( BLKSIZE,NUMB_MECH_SPC ), STAT = STATUS )
      IF ( STATUS .NE. 0 ) THEN
         XMSG = 'ERROR allocating RKI, ATOL, RTOL, or Y'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

#ifdef sens
      ALLOCATE( YAVE( BLKSIZE,NUMB_MECH_SPC ), STAT = STATUS )
      IF ( STATUS .NE. 0 ) THEN
         XMSG = 'ERROR allocating YAVE'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF
#endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set convergence tolerances for each species; currently uses
c  one set of tolerances for all species
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( .NOT. MAP_CHEMISTRY_SPECIES() )THEN
         XMSG = 'Detected above error(s) when mapping Chemistry species from CGRID species'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      DO N = 1, NUMB_MECH_SPC
         ATOL( N ) = REAL( GLBL_ATOL, 8 )
         RTOL( N ) = REAL( GLBL_RTOL, 8 )
         FORWARD_CONV( N ) = 1.0E-3 * MWAIR / REAL( SPECIES_MOLWT( N ) )
         REVERSE_CONV( N ) = REAL( 1.0 / FORWARD_CONV( N ), 8 )
      END DO
      

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get number of cells in grid and store i,j,k indices of cells in
c  sequential order
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NCOUNT = 0
      DO LEV = 1, NLAYS
         DO COL = 1, NCOLS
            DO ROW = 1, NROWS
!        DO ROW = 1, NROWS
!           DO COL = 1, NCOLS
               NCOUNT = NCOUNT + 1
               CCOL( NCOUNT ) = COL
               CROW( NCOUNT ) = ROW
               CLEV( NCOUNT ) = LEV
            END DO
         END DO
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Define block structure for grid; stop if maxblks exceeded
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NOXYZ = NCOUNT
      NBLKS = 1 + ( NOXYZ - 1 ) / BLKSIZE
      IF ( NBLKS .GT. MXBLKS ) THEN
         WRITE( XMSG, 92020 ) NBLKS, MXBLKS
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      IAVGSIZE = 1 + ( NOXYZ - 1 ) / NBLKS
      IAVGSIZE = MIN( IAVGSIZE, BLKSIZE )
      OFFSET = 0

      DO NBLK = 1, NBLKS - 1
         BLKCNO( NBLK ) = OFFSET
         BLKLEN( NBLK ) = IAVGSIZE
         OFFSET = OFFSET + IAVGSIZE
      END DO
      BLKCNO( NBLKS ) = OFFSET
      BLKLEN( NBLKS ) = NOXYZ - ( ( NBLK-1 ) * IAVGSIZE )

      RETURN
      
C********************** FORMAT Statements ******************************      
92020 FORMAT( 1X, 'ERROR: Maximum Number of Blocks Exceeded',
     &            ' for Grid', 'NBLKS=', I3, 1X, ' MAXBLKS=',
     &            I3, '  Change GRPARMS.EXT' )

      END
