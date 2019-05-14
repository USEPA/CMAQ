
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

      MODULE BC_PARMS

C......................................................................
C  From INCLUDE FILE: BC_PARMS.EXT

C  CONTAINS: Dimensioning parameters and data for BCON

C  DEPENDENT UPON: None

C  REVISION HISTORY: Prototype created by Jerry Gipson, January, 1998
C                    Modified by JG May, 1999 to treat PinG concs
C                    Remove PING by J.Young July, 2011
C......................................................................


!terra5:/home/yoj/src/arc/BCON-git> find /home/yoj/src/arc/BCON-git -name '*.[fF]' -exec grep -i BC_PARMS.EXT {} \; -print
!      INCLUDE 'BC_PARMS.EXT'    ! BCON paramters
!/home/yoj/src/arc/BCON-git/common/bcon.F
!      INCLUDE 'BC_PARMS.EXT'    ! BCON parameters
!/home/yoj/src/arc/BCON-git/common/lst_spc_map.F
!      INCLUDE 'BC_PARMS.EXT'    ! BCON parameters
!/home/yoj/src/arc/BCON-git/common/ngc_spc_map.F
!      INCLUDE 'BC_PARMS.EXT'    ! BCON parameters
!/home/yoj/src/arc/BCON-git/common/opn_bc_file.F
!      INCLUDE 'BC_PARMS.EXT'    ! BCON paramters
!/home/yoj/src/arc/BCON-git/m3conc/m3_bcout.F
!      INCLUDE 'BC_PARMS.EXT'    ! BCON paramters
!/home/yoj/src/arc/BCON-git/m3conc/m3_ck_bdy.F
!      INCLUDE 'BC_PARMS.EXT'    ! ICON parameters
!/home/yoj/src/arc/BCON-git/m3conc/m3_ck_ctms.F
!      INCLUDE 'BC_PARMS.EXT'    ! BCON paramters
!/home/yoj/src/arc/BCON-git/m3conc/m3_ck_met.F
!      INCLUDE 'BC_PARMS.EXT'    ! BCON paramters
!/home/yoj/src/arc/BCON-git/m3conc/m3_driver.F
!      INCLUDE 'BC_PARMS.EXT'    ! BCON paramters
!/home/yoj/src/arc/BCON-git/m3conc/m3_vinterp.F
!      INCLUDE 'BC_PARMS.EXT'    ! BCON paramters
!/home/yoj/src/arc/BCON-git/profile/prof_bcout.F
!      INCLUDE 'BC_PARMS.EXT'    ! BCON paramters
!/home/yoj/src/arc/BCON-git/profile/prof_driver.F
!      INCLUDE 'BC_PARMS.EXT'    ! BCON paramters
!/home/yoj/src/arc/BCON-git/profile/prof_vinterp.F
!      INCLUDE 'BC_PARMS.EXT'    ! BCON paramters
!/home/yoj/src/arc/BCON-git/tracer/trac_bc.F
!      INCLUDE 'BC_PARMS.EXT'    ! BCON paramters
!/home/yoj/src/arc/BCON-git/tracer/trac_driver.F

C Dimensioning parameters

      INTEGER, PARAMETER :: MX_MECHFL_BC = 500  ! Max no. of species

      INTEGER, PARAMETER :: MX_INFL_SP = 500    ! Max no. of species in an input file

      INTEGER, PARAMETER :: MXCTMS = 3          ! Max no. of input CTM files

C Type declarations

C Root names of possible output files
      INTEGER, PARAMETER :: NROOTS = 5
      CHARACTER( 16 ) :: BCFL_ROOT( NROOTS ) = (/  ! Output file root names
     &                   'BNDY_CONC_',
     &                   'BNDY_GASC_',
     &                   'BNDY_AERO_',
     &                   'BNDY_NONR_',
     &                   'BNDY_TRAC_' /)

C Catalogued tracer species names
      INTEGER, PARAMETER :: NTRACERS = 8  ! No. of catalogued tracers
      CHARACTER( 16 ) :: TRAC_NAME( NTRACERS ) = (/ ! Catalogued tracer names
     &                   'IC1_BC0',
     &                   'IC1_BC1',
     &                   'IC0_BC1',
     &                   'STREET ',
     &                   'CHKBRD ',
     &                   'SPOS_A ',
     &                   'SPOS_B ',
     &                   'SPOS_C ' /)

C Descriptions of tracers species
      CHARACTER( 80 ) :: TRAC_DESC( NTRACERS ) = (/ ! Description of catalogued tracers
     &                   'Uniform IC/BC with IC = 1.0 and BC 0.0   ',
     &                   'Uniform IC/BC with IC = 1.0 and BC 1.0   ',
     &                   'Uniform IC/BC with IC = 0.0 and BC 0.0   ',
     &                   'Checkerboard pattern of IC/BC = 1.0 & 0.0',
     &                   'Street pattern of IC/BC = 1.0 & 0.0      ',
     &                   'Signal A for superposition test, A-B-C=0 ',
     &                   'Signal B for superposition test, A-B-C=0 ',
     &                   'Signal C for superposition test, A-B-C=0 ' /)

C Descriptions of catalogued vertical coordinate systems
      CHARACTER( 23 ) :: VGDESC( 8 ) = (/        ! Description of vertical grid type
     &                   'Hydrostatic sigma-p    ',
     &                   'Non-hydrostatic sigma-p',
     &                   'Sigma-z                ',
     &                   'Pressure (mb)          ',
     &                   'Z (m above sea level)  ',
     &                   'H (m above ground      ',
     &                   'WRF mass-core sigma    ',
     &                   'WRF NMM                ' /)

C When generating "fine grid" bcon output from a "coarse grid" concentration file...
C If the vertical grid type is different between the input concentration file and
C the targeted, output bcon file, the mid-layer height fields (ZH) for both the
C "coarse" input and the "fine" output are required from appropriate MET_CRO_3D files.
      CHARACTER( 16 ) :: MET_CRO_3D_CRS = 'MET_CRO_3D_CRS'   ! coarse grid
      CHARACTER( 16 ) :: MET_BDY_3D_FIN = 'MET_BDY_3D_FIN'   ! fine grid

      END MODULE BC_PARMS

