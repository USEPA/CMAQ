
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
      SUBROUTINE EDDYX ( EDDYV )

C--------------------------------------------------------------------------
C---- Eddy diffusivity (Kz) computed according to 2 different models:
C----   1- Boundary Layer scaling based on Hostlag and Boville (1993)
C----      Kz = k ust z(1-z/h)2 / phih
C       2- Local scaling based on local Richardson # and vertical shear
C          similar to Liu and Carroll (1996)
C
C  Revision History:
C  JEP        4/00 - CCTM implimentation from MM5
C  JEP        4/06 - Updated for ACM2
C  TLO       10/09 - Allow read of C-staggered UWINDC and VWINDC from
C                    MET_DOT_3D, and corrected algorithm that computes
C                    component-wise shear so that B-staggered winds
C                    will be properly used.  Removed map-scale factor
C                    from wind shear calculation.
C  SJR       02/11 - replaced I/O API include files with UTILIO_DEFN
C  YOJ       05/13 - access met data from VDIFF_MET module
C  JOB       11/14 - Updated for the ASX_DATA_MOD shared data module which 
C                    included variables in the VDIFF_MET module.
C  HF        07/15 - Included pleim5 formulation. 
C  David Wong 02/19 - removed all MY_N clauses
C--------------------------------------------------------------------------

      USE GRID_CONF             ! horizontal domain specifications
      USE ASX_DATA_MOD          ! Includes CONST
      USE UTILIO_DEFN

      IMPLICIT NONE

C Includes:

C Arguments:
      REAL,   INTENT( OUT ) :: EDDYV ( :,:,: ) ! eddy diffusivity (m**2/s)

C Parameters:

      REAL, PARAMETER :: RLAM   = 80.0 ! asymptotic mixing length (m)
      REAL, PARAMETER :: RIC    = 0.25 ! critical Richardson #
      REAL, PARAMETER :: QUARTER = 0.25
      REAL, PARAMETER :: SIXTEENTH = QUARTER * QUARTER  ! 1/16

C External Functions: None

C File Variables:
      REAL    KZM                            ! local KZMIN

C Local variables:
      INTEGER MDATE, MTIME, STEP
      INTEGER C, R, L, V

      REAL    DZL                     ! Z(L+1)-Z(L)
      REAL    WW2                     ! (wind speed)**2
      REAL    WS2                     ! (wind shear)**2
      REAL    RIB                     ! Bulk Richardson Number
      REAL    ZOL
      REAL    ZFUNC, HPBL 
      REAL    EDDV                    ! local EDDYV
      REAL    FH

      INTEGER MCOL                   ! these don't need to be initialized
      INTEGER MROW
      INTEGER MLVL
      REAL    MTH1                   ! pot. temp. in layer L
      REAL    MTH2                   ! pot. temp. in layer L+1
      REAL    MRIB                   ! bulk Richardson Number
      REAL    MWS                    ! wind shear (/sec)
      REAL    MEDDYV                 ! eddy diffusivity (m**2/sec)

      REAL    QMEAN, TMEAN
      REAL    XLV, ALPH, CHI
      REAL    CPAIR, ZK, SQL, PHIH
      REAL    PHIM
      REAL    WT, ZSOL
      REAL    EDYZ
      REAL    ZFL                    ! local ZF

      CHARACTER( 16 ) :: PNAME = 'EDDYX'
      CHARACTER( 16 ) :: VNAME
      CHARACTER( 16 ) :: UNITSCK
      CHARACTER( 96 ) :: XMSG = ' '

C-----------------------------------------------------------------------

      MEDDYV = 0.0

      DO 233 L = 1, NLAYS-1
      DO 222 R = 1, NROWS
      DO 211 C = 1, NCOLS
         HPBL = MAX( Met_Data%PBL( C,R ), 20.0 )
         ZFL = Met_Data%ZF( C,R,L )
         KZM = Met_Data%KZMIN( C,R,L )

         ZOL = ZFL * Met_Data%MOLI( C,R )
         IF ( ZFL .LT. HPBL ) THEN
            IF ( ZOL .LT. 0.0 ) THEN
               IF ( ZFL .LT. 0.1 * HPBL ) THEN
                  PHIH = 1.0 / SQRT( 1.0 - GAMAH * ZOL )
               ELSE
                  ZSOL = 0.1 * HPBL * Met_Data%MOLI( C,R )
                  PHIH = 1.0 / SQRT( 1.0 - GAMAH * ZSOL )
               END IF
            ELSE IF ( ZOL .LT. 1.0 ) THEN
               PHIH = 1.0 + BETAH * ZOL
            ELSE
               PHIH = BETAH + ZOL
            END IF
            WT = Met_Data%USTAR( C,R ) / PHIH
            ZFUNC = 1.0 - ZFL / HPBL
            ZFUNC = ZFL * ZFUNC * ZFUNC
            EDYZ = KARMAN * WT * ZFUNC
            EDYZ = MAX( EDYZ, KZM )
         ELSE
            EDYZ = 0.0
         END IF

         IF ( CSTAGUV ) THEN  ! u- and v-component winds on C-stagger
           WW2 = QUARTER                  ! component-wise wind shear
     &         * ( ( Met_Data%UWIND( C+1,R,  L+1 ) - Met_Data%UWIND( C+1,R  ,L  )
     &             + Met_Data%UWIND( C,  R,  L+1 ) - Met_Data%UWIND( C,  R  ,L  ) ) ** 2
     &         +   ( Met_Data%VWIND( C,  R+1,L+1 ) - Met_Data%VWIND( C,  R+1,L )
     &             + Met_Data%VWIND( C,  R,  L+1 ) - Met_Data%VWIND( C,  R,  L  ) ) ** 2 )
         ELSE  ! u- and v-component winds on B-stagger
           WW2 = SIXTEENTH                ! component-wise wind shear
     &         * ( ( Met_Data%UWIND( C,  R,  L+1 ) - Met_Data%UWIND( C,  R  ,L  )
     &             + Met_Data%UWIND( C+1,R,  L+1 ) - Met_Data%UWIND( C+1,R  ,L  )
     &             + Met_Data%UWIND( C,  R+1,L+1 ) - Met_Data%UWIND( C,  R+1,L  )
     &             + Met_Data%UWIND( C+1,R+1,L+1 ) - Met_Data%UWIND( C+1,R+1,L  ) ) ** 2
     &          +  ( Met_Data%VWIND( C,  R,  L+1 ) - Met_Data%VWIND( C,  R  ,L  )
     &             + Met_Data%VWIND( C+1,R,  L+1 ) - Met_Data%VWIND( C+1,R  ,L  )
     &             + Met_Data%VWIND( C,  R+1,L+1 ) - Met_Data%VWIND( C,  R+1,L  )
     &             + Met_Data%VWIND( C+1,R+1,L+1 ) - Met_Data%VWIND( C+1,R+1,L  ) ) ** 2 )
         END IF

         DZL = Met_Data%ZH( C,R,L+1 ) - Met_Data%ZH( C,R,L )
         WS2 = WW2 / ( DZL * DZL ) + 1.0E-9

         RIB = 2.0 * GRAV * ( Met_Data%THETAV( C,R,L+1 ) - Met_Data%THETAV( C,R,L ) )
     &      / ( DZL * WS2 * ( Met_Data%THETAV( C,R,L+1 ) + Met_Data%THETAV( C,R,L ) ) )

C-- Adjustment to vert diff in Moist air from HIRPBL

         IF ( ( Met_Data%QC( C,R,L ) .GT. 0.01E-3 ) .OR.
     &        ( Met_Data%QC( C,R,L+1 ) .GT. 0.01E-3 ) ) THEN
            QMEAN = 0.5 * ( Met_Data%QV( C,R,L ) + Met_Data%QV( C,R,L+1 ) )
            TMEAN = 0.5 * ( Met_Data%TA( C,R,L ) + Met_Data%TA( C,R,L+1 ) )
            XLV = ( 2.501 - 0.00237 * ( TMEAN - 273.15 ) ) * 1.0E6
            ALPH = XLV * QMEAN / RDGAS / TMEAN
            CPAIR = 1004.67 * ( 1.0 + 0.84 * Met_Data%QV( C,R,L ) )   ! J/(K KG)
            CHI = XLV * XLV * QMEAN / ( CPAIR * RWVAP * TMEAN * TMEAN )
            RIB = ( 1.0 + ALPH )
     &          * ( RIB - GRAV * GRAV / ( WS2 * TMEAN * CPAIR )
     &          * ( ( CHI - ALPH ) / ( 1.0 + CHI ) ) )
         END IF

         ZK = 0.4 * ZFL
         SQL = ZK * RLAM / ( RLAM + ZK )
         SQL = SQL * SQL

         IF ( RIB .GE. 0.0 ) THEN
!           FH = 1.0   ! pleim5
!    &         / ( 1.0 + 10.0 * RIB + 50.0 * RIB ** 2 + 5000.0 * RIB ** 4 ) + 0.0012
            FH =  1.0 + RIB * ( 10.0 + RIB * ( 50.0 + 5000.0 * RIB * RIB ) )
            FH = 0.0012 + 1.0 / FH  ! pleim5

            EDDV = KZM + SQRT( WS2 ) * FH * SQL
         ELSE
            EDDV = KZM + SQRT( WS2 * ( 1.0 - 25.0 * RIB ) ) * SQL
         END IF

         IF ( ZFL .LT. HPBL .AND. EDYZ .GT. EDDV ) THEN
            EDDV = EDYZ
         END IF
         EDDV = MIN( 1000.0, EDDV )

         IF ( EDDV .GT. MEDDYV ) THEN
C Capture the col, row, lvl, and EDDYV for the global min DT
            MCOL = C
            MROW = R
            MLVL = L
            MEDDYV = EDDV
            MTH1 = Met_Data%THETAV( C,R,L )
            MTH2 = Met_Data%THETAV( C,R,L+1 )
            MRIB = RIB
            MWS  = SQRT ( WS2 )
         END IF

         EDDYV( C,R,L ) = EDDV

211   CONTINUE       !  end loop on columns
222   CONTINUE       !  end loop on rows
233   CONTINUE       !  end loop on levels

      !WRITE( LOGDEV,* ) ' '
      !WRITE( LOGDEV,1001 ) MEDDYV, MCOL, MROW, MLVL
1001  FORMAT(/ 5X, 'Maximum eddy diffusivity of:', 1PG13.5,
     &         1X, '(m**2/sec)'
     &       / 5X, 'at col, row, layer:', I4, 2(', ', I3) )
      !WRITE( LOGDEV,1003 ) MWS, MRIB, MTH1, MTH2
1003  FORMAT(  5X, 'corresponding to a free tropospheric wind shear of:',
     &         1PG13.5,  1X, '(/sec),'
     &        /28X, 'a bulk Richardson Number of:', 1PG13.5, ','
     &        / 5X, 'and pot. temps. in layer and layer+1:', 2(1PG13.5) )
      WRITE( LOGDEV,* ) '    '

      DO R = 1, NROWS
         DO C = 1, NCOLS
            EDDYV( C,R,NLAYS ) = 0.0
         END DO
      END DO

      RETURN
      END
