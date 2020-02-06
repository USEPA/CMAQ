
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
      SUBROUTINE CGRID_DEPV( GAS_DEPV_FOUND, GAS_DEPV_SUR, CGRID, CGRIDL1 )

C-----------------------------------------------------------------------
C Return layer one CGRID concentrations for the gas depv species with species
C first in the dimensions order - called by GET_DEPV [DEPV_DEFN module] to
C use for bi-directional flux calculations in m3dry
C Revision History:
C   Jeff Young 11 Jan 07: created
C   21 Jun 10 J.Young: convert for Namelist redesign
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C   1 Feb 19 David Wong: removed MY_N clauses
C-----------------------------------------------------------------------

      USE CGRID_SPCS            ! CGRID mechanism species
      USE HGRD_DEFN             ! horizontal domain specifications
      USE UTILIO_DEFN

C Arguments:

      LOGICAL, INTENT( IN )  :: GAS_DEPV_FOUND( : )
      INTEGER, INTENT( IN )  :: GAS_DEPV_SUR( : )
      REAL,    POINTER       :: CGRID( :,:,:,: )
      REAL,    INTENT( OUT ) :: CGRIDL1( :,:,: )

C Local Variables:

      LOGICAL, SAVE :: FIRSTIME = .TRUE.

       INTEGER C, R, S, N, V, NDX, STRT

C-----------------------------------------------------------------------
      IF ( FIRSTIME )  THEN
         FIRSTIME  =  .FALSE.
      END IF

      N = 0
      STRT = 0
      DO V = 1, N_GC_DEPV
         N = N + 1
         IF ( GAS_DEPV_FOUND( N ) ) THEN
            NDX = GAS_DEPV_SUR( N )
            S = STRT + GC_DEPV_MAP( V )
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  CGRIDL1( NDX,C,R ) = CGRID( C,R,1,S )
               END DO
            END DO
         END IF
      END DO

      STRT = N_GC_SPC + 1 + N_AE_SPC  ! +1 for RHOJ
      DO V = 1, N_NR_DEPV
         N = N + 1
         IF ( GAS_DEPV_FOUND( N ) ) THEN
            NDX = GAS_DEPV_SUR( N )
            S = STRT + NR_DEPV_MAP( V )
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  CGRIDL1( NDX,C,R ) = CGRID( C,R,1,S )
               END DO
            END DO
         END IF
      END DO

      STRT = N_GC_SPC + 1 + N_AE_SPC + N_NR_SPC
      DO V = 1, N_TR_DEPV
         N = N + 1
         IF ( GAS_DEPV_FOUND( N ) ) THEN
            NDX = GAS_DEPV_SUR( N )
            S = STRT + TR_DEPV_MAP( V )
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  CGRIDL1( NDX,C,R ) = CGRID( C,R,1,S )
               END DO
            END DO
         END IF
      END DO

      END SUBROUTINE CGRID_DEPV

C for cb05:
C  strt   v  n ndx  s   GAS_DEPV_FOUND  GAS_DEPV_SUR
C    0    1  1  3   1      (1)   .true.    3        VD_NO2          
C    0    2  2  4   2      (2)   .true.    4        VD_NO           
C    0    3  3  5   4      (3)   .true.    5        VD_O3           
C    0    4  4 19   5      (4)   .true.   19        VD_NO3          
C    0    5  5 18   9      (5)   .true.   18        VD_N2O5         
C    0    6  6  6  10      (6)   .true.    6        VD_HNO3         
C    0    7  7 15  11      (7)   .true.   15        VD_HONO         
C    0    8  8  7  13      (8)   .true.    7        VD_H2O2         
C    0    9  9 14  16      (9)   .true.   14        VD_PAN          
C    0   10 10 10  17     (10)   .true.   10        VD_OP           
C    0   11 11  9  18     (11)   .true.    9        VD_HCHO         
C    0   12 12  8  19     (12)   .true.    8        VD_ALD          
C    0   13 13 20  20     (13)   .true.   20        VD_GEN_ALD      
C    0   14 14 16  22     (14)   .true.   16        VD_CO           
C x  0   15 15 16  22     (15)  .false.   10        VD_OP           
C    0   16 16 17  25     (16)   .true.   17        VD_METHANOL     
C    0   17 17 12  27     (17)   .true.   12        VD_ORA          
C x  0   18 18 12  27     (18)  .false.   14        VD_PAN          
C    0   19 19 11  30     (19)   .true.   11        VD_PAA          
C x  0   20 20 11  30     (20)  .false.   12        VD_ORA          
C x  0   21 21 11  30     (21)  .false.   14        VD_PAN          
C    0   22 22  1  52     (22)   .true.    1        VD_SO2          
C    0   23 23  2  53     (23)   .true.    2        VD_SULF         
C   91    1 24 13  92     (24)   .true.   13        VD_NH3          
C x 91    2 25 13  92     (25)  .false.   12        VD_ORA          
C x 91    3 26 13  92     (26)  .false.   12        VD_ORA          
C x 91    4 27 13  92     (27)  .false.   12        VD_ORA          
C x 91    5 28 13  92     (28)  .false.   12        VD_ORA          
C x 91    6 29 13  92     (29)  .false.   12        VD_ORA          
C x 91    7 30 13  92     (30)  .false.   12        VD_ORA          
C x 91    8 31 13  92     (31)  .false.   12        VD_ORA          
C x 91    9 32 13  92     (32)  .false.   12        VD_ORA          
C x 91   10 33 13  92     (33)  .false.   12        VD_ORA          
C x 91   11 34 13  92     (34)  .false.   12        VD_ORA          
C   91   12 35 21 103     (35)   .true.   21        VD_HCL          

C for cb05cl:
C  strt   v  n ndx  s   GAS_DEPV_FOUND  GAS_DEPV_SUR
C    0    1  1  3   1        .true.         3       VD_NO2
C    0    2  2  4   2        .true.         4       VD_NO
C    0    3  3  5   4        .true.         5       VD_O3
C    0    4  4 19   5        .true.        19       VD_NO3
C    0    5  5 18   9        .true.        18       VD_N2O5
C    0    6  6  6  10        .true.         6       VD_HNO3
C    0    7  7 15  11        .true.        15       VD_HONO
C    0    8  8  7  13        .true.         7       VD_H2O2
C    0    9  9 14  16        .true.        14       VD_PAN
C    0   10 10 10  17        .true.        10       VD_OP
C    0   11 11  9  18        .true.         9       VD_HCHO
C    0   12 12  8  19        .true.         8       VD_ALD
C    0   13 13 20  20        .true.        20       VD_GEN_ALD
C    0   14 14 16  22        .true.        16       VD_CO
C    0   15 15 16  22       .false.        10       VD_OP
C    0   16 16 17  25        .true.        17       VD_METHANOL
C    0   17 17 12  27        .true.        12       VD_ORA
C    0   18 18 12  27       .false.        14       VD_PAN
C    0   19 19 11  30        .true.        11       VD_PAA
C    0   20 20 11  30       .false.        12       VD_ORA
C    0   21 21 11  30       .false.        14       VD_PAN
C    0   22 22  1  52        .true.         1       VD_SO2
C    0   23 23  2  53        .true.         2       VD_SULF
C    0   24 24 21  57        .true.        21       VD_CL2
C    0   25 25 22  59        .true.        22       VD_HOCL
C    0   26 26 24  61        .true.        24       VD_FMCL
C    0   27 27 23  62        .true.        23       VD_HCL
C  122    1 28 13 123        .true.        13       VD_NH3
C  122    2 29 13 123       .false.        12       VD_ORA
C  122    3 30 13 123       .false.        12       VD_ORA
C  122    4 31 13 123       .false.        12       VD_ORA
C  122    5 32 13 123       .false.        12       VD_ORA
C  122    6 33 13 123       .false.        12       VD_ORA
C  122    7 34 13 123       .false.        12       VD_ORA
C  122    8 35 13 123       .false.        12       VD_ORA
C  122    9 36 13 123       .false.        12       VD_ORA
C  122   10 37 13 123       .false.        12       VD_ORA
C  122   11 38 13 123       .false.        12       VD_ORA
C  122   12 39 13 123       .false.        12       VD_ORA
C  122   13 40 13 123       .false.        12       VD_ORA

