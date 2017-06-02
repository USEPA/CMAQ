
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
C $Header$

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE HRRATES

C***********************************************************************
C
C  FUNCTION: To calculate rates of reactions
C
C  PRECONDITIONS: For the CB05TUCL family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, May 26, 2017
C
C***********************************************************************
      USE HRDATA

      IMPLICIT NONE

C.....INCLUDES: NONE


C.....ARGUMENTS: NONE


C.....PARAMETERS: NONE


C.....EXTERNAL FUNCTIONS: NONE


C.....SAVED VARIABLES: NONE
!     CHARACTER( 16 ), SAVE  ::  PNAME = 'HRRATES'   ! Program name


C.....LOCAL VARIABLES: None

C***********************************************************************
C..NOTE: RXRAT commented out are not referenced by any routine
!     RXRAT(    1 ) = RKI(   1 ) * YC(  NO2 )
!     RXRAT(    2 ) = RKI(   2 ) * YC(  O )
!     RXRAT(    3 ) = RKI(   3 ) * YC(  O3 ) * YC(  NO )
!     RXRAT(    4 ) = RKI(   4 ) * YC(  O ) * YC(  NO2 )
      RXRAT(    5 ) = RKI(   5 ) * YC(  O ) * YC(  NO2 )
!     RXRAT(    6 ) = RKI(   6 ) * YC(  O ) * YC(  NO )
      RXRAT(    7 ) = RKI(   7 ) * YC(  NO2 ) * YC(  O3 )
      RXRAT(    8 ) = RKI(   8 ) * YC(  O3 )
      RXRAT(    9 ) = RKI(   9 ) * YC(  O3 )
!     RXRAT(   10 ) = RKI(  10 ) * YC(  O1D )
!     RXRAT(   11 ) = RKI(  11 ) * YC(  O1D )
!     RXRAT(   12 ) = RKI(  12 ) * YC(  O3 ) * YC(  OH )
!     RXRAT(   13 ) = RKI(  13 ) * YC(  O3 ) * YC(  HO2 )
      RXRAT(   14 ) = RKI(  14 ) * YC(  NO3 )
      RXRAT(   15 ) = RKI(  15 ) * YC(  NO3 )
      RXRAT(   16 ) = RKI(  16 ) * YC(  NO3 ) * YC(  NO )
      RXRAT(   17 ) = RKI(  17 ) * YC(  NO3 ) * YC(  NO2 )
!     RXRAT(   18 ) = RKI(  18 ) * YC(  NO3 ) * YC(  NO2 )
      RXRAT(   19 ) = RKI(  19 ) * YC(  N2O5 )
      RXRAT(   20 ) = RKI(  20 ) * YC(  N2O5 )
      RXRAT(   21 ) = RKI(  21 ) * YC(  N2O5 )
!     RXRAT(   22 ) = RKI(  22 ) * YC(  NO ) * YC(  NO )
      RXRAT(   23 ) = RKI(  23 ) * YC(  NO ) * YC(  NO2 )
!     RXRAT(   24 ) = RKI(  24 ) * YC(  NO ) * YC(  OH )
      RXRAT(   25 ) = RKI(  25 ) * YC(  HONO )
      RXRAT(   26 ) = RKI(  26 ) * YC(  OH ) * YC(  HONO )
      RXRAT(   27 ) = RKI(  27 ) * YC(  HONO ) * YC(  HONO )
      RXRAT(   28 ) = RKI(  28 ) * YC(  NO2 ) * YC(  OH )
      RXRAT(   29 ) = RKI(  29 ) * YC(  OH ) * YC(  HNO3 )
!     RXRAT(   30 ) = RKI(  30 ) * YC(  HO2 ) * YC(  NO )
!     RXRAT(   31 ) = RKI(  31 ) * YC(  HO2 ) * YC(  NO2 )
      RXRAT(   32 ) = RKI(  32 ) * YC(  PNA )
      RXRAT(   33 ) = RKI(  33 ) * YC(  OH ) * YC(  PNA )
      RXRAT(   34 ) = RKI(  34 ) * YC(  HO2 ) * YC(  HO2 )
      RXRAT(   35 ) = RKI(  35 ) * YC(  HO2 ) * YC(  HO2 )
      RXRAT(   36 ) = RKI(  36 ) * YC(  H2O2 )
      RXRAT(   37 ) = RKI(  37 ) * YC(  OH ) * YC(  H2O2 )
!     RXRAT(   38 ) = RKI(  38 ) * YC(  O1D )
!     RXRAT(   39 ) = RKI(  39 ) * YC(  OH )
!     RXRAT(   40 ) = RKI(  40 ) * YC(  OH ) * YC(  O )
      RXRAT(   41 ) = RKI(  41 ) * YC(  OH ) * YC(  OH )
      RXRAT(   42 ) = RKI(  42 ) * YC(  OH ) * YC(  OH )
!     RXRAT(   43 ) = RKI(  43 ) * YC(  OH ) * YC(  HO2 )
!     RXRAT(   44 ) = RKI(  44 ) * YC(  HO2 ) * YC(  O )
      RXRAT(   45 ) = RKI(  45 ) * YC(  H2O2 ) * YC(  O )
      RXRAT(   46 ) = RKI(  46 ) * YC(  NO3 ) * YC(  O )
      RXRAT(   47 ) = RKI(  47 ) * YC(  NO3 ) * YC(  OH )
      RXRAT(   48 ) = RKI(  48 ) * YC(  NO3 ) * YC(  HO2 )
      RXRAT(   49 ) = RKI(  49 ) * YC(  NO3 ) * YC(  O3 )
      RXRAT(   50 ) = RKI(  50 ) * YC(  NO3 ) * YC(  NO3 )
      RXRAT(   51 ) = RKI(  51 ) * YC(  PNA )
      RXRAT(   52 ) = RKI(  52 ) * YC(  HNO3 )
      RXRAT(   53 ) = RKI(  53 ) * YC(  N2O5 )
      RXRAT(   54 ) = RKI(  54 ) * YC(  XO2 ) * YC(  NO )
      RXRAT(   55 ) = RKI(  55 ) * YC(  XO2N ) * YC(  NO )
      RXRAT(   56 ) = RKI(  56 ) * YC(  XO2 ) * YC(  HO2 )
      RXRAT(   57 ) = RKI(  57 ) * YC(  XO2N ) * YC(  HO2 )
      RXRAT(   58 ) = RKI(  58 ) * YC(  XO2 ) * YC(  XO2 )
      RXRAT(   59 ) = RKI(  59 ) * YC(  XO2N ) * YC(  XO2N )
      RXRAT(   60 ) = RKI(  60 ) * YC(  XO2 ) * YC(  XO2N )
      RXRAT(   61 ) = RKI(  61 ) * YC(  NTR ) * YC(  OH )
      RXRAT(   62 ) = RKI(  62 ) * YC(  NTR )
      RXRAT(   63 ) = RKI(  63 ) * YC(  ROOH ) * YC(  OH )
      RXRAT(   64 ) = RKI(  64 ) * YC(  ROOH )
      RXRAT(   65 ) = RKI(  65 ) * YC(  OH ) * YC(  CO )
      RXRAT(   66 ) = RKI(  66 ) * YC(  OH )
      RXRAT(   67 ) = RKI(  67 ) * YC(  MEO2 ) * YC(  NO )
      RXRAT(   68 ) = RKI(  68 ) * YC(  MEO2 ) * YC(  HO2 )
      RXRAT(   69 ) = RKI(  69 ) * YC(  MEO2 ) * YC(  MEO2 )
      RXRAT(   70 ) = RKI(  70 ) * YC(  MEPX ) * YC(  OH )
      RXRAT(   71 ) = RKI(  71 ) * YC(  MEPX )
      RXRAT(   72 ) = RKI(  72 ) * YC(  MEOH ) * YC(  OH )
      RXRAT(   73 ) = RKI(  73 ) * YC(  FORM ) * YC(  OH )
      RXRAT(   74 ) = RKI(  74 ) * YC(  FORM )
      RXRAT(   75 ) = RKI(  75 ) * YC(  FORM )
      RXRAT(   76 ) = RKI(  76 ) * YC(  FORM ) * YC(  O )
      RXRAT(   77 ) = RKI(  77 ) * YC(  FORM ) * YC(  NO3 )
      RXRAT(   78 ) = RKI(  78 ) * YC(  FORM ) * YC(  HO2 )
      RXRAT(   79 ) = RKI(  79 ) * YC(  HCO3 )
      RXRAT(   80 ) = RKI(  80 ) * YC(  HCO3 ) * YC(  NO )
      RXRAT(   81 ) = RKI(  81 ) * YC(  HCO3 ) * YC(  HO2 )
      RXRAT(   82 ) = RKI(  82 ) * YC(  FACD ) * YC(  OH )
      RXRAT(   83 ) = RKI(  83 ) * YC(  ALD2 ) * YC(  O )
      RXRAT(   84 ) = RKI(  84 ) * YC(  ALD2 ) * YC(  OH )
      RXRAT(   85 ) = RKI(  85 ) * YC(  ALD2 ) * YC(  NO3 )
      RXRAT(   86 ) = RKI(  86 ) * YC(  ALD2 )
      RXRAT(   87 ) = RKI(  87 ) * YC(  C2O3 ) * YC(  NO )
!     RXRAT(   88 ) = RKI(  88 ) * YC(  C2O3 ) * YC(  NO2 )
      RXRAT(   89 ) = RKI(  89 ) * YC(  PAN )
      RXRAT(   90 ) = RKI(  90 ) * YC(  PAN )
      RXRAT(   91 ) = RKI(  91 ) * YC(  C2O3 ) * YC(  HO2 )
      RXRAT(   92 ) = RKI(  92 ) * YC(  C2O3 ) * YC(  MEO2 )
      RXRAT(   93 ) = RKI(  93 ) * YC(  C2O3 ) * YC(  XO2 )
      RXRAT(   94 ) = RKI(  94 ) * YC(  C2O3 ) * YC(  C2O3 )
      RXRAT(   95 ) = RKI(  95 ) * YC(  PACD ) * YC(  OH )
      RXRAT(   96 ) = RKI(  96 ) * YC(  PACD )
      RXRAT(   97 ) = RKI(  97 ) * YC(  AACD ) * YC(  OH )
      RXRAT(   98 ) = RKI(  98 ) * YC(  ALDX ) * YC(  O )
      RXRAT(   99 ) = RKI(  99 ) * YC(  ALDX ) * YC(  OH )
      RXRAT(  100 ) = RKI( 100 ) * YC(  ALDX ) * YC(  NO3 )
      RXRAT(  101 ) = RKI( 101 ) * YC(  ALDX )
      RXRAT(  102 ) = RKI( 102 ) * YC(  CXO3 ) * YC(  NO )
      RXRAT(  103 ) = RKI( 103 ) * YC(  CXO3 ) * YC(  NO2 )
      RXRAT(  104 ) = RKI( 104 ) * YC(  PANX )
      RXRAT(  105 ) = RKI( 105 ) * YC(  PANX )
      RXRAT(  106 ) = RKI( 106 ) * YC(  PANX ) * YC(  OH )
      RXRAT(  107 ) = RKI( 107 ) * YC(  CXO3 ) * YC(  HO2 )
      RXRAT(  108 ) = RKI( 108 ) * YC(  CXO3 ) * YC(  MEO2 )
      RXRAT(  109 ) = RKI( 109 ) * YC(  CXO3 ) * YC(  XO2 )
      RXRAT(  110 ) = RKI( 110 ) * YC(  CXO3 ) * YC(  CXO3 )
      RXRAT(  111 ) = RKI( 111 ) * YC(  CXO3 ) * YC(  C2O3 )
      RXRAT(  112 ) = RKI( 112 ) * YC(  PAR ) * YC(  OH )
      RXRAT(  113 ) = RKI( 113 ) * YC(  ROR )
      RXRAT(  114 ) = RKI( 114 ) * YC(  ROR )
      RXRAT(  115 ) = RKI( 115 ) * YC(  ROR ) * YC(  NO2 )
      RXRAT(  116 ) = RKI( 116 ) * YC(  O ) * YC(  OLE )
      RXRAT(  117 ) = RKI( 117 ) * YC(  OH ) * YC(  OLE )
      RXRAT(  118 ) = RKI( 118 ) * YC(  O3 ) * YC(  OLE )
      RXRAT(  119 ) = RKI( 119 ) * YC(  NO3 ) * YC(  OLE )
      RXRAT(  120 ) = RKI( 120 ) * YC(  O ) * YC(  ETH )
      RXRAT(  121 ) = RKI( 121 ) * YC(  OH ) * YC(  ETH )
      RXRAT(  122 ) = RKI( 122 ) * YC(  O3 ) * YC(  ETH )
      RXRAT(  123 ) = RKI( 123 ) * YC(  NO3 ) * YC(  ETH )
      RXRAT(  124 ) = RKI( 124 ) * YC(  IOLE ) * YC(  O )
      RXRAT(  125 ) = RKI( 125 ) * YC(  IOLE ) * YC(  OH )
      RXRAT(  126 ) = RKI( 126 ) * YC(  IOLE ) * YC(  O3 )
      RXRAT(  127 ) = RKI( 127 ) * YC(  IOLE ) * YC(  NO3 )
      RXRAT(  128 ) = RKI( 128 ) * YC(  TOL ) * YC(  OH )
      RXRAT(  129 ) = RKI( 129 ) * YC(  TO2 ) * YC(  NO )
      RXRAT(  130 ) = RKI( 130 ) * YC(  TO2 ) * YC(  HO2 )
      RXRAT(  131 ) = RKI( 131 ) * YC(  OH ) * YC(  CRES )
      RXRAT(  132 ) = RKI( 132 ) * YC(  CRES ) * YC(  NO3 )
      RXRAT(  133 ) = RKI( 133 ) * YC(  CRO ) * YC(  NO2 )
      RXRAT(  134 ) = RKI( 134 ) * YC(  CRO ) * YC(  HO2 )
      RXRAT(  135 ) = RKI( 135 ) * YC(  CRON ) * YC(  OH )
      RXRAT(  136 ) = RKI( 136 ) * YC(  CRON ) * YC(  NO3 )
      RXRAT(  137 ) = RKI( 137 ) * YC(  CRNO ) * YC(  NO2 )
      RXRAT(  138 ) = RKI( 138 ) * YC(  CRNO ) * YC(  O3 )
      RXRAT(  139 ) = RKI( 139 ) * YC(  CRN2 ) * YC(  NO )
      RXRAT(  140 ) = RKI( 140 ) * YC(  CRN2 ) * YC(  HO2 )
      RXRAT(  141 ) = RKI( 141 ) * YC(  CRPX )
      RXRAT(  142 ) = RKI( 142 ) * YC(  CRPX ) * YC(  OH )
      RXRAT(  143 ) = RKI( 143 ) * YC(  OPEN )
      RXRAT(  144 ) = RKI( 144 ) * YC(  OPEN ) * YC(  OH )
      RXRAT(  145 ) = RKI( 145 ) * YC(  OPEN ) * YC(  O3 )
      RXRAT(  146 ) = RKI( 146 ) * YC(  OPEN ) * YC(  NO3 )
      RXRAT(  147 ) = RKI( 147 ) * YC(  CAT1 ) * YC(  OH )
      RXRAT(  148 ) = RKI( 148 ) * YC(  CAT1 ) * YC(  NO3 )
      RXRAT(  149 ) = RKI( 149 ) * YC(  CAO2 ) * YC(  NO )
      RXRAT(  150 ) = RKI( 150 ) * YC(  CAO2 ) * YC(  HO2 )
      RXRAT(  151 ) = RKI( 151 ) * YC(  OPO3 ) * YC(  NO )
      RXRAT(  152 ) = RKI( 152 ) * YC(  OPO3 ) * YC(  NO2 )
      RXRAT(  153 ) = RKI( 153 ) * YC(  OPAN )
      RXRAT(  154 ) = RKI( 154 ) * YC(  OH ) * YC(  XYLMN )
      RXRAT(  155 ) = RKI( 155 ) * YC(  OH ) * YC(  NAPH )
      RXRAT(  156 ) = RKI( 156 ) * YC(  OH ) * YC(  MGLY )
      RXRAT(  157 ) = RKI( 157 ) * YC(  MGLY )
      RXRAT(  158 ) = RKI( 158 ) * YC(  O ) * YC(  ISOP )
      RXRAT(  159 ) = RKI( 159 ) * YC(  OH ) * YC(  ISOP )
      RXRAT(  160 ) = RKI( 160 ) * YC(  O3 ) * YC(  ISOP )
      RXRAT(  161 ) = RKI( 161 ) * YC(  NO3 ) * YC(  ISOP )
      RXRAT(  162 ) = RKI( 162 ) * YC(  OH ) * YC(  ISPD )
      RXRAT(  163 ) = RKI( 163 ) * YC(  O3 ) * YC(  ISPD )
      RXRAT(  164 ) = RKI( 164 ) * YC(  NO3 ) * YC(  ISPD )
      RXRAT(  165 ) = RKI( 165 ) * YC(  ISPD )
      RXRAT(  166 ) = RKI( 166 ) * YC(  TERP ) * YC(  O )
      RXRAT(  167 ) = RKI( 167 ) * YC(  TERP ) * YC(  OH )
      RXRAT(  168 ) = RKI( 168 ) * YC(  TERP ) * YC(  O3 )
      RXRAT(  169 ) = RKI( 169 ) * YC(  TERP ) * YC(  NO3 )
      RXRAT(  170 ) = RKI( 170 ) * YC(  SO2 ) * YC(  OH )
      RXRAT(  171 ) = RKI( 171 ) * YC(  OH ) * YC(  ETOH )
      RXRAT(  172 ) = RKI( 172 ) * YC(  OH ) * YC(  ETHA )
      RXRAT(  173 ) = RKI( 173 ) * YC(  NO2 ) * YC(  ISOP )
      RXRAT(  174 ) = RKI( 174 ) * YC(  CL2 )
      RXRAT(  175 ) = RKI( 175 ) * YC(  HOCL )
      RXRAT(  176 ) = RKI( 176 ) * YC(  CL ) * YC(  O3 )
      RXRAT(  177 ) = RKI( 177 ) * YC(  CLO ) * YC(  CLO )
      RXRAT(  178 ) = RKI( 178 ) * YC(  CLO ) * YC(  NO )
      RXRAT(  179 ) = RKI( 179 ) * YC(  CLO ) * YC(  HO2 )
      RXRAT(  180 ) = RKI( 180 ) * YC(  OH ) * YC(  FMCL )
      RXRAT(  181 ) = RKI( 181 ) * YC(  FMCL )
      RXRAT(  182 ) = RKI( 182 ) * YC(  CL )
      RXRAT(  183 ) = RKI( 183 ) * YC(  CL ) * YC(  PAR )
      RXRAT(  184 ) = RKI( 184 ) * YC(  CL ) * YC(  ETHA )
      RXRAT(  185 ) = RKI( 185 ) * YC(  CL ) * YC(  ETH )
      RXRAT(  186 ) = RKI( 186 ) * YC(  CL ) * YC(  OLE )
      RXRAT(  187 ) = RKI( 187 ) * YC(  CL ) * YC(  IOLE )
      RXRAT(  188 ) = RKI( 188 ) * YC(  CL ) * YC(  ISOP )
      RXRAT(  189 ) = RKI( 189 ) * YC(  CL ) * YC(  FORM )
      RXRAT(  190 ) = RKI( 190 ) * YC(  CL ) * YC(  ALD2 )
      RXRAT(  191 ) = RKI( 191 ) * YC(  CL ) * YC(  ALDX )
      RXRAT(  192 ) = RKI( 192 ) * YC(  CL ) * YC(  MEOH )
      RXRAT(  193 ) = RKI( 193 ) * YC(  CL ) * YC(  ETOH )
      RXRAT(  194 ) = RKI( 194 ) * YC(  HCL ) * YC(  OH )
      RXRAT(  195 ) = RKI( 195 ) * YC(  CL ) * YC(  TOL )
      RXRAT(  196 ) = RKI( 196 ) * YC(  CL ) * YC(  XYLMN )
      RXRAT(  197 ) = RKI( 197 ) * YC(  CL ) * YC(  NO2 )
      RXRAT(  198 ) = RKI( 198 ) * YC(  CLNO2 )
      RXRAT(  199 ) = RKI( 199 ) * YC(  TOLRO2 ) * YC(  NO )
      RXRAT(  200 ) = RKI( 200 ) * YC(  TOLRO2 ) * YC(  HO2 )
      RXRAT(  201 ) = RKI( 201 ) * YC(  XYLRO2 ) * YC(  NO )
      RXRAT(  202 ) = RKI( 202 ) * YC(  XYLRO2 ) * YC(  HO2 )
      RXRAT(  203 ) = RKI( 203 ) * YC(  BENZENE ) * YC(  OH )
      RXRAT(  204 ) = RKI( 204 ) * YC(  BENZRO2 ) * YC(  NO )
      RXRAT(  205 ) = RKI( 205 ) * YC(  BENZRO2 ) * YC(  HO2 )
      RXRAT(  206 ) = RKI( 206 ) * YC(  SESQ ) * YC(  O3 )
      RXRAT(  207 ) = RKI( 207 ) * YC(  SESQ ) * YC(  OH )
      RXRAT(  208 ) = RKI( 208 ) * YC(  SESQ ) * YC(  NO3 )
      RXRAT(  209 ) = RKI( 209 ) * YC(  PAHRO2 ) * YC(  NO )
      RXRAT(  210 ) = RKI( 210 ) * YC(  PAHRO2 ) * YC(  HO2 )
      RXRAT(  211 ) = RKI( 211 ) * YC(  SOAALK ) * YC(  OH )
      RXRAT(  212 ) = RKI( 212 ) * YC(  N2O5 )
      RXRAT(  213 ) = RKI( 213 ) * YC(  N2O5 )
      RXRAT(  214 ) = RKI( 214 ) * YC(  H2NO3PIJ )
      RXRAT(  215 ) = RKI( 215 ) * YC(  H2NO3PK )
      RXRAT(  216 ) = RKI( 216 ) * YC(  H2NO3PIJ ) * YC(  ACLI )
      RXRAT(  217 ) = RKI( 217 ) * YC(  H2NO3PIJ ) * YC(  ACLJ )
      RXRAT(  218 ) = RKI( 218 ) * YC(  H2NO3PK ) * YC(  ACLK )
      RXRAT(  219 ) = RKI( 219 ) * YC(  NO2 )
!     RXRAT(  220 ) = RKI( 220 ) * YC(  O3 )
      RXRAT(  221 ) = RKI( 221 ) * YC(  AXYL1J )
      RXRAT(  222 ) = RKI( 222 ) * YC(  AXYL2J )
      RXRAT(  223 ) = RKI( 223 ) * YC(  ATOL1J )
      RXRAT(  224 ) = RKI( 224 ) * YC(  ATOL2J )
      RXRAT(  225 ) = RKI( 225 ) * YC(  ABNZ1J )
      RXRAT(  226 ) = RKI( 226 ) * YC(  ABNZ2J )
      RXRAT(  227 ) = RKI( 227 ) * YC(  ATRP1J )
      RXRAT(  228 ) = RKI( 228 ) * YC(  ATRP2J )
      RXRAT(  229 ) = RKI( 229 ) * YC(  AISO1J )
      RXRAT(  230 ) = RKI( 230 ) * YC(  AISO2J )
      RXRAT(  231 ) = RKI( 231 ) * YC(  ASQTJ )
      RXRAT(  232 ) = RKI( 232 ) * YC(  APAH1J )
      RXRAT(  233 ) = RKI( 233 ) * YC(  APAH2J )
      RXRAT(  234 ) = RKI( 234 ) * YC(  AALK1J )
      RXRAT(  235 ) = RKI( 235 ) * YC(  AALK2J )
      RXRAT(  236 ) = RKI( 236 ) * YC(  PCVOC ) * YC(  OH )
      RXRAT(  237 ) = RKI( 237 ) * YC(  VLVPO1 ) * YC(  OH )
      RXRAT(  238 ) = RKI( 238 ) * YC(  VSVPO1 ) * YC(  OH )
      RXRAT(  239 ) = RKI( 239 ) * YC(  VSVPO2 ) * YC(  OH )
      RXRAT(  240 ) = RKI( 240 ) * YC(  VSVPO3 ) * YC(  OH )
      RXRAT(  241 ) = RKI( 241 ) * YC(  VIVPO1 ) * YC(  OH )
      RXRAT(  242 ) = RKI( 242 ) * YC(  VLVOO1 ) * YC(  OH )
      RXRAT(  243 ) = RKI( 243 ) * YC(  VLVOO2 ) * YC(  OH )
      RXRAT(  244 ) = RKI( 244 ) * YC(  VSVOO1 ) * YC(  OH )
      RXRAT(  245 ) = RKI( 245 ) * YC(  VSVOO2 ) * YC(  OH )
      RXRAT(  246 ) = RKI( 246 ) * YC(  VSVOO3 ) * YC(  OH )


      RETURN

      END
