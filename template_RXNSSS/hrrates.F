
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
R1  PRECONDITIONS: For SAPRC99 family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
R2  REVISION HISTORY: Prototype created by Jerry Gipson, September, 2003
C
C***********************************************************************
      USE HRDATA

      IMPLICIT NONE

C.....INCLUDES: NONE


C.....ARGUMENTS: NONE 


C.....PARAMETERS: NONE


C.....EXTERNAL FUNCTIONS: NONE


C.....SAVED VARIABLES: NONE
      CHARACTER( 16 ), SAVE  ::  PNAME = 'HRRATES'   ! Program name


C.....LOCAL VARIABLES: None

C***********************************************************************
S1

C..NOTE: RXRAT commented out are not referenced by any routine

C..Photolytic reactions
!      RXRAT(   1 ) = RKI( NCELL,    1 )  *  YC( NO2 )
      RXRAT(  15 ) = RKI( NCELL,   15 )  *  YC( NO3 )
      RXRAT(  16 ) = RKI( NCELL,   16 )  *  YC( NO3 )
      RXRAT(  17 ) = RKI( NCELL,   17 )  *  YC( O3 )
      RXRAT(  18 ) = RKI( NCELL,   18 )  *  YC( O3 )
      RXRAT(  22 ) = RKI( NCELL,   22 )  *  YC( HONO )
      RXRAT(  23 ) = RKI( NCELL,   23 )  *  YC( HONO )
      RXRAT(  28 ) = RKI( NCELL,   28 )  *  YC( HNO3 )
      RXRAT(  34 ) = RKI( NCELL,   34 )  *  YC( HNO4 )
      RXRAT(  41 ) = RKI( NCELL,   41 )  *  YC( HO2H )
      RXRAT( 123 ) = RKI( NCELL,  123 )  *  YC( HCHO )
      RXRAT( 124 ) = RKI( NCELL,  124 )  *  YC( HCHO )
      RXRAT( 131 ) = RKI( NCELL,  131 )  *  YC( CCHO )
      RXRAT( 134 ) = RKI( NCELL,  134 )  *  YC( RCHO )
      RXRAT( 137 ) = RKI( NCELL,  137 )  *  YC( ACET )
      RXRAT( 139 ) = RKI( NCELL,  139 )  *  YC( MEK )
      RXRAT( 142 ) = RKI( NCELL,  142 )  *  YC( COOH )
      RXRAT( 144 ) = RKI( NCELL,  144 )  *  YC( ROOH )
      RXRAT( 145 ) = RKI( NCELL,  145 )  *  YC( GLY )
      RXRAT( 146 ) = RKI( NCELL,  146 )  *  YC( GLY )
      RXRAT( 149 ) = RKI( NCELL,  149 )  *  YC( MGLY )
      RXRAT( 152 ) = RKI( NCELL,  152 )  *  YC( BACL )
      RXRAT( 159 ) = RKI( NCELL,  159 )  *  YC( BALD )
      RXRAT( 165 ) = RKI( NCELL,  165 )  *  YC( METHACRO )
      RXRAT( 169 ) = RKI( NCELL,  169 )  *  YC( MVK )
      RXRAT( 173 ) = RKI( NCELL,  173 )  *  YC( ISOPROD )
      RXRAT( 175 ) = RKI( NCELL,  175 )  *  YC( PROD2 )
      RXRAT( 177 ) = RKI( NCELL,  177 )  *  YC( RNO3 )
      RXRAT( 181 ) = RKI( NCELL,  181 )  *  YC( DCB2 )
      RXRAT( 183 ) = RKI( NCELL,  183 )  *  YC( DCB3 )
 
 
C..Thermal reactions
!      RXRAT(   2 ) = RKI( NCELL,    2 ) * YC( O3P )
!      RXRAT(   3 ) = RKI( NCELL,    3 ) * YC( O3P )      * YC( O3 )
!      RXRAT(   4 ) = RKI( NCELL,    4 ) * YC( O3P )      * YC( NO )
!      RXRAT(   5 ) = RKI( NCELL,    5 ) * YC( O3P )      * YC( NO2 )
      RXRAT(   6 ) = RKI( NCELL,    6 ) * YC( O3P )      * YC( NO2 )
!      RXRAT(   7 ) = RKI( NCELL,    7 ) * YC( O3 )       * YC( NO )
      RXRAT(   8 ) = RKI( NCELL,    8 ) * YC( O3 )       * YC( NO2 )
      RXRAT(   9 ) = RKI( NCELL,    9 ) * YC( NO )       * YC( NO3 )
!      RXRAT(  10 ) = RKI( NCELL,   10 ) * YC( NO )       * YC( NO )
!      RXRAT(  11 ) = RKI( NCELL,   11 ) * YC( NO2 )      * YC( NO3 )
      RXRAT(  12 ) = RKI( NCELL,   12 ) * YC( N2O5 )
      RXRAT(  13 ) = RKI( NCELL,   13 ) * YC( N2O5 )
      RXRAT(  14 ) = RKI( NCELL,   14 ) * YC( NO2 )      * YC( NO3 )
!      RXRAT(  19 ) = RKI( NCELL,   19 ) * YC( O1D2 )
!      RXRAT(  20 ) = RKI( NCELL,   20 ) * YC( O1D2 )
!      RXRAT(  21 ) = RKI( NCELL,   21 ) * YC( HO )       * YC( NO )
      RXRAT(  24 ) = RKI( NCELL,   24 ) * YC( HO )       * YC( HONO )
      RXRAT(  25 ) = RKI( NCELL,   25 ) * YC( HO )       * YC( NO2 )
      RXRAT(  26 ) = RKI( NCELL,   26 ) * YC( HO )       * YC( NO3 )
      RXRAT(  27 ) = RKI( NCELL,   27 ) * YC( HO )       * YC( HNO3 )
      RXRAT(  29 ) = RKI( NCELL,   29 ) * YC( HO )       * YC( CO )
!      RXRAT(  30 ) = RKI( NCELL,   30 ) * YC( HO )       * YC( O3 )
!      RXRAT(  31 ) = RKI( NCELL,   31 ) * YC( HO2 )      * YC( NO )
!      RXRAT(  32 ) = RKI( NCELL,   32 ) * YC( HO2 )      * YC( NO2 )
      RXRAT(  33 ) = RKI( NCELL,   33 ) * YC( HNO4 )
      RXRAT(  35 ) = RKI( NCELL,   35 ) * YC( HNO4 )     * YC( HO )
!      RXRAT(  36 ) = RKI( NCELL,   36 ) * YC( HO2 )      * YC( O3 )
      RXRAT(  37 ) = RKI( NCELL,   37 ) * YC( HO2 )      * YC( HO2 )
      RXRAT(  38 ) = RKI( NCELL,   38 ) * YC( HO2 )      * YC( HO2 )
      RXRAT(  39 ) = RKI( NCELL,   39 ) * YC( NO3 )      * YC( HO2 )
      RXRAT(  40 ) = RKI( NCELL,   40 ) * YC( NO3 )      * YC( NO3 )
      RXRAT(  42 ) = RKI( NCELL,   42 ) * YC( HO2H )     * YC( HO )
!      RXRAT(  43 ) = RKI( NCELL,   43 ) * YC( HO )       * YC( HO2 )
      RXRAT(  44 ) = RKI( NCELL,   44 ) * YC( HO )       * YC( SO2 )
!      RXRAT(  45 ) = RKI( NCELL,   45 ) * YC( HO )
      RXRAT(  46 ) = RKI( NCELL,   46 ) * YC( C_O2 )     * YC( NO )
      RXRAT(  47 ) = RKI( NCELL,   47 ) * YC( C_O2 )     * YC( HO2 )
      RXRAT(  48 ) = RKI( NCELL,   48 ) * YC( C_O2 )     * YC( NO3 )
      RXRAT(  49 ) = RKI( NCELL,   49 ) * YC( C_O2 )     * YC( C_O2 )
      RXRAT(  50 ) = RKI( NCELL,   50 ) * YC( C_O2 )     * YC( C_O2 )
      RXRAT(  51 ) = RKI( NCELL,   51 ) * YC( RO2_R )    * YC( NO )
      RXRAT(  52 ) = RKI( NCELL,   52 ) * YC( RO2_R )    * YC( HO2 )
      RXRAT(  53 ) = RKI( NCELL,   53 ) * YC( RO2_R )    * YC( NO3 )
      RXRAT(  54 ) = RKI( NCELL,   54 ) * YC( RO2_R )    * YC( C_O2 )
      RXRAT(  55 ) = RKI( NCELL,   55 ) * YC( RO2_R )    * YC( RO2_R )
      RXRAT(  56 ) = RKI( NCELL,   56 ) * YC( R2O2 )     * YC( NO )
      RXRAT(  57 ) = RKI( NCELL,   57 ) * YC( R2O2 )     * YC( HO2 )
      RXRAT(  58 ) = RKI( NCELL,   58 ) * YC( R2O2 )     * YC( NO3 )
      RXRAT(  59 ) = RKI( NCELL,   59 ) * YC( R2O2 )     * YC( C_O2 )
      RXRAT(  60 ) = RKI( NCELL,   60 ) * YC( R2O2 )     * YC( RO2_R )
      RXRAT(  61 ) = RKI( NCELL,   61 ) * YC( R2O2 )     * YC( R2O2 )
      RXRAT(  62 ) = RKI( NCELL,   62 ) * YC( RO2_N )    * YC( NO )
      RXRAT(  63 ) = RKI( NCELL,   63 ) * YC( RO2_N )    * YC( HO2 )
      RXRAT(  64 ) = RKI( NCELL,   64 ) * YC( RO2_N )    * YC( C_O2 )
      RXRAT(  65 ) = RKI( NCELL,   65 ) * YC( RO2_N )    * YC( NO3 )
      RXRAT(  66 ) = RKI( NCELL,   66 ) * YC( RO2_N )    * YC( RO2_R )
      RXRAT(  67 ) = RKI( NCELL,   67 ) * YC( RO2_N )    * YC( R2O2 )
      RXRAT(  68 ) = RKI( NCELL,   68 ) * YC( RO2_N )    * YC( RO2_N )
!      RXRAT(  69 ) = RKI( NCELL,   69 ) * YC( CCO_O2 )   * YC( NO2 )
      RXRAT(  70 ) = RKI( NCELL,   70 ) * YC( PAN )
      RXRAT(  71 ) = RKI( NCELL,   71 ) * YC( CCO_O2 )   * YC( NO )
      RXRAT(  72 ) = RKI( NCELL,   72 ) * YC( CCO_O2 )   * YC( HO2 )
      RXRAT(  73 ) = RKI( NCELL,   73 ) * YC( CCO_O2 )   * YC( NO3 )
      RXRAT(  74 ) = RKI( NCELL,   74 ) * YC( CCO_O2 )   * YC( C_O2 )
      RXRAT(  75 ) = RKI( NCELL,   75 ) * YC( CCO_O2 )   * YC( RO2_R )
      RXRAT(  76 ) = RKI( NCELL,   76 ) * YC( CCO_O2 )   * YC( R2O2 )
      RXRAT(  77 ) = RKI( NCELL,   77 ) * YC( CCO_O2 )   * YC( RO2_N )
      RXRAT(  78 ) = RKI( NCELL,   78 ) * YC( CCO_O2 )   * YC( CCO_O2 )
      RXRAT(  79 ) = RKI( NCELL,   79 ) * YC( RCO_O2 )   * YC( NO2 )
      RXRAT(  80 ) = RKI( NCELL,   80 ) * YC( PAN2 )
      RXRAT(  81 ) = RKI( NCELL,   81 ) * YC( RCO_O2 )   * YC( NO )
      RXRAT(  82 ) = RKI( NCELL,   82 ) * YC( RCO_O2 )   * YC( HO2 )
      RXRAT(  83 ) = RKI( NCELL,   83 ) * YC( RCO_O2 )   * YC( NO3 )
      RXRAT(  84 ) = RKI( NCELL,   84 ) * YC( RCO_O2 )   * YC( C_O2 )
      RXRAT(  85 ) = RKI( NCELL,   85 ) * YC( RCO_O2 )   * YC( RO2_R )
      RXRAT(  86 ) = RKI( NCELL,   86 ) * YC( RCO_O2 )   * YC( R2O2 )
      RXRAT(  87 ) = RKI( NCELL,   87 ) * YC( RCO_O2 )   * YC( RO2_N )
      RXRAT(  88 ) = RKI( NCELL,   88 ) * YC( RCO_O2 )   * YC( CCO_O2 )
      RXRAT(  89 ) = RKI( NCELL,   89 ) * YC( RCO_O2 )   * YC( RCO_O2 )
      RXRAT(  90 ) = RKI( NCELL,   90 ) * YC( BZCO_O2 )  * YC( NO2 )
      RXRAT(  91 ) = RKI( NCELL,   91 ) * YC( PBZN )
      RXRAT(  92 ) = RKI( NCELL,   92 ) * YC( BZCO_O2 )  * YC( NO )
      RXRAT(  93 ) = RKI( NCELL,   93 ) * YC( BZCO_O2 )  * YC( HO2 )
      RXRAT(  94 ) = RKI( NCELL,   94 ) * YC( BZCO_O2 )  * YC( NO3 )
      RXRAT(  95 ) = RKI( NCELL,   95 ) * YC( BZCO_O2 )  * YC( C_O2 )
      RXRAT(  96 ) = RKI( NCELL,   96 ) * YC( BZCO_O2 )  * YC( RO2_R )
      RXRAT(  97 ) = RKI( NCELL,   97 ) * YC( BZCO_O2 )  * YC( R2O2 )
      RXRAT(  98 ) = RKI( NCELL,   98 ) * YC( BZCO_O2 )  * YC( RO2_N )
      RXRAT(  99 ) = RKI( NCELL,   99 ) * YC( BZCO_O2 )  * YC( CCO_O2 )
      RXRAT( 100 ) = RKI( NCELL,  100 ) * YC( BZCO_O2 )  * YC( RCO_O2 )
      RXRAT( 101 ) = RKI( NCELL,  101 ) * YC( BZCO_O2 )  * YC( BZCO_O2 )
      RXRAT( 102 ) = RKI( NCELL,  102 ) * YC( MA_RCO3 )  * YC( NO2 )
      RXRAT( 103 ) = RKI( NCELL,  103 ) * YC( MA_PAN )
      RXRAT( 104 ) = RKI( NCELL,  104 ) * YC( MA_RCO3 )  * YC( NO )
      RXRAT( 105 ) = RKI( NCELL,  105 ) * YC( MA_RCO3 )  * YC( HO2 )
      RXRAT( 106 ) = RKI( NCELL,  106 ) * YC( MA_RCO3 )  * YC( NO3 )
      RXRAT( 107 ) = RKI( NCELL,  107 ) * YC( MA_RCO3 )  * YC( C_O2 )
      RXRAT( 108 ) = RKI( NCELL,  108 ) * YC( MA_RCO3 )  * YC( RO2_R )
      RXRAT( 109 ) = RKI( NCELL,  109 ) * YC( MA_RCO3 )  * YC( R2O2 )
      RXRAT( 110 ) = RKI( NCELL,  110 ) * YC( MA_RCO3 )  * YC( RO2_N )
      RXRAT( 111 ) = RKI( NCELL,  111 ) * YC( MA_RCO3 )  * YC( CCO_O2 )
      RXRAT( 112 ) = RKI( NCELL,  112 ) * YC( MA_RCO3 )  * YC( RCO_O2 )
      RXRAT( 113 ) = RKI( NCELL,  113 ) * YC( MA_RCO3 )  * YC( BZCO_O2 )
      RXRAT( 114 ) = RKI( NCELL,  114 ) * YC( MA_RCO3 )  * YC( MA_RCO3 )
      RXRAT( 115 ) = RKI( NCELL,  115 ) * YC( TBU_O )    * YC( NO2 )
      RXRAT( 116 ) = RKI( NCELL,  116 ) * YC( TBU_O )
      RXRAT( 117 ) = RKI( NCELL,  117 ) * YC( BZ_O )     * YC( NO2 )
      RXRAT( 118 ) = RKI( NCELL,  118 ) * YC( BZ_O )     * YC( HO2 )
      RXRAT( 119 ) = RKI( NCELL,  119 ) * YC( BZ_O )
      RXRAT( 120 ) = RKI( NCELL,  120 ) * YC( BZNO2_O )  * YC( NO2 )
      RXRAT( 121 ) = RKI( NCELL,  121 ) * YC( BZNO2_O )  * YC( HO2 )
      RXRAT( 122 ) = RKI( NCELL,  122 ) * YC( BZNO2_O )
      RXRAT( 125 ) = RKI( NCELL,  125 ) * YC( HCHO )     * YC( HO )
      RXRAT( 126 ) = RKI( NCELL,  126 ) * YC( HCHO )     * YC( HO2 )
      RXRAT( 127 ) = RKI( NCELL,  127 ) * YC( HOCOO )
      RXRAT( 128 ) = RKI( NCELL,  128 ) * YC( HOCOO )    * YC( NO )
      RXRAT( 129 ) = RKI( NCELL,  129 ) * YC( HCHO )     * YC( NO3 )
      RXRAT( 130 ) = RKI( NCELL,  130 ) * YC( CCHO )     * YC( HO )
      RXRAT( 132 ) = RKI( NCELL,  132 ) * YC( CCHO )     * YC( NO3 )
      RXRAT( 133 ) = RKI( NCELL,  133 ) * YC( RCHO )     * YC( HO )
      RXRAT( 135 ) = RKI( NCELL,  135 ) * YC( RCHO )     * YC( NO3 )
      RXRAT( 136 ) = RKI( NCELL,  136 ) * YC( ACET )     * YC( HO )
      RXRAT( 138 ) = RKI( NCELL,  138 ) * YC( MEK )      * YC( HO )
      RXRAT( 140 ) = RKI( NCELL,  140 ) * YC( MEOH )     * YC( HO )
      RXRAT( 141 ) = RKI( NCELL,  141 ) * YC( COOH )     * YC( HO )
      RXRAT( 143 ) = RKI( NCELL,  143 ) * YC( ROOH )     * YC( HO )
      RXRAT( 147 ) = RKI( NCELL,  147 ) * YC( GLY )      * YC( HO )
      RXRAT( 148 ) = RKI( NCELL,  148 ) * YC( GLY )      * YC( NO3 )
      RXRAT( 150 ) = RKI( NCELL,  150 ) * YC( MGLY )     * YC( HO )
      RXRAT( 151 ) = RKI( NCELL,  151 ) * YC( MGLY )     * YC( NO3 )
      RXRAT( 153 ) = RKI( NCELL,  153 ) * YC( PHEN )     * YC( HO )
      RXRAT( 154 ) = RKI( NCELL,  154 ) * YC( PHEN )     * YC( NO3 )
      RXRAT( 155 ) = RKI( NCELL,  155 ) * YC( CRES )     * YC( HO )
      RXRAT( 156 ) = RKI( NCELL,  156 ) * YC( CRES )     * YC( NO3 )
      RXRAT( 157 ) = RKI( NCELL,  157 ) * YC( NPHE )     * YC( NO3 )
      RXRAT( 158 ) = RKI( NCELL,  158 ) * YC( BALD )     * YC( HO )
      RXRAT( 160 ) = RKI( NCELL,  160 ) * YC( BALD )     * YC( NO3 )
      RXRAT( 161 ) = RKI( NCELL,  161 ) * YC( METHACRO ) * YC( HO )
      RXRAT( 162 ) = RKI( NCELL,  162 ) * YC( METHACRO ) * YC( O3 )
      RXRAT( 163 ) = RKI( NCELL,  163 ) * YC( METHACRO ) * YC( NO3 )
      RXRAT( 164 ) = RKI( NCELL,  164 ) * YC( METHACRO ) * YC( O3P )
      RXRAT( 166 ) = RKI( NCELL,  166 ) * YC( MVK )      * YC( HO )
      RXRAT( 167 ) = RKI( NCELL,  167 ) * YC( MVK )      * YC( O3 )
      RXRAT( 168 ) = RKI( NCELL,  168 ) * YC( MVK )      * YC( O3P )
      RXRAT( 170 ) = RKI( NCELL,  170 ) * YC( ISOPROD )  * YC( HO )
      RXRAT( 171 ) = RKI( NCELL,  171 ) * YC( ISOPROD )  * YC( O3 )
      RXRAT( 172 ) = RKI( NCELL,  172 ) * YC( ISOPROD )  * YC( NO3 )
      RXRAT( 174 ) = RKI( NCELL,  174 ) * YC( PROD2 )    * YC( HO )
      RXRAT( 176 ) = RKI( NCELL,  176 ) * YC( RNO3 )     * YC( HO )
      RXRAT( 178 ) = RKI( NCELL,  178 ) * YC( DCB1 )     * YC( HO )
      RXRAT( 179 ) = RKI( NCELL,  179 ) * YC( DCB1 )     * YC( O3 )
      RXRAT( 180 ) = RKI( NCELL,  180 ) * YC( DCB2 )     * YC( HO )
      RXRAT( 182 ) = RKI( NCELL,  182 ) * YC( DCB3 )     * YC( HO )
      RXRAT( 184 ) = RKI( NCELL,  184 ) * YC( HO )
      RXRAT( 185 ) = RKI( NCELL,  185 ) * YC( ETHENE )   * YC( HO )
      RXRAT( 186 ) = RKI( NCELL,  186 ) * YC( ETHENE )   * YC( O3 )
      RXRAT( 187 ) = RKI( NCELL,  187 ) * YC( ETHENE )   * YC( NO3 )
      RXRAT( 188 ) = RKI( NCELL,  188 ) * YC( ETHENE )   * YC( O3P )
      RXRAT( 189 ) = RKI( NCELL,  189 ) * YC( ISOPRENE ) * YC( HO )
      RXRAT( 190 ) = RKI( NCELL,  190 ) * YC( ISOPRENE ) * YC( O3 )
      RXRAT( 191 ) = RKI( NCELL,  191 ) * YC( ISOPRENE ) * YC( NO3 )
      RXRAT( 192 ) = RKI( NCELL,  192 ) * YC( ISOPRENE ) * YC( O3P )
      RXRAT( 193 ) = RKI( NCELL,  193 ) * YC( TRP1 )     * YC( HO )
      RXRAT( 194 ) = RKI( NCELL,  194 ) * YC( TRP1 )     * YC( O3 )
      RXRAT( 195 ) = RKI( NCELL,  195 ) * YC( TRP1 )     * YC( NO3 )
      RXRAT( 196 ) = RKI( NCELL,  196 ) * YC( TRP1 )     * YC( O3P )
      RXRAT( 197 ) = RKI( NCELL,  197 ) * YC( ALK1 )     * YC( HO )
      RXRAT( 198 ) = RKI( NCELL,  198 ) * YC( ALK2 )     * YC( HO )
      RXRAT( 199 ) = RKI( NCELL,  199 ) * YC( ALK3 )     * YC( HO )
      RXRAT( 200 ) = RKI( NCELL,  200 ) * YC( ALK4 )     * YC( HO )
      RXRAT( 201 ) = RKI( NCELL,  201 ) * YC( ALK5 )     * YC( HO )
      RXRAT( 202 ) = RKI( NCELL,  202 ) * YC( ARO1 )     * YC( HO )
      RXRAT( 203 ) = RKI( NCELL,  203 ) * YC( ARO2 )     * YC( HO )
      RXRAT( 204 ) = RKI( NCELL,  204 ) * YC( OLE1 )     * YC( HO )
      RXRAT( 205 ) = RKI( NCELL,  205 ) * YC( OLE1 )     * YC( O3 )
      RXRAT( 206 ) = RKI( NCELL,  206 ) * YC( OLE1 )     * YC( NO3 )
      RXRAT( 207 ) = RKI( NCELL,  207 ) * YC( OLE1 )     * YC( O3P )
      RXRAT( 208 ) = RKI( NCELL,  208 ) * YC( OLE2 )     * YC( HO )
      RXRAT( 209 ) = RKI( NCELL,  209 ) * YC( OLE2 )     * YC( O3 )
      RXRAT( 210 ) = RKI( NCELL,  210 ) * YC( OLE2 )     * YC( NO3 )
      RXRAT( 211 ) = RKI( NCELL,  211 ) * YC( OLE2 )     * YC( O3P )
      RXRAT( 212 ) = RKI( NCELL,  212 ) * YC( HCOOH )    * YC( HO )
      RXRAT( 213 ) = RKI( NCELL,  213 ) * YC( CCO_OH )   * YC( HO )
      RXRAT( 214 ) = RKI( NCELL,  214 ) * YC( RCO_OH )   * YC( HO )


      RETURN

      END

S1
