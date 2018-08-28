
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
!     CHARACTER( 16 ), SAVE  ::  PNAME = 'HRRATES'   ! Program name


C.....LOCAL VARIABLES: None

C***********************************************************************
S1

C..NOTE: RXRAT commented out are not referenced by any routine

C..Photolytic reactions
!      RXRAT(   1 ) = RKI(    1 )  *  YC( NO2 )
      RXRAT(  15 ) = RKI(   15 )  *  YC( NO3 )
      RXRAT(  16 ) = RKI(   16 )  *  YC( NO3 )
      RXRAT(  17 ) = RKI(   17 )  *  YC( O3 )
      RXRAT(  18 ) = RKI(   18 )  *  YC( O3 )
      RXRAT(  22 ) = RKI(   22 )  *  YC( HONO )
      RXRAT(  23 ) = RKI(   23 )  *  YC( HONO )
      RXRAT(  28 ) = RKI(   28 )  *  YC( HNO3 )
      RXRAT(  34 ) = RKI(   34 )  *  YC( HNO4 )
      RXRAT(  41 ) = RKI(   41 )  *  YC( HO2H )
      RXRAT( 123 ) = RKI(  123 )  *  YC( HCHO )
      RXRAT( 124 ) = RKI(  124 )  *  YC( HCHO )
      RXRAT( 131 ) = RKI(  131 )  *  YC( CCHO )
      RXRAT( 134 ) = RKI(  134 )  *  YC( RCHO )
      RXRAT( 137 ) = RKI(  137 )  *  YC( ACET )
      RXRAT( 139 ) = RKI(  139 )  *  YC( MEK )
      RXRAT( 142 ) = RKI(  142 )  *  YC( COOH )
      RXRAT( 144 ) = RKI(  144 )  *  YC( ROOH )
      RXRAT( 145 ) = RKI(  145 )  *  YC( GLY )
      RXRAT( 146 ) = RKI(  146 )  *  YC( GLY )
      RXRAT( 149 ) = RKI(  149 )  *  YC( MGLY )
      RXRAT( 152 ) = RKI(  152 )  *  YC( BACL )
      RXRAT( 159 ) = RKI(  159 )  *  YC( BALD )
      RXRAT( 165 ) = RKI(  165 )  *  YC( METHACRO )
      RXRAT( 169 ) = RKI(  169 )  *  YC( MVK )
      RXRAT( 173 ) = RKI(  173 )  *  YC( ISOPROD )
      RXRAT( 175 ) = RKI(  175 )  *  YC( PROD2 )
      RXRAT( 177 ) = RKI(  177 )  *  YC( RNO3 )
      RXRAT( 181 ) = RKI(  181 )  *  YC( DCB2 )
      RXRAT( 183 ) = RKI(  183 )  *  YC( DCB3 )
 
 
C..Thermal reactions
!      RXRAT(   2 ) = RKI(    2 ) * YC( O3P )
!      RXRAT(   3 ) = RKI(    3 ) * YC( O3P )      * YC( O3 )
!      RXRAT(   4 ) = RKI(    4 ) * YC( O3P )      * YC( NO )
!      RXRAT(   5 ) = RKI(    5 ) * YC( O3P )      * YC( NO2 )
      RXRAT(   6 ) = RKI(    6 ) * YC( O3P )      * YC( NO2 )
!      RXRAT(   7 ) = RKI(    7 ) * YC( O3 )       * YC( NO )
      RXRAT(   8 ) = RKI(    8 ) * YC( O3 )       * YC( NO2 )
      RXRAT(   9 ) = RKI(    9 ) * YC( NO )       * YC( NO3 )
!      RXRAT(  10 ) = RKI(   10 ) * YC( NO )       * YC( NO )
!      RXRAT(  11 ) = RKI(   11 ) * YC( NO2 )      * YC( NO3 )
      RXRAT(  12 ) = RKI(   12 ) * YC( N2O5 )
      RXRAT(  13 ) = RKI(   13 ) * YC( N2O5 )
      RXRAT(  14 ) = RKI(   14 ) * YC( NO2 )      * YC( NO3 )
!      RXRAT(  19 ) = RKI(   19 ) * YC( O1D2 )
!      RXRAT(  20 ) = RKI(   20 ) * YC( O1D2 )
!      RXRAT(  21 ) = RKI(   21 ) * YC( HO )       * YC( NO )
      RXRAT(  24 ) = RKI(   24 ) * YC( HO )       * YC( HONO )
      RXRAT(  25 ) = RKI(   25 ) * YC( HO )       * YC( NO2 )
      RXRAT(  26 ) = RKI(   26 ) * YC( HO )       * YC( NO3 )
      RXRAT(  27 ) = RKI(   27 ) * YC( HO )       * YC( HNO3 )
      RXRAT(  29 ) = RKI(   29 ) * YC( HO )       * YC( CO )
!      RXRAT(  30 ) = RKI(   30 ) * YC( HO )       * YC( O3 )
!      RXRAT(  31 ) = RKI(   31 ) * YC( HO2 )      * YC( NO )
!      RXRAT(  32 ) = RKI(   32 ) * YC( HO2 )      * YC( NO2 )
      RXRAT(  33 ) = RKI(   33 ) * YC( HNO4 )
      RXRAT(  35 ) = RKI(   35 ) * YC( HNO4 )     * YC( HO )
!      RXRAT(  36 ) = RKI(   36 ) * YC( HO2 )      * YC( O3 )
      RXRAT(  37 ) = RKI(   37 ) * YC( HO2 )      * YC( HO2 )
      RXRAT(  38 ) = RKI(   38 ) * YC( HO2 )      * YC( HO2 )
      RXRAT(  39 ) = RKI(   39 ) * YC( NO3 )      * YC( HO2 )
      RXRAT(  40 ) = RKI(   40 ) * YC( NO3 )      * YC( NO3 )
      RXRAT(  42 ) = RKI(   42 ) * YC( HO2H )     * YC( HO )
!      RXRAT(  43 ) = RKI(   43 ) * YC( HO )       * YC( HO2 )
      RXRAT(  44 ) = RKI(   44 ) * YC( HO )       * YC( SO2 )
!      RXRAT(  45 ) = RKI(   45 ) * YC( HO )
      RXRAT(  46 ) = RKI(   46 ) * YC( C_O2 )     * YC( NO )
      RXRAT(  47 ) = RKI(   47 ) * YC( C_O2 )     * YC( HO2 )
      RXRAT(  48 ) = RKI(   48 ) * YC( C_O2 )     * YC( NO3 )
      RXRAT(  49 ) = RKI(   49 ) * YC( C_O2 )     * YC( C_O2 )
      RXRAT(  50 ) = RKI(   50 ) * YC( C_O2 )     * YC( C_O2 )
      RXRAT(  51 ) = RKI(   51 ) * YC( RO2_R )    * YC( NO )
      RXRAT(  52 ) = RKI(   52 ) * YC( RO2_R )    * YC( HO2 )
      RXRAT(  53 ) = RKI(   53 ) * YC( RO2_R )    * YC( NO3 )
      RXRAT(  54 ) = RKI(   54 ) * YC( RO2_R )    * YC( C_O2 )
      RXRAT(  55 ) = RKI(   55 ) * YC( RO2_R )    * YC( RO2_R )
      RXRAT(  56 ) = RKI(   56 ) * YC( R2O2 )     * YC( NO )
      RXRAT(  57 ) = RKI(   57 ) * YC( R2O2 )     * YC( HO2 )
      RXRAT(  58 ) = RKI(   58 ) * YC( R2O2 )     * YC( NO3 )
      RXRAT(  59 ) = RKI(   59 ) * YC( R2O2 )     * YC( C_O2 )
      RXRAT(  60 ) = RKI(   60 ) * YC( R2O2 )     * YC( RO2_R )
      RXRAT(  61 ) = RKI(   61 ) * YC( R2O2 )     * YC( R2O2 )
      RXRAT(  62 ) = RKI(   62 ) * YC( RO2_N )    * YC( NO )
      RXRAT(  63 ) = RKI(   63 ) * YC( RO2_N )    * YC( HO2 )
      RXRAT(  64 ) = RKI(   64 ) * YC( RO2_N )    * YC( C_O2 )
      RXRAT(  65 ) = RKI(   65 ) * YC( RO2_N )    * YC( NO3 )
      RXRAT(  66 ) = RKI(   66 ) * YC( RO2_N )    * YC( RO2_R )
      RXRAT(  67 ) = RKI(   67 ) * YC( RO2_N )    * YC( R2O2 )
      RXRAT(  68 ) = RKI(   68 ) * YC( RO2_N )    * YC( RO2_N )
!      RXRAT(  69 ) = RKI(   69 ) * YC( CCO_O2 )   * YC( NO2 )
      RXRAT(  70 ) = RKI(   70 ) * YC( PAN )
      RXRAT(  71 ) = RKI(   71 ) * YC( CCO_O2 )   * YC( NO )
      RXRAT(  72 ) = RKI(   72 ) * YC( CCO_O2 )   * YC( HO2 )
      RXRAT(  73 ) = RKI(   73 ) * YC( CCO_O2 )   * YC( NO3 )
      RXRAT(  74 ) = RKI(   74 ) * YC( CCO_O2 )   * YC( C_O2 )
      RXRAT(  75 ) = RKI(   75 ) * YC( CCO_O2 )   * YC( RO2_R )
      RXRAT(  76 ) = RKI(   76 ) * YC( CCO_O2 )   * YC( R2O2 )
      RXRAT(  77 ) = RKI(   77 ) * YC( CCO_O2 )   * YC( RO2_N )
      RXRAT(  78 ) = RKI(   78 ) * YC( CCO_O2 )   * YC( CCO_O2 )
      RXRAT(  79 ) = RKI(   79 ) * YC( RCO_O2 )   * YC( NO2 )
      RXRAT(  80 ) = RKI(   80 ) * YC( PAN2 )
      RXRAT(  81 ) = RKI(   81 ) * YC( RCO_O2 )   * YC( NO )
      RXRAT(  82 ) = RKI(   82 ) * YC( RCO_O2 )   * YC( HO2 )
      RXRAT(  83 ) = RKI(   83 ) * YC( RCO_O2 )   * YC( NO3 )
      RXRAT(  84 ) = RKI(   84 ) * YC( RCO_O2 )   * YC( C_O2 )
      RXRAT(  85 ) = RKI(   85 ) * YC( RCO_O2 )   * YC( RO2_R )
      RXRAT(  86 ) = RKI(   86 ) * YC( RCO_O2 )   * YC( R2O2 )
      RXRAT(  87 ) = RKI(   87 ) * YC( RCO_O2 )   * YC( RO2_N )
      RXRAT(  88 ) = RKI(   88 ) * YC( RCO_O2 )   * YC( CCO_O2 )
      RXRAT(  89 ) = RKI(   89 ) * YC( RCO_O2 )   * YC( RCO_O2 )
      RXRAT(  90 ) = RKI(   90 ) * YC( BZCO_O2 )  * YC( NO2 )
      RXRAT(  91 ) = RKI(   91 ) * YC( PBZN )
      RXRAT(  92 ) = RKI(   92 ) * YC( BZCO_O2 )  * YC( NO )
      RXRAT(  93 ) = RKI(   93 ) * YC( BZCO_O2 )  * YC( HO2 )
      RXRAT(  94 ) = RKI(   94 ) * YC( BZCO_O2 )  * YC( NO3 )
      RXRAT(  95 ) = RKI(   95 ) * YC( BZCO_O2 )  * YC( C_O2 )
      RXRAT(  96 ) = RKI(   96 ) * YC( BZCO_O2 )  * YC( RO2_R )
      RXRAT(  97 ) = RKI(   97 ) * YC( BZCO_O2 )  * YC( R2O2 )
      RXRAT(  98 ) = RKI(   98 ) * YC( BZCO_O2 )  * YC( RO2_N )
      RXRAT(  99 ) = RKI(   99 ) * YC( BZCO_O2 )  * YC( CCO_O2 )
      RXRAT( 100 ) = RKI(  100 ) * YC( BZCO_O2 )  * YC( RCO_O2 )
      RXRAT( 101 ) = RKI(  101 ) * YC( BZCO_O2 )  * YC( BZCO_O2 )
      RXRAT( 102 ) = RKI(  102 ) * YC( MA_RCO3 )  * YC( NO2 )
      RXRAT( 103 ) = RKI(  103 ) * YC( MA_PAN )
      RXRAT( 104 ) = RKI(  104 ) * YC( MA_RCO3 )  * YC( NO )
      RXRAT( 105 ) = RKI(  105 ) * YC( MA_RCO3 )  * YC( HO2 )
      RXRAT( 106 ) = RKI(  106 ) * YC( MA_RCO3 )  * YC( NO3 )
      RXRAT( 107 ) = RKI(  107 ) * YC( MA_RCO3 )  * YC( C_O2 )
      RXRAT( 108 ) = RKI(  108 ) * YC( MA_RCO3 )  * YC( RO2_R )
      RXRAT( 109 ) = RKI(  109 ) * YC( MA_RCO3 )  * YC( R2O2 )
      RXRAT( 110 ) = RKI(  110 ) * YC( MA_RCO3 )  * YC( RO2_N )
      RXRAT( 111 ) = RKI(  111 ) * YC( MA_RCO3 )  * YC( CCO_O2 )
      RXRAT( 112 ) = RKI(  112 ) * YC( MA_RCO3 )  * YC( RCO_O2 )
      RXRAT( 113 ) = RKI(  113 ) * YC( MA_RCO3 )  * YC( BZCO_O2 )
      RXRAT( 114 ) = RKI(  114 ) * YC( MA_RCO3 )  * YC( MA_RCO3 )
      RXRAT( 115 ) = RKI(  115 ) * YC( TBU_O )    * YC( NO2 )
      RXRAT( 116 ) = RKI(  116 ) * YC( TBU_O )
      RXRAT( 117 ) = RKI(  117 ) * YC( BZ_O )     * YC( NO2 )
      RXRAT( 118 ) = RKI(  118 ) * YC( BZ_O )     * YC( HO2 )
      RXRAT( 119 ) = RKI(  119 ) * YC( BZ_O )
      RXRAT( 120 ) = RKI(  120 ) * YC( BZNO2_O )  * YC( NO2 )
      RXRAT( 121 ) = RKI(  121 ) * YC( BZNO2_O )  * YC( HO2 )
      RXRAT( 122 ) = RKI(  122 ) * YC( BZNO2_O )
      RXRAT( 125 ) = RKI(  125 ) * YC( HCHO )     * YC( HO )
      RXRAT( 126 ) = RKI(  126 ) * YC( HCHO )     * YC( HO2 )
      RXRAT( 127 ) = RKI(  127 ) * YC( HOCOO )
      RXRAT( 128 ) = RKI(  128 ) * YC( HOCOO )    * YC( NO )
      RXRAT( 129 ) = RKI(  129 ) * YC( HCHO )     * YC( NO3 )
      RXRAT( 130 ) = RKI(  130 ) * YC( CCHO )     * YC( HO )
      RXRAT( 132 ) = RKI(  132 ) * YC( CCHO )     * YC( NO3 )
      RXRAT( 133 ) = RKI(  133 ) * YC( RCHO )     * YC( HO )
      RXRAT( 135 ) = RKI(  135 ) * YC( RCHO )     * YC( NO3 )
      RXRAT( 136 ) = RKI(  136 ) * YC( ACET )     * YC( HO )
      RXRAT( 138 ) = RKI(  138 ) * YC( MEK )      * YC( HO )
      RXRAT( 140 ) = RKI(  140 ) * YC( MEOH )     * YC( HO )
      RXRAT( 141 ) = RKI(  141 ) * YC( COOH )     * YC( HO )
      RXRAT( 143 ) = RKI(  143 ) * YC( ROOH )     * YC( HO )
      RXRAT( 147 ) = RKI(  147 ) * YC( GLY )      * YC( HO )
      RXRAT( 148 ) = RKI(  148 ) * YC( GLY )      * YC( NO3 )
      RXRAT( 150 ) = RKI(  150 ) * YC( MGLY )     * YC( HO )
      RXRAT( 151 ) = RKI(  151 ) * YC( MGLY )     * YC( NO3 )
      RXRAT( 153 ) = RKI(  153 ) * YC( PHEN )     * YC( HO )
      RXRAT( 154 ) = RKI(  154 ) * YC( PHEN )     * YC( NO3 )
      RXRAT( 155 ) = RKI(  155 ) * YC( CRES )     * YC( HO )
      RXRAT( 156 ) = RKI(  156 ) * YC( CRES )     * YC( NO3 )
      RXRAT( 157 ) = RKI(  157 ) * YC( NPHE )     * YC( NO3 )
      RXRAT( 158 ) = RKI(  158 ) * YC( BALD )     * YC( HO )
      RXRAT( 160 ) = RKI(  160 ) * YC( BALD )     * YC( NO3 )
      RXRAT( 161 ) = RKI(  161 ) * YC( METHACRO ) * YC( HO )
      RXRAT( 162 ) = RKI(  162 ) * YC( METHACRO ) * YC( O3 )
      RXRAT( 163 ) = RKI(  163 ) * YC( METHACRO ) * YC( NO3 )
      RXRAT( 164 ) = RKI(  164 ) * YC( METHACRO ) * YC( O3P )
      RXRAT( 166 ) = RKI(  166 ) * YC( MVK )      * YC( HO )
      RXRAT( 167 ) = RKI(  167 ) * YC( MVK )      * YC( O3 )
      RXRAT( 168 ) = RKI(  168 ) * YC( MVK )      * YC( O3P )
      RXRAT( 170 ) = RKI(  170 ) * YC( ISOPROD )  * YC( HO )
      RXRAT( 171 ) = RKI(  171 ) * YC( ISOPROD )  * YC( O3 )
      RXRAT( 172 ) = RKI(  172 ) * YC( ISOPROD )  * YC( NO3 )
      RXRAT( 174 ) = RKI(  174 ) * YC( PROD2 )    * YC( HO )
      RXRAT( 176 ) = RKI(  176 ) * YC( RNO3 )     * YC( HO )
      RXRAT( 178 ) = RKI(  178 ) * YC( DCB1 )     * YC( HO )
      RXRAT( 179 ) = RKI(  179 ) * YC( DCB1 )     * YC( O3 )
      RXRAT( 180 ) = RKI(  180 ) * YC( DCB2 )     * YC( HO )
      RXRAT( 182 ) = RKI(  182 ) * YC( DCB3 )     * YC( HO )
      RXRAT( 184 ) = RKI(  184 ) * YC( HO )
      RXRAT( 185 ) = RKI(  185 ) * YC( ETHENE )   * YC( HO )
      RXRAT( 186 ) = RKI(  186 ) * YC( ETHENE )   * YC( O3 )
      RXRAT( 187 ) = RKI(  187 ) * YC( ETHENE )   * YC( NO3 )
      RXRAT( 188 ) = RKI(  188 ) * YC( ETHENE )   * YC( O3P )
      RXRAT( 189 ) = RKI(  189 ) * YC( ISOPRENE ) * YC( HO )
      RXRAT( 190 ) = RKI(  190 ) * YC( ISOPRENE ) * YC( O3 )
      RXRAT( 191 ) = RKI(  191 ) * YC( ISOPRENE ) * YC( NO3 )
      RXRAT( 192 ) = RKI(  192 ) * YC( ISOPRENE ) * YC( O3P )
      RXRAT( 193 ) = RKI(  193 ) * YC( TRP1 )     * YC( HO )
      RXRAT( 194 ) = RKI(  194 ) * YC( TRP1 )     * YC( O3 )
      RXRAT( 195 ) = RKI(  195 ) * YC( TRP1 )     * YC( NO3 )
      RXRAT( 196 ) = RKI(  196 ) * YC( TRP1 )     * YC( O3P )
      RXRAT( 197 ) = RKI(  197 ) * YC( ALK1 )     * YC( HO )
      RXRAT( 198 ) = RKI(  198 ) * YC( ALK2 )     * YC( HO )
      RXRAT( 199 ) = RKI(  199 ) * YC( ALK3 )     * YC( HO )
      RXRAT( 200 ) = RKI(  200 ) * YC( ALK4 )     * YC( HO )
      RXRAT( 201 ) = RKI(  201 ) * YC( ALK5 )     * YC( HO )
      RXRAT( 202 ) = RKI(  202 ) * YC( ARO1 )     * YC( HO )
      RXRAT( 203 ) = RKI(  203 ) * YC( ARO2 )     * YC( HO )
      RXRAT( 204 ) = RKI(  204 ) * YC( OLE1 )     * YC( HO )
      RXRAT( 205 ) = RKI(  205 ) * YC( OLE1 )     * YC( O3 )
      RXRAT( 206 ) = RKI(  206 ) * YC( OLE1 )     * YC( NO3 )
      RXRAT( 207 ) = RKI(  207 ) * YC( OLE1 )     * YC( O3P )
      RXRAT( 208 ) = RKI(  208 ) * YC( OLE2 )     * YC( HO )
      RXRAT( 209 ) = RKI(  209 ) * YC( OLE2 )     * YC( O3 )
      RXRAT( 210 ) = RKI(  210 ) * YC( OLE2 )     * YC( NO3 )
      RXRAT( 211 ) = RKI(  211 ) * YC( OLE2 )     * YC( O3P )
      RXRAT( 212 ) = RKI(  212 ) * YC( HCOOH )    * YC( HO )
      RXRAT( 213 ) = RKI(  213 ) * YC( CCO_OH )   * YC( HO )
      RXRAT( 214 ) = RKI(  214 ) * YC( RCO_OH )   * YC( HO )


      RETURN

      END

S1
