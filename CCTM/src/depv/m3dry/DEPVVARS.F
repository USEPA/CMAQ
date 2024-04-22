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
C $Header: /project/yoj/arc/CCTM/src/depv/m3dry/DEPVVARS.F,v 1.7 2012/01/19 14:21:45 yoj Exp $

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE DEPVVARS

C-----------------------------------------------------------------------
C Name:     Dry Deposition Variables
C Purpose:  Contains arrays specific to dry deposition species.
C           Initializes dry deposition arrays.
C Revised:  19 Aug 2005  Original version.  (T. Otte and W. Hutzell)
C           25 Nov 2006 J.Young: combine
C           30 Apr 2008  Removed references to RADMDRY.  Added five air
C                        toxic species to LTOTG.  (T. Otte and W. Hutzell)
C           16 Feb 2011 S.Roselle: replaced I/O API include files with
C                        UTILIO_DEFN
C           11 May 2011 J.Bash: Updated for NH3 bidirectional exchange
C           04 Jan 2012 J.Young: Initialize char variables with blank padding
C           28 Aug 2014  G. Sarwar: added deposition for CLNO2 
C           07 Nov 14 J.Bash: Updated for the ASX_DATA_MOD shared data module. 
C           05 May 2015: H.Pye: Added ISOPNN and MTNO3 with Donna Schwede
C           24 Aug 2015: H.Pye: Added IEPOX and HACET with Donna Schwede
C           26 Jan 2016: H. Pye added info for SOA SVOCs
C           24 Mar 2016: G. Sarwar: added iodine and bromine species
C           26 Sep 2016: D. Wong: removed unnecessary use module statements
C           29 Dec 2015: H.Pye: added new monoterpene SOA surrogates
C           06 Aug 2018: M.Qin: added new lumped anthropogenic semivolatiles
C           16 Aug 2018: G. Sarwar: added ClNO3 and FMBR, and updated INO3 and BRNO3 name changes
C           21 Nov 2018: G. Sarwar: added several halogen species
C           09 Jan 2021: G. Sarwar: removed CH3BR
C-----------------------------------------------------------------------

      INTEGER, PARAMETER :: LDDEP = 4
      INTEGER, PARAMETER :: LTOTG = 265
      INTEGER, PARAMETER :: LCMP  = 8
      CHARACTER( 16 )    :: DEPV_METHOD

C-----------------------------------------------------------------------
C General dry deposition arrays.
C-----------------------------------------------------------------------

      INTEGER, SAVE         :: N_USE_DEPSPC        
      LOGICAL, SAVE         :: USE_DEPSPC( LTOTG )
      CHARACTER( 16 ), SAVE :: DEPSPC( LTOTG )

C----------------------------------------------------------------------
C local deposition velocites [ m/s ]
C----------------------------------------------------------------------

      CONTAINS

         FUNCTION DEPVVARS_INIT( ) RESULT ( SUCCESS )

         IMPLICIT NONE

         LOGICAL SUCCESS

         INTEGER ALLOCSTAT
         CHARACTER( 96 ) :: XMSG

         SUCCESS = .TRUE.

C-------------------------------------------------------------------------------
C For M3DRY, set up core species, and include toxic and chlorine compounds.
C-------------------------------------------------------------------------------

         DEPV_METHOD  = 'M3DRY           '

         DEPSPC(  1 ) = 'SO2             '
         DEPSPC(  2 ) = 'SULF            '
         DEPSPC(  3 ) = 'NO2             '
         DEPSPC(  4 ) = 'NO              '
         DEPSPC(  5 ) = 'O3              '
         DEPSPC(  6 ) = 'HNO3            '
         DEPSPC(  7 ) = 'H2O2            '
         DEPSPC(  8 ) = 'ALD             '
         DEPSPC(  9 ) = 'HCHO            '
         DEPSPC( 10 ) = 'OP              '
         DEPSPC( 11 ) = 'PAA             '
         DEPSPC( 12 ) = 'ORA             '
         DEPSPC( 13 ) = 'NH3             '
         DEPSPC( 14 ) = 'PAN             '
         DEPSPC( 15 ) = 'HONO            '
         DEPSPC( 16 ) = 'CO              '
         DEPSPC( 17 ) = 'METHANOL        '
         DEPSPC( 18 ) = 'N2O5            '
         DEPSPC( 19 ) = 'NO3             '
         DEPSPC( 20 ) = 'GEN_ALD         '
         DEPSPC( 21 ) = 'CL2             '
         DEPSPC( 22 ) = 'HOCL            '
         DEPSPC( 23 ) = 'HCL             '
         DEPSPC( 24 ) = 'FMCL            '
         DEPSPC( 25 ) = 'HG              '
         DEPSPC( 26 ) = 'HGIIGAS         '
         DEPSPC( 27 ) = 'TECDD_2378      '
         DEPSPC( 28 ) = 'PECDD_12378     '
         DEPSPC( 29 ) = 'HXCDD_123478    '
         DEPSPC( 30 ) = 'HXCDD_123678    '
         DEPSPC( 31 ) = 'HXCDD_123789    '
         DEPSPC( 32 ) = 'HPCDD_1234678   '
         DEPSPC( 33 ) = 'OTCDD           '
         DEPSPC( 34 ) = 'TECDF_2378      '
         DEPSPC( 35 ) = 'PECDF_12378     '
         DEPSPC( 36 ) = 'PECDF_23478     '
         DEPSPC( 37 ) = 'HXCDF_123478    '
         DEPSPC( 38 ) = 'HXCDF_123678    '
         DEPSPC( 39 ) = 'HXCDF_234678    '
         DEPSPC( 40 ) = 'HXCDF_123789    '
         DEPSPC( 41 ) = 'HPCDF_1234678   '
         DEPSPC( 42 ) = 'HPCDF_1234789   '
         DEPSPC( 43 ) = 'OTCDF           '
         DEPSPC( 44 ) = 'NAPHTHALENE     '
         DEPSPC( 45 ) = '1NITRONAPHTHA   '
         DEPSPC( 46 ) = '2NITRONAPHTHA   '
         DEPSPC( 47 ) = '14NAPHTHOQUIN   '
         DEPSPC( 48 ) = 'HEXMETH_DIIS    '
         DEPSPC( 49 ) = 'HYDRAZINE       '
         DEPSPC( 50 ) = 'MAL_ANHYDRIDE   '
         DEPSPC( 51 ) = 'TOLUENE_DIIS    '
         DEPSPC( 52 ) = 'TRIETHYLAMINE   '
         DEPSPC( 53 ) = 'NTR             '
         DEPSPC( 54 ) = 'NTRM            '
         DEPSPC( 55 ) = 'MPAN            '
         DEPSPC( 56 ) = 'PPN             '
         DEPSPC( 57 ) = 'ISPD            '
         DEPSPC( 58 ) = 'NTRDN           '
         DEPSPC( 59 ) = 'NTRALK          '
         DEPSPC( 60 ) = 'NTROH           '
         DEPSPC( 61 ) = 'NTRPX           '
         DEPSPC( 62 ) = 'PROPNN          '
         DEPSPC( 63 ) = 'CLNO2           '
         DEPSPC( 64 ) = 'ISOPNN          '
         DEPSPC( 65 ) = 'MTNO3           '
         DEPSPC( 66 ) = 'IEPOX           '
         DEPSPC( 67 ) = 'HACET           '
         DEPSPC( 68 ) = 'SVALK1          '
         DEPSPC( 69 ) = 'SVALK2          '
         DEPSPC( 70 ) = 'SVBNZ1          '
         DEPSPC( 71 ) = 'SVBNZ2          '
         DEPSPC( 72 ) = 'SVISO1          '
         DEPSPC( 73 ) = 'SVISO2          '
         DEPSPC( 74 ) = 'SVPAH1          '
         DEPSPC( 75 ) = 'SVPAH2          '
         DEPSPC( 76 ) = 'SVSQT           '
         DEPSPC( 77 ) = 'SVTOL1          '
         DEPSPC( 78 ) = 'SVTOL2          '
         DEPSPC( 79 ) = 'SVTRP1          '
         DEPSPC( 80 ) = 'SVTRP2          '
         DEPSPC( 81 ) = 'SVXYL1          '
         DEPSPC( 82 ) = 'SVXYL2          '
         DEPSPC( 83 ) = 'IO              '
         DEPSPC( 84 ) = 'OIO             '
         DEPSPC( 85 ) = 'I2O2            '
         DEPSPC( 86 ) = 'I2O3            '
         DEPSPC( 87 ) = 'I2O4            '
         DEPSPC( 88 ) = 'HI              '
         DEPSPC( 89 ) = 'HOI             '
         DEPSPC( 90 ) = 'INO             '
         DEPSPC( 91 ) = 'INO2            '
         DEPSPC( 92 ) = 'INO3            '
         DEPSPC( 93 ) = 'BRO             '
         DEPSPC( 94 ) = 'HOBR            '
         DEPSPC( 95 ) = 'HBR             '
         DEPSPC( 96 ) = 'BRNO3           '
         DEPSPC( 97 ) = 'BRNO2           '
         DEPSPC( 98 ) = 'BRCL            '
         DEPSPC( 99 ) = 'DMS             '
         DEPSPC( 100) = 'MSA             '
         DEPSPC( 101) = 'METHANE         '
         DEPSPC( 102) = 'ACRYACID        '
         DEPSPC( 103) = 'CARBSULFIDE     '
         DEPSPC( 104) = 'ACETONITRILE    '
         DEPSPC( 105) = 'METH_NIT_PHEN   ' ! 6-methyl-2-nitrophenol aka 6-nitro-o-cresol
         DEPSPC( 106) = 'PCVOC           '
         DEPSPC( 107) = 'INTR            '
         DEPSPC( 108) = 'ISPX            '
         DEPSPC( 109) = 'ROOH            '
         DEPSPC( 110) = 'LVPCSOG         '
         DEPSPC( 111) = 'VIVPO1          '
         DEPSPC( 112) = 'VLVOO1          '
         DEPSPC( 113) = 'VLVOO2          '
         DEPSPC( 114) = 'VLVPO1          '
         DEPSPC( 115) = 'VSVOO1          '
         DEPSPC( 116) = 'VSVOO2          '
         DEPSPC( 117) = 'VSVOO3          '
         DEPSPC( 118) = 'VSVPO1          '
         DEPSPC( 119) = 'VSVPO2          '
         DEPSPC( 120) = 'VSVPO3          '
         DEPSPC( 121) = 'FACD            '
         DEPSPC( 122) = 'KET             '
         DEPSPC( 123) = 'ETH             '
         DEPSPC( 124) = 'PNA             '
         DEPSPC( 125) = 'GLY             '
         DEPSPC( 126) = 'GLYD            '
         DEPSPC( 127) = 'MGLY            '
         DEPSPC( 128) = 'ETHA            '
         DEPSPC( 129) = 'ETOH            '
         DEPSPC( 130) = 'PAR             '
         DEPSPC( 131) = 'ACET            '
         DEPSPC( 132) = 'PRPA            '
         DEPSPC( 133) = 'ETHY            '
         DEPSPC( 134) = 'OLE             '
         DEPSPC( 135) = 'IOLE            '
         DEPSPC( 136) = 'IEPOX           '
         DEPSPC( 137) = 'BENZ            '
         DEPSPC( 138) = 'CRES            '
         DEPSPC( 139) = 'TOL             '
         DEPSPC( 140) = 'XYLMN           '
         DEPSPC( 141) = 'NAPH            '
         DEPSPC( 142) = 'CAT1            '
         DEPSPC( 143) = 'SESQ            '
         DEPSPC( 144) = 'TERP            '
         DEPSPC( 145) = 'ISOP            '
         DEPSPC( 146) = 'OPEN            '
         DEPSPC( 147) = 'XOPN            '
         DEPSPC( 148) = 'SOAALK          '
         DEPSPC( 149) = 'BUTADIENE13     '
         DEPSPC( 150) = 'ACROLEIN        '
         DEPSPC( 151) = 'SVMT1           '
         DEPSPC( 152) = 'SVMT2           '
         DEPSPC( 153) = 'SVMT3           '
         DEPSPC( 154) = 'SVMT4           '
         DEPSPC( 155) = 'SVMT5           '
         DEPSPC( 156) = 'SVMT6           '
         DEPSPC( 157) = 'SVMT7           '
         DEPSPC( 158) = 'SVAVB1         '
         DEPSPC( 159) = 'SVAVB2         '
         DEPSPC( 160) = 'SVAVB3         '
         DEPSPC( 161) = 'SVAVB4         '
         DEPSPC( 162) = 'CLNO3           '
         DEPSPC( 163) = 'FMBR            '
         DEPSPC( 164) = 'I2             '
         DEPSPC( 165) = 'CH3I           '
         DEPSPC( 166) = 'ICL            '
         DEPSPC( 167) = 'IBR            '
         DEPSPC( 168) = 'MI2            '
         DEPSPC( 169) = 'MIB            '
         DEPSPC( 170) = 'MIC            '
         DEPSPC( 171) = 'BR2            '
         DEPSPC( 172) = 'MB3            '
         DEPSPC( 173) = 'MB2            '
         DEPSPC( 174) = 'MB2C           '
         DEPSPC( 175) = 'MBC2           '
         DEPSPC( 176) = 'MBC            '
         DEPSPC( 177) = 'CLO            '
         DEPSPC( 178) = 'ACD             '             
         DEPSPC( 179) = 'ACE             '             
         DEPSPC( 180) = 'ACRO            '            
         DEPSPC( 181) = 'ACT             '             
         DEPSPC( 182) = 'API             '             
         DEPSPC( 183) = 'BALD            '            
         DEPSPC( 184) = 'BDE13           '           
         DEPSPC( 185) = 'BEN             '             
         DEPSPC( 186) = 'CSL             '             
         DEPSPC( 187) = 'DCB1            '            
         DEPSPC( 188) = 'DCB2            '            
         DEPSPC( 189) = 'DCB3            '            
         DEPSPC( 190) = 'EOH             '             
         DEPSPC( 191) = 'ETE             '             
         DEPSPC( 192) = 'ETEG            '            
         DEPSPC( 193) = 'FURAN           '           
         DEPSPC( 194) = 'FURANONE        '       
         DEPSPC( 195) = 'HC10            '            
         DEPSPC( 196) = 'HC3             '             
         DEPSPC( 197) = 'HC5             '             
         DEPSPC( 198) = 'HKET            '            
         DEPSPC( 199) = 'ISHP            '            
         DEPSPC( 200) = 'ISO             '             
         DEPSPC( 201) = 'ISON            '            
         DEPSPC( 202) = 'LIM             '             
         DEPSPC( 203) = 'LIMAL           '           
         DEPSPC( 204) = 'MACR            '            
         DEPSPC( 205) = 'MAHP            '            
         DEPSPC( 206) = 'MCT             '             
         DEPSPC( 207) = 'MEK             '             
         DEPSPC( 208) = 'MOH             '             
         DEPSPC( 209) = 'MVK             '             
         DEPSPC( 210) = 'NALD            '            
         DEPSPC( 211) = 'OLI             '             
         DEPSPC( 212) = 'OLT             '             
         DEPSPC( 213) = 'ONIT            '            
         DEPSPC( 214) = 'OP1             '             
         DEPSPC( 215) = 'OP2             '             
         DEPSPC( 216) = 'OPB             '             
         DEPSPC( 217) = 'ORA1            '            
         DEPSPC( 218) = 'ORA2            '            
         DEPSPC( 219) = 'PHEN            '            
         DEPSPC( 220) = 'PINAL           '           
         DEPSPC( 221) = 'PROG            '            
         DEPSPC( 222) = 'VROCIOXY        '       
         DEPSPC( 223) = 'VROCP4ALK       '       
         DEPSPC( 224) = 'VROCP4OXY2      '      
         DEPSPC( 225) = 'VROCP5ALK       '       
         DEPSPC( 226) = 'VROCP5ARO       '       
         DEPSPC( 227) = 'VROCP5OXY1      '      
         DEPSPC( 228) = 'VROCP6ALK       '       
         DEPSPC( 229) = 'VROCP6ARO       '       
         DEPSPC( 230) = 'VROCP6OXY1      '      
         DEPSPC( 231) = 'ROH             '             
         DEPSPC( 232) = 'SLOWROC         '         
         DEPSPC( 233) = 'TRPN            '           
         DEPSPC( 234) = 'UALD            '            
         DEPSPC( 235) = 'XYE             '             
         DEPSPC( 236) = 'XYM             '            
         DEPSPC( 237) = 'ELHOM           '           
         DEPSPC( 238) = 'HOM             '             
         DEPSPC( 239) = 'OP3             '             
         DEPSPC( 240) = 'VROCN1ALK       '       
         DEPSPC( 241) = 'VROCN1OXY1      '      
         DEPSPC( 242) = 'VROCN1OXY3      '      
         DEPSPC( 243) = 'VROCN1OXY6      '      
         DEPSPC( 244) = 'VROCN2ALK       '       
         DEPSPC( 245) = 'VROCN2OXY2      '      
         DEPSPC( 246) = 'VROCN2OXY4      '      
         DEPSPC( 247) = 'VROCN2OXY8      '      
         DEPSPC( 248) = 'VROCP0ALK       '       
         DEPSPC( 249) = 'VROCP0OXY2      '      
         DEPSPC( 250) = 'VROCP0OXY4      '      
         DEPSPC( 251) = 'VROCP1ALK       '       
         DEPSPC( 252) = 'VROCP1OXY1      '      
         DEPSPC( 253) = 'VROCP1OXY3      '      
         DEPSPC( 254) = 'VROCP2ALK       '       
         DEPSPC( 255) = 'VROCP2OXY2      '      
         DEPSPC( 256) = 'VROCP3ALK       '       
         DEPSPC( 257) = 'VROCP3OXY2      '
         DEPSPC( 258) = 'IPN             '
         DEPSPC( 259) = 'IPC             '
         DEPSPC( 260) = 'EBZ             '
         DEPSPC( 261) = 'XYL             '
         DEPSPC( 262) = 'STY             '
         DEPSPC( 263) = 'IPX             '
         DEPSPC( 264) = 'INALD           '
         DEPSPC( 265) = 'HONIT           '

         N_USE_DEPSPC = 0
         USE_DEPSPC = .FALSE.   ! array

         RETURN

         END FUNCTION DEPVVARS_INIT

      END MODULE DEPVVARS
