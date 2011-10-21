
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
C $Header: /project/yoj/arc/CCTM/src/gas/ebi_saprc07tb/hrcalc_special.F,v 1.2 2011/10/21 16:10:59 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE HRCALC_SPECIAL

C***********************************************************************
C  Function: To calculate special concentration dependent rate constants
 
C  Preconditions: For the SAPRC07TB family of mechanisms
 
C  Key Subroutines/Functions Called: None
 
C  Revision History: Created by EBI solver program, Feb. 9, 2011
C***********************************************************************
      USE HRDATA

      IMPLICIT NONE

C.....Includes: NONE

C.....Arguments: NONE

C.....Parameters: NONE

C.....External Functions: NONE

C.....Saved Variables:

      CHARACTER( 16 ), SAVE  ::  PNAME = 'HRCALC_SPECIAL'   ! Program name

      REAL, SAVE ::  MNLOSS = 1.0E-30   ! Min value of loss freq
      REAL, SAVE ::  MNCONC = 1.0E-30   ! Min concentration value

C.....Local Variables:

c..Names assigned to special rate constants
      REAL RO2NO           
      REAL RO2HO2          
      REAL RO2NO3          
      REAL RO2RO2          
      REAL RO2RO3          
      REAL RO2RO           
      REAL RO2XRO          
      REAL RO2RO2M         
      REAL RO22NN          

c***********************************************************************

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  This subroutine calculates special rate constants that may depend on
c  species concentrations.  The special rate constants are computed first,
c  and then the mechanism specific rate constants are set to the value
c  of the appropriate rate constant
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c..Compute values of special rate constants

      RO2NO   =          RKI(   56 ) * YC( NO    )      ! RKI(  56) = RKI<BR07>

      RO2HO2  =          RKI(   57 ) * YC( HO2   )      ! RKI(  57) = RKI<BR08>

      RO2NO3  =          RKI(   58 ) * YC( NO3   )      ! RKI(  58) = RKI<BR09>

      RO2RO2  =          RKI(   59 ) * YC( MEO2  )      ! RKI(  59) = RKI<BR10>
     &        +          RKI(   60 ) * YC( RO2C  )      ! RKI(  60) = RKI<BR11>
     &        +          RKI(   60 ) * YC( RO2XC )      ! RKI(  60) = RKI<BR11>

      RO2RO3  =          RKI(   74 ) * YC( MECO3 )      ! RKI(  74) = RKI<BR25>
     &        +          RKI(   74 ) * YC( RCO3  )      ! RKI(  74) = RKI<BR25>
     &        +          RKI(   74 ) * YC( BZCO3 )      ! RKI(  74) = RKI<BR25>
     &        +          RKI(   74 ) * YC( MACO3 )      ! RKI(  74) = RKI<BR25>

      RO2RO   =          RO2NO
     &        +          RO2NO3
     &        +          RO2RO3
     &        +  0.500 * RO2RO2

      RO2XRO  =          RO2HO2
     &        +  0.500 * RO2RO2

      RO2RO2M =  0.500 * RO2RO2

      RO22NN  =          RO2NO3
     &        +          RO2RO3
     &        +  0.500 * RO2RO2

c..set individual mechanism rate constants

      RKI(  118 ) = RO2RO       !  RKI<RO01>
      RKI(  119 ) = RO2XRO      !  RKI<RO02>
      RKI(  120 ) = RO2RO       !  RKI<RO03>
      RKI(  121 ) = RO2XRO      !  RKI<RO04>
      RKI(  122 ) = RO2RO       !  RKI<RO05>
      RKI(  123 ) = RO2XRO      !  RKI<RO06>
      RKI(  124 ) = RO2RO       !  RKI<RO07>
      RKI(  125 ) = RO2XRO      !  RKI<RO08>
      RKI(  126 ) = RO2RO       !  RKI<RO09>
      RKI(  127 ) = RO2XRO      !  RKI<RO10>
      RKI(  128 ) = RO2RO       !  RKI<RO11>
      RKI(  129 ) = RO2XRO      !  RKI<RO12>
      RKI(  130 ) = RO2RO       !  RKI<RO13>
      RKI(  131 ) = RO2XRO      !  RKI<RO14>
      RKI(  132 ) = RO2RO       !  RKI<RO15>
      RKI(  133 ) = RO2XRO      !  RKI<RO16>
      RKI(  134 ) = RO2RO       !  RKI<RO17>
      RKI(  135 ) = RO2XRO      !  RKI<RO18>
      RKI(  215 ) = RO2RO       !  RKI<PO01>
      RKI(  216 ) = RO2XRO      !  RKI<PO02>
      RKI(  217 ) = RO2RO       !  RKI<PO03>
      RKI(  218 ) = RO2XRO      !  RKI<PO04>
      RKI(  219 ) = RO2RO       !  RKI<PO05>
      RKI(  220 ) = RO2XRO      !  RKI<PO06>
      RKI(  221 ) = RO2RO       !  RKI<PO07>
      RKI(  222 ) = RO2XRO      !  RKI<PO08>
      RKI(  223 ) = RO2RO       !  RKI<PO09>
      RKI(  224 ) = RO2XRO      !  RKI<PO10>
      RKI(  225 ) = RO2RO       !  RKI<PO11>
      RKI(  226 ) = RO2XRO      !  RKI<PO12>
      RKI(  227 ) = RO2RO       !  RKI<PO13>
      RKI(  228 ) = RO2XRO      !  RKI<PO14>
      RKI(  229 ) = RO2RO       !  RKI<PO15>
      RKI(  230 ) = RO2XRO      !  RKI<PO16>
      RKI(  231 ) = RO2RO       !  RKI<PO17>
      RKI(  232 ) = RO2XRO      !  RKI<PO18>
      RKI(  233 ) = RO2RO       !  RKI<PO19>
      RKI(  234 ) = RO2XRO      !  RKI<PO20>
      RKI(  235 ) = RO2RO       !  RKI<PO21>
      RKI(  236 ) = RO2XRO      !  RKI<PO22>
      RKI(  237 ) = RO2RO       !  RKI<PO23>
      RKI(  238 ) = RO2XRO      !  RKI<PO24>
      RKI(  239 ) = RO2RO       !  RKI<PO25>
      RKI(  240 ) = RO2XRO      !  RKI<PO26>
      RKI(  241 ) = RO2RO       !  RKI<PO27>
      RKI(  242 ) = RO2XRO      !  RKI<PO28>
      RKI(  243 ) = RO2RO       !  RKI<PO29>
      RKI(  244 ) = RO2XRO      !  RKI<PO30>
      RKI(  245 ) = RO2RO       !  RKI<PO31>
      RKI(  246 ) = RO2XRO      !  RKI<PO32>
      RKI(  247 ) = RO2RO       !  RKI<PO33>
      RKI(  248 ) = RO2XRO      !  RKI<PO34>
      RKI(  249 ) = RO2NO       !  RKI<PO35>
      RKI(  250 ) = RO22NN      !  RKI<PO36>
      RKI(  251 ) = RO2XRO      !  RKI<PO37>
      RKI(  252 ) = RO2HO2      !  RKI<PO38>
      RKI(  253 ) = RO2RO2M     !  RKI<PO39>
      RKI(  254 ) = RO2RO       !  RKI<PO40>
      RKI(  255 ) = RO2HO2      !  RKI<PO41>
      RKI(  256 ) = RO2RO2M     !  RKI<PO42>
      RKI(  257 ) = RO2RO       !  RKI<PO43>
      RKI(  258 ) = RO2HO2      !  RKI<PO44>
      RKI(  259 ) = RO2RO2M     !  RKI<PO45>
      RKI(  260 ) = RO2RO       !  RKI<PO46>
      RKI(  261 ) = RO2RO       !  RKI<PO47>
      RKI(  262 ) = RO2XRO      !  RKI<PO48>
      RKI(  263 ) = RO2RO       !  RKI<PO49>
      RKI(  264 ) = RO2XRO      !  RKI<PO50>
      RKI(  363 ) = RO2RO       !  RKI<CP23>
      RKI(  364 ) = RO2XRO      !  RKI<CP24>
      RKI(  365 ) = RO2RO       !  RKI<CP25>
      RKI(  366 ) = RO2XRO      !  RKI<CP26>
      RKI(  367 ) = RO2RO       !  RKI<CP27>
      RKI(  368 ) = RO2XRO      !  RKI<CP28>

      RETURN
      END
