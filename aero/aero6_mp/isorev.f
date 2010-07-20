
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/aero/aero6_mp/isorev.f,v 1.1 2010/07/20 11:55:06 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C This is ISORROPIA Version 1.7 (3/15/06).  More information is available
C at the ISORROPIA homepage: http://nenes.eas.gatech.edu/ISORROPIA/
C
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE ISRP1R
C *** THIS SUBROUTINE IS THE DRIVER ROUTINE FOR THE REVERSE PROBLEM OF 
C     AN AMMONIUM-SULFATE AEROSOL SYSTEM. 
C     THE COMPOSITION REGIME IS DETERMINED BY THE SULFATE RATIO AND BY 
C     THE AMBIENT RELATIVE HUMIDITY.
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE ISRP1R (WI, RHI, TEMPI)
      INCLUDE 'isrpia.inc'
      DIMENSION WI(NCOMP)
C
C *** INITIALIZE COMMON BLOCK VARIABLES *********************************
C
      CALL INIT1 (WI, RHI, TEMPI)
C
C *** CALCULATE SULFATE RATIO *******************************************
C
      IF (RH.GE.DRNH42S4) THEN         ! WET AEROSOL, NEED NH4 AT SRATIO=2.0
         SULRATW = GETASR(WAER(2), RHI)     ! AEROSOL SULFATE RATIO
      ELSE
         SULRATW = 2.0D0                    ! DRY AEROSOL SULFATE RATIO
      ENDIF
      SULRAT  = WAER(3)/WAER(2)         ! SULFATE RATIO
C
C *** FIND CALCULATION REGIME FROM (SULRAT,RH) **************************
C
C *** SULFATE POOR 
C
      IF (SULRATW.LE.SULRAT) THEN
C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'K2'
         CALL CALCK2                 ! Only liquid (metastable)
      ELSE
C
         IF (RH.LT.DRNH42S4) THEN    
            SCASE = 'K1'
            CALL CALCK1              ! NH42SO4              ; case K1
C
         ELSEIF (DRNH42S4.LE.RH) THEN
            SCASE = 'K2'
            CALL CALCK2              ! Only liquid          ; case K2
         ENDIF
      ENDIF
C
C *** SULFATE RICH (NO ACID)
C
      ELSEIF (1.0.LE.SULRAT .AND. SULRAT.LT.SULRATW) THEN
      W(2) = WAER(2)
      W(3) = WAER(3)
C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'B4'
         CALL CALCB4                 ! Only liquid (metastable)
         SCASE = 'L4'
      ELSE
C
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'B1'
            CALL CALCB1              ! NH4HSO4,LC,NH42SO4   ; case B1
            SCASE = 'L1'
C
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRLC) THEN         
            SCASE = 'B2'
            CALL CALCB2              ! LC,NH42S4            ; case B2
            SCASE = 'L2'
C
         ELSEIF (DRLC.LE.RH .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'B3'
            CALL CALCB3              ! NH42S4               ; case B3
            SCASE = 'L3'
C
         ELSEIF (DRNH42S4.LE.RH) THEN         
            SCASE = 'B4'
            CALL CALCB4              ! Only liquid          ; case B4
            SCASE = 'L4'
         ENDIF
      ENDIF
C
      CALL CALCNH3P          ! Compute NH3(g)
C
C *** SULFATE RICH (FREE ACID)
C
      ELSEIF (SULRAT.LT.1.0) THEN             
      W(2) = WAER(2)
      W(3) = WAER(3)
C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'C2'
         CALL CALCC2                 ! Only liquid (metastable)
         SCASE = 'M2'
      ELSE
C
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'C1'
            CALL CALCC1              ! NH4HSO4              ; case C1
            SCASE = 'M1'
C
         ELSEIF (DRNH4HS4.LE.RH) THEN         
            SCASE = 'C2'
            CALL CALCC2              ! Only liquid          ; case C2
            SCASE = 'M2'
         ENDIF
      ENDIF
C 
      CALL CALCNH3P
C
      ENDIF
      RETURN
C
C *** END OF SUBROUTINE ISRP1R *****************************************
C
      END

C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE ISRP2R
C *** THIS SUBROUTINE IS THE DRIVER ROUTINE FOR THE REVERSE PROBLEM OF 
C     AN AMMONIUM-SULFATE-NITRATE AEROSOL SYSTEM. 
C     THE COMPOSITION REGIME IS DETERMINED BY THE SULFATE RATIO AND BY
C     THE AMBIENT RELATIVE HUMIDITY.
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE ISRP2R (WI, RHI, TEMPI)
      INCLUDE 'isrpia.inc'
      DIMENSION WI(NCOMP)
      LOGICAL   TRYLIQ
C
C *** INITIALIZE ALL VARIABLES IN COMMON BLOCK **************************
C
      TRYLIQ = .TRUE.             ! Assume liquid phase, sulfate poor limit 
C
10    CALL INIT2 (WI, RHI, TEMPI)
C
C *** CALCULATE SULFATE RATIO *******************************************
C
      IF (TRYLIQ .AND. RH.GE.DRNH4NO3) THEN ! *** WET AEROSOL
         SULRATW = GETASR(WAER(2), RHI)     ! LIMITING SULFATE RATIO
      ELSE
         SULRATW = 2.0D0                    ! *** DRY AEROSOL
      ENDIF
      SULRAT = WAER(3)/WAER(2)
C
C *** FIND CALCULATION REGIME FROM (SULRAT,RH) **************************
C
C *** SULFATE POOR 
C
      IF (SULRATW.LE.SULRAT) THEN                
C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'N3'
         CALL CALCN3                 ! Only liquid (metastable)
      ELSE
C
         IF (RH.LT.DRNH4NO3) THEN    
            SCASE = 'N1'
            CALL CALCN1              ! NH42SO4,NH4NO3       ; case N1
C
         ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'N2'
            CALL CALCN2              ! NH42S4               ; case N2
C
         ELSEIF (DRNH42S4.LE.RH) THEN
            SCASE = 'N3'
            CALL CALCN3              ! Only liquid          ; case N3
         ENDIF
      ENDIF
C
C *** SULFATE RICH (NO ACID)
C
C     FOR SOLVING THIS CASE, NITRIC ACID AND AMMONIA IN THE GAS PHASE ARE
C     ASSUMED A MINOR SPECIES, THAT DO NOT SIGNIFICANTLY AFFECT THE 
C     AEROSOL EQUILIBRIUM.
C
      ELSEIF (1.0.LE.SULRAT .AND. SULRAT.LT.SULRATW) THEN 
      W(2) = WAER(2)
      W(3) = WAER(3)
      W(4) = WAER(4)
C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'B4'
         CALL CALCB4                 ! Only liquid (metastable)
         SCASE = 'O4'
      ELSE
C
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'B1'
            CALL CALCB1              ! NH4HSO4,LC,NH42SO4   ; case O1
            SCASE = 'O1'
C
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRLC) THEN         
            SCASE = 'B2'
            CALL CALCB2              ! LC,NH42S4            ; case O2
            SCASE = 'O2'
C
         ELSEIF (DRLC.LE.RH .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'B3'
            CALL CALCB3              ! NH42S4               ; case O3
            SCASE = 'O3'
C
         ELSEIF (DRNH42S4.LE.RH) THEN         
            SCASE = 'B4'
            CALL CALCB4              ! Only liquid          ; case O4
            SCASE = 'O4'
         ENDIF
      ENDIF
C
C *** Add the NO3 to the solution now and calculate partitioning.
C
      MOLAL(7) = WAER(4)             ! There is always water, so NO3(aer) is NO3-
      MOLAL(1) = MOLAL(1) + WAER(4)  ! Add H+ to balance out
      CALL CALCNAP            ! HNO3, NH3 dissolved
      CALL CALCNH3P
C
C *** SULFATE RICH (FREE ACID)
C
C     FOR SOLVING THIS CASE, NITRIC ACID AND AMMONIA IN THE GAS PHASE ARE
C     ASSUMED A MINOR SPECIES, THAT DO NOT SIGNIFICANTLY AFFECT THE 
C     AEROSOL EQUILIBRIUM.
C
      ELSEIF (SULRAT.LT.1.0) THEN             
      W(2) = WAER(2)
      W(3) = WAER(3)
      W(4) = WAER(4)
C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'C2'
         CALL CALCC2                 ! Only liquid (metastable)
         SCASE = 'P2'
      ELSE
C
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'C1'
            CALL CALCC1              ! NH4HSO4              ; case P1
            SCASE = 'P1'
C
         ELSEIF (DRNH4HS4.LE.RH) THEN         
            SCASE = 'C2'
            CALL CALCC2              ! Only liquid          ; case P2
            SCASE = 'P2'
         ENDIF
      ENDIF
C
C *** Add the NO3 to the solution now and calculate partitioning.
C
      MOLAL(7) = WAER(4)             ! There is always water, so NO3(aer) is NO3-
      MOLAL(1) = MOLAL(1) + WAER(4)  ! Add H+ to balance out
C
      CALL CALCNAP                   ! HNO3, NH3 dissolved
      CALL CALCNH3P
      ENDIF
C
C *** IF SULRATW < SULRAT < 2.0 and WATER = 0 => SULFATE RICH CASE.
C
      IF (SULRATW.LE.SULRAT .AND. SULRAT.LT.2.0  
     &                                    .AND. WATER.LE.TINY) THEN
          TRYLIQ = .FALSE.
          GOTO 10
      ENDIF
C
      RETURN
C
C *** END OF SUBROUTINE ISRP2R *****************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE ISRP3R
C *** THIS SUBROUTINE IS THE DRIVER ROUTINE FOR THE REVERSE PROBLEM OF
C     AN AMMONIUM-SULFATE-NITRATE-CHLORIDE-SODIUM AEROSOL SYSTEM. 
C     THE COMPOSITION REGIME IS DETERMINED BY THE SULFATE & SODIUM 
C     RATIOS AND BY THE AMBIENT RELATIVE HUMIDITY.
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE ISRP3R (WI, RHI, TEMPI)
      INCLUDE 'isrpia.inc'
      DIMENSION WI(NCOMP)
      LOGICAL   TRYLIQ
ccC
ccC *** ADJUST FOR TOO LITTLE AMMONIUM AND CHLORIDE ***********************
ccC
cc      WI(3) = MAX (WI(3), 1.D-10)  ! NH4+ : 1e-4 umoles/m3
cc      WI(5) = MAX (WI(5), 1.D-10)  ! Cl-  : 1e-4 umoles/m3
C
C *** INITIALIZE ALL VARIABLES ******************************************
C
      TRYLIQ = .TRUE.             ! Use liquid phase sulfate poor limit 
C
10    CALL ISOINIT3 (WI, RHI, TEMPI) ! COMMON block variables
ccC
ccC *** CHECK IF TOO MUCH SODIUM ; ADJUST AND ISSUE ERROR MESSAGE *********
ccC
cc      REST = 2.D0*WAER(2) + WAER(4) + WAER(5) 
cc      IF (WAER(1).GT.REST) THEN            ! NA > 2*SO4+CL+NO3 ?
cc         WAER(1) = (ONE-1D-6)*REST         ! Adjust Na amount
cc         CALL PUSHERR (0050, 'ISRP3R')     ! Warning error: Na adjusted
cc      ENDIF
C
C *** CALCULATE SULFATE & SODIUM RATIOS *********************************
C
      IF (TRYLIQ .AND. RH.GE.DRNH4NO3) THEN  ! ** WET AEROSOL
         FRSO4   = WAER(2) - WAER(1)/2.0D0     ! SULFATE UNBOUND BY SODIUM
         FRSO4   = MAX(FRSO4, TINY)
         SRI     = GETASR(FRSO4, RHI)          ! SULFATE RATIO FOR NH4+
         SULRATW = (WAER(1)+FRSO4*SRI)/WAER(2) ! LIMITING SULFATE RATIO
         SULRATW = MIN (SULRATW, 2.0D0)
      ELSE
         SULRATW = 2.0D0                     ! ** DRY AEROSOL
      ENDIF
      SULRAT = (WAER(1)+WAER(3))/WAER(2)
      SODRAT = WAER(1)/WAER(2)
C
C *** FIND CALCULATION REGIME FROM (SULRAT,RH) **************************
C
C *** SULFATE POOR ; SODIUM POOR
C
      IF (SULRATW.LE.SULRAT .AND. SODRAT.LT.2.0) THEN                
C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'Q5'
         CALL CALCQ5                 ! Only liquid (metastable)
         SCASE = 'Q5'
      ELSE
C
         IF (RH.LT.DRNH4NO3) THEN    
            SCASE = 'Q1'
            CALL CALCQ1              ! NH42SO4,NH4NO3,NH4CL,NA2SO4
C
         ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNH4CL) THEN         
            SCASE = 'Q2'
            CALL CALCQ2              ! NH42SO4,NH4CL,NA2SO4
C
         ELSEIF (DRNH4CL.LE.RH  .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'Q3'
            CALL CALCQ3              ! NH42SO4,NA2SO4
C 
        ELSEIF (DRNH42S4.LE.RH  .AND. RH.LT.DRNA2SO4) THEN         
            SCASE = 'Q4'
            CALL CALCQ4              ! NA2SO4
            SCASE = 'Q4'
C
         ELSEIF (DRNA2SO4.LE.RH) THEN         
            SCASE = 'Q5'
            CALL CALCQ5              ! Only liquid
            SCASE = 'Q5'
         ENDIF
      ENDIF
C
C *** SULFATE POOR ; SODIUM RICH
C
      ELSE IF (SULRAT.GE.SULRATW .AND. SODRAT.GE.2.0) THEN                
C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'R6'
         CALL CALCR6                 ! Only liquid (metastable)
         SCASE = 'R6'
      ELSE
C
         IF (RH.LT.DRNH4NO3) THEN    
            SCASE = 'R1'
            CALL CALCR1              ! NH4NO3,NH4CL,NA2SO4,NACL,NANO3
C
         ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNANO3) THEN         
            SCASE = 'R2'
            CALL CALCR2              ! NH4CL,NA2SO4,NACL,NANO3
C
         ELSEIF (DRNANO3.LE.RH  .AND. RH.LT.DRNACL) THEN         
            SCASE = 'R3'
            CALL CALCR3              ! NH4CL,NA2SO4,NACL
C
         ELSEIF (DRNACL.LE.RH   .AND. RH.LT.DRNH4CL) THEN         
            SCASE = 'R4'
            CALL CALCR4              ! NH4CL,NA2SO4
C
         ELSEIF (DRNH4CL.LE.RH .AND. RH.LT.DRNA2SO4) THEN         
            SCASE = 'R5'
            CALL CALCR5              ! NA2SO4
            SCASE = 'R5'
C
         ELSEIF (DRNA2SO4.LE.RH) THEN         
            SCASE = 'R6'
            CALL CALCR6              ! NO SOLID
            SCASE = 'R6'
         ENDIF
      ENDIF
C
C *** SULFATE RICH (NO ACID) 
C
      ELSEIF (1.0.LE.SULRAT .AND. SULRAT.LT.SULRATW) THEN 
      DO 100 I=1,NCOMP
         W(I) = WAER(I)
100   CONTINUE
C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'I6'
         CALL CALCI6                 ! Only liquid (metastable)
         SCASE = 'S6'
      ELSE
C
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'I1'
            CALL CALCI1              ! NA2SO4,(NH4)2SO4,NAHSO4,NH4HSO4,LC
            SCASE = 'S1'
C
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRNAHSO4) THEN         
            SCASE = 'I2'
            CALL CALCI2              ! NA2SO4,(NH4)2SO4,NAHSO4,LC
            SCASE = 'S2'
C
         ELSEIF (DRNAHSO4.LE.RH .AND. RH.LT.DRLC) THEN         
            SCASE = 'I3'
            CALL CALCI3              ! NA2SO4,(NH4)2SO4,LC
            SCASE = 'S3'
C
         ELSEIF (DRLC.LE.RH     .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'I4'
            CALL CALCI4              ! NA2SO4,(NH4)2SO4
            SCASE = 'S4'
C
         ELSEIF (DRNH42S4.LE.RH .AND. RH.LT.DRNA2SO4) THEN         
            SCASE = 'I5'
            CALL CALCI5              ! NA2SO4
            SCASE = 'S5'
C
         ELSEIF (DRNA2SO4.LE.RH) THEN         
            SCASE = 'I6'
            CALL CALCI6              ! NO SOLIDS
            SCASE = 'S6'
         ENDIF
      ENDIF
C
      CALL CALCNHP                ! HNO3, NH3, HCL in gas phase
      CALL CALCNH3P
C
C *** SULFATE RICH (FREE ACID)
C
      ELSEIF (SULRAT.LT.1.0) THEN             
      DO 200 I=1,NCOMP
         W(I) = WAER(I)
200   CONTINUE
C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'J3'
         CALL CALCJ3                 ! Only liquid (metastable)
         SCASE = 'T3'
      ELSE
C
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'J1'
            CALL CALCJ1              ! NH4HSO4,NAHSO4
            SCASE = 'T1'
C
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRNAHSO4) THEN         
            SCASE = 'J2'
            CALL CALCJ2              ! NAHSO4
            SCASE = 'T2'
C
         ELSEIF (DRNAHSO4.LE.RH) THEN         
            SCASE = 'J3'
            CALL CALCJ3              
            SCASE = 'T3'
         ENDIF
      ENDIF
C
      CALL CALCNHP                ! HNO3, NH3, HCL in gas phase
      CALL CALCNH3P
C
      ENDIF
C
C *** IF AFTER CALCULATIONS, SULRATW < SULRAT < 2.0  
C                            and WATER = 0          => SULFATE RICH CASE.
C
      IF (SULRATW.LE.SULRAT .AND. SULRAT.LT.2.0  
     &                      .AND. WATER.LE.TINY) THEN
          TRYLIQ = .FALSE.
          GOTO 10
      ENDIF
C
      RETURN
C
C *** END OF SUBROUTINE ISRP3R *****************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCK2
C *** CASE K2 
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0)
C     2. LIQUID AEROSOL PHASE ONLY POSSIBLE
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCK2
      INCLUDE 'isrpia.inc'
      DOUBLE PRECISION NH4I, NH3GI, NH3AQ
C
C *** SETUP PARAMETERS ************************************************
C
      CALAOU   =.TRUE.     ! Outer loop activity calculation flag
      FRST     =.TRUE.
      CALAIN   =.TRUE.
C
C *** CALCULATE WATER CONTENT *****************************************
C
      MOLALR(4)= MIN(WAER(2), 0.5d0*WAER(3))
      WATER    = MOLALR(4)/M0(4)  ! ZSR correlation
C
C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
C
      DO 10 I=1,NSWEEP
CC         A21  = XK21*WATER*R*TEMP
         A2   = XK2 *R*TEMP/XKW/RH*(GAMA(8)/GAMA(9))**2.
         AKW  = XKW *RH*WATER*WATER
C
         NH4I = WAER(3)
         SO4I = WAER(2)
         HSO4I= ZERO
C
         CALL CALCPH (2.D0*SO4I - NH4I, HI, OHI)    ! Get pH
C
         NH3AQ = ZERO                               ! AMMONIA EQUILIBRIUM
         IF (HI.LT.OHI) THEN
            CALL CALCAMAQ (NH4I, OHI, DEL)
            NH4I  = MAX (NH4I-DEL, ZERO) 
            OHI   = MAX (OHI -DEL, TINY)
            NH3AQ = DEL
            HI    = AKW/OHI
         ENDIF
C
         CALL CALCHS4 (HI, SO4I, ZERO, DEL)         ! SULFATE EQUILIBRIUM
         SO4I  = SO4I - DEL
         HI    = HI   - DEL
         HSO4I = DEL
C
         NH3GI = NH4I/HI/A2   !    NH3AQ/A21
C
C *** SPECIATION & WATER CONTENT ***************************************
C
         MOLAL(1) = HI
         MOLAL(3) = NH4I
         MOLAL(5) = SO4I
         MOLAL(6) = HSO4I
         COH      = OHI
         GASAQ(1) = NH3AQ
         GNH3     = NH3GI
C
C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
C
         IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
            CALL CALCACT     
         ELSE
            GOTO 20
         ENDIF
10    CONTINUE
C
20    RETURN
C
C *** END OF SUBROUTINE CALCK2 ****************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCK1
C *** CASE K1 
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0)
C     2. SOLID AEROSOL ONLY
C     3. SOLIDS POSSIBLE : (NH4)2SO4
C
C     A SIMPLE MATERIAL BALANCE IS PERFORMED, AND THE SOLID (NH4)2SO4
C     IS CALCULATED FROM THE SULFATES. THE EXCESS AMMONIA REMAINS IN
C     THE GAS PHASE.
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCK1
      INCLUDE 'isrpia.inc'
C
      CNH42S4 = MIN(WAER(2),0.5d0*WAER(3))  ! For bad input problems
      GNH3    = ZERO
C
      W(2)    = CNH42S4
      W(3)    = 2.D0*CNH42S4 + GNH3
C
      RETURN
C
C *** END OF SUBROUTINE CALCK1 ******************************************
C
      END


C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCN3
C *** CASE N3
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0)
C     2. THERE IS ONLY A LIQUID PHASE
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCN3
      INCLUDE 'isrpia.inc'
      DOUBLE PRECISION NH4I, NO3I, NH3AQ, NO3AQ
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** SETUP PARAMETERS ************************************************
C
      CALAOU =.TRUE.              ! Outer loop activity calculation flag
      FRST   =.TRUE.
      CALAIN =.TRUE.
C
C *** AEROSOL WATER CONTENT
C
      MOLALR(4) = MIN(WAER(2),0.5d0*WAER(3))       ! (NH4)2SO4
      AML5      = MAX(WAER(3)-2.D0*MOLALR(4),ZERO) ! "free" NH4
      MOLALR(5) = MAX(MIN(AML5,WAER(4)), ZERO)     ! NH4NO3=MIN("free",NO3)
      WATER     = MOLALR(4)/M0(4) + MOLALR(5)/M0(5)
      WATER     = MAX(WATER, TINY)
C
C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
C
      DO 10 I=1,NSWEEP
         A2    = XK2 *R*TEMP/XKW/RH*(GAMA(8)/GAMA(9))**2.
CC         A21   = XK21*WATER*R*TEMP
         A3    = XK4*R*TEMP*(WATER/GAMA(10))**2.0
         A4    = XK7*(WATER/GAMA(4))**3.0
         AKW   = XKW *RH*WATER*WATER
C
C ION CONCENTRATIONS
C
         NH4I  = WAER(3)
         NO3I  = WAER(4)
         SO4I  = WAER(2)
         HSO4I = ZERO
C
         CALL CALCPH (2.D0*SO4I + NO3I - NH4I, HI, OHI)
C
C AMMONIA ASSOCIATION EQUILIBRIUM
C
         NH3AQ = ZERO
         NO3AQ = ZERO
         GG    = 2.D0*SO4I + NO3I - NH4I
         IF (HI.LT.OHI) THEN
            CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
            HI    = AKW/OHI
         ELSE
            HI    = ZERO
            CALL CALCNIAQ2 (GG, NO3I, HI, NO3AQ) ! HNO3
C
C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
C
            CALL CALCHS4 (HI, SO4I, ZERO, DEL)
            SO4I  = SO4I  - DEL
            HI    = HI    - DEL
            HSO4I = DEL
            OHI   = AKW/HI
         ENDIF
C
C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
C
         MOLAL (1) = HI
         MOLAL (3) = NH4I
         MOLAL (5) = SO4I
         MOLAL (6) = HSO4I
         MOLAL (7) = NO3I
         COH       = OHI
C
         CNH42S4   = ZERO
         CNH4NO3   = ZERO
C
         GASAQ(1)  = NH3AQ
         GASAQ(3)  = NO3AQ
C
         GHNO3     = HI*NO3I/A3
         GNH3      = NH4I/HI/A2   !   NH3AQ/A21 
C
C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP ******************
C
         IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
            CALL CALCACT     
         ELSE
            GOTO 20
         ENDIF
10    CONTINUE
C
C *** RETURN ***********************************************************
C
20    RETURN
C
C *** END OF SUBROUTINE CALCN3 *****************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCN2
C *** CASE N2
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0)
C     2. THERE IS BOTH A LIQUID & SOLID PHASE
C     3. SOLIDS POSSIBLE : (NH4)2SO4
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCN2
      INCLUDE 'isrpia.inc'
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** SETUP PARAMETERS ************************************************
C
      CHI1   = MIN(WAER(2),0.5d0*WAER(3))     ! (NH4)2SO4
      CHI2   = MAX(WAER(3) - 2.D0*CHI1, ZERO) ! "Free" NH4+
      CHI3   = MAX(WAER(4) - CHI2, ZERO)      ! "Free" NO3
C
      PSI2   = CHI2
      PSI3   = CHI3
C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI1LO = TINY                ! Low  limit
      PSI1HI = CHI1                ! High limit
C
C *** INITIAL VALUES FOR BISECTION ************************************
C
      X1 = PSI1HI
      Y1 = FUNCN2 (X1)
      IF (Y1.LE.EPS) RETURN   ! IF (ABS(Y1).LE.EPS .OR. Y1.LE.ZERO) RETURN
      YHI= Y1                 ! Save Y-value at HI position
C
C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
C
      DX = (PSI1HI-PSI1LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = MAX(X1-DX, ZERO)
         Y2 = FUNCN2 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
C
C *** NO SUBDIVISION WITH SOLUTION FOUND 
C
      YLO= Y1                      ! Save Y-value at Hi position
      IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         RETURN
C
C *** { YLO, YHI } < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NH3
C
      ELSE IF (YLO.LT.ZERO .AND. YHI.LT.ZERO) THEN
         P4 = CHI4
         YY = FUNCN2(P4)
         GOTO 50
C
C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NH3
C
      ELSE IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         P4 = TINY
         YY = FUNCN2(P4)
         GOTO 50
      ELSE
         CALL PUSHERR (0001, 'CALCN2')    ! WARNING ERROR: NO SOLUTION
         RETURN
      ENDIF
C
C *** PERFORM BISECTION ***********************************************
C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCN2 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCN2')    ! WARNING ERROR: NO CONVERGENCE
C
C *** CONVERGED ; RETURN **********************************************
C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCN2 (X3)
50    CONTINUE
      RETURN
C
C *** END OF SUBROUTINE CALCN2 ******************************************
C
      END



C======================================================================
C
C *** ISORROPIA CODE
C *** FUNCTION FUNCN2
C *** CASE D2 
C     FUNCTION THAT SOLVES THE SYSTEM OF EQUATIONS FOR CASE D2 ; 
C     AND RETURNS THE VALUE OF THE ZEROED FUNCTION IN FUNCN2.
C
C=======================================================================
C
      DOUBLE PRECISION FUNCTION FUNCN2 (P1)
      INCLUDE 'isrpia.inc'
      DOUBLE PRECISION NH4I, NO3I, NH3AQ, NO3AQ
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** SETUP PARAMETERS ************************************************
C
      FRST   = .TRUE.
      CALAIN = .TRUE.
      PSI1   = P1
C
C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
C
      DO 10 I=1,NSWEEP
         A2    = XK2 *R*TEMP/XKW/RH*(GAMA(8)/GAMA(9))**2.
CC         A21   = XK21*WATER*R*TEMP
         A3    = XK4*R*TEMP*(WATER/GAMA(10))**2.0
         A4    = XK7*(WATER/GAMA(4))**3.0
         AKW   = XKW *RH*WATER*WATER
C
C ION CONCENTRATIONS
C
         NH4I  = 2.D0*PSI1 + PSI2 
         NO3I  = PSI2 + PSI3
         SO4I  = PSI1 
         HSO4I = ZERO
C
         CALL CALCPH (2.D0*SO4I + NO3I - NH4I, HI, OHI)
C
C AMMONIA ASSOCIATION EQUILIBRIUM
C
         NH3AQ = ZERO
         NO3AQ = ZERO
         GG    = 2.D0*SO4I + NO3I - NH4I
         IF (HI.LT.OHI) THEN
            CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
            HI    = AKW/OHI
         ELSE
            HI    = ZERO
            CALL CALCNIAQ2 (GG, NO3I, HI, NO3AQ) ! HNO3
C
C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
C
            CALL CALCHS4 (HI, SO4I, ZERO, DEL)
            SO4I  = SO4I  - DEL
            HI    = HI    - DEL
            HSO4I = DEL
            OHI   = AKW/HI
         ENDIF
C
C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
C
         MOLAL (1) = HI
         MOLAL (3) = NH4I
         MOLAL (5) = SO4I
         MOLAL (6) = HSO4I
         MOLAL (7) = NO3I
         COH       = OHI
C
         CNH42S4   = CHI1 - PSI1
         CNH4NO3   = ZERO
C
         GASAQ(1)  = NH3AQ
         GASAQ(3)  = NO3AQ
C
         GHNO3     = HI*NO3I/A3
         GNH3      = NH4I/HI/A2   !   NH3AQ/A21 
C
C *** CALCULATE MOLALR ARRAY, WATER AND ACTIVITIES **********************
C
         CALL CALCMR
C
C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
C
         IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
            CALL CALCACT     
         ELSE
            GOTO 20
         ENDIF
10    CONTINUE
C
C *** CALCULATE OBJECTIVE FUNCTION ************************************
C
20    FUNCN2= NH4I*NH4I*SO4I/A4 - ONE 
      RETURN
C
C *** END OF FUNCTION FUNCN2 ********************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCN1
C *** CASE N1 
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0)
C     2. SOLID AEROSOL ONLY
C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4NO3
C
C     THERE ARE TWO REGIMES DEFINED BY RELATIVE HUMIDITY:
C     1. RH < MDRH  ; ONLY SOLID PHASE POSSIBLE (SUBROUTINE CALCN1A)
C     2. RH >= MDRH ; LIQUID PHASE POSSIBLE (MDRH REGION)
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCN1
      INCLUDE 'isrpia.inc'
      EXTERNAL CALCN1A, CALCN2
C
C *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
C
      IF (RH.LT.DRMASAN) THEN    
         SCASE = 'N1 ; SUBCASE 1'  
         CALL CALCN1A              ! SOLID PHASE ONLY POSSIBLE
         SCASE = 'N1 ; SUBCASE 1'
      ELSE
         SCASE = 'N1 ; SUBCASE 2'  
         CALL CALCMDRP (RH, DRMASAN, DRNH4NO3, CALCN1A, CALCN2)
         SCASE = 'N1 ; SUBCASE 2'
      ENDIF
C 
      RETURN
C
C *** END OF SUBROUTINE CALCN1 ******************************************
C
      END



C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCN1A
C *** CASE N1 ; SUBCASE 1
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0)
C     2. SOLID AEROSOL ONLY
C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4NO3
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCN1A
      INCLUDE 'isrpia.inc'
C
C *** SETUP PARAMETERS *************************************************
C
CCC      A1      = XK10/R/TEMP/R/TEMP
C
C *** CALCULATE AEROSOL COMPOSITION ************************************
C
CCC      CHI1    = 2.D0*WAER(4)        ! Free parameter ; arbitrary value.
      PSI1    = WAER(4)
C
C *** The following statment is here to avoid negative NH4+ values in 
C     CALCN? routines that call CALCN1A
C
      PSI2    = MAX(MIN(WAER(2),0.5d0*(WAER(3)-PSI1)),TINY)
C
      CNH4NO3 = PSI1
      CNH42S4 = PSI2
C
CCC      GNH3    = CHI1 + PSI1 + 2.0*PSI2
CCC      GHNO3   = A1/(CHI1-PSI1) + PSI1
      GNH3    = ZERO
      GHNO3   = ZERO
C
      W(2)    = PSI2
      W(3)    = GNH3  + PSI1 + 2.0*PSI2   
      W(4)    = GHNO3 + PSI1
C
      RETURN
C
C *** END OF SUBROUTINE CALCN1A *****************************************
C
      END

C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCQ5
C *** CASE Q5
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0); SODIUM POOR (SODRAT < 2.0)
C     2. LIQUID AND SOLID PHASES ARE POSSIBLE
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCQ5
      INCLUDE 'isrpia.inc'
C
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** SETUP PARAMETERS ************************************************
C
      FRST    =.TRUE.
      CALAIN  =.TRUE. 
      CALAOU  =.TRUE.
C
C *** CALCULATE INITIAL SOLUTION ***************************************
C
      CALL CALCQ1A
C
      PSI1   = CNA2SO4      ! SALTS DISSOLVED
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI6   = CNH42S4
C
      CALL CALCMR           ! WATER
C
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
C
C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
C
      DO 10 I=1,NSWEEP
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
C
C ION CONCENTRATIONS
C
      NAI    = WAER(1)
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
C
C SOLUTION ACIDIC OR BASIC?
C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I
      IF (GG.GT.TINY) THEN                        ! H+ in excess
         BB =-GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         HI = 0.5D0*(-BB + SQRT(DD))
         OHI= AKW/HI
      ELSE                                        ! OH- in excess
         BB = GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         OHI= 0.5D0*(-BB + SQRT(DD))
         HI = AKW/OHI
      ENDIF
C
C UNDISSOCIATED SPECIES EQUILIBRIA
C
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
         HSO4I = ZERO
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
C
C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
C
         CALL CALCHS4 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
C
C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
C
C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
ccc      CALL PUSHERR (0002, 'CALCQ5')    ! WARNING ERROR: NO CONVERGENCE
C 
C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNACL   = ZERO
      CNANO3  = ZERO
      CNA2SO4 = ZERO
C
      RETURN
C
C *** END OF SUBROUTINE CALCQ5 ******************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCQ4
C *** CASE Q4
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0); SODIUM POOR (SODRAT < 2.0)
C     2. LIQUID AND SOLID PHASES ARE POSSIBLE
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCQ4
      INCLUDE 'isrpia.inc'
C
      LOGICAL PSCONV1
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** SETUP PARAMETERS ************************************************
C
      FRST    =.TRUE.
      CALAIN  =.TRUE. 
      CALAOU  =.TRUE.
C 
      PSCONV1 =.TRUE.
      PSI1O   =-GREAT
      ROOT3   = ZERO
C
C *** CALCULATE INITIAL SOLUTION ***************************************
C
      CALL CALCQ1A
C
      CHI1   = CNA2SO4      ! SALTS
C
      PSI1   = CNA2SO4      ! AMOUNT DISSOLVED
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI6   = CNH42S4
C
      CALL CALCMR           ! WATER
C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
C
C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
C
      DO 10 I=1,NSWEEP
      A5  = XK5 *(WATER/GAMA(2))**3.         ! Na2SO4    <==> Na+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
C
C SODIUM SULFATE
C
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-(WAER(2) + WAER(1))
         CC = WAER(1)*WAER(2) + 0.25*WAER(1)*WAER(1)
         DD =-0.25*(WAER(1)*WAER(1)*WAER(2) - A5)
         CALL POLY3(BB, CC, DD, ROOT3, ISLV)
         IF (ISLV.NE.0) ROOT3 = TINY
         ROOT3 = MIN (ROOT3, WAER(1)/2.0, WAER(2), CHI1)
         ROOT3 = MAX (ROOT3, ZERO)
         PSI1  = CHI1-ROOT3
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
C
C ION CONCENTRATIONS ; CORRECTIONS
C
      NAI = WAER(1) - 2.D0*ROOT3
      SO4I= WAER(2) - ROOT3
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
C
C SOLUTION ACIDIC OR BASIC?
C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I
      IF (GG.GT.TINY) THEN                        ! H+ in excess
         BB =-GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         HI = 0.5D0*(-BB + SQRT(DD))
         OHI= AKW/HI
      ELSE                                        ! OH- in excess
         BB = GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         OHI= 0.5D0*(-BB + SQRT(DD))
         HI = AKW/OHI
      ENDIF
C
C UNDISSOCIATED SPECIES EQUILIBRIA
C
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
         HSO4I = ZERO
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
C
C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
C
         CALL CALCHS4 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
C
C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
C
C *** CALCULATE WATER **************************************************
C
      CALL CALCMR
C
C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT
      ELSE
         IF (PSCONV1) GOTO 20
      ENDIF
10    CONTINUE
ccc      CALL PUSHERR (0002, 'CALCQ4')    ! WARNING ERROR: NO CONVERGENCE
C 
C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNACL   = ZERO
      CNANO3  = ZERO
      CNA2SO4 = CHI1 - PSI1
C
      RETURN
C
C *** END OF SUBROUTINE CALCQ4 ******************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCQ3
C *** CASE Q3
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
C     2. SOLID & LIQUID AEROSOL POSSIBLE
C     3. SOLIDS POSSIBLE : NH4CL, NA2SO4, NANO3, NACL
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCQ3
      INCLUDE 'isrpia.inc'
      LOGICAL EXNO, EXCL
      EXTERNAL CALCQ1A, CALCQ4
C
C *** REGIME DEPENDS ON AMBIENT RELATIVE HUMIDITY & POSSIBLE SPECIES ***
C
      EXNO = WAER(4).GT.TINY   
      EXCL = WAER(5).GT.TINY   
C
      IF (EXNO .OR. EXCL) THEN             ! *** NITRATE OR CHLORIDE EXISTS
         SCASE = 'Q3 ; SUBCASE 1'  
         CALL CALCQ3A                                   
         SCASE = 'Q3 ; SUBCASE 1' 
C
      ELSE                                 ! *** NO CHLORIDE AND NITRATE
         IF (RH.LT.DRMG3) THEN    
            SCASE = 'Q3 ; SUBCASE 2'  
            CALL CALCQ1A             ! SOLID
            SCASE = 'Q3 ; SUBCASE 2'
         ELSE
            SCASE = 'Q3 ; SUBCASE 3' ! MDRH (NH4)2SO4, NA2SO4
            CALL CALCMDRP (RH, DRMG3, DRNH42S4, CALCQ1A, CALCQ4)
            SCASE = 'Q3 ; SUBCASE 3'
         ENDIF
      ENDIF
C 
      RETURN
C
C *** END OF SUBROUTINE CALCQ3 ******************************************
C
      END



C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCQ3A
C *** CASE Q3 ; SUBCASE A
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0); SODIUM POOR (SODRAT < 2.0)
C     2. LIQUID AND SOLID PHASES ARE POSSIBLE
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCQ3A
      INCLUDE 'isrpia.inc'
C
      LOGICAL PSCONV1, PSCONV6
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** SETUP PARAMETERS ************************************************
C
      FRST    =.TRUE.
      CALAIN  =.TRUE. 
      CALAOU  =.TRUE.
C 
      PSCONV1 =.TRUE.
      PSCONV6 =.TRUE.
C
      PSI1O   =-GREAT
      PSI6O   =-GREAT
C
      ROOT1   = ZERO
      ROOT3   = ZERO
C
C *** CALCULATE INITIAL SOLUTION ***************************************
C
      CALL CALCQ1A
C
      CHI1   = CNA2SO4      ! SALTS
      CHI4   = CNH4CL
      CHI6   = CNH42S4
C
      PSI1   = CNA2SO4      ! AMOUNT DISSOLVED
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI6   = CNH42S4
C
      CALL CALCMR           ! WATER
C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
C
C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
C
      DO 10 I=1,NSWEEP
      A5  = XK5 *(WATER/GAMA(2))**3.         ! Na2SO4    <==> Na+
      A7  = XK7 *(WATER/GAMA(4))**3.         ! (NH4)2SO4 <==> Na+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
C
C SODIUM SULFATE
C
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-(WAER(2) + WAER(1) - ROOT1)
         CC = WAER(1)*(WAER(2) - ROOT1) + 0.25*WAER(1)*WAER(1)
         DD =-0.25*(WAER(1)*WAER(1)*(WAER(2) - ROOT1) - A5)
         CALL POLY3(BB, CC, DD, ROOT3, ISLV)
         IF (ISLV.NE.0) ROOT3 = TINY
         ROOT3 = MIN (ROOT3, WAER(1)/2.0, WAER(2) - ROOT1, CHI1)
         ROOT3 = MAX (ROOT3, ZERO)
         PSI1  = CHI1-ROOT3
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
C
C AMMONIUM SULFATE
C
      IF (NH4I*NH4I*SO4I .GT. A4) THEN
         BB =-(WAER(2)+WAER(3)-ROOT3)
         CC =  WAER(3)*(WAER(2)-ROOT3+0.5D0*WAER(3))
         DD =-((WAER(2)-ROOT3)*WAER(3)**2.D0 + A4)/4.D0
         CALL POLY3(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MIN(ROOT1, WAER(3), WAER(2)-ROOT3, CHI6)
         ROOT1 = MAX(ROOT1, ZERO)
         PSI6  = CHI6-ROOT1
      ENDIF
      PSCONV6 = ABS(PSI6-PSI6O) .LE. EPS*PSI6O
      PSI6O   = PSI6
C
C ION CONCENTRATIONS
C
      NAI = WAER(1) - 2.D0*ROOT3
      SO4I= WAER(2) - ROOT1 - ROOT3
      NH4I= WAER(3) - 2.D0*ROOT1
      NO3I= WAER(4)
      CLI = WAER(5)
C
C SOLUTION ACIDIC OR BASIC?
C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I
      IF (GG.GT.TINY) THEN                        ! H+ in excess
         BB =-GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         HI = 0.5D0*(-BB + SQRT(DD))
         OHI= AKW/HI
      ELSE                                        ! OH- in excess
         BB = GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         OHI= 0.5D0*(-BB + SQRT(DD))
         HI = AKW/OHI
      ENDIF
C
C UNDISSOCIATED SPECIES EQUILIBRIA
C
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
         HSO4I = ZERO
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
C
C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
C
         CALL CALCHS4 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
C
C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
C
C *** CALCULATE WATER **************************************************
C
      CALL CALCMR
C
C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT
      ELSE
         IF (PSCONV1 .AND. PSCONV6) GOTO 20      
      ENDIF
10    CONTINUE
ccc      CALL PUSHERR (0002, 'CALCQ3A')    ! WARNING ERROR: NO CONVERGENCE
C 
C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
C
      CNH42S4 = CHI6 - PSI6
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNACL   = ZERO
      CNANO3  = ZERO
      CNA2SO4 = CHI1 - PSI1
C
      RETURN
C
C *** END OF SUBROUTINE CALCQ3A *****************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCQ2
C *** CASE Q2
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
C     2. SOLID & LIQUID AEROSOL POSSIBLE
C     3. SOLIDS POSSIBLE : NH4CL, NA2SO4, NANO3, NACL
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCQ2
      INCLUDE 'isrpia.inc'
      LOGICAL EXNO, EXCL
      EXTERNAL CALCQ1A, CALCQ3A, CALCQ4
C
C *** REGIME DEPENDS ON AMBIENT RELATIVE HUMIDITY & POSSIBLE SPECIES ***
C
      EXNO = WAER(4).GT.TINY   
      EXCL = WAER(5).GT.TINY   
C
      IF (EXNO) THEN                       ! *** NITRATE EXISTS
         SCASE = 'Q2 ; SUBCASE 1'  
         CALL CALCQ2A                                   
         SCASE = 'Q2 ; SUBCASE 1' 
C 
      ELSEIF (.NOT.EXNO .AND. EXCL) THEN   ! *** ONLY CHLORIDE EXISTS
         IF (RH.LT.DRMG2) THEN    
            SCASE = 'Q2 ; SUBCASE 2'  
            CALL CALCQ1A             ! SOLID
            SCASE = 'Q2 ; SUBCASE 2'
         ELSE
            SCASE = 'Q2 ; SUBCASE 3' ! MDRH (NH4)2SO4, NA2SO4, NH4CL
            CALL CALCMDRP (RH, DRMG2, DRNH4CL, CALCQ1A, CALCQ3A)
            SCASE = 'Q2 ; SUBCASE 3'
         ENDIF
C
      ELSE                                 ! *** NO CHLORIDE AND NITRATE
         IF (RH.LT.DRMG3) THEN    
            SCASE = 'Q2 ; SUBCASE 2'  
            CALL CALCQ1A             ! SOLID
            SCASE = 'Q2 ; SUBCASE 2'
         ELSE
            SCASE = 'Q2 ; SUBCASE 4' ! MDRH (NH4)2SO4, NA2SO4
            CALL CALCMDRP (RH, DRMG3, DRNH42S4, CALCQ1A, CALCQ4)
            SCASE = 'Q2 ; SUBCASE 4'
         ENDIF
      ENDIF
C 
      RETURN
C
C *** END OF SUBROUTINE CALCQ2 ******************************************
C
      END


C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCQ2A
C *** CASE Q2 ; SUBCASE A
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0); SODIUM POOR (SODRAT < 2.0)
C     2. LIQUID AND SOLID PHASES ARE POSSIBLE
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCQ2A
      INCLUDE 'isrpia.inc'
C
      LOGICAL PSCONV1, PSCONV4, PSCONV6
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** SETUP PARAMETERS ************************************************
C
      FRST    =.TRUE.
      CALAIN  =.TRUE. 
      CALAOU  =.TRUE.
C 
      PSCONV1 =.TRUE.
      PSCONV4 =.TRUE.
      PSCONV6 =.TRUE.
C
      PSI1O   =-GREAT
      PSI4O   =-GREAT
      PSI6O   =-GREAT
C
      ROOT1   = ZERO
      ROOT2   = ZERO
      ROOT3   = ZERO
C
C *** CALCULATE INITIAL SOLUTION ***************************************
C
      CALL CALCQ1A
C
      CHI1   = CNA2SO4      ! SALTS
      CHI4   = CNH4CL
      CHI6   = CNH42S4
C
      PSI1   = CNA2SO4      ! AMOUNT DISSOLVED
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI6   = CNH42S4
C
      CALL CALCMR           ! WATER
C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
C
C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
C
      DO 10 I=1,NSWEEP
      A5  = XK5 *(WATER/GAMA(2))**3.         ! Na2SO4    <==> Na+
      A14 = XK14*(WATER/GAMA(6))**2.         ! NH4Cl     <==> NH4+
      A7  = XK7 *(WATER/GAMA(4))**3.         ! (NH4)2SO4 <==> Na+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
C
C AMMONIUM CHLORIDE
C
      IF (NH4I*CLI .GT. A14) THEN
         BB    =-(WAER(3) + WAER(5) - 2.D0*ROOT1)
         CC    = WAER(5)*(WAER(3) - 2.D0*ROOT1) - A14
         DD    = BB*BB - 4.D0*CC
         IF (DD.LT.ZERO) THEN
            ROOT2 = ZERO
         ELSE
            DD    = SQRT(DD)
            ROOT2A= 0.5D0*(-BB+DD)  
            ROOT2B= 0.5D0*(-BB-DD)  
            IF (ZERO.LE.ROOT2A) THEN
               ROOT2 = ROOT2A
            ELSE
               ROOT2 = ROOT2B
            ENDIF
            ROOT2 = MIN(ROOT2, WAER(5), WAER(3) - 2.D0*ROOT1, CHI4)
            ROOT2 = MAX(ROOT2, ZERO)
            PSI4  = CHI4 - ROOT2
         ENDIF
      ENDIF
      PSCONV4 = ABS(PSI4-PSI4O) .LE. EPS*PSI4O
      PSI4O   = PSI4
C
C SODIUM SULFATE
C
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-(WAER(2) + WAER(1) - ROOT1)
         CC = WAER(1)*(WAER(2) - ROOT1) + 0.25*WAER(1)*WAER(1)
         DD =-0.25*(WAER(1)*WAER(1)*(WAER(2) - ROOT1) - A5)
         CALL POLY3(BB, CC, DD, ROOT3, ISLV)
         IF (ISLV.NE.0) ROOT3 = TINY
         ROOT3 = MIN (ROOT3, WAER(1)/2.0, WAER(2) - ROOT1, CHI1)
         ROOT3 = MAX (ROOT3, ZERO)
         PSI1  = CHI1-ROOT3
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
C
C AMMONIUM SULFATE
C
      IF (NH4I*NH4I*SO4I .GT. A4) THEN
         BB =-(WAER(2)+WAER(3)-ROOT2-ROOT3)
         CC = (WAER(3)-ROOT2)*(WAER(2)-ROOT3+0.5D0*(WAER(3)-ROOT2))
         DD =-((WAER(2)-ROOT3)*(WAER(3)-ROOT2)**2.D0 + A4)/4.D0
         CALL POLY3(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MIN(ROOT1, WAER(3)-ROOT2, WAER(2)-ROOT3, CHI6)
         ROOT1 = MAX(ROOT1, ZERO)
         PSI6  = CHI6-ROOT1
      ENDIF
      PSCONV6 = ABS(PSI6-PSI6O) .LE. EPS*PSI6O
      PSI6O   = PSI6
C
C ION CONCENTRATIONS
C
      NAI = WAER(1) - 2.D0*ROOT3
      SO4I= WAER(2) - ROOT1 - ROOT3
      NH4I= WAER(3) - ROOT2 - 2.D0*ROOT1
      NO3I= WAER(4)
      CLI = WAER(5) - ROOT2
C
C SOLUTION ACIDIC OR BASIC?
C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I
      IF (GG.GT.TINY) THEN                        ! H+ in excess
         BB =-GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         HI = 0.5D0*(-BB + SQRT(DD))
         OHI= AKW/HI
      ELSE                                        ! OH- in excess
         BB = GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         OHI= 0.5D0*(-BB + SQRT(DD))
         HI = AKW/OHI
      ENDIF
C
C UNDISSOCIATED SPECIES EQUILIBRIA
C
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
         HSO4I = ZERO
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
C
C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
C
         CALL CALCHS4 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
C
C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
C
C *** CALCULATE WATER **************************************************
C
      CALL CALCMR
C
C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT
      ELSE
         IF (PSCONV1 .AND. PSCONV4 .AND. PSCONV6) GOTO 20
      ENDIF      
10    CONTINUE
ccc      CALL PUSHERR (0002, 'CALCQ2A')    ! WARNING ERROR: NO CONVERGENCE
C 
C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
C
      CNH42S4 = CHI6 - PSI6
      CNH4NO3 = ZERO
      CNH4CL  = CHI4 - PSI4
      CNACL   = ZERO
      CNANO3  = ZERO
      CNA2SO4 = CHI1 - PSI1
C
      RETURN
C
C *** END OF SUBROUTINE CALCQ2A *****************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCQ1
C *** CASE Q1
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
C     2. SOLID AEROSOL ONLY
C     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, (NH4)2SO4, NA2SO4
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCQ1
      INCLUDE 'isrpia.inc'
      LOGICAL EXNO, EXCL
      EXTERNAL CALCQ1A, CALCQ2A, CALCQ3A, CALCQ4
C
C *** REGIME DEPENDS ON AMBIENT RELATIVE HUMIDITY & POSSIBLE SPECIES ***
C
      EXNO = WAER(4).GT.TINY   
      EXCL = WAER(5).GT.TINY   
C
      IF (EXNO .AND. EXCL) THEN           ! *** NITRATE & CHLORIDE EXIST
         IF (RH.LT.DRMG1) THEN    
            SCASE = 'Q1 ; SUBCASE 1'  
            CALL CALCQ1A             ! SOLID
            SCASE = 'Q1 ; SUBCASE 1'
         ELSE
            SCASE = 'Q1 ; SUBCASE 2' ! MDRH (NH4)2SO4, NA2SO4, NH4CL, NH4NO3
            CALL CALCMDRP (RH, DRMG1, DRNH4NO3, CALCQ1A, CALCQ2A)
            SCASE = 'Q1 ; SUBCASE 2'
         ENDIF
C
      ELSE IF (EXNO .AND. .NOT.EXCL) THEN ! *** ONLY NITRATE EXISTS
         IF (RH.LT.DRMQ1) THEN    
            SCASE = 'Q1 ; SUBCASE 1'  
            CALL CALCQ1A             ! SOLID
            SCASE = 'Q1 ; SUBCASE 1'
         ELSE
            SCASE = 'Q1 ; SUBCASE 3' ! MDRH (NH4)2SO4, NA2SO4, NH4NO3
            CALL CALCMDRP (RH, DRMQ1, DRNH4NO3, CALCQ1A, CALCQ2A)
            SCASE = 'Q1 ; SUBCASE 3'
         ENDIF
C
      ELSE IF (.NOT.EXNO .AND. EXCL) THEN ! *** ONLY CHLORIDE EXISTS
         IF (RH.LT.DRMG2) THEN    
            SCASE = 'Q1 ; SUBCASE 1'  
            CALL CALCQ1A             ! SOLID
            SCASE = 'Q1 ; SUBCASE 1'
         ELSE
            SCASE = 'Q1 ; SUBCASE 4' ! MDRH (NH4)2SO4, NA2SO4, NH4CL
            CALL CALCMDRP (RH, DRMG2, DRNH4CL, CALCQ1A, CALCQ3A)
            SCASE = 'Q1 ; SUBCASE 4'
         ENDIF
C
      ELSE                                ! *** NO CHLORIDE AND NITRATE
         IF (RH.LT.DRMG3) THEN    
            SCASE = 'Q1 ; SUBCASE 1'  
            CALL CALCQ1A             ! SOLID
            SCASE = 'Q1 ; SUBCASE 1'
         ELSE
            SCASE = 'Q1 ; SUBCASE 5' ! MDRH (NH4)2SO4, NA2SO4
            CALL CALCMDRP (RH, DRMG3, DRNH42S4, CALCQ1A, CALCQ4)
            SCASE = 'Q1 ; SUBCASE 5'
         ENDIF
      ENDIF
C 
      RETURN
C
C *** END OF SUBROUTINE CALCQ1 ******************************************
C
      END


C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCQ1A
C *** CASE Q1 ; SUBCASE 1
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
C     2. SOLID AEROSOL ONLY
C     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, (NH4)2SO4, NA2SO4
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCQ1A
      INCLUDE 'isrpia.inc'
C
C *** CALCULATE SOLIDS **************************************************
C
      CNA2SO4 = 0.5d0*WAER(1)
      FRSO4   = MAX (WAER(2)-CNA2SO4, ZERO)
C
      CNH42S4 = MAX (MIN(FRSO4,0.5d0*WAER(3)), TINY)
      FRNH3   = MAX (WAER(3)-2.D0*CNH42S4, ZERO)
C
      CNH4NO3 = MIN (FRNH3, WAER(4))
CCC      FRNO3   = MAX (WAER(4)-CNH4NO3, ZERO)
      FRNH3   = MAX (FRNH3-CNH4NO3, ZERO)
C
      CNH4CL  = MIN (FRNH3, WAER(5))
CCC      FRCL    = MAX (WAER(5)-CNH4CL, ZERO)
      FRNH3   = MAX (FRNH3-CNH4CL, ZERO)
C
C *** OTHER PHASES ******************************************************
C
      WATER   = ZERO
C
      GNH3    = ZERO
      GHNO3   = ZERO
      GHCL    = ZERO
C
      RETURN
C
C *** END OF SUBROUTINE CALCQ1A *****************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCR6
C *** CASE R6
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0); SODIUM RICH (SODRAT >= 2.0)
C     2. THERE IS ONLY A LIQUID PHASE
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCR6
      INCLUDE 'isrpia.inc'
C
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** SETUP PARAMETERS ************************************************
C
      CALL CALCR1A
C
      PSI1   = CNA2SO4
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
C
      FRST   = .TRUE.
      CALAIN = .TRUE. 
      CALAOU = .TRUE. 
C
C *** CALCULATE WATER **************************************************
C
      CALL CALCMR
C
C *** SETUP LIQUID CONCENTRATIONS **************************************
C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
C
C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
C
      DO 10 I=1,NSWEEP
      AKW = XKW*RH*WATER*WATER                        ! H2O    <==> H+      
C
      NAI    = WAER(1)
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
C
C SOLUTION ACIDIC OR BASIC?
C
      GG  = 2.D0*WAER(2) + NO3I + CLI - NAI - NH4I
      IF (GG.GT.TINY) THEN                        ! H+ in excess
         BB =-GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         HI = 0.5D0*(-BB + SQRT(DD))
         OHI= AKW/HI
      ELSE                                        ! OH- in excess
         BB = GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         OHI= 0.5D0*(-BB + SQRT(DD))
         HI = AKW/OHI
      ENDIF
C
C UNDISSOCIATED SPECIES EQUILIBRIA
C
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
C
C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
C
         CALL CALCHS4 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
C
C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
C
C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
ccc      CALL PUSHERR (0002, 'CALCR6')    ! WARNING ERROR: NO CONVERGENCE
C 
C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
C
20    A2       = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3       = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4       = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
C
      GNH3     = NH4I/HI/A2
      GHNO3    = HI*NO3I/A3
      GHCL     = HI*CLI /A4
C
      GASAQ(1) = NH3AQ
      GASAQ(2) = CLAQ
      GASAQ(3) = NO3AQ
C
      CNH42S4  = ZERO
      CNH4NO3  = ZERO
      CNH4CL   = ZERO
      CNACL    = ZERO
      CNANO3   = ZERO
      CNA2SO4  = ZERO 
C
      RETURN
C
C *** END OF SUBROUTINE CALCR6 ******************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCR5
C *** CASE R5
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0); SODIUM RICH (SODRAT >= 2.0)
C     2. LIQUID AND SOLID PHASES ARE POSSIBLE
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCR5
      INCLUDE 'isrpia.inc'
C
      LOGICAL PSCONV
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
      LOGICAL  NEAN, NEAC, NESN, NESC
C
C *** SETUP PARAMETERS ************************************************
C
      CALL CALCR1A                             ! DRY SOLUTION
C
      NEAN = CNH4NO3.LE.TINY    ! NH4NO3       ! Water exists?
      NEAC = CNH4CL .LE.TINY    ! NH4CL
      NESN = CNANO3 .LE.TINY    ! NANO3
      NESC = CNACL  .LE.TINY    ! NACL
      IF (NEAN .AND. NEAC .AND. NESN .AND. NESC) RETURN
C
      CHI1   = CNA2SO4
C
      PSI1   = CNA2SO4
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
C
      PSIO   =-GREAT
C
C *** CALCULATE WATER **************************************************
C
      CALL CALCMR
C
      FRST   = .TRUE.
      CALAIN = .TRUE. 
      CALAOU = .TRUE. 
      PSCONV = .FALSE.
C
C *** SETUP LIQUID CONCENTRATIONS **************************************
C
      NAI    = WAER(1)
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
C
C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
C
      DO 10 I=1,NSWEEP
      A5  = XK5*(WATER/GAMA(2))**3.                   ! Na2SO4 <==> Na+
      AKW = XKW*RH*WATER*WATER                        ! H2O    <==> H+
C
C SODIUM SULFATE
C
      ROOT = ZERO
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-3.D0*CHI1
         CC = 3.D0*CHI1**2.0
         DD =-CHI1**3.0 + 0.25D0*A5 
         CALL POLY3(BB, CC, DD, ROOT, ISLV)
         IF (ISLV.NE.0) ROOT = TINY
         ROOT = MIN (MAX(ROOT,ZERO), CHI1)
         PSI1 = CHI1-ROOT
      ENDIF
      PSCONV = ABS(PSI1-PSIO) .LE. EPS*PSIO
      PSIO   = PSI1
C
C ION CONCENTRATIONS
C
      NAI  = WAER(1) - 2.D0*ROOT
      SO4I = WAER(2) - ROOT
      NH4I = WAER(3)
      NO3I = WAER(4)
      CLI  = WAER(5)
C
C SOLUTION ACIDIC OR BASIC?
C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I
      IF (GG.GT.TINY) THEN                        ! H+ in excess
         BB =-GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         HI = 0.5D0*(-BB + SQRT(DD))
         OHI= AKW/HI
      ELSE                                        ! OH- in excess
         BB = GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         OHI= 0.5D0*(-BB + SQRT(DD))
         HI = AKW/OHI
      ENDIF
C
C UNDISSOCIATED SPECIES EQUILIBRIA
C
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
C
C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
C
         CALL CALCHS4 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
C
C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
C
C *** CALCULATE WATER **************************************************
C
      CALL CALCMR
C
C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT
      ELSE
         IF (PSCONV) GOTO 20
      ENDIF
10    CONTINUE
ccc      CALL PUSHERR (0002, 'CALCR5')    ! WARNING ERROR: NO CONVERGENCE
C 
C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
C
20    A2       = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
CC      A21      = XK21*WATER*R*TEMP
      A3       = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4       = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
C
      GNH3     = NH4I/HI/A2  ! NH4I*OHI/A2/AKW
      GHNO3    = HI*NO3I/A3
      GHCL     = HI*CLI /A4
C
      GASAQ(1) = NH3AQ
      GASAQ(2) = CLAQ
      GASAQ(3) = NO3AQ
C
      CNH42S4  = ZERO
      CNH4NO3  = ZERO
      CNH4CL   = ZERO
      CNACL    = ZERO
      CNANO3   = ZERO
      CNA2SO4  = CHI1 - PSI1
C
      RETURN
C
C *** END OF SUBROUTINE CALCR5 ******************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCR4
C *** CASE R4
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
C     2. SOLID AEROSOL ONLY
C     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NA2SO4, NANO3, NACL
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCR4
      INCLUDE 'isrpia.inc'
      LOGICAL  EXAN, EXAC, EXSN, EXSC
      EXTERNAL CALCR1A, CALCR5
C
C *** SOLVE FOR DRY CASE AND SEE WHICH SOLIDS ARE POSSIBLE **************
C
      SCASE = 'R4 ; SUBCASE 2'  
      CALL CALCR1A              ! SOLID
      SCASE = 'R4 ; SUBCASE 2'
C     
      EXAN = CNH4NO3.GT.TINY    ! NH4NO3
      EXAC = CNH4CL .GT.TINY    ! NH4CL
      EXSN = CNANO3 .GT.TINY    ! NANO3
      EXSC = CNACL  .GT.TINY    ! NACL
C
C *** REGIME DEPENDS ON RELATIVE HUMIDITY AND POSSIBLE SPECIES **********
C
      IF (EXAN .OR. EXSN .OR. EXSC) THEN   ! *** NH4NO3,NANO3 EXIST
         IF (RH.GE.DRMH1) THEN    
            SCASE = 'R4 ; SUBCASE 1' 
            CALL CALCR4A
            SCASE = 'R4 ; SUBCASE 1'
         ENDIF
C
      ELSE IF (EXAC) THEN                  ! *** NH4CL EXISTS ONLY
         IF (RH.GE.DRMR5) THEN    
            SCASE = 'R4 ; SUBCASE 3'  
            CALL CALCMDRP (RH, DRMR5, DRNH4CL, CALCR1A, CALCR5)
            SCASE = 'R4 ; SUBCASE 3'
         ENDIF
      ENDIF
C 
      RETURN
C
C *** END OF SUBROUTINE CALCR4 ******************************************
C
      END



C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCR4A
C *** CASE R4A
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0); SODIUM RICH (SODRAT >= 2.0)
C     2. LIQUID AND SOLID PHASES ARE POSSIBLE
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCR4A
      INCLUDE 'isrpia.inc'
C
      LOGICAL PSCONV1, PSCONV4
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** SETUP PARAMETERS ************************************************
C
      FRST    = .TRUE.
      CALAIN  = .TRUE. 
      CALAOU  = .TRUE. 
      PSCONV1 = .FALSE.
      PSCONV4 = .FALSE.
      PSIO1   =-GREAT
      PSIO4   =-GREAT
C
C *** CALCULATE INITIAL SOLUTION ***************************************
C
      CALL CALCR1A
C
      CHI1   = CNA2SO4      ! SALTS
      CHI4   = CNH4CL
C
      PSI1   = CNA2SO4
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
C
      CALL CALCMR           ! WATER
C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
C
C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
C
      DO 10 I=1,NSWEEP
      A5  = XK5 *(WATER/GAMA(2))**3.                  ! Na2SO4 <==> Na+
      A14 = XK14*(WATER/GAMA(6))**2.                  ! NH4Cl  <==> NH4+
      AKW = XKW*RH*WATER*WATER                        ! H2O    <==> H+
C
C SODIUM SULFATE
C
      ROOT = ZERO
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-3.D0*CHI1
         CC = 3.D0*CHI1**2.0
         DD =-CHI1**3.0 + 0.25D0*A5 
         CALL POLY3(BB, CC, DD, ROOT, ISLV)
         IF (ISLV.NE.0) ROOT = TINY
         ROOT = MIN (MAX(ROOT,ZERO), CHI1)
         PSI1 = CHI1-ROOT
         NAI  = WAER(1) - 2.D0*ROOT
         SO4I = WAER(2) - ROOT
      ENDIF
      PSCONV1 = ABS(PSI1-PSIO1) .LE. EPS*PSIO1
      PSIO1   = PSI1
C
C AMMONIUM CHLORIDE
C
      ROOT = ZERO
      IF (NH4I*CLI .GT. A14) THEN
         BB   =-(NH4I + CLI)
         CC   =-A14 + NH4I*CLI
         DD   = BB*BB - 4.D0*CC
         ROOT = 0.5D0*(-BB-SQRT(DD)) 
         IF (ROOT.GT.TINY) THEN
            ROOT    = MIN(ROOT, CHI4)
            PSI4    = CHI4 - ROOT
            NH4I    = WAER(3) - ROOT
            CLI     = WAER(5) - ROOT
         ENDIF
      ENDIF
      PSCONV4 = ABS(PSI4-PSIO4) .LE. EPS*PSIO4
      PSIO4   = PSI4
C
      NO3I   = WAER(4)
C
C SOLUTION ACIDIC OR BASIC?
C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I
      IF (GG.GT.TINY) THEN                        ! H+ in excess
         BB =-GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         HI = 0.5D0*(-BB + SQRT(DD))
         OHI= AKW/HI
      ELSE                                        ! OH- in excess
         BB = GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         OHI= 0.5D0*(-BB + SQRT(DD))
         HI = AKW/OHI
      ENDIF
C
C UNDISSOCIATED SPECIES EQUILIBRIA
C
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
C
C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
C
         CALL CALCHS4 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
C
C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
C
C *** CALCULATE WATER **************************************************
C
      CALL CALCMR
C
C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT
      ELSE
         IF (PSCONV1 .AND. PSCONV4) GOTO 20
      ENDIF
10    CONTINUE
ccc      CALL PUSHERR (0002, 'CALCR4A')    ! WARNING ERROR: NO CONVERGENCE
C 
C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = CHI4 - PSI4
      CNACL   = ZERO
      CNANO3  = ZERO
      CNA2SO4 = CHI1 - PSI1
C
      RETURN
C
C *** END OF SUBROUTINE CALCR4A *****************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCR3
C *** CASE R3
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
C     2. SOLID AEROSOL ONLY
C     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NA2SO4, NANO3, NACL
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCR3
      INCLUDE 'isrpia.inc'
      LOGICAL  EXAN, EXAC, EXSN, EXSC
      EXTERNAL CALCR1A, CALCR4A, CALCR5
C
C *** SOLVE FOR DRY CASE AND SEE WHICH SOLIDS ARE POSSIBLE **************
C
      SCASE = 'R3 ; SUBCASE 2'  
      CALL CALCR1A              ! SOLID
      SCASE = 'R3 ; SUBCASE 2'
C     
      EXAN = CNH4NO3.GT.TINY    ! NH4NO3
      EXAC = CNH4CL .GT.TINY    ! NH4CL
      EXSN = CNANO3 .GT.TINY    ! NANO3
      EXSC = CNACL  .GT.TINY    ! NACL
C
C *** REGIME DEPENDS ON RELATIVE HUMIDITY AND POSSIBLE SPECIES **********
C
      IF (EXAN .OR. EXSN) THEN                   ! *** NH4NO3,NANO3 EXIST
         IF (RH.GE.DRMH1) THEN    
            SCASE = 'R3 ; SUBCASE 1' 
            CALL CALCR3A
            SCASE = 'R3 ; SUBCASE 1'
         ENDIF
C
      ELSE IF (.NOT.EXAN .AND. .NOT.EXSN) THEN   ! *** NH4NO3,NANO3 = 0
         IF      (     EXAC .AND.      EXSC) THEN
            IF (RH.GE.DRMR4) THEN    
               SCASE = 'R3 ; SUBCASE 3'  
               CALL CALCMDRP (RH, DRMR4, DRNACL, CALCR1A, CALCR4A)
               SCASE = 'R3 ; SUBCASE 3'
            ENDIF

         ELSE IF (.NOT.EXAC .AND.      EXSC) THEN
            IF (RH.GE.DRMR2) THEN    
               SCASE = 'R3 ; SUBCASE 4'  
               CALL CALCMDRP (RH, DRMR2, DRNACL, CALCR1A, CALCR4A)
               SCASE = 'R3 ; SUBCASE 4'
            ENDIF

         ELSE IF (     EXAC .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR5) THEN    
               SCASE = 'R3 ; SUBCASE 5'  
               CALL CALCMDRP (RH, DRMR5, DRNACL, CALCR1A, CALCR5)
               SCASE = 'R3 ; SUBCASE 5'
            ENDIF
         ENDIF
C
      ENDIF
C 
      RETURN
C
C *** END OF SUBROUTINE CALCR3 ******************************************
C
      END


C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCR3A
C *** CASE R3A
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0); SODIUM RICH (SODRAT >= 2.0)
C     2. LIQUID AND SOLID PHASES ARE POSSIBLE
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCR3A
      INCLUDE 'isrpia.inc'
C
      LOGICAL PSCONV1, PSCONV3, PSCONV4
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** SETUP PARAMETERS ************************************************
C
      FRST    =.TRUE.
      CALAIN  =.TRUE. 
      CALAOU  =.TRUE. 
      PSCONV1 =.TRUE.
      PSCONV3 =.TRUE.
      PSCONV4 =.TRUE.
      PSI1O   =-GREAT
      PSI3O   =-GREAT
      PSI4O   =-GREAT
      ROOT1   = ZERO
      ROOT2   = ZERO
      ROOT3   = ZERO
C
C *** CALCULATE INITIAL SOLUTION ***************************************
C
      CALL CALCR1A
C
      CHI1   = CNA2SO4      ! SALTS
      CHI4   = CNH4CL
      CHI3   = CNACL
C
      PSI1   = CNA2SO4
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
C
      CALL CALCMR           ! WATER
C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
C
      MOLAL(1) = ZERO
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
C
      CALL CALCACT          ! CALCULATE ACTIVITY COEFFICIENTS
C
C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
C
      DO 10 I=1,NSWEEP
      A5  = XK5 *(WATER/GAMA(2))**3.                  ! Na2SO4 <==> Na+
      A8  = XK8 *(WATER/GAMA(1))**2.                  ! NaCl   <==> Na+
      A14 = XK14*(WATER/GAMA(6))**2.                  ! NH4Cl  <==> NH4+
      AKW = XKW*RH*WATER*WATER                        ! H2O    <==> H+
C
C AMMONIUM CHLORIDE
C
      IF (NH4I*CLI .GT. A14) THEN
         BB    =-(WAER(3) + WAER(5) - ROOT3)
         CC    =-A14 + NH4I*(WAER(5) - ROOT3)
         DD    = MAX(BB*BB - 4.D0*CC, ZERO)
         ROOT2A= 0.5D0*(-BB+SQRT(DD))  
         ROOT2B= 0.5D0*(-BB-SQRT(DD))  
         IF (ZERO.LE.ROOT2A) THEN
            ROOT2 = ROOT2A
         ELSE
            ROOT2 = ROOT2B
         ENDIF
         ROOT2 = MIN(MAX(ZERO, ROOT2), MAX(WAER(5)-ROOT3,ZERO), 
     &               CHI4, WAER(3))
         PSI4  = CHI4 - ROOT2
      ENDIF
      PSCONV4 = ABS(PSI4-PSI4O) .LE. EPS*PSI4O
      PSI4O   = PSI4
C
C SODIUM SULFATE
C
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-(CHI1 + WAER(1) - ROOT3)
         CC = 0.25D0*(WAER(1) - ROOT3)*(4.D0*CHI1+WAER(1)-ROOT3)
         DD =-0.25D0*(CHI1*(WAER(1)-ROOT3)**2.D0 - A5) 
         CALL POLY3(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MIN (MAX(ROOT1,ZERO), MAX(WAER(1)-ROOT3,ZERO), 
     &                CHI1, WAER(2))
         PSI1  = CHI1-ROOT1
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
C
C ION CONCENTRATIONS
C
      NAI = WAER(1) - (2.D0*ROOT1 + ROOT3)
      SO4I= WAER(2) - ROOT1
      NH4I= WAER(3) - ROOT2
      CLI = WAER(5) - (ROOT3 + ROOT2)
      NO3I= WAER(4)
C
C SODIUM CHLORIDE  ; To obtain new value for ROOT3
C
      IF (NAI*CLI .GT. A8) THEN
         BB    =-((CHI1-2.D0*ROOT1) + (WAER(5) - ROOT2))
         CC    = (CHI1-2.D0*ROOT1)*(WAER(5) - ROOT2) - A8
         DD    = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT3A= 0.5D0*(-BB-SQRT(DD)) 
         ROOT3B= 0.5D0*(-BB+SQRT(DD)) 
         IF (ZERO.LE.ROOT3A) THEN
            ROOT3 = ROOT3A
         ELSE
            ROOT3 = ROOT3B
         ENDIF
         ROOT3   = MIN(MAX(ROOT3, ZERO), CHI3)
         PSI3    = CHI3-ROOT3
      ENDIF
      PSCONV3 = ABS(PSI3-PSI3O) .LE. EPS*PSI3O
      PSI3O   = PSI3
C
C SOLUTION ACIDIC OR BASIC?
C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I
      IF (GG.GT.TINY) THEN                        ! H+ in excess
         BB =-GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         HI = 0.5D0*(-BB + SQRT(DD))
         OHI= AKW/HI
      ELSE                                        ! OH- in excess
         BB = GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         OHI= 0.5D0*(-BB + SQRT(DD))
         HI = AKW/OHI
      ENDIF
C
C UNDISSOCIATED SPECIES EQUILIBRIA
C
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
C
C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
C
         CALL CALCHS4 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
C
C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
C
C *** CALCULATE WATER **************************************************
C
      CALL CALCMR
C
C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT
      ELSE
         IF (PSCONV1.AND.PSCONV3.AND.PSCONV4) GOTO 20
      ENDIF
10    CONTINUE
ccc      CALL PUSHERR (0002, 'CALCR3A')    ! WARNING ERROR: NO CONVERGENCE
C 
C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
C
20    IF (CLI.LE.TINY .AND. WAER(5).GT.TINY) THEN !No disslv Cl-;solid only
         DO 30 I=1,NIONS
            MOLAL(I) = ZERO
30       CONTINUE
         DO 40 I=1,NGASAQ
            GASAQ(I) = ZERO
40       CONTINUE
         CALL CALCR1A
      ELSE
         A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
         A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
         A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
C
         GNH3    = NH4I/HI/A2
         GHNO3   = HI*NO3I/A3
         GHCL    = HI*CLI /A4
C
         GASAQ(1)= NH3AQ
         GASAQ(2)= CLAQ
         GASAQ(3)= NO3AQ
C
         CNH42S4 = ZERO
         CNH4NO3 = ZERO
         CNH4CL  = CHI4 - PSI4
         CNACL   = CHI3 - PSI3
         CNANO3  = ZERO
         CNA2SO4 = CHI1 - PSI1
      ENDIF
C
      RETURN
C
C *** END OF SUBROUTINE CALCR3A *****************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCR2
C *** CASE R2
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
C     2. SOLID AEROSOL ONLY
C     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NA2SO4, NANO3, NACL
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCR2
      INCLUDE 'isrpia.inc'
      LOGICAL  EXAN, EXAC, EXSN, EXSC
      EXTERNAL CALCR1A, CALCR3A, CALCR4A, CALCR5
C
C *** SOLVE FOR DRY CASE AND SEE WHICH SOLIDS ARE POSSIBLE **************
C
      SCASE = 'R2 ; SUBCASE 2'  
      CALL CALCR1A              ! SOLID
      SCASE = 'R2 ; SUBCASE 2'
C     
      EXAN = CNH4NO3.GT.TINY    ! NH4NO3
      EXAC = CNH4CL .GT.TINY    ! NH4CL
      EXSN = CNANO3 .GT.TINY    ! NANO3
      EXSC = CNACL  .GT.TINY    ! NACL
C
C *** REGIME DEPENDS ON RELATIVE HUMIDITY AND POSSIBLE SPECIES **********
C
      IF (EXAN) THEN                             ! *** NH4NO3 EXISTS
         IF (RH.GE.DRMH1) THEN    
            SCASE = 'R2 ; SUBCASE 1' 
            CALL CALCR2A
            SCASE = 'R2 ; SUBCASE 1'
         ENDIF
C
      ELSE IF (.NOT.EXAN) THEN                   ! *** NH4NO3 = 0
         IF      (     EXAC .AND.      EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMH2) THEN    
               SCASE = 'R2 ; SUBCASE 3'  
               CALL CALCMDRP (RH, DRMH2, DRNANO3, CALCR1A, CALCR3A)
               SCASE = 'R2 ; SUBCASE 3'
            ENDIF

         ELSE IF (.NOT.EXAC .AND.      EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR1) THEN    
               SCASE = 'R2 ; SUBCASE 4'  
               CALL CALCMDRP (RH, DRMR1, DRNANO3, CALCR1A, CALCR3A)
               SCASE = 'R2 ; SUBCASE 4'
            ENDIF

         ELSE IF (.NOT.EXAC .AND. .NOT.EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR2) THEN    
               SCASE = 'R2 ; SUBCASE 5'  
               CALL CALCMDRP (RH, DRMR2, DRNACL, CALCR1A, CALCR4A)
               SCASE = 'R2 ; SUBCASE 5'
            ENDIF

         ELSE IF (.NOT.EXAC .AND.      EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR3) THEN    
               SCASE = 'R2 ; SUBCASE 6'  
               CALL CALCMDRP (RH, DRMR3, DRNANO3, CALCR1A, CALCR3A)
               SCASE = 'R2 ; SUBCASE 6'
            ENDIF

         ELSE IF (     EXAC .AND. .NOT.EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR4) THEN    
               SCASE = 'R2 ; SUBCASE 7'  
               CALL CALCMDRP (RH, DRMR4, DRNACL, CALCR1A, CALCR4A)
               SCASE = 'R2 ; SUBCASE 7'
            ENDIF

         ELSE IF (     EXAC .AND. .NOT.EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR5) THEN    
               SCASE = 'R2 ; SUBCASE 8'  
               CALL CALCMDRP (RH, DRMR5, DRNH4CL, CALCR1A, CALCR5)
               SCASE = 'R2 ; SUBCASE 8'
            ENDIF

         ELSE IF (     EXAC .AND.      EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR6) THEN    
               SCASE = 'R2 ; SUBCASE 9'  
               CALL CALCMDRP (RH, DRMR6, DRNANO3, CALCR1A, CALCR3A)
               SCASE = 'R2 ; SUBCASE 9'
            ENDIF
         ENDIF
C
      ENDIF
C 
      RETURN
C
C *** END OF SUBROUTINE CALCR2 ******************************************
C
      END


C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCR2A
C *** CASE R2A
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0); SODIUM RICH (SODRAT >= 2.0)
C     2. LIQUID AND SOLID PHASES ARE POSSIBLE
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCR2A
      INCLUDE 'isrpia.inc'
C
      LOGICAL PSCONV1, PSCONV2, PSCONV3, PSCONV4
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** SETUP PARAMETERS ************************************************
C
      FRST    =.TRUE.
      CALAIN  =.TRUE. 
      CALAOU  =.TRUE.
C 
      PSCONV1 =.TRUE.
      PSCONV2 =.TRUE.
      PSCONV3 =.TRUE.
      PSCONV4 =.TRUE.
C
      PSI1O   =-GREAT
      PSI2O   =-GREAT
      PSI3O   =-GREAT
      PSI4O   =-GREAT
C
      ROOT1   = ZERO
      ROOT2   = ZERO
      ROOT3   = ZERO
      ROOT4   = ZERO
C
C *** CALCULATE INITIAL SOLUTION ***************************************
C
      CALL CALCR1A
C
      CHI1   = CNA2SO4      ! SALTS
      CHI2   = CNANO3
      CHI3   = CNACL
      CHI4   = CNH4CL
C
      PSI1   = CNA2SO4
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
C
      CALL CALCMR           ! WATER
C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
C
      MOLAL(1) = ZERO
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
C
      CALL CALCACT          ! CALCULATE ACTIVITY COEFFICIENTS
C
C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
C
      DO 10 I=1,NSWEEP
      A5  = XK5 *(WATER/GAMA(2))**3.                  ! Na2SO4 <==> Na+
      A8  = XK8 *(WATER/GAMA(1))**2.                  ! NaCl   <==> Na+
      A9  = XK9 *(WATER/GAMA(3))**2.                  ! NaNO3  <==> Na+
      A14 = XK14*(WATER/GAMA(6))**2.                  ! NH4Cl  <==> NH4+
      AKW = XKW*RH*WATER*WATER                        ! H2O    <==> H+
C
C AMMONIUM CHLORIDE
C
      IF (NH4I*CLI .GT. A14) THEN
         BB    =-(WAER(3) + WAER(5) - ROOT3)
         CC    = NH4I*(WAER(5) - ROOT3) - A14
         DD    = MAX(BB*BB - 4.D0*CC, ZERO)
         DD    = SQRT(DD)
         ROOT2A= 0.5D0*(-BB+DD)  
         ROOT2B= 0.5D0*(-BB-DD)  
         IF (ZERO.LE.ROOT2A) THEN
            ROOT2 = ROOT2A
         ELSE
            ROOT2 = ROOT2B
         ENDIF
         ROOT2 = MIN(MAX(ROOT2, ZERO), CHI4)
         PSI4  = CHI4 - ROOT2
      ENDIF
      PSCONV4 = ABS(PSI4-PSI4O) .LE. EPS*PSI4O
      PSI4O   = PSI4
C
C SODIUM SULFATE
C
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-(WAER(2) + WAER(1) - ROOT3 - ROOT4)
         CC = WAER(1)*(2.D0*ROOT3 + 2.D0*ROOT4 - 4.D0*WAER(2) - ONE)
     &       -(ROOT3 + ROOT4)**2.0 + 4.D0*WAER(2)*(ROOT3 + ROOT4)
         CC =-0.25*CC
         DD = WAER(1)*WAER(2)*(ONE - 2.D0*ROOT3 - 2.D0*ROOT4) +
     &        WAER(2)*(ROOT3 + ROOT4)**2.0 - A5
         DD =-0.25*DD
         CALL POLY3(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MIN (MAX(ROOT1,ZERO), CHI1)
         PSI1  = CHI1-ROOT1
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
C
C SODIUM NITRATE
C
      IF (NAI*NO3I .GT. A9) THEN
         BB    =-(WAER(4) + WAER(1) - 2.D0*ROOT1 - ROOT3)
         CC    = WAER(4)*(WAER(1) - 2.D0*ROOT1 - ROOT3) - A9
         DD    = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT4A= 0.5D0*(-BB-DD) 
         ROOT4B= 0.5D0*(-BB+DD) 
         IF (ZERO.LE.ROOT4A) THEN
            ROOT4 = ROOT4A
         ELSE
            ROOT4 = ROOT4B
         ENDIF
         ROOT4 = MIN(MAX(ROOT4, ZERO), CHI2)
         PSI2  = CHI2-ROOT4
      ENDIF
      PSCONV2 = ABS(PSI2-PSI2O) .LE. EPS*PSI2O
      PSI2O   = PSI2
C
C ION CONCENTRATIONS
C
      NAI = WAER(1) - (2.D0*ROOT1 + ROOT3 + ROOT4)
      SO4I= WAER(2) - ROOT1
      NH4I= WAER(3) - ROOT2
      NO3I= WAER(4) - ROOT4
      CLI = WAER(5) - (ROOT3 + ROOT2)
C
C SODIUM CHLORIDE  ; To obtain new value for ROOT3
C
      IF (NAI*CLI .GT. A8) THEN
         BB    =-(WAER(1) - 2.D0*ROOT1 + WAER(5) - ROOT2 - ROOT4)
         CC    = (WAER(5) + ROOT2)*(WAER(1) - 2.D0*ROOT1 - ROOT4) - A8
         DD    = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT3A= 0.5D0*(-BB-DD) 
         ROOT3B= 0.5D0*(-BB+DD) 
         IF (ZERO.LE.ROOT3A) THEN
            ROOT3 = ROOT3A
         ELSE
            ROOT3 = ROOT3B
         ENDIF
         ROOT3   = MIN(MAX(ROOT3, ZERO), CHI3)
         PSI3    = CHI3-ROOT3
      ENDIF
      PSCONV3 = ABS(PSI3-PSI3O) .LE. EPS*PSI3O
      PSI3O   = PSI3
C
C SOLUTION ACIDIC OR BASIC?
C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I
      IF (GG.GT.TINY) THEN                        ! H+ in excess
         BB =-GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         HI = 0.5D0*(-BB + SQRT(DD))
         OHI= AKW/HI
      ELSE                                        ! OH- in excess
         BB = GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         OHI= 0.5D0*(-BB + SQRT(DD))
         HI = AKW/OHI
      ENDIF
C
C UNDISSOCIATED SPECIES EQUILIBRIA
C
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
C
C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
C
         CALL CALCHS4 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
C
C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
C
C *** CALCULATE WATER **************************************************
C
      CALL CALCMR
C
C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT
      ELSE
         IF (PSCONV1.AND.PSCONV2.AND.PSCONV3.AND.PSCONV4) GOTO 20
      ENDIF      
10    CONTINUE
ccc      CALL PUSHERR (0002, 'CALCR2A')    ! WARNING ERROR: NO CONVERGENCE
C 
C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
C
20    IF (CLI.LE.TINY .AND. WAER(5).GT.TINY) THEN !No disslv Cl-;solid only
         DO 30 I=1,NIONS
            MOLAL(I) = ZERO
30       CONTINUE
         DO 40 I=1,NGASAQ
            GASAQ(I) = ZERO
40       CONTINUE
         CALL CALCR1A
      ELSE                                     ! OK, aqueous phase present
         A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
         A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
         A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
C
         GNH3    = NH4I/HI/A2
         GHNO3   = HI*NO3I/A3
         GHCL    = HI*CLI /A4
C
         GASAQ(1)= NH3AQ
         GASAQ(2)= CLAQ
         GASAQ(3)= NO3AQ
C
         CNH42S4 = ZERO
         CNH4NO3 = ZERO
         CNH4CL  = CHI4 - PSI4
         CNACL   = CHI3 - PSI3
         CNANO3  = CHI2 - PSI2
         CNA2SO4 = CHI1 - PSI1
      ENDIF
C
      RETURN
C
C *** END OF SUBROUTINE CALCR2A *****************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCR1
C *** CASE R1
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
C     2. SOLID AEROSOL ONLY
C     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NA2SO4, NANO3, NACL
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCR1
      INCLUDE 'isrpia.inc'
      LOGICAL  EXAN, EXAC, EXSN, EXSC
      EXTERNAL CALCR1A, CALCR2A, CALCR3A, CALCR4A, CALCR5
C
C *** SOLVE FOR DRY CASE AND SEE WHICH SOLIDS ARE POSSIBLE **************
C
      SCASE = 'R1 ; SUBCASE 1'  
      CALL CALCR1A              ! SOLID
      SCASE = 'R1 ; SUBCASE 1'
C     
      EXAN = CNH4NO3.GT.TINY    ! NH4NO3
      EXAC = CNH4CL .GT.TINY    ! NH4CL
      EXSN = CNANO3 .GT.TINY    ! NANO3
      EXSC = CNACL  .GT.TINY    ! NACL
C
C *** REGIME DEPENDS ON RELATIVE HUMIDITY AND POSSIBLE SPECIES **********
C
      IF (EXAN.AND.EXAC.AND.EXSC.AND.EXSN) THEN  ! *** ALL EXIST
         IF (RH.GE.DRMH1) THEN    
            SCASE = 'R1 ; SUBCASE 2'  ! MDRH
            CALL CALCMDRP (RH, DRMH1, DRNH4NO3, CALCR1A, CALCR2A)
            SCASE = 'R1 ; SUBCASE 2'
         ENDIF
C
      ELSE IF (.NOT.EXAN) THEN                   ! *** NH4NO3 = 0
         IF      (     EXAC .AND.      EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMH2) THEN    
               SCASE = 'R1 ; SUBCASE 3'  
               CALL CALCMDRP (RH, DRMH2, DRNANO3, CALCR1A, CALCR3A)
               SCASE = 'R1 ; SUBCASE 3'
            ENDIF

         ELSE IF (.NOT.EXAC .AND.      EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR1) THEN    
               SCASE = 'R1 ; SUBCASE 4'  
               CALL CALCMDRP (RH, DRMR1, DRNANO3, CALCR1A, CALCR3A)
               SCASE = 'R1 ; SUBCASE 4'
            ENDIF

         ELSE IF (.NOT.EXAC .AND. .NOT.EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR2) THEN    
               SCASE = 'R1 ; SUBCASE 5'  
               CALL CALCMDRP (RH, DRMR2, DRNACL, CALCR1A, CALCR3A) !, CALCR4A)
               SCASE = 'R1 ; SUBCASE 5'
            ENDIF

         ELSE IF (.NOT.EXAC .AND.      EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR3) THEN    
               SCASE = 'R1 ; SUBCASE 6'  
               CALL CALCMDRP (RH, DRMR3, DRNANO3, CALCR1A, CALCR3A)
               SCASE = 'R1 ; SUBCASE 6'
            ENDIF

         ELSE IF (     EXAC .AND. .NOT.EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR4) THEN    
               SCASE = 'R1 ; SUBCASE 7'  
               CALL CALCMDRP (RH, DRMR4, DRNACL, CALCR1A, CALCR3A) !, CALCR4A)
               SCASE = 'R1 ; SUBCASE 7'
            ENDIF

         ELSE IF (     EXAC .AND. .NOT.EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR5) THEN    
               SCASE = 'R1 ; SUBCASE 8'  
               CALL CALCMDRP (RH, DRMR5, DRNH4CL, CALCR1A, CALCR3A) !, CALCR5)
               SCASE = 'R1 ; SUBCASE 8'
            ENDIF

         ELSE IF (     EXAC .AND.      EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR6) THEN    
               SCASE = 'R1 ; SUBCASE 9'  
               CALL CALCMDRP (RH, DRMR6, DRNANO3, CALCR1A, CALCR3A)
               SCASE = 'R1 ; SUBCASE 9'
            ENDIF
         ENDIF
C
      ELSE IF (.NOT.EXAC) THEN                   ! *** NH4CL  = 0
         IF      (     EXAN .AND.      EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR7) THEN    
               SCASE = 'R1 ; SUBCASE 10'  
               CALL CALCMDRP (RH, DRMR7, DRNH4NO3, CALCR1A, CALCR2A)
               SCASE = 'R1 ; SUBCASE 10'
            ENDIF

         ELSE IF (     EXAN .AND. .NOT.EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR8) THEN    
               SCASE = 'R1 ; SUBCASE 11'  
               CALL CALCMDRP (RH, DRMR8, DRNH4NO3, CALCR1A, CALCR2A)
               SCASE = 'R1 ; SUBCASE 11'
            ENDIF

         ELSE IF (     EXAN .AND. .NOT.EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR9) THEN    
               SCASE = 'R1 ; SUBCASE 12'  
               CALL CALCMDRP (RH, DRMR9, DRNH4NO3, CALCR1A, CALCR2A)
               SCASE = 'R1 ; SUBCASE 12'
            ENDIF

         ELSE IF (     EXAN .AND.      EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR10) THEN    
               SCASE = 'R1 ; SUBCASE 13'  
               CALL CALCMDRP (RH, DRMR10, DRNH4NO3, CALCR1A, CALCR2A)
               SCASE = 'R1 ; SUBCASE 13'
            ENDIF
         ENDIF
C
      ELSE IF (.NOT.EXSN) THEN                  ! *** NANO3  = 0
         IF      (     EXAN .AND.      EXAC .AND.      EXSC) THEN
            IF (RH.GE.DRMR11) THEN    
               SCASE = 'R1 ; SUBCASE 14'  
               CALL CALCMDRP (RH, DRMR11, DRNH4NO3, CALCR1A, CALCR2A)
               SCASE = 'R1 ; SUBCASE 14'
            ENDIF

         ELSE IF (     EXAN .AND.      EXAC .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR12) THEN    
               SCASE = 'R1 ; SUBCASE 15'  
               CALL CALCMDRP (RH, DRMR12, DRNH4NO3, CALCR1A, CALCR2A)
               SCASE = 'R1 ; SUBCASE 15'
            ENDIF
         ENDIF
C
      ELSE IF (.NOT.EXSC) THEN                  ! *** NACL   = 0
         IF      (     EXAN .AND.      EXAC .AND.      EXSN) THEN
            IF (RH.GE.DRMR13) THEN    
               SCASE = 'R1 ; SUBCASE 16'  
               CALL CALCMDRP (RH, DRMR13, DRNH4NO3, CALCR1A, CALCR2A)
               SCASE = 'R1 ; SUBCASE 16'
            ENDIF
         ENDIF
      ENDIF
C 
      RETURN
C
C *** END OF SUBROUTINE CALCR1 ******************************************
C
      END


C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCR1A
C *** CASE R1 ; SUBCASE 1
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
C     2. SOLID AEROSOL ONLY
C     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NANO3, NA2SO4, NANO3, NACL
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C
C=======================================================================
C
      SUBROUTINE CALCR1A
      INCLUDE 'isrpia.inc'
C
C *** CALCULATE SOLIDS **************************************************
C
      CNA2SO4 = WAER(2)
      FRNA    = MAX (WAER(1)-2*CNA2SO4, ZERO)
C
      CNH42S4 = ZERO
C
      CNANO3  = MIN (FRNA, WAER(4))
      FRNO3   = MAX (WAER(4)-CNANO3, ZERO)
      FRNA    = MAX (FRNA-CNANO3, ZERO)
C
      CNACL   = MIN (FRNA, WAER(5))
      FRCL    = MAX (WAER(5)-CNACL, ZERO)
      FRNA    = MAX (FRNA-CNACL, ZERO)
C
      CNH4NO3 = MIN (FRNO3, WAER(3))
      FRNO3   = MAX (FRNO3-CNH4NO3, ZERO)
      FRNH3   = MAX (WAER(3)-CNH4NO3, ZERO)
C
      CNH4CL  = MIN (FRCL, FRNH3)
      FRCL    = MAX (FRCL-CNH4CL, ZERO)
      FRNH3   = MAX (FRNH3-CNH4CL, ZERO)
C
C *** OTHER PHASES ******************************************************
C
      WATER   = ZERO
C
      GNH3    = ZERO
      GHNO3   = ZERO
      GHCL    = ZERO
C
      RETURN
C
C *** END OF SUBROUTINE CALCR1A *****************************************
C
      END
