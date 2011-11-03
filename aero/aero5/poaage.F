
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
C $Header: /project/yoj/arc/CCTM/src/aero/aero5/poaage.F,v 1.5 2011/11/03 18:41:14 sjr Exp $




!C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE POAAGE( DT )

 
!C #####################################################################
!C This subroutine calculates oxidative aging of POA using the 
!C following reaction:
!C POCRm ---> PNCOM (rate constant = koheff*[OH])
!C      - koheff comes from Weitkamp et al (2008) and George et al (2007)
!C      - POCRm = reduced primary organic carbon (molar concentration)
!C 
!C POMOC  = (POC + NCOM)/POC 
!C      - in other words: pimary OM/OC = (POC + PNCOM)/POC 
!C PHOrat = (44/12 - POMOC)/(POMOC - 14/12) 
!C      - see Simon et al. (2011) for derivation
!C Omoles = NCOM/(16 + PHOrat) if POMOC is between 14/12 and 44/12 
!C      - see Simon et al (2011) for derivation
!C Omoles = NCOM/16 for POMOC larger than 44/12 
!C      - if OM/OC > 3.667, then POC is fully oxidized and all 
!C        NCOM is oxygen 
!C Omoles = 0 for POMOC smaller than 14/12 
!C      - if OM/OC < 1.167, then POC is fully reduced and all 
!C        NCOM is hydrogen
!C
!C POCRm = POC/12 - Omoles
!C note:we divide POC by 12 b/c we want moles of carbon atoms not moles 
!C      of POC (since each carbon atom w\in the molecule is allowed 
!C      to react)
!C note: we calculate Omoles based on the equations above derived from 
!C      Heald et al (2010) and documented in Simon et al (2011).
!C
!C The following rate equation comes from the reaction above:
!C      dPOCRm/dt = POCRm*koheff*[OH] 
!C        -assume that [OH] does not change as a result of this reaction
!C      solve for POCRm at time, t
!C        moles: POCRm(t) = POCRm(0)*EXP(-koheff*[OH]*t) = POCRm(0)*expdt
!C
!C      One mole of "NCOMm" is formed for every mole of POCRm that reacts:
!C             -dPOCRm/dt = dNCOMm/det
!C        moles of NCOMm: NCOMm = NCOMm(0) + POCRm(0)*[1-EXP(-koheff*[OH]*t)] = 
!C                        NCOMm(0) + POCRm(0)*(1-expdt)
!C        NCOMg = NCOMm*15.0 
!C          - every mole of newly formed NCOM results in an average gain of 1 oxygen atom 
!C            and an average loss of 1 hydrogen atoms (on average, two oxidation steps 
!C            convert a CH3 functional group into a COOH functional group) 
!C               -(based on Heald et al. (2010)) 
!C      Rewrite NCOM formation equation using grams: 
!C        NCOMg/15.0 = NCOMg(0)/15.0 + POCRm*(1-expdt) 
!C                         |
!C                         |
!C                         V 
!C        NCOMg == NCOM(0) + 15.0*POCRm*(1-expdt)
!C
!C #####################################################################
!C
!C
!C 
!c Key Subroutines Called: none
!c
!c Key Functions Called: none
!c
!c Revision History:
!c    First version was coded in February 2011 by Dr. Heather Simon
!C 
!c  REFERENCES:
!c   1. George, I.J., Vlasenko, A., Slowik, J.G., Broekhuizen, K., 
!C      Abbatt, J.P.D. (2007), Heterogeneous oxidation of saturated
!C      organic aerosols by hydroxyl radicals: uptake kinetics, 
!C      condesned-phase products, and particle size change, 
!C      Atmospheric Chemistry and Physics, 7, 4187-4201 
!c     
!c   2. Heald, C.L., Kroll, J.H., Jimenez, J.L., Docherty, K.S., 
!C      DeCarlo, P.F., Aiken, A.C., Chen, Q., Martin, S.T., Farmer, S.T.,
!C      Artaxo, P. (2010), A simplified description of the evolution of 
!C      organic aerosol composition in the atmosphere, GRL, 37, L08803. 
!C
!C   3. Simon, H. and Bhave, P.V. (2011), Simulating the degree of oxidation 
!C      in atmospheric organic particles  
!C      In Review at ES&T.
!C
!C   4. Weitkamp, E.A., Lambe, A.T., Donahue, N.M., Robinson, A.L. (2008),
!C      Laboratory measurements of the heterogeneous oxidation of condensed-
!C      phase organic molecular markers for motor vehicl exhaust, 42, 
!C      7950-7956.
!c   
!C-----------------------------------------------------------------------

      USE AERO_DATA
      USE PRECURSOR_DATA
                           
      IMPLICIT NONE

      INCLUDE SUBST_RXCMMN    ! to get mech name

!C *** Arguments
      REAL,    INTENT( IN )  :: DT        ! Synchronization time step

!C *** Parameters
      REAL, PARAMETER :: koheff  = 0.25E-11       ! effective OH rate constant in cm3 molec-1 sec-1
      REAL, PARAMETER :: Avogadro= 6.02214179E23  ! Avogadro's number

!C *** Local Variables

!C * chemical species concentrations
      REAL      POCRmi     ! aitken mode moles of reduced primary carbon calculated as 
                           ! moles of POC carbon minus moles of NCOM oxygen (POC/12- moles of O)
      REAL      POCRmj     ! accumulation mode moles of reduced primary carbon calculated as 
                           ! moles of POC carbon minus moles of NCOM oxygen (POC/12- moles of O)
      REAL      OHmoleccm3 ! OH concentrations in molec/cm3
      REAL      Omolesi    ! moles of Oxygen in aitken mode primary OA
      REAL      Omolesj    ! moles of Oxygen in accumulation mode primary OA


!C * variables representing mass and molar ratios
      REAL      POMOCi     ! OM/OC ratio in aitken mode primary OA
      REAL      POMOCj     ! OM/OC ratio in accumulation mode primary OA
      REAL      PHOrati    ! H/O molar ratio in aitken mode primary OA
      REAL      PHOratj    ! H/O molar ratio in accumulation mode primary OA

!C * 
      REAL      expdt_POC  ! non-dimensional loss coefficient

!C-----------------------------------------------------------------------

!C POA aging is enabled in aero6
      IF ( INDEX ( MECHNAME, 'AE5' ) .GT. 0 ) RETURN

!C calculate OM/OC and H/O ratios

!C Aitken mode
      IF (aerospc_conc(APOC_IDX,1) .NE. 0.0) THEN
        POMOCi = (aerospc_conc(APOC_IDX,1) + aerospc_conc(APNCOM_IDX,1))/aerospc_conc(APOC_IDX,1)
      ELSE
        POMOCi = 0 ! If POC is 0, then the value of POMOC does not matter b/c POCR will also be 0
      END IF

!C Accumulation mode
      IF (aerospc_conc(APOC_IDX,2) .NE. 0.0) THEN
        POMOCj = (aerospc_conc(APOC_IDX,2) + aerospc_conc(APNCOM_IDX,2))/aerospc_conc(APOC_IDX,2)
      ELSE
        POMOCj = 0 ! If POC is 0, then the value of POMOC does not matter b/c POCR will also be 0
      END IF

      PHOrati = 0.0
      PHOratj = 0.0

!C Aitken mode
      IF (POMOCi .LE. (14.0/12.0)) THEN
           Omolesi = 0.0
      ELSE IF (POMOCi .GE. (44.0/12.0)) THEN
           Omolesi = (aerospc_conc(APNCOM_IDX,1))/16.0
      ELSE IF ((POMOCi .LT. (44.0/12.0)) .AND.
     &(POMOCi .GT. (14.0/12.0)) ) THEN
           PHOrati = ((44.0/12.0)-POMOCi)/(POMOCi-(14.0/12.0))
           Omolesi = (aerospc_conc(APNCOM_IDX,1))/(16.0 + PHOrati)
      END IF

!C Accumulation mode
      IF (POMOCj .LE. (14.0/12.0)) THEN
           Omolesj = 0.0
      ELSE IF (POMOCj .GE. (44.0/12.0)) THEN
           Omolesj = (aerospc_conc(APNCOM_IDX,2))/16.0
      ELSE IF ((POMOCj .LT. (44.0/12.0)) .AND.
     &(POMOCj .GT. (14.0/12.0)) ) THEN
           PHOratj = ((44.0/12.0)-POMOCj)/(POMOCj-(14.0/12.0))
           Omolesj = (aerospc_conc(APNCOM_IDX,2))/(16.0 + PHOratj)
      END IF

!C calculate POCR
      POCRmi = (aerospc_conc(APOC_IDX,1))/12.0 - Omolesi
      POCRmj = (aerospc_conc(APOC_IDX,2))/12.0 - Omolesj

      POCRmi = max(POCRmi, 0.0)
      POCRmj = max(POCRmj, 0.0)

!C convert OH to molecules/cm3

      OHmoleccm3 = precursor_conc(OH_IDX) * 1.0E-12 * Avogadro / precursor_mw(OH_IDX)


!C calculate expdt

      expdt_POC = exp( - koheff* OHmoleccm3 * DT )

!C calculate new NCOM concentrations

      aerospc_conc(APNCOM_IDX,1) = aerospc_conc(APNCOM_IDX,1) + 15.0 * POCRmi * ( 1.0 - expdt_POC)
      aerospc_conc(APNCOM_IDX,2) = aerospc_conc(APNCOM_IDX,2) + 15.0 * POCRmj * ( 1.0 - expdt_POC)

     
      RETURN

      END SUBROUTINE POAAGE


