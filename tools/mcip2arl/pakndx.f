
! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /nas01/depts/ie/cempd/apps/CMAQ/v5.0.1/CMAQv5.0.1/models/TOOLS/src/mcip2arl/pakndx.f,v 1.1.1.1 2012/04/19 19:48:37 sjr Exp $

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  PAKNDX           PAcK iNDeX writes index record
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL           DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!   PACK INDEX - AFTER ALL THE RECORDS FOR A PARTICULAR TIME
!   PERIOD HAVE BEEN WRITTEN TO A FILE, THIS ROUTINE WRITES THE
!   INDEX RECORD FOR THAT TIME GROUP.  THE INDEX RECORD IS ALWAYS
!   THE FIRST RECORD OF THE TIME GROUP.  IT INCLUDES GRID DEFINITION
!   VARIABLES, AND CHECKSUM INFORMATION.
!
! PROGRAM HISTORY LOG:
!   Last Revised: 14 Feb 1997 (RRD) 
!                 02 Feb 2001 (RRD) - fortran90 upgrade
!                 18 Oct 2001 (RRD) - extended grid domains
!                 08 Nov 2001 (RRD) - expanded format of grid in header
!                 09 Sep 2002 (RRD) - fortran coding standards
!
! USAGE:  CALL PAKNDX(LUNIT)
!
!   INPUT ARGUMENT LIST:   see below
!   OUTPUT ARGUMENT LIST:  see below
!   INPUT FILES:           none
!   OUTPUT FILES:          none
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

SUBROUTINE PAKNDX(LUNIT)

  IMPLICIT NONE

!-------------------------------------------------------------------------------
 
  INCLUDE 'DEFPACK.INC'

  INTEGER, INTENT(IN) :: lunit
  INTEGER             :: nvar,nlvl,nrec,jrec,nhl1,nhl2 
  INTEGER             :: i,j,k,l,kk,ng,kg,knx,kny,kol 
  REAL                :: zl 
  CHARACTER(50)       :: label   ! standard record label
  CHARACTER(MLEN)     :: header  ! extender header

! pass structure between routines
  COMMON / PAKCOM / GV, NG

!-------------------------------------------------------------------------------

!==>determine which grid

  KG=0
  DO KK=1,NG
     IF(LUNIT.EQ.GV(KK)%KUNIT)KG=KK
  END DO
  IF(KG.EQ.0)THEN
     WRITE(*,*)'*ERROR* pakndx: Requesting uninitialized unit (call pakset)'
     STOP 900
  END IF

!==>conventional 50 byte label

  IF(GV(KG)%XGPT)THEN
     WRITE(LABEL,'(6I2,A2,A4,I4,2E14.7)')                       &
     GV(KG)%IY0,GV(KG)%IM0,GV(KG)%ID0,GV(KG)%IH0,GV(KG)%IC0,    &
     0,GV(KG)%IGC,'INDX',0,0.0,0.0
!    adjust grid point number to 100s, 10s, 1s
     KNX=GV(NG)%NXG-INT(GV(NG)%NXG/1000)*1000
     KNY=GV(NG)%NYG-INT(GV(NG)%NYG/1000)*1000
  ELSE
     WRITE(LABEL,'(7I2,A4,I4,2E14.7)')                          &
     GV(KG)%IY0,GV(KG)%IM0,GV(KG)%ID0,GV(KG)%IH0,GV(KG)%IC0,    &
     0,GV(KG)%IG,'INDX',0,0.0,0.0
     KNX=GV(NG)%NXG
     KNY=GV(NG)%NYG
  END IF

!==>first part of header: 1 -> 108

! WRITE(HEADER(1:108),'(A4,I3,I2,12F7.2,3I3,I2,I4)')            &
!    GV(KG)%MODEL,GV(KG)%ICX,GV(KG)%MN0,GV(KG)%GRIDS,           &
!    KNX,KNY,GV(KG)%NLVL,GV(KG)%KSYS,GV(KG)%LENH

  WRITE(HEADER(1:9),'(A4,I3,I2)') GV(KG)%MODEL,GV(KG)%ICX,GV(KG)%MN0
  KOL=10
  DO K=1,12 
     IF(GV(KG)%GRIDS(K).GE.1000.0)THEN
        WRITE(HEADER(KOL:KOL+6),'(F7.2)')GV(KG)%GRIDS(K)
     ELSEIF(GV(KG)%GRIDS(K).GE.100.0)THEN
        WRITE(HEADER(KOL:KOL+6),'(F7.3)')GV(KG)%GRIDS(K)
     ELSEIF(GV(KG)%GRIDS(K).GE.10.0)THEN
        WRITE(HEADER(KOL:KOL+6),'(F7.4)')GV(KG)%GRIDS(K)
     ELSEIF(GV(KG)%GRIDS(K).GE.1.0)THEN
        WRITE(HEADER(KOL:KOL+6),'(F7.5)')GV(KG)%GRIDS(K)
     ELSEIF(GV(KG)%GRIDS(K).GE.0.0)THEN
        WRITE(HEADER(KOL:KOL+6),'(F7.6)')GV(KG)%GRIDS(K)
     ELSEIF(GV(KG)%GRIDS(K).GT.-1.0)THEN
        WRITE(HEADER(KOL:KOL+6),'(F7.5)')GV(KG)%GRIDS(K)
     ELSEIF(GV(KG)%GRIDS(K).GT.-10.0)THEN
        WRITE(HEADER(KOL:KOL+6),'(F7.4)')GV(KG)%GRIDS(K)
     ELSEIF(GV(KG)%GRIDS(K).GT.-100.0)THEN
        WRITE(HEADER(KOL:KOL+6),'(F7.3)')GV(KG)%GRIDS(K)
     ELSEIF(GV(KG)%GRIDS(K).GT.-1000.0)THEN
        WRITE(HEADER(KOL:KOL+6),'(F7.2)')GV(KG)%GRIDS(K)
     ELSE
        WRITE(HEADER(KOL:KOL+6),'(E7.1)')GV(KG)%GRIDS(K)
     END IF
     KOL=KOL+7
  END DO
  WRITE(HEADER(KOL:),'(3I3,I2,I4)')   &
        KNX,KNY,GV(KG)%NLVL,GV(KG)%KSYS,GV(KG)%LENH

!==>loop through remainder of the extended header

  KOL=109
  NLVL=GV(KG)%NLVL

  DO L=1,NLVL
     ZL=GV(KG)%HEIGHT(L)

!    precision depends upon the height coordinate
     IF(ZL.GE.10000.0)THEN
        WRITE(HEADER(KOL:KOL+7),'(F6.0,I2)')ZL,GV(KG)%NVAR(L)
     ELSEIF(ZL.GE.1000.0)THEN
        WRITE(HEADER(KOL:KOL+7),'(F6.1,I2)')ZL,GV(KG)%NVAR(L)
     ELSEIF(ZL.GE.100.0.AND.ZL.LT.1000.0)THEN
        WRITE(HEADER(KOL:KOL+7),'(F6.2,I2)')ZL,GV(KG)%NVAR(L)
     ELSEIF(ZL.GE.10.0.AND.ZL.LT.100.0)THEN
        WRITE(HEADER(KOL:KOL+7),'(F6.3,I2)')ZL,GV(KG)%NVAR(L)
     ELSEIF(ZL.GE.1.0.AND.ZL.LT.10.0)THEN
        WRITE(HEADER(KOL:KOL+7),'(F6.4,I2)')ZL,GV(KG)%NVAR(L)
     ELSE
        WRITE(HEADER(KOL:KOL+7),'(F6.5,I2)')ZL,GV(KG)%NVAR(L)
     END IF

!    add variable id's and checksums
     KOL=KOL+8
     NVAR=GV(KG)%NVAR(L)
     DO K=1,NVAR
        WRITE(HEADER(KOL:KOL+7),'(A4,I3)') GV(KG)%VARB(K,L), GV(KG)%CHKS(K,L)
        KOL=KOL+8
     END DO
  END DO

!==>write extended header to disk

  NHL1=1
! number of index records
  NREC=GV(KG)%NHREC
! point to first index record
  JREC=GV(KG)%MREC

! test for previous setup
  IF(JREC.LT.1)THEN
     WRITE(*,*)'*ERROR* pakndx: no prior calls to pakrec'
     STOP 900
  END IF

  DO K=1,NREC
!    byte count for each index
     NHL2=NHL1+GV(KG)%LREC-1
     IF(K.EQ.NREC)NHL2=NHL1+GV(KG)%NHBYT-1

     WRITE(GV(KG)%KUNIT,REC=JREC)LABEL,HEADER(NHL1:NHL2)
     JREC=JREC+1
     NHL1=NHL2+1
  END DO

!==>clear flags

! checksum table
  DO J=1,MLVL
  DO I=1,MVAR
     GV(KG)%CHKS(I,J)=0
  END DO
  END DO

! new time flag
  GV(KG)%NEWT=.TRUE.

END SUBROUTINE pakndx
