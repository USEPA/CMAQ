!*******************************************************************************
!$RCSfile: isc_prime.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
SUBROUTINE define
!***********************************************************************
!                 DEFINE Module of ISC2 Model
!
!        PURPOSE: Defines Location of Fields on Runstream Input Image
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Card Image
!
!        OUTPUTS: Number of Fields on Card, IFC
!                 Beginning and Ending Columns of Fields, LOCB and LOCE
!
!        CALLED FROM:   SCIPRIME
!***********************************************************************
USE sciprime_inc
IMPLICIT  NONE
INTEGER           :: i

!    Initialize the Blank Line and In-field Status Indicators
bline = .true.
infld = .false.

IF (iline == 1) THEN
    !  Define the Starting Column for the Input File In Case File Is Shifted.
    !  Allow for Shift of Up to 12 Columns
    locb(1) = 0
  DO i = 1,12
     IF (runst(i) /= ' ') THEN
           locb(1) = i
       EXIT
     ELSE
         locb(1) = 1
     END IF
  END DO
    loce(1) = locb(1) + 1
    locb(2) = locb(1) + 3
    loce(2) = locb(1) + 10
END IF

ifc = 2

!     Loop Through the Pathway and Keyword Fields To Check for Blank Line
DO  i = locb(1), loce(2)+1
    IF (runst(i) /= ' ') bline = .false.
END DO

!     Loop through the Data Fields
DO  i = locb(1)+12, istrg

    IF (.not.infld .and. runst(i)/=' ') THEN
    !   Location is the Beginning of a Field
    !   Set Mark of not Blank Line
    bline = .false.
    !   Set Mark of in a Field
    infld = .true.
    !   Increment the Field Counter
    ifc = ifc + 1
    !   Record the Location of Beginning of the Field
    locb(ifc) = i
    ELSE IF (infld .and. runst(i)==' ') THEN
    !   Location is the End of a Field
    !   Set Mark of Not In a field
    infld = .false.
    !   Record the Location of Ending of the Field
    loce(ifc) = i - 1
    END IF
    !        Check for End of Input String
    !        (Length of ISTRG is Set as a PARAMETER in MAIN1.INC)
    IF (infld .and. i==istrg) THEN
     loce(ifc) = istrg
    END IF

END DO

RETURN
END

SUBROUTINE getfld
!***********************************************************************
!        GETFLD Module of ISC2 Model
!
!        PURPOSE: Gets Contents of Fields on Runstream Input Image
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Card Image
!
!        OUTPUTS: Contents of Fields on Card
!
!        CALLED FROM:   SCIPRIME
!***********************************************************************
USE sciprime_inc
IMPLICIT  NONE
INTEGER           :: i,j

DO i = 1, ifc
    IF (loce(i)-locb(i) <= 39) THEN
     ! Field Satisfies Limit of 40 Characters
     WRITE(field(i),9004) (runst(j),j=locb(i),loce(i))
    ELSE
     ! Field Exceeds 40 Character Limit (May Be Valid for Met Format)
     ! Truncate Field at 40 Characters
     WRITE(field(i),9004) (runst(j),j=locb(i),locb(i)+39)
    END IF
END DO

9004 FORMAT(40(a1:))

RETURN
END

SUBROUTINE expath(inpfld,nopath)
!***********************************************************************
!                 EXPATH Module of ISC2 Model
!
!        PURPOSE: Extracts and Verifies Pathway ID from
!                 Runstream Input Card Image
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Card Image
!
!        OUTPUTS: The Extracted Pathway ID
!
!        CALLED FROM:   SETUP
!***********************************************************************
USE sciprime_inc
USE error_inc

IMPLICIT  NONE

INTEGER            :: i
CHARACTER*2        :: inpfld, pathwy(ipn)
LOGICAL            :: nopath

!     Variable Initializations
DATA (pathwy(i),i = 1, ipn) /'SO','**'/

nopath = .true.
!     Begin The Processing
IF (inpfld /= '  ') THEN
    ! Check the Read-in Pathway
    path = inpfld
    DO i = 1, ipn
      ! In Case of Match Set NOPATH to FALSE 
      IF (inpfld == pathwy(i)) THEN
        nopath = .false.
        ! Exit to END
        GO TO 999
      END IF
    END DO
    ! In Case Of Invalid Pathway ID, Write Out Error Meassage
    IF (nopath) THEN
      nError = WN_ERROR
      eRoutine='expath'
      eMessage='Invalid SO Pathway ID'
      WRITE(eInform,*)'Resetting Pathway to Not Available Pathway'
      eAction  = CHAR(0)
      CALL WarningMessage(0,.true.)
      IF (nError /= NO_ERROR) GO TO 999
      ! Set the Pathway to the Not Available
      path = 'NA'
    END IF
ELSE
    ! In Case of Blank Field Set Pathway to Previous Pathway
    nopath = .false.
    path  = 'NA'
END IF

999  RETURN
END

SUBROUTINE exkey(inpfld,nokey)
!***********************************************************************
!                 EXKEY Module of ISC2 Model
!
!        PURPOSE: Extracts and Verifies Keyword from
!                 Runstream Input Card Image
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Card Image
!
!        OUTPUTS: The Extracted Keyword
!
!        CALLED FROM:   SCIPRIME
!***********************************************************************
USE error_inc
USE sciprime_inc
IMPLICIT  NONE

INTEGER            :: i
CHARACTER*8        :: inpfld
LOGICAL            :: nokey

!     Variable Initializations
nokey  = .true.

!     Begin The Processing
IF (inpfld /= '        ') THEN
    ! Check the Read-in Keyword
    keywrd = inpfld
    DO  i = 1, ikn
    ! In Case of Match Set NOKEY to FALSE
    IF (inpfld == keywd(i)) then
      nokey = .false.
      !  Exit to END
      GO TO 999
    END IF
    END DO
    ! When Illegal Keyword Output Error Message
    IF (nokey) THEN
  ! Invalid Keyword
    nError = WN_ERROR
    eRoutine='exkey'
    eMessage='Invalid Keyword'
    WRITE(eInform,*)'Unknown Keyword: ',inpfld
    eAction  = 'Stopping SCICHEM run'
      GO TO 999
  END IF
ELSE
    !     In Case of Blank Field, Keyword Is Set to Previous Keyword
    nokey  = .false.
    keywrd = pkeywd
END IF

999  RETURN
END


SUBROUTINE socard
!***********************************************************************
!                 SOCARD Module of ISC2 Model
! ----------------------------------------------------------------------
! ---    ISC-PRIME     Version 1.0    Level 970812              Modified
! ---        V. Tino
! ---        Earth Tech, Inc.
!            Prepared for EPRI under contract WO3527-01
! ----------------------------------------------------------------------
!
!        PURPOSE: To process SOurce Pathway card images
!
!        PROGRAMMER:  Roger Brode, Jeff Wang
!        MODIFIED BY  D. Strimaitis, SRC (for WET DEPOSITION)
!
!        DATE:    November  8, 1993
!
!        MODIFIED BY  D. Strimaitis, SRC (for DRY DEPOSITION)
!        (DATE:    February 15, 1993)
!
!        INPUTS:  Pathway (SO) and Keyword
!
!        OUTPUTS: Source Arrays
!                 Sourcer Setup Status Switches
!
!        CALLED FROM:   SETUP
!***********************************************************************
USE error_inc
USE sciprime_inc
IMPLICIT NONE
INTEGER i

DO i = 1,ikn
  IF (TRIM(keywrd) == TRIM(keywd(i))) THEN
    isstat(i) = isstat(i) + 1
    EXIT
  END IF
  IF (i == ikn) THEN
    nError = WN_ERROR
    eRoutine='socard'
    eMessage='Invalid keyword'
    WRITE(eInform,*)'Unknown keyword ',keywrd
    eAction  = CHAR(0)
      CALL WarningMessage(0,.true.)
    IF (nError /= NO_ERROR) GO TO 999
  END IF
END DO
!        Process Direction-specific Building Dimensions     ---   CALL DSBLDG
CALL dsbldg

999 RETURN
END

SUBROUTINE dsbldg
!***********************************************************************
!                 DSBLDG Module of ISC2 Model
!
!        PURPOSE: Processes Direction-specific Building Directions
!
!        PROGRAMMER:  Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!***********************************************************************
USE error_inc
USE sciprime_inc
IMPLICIT NONE
INTEGER                     :: isdx
CHARACTER*8                 :: lid, hid
CHARACTER*40                :: soid
LOGICAL                     :: rmark


! Check The Number of The Fields
IF (ifc <= 2) THEN
    ! No Parameters
    nError = WN_ERROR
  eRoutine='dsbldg'
  eMessage='No Parameters'
  WRITE(eInform,*)''
  eAction  = CHAR(0)
    CALL WarningMessage(0,.true.)
  IF (nError /= NO_ERROR) GO TO 999
ELSE IF (ifc == 3) THEN
    ! Not Enough Parameters
    nError = WN_ERROR
  eRoutine='dsbldg'
  eMessage='Not Enough Parameters'
  WRITE(eInform,*)''
  eAction  = CHAR(0)
    CALL WarningMessage(0,.true.)
  IF (nError /= NO_ERROR) GO TO 999
END IF

! Get The Source ID(s)
soid = field(3)
CALL fsplit(path,keywrd,soid,40,'-',rmark,lid,hid)
! Verify The Effective Srcid
IF (lid == hid) THEN
  ! Only single source case 
  isdx = 1
  CALL dsfill(isdx) 
ELSE
  nError = WN_ERROR
  eRoutine= 'dsbldg'
  eMessage= 'Cannot handle ranges'
  WRITE(eInform,*)'Source Id',TRIM(soid)
  eAction  = 'Stopping SCICHEM'
  GO TO 999
END IF

999  RETURN
END

SUBROUTINE dsfill(isdx)
!***********************************************************************
!                 DSFILL Module of ISC2 Model
!! ----------------------------------------------------------------------
!! ---    ISC-PRIME     Version 1.0    Level 970812              Modified
!! ---        V. Tino
!! ---        Earth Tech, Inc.
!            Prepared for EPRI under contract WO3527-01
! ----------------------------------------------------------------------
!
!        PURPOSE: Fill Direction-specific Building Dimension Arrays
!
!        PROGRAMMER:  Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!***********************************************************************
USE error_inc
USE sciprime_inc
IMPLICIT NONE

INTEGER                     :: i,j,k,imit,isdx,iset
REAL                        :: fnum
IF (keywrd == 'RURAL   ') THEN
   IF (TRIM(field(4))=='TRUE') THEN
       isrural(isdx) = .true.
   ELSE
       isrural(isdx) = .false.
   END IF
ELSE IF (keywrd == 'BUILDHGT') THEN
    iset = iwrk2(isdx,1)
    DO 200 k = 4, ifc
    ! Change Fields To Numbers
    CALL stonum(field(k),40,fnum,imit)
    ! Check The Numerical Field
    IF (imit == -1) THEN
      nError = WN_ERROR
      eRoutine= 'dsfill'
      eMessage= 'Invalid entry in numerical field'
      WRITE(eInform,*)'Invalid value for BUILDHGT ',field(k)
      eAction  = 'Stopping SCICHEM'
        GO TO 999
    END IF
    DO j = 1, imit
      iset = iset + 1
      ! Assign The Field
      IF (iset <= nsec) THEN
        adsbh(iset,isdx) = fnum
        IF (fnum < 0.0) THEN
          ! WRITE Error Message:  Negative Value for ADSBH
          nError = IV_ERROR
          eRoutine= 'dsfill'
          eMessage= 'Negative Value for ADSBH'
          WRITE(eInform,*)'ADSBH =',fnum
          eAction  = 'Stopping SCICHEM'
            GO TO 999
        END IF
      ELSE
        ! WRITE Error Message    ! 
        nError = WN_ERROR
        eRoutine= 'dsfill'
        eMessage= 'Too Many Sectors Input for ADSBH'
        WRITE(eInform,*)'Number of sectors = ',iset
        eAction  = CHAR(0)
          CALL WarningMessage(0,.true.)
        IF (nError /= NO_ERROR) GO TO 999
      END IF
    END DO    
    200     CONTINUE
    iwrk2(isdx,1) = iset
ELSE IF (keywrd == 'BUILDWID') THEN
  iset = iwrk2(isdx,2)
  DO 400 k = 4, ifc
      ! Change Fields To Numbers
      CALL stonum(field(k),40,fnum,imit)
      ! Check The Numerical Field
      IF (imit == -1) THEN
        nError = WN_ERROR
      eRoutine= 'dsfill'
      eMessage= 'Invalid entry in numerical field'
      WRITE(eInform,*)'Invalid value for BUILDWID ',field(k)
      eAction  = 'Stopping SCICHEM'
        GO TO 999
      END IF
      DO j = 1, imit
      iset = iset + 1
      ! Assign The Field
      IF (iset <= nsec) THEN
          adsbw(iset,isdx) = fnum
          IF (fnum < 0.0) THEN
           ! Negative Value for ADSBW
          nError = IV_ERROR
          eRoutine= 'dsfill'
          eMessage= 'Negative Value for ADSBW'
          WRITE(eInform,*)'ADSBW =',fnum
          eAction  = 'Stopping SCICHEM'
            GO TO 999
          END IF
      ELSE
          ! Too Many Sectors Input
          nError = WN_ERROR
        eRoutine= 'dsfill'
        eMessage= 'Too Many Sectors Input for ADSBW'
        WRITE(eInform,*)'Number of sectors = ',iset
        eAction  = CHAR(0)
          CALL WarningMessage(0,.true.)
        IF (nError /= NO_ERROR) GO TO 999
      END IF
      END DO 
  400     CONTINUE
  iwrk2(isdx,2) = iset
! --- Fill building length information
ELSE IF (keywrd == 'BUILDLEN') THEN
  iset = iwrk2(isdx,3)
  DO 800 k = 4, ifc
    ! Change Fields To Numbers
    CALL stonum(field(k),40,fnum,imit)
    ! Check The Numerical Field
    IF (imit == -1) THEN
      nError = WN_ERROR
      eRoutine= 'dsfill'
      eMessage= 'Invalid entry in numerical field'
      WRITE(eInform,*)'Invalid value for BUILDLEN ',field(k)
      eAction  = 'Stopping SCICHEM'
        GO TO 999
    END IF
    DO j = 1, imit
      iset = iset + 1
      !  Assign The Field
      IF (iset <= nsec) THEN
        adsbl(iset,isdx) = fnum
        IF (fnum < 0.0) THEN
          ! Negative value for ADSBL
          nError = IV_ERROR
          eRoutine= 'dsfill'
          eMessage= 'Negative Value for ADSBL'
          WRITE(eInform,*)'ADSBL =',fnum
          eAction  = 'Stopping SCICHEM'
            GO TO 999
        END IF
      ELSE
        ! Too Many Sectors Input
        nError = WN_ERROR
        eRoutine= 'dsfill'
        eMessage= 'Too Many Sectors Input for ADSBL'
        WRITE(eInform,*)'Number of sectors = ',iset
        eAction  = CHAR(0)
          CALL WarningMessage(0,.true.)
        IF (nError /= NO_ERROR) GO TO 999
      END IF
    END DO
  800     CONTINUE
  iwrk2(isdx,3) = iset
! --- Fill building XBADJ information
ELSE IF (keywrd == 'XBADJ   ') THEN
  iset = iwrk2(isdx,4)
  DO 900 k = 4, ifc
    ! Change Fields To Numbers
    CALL stonum(field(k),40,fnum,imit)
    ! Check The Numerical Field
    IF (imit == -1) THEN
      nError = WN_ERROR
      eRoutine= 'dsfill'
      eMessage= 'Invalid entry in numerical field'
      WRITE(eInform,*)'Invalid value for XBADJ ',field(k)
      eAction  = 'Stopping SCICHEM'
        GO TO 999
    END IF
    DO  j = 1, imit
      iset = iset + 1
      ! Assign The Field
      IF (iset <= nsec) THEN
        adsxadj(iset,isdx) = fnum
      ELSE
        ! Too Many Sectors Input
        nError = WN_ERROR
        eRoutine= 'dsfill'
        eMessage= 'Too Many Sectors Input for XBADJ'
        WRITE(eInform,*)'Number of sectors = ',iset
        eAction  = CHAR(0)
          CALL WarningMessage(0,.true.)
        IF (nError /= NO_ERROR) GO TO 999
      END IF
    END DO
  900     CONTINUE
  iwrk2(isdx,4) = iset
! --- Fill building YBADJ information
ELSE IF (keywrd == 'YBADJ   ') THEN
  iset = iwrk2(isdx,5)
  DO 1200 k = 4, ifc
    ! Change Fields To Numbers
    CALL stonum(field(k),40,fnum,imit)
    ! Check The Numerical Field
    IF (imit == -1) THEN
      nError = WN_ERROR
      eRoutine= 'dsfill'
      eMessage= 'Invalid entry in numerical field'
      WRITE(eInform,*)'Invalid value for XBADJ ',field(k)
      eAction  = 'Stopping SCICHEM'
        GO TO 999
    END IF
    DO j = 1, imit
      iset = iset + 1
      ! Assign The Field
      IF (iset <= nsec) THEN
        adsyadj(iset,isdx) = fnum
      ELSE
        nError = WN_ERROR
        eRoutine= 'dsfill'
        eMessage= 'Too Many Sectors Input for YBADJ'
        WRITE(eInform,*)'Number of sectors = ',iset
        eAction  = CHAR(0)
          CALL WarningMessage(0,.true.)
        IF (nError /= NO_ERROR) GO TO 999
      END IF
    END DO
  1200     CONTINUE
  iwrk2(isdx,5) = iset
END IF

999  RETURN
END

SUBROUTINE srcqa
!***********************************************************************
!  SRCQA Module of ISC2 Model
!  PURPOSE   : Quality Assure Source Parameter Inputs
!
!  PROGRAMMER: Roger Brode, Jeff Wang
!  DATE      : November 8, 1993
!***********************************************************************
USE error_inc
USE sciprime_inc
IMPLICIT NONE
INTEGER :: j

!DO i = 1, numsrc
   DO j = 1,ikn-1
   ! Check Source Array Limits for Too Few Values;
   ! (Too Many Checked In DSFILL)
     IF (iwrk2(1,j) /= 0 .AND. iwrk2(1,j) < nsec) THEN
        nError = IV_ERROR
      eRoutine= 'srcqa'   
      WRITE(eInform,*)' '
      eAction  = 'Stopping SCICHEM'
        SELECT CASE(j) 
           CASE(1)
             eMessage= 'Not Enough BUILDHGTs'
           CASE(2)
             eMessage= 'Not Enough BUILDWIDs'
           CASE(3)
             eMessage= 'Not Enough BUILDLENs'
           CASE(4)
             eMessage= 'Not Enough XBADJs'
           CASE(5)
             eMessage= 'Not Enough YBADJs'
        END SELECT
        GO TO 999
     END IF
  END DO
!END DO
IF (iwrk2(1,1) == 0 .AND. iwrk2(1,2) == 0 .AND. iwrk2(1,3) == 0 &
  .AND. iwrk2(1,4) == 0 .AND. iwrk2(1,5) == 0 ) THEN
      nError = IV_ERROR
    eRoutine= 'srcqa'
      eMessage= 'All building data are zero'    
    WRITE(eInform,*)' '
    eAction  = 'Stopping SCICHEM'
      GO TO 999
END IF
999 CONTINUE
RETURN
END

SUBROUTINE fsplit(pathin,keyin,inpfld,length,delim,lflag,&
                  begfld,endfld)
!***********************************************************************
!                 FSPLIT Module of ISC2 Model
!
!        PURPOSE: SPLIT A FIELD, BASED ON AN INPUT DELIMITER
!                 CHARACTER.  SETS A LOGICAL FLAG AND RETURNS
!                 BEGINNING AND ENDING PARTS OF FIELD.
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!***********************************************************************
USE error_inc
IMPLICIT NONE
CHARACTER             :: chk, inpfld*(*), delim, begfld*8, endfld*8, modnam*6,&
                   pathin*2, keyin*8
INTEGER             :: i,idelm,length
LOGICAL             :: lflag, mend, in

!     Variable Initialization
i   = length
idelm = length
begfld  = ' '
endfld  = ' '
mend  = .false.
in    = .false.
lflag = .false.

!     Begin the Processing
DO WHILE (.not.mend .and. i >= 1)
  chk = inpfld(i:i)
  IF (chk /= ' ') THEN
    in = .true.
    ! Check for the Group Delimiter
    IF (.not.lflag .and. chk==delim) THEN
      lflag = .true.
      idelm = i
      endfld = inpfld(i+1:length)
      IF (i == 1) THEN
        ! Invalid Range Parameter
        nError = WN_ERROR
        eRoutine= 'fsplit'
        eMessage= 'Invalid Range Parameter'
        WRITE(eInform,*)'Delimiter =',chk
        eAction  = 'Stopping SCICHEM'
          GO TO 999
      END IF
    ELSE IF (lflag .and. chk==delim) THEN
      ! More Than One Delimiter in a Field
      nError = WN_ERROR
      eRoutine= 'fsplit'
      eMessage= 'More Than One Delimiter in a Field'
      WRITE(eInform,*)'Delimiter =',chk
      eAction  = 'Stopping SCICHEM'
          GO TO 999
    END IF
  ELSE IF (in .and. chk==' ') THEN
    mend = .true.
    IF (lflag) THEN
      begfld = inpfld(1:idelm-1)
    ELSE
      begfld = inpfld
    END IF
  END IF
  i = i - 1
END DO

IF (.not. mend) THEN
  IF (lflag) THEN
    begfld = inpfld(1:idelm-1)
  ELSE
    begfld = inpfld
  END IF
END IF

!     In Case Of No Delimiter, Set ENDFLD = BEGFLD
IF (.not. lflag) THEN
  endfld = begfld
END IF

999 RETURN
END

SUBROUTINE stonum(strvar,length,fnum,imuti)
!***************************************************************
!                 STONUM Module of ISC2 Model
!
!        PURPOSE: Gets Number From A String Variable
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input String Variable
!                 Length of Character String
!
!        OUTPUTS: Numbers
!
!*****************************************************************
USE error_inc
IMPLICIT NONE
CHARACTER           :: strvar*(*), chk, nums*10
INTEGER             :: i,imuti,length
REAL              :: fnum, cnum, fdec, fdc1, head
LOGICAL             :: mend, in, nmark, pmark, dmark, mmark, emark

!     Variable Initialization
nums = '0123456789'
mend  = .false.
in    = .false.
nmark = .false.
pmark = .false.
dmark = .false.
mmark = .false.
emark = .false.
cnum  = 0.0
i     = 1
imuti = 1
fdec  = 1.

!     Beginning the Processing
DO WHILE (.not.mend .and. i <= length)
  chk = strvar(i:i)
  IF (chk /= ' ') THEN
    in = .true.
    IF (chk >='0' .and. chk <= '9') THEN
      !  CHK is a Number, Assign a Value
      IF (.not. dmark) THEN
        cnum = cnum*10.+ FLOAT(INDEX(nums,chk)-1)
      ELSE
        fdec = fdec/10.
        fdc1 = fdec*FLOAT(INDEX(nums,chk)-1)
        cnum = cnum+fdc1
      END IF
    ELSE
      !  Handle The E-Type Real Number
      IF (.not.emark .and. chk=='E') THEN
        emark = .true.
        IF (.not.nmark) THEN
          head = cnum
        ELSE
          head = -cnum
        END IF
        dmark = .false.
        nmark = .false.
        cnum = 0.0
        ELSE IF (.not.pmark .and. chk=='+') THEN
          !                 Set Positive Indicator
          pmark = .true.
        ELSE IF (.not.nmark .and. chk=='-') THEN
          !                 Set Negative Indicator
          nmark = .true.
        ELSE IF (.not.dmark .and. chk=='.') THEN
          !                 Set Decimal Indicator
          dmark = .true.
        ELSE IF (.not.mmark .and. chk=='*' .and. &
                    .not.nmark) THEN
          !                 Set Repeat Number
          mmark = .true.
          imuti = INT(cnum)
          cnum = 0.0
        ELSE
        !                 Error Occurs, Set Switch and Exit Out Of The Subroutine
           nError   = IV_ERROR
             eRoutine = 'STONUM'
                   eMessage = 'Invalid Real number'
             eAction  = 'Stopping SCICHEM'
                 GO TO 9999
      END IF
    END IF
  ELSE IF (in .and. chk==' ') THEN
  mend = .true.
  END IF
  i = i + 1
END DO
fnum = cnum
!     In Case Of Negative Field, Value Set to Negative
IF (nmark) THEN
  fnum = -fnum
END IF
!     In Case of E-Format, Check for Exponents Out of Range
IF (emark .and. ABS(fnum) <= 30.) THEN
  fnum = head*10**(fnum)
ELSE IF (emark .and. ABS(fnum) .gt. 30.) THEN
  IF (fnum .lt. 0.0) THEN
    fnum = 0.0
  ELSE IF (fnum .gt. 0.0) THEN
    fnum = head * 10**30.
  END IF
    nError   = IV_ERROR
  eRoutine = 'STONUM'
    eMessage = 'Invalid Real number'
  eAction  = 'Stopping SCICHEM'
  GO TO 9999
END IF
GO TO 1000

!     Set Error Switch for Illegal Numerical Field (WRITE Message and Handle
!     Error in Calling Routine)
9999 imuti = -1


1000 RETURN
END

SUBROUTINE lwrupr
!***********************************************************************
!  LWRUPR Module of ISC2 Model
!
!  PURPOSE: Transfer All Characters From Lower Case To
!           Upper Case (Using INDEX Intrinsic Function)
!           Note that the CHAR*80 RUNST1 Variable Includes
!           the Original Case for Echoing and for Later Use
!           To Retrieve Filenames.
!
!  PROGRAMMER: Roger Brode, Kevin Stroupe
!
!  DATE:    March 2, 1992
!
!  INPUTS:  Input Runstream Card Image (80 Character Array)
!                 Number of Characters in String, PARAMETER ISTRG
!
!  OUTPUTS: Input Runstream Card Image (Array) in Uppercase
!***********************************************************************

USE sciprime_inc
IMPLICIT NONE
INTEGER  i,indchk
CHARACTER upcase*26
CHARACTER lwcase*26

! Variable Initializations
DATA upcase/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
DATA lwcase/'abcdefghijklmnopqrstuvwxyz'/

DO i = 1,istrg
   IF (runst(i) /= ' ') THEN
      indchk = INDEX(lwcase,runst(i))
      IF (indchk /= 0) THEN
          runst(i) = upcase(indchk:indchk)
      END IF
   END IF
END DO

RETURN
END

SUBROUTINE setidg(inid,idchr1,idnum,idchr2)
!***********************************************************************
!                 SETIDG Module of ISC2 Model

!        PURPOSE: Find A Source ID's Character Part and
!                 Numerical Part

!        PROGRAMMER: Jeff Wang, Roger Brode, Kevin Stroupe

!        DATE:    March 2, 1992

!        INPUTS:  Input Field Parameters

!        OUTPUTS: An Initial Character String, a Number, and
!                 a Second Character String

!***********************************************************************
IMPLICIT NONE

!     Variable Declarations
CHARACTER                  :: inid*8, idchr1*8, idchr2*8, chki, numid*40
INTEGER                    :: i,idnum,ii,istr,imit
REAL                       :: fnum
LOGICAL                    :: hit

!     Variable Initializations
i = 8
numid  = ' '
idchr1 = ' '
idchr2 = ' '
idnum  = 0
hit    = .false.

! Find The Length of the Input Field, II (<= 8)
DO WHILE (.not.hit .and. i>=1)
  chki = inid(i:i)
  IF (chki /= ' ') THEN
    ii = i
    hit = .true.
  END IF
  i = i - 1
END DO

!     Divide the Input Id into 3 parts (char1, int, and char2)
i = 1
istr = i
chki = inid(i:i)
!     Get first character part
DO WHILE (chki < '0' .or. chki > '9')
  idchr1 = inid(istr:i)
  i = i + 1
  IF (i > ii) THEN
    GO TO 20
  ELSE
    chki = inid(i:i)
  END IF
END DO

!     Get integer part
istr = i
DO WHILE (chki >= '0' .and. chki <= '9')
  numid = inid(istr:i)
  i = i + 1
  IF (i > ii) THEN
    GO TO 20
  ELSE
    chki = inid(i:i)
  END IF
END DO

!     Get second character part
istr = i
DO WHILE (i <= ii)
  idchr2 = inid(istr:i)
  i = i + 1
  IF (i > ii) THEN
    GO TO 20
  ELSE
    chki = inid(i:i)
  END IF
END DO

20   CONTINUE

!     Convert Numeric Part to Integer Variable
CALL stonum(numid,40,fnum,imit)
idnum = INT(fnum)

RETURN
END 

SUBROUTINE sigy(xarg,syout)
!***********************************************************************
!                 SIGY Module of ISC2 Model

!        PURPOSE: Calculates Sigma-y Values From Dispersion Curves

!        PROGRAMMER: Roger Brode, Jeff Wang

!        DATE:    March 2, 1992

!        MODIFIED:   To use calling argument for output
!                    R. W. Brode, PES, Inc. - 9/30/94

!        INPUTS:  Downwind Distance
!                 Stability Class
!                 Rural or Urban Dispersion Option

!        OUTPUTS: Lateral Dispersion Coefficient, SYOUT

!        CALLED FROM:   PDIS
!                       VDIS
!                       ADIS
!                       SYENH
!                       DHPSS
!***********************************************************************
USE sciprime_inc
IMPLICIT NONE
REAL  dtorad
PARAMETER (dtorad = 0.017453293)
REAL    xkm,xarg,th,syout



!     Convert Distance to km
      xkm = xarg * 0.001

!     Determine Sigma-y Based on RURAL/URBAN, Stability Class, and Distance.
!     Stability Classes are Checked in the Order 4, 5, 6, 1, 2, 3
!     For Optimization, Since Neutral and Stable are Generally the Most
!     Frequent Classes.

IF (rural) THEN
 IF (kst == 4) THEN
    th = (8.3330 - 0.72382*ALOG(xkm)) * dtorad
 ELSE IF (kst == 5) THEN
    th = (6.25 - 0.54287*ALOG(xkm)) * dtorad
 ELSE IF (kst == 6) THEN
    th = (4.1667 - 0.36191*ALOG(xkm)) * dtorad
 ELSE IF (kst == 1) THEN
    th = (24.1667 - 2.5334*ALOG(xkm)) * dtorad
 ELSE IF (kst == 2) THEN
    th = (18.333 - 1.8096*ALOG(xkm)) * dtorad
 ELSE IF (kst == 3) THEN
    th = (12.5 - 1.0857*ALOG(xkm)) * dtorad
 END IF

!        NOTE THAT 465.11628 = 1000. (m/km) / 2.15

 syout = 465.11628 * xkm * TAN(th)
ELSE IF (urban) THEN
 IF (kst == 4) THEN
    syout = 160.*xkm/SQRT(1.+0.4*xkm)
 ELSE IF (kst >= 5) THEN
    syout = 110.*xkm/SQRT(1.+0.4*xkm)
 ELSE IF (kst <= 2) THEN
    syout = 320.*xkm/SQRT(1.+0.4*xkm)
 ELSE IF (kst == 3) THEN
    syout = 220.*xkm/SQRT(1.+0.4*xkm)
 END IF
END IF

RETURN
END


SUBROUTINE sigz(xarg,szout)
!***********************************************************************
!                 SIGZ Module of ISC2 Model

!        PURPOSE: Calculates Sigma-z Values From Dispersion Curves

!        PROGRAMMER: Roger Brode, Jeff Wang

!        DATE:    March 2, 1992

!        MODIFIED:   To use calling argument for output
!                    R. W. Brode, PES, Inc. - 9/30/94

!        INPUTS:  Downwind Distance
!                 Stability Class
!                 Rural or Urban Dispersion Option

!        OUTPUTS: Vertical Dispersion Coefficient, SZOUT

!        CALLED FROM:   PDIS
!                       VDIS
!                       ADIS
!                       SZENH
!                       DHPSS
!***********************************************************************
USE sciprime_inc
IMPLICIT NONE
REAL xkm,xarg,a,b,xmin,xmax,szout

!     Convert Distance to km
      xkm = xarg * 0.001

!     Determine Sigma-z Based on RURAL/URBAN, Stability Class, and Distance.
!     Stability Classes are Checked in the Order 4, 5, 6, 1, 2, 3
!     For Optimization, Since Neutral and Stable are Generally the Most
!     Frequent Classes.

IF (rural) THEN
!        Retrieve Coefficients, A and B                     ---   CALL SZCOEF
 CALL szcoef(xkm,a,b,xmin,xmax)
 szout = a*xkm**b
ELSE IF (urban) THEN
 IF (kst == 4) THEN
    szout = 140.*xkm/SQRT(1.+0.3*xkm)
 ELSE IF (kst >= 5) THEN
    szout = 80.*xkm/SQRT(1.+1.5*xkm)
 ELSE IF (kst <= 2) THEN
    szout = 240.*xkm*SQRT(1.+xkm)
 ELSE IF (kst == 3) THEN
    szout = 200.*xkm
 END IF
END IF

RETURN
END

SUBROUTINE szcoef(xkm,a,b,xmin,xmax)
!***********************************************************************
!                 SZCOEF Module of ISC2 Model

!        PURPOSE: Determines Coefficients and Ranges for Rural Sigma-z

!        PROGRAMMER: Roger Brode, Jeff Wang

!        DATE:    March 2, 1992

!        INPUTS:  KST     Stability Category
!                 XKM     Downwind Distance (km)

!        OUTPUTS: Coefficients A and B and Distance Range XMIN and XMAX

!        CALLED FROM:   SIGZ
!                       XVZ
!***********************************************************************

USE sciprime_inc
IMPLICIT NONE
REAL    xkm,a,b,xmin,xmax


IF (kst == 4) THEN
 IF (xkm <= .30) THEN
    a = 34.459
    b = 0.86974
    xmin = 0.
    xmax = 0.30
 ELSE IF (xkm <= 1.0) THEN
    a = 32.093
    b = 0.81066
    xmin = 0.30
    xmax = 1.
 ELSE IF (xkm <= 3.0) THEN
    a = 32.093
    b = 0.64403
    xmin = 1.
    xmax = 3.
 ELSE IF (xkm <= 10.) THEN
    a = 33.504
    b = 0.60486
    xmin = 3.
    xmax = 10.
 ELSE IF (xkm <= 30.) THEN
    a = 36.650
    b = 0.56589
    xmin = 10.
    xmax = 30.
 ELSE
    a = 44.053
    b = 0.51179
    xmin = 30.
    xmax = 100.
 END IF

ELSE IF (kst == 5) THEN
 IF (xkm <= .10) THEN
    a = 24.26
    b = 0.83660
    xmin = 0.
    xmax = .10
 ELSE IF (xkm <= .30) THEN
    a = 23.331
    b = 0.81956
    xmin = 0.10
    xmax = 0.30
 ELSE IF (xkm <= 1.0) THEN
    a = 21.628
    b = 0.75660
    xmin = 0.30
    xmax = 1.
 ELSE IF (xkm <= 2.0) THEN
    a = 21.628
    b = 0.63077
    xmin = 1.
    xmax = 2.
 ELSE IF (xkm <= 4.0) THEN
    a = 22.534
    b = 0.57154
    xmin = 2.
    xmax = 4.
 ELSE IF (xkm <= 10.) THEN
    a = 24.703
    b = 0.50527
    xmin = 4.
    xmax = 10.
 ELSE IF (xkm <= 20.) THEN
    a = 26.97
    b = 0.46713
    xmin = 10.
    xmax = 20.
 ELSE IF (xkm <= 40.) THEN
    a = 35.42
    b = 0.37615
    xmin = 20.
    xmax = 40.
 ELSE
    a = 47.618
    b = 0.29592
    xmin = 40.
    xmax = 100.
 END IF

ELSE IF (kst == 6) THEN
 IF (xkm <= .20) THEN
    a = 15.209
    b = 0.81558
    xmin = 0.
    xmax = 0.20
 ELSE IF (xkm <= .70) THEN
    a = 14.457
    b = 0.78407
    xmin = 0.20
    xmax = 0.70
 ELSE IF (xkm <= 1.0) THEN
    a = 13.953
    b = 0.68465
    xmin = 0.7
    xmax = 1.
 ELSE IF (xkm <= 2.0) THEN
    a = 13.953
    b = 0.63227
    xmin = 1.
    xmax = 2.
 ELSE IF (xkm <= 3.0) THEN
    a = 14.823
    b = 0.54503
    xmin = 2.
    xmax = 3.
 ELSE IF (xkm <= 7.0) THEN
    a = 16.187
    b = 0.46490
    xmin = 3.
    xmax = 7.
 ELSE IF (xkm <= 15.) THEN
    a = 17.836
    b = 0.41507
    xmin = 7.
    xmax = 15.
 ELSE IF (xkm <= 30.) THEN
    a = 22.651
    b = 0.32681
    xmin = 15.
    xmax = 30.
 ELSE IF (xkm <= 60.) THEN
    a = 27.074
    b = 0.27436
    xmin = 30.
    xmax = 60.
 ELSE
    a = 34.219
    b = 0.21716
    xmin = 60.
    xmax = 100.
 END IF

ELSE IF (kst == 1) THEN
 IF (xkm <= 0.10) THEN
    a = 122.8
    b = 0.94470
    xmin = 0.
    xmax = 0.1
 ELSE IF (xkm <= 0.15) THEN
    a = 158.080
    b = 1.05420
    xmin = 0.1
    xmax = 0.15
 ELSE IF (xkm <= 0.20) THEN
    a = 170.22
    b = 1.09320
    xmin = 0.15
    xmax = 0.20
 ELSE IF (xkm <= 0.25) THEN
    a = 179.52
    b = 1.12620
    xmin = 0.20
    xmax = 0.25
 ELSE IF (xkm <= 0.30) THEN
    a = 217.41
    b = 1.2644
    xmin = 0.25
    xmax = 0.30
 ELSE IF (xkm <= 0.40) THEN
    a = 258.89
    b = 1.4094
    xmin = 0.30
    xmax = 0.40
 ELSE IF (xkm <= 0.50) THEN
    a = 346.75
    b = 1.72830
    xmin = 0.40
    xmax = 0.50
 ELSE
    a = 453.85
    b = 2.11660
    xmin = 0.50
    xmax = 100.
 END IF

ELSE IF (kst == 2) THEN
 IF (xkm <= 0.20) THEN
    a = 90.673
    b = 0.93198
    xmin = 0.
    xmax = 0.20
 ELSE IF (xkm <= 0.40) THEN
    a = 98.483
    b = 0.98332
    xmin = 0.20
    xmax = 0.40
 ELSE
    a = 109.3
    b = 1.0971
    xmin = 0.40
    xmax = 100.
 END IF

ELSE IF (kst == 3) THEN
    a = 61.141
    b = 0.91465
    xmin = 0.
    xmax = 100.
END IF

RETURN
END

SUBROUTINE xvy(sigy,xyout)
!***********************************************************************
!                 XVY Module of ISC2 Model
! ----------------------------------------------------------------------
! ---    ISC-PRIME     Version 1.0    Level 970812              Modified
! ---        D. Strimaitis
! ---        Earth Tech, Inc.
!            Prepared for EPRI under contract WO3527-01
! ----------------------------------------------------------------------

!        PURPOSE: Calculates Lateral Virtual Distances

!        PROGRAMMER: Roger Brode, Jeff Wang

!        DATE:    March 2, 1992

!        MODIFIED:   To use calling argument for output
!                    R. W. Brode, PES, Inc. - 9/30/94

!        INPUTS:  Initial Dispersion, SYINIT
!                 Stability Class
!                 Rural or Urban Dispersion Option

!        OUTPUTS: Lateral Virtual Distance, XYOUT (m)

!        CALLED FROM:   VDIS
!                       SYENH
!                       WAKE_DFSN
!***********************************************************************
USE sciprime_inc
IMPLICIT  NONE

!     Variable Declarations
REAL a(6),sp(6),sq(6)
REAL a2,b,sigy,sy2,xyout

!     Variable Initializations
DATA a/0.32,0.32,0.22,0.16,0.11,0.11/, &
     b/0.0004/, &
    sp/.004781486,.006474168,.009684292,.014649868,.019584802, &
           0.029481132/, &
    sq/1.1235955,1.1086475,1.0905125,1.0881393,1.0857763, &
           1.0881393/
 
! --- PRIME -------------------------------------------------------
! --- Initial sigma is provided as an argument
! --- The sigma in MAIN1 common is NOT USED
! -----------------------------------------------------------------

IF (rural) THEN
 xyout = (sigy  *sp(kst))**sq(kst) * 1000.
ELSE IF (urban) THEN
 a2 = a(kst) * a(kst)
 sy2 = sigy   * sigy
 xyout = (b*sy2 + SQRT(b*b*sy2*sy2 + 4.*a2*sy2)) / (2.*a2)
END IF

RETURN
END

SUBROUTINE xvz(sigz,xarg,xzout)
!***********************************************************************
!                 XVZ Module of ISC2 Model
! ----------------------------------------------------------------------
! ---    ISC-PRIME     Version 1.0    Level 970812              Modified
! ---        D. Strimaitis
! ---        Earth Tech, Inc.
!            Prepared for EPRI under contract WO3527-01
! ----------------------------------------------------------------------

!        PURPOSE: Calculates Vertical Virtual Distances

!        PROGRAMMER: Roger Brode, Jeff Wang

!        DATE:    March 2, 1992

!        MODIFIED:   To use calling argument for output
!                    R. W. Brode, PES, Inc. - 9/30/94

!        MODIFIED:   To Change TOL from 1.0E-5 to 1.0E-4 - 9/29/92

!        INPUTS:  Initial Dispersion, SZINIT
!                 Downwind Distance
!                 Stability Class
!                 Rural or Urban Dispersion Option

!        OUTPUTS: Vertical Virtual Distance, XZOUT (m)

!        CALLED FROM:   VDIS
!                       SZENH
!***********************************************************************
USE sciprime_inc
IMPLICIT  NONE

!     Variable Declarations
INTEGER n
REAL aa(6), bb(6), xxz(6)
REAL az,bz,sigz,xzout,xkm,xarg,xmin,xmax
REAL a2,b,sz2,xzero,tol,acoef,bcoef,ccoef

!     Variable Initializations
DATA aa/0.24,0.24,0.2,0.14,0.08,0.08/
DATA bb/.001,.001,.0,.0003,.0015,.0015/
 
! --- PRIME -------------------------------------------------------
! --- Initial sigma is provided as an argument
! --- The sigma in MAIN1 common is NOT USED
! -----------------------------------------------------------------

IF (sigz   <= 0.01) THEN
  xzout = 0.
ELSE IF (rural) THEN
!        Solve Iteratively
!        Convert Distance to km
  xkm = xarg * 0.001
!        Initial Guess of 10 m
  xxz(1) = 0.01
  DO 10 n = 1, 5
!           Retrieve Coef. AZ & BZ, Range XMIN & XMAX    ---   CALL SZCOEF
    CALL szcoef((xxz(n)+xkm),az,bz,xmin,xmax)
    xxz(n+1) = (sigz  /az) ** (1./bz)
!           Check for X+XZ falling within Range of Coefficients
    IF((xxz(n+1)+xkm)>=xmin .and. (xxz(n+1)+xkm)<=xmax) THEN
       xzout = xxz(n+1) * 1000.
!              EXIT LOOP
       GO TO 999
    END IF
  10 CONTINUE
!        If No Convergence in Loop, Use Smaller of Last Two Estimates,
!        Consistent With Original ISC Model - Version 2
 xzout = AMIN1(xxz(5),xxz(6)) * 1000.

ELSE IF (urban) THEN
 IF (kst >= 4) THEN
    a2  = aa(kst) * aa(kst)
    b   = bb(kst)
    sz2 = sigz   * sigz
    xzout  = (b*sz2 + SQRT(b*b*sz2*sz2 + 4.*a2*sz2)) / (2.*a2)
 ELSE IF (kst <= 2) THEN
!           Set Initial Guess and Tolerance Limit for Cubic Equation
    xzero = 4. * sigz
    tol = 1.0e-4
!           Set Cubic Coefficients, ACOEF, BCOEF, and CCOEF
    acoef = 1./bb(kst)
    bcoef = 0.0
    ccoef = -1. * sigz  *sigz  /(aa(kst)*aa(kst) * bb(kst))
!           Solve Cubic Equation                          ---   CALL CUBIC
    CALL cubic(acoef,bcoef,ccoef,xzero,tol,xzout)
 ELSE IF (kst == 3) THEN
    xzout = sigz  /aa(kst)
 END IF
END IF

999  RETURN
END

SUBROUTINE cubic(a,b,c,zinit,tol,ziter)
!***********************************************************************
!                 CUBIC Module of ISC2 Model

!        PURPOSE: Solves Cubic Equation Using Newton's Method

!        PROGRAMMER: Roger Brode, Jeff Wang

!        DATE:    March 2, 1992

!        INPUTS:  Coefficients (A, B and C) of Cubic Equation
!                 Initial Guess for Variable
!                 Tolerance Level for Iteration

!        OUTPUTS: Solution to Cubic Equation;
!                    Z**3 + A*Z**2 + B*Z + C = 0

!        CALLED FROM:   DHPSS
!                       XVZ
!***********************************************************************
USE error_inc
IMPLICIT  NONE

!     Variable Declarations
INTEGER n
REAL z(25)
REAL fz,fp,a,b,c
REAL ziter,zinit
REAL tol

!     Assign Initial Guess to Z(1)
z(1) = zinit

!     Begin Iterative LOOP (24 iterations)
DO 20 n = 1, 24
!        Calculate Cubic Function and First Derivative With Current Guess
 fz = z(n)*z(n)*z(n) + a*z(n)*z(n) + b*z(n) + c
 fp = 3.*z(n)*z(n) + 2.*a*z(n) + b
!        Calculate New Guess
 z(n+1) = z(n) - fz/fp
!        Check successive iterations for specified tolerance level
 IF (ABS(z(n+1) - z(n)) <= tol) THEN
    ziter = z(n+1)
!           Exit Loop
    GO TO 999
 END IF
20   CONTINUE
!     End Iterative LOOP

!     If No Convergence In Loop, Then Use Average of Last Two Estimates,
!     and Write Information Message

nError = WN_ERROR
eRoutine= 'Cubic'
eMessage= 'No Convergence In Loop'
WRITE(eInform,*)''
eAction  = CHAR(0)
CALL WarningMessage(0,.true.)
IF (nError /= NO_ERROR) GO TO 999

ziter = 0.5 * (z(24) + z(25))

999  RETURN
END
