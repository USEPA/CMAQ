!*******************************************************************************
!$RCSfile: error_inc.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
module error_inc
save

!=======================================================================
!      ERROR Include file
!=======================================================================

      integer  EOF_ERROR
      integer  NO_ERROR
      integer  NF_ERROR
      integer  SZ_ERROR
      integer  IV_ERROR
      integer  OP_ERROR
      integer  RD_ERROR
      integer  WR_ERROR
      integer  S0_ERROR
      integer  API_ERROR
      integer  SW_ERROR
      integer  WN_ERROR
      integer  AB_ERROR
      integer  VN_ERROR
      integer  DM_ERROR
      integer  UK_ERROR

      parameter (EOF_ERROR =  -1 )    !EOF Error
      parameter (NO_ERROR =   0 )     !No error
      parameter (NF_ERROR =   1 )     !Not found error
      parameter (SZ_ERROR =   2 )     !Size error
      parameter (IV_ERROR =   3 )     !Invalid value error
      parameter (OP_ERROR =   4 )     !Open error
      parameter (RD_ERROR =   5 )     !Read error
      parameter (S0_ERROR =   6 )     !Switch Zero error
      parameter (WR_ERROR =   7 )     !Write error
      parameter (API_ERROR =  8 )     !API error
      parameter (SW_ERROR =   9 )     !Switch error
      parameter (WN_ERROR =  10 )     !Warning error
      parameter (AB_ERROR =  11 )     !Abort error
      parameter (VN_ERROR =  12 )     !Version error
      parameter (DM_ERROR =  13 )     !Special Domain error
      parameter (UK_ERROR =  99 )     !Unknown error

      character*64 eRoutine
      character*128 eMessage
      character*128 eInform
      character*128 eAction

      integer     nError

!=======================================================================
!      SGI TRIM Include file
!=======================================================================

end module error_inc
