!*******************************************************************************
!$RCSfile: version.f90,v $
!$Revision: 1.5 $
!$Date: 2010/10/29 17:35:30 $
!*******************************************************************************

subroutine set_version(iversion_code)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: Set version number (saved on prj file)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY:  
!
! 03 NOV 2000 :  Changed version from 1220 to 1221. - BC
! 29 NOV 2000 :  Changed version from 1221 to 1222. - DSH
! 08 JAN 2001 :  Changed version from 1222 to 1223. - RIS
! 02 MAR 2001 :  Changed version from 1224 to 1225. - RIS 
!                 (1224 Version no. was not commited)
! 13 MAR 2001 :  Changed version from 1225 to 1226. - RIS 
! 15 MAR 2001 :  Changed version from 1226 to 1227. - RIS
! 12 APR 2001 :  Changed version from 1227 to 1300. - RIS  
! 07 MAY 2001 :  Changed version from 1300 to 1301. - RIS  
! 09 MAY 2001 :  Changed version from 1300 to 1302. - RIS  
! 24 JUL 2001 :  Changed version from 1302 to 1303. - RIS
! 23 AUG 2001 :  Changed version from 1303 to 1400. - BC 
! 24 SEP 2001 :  Changed version from 1400 to 1401. - BC 
! 27 SEP 2001 :  Incorporated same changes as from 
!                version 1303 to 1304 in branch for 1402 - BC
! 18 DEC 2001 :  Changed version from 1402 to 1403. - BC
!                 (1403 version.f90 was not commited)
! 19 MAR 2002 :  Changed version from 1403 to 1500. - BC 
! 19 JUL 2002 :  Changed version from 1500 to 1501. - BC 
! 15 OCT 2002 :  Changed version from 1501 to 1502. - RIS 
! 27 NOV 2003 :  Changed version from 1502 to 1600. - BC
!                ( Standard fortran and cleaned up code)
! 03 JAN 2004 :  Changed version from 1600 to 1601. - BC
! 13 OCT 2005 :  Changed version from 1601 to 1800. - PK
! 18 OCT 2006 :  Changed version from 1800 to 1900. - BC
! 01 DEC 2006 :  Changed version from 1900 to 2000. - BC
!                ( Add MPI calls for multiprocessor runs )
! 15 SEP 2010 :  Changed version from 2000 to 2100. - BC
!                ( Add cpp directives 'parallel')
! 12 SEP 2011 :  Changed version from 2100 to 2200. - PK
!                read i_hostspec_list from project file
!*******************************************************************************
 
implicit none

! --- ARGUMENTS
 
integer iversion_code  !Version number

!**************************** VERSION 2.100 ************************************

iversion_code = 2200

return

end
