
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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WRITE_LOG_HEADER ( FUNIT, IPROC, IPROCTOT )

C   Routine prints the CMAQ header, disclaimer, and I/O API notice to
C   the ascii file identified by FUNIT. It also prints the rank of the
C   processor.
C   16 Nov 17 B.Murphy: Created
C-----------------------------------------------------------------------

      IMPLICIT NONE

C Arguments:

      INTEGER, INTENT( IN ) :: FUNIT        ! File Unit
      INTEGER, INTENT( IN ) :: IPROC        ! Rank of Current Processor
      INTEGER, INTENT( IN ) :: IPROCTOT     ! Total Number of Processors

C-----------------------------------------------------------------------

      ! Write CMAQ Header 
      WRITE( FUNIT, '(/A)'),'================================================================================'
      WRITE( FUNIT, '(A)' ),'|                                                                              |'
      WRITE( FUNIT, '(A)' ),'|               The Community Multiscale Air Quality (CMAQ) Model              |'
      WRITE( FUNIT, '(A)' ),'|                                   Version 5.2                                |'
      WRITE( FUNIT, '(A)' ),'|                             Released June 30, 2017                           |'
      WRITE( FUNIT, '(A)' ),'|                                                                              |'
      WRITE( FUNIT, '(A)' ),'|                           Built and Maintined by the                         |'
      WRITE( FUNIT, '(A)' ),'|                      National Exposure Research Laboratory                   |'
      WRITE( FUNIT, '(A)' ),'|                        Office of Research and Development                    |'
      WRITE( FUNIT, '(A)' ),'|                   United States Environmental Protection Agency              |'
      WRITE( FUNIT, '(A)' ),'|                                                                              |'
      WRITE( FUNIT, '(A)' ),'|                             https://www.epa.gov/cmaq                         |'
      WRITE( FUNIT, '(A)' ),'|                         https://www.github.com/USEPA/cmaq                    |'
      WRITE( FUNIT, '(A)' ),'|                                                                              |'
      WRITE( FUNIT, '(A/)'),'================================================================================'
      
      ! Write EPA Disclaimer
      WRITE( FUNIT, '(/A)'),'--------------------------------------------------------------------------'
      WRITE( FUNIT, '(A)' ),'|  The Community Multiscale Air Quality (CMAQ) system software is in     |'
      WRITE( FUNIT, '(A)' ),'|  continuous development by various groups and is based on information  |'
      WRITE( FUNIT, '(A)' ),'|  from these groups: Federal Government employees, contractors working  |'
      WRITE( FUNIT, '(A)' ),'|  within a United States Government contract, and non-Federal sources   |'
      WRITE( FUNIT, '(A)' ),'|  including research institutions.  These groups give the Government    |'
      WRITE( FUNIT, '(A)' ),'|  permission to use, prepare derivative works of, and distribute copies |'
      WRITE( FUNIT, '(A)' ),'|  of their work in the CMAQ system to the public and to permit others   |'
      WRITE( FUNIT, '(A)' ),'|  to do so.  The United States Environmental Protection Agency          |'
      WRITE( FUNIT, '(A)' ),'|  therefore grants similar permission to use the CMAQ system software,  |'
      WRITE( FUNIT, '(A)' ),'|  but users are requested to provide copies of derivative works or      |'
      WRITE( FUNIT, '(A)' ),'|  products designed to operate in the CMAQ system to the United States  |'
      WRITE( FUNIT, '(A)' ),'|  Government without restrictions as to use by others.  Software        |'
      WRITE( FUNIT, '(A)' ),'|  that is used with the CMAQ system but distributed under the GNU       |'
      WRITE( FUNIT, '(A)' ),'|  General Public License or the GNU Lesser General Public License is    |'
      WRITE( FUNIT, '(A)' ),'|  subject to their copyright restrictions.                              |'
      WRITE( FUNIT, '(A/)'),'--------------------------------------------------------------------------'
 
      ! Write IOAPI Notice with Header and Footer
      WRITE( FUNIT, '(/A)'),'-------I/O API Notice------------------------------------------'
      WRITE( FUNIT, '(A)' ),'|This program uses the EPA-AREAL/MCNC-EnvPgms/BAMS Models-3   |'
      WRITE( FUNIT, '(A)' ),'|I/O Applications Programming Interface, [I/O API] which is   |'
      WRITE( FUNIT, '(A)' ),'|built on top of the netCDF I/O library (Copyright 1993, 1996 |'
      WRITE( FUNIT, '(A)' ),'|University Corporation for Atmospheric Research/Unidata      |'
      WRITE( FUNIT, '(A)' ),'|Program) and the PVM parallel-programming library (from      |'
      WRITE( FUNIT, '(A)' ),'|Oak Ridge National Laboratory).  Copyright (C) 1992-2002 MCNC|'
      WRITE( FUNIT, '(A)' ),'|and Carlie J. Coats, Jr., and 2003-2006 Baron Advanced       |'
      WRITE( FUNIT, '(A)' ),'|Meteorological Systems, LLC and released under the GNU LGPL  |'
      WRITE( FUNIT, '(A)' ),'|License, version 2.1.  See URL                               |'
      WRITE( FUNIT, '(A)' ),'|                                                             |'
      WRITE( FUNIT, '(A)' ),'|    http://www.baronams.com/products/ioapi/LGPL.txt          |'
      WRITE( FUNIT, '(A)' ),'|                                                             |'
      WRITE( FUNIT, '(A)' ),'|for conditions of use.                                       |'
      WRITE( FUNIT, '(A/)'),'---------------------------------------------------------------'

      ! Write Processor Rank and Total Processors
      WRITE( FUNIT, '(/,/,A,I3,A,I3,A/)' ),'This Process is Rank ',IPROC,
     &                                     '. Final Process is Rank ',IPROCTOT,'.'


      END
