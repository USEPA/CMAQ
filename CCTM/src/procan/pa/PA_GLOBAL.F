
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

C......................................................................
      MODULE PA_GLOBAL
 
C Contains: Mech and Grid data used in the Process Analysis Control Program
 
C Dependent Upon: Nothing
C Revision History
C Prototype created by Bill Hutzell, June, 2014 based in PA_GLOBAL.EXT
C  from PROCAN version 5.0.2
C PA_GLOBAL.EXT History:
C  Created by Jerry Gipson, May, 1997
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Modified May 2002 by Jerry Gipson to increase number of products (for saprc99)
C  Aug 2011 Jeff Young: Removed domain data
  
C......................................................................

      USE RXNS_DATA, Only: ! chemical mechanism arrays and data 
     & NRXNS, MXPRD, NUMB_MECH_SPC,
     & RXLABEL, IRR, CHEMISTRY_SPC, NREACT, IRXBITS, SC, NPRDCT

      IMPLICIT NONE


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Dimensioning parameters for species and mechanism data
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      INTEGER, PARAMETER :: MXSPEC = 1000 ! Number of species in mechanism
      
      INTEGER, PARAMETER :: MXALLSPC = 4 * MXSPEC + 1

      INTEGER, PARAMETER :: MXRXNS   = NRXNS ! Maximum number of Reactions in mechanism
      
      INTEGER, PARAMETER :: MXRPP    = MXPRD + 3 ! Maximum number of Products and reactants

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Declarations for species and mechanism data
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      END MODULE PA_GLOBAL
 
