
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

      SUBROUTINE AERO_SENS( SCASI, W )
c
c  Jim Boylan, Yeuh-Jin Yang, Ted Russell, and others at Georgia Tech - Aug 99
c     -- derived the aerosol equilibrium sensitivity equations for ISORROPIA
c     -- implemented and tested for URM-1ATM model
c
c  Sergey L. Napelenok - Apr 03
c     -- adapted for the CMAQ ver4.3
c
c  Sergey L. Napelenok - Jul 06
c     -- updated for CMAQ ver4.5
c  
c  Sergey L. Napelenok - May 08
c     -- updated for CMAQ ver4.7
c     -- changed the call structure (now called from AEROPROC)
c     -- now accepts the SBLK array assigned in AERO
c     -- now uses AERO_INFO to get index assignments
c     -- cleared a bunch of unused variables
c
c  Wenxian Zhang - Sep 2013
c     -- restructure for new ddm and hddm calculations
c
c  Sergey L. Napelenok - May 2014
c     --  implementation for CMAQ ver 5.0.2
c
c  Sergey L. Napelenok - Feb 2015
c     --  remove limiting checks for stot and psen
c
c  Variables passed from ISORROPIA:
c
c  W		total concentration vector (mole/m3-air)
c               WI(1) - sodium
c               WI(2) - sulfate
c               WI(3) - ammonium
c               WI(4) - nitrate
c               WI(5) - chloride
c               WI(6) - calcium
c               WI(7) - potassium
c               WI(8) - MG 
c
c               SCASI - ISORROPIA case name
c
c  Variables calculated:
c
c  asen		speciated aerosol sensitivities (total of I & J modes)
c  		asen(1)	aerosol sulfate
c  		asen(2)	aerosol nitrate
c  		asen(3)	aerosol ammonia
c  		asen(4)	aerosol sodium
c  		asen(5)	aerosol chlorine
c  		asen(6)	aerosol hydrogen

      USE AERO_DATA, ONLY : aerospc_conc, aerospc_mw, ASO4_IDX, ANH4_IDX, ANO3_IDX
      USE DDM3D_DEFN, ONLY : NPMAX, IPT, IPARM, HIGH, SEN_PAR, IHIGH
      Use aero6_ddm3d, ONLY : s_precursor_conc, s_aerospc_conc
      USE UTILIO_DEFN

      IMPLICIT NONE


C sln =-=-=-=-=-=-=  DDM-3D sensitivity variables

      CHARACTER( 15 )                   :: SCASI    ! (INPUT) subcase number output
      REAL(KIND=8), DIMENSION(8)        :: W        ! (INPUT) WI concentrations 
  
      REAL, PARAMETER                   :: cmin = 1.0E-30     ! minimum concentration

      INTEGER, PARAMETER                :: nsize = 33             ! full matrix size
      INTEGER, PARAMETER                :: ncomp = 8

      REAL(KIND=8), DIMENSION(nsize)    :: s1    !First-order sensitivity to p1
      REAL(KIND=8), DIMENSION(nsize)    :: s2    !First-order sensitivity to p2
      REAL(KIND=8), DIMENSION(nsize)    :: s1d   !First-order sensitivity to p1 before adjusting minor species
      REAL(KIND=8), DIMENSION(nsize)    :: s2d   !First-order sensitivity to p2 before adjusting minor species
      REAL(KIND=8), DIMENSION(nsize)    :: psen  !Temporary vector for storing solved sensitiities
      REAL(KIND=8), DIMENSION(nsize)    :: psend !Temporary vector for storing solved sensitiities before adjusting minor species
      REAL(KIND=8), ALLOCATABLE, SAVE   :: sens1( :,: ) 
      REAL(KIND=8), ALLOCATABLE, SAVE   :: sens1d( :,: )
  
      INTEGER i, j, k, ip, ip1, ip2
c     integer bflag(ncomp)

      REAL(KIND=8), DIMENSION(ncomp)    :: stot

      REAL mi, mj
      REAL(KIND=8) fji                           ! inorganic Aitken  mode fraction

      INTEGER, SAVE :: LOGDEV
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      CHARACTER( 96 ) :: XMSG = ' '
      INTEGER ALLOCSTAT

C sln =-=-=-=-=-=-=  end DDM-3D sensitivity variables

c-----------------------------------------------------------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         LOGDEV = INIT3 ()
         ALLOCATE( sens1 ( nsize, NPMAX ), 
     &             sens1d( nsize, NPMAX ),
     &             STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating sens1 or sens1d'
            CALL M3EXIT( 'aero_sens', 0 , 0, XMSG, XSTAT2 )
         END IF
      END IF

c
c calculate modal concentration based modal fractions
c
      mi = aerospc_conc( ASO4_IDX,1 ) + aerospc_conc( ANH4_IDX,1 ) + aerospc_conc( ANO3_IDX,1 )
      mj = aerospc_conc( ASO4_IDX,2 ) + aerospc_conc( ANH4_IDX,2 ) + aerospc_conc( ANO3_IDX,2 )

      fji = REAL( ( mi / MAX ( (mi+mj), cmin ) ), 8 )
      fji = MIN( MAX( 0.0D0, fji ), 1.0D0 )

c loop through all sensitivity parameters

      sens1  = 0.0D0
      sens1d = 0.0D0

      do ip = 1,NPMAX

        stot  = 0.0D0
        psen  = 0.0D0
        psend = 0.0D0

        call total_sens(stot,ncomp,ip)

c       do j = 1,ncomp
c          if(abs(stot(j)).gt.W(j)) then
c             if (stot(j).gt.0.d0) stot(j) = 0.10d0*W(j)
c             if (stot(j).lt.0.d0) stot(j) =-0.10d0*W(j)
c          end if
c       end do

        if(IPT(ip).eq.4) then ! calculate second-order sensitivities

           if ( HIGH ) then
              ip1 = IHIGH(ip,1)
              ip2 = IHIGH(ip,2)
              do j = 1,nsize
                 s1(j) = sens1(j,ip1)
                 s2(j) = sens1(j,ip2)
                 s1d(j) = sens1d(j,ip1)
                 s2d(j) = sens1d(j,ip2)
              enddo

              call hddmsens(stot,psen,s1,s2,s1d,s2d,SCASI)

           else
              XMSG = 'DDM-3D HIGH option not enabled - check runscript'
              CALL M3EXIT ( 'aero_sens', 0, 0, XMSG, XSTAT3 )
           endif
  
        else ! calculate first-order sensitivities

           call ddmsens(stot,psen,psend,SCASI)

           if ( HIGH ) then ! store for use in hddm calculations
              do i = 1, nsize
                 sens1d(i,ip) = psend(i)
                 sens1(i,ip) = psen(i)
              enddo
           end if

        endif

c       do j = 1,ncomp
c          bflag(j) = 0
c       enddo

c       if(abs(psen(3)) .gt.1.5D0*W(3)) bflag(3) = 1                          
c       if(abs(psen(5)) .gt.1.5D0*W(2).or.abs(psen(6)).gt.2.0D0*W(2)) bflag(2) = 1 
c       if(abs(psen(7)) .gt.1.5D0*W(4)) bflag(4) = 1
c       if(abs(psen(2)) .gt.1.5D0*W(1)) bflag(1) = 1
c       if(abs(psen(4)) .gt.1.5D0*W(5)) bflag(5) = 1
c       if(abs(psen(8)) .gt.1.5D0*W(6)) bflag(6) = 1
c       if(abs(psen(9)) .gt.1.5D0*W(7)) bflag(7) = 1
c       if(abs(psen(10)).gt.1.5D0*W(8)) bflag(8) = 1

c       call asupdt(psen,fji,nsize,bflag,ip,ncomp)
        call asupdt(psen,fji,nsize,ip,ncomp)

      end do

      return
      end

c-----------------------------------------------------------------------------------------------------------------------
      subroutine total_sens(stot,ncomp,ip)
 
      USE AERO_DATA, ONLY : aerospc_mw, n_aerospc, n_mode,
     &                      ASO4_IDX, ANO3_IDX, ANH4_IDX, ANA_IDX, ACL_IDX, AK_IDX, ACA_IDX, AMG_IDX
      USE PRECURSOR_DATA, ONLY : precursor_mw, HNO3_IDX, NH3_IDX, SULPRD_IDX, HCL_IDX
      USE AERO6_DDM3D, ONLY : s_aerospc_conc, s_precursor_conc
  
      implicit none
  
      integer ncomp
      integer ip
      integer i
  
      real(kind=8), dimension(ncomp)   :: stot
  
      stot = 0.0d0

      stot(1) = ( s_aerospc_conc( ANA_IDX,1,ip )                  ! ANAI
     &          + s_aerospc_conc( ANA_IDX,2,ip ) )                ! ANAJ
     &        * 1.0d-6 / REAL( aerospc_mw(ANA_IDX), 8 )

      stot(2) = ( s_aerospc_conc( ASO4_IDX,1,ip )                 ! ASO4I
     &          + s_aerospc_conc( ASO4_IDX,2,ip ) )               ! ASO4J 
     &        * 1.0d-6 / REAL( aerospc_mw(ASO4_IDX), 8 )
     &        + s_precursor_conc( SULPRD_IDX,ip )                 ! H2SO4
     &        * 1.0d-6 / REAL ( precursor_mw( SULPRD_IDX ), 8 )

      stot(3) = ( s_aerospc_conc( ANH4_IDX,1,ip )                 ! ANH4I
     &          + s_aerospc_conc( ANH4_IDX,2,ip ) )               ! ANH4J
     &        * 1.0d-6 / REAL( aerospc_mw(ANH4_IDX), 8 )
     &        + s_precursor_conc( NH3_IDX,ip )                    ! NH3
     &        * 1.0d-6 / REAL ( precursor_mw( NH3_IDX ), 8 )

      stot(4) = ( s_aerospc_conc( ANO3_IDX,1,ip )                 ! ANO3I
     &          + s_aerospc_conc( ANO3_IDX,2,ip ) )               ! ANO3J
     &        * 1.0d-6 / REAL( aerospc_mw(ANO3_IDX), 8 )
     &        + s_precursor_conc( HNO3_IDX,ip )                   ! HNO3
     &        * 1.0d-6 / REAL ( precursor_mw( HNO3_IDX ), 8 )

      stot(5) = ( s_aerospc_conc( ACL_IDX,1,ip )                  ! ACLI
     &          + s_aerospc_conc( ACL_IDX,2,ip ) )                ! ACLJ
     &        * 1.0d-6 / REAL( aerospc_mw(ACL_IDX), 8 )
     &        + s_precursor_conc( HCL_IDX,ip )                    ! HCL
     &        * 1.0d-6 / REAL ( precursor_mw( HCL_IDX ), 8 )

      stot(6) = ( s_aerospc_conc( ACA_IDX,1,ip )                  ! ACAI
     &          + s_aerospc_conc( ACA_IDX,2,ip ) )                ! ACAJ
     &        * 1.0d-6 / REAL( aerospc_mw(ACA_IDX), 8 )

      stot(7) = ( s_aerospc_conc( AK_IDX,1,ip )                   ! AKI
     &          + s_aerospc_conc( AK_IDX,2,ip ) )                 ! AKJ
     &        * 1.0d-6 / REAL( aerospc_mw(AK_IDX), 8 )

      stot(8) = ( s_aerospc_conc( AMG_IDX,1,ip )                  ! AMGI
     &          + s_aerospc_conc( AMG_IDX,2,ip ) )                ! AMGJ
     &        * 1.0d-6 / REAL( aerospc_mw(ANA_IDX), 8 )

      return
      end

c-----------------------------------------------------------------------------------------------------------------------

c     subroutine asupdt(csens,fji,nsize,bflag,ip,ncomp)
      subroutine asupdt(csens,fji,nsize,ip,ncomp)

      USE AERO6_DDM3D, ONLY : s_precursor_conc, s_aerospc_conc
      USE AERO_DATA, ONLY : aerospc_mw, ASO4_IDX, ANH4_IDX, ANO3_IDX, ANA_IDX, ACL_IDX,
     &                      ACA_IDX, AK_IDX, AMG_IDX
      USE PRECURSOR_DATA, ONLY : precursor_mw, HNO3_IDX, NH3_IDX, SULPRD_IDX, HCL_IDX
      USE UTILIO_DEFN
  
      implicit none
  
      integer ncomp
      integer nsize
c     integer bflag(ncomp)
      integer ip
      integer m

      real(kind=8), dimension(nsize)   :: csens
      real(kind=8), dimension(ncomp)   :: asen
      real(kind=8)                     :: f
      real(kind=8)                     :: fji

c     INTEGER, SAVE :: LOGDEV
c     LOGICAL, SAVE :: FIRSTIME = .TRUE.

c     IF ( FIRSTIME ) THEN
c        FIRSTIME = .FALSE.
c        LOGDEV = INIT3 ()
c     ENDIF


c Filter very small numbers
      do m = 1, nsize
         if ( csens(m) .gt. 0.0d0 .and. csens(m) .lt. 1.0d-21 ) then
            csens(m) = 0.0d0
         endif
         if ( csens(m) .lt. 0.0d0 .and. csens(m) .gt. 1.0d-21 ) then
            csens(m) = 0.0d0
         endif
      end do

      f = 1.0D6

      asen(1) = (csens(5) + csens(6)) * f * dble(aerospc_mw(aso4_idx))
      asen(2) =  csens(3)             * f * dble(aerospc_mw(anh4_idx))
      asen(3) =  csens(7)             * f * dble(aerospc_mw(ano3_idx))
      asen(4) =  csens(2)             * f * dble(aerospc_mw(ana_idx))
      asen(5) =  csens(4)             * f * dble(aerospc_mw(acl_idx))
      asen(6) =  csens(8)             * f * dble(aerospc_mw(aca_idx))
      asen(7) =  csens(9)             * f * dble(aerospc_mw(ak_idx))
      asen(8) =  csens(10)            * f * dble(aerospc_mw(amg_idx))

c SO4

      s_precursor_conc( SULPRD_IDX, ip ) = 0.0D0
c     if ( bflag(2) .eq. 0 ) then
           s_aerospc_conc( ASO4_IDX,1,ip ) = asen(1) * fji
           s_aerospc_conc( ASO4_IDX,2,ip ) = asen(1) - s_aerospc_conc( ASO4_IDX,1,ip )
c     endif

c NH4
c     if ( bflag(3) .eq. 0 ) then
           s_aerospc_conc( ANH4_IDX,1,ip ) = asen(2) * fji
           s_aerospc_conc( ANH4_IDX,2,ip ) = asen(2) - s_aerospc_conc( ANH4_IDX,1,ip )
           s_precursor_conc( NH3_idx,ip )  = csens(12) * f * REAL( precursor_mw(NH3_idx),8 )
c     endif
         
c NO3
c     if ( bflag(4) .eq. 0 ) then
           s_aerospc_conc( ANO3_IDX,1,ip ) = asen(3) * fji
           s_aerospc_conc( ANO3_IDX,2,ip ) = asen(3) - s_aerospc_conc( ANO3_IDX,1,ip )
           s_precursor_conc( HNO3_idx,ip )  = csens(14) * f * REAL( precursor_mw(HNO3_idx),8 )
c     endif

c NA
c     if ( bflag(1) .eq. 0 ) then
           s_aerospc_conc( ANA_IDX,1,ip ) = asen(4) * fji
           s_aerospc_conc( ANA_IDX,2,ip ) = asen(4) - s_aerospc_conc( ANA_IDX,1,ip )
c     endif

c CL
c     if ( bflag(5) .eq. 0 ) then
           s_aerospc_conc( ACL_IDX,1,ip ) = asen(5) * fji
           s_aerospc_conc( ACL_IDX,2,ip ) = asen(5) - s_aerospc_conc( ACL_IDX,1,ip )
           s_precursor_conc( HCL_idx,ip ) = csens(13) * f * REAL( precursor_mw(HCL_idx),8 )
c     endif
 
c CA
c     if ( bflag(6) .eq. 0 ) then
           s_aerospc_conc( ACA_IDX,1,ip ) = asen(6) * fji
           s_aerospc_conc( ACA_IDX,2,ip ) = asen(6) - s_aerospc_conc( ACA_IDX,1,ip )
c     endif

c MG
c     if ( bflag(8) .eq. 0 ) then
           s_aerospc_conc( AMG_IDX,1,ip ) = asen(8) * fji
           s_aerospc_conc( AMG_IDX,2,ip ) = asen(8) - s_aerospc_conc( AMG_IDX,1,ip )
c     endif

c K
c     if ( bflag(7) .eq. 0 ) then
           s_aerospc_conc( AK_IDX,1,ip ) = asen(7) * fji
           s_aerospc_conc( AK_IDX,2,ip ) = asen(7) - s_aerospc_conc( AK_IDX,1,ip )
c     endif

      return
      end
  
  
