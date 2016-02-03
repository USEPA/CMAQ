      subroutine update_k(lamb,xbar,ybar,zpuff)
!******************************************************************************
! Developed jointly by the ARAP Group of Titan Corporation, Princeton, NJ and 
! Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION:  Update the rate constants
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                   ratek              get_latlon               get_topog
!                
!
! REVISION HISTORY: 
!
!    Minor updates (Fortran 90), February 2004 (PKK, AER)
!    May 2007: Add Br chemistry for Hg, PK, AER (updated Aug 2007)
!    Updated for CMAQ-AERO3 option, April 2008 (PKK, AER)
!    Updated for CMAQ 5.0, August 2011 (PKK, ENVIRON)
!******************************************************************************
 
! --- MODULES

      use common_puf
      use common_met
      use multcomp_inc
      use step_p_inc
      use common_date
      use host_chem_inc

      implicit none

! --- ARGUMENTS

      logical lamb               !Flag for if k's are for ambient or not
      real   xbar, ybar, zpuff   !Location to set k's at

! --- LOCALS

      real( 8 ), dimension( NRXNS ) :: rk
      real rj(NPHOTAB)
      real zx, lat, lon, pab
      integer i, jrxn
      real dum, terr
      real factor

      INTEGER IRXN, N, NN
      INTEGER PTAB, PREF

      logical ldark

      REAL H2OCONC

!==== Check that host model reactions match the number read from the input file

      if (t == 0.) then

        if (nreacts /= NRXNS ) then

          nError = IV_ERROR
          eRoutine = 'update_k'
          eMessage = 'SCICHEM reactions do not match '//
     &                'host model reactions'
          write(eAction,*)'No. of SCICHEM reactions =', nreacts
          write(eInform,*)'No. of host model reactions =', NRXNS
          go to 9999
        end if

      end if

!====   Set temperature from potential temperature

      tab = tb*(pb**0.285714)

!====   Add the plume temperature

      if (.not. lamb) then
        if (dynamic) tab = tab + tdyn
      end if

!====   Set water concentration from humidity

      H2OCONC = 1.e+06*hb*(29./18.)/(1. + hb*(29./18.))

!====   Save to common for error messages
      tk = tab
      hk = hb
      pk = pb
      cldk = wcbar

!====  Adjust quadratic reactions for actual puff temp & pressure

      if (ic_units == UNIT_PPM) then
        factor = tk/(298.*pk)
      else
        factor = 1.
      end if

!====   Set photolytic rates

      call get_latlon(xbar,ybar,lat,lon)

      zx = zpuff + hmin !get mean sea level

      call get_topog(xbar,ybar,terr,dum,dum)

      CALL SCIPHOT (MDATE, MTIME, LAT, LON, ZX, TERR,
     &              WCBAR, CLDTOP, CLDBOT, FCC, RJ)

      IF (.NOT. LDARK()) THEN
         DO N = 1, NMPHOT
            IRXN = IPH(N,1)
            PTAB = IPH(N,2)
            IF (IPH(N,3) /= 0) THEN
               RK(IRXN) = RTDAT(1,IRXN) * RJ(PTAB)
            ELSE
               RK(IRXN) = RTDAT(1,IRXN) * RK(PTAB)
            END IF
         END DO
      ELSE  ! in darkness
         DO N = 1, NMPHOT
            IRXN = IPH(N,1)
            RK(IRXN) = 0.0
         END DO
      END IF

!====   Set temperature and pressure dependent reaction rate coefficients

      pab = pb

      call ratek(tab,H2OCONC,pab,rk)

      NRXN_CURR = 0
      DO I = 1, NRXNS

!...... Convert from ppm-min units to ppm-sec units for SCICHEM

        REACTION(I)%K = RK(REACTION(I)%ID)/60.

        if (.not. IsLinear(i) .and. .not. lamb) then
          reaction(i)%k = reaction(i)%k*factor !adjust for actual temp & press
        end if

        kamb(i) = reaction(i)%k

        IF (REACTION(I)%K > 0.) THEN
          NRXN_CURR = NRXN_CURR + 1
          INDX_RXNS(NRXN_CURR) = I
        END IF

      END DO

9999  RETURN
      END

      subroutine get_latlon( x , y , lat , lon )
!******************************************************************************
!
! FUNCTION:  Get the lat and lon for a given x and y
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES

      use common_puf
      use common_met

      implicit none

! --- ARGUMENTS
 
      real x, y      !x and y location
      real lat, lon  !latitude and longitude

! --- LOCALS

      integer i, j, ij

      i = nint((x-xmin)/dxb) + 1
      j = nint((y-ymin)/dyb) + 1

      i = max(1,i)
      i = min(i,nxb)
      j = max(1,j)
      j = min(j,nyb)

      ij = (j-1)*nxb + i

      lat = lat2d(ij)
      lon = lon2d(ij)

      return
      end
