      subroutine sum_depos(ddepos2d_cur,wdepos2d_cur,dens,nvarsdd,dname,
     &                     ddep,nvarswd,wname,wdep)

      use common_met
      use files_inc
      use multcomp_inc

      implicit none

      REAL, PARAMETER :: MWAIR = 28.9644  ! mean molecular weight for dry air [ g/mol ]

      real, dimension(:,:) :: ddepos2d_cur, wdepos2d_cur
      REAL, DIMENSION(:,:,:) :: ddep, wdep
      REAL, DIMENSION(:,:,:) :: dens    ! air density (kg/m3)
      INTEGER         NVARSDD, NVARSWD
      CHARACTER( 16 ), DIMENSION( MAX_MC ) :: DNAME, WNAME
      CHARACTER( 16 ) :: SPNAME

      REAL, DIMENSION(:,:,:), allocatable, SAVE :: ddepsci, wdepsci

      real rho
      INTEGER       VAR, SPC             ! species loop counters
      INTEGER ROW                   ! Row index
      INTEGER COL                   ! Column index
      real ppmfactor
      integer ij,i,j,jj
      integer ios

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      REAL, SAVE :: AREA
! cross-reference arrays between SCICHEM and host model species
      INTEGER, DIMENSION( MAX_MC ), SAVE :: DD2S, WD2S

      INTEGER NCOUNTD, NEGCOUNTD, NCOUNTW, NEGCOUNTW

      logical IsMCParticle

      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.

         allocate(ddepsci(MAXXB,MAXYB,MAX_MC), stat = ios)
         if (ios /= 0) then
            write(*,*) 'Could not allocate DDEPSCI array '
            stop
         end if

         allocate(wdepsci(MAXXB,MAXYB,MAX_MC), stat = ios)
         if (ios /= 0) then
            write(*,*) 'Could not allocate WDEPSCI array '
            stop
         end if

         area = dxb*dyb

! --- x-ref array for dry, wet and total deposition species
         do spc = 1, nspecies
            dd2s(spc) = 0
            wd2s(spc) = 0
         end do

         do var = 1, nvarsdd
            spname = dname( var )
            do spc = 1, nspecies
               if ( TRIM(species(spc)%name) == TRIM(spname) ) then
                  dd2s(spc) = var
                  exit
               end if
            end do
         end do

         do var = 1, nvarswd
            spname = wname( var )
            do spc = 1, nspecies
               if ( TRIM(species(spc)%name) == TRIM(spname) ) then
                  wd2s(spc) = var
                  exit
               end if
            end do
         end do
!pk
         do spc = 1,nspecies
            write(*,*)'species,dd2s,wd2s: ',
     &                 species(spc)%name,dd2s(spc),wd2s(spc)
         end do
         write(*,*)'area: ',area
         call flush(6)
!pk
      END IF

      NCOUNTD = 0
      NEGCOUNTD = 0
      NCOUNTW = 0
      NEGCOUNTW = 0

      do ROW = 1, nyb
         JJ = (ROW - 1)*NXB
         do COL = 1, nxb
            IJ = JJ + COL
            tb = t_ua(ij)
            pb = p_ua(ij)
            tab = tb*(pb**0.285714)
            ppmfactor = tab/(298.*pb)  !scale STP conc to cgrid ppm
            rho = DENS(COL,ROW,1)
!pktemp
!        write(*,*)'grid cell: ',col,',',row
!        write(*,*)'ppmfactor: ',ppmfactor
!        write(*,*)'density: ',rho
!        call flush(6)
!pktemp
!Do dry dep calculations
!pktemp
!        write(*,*)'Dry dep merging'
!        call flush(6)
!pktemp
            do spc = 1, nspecies
!pktemp
!               write(*,*)'Doing species ',spc
!               call flush(6)
!pktemp
               VAR = DD2S(spc)
               if(var == 0) cycle
       
               ddepsci(COL,ROW,VAR) = ddepos2d_cur(ij,SPC)
!pktemp
!               write(*,*)'species: ',var,',',species(SPC)%name
!               write(*,*)'mol. wt: ',sparam(SPC)%mwt
!               if (ddepsci(col,row,var) /= 0.) then
!                  write(*,*)'depsci: ',ddepsci(col,row,var)
!               end if
!               write(*,*)'IsMCParticle: ',IsMCParticle(spc)
!               write(*,*)'col,row,var: ',col,row,var
!               call flush(6)
!pktemp
! --- convert from ppm-m3 to kg/ha (gases) and ug to kg/ha (particles)
               if (IsMCParticle(spc)) then
                  ddepsci(COL,ROW,VAR) = ddepsci(COL,ROW,VAR)*1.E-5/area
               else
                  ddepsci(COL,ROW,VAR) = ddepsci(COL,ROW,VAR)*1.E-2*ppmfactor*
     &                        rho*sparam(SPC)%mwt/MWAIR/area
               end if
!pktemp
!               write(*,*)'finished unit conversion'
!               if (ddepsci(col,row,var) /= 0.) then
!                  write(*,*)'ddepm3: ',ddep(col,row,var)
!               end if
!               call flush(6)
!pktemp
! --- add to host model dry dep
               ddep(COL,ROW,VAR) = ddep(COL,ROW,VAR) +
     &                                ddepsci(COL,ROW,VAR)
               if (ddepsci(col,row,var) /= 0.) then
                 NCOUNTD = NCOUNTD + 1
                 if ( ddep(COL,ROW,VAR) < 0. ) then
                    NEGCOUNTD = NEGCOUNTD + 1
                 end if
               end if
               ddep(COL,ROW,VAR) = MAX( 0., ddep(COL,ROW,VAR) )
!pktemp
!               write(*,*)'Finished species ',spc
!               call flush(6)
!pktemp
            end do 
!Do wet dep calculations
!pktemp
!        write(*,*)'Wet dep merging'
!        call flush(6)
!pktemp
            do spc = 1, nspecies
!pktemp
!               write(*,*)'Doing species ',spc
!               call flush(6)
!pktemp
               VAR = WD2S(spc)
               if(var == 0) cycle
       
               wdepsci(COL,ROW,VAR) = wdepos2d_cur(ij,SPC)
!pktemp
!               write(*,*)'species: ',var,',',species(SPC)%name
!               write(*,*)'mol. wt: ',sparam(SPC)%mwt
!               if (wdepsci(col,row,var) /= 0.) then
!                  write(*,*)'depsci: ',wdepsci(col,row,var)
!               end if
!               write(*,*)'IsMCParticle: ',IsMCParticle(spc)
!               write(*,*)'col,row,var: ',col,row,var
!               call flush(6)
!pktemp
! --- convert from ppm-m3 to kg/ha (gases) and ug to kg/ha (particles)
               if (IsMCParticle(spc)) then
                  wdepsci(COL,ROW,VAR) = wdepsci(COL,ROW,VAR)*1.E-5/area
               else
                  wdepsci(COL,ROW,VAR) = wdepsci(COL,ROW,VAR)*1.E-2*ppmfactor*
     &                        rho*sparam(SPC)%mwt/MWAIR/area
               end if
!pktemp
!               write(*,*)'finished unit conversion'
!               if (wdepsci(col,row,var) /= 0.) then
!                  write(*,*)'wdepm3: ',wdep(col,row,var)
!               end if
!               call flush(6)
!pktemp
! --- add to host model wet dep
               wdep(COL,ROW,VAR) = wdep(COL,ROW,VAR) +
     &                                wdepsci(COL,ROW,VAR)
               if (wdepsci(col,row,var) /= 0.) then
                 NCOUNTW = NCOUNTW + 1
                 if ( wdep(COL,ROW,VAR) < 0. ) then
                    NEGCOUNTW = NEGCOUNTW + 1
                 end if
               end if
               wdep(COL,ROW,VAR) = MAX( 0., wdep(COL,ROW,VAR) )
!pktemp
!               write(*,*)'Finished species ',spc
!               call flush(6)
!pktemp
            end do
        end do
      end do

      write(*,*)'NCOUNTD,NEGCOUNTD: ',NCOUNTD,NEGCOUNTD
      write(*,*)'NCOUNTW,NEGCOUNTW: ',NCOUNTW,NEGCOUNTW

      return

      end
