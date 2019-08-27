
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

c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      module cgrid_spcs

c CGRID gas chem, aerosol, non-reactive, and tracer species definitions
c based on Fortran include file specifications
c Revision History: Jeff Young 31 Aug 09: created
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module
c-----------------------------------------------------------------------

      implicit none

      include SUBST_GC_SPC      ! gas chemistry species table
      include SUBST_GC_EMIS
      include SUBST_GC_ICBC
      include SUBST_GC_TRNS
      include SUBST_GC_DDEP
      include SUBST_GC_DEPV
      include SUBST_GC_CONC
      include SUBST_GC_G2AE
      include SUBST_GC_G2AQ
      include SUBST_GC_SCAV
      include SUBST_GC_WDEP

      include SUBST_AE_SPC      ! aerosol species table
      include SUBST_AE_EMIS
      include SUBST_AE_ICBC
      include SUBST_AE_TRNS
      include SUBST_AE_DDEP
      include SUBST_AE_DEPV
      include SUBST_AE_CONC
      include SUBST_AE_A2AQ
      include SUBST_AE_SCAV
      include SUBST_AE_WDEP

      include SUBST_NR_SPC      ! non-reactive species table
      include SUBST_NR_EMIS
      include SUBST_NR_ICBC
      include SUBST_NR_TRNS
      include SUBST_NR_DDEP
      include SUBST_NR_DEPV
      include SUBST_NR_N2AE
      include SUBST_NR_CONC
      include SUBST_NR_N2AQ
      include SUBST_NR_SCAV
      include SUBST_NR_WDEP

      include SUBST_TR_SPC      ! tracer species table
      include SUBST_TR_EMIS
      include SUBST_TR_ICBC
      include SUBST_TR_DIFF
      include SUBST_TR_ADV
      include SUBST_TR_DDEP
      include SUBST_TR_DEPV
      include SUBST_TR_T2AQ
      include SUBST_TR_SCAV
      include SUBST_TR_WDEP

      integer, save :: nspcsd   ! Number of species in CGRID
      integer, save :: gc_strt  ! Starting index of gas chemistry species
      integer, save :: gc_fini  ! Ending index of gas chemistry species
      integer, save :: ae_strt  ! Starting index of aerosol species
      integer, save :: ae_fini  ! Ending index of aerosol species
      integer, save :: nr_strt  ! Starting index of non-reactive species
      integer, save :: nr_fini  ! Ending index of non-reactive species
      integer, save :: tr_strt  ! Starting index of tracer species
      integer, save :: tr_fini  ! Ending index of tracer species

      contains

c init routine to set the CGRID species locations

         function cgrid_spcs_init() result ( success )
            use UTILIO_DEFN
            use RXNS_DATA           ! chemical mechanism data
            
            implicit none


            logical success
            logical, save :: firstime = .true.

c----------------------------------------------------------------------------------------
            success = .true.

c intialize module public data

            if ( firstime ) then
               firstime = .false.

               nspcsd = n_gc_spcd + n_ae_spc + n_nr_spc + n_tr_spc

               gc_strt = 1 ! always, even if n_gc_spcs = 0
               gc_fini = gc_strt + n_gc_spc - 1
               ae_strt = n_gc_spcd + 1
               ae_fini = ae_strt + n_ae_spc - 1
               nr_strt = n_gc_spcd + n_ae_spc + 1
               nr_fini = nr_strt + n_nr_spc - 1
               tr_strt = n_gc_spcd + n_ae_spc + n_nr_spc + 1
               tr_fini = tr_strt + n_tr_spc - 1

            end if

         end function cgrid_spcs_init

      end module cgrid_spcs
