!------------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in           !
!  continuous development by various groups and is based on information        !
!  from these groups: Federal Government employees, contractors working        !
!  within a United States Government contract, and non-Federal sources         !
!  including research institutions.  These groups give the Government          !
!  permission to use, prepare derivative works of, and distribute copies       !
!  of their work in the CMAQ system to the public and to permit others         !
!  to do so.  The United States Environmental Protection Agency                !
!  therefore grants similar permission to use the CMAQ system software,        !
!  but users are requested to provide copies of derivative works or            !
!  products designed to operate in the CMAQ system to the United States        !
!  Government without restrictions as to use by others.  Software              !
!  that is used with the CMAQ system but distributed under the GNU             !
!  General Public License or the GNU Lesser General Public License is          !
!  subject to their copyright restrictions.                                    !
!------------------------------------------------------------------------------!

SUBROUTINE getluse

!-------------------------------------------------------------------------------
! Name:     Get Land Use
! Purpose:  Reads in land use fractions and reclassifies categories
!           following RADM as needed for use with PBLPKG and RADMDRY.
! Revised:  15 Jan 1997  Created for MCIP and generalized CTM.  (D. Byun)
!           20 May 1997  Adapted for Models-3 BETA system.  (D. Byun)
!           04 Feb 1998  Changed include for nonglobal includes.  (D. Byun)
!           14 Apr 2000  Added more LU data options.  (D. Byun)
!           10 Sep 2001  Converted to free-form f90.  Removed retrieval of BMAX
!                        from environment and added it to MCIPPARM.  Converted
!                        arrays to allocatable based on run-time definitions.
!                        Removed re-calculations of NCG_I and NCG_J.  Changed
!                        MAXI and MAXJ to METROW and METCOL.  Removed argument
!                        list.  Changed 11-category water from "7" to LWATER.
!                        Added calculations for LUTYPE=3.  Moved calculations
!                        for each definition of LUTYPE to separate subroutines.
!                        (T. Otte)
!           09 Jan 2002  Changed calls to "abort" to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  (T. Otte)
!           26 Mar 2003  Simplified algorithm to map input meteorology to
!                        MCIP_X domain.  (T. Otte)
!           11 Aug 2004  Removed obsolete land-use input sources so that all
!                        land-use input is assumed to come directly from MM5
!                        to be converted to RADM categories.  Added algorithms
!                        from lutrans3.F here.  Removed XFLAGS.  Removed
!                        conversion of land use to RADM categories if RADM dry
!                        deposition and/or PBL recalculation is not used.
!                        (T. Otte)
!           01 Dec 2004  Added processing for fractional land use categories
!                        if those fields are available.  (T. Otte)
!           08 Apr 2005  Removed NDX and option to interpolate to finer scale
!                        meteorology.  Changed I and J dimensions to Y and X
!                        to make the code more general.  Added optimization
!                        of loops using F90 implicit loop structures.  (T. Otte)
!           20 Jul 2005  Recoded nested WHERE-ELSEWHERE-END WHERE for known bug
!                        in PGF90v5.2 compiler.  (T. Otte)
!           14 Jul 2006  Corrected comparison of REAL field in XDLUSE with
!                        INTEGER scalar LWATER.  Removed unused variables
!                        IIL and JJL.  Define land-water mask array.  (T. Otte)
!           04 May 2007  Removed option to convert to RADM land use categories.
!                        Defined character string with land use classification
!                        source.  Changed criteria to file XLUSRC.  (T. Otte)
!           05 May 2008  Expanded possible land use configurations to include
!                        33-category USGS classification in WRFv2.2+.  Changed
!                        definition of PURB to reflect urban land use that can
!                        be in categories 1, 31, 32, and 33 for USGS 33-category
!                        land use.  Changed to preferentially fill PURB from
!                        FRC_URB when the urban canopy model is used in WRF.
!                        (T. Otte)
!           27 Oct 2009  Added NLCD/MODIS and Modified IGBP MODIS-NOAH as
!                        land-use classification systems for WRF.  Changed
!                        MET_UCMCALL to MET_URBAN_PHYS, and allowed for variable
!                        to be set to be greater than 1.  (T. Otte)
!           12 Feb 2010  Removed unused variables IDOM and J.  (T. Otte)
!           15 Dec 2010  Moved LWATER from module MCIPPARM to be a local
!                        variable.  Added LICE, and use it to refine land/water
!                        mask to include ice cells as water rather than land.
!                        (T. Otte)
!           29 Aug 2011  Improved error handling.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           10 Sep 2012  Added handling for 40-category 2006 NLCD-MODIS land
!                        use classification as "NLCD40".  Added alternate name
!                        for 50-category 2001 NLCD-MODIS land use classification
!                        as "NLCD50".  Changed LUCATNLCD to LUCATNLCD50 and
!                        added new field LUCATNLCD40.  Changed XLUSRC for the
!                        50-category classification from "NLCD-MODIS" to
!                        "NLCD50".  Added XLUSRC of "NLCD40" for the 40-category
!                        classification.  Corrected error in mapping of water
!                        from NLCD-MODIS (NLCD50) into a single category where
!                        category 47 (barren or sparsely vegetated) was used
!                        instead of category 48 (IGBP water).  In the computed
!                        land mask, returned snow/ice to land.  Now use land
!                        mask directly from WRF if Pleim-Xiu LSM was used in
!                        WRF.  (T. Otte)
!           26 Nov 2014  Added code to handle separate lake category option in
!                        USGS and MODIS land use classifications from WRF by
!                        converting lakes to generic water (because CMAQ does
!                        not distinguish handling lakes of yet).  Also removed
!                        requirement to have FRC_URB available when the urban
!                        canopy model is used in WRF.  (T. Spero and C. Nolte)
!           30 Oct 2015  Corrected logic on filling PURB to account for case
!                        where FRC_URB has not been allocated.  (T. Spero)
!           09 Feb 2018  Added capability to properly process PURB when the
!                        21-category MODIS land use is used in WRF.  Added
!                        processing of fractional land use for NOAH Mosaic.
!                        (T. Spero)
!           22 Jun 2018  Changed name of module LUVARS to LUCATS to minimize
!                        confusion.  (T. Spero)
!           08 Aug 2018  Corrected bug in setting land use category names in
!                        MCIP for USGS24 + lakes.  (T. Spero)
!-------------------------------------------------------------------------------

  USE lucats
  USE metvars
  USE metinfo, nx => met_nx, ny => met_ny
  USE xvars
  USE mcipparm

  IMPLICIT NONE

  INTEGER                           :: col
  INTEGER                           :: ec
  INTEGER                           :: er
  INTEGER                           :: i
  INTEGER                           :: ii
  INTEGER                           :: jj
  INTEGER                           :: lu
  INTEGER                           :: lumax
  CHARACTER(LEN=16),  PARAMETER     :: pname     = 'GETLUSE'
  INTEGER                           :: row
  INTEGER                           :: sc
  INTEGER                           :: sr

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   TOO MANY INPUT LAND USE CATEGORIES', &
    & /, 1x, '***   MAXIMUM ALLOWED (MAXLUC) = ', i4, &
    & /, 1x, '***   ATTEMPTED SIZE = ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9100 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   UNKNOWN LAND USE SOURCE AND MAX CATEGORIES', &
    & /, 1x, '***   LAND USE SOURCE = ', a, &
    & /, 1x, '***   NUMBER OF CATEGORIES = ', i4, &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Set up land-use classification-specific information.
!-------------------------------------------------------------------------------

  IF ( nummetlu > SIZE(xluse,3) ) THEN
    WRITE (*,f9000) TRIM(pname), SIZE(xluse,3), nummetlu
    CALL graceful_stop (pname)
  ENDIF

  lumax  = nummetlu

  IF ( ( met_lu_src(1:3) == "USG" ) .AND. ( nummetlu == 24 ) ) THEN
    xlusrc = "USGS24"  ! accounts for lake category 28
    DO i = 1, nummetlu
      xludesc(i) = TRIM(xlusrc) // ': ' // TRIM(lucatusgs24(i))
    ENDDO
  ELSE IF ( ( met_lu_src(1:3) == "USG" ) .AND. ( nummetlu == 28 ) ) THEN
    xlusrc = "USGS28"
    DO i = 1, nummetlu
      xludesc(i) = TRIM(xlusrc) // ': ' // TRIM(lucatusgs28(i))
    ENDDO
  ELSE IF ( ( met_lu_src(1:3) == "USG" ) .AND. ( nummetlu == 33 ) ) THEN
    xlusrc = "USGS33"
    DO i = 1, nummetlu
      xludesc(i) = TRIM(xlusrc) // ': ' // TRIM(lucatusgs33(i))
    ENDDO
  ELSE IF ( ( met_lu_src(1:3) == "OLD" ) .AND. ( nummetlu == 13 ) ) THEN
    xlusrc = "MM513"
    DO i = 1, nummetlu
      xludesc(i) = TRIM(xlusrc) // ': ' // TRIM(lucatold(i))
    ENDDO
  ELSE IF ( ( met_lu_src(1:3) == "MOD" ) .AND. ( nummetlu == 20 ) .OR.  &
            ( met_lu_src(1:3) == "MOD" ) .AND. ( nummetlu == 21 ) .OR.  &
            ( met_lu_src(1:3) == "MOD" ) .AND. ( nummetlu == 33 ) ) THEN
    xlusrc = "MODIS NOAH"  ! accounts for lake category 21
    DO i = 1, nummetlu
      xludesc(i) = TRIM(xlusrc) // ': ' // TRIM(lucatmod(i))
    ENDDO
  ELSE IF ( ( met_lu_src(1:3) == "NLC" ) .AND. ( nummetlu == 50 ) ) THEN
    xlusrc = "NLCD50"
    DO i = 1, nummetlu
      xludesc(i) = TRIM(xlusrc) // ': ' // TRIM(lucatnlcd50(i))
    ENDDO
  ELSE IF ( ( met_lu_src(1:3) == "NLC" ) .AND. ( nummetlu == 40 ) ) THEN
    xlusrc = "NLCD40"
    DO i = 1, nummetlu
      xludesc(i) = TRIM(xlusrc) // ': ' // TRIM(lucatnlcd40(i))
    ENDDO
  ELSE IF ( ( ( met_lu_src(1:3) == "SiB" ) .OR. ( met_lu_src(1:3) == "SIB" ) ) &
            .AND. ( nummetlu == 16 ) ) THEN
    xlusrc = "SIB"
    DO i = 1, nummetlu
      xludesc(i) = TRIM(xlusrc) // ': ' // TRIM(lucatsib(i))
    ENDDO
  ELSE
    WRITE (*,f9100) TRIM(pname), met_lu_src, nummetlu
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! Fill input land use from meteorological model into output arrays.
!-------------------------------------------------------------------------------

  sc = x0
  ec = x0 + ncols_x - 1
  sr = y0
  er = y0 + nrows_x - 1

  IF ( iflufrc ) THEN

    xluse (:,:,1:lumax) = lufrac (sc:ec,sr:er,:)
    xdluse(:,:)         = landuse(sc:ec,sr:er)

    IF ( iflu2wrfout ) THEN

      xlufrac2   (:,:,:) = lufrac2  (sc:ec,sr:er,1:nummosaic)
      xmoscatidx (:,:,:) = moscatidx(sc:ec,sr:er,1:nummosaic)

    ENDIF

  ELSE

    DO col = 1, ncols_x
      ii = x0 + col - 1
      DO row = 1, nrows_x
        jj = y0 + row - 1

        lu = landuse(ii,jj)

        xluse (col,row,:)  = 0.0
        xluse (col,row,lu) = 1.0
        xdluse(col,row)    = lu

      ENDDO
    ENDDO

    ! Adjust NLCD-MODIS (50-category and 40-category versions) 
    ! for duplicate water and ice categories.

    IF ( TRIM(xlusrc) == "NLCD50" ) THEN  ! NLCD50

      ! Adjust NLCD50 (formerly NLCD-MODIS) to consolidate all water
      ! into single category.

      xluse(:,:, 1) = xluse(:,:,1) + xluse(:,:,31) + xluse(:,:,48)
      xluse(:,:,31) = 0.0
      xluse(:,:,48) = 0.0

      ! Adjust NLCD50 to consolidate all ice into single category.

      xluse(:,:, 2) = xluse(:,:,2) + xluse(:,:,46)
      xluse(:,:,46) = 0.0

      DO col = 1, ncols_x
        DO row = 1, nrows_x

          ! Convert "water" to "open water".

          IF ( xdluse(col,row) == 31 ) THEN
            xdluse(col,row) = 1
          ENDIF

          ! Convert "IGBP water" to "open water".

          IF ( xdluse(col,row) == 48 ) THEN
            xdluse(col,row) = 1
          ENDIF

          ! Convert "Permanent Snow and Ice" to "Perennial Ice-Snow"

          IF ( xdluse(col,row) == 46 ) THEN
            xdluse(col,row) = 2
          ENDIF

          ! If there was overlap in water categories, ensure dominant category
          ! is water if the sum is greater than 50% of the cell.

          IF ( xluse(col,row,1) > 0.50 ) THEN
            xdluse(col,row) = 1
          ENDIF

          ! If there was overlap in ice categories, ensure dominant category
          ! is ice if the sum is greater than 50% of the cell.

          IF ( xluse(col,row,2) > 0.50 ) THEN
            xdluse(col,row) = 2
          ENDIF

        ENDDO
      ENDDO

    ELSE IF ( TRIM(xlusrc) == "NLCD40" ) THEN  ! NLCD40

      ! Adjust NLCD40 to consolidate all water into single category.

      xluse(:,:,17) = xluse(:,:,17) + xluse(:,:,21)
      xluse(:,:,21) = 0.0

      ! Adjust NLCD50 to consolidate all ice into single category.

      xluse(:,:,15) = xluse(:,:,15) + xluse(:,:,22)
      xluse(:,:,22) = 0.0

      DO col = 1, ncols_x
        DO row = 1, nrows_x

          ! Convert "open water" to "IGBP water".

          IF ( xdluse(col,row) == 21 ) THEN
            xdluse(col,row) = 17
          ENDIF

          ! Convert "Perennial Ice-Snow" to "Snow and Ice"

          IF ( xdluse(col,row) == 22 ) THEN
            xdluse(col,row) = 15
          ENDIF

          ! If there was overlap in water categories, ensure dominant category
          ! is water if the sum is greater than 50% of the cell.

          IF ( xluse(col,row,17) > 0.50 ) THEN
            xdluse(col,row) = 17
          ENDIF

          ! If there was overlap in ice categories, ensure dominant category
          ! is ice if the sum is greater than 50% of the cell.

          IF ( xluse(col,row,15) > 0.50 ) THEN
            xdluse(col,row) = 15
          ENDIF

        ENDDO
      ENDDO

    ENDIF  ! NLCD-MODIS

  ENDIF  ! fractional land use

!-------------------------------------------------------------------------------
! If lakes are defined in WRF land-use classification, convert lakes to
! generic water category here.
!-------------------------------------------------------------------------------

  IF ( met_lu_lake > 0 ) THEN  ! lakes in a separate land use index
    WHERE ( NINT(xdluse) == met_lu_lake )
      xdluse = met_lu_water
    ENDWHERE
    xluse(:,:,met_lu_water) = xluse(:,:,met_lu_water) + xluse(:,:,met_lu_lake)
  ENDIF

!-------------------------------------------------------------------------------
! Define land-water mask.
!-------------------------------------------------------------------------------

  IF ( ( met_soil_lsm == 7 ) .AND. ( met_model == 2 ) ) THEN  ! P-X and WRF

    xlwmask(:,:) = landmask(sc:ec,sr:er)

  ELSE

    ! LANDMASK is a dynamic, time-variant field in WRF for LSMs other than P-X,
    ! so reconstruct it as a static field using the dominant land-use category.

    WHERE ( NINT(xdluse) == met_lu_water )  ! water
      xlwmask = 0.0
    ELSEWHERE  ! land
      xlwmask = 1.0
    END WHERE

  ENDIF

!-------------------------------------------------------------------------------
! Fill percentage of urban area (PURB) based on amount of land in grid cell.
! When urban canopy model is used in WRF, use fraction of urban area in
! cell (FRC_URB) to fill PURB, if PRC_URB is available.
!-------------------------------------------------------------------------------

  IF ( met_urban_phys >= 1 ) THEN

    sc = x0
    ec = x0 + ncols_x - 1
    sr = y0
    er = y0 + nrows_x - 1

    xpurb(:,:) = frc_urb(sc:ec,sr:er) * 100.0  ! [fraction -> percent]

  ELSE IF ( iflufrc ) THEN  ! fractional land use available

    DO row = 1, nrows_x
      DO col = 1, ncols_x
 
        IF ( NINT(xdluse(col,row)) == met_lu_water ) THEN  ! water is dominant
          xpurb(col,row) = 0.0
        ELSE  ! land is dominant over water in cell
          IF ( xluse(col,row,met_lu_water) < 1.0 ) THEN
            IF ( TRIM(xlusrc) == 'USGS33' ) THEN
              xpurb(col,row) = ( ( xluse(col,row,1)  + xluse(col,row,31) +    &
                                   xluse(col,row,32) + xluse(col,row,33) ) /  &
                                 (1.0 - xluse(col,row,met_lu_water)) ) * 100.0
            ELSE IF ( TRIM(xlusrc) == 'MODIS NOAH' ) THEN
              IF ( nummetlu == 33 ) THEN
                xpurb(col,row) = ( ( xluse(col,row,13) + xluse(col,row,31) +    &
                                     xluse(col,row,32) + xluse(col,row,33) ) /  &
                                   (1.0 - xluse(col,row,met_lu_water)) ) * 100.0
              ELSE IF ( ( nummetlu == 20 ) .OR. ( nummetlu == 21 ) ) THEN
                xpurb(col,row) = ( xluse(col,row,13) /  &
                                   (1.0 - xluse(col,row,met_lu_water)) ) * 100.0
              ENDIF
            ELSE IF ( TRIM(xlusrc) == 'NLCD50' ) THEN
              xpurb(col,row) = ( ( xluse(col,row,3) * 0.10 +    &
                                   xluse(col,row,4) * 0.35 +    &
                                   xluse(col,row,5) * 0.65 +    &
                                   xluse(col,row,6) * 0.90 +    &
                                   xluse(col,row,44)       ) /  &
                                 (1.0 - xluse(col,row,met_lu_water)) ) * 100.0
            ELSE IF ( TRIM(xlusrc) == 'NLCD40' ) THEN
              xpurb(col,row) = ( ( xluse(col,row,23) * 0.10 +    &
                                   xluse(col,row,24) * 0.35 +    &
                                   xluse(col,row,25) * 0.65 +    &
                                   xluse(col,row,26) * 0.90 +    &
                                   xluse(col,row,13)        ) /  &
                                 (1.0 - xluse(col,row,met_lu_water)) ) * 100.0
            ELSE IF ( TRIM(xlusrc) == 'SIB' ) THEN
              xpurb(col,row) = 0.0  ! urban is not specified in SiB
            ELSE  ! "OLD" from MM5
              xpurb(col,row) = ( xluse(col,row,1) /  &
                                 (1.0 - xluse(col,row,met_lu_water)) ) * 100.0
            ENDIF
          ELSE
            xpurb(col,row) = 0.0
          ENDIF
        ENDIF

      ENDDO
    ENDDO

  ENDIF

END SUBROUTINE getluse
