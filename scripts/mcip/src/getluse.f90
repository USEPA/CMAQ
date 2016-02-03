
!***********************************************************************
!   Portions of Models-3/CMAQ software were developed or based on      *
!   information from various groups: Federal Government employees,     *
!   contractors working on a United States Government contract, and    *
!   non-Federal sources (including research institutions).  These      *
!   research institutions have given the Government permission to      *
!   use, prepare derivative works, and distribute copies of their      *
!   work in Models-3/CMAQ to the public and to permit others to do     *
!   so.  EPA therefore grants similar permissions for use of the       *
!   Models-3/CMAQ software, but users are requested to provide copies  *
!   of derivative works to the Government without restrictions as to   *
!   use by others.  Users are responsible for acquiring their own      *
!   copies of commercial software associated with Models-3/CMAQ and    *
!   for complying with vendor requirements.  Software copyrights by    *
!   the MCNC Environmental Modeling Center are used with their         *
!   permissions subject to the above restrictions.                     *
!***********************************************************************

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /project/work/rep/MCIP2/src/mcip2/getluse.F,v 1.5 2007/08/03 20:50:05 tlotte Exp $ 


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
!-------------------------------------------------------------------------------

  USE luvars
  USE metvars
  USE metinfo, nx => met_nx, ny => met_ny
  USE xvars
  USE mcipparm

  IMPLICIT NONE

  INTEGER                      :: col
  INTEGER                      :: ec
  INTEGER                      :: er
  INTEGER                      :: i
  INTEGER                      :: ii
  INTEGER                      :: jj
  INTEGER                      :: lu
  INTEGER                      :: lumax
  CHARACTER*16,  PARAMETER     :: pname     = 'GETLUSE'
  INTEGER                      :: row
  INTEGER                      :: sc
  INTEGER                      :: sr

!-------------------------------------------------------------------------------
! Set up land-use classification-specific information.
!-------------------------------------------------------------------------------

  IF ( nummetlu > SIZE(xluse,3) ) THEN
    WRITE (6,9000) SIZE(xluse,3), nummetlu
    GOTO 1001
  ENDIF

  lumax  = nummetlu

  IF ( ( met_lu_src(1:3) == "USG" ) .AND. ( nummetlu == 24 ) ) THEN
    xlusrc = "USGS24"
    lwater = 16
    DO i = 1, nummetlu
      xludesc(i) = TRIM(xlusrc) // ': ' // TRIM(lucatusgs24(i))
    ENDDO
  ELSE IF ( ( met_lu_src(1:3) == "USG" ) .AND. ( nummetlu == 33 ) ) THEN
    xlusrc = "USGS33"
    lwater = 16
    DO i = 1, nummetlu
      xludesc(i) = TRIM(xlusrc) // ': ' // TRIM(lucatusgs33(i))
    ENDDO
  ELSE IF ( ( met_lu_src(1:3) == "OLD" ) .AND. ( nummetlu == 13 ) ) THEN
    xlusrc = "MM513"
    lwater = MAX (met_lu_water, 7)  ! accounts for -999 prior to MM5v2.12
    DO i = 1, nummetlu
      xludesc(i) = TRIM(xlusrc) // ': ' // TRIM(lucatold(i))
    ENDDO
  ELSE IF ( ( met_lu_src(1:3) == "MOD" ) .AND. ( nummetlu == 33 ) ) THEN
    xlusrc = "MODIS NOAH"
    lwater = 17
    DO i = 1, nummetlu
      xludesc(i) = TRIM(xlusrc) // ': ' // TRIM(lucatmod(i))
    ENDDO
  ELSE IF ( ( met_lu_src(1:3) == "MOD" ) .AND. ( nummetlu == 20 ) ) THEN
    xlusrc = "MODIS NOAH"
    lwater = 17
    DO i = 1, nummetlu
      xludesc(i) = TRIM(xlusrc) // ': ' // TRIM(lucatmod(i))
    ENDDO
  ELSE IF ( ( met_lu_src(1:3) == "NLC" ) .AND. ( nummetlu == 50 ) ) THEN
    xlusrc = "NLCD-MODIS"
    lwater = 1  ! and also 31 and 47...to be combined, below
    DO i = 1, nummetlu
      xludesc(i) = TRIM(xlusrc) // ': ' // TRIM(lucatnlcd(i))
    ENDDO
  ELSE IF ( ( ( met_lu_src(1:3) == "SiB" ) .OR. ( met_lu_src(1:3) == "SIB" ) ) &
            .AND. ( nummetlu == 16 ) ) THEN
    xlusrc = "SIB"
    lwater = 15
    DO i = 1, nummetlu
      xludesc(i) = TRIM(xlusrc) // ': ' // TRIM(lucatsib(i))
    ENDDO
  ELSE
    WRITE (6,9100) met_lu_src, nummetlu
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Fill input land use from meteorological model into output arrays.
!-------------------------------------------------------------------------------

  IF ( iflufrc ) THEN

    sc = x0
    ec = x0 + ncols_x - 1
    sr = y0
    er = y0 + nrows_x - 1

    xluse (:,:,1:lumax) = lufrac (sc:ec,sr:er,:)
    xdluse(:,:)         = landuse(sc:ec,sr:er)

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

    ! Adjust NLCD-MODIS to consolidate all water into single category.

    IF ( TRIM(xlusrc) == "NLC" ) THEN  ! NLCD-MODIS
      xluse(:,:, 1) = xluse(:,:,1) + xluse(:,:,31) + xluse(:,:,47)
      xluse(:,:,31) = 0.0
      xluse(:,:,47) = 0.0
    ENDIF

    DO col = 1, ncols_x
      DO row = 1, nrows_x

        ! Convert "water" to "open water".

        IF ( xdluse(col,row) == 31 ) THEN
          xdluse(col,row) = 1
        ENDIF

        ! Convert "IGBP water" to "open water".

        IF ( xdluse(col,row) == 47 ) THEN
          xdluse(col,row) = 1
        ENDIF

        ! If there was overlap in water categories, ensure dominant category
        ! is water if the sum is greater than 50% of the cell.

        IF ( xluse(col,row,1) > 0.50 ) THEN
          xdluse(col,row) = 1
        ENDIF

      ENDDO
    ENDDO

  ENDIF

!-------------------------------------------------------------------------------
! Define land-water mask.
!-------------------------------------------------------------------------------

  WHERE ( NINT(xdluse) == lwater )  ! water points
    xlwmask = 0.0
  ELSEWHERE                         ! land points
    xlwmask = 1.0
  END WHERE

!-------------------------------------------------------------------------------
! Fill percentage of urban area (PURB) based on amount of land in grid cell.
! When urban canopy model is used in WRF, use fraction of urban area in
! cell (FRC_URB) to fill PURB.
!-------------------------------------------------------------------------------

  IF ( met_urban_phys >= 1 ) THEN  ! urban canopy model used in WRF; use FRC_URB

    sc = x0
    ec = x0 + ncols_x - 1
    sr = y0
    er = y0 + nrows_x - 1

    xpurb(:,:) = frc_urb(sc:ec,sr:er) * 100.0  ! [fraction -> percent]

  ELSE IF ( iflufrc ) THEN  ! fractional land use available

    DO row = 1, nrows_x
      DO col = 1, ncols_x
 
        IF ( NINT(xdluse(col,row)) == lwater ) THEN  ! water is dominant
          xpurb(col,row) = 0.0
        ELSE  ! land is dominant over water in cell
          IF ( xluse(col,row,lwater) < 1.0 ) THEN
            IF ( TRIM(xlusrc) == 'USGS33' ) THEN
              xpurb(col,row) = ( ( xluse(col,row,1)  + xluse(col,row,31) +    &
                                   xluse(col,row,32) + xluse(col,row,33) ) /  &
                                 (1.0 - xluse(col,row,lwater)) ) * 100.0
            ELSE IF ( TRIM(xlusrc) == 'MODIS NOAH' ) THEN
              IF ( nummetlu == 33 ) THEN
                xpurb(col,row) = ( ( xluse(col,row,13) + xluse(col,row,31) +    &
                                     xluse(col,row,32) + xluse(col,row,33) ) /  &
                                   (1.0 - xluse(col,row,lwater)) ) * 100.0
              ELSE IF ( nummetlu == 20 ) THEN
                xpurb(col,row) = ( xluse(col,row,13) /  &
                                   (1.0 - xluse(col,row,lwater)) ) * 100.0
              ENDIF
            ELSE IF ( TRIM(xlusrc) == 'NLCD-MODIS' ) THEN
              xpurb(col,row) = ( ( xluse(col,row,3) * 0.10 +    &
                                   xluse(col,row,4) * 0.35 +    &
                                   xluse(col,row,5) * 0.65 +    &
                                   xluse(col,row,6) * 0.90 +    &
                                   xluse(col,row,44)       ) /  &
                                 (1.0 - xluse(col,row,lwater)) ) * 100.0
            ELSE IF ( TRIM(xlusrc) == 'SIB' ) THEN
              xpurb(col,row) = 0.0  ! urban is not specified in SiB
            ELSE
              xpurb(col,row) = ( xluse(col,row,1) /  &
                                 (1.0 - xluse(col,row,lwater)) ) * 100.0
            ENDIF
          ELSE
            xpurb(col,row) = 0.0
          ENDIF
        ENDIF

      ENDDO
    ENDDO

  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: GETLUSE',                            &
              /, 1x, '***   TOO MANY INPUT LAND USE CATEGORIES',           &
              /, 1x, '***   MAXIMUM ALLOWED (MAXLUC) = ', i4,              &
              /, 1x, '***   ATTEMPTED SIZE = ', i4,                        &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: GETLUSE',                            &
              /, 1x, '***   UNKNOWN LAND USE SOURCE AND MAX CATEGORIES',   &
              /, 1x, '***   LAND USE SOURCE = ', a,                        &
              /, 1x, '***   NUMBER OF CATEGORIES = ', i4,                  &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE getluse
