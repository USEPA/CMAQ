
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/aero/aero6/PRECURSOR_DATA.F,v 1.2 2011/03/15 19:11:56 sjr Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module precursor_data

C  Defines gas phase aerosol precursor data.

C  Contains:
C     Subroutine extract_precursor
C     Subroutine map_precursor
C     Subroutine update_precursor
C     Function findPrecursor

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C-----------------------------------------------------------------------

      Implicit None

      Integer, Parameter :: n_precursor = 17   ! # of precursors

      Type precursor_type
         Character( 16 ) :: name               ! species name
         Logical         :: accum              ! accumulates
      End Type precursor_type

      Type ( precursor_type ), Private :: precursor( n_precursor ) = (/
C                       Name       Accum
C                    ----------   -------
     & precursor_type( 'NO2     ', .False. ),
     & precursor_type( 'N2O5    ', .False. ),
     & precursor_type( 'HNO3    ', .False. ),
     & precursor_type( 'HONO    ', .False. ),
     & precursor_type( 'ISOPRXN ', .True.  ),
     & precursor_type( 'TRPRXN  ', .True.  ),
     & precursor_type( 'SULF    ', .False. ),
     & precursor_type( 'SULPRD  ', .True.  ),
     & precursor_type( 'HCL     ', .False. ),
     & precursor_type( 'TOLNRXN ', .True.  ),
     & precursor_type( 'TOLHRXN ', .True.  ),
     & precursor_type( 'XYLNRXN ', .True.  ),
     & precursor_type( 'XYLHRXN ', .True.  ),
     & precursor_type( 'BNZNRXN ', .True.  ),
     & precursor_type( 'BNZHRXN ', .True.  ),
     & precursor_type( 'SESQRXN ', .True.  ),
     & precursor_type( 'NH3     ', .False. ) /)

C Required species
      Character( 16 ), Private, Parameter :: req_NO2    = 'NO2'
      Character( 16 ), Private, Parameter :: req_N2O5   = 'N2O5'
      Character( 16 ), Private, Parameter :: req_HNO3   = 'HNO3'
      Character( 16 ), Private, Parameter :: req_HONO   = 'HONO'
      Character( 16 ), Private, Parameter :: req_SULF   = 'SULF'
      Character( 16 ), Private, Parameter :: req_SULPRD = 'SULPRD'
      Character( 16 ), Private, Parameter :: req_HCL    = 'HCL'
      Character( 16 ), Private, Parameter :: req_NH3    = 'NH3'

C Indices of required species
      Integer :: NO2_idx
      Integer :: N2O5_idx
      Integer :: HNO3_idx
      Integer :: HONO_idx
      Integer :: SULF_idx
      Integer :: SULPRD_idx
      Integer :: HCL_idx
      Integer :: NH3_idx

      Real    :: precursor_mw( n_precursor )   ! precursor M.W., g/mol from GC_SPC.EXT
                                               ! or NR_SPC.EXT

      Real    :: precursor_conc( n_precursor ) ! precursor concentration [ug/m^3]

      Real    :: so4rate                       ! sulfate gas-phase production rate [ug/m^3*s]

C Private variables for loading and unloading to CGRID array
      Integer          :: precursor_map( n_precursor )  ! pointers to CGRID
      Logical, Private, Save :: mapped = .False.

      Contains

C-----------------------------------------------------------------------
      Subroutine map_precursor()

C  Defines mapping from CGRID for species concentration and moments.

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C----------------------------------------------------------------------

      Use cgrid_spcs          ! CGRID mechanism species

      Implicit None

C Includes:
      Include SUBST_IOPARMS   ! I/O parameters definitions

C Functions:
      Integer, External :: index1

C Local Variables:
      Character( 80 ) :: xmsg
      Real            :: vtmp
      Integer         :: n
      Integer         :: spc

      precursor_mw = 0.0
      precursor_map = 0

C Build mapping to CGRID for each precursor species
      Do spc = 1, n_precursor
         n = index1( precursor( spc )%name, n_gc_g2ae, gc_g2ae )
         If ( n .Ne. 0 ) Then
            precursor_mw( spc ) = gc_molwt( gc_g2ae_map( n ) )
            precursor_map( spc ) = gc_strt - 1 + gc_g2ae_map( n )
         Else
            n = index1( precursor( spc )%name, n_nr_n2ae, nr_n2ae )
            If ( n .Ne. 0 ) Then
               precursor_mw( spc ) = nr_molwt( nr_n2ae_map( n ) )
               precursor_map( spc ) = nr_strt - 1 + nr_n2ae_map( n )
            Else
               xmsg = 'Species ' // Trim( precursor( spc )%name ) //
     &               ' in precursor name is not in GC_G2AE or NR_N2AE tables'
               Call m3exit( 'xtract_precursor', 0, 0, xmsg, xstat3 )
            End If
         End If
      End Do

C Find indices of required species
      NO2_idx    = findPrecursor( req_NO2 )
      N2O5_idx   = findPrecursor( req_N2O5 )
      HNO3_idx   = findPrecursor( req_HNO3 )
      HONO_idx   = findPrecursor( req_HONO )
      SULF_idx   = findPrecursor( req_SULF )
      SULPRD_idx = findPrecursor( req_SULPRD )
      HCL_idx    = findPrecursor( req_HCL )
      NH3_idx    = findPrecursor( req_NH3 )

      mapped = .True.
      Return
      End Subroutine map_precursor

C-----------------------------------------------------------------------
      Subroutine extract_precursor( conc )

C  Extracts the required precursor data from CGRID into conc.

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C
C SH  03/10/11 Renamed met_data to aeromet_data
C-----------------------------------------------------------------------

      Use aeromet_data    ! air and met variables

      Implicit None

C Arguments:
      Real, Intent( In ) :: conc( : )

C Functions:
      Integer, External :: index1

C Local Variables:
      Real            :: gasconv       ! converts from [ppm] to [ug/m^3]
      Real            :: vtmp
      Integer         :: n
      Integer         :: spc

      If ( .Not. mapped ) Then
         Call map_precursor()
      End If

C Compute gas conversion constant
      gasconv = airdens * 1.0E3 / mwair

C Copy grid cell concentrations of precursor species
      precursor_conc = 0.0

      Do spc = 1, n_precursor
         n = precursor_map( spc )
         If ( n .Ne. 0 ) Then
            vtmp = gasconv * precursor_mw( spc )
            precursor_conc( spc ) = Max( conc( n ) * vtmp, min_gasconc )
         End If
      End Do

      Return
      End Subroutine extract_precursor

C------------------------------------------------------------------------
      Subroutine update_precursor( conc )

C  Updates CGRID aerosol precursor values from the conc array.

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C
C SH  03/10/11 Renamed met_data to aeromet_data
C-----------------------------------------------------------------------

      Use aeromet_data    ! air and met variables

      Implicit None

C arguments:
      Real, Intent( Out ) :: conc( : )

C includes:
      Include SUBST_IOPARMS   ! I/O parameters definitions

C local variables:
      Character( 80 ) :: xmsg
      Real            :: gasconv       ! converts from [ppm] to [ug/m^3]
      Real            :: vtmp
      Integer         :: n
      Integer         :: spc

      If ( .Not. mapped ) Then
         xmsg = 'CGRID Species has not been mapped'
         Call m3exit( 'update_precursor', 0, 0, xmsg, xstat3 )
      End If

C compute gas conversion constant
      gasconv = airdens * 1.0E3 / mwair

C copy precursor_conc back to grid cell concentrations
      Do spc = 1, n_precursor
         n = precursor_map( spc )
         If ( n .Ne. 0 ) Then
            If ( precursor( spc )%accum ) Then
               conc( n ) = 0.0
            Else
               vtmp = precursor_mw( spc ) * gasconv
               conc( n ) = Max( precursor_conc( spc ) / vtmp, min_gasconc )
            End If
         End If
      End Do

      Return
      End Subroutine update_precursor

C-----------------------------------------------------------------------
      Function findPrecursor( vname ) Result ( ndx )

C  Finds the index of 'required' aerosol species in the precursor list

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C-----------------------------------------------------------------------

      Implicit None

C Arguments:
      Character( * ) :: vname
      Integer ndx

C Includes:
      Include SUBST_IOPARMS   ! I/O parameters definitions

C Local Variables:
      Integer         :: spc
      Character( 80 ) :: xmsg

      Do spc = 1, n_precursor
         If ( Index( precursor( spc )%name, Trim( vname ) ) .Gt. 0 ) Then
            ndx = spc
            Return
         End If
      End Do

      xmsg = 'Required Species ' // Trim( vname )
     &     // ' Not found in [precursor names] array'
      Call m3exit( 'findPrecursor', 0, 0, xmsg, xstat3 )

      Return
      End Function findPrecursor

      End Module precursor_data
