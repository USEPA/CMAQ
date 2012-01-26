
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

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/aero/aero6_mp/PRECURSOR_DATA.F,v 1.3 2012/01/26 18:44:38 sjr Exp $

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

C HS  03/10/11 Made OH a required species
C    -added OH to "precursor" array (set rxncounter to .FALSE.);
C    -added "update" column to precursor array to note which species
C     concentrations will be affected by aerosol routines
C    -added req_OH and OH_idx variables
C    -changed precursor_conc to Real(8)

C H.Pye 05/22/11: added ALK5RXN to precursor list for SAPRC mechanisms
C
C-----------------------------------------------------------------------

      Implicit None

      Integer, Parameter :: n_precursor = 20   ! # of precursors

      Type precursor_type
         Character( 16 ) :: name               ! species name
         Logical         :: rxncounter         ! flag to reset counter species
         Logical         :: update             ! update precursor concentration
                                               ! in cgrid after aeroproc
      End Type precursor_type

      Type ( precursor_type ), Private :: precursor( n_precursor ) = (/
C                       Name      RxnCounter  Update
C                    ----------   ----------  ------
     & precursor_type( 'NO2             ', .False. , .True.  ),
     & precursor_type( 'N2O5            ', .False. , .True.  ),
     & precursor_type( 'HNO3            ', .False. , .True.  ),
     & precursor_type( 'HONO            ', .False. , .True.  ),
     & precursor_type( 'ISOPRXN         ', .True.  , .True.  ),
     & precursor_type( 'TRPRXN          ', .True.  , .True.  ),
     & precursor_type( 'SULF            ', .False. , .True.  ),
     & precursor_type( 'SULPRD          ', .True.  , .True.  ),
     & precursor_type( 'HCL             ', .False. , .True.  ),
     & precursor_type( 'ALK5RXN         ', .True.  , .True.  ),
     & precursor_type( 'TOLNRXN         ', .True.  , .True.  ),
     & precursor_type( 'TOLHRXN         ', .True.  , .True.  ),
     & precursor_type( 'XYLNRXN         ', .True.  , .True.  ),
     & precursor_type( 'XYLHRXN         ', .True.  , .True.  ),
     & precursor_type( 'BNZNRXN         ', .True.  , .True.  ),
     & precursor_type( 'BNZHRXN         ', .True.  , .True.  ),
     & precursor_type( 'SESQRXN         ', .True.  , .True.  ),
     & precursor_type( 'NH3             ', .False. , .True.  ),
     & precursor_type( 'OH              ', .False. , .False. ),
     & precursor_type( 'PHGRXN          ', .True.  , .True. )/)

C Required species
      Character( 16 ), Private, Parameter :: req_NO2    = 'NO2             '
      Character( 16 ), Private, Parameter :: req_N2O5   = 'N2O5            '
      Character( 16 ), Private, Parameter :: req_HNO3   = 'HNO3            '
      Character( 16 ), Private, Parameter :: req_HONO   = 'HONO            '
      Character( 16 ), Private, Parameter :: req_SULF   = 'SULF            '
      Character( 16 ), Private, Parameter :: req_SULPRD = 'SULPRD          '
      Character( 16 ), Private, Parameter :: req_HCL    = 'HCL             '
      Character( 16 ), Private, Parameter :: req_NH3    = 'NH3             '
      Character( 16 ), Private, Parameter :: req_OH     = 'OH              '
      Character( 16 ), Private, Parameter :: req_PHGRXN = 'PHGRXN          '

C Indices of required species
      Integer :: NO2_idx
      Integer :: N2O5_idx
      Integer :: HNO3_idx
      Integer :: HONO_idx
      Integer :: SULF_idx
      Integer :: SULPRD_idx
      Integer :: HCL_idx
      Integer :: NH3_idx
      Integer :: OH_idx
      Integer :: PHGRXN_idx

      Real    :: precursor_mw( n_precursor )   ! mol wt from cgrid_spcs [g/mol]

      Real( 8 ) :: precursor_conc( n_precursor ) ! precursor concentration [ug/m^3]

      Real    :: so4rate                       ! sulfate gas-phase production rate [ug/m^3*s]
      Real    :: phg_rate                      ! particulate mercury gas-phase production rate [ug/m^3*s]

C Private variables for loading and unloading to CGRID array
      Integer          :: precursor_map( n_precursor )  ! pointers to CGRID
      Logical, Private, Save :: mapped = .False.

      Integer, Private, Save :: logdev
      Integer, Private, External :: setup_logdev

      Contains

C-----------------------------------------------------------------------
      Subroutine map_precursor()

C  Defines mapping from CGRID for species concentration and moments.

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.

C HS  03/10/11 Changed vtmp to real(8)
C SR  03/25/11 Replaced I/O API include files with UTILIO_DEFN
C----------------------------------------------------------------------

      Use aero_data
      Use cgrid_spcs          ! CGRID mechanism species
      Use utilio_defn

      Implicit None

C Local Variables:
      Character( 180 ) :: xmsg
      Real( 8 )        :: vtmp
      Integer          :: n
      Integer          :: spc

      logdev = setup_logdev()

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
!               Call m3exit( 'map_precursor', 0, 0, xmsg, xstat3 )
                Call m3warn( 'map_precursor', 0, 0, xmsg )
            End If
         End If
      End Do

C Find indices of required species
      NO2_idx    = findPrecursor( req_NO2,    .true.)
      N2O5_idx   = findPrecursor( req_N2O5,   .true.)
      HNO3_idx   = findPrecursor( req_HNO3,   .true.)
      HONO_idx   = findPrecursor( req_HONO,   .true.)
      SULF_idx   = findPrecursor( req_SULF,   .true.)
      SULPRD_idx = findPrecursor( req_SULPRD, .true.)
      HCL_idx    = findPrecursor( req_HCL,    .true.)
      NH3_idx    = findPrecursor( req_NH3,    .true.)
      OH_idx     = findPrecursor( req_OH,     .true.)
      If( aphgj_idx .gt. 0 )then
          PHGRXN_idx = findPrecursor( req_phgrxn,  .false.)
          If(   precursor_map( PHGRXN_idx ) .Lt. 1  )Then
             xmsg = ' Gas Phase Precursor ' // Trim( req_PHGRXN )
     &           // ' not found -> NO gas production of APHG species'
             Call m3warn( 'map_precursor', 0, 0, xmsg )
          End If 
#ifdef verbose_aero
      Else
          xmsg = ' NO particulate mercury in model.'
          Call m3warn( 'map_precursor', 0, 0, xmsg )
          PHGRXN_idx = 0
#endif
      end if

#ifdef verbose_aero
      Write( logdev,'( /5x, a )' ) 'map_precursor required species'
      Write( logdev,'( 5x, a, i4 )' ) 'NO2_idx:    ', NO2_idx
      Write( logdev,'( 5x, a, i4 )' ) 'N2O5_idx:   ', N2O5_idx
      Write( logdev,'( 5x, a, i4 )' ) 'HNO3_idx:   ', HNO3_idx
      Write( logdev,'( 5x, a, i4 )' ) 'HONO_idx:   ', HONO_idx
      Write( logdev,'( 5x, a, i4 )' ) 'SULF_idx:   ', SULF_idx
      Write( logdev,'( 5x, a, i4 )' ) 'SULPRD_idx: ', SULPRD_idx
      Write( logdev,'( 5x, a, i4 )' ) 'HCL_idx:    ', HCL_idx
      Write( logdev,'( 5x, a, i4 )' ) 'NH3_idx:    ', NH3_idx
      Write( logdev,'( 5x, a, i4 )' ) 'OH_idx:     ', OH_idx
      if( aphgj_idx .gt. 0 )then
          Write( logdev,'( 5x, a, i4 )' ) 'PHGRXN_idx:     ', PHGRXN_idx
      end if
#endif

      mapped = .True.

      Write( logdev,'( 5x, a )' ) ' --- Precursor Species Mapped ---'

      Return
      End Subroutine map_precursor

C-----------------------------------------------------------------------
      Subroutine extract_precursor( conc )

C  Extracts the required precursor data from CGRID into conc.

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.

C SH  03/10/11 Renamed met_data to aeromet_data
C HS  03/10/11 Changed vtmp and gasconv to real(8)
C-----------------------------------------------------------------------

      Use aeromet_data            ! air and met variables
      Use utilio_defn

      Implicit None

C Arguments:
      Real, Intent( In ) :: conc( : )

C Local Variables:
      Real( 8 )       :: gasconv       ! converts from [ppm] to [ug/m^3]
      Real( 8 )       :: vtmp
      Integer         :: n
      Integer         :: spc

      If ( .Not. mapped ) Then
         Call map_precursor()
      End If

C Compute gas conversion constant
      gasconv = Real( airdens * 1.0E3 / mwair, 8 )

C Copy grid cell concentrations of precursor species
      precursor_conc = 0.0

      Do spc = 1, n_precursor
         n = precursor_map( spc )
         If ( n .Ne. 0 ) Then
            vtmp = gasconv * precursor_mw( spc )
            precursor_conc( spc ) = Max( Real( conc( n ), 8) * vtmp, Real( min_gasconc, 8) )
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

C SH  03/10/11 Renamed met_data to aeromet_data
C HS  03/10/11 Only update conc for species with "update" = .TRUE.;
C    -changed vtmp and gasconv to real(8)
C SR  03/25/11 Replaced I/O API include files with UTILIO_DEFN
C-----------------------------------------------------------------------

      Use aeromet_data        ! air and met variables
      Use utilio_defn

      Implicit None

C arguments:
      Real, Intent( Out ) :: conc( : )

C local variables:
      Character( 80 ) :: xmsg
      Real( 8 )       :: gasconv       ! converts from [ppm] to [ug/m^3]
      Real( 8 )       :: vtmp
      Integer         :: n
      Integer         :: spc

      If ( .Not. mapped ) Then
         xmsg = 'CGRID Species has not been mapped'
         Call m3exit( 'update_precursor', 0, 0, xmsg, xstat3 )
      End If

C compute gas conversion constant
      gasconv = Real( airdens * 1.0E3 / mwair, 8 )

C copy precursor_conc back to grid cell concentrations
      Do spc = 1, n_precursor
         n = precursor_map( spc )
         If ( n .Ne. 0 ) Then
            If ( precursor( spc )%update ) Then
               If ( precursor( spc )%rxncounter ) Then
                  conc( n ) = 0.0
               Else
                  vtmp = precursor_mw( spc ) * gasconv
                  conc( n ) = Max( precursor_conc( spc ) / vtmp, real(min_gasconc, 8) )
               End If
            End If
         End If
      End Do

      Return
      End Subroutine update_precursor

C-----------------------------------------------------------------------
      Function findPrecursor( vname, required ) Result ( ndx )

C  Finds the index of 'required' aerosol species in the precursor list

C  Revision History:
C     First version was coded in April 2010 by Steve Howard with
C     Prakash Bhave, Jeff Young, and Sergey Napelenok.
C
C SR  03/25/11 Replaced I/O API include files with UTILIO_DEFN
C-----------------------------------------------------------------------

      Use utilio_defn

      Implicit None

C Arguments:
      Character( * ) :: vname
      Logical        :: required
      
      Integer ndx

C Local Variables:
      Integer         :: spc
      Character( 80 ) :: xmsg

      ndx = 0
      
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
