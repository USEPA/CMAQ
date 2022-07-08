
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
      module cctm_species

c CGRID gas chem, aerosol, non-reactive, and tracer species definitions
c based on namelist specifications
c Revision History:
c Jeff Young 31 Aug 09: created
c Bill Hutzell 8 Oct 10: added Gas Chemistry consistency check
C Shawn Roselle 16 Feb 11: replaced I/O API include files with UTILIO_DEFN
C 07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module
C 21 Apr 16 D.Luecken:  increased spc_dim to 1000
C 07 Mar 2019 F. Sidi:  Split up ICBC for CMAQ species  namelist 
c-----------------------------------------------------------------------

      implicit none

c main CGRID table
      integer,                      save :: n_gc_spc = 0, n_ae_spc = 0, n_nr_spc = 0, n_tr_spc = 0
      character( 16 ), allocatable, save :: gc_spc( : ),  ae_spc( : ),  nr_spc( : ),  tr_spc( : )
      real, allocatable,            save :: gc_molwt( : ), ae_molwt( : ), nr_molwt( : ), tr_molwt( : )

      integer,                      save :: n_gc_spcd = 0

c Starting and ending index of gas chemistry species in CGRID
      integer,                      save :: gc_strt = 1
      integer,                      save :: gc_fini = 0

c Starting and ending index of aerosol species in CGRID
      integer,                      save :: ae_strt = 0
      integer,                      save :: ae_fini = 0

c Starting and ending index of non-reactive species in CGRID
      integer,                      save :: nr_strt = 0
      integer,                      save :: nr_fini = 0

c Starting and ending index of tracer species in CGRID
      integer,                      save :: tr_strt = 0
      integer,                      save :: tr_fini = 0

c number of species in CGRID
      integer,                      save :: nspcsd = 1

      private :: index1a

      contains

            SUBROUTINE SET_AERO_MODE_NAMES( COMPONENT,FLAGS,NAMES )
!         This subroutine expands the aerosol component to set 
!         mode-specific names.
              
              IMPLICIT NONE
              
              CHARACTER*(*), INTENT( IN    ) :: COMPONENT
              LOGICAL,       INTENT( INOUT ) :: FLAGS( : )
              CHARACTER*(*), INTENT( INOUT ) :: NAMES( : )
              
              INTEGER ISP, IM, IAER, IOST, IMODE

              CHARACTER(20), PARAMETER :: PNAME = 'SET_AERO_MODE_NAMES'
              
              LOGICAL, SAVE :: INITIALIZE = .TRUE.
              
              INTEGER NSP2          
          
              IF( INITIALIZE )THEN
                N_AE_SPC   = 0
                INITIALIZE = .FALSE.
              END IF
                        
              NSP2 = 0
              
              NAMES = ''

              IF ( LEN_TRIM( COMPONENT ) .EQ. 0 )THEN
                 FLAGS = .FALSE.
                 RETURN
              END IF

              ! For Aitken-Mode Particles
              IF ( FLAGS(1) ) THEN
                   IF ( TRIM( COMPONENT ) .EQ. 'NUM' .OR. 
     &                  TRIM( COMPONENT ) .EQ. 'SRF' ) THEN
                      ! Add Aitken Mode Number or Surface Area
                      NSP2 = NSP2 + 1
                      NAMES(1) =  trim(COMPONENT)//'ATKN'
                   ELSE
                      ! Add Aitken Mode Mass
                      NSP2 = NSP2 + 1
                      NAMES(1) =  trim(COMPONENT)//'I'
                   END IF
              END IF

              ! For Accumulation-Mode Particles
              IF ( FLAGS(2) ) THEN
                   IF ( TRIM( COMPONENT ) .EQ. 'NUM' .OR. 
     &                  TRIM( COMPONENT ) .EQ. 'SRF' ) THEN
                      ! Add Accumulation Mode Number or Surface Area
                      NSP2 = NSP2 + 1
                      NAMES(2) =  trim(COMPONENT)//'ACC'
                   ELSE
                      ! Add Accumulation Mode Mass
                      nsp2 = nsp2 + 1
                      NAMES(2) =  trim(COMPONENT)//'J'
                   END IF
              END IF
             
              ! For Coarse-Mode Particles
              IF ( FLAGS(3) ) THEN
                 IF ( TRIM( COMPONENT ) .EQ. 'NUM' .OR. 
     &                TRIM( COMPONENT ) .EQ. 'SRF' ) THEN
                    ! Add Coarse Mode Number or Surface Area
                    NSP2 = NSP2 + 1
                    NAMES(3) =  trim(COMPONENT)//'COR'
                 ELSE
                    ! Add Coarse Mode Mass
                    NSP2 = NSP2 + 1
                    IF ( TRIM( COMPONENT )  .EQ. 'ACORS' .OR.
     &                   TRIM( COMPONENT )  .EQ. 'ASOIL' .OR.
     &                   TRIM( COMPONENT )  .EQ. 'ASEACAT' .OR.
     &                   TRIM( COMPONENT )  .EQ. 'ADE_CORS' ) THEN
                         NAMES(3) =  TRIM(COMPONENT)
                    ELSE  
                         NAMES(3) =  TRIM(COMPONENT)//'K'
                    END IF
                 END IF
              END IF

           END SUBROUTINE SET_AERO_MODE_NAMES

         function map_cctm_species() result ( success )

C----------------------------------------------------------------------- 
c  function maps chemistry species to CCTM species

            USE GET_ENV_VARS
            USE MECHANISM_DATA

            implicit none

            logical success

            integer i,  i1, j, ios, IGC, IAE, INR, ITR
            character(  1 ), parameter :: bl = ' '
            integer, parameter :: spc_dim = 1000
            logical :: order = .true., found = .true.
            character( 120 ) :: xmsg
            INTEGER :: ISPC, IGRID

            integer, save :: logdev = 6


            character( 16 ) :: gc_matrix = 'gc_matrix_nml'           
            character( 16 ) :: ae_matrix = 'ae_matrix_nml'      
            character( 16 ) :: nr_matrix = 'nr_matrix_nml'      
            character( 16 ) :: tr_matrix = 'tr_matrix_nml'      
                                                                
            integer, external :: junit                          
            integer, external :: index1                         
                                                                
            character( 16 ), allocatable     :: nml_spc  ( : )  
            character(  2 ), allocatable     :: nml_type ( : )  
            integer,         allocatable     :: nml_index( : )  
            logical,         allocatable     :: nml_convert( : )
            real,            allocatable     :: nml_molwt( : )  

c----------------------------------------------------------------------------------------
            success = .true.

            ! Determine Number of Species Boundaries
            GC_FINI   = GC_STRT + N_GC_SPC - 1
            N_GC_SPCD = GC_FINI + 1
            AE_STRT   = GC_FINI + 2
            AE_FINI   = AE_STRT + N_AE_SPC - 1
            NR_STRT   = AE_FINI + 1
            NR_FINI   = NR_STRT + N_NR_SPC - 1
            TR_STRT   = NR_FINI + 1
            TR_FINI   = TR_STRT + N_TR_SPC - 1
            ! Sum Up All Species Across Phases and Types
            NSPCSD = N_GC_SPCD + N_AE_SPC + N_NR_SPC + N_TR_SPC
 
            allocate ( cgrid_spc( nspcsd - 1 ), nml_spc( nspcsd - 1 ),                                       
     &                 nml_index( nspcsd - 1 ), nml_type( nspcsd - 1 ),                                      
     &                 nml_molwt( nspcsd - 1 ), type_index( nspcsd - 1 ),                                    
     &                 nml_convert( nspcsd - 1 ), stat = ios )                                               
                                                                                                             
            allocate ( species_molwt( nspcsd - 1 ), convert_conc( nspcsd - 1 ), stat = ios )                 
                                                                                                             
                                                                                                             
            j = 0                                                                                            
            cgrid_spc  = 'BLANK'                                                                             
            nml_index  = -1                                                                                  
            type_index = -1                                                                                  
            nml_type   = '??'                                                                                
            nml_molwt  = -1.0                                                                                
            convert_conc = .False.
            nml_convert  = .False.                                                                          
                                                                                                        
            do i = 1, n_gc_spc ! load GC names and indices                                                   
               j = j + 1                                                                                     
               cgrid_spc( i )   = gc_spc( i )                                                                
               nml_index( j )  = i + gc_strt -1                                                              
               type_index( j ) = i                                                                           
               nml_type( j )   = 'GC'                                                                        
               nml_molwt( j ) = gc_molwt( i )                                                                
            end do                                                                                           
                                                                                                             
            do i = 1, n_ae_spc ! load AE names and indices                                                   
               j = j + 1                                                                                     
               cgrid_spc( j )    = ae_spc( i )                                                               
               nml_index( j )    = i + ae_strt - 1                                                           
               nml_type( j )     = 'AE'                                                                      
               type_index( j )   = i                                                                         
               nml_molwt( j )    = ae_molwt( i )                                                             
               nml_convert( j )  = .True.                                                                    
            end do                                                                                           
                                                                                                             
            do i = 1, n_nr_spc ! load NR names and indices                                                   
               j = j + 1                                                                                     
               cgrid_spc( j )   = nr_spc( i )                                                                
               nml_index( j )   = i + nr_strt - 1                                                            
               nml_type( j )    = 'NR'                                                                       
               type_index( j )  = i                                                                          
               nml_molwt( j ) = nr_molwt( i )                                                                
            end do                                                                                           
                                                                                                             
            do i = 1, n_tr_spc ! load TR names and indices                                                   
               j = j + 1                                                                                     
               cgrid_spc( j )   = tr_spc( i )                                                                
               nml_index( j )   = i + tr_strt - 1                                                            
               nml_type( j )    = 'TR'                                                                       
               type_index( j )  = i                                                                          
               nml_molwt( j )   = tr_molwt( i )                                                              
            end do                                                                                           
                                                                                                             
            nml_spc( 1:(nspcsd-1) ) = cgrid_spc( 1:(nspcsd-1) )                                              
                                                                                                             
                                                                                                             
C determine if mechanism species are in cgrid species                                                        
                                                                                                             
            do i = 1, numb_mech_spcs                                                                         
               i1 = index1a( mechanism_spc( i ), (nspcsd-1), cgrid_spc )                                     
               if ( i1 .lt. 1 ) then                                                                         
                  found = .false.                                                                            
               else                                                                                          
                  found = .true.                                                                             
                  cgrid_index( i )   = nml_index( i1 )                                                       
                  species_type( i )  = nml_type ( i1 )                                                       
                  species_molwt( i ) = nml_molwt( i1 )                                                       
                  convert_conc( i )  = nml_convert( i1 )
               end if                                                                                        
               if( index( mechanism_spc( i ), 'SRF') .gt. 0 )then                                            
                   found = .false.                                                                           
                   xmsg = '*** reactions cannot use modal aerosol surface area as species'                   
                   write( logdev,'( /5x, a )' ) trim( xmsg )                                                 
                   xmsg = trim( mechanism_spc( i ) )                                                         
                   write( logdev,'( 9x, i4, 2x, a )' ) i, trim( xmsg )                                       
               end if                                                                                        
               if( index( mechanism_spc( i ), 'NUM') .gt. 0 )then                                            
                   found = .false.                                                                           
                   xmsg = '*** reactions cannot use modal aerosol number density as species'                 
                   write( logdev,'( /5x, a )' ) trim( xmsg )                                                 
                   xmsg = trim( mechanism_spc( i ) )                                                         
                   write( logdev,'( 9x, i4, 2x, a )' ) i, trim( xmsg )                                       
               end if                                                                                        
               if ( .Not. found ) then                                                                       
                  xmsg = 'Fatal error: Mechanism Species found not in species namelist:'                     
                  write( logdev,'( /5x, a )', ADVANCE = 'NO' ) trim( xmsg )                                  
                  xmsg = trim( mechanism_spc( i ) )                                                          
                  write( logdev,'( 9x, i4, 2x, a )' ) i, trim( xmsg )                                        
                  success = found                                                                            
               end if                                                                                        
            end do                                                                                           

         return

         end function map_cctm_species
c----------------------------------------------------------------------------------------
          integer function index1a ( name, n, nlist )
              implicit none
              character( * ) name        ! character string being searched for
              integer n                  ! length of array to be searched
              character( * ) nlist( : )  ! array to be searched
             
              integer i
             
              do i = 1, n
                 if ( name .eq. nlist( i ) ) then
                    index1a = i
                    return
                 end if
             end do
             index1a = 0
             return
         end function index1a
      end module cctm_species

