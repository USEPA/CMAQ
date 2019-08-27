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

C*************************************************************************
C
C  MODULE:  defines a set of species variables
C
C*************************************************************************
      MODULE SPECIES

      Character*(16) :: SPECVARS(2048)
      INTEGER NSPECVAR
 
      CONTAINS
 
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C   ROUTINE TO GET SPECIES DEFINITIONS FROM ENVIRONMENT VARIABLES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         SUBROUTINE LoadSpecies

         USE M3UTILIO
 
         IMPLICIT NONE

         ! local variables
         Integer k, n
         Integer status
         Character*(16)  varname
         Character*(16)  field   

         NSPECVAR = 0
         do k=1,2048
 
           !  build VARNAME
           write(field,'(i5)') k
           Call leftTrim(field)
           varname = 'SPECIES_' // TRIM(field)
 
           ! get environment variable varname
           CALL ENVSTR( varname, 'Species Variable definition', 'Not Used', field, status)
           IF( status .ne. 0 ) EXIT

           ! check for SPECIES_1 = 'ALL" for all species
           if( k.eq.1 .and. TRIM(field).eq.'ALL' ) then
  
             NSPECVAR = NVARS3D
	     
             if( NSPECVAR.gt.SIZE(SPECVARS) ) then
                NSPECVAR = SIZE(SPECVARS)
                write(*,'(//,''**Warning** the number of species exceed maximum'')')
                write(*,'(''  Only the first'',i4,'' species will be processed'')') NSPECVAR
              endif                

             ! copy all species
             do n=1,NVARS3D
               SPECVARS(n) = VNAME3D(n)
             enddo 

             EXIT !using all species
           endif
      
           NSPECVAR = NSPECVAR + 1
           SPECVARS( NSPECVAR ) = field

          enddo
 
         end Subroutine LoadSpecies
 
      END MODULE SPECIES

