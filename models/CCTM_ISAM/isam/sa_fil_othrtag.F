
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

      SUBROUTINE SA_FIL_OTHRTAG( ISAMEM, JDATE, JTIME ) 

C*************************************************************
C20140428 Subtracts total tagged emission ISAMEM(non-othrtag) 
C         from total emission VDEMIS.
C         Then fill up ISAMEM(othrtag) with this difference.
C
C         Called by vdiffacm2.F
C*************************************************************

      USE HGRD_DEFN        ! 20120712 just for diagnosis with single mype
      USE EMIS_DEFN        ! fini_gc, strt_gc, etc, vdemis defined      
      USE SA_DEFN          ! Mc06
      USE CGRID_SPCS       ! 20120712 in place of gc_emis, ae_emis, nr_emis, tr_emis, etc
      USE UTILIO_DEFN      ! 20120712 in place of ioparms, iodecl
      USE SUBST_MODULES    ! stenex
      USE OZVOC_DEFN       ! 20120918  for VOC

      IMPLICIT NONE

Ckrt....Arguments
      
      REAL, POINTER :: ISAMEM( :,:,:,:,: )
      INTEGER, INTENT( IN ) :: JDATE, JTIME

Ckrt....Local variables
      INTEGER  L, R, C, N, V, S
      INTEGER  JSPC, ITAG, JSPCTAG, IBGN
      INTEGER  NVARI
      REAL     KNTAG

Ckrt...Dealing with NOX...
      INTEGER  JNO, JNO2, JHONO

Ckrt...20120918 dealing with VOC
      INTEGER JV
      REAL    VDEMIS_VOC

Ckrt...Error handling
      CHARACTER( 120 ) :: XMSG
      CHARACTER( 16 ) :: PNAME = 'SA_FIL_OTHRTAG  '

C==========================================================


      DO L = 1, EMLAYS
       DO R = 1, MY_NROWS
        DO C = 1, MY_NCOLS
         DO JSPC = 1, NSPC_SA
          KNTAG = 0.0
          DO ITAG = 1, NTAG_SA - 3
           KNTAG = KNTAG + ISAMEM( C,R,L,JSPC,ITAG )
          ENDDO ! itag

Ckrt...Search from Gas emis species list
          NVARI = LAST_GC - FRST_GC + 1
          N = INDEX1( SPC_NAME(JSPC,OTHRTAG),NVARI,GC_EMIS )
          IF ( N .GT. 0 ) THEN
           V = N
           ISAMEM(C,R,L,JSPC,OTHRTAG) =
     &       VDEMIS( V,L,C,R ) - KNTAG
           IF ( ISAMEM(C,R,L,JSPC,OTHRTAG) .LT. 0. ) THEN
             IF ( ABS(ISAMEM(C,R,L,JSPC,OTHRTAG)) .LE. 1.E-10 ) THEN
               ISAMEM(C,R,L,JSPC,OTHRTAG) = 0.
             ELSE
               IF ( YES_PRINT ) THEN
                 print*,'mype,L,R,C,JSPC,spc_nam,vdemis,kntag:'
                 print*, MYPE,L,R,C,JSPC,SPC_NAME(JSPC,OTHRTAG),
     &                   VDEMIS(V,L,C,R),KNTAG
               END IF !optional yes print
               XMSG = 'Total tagging GC emiss exceeds basic emiss !'
               CALL M3EXIT( PNAME,JDATE,JTIME,XMSG,3 )
             ENDIF ! negative number very small ?
           ENDIF ! indeed a negative number ?
!KRT20120920          ELSEIF ( SPC_NAME(JSPC,OTHRTAG) .EQ. 'O3N' .OR.
!KRT20120920     &            SPC_NAME(JSPC,OTHRTAG) .EQ. 'O3V' ) THEN
!KRT20120920           V = INDEX1( 'O3',NVARI,GC_EMIS )
!KRT20120920           ISAMEM(C,R,L,JSPC,OTHRTAG) =
!KRT20120920     &       VDEMIS( V,L,C,R ) - KNTAG
!KRT20120920           IF ( ISAMEM(C,R,L,JSPC,OTHRTAG) .LT. 0. ) THEN
!KRT20120920             IF ( ABS(ISAMEM(C,R,L,JSPC,OTHRTAG)) .LE. 1.E-10 ) THEN
!KRT20120920               ISAMEM(C,R,L,JSPC,OTHRTAG) = 0.
!KRT20120920             ELSE
!KRT20120920               XMSG = 'Total tagging GC emiss exceeds basic emiss !'
!KRT20120920               CALL M3EXIT( PNAME,JDATE,JTIME,XMSG,3 )
!KRT20120920             ENDIF ! negative number very small ?
!KRT20120920           ENDIF ! indeed a negative number ?
          ENDIF ! species matched

Ckrt...Search from PM emis species list
          NVARI = LAST_AE - FRST_AE + 1
          N = INDEX1( SPC_NAME(JSPC,OTHRTAG),NVARI,AE_EMIS )
          IF ( N .GT. 0 ) THEN
           V = N + FRST_AE - 1
           ISAMEM(C,R,L,JSPC,OTHRTAG) =
     &       VDEMIS( V,L,C,R ) - KNTAG
           IF ( ISAMEM(C,R,L,JSPC,OTHRTAG) .LT. 0. ) THEN
             IF ( ABS(ISAMEM(C,R,L,JSPC,OTHRTAG)) .LE. 1.E-10 ) THEN
               ISAMEM(C,R,L,JSPC,OTHRTAG) = 0.
             ELSE
               print*,'SPC_NAME,C,R,L,VDEMIS,KNTAG'
               print*, SPC_NAME(JSPC,OTHRTAG),C,R,L,VDEMIS(V,L,C,R),KNTAG 
               XMSG = 'Total tagging AE emiss exceeds basic emiss !'
               CALL M3EXIT( PNAME,JDATE,JTIME,XMSG,3 )
             ENDIF ! negative number very small ?
           ENDIF ! indeed a negative number ?
          ENDIF ! species matched

Ckrt...Search from NR emis species list
          NVARI = LAST_NR - FRST_NR + 1
          N = INDEX1( SPC_NAME(JSPC,OTHRTAG),NVARI,NR_EMIS )
          IF ( N .GT. 0 ) THEN
           V = N + FRST_NR - 1
           ISAMEM(C,R,L,JSPC,OTHRTAG) =
     &       VDEMIS( V,L,C,R ) - KNTAG
           IF ( ISAMEM(C,R,L,JSPC,OTHRTAG) .LT. 0 ) THEN
             IF ( ABS(ISAMEM(C,R,L,JSPC,OTHRTAG)) .LE. 1.E-10 ) THEN
               ISAMEM(C,R,L,JSPC,OTHRTAG) = 0.
             ELSE
               XMSG = 'Total tagging NR emiss exceeds basic emiss !'
               CALL M3EXIT( PNAME,JDATE,JTIME,XMSG,3 )
             ENDIF ! negative number very small ?
           ENDIF ! indeed a negative number ?
          ENDIF ! species matched


         ENDDO ! jspc
        ENDDO ! C
       ENDDO ! R
      ENDDO ! L

      END SUBROUTINE SA_FIL_OTHRTAG
