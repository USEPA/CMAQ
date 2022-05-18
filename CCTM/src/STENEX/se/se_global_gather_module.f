
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

! what(1) key, module and SID; SCCS file; date and time of last delta:
! %W% %P% %G% %U%

! --------------------------------------------------------------------------
! Purpose:
!
!   use F90 interface feature to achieve "faked" polymorphism for global gather
!   operation for PE 0
!
! Revision history:
!
!   Orginal version: 12/14/21 by David Wong
!
! Parameter List:
!
!   In: var -- distributed variable which needs be determined the max value
!              among all processors
!
! Local Variable:
!
!   max   -- maximum value among all processors
!   error -- error code of mpi call
!
! Include Files:
!
!   mpif.h
C -----------------------------------------------------------------------------

        module se_global_gather_module

          use se_pe_info_ext, only : se_worker_comm, se_myworker_pe, se_numworkers
          use se_domain_info_ext, only : se_gl_ind

          implicit none

          integer :: sc, ec, sr, er, nc, nr
          logical :: called_setup = .false.

          interface se_global_gather
            module procedure se_global_gather_2d_r,
     $                       se_global_gather_3d_r,
     $                       se_global_gather_2d_i,
     $                       se_global_gather_3d_i,
     $                       se_global_gather_2d_l,
     $                       se_global_gather_3d_l
          end interface

          contains

! -----------------------------------------------------------------------------
          subroutine se_global_gather_setup

            sc = se_gl_ind (1,1,se_myworker_pe)
            ec = se_gl_ind (2,1,se_myworker_pe)

            sr = se_gl_ind (1,2,se_myworker_pe)
            er = se_gl_ind (2,2,se_myworker_pe)

            nc = ec - sc + 1
            nr = er - sr + 1

          end subroutine se_global_gather_setup

! -----------------------------------------------------------------------------
          subroutine se_global_gather_2d_r (indata, outdata)

            real, intent(in)  :: indata(:,:)
            real, intent(out) :: outdata(:,:)

            include "mpif.h"

            integer :: data_size, error, pe, lsc, lec, lsr, ler,
     $                 lnc, lnr, status(MPI_STATUS_SIZE)
            real, allocatable :: buf(:,:)

            if (.not. called_setup) then
               call se_global_gather_setup
               called_setup = .true.
            end if

            if (se_myworker_pe == 0) then

               outdata(sc:ec, sr:er) = indata

               do pe = 1, se_numworkers-1
                  lsc = se_gl_ind (1,1,pe)
                  lec = se_gl_ind (2,1,pe)

                  lsr = se_gl_ind (1,2,pe)
                  ler = se_gl_ind (2,2,pe)

                  lnc = lec - lsc + 1
                  lnr = ler - lsr + 1

                  data_size = lnc * lnr

                  allocate (buf(lnc, lnr), stat=error)

                  call mpi_recv (buf, data_size, mpi_real, pe, pe+100,
     &                          se_worker_comm, status, error)

                  if (error .ne. 0) then
                     write (6, *) ' Abort: Error occurred while performing global 2d'
                     write (6, *) '        real gather in receiving data from PE ', pe
                     stop
                  end if

                  outdata(lsc:lec, lsr:ler) = buf

                  deallocate (buf)
               end do
            else
               data_size = nc * nr

               call mpi_send (indata, data_size, mpi_real, 0,
     $                        se_myworker_pe+100, se_worker_comm, error)

               if (error .ne. 0) then
                  write (6, *) ' Abort: Error occurred while performing global 2d'
                  write (6, *) '        real gather in sending data from PE ', pe
                  stop
               end if
            end if

          end subroutine se_global_gather_2d_r

! -----------------------------------------------------------------------------
          subroutine se_global_gather_3d_r (indata, outdata)

            real, intent(in)  :: indata(:,:,:)
            real, intent(out) :: outdata(:,:,:)

            include "mpif.h"

            integer :: data_size, error, pe, lsc, lec, lsr, ler,
     $                 lnc, lnr, status(MPI_STATUS_SIZE)
            real, allocatable :: buf(:,:,:)

            if (.not. called_setup) then
               call se_global_gather_setup
               called_setup = .true.
            end if

            if (se_myworker_pe == 0) then

               outdata(sc:ec, sr:er, :) = indata

               do pe = 1, se_numworkers-1
                  lsc = se_gl_ind (1,1,pe)
                  lec = se_gl_ind (2,1,pe)

                  lsr = se_gl_ind (1,2,pe)
                  ler = se_gl_ind (2,2,pe)

                  lnc = lec - lsc + 1
                  lnr = ler - lsr + 1

                  data_size = lnc * lnr * size(indata,3)

                  allocate (buf(lnc, lnr, size(indata,3)), stat=error)

                  call mpi_recv (buf, data_size, mpi_real, pe, pe+100,
     &                          se_worker_comm, status, error)

                  if (error .ne. 0) then
                     write (6, *) ' Abort: Error occurred while performing global 3d'
                     write (6, *) '        real gather in receiving data from PE ', pe
                     stop
                  end if

                  outdata(lsc:lec, lsr:ler, :) = buf

                  deallocate (buf)
               end do
            else
               data_size = nc * nr * size(indata, 3)

               call mpi_send (indata, data_size, mpi_real, 0,
     $                        se_myworker_pe+100, se_worker_comm, error)

               if (error .ne. 0) then
                  write (6, *) ' Abort: Error occurred while performing global 3d'
                  write (6, *) '        real gather in sending data from PE ', pe
                  stop
               end if
            end if

          end subroutine se_global_gather_3d_r

! -----------------------------------------------------------------------------
          subroutine se_global_gather_2d_i (indata, outdata)

            integer, intent(in)  :: indata(:,:)
            integer, intent(out) :: outdata(:,:)

            include "mpif.h"

            integer :: data_size, error, pe, lsc, lec, lsr, ler,
     $                 lnc, lnr, status(MPI_STATUS_SIZE)
            integer, allocatable :: buf(:,:)

            if (.not. called_setup) then
               call se_global_gather_setup
               called_setup = .true.
            end if

            if (se_myworker_pe == 0) then

               outdata(sc:ec, sr:er) = indata

               do pe = 1, se_numworkers-1
                  lsc = se_gl_ind (1,1,pe)
                  lec = se_gl_ind (2,1,pe)

                  lsr = se_gl_ind (1,2,pe)
                  ler = se_gl_ind (2,2,pe)

                  lnc = lec - lsc + 1
                  lnr = ler - lsr + 1

                  data_size = lnc * lnr

                  allocate (buf(lnc, lnr), stat=error)

                  call mpi_recv (buf, data_size, mpi_int, pe, pe+100,
     &                          se_worker_comm, status, error)

                  if (error .ne. 0) then
                     write (6, *) ' Abort: Error occurred while performing global 2d'
                     write (6, *) '        integer gather in receiving data from PE ', pe
                     stop
                  end if

                  outdata(lsc:lec, lsr:ler) = buf

                  deallocate (buf)
               end do
            else
               data_size = nc * nr

               call mpi_send (indata, data_size, mpi_int, 0,
     $                        se_myworker_pe+100, se_worker_comm, error)

               if (error .ne. 0) then
                  write (6, *) ' Abort: Error occurred while performing global 2d'
                  write (6, *) '        integer gather in sending data from PE ', pe
                  stop
               end if
            end if

          end subroutine se_global_gather_2d_i

! -----------------------------------------------------------------------------
          subroutine se_global_gather_3d_i (indata, outdata)

            integer, intent(in)  :: indata(:,:,:)
            integer, intent(out) :: outdata(:,:,:)

            include "mpif.h"

            integer :: data_size, error, pe, lsc, lec, lsr, ler,
     $                 lnc, lnr, status(MPI_STATUS_SIZE)
            integer, allocatable :: buf(:,:,:)

            if (.not. called_setup) then
               call se_global_gather_setup
               called_setup = .true.
            end if

            if (se_myworker_pe == 0) then

               outdata(sc:ec, sr:er, :) = indata

               do pe = 1, se_numworkers-1
                  lsc = se_gl_ind (1,1,pe)
                  lec = se_gl_ind (2,1,pe)

                  lsr = se_gl_ind (1,2,pe)
                  ler = se_gl_ind (2,2,pe)

                  lnc = lec - lsc + 1
                  lnr = ler - lsr + 1

                  data_size = lnc * lnr * size(indata,3)

                  allocate (buf(lnc, lnr, size(indata,3)), stat=error)

                  call mpi_recv (buf, data_size, mpi_int, pe, pe+100,
     &                          se_worker_comm, status, error)

                  if (error .ne. 0) then
                     write (6, *) ' Abort: Error occurred while performing global 3d'
                     write (6, *) '        integer gather in receiving data from PE ', pe
                     stop
                  end if

                  outdata(lsc:lec, lsr:ler, :) = buf

                  deallocate (buf)
               end do
            else
               data_size = nc * nr * size(indata, 3)

               call mpi_send (indata, data_size, mpi_int, 0,
     $                        se_myworker_pe+100, se_worker_comm, error)

               if (error .ne. 0) then
                  write (6, *) ' Abort: Error occurred while performing global 3d'
                  write (6, *) '        integer gather in sending data from PE ', pe
                  stop
               end if
            end if

          end subroutine se_global_gather_3d_i

! -----------------------------------------------------------------------------
          subroutine se_global_gather_2d_l (indata, outdata)

            logical, intent(in)  :: indata(:,:)
            logical, intent(out) :: outdata(:,:)

            include "mpif.h"

            integer :: data_size, error, pe, lsc, lec, lsr, ler,
     $                 lnc, lnr, status(MPI_STATUS_SIZE)
            logical, allocatable :: buf(:,:)

            if (.not. called_setup) then
               call se_global_gather_setup
               called_setup = .true.
            end if

            if (se_myworker_pe == 0) then

               outdata(sc:ec, sr:er) = indata

               do pe = 1, se_numworkers-1
                  lsc = se_gl_ind (1,1,pe)
                  lec = se_gl_ind (2,1,pe)

                  lsr = se_gl_ind (1,2,pe)
                  ler = se_gl_ind (2,2,pe)

                  lnc = lec - lsc + 1
                  lnr = ler - lsr + 1

                  data_size = lnc * lnr

                  allocate (buf(lnc, lnr), stat=error)

                  call mpi_recv (buf, data_size, mpi_logical, pe, pe+100,
     &                          se_worker_comm, status, error)

                  if (error .ne. 0) then
                     write (6, *) ' Abort: Error occurred while performing global 2d'
                     write (6, *) '        logical gather in receiving data from PE ', pe
                     stop
                  end if

                  outdata(lsc:lec, lsr:ler) = buf

                  deallocate (buf)
               end do
            else
               data_size = nc * nr

               call mpi_send (indata, data_size, mpi_logical, 0,
     $                        se_myworker_pe+100, se_worker_comm, error)

               if (error .ne. 0) then
                  write (6, *) ' Abort: Error occurred while performing global 2d'
                  write (6, *) '        logical gather in sending data from PE ', pe
                  stop
               end if
            end if

          end subroutine se_global_gather_2d_l

! -----------------------------------------------------------------------------
          subroutine se_global_gather_3d_l (indata, outdata)

            logical, intent(in)  :: indata(:,:,:)
            logical, intent(out) :: outdata(:,:,:)

            include "mpif.h"

            integer :: data_size, error, pe, lsc, lec, lsr, ler,
     $                 lnc, lnr, status(MPI_STATUS_SIZE)
            logical, allocatable :: buf(:,:,:)

            if (.not. called_setup) then
               call se_global_gather_setup
               called_setup = .true.
            end if

            if (se_myworker_pe == 0) then

               outdata(sc:ec, sr:er, :) = indata

               do pe = 1, se_numworkers-1
                  lsc = se_gl_ind (1,1,pe)
                  lec = se_gl_ind (2,1,pe)

                  lsr = se_gl_ind (1,2,pe)
                  ler = se_gl_ind (2,2,pe)

                  lnc = lec - lsc + 1
                  lnr = ler - lsr + 1

                  data_size = lnc * lnr * size(indata,3)

                  allocate (buf(lnc, lnr, size(indata,3)), stat=error)

                  call mpi_recv (buf, data_size, mpi_logical, pe, pe+100,
     &                          se_worker_comm, status, error)

                  if (error .ne. 0) then
                     write (6, *) ' Abort: Error occurred while performing global 3d'
                     write (6, *) '        logical gather in receiving data from PE ', pe
                     stop
                  end if

                  outdata(lsc:lec, lsr:ler, :) = buf

                  deallocate (buf)
               end do
            else
               data_size = nc * nr * size(indata, 3)

               call mpi_send (indata, data_size, mpi_logical, 0,
     $                        se_myworker_pe+100, se_worker_comm, error)

               if (error .ne. 0) then
                  write (6, *) ' Abort: Error occurred while performing global 3d'
                  write (6, *) '        logical gather in sending data from PE ', pe
                  stop
               end if
            end if

          end subroutine se_global_gather_3d_l

        end module se_global_gather_module
