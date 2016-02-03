!*******************************************************************************
!$RCSfile: files_inc.f90,v $
!$Revision: 1.5 $
!$Date: 2010/10/28 20:13:41 $
!*******************************************************************************
module files_inc
!
!  Updated Oct 2005 to calculate and store dry and wet dep separately, PK, AER
!  May 2006: Allow separate output files for restarted run instead of
!            cumulative files (PK, AER)
!  Updated Jan 2007 to add lun_amr, arrange names alphabetically and lWrap for
!                   setting sampler output format - BC  

save

!------ SCICHEM paths, files, and unit numbers

!------ path names

character*128 path_app  ! application path
character*128 path_inv  ! HASCAL inventory path
character*128 path_map  ! DCW map path
character*128 path_2Dc  ! 2D Climo weather path
character*128 path_3Dc  ! 3D Climo weather path
character*128 path_prm  ! permanent data path
character*128 path_tmp  ! working path
character*128 path_usr  ! user path
character*128 path_rst  ! restart file path

!------ file names (sorted alphabetically)
character*128 file_amb  ! Ambient data file if nxyz > 1
character*128 file_ATP  ! ATP45 Output file
character*128 file_dat  ! HASCAL DOSCAL.DAT permanent data
character*128 file_dbg  ! debug output
character*128 file_ddp  ! dry deposition output
character*128 file_def  ! RAS2SCI input
character*128 file_dep  ! surface deposition output
character*128 file_dgn  ! Diagnostics file
character*128 file_dmp  ! dump output
character*128 file_dos  ! surface dose output
character*128 file_err  ! error output
character*128 file_icn  ! SCICHEM operation incident file
character*128 file_imc  ! SCICHEM multi-component input
character*128 file_ind  ! HASCAL INDEXR.DAT permanent data file
character*128 file_inp  ! SCICHEM namelist input
character*128 file_log  ! SCICHEM log file output
character*128 file_mcw  ! mass-consistent wind field output
character*128 file_met  ! met profile input
character*128 file_msc  ! met scenario input
character*128 file_pal  ! GUI palette input
character*128 file_prj  ! project output
character*128 file_puf  ! puff output
character*128 file_rad  ! FACCAL output
character*128 file_rst  ! Restart project name
character*128 file_sbl  ! boundary layer input
character*128 file_scn  ! release scenario input
character*128 file_sfc  ! met surface input or RAS2SCI output
character*128 file_smp  ! sampler time history output
character*128 file_src  ! HASCAL SRCOUT.TMP output
character*128 file_ter  ! terrain description output
character*128 file_tmp  ! temporary/local file name
character*128 file_usr  ! HASCAL USERDATA.TMP output
character*128 file_wdp  ! wet deposition output

! Files for restarted run
character*128 file_ddpr ! dry deposition file
character*128 file_dgnr ! diagnostics file
character*128 file_dmpr ! dump file
character*128 file_prjr ! project file
character*128 file_pufr ! puff file
character*128 file_wdpr ! wet deposition file

!------ file unit numbers (sorted alphabetically)
integer lun_amb  
integer lun_amr
integer lun_ATP  
integer lun_dat  
integer lun_dbg  
integer lun_ddp  
integer lun_def  
integer lun_dep  
integer lun_dgn  
integer lun_dmp  
integer lun_dos  
integer lun_err  
integer lun_icn  
integer lun_imc  
integer lun_ind  
integer lun_inp  
integer lun_log  
integer lun_mcw  
integer lun_met  
integer lun_msc  
integer lun_pal  
integer lun_prj  
integer lun_puf  
integer lun_rad  
integer lun_rst  
integer lun_sbl  
integer lun_scn  
integer lun_sfc  
integer lun_smp  
integer lun_src  
integer lun_ter  
integer lun_tmp  
integer lun_usr  
integer lun_wdp

logical lWrap

end module files_inc
