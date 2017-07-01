;##############################################################################
; Interpolates monthly MODIS FPAR and LAI data into a daily file
; The 10-year averaged MODIS data (2001-2010) will be labeled as 2005
; input  : geo_em.d01.nc
; output : MODIS_FPAR_LAI_daily.nc
; P.O.C. : Peng Liu (liu.peng@epa.gov) and Hosein Foroutan (foroutan.hosein@epa.gov)
;##############################################################################

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

;read in the input file
    dir="/my/work/directory"                                  ; working dir
    file_monthly2005=addfile(dir+"geo_em.d01.nc","r")         ; input file (from geogrid.exe)
    size_dim=getfiledimsizes(file_monthly2005)
    name_dim=getvardims(file_monthly2005)
    n=dimsizes(size_dim)
    do i=0,n-1,1
      if (name_dim(i) .eq. "south_north") then
          nsouth_north=size_dim(i)
      end if
      if (name_dim(i) .eq. "west_east") then
          nwest_east=size_dim(i)
      end if
      if (name_dim(i) .eq. "DateStrLen") then
          nDateStrLen=size_dim(i)
      end if
    end do
    delete(size_dim)
    delete(name_dim)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;define output file file_daily2005
    name_dim_out=(/"Time","DateStrLen","south_north","west_east"/)
    size_dim_out=(/365,nDateStrLen,nsouth_north,nwest_east/)           ; need to be modified for a leap year
    dim_unlim_out=(/True,False,False,False/)
    file_daily2005 = addfile(dir+"MODIS_FPAR_LAI_daily.nc","cw")       ; output file
    filedimdef(file_daily2005,name_dim_out,size_dim_out,dim_unlim_out)

;define time related variables in file_daily2005
    filevardef(file_daily2005,"Times","character",(/"Time","DateStrLen"/))
    file_daily2005->Times@description = "Time"
    file_daily2005->Times@units = "yyyy-mm-dd_hh:mm:ss"
    filevardef(file_daily2005,"Times_julian","float",(/"Time"/))
    file_daily2005->Times_julian@description = "Time in Julian days for year 2005"
    file_daily2005->Times_julian@units = "Day"

;define other variables in file_daily2005
    vab_monthly2005=getfilevarnames(file_monthly2005)
    nvab_monthly2005=dimsizes(vab_monthly2005)
    do n=0,nvab_monthly2005-1,1
      replaceSingleChar(vab_monthly2005(n), "/", "")
      if ((vab_monthly2005(n) .eq. "XLONG_M").or. (vab_monthly2005(n) .eq. "XLAT_M")) then
         vab_type=getfilevartypes(file_monthly2005,vab_monthly2005(n))
         filevardef(file_daily2005,vab_monthly2005(n),vab_type,(/"south_north","west_east"/))
         delete(vab_type)
         vartemp=file_monthly2005->$vab_monthly2005(n)$
         attlist=getvaratts(vartemp)
         nattlist=dimsizes(attlist)
         do att=0,nattlist-1,1
             file_daily2005->$vab_monthly2005(n)$@$attlist(att)$=file_monthly2005->$vab_monthly2005(n)$@$attlist(att)$
         end do 
         delete(vartemp)
         delete(attlist) 
      else if ((vab_monthly2005(n) .eq. "GREENFRAC") .or. (vab_monthly2005(n) .EQ. "LAI12M")) then
         if (vab_monthly2005(n) .eq. "GREENFRAC") then
           vabtemp="MODIS_FPAR_T"
         else
           vabtemp="MODIS_LAI_T"
         end if
         vab_type=getfilevartypes(file_monthly2005,vab_monthly2005(n))
         filevardef(file_daily2005,vabtemp,vab_type,(/"Time","south_north","west_east"/))
         vabtemp2=file_monthly2005->$vab_monthly2005(n)$
         attlist=getvaratts(vabtemp2)
         nattlist=dimsizes(attlist)
         do att=0,nattlist-1,1
             file_daily2005->$vabtemp$@$attlist(att)$=file_monthly2005->$vab_monthly2005(n)$@$attlist(att)$
         end do
         file_daily2005->$vabtemp$@_FillValue = -999. 
         if (vab_monthly2005(n) .eq. "GREENFRAC") then
            file_daily2005->$vabtemp$@description="MODIS FPAR averaged from 2001 to 2010 from WPS geog"
            file_daily2005->$vabtemp$@units="fraction"
         else
            file_daily2005->$vabtemp$@description="MODIS LAI averaged from 2001 to 2010 from WPS geog"
         end if
         delete(vabtemp)
         delete(vabtemp2)
         delete(vab_type)
         delete(attlist)
      end if
      end if
    end do ;nvab_monthly2005
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;get monthly data from file_monthly2005 and interpolate to daily
    fpar_monthly=file_monthly2005->GREENFRAC(0,:,:,:)
    lai_monthly=file_monthly2005->LAI12M(0,:,:,:)
    xlong_m=file_monthly2005->XLONG_M(0,:,:)
    xlat_m=file_monthly2005->XLAT_M(0,:,:)

;array to record daily data (need to be modified for a leap year)
    fpar_daily=new((/365,nsouth_north,nwest_east/),float,"No_FillValue")
    lai_daily=new((/365,nsouth_north,nwest_east/),float,"No_FillValue")
    timejulian=new(365,float,"No_FillValue")
    timeschar=new((/365,nDateStrLen/),character,"No_FillValue")

;to interpolate
    year="2005"
    yearfloat=stringtofloat(year)*1000
    daylist=(/31,28,31,30,31,30,31,31,30,31,30,31/) ;days of each month (need to be modified for a leap year)
    monthlist=(/"01","02","03","04","05","06","07","08","09","10","11","12"/)
    checkdaylist=daylist

;get the date at 15th of each month
    checkdaylist(0)=15
    do mon=1,12-1,1
     checkdaylist(mon)=0
     checkdaylist(mon)=sum(daylist(0:mon-1))+15
    end do
    currentday=1
    do mon=1,12,1
      do day=1,daylist(mon-1),1

;determine the month before and after currentday according to checkdaylist
         if (currentday .le. checkdaylist(mon-1)) then
             if (mon .ne.1) then
              mon2=mon
              mon1=mon-1
              checkday2=checkdaylist(mon2-1)
              checkday1=checkdaylist(mon1-1)
             else
              mon2=mon
              mon1=12
              checkday2=checkdaylist(mon2-1)
              checkday1=15-31
             end if
         end if
         if (currentday .gt. checkdaylist(mon-1)) then
             if (mon .ne. 12) then
              mon2=mon+1
              mon1=mon
              checkday2=checkdaylist(mon2-1)
              checkday1=checkdaylist(mon1-1)
             else
              mon2=1
              mon1=mon
              checkday2=365+15
              checkday1=checkdaylist(mon1-1)
             end if
         end if
;        print("processing day "+currentday)
;        print("checkday1 2 = "+checkday1+"  "+checkday2)

;do interpolation for FPAR and LAI
         fpar_daily(currentday-1,:,:) = (fpar_monthly(mon2-1,:,:)*(currentday-checkday1) \
                                      + fpar_monthly(mon1-1,:,:)*(checkday2-currentday)) \
                                      /(checkday2-checkday1)
         lai_daily(currentday-1,:,:) = (lai_monthly(mon2-1,:,:)*(currentday-checkday1) \
                                     + lai_monthly(mon1-1,:,:)*(checkday2-currentday)) \
                                     /(checkday2-checkday1)

;get time series and julia date
         timejulian(currentday-1)=yearfloat+currentday
         daystring=sprinti("%0.2i",day)
         timestring_temp=year+"-"+monthlist(mon-1)+"-"+daystring+"_00:00:00"
         timechar_temp=stringtochar(timestring_temp)
         timeschar(currentday-1,:)=timechar_temp(0:nDateStrLen-1)
         delete(daystring)
         delete(timestring_temp)
         delete(timechar_temp)
         currentday=currentday+1
     end do
   end do 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;write vab to file_daily2005 and modify some attributes based on file_monthly2005
    file_daily2005->MODIS_FPAR_T=(/fpar_daily/)
    file_daily2005->MODIS_LAI_T=(/lai_daily/)
    file_daily2005->Times=(/timeschar/)
    file_daily2005->Times_julian=(/timejulian/)
    file_daily2005->XLAT_M=(/xlat_m/)
    file_daily2005->XLONG_M=(/xlong_m/)

;copy the global attribute from file_monthly2005 to file_daily2005
    attlist=getvaratts(file_monthly2005)
    nattlist=dimsizes(attlist)
    do att=0,nattlist-1,1
       if (attlist(att) .eq. "SIMULATION_START_DATE") then
          file_daily2005@$attlist(att)$="200501010000" 
       else   
          file_daily2005@$attlist(att)$=file_monthly2005@$attlist(att)$
       end if
    end do
    delete(attlist)
end
