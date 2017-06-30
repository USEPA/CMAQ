#! /bin/csh -f

# type fmm -h for help
set exist_h = ` echo $argv | grep -e "-h" | wc -w `

set temp_time = `date`
setenv ctime $temp_time[4]

if ($exist_h != 0) then
   goto usage
else
   goto checkarg
endif

cont:

set nfile = 1

setenv ofile $oname

./dpp.x

exit

#---------------------------------------------------------------------
checkarg:

set count = $#argv

#set default values
setenv spc N

      @ stop = 1
      @ lc = 0

      while ($lc < $count)
        @ lc++

        if ("$argv[$lc]" == '-cw') then
           @ lc++
           set argument = `echo $argv[$lc]`
           setenv wrf_lc_ref_lat $argument[1]
           setenv convert 2
        else if ("$argv[$lc]" == '-s') then
           @ lc++
           set argument = `echo $argv[$lc]`
           setenv spc Y
           setenv spc_list "$argument"
        else if ("$argv[$lc]" == '-o') then
           @ lc++
           set argument = `echo $argv[$lc]`
           setenv template $argument[1]
           setenv convert 1
        else
           set len = `echo $argv[$lc] | wc -c`
           set found = `echo $argv[$lc] | grep "-" | wc -l`
           if (($found) && ($len <= 3)) then
              echo ' '
              echo " Error: Invalid option $argv[$lc]"
              echo ' '
              exit
           else
              @ remaining = 1 + $count - $lc
              if ($remaining == 2) then
                 set infile = $argv[$lc]
                 if (-f $infile) then
                    setenv infile1 $infile
                 endif
                 @ lc++
                 set oname = $argv[$lc]
              else
                 echo ' Invalid number of arguments '
                 exit
              endif
           endif
           setenv num_file 1
        endif

      end

  if (`ncdump -k $infile | grep '64-bit'  | wc -l`) then
     setenv ncd_64bit_offset .true.
  else
     setenv ncd_64bit_offset .false.
  endif

goto cont

# -------------------------------------------------------------------------
usage:
echo ' '
echo 'dpp [ -h ] [ -o "cmaq_file" ] [ -cw "reference_lat" ] '
echo '    [ -s "SPC_LIST" ] input_file output_file '
echo ' '
echo '    where -o     -- convert wrf data to ioapi_3 format w.r.t to the'
echo '                    domain defines in cmaq_file '
echo '          -cw    -- convert wrf file into IOAPI_3 format without any '
echo '                    existing IOAPI_3 file '
echo '          -s     -- specify a species list'
echo '          -h     -- display usage information'
echo ' '
echo '    e.g. (for wrf data)'
echo '         dpp -cw "40" -s "MODIS_FPAR_T MODIS_LAI_T" DATA_FILE  outfile'
echo ' '
echo '         This will produce data subset of WRF DATA_FILE to IOAPI_3 format outfile'
echo ' '
echo ' If you have any comment/question/problem using this script/program, '
echo ' please contact David Wong at wong.david-c@epa.gov'
echo ' '

exit
