#! /bin/csh -f

# RCS file, release, date & time of last delta, author, state, [and locker]
# $Header$

#> csv2nml - csh script to create a namelist (.nml) from a csv (.csv)
#set echo

 if ( $#argv < 1 ) then
    echo "   usage: cvs2nml <file.csv>"
    exit
 endif

 set File = $argv[1]
 set Tail = $File:t
 set Name = $Tail:r

#> determine which species category
 @ Ndx = `echo $Name | awk 'P=index($0,"GC") {print(P)}'`
#echo $Ndx
 if ( $Ndx != 0 ) then
    set Cat = GC
#   echo $Cat
 else
    @ Ndx = `echo $Name | awk 'P=index($0,"AE") {print(P)}'`
#   echo $Ndx
    if ( $Ndx != 0 ) then
       set Cat = AE
#      echo $Cat
    else
       @ Ndx = `echo $Name | awk 'P=index($0,"NR") {print(P)}'`
#      echo $Ndx
       if ( $Ndx != 0 ) then
          set Cat = NR
#         echo $Cat
       else
          @ Ndx = `echo $Name | awk 'P=index($0,"TR") {print(P)}'`
#         echo $Ndx
          if ( $Ndx != 0 ) then
             set Cat = TR
#            echo $Cat
          else
            echo "  error determining mechanism category"
            exit
          endif
       endif
    endif
 endif

 pwd
 echo "   Converting $File"



#> modify csv to a "colon separated values" file with main variables defined
#> in which each line is a character string
 sed \
     -e 's/\r//' \
     -e '1s/^/\!/' \
     -e "1\!s/\s//g" \
     -e "1\!s/[A-Za-z0-9]*[[-]]*[A-Za-z]\w*\|[A-Za-z]\w*\|[A-Za-z0-9]*[[_]]*[A-Za-z]\w*/'&\'/g" \
     -e '1\!s/^\,//' \
     -e '1\!s/$/\,/' \
     -e "1\!s/\,\,/\,\'\'\,/g" \
     -e "1\!s/\,\,/\,\'\'\,/g" \
     -e '$ s/\,$//' \
     $File > ! /tmp/${Name}_$$

#> add eof to namelist
 echo "/" >> /tmp/${Name}_$$

#> create top part of namelist
 echo "&${Cat}_nml"           > /tmp/nml_$$
 echo " "                    >> /tmp/nml_$$
 echo "${Cat}_Species_Data =" >> /tmp/nml_$$
echo " "                    >> /tmp/nml_$$

#> insert top part of namelist
 cat /tmp/nml_$$ /tmp/${Name}_$$ > $Name.mod

 if ( -e $Name.nml ) then
    echo "don't overwrite $Name.nml"
    exit 1
 endif
 /bin/mv $Name.mod $Name.nml

 /bin/rm /tmp/nml_$$ /tmp/${Name}_$$

 exit

#> description
#> sed \
#>     -e 's/\r//'                       <- remove any carriage returns ("cr" or "^M")
#>     -e '1s/^/\!/'                     <- Insert Comment Symbol before Matrix Header to 
#>                                          ensure it does not get read 
#>     -e "1\!s/\s//g"                   <- Remove any spaces present in the list 
#>     -e "1\!s/[A-Za-z0-9]*[[-]]        <- Look for patterns that has words and put and
#>         *[A-Za-z]\w*\|[A-Za-z]\w*\       single quote around them 
#>         |[A-Za-z0-9]*[[_]]*[A-Za-z]
#>         \w*/'&\'/g" \    
#>                                          single quote around them  
#>     -e '1\!s/^\,//'                   <- Delete a comma from start of each line 
#>                                          if present      
#>     -e '1\!s/$/\,/'                   <- Add a comma to the end of each line to signify 
#>                                          line break
#>     -e "1\!s/\,\,/\,\'\'\,/g"         <- Add quotes to any fields that are left blank and have
#>                                          2 commas surronding them 
#>     -e "1\!s/\,\,/\,\'\'\,/g"         <- Add quotes to any fileds that are left blank and have 
#>                                          3 commas surrounding them
#>     -e '$ s/\,$//'                    <- go to last line and delete the "," after the last char
#>                                          in the line
#>                                          ( "$" is a special address, representing the last line)


