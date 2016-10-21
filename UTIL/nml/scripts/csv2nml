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
          echo "  error determining mechanism category"
          exit
       endif
    endif
 endif

 pwd
 echo "   Converting $File"

#> surrogate type 1
 @ count = 0
 if ( `grep -oic EMIS $File` ) @ count++
 if ( `grep -oic ICBC $File` ) @ count++
 if ( `grep -oic DEPV $File` ) @ count++
 if ( `grep -oic SCAV $File` ) @ count++
 set n_surr1 = $count

#> surrogate type 2
 @ count = 0
#if ( `grep -oic ${X2}2AE $File` ) @ count++
#if ( `grep -oic ${X2}2AQ $File` ) @ count++
 if ( `grep -oic      2AE $File` ) @ count++
 if ( `grep -oic      2AQ $File` ) @ count++
 set n_surr2 = $count

#> control type
 @ count = 0
 if ( `grep -oic  ADV $File` ) @ count++
 if ( `grep -oic DIFF $File` ) @ count++
 if ( `grep -oic TRNS $File` ) @ count++
 if ( `grep -oic DDEP $File` ) @ count++
 if ( `grep -oic WDEP $File` ) @ count++
 if ( `grep -oic CONC $File` ) @ count++
 set n_ctrl = $count

#> modify csv to a "colon separated values" file with main variables defined
#> in which each line is a character string
 sed \
     -e 's/\r//' \
     -e 's/\,/\:/g' \
     -e "s/^./\'&/" \
     -e 's/$/&#/' \
     -e "s/#/\'\,/" \
     -e '$ s/\,$//' \
     -e '1 i TYPE_HEADER =' \
     -e '2 i TYPE_MATRIX =' \
     $File > ! /tmp/${Name}_$$

#> add eof to namelist
 echo "/" >> /tmp/${Name}_$$

#> create top part of namelist
 echo "&${Cat}_nml"           > /tmp/nml_$$
 echo " "                    >> /tmp/nml_$$
 echo "n_surr1 = $n_surr1,"  >> /tmp/nml_$$
 echo "n_surr2 = $n_surr2,"  >> /tmp/nml_$$
 echo "n_ctrl = $n_ctrl,"    >> /tmp/nml_$$
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
#>     -e 's/\r//'            <- remove any carriage returns ("cr" or "^M")
#>     -e 's/\,/\:/g'         <- change "," to ":" everywhere
#>     -e "s/^./\'&/"         <- insert "'" before first char in the line
#>                               ("&" stores the pattern found)
#>     -e 's/$/&#/'           <- append "#" to last char in the line
#>     -e "s/#/\'\,/"         <- change "#" to "',"
#>     -e '$ s/\,$//'         <- go to last line and delete the "," after the last char
#>                               in the line
#>                               ( "$" is a special address, representing the last line)
#>     -e '1 i TYPE_HEADER =' <- insert "TYPE_HEADER =" at line 1
#>     -e '2 i TYPE_MATRIX =' <- insert "TYPE_MATRIX =" at line 2
