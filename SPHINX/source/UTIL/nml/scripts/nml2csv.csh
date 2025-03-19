#! /bin/csh -f

# RCS file, release, date & time of last delta, author, state, [and locker]
# $Header$

#> nml2csv - csh script to revert a sednml Namelist file back to a .csv
#> requirements: the nml Namelist file must be in the subdir; i.e. this script
#> does not support a file path.

#set echo

 if ( $#argv == 0 ) then
    echo "   usage: nml2csv <file.nml>"
    exit
 endif

 set File = $argv[1]
 set Name = $File:r

 if ( -e ${Name}.csv ) then
    echo "   ${Name}.csv exists.  Not overwriting"
    exit
 endif

 pwd
 echo "   Converting $File"

#> conversion
 sed \
     -e '1,4d' \
     -e 's/'^!'//' \
     -e "1\!s/\s//g"\
     -e '/\//,/$/d' \
     -e "s/'//g" \
     -e 's/,$//' \
     $File > ! /tmp/${Name}_$$

 /bin/mv /tmp/${Name}_$$ $Name.csv

 exit

#> description
#> sed \
#>     -e '1,4d'               <- delete the first 4 lines of namelist file
#>     -e 's/'^!'//'           <- delte the comment symbole (!)
#>     -e "1\!s/\s//g"         <- delete all spaces existing in the file
#>     -e '/\//,/$/d'          <- delete the last lines (starting at "/")
#>     -e "s/'//g"             <- remove all "'"s
#>     -e 's/,$//'             <- remove last "," in all lines
