#! /bin/csh -f

 set echo

# create a directory for the symbolic linked files
 
 if ( ! -d "symlinked" ) mkdir -p symlinked
 cd symlinked

# symbolically link all scichem source file to one directory for 
# compatibility with the CMAQ bldmake utility

 set src = ../aero

 set noglob

 foreach file ( `find ${src}/ -type f -name "*" -print` )
      if ( ! -e $file:t ) ln -s $file || exit 1
 end

 set src = ../aero/inc

 set noglob

 foreach file ( `find ${src}/ -type f -name "*" -print` )
      if ( ! -e $file:t ) ln -s $file || exit 1
 end

 set src = ../aqueous

 set noglob

 foreach file ( `find ${src}/ -type f -name "*" -print` )
      if ( ! -e $file:t ) ln -s $file || exit 1
 end

 set src = ../aqueous/inc

 set noglob

 foreach file ( `find ${src}/ -type f -name "*" -print` )
      if ( ! -e $file:t ) ln -s $file || exit 1
 end

 set src = ../pig

 set noglob

 foreach file ( `find ${src}/ -type f -name "*" -print` )
      if ( ! -e $file:t ) ln -s $file || exit 1
 end

 set src = ../pig/host

 set noglob

 foreach file ( `find ${src}/ -type f -name "*" -print` )
      if ( ! -e $file:t ) ln -s $file || exit 1
 end

 set src = ../pig/inc

 set noglob

 foreach file ( `find ${src}/ -type f -name "*" -print` )
      if ( ! -e $file:t ) ln -s $file || exit 1
 end

 set src = ../scipuff

 set noglob

 foreach file ( `find ${src}/ -type f -name "*" -print` )
      if ( ! -e $file:t ) ln -s $file || exit 1
 end

 set src = ../scipuff/inc

 set noglob

 foreach file ( `find ${src}/ -type f -name "*" -print` )
      if ( ! -e $file:t ) ln -s $file || exit 1
 end

 set src = ../stubpig

 set noglob

 foreach file ( `find ${src}/ -type f -name "*" -print` )
      if ( ! -e $file:t ) ln -s $file || exit 1
 end

 set src = ../stubs

 set noglob

 foreach file ( `find ${src}/ -type f -name "*" -print` )
      if ( ! -e $file:t ) ln -s $file || exit 1
 end

 exit
