
#include <stdio.h>
#include <stdlib.h>

#define BUFLEN 512

#if ! defined(FSTR_L)
#define    FSTR_L       int
#endif              /** IF "FC" uses "int" string-length arguments **/

#ifdef FLDMN
#define NAMEVAL nameval_
#endif

void NAMEVAL ( char * lname,
               char * eqname,
               FSTR_L llen,
               FSTR_L elen )
    {
    char   buffer[ BUFLEN ] ;
    char  *source, *target , *bound, *limit, ch ;
    FSTR_L length ;

        /** Copy lname into buffer[] as a c-string **/
        /** Trim trailing blanks, and add terminal null **/

    source = lname ;
    length = llen ;
    target = buffer ;
    bound  = target + ( length < BUFLEN-1 ? length : BUFLEN-1 ) ;
    for ( ; target < bound ; target++ , source++ )
        {
        ch = *source ;
        if ( ch == ' ' )
            {
            break ;
            } /** end if-clause: blank character **/
        else *target = ch ;

        } /** end for-loop copying lname to buffer[], etc.  **/

        *target = '\0' ;


        /** Copy value from environment into contents of eqname **/

    target = eqname ;
    bound  = target + elen ;
    if ( source = getenv( buffer ) )
        {
        for (   ; *target = *source ; target++ , source++ )
            {
            if ( target >= bound ) break ;
            }   /** end for-loop copying value from environment to eqname **/

        } /** end if-clause: getenv() succeeded**/
    else
        {
        source = lname ;
        limit  = target + llen ;
        limit  = ( limit < bound ? limit : bound ) ;
        for (  ; target < limit ; *target++ = *source++ )  ; /** empty body **/

        } /** end else-clause: getenv() failed **/

        /** pad Fortran output string with trailing blanks:  **/

    for (   ; target < bound ; target++ )  *target = ' ' ;

    } /** end void function nameval() **/

