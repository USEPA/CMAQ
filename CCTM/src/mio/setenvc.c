#include <stdlib.h>
#include <string.h>

#if FLDMN

#define SETENV  setenvvar_

#elif defined(__hpux) || defined(_AIX)

#define SETENV  setenvvar

#endif

void SETENV(str, strl)
char *str;
long strl;
{
        void *malloc();
        char *putstr = malloc(strl+1);
        strncpy(putstr, str, strl);
        putstr[strl] = 0;
        putenv(putstr); 
}
