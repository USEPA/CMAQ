/***********************************************************************/
/*  Portions of Models-3/CMAQ software were developed or based on      */
/*  information from various groups: Federal Government employees,     */
/*  contractors working on a United States Government contract, and    */
/*  non-Federal sources (including research institutions).  These      */
/*  research institutions have given the Government permission to      */
/*  use, prepare derivative works, and distribute copies of their      */
/*  work in Models-3/CMAQ to the public and to permit others to do     */
/*  so.  EPA therefore grants similar permissions for use of the       */
/*  Models-3/CMAQ software, but users are requested to provide copies  */
/*  of derivative works to the Government without restrictions as to   */
/*  use by others.  Users are responsible for acquiring their own      */
/*  copies of commercial software associated with Models-3/CMAQ and    */
/*  for complying with vendor requirements.  Software copyrights by    */
/*  the MCNC Environmental Modeling Center are used with their         */
/*  permissions subject to the above restrictions.                     */
/***********************************************************************/

/* Distributes the CCTM script run time environment from the machine   */
/* that launched the script to the other participating machines        */

/* Revision History:                                                   */
/* Written by shanzhong zhu                                            */
/* Modified by David Wong, SAIC, Api 2003                              */
/* Modified 06/2021: Bugfix from Steve Fine, US EPA-OAR                */
/* Modified 06/2021: Fahim Sidi, US EPA, Enhanced portability to       */ 
/* other arch(s) that do NOT use Feldman-style Fortran bindings        */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "mpi.h"

#ifdef DEBUGGING
#define DEBUG(s) s
#else
#define DEBUG(s)
#endif

#ifdef FLDMN
#define distr_env distr_env_
#endif

extern char **environ;
#define TEMP_BUF_SIZE   102400
#define CURR_STR_SIZE   10240

extern void distr_env (int *myid_p, int *numprocs_p)
{
   char **environ_ptr;
   int env_size, total_size, total_size_0, str_size, avail_size;
   int myid, numprocs;
   char temp_buf[TEMP_BUF_SIZE], curr_str[CURR_STR_SIZE], *curr_ptr, *curr_name, *curr_val;
   int ret, i, error;
   
   myid = *myid_p;
   numprocs = *numprocs_p;
 
   if (myid == 0)
      { environ_ptr = environ;
        env_size = 0;
        total_size = 0;
        i = 0;
        while (environ_ptr[i++] != NULL)
          {   env_size++;
              total_size = total_size + strlen(environ_ptr[i-1]) + 1;
          }

        DEBUG( printf ("last of environment context is %s, total_size is %d. \n", 
                environ_ptr[env_size-1], total_size); )
 
        total_size_0 = total_size; 
        curr_ptr = temp_buf;
        avail_size = TEMP_BUF_SIZE;

        for (i=0; i<env_size; i++)
            { str_size = strlen(environ_ptr[i]);
              if ( (environ_ptr[i] != NULL)&&(avail_size > str_size) )
                 { strcpy (curr_ptr, environ_ptr[i]);
                   curr_ptr = curr_ptr + str_size + 1;
                   avail_size = avail_size - str_size - 1;
                 }
              else
                 {
                   printf ("your temp_buf in distr_env may not big enough to ");
                   printf ("hold next environmental pair \n");
                   exit (1);
                 }
            }

      }

   error = MPI_Bcast (&total_size_0, 1, MPI_INT, 0, MPI_COMM_WORLD);

   error = MPI_Bcast (temp_buf, total_size_0, MPI_CHAR, 0, MPI_COMM_WORLD); 

   if (myid != 0)
   {
      DEBUG( printf ("total_size_0 is: %d \n", total_size_0); )
 
      curr_ptr = temp_buf;
      while (curr_ptr < temp_buf+total_size_0)
      {
         if (strlen(curr_ptr) <= CURR_STR_SIZE)
       {
            strcpy (curr_str, curr_ptr);
          curr_ptr = curr_ptr+strlen(curr_str)+1;
       }
       else
       {
          printf ("The curr_str buffer is not big enough! \n");
          exit (1);
       }

       DEBUG( printf ("The current environmental value pair is: %s \n", curr_str); )
       
       curr_name = strtok (curr_str, "=");
       curr_val = strtok (NULL, "\0");
       
       if (curr_val) 
       {
       if ( ret = setenv (curr_name, curr_val, 0) )
       {    
          printf ("error in setting environmental variable %s = %s. \n", curr_name, curr_val);
          exit (1);
       }
       } 
         
       DEBUG( printf ("check the environmetal variable %s = %s. \n", curr_name, getenv(curr_name)); )
      }

   }
      MPI_Barrier (MPI_COMM_WORLD);

}
