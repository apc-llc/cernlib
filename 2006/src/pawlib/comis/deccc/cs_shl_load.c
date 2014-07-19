/*
 * $Id: cs_shl_load.c,v 1.1.1.1 1996/02/26 17:16:55 mclareni Exp $
 *
 * $Log: cs_shl_load.c,v $
 * Revision 1.1.1.1  1996/02/26 17:16:55  mclareni
 * Comis
 *
 */
#include "comis/pilot.h"
#if (defined(CERNLIB_HPUX))&&(defined(CERNLIB_SHL))
/*CMZ :  1.19/00 18/01/95  17.16.20  by  Vladimir Berezhnoi*/
/*-- Author :*/
#include <string.h>
#include <dl.h>
void perror();
int  cs_shl_load_(path, n)
   char *path;
   int n;
{
   shl_t  handle;
   char   lib_name[80];
/*   int    flags=BIND_DEFERRED; */
   int    flags=BIND_IMMEDIATE;
/*   int    flags=BIND_IMMEDIATE | BIND_NONFATAL; */
   long   address=0L;
   extern int errno;

   strncpy(lib_name, path, n);
   lib_name[n] = '\0';

   handle = shl_load(lib_name, flags, address);

   if (handle==0) {
     printf(" CS: Could not load shared library: %s\n",lib_name);
     perror("cs_shl_load");
     };
   return (errno);

}
#endif
