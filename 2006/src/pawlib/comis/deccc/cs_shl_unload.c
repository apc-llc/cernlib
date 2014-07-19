/*
 * $Id: cs_shl_unload.c,v 1.1.1.1 1996/02/26 17:16:55 mclareni Exp $
 *
 * $Log: cs_shl_unload.c,v $
 * Revision 1.1.1.1  1996/02/26 17:16:55  mclareni
 * Comis
 *
 */
#include "comis/pilot.h"
#if (defined(CERNLIB_HPUX))&&(defined(CERNLIB_SHL))
/*CMZ :  1.17/07 16/12/93  18.44.32  by  Vladimir Berezhnoi*/
/*-- Author :    Fons Rademakers   16/12/93*/
#include <string.h>
#include <dl.h>
void cs_shl_unload_(path, n)
   char *path;
   int n;
{
   shl_t  handle;
   struct shl_descriptor *desc;
   char   lib_name[80];
   int    index;

   strncpy(lib_name, path, n);
   lib_name[n] = '\0';

   /* find handle of shared library using its name */
   index  = 0;
   handle = NULL;
   while (shl_get(index++, &desc) == 0) {
      if (!strcmp(lib_name, desc->filename)) {
         handle = desc->handle;
         break;
      }
   }

   if (!handle) {
/*      printf(" CS: Shared library not loaded: %s\n", lib_name);*/
      return;
   }

   if (shl_unload(handle) == -1)
      printf(" CS: Could not unload shared library: %s\n", lib_name);
}
#endif
