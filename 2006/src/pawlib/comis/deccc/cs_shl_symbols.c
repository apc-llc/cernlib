/*
 * $Id: cs_shl_symbols.c,v 1.1.1.1 1996/02/26 17:16:55 mclareni Exp $
 *
 * $Log: cs_shl_symbols.c,v $
 * Revision 1.1.1.1  1996/02/26 17:16:55  mclareni
 * Comis
 *
 */
#include "comis/pilot.h"
#if (defined(CERNLIB_HPUX))&&(defined(CERNLIB_SHL))
/*CMZ :  1.18/00 02/02/94  16.46.03  by  Vladimir Berezhnoi*/
/*-- Author :    Fons Rademakers   16/12/93*/
#include <string.h>
#include <stdlib.h>
#include <dl.h>
void cs_shl_symbols_(path, ns, symbol, n)
   char *path, *symbol;
   int  *ns;
   int   n;
{
   shl_t  handle;
   struct shl_descriptor *desc;
   char   lib_name[80];
   int    index, flags;
   short  type;
   static nsym;
   static struct shl_symbol *symbols;

   if (*ns == -1) {
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
         *ns = -2;
         return;
      }

      nsym = shl_getsymbols(handle, TYPE_PROCEDURE,
                            EXPORT_SYMBOLS|NO_VALUES, malloc, &symbols);
      if (nsym == -1) {
         printf(" CS: Could not get symbols from shared library: %s\n",
                lib_name);
         *ns = -2;
         return;
      }

      *ns = 0;
   } else {
      if (*ns >= nsym-1) {
         *ns = -2;
         free(symbols);
         return;
      } else
         (*ns)++;
   }

   memset(symbol, ' ', 32);
   strncpy(symbol, symbols[*ns].name, strlen(symbols[*ns].name));

}
#endif
