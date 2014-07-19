/*
 * $Id: cs_shl_get.c,v 1.1.1.1 1996/02/26 17:16:55 mclareni Exp $
 *
 * $Log: cs_shl_get.c,v $
 * Revision 1.1.1.1  1996/02/26 17:16:55  mclareni
 * Comis
 *
 */
#include "comis/pilot.h"
#if (defined(CERNLIB_HPUX))&&(defined(CERNLIB_SHL))
/*CMZ :  1.17/10 13/01/94  11.08.45  by  Vladimir Berezhnoi*/
/*-- Author :    Fons Rademakers   16/12/93*/
#include <string.h>
#include <stdlib.h>
#include <dl.h>
void cs_shl_get_(ns, symbol, n)
   char *symbol;
   int  *ns;
   int   n;
{
   /* find name of ns [ns=0 for first] shared library  */
   struct shl_descriptor *desc;

   memset(symbol, ' ', n);
   if (shl_get(*ns, &desc) == 0)

      strncpy(symbol,desc->filename , strlen(desc->filename));

}
#endif
