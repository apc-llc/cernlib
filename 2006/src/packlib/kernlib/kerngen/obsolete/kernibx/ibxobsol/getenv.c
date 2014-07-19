/*
 * $Id: getenv.c,v 1.1.1.1 1996/02/15 17:54:45 mclareni Exp $
 *
 * $Log: getenv.c,v $
 * Revision 1.1.1.1  1996/02/15 17:54:45  mclareni
 * Kernlib
 *
 */
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
/*>    ROUTINE GETENV
  CERN PROGLIB#         GETENV          .VERSION KERNIBX  1.02  900801
  ORIG. 20/07/90, JZ
  Fortran interface routine to getenv   */
#include <stdio.h>
      void getenv_(name, text, ann, ant)
      char name[], text[];
      long *ann, *ant;
{
      int  nname, ntext;
      int  jcol, nall;
      char ch;
      char *ptname, *ptuse, *pttx, *getenv(), *malloc();

      nname = *ann;
      ntext = *ant;
      jcol  = 0;

/*--      find last non-blank of NAME  */
      while (--nname >= 0)
          if (name[nname] != ' ')   goto endn;
      goto blfi;
endn: nname = nname + 1;

/*        get memory and copy NAME terminated  */

      nall   = nname + 6;
      ptname = malloc (nall);
      if (ptname == NULL)           goto blfi;

      ptuse = ptname;
      while (jcol < nname)    *ptuse++ = name[jcol++];
      *ptuse = '\0';

      jcol = 0;
      pttx = getenv (ptname);
      if (pttx == NULL)             goto free;

/*--      copy the TEXT to caller   */
      while (jcol < ntext)
      {   ch = *pttx++;
          if (ch == '\0')          goto free;
          text[jcol++] = ch;
       }
free: free (ptname);

/*        blank fill the trailing text  */
blfi: while (jcol < ntext)    text[jcol++] = ' ';
      return;
}
/*> END <----------------------------------------------------------*/
