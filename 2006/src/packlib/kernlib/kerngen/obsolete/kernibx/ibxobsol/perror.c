/*
 * $Id: perror.c,v 1.1.1.1 1996/02/15 17:54:45 mclareni Exp $
 *
 * $Log: perror.c,v $
 * Revision 1.1.1.1  1996/02/15 17:54:45  mclareni
 * Kernlib
 *
 */
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
/*>    ROUTINE PERROR
  CERN PROGLIB#         PERROR          .VERSION KERNIBX  1.02  900801
  ORIG. 20/07/90, JZ
  Fortran interface routine to perror   */
#include <stdio.h>
      void perror_ (text, ant)
      char text[];
      long *ant;
{
      int  ntext;
      int  jcol, nall;
      char *pttext, *ptuse, *malloc();

      ntext = *ant;

/*        get memory and copy NAME terminated  */

      nall   = ntext + 6;
      pttext = malloc (nall);
      if (pttext == NULL)          return;

      ptuse = pttext;
      jcol  = 0;
      while (jcol < ntext)    *ptuse++ = text[jcol++];
      *ptuse = '\0';

      perror (pttext);
      free (pttext);
      return;
}
/*> END <----------------------------------------------------------*/
