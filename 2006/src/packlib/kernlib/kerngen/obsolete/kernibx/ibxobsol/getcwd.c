/*
 * $Id: getcwd.c,v 1.1.1.1 1996/02/15 17:54:45 mclareni Exp $
 *
 * $Log: getcwd.c,v $
 * Revision 1.1.1.1  1996/02/15 17:54:45  mclareni
 * Kernlib
 *
 */
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
/*>    ROUTINE GETCWD
  CERN PROGLIB#         GETCWD          .VERSION KERNIBX  1.02  900801
  ORIG. 20/07/90, JZ
  Fortran interface routine to getcwd   */
#include <stdio.h>
      void getcwd_(text, ant)
      char text[];
      long *ant;
{
      int  ntext, jcol;
      char *pttext, *getcwd();

      ntext = *ant;
      jcol  = 0;

      pttext = getcwd (text, ntext);
      if (pttext == NULL)          goto blfi;

/*--      find the terminator in the returned path-name  */
      while (jcol < ntext)
      {   if (text[jcol] == '\0')   goto blfi;
          ++jcol;
      }
      return;

/*        blank fill the trailing text  */
blfi: while (jcol < ntext)    text[jcol++] = ' ';
      return;
}
/*> END <----------------------------------------------------------*/
