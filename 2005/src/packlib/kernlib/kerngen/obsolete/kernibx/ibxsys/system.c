/*
 * $Id: system.c,v 1.1.1.1 1996/02/15 17:54:45 mclareni Exp $
 *
 * $Log: system.c,v $
 * Revision 1.1.1.1  1996/02/15 17:54:45  mclareni
 * Kernlib
 *
 */
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
/*>    ROUTINE SYSTEM
  CERN PROGLIB#         SYSTEM          .VERSION KERNIBX  1.02  900801
  ORIG. 20/07/90, JZ
  Fortran interface routine to system   */
#include <stdio.h>
      int system_ (text, anc)
      char text[];
      long *anc;
{
      int  ncmd, jcol, istat;
      char ch;
      int  system();

      ncmd = *anc;

/*--      find last blank of command line  */
      jcol = ncmd;
      while (--jcol >= 0)
          if (text[jcol] != ' ')   goto endn;
      return 7;

endn: jcol = jcol + 1;
      ch = text[jcol];
      text[jcol] = '\0';

      istat = system (text);

      text[jcol] = ch;
      return istat;
}
/*> END <----------------------------------------------------------*/
