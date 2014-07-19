/*
 * $Id: chdir.c,v 1.1.1.1 1996/02/15 17:54:45 mclareni Exp $
 *
 * $Log: chdir.c,v $
 * Revision 1.1.1.1  1996/02/15 17:54:45  mclareni
 * Kernlib
 *
 */
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
/*>    ROUTINE CHDIR
  CERN PROGLIB#         CHDIR           .VERSION KERNIBX  1.02  900801
  ORIG. 20/07/90, JZ
  Fortran interface routine to chdir   */
#include <stdio.h>
      long chdir_(name, ann)
      char name[];
      long *ann;
{
      int  nname, jcol, nall, istat;
      char *ptname, *ptuse, *malloc();
      int  chdir();

      nname = *ann;
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

      istat = chdir (ptname);
      free (ptname);
      return istat;

blfi: return 0;
}
/*> END <----------------------------------------------------------*/
