/*
 * $Id: rename.c,v 1.1.1.1 1996/02/15 17:54:46 mclareni Exp $
 *
 * $Log: rename.c,v $
 * Revision 1.1.1.1  1996/02/15 17:54:46  mclareni
 * Kernlib
 *
 */
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
/*>    ROUTINE RENAME
  CERN PROGLIB#         RENAME          .VERSION KERNIBX  1.02  900801
  ORIG. 20/07/90, JZ
  Fortran interface routine to rename   */
#include <stdio.h>
      int rename_ (frpath, topath, anfr, anto)
      char frpath[], topath[];
      long *anfr, *anto;
{
      int  nnfr, ncolfr,  nnto, ncolto;
      int  jcol, nall, istat;
      char *ptall, *ptfr, *ptto, *ptuse;
      char *malloc();
      int  rename();

      nnfr = *anfr;
      nnto = *anto;
      istat= -1;

/*--      find last blank of from-path  */
      ncolfr = nnfr;
      while (--ncolfr >= 0)
          if (frpath[ncolfr] != ' ')   goto endfr;
      goto home;

endfr: ncolfr = ncolfr + 1;

/*--      find last blank of to-path  */
      ncolto = nnto;
      while (--ncolto >= 0)
          if (topath[ncolto] != ' ')   goto endto;
      goto home;

endto: ncolto = ncolto + 1;

/*        get memory and copy file-names terminated  */

      nall  = ncolfr + ncolto + 6;
      ptall = malloc (nall);
      if (ptall == NULL)           goto home;

      ptfr  = ptall;
      ptuse = ptall;

      jcol = 0;
      while (jcol < ncolfr)    *ptuse++ = frpath[jcol++];
      *ptuse++ = '\0';

      ptto = ptuse;
      jcol = 0;
      while (jcol < ncolto)    *ptuse++ = topath[jcol++];
      *ptuse = '\0';

/*        execute the RENAME  */

      istat = rename (ptfr, ptto);

      free (ptall);
home: return istat;
}
/*> END <----------------------------------------------------------*/
