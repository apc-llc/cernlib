/*
 * $Id: tmread.c,v 1.1.1.1 1996/02/15 17:52:45 mclareni Exp $
 *
 * $Log: tmread.c,v $
 * Revision 1.1.1.1  1996/02/15 17:52:45  mclareni
 * Kernlib
 *
 */
#include "kerncry/pilot.h"
/*>    ROUTINE TMREAD
  CERN PROGLIB#         TMREAD          .VERSION KERNCRY  1.16  911111
  ORIG. 20/10/91, JZ
      read the next line from stdin :
      CALL TMREAD (MAXCH, LINE, NCH, ISTAT)
          MAXCH   maxim. # of characters into LINE
          NCH     actual # of characters read into LINE
          ISTAT   status return, zero : OK  -ve : EoF
*/
#include <fortran.h>
#include <stdio.h>
void TMREAD(alim, line, anch, astat)
      _fcd line;
      int  *alim, *anch, *astat;
{
      char *cols;
      int ch, jcol, lim;

/*--      read the text   */
      cols = _fcdtocp(line);
      lim  = *alim;
      jcol = 0;
      while (lim-- > 0)
      {   ch = getchar();
          if (ch == EOF)           goto endf;
          if (ch == '\n')          goto endl;
          *cols++ = ch;
          jcol++;
       }
/*        discard excess characters   */
loop: ch = getchar();
      if (ch == '\n')          goto endl;
      if (ch != EOF)           goto loop;

endf: *astat = -1;
      clearerr(stdin);
      return;

endl: *anch = jcol;
      *astat = 0;
      return;
}
/*> END <----------------------------------------------------------*/
#ifdef CERNLIB_CCGEN_TMREAD
#undef CERNLIB_CCGEN_TMREAD
#endif
