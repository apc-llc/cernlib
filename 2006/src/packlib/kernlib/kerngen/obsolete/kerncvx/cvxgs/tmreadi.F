*
* $Id: tmreadi.F,v 1.1.1.1 1996/02/15 17:52:29 mclareni Exp $
*
* $Log: tmreadi.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:29  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
/*>    ROUTINE TMREADI
  CERN PROGLIB#         TMREAD          .VERSION KERNCVX  1.08  910806
  ORIG. 06/08/91, JZ
      read the next line from stdin :
      CALL TMREAD (MAXCH, LINE, NCH, ISTAT)
          MAXCH   maxim. # of characters into LINE
          NCH     actual # of characters read into LINE
          ISTAT   status return, zero : OK  -ve : EoF
*/
#include <stdio.h>

void tmreadi_(alim, cols, anch, astat)
      char *cols;
      int  *alim, *anch, *astat;
{
      int ch, jcol, lim;

/*--      read the text   */
      lim  = *alim;
      jcol = 0;
      while (lim-- > 0)
      {   ch = getchar();
          if (ch == EOF)           goto endf;
          if (ch == '\n')          goto endl;
          cols[jcol] = ch;
          jcol = jcol + 1;
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
