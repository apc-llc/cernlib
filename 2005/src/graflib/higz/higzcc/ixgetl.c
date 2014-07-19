/*
 * $Id: ixgetl.c,v 1.3 1997/09/02 15:30:30 mclareni Exp $
 *
 * $Log: ixgetl.c,v $
 * Revision 1.3  1997/09/02 15:30:30  mclareni
 * WINNT corrections
 *
 * Revision 1.2  1997/03/14 14:22:28  mclareni
 * WNT mods
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:25:22  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.1.1.1  1996/02/14 13:10:26  mclareni
 * Higz
 *
 */
#include "higz/pilot.h"
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_CCREAD))
/*CMZ :  1.23/06 22/11/95  10.38.33  by  O.Couet*/
/*-- Author :    O.Couet   30/09/93*/
/*
 * C routine to read a character string on stdin.
 */

#include <stdio.h>
#include <stdlib.h>

#if defined(CERNLIB_QX_SC)
#define ixgetl ixgetl_
#endif
#if defined(CERNLIB_CRAY)
#define ixgetl IXGETL
#endif
#if (defined(CERNLIB_TKTCL))&&(defined(CERNLIB_WINNT))  || defined(CERNLIB_MSSTDCALL)
#define ixgetl __stdcall IXGETL
#endif

#define SMALLBUF_SIZE   200

#ifndef CERNLIB_MSSTDCALL
 void ixgetl(lline, chline)
#else
 void ixgetl(lline, len_lline, chline)
 int len_lline;
#endif

char *chline;
int  *lline;
{
  static char *buf;
  static int bufsize = 0;
  int ch, pos;

  if (bufsize == 0)
    {
      bufsize = 80;
      buf = (char*) calloc(bufsize + 1, sizeof(char));
    }

  for (pos = 0; (ch = getc(stdin)) != EOF && ch != '\n'; buf[pos++] = ch)
    {
      if (pos == bufsize)
        {
          bufsize *= 2;
          buf = (char*) realloc(buf, (bufsize + 1)*sizeof(char));
        }
    }
  buf[pos] = '\0';

  /*  If we have a huge buffer from the last call and now have a
   *  short line, try to dump the excess.
   */
  if (pos <= SMALLBUF_SIZE && bufsize > 5000)
    {
      char *smallbuf;

 if ((smallbuf = (char*)realloc(buf,(SMALLBUF_SIZE+1)*sizeof(char))) != NULL)
        {
          buf = smallbuf;
          bufsize = SMALLBUF_SIZE;
        }
    }

  if (pos == 0 && ch == EOF) {
     *lline = 0;
     chline = '\0';
  } else {
     *lline = pos;
     strcpy (chline, buf);
   }
}
#endif
