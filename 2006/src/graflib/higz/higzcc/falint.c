/*
 * $Id: falint.c,v 1.4 1997/10/23 12:29:51 mclareni Exp $
 *
 * $Log: falint.c,v $
 * Revision 1.4  1997/10/23 12:29:51  mclareni
 * NT mods
 *
 * Revision 1.2  1997/03/14 14:22:27  mclareni
 * WNT mods
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:25:21  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.1.1.1  1996/02/14 13:10:25  mclareni
 * Higz
 *
 */
#include "higz/pilot.h"
#if (defined(CERNLIB_FALCO))&&(!defined(CERNLIB_VAX))
/*CMZ :  1.23/06 22/11/95  10.36.54  by  O.Couet*/
/*-- Author :*/
/*
 * C code for HIGZ/FALCO interface
 */
#ifdef CERNLIB_WINNT
# include <io.h>
#endif
#if defined(CERNLIB_CRAY)
#define ifput1   IFPUT1
#endif
#if defined(CERNLIB_QX_SC)
#define ifput1   ifput1_
#endif
#if (defined(CERNLIB_TKTCL))&&(defined(CERNLIB_WINNT)) || defined(CERNLIB_QFMSOFT)
#define ifput1 __stdcall IFPUT1
#endif

static char lbout [256];

void ifput1(bout,ipout)
int *bout;
int *ipout;
      {
      int i;

      for (i=0 ; i < *ipout ; i++)
        lbout[i] = (char)(bout[i]);
      write(1, lbout, *ipout);
}
#endif
