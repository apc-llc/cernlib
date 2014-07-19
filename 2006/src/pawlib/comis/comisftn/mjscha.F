*
* $Id: mjscha.F,v 1.2 1997/03/14 12:00:48 mclareni Exp $
*
* $Log: mjscha.F,v $
* Revision 1.2  1997/03/14 12:00:48  mclareni
* WNT mods
*
* Revision 1.1.1.1.2.1  1997/01/21 11:34:39  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/02/26 17:16:48  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :          23/10/95  13.12.36  by  Julian Bunn
*-- Author : V.Berezhnoi
#if defined(CERNLIB_MSDOS)||defined(CERNLIB_WINNT)
      Integer Function MJSCHA(string)
#endif
#if (!defined(CERNLIB_MSDOS))&&(!defined(CERNLIB_WINNT))
      FUNCTION MJSCHA(I)
#endif
#if defined(CERNLIB_VAX)||defined(CERNLIB_DECS)
      CHARACTER *(*)I
      MJSCHA=%LOC(I)
#endif
#if defined(CERNLIB_APOLLO)
      CHARACTER *(*)I
***--------------------
*   gets byte-address of character substring
***--------------------
      MJSCHA=IADDR(I)
#endif
#if (defined(CERNLIB_IBM)||defined(CERNLIB_UNIX))&&(!defined(CERNLIB_DECS))&&!defined(CERNLIB_WINNT)
      MJSCHA=MJCHAR(I)
#endif
#if defined(CERNLIB_WINNT)||defined(CERNLIB_MSDOS)
      Character*(*) string
      MJSCHA = LOCB(string)
#endif
      END
