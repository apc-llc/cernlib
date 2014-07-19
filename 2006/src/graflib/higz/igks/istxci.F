*
* $Id: istxci.F,v 1.2 1996/06/05 10:20:59 cernlib Exp $
*
* $Log: istxci.F,v $
* Revision 1.2  1996/06/05 10:20:59  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:46  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.06/04 10/02/89  14.28.34  by  O.Couet
*-- Author :
      SUBROUTINE ISTXCI(ICOLI)
*.===========>
*.
*.   This routine sets the text color index .
*.
*. _Input parameters:
*.
*.  INTEGER COLI : Color index .
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
*.______________________________________
*
      IF(ICOLI.EQ.ITXCI)RETURN
      ITXCI=ICOLI
      IF(GFLAG)CALL GSTXCI(ICOLI)
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(ITXCCO)
#endif
*
      END
#endif
