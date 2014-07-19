*
* $Id: isplci.F,v 1.2 1996/06/05 10:20:56 cernlib Exp $
*
* $Log: isplci.F,v $
* Revision 1.2  1996/06/05 10:20:56  cernlib
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
      SUBROUTINE ISPLCI(ICOLI)
*.===========>
*.
*.   This routine sets the polyline color index .
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
      IF(ICOLI.EQ.IPLCI)RETURN
      IPLCI=ICOLI
      IF(GFLAG)CALL GSPLCI(ICOLI)
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(IPLCCO)
#endif
*
      END
#endif
