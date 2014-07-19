*
* $Id: islwsc.F,v 1.2 1996/06/05 10:20:54 cernlib Exp $
*
* $Log: islwsc.F,v $
* Revision 1.2  1996/06/05 10:20:54  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:45  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.06/04 10/02/89  14.28.34  by  O.Couet
*-- Author :
      SUBROUTINE ISLWSC(WIDTH)
*.===========>
*.
*.   The routine sets the line width scale factor .
*.
*. _Input parameters:
*.
*.  REAL LWIDTH : Line width .
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
*.______________________________________
*
      IF(WIDTH.EQ.RLWSC)RETURN
      RLWSC=WIDTH
      IF(GFLAG)CALL GSLWSC(WIDTH)
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(ILWSCO)
#endif
*
      END
#endif
