*
* $Id: iclks.F,v 1.2 1996/06/05 10:20:30 cernlib Exp $
*
* $Log: iclks.F,v $
* Revision 1.2  1996/06/05 10:20:30  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:44  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.06/04 27/01/89  16.40.30  by  O.Couet
*-- Author :
      SUBROUTINE ICLKS
*.===========>
*.
*.   This routine closes the graphic package .
*.
*..==========> (O.Couet)
#include "higz/hiflag.inc"
*.______________________________________
*
#if defined(CERNLIB_GKS)
      IF(GFLAG)CALL GCLKS
#endif
*
      END
#endif
