*
* $Id: ischh.F,v 1.2 1996/06/05 10:20:39 cernlib Exp $
*
* $Log: ischh.F,v $
* Revision 1.2  1996/06/05 10:20:39  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:44  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.06/04 10/02/89  14.28.33  by  O.Couet
*-- Author :
      SUBROUTINE ISCHH(CHH)
*.===========>
*.
*.   This routine sets the character heigh .
*.
*. _Input parameters:
*.
*.  INTEGER CHH : Character heigh .
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
*.______________________________________
*
      IF(CHH.EQ.RCHH)RETURN
      RCHH=CHH
      IF(GFLAG)CALL GSCHH(CHH)
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(ICHHCO)
#endif
*
      END
#endif
