*
* $Id: istxal.F,v 1.2 1996/06/05 10:20:57 cernlib Exp $
*
* $Log: istxal.F,v $
* Revision 1.2  1996/06/05 10:20:57  cernlib
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
      SUBROUTINE ISTXAL(JTXALH,JTXALV)
*.===========>
*.
*.   This routine sets the text alignment .
*.
*. _Input parameters:
*.
*.  INTEGER TXALH : Horizontal alignment .
*.  INTEGER TXALV : Vertical alignment .
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
*.______________________________________
*
      IF((JTXALH.EQ.ITXALH).AND.(JTXALV.EQ.ITXALV))RETURN
      ITXALH=JTXALH
      ITXALV=JTXALV
      IF(GFLAG)CALL GSTXAL(JTXALH,JTXALV)
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      ITXALG=10*JTXALH+JTXALV
      IF(GLFLAG)CALL IZATT(ITXACO)
#endif
*
      END
#endif
