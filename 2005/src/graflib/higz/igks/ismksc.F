*
* $Id: ismksc.F,v 1.2 1996/06/05 10:20:55 cernlib Exp $
*
* $Log: ismksc.F,v $
* Revision 1.2  1996/06/05 10:20:55  cernlib
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
      SUBROUTINE ISMKSC(RMSCF)
*.===========>
*.
*.  This routine sets the marker scale factor .
*.
*. _Input parameters:
*.
*.  REAL MSCF : Marker scale factor .
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
*.______________________________________
*
      IF(RMSCF.EQ.RMKSC)RETURN
      RMKSC=RMSCF
#if !defined(CERNLIB_MGKS)
      IF(GFLAG)CALL GSMKSC(RMSCF)
#endif
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(IMKSCO)
#endif
*
      END
#endif
