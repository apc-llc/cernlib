*
* $Id: isfaci.F,v 1.2 1996/06/05 10:20:43 cernlib Exp $
*
* $Log: isfaci.F,v $
* Revision 1.2  1996/06/05 10:20:43  cernlib
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
      SUBROUTINE ISFACI(ICOLI)
*.===========>
*.
*.   This routine sets the fill area color index .
*.
*. _Input parameters:
*.
*.  INTEGER COLI : Fill area color index .
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
*.______________________________________
*
      IF(ICOLI.EQ.IFACI)RETURN
      IFACI=ICOLI
      IF(GFLAG)CALL GSFACI(ICOLI)
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(IFACCO)
#endif
*
      END
#endif
