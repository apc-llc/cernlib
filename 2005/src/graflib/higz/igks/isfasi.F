*
* $Id: isfasi.F,v 1.2 1996/06/05 10:20:44 cernlib Exp $
*
* $Log: isfasi.F,v $
* Revision 1.2  1996/06/05 10:20:44  cernlib
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
      SUBROUTINE ISFASI(ISTYL)
*.===========>
*.
*.   This routine sets the fill area style index .
*.
*. _Input parameters:
*.
*.  INTEGER ISTYL : Style index .
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
*.______________________________________
*
      IF(ISTYL.EQ.IFASI)RETURN
      IFASI=ISTYL
#if defined(CERNLIB_MGKS)
      IF(GFLAG.AND.(IFASI.GE.2).AND.(IFASI.LE.3))CALL GSFAIS(IFASI)
#endif
#if !defined(CERNLIB_MGKS)
      IF(GFLAG)CALL GSFASI(ISTYL)
#endif
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(IFASCO)
#endif
*
      END
#endif
