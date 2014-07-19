*
* $Id: isfais.F,v 1.2 1996/06/05 10:20:43 cernlib Exp $
*
* $Log: isfais.F,v $
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
      SUBROUTINE ISFAIS(INTS)
*.===========>
*.
*.   This routine sets the fill area interior style .
*.
*. _Input parameters:
*.
*.  INTEGER INTS : Interior style .
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
*.______________________________________
*
      IF(INTS.EQ.IFAIS)RETURN
      IF((INTS.LT.0).OR.(INTS.GT.3))THEN
         CALL IGERR('Fill area interior style must be > -1 and < 4'
     +,             'ISFAIS')
         RETURN
      ENDIF
      IFAIS=INTS
#if defined(CERNLIB_MGKS)
      IF(GFLAG.AND.(IFAIS.LE.1))CALL GSFAIS(INTS)
#endif
#if !defined(CERNLIB_MGKS)
      IF(GFLAG)CALL GSFAIS(INTS)
#endif
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(IFAICO)
#endif
*
      END
#endif
