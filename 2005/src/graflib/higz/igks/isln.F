*
* $Id: isln.F,v 1.2 1996/06/05 10:20:44 cernlib Exp $
*
* $Log: isln.F,v $
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
      SUBROUTINE ISLN(LTYPE)
*.===========>
*.
*.   This routine sets the line type .
*.
*. _Input parameters:
*.
*.  INTEGER LTYPE : Line type .
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
*.______________________________________
*
      IF(LTYPE.EQ.ILN)RETURN
      ILN=LTYPE
      IF(GFLAG)CALL GSLN(LTYPE)
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(ILNCO)
#endif
*
      END
#endif
