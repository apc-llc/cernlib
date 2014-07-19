*
* $Id: izgadr.F,v 1.1.1.1 1996/02/14 13:11:10 mclareni Exp $
*
* $Log: izgadr.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:10  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.11/00 27/09/90  15.56.02  by  O.Couet
*-- Author :
      FUNCTION IZGADR(LN,IPOS)
*.===========>
*.
*.   This function Gives the ADRess in the word number IPOS in the bank LN .
*.
*. _Input parameters:
*.
*.  INTEGER LN : NT link .
*.  INTEGER IPOS : Word position .
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
*.______________________________________
*
      IF(LN.GT.0)THEN
         IZGADR=MOD(IQ(LN+IPOS),1000000)
      ELSE
         IZGADR=0
      ENDIF
*
      END
#endif
