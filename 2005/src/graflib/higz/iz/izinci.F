*
* $Id: izinci.F,v 1.1.1.1 1996/02/14 13:11:10 mclareni Exp $
*
* $Log: izinci.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:10  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.11/00 27/09/90  15.56.02  by  O.Couet
*-- Author :
      SUBROUTINE IZINCI(INC)
*.===========>
*.
*.   Increments the (I) bank pointer .
*.
*. _Input parameters:
*.
*.  INTEGER INC : Increment .
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
*.______________________________________
*
      INTPTR=INTPTR+INC
      IQ(LPICT+2)=INTPTR
*
      END
#endif
