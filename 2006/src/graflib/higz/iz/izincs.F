*
* $Id: izincs.F,v 1.1.1.1 1996/02/14 13:11:10 mclareni Exp $
*
* $Log: izincs.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:10  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.11/00 27/09/90  15.56.02  by  O.Couet
*-- Author :
      SUBROUTINE IZINCS(INC)
*.===========>
*.
*.   Increments the (C) bank pointer .
*.
*. _Input parameters:
*.
*.  INTEGER INC : Increment .
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
*.______________________________________
*
      ISTPTR=ISTPTR+INC
      IQ(LPICT+4)=ISTPTR
*
      END
#endif
