*
* $Id: izstcc.F,v 1.1.1.1 1996/02/14 13:11:14 mclareni Exp $
*
* $Log: izstcc.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:14  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.11/00 27/09/90  15.56.04  by  O.Couet
*-- Author :
      SUBROUTINE IZSTCC(ICODE,IADR)
*.===========>
*.
*.   This routine STores the Control Code ICODE and the adress IADR in the
*. current normalisation transformation bank .
*.
*. _Input parameters:
*.
*.  INTEGER ICODE : Control code .
*.  INTEGER IADR : Adress .
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
*.______________________________________
*
      IQ(LHNT+ITBPTR)=1000000*ICODE+IADR
      ITBPTR=ITBPTR+1
      IQ(LHNT+4)=ITBPTR
*
      END
#endif
