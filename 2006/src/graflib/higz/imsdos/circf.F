*
* $Id: circf.F,v 1.1.1.1 1996/02/14 13:11:03 mclareni Exp $
*
* $Log: circf.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:03  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.49.21  by  Unknown
*-- Author : V.Fine
      SUBROUTINE CIRCF(IX,IY,IRAD)
#include "higz/grex.inc"
      IER = FILLED_ELLIPSE(IX,IY,IRAD,IRAD)
      END
#endif
