*
* $Id: cursof.F,v 1.1.1.1 1996/02/14 13:11:03 mclareni Exp $
*
* $Log: cursof.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:03  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.49.21  by  Unknown
*-- Author : V.Fine
      SUBROUTINE CURSOF
#include "higz/grex.inc"
      IER = MOVE_CURSOR(-1,-1)
      END
#endif
