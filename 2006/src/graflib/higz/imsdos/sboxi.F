*
* $Id: sboxi.F,v 1.1.1.1 1996/02/14 13:11:04 mclareni Exp $
*
* $Log: sboxi.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:04  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.49.21  by  Unknown
*-- Author : V.Fine
      SUBROUTINE SBOXI(IX1,IY1,IX2,IY2)
#include "higz/hiatt.inc"
      CALL SETLIN(1)
      CALL MOVE2I(IX1,IY1)
      CALL DRAW2I(IX2,IY1)
      CALL DRAW2I(IX2,IY2)
      CALL DRAW2I(IX1,IY2)
      CALL DRAW2I(IX1,IY1)
      CALL SETLIN(ILN)
      END
#endif
