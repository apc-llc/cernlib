*
* $Id: scrmas.F,v 1.1.1.1 1996/02/14 13:11:04 mclareni Exp $
*
* $Log: scrmas.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:04  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.49.21  by  Unknown
*-- Author : V.Fine
      SUBROUTINE SCRMAS(IX1,IX2,IY2,IY1)
#include "higz/hikern.inc"
#include "higz/hiatt.inc"
#include "higz/grex.inc"
      IYMIN = IY1 - 1
      IYMAX = IY2 + 1
      IXMIN = IX1 - 1
      IXMAX = IX2 + 1
      IYMIN = MAX(0,IYMIN)
      IXMIN = MAX(0,IXMIN)
      IYMAX = MIN(INT(YWKSIZ(IWKNB))-1,IYMAX)
      IXMAX = MIN(INT(XWKSIZ(IWKNB))-1,IXMAX)
      IER = SET_CLIP_LIMITS(IXMIN,IYMIN,IXMAX,IYMAX)
      END
#endif
