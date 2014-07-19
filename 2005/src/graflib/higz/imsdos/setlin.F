*
* $Id: setlin.F,v 1.1.1.1 1996/02/14 13:11:04 mclareni Exp $
*
* $Log: setlin.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:04  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.49.21  by  Unknown
*-- Author : V.Fine
      SUBROUTINE SETLIN(ILIN)
#include "higz/hikern.inc"
#include "higz/hiatt.inc"
#include "higz/himeta.inc"
      I = ILIN
      IF (I .LE. 0) I = 1
      IF (ILOPWK(IWKNB) .EQ. IDMETA) THEN
         IF (IWTYL(IWKNB) .EQ. ISURF) THEN
         END IF
      ELSE
         IER = SET_DASH(DASH_PATTERN(ILN))
      END IF
      END
#endif
