*
* $Id: setpix.F,v 1.1.1.1 1996/02/14 13:11:05 mclareni Exp $
*
* $Log: setpix.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:05  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.49.21  by  Unknown
*-- Author : V.Fine
      SUBROUTINE SETPIX(IX,IY)
#include "higz/hikern.inc"
#include "higz/hiatt.inc"
#include "higz/himeta.inc"
      IF (ILOPWK(IWKNB) .EQ. IDMETA) THEN
         IF (IWTYL(IWKNB) .EQ. ISURF) THEN
            CALL MOVE2I(IX,IY)
            CALL DRAW2I(IX,IY)
         END IF
      ELSE
         IER = SET_PIXEL(IX,IY)
         IXSAVE(IWKNB) = IX
         IYSAVE(IWKNB) = IY
      END IF
      END
#endif
