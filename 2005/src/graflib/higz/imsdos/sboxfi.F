*
* $Id: sboxfi.F,v 1.1.1.1 1996/02/14 13:11:04 mclareni Exp $
*
* $Log: sboxfi.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:04  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.49.21  by  Unknown
*-- Author : V.Fine
      SUBROUTINE SBOXFI(IX1,IY1,IX2,IY2)
#include "higz/hikern.inc"
#include "higz/hiatt.inc"
#include "higz/himeta.inc"
      DIMENSION IBX(2,4)
      IF (ILOPWK(IWKNB) .EQ. IDMETA) THEN
         IF (IWTYL(IWKNB).EQ. ISURF) THEN
            IBX(2,1) = IX1
            IBX(2,1) = IX2
            IBX(3,1) = IX2
            IBX(4,1) = IX1
            IBX(1,2) = IY1
            IBX(2,2) = IY1
            IBX(3,2) = IY2
            IBX(4,2) = IY2
            IDX = IABS(IX2-IX1)
            IDY = IABS(IY2-IY1)
            IF (IDX .GT. IDY) THEN
               IFY = MIN(IY1,IY2)-1
               DO 10 I=1,IDY
                  CALL MOVE2I(IX1,IFY+I)
                  CALL DRAW2I(IX2,IFY+I)
   10          CONTINUE
            ELSE
               IFX = MIN(IX1,IX2)-1
               DO 20 I=1,IDX
                  CALL MOVE2I(IFX+I,IY1)
                  CALL DRAW2I(IFX+1,IY2)
   20          CONTINUE
            END IF
         END IF
      ELSE
         IER = FILLED_RECTANGLE(IX1,IY2,IX2,IY1)
      END IF
      END
#endif
