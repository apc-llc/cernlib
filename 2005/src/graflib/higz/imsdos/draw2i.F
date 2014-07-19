*
* $Id: draw2i.F,v 1.1.1.1 1996/02/14 13:11:03 mclareni Exp $
*
* $Log: draw2i.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:03  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.49.21  by  Unknown
*-- Author : V.Fine
      SUBROUTINE DRAW2I(IX,IY)
#include "higz/hikern.inc"
#include "higz/hiatt.inc"
#include "higz/himeta.inc"
      IF (ILOPWK(IWKNB) .EQ. IDMETA) THEN
         IXSAVE(IWKNB) = IX
         IYSAVE(IWKNB) = IY
         IF (IWTYL(IWKNB) .EQ. ISURF) THEN
            COOR(1) = IX
            COOR(2) = YWKSIZ(IWKNB)-IY
            CALL GOLWRI(PA,COOR,' ')
         END IF
      ELSE
         IER = DRAW(IX,IY)
      END IF
      END
#endif
