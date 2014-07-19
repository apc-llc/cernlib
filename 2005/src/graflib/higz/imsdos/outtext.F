*
* $Id: outtext.F,v 1.1.1.1 1996/02/14 13:11:04 mclareni Exp $
*
* $Log: outtext.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:04  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.49.21  by  Unknown
*-- Author : V.Fine
      SUBROUTINE OUT_TEXT(CHARS,IX,IY,ICOLOR)
#include "higz/hiatt.inc"
#include "higz/hikern.inc"
#include "higz/himeta.inc"
      CHARACTER CHARS*(*)
*
#include "higz/hiwcdc.inc"
      IF (ILOPWK(IWKNB) .EQ. IDMETA) THEN
         IF (IWTYL(IWKNB) .EQ. ISURF) THEN
            COOR(1) = IX
            COOR(2) = YWKSIZ(IWKNB)-IY
            COOR(3) = YWKSIZ(IWKNB) - IYWCDC(RCHH)
            COOR(4) = RANGLE
            CALL GOLWRI(PS,COOR,CHARS)
         END IF
      ELSE
         IHHH = STRWID(CHARS,1)
         IER = GRAPHIC_TEXT(CHARS,IX,IY -IHHH+1,ICOLOR)
      END IF
      IXSAVE(IWKNB) = -1
      IYSAVE(IWKNB) = -1
      END
#endif
