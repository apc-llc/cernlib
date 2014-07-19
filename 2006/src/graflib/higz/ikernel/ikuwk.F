*
* $Id: ikuwk.F,v 1.1.1.1 1996/02/14 13:10:56 mclareni Exp $
*
* $Log: ikuwk.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:56  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_X11)
*CMZ :  1.14/11 26/06/92  14.55.46  by  O.Couet
*-- Author :    O.Couet   24/06/92
      SUBROUTINE IKUWK(IWKID)
#include "higz/hiatt.inc"
      I       = IGIWIN(IWKID)
      RW      = (XWKW2(I)-XWKW1(I)) / (YWKW2(I)-YWKW1(I))
      XWKV2N  = RW*(YWKV2(I)-YWKV1(I)) + XWKV1(I)
      YWKV2N  = (1./RW)*(XWKV2(I)-XWKV1(I)) + YWKV1(I)
      IF(XWKV2N.GT.XWKV2(I))THEN
         YWKV2(I) = YWKV2N
         GOTO 10
      ENDIF
      IF(YWKV2N.GT.YWKV2(I))THEN
         XWKV2(I) = XWKV2N
         GOTO 10
      ENDIF
   10 R       = REDIT
      IF(R.LE.0)R=1
      XWKR(I) = R*(XWKV2(I)-XWKV1(I)) / (XWKW2(I)-XWKW1(I))
      YWKR(I) = R*(YWKV2(I)-YWKV1(I)) / (YWKW2(I)-YWKW1(I))
      END
#endif
