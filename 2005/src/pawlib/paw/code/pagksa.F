*
* $Id: pagksa.F,v 1.1.1.1 1996/03/01 11:38:39 mclareni Exp $
*
* $Log: pagksa.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:39  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.06/11 30/01/95  18.26.51  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAGKSA
*
*          /GRAPHICS_BASIC/ATTRIBUTES
*
#include "paw/pcpatl.inc"
      DIMENSION LISTI(50)
C.
C.    ------------------------------------------------------------------
C.
      CALL KUPATL(CHPATL,NPAR)
C
C           COLOR_TABLE
C
      IF(CHPATL.EQ.'COLOR_TABLE')THEN
         CALL KUGETI(ICOL)
         CALL KUGETR(RED)
         CALL KUGETR(GREEN)
         CALL KUGETR(BLUE)
         CALL ISCR(1,ICOL,RED,GREEN,BLUE)
      ENDIF
C
C           PALETTE
C
      IF(CHPATL.EQ.'PALETTE')THEN
         CALL KUGETI(LISTNB)
         CALL KUGETI(NI)
         DO 10 I=1,NI
            CALL KUGETI(LISTI(I))
  10     CONTINUE
         CALL HPLIST(LISTNB,NI,LISTI)
      ENDIF
*
  99  END
