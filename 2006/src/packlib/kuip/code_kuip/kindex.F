*
* $Id: kindex.F,v 1.1.1.1 1996/03/08 15:32:53 mclareni Exp $
*
* $Log: kindex.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  1.63/02 23/02/90  15.21.43  by
*-- Author :
      FUNCTION KINDEX(STR,TOK)
************************************************************************
*                                                                      *
* KINDEX                                                               *
*                                                                      *
* Search backward for TOKen in STRing. Opposite from INDEX.            *
*                                                                      *
* Author :    Fons Rademakers   07/02/89                               *
*                                                                      *
************************************************************************
*
      CHARACTER*(*) STR, TOK
*
      LS = LEN(STR)
      LT = LEN(TOK)
*
      IF (LT .GT. LS) THEN
         KINDEX = 0
         RETURN
      ENDIF
*
      DO 10 I = LS, LT, -1
         IF (STR(I-LT+1:I) .EQ. TOK(1:LT)) THEN
            KINDEX = I-LT+1
            RETURN
         ENDIF
   10 CONTINUE
*
      KINDEX = 0
*
      END

