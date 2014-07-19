*
* $Id: subword.F,v 1.1.1.1 1996/02/15 17:47:49 mclareni Exp $
*
* $Log: subword.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:49  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      CHARACTER*(*) FUNCTION SUBWORD(STR,IW,NW)
C
C CERN PROGLIB# M433    SUBWORD         .VERSION KERNBIT  1.07  910902
C Author: M.Marquina 91/03/03
C
C-    Select NW words of string STR starting at word IW (word-del CHSEP)
C
      CHARACTER*(*) STR
#include "m433cs.inc"
      LSTR=LENOCC(STR)
      SUBWORD=' '
      IF(IW.LT.1) RETURN
C*>   Strip leading characters
      I0=1
      DO WHILE(STR(I0:I0).EQ.CHSEP.AND.I0.LT.LSTR)
        I0=I0+1
      ENDDO
      IF(I0.EQ.LSTR) RETURN
      JW=1
      IS=I0
      IE=LSTR
      DO 10 N=I0,LSTR
        IF(STR(N:N).NE.CHSEP)            GOTO 10
        IF(STR(N-1:N-1).NE.CHSEP) JW=JW+1
        IF(JW.LT.IW)                     GOTO 10
        IF(JW.EQ.IW) IS=N+1
        IF(JW.NE.IW+NW)                  GOTO 10
        IE=N-1
        GOTO 20
 10   CONTINUE
 20   CONTINUE
      IF(IW.GT.JW) RETURN
      IF(IE.GE.IS) SUBWORD=STR(IS:IE)
      RETURN
      END
