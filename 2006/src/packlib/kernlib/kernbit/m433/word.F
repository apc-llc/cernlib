*
* $Id: word.F,v 1.1.1.1 1996/02/15 17:47:49 mclareni Exp $
*
* $Log: word.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:49  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      CHARACTER*(*) FUNCTION WORD(STR,IW)
C
C CERN PROGLIB# M433    WORD            .VERSION KERNBIT  1.07  910903
C Author: M.Marquina 91/03/03
C
C-    Select word IW of string STR (word-delimiter CHSEP)
C
      CHARACTER*(*) STR
#include "m433cs.inc"
      LSTR=LENOCC(STR)
      WORD=' '
      IF(IW.LT.1) RETURN
C*>   Strip leading characters
      I0=1
      DO WHILE(STR(I0:I0).EQ.CHSEP.AND.I0.LT.LSTR)
        I0=I0+1
      ENDDO
      IF(I0.EQ.LSTR) RETURN
      NW=1
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
      IF(IE.GE.IS) WORD=STR(IS:IE)
      RETURN
      END
