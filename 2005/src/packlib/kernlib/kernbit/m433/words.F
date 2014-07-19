*
* $Id: words.F,v 1.1.1.1 1996/02/15 17:47:49 mclareni Exp $
*
* $Log: words.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:49  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      INTEGER FUNCTION WORDS(STR)
C
C CERN PROGLIB# M433    WORDS           .VERSION KERNBIT  1.07  910903
C Author: M.Marquina 91/03/03
C
C-    Return number of words in string STR
C
      CHARACTER*(*) STR
#include "m433cs.inc"
      LSTR=LENOCC(STR)
      WORDS=0
C*>   Strip leading characters
      I0=1
      DO WHILE(STR(I0:I0).EQ.CHSEP.AND.I0.LT.LSTR)
        I0=I0+1
      ENDDO
      IF(I0.EQ.LSTR) RETURN
      JW=1
      DO 10 N=I0,LSTR
        IF(STR(N:N).NE.CHSEP)            GOTO 10
        IF(STR(N-1:N-1).NE.CHSEP) JW=JW+1
 10   CONTINUE
      WORDS=JW
      RETURN
      END
