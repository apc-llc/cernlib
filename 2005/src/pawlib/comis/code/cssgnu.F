*
* $Id: cssgnu.F,v 1.1.1.1 1996/02/26 17:16:22 mclareni Exp $
*
* $Log: cssgnu.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:22  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 17/02/94  18.16.33  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSSGNU(JJ,NN,RV)
      INTEGER CSNUMB
      DOUBLE PRECISION DV
      LOGICAL MINUS
#include "comis/csichv.inc"
*      DATA NHMINU/4H-   /, NHPLUS/4H+   /
      J=JJ
      N=NN
      MINUS=.FALSE.
      CSSGNU=0
      KCH=MKBLAN(J,N)
      IF(KCH.EQ.ICHMINU)THEN
        J=J+1
        N=N-1
        KCH=MKBLAN(J,N)
        MINUS=.TRUE.
      ELSEIF(KCH.EQ.ICHPLUS)THEN
        J=J+1
        N=N-1
        KCH=MKBLAN(J,N)
      END IF
      IF(N.LE.0)RETURN
      I=CSNUMB(J,N,IV,RV,DV)
      CSSGNU=I
      IF(I.EQ.0)THEN
        RETURN
      ELSE IF(I.LT.0)THEN
        JJ=J
        NN=N
        RETURN
      ELSE IF(I.EQ.1)THEN
        RV=IV
      ELSE IF(I.EQ.5)THEN
        RV=DV
      END IF
      JJ=J
      NN=N
      IF(MINUS)RV=-RV
      END
