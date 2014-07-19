*
* $Id: lhxnmb.F,v 1.1.1.1 1996/02/26 17:16:44 mclareni Exp $
*
* $Log: lhxnmb.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:44  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.06  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION LHXNMB(J,N,IV)
      CHARACTER *20 TS,C*1
      LHXNMB=0
      IF(N.LE.0)RETURN
      L=MIN0(20,N)
      CALL CCOPYS(J,%LOC(TS),L)
      IV=0
      I=0
 1    I=I+1
      K=ICHAR(TS(I:I))
      C=TS(I:I)
      IF(C.GE.'0' .AND. C.LE.'9')THEN
        K=K-48
      ELSEIF(C.GE.'A' .AND. C.LE.'F')THEN
        K=K-55
      ELSE
        IF(I.GT.1)LHXNMB=1
        J=J+I-1
        N=N-I+1
        RETURN
      ENDIF
      IV=IV*16+K
      GO TO 1
      END
