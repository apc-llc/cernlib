*
* $Id: intq.F,v 1.1.1.1 1996/04/01 15:03:02 mclareni Exp $
*
* $Log: intq.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:02  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
#if defined(CERNLIB_CDC)
      FUNCTION INTQ(IN,ID,IR)
C   INTQ=[IN/ID] WITH REMAINDER IR
C   COMPUTED WITHOUT USING DIVISION OR MULTIPLICATION IN ORDER
C   TO PRESERVE MAXIMUM INTEGER CAPACITY, I.E. 2**59-1
C   CDC ONLY
C   T LINDELOF DECEMBER 1977
      IF(ID.EQ.0) GO TO 10
      INTQ=0
      IR=0
      IF(IN.EQ.0) RETURN
      ISIG=1
      IF(IN.LT.0) ISIG=-ISIG
      IF(ID.LT.0) ISIG=-ISIG
      M=0
      K=ABS(IN)
      KN=ABS(ID)
    1 IF(KN.GT.K) GO TO 2
      KN=SHIFT(KN,1)
      M=M+1
      IF(KN.GT.0) GO TO 1
    2 IF(M.EQ.0) GO TO 4
      DO 3 J=1,M
      N=0
      KN=SHIFT(KN,-1)
      IF(J.EQ.1) CALL SBIT0(KN,60)
    6 IF(K.LT.KN) GO TO 5
      K=K-KN
      N=N+1
      GO TO 6
    5 N=SHIFT(N,M-J)
      INTQ=INTQ+N
    3 CONTINUE
    4 IR=SIGN(K,IN)
      INTQ=INTP(ISIG,INTQ)
      RETURN
   10 WRITE(6,100)
  100 FORMAT('0*** ERROR IN INTQ (A105) *** ZERO DENOMINATOR')
      STOP
      END
#endif
