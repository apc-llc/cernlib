*
* $Id: mdexp.F,v 1.1.1.1 1996/02/26 17:16:56 mclareni Exp $
*
* $Log: mdexp.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:56  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      LOGICAL FUNCTION MDEXP(LENG,IND)
      LOGICAL MDEXT
      COMMON /MDPOOL/ IQ(6)
      J=1
      K=1
      IF(MOD(LENG,2).EQ.1) LENG=LENG+1
    1 IF(IQ(K+1) .GE. LENG) GO TO 8
      IF(IQ(K)) 3,3,2
    2 J=K
      K=IQ(K)
      GO TO 1
    3 L1=LENG+4
      IF(MDEXT(L1,IN)) GO TO 4
      IND=0
      MDEXP=.FALSE.
      RETURN
    4 IF(IN.NE.(K+IQ(J+1)+2)) GO TO 5
      K=J
      IQ(K+1)=IQ(K+1)+L1
      GO TO 8
    5 IF(IN-IN/2*2) 6,6,7
    6 IN=IN+1
      L1=L1-1
    7 IQ(K)=IN
      IQ(K+1)=L1-2
      IQ(IN)=0
      IQ(IN+1)=0
    8 I=IQ(K)
      N=IQ(K+1)-LENG
      IF(IQ(I)) 10,10,9
    9 IF(N.GE.50) GO TO 10
      IND=I
      LENG=IQ(K+1)
      IQ(K)=IQ(I)
      IQ(K+1)=IQ(I+1)
      GO TO 13
   10 IND=I
      M=I+LENG
      IF(M-M/2*2) 11,11,12
   11 M=M+1
      N=N-1
   12 IQ(M)=IQ(I)
      IQ(M+1)=IQ(I+1)
      IQ(K)=M
      IQ(K+1)=N
   13 IF(IQ(J+1) .GE. 50) GO TO 14
      IQ(J)=IQ(K)
      IQ(J+1)=IQ(K+1)
   14 MDEXP=.TRUE.
      RETURN
      END
