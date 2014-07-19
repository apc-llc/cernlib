*
* $Id: mdlh.F,v 1.1.1.1 1996/02/26 17:16:56 mclareni Exp $
*
* $Log: mdlh.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:56  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.01  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MDLH(IND,LENGTH)
      LOGICAL MDEXP
      COMMON /MDPOOL/ IQ(6)
      IF(IQ(IND).EQ.0) CALL MDHINI(IND,0)
      NSTEP=0
      LAV=IND+8
      N=LENGTH+2
      IF(N.LT.4) N=4
    1 LR=IQ(IND+11)
      K=LR
      M=0
    2 IF(K.NE.LAV) GO TO 3
      IF(M.NE.0) GO TO 7
      K=IQ(IND+9)
      M=1
      GO TO 2
    3 L=-IQ(K)
      NSTEP=NSTEP+1
      IF(L.GE.N) GO TO 4
      K=IQ(K+1)
      GO TO 2
    4 LR=IQ(K+1)
      IQ(IND+11)=LR
      I=L-N
      IF(I.LE.4) GO TO 5
C  PART OF BLOCK:
      J=K+I
      IQ(K)=-I
      IQ(J-1)=-I
      IQ(J)=N
      GO TO 6
C  ENTIER BLOCK:
    5 J=IQ(K+2)
      IQ(J+1)=LR
      IQ(LR+2)=J
      J=K
      IQ(K)=L
    6 IQ(K+L-1)=0
      MDLH=J+1
      IF(IQ(IND).EQ.-1) RETURN
      IQ(IND+12)=IQ(IND+12)+1
      IQ(IND+13)=IQ(IND+13)+NSTEP
      RETURN
C  DEADLOCK:
    7 LP=N+4
      JP=IQ(IND+4)
   30 CONTINUE
      IF(MDEXP(LP,IP)) GO TO 8
      CALL MDPAUS('?MDLH: insufficient memory')
      GO TO 30
    8 IQ(IP)=0
      M=IP+LP-1
      IQ(M)=0
      IQ(IP+2)=LP
      ND=IP+3
    9 IQ(IP+1)=M
      IQ(ND)=LP-4
      ND=ND+1
      IF(IP.EQ.(IND+IQ(IND+3))) GO TO 14
      I1=IND+7
   10 I=IQ(I1)
      IF(I) 11,19,11
   11 IF(IP.EQ.(I+IQ(I+2))) GO TO 15
      I1=I
      GO TO 10
   14 ND=IQ(IND+6)
      IQ(IND+6)=M
      IQ(IND+3)=IQ(IND+3)+LP
      GO TO 16
   15 ND=IQ(I+1)
      IQ(I+1)=M
      IQ(I+2)=IQ(I+2)+LP
   16 IQ(ND)=LP
      ND=ND+1
      GO TO 20
   19 IQ(I1)=IP
   20 CALL MDFH(IND,ND)
      IQ(IND+11)=IQ(IND+9)
      GO TO 1
      END
