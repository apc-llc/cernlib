*
* $Id: sortzv.F,v 1.1.1.1 1996/02/15 17:53:58 mclareni Exp $
*
* $Log: sortzv.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:58  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
C                                       LAST MODIF.
C                                       FROM COPENHAGEN  3/05/72
      SUBROUTINE SORTZV(A,I,N,MODE,IWAY,ISEQ)
      DIMENSION A(2),I(2)
      COMMON /SORT/ NSUB,NN1(70),NN2(70)
      DATA BMAX /1.0E38/
      IF(N.LT.2) RETURN
C      SPLIT UP INTO SUBSETS
      AN=N
      AN=SQRT(AN/2.)
      NSUB=AN
      IF(N.LE.10) NSUB=1
      IF(NSUB.GT.70) NSUB=70
C       SET UP LIMITS
      LEN=N/NSUB
      NN2(1)=LEN
      IF(NSUB.EQ.1) GO TO 20
      DO 55 L=2,NSUB
   55 NN2(L)=NN2(L-1)+LEN
   20 NN2(NSUB)=N
      IF(ISEQ) 1,2,1
    2 DO 3 L=1,N
    3 I(L)=L
    1 IF(MODE)  4,555,6
  555 CALL ASORTZ(A,I,N)
      GO TO 100
    4 CALL ISORTZ(A,I,N)
      GO TO 100
C        SORT FLOATING
    6 CONTINUE
      N2=0
      DO 30 J=1,NSUB
      N1=N2+1
      NN1(J)=N1
      N2=NN2(J)
      DO 10 L=N1,N2
      AMAX=BMAX
      DO 11 M=N1,N2
      IF=(FLD(0,2,I(M))
      IF(IF) 7,7,11
    7 IM=FLD(19,17,I(M))
      IF(A(IM)-AMAX) 8,11,11
    8 IIM=IM
      IIF=M
      AMAX=A(IM)
   11 CONTINUE
      FLD(2,17,I(L))=IIM
      FLD(0,2,I(IIF))=1
   10 CONTINUE
      DO 5 K=N1,N2
    5 I(K)=FLD(2,17,I(K))
   30 CONTINUE
C      MERGE SUBSETS
      DO 40 K=1,N
      AMAX=BMAX
      DO 41 J=1,NSUB
      N1=NN1(J)
      IF(N1-NN2(J)) 42,42,41
   42 KT=FLD(19,17,I(N1))
      IF(A(KT)-AMAX) 44,41
   44 AMAX=A(KT)
      JJ=J
      KTT=KT
   41 CONTINUE
      NN1(JJ)=NN1(JJ)+1
      FLD(2,17,I(K))=KTT
   40 CONTINUE
C        REMOVE FLAGS AND FOLD
  100 DO 12 L=1,N
      IRES=FLD(2,17,I(L))
   12 I(L)=IRES
C        ORDER IWAY
      IF(IWAY) 13,14,13
   13 NN=N/2
      DO 15 L=1,NN
      LL=N-L+1
      II=I(L)
      I(L)=I(LL)
   15 I(LL)=II
   14 RETURN
      END
#ifdef CERNLIB_TCGEN_SORTZV
#undef CERNLIB_TCGEN_SORTZV
#endif
