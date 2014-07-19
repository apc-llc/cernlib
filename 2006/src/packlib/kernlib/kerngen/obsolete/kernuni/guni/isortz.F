*
* $Id: isortz.F,v 1.1.1.1 1996/02/15 17:53:56 mclareni Exp $
*
* $Log: isortz.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:56  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
C                                       LAST MODIF.
C                                       FROM COPENHAGEN  3/05/72
      SUBROUTINE ISORTZ(IA,I,N)
      DIMENSION IA(2),I(2)
      COMMON /SORT/ NSUB,NN1(70),NN2(70)
C     FIND LARGEST NUMBER IF POSITIVE
      MAX=0
      DO 1 L=1,N
      K=I(L)
      IF(IA(K)) 1,1,2
    2 IF(IA(K)-MAX) 1,1,3
    3 MAX=IA(K)
    1 CONTINUE
      MAX=MAX+1
      N2=0
      DO 30 J=1,NSUB
      N1=N2+1
      NN1(J)=N1
      N2=NN2(J)
      DO 10 L=N1,N2
      IMAX=MAX
      DO 11 M=N1,N2
      IF=FLD(0,2,I(M))
      IF(IF) 7,7,11
    7 IM=FLD(19,17,I(M))
      IF(IA(IM)-IMAX) 8,11
    8 IIM=IM
      IIF=M
      IMAX=IA(IM)
   11 CONTINUE
      FLD(2,17,I(L))=IIM
      FLD(0,2,I(IIF))=1
   10 CONTINUE
      DO 5 K=N1,N2
    5 I(K)=FLD(2,17,I(K))
   30 CONTINUE
C       MERGE SUBSETS
      DO 40 K=1,N
      IMAX=MAX
      DO 41 J=1,NSUB
      N1=NN1(J)
      IF(N1-NN2(J)) 42,42,41
   42 KT=FLD(19,17,I(N1))
      IF(IA(KT)-IMAX) 44,41
   44 IMAX=IA(KT)
      JJ=J
      KTT=KT
   41 CONTINUE
      NN1(JJ)=NN1(JJ)+1
      FLD(2,17,I(K))=KTT
   40 CONTINUE
      RETURN
      END
