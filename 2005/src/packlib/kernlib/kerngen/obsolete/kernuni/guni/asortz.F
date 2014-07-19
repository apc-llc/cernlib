*
* $Id: asortz.F,v 1.1.1.1 1996/02/15 17:53:56 mclareni Exp $
*
* $Log: asortz.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:56  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
C                                       LAST MODIF.
C                                       FROM COPENHAGEN  3/05/72
      COMPILER (DATA=IBM),(FLD=ABS)
      SUBROUTINE ASORTZ(A,I,N)
      COMMON /SORT/ NSUB,NN1(70),NN2(70)
      DATA MAXMAX /O777777777777/
      DIMENSION A(2),I(2),IC(64)
      DATA (IC(J),J=1,32)
     */38,39,40,41,42,1,12,13,14,15,16,17,18,19,20,21,22,
     *23,24,25,26,27,28,29,30,31,32,33,34,35,36,37/
      DATA (IC(J),J=33,64)
     */43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,
     *2,3,4,5,6,7,8,9,10,11,59,60,61,62,63,64/
      N2=0
      DO 30 NN=1,NSUB
      N1=N2+1
      NN1(NN)=N1
      N2=NN2(NN)
      IP=36
      DO 1 L=1,6
      IP=IP-6
      DO 2 K=N1,N2
      MAX=100
      DO 4 M=N1,N2
      IF=FLD(0,2,I(M))
      IF(IF) 5,5,4
    5 J=FLD(19,17,I(M))
      ICC=FLD(IP,6,A(J))+1
      ICC=IC(ICC)
      IF(ICC-MAX) 6,4,4
    6 MAX=ICC
      JJ=J
      MM=M
    4 CONTINUE
      FLD(2,17,I(K))=JJ
      FLD(0,2,I(MM))=1
    2 CONTINUE
      DO 7 K=N1,N2
      ICC=FLD(2,17,I(K))
      FLD(19,17,I(K))=ICC
    7 FLD(0,2,I(K))=0
    1 CONTINUE
   30 CONTINUE
C      MERGE
      DO 40 K=1,N
      IMAX=MAXMAX
      DO 41 J=1,NSUB
      N1=NN1(J)
      IF(N1-NN2(J)) 42,42,41
   42 KT=FLD(19,17,I(N1))
      LMAX=MAXMAX
      IP=-6
      DO 51 L=1,6
      IP=IP+6
      ICC=FLD(IP,6,A(KT))+1
      ICC=IC(ICC)-1
   51 FLD(IP,6,LMAX)=IABS(ICC)
      IF(FLD(0,18,LMAX)-FLD(0,18,IMAX)) 44,45,41
   45 IF(FLD(18,18,LMAX)-FLD(18,18,IMAX)) 44,41,41
   44 IMAX=LMAX
      JJ=J
      KTT=KT
   41 CONTINUE
      NN1(JJ)=NN1(JJ)+1
      FLD(2,17,I(K))=KTT
   40 CONTINUE
      RETURN
      END
