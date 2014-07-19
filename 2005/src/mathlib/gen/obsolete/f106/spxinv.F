*
* $Id: spxinv.F,v 1.1.1.1 1996/04/01 15:03:15 mclareni Exp $
*
* $Log: spxinv.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:15  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE SPXINV (A,NDIM,IFAIL)
C
      DIMENSION A(2),INDEX(100),RI(100)
C
      DATA  TOL / 1.E-12/
C
      IFAIL=0
      N=NDIM
      NM1=N-1
         DO 10 I=1,N
   10 INDEX(I)=1
C
         DO 80 I=1,N
C
C--                FIND PIVOT
      NPD=N
      PIVOT=0.
      JJ=1
         DO 20 J=1,N
      IF (INDEX(J).EQ.0) GO TO 19
      ELM=ABS (A(JJ))
      IF (ELM.LE.PIVOT) GO TO 19
      PIVOT=ELM
      K=J
      KK=JJ
   19 JJ=JJ+NPD
      NPD=NPD-1
   20 CONTINUE
      IF (PIVOT/ABS(A(1)).LT.TOL) GO TO 100
      INDEX(K)=0
      PIVOT=-A(KK)
C
C--                ELIMINATION
      NP=NM1
      KJ=K
      NM=1
C
         DO 70 J=1,N
      IF (J-K) 34,30,34
C
   30 A(KJ)=1./PIVOT
      RI(J)=0.
      NM=0
      NP=1
      GO TO 65
C
   34 ELM=-A(KJ)
C*UL   40 RI(J)=ELM/PIVOT
      RI(J)=ELM/PIVOT
      IF (ELM.EQ.0.) GO TO 50
C
      NPC=NM1
      JL=J
         DO 45 L=1,J
      A(JL)=A(JL)+ELM*RI(L)
      JL=JL+NPC
      NPC=NPC-1
   45 CONTINUE
C
   50 A(KJ)=RI(J)
C
   65 KJ=KJ+NP
   70 NP=NP-NM
C
   80 CONTINUE
C
C--                CHANGE THE SIGN
      N=(N*N+N)/2
         DO 90 IJ=1,N
   90 A(IJ)=-A(IJ)
      RETURN
C
C--                FAILURE RETURN
  100 IFAIL=1
      RETURN
      END
