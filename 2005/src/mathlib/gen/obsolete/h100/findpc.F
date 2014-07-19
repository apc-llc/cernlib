*
* $Id: findpc.F,v 1.1.1.1 1996/04/01 15:03:16 mclareni Exp $
*
* $Log: findpc.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:16  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE  FINDPC  (A, B, C, Z0, M, N, MD, LR, LC, LCQ, IROW,
     1                     ICOL, IP, KP, KK, Q1, Q2, EPS, IRC)
C  FINDPC FINDS THE PIVOTCOLUMN WHEREBY DEGENERACY IS TAKEN INTO ACCOUNT
C  =====================================================================
C*NS  DOUBLE PRECISION  A, B, C, Z0, MAXD, MIND, Q, QQ, Q1, Q2
      DOUBLE PRECISION  A, B, C, Z0,       MIND, Q,     Q1, Q2
      DIMENSION         A(MD,1), B(1), C(1), Q1(1), Q2(1)
      INTEGER           LR(1), LC(1), LCQ(1), IROW(1), ICOL(1)
C     THE COLUMNS WITH NONNEGATIVE C(K) AND POSITIVE A(IP,K) ARE ASSIGNE
C     WITH LCQ = 1
      MN = M + N
      DO 1  K = 1, N
    1 LCQ(K) = 0
      DO 5  K = 1, N
      IF(LC(K) .NE. IRC)  GOTO 5
      IF(K .EQ. KK)  GOTO 5
      IF(ABS(SNGL(A(IP,K))) .LT. EPS)  A(IP,K) = 0.
      IF(ABS(SNGL(C(K))) .LT. EPS)  C(K) = 0.
      IF(A(IP,K) .LE. 0.  .OR.  C(K) .LT. 0.)  GOTO 5
      LCQ(K) = 1
      Q = C(K)/A(IP,K)
      IF(C(K) .EQ. 0.)  Q = 0.
    5 CONTINUE
      KP = 0
      DO 6  K = 1, N
    6 KP = KP + LCQ(K)
      IF(KP .EQ. 0)  RETURN
C     KP = 0  MEANS THAT NO COLUMN COULD BE FOUND
C     IN THE NEXT SECTION ONLY THE COLUMNS WITH MINIMUM QUOTIENT ARE
C     ASSIGNED WITH LCQ = 1
      MIND = Q
      DO 11  J = 1, 2
      DO 11  K = 1, N
      IF(LCQ(K) .EQ. 0)  GOTO 11
      Q = C(K)/A(IP,K)
      IF(C(K) .EQ. 0.)  Q = 0.
      IF(Q .GT. MIND)  GOTO 10
      MIND = Q
      KP = K
      GOTO 11
   10 LCQ(K) = 0
   11 CONTINUE
   12 IND = 0
      DO 15  K = 1, N
   15 IND = IND + LCQ(K)
      IF(IND .EQ. 1)  RETURN
C     IN THE NEXT SECTION TWO OF THE POSSIBLE COLUMNS ARE PICKED OUT
      KP1 = 0
      DO 20  K = 1, N
      IF(LCQ(K) .EQ. 0)  GOTO 20
      IF(KP1 .EQ. 0)  KP1 = K
      KP2 = K
   20 CONTINUE
C     HERE IT IS DETERMINED WHICH OF TWO COLUMNS WITH EQUAL QUOTIENT
C     QUALIFIES AS PIVOTCOLUMN
      DO 25  J = 1, MN
      Q1(J) = 0.
   25 Q2(J) = 0.
      J = ICOL(KP1)
      Q1(J) = 1.
      J = ICOL(KP2)
      Q2(J) = 1.
      DO 30  I = 1, M
      IF(LR(I) .NE. IRC)  GOTO 30
      J = IROW(I)
      IF(ABS(SNGL(A(I,KP1))) .LT. EPS)  A(I,KP1) = 0.
      IF(ABS(SNGL(A(I,KP2))) .LT. EPS)  A(I,KP2) = 0.
      Q1(J) = A(I,KP1)
      Q2(J) = A(I,KP2)
   30 CONTINUE
      DO 35  J = 1, MN
      IF(J .NE. IROW(IP))  GOTO 31
      Q1(J) = 1.
      Q2(J) = 1.
      GOTO 35
   31 IF(Q1(J) .NE. 0.)  Q1(J) = Q1(J)/A(IP,KP1)
      IF(Q2(J) .NE. 0.)  Q2(J) = Q2(J)/A(IP,KP2)
   35 CONTINUE
      DO 50  J = 1, MN
      IF(Q1(J) .EQ. Q2(J))  GOTO 50
      IF(Q1(J) .GT. Q2(J))  GOTO 40
      LCQ(KP2) = 0
      KP = KP1
      GOTO 12
   40 LCQ(KP1) = 0
      KP = KP2
      GOTO 12
   50 CONTINUE
      RETURN
      END
