*
* $Id: mneig.F,v 1.1.1.1 1996/03/07 14:31:29 mclareni Exp $
*
* $Log: mneig.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:29  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNEIG(A,NDIMA,N,MITS,WORK,PRECIS,IFAULT)
#include "minuit/d506dp.inc"
C
      PARAMETER (ZERO=0.0,  ONE=1.0,   TWO=2.0)
      PARAMETER (TOL=1.0E-35)
      DIMENSION A(NDIMA,*),WORK(*)
C          PRECIS is the machine precision EPSMAC
      IFAULT = 1
C
      I = N
      DO 70 I1 = 2,N
      L = I-2
      F = A(I,I-1)
      GL = ZERO
C
      IF(L .LT. 1) GO TO 25
C
      DO 20 K = 1,L
   20 GL = GL+A(I,K)**2
   25 H = GL + F**2
C
      IF(GL .GT. TOL) GO TO 30
C
      WORK(I) = ZERO
      WORK(N+I) = F
      GO TO 65
   30 L = L+1
C
      GL = SQRT(H)
C
      IF(F .GE. ZERO) GL = -GL
C
      WORK(N+I) = GL
      H = H-F*GL
      A(I,I-1) = F-GL
      F = ZERO
      DO 50 J = 1,L
      A(J,I) = A(I,J)/H
      GL = ZERO
      DO 40 K = 1,J
   40 GL = GL+A(J,K)*A(I,K)
C
      IF(J .GE. L) GO TO 47
C
      J1 = J+1
      DO 45 K = J1,L
   45 GL = GL+A(K,J)*A(I,K)
   47 WORK(N+J) = GL/H
      F = F+GL*A(J,I)
   50 CONTINUE
      HH = F/(H+H)
      DO 60 J = 1,L
      F = A(I,J)
      GL = WORK(N+J)-HH*F
      WORK(N+J) = GL
      DO 60 K = 1,J
      A(J,K) = A(J,K)-F*WORK(N+K)-GL*A(I,K)
   60 CONTINUE
      WORK(I) = H
   65 I = I-1
   70 CONTINUE
      WORK(1) = ZERO
      WORK(N+1) = ZERO
      DO 110 I = 1,N
      L = I-1
C
      IF(WORK(I) .EQ. ZERO .OR. L .EQ. 0) GO TO 100
C
      DO 90 J = 1,L
      GL = ZERO
      DO 80 K = 1,L
   80 GL = GL+A(I,K)*A(K,J)
      DO 90 K = 1,L
      A(K,J) = A(K,J)-GL*A(K,I)
   90 CONTINUE
  100 WORK(I) = A(I,I)
      A(I,I) = ONE
C
      IF(L .EQ. 0) GO TO 110
C
      DO 105 J = 1,L
      A(I,J) = ZERO
      A(J,I) = ZERO
  105 CONTINUE
  110 CONTINUE
C
C
      N1 = N-1
      DO 130 I = 2,N
      I0 = N+I-1
  130 WORK(I0) = WORK(I0+1)
      WORK(N+N) = ZERO
      B = ZERO
      F = ZERO
      DO 210 L = 1,N
      J = 0
      H = PRECIS*(ABS(WORK(L))+ABS(WORK(N+L)))
C
      IF(B .LT. H) B = H
C
      DO 140 M1 = L,N
      M = M1
C
      IF(ABS(WORK(N+M)) .LE. B) GO TO 150
C
  140 CONTINUE
C
  150 IF(M .EQ. L) GO TO 205
C
  160 IF(J .EQ. MITS) RETURN
C
      J = J+1
      PT = (WORK(L+1)-WORK(L))/(TWO*WORK(N+L))
      R = SQRT(PT*PT+ONE)
      PR = PT+R
C
      IF(PT .LT. ZERO) PR=PT-R
C
      H = WORK(L)-WORK(N+L)/PR
      DO 170 I=L,N
  170 WORK(I) = WORK(I)-H
      F = F+H
      PT = WORK(M)
      C = ONE
      S = ZERO
      M1 = M-1
      I = M
      DO 200 I1 = L,M1
      J = I
      I = I-1
      GL = C*WORK(N+I)
      H = C*PT
C
      IF(ABS(PT) .GE. ABS(WORK(N+I))) GO TO 180
C
      C = PT/WORK(N+I)
      R = SQRT(C*C+ONE)
      WORK(N+J) = S*WORK(N+I)*R
      S = ONE/R
      C = C/R
      GO TO 190
  180 C = WORK(N+I)/PT
      R = SQRT(C*C+ONE)
      WORK(N+J) = S*PT*R
      S = C/R
      C = ONE/R
  190 PT = C*WORK(I)-S*GL
      WORK(J) = H+S*(C*GL+S*WORK(I))
      DO 200 K = 1,N
      H = A(K,J)
      A(K,J) = S*A(K,I)+C*H
      A(K,I) = C*A(K,I)-S*H
  200 CONTINUE
      WORK(N+L) = S*PT
      WORK(L) = C*PT
C
      IF(ABS(WORK(N+L)) .GT. B) GO TO 160
C
  205 WORK(L) = WORK(L)+F
  210 CONTINUE
      DO 240 I=1,N1
      K = I
      PT = WORK(I)
      I1 = I+1
      DO 220 J = I1,N
C
      IF(WORK(J) .GE. PT) GO TO 220
C
      K = J
      PT = WORK(J)
  220 CONTINUE
C
      IF(K .EQ. I) GO TO 240
C
      WORK(K) = WORK(I)
      WORK(I) = PT
      DO 230 J=1,N
      PT = A(J,I)
      A(J,I) = A(J,K)
      A(J,K) = PT
  230 CONTINUE
  240 CONTINUE
      IFAULT = 0
C
      RETURN
      END
