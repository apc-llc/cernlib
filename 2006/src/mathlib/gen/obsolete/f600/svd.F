*
* $Id: svd.F,v 1.1.1.1 1996/04/01 15:03:15 mclareni Exp $
*
* $Log: svd.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:15  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE SVD (A, S, V, MMAX, NMAX, M, N, P, WITHU, WITHV)
C     ---------------------  START OF SVD  -----------------------------
C
      REAL       A(MMAX,1), S(NMAX), V(NMAX,NMAX)
      INTEGER    M, N, P
      LOGICAL    WITHU, WITHV
C
C     ADDITIONAL SUBROUTINE NEEDED: ROTATE
C
C     THIS SUBROUTINE COMPUTES THE SINGULAR VALUE DECOMPOSITION
C     OF A REAL M*N MATRIX A, I.E. IT COMPUTES MATRICES U, S, AND V
C     SUCH THAT
C                  A = U * S * VT ,
C     WHERE
C              U IS AN M*N MATRIX AND UT*U = I, (UT=TRANSPOSE
C                                                    OF U),
C              V IS AN N*N MATRIX AND VT*V = I, (VT=TRANSPOSE
C                                                    OF V),
C        AND   S IS AN N*N DIAGONAL MATRIX.
C
C     DESCRIPTION OF PARAMETERS:
C     A = REAL ARRAY. A CONTAINS THE MATRIX TO BE DECOMPOSED.
C         THE ORIGINAL DATA ARE LOST. IF WITHU=.TRUE., THEN
C         THE MATRIX U IS COMPUTED AND STORED IN THE ARRAY A.
C     MMAX = INTEGER VARIABLE. THE NUMBER OF ROWS IN THE
C            ARRAY A.
C     NMAX = INTEGER VARIABLE. THE NUMBER OF ROWS IN THE
C            ARRAY V.
C     M,N = INTEGER VARIABLES. THE NUMBER OF ROWS AND COLUMNS
C           IN THE MATRIX STORED IN A. (N<=M<=100. IF IT IS
C           NECESSARY TO SOLVE A LARGER PROBLEM, THEN THE
C           AMOUNT OF STORAGE ALLOCATED TO THE ARRAY T MUST
C           BE INCREASED ACCORDINGLY.) IF M<N, THEN EITHER
C           TRANSPOSE THE MATRIX A OR ADD ROWS OF ZEROS TO
C           INCREASE M TO N.
C     P = INTEGER VARIABLE. IF P>0, THEN COLUMNS N+1, . . . ,
C         N+P OF A ARE ASSUMED TO CONTAIN THE COLUMNS OF AN M*P
C         MATRIX B. THIS MATRIX IS MULTIPLIED BY UT, AND UPON
C         EXIT, A CONTAINS IN THESE SAME COLUMNS THE N*P MATRIX
C         UT*B. (P-=0)
C     WITHU, WITHV = LOGICAL VARIABLES. IF WITHU=.TRUE., THEN
C         THE MATRIX U IS COMPUTED AND STORED IN THE ARRAY A.
C         IF WITHV=.TRUE., THEN THE MATRIX V IS COMPUTED AND
C         STORED IN THE ARRAY V.
C     S = REAL ARRAY.  S(1), . . . , S(N) CONTAIN THE DIAGONAL
C         ELEMENTS OF THE MATRIX S ORDERED SO THAN S(I)>=S(I+1),
C         I=1, . . . , N-1.
C     V = REAL ARRAY.  V CONTAINS THE MATRIX V.  IF WITHU
C         AND WITHV ARE NOT BOTH =.TRUE., THEN THE ACTUAL
C         PARAMETER CORRESPONDING TO A AND V MAY BE THE SAME.
C
C     THIS SUBROUTINE IS A REAL VERSION OF A FORTRAN SUBROUTINE
C     BY BUSINGER AND GOLUB, ALGORITHM 358:  SINGULAR VALUE
C     DECOMPOSITION OF A COMPLEX MATRIX, COMM. ACM, V. 12,
C     NO. 10, PP. 564-565 (OCT. 1969).
C     WITH REVISIONS BY RC SINGLETON, MAY 1972.
C     ------------------------------------------------------------------
      DIMENSION  T(100)
C
#if defined(CERNLIB_CDC)
      DATA ETA,TOL/1.E-14,1.E-279/
#endif
#if defined(CERNLIB_IBM)
      DATA ETA,TOL/1.E-6,1.E-72/
#endif
#if (!defined(CERNLIB_CDC))&&(!defined(CERNLIB_IBM))
      DATA ETA,TOL / 1.E-6,1.E-37/
#endif
C     ETA AND TOL ARE MACHINE DEPENDENT CONSTANTS
C     ETA IS THE MACHINE EPSILON (RELATIVE ACCURACY);
C     TOL IS THE SMALLEST REPRESENTABLE REAL DIVIDED BY ETA.
C
      NP = N + P
      N1 = N + 1
C
C     HOUSEHOLDER REDUCTION TO BIDIAGONAL FORM
      G = 0.0
      EPS = 0.0
      L = 1
   10 T(L) = G
      K = L
      L = L + 1
C
C     ELIMINATION OF A(I,K), I=K+1, . . . , M
      S(K) = 0.0
      Z = 0.0
      DO 20 I = K,M
   20    Z = Z + A(I,K)**2
      IF (Z.LT.TOL) GOTO 50
      G = SQRT(Z)
      F = A(K,K)
      IF (F.GE.0.0) G = - G
      S(K) = G
      H = G * (F - G)
      A(K,K) = F - G
      IF (K.EQ.NP) GOTO 50
      DO 40 J = L,NP
      F = 0
      DO 30 I = K,M
   30 F= F + A(I,K)*A(I,J)
      F = F/H
      DO 40 I = K,M
   40 A(I,J) = A(I,J) + F*A(I,K)
C
C     ELIMINATION OF A(K,J), J=K+2, . . . , N
   50 EPS = MAX(EPS,ABS(S(K)) + ABS(T(K)))
      IF (K.EQ.N) GOTO 100
      G = 0.0
      Z = 0.0
      DO 60 J = L,N
   60 Z = Z + A(K,J)**2
      IF (Z.LT.TOL) GOTO 10
      G = SQRT(Z)
      F = A (K,L)
      IF (F.GE.0.0) G = - G
      H = G * (F - G)
      A(K,L) = F - G
      DO 70 J = L,N
   70 T(J) = A(K,J)/H
      DO 90 I = L,M
      F = 0
      DO 80 J = L,N
   80 F = F + A(K,J)*A(I,J)
      DO 90 J = L,N
   90 A(I,J) = A(I,J) + F*T(J)
C
      GOTO 10
C
C     TOLERANCE FOR NEGLIGIBLE ELEMENTS
  100 EPS = EPS*ETA
C
C     ACCUMULATION OF TRANSFORMATIONS
      IF (.NOT.WITHV) GOTO 160
      K = N
      GOTO 140
  110 IF (T(L).EQ.0.0) GOTO 140
      H = A(K,L)*T(L)
      DO 130 J = L,N
      Q = 0
      DO 120 I = L,N
  120 Q = Q + A(K,I)*V(I,J)
      Q = Q/H
      DO 130 I = L,N
  130 V(I,J) = V(I,J) + Q*A(K,I)
  140 DO 150 J = 1,N
  150 V(K,J) = 0
      V(K,K) = 1.0
      L=K
      K = K - 1
      IF (K.NE.0) GOTO 110
C
  160 K = N
      IF (.NOT. WITHU) GO TO 230
      G = S (N)
      IF (G.NE.0.0) G = 1.0/G
      GO TO 210
  170 DO 180 J = L,N
  180 A(K,J) = 0
      G = S(K)
      IF (G.EQ.0.0) GOTO 210
      H = A(K,K)*G
      DO 200 J = L,N
      Q =0
      DO 190 I = L,M
  190 Q= Q + A(I,K)*A(I,J)
      Q = Q/H
      DO 200 I = K,M
  200 A(I,J) = A(I,J) + Q*A(I,K)
      G = 1.0/G
  210 DO 220 J = K,M
  220 A(J,K) = A(J,K)*G
      A(K,K) = A(K,K) + 1.0
      L = K
      K = K - 1
      IF (K.NE.0) GOTO 170
C
C     QR DIAGONALIZATION
      K = N
C
C     TEST FOR SPLIT
  230 L = K
  240 IF (ABS(T(L)).LE.EPS) GOTO 290
      L = L - 1
      IF (ABS(S(L)).GT.EPS) GOTO 240
C
C     CANCELLATION
      CS = 0.0
      SN = 1.0
      L1 = L
      L = L + 1
      DO 280 I = L,K
      F = SN*T(I)
      T(I) = CS*T(I)
      IF (ABS(F).LE.EPS) GOTO 290
      H = S(I)
      W = SQRT(F*F + H*H)
      S(I) = W
      CS = H/W
      SN = - F/W
      IF (WITHU) CALL ROTATE(A(1,L1),A(1,I), CS, SN, M)
      IF(NP.EQ.N) GO TO 280
      DO 270 J = N1,NP
      Q = A(L1,J)
      R = A(I,J)
      A(L1,J) = Q*CS + R*SN
  270 A(I,J) = R*CS - Q*SN
  280 CONTINUE
C
C     TEST FOR CONVERGENCE
  290 W = S(K)
      IF (L.EQ.K) GO TO 360
C
C     ORIGIN SHIFT
      X = S(L)
      Y = S(K-1)
      G = T(K-1)
      H = T(K)
      F = ((Y-W) * (Y+W) + (G-H) * (G+H)) / (2.0*H*Y)
      G=SQRT(F*F+1.0)
      IF (F.LT.0.0) G = - G
      F = ((X - W) * (X+W)+ (Y/(F+G) - H) *H)/X
C
C   QR STEP
      CS = 1.0
      SN = 1.0
      L1 = L + 1
      DO 350 I = L1,K
      G = T(I)
      Y = S(I)
      H = SN*G
      G = CS*G
      W = SQRT(H*H + F*F)
      T(I-1) = W
      CS = F/W
      SN = H/W
      F = X*CS + G*SN
      G = G*CS - X*SN
      H = Y*SN
      Y = Y*CS
      IF (WITHV) CALL ROTATE(V(1,I-1), V(1,I), CS, SN, N)
      W = SQRT(H*H + F*F)
      S(I-1) = W
      CS = F/W
      SN = H/W
      F = CS*G + SN*Y
      X = CS*Y - SN*G
      IF (WITHU) CALL ROTATE(A(1,I-1), A(1,I), CS, SN, M)
      IF (N.EQ.NP) GO TO 350
      DO 340 J = N1,NP
      Q = A(I-1,J)
      R = A(I,J)
      A(I-1,J) = Q*CS + R*SN
  340 A(I,J) = R*CS - Q*SN
  350 CONTINUE
C
      T(L) = 0.0
      T(K) = F
      S(K) = X
      GO TO 230
C
C   CONVERGENCE
  360 IF (W.GE.0.0) GO TO 380
      S(K) = - W
      IF (.NOT.WITHV) GO TO 380
      DO 370 J = 1,N
  370 V(J,K) = - V(J,K)
  380 K = K-1
      IF (K.NE.0) GO TO 230
C
C   SORT SINGULAR VALUES
      DO 450 K = 1,N
      G = -1.0
      DO 390 I = K,N
      IF (S(I).LT.G) GO TO 390
      G = S(I)
      J = I
  390 CONTINUE
      IF (J.EQ.K) GO TO 450
      S(J) = S(K)
      S(K) = G
      IF (.NOT.WITHV) GO TO 410
      DO 400 I = 1,N
      Q = V(I,J)
      V(I,J) = V(I,K)
  400 V(I,K) = Q
  410 IF (.NOT.WITHU) GO TO 430
      DO 420 I = 1,M
      Q = A(I,J)
      A(I,J) = A(I,K)
  420 A(I,K) = Q
  430 IF (N.EQ.NP) GO TO 450
      DO 440 I = N1,NP
      Q = A(J,I)
      A(J,I) = A(K,I)
  440 A(K,I) = Q
  450 CONTINUE
C
      RETURN
      END
