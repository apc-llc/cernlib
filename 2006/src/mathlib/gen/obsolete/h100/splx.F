*
* $Id: splx.F,v 1.1.1.1 1996/04/01 15:03:16 mclareni Exp $
*
* $Log: splx.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:16  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE SPLX (A,B,C,Z0,M1,M,N1,N,MD,IROW,ICOL,LR,LC,LCQ,
     1                 Q1,Q2,MODE)
C  =====================================================================
C  ROUTINE FOR THE OPTIMALIZATION OF A LINEAR PROGRAM USING THE SIMPLEX
C  DEGENERACIES ARE TAKEN INTO ACCOUNT.
C  CONSTRAINTS MAY BE EITHER INEQUALITIES OR EQUALITIES.
C  THE VARIABLES MAY BE RESTRICTED OR UNRESTRICTED.
C  =====================================================================
C     MODE = 0   EXACTLY ONE FINITE MAXIMUM SOLUTION
C     MODE = 1   THE PROBLEM HAS NO FINITE SOLUTION (MAX = INFINITE)
C     MODE = 2   NO FEASIBLE INITIAL SOLUTION EXISTS
C     MODE = 3   THERE IS MORE THAN ONE FINITE MAXIMUM SOLUTION
      DOUBLE PRECISION  A, B, C, Z0, MAXD, MIND, Q, QQ, Q1, Q2
      DIMENSION         A(MD,1), B(1), C(1), Q1(1), Q2(1)
      INTEGER           IROW(1), ICOL(1), LR(1), LC(1), LCQ(1)
C  COMPUTATION OF THE EPSILON-VALUE
C  =====================================================================
      CALL EPSILO(A,M,N,MD,EPS)
C  INITIALIZATION OF THE INDEX LIST
C  =====================================================================
      M2 = M - M1
      N2 = N - N1
      IF(M1 .EQ. 0)  GOTO 12
      DO 10  I = 1, M1
      IROW(I) = I
   10 LR(I) = 0
      IF(M1 .EQ. M)  GOTO 15
   12 MM1 = M1 + 1
      DO 11  I = MM1, M
      IROW(I) = I
   11 LR(I) = 1
   15 IF(N1 .EQ. 0)  GOTO 20
      DO 16  K = 1, N1
      ICOL(K) = M + K
   16 LC(K) = 0
      IF(N1 .EQ. N)  GOTO 18
   20 NN1 = N1 + 1
      DO 17  K = NN1, N
      ICOL(K) = M + K
   17 LC(K) = -1
   18 DO 19  K = 1, N
   19 LCQ(K) = 0
C     LR (RESP. LC)  =  0    X .GE. 0.
C     LR (RESP. LC)  = -1    X .EQ. 0.
C     LR (RESP. LC)  =  1    X IS UNRESTRICTED
C     LR (RESP. LC)  =  2    X IS LINEAR DEPENDENT WITHOUT INFLUENCE ON
C                            THE OBJECTIVE FUNCTION
C     LR  = -2               X IS UNRESTRICTED AND CANNOT BE ELIMINATED
C  ELIMINATION OF THE EQUALITY-CONSTRAINTS OUT OF THE BASIS
C  =====================================================================
C*UL   50 IF(N1 .EQ. N)  GOTO 100
      IF(N1 .EQ. N)  GOTO 100
C     IF THERE ARE NO EQUALITY-CONSTRAINTS, THE SECTION UP TO 100 CAN
C     BE BYPASSED.
   55 KP = 0
      DO 60  K = 1, N
   60 IF(LC(K) .EQ. -1)  KP = K
      IF(KP .EQ. 0)  GOTO 80
C     IF KP = 0, NO MORE EQUALITY-CONSTRAINTS IN THE BASIS
      MAXD = 0.
      DO 61  I = 1, M
      IF(LR(I) .EQ. -1)  GOTO 61
      IF(ABS(A(I,KP)) .LT. MAXD)  GOTO 61
      MAXD = ABS(A(I,KP))
      IP = I
   61 CONTINUE
      IF(SNGL(MAXD) .LT. EPS)  MAXD = 0.
      IF(MAXD .NE. 0.)  GOTO 70
      IF(ABS(SNGL(C(KP))) .LT. EPS)  C(KP) = 0.
      IF(C(KP) .NE. 0.)  GOTO 65
      LC(KP) = 1
      GOTO 55
   65 MODE = 2
      RETURN
C     THE HOMOGENEOUS PART OF AT LEAST TWO EQUATIONS IS LINEAR DEPENDENT
C     WHEREAS THE INHOMOGENEOUS PART IS CONTRADICTORY.
   70 CALL PIVOT  (A, B, C, Z0, M, N, MD, IP, KP, IROW, ICOL, EPS)
      LC(KP) = LR(IP)
      LR(IP) = -1
      GOTO 55
C     CHECK WHETHER THE NONBASIC VARIABLES CONSIST ONLY OF EQUATION-VARI
   80 IND = 0
      DO 81  I = 1, M
   81 IF(LR(I) .NE. -1)  IND = IND + 1
      IF(IND .NE. 0)  GOTO 100
C     IF THE NONBASIC VARIABLES ARE ONLY EQUATION-VARIABLES, THERE IS NO
C     CHECKED, WHETHER NO CONSTRAINT IS VIOLATED
      MIND = 0.
      DO 85  K = 1, N
      IF(LC(K) .EQ. 1)  GOTO 85
      IF(ABS(SNGL(C(K))) .LT. EPS)  C(K) = 0.
      IF(C(K) .GE. 0.)  GOTO 85
      IF(C(K) .LT. MIND)  MIND = C(K)
   85 CONTINUE
      IF(MIND .LT. 0.)  GOTO 90
      MODE = 0
      RETURN
   90 MODE = 2
      RETURN
C  ELIMINATION OF THE UNRESTRICTED VARIABLES
C  =====================================================================
  100 IF(M1 .EQ. M)  GOTO 200
  101 IP = 0
      DO 105  I = 1, M
  105 IF(LR(I) .EQ. 1)  IP = I
      IF(IP .EQ. 0)  GOTO 200
C     IF THE PROBLEM CONTAINS NO FREE VARIABLES OR IF THEY HAVE ALREADY
C     BEEN EXCHANGED WITH EQUATIONS, THE FOLLOWING SECTION UP TO 200
C     CAN BE BYPASSED
C     CHECK IS NOW MADE WHETHER THE PRESENT TABLEAU REPRESENTS A
C     FEASIBLE INITIAL SOLUTION
      MAXD = 0.
      MIND = 0.
      DO 110  K = 1, N
      IF(LC(K) .EQ. 1)  GOTO 110
      IF(ABS(SNGL(C(K))) .LT. EPS)  C(K) = 0.
      IF(C(K) .LT. MIND)  MIND = C(K)
      IF(C(K) .GT. MAXD)  MAXD = C(K)
  110 CONTINUE
      IND = 0
      IF(MIND .GE. 0.)  IND = 1
      Q = MAXD
C     IF THE TABLEAU ALREADY REPRESENTS AN INITIAL SOLUTION (IND = 1),
C     THE ELIMINATION OF THE REMAINING UNRESTRICTED VARIABLES IS DONE
C     BY A SPECIAL ALGORITHM.
      MAXD = 0.
      MIND = 0.
      DO 112  K = 1, N
      IF(LC(K) .EQ. 1)  GOTO 112
      IF(ABS(SNGL(A(IP,K))) .LT. EPS)  A(IP,K) = 0.
      IF(A(IP,K) .LT. MAXD)  GOTO 111
      MAXD = A(IP,K)
      KPMAX = K
  111 IF(A(IP,K) .GT. MIND)  GOTO 112
      MIND = A(IP,K)
      KPMIN = K
  112 CONTINUE
      IF(MAXD .EQ. 0.  .AND.  MIND .EQ. 0.)  GOTO 130
      IF(IND .EQ. 0)  GOTO 120
      IF(ABS(SNGL(B(IP))) .LT. EPS)  B(IP) = 0.
      IF(B(IP) .LT. 0.  .AND.  MAXD .GT. 0.)  GOTO 140
      IF(B(IP) .GT. 0.  .AND.  MIND . LT. 0.)  GOTO 160
      IF(B(IP) .EQ. 0.)  GOTO 180
  120 IF(MAXD .GE. ABS(MIND))  KP = KPMAX
      IF(MAXD .LT. ABS(MIND))  KP = KPMIN
  125 CALL PIVOT  (A, B, C, Z0, M, N, MD, IP, KP, IROW, ICOL, EPS)
      LC(KP) = 1
      LR(IP) = 0
      GOTO 101
C     IF ALL THE PIVOTELEMENTS FOR THE FREE VARIABLE ARE ZERO, THIS
C     VARIABLE HAS TO REMAIN OUT OF THE BASIS. IT IS ASSIGNED WITH LR =
  130 LR(IP) = -2
      GOTO 101
  140 Q = Q/MAXD
      DO 150  K = 1, N
      IF(LC(K) .EQ. 1)  GOTO 150
      IF(A(IP,K) .LE. 0.)  GOTO 150
      QQ = C(K)/A(IP,K)
      IF(QQ .GT. Q)  GOTO 150
      Q = QQ
      KP = K
  150 CONTINUE
      GOTO 125
  160 Q = Q/MIND
      DO 170  K = 1, N
      IF(LC(K) .EQ. 1)  GOTO 170
      IF(A(IP,K) .GE. 0.)  GOTO 170
      QQ = C(K)/A(IP,K)
      IF(QQ .LT. Q)  GOTO 170
      Q = QQ
      KP = K
  170 CONTINUE
      GOTO 125
  180 Q = Q/MAX(MAXD,ABS(MIND))
      DO 190  K = 1, N
      IF(LC(K) .EQ. 1)  GOTO 190
      IF(A(IP,K) .EQ. 0.)  GOTO 190
      QQ = ABS(C(K)/A(IP,K))
      IF(QQ .GT. Q)  GOTO 190
      Q = QQ
      KP = K
  190 CONTINUE
      GOTO 125
C  COMPUTATION OF A FEASIBLE INITIAL SOLUTION USING THE MULTIPHASE-METHO
C  =====================================================================
  200 IF(M1 .EQ. M  .AND.  N1 .EQ. N)  GOTO 250
      KP = 0
      DO 201  K = 1, N
  201 IF(LC(K) .EQ. 0)  KP = KP + 1
      IF(KP .NE. 0)  GOTO 210
      IP = 0
      DO 202  I = 1, M
  202 IF(LR(I) .NE. -1)  IP = IP + 1
      IF(IP .NE. 0)  GOTO 205
      MODE = 0
      RETURN
  205 L1 = 0
      L2 = 0
      L3 = 0
      DO 206  I = 1, M
      IF(LR(I) .EQ. -1)  GOTO 206
      IF(ABS(SNGL(B(I))) .LT. EPS)  B(I) = 0.
      IF(LR(I) .EQ. 0  .AND.  B(I) .LT. 0.)  L3 = 1
      IF(LR(I) .EQ. 0  .AND.  B(I) .EQ. 0.)  L2 = 1
      IF(LR(I) .EQ. 0  .AND.  B(I) .GT. 0.)  L1 = 1
      IF(LR(I) .EQ. -2  .AND.  B(I) .NE. 0.)  L3 = 1
      IF(LR(I) .EQ. -2  .AND.  B(I) .EQ. 0.)  L2 = 1
  206 CONTINUE
      IF(L1 .EQ. 1)  MODE = 0
      IF(L2 .EQ. 1)  MODE = 3
      IF(L3 .EQ. 1)  MODE = 1
      RETURN
C     SINCE THERE ARE ONLY UNRESTRICTED VARIABLES IN THE BASIS, THE
C     FINAL TABLEAU HAS BEEN OBTAINED
  210 IP = 0
      DO 211  I = 1, M
  211 IF(LR(I) .EQ. 0)  IP = IP + 1
      IF(IP .NE. 0)  GOTO 250
      MIND = 0.
      DO 212  K = 1, N
      IF(LC(K) .EQ. 1)  GOTO 212
      IF(ABS(SNGL(C(K))) .LT. EPS)  C(K) = 0.
      IF(C(K) .LT. MIND)  MIND = C(K)
  212 CONTINUE
      IF(MIND .GE. 0.)  GOTO 215
      MODE = 2
      RETURN
  215 IP = 0
      DO 216  I = 1, M
  216 IF(LR(I) .EQ. -2)  IP = IP + 1
      IF(IP .NE. 0)  GOTO 220
      MODE = 0
      RETURN
  220 L1 = 0
      L2 = 0
      DO 225  I = 1, M
      IF(LR(I) .EQ. -1)  GOTO 225
      IF(ABS(SNGL(B(I))) .LT. EPS)  B(I) = 0.
      IF(B(I) .NE. 0.)  L1 = 1
      IF(B(I) .EQ. 0.)  L2 = 1
  225 CONTINUE
      IF(L2 .EQ. 1)  MODE = 3
      IF(L1 .EQ. 1)  MODE = 1
      RETURN
C     SINCE THE VARIABLES OUT OF THE BASIS ARE EITHER UNRESTRICTED OR
C     EQUAL TO ZERO, THEY CANNOT BE EXCHANGED. THE FINAL TABLEAU WAS
C     THEREFORE OBTAINED.
C     IN THE FOLLOWING SECTION THERE IS CHECKED WHETHER THE TABLEAU
C     ALREADY REPRESENTS AN INITIAL SOLUTION
  250 MIND = 0.
      DO 255  K = 1, N
      IF(LC(K) .EQ. 1)  GOTO 255
      IF(ABS(SNGL(C(K))) .LT. EPS)  C(K) = 0.
      IF(C(K) .GT. MIND)  GOTO 255
      MIND = C(K)
      KK = K
  255 CONTINUE
      IF(MIND .GE. 0.)  GOTO 500
C     IF ALL THE C(K) BELONGING TO CONSTRAINTS  X .GE. 0.  ARE NONNEGATI
C     AN INITIAL SOLUTION HAS BEEN FOUND.
C     ATHERWISE THE COLUMN WITH INDEX KK BECOMES NEW OBJECTIVE FUNCTION
C     WHICH HAS TO BE MAXIMIZED.
  270 MIND=0.
      DO 300  I = 1, M
      IF(LR(I) .NE. 0)  GOTO 300
      IF(ABS(SNGL(A(I,KK))) .LT. EPS)  A(I,KK) = 0.
      IF(A(I,KK) .GT. MIND)  GOTO 300
      MIND = A(I,KK)
      IP = I
  300 CONTINUE
      IF(MIND .LT. 0.)  GOTO 310
      MODE = 2
      RETURN
  310 CALL FINDPC  (A, B, C, Z0, M, N, MD, LR, LC, LCQ, IROW, ICOL,
     1              IP, KP, KK, Q1, Q2, EPS, 0)
      IF(KP .EQ. 0)  KP = KK
      CALL PIVOT  (A, B, C, Z0, M, N, MD, IP, KP, IROW, ICOL, EPS)
      IF(ABS(SNGL(C(KK))).LT.EPS) C(KK)=0.
      IF(C(KK).LT.0.) GO TO 270
      GOTO 250
C  SINCE AN INITIAL SOLUTION HAS BEEN FOUND, SIMPLEX ALGORITHM MAY NOW S
C  =====================================================================
C     IT IS CHECKED WHETHER THE MAXIMUM IS ALREADY OBTAINED
  500 IFALL = 0
      DO 510  I = 1, M
      IF(LR(I) .NE. -2)  GOTO 510
      IF(ABS(SNGL(B(I))) .LT. EPS)  B(I) = 0.
      IF(B(I) .EQ. 0.)  GOTO 505
      MODE = 1
      RETURN
  505 IFALL = 1
  510 CONTINUE
C     IFALL = 1  MEANS THAT THERE IS A FREE VARIABLE OUT OF THE BASIS
C     WHICH DOES NOT INFLUENCE THE MAXIMUM PROBLEM  (MODE = 3)
C     THE NEGATIVE B(I) ARE NOW PICKED OUT
C*UL  550 IP = 0
      IP = 0
      DO 555  I = 1, M
      IF(LR(I) .NE. 0)  GOTO 555
      IF(ABS(SNGL(B(I))) .LT. EPS)  B(I) = 0.
      IF(B(I) .LT. 0.)  IP = I
  555 CONTINUE
      IF(IP .NE. 0)  GOTO 600
C     SINCE ALL THE COEFFICIENTS B(I) ARE NONNEGATIVE, THE FINAL TABLEAU
C     HAS BEEN OBTAINED
C     THERE IS NOW CHECKED, WHETHER THE SOLUTION IS UNIQUE OR NOT.
      IF(IFALL .EQ. 0)  GOTO 560
      MODE = 3
      RETURN
  560 IND = 0
      DO 562  K = 1, N
      IF(LC(K) .NE. 0)  GOTO 562
      IF(ABS(SNGL(C(K))) .LT. EPS)  C(K) = 0.
      IF(C(K) .NE. 0.)  GOTO 562
      IND = IND + 1
      LC(K) = 2
  562 CONTINUE
      IFALL = IND
      IND = 0
      DO 565  I = 1, M
      IF(LR(I) .NE. 0)  GOTO 565
      IF(ABS(SNGL(B(I))) .LT. EPS)  B(I) = 0.
      IF(B(I) .NE. 0.)  GOTO 565
      IND = IND + 1
      LR(I) = 2
  565 CONTINUE
      IF(IND .NE. 0)  GOTO 570
      MODE = 0
      RETURN
  570 IF(IFALL .NE. 0)  GOTO 575
      MODE = 3
      RETURN
C     SINCE IN THE FINAL TABLEAU THERE ARE SOME C(K) = 0. AND SOME B(I)
C     IT MUST BE CHECKED, WHETHER THE SOLUTION IS UNIQUE OR NOT.
  575 DO 577  I = 1, M
      IF(LR(I) .NE. 2)  GOTO 577
      MAXD = 0.
      DO 576  K = 1, N
      IF(LC(K) .NE. 2)  GOTO 576
      IF(ABS(SNGL(A(I,K))) .LT. EPS)  A(I,K) = 0.
      IF(A(I,K) .GT. MAXD)  MAXD = A(I,K)
  576 CONTINUE
      IF(MAXD .GT. 0.)  GOTO 577
      MODE = 3
      RETURN
  577 CONTINUE
      DO 581  K = 1, N
      IF(LC(K) .NE. 2)  GOTO 581
      MIND = 1.
      DO 578  I = 1, M
      IF(LR(I) .NE. 2)  GOTO 578
      IF(A(I,K) .LT. MIND)  MIND = A(I,K)
  578 CONTINUE
      IF(MIND .LT. 0.) GOTO 581
      IF(MIND .EQ. 0.)  GOTO 579
      MODE = 0
      RETURN
  579 DO 580  I = 1, M
      IF(LR(I) .NE. 2)  GOTO 580
      IF(A(I,K) .GT. 0.)  LR(I) = -2
  580 CONTINUE
      LC(K) = 1
  581 CONTINUE
      NC = 0
      DO 582  K = 1, N
      IF(LC(K) .NE. 2)  GOTO 582
      NC = NC + 1
  582 CONTINUE
      NR = 0
      DO 583  I = 1, M
      IF(LR(I) .NE. 2)  GOTO 583
      NR = NR + 1
      IP = I
  583 CONTINUE
      IF(NR .NE. 0  .AND.  NC .NE. 0)  GOTO 585
      MODE = 0
      RETURN
  585 CALL FINDPC  (A, B, C, Z0, M, N, MD, LR, LC, LCQ, IROW, ICOL,
     1              IP, KP, 0, Q1, Q2, EPS, 2)
      IF(KP .NE. 0)  GOTO 590
      MODE = 3
      RETURN
  590 CALL PIVOT  (A, B, C, Z0, M, N, MD, IP, KP, IROW, ICOL, EPS)
      GOTO 575
  600 CALL FINDPC  (A, B, C, Z0, M, N, MD, LR, LC, LCQ, IROW, ICOL,
     1              IP, KP, 0, Q1, Q2, EPS, 0)
      IF(KP .NE. 0)  GOTO 650
      MODE = 1
      RETURN
  650 CALL PIVOT  (A, B, C, Z0, M, N, MD, IP, KP, IROW, ICOL, EPS)
      GOTO 500
      END
