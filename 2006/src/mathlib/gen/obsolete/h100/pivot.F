*
* $Id: pivot.F,v 1.1.1.1 1996/04/01 15:03:16 mclareni Exp $
*
* $Log: pivot.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:16  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE  PIVOT (A, B, C, Z0, M, N, MD, IP, KP, IROW, ICOL, EPS)
C  PIVOT EXCHANGES A BASIC AND A NONBASIC VARIABLE AND TRANSFORMS THE
C  WHOLE TABLEAU
C  =====================================================================
      DOUBLE PRECISION  A, B, C, Z0
      DIMENSION         A(MD,1), B(1), C(1), IROW(1), ICOL(1)
      A(IP,KP) = DBLE(1.)/A(IP,KP)
      DO 1  I = 1, M
      IF(I .EQ. IP)  GOTO 1
      IF(A(I,KP) .NE. 0.)  A(I,KP) = A(I,KP)*A(IP,KP)
    1 CONTINUE
      IF(C(KP) .NE. 0.)  C(KP) = C(KP)*A(IP,KP)
      DO 3  I = 1, M
      IF(I .EQ. IP)  GOTO 3
      DO 2  K = 1, N
      IF(K .EQ. KP)  GOTO 2
      IF(A(IP,K) .EQ. 0.  .OR.  A(I,KP) .EQ. 0.)  GOTO 2
      A(I,K) = A(I,K) - A(IP,K)*A(I,KP)
    2 CONTINUE
      IF(B(IP) .EQ. 0.  .OR.  A(I,KP) .EQ. 0.)  GOTO 3
      B(I) = B(I) - B(IP)*A(I,KP)
    3 CONTINUE
      DO 4  K = 1, N
      IF(K .EQ. KP)  GOTO 4
      IF(A(IP,K) .EQ. 0.  .OR.  C(KP) .EQ. 0.)  GOTO 4
      C(K) = C(K) -  A(IP,K)*C(KP)
    4 CONTINUE
      IF(B(IP) .EQ. 0.  .OR.  C(KP) .EQ. 0.)  GOTO 5
      Z0 = Z0 - B(IP)*C(KP)
    5 DO 6  K = 1, N
      IF(K .EQ. KP)  GOTO 6
      IF(A(IP,K) .NE. 0.)  A(IP,K) = -A(IP,K)*A(IP,KP)
    6 CONTINUE
      IF(B(IP) .NE. 0.)  B(IP) = -B(IP)*A(IP,KP)
C     CALCULATION OF THE NEW EPSILON-VALUE
      CALL EPSILO(A,M,N,MD,EPS)
C     IF THE ABSOLUTE VALUE OF AN ELEMENT IS LESS THAN EPSILON, THEN
C     ITS VALUE IS ZERO
      DO 15  I = 1, M
      DO 10  K = 1, N
   10 IF(ABS(SNGL(A(I,K))) .LT. EPS)  A(I,K) = 0.
   15 CONTINUE
      DO 20  I = 1, M
   20 IF(ABS(SNGL(B(I))) .LT. EPS)  B(I) = 0.
      DO 25  K = 1, N
   25 IF(ABS(SNGL(C(K))) .LT. EPS)  C(K) = 0.
      IF(ABS(SNGL(Z0)) .LT. EPS)  Z0 = 0.
      IR = IROW(IP)
      IROW(IP) = ICOL(KP)
      ICOL(KP) = IR
      RETURN
      END
