*
* $Id: derf.F,v 1.1.1.1 1996/02/15 17:48:17 mclareni Exp $
*
* $Log: derf.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:17  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
      DOUBLE PRECISION FUNCTION DERF(DX)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LERF
C
      DIMENSION P1(4),Q1(3), P2(8),Q2(7), P3(5),Q3(4)
C
C     ******************************************************************
C
C     ENTRY POINTS ... DERF, DERFC.
C
C     THESE FUNCTIONS ARE COMPUTED FROM THE RATIONAL APPROXIMATIONS OF
C     W.J.CODY, MATHEMATICS OF COMPUTATION, VOLUME 22 (1969), PAGES
C     631-637.
C
C     FOR ABS(X) LE 0.47 THE BASIC FUNCTION IS ERF.  FOR ABS(X) GT 0.47
C     THE BASIC FUNCTION IS ERFC.  THE FINAL RESULT IS OBTAINED IN TERMS
C     OF THE BASIC FUNCTION AS SHOWN IN THE FOLLOWING TABLE, IN WHICH
C     A=ABS(X).
C
C       FUNCTION     A.LE.(0.47)                   A.GT.(0.47)
C       REQUIRED       (ALL X)          (NEGATIVE X)       (POSITIVE X)
C
C       ERF(X)         ERF(X)             ERFC(A)-1          1-ERFC(X)
C
C       ERFC(X)       1-ERF(X)            2-ERFC(A)           ERFC(X)
C
C     ******************************************************************
C
      DATA  ZERO/ 0. D0 /,  ONE/ 1. D0 /,  TWO/ 2. D0 /,  FOUR/ 4. D0 /
C
      DATA ACUT/ 0.46875 D0 /
C     ( ACUT AND 4.0 ARE CHANGE-OVER POINTS FOR THE RATIONAL APPROXIM-
C     ATIONS. )
C
      DATA CONST/ 0.56418 95835 47756 3 D0 /
C     ( CONST=SQRT(1/PI). )
C
#if defined(CERNLIB_NUME2465)
      DATA XMAX/ 75.3 D0 /
#endif
#if defined(CERNLIB_NUME293)
      DATA XMAX/ 25.8 D0 /
#endif
#if defined(CERNLIB_NUME75)
      DATA XMAX/ 13.0 D0 /
#endif
#if defined(CERNLIB_NUME38)
      DATA XMAX/ 8.9 D0 /
#endif
#if defined(CERNLIB_NUME999)
      DATA XMAX/ *** NOT AVAILABLE *** /
#endif
C     ( XMAX=SQRT(-ALOG(RMIN)-10.0), WHERE RMIN IS THE SMALLEST NORMAL-
C     IZED REPRESENTABLE NUMBER.  ERFC(XMAX) IS CLOSE TO THE UNDERFLOW
C     THRESHOLD. )
C
      DATA XUNIT/ 5.9 D0 /
C     ( XUNIT=SQRT(-ALOG(RELPR)+1.0), WHERE RELPR IS THE SMALLEST NUMBER
C     FOR WHICH 1.0+RELPR DIFFERS FROM 1.0.  ERF(XUNIT) IS INDISTIN-
C     GUISHABLE FROM 1.0. )
C
      DATA P1/ 2.42667 95523 05318 D+2,
     *         2.19792 61618 29415 D+1,
     *         6.99638 34886 19136 D+0,
     *        -3.56098 43701 81538 D-2 /
      DATA Q1/ 2.15058 87586 98612 D+2,
     *         9.11649 05404 51490 D+1,
     *         1.50827 97630 40779 D+1 /
      DATA P2/ 3.00459 26102 01616 D+2,
     *         4.51918 95371 18729 D+2,
     *         3.39320 81673 43437 D+2,
     *         1.52989 28504 69404 D+2,
     *         4.31622 27222 05674 D+1,
     *         7.21175 82508 83094 D+0,
     *         5.64195 51747 89740 D-1,
     *        -1.36864 85738 27167 D-7 /
      DATA Q2/ 3.00459 26095 69833 D+2,
     *         7.90950 92532 78980 D+2,
     *         9.31354 09485 06096 D+2,
     *         6.38980 26446 56312 D+2,
     *         2.77585 44474 39876 D+2,
     *         7.70001 52935 22947 D+1,
     *         1.27827 27319 62942 D+1 /
      DATA P3/-2.99610 70770 35422 D-3,
     *        -4.94730 91062 32507 D-2,
     *        -2.26956 59353 96869 D-1,
     *        -2.78661 30860 96478 D-1,
     *        -2.23192 45973 41847 D-2 /
      DATA Q3/ 1.06209 23052 84679 D-2,
     *         1.91308 92610 78298 D-1,
     *         1.05167 51070 67932 D+0,
     *         1.98733 20181 71353 D+0 /
C
C     ******************************************************************
C
C  ENTRY DERF.
      LERF=.TRUE.
      T=DX
      A=ABS(T)
      IF (A.GT.XUNIT) THEN
         DERF=SIGN(ONE,T)
         RETURN
      ENDIF
      GO TO 1
C
C  ENTRY DERFC.
      ENTRY DERFC(DX)
      LERF=.FALSE.
      T=DX
      A=ABS(T)
      IF (T.LT.-XUNIT) THEN
         DERFC=TWO
         RETURN
      ELSEIF (T.GT.XMAX) THEN
         DERFC=ZERO
         RETURN
      ENDIF
C
C  COMMON CODE.
C
    1 S=T**2
      IF (A.LE.ACUT) THEN
C
C  SET Y=DERF(X) AND TERMINATE.
C
         Y=T*(P1(1)+S*(P1(2)+S*(P1(3)+S*P1(4))))
     *      /(Q1(1)+S*(Q1(2)+S*(Q1(3)+S)))
         IF (LERF) THEN
            DERF=Y
         ELSE
            DERFC=ONE-Y
         ENDIF
C
      ELSE
C
C  SET Y=DERFC(A) AND TERMINATE.
C
         IF (A.LE.FOUR) THEN
            Y=EXP(-S)*(P2(1)+A*(P2(2)+A*(P2(3)+A*(P2(4)+A*(P2(5)+
     *              A*(P2(6)+A*(P2(7)+A*P2(8))))))))
     *               /(Q2(1)+A*(Q2(2)+A*(Q2(3)+A*(Q2(4)+A*(Q2(5)+
     *              A*(Q2(6)+A*(Q2(7)+A)))))))
         ELSE
            R=ONE/A
            U=R**2
            Y=R*EXP(-S)*( CONST +
     *           U*(P3(1)+U*(P3(2)+U*(P3(3)+U*(P3(4)+U*P3(5)))))
     *            /(Q3(1)+U*(Q3(2)+U*(Q3(3)+U*(Q3(4)+U)))) )
         ENDIF
         IF (LERF) THEN
            DERF=SIGN(ONE-Y,T)
         ELSE
            IF (T.GE.ZERO) THEN
               DERFC=Y
            ELSE
               DERFC=TWO-Y
            ENDIF
         ENDIF
C
      ENDIF
      RETURN
C
      END
