*
* $Id: erf.F,v 1.1.1.1 1996/02/15 17:53:26 mclareni Exp $
*
* $Log: erf.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:26  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION ERF (X)
C
C CERN PROGLIB# C300    ERF             .VERSION KERNFOR  3.15  820113
C ORIG. 10/08/76 G.ERSKINE
C
C     ******************************************************************
C
C     ENTRY POINTS ... ERF, ERFC.
C
C     THESE THREE FUNCTIONS ARE COMPUTED FROM THE RATIONAL APPROXIMAT-
C     IONS OF W.J.CODY, MATHEMATICS OF COMPUTATION, VOLUME 22 (1969),
C     PAGES 631-637.
C
C     FOR ABS(X) LE 0.47 THE BASIC FUNCTION IS ERF.  FOR ABS(X) GT 0.47
C     THE BASIC FUNCTION IS ERFC.  THE FINAL RESULT IS OBTAINED IN TERMS
C     OF THE BASIC FUNCTION AS SHOWN IN THE FOLLOWING TABLE.  A=ABS(X)
C     AND C=SQRT(2).
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
      DIMENSION P1(4),Q1(4), P2(8),Q2(8), P3(5),Q3(5)
      DATA P1/2.42667 95523 0532 E2,
     1        2.19792 61618 2942 E1,
     2        6.99638 34886 1914 E0,
     3       -3.56098 43701 8154 E-2/
      DATA Q1/2.15058 87586 9861 E2,
     1        9.11649 05404 5149 E1,
     2        1.50827 97630 4078 E1,
     3        1.00000 00000 0000 E0/
      DATA P2/3.00459 26102 0162 E2,
     1        4.51918 95371 1873 E2,
     2        3.39320 81673 4344 E2,
     3        1.52989 28504 6940 E2,
     4        4.31622 27222 0567 E1,
     5        7.21175 82508 8309 E0,
     6        5.64195 51747 8974 E-1,
     7       -1.36864 85738 2717 E-7/
      DATA Q2/3.00459 26095 6983 E2,
     1        7.90950 92532 7898 E2,
     2        9.31354 09485 0610 E2,
     3        6.38980 26446 5631 E2,
     4        2.77585 44474 3988 E2,
     5        7.70001 52935 2295 E1,
     6        1.27827 27319 6294 E1,
     7        1.00000 00000 0000 E0/
      DATA P3/-2.99610 70770 3542 E-3,
     1        -4.94730 91062 3251 E-2,
     2        -2.26956 59353 9687 E-1,
     3        -2.78661 30860 9648 E-1,
     4        -2.23192 45973 4185 E-2/
      DATA Q3/1.06209 23052 8468 E-2,
     1        1.91308 92610 7830 E-1,
     2        1.05167 51070 6793 E0,
     3        1.98733 20181 7135 E0,
     4        1.00000 00000 0000 E0/
      DATA CONST1/ 0.70710 67811 86548 /
C        ( CONST1= SQRT(1/2) )
      DATA CONST2/0.56418 95835 47756 /
C        ( CONST2= SQRT(1/PI) )
C
C     ******************************************************************
C
C  ENTRY POINTS. SET IENTRY (=1 FOR ERF, =2 FOR ERFC).
C
      IENTRY=1
      T=X
      A=ABS(T)
      IF(A.LE.6.0) GO TO 11
      ERF=SIGN(1.0,X)
      RETURN
C
#if defined(CERNLIB_ENTRCDC)
      ENTRY  ERFC
#endif
#if !defined(CERNLIB_ENTRCDC)
      ENTRY  ERFC (X)
#endif
      IENTRY=2
      T=X
      IF(T.GE.-6.0) GO TO 10
#if defined(CERNLIB_ENTRET)
      ERFC=2.0
#endif
#if !defined(CERNLIB_ENTRET)
      ERF=2.0
#endif
      RETURN
C
C  SELECT BASIC FUNCTION. SET IBASIC (=1 FOR ERF, =2 FOR ERFC).
C
   10 A=ABS(T)
   11 S=T**2
      IF(A.GT.0.47) GO TO 20
C
C  IBASIC=1.  SET Y=ERF(T).
C
      IBASIC=1
      Y=T*(P1(1)+S*(P1(2)+S*(P1(3)+S*P1(4) )))
     1   /(Q1(1)+S*(Q1(2)+S*(Q1(3)+S*Q1(4) )))
      GO TO 30
C
C  IBASIC=2. SET Y=ERFC(A).
C
   20 IBASIC=2
      IF(A.GT.4.0) GO TO 21
      Y=EXP(-S)*(P2(1)+A*(P2(2)+A*(P2(3)+A*(P2(4)+A*(P2(5)+
     1        A*(P2(6)+A*(P2(7)+A*P2(8) )))))))
     2         /(Q2(1)+A*(Q2(2)+A*(Q2(3)+A*(Q2(4)+A*(Q2(5)+
     3        A*(Q2(6)+A*(Q2(7)+A*Q2(8) )))))))
      GO TO 30
C
   21 Y=0.0
      IF(A.GT.26.0) GO TO 30
      R=1.0/A
      U=R**2
      Y=R*EXP(-S)*( CONST2 +
     1     U*(P3(1)+U*(P3(2)+U*(P3(3)+U*(P3(4)+U*P3(5) ))))
     2      /(Q3(1)+U*(Q3(2)+U*(Q3(3)+U*(Q3(4)+U*Q3(5) )))) )
C
C  EXPRESS FINAL RESULT IN TERMS OF Y.
C
   30 IF(IENTRY.NE.1) GO TO 40
      IF(IBASIC.EQ.2) GO TO 31
      ERF=Y
      RETURN
   31 ERF=1.0-Y
      IF(X.LT.0.0) ERF=-ERF
      RETURN
C
   40 IF(IBASIC.EQ.2) GO TO 41
      Y=1.0-Y
      GO TO 42
   41 IF(X.LT.0.0) Y=2.0-Y
#if defined(CERNLIB_ENTRET)
   42 ERFC=Y
#endif
#if !defined(CERNLIB_ENTRET)
   42 ERF=Y
#endif
      RETURN
C
      END
