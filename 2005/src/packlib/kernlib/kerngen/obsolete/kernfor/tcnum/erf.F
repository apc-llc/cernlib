*
* $Id: erf.F,v 1.1.1.1 1996/02/15 17:54:16 mclareni Exp $
*
* $Log: erf.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:16  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      FUNCTION ERF(X)
      LOGICAL CFN
C     ( CFN IS SET .FALSE. FOR ENTRY ERF AND .TRUE. FOR ENTRY ERFC. )
C
C     ******************************************************************
C
C     ENTRY POINTS ... ERF, ERFC.
C
C     THESE TWO FUNCTIONS ARE COMPUTED FROM THE RATIONAL APPROXIMAT-
C     IONS OF W.J.CODY, MATHEMATICS OF COMPUTATION, VOLUME 22 (1969),
C     PAGES 631-637.
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
#if defined(CERNLIB_NUMHIPRE)
      DIMENSION P1(4),Q1(4), P2(8),Q2(8), P3(5),Q3(5)
C
C     (EACH DECIMAL CONSTANT HAS MAXMIMUM CDC NO-DIAGNOSTIC PRECISION.)
C
      DATA CONST/ 0.56418 95835 4776 /
C     ( CONST=SQRT(1/PI). )
C
#endif
#if defined(CERNLIB_NUMLOPRE)
      DIMENSION P1(3),Q1(3), P2(5),Q2(5), P3(3),Q3(3)
C
      DATA CONST/ 0.56418 9584 /
C     ( CONST=SQRT(1/PI) . )
C
#endif
#if defined(CERNLIB_NUME293)
      DATA XMAX/ 25.8 /
#endif
#if defined(CERNLIB_NUME75)
      DATA XMAX/ 13.0 /
#endif
#if defined(CERNLIB_NUME38)
      DATA XMAX/ 8.9 /
#endif
C     ( XMAX=SQRT(-ALOG(RMIN)-10.0), WHERE RMIN IS THE SMALLEST NORMAL-
C     IZED REPRESENTABLE NUMBER.  ERFC(XMAX) IS CLOSE TO THE UNDERFLOW
C     THRESHOLD. )
C
#if defined(CERNLIB_NUME293)
      DATA XUNIT/ 5.8 /
#endif
#if defined(CERNLIB_NUME75)
      DATA XUNIT/ 3.9 /
#endif
#if defined(CERNLIB_NUME38)
      DATA XUNIT/ 4.4 /
#endif
C     ( XUNIT=SQRT(-ALOG(RELPR)+1.0), WHERE RELPR IS THE SMALLEST NUMBER
C     FOR WHICH 1.0+RELPR DIFFERS FROM 1.0.  ERF(XUNIT) IS INDISTIN-
C     GUISHABLE FROM 1.0. )
C
#if defined(CERNLIB_NUMHIPRE)
      DATA P1/2.42667 95523 0532 E2,
     *        2.19792 61618 2942 E1,
     *        6.99638 34886 191  E0,
     *       -3.56098 43701 815  E-2/
      DATA Q1/2.15058 87586 9861 E2,
     *        9.11649 05404 515  E1,
     *        1.50827 97630 4078 E1,
     *        1.00000 00000 0000 E0/
      DATA P2/3.00459 26102 016  E2,
     *        4.51918 95371 187  E2,
     *        3.39320 81673 434  E2,
     *        1.52989 28504 6940 E2,
     *        4.31622 27222 057  E1,
     *        7.21175 82508 831  E0,
     *        5.64195 51747 897  E-1,
     *       -1.36864 85738 2717 E-7/
      DATA Q2/3.00459 26095 698  E2,
     *        7.90950 92532 790  E2,
     *        9.31354 09485 061  E2,
     *        6.38980 26446 563  E2,
     *        2.77585 44474 3988 E2,
     *        7.70001 52935 229  E1,
     *        1.27827 27319 6294 E1,
     *        1.00000 00000 0000 E0/
      DATA P3/-2.99610 70770 354  E-3,
     *        -4.94730 91062 325  E-2,
     *        -2.26956 59353 9687 E-1,
     *        -2.78661 30860 9648 E-1,
     *        -2.23192 45973 4185 E-2/
      DATA Q3/1.06209 23052 8468 E-2,
     *        1.91308 92610 7830 E-1,
     *        1.05167 51070 6793 E0,
     *        1.98733 20181 7135 E0,
     *        1.00000 00000 0000 E0/
#endif
#if defined(CERNLIB_NUMLOPRE)
      DATA P1/2.13853 322 E1,
     *        1.72227 577 E0,
     *        3.16652 891 E-1/
      DATA Q1/1.89522 572 E1,
     *        7.84374 571 E0,
     *        1.00000 000 E0/
      DATA P2/7.37388 831 E0,
     *        6.86501 848 E0,
     *        3.03179 934 E0,
     *        5.63169 619 E-1,
     *        4.31877 874 E-5/
      DATA Q2/7.37396 089 E0,
     *        1.51849 082 E1,
     *        1.27955 295 E1,
     *        5.35421 679 E0,
     *        1.00000 000 E0/
      DATA P3/-4.25799 644 E-2,
     *        -1.96068 974 E-1,
     *        -5.16882 262 E-2/
      DATA Q3/1.50942 071 E-1,
     *        9.21452 412 E-1,
     *        1.00000 000 E0/
#endif
C
C     ******************************************************************
C
C  START.
C
      CFN=.FALSE.
      T=X
      A=ABS(T)
      IF(A.LE.XUNIT) GO TO 2
         ERF=SIGN(1.0,T)
         RETURN
C
#if defined(CERNLIB_ENTRCDC)
      ENTRY ERFC
#endif
#if !defined(CERNLIB_ENTRCDC)
      ENTRY ERFC(X)
#endif
      CFN=.TRUE.
      T=X
      A=ABS(T)
      IF(T.GE.-XUNIT) GO TO 1
#if defined(CERNLIB_ENTRET)
         ERFC=2.0
#endif
#if !defined(CERNLIB_ENTRET)
         ERF=2.0
#endif
         RETURN
    1 IF(T.LE.XMAX) GO TO 2
#if defined(CERNLIB_ENTRET)
         ERFC=0.0
#endif
#if !defined(CERNLIB_ENTRET)
         ERF=0.0
#endif
         RETURN
C
    2 S=T**2
      IF(A.GT.0.47) GO TO 4
C
C  SET Y=ERF(X), THEN TERMINATE.
C
#if defined(CERNLIB_NUMHIPRE)
      Y=T*(P1(1)+S*(P1(2)+S*(P1(3)+S*P1(4) )))
     *   /(Q1(1)+S*(Q1(2)+S*(Q1(3)+S*Q1(4) )))
#endif
#if defined(CERNLIB_NUMLOPRE)
      Y=T*(P1(1)+S*(P1(2)+S*P1(3) ))
     *   /(Q1(1)+S*(Q1(2)+S*Q1(3) ))
#endif
      IF(CFN) GO TO 3
         ERF=Y
         RETURN
#if defined(CERNLIB_ENTRET)
    3 ERFC=1.0-Y
#endif
#if !defined(CERNLIB_ENTRET)
    3 ERF=1.0-Y
#endif
      RETURN
C
C  SET Y=ERFC(A), THEN TERMINATE.
C
    4 IF(A.GT.4.) GO TO 5
C
#if defined(CERNLIB_NUMHIPRE)
      Y=EXP(-S)*(P2(1)+A*(P2(2)+A*(P2(3)+A*(P2(4)+A*(P2(5)+
     *        A*(P2(6)+A*(P2(7)+A*P2(8) )))))))
     *         /(Q2(1)+A*(Q2(2)+A*(Q2(3)+A*(Q2(4)+A*(Q2(5)+
     *        A*(Q2(6)+A*(Q2(7)+A*Q2(8) )))))))
      GO TO 6
C
    5 R=1./A
      U=R**2
      Y=R*EXP(-S)*( CONST +
     *     U*(P3(1)+U*(P3(2)+U*(P3(3)+U*(P3(4)+U*P3(5) ))))
     *      /(Q3(1)+U*(Q3(2)+U*(Q3(3)+U*(Q3(4)+U*Q3(5) )))) )
#endif
#if defined(CERNLIB_NUMLOPRE)
      Y=EXP(-S)*(P2(1)+A*(P2(2)+A*(P2(3)+A*(P2(4)+A*P2(5) ))))
     *         /(Q2(1)+A*(Q2(2)+A*(Q2(3)+A*(Q2(4)+A*Q2(5) ))))
      GO TO 6
C
    5 R=1.0/A
      U=R**2
      Y=R*EXP(-S)*( CONST + U*(P3(1)+U*P3(2))/(Q3(1)+U*Q3(2)) )
#endif
C
    6 IF(CFN) GO TO 7
         ERF=SIGN(1.0-Y,T)
         RETURN
    7 IF(T.LT.0.) Y=2.0-Y
#if defined(CERNLIB_ENTRET)
         ERFC=Y
#endif
#if !defined(CERNLIB_ENTRET)
         ERF=Y
#endif
      RETURN
C
      END
#ifdef CERNLIB_C300FORT_C300XX
#undef CERNLIB_C300FORT_C300XX
#endif
