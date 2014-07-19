*
* $Id: besi0.F,v 1.1.1.1 1996/02/15 17:49:09 mclareni Exp $
*
* $Log: besi0.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:09  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
      REAL FUNCTION BESI0(RX)
      REAL RX
      CHARACTER*6 ENAME
      LOGICAL LEX
#if defined(CERNLIB_NUMHIPRE)
      REAL X,Y,R,V,V1,V4,V5,A0,A,A1,A2,B,B0,B1,B2,T(6)
      REAL W,W0,W1,W2,W3,W4,W5,W6
      REAL F,F1,F2,F3,C,C0,C1(0:15),PI1,EPS,H,ALFA,D
      REAL ZERO,ONE,TWO,FIVE,EIGHT
#endif
#if defined(CERNLIB_NUMLOPRE)
      DOUBLE PRECISION X,Y,R,V,V1,V4,V5,A0,A,A1,A2,B,B0,B1,B2,T(6)
      DOUBLE PRECISION W,W0,W1,W2,W3,W4,W5,W6
      DOUBLE PRECISION F,F1,F2,F3,C,C0,C1(0:15),PI1,EPS,H,ALFA,D
      DOUBLE PRECISION ZERO,ONE,TWO,FIVE,EIGHT
      DOUBLE PRECISION DBESI0,DEBSI0,DX
#endif
 
      DATA ZERO /0.0D0/, ONE /1.0D0/, TWO /2.0D0/, FIVE /5.0D0/
      DATA EIGHT /8.0D0/
      DATA PI1 /0.39894 22804 0143D0/, EPS /1.0D-14/
 
      DATA T(1) /0.66666 66666 6667D0/
      DATA T(2) /0.80000 00000 0000D0/
      DATA T(3) /0.07500 00000 0000D0/
      DATA T(4) /0.33333 33333 3333D0/
      DATA T(5) /0.20000 00000 0000D0/
      DATA T(6) /0.02500 00000 0000D0/
 
      DATA C1( 0) /+1.00827 92054 587D0/
      DATA C1( 1) /+0.00844 51226 249D0/
      DATA C1( 2) /+0.00017 27006 308D0/
      DATA C1( 3) /+0.00000 72475 911D0/
      DATA C1( 4) /+0.00000 05135 877D0/
      DATA C1( 5) /+0.00000 00568 170D0/
      DATA C1( 6) /+0.00000 00085 131D0/
      DATA C1( 7) /+0.00000 00012 384D0/
      DATA C1( 8) /+0.00000 00000 298D0/
      DATA C1( 9) /-0.00000 00000 790D0/
      DATA C1(10) /-0.00000 00000 331D0/
      DATA C1(11) /-0.00000 00000 045D0/
      DATA C1(12) /+0.00000 00000 018D0/
      DATA C1(13) /+0.00000 00000 010D0/
      DATA C1(14) /+0.00000 00000 000D0/
      DATA C1(15) /-0.00000 00000 001D0/
#if defined(CERNLIB_NUMHIPRE)
      ROUND(D)  =  D
#endif
#if defined(CERNLIB_NUMLOPRE)
      ROUND(D)  =  SNGL(D+(D-DBLE(SNGL(D))))
#endif
 
      X=RX
      ENAME=' BESI0'
      LEX=.FALSE.
      GOTO 9
 
      ENTRY EBESI0(RX)
      X=RX
      ENAME='EBESI0'
      LEX=.TRUE.
#if defined(CERNLIB_NUMLOPRE)
      GOTO 9
      ENTRY DBESI0(DX)
      X=DX
      ENAME='DBESI0'
      LEX=.FALSE.
      GO TO 9
      ENTRY DEBSI0(DX)
      X=DX
      ENAME='DEBSI0'
      LEX=.TRUE.
#endif
 
    9 V=ABS(X)
      IF(V .LT. EIGHT) THEN
       Y=(V/TWO)**2
       W=Y**2
       A0=ONE
       A1=ONE+T(1)*Y
       A2=ONE+T(2)*Y+T(3)*W
       B0=ONE
       B1=ONE-T(4)*Y
       B2=ONE-T(5)*Y+T(6)*W
       W1=FIVE
       V1=ONE
       C=ZERO
       F=TWO
    3  C0=C
       F=F+ONE
       W1=W1+TWO
       W2=W1-ONE
       W3=W2-ONE
       W4=W3-ONE
       W5=W4-ONE
       W6=W5-ONE
       V1=V1+ONE
       V4=F*W4
       V5=Y/W5
       W=Y*V1/(V4*W3)
       W0=W*V5
       F1=ONE+Y*V1/(V4*W1)
       F2=W+F*W0/(V1*W2)
       F3=-W0*V5/(W4*W6)
       A=F1*A2+F2*A1+F3*A0
       B=F1*B2+F2*B1+F3*B0
       C=A/B
       IF(ABS((C0-C)/C) .GE. EPS) THEN
        A0=A1
        A1=A2
        A2=A
        B0=B1
        B1=B2
        B2=B
        GO TO 3
       ENDIF
       B1=C
       IF(LEX) B1=EXP(-V)*B1
      ELSE
       R=ONE/V
       Y=EIGHT*R
       H=TWO*Y-ONE
       ALFA=-TWO*H
       B1=ZERO
       B2=ZERO
       DO 1 I = 15,0,-1
       B0=C1(I)-ALFA*B1-B2
       B2=B1
    1  B1=B0
       B1=PI1*SQRT(R)*(B0-H*B2)
       IF(.NOT.LEX) B1=EXP(V)*B1
      ENDIF
      IF(LEX)  THEN
         IF(ENAME .EQ. 'EBESI0')  THEN
            EBESI0=ROUND(B1)
         ELSE
            DEBSI0=B1
         ENDIF
      ELSE
         IF(ENAME .EQ. ' BESI0')  THEN
            BESI0=ROUND(B1)
         ELSE
            DBESI0=B1
         ENDIF
      ENDIF
      RETURN
      END
