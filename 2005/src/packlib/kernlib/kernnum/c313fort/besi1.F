*
* $Id: besi1.F,v 1.1.1.1 1996/02/15 17:49:09 mclareni Exp $
*
* $Log: besi1.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:09  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
      REAL FUNCTION BESI1(RX)
      REAL RX
      CHARACTER*6 ENAME
      LOGICAL LEX
#if defined(CERNLIB_NUMHIPRE)
      REAL X,Y,R,V,V1,V2,V4,V5,A,A0,A1,A2,B,B0,B1,B2,T(2)
      REAL W,W0,W1,W2,W3,W4,W5,W6
      REAL F1,F2,F3,C,C0,F,C1(0:17),PI1,EPS,H,ALFA,D
      REAL ZERO,ONE,TWO,THREE,FOUR,SIX,EIGHT
#endif
#if defined(CERNLIB_NUMLOPRE)
      DOUBLE PRECISION X,Y,R,V,V1,V2,V4,V5,A,A0,A1,A2,B,B0,B1,B2,T(2)
      DOUBLE PRECISION W,W0,W1,W2,W3,W4,W5,W6
      DOUBLE PRECISION F1,F2,F3,C,C0,F,C1(0:17),PI1,EPS,H,ALFA,D
      DOUBLE PRECISION ZERO,ONE,TWO,THREE,FOUR,SIX,EIGHT
      DOUBLE PRECISION DBESI1,DEBSI1,DX
#endif
 
      DATA ZERO /0.0D0/, ONE /1.0D0/, TWO /2.0D0/, THREE /3.0D0/
      DATA FOUR /4.0D0/, SIX /6.0D0/, EIGHT /8.0D0/
      DATA T /0.05D0,0.1D0/
      DATA PI1 /0.39894 22804 0143D0/, EPS /1.0D-14/
 
      DATA C1( 0) /+0.97580 06023 2629D0/
      DATA C1( 1) /-0.02446 74429 6328D0/
      DATA C1( 2) /-0.00027 72053 6076D0/
      DATA C1( 3) /-0.00000 97321 4673D0/
      DATA C1( 4) /-0.00000 06297 2424D0/
      DATA C1( 5) /-0.00000 00659 6114D0/
      DATA C1( 6) /-0.00000 00096 1387D0/
      DATA C1( 7) /-0.00000 00014 0114D0/
      DATA C1( 8) /-0.00000 00000 4756D0/
      DATA C1( 9) /+0.00000 00000 8153D0/
      DATA C1(10) /+0.00000 00000 3541D0/
      DATA C1(11) /+0.00000 00000 0510D0/
      DATA C1(12) /-0.00000 00000 0180D0/
      DATA C1(13) /-0.00000 00000 0102D0/
      DATA C1(14) /-0.00000 00000 0005D0/
      DATA C1(15) /+0.00000 00000 0011D0/
      DATA C1(16) /+0.00000 00000 0003D0/
      DATA C1(17) /-0.00000 00000 0001D0/
#if defined(CERNLIB_NUMHIPRE)
      ROUND(D)  =  D
#endif
#if defined(CERNLIB_NUMLOPRE)
      ROUND(D)  =  SNGL(D+(D-DBLE(SNGL(D))))
#endif
 
      X=RX
      ENAME=' BESI1'
      LEX=.FALSE.
      GOTO 9
 
      ENTRY EBESI1(RX)
      X=RX
      ENAME='EBESI1'
      LEX=.TRUE.
#if defined(CERNLIB_NUMLOPRE)
      GOTO 9
      ENTRY DBESI1(DX)
      X=DX
      ENAME='DBESI1'
      LEX=.FALSE.
      GO TO 9
      ENTRY DEBSI1(DX)
      X=DX
      ENAME='DEBSI1'
      LEX=.TRUE.
#endif
 
    9 V=ABS(X)
      IF(V .LT. EIGHT) THEN
       Y=(V/TWO)**2
       W=Y**2
       A0=ONE
       A1=ONE+Y/FOUR
       A2=ONE+(Y+T(1)*W)/THREE
       B0=ONE
       B1=ONE-Y/FOUR
       B2=ONE-(Y-T(2)*W)/SIX
       W1=SIX
       V1=ZERO
       V2=FOUR
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
       V2=V2+ONE
       V4=F*W4
       V5=Y/W5
       W=F*Y/(V4*W3)
       W0=W*V5
       F1=ONE+Y*V1/(V4*W1)
       F2=W+V2*W0/(F*W2)
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
       B1=X*C/TWO
       IF(LEX) B1=EXP(-V)*B1
      ELSE
       R=ONE/V
       Y=EIGHT*R
       H=TWO*Y-ONE
       ALFA=-TWO*H
       B1=ZERO
       B2=ZERO
       DO 1 I = 17,0,-1
       B0=C1(I)-ALFA*B1-B2
       B2=B1
    1  B1=B0
       B1=PI1*SQRT(R)*(B0-H*B2)
       IF(.NOT.LEX) B1=EXP(V)*B1
       IF(X .LT. ZERO) B1=-B1
      ENDIF
      IF(LEX)  THEN
         IF(ENAME .EQ. 'EBESI1')  THEN
            EBESI1=ROUND(B1)
         ELSE
            DEBSI1=B1
         ENDIF
      ELSE
         IF(ENAME .EQ. ' BESI1')  THEN
            BESI1=ROUND(B1)
         ELSE
            DBESI1=B1
         ENDIF
      ENDIF
      RETURN
      END
