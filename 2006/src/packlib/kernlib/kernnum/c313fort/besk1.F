*
* $Id: besk1.F,v 1.1.1.1 1996/02/15 17:49:09 mclareni Exp $
*
* $Log: besk1.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:09  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
      REAL FUNCTION BESK1(RX)
      REAL RX,SX
      LOGICAL LEX
      CHARACTER*6 ENAME
      LOGICAL MFLAG,RFLAG
#if defined(CERNLIB_NUMHIPRE)
      REAL X,Y,R,A,A0,A1,A2,B,B0,B1,B2,T(12)
      REAL U0,U1,U2,U3,U4,U5,U6,U7,U8,U9
      REAL F,F1,F2,F3,C,C0,PI1,CE,EPS,H,ALFA,D
      REAL ZERO,ONE,TWO,THREE,FOUR,FIVE,SIX,EIGHT,HALF
      REAL C1(0:14),C2(0:14),C3(0:11)
#endif
#if defined(CERNLIB_NUMLOPRE)
      DOUBLE PRECISION X,Y,R,A,A0,A1,A2,B,B0,B1,B2,T(12)
      DOUBLE PRECISION U0,U1,U2,U3,U4,U5,U6,U7,U8,U9
      DOUBLE PRECISION F,F1,F2,F3,C,C0,PI1,CE,EPS,H,ALFA,D
      DOUBLE PRECISION ZERO,ONE,TWO,THREE,FOUR,FIVE,SIX,EIGHT,HALF
      DOUBLE PRECISION C1(0:14),C2(0:14),C3(0:11)
      DOUBLE PRECISION DBESK1,DEBSK1,DX
#endif
 
      DATA ZERO /0.0D0/, ONE /1.0D0/, TWO /2.0D0/, THREE /3.0D0/
      DATA FOUR /4.0D0/, FIVE /5.0D0/, SIX /6.0D0/, EIGHT /8.0D0/
      DATA HALF /0.5D0/
 
      DATA T /16.0D0,3.2D0,2.2D0,432.0D0,131.0D0,35.0D0,336.0D0,
     1        40.0D0,48.0D0,12.0D0,20.0D0,28.0D0/
 
      DATA PI1 /1.25331 41373 155D0/, CE /0.57721 56649 0153D0/
      DATA EPS /1.0D-14/
 
      DATA C1( 0) /0.22060 14269 2352D3/
      DATA C1( 1) /0.12535 42668 3715D3/
      DATA C1( 2) /0.42865 23409 3128D2/
      DATA C1( 3) /0.94530 05229 4349D1/
      DATA C1( 4) /0.14296 57709 0762D1/
      DATA C1( 5) /0.15592 42954 7626D0/
      DATA C1( 6) /0.01276 80490 8173D0/
      DATA C1( 7) /0.00081 08879 0069D0/
      DATA C1( 8) /0.00004 10104 6194D0/
      DATA C1( 9) /0.00000 16880 4220D0/
      DATA C1(10) /0.00000 00575 8695D0/
      DATA C1(11) /0.00000 00016 5345D0/
      DATA C1(12) /0.00000 00000 4048D0/
      DATA C1(13) /0.00000 00000 0085D0/
      DATA C1(14) /0.00000 00000 0002D0/
 
      DATA C2( 0) /0.41888 94461 6640D3/
      DATA C2( 1) /0.24989 55490 4287D3/
      DATA C2( 2) /0.91180 31933 8742D2/
      DATA C2( 3) /0.21444 99505 3962D2/
      DATA C2( 4) /0.34384 15392 8805D1/
      DATA C2( 5) /0.39484 60929 4094D0/
      DATA C2( 6) /0.03382 87455 2688D0/
      DATA C2( 7) /0.00223 57203 3417D0/
      DATA C2( 8) /0.00011 71310 2246D0/
      DATA C2( 9) /0.00000 49754 2712D0/
      DATA C2(10) /0.00000 01746 0493D0/
      DATA C2(11) /0.00000 00051 4329D0/
      DATA C2(12) /0.00000 00001 2890D0/
      DATA C2(13) /0.00000 00000 0278D0/
      DATA C2(14) /0.00000 00000 0005D0/
 
      DATA C3( 0) /+1.03595 08587 724D0/
      DATA C3( 1) /+0.03546 52912 433D0/
      DATA C3( 2) /-0.00046 84750 282D0/
      DATA C3( 3) /+0.00001 61850 638D0/
      DATA C3( 4) /-0.00000 08451 720D0/
      DATA C3( 5) /+0.00000 00571 322D0/
      DATA C3( 6) /-0.00000 00046 456D0/
      DATA C3( 7) /+0.00000 00004 354D0/
      DATA C3( 8) /-0.00000 00000 458D0/
      DATA C3( 9) /+0.00000 00000 053D0/
      DATA C3(10) /-0.00000 00000 007D0/
      DATA C3(11) /+0.00000 00000 001D0/
#if defined(CERNLIB_NUMHIPRE)
      ROUND(D)  =  D
#endif
#if defined(CERNLIB_NUMLOPRE)
      ROUND(D)  =  SNGL(D+(D-DBLE(SNGL(D))))
#endif
 
      ENAME=' BESK1'
      X=RX
      LEX=.FALSE.
      GOTO 9
 
      ENTRY EBESK1(RX)
      ENAME='EBESK1'
      X=RX
      LEX=.TRUE.
#if defined(CERNLIB_NUMLOPRE)
      GO TO 9
      ENTRY DBESK1(DX)
      ENAME='DBESK1'
      X=DX
      LEX=.FALSE.
      GO TO 9
      ENTRY DEBSK1(DX)
      ENAME='DEBSK1'
      X=DX
      LEX=.TRUE.
#endif
 
    9 IF(X .LE. ZERO) THEN
       CALL KERMTR('C313.1',LGFILE,MFLAG,RFLAG)
       IF(MFLAG) THEN
        SX=X
        IF(LGFILE .EQ. 0) THEN
         WRITE(*,100) ENAME,SX
        ELSE
         WRITE(LGFILE,100) ENAME,SX
        ENDIF
       ENDIF
       IF(.NOT.RFLAG) CALL ABEND
      IF(LEX)  THEN
         IF(ENAME .EQ. 'EBESK1')  THEN
            EBESK1=ZERO
         ELSE
            DEBSK1=ZERO
         ENDIF
      ELSE
         IF(ENAME .EQ. ' BESK1')  THEN
            BESK1=ZERO
         ELSE
            DBESK1=ZERO
         ENDIF
      ENDIF
       RETURN
      ENDIF
      IF(X .LT. HALF) THEN
       Y=X/EIGHT
       H=TWO*Y**2-ONE
       ALFA=-TWO*H
       B1=ZERO
       B2=ZERO
       DO 1 I = 14,0,-1
       B0=C1(I)-ALFA*B1-B2
       B2=B1
    1  B1=B0
       R=Y*(B0-B2)
       B1=ZERO
       B2=ZERO
       DO 2 I = 14,0,-1
       B0=C2(I)-ALFA*B1-B2
       B2=B1
    2  B1=B0
       B1=(CE+LOG(HALF*X))*R+ONE/X-Y*(B0-B2)
       IF(LEX) B1=EXP(X)*B1
      ELSE IF(X .GT. FIVE) THEN
       R=ONE/X
       Y=FIVE*R
       H=TWO*Y-ONE
       ALFA=-TWO*H
       B1=ZERO
       B2=ZERO
       DO 3 I = 11,0,-1
       B0=C3(I)-ALFA*B1-B2
       B2=B1
    3  B1=B0
       B1=PI1*SQRT(R)*(B0-H*B2)
       IF(.NOT.LEX) B1=EXP(-X)*B1
      ELSE
       Y=(T(1)*X)**2
       A0=ONE
       A1=T(2)*X+T(3)
       A2=(Y+T(4)*X+T(5))/T(6)
       B0=ONE
       B1=T(2)*X+ONE
       B2=(Y+T(7)*X+T(6))/T(6)
       U1=ONE
       U4=T(8)
       U5=T(9)
       C=ZERO
       F=TWO
    4  C0=C
       F=F+ONE
       U0=T(10)*F**2+THREE
       U1=U1+TWO
       U2=U1+TWO
       U3=U1+FOUR
       U4=U4+T(11)
       U5=U5+T(12)
       U6=ONE/(U3**2-FOUR)
       U7=U2*U6
       U8=-U7/U1
       U9=T(1)*U7*X
       F1=U9-(U0-U4)*U8
       F2=U9-(U0-U5)*U6
       F3=U8*(FOUR-(U3-SIX)**2)
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
        GO TO 4
       ENDIF
       B1=PI1*C/SQRT(X)
       IF(.NOT.LEX) B1=EXP(-X)*B1
      ENDIF
      IF(LEX)  THEN
         IF(ENAME .EQ. 'EBESK1')  THEN
            EBESK1=ROUND(B1)
         ELSE
            DEBSK1=B1
         ENDIF
      ELSE
         IF(ENAME .EQ. ' BESK1')  THEN
            BESK1=ROUND(B1)
         ELSE
            DBESK1=B1
         ENDIF
      ENDIF
      RETURN
 
  100 FORMAT(7X,A6,' ... NON-POSITIVE ARGUMENT X = ',E16.6)
      END
