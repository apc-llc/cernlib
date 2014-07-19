*
* $Id: besk0.F,v 1.1.1.1 1996/02/15 17:49:09 mclareni Exp $
*
* $Log: besk0.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:09  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
      REAL FUNCTION BESK0(RX)
      REAL RX,SX
      LOGICAL LEX
      CHARACTER*6 ENAME
      LOGICAL MFLAG,RFLAG
#if defined(CERNLIB_NUMHIPRE)
      REAL X,Y,R,A,A0,A1,A2,B,B0,B1,B2,T(10)
      REAL U0,U1,U2,U3,U4,U5,U6,U7,U8,U9
      REAL F,F1,F2,F3,C,C0,PI1,CE,EPS,H,ALFA,D
      REAL ZERO,ONE,TWO,FOUR,FIVE,SIX,SEVEN,EIGHT,NINE,HALF
      REAL C1(0:14),C2(0:15),C3(0:12)
#endif
#if defined(CERNLIB_NUMLOPRE)
      DOUBLE PRECISION X,Y,R,A,A0,A1,A2,B,B0,B1,B2,T(10)
      DOUBLE PRECISION U0,U1,U2,U3,U4,U5,U6,U7,U8,U9
      DOUBLE PRECISION F,F1,F2,F3,C,C0,PI1,CE,EPS,H,ALFA,D
      DOUBLE PRECISION ZERO,ONE,TWO,FOUR,FIVE,SIX,SEVEN,EIGHT,NINE,HALF
      DOUBLE PRECISION C1(0:14),C2(0:15),C3(0:12)
      DOUBLE PRECISION DBESK0,DEBSK0,DX
#endif
 
      DATA ZERO /0.0D0/, ONE /1.0D0/, TWO /2.0D0/
      DATA FOUR /4.0D0/, FIVE /5.0D0/, SIX /6.0D0/, SEVEN /7.0D0/
      DATA EIGHT /8.0D0/, NINE /9.0D0/, HALF /0.5D0/
 
      DATA T /16.0D0,368.0D0,43.0D0,75.0D0,400.0D0,40.0D0,
     1        48.0D0,12.0D0,20.0D0,28.0D0/
 
      DATA PI1 /1.25331 41373 155D0/, CE /0.57721 56649 0153D0/
      DATA EPS /1.0D-14/
 
      DATA C1( 0) /0.12773 34398 1218D3/
      DATA C1( 1) /0.19049 43201 7274D3/
      DATA C1( 2) /0.82489 03274 4024D2/
      DATA C1( 3) /0.22274 81924 2462D2/
      DATA C1( 4) /0.40116 73760 1793D1/
      DATA C1( 5) /0.50949 33654 3998D0/
      DATA C1( 6) /0.04771 87487 9817D0/
      DATA C1( 7) /0.00341 63317 6601D0/
      DATA C1( 8) /0.00019 24693 5969D0/
      DATA C1( 9) /0.00000 87383 1550D0/
      DATA C1(10) /0.00000 03260 9105D0/
      DATA C1(11) /0.00000 00101 6973D0/
      DATA C1(12) /0.00000 00002 6883D0/
      DATA C1(13) /0.00000 00000 0610D0/
      DATA C1(14) /0.00000 00000 0012D0/
 
      DATA C2( 0) /0.24027 70596 4072D3/
      DATA C2( 1) /0.36947 40739 7287D3/
      DATA C2( 2) /0.16997 34116 9840D3/
      DATA C2( 3) /0.49020 46377 7263D2/
      DATA C2( 4) /0.93884 97325 2684D1/
      DATA C2( 5) /0.12594 79763 6677D1/
      DATA C2( 6) /0.12377 69641 1492D0/
      DATA C2( 7) /0.00924 43098 6287D0/
      DATA C2( 8) /0.00054 06238 9649D0/
      DATA C2( 9) /0.00002 53737 9603D0/
      DATA C2(10) /0.00000 09754 7830D0/
      DATA C2(11) /0.00000 00312 4957D0/
      DATA C2(12) /0.00000 00008 4643D0/
      DATA C2(13) /0.00000 00000 1963D0/
      DATA C2(14) /0.00000 00000 0039D0/
      DATA C2(15) /0.00000 00000 0001D0/
 
      DATA C3( 0) /+0.98840 81742 3083D0/
      DATA C3( 1) /-0.01131 05046 4693D0/
      DATA C3( 2) /+0.00026 95326 1276D0/
      DATA C3( 3) /-0.00001 11066 8520D0/
      DATA C3( 4) /+0.00000 06325 7511D0/
      DATA C3( 5) /-0.00000 00450 4734D0/
      DATA C3( 6) /+0.00000 00037 9300D0/
      DATA C3( 7) /-0.00000 00003 6455D0/
      DATA C3( 8) /+0.00000 00000 3904D0/
      DATA C3( 9) /-0.00000 00000 0458D0/
      DATA C3(10) /+0.00000 00000 0058D0/
      DATA C3(11) /-0.00000 00000 0008D0/
      DATA C3(12) /+0.00000 00000 0001D0/
#if defined(CERNLIB_NUMHIPRE)
      ROUND(D)  =  D
#endif
#if defined(CERNLIB_NUMLOPRE)
      ROUND(D)  =  SNGL(D+(D-DBLE(SNGL(D))))
#endif
 
      ENAME=' BESK0'
      X=RX
      LEX=.FALSE.
      GOTO 9
 
      ENTRY EBESK0(RX)
      ENAME='EBESK0'
      X=RX
      LEX=.TRUE.
#if defined(CERNLIB_NUMLOPRE)
      GOTO 9
      ENTRY DBESK0(DX)
      ENAME='DBESK0'
      X=DX
      LEX=.FALSE.
      GO TO 9
      ENTRY DEBSK0(DX)
      ENAME='DEBSK0'
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
          IF(ENAME .EQ. 'EBESK0')  THEN
             EBESK0=ZERO
          ELSE
             DEBSK0=ZERO
          ENDIF
       ELSE
          IF(ENAME .EQ. ' BESK0')  THEN
             BESK0=ZERO
          ELSE
             DBESK0=ZERO
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
       R=B0-H*B2
       B1=ZERO
       B2=ZERO
       DO 2 I = 15,0,-1
       B0=C2(I)-ALFA*B1-B2
       B2=B1
    2  B1=B0
       B1=-(CE+LOG(HALF*X))*R+B0-H*B2
       IF(LEX) B1=EXP(X)*B1
      ELSE IF(X .GT. FIVE) THEN
       R=ONE/X
       Y=FIVE*R
       H=TWO*Y-ONE
       ALFA=-TWO*H
       B1=ZERO
       B2=ZERO
       DO 3 I = 12,0,-1
       B0=C3(I)-ALFA*B1-B2
       B2=B1
    3  B1=B0
       B1=PI1*SQRT(R)*(B0-H*B2)
       IF(.NOT.LEX) B1=EXP(-X)*B1
      ELSE
       Y=(T(1)*X)**2
       A0=ONE
       A1=(T(1)*X+SEVEN)/NINE
       A2=(Y+T(2)*X+T(3))/T(4)
       B0=ONE
       B1=(T(1)*X+NINE)/NINE
       B2=(Y+T(5)*X+T(4))/T(4)
       U1=ONE
       U4=T(6)
       U5=T(7)
       C=ZERO
       F=TWO
    4  C0=C
       F=F+ONE
       U0=T(8)*F**2-ONE
       U1=U1+TWO
       U2=U1+TWO
       U3=U1+FOUR
       U4=U4+T(9)
       U5=U5+T(10)
       U6=ONE/U3**2
       U7=U2*U6
       U8=-U7/U1
       U9=T(1)*U7*X
       F1=U9-(U0-U4)*U8
       F2=U9-(U0-U5)*U6
       F3=-U8*(U3-SIX)**2
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
         IF(ENAME .EQ. 'EBESK0')  THEN
            EBESK0=ROUND(B1)
         ELSE
            DEBSK0=B1
         ENDIF
      ELSE
         IF(ENAME .EQ. ' BESK0')  THEN
            BESK0=ROUND(B1)
         ELSE
            DBESK0=B1
         ENDIF
      ENDIF
      RETURN
 
  100 FORMAT(7X,A6,' ... NON-POSITIVE ARGUMENT X = ',E16.6)
      END
