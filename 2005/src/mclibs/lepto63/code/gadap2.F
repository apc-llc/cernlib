
C **********************************************************************

      SUBROUTINE GADAP2(A0,B0,FL,FU,F,EPS,SUM)
C
C   PURPOSE           - INTEGRATE A FUNCTION F(X,Y) OF TWO VARIABLES
C   METHOD            - ADAPTIVE GAUSSIAN IN BOTH DIRECTIONS
C   USAGE             - CALL GADAP2(A0,B0,FL,FU,F,EPS,SUM)
C   PARAMETERS  A0    - LOWER X-LIMIT (INPUT,REAL)
C               B0    - UPPER X-LIMIT (INPUT,REAL)
C               FL    - USER SUPPLIED FUNCTION FL(X) GIVING THE LOWER
C                       Y-LIMIT FOR A GIVEN X-VALUE
C                       (INPUT,REAL FUNCTION)
C               FU    - USER SUPPLIED FUNCTION FU(X) GIVING THE UPPER
C                       Y-LIMIT FOR A GIVEN X-VALUE
C                       (INPUT,REAL FUNCTION)
C               F     - USER SUPPLIED FUNCTION F(X,Y) TO BE INTEGRATED
C                       (INPUT,REAL FUNCTION)
C               EPS   - DESIRED ACCURACY (INPUT,REAL)
C               SUM   - CALCULATED VALUE FOR THE INTEGRAL (OUTPUT,REAL)
C   PRECISION         - SINGLE
C   REQ'D PROG'S      - FL,FU,F,GADAPF
C   AUTHOR            - THOMAS JOHANSSON, LDC,1973
C
      COMMON/GADAP1/ NUM,IFU
      EXTERNAL F,FL,FU
      DIMENSION A(300),B(300),F1(300),F2(300),F3(300),S(300),N(300)
    1 FORMAT(16H GADAP:I TOO BIG)
      DSUM(F1F,F2F,F3F,AA,BB)=5./18.*(BB-AA)*(F1F+1.6*F2F+F3F)
      IF(EPS.LT.1.0E-8) EPS=1.0E-8
      RED=1.4
      L=1
      I=1
      SUM=0.
      C=SQRT(15.)/5.
      A(1)=A0
      B(1)=B0
      X=0.5*(1+C)*A0+0.5*(1-C)*B0
      AY=FL(X)
      BY=FU(X)
      F1(1)=GADAPF(X,AY,BY,F,EPS)
      X=0.5*(A0+B0)
      AY=FL(X)
      BY=FU(X)
      F2(1)=GADAPF(X,AY,BY,F,EPS)
      X=0.5*(1-C)*A0+0.5*(1+C)*B0
      AY=FL(X)
      BY=FU(X)
      F3(1)=GADAPF(X,AY,BY,F,EPS)
      IFU=3
      S(1)=  DSUM(F1(1),F2(1),F3(1),A0,B0)
  100 CONTINUE
      L=L+1
      N(L)=3
      EPS=EPS*RED
      A(I+1)=A(I)+C*(B(I)-A(I))
      B(I+1)=B(I)
      A(I+2)=A(I)+B(I)-A(I+1)
      B(I+2)=A(I+1)
      A(I+3)=A(I)
      B(I+3)=A(I+2)
      W1=A(I)+(B(I)-A(I))/5.
      U2=2.*W1-(A(I)+A(I+2))/2.
      X=A(I)+B(I)-W1
      AY=FL(X)
      BY=FU(X)
      F1(I+1)=GADAPF(X,AY,BY,F,EPS)
      F2(I+1)=F3(I)
      X=B(I)-A(I+2)+W1
      AY=FL(X)
      BY=FU(X)
      F3(I+1)=GADAPF(X,AY,BY,F,EPS)
      X=U2
      AY=FL(X)
      BY=FU(X)
      F1(I+2)=GADAPF(X,AY,BY,F,EPS)
      F2(I+2)=F2(I)
      X=B(I+2)+A(I+2)-U2
      AY=FL(X)
      BY=FU(X)
      F3(I+2)=GADAPF(X,AY,BY,F,EPS)
      X=A(I)+A(I+2)-W1
      AY=FL(X)
      BY=FU(X)
      F1(I+3)=GADAPF(X,AY,BY,F,EPS)
      F2(I+3)=F1(I)
      X=W1
      AY=FL(X)
      BY=FU(X)
      F3(I+3)=GADAPF(X,AY,BY,F,EPS)
      IFU=IFU+6
      IF(IFU.GT.5000) GOTO 130
      S(I+1)=  DSUM(F1(I+1),F2(I+1),F3(I+1),A(I+1),B(I+1))
      S(I+2)=  DSUM(F1(I+2),F2(I+2),F3(I+2),A(I+2),B(I+2))
      S(I+3)=  DSUM(F1(I+3),F2(I+3),F3(I+3),A(I+3),B(I+3))
      SS=S(I+1)+S(I+2)+S(I+3)
      I=I+3
      IF(I.GT.300)GOTO 120
      SOLD=S(I-3)
      IF(ABS(SOLD-SS).GT.EPS*(1.+ABS(SS))/2.) GOTO 100
      SUM=SUM+SS
      I=I-4
      N(L)=0
      L=L-1
  110 CONTINUE
      IF(L.EQ.1) GOTO 130
      N(L)=N(L)-1
      EPS=EPS/RED
      IF(N(L).NE.0) GOTO 100
      I=I-1
      L=L-1
      GOTO 110
  120 WRITE(6,1)
 130  RETURN
      END
