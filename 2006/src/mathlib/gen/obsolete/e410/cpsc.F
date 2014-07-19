*
* $Id: cpsc.F,v 1.1.1.1 1996/04/01 15:03:15 mclareni Exp $
*
* $Log: cpsc.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:15  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE CPSC(F,Z,N,RS,ER)
C
C  EVALUATION OF COMPLEX POWER SERIES COEFFICIENTS
C
C  *** INPUT PARAMETERS ***
C    F  COMPLEX FUNCTION, OF WHICH THE COEFFICIENTS ARE
C       SOUGHT.
C    Z  COMPLEX POINT AROUND WHICH F SHALL BE EXPANDED.
C    N  INTEGER, NUMBER OF COEFFICIENTS WANTED.
C       N MUST BE GE 1 AND LE 51.
C  *** OUTPUT PARAMETERS ***
C    RS COMPLEX ARRAY RS(N) CONTAINING THE N FIRST
C       COEFFICIENTS (CORRESPONDING TO THE POWERS 0 TO N-1).
C    ER REAL ARRAY ER(N) CONTAINING ERROR ESTIMATES FOR THE
C       COEFFICIENTS RS(N).
C
      EXTERNAL F
      DIMENSION IP(32),A(64),RS(N),ER(N),RT(51,3),FV(6),
     *  IW(7),SC(4),RV(4),C(4),FC(3)
      COMPLEX F,A,V,RS,RT,FV,U,W,T,Z,RV,RQ,S,XK,MULT,CO
      DATA L2/1/,IW          /1,2,4,8,16,32,64/,IP           /64,32,48,1
     +6,56,24,40,8,60,28,44,12,                  52,20,36,4,62,30,46,14,
     +54,22,38,6,58,26,42,10,50,                 18,34,2/,SC          /.
     +125,.0625,.03125,.015625/,C          /.31622776601684,.56234132519
     +035,                       .74989420933246,.86596432336007/,FV
     +      /(-1.,0.),(0.,-1.),                                    (.707
     +10678118655,-.70710678118655),                               (.923
     +87953251129,-.38268343236509),                               (.980
     +78528040323,-.19509032201613),                               (.995
     +18472667220,-.098017140329561)/,RV          /(-.4,.3),(0.,0.),(.7,
     +.2),(.02,-.06)/
C
C  STATEMENT FUNCTION FOR MULTIPLICATION OF A COMPLEX NUMBER
C  BY A REAL NUMBER.
C
      MULT(RE,CO) = CMPLX(RE*REAL(CO),RE*AIMAG(CO))
C*IA CO,RE
      DATA CO,RE/2*0/
C
C  START EXECUTION.
C
      IF(N.GT.51.OR.N.LT.1) GOTO 230
      LF = 0
      NP = 0
      M = 0
      NR = -1
C
C  FIND IF A FFT OVER 8, 16, 32, OR 64 POINTS SHOULD BE
C  USED.
C
      KL = 1
      IF(N.GT.6) KL=2
      IF(N.GT.12) KL=3
      IF(N.GT.25) KL=4
      KM = KL+2
      KN = 7-KM
      IX = IW(KM+1)
      IS = IW(KN)
C
C  FIRST RADIUS USED  R = .658... .
C
      R = .65809246578231
   10 V = CMPLX(R,0.)
C
C  FUNCTION VALUES OF F ARE STORED READY PERMUTATED FOR
C  THE FFT.
C
      DO 20 I=IS,32,IS
        IQ = IP(I)
        V = V*FV(KM)
        A(IQ) = F(Z+V)
   20   A(IQ-1) = F(Z-V)
      LN = 2
      JN = 1
C
C  THE LOOP  DO 50 ... CONSTITUTES THE FFT.
C
      DO 50 L=1,KM
        U = (1.,0.)
        W = FV(L)
        DO 40 J=1,JN
          DO 30 I=J,IX,LN
            IT = I+JN
            T = A(IT)*U
            A(IT) = A(I)-T
   30       A(I) = A(I)+T
   40     U = U*W
        LN = LN+LN
   50   JN = JN+JN
      CX = 0.
      B = 1.
C
C  TEST ON HOW FAST THE COEFFICIENTS OBTAINED DECREASE.
C
      DO 60 I=1,IX
        CT = ABS(A(I))/B
        IF(CT.LT.CX) GOTO 60
        CX = CT
        INR = I
   60   B = B*C(KL)
      IF(M.LE.1) GOTO 80
C
C  ESTIMATE OF THE ROUNDING ERROR LEVEL FOR THE LAST RADIUS.
C
      ER(1) = CX*1.E-14
      DO 70 I=2,N
   70   ER(I) = ER(I-1)/R
   80 SF = SC(KL)
      DO 90 I=1,IX
        A(I) = MULT(SF,A(I))
   90   SF = SF/R
      L1 = L2
      L2 = 1
      IF(INR.GT.IW(KM)) GOTO 130
      IF(LF.EQ.1) GOTO 120
C
C  TEST IF THE SERIES IS A TAYLOR OR A LAURENT SERIES.
C
      SR = 0.
      SP = 0.
      DO 110 J=1,4
        RQ = MULT(R,RV(J))
        S = A(IX)
        DO 100 I=2,IX
          IA = IX+1-I
  100     S = S*RQ+A(IA)
        CP = ABS(S)
        IF(CP.GT.SP) SP=CP
        CM = ABS(S-F(Z+RQ))
  110   IF(CM.GT.SR) SR=CM
      IF(SR.GT.1.E-3*SP) GOTO 130
      LF = 1
  120 L2 = -1
C
C  DETERMINATION OF THE NEXT RADIUS TO BE USED.
C
  130 IF(NR.GE.0) GOTO 140
      FACT = 2.
      IF(L2.EQ.1) FACT=.5
      L1 = L2
      NR = 0
  140 IF(L1.NE.L2) GOTO 160
      IF(NR.GT.0) GOTO 150
      NP = NP+1
      IF(NP-15) 170,170,230
  150 FACT = 1./FACT
  160 FACT = 1./SQRT(FACT)
      NR = NR+1
  170 R = R*FACT
      M = NR-KL-1
      IF(M.LE.0) GOTO 10
C
C  THE RESULTS FOR THE LAST THREE RADII ARE STORED.
C
      DO 180 I=1,N
  180   RT(I,M) = A(I)
      IF(M.EQ.1) GOTO 200
C
C  EXTRAPOLATION.
C
      DO 190 I=1,N
        XK = RT(I,M-1)-RT(I,M)
  190   RT(I,M-1) = RT(I,M)-MULT(FC(M-1),XK)
      IF(M.EQ.3) GOTO 210
C
C  CALCULATION OF THE EXTRAPOLATION CONSTANTS.
C
  200 FC(M) = 1.5+SIGN(.5,FACT-1.)
      IF(M.EQ.2) FC(M)=FC(M)+1.4142135623731
      IF(FACT.GT.1.) FC(M)=-FC(M)
      GOTO 10
  210 FC(3) = FC(1)*FC(2)/(FC(1)+FC(2)+1.)
C
C  FINAL EXTRAPOLATION AND ERROR ESTIMATION.
C
      DO 220 I=1,N
        XK = RT(I,1)-RT(I,2)
        ER(I) = ER(I)+1.E-3*ABS(XK)
  220   RS(I) = RT(I,2)-MULT(FC(3),XK)
      RETURN
C
C  ERROR RETURN.
C
  230 DO 240 I=1,N
        RS(I) = (0.,0.)
  240   ER(I) = 1.E10
      RETURN
      END
