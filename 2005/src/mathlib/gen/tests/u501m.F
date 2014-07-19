*
* $Id: u501m.F,v 1.1.1.1 1996/04/01 15:01:30 mclareni Exp $
*
* $Log: u501m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:30  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE U501M
C     This Subroutine tests the MATHLIB routines DJMNB and DDJMNB (U501)
#include "gen/imp64.inc"

      DIMENSION JI(0:9),MI(0:9),NI(0:9),JH(0:9),MH(0:9),NH(0:9)
      DIMENSION TI(0:9),TH(0:9),DI(0:9),DH(0:9),BE(0:9)

#include "iorc.inc"
      PARAMETER (Z0 = 0, Z1 = 1)
      PARAMETER ( TSTERR=1D-12 )

      DATA JI(0) /  0/, MI(0) /  0/, NI(0) /  0/
      DATA JI(1) /  1/, MI(1) /  0/, NI(1) /  1/
      DATA JI(2) /  3/, MI(2) /  1/, NI(2) / -3/
      DATA JI(3) /  4/, MI(3) / -3/, NI(3) /  2/
      DATA JI(4) /  8/, MI(4) /  7/, NI(4) /  0/
      DATA JI(5) /  9/, MI(5) /  0/, NI(5) /  6/
      DATA JI(6) / 10/, MI(6) /  0/, NI(6) / -8/
      DATA JI(7) / 15/, MI(7) /-15/, NI(7) /-13/
      DATA JI(8) / 20/, MI(8) /-18/, NI(8) /-19/
      DATA JI(9) / 25/, MI(9) /-25/, NI(9) /-25/
      DATA JH(0) /  1/, MH(0) /  1/, NH(0) /  1/
      DATA JH(1) /  3/, MH(1) / -1/, NH(1) /  1/
      DATA JH(2) /  3/, MH(2) /  1/, NH(2) / -3/
      DATA JH(3) /  5/, MH(3) / -3/, NH(3) /  3/
      DATA JH(4) /  7/, MH(4) /  7/, NH(4) /  1/
      DATA JH(5) /  9/, MH(5) / -3/, NH(5) /  5/
      DATA JH(6) / 11/, MH(6) / -7/, NH(6) / -9/
      DATA JH(7) / 15/, MH(7) /-15/, NH(7) /-13/
      DATA JH(8) / 21/, MH(8) /-17/, NH(8) /-19/
      DATA JH(9) / 25/, MH(9) /-23/, NH(9) /-21/

      DATA BE /0,1,45,90,135,180,225,270,315,360/

      W(I)=SQRT(I+Z0)
      CALL HEADER('U501',0)
      ERMAX=0D0

      PI = 3.14159 26535 89793D0

      DO 3 IB = 0,9
      IF(MOD(IB,2) .EQ. 0) WRITE(LOUT,100)
      BETA=BE(IB)
      B=(PI/180)*BETA
      IF(BETA .EQ. 180)THEN
      C=0
      S=1
      ELSEIF(BETA .EQ. 360)THEN
      C=-1
      S=0
      ELSE
      C=COS(B/2)
      S=SIN(B/2)
      ENDIF

      TI(0)=1
      TI(1)=W(2)*C*S
      TI(2)=W(15)*C**2*S**4
      TI(3)=W(14)*C*S**5*(3*C**2-S**2)
      TI(4)=6*W(1430)*(C*S)**7*(-C**2+S**2)
      TI(5)=2*W(2145)*(C*S)**6*(7*C**6-27*C**4*S**2+27*C**2*S**4-7*S**6)
      TI(6)=W(24310)*(C*S)**8*(9*C**4-20*C**2*S**2+9*S**4)
      TI(7)=W(435)*C**28*S**2
      TI(8)=W(78)*C**37*S*(-C**2+19*S**2)
      TI(9)=C**50

      TH(0)=C
      TH(1)=S*(2*C**2-S**2)
      TH(2)=W(3)*C*S**2
      TH(3)=S**3*(4*C**2-S**2)
      TH(4)=-W(35)*C**4*S**3
      TH(5)=W(21)*C*S**4*(5*C**4-6*C**2*S**2+S**4)
      TH(6)=W(5)*C**8*S*(-2*C**2+9*S**2)
      TH(7)=W(15)*C**14*S
      TH(8)=W(10)*C**18*S*(-2*C**2+19*S**2)
      TH(9)=2*W(3)*C**22*S*(2*C**2-23*S**2)

      WRITE(LOUT,'(1X)')
      DO 1 K = 0,9
      AJ=JI(K)
      AM=MI(K)
      AN=NI(K)
#if defined(CERNLIB_DOUBLE)
      DI(K)=DDJMNB(AJ,AM,AN,BETA)
#endif
#if !defined(CERNLIB_DOUBLE)
      DI(K)= DJMNB(AJ,AM,AN,BETA)
#endif
      ER=ABS(DI(K)-TI(K))
      ERMAX=MAX(ERMAX,ER)
      WRITE(LOUT,'(1X,3F6.1,F7.0,3F25.15)')
     1 AJ,AM,AN,BETA,DI(K),TI(K),ER
    1 CONTINUE

      WRITE(LOUT,'(1X)')
      DO 2 K = 0,9
      AJ=JH(K)
      AM=MH(K)
      AN=NH(K)
#if defined(CERNLIB_DOUBLE)
      DH(K)=DDJMNB(AJ/2,AM/2,AN/2,BETA)
#endif
#if !defined(CERNLIB_DOUBLE)
      DH(K)= DJMNB(AJ/2,AM/2,AN/2,BETA)
#endif
      ER=ABS(DH(K)-TH(K))
      ERMAX=MAX(ERMAX,ER)
      WRITE(LOUT,'(1X,3F6.1,F7.0,3F25.15)')
     1 AJ/2,AM/2,AN/2,BETA,DH(K),TH(K),ER
    2 CONTINUE
    3 CONTINUE
      WRITE(LOUT,'(/7X,''TESTING ERROR MESSAGES:''/)')
#if defined(CERNLIB_DOUBLE)
      DI(0)=DDJMNB(45*Z1,Z1,Z1,Z0)
#endif
#if !defined(CERNLIB_DOUBLE)
      DI(0)= DJMNB(45*Z1,Z1,Z1,Z0)
#endif
  100 FORMAT('1'/1X,5X,'J',5X,'M',5X,'N',3X,'BETA',15X,
     1       'DMNJ(BETA)',21X,'TEST',10X,'Error')
      WRITE(LOUT,'('' Largest Error was'',1P,D10.1)') ERMAX
      IRC= ITEST('U501',ERMAX .LE. TSTERR)
      CALL PAGEND('U501')
      RETURN
      END
