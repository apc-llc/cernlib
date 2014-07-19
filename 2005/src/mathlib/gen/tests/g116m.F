*
* $Id: g116m.F,v 1.1.1.1 1996/04/01 15:01:30 mclareni Exp $
*
* $Log: g116m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:30  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE G116M
C     G116  VVILOV     (B. SCHORR,  KSK)

#include "iorc.inc"

      LOGICAL LOK
      DIMENSION RK(5,0:3),RL(5,0:3),RB(5,0:3),VV(5,0:3),RER(0:3)

*      DATA RER /2*0.000025, 2*0.0016/
      DATA RER /2*0.000095, 2*0.0016/

C     DENSITY

      DATA RK(1,0),RL(1,0),RB(1,0),VV(1,0) /0.01,  4. , 0.1, 5.3812E-2/
      DATA RK(2,0),RL(2,0),RB(2,0),VV(2,0) /0.04, -1. , 0.7, 1.7519E-1/
      DATA RK(3,0),RL(3,0),RB(3,0),VV(3,0) /0.2 ,  2.5, 0. , 1.0767E-1/
      DATA RK(4,0),RL(4,0),RB(4,0),VV(4,0) /0.24,  5.5, 0.8, 6.0560E-3/
      DATA RK(5,0),RL(5,0),RB(5,0),VV(5,0) /7.  , -3.8, 0.6, 2.5747E-2/

C     DISTRIBUTION

      DATA RK(1,1),RL(1,1),RB(1,1),VV(1,1) /0.015, 1.5, 0.3, 5.3557E-1/
      DATA RK(2,1),RL(2,1),RB(2,1),VV(2,1) /0.1  ,12. , 0.4, 9.9325E-1/
      DATA RK(3,1),RL(3,1),RB(3,1),VV(3,1) /0.2  ,-0.5, 0.9, 3.6597E-1/
      DATA RK(4,1),RL(4,1),RB(4,1),VV(4,1) /0.28 ,-2. , 0.5, 2.8715E-2/
      DATA RK(5,1),RL(5,1),RB(5,1),VV(5,1) /1.   , 0. , 0. , 6.8799E-1/

C     LOWER LIMIT XL (DENSITY)

      DATA RK(1,2),RL(1,2),RB(1,2),VV(1,2) /0.01,  4. , 0.1,  -3.03/
      DATA RK(2,2),RL(2,2),RB(2,2),VV(2,2) /0.04, -1. , 0.7,  -3.05/
      DATA RK(3,2),RL(3,2),RB(3,2),VV(3,2) /0.2 ,  2.5, 0. ,  -3.05/
      DATA RK(4,2),RL(4,2),RB(4,2),VV(4,2) /0.24,  5.5, 0.8,  -3.16/
      DATA RK(5,2),RL(5,2),RB(5,2),VV(5,2) /7.  , -3.8, 0.6,  -4.07/

C     UPPER LIMIT XU (DISTRIBUTION)

      DATA RK(1,3),RL(1,3),RB(1,3),VV(1,3) /0.015, 1.5, 0.3, 100.06/
      DATA RK(2,3),RL(2,3),RB(2,3),VV(2,3) /0.1  ,12. , 0.4,  23.44/
      DATA RK(3,3),RL(3,3),RB(3,3),VV(3,3) /0.2  ,-0.5, 0.9,  11.44/
      DATA RK(4,3),RL(4,3),RB(4,3),VV(4,3) /0.28 ,-2. , 0.5,  10.67/
      DATA RK(5,3),RL(5,3),RB(5,3),VV(5,3) /1.   , 0. , 0. ,   4.72/

      CALL HEADER('G116',0)

      LOK=.TRUE.
      DO 1 I = 0,3
      DO 1 J = 1,5
      CALL VVISET(RK(J,I),RB(J,I),MOD(I,2),TL,TU)
      IF(I .EQ. 0) TEST=VVIDEN(RL(J,0))
      IF(I .EQ. 1) TEST=VVIDIS(RL(J,1))
      IF(I .EQ. 2) TEST=TL
      IF(I .EQ. 3) TEST=TU
      LOK=LOK .AND. ABS((TEST-VV(J,I))/VV(J,I)) .LE. RER(I)

    1 CONTINUE

      IF(LOK)      WRITE(LOUT,'(7X,''G115 VVILOV **TEST SUCCESFUL**'')')
      IF(.NOT.LOK) WRITE(LOUT,'(7X,''G115 VVILOV **TEST FAILED**'')')

      CALL VVISET(20.,0.,0,XL,XU)
      CALL VVISET(10.,2.,1,XL,XU)

      IRC= ITEST('G116',LOK)
      CALL PAGEND('G116')

      END
