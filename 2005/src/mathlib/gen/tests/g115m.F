*
* $Id: g115m.F,v 1.1.1.1 1996/04/01 15:01:30 mclareni Exp $
*
* $Log: g115m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:30  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE G115M
C     G115  VAVLOV     (MONTAGNA & ROTONDI, ADAPTED BY KSK)

#include "iorc.inc"

      LOGICAL LOK
      DIMENSION RK(5,0:3),RL(5,0:3),RB(5,0:3),VV(5,0:3)

C     DENSITY

      DATA RK(1,0),RL(1,0),RB(1,0),VV(1,0) /0.01,  4. , 0.1, 5.4016E-2/
      DATA RK(2,0),RL(2,0),RB(2,0),VV(2,0) /0.04, -1. , 0.7, 1.7579E-1/
      DATA RK(3,0),RL(3,0),RB(3,0),VV(3,0) /0.2 ,  2.5, 0. , 1.0826E-1/
      DATA RK(4,0),RL(4,0),RB(4,0),VV(4,0) /0.24,  5.5, 0.8, 6.4414E-3/
      DATA RK(5,0),RL(5,0),RB(5,0),VV(5,0) /7.  , -3.8, 0.6, 2.5655E-2/

C     DISTRIBUTION

      DATA RK(1,1),RL(1,1),RB(1,1),VV(1,1) /0.015, 1.5, 0.3, 5.2739E-1/
      DATA RK(2,1),RL(2,1),RB(2,1),VV(2,1) /0.1  ,12. , 0.4, 9.9107E-1/
      DATA RK(3,1),RL(3,1),RB(3,1),VV(3,1) /0.2  ,-0.5, 0.9, 3.6634E-1/
      DATA RK(4,1),RL(4,1),RB(4,1),VV(4,1) /0.28 ,-2. , 0.5, 2.2229E-2/
      DATA RK(5,1),RL(5,1),RB(5,1),VV(5,1) /1.   , 0. , 0. , 6.8794E-1/

C     RANDOM NUMBERS (VAVRND)

      DATA RK(1,2),RL(1,2),RB(1,2),VV(1,2) /0.015,0.53, 0.4, 1.5172/
      DATA RK(2,2),RL(2,2),RB(2,2),VV(2,2) /0.1  ,0.19, 0.7,-0.8591/
      DATA RK(3,2),RL(3,2),RB(3,2),VV(3,2) /0.2  ,0.98, 0.9, 4.4897/
      DATA RK(4,2),RL(4,2),RB(4,2),VV(4,2) /0.28 ,0.02, 0.1,-1.9712/
      DATA RK(5,2),RL(5,2),RB(5,2),VV(5,2) /1.   ,0.72, 0. , 0.1003/

C     RANDOM NUMBERS (VAVRAN)

      DATA RK(1,3),RL(1,3),RB(1,3),VV(1,3) /0.015,0.53, 0.4, 1.5177/
      DATA RK(2,3),RL(2,3),RB(2,3),VV(2,3) /0.1  ,0.19, 0.7,-0.8589/
      DATA RK(3,3),RL(3,3),RB(3,3),VV(3,3) /0.2  ,0.98, 0.9, 4.4914/
      DATA RK(4,3),RL(4,3),RB(4,3),VV(4,3) /0.28 ,0.02, 0.1,-1.9710/
      DATA RK(5,3),RL(5,3),RB(5,3),VV(5,3) /1.   ,0.72, 0. , 0.1003/

      CALL HEADER('G115',0)

      LOK=.TRUE.
      DO 1 I = 0,3
      DO 1 J = 1,5
      CALL VAVSET(RK(J,I),RB(J,I),MIN(I,1))
      IF(I .EQ. 0) TEST=VAVDEN(RL(J,0))
      IF(I .EQ. 1) TEST=VAVDIS(RL(J,1))
      IF(I .EQ. 2) TEST=VAVRND(RL(J,2))
      IF(I .EQ. 3) TEST=VAVRAN(RK(J,3),RB(J,3),RL(J,3))
      LOK=LOK .AND. ABS((TEST-VV(J,I))/VV(J,I)) .LE. 0.00075
    1 CONTINUE

      DO 2 I = 0,3
      DO 2 J = 1,5
      CALL VAVSET(RK(J,I),RB(J,I),1)
      IF(I .EQ. 0) TEST=VAVDEN(RL(J,0))
      IF(I .EQ. 1) TEST=VAVDIS(RL(J,1))
      IF(I .EQ. 2) TEST=VAVRND(RL(J,2))
      IF(I .EQ. 3) TEST=VAVRAN(RK(J,3),RB(J,3),RL(J,3))
      LOK=LOK .AND. ABS((TEST-VV(J,I))/VV(J,I)) .LE. 0.00075
    2 CONTINUE

      IF(LOK)      WRITE(LOUT,'(7X,''G115 VAVLOV **TEST SUCCESFUL**'')')
      IF(.NOT.LOK) WRITE(LOUT,'(7X,''G115 VAVLOV **TEST FAILED**'')')

      IRC= ITEST('G115',LOK)
      CALL PAGEND('G115')

      END
