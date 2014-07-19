*
* $Id: tiusam.F,v 1.1.1.1 1996/02/15 17:54:57 mclareni Exp $
*
* $Log: tiusam.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:57  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TIUSAM

#include "kerngent/mkcde.inc"

      DIMENSION    MUST(20)
      DIMENSION    KB(100)
      EQUIVALENCE (KB(1),IB(101))

      INTEGER TINF(4)
      DATA TINF /1000,4HIUSA,500,4HWRDS/

      DATA  MUST /  40, 40,  0, 40,100,  0,  0, 20, 21,100
     +,              1, 11, 51, 11,101, 91,120,101,101,101  /
      DATA IWORD1 / 15/
      DATA IWORD2 / -375/

      CALL NEWGUY ('IUSAME.','TIUSAM   ')

C--                PREPARE THE TEST VECTOR  (IB)
         DO 5 I=1,500
    5 KB(I)=I
      CALL UFILL (KB,1,50,6HSAME50)

         DO 10 I=1,100

      KB(I+100)=IWORD1
      KB(I+300)=IWORD2

   10 CONTINUE

C--                SET THE RESULT VECTOR

      IA(1)  =  IUSAME (KB,   1, 40, 10, IA(11))
      IA(2)  =  IUSAME (KB,  11, 50, 40, IA(12))
      IA(3)  =  IUSAME (KB,  11, 50, 41, IA(13))
      IA(4)  =  IUSAME (KB,  11,100, 30, IA(14))
      IA(5)  =  IUSAME (KB,  11,500, 41, IA(15))
      IA(6)  =  IUSAME (KB,  41, 90, 20, IA(16))
      IA(7)  =  IUSAME (KB,  41,119, 20, IA(17))
      IA(8)  =  IUSAME (KB,  41,120, 20, IA(18))
      IA(9)  =  IUSAME (KB,  50,121, 20, IA(19))
      IA(10) =  IUSAME (KB,  41,400, 20, IA(20))

      CALL MVERII  (1,IA,MUST,20)

C--      TIMING
      IF (ITIMES.EQ.0) RETURN
      NTIMES = ITIMES*TINF(1)
      TINF(1) = NTIMES
      CALL TIMED (TIMERD)
         DO 80 J=1,NTIMES
   80 IA(1)=IUSAME (KB,1,500,75,JSAME)
      CALL TIMING (TINF)
      RETURN
      END
