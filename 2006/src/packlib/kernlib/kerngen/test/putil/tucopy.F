*
* $Id: tucopy.F,v 1.1.1.1 1996/02/15 17:54:57 mclareni Exp $
*
* $Log: tucopy.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:57  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TUCOPY

#include "kerngent/mkcde.inc"

      INTEGER CHECK(10)
      INTEGER TINF(4)

      DATA  CHECK/3*0,4HA   ,4HB   ,4HC   ,4HD   ,3*0/
      DATA   TINF /1000, 4HCOPY, 1000, 4HWORD/

      CALL NEWGUY ('UCOPY-UCOPY2-UCOPYN-UCOPIV.','TUCOPY  ')

      DO    16           NW=1,18
      NR = 20 - NW

      DO 14 J=1,NW
   14 IB(J) = INTG(J)
      DO 15 J=1,NR
   15 IB(J+NW) = INTG(J)

      CALL VZERO (IA,20)
      CALL UCOPY (INTG,IA(NW+1),NR)
      CALL UCOPY (INTG,IA,      NW)
      CALL MVERII (NW,IA,IB,20)

      CALL VZERO  (IA,20)
      CALL UCOPY2 (IB(NW+1),IA(NW+1),NR)
      CALL UCOPY2 (IB,      IA,      NW)
      CALL MVERII (NW+100,IA,IB,20)

      CALL UCOPY2 (IA(NW+1),IA(NW+3),NR)
      CALL UCOPY2 (IA,      IA(3),   NW)
      CALL MVERII (NW+200,IA(3),IB,20)
   16 CONTINUE

      CALL UZERO (A,1,100)
      CALL UCOPY (IBCD(12),A(4),4)
      CALL UCOPY (IBCD,A,0)
      CALL MVERII (51,A,CHECK,10)

      CALL UZERO (CHECK,5,10)
      CALL UZERO (A,1,100)
      CALL UCOPY (IBCD(12),A(4),1)
      CALL MVERII (53,A,CHECK,10)

      CALL UCOPY (INTG,IB,25)
      CALL UCOPY (INTG,IB(26),50)
      CALL UCOPY (INTG,IB(76),25)
      CALL UCOPY (INTG,IA,100)

      CALL UCOPY2 (IA,IA(76),25)
      CALL UCOPY2 (IA,IA,100)
      CALL UCOPY2 (IBCD,IA,0)
      CALL UCOPY2 (IA,IA(26),50)
      CALL MVERII (55,IA,IB,100)

      J=100
         DO 20 I=1,50
      IB(I)=-INTG(I)
      IB(I+50)=INTG(J)
   20 J=J-1

      CALL UCOPYN (INTG,IA,50)
      CALL UCOPYN (INTG(50),IA,0)
      CALL UCOPIV (INTG(51),IA(51),50)
      CALL UCOPIV (IBCD,IA(51),0)
      CALL MVERII (60,IA,IB,100)

C--      TIMING
      IF (ITIMES.EQ.0) RETURN
      NTIMES = ITIMES*TINF(1)
      TINF(1) = NTIMES
      CALL TIMED (TIMERD)

      DO 80 J=1,NTIMES
   80 CALL UCOPY(A,B,1000)
      CALL TIMING (TINF)

      DO 84 NW=1,7
      TINF(3) = NW
      CALL TIMED (TIMERD)

      DO 83 J=1,NTIMES
   83 CALL UCOPY (A,B,NW)
      CALL TIMING (TINF)
   84 CONTINUE
      RETURN
      END
