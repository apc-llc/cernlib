*
* $Id: tsortx.F,v 1.1.1.1 1996/02/15 17:55:02 mclareni Exp $
*
* $Log: tsortx.F,v $
* Revision 1.1.1.1  1996/02/15 17:55:02  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "_kerngent/pilot.h"
      SUBROUTINE TSORTX

#include "mkcde.inc"

      EQUIVALENCE (IMAT,IB(101))
      INTEGER TINF(4)
      DATA TINF/1000, 4HSORT, 20, 4HWORD/

      CALL UCOPY (INTG,IA,20)
      CALL UCOPY (INTG(2),IA(21),10)
      IA(31)= 12
      CALL UCOPY (INTG(12),IA(32),9)

      DO 10 I= 1,20
      J=21-I
   10 IA(I+40)=IA(J)

      CALL NEWGUY ('SORTX.','TSORTX  ')
      CALL UCOPY (INTG,IB(101),20)

C----              TEST NORMAL
      CALL SORTX (IMAT, 4, 5,-1,IB)
      CALL SORTX (IMAT, 4, 5, 1,IB)
      CALL MVERII (1,IMAT,IA,20)

      IB(101)=12
      CALL SORTX (IMAT,10, 2, 1,IB)
      CALL SORTX (IMAT, 1,20, 1,IB)
      CALL MVERII (2,IMAT,IA(21),20)

      CALL UCOPY(IA,IMAT,20)
      CALL SORTX (IMAT, 1,20,-1,IB)
      CALL MVERII (3,IMAT,IA(41),20)

C----              TEST FAULTY CALLS
      CALL UCOPY (IA,IMAT,20)
      CALL SORTX (IMAT, 4, 5, 0,IB)
      CALL SORTX (IMAT, 4, 5,100,IB)
      CALL SORTX (IMAT, 4, 0,1,IB)
      CALL MVERII (4,IMAT,IA    ,20)

C--      TIMING
      IF (ITIMES.EQ.0) RETURN
      NTIMES = ITIMES*TINF(1)
      TINF(1) = NTIMES
      CALL TIMED (TIMERD)

      DO 80 J= 1,NTIMES
      CALL SORTX (IMAT,4,5,1,IB)
   80 CONTINUE

      CALL TIMING (TINF)
      RETURN
      END
