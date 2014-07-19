*
* $Id: tcocop.F,v 1.1.1.1 1996/02/15 17:54:57 mclareni Exp $
*
* $Log: tcocop.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:57  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TCOCOP

#include "kerngent/mkcde.inc"

      INTEGER TINF(4)
      DATA   TINF /1000, 4HUCOP, 100, 4H5X20/

      CALL NEWGUY ('UCOCOP-UDICOP.','TCOCOP  ')
      IDO=5
      IW=10
      NG=20
      NA=15
      IG=1
      I1=1
      CALL UCOPY (INTG,A,100)
      CALL UCOPY (INTG,B,100)

         DO 10 I=1,IDO
      CALL UCOPY (INTG(IG),A(I1),IW)
      IG=IG+NG
   10 I1=I1+NA

      CALL UCOCOP (INTG,B,IDO,IW,NG,NA)
      CALL UCOCOP (IBCD,B,0,IW,NG,NA)
      CALL UCOCOP (IBCD,B,IDO,0,NG,NA)
      CALL MVERII (1,B,A,100)

      CALL UCOPY (INTG,B,100)
      CALL UDICOP (INTG,B,IDO,IW,NG,NA)
      CALL UDICOP (IBCD,B,0,IW,NG,NA)
      CALL UDICOP (IBCD,B,IDO,0,NG,NA)
      CALL MVERII (10,B,A,100)

      NB=25
      CALL UCOPY (INTG,B,100)
      CALL UCOCOP (INTG,B,IDO,NG,NG,NB)
      CALL UCOCOP (B,A,IDO,NG,NB,NG)
      CALL MVERII (2,A,INTG,100)

      CALL UCOPY (INTG,A,100)
      CALL UDICOP (A,A,IDO,NG,NG,NB)
      CALL MVERII (20,A,B,100)

C--      TIMING
      IF (ITIMES.EQ.0) RETURN
      NTIMES = ITIMES*TINF(1)
      TINF(1) = NTIMES
      CALL TIMED (TIMERD)

         DO 80 J=1,NTIMES
   80 CALL UCOCOP (B,A,IDO,NG,NB,NA)

      CALL TIMING (TINF)
      RETURN

      END
