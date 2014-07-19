*
* $Id: ptor.F,v 1.1.1.1 1996/02/15 17:53:27 mclareni Exp $
*
* $Log: ptor.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:27  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION PTOR(ARG)
C
C
      COMMON/PHYS/PHYSIC(20)
      DIMENSION IPHYS(20)
      EQUIVALENCE(PHYSIC,IPHYS)
      DATA  RANGE/5HRANGE/
C
C
C--                ENTRY PTOR
      A=ARG
      GO TO 10
C
         ENTRY PTOR1                                                     CDC
C        ENTRY PTOR1   (ARG)                                            -CDC
C
      IF (ARG.EQ.0.) GO TO 50
      A=1./ARG
   10 INDIC= 0
      GO TO 30
C
         ENTRY RTOP                                                      CDC
C        ENTRY RTOP    (ARG)                                            -CDC
C
      INDIC= 1
      GO TO 20
C
      ENTRY RTOP1                                                        CDC
C--   ENTRY RTOP1 (ARG)                                                 -CDC
      IF (ARG.EQ.0.) GO TO 50
      INDIC= 2
   20 A=ARG
C
   30 I=ITIT(RANGE)
      J=I+IPHYS(I+2)+3
      IF (INDIC.NE.0)  GO TO 40
      PTOR= FINT(1,A,IPHYS(I+2),PHYSIC(J),PHYSIC(I+3)) /PHYSIC(I+1)
C     RTOP= PTOR                                                        IBM
C     PTOR1= PTOR                                                       IBM
C     RTOP1= PTOR                                                       IBM
      GO TO 100
C
   40 A= A*PHYSIC(I+1)
      PTOR= FINT(1,A,IPHYS(I+2),PHYSIC(I+3),PHYSIC(J))
      IF (INDIC.EQ.1)  GO TO 100
      PTOR= 1./PTOR
C     RTOP= PTOR                                                        IBM
C     PTOR1= PTOR                                                       IBM
C     RTOP1= PTOR                                                       IBM
      GO TO 100
C
   50 PTOR=1.E37
C     PTOR1= PTOR                                                       IBM
C     RTOP1= PTOR                                                       IBM
  100 RETURN
      END
