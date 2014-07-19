*
* $Id: mxdmal.F,v 1.1.1.1 1996/02/15 17:53:27 mclareni Exp $
*
* $Log: mxdmal.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:27  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE MXDMAL (A,B,R,I,J)
C
C
      DIMENSION A(400), B(400), R(400)
C
C--                ENTRY MXDMAL
      IF((I.EQ.0).OR.(J.EQ.0)) GO TO 100
      IB=1
      DO 21 L=1,I
      DO 21 K=1,J
      R(IB)= R(IB) + B(IB)*A(L)
   21 IB=IB+1
      GO TO 100
C
         ENTRY MXDMAR                                                    CDC
C        ENTRY MXDMAR  (A,B,R,I,J)                                      -CDC
C
      IF((I.EQ.0).OR.(J.EQ.0)) GO TO 100
      IB=1
      DO 31 L=1,I
      DO 31 K=1,J
      R(IB)= R(IB) + A(IB)*B(K)
   31 IB=IB+1
      GO TO 100
C
         ENTRY MXDML                                                     CDC
C        ENTRY MXDML   (A,B,R,I,J)                                      -CDC
C
      IF((I.EQ.0).OR.(J.EQ.0)) GO TO 100
      IB=1
      DO 41 L=1,I
      DO 41 K=1,J
      R(IB)= B(IB)*A(L)
   41 IB=IB+1
      GO TO 100
C
         ENTRY MXDMR                                                     CDC
C        ENTRY MXDMR   (A,B,R,I,J)                                      -CDC
C
      IF((I.EQ.0).OR.(J.EQ.0)) GO TO 100
      IB=1
      DO 51 L=1,I
      DO 51 K=1,J
      R(IB)= A(IB)*B(K)
   51 IB=IB+1
  100 RETURN
      END
