*
* $Id: mxadd.F,v 1.1.1.1 1996/02/15 17:53:27 mclareni Exp $
*
* $Log: mxadd.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:27  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE MXADD(A,B,C,I,J)
C
C
      DIMENSION A(400),B(400),C(400)
C
C
C--                ENTRY MXADD
      N=I*J
      IF(N.EQ.0) GO TO 100
      DO 11 L=1,N
   11 C(L)=A(L)+B(L)
      GO TO 100
C
         ENTRY MXSUB                                                     CDC
C        ENTRY MXSUB   (A,B,C,I,J)                                      -CDC
C
      N=I*J
      IF(N.EQ.0) GO TO 100
      DO 21 L=1,N
   21 C(L)=A(L)-B(L)
      GO TO 100
C
         ENTRY MXTRA                                                     CDC
C        ENTRY MXTRA   (A,B,C,I,J)                                      -CDC
C
      N=I*J
      IF (N.GT.0)  CALL UCOPY (A,C,N)
      GO TO 100
C
         ENTRY MXMTR                                                     CDC
C        ENTRY MXMTR   (A,B,C,I,J)                                      -CDC
C
      N=I*J
      IF(N.EQ.0) GO TO 100
      DO 41 L=1,N
   41 C(L)=B(1)*A(L)
      GO TO 100
C
         ENTRY MXNTR                                                     CDC
C        ENTRY MXNTR   (A,B,C,I,J)                                      -CDC
C
      N=I*J
      IF(N.EQ.0) GO TO 100
      DO 51 L=1,N
   51 C(L)=-A(L)
C
  100 RETURN
      END
