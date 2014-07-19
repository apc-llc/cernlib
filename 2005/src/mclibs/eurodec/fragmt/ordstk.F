*
* $Id: ordstk.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: ordstk.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE ORDSTK(A,IB,N)
C.----------------------------------------------------------------------
C.
C.    ROUTINE TO ORDER N NUMBERS CONTAINED IN ARRAY A
C.    LAST UPDATE: 10/04/88
C.
C.-----------------------------------------------------------------------
#include "eurodec/hadgen.inc"
      PARAMETER (IDMAX=(NHMAX*(NHMAX+1))/2)
      DIMENSION A(IDMAX),IB(IDMAX),LT(IDMAX),RT(IDMAX)
      INTEGER R,RT
C--
C--   INITIALISE
      LEVEL=1
      LT(1)=1
      RT(1)=N
   10 L=LT(LEVEL)
      R=RT(LEVEL)
      LEVEL=LEVEL-1
   20 IF (R.LE.L) IF (LEVEL) 80,80,10
C--
C--   SUBDIVIDE THE INTERVAL L,R
C--   L : LOWER LIMIT OF THE INTERVAL (INPUT)
C--   R : UPPER LIMIT OF THE INTERVAL (INPUT)
C--   J : UPPER LIMIT OF LOWER SUB-INTERVAL (OUTPUT)
C--   I : LOWER LIMIT OF UPPER SUB-INTERVAL (OUTPUT)
      I=L
      J=R
      M=(L+R)/2
      X=A(M)
   30 IF (A(I).GE.X) GOTO 40
      I=I+1
      GOTO 30
   40 IF (A(J).LE.X) GOTO 50
      J=J-1
      GOTO 40
   50 IF (I.GT.J) GOTO 60
      W=A(I)
      A(I)=A(J)
      A(J)=W
      W=IB(I)
      IB(I)=IB(J)
      IB(J)=W
      I=I+1
      J=J-1
      IF (I.LE.J) GOTO 30
   60 LEVEL=LEVEL+1
      IF ((R-I).GE.(J-L)) GOTO 70
      LT(LEVEL)=L
      RT(LEVEL)=J
      L=I
      GOTO 20
   70 LT(LEVEL)=I
      RT(LEVEL)=R
      R=J
      GOTO 20
   80 RETURN
      END
