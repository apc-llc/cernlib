*
* $Id: baryo.F,v 1.1.1.1 1996/01/11 14:14:31 mclareni Exp $
*
* $Log: baryo.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:31  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE BARYO(IQV,ISBARY)
C     ****************************
C-- GENERATES BARYON WITH ISAJET CODE ISBARY
C-- SETS IQV QUARK ENTRIES ORDERED AS IN ISBARY
C-- CREATED: 88/04/27
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
#include "cojets/par.inc"
      INTEGER IQV(3)
C-- SET IQV IN INCREASING ORDER
1     ISWAP=0
      DO 2 I=1,2
      IF (IQV(I+1).LT.IQV(I)) THEN
        JSWAP =IQV(I)
        IQV(I)=IQV(I+1)
        IQV(I+1)=JSWAP
        ISWAP=1
      ENDIF
2     CONTINUE
      IF (ISWAP.EQ.1) GO TO 1
C-- SPIN
      IF(IQV(1).EQ.IQV(2).AND.IQV(2).EQ.IQV(3)) THEN
       JSPIN=1
      ELSE
       JSPIN=CJRN(0)+PS1(IQV(3))
      ENDIF
      IF(JSPIN.EQ.1.OR.IQV(1).EQ.IQV(2).OR.IQV(2).EQ.IQV(3)) GO TO 5
      IF(CJRN(0).GT.PISOPN) GO TO 5
      IQC=IQV(1)
      IQV(1)=IQV(2)
      IQV(2)=IQC
5     CONTINUE
      ISBARY=1000*IQV(1)+100*IQV(2)+10*IQV(3)+JSPIN
      RETURN
      END
