*
* $Id: v110m.F,v 1.1.1.1 1996/04/01 15:01:30 mclareni Exp $
*
* $Log: v110m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:30  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE V110M
C
      CHARACTER*(*) CODE
      PARAMETER (CODE='V110')
C
C     This Subroutine tests the GENLIB routine V110 POISSN
CSEQ,IMP64.
#include "iorc.inc"
      PARAMETER (NBIN=20)
      REAL V(NBIN),X
      DATA V/NBIN*0.D0/,X/3.9/
      CALL HEADER(CODE,1)
      IRCT=0
      DO 10 I=1,2000
         CALL POISSN(X,NX,IRC)
         IRCT=IRCT+IRC
         IF(NX.GT.0.AND.NX.LE.NBIN) V(NX)=V(NX)+1
10    CONTINUE
      CALL VECDRAW(V,NBIN)
      IRC= ITEST(CODE,IRCT.EQ.0)
      CALL PAGEND(CODE)
      RETURN
      END
