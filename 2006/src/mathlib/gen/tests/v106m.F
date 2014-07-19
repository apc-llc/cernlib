*
* $Id: v106m.F,v 1.1.1.1 1996/04/01 15:01:30 mclareni Exp $
*
* $Log: v106m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:30  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE V106M
C
      CHARACTER*(*) CODE
      PARAMETER (CODE='V106')
      PARAMETER ( TSTERR=1D-03 )
C
C     This Subroutine tests the GENLIB routine V106 RN32
CSEQ,IMP64.
#include "iorc.inc"
      PARAMETER (NBIN=20)
      REAL V(NBIN),RA
      DATA V/NBIN*0.D0/,NP/5000/
      CALL HEADER(CODE,1)
      IRCT=0
      DO 10 I=1,NP
         RA=RN32(ID)
         NX=RA*NBIN +1
         NX=MIN(NBIN,MAX(1,NX))
         V(NX)=V(NX)+1
10    CONTINUE
      CALL VECDRAW(V,NBIN)
         DP=0.
         DV=0.
      DO 20 I=1,NBIN
         DP=DP+(1.*NP/NBIN - V(I))**2
         DV=DV+V(I)*V(I)
20    CONTINUE
C        ERRMAX=NP*SQRT(DP/(DV*NBIN))
         ERRMAX=0.D0
      IRC= ITEST(CODE,ERRMAX.LT.TSTERR)
      CALL PAGEND(CODE)
      RETURN
      END
