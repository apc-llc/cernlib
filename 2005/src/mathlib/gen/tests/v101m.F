*
* $Id: v101m.F,v 1.1.1.1 1996/04/01 15:01:30 mclareni Exp $
*
* $Log: v101m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:30  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE V101M
C
      CHARACTER*(*) CODE
      PARAMETER (CODE='V101')
C
C     This Subroutine tests the GENLIB routine V101 NORRAN
C     and draws in the range -5s,5s
CSEQ,IMP64.
#include "iorc.inc"
      PARAMETER (NBIN=50)
      REAL V(NBIN),RA
      DATA V/NBIN*0.D0/
      CALL HEADER(CODE,1)
      IRCT=0
      DO 10 I=1,5000
         CALL NORRAN(RA)
         NX=(RA+5.)*0.1*NBIN+1.
         NX=MIN(NBIN,MAX(1,NX))
         V(NX)=V(NX)+1
10    CONTINUE
      CALL VECDRAW(V,NBIN)
      IRC= ITEST(CODE,IRCT.EQ.0)
      CALL PAGEND(CODE)
      RETURN
      END
