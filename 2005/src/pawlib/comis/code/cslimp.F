*
* $Id: cslimp.F,v 1.1.1.1 1996/02/26 17:16:18 mclareni Exp $
*
* $Log: cslimp.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:18  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.36  by  Rene Brun
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSLIMP(L1,L2,ITYPE,LENEL)
***--------------------------
*        looks for implicit intervals
***--------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cspnts.inc"
      N=IQ(IBIMPL)
      I=N*4+1
      IF(I.EQ.LIMPL)THEN
        LIMPL=LIMPL+8
        I1=MHLOC(LIMPL)
        CALL CCOPYA(IQ(IBIMPL),IQ(I1),I)
        CALL MHFREE(IBIMPL)
        IBIMPL=I1
      ENDIF
      CSLIMP=0
      I1=IBIMPL+1
      DO 1 K=1,N
      IF(L1.GE.IQ(I1).AND.L1.LE.IQ(I1+1))GO TO 2
      IF(L2.GE.IQ(I1).AND.L2.LE.IQ(I1+1))GO TO 2
  1   I1=I1+4
      CSLIMP=1
      IQ(IBIMPL)=N+1
      I1=IBIMPL+4*N+1
      IQ(I1)=L1
      IQ(I1+1)=L2
      IQ(I1+2)=-ITYPE
      IQ(I1+3)=LENEL
  2   END
