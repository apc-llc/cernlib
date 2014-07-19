*
* $Id: hfetc1.F,v 1.1.1.1 1996/01/16 17:07:36 mclareni Exp $
*
* $Log: hfetc1.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:36  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.47  by  Rene Brun
*-- Author :
      SUBROUTINE HFETC1(NBP,NC)
*.==========>
*.            Auxiliary for HFETCH
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcflag.inc"
      DIMENSION IHCV(99)
      EQUIVALENCE (IHCV(1),HCV(1))
*.___________________________________________
      DO 10 I=0,NC+1
         IF(NBP.EQ.NBIT)THEN
            W=HCV(IFW+I+1)
         ELSE
            L1=I*NBP
            NBITH=NBIT-MOD(NBIT,NBP)
            L2=MOD(L1,NBITH)+1
            L1=IFW+1+L1/NBITH
            W=JBYT(IHCV(L1),L2,NBP)
         ENDIF
         CALL HFCX(I,W)
  10  CONTINUE
      IQ(LCONT+KNOENT)=IHCV(IFW)
      LW=LQ(LCONT)
      IF(LW.NE.0)THEN
         NBB=NBIT/NBP
         NW=(NC+2)/NBB+1
         DO 20 I=1,NC
            J=IFW+I+NW+1
            Q(LW+I)=HCV(J)
  20     CONTINUE
      ENDIF
      END
