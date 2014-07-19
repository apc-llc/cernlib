*
* $Id: csitgb.F,v 1.1.1.1 1996/02/26 17:16:18 mclareni Exp $
*
* $Log: csitgb.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:18  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 04/10/93  12.12.36  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSITGB(IP)
***-------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cspnts.inc"
#include "comis/cstabps.inc"
      I=MHLOC(KSIDB+NWIDEN)
      IQ(I)=0
      IQ(I+1)=NCIDEN
      DO 1 K=1,NWIDEN
        IQ(I+KSIDB-1+K)=IDEN(K)
   1  CONTINUE
      CALL CSRTGB(I)
      IF(IP.EQ.0)THEN
        IPGB=I
      ELSE
        IQ(IP)=I
      ENDIF
      CSITGB=I
      END
