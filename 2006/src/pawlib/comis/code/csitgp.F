*
* $Id: csitgp.F,v 1.1.1.1 1996/02/26 17:16:18 mclareni Exp $
*
* $Log: csitgp.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:18  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 04/10/93  12.14.52  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSITGP(IP)
***------------------------------
*   it is last routine from the tables-set.
***-----------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cspnts.inc"
#include "comis/cstabps.inc"
      I=MHLOC(KSIDP+NWIDEN)
      IQ(I)=0
      IQ(I+1)=NCIDEN
      DO 1 K=1,NWIDEN
        IQ(I+KSIDP-1+K)=IDEN(K)
   1  CONTINUE
      CALL CSRTGP(I)
      IF(IP.EQ.0)THEN
        IPGP=I
      ELSE
        IQ(IP)=I
      ENDIF
      CSITGP=I
      END
