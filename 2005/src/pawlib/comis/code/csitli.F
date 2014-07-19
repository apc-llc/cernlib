*
* $Id: csitli.F,v 1.1.1.1 1996/02/26 17:16:18 mclareni Exp $
*
* $Log: csitli.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:18  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 04/10/93  12.11.27  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSITLI(IP)
***-----------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cspnts.inc"
#include "comis/cstabps.inc"
      I=MHLOC(KSIDL+NWIDEN)
      IQ(I)=0
      IQ(I+1)=NCIDEN
      DO 1 K=1,NWIDEN
        IQ(I+KSIDL-1+K)=IDEN(K)
   1  CONTINUE
      CALL CSRTLI(I)
      IF(IP.EQ.0)THEN
        IPLI=I
      ELSE
        IQ(IP)=I
      ENDIF
      CSITLI=I
      END
