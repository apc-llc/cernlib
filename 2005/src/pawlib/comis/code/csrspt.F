*
* $Id: csrspt.F,v 1.1.1.1 1996/02/26 17:16:17 mclareni Exp $
*
* $Log: csrspt.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:17  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.34  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSRSPT(IDELPG)
***-----------------------
*   restores saved pointers
***-----------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cspnts.inc"
      DO 1 M=1,3
        J=IPNT(M)
        IF(J.NE.0) THEN
          I=IQ(J)
          K=0
          IF(M.NE.2 .OR. IDELPG.EQ.0)THEN
          CALL CSDTAB(J,I,K)
          ENDIF
        ENDIF
 1    CONTINUE
      IF(IPNT(1).EQ.0)IPGB=0
      IF(IPNT(2).EQ.0)IPGP=0
      IF(IPNT(3).EQ.0)IPGI=0
      CALL CSDTAB(IPLL,IPLL,0)
      CALL CSDTAB(IPLI,IPLI,1)
      CALL CSDTAB(LDATA,LDATA,0)
      IPLI=0
      IPLL=0
      LDATA=0
      END
