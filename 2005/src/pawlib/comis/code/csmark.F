*
* $Id: csmark.F,v 1.1.1.1 1996/02/26 17:16:29 mclareni Exp $
*
* $Log: csmark.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:29  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.17/07 16/12/93  12.24.14  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
       SUBROUTINE CSMARK(MARK)
***--------------------------------
*      puts mark on SOURCE
***--------------------------------
*---
*+SEQ,CSREC.
*+SEQ,CSKEYS.
*      IF(NSTRG.GT.MXSLIN)RETURN
*      IF(MARK.EQ.1)THEN
*        NSOURC(NSTRG)=NSOURC(NSTRG) + MARK
*      ELSE
*        NSOURC(NSTRG)=MARK
*      ENDIF
*      END
*---------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cspnts.inc"
      IF(MARK.EQ.1)THEN
        IQ(JPMC+1)=IQ(JPMC+1) + MARK
      ELSE
        IQ(JPMC+1)=MARK
      ENDIF
      END
