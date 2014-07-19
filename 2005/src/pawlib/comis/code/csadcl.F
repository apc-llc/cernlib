*
* $Id: csadcl.F,v 1.1.1.1 1996/02/26 17:16:32 mclareni Exp $
*
* $Log: csadcl.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:32  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 29/09/94  15.45.27  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSADCL(LSTCL,IPADDR)
***------------------------------------
*    Try add procedure to list of called
***------------------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
**+SEQ,CSTAB.
*
*-  element=( next, address )
*
      I=LSTCL
  1   IF(I.EQ.0)THEN
        IND=MHLOC(2)
        IQ(IND)  =LSTCL
        IQ(IND+1)=IPADDR
        LSTCL=IND
      ELSEIF(IQ(I+1).EQ.IPADDR)THEN
        RETURN
      ELSE
        I=IQ(I)
        GO TO 1
      ENDIF
      END
