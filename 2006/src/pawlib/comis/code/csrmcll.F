*
* $Id: csrmcll.F,v 1.1.1.1 1996/02/26 17:16:32 mclareni Exp $
*
* $Log: csrmcll.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:32  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 29/09/94  16.49.37  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSRMCLL(LSTCL)
***------------------------------------
*    Clear list of called
***------------------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
**+SEQ,CSTAB.
*
*-  element=( next, address )
*
      I=LSTCL
  1   IF(I.EQ.0)THEN
        LSTCL=0
        RETURN
      ELSE
        I1=I
        I=IQ(I)
        CALL MHFREE(I1)
        GO TO 1
      ENDIF
      END
