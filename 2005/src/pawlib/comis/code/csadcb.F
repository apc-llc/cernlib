*
* $Id: csadcb.F,v 1.1.1.1 1996/02/26 17:16:32 mclareni Exp $
*
* $Log: csadcb.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:32  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 01/11/94  14.03.45  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   31/10/94
      SUBROUTINE CSADCB(LIST,NUMGB,ISFTG)
*----------------------------------------
*    add element to the head of the list
*  common blocks list:  next;listgi;numgb
*  common block element:next;iptli
*---------------------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
*+SEQ,CSTAB.
      I=LIST
 1    IF(I.EQ.0)THEN
* element not found, add it
        I=MHLOC(3)
        IQ(I)=LIST
        IQ(I+1)=0
        IQ(I+2)=NUMGB
        ISFTG=I+1
        LIST=I
      ELSE
        IF(IQ(I+2).NE.NUMGB)THEN
          I=IQ(I)
          GO TO 1
        ELSE
          ISFTG=I+1
 2        IF(IQ(ISFTG).EQ.0)RETURN
          ISFTG=IQ(ISFTG)
          GO TO 2
        ENDIF
      ENDIF
      END
