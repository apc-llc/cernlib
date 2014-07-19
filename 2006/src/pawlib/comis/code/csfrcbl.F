*
* $Id: csfrcbl.F,v 1.1.1.1 1996/02/26 17:16:33 mclareni Exp $
*
* $Log: csfrcbl.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:33  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 01/11/94  12.25.04  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   31/10/94
      SUBROUTINE CSFRCBL(LIST)
*----------------------------------------
*  free memory
*  common blocks list:  next;listgi;numgb
*  common block element:next;iptli
*---------------------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
*+SEQ,CSTAB.
      I=LIST
 1    IF(I.GT.0)THEN
        II=IQ(I+1)
 2      IF(II.GT.0)THEN
          K=IQ(II)
          CALL MHFREE(II)
          II=K
          GO TO 2
         ENDIF
         K=IQ(I)
         CALL MHFREE(I)
         I=K
         GO TO 1
       ENDIF
       LIST=0
       END
