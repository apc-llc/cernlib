*
* $Id: cspecb.F,v 1.1.1.1 1996/02/26 17:16:32 mclareni Exp $
*
* $Log: cspecb.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:32  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 02/11/94  13.59.45  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   31/10/94
      SUBROUTINE CSPECB(LISTCB)
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
      CHARACTER *32 CBNAME
      I=LISTCB
 1    IF(I.EQ.0)RETURN
      IERR=0
      CALL CSETCVA(IQ(I+1),IERR)
      IF(IERR.NE.0)THEN
        CALL CSGTIDB(IQ(I+2)-KSADGB,CBNAME,NC)
        print *,' CS Warning: mixed char and arith in common ',
     +            cbname(1:nc)
      ENDIF
      I=IQ(I)
      GO TO 1
      END
