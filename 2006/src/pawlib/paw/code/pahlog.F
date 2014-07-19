*
* $Id: pahlog.F,v 1.1.1.1 1996/03/01 11:38:40 mclareni Exp $
*
* $Log: pahlog.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:40  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.02/01 04/05/93  09.16.25  by  O.Couet
*-- Author :    O.Couet   03/05/93
      SUBROUTINE PAHLOG(LOGX,LOGY,LOGZ)
#include "paw/quest.inc"
      CALL HPLOPT('LOGX',-1)
      LOGX = IQUEST(11)
      CALL HPLOPT('LOGY',-1)
      LOGY = IQUEST(11)
      CALL HPLOPT('LOGZ',-1)
      LOGZ = IQUEST(11)
      END
