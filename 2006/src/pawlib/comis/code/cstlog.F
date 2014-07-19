*
* $Id: cstlog.F,v 1.1.1.1 1996/02/26 17:16:17 mclareni Exp $
*
* $Log: cstlog.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:17  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.17/07 16/12/93  16.51.29  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSTLOG(JPM)
***----------------------------
*       puts text of comis routine into log-file
***----------------------------
#include "comis/csrec.inc"
#include "comis/cslun.inc"
      IF(ISTLOG.EQ.0)RETURN
      J=JPM
 1    IF(J.EQ.0)RETURN
      CALL CSGETL(J,REC1,N,M)
      WRITE(LUNLOG,'(A)')REC1(1:N)
      GO TO 1
      END
