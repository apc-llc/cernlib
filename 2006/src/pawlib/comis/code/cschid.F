*
* $Id: cschid.F,v 1.1.1.1 1996/02/26 17:16:29 mclareni Exp $
*
* $Log: cschid.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:29  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 17/02/94  16.39.17  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSCHID(NAME)
***--------------------------
      CHARACTER *(*)NAME
#include "comis/cspar.inc"
#include "comis/cspnts.inc"
      N=LEN(NAME)
   1  IF(N.LE.0)GO TO 2
      IF(NAME(N:N).EQ.' ')THEN
         N=N-1
         GO TO 1
      ENDIF
   2  NCIDEN=N
      NWIDEN=(N+3)/4
      CALL UCTOH(NAME,IDEN,4,NCIDEN)
**      IDEN(NWIDEN)=KBLN
**      CALL CCOPYS(MJSCHA(NAME),JID,NCIDEN)
      END
