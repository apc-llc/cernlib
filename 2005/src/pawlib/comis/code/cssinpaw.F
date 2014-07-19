*
* $Id: cssinpaw.F,v 1.1.1.1 1996/02/26 17:16:32 mclareni Exp $
*
* $Log: cssinpaw.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:32  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_PAW)
*CMZ :  1.18/09 02/09/94  15.17.49  by  Vladimir Berezhnoi
*-- Author :    Alfred Nathaniel   05/07/94
      SUBROUTINE CSSINP(CHLINE,CHPRMT,NCH)
      CHARACTER*(*) CHLINE,CHPRMT

#include "comis/cslun.inc"

      IF(LUNINP.EQ.5) THEN
*-- Motif compatible keyboard input with prompt
        CALL KUINPS(CHPRMT,CHLINE,NCH)
      ELSE
        CALL KUREAD(LUNINP,CHLINE,NCH)
      ENDIF
      IF(NCH.GT.0) THEN
*-- remove TABs etc.
        NCH=MCLINE(CHLINE)
      ENDIF

      END
#endif
