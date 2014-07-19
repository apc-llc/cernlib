*
* $Id: mdpaus.F,v 1.1.1.1 1996/02/26 17:16:56 mclareni Exp $
*
* $Log: mdpaus.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:56  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/02 08/07/93  18.34.40  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE MDPAUS(TEXT)
*      CHARACTER REPLY*20
      CHARACTER *(*) TEXT
      PRINT *,TEXT
*  1   CALL CSSINP(REPLY,'CONTINUE?(Y/N):',NS)
*      IF(REPLY.EQ.'Y'.OR.REPLY.EQ.'YES')RETURN
*      IF(REPLY.EQ.'N'.OR.REPLY.EQ.'NO')STOP'COMIS: PAUSE'
*      GO TO 1
      END
