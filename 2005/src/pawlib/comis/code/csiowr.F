*
* $Id: csiowr.F,v 1.1.1.1 1996/02/26 17:16:24 mclareni Exp $
*
* $Log: csiowr.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:24  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.58  by  Rene Brun
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSIOWR(LUN)
#include "comis/csfmt.inc"
      CSIOWR=0
      LUNIO=LUN
      IF(LUN.EQ.-1)LUNIO=6
      BUF=' '
      IBF=1
      IBFLST=1
      LBUF=132
      IF(IFMT.EQ.0)THEN
        IFMTST=0
      ELSEIF(IFMT.EQ.-1)THEN
        LUBTOT=512
        IUBS=MHLOC(LUBTOT)
        LULIST=0
      ELSE
        CALL CSFMTC(-1)
      ENDIF
      CSIOWR=1
      END
