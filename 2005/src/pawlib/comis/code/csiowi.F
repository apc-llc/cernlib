*
* $Id: csiowi.F,v 1.1.1.1 1996/02/26 17:16:24 mclareni Exp $
*
* $Log: csiowi.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:24  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.58  by  Rene Brun
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSIOWI(NNN)
#include "comis/csfmt.inc"
      CSIOWI=0
      BUF=' '
      IF(IFMT.EQ.0)THEN
        IFMTST=0
        IBF=1
        IBFLST=1
      ELSE
        CALL CSFMTC(-1)
      ENDIF
      LBUF=LENSTR
      CSIOWI=1
      END
