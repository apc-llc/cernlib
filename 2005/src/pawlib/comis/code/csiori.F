*
* $Id: csiori.F,v 1.1.1.1 1996/02/26 17:16:24 mclareni Exp $
*
* $Log: csiori.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:24  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.58  by  Rene Brun
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSIORI(STR)
      CHARACTER *(*) STR
#include "comis/csfmt.inc"
      CSIORI=0
      BUF=' '
      IBF=1
      IBFLST=1
      LBUF=132
      IF(IFMT.GT.0)CALL CSFMTC(-1)
      IF(IFMTST.EQ.0)THEN
        IF(LENSTR.GT.LBUF)THEN
      IF(LIOERR.EQ.-1) PRINT *,' Length of internal file > 256 '
          IFMTST=4
          LENSTR=LBUF
        ENDIF
        LBUF=LENSTR
        BUF(:LBUF)=STR
      ENDIF
      CSIORI=1
      END
