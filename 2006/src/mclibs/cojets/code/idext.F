*
* $Id: idext.F,v 1.1.1.1 1996/01/11 14:14:38 mclareni Exp $
*
* $Log: idext.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:38  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION IDEXT(IDIN)
C     ********************
C-- GIVES ISAJET ID OF INTERNAL ID
#include "cojets/data1.inc"
      IDINA=ABS(IDIN)
      IF(IDINA.GT.0.AND.IDINA.LE.352) THEN
        IDEXT=IDENTF(IDINA)
      ELSEIF(IDINA.GE.1000.AND.IDINA.LT.10000) THEN
        IDEXT=IDIN/1000
      ELSEIF(IDINA.EQ.0) THEN
        IDEXT=IDGL
      ELSEIF(IDINA.GT.10000) THEN
        IDEXT=IDIN/10
      ELSE
        IDEXT=0
      ENDIF
      RETURN
      END
