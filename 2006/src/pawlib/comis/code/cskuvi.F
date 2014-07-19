*
* $Id: cskuvi.F,v 1.1.1.1 1996/02/26 17:16:28 mclareni Exp $
*
* $Log: cskuvi.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:28  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :          08/11/95  19.35.45  by  Julian Bunn
*-- Author :    Vladimir Berezhnoi   04/05/93
      INTEGER FUNCTION CSKUVI(VNAME)
***--------------------------------------------
*       returns index of the ku-vector
***--------------------------------------------
      CHARACTER  VNAME*(*)
#include "comis/cskucs.inc"
      DO 1 I=1,KUVCUR
        IF(VNAME.EQ.KUVNMS(I))GO TO 2
 1    CONTINUE
      CSKUVI=-1
      RETURN
 2    CSKUVI=I
**??      CALL CSMARK(1)
      END

