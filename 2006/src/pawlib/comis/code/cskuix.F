*
* $Id: cskuix.F,v 1.1.1.1 1996/02/26 17:16:22 mclareni Exp $
*
* $Log: cskuix.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:22  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_PAW)
*CMZ :          08/11/95  19.37.29  by  Julian Bunn
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSKUIX(VNAME,LLOW)
***--------------------------------------------
*       test was ku-vector declared early
***--------------------------------------------
      CHARACTER *(*) VNAME
#include "comis/cskucs.inc"
      DO 1 I=1,KUVCUR
        IF(VNAME.EQ.KUVNMS(I))GO TO 2
 1    CONTINUE
      IF(KUVCUR.EQ.KUVTOT)THEN
        PRINT *,' CS: More then 500 KUIP vectors'
        CSKUIX=0
        RETURN
      ENDIF
      KUVCUR=KUVCUR+1
      KUVNMS(KUVCUR)=VNAME
      I=KUVCUR
 2    KUVLNK(I)=LLOW-KUVOFS
      CSKUIX=I
      END
#endif
