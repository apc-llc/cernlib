*
* $Id: csmkvd.F,v 1.1.1.1 1996/02/26 17:16:28 mclareni Exp $
*
* $Log: csmkvd.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:28  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_PAW)
*CMZ :  1.14/02 11/05/93  14.47.47  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSMKVD
***--------------------------------------------
*       marks deleted ku-vector
***--------------------------------------------
      CHARACTER *8 VNAME
#include "comis/cskucs.inc"
      CALL KUVNAM(VNAME)
      IF(VNAME.EQ.'*')THEN
         DO 1 I=1,KUVCUR
           KUVLNK(I)=0
 1       CONTINUE
         KUVCUR=0
         RETURN
      ENDIF
      DO 2 I=1,KUVCUR
        IF(VNAME.EQ.KUVNMS(I))GO TO 3
 2    CONTINUE
        RETURN
 3    KUVLNK(I)=0
      END
#endif
