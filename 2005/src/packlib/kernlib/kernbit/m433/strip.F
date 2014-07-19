*
* $Id: strip.F,v 1.1.1.1 1996/02/15 17:47:49 mclareni Exp $
*
* $Log: strip.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:49  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      CHARACTER*(*) FUNCTION STRIP(STR,CHOPT,CHD)
C
C CERN PROGLIB# M433    STRIP           .VERSION KERNBIT  1.07  910903
C Author: M.Marquina 91/03/03
C
C-    Remove Leading/Trailing CHD characters from string STR
C
      CHARACTER*(*) STR,CHOPT
      CHARACTER*1   CHD,CHO
#include "m433cs.inc"
      LSTR=LENOCC(STR)
      CHO=CHOPT(1:1)
      CALL CLTOU(CHO)
                     MODE=0
      IF(CHO.EQ.'L') MODE=1
      IF(CHO.EQ.'T') MODE=2
      IS=1
      IE=LSTR
      IF(MODE.LE.1) THEN
        DO WHILE(STR(IS:IS).EQ.CHD.AND.IS.LT.LSTR)
           IS=IS+1
        ENDDO
        IF(MODE.EQ.0) MODE=3
      ENDIF
      IF(MODE.GE.2) THEN
        DO WHILE(STR(IE:IE).EQ.CHD.AND.IE.GT.IS)
           IE=IE-1
        ENDDO
      ENDIF
      STRIP=STR(IS:IE)
      RETURN
      END
