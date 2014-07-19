*
* $Id: zgtfor.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zgtfor.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZGTFOR(IZ,IST,NW,IW)
*
* author HG       7/4/84
*
* PUTS BANK FORMAT INTO AN ARRAY
*
* INPUT
* IZ        STRUCTURE
* IST       NO. OF FORMAT
* OUTPUT
* NW        NO. OF WORDS, 0 IF NO FORMAT FOUND
* IW        FORMAT IN CONVERTED (INTEGER) FORM (SEE ZECFOR)
*
      PARAMETER (MLINK=6,MFBANK=4)
      DIMENSION IW(*),IZ(*)
      LOGICAL ZIDOK
*
      NW=0
      JMOTH=IZ(IZ(1)-MLINK)
      IF(ZIDOK(IZ,JMOTH))  THEN
         IF (IST.GT.0.AND.IST.LE.IZ(JMOTH+1))  THEN
            NW=IZ(IZ(JMOTH-3)+IST)
            CALL UCOPY(IZ(IZ(JMOTH-4)+IZ(IZ(JMOTH-2)+IST)+1),IW,NW)
         ENDIF
      ENDIF
      END
