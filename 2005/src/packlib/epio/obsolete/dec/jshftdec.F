*
* $Id: jshftdec.F,v 1.1.1.1 1996/03/08 15:21:53 mclareni Exp $
*
* $Log: jshftdec.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:53  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if (defined(CERNLIB_DECS))&&(defined(CERNLIB_NEVER))
      INTEGER FUNCTION JSHFTC(IW,M1,M2)
C   Hopefully temporary fix for the Decstation where ISHFTC extends the
C     sign bit for ishftc(iw,16,32)
      INTEGER IW,M1,M2
      IF(ABS(M1).EQ.16)THEN
         JSHFTC=IOR(ISHFT(IW,-16) , ISHFT(IW,16))
      ELSE
         JSHFTC=ISHFTC(IW,M1,M2)
      ENDIF
      END
#endif
