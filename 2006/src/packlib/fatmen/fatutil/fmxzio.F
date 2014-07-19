*
* $Id: fmxzio.F,v 1.1.1.1 1996/03/07 15:18:07 mclareni Exp $
*
* $Log: fmxzio.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:07  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if defined(CERNLIB_CSPACK)
      SUBROUTINE FMXZIO(IBUF,IOWAY)
      DIMENSION IBUF(8192)
#include "zebra/zmach.inc"
#include "zebra/quest.inc"
#include "fatmen/fatbug.inc"
      CHARACTER*6  CHWAY
 
      IRC  = 0
      IF(IDEBFA.GE.3) PRINT *,'FMXZIO. IQUEST(1-6) = ',
     +   (IQUEST(J),J=1,6)
      LUN  = IQUEST(1)
      NREC = IQUEST(4)
      IF(IOWAY.EQ.0) THEN
         CALL XZREAD(LUN,IBUF,NREC,IQUEST(2)*IQCHAW,NGOT,' ',IRC)
      ELSEIF(IOWAY.EQ.1) THEN
         CALL XZRITE(LUN,IBUF,NREC,IQUEST(2)*IQCHAW,' ',IRC)
      ELSE
         WRITE(CHWAY,'(I6)') IOWAY
         CALL ZFATAM('Invalid value for IOWAY in FMXZIO - '//CHWAY)
      ENDIF
 
      IQUEST(1) = IRC
 
      END
#endif
