*
* $Id: fmvmio.F,v 1.1.1.1 1996/03/07 15:18:06 mclareni Exp $
*
* $Log: fmvmio.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:06  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if defined(CERNLIB_IBMVM)
      SUBROUTINE FMVMIO(IBUF,IOWAY)
      DIMENSION IBUF(8192)
#include "zebra/zmach.inc"
#include "zebra/quest.inc"
#include "fatmen/fatbug.inc"
      CHARACTER*6  CHWAY
 
      IRC  = 0
      IF(IDEBFA.GE.3) PRINT *,'FMVMIO. IQUEST(1-6) = ',
     +   (IQUEST(J),J=1,6)
      LUN  = IQUEST(1)
      NREC = IQUEST(4)
      IF(IOWAY.EQ.0) THEN
         CALL VMRNDM(LUN,IBUF,IQUEST(2)*IQCHAW,LENDAT,NREC,IRC,INFO)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMVMIO. return code ',IRC,
     +         ' from VMRNDM. Info = ',INFO
         ENDIF
      ELSEIF(IOWAY.EQ.1) THEN
         CALL VMRITE(LUN,IBUF,IQUEST(2)*IQCHAW,IRC,INFO)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMVMIO. return code ',IRC,
     +         ' from VMRITE. Info = ',INFO
         ENDIF
      ELSE
         WRITE(CHWAY,'(I6)') IOWAY
         CALL ZFATAM('Invalid value for IOWAY in FMVMIO - '//CHWAY)
      ENDIF
 
      IQUEST(1) = IRC
 
      END
#endif
