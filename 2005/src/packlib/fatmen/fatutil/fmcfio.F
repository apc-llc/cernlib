*
* $Id: fmcfio.F,v 1.1.1.1 1996/03/07 15:18:07 mclareni Exp $
*
* $Log: fmcfio.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:07  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if defined(CERNLIB_VAXVMS)
      SUBROUTINE FMCFIO(IBUF,IOWAY)
      DIMENSION IBUF(8192)
#include "zebra/zmach.inc"
#include "zebra/quest.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatinfo.inc"
      CHARACTER*6  CHWAY
 
      IRC  = 0
      IF(IDEBFA.GE.3) PRINT *,'FMCFIO. IQUEST(1-6) = ',
     +   (IQUEST(J),J=1,6)
      LUN  = IQUEST(1)
      NREC = IQUEST(4)
*
*     Medium
*
      IF(LFMODE(LUN).EQ.3) THEN
         MEDIUM = 1
      ELSE
         MEDIUM = 0
      ENDIF
*
*     File pointer
*
      LUNDES = IFPNTR(LUN)
 
      IF(IOWAY.EQ.0) THEN
         IF(NREC.GT.0) THEN
            CALL CFSEEK(LUNDES,MEDIUM,IQUEST(2),NREC-1,IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.-3) PRINT *,'FMCFIO. return code ',IRC,
     +         ' from CFSEEK.'
               GOTO 99
            ENDIF
         ENDIF
         CALL CFGET(LUNDES,MEDIUM,IQUEST(2),IQUEST(2),IBUF,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMCFIO. return code ',IRC,
     +         ' from CFGET.'
         ENDIF
      ELSEIF(IOWAY.EQ.1) THEN
         CALL CFPUT(LUNDES,MEDIUM,IQUEST(2),IBUF,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMCFIO. return code ',IRC,
     +         ' from CFPUT.'
         ENDIF
      ELSE
         WRITE(CHWAY,'(I6)') IOWAY
         CALL ZFATAM('Invalid value for IOWAY in FMCFIO - '//CHWAY)
      ENDIF
 
99    CONTINUE
 
      IQUEST(1) = IRC
 
      END
#endif
