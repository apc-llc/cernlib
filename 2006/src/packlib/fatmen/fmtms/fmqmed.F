*
* $Id: fmqmed.F,v 1.1.1.1 1996/03/07 15:18:15 mclareni Exp $
*
* $Log: fmqmed.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:15  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMQMED(GENAM,LBANK,KEYS,IMEDIA,IROBOT,IRC)
*CMZ :          22/04/91  12.02.04  by  Jamie Shiers
*-- Author :    Jamie Shiers   22/04/91
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fmnkeys.inc"
      DIMENSION     KEYS(LKEYFA)
      CHARACTER*(*) GENAM
      CHARACTER*6   VID
      CHARACTER*15  XVID
      CHARACTER*8   VIP
#include "fatmen/tmsdef.inc"
 
      IRC    = 0
      IMEDIA = -1
      IROBOT = -1
      LGN    = LENOCC(GENAM)
 
      IF(LBANK.EQ.0) THEN
         CALL FMGETK(GENAM(1:LGN),LBANK,KEYS,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMQMED. return code ',
     +         IRC,' from FMGETK'
            RETURN
         ENDIF
      ENDIF
 
      IMEDIA = IQ(LBANK+KOFUFA+MMTPFA)
 
      IF(IMEDIA.EQ.1) THEN
         IROBOT = 0
         RETURN
      ENDIF
 
      CALL UHTOC(IQ(LBANK+KOFUFA+MVIDFA),4,VID,6)
 
      MNTTYP = ' '
 
#if defined(CERNLIB_PREFIX)
       CALL FMXVID(VID,IQ(LBANK+KOFUFA+MVIPFA),XVID,VIP,'C',IC)
       LXVID = LENOCC(XVID)
       CALL FMQTMS(XVID(1:LXVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
#if !defined(CERNLIB_PREFIX)
      CALL FMQTMS(VID,LIB,MODEL,DENS,MNTTYP,LABTYP,IRC)
#endif
      IQUEST(1) = IRC
 
      IF(IRC.EQ.312) THEN
         IRC = 0
         IF(IDEBFA.GE.3) PRINT *,'FMQMED. volume ',
#if !defined(CERNLIB_PREFIX)
     +       VID,
#endif
#if defined(CERNLIB_PREFIX)
     +      XVID,
#endif
     +      ' unavailable'
      ENDIF
 
      IF(MNTTYP.EQ.'R') THEN
         IROBOT = 1
      ELSE
         IROBOT = 0
      ENDIF
 
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMQMED. return code ',
     +      IRC,' from FMQTMS'
          RETURN
      ENDIF
 
      END
