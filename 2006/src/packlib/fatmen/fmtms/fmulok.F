*
* $Id: fmulok.F,v 1.1.1.1 1996/03/07 15:18:14 mclareni Exp $
*
* $Log: fmulok.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:14  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMULOK(GENAM,LBANK,KEYS,CHOPT,IRC)
*
*     routine to unlock a VID in the TMS according to CHOPT
*     Current function is to enable Write (independant of CHOPT)
*
#include "fatmen/faust.inc"
      CHARACTER*(*) GENAM,CHOPT
      PARAMETER     (LKEYFA=10)
      DIMENSION     KEYS(LKEYFA)
      CHARACTER*80  COMAND
      CHARACTER*15  VID
#include "fatmen/tmsrep.inc"
      CHARACTER*15  XVID
      CHARACTER*8   VIP
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatvid0.inc"
#include "fatmen/fatoptd.inc"
#include "fatmen/fatvid1.inc"
#include "fatmen/fatoptc.inc"
 
      NFULOK = NFULOK + 1
      IRC = 0
      LGN = LENOCC(GENAM)
#if !defined(CERNLIB_TMS)
      IF(IDEBFA.GE.0) PRINT *,
     +                'FMULOK. TMS option not installed on this node'
#endif
#if defined(CERNLIB_TMS)
      IF(LBANK.EQ.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMULOK. get bank for ',GENAM(1:LGN)
         CALL FMGETK(GENAM(1:LGN),LBANK,KEYS,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0)
     +      PRINT *,'FMULOK. Return code ',IRC,' from FMGETK'
            IRC = 1
            RETURN
            ENDIF
         ELSE
         IF(IDEBFA.GE.0)
     +      PRINT *,'FMULOK. enter for user supplied bank for ',
     +         GENAM(1:LGN)
         ENDIF
 
      IF(KEYS(MKMTFA).LT.2) THEN
         IF(IDEBFA.GE.-1) PRINT *,'FMULOK. request ignored for ',
     +      'media type ',KEYS(MKMTFA)
         RETURN
      ENDIF
 
      VID  = ' '
      CALL FMGETC(LBANK,VID,MVIDFA,6,IRC)
      LVID = LENOCC(VID)
#endif
#if (defined(CERNLIB_PREFIX))&&(defined(CERNLIB_TMS))
*
*        Generate eXtended VID - with VID prefix
*
         JP = IQ(LBANK+KOFUFA+MVIPFA)
            IF(JP.NE.0) THEN
            LVIP   = LENOCC(PREVID(JP))
            VIP    = PREVID(JP)(1:LVIP)
            XVID   = PREVID(JP)(1:LVIP) // '.' // VID(1:LVID)
            LXVID  = LENOCC(XVID)
 
            ELSE
            XVID   = VID
            LXVID  = LVID
            LVIP   = 0
            ENDIF
 
         VID   = XVID
         LVID  = LXVID
#endif
#if defined(CERNLIB_TMS)
 
10    CONTINUE
      I = LENREP
*
      IF(IDEBFA.GE.0) PRINT *,'FMULOK. enable write access to ',
     +   VID(1:LVID)
      CALL FMSREQ('TMS     ',
     +            'LOCK ENABLE WRITE VID '//VID(1:LVID),
     +            IRC,TMSREP,I)
 
#endif
      END
 
