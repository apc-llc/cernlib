*
* $Id: fmgtms.F,v 1.1.1.1 1996/03/07 15:18:14 mclareni Exp $
*
* $Log: fmgtms.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:14  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMGTMS(XVID,LBANK,VSN,IDENS,IMEDIA,IRC)
*     SUBROUTINE FMGTMS(XVID,LBANK,VSN*,IDENS*,IMEDIA*,IRC*)
*
*     Fill bank at LBANK with VID, (XVID?), VSN, DENSITY and MEDIATYPE
*
*     Return codes: 0   ok
*                   12  Access denied
*                   100 Volume does not exist
*
#include "fatmen/fattyp.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
      CHARACTER*15  XVID
      CHARACTER*8   PREFIX
      CHARACTER*6   VSN,VID
#include "fatmen/tmsrep.inc"
      CHARACTER*132 LINE
      CHARACTER*8   MODEL,DENS
#include "fatmen/fatvidp.inc"
      IMEDIA = 2
      IDENS  = 38000
      JP = INDEX(XVID,'.')
      IF(JP.NE.0) THEN
         PREFIX = XVID(1:JP-1)
         VID    = XVID(JP+1:)
         JP     = ICNTH(PREFIX(1:LENOCC(PREFIX)),PREVID,NTMS)
      ELSE
         VID    = XVID
      ENDIF
 
      IQUEST(1) = 0
 
#if !defined(CERNLIB_TMS)
      IQUEST(1) = -1
      GOTO 99
#endif
#if defined(CERNLIB_TMS)
      IRC    = 0
   10 CONTINUE
*
      I = LENREP
*
      CALL FMSREQ('TMS     ',
     +            'Q VID '//VID//' (VSN MODEL DENSITY',
     +            IRC,TMSREP,I)
 
#endif
#if (defined(CERNLIB_TMS))&&(defined(CERNLIB_TMSTEST))
      IF(IRC.EQ.100) GOTO 99
#endif
#if defined(CERNLIB_TMS)
      LINE   = TMSREP(1)
      IF(IDEBFA.GE.3) PRINT *,'FMGTMS. reply from TMS = ',LINE
      CALL CSQMBL(LINE  ,1,LENOCC(LINE))
      CALL FMWORD(VSN   ,1,' ',LINE,IRC)
      CALL FMWORD(MODEL ,2,' ',LINE,IRC)
      CALL FMWORD(DENS  ,3,' ',LINE,IRC)
      IMEDIA = ICNTH(MODEL(1:LENOCC(MODEL)),CHMTYP,NMTYP)
      IDENS  = ICDECI(DENS,1,LEN(DENS))
      IF(INDEX(DENS,'K').NE.0) IDENS = IDENS * 1000
*
#endif
   99 CONTINUE
*
*     Get here if -TMS or IF=TMSTEST & RC=100
*
      IF(LBANK.NE.0) THEN
         IQ(LBANK+KOFUFA+MMTPFA) = IMEDIA
         IQ(LBANK+KOFUFA+MDENFA) = IDENS
         IQ(LBANK+KOFUFA+MVIPFA) = JP
         CALL UCTOH(VID,IQ(LBANK+KOFUFA+MVSNFA),4,6)
      ENDIF
 
      END
