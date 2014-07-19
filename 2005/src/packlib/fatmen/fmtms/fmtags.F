*
* $Id: fmtags.F,v 1.1.1.1 1996/03/07 15:18:15 mclareni Exp $
*
* $Log: fmtags.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:15  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMTAGS(GENAM,LBANK,KEYS,TAG,CHOPT,IRC)
*
*     Routine to get, set or delete TMS Tags
*
#include "fatmen/faust.inc"
      CHARACTER*(*) GENAM,TAG,CHOPT
      CHARACTER*255 CHTAG
      PARAMETER     (LKEYFA=10)
      DIMENSION     KEYS(LKEYFA)
      CHARACTER*6   COMAND
      CHARACTER*7   CHTYPE
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
*
*     Options: D - delete tag
*              G - get tag
*              S - set tag
*              B - binary tag
*              T - text tag   (D)
*              V - volinfo tag
*
      IF(IOPTD.NE.0) NFDTAG = NFDTAG + 1
      IF(IOPTG.NE.0) NFGTAG = NFGTAG + 1
      IF(IOPTS.NE.0) NFSTAG = NFSTAG + 1
 
      IRC = 0
      LGN = LENOCC(GENAM)
      LTG = LENOCC(TAG)
      CHTAG = TAG(1:LTG)
 
      LCH = LENOCC(CHOPT)
 
      IF(IDEBFA.GE.3) PRINT *,'FMTAGS. enter for ',
     +   GENAM(1:LGN),' options ',CHOPT(1:LCH),' tag ',
     +   CHTAG(1:LTG)
 
      IF(IOPTB.EQ.0.AND.IOPTT.EQ.0.AND.IOPTV.EQ.0) THEN
         IF(IDEBFA.GE.1) PRINT *,'FMTAGS. tag typed defaulted to TEXT'
         IOPTT = 1
      ENDIF
 
      IF(IOPTD.NE.0) THEN
         COMAND = 'DELETE'
      ELSEIF(IOPTG.NE.0) THEN
         COMAND = 'GET   '
      ELSEIF(IOPTS.NE.0) THEN
         COMAND = 'SET   '
         IF(LTG.EQ.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMTAGS. tag text has zero length'
            IRC = 8
            RETURN
         ENDIF
      ENDIF
 
      IF((IOPTD.EQ.0).AND.(IOPTG.EQ.0).AND.(IOPTS.EQ.0)) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMTAGS. please specify one of ',
     +      'D, G or S'
         IRC = 8
         RETURN
      ENDIF
 
      IF(IOPTB.NE.0) THEN
         CHTYPE = 'BINARY '
      ELSEIF(IOPTT.NE.0) THEN
         CHTYPE = 'TEXT   '
      ELSEIF(IOPTV.NE.0) THEN
#if defined(CERNLIB_IN2P3)
         CHTYPE = 'SYSTEM'
#endif
#if !defined(CERNLIB_IN2P3)
         CHTYPE = 'VOLINFO'
#endif
      ENDIF
 
#if !defined(CERNLIB_TMS)
      IF(IDEBFA.GE.0) PRINT *,
     +                'FMTAGS. TMS option not installed on this node'
      CHTAG = ' '
      IRC = 99
#endif
#if defined(CERNLIB_TMS)
      IF(LBANK.EQ.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMTAGS. get bank for ',GENAM(1:LGN)
         CALL FMGETK(GENAM(1:LGN),LBANK,KEYS,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0)
     +      PRINT *,'FMTAGS. Return code ',IRC,' from FMGETK'
            IRC = 1
            RETURN
         ENDIF
      ELSE
         IF(IDEBFA.GE.1)
     +      PRINT *,'FMTAGS. enter for user supplied bank for ',
     +         GENAM(1:LGN)
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
         LVIP = LENOCC(PREVID(JP))
         VIP = PREVID(JP)(1:LVIP )
         XVID = PREVID(JP)(1:LVIP) // '.' // VID(1:LVID)
         LXVID = LENOCC(XVID )
 
      ELSE
         XVID = VID
         LXVID = LVID
         LVIP = 0
      ENDIF
 
      VID = XVID
      LVID = LXVID
#endif
#if defined(CERNLIB_TMS)
 
      IF(LVID.EQ.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMTAGS. something is wrong  - '//
     +      'VID has zero length'
         IRC = -1
         RETURN
      ENDIF
 
      I = LENREP
      DO 1 JJ=1,LENREP
    1 TMSREP(JJ) = ' '
 
      IF(IDEBFA.GE.3) PRINT *,'FMTAGS. issuing ', 'TMS TAG VID '//
     +VID(1:LVID)//' '//COMAND//' '//CHTYPE
 
      IF(IOPTS.NE.0) THEN
 
         IF(IDEBFA.GE.3) PRINT *,'            for ',CHTAG(1:LTG)
 
         CALL FMSREQ('TMS     ', 'TAG VID '//VID(1:LVID) //' '//COMAND/
     +   /' '//CHTYPE //' '//CHTAG(1:LTG) ,IRC,TMSREP,I)
 
      ELSE
         CALL FMSREQ('TMS     ', 'TAG VID '//VID(1:LVID)//' ' //COMAND/
     +   /' '//CHTYPE ,IRC,TMSREP,I)
 
         IF(IOPTG.NE.0) THEN
            TAG = ' '
            LTAG = LENOCC(TMSREP(1))
            IF(IRC.EQ.0.AND.LTAG.GT.0) TAG = TMSREP(1)(1:LTAG)
         ENDIF
 
      ENDIF
 
#endif
      END
 
