*
* $Id: fmprot.F,v 1.1.1.1 1996/03/07 15:18:15 mclareni Exp $
*
* $Log: fmprot.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:15  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMPROT(GENAM,LBANK,KEYS,CHPROT,CHOPT,IRC)
*
*     Routine to set the protection group on a volume
*
      CHARACTER*(*) GENAM,CHOPT
      PARAMETER     (LKEYFA=10)
      DIMENSION     KEYS(LKEYFA)
      CHARACTER*80  COMAND
      CHARACTER*15  VID
#include "fatmen/tmsrep.inc"
      CHARACTER*15  XVID
      CHARACTER*8   VIP,PROT,CHACCT
      CHARACTER*(*) CHPROT
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatvid0.inc"
#include "fatmen/fatoptd.inc"
#include "fatmen/fatvid1.inc"
#include "fatmen/fatoptc.inc"
*
*     Options:
*
      IRC = 0
      LGN = LENOCC(GENAM)
#if !defined(CERNLIB_TMS)
      IF(IDEBFA.GE.0) PRINT *,
     +                'FMPROT. TMS option not installed on this node'
#endif
#if defined(CERNLIB_TMS)
      IF(LBANK.EQ.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMPROT. get bank for ',GENAM(1:LGN)
         CALL FMGETK(GENAM(1:LGN),LBANK,KEYS,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0)
     +      PRINT *,'FMPROT. Return code ',IRC,' from FMGETK'
            IRC = 1
            RETURN
         ENDIF
      ELSE
         IF(IDEBFA.GE.0)
     +      PRINT *,'FMPROT. enter for user supplied bank for ',
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
      LPROT = LENOCC(CHPROT)
      PROT  = CHPROT(1:LPROT)
 
   10 CONTINUE
*
      IF(IDEBFA.GE.0) PRINT *,'FMPROT. apply protection ',
     +   PROT(1:LPROT),' to VID ',VID(1:LVID)
*
*     Get the current account...
*
         I = LENREP
         CALL FMSREQ('TMS     ',
     +               'QUERY VID '//VID(1:LVID)//' (ACCOUNT',
     +               IRC,TMSREP,I)
 
         ISTART = INDEX(TMSREP(1),' ')+1
         IEND   = LENOCC(TMSREP(1))
         CHACCT = TMSREP(1)(ISTART:IEND)
         LACCT  = LENOCC(CHACCT)
*
*     Now set the protection group...
*
         I = LENREP
         CALL FMSREQ('TMS     ',
     +               'PROTECT '//PROT(1:LPROT)//' '//
     +               'ACCOUNT '//CHACCT(1:LACCT)//' '//
     +               'APPLY SET '//VID(1:LVID),
     +               IRC,TMSREP,I)
 
#endif
      END
