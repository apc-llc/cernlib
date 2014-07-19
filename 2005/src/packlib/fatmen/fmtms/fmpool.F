*
* $Id: fmpool.F,v 1.1.1.1 1996/03/07 15:18:14 mclareni Exp $
*
* $Log: fmpool.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:14  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMPOOL(GENAM,LBANK,KEYS,CHPOOL,CHOPT,IRC)
*
*     Routine to move a tape from one pool to another
*
#include "fatmen/faust.inc"
      CHARACTER*(*) GENAM,CHOPT
      PARAMETER     (LKEYFA=10)
      DIMENSION     KEYS(LKEYFA)
      CHARACTER*80  COMAND
      CHARACTER*15  VID
#include "fatmen/tmsrep.inc"
      CHARACTER*15  XVID
      CHARACTER*8   VIP,POOL,USER
      CHARACTER*(*) CHPOOL
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatvid0.inc"
#include "fatmen/fatoptd.inc"
#include "fatmen/fatvid1.inc"
#include "fatmen/fatoptc.inc"
*
*     Options: L - write lock
*              U - unlock (write enable)
*              P - permissive - transfer even if not owner
*              D - delete the TMS tag
*              S - set the TMS tag to the generic name
*              B - binary tag
*              T - text  tag
*
      NFPOOL = NFPOOL + 1
      IRC = 0
      LGN = LENOCC(GENAM)
#if !defined(CERNLIB_TMS)
      IF(IDEBFA.GE.0) PRINT *,
     +                'FMPOOL. TMS option not installed on this node'
#endif
#if defined(CERNLIB_TMS)
      IF((IOPTT.EQ.0).AND.(IOPTB.EQ.0)) IOPTT = 1
      IF(LBANK.EQ.0) THEN
         IF(IDEBFA.GE.1) PRINT *,'FMPOOL. get bank for ',GENAM(1:LGN)
         CALL FMGETK(GENAM(1:LGN),LBANK,KEYS,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0)
     +      PRINT *,'FMPOOL. Return code ',IRC,' from FMGETK'
            IRC = 1
            RETURN
         ENDIF
      ELSE
         IF(IDEBFA.GE.1)
     +      PRINT *,'FMPOOL. enter for user supplied bank for ',
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
         IF(IDEBFA.GE.-3) PRINT *,'FMPOOL. something is wrong  - '//
     +      'VID has zero length'
         IRC = -1
         RETURN
      ENDIF
*
*     Set or delete TMS tags?
*
      IF(IOPTS.NE.0) THEN
         IF(IOPTB.NE.0)
     +   CALL FMTAGS(GENAM(1:LGN),LBANL,KEYS,GENAM(1:LGN),'SB',IRC)
         IF(IOPTT.NE.0)
     +   CALL FMTAGS(GENAM(1:LGN),LBANL,KEYS,GENAM(1:LGN),'ST',IRC)
      ENDIF
 
      IF(IOPTD.NE.0) THEN
         IF(IOPTB.NE.0)
     +      CALL FMTAGS(GENAM(1:LGN),LBANL,KEYS,' ','DB',IRC)
         IF(IOPTT.NE.0)
     +      CALL FMTAGS(GENAM(1:LGN),LBANL,KEYS,' ','DT',IRC)
      ENDIF
 
      LPOOL = LENOCC(CHPOOL)
      POOL  = CHPOOL(1:LPOOL)
 
   10 CONTINUE
*
      IF(IDEBFA.GE.0) PRINT *,'FMPOOL. Move ',
     +   VID(1:LVID),' to pool ',POOL(1:LPOOL)
 
      IF(IOPTP.NE.0) THEN
*
*     Get the current owner...
*
         I = LENREP
         CALL FMSREQ('TMS     ',
     +               'QUERY VID '//VID(1:LVID)//' (OWNER',
     +               IRC,TMSREP,I)
 
         ISTART = INDEX(TMSREP(1),' ')+1
         IEND   = LENOCC(TMSREP(1))
         USER   = TMSREP(1)(ISTART:IEND)
         LUSER  = LENOCC(USER)
 
         I = LENREP
         CALL FMSREQ('TMS     ',
     +               'TRANSFER VID '//VID(1:LVID)//
     +               ' FROM    '//USER(1:LUSER)//
     +               ' TO USER '//POOL(1:LPOOL),
     +               IRC,TMSREP,I)
 
      ELSE
 
         I = LENREP
         CALL FMSREQ('TMS     ',
     +               'TRANSFER VID '//VID(1:LVID)//
     +               ' TO USER '//POOL(1:LPOOL),
     +               IRC,TMSREP,I)
      ENDIF
 
      IF(IRC.NE.0) RETURN
      IF(IOPTL.NE.0) THEN
         IF(IDEBFA.GE.3) PRINT *,'Calling FMLOCK to write lock ',VID
         CALL FMLOCK(GENAM(1:LGN),LBANK,KEYS,'L',IRC)
      ENDIF
      IF(IOPTU.NE.0) THEN
         IF(IDEBFA.GE.3) PRINT *,'Calling FMULOK to write enable ',VID
         CALL FMULOK(GENAM(1:LGN),LBANK,KEYS,'U',IRC)
      ENDIF
#endif
      END
