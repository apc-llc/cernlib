*
* $Id: fmxvid.F,v 1.1.1.1 1996/03/07 15:18:15 mclareni Exp $
*
* $Log: fmxvid.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:15  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMXVID(VID,JP,XVID,VIP,CHOPT,IRC)
*CMZ :          25/04/91  08.14.14  by  Jamie Shiers
*-- Author :    Jamie Shiers   25/04/91
*
*     Convert eXtended VID to VID & integer prefix and vice versa
*
*     CHOPT: I - convert XVID to VID and integer prefix
*            C - convert VID and integer prefix to XVID
*                (return character prefix in VIP)
*
      CHARACTER*(*) VID,XVID,VIP,CHOPT
#include "fatmen/fatbug.inc"
#include "fatmen/fatvid0.inc"
#include "fatmen/fatoptd.inc"
#include "fatmen/fatvid1.inc"
#include "fatmen/fatoptc.inc"
      IRC  = 0
 
      IF(IDEBFA.GE.3) PRINT *,'FMXVID. enter for ',
     +   'VID, JP, XVID, VIP, CHOPT = ',
     +    VID, JP, XVID, VIP, CHOPT
 
      IF(IOPTC.NE.0) THEN
         IF((JP.LT.0).OR.(JP.GT.NTMS)) THEN
            IRC  = -1
         ELSEIF(JP.EQ.0) THEN
            XVID = VID
         ELSE
            LVID = LENOCC(VID)
            VIP  = PREVID(JP)
            LVIP = LENOCC(VIP)
            XVID = VIP(1:LVIP) // '.' // VID(1:LVID)
         ENDIF
 
      ELSEIF(IOPTI.NE.0) THEN
         LXVID = LENOCC(XVID)
         IDOT  = INDEX(XVID(1:LXVID),'.')
         IF(IDOT.EQ.0) THEN
            VID = XVID(1:LXVID)
            JP  = 0
            IRC = -1
            RETURN
         ELSE
            VIP  = XVID(1:IDOT-1)
            LVIP = LENOCC(VIP)
            VID  = XVID(IDOT+1:LXVID)
            JP   = ICNTH(VIP(1:LVIP),PREVID,NTMS)
            IF(JP.EQ.0) IRC = -1
         ENDIF
      ENDIF
 
      END
