*
* $Id: fmgvid.F,v 1.1.1.1 1996/03/07 15:18:15 mclareni Exp $
*
* $Log: fmgvid.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:15  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMGVID(IFREE,IMEDIA,CHLIB,CHPOOL,CHFREE,CHVSN,CHVID,
     +                  IFILE,CHOPT,IRC)
#include "fatmen/faust.inc"
      CHARACTER*(*) CHLIB,CHFREE,CHPOOL,CHVSN,CHVID
      CHARACTER*80  COMAND
#include "fatmen/tmsrep.inc"
      PARAMETER     (LENTAP=100)
      CHARACTER*132 TAPREP(LENTAP)
      CHARACTER*255 CHTAG(2)
      CHARACTER*6   VID
#include "fatmen/fattyp.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatopts.inc"
 
      NFASPC = NFASPC + 1
 
      IRC = 0
 
      LLIB  = LENOCC(CHLIB)
      LPOOL = LENOCC(CHPOOL)
      LFREE = LENOCC(CHFREE)
*
*     First look through pool CHPOOL for a volume with the desired
*     characteristics
*
      COMAND = ' '
      WRITE(COMAND,9001) CHLIB(1:LLIB),CHPOOL(1:LPOOL)
 9001 FORMAT('QUERY CONTENTS LIBRARY ',A,' USER ',A,' SLOT 0')
      LC = LENOCC(COMAND)
      ITMSHD=1
      ITMSFU=0
*
*     Retry loop
*
   10 CONTINUE
      ICONT = 0
      IF(IDEBFA.GT.0)
     +PRINT *,'FMGVID. Issuing TMS command ',COMAND(1:LC)
      I = LENTAP
      CALL FMSREQ('TMS     ',COMAND(1:LC),
     +            IRC,TAPREP,I)
 
      IF((IDEBFA.GE.0).AND.(IRC.GT.2)) THEN
         PRINT *,'FMGVID. Return code ',IRC,' from FMSREQ'
         PRINT *,TAPREP(1)
         RETURN
      ENDIF
 
      IF(ITMSHD.EQ.1) THEN
         IF((TAPREP(1)(1:7).NE.'Library').OR.
     +      (TAPREP(2)(1:7).NE.'-------')) THEN
            PRINT *,'FMGVID. Cannot interpret reply from FMSREQ'
            PRINT *,TAPREP(1)
            PRINT *,TAPREP(2)
            IRC = 1
            RETURN
         ENDIF
         ITMSHD=0
         ILINES=3
      ELSE
         ILINES=1
      ENDIF
*
*     continuation command
*
      IF(IRC.EQ.2) THEN
         IF(I.GT.0) THEN
            COMAND = TAPREP(I)
            LC     = LENOCC(TAPREP(I))
            ICONT  = 1
            NLINES = I-1
         ELSE
            ICONT  = 0
            NLINES = LENTAP
            ITMSFU = LENTAP
         ENDIF
      ELSE
         NLINES = 1
      ENDIF
 
      DO 30 J=ILINES,NLINES
         LLINE = LENOCC(TAPREP(J))
         IF(IDEBFA.GE.1) PRINT *,'FMGVID. processing ',
     +      TAPREP(J)(1:LLINE)
*
*     Get VID
*
         CHVID = ' '
         CHVID = TAPREP(J)(24:29)
         LVID  = LENOCC(CHVID)
         VID   = CHVID(1:LVID)
*
*     Decode VOLINFO tag for this volume
*
         I     = 2
         CALL FMSREQ('TMS     ', 'TAG VID '//VID(1:LVID)
     +               //' GET VOLINFO ', IRC,CHTAG,I)
         CALL    FM0TOB(CHTAG(1))
         LTAG  = LENOCC(CHTAG(1))
         IF(LTAG.EQ.0) GOTO 20
*
*     Look for MB field
*
         IF(CHTAG(1)(1:3).EQ.'MB=') THEN
            IFIRST = 4
         ELSE
            IFIRST = INDEX(CHTAG(1)(1:LTAG),' MB=')
            IF(IFIRST.EQ.0) GOTO 20
            IFIRST = IFIRST + 4
         ENDIF
 
         NUSED = ICDECI(CHTAG(1),IFIRST,LTAG)
 
         IF(IDEBFA.GE.2) WRITE(LPRTFA,9006) CHVID(1:LVID),NUSED
*
*     Calculate remaining space up to high water mark
*
         JFREE = MEDHWM(IMEDIA) - NUSED
*
*     Try next volume if insufficient space
*
         IF(IFREE.GT.JFREE) GOTO 30
 
         IF(IDEBFA.GE.0) WRITE(LPRTFA,9002) CHVID(1:LVID),JFREE
 9002 FORMAT(' FMGVID. volume ',A,' has ',I6,' MB free')
*
*     Get file number
*
         IF(CHTAG(1)(1:7).EQ.'NFILES=') THEN
            IFIRST = 8
         ELSE
            IFIRST = INDEX(CHTAG(1)(1:LTAG),' NFILES=')
            IF(IFIRST.EQ.0) GOTO 20
            IFIRST = IFIRST + 8
         ENDIF
 
         IFILE = ICDECI(CHTAG(1),IFIRST,LTAG) + 1
         IF(IDEBFA.GE.2) WRITE(LPRTFA,9007) CHVID(1:LVID),IFILE
*
*     Issue GETPOOL on this volume
*
         CALL FMGETP(CHVID(1:LVID),CHPOOL(1:LPOOL),CHOPT,IRC)
         IF((IDEBFA.GE.0).AND.(IRC.NE.0)) THEN
            PRINT *,'FMGVID. Return code ',IRC,' from FMGETP'
         ENDIF
*
*     Try next volume if getpool fails
*
         IF(IRC.NE.0) GOTO 30
 
*
*     Update the  VOLINFO tag (size should be updated at end of job)
*
         CALL FMVINF(CHVID(1:LVID),IFREE,IFILE,'I',IRC)
         GOTO 70
 
   20    CONTINUE
         IF(IDEBFA.GE.-3) THEN
            IF(LTAG.EQ.0) THEN
               WRITE(LPRTFA,9003) CHVID(1:LVID)
            ELSE
               WRITE(LPRTFA,9004) CHVID(1:LVID)
               WRITE(LPRTFA,9005) CHTAG(1)(1:LTAG)
            ENDIF
         ENDIF
 
 9003 FORMAT(' FMGVID. volume ',A,' has an empty VOLINFO tag')
 9004 FORMAT(' FMGVID. volume ',A,' has an invalid VOLINFO tag')
 9005 FORMAT(' FMGVID. No MB or NFILES field in VOLINFO tag "',
     +         33A1,:,/(9X,71A1))
 9006 FORMAT(' FMGVID. volume ',A,' has ',I6,' MB used.')
 9007 FORMAT(' FMGVID. volume ',A,' has ',I6,' files.')
 9008 FORMAT(' FMGVID. Increase the number of lines given to SYSREQ',
     +       ' (from',I4,') to allow',/,
     +       '         the FMSREQ continuation mode to be used and',
     +       ' therefore more volumes to be inspected.')
 
   30 CONTINUE
*
*     Have another bash
*
      IF(ICONT.NE.0) GOTO 10
 
*
*     Report implementation detail if more buffers may have been useful.
*
      IF(ITMSFU.GT.0) WRITE(LPRTFA,9008) ITMSFU
 
   40 CONTINUE
*
*     Try FREE pool
*
      COMAND = ' '
      WRITE(COMAND,9001) CHLIB(1:LLIB),CHFREE(1:LFREE)
      LC = LENOCC(COMAND)
      ITMSHD=1
      ITMSFU=0
*
*     Retry loop
*
   50 CONTINUE
      ICONT = 0
      IF(IDEBFA.GT.0)
     +PRINT *,'FMGVID. Issuing TMS command ',COMAND(1:LC)
      I = LENTAP
      CALL FMSREQ('TMS     ',COMAND(1:LC),
     +            IRC,TAPREP,I)
 
      IF((IDEBFA.GE.0).AND.(IRC.GT.2)) THEN
         PRINT *,'FMGVID. Return code ',IRC,' from FMSREQ'
         PRINT *,TAPREP(1)
         RETURN
      ENDIF
 
      IF(ITMSHD.EQ.1) THEN
         IF((TAPREP(1)(1:7).NE.'Library').OR.
     +      (TAPREP(2)(1:7).NE.'-------')) THEN
            PRINT *,'FMGVID. Cannot interpret reply from FMSREQ'
            PRINT *,TAPREP(1)
            PRINT *,TAPREP(2)
            IRC = 1
            RETURN
         ENDIF
         ITMSHD=0
         ILINES=3
      ELSE
         ILINES=1
      ENDIF
*
*     continuation command
*
      IF(IRC.EQ.2) THEN
         IF(I.GT.0) THEN
            COMAND = TAPREP(I)
            LC     = LENOCC(TAPREP(I))
            ICONT  = 1
            NLINES = I-1
         ELSE
            ICONT  = 0
            NLINES = LENTAP
            ITMSFU = LENTAP
         ENDIF
      ELSE
         NLINES = 1
      ENDIF
 
      DO 60 J=ILINES,NLINES
         LLINE = LENOCC(TAPREP(J))
         IF(IDEBFA.GE.1) PRINT *,'FMGVID. processing ',
     +      TAPREP(J)(1:LLINE)
*
*     Get VID
*
         CHVID = ' '
         CHVID = TAPREP(J)(24:29)
         LVID  = LENOCC(CHVID)
*
*     Try GETPOOL on this volume
*
         CALL FMGETP(CHVID(1:LVID),CHFREE(1:LFREE),CHOPT,IRC)
         IF(IRC.EQ.0) THEN
            IFILE = 1
*
*     Set VOLINFO tag (size should be updated at end of job)
*
            CALL FMVINF(CHVID(1:LVID),IFREE,IFILE,'S',IRC)
            GOTO 70
         ENDIF
 
   60 CONTINUE
      IF(ICONT.NE.0) GOTO 50
 
*
*     Report implementation detail if more buffers may have been useful.
*
      IF(ITMSFU.GT.0) WRITE(LPRTFA,9008) ITMSFU
 
*
*     Exhausted all Pool and Free Libraries without technical errors.
*     Set a return code for failure to find a VID.
*
      IRC = 1
      CHVID=' '
      CHVSN=' '
   70 CONTINUE
      IRC = 0
*
*     Get VSN from the VID.
*
      COMAND = 'QUERY VID '//VID//' (VSN '
      LC = LENOCC(COMAND)
      IF(IDEBFA.GT.0)
     +PRINT *,'FMGVID. Issuing TMS command ',COMAND(1:LC)
      I = LENREP
      CALL FMSREQ('TMS     ',COMAND(1:LC),
     +            IRC,TMSREP,I)
      IF((IDEBFA.GE.0).AND.(IRC.NE.0)) THEN
         PRINT *,'FMGVID. Return code ',IRC,' from FMSREQ'
         PRINT *,TMSREP(1)
         RETURN
      ENDIF
      CHVSN   = TMSREP(1)(8:13)
 
      END
