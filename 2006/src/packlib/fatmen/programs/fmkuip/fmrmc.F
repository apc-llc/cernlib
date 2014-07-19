*
* $Id: fmrmc.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fmrmc.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMRMC
#include "fatmen/faust.inc"
#include "fatmen/fatout.inc"
#include "fatmen/fmpath.inc"
#include "fatmen/fmnkeys.inc"
#include "fatmen/fmaxcop.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatsea.inc"
      DIMENSION     KEYS(LKEYFA)
      DIMENSION     KEYSIN(LKEYFA)
      DIMENSION     KEYSOU(LKEYFA,MAXCOP)
      CHARACTER*20  CHOPT,CHPOOL,CHPROT,FNAME
      CHARACTER*255 PATH2
      CHARACTER*255 DSN,CHDSN
      CHARACTER*12  HOST,VID,USER
      CHARACTER*4   COMM
      CHARACTER*8   ACCT,ACNT,CHANS
      CHARACTER*3   CHYES
      INTEGER       FMACNT
 
*
*     Options: A - remove all occurances of this generic name
*              E - erase disk file
*              I - prompt before removing each matching entry
*              N - noprompt for option A
*              F - free tape associated with specified entry
*              P - when used with option F, allows privileged TMS
*                  user to free anyones tapes (within a group)
*              U - 'unlock' or write-enable tape
*              D - delete TMS tag
*              B - binary TMS tag
*              T - text TMS tag
*
      IC     = FMACNT(ACCT)
 
      COMM = 'DEL '
*
*     Save current directory
*
      CALL RZCDIR(CDIR,'R')
      LCDIR = LENOCC(CDIR)
 
      CHYES = 'YES'
 
      OUTPUT = ' '
 
      KSN   = 0
      IOPTA = 0
      IOPTB = 0
      IOPTD = 0
      IOPTE = 0
      IOPTF = 0
      IOPTG = 0
      IOPTI = 0
      IOPTN = 0
      IOPTP = 0
      IOPTU = 0
      CALL KUGETC(PATH,LPATH)
      CALL KUGETI(KSN)
      CALL KUGETC(DSN,LDSN)
      CALL KUGETC(HOST,LHOST)
      CALL KUGETC(VID,LVID)
      CALL KUGETC(USER,LUSER)
      CALL KUGETC(CHPOOL,LPOOL)
      CALL KUGETC(CHPROT,LPROT)
      CALL KUGETC(CHOPT,LCHOPT)
 
      IF(LPATH.EQ.0) RETURN
      CALL FMFIXF(PATH,PATH2)
      LPATH = LENOCC(PATH2)
      PATH  = PATH2
 
      IF(IDEBFA.GE.3) PRINT *,'FMRMC. PATH = ',PATH(1:LPATH)
 
      IF(LDSN.EQ.0)  THEN
         DSN = ' '
      ELSE
         IF(IDEBFA.GE.3) PRINT *,'FMRMC. DSN = ',DSN(1:LDSN)
      ENDIF
 
      IF(LHOST.EQ.0) THEN
         HOST = ' '
      ELSE
         IF(IDEBFA.GE.3) PRINT *,'FMRMC. HOST = ',HOST(1:LHOST)
      ENDIF
 
      IF(LVID.EQ.0)  THEN
         VID = ' '
      ELSE
         IF(IDEBFA.GE.3) PRINT *,'FMRMC. VID = ',VID(1:LVID)
      ENDIF
 
      IF(LUSER.EQ.0) THEN
         USER = ' '
      ELSE
         IF(IDEBFA.GE.3) PRINT *,'FMRMC. USER = ',USER(1:LUSER)
      ENDIF
 
      IF(LCHOPT.EQ.0) THEN
         LCHOPT = 1
         CHOPT  = ' '
      ELSE
         IF(IDEBFA.GE.3) PRINT *,'FMRMC. CHOPT = ',CHOPT(1:LCHOPT)
      ENDIF
 
      IF(INDEX(CHOPT(1:LCHOPT),'A').NE.0) IOPTA = 1
      IF(INDEX(CHOPT(1:LCHOPT),'B').NE.0) IOPTB = 1
      IF(INDEX(CHOPT(1:LCHOPT),'D').NE.0) IOPTD = 1
      IF(INDEX(CHOPT(1:LCHOPT),'E').NE.0) IOPTE = 1
      IF(INDEX(CHOPT(1:LCHOPT),'G').NE.0) IOPTG = 1
      IF(INDEX(CHOPT(1:LCHOPT),'F').NE.0) IOPTF = 1
      IF(INDEX(CHOPT(1:LCHOPT),'I').NE.0) IOPTI = 1
      IF(INDEX(CHOPT(1:LCHOPT),'N').NE.0) IOPTN = 1
      IF(INDEX(CHOPT(1:LCHOPT),'P').NE.0) IOPTP = 1
      IF(INDEX(CHOPT(1:LCHOPT),'U').NE.0) IOPTU = 1
 
      IF(IOPTA.NE.0) THEN
         IF(IOPTN.NE.0) THEN
            IF(IDEBFA.GE.0) THEN
               PRINT *,'FMRMC. warning. all matching entries will '
     +         //'be deleted without prompting.'
               CALL KUPROC('FMRMC. Do you want to continue??? ',
     +         CHANS,LENANS)
               IF(LENANS.EQ.0) THEN
                  LENANS = 2
                  CHANS = 'NO'
               ENDIF
               IF(CHANS(1:LENANS).NE.CHYES(1:LENANS)) RETURN
               IOPTI = 0
            ENDIF
         ELSE
            IOPTI = 1
         ENDIF
      ENDIF
 
      IDCREA = -1
      IDCATA = -1
      IDLAST = -1
 
      DO 20 I=1,2
         DO 10 J=1,10
            IUSER(I,J) = -1
   10    CONTINUE
   20 CONTINUE
 
      IF((IOPTF.NE.0).AND.(LPOOL.EQ.0)) THEN
         CHPOOL = ACCT(5:6)//'_FAT1'
         LPOOL  = 7
         IF(IDEBFA.GE.0) PRINT *,'FMRMC. tape will be returned to ',
     +      ' pool ',CHPOOL(1:LPOOL)
      ENDIF
 
      IF((IOPTG.NE.0).AND.(LPROT.EQ.0)) THEN
 
         IF(IOPTP.EQ.0) THEN
            CHPROT = '*None'
            LPROT  = 5
         ELSE
            CHPROT = CHPOOL
            LPROT  = 7
         ENDIF
 
         IF(IDEBFA.GE.0) PRINT *,'FMRMC. protection group will ',
     +      'be set to ',CHPROT(1:LPROT)
      ENDIF
 
      IF(KSN.EQ.0) THEN
         CALL VZERO(KEYS,10)
      ELSE
         KEYS(1) = KSN
      ENDIF
 
      LFILE = INDEXB(PATH(1:LPATH),'/') -1
      FNAME = PATH(LFILE+2:LPATH)
      CALL RZCDIR(PATH(1:LFILE),'U')
      LNAME = LENOCC(FNAME)
*
*     Check how many copies of this dataset exist
*
      CALL UCOPY(KEYS,KEYSIN,10)
*
*     Don't compare media type, copy level or location code
*
      KEYSIN(MKMTFA) = -1
      KEYSIN(MKCLFA) = -1
      KEYSIN(MKLCFA) = -1
      CALL FMSELK(PATH(1:LPATH),KEYSIN,KEYSOU,NMATCH,MAXCOP,IRC)
      IF(NMATCH.EQ.0) THEN
         IF(IDEBFA.GE.0)
     +   PRINT *,'FMRMC. found 0 matches for ',PATH(1:LPATH)
         IRC = 1
         GOTO 50
      ELSEIF((NMATCH.GT.1).AND.(KEYS(1).EQ.0)
     +                    .AND.(IOPTA.EQ.0)) THEN
         IF(IDEBFA.GE.0) THEN
            PRINT *,'FMRMC. found ',NMATCH,' matches for ',PATH(1:LPATH)
            PRINT *,'FMRMC. Please specify which copy is to be deleted'
         ENDIF
         IRC = 1
         GOTO 50
      ELSE
 
         IF(IDEBFA.GE.1)
     +   PRINT *,'FMRMC. found ',NMATCH,' matches for ',PATH(1:LPATH)
 
         CALL UCOPY(KEYS,KEYSIN,10)
 
         IFOUND = 0
         DO 40 I=1,NMATCH
*
*     Was a specific key serial number specified?
*
            IF((KEYSIN(1).NE.KEYSOU(1,I)).AND.(KEYSIN(1).NE.0)) GOTO
     +      40
 
            IFOUND = 1
            CALL UCOPY(KEYSOU(1,I),KEYS,10)
 
            IF(IDEBFA.GE.1) THEN
               PRINT *,'FMRMC. candidate number ',I
               CALL FMPKEY(KEYSOU(1,I),LKEYFA)
            ENDIF
 
            LTDSFA = 0
            CALL RZCDIR(PATH(1:LFILE),'U')
            CALL FMGETK(PATH(1:LPATH),LTDSFA,KEYSOU(1,I),IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMRMC. Return code ',IRC,' '
     +         //'from FMGETK'
               GOTO 50
            ENDIF
 
*
*     Check account against that of creator, permitting account aliases
*
            CALL UHTOC(IQ(LTDSFA+KOFUFA+MCIDFA),4,ACNT,8)
            CALL FMACCT(ACCT,ACNT,IRC)
*     IF (ACNT .NE. ACCT) THEN
            IF(IRC.NE.0) THEN
 
               WRITE(LPRTFA,*) 'FMRMC. key serial number = ',KEYSOU(1,
     +         I)
               WRITE(LPRTFA,*) 'You cannot delete this file'
               WRITE(LPRTFA,*) 'File is owned by ',ACNT, ' current '
     +         //'account = ',ACCT
               IRC = 1
               GOTO 30
            ENDIF
*
*     Now check if this entry matches
*     (no check on user file format or comment)
*
            CALL FMSEAR(PATH(1:LPATH),LTDSFA,KEYS,DSN,HOST,VID, USER,' '
     +      ,' ',IRET)
            IF(IRET.NE.0) THEN
               IF(IDEBFA.GE.-3) PRINT *,'FMRMC. return code ',IRET,
     +         ' from FMSEAR'
               GOTO 30
            ENDIF
 
            IF(IOPTI.NE.0) THEN
 
               CALL FMSHOW(PATH(1:LPATH),LTDSFA,KEYS,'A',IC)
               CALL KUPROC('FMRMC. Delete??? ',CHANS,LENANS)
               IF(LENANS.EQ.0) THEN
                  LENANS = 2
                  CHANS = 'NO'
               ENDIF
 
               IF(CHANS(1:LENANS).NE.CHYES(1:LENANS)) GOTO 30
 
            ENDIF
 
            CALL FMVERI(PATH(1:LPATH),LTDSFA,KEYS,'A',IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.-3) PRINT *,'FMRMC. error(s) detected ',
     +         'by FMVERI. Cannot perform deletion.'
               GOTO 30
            ENDIF
 
            NFRMFL = NFRMFL + 1
            CALL FMFZO(COMM,PATH,LTDSFA,KEYS,IRC)
            IF(IRC.NE.0) GOTO 30
*
*     Erase disk file?
*
            IF(IOPTE.NE.0.AND.KEYS(MKMTFA).EQ.1) THEN
               CALL UHTOC(IQ(LTDSFA+KOFUFA+MFQNFA),4,CHDSN,NFQNFA)
               CALL FAERAS(CHDSN,IRC)
            ELSEIF(KEYS(MKMTFA).GT.1) THEN
*
*     TMS tags?
*
               IF(IOPTD.NE.0) THEN
                  IF(IOPTB.EQ.0.AND.IOPTT.EQ.0) IOPTT = 1
                  IF(IOPTB.NE.0) THEN
                     CALL FMTAGS(PATH(1:LPATH),LTDSFA,KEYS,' ','DB',
     +               IRC)
                  ENDIF
                  IF(IOPTT.NE.0) THEN
                     CALL FMTAGS(PATH(1:LPATH),LTDSFA,KEYS,' ','DT',
     +               IRC)
                  ENDIF
               ENDIF
 
               IF(IOPTU.NE.0) THEN
                  CALL FMULOK(PATH(1:LPATH),LTDSFA,KEYS,' ',IRC)
                  IF(IRC.NE.0) PRINT *,'FMRMC. return code ',IRC,
     +            ' from FMULOK'
               ENDIF
 
               IF(IOPTG.NE.0) THEN
                  CALL FMPROT(PATH(1:LPATH),LTDSFA,KEYS, CHPROT(1:
     +            LPROT),' ',IRC)
                  IF(IRC.NE.0) PRINT *,'FMRMC. return code ',IRC,
     +            ' from FMPROT'
               ENDIF
 
               IF((IOPTF.NE.0).AND.(LPOOL.NE.0)) THEN
                  IF(IOPTP.EQ.0) THEN
                     CALL FMPOOL(PATH(1:LPATH),LTDSFA,KEYS,CHPOOL(1:
     +               LPOOL), ' ',IRC)
                  ELSE
                     CALL FMPOOL(PATH(1:LPATH),LTDSFA,KEYS,CHPOOL(1:
     +               LPOOL), 'P',IRC)
                  ENDIF
                  IF(IRC.NE.0) PRINT *,'FMRMC. return code ',IRC,
     +            ' from FMPOOL'
               ENDIF
            ENDIF
 
   30       CONTINUE
            CALL MZDROP(IDIVFA,LTDSFA,' ')
            LTDSFA = 0
   40    CONTINUE
      ENDIF
      IF(KSN.NE.0.AND.IFOUND.EQ.0.AND.IDEBFA.GE.-2) PRINT *,'FMRMC. ',
     +   ' no match found for ',PATH(1:LPATH),' key = ',KSN
 
   50 CONTINUE
*
*     Reset current directory
*
      CALL RZCDIR(CDIR(1:LCDIR),' ')
      END
 
