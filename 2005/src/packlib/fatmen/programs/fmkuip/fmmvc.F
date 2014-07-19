*
* $Id: fmmvc.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fmmvc.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMMVC
      CHARACTER*255 FTEMP
      CHARACTER*255 CHDIR
#include "fatmen/faust.inc"
#include "fatmen/fatupd.inc"
#include "fatmen/fmpath.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fmnkeys.inc"
#include "fatmen/fmaxcop.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatsys.inc"
      DIMENSION     KEYS(LKEYFA)
      DIMENSION     KEYSIN(LKEYFA)
      DIMENSION     KEYSOU(LKEYFA,MAXCOP)
      CHARACTER*20  FNAME
      CHARACTER*15  XVID
      CHARACTER*6   VSN,VID
      CHARACTER*8   HOST,VIP
      CHARACTER*255 DSN
#include "fatmen/fatinit.inc"
*
*     Check that we have enough update tokens left
*
      IF(MAXUPD-NUPDT.LT.2) THEN
         IF(IDEBFA.GE.0) PRINT 9001,NUPDT,MAXUPD
9001  FORMAT(' FMMVC. A "mv" operation requires two updates'/,
     +       '        You have made ',I10,' updates out of ',I10,
     +       ' maximum')
         RETURN
      ENDIF
*
*     Save current directory
*
      CALL RZCDIR(CHDIR,'R')
      LCDIR = LENOCC(CHDIR)
      CALL KUGETC(FILE1,LFILE1)
      CALL KUGETC(FILE2,LFILE2)
      IF((LFILE1.EQ.0).OR.(LFILE2.EQ.0)) RETURN
      CALL FMFIXF(FILE1,FTEMP)
      FILE1  = FTEMP
      LFILE1 = LENOCC(FILE1)
      CALL FMFIXF(FILE2,FTEMP)
      FILE2  = FTEMP
      LFILE2 = LENOCC(FILE2)
      CALL KUGETI(KSN)
*
*     Allow user to change keys, tape or disk details
*
      CALL KUGETI(JLOC)
      CALL KUGETI(JDAT)
      CALL KUGETI(JMED)
 
      CALL KUGETC(VSN,LVSN)
      CALL KUGETC(XVID,LVID)
      IF(LVID.NE.0) THEN
         CALL FMXVID(VID,JP,XVID,VIP,'I',IRC)
         LVID = LENOCC(VID)
      ENDIF
      CALL KUGETI(JFILE)
 
      CALL KUGETC(DSN,LDSN)
      CALL KUGETC(HOST,LHOST)
 
      IF(IDEBFA.GE.0) THEN
         PRINT *,'FMMVC. source: ',FILE1(1:LFILE1)
         PRINT *,'FMMVC. target: ',FILE2(1:LFILE2)
      ENDIF
 
      IF(KSN.EQ.0) THEN
         CALL VZERO(KEYS,10)
      ELSE
         KEYS(1) = KSN
      ENDIF
 
      LPATH = INDEXB(FILE1(1:LFILE1),'/') -1
      FNAME = FILE1(LPATH+2:LFILE1)
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
      CALL FMSELK(FILE1(1:LFILE1),KEYSIN,KEYSOU,NMATCH,MAXCOP,IRC)
      IF(NMATCH.EQ.0) THEN
         IF(IDEBFA.GE.0)
     +   PRINT *,'FMMVC. found 0 matches for ',FILE1(1:LFILE1)
         IRC = 1
         GOTO 99
      ELSEIF(NMATCH.GT.1.AND.KEYS(1).EQ.0) THEN
         IF(IDEBFA.GE.0) THEN
            PRINT *,'FMMVC. found ',NMATCH,' matches for ',
     +              FILE1(1:LFILE1)
            PRINT *,'FMMVC. Please specify which entry is to be moved'
         ENDIF
         IRC = 1
         GOTO 99
      ELSE
 
         IF(IDEBFA.GE.1)
     +   PRINT *,'FMMVC. found ',NMATCH,' matches for ',FILE1(1:LFILE1)
 
         CALL UCOPY(KEYS,KEYSIN,10)
 
         IFOUND = 0
         DO 10 I=1,NMATCH
*
*     Was a specific key serial number specified?
*
            IF((KEYSIN(1).NE.KEYSOU(1,I)).AND.(KEYSIN(1).NE.0)) GOTO 10
 
            IFOUND = 1
            CALL UCOPY(KEYSOU(1,I),KEYS,10)
 
            IF(IDEBFA.GE.1) THEN
               PRINT *,'FMMVC. candidate number ',I
               CALL FMPKEY(KEYSOU(1,I),LKEYFA)
            ENDIF
 
            LTDSFA = 0
            CALL FMGETK(FILE1(1:LFILE1),LTDSFA,KEYSOU(1,I),IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMMVC. Return code ',IRC,' '
     +         //'from FMGETK'
               GOTO 99
            ENDIF
*
*     Remove old file
*
            CALL FMRM(FILE1(1:LFILE1),LTDSFA,KEYS,IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMMVC. return code ',IRC,
     +            ' from FMRM'
               CALL MZDROP(IDIVFA,LTDSFA,' ')
               LTDSFA = 0
               GOTO 99
            ENDIF
*
*     Override fields that were given
*
            IF(JLOC.NE.0) THEN
               IQ(LTDSFA+KOFUFA+MLOCFA) = JLOC
            ENDIF
 
            IF(JDAT.NE.0) THEN
               IQ(LTDSFA+KOFUFA+MCPLFA) = JDAT
            ENDIF
 
            IF(JMED.NE.0) THEN
               IQ(LTDSFA+KOFUFA+MMTPFA) = JMED
            ENDIF
*
*     Tape information
*
            IF(LVID.GT.0) THEN
               CALL VBLANK(IQ(LTDSFA+KOFUFA+MVIDFA),NVIDFA/4)
               CALL UCTOH(VID,IQ(LTDSFA+KOFUFA+MVIDFA),4,LVID)
               IQ(LTDSFA+KOFUFA+MVIPFA) = JP
            ENDIF
 
            IF(LVSN.GT.0) THEN
               CALL VBLANK(IQ(LTDSFA+KOFUFA+MVSNFA),NVSNFA/4)
               CALL UCTOH(VSN,IQ(LTDSFA+KOFUFA+MVSNFA),4,LVSN)
            ENDIF
 
            IF(JFILE.NE.0) IQ(LTDSFA+KOFUFA+MFSQFA) = JFILE
*
*     Disk information
*
            IF(LDSN.GT.0) THEN
               CALL VBLANK(IQ(LTDSFA+KOFUFA+MFQNFA),NFQNFA/4)
               CALL UCTOH(DSN,IQ(LTDSFA+KOFUFA+MFQNFA),4,LDSN)
            ENDIF
 
            IF(LHOST.GT.0) THEN
               CALL VBLANK(IQ(LTDSFA+KOFUFA+MHSNFA),NHSNFA/4)
               CALL UCTOH(HOST,IQ(LTDSFA+KOFUFA+MHSNFA),4,LHOST)
            ENDIF
*
*     Put new file
*
            NFMVFL = NFMVFL + 1
            CALL FMPUT(FILE2(1:LFILE2),LTDSFA,IRC)
            CALL MZDROP(IDIVFA,LTDSFA,' ')
            LTDSFA = 0
   10    CONTINUE
 
      ENDIF
 
      IF(KSN.NE.0.AND.IFOUND.EQ.0.AND.IDEBFA.GE.-2) PRINT *,'FMMVC. ',
     +   ' no match found for ',PATH(1:LPATH),' key = ',KSN
 
   99 CONTINUE
*
*     Reset current directory
*
      CALL RZCDIR(CHDIR(1:LCDIR),' ')
      END
