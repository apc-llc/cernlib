*
* $Id: fmlsc.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fmlsc.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMLSC
      CHARACTER*255 PATH,CHPATH,PREDIR
      CHARACTER*255 PATH2,CHNAME
      CHARACTER*255 CHDESC
#include "fatmen/fmnkeys.inc"
      DIMENSION     KEYS(LKEYFA)
#include "fatmen/slate.inc"
#include "fatmen/falscm.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatloc.inc"
#include "fatmen/fatmtp.inc"
#include "fatmen/fatcpl.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatout.inc"
#include "fatmen/fatsho.inc"
      CHARACTER*36  CHOPT
      DIMENSION     JOPT(36)
#include "fatmen/fatbank.inc"
      LOGICAL       IOPEN,IEXIST
      CHARACTER*5   KFILE
      CHARACTER*11  KDIR
      EXTERNAL      FALS
#include "fatmen/fatinit.inc"
*
      OUTPUT = ' '
      LOUT   = 0
      LWRITE = LPRTFA
 
      NFILES = 0
      NDIRT  = 0
      NFILT  = 0
 
      CALL RZCDIR(PREDIR,'R')
      LPRE = LENOCC(PREDIR)
      CALL RZCDIR(PREDIR(1:LPRE),'U')
 
      CWDSHO = ' '
 
      PATH  = ' '
      PATH2 = ' '
      CALL KUGETC(PATH,LPATH)
      CALL KUGETI(KSN)
      CALL KUGETC(OUTPUT,LOUT)
      CALL KUGETC(CHNAME,LNAME)
      CALL KUGETC(CHOPT,LCHOPT)
 
      IF(PATH(1:1).EQ.'%') THEN
*
*     Names file handling
*
      CALL FMGLUN(LUN,IC)
      CALL FMNICK(LUN,CHNAME(1:LNAME),PATH(2:LPATH),
     +   PATH2,CHDESC,CHOPT,IRC)
      CALL FMFLUN(LUN,IC)
 
      IF(IRC.EQ.28) RETURN
 
      IF(IRC.EQ.32) THEN
         PRINT *,'FMLSC. nick name ',PATH(2:LPATH),
     +      ' not found in names file'
         RETURN
      ENDIF
 
      LNICK = LENOCC(PATH2)
      LDESC = LENOCC(CHDESC)
 
      WRITE(LWRITE,9002) PATH(2:LNICK),PATH2(1:LNICK),CHDESC(1:LDESC)
9002  FORMAT(' FMLSC. nickname: ',A,' = ',A/,'      description: ',A)
 
      ELSEIF(PATH(1:1).EQ.'$') THEN
*
*     Environment variables
*
         LEND = INDEX(PATH(1:LPATH),'/')
         IF(LEND.EQ.0) LEND = LPATH
         CALL GETENVF(PATH(2:LEND),PATH2)
         LPATH = IS(1)
         IF(LPATH.EQ.0) THEN
            PRINT *,'FMLSC. environment variable ',PATH(2:LEND),
     +         ' not defined'
            RETURN
         ENDIF
      ELSE
         PATH2 = PATH(1:LPATH)
      ENDIF
*
*     default handling
*
      CALL FMFIXF(PATH2,PATH)
      LPATH = LENOCC(PATH)
 
      IF(LCHOPT.EQ.0) THEN
         CHOPT  = ' '
         LCHOPT = 1
      ENDIF
 
      CALL FMOPTC(CHOPT,ALFNUM,JOPT)
      CALL UCOPY(JOPT,IOPT,36)
*
*     Fix for strange KUIP behaviour...
*
      IF(OUTPUT(1:1).EQ.'-') THEN
         CHOPT  = OUTPUT
         LCHOPT = LOUT
         OUTPUT = 'TTY'
         LOUT   = 3
      ENDIF
 
      LOUT      = LENOCC(OUTPUT)
      IF((OUTPUT(1:LOUT).EQ.'TTY').OR.(LOUT.EQ.0)) THEN
         OUTPUT = ' '
         LWRITE = LPRTFA
      ELSE
         LWRITE = 3
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMLSC. output will be redirected to ',OUTPUT(1:LOUT)
         CALL FAFILE(LWRITE,OUTPUT(1:LOUT),IRET)
         IF(IRET.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMLSC. return code ',IRET,
     +      ' opening file ',OUTPUT(1:LOUT),
     +      ' - output will be sent to screen'
            OUTPUT = ' '
            LWRITE = LPRTFA
         ENDIF
      ENDIF
 
      IF(LENOCC(CHOPT).EQ.0) THEN
         CHOPT  = ' '
         LCHOPT = 1
      ENDIF
*
*     Check for D option
*
      IF(INDEX(CHOPT(1:LCHOPT),'D').NE.0) THEN
         PRINT *,'FMLSC. warning - files should be deleted by '//
     +     'descending key serial number'
      ENDIF
*
      NFILES = 0
*
* Look for some files and show the bank and key content:
*
      LSLASH = INDEXB(PATH(1:LPATH),'/')
      CHPATH = PATH(1:LSLASH-1)
      CHFILE = PATH(LSLASH+1:LPATH)
      LP     = LENOCC(CHPATH)
      LF     = LENOCC(CHFILE)
 
      IF(IDEBFA.GE.2) PRINT *,'FMLSC. enter for ',PATH(1:LPATH),
     +                ',',OUTPUT,',',CHOPT
*
      IF(INDEX(CHOPT(1:LCHOPT),'H').NE.0) THEN
         WRITE(LWRITE,*) '>>> Command: LS ',
     +   'Options: ',CHOPT(1:LCHOPT)
         WRITE(LWRITE,*) '>>> Path: ',PATH(1:LPATH)
         WRITE(LWRITE,*) '>>> Current directory: ',
     +   PREDIR(1:LENOCC(PREDIR))
      ENDIF
 
      IF(ICFMUL('*%(<>[]',CHPATH,1,LP).GT.LP) THEN
*
*     Was a KSN specified?
*
         IF(KSN.NE.0) THEN
            NDIRT   = 1
            KEYS(1) = KSN
            LBANK   = 0
            CALL FMGBYK(PATH(1:LPATH),LBANK,KEYS,IRC)
            IF(IRC.EQ.0) THEN
               NFILT = 1
               CALL FMSHOW(PATH(1:LPATH),LBANK,KEYS,CHOPT,IRC)
               CALL MZDROP(IDIVFA,LBANK,' ')
               LBANK = 0
            ENDIF
         ELSE
            CALL RZCDIR(CHPATH(1:LP),' ')
            CALL FALS(CHPATH(1:LP),IRC)
         ENDIF
      ELSE
*
*     Directory scan required
*
         CALL FMSCAN(CHPATH(1:LP),99,FALS,IRC)
      ENDIF
 
      IF(NFILT.EQ.1) THEN
         KFILE = 'file '
      ELSE
         KFILE = 'files'
      ENDIF
 
      IF(NDIRT.EQ.1) THEN
         KDIR = 'directory  '
      ELSE
         KDIR = 'directories'
      ENDIF
 
      WRITE(LWRITE,9001) NFILT,KFILE,NDIRT,KDIR
 9001 FORMAT(' Total of ',I6,1X,A5,' in ',I6,1X,A11)
 
      INQUIRE(3,OPENED=IOPEN)
      IF(IOPEN) CLOSE(3)
      CALL RZCDIR(PREDIR(1:LPRE),' ')
      END
