*
* $Id: fmskan.F,v 1.1.1.1 1996/03/07 15:17:44 mclareni Exp $
*
* $Log: fmskan.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:44  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSKAN
      CHARACTER*255 PATH2
      CHARACTER*255 PATH,CHPATH,PREDIR
      CHARACTER*80  CHCRE,CHCAT,CHACC
#include "fatmen/facard.inc"
#include "fatmen/fasecm.inc"
#include "fatmen/fatsea.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatuwd.inc"
#include "fatmen/fatloc.inc"
#include "fatmen/fatmtp.inc"
#include "fatmen/fatcpl.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatout.inc"
      CHARACTER*36  CHOPT
#include "fatmen/fatbank.inc"
      LOGICAL       IOPEN,IEXIST
      EXTERNAL      FASEAR
#include "fatmen/fatinit.inc"
*
      OUTPUT = ' '
      LOUT   = 0
      LWRITE = LPRTFA
      CALL RZCDIR(PREDIR,'R')
      CALL RZCDIR(PREDIR,'U')
 
      PATH = ' '
      CALL KUGETC(PATH,LPATH)
      CALL FMFIXF(PATH,PATH2)
      PATH  = PATH2
      LPATH = LENOCC(PATH2)
 
      CALL KUGETI(NLEVEL)
 
      CALL KUGETC(DSN,LDSN)
      CALL KUGETC(HOST,LHOST)
      CALL KUGETC(VID,LVID)
      CALL KUGETC(USER,LUSER)
      CALL KUGETI(NMATCH)
      CALL KUGETC(CHCRE,LCHCRE)
      CALL KUGETC(CHCAT,LCHCAT)
      CALL KUGETC(CHACC,LCHACC)
      CALL KUGETC(UFORM,LFORM)
      CALL KUGETC(COMM,LCOMM)
      CALL KUGETC(OUTPUT,LOUT)
      CALL KUGETC(CHSTR,LSTR)
      CALL KUGETC(CHOPT,LCHOPT)
 
      IF(LDSN.EQ.0)   DSN   = ' '
      IF(LHOST.EQ.0)  HOST  = ' '
      IF(LVID.EQ.0)   VID   = ' '
      IF(LUSER.EQ.0)  USER  = ' '
      IF(LFORM.EQ.0)  UFORM = ' '
      IF(LSTR.EQ.0)   CHSTR = ' '
      IF(LCOMM.EQ.0)  COMM  = ' '
      IF(LCHOPT.EQ.0) CHOPT = ' '
 
      IF(IDEBFA.GE.1) THEN
         IF(LDSN .GT.0) PRINT *,'FMSKAN. dsn     = ',DSN(1:LDSN)
         IF(LHOST.GT.0) PRINT *,'FMSKAN. host    = ',HOST(1:LHOST)
         IF(LVID .GT.0) PRINT *,'FMSKAN. vid     = ',VID(1:LVID)
         IF(LUSER.GT.0) PRINT *,'FMSKAN. user    = ',USER(1:LUSER)
         IF(LFORM.GT.0) PRINT *,'FMSKAN. uform   = ',UFORM(1:LFORM)
         IF(LSTR .GT.0) PRINT *,'FMSKAN. string  = ',CHSTR(1:LSTR)
         IF(LCOMM.GT.0) PRINT *,'FMSKAN. comment = ',COMM(1:LCOMM)
      ENDIF
*
*     Date and time ranges:
*
      IF(LCHCRE.NE.0) THEN
         CALL FMDTRN(CHCRE(1:LCHCRE),IDCREA,ITCREA,JDCREA,JTCREA,IC)
         IF(IDEBFA.GE.1) PRINT *,'FMSKAN. range of creation dates & ',
     +      'times is ',IDCREA,ITCREA,JDCREA,JTCREA
      ELSE
         IDCREA = -1
      ENDIF
 
      IF(LCHCAT.NE.0) THEN
         CALL FMDTRN(CHCAT(1:LCHCAT),IDCATA,ITCATA,JDCATA,JTCATA,IC)
         IF(IDEBFA.GE.1) PRINT *,'FMSKAN. range of dates & ',
     +      'times of cataloging is ',IDCATA,ITCATA,JDCATA,JTCATA
      ELSE
         IDCATA = -1
      ENDIF
 
      IF(LCHACC.NE.0) THEN
         CALL FMDTRN(CHACC(1:LCHACC),IDLAST,ITLAST,JDLAST,JTLAST,IC)
         IF(IDEBFA.GE.1) PRINT *,'FMSKAN. range of dates & ',
     +      'times of last access is ',IDLAST,ITLAST,JDLAST,JTLAST
      ELSE
         IDLAST = -1
      ENDIF
*
*     Copy ranges of user words
*
      CALL UCOPY(IFUSER,IUSER,20)
 
      IF((OUTPUT(1:LOUT).EQ.'TTY').OR.(LOUT.EQ.0)) THEN
         OUTPUT = ' '
         LWRITE = LPRTFA
      ELSE
         LWRITE = 3
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMSKAN. output will be redirected to ',OUTPUT(1:LOUT)
         CALL FAFILE(LWRITE,OUTPUT(1:LOUT),IRET)
         IF(IRET.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMSKAN. return code ',IRET,
     +                                  'from OPEN for ',OUTPUT(1:LOUT)
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
         PRINT *,'FMSKAN. warning - files should be deleted by '//
     +     'descending key serial number'
         PRINT *,'FMSKAN. directories will be scanned backwards'
      ENDIF
*
      IF(INDEX(CHOPT(1:LCHOPT),'H').NE.0) THEN
         WRITE(LWRITE,*) '>>> Command: SEARCH ',
     +   'Options: ',CHOPT(1:LCHOPT)
         WRITE(LWRITE,*) '>>> Path: ',PATH(1:LPATH)
         WRITE(LWRITE,*) '>>> Current directory: ',
     +   PREDIR(1:LENOCC(PREDIR))
      ENDIF
*
      NFILES = 0
      NDIRS  = 0
*
* Look for some files and show the bank and key content:
*
      LSLASH = INDEXB(PATH(1:LPATH),'/')
      CHPATH = PATH(1:LSLASH-1)
      CHFILE = PATH(LSLASH+1:LPATH)
      LP     = LENOCC(CHPATH)
      LC     = LENOCC(CHFILE)
      IF(IDEBFA.GE.2) PRINT *,'FMSKAN. enter for path = ',
     +   PATH(1:LPATH),' chopt = ',CHOPT
 
      CALL FMOPTC(CHOPT,ALFNUM,IOPT)
 
      IF(IOPTD.EQ.0) THEN
         CALL FMLOOP(PATH(1:LPATH),NLEVEL,FASEAR,IRC)
      ELSE
         CALL FMBACK(PATH(1:LPATH),NLEVEL,FASEAR,IRC)
      ENDIF
*
*     flush buffer
*
      IF(IWIDTH.GT.0.AND.INDEX(CHOPT(1:LCHOPT),'W').NE.0)
     +   WRITE(LWRITE,'(1X,A)') CARD(1:IWIDTH)
 
      WRITE(LWRITE,*)
 
      IF(IOPTD.NE.0) THEN
         WRITE(LPRTFA,*) 'Total of ',NFILES,
     +      ' candidates for deletion in ',NDIRS,' directories'
      ELSE
         WRITE(LWRITE,*) 'Total of ',NFILES,
     +      ' matches in ',NDIRS,' directories'
      ENDIF
 
      INQUIRE(3,OPENED=IOPEN)
      IF(IOPEN) CLOSE(3)
      CALL RZCDIR(PREDIR,' ')
      RETURN
      END
