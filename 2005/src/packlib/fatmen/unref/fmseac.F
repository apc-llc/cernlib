*
* $Id: fmseac.F,v 1.1.1.1 1996/03/07 15:18:24 mclareni Exp $
*
* $Log: fmseac.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:24  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSEAC
      PARAMETER (MAXFIL=1000)
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA,MAXFIL),JSORT(MAXFIL)
      DIMENSION KEYSAV(LKEYFA)
      CHARACTER*255 PATH2
      CHARACTER*255 FILES,PATH,CHPATH,PREDIR,GENAM,OLDNAM,
     +              CWD,OLDWD
      CHARACTER*255 DSN
      CHARACTER*80  CHCRE,CHCAT,CHACC
      CHARACTER*12  HOST,VID,USER
#include "fatmen/fatsea.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatloc.inc"
#include "fatmen/fatmtp.inc"
#include "fatmen/fatcpl.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatout.inc"
      CHARACTER*20  CHFILE,MATCH
      CHARACTER*36  CHOPT,OPTN
#include "fatmen/fatbank.inc"
      COMMON /MYWKSP/ FILES(MAXFIL)
      CHARACTER*132 CARD
      CHARACTER*3   CHSTAT
      LOGICAL       IOPEN,IEXIST
      DATA   NENT/0/
#include "fatmen/fatinit.inc"
*
      OUTPUT = ' '
      LOUT   = 0
      LWRITE = LPRTFA
      CALL FACDIR(PREDIR,'R')
      CALL FACDIR(PREDIR,'U')
 
      IF(NENT.EQ.0) THEN
         NENT = 1
*        RETURN
      ENDIF
 
      PATH = ' '
      CALL KUGETC(PATH,LPATH)
      CALL FMFIXF(PATH,PATH2)
      PATH  = PATH2
      LPATH = LENOCC(PATH2)
 
      CALL KUGETC(DSN,LDSN)
      CALL KUGETC(HOST,LHOST)
      CALL KUGETC(VID,LVID)
      CALL KUGETC(USER,LUSER)
      CALL KUGETI(NMATCH)
      CALL KUGETC(CHCRE,LCHCRE)
      CALL KUGETC(CHCAT,LCHCAT)
      CALL KUGETC(CHACC,LCHACC)
      CALL KUGETC(OUTPUT,LOUT)
      CALL KUGETC(CHOPT,LCHOPT)
 
      IF(LDSN.EQ.0)   DSN   = ' '
      IF(LHOST.EQ.0)  HOST  = ' '
      IF(LVID.EQ.0)   VID   = ' '
      IF(LUSER.EQ.0)  USER  = ' '
      IF(LCHOPT.EQ.0) CHOPT = ' '
*
*     Date and time ranges:
*
      IF(LCHCRE.NE.0) THEN
         CALL FMDTRN(CHCRE(1:LCHCRE),IDCREA,ITCREA,JDCREA,JTCREA,IC)
         IF(IDEBFA.GE.1) PRINT *,'FMSEAC. range of creation dates & ',
     +      'times is ',IDCREA,ITCREA,JDCREA,JTCREA
      ELSE
         IDCREA = -1
      ENDIF
 
      IF(LCHCAT.NE.0) THEN
         CALL FMDTRN(CHCAT(1:LCHCAT),IDCATA,ITCATA,JDCATA,JTCATA,IC)
         IF(IDEBFA.GE.1) PRINT *,'FMSEAC. range of dates & ',
     +      'times of cataloging is ',IDCATA,ITCATA,JDCATA,JTCATA
      ELSE
         IDCATA = -1
      ENDIF
 
      IF(LCHACC.NE.0) THEN
         CALL FMDTRN(CHACC(1:LCHACC),IDLAST,ITLAST,JDLAST,JTLAST,IC)
         IF(IDEBFA.GE.1) PRINT *,'FMSEAC. range of dates & ',
     +      'times of last access is ',IDLAST,ITLAST,JDLAST,JTLAST
      ELSE
         IDLAST = -1
      ENDIF
 
      IF((OUTPUT(1:LOUT).EQ.'TTY').OR.(LOUT.EQ.0)) THEN
         OUTPUT = ' '
         LWRITE = LPRTFA
      ELSE
         LWRITE = 3
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMSEAC. output will be redirected to ',OUTPUT(1:LOUT)
         CALL FAFILE(LWRITE,OUTPUT(1:LOUT),IRET)
         IF(IRET.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMSEAC. return code ',IRET,
     +                                  'from OPEN'
         ENDIF
      ENDIF
 
      IF(LENOCC(CHOPT).EQ.0) THEN
         CHOPT  = ' '
         LCHOPT = 1
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
      OLDWD  = ' '
      LOLDWD = 1
*
* Look for some files and show the bank and key content:
*
      LSLASH = INDEXB(PATH(1:LPATH),'/')
      CHPATH = PATH(1:LSLASH-1)
      MATCH  = PATH(LSLASH+1:LPATH)
      LP     = LENOCC(CHPATH)
      LM     = LENOCC(MATCH)
      IF(IDEBFA.GE.2) PRINT *,'FMSEAC. enter for ',PATH(1:LPATH),
     +                CHOPT
1     CONTINUE
      CALL FMLFIL(PATH(1:LPATH),FILES,KEYS,NFOUND,MAXFIL,JCONT,IRC)
      IF(IRC.EQ.-1) THEN
         JCONT = 1
      ELSE
         JCONT = 0
      ENDIF
*     CALL FMLIST(CHPATH(1:LP),FILES,KEYS,NFOUND,MAXFIL,IRC)
      OLDNAM = ' '
      LOLD   = 1
      IF(INDEX(CHOPT,'I').NE.0) THEN
         IF(IDEBFA.EQ.3) PRINT *,'FMSEAC. sorting file names...'
         CALL FMSORT(FILES,KEYS,NFOUND,JSORT,IC)
      ENDIF
      IWIDTH = 0
      JWIDTH = 78
      IF(INDEX(CHOPT,'1').NE.0) JWIDTH = 132
      DO 200 I=1,NFOUND
         IF(INDEX(CHOPT,'I').NE.0) THEN
            J = JSORT(I)
         ELSE
            J = I
         ENDIF
         GENAM  = FILES(J)
         LFILE  = LENOCC(GENAM)
         IF(IDEBFA.GE.3) PRINT *,'FMSEAC. GENAM  = ',GENAM(1:LFILE)
         IF(IDEBFA.GE.3) PRINT *,'FMSEAC. OLDNAM = ',OLDNAM(1:LOLD)
         IF(INDEX(CHOPT,'X').NE.0) THEN
            IF(GENAM.EQ.OLDNAM) GOTO 200
         ENDIF
         ISLASH = INDEXB(OLDNAM(1:LOLD),'/')
         JSLASH = INDEXB(GENAM(1:LFILE),'/')
         CHFILE = GENAM(JSLASH+1:LFILE)
         LF     = LENOCC(CHFILE)
 
         IF(GENAM(1:JSLASH-1).NE.OLDWD(1:LOLDWD)) THEN
            IF(IDEBFA.GE.0)
     +         PRINT *,'Searching directory ',GENAM(1:JSLASH-1)
            OLDWD  = GENAM(1:JSLASH-1)
            LOLDWD = JSLASH-1
         ENDIF
         CALL FMATCH(CHFILE(1:LF),MATCH(1:LM),IMAT)
         IF(IMAT.NE.0) GOTO 200
*
*     Check that keys match those selected
*     Location code:
*
        IF(NUMLOC.GT.0) THEN
           IF(IUCOMP(KEYS(MKLCFA,J),MFMLOC,NUMLOC).EQ.0) THEN
           IF(IDEBFA.GE.3) PRINT *,'FMLSC. candidate # ',J,
     +        ' fails location code check'
           GOTO 200
           ENDIF
        ENDIF
*
*     Copy level:
*
        IF(NUMCPL.GT.0) THEN
           IF(IUCOMP(KEYS(MKCLFA,J),MFMCPL,NUMCPL).EQ.0) THEN
           IF(IDEBFA.GE.3) PRINT *,'FMLSC. candidate # ',J,
     +        ' fails copy level check'
           GOTO 200
           ENDIF
        ENDIF
*
*     Media type:
*
        IF(NUMMTP.GT.0) THEN
           IF(IUCOMP(KEYS(MKMTFA,J),MFMMTP,NUMMTP).EQ.0) THEN
           IF(IDEBFA.GE.3) PRINT *,'FMLSC. candidate # ',J,
     +        ' fails media type check'
           GOTO 200
           ENDIF
        ENDIF
 
         LBANK=0
         IF(INDEX(CHOPT,'X').NE.0) THEN
*
*     Option X - display each entry only once
*     Display entry chosen by current selection unless not found
*
            CALL UCOPY(KEYS(1,J),KEYSAV,10)
            CALL FMGET(GENAM,LBANK,KEYS(1,J),IC)
            IF(IC.NE.0) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMSEAC. ** Warning. ** ',
     +         'No match with current selection criteria for ',
     +         CHFILE(1:LF)
               CALL UCOPY(KEYSAV,KEYS(1,J),10)
            ENDIF
            LBANK=0
         ENDIF
*
*     Now check if this entry matches
*
         CALL FMSEAR(GENAM,LBANK,KEYS(1,J),DSN,HOST,VID,USER,IRET)
         IF(IRET.NE.0) THEN
            CALL MZDROP(IXSTOR,LBANK,' ')
            LBANK = 0
            GOTO 200
         ENDIF
         NFILES = NFILES + 1
 
         IF(INDEX(CHOPT,'W').NE.0) THEN
            IF(OLDNAM(1:JSLASH-1).NE.GENAM(1:ISLASH-1)) THEN
               IF(IWIDTH.GT.0) THEN
                  WRITE(LWRITE,'(1X,A)') CARD(1:IWIDTH)
                  IWIDTH = 0
               ENDIF
               WRITE(LWRITE,*)
               WRITE(LWRITE,8001) GENAM(1:JSLASH-1)
               WRITE(LWRITE,*)
 8001    FORMAT(' Directory :',A)
            ENDIF
*
*     Just display file names across the terminal...
*
            IF(IWIDTH+LF.GE.JWIDTH) THEN
*     flush current buffer
               WRITE(LWRITE,'(1X,A)') CARD(1:IWIDTH)
               IWIDTH = 0
            ENDIF
            IF(IWIDTH.EQ.0) THEN
               CARD = CHFILE(1:LF) // ' '
            ELSE
               CARD = CARD(1:IWIDTH) // CHFILE(1:LF) // ' '
            ENDIF
            IWIDTH = IWIDTH + LF + 1
         ELSE
            CALL FMSHOW(GENAM,LBANK,KEYS(1,J),CHOPT(1:LCHOPT),IRC)
            CALL MZDROP(IXSTOR,LBANK,' ')
            LBANK = 0
         ENDIF
         OLDNAM = GENAM
         LOLD   = LFILE
         IF((NFILES.EQ.NMATCH).AND.(NMATCH.NE.0)) THEN
            IF(INDEX(CHOPT,'W').NE.0)
     +                       WRITE(LWRITE,'(1X,A)') CARD(1:IWIDTH)
            IF(IDEBFA.GE.-3) PRINT *,'FMSEAC. stopping after ',
     +         NFILES,' matches'
            GOTO 400
         ENDIF
  200 CONTINUE
  300 CONTINUE
      IF(INDEX(CHOPT,'W').NE.0) WRITE(LWRITE,'(1X,A)') CARD(1:IWIDTH)
 
      IF(JCONT.NE.0) GOTO 1
 
  400 CONTINUE
 
      IF(INDEX(CHOPT,'D').NE.0) THEN
         PRINT *,NFILES,' candidates for deletion'
      ELSE
         IF(IDEBFA.GE.-2) THEN
 
            WRITE(LWRITE,'(A)') ' '
            IF(NFILES .EQ. 0) THEN
               WRITE(LWRITE,8016)
 8016          FORMAT(' No matches found')
            ELSE
               WRITE(LWRITE,8017) NFILES
 8017          FORMAT(' Matches:',I5)
            ENDIF
         ENDIF
      ENDIF
 
      INQUIRE(3,OPENED=IOPEN)
      IF(IOPEN) CLOSE(3)
      CALL FACDIR(PREDIR,' ')
      RETURN
      END
