*
* $Id: fmfc.F,v 1.1.1.1 1996/03/07 15:18:23 mclareni Exp $
*
* $Log: fmfc.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:23  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMFC
      PARAMETER (MAXFIL=100)
      PARAMETER (MAXDIR=500)
      PARAMETER (LKEYFA=10)
      CHARACTER*255 FILES,PATH,CHPATH,PREDIR
      CHARACTER*255 PATH2
      CHARACTER*255 CHDIR(MAXDIR),CHTMP
#include "fatmen/fatsys.inc"
#include "fatmen/fatout.inc"
      CHARACTER*20  CHFILE,MATCH,FNAME
      CHARACTER*36  CHOPT,OPTN
      CHARACTER*3   CHSTAT
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
      COMMON/FATFC/FILES(MAXFIL),MYKEYS(LKEYFA,MAXFIL)
      LOGICAL IEXIST,IOPEN
      DATA   NENT/0/
#include "fatmen/fatinit.inc"
*
      OUTPUT = ' '
      LOUT   = 0
      LWRITE = LPRTFA
      CALL FACDIR(PREDIR,'R')
 
      PATH = ' '
      CALL KUGETC(PATH,LPATH)
      CALL KUGETC(OUTPUT,LOUT)
      CALL KUGETC(CHOPT,LCHOPT)
 
      CALL FMFIXF(PATH,PATH2)
      PATH  = PATH2
      LPATH = LENOCC(PATH)
 
      IF(LCHOPT.EQ.0) THEN
         CHOPT  = ' '
         LCHOPT = 1
      ENDIF
 
      LWRITE = LPRTFA
*
*     Fix for strange KUIP behaviour...
*
      IF(OUTPUT(1:1).EQ.'-') THEN
         CHOPT  = OUTPUT
         LCHOPT = LOUT
         OUTPUT = 'TTY'
         LOUT   = 3
      ENDIF
 
      IF((OUTPUT(1:LOUT).NE.'TTY').OR.(LOUT.EQ.0)) THEN
         LWRITE = 3
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMFC. output will be redirected to ',OUTPUT(1:LOUT)
         CALL FAFILE(LWRITE,OUTPUT(1:LOUT),IRET)
         IF(IRET.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMFC. error ',IRET,
     +      ' opening file ',OUTPUT(1:LOUT),
     +      ' - output will be sent to the screen'
               LWRITE = LPRTFA
               OUTPUT = 'TTY'
         ENDIF
      ELSE
         OUTPUT = 'TTY'
      ENDIF
*
      IF(INDEX(CHOPT(1:LCHOPT),'H').NE.0) THEN
         WRITE(LWRITE,*) '>>> Command: FC ',
     +   'Options: ',CHOPT(1:LCHOPT)
         WRITE(LWRITE,*) '>>> Path: ',PATH(1:LPATH)
         WRITE(LWRITE,*) '>>> Current directory: ',
     +    PREDIR(1:LENOCC(PREDIR))
      ENDIF
*
 
      IOPTD = 0
      IOPTF = 0
      IOPTL = 0
      IOPTZ = 0
      IF(INDEX(CHOPT(1:LCHOPT),'D').NE.0) IOPTD = 1
      IF(INDEX(CHOPT(1:LCHOPT),'F').NE.0) IOPTF = 1
      IF(INDEX(CHOPT(1:LCHOPT),'L').NE.0) IOPTL = 1
      IF(INDEX(CHOPT(1:LCHOPT),'Z').NE.0) IOPTZ = 1
 
      IF(IOPTL.NE.0) IOPTD = 1
      IF(IOPTZ.NE.0) IOPTF = 1
*
      NFOUND = 0
      NMATCH = 0
      NDIRT  = 0
*
*     Count number of files matching pattern
*
      LSLASH = INDEXB(PATH(1:LPATH),'/')
      CHPATH = PATH(1:LSLASH-1)
      CHFILE = PATH(LSLASH+1:LPATH)
      LP     = LENOCC(CHPATH)
      LF     = LENOCC(CHFILE)
      IF(IDEBFA.GE.2) PRINT *,'FMFC. enter for ',PATH(1:LPATH)
      IWILD  = ICFMUL('*%(<',CHPATH,1,LP)
      IF(IWILD.LE.LP) THEN
         GOTO 10
      ELSE
         NDIRS    = 1
         CHDIR(1) = CHPATH
         GOTO 20
      ENDIF
*
*     Get list of subdirectories
*
   10 CONTINUE
      CALL FMLDIR(CHPATH(1:LP),CHDIR,NDIRS,MAXDIR,ICONT,IRC)
      IF(IRC.EQ.-1) THEN
         ICONT = 1
      ELSE
         ICONT = 0
      ENDIF
 
   20 CONTINUE
 
      NDIRT = NDIRT + NDIRS
 
      DO 70 I=1,NDIRS
         LEND = LENOCC(CHDIR(I))
         CALL FACDIR(CHDIR(I)(1:LEND),' ')
*
*     How many subdirectories are there at this level?
*
         CALL RZRDIR(1,CHTMP,NDIR)
         NDIR = IQUEST(11)
*
*     Get total number of files, number which match
*
         IFIRST = 1
         ILAST  = MAXFIL
         NMAT   = 0
         NFIL   = 0
 
   30    CONTINUE
         CALL FMKEYS(LKEYFA,MAXFIL,IFIRST,ILAST,MYKEYS,NFILES,IRET)
         NFIL = IQUEST(11)
         IF(IQUEST(1) .NE. 0) THEN
            IF(IDEBFA.GE.2) PRINT *,'FMFC. More than ',MAXFIL,' files '
     +      //'in ',PATH(1:LP)
            IF(IDEBFA.GE.2) PRINT *,'FMFC. IQUEST(11-12) = ',IQUEST(11)
     +      ,IQUEST(12)
         ENDIF
*
*     Process all keys returned and move those that match to KEYS
*
         NRET = IQUEST(13)
         IF(NRET.EQ.0) GOTO 60
   40    CONTINUE
         DO 50 J=1,NRET
            CALL UHTOC(MYKEYS(2,J),4,FNAME,(MKCLFA-MKFNFA)*4)
            CALL FMATCH(FNAME(1:LENOCC(FNAME)),CHFILE(1:LF),IMAT)
            IF(IMAT.NE.0) GOTO 50
            NMAT = NMAT + 1
 
   50    CONTINUE
         IF(ILAST.LT.NFIL) THEN
            IFIRST = IFIRST + MAXFIL
            ILAST = MIN(NFIL,IFIRST+MAXFIL-1)
            GOTO 30
         ENDIF
 
   60    CONTINUE
         NFOUND = NFOUND + NFIL
         NMATCH = NMATCH + NMAT
*
*     Display only empty (zero files) directories
*
         IF((IOPTZ.NE.0).AND.(NFIL.NE.0)) GOTO 70
*
*     Display only lowest level directories
*
         IF((IOPTL.NE.0).AND.(NDIR.NE.0)) GOTO 70
 
          IF((IOPTD.NE.0).OR.(IOPTF.NE.0)) THEN
             WRITE(LWRITE,*)
             WRITE(LWRITE,*) 'Directory: ',CHDIR(I)(1:LEND)
         ENDIF
 
         IF(IOPTD.NE.0) WRITE(LWRITE,*) ' subdirectories: ',NDIR
*
*     Number of files, matches
*
         IF(IOPTF.NE.0) WRITE(LWRITE,*) ' files: ',NFIL,
     +      ' matches: ',NMAT
 
   70 CONTINUE
      IF(ICONT.NE.0) GOTO 10
 
      WRITE(LWRITE,9001) NMATCH,NFOUND,NDIRT
 9001 FORMAT(' Total of ',I6,' matches (',I6,' files) in ',I6,
     +       ' directories')
 
      INQUIRE(3,OPENED=IOPEN)
      IF(IOPEN) CLOSE(3)
 
      CALL FACDIR(PREDIR,' ')
      RETURN
      END
