*
* $Id: fmfc.F,v 1.1.1.1 1996/03/07 15:17:42 mclareni Exp $
*
* $Log: fmfc.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:42  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMFC
      CHARACTER*255 FILES,PATH,CHPATH,PREDIR
      CHARACTER*255 PATH2
#include "fatmen/fatsys.inc"
#include "fatmen/fatout.inc"
      CHARACTER*36  CHOPT
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
      LOGICAL IEXIST,IOPEN
#include "fatmen/fafccm.inc"
      EXTERNAL      FAFC
#include "fatmen/fatinit.inc"
*
      OUTPUT = ' '
      LOUT   = 0
      LWRITE = LPRTFA
      CALL RZCDIR(PREDIR,'R')
      LDIR = LENOCC(PREDIR)
      CALL RZCDIR(PREDIR(1:LDIR),'U')
 
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
     +    PREDIR(1:LDIR)
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
 
      IF(ICFMUL('*%(<>[]',CHPATH,1,LP).GT.LP) THEN
         CALL RZCDIR(CHPATH(1:LP),' ')
         IQUEST(12) = IQUEST(9)
         IQUEST(14) = IQUEST(7)
         CALL FAFC(CHPATH(1:LP),IRC)
      ELSE
         CALL FMSCAN(CHPATH(1:LPATH),99,FAFC,IRC)
      ENDIF
 
      IF(IDEBFA.GE.-2) THEN
         WRITE(LWRITE,9001) NMATCH,NFOUND,NDIRT
 9001    FORMAT(' Total of ',I8,' matches (',I8,' files) in ',I6,
     +          ' directories')
      ENDIF
 
      INQUIRE(3,OPENED=IOPEN)
      IF(IOPEN) CLOSE(3)
 
      CALL RZCDIR(PREDIR(1:LDIR),' ')
      RETURN
      END
