*
* $Id: fmfilc.F,v 1.1.1.1 1996/03/07 15:18:24 mclareni Exp $
*
* $Log: fmfilc.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:24  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMFILC(PATH,NFILES,IRC)
      PARAMETER (MAXFIL=1000)
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA,MAXFIL)
      CHARACTER*(*) PATH
      CHARACTER*255 FILES,CHPATH,PREDIR
      CHARACTER*20  CHFILE,MATCH
      CHARACTER*36  CHOPT,OPTN
#include "fatmen/fatbank.inc"
      COMMON /MYWKSP/ FILES(MAXFIL)
*
      IRC    = 0
      NFILES = 0
*
*     Count number of files matching pattern
*
      LPATH  = LENOCC(PATH)
      LSLASH = INDEXB(PATH(1:LPATH),'/')
      CHPATH = PATH(1:LSLASH-1)
      CHFILE = PATH(LSLASH+1:LPATH)
      LP     = LENOCC(CHPATH)
      LF     = LENOCC(CHFILE)
      IF(IDEBFA.GE.2) PRINT *,'FMFILC. enter for ',PATH(1:LPATH)
      CALL FMLIST(CHPATH(1:LP),FILES,KEYS,NFOUND,MAXFIL,IRC)
      DO 200 I=1,NFOUND
         LFILE  = LENOCC(FILES(I))
         JSLASH = INDEXB(FILES(I)(1:LFILE),'/')
         MATCH  = FILES(I)(JSLASH+1:LFILE)
         LM     = LENOCC(MATCH)
         CALL FMATCH(MATCH(1:LM),CHFILE(1:LF),IMAT)
         IF(IMAT.NE.0) GOTO 200
         LBANK=0
         NFILES = NFILES + 1
200   CONTINUE
 
      RETURN
      END
 
