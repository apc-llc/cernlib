*
* $Id: fmls.F,v 1.1.1.1 1996/03/07 15:18:23 mclareni Exp $
*
* $Log: fmls.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:23  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMLS(PATHN,CHOPT,IRC)
      CHARACTER*(*) PATHN
      PARAMETER (MAXFIL=1000)
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA,MAXFIL),JSORT(MAXFIL),KEYSAV(LKEYFA)
      CHARACTER*255 FILES,PATH,CHPATH,PREDIR,GENAM,OLDNAM
#include "fatmen/fatsho.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatloc.inc"
#include "fatmen/fatmtp.inc"
#include "fatmen/fatcpl.inc"
#include "fatmen/fatout.inc"
      CHARACTER*20  CHFILE,MATCH
#include "fatmen/fatbank.inc"
      COMMON /MYWKSP/ FILES(MAXFIL)
      CHARACTER*132 CARD
#include "fatmen/fatopts.inc"
*
*     Options which effect this routine: I=sort in increasing order
*                                        X=display each generic name only once
*                                        V=very wide display
*                                        X=wide display
*
      PATH   = PATHN
      LPATH  = LENOCC(PATHN)
      PATH   = PATHN(1:LPATH)
      IRC    = 0
      IF(IOPTI.NE.0) IOPTX = 1
 
      OUTPUT = 'TTY'
      LOUT   = 3
      LWRITE = LPRTFA
 
      CWDSHO = ' '
 
      CALL FACDIR(PREDIR,'R')
*
      NFILES = 0
*
* Look for some files and show the bank and key content:
*
      LSLASH = INDEXB(PATH(1:LPATH),'/')
      CHPATH = PATH(1:LSLASH-1)
      MATCH  = PATH(LSLASH+1:LPATH)
      LP     = LENOCC(CHPATH)
      LM     = LENOCC(MATCH)
      IF(IDEBFA.GE.2) PRINT *,'FMLS. enter for ',PATH(1:LPATH),
     +                CHOPT
      JCONT  = 0
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
      IF(IOPTI.NE.0) THEN
         IF(IDEBFA.GE.3) PRINT *,'FMLS. sorting file names...'
         CALL FMSORT(FILES,KEYS,NFOUND,JSORT,IC)
      ENDIF
      IWIDTH = 0
      JWIDTH = 78
      IF(IOPTV.NE.0) JWIDTH = 132
      DO 200 I=1,NFOUND
         IF(IOPTI.NE.0) THEN
            J = JSORT(I)
         ELSE
            J = I
         ENDIF
         GENAM  = FILES(J)
         IF(IDEBFA.GE.3) PRINT *,'FMLS. GENAM = ',GENAM
         IF(IOPTX.NE.0) THEN
            IF(IDEBFA.EQ.3) PRINT *,'FMLS. GENAM/OLDNAM = ',
     +         GENAM,' / ',OLDNAM
            IF(GENAM.EQ.OLDNAM) GOTO 200
         ENDIF
         LFILE  = LENOCC(GENAM)
         ISLASH = INDEXB(OLDNAM(1:LOLD),'/')
         JSLASH = INDEXB(GENAM(1:LFILE),'/')
         CHFILE = GENAM(JSLASH+1:LFILE)
         LF     = LENOCC(CHFILE)
         CALL FMATCH(CHFILE(1:LF),MATCH(1:LM),IMAT)
         IF(IMAT.NE.0) GOTO 200
*
*     Check that keys match those selected
*     Location code:
*
        IF(NUMLOC.GT.0) THEN
           IF(IUCOMP(KEYS(MKLCFA,J),MFMLOC,NUMLOC).EQ.0) THEN
           IF(IDEBFA.GE.3) PRINT *,'FMLS. candidate # ',J,
     +        ' fails location code check'
           GOTO 200
           ENDIF
        ENDIF
*
*     Copy level:
*
        IF(NUMCPL.GT.0) THEN
           IF(IUCOMP(KEYS(MKCLFA,J),MFMCPL,NUMCPL).EQ.0) THEN
           IF(IDEBFA.GE.3) PRINT *,'FMLS. candidate # ',J,
     +        ' fails copy level check'
           GOTO 200
           ENDIF
        ENDIF
*
*     Media type:
*
        IF(NUMMTP.GT.0) THEN
           IF(IUCOMP(KEYS(MKMTFA,J),MFMMTP,NUMMTP).EQ.0) THEN
           IF(IDEBFA.GE.3) PRINT *,'FMLS. candidate # ',J,
     +        ' fails media type check'
           GOTO 200
           ENDIF
        ENDIF
 
 
         NFILES = NFILES + 1
         LBANK=0
         IF(IOPTX.NE.0) THEN
*
*     Option 1 - display each entry only once
*     Display entry chosen by current selection unless not found
*
            CALL UCOPY(KEYS(1,J),KEYSAV,10)
            KEYS(1,J) = 0
            CALL FMGET(GENAM,LBANK,KEYS(1,J),IC)
            IF(IC.NE.0) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMLS. ** Warning. ** ',
     +         'No match with current selection criteria for ',
     +         CHFILE(1:LF)
               CALL UCOPY(KEYSAV,KEYS(1,J),10)
               ENDIF
            LBANK=0
         ENDIF
         IF((IOPTW.NE.0).OR.(IOPTV.NE.0)) THEN
         IF(OLDNAM(1:JSLASH-1).NE.GENAM(1:ISLASH-1)) THEN
            IF(IWIDTH.GT.0) THEN
            WRITE(LWRITE,'(1X,A)') CARD(1:IWIDTH)
            IWIDTH = 0
            ENDIF
            WRITE(LWRITE,*)
            WRITE(LWRITE,8001) GENAM(1:JSLASH-1)
            WRITE(LWRITE,*)
8001     FORMAT(' Directory :',A)
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
            CALL FMPKEY(KEYS(1,J),10)
            CALL FMSHOW(GENAM,LBANK,KEYS(1,J),CHOPT,IRC)
            CALL MZDROP(IXSTOR,LBANK,' ')
            LBANK = 0
         ENDIF
         OLDNAM = GENAM
         LOLD   = LFILE
200   CONTINUE
      IF((IOPTW.NE.0).OR.(IOPTV.NE.0))
     +    WRITE(LWRITE,'(1X,A)') CARD(1:IWIDTH)
 
      IF(JCONT.NE.0) GOTO 1
 
      IRC = NFILES
      CALL FACDIR(PREDIR,' ')
      RETURN
      END
 
