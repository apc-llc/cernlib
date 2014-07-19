*
* $Id: fmrmtc.F,v 1.2 1996/04/02 10:26:36 cernlib Exp $
*
* $Log: fmrmtc.F,v $
* Revision 1.2  1996/04/02 10:26:36  cernlib
* Split up line with "/ *" ( without this blank) to avoid start a comment
*
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMRMTC
#include "fatmen/faust.inc"
      PARAMETER (MAXFIL=2)
      PARAMETER (MAXDIR=100)
      PARAMETER (LKEYFA=10)
      PARAMETER (LZERO=0)
      CHARACTER*255 FILES(MAXFIL),PATH,CHPATH,PREDIR
      CHARACTER*255 PATH2
      CHARACTER*255 CHDIR(MAXDIR),CHTMP
#include "fatmen/fatsys.inc"
      CHARACTER*20  CHFILE,MATCH,FNAME
      CHARACTER*36  CHOPT,OPTN
      CHARACTER*3   CHSTAT
      CHARACTER*4   COMM
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
      DIMENSION     MYKEYS(LKEYFA,MAXFIL)
      DIMENSION     KEYS(LKEYFA)
      CHARACTER*4   WILD
      DATA          WILD/'%*(['/
#include "fatmen/fatinit.inc"
*
      CALL RZCDIR(PREDIR,'R')
 
      COMM = 'DDIR'
      PATH = ' '
      CALL KUGETC(PATH,LPATH)
      IX = ICFMUL(WILD,PATH,1,LPATH)
      IF(IX.LE.LPATH) THEN
         PRINT *,'FMRMTC. path must not contain wild-cards'
         RETURN
      ENDIF
 
      NFRTRE = NFRTRE + 1
 
      CALL FMFIXF(PATH,CHPATH)
      LPATH = LENOCC(CHPATH)
      CHPATH(LPATH+1:LPATH+1) = '/'
      CHPATH(LPATH+2:LPATH+2) = '*'
      LP    = LPATH + 2
*
      NFOUND = 0
      NDIRT  = 0
      ICONT  = 0
      IPASS  = 0
*
*     Any files at this level?
*
      CALL RZCDIR(PATH(1:LPATH),' ')
      IF(IQUEST(1).NE.0) THEN
         PRINT *,'FMRMTC. error setting directory to ',
     +      PATH(1:LPATH)
         RETURN
      ENDIF
 
      CALL FMKEYS(LKEYFA,MAXFIL,IFIRST,ILAST,MYKEYS,NFILES,IRET)
      NFIL = IQUEST(11)
      IF(IDEBFA.GE.2) PRINT *,'FMRMTC. ',NFIL,' files in ',
     +   PATH(1:LPATH)
 
      IF(NFIL.NE.0) THEN
         WRITE(LPRTFA,*)
         WRITE(LPRTFA,*) 'Directory: ',PATH(1:LPATH),
     +       ' contains ',NFIL,' file(s)'
         PRINT *,'FMRMTC. directory tree cannot be deleted'
         GOTO 99
      ENDIF
 
*
*     Get list of subdirectories
*
   10 CONTINUE
      CALL FMLDIR(CHPATH(1:LP),CHDIR,NDIRS,MAXDIR,ICONT,IRC)
      IF(IRC.EQ.-1) THEN
         ICONT = 1
      ELSEIF(IRC.NE.0) THEN
         PRINT *,'FMRMTC. return code ',IRC,' from FMLDIR'
         RETURN
      ELSE
         ICONT = 0
      ENDIF
 
   20 CONTINUE
 
      NDIRT = NDIRT + NDIRS
 
      DO 70 I=1,NDIRS
         LEND = LENOCC(CHDIR(I))
         IF(IPASS.EQ.1) THEN
            CALL FMVERI(CHDIR(I)(1:LEND),LZERO,KEYS,'G',IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GT.-3) PRINT *,'FMRMTC. errors detected by ',
     +            'FMVERI - update will not be processed'
               RETURN
            ENDIF
 
            IF(IDEBFA.GE.0) PRINT *,'FMRMTC. removing directory ',
     +      CHDIR(I)(1:LEND)
 
            CALL FMFZO(COMM,CHDIR(I)(1:LEND),LZERO,KEYS,IRC)
         ELSE
            IF(IDEBFA.GE.2) PRINT *,'FMRMTC. processing directory ',
     +      CHDIR(I)(1:LEND)
            CALL RZCDIR(CHDIR(I)(1:LEND),' ')
*
*     Any subdirectories are there at this level?
*
            CALL RZRDIR(1,CHTMP,NDIR)
            NDIR = IQUEST(11)
            IF(IDEBFA.GE.2) PRINT *,'FMRMTC. ',NDIR,' subdirectories'
*
*     Any files?
*
            IFIRST = 1
            ILAST = MAXFIL
            NMAT = 0
            NFIL = 0
 
   30       CONTINUE
            CALL FMKEYS(LKEYFA,MAXFIL,IFIRST,ILAST,MYKEYS,NFILES,IRET)
            NFIL = IQUEST(11)
            IF(IDEBFA.GE.2) PRINT *,'FMRMTC. ',NFIL,' files'
 
            IF(NFIL.NE.0) THEN
               WRITE(LPRTFA,*)
               WRITE(LPRTFA,*) 'Directory: ',CHDIR(I)(1:LEND), ' '
     +         //'contains ',NFIL,' file(s)'
               PRINT *,'FMRMTC. directory tree cannot be deleted'
               GOTO 99
            ENDIF
 
   60       CONTINUE
            NFOUND = NFOUND + NFIL
 
         ENDIF
   70 CONTINUE
      IF(ICONT.NE.0) GOTO 10
 
      IF(IPASS.EQ.1) THEN
*
*     Now remove top level directory
*
         CALL FMVERI(CHPATH(1:LPATH),LZERO,KEYS,'G',IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GT.-3) PRINT *,'FMRMTC. errors detected by ',
     +         'FMVERI - update will not be processed'
            RETURN
         ENDIF
         IF(IDEBFA.GE.0) PRINT *,'FMRMTC. removing directory ',
     +      CHPATH(1:LPATH)
         CALL FMFZO(COMM,CHPATH(1:LPATH),LZERO,KEYS,IRC)
         GOTO 99
      ENDIF
 
      PRINT *,'FMRMTC. directory tree empty, starting deletion pass'
      IPASS = 1
      ICONT = 0
      GOTO 10
 
   99 CONTINUE
      CALL RZCDIR(PREDIR,' ')
      RETURN
      END
