*
* $Id: fals.F,v 1.1.1.1 1996/03/07 15:18:06 mclareni Exp $
*
* $Log: fals.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:06  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FALS(PATH,IRC)
      CHARACTER*(*) PATH
      CHARACTER*255 CHPATH
      PARAMETER (MAXFIL=1000)
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA,MAXFIL),JSORT(MAXFIL)
      DIMENSION KEYSAV(LKEYFA)
      CHARACTER*255 FILES(MAXFIL),GENAM,OLDNAM
      COMMON/FALSC/KEYS,JSORT,FILES
      CHARACTER*255 FNAME
      CHARACTER*36  CHOPT
      INTEGER       FMISET
#include "fatmen/fatpara.inc"
#include "fatmen/fatloc.inc"
#include "fatmen/fatmtp.inc"
#include "fatmen/fatcpl.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatout.inc"
#include "fatmen/fatsho.inc"
#include "fatmen/fatbank.inc"
      CHARACTER*132 CARD
#include "fatmen/falscm.inc"
#include "fatmen/fatinit.inc"
*
* Look for some files and show the bank and key content:
*
      NDIRT  = NDIRT + 1
      NFILES = 0
      IRC    = 0
      ITIME  = 0
      LPATH  = LENOCC(PATH)
      LCHF   = LENOCC(CHFILE)
      CHPATH = PATH(1:LPATH)
 
      IF(IDEBFA.GE.2) PRINT *,'FALS. enter for ',PATH(1:LPATH),
     +   ' match = ',CHFILE(1:LCHF)
*
*     Reconstruct character option
*
      LCH    = 0
      CHOPT  = ' '
 
      DO 10 J=1,36
         IF(IOPT(J).NE.0) THEN
            LCH = LCH + 1
            CHOPT(LCH:LCH) = ALFNUM(J:J)
         ENDIF
   10 CONTINUE
*
*     Do we need to call FMSHOW?
*
      IF(LCH.EQ.0) THEN
         LCH   = 1
         ISHOW = 0
      ELSE
         ISHOW = FMISET(CHOPT(1:LCH),'DGK')
      ENDIF
 
      JSHOW    = 0
      ICONT    = 0
      JCONT    = 0
   20 CONTINUE
*
*     Only process one directory at a time, otherwise use FMLFIL
*
      ITIME = ITIME + 1
      CALL FMFILS(CHPATH(1:LPATH)//'/'//CHFILE(1:LCHF),
     +            FILES,KEYS,NFOUND,MAXFIL,JCONT,IRC)
      IF(IRC.EQ.-1) THEN
         JCONT = 1
         IF(ITIME.EQ.1.AND.IDEBFA.GE.0.AND.IOPTI+IOPTX.GT.0) THEN
            PRINT 9001,MAXFIL
            PRINT 9002,MAXFIL
 9001 FORMAT(' FALS. directory contains more than ',I5,' entries.')
 9002 FORMAT(' FALS. files will be processed in batches of ',I5,
     +       ' (for options I/X etc.)')
            ICONT = 1
         ENDIF
      ELSE
         JCONT = 0
      ENDIF
 
      OLDNAM = ' '
      LOLD   = 1
 
      IF(NFOUND.GT.0.AND.(IOPTI.NE.0.OR.IOPTX.NE.0)) THEN
         IF(IDEBFA.EQ.3) PRINT *,'FALS. sorting file names...'
         CALL FMSORT(FILES,KEYS,NFOUND,JSORT,IC)
      ENDIF
      IWIDTH = 0
      JWIDTH = 78
      IF(IOPTV.NE.0) JWIDTH = 132
      DO 30  I=1,NFOUND
         IF((IOPTI.NE.0).OR.(IOPTX.NE.0)) THEN
            J = JSORT(I)
         ELSE
            J = I
         ENDIF
         GENAM  = FILES(J)
         LFILE  = LENOCC(GENAM)
         IF(IDEBFA.GE.3) PRINT *,'FALS. GENAM  = ',GENAM(1:LFILE)
         IF(IDEBFA.GE.3) PRINT *,'FALS. OLDNAM = ',OLDNAM(1:LOLD)
         IF(IOPTX.NE.0) THEN
            IF(GENAM.EQ.OLDNAM) GOTO 30
         ENDIF
         ISLASH = INDEXB(OLDNAM(1:LOLD),'/')
         JSLASH = INDEXB(GENAM(1:LFILE),'/')
         FNAME = GENAM(JSLASH+1:LFILE)
         LF     = LENOCC(FNAME)
*
*     Fast check: CHFILE = '*'
*
         IF(CHFILE(1:LCHF).EQ.'*') THEN
            IMAT = 0
         ELSE
            CALL FMATCH(FNAME(1:LF),CHFILE(1:LCHF),IMAT)
         ENDIF
         IF(IMAT.NE.0) GOTO 30
*
*     Check that keys match those selected
*     Location code:
*
         IF(NUMLOC.GT.0) THEN
            IF(IUCOMP(KEYS(MKLCFA,J),MFMLOC,NUMLOC).EQ.0) THEN
               IF(IDEBFA.GE.3) PRINT *,'FALS. candidate # ',J, ' fails '
     +         //'location code check'
               GOTO 30
            ENDIF
         ENDIF
*
*     Copy level:
*
         IF(NUMCPL.GT.0) THEN
            IF(IUCOMP(KEYS(MKCLFA,J),MFMCPL,NUMCPL).EQ.0) THEN
               IF(IDEBFA.GE.3) PRINT *,'FALS. candidate # ',J, ' fails '
     +         //'copy level check'
               GOTO 30
            ENDIF
         ENDIF
*
*     Media type:
*
         IF(NUMMTP.GT.0) THEN
            IF(IUCOMP(KEYS(MKMTFA,J),MFMMTP,NUMMTP).EQ.0) THEN
               IF(IDEBFA.GE.3) PRINT *,'FALS. candidate # ',J, ' fails '
     +         //'media type check'
               GOTO 30
            ENDIF
         ENDIF
 
         NFILES = NFILES + 1
         LBANK=0
         IF(IOPTX.NE.0) THEN
*
*     Option X - display each entry only once
*     Display entry chosen by current selection unless not found
*
            CALL UCOPY(KEYS(1,J),KEYSAV,10)
            CALL FMGET(GENAM,LBANK,KEYS(1,J),IC)
            IF(IC.NE.0) THEN
               IF(IDEBFA.GE.0) PRINT *,'FALS. ** Warning. ** ',
     +         'No match with current selection criteria for ',
     +         FNAME(1:LF)
               CALL UCOPY(KEYSAV,KEYS(1,J),10)
            ENDIF
            LBANK=0
         ENDIF
         IF(IOPTW.NE.0.OR.IOPTV.NE.0) THEN
            IF(OLDNAM(1:JSLASH-1).NE.GENAM(1:ISLASH-1)) THEN
               IF(IWIDTH.GT.0) THEN
                  WRITE(LWRITE,'(1X,A)') CARD(1:IWIDTH)
                  IWIDTH = 0
               ENDIF
               IF(ICONT.EQ.0) THEN
                  IF(ITIME.EQ.1) THEN
                     WRITE(LWRITE,*)
                     WRITE(LWRITE,9003) GENAM(1:JSLASH-1)
                     WRITE(LWRITE,*)
                  ENDIF
               ELSE
                  WRITE(LWRITE,*)
                  WRITE(LWRITE,9004) GENAM(1:JSLASH-1),ITIME
                  WRITE(LWRITE,*)
               ENDIF
 9003    FORMAT(' Directory :',A)
 9004    FORMAT(' Directory :',A,' Batch :',I3)
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
               CARD = FNAME(1:LF) // ' '
            ELSE
               CARD = CARD(1:IWIDTH) // FNAME(1:LF) // ' '
            ENDIF
            IWIDTH = IWIDTH + LF + 1
         ELSEIF(ISHOW.EQ.0) THEN
            IF(IOPTG.NE.0) THEN
               WRITE(LWRITE,9005) GENAM(1:LFILE)
 9005 FORMAT(' Generic filename: ',A)
            ELSEIF(IOPTD.NE.0) THEN
               WRITE(LWRITE,9006) GENAM(1:LFILE),KEYS(1,J)
 9006    FORMAT(' rm ',A,1X,I10)
            ELSEIF(IOPTK.NE.0) THEN
               WRITE(LWRITE,9007) (KEYS(K,J),K=1,LKEYFA-1)
 9007 FORMAT(' Key serial number = ',I6,' filename = ',5A4,
     +' data repr. = ',I3,' media type = ',I2,' location code = ',I6)
            ELSE
               WRITE(LWRITE,9008) FNAME(1:LF)
 9008 FORMAT(' ',A)
            ENDIF
 
         ELSE
 
            IF(IOPTB+IOPTE+IOPTG+JSHOW.EQ.0) THEN
               WRITE(LWRITE,*)
               WRITE(LWRITE,*) 'Directory: ',CHPATH(1:LPATH)
               WRITE(LWRITE,*)
            ENDIF
 
            JSHOW = 1
            CALL FMSHOW(GENAM,LBANK,KEYS(1,J),CHOPT,IRC)
            CALL MZDROP(IXSTOR,LBANK,' ')
            LBANK = 0
         ENDIF
         OLDNAM = GENAM
         LOLD   = LFILE
   30 CONTINUE
      IF((IOPTW.NE.0.OR.IOPTV.NE.0).AND.IWIDTH.GT.0)
     +       WRITE(LWRITE,'(1X,A)') CARD(1:IWIDTH)
 
      IF(JCONT.NE.0) GOTO 20
 
      IF (IDEBFA.GE.-2) THEN
         IF(JSHOW.NE.0) THEN
            WRITE(LWRITE,*)
            WRITE(LWRITE,9009) NFILES
 9009       FORMAT(' Files:',I5)
         ELSE
            IF(IDEBFA.GE.1) THEN
               WRITE(LWRITE,*)
               WRITE(LWRITE,*) 'No files found in directory: ',
     +            CHPATH(1:LPATH)
            ENDIF
         ENDIF
      ENDIF
 
      NFILT = NFILT + NFILES
 
      END
