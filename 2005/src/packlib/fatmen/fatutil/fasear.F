*
* $Id: fasear.F,v 1.1.1.1 1996/03/07 15:18:07 mclareni Exp $
*
* $Log: fasear.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:07  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FASEAR(PATH,KEYS,IRC)
      CHARACTER*(*) PATH
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA)
      CHARACTER*255 OLDNAM
      CHARACTER*20  FNAME
#include "fatmen/facard.inc"
#include "fatmen/fatsea.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatloc.inc"
#include "fatmen/fatmtp.inc"
#include "fatmen/fatcpl.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatout.inc"
      INTEGER       FMISET
      CHARACTER*36  CHOPT
#include "fatmen/fatbank.inc"
      SAVE     JFILES
#include "fatmen/fasecm.inc"
*
      LPATH  = LENOCC(PATH)
      ISLASH = INDEXB(PATH(1:LPATH),'/')
      FNAME  = PATH(ISLASH+1:LPATH)
      LF     = LPATH - ISLASH
      IRC    = 0
 
      IF(NDIRS.EQ.0) THEN
         JSLASH = 1
         IWIDTH = 0
      ENDIF
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
*
*     Now check for selection on user words
*
      IUWORD = 0
      DO 20 I=1,10
         IF(IUSER(1,I).EQ.-1) GOTO 20
         IUWORD = I
         GOTO 30
   20 CONTINUE
   30 CONTINUE
 
      JWIDTH = 78
      IF(IOPTV.NE.0) JWIDTH = 132
*
*     Look for some files and show the bank and key content:
*
      IF(IDEBFA.GE.2) PRINT *,'FASEAR. enter for ',PATH(1:LPATH),
     +                CHOPT
*
*     Do we have to search the catalogue entry?
*
      LBANK  = 0
      IDTIME = 0
      IF(IDCATA.GE.0.OR.IDLAST.GE.0.OR.IDCREA.GE.0) IDTIME = 1
      IF(LDSN+LHOST+LVID+LUSER+LFORM+LCOMM+IUWORD+IDTIME.GT.0) THEN
*
*     Now check if this entry matches
*
         CALL FMSEAR(PATH,LBANK,KEYS,DSN,HOST,VID, USER,UFORM,COMM,
     +   IRET)
         IF(IRET.NE.0) THEN
            CALL MZDROP(IXSTOR,LBANK,' ')
            LBANK = 0
            GOTO 40
         ENDIF
 
      ENDIF
 
      IF(OLDNAM(1:JSLASH-1).NE.PATH(1:ISLASH-1)) THEN
         IF(NDIRS.GT.0) THEN
            IF(IOPTW.NE.0.OR.IOPTV.NE.0) THEN
               WRITE(LWRITE,'(1X,A)') CARD(1:IWIDTH)
               IWIDTH = 0
            ENDIF
            IF(IOPTD.NE.0) THEN
               WRITE(LPRTFA,*) JFILES,' candidates for deletion'
            ELSE
               IF(IDEBFA.GE.0) THEN
 
                  WRITE(LWRITE,'(A)') ' '
                  IF(JFILES .EQ. 0) THEN
                     WRITE(LWRITE,9001)
 9001          FORMAT(' No matches found')
                  ELSE
                     WRITE(LWRITE,9002) JFILES
 9002          FORMAT(' Matches:',I5)
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         IF(IDEBFA.GE.0) THEN
            PRINT *
            PRINT *,'Searching directory: ', PATH(1:ISLASH-1)
            PRINT *
         ENDIF
         IF(IOPTW.NE.0.OR.IOPTV.NE.0) THEN
            WRITE(LWRITE,*)
            WRITE(LWRITE,9003) PATH(1:ISLASH-1)
            WRITE(LWRITE,*)
 9003    FORMAT(' Directory :',A)
         ELSEIF(LWRITE.NE.LPRTFA.AND.IOPTD.EQ.0) THEN
            WRITE(LWRITE,*) 'Directory: ',PATH(1:ISLASH-1)
         ENDIF
         OLDNAM = PATH(1:ISLASH)
         JSLASH = ISLASH
         NDIRS  = NDIRS + 1
         JFILES = 0
      ENDIF
 
      NFILES = NFILES + 1
      JFILES = JFILES + 1
 
      IF(IOPTW.NE.0.OR.IOPTV.NE.0) THEN
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
            WRITE(LWRITE,9004) PATH(1:LPATH)
 9004 FORMAT(' Generic filename: ',A)
         ELSEIF(IOPTD.NE.0) THEN
            WRITE(LWRITE,9005) PATH(1:LPATH),KEYS(1),CHSTR(1:LSTR)
 9005    FORMAT(' rm ',A,1X,I10,1X,A)
         ELSEIF(IOPTK.NE.0) THEN
            WRITE(LWRITE,9006) (KEYS(I),I=1,LKEYFA-1)
 9006 FORMAT(' Key serial number = ',I6,' filename = ',5A4,
     +' data repr. = ',I3,' media type = ',I2,' location code = ',I6)
         ELSE
            WRITE(LWRITE,9007) FNAME(1:LF)
 9007 FORMAT(' ',A)
         ENDIF
      ELSE
         CALL FMSHOW(PATH,LBANK,KEYS,CHOPT(1:LCH),IRC)
         CALL MZDROP(IXSTOR,LBANK,' ')
         LBANK = 0
      ENDIF
      LOLD = LF
      IF((NFILES.EQ.NMATCH).AND.(NMATCH.NE.0)) THEN
         IF(IOPTW.NE.0.OR.IOPTV.NE.0)
     +      WRITE(LWRITE,'(1X,A)') CARD(1:IWIDTH)
         IF(IDEBFA.GE.-3) PRINT *,'FASEAR. stopping after ', NFILES,' '
     +   //'matches'
         IRC = NFILES
         RETURN
      ENDIF
   40 CONTINUE
 
      END
 
