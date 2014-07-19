*
* $Id: faseac.F,v 1.1.1.1 1996/03/07 15:18:06 mclareni Exp $
*
* $Log: faseac.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:06  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FASEAC(PATH,IRC)
      CHARACTER*(*) PATH
      CHARACTER*255 CHPATH
      PARAMETER (MAXFIL=1000)
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA,MAXFIL),JSORT(MAXFIL)
      DIMENSION KEYSAV(LKEYFA)
      CHARACTER*255 FILES,GENAM,OLDNAM,
     +              CWD,OLDWD
      CHARACTER*20  FNAME
#include "fatmen/fatsea.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatloc.inc"
#include "fatmen/fatmtp.inc"
#include "fatmen/fatcpl.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatout.inc"
      CHARACTER*36  CHOPT
#include "fatmen/fatbank.inc"
      COMMON /MYWKSP/ FILES(MAXFIL)
      CHARACTER*132 CARD
      INTEGER       FMISET
#include "fatmen/fasecm.inc"
*
      LPATH  = LENOCC(PATH)
      LF     = LENOCC(CHFILE)
      CHPATH = PATH(1:LPATH)
      OLDWD  = ' '
      LOLDWD = 1
      IRC    = 0
      JFILES = 0
      JCONT  = 0
      NDIRS  = NDIRS + 1
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
*
*     Look for some files and show the bank and key content:
*
      IF(IDEBFA.GE.2) PRINT *,'FASEAC. enter for ',PATH(1:LPATH),
     +                CHOPT
      IF(IDEBFA.GE.0)
     +         PRINT *,'Searching directory: ',PATH(1:LPATH)
      IF((IDEBFA.GE.0).AND.(LWRITE.NE.LPRTFA).AND.IOPTD.EQ.0)
     +         WRITE(LWRITE,*) 'Directory: ',PATH(1:LPATH)
   40 CONTINUE
      CALL FMFILS(CHPATH(1:LPATH)//'/'//CHFILE(1:LF),
     +            FILES,KEYS,NFOUND,MAXFIL,JCONT,IRC)
      IF(IDEBFA.GE.2) PRINT *,'FASEAC. found ',NFOUND,
     +   ' files in FMFILS'
      IF(IRC.EQ.-1) THEN
         JCONT = 1
      ELSE
         JCONT = 0
      ENDIF
 
      OLDNAM = ' '
      LOLD   = 1
      IF(IOPTI.NE.0) THEN
         IF(IDEBFA.EQ.3) PRINT *,'FASEAC. sorting file names...'
         CALL FMSORT(FILES,KEYS,NFOUND,JSORT,IC)
      ENDIF
      IWIDTH = 0
      JWIDTH = 78
      IF(IOPTV.NE.0) JWIDTH = 132
      DO 50  I=1,NFOUND
         IF(IOPTI.NE.0) THEN
            J = JSORT(I)
         ELSE
            J = I
         ENDIF
         GENAM  = FILES(J)
         LFILE  = LENOCC(GENAM)
         IF(IOPTX.NE.0) THEN
            IF(IDEBFA.GE.3) PRINT *,'FASEAC. GENAM  = ',GENAM(1:LFILE)
            IF(IDEBFA.GE.3) PRINT *,'FASEAC. OLDNAM = ',OLDNAM(1:LOLD)
            IF(GENAM.EQ.OLDNAM) GOTO 50
         ENDIF
         ISLASH = INDEXB(OLDNAM(1:LOLD),'/')
         JSLASH = INDEXB(GENAM(1:LFILE),'/')
         FNAME  = GENAM(JSLASH+1:LFILE)
         LFNAME = LENOCC(FNAME)
 
         CALL FMATCH(FNAME(1:LFNAME),CHFILE(1:LF),IMAT)
         IF(IMAT.NE.0) GOTO 50
*
*     Check that keys match those selected
*     Location code:
*
         IF(NUMLOC.GT.0) THEN
            IF(IUCOMP(KEYS(MKLCFA,J),MFMLOC,NUMLOC).EQ.0) THEN
               IF(IDEBFA.GE.3) PRINT *,'FASEAC. candidate # ',J,
     +         ' fails location code check'
               GOTO 50
            ENDIF
         ENDIF
*
*     Copy level:
*
         IF(NUMCPL.GT.0) THEN
            IF(IUCOMP(KEYS(MKCLFA,J),MFMCPL,NUMCPL).EQ.0) THEN
               IF(IDEBFA.GE.3) PRINT *,'FASEAC. candidate # ',J,
     +         ' fails copy level check'
               GOTO 50
            ENDIF
         ENDIF
*
*     Media type:
*
         IF(NUMMTP.GT.0) THEN
            IF(IUCOMP(KEYS(MKMTFA,J),MFMMTP,NUMMTP).EQ.0) THEN
               IF(IDEBFA.GE.3) PRINT *,'FASEAC. candidate # ',J,
     +         ' fails media type check'
               GOTO 50
            ENDIF
         ENDIF
 
         LBANK=0
         IF(IOPTX.NE.0) THEN
*
*     Option X - display each entry only once
*     Display entry chosen by current selection unless not found
*
            CALL UCOPY(KEYS(1,J),KEYSAV,10)
            CALL FMGET(GENAM,LBANK,KEYS(1,J),IC)
            IF(IC.NE.0) THEN
               IF(IDEBFA.GE.0) PRINT *,'FASEAC. ** Warning. ** ',
     +         'No match with current selection criteria for ',
     +         FNAME(1:LFNAME)
               CALL UCOPY(KEYSAV,KEYS(1,J),10)
            ENDIF
            LBANK=0
         ENDIF
*     Do we have to search the catalogue entry?
*
         LBANK  = 0
         IDTIME = 0
         IF(IDCATA.GE.0.OR.IDCREA.GE.0.OR.IDLAST.GE.0) IDTIME = 1
         IF(LDSN+LHOST+LVID+LUSER+LFORM+LCOMM+IUWORD+IDTIME.GT.0) THEN
*
*     Now check if this entry matches
*
            CALL FMSEAR(GENAM,LBANK,KEYS(1,J),DSN,HOST,VID, USER,UFORM,
     +      COMM,IRET)
            IF(IRET.NE.0) THEN
               CALL MZDROP(IXSTOR,LBANK,' ')
               LBANK = 0
               GOTO 50
            ENDIF
         ENDIF
         NFILES = NFILES + 1
         JFILES = JFILES + 1
 
         IF(IOPTW.NE.0.OR.IOPTV.NE.0) THEN
            IF(OLDNAM(1:JSLASH-1).NE.GENAM(1:ISLASH-1)) THEN
               IF(IWIDTH.GT.0) THEN
                  WRITE(LWRITE,'(1X,A)') CARD(1:IWIDTH)
                  IWIDTH = 0
               ENDIF
               WRITE(LWRITE,*)
               WRITE(LWRITE,9001) GENAM(1:JSLASH-1)
               WRITE(LWRITE,*)
 9001    FORMAT(' Directory :',A)
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
               CARD = FNAME(1:LFNAME) // ' '
            ELSE
               CARD = CARD(1:IWIDTH) // FNAME(1:LFNAME) // ' '
            ENDIF
            IWIDTH = IWIDTH + LF + 1
         ELSEIF(ISHOW.EQ.0) THEN
            IF(IOPTG.NE.0) THEN
               WRITE(LWRITE,9002) GENAM(1:LFILE)
 9002 FORMAT(' Generic filename: ',A)
            ELSEIF(IOPTD.NE.0) THEN
               WRITE(LWRITE,9003) GENAM(1:LFILE),KEYS(1,J)
 9003    FORMAT(' rm ',A,1X,I10)
            ELSEIF(IOPTK.NE.0) THEN
               WRITE(LWRITE,9004) (KEYS(K,J),K=1,LKEYFA-1)
 9004 FORMAT(' Key serial number = ',I6,' filename = ',5A4,
     +' data repr. = ',I3,' media type = ',I2,' location code = ',I6)
            ELSE
               WRITE(LWRITE,9005) FNAME(1:LFNAME)
 9005 FORMAT(' ',A)
            ENDIF
         ELSE
            CALL FMSHOW(GENAM,LBANK,KEYS(1,J),CHOPT(1:LCH),IRC)
            CALL MZDROP(IXSTOR,LBANK,' ')
            LBANK = 0
         ENDIF
         OLDNAM = GENAM
         LOLD   = LFILE
         IF((NFILES.EQ.NMATCH).AND.(NMATCH.NE.0)) THEN
            IF(IOPTW.NE.0.OR.IOPTV.NE.0)
     +                       WRITE(LWRITE,'(1X,A)') CARD(1:IWIDTH)
            IF(IDEBFA.GE.-3) PRINT *,'FASEAC. stopping after ',
     +         NFILES,' matches'
            IRC = NFILES
            GOTO 60
         ENDIF
   50 CONTINUE
      IF((IOPTW.NE.0.OR.IOPTV.NE.0).AND.IWIDTH.GT.0)
     +   WRITE(LWRITE,'(1X,A)') CARD(1:IWIDTH)
 
      IF(JCONT.NE.0) GOTO 40
 
   60 CONTINUE
 
      IF(IOPTD.NE.0) THEN
         WRITE(LPRTFA,*) JFILES,' candidates for deletion'
      ELSE
         IF(IDEBFA.GE.0) THEN
 
            WRITE(LWRITE,'(A)') ' '
            IF(JFILES .EQ. 0) THEN
               WRITE(LWRITE,9006)
 9006          FORMAT(' No matches found')
            ELSE
               WRITE(LWRITE,9007) JFILES
 9007          FORMAT(' Matches:',I5)
            ENDIF
         ENDIF
      ENDIF
 
      END
