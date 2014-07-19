*
* $Id: famodu.F,v 1.1.1.1 1996/03/07 15:18:06 mclareni Exp $
*
* $Log: famodu.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:06  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FAMODU(PATH,IRC)
      CHARACTER*(*) PATH
      CHARACTER*255 CHPATH
      PARAMETER (MAXFIL=100)
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA,MAXFIL)
      CHARACTER*255 FILES(MAXFIL),GENAM
      CHARACTER*20  FNAME
#include "fatmen/fatpara.inc"
#include "fatmen/fatloc.inc"
#include "fatmen/fatmtp.inc"
#include "fatmen/fatcpl.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatout.inc"
#include "fatmen/fatsho.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/famucm.inc"
#include "fatmen/fatinit.inc"
*
      IRC    = 0
      LPATH  = LENOCC(PATH)
      LF     = LENOCC(CHFILE)
      CHPATH = PATH(1:LPATH)
 
      IF(IDEBFA.GE.2) PRINT *,'FAMODU. enter for ',PATH(1:LPATH),
     +   ' match = ',CHFILE(1:LF)
 
      JCONT    = 0
 1    CONTINUE
*
      CALL FMFILS(CHPATH(1:LPATH)//'/'//CHFILE(1:LF),
     +            FILES,KEYS,NFOUND,MAXFIL,JCONT,IRC)
      IF(IRC.EQ.-1) THEN
         JCONT = 1
      ELSE
         JCONT = 0
      ENDIF
 
      DO 200 I=1,NFOUND
         GENAM  = FILES(I)
         LFILE  = LENOCC(GENAM)
         JSLASH = INDEXB(GENAM(1:LFILE),'/')
         FNAME = GENAM(JSLASH+1:LFILE)
         LF     = LENOCC(FNAME)
         CALL FMATCH(FNAME(1:LF),CHFILE(1:LF),IMAT)
         IF(IMAT.NE.0) GOTO 200
*
*     Check that keys match those selected
*     Location code:
*
        IF(NUMLOC.GT.0) THEN
           IF(IUCOMP(KEYS(MKLCFA,I),MFMLOC,NUMLOC).EQ.0) THEN
           IF(IDEBFA.GE.3) PRINT *,'FAMODU. candidate # ',I,
     +        ' fails location code check'
           GOTO 200
           ENDIF
        ENDIF
*
*     Copy level:
*
        IF(NUMCPL.GT.0) THEN
           IF(IUCOMP(KEYS(MKCLFA,I),MFMCPL,NUMCPL).EQ.0) THEN
           IF(IDEBFA.GE.3) PRINT *,'FAMODU. candidate # ',I,
     +        ' fails copy level check'
           GOTO 200
           ENDIF
        ENDIF
*
*     Media type:
*
        IF(NUMMTP.GT.0) THEN
           IF(IUCOMP(KEYS(MKMTFA,I),MFMMTP,NUMMTP).EQ.0) THEN
           IF(IDEBFA.GE.3) PRINT *,'FAMODU. candidate # ',I,
     +        ' fails media type check'
           GOTO 200
           ENDIF
        ENDIF
 
         LBANK=0
         CALL FMGETK(GENAM,LBANK,KEYS(1,I),IRC)
         IF(IRC.EQ.0) THEN
            IF(IOPTF.NE.0) CALL FMPUTC(LBANK,CHUFRM,MFUTFA,NFUTFA,IRC)
            IF(IOPTW.NE.0) CALL FMPUTV(LBANK,IWORDS,MUSWFA,NUSWFA,IRC)
            IF(IOPTC.NE.0) CALL FMPUTC(LBANK,CHUCOM,MUSCFA,NUSCFA,IRC)
            CALL FMMOD(GENAM,LBANK,0,IRC)
            CALL MZDROP(IXSTOR,LBANK,' ')
            LBANK = 0
         ENDIF
  200 CONTINUE
 
      RETURN
      END
