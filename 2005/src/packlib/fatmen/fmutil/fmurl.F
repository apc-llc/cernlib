*
* $Id: fmurl.F,v 1.1.1.1 1996/03/07 15:18:20 mclareni Exp $
*
* $Log: fmurl.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:20  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMURL(GENAM,LBANK,KEYS,
     +                 CHPROT,CHSERV,CHNAME,CHSGRP,CHBFID,CHOPT,IRC)
 
      CHARACTER*(*)    GENAM,CHPROT,CHSERV,CHNAME,CHSGRP,CHBFID,CHOPT
*
*     Generic name <-> host/serv/grp conversion
*
*     CHOPT: G (default) get attributes from bank into arg-list
*            P           put data from arg-list into bank
*                        FMBOOK should be called first to create empty bank
*
*            fields that are missing (zero length) are taken from
*            configuration file
*
      PARAMETER     (NPOSS=4)
      CHARACTER*8    CHPOSS(NPOSS)
 
      CHARACTER*255 GNAME,CHURL
      CHARACTER*64  STRGRP,STNAME,STSERV
#include "fatmen/fmnkeys.inc"
      DIMENSION      KEYS(LKEYFA),KEYL(LKEYFA)
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatoptd.inc"
      SAVE          CHPOSS
 
      DATA          CHPOSS(1)/'OSM'/,
     +              CHPOSS(2)/'UNITREE'/,
     +              CHPOSS(3)/'ADSM'/,
     +              CHPOSS(4)/'EMASS'/
 
#include "fatmen/fatoptc.inc"
 
      IRC = 0
 
      NCH = LENOCC(GENAM)
      IF(NCH.EQ.0) THEN
         IF(IDEBFA.GE.-3) WRITE(LPRTFA,9001)
 9001 FORMAT(' FMURL. illegal/missing generic name.')
         IRC = 1
         GOTO 999
      ENDIF
 
      IF(IOPTG.EQ.0.AND.IOPTP.EQ.0) IOPTG = 1
 
      IF(IOPTG.NE.0) THEN
         IF (LBANK.EQ.0) THEN
            CALL FMGET(GENAM,LBANK,KEYS,IRC)
            IF (IRC.NE.0) GOTO 999
         ELSE
*
*     Update keys from bank
*
            CALL FMUPKY(GENAM,LBANK,KEYS,IRC)
         ENDIF
*
*     Is this entry a link?
*
         IF(KEYS(MKLCFA).EQ.0) THEN
            CALL UHTOC(IQ(LBANK+KOFUFA+MFQNFA),4,GNAME,NFQNFA)
            LGN = LENOCC(GNAME)
            IF(IDEBFA.GE.0) WRITE(LPRTFA,9002) GENAM(1:NCH),GNAME(1:
     +      LGN)
 9002 FORMAT(' FMURL. ',A,' --> ',A)
            CALL VZERO(KEYL,LKEYFA)
            CALL FMGET(GENAM,LBANK,KEYL,IRC)
            IF (IRC.NE.0) GOTO 999
         ELSE
            IF(KEYS(MKMTFA).NE.1) THEN
               WRITE(LPRTFA,9003) KEYS(MKMTFA)
 9003 FORMAT(' FMURL. invalid media type ',I10,
     +       ' - only type 1 valid for URLs')
            IRC = 2
            GOTO 999
            ENDIF
         ENDIF
 
         CALL UHTOC(IQ(LBANK+KOFUFA+MFQNFA),4,CHURL,NFQNFA)
         LURL = LENOCC(CHURL)
*
*     Split URL into component pieces
*           protocol://server-node/store-name/storage-class/bfid
*
         LSLASH = INDEXB(CHURL(1:LURL),'/')
         IF(LSLASH.EQ.0) GOTO 10
*
*     BitfileID
*
         CHBFID = CHURL(LSLASH+1:LURL)
         LURL   = LSLASH-1
 
         LSLASH = INDEXB(CHURL(1:LURL),'/')
         IF(LSLASH.EQ.0) GOTO 10
*
*     Storage-class
*
         CHSGRP = CHURL(LSLASH+1:LURL)
         LURL   = LSLASH-1
 
         LSLASH = INDEXB(CHURL(1:LURL),'/')
         IF(LSLASH.EQ.0) GOTO 10
*
*     Store-name
*
         CHNAME  = CHURL(LSLASH+1:LURL)
         LURL    = LSLASH-1
 
         LSLASH = INDEXB(CHURL(1:LURL),'/')
         IF(LSLASH.EQ.0) GOTO 10
*
*     Server-node
*
         CHSERV  = CHURL(LSLASH+1:LURL)
         LURL    = LSLASH-1
 
         IF(INDEX (CHURL(1:LURL),'/').NE.
     +      INDEXB(CHURL(1:LURL),'/')) GOTO 10
 
         GOTO 999
   10 CONTINUE
         WRITE(LPRTFA,9004)
 9004 FORMAT(' FMURL. error interpreting URL: ',A)
         IRC = 3
         GOTO 999
 
      ELSEIF(IOPTP.NE.0) THEN
 
         IF(LBANK.EQ.0) THEN
            IF(IDEBFA.GE.-3) WRITE(LPRTFA,9005) CHOPT
 9005 FORMAT(' FMURL. Error - bank address must be given for option ',A)
            IRC = 98
            GOTO 999
         ENDIF
         LPROT = LENOCC(CHPROT)
         LSERV = LENOCC(CHSERV)
         LNAME = LENOCC(CHNAME)
         LSGRP = LENOCC(CHSGRP)
 
         IF(LPROT.EQ.0) THEN
            IF(IDEBFA.GE.-3) THEN
               WRITE(LPRTFA,9006)
 9006    FORMAT(' FMURL. error - protocol must be specified.')
               WRITE(LPRTFA,9007) CHPOSS
 9007    FORMAT(' FMURL. following protocols currently permitted: ',
     +          /,(1X,A))
            ENDIF
            IRC = 4
            GOTO 999
         ELSE
 
            JPROT = ICNTHU(CHPROT(1:LPROT),CHPOSS,NPOSS)
            IF(JPROT.EQ.0) THEN
               IF(IDEBFA.GE.-3) THEN
                  WRITE(LPRTFA,9008)
 9008    FORMAT(' FMURL. error - invalid protocol specified - ',A)
                  WRITE(LPRTFA,9007)
               ENDIF
               IRC = 5
               GOTO 999
            ENDIF
*
*     Call FMCLASS and override with information from arg-list
*
*           CALL FMCLASS(GNAME,STRGRP,STNAME,STSERV,CHOPT,IRC)
            IF(IRC.NE.0) GOTO 999
            LGRP = LENOCC(STRGRP)
 
            IF(LSGRP.EQ.0) CHSGRP = STRGRP(1:LENOCC(STRGRP))
            IF(LNAME.EQ.0) CHNAME = STNAME(1:LENOCC(STNAME))
            IF(LSERV.EQ.0) CHSERV = STSERV(1:LENOCC(STSERV))
*
*     now build URL and stuff it back into bank
*
            CHURL = CHPROT(1:LPROT)//'://'//CHSERV(1:LSERV)//'/'//
     +              CHNAME(1:LNAME)//'/'//CHSGRP(1:LSGRP)//'/?'
            LURL = LPROT + LSERV + LNAME + LSGRP + 7
 
            CALL FMPUTC(LBANK,CHURL(1:LURL),MFQNFA,LURL,IRC)
 
         ENDIF
 
      ENDIF
 
  999 END
