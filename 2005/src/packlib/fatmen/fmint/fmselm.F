*
* $Id: fmselm.F,v 1.3 1996/06/18 15:57:37 jamie Exp $
*
* $Log: fmselm.F,v $
* Revision 1.3  1996/06/18 15:57:37  jamie
* revert
*
* Revision 1.2  1996/06/18 15:03:13  jamie
* ensure IWANT is set
*
* Revision 1.1.1.1  1996/03/07 15:18:12  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSELM(GENAM,LBANK,KEYS,KEYM,NK,CHOPT,IRC)
*CMZ :          16/10/91  17.14.49  by  Jamie Shiers
*-- Author :    Jamie Shiers   16/10/91
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fmnkeys.inc"
      CHARACTER*(*) GENAM
      CHARACTER*8   HOST,FFORM
      CHARACTER*255 DSN,CHFILE
      PARAMETER     (MAXKEY=100)
      DIMENSION     KEYS(LKEYFA),KEYM(LKEYFA,NK)
      DIMENSION     KEYZ(LKEYFA,MAXKEY)
      INTEGER       FMNODE
#include "fatmen/fatopts.inc"
 
      LGN   = LENOCC(GENAM)
      LCH   = LENOCC(CHOPT)
      IRC   = 0
      LBANK = 0
 
      IF(IDEBFA.GE.1) PRINT *,'FMSELM. enter for generic-name ',
     +   ' / chopt = ',GENAM(1:LGN),' / ',CHOPT
 
      IF(IOPTM.NE.0.AND.IOPTR.NE.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMSELM. error - both options ',
     +      'R and M must not be set'
         IRC = 1
         RETURN
      ENDIF
 
      IF(IOPTR.NE.0) IWANT = 1
      IF(IOPTM.NE.0) IWANT = 0
*
*     Count number of files that match input generic name
*
      CALL FMFILC(GENAM(1:LGN),NFILES,IRC)
      IF(IDEBFA.GE.1) PRINT *,'FMSELM. found ',NFILES,' entries ',
     +   'for specified generic-name'
*
*     Can only accept 100
      DO 10 I=1,NK
         CALL FMSELK(GENAM(1:LGN),KEYM(1,I),KEYZ,NKEYZ,MAXKEY,IRC)
         IF(IDEBFA.GE.1) PRINT *,'FMSELM. found ',NKEYZ,' matches',
     +      ' for selection # ',I
         JROBOT = 0
         JHUMAN = 0
         DO 20 J=1,NKEYZ
            CALL FMGETK(GENAM(1:LGN),LBANK,KEYZ(1,J),IRC)
            IF(IRC.EQ.0) THEN
*
*     Check on robot/manual (if requested)
*
               IF(KEYZ(MKMTFA,J).GT.1) THEN
                  CALL FMQMED(GENAM,LBANK,KEYZ(1,J),IMEDIA,IROBOT,IRC)
                  IF(IRC.NE.99.AND.IRC.NE.0) GOTO 40
                  IF(LCH.GT.0) THEN
                     IF(IROBOT.EQ.IWANT) THEN
                        IRC = 0
                        CALL UCOPY(KEYZ(1,J),KEYS,LKEYFA)
                        GOTO 30
                     ELSE
                        IF(IDEBFA.GE.3) PRINT *,'FMSELM. candidate # ',
     +                  J, ' rejected on manual/robot check'
                     ENDIF
                  ELSE
                     IF(JROBOT.EQ.0.AND.IROBOT.NE.0) JROBOT = J
                     IF(JHUMAN.EQ.0.AND.IROBOT.EQ.0) JHUMAN = J
*                    IRC = 0
*                    CALL UCOPY(KEYZ(1,J),KEYS,LKEYFA)
*                    GOTO 30
                  ENDIF
               ELSE
*
*     Check node name if option N
*
                  IF(IOPTN.NE.0) THEN
                     CALL FMGETC(LBANK,HOST,MHSNFA,NHSNFA,IRC)
                     IF(FMNODE(HOST).NE.0) GOTO 40
                  ENDIF
*
*     INQUIRE if dataset exists if option I
*
                  IF(IOPTI.NE.0) THEN
                     CALL FMGETC(LBANK,DSN,MFQNFA,NFQNFA,IRC)
#if defined(CERNLIB_CSPACK)
*
*     ZEBRA exchange format files
*
                     IF(IOPTN.EQ.0)
     +                  CALL FMGETC(LBANK,HOST,MHSNFA,NHSNFA,IRC)
*
*     Does node name match?
*
                     IF(FMNODE(HOST).EQ.0) THEN
*
*     Yes, just issue inquire
*
                        CALL FAINQR(DSN,' ',CHFILE,IRC)
                        IF(IRC.NE.0) GOTO 40
                     ELSE
*
*     No, if ZEBRA exchange format file, see if we could reach
*     remote node using CSPACK
*
                        CALL FMGETC(LBANK,FFORM,MFLFFA,NFLFFA,IRC)
                        IF(FFORM(1:3).NE.'FXN'.AND.
     +                     INDEX(FFORM,'FX').NE.0) THEN
                           CALL FMNTRC(HOST,FFORM,IRC)
                           IF(IRC.NE.0) GOTO 40
                        ELSE
                           GOTO 40
                        ENDIF
                     ENDIF
#endif
#if defined(CERNLIB_FPACK)
*
*     If node name does not match, then check .netrc file for host name
*
                     IF(IOPTN.EQ.0)
     +                  CALL FMGETC(LBANK,HOST,MHSNFA,NHSNFA,IRC)
                     IF(FMNODE(HOST).NE.0) THEN
                        CALL FMGETC(LBANK,FFORM,MFLFFA,NFLFFA,IRC)
*
*     FPACK files...
*
                        IF(FFORM(1:2).EQ.'FP') THEN
                           CALL FMNTRC(HOST,'FP',IRC)
                           IF(IRC.NE.0) GOTO 40
                        ELSE
                           CALL FAINQR(DSN,' ',CHFILE,IRC)
                           IF(IRC.NE.0) GOTO 40
                        ENDIF
                     ELSE
*
*     Other files, or local FPACK files...
*
                        CALL FAINQR(DSN,' ',CHFILE,IRC)
                        IF(IRC.NE.0) GOTO 40
                     ENDIF
#endif
#if (!defined(CERNLIB_FPACK))&&(!defined(CERNLIB_CSPACK))
                     CALL FAINQR(DSN,' ',CHFILE,IRC)
                     IF(IRC.NE.0) GOTO 40
#endif
                  ENDIF
 
                  CALL UCOPY(KEYZ(1,J),KEYS,LKEYFA)
                  GOTO 30
               ENDIF
            ELSE
               IF(IDEBFA.GE.1) PRINT *,'FMSELM. return code ',
     +            IRC,' from FMGETK for candidate # ',I
            ENDIF
 
   40       CONTINUE
 
            CALL MZDROP(IDIVFA,LBANK,'L')
            LBANK = 0
 
   20    CONTINUE
 
      IF(JROBOT.NE.0) THEN
         IF(IDEBFA.GE.3) PRINT *,'FMSELM. candidate # ',
     +      JROBOT, ' selected as best of the bunch'
         IRC = 0
         CALL UCOPY(KEYZ(1,JROBOT),KEYS,LKEYFA)
         CALL FMGETK(GENAM(1:LGN),LBANK,KEYS,IRC)
         GOTO 30
      ELSEIF(JHUMAN.NE.0) THEN
         IF(IDEBFA.GE.3) PRINT *,'FMSELM. candidate # ',
     +      JHUMAN, ' selected as best of the bunch'
         IRC = 0
         CALL UCOPY(KEYZ(1,JHUMAN),KEYS,LKEYFA)
         CALL FMGETK(GENAM(1:LGN),LBANK,KEYS,IRC)
         GOTO 30
      ENDIF
 
   10 CONTINUE
 
      IF(NFILES.EQ.0) THEN
         IRC = 14
      ELSE
         IRC = 15
      ENDIF
 
   30 IQUEST(11) = NFILES
 
      END
