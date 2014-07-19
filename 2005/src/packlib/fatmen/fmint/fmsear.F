*
* $Id: fmsear.F,v 1.1.1.1 1996/03/07 15:18:11 mclareni Exp $
*
* $Log: fmsear.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:11  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSEAR(GENAM,LFAT,KEYS,DSN,HOST,VID,
     +                  USER,UFORM,COMM,IRC)
#include "fatmen/fmpath.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatout.inc"
#include "fatmen/fatsea.inc"
      CHARACTER*(*)   GENAM,DSN,HOST,VID,USER,UFORM,COMM
      CHARACTER*256   CHDSN
      CHARACTER*8     CHHOST
      CHARACTER*8     CHUSER
      CHARACTER*8     CHVID
      CHARACTER*4     CHFORM
      CHARACTER*80    CHCOMM
#include "fatmen/fmnkeys.inc"
      DIMENSION       KEYS(LKEYFA)
*
*     Check bank for DSN/HOST/VID/USER
*
      IRC = 0
      LGEN = LENOCC(GENAM)
 
      IF(IDEBFA.GE.1) PRINT *,'FMSEAR. enter for ',GENAM(1:LGEN)
 
      IF(LENOCC(OUTPUT).EQ.0) THEN
         LWRITE = LPRTFA
      ELSE
         LWRITE = 3
      ENDIF
      IF(LFAT.EQ.0) THEN
         CALL FMGETK(GENAM(1:LGEN),LBANK,KEYS,IRC)
         IF(IRC.NE.0) THEN
            WRITE (LWRITE,*) 'FMSEAR. Return code ',IRC,' from FMGETK'
            RETURN
         ENDIF
         LBANK = LBANK + KOFUFA
      ELSE
         IF(IDEBFA.GE.1)
     +   WRITE(LWRITE,* )
     +        'FMSEAR. Enter for user-supplied bank at address ',LFAT
         LBANK = LFAT + KOFUFA
      ENDIF
 
      CHDSN  = ' '
      CALL UHTOC(IQ(LBANK+MFQNFA),4,CHDSN,NFQNFA)
      CALL CLTOU(CHDSN)
      CHHOST = ' '
      CALL UHTOC(IQ(LBANK+MHSNFA),4,CHHOST,NHSNFA)
      CALL CLTOU(CHHOST)
      CHVID  = ' '
      CALL UHTOC(IQ(LBANK+MVIDFA),4,CHVID,NVIDFA)
      CALL CLTOU(CHVID)
      CHUSER = ' '
      CALL UHTOC(IQ(LBANK+MCURFA),4,CHUSER,NCURFA)
      CALL CLTOU(CHUSER)
      CALL UHTOC(IQ(LBANK+MFUTFA),4,CHFORM,NFUTFA)
      CALL CLTOU(CHFORM)
      CHCOMM = ' '
      CALL UHTOC(IQ(LBANK+MUCMFA),4,CHCOMM,NUCMFA)
      CALL CLTOU(CHCOMM)
 
      JDSN  = LENOCC(CHDSN)
      JHOST = LENOCC(CHHOST)
      JVID  = LENOCC(CHVID)
      JUSER = LENOCC(CHUSER)
      JFORM = LENOCC(CHFORM)
      JCOMM = LENOCC(CHCOMM)
 
      LDSN  = LENOCC(DSN)
      LHOST = LENOCC(HOST)
      LVID  = LENOCC(VID)
      LUSER = LENOCC(USER)
      LFORM = LENOCC(UFORM)
      LCOMM = LENOCC(COMM)
 
      IF(LDSN.NE.0) THEN
         IF(IDEBFA.GE.2) PRINT *,'FMSEAR. comparing DSN'
         CALL FMATCH(CHDSN(1:JDSN),DSN(1:LDSN),IRC)
         IF(IRC.NE.0) THEN
            IRC = -1
            GOTO 99
         ENDIF
      ENDIF
 
      IF(LHOST.NE.0) THEN
         IF(IDEBFA.GE.2) PRINT *,'FMSEAR. comparing HOST'
         CALL FMATCH(CHHOST(1:JHOST),HOST(1:LHOST),IRC)
         IF(IRC.NE.0) THEN
            IRC = -2
            GOTO 99
         ENDIF
      ENDIF
 
      IF(LVID.NE.0) THEN
         IF(IDEBFA.GE.2) PRINT *,'FMSEAR. comparing VID'
         CALL FMATCH(CHVID(1:JVID),VID(1:LVID),IRC)
         IF(IRC.NE.0) THEN
            IRC = -3
            GOTO 99
         ENDIF
      ENDIF
 
      IF(LUSER.NE.0) THEN
         IF(IDEBFA.GE.2) PRINT *,'FMSEAR. comparing USERNAME'
         CALL FMATCH(CHUSER(1:JUSER),USER(1:LUSER),IRC)
         IF(IRC.NE.0) THEN
            IRC = -4
            GOTO 99
         ENDIF
      ENDIF
 
      IF(LFORM.NE.0) THEN
         IF(IDEBFA.GE.2) PRINT *,'FMSEAR. comparing USER file format'
         CALL FMATCH(CHFORM(1:JFORM),UFORM(1:LFORM),IRC)
         IF(IRC.NE.0) THEN
            IRC = -5
            GOTO 99
         ENDIF
      ENDIF
 
      IF(LCOMM.NE.0) THEN
         IF(IDEBFA.GE.2) PRINT *,'FMSEAR. comparing USER comment'
         CALL FMATCH(CHCOMM(1:JCOMM),COMM(1:LCOMM),IRC)
         IF(IRC.NE.0) THEN
            IRC = -6
            GOTO 99
         ENDIF
      ENDIF
 
      IF(IDCREA.GE.0) THEN
         IF(IDEBFA.GE.2) PRINT *,'FMSEAR. comparing creation date'
         CALL FMUPTM(LDCREA,LTCREA,IQ(LBANK+MCRTFA),IC)
 
         IF((LDCREA.LT.IDCREA).OR. (LDCREA.GT.JDCREA)) THEN
            IRC = -7
            GOTO 99
         ENDIF
         IF((LDCREA.EQ.IDCREA).AND.(LTCREA.LT.ITCREA)) THEN
            IRC = -7
            GOTO 99
         ENDIF
         IF((LDCREA.EQ.JDCREA).AND.(LTCREA.GT.JTCREA)) THEN
            IRC = -7
            GOTO 99
         ENDIF
 
      ENDIF
 
      IF(IDCATA.GE.0) THEN
         IF(IDEBFA.GE.2) PRINT *,'FMSEAR. comparing date catalogued'
         CALL FMUPTM(LDCATA,LTCATA,IQ(LBANK+MCTTFA),IC)
 
         IF((LDCATA.LT.IDCATA).OR. (LDCATA.GT.JDCATA)) THEN
            IRC = -8
            GOTO 99
         ENDIF
         IF((LDCATA.EQ.IDCATA).AND.(LTCATA.LT.ITCATA)) THEN
            IRC = -8
            GOTO 99
         ENDIF
         IF((LDCATA.EQ.JDCATA).AND.(LTCATA.GT.JTCATA)) THEN
            IRC = -8
            GOTO 99
         ENDIF
 
      ENDIF
 
      IF(IDLAST.GE.0) THEN
         IF(IDEBFA.GE.2) PRINT *,'FMSEAR. comparing date last accessed'
         CALL FMUPTM(LDLAST,LTLAST,IQ(LBANK+MLATFA),IC)
 
         IF((LDLAST.LT.IDLAST).OR. (LDLAST.GT.JDLAST)) THEN
            IRC = -9
            GOTO 99
         ENDIF
         IF((LDLAST.EQ.IDLAST).AND.(LTLAST.LT.ITLAST)) THEN
            IRC = -9
            GOTO 99
         ENDIF
         IF((LDLAST.EQ.JDLAST).AND.(LTLAST.GT.JTLAST)) THEN
            IRC = -9
            GOTO 99
         ENDIF
 
      ENDIF
*
*     Compare user words
*
      IF(IDEBFA.GE.2) PRINT *,'FMSEAR. comparing user words'
      DO 10 I=1,10
         IF(IUSER(1,I).EQ.-1) GOTO 10
         IF((IQ(LBANK+MUSWFA+I-1).LT.IUSER(1,I)).OR.
     +      (IQ(LBANK+MUSWFA+I-1).GT.IUSER(2,I))) THEN
            IRC = -9
            GOTO 99
         ENDIF
   10 CONTINUE
 
      RETURN
 
   99 CONTINUE
      IF((IDEBFA.GE.1).AND.(IRC.NE.0))
     +   PRINT *,'FMSEAR. comparison failed - status = ',IRc
 
      END
