*
* $Id: cxpref.F,v 1.1.1.1 1996/02/28 16:23:39 mclareni Exp $
*
* $Log: cxpref.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:39  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CXPREF(PF)
*     ---------------------
*                        -- Author :    Boris Khomenko   10/02/94
      IMPLICIT NONE
*
      CHARACTER PF*(*)
*
#include "cdxluns.inc"
*
#include "cdxfils.inc"
*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC+KEEP,CDXFILS.
CC*
CC      INTEGER MCDXFIL
CC      PARAMETER (MCDXFIL = 10)
CC      COMMON /CDXFILS/ NCDXFIL,KCDXFIL,
CC     +                 ICXFSTA(10),CDXFPRF(10),CDXFTOP(10),CDXFNAM(10)
CC      INTEGER   NCDXFIL, KCDXFIL, ICXFSTA
CC      CHARACTER CDXFPRF*2, CDXFTOP*8, CDXFNAM*40
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*
      CHARACTER MESL*78
      INTEGER   LMES
*
      CHARACTER XX*2 , CHTOP*8 , CHFILE*40
      INTEGER   IRC , J1 , J2 , LR , LFIL , LEN , JLASNC
      INTEGER   KK , JJ , JE , K , J
*
      XX=PF
      CALL CLTOU(XX)
*
      CHTOP='-'
      CHFILE='-'
      CALL CDPREF(LUNAMES,XX,CHTOP,CHFILE,IRC)
*
*     -- Locate/add the XX in the CDXFILS table
      KK=0
      JJ=0
      JE=0
      DO 111 K=1,NCDXFIL
        IF(KK.EQ.0.AND.CDXFPRF(K).EQ.XX ) KK=K
        IF(JJ.EQ.0.AND.CDXFPRF(K).EQ.' ') JJ=K
        IF(JE.EQ.0.AND.ICXFSTA(K).NE.0.AND.ICXFSTA(K).NE.-1) JE=K
  111 CONTINUE
      IF(KK.LE.0) THEN
        IF(JJ.GT.0) THEN
          KK=JJ
        ELSE IF(NCDXFIL.LT.MCDXFIL) THEN
          NCDXFIL=NCDXFIL+1
          KK=NCDXFIL
        ELSE IF(JE.GT.0) THEN
          KK=JE
        ELSE
          KK=NCDXFIL
        ENDIF
      ENDIF
*
*     -- store result into CDXFILS table
      ICXFSTA(KK)=-IABS(IRC)-1
      CDXFPRF(KK)=XX
      CDXFTOP(KK)=CHTOP
      CDXFNAM(KK)=CHFILE
*
      MESL='  CDPREF>>'
      J=12
      CALL UXFILLN(KK,MESL(J:),LMES)
      LMES=LMES+J-1
      CALL CDX_MESS(MESL(:LMES))
*
      END
