*
* $Id: cxtime.F,v 1.1.1.1 1996/02/28 16:23:41 mclareni Exp $
*
* $Log: cxtime.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:41  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CXTIME(CHNAM,LINE,JPAK,IFERR)
*     ----------------------------------=====-
      CHARACTER*(*) CHNAM , LINE
      INTEGER       JPAK , IFERR
*
      CHARACTER  CHNM*6 , CHPU*1 , CHMS*1
*
      INTEGER   IFTX , IFNX
      CHARACTER PRFX*8
      INTEGER   NUM1 , NUM2 , IPACK
      REAL      FNUM
*
      IFERR=0
      CHNM=CHNAM
      CALL TEXINS(LINE,JPAK,IFTX,PRFX,NUM1,FNUM)
      IF(IFTX/4.LE.0) THEN
        IF(CHNM.EQ.'CDTM') CALL CDX_MESS
     +         ('Call formats:> CDTM yymmdd hhmm    or:>  CDTM pktime')
        IF(CHNM.EQ.'CDTS') CALL CDX_MESS
     +         ('Call formats:> CDTS yymmdd hhmmss  or:>  CDTS pktime')
        IF(CHNM(6:6).EQ.'M') CALL CDX_MESS
     +      ('Call formats:> CDPKTM yymmdd hhmm    vv:>  CDUPTM pktime')
        IF(CHNM(6:6).EQ.'S') CALL CDX_MESS
     +      ('Call formats:> CDPKTS yymmdd hhmmss  vv:>  CDUPTS pktime')
*...........and return.....
                     RETURN
      ENDIF
      IF(CHNM.EQ.'CDTM'.OR.CHNM.EQ.'CDTS') THEN
        CHMS=CHNM(4:4)
        IF(MOD(IFTX,4).GT.0) THEN
          CHPU='P'
          CALL CDX_MESS('>>CDTM/CDTS pack mode')
        ELSE
          CHPU='U'
          CALL CDX_MESS('>>CDTM/CDTS unpack mode')
        ENDIF
      ELSE
        CHPU=CHNM(3:3)
        CHMS=CHNM(6:6)
      ENDIF
      CHNM='CD'//CHPU//'KT'//CHMS
      IF(CHPU.EQ.'U') CHNM(4:4)='P'
      IF(CHPU.EQ.'P') THEN
        CALL TEXINS(LINE,JPAK,IFTX,PRFX,NUM2,FNUM)
        CALL CDI_MESS('>>CALL '//CHNM//'(d=@@@@@@,.>',NUM1)
        IF(CHMS.EQ.'S') THEN
          CALL CDI_MESS('t=@@@@@@,IPACK,irc)',NUM2)
          CALL CDPKTS(NUM1,NUM2,IPACK,IRC)
        ELSE
          CALL CDI_MESS('t=@@@@,IPACK,irc)',NUM2)
          CALL CDPKTM(NUM1,NUM2,IPACK,IRC)
        ENDIF
        IF(IRC.NE.0) CALL UXIRCM(CHNM,IRC)
        CALL CDI_MESS('=='//CHNM//'>> pktime=0',IPACK)
      ELSE
        IPACK=NUM1
        CALL CDI_MESS('>>CALL '//CHNM//'(IDATE,ITIME,pkt=0,irc)',IPACK)
        IF(CHMS.EQ.'S') THEN
          CALL CDUPTS(NUM1,NUM2,IPACK,IRC)
        ELSE
          CALL CDUPTM(NUM1,NUM2,IPACK,IRC)
        ENDIF
        IF(IRC.NE.0) CALL UXIRCM(CHNM,IRC)
        CALL CDI_MESS('=='//CHNM//'>> date=@@@@@@,.>',NUM1)
        IF(CHMS.EQ.'S') CALL CDI_MESS(' time=@@@@@@',NUM2)
        IF(CHMS.EQ.'M') CALL CDI_MESS(' time=@@@@',NUM2)
      ENDIF
*
      END
 
 
