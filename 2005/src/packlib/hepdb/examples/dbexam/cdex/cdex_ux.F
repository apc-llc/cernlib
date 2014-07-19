*
* $Id: cdex_ux.F,v 1.1.1.1 1996/02/28 16:23:39 mclareni Exp $
*
* $Log: cdex_ux.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:39  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE UXCPARM(COMND,PNAME,LVNEW,CVNEW,LVOLD,CVOLD,CVDEF)
*     -------------------------------------------------------------
*
*                        -- Author :    Boris Khomenko   10/02/94
      IMPLICIT NONE
*
*              -- arguments --
      CHARACTER*(*) COMND,PNAME,CVNEW,CVOLD,CVDEF
      INTEGER       LVNEW,LVOLD
*
*              -- externals --
      INTEGER       LNBLNK
*
      CHARACTER*8   CMND,PARM
      CHARACTER*11  CNEW , COLD , CDEF
      CHARACTER*6   TXNEW , TXOLD , TXDEF
      CHARACTER MESL*78
      INTEGER   LMES
      INTEGER   JJ , LL
*
      CMND=COMND
      PARM=' '
      LL=LNBLNK(PNAME)
      IF(LL.GT.0) THEN
        JJ=LEN(PARM)
        PARM(MAX0(JJ-LL,1):)=PNAME(:LL)
        PARM(JJ:JJ)='/'
      ENDIF
*
      CNEW=CVNEW
      COLD=CVOLD
      CDEF=CVDEF
*
      TXNEW=' (new)'
      TXOLD='  old='
      TXDEF='   * ='
      IF(LVNEW.EQ.1) TXNEW='(*new)'
      IF(LVOLD.EQ.1) TXOLD=' *old='
      IF(LVNEW.EQ.1.OR.LVOLD.EQ.1) TXDEF='$'
      IF(LVNEW.GT.0) THEN
        IF(LVOLD.LE.0) THEN
          TXNEW='  set:'
          IF(LVNEW.EQ.1) TXNEW=' *set:'
          TXOLD='#'
        ELSE IF(CNEW.EQ.COLD.AND.LVNEW.EQ.LVOLD) THEN
          TXNEW=' (old)'
          IF(LVNEW.EQ.1) TXNEW='(*old)'
          TXOLD='#'
        ENDIF
        IF(TXOLD.EQ.'#'.AND.LVNEW.EQ.1) TXOLD='$'
      ELSE IF(LVNEW.LE.0) THEN
        TXNEW='sorry?'
        TXOLD='$'
      ENDIF
*
      IF(TXOLD(1:1).NE.'#') THEN
        MESL='>>'//CMND//' '//TXNEW//' '//PARM//CNEW//
     +                                   TXOLD//COLD//TXDEF//CDEF
      ELSE
        MESL='>>'//CMND//' '//TXNEW//' '//PARM//CNEW//TXDEF//CDEF
      ENDIF
      LMES=INDEX(MESL,'$')-1
      IF(LMES.LE.0) LMES=LEN(MESL)
      CALL CDX_MESS(MESL(:LMES))
*
      END
*
*
      SUBROUTINE UXIPARM(COMND,PNAME,LVNEW,IVNEW,LVOLD,IVOLD,IVDEF)
*     -------------------------------------------------------------
*
*                        -- Author :    Boris Khomenko   10/02/94
      IMPLICIT NONE
*
*              -- arguments --
      CHARACTER*(*) COMND,PNAME
      INTEGER       IVNEW,IVOLD,IVDEF
      INTEGER       LVNEW,LVOLD
*
*         -- local variables --
      CHARACTER*11  CVNEW,CVOLD,CVDEF
      INTEGER       LL
*
      CALL ENCODI(IVNEW,-1,CVNEW,LL)
      CALL ENCODI(IVOLD,-1,CVOLD,LL)
      CALL ENCODI(IVDEF,-1,CVDEF,LL)
*
      CALL UXCPARM(COMND,PNAME,LVNEW,CVNEW,LVOLD,CVOLD,CVDEF)
      END
*
*
      SUBROUTINE UXRPARM(COMND,PNAME,LVNEW,RVNEW,LVOLD,RVOLD,RVDEF)
*     -------------------------------------------------------------
*
*                        -- Author :    Boris Khomenko   10/02/94
      IMPLICIT NONE
*
*              -- arguments --
      CHARACTER*(*) COMND,PNAME
      INTEGER       RVNEW,RVOLD,RVDEF
      INTEGER       LVNEW,LVOLD
*
*         -- local variables --
      CHARACTER*11  CVNEW,CVOLD,CVDEF
      INTEGER       LL
*
      INTEGER       IRPREC
      DATA          IRPREC/54/
*
      CALL ENCODR(RVNEW,-IRPREC,CVNEW,LL)
      CALL ENCODR(RVOLD,-IRPREC,CVOLD,LL)
      CALL ENCODR(RVDEF,-IRPREC,CVDEF,LL)
*
      CALL UXCPARM(COMND,PNAME,LVNEW,CVNEW,LVOLD,CVOLD,CVDEF)
      END
*
*
      SUBROUTINE UXFILLS
*     ------------------
*                        -- Author :    Boris Khomenko   10/02/94
C     IMPLICIT NONE
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
      INTEGER   K
*
      DO K=0,NCDXFIL
        CALL UXFILLN(K,MESL,LMES)
        CALL CDX_MESS(MESL(:LMES))
      ENDDO
*
      END
*
*
      SUBROUTINE UXFILLN(JTAB,LINE,LRES)
*     ------------------------====-====-
*                        -- Author :    Boris Khomenko   10/02/94
C     IMPLICIT NONE
*
*      --- arguments ---
      INTEGER   JTAB,LRES
      CHARACTER LINE*(*)
*      -----------------
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
*              -- externals --
      INTEGER   JLASNC
*
      CHARACTER MESL*78
      INTEGER   LMES
*
      INTEGER   NOPN , NERO , K
      CHARACTER ERRC*4
*
C     INTEGER   IRC , LR , LMAX , LFIL , LEN , JLASNC
*
      LMAX=MIN0(LEN(MESL),LEN(LINE))
      MESL=' '
*
      NOPN=0
      NERO=0
      IF(JTAB.EQ.0) THEN
        DO K=1,NCDXFIL
          IF(ICXFSTA(K).EQ.1) THEN
            NOPN=NOPN+1
          ELSE IF(ICXFSTA(K).GT.0) THEN
            NERO=NERO+1
          ENDIF
        ENDDO
        MESL=' --- DB files: 00/00 opened (+00 with errs.) ---$'
        J=INDEX(MESL,'/')
        CALL ENCODI(NOPN,    1,MESL(J-2:J-1),LR)
        CALL ENCODI(NCDXFIL,-1,MESL(J+1:J+2),LR)
        IF(NERO.LE.0) THEN
          LMES=INDEX(MESL,'(')-1
        ELSE
          J=INDEX(MESL,'+')
          CALL ENCODI(NOPN,    1,MESL(J+1:J+2),LR)
          LMES=INDEX(MESL,')')
        ENDIF
        MESL(LMES+1:LMES+4)=' ---'
        LMES=LMES+4
*
      ELSE IF(JTAB.GT.0.AND.JTAB.LE.NCDXFIL) THEN
*
        IRC=ICXFSTA(JTAB)
        IF(IRC.EQ.-1) THEN
          ERRC='ok'
        ELSE IF(IRC.EQ.1) THEN
          ERRC='Opnd'
        ELSE IF(IRC.EQ.0) THEN
          ERRC='?'
        ELSE
          IRC=IRC-ISIGN(1,IRC)
          CALL ENCODI(IRC,-1,ERRC,LR)
        ENDIF
*
        WRITE(MESL,1111) JTAB,CDXFPRF(JTAB),ERRC,CDXFTOP(JTAB)
 1111   FORMAT(I2,1H.,A2,' st=',A4,' top=',A8,' file:')
        LMES=INDEX(MESL,':')
        LFIL=JLASNC(CDXFNAM(JTAB),' ')
        IF(LFIL.GT.0) THEN
          MESL(LMES+2:)=CDXFNAM(JTAB)
          LMES=LMES+1+LFIL
        ENDIF
*
      ELSE
      ENDIF
      LINE=MESL
      LRES=MIN0(LMES,LEN(LINE))
      END
*
*
      SUBROUTINE UXIRCM(COMND,IRC)
*     ---------------------------
*                        -- Author :    Boris Khomenko   10/02/94
      IMPLICIT NONE
*
*       -- arguments --
      CHARACTER*(*) COMND
      INTEGER       IRC
*
#include "cdxluns.inc"
*
*  -- externals --
      INTEGER LNBLNK
*
*  -- Local variables --
*
      INTEGER   LL , LR , LM
      CHARACTER CMND*20 , CODE*10 , MESL*78
*
      CMND=COMND
      LL=LNBLNK(CMND)
      LL=MAX0(LL,6)
      CALL ENCODI(IRC,-1,CODE,LR)
      CALL CDERRMS(IRC,MESL)
      LM=LNBLNK(MESL)
      CALL CDX_MESS
     +         ('=='//CMND(:LL)//'>>  IRC='//CODE(:LR+1)//MESL(:LM))
      END
