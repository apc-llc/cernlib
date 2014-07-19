*
* $Id: cxpurg.F,v 1.1.1.1 1996/02/28 16:23:41 mclareni Exp $
*
* $Log: cxpurg.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:41  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CXPURG(LINE,JPAK,IFERR)
*     -----------------------****-====-
*                        -- Author :    Boris Khomenko   10/02/94
      IMPLICIT NONE
*
      CHARACTER LINE*(*)
      INTEGER   JPAK
*
#include "zquest.inc"
*
#include "yeaaas.inc"
*
*  -- externals --
      INTEGER JBYT , LNBLNK
      INTEGER IFSHEQC
*
*  -- parameters for CDPURG --
      CHARACTER CHPAT*40
      CHARACTER CHOPT*4
      INTEGER   LPAT , LOPT
      INTEGER   KYNUM , KYLIM
      INTEGER   IRC
*  ----------------------------
*
*  -- Command string analyse variables --
      INTEGER JPK1
      INTEGER IFTX , IFNX , LPRF
      CHARACTER PRFX*8 , PRF*8
      INTEGER NUMB
      REAL    FNUM
*
*  -- Local variables --
*
      INTEGER   IFERR , IFQUI
      CHARACTER YES*1 , CODE*11
      CHARACTER MESL*78
      INTEGER   JJ , LR , LL
*
      CHPAT='-'
      CHOPT='-'
      KYNUM=0
      KYLIM=0
*
      IFERR=0
      IFQUI=0
      YES=YEAAAS
*
   12 CONTINUE
*     _ save the next item's pointer
      JPK1=JPAK
      CALL TEXINS(LINE,JPAK,IFTX,PRFX,NUMB,FNUM)
      IFNX=MOD(IFTX,4)
      CALL CLTOU(PRFX)
*
      IF(IFTX.LE.0) THEN
*     -- it is a call without parameters
*       -- show the call format --
        CALL CDX_MESS('Call format:')
        CALL CDX_MESS('> CDPURG [path] [-opt] [kynum/kylim]')
        IFQUI=99
*
      ELSE IF(PRFX.EQ.'YES') THEN
*     -- it is a "YES" --
        YES='Y'
        CALL CDX_MESS('>>CDPURG: "YES"')
*
      ELSE IF(PRFX.EQ.'ASK') THEN
*     -- it is a "ASK" --
        YES='-'
        CALL CDX_MESS('>>CDPURG: "ASK"')
*
      ELSE IF(PRFX(1:1).EQ.'-') THEN
*     -- it is an option --
        CHOPT=PRFX
*
      ELSE IF(PRFX.EQ.'*'.OR.PRFX.EQ.'.') THEN
*     -- it looks like a current directory ("*" or ".")
        CHPAT=' '
        LPAT=1
        IFQUI=1
*
      ELSE IF(IFTX.EQ.3) THEN
*     -- it looks like a pathname (starts with "/")
*       -- get pathname with previously saved pointers
        JPAK=JPK1
        CALL TEXSTR(LINE,JPAK,IFTX,CHPAT)
        IFNX=MOD(IFTX,4)
        LPAT=LNBLNK(CHPAT)
        CALL CLTOU(CHPAT(:LPAT))
        IFQUI=1
*
      ELSE IF(PRFX.EQ.' '.AND.NUMB.GT.0) THEN
*     -- it's an alone positive number;
*        must be KYNUM followed by KYLIM, if any
        KYNUM=NUMB
*       -- try to get KYLIM
        IF(IFNX.EQ.3) THEN
          CALL TEXINS(LINE,JPAK,IFTX,PRFX,NUMB,FNUM)
          IFNX=MOD(IFTX,4)
          KYLIM=NUMB
        ELSE
          KYLIM=0
        ENDIF
*
      ELSE
*       -- signal an INVALID item --
        IFERR=1
        CALL CDX_MESS('>>CDPURG: unrecognized item')
      ENDIF
      IF(IFNX.GT.0.AND.IFQUI.EQ.0.AND.IFERR.EQ.0) GOTO 12
*
      IF(IFQUI.EQ.1.AND.IFERR.EQ.0) THEN
*       -- call parameters --
*
        IF(CHOPT(1:1).EQ.'-') CHOPT=CHOPT(2:)
        LOPT=MAX0(LNBLNK(CHOPT),1)
*
        IF(CHOPT.EQ.'S') KYLIM=MAX0(KYLIM,KYNUM)
*
        MESL='>>CALL CDPURG(CHPATH,'
        LL=LNBLNK(MESL)
        CALL ENCODI(KYNUM,-1,CODE,LR)
        MESL(LL+1:)=CODE
        LL=LNBLNK(MESL)
        CALL ENCODI(KYLIM,-1,CODE,LR)
        MESL(LL+1:)=','//CODE(:LR)//',"'//CHOPT(:LOPT)//'",IRC)'
        CALL CDX_MESS(MESL(:LNBLNK(MESL)))
*
        MESL=' '
        MESL(15:)=' chpath='//CHPAT(:LPAT)
        CALL CDX_MESS(MESL(:LNBLNK(MESL)))
*
        YES='-'
        IF(YES.NE.'Y') CALL CDX_ANSW('OK ? (y/n , <cr>=y )',YES)
        IF(YES.EQ.' ') YES='Y'
        CALL CLTOU(YES)
*
        IF(YES.EQ.'Y') THEN
          CALL CDPURG(CHPAT,KYNUM,KYLIM,CHOPT,IRC)
          IF(IRC.EQ.0) THEN
            CALL ENCODI(IQUEST(2),-1,CODE,LR)
            CALL CDX_MESS('==CDPURG>>  '//CODE(:LR)//' objects deleted')
            write(*,*) (iquest(jj),jj=1,20)
          ENDIF
*         -- report the Return Code --
          CALL UXIRCM('CDPURG',IRC)
        ELSE
          CALL CDX_MESS('>>CDPURG not called (NOK)')
        ENDIF
      ELSE IF(IFERR.NE.0) THEN
*     -- error end --
        CALL CDX_MESS('>>CDPURG not called (instruction error)')
*
      ELSE IF(IFQUI.EQ.0) THEN
*     -- no path given --
        CALL CDX_MESS('>>CDPURG not called (no path given)')
      ENDIF
      END
