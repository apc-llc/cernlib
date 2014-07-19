*
* $Id: cxddir.F,v 1.1.1.1 1996/02/28 16:23:39 mclareni Exp $
*
* $Log: cxddir.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:39  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CXDDIR(LINE,JPAK,IFERR)
*     ---------------- ------****-====-
*                        -- Author :    Boris Khomenko   10/02/94
      IMPLICIT NONE
*
      CHARACTER LINE*(*)
      INTEGER   JPAK
*
C\+CDE,CDXDIV.  (ICDXDIV)
*
C\+CDE,CDXLUNS.
*
#include "cdxfils.inc"
*
#include "yeaaas.inc"
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
*  -- externals --
      INTEGER JBYT , LNBLNK
      INTEGER IFSHEQC
*
*  -- initialization flag --
      INTEGER IFINIT
*  -- default/set parameters
*                  for CDDDIR --
*
CC:   'ChOpt'
CC:   LVOPTN
CC:   CHOPT
CC:   CHOPTN(2)
*
*     .... CHOPT ....
      INTEGER   LVOPTN
      CHARACTER*6        CHOPTN(2) , CHOPT
*
#if !defined(CERNLIB_IBMVM)
      STATIC  IFINIT
      STATIC    LVOPTN , CHOPTN
#endif
*  -- other parameters for CDDDIR --
      CHARACTER CHPATH*40
      INTEGER   LPATH
      INTEGER   IRC
*  - - - - - - - - - - - - - -
      INTEGER   LVNEW
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
      CHARACTER YES*1
      CHARACTER MESL*78
      INTEGER   LMES
      INTEGER   LL
*
      DATA IFINIT/0/
*
      IF(IFINIT.EQ.0) THEN
        IFINIT=1
        LVOPTN=1
        CHOPTN(1)='-'
      ENDIF
*
      IFERR=0
      IFQUI=0
      YES=YEAAAS
*
   12 CONTINUE
*     _ save the next item's pointer
      JPK1=JPAK
      CALL TEXINS(LINE,JPAK,IFTX,PRFX,NUMB,FNUM)
      IFNX=JBYT(IFTX,1,2)
      CALL CLTOU(PRFX)
*
      IF(IFTX.LE.0) THEN
*     -- it is a call without parameters
*            -- show the parameters setting --
        CALL CDX_MESS('>>CDDDIR --- parameters --- <<')
        CALL UXCPARM(' ','ChOpt',LVOPTN,CHOPTN(LVOPTN),0,' ',CHOPTN(1))
*     -- show the call format --
        CALL CDX_MESS('Call format:')
        CALL CDX_MESS('> CDDDIR  [par/val] [-opt] [path]')
*
      ELSE IF(PRFX.EQ.'YES') THEN
*     -- it is a "YES" --
        YES='Y'
        CALL CDX_MESS('>>CDDDIR: "YES"')
*
      ELSE IF(PRFX.EQ.'ASK') THEN
*     -- it is a "ASK" --
        YES='-'
        CALL CDX_MESS('>>CDDDIR: "ASK"')
*
      ELSE IF(PRFX(1:1).EQ.'-') THEN
*     -- it is an option --
        LVNEW=0
        IF(PRFX(1:2).EQ.'-=') LVNEW=1
        CALL UXNEWCP('CDDDIR','opt  ',LVNEW,PRFX,LVOPTN,CHOPTN)
*
      ELSE IF(IFTX.EQ.3) THEN
*     -- it looks like a pathname (starts with "/")
        IFQUI=1
*       -- get pathname with previously saved pointers
        CALL TEXSTR(LINE,JPK1,IFTX,CHPATH)
        LPATH=LNBLNK(CHPATH)
        CALL CLTOU(CHPATH(:LPATH))
*
*       -- other call parameters --
        CHOPT=CHOPTN(LVOPTN)
        IF(CHOPT.EQ.'-') CHOPT=' '
        LL=MAX0(LNBLNK(CHOPT),1)
*
        CALL CDX_MESS('>>CALL ...')
        MESL='>>CDDDIR(CHPATH,"'//CHOPT(:LL)//'",IRC)'
        LMES=INDEX(MESL,')')
        CALL CDX_MESS(MESL(:LMES))
*
        CALL CDX_MESS('         chpath='//CHPATH(:LPATH))
*
        IF(YES.NE.'Y') CALL CDX_ANSW('OK ? (y/n , <cr>=y )',YES)
        IF(YES.EQ.' ') YES='Y'
        CALL CLTOU(YES)
*
        IF(YES.EQ.'Y') THEN
          CALL CDX_MESS('>>...... OK, delete '//CHPATH(:LPATH))
          CALL CDDDIR(CHPATH,CHOPT,IRC)
*         -- report the Return Code --
          CALL UXIRCM('CDDDIR',IRC)
        ELSE
          CALL CDX_MESS('>>CDDDIR not called (NOK)')
        ENDIF
*
      ELSE
*       -- signal an INVALID item --
        IFERR=1
        CALL CDX_MESS('>>CDDDIR: Unrecognized item')
      ENDIF
      IF(IFNX.GT.0.AND.IFQUI.EQ.0.AND.IFERR.EQ.0) GOTO 12
*
      END
